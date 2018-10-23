(ns mud.boot.generics
  (:use [mud.boot.utils])
  (:require [mud.boot.types :as t]))

;; generic function
;; based on clojure protocols with some extra features

;; generics can be variadics
;; generics can be extended by arity
;; (in regular protocols, you have to implement all arity when extending your proto to your type
;; even if a parent class already implement those...)
;; there is some shortcuts when 2 or more types share an implementation
;; via the set syntax litteral e.g #{:type1 :type2}
;; it works with mud.boot.types

;; state -----------------------------------

;; hold generics implementations
(defonce reg (atom {}))

;; utils -----------------------------------

(do

  (defn arities [cases]
    (->> cases (map first) (map count) set))

  (defn with-ns [ns sym]
    (symbol ns (str sym)))

  (defn derive-name [n]
    {:name n
     :pname (symcat 'I n)
     :mname (symcat 'p_ n)
     :ns (namespace (ns-qualify n))})

  (defn variadic-argv? [x]
    ((set x) '&))

  (defn split-cases [xs]
    (let [{fixed nil
           variadic '&}
          (group-by (comp variadic-argv? first) xs)]
      (assert (#{0 1} (count variadic)))
      {:fixed fixed
       :variadic (first variadic)}))

  (defn format-variadic-case
    [[argv & members]]
    (cons (-> argv butlast butlast
            vec (conj (last argv)))
      members))

  (defn exprmap [xs]
    (redh
      (fn [a [t e]]
        (merge a (zipmap (t/classes t) (repeat e))))
      (reverse (partition 2 xs))))

  (defn casemap [cases]
    (redh
      (fn [a [argv & decls]]
        (let [arity (count argv)]
          (reduce
            (fn [a [c e]]
              (update a c assoc arity (list argv e)))
            a (exprmap decls))))
      cases))

  (defn emit-impls [name cases]
    (mapcat
      (fn [[c m]]
        [c (cons name (vals m))])
      (casemap cases))))

;; impl ------------------------------------

(do

  (defn parse-case [x]
    (if (even? (count x))
      (concat (butlast x) [:any (last x)])
      x))

  (defn parse-cases [xs]
    (map parse-case
      (if (vector? (first xs))
        (list xs)
        xs)))

  (asserts
    (= (parse-cases '([a] :any 1))
      (parse-cases '(([a] 1))))
    (= (parse-cases '([a] :vec 1 1))
      (parse-cases '(([a] :vec 1 :any 1)))))

  (defn generic-spec [name body]

    (let [doc (when (string? (first body)) (first body))
          cases' (if doc (rest body) body)
          cases (parse-cases cases')
          {:keys [fixed variadic]} (split-cases cases)
          variadic (some-> variadic format-variadic-case)
          variadic-arity (some-> variadic first count)
          argvs (concat (map first fixed) (some-> variadic first vector))
          arities (set (map count argvs))
          cases (if-not variadic fixed (concat fixed [variadic]))]

      (assert (if variadic (= variadic-arity (apply max arities)) true)
        "arity error, fixed arity > variadic arity")
      (merge
        (derive-name name)
        {:variadic? (boolean variadic)
         :arities arities
         :sigs (map argv-litt arities)
         :cases cases
         :doc doc})))

  (defn init-generic
    [{:keys [name pname mname
             sigs variadic? arities]}]
    `(do

       (defprotocol ~pname
         (~mname ~@sigs))

       ;; this does not work
       #_~(if-not variadic?
          `(def ~name (var ~mname))
          `(defn ~name [& ~'xs]
             (apply ~mname
               (let [[a# b#] (split-at ~(dec (apply max arities)) ~'xs)]
                 (concat a# (when (seq b#) [b#]))))))

       (defn ~name [& ~'xs]
           ~(if-not variadic?
              `(apply ~mname ~'xs)
              `(apply ~mname
                 (let [[a# b#] (split-at ~(dec (apply max arities)) ~'xs)]
                   (concat a# (when (seq b#) [b#]))))))))

  (defn extend-generic
    [{:keys [ns pname mname cases]}]
    `(extend-protocol ~(with-ns ns pname)
       ~@(emit-impls (with-ns ns mname) cases)))

  (defn compile-generic [spec]
    `(do ~(init-generic spec)
         ~(extend-generic spec)))

  ;; extend-type

  (defn impl-body->cases
    [tag [x1 x2 :as xs]]
    (if (vector? x1)
      [(list x1 tag x2)]
      (map (fn [[argv expr]] (list argv tag expr)) xs)))

  (defn implement [tag [name & body]]
    `(generic+ ~name ~@(impl-body->cases tag body))))

;; api -------------------------------------

(defmacro generic
  "create a generic function"
  [name & cases]
  (let [spec (generic-spec name cases)]
    `(do (swap! reg assoc '~name '~spec)
         ~(compile-generic spec))))

(defmacro generic+
  "add new cases to an existant generic
   all given arities must already be known"
  [name & cases]

  (let [{ars+ :arities cases+ :cases}
        (generic-spec name cases)
        arities (-> @reg name :arities)]

    (assert arities "unknown generic")
    (assert (every? arities ars+) "unknown arity")

    (swap! reg update-in
      [name :cases] concat cases+)

    (extend-generic (get @reg name))))

(defmacro compile-all! []
  `(do ~@(map extend-generic (vals @reg))))

(defmacro type+
  "like extend type"
  [tag & impls]
  `(do ~@(map #(implement tag %) impls)))

;; tests -----------------------------------

(do

  (generic g1 [x]
    ;; prim type impl
    :vec :g1vec
    ;; this type is a group
    ;; under the hood it implements for all collections
    :coll [:g1coll x]
    ;; group litteral can be handy
    #{:key :sym} :key-or-sym)

  (asserts
    (g1 [])
    (g1 #{})
    (g1 '())
    (g1 'a)
    (g1 :a))

  ;; extension
  (generic+ g1 [x]
    ;; str impl
    :str [:str x]
    ;; if a last expresion is given it extends Object
    [:unknown x])

  (asserts
    (g1 "a")
    (g1 (atom {})))

  ;; poly arity exemple
  (generic g2
    ([x y]
      :vec [:g2vec x y]
      :coll [:g2coll x y]
      :num [:g2num x y]
      :any [:g2any x y])
    ([x y z]
      :coll [:coll x y z])
    ;; variadic arity
    ([x y z & more]
      [:variadic x y z more]))

  (asserts
    (= (g2 [] 1)
      [:g2vec [] 1])
    (= (g2 #{} 1)
      [:g2coll #{} 1])
    (= (g2 #{} 1 2)
      [:coll #{} 1 2])
    (= (g2 "me" 1 2 3 4)
      [:variadic "me" 1 2 '(3 4)]))

  (generic+ g2
    ([a b] :vec [:g2vec2 a b])
    ([a b c & ds] :str [:variadstr a b c ds]))

  (asserts
    (= (g2 [] 1)
      [:g2vec2 [] 1])
    (= (g2 "me" 1 2 3 4)
      [:variadstr "me" 1 2 '(3 4)]))


  (generic sip'
    ([a b]
      :vec (conj a b)
      :map (apply assoc a b)
      :set (conj a b)
      :seq (concat a [b])
      :str (str a (.toString b))
      :sym (symbol (sip' (name a) (.toString b)))
      :key (keyword (sip' (name a) (.toString b))))
    ([a b & xs]
      (apply sip' (sip' a b) xs)))

  (asserts
    (= (sip' [] 1 2 3)
      [1 2 3])
    (= (sip' #{} 1 2 3)
      #{1 2 3}))

  (generic valid'
    [x]
    :nil nil
    :map (when (every? valid' (vals x)) x)
    :coll (when (every? valid' x) x)
    :word :validword
    :any x)

  (asserts
    (not (valid' [nil 1 nil]))
    (valid' [1 2 3])
    (valid' #{1 2 3})
    (valid' {:a 1 :b 2})
    (not (valid' {:a 1 :b 2 :c nil})))

  (generic+ valid'
    [x] :key :validkey)

  (asserts
    (= :validkey (valid' :a))
    (= :validword (valid' 'a)))


  )