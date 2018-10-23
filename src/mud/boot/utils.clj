(ns mud.boot.utils
  (:require [backtick :as bt]))

;; fns --------------------------------------------

(def remnil
  (partial remove nil?))

(defn redh [f xs]
  (reduce f {} xs))

(def kset
  (comp set keys))

(defn catv [& xs]
  (vec (apply concat xs)))

(defn probe [& xs]
  (mapv println xs)
  (last xs))

(defn chunks [xs n]
  (partition n n nil xs))

(defn maph [f xs]
  (into {} (map f xs)))

(defn mapvals [f m]
  (maph (fn [[k v]] [k (f v)]) m))

(defn call* [xs]
  (apply (first xs) (rest xs)))

(defn argv-litt [n & [prefix]]
  (vec (repeatedly n #(gensym (or prefix "a")))))

(defn symcat [& xs]
  (symbol #_(namespace (first xs))
    (apply str (map name xs))))

(defn symcat
  "ns aware version"
  [x & xs]
  (symbol (namespace x)
    (apply str (name x) (map name xs))))

(defn member? [xs e]
  (contains? (set xs) e))

(defn indexof [xs e]
  (and (member? xs e)
    (loop [i 0 [x & xs] xs]
      (if (= x e)
        i (recur (inc i) xs)))))

(defn gat [xs i]
  (if (>= i 0)
    (cond
      (vector? xs) (get xs i)
      (seq? xs) (first (drop i xs)))
    (gat (reverse xs) (- i))))

;; compile time related

(defn ns-qualify [x]
  (eval `(bt/syntax-quote ~x)))

(defn quotef [x] (list 'quote x))

(defn quote? [x]
  #_(println x)
  (and (seq? x)
       (= 'quote (first x))
    (= 2 (count x))))

(defn gensyms
  ([] (repeatedly gensym))
  ([n] (take n (gensyms))))

(defn parse-defn [[fb & rb]]
  (let [[name [fb & rb]] (if (symbol? fb) [fb rb] [(gensym 'fn) (cons fb rb)])
        [doc [fb & rb]] (if (string? fb) [fb rb] ["" (cons fb rb)])
        [opts [fb & rb]] (if (map? fb) [fb rb] [{} (cons fb rb)])
        cases (if (seq? fb) (cons fb rb) (list (list* fb rb)))]
    {:name name
     :doc doc
     :opts opts
     :cases cases
     :body (list* doc opts cases)}))

;; macros ----------------------------------------

(defmacro err [& xs]
  `(throw (Exception. (str ~@(interpose "\n" xs)))))

(defmacro dbg [& xs]
  `(do (mapv println ~(vec (butlast xs)))
       ~(last xs)))

(defmacro when! [x & body]
  `(do (assert ~x)
       ~@body))

(defmacro asserts [& xs]
  `(do ~@(map (fn [x] `(assert ~x)) xs)))

(defmacro exp [x]
  `(macroexpand-1 '~x))

(defmacro locals []
  (let [locals (keys &env)]
    `(zipmap '~locals ~(vec locals))))

(comment
  (defn expnth' [n x]
    (if (zero? n)
      x (expnth' (dec n) (macroexpand-1 x))))

  (defmacro expnth [n x]
    (if (zero? n)
      x (expnth' (dec n) (macroexpand-1 x))))

  (expnth 2 '(fn [b] (fn [a] (+ b a))))

  (defmacro exprec [x]
    `(clojure.walk/macroexpand-all '~x)))

(defmacro import-macros [x y & nxt]
  `(do (def ~x (var ~y))
       (.setMacro (var ~x))
       ~(when nxt `(import-macros ~@nxt))))

(defmacro import-fns [x y & nxt]
  `(do (defn ~x [& xs#] (apply ~y xs#))
       ~(when nxt `(import-fns ~@nxt))))

(defmacro defs [& xs]
  (when xs
    `(do
       (def ~(first xs) ~(second xs))
       (defs ~@xs))))

(do :not-sure-it-belongs-here

    (defmacro defmac
      "personal defmacro
       define a regular macro
       but also a function that do the same thing as the macro
       (when receiving quoted args)
       here I hope that it could ease macro composition and later ckish embeddings"
      [& body]
      (let [{:keys [name doc opts body]} (parse-defn body)
            fname (symcat name '-fn)]
        `(do (defn ~fname ~@body)
             (defmacro ~name ~doc
               ~(assoc opts :fn fname)
               ([& xs#] (apply ~fname xs#))))))

    (defmac marked-fn

      "marked function,
       define an anonymous form (like fn)
       a def form (like defn)
       and a predicate function (like fn?)"

      [name & [doc]]

      `(do

         (defmac ~name
           [& body#]
           (let [parsed# (parse-defn body#)]
             `(with-meta
                (fn ~(:name parsed#) ~@(:cases parsed#))
                {~~(keyword name) true})))

         (defn ~(symcat name '?) [x#]
           (when (-> x# meta ~(keyword name)) x#))

         (defmac ~(symcat 'def name) [name'# & body#]
           `(def ~name'# (~'~name ~@body#))))))

;; not used ---------------------------------------

(defmacro defnlike
  "a macro for handling the doc meta boring parsing"
  [name & form]
  `(defmacro []))

(defn hostsym
  "ex: 'inc -> 'clojure.core/inc"
  [s]
  (let [{ns :ns n :name} (meta (resolve s))]
    (symbol (name (.getName ns)) (name n))))


;; tests -------------------------------------------

(asserts
  (= 3 (gat '(1 2 3) 2))
  (= 2 (gat '(1 2 3) -1))
  (nil? (gat '(1 2 3) 3))
  (= 1 (gat [1 2 3] 0))
  (= 2 (gat [1 2 3] -1))
  (nil? (gat [1 2 3] -10)))


(let [x 1] (locals))

(defn fff [& xs]
  (locals))

(fff 1 2 3)