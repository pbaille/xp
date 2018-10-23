;; last implementation of this object model that i'm working on for so long...
;; 500 first lines are kinda fixed
;; contains experiments after that (dispatcher, destructure etc...)
;; have to study IO object model for inspiration

(ns fun.core
  (:refer-clojure :exclude [eval fn])
  (:require [clojure.core :as c]
            [fun.km :as km :refer [km km?]]
            [fun.fn :as f :refer [fn]]
            [fun.utils :as u]))

(declare obj?)

;; getter --------------------------------------

(defn §
  ([x] x)
  ([x y] (when (obj? x) ((:get x) x y)))
  ([x y & args] (when-let [o (§ x y)] (apply o args))))

(def §*
  (partial apply §))

(defmacro §>
  "
   a bit like core/->

   (§> a
       (:a 1)
       (:b 2)
       (:c 3))

   (-> a
       (§ :a 1)
       (§ :b 2)
       (§ :c 3))
   "
  [init & forms]
  `(-> ~init
       ~@(map (fn [x] (cons '§ x)) forms)))

;; base record ---------------------------------

(u/applicable-record
  BaseObj
  [{:as o :keys [apply]} args]
  (if apply
    (apply o args)
    (§* o args)))

(defmethod print-method BaseObj [o w]
  (print-method (or (§ o :show) 'obj) w))

(c/defn obj? [x]
  (and (instance? BaseObj x) x))

;; primitive closure ----------------------------

(defmacro ^:private cls [& xs]
  `(vary-meta (fn ~@xs) assoc
              :closure? true))

(defn cls? [f]
  (and (fn? f)
       (:closure? (meta f))))

;; obj ------------------------------------------

(declare f*)

(def obj0

  (map->BaseObj

    {:tag 'obj

     :get
     (cls obj-get
          [{:as o
            :keys [wrap]
            :or {wrap u/a2}}
           x]
          (when-let [found (get o x)]
            (wrap o found)))

     :nest
     (cls obj-nest [o x] o)

     :wrap
     (cls obj-wrap [o x]

          (cond

            (cls? x)
            (if (= 1 (f/arity x))
              (x o)
              (f* x :args [o]))

            (obj? x)
            ((:nest x) x o)

            :else x))

     :swap
     (cls o-swap [o f] (f o))

     :show
     (cls obj-show [o]
          (list (§ o :tag)
                (into {} o)))}))

(defn obj [& opts]
  (km obj0 opts))

;; function -------------------------------------

(def f0

  (obj

    :tag 'f

    :args []

    :cont []

    :apply
    (cls f-apply
         [{:as f
           :keys [value args cont]}
          args*]

         (let [all-args (u/catv args args*)
               acnt (count all-args)
               maxa (f/max-arity value)
               apply&cont
               (fn [xs]
                   (reduce
                     #(%2 %1)
                     (apply value xs)
                     cont))]

           (cond

             ;; fully applicable
             (f/has-arity? value acnt)
             (apply&cont all-args)

             ;; apply as args as possible and continue
             (and maxa (> acnt maxa))
             (let [[taken rem] (split-at maxa all-args)]
               (apply (apply&cont taken)
                      rem))

             ;; partial application
             (f/has-partial-arity? value acnt)
             (assoc f :args all-args)

             :else
             (u/throw* "f apply error"))))

    :swap
    (cls f-swap
         [f g]
         (update f :cont conj g))

    :show
    (cls f-show
         [{:as f
           :keys [code name args tag]}]

         (let [args (seq args)]
           (cond

             (not code) 'f0

             (and name args)
             (list* 'partial name args)

             name name

             args
             (list* 'partial
                    (cons tag (next code))
                    (:args f))

             :else
             (cons tag (next code)))))))

(defn f* [f & opts]
  ;; cant call :extend here: stackoverflow
  (km f0
      (meta f)
      {:value f}
      opts))

(defmacro f [& form]
  `(f* (~'fun.fn/fn ~@form)))

(defn f? [x]
  (and (obj? x) (= 'f (:tag x))))

(do
  "obj"
  (let [o (obj :a 1
               :b 2
               :c (cls [{:keys [a]} b] (+ a b))
               :d (cls [{:keys [b]}] b)
               )]
    (§ o :c 2))

  "f"
  (let [f1 (f aze [a b c] (+ a b c))
        f2 (f bob
              "doc"
              {:meta :data}
              ([] :zero)
              ([a] :one)
              ([a e r] :ter) ; comment this to see the catch
              ([a e r & _] :variadic))
        f3 (f2 2 3)
        f4 (f [a b] (+ a b))]
    (f2 1)
    #_((:arities f3) f3)
    (:args f3)
    f3
    (f3 2)
    [(f1 1 2)
     (f2)
     (f2 1)
     (f2 2 3)
     (f2 2 3 4)
     (f2 2 3 4 5)
     ((f2 2 3) 1 2 3)
     ((f2 2 3) 6)
     (f4 1)]))

;; closure --------------------------------------

(defn c* [v & opts]
  (f* v
      :tag 'c
      :nest
      (cls c-nest [a b] (a b))
      opts))

(defmacro c [& form]
  `(c* (~'fun.fn/fn ~@form)))

(defn c? [x]
  (and (obj? x) (= 'c (:tag x))))

(do
  (assert
    (= 3
       (§ (obj :a (c [{b :b} a] (+ a b))
               :b 2)
          :a 1))))

;; module ---------------------------------------

(def m0

  (obj

    :tag 'm

    :not-found
    (c [o x]
       (when-let [h (:host o)]
         (§ h x)))

    :get
    (c m-get
       [{:as o :keys [wrap not-found]} x]

       (cond

         (u/simple-kw? x)
         (if-let [found (get o x)]
           (wrap o found)
           (not-found o x))

         ;; composite keywords (containing dots)
         (keyword? x)
         (let [[fkw rkw] (u/kw-uncons x)]
           (or (§ (m-get o fkw) rkw)
               (not-found o x)))

         :else (u/scream!)))

    :def
    (c m-def [o xs]
       (reduce
         (fn [o [k v]]
             (cond

               (u/simple-kw? k) (assoc o k v)

               (keyword? k)
               (let [[fkw rkw] (u/kw-uncons k)]
                 (assoc o
                   fkw (or (§ (get o fkw) :swap (f [o] (o :def [rkw v])))
                           (§ m0 :def [rkw v]))))

               :else (u/scream!)))

         o (km xs)))

    :swap
    (c c-upd [o x]
       (if (f? x)
         (x o)
         (reduce
           (fn [o [k f]]
               (cond

                 (u/simple-kw? k)
                 (if-let [found (get o k)]
                   (assoc o
                     k (or (§ found :swap f)
                           (f found)))
                   o)

                 (keyword? k)
                 (let [[fkw rkw] (u/kw-uncons k)
                       found (get o fkw)]
                   (if found
                     (assoc o
                       fkw (§ found :swap [rkw f]))
                     o))

                 :else (u/scream!)))
           o (km x))))

    :nest
    (c m-nest [o x]
       (assoc o :host x))

    :show
    (c m-show [o]
       (list 'm (into {} (dissoc o :host))))))

(defn m
  "basic module with lexical scope like resolution"
  [& opts]
  (§ m0 :def opts))

(defmacro defm [name & args]
  `(def ~name (m :name '~name ~@args)))

(do
  (let [m1
        (m
          :a 1
          :b 2
          :a+ (c [o x] (+ x (o :a)))
          :m1
          (m :b 3
             :a+b (c [o] (+ (o :b) (o :a)))
             :a+b+m2 (c [o] (+ (o :a+b) (o :m2.a+b)))
             :m2
             (m :b 4
                :a+b (c [o] (+ (o :m1.b) (o :b) (o :a))))))
        m2
        (m :a (m :b 1)
           :b (m :a 1
                 :b 12
                 :c (c [o] (o :a.b)))
           :c (m :a (m :b 2)
                 :d (c [o] (o :a.b))))

        m3
        (m :a 1
           :b (c [o] (m :a' (o :a) :b' 2)))]

    (assert
      (and

        ;; basics

        (= 1 (m1 :a))
        (= 3 (m1 :a+ 2))
        (= 8 (m1 :m1.m2.a+b))
        (= 12 (m1 :m1.a+b+m2))
        (= 1 (m2 :b.c))
        (= 2 (m2 :c.d))
        (= nil (m1 :b.a))

        ;; def
        (= 100
           (m1 :def
               [:zer 43
                :m1.c 57
                :d
                (c [o]
                   (+ (o :zer)
                      (o :m1.c)))]
               :d))

        ;; swap a closure and a constant
        (= 4
           (m1 :swap [:a+ inc :a inc]
               :a+ 1))

        ; nested def with submodule creation
        (= 1 (m1 :def [:yo.bar 1]
                 :yo.bar))

        ;; def trough closure
        (= 2 (m3 :def [:b.c 2] :b.c))

        ;; nested with creation then extension of submodule (:x)
        (let [m1' (m1 :def [:x.y 1 :x.z 2])]
          (and
            (= 1 (m1' :x.y))
            (= 2 (m1' :x.z))
            (= 'm (m1' :x.tag))))))

    )

  "lexical scope"
  (let [m2 (m :a 1
              :c (m :d 1))]
    (assert (not (m2 :b)))
    (assert (not (m2 :c.f)))
    (assert (= 1 (m2 :a) (m2 :c.d) (m2 :c.a)))))

(defn o
  "prototypal inheritance type object
   takes a field :protos that holds other objects
   that will be used in order when the object cannot respond to a message
   it will do so depth first

   one thing to clarify is which self should be passed to 'methods' found in protos...
   now it is the proto himself, not always what we want I suppose..."

  [& opts]
  (m opts
     :not-found
     (c [• x]
        (loop [[p & ps] (:protos •)]
          (when p
            (or (§ p x)
                (recur ps)))))))

(do
  (let [o1 (o :protos [(o :foo "foo" :a 45) (o :foo "foo2" :bar 1)]
              :a 12)]
    (assert (= 12 (o1 :a)))
    (assert (= "foo" (o1 :foo)))
    (assert (= 1 (o1 :bar)))))

;; constant -------------------------------------

(defn k [x]
  (obj
    :const? true
    :value x
    :show (list 'k x)
    :apply (c [• _] (§ • :value))))

(defn k? [x]
  (and (obj? x)
       (§ x :const?)))

;; expression -----------------------------------

(def e

  (obj

    :expr? true

    :show
    (c [•]
       (list* 'e (§ • :content)))

    :nth-arg
    (c [• n]
       (nth (§ • :args) n))

    :apply
    (c [• args]
       (cond

         (§ • :content)
         (assoc •
           :content (cons • args)
           :verb •
           :args args)

         (first args)
         (assoc •
           :content args
           :verb (first args)
           :args (rest args))

         :else
         (u/throw* "empty expr error")))))

(def e* (partial apply e))

(defn e? [x]
  (and (obj? x)
       (§ x :expr?)))

(do
  (e :+ 1 2)
  ((e :comp :inc :inc) 1))

;; dependant -----------------------------------

(defn dependant [f & opts]

  "an object that depends on another,
   can be initiated by being applied to its dependance
   or use its container (via :nest) for extraction."

  (m

    :tag 'dependant

    :init f

    :apply
    (c [• xs]
       (§* • :nest xs))

    :nest
    (c [• x]
       (if-let [ret (§ • :init x)]
         (ret :def (§ • :fields))
         (u/throw* "dependant init error")))

    :fields (km opts)))

(do
  (let [d1 (dependant
             (f [host] (m (select-keys host [:a :b])))
             :d (c [•] (+ (• :a) (• :b))))
        m1 (m :a 2
              :b 1
              :c d1)]
    (assert
      (=
        ;; nested
        (m1 :c.d)
        ;; built directly
        (d1 (m :a 1 :b 2) :d)
        ))))

(defn ks
  "ex:
   (ks :req [:a :b]
       :opt [:f :g])
   an object that requires :a and :b keys and optionally :f and :g keys
   "
  [& opts]

  (dependant
    (f [host]
       (let [req-ks (host :req)
             rks (select-keys host req-ks)
             oks (select-keys host (host :opt))]

         (if-not (= (count req-ks) (count rks))
           (u/throw* "ks required key not found")
           (m rks oks))))
    opts))

(comment

  (let [m1 (m :a 2
              :b 1
              :c (dependant (fn [host] (select-keys host [:a :b]))
                            :d 10)
              :d (ks :req [:a :b]
                     :opt [:f :g]))
        ]
    (assert (= 10 (m1 :c.d)))
    (m1 :d)))

;; navigable ------------------------------------

(defn pointed
  "a module that holds a root key
   that will prefix all field access,
   and defer to top level of not found"
  [root & opts]
  (m
    :root root
    :not-found
    (c [• x]
       #_(println "not found")
       (§ • x))

    :get
    (c [• x]
       #_(println "hey")
       (let [find (:get m0)]
         (find • (u/kw-cat (:root •) x))))
    opts))

(comment
  (let [p1 (pointed
             :foo
             :foo.a 1
             :a 2
             :b 12)]
    [(p1 :a)
     (p1 :b)]))



;; constructor ---------------------------------

(defn constructor*

  "a constructor can be used in two way
  as a constructor function, with positional arguments
  ex: (constructor [:a :b]) applied to [1 2]
      will yield something equivalent to (fun :a 1 :b 2)
  it can take additional fields too
  ex: (constructor [:a :b] :c 1)
  the resulting object can take extra options too
  (let [c (constructor [:a :b])
        x (c 1 2 :d 4)]
    (equiv x (fun :a 1 :b 2 :d 4)))"

  [keys opts]

  (m

    :apply
    (c [• args]

       (assert (>= (count args) (count keys))
               "constructor args count mismatch")

       (let [[taken rst] (split-at (count keys) args)]
         (m (interleave keys taken)
            rst
            opts)))

    :nest
    (c [• x]
       (println "nest" keys)
       (if-let [r (u/require-keys x keys)]
         (§ (m r) :def opts)
         (u/throw* "dependancy error")))))

(defmacro defc

  "(defc a [b c] :foo 1)
   <=>
   (def a (constructor* [:b :c] :foo 1))"

  [name keys & opts]

  `(def ~name
     (constructor*
       ~(mapv keyword keys)
       (km ~(vec opts)))))

(do

  (((constructor*
      [:r :l]
      (km :a 1 :b 2 :rev (c [•] "boo")))
     1 2) :rev)

  (macroexpand-1
    '(defc pair [l r]
           :rev
           (c [•] (• :def {:l (• :r) :r (• :l)}))))

  (defc pair [l r]
        :rev
        (c [•] (• :def {:l (• :r) :r (• :l)})))

  ((pair 1 2) :rev))

;; to test and understand better

(defmacro a [& xs]
  (let [opts (take-while (complement vector?) xs)
        [argv body & opts*] (u/memf vector? xs)]
    `(m
       :mapply
       (f ~argv ~body)
       :apply
       (c [~'o ~'xs]
          (apply (§ ~'o :mapply)
                 (§ ~'o :host)
                 ~'xs))
       ~@opts
       ~@opts*)))

(do

  (macroexpand '(a [o x] (assoc o :pouet x)
                   :foo 42))

  (let [m1 (m
             :a (a [o x y] (assoc o x y)
                   :foo 42)
             :b 2)]
    [(m1 :a :po 12)
     (m1 :a.foo)]))

;; experiments ----------------------------------

;; destr

(def destructure0
  (m

    :bind
    (c bind
       #_{:debug {:details true}}
       [x pat seed]
       (cond
         (symbol? pat)
         [pat seed]

         (vector? pat)
         (if (keyword? (first pat))
           (let [[fp :as p] pat] (x :cmds fp p seed))
           (x :cmds.zip pat seed))

         (map? pat)
         (reduce
           (fn [acc [pat getter-or-arg]]
               (if (keyword? pat)
                 (into acc (bind x [pat getter-or-arg] seed))
                 (into acc (bind x pat (list getter-or-arg seed)))))
           []
           ;; just put :or cmd at the end...
           (sort-by #(= :or (key %)) pat))))

    :cmds
    (m
      :a 1

      :as
      (f as [[_ s] seed] [s seed])

      :keys
      (f ks [[_ syms] seed]
         (reduce (fn [ret sym]
                     (conj ret sym `(get ~seed ~(keyword sym))))
                 []
                 syms))

      :key
      (f [[_ k sym] seed]
         [sym `(get ~seed ~k)])

      :pick
      (c pick [o [_ pat f] seed]
         (o :bind pat `(~f ~seed)))

      :picks
      (c picks [o [_ m] seed]
         (reduce
           (fn [acc [pat f]]
               (into acc (o :bind [:pick pat f] seed)))
           []
           m))

      :zip
      (c zip [o [_ & xs] seed]
         (reduce
           (fn [acc [idx pat]]
               (into acc (o :bind pat `(nth ~seed ~idx))))
           []
           (map-indexed vector xs)))

      :zip*
      (c zip* [o [_ & xs] seed]
         (vec (concat (o :bind (into [:zip] (butlast xs)) seed)
                      (o :bind (last xs) `(drop ~(dec (count xs)) ~seed)))))

      :or
      (c or* [o [_ a b] seed]
         (if b
           (concat
             (o :bind a seed)
             (o :bind [:or b] seed))
           (reduce
             (fn [acc [k v]]
                 (conj acc k `(or ~k ~v)))
             []
             a))))))

(defn destr [pat seed]
  (destructure0 :bind pat seed))

(assert
  (and
    (destr 'a 1)
    (destr '[a b c] 'seed)
    (destr '[:keys [b a]] 'seed)
    (destr '[:zip a b] 'seed)
    (destr '[:zip a b] '(parse-input (:input fun)))
    (destr '[:zip [:keys [b c]] a] 'seed)
    (destr '[:zip* [:keys [b c]] a [d e]] 'seed)
    (destr '[:or {a 10 b 11}] 'seed)
    (destr '{a :a b :b c :c :as t :or {a 10 c 12}} 'seed)))

;; dispatcher

(def dispatcher0

  (m

    ;; user overwritable fields ----------------------------

    ;; the dispatch table
    :table {}

    ;; the function used on args passed to the :dispatch action
    :take-input identity

    ;; the function that will be called on the input before trying to produce matches
    :conform first

    ;; the function that will be used on each table entry key against :conformed-input, to produces matches.
    ;; can be bypassed if you overide the :match function
    :table-key-match =

    ;; the match function
    :match
    (c [o]
       (filter
         #(o :table-key-match
             (key %)
             (o :conformed-input))
         (o :table)))

    ;; the function that will be used on matches to reduce it
    :shrink identity

    ;; the function that will be called on this if nothing matches
    :no-matches
    (c [o]
       (u/throw* "No matches for input: "
                 (o :input)))

    ;; the function that will be called on this if the dispatch fails
    :dispatch-failure
    (c [o]
       (u/throw* "More than one matches: "
                 (map key (o :matches))))

    ;; the function that will be used to produce a dispatch validation code from this
    :validate-matches
    (c [o]
       (condp = (count (o :matches))
         1 :ok
         0 :no-matches
         :else :dispatch-failure))

    ;; the function used on matches to produce a dispatch result
    :combine-matches
    (c [o]
       (-> (o :matches) first val))

    ;; dispatch-impl-steps ------------------------------

    :impl
    (m

      :hdef
      (c [o x] (o :host.def x))

      :assoc-input
      (c [o input]
         (o :hdef
            [:input (o :take-input input)]))

      :conform-input
      (c [o]
         (o :hdef
            [:conformed-input
             (o :conform (o :input))]))

      :assoc-matches
      (c [o]
         (o :hdef
            [:matches (o :match)]))

      :shrink-matches
      (c [o]
         (o :host.swap
            [:matches (o :shrink)]))

      :assoc-validation-code
      (c [o]
         (o :hdef
            [:dispatch-validation-code
             (o :validate-matches)]))

      :return
      (c [o]
         (condp = (o :dispatch-validation-code)
           :ok (o :combine-matches)
           :dispatch-failure (o :dispatch-failure)
           :no-matches (o :no-matches))))

    ;; --------------------------------------------------

    ;; the main cmd
    :dispatch
    (c [o args]
       (reduce §
               (§ o :impl.assoc-input args)
               [:impl.conform-input
                :impl.assoc-matches
                :impl.shrink-matches
                :impl.assoc-validation-code
                :impl.return]))))

(defn dispatcher
  [& opts]
  (§ dispatcher0 :def opts))

(defn dfn [])

(assert
  (and

    (= 1 ((dispatcher :table {:a 1 :b 2})
           :dispatch [:a]))

    (= 42
       ((dispatcher
          :table-key-match #(%1 %2)
          :table {string? 42 number? 43})
         :dispatch ["er"]))))

(assert
  (let [A (m :tag 'A :val 11)
        B (m :tag 'B :val 2)
        t {['A 'B] 1
           ['B 'A] 2
           ['A 'A] 3
           ['B 'B] 4}
        d1 (dispatcher
             :conform (fn [input] (mapv #(§ % :tag) input))
             :table t)
        d2 (dispatcher
             :conform identity
             :table-key-match
             (fn [[tx ty] [x y]]
                 (and (= (§ x :tag) tx)
                      (= (§ y :tag) ty)))
             :table t)]
    (and
      (= 1 (d1 :dispatch [A B]))
      (= 2 (d1 :dispatch [B A]))
      (= 3 (d2 :dispatch [A A]))
      (= 4 (d2 :dispatch [B B])))))

;; dispatcher serializer

(defn serialize

  "serialize several dispatchers into one
   ex:

   (serialize

      ;; regular dispatcher
      (dispatcher
        :table {:a :a :b :b})

      ;; predicate dispatcher
      (dispatcher
        :table-key-match #(%1 %2)
        :table {string? :str number? :num}))
  "

  [& ds]

  (m

    :dispatchers ds

    :no-matches (:no-matches dispatcher0)

    :dispatch
    (c sd-apply
       [o args]
       (let [[fd & rd :as ds] (o :dispatchers)]
         (if-not (seq ds)
           (o :no-matches)
           (fd
             ;; we overide the no-matches field in order to try the next dispatcher if the current one has no matches.
             ;; note that we could consider overide the dispatch-failure case too...
             :def
             [:no-matches
              (c [o] ((apply serialize rd) :dispatch args))]

             ;; call
             :dispatch args))))))

(assert
  (let [d (serialize

            ;; regular dispatcher
            (dispatcher
              :table {:a 1 :b 2})

            ;; predicate dispatcher
            (dispatcher
              :table-key-match #(%1 %2)
              :table {string? 42 number? 43}))]

    (and (= 1 (d :dispatch [:a]))
         (= 2 (d :dispatch [:b]))
         (= 42 (d :dispatch ["aze"]))
         (= 43 (d :dispatch [2])))))

;; benchmark -----------------------------------------

(comment

  (use 'criterium.core)

  (comment
    (let [o (m :a 1)]
      (quick-bench (o :swap [:a inc])))

    (+ 1 2))

  (macroexpand '(u/time 1000 (o :swap [:a inc])))

  ;; roughly 100 times slower than regular clojure...

  (let [o (m :a 1)]

    {:swap
     (/
       (u/time 1000 (o :swap [:a inc]))
       (u/time 1000 (update o :a inc))
       )

     :get

     (/ (u/time 1000 (o :a))
        (u/time 1000 (get o :a))
        )}))
