(ns mud.deprecated.old
  (:refer-clojure
    :exclude
    [*' * + - /
     map vec seq set str key
     map? seq? set? pos? neg? coll? empty?
     take drop last butlast cons rem
     case chunk loop])
  (:use mud.boot.core)
  (:require [clojure.core :as c]
            [clojure.string :as s]
            [clojure.template :refer [do-template]]))

;; don't judge me

(do
  (def id identity)
  (def k constantly)
  (def p partial)
  (def _ '_)
  (def . '.)
  (def ... '...)

  (do-template
    [k v]
    (def k v)
    add c/+
    sub c/-
    div c//
    mul c/*))

;; guards -------------------------------------------------

(do

  ;; the idea is to eliminate booleans,
  ;; pred that succeed returns the first argument
  ;; or nil in case of failure

  (defn guard [f]
    (fn [x & xs]
      (when (apply f x xs) x)))

  (defn bin-guard [f]
    (let [g (guard f)]
      (fn ([x] #(g % x))
        ([x y] (g x y)))))

  (defn named? [x]
    (or (string? x)
        (keyword? x)
        (symbol? x)))

  (do-template
    [k v]
    (def k (bin-guard v))
    gt >
    lt <
    gte >=
    lte <=
    eq =)

  (do-template
    [k v]
    (def k (guard v))
    pos? c/pos?
    neg? c/neg?
    coll? c/coll?
    linear? sequential?
    named? named?
    empty? c/empty?))

;; invocation, application, mapping -----------------------

(do

  (declare §)

  (defg $
    [x f]
    :map (into {} (c/map (fn [[k v]] [k (§ f v)]) x))
    :set (c/set (c/map (§ f) x))
    :vec (mapv (§ f) x)
    :seq (c/map (§ f) x)
    :any (§ f x))

  (defg $i ;; kind of map-indexed
    [x f]
    :map (into {} (c/map (fn [[k v]] [k (§ f k v)]) x))
    :set (c/set (vals ($i (zipmap x x) f)))
    :vec (c/vec (map-indexed (§ f) x))
    :seq (map-indexed (§ f) x)
    :any (§ f x))

  (defg *'
    [x xs]
    :fun (apply x xs)
    :any ($ x #(*' % xs)))

  (defn * [x & xs]
    (*' x (apply list* xs)))

  (defn §
    ([x] (p § x))
    ([x & xs] (*' x xs)))

  (do

    (§ [add sub] 1 2 3)
    (§ {:+ add :- sub} 1 2 3)
    (§ [{:+ add :- sub :foo [div mul]} min max] 1 2 3)

    ($ [1 -2 3 -8 4] [neg? zero? pos?])

    (let [noop (fn [k v] v)]
      [($i {:a 1 :b 2} noop)
       ($i [0 1 2] noop)
       ($i '(0 1 2) noop)
       ($i #{0 1 2} noop)
       ($i {:a 1 :b 2} vector)
       ($i [0 1 2] vector)
       ($i '(0 1 2) vector)
       ($i #{0 1 2} vector)])))

;; validation ---------------------------------------------

(do
  (generic ?
    [x]
    nil nil
    :vec (when (every? ? x) x)
    :seq (when (every? ? x) x)
    :set (when (every? ? x) x)
    :map (when (every? ? (vals x)) x)
    :any x)

  (defn ! [x f & fs]
    (if fs
      (when-let [x (! x f)]
        (* ! x fs))
      (? (§ f x))))

  (asserts
    (! 1 pos?)
    (! 1 pos? (eq 1))
    (! 1 pos? (p add -2) neg? [id id])
    (not (! 3 pos? (lt 2)))

    ;(! [1 4] #($! % num?))
    )

  (defn flip [f]
    (fn [x y] (f y x)))

  #_(defn &! [& gs]
    #(red % ! gs))

  (defn or!
    ([] id)
    ([p] (guard p))
    ([p & ps]
     (fn [x]
       (or (! x p)
           (! x (* or! ps))))))



  (asserts
    ;((&! num? (gt 0)) 1)
    #_(not ((&! num? (gt 2)) 1))

    #_((or! num? vec?) [] )
    #_((or! num? vec?) 1 ))

  (defn $! [x f & fs]
    (if fs
      (when-let [x ($! x f)]
        (* $! x fs))
      (? ($ x f))))

  (asserts
    ($! [1 2] id)
    (not ($! [1 2] neg?))
    ($! [1 2] pos? (gte 1))
    (not ($! [1 2] pos? (gt 1)))))

;; joining  -----------------------------------------------

(do

  (generic pure
           [_]
           :fun id
           :vec []
           :seq ()
           :map {}
           :set #{}
           :str ""
           :sym (symbol "")
           :key (keyword ""))

  (combination sip
               [a b]
               :vec (conj a b)
               :map (* assoc a b)
               :set (conj a b)
               :seq (concat a [b])
               :str (c/str a (.toString b))
               :sym (symbol (sip (name a) (.toString b)))
               :key (keyword (sip (name a) (.toString b)))
               :fun (p a b))

  (asserts
    (= [1 2 3] (sip [] 1 2 3))
    (= #{1 2 3} (sip #{1} 2 3))
    (= {:a 1 :b 2} (sip {} [:a 1] [:b 2]))
    (= '(1 2 3) (sip () 1 2 3))
    (= :azerty (sip :aze \r \t \y))
    (= 'azerty (sip 'aze \r \t \y))
    (= "azerty" (sip "aze" \r \t \y))
    (= 5 ((sip c/+ 1 1 2) 1)))

  (generic iter
           [a]
           nil ()
           :key (iter (name a))
           :sym (iter (name a))
           :any (c/seq a))

  (combination +
               [a b]
               :fun (comp b a)
               :any (reduce sip a (iter b)))

  (asserts
    (= {:a 4 :b 2 :c 3}
       (+ {:a 1} {:b 2 :c 3} {:a 4}))
    (= [1 2 3 4 5 6]
       (+ [1 2 3] '(4 5 6)))
    (= '(1 2 3 4 5 6)
       (+ '(1 2 3) [4 5 6]))
    (= 0 ((+ inc dec) 0))
    (= 'azerty
       (+ 'aze "rty"))
    (= :azertyiopqs
       (+ :aze "rty" 'iop [\q \s])))
  )

;; constructor and guards ---------------------------------

(do

  (do-template
    [x y]
    (def x (p + y))
    map {}
    vec []
    seq ()
    set #{}
    str ""
    sym (symbol "")
    key (keyword "")
    fun id)

  (do-template
    [x y]
    (def x (guard y))
    map? c/map?
    vec? vector?
    seq? c/seq?
    set? c/set?
    str? string?
    sym? symbol?
    key? keyword?
    num? number?
    fun? fn?))

;; basics -------------------------------------------------

(do

  (generic take
    [x n] (+ (pure x) (c/take n x)))

  (generic drop
    [x n] (+ (pure x) (c/drop n x)))

  (defn splat [x n]
    [(take x n) (drop x n)])

  (generic last
    [x] (c/last (iter x)))

  (generic butlast
    [x] (+ (pure x) (c/butlast (iter x))))

  (generic car
    [x] (c/first (iter x)))

  (generic cdr
    [x] (+ (pure x) (c/rest (iter x))))

  ;; uncons
  (def uncs (§ [car cdr]))

  ;; reversed uncons
  (def runcs (§ [butlast last]))

  (defn cons
    "like core.list*
     but preserve collection type"
    [& xs]
    (let [[cars cdr] (runcs xs)]
      (+ (pure cdr) cars cdr)))

  ;; not empty
  (defn cons? [x]
    (when-not (empty? x) x)))

;; $ variations -------------------------------------------

(do

  (defn red
    "reduce with seed (init) as first argument
     and variadic seq(s) argument (like map does)"
    [• f & xs]
    (if ($! xs cons?)
      (* red
         (* f • ($ xs car))
         f ($ xs cdr))
      •))

  (combination $cat
               [x f]
               (* + (pure x) ($ x f)))

  (defn zip [f x & xs]
    (+ (pure x)
       (* c/map f x xs)))

  (defn zipcat [f x & xs]
    (+ (pure x)
       (* c/mapcat f x xs)))

  (def zip! (+ zip ?))

  ;; ??? not clear
  (defn zipcat! [f & xs]
    (when-let [ret (* zip! f xs)]
      (* + ret)))

  (defn scan [x size step]
    (let [[pre post] (splat x size)]
      (if (cons? post)
        (cons pre (scan (drop x step) size step))
        (sip (pure x) pre))))

  (defn chunk [x size]
    (scan x size size))

  (defn nhts [x n]
    ($ (scan x n n) car))

  (asserts
    (= [[1 2] [3 4]]
       (scan [1 2 3 4] 2 2))
    (= [[1 2] [2 3] [3 4]]
       (scan [1 2 3 4] 2 1))
    (= '((0 1 2 3) (2 3 4))
       (scan (range 5) 4 2))

    (= [[1 2] [3]]
       (chunk [1 2 3] 2)))

  (combination filt
               [a f]
               (+ (pure a)
                  (c/filter f (iter a))))

  (combination rem
               [a f]
               (+ (pure a)
                  (c/remove f (iter a))))

  (asserts
    ($cat [5 6 7]
          (p c/range 0)
          (c/juxt id (p c/+ 10)))
    (= [3 6 9]
       (zip add [1 2 3] [1 2 3] [1 2 3]))
    (zip! (+ add pos?) [0 1] [1 2])
    (not (zip! (+ add pos?) [0 1] [0 2]))
    (= [0 1 2 3 4 5 6 7 8 9 -5 -4 -3 -2 -1 10 11 12 13 14]
       (zipcat range [0 -5 10] [10 0 15]))
    (zipcat! (+ range c/seq) [1 2 3] [5 6 7])
    (zipcat! (+ range #($ % pos?)) [1 2 3] [1 6 7])))

;; dive ---------------------------------------------------

(do

  (defn split-key [k]
    (and (key? k)
         (c/-> (c/name k)
               (s/split #"\.")
               ($ key))))

  (defn simple-key? [k]
    (and (key? k)
         (= 1 (c/count (split-key k)))))

  (defn dotted-key? [k]
    (and (key? k)
         (c/< 1 (c/count (split-key k)))))


  (intergen dive

            [x y]

            :vec
            (c/reduce #(dive %2 %1) y x)

            :key
            (when (c/or (map? y) (set? y))
              (cond
                (simple-key? x) (c/get y x)
                (dotted-key? x) (dive (split-key x) y)))

            :num
            (cond
              (vec? y) (c/get y x)
              (seq? y) (c/nth y x))

            :set
            (map ($ x (fn [x'] [x' (dive x' y)])))

            :map
            ($i x (fn [k f] [k (dive f (dive k y))]))

            :fun (x y)

            :nil y

            [])

  (asserts

    (dive :a
          {:a 1 :b 2 :c 3})

    (dive :b.c
          {:a 1 :b {:c 1} :c 3})

    (dive c/inc 0)

    (dive #{:a :b}
          {:a 1 :b 2 :c 3})

    (dive #{:a :b.c}
          {:a 1 :b {:c 1} :c 3})

    (dive {:a c/inc :b.c 2}
          {:a 1 :b {:c [1 2 'pouet]} :c 3})

    (dive [:b :c c/dec]
          {:a 1 :b {:c 1} :c 3})

    (dive #{[0 0 0] [0 0 2]}
          [[[1 2 3 4]]]))
  )

;; tack ---------------------------------------------------

(do

  (generic tack

    [k x v]

    :vec
    (let [[k & ks] k]
      (if-not k v
        (tack k x
              (tack (vec ks)
                    (dive k x) v))))

    :key
    (cond
      (simple-key? k) (c/assoc (or x {}) k v)
      (dotted-key? k) (tack (split-key k) x v))

    :num
    (cond
      (gt (c/count x) k) (c/assoc x k v)
      (c/nil? x) (sip (vec (c/repeat k nil)) v)
      :else (+ x (tack (sub k (c/count x)) nil v))))

  (defn put [x & xs]
    (red x
      (fn [x [k v]] (tack k x v))
      (chunk xs 2)))

  (defn upd [x & xs]
    (red x
      (fn [x [k f]]
        (tack k x (f (dive k x))))
      (chunk xs 2)))

  (asserts

    (tack [0 0 2] nil 42)
    (tack :a.b {} 1)

    (put nil :a 1)
    (put {} :a.b 1)
    (put nil [0 0 2] 42)
    (put [] [1 2 0] 1 3 89)
    (put {} :a.b 1 :a.p.l [0 1 2])

    (upd [0 [0 1]] [1 0] inc)
    (upd {:a {:b [0 1 2]}}
         [:a :b 1] inc
         :a.c.d (k 42))))

;; match --------------------------------------------------

(do

  (intergen match

    [x y]

    :vec
    (when (linear? y)
      (let [dot (c/-> x butlast last (= .))
            ellipsis (= (last x) ...)
            x (rem x #{... .})
            cx (c/count x)]
        (cond
          dot (zipcat! match (runcs x) (splat y (c/dec cx)))
          ellipsis (match x (take y cx))
          :else (zip! match x y))))

    :map
    (when (map? y)
      ((+ $i ?) x (fn [k v] (match v (dive k y)))))

    :sym
    (c/or (c/and (eq x _) (or y true))
          (eq y x))

    :set
    (when ($! x #(match % y)) y)

    :fun
    ((guard x) y)

    :any
    (eq y x)

    nil (nil? y)

    ;; operations

    [(or [[x & xs] y]
         (c/or (match x y)
               (when xs
                 (match (tpl (or ~@xs)) y))))

     (& [[x & xs] y]
        (c/and (match x y)
               (if xs
                 (match `(& ~@xs) y)
                 y)))])

  (asserts
    (match _ "aze")
    (match nil nil)
    (match _ nil)
    (match _ [4 2])
    (match num? 1)
    (match 1 1)
    (match 'aze 'aze)
    (match [num? str?] [1 "a"])
    (c/not (match [num? str?] [1 2]))
    (match #{num? (p c/< 10)} 12)
    (match {:a num? :b str?} {:a 1 :b "a"})
    (match {:c _} {:a 1 :b "a"})
    (not (match {:a num? :b str?} {:a 1 :b 2}))
    (match (sip '(&) num? (p c/< 10)) 12)
    (match (sip '(or) num? str?) 1)
    (match (sip '(or) num? str?) "a")
    (match (sip '(or) vec? map?)
           {:a 1})
    (match [num? ...] [1 2])
    (match [num? . #($! % num?)] [1 2])
    (match [num? . _] [1 :a "a"])
    (match [num? . _] [1])
    (not (match [num? . #($! % str?)] [1 2]))))

;; bind ---------------------------------------------------

(do

  (declare bindings)

  (intergen bind

            [x y]


            :sym [x y]

            :vec
            (let [[ysym checksym] (gensyms)
                  dot (-> x butlast last (eq '.))
                  ellipsis (= (last x) ...)]
              (cond
                dot (bind (+ '(.) (rem x (eq .))) y)
                ellipsis (bind (+ (butlast x) [. _]) y)
                :else
                (+ [ysym y
                    checksym `(= ~(count x) (count ~ysym))]
                   (zipcat (fn [i e] (bind e (list 'nth ysym i)))
                           (range) x))))

            :map
            (let [ysym (gensym)]
              (* + [ysym y]
                 ($ x (fn [[k e]] (bind e (list 'dive k ysym))))))

            :set
            (let [ysym (gensym)]
              (* + [ysym y]
                 ($ x (fn [p] (bind p ysym)))))

            :any
            [(gensym) (match x y)]



            [(and [xs p]
                  ($cat (vec xs) #(bind % p)))

             (or [xs y]
                 (+ ($cat (last xs) (p * bind))
                    (bind `(and ~@(butlast xs)) y)))

             (ks [xs y]
                 (bind ($ xs key) y))

             (< [[p e] y]
                (let [s (gensym)]
                  (bindings _ y s e p s)))

             (! [[x b] y]
                [(or (gensym) b) `(match ~x ~y)])

             (. [xs y]
                (let [[ysym cars cdr] (gensyms)
                      n (dec (count xs))]
                  (bindings
                    ysym y
                    cars `(take ~ysym ~n)
                    cdr `(drop ~ysym ~n)
                    (vec (butlast xs)) cars
                    (last xs) cdr)))

             (not-found
               [e y]
               (bindings _ y (gensym) e))]
            )

  (defn bindings
    ([x y]
     (bind x y))
    ([x y & xs]
     (+ (bind x y)
        (* bindings xs))))

  (defn ubindings
    "like bindings but add unification constraints
     same syms have to be bound to equals values"
    [& bs]
    (c/loop [ret [] seen #{}
             [a b & nxt] (* bindings bs)]
      (if a
        (if (seen a)
          (recur (conj ret (gensym) `(= ~a ~b)) seen nxt)
          (recur (conj ret a b) (conj seen a) nxt))
        ret)))



  (comment

    (bind '[a b . _] [0 1])
    (bind '[b c . _] (range 10))

    (bind 'a 1)
    (bind ['a 'b] [0 1])
    (bind '[a b . _] [0 1])
    (bind ['a 'b] 1)

    (bind '(pos? _) 1)

    (bind '(! pos?) 1)

    (bind '#{(! [num? num?])
             [a b]}
          [1 2])))

;; case ---------------------------------------------------

(do

  (defn- case-form
    ([form [p1 e1 & xs]]
     (if e1
       `(if (match ~p1 ~form)
          ~e1
          ~(when xs (case-form form xs)))
       p1)))

  (defmacro case
    "core.cond meets match"
    [form & xs]
    (let [v (gensym)]
      `(let [~v ~form]
         ~(case-form v xs))))

  (asserts
    (= :ouf
       (case "aze"
             ;#{neg? num?} :op
             num? :yeah
             `(& ~num? ~pos?) :pos?
             :ouf))

    #_(with [x ['aze 'aze]
             [a b] x]
            (case x
                  [num? num?] (add a b)
                  [named? named?] (+ a b))))
  )

;; with ---------------------------------------------------

(do

  (defn- with-form
    "desugar let like form into lambdas"
    [[b1 b2 & bs] e]
    (tpl
      ((fn [~b1]
         ~(if bs (with-form bs e) e))
        ~b2)))

  (defmacro with
    "like core/let but using bind"
    [bs expr]
    (with-form (* bindings bs) expr))

  (asserts
    (= 2 (with [a 1] (add a a)))
    (= 3 (with [a 1 b 2] (add a b)))
    (= 3 (with [(. a b _) [1 2]] (add a b)))
    (= 6 (with [[x . xs] [1 2 3]] (* add x xs)))
    (= 3 (with [[a b ...] [1 2 3]] (add a b))))

  (defn- with-form!
    "like with-form but shorts in case of nil binding"
    [[b1 b2 & bs] e]
    (tpl
      ((fn [~b1]
         (when (and (not= '_ ~b1) ~b1)
           ~(if bs (with-form! bs e) e)))
        ~b2)))

  (defmacro with!
    "like with but shorts in case of nil binding"
    [bs expr]
    (with-form! (* bindings bs) expr))

  (asserts

    (= 1 (with! [[a b] [0 1]]
                (add a b)))

    (not (with! [[a b] [0 1 2]]
                (add a b)))

    (= 1 (with! [[a b ...] [0 1 2]]
                (add a b)))

    (= 6 (with! [[a b . xs] [0 1 2 3]]
                (apply add a b xs)))

    (= 1 (with! [(. a b _) [0 1]]
                (add a b)))

    (= 1 (with! [(. a b (empty? _)) [0 1]]
                (add a b)))

    (not (with! [(. a b (empty? _)) [0 1 2]]
                (add a b)))

    (= [1 1]
       (with! [#{(! [num? num?]) [a b]} [1 1]]
              [a b]))

    (= [{:a 1} "aze"]
       (with! [#{(! [{:a #{num? (p < 0)}} str?])
                 [a b]} [{:a 1} "aze"]]
              [a b]))

    (not
      (with! [#{(! [{:a num?} num?]) [a b]} [{:a 1} "a"]]
             [a b])))

  (defmacro wuth
    "like with! but with unification
     ex:
     (wuth [[a a] [1 1]] a) ;=> 1
     (wuth [[a a] [1 2]] a) ;=> nil"
    [bs expr]
    (with-form! (* ubindings bs) expr))

  (asserts
    (= 4
       (wuth [[a a b] [1 1 3]]
             (add a b)))

    (not (wuth [[a a b] [1 2 3]]
               (add a b))))

  (defmacro with? [bs e1 e2]
    `(or (with! ~bs ~e1) ~e2))

  (asserts

    (= 1
       (with [a 1]
             (with? [x (pos? a)] x :neg)))

    (= :neg
       (with [a -1]
             (with? [x (pos? a)] x :neg)))))

;; binding case -------------------------------------------

(do

  (defmacro casb
    "like case but binds"
    [e & body]
    (let [s (gensym)]
      `(let [~s ~e]
         (or
           ~@($ (chunk body 2)
                (fn [[pat expr]]
                  (if expr
                    (list `with! [pat s] expr)
                    pat)))))))

  (letfn [(t-casb [x]
            (casb x
                  [a b] :ab
                  [a b c] :abc
                  :pouet))]

    (assert
      (and
        (= :abc (t-casb [1 2 3]))
        (= :ab (t-casb [1 2]))
        (= :pouet (t-casb [1 2 3 4]))))))

;; fns ----------------------------------------------------

(do

  (defmacro f
    "like a single arity core.fn
    but use bind for args"
    ([expr]
     `(f ~(gensym 'f) [~'_] ~expr))
    ([pat expr]
     `(f ~(gensym 'f) ~pat ~expr))
    ([name pat expr]
     `(fn ~name [& xs#]
        (with [~pat xs#] ~expr))))

  (asserts

    (= 2 ((f (add _ _)) 1))

    (= 20
       ((f aze [x] (add x x)) 10))

    (= [1 2 3]
       ((f xs (vec xs)) 1 2 3))

    ;; named with recursion
    (= '(1 2 3)
       ((f lst [x . xs]
           (if (cons? xs)
             (cons x (* lst xs))
             (list x)))
         1 2 3))

    (= [1 '(2 3)]
       ((f (. x xs) [x xs]) 1 2 3)))

  (defmacro loop
    "like clojure loop but uses f"

    ([bindings expr]
     `(loop ~'rec ~bindings ~expr))

    ([name bindings expr]
     (with [xs (chunk bindings 2)]
           `((f ~name ~($ xs car)
                ~expr)
              ~@($ xs last)))))

  (asserts
    (= #{0 1 2 3 4}
       (loop [[x . xs] (range 5)]
             (if (cons? xs)
               (cons x (rec xs))
               #{x}))))

  (defmacro fm
    "pattern matching fn"
    [& form]
    (with [s (gensym)
           named? (sym? (car form))
           [name bform . xs]
           (if named? form (cons (gensym 'fc) form))]

          `(f ~name ~s
              (with [~bform ~s]
                    (case ~s ~@xs)))))

  (do
    (exp (fm [a b]
             [num? num?] (add a b)))

    (let [t (fm rec [a b]
                [num? num?] (add a b)
                [key? named?] (rec b a)
                [named? named?] (+ a b)
                [(tpl (or ~vec? ~map?)) _] (set a))]

      (asserts
        (= 3 (t 1 2))
        (= "azeert" (t "aze" 'ert))
        (= 'foobar (t :bar 'foo))
        (t {:a 1} 42))))

  (defmacro fc
    "pattern matching fn"
    [& body]
    (with [named? (sym? (car body))
           [name . body] (if named? body (cons (gensym 'fc) body))
           s (gensym)]
          `(f ~name ~s (casb ~s ~@body))))

  (do

    (macroexpand-1
      '(fc
         [x] 0
         [x y] #{x y}
         xs (* sub xs)))

    (let [a (fc
              [x] 0
              [x y] #{x y}
              xs (* sub xs))]

      (asserts
        (= 0 (a 1))
        (= #{1 2} (a 1 2))
        (= -8 (a 1 2 3 4)))))

  (defmacro defdefwrap [name wrap & body]
    `(defmacro ~name [name# & body#]
       `(def ~name# (~~wrap ~@body#))))

  (defdefwrap deff `f)
  (defdefwrap defc `fc)
  (defdefwrap defm `fm)

  (asserts

    (deff idf [x] x)

    (idf 42)

    (defm idm [x]
          [num?] :num
          [vec?] :vec)

    (= :num (idm 1))
    (= :vec (idm [1 2]))
    (not (idm 'aze))

    (defc idc
          [(! vec? a)] :vec
          [(! num? a)] :num)

    (= :num (idc 1))
    (= :vec (idc [1 2]))
    (not (idc 'aze)))

  (defn ?>
    "functional some->"
    [seed & xs]
    (loop [s seed [x . xs] xs]
          (with! [s (x s)]
                 (if (cons? xs)
                   (rec s xs)
                   s))))

  (asserts
    (= 3 (?> [1 2 3] (guard (+ count odd?)) last))
    (not (?> [1 2] (guard (+ count odd?)) last)))

  (defmacro conb
    "cond-let(ish)"
    [& xs]

    (with

      [bsym (?> xs car sym?)
       cases (if bsym (cdr xs) xs)
       default (?> cases (guard (+ count odd?)) last)
       cases (if default (butlast cases) cases)]

      `(or ~@($ (chunk cases 2)
                (f [[pat expr]]
                   (if bsym
                     `(with! [~bsym ~pat] ~expr)
                     `(with! ~pat ~expr))))
           ~default)))

  (asserts

    (= [2]
       (with [a [1] b [1 2]]
             (conb
               [[x y] a] [x b]
               [[x . xs] b] xs
               :pouet)))

    ;; default case
    (= :pouet
       (with [a [1] b []]
             (conb
               [[x y] a] [x b]
               [[x . xs] b] xs
               :pouet)))

    ;; with binding sym
    (let [form '(conb x
                      (pos? (sub a b)) (add x x)
                      (neg? (sub a b)) (mul x -1))
          t1 (list 'with '[a 1 b 2] form)
          t2 (list 'with '[a 3 b 2] form)]

      (= [1 2]
         [(c/eval t1)
          (c/eval t2)]))))

