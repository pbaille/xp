;; a previous version of core with regular macro system

(ns mud.deprecated.core
  (:refer-clojure
    :only [defn defn- defmacro comment])
  (:use mud.boot.core)
  (:require [clojure.core :as c]
            [clojure.string :as s]
            [clojure.template :refer [do-template]]
            [mud.boot.utils :as u]))

;; prelude ----------------------------------------------
(do

  ;;essentials
  (def inc c/inc)
  (def dec c/dec)
  (def not c/not)

  ;; lazy man aliases
  (def id c/identity)
  (def k c/constantly)
  (def p c/partial)

  ;; aliases for patterns
  (def _ '_)
  (def . '.)
  (def ... '...)

  ;; those vars will be overitten
  ;; short symbol are precious
  ;; and math code not so common
  (do-template
    [k v]
    (def k v)
    add c/+
    sub c/-
    div c//
    mul c/*))

;; guards -----------------------------------------------
(do

  ;; guards are like preds but
  ;; returns their first argument on success and nil on failure
  ;; it has more compositional power

  (defn guard [f]
    (c/fn [x & xs]
      (c/when (c/apply f x xs) x)))

  ;; TODO 1+ arity guards should be curried on arity one

  (defmacro defguard [name & body]
    `(def ~name (guard (c/fn ~@body))))

  (defn bin-guard [f]
    (c/let [g (guard f)]
      (c/fn ([x] #(g % x))
        ([x y] (g x y)))))

  ;; convert core preds to guards

  (do-template
    [k v]
    (def k (bin-guard v))
    gt c/>
    lt c/<
    gte c/>=
    lte c/<=
    eq c/=
    neq c/not=)

  (do-template
    [k v]
    (def k (guard v))
    pos? c/pos?
    neg? c/neg?
    empty? c/empty?)

  ;; create a predicate for each type/group
  (c/doseq [t (all-types)]
    (c/eval `(def ~(u/symcat t '?)
               (guard (isa ~t)))))

  ;; this guy can not be made a guard
  ;; for obvious reasons :)
  (def nil? c/nil?)

  )

;; invocation, application, mapping ---------------------
(do

  (c/declare § *)

  ;; fmap like
  ;; apply one or several functions inside a datastructure (not deep)
  (defred $
    [x f]
    :map (c/into {} (c/map (c/fn [[k v]] [k (§ f v)]) x))
    :set (c/set (c/map (§ f) x))
    :vec (c/mapv (§ f) x)
    :seq (c/map (§ f) x)
    :any (err "not mappable" x))

  ;; $ indexed
  (defred $i
    [x f]
    :map (c/into {} (c/map (c/fn [[k v]] [k (§ f k v)]) x))
    :set (c/set (c/vals ($i (c/zipmap x x) f)))
    :vec (c/vec (c/map-indexed (§ f) x))
    :seq (c/map-indexed (§ f) x))

  (comment
    (defn $'
      "$ in terms of $i (slower)"
      [x & fs]
      (* $i x ($i fs #(c/fn [_ v] (%2 v)))))
    (time (dotimes [_ 10000] ($> [1 2 3] inc inc)))
    (time (dotimes [_ 10000] ($ [1 2 3] inc inc)))
    (time (dotimes [_ 10000] (->> [1 2 3] (mapv inc) (mapv inc)))))

  (defg *'
    "application generic"
    [x xs]
    :fun (c/apply x xs)
    :set (c/apply x xs)
    :coll ($i x (c/fn [i v] (*' v ($ xs #(c/get % i)))))
    :any x)

  (defn *
    "application"
    ([x] (p * x))
    ([x & xs]
     (*' x (c/apply c/list* xs))))

  (defn §
    "invocation"
    ([x] (p § x))
    ([x & xs] (*' x xs)))

  (defn >
    "thread x thru callables"
    ([x] x)
    ([x f & fs]
     (* > (§ f x) fs)))

  (defn §>
    "pipe callables,
     kind of a reversed core/comp but using §"
    [& fs] #(* > % fs))

  (asserts

    [(c/=
       ;; application
       (* add 1 2 3 [4 5])
       ;; apply is curried on arity 1
       ((* add) 1 2 3 [4 5])
       ;; invocation
       (§ add 1 2 3 4 5)
       ;; curried on arity 1
       ((§ add) 1 2 3 4 5))]

    ;; simple mapping preserving coll type
    (eq [2 3 4]
      ($ [1 2 3] inc))

    ;; on maps funs are applied to values
    (eq {:a 2 :b 3}
      ($ {:a 1 :b 2} inc))

    ;; but $i provide indexes to the funs to
    ;; but cannot modify the keys
    (eq {:a [:a 1], :b [:b 2]}
      ($i {:a 1 :b 2} (c/fn [k e] [k e])))

    ;; with several funs
    (eq #{2 3 4}
      ($ #{1 2 3} inc dec inc))

    ;; invocation
    ;; why not?
    (eq [2 1] (§ [inc dec] [1 2]))

    ;; works with several arguments
    (eq [4 -2] (§ [add sub] [1 2] [3 4]))

    ;; $ is using § under the hood
    ($ [[1 2] [3 4]] [inc dec])

    ;; atomic types return themselves when used as functions
    (eq 42 (§ 42 {:foo 'bar}))

    ;; for now it throws the remaining elements
    ;; but it could not...?
    (eq (§ [9 8] [0 1 2 3]))

    ;; more data invocation
    (eq {:a 1 :b 0} (§ {:a inc :b dec} {:a 0 :b 1}))
    (eq {:a 0 :b 0} (§ {:a add :b sub} {:a 0 :b 1} {:a 0 :b 1}))

    ;; within a mapping
    (eq {:a [2 -2], :b [3 -1]}
      ($ {:a [0 0] :b [1 1]}
        [inc dec]
        [inc dec]))

    (eq {:a [6 -4], :b -7}
      (§ {:a (c/juxt (* add) (* sub))
          :b (* sub)}
        {:a [1 2 3] :b [4 5 6]}))

    ;; threading
    (eq {:a -4}
      (> {:a [1 2 3] :b [4 5 6]}
        {:a (c/juxt (* add) (* sub))
         :b (* sub)}
        {:a (* c/min)})))

  ;; clojure.walk

  (defn walk [in out x]
    (if (coll? x)
      (out ($ x in))
      (out x)))

  (defn dfwalk
    "depth first walk"
    [f x]
    (walk (p dfwalk f) f x))

  (defn bfwalk
    "broad first walk"
    [f x]
    (walk (p bfwalk f) id (f x)))

  (defn walk?
    "x: some data
     ?: if this function succeed, map recur its result
     f: if ? does not succeed f is applied"
    [x ? f]
    (c/if-let [nxt (? x)]
      ($ nxt #(walk? % ? f))
      (f x)))

  (comment

    (def data
      {:a [1 2 3]
       :b {:c #{0 1 {:d 42}}}})

    ;; walking demos

    (bfwalk #(dbg "walked:" %)
      data)

    (dfwalk #(dbg "walked:" %)
      data)

    (walk? data
      #(when (coll? %) (println "out" %) %)
      #(dbg "in" % %)))

  ;; generic?
  (defg df
    "data function,
     create a function from a data structure that
     apply all functions contained in it (deeply) to further args.
     preserve original structure"
    [x]
    :fun x
    :coll
    (c/fn [& xs]
      ($ x #(* (df %) xs)))
    :any (k x))

  (asserts

    (§ (df {:a inc
            :b dec}) 1)

    (§ (df [inc dec]) 1)

    (§ (df {:f (gt 10)
            :e [inc dec]}) 1))

  )

;; joining ----------------------------------------------
(do

  (defg pure
    [_]
    :fun id
    :vec []
    :seq ()
    :map {}
    :set #{}
    :str ""
    :sym (c/symbol "")
    :key (c/keyword "")
    #{:nil :any} nil)

  (defred sip
    [a b]
    :seq (c/concat a [b])
    #{:set :vec} (c/conj a b)
    :map (c/assoc a (c/first b) (c/second b))
    :str (c/str a (.toString b))
    :sym (c/symbol (sip (c/name a) (.toString b)))
    :key (c/keyword (sip (c/name a) (.toString b)))
    :fun (p a b))

  (asserts
    (eq [1 2 3] (sip [] 1 2 3))
    (eq #{1 2 3} (sip #{1} 2 3))
    (eq {:a 1 :b 2} (sip {} [:a 1] [:b 2]))
    (eq '(1 2 3) (sip () 1 2 3))
    (eq :azerty (sip :aze \r \t \y))
    (eq 'azerty (sip 'aze \r \t \y))
    (eq "azerty" (sip "aze" \r \t \y))
    (eq 5 ((sip c/+ 1 1 2) 1)))

  (defg iter
    [a]
    :nil ()
    #{:sym :key} (iter (c/name a))
    :any (c/or (c/seq a) ()))

  (defred +
    [a b]
    :fun (c/comp b a)
    :any (c/reduce sip a (iter b)))

  ;; constructors
  (do-template
    [x y]
    (def x (p + y))
    str ""
    sym (c/symbol "")
    key (c/keyword "")
    fun id)

  ;; wrap
  ;; use first argument to determine targeted type
  (defn wrap [x & xs] (* sip (pure x) xs))
  (defn wrap+ [x & xs] (* + (pure x) xs))
  (def wrap* (* wrap))
  (def wrap+* (* wrap+))

  ;; holy colls impls
  (defmacro iterables [& xs]
    `(do ~@($ (c/partition 2 xs)
             (c/fn [[n e]]
               (c/let [n+ (+ n '+)
                       n* (+ n '*)
                       n+* (+ n '+*)]
                 `(do (def ~n (p sip ~e))
                      (def ~n+ (p + ~e))
                      (def ~n* (* ~n))
                      (def ~n+* (* ~n+))))))))

  (iterables
    seq () vec []
    set #{} map {})

  (asserts

    (wrap [1] 1 2)
    (wrap+ [1] [1 2])
    (wrap* [1] 1 2 '(3 4))
    (wrap+* [1] [1 2] '((3 4) (5 6)))

    (seq? (seq 1 2 3))

    (c/= (seq 1 2 3 4 5)
      (seq* 1 2 3 (seq 4 5))
      (seq+ (seq 1 2 3) (seq 4 5)))

    (nil? (seq? []))
    (nil? (seq? {}))
    (nil? (seq? #{}))

    (c/= (vec 1 2 3 4 5)
      (vec* 1 2 3 (vec 4 5))
      (vec+ (vec 1 2 3) (vec 4 5)))

    (set? (set 1 2 3))
    (nil? (set? []))
    (nil? (set? ()))
    (nil? (set? {}))

    (c/= (set 1 2 3 4 5)
      (set* 1 2 3 (set 4 5))
      (set+ (set 1 2 3) (set 4 5)))

    (c/= {:a 1 :b 2}
      (map [:a 1] [:b 2])
      (map* [:a 1] {:b 2})
      (map+ (map [:a 1]) {:b 2}))

    (map? (map [:a 1]))
    (nil? (map? []))
    (nil? (map? ()))
    (nil? (map? #{})))

  (defg vals [x]
    :map (c/or (c/vals x) ())
    :coll (iter x)
    :any (err "vals: no impl for " x))

  (defg idxs [x]
    :map (c/or (c/keys x) ())
    :set (iter x)
    :coll (c/range (c/count x))
    :any (err "idxs: no impl for " x))

  (defguard pure? [x]
    (c/identical? x (pure x)))

  (asserts
    (eq {:a 4 :b 2 :c 3}
      (+ {:a 1} {:b 2 :c 3} {:a 4}))
    (eq [1 2 3 4 5 6]
      (+ [1 2 3] '(4 5 6)))
    (eq '(1 2 3 4 5 6)
      (+ '(1 2 3) [4 5 6]))
    (eq 0 ((+ inc dec) 0))
    (eq 'azerty
      (+ 'aze "rty"))
    (eq :azertyiopqs
      (+ :aze "rty" 'iop [\q \s]))))

;; basics -----------------------------------------------
(do

  (defn flip [f]
    (curf [a b] (f b a)))

  ;; an helper to wrap clojure.core seq function
  ;; iter first argument and wrap the result
  (c/defmacro iterg [name [a1 :as argv] expr]
    `(defg ~name ~argv
       (c/let [a# ~a1
               ~a1 (iter ~a1)]
         (wrap* a# ~expr))))

  (defg car [x] (c/first (iter x)))
  (defg last [x] (c/last (iter x)))

  (iterg take [x n] (c/take n x))
  (iterg drop [x n] (c/drop n x))
  (iterg butlast [x] (c/butlast x))
  (iterg cdr [x] (c/rest x))

  (asserts

    (def sm1 {:a 1 :b 2 :c 3})

    ;; maps
    (eq {:a 1 :b 2} (take sm1 2))
    (eq {:a 1 :b 2} (butlast sm1))
    (eq {:c 3} (drop sm1 2))
    (eq [:c 3] (last sm1))
    (eq [:a 1] (car sm1))

    ;; syms
    (eq \e (last 'aze))
    (eq \a (car 'aze))
    (eq 'az (butlast 'aze))
    (eq 'az (take 'aze 2))
    (eq 'oobar (cdr 'foobar))


    ;; vecs
    (eq [1 2] (butlast [1 2 3]))
    (eq 3 (last [1 2 3]))
    (eq 1 (car [1 2 3]))

    )

  (defn splat [x n]
    [(take x n) (drop x n)])

  ;; uncons
  (def uncs (df [car cdr]))

  ;; reversed uncons
  (def runcs (df [butlast last]))

  (defn cons
    "like core.list*
     but preserve collection type"
    [& xs]
    (c/let [[cars cdr] (runcs xs)]
      (+ (pure cdr) cars cdr)))

  (defguard cons? [x]
    (c/and (coll? x)
           (not (pure? x))))

  (defn takend [xs n]
    (+ (pure xs) (c/take-last n xs)))

  (defn dropend [xs n]
    (+ (pure xs) (c/drop-last n xs)))

  (defn rev [x]
    (+ (pure x) (c/reverse (iter x))))

  )

;; $ variations -----------------------------------------
(do

  ;(def ?$ (+ $ valid))

  (defn red
    "reduce with seed (init) as first argument
     and variadic seq(s) argument (like map does)"
    [• f & xs]
    (if (c/every? cons? xs)
      (* red
        (* f • ($ xs car))
        f ($ xs cdr))
      •))

  (defred $+
    [x f]
    (* + (pure x) ($ x f)))

  (defn zip [f & xs]
    (* c/map f ($ xs vals)))

  (defn zip+ [f & xs]
    (c/if-let [ret (c/seq (* zip f xs))]
      (* + ret) ()))

  (defn scan [x size step]
    (c/let [[pre post] (splat x size)]
      (if (cons? post)
        (cons pre (scan (drop x step) size step))
        (sip (pure x) pre))))

  (defn chunk [x size]
    (scan x size size))

  (defn nths [x n]
    ($ (scan x n n) car))

  (def braid
    "interleave"
    (p zip+ (p sip ())))

  (asserts
    (eq [[1 2] [3 4]]
      (scan [1 2 3 4] 2 2))
    (eq [[1 2] [2 3] [3 4]]
      (scan [1 2 3 4] 2 1))
    (eq '((0 1 2 3) (2 3 4))
      (scan (c/range 5) 4 2))

    (eq [[1 2] [3]]
      (chunk [1 2 3] 2)))

  (asserts
    ($+ [5 6 7]
      (p c/range 0)
      (c/juxt id (p c/+ 10)))

    (eq '(3 6 9)
      (zip add [1 2 3] [1 2 3] [1 2 3]))

    (eq [1 1 1 2 2 2 3 3 3]
      (zip+ (p sip []) [1 2 3] [1 2 3] [1 2 3]))))

;; compare ----------------------------------------------
(do

  ;; redefining eq
  (defg eq
    ;; build a guard
    ([x] #(eq % x))
    ;; intended to be extended (if needed)
    ([x y]
      :coll
      (c/when (c/identical? (pure x) (pure y))
        ((guard c/=) x y))
      :nil (nil? y)
      :any ((guard c/=) x y))
    ;; variadic
    ([x y & xs]
      (* eq (eq x y) xs)))

  (defguard neq [x y]
    (nil? (eq x y)))

  (asserts
    ;; in clojure this would be considered equal
    (nil? (eq [1 2] '(1 2)))
    ;; eq is a guard and return its first arg
    (c/= [1 2] (eq [1 2] [1 2]))
    ;; neq is a guard too
    (c/= [1 2] (neq [1 2] '(1 2)))
    ;; neq failure
    (nil? (neq [1 2] [1 2]))
    ;; variadic arity
    (eq [1 2] [1 2] [1 2])
    ))

;; composite linear accessors ---------------------------
(do

  (defn- line-accesses
    [size]
    (c/loop [s size ret [['a 'd]]]
      (if (eq 1 s)
        (* + (cdr ret))
        (recur (dec s)
          (sip ret
            ($+ (last ret)
              #($ [\a \d] (p sip %))))))))

  ;; defines cadr caar cdar cddr ...
  ;; cadr :: (cdr (car %)) but maybe cdar should be more readable
  (c/doseq [s (line-accesses 5)]
    (c/eval
      `(def ~(+ 'c s 'r)
         (+ ~@($ (> s iter rev)
                (p c/get `{\a car \d cdr}))))))

  (asserts
    (eq :io
      (cadr [1 :io])
      (caddr [1 2 :io])
      (caadr [1 [:io 2] 3])
      (cadadr [1 [2 :io]]))))

;; flow -------------------------------------------------
(do

  (defn ??
    "id if not nil"
    [x]
    (c/when-not (nil? x) x))

  (defn ?$
    "like every?
     return the result of mapping the guard(s) over x.
     arity one use id as guard"
    ([x] (?$ x id))
    ([x f]
     (c/let [ret ($ x f)]
       (c/when (c/every? ?? (vals ret)) ret)))
    ([x f & fs]
     (red x ?$ (cons f fs))))

  ;; ?zip is to zip what ?$ is to $
  (def ?zip (+ zip ?$))

  (asserts
    ;; this fails because it contains nil
    (nil? (?$ [1 2 nil]))
    ;; fails because -1 is not pos?
    (nil? (?$ [1 2 -1] pos?))
    ;; like $ it can take several functions
    ;; thread subject checking at each step for nils
    (?$ [1 2 0] inc pos?)
    ;; maps: checks values
    (?$ {:a 1 :b 2})
    ;; fails
    (nil? (?$ {:a 1 :b nil}))
    ;; ?$ is an error for non coll subject
    (nil? (comment :err (?$ 1)))
    ;; ?zip
    (?zip #(pos? (add %1 %2)) [1 2 3] [1 2 3])
    (nil? (?zip #(pos? (add %1 %2)) [1 2 3] [1 2 -3])))

  ;; help to define 'filt and 'rem
  (defn filt'
    [a f keep?]
    (red (pure a)
      (c/fn [a v e]
        (if (keep? (§ f v)) (sip a e) a))
      (vals a) a))

  ;; is to $ what core/filter is to core/map
  (defred filt [a f] (filt' a f id))

  ;; is to $ what core/remove is to core/map
  (defred rem [a f] (filt' a f not))

  (asserts
    (eq [1 2 3] (filt [1 2 -1 -2 3] pos?))
    (eq [-1 -2] (rem [1 2 -1 -2 3] pos?)))

  (def ?$i (+ $i ?$))

  (defn ?deep
    "deeply check x"
    [x]
    (if (coll? x)
      (?$ x ?deep)
      (?? x)))

  (asserts
    ;; fails if an inner value is nil
    (nil? (?deep {:a {:b 1 :c [1 2 nil]}}))
    (nil? (?deep {:a {:b 1 :c [1 2 3 {:d nil}]}}))
    ;; succeed
    (?deep {:a {:b 1 :c [1 2 3]}}))

  (defn ?>
    "thread x thru guards
     shorting on first nil result"
    [x f & fs]
    (c/when-let [x (c/and x (§ f x))]
      (if fs (* ?> x fs) x)))

  (asserts
    (eq 1 (?> 1 num? pos?))
    (nil? (?> 1 str? pos?))
    (last [1 2 3])
    (eq 3 (?> [1 2 3] (guard (+ c/count c/odd?)) last))
    (nil? (?> [1 2] (guard (+ c/count c/odd?)) last)))

  (defn ?<
    "thread x thru guards
     until first non nil result"
    [x f & fs]
    (c/or (§ f x)
      (c/when fs (* ?< x fs))))

  (defn §?> [& fs] #(* ?> % fs))
  (defn §?< [& fs] #(* ?< % fs))

  (asserts

    (c/let [f (§?< num? str?)]
      (eq [1 "a" nil]
        [(f 1) (f "a") (f :a)]))

    (eq 42
      (?< 44
        str?
        (§?> num? (gt 10) dec dec))))

  (defn ?c>
    "a scheme-cond(ish) function"
    [x & bs]
    (* ?< x ($ bs (* §?>))))

  (defn ?c
    "a clojure-cond(ish) function"
    [x & cs]
    (* ?c> x (chunk cs 2)))

  (defn ?><
    "at least one guard of each branch have to succeed
    last branch's first success returned"
    [x & bs]
    (* ?> x ($ bs (* §?<))))

  (defn §?c> [& bs] #(* ?c> % bs))
  (defn §?c [& bs] #(* ?c % bs))

  (asserts

    (?c "a"
      str? :pouet
      pos? inc
      neg? dec)

    ((§?c
       str? :pouet
       pos? inc
       neg? dec)
      1)

    (?c> -2
      [str? :pouet]
      [pos? inc inc inc]
      [neg? dec dec (p mul 2)])

    ((§?c> [str? :pouet]
       [pos? inc inc inc]
       [neg? dec dec (p mul 2)])
      2))

  (asserts

    (?> 1 num? (gt 0))
    ((§?> num? (gt 0)) 1)
    (nil? (?> 0 num? (gt 0)))
    (nil? ((§?> num? (gt 0)) 0))

    (?> -1
      num?
      ;(p add -2)
      (c/juxt (p add -2) (§?> (p add 2) pos?))
      car
      neg?
      #(do {:a % :-a (mul % -1)})
      {:-a pos?}
      )))

;; igen -------------------------------------------------
(do

  (do

    (defn- igen-make-vimpl [envsym argv vimpl]
      `(c/let [[v# & args#] ~(car argv)]
         (c/if-let [impl# ((c/deref ~envsym) v#)]
           (impl# args# ~@(cdr argv))
           ~(c/or vimpl `(err "no impl")))))

    (comment igen-patch-vimpl [envsym body]
      (c/let [[argv cases] (uncs body)
              cs1 (c/take-while (p c/not= :vec) cases)
              [_ vimpl & cs2] (c/drop-while (p c/not= :vec) cases)
              impl (igen-make-vimpl envsym argv vimpl)]
        (+ [argv :vec impl] cs1 cs2)))

    (defn- igen-patch-vimpl [envsym body]
      (c/let [[argv cases] (uncs body)
              cs (chunk cases 2)
              filt #(not (type<= :vec (car %)))
              flat1 #($+ (c/or % ()) id)
              cs1 (c/take-while filt cs)
              [[t vimpl] & cs2] (c/drop-while filt cs)
              impl (igen-make-vimpl envsym argv vimpl)]
        (+ [argv :vec impl] (c/when-not (eq t :vec) [t vimpl]) (flat1 cs1) (flat1 cs2))))

    (defn- igen-init-env [envsym extsym ops]
      `[(def ~envsym (c/atom {}))
        (defmacro ~extsym [& ops#]
          (c/list c/swap! '~envsym +
            (c/zipmap ($ (c/or ops# []) car key)
              ($ (c/or ops# []) (p cons `c/fn)))))
        (~extsym ~@ops)]))

  (defmacro igen
    "a generic function
     that use custom environment to expand its first argument"
    [nam & body]
    (c/let [envsym (+ nam '-env)
            extsym (+ nam '+)
            [doc body] (if (str? (car body)) (uncs body) [nil body])
            [body ops] (runcs body)]
      `(do
         (c/declare ~nam)
         ~@(igen-init-env envsym extsym ops)
         (defg ~nam ~@(igen-patch-vimpl envsym body))
         #'~nam)))

  (do

    (igen igt [a b]
      :fun (a b)
      :vec (vec+ (zip igt a b))
      [(ju [[c d] e] ((c/juxt c d) e))
       (jus [xs e] ($ xs (p + [:ju]) #(igt % e)))])

    (asserts
      (eq [3 1] (igt [:ju inc dec] 2))
      (eq [2 1] (igt [inc dec] [1 2]))
      (eq [[2 0] 2] (igt [[:ju inc dec] pos?] [1 2]))
      (eq '([1 -1] [-1 1]) (igt [:jus [inc dec] [dec inc]] 0))
      (eq 2 (igt (+ inc dec) 2))))

  )

;; dive -------------------------------------------------
(do

  (igen dive

    [x y]

    :vec
    (c/reduce (flip dive) y x)

    #{:key :sym}
    (c/when (c/or (map? y) (c/record? y) (set? y))
      (c/get y x))

    :num
    (u/gat y x)

    :set
    (+ {} ($ (iter x) (c/fn [x'] [x' (dive x' y)])))

    :map
    ($i x (c/fn [k f] [k (dive f (dive k y))]))

    :fun (§ x y)

    :nil y

    [])

  (asserts

    #_(dive :a nil)

    (dive :a
      {:a 1 :b 2 :c 3})

    (dive [:b :c]
      {:a 1 :b {:c 1} :c 3})

    (dive c/inc 0)

    (dive #{:a :b}
      {:a 1 :b 2 :c 3})

    (dive #{:a [:b :c]}
      {:a 1 :b {:c 1} :c 3})

    (dive {:a c/inc [:b :c] 2}
      {:a 1 :b {:c [1 2 'pouet]} :c 3})

    (dive [:b :c c/dec]
      {:a 1 :b {:c 1} :c 3})

    (dive #{[0 0 0] [0 0 2]}
      [[[1 2 3 4]]])))

;; tack -------------------------------------------------
(do

  (igen tack

    [k x v]

    :vec
    (c/let [[k & ks] k]
      (c/if-not k v
        (tack k x
          (tack (vec+ ks)
            (dive k x) v))))

    :num
    (u/when! (c/or (line? x) (nil? x))
      (c/cond
        (gt (c/count x) k)
        (c/cond
          (seq? x) (+ (take x k) (seq v) (drop x (inc k)))
          (vec? x) (c/assoc x k v))
        (nil? x) (sip (vec+ (c/repeat k nil)) v)
        :else (+ x (tack (sub k (c/count x)) nil v))))

    :any
    (u/when! (c/or (map? x) (c/record? x) (nil? x))
      (c/assoc (c/or x {}) k v))

    [])

  (defn put [x & xs]
    (red x
      (c/fn [x [k v]] (tack k x v))
      (chunk xs 2)))

  (defn upd [x & xs]
    (red x
      (c/fn [x [k f]]
        (tack k x (f (dive k x))))
      (chunk xs 2)))




  (asserts

    ;; tack

    (eq [[[nil nil 42]]]
      (tack [0 0 2] nil 42))

    (eq {:a {:b 1}}
      (tack [:a :b] {} 1))

    (tack :a {:a 1} 2)

    ;; put

    (eq {:a 1}
      (put nil :a 1))

    (eq {:a {:b 1}}
      (put {} [:a :b] 1))

    (eq [[[nil nil 42]]]
      (put nil [0 0 2] 42))

    (eq [nil [nil nil [1]] nil 89]
      (put [] [1 2 0] 1 3 89))

    (eq {:a {:b 1, :p {:l [0 1 2]}}}
      (put {} [:a :b] 1 [:a :p :l] [0 1 2]))

    ;; upd

    (eq [0 [1 1]]
      (upd [0 [0 1]] [1 0] inc))

    (eq {:a {:b [0 2 2], :c {:d 42}}}
      (upd {:a {:b [0 1 2]}}
        [:a :b 1] inc
        [:a :c :d] (k 42)))

    ))

;; match ------------------------------------------------
(do

  (igen match

    [x y]

    :vec
    (c/when (line? y)
      (c/let [dot (c/-> x butlast last (eq .))
              ellipsis (eq (last x) ...)
              x (rem x #{... .})
              cx (c/count x)]

        (c/cond
          ellipsis
          (match (+ x [. id]) y)

          dot
          (?> (?zip match
                (runcs x)
                (splat y (c/dec cx)))
            (p * + (pure x)))

          :else
          (c/and (eq (c/count x) (c/count y))
                 (?zip match x y)))))

    :map
    (c/when (map? y)
      (?$i x (c/fn [k v] (match v (dive k y)))))

    :sym
    (c/or (c/and (eq x _) (c/or y true))
      (eq y x))

    :set
    (c/when (?$ x #(match % y)) y)

    :fun ((guard x) y)

    :nil (nil? y)

    :any (eq y x)

    [(or [xs y]
       (c/when (cons? xs)
         (c/or (match (car xs) y)
           (or (cdr xs) y))))
     (& [xs y]
       (c/if-not (cons? xs) y
         (c/and (match (car xs) y)
                (& (cdr xs) y))))])

  (asserts
    (match _ "aze")
    (match nil nil)
    (match _ nil)
    (match _ [4 2])
    (match num? 1)
    (match 1 1)
    (match 'aze 'aze)
    (match [num? str?] [1 "a"])
    (exp (match [num? [:or str? key?]] [1 "a"]))
    (nil? (match [num? str?] [1 "a" 2]))
    (nil? (match [num? str?] [1 2]))
    (match #{num? (p c/< 10)} 12)
    (match {:a num? :b str?} {:a 1 :b "a"})
    (match {:c _} {:a 1 :b "a"})
    (nil? (match {:a num? :b str?} {:a 1 :b 2}))
    (nil? (match [:& num? (gt 10) neg?] 12))
    (nil? (match [:& num? (gt 10) neg?] 12))
    (match [:& num? [:& (gt 10) pos?]] 12)
    (match [:or num? str?] 1)
    (nil? (match [:or num? str?] :a))
    (match [:or num? str? sym?] "a")
    (match [:or num? str? [:or sym? key?]] 'a)
    (match [:or vec? map?] {:a 1})
    ;; TODO
    (match [num? ...] [1 2])
    (match [num? . #(?$ % num?)] [1 2])
    (match [num? . _] [1 :a "a"])
    (match [num? . _] [1])
    (nil? (match [num? . #(?$ % str?)] [1 2]))))

;; bind -------------------------------------------------
(do

  (c/declare bindings)

  (igen bind

    [x y]


    :sym [x y]

    :vec
    (c/let [[ysym checksym] (u/gensyms)
            dot (c/-> x butlast last (eq '.))
            ellipsis (u/member? x ...)]

      (c/cond
        dot (bind (+ [:.] (rem x (eq .))) y)
        ellipsis (bind (+ [:...] x) y)
        :else
        (+ [ysym y
            checksym `(c/and (line? ~ysym) (eq ~(c/count x) (c/count ~ysym)))]
          (zip+ (c/fn [i e] (bind e `(dive ~i ~ysym)))
            (c/range) x))))

    :map
    (c/let [ysym (c/gensym)]
      (* + [ysym y]
        ($ (iter x) (c/fn [[k e]] (bind e `(dive ~k ~ysym))))))

    :set
    (c/let [ysym (c/gensym)]
      (* + [ysym y]
        ($ x (c/fn [p] (bind p ysym)))))

    :any
    [(c/gensym) `(match ~x ~y)]

    ;; ops
    [(& [xs p]
       ($+ (vec+ xs) #(bind % p)))

     (or [xs y]
       (+ (* bindings (last xs))
         (bind (vec* :& (butlast xs)) y)))

     (ks [xs y]
       (bind (+ {} ($ xs (df [key id]))) y))

     (< [[p e] y]
       (c/let [s (c/gensym)]
         (bindings _ y s e p s)))

     (! [[x b] y]
       [(c/or b (c/gensym)) `(match ~x ~y)])

     (. [xs y]
       (c/let [[ysym check cars cdr] (u/gensyms)
               n (dec (c/count xs))]
         (bindings
           ysym y
           check `(line? ~ysym)
           cars `(take ~ysym ~n)
           cdr `(drop ~ysym ~n)
           (vec+ (butlast xs)) cars
           (last xs) cdr)))

     (... [xs y]
       (c/let [[a [_ & b]] (splat xs (u/indexof xs '...))
               queue-cnt (c/count b)]
         (bindings
           (+ [:.] a) `(dropend ~y ~queue-cnt)
           (+ [] b) `(takend ~y ~queue-cnt))))

     (not-found
       [e y]
       (bindings _ y (c/gensym) e))]
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
          (recur (sip ret (c/gensym) `(eq ~a ~b)) seen nxt)
          (recur (sip ret a b) (sip seen a) nxt))
        ret)))

  (asserts

    (c/eval `(c/let ~(bind '[a b ... c d] [0 1 2 3 4 5]) [~'a ~'b ~'c ~'d]))
    (c/eval `(c/let ~(bind '[a b ... c d] [0 1 2 3]) [~'a ~'b ~'c ~'d]))
    (c/eval `(c/let ~(bind '[a b e ... c d] [0 1]) [~'a ~'b ~'e ~'c ~'d]))
    (c/eval `(c/let ~(bind '[a b c ...] [0 1]) [~'a ~'b ~'c]))
    (bind '[a b . _] [0 1])
    (bind '[a b ...] [0 1])
    (bind '[b c . _] (c/range 10))

    (bind 'a 1)
    (bind '(num? a) 1)
    (bind '[a b] [0 1])
    (bind '[a b . _] [0 1])
    (bind '[a b] 1)
    (bind '[:! pos? a] 1)
    (bind '#{[:! [num? num?]] [a b]} [1 2])
    (bind '{:a a :b b} 'pouet)
    (bind '[:ks a b] 'pouet)
    (bind '[:& a b c] 1)
    (bind '[:. a b c] [1 2 3])
    ;; this will not work with unification
    (bind '[:or [a b . cs] [cs [1 2 3]]] 1)
    ;; do somethig with right part before binding
    (bind '[:< [a b c] (vec+ _)] #{1 2 3})

    ))

;; case -------------------------------------------------
(do

  (defn- case-form
    ([form [p1 e1 & xs]]
     (if e1
       `(if (match ~p1 ~form)
          ~e1
          ~(c/when xs (case-form form xs)))
       p1)))

  (defmacro case
    "core.cond meets match"
    [form & xs]
    (c/let [v (c/gensym)]
      `(c/let [~v ~form]
         ~(case-form v xs))))

  (asserts
    (eq :unknown
      (case 'foo ;-1
        ;#{neg? num?} :op
        [:& num? pos?] :posnum
        num? :num
        :unknown)))
  )

;; with -------------------------------------------------
(do

  (defn with-form
    "desugar let like form into lambdas"
    [[b1 b2 & bs] e]
    (tpl
      ((~`c/fn [~b1]
         ~(if bs (with-form bs e) e))
        ~b2)))

  (defmacro with
    "like core/let but using bind"
    [bs expr]
    (with-form (* bindings bs) expr))

  (asserts
    (eq 2 (with [a 1] (add a a)))
    (eq 3 (with [a 1 b 2] (add a b)))
    (eq 3 (with [[a b . _] [1 2]] (add a b)))
    (eq 6 (with [[x . xs] [1 2 3]] (* add x xs)))
    (eq 6 (with [[a b ...] [1 2 3]] (* add a b)))
    (eq 2 (with [[:& a b] 1] (add a b)))
    (eq 2 (with [[:< a (pos? _)] 1] (add a a)))
    (eq 6 (with [[:ks a b c] {:a 1 :b 2 :c 3}] (add a b c)))
    (eq 6 (with [[:< [a b c] (vec+ _)] #{1 2 3}] (add a b c))))

  (defn with-form!
    "like with-form but shorts in case of nil binding"
    [[b1 b2 & bs] e]
    (tpl
      ((~`c/fn [~b1]
         (~`c/when ~(c/or (eq \_ (car b1)) b1)
           ~(if bs (with-form! bs e) e)))
        ~b2)))

  (defmacro with!
    "like with but shorts in case of nil binding"
    [bs expr]
    (with-form! (* bindings bs) expr))

  (asserts

    (exp (with! [[a _] [1 nil]] :pouet))

    (eq 1 (with! [[a b] [0 1]]
            (add a b)))

    (not (with! [[a b] [0 1 2]]
           (add a b)))

    (eq 3 (with! [[a b ...] [0 1 2]]
            (* add a b)))

    (eq 6 (with! [[a b . xs] [0 1 2 3]]
            (* add a b xs)))

    (eq 1 (with! [[:. a b _] [0 1]]
            (add a b)))

    (eq 1 (with! [[a b . [:! empty?]] [0 1]]
            (add a b)))

    (not (with! [[a b . [:! empty?]] [0 1 2]]
           (add a b)))

    (eq [1 1]
      (with! [#{[:! [num? num?]] [a b]} [1 1]]
        [a b]))

    (bind '[42 a] '[42 1])

    (eq [{:a 1} "aze"]
      (with! [#{[:! [{:a #{num? (gt 0)}} str?]]
                [a b]} [{:a 1} "aze"]]
        [a b]))

    (not
      (with! [#{[:! [{:a num?} num?]] [a b]} [{:a 1} "a"]]
        [a b])))

  (defmacro wuth
    "like with! but with unification
     ex:
     (wuth [[a a] [1 1]] a) ;eq> 1
     (wuth [[a a] [1 2]] a) ;eq> nil"
    [bs expr]
    (with-form! (* ubindings bs) expr))

  (asserts
    (eq 4
      (wuth [[a a b] [1 1 3]]
        (add a b)))

    (not (wuth [[a a b] [1 2 3]]
           (add a b))))

  (defmacro with? [bs e1 e2]
    `(c/or (with! ~bs ~e1) ~e2))

  (asserts

    (eq 1
      (with [a 1]
        (with? [x (pos? a)] x :neg)))

    (eq :neg
      (with [a -1]
        (with? [x (pos? a)] x :neg)))))

;; binding case -----------------------------------------
(do

  (defmacro casb
    "like case but binds"
    [e & body]
    (c/let [s (c/gensym)]
      `(c/let [~s ~e]
         (c/or
           ~@($ (chunk body 2)
               (c/fn [[pat expr]]
                 (if expr
                   (c/list `with! [pat s] expr)
                   pat)))))))

  (c/letfn [(t-casb [x]
              (casb x
                [a b] :ab
                [a b c] :abc
                :pouet))]

    (asserts
      (eq :abc (t-casb [1 2 3]))
      (eq :ab (t-casb [1 2]))
      (eq :pouet (t-casb [1 2 3 4])))))

;; fns --------------------------------------------------
(comment

  (defmacro f
    "like a single arity core.fn
    but use bind for args"
    ([expr]
     `(f ~(c/gensym 'f) [~'_] ~expr))
    ([pat expr]
     `(f ~(c/gensym 'f) ~pat ~expr))
    ([name pat expr]
     `(c/fn ~name [& xs#]
        (with [~pat xs#] ~expr))))

  (asserts

    (eq 2 ((f (add _ _)) 1))

    (eq 20
      ((f aze [x] (add x x)) 10))

    (eq [1 2 3]
      ((f xs (vec+ xs)) 1 2 3))

    ;; named with recursion
    (eq '(1 2 3)
      ((f lst [x . xs]
         (if (cons? xs)
           (cons x (* lst xs))
           (c/list x)))
        1 2 3))

    (eq [1 '(2 3)]
      ((f [x . xs] [x xs]) 1 2 3)))

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
    (eq #{0 1 2 3 4}
      (loop [[x . xs] (c/range 5)]
        (if (cons? xs)
          (cons x (rec xs))
          #{x}))))

  (defmacro fm
    "case fn"
    [& body]
    (with [[name . body]
           (if (sym? (car body)) body (cons (c/gensym 'fm) body))
           [doc bform . xs]
           (if (str? (car body)) body (cons "" body))
           s (c/gensym)]

      `(f ~name ~s
         (with [~bform ~s]
           (case ~s ~@xs)))))

  (do
    (exp (fm [a b]
           [num? num?] (add a b)))

    (c/let [t (fm rec [a b]
                [num? num?] (add a b)
                [key? word?] (rec b a)
                [word? word?] (+ a b)
                [[:or map? vec?] _] (set a))]

      (asserts
        (eq 3 (t 1 2))
        (eq "azeert" (t "aze" 'ert))
        (eq 'foobar (t :bar 'foo))
        (t {:a 1} 42))))

  (defmacro fc
    "pattern matching fn"
    [& body]
    (with [[name . body]
           (if (sym? (car body)) body (cons (c/gensym 'fc) body))
           [doc . body]
           (if (str? (car body)) body (cons "" body))
           s (c/gensym)]
      `(f ~name ~s (casb ~s ~@body))))

  (do

    (exp
      '(fc
         [x] 0
         [x y] #{x y}
         xs (* sub xs)))

    (c/let [a (fc
                [x] 0
                [x y] #{x y}
                xs (* sub xs))]

      (asserts
        (eq 0 (a 1))
        (eq #{1 2} (a 1 2))
        (eq -8 (a 1 2 3 4)))))

  (defmacro defdefwrap [name wrap]
    `(defmacro ~name [name# & body#]
       `(def ~name# (~~wrap ~@body#))))

  (defdefwrap deff `f)
  (defdefwrap defc `fc)
  (defdefwrap defm `fm)

  (asserts

    (deff idf [x] x)
    (eq 42 (idf 42))

    (deff tf1 [a b ... c d] (add a c d))
    (eq 12 (tf1 1 2 3 4 5 6))

    (defm idm [x]
      [num?] :num
      [vec?] :vec)

    (eq :num (idm 1))
    (eq :vec (idm [1 2]))
    (nil? (idm 'aze))

    (defm idm2 [a b]
      [num? num?] :numnum
      [vec? num?] :vecnum)

    (eq :numnum (idm2 1 2))
    (eq :vecnum (idm2 [] 1))
    (nil? (idm2 'a 'a))

    (defc idc
      [[a b c]] [:vec3 a b c]
      [[:! vec? a]] [:vec a]
      [[:! num? a]] [:num a])

    (eq [:num 1] (idc 1))
    (eq [:vec [1 2]] (idc [1 2]))
    (eq [:vec3 1 2 3] (idc [1 2 3]))

    (nil? (idc 'aze))

    (defc c1
      [x [y]] :one
      [x [y . ys]] :two
      [x y] :three)

    (eq :one (c1 1 [1]))
    (eq :two (c1 1 [1 2]))
    (eq :three (c1 1 2))

    )


  (defmacro defdefwrap-m [name wrap]
    `(defmacro ~name [name# & body#]
       `(do (def ~(sym name# 'fn) (~~wrap ~@body#))
            (defmacro ~name# [& ~'~'xs]
              (* ~(sym name# 'fn) ~'~'xs)))))

  (defdefwrap-m deff-m `f)
  (defdefwrap-m defc-m `fc)
  (defdefwrap-m defm-m `fm)

  (do
    (defc-m yop
      [a] :pouet
      [a b] :flute
      xs :pluc)

    (asserts
      (eq :pluc (exp (yop 1 2 3)))
      (eq :flute (exp (yop 1 2)))
      (eq :pouet (exp (yop 1)))))



  (defmacro conb
    "cond-let(ish)"
    [& xs]

    (with

      [bsym (?> xs car sym?)
       cases (if bsym (cdr xs) xs)
       default (?> cases (guard (+ c/count c/odd?)) last)
       cases (if default (butlast cases) cases)]

      `(c/or ~@($ (chunk cases 2)
                 (f [[pat expr]]
                   (if bsym
                     `(with! [~bsym ~pat] ~expr)
                     `(with! ~pat ~expr))))
         ~default)))

  (asserts

    (eq [2]
      (with [a [1] b [1 2]]
        (conb
          [[x y] a] [x b]
          [[x . xs] b] xs
          :pouet)))

    ;; default case
    (eq :pouet
      (with [a [1] b []]
        (conb
          [[x y] a] [x b]
          [[x . xs] b] xs
          :pouet)))

    ;; with binding sym
    (c/let [form '(conb x
                    (pos? (sub a b)) (add x x)
                    (neg? (sub a b)) (mul x -1))
            t1 (c/list 'with '[a 1 b 2] form)
            t2 (c/list 'with '[a 3 b 2] form)]

      (eq [1 2]
        [(c/eval t1)
         (c/eval t2)])))

  (defmacro ?>as
    "like core/some->as but handle composite values
     e.g: [true nil] shorts, as {:a {:b nil} :c 'foo}"
    [s x & xs]
    `(loop [v# ~x
            xs#
            ~($ (vec+ xs)
               (f rec [x]
                 (c/cond
                   (seq? x) (seq `f [s] x)
                   (coll? x) (seq `f [s] `((df ~($ x rec)) ~s))
                   (sym? x) x
                   :else (seq `f x))))]
       (c/when (?deep v#)
         (if (cons? xs#)
           (~'rec ((car xs#) v#) (cdr xs#))
           v#))))

  (defmacro ?>_ [& xs]
    `(?>as ~'_ ~@xs))

  (asserts
    ;; those 3 forms are semantically equivalent
    (c/=
      ;; [1 2 3] is threaded thru all expressions as x
      (?>as x [1 2 3] (cdr x) (cons? x) (pos? (car x)) (mul x 4))
      ;; shortcut syntax (threaded as _)
      (?>_ [1 2 3] (cdr _) (cons? _) (pos? (car _)) (mul _ 4))
      ;; symbols are interpreted as single arity functions
      (?>_ [1 2 3] cdr cons? (pos? (car _)) (mul _ 4)))

    ;; if an expr is not a seq it is functionized via 'df
    (?>_ [1 2 3]
      {:a (* add _) :b (* sub _) :c (pos? (car _)) :nested {:cons cons?}}
      (dive [:nested :cons] _))
    ;; if the result of an expr (here data function) is not deeply valid it fails
    (nil? (?>_ [1 2 3] {:a (* add _) :b (* sub _) :c (neg? (car _))})))

  (defmacro or
    "like core/or but deep checks"
    [& xs]
    `(c/or ~@($ xs (f (seq `?deep _)))))

  (defmacro or>
    "each member is wrapped with ?>_
     scheme-cond(ish)"
    [& xs]
    `(c/or ~@($ xs (f (seq* `?>_ _)))))

  (asserts
    ;; like
    (or [1 2 nil] {:a 1 :b {:c nil}} #{1 2 3})
    (or>
      [1 (mul -4 _) pos? :aze]
      [-1 (mul -4 _) pos? :baz]))

  )

;; fns redo ---------------------------------------------
(do

  (defn parse-fform [[fb & rb]]
    (with [[name [fb . rb]]
           (if (key? fb) [fb rb] [(key (c/gensym 'f)) (cons fb rb)])
           [doc body]
           (if (str? fb) [fb rb] ["" (cons fb rb)])]
      {:name name
       :doc doc
       :body body
       :form (vec* name doc body)}))

  (with [[:ks name doc body form] (parse-fform '(xs xs))]
    [name doc body form])

  (defmacro f
    ;; anonymous unary function
    ;; arg is bound to _
    ;; (f (add _ _)) <=> #(add % %)
    ([e] `(f [~'_] ~e))
    ([fb & rb]
     (with [[:ks name doc body] (parse-fform (cons fb rb))
            [pat body] (uncs body)]
       `(c/fn ~(sym name) [& xs#]
          (with [~pat xs#] (do ~@body))))))

  ((f :foo [x . xs]
     (if (cons? xs)
       (dbg x (* foo xs))
       x)) 1 2 3)

  (asserts

    (eq 2 ((f (add _ _)) 1))

    (eq 20
      ((f :aze [x] (add x x)) 10))

    (eq [1 2 3]
      ((f xs (vec+ xs)) 1 2 3))

    ;; named with recursion
    (eq '(1 2 3)
      ((f :lst
         "docstr"
         [x . xs]
         (if (cons? xs)
           (cons x (* lst xs))
           (c/list x)))
        1 2 3))

    (eq [1 '(2 3)]
      ((f [x . xs] [x xs]) 1 2 3)))

  (defmacro loop
    "like clojure loop but uses f"

    ([bindings expr]
     `(loop :rec ~bindings ~expr))

    ([name bindings expr]
     (with [xs (chunk bindings 2)]
       `((f ~name ~($ xs car)
           ~expr)
          ~@($ xs last)))))

  (asserts
    (eq #{0 1 2 3 4}
      (loop [[x . xs] (c/range 5)]
        (if (cons? xs)
          (cons x (rec xs))
          #{x}))))

  (defmacro fm
    "case fn"
    [& body]
    (with [[:ks name doc body] (parse-fform body)
           [bform . xs] body
           s (c/gensym)]

      `(f ~name ~s
         (with [~bform ~s]
           (case ~s ~@xs)))))

  (do
    (exp (fm [a b]
           [num? num?] (add a b)))

    (c/let [t (fm :rec [a b]
                [num? num?] (add a b)
                [key? word?] (rec b a)
                [word? word?] (+ a b)
                [[:or map? vec?] _] (set a))]

      (asserts
        (eq 3 (t 1 2))
        (eq "azeert" (t "aze" 'ert))
        (eq 'foobar (t :bar 'foo))
        (t {:a 1} 42))))

  (defmacro fc
    "pattern matching fn"
    [& body]
    (with [[:ks name doc body] (parse-fform body)
           s (c/gensym)]
      `(f ~name ~s (casb ~s ~@body))))

  (do

    (exp
      (fc
        [x] 0
        [x y] #{x y}
        xs (* sub xs)))

    (c/let [a (fc
                [x] 0
                [x y] #{x y}
                xs (* sub xs))]

      (asserts
        (eq 0 (a 1))
        (eq #{1 2} (a 1 2))
        (eq -8 (a 1 2 3 4)))))

  (defmacro defdefwrap [name wrap]
    `(defmacro ~name [name# & body#]
       `(def ~name# (~~wrap ~@body#))))

  (defdefwrap deff `f)
  (defdefwrap defc `fc)
  (defdefwrap defm `fm)

  (asserts

    (deff idf [x] x)
    (eq 42 (idf 42))

    (deff tf1 [a b ... c d] (add a c d))
    (eq 12 (tf1 1 2 3 4 5 6))

    (defm idm [x]
      [num?] :num
      [vec?] :vec)

    (eq :num (idm 1))
    (eq :vec (idm [1 2]))
    (nil? (idm 'aze))

    (defm idm2 [a b]
      [num? num?] :numnum
      [vec? num?] :vecnum)

    (eq :numnum (idm2 1 2))
    (eq :vecnum (idm2 [] 1))
    (nil? (idm2 'a 'a))

    (defc idc
      [[a b c]] [:vec3 a b c]
      [[:! vec? a]] [:vec a]
      [[:! num? a]] [:num a])

    (eq [:num 1] (idc 1))
    (eq [:vec [1 2]] (idc [1 2]))
    (eq [:vec3 1 2 3] (idc [1 2 3]))

    (nil? (idc 'aze))

    (defc c1
      [x [y]] :one
      [x [y . ys]] :two
      [x y] :three)

    (eq :one (c1 1 [1]))
    (eq :two (c1 1 [1 2]))
    (eq :three (c1 1 2))

    )


  (defmacro defdefwrap-m [name wrap]
    `(defmacro ~name [name# & body#]
       `(do (def ~(sym name# 'fn) (~~wrap ~@body#))
            (defmacro ~name# [& ~'~'xs]
              (* ~(sym name# 'fn) ~'~'xs)))))

  (defdefwrap-m deff-m `f)
  (defdefwrap-m defc-m `fc)
  (defdefwrap-m defm-m `fm)

  (do
    (defc-m yop
      [a] :pouet
      [a b] :flute
      xs :pluc)

    (asserts
      (eq :pluc (exp (yop 1 2 3)))
      (eq :flute (exp (yop 1 2)))
      (eq :pouet (exp (yop 1)))))



  (defmacro conb
    "cond-let(ish)"
    [& xs]

    (with

      [bsym (?> xs car sym?)
       cases (if bsym (cdr xs) xs)
       default (?> cases (guard (+ c/count c/odd?)) last)
       cases (if default (butlast cases) cases)]

      `(c/or ~@($ (chunk cases 2)
                 (f [[pat expr]]
                   (if bsym
                     `(with! [~bsym ~pat] ~expr)
                     `(with! ~pat ~expr))))
         ~default)))

  (asserts

    (eq [2]
      (with [a [1] b [1 2]]
        (conb
          [[x y] a] [x b]
          [[x . xs] b] xs
          :pouet)))

    ;; default case
    (eq :pouet
      (with [a [1] b []]
        (conb
          [[x y] a] [x b]
          [[x . xs] b] xs
          :pouet)))

    ;; with binding sym
    (c/let [form '(conb x
                    (pos? (sub a b)) (add x x)
                    (neg? (sub a b)) (mul x -1))
            t1 (c/list 'with '[a 1 b 2] form)
            t2 (c/list 'with '[a 3 b 2] form)]

      (eq [1 2]
        [(c/eval t1)
         (c/eval t2)])))

  (defmacro ?>as
    "like core/some->as but handle composite values
     e.g: [true nil] shorts, as {:a {:b nil} :c 'foo}"
    [s x & xs]
    `(loop [v# ~x
            xs#
            ~($ (vec+ xs)
               (f :rec [x]
                 (c/cond
                   (seq? x) (seq `f [s] x)
                   (coll? x) (seq `f [s] `((df ~($ x rec)) ~s))
                   (sym? x) x
                   :else (seq `f x))))]
       (c/when (?deep v#)
         (if (cons? xs#)
           (~'rec ((car xs#) v#) (cdr xs#))
           v#))))

  (defmacro ?>_ [& xs]
    `(?>as ~'_ ~@xs))

  (asserts
    ;; those 3 forms are semantically equivalent
    (c/=
      ;; [1 2 3] is threaded thru all expressions as x
      (?>as x [1 2 3] (cdr x) (cons? x) (pos? (car x)) (mul x 4))
      ;; shortcut syntax (threaded as _)
      (?>_ [1 2 3] (cdr _) (cons? _) (pos? (car _)) (mul _ 4))
      ;; symbols are interpreted as single arity functions
      (?>_ [1 2 3] cdr cons? (pos? (car _)) (mul _ 4)))

    ;; if an expr is not a seq it is functionized via 'df
    (?>_ [1 2 3]
      {:a (* add _) :b (* sub _) :c (pos? (car _)) :nested {:cons cons?}}
      (dive [:nested :cons] _))
    ;; if the result of an expr (here data function) is not deeply valid it fails
    (nil? (?>_ [1 2 3] {:a (* add _) :b (* sub _) :c (neg? (car _))})))

  (defmacro or
    "like core/or but deep checks"
    [& xs]
    `(c/or ~@($ xs (f (seq `?deep _)))))

  (defmacro or>
    "each member is wrapped with ?>_
     scheme-cond(ish)"
    [& xs]
    `(c/or ~@($ xs (f (seq* `?>_ _)))))

  (asserts
    ;; like
    (or [1 2 nil] {:a 1 :b {:c nil}} #{1 2 3})
    (or>
      [1 (mul -4 _) pos? :aze]
      [-1 (mul -4 _) pos? :baz]))

  )

;; object -----------------------------------------------
(comment

  ;(defrecord Obj [])

  ;(prim+ :obj Obj)

  ;(group+ :coll #{:obj})

  ;(do @types)

  ;(extendg iter [x] :obj (list x))

  ;(iter (Obj.))

  (defn split-key [k]
    (c/and (key? k)
           (c/-> (c/name k)
             (s/split #"\.")
             ($ key))))

  (defn simple-key? [k]
    (c/and (key? k)
           (eq 1 (c/count (split-key k)))))

  (defn dotted-key? [k]
    (c/and (key? k)
           (c/< 1 (c/count (split-key k)))))

  (defn o-args
    [[a & [b & nnxt :as nxt]]]
    (c/cond
      (key? a) (cons [a b] (o-args nnxt))
      (map? a) (o-args (+ (iter a) nxt))
      (cons? a) (o-args (+ a nxt))
      :else '()))

  (defn o
    "create an 'object' :: {key any},
     handles dotted keys and more"
    [& xs]
    (red {}
      (c/fn [a [k v]]
        (c/assoc-in a (split-key k) v))
      (o-args xs)))

  (def fo
    "function object"
    (+ o df))

  (asserts

    (eq {:aze {:iop 1}
         :qsd {:i 2, :o 3}
         :m 1
         :o {:l 3}
         :w {:c 3}}

      (o :aze.iop 1
        :qsd.i 2
        :qsd.o 3
        {:m 1 :o.l 3}
        [:w.c 3]))

    (§ (fo :a.b inc
         :a.c dec
         :a.d [pos? neg?]) 10)))

(defmacro ! [x] `(add 1 ~x))

(add 1 2 (sub 2 3))

(add 2471 2471 1080 1080 1808)

(c/int (add (mul 3 1766)
         (div 1766 4)))
