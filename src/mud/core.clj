(ns mud.core
  (:refer-clojure
    :only [defn defn- defmacro comment])
  (:use mud.boot.core)
  (:require [clojure.core :as c]
            [clojure.template :refer [do-template]]
            [mud.boot.utils :as u]))

;; this idea is to make more generic version of clojure core features
;; it implies generic and extensible core operations
;; more flexible macro system (explicit envirnonment passing, local macros...)
;; more powerful/composable control flow constructs (guards and flow sections)
;; new versions of commonly used macros (let, fn ...)
;; performance still have to be ameliored, but seems reasonable
;; for now the focus is on expressivity, genericity, extensibility and composability
;; and on implementation readibility and conscision (lots of comments and exemples along the code (in 'asserts blocks))

;; the below code relies on several low lowel utils contained in mud.boot package and exposed via mud.boot.core ns

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

  ;; those vars will be overitten
  ;; short symbol are precious
  ;; and math code not so common
  (do-template
    [k v]
    (def k v)
    add c/+
    sub c/-
    div c//
    mul c/*)

  ;; most functions of this namespace takes the subject as first argument
  ;; (what we sometimes call 'this'), given a symbol pointing to one of this functions
  ;; this macro defines another function with the same name with appended _ char (eg. foo becomes foo_)
  ;; that takes a variadic number of args and return a function of the subject
  ;; it is really usefull in combinator based flow expressions
  (defmacro subject-fn [f]
    `(defn ~(u/symcat f '_) [& xs#] #(* ~f % xs#)))

  (defmacro subject-fns [& fs]
    `(do ~@(c/map (c/fn [f] `(subject-fn ~f)) fs)))

  )

;; guards -----------------------------------------------
(do

  ;; guards are like preds but
  ;; returns their first argument on success and nil on failure
  ;; it has more compositional power

  (defn guard [f]
    (c/fn [x & xs]
      (c/when (c/apply f x xs) x)))

  (defmacro defguard
    "guard wrapped defn"
    [name & body]
    `(def ~name (guard (c/fn ~@body))))

  (defn bin-guard
    "wrap a binary function,
     the returned guard can be partially applied to one arg
     that will be used as second argument when finally called
     see section assertions"
    [f]
    (c/let [g (guard f)]
      (c/fn
        ([x] #(g % x))
        ([x y] (g x y)))))

  ;; convert core preds to guards

  ;; unary
  (do-template
    [k v]
    (def k (guard v))
    pos? c/pos?
    neg? c/neg?
    empty? c/empty?)

  ;; binary
  (do-template
    [k v]
    (def k (bin-guard v))
    gt c/>
    lt c/<
    gte c/>=
    lte c/<=
    eq c/=
    neq c/not=)

  ;; define a guard for each type/group
  ;; see mud.boot.types
  (c/doseq [t (all-types)]
    (c/eval `(def ~(u/symcat t '?)
               (guard (isa ~t)))))

  ;; this guy can not be made a guard
  ;; for obvious reasons :)
  (def nil? c/nil?)

  (asserts
    ;; simple arity 1 guard
    (eq 1 (pos? 1))
    ;; binary guard
    (eq 2 ((gt 1) 2))
    ;; nil?
    (eq true (nil? nil))
    ;; type guards
    (line? [1 2])
    (sym? 'aze)
    (nil? (line? {:a 1}))
    )

  )

;; invocation, application, mapping ---------------------
(do

  (c/declare § *)

  ;; fmap like
  ;; apply one or several functions inside a datastructure (not deep)
  ;; for knowing about defred see mud.boot.reduction
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

  (defn flip [f]
    ;; curf is defined in mud.boot.curried
    (curf [a b] (f b a)))

  (defg *'
    "application generic"
    [x xs]
    :fun (c/apply x xs)
    :set (c/apply x xs) #_($ x #(*' % xs))
    :seq (c/apply (c/reduce (flip c/comp) (c/map § x)) xs)
    #{:vec :map} ($i x (c/fn [i v] (*' v ($ xs #(c/get % i)))))
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


  (subject-fns $ $i * § >)

  (asserts

    (c/=
      ;; application
      (* add 1 2 3 [4 5])
      ;; apply is curried on arity 1
      ((* add) 1 2 3 [4 5])
      ;; invocation
      (§ add 1 2 3 4 5)
      ;; curried on arity 1
      ((§ add) 1 2 3 4 5))

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

    ;; data invocation
    (eq [2 1] (§ [inc dec] [1 2]))

    ;; works with several arguments
    (eq [4 -2] (§ [add sub] [1 2] [3 4]))

    ;; $ is using § under the hood
    ($ [[1 2] [3 4]] [inc dec])

    ;; list represent linear function composition
    (eq 3 (§ (c/list inc inc) 1))

    ;; sets do what they do in raw clojure
    (eq 1 (§ #{1 2} 1))
    (nil? (§ #{1 2} 3))

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

  (defn walk [x in out]
    (if (coll? x)
      (out ($ x in))
      (out x)))

  (defn dfwalk
    "depth first walk"
    [x f]
    (walk x #(dfwalk % f) f))

  (defn bfwalk
    "broad first walk"
    [x f]
    (walk (f x) #(bfwalk % f) id))

  (defn walk?
    "x: some data
     ?: if this function succeed, map recur its result
     f: if ? does not succeed f is applied"
    [x ? f]
    (c/if-let [nxt (? x)]
      ($ nxt #(walk? % ? f))
      (f x)))

  (subject-fns walk dfwalk bfwalk walk?)

  (comment

    (def data'
      {:a [1 2 3]
       :b {:c #{0 1 {:d 42}}}})

    ;; walking demos

    (bfwalk data' pr)
    (dfwalk data' pr)
    (walk? data'
      #(c/when (coll? %) (pr "out" %) %)
      #(pr "in" %)))

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

  ;; monoid instances for builtin clojure types

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

  ;; sip function
  ;; semantically equiv to conj
  ;; adds an element to a collection
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
    ;; vec
    (eq [1 2 3] (sip [] 1 2 3))
    ;; set
    (eq #{1 2 3} (sip #{1} 2 3))
    ;; map
    (eq {:a 1 :b 2} (sip {} [:a 1] [:b 2]))
    ;; list
    (eq '(1 2 3) (sip () 1 2 3))
    ;; words
    (eq :azerty (sip :aze \r \t \y))
    (eq 'azerty (sip 'aze \r \t \y))
    (eq "azerty" (sip "aze" \r \t \y))
    ;; function
    (eq 5 ((sip c/+ 1 1 2) 1)))

  (defg iter
    "like clojure.core/seq
     but extensible and more generic"
    [a]
    :nil ()
    #{:sym :key} (iter (c/name a))
    :any (c/or (c/seq a) ()))

  (asserts
    (eq '(1 2 3) (iter [1 2 3]))
    (eq '([:a 1] [:b 2]) (iter {:a 1 :b 2}))
    (eq '(\f \o \o) (iter 'foo))
    (eq '() (iter nil)))

  ;merging sipable things
  ;analoguous to core/merge, core/concat...
  ;first argument's type sets the returns type
  (defred +
    [a b]
    :fun (c/comp b a)
    :any (c/reduce sip a (iter b)))

  (asserts
    ;; colls
    (eq [1 2 3] (+ [] '(1 2 3)))
    (eq '(1 2 3 4) (+ '(1 2) [3 4]))
    (eq {:a 1 :b 2 :c 3} (+ {:a 1} [[:b 2] [:c 3]]))
    ;; words
    (eq 'foobarbaz (+ 'foo :bar "baz"))
    (eq :azertyiopqs (+ :aze "rty" 'iop [\q \s]))
    ;; functions
    (eq 0 ((+ inc dec) 0)))

  (subject-fns iter sip +)

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

  (subject-fns wrap wrap+ wrap* wrap+*)

  ;; holy colls impls
  ;; each collection type is derived into 4 constructor functions, like 'wrap is
  ;; see following assertions for exemples
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
    ;; using identical? is suspicious but seems to work...
    ;; core/= cannot be used here because (= [] ()) is true...
    (c/identical? x (pure x))))

;; basics -----------------------------------------------
(do

  ;; the following section implements basics operations
  ;; in terms our freshly defined generics operators (pure, + ...)


  ;; an helper to wrap clojure.core seq function
  ;; iter first argument and wrap the result
  (c/defmacro iterg [name [a1 :as argv] expr]
    `(defg ~name ~argv
       (c/let [a# ~a1
               ~a1 (iter ~a1)]
         (wrap* a# ~expr))))

  ;; we prefer car and cdr over first and rest/next
  ;; because its shorter and composable
  (defg car [x] (c/first (iter x)))
  (defg last [x] (c/last (iter x)))

  (iterg take [x n] (c/take n x))
  (iterg drop [x n] (c/drop n x))
  (iterg takend [x n] (c/take-last n x))
  (iterg dropend [x n] (c/drop-last n x))
  (iterg butlast [x] (c/butlast x))
  (iterg cdr [x] (c/rest x))
  (iterg rev [x] (c/reverse x))

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

  (subject-fns take drop splat takend dropend)

  )

;; $ variations -----------------------------------------
(do

  (defn red
    "reduce with seed (init) as first argument
     and variadic seq(s) argument (like map does)"
    [x f & xs]
    (if (c/every? cons? xs)
      (* red
        (* f x ($ xs car))
        f ($ xs cdr))
      x))

  ;; $+ is to § what c/mapcat is to c/map
  (defred $+
    [x f]
    (* + (pure x) ($ x f)))

  (defn zip
    "core/map(ish)"
    [f & xs]
    (* c/map f ($ xs vals)))

  (defn zip+
    "core/mapcat(ish)"
    [f & xs]
    (c/if-let [ret (c/seq (* zip f xs))]
      (* + ret) ()))

  (defn scan
    "similar to core/partition"
    [x size step]
    (c/let [[pre post] (splat x size)]
      (if (cons? post)
        (cons pre (scan (drop x step) size step))
        (if (cons? pre)
          (sip (pure x) pre)
          (pure x)))))

  (defn chunk [x size]
    (scan x size size))

  (defn nths [x n]
    ($ (scan x n n) car))

  (def braid
    "interleave"
    (p zip+ (p sip ())))

  (subject-fns red $+ scan chunk nths)

  (asserts
    ;; $+
    ($+ [5 6 7]
      (p c/range 0)
      (c/juxt id (p c/+ 10)))

    ;; zip
    (eq '(3 6 9)
      (zip add [1 2 3] [1 2 3] [1 2 3]))

    ;; zip+
    (eq [1 1 1 2 2 2 3 3 3]
      (zip+ (p sip []) [1 2 3] [1 2 3] [1 2 3]))

    ;scan
    (eq [[1 2] [3 4]]
      (scan [1 2 3 4] 2 2))
    (eq [[1 2] [2 3] [3 4]]
      (scan [1 2 3 4] 2 1))
    (eq '((0 1 2 3) (2 3 4))
      (scan (c/range 5) 4 2))

    ;chunk
    (eq [[1 2] [3]]
      (chunk [1 2 3] 2))
    (eq []
      (chunk [] 2))

    ;; braid
    (eq '(1 4 2 5 3 6)
      (braid [1 2 3] [4 5 6]))
    (eq '(1 4 2 5)
      (braid [1 2 3] [4 5]))))

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

  ;; guard based constructs
  ;; guard composition, filtering, control flow etc...

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
    [x f keep?]
    (red (pure x)
      (c/fn [x v e]
        (if (keep? (§ f v)) (sip x e) x))
      (vals x) x))

  ;; is to $ what core/filter is to core/map
  (defred filt [x f] (filt' x f id))

  ;; is to $ what core/remove is to core/map
  (defred rem [x f] (filt' x f not))

  (asserts
    (eq [1 2 3] (filt [1 2 -1 -2 3] pos?))
    (eq [-1 -2] (rem [1 2 -1 -2 3] pos?)))

  (def ?$i (+ $i ?$))

  (asserts
    (eq [1 2 3] (filt [1 2 -1 -2 3] pos?))
    (eq [-1 -2] (rem [1 2 -1 -2 3] pos?))
    ;; success because no element is equal to its idx
    (?$i [1 2 3] neq)
    ;; fail because 2 element is equal to its idx
    (nil? (?$i [1 1 3] neq)))

  ;; walk ??
  (defn ?deep
    "deeply check x for nils"
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

  (defn ?<
    "thread x thru guards
     until first non nil result"
    [x f & fs]
    (c/or (§ f x)
      (c/when fs (* ?< x fs))))

  (subject-fns ?$ ?$i filt rem ?> ?<)

  (asserts

    ;; success
    (eq 1 (?> 1 num? pos?))
    ;; failure
    (nil? (?> 1 num? neg?))
    ;; shorts after str? (else it would be an error)
    (nil? (?> 1 str? (p + "aze")))
    ;; more exemples
    (eq 3 (?> [1 2 3] (guard (+ c/count c/odd?)) last))
    (nil? (?> [1 2] (guard (+ c/count c/odd?)) last))
    ;; more composed exemple
    ;; use § under the hood,
    ;; applicable data structure can be used
    (eq {:-a 3}
      (?> -1
        num?
        (c/juxt (p add -2) (?>_ (p add 2) pos?))
        car
        neg?
        #(do {:a % :-a (mul % -1)})
        {:-a pos?}))

    ;; build a guard
    ;; that succeed for numbers or strings
    (c/let [f (?<_ num? str?)]
      (eq [1 "a" nil]
        [(f 1) (f "a") (f :a)]))

    ;; basic composition with ?< and ?>_
    (eq 42
      (?< 44
        str?
        (?>_ num? (gt 10) dec dec)))

    )

  (defn ?c>
    "a scheme-cond(ish) function"
    [x & bs]
    (* ?< x ($ bs (* ?>_))))

  (defn ?c
    "a clojure-cond(ish) function"
    [x & cs]
    (* ?c> x (chunk cs 2)))

  (defn ?><
    "at least one guard of each branch have to succeed
    last branch's first success returned"
    [x & bs]
    (* ?> x ($ bs (* ?<_))))

  (subject-fns ?c> ?c ?><)

  (asserts

    (eq 2
      (?c 1
        ;; like clojure cond
        ;; works by couples
        str? :pouet ;; if str? succeed :pouet is called
        pos? inc
        neg? dec))

    (eq 10
      (?c 10
        num? (lt 3) ;; if the second pred fail, we go to next couple
        num? (gt 7) ;; this line succeed
        ))

    ;; (non function values act as constant functions)
    (eq :pouet
      (?c "a"
        str? :pouet
        pos? inc
        neg? dec))

    ;; same with ?c_
    (eq -2
      ((?c_
         str? :pouet
         pos? inc
         neg? dec)
        -1))

    (eq -8
      (?c> -2
        ;; like scheme cond
        ;; several vecs of guards
        [str? :pouet]
        [pos? inc inc inc]
        [neg? dec dec (p mul 2)]))

    (?c> 1
      ;; here too, if the line does not succeed entirely,
      ;; skip to the next line
      [pos? dec pos? :gt1]
      [pos? :1])

    (eq 5
      ((?c>_
         [str? :pouet]
         [pos? inc inc inc]
         [neg? dec dec (p mul 2)])
        2))
    ))

;; composite literals -----------------------------------
(do

  ;; the idea is to handle composite literals
  ;; this is done with . and .. composition operators
  ;; lists: (similar for vectors)
  ;; (a b . c) <=> (concat (a b) c)
  ;; (a . b c) <=> (concat (a) b (c))
  ;; (a .. b c d) <=> (a . b . c . d) <=> (concat (a) b c d)
  ;; maps
  ;; {:a 1 . b} <=> (merge {:a 1} b)
  ;; {:a .. (b c)} <=> (merge {:a 1} b c)

  ;; those functions and guards are not intented to be used directly
  ;; but are nescessary later (bindings, macro expansion)


  ;; helper macro
  (defmacro gond [x & xs]
    `(c/condp #(%1 %2) ~x ~@xs))

  (defguard dotted?
    [x]
    (gond x
      map? (c/contains? x '.)
      line? (u/indexof x '.)
      nil))

  ;; x contains some composition operators
  ;; (. and/or ..)
  (defguard composed?
    [x]
    (gond x
      map? (c/or (c/contains? x '.) (c/contains? x '..))
      line? (c/or (u/indexof x '.) (u/indexof x '..))
      nil))

  (def not-composed?
    (guard (c/complement composed?)))

  ;; x has only one dot
  ;; (useful in bind)
  (defguard single-dotted?
    [x]
    (c/and (dotted? x)
           (gond x
             map? (not (c/contains? x '..))
             line? (c/and (not (u/indexof x '..))
                          (eq 1 (c/count (filt x (eq '.))))))))

  (defn seq-parts
    "decompose a composed list litteral
     into a seq of lists"
    [s]
    (c/loop [[fs ss & rs] s ret []]
      (gond fs
        not ret
        (eq '.) (recur rs (sip ret ss))
        (eq '..) (* sip ret ss rs)
        (recur (cons ss (c/or rs ()))
          (if (vec? (last ret))
            (sip (butlast ret) (sip (last ret) fs))
            (sip ret [fs]))))))

  (asserts
    (?> '(a . b) composed? dotted? single-dotted?)
    (nil? (single-dotted? '(a . b . c)))
    (?> '{:a 1 . b} composed? dotted? single-dotted?)
    (?> '(a . b .. c) composed? dotted?)
    (?> '{:a 1 . b .. [c d]} composed? dotted?)
    (nil? (single-dotted? '(a . b .. c)))
    (eq '[[a b] c [d e]] (seq-parts '(a b . c d e))))

  )

;; dive -------------------------------------------------
(do

  ;; dive is similar to core/get with args flipped
  ;; but is extensible and generic
  ;; it will be further extensible after defining macro system
  (defg dive

    [x y]

    :vec
    (c/reduce (flip dive) y x)

    #{:key :sym}
    (c/when (c/or (map? y) (c/record? y) (set? y))
      (c/get y x))

    :num
    (u/gat y x)

    :set
    (dive (+ {} ($ (iter x) #(vec % %))) y)

    :map
    ($ x #(dive % y))

    :fun (§ x y)

    :nil y
    )

  ;; flipped version of dive
  ;; analogous to core/get
  ;; the issue is that dive macro expansion will not occur...
  ;; at
  #_(defn at
      ([x y] (dive y x))
      ([x y & ys] (dive (vec* y ys) x)))

  #_(subject-fn at)

  (asserts

    ;; dive exemples

    (eq 1
      (dive :a
        {:a 1 :b 2 :c 3}))

    (eq 1
      (dive [:b :c]
        {:a 1 :b {:c 1} :c 3}))

    ;; functions are addresses to
    (dive c/inc 0)

    ;; sets can be used to compose a selection
    (eq {:a 1 :b 2}
      (dive #{:a :b}
        {:a 1 :b 2 :c 3}))

    (eq {[:b :c] 1, :a 1}
      (dive #{:a [:b :c]}
        {:a 1 :b {:c 1} :c 3}))

    ;; maps can serve as addresses to
    (eq {:a+ 2, :bc2 'pouet}
      (dive {:a+ [:a c/inc] :bc2 [:b :c 2]}
        {:a 1 :b {:c [1 2 'pouet]} :c 3}))

    (eq 0
      (dive [:b :c c/dec]
        {:a 1 :b {:c 1} :c 3}))

    ;; composed
    (eq {[0 0 2] 3, [0 0 0] 1}
      (dive #{[0 0 0] [0 0 2]}
        [[[1 2 3 4]]]))

    ;; at
    #_(eq {:b 1} ((at_ :a) {:a {:b 1}}))
    #_(eq 1 ((at_ :a :b) {:a {:b 1}}))
    #_(eq (dive c/inc 0) (at 0 c/inc))

    ))

;; tack -------------------------------------------------
(do

  ;; not intended to be used directly
  ;; prefer using put and upd
  ;; semantically similar to assoc with different arg order
  ;; like in dive the first argument is the address (and is used to dispatch)
  (defg tack

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
      (c/assoc (c/or x {}) k v)))

  (defn put
    "like assoc but more generic and extensible"
    [x & xs]
    (red x
      (c/fn [x [k v]] (tack k x v))
      (chunk xs 2)))

  (defn upd
    "like update but more generic and extensible"
    [x & xs]
    (red x
      (c/fn [x [k f]]
        (tack k x (f (dive k x))))
      (chunk xs 2)))

  (subject-fns put upd)

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

;; bind -------------------------------------------------
(do

  (c/declare bind bindings)

  ;; bind special operations
  ;; if bind's first arg is a list with head being an operator symbol
  ;; the corresponding implementation is used to expand in place the first arg
  ;; intended to be extended with user operations
  (def bind-ops

    (c/atom
      {
       ;; & is used to apply several pattern to the same thing
       ;; resulting bindings are concatenated in order
       '&
       (c/fn [& xs]
         (c/fn [y]
           (c/let [ysym (c/gensym)]
             (* + [ysym y]
               ($ xs (c/fn [p] (bind p ysym)))))))

       ;; ks is semantically equivalent to {:keys [...]}
       ;; (ks a b) <=> {:keys [a b]}
       'ks
       (c/fn [& xs]
         (c/fn [y]
           (bind (c/zipmap ($ xs key) xs) y)))

       ;; given a predicate or a guard
       ;; bind the result of applying it to y
       ;; the result is bound to the optionally given pattern
       ;; or a fresh symbol (subject to shortcircuiting if nil)
       '!
       (c/fn [f & [p]]
         (c/fn [y]
           (bind (c/or p (c/gensym)) `(~f ~y))))

       ;; handle dotted map litteral
       'dotmap
       (c/fn [x]
         (c/fn [y]
           (c/let [rs (c/get x '.)
                   m (c/dissoc x '.)
                   ks (c/keys m)
                   [checkmap msym] (u/gensyms)]
             (+
               [checkmap `(map? ~y)
                msym y]
               (bind m msym)
               (bind rs `(c/dissoc ~msym ~@ks))))))

       ;; handle dotted vec litteral
       'dotvec
       (c/fn [x]
         (c/fn [y]
           (c/let [doti (u/indexof x '.)
                   cars (take x doti)
                   [eli queue] (uncs (drop x (inc doti)))
                   qcnt (c/count queue)
                   [checkline cdr'] (u/gensyms)]
             (+
               [checkline `(line? ~y)
                cdr' `(drop ~y ~doti)]
               (bind cars `(take ~y ~doti))
               (if (c/zero? qcnt)
                 (bind eli cdr')
                 (+
                   (bind eli `(dropend ~cdr' ~qcnt))
                   (bind queue `(takend ~cdr' ~qcnt))))))))}))

  (defg bind
    "like clojure.core/destructure
     return a vector of bindings (like used by let)
     extensible via the bind-ops atom
     handle 'single-dotted? literal see 'composite litteral' section"
    [x y]
    :map
    (gond x

      single-dotted?
      (bind (seq 'dotmap x) y)

      not-composed?
      (c/let [[checkmap ysym] (u/gensyms)]
        (+
          [ysym y
           checkmap `(map? ~y)]
          ($+ (iter x)
            (c/fn [[k v]]
              (bind v `(dive ~k ~ysym)))))))

    :vec
    (gond x

      single-dotted?
      (bind (seq 'dotvec x) y)

      not-composed?
      (c/let [[ysym checkline checkcount] (u/gensyms)]
        (+
          [ysym y
           checkline `(line? ~y)
           checkcount `(eq ~(c/count x) (c/count ~ysym))]
          (zip+
            (c/fn [v i] (bind v `(dive ~i ~ysym)))
            x (c/range)))))

    :seq
    (c/if-let [f (c/get @bind-ops (car x))]
      (bind (* f (cdr x)) y)
      (bind (c/eval x) y)) ;;very suspicious...

    :set
    (bind (seq* '& x) y)

    :sym [x y]

    :fun (x y))

  (defn bindings
    "bind several couples and cat the results"
    [xs]
    ($+ (chunk xs 2) (p * bind)))

  (defn ubindings
    "like bindings but add unification constraints
     same syms have to be bound to equals values"
    [bs]
    (c/loop [ret [] seen #{}
             [a b & nxt] (bindings bs)]
      (if a
        (if (seen a)
          (recur (sip ret (c/gensym) `(eq ~a ~b)) seen nxt)
          (recur (sip ret a b) (sip seen a) nxt))
        ret)))

  (do
    (bind '(& a b) 'x)
    (bind '(ks a b) 'x)
    (bind '(c/fn [_] []) 'x)
    (bind '#{a b} 'x)
    (bind '[a b . c d] [1 2 3 4 5])
    (bind '[a b] nil)
    (bind '{:a a :b b . x} {:a 1 :b 2 :c 3})
    (c/eval `(c/let ~(bind '{:a a :b b . x} {:a 1 :b 2 :c 3}) ~'[a b x]))
    (c/eval `(c/let ~(bind '[a b . c d] [1 2 3 4 5]) ~'[a b c d]))
    (bind '(! pos? a) 'x)
    ))

;; macro expansion --------------------------------------
(do

  ;; m-obj :: {:parse fn :expand fn ...}
  ;; the idea here is to implement a macro system with explicit environment
  ;; influenced by scheme fexprs, scheme expansion passing style, and metaclj

  ;; this atom will handle global m-objs
  ;; {sym m-obj}
  (def m-env (c/atom {}))

  (defn m-apply
    "take an m-obj and some args
     and do the expansion"
    [{:as o :keys [expand parse]} args]
    (expand o (parse o args)))

  (defn expand
    "meta-expand an expression  x
     given an environment e
     (handle composite literals expansion)"
    ([e] (p expand e))

    ([e x]
      #_(pr "expand" e x)
     (gond x

       u/quote? x

       composed?
       (gond x
         vec? (c/let [ps (seq-parts x)] `(+ ~@($ ps (expand e))))
         seq? (c/let [ps (seq-parts x)] `(u/call* (+ ~@($ ps (expand e)))))
         map? `(c/merge ~(expand e (c/dissoc x '. '..)) ~(c/get x '.) ~@(c/get x '..)))

       sym?
       (:value (c/get e x) x)

       seq?
       (c/if-let [m (c/get e (car x))]
         (m-apply (sip m [:env e]) (cdr x))
         ($ x (expand e)))

       coll? ($ x (expand e))

       x)))

  (defmacro e!
    "dev util" [x]
    `(expand @m-env ~x))

  (def m-id
    "the identity m-obj"
    {:parse (c/fn [_ x] x)
     :expand (c/fn [_ x] x)})

  (defn m-mk
    "build an m-obj
     from either a function (used as :expand impl)
     or a potentially partial or extended one"
    [x]
    (gond x
      map? (+ m-id x)
      fun? (m-mk {:expand x})))

  (defmacro m-def
    "like defmacro, but use expand"
    [name spec]
    `(do
       (defmacro ~name [& xs#] (expand @m-env (cons '~name xs#)))
       (c/swap! m-env c/assoc '~name (m-mk ~spec))))

  (defmacro m-ext
    "declare a meta expansion, but not a macro
     usable nested in top forms, (defined using m-def)"
    [name spec]
    `(c/swap! m-env c/assoc '~name (m-mk ~spec))))

;; lambda -----------------------------------------------
(do

  ;; define lambda-like m-objs (using 'bind)

  (defn- parse-f
    [_ [x & xs :as form]]
    (c/let [[name [pat & body]]
            (if (key? x)
              [(sym x) xs]
              [(c/gensym) (cons x xs)])]
      {:name name
       :pat pat
       :body body
       :form form}))

  (defn- expand-f
    [o {:keys [pat name body]}]
    (c/let [s (c/gensym)
            bs (bind pat s)
            ;; taking all boud syms
            shadows (nths bs 2)
            ;; and removing them from the environment
            ;; for the rest of expansion
            e (* c/dissoc (:env o) shadows)]
      `(c/fn ~name ~(if (:unary o) [s] ['& s])
         (c/let ~bs ~@(expand e body)))))

  (comment
    (m-def f
      {:parse parse-f
       :expand
       (c/fn [o {:keys [pat name body]}]
         (c/let [s (c/gensym)
                 bs (bind pat s)
                 shadows (nths bs 2)
                 e (* c/dissoc (:env o) shadows)]
           `(c/fn ~name [& ~s]
              (c/let ~bs ~@(expand e body)))))}))

  ;; variadic function
  (m-def f
    {:parse parse-f
     :expand expand-f})

  ;; unary function
  (m-def f1
    {:parse parse-f
     :expand expand-f
     :unary true})

  (do
    ;; simple function
    ;; optional name is given as a keyword
    ((f :foo [a]
       (if (pos? a)
         ;; recursion
         (foo (mul -1 a))
         ;; return
         a))
      10) ;;=> -10

    ;; can bind arguments to a single symbol (like in scheme)
    ;; composite literal can be used in the body of the function
    ;; (sub . xs) <=> (apply sub xs)
    ((f xs (sub . xs)) 1 2 3)

    ;; dot syntax let you do that
    ((f xs (sub . xs 4)) 1 2 3) ;;=> -8

    ;; or that
    ((f xs (sub .. xs xs)) 1 2 3) ;;=> -10

    ;; f is more permissive than regular clojure fn
    ;; the following does not throw
    ;; the behavior is more like let destructuration
    ((f [a b] a) 1)

    ;; the usual clojure's & is replaced by scheme's .
    ((f [x . xs] [x xs]) 1 2 3)
    ;; => [1 [2 3]]

    ;; map bindings
    ((f [{:a a :b [b1 . bs]}]
       [a b1 bs])
      {:a 1 :b [2 3 4]})
    ;;=> [1 2 [3 4]]

    ;; rest pattern for maps
    ((f [{:a a . rs}] [a rs])
      {:a 1 :b 2 :c 3})
    ;;=> [1 {:b 2, :c 3}]

    ;; keys (using 'ks binding op)
    ((f [(ks a b)] [a b])
      {:a 1 :b 2})
    ;=> [1 2]

    ;; '& binding op
    ;; bind several patterns to the same thing
    ;; the following is similar to {:as m :keys [a b]}
    ((f [(& m (ks a b))] [m a b]) {:a 1 :b 2})
    ;;=> [{:a 1, :b 2} 1 2]

    ;; f is properly shadowed
    ((f [f a b . c]
       (f a b . c))
      sub 1 2 3 4 5)

    ;; unary version
    ((f1 x (add x x)) 1) ;; 2
    ((f1 {:a a} a) {:a 1}) ;; 1
    ((f1 (ks a b) (add a b)) {:a 1 :b 2}) ;; 3

    )

  ;; like defn using f
  (m-def deff
    {:parse
     (f [_ [ff . rs]]
       (parse-f {} (cons (key ff) rs)))
     :expand
     (f [(ks env)
         (ks name form)]
       `(def ~name
          ~(expand env (seq 'f . form))))})

  (do
    (deff sub' [a b] (sub a b))
    (sub' 1 2))

  ;; maybe function
  ;; try to bind with given arguments,
  ;; execute body in case of success
  ;; or returns nil
  ;; useful for nil based control flow

  (deff ?let-form
    ;; emit a form semantically equivalent to let
    ;; except that it shorts on nil bindings
    ;; (exception for bound symbols starting with _)
    ;; this is crude... have to find a better way
    [[b1 b2 . bs] e]
    `((c/fn [~b1]
        (c/when ~(c/or (eq \_ (car b1)) b1)
          ~(if (cons? bs) (?let-form bs e) e)))
       ~b2))

  (deff expand-?f
    [(ks env unary) (ks pat name body)]
    (c/let [s (c/gensym)
            bs (bind pat s)
            shadows (nths bs 2)
            e (c/dissoc env . shadows)]
      `(c/fn ~name ~(if unary [s] ['& s])
         ~(?let-form bs `(do ~@(expand e body))))))

  (m-def ?f
    {:parse parse-f
     :expand expand-?f})

  (m-def ?f1
    {:parse parse-f
     :expand expand-?f
     :unary true})

  (do

    ((?f :foo [a b] a) 1) ;;=> nil
    ((?f :foo [a b] a) 1 2) ;;=> 1
    ((?f :foo [a b] a) 1 2 3) ;; nil

    ;; every bound symbol has to be bound for the pattern to succeed
    ((?f [{:a a :b b}] [a b]) {:a 1 :b 2}) ;=> [1 2]
    ((?f [{:a a :b b}] [a b]) {:a 1}) ;; nil
    ((?f [{:a a :b _b}] [a _b]) {:a 1}) ;; _ prepended symbols can fail ;; => [1 nil]

    ;; check op (!)
    ((?f [(! pos?) b] b) 1 2) ;;=> 2
    ((?f [(! pos?) b] b) -1 2) ;;=> nil
    ((?f [(! pos? a) b] [a b]) 1 2) ;; the result of the guard can be bounded
    ((?f [(! vec? [x . xs])] [x xs]) [1 2 3]) ;; to any pattern
    ((?f (! vec? [x . xs]) [x xs]) 1 2 3) ;; does not make much sense
    ;; it should make sense if function can specify how it takes args (kw functions, custom agregation (like km))
    ;; but maybe some variants (like f1 is) could be sufficient, fk for instance (with km like semantics see fun.km )

    ;; unary version
    ((?f1 (ks a b) [a b]) {:a 1 :b 2}) ;;=> [1 2]
    ((?f1 (ks a b) [a b]) {:a 1 :c 2}) ;;=> nil
    ))

;; let --------------------------------------------------
(do



  (deff parse-let
    [_ (& form [x . xs])]
    (c/let [[name [x & xs]]
            (if (key? x) [(sym x) xs] [nil (cons x xs)])
            [bs [bod1 & bods]]
            (gond x
              map? [(vec* (iter x)) xs]
              vec? [(chunk x 2) xs])
            expr
            (if bods `(do ~bod1 ~@bods) bod1)]

      {:name (c/or name (c/gensym))
       :name? name
       :bs bs
       :expr expr
       :parrallel? (map? x)
       :form form}))

  ;; the usual let form
  ;; use bind under the hood
  (m-def let

    {:parse parse-let

     :expand
     (f :rec
       [(& o (ks env))
        (& parsed
          (ks bs expr name name? parrallel?))]

       (if (c/or name? parrallel?)

         ;; named let (loop)
         (expand env
           (seq
             (seq 'f (key name) ($ bs car) expr)
             . ($ bs cadr)))

         ;; redo
         (c/let [[[b1 e1] & bs] bs]
           (if b1
             (expand env
               (seq
                 (seq 'f [b1]
                   (rec o (sip parsed [:bs bs])))
                 e1))
             expr))
         ))})

  (do
    (let [] 1)
    ;; let
    (let [a 1 b 2] (add a b))
    (let [a 1 b a] (add a b))
    (let [[a . b] (c/range 10)] [a b])

    ;; is with 'f and '?f
    ;; the body can contains composite literals
    (let [a 1 b 2 c [1 2 3] d [9 . c]]
      (add a b . c . d))

    ;; named let (similar to clojure's loop)
    (let :rec [a 1 b 10]
      (c/when-not (c/zero? b) (pr b) (rec a (dec b))))

    ;; if there no dependencies between couples
    ;; parrallel form can be used (faster)
    (let {a 1 b 2} (add a b))
    )

  ;; a let that shorts on nil bindings
  ;; use bind under the hood
  ;; (except if bound symbol starts with _)
  (m-def ?let

    {:parse parse-let

     :expand
     (f :rec
       [(& o (ks env))
        (& parsed (ks expr)
          {:bs [[b1 e1] . bs]})]

       (expand env
         (seq
           (seq '?f [b1]
             (if (cons? bs)
               (rec o (sip parsed [:bs bs]))
               expr))
           e1)))})

  (do

    ;; is to let what ?f is to f
    (?let [a 1 b 2] (add a b))

    ;; if binding fail the expression is not computed
    ;; else it would be an error here
    (?let [[a b] nil] (add a b)) ;;=> nil
    (?let [a 1 b nil] (add a b)) ;;=> nil

    ;; take care of composite literals to
    (?let [[a b . c] (c/range 10)] (add a b . c)))

  (deff parse-binding-cases
    [_ (& form [ff . rf])]
    (let [[name cases]
          (if (key? ff) [(sym ff) rf] [(c/gensym) form])
          [cases default]
          (if (c/odd? (c/count cases)) (runcs cases) [cases nil])]
      {:name name
       :cases (chunk cases 2)
       :default default}))

  ;; like cond-let using ?let
  ;; (clet b1 e1 b2 e2 default)
  ;; <=>
  ;; (or (?let b1 e1) (?let b2 e2) default)
  (m-def clet

    {:parse parse-binding-cases

     :expand
     (f :rec
       [(& o (ks env)) ;; same as {:as o :keys [env]}
        (& parsed (ks cases default))]
       (expand env
         (if (cons? cases)
           (seq `c/or (seq '?let . (car cases))
             (rec o (upd parsed :cases cdr)))
           default)))})

  (asserts

    ;; clet exmeples
    (let [x 1]

      (clet
        ;; this fails
        [[a b] x] a
        ;; this succeed so b is returned
        [b 1] b
        ;; the default case
        :pouet))

    (let [x 1]

      (clet
        ;; the binding forms can contains several couples
        ;; this succeed for positive x
        [a x
         (! pos?) a] a
        ;; the default case
        [:neg x])))

  (m-def case

    {:parse
     (f [_ (& form [ff . rf])]
       (let [[name [init . rf]]
             (if (key? ff) [(sym ff) rf] [(c/gensym) form])]
         (+
           (parse-binding-cases {} rf)
           {:name name
            :init init})))

     :expand
     (f [(ks env) (ks name cases default init)]
       (let [s (c/gensym)
             cases ($+ cases (f [[pat e]] [[pat s] e]))]
         (expand env
           (seq (seq 'f (key name) [s]
                  (seq 'clet . cases default))
             init))))})

  (asserts

    ;; binding case
    (eq :b
      (case 1
        [a b] :a
        x :b))

    (eq [:a 1 2]
      (case [1 2]
        [a b] [:a a b]
        x :b))

    ;; can be named and do recursion :)
    (case :rec
      (c/range 10)
      [a] a
      [a . as] (rec as)))

  ;; case function
  (m-def cf

    {:parse
     (f [_ (& form [rf . ff])]
       (if (key? rf)
         {:name rf :tail ff}
         {:name (key (c/gensym)) :tail form}))

     :expand
     (f [(ks env) (ks name tail)]
       (expand env
         (let [s (c/gensym)]
           (seq 'f name s (seq 'case s . tail)))))})

  (do
    (def cf1 (cf
               [a] :a
               [a b] :b
               :c))
    (asserts
      (eq :a (cf1 1))
      (eq :b (cf1 1 2))
      (eq :c (cf1 1 2 3))))

  ;; meta let
  ;; let you bind locals macros
  (m-def m-let
    (f [(ks env) [xs expr]]
      (let [env'
            (if (sym? (car xs))
              ;; couple bindings (let like)
              (+ env ($ (chunk xs 2) (f [[k i]] [k (m-mk (c/eval i))])))
              ;; letfn like bindings
              (+ env ($ xs (f [[name . form]] [name (m-mk (c/eval (seq 'f . form)))]))))]
        (expand env' expr))))

  (asserts
    ;; dummy exemple
    ;; defined a lexically scoped macro called fi (similar to core/if-not)
    ;; note that for brievety, we do not expand arguments (what we should do in most cases)

    ;; binding an expand function to a symbol
    (eq 2
      (m-let [fi (f [e [p f t]] (seq 'if p t f))]
        (fi true (pr 1) 2)))

    ;; shortcut syntax (similar to core/letfn)
    ;; the given function is used as :expand impl
    (eq 2
      (m-let [(fi [e [p f t]] (seq 'if p t f))]
        (fi true (pr 1) 2)))

    ;; binding a meta spec to a symbol
    (eq 2
      (m-let [fi {:parse (f [_ [p f t]] {:pred p :then t :else f})
                  :expand (f [e (ks pred else then)]
                            ;; expansion is not needed in this exemple,
                            ;; but if it was, we would do like this
                            ($ (seq 'if pred then else) (expand e)))}]
        (fi true (pr 1) 2)))

    )

  )

;; meta utils -------------------------------------------
(do
  ;; bind uniq symbols to given symbols
  ;; and
  (m-def let-syms
    (f [(ks env) [xs expr]]
      (seq 'let [xs (seq 'u/gensyms (c/count xs))] expr)))

  (asserts
    (let-syms [a b c] [a b c]))

  ;; like do using 'expand
  (m-def progn
    (f [(ks env) xs]
      (seq 'do . ($ xs (expand env)))))

  (asserts
    ;; return last expression value
    (eq 3 (progn 1 2 3))
    ;; f form is expanded
    (eq :ok (progn 1 2 ((f [x . xs] :ok) 1 2))))
  )

;; exp --------------------------------------------------
(do

  ;; with this macro system, it becomes possible to add compile time preprocessing to existing functions
  ;; let's try on 'dive
  ;; the first argument will be treated differently, as bind does

  ;; xp 1

  (comment

    ;; this atom hold dive meta expansions
    ;; if dive first args is a seq
    ;; the car of this seq is used as meta operator
    ;; e.g (dive (op . args) x)
    ;; the operation impl has to return a valid dive arg1
    (def dive-ops
      (c/atom
        {'ks
         (f xs
           `(df ~(+ {} ($ xs key (f [k] [k `(p dive ~k)])))))}))

    ;; adding an expansion to a function
    (c/swap! m-env put
      'dive
      (m-mk
        (f [(ks env) [a b]]
          (clet [i (dive (car a) @dive-ops)]
            (seq 'dive (i . (expand env (cdr a))) (expand env b))
            (seq 'dive (expand env a) (expand env b))))))


    (let [a {:a 1 :b 1 :c 2}] (dive (ks a b) a))
    (let [a {:a 1 :b 1 :c 2}] (dive (f [_] :fux) a)))

  ;; but dive-ops could be embedded in m-env under 'dive key
  ;; this mecanics could be abstracted, let's try

  ;; xp 2

  ;; wrap a function given its name and op-map :: {sym fun}
  (deff opify!
    [name ops]
    (c/swap! m-env put
      name
      (m-mk
        {:ops ops
         :expand
         (f [(ks env ops) (& xs [fx . rx])]
           (clet [(! seq? [op . args]) fx
                  i (dive op ops)]
             (seq 'dive (i . (expand env args)) . (expand env rx))
             (seq 'dive . (expand env xs))))})))

  ;; same as in exemple 1 with opify! (ops are stored in m-env)
  (opify! 'dive
    {'ks
     (f xs
       `(df ~(+ {} ($ xs key (f [k] [k `(p dive ~k)])))))})

  (let [a {:a 1 :b 1 :c 2}]
    (dive (ks a b) a))

  ;; 'at is implemented in terms of 'dive, but this wrapping does not profit from dive compile time extensions
  ;; is there a way to link those kind of dual functions?
  ;; depending on the usage (literally applied or passed by value)
  ;; in the case of literal application, compile time stuff should be propagated

  ;; doing it manually first
  (do

    ;; xp 1 (hard way)

    (comment
      ;; function
      (deff at [x y] (dive y x))

      (m-ext at
        (f [(ks env) [x y]]
          (expand env (seq 'dive y x))))

      (at {:a 1} :a)
      ;; using dive op
      ;; this let wrapping is nescessary to at being expanded
      (let [_ nil] (at {:a 1 :b 2} (ks a b)))

      ;; at is subject to subject abstraction (at_)
      ;; we have to define another macro for this...

      (subject-fn at)

      (m-ext at_
        (f [(ks env) [y . ys]]
          (if (cons? ys)
            (seq 'at_ (expand env (vec y . ys)))
            (expand env (seq 'f ['x] (seq 'dive y 'x))))))

      (let [_ nil]
        ((at_ (ks a b) :b) {:a 1 :b 2 :c 3}))

      ;; but... it is a lot to write for such simple thing
      )

    ;; xp 2 (introduce m-sub)

    (m-def m-sub
      (f [_ [name pat expr]]
        (seq 'm-ext name
          (seq 'f ['(ks env) pat]
            (seq 'expand 'env expr)))))

    ;; define a meta substitution
    (m-sub at [a b]
      (seq 'dive b a))

    (progn (at {:a 1 :b 2 :c 3} (ks a b)))
    ;; sub
    ;; => (dive (ks a b) {:a 1 :b 2 :c 3})
    ;; expand and compute
    ;; => {:a 1, :b 2}

    (m-sub at_ [x]
      (let-syms [s]
        (seq 'f [s] (seq 'dive x s))))

    (progn ((at_ (ks a b)) {:a 1 :b 2 :c 3}))

    ;; for 'at being able to be used as first class function
    ;; we are using the :value field
    (c/swap! m-env put ['at :value] `(flip dive))

    (progn
      ;; at is in non verb position
      ;; it is substituted by `(flip dive)
      (eq [1 2]
        ($ [:a :b] (p at {:a 1 :b 2}))))



    )

  ;; thinking about ns qualification
  ;; does i want to stick with clojure ns system?

  ;; one :value field could be attached to m-objs
  ;; for symbol in non-verb position


  )




























