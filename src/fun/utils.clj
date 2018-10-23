(ns fun.utils
  (:refer-clojure :exclude [time])
  (:require [clojure.pprint :as p]
            [clojure.string :as s]))

;; common combinators ----------

(defn a1 [a & _] a)
(defn a2 [_ a & _] a)
(def always (constantly true))
(def never (constantly false))
(defn flip [f] (fn [a b] (f b a)))

;; debug and test ---------------

(defn pp [& xs]
  (mapv p/pprint xs)
  (println "\n" (apply str (repeat 100 "-"))))

(defn throw* [& xs]
  (throw (Exception. (apply str xs))))

(defn check-fn
  ([f m]
   (doseq [[in out] m]
     (let [error-str
           (str "\nfunction: " f
                "\ninput: " in
                "\nexpected output:" out)

           unexpected-output (atom nil)
           unexpected-error (atom nil)]

       (try
         (let [out' (apply f in)]
           (when-not (= out out')
             (reset! unexpected-output out')))

         (catch Exception e
           (when-not (= out :error)
             (reset! unexpected-error e))))

       (cond
         @unexpected-error
         (throw*
           "\ncheck-fn unexpected error:"
           error-str
           "\nerror: " @unexpected-error)
         @unexpected-output
         (throw*
           "\ntest failed: "
           error-str
           "\nactual output: " @unexpected-output)))))
  ([f x & xs]
   (check-fn f (apply hash-map x xs))))

(defmacro asserts [& xs]
  `(do ~@(map (fn [e] `(assert ~e)) xs)))

(defmacro time
  "Evaluates expr and prints the time it took.  Returns the value of
 expr."
  {:added "1.0"}
  [n expr]
  `(let [start# (. System (nanoTime))
         ret# (~'dotimes [_# ~n] ~expr)]
     (double (- (. System (nanoTime)) start#))))

;; macros ------------------------

(defmacro applicable-record
  "define a record given an apply impl
   derive all arities of invoke based on it
   TODO: let user pass other protocols impls"
  [name & body]
  `(defrecord ~name []

     clojure.lang.IFn
     (~'applyTo ~@body)

     ~@(let [asyms (map #(symbol (str "a" %))
                        (range))]
         (map
           (fn [n]
             (let [args (vec (take n asyms))]
               `(~'invoke ~(into ['f] args)
                  (apply ~'f ~args))))
           (range 0 21)))))

(defmacro locals []
  (let [locals (keys &env)]
    `(zipmap '~locals ~(vec locals))))

(defmacro local-bindings []
  `(vec (mapcat identity (fun.utils/locals))))

(defmacro extract [e]
  `(list 'let (local-bindings) '~e))

(comment

  (let [a 1]
    (let [b 2]
      (assert (= (locals) '{a 1 b 2}))))

  (let [a 1]
    (let [b 2]
      (assert (= (local-bindings) '[a 1 b 2]))))

  (let [a 1] (extract (+ b a)))

  (macroexpand
    '(defmacro m [a b c] d e f))

  (if true 1 (scream!)))

(defmacro scream! [& xs]
  `(throw* *ns* ":" ~(:line (meta &form))
           "\n"
           ~(zipmap (map #(list 'quote %) xs)
                    xs)))

;; keywords and path ------------

(defn simple-kw? [k]
  (and (keyword? k)
       (-> k
           name
           (clojure.string/split #"\.")
           count
           (= 1))))

(defn kw->path [kw]
  (-> kw
      name
      (s/split #"\.")
      (->> (mapv keyword))))

(defn kw-uncons [kw]

  (let [[f & r]
        (s/split (name kw) #"\.")]

    [(keyword f)
     (keyword (s/join "." r))]))

(kw-uncons :a.b.c)

(defn kw-cat [& kws]
  (->> kws
       (mapcat kw->path)
       (map name)
       (interpose ".")
       (apply str)
       keyword))

;; vectors -----------------------

(defn catv [& xs]
  (vec (apply concat xs)))

(defn ensure-vec [x]
  (cond
    (nil? x) []
    (sequential? x) (vec x)
    :else [x]))

;; sets --------------------------

(defn subset? [a b]
  (clojure.set/subset? (set a) (set b)))

;; numbers -----------------------

(defn round [n]
  (Math/round (float n)))

;; hashmaps ---------------------

(def hm hash-map)

(def hm*
  (partial apply hash-map))

(defn raw-map? [x]
  (and (not (record? x))
       (map? x)))

(defn map-vals [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn map-hm [f m]
  (into {} (map (fn [[k v]] (f k v)) m)))

(defn vec->map [x]
  (hm* (interleave (range) x)))

(defn map->vec [m]
  (assert (every? (comp number? key) m)
          "not castable to vector")
  (mapv val (sort-by key m)))

(defn deep-merge
  "Like merge, but merges maps recursively."
  [& maps]
  (cond
    (every? map? maps)
    (apply merge-with deep-merge maps)

    (every? vector? maps)
    (map->vec
      (apply deep-merge
             (map vec->map maps)))

    :else
    (last maps)))

(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level."
  [f & maps]
  (apply
    (fn m [& maps]
      (if (every? map? maps)
        (apply merge-with m maps)
        (apply f maps)))
    maps))

(defn require-keys
  ([ks]
   (fn [m]
     (require-keys m ks)))
  ([m ks]
   (let [m' (select-keys m ks)]
     (when (= (count ks) (count m'))
       m'))))

(defn ensure-keys [m ks]
  (and (require-keys m ks) m))

(comment
  (deep-merge
    {:a {:b [1 2] :d 1}}
    {:a {:c 1 :b [3]} :m 23}))

;; seqs -----------------------

(defn member
  [a xs]
  (cond
    (= a (first xs)) xs
    (next xs) (member a (next xs))))

(defn memf
  [f xs]
  (cond
    (f (first xs)) xs
    (next xs) (memf f (next xs))))



(comment
  (member 1 [6 7 8 1 2 3])
  (member 1 [6 7 8 2 3]))

(partition-by (partial > 1) (range -2 2))