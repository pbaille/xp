(ns fun.km
  (:refer-clojure :exclude [extend set get empty map])
  (:require [fun.utils :as u]
            [clojure.core :as c]))

;impl -----------------------------------------------

(defn kset? [x]
  (and (set? x)
       (every? keyword? x)))

(defn kset->km [x]
  (apply hash-map
         (interleave x (repeat true))))

(defn nilable? [x]
  (or (nil? x)
      (and
        ;; empty records are kept to conserve type
        (not (record? x))
        ;; empty colls are rejected
        (coll? x) (empty? x))))

(defn km? [x]
  (and (map? x)
       (every? keyword? (keys x))))

(defn parse-args

  [xs]

  (loop [ret []
         [x & nxt] xs]

    (cond

      (keyword? x)
      (recur (conj ret {x (first nxt)})
             (next nxt))

      (kset? x)
      (recur (conj ret (kset->km x))
             nxt)

      (km? x)
      (recur (conj ret x)
             nxt)

      (sequential? x)
      (recur (u/catv ret (parse-args x))
             nxt)

      (not x)
      (if (seq nxt)
        (recur ret nxt)
        ret)

      :else
      (u/scream!))))

;; api ----------------------------------------------

(defn km [& xs]
  (apply merge (parse-args xs)))

(defn empty [x]
  (if (record? x)
    (apply dissoc x (keys x))
    {}))

(defn map-vals [f m]
  (into (empty m)
        (c/map (fn [[k v]] [k (f v)]) m)))

(defn map [f m]
  (into (empty m)
        (c/map (fn [[k v]] (f k v)) m)))

;; tests --------------------------------------------

(do "checks"

    (km? {})

    (assert
      (= (km :a 1 :b 2)
         (km {:a 1 :b 2})
         (km :a 1 {:b 2})
         (km {:a 1} :b 2)
         (km [:a 1] :b 2)
         (km [:a 1 :b 2])))

    (assert
      (= (km [:a :b]) (km {:a :b}) (km :a :b)))

    (assert
      (= (km :a true :b true)
         (km #{:a :b})
         (km #{:a} #{:b})
         (km #{:a} [:b true])))

    (u/check-fn
      km
      [:a 1 :b 2]
      {:a 1 :b 2}

      [#{:a :b}]
      {:a true :b true}

      [:a 1 #{:b} {:c 2} :c 1]
      {:a 1 :b true :c 1})
    )

;; get (not used in core) ---------------------------------------------------

(defn get-impl
  [{:keys
    [get-fn
     parse
     empty]
    :or
    {get-fn c/get
     empty {}
     parse #(remove nilable? %)}}]

  (letfn
    [(looop [m x]

       ;(println "looop: " x)
       (u/simple-kw? x)
       (get-fn m x)

       (keyword? x)
       (reduce looop m (u/kw->path x))

       (vector? x)
       (reduce looop m x)

       (set? x)
       (reduce
         (fn [a e]
           (km a {e (looop m e)}))
         {} x)

       (km? x)
       (km (map-vals #(looop m %) x))

       :else
       (u/throw* "km.get bad format\n" x "\n" m))]

    (fn self
      [target
       & [a1 & anxt :as args]]
      (cond
        (not a1) target
        (not anxt) (looop target a1)
        :else
        (km empty (c/map #(looop target %) (parse args)))))))

(def get (get-impl {}))










