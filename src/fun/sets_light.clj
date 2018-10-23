;; sets was dog slow after all this hard work...
;; In order to compare performance, here is this minimal implementation
;; that is just a thin wrapper around sorted sets

(ns fun.sets-light
  (:use [fun.core])
  (:require [clojure.core :as c]
            [fun.utils :as u]
            [clojure.set :as set]))

(def nset0
  (m

    :tag 'nset

    :nset? true

    :members
    (sorted-set-by <)

    :mk
    (c [o xs]
       (let [flatten
             (mapcat (fn [x]
                       (if (number? x)
                         [x]
                         (x :members)))
                     xs)]
         (update o
                 :members
                 #(into (empty %) flatten))))

    :eq (c [o p] (= (o :members) (p :members)))
    :member? (c [o n] (o :members n))
    :min (c [o] (first (o :members)))
    :max (c [o] (first (o :rseq)))
    :len (c [o] (count (o :members)))
    :seq (c [o] (seq (o :members)))
    :rseq (c [o] (rseq (o :members)))

    :splat
    (c [o n]
       [(o :mk (take n (o :members)))
        (o :mk (drop n (o :members)))])

    :splatv
    (c [o v]
       [(o :mk (take-while #(<= v %) (o :members)))
        (o :mk (drop-while #(>= v %) (o :members)))])

    :disj
    (c [o p]

       (cond
         (number? p)
         (update o :members disj p)
         (p :nset?)
         (assoc o :members (set/difference (o :members) (p :members)))))

    :conj
    (c [o p]

       (cond
         (number? p)
         (update o :members conj p)
         (p :nset?)
         (assoc o :members (set/union (o :members) (p :members))))
       )

    :intersect
    (c [o p]

       (cond
         (number? p) (o :unit p)
         (p :nset?)
         (assoc o :members (set/intersection (o :members) (p :members)))))

    :+
    (c [o p]
       (o :mk
          (if (number? p)
            (map (fn [x] (+ x p)) (o :members))
            (map (fn [x] (p :+ x)) (o :members)))))

    :*
    (c [o p]
       (o :mk
          (if (number? p)
            (map (fn [x] (* x p)) (o :members))
            (map (fn [x] (p :* x)) (o :members)))))

    :show
    (c [o]
       (let [ms (o :members)
             l (o :len)]
         (cond
           (zero? l) 'empty
           (= 1 l) (list 'unit (first ms))
           :else (list* 'nset ms))))))

(defn nset [& xs]
  (nset0 :mk xs))

(defn rng [min max by]
  (nset0 :mk (range min max by)))

(nset 1 2 3 (rng 0 2 1/2))

((nset (rng 0 2 1/2) 3 7)
  :+ 1/2)

((nset (rng 0 2 1/2) 3 7)
  :+ (nset 33 71))

((nset (rng 0 2 1/2) 3 7)
  :+ (nset (rng 10 20 2) 33 71))

(def logic
  (m
    :var
    (c [o]
       ())))