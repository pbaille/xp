(ns fun.seq
  (:refer-clojure :exclude [fn cons])
  (:require [fun.core :as f :refer [§ §* m c defm obj? obj •>]]
            [fun.fn :refer [fn]]
            [fun.utils :as fu :refer [pp]]
            [clojure.core :as c]))

;; misc -----------------------------------------------------

(comment
  (def lazy-proto
    (m
      :lazy? true

      :mget
      (c [{:as • r :realize} x]
         (or (mget • x)
             (mget (r •) x)))

      :apply
      (c [{:as • r :realize} xs]
         (apply (r •) xs))

      ;; a simple closure
      ;; (c [•] ...)
      #_:realize
      #_(no-impl-error :realize)
      ))

  (defmacro lazy [e]
    `(assoc lazy-proto
       :realize (c [~'_] ~e)))

  (defn extendable [& opts]
    (m (assoc (fu/hm* opts)
         :extend
         (c
           ([• m] (merge • m))
           ([• x & xs]

             (cond

               (keyword? x)
               (let [nxt (• :extend {x (first xs)})]
                 (if-let [xs (next xs)]
                   (apply nxt :extend xs)
                   nxt))

               (map? x)
               (• :extend (apply merge (c/cons x xs))))))))))

;; pair -----------------------------------------------------

(defc pair [l r]

      :uncs
      (c [•]
         [(• :l) (• :r)])

      :l= (c [• x] (assoc • :l x))
      :r= (c [• x] (assoc • :r x))

      :l! (c [• x & xs] (• :l= (apply (• :l) (c/cons x xs))))
      :r! (c [• x & xs] (• :r= (apply (• :r) (c/cons x xs))))

      :show
      (c [•]
         (list* 'pair (• :uncs)))

      :map
      (c [• f]
         (fu/throw* "todo")
         (•> •
             (:l! :map f)
             (:r! :map f))))

;; tests

(let [c (pair 1 2)]
  [(c :l)
   (c :r)
   (c :l= 4)
   (c :r= 4)])

(declare lst cons empty-lst)

;; common operations on lists -------------------------------

(defm lst-base

      :at!
      (c [• n]
         (or (• :at n)
             (fu/throw* "out of bound")))

      :length+
      (c [•]
         (if-let [l (• :length)]
           (assoc • :length (inc l))
           •))

      :take
      (c [• n]
         (cond
           (zero? n) (• :empty)
           (• :r) (• :r! :take (dec n))
           :else •))

      :drop
      (c [• n]
         (second (• :splat n)))

      :last
      (c [•]
         (if (• :length)
           (if (• :atom?)
             (• :l)
             (• :r.last))
           (fu/throw* "no last in infinite seqs")))

      :show
      (c show [•]
         (let [[a b] (• :splat 5)]
           (concat (a :seq)
                   (when-not (b :empty?) ['...])))))

;; empty list -----------------------------------------------

(def empty-lst

  (assoc lst-base
    :seq ()
    :length 0
    :empty? true
    :empty (c [•] •)
    :cat (f [x] x)
    :at (f [_] nil)
    :splat (c [• _] [• •])
    :append (c [• x] (• :cons x))
    :cons (c [• x] (cons x •))

    :split
    (c [• n]
       (apply
         lst
         (repeat n (• :empty))))))

;; cons -----------------------------------------------------

(def cons-proto

  (assoc lst-base

    :empty? false

    :empty empty-lst

    :atom?
    (c [•] (• :r.empty?))

    :cat
    (c [• x]
       (• :r! :cat x))

    :append
    (c [• x]
       (• :r! :append x))

    :cons
    (c [• x]
       (• :l= x :r= • :length+))

    :at
    (c [• n]
       (if (zero? n)
         (• :l)
         (• :r :at (dec n))))

    :splat
    (c [• n]
       (if (pos? n)
         (let [[a b] (• :r :splat (dec n))]
           [(a :cons (• :l)) b])
         [(• :empty) •]))

    :split
    (c [• n]
       (if-let [l (• :length)]
         (let [[a b] (• :splat (fu/round (/ l n)))]
           (cons a (b :split (dec n))))
         (fu/throw* "split called on infinite seq")))


    :seq
    (c [•]
       (lazy-seq
         (c/cons (• :l) (• :r.seq))))

    ))

(defn cons [x xs & opts]
  (merge (pair x xs)
         cons-proto
         (fu/hm* opts)))

;; list constructors -----------------------------------------

(defn lst
  "list constructor"
  ([] empty-lst)
  ([x & xs]
   (cons x (apply lst xs)
         :length (inc (count xs)))))

(defn lzt
  "lazy list constructor"
  [head nxt & opts]
  (merge
    (cons
      head
      nxt
      :length nil)
    (fu/hm* opts)))

(defn iteration [f seed]
  (lzt seed
       (c [•]
          (• :l= (• :step-fn (• :l))))

       :step-fn f))

;; tests -----------------------------------------------------

(comment
  (:length ((iteration inc 0) :step-fn)))

(let [s (lst 1 2 3 4 5 6 7 8 9)]
  (s :seq)
  (s :cat s :splat 9)
  (s :split 4))

(comment
  ((lst) :cons 1)
  ((lst 1 2 3) :length)
  (((lst 1 2 3 4 5 6 7) :cons 10) :take 3)

  ((lst 1 2 3 4) :r! :l)
  ((lst 1 2 3 4) :take 2)
  (apply ((lst 1 2 3 4) :r) [:l]))




