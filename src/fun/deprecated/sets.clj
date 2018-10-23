(ns light.deprecated.sets
  (:use [light.core])
  (:require [clojure.core :as c]
            [fun.utils :as u]
            [clojure.set :as set]))

;; lonely var
(def •

  (m
    :nset
    (m

      :tag 'nset

      :nset? true

      :members (sorted-set)

      :mk
      (c [o xs]
         (let [xs (if (number? xs) [xs] (flatten xs))]
           (assert (every? number? xs))
           (o :swap
              [:members #(into % xs)])))

      :min
      (c [o]
         (first (o :members)))

      :max
      (c [o]
         (first (o :rseq)))

      :eq
      (c [o p]
         (= (o :members)
            (p :members)))

      :size
      (c [o] (- (o :max) (o :min)))

      :len
      (c [o] (count (o :members)))

      :member?
      (c [o n]
         (o :members n))

      :seq
      (c [o]
         (seq (o :members)))

      :rseq
      (c [o]
         (rseq (o :members)))

      :min!
      (c [o m]
         (o :swap
            [:members
             (partial drop-while #(< % m))]))

      :max!
      (c [o m]
         (o :swap
            [:members
             (partial take-while #(<= % m))]))

      :splat
      (c [o n]
         [(o :mk (take n (o :members)))
          (o :mk (drop n (o :members)))])

      :split
      (c [o n]
         (if (= 1 n)
           [o]
           (let [chunk-size (int (Math/ceil (/ (o :len) n)))
                 [fi nxt] (o :splat chunk-size)]
             (cons fi
                   (nxt :split (dec n))))))

      :union
      (a [o p]
         (cond
           (obj? p) (o :union.with (§ p :tag) p)
           (number? p) (o :union.with.num p))

         :with
         (m

           :num
           (c [o n]
              (o :mk (conj (o :members) n)))

           :nset
           (c [o p]
              (o :mk (set/union
                       (o :members)
                       (p :members))))
           :rng
           (c [o p]
              (let [overlap
                    (o :min! (p :min)
                       :max! (p :max))]
                (o :cset.mk
                   [(o :max! (p :min))
                    (p :union overlap)
                    (o :min! (p :max))])))

           :cset
           (c [o p]
              (p :union.with.nset o))))

      :show
      (c [o]
         (list* 'nset (o :members))))

    :rng
    (m

      :tag 'rng

      :rng? true

      :mk
      (c [o [min max step]]
         (let [min (c/min min max)
               max (c/max min max)
               max (- max (rem (- max min) step))]
           (o :def
              [:min min
               :max max
               :step step])))

      :eq
      (c [o p]
         (= (select-keys o [:ns :min :max :step])
            (select-keys p [:ns :min :max :step])))

      :size
      (c [o] (- (o :max) (o :min)))

      :empty?
      (c [o]
         (zero? (o :len)))

      :len
      (c [o] (quot (o :size) (o :step)))

      :member?
      (c [o n]
         (and
           (<= (o :min) n (o :max))
           (let [diff (- n (o :min))]
             (and (pos? diff)
                  (zero? (rem diff (o :step)))))))

      :seq
      (c [o]
         (range
           (o :min)
           (+ (o :max) 1E-10)
           (o :step)))

      :rseq
      (c [o]
         (range
           (o :max)
           (+ (o :min) 1E-10)
           (- (o :step))))

      :min!
      (c [o m]
         (if (>= m (o :max))
           (o :def [:min (o :max)])
           (loop [x (o :min)]
             (if (<= m x)
               (o :def [:min x])
               (recur (+ (o :step) x))))))

      :max!
      (c [o m]
         (if (<= m (o :min))
           (o :def [:max (o :min)])
           (loop [x (o :max)]
             (if (>= m x)
               (o :def [:max x])
               (recur (- x (o :step)))))))

      :splat
      (c [o n]
         (let [minxt (+ (o :min) (* n (o :step)))]
           [(o :max! (- minxt 1E-10))
            (o :min! minxt)]))

      :split
      (c [o n]
         (if (= 1 n)
           [o]
           (let [chunk-size (int (Math/ceil (/ (o :len) n)))
                 [fi nxt] (o :splat chunk-size)]
             (cons fi
                   (nxt :split (dec n))))))

      :union
      (a [o [p]]
         (do
           (println "rng u" o p)
           (cond
             (obj? p) (o :union.with (keyword (§ p :tag)) o p)
             (number? p) (o :union.with.num o p)))

         :with
         (m

           :num
           (f [o n]
              (println "rng u num" (o :member? n) [(o :max! (- n 1E-10)) n (o :min! (+ n 1E-10))])
              (if (o :member? n)
                o
                (o :cset.mk
                   [(o :max! (- n 1E-10)) n (o :min! (+ n 1E-10))])))

           :nset
           (f [o p]
              (reduce
                (fn [o e]
                  (o :union.with.num o e))
                o (p :seq)))

           :rng
           (f [o p]
              (let [joined
                    (and
                      (= (o :step) (p :step))
                      (zero? (rem (- (o :min) (p :min)) (o :step)))
                      (or (o :member? (p :max))
                          (o :member? (p :min)))
                      (o :min! (min (o :min) (p :min))
                         :max! (max (o :max) (p :max))))
                    slurp
                    (fn [o p]
                      (and
                        (zero? (rem (p :step) (o :step)))
                        (< (o :min) (p :min))
                        (> (o :max) (p :max))
                        o))]
                (or joined
                    (slurp o p)
                    (slurp p o)
                    (o :cset.mk [o p]))))

           :cset
           (f [o p]
              (p :union.with.rng p o))))

      :show
      (c [o]
         (list 'rng
               [(o :min) (o :max)]
               :by (o :step))))

    :cset
    (m

      :tag 'cset

      :cset? true

      :members
      (sorted-set-by
        (fn [a b]
          (< (or (§ a :min) a)
             (or (§ b :min) b))))

      :mk
      (c [o xs]
         (println "cset mk " xs)
         (reduce
           #(%1 :union %2)
           o
           (remove
             #(or (nil? %)
                  (§ % :empty?))
             xs)))

      :member?
      (c [o n]
         (or (o :members n)
             (let [closest
                   (->> (o :members)
                        (take-while
                          #(> n (or (§ :min %) %)))
                        last)]
               (§ closest :member? n))))

      :min
      (c [o]
         (let [fm (first (o :members))]
           (or (§ fm :min) fm)))

      :max
      (c [o]
         (let [lm (last (o :members))]
           (or (§ lm :max) lm)))

      :size
      (c [o]
         (- (o :min) (o :max)))

      :len
      (c [o]
         (reduce
           (fn [a m]
             (+ a (or (§ e :len) 1)))
           0
           (o :members)))

      :min!
      (c [o n]
         ;(println "cset min!" n)
         (let [xs (o :members)

               closest
               (->> xs
                    (take-while
                      #(>= n (or (§ % :min) %)))
                    last)

               kept
               (into (empty xs)
                     (drop-while #(> n (or (§ % :min) %))
                                 xs))]

           ;(println "cset min2 " closest kept)

           (println "yo: "
                    n closest
                    (§ closest :rng?)
                    (§ closest :min! n))

           (if (§ closest :rng?)
             (o :def
                [:members
                 (conj kept (§ closest :min! n))])
             (o :def [:members kept]))))

      :max!
      (c [o n]
         ;(println "cset max!" (o :members) n)
         (let [xs (o :members)

               closest
               (->> xs
                    (take-while
                      #(<= n (or (§ % :min) %)))
                    last)

               ;_ (println "closest")

               kept
               (into (empty xs)
                     (take-while #(>= n (or (§ % :min) %))
                                 xs))]

           ;(println "max! 2" closest kept)
           (if (§ closest :rng?)
             (o :def
                [:members
                 (conj kept (§ closest :max! n))])
             (o :def [:members kept]))))

      :seq
      (c [o]
         (mapcat #(or (§ % :seq) %)
                 (o :members)))

      :rseq
      (c [o]
         (mapcat #(or (§ % :rseq) %)
                 (rseq (o :members))))

      :splat
      (c [o n]
         (loop [n n a (o :cset) b o]
           (if (zero? n)
             [a b]
             (let [[fb & rb :as bs] (b :members)
                   fbl (or (§ fb :len) 1)
                   n' (- n fbl)
                   b' (b :def
                         [:members (into (empty bs) rb)])]
               (cond
                 (zero? n')
                 [(a :union fb) b']

                 (neg? n')
                 (let [[fb1 fb2] (fb :splat n)]
                   [(a :union fb1) (b' :union fb2)])

                 :else
                 (recur n' (a :union fb) b'))))))

      :split
      (c [o n]
         (println "cset split")
         (if (= 1 n)
           [o]
           (let [chunk-size (int (Math/ceil (/ (o :len) n)))
                 [fi nxt] (o :splat chunk-size)]
             (cons fi
                   (nxt :split (dec n))))))

      :union
      (a [o [p]]
         (do
           (println "cset union" p)
           (cond
             (obj? p)
             (o :union.with
                (keyword (§ p :tag))
                o p)

             (number? p)
             (o :union.with.num o p)

             (set? p)
             (o :union.with.set o p)))

         :with
         (m

           :num
           (f [o n]
              (println "cset with num" (§ o :tag) n)
              (let [pre (o :max! n :members)
                    post (o :min! n :members)]
                (println pre post)
                (o :cset.def
                   [:members
                    (into (conj pre n) post)])))

           :nset
           (f [o p]
              (reduce
                #(%1 :union.with.num %1 %2)
                o (p :members)))

           :set
           (f [o s]
              (reduce
                #(%1 :union.with.num %1 %2)
                o s))

           :rng
           (f [o p]
              (let [overlap
                    (o :min! (p :min)
                       :max! (p :max)
                       :members)]
                (if (not (seq overlap))
                  (o :swap [:members #(conj % p)])
                  (let [pre (o :max! (p :min) :members)
                        post (o :min! (p :max) :members)
                        p' (reduce #(%1 :union %2) p overlap)]
                    (o :def
                       [:members (into pre post)]
                       :union p')))))

           :cset
           (f [o p]
              (reduce
                #(%1 :union %2)
                o (p :members))))))))


(comment
  (• :rng.mk [1 3.5 1] :min! 1.1)
  (• :rng.mk [1 10 1] :max! 6.5)
  (• :rng.mk [3 89 1/3] :max! 10 :split 3))

(comment
  (• :nset.mk [1 2 3] :split 2)
  (• :nset.mk [1 2 3] :min! 2)
  (• :nset.mk [1 2 3] :max! 2))

(let [r (• :rng.mk [0 2 1/3])]
  (r :min! 3)
  ((r :union 3) :members)
  #_[((r :union 1) :members)
   ((r :union -1) :members)
   ((r :union 3) :members)
   ]
  )

(• :rng.mk [0 3/2 1/2] :min! 2)


(• :cset.mk
   [(• :rng.mk [0 3/2 1/2])
    2]
   :members)

(• :cset.mk
   [(• :rng.mk [0 3/2 1/2])
    2
    (• :rng.mk [5/2 4 1/2])]
   :members)

(• :cset.mk
   [(• :rng.mk [0 6 1/2])
    2]
   :members)

(let [cs (• :cset.swap
            [:members (fn [xs] (into xs [1 2 3]))])]

  {:min! [(cs :min! 0 :members)
          (cs :min! 2 :members)
          (cs :min! 3 :members)
          (cs :min! 4 :members)]

   :max! [(cs :max! 0 :members)
          (cs :max! 2 :members)
          (cs :max! 3 :members)
          (cs :max! 4 :members)]

   :mk [(• :cset.mk [1 2 3] :members)
        (• :cset.mk [1 2 #{2 3 5}] :members)
        #_(• :cset.mk [1 2 (• :rng.mk [7 9 1/2])] :members)
        ]})



(comment
  (• :cset.union.with.num)
  (• :cset.mk [1 2 3])
  (• :cset.mk [1 2 3 (• :rng.mk [1 3.5 1])])
  (• :cset.mk [1 2 3] :min! 2)
  (• :cset.mk [1 2 3] :max! 2))





















