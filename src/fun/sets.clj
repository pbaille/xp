;; an extensive implementation of sets
;; but too slow apparently
;; despite this fact, the implementation is nice
;; and tested

(ns fun.sets
  (:use [fun.core])
  (:require [clojure.core :as c]
            [fun.utils :as u]
            [clojure.set :as set]))

(def •

  (m

    :prototypes
    (m

      :rng0
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
           (= (select-keys o [:tag :min :max :step])
              (select-keys p [:tag :min :max :step])))

        :len
        (c [o]
           (inc (quot (o :size) (o :step))))

        :member?
        (c [o n]
           (and
             (<= (o :min) n (o :max))
             (let [diff (- n (o :min))]
               (and (>= diff 0)
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
             (- (o :min) 1E-10)
             (- (o :step))))

        :splat
        (c [o n]
           (cond

             (<= n 0) [(o :empty) o]
             (>= n (o :len)) [o (o :empty)]

             :else
             (let [minxt (+ (o :min) (* n (o :step)))]
               [(o :def [:max (- minxt (o :step))])
                (o :def [:min minxt])])))

        :splatv
        (c [o v]
           (cond

             (> v (o :max)) [o (o :empty)]
             (< v (o :min)) [(o :empty) o]
             (= v (o :min)) [(o :unit v) o]
             (= v (o :max)) [o (o :unit v)]

             :else
             (let [diff (- v (o :min))
                   r (rem diff (o :step))]
               (if (zero? r)
                 [(o :def [:max v])
                  (o :def [:min v])]
                 [(o :def [:max (- v r)])
                  (o :def [:min (+ (- v r) (o :step))])]))))

        :disj
        (c [o p]

           (if (number? p)

             (if (o :member? p)
               (o :nset [(o :sel< p) (o :sel> p)])
               o)

             (reduce #(%1 :disj %2)
                     o (p :seq))))

        :conj
        (c [o p]
           (cond

             (§ p :empty?) o

             (number? p)
             (cond

               (o :member? p) o

               (or (> p (o :max))
                   (< p (o :min)))
               (o :empty.members!
                  [o p])

               :else
               (o :empty.members!
                  [(o :sel< p)
                   (o :sel> p)
                   p]))

             (§ p :rng?)
             (let [;_ (println "rng conj rng")
                   joined
                   (and
                     (= (o :step) (p :step))
                     (zero? (rem (- (o :min) (p :min)) (o :step)))
                     (or (o :member? (p :max))
                         (o :member? (p :min)))
                     (o :sel=
                        (min (o :min) (p :min))
                        (max (o :max) (p :max))))
                   slurp
                   (fn [o p]
                     (and
                       (zero? (rem (p :step) (o :step)))
                       (<= (o :min) (p :min))
                       (>= (o :max) (p :max))
                       o))]

               (or joined
                   (slurp o p)
                   (slurp p o)
                   (o :nset [o p])))

             (§ p :nset?)
             (p :conj o)

             ))

        :intersect
        (c [o p]

           (if (number? p)

             (o :unit p)

             (let [min (min (o :min) (p :min))
                   max (max (o :max) (p :max))
                   o' (o :sel= min max)
                   p' (p :sel= min max)]

               (cond
                 (§ p :rng?)
                 (or (and (= (o :step) (p :step))
                          (= (o' :min) (p' :min))
                          o')
                     (and (zero? (rem (o :step) (p :step)))
                          (p' :member? (o' :min))
                          o')
                     (and (zero? (rem (p :step) (o :step)))
                          (o' :member? (p' :min))
                          p')
                     (o :nset
                        (filter #(o' :member? %)
                                (p' :seq))))

                 (§ p :nset?)
                 (o :nset
                    (mapv #(o' :intersect %)
                          (p' :members)))))
             ))

        :+
        (c [o p]
           ;(println "ert" o p)
           (cond
             (number? p)
             (o :swap
                [:min (partial + p)
                 :max (partial + p)])

             (§ p :rng?)
             (o :nset
                (map #(o :+ %) (p :seq)))

             (§ p :nset?)
             (o :nset
                (map #(o :+ %) (p :members)))))

        :show
        (c [o]
           (list 'rng
                 (o :min)
                 (o :max)
                 '. (o :step))))

      :nset0
      (m

        :tag 'nset

        :nset? true

        :members
        (sorted-set-by
          (fn [a b]
            (< (or (§ a :min) a)
               (or (§ b :min) b))))

        :members!
        (c [o xs]
           (o :swap
              [:members
               (fn [ms]
                 (into (empty ms)
                       (remove #(§ % :empty?) xs)))]))

        :normalize-member
        (f [m]
           (if (and (§ m :rng?)
                    (= (m :min) (m :max)))
             (m :min)
             m))

        :mk
        (c [o xs]
           (reduce
             #(%1 :conj %2)
             o (->> xs
                    (remove nil?)
                    (map (o :normalize-member)))))

        :eq
        (c [o p]
           (cond
             (§ p :nset?)
             (or (= (o :members) (p :members))
                 (= (o :swap
                       [:members
                        #(set/difference % (p :members))]
                       :seq)
                    (p :swap
                       [:members
                        #(set/difference % (o :members))]
                       :seq)))

             (§ p :rng)
             (= (o :seq) (p :seq))

             (number? p)
             (and (= 1 (o :len))
                  (= (first (o :members)) p))))

        :member?
        (c [o n]
           (or (o :members n)
               (let [closest
                     (->> (o :members)
                          (take-while
                            #(> n (or (§ % :min) %)))
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

        :len
        (c [o]
           (reduce
             (fn [a m]
               (+ a (or (§ m :len) 1)))
             0 (o :members)))

        :seq
        (c [o]
           (mapcat #(or (§ % :seq) [%])
                   (o :members)))

        :rseq
        (c [o]
           (mapcat #(or (§ % :rseq) [%])
                   (rseq (o :members))))

        :splat
        (c [o n]

           (cond

             (<= n 0) [(o :empty) o]
             (>= n (dec (o :len))) [o (o :empty)]

             :else
             (loop [i 0
                    left []
                    [r1 & rnxt :as right] (seq (o :members))]
               (cond
                 (= i n)
                 [(• :nset left)
                  (• :nset right)]

                 (> i n)
                 (let [ll (last left)
                       [a b] (ll :splat (- (ll :len) (- i n)))]
                   [(• :nset (conj (butlast left) a))
                    (• :nset (conj right b))])

                 (< i n)
                 (recur (+ i (or (§ r1 :len) 1))
                        (conj left r1)
                        rnxt)
                 )
               ))
           )

        :splatv
        (c [o v]
           ;(println "nset splatv" o v (o :members) (o :max))
           (cond

             (o :empty?)
             [(o :empty) (o :empty)]

             (> v (o :max)) [o (o :empty)]
             (< v (o :min)) [(o :empty) o]
             (= v (o :min)) [(o :unit v) o]
             (= v (o :max)) [o (o :unit v)]

             :else
             (let [[pre [fp :as post]]
                   (partition-by #(<= v (or (§ % :min) %))
                                 (o :members))
                   done?
                   (= v (or (§ fp :min) fp))

                   closest (last pre)]

               #_(printl done? pre post)

               (cond

                 (and (not done?)
                      (§ closest :rng?))
                 (let [[a b] (closest :splatv v)]
                   ;(println pre post a b)
                   [(o :nset (conj (butlast pre) a))
                    (o :nset (conj post b))])

                 done?
                 [(o :nset (conj pre fp))
                  (o :nset post)]

                 :else
                 [(o :nset pre)
                  (o :nset post)]
                 ))))

        :disj
        (c [o p]

           (cond

             (o :members p)
             (o :swap [:members #(disj % p)])

             (number? p)
             (if (o :member? p)
               (o :nset [(o :sel< p) (o :sel> p)])
               o)

             :else
             (reduce #(%1 :disj %2)
                     o (p :seq))))

        :conj
        (c [o p]

           #_(println "nset conj" o p)
           (cond

             (number? p)
             (cond

               (o :member? p) o

               (or (o :empty?)
                   (> p (o :max))
                   (< p (o :min)))
               (o :swap
                  [:members #(conj % p)])

               :else
               (o :empty.swap
                  [:members
                   #(-> (empty %)
                        (into (o :sel< p :members))
                        (into (o :sel> p :members))
                        (conj p))]))

             ;(§ p :empty?) o

             (§ p :rng?)
             (let [overlap
                   (o :sel= (p :min) (p :max))]
               ;(println "nset conj rng" overlap (overlap :empty?))
               (if (overlap :empty?)

                 (o :swap
                    [:members #(conj % p)])

                 (o :nset
                    [(o :sel< (p :min))
                     (o :sel> (p :max))
                     (reduce #(%1 :conj %2)
                             p (overlap :members))])))

             (§ p :nset)
             (reduce
               #(%1 :conj %2)
               o (p :members))

             ))

        :intersect
        (c [o p]

           (cond

             (number? p) (o :unit p)

             (or (> (o :min) (p :max))
                 (< (o :max) (p :min)))
             (o :empty)

             :else
             (let [min' (max (o :min) (p :min))
                   max' (min (o :max) (p :max))
                   o' (o :sel= min' max')
                   p' (p :sel= min' max')]

               (cond

                 (§ p :rng?)
                 (o :nset (mapv #(p' :intersect %) o'))

                 (§ p :nset?)
                 (let [common-ms
                       (set/intersection
                         (into #{} (o' :members))
                         (into #{} (p' :members)))
                       o-extra (set/difference (o' :members) common-ms)
                       p-extra (set/difference (p' :members) common-ms)
                       ox (o :nset o-extra)
                       px (o :nset p-extra)]

                   (if (> (count o-extra)
                          (count p-extra))

                     (o :nset
                        (concat common-ms
                                (map #(ox :intersect %)
                                     p-extra)))
                     (o :nset
                        (concat common-ms
                                (map #(px :intersect %)
                                     o-extra)))))))

             ))

        :+
        (c [o p]
           ;(println "top" o p)
           (o :nset
              (if (number? p)
                (map #(if (number? %)
                        (+ % p)
                        (% :+ p))
                     (o :members))
                (map #(p :+ %)
                     (o :members)))))

        :show
        (c [o]
           (let [ms (o :members)
                 l (o :len)]
             (cond
               (zero? l) 'empty
               (= 1 l) (list 'unit (first ms))
               :else (list* 'nset ms)))))

      )

    ;; common ----------

    :min-step
    1/1000000000

    :commons
    (m

      :sel (c [o from to] (o :sel> from :sel< to))
      :sel= (c [o from to] (o :sel>= from :sel<= to))
      :sel> (c [o v] (second (o :splatv+ v)))
      :sel< (c [o v] (first (o :splatv- v)))
      :sel>= (c [o v] (second (o :splatv v)))
      :sel<= (c [o v] (first (o :splatv v)))

      :empty?
      (c [o] (zero? (§ o :len)))

      :size
      (c [o] (- (o :max) (o :min)))

      :splatv+ (c [o v] (o :splatv (+ v (o :min-step))))
      :splatv- (c [o v] (o :splatv (- v (o :min-step))))

      :split
      (c [o n]
         (cond
           (= 1 n) [o]

           (>= n (o :len))
           (map #(o :nset [%]) (o :seq))

           (pos? n)
           (let [chunk-size (u/round (/ (o :len) n))
                 [fi nxt] (o :splat chunk-size)]
             (cons fi
                   (nxt :split (dec n))))))

      )

    ;; constructors -----

    :rng
    (c [o spec]
       (o :commons.def
          (o :prototypes.rng0)
          :mk spec))

    :empty
    (c [o]
       (o :commons.def
          (o :prototypes.nset0)))

    :unit
    (c [o x]
       (o :nset [x]))

    :nset
    (c [o xs]
       ;(println "iop" xs)
       (o :commons.def
          (o :prototypes.nset0)
          :mk xs))

    ))

;; tests -----------------------------------------------

(def dev? false)

;; rng
(when dev?
  (let [r1 (• :rng [0 3 1/3])
        r2 (• :rng [0 3 2/3])]

    (u/asserts

      (r1 :member? 1)
      (not (r1 :member? 10))

      (r1 :member? 0)

      (r1 :eq r1)

      (= (r2 :seq)
         '(0 2/3 4/3 6/3 8/3))

      (= (r2 :rseq)
         (reverse (r2 :seq)))

      (r1 :sel< 10 :eq r1)
      (r1 :sel> -1 :eq r1)
      (r1 :sel<= 3 :eq r1)
      (r1 :sel>= 0 :eq r1)
      (r1 :sel<= 1 :eq (• :rng [0 1 1/3]))
      (r1 :sel>= 2 :eq (• :rng [2 3 1/3]))
      (r1 :sel< 1 :eq (• :rng [0 2/3 1/3]))
      (r1 :sel> 2 :eq (• :rng [7/3 3 1/3]))

      (= r1
         (r1 :conj (• :empty))
         (r1 :conj r2)
         (r2 :conj r1)
         (r1 :conj 1))

      (r1 :conj 5
          :eq (• :nset [r1 5]))

      (r1 :disj 3 :eq
          (• :rng [0 8/3 1/3]))

      (r1 :disj 1 :eq
          (• :nset
             [(• :rng [0 2/3 1/3])
              (• :rng [4/3 3 1/3])]))

      (r1 :disj r2 :eq
          (• :nset [1/3 1 5/3 7/3 3]))

      (= (r1 :splat 5)
         [(• :rng [0 4/3 1/3])
          (• :rng [5/3 3 1/3])])

      (= (r1 :splat -1) [(• :empty) r1])
      (= (r1 :splat 10) [r1 (• :empty)])


      (= (r1 :splatv 2)
         [(• :rng [0 2 1/3])
          (• :rng [2 3 1/3])])

      (= (r1 :splatv 0) [(• :unit 0) r1])
      (= (r1 :splatv 10/3) [r1 (• :empty)])
      (= (r1 :splatv+ 3) [r1 (• :empty)])
      (= (r1 :splatv- 0) [(• :empty) r1])

      (r1 :split 10)
      (r1 :split 3)
      (r1 :split 1)
      (not (r1 :split 0))

      (r1 :intersect 1 :eq (• :unit 1))
      (r1 :intersect r2 :eq r2)
      (r2 :intersect r1 :eq r2)
      (r1 :intersect (• :rng [1/2 4 1/3])
          :eq (• :empty))

      (r1 :intersect (• :rng [0 4 1/2])
          :eq (• :nset [0 1 2 3])))
    ))

;; nset
(when dev?
  (let [s1 (• :nset [1 3 7 9])
        s2 (• :nset [(• :rng [0 2 1/2]) 3 5])
        ]

    ;(s2 :conj 1/3)

    (u/asserts

      (s1 :member? 1)
      (s1 :member? 7)
      (s2 :member? 1/2)
      (s2 :member? 5)
      (not (s1 :member? 10))
      (not (s1 :member? 1/2))
      (not (s2 :member? 10))
      (not (s2 :member? 1/3))

      (s1 :eq s1)
      (s2 :eq s2)
      (not (s1 :eq s2))
      (not (s2 :eq s1))

      (= (s1 :seq) '(1 3 7 9))
      (= (s2 :seq) '(0 1/2 1 3/2 2 3 5))
      (= (s1 :rseq) (reverse (s1 :seq)))
      (= (s2 :rseq) (reverse (s2 :seq)))

      (s1 :sel< 10 :eq s1)
      (s1 :sel> 0 :eq s1)
      (s1 :sel<= 9 :eq s1)
      (s1 :sel>= 1 :eq s1)

      (s1 :sel<= 5 :eq (• :nset [1 3]))
      (s1 :sel>= 2 :eq (• :nset [3 7 9]))
      (s1 :sel< 9 :eq (• :nset [1 3 7]))
      (s1 :sel> 3 :eq (• :nset [7 9]))

      (= s1
         (s1 :conj (• :empty))
         (s1 :conj (• :nset [3 7]))
         (s1 :conj s1)
         (s1 :conj 1)
         (s1 :conj 3))

      (s1 :conj s2 :eq (s2 :conj s1))

      (s1 :conj s2 :eq
          (• :nset [(• :rng [0 2 1/2]) 3 5 7 9]))

      (s2 :conj 1/3 :eq
          (• :nset [0 1/3 (• :rng [1/2 2 1/2]) 3 5]))

      (s1 :disj 1 :eq (• :nset [3 7 9]))
      (s1 :disj 3 :eq (• :nset [1 7 9]))
      (s1 :disj 7 :eq (• :nset [1 3 9]))
      (s1 :disj 9 :eq (• :nset [1 3 7]))
      (s1 :disj 10 :eq s1)
      (s1 :disj -1 :eq s1)
      (s2 :disj 1 :eq
          (• :nset [(• :rng [0 1/2 1/2])
                    (• :rng [3/2 2 1/2])
                    3 5]))

      (= (s1 :splat 2)
         [(• :nset [1 3])
          (• :nset [7 9])])

      (= (s1 :splatv 3)
         [(• :nset [1 3])
          (• :nset [3 7 9])])

      (= (s1 :splatv 2)
         [(• :nset [1])
          (• :nset [3 7 9])])

      (= (s1 :splatv 1)
         [(• :unit 1)
          (• :nset [1 3 7 9])])

      (= (s1 :splatv 9)
         [(• :nset [1 3 7 9])
          (• :unit 9)])

      (= (s2 :splatv 1)
         [(• :unit (• :rng [0 1 1/2]))
          (• :nset [(• :rng [1 2 1/2]) 3 5])])

      (= (s2 :splatv 2)
         [(• :unit (• :rng [0 2 1/2]))
          (• :nset [2 3 5])])

      (= (s1 :splatv- 1)
         [(• :empty)
          (• :nset [1 3 7 9])])

      (= (s1 :splatv- 2)
         (s1 :splatv 2))

      (= (s1 :splatv+ 9)
         [(• :nset [1 3 7 9])
          (• :empty)])

      (= (s1 :splatv+ 2)
         (s1 :splatv 2))

      (= (s1 :splat 2)
         [(• :nset [1 3]) (• :nset [7 9])])

      (= (s1 :splat 0) [(• :empty) s1])
      (= (s1 :splat -1) [(• :empty) s1])
      (= (s1 :splat 5) [s1 (• :empty)])

      (= (s2 :splat 1)
         [(• :unit 0)
          (• :nset [(• :rng [1/2 2N 1/2]) 3 5])])

      (= (s2 :splat 3)
         [(• :unit (• :rng [0 1 1/2]))
          (• :nset [(• :rng [3/2 2N 1/2]) 3 5])])

      (= (s2 :splat 7)
         [s2 (• :empty)])

      (s1 :split 1)
      (s1 :split 3)
      (s1 :split 9)

      (s2 :split 1)
      (s2 :split 3)
      (s2 :split 9)

      (s1 :intersect 3)

      (s1 :intersect s2 :eq (• :nset [1 3]))
      )

    ))



(comment "+ and bench..."

         (• :nset [1 2 3] :+ 6)
         (• :nset [1 2 3] :+ (• :rng [0 2 1/2]))

         ;; OMG this is slow...
         (time
           (dotimes [_ 1]
             (• :nset [0 1/2 1 3/2 2 3 7]
                :+ (• :nset [10 12 14 16 18 20 33 71]))))

         (time
           (dotimes [_ 1]
             (• :nset [(• :rng [0 2 1/2]) 3 7]
                :+ (• :nset [(• :rng [10 20 2]) 33 71])))))



