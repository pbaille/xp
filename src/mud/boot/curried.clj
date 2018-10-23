(ns mud.boot.curried)

;; simple curried function

;; impl ------------------------------------

(defn curf' [[nam argv expr]]
  `(fn ~nam
     ~@(map
         (fn [n]
           (let [argv (vec (take n argv))]
             `(~argv (partial ~nam ~@argv))))
         (range (if ((set argv) '&)
                  (dec (count argv))
                  (count argv))))
     (~argv ~expr)))

;; api -------------------------------------

(defmacro curf [& form]
  (if (symbol? (first form))
    (curf' form)
    (curf' (cons (gensym) form))))

(defmacro defcurf [& form]
  `(def ~(first form)
     ~(curf' form)))

;; tests -----------------------------------

(comment

  (exp (cfn er [x y & zs] (apply add x y zs)))

  ((((cfn [x y z] (add x y z)) 1) 2) 3)
  (((cfn yop [x y z] (add x y z)) 1) 2 3)
  ((cfn [x y z] (add x y z)) 1 2 3)

  (defnc yop [x y z] (add x y z))

  ((yop 1) 2 3))