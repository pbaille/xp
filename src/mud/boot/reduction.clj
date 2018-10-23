(ns mud.boot.reduction
  (:use mud.boot.generics)
  (:use mud.boot.utils))

(defmacro reduction
  "define a 2 argument generic of type a -> b -> a
   automaticaly implement a variadic arity
   that reduce with the defined function"
  [name argv & decls]
  `(generic ~name
            ([x#] x#)
            ~(list* argv decls)
            (~(conj argv '& 'xs)
              (reduce ~name (~name ~@argv) ~'xs))))

;; tests ------------------------------------------

(comment

  (exp (reduction fmap'
     [x f]
     :map (into {} (map (fn [[k v]] [k (f v)]) x))
     :set (set (map f x))
     :vec (mapv f x)
     :seq (map f x)))

  (fmap' [1 2 3] inc inc)
  (fmap' nil inc inc))