(ns mud.boot.data
  (:use [mud.boot.utils])
  (:require [mud.boot.types :as t]
            [mud.boot.generics :as g]))

(defn parse-data [[name fields & impls]]
  )

(defmacro data
  [name fields & impls]
  (let [tag (keyword name)
        recsym (symcat name 'Record)
        mmk (symcat 'map-> recsym)
        pmk (symcat '-> recsym)
        fields-kws (mapv keyword fields)]
    `(do
        (declare ~name)
        (defrecord ~recsym [])
        (t/prim+ ~tag [~recsym] [])
        (g/type+ ~tag
          ;(~'pure [x#] (~(symcat recsym '.)))
          ~@impls)
        (defn ~name [& xs#]
          (if (every? (set (take-nth 2 xs#)) ~fields-kws)
            (~mmk (apply hash-map xs#))
            (~mmk (zipmap ~fields-kws xs#)))))))

(exp (data pt [x y]
       (iter [x] ((juxt :x :y) x))))




