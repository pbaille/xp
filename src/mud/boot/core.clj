(ns mud.boot.core
  (:refer-clojure :exclude [pr parents])
  (:require
    [backtick :as bt]
    [mud.boot.types :as t]
    [mud.boot.generics :as g]
    [mud.boot.reduction :as r]
    [mud.boot.curried :as c]
    [mud.boot.data :as d]
    [mud.boot.utils :as u]))

;; state ---------------------------------

(def types t/reg)
(def generics g/reg)

;; api -----------------------------------

(u/import-fns
  quotef u/quotef
  ns-qualify u/ns-qualify
  isa t/isa
  childs t/childs
  childof t/childof
  parents t/parents
  parentof t/parentof
  type>= t/>=
  type<= t/<=
  all-types t/all-types
  pr u/probe)

(u/import-macros
  defg g/generic
  extendg g/generic+
  defred r/reduction
  curf c/curf
  defcurf c/defcurf
  data d/data
  asserts u/asserts
  tpl bt/template
  sq bt/syntax-quote
  exp u/exp
  ;expnth u/expnth
  ;exprec u/exprec
  dbg u/dbg
  err u/err)

(defmacro group+
  "add a group to the type system
   or append to it if already present
   and recompile generics"
  [tag primset]
  `(do ~(t/group+ tag primset)
       (g/compile-all!)))

(defmacro prim+
  "add a primitive to the type system
   and recompile generics"
  [tag class & [groups]]
  `(do ~(t/prim+ tag [class] groups)
       (g/compile-all!)))

;; tests -------------------------------

(do

  )
