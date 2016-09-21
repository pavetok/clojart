(ns reflector.core)
(use 'clojure.string)

(defn generate
  "docstring"
  [expression]
  (let [[head, tail] expression]
    (str head "(" tail ")")
    )
  )
