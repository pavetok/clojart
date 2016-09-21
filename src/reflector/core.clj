(ns reflector.core)
(use 'clojure.string)

(defn generate
  "docstring"
  [expression]
  (if (not (list? expression))
    expression
    (let [[head, tail] expression]
      (str head "(" (generate tail) ")")
      )
    )
  )
