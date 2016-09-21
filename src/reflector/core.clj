(ns reflector.core)
(use 'clojure.string)

(def operators {:assert "assert" :is-prime "isPrime"})

(defn generate
  "docstring"
  [expression]
  (if (not (list? expression))
    expression
    (let [[name, operands] expression]
      (str (operators (keyword name)) "(" (generate operands) ")")
      )
    )
  )
