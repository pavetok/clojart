(ns clojart.core)

(use 'clojure.string)

(def operators {
                :java   {"is-prime" "isPrime" "not" "!"}
                :ruby   {"is-prime" "is_prime"}
                :python {"true" "True" "is-prime" "is_prime"}
                :js     {"is-prime" "isPrime"}
                })

(defn translate
  "Translate operands"
  [lang operand]
  (let [dictionary (operators (keyword lang))]
    (get dictionary (str operand) (str operand))
    )
  )

(defn generate
  "Language specific assert expressions generator"
  [lang expression]
  (if (not (list? expression))
    (translate lang expression)
    (let [[operator, operands] expression]
      (str (translate lang operator) "(" (generate lang operands) ")")
      )
    )
  )
