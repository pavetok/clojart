(ns clojart.core)

(require '[clojure.string :as s])

(def langs (-> (make-hierarchy)
               (derive :python :underscore)
               (derive :ruby :underscore)
               (derive :java :camelCase)
               (derive :js :camelCase)
               ))

(defmulti translate (fn [lang expression & _] [lang expression]) :hierarchy #'langs)
(defmethod translate [:camelCase 'is-prime] [_ _] 'isPrime)
(defmethod translate [:underscore 'is-prime] [_ _] 'is_prime)
(defmethod translate [:python 'true] [_ _] 'True)
(defmethod translate :default [_ expression] expression)

(defn generate
  "Generator of language specific assertions"
  [lang expression]
  (if (not (list? expression))
    (str (translate lang expression))
    (let [[operator operands] expression]
      (str (translate lang operator) "(" (generate lang operands) ")")
      )
    )
  )
