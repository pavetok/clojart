(ns clojart.core)

(require '[clojure.string :as s])

(def kinds {:python :underscore,
            :ruby :underscore,
            :java :camelCase,
            :js :camelCase})

(def dictionary {})

(defn underscorize [function] (s/replace function "-" "_"))

(defn camelize [function]
  (let [words (s/split (str function) #"-")]
    (s/join "" (cons (s/lower-case (first words)) (map s/capitalize (rest words))))))

(defmulti to (fn [lang operator] [(lang kinds) (dictionary operator :function)]))
(defmethod to [:underscore :function] [_ operator] (underscorize operator))
(defmethod to [:camelCase :function] [_ operator] (camelize operator))
(defmethod to :default [_ operator] operator)

(defn generate
  "Generator of language specific assertions"
  [lang expression]
  (if (not (list? expression))
    (str (to lang expression))
    (let [[operator operands] expression]
      (str (to lang operator) "(" (generate lang operands) ")")
      )
    )
  )
