(ns clojart.core)

(require '[clojure.string :as s])

(def taxonomy (-> (make-hierarchy)
                  (derive :python :underscore)
                  (derive :ruby :underscore)
                  (derive :java :camelCase)
                  (derive :js :camelCase)))

(defn classify [operator]
  (if (re-find #".+-" (str operator))
    :function
    operator))

(defn underscorize [function]
  (s/replace function "-" "_"))

(defn camelize [function]
  (let [words (s/split (str function) #"-")]
    (s/join "" (cons (s/lower-case (first words)) (map s/capitalize (rest words))))))

(defmulti to (fn [lang operator] [lang (classify operator)]) :hierarchy #'taxonomy)
(defmethod to [:underscore :function] [_ operator] (underscorize operator))
(defmethod to [:camelCase :function] [_ operator] (camelize operator))
(defmethod to [:python 'true] [_ _] 'True)
(defmethod to :default [_ operator] operator)

(defn generate
  "Generator of language specific assertions"
  [lang expression]
  (if (not (list? expression))
    (str (to lang expression))
    (let [[operator & operands] expression]
      (s/join "" (cons (to lang operator) (map #(generate lang %) operands)))
      )
    )
  )
