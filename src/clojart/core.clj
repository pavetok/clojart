(ns clojart.core)

(require '[clojure.string :as s])

(def taxonomy (-> (make-hierarchy)
                  (derive :underscore :any)
                  (derive :camelCase :any)
                  (derive :python :underscore)
                  (derive :ruby :underscore)
                  (derive :java :camelCase)
                  (derive :js :camelCase)))

(def infix #{'+ '-})

(defn classify [operator]
  (cond
    (re-find #".+-" (str operator)) :function
    (some #{operator} infix) :infix
    :else operator))

(defn underscorize [function]
  (s/replace function "-" "_"))

(defn camelize [function]
  (let [words (s/split (str function) #"-")]
    (s/join "" (cons (s/lower-case (first words)) (map s/capitalize (rest words))))))

(defmulti enrich (fn [lang expression] [lang (classify (first expression))]) :hierarchy #'taxonomy)
(defmethod enrich [:any 'assert] [_ expression] expression)
(defmethod enrich :default [_ expression] (cons :ob (conj (vec expression) :cb)))

(defmulti rearrange (fn [lang expression] [lang (classify (first expression))]) :hierarchy #'taxonomy)
(defmethod rearrange [:any :infix] [_ expression]
  (let [[op left right] expression] (list left op right)))

(defmulti translate (fn [lang operator] [lang (classify operator)]) :hierarchy #'taxonomy)
(defmethod translate [:underscore :function] [_ operator] (underscorize operator))
(defmethod translate [:camelCase :function] [_ operator] (camelize operator))
(defmethod translate [:python 'true] [_ _] 'True)
(defmethod translate [:any :ob] [_ _] \()
(defmethod translate [:any :cb] [_ _] \))
(defmethod translate :default [_ operator] operator)

(defn generate
  "Generator of language specific assertions"
  [lang expression]
  (if (not (list? expression))
    (str (translate lang expression))
    (let [[operator & operands] expression]
      (s/join "" (map #(generate lang %) (enrich lang expression)))
      )
    )
  )
