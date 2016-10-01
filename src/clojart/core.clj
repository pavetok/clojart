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
    (number? operator) :number
    :else operator))

(defn underscorize [function]
  (s/replace function "-" "_"))

(defn camelize [function]
  (let [words (s/split (str function) #"-")]
    (s/join "" (cons (s/lower-case (first words)) (map s/capitalize (rest words))))))

(defmulti enrich-seq (fn [lang expression] [lang (first expression)]) :hierarchy #'taxonomy)
(defmethod enrich-seq [:any 'assert] [_ expression] expression)
(defmethod enrich-seq [:any 'not] [_ expression] expression)
(defmethod enrich-seq :default [_ expression] (cons :ob (conj (vec expression) :cb)))

(defmulti enrich-op (fn [lang operator] [lang (classify operator)]) :hierarchy #'taxonomy)
(defmethod enrich-op [:any 'assert] [_ operator] operator)
(defmethod enrich-op [:any :infix] [_ operator] operator)
(defmethod enrich-op [:any :number] [_ operator] operator)
(defmethod enrich-op [:any :ob] [_ operator] operator)
(defmethod enrich-op [:any :cb] [_ operator] operator)
(defmethod enrich-op [:any :function] [_ operator] operator)
(defmethod enrich-op :default [_ something] (with-meta (list :ob something :cb) {:enriched true}))

(defn enrich [lang expression] (if (seq? expression)
                                 (enrich-seq lang expression)
                                 (enrich-op lang expression)))

(defmulti rearrange-seq (fn [lang expression] [lang (classify (first expression))]) :hierarchy #'taxonomy)
(defmethod rearrange-seq [:any :infix] [_ expression]
  (let [[op left right] expression] (list left op right)))
(defmethod rearrange-seq :default [_ expression] expression)

(defn rearrange [lang expression] (if (seq? expression)
                                 (rearrange-seq lang expression)
                                 expression))

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
  ;(println expression)
  (cond
    (not (seq? expression)) (str (translate lang expression))
    (:enriched (meta expression)) (s/join "" (map #(translate lang %) expression))
    :else (s/join "" (->> expression
                          (map #(rearrange lang %))
                          (map #(enrich lang %))
                          (map #(generate lang %))
                          ))
    )
  )
