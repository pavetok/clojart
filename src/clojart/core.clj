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
(def bools #{true false})
(def logic #{'not})

(defn classify [operator]
  (cond
    ;(re-find #".+-" (str operator)) :function
    (some #{operator} infix) :infix
    (some #{operator} bools) :bool
    (some #{operator} logic) :logic
    :else :function))

(defn underscorize [function]
  (s/replace function "-" "_"))

(defn camelize [function]
  (let [words (s/split (str function) #"-")]
    (s/join "" (cons (s/lower-case (first words)) (map s/capitalize (rest words))))))

(defmulti tokenize-seq (fn [lang expression] [lang (classify (first expression))]) :hierarchy #'taxonomy)
(defmethod tokenize-seq [:any :logic] [_ operator] operator)
(defmethod tokenize-seq [:any :infix] [_ expression]
  (let [[operator left right] expression]
    (with-meta (list left " " operator " " right) {:tokenized true})))
(defmethod tokenize-seq :default [_ expression]
  (let [[operator & args] expression]
    (with-meta
      (concat
        (list operator "(")
        (interpose ", " args)
        (list ")"))
      {:tokenized true})))

(defn tokenize [lang expression]
  (cond
    (not (seq? expression)) expression
    (:tokenized (meta expression)) expression
    :else (tokenize-seq lang expression)))

(defmulti translate (fn [lang operator] [lang (classify operator)]) :hierarchy #'taxonomy)
(defmethod translate [:underscore :function] [_ operator] (underscorize operator))
(defmethod translate [:camelCase :function] [_ operator] (camelize operator))
(defmethod translate [:python :bool] [_ operator] (s/capitalize operator))
(defmethod translate :default [_ operator] (str operator))

(defn generate
  "Generator of language specific assertions"
  [lang expression]
  ;(println expression)
  (cond
    (not (seq? expression)) (translate lang expression)
    :else (s/join "" (->>
                       (tokenize lang expression)
                       (map #(tokenize lang %))
                       (map #(generate lang %))
                       ))
    )
  )
