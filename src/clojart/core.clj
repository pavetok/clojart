(ns clojart.core)

(require '[clojure.string :as s])

(def taxonomy (-> (make-hierarchy)
                  (derive :python :any)
                  (derive :ruby :any)
                  (derive :java :any)
                  (derive :js :any)

                  (derive :python :underscore)
                  (derive :ruby :underscore)
                  (derive :java :camelCase)
                  (derive :js :camelCase)

                  (derive :js :!)
                  (derive :java :!)
                  (derive :ruby :!)

                  (derive :python :simple)
                  (derive :ruby :simple)
                  ))

(def infix #{'+ '-})
(def logic #{'not})

(defn classify [operator]
  (cond
    (some #{operator} infix) :infix
    (instance? Boolean operator) :bool
    (some #{operator} logic) :logic
    (= operator 'def) :variable
    :else :function))

(defn underscorize [function]
  (s/replace function "-" "_"))

(defn camelize [function]
  (let [words (s/split (str function) #"-")]
    (s/join "" (cons (s/lower-case (first words)) (map s/capitalize (rest words))))))

(defmulti tokenize-seq (fn [lang expression] [lang (classify (first expression))]) :hierarchy #'taxonomy)

(defmethod tokenize-seq [:any :logic] [_ operator] operator)

(defmethod tokenize-seq [:python :logic] [_ expression]
  (let [[operator operand] expression]
    (list operator " " operand)))

(defmethod tokenize-seq [:any :infix] [_ expression]
  (let [[operator left right] expression]
    (list left " " operator " " right)))

(defmethod tokenize-seq [:simple :variable] [_ expression]
  (let [[_ name value] expression]
    (list name " " '= " " value)))

(defmethod tokenize-seq [:js :variable] [_ expression]
  (concat '(var " ") (tokenize-seq :simple expression)))

(defmethod tokenize-seq :default [_ expression]
  (let [[operator & args] expression]
    (concat
      (list operator "(")
      (interpose ", " args)
      (list ")"))))

(defn tokenize [lang expression]
  (cond
    (not (seq? expression)) expression
    (:tokenized (meta expression)) expression
    :else (with-meta (tokenize-seq lang expression) {:tokenized true})))

(defmulti translate (fn [lang operator] [lang (classify operator)]) :hierarchy #'taxonomy)
(defmethod translate [:underscore :function] [_ operator] (underscorize operator))
(defmethod translate [:camelCase :function] [_ operator] (camelize operator))
(defmethod translate [:python :bool] [_ operator] (s/capitalize operator))
(defmethod translate [:! :logic] [_ _] "!")
(defmethod translate :default [_ operator] (str operator))

(defn generate
  "Generator of language specific assertions"
  [lang expression]
  (cond
    (not (seq? expression)) (translate lang expression)
    :else (s/join "" (->>
                       (tokenize lang expression)
                       (map #(tokenize lang %))
                       (map #(generate lang %))
                       ))
    )
  )
