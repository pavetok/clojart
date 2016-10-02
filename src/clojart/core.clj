(ns clojart.core
  (:import (clojure.lang PersistentArrayMap Keyword)
           (java.util Collection)))

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

                  (derive :python :dynamic)
                  (derive :ruby :dynamic)
                  (derive :js :dynamic)
                  ))

(def infix #{'+ '-})
(def logic #{'not})

(defn classify [operator]
  (cond
    (some #{operator} infix) :infix
    (instance? Boolean operator) :bool
    (some #{operator} logic) :logic
    (= operator 'def) :variable
    (nil? operator) :nil
    (symbol? operator) :function
    :else operator))

(defn insert [to what at]
  (let [[before after] (split-at at to)]
    (concat before (list what) after)))

(defmulti construct (fn [lang value] [lang (type value)]) :hierarchy #'taxonomy)
(defmethod construct [:dynamic Collection] [lang collection]
  (concat
    (list "[")
    (flatten (interpose ", " (map #(construct lang %) collection)))
    (list "]")))
(defmethod construct [:ruby PersistentArrayMap] [lang dict]
  (let [vals (->> (zipmap
                    (keys dict)
                    (map #(construct lang %) (vals dict)))
                  (seq)
                  (map #(insert % " => " 1)))]
    (concat
      (list "{")
      (flatten (interpose ", " vals))
      (list "}"))))
(defmethod construct [:python PersistentArrayMap] [lang dict]
  (let [vals (->> (zipmap
                    (map #(construct lang %) (keys dict))
                    (map #(construct lang %) (vals dict)))
                  (seq)
                  (map #(insert % ": " 1)))]
    (concat
      (list "{")
      (flatten (interpose ", " vals))
      (list "}"))))
(defmethod construct [:js PersistentArrayMap] [lang dict]
  (let [vals (->> (zipmap
                    (map #(construct lang %) (keys dict))
                    (map #(construct lang %) (vals dict)))
                  (seq)
                  (map #(insert % ": " 1)))]
    (concat
      (list "{")
      (flatten (interpose ", " vals))
      (list "}"))))
(defmethod construct [:any String] [_ string] (list \' string \'))
(defmethod construct [:any Keyword] [_ keyword] (list \' (name keyword) \'))
(defmethod construct [:ruby Keyword] [_ keyword] (list keyword))
(defmethod construct :default [_ value] (list value))

(defmulti structurize-expression (fn [lang expression] [lang (classify (first expression))]) :hierarchy #'taxonomy)
(defmethod structurize-expression [:any :logic] [_ operator] operator)
(defmethod structurize-expression [:python :logic] [_ expression]
  (let [[operator operand] expression]
    (list operator " " operand)))
(defmethod structurize-expression [:any :infix] [_ expression]
  (let [[operator left right] expression]
    (list left " " operator " " right)))
(defmethod structurize-expression [:simple :variable] [lang expression]
  (let [[_ name value] expression]
    (concat
      (list name " " '= " ")
      (construct lang value))))
(defmethod structurize-expression [:js :variable] [lang expression]
  (let [[_ name value] expression]
    (concat
      (list 'var " " name " " '= " ")
      (construct lang value))))
(defmethod structurize-expression [:any :function] [_ expression]
  (let [[operator & args] expression]
    (concat
      (list operator "(")
      (interpose ", " args)
      (list ")"))))
(defmethod structurize-expression :default [_ operator] operator)

(defn structurize [lang expression]
  (cond
    (not (coll? expression)) expression
    (:restructured (meta expression)) expression
    :else (with-meta (structurize-expression lang expression) {:restructured true})))

(defn underscorize [function]
  (s/replace function "-" "_"))

(defn camelize [function]
  (let [words (s/split (str function) #"-")]
    (s/join "" (cons (s/lower-case (first words)) (map s/capitalize (rest words))))))

(defmulti translate (fn [lang operator] [lang (classify operator)]) :hierarchy #'taxonomy)
(defmethod translate [:underscore :function] [_ name] (underscorize name))
(defmethod translate [:camelCase :function] [_ name] (camelize name))
(defmethod translate [:python :bool] [_ bool] (s/capitalize bool))
(defmethod translate [:python :nil] [_ _] "None")
(defmethod translate [:ruby :nil] [_ _] "nil")
(defmethod translate [:js :nil] [_ _] "null")
(defmethod translate [:! :logic] [_ _] "!")
(defmethod translate :default [_ operator] (str operator))

(defn generate
  "Generator of language specific expressions"
  [lang expression]
  (cond
    (not (coll? expression)) (translate lang expression)
    :else (s/join "" (->>
                       (structurize lang expression)
                       (map #(structurize lang %))
                       (map #(generate lang %))
                       ))
    )
  )
