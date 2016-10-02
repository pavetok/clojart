(ns clojart.core
  (:import (clojure.lang PersistentArrayMap Keyword)
           (java.util Collection)))

(require '[clojure.string :as s])

(def taxonomy (-> (make-hierarchy)
                  (derive :python :any)
                  (derive :ruby :any)
                  (derive :java :any)
                  (derive :javascript :any)

                  (derive :python :underscore-like)
                  (derive :ruby :underscore-like)
                  (derive :java :camel-case-like)
                  (derive :javascript :camel-case-like)

                  (derive :javascript :exclamation-like)
                  (derive :java :exclamation-like)
                  (derive :ruby :exclamation-like)

                  (derive :python :simple)
                  (derive :ruby :simple)

                  (derive :python :dynamic)
                  (derive :ruby :dynamic)
                  (derive :javascript :dynamic)

                  (derive :javascript :semicolon-like)
                  (derive :java :semicolon-like)
                  (derive :ruby :newline-like)
                  (derive :python :newline-like)
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

(defmulti structurize-data (fn [lang value] [lang (type value)]) :hierarchy #'taxonomy)
(defmethod structurize-data [:dynamic Collection] [lang collection]
  (concat
    (list \[)
    (flatten (interpose ", " (map #(structurize-data lang %) collection)))
    (list \])))
(defmethod structurize-data [:ruby PersistentArrayMap] [lang dict]
  (let [vals (->> (zipmap
                    (keys dict)
                    (map #(structurize-data lang %) (vals dict)))
                  (seq)
                  (map #(insert % " => " 1)))]
    (concat
      (list \{)
      (flatten (interpose ", " vals))
      (list \}))))
(defmethod structurize-data [:python PersistentArrayMap] [lang dict]
  (let [vals (->> (zipmap
                    (map #(structurize-data lang %) (keys dict))
                    (map #(structurize-data lang %) (vals dict)))
                  (seq)
                  (map #(insert % ": " 1)))]
    (concat
      (list \{)
      (flatten (interpose ", " vals))
      (list \}))))
(defmethod structurize-data [:javascript PersistentArrayMap] [lang dict]
  (let [vals (->> (zipmap
                    (map #(structurize-data lang %) (keys dict))
                    (map #(structurize-data lang %) (vals dict)))
                  (seq)
                  (map #(insert % ": " 1)))]
    (concat
      (list \{)
      (flatten (interpose ", " vals))
      (list \}))))
(defmethod structurize-data [:any String] [_ string] (list \' string \'))
(defmethod structurize-data [:any Keyword] [_ keyword] (list \' (name keyword) \'))
(defmethod structurize-data [:ruby Keyword] [_ keyword] (list keyword))
(defmethod structurize-data :default [_ value] (list value))

(defmulti structurize-expression (fn [lang expression] [lang (classify (first expression))]) :hierarchy #'taxonomy)
(defmethod structurize-expression [:any :logic] [_ expression] expression)
(defmethod structurize-expression [:python :logic] [_ expression]
  (let [[operator operand] expression]
    (list operator \space operand)))
(defmethod structurize-expression [:any :infix] [_ expression]
  (let [[operator left right] expression]
    (list left \space operator \space right)))
(defmethod structurize-expression [:simple :variable] [lang expression]
  (let [[_ name value] expression]
    (concat
      (list name \space '= \space)
      (structurize-data lang value))))
(defmethod structurize-expression [:javascript :variable] [lang expression]
  (let [[_ name value] expression]
    (concat
      (list 'var \space name \space '= \space)
      (structurize-data lang value))))
(defmethod structurize-expression [:any :function] [_ expression]
  (let [[operator & args] expression]
    (concat
      (list operator \()
      (interpose ", " args)
      (list \)))))
(defmethod structurize-expression :default [_ expression] expression)

(defn structurize [lang expression]
  (cond
    (not (coll? expression)) expression
    (:structurized (meta expression)) expression
    :else (with-meta (structurize-expression lang expression) {:structurized true})))

(defn underscorize [function]
  (s/replace function "-" "_"))

(defn camelize [function]
  (let [words (s/split (str function) #"-")]
    (s/join "" (cons (s/lower-case (first words)) (map s/capitalize (rest words))))))

(defmulti translate (fn [lang operator] [lang (classify operator)]) :hierarchy #'taxonomy)
(defmethod translate [:underscore-like :function] [_ name] (underscorize name))
(defmethod translate [:camel-case-like :function] [_ name] (camelize name))
(defmethod translate [:python :bool] [_ bool] (s/capitalize bool))
(defmethod translate [:python :nil] [_ _] "None")
(defmethod translate [:ruby :nil] [_ _] "nil")
(defmethod translate [:javascript :nil] [_ _] "null")
(defmethod translate [:exclamation-like :logic] [_ _] "!")
(defmethod translate [:semicolon-like :separator] [_ _] ";")
(defmethod translate [:newline-like :separator] [_ _] "")
(defmethod translate :default [_ operator] (str operator))

(defn generate
  ([first? lang expression]
   (cond
     (not (coll? expression)) (translate lang expression)
     :else (s/join "" (concat (->>
                                (structurize lang expression)
                                (map #(structurize lang %))
                                (map #(generate false lang %)))
                              (when first?
                                (list (translate lang :separator)))))
     ))
  ([lang expression] (generate true lang expression))
  )
