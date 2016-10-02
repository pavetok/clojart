(ns clojart.core
  (:import (clojure.lang PersistentVector PersistentArrayMap Keyword)
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
    :else :function))

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

(defmulti restructure-seq (fn [lang expression] [lang (classify (first expression))]) :hierarchy #'taxonomy)
(defmethod restructure-seq [:any :logic] [_ operator] operator)
(defmethod restructure-seq [:python :logic] [_ expression]
  (let [[operator operand] expression]
    (list operator " " operand)))
(defmethod restructure-seq [:any :infix] [_ expression]
  (let [[operator left right] expression]
    (list left " " operator " " right)))
(defmethod restructure-seq [:simple :variable] [lang expression]
  (let [[_ name value] expression]
    (concat
      (list name " " '= " ")
      (construct lang value))))
(defmethod restructure-seq [:js :variable] [lang expression]
  (let [[_ name value] expression]
    (concat
      (list 'var " " name " " '= " ")
      (construct lang value))))
(defmethod restructure-seq :default [_ expression]
  (let [[operator & args] expression]
    (concat
      (list operator "(")
      (interpose ", " args)
      (list ")"))))

(defn restructure [lang expression]
  (cond
    (not (coll? expression)) expression
    (:restructured (meta expression)) expression
    :else (with-meta (restructure-seq lang expression) {:restructured true})))

(defn underscorize [function]
  (s/replace function "-" "_"))

(defn camelize [function]
  (let [words (s/split (str function) #"-")]
    (s/join "" (cons (s/lower-case (first words)) (map s/capitalize (rest words))))))

(defmulti translate (fn [lang operator] [lang (classify operator)]) :hierarchy #'taxonomy)
(defmethod translate [:underscore :function] [_ operator] (underscorize operator))
(defmethod translate [:camelCase :function] [_ operator] (camelize operator))
(defmethod translate [:python :bool] [_ operator] (s/capitalize operator))
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
                       (restructure lang expression)
                       (map #(restructure lang %))
                       (map #(generate lang %))
                       ))
    )
  )
