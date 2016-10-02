(ns clojart.core-test)

(use 'clojure.test)
(use 'clojart.core)

(deftest classify-test
  (are [expected actual] (= expected (classify actual))
                         :function 'is-prime
                         :function 'assert
                         :infix '+
                         :variable 'def))

(deftest transform-test
  (is (= (camelize 'is-prime) "isPrime"))
  (is (= (underscorize 'is-prime) "is_prime"))
  )

(deftest construct-test
  (is (= (structurize-data :dynamic [1]) '(\[ 1 \])))
  (is (= (structurize-data :dynamic [1 [2]]) '(\[ 1 ", " \[ 2 \] \])))
  (is (= (structurize-data :dynamic ["a"]) '(\[ "a" \])))
  (is (= (structurize-data :dynamic [1 2]) '(\[ 1 ", " 2 \])))
  (is (= (structurize-data :ruby {:k 1}) '(\{ :k " => " 1 \})))
  (is (= (structurize-data :ruby {:k {:inner 1}}) '(\{ :k " => " \{ :inner " => " 1 \} \})))
  (is (= (structurize-data :ruby {:k "v"}) '(\{ :k " => " \' "v" \' \})))
  (is (= (structurize-data :ruby {:a 1, :b "b"}) '(\{ :a " => " 1 ", " :b " => " \' "b" \' \})))
  )

(deftest structurize-test
  (is (= (structurize :any '(assert true)) '(assert \( true \))))
  (is (= (structurize :any (structurize :any '(assert true))) '(assert \( true \))))
  (is (= (structurize :any '(assert-equal 3 (fib 4))) '(assert-equal \( 3 ", " (fib 4) \))))
  (is (= (structurize :any '(+ 1 2)) '(1 \space + \space 2)))
  (is (= (structurize :any '(is-prime 5)) '(is-prime \( 5 \))))
  (is (= (structurize :any 5) 5))
  (is (= (structurize :any '(not true)) '(not true)))
  (is (= (structurize :python '(def my-var 1)) '(my-var \space = \space 1)))
  (is (= (structurize :python '(def my-var "a")) '(my-var \space = \space \' "a" \')))
  (is (= (structurize :python '(def my-var [1 "a"])) '(my-var \space = \space \[ 1 ", " \' "a" \' \])))
  )

(deftest translate-test
  (is (= (translate :java 'is-prime) "isPrime"))
  (is (= (translate :ruby 'is-prime) "is_prime"))
  (is (= (translate :any 'assert) "assert"))
  (is (= (translate :java true) "true"))
  (is (= (translate :python true) "True"))
  (is (= (translate :python false) "False"))
  (is (= (translate :python nil) "None"))
  (is (= (translate :ruby nil) "nil"))
  (is (= (translate :javascript nil) "null"))
  )

(deftest java-test
  (are [java clojure] (= java (generate :java clojure))
                      "assert(true);" '(assert true)
                      "isPrime(5);" '(is-prime 5)
                      "assert(1 + 2);" '(assert (+ 1 2))
                      "assert(isPrime(5));" '(assert (is-prime 5))
                      "assertEqual(3, fib(4));" '(assert-equal 3 (fib 4))
                      "assert(!false);" '(assert (not false))))

(deftest ruby-test
  (are [ruby clojure] (= ruby (generate :ruby clojure))
                      "assert(true)" '(assert true)
                      "assert(is_prime(5))" '(assert (is-prime 5))
                      "my_var = 1" '(def my-var 1)
                      "x = :a" '(def x :a)
                      "my_var = [1, 'a', nil]" '(def my-var [1 "a" nil])
                      "my_var = {1 => 1, :b => 'b'}" '(def my-var {1 1, :b "b"})
                      "my_var = ['1', 2, [:inner, 'ha']]" '(def my-var ["1" 2 [:inner "ha"]])
                      "their_var = ['1', 2, [:inner, 'he', ['innermost', 3]]]" '(def their-var ("1" 2 [:inner "he" ("innermost" 3)]))
                      "your_var = [1, '2', nil, [nil, [nil]]]" '(def your-var (1 "2" nil (nil [nil])))
                      "x = {:a => 3, :b => 'u', :inner => {:key => :value}}" '(def x {:a 3, :b "u", :inner {:key :value}})
                      "assert_equal(3, fib(4))" '(assert-equal 3 (fib 4))
                      "assert(!false)" '(assert (not false))))

(deftest javascript-test
  (are [javascript clojure] (= javascript (generate :javascript clojure))
                            "assert(true);" '(assert true)
                            "assert(isPrime(5));" '(assert (is-prime 5))
                            "assert(!false);" '(assert (not false))
                            "var myVar = 1;" '(def my-var 1)
                            "var myVar = [1, 'a', null];" '(def my-var [1 "a" nil])
                            "var myVar = {1: 1, 'b': 'b'};" '(def my-var {1 1, :b "b"})
                            "var myVar = ['1', 2, ['inner', 'ha']];" '(def my-var ["1" 2 [:inner "ha"]])
                            "var theirVar = ['1', 2, ['inner', 'he', ['innermost', 3]]];" '(def their-var ("1" 2 [:inner "he" ("innermost" 3)]))
                            "var yourVar = [1, '2', null, [null, [null]]];" '(def your-var (1 "2" nil (nil [nil])))
                            "var x = {'a': 3, 'b': 'u', 'inner': {'key': 'value'}};" '(def x {:a 3, :b :u, :inner {:key :value}})
                            "assertEqual(3, fib(4));" '(assert-equal 3 (fib 4))
                            "assert(!false);" '(assert (not false))))

(deftest python-test
  (are [python clojure] (= python (generate :python clojure))
                        "assert(True)" '(assert true)
                        "assert(is_prime(5))" '(assert (is-prime 5))
                        "my_var = 1" '(def my-var 1)
                        "my_var = [1, 'a', None]" '(def my-var [1 "a" nil])
                        "my_var = {1: 1, 'b': 'b'}" '(def my-var {1 1, :b "b"})
                        "my_var = ['1', 2, ['inner', 'ha']]" '(def my-var ["1" 2 [:inner "ha"]])
                        "their_var = ['1', 2, ['inner', 'he', ['innermost', 3]]]" '(def their-var ("1" 2 [:inner "he" ("innermost" 3)]))
                        "your_var = [1, '2', None, [None, [None]]]" '(def your-var (1 "2" nil (nil [nil])))
                        "x = {'a': 3, 'b': 'u', 'inner': {'key': 'value'}}" '(def x {:a 3, :b "u", :inner {:key :value}})
                        "assert_equal(3, fib(4))" '(assert-equal 3 (fib 4))
                        "assert(not False)" '(assert (not false))))
