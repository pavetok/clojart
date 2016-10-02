(ns clojart.core-test)

(use 'clojure.test)
(use 'clojart.core)

(deftest classify-test
  (is (= (classify 'is-prime) :function))
  (is (= (classify 'assert) :function))
  (is (= (classify '+) :infix))
  (is (= (classify 'def) :variable))
  )

(deftest transform-test
  (is (= (camelize 'is-prime) "isPrime"))
  (is (= (underscorize 'is-prime) "is_prime"))
  )

(deftest construct-test
  (is (= (construct :simple [1]) '("[" 1 "]")))
  (is (= (construct :simple ["a"]) '("[" "a" "]")))
  (is (= (construct :simple [1 2]) '("[" 1 ", " 2 "]")))
  (is (= (construct :ruby {:k 1}) '("{" :k " => " 1 "}")))
  (is (= (construct :ruby {:k "v"}) '("{" :k " => " \' "v" \' "}")))
  (is (= (construct :ruby {:a 1 :b 2}) '("{" :a " => " 1 ", " :b " => " 2 "}")))
  )

(deftest restructure-test
  (is (= (restructure :any '(assert true)) '(assert "(" true ")")))
  (is (= (restructure :any (restructure :any '(assert true))) '(assert "(" true ")")))
  (is (= (restructure :any '(assert-equal 3 (fib 4))) '(assert-equal "(" 3 ", " (fib 4) ")")))
  (is (= (restructure :any '(+ 1 2)) '(1 " " + " " 2)))
  (is (= (restructure :any '(is-prime 5)) '(is-prime "(" 5 ")")))
  (is (= (restructure :any 5) 5))
  (is (= (restructure :any '(not true)) '(not true)))
  (is (= (restructure :python '(def my-var 1)) '(my-var " " = " " 1)))
  (is (= (restructure :python '(def my-var "a")) '(my-var " " = " " \' "a" \')))
  (is (= (restructure :python '(def my-var [1 "a"])) '(my-var " " = " " "[" 1 ", " \' "a" \' "]")))
  )

(deftest translate-test
  (is (= (translate :java 'is-prime) "isPrime"))
  (is (= (translate :ruby 'is-prime) "is_prime"))
  (is (= (translate :any 'assert) "assert"))
  (is (= (translate :java true) "true"))
  (is (= (translate :python true) "True"))
  (is (= (translate :python false) "False"))
  )

(deftest generate-java-test
  (is (= (generate :java '(assert true)) "assert(true)"))
  (is (= (generate :java '(is-prime 5)) "isPrime(5)"))
  (is (= (generate :java '(assert (is-prime 5))) "assert(isPrime(5))"))
  (is (= (generate :java '(assert (+ 1 2))) "assert(1 + 2)"))
  (is (= (generate :java '(assert (not false))) "assert(!false)"))
  )

(deftest generate-python-test
  (is (= (generate :python '(assert true)) "assert(True)"))
  (is (= (generate :python '(assert (is-prime 5))) "assert(is_prime(5))"))
  (is (= (generate :python '(assert-equal 3 (fib 4))) "assert_equal(3, fib(4))"))
  (is (= (generate :python '(assert (not false))) "assert(not False)"))
  (is (= (generate :python '(def my-var 1)) "my_var = 1"))
  (is (= (generate :python '(def my-var [1 "a"])) "my_var = [1, 'a']"))
  )

(deftest generate-ruby-test
  (is (= (generate :ruby '(assert true)) "assert(true)"))
  (is (= (generate :ruby '(assert (is-prime 5))) "assert(is_prime(5))"))
  (is (= (generate :ruby '(assert-equal 3 (fib 4))) "assert_equal(3, fib(4))"))
  (is (= (generate :ruby '(assert (not false))) "assert(!false)"))
  (is (= (generate :ruby '(def my-var 1)) "my_var = 1"))
  (is (= (generate :ruby '(def my-var [1 "a"])) "my_var = [1, 'a']"))
  (is (= (generate :ruby '(def my-var {:a 1, :b "b"})) "my_var = {:a => 1, :b => 'b'}"))
  )

(deftest generate-javascript-test
  (is (= (generate :js '(assert true)) "assert(true)"))
  (is (= (generate :js '(assert (is-prime 5))) "assert(isPrime(5))"))
  (is (= (generate :js '(assert-equal 3 (fib 4))) "assertEqual(3, fib(4))"))
  (is (= (generate :js '(assert (not false))) "assert(!false)"))
  (is (= (generate :js '(def my-var 1)) "var myVar = 1"))
  (is (= (generate :ruby '(def my-var [1 "a"])) "my_var = [1, 'a']"))
  )
