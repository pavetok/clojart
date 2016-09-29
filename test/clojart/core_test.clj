(ns clojart.core-test)

(use 'clojure.test)
(use 'clojart.core)

(deftest hierarchy-test
  (is (isa? langs :python :underscore))
  )

(deftest generate-operator-test
  (is (= (generate ::java 'assert) "assert"))
  (is (= (generate ::java 'true) "true"))
  (is (= (generate :python 'true) "True"))
  )

(deftest generate-java-test
  (is (= (generate :java '(assert true)) "assert(true)"))
  (is (= (generate :java '(assert (is-prime 5))) "assert(isPrime(5))"))
  )

(deftest generate-python-test
  (is (= (generate :python '(assert true)) "assert(True)"))
  (is (= (generate :python '(assert (is-prime 5))) "assert(is_prime(5))"))
  (is (= (generate :python '(assert-equal 3 (fib 4))) "assert_equal(3, fib(4))"))
  )

(deftest generate-ruby-test
  (is (= (generate :ruby '(assert true)) "assert(true)"))
  (is (= (generate :ruby '(assert (is-prime 5))) "assert(is_prime(5))"))
  )

(deftest generate-javascript-test
  (is (= (generate :js '(assert true)) "assert(true)"))
  (is (= (generate :js '(assert (is-prime 5))) "assert(isPrime(5))"))
  )
