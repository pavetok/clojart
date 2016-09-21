(ns clojart.core-test)

(use 'clojure.test)
(use 'clojart.core)

(deftest translate-test
  (is (= (translate :java 'assert) "assert"))
  (is (= (translate :java 'true) "true"))
  (is (= (translate :python 'true) "True"))
  )

(deftest generate-java-test
  (is (= (generate :java '(assert true)) "assert(true)"))
  (is (= (generate :java '(assert (is-prime 5))) "assert(isPrime(5))"))
  (is (= (generate :java '(assert (not false))) "assert(!false)"))
  )

(deftest generate-python-test
  (is (= (generate :python '(assert true)) "assert(True)"))
  (is (= (generate :python '(assert (is-prime 5))) "assert(is_prime(5))"))
  )

(deftest generate-ruby-test
  (is (= (generate :ruby '(assert true)) "assert(true)"))
  (is (= (generate :ruby '(assert (is-prime 5))) "assert(is_prime(5))"))
  )

(deftest generate-ruby-test
  (is (= (generate :js '(assert true)) "assert(true)"))
  (is (= (generate :js '(assert (is-prime 5))) "assert(isPrime(5))"))
  )
