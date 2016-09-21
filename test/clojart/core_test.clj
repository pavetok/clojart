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
  )

(deftest generate-python-test
  (is (= (generate :python '(assert true)) "assert True"))
  (is (= (generate :python '(assert (is-prime 5))) "assert is_prime(5)"))
  )
