(ns clojart.core-test)

(use 'clojure.test)
(use 'clojart.core)

(deftest classify-test
  (is (= (classify 'is-prime) :function))
  (is (= (classify 'assert) 'assert))
  (is (= (classify '+) :infix))
  )

(deftest transformation-test
  (is (= (camelize 'is-prime) "isPrime"))
  (is (= (underscorize 'is-prime) "is_prime"))
  )

(deftest rearrange-test
  (is (= (rearrange :python '(+ 1 2)) '(1 + 2)))
  (is (= (rearrange :python '(- 1 2)) '(1 - 2)))
  )

(deftest enrich-test
  (is (= (enrich :python '(+ 1 2)) '(:ob + 1 2 :cb)))
  (is (= (enrich :python '(assert true)) '(assert true)))
  )

(deftest translate-test
  (is (= (translate :java 'is-prime) "isPrime"))
  (is (= (translate :ruby 'is-prime) "is_prime"))
  (is (= (translate :java 'assert) 'assert))
  (is (= (translate :python 'true) 'True))
  )

(deftest generate-java-test
  (is (= (generate :java '(assert true)) "assert(true)"))
  (is (= (generate :java '(assert (is-prime 5))) "assert(isPrime(5))"))
  (is (= (generate :java '(assert (+ 1 2))) "assert(1 + 2)"))
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
