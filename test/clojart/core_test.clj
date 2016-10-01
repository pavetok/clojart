(ns clojart.core-test)

(use 'clojure.test)
(use 'clojart.core)

(deftest classify-test
  (is (= (classify 'is-prime) :function))
  (is (= (classify 'assert) :function))
  (is (= (classify '+) :infix))
  )

(deftest transformation-test
  (is (= (camelize 'is-prime) "isPrime"))
  (is (= (underscorize 'is-prime) "is_prime"))
  )

(deftest tokenize-test
  (is (= (tokenize :any '(assert true)) '(assert "(" true ")")))
  (is (= (tokenize :any (tokenize :any '(assert true))) '(assert "(" true ")")))
  (is (= (tokenize :any '(assert-equal 3 (fib 4))) '(assert-equal "(" 3 ", " (fib 4) ")")))
  (is (= (tokenize :any '(+ 1 2)) '(1 " " + " " 2)))
  (is (= (tokenize :any '(is-prime 5)) '(is-prime "(" 5 ")")))
  (is (= (tokenize :any 5) 5))
  (is (= (tokenize :any '(not true)) '(not true)))
  )

(deftest translate-test
  (is (= (translate :java 'is-prime) "isPrime"))
  (is (= (translate :ruby 'is-prime) "is_prime"))
  (is (= (translate :any 'assert) "assert"))
  (is (= (translate :any \() "("))
  (is (= (translate :any \,) ","))
  (is (= (translate :any \space) " "))
  (is (= (translate :java true) "true"))
  (is (= (translate :python true) "True"))
  )

(deftest generate-java-test
  (is (= (generate :java '(assert true)) "assert(true)"))
  (is (= (generate :java '(is-prime 5)) "isPrime(5)"))
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
  (is (= (generate :ruby '(assert-equal 3 (fib 4))) "assert_equal(3, fib(4))"))
  )

(deftest generate-javascript-test
  (is (= (generate :js '(assert true)) "assert(true)"))
  (is (= (generate :js '(assert (is-prime 5))) "assert(isPrime(5))"))
  (is (= (generate :js '(assert-equal 3 (fib 4))) "assertEqual(3, fib(4))"))
  )
