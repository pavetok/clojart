(ns reflector.core-test)
(use 'clojure.test)
(use 'reflector.core)

(deftest generate-test
  (is (= (generate '(assert true)) "assert(true)"))
  (is (= (generate '(assert (is-prime 5))) "assert(isPrime(5))"))
  )
