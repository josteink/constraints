(ns constraints.elements-test
  (:use clojure.test
        constraints.interface constraints.elements))

(deftest test-divider
  (testing "Divider element"
    (is (= :done
           (do
             (def d1 (make-connector))
             (def d2 (make-connector))
             (def result (make-connector))
             
             (divider d1 d2 result)
             
             (probe "D1" d1)
             (probe "D2" d2)
             (probe "Result" result)
             
             (constant 10 d2)
             
             (set-value! d1 100 :user)  
             (forget-value! d1 :user)
             
             (set-value! result 5 :user)  
             (forget-value! result :user)
             
             :done)))))