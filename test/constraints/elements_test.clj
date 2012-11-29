(ns constraints.elements-test
  (:use clojure.test
        constraints.interface constraints.elements))

; utility-functions

(defn get-connectors [num]
  (let [get-connectors-impl
        (fn [num-remaining items]
          (if  (= 0 num-remaining)
            items
            (recur (- num-remaining 1) (cons (make-connector) items))))]
  (get-connectors-impl num [])))
    
(defn work-ports [port1 port2]
  (set-value! port1 100 :user)  
  (forget-value! port1 :user)
  
  (set-value! port2 5 :user)  
  (forget-value! port2 :user)
  
  :done)

(defn do-3port-test [component]
  (let [[p1 p2 result] (get-connectors 3)]
    (component p1 p2 result)
    
    (probe "P1" p1)
    (probe "P2" p2)
    (probe "Result" result)
    (constant 10 p2)
    
    (work-ports p1 result)))

; self-tests

(deftest get-connectors-test
  (testing "Get connectors self-test"
    (is (= 3
        (count (get-connectors 3))))))

; elements-not-crashing tests

(deftest test-adder
  (testing "Adder element"
    (is (= :done (do-3port-test adder)))))

(deftest test-subtractor
  (testing "Subtractor element"
    (is (= :done (do-3port-test subtractor)))))

(deftest test-multiplier
  (testing "Multiplier element"
    (is (= :done (do-3port-test multiplier)))))

(deftest test-divider
  (testing "Divider element"
    (is (= :done (do-3port-test divider)))))
