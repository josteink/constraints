(ns constraints.modules-test
  (:use clojure.test
        constraints.interface constraints.elements constraints.modules))

(def C (make-connector))
(def F (make-connector))

(deftest test-tempconverter
  (testing "Celsius fahrenheit conversion"
    (is (= :done
           (do
             (celsius-fahrenheit-converter C F)
             
             (probe "Celsius temp" C)
             (probe "Fahrenheit temp" F)
             
             (set-value! C 25 :user)
             (forget-value! C :user)
             (set-value! F 212 :user)
             
             :done)))))

