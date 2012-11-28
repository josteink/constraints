(ns constraints.modules
  (:use constraints.elements))
;  (:use constraints.interface constraints.elements))

(defn celsius-fahrenheit-converter
  "A celsius-fahrenheit converter constraint-network"
   [c f]
	(let [u (make-connector)
		  v (make-connector)
		  w (make-connector)
		  x (make-connector)
		  y (make-connector)]
		(multiplier c w u)
		(multiplier v x u)
		(adder v y f)
		(constant 9 w)
		(constant 5 x)
		(constant 32 y)
		:ok))
