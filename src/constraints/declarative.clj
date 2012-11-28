(ns constraints.declarative
	(:use constraints.elements))

(defn create-for-component [component port1 port2]
	(let [output-port (make-connector)]
		(component port1 port2 output-port)
		output-port))

(defn c+ [a1 a2]
	(create-for-component adder a1 a2))

(defn c- [a1 a2]
	(create-for-component subtractor a1 a2))

(defn c* [m1 m2]
	(create-for-component multiplier m1 m2))

(defn cDiv [d1 d2]
	(create-for-component divider d1 d2))

(defn cC [value]
  	(let [wire (make-connector)]
     	(constant [value wire])
     	wire))

(defn c= [port1 port2]
  ; TODO: create passthrough component in elements!
  :done)

