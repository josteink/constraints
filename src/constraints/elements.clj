(ns constraints.elements
	(:use constraints.interface))

; implementations

(defn adder [a1 a2 sum]
  (let [me (atom nil)
        dispatcher {
                    :type "Adder"
                    :i-have-a-value
                    (fn []
                      (let [[[hv1 hv2 hvs][v1 v2 vs]] (dmap [has-value? get-value] [a1 a2 sum])]
                        (cond
                          (and hv1 hv2) (set-value! sum (+ v1 v2) @me)
                          (and hv1 hvs) (set-value! a2 (- vs v1) @me)
                          (and hv2 hvs) (set-value! a1 (- vs v2) @me))))
                    :i-lost-my-value
                    (fn []
                      (forget-value! sum @me)
                      (forget-value! a1 @me)
                      (forget-value! a2 @me))
                    }]
    (reset! me dispatcher)
    (connect a1 @me)
    (connect a2 @me)
    (connect sum @me)
    dispatcher))

(defn subtractor [a1 a2 sum]
  (let [me (atom nil)
        dispatcher {
                    :type "Subtractor"
                    :i-have-a-value
                    (fn []
                      (let [[[hv1 hv2 hvs][v1 v2 vs]] (dmap [has-value? get-value] [a1 a2 sum])]
                        (cond
                          (and hv1 hv2) (set-value! sum (- v1 v2) @me)
                          (and hv1 hvs) (set-value! a2 (- a2 vs) @me)
                          (and hv2 hvs) (set-value! a1 (+ vs v2) @me))))
                    :i-lost-my-value
                    (fn []
                      (forget-value! sum @me)
                      (forget-value! a1 @me)
                      (forget-value! a2 @me))
                    }]
    (reset! me dispatcher)
    (connect a1 @me)
    (connect a2 @me)
    (connect sum @me)
    dispatcher))

(defn multiplier [m1 m2 product]
  (let [me (atom nil)
        dispatcher {
                    :type "Multiplier"
                    :i-have-a-value
                    (fn []
                      (let [[[hv1 hv2 hvp][v1 v2 vp]] (dmap [has-value? get-value] [m1 m2 product])]
                        (cond
                          (or (and hv1 (= 0 v1)) (and hv2 (= 0 v2)))
                          	(set-value! product 0 @me)
                          (and hv1 hv2) (set-value! product (* v1 v2) @me)
                          (and hv1 hvp) (set-value! m2 (/ vp v1) @me)
                          (and hv2 hvp) (set-value! m1 (/ vp v2) @me))))
                    :i-lost-my-value
                    (fn []
                      (forget-value! product @me)
                      (forget-value! m1 @me)
                      (forget-value! m2 @me))
                    }]
    (reset! me dispatcher)
    (connect m1 @me)
    (connect m2 @me)
    (connect product @me)
    dispatcher))

(defn divider [d1 d2 result]
	(let [me (atom nil)
		  dispatcher {
			:type "Divider"
			:i-have-a-value (fn []
                     (let [[[hv1 hv2 hvr] [v1 v2 vr]] (dmap [has-value? get-value] [d1 d2 result])]
						(cond (and hv1 (= 0 v1)) (set-value! result 0 @me)
            				  (and hvr (= 0 vr)) (set-value! d1 0 @me)
							  (and hv1 hv2) (set-value! result (/ v1 v2) @me)
							  (and hv1 hvr) (set-value! d2 (/ v1 vr) @me)
							  (and hv2 hvr) (set-value! d1 (* vr v2) @me))))
			:i-lost-my-value (fn []
				(forget-value! result @me)
				(forget-value! d1 @me)
				(forget-value! d2 @me))
		}]
		(reset! me dispatcher)
		(connect d1 @me)
		(connect d2 @me)
		(connect result @me)
		dispatcher))

(defn constant [value connector]
	(let [me (atom nil)
		  dispatcher {
			:type "Constant"
			:identity (fn [] @me)
			:i-have-a-value (fn [] :ignored)
			:i-lost-my-value (fn [] :ignored)
		}]
		(reset! me dispatcher)
		(connect connector @me)
		(set-value! connector value @me)
		dispatcher))

(defn passthrough [p1 p2]
  (let [me (atom nil)
        dispatcher {
                    :type "Passthrough"
                    :i-have-a-value
                    (fn []
                      (let [[[hv1 hv2] [v1 v2]] (dmap [has-value? get-value] [p1 p2])]
                        (cond
                          hv1 (set-value! p2 v1 @me)
                          hv2 (set-value! p1 v2 @me))))
                    :i-lost-my-value (fn []
                                       (forget-value! p1 @me)
                                       (forget-value! p2 @me))
                    }]
    (reset! me dispatcher)
    (connect p1 @me)
    (connect p2 @me)
    dispatcher))

(defn probe [name connector]
	(let [print-probe (fn [value] (println (str "Probe: " name " = " value)))
	      me (atom nil)
		  dispatcher {
		  	:type "Probe"
		  	:i-have-a-value (fn [] (print-probe (get-value connector)))
		  	:i-lost-my-value (fn [] (print-probe "?"))
	  	}]
		(reset! me dispatcher)
		(connect connector @me)
		dispatcher))

(defn make-connector []
	(let [; object data
		  value (atom false)
		  informant (atom false)
		  constraints (atom '())

		  ; object-itself
		  me (atom nil)
		  dispatcher {
			:has-value? (fn []
				(if @informant true false))
			:value (fn [] @value)
			:set-value! (fn [new-value setter]
				(cond
					(not (has-value? @me))
					(do
						(reset! value new-value)
						(reset! informant setter)
						(for-each-except setter inform-about-value @constraints))
					(not (= @value new-value))
					(error (str "Contradiction - " @value " - " new-value))
					:else :ignored))
			:forget-value! (fn [retractor]
				(if (= retractor @informant)
					(do
						(reset! informant nil)
						(for-each-except retractor inform-about-no-value @constraints))
					:ignored))
			:connect (fn [new-constraint]
				(if (not (in? @constraints new-constraint))
						(swap! constraints conj new-constraint))
				(if (has-value? @me)
						(inform-about-value new-constraint))
				:done)
		  }]
		(reset! me dispatcher)
		dispatcher))


