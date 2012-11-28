(ns constraints.interface)

; utility

(defn in? 
  "true if seq contains elm"
  [seq elm]  
  (if (some #(= elm %) seq) true false))

(defn error [message]
	(throw (Exception. message)))

(defn for-each-except [exception procedure everyone]
	(let[iter (fn [items]
		(cond (or (nil? items)
		          (= 0 (count items)))
		          :done
			  (= (first items) exception) (recur (rest items))
			  :else (do
			  	(procedure (first items))
			  	(recur (rest items)))))]
		(iter everyone)))

(defn dmap [functions items]
  (let [dmap-iter (fn [remaining-functions results]
                    (if (empty? remaining-functions)
                      results
                      (recur (rest remaining-functions)
                             (conj results (map (first remaining-functions) items)))))]
    (dmap-iter functions [])))

(defn message-safely [target message & args]
  (if (not (nil? target))
    (let [func (message target)]
      (if (not (nil? func))
        (apply func args)
        (println (str "Message has no endpoint! Target = " (:type target)))))
    (println (str "No target provided for message " message))))

; interface

(defn has-value? [connector]
	((:has-value? connector)))
(defn get-value [connector]
	((:value connector)))
(defn set-value! [connector new-value informant]
	((:set-value! connector) new-value informant))
(defn forget-value! [connector retractor]
  (message-safely connector :forget-value! retractor))
(defn connect [connector new-constraint]
  (message-safely connector :connect new-constraint))

; more interface

(defn inform-about-value [constraint]
  (message-safely constraint :i-have-a-value))

(defn inform-about-no-value [constraint]
  (message-safely constraint :i-lost-my-value))
