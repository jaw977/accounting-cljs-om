(ns accounting.util
  (:require [clojure.string :as str]))
  
(defn assoc-last [xs ks x]
  (assoc-in xs (into [(dec (count xs))] ks) x))

(defn update-last [xs ks f & args]
  (apply update-in xs (into [(dec (count xs))] ks) f args))

(defn ucfirst [s] 
  (str (.toUpperCase (first s)) (subs s 1)))

(defn log [x]
  (.log js/console x))
  
(defn log-clj [x]
  (log (clj->js x)))

(defn str->fixpt [s]
  (* s 100))
  
(defn fixpt->str [n]
  (.toFixed (/ n 100) 2))
  
(defn account-key->vec [account]
  (str/split (subs (str account) 1) "-"))

(defn account-vec->str [account]
  (str/join "." (map ucfirst account)))

(def account-key->str (comp account-vec->str account-key->vec))

(defn str->account [account]
  (keyword (str/join "-" (map #(.toLowerCase %) (str/split account ".")))))

(defn add-leading-zero [n]
  (let [s (str n)]
    (if (= 1 (count s))
      (str "0" s)
      s)))
      
(defn today []
  (let [d (js/Date.)]
    (str (.getFullYear d) "/"
         (add-leading-zero (inc (.getMonth d))) "/"
         (add-leading-zero (.getDate d)))))
      
(defn date->int [date]
  (if (number? date)
    date
    (js/parseInt (.replace date (js/RegExp. "[^0-9]" "g") ""))))

