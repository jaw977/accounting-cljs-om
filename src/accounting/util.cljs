(ns accounting.util
  (:require [clojure.string :as str]))
  
(defn assoc-last [xs ks x]
  (assoc-in xs (into [(dec (count xs))] ks) x))

(defn update-last [xs ks f & args]
  (apply update-in xs (into [(dec (count xs))] ks) f args))

(defn log [x]
  (.log js/console x))
  
(defn log-clj [x]
  (log (clj->js x)))

(defn str->fixpt [s]
  (* 100
     (if (string? s)
       (str/replace s #"[^-.0-9]" "")
       s)))
  
(defn fixpt->float [n]
  (/ n 100))

(defn fixpt->str [n]
  (.toFixed (fixpt->float n) 2))
  
(defn account-key->vec [account]
  (str/split (subs (str account) 1) "-"))

(defn account-vec->str [account]
  (str/join "." (map str/capitalize account)))

(def account-key->str (comp account-vec->str account-key->vec))

(defn str->account [account]
  (keyword (str/join "-" (map str/lower-case (str/split account ".")))))

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
    (js/parseInt (str/replace date #"[^0-9]" ""))))

(defn balance-amount [parts]
  (- (apply + (map (comp str->fixpt :amount) parts))))


; The MIT License (MIT)
;
; Copyright (c) 2015 Jason Waag
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
; THE SOFTWARE.

