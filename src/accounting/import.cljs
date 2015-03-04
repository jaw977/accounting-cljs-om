(ns accounting.import
  (:require [cljs.reader]
            [accounting.util :refer [update-last log log-clj str->fixpt date->int]]))

(defn read-transaction-element [x]
  (cond
    (number? x) {:amount (str->fixpt x)}
    (string? x) {:note x}
    (keyword? x) {:account x}
    (vector? x) {:amount (str->fixpt (nth x 0)) :unit (nth x 1)}
    :else {}))
        
(defn update-parts [parts m]
  (let [last-part (peek parts)]
    (cond
      (:account m) (conj parts m)
      (:note m) (update-last parts [] merge m)
      (:amount last-part) (conj parts (merge m {:account (:account last-part)}))
      :else (update-last parts [] merge m))))

(defn normalize-transaction [[date desc & tx]]
  (loop [[x & xs] tx
         balance-amounts {}
         parts []]
    (if x
      (let [{:keys [amount unit] :or {amount 0} :as m} (read-transaction-element x)]
        (recur xs
               (assoc balance-amounts unit (- (balance-amounts unit 0) amount))
               (update-parts parts m)))
      (let [last-amount-ks [(dec (count parts)) :amount]
            out-tx {:date (date->int date), :description desc, :parts parts}]
        (if (get-in parts last-amount-ks)
          out-tx
          (assoc out-tx :parts 
                 (reduce update-parts parts
                         (map (fn [[unit amount]] {:unit unit :amount amount})
                              balance-amounts))))))))

(defn import-transactions [{:keys [target-value]} {:keys [txs]}]
  (into txs
        (map normalize-transaction
             (cljs.reader/read-string target-value))))

(defn import-prices [{:keys [target-value]} _]
  (cljs.reader/read-string target-value))
