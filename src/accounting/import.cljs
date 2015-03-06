(ns accounting.import
  (:require [cljs.reader]
            [om.dom :as dom :include-macros true]
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

(defn in-vec [s]
  (str "[" s "]"))

(defn import-transactions [{:keys [target-value]} {:keys [txs]}]
  (into txs
        (map normalize-transaction
             (cljs.reader/read-string (in-vec target-value)))))

(defn import-prices [{:keys [target-value]} _]
  (cljs.reader/read-string (in-vec target-value)))
  
(def example-txs 
"[20140224 StartingEquity :assets-cash 100 :assets-etrade 2000 :equity]
[20140225 TraderJoes :expenses-groceries 25 :assets-cash]
[\"2014-02-26\" \"E*Trade\" :assets-etrade [10 :GLD] :expenses-commissions 10 :assets-etrade -1167]
[\"2014-02-27\" \"Whole Foods\" :expenses-groceries 30 :liabilities-visa]
[\"2014/02/28\" Paycheck :assets-bank 1000 :income-job]
")

(def example-prices "{:GLD 115.70}")

(defn render [{:keys [import-txs import-prices]} send!]
  (dom/div nil
    (dom/p nil "Import transactions:  Paste data in the text area and press enter. "  
      (dom/a #js {:onClick (send! :import-text [:import-txs example-txs]) :href "#"} "Example data"))
    (dom/textarea #js {:onChange (send! :import-text [:import-txs]) 
                       :onKeyDown (send! :import-txs)
                       :value import-txs
                       :rows 20 :cols 100})
    (dom/p nil "Import asset values:  Paste data and press enter. "
      (dom/a #js {:onClick (send! :import-text [:import-prices example-prices]) :href "#"} "Example data"))
    (dom/textarea #js {:onChange (send! :import-text [:import-prices])
                       :onKeyDown (send! :import-prices)
                       :value import-prices
                       :rows 20 :cols 100})))
