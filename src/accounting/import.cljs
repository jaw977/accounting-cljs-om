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

(defn import-transactions [{:keys [target-value]} {:keys [txs]}]
  (into txs
        (map normalize-transaction
             (cljs.reader/read-string target-value))))

(defn import-prices [{:keys [target-value]} _]
  (cljs.reader/read-string target-value))

(defn render [send!]
  (dom/div nil
    (dom/table nil
      (dom/tr nil
        (dom/td nil
          (dom/p nil "Import transactions:  Paste data in the text area and press enter.  Example data: →")
          (dom/textarea #js {:onKeyDown (send! :import-txs) :rows 20 :cols 100}))
        (dom/td top-align
          (dom/pre nil 
            "[\n"
            "[20140224 StartingEquity :assets-cash 100 :assets-etrade 2000 :equity]\n"
            "[20140225 TraderJoes :expenses-groceries 25 :assets-cash]\n"
            "[\"2014-02-26\" \"E*Trade\" :assets-etrade [10 :GLD] :expenses-commissions 10 :assets-etrade -1167]\n"
            "[\"2014-02-27\" \"Whole Foods\" :expenses-groceries 30 :liabilities-visa]\n"
            "[\"2014/02/28\" Paycheck :assets-bank 1000 :income-job]\n"
            "]\n"))))
    (dom/p nil "Import asset values:  Paste data and press enter.  Example Data:  "
      (dom/span #js {:style #js {:fontFamily "monospace"}} "[{:GLD 115.70}]"))
    (dom/textarea #js {:onKeyDown (send! :import-prices) :rows 20 :cols 100})))
