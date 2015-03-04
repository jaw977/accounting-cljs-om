(ns accounting.entry
  (:require [clojure.string :as str]
            [accounting.util :refer [assoc-last log log-clj str->fixpt fixpt->str str->account date->int today]]))

(def empty-parts
  [{:account "", :amount "", :note "", :first? true}
   {:account "", :amount "", :note ""}])

(defn str->amount-unit [s]
  (let [[amount unit] (str/split s #"\s+" 2)]
    {:amount (str->fixpt amount), :unit (keyword unit)}))

(defn change-input [ks s state]
  (assoc (assoc-in state ks s)
         :entry-status :editing))

(defn create-transaction [{:keys [txs entry-date entry-description entry-parts] :as state}]
  (merge state 
         {:entry-date (today), :entry-description "", :entry-parts empty-parts, :entry-status :complete
          :txs (conj txs
                     {:date (date->int entry-date)
                      :description entry-description
                      :parts (for [{:keys [account amount note]} entry-parts
                                   :when (and (seq account) (seq amount))]
                               (merge {:account (str->account account), :note note}
                                      (str->amount-unit amount)))})}))

(defn balance-amount [parts]
  (- (apply + (map (comp str->fixpt :amount) parts))))

(defn blur-amount [ev {:keys [last?]} parts]
  (let [balance (balance-amount parts)]
    (if (zero? balance)
      parts
      (assoc-last 
        (if last?
          (conj parts {:account "", :amount "", :note ""})
          parts)
        [:amount]
        (if (js/isNaN balance)
          ""
          (fixpt->str balance))))))
  
