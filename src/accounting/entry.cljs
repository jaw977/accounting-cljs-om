(ns accounting.entry
  (:require [clojure.string :as str]
            [om.dom :as dom :include-macros true]
            [accounting.render :as render]
            [accounting.util :refer [assoc-last log log-clj str->fixpt fixpt->str str->account date->int today balance-amount]]))

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
                      :parts (vec (for [{:keys [account amount note]} entry-parts
                                        :when (and (seq account) (seq amount))]
                                    (merge {:account (str->account account), :note note}
                                           (str->amount-unit amount))))})}))

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
  
(defn render [{:keys [entry-date entry-description entry-parts entry-status]} send!]
  (dom/div nil
    (dom/p nil (cond
                 (= entry-status :complete) "Transaction Created!"
                 (= entry-status :editing) "You are currently creating a transaction.  It will not be saved until you click the Save button."
                 :else "Create a new transaction by entering in data below."))
    (dom/p nil "Amount:  Enter a plain number for a dollar amount (e.g. \"20\" for $20), or a number followed by symbol for a different asset (e.g. \"10 AAPL\" for 10 shares of AAPL).")
    (dom/table #js {:className "table"}
      (render/table-headings ["Date" "Description" "Account" "Amount" "Note" ""])
      (apply dom/tbody nil
        (let [parts-count (count entry-parts)]
          (for [parts-index (range parts-count)
                :let [{:keys [account amount note first?]} (nth entry-parts parts-index)
                      last? (= parts-index (dec parts-count))]]
            (dom/tr nil
              (dom/td nil (if first? (dom/input #js {:value entry-date, :size 10, :onChange (send! :entry-change [:entry-date])})))
              (dom/td nil (if first? (dom/input #js {:value entry-description, :onChange (send! :entry-change [:entry-description])})))
              (dom/td nil (dom/input #js {:value account, :onChange (send! :entry-change [:entry-parts parts-index :account])}))
              (dom/td nil (dom/input #js 
                {:value amount, :size 10
                 :onChange (send! :entry-change [:entry-parts parts-index :amount])
                 :onBlur (send! :blur {:last? last?})}))
              (dom/td nil (dom/input #js {:value note, :onChange (send! :entry-change [:entry-parts parts-index :note])}))
              (dom/td nil (if last? (dom/button #js {:type "button" :onClick (send! :entry-create)} "Save") " ")))))))))
