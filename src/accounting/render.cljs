(ns accounting.render
  (:require [clojure.string :as str]
            [om.dom :as dom :include-macros true]
            [accounting.util :refer [assoc-last update-last log log-clj str->fixpt fixpt->str account-key->vec account-vec->str account-key->str str->account]]
            [accounting.calc :as calc]
            [accounting.export :as export]))

(def right-align #js {:style #js {:textAlign "right"}})
(def top-align #js {:style #js {:verticalAlign "text-top"}})

(defn import [send!]
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

(defn- table-headings [headings]
  (dom/thead nil
    (apply dom/tr nil
      (for [heading headings]
        (dom/th nil heading)))))

(defn menu [desc titles current-screen send! event-type]
  (apply dom/p nil desc
    (interpose " \u00A0 "
      (for [title titles
            :let [screen (keyword (str/lower-case title))]]
        (if (= screen current-screen)
          (dom/span #js {:style #js {:fontWeight "bold"}} title)
          (dom/a #js {:onClick (send! event-type screen) :href "#"} title))))))

(defn summary [{:keys [summary-account summary-groupby txs prices]} send!]
  (let [summary-rows (calc/summarize summary-account summary-groupby txs prices)
        max-amounts (apply max (map (comp count :amounts) summary-rows))]
    (dom/div nil 
      (menu "Group By: " ["Account" "Unit"] summary-groupby send! :summary-groupby)
      (dom/p nil "Account (js RegExp): " (dom/input #js {:value summary-account, :onChange (send! :summary-change), :onKeyDown (send! :summary-keydown)}))
      (dom/table #js {:className "table"}
        (table-headings (into [(if (= summary-groupby :account) "Account" "Unit") "Value" "Amounts"] (repeat (* 2 (dec max-amounts)) "")))
        (apply dom/tbody nil
          (for [{:keys [group value amounts]} summary-rows]
            (apply dom/tr nil
              (dom/td (if (= summary-groupby :unit) right-align) group)
              (dom/td right-align value)
              (for [amount (into (vec (interpose " + " amounts)) (repeat (* 2 (- max-amounts (count amounts))) ""))]
                (dom/td right-align amount)))))))))

(defn entry [{:keys [entry-date entry-description entry-parts entry-status]} send!]
  (dom/div nil
    (dom/p nil (cond
                 (= entry-status :complete) "Transaction Created!"
                 (= entry-status :editing) "You are currently creating a transaction.  It will not be saved until you click the Save button."
                 :else "Create a new transaction by entering in data below."))
    (dom/p nil "Amount:  Enter a plain number for a dollar amount (e.g. \"20\" for $20), or a number followed by symbol for a different asset (e.g. \"10 AAPL\" for 10 shares of AAPL).")
    (dom/table #js {:className "table"}
      (table-headings ["Date" "Description" "Account" "Amount" "Note" ""])
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
            
(defn camelcase-symbol->str [sym]
  (if (symbol? sym)
    (str/replace (str sym) #"([a-z])([A-Z])" "$1 $2")
    (str sym)))

(defn format-date [date]
  (if date
    (let [date (str date)]
      (str (subs date 0 4) "/" (subs date 4 6) "/" (subs date 6)))))

(defn detail [txs]
  (dom/table #js {:className "table"}
    (table-headings ["Date" "Description" "Debit Acct" "" "Amt" "" "Credit Acct" "Note"])
    (apply dom/tbody nil
      (for [tx txs
            {:keys [date description to-account amount from-account note]} (calc/tx-detail-rows tx)]
        (dom/tr nil
          (dom/td nil (format-date date))
          (dom/td nil (camelcase-symbol->str description))
          (dom/td nil (account-key->str to-account))
          (dom/td nil (if to-account "←"))
          (dom/td right-align amount)
          (dom/td nil (if from-account "←"))
          (dom/td nil (account-key->str from-account))
          (dom/td nil note))))))

(defn register [{:keys [register-account register-parts] :as state} send!]
  (dom/div nil
    (dom/p nil "Account: " (dom/input #js {:value register-account, :onChange (send! :register-change), :onKeyDown (send! :register-keydown)}))
    (dom/table #js {:className "table"}
      (table-headings ["Date" "Description" "Amount" "Note" "Totals"])
      (apply dom/tbody nil
        (for [{:keys [date description amount note unit totals]} register-parts]
          (apply dom/tr nil
            (dom/td nil (format-date date))
            (dom/td nil (camelcase-symbol->str description))
            (dom/td right-align (calc/display-amount amount unit))
            (dom/td nil note)
            (interpose (dom/td nil " + ")
              (for [[total-unit total-amount] totals]
                (dom/td right-align (calc/display-amount total-amount total-unit))))))))))
            
(defn screen [{:keys [screen txs] :as state} send!]
  (dom/div nil
    (menu "" ["Import" "Summary" "Detail" "Entry" "Register" "Export"] screen send! :screen) 
    (case screen
      :import (import send!)
      :export (export/render txs)
      :summary (summary state send!)
      :detail (detail txs)
      :entry (entry state send!)
      :register (register state send!))))


