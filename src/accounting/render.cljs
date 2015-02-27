(ns accounting.render
  (:require 
            [om.dom :as dom :include-macros true]
            [accounting.util :refer [assoc-last update-last ucfirst log log-clj str->fixpt fixpt->str account-key->vec account-vec->str account-key->str str->account]]
            [accounting.calc :as calc]))

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
            "[\"2014-02-24\" \"Starting Equity\" :assets-cash 100 :assets-etrade 2000 :equity]\n"
            "[\"2014-02-25\" \"Trader Joe's\" :expenses-groceries 25 :assets-cash]\n"
            "[\"2014-02-26\" \"E*Trade\" :assets-etrade [10 :GLD] :expenses-commissions 10 :assets-etrade -1167]\n"
            "[\"2014-02-27\" \"Whole Foods\" :expenses-groceries 30 :liabilities-visa]\n"
            "[\"2014-02-28\" \"Paycheck\" :assets-bank 1000 :income-job]\n"
            "]\n"))))
    (dom/p nil "Import asset values:  Paste data and press enter.  Example Data:  "
      (dom/span #js {:style #js {:fontFamily "monospace"}} "[{:GLD 115.70}]"))
    (dom/textarea #js {:onKeyDown (send! :import-prices) :rows 20 :cols 100})))

(defn- table-headings [headings]
  (dom/thead nil
    (apply dom/tr nil
      (for [heading headings]
        (dom/th nil heading)))))

(defn summary [summary-rows]
  (dom/table nil
    (table-headings ["Account" "Value" "Amounts"])
    (apply dom/tbody nil
      (for [{:keys [account value amounts]} summary-rows]
        (apply dom/tr nil
          (dom/td nil account)
          (dom/td right-align value)
          (for [amount (interpose " + " amounts)]
            (dom/td right-align amount)))))))

(defn entry [state send!]
  (dom/table nil
    (table-headings ["Date" "Description" "Account" "Amount"])
    (apply dom/tbody nil
      (let [parts (:entry-parts state)
            parts-count (count parts)]
        (for [parts-index (range parts-count)
              :let [{:keys [account amount first?]} (nth parts parts-index)
                    last? (= parts-index (dec parts-count))]]
          (dom/tr nil
            (dom/td nil (if first? (dom/input #js {:id "date"})))
            (dom/td nil (if first? (dom/input #js {:id "description"})))
            (dom/td nil (dom/input #js {:name "account"}))
            (dom/td nil (dom/input #js 
              {:name "amount", :value amount 
               :onChange (send! :change {:field :amount, :index parts-index}) 
               :onBlur (send! :blur {:last? last?})}))
            (dom/td nil (if last? (dom/button #js {:type "button" :onClick (send! :create)} "Save")))))))))
            
(defn detail [txs]
  (dom/table nil
    (table-headings ["Date" "Description" "Debit Acct" "" "Amt" "" "Credit Acct" "Note"])
    (apply dom/tbody nil
      (for [tx txs
            {:keys [date description to-account amount from-account note]} (calc/tx-detail-rows tx)]
        (dom/tr nil
          (dom/td nil date)
          (dom/td nil description)
          (dom/td nil (account-key->str to-account))
          (dom/td nil (if to-account "←"))
          (dom/td right-align amount)
          (dom/td nil (if from-account "←"))
          (dom/td nil (account-key->str from-account))
          (dom/td nil note))))))

(defn register [{:keys [register-account register-parts] :as state} send!]
  (dom/div nil
    "Account: "
    (dom/input #js {:value register-account, :onKeyDown (send! :register)})
    (dom/table nil
      (table-headings ["Date" "Description" "Amount" "Note" "Totals"])
      (apply dom/tbody nil
        (for [{:keys [date description amount note unit totals]} register-parts]
          (apply dom/tr nil
            (dom/td nil date)
            (dom/td nil description)
            (dom/td right-align (calc/display-amount amount unit))
            (dom/td nil note)
            (interpose (dom/td nil " + ")
              (for [[total-unit total-amount] totals]
                (dom/td right-align (calc/display-amount total-amount total-unit))))))))))
            
(defn export [state]
  (dom/p nil "Export screen coming soon!"))

(defn menu [current-screen send!]
  (apply dom/p nil
    (interpose " \u00A0 "
      (for [title ["Import" "Summary" "Detail" "Register" "Export"]
            :let [screen (keyword (.toLowerCase title))]]
        (if (= screen current-screen)
          (dom/span #js {:style #js {:fontWeight "bold"}} title)
          (dom/a #js {:onClick (send! :screen screen) :href "#"} title))))))
           
(defn screen [{:keys [screen txs prices] :as state} send!]
  (dom/div nil
    (menu screen send!) 
    (case screen
      :import (import send!)
      :export (export state)
      :summary (summary (calc/summarize txs prices))
      :detail (dom/div nil
        (entry state send!)
        (detail txs))
      :register (register state send!))))


