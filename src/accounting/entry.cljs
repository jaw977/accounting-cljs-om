(ns accounting.entry
  (:require [clojure.string :as str]
            [om.dom :as dom :include-macros true]
            [accounting.render :as render]
            [accounting.util :refer [assoc-last log log-clj str->fixpt fixpt->str str->account account-key->str date->int today balance-amount]]))

(def empty-parts
  [{:account "", :amount "", :note "", :first? true}
   {:account "", :amount "", :note ""}])

(defn str->amount-unit [s]
  (let [[amount unit] (str/split s #"\s+" 2)]
    {:amount (str->fixpt amount), :unit (keyword unit)}))

(defn str->description [s]
  (let [sym (symbol (str/replace s #"\s" ""))]
    (if (= s (render/camelcase-symbol->str sym))
      sym
      s)))

(defn change-input [ks s state]
  (assoc (assoc-in state ks s)
         :entry-status :editing))

(defn new-tx [{:keys [entry-date entry-description entry-parts]}]
  {:date (date->int entry-date)
   :description (str->description entry-description)
   :parts (vec (for [{:keys [account amount note]} entry-parts
                     :when (and (seq account) (seq amount))]
                 (merge {:account (str->account account), :note note}
                        (str->amount-unit amount))))})

(defn save-tx [{:keys [txs entry-index] :as state}]
  (let [tx (new-tx state)]
    (merge state 
           {:entry-date (today), 
            :entry-description "", 
            :entry-parts empty-parts, 
            :entry-status :complete,
            :entry-index nil,
            :txs (if entry-index
                   (assoc txs entry-index tx)
                   (conj txs tx))})))

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
  
(defn first-part [parts]
  (assoc-in (vec parts) [0 :first?] true))

(defn edit-tx [{:keys [txs] :as state} index]
  (let [{:keys [date description parts]} (nth txs index)]
    (merge state
           {:screen :entry
            :entry-index index
            :entry-status :editing
            :entry-date (render/format-date date)
            :entry-description (render/camelcase-symbol->str description)
            :entry-parts (first-part
                          (for [{:keys [account amount unit note]} parts
                                :let [amount (subs (render/display-amount amount) 1)]]
                            {:account (account-key->str account)
                             :amount (if unit
                                       (str amount " " (render/display-unit unit))
                                       amount)
                             :note note}))})))

(defn render [{:keys [entry-date entry-description entry-parts entry-status entry-index]} send!]
  (dom/div nil
    (dom/p nil (cond
                 (= entry-status :complete) "Transaction Saved!"
                 (= entry-status :editing) (if entry-index
                                             "You are currently editing an existing transaction."
                                             "You are currently creating a new transaction.")
                 :else "Create a new transaction by entering in data below."))
    (dom/p nil "Amount:  Enter a plain number for a dollar amount (e.g. \"20\" for $20), or a number followed by symbol for a different asset (e.g. \"10 AAPL\" for 10 shares of AAPL).  A positive number indicates money flowing into an account (a debit), and a negative number indicates money flowing out of an account (a credit).  Income amounts are negative numbers and expense amounts are positive numbers.")
    (dom/p nil "Data that you enter here is not automatically saved, either locally or externally.  Currently, the only way to save data that you enter is to enter the export screen, and manually copy and paste the contents displayed there, prior to closing or refreshing the browser window.")
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
              (dom/td nil (if last? (dom/button #js {:type "button" :onClick (send! :entry-save)} "Save") " ")))))))))


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

