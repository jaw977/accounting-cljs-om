(ns accounting.summary
  (:require [clojure.string :as str]
            [om.dom :as dom :include-macros true]
            [accounting.render :as render]
            [accounting.util :refer [log log-clj account-key->vec account-vec->str]]))

(defn get-price [unit prices]
  ((peek prices) unit 1))
  
(defn get-value [amount unit prices]
  (* amount (get-price unit prices)))

(defn summarize [account-filter groupby txs prices]
  (->> txs
       (map :parts)
       (apply concat)
       (map (fn [part]
              (let [account (account-key->vec (:account part))]
                (map #(assoc part :account (take % account))
                     (if (= groupby :account)
                       (range 1 (inc (count account)))
                       [(count account)])))))
       (apply concat)
       (filter #(re-find (js/RegExp. account-filter) (account-vec->str (:account %))))
       (group-by groupby)
       (map (fn [[group parts]]
              (let [units (map (fn [[unit parts]] 
                                 (let [amount (apply + (map :amount parts))]
                                   {:amount amount, :unit unit, :value (get-value amount unit prices)}))
                               (group-by :unit parts))]
                {:group (if (= groupby :account) 
                          (account-vec->str group)
                          (render/display-unit group))
                 :value (render/display-amount (apply + (map :value units)))
                 :amounts (map (fn [{:keys [amount unit]}] (render/display-amount amount unit)) units)})))
       (sort-by :group)))

(defn render [{:keys [summary-account summary-groupby txs prices]} send!]
  (let [summary-rows (summarize summary-account summary-groupby txs prices)
        max-amounts (apply max (map (comp count :amounts) summary-rows))]
    (dom/div nil 
      (render/menu "Group By: " ["Account" "Unit"] summary-groupby send! :summary-groupby)
      (dom/p nil "Account (js RegExp): " (dom/input #js {:value summary-account, :onChange (send! :summary-change), :onKeyDown (send! :summary-keydown)}))
      (dom/table #js {:className "table"}
        (render/table-headings (into [(if (= summary-groupby :account) "Account" "Unit") "Value" "Amounts"] (repeat (* 2 (dec max-amounts)) "")))
        (apply dom/tbody nil
          (for [{:keys [group value amounts]} summary-rows]
            (apply dom/tr nil
              (dom/td (if (= summary-groupby :unit) right-align) group)
              (dom/td right-align value)
              (for [amount (into (vec (interpose " + " amounts)) (repeat (* 2 (- max-amounts (count amounts))) ""))]
                (dom/td right-align amount)))))))))


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

