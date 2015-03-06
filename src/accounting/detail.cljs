(ns accounting.detail
  (:require [clojure.string :as str]
            [om.dom :as dom :include-macros true]
            [accounting.render :as render]
            [accounting.util :refer [log log-clj account-key->str]]))

(defn single-row-tx? [[part1 part2 :as parts]]
  (and (= 2 (count parts))
       (= (- (:amount part1)) (:amount part2))
       (or (not (:note part1)) (not (:note part2)) (= (:note part1) (:note part2)))))

(defn cmp-sign-abs [n1 n2]
  (if (= (compare n1 0) (compare n2 0))
    (compare (Math/abs n2) (Math/abs n1))
    (compare n1 n2)))

(defn tx-detail-rows [tx]
  (let [[from-part to-part :as parts] (sort-by :amount cmp-sign-abs (:parts tx))]
    (update-in 
      (if (single-row-tx? parts)
        [{:to-account (:account to-part)
          :from-account (:account from-part)
          :amount (render/display-amount (:amount to-part) (:unit to-part))
          :note (or (:note to-part) (:note from-part))}]
        (vec
          (for [{:keys [account amount unit note]} parts
                :let [pos (pos? amount)]]
            {:amount (render/display-amount amount unit (not pos))
             (if pos :to-account :from-account) account
             :note note})))
      [0] merge (select-keys tx [:date :description]))))

(defn render [txs send!]
  (dom/table #js {:className "table"}
    (render/table-headings ["Edit" "Date" "Description" "Debit Acct" "" "Amt" "" "Credit Acct" "Note"])
    (apply dom/tbody nil
      (for [[tx index] (sort-by (comp :date first) (map list txs (range)))
            {:keys [date description to-account amount from-account note]} (tx-detail-rows tx)]
        (dom/tr nil
          (dom/td #js {:onClick (send! :edit index)} (if date "✎"))
          (dom/td nil (render/format-date date))
          (dom/td nil (render/camelcase-symbol->str description))
          (dom/td nil (account-key->str to-account))
          (dom/td nil (if to-account "←"))
          (dom/td render/right-align amount)
          (dom/td nil (if from-account "←"))
          (dom/td nil (account-key->str from-account))
          (dom/td nil note))))))


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

