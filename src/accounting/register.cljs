(ns accounting.register
  (:require [clojure.string :as str]
            [om.dom :as dom :include-macros true]
            [accounting.render :as render]
            [accounting.util :refer [log log-clj]]))

(defn register-parts [account txs]
  (->> txs
       (sort-by :date)
       (map (fn [tx]
              (map #(merge % (select-keys tx [:date :description]))
                   (:parts tx))))
       (apply concat)
       (filter #(= account (:account %)))
       (reduce (fn [parts {:keys [amount unit] :as part}]
                 (let [totals ((or (peek parts) {}) :totals {})]
                   (conj parts
                         (assoc part :totals
                                (assoc totals unit
                                       (+ amount (totals unit 0)))))))
               [])))

(defn render [{:keys [register-account register-parts] :as state} send!]
  (dom/div nil
    (dom/p nil "Account: " (dom/input #js {:value register-account, :onChange (send! :register-change), :onKeyDown (send! :register-keydown)}))
    (dom/table #js {:className "table"}
      (render/table-headings ["Date" "Description" "Amount" "Note" "Totals"])
      (apply dom/tbody nil
        (for [{:keys [date description amount note unit totals]} register-parts]
          (apply dom/tr nil
            (dom/td nil (render/format-date date))
            (dom/td nil (render/camelcase-symbol->str description))
            (dom/td right-align (render/display-amount amount unit))
            (dom/td nil note)
            (interpose (dom/td nil " + ")
              (for [[total-unit total-amount] totals]
                (dom/td right-align (render/display-amount total-amount total-unit))))))))))


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

