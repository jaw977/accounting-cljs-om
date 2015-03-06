(ns accounting.export
  (:require [clojure.string :as str]
            [om.dom :as dom :include-macros true]
            [accounting.util :refer [assoc-last log log-clj fixpt->float balance-amount]]))

(defn export-part [{:keys [account amount unit]} last-account]
  (let [amount (and amount (fixpt->float amount))]
    (filter identity
            [(if (not= account last-account) account)
             (if amount
               (if unit
                 [amount unit]
                 amount))])))

(defn export-parts [parts]
  (apply concat
         (map export-part
              (if (= 0 (balance-amount parts))
                (assoc-last parts [:amount] nil)
                parts)
              (into [nil] (map :account parts)))))
              
(defn export-txs [txs]
  (str/join "\n"
            (for [{:keys [date description parts]} txs]
              (str (into [date description] (export-parts parts))))))

(defn render [txs]
  (dom/textarea #js {:rows 40 :cols 100 :value (export-txs txs)}))


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

