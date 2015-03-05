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

(defn render [txs]
  (dom/table #js {:className "table"}
    (render/table-headings ["Date" "Description" "Debit Acct" "" "Amt" "" "Credit Acct" "Note"])
    (apply dom/tbody nil
      (for [tx (sort-by :date txs)
            {:keys [date description to-account amount from-account note]} (tx-detail-rows tx)]
        (dom/tr nil
          (dom/td nil (render/format-date date))
          (dom/td nil (render/camelcase-symbol->str description))
          (dom/td nil (account-key->str to-account))
          (dom/td nil (if to-account "←"))
          (dom/td render/right-align amount)
          (dom/td nil (if from-account "←"))
          (dom/td nil (account-key->str from-account))
          (dom/td nil note))))))
