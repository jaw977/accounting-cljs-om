(ns accounting.calc
  (:require [clojure.string :as str]
            [accounting.util :refer [assoc-last update-last log log-clj str->fixpt fixpt->str account-key->vec account-vec->str account-key->str str->account]]))

(defn get-price [unit prices]
  ((peek prices) unit 1))
  
(defn get-value [amount unit prices]
  (* amount (get-price unit prices)))

(defn display-unit [unit]
  (if unit
    (str (subs (str unit) 1) " ")
    "$"))

(defn display-amount
  ([amount] (display-amount amount nil nil))
  ([amount unit] (display-amount amount unit nil))
  ([amount unit neg]
   (str (display-unit unit)
        (str/replace (fixpt->str (if neg (- amount) amount))
                     #"\B(?=(\d{3})+(?!\d))"
                     ","))))

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
                          (display-unit group))
                 :value (display-amount (apply + (map :value units)))
                 :amounts (map (fn [{:keys [amount unit]}] (display-amount amount unit)) units)})))
       (sort-by :group)))

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
          :amount (display-amount (:amount to-part) (:unit to-part))
          :note (or (:note to-part) (:note from-part))}]
        (vec
          (for [{:keys [account amount unit note]} parts
                :let [pos (pos? amount)]]
            {:amount (display-amount amount unit (not pos))
             (if pos :to-account :from-account) account
             :note note})))
      [0] merge (select-keys tx [:date :description]))))

(defn register-parts [account txs]
  (->> txs
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


