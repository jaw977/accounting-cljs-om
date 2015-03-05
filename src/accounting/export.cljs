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
  (str "[\n"
       (str/join "\n"
              (for [{:keys [date description parts]} txs]
                (str (into [date description] (export-parts parts)))))
       "\n]\n"))

(defn render [txs]
  (dom/textarea #js {:rows 40 :cols 100 :value (export-txs txs)}))
