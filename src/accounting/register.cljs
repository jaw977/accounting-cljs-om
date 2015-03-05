(ns accounting.register
  (:require [clojure.string :as str]
            [om.dom :as dom :include-macros true]
            [accounting.render :as render]
            [accounting.util :refer [log log-clj]]))

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
