(ns accounting.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)

(defn assoc-last [xs ks x]
  (assoc-in xs (into [(dec (count xs))] ks) x))

(defn log [x]
  (.log js/console x))
  
(defn log-clj [x]
  (log (clj->js x)))

(def channel (chan))
  
(def sample-txs [
  {:date "2015/02/18", :description "Whole Foods", :parts [
    {:account "E.Groceries", :amount 2000}
    {:account "A.Cash", :amount -2000}]}
  {:date "2015/02/19", :description "Whole Foods", :parts [
    {:account "E.Groceries", :amount 2000, :note "Food"}
    {:account "E.Alcohol", :amount 1000, :note "Beer"}
    {:account "A.Cash", :amount -3000}]}
])

(defn str->fixpt [s]
  (* s 100))
  
(defn fixpt->str [n]
  (.toFixed (/ n 100) 2))

(defn single-row-tx? [[part1 part2 :as parts]]
  (and (= 2 (count parts))
       (= (- (:amount part1)) (:amount part2))
       (= (:note part1) (:note part2))))

(defn cmp-sign-abs [n1 n2]
  (if (= (compare n1 0) (compare n2 0))
    (compare (Math/abs n2) (Math/abs n1))
    (compare n1 n2)))
    
(defn tx-tds [tx]
  (let [[from-part to-part :as parts] (sort-by :amount cmp-sign-abs (:parts tx))]
    (update-in 
      (if (single-row-tx? parts)
        [{:to-account (:account to-part)
          :from-account (:account from-part)
          :amount (fixpt->str (:amount to-part))
          :note (:note to-part)}]
        (vec
          (for [{:keys [account amount note]} parts
                :let [pos (pos? amount)]]
            {:amount (fixpt->str (if pos amount (- amount)))
             (if pos :to-account :from-account) account
             :note note})))
      [0] merge (select-keys tx [:date :description]))))

(def empty-entry-parts
  [{:account "", :amount "", :first? true}
   {:account "", :amount ""}])

(def app-state (atom 
  {:txs sample-txs 
   :entry-parts empty-entry-parts}))

(defn store-event [ev]
  {:target (aget ev "target")
   :target-value (aget ev "target" "value")})

(defn send! 
  ([type] (send! type {}))
  ([type arg]
    (fn [ev]
      (put! channel [type (store-event ev) arg]))))

(defn render-screen [state]
  (dom/form nil
    (dom/table nil
      (dom/thead nil
        (apply dom/tr nil
          (for [heading ["Date" "Description" "Account" "Amount"]]
            (dom/th nil heading))))
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
              (dom/td nil (if last? (dom/button #js {:type "button" :onClick (send! :create)} "Save"))))))))
    (dom/table nil
      (dom/thead nil
        (apply dom/tr nil
          (for [heading ["Date" "Description" "Debit Acct" "" "Amt" "" "Credit Acct" "Note"]]
            (dom/th nil heading))))
      (apply dom/tbody nil
        (for [tx (:txs @app-state)
              {:keys [date description to-account amount from-account note]} (tx-tds tx)]
          (dom/tr nil
            (dom/td nil date)
            (dom/td nil description)
            (dom/td nil to-account)
            (dom/td nil (if to-account "←"))
            (dom/td nil amount)
            (dom/td nil (if from-account "←"))
            (dom/td nil from-account)
            (dom/td nil note)))))))

(defn dom-el [id]
  (.getElementById js/document id))

(defn dom-el-val [id]
  (.-value (dom-el id)))
  
(defn dom-els [name]
  (.getElementsByName js/document name))
  
(defn dom-els-vals [name]
  (let [els (dom-els name)]
    (map #(aget els % "value")
         (range (.-length els)))))

(defn get-inputs-state []
  {:date (dom-el-val "date") 
   :description (dom-el-val "description")
   :accounts (dom-els-vals "account")
   :amounts (dom-els-vals "amount")
})

(defn create-transaction [{:keys [date description accounts amounts]}]
  {:date date
   :description description
   :parts (map (fn [account amount] {:account account, :amount (str->fixpt amount)})
               accounts
               amounts)}) 

(defn balance-amount [parts]
  (- (apply + (map (comp str->fixpt :amount) parts))))

(defn change-part-field [{:keys [target-value]} {:keys [index field]} parts]
  (assoc-in parts [index field] target-value))

(defn blur-amount [ev {:keys [last?]} parts]
  (let [balance (balance-amount parts)]
    (if (zero? balance)
      parts
      (assoc-last 
        (if last?
          (conj parts {:account "", :amount ""})
          parts)
        [:amount]
        (fixpt->str balance)))))
           
(defn handle-events [app owner]
  (go
    (while true
      (let [[type ev arg] (<! channel)]
        (case type
          :create (om/transact! app [:txs] #(conj % (create-transaction (get-inputs-state))))
          :change (om/transact! app [:entry-parts] #(change-part-field ev arg %))
          :blur (om/transact! app [:entry-parts] #(blur-amount ev arg %)))))))

(om/root 
  (fn [app owner]
    (reify
      om/IWillMount (will-mount [_] (handle-events app owner))
      om/IRenderState (render-state [_ _] (render-screen @app-state))))
  app-state
  {:target (. js/document (getElementById "accounting"))})

