(ns accounting.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as str]
            [cljs.reader]
            [cljs.core.async :refer [put! chan <!]]
            [accounting.util :refer [assoc-last update-last ucfirst log log-clj str->fixpt fixpt->str account-key->vec account-vec->str account-key->str str->account]]
            [accounting.render :as render]
            [accounting.calc :as calc]))

(enable-console-print!)

(def channel (chan))
  
(def empty-entry-parts
  [{:account "", :amount "", :first? true}
   {:account "", :amount ""}])

(def app-state (atom 
  {:screen :import
   :txs []
   :prices [{}]
   :register-parts []
   :entry-parts empty-entry-parts}))

(defn store-event [ev]
  {:target (aget ev "target")
   :target-value (aget ev "target" "value")})

(defn send! 
  ([type] (send! type {}))
  ([type arg]
    (fn [ev]
      (if (or (not= "keydown" (.-type ev)) (= 13 (.-keyCode ev)))
        (put! channel [type (store-event ev) arg])))))
      
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

(defn read-transaction-element [x]
  (cond
    (number? x) {:amount (str->fixpt x)}
    (string? x) {:note x}
    (keyword? x) {:account x}
    (vector? x) {:amount (str->fixpt (nth x 0)) :unit (nth x 1)}
    :else {}))
        
(defn update-parts [parts m]
  (let [last-part (peek parts)]
    (cond
      (:account m) (conj parts m)
      (:note m) (update-last parts [] merge m)
      (:amount last-part) (conj parts (merge m {:account (:account last-part)}))
      :else (update-last parts [] merge m))))

(defn date->int [date]
  (if (number? date)
    date
    (js/parseInt (.replace date (js/RegExp. "[^0-9]" "g") ""))))

(defn normalize-transaction [[date desc & tx]]
  (loop [[x & xs] tx
         balance-amounts {}
         parts []]
    (if x
      (let [{:keys [amount unit] :or {amount 0} :as m} (read-transaction-element x)]
        (recur xs
               (assoc balance-amounts unit (- (balance-amounts unit 0) amount))
               (update-parts parts m)))
      (let [last-amount-ks [(dec (count parts)) :amount]
            out-tx {:date (date->int date), :description desc, :parts parts}]
        (if (get-in parts last-amount-ks)
          out-tx
          (assoc out-tx :parts 
                 (reduce update-parts parts
                         (map (fn [[unit amount]] {:unit unit :amount amount})
                              balance-amounts))))))))

(defn import-transactions [{:keys [target-value]} {:keys [txs]}]
  (into txs
        (map normalize-transaction
             (cljs.reader/read-string target-value))))

(defn import-prices [{:keys [target-value]} _]
  (cljs.reader/read-string target-value))

(defn create-transaction [{:keys [date description accounts amounts]}]
  {:date (date->int date)
   :description description
   :parts (map (fn [account amount] {:account (str->account account), :amount (str->fixpt amount)})
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
          :screen (om/update! app [:screen] arg)
          :import-txs (om/transact! app #(merge % {:screen :summary, :txs (import-transactions ev %)}))
          :import-prices (om/transact! app #(merge % {:screen :summary, :prices (import-prices ev %)}))
          :register (om/update! app [:register-parts] (calc/register-parts (str->account (:target-value ev)) (:txs @app-state)))
          :create (om/transact! app [:txs] #(conj % (create-transaction (get-inputs-state))))
          :change (om/transact! app [:entry-parts] #(change-part-field ev arg %))
          :blur (om/transact! app [:entry-parts] #(blur-amount ev arg %)))))))

(om/root 
  (fn [app owner]
    (reify
      om/IWillMount (will-mount [_] (handle-events app owner))
      om/IRenderState (render-state [_ _] (render/screen @app-state send!))))
  app-state
  {:target (. js/document (getElementById "accounting"))})

