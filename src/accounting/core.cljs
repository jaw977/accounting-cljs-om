(ns accounting.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as str]
            [cljs.reader]
            [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)

(defn assoc-last [xs ks x]
  (assoc-in xs (into [(dec (count xs))] ks) x))

(defn update-last [xs ks f & args]
  (apply update-in xs (into [(dec (count xs))] ks) f args))

(defn ucfirst [s] 
  (str (.toUpperCase (first s)) (subs s 1)))

(defn log [x]
  (.log js/console x))
  
(defn log-clj [x]
  (log (clj->js x)))

(def channel (chan))
  
(def sample-txs [
  {:date "2015/02/17", :description "Starting Cash", :parts [
    {:account :a-cash, :amount 10000}
    {:account :i-start, :amount -10000}]}
  {:date "2015/02/18", :description "Whole Foods", :parts [
    {:account :e-groceries, :amount 2000}
    {:account :a-cash, :amount -2000}]}
  {:date "2015/02/19", :description "Whole Foods", :parts [
    {:account :e-groceries, :amount 2000, :note "Food"}
    {:account :e-alcohol, :amount 1000, :note "Beer"}
    {:account :a-cash, :amount -3000}]}
])

(defn str->fixpt [s]
  (* s 100))
  
(defn fixpt->str [n]
  (.toFixed (/ n 100) 2))
  
(defn account-key->vec [account]
  (str/split (subs (str account) 1) "-"))

(defn account-vec->str [account]
  (str/join "." (map ucfirst account)))

(def account-key->str (comp account-vec->str account-key->vec))

(defn str->account [account]
  (keyword (str/join "-" (map #(.toLowerCase %) (str/split account ".")))))

(defn single-row-tx? [[part1 part2 :as parts]]
  (and (= 2 (count parts))
       (= (- (:amount part1)) (:amount part2))
       (= (:note part1) (:note part2))))

(defn cmp-sign-abs [n1 n2]
  (if (= (compare n1 0) (compare n2 0))
    (compare (Math/abs n2) (Math/abs n1))
    (compare n1 n2)))
    
(defn display-amount [amount unit neg]
  (str (if unit (str (subs (str unit) 1) " ") "$") 
       (fixpt->str (if neg (- amount) amount))))

(defn tx-tds [tx]
  (let [[from-part to-part :as parts] (sort-by :amount cmp-sign-abs (:parts tx))]
    (update-in 
      (if (single-row-tx? parts)
        [{:to-account (:account to-part)
          :from-account (:account from-part)
          :amount (display-amount (:amount to-part) (:unit to-part))
          :note (:note to-part)}]
        (vec
          (for [{:keys [account amount unit note]} parts
                :let [pos (pos? amount)]]
            {:amount (display-amount amount unit (not pos))
             (if pos :to-account :from-account) account
             :note note})))
      [0] merge (select-keys tx [:date :description]))))

(def empty-entry-parts
  [{:account "", :amount "", :first? true}
   {:account "", :amount ""}])

(def app-state (atom 
  {:screen :import
   :txs sample-txs 
   :register-parts []
   :entry-parts empty-entry-parts}))

(defn store-event [ev]
  {:target (aget ev "target")
   :key-code (.-keyCode ev)
   :target-value (aget ev "target" "value")})

(defn send! 
  ([type] (send! type {}))
  ([type arg]
    (fn [ev]
      (put! channel [type (store-event ev) arg]))))
      
(defn summarize [txs]
  (->> txs
       (map :parts)
       (apply concat)
       (map (fn [part]
              (let [account (account-key->vec (:account part))]
                (map #(assoc part :account (take % account))
                     (range 1 (inc (count account)))))))
       (apply concat)
       (group-by #(map % [:account :unit]))
       (map (fn [[[account unit] parts]] 
              {:account (account-vec->str account), 
               :amount (display-amount (apply + (map :amount parts)) unit)}))
       (sort-by :account)))

(defn register-parts [account txs]
  (->> txs
       (map (fn [tx]
              (map #(merge % (select-keys tx [:date :description]))
                   (:parts tx))))
       (apply concat)
       (filter #(= account (:account %)))
       (map (fn [{:keys [account amount unit] :as part}]
              (merge part 
                     {:account (account-key->str account)
                      :amount (display-amount amount unit)})))))

(defn render-import [state]
  (dom/textarea #js {:onKeyDown (send! :import) :rows 25 :cols 100}))
  
(defn render-summary [summary-rows]
  (dom/table nil
    (dom/thead nil
      (dom/tr nil
        (dom/th nil "Account")
        (dom/th nil "Amount")))
    (apply dom/tbody nil
      (for [{:keys [account amount]} summary-rows]
        (dom/tr nil
          (dom/td nil account)
          (dom/td #js {:style #js {:textAlign "right"}} amount))))))

(defn render-detail [state]
  (dom/div nil
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
            (dom/td nil (account-key->str to-account))
            (dom/td nil (if to-account "←"))
            (dom/td #js {:style #js {:textAlign "right"}} amount)
            (dom/td nil (if from-account "←"))
            (dom/td nil (account-key->str from-account))
            (dom/td nil note)))))))

(defn render-register [{:keys [register-account register-parts] :as state}]
  (dom/div nil
    "Account: "
    (dom/input #js {:value register-account, :onKeyDown (send! :register)})
    (dom/table nil
      (dom/thead nil
        (apply dom/tr nil
          (for [heading ["Date" "Description" "Amount" "Note"]]
            (dom/th nil heading))))
      (apply dom/tbody nil
        (for [{:keys [date description amount note]} register-parts]
          (dom/tr nil
            (dom/td nil date)
            (dom/td nil description)
            (dom/td nil amount)
            (dom/td nil note)))))))

(defn render-menu [current-screen]
  (apply dom/p nil
    (interpose " \u00A0 "
      (for [title ["Import" "Summary" "Detail" "Register"]
            :let [screen (keyword (.toLowerCase title))]]
        (if (= screen current-screen)
          (dom/span #js {:style #js {:fontWeight "bold"}} title)
          (dom/a #js {:onClick (send! :screen screen) :href "#"} title))))))
           
(defn render-screen [{:keys [screen txs] :as state}]
  (dom/div nil
    (render-menu screen)
    (case screen
      :import (render-import state)
      :summary (render-summary (summarize txs))
      :detail (render-detail state)
      :register (render-register state))))

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
        
(defn update-parts [m parts]
  (let [last-part (peek parts)]
    (cond
      (:account m) (conj parts m)
      (:note m) (update-last parts [] merge m)
      (:amount last-part) (conj parts (merge m {:account (:account last-part)}))
      :else (update-last parts [] merge m))))

(defn normalize-transaction [[date desc & tx]]
  (loop [[x & xs] tx
         balance-amount 0
         parts []]
    (if x
      (let [m (read-transaction-element x)]
        (recur xs
               (- balance-amount (or (:amount m) 0))
               (update-parts m parts)))
      (let [last-amount-ks [(dec (count parts)) :amount]
            out-tx {:date date, :description desc, :parts parts}]
        (if (get-in parts last-amount-ks)
          out-tx
          (assoc-in out-tx (into [:parts] last-amount-ks) balance-amount))))))

(defn import-transactions [{:keys [target-value]} {:keys [txs]}]
  (into txs
        (map normalize-transaction
             (cljs.reader/read-string target-value))))

(defn create-transaction [{:keys [date description accounts amounts]}]
  {:date date
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
          :import (if (= 13 (:key-code ev)) 
            (om/transact! app #(merge % {:screen :summary, :txs (import-transactions ev %)})))
          :register (if (= 13 (:key-code ev))
            (om/update! app [:register-parts] (register-parts (str->account (:target-value ev)) (:txs @app-state))))
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

(comment
  [["2015/02/23" "Whole Foods" :e-groceries 25 :l-amex]
   ["2015/02/24" "Trader Joe's" :e-groceries 20 :e-alcohol 10 :l-amex]
   ["2015/02/25" "Ameritrade" :a-ameritrade [100 :GLD] :a-ameritrade -10010]]
)   
    
