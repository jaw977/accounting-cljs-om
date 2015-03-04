(ns accounting.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as str]
            [cljs.core.async :refer [put! chan <!]]
            [accounting.util :refer [assoc-last update-last log log-clj str->fixpt fixpt->str account-key->vec account-vec->str account-key->str str->account today]]
            [accounting.render :as render]
            [accounting.entry :as entry]
            [accounting.import :as import]
            [accounting.export :as export]
            [accounting.calc :as calc]))

(enable-console-print!)

(def channel (chan))
  
(def app-state (atom 
  {:screen :import
   :txs []
   :prices [{}]
   :register-parts []
   :summary-account ""
   :summary-groupby :account
   :register-account ""
   :entry-date (today)
   :entry-description ""
   :entry-parts entry/empty-parts}))

(defn store-event [ev]
  {:target (aget ev "target")
   :target-value (aget ev "target" "value")})

(defn send! 
  ([type] (send! type {}))
  ([type arg]
    (fn [ev]
      (if (or (not= "keydown" (.-type ev)) (= 13 (.-keyCode ev)))
        (put! channel [type (store-event ev) arg])))))
           
(defn handle-events [app owner]
  (go
    (while true
      (let [[type {:keys [target-value] :as ev} arg] (<! channel)]
        (case type
          :screen (om/update! app [:screen] arg)
          :import-txs (om/transact! app #(merge % {:screen :summary, :txs (import/import-transactions ev %)}))
          :import-prices (om/transact! app #(merge % {:screen :summary, :prices (import/import-prices ev %)}))
          :summary-groupby (om/update! app [:summary-groupby] arg)
          :summary-change (om/update! app [:summary-account] target-value)
          :register-change (om/update! app [:register-account] target-value)
          :register-keydown (om/update! app [:register-parts] (calc/register-parts (str->account target-value) (:txs @app-state)))
          :entry-change (om/transact! app #(entry/change-input arg target-value %))
          :entry-create (om/transact! app entry/create-transaction)
          :blur (om/transact! app [:entry-parts] #(entry/blur-amount ev arg %)))))))

(om/root 
  (fn [app owner]
    (reify
      om/IWillMount (will-mount [_] (handle-events app owner))
      om/IRenderState (render-state [_ _] (render/screen @app-state send!))))
  app-state
  {:target (. js/document (getElementById "accounting"))})

