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
            [accounting.summary :as summary]
            [accounting.detail :as detail]
            [accounting.register :as register]))

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
   :import-txs ""
   :import-prices ""
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
          :import-text (om/update! app [(first arg)] (or (second arg) target-value))
          :import-txs (om/transact! app #(merge % {:screen :summary, :txs (import/import-transactions ev %)}))
          :import-prices (om/transact! app #(merge % {:screen :summary, :prices (import/import-prices ev %)}))
          :summary-groupby (om/update! app [:summary-groupby] arg)
          :summary-change (om/update! app [:summary-account] target-value)
          :register-change (om/update! app [:register-account] target-value)
          :register-keydown (om/update! app [:register-parts] (register/register-parts (str->account target-value) (:txs @app-state)))
          :entry-change (om/transact! app #(entry/change-input arg target-value %))
          :entry-save (om/transact! app entry/save-tx)
          :edit (om/transact! app #(entry/edit-tx % arg))
          :blur (om/transact! app [:entry-parts] #(entry/blur-amount ev arg %)))))))

(defn render [{:keys [screen txs] :as state} send!]
  (dom/div nil
    (render/menu "" ["Import" "Summary" "Detail" "Entry" "Register" "Export"] screen send! :screen) 
    (case screen
      :import (import/render state send!)
      :export (export/render txs)
      :summary (summary/render state send!)
      :detail (detail/render txs send!)
      :entry (entry/render state send!)
      :register (register/render state send!))))

(om/root 
  (fn [app owner]
    (reify
      om/IWillMount (will-mount [_] (handle-events app owner))
      om/IRenderState (render-state [_ _] (render @app-state send!))))
  app-state
  {:target (. js/document (getElementById "accounting"))})

