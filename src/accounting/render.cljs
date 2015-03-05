(ns accounting.render
  (:require [clojure.string :as str]
            [om.dom :as dom :include-macros true]
            [accounting.util :refer [log log-clj fixpt->str]]))

(def right-align #js {:style #js {:textAlign "right"}})
(def top-align #js {:style #js {:verticalAlign "text-top"}})

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

(defn table-headings [headings]
  (dom/thead nil
    (apply dom/tr nil
      (for [heading headings]
        (dom/th nil heading)))))

(defn menu [desc titles current-screen send! event-type]
  (apply dom/p nil desc
    (interpose " \u00A0 "
      (for [title titles
            :let [screen (keyword (str/lower-case title))]]
        (if (= screen current-screen)
          (dom/span #js {:style #js {:fontWeight "bold"}} title)
          (dom/a #js {:onClick (send! event-type screen) :href "#"} title))))))
            
(defn camelcase-symbol->str [sym]
  (if (symbol? sym)
    (str/replace (str sym) #"([a-z])([A-Z])" "$1 $2")
    (str sym)))

(defn format-date [date]
  (if date
    (let [date (str date)]
      (str (subs date 0 4) "/" (subs date 4 6) "/" (subs date 6)))))
            


