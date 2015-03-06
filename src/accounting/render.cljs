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
            

; The MIT License (MIT)
;
; Copyright (c) 2015 Jason Waag
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
; THE SOFTWARE.

