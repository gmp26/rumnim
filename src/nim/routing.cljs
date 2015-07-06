(ns ^:figwheel-no-load routing.core
    (:require [goog.events :as events]
              [goog.history.EventType :as EventType]
              [secretary.core :as secretary :refer-macros [defroute]]
              [clojure.string :as str]
              )
    (:import goog.History))


;;
;; define level-spec as an atom for game level configuration
;;
(def level-spec
  (atom [2 6 1 15]))


;;
;; basic hashbang routing to configure some game options
;;(

(secretary/set-config! :prefix "#")
 
;;
;; TODO: parameter validation
;;

(defroute 
  "/heaps/:heaps/height/:height" {:as params}
  (do
    (swap! level-spec (fn [cur x y] [2 (int x) 1 (int y)])
             (:heaps params)
             (:height params))
    (js/console.log (str (:heaps params) ":" (:height params)))))

(defroute 
  "/levels/:x/:x/:x/:x" {:as params}
  (do
    (let [read-levels #(map int (flatten %))]
      (swap! level-spec (fn [cur levels] (read-levels levels)) (:x params))
      (js/console.log (str "levels" (read-levels (:x params)))))))


;; history configuration.
;;
;; The invisible element "dummy" is needed to make goog.History reloadable by
;; figwheel. Without it we see
;; Failed to execute 'write' on 'Document': 
;; It isn't possible to write into a document from an
;; asynchronously-loaded external script unless it is explicitly 
;;
;; Note that this history handling must happen after route definitions for it
;; to kick in on initial page load.
;;
(let [h (History. false false "dummy")]
  (goog.events/listen h EventType/NAVIGATE #(secretary/dispatch! (.-token %)))
  (doto h (.setEnabled true)))
