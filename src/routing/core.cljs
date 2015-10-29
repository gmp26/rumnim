(ns ^:figwheel-always routing.core
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
    (swap! level-spec (fn [cur x y]
                        (let [valid-x (max 2 (min (int x) 6))
                              valid-y (max 1 (min (int y) 15))]
                         [2 valid-x 1 valid-y]))
             (:heaps params)
             (:height params))
    ))

(defroute
  "/levels/:x/:x/:x/:x" {:as params}
  (do
    (let [read-levels #(map int (flatten %))
          valid-range (fn [a b v] (max a (min b v)))]
      (swap! level-spec
             (fn [cur levels]
               (let [[heaps-min heaps-max height-min height-max] (read-levels levels)
                     valid-heaps-min (valid-range 1 6 heaps-min)
                     valid-heaps-max (valid-range valid-heaps-min 6 heaps-max)
                     valid-height-min (valid-range 1 15 height-min)
                     valid-height-max (valid-range valid-height-min 15 height-max)]
                 [valid-heaps-min valid-heaps-max valid-height-min valid-height-max])
               ) (:x params))
      )))


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
