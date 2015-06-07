(ns ^:figwheel-always nim.core
    (:require [rum :as r]
              [cljs.reader :as reader]))

(enable-console-print!)

(def grid-size 12)

(def tick-in-ms 250)

;; utilities

;; don't assume es6 yet!
(def log2 #(/ (Math.log %) (Math.log 2)))
(def msb #(Math.floor (log2 %)))

(defn rand-in-range [start end]
  "generate an int inside [start, end] closed interval"
  (+ start (rand-int (- (inc end) start))))

(defn rand-heap [min-height max-height]
  "generate a heap sized from min-height to max-height"
  (rand-in-range min-height max-height))

(defn rand-heaps [min-heaps max-heaps min-height max-height]
  "generate some heaps"
  (let [heap-count (rand-in-range min-heaps max-heaps)
        make-heap (fn [] (rand-heap min-height max-height))]
    (vec (repeatedly heap-count make-heap))))

(defn heaps-at-k-take-n [heaps k n]
  "take n counters from kth heap"
  (if (contains? heaps k)
    (let [heap (nth heaps k)
          new-heap (if (>= heap n) (- heap n) 0)]
      (assoc heaps k new-heap))
    heaps))

(defn max-pairing [heaps]
  (msb (apply max heaps))
  )

(def level-spec 
  [2 6 1 12])

(defn game-setup []
  "setup or restart the game"
  (let [heaps (apply rand-heaps level-spec)]
    {:primed nil
     :heaps heaps
     :level 1
     :round 1
     :pairing 0}))

;;
;; define game as the single? game state atom
;;
;; TODO: change to defonce
;;
(defonce game
  (atom (game-setup)))

(defn next-game! [game]
  (let [{:keys [level round]} game]
    [level round]))

(def level2-heaps [
                   [1 0]
                   [1 1]
                   [2 1]
                   [2 2]
                   [4 2]
                   [3 8]
                   [4 5]
                   [12 1]
                   ])

(def level3-heaps [
                   [1]
                   [1 1 1]
                   [1 1 1 1]
                   [1 1 1 1 1 1 1]
                   [1 1 1 1 1 1 1 1 1 1]
                   ])

(defn lookup-heaps [heap-table round] 
  (if (contains? heap-table round) (nth heap-table round) nil))

(defn make-heaps [level round]
  "make heaps for given round and level. Return round level and heaps "
  (loop [level level
         round round]

    (let [result (cond 
                  (= 1 level) (rand-heaps 1 1 1 10)
                  (= 2 level) (lookup-heaps level2-heaps round)
                  (= 3 level) (lookup-heaps level3-heaps round)
                  (= 4 level) (rand-heaps 3 3 1 2)
                  (= 5 level) (rand-heaps 3 3 1 4)
                  (= 6 level) (rand-heaps 3 3 1 8)
                  (= 7 level) (rand-heaps 3 3 1 16)
                  :else (rand-heaps 1 6 1 12))]
      
      (if (not= nil result)
        {:heaps result :level level :round round}
        (recur (inc level) 0)))))

;;
;; layout
;;


;;
;; game strategy
;;
(defn nim-sum [heaps]
  "Calculate the nim-sum of all heaps by xoring them all together"
  (reduce bit-xor heaps))

(defn take-one-moves [heaps]
  "return a list of possible moves which take 1, or nil if heaps are all empty"
  (let [x-ifnz (fn [index heap-size] 
                 (if (> heap-size 0) [index 1]))]
    (seq (keep-indexed x-ifnz heaps))))

(defn random-move [heaps]
  "return a random move"
  (let [possibles (take-one-moves heaps)]
    (if possibles
      (rand-nth possibles)
      nil))
  )

(defn get-best-move [heaps]
  "get the column and number to take for the best move"
  (let [nsum (nim-sum heaps)]
    (if (> nsum 0)
      (let [ks (keep-indexed #(if (< (bit-xor nsum %2) %2) %1) heaps)
; todo: ks may be empty?
            k (rand-nth ks)
            n (nth heaps k)
            n' (bit-xor n nsum)]
        [k (- n n')])
      (random-move heaps))))

;;
;; mutate game
;;
(defn start! []
  "start a new game"
  (swap! game game-setup))

(defn change-level! [event]
  (.debug js/console (-> event .-target .-value))
  (let [new-level (-> event .-target .-value int)]
    (swap! game #(assoc % :level new-level))))

(defn from-k-take-n! [k n]
  (let [heaps (:heaps @game)
        new-heaps (heaps-at-k-take-n heaps k n)]
    (do 
      (swap! game #(assoc % :heaps new-heaps))
      (println "taking " n " from " k))))

(defn is-primed? [k n]
  "is item at col k, height n, primed for deletion?"
  (let  [[k' n' :as p] (:primed @game)]  
    (and p (= k k') (= n n'))))

(defn is-highlighted? [k n]
  "is item at col k, height n, highlighted for deletion?"
  (let [[k' n' :as p] (:primed @game)]
    (and p (= k k') (>= n n'))))

(defn un-prime! []
  "unprime all"
  (swap! game #(assoc % :primed nil)))

(defn prime! [k n]
  "prime the nth item in heap k"
  (swap! game #(assoc % :primed [k n])))

(defn at-k-leave-n! [k n]
  (- (nth (:heaps @game) k) (- n 1)))

(defn prime-or-delete! [k n]
  "item k n is to be primed for deletion, or deleted if already primed"
  (if (is-primed? k n)
    (do
      (un-prime!)
      (from-k-take-n! k (at-k-leave-n! k n)))
    (prime! k n)))

(defn hint! []
  "highlight the next best move"
  (let [[k n] (get-best-move (:heaps @game))]
    (prime! k (at-k-leave-n! k n))))

(defn pair! []
  "pair items together somehow"
  (println "pair")
  (let [heaps (:heaps @game)
        p (:pairing @game)
        pairing (if (>= p (max-pairing heaps)) 0 (inc p))]
    (swap! game #(assoc % :pairing pairing))
    )
)

;;
;; event handling
;;

(defn you-clicked-on [[k n]]
  (println (str "you clicked on " k " " n)))

(defn item-clicked [event]
  "read [col row] form from event.target.id string"
  (let [[k n] (reader/read-string (-> event .-target .-id))]
    (do
      (println "prime-or-delete " k " " n)
      (prime-or-delete! k n)
     )))

;;
;; rendering
;;
(r/defc debug-game < r/reactive []
  [:div {:class "debug"} "Game state: " (str (r/react game))])

(r/defc render-item < r/reactive [k n]
  "render item n in heap k"
  [:circle {:cx (+ 1 (* 2 k))
            :cy (+ 0.5 (- n 1))                 ;; dangling
;            :cy (- (- grid-size (- n 1)) 0.5)  ;; standing
            :r 0.5
            :id (str "[" k " " n "]")
            :fill (if (is-highlighted? k n) "#f00" "rgba(100,200,100,0.7)")
            :stroke "#000"
            :stroke-width "0.1" 
            :on-click (fn [e] (item-clicked e))
            }])

(r/defc render-heap < r/reactive [k n]
  [:g
   (map #(render-item k %) (range 1 (inc n)))]
  )

(defn draw-heap [k n]
  (if (> n 0)
    (render-heap k n)
))

(r/defc render-heaps < r/reactive []
  [:g (map draw-heap (iterate inc 0) (:heaps (r/react game)))]
  )

(r/defc render-toolbar < r/reactive []
  (let [game-state (r/react game)
        level (:level game-state)
        pairing (:pairing game-state)
        pair? (< pairing (max-pairing (:heaps game-state)))] 
    [:div {:class "controlls"}
     [:button {:on-click start!} "New game"]
     [:button {:on-click hint!} "Hint"]
     [:button {:on-click pair!} (if pair? 
                                  (if (= 0 pairing) "Pair" "Pair again") 
                                  "Separate")]
     [:select {:on-change change-level! :value level}
      [:option {:value 1} "Level 1"]
      [:option {:value 2} "Level 2"]
      [:option {:value 3} "Level 3"]
      [:option {:value 4} "Level 4"]
      [:option {:value 5} "Level 5"]
      ]])
)

(r/defc render-game < r/reactive []
  [:div
   [:h1 "Play NIM" ]
   (debug-game)
   (render-toolbar)
   [:div
    [:svg {:class "playfield"
           :width grid-size :height grid-size
           :viewBox (str "0 0 " grid-size " " grid-size)}
     (render-heaps)
     ]
    (render-toolbar)
    ]
   ])

(r/mount (render-game)
         (.getElementById js/document "game"))


(defn on-js-reload []
  (.log js/console "on-js-reload called"))


;; (def key-mapping {38 :up
;;                   40 :down
;;                   37 :left
;;                   39 :right
;;                   80 :pause
;;                   82 :restart })


;; (defonce key-watch
;;   (.addEventListener js/document "keydown"
;;                      #(when-let [cmd (get key-mapping (.-keyCode %))]
;;                         (handle-command! cmd))))

;; (defonce tick-watch
;;   (js/setInterval tick! tick-in-ms))
