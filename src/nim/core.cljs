(ns ^:figwheel-always nim.core
    (:require [rum :as r]
              [cljs.reader :as reader]))

(enable-console-print!)

(def grid-size 12)

(def scale 4)

(def base-unit "ex")

(defn unit
  [n]
  (str (* scale n) base-unit))

(def tick-in-ms 250)

;; utilities

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

(defn game-setup
  []
  "setup or restart the game"
  (let [heaps (rand-heaps 2 6 1 12)]
    {:primed nil
     :heaps heaps}))

;;
;; define game as the single? game state atom
;;
;; TODO: change to defonce
;;
(def game
  (atom (game-setup)))

(defn msb [n]
  "find the most significant bit in an integer"
  (.floor js/Math (/  (.log js/Math n) (.log js/Math 2)))
)

;;
;; game strategy
;;
(defn nim-sum [heaps]
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
            k (first ks)
            n (nth heaps k)
            n' (bit-xor n nsum)]
        (do
          (println (str "k = " k " n = " n " return " [k (- n n')]))
          [k (- n n')]
          ))
      (random-move heaps))))

;;
;; mutate game
;;
(defn start! []
  "start a new game"
  (swap! game game-setup))

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
            :cy (- (- grid-size (- n 1)) 0.5)
            :r 0.5
            :id (str "[" k " " n "]")
            :fill (if (is-highlighted? k n) "#f00" "rgba(0,0,0,0.5)")
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


(r/defc render-game < r/reactive []
  [:div
   (debug-game)
   [:h3 "Play NIM" ]
   [:div
    [:svg {:class "playfield"
           :width (unit grid-size) :height (unit grid-size)
           :viewBox (str "0 0 " grid-size " " grid-size)}
     (render-heaps)
     ]
    [:div {:class "controlls"}
     [:button {on-click start!} "New game"]
     [:button {on-click hint!} "Hint"]]
    ]
   ])

(r/mount (render-game)
         (.getElementById js/document "game"))

;; (r/defc render-game < r/reactive []
;;   [:div
;;    [:h3 (case (map (r/react game) [:running :alive])
;;           [true true] "Eat up - press [p] to pause"
;;           [false true] "Paused - press [p] to continue"
;;           (str "You are dead! You ate " (dec (count (:snake (r/react world)))) " apples!  Restart with [r]"))]
;;    [:div
;;     [:svg {:class "playfield"
;;            :width (unit grid-size) :height (unit grid-size)
;;            :viewBox (str "0 0 " grid-size " " grid-size)}
;;      (apple)
;;      (snake)]]
;;    [:div {:class "controlls"}
;;     [:button {:on-click restart!} "(R)estart"]
;;     [:button {:on-click toggle-pause!} "Toggle (P)ause"]]
;;    (debug)])







(defn on-js-reload []
  (.log js/console "on-js-reload called"))

;; helpers
;; (defn rand-in-size
;;   ([padding]
;;    (+ padding (rand-int (- grid-size (* 2 padding)))))
;;   ([]
;;    (rand-in-size 0)))

;; (defn rand-pos
;;   ([padding]
;;    (seq [(rand-in-size padding) (rand-in-size padding)]))
;;   ([]
;;    (rand-pos 0)))

;; (defn collide?
;;   [pos snake]
;;   (some #{pos} snake))

;; (defn rand-apple
;;   [snake]
;;   (first
;;     (remove
;;       #(collide? % snake)
;;       (repeatedly rand-pos))))

;; (defn hit-border?
;;   [pos]
;;   (or (some #(< (dec grid-size) %) pos)
;;       (some neg? pos)))

;; (def dir-offset {:up [0 -1]
;;                  :down [0 1]
;;                  :left [-1 0]
;;                  :right [1 0]})

;; (defn next-snake-head [snake dir]
;;   (map + (first snake) (dir dir-offset)))

;; the world

;; (defn world-setup
;;   []
;;   (let [snake (seq [(rand-pos 2)])
;;         apple (rand-apple snake)
;;         dir (rand-nth (keys dir-offset))]
;;     {:running false
;;      :alive true
;;      :snake snake
;;      :dir dir
;;      :apple apple}))


;(defonce world (atom (world-setup)))

;; manipulation of the world

;; (defn toggle-pause!
;;   []
;;   (swap! world
;;          #(assoc % :running
;;                  (and
;;                   (:alive %)
;;                   (not (:running %))))))

;; (defn restart!
;;   []
;;   (swap! world world-setup))


;; (defn change-dir!
;;   [new-dir]
;;   (swap! world assoc :dir new-dir))

(def key-mapping {38 :up
                  40 :down
                  37 :left
                  39 :right
                  80 :pause
                  82 :restart })

;; (defn handle-command!
;;   [cmd]
;;   (case cmd
;;     :pause (toggle-pause!)
;;     :restart (restart!)
;;     (change-dir! cmd)))

;; (defn next-world
;;   [world]
;;   (let [{:keys [snake dir apple]} world
;;         next-head (next-snake-head snake dir)
;;         eat? (= next-head (:apple world))
;;         body (if eat? snake (butlast snake))
;;         alive (and (not (hit-border? next-head))
;;                    (not (collide? next-head snake)))
;;         next-snake (conj body next-head)
;;         next-apple (if eat?
;;                      (rand-apple next-snake)
;;                      apple)]
;;     (assoc world
;;            :running (and alive (:running world))
;;            :alive alive
;;            :snake next-snake
;;            :apple next-apple
;;            )))

;; (defn tick!
;;   []
;;   (when (:running @world)
;;     (swap! world next-world)))

;; render the world
;; (r/defc debug < r/reactive []
;;   [:div {:class "debug"} "Debug of world: " (str (r/react world))])

;; (r/defc apple < r/reactive []
;;   (dot (:apple (r/react world)) "red"))

;; (r/defc snake < r/reactive []
;;   (let [snake (:snake (r/react world))
;;         head (first snake)
;;         tail (rest snake)]
;;     (if (empty? tail)
;;       (dot head "green")
;;       [:path {:d (let [cmb (fn [[x y]] (str x "," y))]
;;                    (str
;;                      "M" (cmb head)
;;                      (apply str (map #(str " L" (cmb %)) tail))))
;;               :style {:fill "none"
;;                       :stroke "green"
;;                       :stroke-width 1
;;                       :stroke-linecap "round"
;;                       :stroke-linejoin "round"}}])))

;; (r/defc render-world < r/reactive []
;;   [:div
;;    [:h3 (case (map (r/react world) [:running :alive])
;;           [true true] "Eat up - press [p] to pause"
;;           [false true] "Paused - press [p] to continue"
;;           (str "You are dead! You ate " (dec (count (:snake (r/react world)))) " apples!  Restart with [r]"))]
;;    [:div
;;     [:svg {:class "playfield"
;;            :width (unit grid-size) :height (unit grid-size)
;;            :viewBox (str "0 0 " grid-size " " grid-size)}
;;      (apple)
;;      (snake)]]
;;    [:div {:class "controlls"}
;;     [:button {:on-click restart!} "(R)estart"]
;;     [:button {:on-click toggle-pause!} "Toggle (P)ause"]]
;;    (debug)])

;; ;; setup

;; (r/mount (render-world)
;;          (.getElementById js/document "app"))

;; (defonce key-watch
;;   (.addEventListener js/document "keydown"
;;                      #(when-let [cmd (get key-mapping (.-keyCode %))]
;;                         (handle-command! cmd))))

;; (defonce tick-watch
;;   (js/setInterval tick! tick-in-ms))
