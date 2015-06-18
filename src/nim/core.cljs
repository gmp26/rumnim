(ns ^:figwheel-always nim.core
    (:require [rum :as r]
              [cljs.reader :as reader]
              [clojure.set :refer [difference]]))

(enable-console-print!)

(def grid-size 12)

(def tick-in-ms 250)

(def unit (identity (/ 300 12)))

(defn px [x]
  (str (* x unit) "px"))

;; debug
(defn deb [x & msg]
  (do (if msg (println msg x) (println x)) x))

;;
;; utilities
;;

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

; sets the limit of pairing wrap around
(defn max-pairing [heaps]
  (msb (apply max heaps))
  )

;;
;; rounds and levels
;;
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


(def level-spec 
  [2 6 1 15])

(declare start!)
(declare continue!)
(declare pair!)
(declare pair-label)

(r/defc instructions < r/reactive [] 
  [:div
   [:p "Take turns to remove as many drips as you like from a single drip trail."]
   [:p "Click once to choose, and once again in the same place to confirm. Take the very last drip to win the game."]
   [:p "Press 'New Game' to start, you then have 20 seconds to make the first move before
Al the computer loses patience and starts anyway."]
   [:button {:on-click start!} "New game"]
   [:button {:on-click continue!} "OK"]
   [:p "The 'Pair/Pair again/Separate' button can help you calculate a winning move."]
   [:button {:on-click pair!} (pair-label)]
   ])

(r/defc well-done < r/reactive []
  [:div
   [:p "Well done."] 
   [:p "If you understand the winning strategy you should be able to beat Al whenever you like!"]
   [:p "We'd love to hear your explanation of how to win. Emailwild@maths.org with your thoughts."]
   [:button {:on-click start!} "New game"]
   [:button {:on-click continue!} "OK"]
])

(r/defc try-again < r/reactive []
  [:div
   [:p "Bad luck, but try again."]
   [:p "You may find it helpful to analyse what Al does. Try using the Pair button after he's made a move."]
   [:p "See if you can work out how he leaves you in a losing position."]
   [:button {:on-click start!} "New game"]
   [:button {:on-click continue!} "OK"]
])

(def messages
  {:none {:visible false}
   :instructions {:visible true
                  :title "Rules"
                  :body instructions 
                  }
   :you-won {:visible true
             :title "You won!"
             :body well-done}
   :al-won {:visible true
            :title "Al won!"
            :body try-again}
   })

(defn game-setup []
  "setup or restart the game"
  (let [heaps (apply rand-heaps level-spec)]
    {:primed nil
     :heaps heaps
     :pairing 0
     :score [2 3]
     :hovered nil
     :status :none
     :flash-key :none
     :countdown nil
     :best nil
}))

;;
;; define game as the single? game state atom
;;
;; TODO: change to defonce
;;
(defonce game
  (atom (game-setup)))

(defn flashes [a-key]
  (condp = a-key 
    :none ""
    :als "Al's turn"
    :yours "Your turn"
    :timer (str "Move or let Al go in " (:countdown @game) " s"
)
    :game-over "Game Over"))

;;
;; layout
;;
(def radius 0.5)

(defn grouper [n]
 "needed for groups calculation. (grouper 9) => (9 4 2 1)"
 (take-while #(not= 0 %) (iterate #(bit-shift-right % 1) n)))

(defn wrap-groups [aseq]
  (conj () aseq))

(defn groups [pairing n]
  "the power 2 groups to display for each pairing"
  (map wrap-groups (keep-indexed #(if (<= %1 pairing) 
                                (if (< %1 pairing) 
                                  (mod  %2 2)
                                  %2)) (grouper n))))

(def i-gp 0.1)
(def o-gp 1)

(defn unwrap [alist] (apply concat alist))

(defn expanded-groups [pairing n]
  (let [egs (map-indexed 
             #(range 0 (* (Math.pow 2 %1) (first %2))) 
             (groups pairing n))]

    (map #(partition-all (Math.pow 2 pairing) %) egs)))

(defn group-offsets [pairing n]
  (map-indexed 
   (fn [gx g] 
     (let [gstart (- (Math.pow 2 gx) 2)
           ioff #(* i-gp %)
           glen (inc (Math.pow 1.4 gx))
           goff (map-indexed 
                 (fn [g,m]
                   (map-indexed
                    (if (not= 0 pairing)
                      #(+ (ioff %1) 
                          (+ (* 0.7 (+ g -0.5)) glen (* o-gp gstart))
                          %2)
                      #(+ (ioff %1) 
                          (+ (* 0.3 g) 0.8 )
                          %2))
                    m)) 
                 g)]
       goff)) (expanded-groups pairing n)))

(defn item-offsets [pairing n]
  (flatten (group-offsets pairing n)))

(defn item-offset [pairing k n]
  (nth (item-offsets pairing k) n))


;;
;; game strategy
;;
(defn nim-sum [heaps]
  "Calculate the nim-sum of all heaps by xoring them all together"
  (reduce bit-xor heaps))

(defn take-one-moves [heaps]
  "return a list of possible random moves which take 1, or nil if heaps are all empty"
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

(defn random-go [heaps]
  (let [ks (seq (flatten (keep-indexed #(if (> %2 0) [%1]) heaps)))]
    (if (> (count ks) 0)
      (let [k (rand-nth ks)
            n (rand-int (nth heaps k))]
        [k, n]))))
;;
;; mutate game
;;
(defn start! []
  "start a new game"
  (do
    (swap! game game-setup)
    (swap! game #(assoc % 
                   :countdown 20 
                   :status :none
                   :flash-key :timer))))

(defn continue! []
  "continue game - hide popover"
  (swap! game #(assoc %
                 :status :none)))

(defn change-level! [event]
  (.debug js/console (-> event .-target .-value))
  (let [new-level (-> event .-target .-value int)]
    (swap! game #(assoc % :level new-level))))

(defn is-primed? [k n]
  "is item at col k, height n, primed for deletion?"
  (let  [[k' n' :as p] (:primed @game)]  
    (and p (= k k') (= n n'))))

(defn is-highlighted? [k n]
  "is item at col k, height n, highlighted for deletion?"
  (let [[k' n' :as p] (:primed @game)
        [kh nh :as h] (:hovered @game)]
    (or 
     (and h (= k kh) (>= n nh)) 
     (and p (= k k') (>= n n')))))

(defn un-prime! []
  "unprime all"
  (swap! game #(assoc % :primed nil)))

(defn prime! [k n]
  "prime the nth item in heap k"
  (swap! game #(assoc % :primed [k n])))

;
; problem area starts
;
(defn at-k-leave-n! [k n]
  (do
    (println "at " k " leave " n)
    (- (nth (:heaps @game) k) (- n 0))))

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
      (random-go heaps))))

(defn show-best-move! []
  "highlight the next best move"
  (let [[k n] (get-best-move (:heaps @game))]
    (do (prime! k (at-k-leave-n! k n))
        (println (str "best move" [k n]))
        [k n])))

(defn hint! []
  (do
    (swap! game #(assoc % 
                   :status :instructions
                   :countdown nil
                   :flash-key :none
                   ))))

(defn from-k-take-n! [k n]
  (let [heaps (:heaps @game)
        new-heaps (heaps-at-k-take-n heaps k n)]
    (do 
      (swap! game #(assoc % :heaps new-heaps))
      (println "taking " n " from " k))))

(declare show-a-winner!)

(defn prime-or-delete! [k n]
  "item k n is to be primed for deletion, or deleted if already primed"
  (if (or (= (:flash-key @game) :yours)
          (= (:flash-key @game) :timer)) 
    (if (is-primed? k n)
      (do
        (un-prime!)
        (swap! game #(assoc % 
                       :flash-key :als
                       :countdown 2))
        (from-k-take-n! k (at-k-leave-n! k n))
        (show-a-winner!))
      (prime! k n))))

(defn make-best-move! [[k n]]
  (do 
    (swap! game #(assoc % 
                   :countdown nil
                   :flash-key :yours
                   :pairing 0
                   :primed nil))
    (from-k-take-n! k n)))
;
; problem area ends
;

(defn mouse-out! [k n]
  "mouse-out all"
  (swap! game #(assoc % :hovered nil)))

(defn mouse-over! [k n]
  "hover over the nth item in heap k"
  (if (= (:flash-key @game) :yours)
    (swap! game #(assoc % :hovered [k n]))))

(defn next-pairing [p] 
  (inc p))

(defn pair! []
  "pair items together somehow"
  (let [heaps (:heaps @game)
        p (:pairing @game)
        pairing (if (>= p (max-pairing heaps)) 0 (next-pairing p))]
    (swap! game #(assoc % :pairing pairing))
    )
)

;;
;; event handling
;;

(defn you-clicked-on [k n]
  (println (str "you clicked on " k " " n)))

(defn item-clicked [event]
  "read [col row] form from event.target.id string"
  (let [[k n] (reader/read-string (-> event .-target .-id))]
    (do
      #_(you-clicked-on k n)
      (prime-or-delete! k n)
     )))

(defn item-over [event]
  "read [col row] form from event.target.id string"
  (let [[k n] (reader/read-string (-> event .-target .-id))]
    (do
      (mouse-over! k n)
     )))

(defn item-out [event]
  "read [col row] form from event.target.id string"
  (let [[k n] (reader/read-string (-> event .-target .-id))]
    (do
      (mouse-out! k n)
     )))
;;
;; rendering
;;
(r/defc debug-game < r/reactive []
  [:div {:class "debug"} "Game state: " (str (r/react game))])

(r/defc render-item < r/reactive [k n]
  "render item n in kth heap"
  (let [g @game
        pairing (:pairing g)
        heaps (:heaps g)]
    [:circle {:cx (+ 1 (* 2 k))
              :cy (item-offset pairing (nth heaps k) n)
              :r radius
              :id (str "[" k " " n "]")
              :fill (if (is-highlighted? k n) "rgba(255,80,100,1)" "rgba(100,150,255,1)")
              :on-click (fn [e] (item-clicked e))
              :class "blobs"
              }]))

(r/defc render-html-item < r/reactive [k n]
  "render item n in kth heap"
  (let [g @game
        pairing (:pairing g)
        heaps (:heaps g)
        key (str "[" k " " n "]")]
    [:div 
     
     {:id key
      :class "blobs"
      :key key 
      :on-click (fn [e] (item-clicked e))
      :on-mouse-over (fn [e] (item-over e))
      :on-mouse-out (fn [e] (item-out e))
      :style 
      {:left (px (+ 0.3 (* 2 k)))
       :top (px (+ (item-offset pairing (nth heaps k) n) 0.6))
       :width (px (* radius 2))
       :height (px (* radius 2))
       :background-color 
       (if (= (:flash-key g) :als)
         (if (is-highlighted? k n) 
           "rgba(235, 100, 0, 1)" 
           "rgba(80, 100, 255, 1)")
         (if (is-highlighted? k n) 
           "rgba(255, 180, 0, 1)" 
           "rgba(100, 180, 255, 1)"))
       }}
     
     ]))

(r/defc render-heap < r/reactive [k n]
  [:g
   (map #(render-item k %) (range n))]
  )

(r/defc render-html-heap < r/reactive [k n]
  [:div {:key (str "heap" k "-" n)}
   (map #(render-html-item k %) (range n))]
  )

(defn draw-heap [k n]
  (if (> n 0)
    (render-heap k n)
))

(defn draw-html-heap [k n]
  (if (> n 0)
    (render-html-heap k n)
))

(r/defc render-heaps < r/reactive []
  [:g (map-indexed draw-heap (:heaps (r/react game)))]
  )

(r/defc render-html-heaps < r/reactive []
  [:div (map-indexed draw-html-heap (:heaps (r/react game)))]
  )

(defn pair-label []
  (let [pairing (:pairing @game)
        pair? (< pairing (max-pairing (:heaps @game)))]
    (if pair? 
        (if (= 0 pairing) "Pair" "Pair again") 
        "Separate")))

(r/defc render-toolbar < r/reactive []
  (let [game-state (r/react game)
        level (:level game-state)] 
    [:div {:class "controls"}
     [:button {:on-click start!} "New game"]
     [:button {:on-click hint!} "Rules"]
     [:button {:on-click pair!} (pair-label)]
     #_[:select {:on-change change-level! :value level}
      [:option {:value 1} "Level 1"]
      [:option {:value 2} "Level 2"]
      [:option {:value 3} "Level 3"]
      [:option {:value 4} "Level 4"]
      [:option {:value 5} "Level 5"]
      ]])
)

(r/defc render-svg-board < r/reactive [gsize]
  (let [gsz (* 1.8 gsize)]
    [:div {:class "bordered"}
     [:svg {:class "playfield"
            :width gsize :height gsz
            :viewBox (str "0 0 " gsize " " gsz)}
      (render-heaps)]])
)

(defn player-score! []
  (nth (:score @game) 0))

(defn computer-score! []
  (nth (:score @game) 1))

(r/defc render-flash < r/reactive []
  (let [g (r/react game)
        flash-msg  (flashes (:flash-key g))
        [yours als] (:score g)]
    [:div.flash-box
     [:span.msg flash-msg]
     [:span.score "Al: " als " You: " yours]
]))

(defn divider-offset [g]
  (nth [0 61 125 477] (:pairing g)))

(r/defc render-html-board < r/reactive []
  [:div.bordered
   {:style {:background-position-y (str (divider-offset (r/react game)) "px")}}
   (render-flash)
   [:div.playfield 
    [:div.pad
     (render-html-heaps)]]])

(r/defc para < r/reactive [text]
  [:p {:class "msg"} text])


(r/defc render-popover < r/reactive []
  (let [status ((:status (r/react game)) messages)
        visible (:visible status)
        title (:title status)
        body (:body status)]
    (if visible
      [:div {:class "popover" :key "popover"}
       [:div {:class "title"} title]
       (body {:key "body"})]
      )))

(r/defc render-game < r/reactive []
  [:div
   [:h1 "Drips" ]
   (debug-game)
   (render-toolbar)
   [:div
    (render-html-board)
    (render-popover)
    ]
   ]
)

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


(defn show-a-winner! []
  (if (= 0 (count (:heaps @game)))
    (let [winner (:flash-key @game)
          [yu al] (:score @game)
          [score status]  (if (= :yours winner)
                         [[(inc yu) al] :you-won]
                         [[yu (inc al)] :al-won])]
      (swap! game #(assoc %
                     :status status 
                     :flash-key :game-over
                     :score score)))))


(defn tick! []
  (let [timer (:countdown @game)]
    (do
      (println (str "tick " timer))
      (if timer
        (do
          (swap! game #(assoc % :countdown (- timer 1)))
          (cond  
           (= timer 0) (do
                         (let [[k n] (show-best-move!)]
                           (if (or (= nil k) (= nil n))
                             (show-a-winner!)
                             (do
                               (println (str "best move is: " [k n]))
                               (swap! game #(assoc % 
                                              :flash-key :als
                                              :best [k n]))))))
           (= timer -3) (make-best-move! (:best @game))
           :else (println timer))
          )))))

(def one-second 1000)
  
(defonce tick-watch
  (js/setInterval tick! one-second))


(start!)
