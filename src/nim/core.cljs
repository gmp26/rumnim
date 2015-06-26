(ns ^:figwheel-always nim.core
    (:require [rum :as r]
              [cljs.reader :as reader]
              [cljsjs.react]
              [goog.events :as events]
              [goog.history.EventType :as EventType]
              [secretary.core :as secretary :refer-macros [defroute]]
              [clojure.string :as str]
              )
    (:import goog.History))

(enable-console-print!)


;; Things to sort out:
;; Playhead is inconsistent
;; Need to retain player info in history
;; Forward and back buttons might be better (Undo and Redo?)



;;
;; define level-spec as an atom for game level configuration
;;
(def level-spec
  (atom [2 6 1 15]))

(declare game-init)


;;
;; basic hashbang routing to configure some game options
;;

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

(def unit (Math.round (/ 300 12)))

(defn px [x]
  (str (* x unit) "px"))

;; debug
#_(defn deb [x & msg] (do (if msg (println msg x) (println x)) x))

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

(declare start!)
(declare continue!)
(declare pair!)
(declare pair-label)

(r/defc instructions < r/reactive [] 
  [:div
   [:p {:key "i1"} "Take turns to remove as many drips as you like from a single drip trail."]
   [:p {:key "i2"} "Click once to choose, and once again in the same place to confirm. Take the very last drip to win the game."]
   [:p {:key "i3"} "Press 'New Game' to start, you then have 20 seconds to make the first move before
Al the computer loses patience and starts anyway."]
   [:button {:on-click start! :on-touch-end start! :key "i4" } "New game"]
   [:button {:on-click continue! :on-touch-end continue! :key "i5" } "OK"]
   [:p {:key "i6"} "The Pairer button can help you calculate a winning move by rearranging the drips. Each press makes a different arrangement."]
   [:button {:on-click pair! :on-touch-end pair! :key "i7" } (pair-label)]
   ])

(r/defc well-done < r/reactive []
  [:div
   [:p {:key "i1"} "Well done."] 
   [:p {:key "i2"} "If you understand the winning strategy you should be able to beat Al whenever you like! If not, keep trying till you've pinned it down."]
   [:p {:key "i3"} "We'd love to hear your explanation of how to win. Email "
    [:a {:href "mailto:wild@maths.org"} "wild@maths.org"]
    " with your thoughts."]
   [:button {:on-click start! :on-touch-end start! :key "i4" } "New game"]
   [:button {:on-click continue! :on-touch-end continue! :key "i5" } "OK"]
])

(r/defc try-again < r/reactive []
  [:div
   [:p {:key "i1"} "Bad luck, but try again."]
   [:p {:key "i2"} "You may find it helpful to study what Al does. Try using the Pairer button after he's made a move to see what's special about the losing positions he leaves you in."]
   [:button {:on-click start! :on-touch-end start! :key "i3" } "New game"]
   [:button {:on-click continue! :on-touch-end continue! :key "i4" } "OK"]
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

(defn game-setup [new-level-spec]
  "setup or restart the game"
  (let [heaps (apply rand-heaps new-level-spec)]
    (do
      (.initializeTouchEvents js/React true)
      {
       :primed nil
       :heaps heaps
       :pairing 0
       :hovered nil
       :status :none
       :flash-key :timer
       :countdown 20
       :best nil
       :score [0 0]
       :playback false
       :playhead 0
       })))

(def game (atom (game-setup @level-spec)))

;;
;; Don't confuse with browser history! This records the history of
;; heap state since the last new game by listening on game atom mutations
;;
(def game-history (atom [@game]))

(add-watch 
 game :history
 (fn [_ _ _ new-state]
   (let [last-state (last @game-history)] 
     (if (and (not (:playback new-state)) 
              (or (not= (:heaps last-state) (:heaps  new-state))
                  (not= (:primed last-state) (:primed  new-state))
                  (not= (:pairing last-state) (:pairing  new-state)))
              )
       (swap! game-history conj 
              (assoc new-state :play-head
                     (inc (count @game-history))))))))

;;
;; define game as the single? game state atom
;;
;; TODO: change to defonce
;;
(defn game-init [key levels old-levels new-level-spec]
  (do
    (prn "reset")
    (reset! game (game-setup new-level-spec))))

;;
;; I don't see any way to make this reloadable, but at least we
;; don't have to call it during debug
;;
(defn start! [e]
  "start a new game"
  (let [heaps (apply rand-heaps @level-spec)]
    (do
      (.preventDefault e)
      (swap! game #(assoc % 
                     :primed nil
                     :heaps heaps
                     :pairing 0
                     :hovered nil
                     :status :none
                     :best nil
                     :countdown 20 
                     :status :none
                     :flash-key :timer
                     :playback false
                     :playhead 0))
      (reset! game-history [@game])
)))

(defn show-frame! [e]
  (do
    (.preventDefault e)
    (.log js/console "undo count=" (count @game-history))
    (.log js/console "value=" (-> e .-target .-value))
    (let [gh @game-history
          ghc (count gh)
          slider (int (-> e .-target .-value))]
      (if (and (> ghc 0)
                 (< slider ghc))
        (let [nth-state (nth gh slider)] 
          (swap! game #(assoc %
                         :heaps (:heaps nth-state)
                         :pairing (:pairing nth-state)
                         :primed (:primed nth-state)
                         :playhead slider)))))))

(defn playback! [e]
  (do
    (.preventDefault e)
    (.debug js/console (.-target e))
    (.debug js/console (-> e .-target .-value))
    (if (:playback @game)
      (do
        (swap! game #(assoc %
                       :flash-key (:playback @game)
                       :playback false))
        (reset! game (last @game-history)))
      (swap! game #(assoc %
                     :flash-key :playback
                     :playback (:flash-key @game)))

      )))

(defn playback []
  "Playback class"
  (if (:playback @game) "playback" ""))

(defn flashes [a-key]
  (condp = a-key 
    :none ""
    :als "Al's turn"
    :yours "Your turn"
    :timer (str "Move or let Al go in " (:countdown @game) " s")
    :game-over "Game Over"
    :playback "Playback"))

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

(def i-gp -0.1)
(def o-gp 1.2)

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
        [k, (inc n)]))))
;;
;; mutate game
;;
(defn continue! [e]
  "continue game - hide popover"
  (do
    (.preventDefault e)
    (swap! game #(assoc %
                   :status :none))))

(defn change-level! [event]
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

(defn at-k-leave-n! [k n]
  (do
    #_(println "at " k " leave " n)
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
    (if (or (= k nil) (= n nil))
      [k n]
      (do (prime! k (at-k-leave-n! k n))
          #_(println (str "best move" [k n]))
          [k n]))))

(defn hint! []
  (if (= (:status @game) :instructions)
    (swap! game #(assoc % :status :none ))
    (swap! game #(assoc % :status :instructions))
    ))

(defn from-k-take-n! [k n]
  (let [heaps (:heaps @game)
        new-heaps (heaps-at-k-take-n heaps k n)]
    (do 
      (swap! game #(assoc % :heaps new-heaps))
      #_(println "taking " n " from " k))))

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

(defn mouse-out! [k n]
  "mouse-out all"
  (swap! game #(assoc % :hovered nil)))

(defn mouse-over! [k n]
  "hover over the nth item in heap k"
  (let [fkey (:flash-key @game)]
    (if (or (= fkey :yours) (= fkey :timer))
      (swap! game #(assoc % :hovered [k n])))))

(defn next-pairing [p] 
  (inc p))

(defn pair! [e]
  "pair items together somehow"
  (let [heaps (:heaps @game)
        p (:pairing @game)
        pairing (if (>= p (max-pairing heaps)) 0 (next-pairing p))]
    (do
      (.preventDefault e)
      (swap! game #(assoc % :pairing pairing)))
    )
)


;;
;; event handling
;;

#_(defn you-clicked-on [k n] (.log js/console (str "you clicked on " k " " n)))

(defn item-clicked [event]
  "read [col row] form from event.target.id string"
  (let [[k n] (reader/read-string (-> event .-target .-id))]
    (do
      (.preventDefault event)
      #_(you-clicked-on k n)
      (prime-or-delete! k n)
     )))

(defn item-over [event]
  "read [col row] form from event.target.id string"
  (let [[k n] (reader/read-string (-> event .-target .-id))]
    (do
      (.preventDefault event)
      (mouse-over! k n)
     )))

(defn item-out [event]
  "read [col row] form from event.target.id string"
  (let [[k n] (reader/read-string (-> event .-target .-id))]
    (do
      (.preventDefault event)
      (mouse-out! k n)
     )))
;;
;; rendering
;;
(r/defc debug-game < r/reactive [g]
  [:div {:class "debug"} (str g)])


(r/defc render-html-item < r/reactive [pairing k n i]
  "render item i in kth heap"
  (let [g @game
        key (str "[" k " " i "]")]
    [:div 
     
     {:id key
      :class "blobs"
      :on-click (fn [e] (item-clicked e))
      :on-touch-end (fn [e] (item-clicked e))
      :on-mouse-over (fn [e] (item-over e))
      :on-mouse-out (fn [e] (item-out e))
      :style 
      {:left (px (+ 0.3 (* 2 k)))
       :top (px (+ (item-offset pairing n i) 0.8))
       :width (px (* radius 2))
       :height (px (* radius 2))
       :background-color 
       (if (= (:flash-key g) :als)
         (if (is-highlighted? k i) 
           "rgba(255, 180, 0, 1)" 
           "rgba(100, 180, 255, 1)")
         (if (is-highlighted? k i) 
           "rgba(235, 100, 0, 1)" 
           "rgba(80, 100, 255, 1)")
         )
       }}
     
     ]))


(r/defc render-html-heap < r/reactive [pairing k n]
  [:div 
   (map #(r/with-props render-html-item pairing k n % :rum/key (str "h" %)) (range n))]
  )

(defn draw-html-heap [pairing k n]
  (if (> n 0)
    (r/with-props render-html-heap pairing k n :rum/key (str "hps" k n))
))

(r/defc render-html-heaps < r/reactive [pairing heaps]
  [:div (map-indexed #(draw-html-heap pairing %1 %2) heaps)]
  )

(defn pair-label []
  "Pairer"
)  
  ;; (let [pairing (:pairing @game)
  ;;       pair? (< pairing (max-pairing (:heaps @game)))]
  ;;   (if pair? 
  ;;       (if (= 0 pairing) "Pair" "Pair again") 
  ;;       "Separate")))

(r/defc render-toolbar < r/reactive []
  (let [game-state (r/react game)
        level (:level game-state)] 
    [:div {:class "controls"}
     [:button {:key "stb" :on-click start! :on-touch-end start!} "New game"]
     [:button {:key "htb" :on-click hint! :on-touch-end hint!} "Rules"]
     [:button {:key "prb" :on-click pair! :on-touch-end pair!} (pair-label)]
])
)

(defn player-score! []
  (nth (:score @game) 0))

(defn computer-score! []
  (nth (:score @game) 1))

(r/defc render-flash < r/reactive [flash-msg score]
  (let [[als yours] score] 
    [:div.flash-box
     [:span.msg {:key "f1"} flash-msg]
     [:span.score {:key "f2"} "Al: " als " You: " yours]
     ]))

(defn divider-offset [pairing]
  (nth [0 63 132 477] pairing))

(r/defc render-html-board < r/reactive [pairing heaps flash-msg score]
  [:div.bordered
   {:style {:background-position (str  0 "px " (divider-offset pairing) "px")}}
   (render-flash flash-msg score)
   [:div.playfield  
    [:div.pad
     (render-html-heaps pairing heaps)]]])

(r/defc para < r/reactive [text]
  [:p {:class "msg"} text])


(r/defc render-popover < r/reactive []
  (let [status ((:status (r/react game)) messages)
        visible (:visible status)
        title (:title status)
        body (:body status)]
    (if visible
      [:div {:class "popover" :key "popover"}
       [:div {:class "title" :key "title"} title]
       (r/with-props body :rum/key "body")]
      )))

(r/defc render-footer < r/reactive []
  (let [game-state (r/react game)
        level (:level game-state)] 
    [:div {:class "footer"}
     [:button {:key "plb" :on-click playback! :on-touch-end playback! :class (playback)} "Playback"]
     (if (:playback @game)
       (do [:div [:button [:i
                           {:class "fa fa-camera-retro"}]
                  ]
            [:input {:id "slider" :key "inp"
                     :type "range"
                     :min 0
                     :value (:playhead @game)
                     :max (count @game-history)
                     :on-change show-frame!}]]))
])
)

(r/defc render-game < r/reactive []
  (let [g (r/react game)
        pairing (:pairing g)
        heaps (:heaps g)
        flash-msg (flashes (:flash-key g))
        score (:score g)]
    [:div
     [:h1 {:key "g1"} "Drips" ]
     (r/with-props render-toolbar :rum/key "toolbar")
     [:div {:key "g2" :class "board"}
      (r/with-props render-html-board pairing heaps flash-msg score :rum/key "board")
      (r/with-props render-popover :rum/key "popup")
      ]
     (r/with-props render-footer :rum/key "footer")
     ])
)

(r/mount (render-game)
         (.getElementById js/document "game"))

#_(defn on-js-reload [] (.log js/console "on-js-reload called"))


(defn show-a-winner! []
  (if (and (not= :game-over (:flash-key @game)) (= 0 (reduce + (:heaps @game))))
    (let [loser (:flash-key @game)
          [yu al] (:score @game)
          [score status]  (if (= :yours loser)
                         [[yu (inc al)] :al-won]
                         [[(inc yu) al] :you-won]
                         )]
      (swap! game #(assoc %
                     :status status 
                     :flash-key :game-over
                     :score score)))))

(defn tick! []
  (let [g @game
        fk (:flash-key g)
        timer (:countdown g)]
    (if (and
         (not= fk :playback)
         (not= fk :game-over) 
         timer)
      (cond  
       (= timer 0) (let [[k n] (show-best-move!)]
                     (swap! game #(assoc %
                                    :countdown (- timer 1)
                                    :flash-key :als
                                    :best [k n])))
       (= timer -4) (do 
                      (make-best-move! (:best @game))
                      (if (= 0 (reduce + (:heaps @game)))
                        (show-a-winner!))
                      )
       :else (swap! game #(assoc % :countdown (- timer 1)))
       ))))

(def one-second 1000)

(defonce tick-watch
  (js/setInterval tick! one-second))

(add-watch level-spec "akey" game-init)
