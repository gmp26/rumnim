(ns ^:figwheel-always nim.core
    (:require [rum.core :as r]
              [cljs.reader :as reader]
              [cljsjs.react]
              [routing.core :as routing]))

(enable-console-print!)

;; Sound
;; (def ^:constant sound-src "/sounds/bell.mp3")
;; (def sound (js/Audio. sound-src))
;; (defn play-sound []
;;   (do
;;     (set! (.-src sound) sound-src)
;;     (.play sound)))

;; Sounds
(def ^:constant drip "assets/drip.mp3")
(def ^:constant drips "assets/pair.mp3")
(defn play-sound [mp3]
  (let [sound (js/Audio. mp3)]
    (do (set! (.-src sound) mp3)
        (.play sound))))

(declare game-init)

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


(defn foo [x y]
  (+ 3 (* x y)))

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

(defn game-setup [new-level-spec]
  "setup or restart the game"
  (let [heaps (apply rand-heaps new-level-spec)]
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
     }))

(def game (atom (game-setup @routing/level-spec)))

(defn big-enough [game]
  (> (apply max (:heaps game)) 4))

;;
;; Don't confuse with browser history! This records the history of
;; heap state since the last new game by listening on game atom mutations
;;
(def game-history (atom []))

(declare replay-flash)

(add-watch
 game :history
 (fn [_ _ _ new-state]
   (let [last-state (last @game-history)]
     (if (and
          (not (:playback new-state))
          (or (not= (:heaps last-state) (:heaps  new-state))
              (not= (:primed last-state) (:primed  new-state))
              (and
               (not= (:flash-key last-state) (:flash-key  new-state))
               (not= (:flash-key last-state) (replay-flash (:flash-key  new-state))))
              (not= (:pairing last-state) (:pairing  new-state))))
       (swap! game-history conj
              (assoc new-state
                :playhead (count @game-history)
                :flash-key (replay-flash (:flash-key new-state))
                :playback (:flash-key new-state)
                :hovered nil
                ))))))

;;
;; define game as the single? game state atom
;;
;; TODO: change to defonce
;;
(defn game-init [key levels old-levels new-level-spec]
  (reset! game (game-setup new-level-spec)))

;;
;; I don't see any way to make this reloadable, but at least we
;; don't have to call it during debug
;;
(defn start! [e]
  "start a new game"
  (let [heaps (apply rand-heaps @routing/level-spec)]
    (do
      (.preventDefault e)
      (.stopPropagation e)
      (swap! game #(assoc %
                     :primed nil
                     :heaps heaps
                     :pairing 0
                     :hovered nil
                     :status :none
                     :best nil
                     :countdown 20
                     :flash-key :timer
                     :playback false
                     :playhead 0))
      (reset! game-history [])
)))


(defn safe-dec [val limit] (max (- val 1) limit))
(defn safe-inc [val limit] (min (inc val) limit))

(defn saved-game [current key-action history]
  "retrieve a saved game depending on the replay button pressed"
  (let [endx (safe-dec (count history) 0)]
    (condp = key-action
      :first (nth history 0)
      :back  (nth history (safe-dec (:playhead current) 0))
      :next  (nth history (safe-inc (:playhead current) endx))
      :last  (nth history endx))))

(defn show-frame! [key-action]
  (swap! game saved-game key-action @game-history))

(defn first! [e]
  (.preventDefault e)
  (.stopPropagation e)
  (show-frame! :first))

(defn back! [e]
  (.preventDefault e)
  (.stopPropagation e)
  (show-frame! :back))

(defn next! [e]
  (.preventDefault e)
  (.stopPropagation e)
  (show-frame! :next))

(defn last! [e]
  (.preventDefault e)
  (.stopPropagation e)
  (show-frame! :last))

(defn replay-flash [key]
  (condp = key
    :none :playback
    :als  :replay-als
    :yours :replay-yours
    :timer :replay-yours
    key key
    ))

(defn playback! [e]
  (do
    (.preventDefault e)
    (.stopPropagation e)
    (if (:playback @game)
      (do
        (reset! game (last @game-history))
        (swap! game #(assoc %
                       :playback false
                       :flash-key (:playback @game))))
      (show-frame! :first)
      #_(swap! game #(assoc %
                     :flash-key (replay-flash (:flash-key @game))
                     :playback (:flash-key @game)
                     )))
    ))

(defn flashes [a-key]
  (condp = a-key
    :none ""
    :als "Al's turn"
    :replay-als "Replaying Al's turn"
    :yours "Your turn"
    :replay-yours "Replaying Your turn"
    :timer (str "Move or let Al go in " (:countdown @game) " s")
    :game-over "Game Over"
    :playback "Replaying"
    a-key (prn "flashes barf!" a-key)))

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
    (.stopPropagation e)
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
;;
;; Do these atomically!
;;
        #_(un-prime!)
        #_(from-k-take-n! k (at-k-leave-n! k n))

        (let [heaps (:heaps @game)
              new-heaps (heaps-at-k-take-n heaps k (at-k-leave-n! k n))]
          (swap! game #(assoc %
                         :flash-key :als
                         :countdown 2
                         :primed nil
                         :heaps new-heaps)))
        (play-sound drip)
        (show-a-winner!))
      (prime! k n))))

(defn make-best-move! [[k n]]
  (do
    (let [heaps (:heaps @game)
          new-heaps (heaps-at-k-take-n heaps k n)]
      (swap! game #(assoc %
                     :countdown nil
                     :flash-key :yours
                     :pairing 0
                     :primed nil
                     :heaps new-heaps))
      (play-sound drip)
      )))

(defn mouse-out! [k n]
  "mouse-out all"
  (swap! game #(assoc % :hovered nil)))

(defn mouse-over! [k n]
  "hover over the nth item in heap k"
  (let [fkey (:flash-key @game)]
    (if (or (= fkey :yours) (= fkey :timer))
      (swap! game #(assoc % :hovered [k n]))
      )))

(defn next-pairing [p]
  (inc p))

(defn pair! [e]
  "pair items together somehow"
  (let [heaps (:heaps @game)
        p (:pairing @game)
        pairing (if (>= p (max-pairing heaps)) 0 (next-pairing p))]
    (do
      (.preventDefault e)
      (.stopPropagation e)
      (swap! game #(assoc % :pairing pairing))
      (play-sound drips))
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
      (.stopPropagation event)
      #_(you-clicked-on k n)
      (prime-or-delete! k n)
     )))

(defn item-over [event]
  "read [col row] form from event.target.id string"
  (let [[k n] (reader/read-string (-> event .-target .-id))]
    (do
      (.preventDefault event)
      (.stopPropagation event)
      (mouse-over! k n)
     )))

(defn item-out [event]
  "read [col row] form from event.target.id string"
  (let [[k n] (reader/read-string (-> event .-target .-id))]
    (do
      (.preventDefault event)
      (.stopPropagation event)
      (mouse-out! k n)
     )))

;;
;; rendering
;;
(r/defc tap-button < r/reactive [label handler key & [attrs]]
  (if attrs
    [:button (merge attrs {:on-click handler :on-touch-start handler :key key}) label]
    [:button {:on-click handler :on-touch-start handler :key key} label]
    ))

(declare replay-button)

(r/defc new-game < r/reactive []
  [:div
   (tap-button "New Game" start! "i4")
   (tap-button "OK" continue! "i5")
   (replay-button "i6")]
)

(r/defc instructions < r/reactive []
  [:div
   [:p {:key "i1"} "Take turns to remove as many drips as you like from a single drip trail."]
   [:p {:key "i2"} "Click once to choose, and once again in the same place to confirm. Take the very last drip to win the game."]
   [:p {:key "i3"} "Press 'New Game' to start, you then have 20 seconds to make the first move before
Al the computer loses patience and starts anyway."]
   (new-game)
   [:p {:key "i6"} "When present, the Pairer button can help you calculate a winning move by rearranging the drips. Each press makes a different arrangement."]
   (tap-button "Pairer" pair! "i7")
   ])


(r/defc well-done < r/reactive []
  [:div
   [:p {:key "i1"} "Well done."]
   [:p {:key "i2"} "If you understand the winning strategy you should be able to beat Al whenever you like! If not, keep trying till you've pinned it down."]
   [:p {:key "i3"} "We'd love to hear your explanation of how to win. Email "
    [:a {:href "mailto:wild@maths.org"} "wild@maths.org"]
    " with your thoughts."]
   (new-game)
])

(r/defc try-again < r/reactive []
  [:div
   [:p {:key "i1"} "Bad luck, but try again."]
   [:p {:key "i2"} "You may find it helpful to study what Al does. Try using the Pairer button after he's made a move to see what's special about the losing positions he leaves you in."]
   (new-game)
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

(r/defc debug-history-entry < r/reactive [i he]
  [:div {:class "debug" :key (str "he" i)} (str  he)])

(r/defc debug-history < r/reactive []
  [:div
   (map-indexed #(debug-history-entry %1 %2) (r/react game-history))])

(r/defc debug-game < r/reactive [g]
  [:section
   [:div {:class "debug" :key "d1"} (str g)]
   [:h3 {:key "d2"} "History" ]
   (r/with-props debug-history :rum/key "d3")])


(defn primed [g k i]
  (if (or
       (= (:flash-key g) :timer)
       (= (:flash-key g) :yours)
       (= (:flash-key g) :replay-yours))
    (if (is-primed? k i) "primed" "")))

(r/defc render-html-item < r/reactive [pairing k n i]
  "render item i in kth heap"
  (let [g @game
        key (str "[" k " " i "]")]
    [:div

     {:id key
      :class (str  "blobs " (primed g k i))
      :on-click (fn [e] (item-clicked e))
      :on-touch-start (fn [e] (item-clicked e))
      :on-mouse-over (fn [e] (item-over e))
      :on-mouse-out (fn [e] (item-out e))
      :style
      {:left (px (+ 0.3 (* 2 k)))
       :top (px (+ (item-offset pairing n i) 0.8))
       :width (px (* radius 2))
       :height (px (* radius 2))
       :background-color
       (if (or
            (= (:flash-key g) :als)
            (= (:flash-key g) :replay-als))
         (if (is-highlighted? k i)
           "rgba(255, 180, 0, 1)"
           "rgba(219, 133, 215, 1)")
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

(r/defc render-flash < r/reactive [fkey flash-msg score]
  (let [[yours als] score]
    [:div.flash-box {:class (subs (str fkey) 1)}
     [:span.msg {:key "f1"} flash-msg]
     [:span.score {:key "f2"} "Al: " als " You: " yours]
     ]))

(defn divider-offset [pairing]
  (nth [0 63 132 477] pairing))

(r/defc render-html-board < r/reactive [pairing heaps fkey flash-msg score]
  [:div.bordered
   {:style {:background-position (str  0 "px " (divider-offset pairing) "px")}}
   (render-flash fkey flash-msg score)
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

(r/defc icon-button < r/reactive [icon handler key & [attrs]]
  (tap-button [:i {:class (str "fa fa-" icon)}] handler key attrs))

(r/defc icon-label < r/reactive [icon label]
  [:span
   [:span {:class (str "fa fa-" icon)}]
   (str " " label)])

(r/defc replay-button < r/reactive [key]
  (tap-button (icon-label "fast-backward" "Replay") playback! key))

(r/defc render-toolbar < r/reactive [g]
  (let [level (:level g)
        playback (:playback g)
        ghc (count (r/react game-history))]
    [:div {:class "toolbar"}
     (if playback
       [:span {:class "controls" :key "t1"}
        (tap-button "New game" start! "stb")
        [:span {:key "t0" :class "left footer"}
         (if (not= (:playhead g) 0)
           (icon-button "step-backward" back! "back")
           (icon-button "step-backward disabled" back! "back" {:class "disabled"}))
         (if (< (inc (:playhead g)) ghc)
           (icon-button "step-forward" next! "next")
           (icon-button "step-forward disabled" next! "next" {:class "disabled"}))]
        [:span {:key "f1" :class "center"}
         (str "move " (:playhead g))]
        [:span {:key "t2" :class "right"}
         [:span {:class "footer" :key "rep"}
          (tap-button "Resume" playback! "plb" )]
         (when (big-enough g)
           (tap-button "Pairer" pair! "prb"))
         ]]

       [:span {:class "controls" :key "t2"}
        [:span {:class "left" :key "left"}
         (tap-button "New game" start! "stb")
         (tap-button "Rules" hint! "htb")
         ]
        [:span {:class "right" :key "right"}
         (if (> ghc 1)
           [:span {:class "footer" :key "rep"}
            (replay-button "replay")])
         (when (big-enough g)
           (tap-button "Pairer" pair! "prb"))
         ]]

       )]))

(r/defc render-game < r/reactive []
  (let [g (r/react game)
        pairing (:pairing g)
        heaps (:heaps g)
        fkey  (:flash-key g)
        flash-msg (flashes fkey)
          score (:score g)
        playback (:playback g)]
    [:div
     [:h1 {:key "g1"} "Drips" ]
     (r/with-props render-toolbar g :rum/key "toolbar")
     [:div {:key "g2" :class "board"}
      (r/with-props render-html-board pairing heaps fkey flash-msg score :rum/key "board")
      (r/with-props render-popover :rum/key "popup")
      ]
     (debug-game g)
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
         (not= fk :replay-als)
         (not= fk :replay-yours)
         (not= fk :playback)
         (not= fk :game-over)
         timer)
      (cond
       (= timer 0) (let [[k n] (show-best-move!)]
                     (swap! game #(assoc %
                                    :countdown (- timer 1)
                                    :flash-key :als
                                    :best [k n])))
       (= timer -3) (do
                      (make-best-move! (:best @game))
                      (if (= 0 (reduce + (:heaps @game)))
                        (show-a-winner!))
                      )
       :else (swap! game #(assoc % :countdown (- timer 1)))
       ))))

(def one-second 1000)

(defonce tick-watch
  (js/setInterval tick! one-second))

(add-watch routing/level-spec "akey" game-init)
