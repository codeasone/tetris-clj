(ns tetris.model
  (:require [clojure.core.matrix :as m]
            [clojure.spec.alpha :as s]
            [com.fulcrologic.guardrails.core :refer [>defn]]))

(def grid-width 10)
(def grid-height 20)

(def empty-game-grid
  [[0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]])

(def tetrimino-types [:tetrimino/I
                      :tetrimino/O
                      :tetrimino/T
                      :tetrimino/L
                      :tetrimino/J
                      :tetrimino/S
                      :tetrimino/Z])

(s/def ::tetrimino-type (set tetrimino-types))

(def tetrimino-shapes {:tetrimino/I [[[1, 1, 1, 1]]
                                     [[1]
                                      [1]
                                      [1]
                                      [1]]]

                       :tetrimino/O [[[2 2]
                                      [2 2]]]

                       :tetrimino/T [[[0 3 0]
                                      [3 3 3]]
                                     [[3 0]
                                      [3 3]
                                      [3 0]]
                                     [[3 3 3]
                                      [0 3 0]]
                                     [[0 3]
                                      [3 3]
                                      [0 3]]]

                       :tetrimino/L [[[4 0]
                                      [4 0]
                                      [4 4]]
                                     [[4 4 4]
                                      [4 0 0]]
                                     [[4 4]
                                      [0 4]
                                      [0 4]]
                                     [[0 0 4]
                                      [4 4 4]]]

                       :tetrimino/J [[[0 5]
                                      [0 5]
                                      [5 5]]
                                     [[5 0 0]
                                      [5 5 5]]
                                     [[5 5]
                                      [5 0]
                                      [5 0]]
                                     [[5 5 5]
                                      [0 0 5]]]

                       :tetrimino/S [[[0 6 6]
                                      [6 6 0]]
                                     [[6 0]
                                      [6 6]
                                      [0 6]]]

                       :tetrimino/Z [[[7 7 0]
                                      [0 7 7]]
                                     [[0 7]
                                      [7 7]
                                      [7 0]]]})

(defn- rotate [tetrimino]
  (let [tetrimino-orientations (->> tetrimino-shapes
                                    (filter (fn [[_ tetriminos]] ((set tetriminos) tetrimino)))
                                    first
                                    val)
        index-of-current (.indexOf tetrimino-orientations tetrimino)
        num-orientations (count tetrimino-orientations)
        next-orientation (get tetrimino-orientations (rem (inc index-of-current) num-orientations))]
    next-orientation))

(comment
  (rotate [[5 5]
           [5 0]
           [5 0]])
  ;;
  )

(defn random-tetrimino
  []
  (let [type (nth tetrimino-types (rand-int (count tetrimino-types)))
        possible-orientations (count (get tetrimino-shapes type))]
    (nth (get tetrimino-shapes type) (rand-int possible-orientations))))

(defn- width-of-tetrimino
  [tetrimino]
  (count (first tetrimino)))

(defn- height-of-tetrimino
  [tetrimino]
  (count tetrimino))

(defn initial-game-state
  []
  (let [initial-tetrimino (random-tetrimino)
        ;; initial-tetrimino [[1 1 1 1]]
        initial-position [(- 1 (height-of-tetrimino initial-tetrimino))
                          (rand-int (- grid-width (width-of-tetrimino initial-tetrimino)))]]
    {:game-grid empty-game-grid
     :current-tetrimino initial-tetrimino
     :next-tetrimino (random-tetrimino)
     :player-row-col initial-position
     :game-status :game-status/initialised}))

(s/def ::tetrimino-cell-value (s/int-in 0 8))
(s/def ::grid-of-tetrimino-cells (s/coll-of (s/coll-of ::tetrimino-cell-value)))
(s/def ::game-grid ::grid-of-tetrimino-cells)
(s/def ::current-tetrimino ::grid-of-tetrimino-cells)
(s/def ::next-tetrimino ::grid-of-tetrimino-cells)
(s/def ::column-extents (s/and vector? #(= 2 (count %))
                               (s/every integer?)
                               #(<= 0 (first %) grid-width)))
(s/def ::position-in-game-grid (s/and vector? #(= 2 (count %))
                                      #(<= -4 (first %) grid-height)
                                      #(<= 0 (second %) grid-width)))
(s/def ::player-row-col ::position-in-game-grid)
(s/def ::game-status #{:game-status/initialised
                       :game-status/playing
                       :game-status/game-over})

(s/def ::tetris (s/keys :req-un [::game-grid
                                 ::current-tetrimino
                                 ::player-row-col
                                 ::game-status]
                        :opt-un [::next-tetrimino]))

(comment
  (s/valid? ::tetris (initial-game-state))
  ;;
  )

(>defn extents-of-current-tetrimino
  [current-tetrimino player-row-col]
  [::current-tetrimino ::position-in-game-grid => (s/coll-of ::column-extents)]
  (let [[row col] player-row-col]
    (->> current-tetrimino
         m/transpose
         (mapv #(keep-indexed (fn [idx val] (when (pos? val) idx)) %))
         (mapv #(apply max %))
         (map-indexed (fn [idx extent-within-tetrimino]
                        [(+ col idx) (+ row extent-within-tetrimino)])))))

(s/def ::peaks (s/coll-of (s/nilable (s/int-in 0 (inc grid-height)))))

(>defn peaks
  [game-grid]
  [::game-grid => ::peaks]
  (let [transposed (m/transpose game-grid)]
    (->> transposed
         (mapv #(keep-indexed (fn [idx val] (when (pos? val) idx)) %))
         (mapv #(if (seq %)
                  (apply min %)
                  grid-height)))))

(defn tetrimino-collides-with-peaks?
  [{:keys [game-grid
           current-tetrimino
           player-row-col]}]
  (let [[_ col] player-row-col
        relevant-tetrimino-extents (->> (extents-of-current-tetrimino current-tetrimino player-row-col)
                                        (mapv second))
        relevant-game-grid-peaks (-> (peaks game-grid)
                                     (subvec col (+ col (width-of-tetrimino current-tetrimino))))]
    (->> (interleave relevant-tetrimino-extents relevant-game-grid-peaks)
         (partition 2)
         (some (fn [[tetrimino-extent grid-peak]] (when grid-peak (>= tetrimino-extent grid-peak))))
         boolean)))

(defn tetrimino-crosses-baseline?
  [{:keys [current-tetrimino
           player-row-col]}]
  (let [extents (extents-of-current-tetrimino current-tetrimino player-row-col)]
    (boolean (some (fn [[_ row-extent]]
                     (>= row-extent grid-height)) extents))))

(>defn compose-current-tetrimino-into-game-grid
  "This compose helper is not responsible for any validation"
  [{:keys [game-grid current-tetrimino player-row-col]}]
  [::tetris => ::game-grid]
  (let [[row col] player-row-col
        mutable-game-grid* (atom game-grid)]
    ;; Code is easier to understand when implemented using local mutable atom
    (doseq [{:keys [row-idx tetrimino-row]}
            (map-indexed (fn [row-idx tetrimino-row]
                           {:row-idx row-idx :tetrimino-row tetrimino-row})
                         current-tetrimino)]

      (doseq [{:keys [col-idx tetrimino-cell-value]}
              (map-indexed (fn [col-idx tetrimino-cell-value]
                             {:col-idx col-idx :tetrimino-cell-value tetrimino-cell-value})
                           tetrimino-row)]
        (when (pos-int? tetrimino-cell-value)
          (swap! mutable-game-grid*
                 (fn [current-grid]
                   (if (and (<= 0 (+ row row-idx) (dec grid-height))
                            (<= 0 (+ col col-idx) (dec grid-width)))
                     (update-in current-grid [(+ row row-idx) (+ col col-idx)] (fn [_] tetrimino-cell-value))
                     current-grid))))))
    @mutable-game-grid*))

(defn- handle-left [{:keys [_game-grid
                            _current-tetrimino
                            _next-tetrimino
                            player-row-col] :as game-state-before}]
  (let [[_ col] player-row-col]
    (if (pos-int? col)
      (assoc-in game-state-before [:player-row-col 1] (dec col))
      game-state-before)))

(defn- handle-right [{:keys [_game-grid
                             current-tetrimino
                             _next-tetrimino
                             player-row-col] :as game-state-before}]
  (let [[_ col] player-row-col]
    (if (< (+ col (width-of-tetrimino current-tetrimino)) grid-width)
      (assoc-in game-state-before [:player-row-col 1] (inc col))
      game-state-before)))

(defn- handle-rotate [{:keys [current-tetrimino
                              player-row-col] :as game-state-before}]
  (let [[row col] player-row-col
        rotated (rotate current-tetrimino)
        rotated-width (width-of-tetrimino rotated)
        new-game-state (cond-> (assoc game-state-before :current-tetrimino rotated)
                         ;; Boundary handling on rhs of game-grid
                         (> (+ col rotated-width) grid-width)
                         (assoc :player-row-col [row (- grid-width rotated-width)]))]

    (loop [{:keys [player-row-col] :as adjusted-game-state} new-game-state]
      (if (or (tetrimino-crosses-baseline? adjusted-game-state)
              (tetrimino-collides-with-peaks? adjusted-game-state))
        (let [[row col] player-row-col]
          (recur (assoc adjusted-game-state :player-row-col [(dec row) col])))
        adjusted-game-state))))

(defn- handle-down [{:keys [_game-grid
                            current-tetrimino
                            _next-tetrimino
                            player-row-col] :as game-state-before}]
  (let [[row _] player-row-col]
    (if (< (+ row (height-of-tetrimino current-tetrimino)) grid-height)
      (assoc-in game-state-before [:player-row-col 0] (inc row))
      game-state-before)))

(defn- handle-drop [{:keys [game-grid
                            current-tetrimino
                            player-row-col] :as game-state-before}]
  (let [[_ col] player-row-col
        extents-of-current-tetrimino (->> (extents-of-current-tetrimino current-tetrimino [0 col])
                                          (mapv second))
        relevant-game-grid-peaks (as-> (peaks game-grid) $
                                   (subvec $ col (+ col (width-of-tetrimino current-tetrimino)))
                                   (mapv (fn [extent] (or extent grid-height)) $))
        new-player-row-col [(apply min (->> (interleave relevant-game-grid-peaks extents-of-current-tetrimino)
                                            (partition 2)
                                            (mapv (fn [[peak extent]] (- peak (inc extent)))))) col]]

    (assoc game-state-before :player-row-col new-player-row-col)))

(s/def ::game-event #{::move-left
                      ::move-right
                      ::move-down
                      ::rotate-current
                      ::drop-current})

(s/def ::game-events (s/coll-of ::game-event))

(>defn start-playing-next-tetrimino? [{:keys [game-grid
                                              current-tetrimino
                                              player-row-col]}]
  [::tetris => boolean?]
  (let [[_ col] player-row-col
        relevant-tetrimino-extents (->> (extents-of-current-tetrimino current-tetrimino player-row-col)
                                        (mapv (or second (dec grid-height))))
        relevant-game-grid-peaks (-> (peaks game-grid)
                                     (subvec col (+ col (width-of-tetrimino current-tetrimino))))]
    (->> (interleave relevant-tetrimino-extents relevant-game-grid-peaks)
         (partition 2)
         (some (fn [[extent peak]] (= (inc extent) peak)))
         boolean)))

(>defn game-over? [{:keys [player-row-col] :as game-state}]
  [::tetris => boolean?]
  (let [[row _] player-row-col]
    (and (<= row 0)
         (start-playing-next-tetrimino? game-state))))

(>defn handle-events [game-state-before events]
  [::tetris ::game-events => ::tetris]
  (let [{:keys [next-tetrimino] :as adjusted-game-state}
        (reduce (fn [acc ev]
                  (case ev
                    ::move-left
                    (handle-left acc)
                    ::move-right
                    (handle-right acc)
                    ::move-down
                    (handle-down acc)
                    ::rotate-current
                    (handle-rotate acc)
                    ::drop-current
                    (handle-drop acc)))
                game-state-before events)
        start-playing-next-tetrimino? (start-playing-next-tetrimino? adjusted-game-state)
        game-over? (game-over? adjusted-game-state)]

    (cond-> adjusted-game-state
      start-playing-next-tetrimino?
      (assoc :game-grid (compose-current-tetrimino-into-game-grid adjusted-game-state)
             :current-tetrimino next-tetrimino
             :player-row-col [(- (height-of-tetrimino next-tetrimino))
                              (rand-int (- grid-width (width-of-tetrimino next-tetrimino)))]
             :next-tetrimino (random-tetrimino))

      game-over?
      (assoc :game-status :game-status/game-over))))
