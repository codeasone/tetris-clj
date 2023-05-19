(ns tetris.logic
  (:require [clojure.core.matrix :as m]
            [clojure.spec.alpha :as s]
            [com.fulcrologic.guardrails.core :refer [>defn]]))

(def visible-grid-width 10)
(def lead-in-grid-height 4)
(def visible-grid-height 20)

(def empty-row (vec (repeat visible-grid-width 0)))
(def empty-game-grid (vec (repeat (+ lead-in-grid-height visible-grid-height) empty-row)))
;;    [[[0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]
;;      [0 0 0 0 0 0 0 0 0 0]]]

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

(defn entry-row-for-tetrimino
  [tetrimino]
  (- (height-of-tetrimino tetrimino)))

(defn entry-column-for-tetrimino
  [tetrimino]
  (rand-int (- visible-grid-width (width-of-tetrimino tetrimino))))

(defn initial-game-state
  []
  (let [initial-grid empty-game-grid
        ;; initial-grid [[0 0 0 0 0 0 0 0 0 0]
        ;;               [0 0 0 0 0 0 0 0 0 0]
        ;;               [0 0 0 0 0 0 0 0 0 0]
        ;;               [0 0 0 0 0 0 0 0 0 0]
        ;;               [0 0 0 0 0 0 0 0 0 0]
        ;;               [0 0 0 0 0 0 0 0 0 0]
        ;;               [0 0 0 0 0 0 0 0 0 0]
        ;;               [0 0 0 0 0 0 0 0 0 0]
        ;;               [0 0 0 0 0 0 0 0 0 0]
        ;;               [0 0 0 0 0 0 0 0 0 0]
        ;;               [0 0 0 0 0 0 0 0 0 0]
        ;;               [0 0 0 0 0 0 0 0 0 0]
        ;;               [0 0 0 0 0 0 0 0 0 0]
        ;;               [0 0 0 0 0 0 0 0 0 0]
        ;;               [0 0 0 0 0 0 0 0 0 0]
        ;;               [0 0 0 0 0 0 0 0 0 0]
        ;;               [0 0 0 0 0 0 0 0 0 0]
        ;;               [0 0 0 0 0 0 0 0 0 0]
        ;;               [0 0 0 0 0 0 0 0 0 0]
        ;;               [0 0 1 1 1 1 1 1 1 1]]
        initial-tetrimino (random-tetrimino)
        ;; initial-tetrimino [[2 2]
        ;;                    [2 2]]
        initial-position [(entry-row-for-tetrimino initial-tetrimino)
                          (entry-column-for-tetrimino initial-tetrimino)]
        ;; initial-position [0 0]
        ]
    {:game-grid initial-grid
     :current-tetrimino initial-tetrimino
     :next-tetrimino (random-tetrimino)
     :player-row-col initial-position
     :game-status :game-status/initialised
     :game-score 0}))

(s/def ::tetrimino-cell-value (s/int-in 0 8))
(s/def ::grid-of-tetrimino-cells (s/coll-of (s/coll-of ::tetrimino-cell-value)
                                            :count (+ lead-in-grid-height visible-grid-height)))
(s/def ::game-grid ::grid-of-tetrimino-cells)
(s/def ::current-tetrimino (s/coll-of (s/coll-of ::tetrimino-cell-value)))
(s/def ::next-tetrimino (s/coll-of (s/coll-of ::tetrimino-cell-value)))
(s/def ::column-extents (s/and vector? #(= 2 (count %))
                               (s/every integer?)
                               #(<= 0 (first %) visible-grid-width)))
(s/def ::position-in-game-grid (s/and vector? #(= 2 (count %))
                                      #(<= -4 (first %) visible-grid-height)
                                      #(<= 0 (second %) visible-grid-width)))
(s/def ::player-row-col ::position-in-game-grid)
(s/def ::game-status #{:game-status/initialised
                       :game-status/playing
                       :game-status/game-over})
(s/def ::game-score nat-int?)

(s/def ::tetris (s/keys :req-un [::game-grid
                                 ::current-tetrimino
                                 ::player-row-col]
                        :opt-un [::next-tetrimino
                                 ::game-status
                                 ::game-score]))

(comment
  (s/valid? ::tetris (initial-game-state))
  ;;
  )

(>defn start-playing [game-state]
  [::tetris => ::tetris]
  (assoc game-state :game-status :game-status/playing))

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

(s/def ::peaks (s/coll-of (s/nilable (s/int-in 0 (inc visible-grid-height)))))

(>defn peaks
  [game-grid]
  [::game-grid => ::peaks]
  (let [transposed (m/transpose game-grid)]
    (->> transposed
         (mapv #(keep-indexed (fn [idx val] (when (pos? val) idx)) %))
         (mapv #(if (seq %)
                  (- (apply min %) lead-in-grid-height)
                  visible-grid-height)))))

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
                     (>= row-extent visible-grid-height)) extents))))

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
                   (if (and (<= 0 (+ row row-idx) (dec visible-grid-height))
                            (<= 0 (+ col col-idx) (dec visible-grid-width)))
                     (update-in current-grid [(+ row row-idx lead-in-grid-height)
                                              (+ col col-idx)]
                                (fn [_] tetrimino-cell-value))
                     current-grid))))))
    @mutable-game-grid*))

(defn- select-rows
  [coll rows]
  (into [] (keep-indexed #(when (contains? rows %1) %2) coll)))

(defn- remove-rows
  [coll rows]
  (into [] (keep-indexed #(when (not (contains? rows %1)) %2) coll)))

(defn- move-left [{:keys [game-grid
                          current-tetrimino
                          player-row-col] :as game-state-before}]
  (let [[row col] player-row-col
        relevant-rows (select-rows game-grid
                                   (set (range (+ row lead-in-grid-height)
                                               (+ row lead-in-grid-height (height-of-tetrimino current-tetrimino)))))]
    (if (pos? col)
      (if-let [_collision-free?
               (->> (interleave current-tetrimino relevant-rows)
                    (partition 2)
                    (every? (fn [[[left-most-tetrimino-cell & _]
                                  grid-row]]
                              (not (and (pos? left-most-tetrimino-cell)
                                        (pos? (nth grid-row (dec col))))))))]
        (assoc-in game-state-before [:player-row-col 1] (dec col))
        game-state-before)
      game-state-before)))

(defn- move-right [{:keys [game-grid
                           current-tetrimino
                           player-row-col] :as game-state-before}]
  (let [[row col] player-row-col
        relevant-rows (select-rows game-grid
                                   (set (range (+ row lead-in-grid-height)
                                               (+ row lead-in-grid-height (height-of-tetrimino current-tetrimino)))))]
    (if (< (+ col (width-of-tetrimino current-tetrimino)) visible-grid-width)
      (if-let [_collision-free?
               (->> (interleave current-tetrimino relevant-rows)
                    (partition 2)
                    (every? (fn [[tetrimino-row
                                  grid-row]]
                              (not (and (pos? (last tetrimino-row))
                                        (pos? (nth grid-row (+ col (width-of-tetrimino current-tetrimino)))))))))]
        (assoc-in game-state-before [:player-row-col 1] (inc col))
        game-state-before)
      game-state-before)))

(defn- move-down [{:keys [_game-grid
                          current-tetrimino
                          _next-tetrimino
                          player-row-col] :as game-state-before}]
  (let [[row _] player-row-col]
    (if (< (+ row (height-of-tetrimino current-tetrimino)) visible-grid-height)
      (assoc-in game-state-before [:player-row-col 0] (inc row))
      game-state-before)))

(defn- rotate-current [{:keys [current-tetrimino
                               player-row-col] :as game-state-before}]
  (let [[row col] player-row-col
        rotated (rotate current-tetrimino)
        rotated-width (width-of-tetrimino rotated)
        new-game-state (cond-> (assoc game-state-before :current-tetrimino rotated)
                         ;; Boundary handling on rhs of game-grid
                         (> (+ col rotated-width) visible-grid-width)
                         (assoc :player-row-col [row (- visible-grid-width rotated-width)]))]

    (loop [{:keys [player-row-col] :as adjusted-game-state} new-game-state]
      (if (or (tetrimino-crosses-baseline? adjusted-game-state)
              (tetrimino-collides-with-peaks? adjusted-game-state))
        (let [[row col] player-row-col]
          (recur (assoc adjusted-game-state :player-row-col [(dec row) col])))
        adjusted-game-state))))

(defn- drop-current [{:keys [game-grid
                             current-tetrimino
                             player-row-col] :as game-state-before}]
  (let [[_ col] player-row-col
        extents-of-current-tetrimino (->> (extents-of-current-tetrimino current-tetrimino [0 col])
                                          (mapv second))
        relevant-game-grid-peaks (as-> (peaks game-grid) $
                                   (subvec $ col (+ col (width-of-tetrimino current-tetrimino)))
                                   (mapv (fn [extent] (or extent visible-grid-height)) $))
        new-player-row-col [(apply min (->> (interleave relevant-game-grid-peaks extents-of-current-tetrimino)
                                            (partition 2)
                                            (mapv (fn [[peak extent]]
                                                    (- peak (inc extent)))))) col]]

    (assoc game-state-before :player-row-col new-player-row-col)))

(s/def ::game-event #{::move-left
                      ::move-right
                      ::move-down
                      ::rotate
                      ::drop})

(s/def ::game-events (s/coll-of ::game-event))

(>defn tetrimino-adjacent-to-peaks? [{:keys [game-grid
                                             current-tetrimino
                                             player-row-col]}]
  [::tetris => boolean?]
  (let [[_ col] player-row-col
        relevant-tetrimino-extents (->> (extents-of-current-tetrimino current-tetrimino player-row-col)
                                        (mapv (or second (dec visible-grid-height))))
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
         (tetrimino-adjacent-to-peaks? game-state))))

(defn- complete-rows [{:keys [game-grid]}]
  (keep-indexed (fn [idx row] (when (every? pos? row) idx)) game-grid))

(>defn clear-complete-rows [{:keys [game-grid game-score] :as game-state}]
  [::tetris => ::tetris]
  (let [rows-to-remove (set (complete-rows game-state))
        replacement-rows-at-top (repeat (count rows-to-remove) empty-row)
        remaining-rows (remove-rows game-grid rows-to-remove)]
    (assoc game-state
           :game-grid (into [] (concat replacement-rows-at-top remaining-rows))
           :game-score (+ game-score (* 100 (count rows-to-remove))))))

(>defn handle-events [game-state-before events]
  [::tetris ::game-events => ::tetris]
  (if (= :game-status/playing (:game-status game-state-before))
    (reduce
     (fn [acc ev]
       (let [{:keys [next-tetrimino] :as adjusted-game-state}
             (case ev
               ::move-left
               (move-left acc)
               ::move-right
               (move-right acc)
               ::move-down
               (move-down acc)
               ::rotate
               (rotate-current acc)
               ::drop
               (drop-current acc))]

         (if (game-over? adjusted-game-state)
           (assoc adjusted-game-state :game-status :game-status/game-over)
           (let [tetrimino-adjacent-to-peaks? (tetrimino-adjacent-to-peaks? adjusted-game-state)
                 adjusted-game-state (cond-> adjusted-game-state
                                       tetrimino-adjacent-to-peaks?
                                       (assoc :game-grid (compose-current-tetrimino-into-game-grid adjusted-game-state)))
                 game-grid-contains-complete-rows? (seq (complete-rows adjusted-game-state))
                 start-playing-next-tetrimino? (or game-grid-contains-complete-rows? tetrimino-adjacent-to-peaks?)]
             (cond-> adjusted-game-state
               game-grid-contains-complete-rows?
               clear-complete-rows

               start-playing-next-tetrimino?
               (assoc :current-tetrimino next-tetrimino
                      :player-row-col [(- (height-of-tetrimino next-tetrimino))
                                       (entry-column-for-tetrimino next-tetrimino)]
                      :next-tetrimino (random-tetrimino)))))))
     game-state-before events)
    game-state-before))
