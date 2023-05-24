(ns tetris.logic
  (:require [clojure.core.matrix :as m]
            [clojure.spec.alpha :as s]
            [com.fulcrologic.guardrails.core :refer [>defn]]
            [tetris.utils :as utils]))

(def visible-grid-width 10)
(def lead-in-grid-height 4)
(def visible-grid-height 20)

(def tetrimino-types [:tetrimino/I
                      :tetrimino/O
                      :tetrimino/T
                      :tetrimino/L
                      :tetrimino/J
                      :tetrimino/S
                      :tetrimino/Z])

(s/def ::tetrimino-type (set tetrimino-types))

(s/def ::game-event #{::move-left
                      ::move-right
                      ::move-down
                      ::rotate
                      ::drop})

(s/def ::game-events (s/coll-of ::game-event))
(s/def ::tetrimino-cell-value (s/int-in 0 8))
(s/def ::tetrimino (s/coll-of (s/coll-of ::tetrimino-cell-value)))
(s/def ::grid-of-tetrimino-cells (s/coll-of (s/coll-of ::tetrimino-cell-value)
                                            :count (+ lead-in-grid-height visible-grid-height)))
(s/def ::game-grid ::grid-of-tetrimino-cells)
(s/def ::peaks (s/coll-of (s/nilable (s/int-in 0 (inc visible-grid-height)))))
(s/def ::current-tetrimino ::tetrimino)
(s/def ::next-tetrimino ::tetrimino)
(s/def ::column-extents (s/and vector? #(= 2 (count %))
                               (s/every integer?)
                               #(<= 0 (first %) visible-grid-width)))
(s/def ::position-in-game-grid (s/and vector? #(= 2 (count %))
                                      #(<= -4 (first %) visible-grid-height)
                                      #(<= 0 (second %) visible-grid-width)))
(s/def ::player-row-col ::position-in-game-grid)
(s/def ::game-status #{:game-status/initialised
                       :game-status/playing})
(s/def ::game-score nat-int?)
(s/def ::game-level pos-int?)

(s/def ::game-state (s/keys :req-un [::game-grid
                                     ::current-tetrimino
                                     ::player-row-col]
                            :opt-un [::next-tetrimino
                                     ::game-status
                                     ::game-score
                                     ::game-level]))
(def empty-row (vec (repeat visible-grid-width 0)))
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

(def empty-game-grid (vec (repeat (+ lead-in-grid-height visible-grid-height) empty-row)))

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

(>defn rotate
  [tetrimino]
  [::tetrimino => ::tetrimino]
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

(>defn random-tetrimino
  []
  [=> ::tetrimino]
  (let [type (nth tetrimino-types (rand-int (count tetrimino-types)))
        possible-orientations (count (get tetrimino-shapes type))]
    (nth (get tetrimino-shapes type) (rand-int possible-orientations))))

(>defn width-of-tetrimino
  [tetrimino]
  [::tetrimino => (s/int-in 1 5)]
  (count (first tetrimino)))

(>defn height-of-tetrimino
  [tetrimino]
  [::tetrimino => (s/int-in 1 5)]
  (count tetrimino))

(>defn entry-row-for-tetrimino
  [tetrimino]
  [::tetrimino => (s/int-in (- lead-in-grid-height) 0)]
  (- (height-of-tetrimino tetrimino)))

(>defn entry-column-for-tetrimino
  [tetrimino]
  [::tetrimino => (s/int-in 0 visible-grid-width)]
  (rand-int (- visible-grid-width (width-of-tetrimino tetrimino))))

(>defn initial-game-state
  []
  [=> ::game-state]
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
     :game-score 0
     :game-level 1}))

(>defn start-playing
  [game-state]
  [::game-state => ::game-state]
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

(>defn peaks
  [game-grid player-row]
  [::game-grid nat-int? => ::peaks]
  (let [ignore-rows-up-to-player-row
        (reduce (fn [acc [idx row]]
                  (conj acc (if (<= idx (+ player-row lead-in-grid-height))
                              empty-row
                              row))) [] (->> (interleave (range (count game-grid)) game-grid)
                                             (partition 2)))
        transposed (m/transpose ignore-rows-up-to-player-row)]
    (->> transposed
         (mapv #(keep-indexed (fn [idx val] (when (pos? val) idx)) %))
         (mapv #(if (seq %)
                  (- (apply min %) lead-in-grid-height)
                  visible-grid-height)))))

(>defn tetrimino-collides-with-peaks-below-it?
  [{:keys [game-grid
           current-tetrimino
           player-row-col]}]
  [::game-state => boolean?]
  (let [[row col] player-row-col
        relevant-tetrimino-extents (->> (extents-of-current-tetrimino current-tetrimino player-row-col)
                                        (mapv second))
        relevant-game-grid-peaks (-> (peaks game-grid row)
                                     (subvec col (+ col (width-of-tetrimino current-tetrimino))))]
    (->> (interleave relevant-tetrimino-extents relevant-game-grid-peaks)
         (partition 2)
         (some (fn [[tetrimino-extent grid-peak]] (when grid-peak
                                                    (and (< row grid-peak)
                                                         (>= tetrimino-extent grid-peak)))))
         boolean)))

;; This compose helper is not responsible for any validation
(>defn tetrimino-crosses-baseline?
  [{:keys [current-tetrimino
           player-row-col]}]
  [::game-state => boolean?]
  (let [extents (extents-of-current-tetrimino current-tetrimino player-row-col)]
    (boolean (some (fn [[_ row-extent]]
                     (>= row-extent visible-grid-height)) extents))))

(>defn compose-current-tetrimino-into-game-grid
  [{:keys [game-grid current-tetrimino player-row-col]}]
  [::game-state => ::game-grid]
  (let [[row col] player-row-col
        mutable-game-grid* (atom game-grid)]
    ;; Code is easier to understand when implemented using a local mutable atom
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

(>defn get-cells
  [game-grid row col-start num-cells]
  [::game-grid (s/int-in (- lead-in-grid-height) visible-grid-height)
   (s/int-in 0 visible-grid-width) (s/int-in 1 visible-grid-width)
   => (s/coll-of ::tetrimino-cell-value)]
  (let [row-cells (nth game-grid (+ lead-in-grid-height row))]
    (->> row-cells
         (keep-indexed (fn [idx val] (when (< (dec col-start) idx (+ col-start num-cells)) val))))))

(>defn tetrinimo-can-move-down?
  [{:keys [game-grid
           current-tetrimino
           player-row-col]}]
  [::game-state => boolean?]
  (let [[row col] player-row-col]
    (and
     (< (+ row (height-of-tetrimino current-tetrimino)) visible-grid-height)
     (every? (fn [[t-cell g-cell]]
               (or (zero? t-cell)
                   (zero? g-cell)))
             (->> (interleave (last current-tetrimino)
                              (get-cells game-grid (+ row (height-of-tetrimino current-tetrimino))
                                         col (width-of-tetrimino current-tetrimino)))
                  (partition 2))))))

(>defn tetrinimo-cannot-shift-upwards?
  [{:keys [game-grid
           current-tetrimino
           player-row-col]}]
  [::game-state => boolean?]
  (let [[row col] player-row-col]
    (boolean (some #(pos? %) (get-cells game-grid (dec row) col (width-of-tetrimino current-tetrimino))))))

(>defn tetrimino-adjacent-to-peaks?
  [{:keys [game-grid
           current-tetrimino
           player-row-col]}]
  [::game-state => boolean?]
  (let [[row col] player-row-col
        relevant-tetrimino-extents (->> (extents-of-current-tetrimino current-tetrimino player-row-col)
                                        (mapv (or second (dec visible-grid-height))))
        relevant-game-grid-peaks (-> (peaks game-grid row)
                                     (subvec col (+ col (width-of-tetrimino current-tetrimino))))]
    ;; (tap> {:relevant-tetrimino-extents relevant-tetrimino-extents
    ;;        :relevant-game-grid-peaks relevant-game-grid-peaks
    ;;        :status (->> (interleave relevant-tetrimino-extents relevant-game-grid-peaks)
    ;;                     (partition 2)
    ;;                     (some (fn [[extent peak]] (= (inc extent) peak)))
    ;;                     boolean)})
    (->> (interleave relevant-tetrimino-extents relevant-game-grid-peaks)
         (partition 2)
         (some (fn [[extent peak]] (= (inc extent) peak)))
         boolean)))

(>defn game-over?
  [{:keys [player-row-col] :as game-state}]
  [::game-state => boolean?]
  (let [[row _] player-row-col]
    (and (<= row 0)
         (tetrimino-adjacent-to-peaks? game-state))))

(>defn complete-rows
  [{:keys [game-grid]}]
  [::game-state => (s/coll-of (s/int-in 0 (+ lead-in-grid-height visible-grid-height)))]
  (keep-indexed (fn [idx row] (when (every? pos? row) idx)) game-grid))

(>defn introduce-next-tetrimino
  [{:keys [next-tetrimino] :as game-state-before}]
  [::game-state => ::game-state]
  (assoc game-state-before :current-tetrimino next-tetrimino
         :player-row-col [(- (height-of-tetrimino next-tetrimino))
                          (entry-column-for-tetrimino next-tetrimino)]
         :next-tetrimino (random-tetrimino)))

(def difficulty-increment-in-pts 1000)

(>defn level-from-score [score]
  [pos-int? => pos-int?]
  (inc (quot score difficulty-increment-in-pts)))

(>defn clear-complete-rows
  [{:keys [game-grid game-score] :as game-state-before}]
  [::game-state => ::game-state]
  (if-let [rows-to-remove (seq (complete-rows game-state-before))]
    (let [replacement-rows-at-top (repeat (count rows-to-remove) empty-row)
          remaining-rows (utils/remove-from game-grid (set rows-to-remove))
          new-score (+ game-score (* 100 (count rows-to-remove)))]
      (-> game-state-before
          (assoc :game-grid (into [] (concat replacement-rows-at-top remaining-rows))
                 :game-score new-score
                 :game-level (level-from-score new-score))
          introduce-next-tetrimino))
    game-state-before))

(>defn burn-in-current-tetrimino
  [game-state]
  [::game-state => ::game-state]
  (assoc game-state :game-grid (compose-current-tetrimino-into-game-grid game-state)))

(>defn move-left
  [{:keys [game-grid
           current-tetrimino
           player-row-col] :as game-state-before}]
  [::game-state => ::game-state]
  (let [[row col] player-row-col
        relevant-rows (utils/select-from game-grid
                                         (set (range (+ row lead-in-grid-height)
                                                     (+ row lead-in-grid-height
                                                        (height-of-tetrimino current-tetrimino)))))]
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

(>defn move-right
  [{:keys [game-grid
           current-tetrimino
           player-row-col] :as game-state-before}]
  [::game-state => ::game-state]
  (let [[row col] player-row-col
        relevant-rows (utils/select-from game-grid
                                         (set (range (+ row lead-in-grid-height)
                                                     (+ row lead-in-grid-height
                                                        (height-of-tetrimino current-tetrimino)))))]
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

(>defn rotate-current
  [{:keys [current-tetrimino
           player-row-col] :as game-state-before}]
  [::game-state => ::game-state]
  (let [[row col] player-row-col
        rotated (rotate current-tetrimino)
        rotated-width (width-of-tetrimino rotated)
        new-game-state (cond-> (assoc game-state-before :current-tetrimino rotated)
                         ;; Boundary handling on rhs of game-grid
                         (> (+ col rotated-width) visible-grid-width)
                         (assoc :player-row-col [row (- visible-grid-width rotated-width)]))
        no-space-to-rotate? (and (tetrinimo-cannot-shift-upwards? new-game-state)
                                 (or (tetrimino-crosses-baseline? new-game-state)
                                     (tetrimino-collides-with-peaks-below-it? new-game-state)))]
    (loop [{:keys [player-row-col] :as adjusted-game-state} new-game-state]
      (if no-space-to-rotate?
        game-state-before
        (if (or (tetrimino-crosses-baseline? adjusted-game-state)
                (tetrimino-collides-with-peaks-below-it? adjusted-game-state))
          (let [[row col] player-row-col]
            (recur (assoc adjusted-game-state :player-row-col [(dec row) col])))
          adjusted-game-state)))))

(>defn move-down
  [{:keys [player-row-col] :as game-state-before} play-next-tetrimino-fn]
  [::game-state fn? => ::game-state]
  (let [[row _] player-row-col]
    (if (tetrinimo-can-move-down? game-state-before)
      (let [adjusted-game-state (assoc-in game-state-before [:player-row-col 0] (inc row))]
        (cond-> adjusted-game-state
          (tetrimino-adjacent-to-peaks? adjusted-game-state)
          play-next-tetrimino-fn))
      (play-next-tetrimino-fn game-state-before))))

(>defn drop-current
  [{:keys [game-grid
           current-tetrimino
           player-row-col] :as game-state-before} play-next-tetrimino-fn]
  [::game-state fn? => ::game-state]
  (if (tetrinimo-can-move-down? game-state-before)
    (let [[row col] player-row-col
          extents-of-current-tetrimino (->> (extents-of-current-tetrimino current-tetrimino [0 col])
                                            (mapv second))
          relevant-game-grid-peaks (as-> (peaks game-grid row) $
                                     (subvec $ col (+ col (width-of-tetrimino current-tetrimino)))
                                     (mapv (fn [extent] (or extent visible-grid-height)) $))
          new-player-row-col [(apply min (->> (interleave relevant-game-grid-peaks extents-of-current-tetrimino)
                                              (partition 2)
                                              (mapv (fn [[peak extent]]
                                                      (- peak (inc extent)))))) col]]
      (-> game-state-before
          (assoc :player-row-col new-player-row-col)
          play-next-tetrimino-fn))
    game-state-before))

(def play-next-tetrimino (comp introduce-next-tetrimino clear-complete-rows burn-in-current-tetrimino))

(>defn handle-events
  ([game-state-before events]
   [::game-state ::game-events => ::game-state] (handle-events game-state-before events play-next-tetrimino))
  ([game-state-before events play-next-tetrimino-fn]
   [::game-state ::game-events fn? => ::game-state]
   (if (= :game-status/playing (:game-status game-state-before))
     (reduce
      (fn [acc ev]
        (case ev
          ::move-left
          (move-left acc)
          ::move-right
          (move-right acc)
          ::move-down
          (move-down acc play-next-tetrimino-fn)
          ::rotate
          (rotate-current acc)
          ::drop
          (drop-current acc play-next-tetrimino-fn)))
      game-state-before events)
     game-state-before)))
