(ns tetris.model
  (:require [clojure.spec.alpha :as s]))

(def grid-width 10)
(def grid-height 20)

(def empty-game-grid
  [[0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
   [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]])

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

(defn random-tetrimino
  []
  (let [type (nth tetrimino-types (rand-int (count tetrimino-types)))
        possible-orientations (count (get tetrimino-shapes type))]
    (nth (get tetrimino-shapes type) (rand-int possible-orientations))))

(defn width-of-tetrimino
  [tetrimino]
  (count (first tetrimino)))

(defn initial-game-state
  []
  (let [initial-tetrimino (random-tetrimino)
        initial-position [0 (rand-int (- grid-width (width-of-tetrimino initial-tetrimino)))]]
    {:game-grid empty-game-grid
     :current-tetrimino initial-tetrimino
     :next-tetrimino (random-tetrimino)
     :player-row-column initial-position}))

(s/def ::tetrimino-cell-value (s/int-in 0 8))
(s/def ::grid-of-tetrimino-cells (s/coll-of (s/coll-of ::tetrimino-cell-value)))
(s/def ::game-grid ::grid-of-tetrimino-cells)
(s/def ::current-tetrimino ::grid-of-tetrimino-cells)
(s/def ::next-tetrimino ::grid-of-tetrimino-cells)
(s/def ::player-row-column (s/cat :row (s/int-in 0 (inc grid-height)) :col (s/int-in 0 (inc grid-width))))
(s/def ::game-state (s/keys :req-un [::game-grid
                                     ::current-tetrimino
                                     ::next-tetrimino
                                     ::player-row-column]))

(comment
  (s/valid? ::game-state (initial-game-state))
  ;;
  )

(defn compose-grid
  "This compose helper is not responsible game state for any validation"
  [current-grid current-tetrimino play-position]
  (let [[row col] play-position
        mutable-game-grid* (atom current-grid)]
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
                   (update-in current-grid [(+ row row-idx) (+ col col-idx)] (fn [_] tetrimino-cell-value)))))))
    @mutable-game-grid*))
