(ns tetris.model-test
  (:require #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :refer :all :include-macros true])
            [tetris.model :as sut]))

(defn tetrimino-to-coords [tetrimino]
  (->> tetrimino
       (map-indexed (fn [row-idx row]
                      (map (fn [col-idx]
                             [row-idx col-idx])
                           (keep-indexed (fn [idx elem] (when (pos? elem) idx)) row))))
       (reduce concat)))

(deftest tetrimino-to-coords-test
  (is (= [[0 0] [0 1] [1 1] [1 2]]
         (tetrimino-to-coords [[7 7 0]
                               [0 7 7]])))
  (is (= [[0 0] [0 1] [1 0] [1 1]]
         (tetrimino-to-coords [[2 2]
                               [2 2]])))
  (is (= [[0 0] [0 1] [1 1] [2 1]]
         (tetrimino-to-coords [[4 4]
                               [0 4]
                               [0 4]])))
  (is (= [[0 0] [0 1] [0 2] [0 3]]
         (tetrimino-to-coords [[1, 1, 1, 1]])))
  (is (= [[0 0] [1 0] [2 0] [3 0]]
         (tetrimino-to-coords [[1]
                               [1]
                               [1]
                               [1]]))))

(def tetrimino-shape-normalised-coords
  (reduce-kv
   (fn [acc type tetriminos]
     (assoc acc type (mapv tetrimino-to-coords tetriminos)))
   {}
   sut/tetrimino-shapes))

(defn- game-state-from-before-grid
  [test-grid]
  (let [game-grid (mapv (fn [row]
                          (mapv (fn [ev] (if (= ev *) 0 ev)) row)) test-grid)
        current-tetrimino-coords
        (->> test-grid
             flatten
             (keep-indexed (fn [idx elem] (when (= elem *) idx)))
             (mapv (fn [idx] [(unchecked-divide-int idx sut/grid-width) (rem idx sut/grid-width)])))
        row-offset (apply min (mapv first current-tetrimino-coords))
        col-offset (apply min (mapv second current-tetrimino-coords))
        player-row-col [row-offset col-offset]
        normalised-tetrimino-coords (mapv (fn [[row col]]
                                            [(- row row-offset)
                                             (- col col-offset)]) current-tetrimino-coords)
        current-tetrimino-type (key (first (filter (fn [[_ tetriminos]]
                                                     ((set tetriminos) normalised-tetrimino-coords))
                                                   tetrimino-shape-normalised-coords)))
        current-tetrimino-idx (first (keep-indexed (fn [idx elem] (when (= elem normalised-tetrimino-coords) idx))
                                                   (get tetrimino-shape-normalised-coords
                                                        current-tetrimino-type)))]
    {:game-grid game-grid
     :current-tetrimino (get-in sut/tetrimino-shapes [current-tetrimino-type current-tetrimino-idx])
     :player-row-col player-row-col}))

(deftest game-state-from-before-grid-test
  (testing "within empty game-grid context"
    (is (= {:game-grid [[0 0 0 0 0 0 0 0 0 0]
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
                        [0 0 0 0 0 0 0 0 0 0]]
            :current-tetrimino [[1]
                                [1]
                                [1]
                                [1]]
            :player-row-col [0 0]}
           (game-state-from-before-grid [[* 0 0 0 0 0 0 0 0 0]
                                         [* 0 0 0 0 0 0 0 0 0]
                                         [* 0 0 0 0 0 0 0 0 0]
                                         [* 0 0 0 0 0 0 0 0 0]
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
                                         [0 0 0 0 0 0 0 0 0 0]])))

    (is (= {:game-grid [[0 0 0 0 0 0 0 0 0 0]
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
                        [0 0 0 0 0 0 0 0 0 0]]
            :current-tetrimino [[7 7 0]
                                [0 7 7]]
            :player-row-col [2 3]}
           (game-state-from-before-grid [[0 0 0 0 0 0 0 0 0 0]
                                         [0 0 0 0 0 0 0 0 0 0]
                                         [0 0 0 * * 0 0 0 0 0]
                                         [0 0 0 0 * * 0 0 0 0]
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
                                         [0 0 0 0 0 0 0 0 0 0]])))

    (is (= {:game-grid [[0 0 0 0 0 0 0 0 0 0]
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
                        [0 0 0 0 0 0 0 0 0 0]]
            :current-tetrimino [[2 2]
                                [2 2]]
            :player-row-col [18 8]}
           (game-state-from-before-grid [[0 0 0 0 0 0 0 0 0 0]
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
                                         [0 0 0 0 0 0 0 0 * *]
                                         [0 0 0 0 0 0 0 0 * *]]))))

  (testing "within partially populated game-grid context"
    (is (= {:game-grid [[0 1 1 1 0 0 0 0 0 0]
                        [0 1 1 1 0 0 0 0 0 0]
                        [0 1 1 1 0 0 0 0 0 0]
                        [0 1 1 1 0 0 0 0 0 0]
                        [2 2 2 2 0 0 0 0 0 0]
                        [2 2 2 2 0 0 0 0 0 0]
                        [2 2 2 2 0 0 0 0 0 0]
                        [2 2 2 2 0 0 0 0 0 0]
                        [2 2 2 2 0 0 0 0 0 0]
                        [2 2 2 2 0 0 0 0 0 0]
                        [2 2 2 2 0 0 0 0 0 0]
                        [2 2 2 2 0 0 0 0 0 0]
                        [2 2 2 2 0 0 0 0 0 0]
                        [2 2 2 2 0 0 0 0 0 0]
                        [2 2 2 2 0 0 0 0 0 0]
                        [2 2 2 2 0 0 0 0 0 0]
                        [2 2 2 2 0 0 0 0 0 0]
                        [2 2 2 2 0 0 0 0 0 0]
                        [2 2 2 2 0 0 0 0 0 0]
                        [2 2 2 2 0 0 0 0 0 0]]
            :current-tetrimino [[1]
                                [1]
                                [1]
                                [1]]
            :player-row-col [0 0]}
           (game-state-from-before-grid [[* 1 1 1 0 0 0 0 0 0]
                                         [* 1 1 1 0 0 0 0 0 0]
                                         [* 1 1 1 0 0 0 0 0 0]
                                         [* 1 1 1 0 0 0 0 0 0]
                                         [2 2 2 2 0 0 0 0 0 0]
                                         [2 2 2 2 0 0 0 0 0 0]
                                         [2 2 2 2 0 0 0 0 0 0]
                                         [2 2 2 2 0 0 0 0 0 0]
                                         [2 2 2 2 0 0 0 0 0 0]
                                         [2 2 2 2 0 0 0 0 0 0]
                                         [2 2 2 2 0 0 0 0 0 0]
                                         [2 2 2 2 0 0 0 0 0 0]
                                         [2 2 2 2 0 0 0 0 0 0]
                                         [2 2 2 2 0 0 0 0 0 0]
                                         [2 2 2 2 0 0 0 0 0 0]
                                         [2 2 2 2 0 0 0 0 0 0]
                                         [2 2 2 2 0 0 0 0 0 0]
                                         [2 2 2 2 0 0 0 0 0 0]
                                         [2 2 2 2 0 0 0 0 0 0]
                                         [2 2 2 2 0 0 0 0 0 0]])))

    (is (= {:game-grid [[0 0 0 0 0 0 0 0 0 0]
                        [0 0 0 0 0 0 0 0 0 0]
                        [0 7 7 0 0 0 0 0 0 0]
                        [0 0 7 7 0 0 0 0 0 0]
                        [2 2 2 2 2 2 2 2 2 2]
                        [2 2 2 2 2 2 2 2 2 2]
                        [2 2 2 2 2 2 2 2 2 2]
                        [2 2 2 2 2 2 2 2 2 2]
                        [2 2 2 2 2 2 2 2 2 2]
                        [2 2 2 2 2 2 2 2 2 2]
                        [2 2 2 2 2 2 2 2 2 2]
                        [2 2 2 2 2 2 2 2 2 2]
                        [2 2 2 2 2 2 2 2 2 2]
                        [2 2 2 2 2 2 2 2 2 2]
                        [2 2 2 2 2 2 2 2 2 2]
                        [2 2 2 2 2 2 2 2 2 2]
                        [2 2 2 2 2 2 2 2 2 2]
                        [2 2 2 2 2 2 2 2 2 2]
                        [2 2 2 2 2 2 2 2 2 2]
                        [2 2 2 2 2 2 2 2 2 2]]
            :current-tetrimino [[7 7 0]
                                [0 7 7]]
            :player-row-col [2 3]}
           (game-state-from-before-grid [[0 0 0 0 0 0 0 0 0 0]
                                         [0 0 0 0 0 0 0 0 0 0]
                                         [0 7 7 * * 0 0 0 0 0]
                                         [0 0 7 7 * * 0 0 0 0]
                                         [2 2 2 2 2 2 2 2 2 2]
                                         [2 2 2 2 2 2 2 2 2 2]
                                         [2 2 2 2 2 2 2 2 2 2]
                                         [2 2 2 2 2 2 2 2 2 2]
                                         [2 2 2 2 2 2 2 2 2 2]
                                         [2 2 2 2 2 2 2 2 2 2]
                                         [2 2 2 2 2 2 2 2 2 2]
                                         [2 2 2 2 2 2 2 2 2 2]
                                         [2 2 2 2 2 2 2 2 2 2]
                                         [2 2 2 2 2 2 2 2 2 2]
                                         [2 2 2 2 2 2 2 2 2 2]
                                         [2 2 2 2 2 2 2 2 2 2]
                                         [2 2 2 2 2 2 2 2 2 2]
                                         [2 2 2 2 2 2 2 2 2 2]
                                         [2 2 2 2 2 2 2 2 2 2]
                                         [2 2 2 2 2 2 2 2 2 2]])))

    (is (= {:game-grid [[0 0 0 0 0 0 0 0 0 0]
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
                        [0 0 0 0 0 0 2 2 0 0]
                        [0 0 0 0 0 0 2 2 0 0]]
            :current-tetrimino [[2 2]
                                [2 2]]
            :player-row-col [18 8]}
           (game-state-from-before-grid [[0 0 0 0 0 0 0 0 0 0]
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
                                         [0 0 0 0 0 0 2 2 * *]
                                         [0 0 0 0 0 0 2 2 * *]])))))

;; Credit: phind.com > debugged by me 🤯
(defmacro check-scenario [before->after events]
  (let [before-grid (mapv #(mapv (fn [x] (if (= x '*) '* x)) %)
                          (mapv (fn [row] (take-while #(not= % '->) row)) before->after))
        after-grid (mapv #(drop-while (fn [token] (not= token '->)) %) before->after)
        after-grid (mapv #(into [] (rest %)) after-grid)]
    `(is (= ~after-grid
            (sut/game-state->visible-grid (sut/handle-events (game-state-from-before-grid
                                                              ~before-grid)
                                                             ~events))))))

(comment
  (macroexpand '(check-scenario
                 [[* * * * 0 0 0 0 0 0 -> 0 1 1 1 1 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]]
                 [:key-input/right])))

(deftest moving-current-tetrimino-test
  (testing "basic positive test"
    (check-scenario
     [[* * * * 0 0 0 0 0 0 -> 0 0 1 1 1 1 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]]
     [:key-event/right
      :key-event/right]))

  (testing "boundary limits"
    (testing "at rhs"
      (check-scenario
       [[0 0 0 0 0 0 0 * * * -> 0 0 0 0 0 0 0 3 3 3]
        [0 0 0 0 0 0 0 0 * 0 -> 0 0 0 0 0 0 0 0 3 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]]
       [:key-event/right]))

    (testing "at lhs"
      (check-scenario
       [[* * * 0 0 0 0 0 0 0 -> 3 3 3 0 0 0 0 0 0 0]
        [0 * 0 0 0 0 0 0 0 0 -> 0 3 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]]
       [:key-event/left]))

    (testing "at bottom"
      (check-scenario
       [[0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 0 0 0 0 0 0 0 -> 0 0 0 0 0 0 0 0 0 0]
        [0 0 0 * * * 0 0 0 0 -> 0 0 0 3 3 3 0 0 0 0]
        [0 0 0 0 * 0 0 0 0 0 -> 0 0 0 0 3 0 0 0 0 0]]
       [:key-event/down]))))

(deftest game-grid-peaks-test
  (testing "empty grid"
    (is (= [nil nil nil nil nil nil nil nil nil nil]
           (sut/peaks [[0 0 0 0 0 0 0 0 0 0]
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
                       [0 0 0 0 0 0 0 0 0 0]]))))

  (testing "full grid"
    (is (= [0 0 0 0 0 0 0 0 0 0]
           (sut/peaks [[2 2 2 2 2 2 2 2 2 2]
                       [2 2 2 2 2 2 2 2 2 2]
                       [2 2 2 2 2 2 2 2 2 2]
                       [2 2 2 2 2 2 2 2 2 2]
                       [2 2 2 2 2 2 2 2 2 2]
                       [2 2 2 2 2 2 2 2 2 2]
                       [2 2 2 2 2 2 2 2 2 2]
                       [2 2 2 2 2 2 2 2 2 2]
                       [2 2 2 2 2 2 2 2 2 2]
                       [2 2 2 2 2 2 2 2 2 2]
                       [2 2 2 2 2 2 2 2 2 2]
                       [2 2 2 2 2 2 2 2 2 2]
                       [2 2 2 2 2 2 2 2 2 2]
                       [2 2 2 2 2 2 2 2 2 2]
                       [2 2 2 2 2 2 2 2 2 2]
                       [2 2 2 2 2 2 2 2 2 2]
                       [2 2 2 2 2 2 2 2 2 2]
                       [2 2 2 2 2 2 2 2 2 2]
                       [2 2 2 2 2 2 2 2 2 2]
                       [2 2 2 2 2 2 2 2 2 2]]))))

  (testing "partial grid"
    (is (= [nil 15 13 12 10 10 10 10 11 8]
           (sut/peaks [[0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 0 0 0 0 0 0 1]
                       [0 0 0 0 0 0 0 0 0 1]
                       [0 0 0 0 4 4 4 3 0 1]
                       [0 0 0 0 4 0 3 3 3 1]
                       [0 0 0 2 2 2 2 2 2 2]
                       [0 0 5 2 2 2 2 2 2 2]
                       [0 0 5 2 2 2 2 2 2 2]
                       [0 5 5 2 2 2 2 2 2 2]
                       [0 2 2 2 2 2 2 2 2 2]
                       [0 2 2 2 2 2 2 2 2 2]
                       [0 2 2 2 2 2 2 2 2 2]
                       [0 2 2 2 2 2 2 2 2 2]])))))
