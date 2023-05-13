(ns tetris.client
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [reagent.dom.client :as rdom]
            [tetris.model :as model]))

(defonce game-state (r/atom {:game-grid model/empty-game-grid
                             :current-tetrimino (model/random-tetrimino)
                             :next-tetrimino (model/random-tetrimino)}))

(defn classes
  [& strings]
  (str/join " " strings))

(defn grid-cell
  [num-value]
  (let [common-cell-classes "border h-4 w-4"]
    (case num-value
      0 [:div {:class (classes common-cell-classes "bg-white")}]
      1 [:div {:class (classes common-cell-classes "bg-blue-500")}]
      2 [:div {:class (classes common-cell-classes "bg-yellow-500")}]
      3 [:div {:class (classes common-cell-classes "bg-orange-500")}]
      4 [:div {:class (classes common-cell-classes "bg-red-500")}]
      5 [:div {:class (classes common-cell-classes "bg-green-500")}]
      6 [:div {:class (classes common-cell-classes "bg-pink-500")}]
      7 [:div {:class (classes common-cell-classes "bg-cyan-500")}])))

(defn ^:dev/always tetris []
  (fn [_]
    (let [{:keys [game-grid current-tetrimino]} @game-state
          play-position (r/atom [0
                                 (rand-int (- model/grid-width (model/width-tetrimino current-tetrimino)))])]
      (into
       [:div {:class "flex flex-col"}]
       (for [row (model/compose-grid game-grid current-tetrimino @play-position)]
         (into
          [:div {:class "flex"}]
          (for [cell-value row]
            [grid-cell cell-value])))))))

(defonce root (rdom/create-root (js/document.getElementById "root")))

(defn ^:dev/after-load init []
  (println model/empty-game-grid)
  (rdom/render root [tetris]))
