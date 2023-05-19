(ns tetris.components
  (:require [clojure.string :as str]
            [tetris.events :as events]
            [tetris.logic :as logic]
            [tetris.timers :as timers]))

(defn classes
  [& strings]
  (str/join " " strings))

(defn grid-cell
  [num-value]
  (let [common-cell-classes "border h-6 w-6"]
    (case num-value
      0 [:div {:class (classes common-cell-classes "bg-white")}]
      1 [:div {:class (classes common-cell-classes "bg-blue-500")}]
      2 [:div {:class (classes common-cell-classes "bg-yellow-500")}]
      3 [:div {:class (classes common-cell-classes "bg-orange-500")}]
      4 [:div {:class (classes common-cell-classes "bg-red-500")}]
      5 [:div {:class (classes common-cell-classes "bg-green-500")}]
      6 [:div {:class (classes common-cell-classes "bg-pink-500")}]
      7 [:div {:class (classes common-cell-classes "bg-cyan-500")}])))

(defn message []
  [:h1 {:class "font-bold"} "Press ENTER key to play"])

(defn score [{:keys [game-score]}]
  [:h1 {:class "font-bold mt-3"} (str "Score: " game-score)])

(defn grid [game-state]
  [:div {:class "w-60 relative mt-3"}
   (into
    [:div {:class "flex flex-col border"}]
    (for [row (drop logic/lead-in-grid-height (logic/compose-current-tetrimino-into-game-grid game-state))]
      (into
       [:div {:class "flex"}]
       (for [cell-value row]
         [grid-cell cell-value]))))

   (when (logic/game-over? game-state)
     (timers/clear-all!)

     [:div {:class (classes "absolute top-1/2 left-1/2 z-10 px-4 py-2"
                            "bg-black text-white text-center rounded-lg"
                            "transform -translate-x-1/2 -translate-y-1/2")}
      "Game Over"])])

(defn tetris []
  (let [game-state @events/game-state]
    (set! (.-onkeydown js/document) events/handle-key-event!)
    [:div {:class "flex flex-col items-center mt-6"}
     [message]
     [grid game-state]
     [score game-state]]))
