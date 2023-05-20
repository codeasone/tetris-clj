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
      1 [:div {:class (classes common-cell-classes "bg-cyan")}]
      2 [:div {:class (classes common-cell-classes "bg-yellow")}]
      3 [:div {:class (classes common-cell-classes "bg-magenta")}]
      4 [:div {:class (classes common-cell-classes "bg-orange")}]
      5 [:div {:class (classes common-cell-classes "bg-blue")}]
      6 [:div {:class (classes common-cell-classes "bg-green")}]
      7 [:div {:class (classes common-cell-classes "bg-red")}])))

(defn message [{:keys [game-status] :as game-state}]
  [:h1 {:class "font-bold"}
   (if (logic/game-over? game-state)
     "Reload 🔁 to play another round..."
     (case game-status
       :game-status/initialised "Press ENTER key to play"
       :game-status/playing "TETRIS"))])

(defn statistics [{:keys [game-level game-score]}]
  [:div {:class "flex"}
   [:h1 {:class "font-bold mt-3"} (str "Level: " game-level)]
   [:h1 {:class "font-bold mt-3 ml-6"} (str "Score: " game-score)]])

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
    (events/listen!)
    [:div {:class "bg-gray-100 h-screen"}
     [:div {:class "flex flex-col items-center pt-6"}
      [message game-state]
      [grid game-state]
      [statistics game-state]]]))
