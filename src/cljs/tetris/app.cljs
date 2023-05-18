(ns tetris.app
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [reagent.dom.client :as rdom]
            [tetris.keys :as keys]
            [tetris.logic :as logic]))

(defonce game-state (r/atom (logic/initial-game-state)))

(def game-interval-ms 1000)

(defn handle-key-event [event]
  (when-not (= :game-status/game-over (:game-status @game-state))
    (let [key-code (.-keyCode event)]
      (cond
        (= key-code keys/enter)
        (reset! game-state
                (assoc @game-state :timer (or (:timer @game-state)
                                              (.setInterval
                                               js/window
                                               (fn [] (keys/dispatch keys/down))
                                               1000))))
        (= key-code keys/escape)
        (do
          (.clearTimeout js/window (:timer @game-state))
          (reset! game-state (assoc @game-state :timer nil)))
        (= key-code keys/space) (reset! game-state (logic/handle-events @game-state [::logic/drop]))
        (= key-code keys/left) (reset! game-state (logic/handle-events @game-state [::logic/move-left]))
        (= key-code keys/up) (reset! game-state (logic/handle-events @game-state [::logic/rotate]))
        (= key-code keys/right) (reset! game-state (logic/handle-events @game-state [::logic/move-right]))
        (= key-code keys/down) (reset! game-state (logic/handle-events @game-state [::logic/move-down]))
        :else nil))))

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

(defn grid []
  [:div {:class "w-40 relative"}
   (into
    [:div {:class "flex flex-col"}]
    (for [row (drop logic/lead-in-grid-height (logic/compose-current-tetrimino-into-game-grid @game-state))]
      (into
       [:div {:class "flex"}]
       (for [cell-value row]
         [grid-cell cell-value]))))

   (when (logic/game-over? @game-state)
     (.clearTimeout js/window (:timer @game-state))
     (reset! game-state (assoc @game-state :timer nil))
     [:div {:class (classes "absolute top-1/2 left-1/2 z-10 px-4 py-2"
                            "bg-black text-white text-center rounded-lg"
                            "transform -translate-x-1/2 -translate-y-1/2")}
      "Game Over"])])

(defn tetris []
  [grid])

(defonce root (rdom/create-root (js/document.getElementById "root")))

(defn ^:export ^:dev/after-load init []
  (rdom/render root [tetris])
  (set! (.-onkeydown js/document) handle-key-event))
