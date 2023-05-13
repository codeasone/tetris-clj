(ns tetris.app
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [reagent.dom.client :as rdom]
            [tetris.model :as model]))

(defonce game-state (r/atom (model/initial-game-state)))

(def handle-key-event
  (fn [event]
    (case (.-keyCode event)
      13 (js/alert "ENTER!")
      27 (js/alert "ESCAPE!")
      32 (js/alert "SPACE!")
      37 (reset! game-state (model/handle-left @game-state))
      38 (reset! game-state (model/handle-up @game-state))
      39 (reset! game-state (model/handle-right @game-state))
      40 (reset! game-state (model/handle-down @game-state))
      nil)))

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

(defn tetris []
  (into
   [:div {:class "flex flex-col"}]
   (for [row (model/compose-grid @game-state)]
     (into
      [:div {:class "flex"}]
      (for [cell-value row]
        [grid-cell cell-value])))))

(defonce root (rdom/create-root (js/document.getElementById "root")))

(defn ^:export ^:dev/after-load init []
  (rdom/render root [tetris])
  (set! (.-onkeydown js/document) handle-key-event))
