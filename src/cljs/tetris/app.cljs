(ns tetris.app
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [reagent.dom.client :as rdom]
            [tetris.keys :as keys]
            [tetris.logic :as logic]))

(defonce game-state (r/atom (logic/initial-game-state)))

(def step-timer (atom nil))
(def speed-up-timer (atom nil))

(def initial-step-interval-ms 1000)
(def step-interval-ms (atom initial-step-interval-ms))
(def speed-up-interval-ms 30000)
(def speed-up-factor 0.75)

(defn speed-up-by-20-percent! []
  (when-let [active-step-timer @step-timer]
    (.clearTimeout js/window active-step-timer)
    (reset! step-timer (.setInterval
                        js/window
                        (fn [] (keys/dispatch keys/down))
                        (swap! step-interval-ms #(Math/floor (* @step-interval-ms speed-up-factor)))))))

(defn clear-all-timers! []
  (when-let [active-step-timer @step-timer]
    (.clearTimeout js/window active-step-timer)
    (reset! step-timer nil))

  (when-let [active-speed-up-timer @speed-up-timer]
    (.clearTimeout js/window active-speed-up-timer)
    (reset! speed-up-timer nil)))

(defn handle-key-event! [event]
  (when-not (= :game-status/game-over (:game-status @game-state))
    (let [key-code (.-keyCode event)]
      (cond
        (= key-code keys/enter)
        (do
          (when-not @step-timer
            (reset! step-timer (.setInterval
                                js/window
                                (fn [] (keys/dispatch keys/down))
                                initial-step-interval-ms)))

          (when-not @speed-up-timer
            (reset! speed-up-timer (.setInterval
                                    js/window
                                    speed-up-by-20-percent!
                                    speed-up-interval-ms)))

          (reset! game-state (logic/start-playing @game-state)))

        (= key-code keys/escape)
        (clear-all-timers!)

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
     (clear-all-timers!)

     [:div {:class (classes "absolute top-1/2 left-1/2 z-10 px-4 py-2"
                            "bg-black text-white text-center rounded-lg"
                            "transform -translate-x-1/2 -translate-y-1/2")}
      "Game Over"])])

(defn tetris []
  (let [game-state @game-state]
    [:div {:class "flex flex-col items-center mt-6"}
     [message]
     [grid game-state]
     [score game-state]]))

(defonce root (rdom/create-root (js/document.getElementById "root")))

(defn ^:export ^:dev/after-load init []
  (rdom/render root [tetris])
  (set! (.-onkeydown js/document) handle-key-event!))
