(ns tetris.events
  (:require [reagent.core :as r]
            [tetris.keys :as keys]
            [tetris.logic :as logic]
            [tetris.timers :as timers]))

(defonce game-state (r/atom (logic/initial-game-state)))

(defn handle-key-event! [event]
  (when-not (= :game-status/game-over (:game-status @game-state))
    (let [key-code (.-keyCode event)]
      (cond
        (= key-code keys/enter)
        (do
          (timers/establish-initial-step-timer!)
          (timers/establish-speed-up-timer! game-state)
          (reset! game-state (logic/start-playing @game-state)))

        (= key-code keys/escape)
        (timers/clear-all!)

        (= key-code keys/space) (reset! game-state (logic/handle-events @game-state [::logic/drop]))
        (= key-code keys/left) (reset! game-state (logic/handle-events @game-state [::logic/move-left]))
        (= key-code keys/up) (reset! game-state (logic/handle-events @game-state [::logic/rotate]))
        (= key-code keys/right) (reset! game-state (logic/handle-events @game-state [::logic/move-right]))
        (= key-code keys/down) (reset! game-state (logic/handle-events @game-state [::logic/move-down]))
        :else nil))))
