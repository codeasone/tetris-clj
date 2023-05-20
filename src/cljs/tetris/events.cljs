(ns tetris.events
  (:require [reagent.core :as r]
            [tetris.events :as events]
            [tetris.keys :as keys]
            [tetris.logic :as logic]
            [tetris.timers :as timers]))

(defonce game-state (r/atom (logic/initial-game-state)))

(defn handle-key-event! [event]
  (when-not (logic/game-over? @game-state)
    (let [key-code (.-keyCode event)]
      (cond
        (= key-code keys/enter)
        (do
          (timers/establish-initial-step-timer!)
          (timers/establish-speed-up-timer! game-state)
          (reset! game-state (logic/start-playing @game-state)))

        ;; ESC/ENTER are like PAUSE/RESUME - helpful during exploratory testing
        (= key-code keys/escape)
        (do
          (timers/clear-all!)
          @game-state)

        (= key-code keys/space)
        (when-not @timers/next-tetrimino-scheduled
          (reset! game-state (logic/handle-events @game-state [::logic/drop]
                                                  (partial timers/play-next-tetrimino game-state))))

        (= key-code keys/down)
        (when-not @timers/next-tetrimino-scheduled
          (reset! game-state (logic/handle-events @game-state [::logic/move-down]
                                                  (partial timers/play-next-tetrimino game-state))))

        :else
        (do
          (timers/play-next-tetrimino-debounce! game-state)
          (cond
            (= key-code keys/left)
            (reset! game-state (logic/handle-events @game-state [::logic/move-left]))

            (= key-code keys/up)
            (reset! game-state (logic/handle-events @game-state [::logic/rotate]))

            (= key-code keys/right)
            (reset! game-state (logic/handle-events @game-state [::logic/move-right]))

            :else
            @game-state))))))

(defn listen! []
  (set! (.-onkeydown js/document) handle-key-event!))
