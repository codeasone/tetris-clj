(ns tetris.timers
  (:require [com.fulcrologic.guardrails.core :refer [>defn]]
            [tetris.keys :as keys]
            [tetris.logic :as logic]))

(def step-timer (atom nil))
(def speed-up-timer (atom nil))

(def initial-step-interval-ms 1000)
(def step-interval-ms (atom initial-step-interval-ms))
(def speed-up-interval-ms 30000)
(def speed-up-factor 0.75)

(defn establish-initial-step-timer! []
  (when-not @step-timer
    (reset! step-timer (.setInterval
                        js/window
                        (fn [] (keys/dispatch keys/down))
                        initial-step-interval-ms))))

(defn speed-up-by-20-percent! [game-state]
  (when-let [active-step-timer @step-timer]
    (swap! game-state #(update-in % [:game-level] inc))
    (.clearTimeout js/window active-step-timer)
    (reset! step-timer (.setInterval
                        js/window
                        (fn [] (keys/dispatch keys/down))
                        (swap! step-interval-ms #(Math/floor (* @step-interval-ms speed-up-factor)))))))

(defn establish-speed-up-timer! [game-state]
  (when-not @speed-up-timer
    (reset! speed-up-timer (.setInterval
                            js/window
                            (partial speed-up-by-20-percent! game-state)
                            speed-up-interval-ms))))

(def next-tetrimino-scheduled (atom nil))

(def next-tetrimino-grace-ms 500)

(>defn play-next-tetrimino
  [game-state before-game-state]
  [any? ::logic/game-state => ::logic/game-state]
  (reset! next-tetrimino-scheduled (.setTimeout
                                    js/window
                                    #(do
                                       (reset! game-state (logic/play-next-tetrimino @game-state))
                                       (reset! next-tetrimino-scheduled nil))
                                    next-tetrimino-grace-ms))
  before-game-state)

(defn play-next-tetrimino-debounce! [game-state]
  (when @next-tetrimino-scheduled
    (.clearTimeout js/window @next-tetrimino-scheduled)
    (reset! next-tetrimino-scheduled (.setTimeout
                                      js/window
                                      #(do
                                         (reset! game-state (logic/play-next-tetrimino @game-state))
                                         (reset! next-tetrimino-scheduled nil))
                                      next-tetrimino-grace-ms))))

(defn clear-all! []
  (when-let [active-step-timer @step-timer]
    (.clearTimeout js/window active-step-timer)
    (reset! step-timer nil))

  (when-let [active-speed-up-timer @speed-up-timer]
    (.clearTimeout js/window active-speed-up-timer)
    (reset! speed-up-timer nil))

  (when-let [active-next-tetrimino-scheduled @next-tetrimino-scheduled]
    (.clearTimeout js/window active-next-tetrimino-scheduled)
    (reset! next-tetrimino-scheduled nil)))
