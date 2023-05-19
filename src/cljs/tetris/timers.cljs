(ns tetris.timers
  (:require [tetris.keys :as keys]))

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

(defn speed-up-by-20-percent! []
  (when-let [active-step-timer @step-timer]
    (.clearTimeout js/window active-step-timer)
    (reset! step-timer (.setInterval
                        js/window
                        (fn [] (keys/dispatch keys/down))
                        (swap! step-interval-ms #(Math/floor (* @step-interval-ms speed-up-factor)))))))

(defn establish-speed-up-timer! []
  (when-not @speed-up-timer
    (reset! speed-up-timer (.setInterval
                            js/window
                            speed-up-by-20-percent!
                            speed-up-interval-ms))))

(defn clear-all! []
  (when-let [active-step-timer @step-timer]
    (.clearTimeout js/window active-step-timer)
    (reset! step-timer nil))

  (when-let [active-speed-up-timer @speed-up-timer]
    (.clearTimeout js/window active-speed-up-timer)
    (reset! speed-up-timer nil)))
