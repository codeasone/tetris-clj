(ns tetris.app
  (:require [reagent.dom.client :as rdom]
            [tetris.components :as components]))

(defonce root (rdom/create-root (js/document.getElementById "root")))

(defn ^:export ^:dev/after-load init []
  (rdom/render root [components/tetris]))
