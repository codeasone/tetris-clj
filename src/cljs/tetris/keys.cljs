(ns tetris.keys)

(def enter 13)
(def escape 27)
(def space 32)
(def left 37)
(def up 38)
(def right 39)
(def down 40)

(defn dispatch [key-code]
  (let [synthetic-event (js/KeyboardEvent. "keydown" #js {:keyCode key-code})]
    (.dispatchEvent js/document synthetic-event)))

(comment
  (let [synthetic-event (js/KeyboardEvent. "keydown" #js {:keyCode 40})]
    (.dispatchEvent js/document synthetic-event))
  ;;
  )
