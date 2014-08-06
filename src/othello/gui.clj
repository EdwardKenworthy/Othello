(ns othello.gui
  (:require [seesaw.core :refer [frame action menu border-panel canvas menubar native! repaint! select show!]]
            [seesaw.graphics :refer [draw ellipse style rect]]
            [seesaw.color :refer [color]]
            [othello.core :refer :all]
            [othello.strategies :refer :all]
            [othello.command-line :refer print-board]))

;
; GUI
;
(def ^:constant black (color 0 0 0 255))
(def ^:constant white (color 255 255 255 255))
(def ^:constant grey (color 127 127 127 255))

(def ^:constant name-of {:black "Black" :white "White"})

;(def current-board (atom starting-position))

#_(defn- print-board                                          ; debugging only
  "Print a board along with some statistics"
  [board]
  (newline)
  (println "\t    1  2  3  4  5  6  7  8  "
           (name-of :black) "=" (count-pieces board :black)
           (name-of :white) "=" (count-pieces board :white)
           " " (count-difference board :black))
  (dotimes [row (count board)]
    (print (* row 10) "\t")
    (doseq [cell (board row)]
      (print ({:black " b " :white " w " :edge " * "} cell " . ")))
    (newline) (newline) (flush)))

(defn- draw-board
  [c g board]
  (let [rows (board-rows board)
        columns (board-columns board)
        cell-height (/ (.getHeight c) rows)
        cell-width (/ (.getWidth c) columns)]
    (doseq [x (range columns) y (range rows)]
      (case (get-in board [y x])                            ; cell type
        :black (draw g
                     (ellipse (* x cell-width) (* y cell-height) cell-width cell-height)
                     (style :background black))
        :white (draw g
                     (ellipse (* x cell-width) (* y cell-height) cell-width cell-height)
                     (style :background white))
        :edge (draw g
                    (rect (* x cell-width) (* y cell-height) cell-width cell-height)
                    (style :background grey))
        "do nothing"))))

(defn- make-main-panel [the-board]
  (border-panel :hgap 5 :vgap 5 :border 5
                :center (canvas :id :canvas :background "#BBBBDD" :paint #(draw-board %1 %2 @the-board))))

(defn- make-menu []
  (let [a-exit (action :handler #(System/exit 0) :name "Exit" :tip "Exit Othello.")
        ]
    (menubar
      :items [(menu :text "File" :items [a-exit])
              ])))

(defn- make-frame [the-board]
  (frame :title "Othello"
         :width 500
         :height 500
         :content (make-main-panel the-board)
         :on-close :exit
         :menubar (make-menu)))

(defn- get-and-make-move
  "Gets a valid and legal move from the strategy
  and then makes it, returning the new board.
  This does what the PAIP 'get-move' function did."
  [board strategy player]
  (make-move board (get-move board strategy player) player))                         ; return the new board

(defn- play [the-board player-strategies player]
  "Takes a board atom, the player strategies to use as a map and the first player
  and plays a game of Othello with a 5 second delay between moves."
  (print-board @the-board)
  (Thread/sleep 5000)
  (swap! the-board #(get-and-make-move %1 (player-strategies player) player))
  (let [next-player (next-to-play @the-board player)]
    (if-not next-player
      (let [result (count-difference @the-board :black)]
        (printf "GAME OVER")
        (print-board @the-board)
        (newline)(flush)
        result)
        (recur the-board player-strategies next-player))))

#_(defn board-changed
      "called when current-board changes"
  [fr _ _ _ _]
  (repaint! (select fr [:#canvas])))

(defn othello-gui [board black-strategy white-strategy]
  "Creates the entire Othello GUI"
  (native!)
  (let [the-board (atom board)
        fr (make-frame the-board)]
    (add-watch the-board :board (fn [_ _ _ _] (repaint! (select fr [:#canvas]))))
    ;; this needs work to be able to display the final result
    (future (play the-board {:black black-strategy :white white-strategy} :black))
    (show! fr)))

; (defn -main [& args]
;  (othello-gui starting-position random-strategy random-strategy))

;(othello-gui starting-position random-strategy random-strategy)

;(othello-gui starting-position (minimax-searcher 3 weighted-squares) (minimax-searcher 3 weighted-squares))

