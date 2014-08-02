;;;;
;;;; OTHELLO GAME COMMAND LINE UI
;;;;

(ns othello.command-line
  (:require [othello.core :refer :all]
            [othello.strategies :refer :all])
  (:gen-class))

(defn result-string
  "Converts the result into a string for display."
  [result]
  (cond
     (neg? result) "White wins."
     (pos? result) "Black wins."
     :else "Game is a draw."))

;;;
;;; View
;;;

(def name-of {:black "Black" :white "White"})

(defn print-board
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
    (newline)(newline)(flush)))

;(print-board starting-position)

;;
;; Converts between tournament notation (a1 to h8) and our internal mapping (11 to 88)
;;

(def square-names (for [a '[? a b c d e f g h ?]
                        b '[? 1 2 3 4 5 6 7 8 ?]]
                    (str a b)))

;(nth square-names 11)

(defn c88->h8
  [number]
  (if (number? number)
    (nth square-names num)
    number))

(defn h8->88
  [string]
  (let [number (.indexOf square-names string)]
    (if (neg? number)
      string
      number)))

; (h8->88 "??")
; (.indexOf square-names "h8")
; (.indexOf square-names "edward")

#_(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(defn human
  "A human player for the game of Othello"
  [_ player]
  (print (name-of player) "to move: ")
  (flush)
  (let [input (h8->88(read-line))]
    [(mod input 10)(/ (- input (mod input 10)) 10)]))

;;;
;;; The main command line game loop.
;;;

(defn get-and-make-move
  "Gets a valid and legal move from the strategy
  and then makes it, returning the new board.
  This does what the PAIP 'get-move' function did."
  [board strategy player print?]
  (when print? (print-board board))
  (let [[x y :as move] (get-move board strategy player)]
    (if print? (printf "%s moves to %d%d\n" (name-of player) y x))
    (make-move board move player))) ; return the new board

(defn othello
  "Play a game of Othello.
  Return the score, where a positive
  difference means black (the first player) wins."
  ; Note that it assumes that :black plays first and that there must be a valid first move.
  ; there is an improved loop in othello.gui (play) that fixes this and the fix should be ported back here
  [black-strategy white-strategy
   & {:keys [print? initial-board] :or {print? true initial-board starting-position}}]
  (loop [board (get-and-make-move initial-board black-strategy :black print?)
         player :black]
    (let [next-player (next-to-play board player)]
      (if-not next-player
        (let [result (count-difference board :black)]
          (when print?
            (printf "GAME OVER. Final result:\n")
            (print-board board)
            (print (result-string result))
            (newline))
          result)
        (recur (get-and-make-move board
                                  ({:black black-strategy :white white-strategy} next-player)
                                  next-player print?)
               next-player)))))

;(next-to-play full-board :black true)
;(count-pieces full-board :white)
;({:black black-strategy :white white-strategy} :black)

; use these to try out the game
#_(othello human random-strategy)

#_(othello (maximiser weighted-squares)
         (maximiser weighted-squares))

#_(othello (alpha-beta-searcher 9 count-difference)
         (alpha-beta-searcher 9 weighted-squares))

(defn -main
  "For playing a game of Othello at the command line."
  []
  (println "Running (othello (alpha-beta-searcher 8 count-difference) (alpha-beta-searcher 8 weighted-squares))...")
  (println "Press any ENTER to start.")
  (read-line)
  (othello (alpha-beta-searcher 8 count-difference)
           (alpha-beta-searcher 8 weighted-squares))
  )


