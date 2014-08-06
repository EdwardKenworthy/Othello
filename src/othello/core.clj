;;;;
;;;; OTHELLO GAME
;;;;

(ns othello.core)

;;;
;;; Othello board and functions to operate on them.
;;;

(def ^:constant starting-position
  "Othello board setup in the starting position."
  '[
   [:edge :edge  :edge  :edge  :edge  :edge  :edge  :edge  :edge  :edge]
   [:edge :empty :empty :empty :empty :empty :empty :empty :empty :edge]
   [:edge :empty :empty :empty :empty :empty :empty :empty :empty :edge]
   [:edge :empty :empty :empty :empty :empty :empty :empty :empty :edge]
   [:edge :empty :empty :empty :white :black :empty :empty :empty :edge]
   [:edge :empty :empty :empty :black :white :empty :empty :empty :edge]
   [:edge :empty :empty :empty :empty :empty :empty :empty :empty :edge]
   [:edge :empty :empty :empty :empty :empty :empty :empty :empty :edge]
   [:edge :empty :empty :empty :empty :empty :empty :empty :empty :edge]
   [:edge :edge  :edge  :edge  :edge  :edge  :edge  :edge  :edge  :edge]])

(def ^:constant full-board
  "Othello full board for testing."
  '[
   [:edge :edge  :edge  :edge  :edge  :edge  :edge  :edge  :edge  :edge]
   [:edge :black :black :black :black :black :black :black :black :edge]
   [:edge :empty :black :white :black :black :black :black :black :edge]
   [:edge :black :black :black :black :black :black :black :black :edge]
   [:edge :black :black :black :black :black :black :black :black :edge]
   [:edge :black :black :black :black :black :black :black :black :edge]
   [:edge :black :black :black :black :black :black :black :black :edge]
   [:edge :black :black :black :black :black :black :black :black :edge]
   [:edge :black :black :black :black :black :black :black :black :edge]
   [:edge :edge  :edge  :edge  :edge  :edge  :edge  :edge  :edge  :edge]])

(def ^:constant all-directions
  "Defines the array of all possible directions from a square."
  [[-1 -1][0 -1][+1 -1]
   [-1  0]      [+1  0]
   [-1 +1][0 +1][+1 +1]])

(def opponent
  "'function' that returns the player's opponent."
  {:black :white :white :black})

(defn board-columns [board] (count (first board)))
(defn board-rows [board] (count board))

(defn- valid-move?
  "Valid moves are inside the board"
  [board [x y]]
  (and (< y (board-rows board)) (< x (board-columns board))))

(defn- place-piece
  "Takes a board (a 2d array y high, x wide) and places the given piece at the required position
  and returns the new board.
  Note that it does not apply the rules of Othello.
  Will throw exceptions if it is outside the board and can mess-up the board."
  [board [x y] player]
  (assoc-in board [y x] player))

(defn get-piece
  "Returns the piece at the given co-ords in the given board"
  [board [x y]]
  ((board y) x))

;;
;; Implements the rules of Othello
;;

(defn- find-bracketing-piece
  "Return the square co-ords [x y] of the bracketing piece,
  nil if there isn't one."
  [board [x y :as square] player [dx dy :as direction]]
  (cond
    (= (get-piece board square) player)
      square
    (= (get-piece board square) (opponent player))
      (recur board [(+ x dx) (+ y dy)] player direction)
    :else nil))

(defn- would-flip?
  "Would this move result in any flips in this direction?
  If so, return the square of the bracketing piece."
  [board [x y] player [dx dy :as direction]]
  ;; A flip occurs if, starting at the adjacent square, c, there
  ;; is a string of at least one opponent pieces, bracketed by
  ;; one of player's pieces
  (let   [c-x (+ x dx)
          c-y (+ y dy)]
    (and (= (get-piece board [c-x c-y]) (opponent player))
         (find-bracketing-piece board [(+ c-x dx) (+ c-y dy)] player direction))))

(defn legal-move?
  "A legal move must be into a valid empty square, and it must flip at least one opponent piece"
  [board player move]
  (and (valid-move? board move)
       (= (get-piece board move) :empty)
       (some (partial would-flip? board move player) all-directions)))

(defn- make-flips
  "Make any flips in the given direction"
  [board [x y :as square] player [dx dy :as direction]]
;  (printf "Checking flips for %s from square [%d %d] in direction [%d %d]\n" player x y dx dy)
  (let [bracketer (would-flip? board square player direction)] ; bracketer will be either an array or the boolean false.
    (if-not bracketer
      board
      (loop [brd board
             flip-x (+ x dx)
             flip-y (+ y dy)]
        (if (and (= flip-x (bracketer 0)) (= flip-y (bracketer 1)))
          brd
          (recur (place-piece brd [flip-x flip-y] player) (+ flip-x dx)(+ flip-y dy)))))))

(defn make-move
  "Update board to reflect move by a player"
  [board move player]
  ;; First make the move, then make any flips
  (loop [brd (place-piece board move player)
         idx 0]
    (if (>= idx (count all-directions))
        brd
        (recur (make-flips brd move player (all-directions idx)) (inc idx)))))

;(print-board b1)
;(print-board (make-move b1 [1 1] :white))
;(find-bracketing-piece initial-board [4 5] :black [0 -1])
;(print-board initial-board)
;(would-flip? initial-board [4 5] :black [0 -1])
;(print-board (make-move initial-board [4 5] :black))
;(would-flip? initial-board [0 0] :black [+1 +1])

(defn count-pieces
  "Counts the number of player's pieces on the board."
  [board player]
  (reduce + (map (fn [row] (count (filter #(= % player) row))) board)))

(defn count-difference
  "Returns the difference between the player's and opponent's pieces on the board"
  [board player]
  (- (count-pieces board player) (count-pieces board (opponent player))))

; (count-difference initial-board :black)

;;;
;;; Game functions
;;;

(defn get-move
  "Call the player's strategy function to get a move.
  Keep calling until a valid and legal move is returned
  and pass that back.
  There is no way to escape without the strategy returning
  a valid and legal move."
  [board strategy player]
  (let [[x y :as move] (strategy board player)]
    (if (and (valid-move? board move) (legal-move? board player move))
      move ; return the move
      (do
        (printf "!Attempted illegal move [%d %d] by %s.\n" x y player)
        (recur board strategy player)))))

;(get-move initial-board random-strategy :black true)
;(make-move board move player)
;(print-board (get-and-make-move initial-board random-strategy :black true))
;(board-height initial-board)

(def all-squares
  "An array of all possible moves/squares on an 8x8 Othello board."
  (for [row [1 2 3 4 5 6 7 8] column [1 2 3 4 5 6 7 8]]
    [column row]))

(defn any-legal-move?
  "Does the player have any legal moves in this position (board)?"
  [board player]
  (some (partial legal-move? board player) all-squares))

(defn next-to-play
  "Compute the player to move next, or nil if nobody can move."
  [board previous-player]
  (let [opp (opponent previous-player)]
    (cond (any-legal-move? board opp)
            opp
          (any-legal-move? board previous-player)
;           (do
;             (println (previous-player " has no moves and must pass.")
              previous-player
;)
           :else nil))) ; neither player can make a legal move

;(next-to-play full-board :white true)
;(next-to-play starting-position :black true)
;(any-legal-move? full-board :black)
;(any-legal-move? starting-position :black)
;(print-board full-board)
;(any-legal-move? starting-position :white)
;(any-legal-move? full-board :black)
; (next-to-play full-board :white true)
;(if-not nil (printf "game over"))
; (next-to-play starting-position :white true)
; (next-to-play initial-board :white true)
