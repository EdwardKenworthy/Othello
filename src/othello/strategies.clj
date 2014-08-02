;;;;
;;;; OTHELLO GAME STRATEGIES
;;;;

(ns othello.strategies
  (:require [othello.core :refer :all]))

(defn legal-moves
  "Return an array of legal moves for player"
  [board player]
  (for [row [1 2 3 4 5 6 7 8] column [1 2 3 4 5 6 7 8]
      :when (legal-move? board player [column row])]
      [column row]))

;(legal-moves starting-position :black)
;(legal-moves full-board :black)
;(random-strategy full-board :black)
;(legal-moves full-board :black)

(defn random-strategy
  "Returns a random legal move:
  simple, but not a very effective Othello playing strategy."
  [board player]
  (rand-nth (legal-moves board player)))

;(rand-nth [[0 1][1 1][1 2][2 3]])
;(rand-nth (legal-moves initial-board :black))
;(random-strategy initial-board :black)

(defn maximiser
  "Return a strategy that will consider every legal move,
  apply eval-fn to each resulting board, and choose
  the move for which eval-fn returns the best score.
  FN takes two arguments: the board and the player-to-move."
  [eval-fn]
  (fn
    [board player]
    (let [moves (legal-moves board player)
          scores (map (fn [move]
                          (eval-fn (make-move board move player)
                                   player))
                          moves)
          best-index (first (apply max-key second (map-indexed vector scores)))]
      (nth moves best-index))))

;
; Calculates the weighted position of the given board for the given player
;

(def ^:const weights
                    [[0   0   0   0   0   0   0   0   0   0]
                     [0 120 -20  20   5   5  20 -20 120   0]
                     [0 -20 -40  -5  -5  -5  -5 -40 -20   0]
                     [0  20  -5  15   3   3  15  -5  20   0]
                     [0   5  -5   3   3   3   3  -5   5   0]
                     [0   5  -5   3   3   3   3  -5   5   0]
                     [0  20  -5  15   3   3  15  -5  20   0]
                     [0 -20 -40  -5  -5  -5  -5 -40 -20   0]
                     [0 120 -20  20   5   5  20 -20 120   0]
                     [0   0   0   0   0   0   0   0   0   0]])

(defn weight-this-square
  [player square-piece square-weight]
  (cond
         (= square-piece player) square-weight
         (= square-piece (opponent player))  (- square-weight)
         :else 0))

(defn weight-row
  [player row row-weights]
  (reduce + (map (partial weight-this-square player) row row-weights)))

(defn weighted-squares
  "An eval-fn to use with the maximiser function that will generate a
  strategy that maximises the weighted score (using weights)."
  [board player]
  (reduce + (map (partial weight-row player) board weights)))

;;
;; Minimax
;;

(def winning-value 100000)
(def losing-value -100000)
(def draw-value 0)

(defn- final-value
  "Is this a win, loss or draw for player?"
  [board player]
  (let [score (count-difference board player)]
  (cond
     (neg? score) losing-value
     (pos? score) winning-value
     :else draw-value)))

; (bigger [1 [1 2]] [10 [3 4]])

(defn- convert
  "Converts the value for an opposing player's
  evaluated move by negating the value component"
  [[value move]]
  [(- value) move])

(defn- bigger
  "Compares two [value move] and returns the one with the bigger value.
  Returns the second one (different move) if they have the same value."
  [[val-1 _ :as val-mv-1] [val-2 _ :as val-mv-2]]
  (if (> val-1 val-2)
    val-mv-1
    val-mv-2))

#_(declare minimax)

#_(defn- best-move
  "Returns the best move out of MOVES for the player
  as a 2 element array [value move]"
  [board moves player ply eval-fn]
  (reduce bigger
    (for [move moves] ; create a vector of 2 element vectors of value and move.
      [(- (first (minimax (make-move board move player)
                        (opponent player)
                        (dec ply)
                        eval-fn))) ; note that we deliberately do *not*
                                   ; use the move returned by minimax
                                   ; as that is the *opponent's* move.
      move])))

(defn- minimax
  "Find the best move for PLAYER, according to EVAL-FN,
  searching PLY levels deep and backing up values."
  [board player ply eval-fn]
  (if (zero? ply)
    [(eval-fn board player) nil]
    (let [moves (legal-moves board player)]
      (if (empty? moves)
        (if (any-legal-move? board (opponent player))
          (convert (minimax board (opponent player) (dec ply) eval-fn))
          [(final-value board player) nil])
;        (best-move board moves player ply eval-fn)
        (reduce bigger
          (for [move moves] ; create a vector of 2 element vectors of value and move.
               [(- (first (minimax (make-move board move player)
                                   (opponent player)
                                   (dec ply)
                                   eval-fn))) ; note that we deliberately do *not*
                                              ; use the move returned by minimax
                                              ; as that is the *opponent's* move.
                  move]))

        ))))

;(minimax starting-position :black 8 weighted-squares)
;(final-value full-board :white)

(defn minimax-searcher
  "Returns a strategy function based on minimax."
  [ply eval-fn]
  (fn
    [board player]
    (nth (minimax board player ply eval-fn) 1)))

; (minimax-searcher 3 weighted-squares)

;;
;; Minimax with alpha-beta pruning.
;;

#_(declare alpha-beta)

#_(defn- best-move-alpha-beta
  "Returns the best move out of MOVES for the player
  as a 2 element array [value move]"
  [board moves player achievable cutoff ply eval-fn]
  (loop  [mvs moves
          current-achievable achievable
          best-move (first moves)]
    (if (empty? mvs)
      [current-achievable best-move]
      (let [move (first mvs)
            value (- (first (alpha-beta (make-move board move player)
                                        (opponent player) (- cutoff)
                                        (- achievable) (dec ply) eval-fn)))]
        (if (> value current-achievable)
          (if (>= value cutoff)
              [value move]
              (recur (rest mvs) value move))
          (recur (rest mvs) current-achievable best-move))))))

(defn alpha-beta
  "Find the best move, for PLAYER, according to EVAL-FN,
  searching PLY levels deep and backing up values,
  using cutoffs whenever possible."
  [board player achievable cutoff ply eval-fn]
  (if (zero? ply)
    [(eval-fn board player) nil]
    (let [moves (legal-moves board player)]
      (if (empty? moves)
        (if (any-legal-move? board (opponent player))
          ; player's turn skipped, opponent plays again
          (convert (alpha-beta board (opponent player)
                               (- cutoff) (- achievable)
                               (dec ply) eval-fn))
          ; Neither player nor opponent has a move: game over
          [(final-value board player) nil])
        ; player has at least one legal move, which is the best?
;        (best-move-alpha-beta board moves player achievable cutoff ply eval-fn)
        (loop  [mvs moves current-achievable achievable best-move (first moves)]
          (if (empty? mvs)
            [current-achievable best-move]
            (let [move (first mvs)
                  value (- (first (alpha-beta (make-move board move player)
                                              (opponent player) (- cutoff)
                                              (- achievable) (dec ply) eval-fn)))]
              (if (> value current-achievable)
                (if (>= value cutoff)
                  [value move]
                  (recur (rest mvs) value move))
                (recur (rest mvs) current-achievable best-move)))))

        ))))

; (minimax starting-position :black 8 weighted-squares)
;(time (trampoline (alpha-beta starting-position :black losing-value winning-value 8 weighted-squares)))

(defn alpha-beta-searcher
  "Returns a strategy that searches to PLY and uses EVAL-FN."
  [ply eval-fn]
  (fn
    [board player]
    (second (alpha-beta board player losing-value winning-value ply eval-fn))))

;;
;; Modified weighted-squares
;;

;; Bit boring

#_(defn modified-weighted-squares
  "Like WEIGHTED-SQUARES, but don't take off
  for moving near an occupied corner."
  [board player]

  (let [w (weighted-squares player board)]
    (dolist [corner [11 18 81 88]]
      (when-not (eql (bref board corner) :empty)
        (dolist [c (neighbours corner)]

                (when-not (eql (bref board c) :empty))
                  (incf w (* (- 5 (aref *weights* c))
                             (if (eql (bref bard c) player)
                               +1
                               -1
                               ))))))
    w))
