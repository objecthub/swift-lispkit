;;; Connect 4
;;; 
;;; This is a terminal-based program implementing the classic "Connect 4"
;;; game. You play against a computer opponent that uses a minimax algorithm
;;; with alpha-beta pruning to choose its moves.
;;; 
;;; The board is represented with a vector of 42 cells (7 columns × 6 rows),
;;; stored in column-major order. Each cell is 0 (empty), 1 (player 1/human),
;;; or 2 (player 2/computer).
;;; 
;;; Enter (connect4) to get started playing against the computer player.
;;; Below are some details explaining how the implementation works.
;;; 
;;; 
;;; HOW IT WORKS
;;; ▔▔▔▔▔▔▔▔▔▔▔▔
;;; Board Representation
;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;;   • A flat vector of 42 cells (7 columns × 6 rows) in column-major order.
;;;   • Each cell is 0 (empty), 1 (human/X), or 2 (computer/O).
;;;   • Row 0 is the bottom of the board, row 5 is the top.
;;; 
;;; Display
;;; ‾‾‾‾‾‾‾
;;; The board is printed with column numbers 1–7 across the bottom. 'X' labels
;;; chips placed by the human, 'O' labels chips placed by the computer player,
;;; and '·' indicates empty cells:
;;; 
;;;   │ ·  ·  ·  ·  ·  ·  · │ 
;;;   │ ·  ·  ·  ·  ·  ·  · │ 
;;;   │ ·  ·  ·  ·  ·  ·  · │ 
;;;   │ ·  ·  ·  O  ·  ·  · │ 
;;;   │ ·  ·  X  O  ·  ·  · │ 
;;;   │ ·  X  X  O  X  ·  · │ 
;;;   ┕━━━━━━━━━━━━━━━━━━━━━┙ 
;;;     ¹  ²  ³  ⁴  ⁵  ⁶  ⁷   
;;; 
;;; Win Detection
;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾
;;; winner? scans every cell in four directions (horizontal, vertical, and
;;; two diagonals), checking for 4 consecutive chips of the same player
;;; via check-direction.
;;; 
;;; Computer Player: Minimax with Alpha-Beta Pruning
;;; ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;;; The computer player AI uses a classic minimax search with alpha-beta
;;; pruning to a configurable depth (AI-DEPTH, default 4):
;;; 
;;; ┌────────────────┬─────────────────────────────────────────────────────────┐
;;; │ Component      │ Purpose                                                 │
;;; ╞════════════════╪═════════════════════════════════════════════════════════╡
;;; │ minimax        │ Recursive search that alternates maximizing (computer)  │
;;; │                │ and minimizing (human) layers. Returns                  │
;;; │                │ (best-column . score).                                  │
;;; ├────────────────┼─────────────────────────────────────────────────────────┤
;;; │ evaluate-board │ Heuristic scoring at the depth limit. Scans all windows │
;;; │                │ of 4 cells and rewards patterns (3-in-a-row with an     │
;;; │                │ empty slot, center control, etc.).                      │
;;; ├────────────────┼─────────────────────────────────────────────────────────┤
;;; │ score-window   │ Scores a single 4-cell window: +100000 for 4-in-a-row,  │
;;; │                │ +10 for 3+empty, +3 for 2+2 empty, −8 for opponent's    │
;;; │                │ 3+empty.                                                │
;;; ├────────────────┼─────────────────────────────────────────────────────────┤
;;; │ Alpha-beta     │ Cuts off branches that cannot improve the outcome,      │
;;; │ pruning        │ making depth-6 search fast.                             │
;;; └────────────────┴─────────────────────────────────────────────────────────┘
;;; 
;;; User Input
;;; ‾‾‾‾‾‾‾‾‾‾
;;; human-move reads a line, trims whitespace, converts to a number, and
;;; validates the column is in range and not full. Invalid input loops with a
;;; helpful message.
;;; 
;;; Tuning
;;; ‾‾‾‾‾‾
;;;   • Increase AI-DEPTH (e.g. to 8 or 10) for a stronger but slower computer
;;;     opponent.
;;;   • Decrease to 4 for faster play on constrained hardware.
;;; 
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not
;;; use this file except in compliance with the License. You may obtain a copy
;;; of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;;; License for the specific language governing permissions and limitations
;;; under the License.

(import (lispkit base))

;; ----- Constants -----

(define COLDESIG '("¹" "²" "³" "⁴" "⁵" "⁶" "⁷"))
(define COLS (length COLDESIG))
(define ROWS 6)
(define EMPTY 0)
(define HUMAN 1)
(define COMPUTER 2)

;; Search depth for the AI (higher = stronger but slower)
(define AI-DEPTH 4)

;; ----- Board helpers -----

;; Create a fresh empty board
(define (make-board)
  (make-vector (* COLS ROWS) EMPTY))

;; Copy a board
(define (copy-board board)
  (vector-copy board))

;; Convert (col, row) to a flat index (column-major)
(define (cell-index col row)
  (+ (* col ROWS) row))

;; Read cell value at (col, row)
(define (board-ref board col row)
  (vector-ref board (cell-index col row)))

;; Set cell value at (col, row)
(define (board-set! board col row val)
  (vector-set! board (cell-index col row) val))

;; Find the lowest empty row in a column, or #f if full
(define (lowest-empty-row board col)
  (let loop ((r 0))
    (cond ((>= r ROWS) #f)
          ((= (board-ref board col r) EMPTY) r)
          (else (loop (+ r 1))))))

;; Drop a piece into a column; returns the row placed, or #f if full
(define (drop-piece! board col player)
  (let ((row (lowest-empty-row board col)))
    (when row
      (board-set! board col row player))
    row))

;; List of columns that are not yet full
(define (valid-moves board)
  (let loop ((c 0) (acc '()))
    (if (>= c COLS)
        (reverse acc)
        (loop (+ c 1)
              (if (lowest-empty-row board c)
                  (cons c acc)
                  acc)))))

;; ----- Win detection -----

;; Check if `player` has 4 in a row starting at (col, row)
;; in the direction (dc, dr)
(define (check-direction board player col row dc dr)
  (let loop ((i 0) (c col) (r row))
    (cond ((= i 4)
            #t)
          ((or (< c 0) (>= c COLS) (< r 0) (>= r ROWS))
         		#f)
          ((not (= (board-ref board c r) player))
            #f)
          (else
            (loop (+ i 1) (+ c dc) (+ r dr))))))

;; The four directions to check: horizontal, vertical, two diagonals
(define DIRECTIONS '((1 0) (0 1) (1 1) (1 -1)))

;; Check whether `player` has won
(define (winner? board player)
  (let col-loop ((c 0))
    (if (>= c COLS)
        #f
        (let row-loop ((r 0))
          (if (>= r ROWS)
              (col-loop (+ c 1))
              (or (let dir-loop ((dirs DIRECTIONS))
                    (cond ((null? dirs) #f)
                          ((check-direction board player c r
                                            (car (car dirs))
                                            (cadr (car dirs)))
                           #t)
                          (else (dir-loop (cdr dirs)))))
                  (row-loop (+ r 1))))))))

;; Is the board completely full?
(define (board-full? board)
  (null? (valid-moves board)))

;; ----- Display -----

;; Character for a cell
(define (cell-char val)
  (cond ((= val HUMAN)    "X")
        ((= val COMPUTER) "O")
        (else             "·")))

;; Print the board with column numbers along the top
;; Row 0 is the bottom; we display top-to-bottom
(define (display-board board)
  (newline)
  ;; board rows (top row = ROWS-1, bottom row = 0)
  (let row-loop ((r (- ROWS 1)))
    (when (>= r 0)
      (display " │")
      (let col-loop ((c 0))
        (when (< c COLS)
          (display " ")
          (display (cell-char (board-ref board c r)))
          (display " ")
          (col-loop (+ c 1))))
      (display "│ ")
      (newline)
      (row-loop (- r 1))))
  ;; separator line
  (display " ┕")
  (let loop ((c 0))
    (when (< c COLS)
      (display "━━━")
      (loop (+ c 1))))
  (display "┙ ")
  (newline)
  ;; column labels
  (display " ")
  (let loop ((c 0))
    (when (< c COLS)
      (display "  ")
      (display (list-ref COLDESIG c))
      (loop (+ c 1))))
  (display "   ")
  (newline))

;; ----- Evaluation (heuristic for AI) -----

;; Score a window of 4 cells for a given player
(define (score-window a b c d player)
  (let* ((opp    (if (= player HUMAN) COMPUTER HUMAN))
         (pcount (+ (if (= a player) 1 0)
                    (if (= b player) 1 0)
                    (if (= c player) 1 0)
                    (if (= d player) 1 0)))
         (ecount (+ (if (= a EMPTY) 1 0)
                    (if (= b EMPTY) 1 0)
                    (if (= c EMPTY) 1 0)
                    (if (= d EMPTY) 1 0)))
         (ocount (+ (if (= a opp) 1 0)
                    (if (= b opp) 1 0)
                    (if (= c opp) 1 0)
                    (if (= d opp) 1 0))))
    (cond ((= pcount 4)                     100000)
          ((and (= pcount 3) (= ecount 1))  10)
          ((and (= pcount 2) (= ecount 2))  3)
          ((and (= ocount 3) (= ecount 1)) -8)
          (else                             0))))

;; Evaluate the board from `player`'s perspective
(define (evaluate-board board player)
  (let ((total 0))
    ;; Center column preference
    (let loop ((r 0))
      (when (< r ROWS)
        (when (= (board-ref board 3 r) player)
          (set! total (+ total 4)))
        (loop (+ r 1))))
    ;; Horizontal windows
    (let cloop ((c 0))
      (when (<= c (- COLS 4))
        (let rloop ((r 0))
          (when (< r ROWS)
            (set! total
                  (+ total
                     (score-window (board-ref board c r)
                                   (board-ref board (+ c 1) r)
                                   (board-ref board (+ c 2) r)
                                   (board-ref board (+ c 3) r)
                                   player)))
            (rloop (+ r 1))))
        (cloop (+ c 1))))
    ;; Vertical windows
    (let cloop ((c 0))
      (when (< c COLS)
        (let rloop ((r 0))
          (when (<= r (- ROWS 4))
            (set! total
                  (+ total
                     (score-window (board-ref board c r)
                                   (board-ref board c (+ r 1))
                                   (board-ref board c (+ r 2))
                                   (board-ref board c (+ r 3))
                                   player)))
            (rloop (+ r 1))))
        (cloop (+ c 1))))
    ;; Positive diagonal (/) windows
    (let cloop ((c 0))
      (when (<= c (- COLS 4))
        (let rloop ((r 0))
          (when (<= r (- ROWS 4))
            (set! total
                  (+ total
                     (score-window (board-ref board c r)
                                   (board-ref board (+ c 1) (+ r 1))
                                   (board-ref board (+ c 2) (+ r 2))
                                   (board-ref board (+ c 3) (+ r 3))
                                   player)))
            (rloop (+ r 1))))
        (cloop (+ c 1))))
    ;; Negative diagonal (\) windows
    (let cloop ((c 0))
      (when (<= c (- COLS 4))
        (let rloop ((r 3))
          (when (< r ROWS)
            (set! total
                  (+ total
                     (score-window (board-ref board c r)
                                   (board-ref board (+ c 1) (- r 1))
                                   (board-ref board (+ c 2) (- r 2))
                                   (board-ref board (+ c 3) (- r 3))
                                   player)))
            (rloop (+ r 1))))
        (cloop (+ c 1))))
    total))

;; ----- Minimax with alpha-beta pruning -----

(define (minimax board depth alpha beta maximizing?)
  (let ((moves (valid-moves board)))
    (cond
      ;; Terminal: computer wins
      ((winner? board COMPUTER)
        (cons #f (+ 1000000 depth)))
      ;; Terminal: human wins
      ((winner? board HUMAN)
        (cons #f (- (+ 1000000 depth))))
      ;; Terminal: draw
      ((null? moves)
        (cons #f 0))
      ;; Depth limit reached – use heuristic
      ((= depth 0)
        (cons #f (evaluate-board board COMPUTER)))
      ;; Maximising (computer's turn)
      (maximizing?
        (let loop ((cols moves) (best-col (car moves)) (best-val -10000000))
          (if (null? cols)
              (cons best-col best-val)
              (let* ((c     (car cols))
                     (b2    (copy-board board))
                     (dp     (drop-piece! b2 c COMPUTER))
                     (score (cdr (minimax b2 (- depth 1) alpha beta #f)))
                     (new-best-val (max best-val score))
                     (new-best-col (if (> score best-val) c best-col))
                     (new-alpha    (max alpha new-best-val)))
                (if (>= new-alpha beta)
                    (cons new-best-col new-best-val)  ; prune
                    (loop (cdr cols) new-best-col new-best-val))))))
      ;; Minimising (human's turn)
      (else
        (let loop ((cols moves) (best-col (car moves)) (best-val 10000000))
          (if (null? cols)
              (cons best-col best-val)
              (let* ((c     (car cols))
                     (b2    (copy-board board))
                     (dp     (drop-piece! b2 c HUMAN))
                     (score (cdr (minimax b2 (- depth 1) alpha beta #t)))
                     (new-best-val (min best-val score))
                     (new-best-col (if (< score best-val) c best-col))
                     (new-beta     (min beta new-best-val)))
                (if (>= alpha new-beta)
                    (cons new-best-col new-best-val)  ; prune
                    (loop (cdr cols) new-best-col new-best-val)))))))))

;; Choose the best column for the computer
(define (computer-move board)
  (car (minimax board AI-DEPTH -10000000 10000000 #t)))

;; ----- Human input -----

;; Prompt the user for a valid column (1–7)
(define (human-move board)
  (display "Your move (1-7): ")
  (flush-output-port)
  (let* ((line (read-line))
         (num  (string->number line)))
    (cond
      ((or (equal? line "exit") (equal? line "q"))
        (raise 'aborted))
      ((or (not num) (not (fixnum? num)))
        (display "Invalid input. Enter an integer 1-7.\n")
        (human-move board))
      ((or (< num 1) (> num 7))
        (display "Column out of range. Enter 1-7.\n")
        (human-move board))
      ((not (lowest-empty-row board (- num 1)))
        (display "That column is full. Choose another.\n")
        (human-move board))
      (else
        (- num 1)))))

;; ----- Main game loop -----

;; Play one complete game; human is X (moves first), computer is O
(define (connect4)
  (let ((board (make-board)))
    (display "═══════╡ CONNECT 4 ╞═══════\n\n")
    (display "You are X, computer is O.\n")
    (display "Drop chips by entering a column number (1-7).\n")
    (display "Get 4 in a row (horizontal, vertical, or diagonal) to win!\n")
    (display-board board)
    (let loop ((turn HUMAN))
      (cond
        ;; Human's turn
        ((= turn HUMAN)
          (let* ((col (human-move board))
                 (row (drop-piece! board col HUMAN)))
            (display-board board)
            (cond ((winner? board HUMAN)
                    (display "\n*** YOU WIN! Congratulations! ***\n"))
                  ((board-full? board)
                    (display "\n*** It's a draw! ***\n"))
                  (else
                    (loop COMPUTER)))))
        ;; Computer's turn
        (else
          (display "Computer is thinking...\n")
          (let* ((col (computer-move board))
                 (row (drop-piece! board col COMPUTER)))
            (display (string-append "Computer plays column "
                                    (number->string (+ col 1))
                                    ".\n"))
            (display-board board)
            (cond ((winner? board COMPUTER)
                    (display "\n*** COMPUTER WINS! Better luck next time. ***\n"))
                  ((board-full? board)
                    (display "\n*** It's a draw! ***\n"))
                  (else
                    (loop HUMAN)))))))))

;; Start the game
(connect4)

