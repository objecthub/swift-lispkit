;;; Sudoku solver
;;;
;;; This is a port of Peter Norvig's Sudoku solver to idiomatic Scheme code. It uses
;;; some LispKit-specific libraries (e.g. R6RS-style hashtables), but can be easily translated
;;; to generic R7RS code. A detailed description of the Sudoku solver algorithm can be found at
;;; https://norvig.com/sudoku.html .
;;;
;;; The Sudoku solver uses a string to represent an initial, partially filled grid. For instance,
;;; the following string:
;;;
;;;   "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
;;;
;;; represents this Sudoku grid:
;;;
;;;   4 . . |. . . |8 . 5
;;;   . 3 . |. . . |. . .
;;;   . . . |7 . . |. . .
;;;   ------+------+------
;;;   . 2 . |. . . |. 6 .
;;;   . . . |. 8 . |4 . .
;;;   . . . |. 1 . |. . .
;;;   ------+------+------
;;;   . . . |6 . 3 |. 7 .
;;;   5 . . |2 . . |. . .
;;;   1 . 4 |. . . |. . .
;;;
;;; The grid can be solved using the `solve` function:
;;;
;;;  (define solution
;;;    (solve "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"))
;;;
;;; The solution can be printed with `display-grid`: `(display-grid solution)`
;;;
;;;  4 1 7 |3 6 9 |8 2 5
;;;  6 3 2 |1 5 8 |9 4 7
;;;  9 5 8 |7 2 4 |3 1 6
;;;  ------+------+------
;;;  8 2 5 |4 3 7 |1 6 9
;;;  7 9 1 |5 8 6 |4 3 2
;;;  3 4 6 |9 1 2 |7 5 8
;;;  ------+------+------
;;;  2 8 9 |6 4 3 |5 7 1
;;;  5 7 3 |2 9 1 |6 8 4
;;;  1 6 4 |8 7 5 |2 9 3
;;;
;;; The code below comes with a number of Sudoku grids of different difficulty. They can be
;;; solved using function `solve-all-examples`: `(solve-all-examples)`.
;;;
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2019 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
;;; except in compliance with the License. You may obtain a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed under the
;;; License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
;;; either express or implied. See the License for the specific language governing permissions
;;; and limitations under the License.

(import (lispkit base))

;; The program uses the following variable naming conventions:
;;   r is a row,     e.g. A
;;   c is a column,  e.g. 2
;;   s is a square,  e.g. A2
;;   d is a digit,   e.g. 7
;;   u is a unit,    e.g. (A1 B1 C1 ... I1)
;;   grid is a grid string, e.g. 81 non-blank characters ".7...12.."
;;   values is a grid table, i.e. a hashtable mapping squares to lists of digits
;;
;; A square is one cell in a Sudoku grid. Here is how squares map to the individual cells:
;;
;;  A1 A2 A3 |A4 A5 A6 |A7 A8 A9
;;  B1 B2 B3 |B4 B5 B6 |B7 B8 B9
;;  C1 C2 C3 |C4 C5 C6 |C7 C8 C9
;;  ---------+---------+---------
;;  D1 D2 D3 |D4 D5 D6 |D7 D8 D9
;;  E1 E2 E3 |E4 E5 E6 |E7 E8 E9
;;  F1 F2 F3 |F4 F5 F6 |F7 F8 F9
;;  ---------+---------+---------
;;  G1 G2 G3 |G4 G5 G6 |G7 G8 G9
;;  H1 H2 H3 |H4 H5 H6 |H7 H8 H9
;;  I1 I2 I3 |I4 I5 I6 |I7 I8 I9

;; Cartesian product of elements in list `xs` and `ys`. `f` is a function taking two arguments.
;; Is is used to build the "cross product" of two list elements `x` and `y`.
(define (cartesian-product f xs ys)
  (apply append (map (lambda (x) (map (lambda (y) (f x y)) ys)) xs)))

;; Combines a symbol and a number into a new symbol
(define (join x y)
  (string->symbol (string-append (symbol->string x) (number->string y))))

;; Computes the cross product of a list of symbols `xs` and a list of numbers `ys` using
;; function `join` to combine elements from both lists.
(define (cross xs ys)
  (cartesian-product join xs ys))

;; The space of the entities

(define digits '(1 2 3 4 5 6 7 8 9))
(define rows '(A B C D E F G H I))
(define cols digits)
(define squares (cross rows cols))
(define row-blocks '((A B C) (D E F) (G H I)))
(define col-blocks '((1 2 3) (4 5 6) (7 8 9)))

(define unitlist (append (map (lambda (c) (cross rows (list c))) cols)
                         (map (lambda (r) (cross (list r) cols)) rows)
                         (cartesian-product cross row-blocks col-blocks)))

(define units
  (alist->eq-hashtable
    (map (lambda (s) (cons s (filter (lambda (u) (any? (lambda (x) (eq? x s)) u)) unitlist)))
         squares)))

(define peers
  (alist->eq-hashtable
    (map (lambda (s)
           (cons s (filter (lambda (x) (not (eq? x s))) (concatenate (hashtable-ref units s '())))))
         squares)))

;; Solve a Sudoku grid. `grid` is a grid string. `(solve grid)` returns a grid table
;; containing a solution, or `#f` if no solution exists. A grid table returned by `solve`
;; is a hashtable each mapping a square to a singleton list containing the digit of the
;; solution for this square.
(define (solve grid)
  (search (parse-grid grid)))

;; Convert a grid string into a grid table. A grid table is a hashtable mapping squares to
;; possible digits. If a contradiction is detected, i.e. the Sudoku grid cannot be solved,
;; return `#f`.
(define (parse-grid grid)
  (let ((values (make-eq-hashtable)))
    (for-each (lambda (s) (hashtable-set! values s digits)) squares)
    (if (every? (lambda (s d) (or (zero? d) (assign values s d))) squares (grid-values grid))
        values
        #f)))

;; Convert a grid string into a list of digits (the "grid values"), one for each square,
;; starting on top from left to right.
(define (grid-values grid)
  (filter-map (lambda (c) (or (if (char=? c #\.) 0 #f) (digit-value c)))
              (string->list grid)))

;; Eliminate all values other than d from grid values[s] and propagate. `assign` returns
;; `#f` if a contradiction was detected, `#t` otherwise.
(define (assign values s d)
  (every? (lambda (d2) (or (eq? d d2) (eliminate values s d2)))
          (hashtable-ref values s '())))

;; Eliminate digit `d` from square `s` in grid table `values` and propagate corresponding
;; constraints in `values`. `eliminiate` returns `#f` if there is a contradiction; it returns
;; `#t` otherwise.
(define (eliminate values s d)
  (let ((ds (hashtable-ref values s '())))
    (or (not (memq d ds))
        (let ((dn (delq d ds)))
          (hashtable-set! values s dn)
          (and (pair? dn)
               (or (pair? (cdr dn))
                   (every? (lambda (s2) (eliminate values s2 (car dn)))
                           (hashtable-ref peers s '())))
               (every? (lambda (u)
                         (let ((dp (filter (lambda (s2) (memq d (hashtable-ref values s2 '()))) u)))
                           (and (pair? dp)
                                (or (pair? (cdr dp)) (assign values (car dp) d)))))
                       (hashtable-ref units s '())))))))

;; Applies depth-first search and constraint propagation to try all possible values for finding
;; a solution of the given grid table `values`. `search` returns the solution in form of a grid
;; table which has exactly one value/digit for every square.
(define (search values)
  (if (every? (lambda (s)
                (let ((ds (hashtable-ref values s '()))) (or (null? ds) (null? (cdr ds)))))
              squares)
      values
      (let ((s (cdr (fold-left (lambda (z s)
                                 (let ((n (length (hashtable-ref values s '()))))
                                   (if (< 1 n (car z)) (cons n s) z)))
                               (cons 10 #f) squares))))
        (fold-left (lambda (v d)
                     (or v (let ((values2 (hashtable-copy values #t)))
                             (and (assign values2 s d) (search values2)))))
                   #f (hashtable-ref values s '())))))

;; Displays a grid table on the default text output port.
(define (display-grid values)
  (let* ((w (fx1+ (fold-left (lambda (z x) (max z (length x))) 0 (hashtable-value-list values))))
         (divider (make-string (fx* w 3) #\-))
         (line (string-append divider "+" divider "+" divider)))
    (for-each
      (lambda (rb)
        (for-each
          (lambda (r)
            (display
              (fold-left
                (lambda (z cb)
                  (string-append z (if (string=? z "") "" "|")
                    (fold-left (lambda (z c)
                                 (string-append z
                                   (string-pad-right (value->string values (join r c)) #\space w)))
                                ""
                                cb)))
                ""
                col-blocks))
            (newline))
          rb)
        (cond ((not (eq? (car rb) 'G))
                (display line)
                (newline))))
      row-blocks)))

;; Returns a string representation of the value at square `s` on grid table `values`.
(define (value->string values s)
  (string-concatenate (map number->string (hashtable-ref values s '()))))

;; Some example grids

(define (solve-all . gridspecs)
  (for-each (lambda (gridspec)
              (display "~~~~~~ ")
              (display (car gridspec))
              (newline)
              (display-grid (solve (cdr gridspec)))
              (newline)) gridspecs))

(define (solve-all-examples)
  (solve-all (cons "GRID 1" grid1)
             (cons "GRID 2" grid2)
             (cons "GRID 3" grid3)
             (cons "GRID 4" grid4)
             (cons "GRID 5" grid5)
             (cons "GRID 6" grid6)
             (cons "GRID 7" grid7)
             (cons "GRID 8" grid8)
             (cons "GRID 9" grid9)
             (cons "GRID 10" grid10)
             (cons "GRID 11" grid11)
             (cons "GRID 12" grid12)
             (cons "GRID 13" grid13)
             (cons "GRID 14" grid14)
             (cons "GRID 15" grid15)
             (cons "GRID 16" grid16)))

(define grid1  "..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3..")
(define grid2  "2...8.3...6..7..84.3.5..2.9...1.54.8.........4.27.6...3.1..7.4.72..4..6...4.1...3")
(define grid3  "......9.7...42.18....7.5.261..9.4....5.....4....5.7..992.1.8....34.59...5.7......")
(define grid4  ".3..5..4...8.1.5..46.....12.7.5.2.8....6.3....4.1.9.3.25.....98..1.2.6...8..6..2.")
(define grid5  ".2.81.74.7....31...9...28.5..9.4..874..2.8..316..3.2..3.27...6...56....8.76.51.9.")
(define grid6  "1..92....524.1...........7..5...81.2.........4.27...9..6...........3.945....71..6")
(define grid7  ".43.8.25.6.............1.949....4.7....6.8....1.2....382.5.............5.34.9.71.")
(define grid8  "48...69.2..2..8..19..37..6.84..1.2....37.41....1.6..49.2..85..77..9..6..6.92...18")
(define grid9  "6..3.2....5.....1..........7.26............543.........8.15........4.2........7..")
(define grid10 "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")
(define grid11 "85...24..72......9..4.........1.7..23.5...9...4...........8..7..17..........36.4.")
(define grid12 "..53.....8......2..7..1.5..4....53...1..7...6..32...8..6.5....9..4....3......97..")
(define grid13 "..84...3....3.....9....157479...8........7..514.....2...9.6...2.5....4......9..56")
(define grid14 "1.....3.8.6.4..............2.3.1...........958.........5.6...7.....8.2...4.......")
(define grid15 ".476...5.8.3.....2.....9......8.5..6...1.....6.24......78...51...6....4..9...4..7")
(define grid16 ".2..........6....3.74.8.........3..2.8..4..1.6..5.........1.78.5....9..........4.")
(define hard1  ".....6....59.....82....8....45........3........6..3.54...325..6..................")

