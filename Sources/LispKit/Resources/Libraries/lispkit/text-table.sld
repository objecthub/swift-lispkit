;;; LISPKIT TEXT-TABLE
;;;
;;; Library for creating tables of textual content. The library supports column and
;;; cell-based text alignment, allows for multi-line rows, and supports different
;;; types of row separators.
;;; 
;;; A text table consists of one header row followed by regular text and separator
;;; rows. As part of the header row, it is possible to specify the text alignment
;;; of the header cell, the default text alignment of the corresponding column and
;;; a minimum and maximum size of the column (in terms of characters).
;;; 
;;; The following example shows how text tables are created:
;;; 
;;; ```
;;; (define tt (make-text-table
;;;              '(("ID" center right)
;;;                ("Name" center left)
;;;                ("Address" center left 10 20)
;;;                ("Approved" center center))
;;;              double-line-sep))
;;; (add-text-table-row! tt '("1" "Mark Smith" "2600 Windsor Road\nRedwood City, CA" "Yes"))
;;; (add-text-table-separator! tt line-sep)
;;; (add-text-table-row! tt '("2" "Emily Armstrong"
;;;                           "160 Randy Rock Way\nMountain View, CA" "No"))
;;; (add-text-table-separator! tt line-sep)
;;; (add-text-table-row! tt '("3" "Alexander Montgomery"
;;;                           "1500 Valencia Street\nSuite 100\nLos Altos, CA" "Yes"))
;;; (add-text-table-separator! tt line-sep)
;;; (add-text-table-row! tt '("4" "Myra Jones" "1320 Topaz Street\nPalo Alto, CA" "Yes"))
;;; ```
;;; 
;;; A displayable string representation can be generated via procedure `text-table->string`.
;;; This is what the result looks like:
;;; 
;;; ```
;;; ┌────┬──────────────────────┬──────────────────────┬──────────┐
;;; │ ID │         Name         │       Address        │ Approved │
;;; ╞════╪══════════════════════╪══════════════════════╪══════════╡
;;; │  1 │ Mark Smith           │ 2600 Windsor Road    │   Yes    │
;;; │    │                      │ Redwood City, CA     │          │
;;; ├────┼──────────────────────┼──────────────────────┼──────────┤
;;; │  2 │ Emily Armstrong      │ 160 Randy Rock Way   │    No    │
;;; │    │                      │ Mountain View, CA    │          │
;;; ├────┼──────────────────────┼──────────────────────┼──────────┤
;;; │  3 │ Alexander Montgomery │ 1500 Valencia Street │   Yes    │
;;; │    │                      │ Suite 100            │          │
;;; │    │                      │ Los Altos, CA        │          │
;;; ├────┼──────────────────────┼──────────────────────┼──────────┤
;;; │  4 │ Myra Jones           │ 1320 Topaz Street    │   Yes    │
;;; │    │                      │ Palo Alto, CA        │          │
;;; └────┴──────────────────────┴──────────────────────┴──────────┘
;;; ```
;;; 
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2021-2026 Matthias Zenger. All rights reserved.
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

(define-library (lispkit text-table)
  
  (export text-table?
          text-table-type-tag
          text-table-header?
          text-table-row?
          
          make-text-table
          add-text-table-row!
          add-text-table-separator!
          
          alignment-specifier?
          left
          right
          center
          
          text-table-edges?
          no-edges
          sharp-edges
          round-edges
          
          text-table-separator?
          no-sep
          space-sep
          line-sep
          double-line-sep
          bold-line-sep
          dashed-line-sep
          bold-dashed-line-sep
          
          text-table->string
          data-frame->text-table)
  
  (import (lispkit base)
          (lispkit gvector)
          (only (lispkit date-time)
            date-time? date-time->string)
          (only (lispkit data-frame)
            data-frame? data-frame-column-names data-frame-column-types
            data-frame-column data-frame-row-count data-frame-missing?))

  (begin
    
    ;; Creating text tables
    
    (define-values (text-table-type-tag new-text-table text-table? text-table-ref
                    text-table-subtype)
      (make-type 'text-table))
    
    (define (make-text-table headers . args)
      (assert (text-table-header? headers))
      (let-optionals args ((separator bold-line-sep)
                           (edges sharp-edges))
        (assert (text-table-separator? separator)
                (text-table-edges? edges))
        (new-text-table (gvector headers separator edges))))
    
    (define (text-table-header? row)
      (if (pair? row)
          (and (text-table-header-cell? (car row)) (text-table-header? (cdr row)))
          (null? row)))
    
    (define (text-table-row? row)
      (if (pair? row)
          (and (text-table-cell? (car row)) (text-table-row? (cdr row)))
          (null? row)))
    
    ;; A row cell is either a string, a list ("content" align), or a pair ("content" . align)
    (define (text-table-cell? cell)
      (or (string? cell)
          (and (pair? cell)
               (string? (car cell))
               (or (alignment-specifier? (cdr cell))
                   (and (pair? (cdr cell))
                        (alignment-specifier? (cadr cell))
                        (null? (cddr cell)))))))
    
    ;; Returns the alignment specifier of a cell which is not a string
    (define (cell-alignment cell)
      (if (pair? (cdr cell)) (cadr cell) (cdr cell)))
    
    ;; Column alignment specifiers
    
    (define (alignment-specifier? spec)
      (or (null? spec)
          (eq? spec 'left)
          (eq? spec 'right)
          (eq? spec 'center)))
    
    (define left 'left)
    (define right 'right)
    (define center 'center)
    
    (define (text-table-header-cell? cell)
      (or (string? cell)
          (and (pair? cell)
               (string? (car cell))
               (pair? (cdr cell))
               (alignment-specifier? (cadr cell))
               (or (null? (cddr cell))
                   (and (pair? (cddr cell))
                        (alignment-specifier? (caddr cell))
                        (or (null? (cdddr cell))
                            (and (pair? (cdddr cell))
                                 (fixnum? (cadddr cell))
                                 (or (null? (cdddr (cdr cell)))
                                     (and (pair? (cddddr cell))
                                          (fixnum? (car (cddddr cell))))))))))))
    
    ;; Adding rows and separators
    
    (define (add-text-table-row! table row)
      (assert (text-table? table)
              (text-table-row? row))
      (gvector-add! (text-table-ref table) row))
    
    (define (add-text-table-separator! table . args)
      (assert (text-table? table))
      (let-optionals args ((sep line-sep))
        (assert (text-table-separator? sep))
        (gvector-add! (text-table-ref table) sep)))
    
    ;; Edges
    
    (define (text-table-edges? obj)
      (and (list? obj) (= (length obj) 4) (every? string? obj)))
    
    (define no-edges             '("╷" "╷" "╵" "╵"))
    (define sharp-edges          '("┌" "┐" "└" "┘"))
    (define round-edges          '("╭" "╮" "╰" "╯"))
    
    ;; Separators
    
    (define (text-table-separator? obj)
      (and (pair? obj) (char? (car obj)) (pair? (cdr obj)) (string? (cadr obj))))
    
    (define no-sep #f)
    (define space-sep            '(#\space "│" "│" "│"))
    (define line-sep             '(#\─ "├" "┼" "┤"))
    (define double-line-sep      '(#\═ "╞" "╪" "╡"))
    (define bold-line-sep        '(#\━ "┝" "┿" "┥"))
    (define dashed-line-sep      '(#\╌ "├" "┼" "┤"))
    (define bold-dashed-line-sep '(#\╍ "┝" "┿" "┥"))
    
    ;; Table headers
    ;; 
    ;; Format: (text header-align column-align min-size max-size)
    
    (define (column-header-text h)
      (if (string? h) h (car h)))
    
    (define (column-header-alignment h)
      (if (or (string? h) (null? (cdr h))) 'center (cadr h)))
    
    (define (column-alignment h)
      (if (or (string? h) (null? (cdr h)) (null? (cddr h))) 'left (caddr h)))
    
    (define (column-min-size h)
      (if (or (string? h) (null? (cdr h)) (null? (cddr h)) (null? (cdddr h))) 1 (cadddr h)))
    
    (define (column-max-size h)
      (if (or (string? h) (null? (cdr h)) (null? (cddr h)) (null? (cdddr h)) (null? (cddddr h)))
          100
          (cadddr (cdr h))))
    
    (define (column-size-ranges header)
      (if (pair? header)
          (let-values (((rmin rmax) (column-size-ranges (cdr header))))
            (values (cons (column-min-size (car header)) rmin)
                    (cons (column-max-size (car header)) rmax)))
          (values '() '())))
    
    (define (column-size text)
      (fold-left (lambda (x y) (max x (string-length y))) 0 (string-split text #\newline)))
    
    (define (column-sizes row)
      (if (pair? row)
          (cons (column-size (if (string? (car row)) (car row) (caar row)))
                (column-sizes (cdr row)))
          '()))
    
    (define (table-column-sizes row)
      (if (text-table-separator? row) 0 (column-sizes row)))
    
    (define (max-column-sizes cur new cmax)
      (if (pair? cur)
          (if (pair? new)
              (cons (min (max (car cur) (car new)) (car cmax))
                    (max-column-sizes (cdr cur) (cdr new) (cdr cmax)))
              (cons (car cur)
                    (max-column-sizes (cdr cur) '() (cdr cmax))))
          '()))
    
    (define (text-table-column-sizes table)
      (let ((vec (text-table-ref table)))
        (let-values (((colmin colmax) (column-size-ranges (gvector-ref vec 0))))
          (do ((i 1 (+ i 1))
               (sizes (max-column-sizes colmin (column-sizes (gvector-ref vec 0)) colmax)
                      (max-column-sizes sizes (table-column-sizes (gvector-ref vec i)) colmax)))
              ((>= i (gvector-length vec)) sizes)))))
    
    ;; Rendering tables
    
    (define (alignment-proc cell default)
      (case (let ((align (if (string? cell) '() (cell-alignment cell))))
              (if (null? align) default align))
        ((center) string-pad-center)
        ((right) string-pad-left)
        (else string-pad-right)))
    
    (define (header-alignment-proc cell)
      (case (column-header-alignment cell)
        ((center) string-pad-center)
        ((right) string-pad-left)
        (else string-pad-right)))
    
    (define (cell-text cell size)
      (let ((text (if (string? cell) cell (car cell))))
        (if (> (string-length text) size)
            (string-append " " (substring text 0 (- size 1)) "… ")
            (string-append " " text " "))))
    
    (define (line-by-line-row row)
      (if (pair? row)
          (cons (if (string? (car row))
                    (string-split (car row) #\newline)
                    (map (lambda (s) (cons s (cdar row))) (string-split (caar row) #\newline)))
                (line-by-line-row (cdr row)))
          '()))
    
    (define (transpose1-line-by-line-row lblr)
      (if (pair? lblr)
          (let-values (((transp1 rest) (transpose1-line-by-line-row (cdr lblr))))
            (values (cons (if (pair? (car lblr)) (caar lblr) "") transp1)
                    (cons (if (pair? (car lblr)) (cdar lblr) '()) rest)))
          (values '() '())))
    
    (define (transpose-line-by-line-row lblr)
      (if (every? null? lblr)
          '()
          (let-values (((row rest) (transpose1-line-by-line-row lblr)))
            (cons row (transpose-line-by-line-row rest)))))
    
    (define (header-line headers sizes)
      (if (pair? headers)
          (string-append "│"
                         ((header-alignment-proc (car headers))
                           (cell-text (car headers) (car sizes))
                           #\space
                           (+ (car sizes) 2))
                         (header-line (cdr headers) (cdr sizes)))
          "│"))
    
    (define (header-alignments headers)
      (if (pair? headers)
          (cons (column-alignment (car headers)) (header-alignments (cdr headers)))
          '()))
    
    (define (header-lines headers sizes)
      (map (lambda (r) (header-line r sizes))
           (transpose-line-by-line-row (line-by-line-row headers))))
    
    (define (separator-line sizes sep bar)
      (if (pair? sizes)
          (string-append
            (make-string (+ (car sizes) 2) bar)
            (opt (lambda (s) (string-append sep s)) (separator-line (cdr sizes) sep bar) ""))
          #f))
    
    (define (row-line row sizes alignments)
      (if (pair? row)
          (string-append
            "│"
            ((alignment-proc (car row) (car alignments))
              (cell-text (car row) (car sizes))
              #\space
              (+ (car sizes) 2))
            (row-line (cdr row) (cdr sizes) (cdr alignments)))
          "│"))
    
    (define (table-line row sizes alignments)
      (if (text-table-separator? row)
          (list (string-append (cadr row)
                               (separator-line sizes (caddr row) (car row))
                               (cadddr row)))
          (map (lambda (r) (row-line r sizes alignments))
               (transpose-line-by-line-row (line-by-line-row row)))))
    
    (define (text-table->string table . args)
      (let-optionals args ((border #t))
        (let* ((topcolch (if border "┬" "╷"))
               (botcolch (if border "┴" "╵"))
               (linech   (if border #\─ #\space))
               (vec      (text-table-ref table))
               (hs       (vector-ref vec 1))
               (edges    (if border (vector-ref vec 2) no-edges))
               (sizes    (text-table-column-sizes table))
               (topline  (string-append (car edges) (separator-line sizes topcolch linech) (cadr edges)))
               (medline  (if hs
                             (string-append (cadr hs)
                                            (separator-line sizes (caddr hs) (car hs))
                                            (cadddr hs)
                                            "\n")
                             ""))
               (botline  (string-append (caddr edges)
                                        (separator-line sizes botcolch linech)
                                        (cadddr edges))))
          (let ((alignments (header-alignments (vector-ref vec 0))))
            (string-append
              topline "\n"
              (string-concatenate (header-lines (vector-ref vec 0) sizes) #\newline) "\n"
              medline
              (string-concatenate
                (do ((i (vector-length vec) (- i 1))
                     (strs (list botline)
                           (append (table-line (vector-ref vec (- i 1)) sizes alignments) strs)))
                    ((<= i 3) strs))
                "\n"))))))
    
    ;; Data frames
    
    (define data-frame-options
      '(columns headers align format missing max-cell-width max-rows row-indices
        header-sep row-sep edges))
    
    (define (option options key default)
      (let ((entry (assq key options)))
        (if entry (cdr entry) default)))
    
    ;; Looks up column `name` (a string) in an association list whose keys are strings
    ;; or symbols
    (define (column-option alist name default)
      (let loop ((alist alist))
        (cond ((null? alist) default)
              ((let ((key (caar alist)))
                 (string=? (if (symbol? key) (symbol->string key) key) name))
                (cdar alist))
              (else (loop (cdr alist))))))
    
    (define (column-name name)
      (if (symbol? name) (symbol->string name) name))
    
    (define (separator-option obj)
      (case obj
        ((no-sep) no-sep)
        ((space-sep) space-sep)
        ((line-sep) line-sep)
        ((double-line-sep) double-line-sep)
        ((bold-line-sep) bold-line-sep)
        ((dashed-line-sep) dashed-line-sep)
        ((bold-dashed-line-sep) bold-dashed-line-sep)
        (else
          (if (or (not obj) (text-table-separator? obj))
              obj
              (error "invalid text table separator" obj)))))
    
    (define (edges-option obj)
      (case obj
        ((no-edges) no-edges)
        ((sharp-edges) sharp-edges)
        ((round-edges) round-edges)
        (else
          (if (text-table-edges? obj)
              obj
              (error "invalid text table edges" obj)))))
    
    (define (default-alignment type)
      (case type
        ((integer double) 'right)
        ((boolean) 'center)
        (else 'left)))
    
    (define (format-value x)
      (cond ((string? x) x)
            ((number? x) (number->string x))
            ((boolean? x) (if x "true" "false"))
            ((date-time? x) (date-time->string x))
            (else (object->string x #t))))
    
    (define (data-frame->text-table df . args)
      (let-optionals args ((options '()))
        (assert (data-frame? df))
        (for-each (lambda (entry)
                    (if (not (and (pair? entry) (memq (car entry) data-frame-options)))
                        (error "invalid data frame option" entry)))
                  options)
        (let* ((names (map column-name (option options 'columns (data-frame-column-names df))))
               (types (data-frame-column-types df))
               (headers (option options 'headers '()))
               (aligns (option options 'align '()))
               (fmt (option options 'format '()))
               (missing (option options 'missing ""))
               (max-width (option options 'max-cell-width 100))
               (count (data-frame-row-count df))
               (max-rows (option options 'max-rows #f))
               (rows (if (and max-rows (< max-rows count)) max-rows count))
               (indices? (option options 'row-indices #f))
               (row-sep (separator-option (option options 'row-sep no-sep)))
               (columns (map (lambda (name) (data-frame-column df name)) names))
               (column-types (map (lambda (name) (column-option types name 'other)) names))
               (alignments (map (lambda (name type)
                                  (column-option aligns name (default-alignment type)))
                                names column-types))
               (formatters (map (lambda (name)
                                  (if (procedure? fmt)
                                      (lambda (x) (fmt x name))
                                      (column-option fmt name format-value)))
                                names))
               (header-cells (map (lambda (name align)
                                    (let ((h (column-option headers name name)))
                                      (if (string? h) (list h 'center align 1 max-width) h)))
                                  names alignments))
               (table (new-text-table
                        (gvector (if indices? (cons '("" center right) header-cells) header-cells)
                                 (separator-option (option options 'header-sep bold-line-sep))
                                 (edges-option (option options 'edges sharp-edges))))))
          (if (null? names)
              (error "text tables require at least one column" df))
          (assert (text-table-header? header-cells))
          (do ((i 0 (+ i 1)))
              ((>= i rows))
            (if (and row-sep (> i 0))
                (add-text-table-separator! table row-sep))
            (let ((cells (map (lambda (name column type formatter)
                                (let ((x (vector-ref column i)))
                                  ;; In boolean columns, #f is either false or missing
                                  (if (if (eq? type 'boolean)
                                          (data-frame-missing? df i name)
                                          (not x))
                                      missing
                                      (formatter x))))
                              names columns column-types formatters)))
              (add-text-table-row! table (if indices? (cons (number->string i) cells) cells))))
          (if (< rows count)
              (add-text-table-row! table (make-list (+ (length names) (if indices? 1 0)) "…")))
          table)))
  )
)
