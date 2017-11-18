;;; SRFI 1
;;; List-processing library
;;; 
;;; R5RS Scheme has an impoverished set of list-processing utilities, which is a problem
;;; for authors of portable code. This SRFI proposes a coherent and comprehensive set of
;;; list-processing procedures.
;;;
;;; This implementation is incomplete. It leaves out all functionality using mutable pairs,
;;; since mutable lists are not supported by LispKit.
;;;
;;; Portions of this code: SRFI 1 reference implementation
;;;   Copyright © 1998, 1999 by Olin Shivers.
;;;   You may do as you please with this code as long as you do not remove this copyright
;;;   notice or hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;;
;;; Portions of this code: SRFI 1 library provided by Picrin
;;;   Copyright © 2013-2014 Yuichi Nishiwaki and other picrin contributors.
;;;
;;; Adaptation to LispKit
;;;   Copyright © 2017 Matthias Zenger. All rights reserved.

(define-library (srfi 1)
  
  (import (lispkit base))
  
  ;; # Constructors
  ;; cons list
  ;; xcons cons* make-list list-tabulate
  ;; list-copy iota

  (begin
    (define (xcons d a) (cons a d))

    (define (cons* first . rest)
      (let recur ((x first) (rest rest))
        (if (pair? rest)
        (cons x (recur (car rest) (cdr rest))) x)))

    (define (list-tabulate len proc)
      (do ((i (- len 1) (- i 1))
           (ans '() (cons (proc i) ans)))
          ((< i 0) ans)))

    (define (iota count . lst)
      (let ((start (if (pair? lst) (car lst) 0))
            (step (if (and (pair? lst)
                      (pair? (cdr lst)))
                      (cadr lst) 1)))
        (let rec ((count (- count 1))
                  (acc '()))
          (if (zero? count)
              (cons start acc)
              (rec (- count 1) (cons (+ start (* count step)) acc)))))))

  (export cons list xcons cons* make-list list-tabulate iota)

  ;; # Predicates
  ;; pair? null?
  ;; proper-list? dotted-list?
  ;; not-pair? null-list?
  ;; list=

  (begin
    (define (not-pair? x) (not (pair? x)))

    (define proper-list? list?)

    (define (dotted-list? x)
      (and (pair? x) (not (proper-list? x))))

    (define (null-list? x)
      (cond ((pair? x) #f)
            ((null? x) #t)
            (else (error "null-list?: argument out of domain" x))))

    (define (list= elt= . lists)
      (or (null? lists)
          (let rec1 ((list1 (car lists)) (others (cdr lists)))
            (or (null? others)
                (let ((list2 (car others))
                      (others (cdr others)))
                  (if (eq? list1 list2)
                      (rec1 list2 others)
                      (let rec2 ((l1 list1) (l2 list2))
                        (if (null-list? l1)
                            (and (null-list? l2)
                                 (rec1 list2 others))
                            (and (not (null-list? l2))
                                 (elt= (car l1) (car l2))
                                 (rec2 (cdr l1) (cdr l2))))))))))))

  (export pair? null? not-pair? proper-list? dotted-list? null-list? list=)

  ;; # Selectors
  ;; car cdr ... cddadr cddddr list-ref
  ;; first second third fourth fifth sixth seventh eighth ninth tenth
  ;; car+cdr
  ;; take drop
  ;; take-right drop-right
  ;; split-at
  ;; last last-pair

  (begin
    (define (take x i)
      (if (zero? i)
          '()
          (cons (car x) (take (cdr x) (- i 1)))))

    (define (drop x i)
      (if (zero? i) x (drop (cdr x) (- i 1))))

    (define (take-right lis k)
      (let lp ((lag lis)
               (lead (drop lis k)))
        (if (pair? lead)
            (lp (cdr lag) (cdr lead))
            lag)))

    (define (drop-right lis k)
      (let recur ((lag lis) (lead (drop lis k)))
        (if (pair? lead)
            (cons (car lag) (recur (cdr lag) (cdr lead)))
            '())))

    (define (split-at x i)
      (values (take x i) (drop x i)))

    (define (last pair)
      (car (take-right pair 1)))

    (define (last-pair pair)
      (take-right pair 1))

    (define first car)

    (define second cadr)

    (define third caddr)

    (define fourth cadddr)

    (define (fifth x) (car    (cddddr x)))

    (define (sixth x) (cadr   (cddddr x)))

    (define (seventh x) (caddr  (cddddr x)))

    (define (eighth x) (cadddr (cddddr x)))

    (define (ninth x) (car  (cddddr (cddddr x))))

    (define (tenth x) (cadr (cddddr (cddddr x))))

    (define (car+cdr pair) (values (car pair) (cdr pair))))

  (export car cdr car+cdr list-ref
          caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
          caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr
          cdadar cdaddr cddaar cddadr cdddar cddddr
          first second third fourth fifth sixth seventh eighth ninth tenth
          take drop take-right drop-right
          split-at last last-pair)

  ;; # Miscellaneous
  ;; length
  ;; append concatenate reverse
  ;; append-reverse
  ;; zip unzip1 unzip2 unzip3 unzip4 unzip5
  ;; count

  (begin
    (define (concatenate lists)
      (apply append lists))

    (define (append-reverse rev-head tail)
      (if (null? rev-head)
          tail
          (append-reverse (cdr rev-head) (cons (car rev-head) tail))))

    (define (zip . lists)
      (apply map list lists))

    (define (unzip1 list)
      (map first list))

    (define (unzip2 list)
      (values (map first list)
              (map second list)))

    (define (unzip3 list)
      (values (map first list)
              (map second list)
              (map third list)))

    (define (unzip4 list)
      (values (map first list)
              (map second list)
              (map third list)
              (map fourth list)))

    (define (unzip5 list)
      (values (map first list)
              (map second list)
              (map third list)
              (map fourth list)
              (map fifth list)))

    (define (count pred . clists)
      (let rec ((tflst (apply map pred clists)) (n 0))
        (if (null? tflst)
            n
            (rec (cdr tflst) (if (car tflst) (+ n 1) n))))))

  (export length
          append concatenate
          reverse append-reverse
          zip unzip1 unzip2 unzip3 unzip4 unzip5
          count)

  ;; # Fold, unfold & map
  ;; map for-each
  ;; fold unfold pair-fold reduce
  ;; fold-right unfold-right pair-fold right reduce-right
  ;; append-map
  ;; pair-for-each filter-map map-in-order

  (begin
    (define (fold kons knil clist . clists)
      (if (null? clists)
          (let rec ((acc knil) (clist clist))
            (if (null? clist)
                acc
                (rec (kons (car clist) acc) (cdr clist))))
          (let rec ((acc knil) (clists (cons clist clists)))
            (if (every pair? clists)
                (rec (apply kons (append (map car clists) (list acc)))
                     (map cdr clists))
                acc))))
    
    (define (fold-right kons knil clist . clists)
      (if (null? clists)
          (let rec ((clist clist) (cont values))
            (if (null? clist)
                (cont knil)
                (rec (cdr clist) (lambda (x) (cont (kons (car clist) x))))))
          (let rec ((clists (cons clist clists)) (cont values))
            (if (every pair? clists)
                (rec (map cdr clists)
                     (lambda (x)
                       (cont (apply kons (append (map car clists) (list x))))))
                (cont knil)))))

    (define (pair-fold kons knil clist . clists)
      (if (null? clists)
          (let rec ((acc knil) (clist clist))
            (if (null? clist)
                acc
                (let ((tail (cdr clist)))
                  (rec (kons clist acc) tail))))
          (let rec ((acc knil) (clists (cons clist clists)))
            (if (every pair? clists)
                (let ((tail (map cdr clists)))
                  (rec (apply kons (append clists (list acc)))
                       tail))
                acc))))

    (define (pair-fold-right kons knil clist . clists)
      (if (null? clists)
          (let rec ((clist clist) (cont values))
            (if (null? clist)
                (cont knil)
                  (rec (cdr clist) (lambda (x) (cont (kons clist x))))))
          (let rec ((clists (cons clist clists)) (cont values))
            (if (every pair? clists)
                (let ((tail (map cdr clists)))
                  (rec tail
                       (lambda (x)
                         (cont (apply kons (append clists (list x)))))))
                (cont knil)))))

    (define (reduce f ridentity list)
      (if (null? list)
          ridentity
          (fold f (car list) (cdr list))))

    (define (reduce-right f ridentity list)
      (fold-right f ridentity list))

    (define (unfold p f g seed . tail-gen)
      (let ((tail-gen (if (null? tail-gen)
                          (lambda (x) '())
                          (car tail-gen))))
        (let rec ((seed seed) (cont values))
          (if (p seed)
              (cont (tail-gen seed))
              (rec (g seed) (lambda (x) (cont (cons (f seed) x))))))))

    (define (unfold-right p f g seed . tail)
      (let rec ((seed seed) (lst tail))
        (if (p seed)
            lst
            (rec (g seed) (cons (f seed) lst)))))

    (define (append-map f . clists)
      (apply append (apply map f clists)))

    (define (pair-for-each f clist . clists)
      (if (null? clist)
          (let rec ((clist clist))
            (if (pair? clist)
                (begin (f clist) (rec (cdr clist)))))
          (let rec ((clists (cons clist clists)))
            (if (every pair? clists)
                (begin (apply f clists) (rec (map cdr clists)))))))

    (define (map-in-order f clist . clists)
      (if (null? clists)
          (let rec ((clist clist)
                    (acc '()))
            (if (null? clist)
                (reverse acc)
                (rec (cdr clist) (cons (f (car clist)) acc))))
          (let rec ((clists (cons clist clists))
                    (acc '()))
            (if (every pair? clists)
                (rec (map cdr clists)
                     (cons* (apply f (map car clists)) acc))
                (reverse acc)))))

    (define (filter-map f clist . clists)
      (let recur ((l (apply map f clist clists)))
        (cond ((null? l) '())
              ((car l)   (cons (car l) (recur (cdr l))))
              (else      (recur (cdr l)))))))

  (export map for-each
          fold unfold pair-fold reduce
          fold-right unfold-right pair-fold-right reduce-right
          append-map
          pair-for-each filter-map map-in-order)

  ;; # Filtering & partitioning
  ;; filter partition remove

  (begin
    (define (filter pred list)
      (let ((pcons (lambda (v acc) (if (pred v) (cons v acc) acc))))
        (reverse (fold pcons '() list))))

    (define (remove pred list)
      (filter (lambda (x) (not (pred x))) list))

    (define (partition pred list)
      (values (filter pred list)
              (remove pred list))))

  (export filter partition remove)

  ;; # Searching
  ;; member memq memv
  ;; find find-tail
  ;; any every
  ;; list-index
  ;; take-while drop-while
  ;; span break

  (begin
    (define (find-tail pred list)
      (if (null? list)
          #f
          (if (pred (car list))
              list
              (find-tail pred (cdr list)))))

    (define (find pred list)
      (let ((tail (find-tail pred list)))
        (if tail
            (car tail)
            #f)))

    (define (take-while pred clist)
      (let rec ((clist clist) (cont values))
        (if (null? clist)
            (cont '())
            (if (pred (car clist))
                (rec (cdr clist)
                     (lambda (x) (cont (cons (car clist) x))))
                (cont '())))))

    (define (drop-while pred clist)
      (let rec ((clist clist))
        (if (null? clist)
            '()
            (if (pred (car clist))
                (rec (cdr clist))
                clist))))

    (define (span pred clist)
      (values (take-while pred clist)
              (drop-while pred clist)))

    (define (break pred clist)
      (values (take-while (lambda (x) (not (pred x))) clist)
              (drop-while (lambda (x) (not (pred x))) clist)))

    (define (any pred ls . lists)
      (if (null? lists)
          (let lp ((ls ls))
            (cond ((null? ls)        #f)
                  ((null? (cdr ls))  (pred (car ls)))
                  (else              (or (pred (car ls)) (lp (cdr ls))))))
          (let lp ((lists (cons ls lists)))
            (cond ((any1 null? lists)           #f)
                  ((any1 null? (map cdr lists)) (apply pred (map car lists)))
                  (else                         (or (apply pred (map car lists))
                                                    (lp (map cdr lists))))))))

    (define (every pred ls . lists)
      (if (null? lists)
          (let lp ((ls ls))
            (cond ((null? ls)       #t)
                  ((null? (cdr ls)) (pred (car ls)))
                  (else             (and (pred (car ls)) (lp (cdr ls))))))
          (let lp ((lists (cons ls lists)))
            (cond ((any1 null? lists)     #t)
            ((any1 null? (map cdr lists)) (apply pred (map car lists)))
            (else                         (and (apply pred (map car lists))
                                               (lp (map cdr lists))))))))

    (define (list-index pred clist . clists)
      (if (null? clists)
          (let rec ((clist clist) (n 0))
            (if (pair? clist)
                (if (pred (car clist))
                    n
                    (rec (cdr clist) (+ n 1)))))
          (let rec ((clists (cons clist clists)) (n 0))
            (if (every pair? clists)
                (if (apply pred (map car clists))
                    n
                    (rec (map cdr clists) (+ n 1))))))))

  (export member memq memv
          find find-tail
          any every
          list-index
          take-while drop-while
          span break)

  ;; # Deleting
  ;; delete delete-duplicates

  (begin
    (define (delete x list . =)
      (let ((= (if (null? =) equal? (car =))))
        (remove (lambda (a) (= x a)) list)))

    (define (delete-duplicates list . =)
      (let ((= (if (null? =) equal? (car =))))
        (let rec ((list list) (cont values))
          (if (null? list)
              (cont '())
              (let* ((x (car list))
                     (rest (cdr list))
                     (deleted (delete x rest =)))
                (rec deleted (lambda (y) (cont (cons x y))))))))))

  (export delete delete-duplicates)

  ;; # Association lists
  ;; assoc assq assv
  ;; alist-cons alist-copy
  ;; alist-delete

  (begin
    (define (alist-cons key datum alist)
      (cons (cons key datum) alist))

    (define (alist-copy alist)
      (map (lambda (elt) (cons (car elt) (cdr elt))) alist))

    (define (alist-delete key alist . =)
      (let ((= (if (null? =) equal? (car =))))
        (remove (lambda (x) (= key (car x))) alist))))

  (export assoc assq assv
          alist-cons alist-copy
          alist-delete)

  ;; # Set operations on lists
  ;; lset<= lset= lset-adjoin
  ;; lset-union
  ;; lset-intersection
  ;; lset-difference
  ;; lset-xor
  ;; lset-diff+intersenction

  (begin
    (define (lset<= = . lists)
      (or (null? lists)
          (let rec ((head (car lists)) (rest (cdr lists)))
            (or (null? rest)
                (let ((next (car rest)) (rest (cdr rest)))
                  (and (or (eq? head next)
                           (every (lambda (x) (member x next =)) head))
                       (rec next rest)))))))

    (define (lset= = . lists)
      (or (null? lists)
          (let rec ((head (car lists)) (rest (cdr lists)))
            (or (null? rest)
                (let ((next (car rest)) (rest (cdr rest)))
                  (and (or (eq? head next)
                           (and (every (lambda (x) (member x next =)) head)
                                (every (lambda (x) (member x head =)) next))
                           (rec next rest))))))))

    (define (lset-adjoin = list . elts)
      (let rec ((list list) (elts elts))
        (if (null? elts)
            list
            (if (member (car elts) list)
                (rec list (cdr elts))
                (rec (cons (car elts) list) (cdr elts))))))

    (define (lset-union = . lists)
      (if (null? lists)
          lists
          (let rec ((head (car lists)) (rest (cdr lists)))
            (if (null? rest)
                head
                (let ((next (car rest)) (rest (cdr rest)))
                  (if (eq? head next)
                      (rec head rest)
                      (rec (apply lset-adjoin = head next) rest)))))))

    (define (lset-intersection = . lists)
      (if (null? lists)
          lists
          (let rec ((head (car lists)) (rest (cdr lists)))
            (if (null? rest)
                head
                (let ((next (car rest)) (rest (cdr rest)))
                  (if (eq? head next)
                      (rec head rest)
                      (rec (filter (lambda (x) (member x next =)) head)
                           rest)))))))

    (define (lset-difference = list . lists)
      (let rec ((head list) (rest lists))
        (if (null? rest)
            head
            (let ((next (car rest)) (rest (cdr rest)))
              (if (eq? head next)
                  '()
                  (rec (remove (lambda (x) (member x next =)) head)
                       rest))))))

    (define (lset-xor = . lists)
      (if (null? lists)
          lists
          (let rec ((head (car lists)) (rest (cdr lists)))
            (if (null? rest)
                head
                (let ((next (car rest)) (rest (cdr rest)))
                  (if (eq? head next)
                      '()
                      (rec (append (remove (lambda (x) (member x next =)) head)
                                   (remove (lambda (x) (member x head =)) next))
                           rest)))))))

    (define (lset-diff+intersection = list . lists)
      (values (apply lset-difference = list lists)
              (lset-intersection = list (apply lset-union lists)))))

  (export lset<= lset= lset-adjoin
          lset-union
          lset-intersection
          lset-difference
          lset-xor
          lset-diff+intersection))
