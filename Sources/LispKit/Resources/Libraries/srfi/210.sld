;;; SRFI 210
;;; Procedures and Syntax for Multiple Values
;;;
;;; This SRFI extends the Scheme standard with a number of procedures and syntax
;;; dealing with multiple values, including syntax to create lists and vectors
;;; from expressions returning multiple values and procedures returning the elements
;;; of a list or vector as multiple values.
;;; 
;;; Copyright © 2020 Marc Nieper-Wißkirchen. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;; software and associated documentation files (the "Software"), to deal in the Software
;;; without restriction, including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software, and to permit
;;; persons to whom the Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all copies or
;;; substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
;;; PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
;;; FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;;; OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;
;;; Adaptation to LispKit
;;;   Copyright © 2020 Matthias Zenger. All rights reserved.

(define-library (srfi 210)
  
  (export apply/mv
          call/mv
          list/mv
          vector/mv
          box/mv
          value/mv
          arity
          set!-values
          with-values
          case-receive
          bind/mv
          list-values
          vector-values
          box-values
          value
          identity
          compose
          map-values
          bind/list
          bind/box
          bind)
  
  (import (except (lispkit base) value identity))

  (begin
    
    ;;;;;;;;;;;;
    ;; Syntax ;;
    ;;;;;;;;;;;;
    
    (define-syntax apply/mv
      (syntax-rules ()
        ((apply/mv operator operand1 ... producer)
         (letrec-syntax
             ((aux (syntax-rules ::: ()
                     ((aux %operator () ((%operand1 arg1) :::) %producer)
                      (let-values (((proc) %operator)
                                   ((arg1) %operand1) :::
                                   (args %producer))
                        (apply proc arg1 ::: args)))
                     ((aux %operator (%operand1 operand2 :::) (temp :::) %producer)
                      (aux %operator (operand2 :::) (temp ::: (%operand1 arg1))
                           %producer)))))
           (aux operator (operand1 ...) () producer)))))
    
    (define-syntax call/mv
      (syntax-rules ()
        ((call/mv consumer producer1 ...)
         (letrec-syntax
             ((aux (syntax-rules ::: ()
                     ((aux %consumer () ((%producer1 args1) :::))
                      (let-values (((proc) %consumer)
                                   (args1 %producer1) :::)
                        (apply proc (append args1 :::))))
                     ((aux %consumer (%producer1 producer2 :::) (temp :::))
                      (aux %consumer (producer2 :::) (temp ::: (%producer1 args1)))))))
           (aux consumer (producer1 ...) ())))))
    
    (define-syntax list/mv
      (syntax-rules ()
        ((list/mv element1 ... producer)
         (apply/mv list element1 ... producer))))
    
    (define-syntax vector/mv
      (syntax-rules ()
        ((vector/mv element1 ... producer)
         (apply/mv vector element1 ... producer))))
    
    (define-syntax box/mv
      (syntax-rules ()
        ((box/mv element1 ... producer)
         (apply/mv box element1 ... producer))))
    
    (define-syntax value/mv
      (syntax-rules ()
        ((value/mv index operand1 ... producer)
         (apply/mv value index operand1 ... producer))))
    
    (define-syntax arity
      (syntax-rules ()
        ((arity producer)
         (let-values ((res producer))
           (length res)))))
    
    (define-syntax set!-values
      (syntax-rules ()
        ((set!-values (var1 ...) producer)
         (letrec-syntax
             ((aux (syntax-rules ::: ()
                     ((aux () ((%var1 temp1) :::) %producer)
                      (let-values (((temp1 ::: . temp*) %producer))
                        (set! %var1 temp1) :::))
                     ((aux (%var1 var2 :::) (temp :::) %producer)
                      (aux (var2 :::) (temp ::: (%var1 temp1)) %producer)))))
           (aux (var1 ... ) () producer)))
        ((set!-values (var1 ... . var*) producer)
         (letrec-syntax
             ((aux (syntax-rules ::: ()
                     ((aux () ((%var1 temp1) ::: (%var* temp*)) %producer)
                      (let-values (((temp1 ::: . temp*) %producer))
                        (set! %var1 temp1) :::
                        (set! %var* temp*)))
                     ((aux (%var1 var2 :::) (temp :::) %producer)
                      (aux (var2 :::) (temp ::: (%var1 temp1)) %producer)))))
           (aux (var1 ... var*) () producer)))
        ((set!-values var* producer)
         (let-values ((temp*) producer)
           (set! var* temp*)))))
    
    (define-syntax with-values
      (syntax-rules ()
        ((with-values producer consumer)
         (apply/mv consumer producer))))
    
    (define-syntax case-receive
      (syntax-rules ()
        ((case-receive producer clause ...)
         (with-values producer
           (case-lambda clause ...)))))
    
    (define-syntax bind/mv
      (syntax-rules ()
        ((bind/mv producer transducer ...)
         (bind/list (list/mv producer) transducer ...))))
    
    ;;;;;;;;;;;;;;;;
    ;; Procedures ;;
    ;;;;;;;;;;;;;;;;
    
    (define (list-values lis)
      (apply values lis))
    
    (define (vector-values vec)
      (list-values (vector->list vec)))
    
    (define box-values unbox)
    
    (define (value k . objs)
      (list-ref objs k))
    
    (define identity values)
    
    (define compose
      (case-lambda
        (() identity)
        ((transducer . transducers)
         (let f ((transducer transducer) (transducers transducers))
           (if (null? transducers)
               transducer
               (let ((composition (f (car transducers) (cdr transducers))))
                 (lambda args
                   (apply/mv composition (apply transducer args)))))))))
    
    (define (map-values proc)
      (lambda args
        (list-values (map proc args))))
    
    (define (bind/list lis . transducers)
      (list-values (fold-left (lambda (lis transducer)
                                (list/mv (apply transducer lis)))
                              lis
                              transducers)))
    
    (define (bind/box bx . transducers)
      (apply bind/list (list/mv (unbox bx)) transducers))
    
    (define (bind obj . transducers)
      (apply bind/list (list obj) transducers))
  )
)
