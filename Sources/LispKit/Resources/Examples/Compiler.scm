;;; Tiny Arithmetic Compiler
;;;
;;; This is a very simple compiler for compiling arithmetic expressions consisting of
;;; +, -, * and / into simple virtual machine instructions. The virtual machine just consists
;;; of a number of registers; there is no stack and heap. Thus, the complexity of the
;;; compilable expressions depends a lot on the number of available registers.
;;;
;;; Here is an example for interacting with the compiler and the virtual machine.
;;;
;;; First, we create a new virtual machine with 16 registers:
;;;   (define mvm (make-vm 16))
;;; Next, we compile an arithmetic expression such that the result gets written into
;;; register 0. The resulting machine code gets stored in `code`:
;;;   (define code (compile '(* 7 (+ (* 2 (- 8 3)) 1) (/ 10 2)) 0))
;;; We can pretty print the instructions like this:
;;;   (display-code code)
;;;   =>  (MOV 0 7)
;;;       (MOV 1 2)
;;;       (MOV 2 8)
;;;       (MOV 3 3)
;;;       (SUB 2 3)
;;;       (MUL 1 2)
;;;       (MOV 2 1)
;;;       (ADD 1 2)
;;;       (MUL 0 1)
;;;       (MOV 1 10)
;;;       (MOV 2 2)
;;;       (DIV 1 2)
;;;       (MUL 0 1)
;;; Finally, we can execute the code on virtual machine mvm:
;;;   (execute mvm code)
;;; The result can be found in register 0 of mvm:
;;;   (read-register mvm 0)
;;;   => 385
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2018 Matthias Zenger. All rights reserved.
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


;; A VM encapsulates a number of registers. It gets created via `make-vm`. The registers
;; of a VM can be read or set with the functions `read-register` and `write-register`.

(define-record-type vm (new-vm registers) vm? (registers get-registers))

(define (make-vm n)
  (new-vm (make-vector n 0)))

(define (read-register vm i)
  (vector-ref (get-registers vm) i))

(define (write-register vm i value)
  (vector-set! (get-registers vm) i value))

;; `compile` compiles an arithmetic expression into machine code. Machine code is
;; represented simply as a list of VM instructions.

(define (compile expr reg)
  (if (pair? expr)
      (if (= (length expr) 2)
          (case (car expr)
            ((+) (compile (cadr expr) reg))
            ((-) (append (compile (cadr expr) reg) (list (list 'NEG reg))))
            (else (list (list 'EVL reg expr))))
          (case (car expr)
            ((+) (compile-op 'ADD reg (cdr expr)))
            ((-) (compile-op 'SUB reg (cdr expr)))
            ((*) (compile-op 'MUL reg (cdr expr)))
            ((/) (compile-op 'DIV reg (cdr expr)))
            (else (list (list 'EVL reg expr)))))
      (list (list 'MOV reg expr))))

(define (compile-op oper reg args)
  (fold-left (lambda (arg code)
               (append code (compile arg (+ reg 1)) (list (list oper reg (+ reg 1)))))
             (compile (car args) reg)
             (cdr args)))

;; `display-code` is a pretty printer for machine code.

(define (display-code code)
  (if (pair? code)
      (begin (display "    ")
             (display (car code))
             (newline)
             (display-code (cdr code)))))

;; `execute` executes the machine code in the given VM.

(define (execute vm code)
  (if (pair? code)
      (begin
        (write-register vm (cadar code)
          (case (caar code)
            ((EVL) (eval (caddar code)))
            ((NEG) (- (read-register vm (cadar code))))
            ((MOV) (caddar code))
            ((ADD) (+ (read-register vm (cadar code)) (read-register vm (caddar code))))
            ((SUB) (- (read-register vm (cadar code)) (read-register vm (caddar code))))
            ((MUL) (* (read-register vm (cadar code)) (read-register vm (caddar code))))
            ((DIV) (/ (read-register vm (cadar code)) (read-register vm (caddar code))))
            (else  (error "unknown instruction" (car code)))))
        (execute vm (cdr code)))))
