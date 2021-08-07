;;; SRFI 215
;;; Central Log Exchange
;;; 
;;; This SRFI specifies a central log exchange for Scheme that connects log producers
;;; with log consumers. It allows multiple logging systems to interoperate and co-exist
;;; in the same program. Library code can produce log messages without knowledge of
;;; which log system is actually used. Simple applications can easily get logs on
;;; standard output, while more advanced applications can send them to a full
;;; logging system.
;;; 
;;; Copyright © 2020 Göran Weinholt. All rights reserved.
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
;;;   Copyright © 2021 Matthias Zenger. All rights reserved.

(define-library (srfi 215)
  
  (export send-log
          current-log-fields
          current-log-callback
          EMERGENCY
          ALERT
          CRITICAL
          ERROR
          WARNING
          NOTICE
          INFO
          DEBUG)
  
  (import (lispkit base))
  
  (begin
    
    ;; This queue code is based on public domain code from SLIB,
    ;; originally written by Andrew Wilcox in 1992. Here it has been
    ;; reduced in size and rewritten to use mutable pairs.
    
    (define (make-queue)
      (mcons '() '()))
    
    (define (enqueue! q datum)
      (let ((new-pair (mcons datum '())))
        (if (null? (mcar q))
            (set-mcar! q new-pair)
            (set-mcdr! (mcdr q) new-pair))
        (set-mcdr! q new-pair)))
    
    (define (dequeue! q)
      (let ((first-pair (mcar q)))
        (if (null? first-pair)
            (error "attempt to dequeue an empty queue"))
        (let ((first-cdr (mcdr first-pair)))
          (set-mcar! q first-cdr)
          (when (null? first-cdr)
            (set-mcdr! q '()))
          (mcar first-pair))))
    
    (define (queue-empty? q)
      (null? (mcar q)))
    
    ;; These severities are from RFC 5424 ("The Syslog Protocol").
    (define EMERGENCY 0)              ; system is unusable
    (define ALERT     1)              ; action must be taken immediately
    (define CRITICAL  2)              ; critical conditions
    (define ERROR     3)              ; error conditions
    (define WARNING   4)              ; warning conditions
    (define NOTICE    5)              ; normal but significant condition
    (define INFO      6)              ; informational messages
    (define DEBUG     7)              ; debug-level messages
    
    (define (field-list->alist plist)
      (let f ((fields plist))
        (cond ((null? fields)
               '())
              ((or (not (pair? fields)) (not (pair? (cdr fields))))
               (error "short field list" plist))
              (else
               (let ((k (car fields)) (v (cadr fields)))
                 (if (not v)
                     (f (cddr fields))
                     (let ((k^ (cond ((symbol? k) k)
                                     (else
                                      (error "invalid key" k plist))))
                           (v^ (cond ((string? v) v)
                                     ((and (integer? v) (exact? v)) v)
                                     ((bytevector? v) v)
                                     ;; ((condition?) v) ;R6RS
                                     ((error-object? v) v) ;R7RS
                                     (else
                                      (let ((p (open-output-string)))
                                        (write v p)
                                        (get-output-string p))))))
                       (cons (cons k^ v^)
                             (f (cddr fields))))))))))
    
    (define current-log-fields
      (make-parameter '()
                      (lambda (plist)
                        (field-list->alist plist)
                        plist)))
    
    (define current-log-callback
      (let ((num-pending-logs 0)
            (pending-logs (make-queue)))
        (make-parameter (lambda (log-entry)
                          (enqueue! pending-logs log-entry)
                          (if (eqv? num-pending-logs 100)
                              (dequeue! pending-logs)
                              (set! num-pending-logs (+ num-pending-logs 1))))
                        (lambda (hook)
                          (unless (procedure? hook)
                            (error "current-log-hook: expected a procedure" hook))
                          (let ((q pending-logs))
                            (set! num-pending-logs 0)
                            (set! pending-logs (make-queue))
                            (let lp ()
                              (unless (queue-empty? q)
                                (hook (dequeue! q))
                                (lp))))
                          hook))))
    
    ;; Send a log entry with the given severity and message. This
    ;; procedure also takes a list of extra keys and values.
    (define (send-log severity message . plist)
      (unless (and (exact? severity) (integer? severity) (<= 0 severity 7))
        (error "send-log: expected a severity from 0 to 7"
               severity message plist))
      (unless (string? message)
        (error "send-log: expected message to be a string"
               severity message plist))
      (let* ((fields (append plist (current-log-fields)))
             (alist (field-list->alist fields)))
        ((current-log-callback) `((SEVERITY . ,severity)
                                  (MESSAGE . ,message)
                                  ,@alist))))
  )
)
