;;; SRFI 19
;;; Time data types and procedures
;;;
;;; Points in time are represented a the number of seconds (with nanosecond precision) since
;;; "the epoch," a zero point in time. Several standard variants are defined, including UTC
;;; (universal coordinated time), TAI (international atomic time), and monotonic time.
;;; A point in time can also be represented as a Julian Day or Modified Julian Day number.
;;; Time durations are defined. Conversion routines are provided. The procedure CURRENT-TIME
;;; queries the current time in a specified variant, with a system-dependent resolution.
;;; Procedures for time arithmetic and time comparisons are also provided.
;;; 
;;; A date is a representation of a point in time in the Gregorian calendar, a 24 hour clock
;;; (with nanosecond precision) and a time zone offset from UTC. Procedures for converting
;;; between time and dates are provided, as well as for reading and writing string
;;; representations of dates.
;;;
;;; Author: Will Fitzgerald
;;; Copyright © 2000-2003 I/NET, Inc. All Rights Reserved.
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
;;;   Copyright © 2017 Matthias Zenger. All rights reserved.

(define-library (srfi 19)

  (export add-duration
          add-duration!
          copy-time
          current-date
          current-julian-day
          current-modified-julian-day
          current-time
          date?
          date->julian-day
          date->modified-julian-day
          date->string
          date->time-monotonic
          date->time-tai
          date->time-utc
          date-day
          date-hour
          date-minute
          date-month
          date-nanosecond
          date-second
          date-week-day
          date-week-number
          date-year
          date-year-day
          date-zone-offset
          julian-day->date
          julian-day->time-monotonic
          julian-day->time-tai
          julian-day->time-utc
          make-date
          make-time
          modified-julian-day->date
          modified-julian-day->time-monotonic
          modified-julian-day->time-tai
          modified-julian-day->time-utc
          set-time-nanosecond!
          set-time-second!
          set-time-type!
          string->date
          subtract-duration
          subtract-duration!
          time-difference
          time-difference!
          time-duration
          time-monotonic
          time-monotonic->date
          time-monotonic->julian-day
          time-monotonic->modified-julian-day
          time-monotonic->time-tai
          time-monotonic->time-tai!
          time-monotonic->time-utc
          time-monotonic->time-utc!
          time-nanosecond
          time-resolution
          time-second
          time-tai
          time-tai->date
          time-tai->julian-day
          time-tai->modified-julian-day
          time-tai->time-monotonic
          time-tai->time-monotonic!
          time-tai->time-utc
          time-tai->time-utc!
          time-type
          time-utc
          time-utc->date
          time-utc->julian-day
          time-utc->modified-julian-day
          time-utc->time-monotonic
          time-utc->time-monotonic!
          time-utc->time-tai
          time-utc->time-tai!
          time?
          time<?
          time<=?
          time=?
          time>?
          time>=?)
  
  (import (lispkit base))

  (begin
  
    ;; -- Bug fixes.
    ;;
    ;; MAKE-TIME had parameters seconds and nanoseconds reversed; change all
    ;;           references in file to match.  Will F: 2002-10-15
    ;;
    ;; DATE-YEAR-DAY returned the wrong day; tm:year-day fixed to do the right
    ;;               thing. Will F: 2002-10-15
    ;;               It also called an undefined error procedure.
    ;;
    ;; DISPLAYING procedure removed. Will F: 2002-10-15.
    ;;
    ;; TM:NANO constant corrected. 2002-11-04.
    ;;
    ;; The following fixes by Will Fitzgerald, February, 2003.
    ;;  -- Thanks to Steven Ma and others.
    ;;
    ;; (CURRENT-TIME 'TIME-THREAD) added.
    ;;
    ;; TIME-RESOLUTION for TIME-PROCESS added. 
    ;;
    ;; TIME comparison procedures (time=?, etc. fixed. 
    ;;
    ;; Corrected errors in converting between TAI and UTC time.
    ;;
    ;; TAI and UTC date converters no longer look at leap seconds,
    ;; which was an error.
    ;;
    ;; corrections to calls to tm:time-error
    ;;
    ;; timezone offset not used in date->time-utc and date->julian-day
    ;;
    ;; typos in tm:integer-reader-exact, tm:string->date,
    ;; time-monotonic->time-utc!, tm:char->int fixed
    ;;
    ;; corrected "~k", "~f" formatting for date->string (includes fix for
    ;; "~4"
    ;;
    ;; 'split-real' fixed.
    ;;
    ;; fixed julian-day->time-utc and variants.
    ;;
    ;; changes 2003-02-26, based on comments by Martin Gasbichler.
    ;; 
    ;; moronic, overly complicated COPY-TIME procedure changed
    ;; to simple version suggested by Martin Gasbichler.
    ;;
    ;; To provide more portability, changed #\Space to #\space
    ;; and #\tab to #\Tab to (integer->char 9)
    ;;
    ;; changed arity-3 calls to / and - to arity 2 calls (again,
    ;; for more general portability). 
    ;;
    ;; split-real fixed again -- by removing it, and using
    ;; 'fractional part'. Will Fitzgerald 5/16/2003.

    (define time-tai       'time-tai)
    (define time-utc       'time-utc)
    (define time-monotonic 'time-monotonic)
    (define time-duration  'time-duration)

    ;;-- LOCALE dependent constants

    (define tm:locale-number-separator ".")

    (define tm:locale-abbr-weekday-vector (vector "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

    (define tm:locale-long-weekday-vector (vector "Sunday" "Monday" "Tuesday" "Wednesday"
                                                  "Thursday" "Friday" "Saturday"))

    ;; note empty string in 0th place. 
    (define tm:locale-abbr-month-vector   (vector "" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
                                                  "Aug" "Sep" "Oct" "Nov" "Dec"))

    (define tm:locale-long-month-vector   (vector "" "January" "February" "March" "April" "May"
                                                  "June" "July" "August" "September" "October"
                                                  "November" "December")) 

    (define tm:locale-am "am")
    (define tm:locale-pm "pm")

    ;; See date->string
    (define tm:locale-date-time-format "~a ~b ~d ~H:~M:~S~z ~Y")
    (define tm:locale-short-date-format "~m/~d/~y")
    (define tm:locale-time-format "~H:~M:~S")
    (define tm:iso-8601-date-time-format "~Y-~m-~dT~H:~M:~S~z")
    (define tm:uk-short-date-format "~d/~m/~y")
    (define tm:uk-short-date-time-format "~d/~m/~Y ~H:~M")

    ;;-- Miscellaneous Constants.
    ;;-- only the tm:tai-epoch-in-jd might need changing if a different epoch is used.

    (define tm:nano            (expt 10 9))
    (define tm:sid             86400)      ; seconds in a day
    (define tm:sihd            43200)      ; seconds in a half day
    (define tm:tai-epoch-in-jd 4881175/2)  ; julian day number for 'the epoch'

    ;; A table of leap seconds
    ;; See ftp://maia.usno.navy.mil/ser7/tai-utc.dat
    ;; and update as necessary.
    ;; This procedures reads the file in the abover format and creates the leap second table.
    ;; It also calls the almost standard, but not R5 procedures read-line & open-input-string
    ;; ie (set! tm:leap-second-table (tm:read-tai-utc-date "tai-utc.dat"))
    
    (define (tm:read-tai-utc-data filename)
      (let ((convert-jd (lambda (jd) (* (- (exact jd) tm:tai-epoch-in-jd) tm:sid)))
            (port (open-input-file filename))
            (table '()))
        (let loop ((line (read-line port)))
          (if (eof-object? line)
              table
              (begin (let* ((data (read (open-input-string (string-append "(" line ")"))))
                            (year (car data))
                            (jd   (cadddr (cdr data)))
                            (secs (cadddr (cdddr data))))
                       (if (>= year 1972)
                           (set! table (cons (cons (convert-jd jd) (exact secs)) table)))
                           (loop (read-line port))))))))

    ;; each entry is (utc seconds since epoch . # seconds to add for tai)
    ;; note they go higher to lower, and end in 1972.
    (define tm:leap-second-table
      '((1483228800 . 37)
        (1435708800 . 36)
        (1341100800 . 35)
        (1230768000 . 34)
        (1136073600 . 33)
         (915148800 . 32)
         (867715200 . 31)
         (820454400 . 30)
         (773020800 . 29)
         (741484800 . 28)
         (709948800 . 27)
         (662688000 . 26)
         (631152000 . 25)
         (567993600 . 24)
         (489024000 . 23)
         (425865600 . 22)
         (394329600 . 21)
         (362793600 . 20)
         (315532800 . 19)
         (283996800 . 18)
         (252460800 . 17)
         (220924800 . 16)
         (189302400 . 15)
         (157766400 . 14)
         (126230400 . 13)
          (94694400 . 12)
          (78796800 . 11)
          (63072000 . 10)))

    (define (read-leap-second-table filename)
      (set! tm:leap-second-table (tm:read-tai-utc-data filename)))

    (define (tm:leap-second-delta utc-seconds)
      (letrec ((lsd (lambda (table)
                      (if (>= utc-seconds (caar table))
                          (cdar table)
                          (lsd (cdr table))))))
        (if (< utc-seconds (* (- 1972 1970) 365 tm:sid))
            0
            (lsd tm:leap-second-table))))

    ;; going from tai seconds to utc seconds

    (define (tm:leap-second-neg-delta tai-seconds)
      (letrec ((lsd (lambda (table)
                      (cond ((null? table)                                  0)
                            ((<= (cdar table) (- tai-seconds (caar table))) (cdar table))
                            (else                                           (lsd (cdr table)))))))
        (if (< tai-seconds (* (- 1972 1970) 365 tm:sid))
            0
            (lsd tm:leap-second-table))))

    ;;; the time structure

    (define-record-type <time>
      (make-time type nanosecond second)
      time?
      (type time-type set-time-type!)
      (nanosecond time-nanosecond set-time-nanosecond!)
      (second time-second set-time-second!))

    ;; thanks, Martin Gasbichler ...

    (define (copy-time time)
      (make-time (time-type time)
                 (time-nanosecond time)
                 (time-second time)))

    ;;; specific time getters.
    ;;; these should be rewritten to be os specific.
    ;;
    ;; -- using gnu gettimeofday() would be useful here -- gets
    ;;    second + millisecond 
    ;;    let's pretend we do, using mzscheme's current-seconds & current-milliseconds
    ;;    this is supposed to return utc.
    ;; 

    (define (tm:get-time-of-day)
      (let ((cursec (current-second)))
        (cons (exact (truncate cursec))
              (abs (remainder (exact (truncate (* cursec 1000.0))) 1000)))))

    (define (tm:current-time-utc)
      (let ((tod (tm:get-time-of-day)))
        (make-time time-utc (* (cdr tod) 10000) (car tod))))

    (define (tm:current-time-tai)
      (let ((tod (tm:get-time-of-day)))
        (make-time time-tai (* (cdr tod) 10000) (+ (car tod) (tm:leap-second-delta (car tod))))))

    (define (tm:current-time-ms-time time-type proc)
      (let ((current-ms (proc)))
        (make-time time-type (* (remainder current-ms 1000) 10000) (quotient current-ms 10000))))

    ;; -- we define it to be the same as tai.
    ;;    a different implemation of current-time-montonic will require rewriting all of the
    ;;    time-monotonic converters, of course.

    (define (tm:current-time-monotonic)
      (let ((tod (tm:get-time-of-day)))
        (make-time time-monotonic (* (cdr tod) 10000)
                                  (+ (car tod) (tm:leap-second-delta (car tod))))))
    
    (define (current-time . clock-type)
      (cond ((null? clock-type)
              (tm:current-time-utc))
            ((eq? (car clock-type) time-tai)
              (tm:current-time-tai))
            ((eq? (car clock-type) time-utc)
              (tm:current-time-utc))
            ((eq? (car clock-type) time-monotonic)
              (tm:current-time-monotonic))
            (else
              (error "current-time: invalid clock type" (car clock-type)))))

    ;; -- time resolution
    ;; this is the resolution of the clock in nanoseconds.
    ;; this will be implementation specific.

    (define (time-resolution . clock-type)
      (cond ((null? clock-type) ; utc
              10000)
            ((eq? (car clock-type) time-tai)
              10000)
            ((eq? (car clock-type) time-utc)
              10000)
            ((eq? (car clock-type) time-monotonic)
              10000)
            (else
              (error "time-resolution: invalid clock type" clock-type))))

    ;; -- time comparisons

    (define (tm:time-compare-check time1 time2 caller)
      (if (or (not (and (time? time1) (time? time2)))
              (not (eq? (time-type time1) (time-type time2))))
          (error (string-append caller ": incompatible time types") time1 time2)))

    (define (time=? time1 time2)
      (tm:time-compare-check time1 time2 "time=?")
      (and (= (time-second time1) (time-second time2))
           (= (time-nanosecond time1) (time-nanosecond time2))))

    (define (time>? time1 time2)
      (tm:time-compare-check time1 time2 "time>")
      (or (> (time-second time1) (time-second time2))
          (and (= (time-second time1) (time-second time2))
               (> (time-nanosecond time1) (time-nanosecond time2)))))

    (define (time<? time1 time2)
      (tm:time-compare-check time1 time2 "time<")
      (or (< (time-second time1) (time-second time2))
          (and (= (time-second time1) (time-second time2))
               (< (time-nanosecond time1) (time-nanosecond time2)))))

    (define (time>=? time1 time2)
      (tm:time-compare-check time1 time2 "time>=")
      (or (> (time-second time1) (time-second time2))
          (and (= (time-second time1) (time-second time2))
               (>= (time-nanosecond time1) (time-nanosecond time2)))))

    (define (time<=? time1 time2)
      (tm:time-compare-check time1 time2 "time<=")
      (or (< (time-second time1) (time-second time2))
          (and (= (time-second time1) (time-second time2))
               (<= (time-nanosecond time1) (time-nanosecond time2)))))

    ;; -- time arithmetic

    (define (tm:time->nanoseconds time)
      (+ (* (time-second time) tm:nano) (time-nanosecond time)))

    (define (tm:nanoseconds->time time-type nanoseconds)
      (make-time time-type
                 (remainder nanoseconds tm:nano)
                 (quotient nanoseconds tm:nano)))

    (define (tm:time-difference time1 time2 time3)
      (if (or (not (and (time? time1) (time? time2)))
              (not (eq? (time-type time1) (time-type time2))))
          (error "time-difference: incompatible time types" time1 time2))
      (set-time-type! time3 time-duration)
      (if (time=? time1 time2)
          (begin (set-time-second! time3 0)
                 (set-time-nanosecond! time3 0))
          (let ((nanoseconds (- (tm:time->nanoseconds time1) (tm:time->nanoseconds time2))))
            (set-time-second! time3 (quotient nanoseconds tm:nano))
            (set-time-nanosecond! time3 (abs (remainder nanoseconds tm:nano)))))
      time3)

    (define (time-difference time1 time2)
      (tm:time-difference time1 time2 (make-time #f #f #f)))

    (define (time-difference! time1 time2)
      (tm:time-difference time1 time2 time1))

    (define (tm:add-duration time1 duration time3)
      (if (not (and (time? time1) (time? duration)))
          (error "tm:add-duration: incompatible time types" time1 duration))
      (if (not (eq? (time-type duration) time-duration))
          (error "tm:add-duration: not a duration" duration)
          (let* ((sec-plus  (+ (time-second time1) (time-second duration)))
                 (nsec-plus (+ (time-nanosecond time1) (time-nanosecond duration)))
                 (r (remainder nsec-plus tm:nano))
                 (q (quotient nsec-plus tm:nano)))
            ; (set-time-type! time3 (time-type time1))
            (if (negative? r)
                (begin (set-time-second! time3 (+ sec-plus q -1))
                       (set-time-nanosecond! time3 (+ tm:nano r)))
                (begin (set-time-second! time3 (+ sec-plus q))
                       (set-time-nanosecond! time3 r)))
            time3)))

    (define (add-duration time1 duration)
      (tm:add-duration time1 duration (make-time (time-type time1) #f #f)))

    (define (add-duration! time1 duration)
      (tm:add-duration time1 duration time1))

    (define (tm:subtract-duration time1 duration time3)
      (if (not (and (time? time1) (time? duration)))
          (error "tm:subtract-duration: incompatible time types" time1 duration))
      (if (not (eq? (time-type duration) time-duration))
          (error "tm:subtract-duration: not a duration" duration)
          (let* ((sec-minus  (- (time-second time1) (time-second duration)))
                 (nsec-minus (- (time-nanosecond time1) (time-nanosecond duration)))
                 (r (remainder nsec-minus tm:nano))
                 (q (quotient nsec-minus tm:nano)))
            (if (negative? r)
                (begin (set-time-second! time3 (- sec-minus q 1))
                       (set-time-nanosecond! time3 (+ tm:nano r)))
                (begin (set-time-second! time3 (- sec-minus q))
                       (set-time-nanosecond! time3 r)))
            time3)))

    (define (subtract-duration time1 duration)
      (tm:subtract-duration time1 duration (make-time (time-type time1) #f #f)))

    (define (subtract-duration! time1 duration)
      (tm:subtract-duration time1 duration time1))

    ;; -- converters between types

    (define (tm:time-tai->time-utc! time-in time-out caller)
      (if (not (eq? (time-type time-in) time-tai))
          (error (string-append caller ": expected time to be of type tai") time-in))
      (set-time-type!       time-out time-utc)
      (set-time-nanosecond! time-out (time-nanosecond time-in))
      (set-time-second!     time-out (- (time-second time-in)
                                        (tm:leap-second-neg-delta (time-second time-in))))
      time-out)

    (define (time-tai->time-utc time-in)
      (tm:time-tai->time-utc! time-in (make-time #f #f #f) "time-tai->time-utc"))

    (define (time-tai->time-utc! time-in)
      (tm:time-tai->time-utc! time-in time-in "time-tai->time-utc!"))

    (define (tm:time-utc->time-tai! time-in time-out caller)
      (if (not (eq? (time-type time-in) time-utc))
          (error (string-append caller ": expected time to be of type utc") time-in))
      (set-time-type!       time-out time-tai)
      (set-time-nanosecond! time-out (time-nanosecond time-in))
      (set-time-second!     time-out (+ (time-second time-in)
                                        (tm:leap-second-delta (time-second time-in))))
      time-out)

    (define (time-utc->time-tai time-in)
      (tm:time-utc->time-tai! time-in (make-time #f #f #f) "time-utc->time-tai"))

    (define (time-utc->time-tai! time-in)
      (tm:time-utc->time-tai! time-in time-in "time-utc->time-tai!"))

    ;; -- these depend on time-monotonic having the same definition as time-tai!

    (define (time-monotonic->time-utc time-in)
      (if (not (eq? (time-type time-in) time-monotonic))
          (error (string-append caller ": expected time to be of type monotonic") time-in))
      (let ((ntime (copy-time time-in)))
        (set-time-type! ntime time-tai)
        (tm:time-tai->time-utc! ntime ntime "time-monotonic->time-utc")))

    (define (time-monotonic->time-utc! time-in)
      (if (not (eq? (time-type time-in) time-monotonic))
          (error "time-monotonic->time-utc!: expected time to be of type monotonic" time-in))
      (set-time-type! time-in time-tai)
      (tm:time-tai->time-utc! time-in time-in "time-monotonic->time-utc"))

    (define (time-monotonic->time-tai time-in)
      (if (not (eq? (time-type time-in) time-monotonic))
          (error "time-monotonic->time-tai: expected time to be of type monotonic" time-in))
      (let ((ntime (copy-time time-in)))
        (set-time-type! ntime time-tai)
        ntime))

    (define (time-monotonic->time-tai! time-in)
      (if (not (eq? (time-type time-in) time-monotonic))
          (error "time-monotonic->time-tai!: expected time to be of type monotonic" time-in))
      (set-time-type! time-in time-tai)
      time-in)

    (define (time-utc->time-monotonic time-in)
      (if (not (eq? (time-type time-in) time-utc))
          (error "time-utc->time-monotonic: expected time to be of type utc" time-in))
      (let ((ntime (tm:time-utc->time-tai! time-in
                                           (make-time #f #f #f)
                                           "time-utc->time-monotonic")))
        (set-time-type! ntime time-monotonic)
        ntime))

    (define (time-utc->time-monotonic! time-in)
      (if (not (eq? (time-type time-in) time-utc))
          (error "time-utc->time-monotonic!: expected time to be of type utc" time-in))
      (let ((ntime (tm:time-utc->time-tai! time-in time-in "time-utc->time-monotonic!")))
        (set-time-type! ntime time-monotonic)
        ntime))

    (define (time-tai->time-monotonic time-in)
      (if (not (eq? (time-type time-in) time-tai))
          (error "time-tai->time-monotonic: expected time to be of type tai" time-in))
      (let ((ntime (copy-time time-in)))
        (set-time-type! ntime time-monotonic)
        ntime))

    (define (time-tai->time-monotonic! time-in)
      (if (not (eq? (time-type time-in) time-tai))
          (error "time-tai->time-monotonic!: expected time to be of type tai" time-in))
      (set-time-type! time-in time-monotonic)
      time-in)

    ;; -- date structures

    (define-record-type <date>
      (make-date nanosecond second minute hour day month year zone-offset)
      date?
      (nanosecond  date-nanosecond  tm:set-date-nanosecond!)
      (second      date-second      tm:set-date-second!)
      (minute      date-minute      tm:set-date-minute!)
      (hour        date-hour        tm:set-date-hour!)
      (day         date-day         tm:set-date-day!)
      (month       date-month       tm:set-date-month!)
      (year        date-year        tm:set-date-year!)
      (zone-offset date-zone-offset tm:set-date-zone-offset!))

    ;; gives the julian day which starts at noon.
    (define (tm:encode-julian-day-number day month year)
      (let* ((a (quotient (- 14 month) 12))
             (y (- (- (+ year 4800) a) (if (negative? year) -1 0)))
             (m (- (+ month (* 12 a)) 3)))
        (+ day
           (quotient (+ (* 153 m) 2) 5)
           (* 365 y)
           (quotient y 4)
           (- (quotient y 100))
           (quotient y 400)
           -32045)))

    (define (tm:char-pos char str index len)
      (cond ((>= index len)                       #f)
            ((char=? (string-ref str index) char) index)
            (else                                 (tm:char-pos char str (+ index 1) len))))

    (define (tm:fractional-part r)
      (if (integer? r)
          "0"
          (let* ((str (number->string (exact->inexact r)))
                 (ppos (tm:char-pos #\. str 0 (string-length str))))
            (substring str (+ ppos 1) (string-length str)))))

    ;; gives the seconds/date/month/year 
    (define (tm:decode-julian-day-number jdn)
      (let* ((days (truncate jdn))
             (a (+ days 32044))
             (b (quotient (+ (* 4 a) 3) 146097))
             (c (- a (quotient (* 146097 b) 4)))
             (d (quotient (+ (* 4 c) 3) 1461))
             (e (- c (quotient (* 1461 d) 4)))
             (m (quotient (+ (* 5 e) 2) 153))
             (y (+ (* 100 b) d -4800 (quotient m 10))))
        (list ; seconds date month year
          (* (- jdn days) tm:sid)
          (+ e (- (quotient (+ (* 153 m) 2) 5)) 1)
          (+ m 3 (* -12 (quotient m 10)))
          (if (>= 0 y) (- y 1) y))))

    ;; tm:local-tz-offset reports the number of seconds east of UTC (GMT) for the current time
    ;; zone (e.g., Pacific Standard Time is -28800), including any daylight-saving adjustment
    ;; (e.g., Pacific Daylight Time is -25200). Without argument, it returns the default
    ;; defined by the system. The argument overrides this default if provided.
    (define (tm:local-tz-offset . tzarg)
      (if (or (null? tzarg) (null? (car tzarg))) (seconds-from-gmt) (caar tzarg)))

    ;; special thing -- ignores nanos
    (define (tm:time->julian-day-number seconds tz-offset)
      (+ (/ (+ seconds tz-offset tm:sihd) tm:sid) tm:tai-epoch-in-jd))

    (define (tm:find proc l)
      (if (null? l) #f (if (proc (car l)) #t (tm:find proc (cdr l)))))

    (define (tm:tai-before-leap-second? second)
      (tm:find (lambda (x) (= second (- (+ (car x) (cdr x)) 1))) tm:leap-second-table))

    (define (tm:time->date time tz-offset ttype)
      (if (not (eq? (time-type time) ttype))
          (error "tm:time->date: incompatible time type" time ttype))
      (let ((offset (tm:local-tz-offset tz-offset)))
        (apply (lambda (secs date month year)
                 (let* ((hrs (quotient secs 3600))
                        (rem (remainder secs 3600))
                        (min (quotient rem 60))
                        (sec (remainder rem 60)))
                   (make-date (time-nanosecond time) sec min hrs date month year offset)))
               (tm:decode-julian-day-number
                 (tm:time->julian-day-number (time-second time) offset)))))

    (define (time-tai->date time . tz-offset)
      (if (tm:tai-before-leap-second? (time-second time))
          ;; if it's *right* before the leap, we need to pretend to subtract a second ...
          (let ((d (tm:time->date (subtract-duration! (time-tai->time-utc time)
                                                      (make-time time-duration 0 1))
                                  tz-offset time-utc)))
               (tm:set-date-second! d 60)
               d)
          (tm:time->date (time-tai->time-utc time) tz-offset time-utc)))

    (define (time-utc->date time . tz-offset)
      (tm:time->date time tz-offset time-utc))

    ;; again, time-monotonic is the same as time tai
    (define (time-monotonic->date time . tz-offset)
      (tm:time->date time tz-offset time-monotonic))

    (define (date->time-utc date)
      (let ((nanosecond (date-nanosecond date))
            (second (date-second date))
            (minute (date-minute date))
            (hour (date-hour date))
            (day (date-day date))
            (month (date-month date))
            (year (date-year date))
            (offset (date-zone-offset date)))
        (let ((jdays (- (tm:encode-julian-day-number day month year) tm:tai-epoch-in-jd)))
          (make-time time-utc
                     nanosecond
                     (+ (* (- jdays 1/2) 24 60 60)
                        (* hour 60 60)
                        (* minute 60)
                        second
                        (- offset))))))

    (define (date->time-tai d)
      (if (= (date-second d) 60)
          (subtract-duration! (time-utc->time-tai! (date->time-utc d))
                              (make-time time-duration 0 1))
          (time-utc->time-tai! (date->time-utc d))))

    (define (date->time-monotonic date)
      (time-utc->time-monotonic! (date->time-utc date)))

    (define (tm:leap-year? year)
      (or (= (modulo year 400) 0) (and (= (modulo year 4) 0) (not (= (modulo year 100) 0)))))

    (define (leap-year? date)
      (tm:leap-year? (date-year date)))

    ;; tm:year-day fixed: adding wrong number of days.
    (define  tm:month-assoc '((0 . 0) (1 . 31)  (2 . 59)   (3 . 90)   (4 . 120) 
                              (5 . 151) (6 . 181)  (7 . 212)  (8 . 243)
                              (9 . 273) (10 . 304) (11 . 334)))

    (define (tm:year-day day month year)
      (let ((days-pr (assoc (- month 1) tm:month-assoc)))
        (if (not days-pr)
            (error "tm:date-year-day: invalid month" month))
        (if (and (tm:leap-year? year) (> month 2))
            (+ day (cdr days-pr) 1)
            (+ day (cdr days-pr)))))

    (define (date-year-day date)
      (tm:year-day (date-day date) (date-month date) (date-year date)))

    ;; from calendar faq 
    (define (tm:week-day day month year)
      (let* ((a (quotient (- 14 month) 12))
             (y (- year a))
             (m (+ month (* 12 a) -2)))
        (modulo (+ day y (quotient y 4) (- (quotient y 100))
                   (quotient y 400) (quotient (* 31 m) 12))
                7)))

    (define (date-week-day date)
      (tm:week-day (date-day date) (date-month date) (date-year date)))

    (define (tm:days-before-first-week date day-of-week-starting-week)
      (let* ((first-day  (make-date 0 0 0 0 1 1 (date-year date) #f))
             (fdweek-day (date-week-day first-day)))
        (modulo (- day-of-week-starting-week fdweek-day) 7)))

    (define (date-week-number date day-of-week-starting-week)
      (quotient (- (date-year-day date)
                   (tm:days-before-first-week date day-of-week-starting-week)) 7))

    (define (current-date . tz-offset) 
      (time-utc->date (current-time time-utc) (tm:local-tz-offset tz-offset)))

    ;; given a 'two digit' number, find the year within 50 years +/-
    (define (tm:natural-year n)
      (let* ((current-year (date-year (current-date)))
             (current-century (* (quotient current-year 100) 100)))
        (cond ((>= n 100)                                     n)
              ((<  n 0)                                       n)
              ((<= (- (+ current-century n) current-year) 50) (+ current-century n))
              (else                                           (+ (- current-century 100) n)))))

    (define (date->julian-day date)
      (let ((nanosecond (date-nanosecond date))
            (second (date-second date))
            (minute (date-minute date))
            (hour (date-hour date))
            (day (date-day date))
            (month (date-month date))
            (year (date-year date))
            (offset (date-zone-offset date)))
        (+ (tm:encode-julian-day-number day month year)
           (- 1/2)
           (+ (/ (/ (+ (* hour 60 60)
           (* minute 60) second (/ nanosecond tm:nano)) tm:sid)
           (- offset))))))

    (define (date->modified-julian-day date)
      (- (date->julian-day date) 4800001/2))

    (define (time-utc->julian-day time)
      (if (not (eq? (time-type time) time-utc))
          (error "time-utc->julian-day: expected time in utc" time))
      (+ (/ (+ (time-second time) (/ (time-nanosecond time) tm:nano)) tm:sid) tm:tai-epoch-in-jd))

    (define (time-utc->modified-julian-day time)
      (- (time-utc->julian-day time) 4800001/2))

    (define (time-tai->julian-day time)
      (if (not (eq? (time-type time) time-tai))
          (error "time-tai->julian-day: expected time in tai" time))
      (+ (/ (+ (- (time-second time) (tm:leap-second-delta (time-second time)))
               (/ (time-nanosecond time) tm:nano))
            tm:sid)
         tm:tai-epoch-in-jd))

    (define (time-tai->modified-julian-day time)
      (- (time-tai->julian-day time) 4800001/2))

    ;; this is the same as time-tai->julian-day
    (define (time-monotonic->julian-day time)
      (if (not (eq? (time-type time) time-monotonic))
          (error "time-monotonic->julian-day: expected time in monotonic" time))
      (+ (/ (+ (- (time-second time) (tm:leap-second-delta (time-second time)))
               (/ (time-nanosecond time) tm:nano))
            tm:sid)
         tm:tai-epoch-in-jd))

    (define (time-monotonic->modified-julian-day time)
      (- (time-monotonic->julian-day time) 4800001/2))

    (define (julian-day->time-utc jdn)
      (let ((nanosecs (* tm:nano tm:sid (- jdn tm:tai-epoch-in-jd))))
        (make-time time-utc (remainder nanosecs tm:nano) (floor (/ nanosecs tm:nano)))))

    (define (julian-day->time-tai jdn)
      (time-utc->time-tai! (julian-day->time-utc jdn)))

    (define (julian-day->time-monotonic jdn)
      (time-utc->time-monotonic! (julian-day->time-utc jdn)))

    (define (julian-day->date jdn . tz-offset)
      (time-utc->date (julian-day->time-utc jdn) (tm:local-tz-offset tz-offset)))

    (define (modified-julian-day->date jdn . tz-offset)
      (julian-day->date (+ jdn 4800001/2) (tm:local-tz-offset tz-offset)))

    (define (modified-julian-day->time-utc jdn)
      (julian-day->time-utc (+ jdn 4800001/2)))

    (define (modified-julian-day->time-tai jdn)
      (julian-day->time-tai (+ jdn 4800001/2)))

    (define (modified-julian-day->time-monotonic jdn)
      (julian-day->time-monotonic (+ jdn 4800001/2)))

    (define (current-julian-day)
      (time-utc->julian-day (current-time time-utc)))

    (define (current-modified-julian-day)
      (time-utc->modified-julian-day (current-time time-utc)))
  
    ;; returns a string rep. of number N, of minimum LENGTH,
    ;; padded with character PAD-WITH. If PAD-WITH is #f, 
    ;; no padding is done, and it's as if number->string was used.
    ;; if string is longer than LENGTH, it's as if number->string was used.

    (define (tm:padding n pad-with length)
      (let* ((str (number->string n))
             (str-len (string-length str)))
        (if (or (>= str-len length) (not pad-with))
            str
            (string-append (make-string (- length str-len) pad-with) str))))

    (define (tm:last-n-digits i n)
      (abs (remainder i (expt 10 n))))

    (define (tm:locale-abbr-weekday n) 
      (vector-ref tm:locale-abbr-weekday-vector n))

    (define (tm:locale-long-weekday n)
      (vector-ref tm:locale-long-weekday-vector n))

    (define (tm:locale-abbr-month n)
      (vector-ref tm:locale-abbr-month-vector n))

    (define (tm:locale-long-month n)
      (vector-ref tm:locale-long-month-vector n))

    (define (tm:vector-find needle haystack comparator)
      (let ((len (vector-length haystack)))
        (let loop ((index 0))
          (cond ((>= index len)                                  #f)
                ((comparator needle (vector-ref haystack index)) index)
                (else                                            (loop (+ index 1)))))))

    (define (tm:locale-abbr-weekday->index string)
      (tm:vector-find string tm:locale-abbr-weekday-vector string=?))

    (define (tm:locale-long-weekday->index string)
      (tm:vector-find string tm:locale-long-weekday-vector string=?))

    (define (tm:locale-abbr-month->index string)
      (tm:vector-find string tm:locale-abbr-month-vector string=?))

    (define (tm:locale-long-month->index string)
      (tm:vector-find string tm:locale-long-month-vector string=?))

    ;; do nothing. 
    ;; Your implementation might want to do something...
    ;; 
    (define (tm:locale-print-time-zone date port)
      (void))

    ;; Again, locale specific.
    (define (tm:locale-am/pm hr)
      (if (> hr 11) tm:locale-pm tm:locale-am))

    (define (tm:tz-printer offset port)
      (cond ((= offset 0)       (display "Z" port))
            ((negative? offset) (display "-" port))
            (else               (display "+" port)))
      (if (not (= offset 0))
          (let ((hours (abs (quotient offset (* 60 60))))
                (minutes (abs (quotient (remainder offset (* 60 60)) 60))))
            (display (tm:padding hours #\0 2) port)
            (display (tm:padding minutes #\0 2) port))))

    ;; A table of output formatting directives.
    ;; the first time is the format char.
    ;; the second is a procedure that takes the date, a padding character
    ;; (which might be #f), and the output port.
    ;;
    (define tm:directives 
      (list
        (cons #\~ (lambda (date pad-with port) (display #\~ port)))
        (cons #\a (lambda (date pad-with port)
                    (display (tm:locale-abbr-weekday (date-week-day date)) port)))
        (cons #\A (lambda (date pad-with port)
                    (display (tm:locale-long-weekday (date-week-day date)) port)))
        (cons #\b (lambda (date pad-with port)
                    (display (tm:locale-abbr-month (date-month date)) port)))
        (cons #\B (lambda (date pad-with port)
                    (display (tm:locale-long-month (date-month date)) port)))
        (cons #\c (lambda (date pad-with port)
                    (display (date->string date tm:locale-date-time-format) port)))
        (cons #\d (lambda (date pad-with port)
                    (display (tm:padding (date-day date) #\0 2) port)))
        (cons #\D (lambda (date pad-with port)
                    (display (date->string date "~m/~d/~y") port)))
        (cons #\e (lambda (date pad-with port)
                    (display (tm:padding (date-day date) #\space 2) port)))
        (cons #\f (lambda (date pad-with port)
                    (if (> (date-nanosecond date) tm:nano)
                        (display (tm:padding (+ (date-second date) 1) pad-with 2) port)
                        (display (tm:padding (date-second date) pad-with 2) port))
                    (let* ((ns (tm:fractional-part (/ (date-nanosecond date) tm:nano 1.0)))
                           (le (string-length ns)))
                      (if (> le 2)
                          (begin (display tm:locale-number-separator port)
                                 (display (substring ns 2 le) port))))))
        (cons #\h (lambda (date pad-with port) (display (date->string date "~b") port)))
        (cons #\H (lambda (date pad-with port)
                    (display (tm:padding (date-hour date) pad-with 2) port)))
        (cons #\I (lambda (date pad-with port)
                    (let ((hr (date-hour date)))
                      (if (> hr 12)
                          (display (tm:padding (- hr 12) pad-with 2) port)
                          (display (tm:padding hr pad-with 2) port)))))
        (cons #\j (lambda (date pad-with port)
                    (display (tm:padding (date-year-day date) pad-with 3) port)))
        (cons #\k (lambda (date pad-with port)
                    (display (tm:padding (date-hour date) #\0 2) port)))
        (cons #\l (lambda (date pad-with port)
                    (let ((hr (if (> (date-hour date) 12)
                                  (- (date-hour date) 12)
                                  (date-hour date))))
                      (display (tm:padding hr  #\space 2) port))))
        (cons #\m (lambda (date pad-with port)
                    (display (tm:padding (date-month date) pad-with 2) port)))
        (cons #\M (lambda (date pad-with port)
                    (display (tm:padding (date-minute date) pad-with 2) port)))
        (cons #\n (lambda (date pad-with port) (newline port)))
        (cons #\N (lambda (date pad-with port)
                    (display (tm:padding (date-nanosecond date) pad-with 9) port)))
        (cons #\p (lambda (date pad-with port) (display (tm:locale-am/pm (date-hour date)) port)))
        (cons #\r (lambda (date pad-with port) (display (date->string date "~I:~M:~S ~p") port)))
        (cons #\s (lambda (date pad-with port) (display (time-second (date->time-utc date)) port)))
        (cons #\S (lambda (date pad-with port)
                    (if (> (date-nanosecond date) tm:nano)
                        (display (tm:padding (+ (date-second date) 1) pad-with 2) port)
                        (display (tm:padding (date-second date) pad-with 2) port))))
        (cons #\t (lambda (date pad-with port) (display (integer->char 9) port)))
        (cons #\T (lambda (date pad-with port) (display (date->string date "~H:~M:~S") port)))
        (cons #\U (lambda (date pad-with port)
                    (if (> (tm:days-before-first-week date 0) 0)
                        (display (tm:padding (+ (date-week-number date 0) 1) #\0 2) port)
                        (display (tm:padding (date-week-number date 0) #\0 2) port))))
        (cons #\V (lambda (date pad-with port)
                    (display (tm:padding (date-week-number date 1) #\0 2) port)))
        (cons #\w (lambda (date pad-with port) (display (date-week-day date) port)))
        (cons #\x (lambda (date pad-with port)
                    (display (date->string date tm:locale-short-date-format) port)))
        (cons #\X (lambda (date pad-with port)
                    (display (date->string date tm:locale-time-format) port)))
        (cons #\W (lambda (date pad-with port)
                    (if (> (tm:days-before-first-week date 1) 0)
                        (display (tm:padding (+ (date-week-number date 1) 1) #\0 2) port)
                        (display (tm:padding (date-week-number date 1) #\0 2) port))))
        (cons #\y (lambda (date pad-with port)
                    (display (tm:padding (tm:last-n-digits (date-year date) 2) pad-with 2) port)))
        (cons #\Y (lambda (date pad-with port)
                    (display (tm:padding (date-year date) pad-with 4) port)))
        (cons #\z (lambda (date pad-with port)
                    (tm:tz-printer (date-zone-offset date) port)))
        (cons #\Z (lambda (date pad-with port)
                    (tm:locale-print-time-zone date port)))
        (cons #\1 (lambda (date pad-with port)
                    (display (date->string date "~Y-~m-~d") port)))
        (cons #\2 (lambda (date pad-with port)
                    (display (date->string date "~k:~M:~S~z") port)))
        (cons #\3 (lambda (date pad-with port)
                    (display (date->string date "~k:~M:~S") port)))
        (cons #\4 (lambda (date pad-with port)
                    (display (date->string date "~Y-~m-~dT~k:~M:~S~z") port)))
        (cons #\5 (lambda (date pad-with port)
                    (display (date->string date "~Y-~m-~dT~k:~M:~S") port)))))

    (define (tm:get-formatter char)
      (let ((associated (assoc char tm:directives)))
        (if associated (cdr associated) #f)))

    (define (tm:date-printer date index formatstr str-len port)
      (if (>= index str-len)
          (void)
          (let ((current-char (string-ref formatstr index)))
            (if (not (char=? current-char #\~))
                (begin (display current-char port)
                       (tm:date-printer date (+ index 1) formatstr str-len port))
                (if (= (+ index 1) str-len) ; bad format string.
                    (error "tm:date-printer: bad date format string" formatstr)
                    (let ((pad-char? (string-ref formatstr (+ index 1))))
                      (cond ((char=? pad-char? #\-)
                               (if (= (+ index 2) str-len) ; bad format string.
                                   (error "tm:date-printer: bad date format string" formatstr)
                                   (let ((formatter (tm:get-formatter
                                                      (string-ref formatstr (+ index 2)))))
                                     (if (not formatter)
                                         (error "tm:date-printer: bad date format string" formatstr)
                                         (begin (formatter date #f port)
                                                (tm:date-printer date
                                                                 (+ index 3)
                                                                 formatstr str-len port))))))
                            ((char=? pad-char? #\_)
                               (if (= (+ index 2) str-len) ; bad format string.
                                   (error "tm:date-printer: bad date format string" formatstr)
                                   (let ((formatter (tm:get-formatter 
                                                      (string-ref formatstr (+ index 2)))))
                                     (if (not formatter)
                                         (error "tm:date-printer: bad date format string" formatstr)
                                         (begin (formatter date #\space port)
                                                (tm:date-printer date
                                                                 (+ index 3)
                                                                 formatstr str-len port))))))
                            (else
                               (let ((formatter (tm:get-formatter 
                                                  (string-ref formatstr (+ index 1)))))
                                 (if (not formatter)
                                     (error "tm:date-printer: bad date format string" formatstr)
                                     (begin (formatter date #\0 port)
                                            (tm:date-printer date
                                                             (+ index 2)
                                                             formatstr str-len port))))))))))))

    (define (date->string date .  format-string)
      (let ((str-port (open-output-string))
            (fmt-str (if (null? format-string) "~c" (car format-string))))
        (tm:date-printer date 0 fmt-str (string-length fmt-str) str-port)
        (get-output-string str-port)))

    (define (tm:char->int ch)
      (cond ((char=? ch #\0) 0)
            ((char=? ch #\1) 1)
            ((char=? ch #\2) 2)
            ((char=? ch #\3) 3)
            ((char=? ch #\4) 4)
            ((char=? ch #\5) 5)
            ((char=? ch #\6) 6)
            ((char=? ch #\7) 7)
            ((char=? ch #\8) 8)
            ((char=? ch #\9) 9)
            (else (error "string->date: bad date template string; non-digit character" ch))))

    ;; read an integer upto n characters long on port; upto -> #f if any length
    (define (tm:integer-reader upto port)
      (let loop ((accum 0)
                 (nchars 0))
        (let ((ch (peek-char port)))
          (if (or (eof-object? ch) (not (char-numeric? ch)) (and upto (>= nchars  upto )))
              accum
              (loop (+ (* accum 10) (tm:char->int (read-char port))) (+ nchars 1))))))

    (define (tm:make-integer-reader upto)
      (lambda (port) (tm:integer-reader upto port)))

    ;; read an fractional integer upto n characters long on port; upto -> #f if any length
    ;; 
    ;; The return value is normalized to upto decimal places. For example, if upto is 9 and 
    ;; the string read is "123", the return value is 123000000.
    (define (tm:fractional-integer-reader upto port)
      (let loop ((accum 0)
                 (nchars 0))
        (let ((ch (peek-char port)))
          (if (or (eof-object? ch) (not (char-numeric? ch))	(and upto (>= nchars  upto )))
              (* accum (expt 10 (- upto nchars)))
              (loop port (+ (* accum 10) (tm:char->int (read-char port))) (+ nchars 1))))))

    (define (tm:make-fractional-integer-reader upto)
      (lambda (port) (tm:fractional-integer-reader upto port)))

    ;; read *exactly* n characters and convert to integer; could be padded
    (define (tm:integer-reader-exact n port)
      (let loop ((padding-ok #t)
                 (accum 0)
                 (nchars 0))
        (let ((ch (peek-char port)))
          (cond ((>= nchars n) accum)
                ((eof-object? ch) 
                  (error "tm:integer-reader-exact: premature ending to integer read"))
                ((char-numeric? ch)
                  (loop #f (+ (* accum 10) (tm:char->int (read-char port))) (+ nchars 1)))
                (padding-ok
                  (read-char port) ; consume padding
                  (loop padding-ok accum (+ nchars 1)))
                (else ; padding where it shouldn't be
                  (error "tm:integer-reader-exact: non-numeric characters in integer read"))))))

    (define (tm:make-integer-exact-reader n)
      (lambda (port) (tm:integer-reader-exact n port)))

    (define (tm:zone-reader port) 
      (let ((offset 0) 
            (positive? #f))
        (let ((ch (read-char port)))
          (if (eof-object? ch)
              (error "tm:zone-reader: invalid time zone +/-"))
          (if (or (char=? ch #\Z) (char=? ch #\z))
              0
              (begin (cond ((char=? ch #\+) (set! positive? #t))
                           ((char=? ch #\-) (set! positive? #f))
                           (else
                              (error "tm:zone-reader: invalid time zone +/-" ch)))
                     (let ((ch (read-char port)))
                       (if (eof-object? ch)
                           (error "tm:zone-reader: invalid time zone number" ch))
                       (set! offset (* (tm:char->int ch) 10 60 60)))
                     (let ((ch (read-char port)))
                       (if (eof-object? ch)
                           (error "tm:zone-reader: invalid time zone number" ch))
                       (set! offset (+ offset (* (tm:char->int ch) 60 60))))
                     (let ((ch (read-char port)))
                       (if (eof-object? ch)
                           (error "tm:zone-reader: invalid time zone number" ch))
                       (set! offset (+ offset (* (tm:char->int ch) 10 60))))
                     (let ((ch (read-char port)))
                       (if (eof-object? ch)
                           (error "tm:zone-reader: invalid time zone number" ch))
                       (set! offset (+ offset (* (tm:char->int ch) 60))))
                     (if positive? offset (- offset)))))))

    ;; looking at a char, read the char string, run thru indexer, return index
    (define (tm:locale-reader port indexer)
      (letrec ((string-port (open-output-string))
               (read-char-string (lambda () 
                                   (let ((ch (peek-char port)))
                                     (if (char-alphabetic? ch)
                                         (begin (write-char (read-char port) string-port) 
                                                (read-char-string))
                                         (get-output-string string-port))))))
        (or (indexer (read-char-string)) (error "tm:locale-reader: invalid string for " indexer))))

    (define (tm:make-locale-reader indexer)
      (lambda (port) (tm:locale-reader port indexer)))

    (define (tm:make-char-id-reader char)
      (lambda (port)
        (if (char=? char (read-char port))
            char
            (error "tm:make-char-id-reader: invalid character match" char))))

    ;; A List of formatted read directives.
    ;; Each entry is a list.
    ;; 1. the character directive; 
    ;; 2. a procedure, which takes a character as input & returns #t as soon as a character
    ;;    on the input port is acceptable for input,
    ;; 3. a port reader procedure that knows how to read the current port for a value.
    ;;    Its one parameter is the port.
    ;; 4. a action procedure, that takes the value (from 3.) and some object (here, always
    ;;    the date) and (probably) side-effects it. In some cases (e.g., ~A) the action is
    ;;    to do nothing

    (define tm:read-directives 
      (let ((ireader4                   (tm:make-integer-reader 4))
            (ireader2                   (tm:make-integer-reader 2))
            (fireader9                  (tm:make-fractional-integer-reader 9))
            (ireaderf                   (tm:make-integer-reader #f))
            (eireader2                  (tm:make-integer-exact-reader 2))
            (eireader4                  (tm:make-integer-exact-reader 4))
            (locale-reader-abbr-weekday (tm:make-locale-reader tm:locale-abbr-weekday->index))
            (locale-reader-long-weekday (tm:make-locale-reader tm:locale-long-weekday->index))
            (locale-reader-abbr-month   (tm:make-locale-reader tm:locale-abbr-month->index))
            (locale-reader-long-month   (tm:make-locale-reader tm:locale-long-month->index))
            (char-fail                  (lambda (ch) #t))
            (do-nothing                 (lambda (val object) (void))))    
        (list
         (list #\~ char-fail        (tm:make-char-id-reader #\~) do-nothing)
         (list #\a char-alphabetic? locale-reader-abbr-weekday do-nothing)
         (list #\A char-alphabetic? locale-reader-long-weekday do-nothing)
         (list #\b char-alphabetic? locale-reader-abbr-month
                                    (lambda (val object) (tm:set-date-month! object val)))
         (list #\B char-alphabetic? locale-reader-long-month
                                    (lambda (val object) (tm:set-date-month! object val)))
         (list #\d char-numeric?    ireader2
                                    (lambda (val object) (tm:set-date-day! object val)))
         (list #\e char-fail        eireader2
                                    (lambda (val object) (tm:set-date-day! object val)))
         (list #\h char-alphabetic? locale-reader-abbr-month
                                    (lambda (val object) (tm:set-date-month! object val)))
         (list #\H char-numeric?    ireader2
                                    (lambda (val object) (tm:set-date-hour! object val)))
         (list #\k char-fail        eireader2
                                    (lambda (val object) (tm:set-date-hour! object val)))
         (list #\m char-numeric?    ireader2
                                    (lambda (val object) (tm:set-date-month! object val)))
         (list #\M char-numeric?    ireader2
                                    (lambda (val object) (tm:set-date-minute! object val)))
         (list #\N char-numeric?    fireader9
                                    (lambda (val object) (tm:set-date-nanosecond! object val)))
         (list #\S char-numeric?    ireader2
                                    (lambda (val object) (tm:set-date-second! object val)))
         (list #\y char-fail        eireader2 
                                    (lambda (val object)
                                      (tm:set-date-year! object (tm:natural-year val))))
         (list #\Y char-numeric?    ireader4
                                    (lambda (val object) (tm:set-date-year! object val)))
         (list #\z (lambda (c) (or (char=? c #\Z) (char=? c #\z) (char=? c #\+) (char=? c #\-)))
                   tm:zone-reader
                   (lambda (val object) (tm:set-date-zone-offset! object val))))))

    (define (tm:string->date date index format-string str-len port template)
      (letrec ((skip-until (lambda (skipper)
                 (let ((ch (peek-char port)))
                   (if (eof-object? ch)
                       (error "tm:string->date: bad template string" template)
                       (if (not (skipper ch))
                           (begin (read-char port)
                                  (skip-until skipper))))))))
        (if (>= index str-len)
            (void)
            (let ((current-char (string-ref format-string index)))
              (if (not (char=? current-char #\~))
                  (let ((port-char (read-char port)))
                    (if (or (eof-object? port-char) (not (char=? current-char port-char)))
                        (error "tm:string->date: bad date format string" template))
                    (tm:string->date date (+ index 1) format-string str-len port template))
                  ;; otherwise, it's an escape, we hope
                  (if (> (+ index 1) str-len)
                      (error "tm:string->date: bad date format string" template)
                      (let* ((format-char (string-ref format-string (+ index 1)))
                             (format-info (assoc format-char tm:read-directives)))
                        (if (not format-info)
                            (error "tm:string->date: bad date format string" template)
                            (begin (let ((skipper (cadr format-info))
                                         (reader  (caddr format-info))
                                         (actor   (cadddr format-info)))
                                     (display "1-\n")
                                     (skip-until skipper)
                                     (display "2-\n")
                                     (let ((val (reader port)))
                                       (if (eof-object? val)
                                           (error "tm:string->date: bad date" template)
                                           (actor val date)))
                                     (tm:string->date date
                                                      (+ index 2)
                                                      format-string
                                                      str-len
                                                      port
                                                      template)))))))))))

    (define (string->date input-string template)
      (let ((newdate (make-date 0 0 0 0 #f #f #f (tm:local-tz-offset))))
        (tm:string->date newdate
                         0
                         template
                         (string-length template)
                         (open-input-string input-string)
                         template)
        (if (and (date-nanosecond newdate)
                 (date-second newdate)
                 (date-minute newdate)
                 (date-hour newdate)
                 (date-day newdate)
                 (date-month newdate)
                 (date-year newdate)
                 (date-zone-offset newdate))
            newdate
            (error "string->date: incomplete date read" newdate template))))))
