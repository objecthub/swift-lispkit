;;; SRFI 272 COLORIZE
;;; Pretty Printing: color support
;;;
;;; Library `(srfi 272 colorize)` provides a framework for adding color and text
;;; attributes to the output of the pretty printer, primarily for terminals that
;;; support ANSI SGR (Select Graphic Rendition) control sequences. It implements
;;; facilities for detecting terminal capabilities, generating SGR sequences for
;;; different support tiers, bundling them into palettes, and mapping semantic
;;; color categories (e.g. `comment`, `keyword`, `string`) to visual styles.
;;;
;;; Copyright © 2026 Sergei Egorov. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;; this software and associated documentation files (the "Software"), to deal in the
;;; Software without restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so, subject to the
;;; following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;; PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; LispKit Port:
;;;   Copyright © 2026 Matthias Zenger. All rights reserved.

(define-library (srfi 272 colorize)
  (import (scheme base) (scheme write) (scheme case-lambda)
    (scheme char) (scheme process-context))

  ; procedures
  (export detect-sgr-tier sgr0 sgr1 sgr2 sgr3 make-asb
    asb->sgr-string make-asb-palette default-asb-palette
    asb-palette-ref semantic-color-mapper?
    make-semantic-color-mapper default-semantic-color-mapper
    semantic-color->start-string semantic-color->end-string)

  ; parameters
  (export sgr-support-tier)

  (begin
    (define get-env get-environment-variable)

    ; much simplified
    (define-syntax let*-optionals
      (syntax-rules ()
        ((_ args () . body)
         (let ()
           (when (pair? args) (error "too many arguments" args)) . body))
        ((_ args ((var def) . more) . body)
         (let* ((var (if (null? args) def (car args)))
                (rest (if (null? args) '() (cdr args))))
           (let*-optionals rest more . body)))))

    (define (string-contains-ci? hay needle)
      (let* ((h (string-downcase hay))
             (n (string-downcase needle))
             (hn (string-length h))
             (nn (string-length n)))
        (let loop ((i 0))
          (cond ((> (+ i nn) hn) #f)
                ((string=? n (substring h i (+ i nn))) i)
                (else (loop (+ i 1)))))))

    (define (string-suffix-ci? s suf)
      (let* ((s (string-downcase s))
             (suf (string-downcase suf))
             (n (string-length s))
             (m (string-length suf)))
        (and (>= n m) (string=? (substring s (- n m) n) suf))))

    (define escape (string (integer->char 27)))

    ; Env helpers

    (define (in-env? name needle)
      (let ((v (get-env name)))
        (and v (string-contains-ci? v needle))))

    (define (env-flag-true? name)
      (let ((v (get-env name)))
        (and v
             (let ((s (string-downcase v)))
               (or (string=? s "") (string=? s "1") (string=? s "true")
                   (string=? s "yes") (string=? s "on") (string=? s "always"))))))

    (define (env-flag-false? name)
      (let ((v (get-env name)))
        (and v
             (let ((s (string-downcase v)))
               (or (string=? s "0") (string=? s "false") (string=? s "no")
                   (string=? s "off") (string=? s "never"))))))

    ; Heuristics: capabilities

    (define (truecolor-supported?)
      (let* ((term (or (get-env "TERM") ""))
             (term-low (string-downcase term)))
        (or (in-env? "COLORTERM" "truecolor")
            (in-env? "COLORTERM" "24bit")
            (string-suffix-ci? term-low "-direct")
            (or (string-ci=? term-low "wezterm")
                (string-ci=? term-low "xterm-kitty")
                (string-ci=? term-low "foot")
                (string-ci=? term-low "foot-extra")
                (string-ci=? term-low "alacritty"))
            (get-env "WEZTERM_EXECUTABLE") (get-env "WEZTERM_PANE")
            (get-env "KITTY_WINDOW_ID") (get-env "WT_SESSION")
            (get-env "VTE_VERSION") (get-env "KONSOLE_VERSION")
            (in-env? "TERM_PROGRAM" "iTerm")
            (get-env "ITERM_SESSION_ID"))))

    (define (colors256-supported?)
      (let* ((term (or (get-env "TERM") "")))
        (or (truecolor-supported?) (string-contains-ci? term "256color")
            (string-ci=? term "screen-256color")
            (string-ci=? term "tmux-256color"))))

    ; conservative: mark only “known rich” terminals as Tier 3.
    (define (extended-sgr-supported?)
      (let* ((term (or (get-env "TERM") "")))
        (or (string-ci=? term "xterm-kitty")
            (string-ci=? term "wezterm") (string-ci=? term "foot")
            (string-ci=? term "foot-extra")
            (in-env? "TERM_PROGRAM" "iTerm") (get-env "WT_SESSION"))))

    ; disable conditions (return #f)
    (define (ansi-sgr-disallowed?)
      (let*
        ((term (string-downcase (or (get-env "TERM") "")))
         (os (string-downcase (or (get-env "OS") "")))
         (windows?
          (or (string=? os "windows_nt") (get-env "WT_SESSION")
              (get-env "ConEmuANSI") (get-env "ANSICON")))
         ; Force flags
         (force-on?
          (or (env-flag-true? "CLICOLOR_FORCE")
              (env-flag-true? "FORCE_COLOR")))
         (force-off?
          (or (env-flag-false? "FORCE_COLOR") (env-flag-false? "CLICOLOR")
              (get-env "NO_COLOR"))) ; presence means “off” per spec
         ; User override via SGR_TIER
         (override (get-env "SGR_TIER"))
         (override-off?
          (and override
               (let ((s (string-downcase override)))
                 (or (string=? s "off") (string=? s "none") (string=? s "no")
                     (string=? s "-1") (string=? s "disable"))))))
        (cond (override-off? #t)
              (force-off? #t)
              ; TERM says “don’t try”
              ((or (string-ci=? term "dumb")
                   (and (not windows?) (string-ci=? term ""))
                   (string-ci=? term "unknown"))
               (not force-on?))
              ; Windows classic consoles: disable unless we detect known VT support or force on
              (windows?
               (let
                 ((has-vt?
                   (or (get-env "WT_SESSION") (get-env "ANSICON")
                       (string-contains-ci? term "xterm")
                       (string-contains-ci? term "cygwin")
                       (string-contains-ci? term "msys")
                       (string-contains-ci? term "mintty")
                       (in-env? "TERM_PROGRAM" "vscode"))))
                 (not (or has-vt? force-on?))))
              ; old pure VT terminals without color (vt100/vt220/vt52): disable unless forced
              ((or (string-ci=? term "vt100") (string-ci=? term "vt102")
                   (string-ci=? term "vt220") (string-ci=? term "vt320")
                   (string-ci=? term "vt52"))
               (not force-on?))
              ; otherwise, allowed
              (else #f))))

    ; returns one of: #f (no SGR at all), 0, 1, 2, 3
    (define (detect-sgr-tier)
      (let ((override (get-env "SGR_TIER")))
        (cond ((and override
                    (let ((s (string-downcase override)))
                      (or (string=? s "off") (string=? s "none") (string=? s "no")
                          (string=? s "-1") (string=? s "disable"))))
               #f)
              ; explicit numeric override 0..3
              ((and override
                    (let ((n (string->number override)))
                      (and n (exact-integer? n) (<= 0 n) (<= n 3))))
               (exact (string->number override)))
              ; heuristic disable
              ((ansi-sgr-disallowed?) #f)
              ; otherwise classify
              ((and (extended-sgr-supported?) (truecolor-supported?)) 3)
              ((truecolor-supported?) 2)
              ((colors256-supported?) 1)
              (else 0))))


    ; public parameter (can be set or parameterized)

    (define sgr-support-tier (make-parameter (detect-sgr-tier)))

    ; tier-specific SGR sequence generators

    ; tier0-terminal sgr sequences
    (define (sgr0 fg16 . more)
      (define (fg i) (if (< i 8) (+ 30 i) (+ 90 (- i 8))))
      (define (bg i) (if (< i 8) (+ 40 i) (+ 100 (- i 8))))
      (let*-optionals more ((bold? #f) (uline? #f) (bg16 #f))
        (unless (<= 0 fg16 15) (error "invalid ansi-16 color" fg16))
        (when bg16
          (unless (<= 0 bg16 15) (error "invalid ansi-16 color" bg16)))
        (let
          ((fgs (number->string (fg fg16)))
           (bs (if bold? ";1" ""))
           (us (if uline? ";4" ""))
           (bgs
            (if bg16 (string-append ";" (number->string (bg bg16))) "")))
          (string-append fgs bs us bgs))))

    ; tier1-terminal sgr sequences
    (define (sgr1 fg256 . more)
      (let*-optionals more ((bold? #f) (uline? #f) (bg256 #f))
        (unless (<= 0 fg256 255)
          (error "invalid ansi-256 color" fg256))
        (when bg256
          (unless (<= 0 bg256 255)
            (error "invalid ansi-256 color" bg256)))
        (let
          ((fgs (string-append "38;5;" (number->string fg256)))
           (bs (if bold? ";1" ""))
           (us (if uline? ";4" ""))
           (bgs
            (if bg256
                (string-append ";48;5;" (number->string bg256))
                "")))
          (string-append fgs bs us bgs))))

    ; tier2-terminal sgr sequences
    (define (sgr2 fgtrue . more)
      (define (rgbs rgb)
        (string-append
          (number->string (remainder (quotient rgb 65536) 256)) ";"
          (number->string (remainder (quotient rgb 256) 256)) ";"
          (number->string (remainder rgb 256))))
      (let*-optionals more
        ((bold? #f) (uline? #f) (italic? #f) (strike? #f) (bgtrue #f))
        (unless (<= 0 fgtrue 16777215)
          (error "invalid ansi true color" fgtrue))
        (when bgtrue
          (unless (<= 0 bgtrue 16777215)
            (error "invalid ansi true color" bgtrue)))
        (let
          ((fgs (string-append "38;2;" (rgbs fgtrue)))
           (bs (if bold? ";1" ""))
           (us (if uline? ";4" ""))
           (is (if italic? ";3" ""))
           (ss (if strike? ";9" ""))
           (bgs (if bgtrue (string-append ";48;2;" (rgbs bgtrue)) "")))
          (string-append fgs bs us is ss bgs))))

    ; tier3-terminal sgr sequences
    (define (sgr3 fgtrue . more)
      (define (rgbs rgb sep)
        (string-append
          (number->string (remainder (quotient rgb 65536) 256)) sep
          (number->string (remainder (quotient rgb 256) 256)) sep
          (number->string (remainder rgb 256))))
      (define (ulines uline)
        (define utype (if (pair? uline) (car uline) uline))
        (define utrue (if (pair? uline) (cdr uline) #f))
        (define uts
          (case utype
            ((0 off) "24") ; off
            ((1 straight _) "4") ; classic single
            ((2 double =) "21") ; ECMA-48 double
            ((3 curly ~) "4:3") ; Kitty/VTE/mintty/iTerm2
            ((4 dotted :) "4:4") ; VTE >= 0.76 dotted
            ((5 dashed -) "4:5") ; VTE >= 0.76 dashed
            ((#t) "4") ; classic single
            (else (error "invalid sgr3 underline spec" uline))))
        (define utc
          (cond ((not utrue) "")
                ((and (exact-integer? utrue) (<= 0 utrue 16777215))
                 (string-append ";58:2:" (rgbs utrue ":")))
                (else (error "invalid ansi true color" utrue))))
        (string-append ";" uts utc))
      (let*-optionals more
        ((bold? #f) (dim? #f) (uline #f) (italic? #f) (strike? #f)
          (bgtrue #f))
        (unless (<= 0 fgtrue 16777215)
          (error "invalid ansi true color" fgtrue))
        (when bgtrue
          (unless (<= 0 bgtrue 16777215)
            (error "invalid ansi true color" bgtrue)))
        (let
          ((fgs (string-append "38;2;" (rgbs fgtrue ";")))
           (bs (if bold? ";1" ""))
           (ds (if dim? ";2" ""))
           (us (if uline (ulines uline) ""))
           (is (if italic? ";3" ""))
           (ss (if strike? ";9" ""))
           (bgs
            (if bgtrue (string-append ";48;2;" (rgbs bgtrue ";")) "")))
          (string-append fgs bs ds us is ss bgs))))


    ; ANSI SGR bundle allows to specify separate colors for color-limited,
    ; paletted, and truecolor ttys. Rendering of base 16 colors depends on the
    ; current tty's color scheme, but usually does not deviate too far from
    ; their original values, so red may shift, but will still remain red-like.
    ; Escape sequences for each tier can be generated via sgr0 .. sgr3 procedures;
    ; only sgr0 tier is required

    (define make-asb
      (case-lambda
        ((s0) (vector s0 s0 s0 s0))
        ((s0 s1) (vector s0 s1 s1 s1))
        ((s0 s1 s2) (vector s0 s1 s2 s2))
        ((s0 s1 s2 s3) (vector s0 s1 s2 s3))))

    (define asb-ref vector-ref)

    ; rendering sgr bundle on screen depends on the value of (sgr-support-tier)

    (define (asb->sgr-string asb . more)
      (unless (and (vector? asb) (= (vector-length asb) 4))
        (error "invalid ANSI SGR bundle" asb))
      (let*-optionals more ((tier (sgr-support-tier)))
        (case tier
          ((#f) "") ; no ansi support
          ((0 1 2 3)
           (let ((s (asb-ref asb tier)))
             (if s
                 (string-append escape "[" (vector-ref asb tier) "m")
                 "")))
          (else (error "invalid ANSI SGR support tier" tier)))))

    ; basic sgr bundles

    (define default (make-asb #f #f #f #f)) ; default
    (define default-bold (make-asb "1" "1" "1" "1"))
    (define default-dim (make-asb "2" "2" "2" "2"))
    (define black
      (make-asb (sgr0 0) (sgr1 0) (sgr2 0) (sgr3 0))) ; pitch black
    (define white
      (make-asb (sgr0 15) (sgr1 15) (sgr2 16777215) (sgr3 16777215))) ; blinding white

    (define make-asb-palette
      (case-lambda
        ; LispKit: the sample implementation accepts 9 bundles here and returns
        ; a vector of 17 bundles; the SRFI specifies 8 bundles which get copied
        ; over to the rightmost 8 slots
        ((b0 b1 b2 b3 b4 b5 b6 b7)
         (vector b0 b1 b2 b3 b4 b5 b6 b7 b0 b1 b2 b3 b4 b5 b6 b7))
        ((b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15)
         (vector b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15))))

    (define asb-palette-ref vector-ref)

    ; default bundles and palettes

    (define default-asb-palette
      (make-asb-palette
        (make-asb (sgr0 0) (sgr1 237) (sgr2 3883602) (sgr3 3883602))
        (make-asb (sgr0 1) (sgr1 131) (sgr2 12542314) (sgr3 12542314))
        (make-asb (sgr0 2) (sgr1 144) (sgr2 10731148) (sgr3 10731148))
        (make-asb (sgr0 3) (sgr1 186) (sgr2 15453067) (sgr3 15453067))
        (make-asb (sgr0 4) (sgr1 109) (sgr2 8495553) (sgr3 8495553))
        (make-asb (sgr0 5) (sgr1 139) (sgr2 11833005) (sgr3 11833005))
        (make-asb (sgr0 6) (sgr1 110) (sgr2 8962256) (sgr3 8962256))
        (make-asb (sgr0 7) (sgr1 254) (sgr2 15067632) (sgr3 15067632))
        (make-asb (sgr0 8) (sgr1 240) (sgr2 5002858) (sgr3 5002858))
        (make-asb (sgr0 1) (sgr1 131) (sgr2 12542314) (sgr3 12542314))
        (make-asb (sgr0 2) (sgr1 144) (sgr2 10731148) (sgr3 10731148))
        (make-asb (sgr0 3) (sgr1 186) (sgr2 15453067) (sgr3 15453067))
        (make-asb (sgr0 4) (sgr1 109) (sgr2 8495553) (sgr3 8495553))
        (make-asb (sgr0 5) (sgr1 139) (sgr2 11833005) (sgr3 11833005))
        (make-asb (sgr0 14) (sgr1 109) (sgr2 9419963) (sgr3 9419963))
        (make-asb (sgr0 15) (sgr1 255) (sgr2 15527924)
          (sgr3 15527924))))

    ; we use palettes as semantic colors -- but mappings vary

    (define (default-sc->asb sc bpal)
      (case sc
        ((comment) (asb-palette-ref bpal 8))
        ((char) (asb-palette-ref bpal 10))
        ((string) (asb-palette-ref bpal 10))
        ((escape) (asb-palette-ref bpal 11))
        ((char-escape) (asb-palette-ref bpal 11))
        ((string-escape) (asb-palette-ref bpal 11))
        ((char-meta) (asb-palette-ref bpal 2))
        ((string-meta) (asb-palette-ref bpal 2))
        ((symbol-escape) (asb-palette-ref bpal 11))
        ((formal) (asb-palette-ref bpal 7))
        ((variable) default)
        ((defined) default-bold)
        ((keyword) (asb-palette-ref bpal 12))
        ((number) (asb-palette-ref bpal 13))
        ((literal) (asb-palette-ref bpal 14))
        ((literal-meta) (asb-palette-ref bpal 6))
        ((meta) (asb-palette-ref bpal 6))
        ((directive) (asb-palette-ref bpal 13))
        ((paren) (asb-palette-ref bpal 7))
        ((bracket) default)
        ((warning) (asb-palette-ref bpal 1))
        (else default)))


    ; mapper with a pallete combined into a single function

    (define semantic-color-mapper? procedure?)

    (define make-semantic-color-mapper
      (case-lambda
        ((bpal) (make-semantic-color-mapper bpal default-sc->asb))
        ((bpal sc->asb) (lambda (sc start?) (sc->asb sc bpal)))))

    (define default-semantic-color-mapper
      (make-semantic-color-mapper default-asb-palette
        default-sc->asb))

    (define (semantic-color->start-string sc . more)
      (let*-optionals more
        ((cm default-semantic-color-mapper) (tier (sgr-support-tier)))
        (let ((cmr (cm sc #t)))
          (cond ((string? cmr) cmr)
                ((vector? cmr) (asb->sgr-string cmr tier))
                (else "")))))

    (define (semantic-color->end-string sc . more)
      (let*-optionals more
        ((cm default-semantic-color-mapper) (tier (sgr-support-tier)))
        (let ((cmr (cm sc #f)))
          (cond ((string? cmr) cmr)
                ((and (vector? cmr) tier) (string-append escape "[0m"))
                (else "")))))))
