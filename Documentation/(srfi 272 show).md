# SRFI 272: Pretty Printing with SRFI 166

Library `(srfi 272 show)` integrates the pretty printer of library [`(srfi 272 advanced)`](<(srfi 272 advanced).md>) with the monadic formatting framework of [SRFI 166](https://srfi.schemers.org/srfi-166/srfi-166.html). It provides two procedures which return SRFI 166 formatters. Library `(srfi 272 show)` also re-exports all parameter objects of library `(srfi 272 advanced)` whose name starts with `pp-`, such that they can be used as keys of keyword arguments: `pp-width`, `pp-circle`, `pp-graph`, `pp-radix`, `pp-length`, `pp-level`, `pp-lines`, `pp-level-stub`, `pp-length-stub`, `pp-lines-stub`, `pp-indent`, `pp-tab`, `pp-max-tab`, `pp-miser-width`, `pp-inline-width`, `pp-brackets`, `pp-code`, `pp-pretty`, `pp-newline`, `pp-color`, `pp-emit`, `pp-tint`, `pp-decorate`, `pp-styles`, and `pp-hooks`.

**(pprinted _obj_ [_key value_] ...)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

Returns a SRFI 166 formatter which pretty prints _obj_ using the pretty printer of library `(srfi 272 advanced)`. Like `pp`, `pprinted` accepts keyword arguments consisting of alternating parameter objects and values. The right margin is determined by the SRFI 166 state variable `width`, which overrides any explicitly or implicitly specified value of `pp-width`.

```scheme
(import (srfi 166) (srfi 272 show))
(show #f "result = " (pprinted '(define (sq x) (* x x)) pp-newline #f) nl)
⇒ "result = (define (sq x) (* x x))\n"
```

The following example, which is borrowed from the SRFI 272 specification, shows how `pprinted` can be combined with other formatters:

```scheme
(define func
  '(define (fold kons knil ls)
     (let lp ((ls ls) (acc knil))
       (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc))))))
(show #t (columnar 37 (pprinted func pp-brackets #t)
                   " ; "
                   (joined displayed
                           '("The fundamental list iterator."
                             "Applies KONS to each element"
                             "of LS and the result of the"
                             "previous application.")
                           nl)))
⇒
(define (fold kons knil ls)           ; The fundamental list iterator.
  (let lp ([ls ls] [acc knil])        ; Applies KONS to each element
    (if (null? ls)                    ; of LS and the result of the
        acc                           ; previous application.
        (lp (cdr ls)                  ;
            (kons (car ls) acc)))))   ;
```

**(pprinted* _obj_ [_key value_] ... _kv-list_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  

This variant of `pprinted` accepts keyword arguments followed by a final argument _kv-list_ which is a list of alternating keys and values. This final list is spliced into the call, as if its contents were passed to `pprinted` via `apply`.
