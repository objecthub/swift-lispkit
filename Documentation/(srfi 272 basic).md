# SRFI 272: Basic Pretty Printing

Library `(srfi 272 basic)` provides all the functionality of library `(srfi 272)`, adding parameter objects for controlling the right margin and the printing of shared and circular structures, as well as procedures `pprint`, `pprint-shared`, and `pprint-simple` which behave like `write`, `write-shared`, and `write-simple` with respect to shared and circular structures.

Datum labels `#N=` and `#N#` are used for representing shared and circular structures. In LispKit, only mutable objects such as vectors, strings, boxes, mutable pairs, and growable vectors get labeled. Pairs are values without identity in LispKit and are therefore never labeled.

## Procedures

**(pp _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pp _obj port_)**  

Writes a pretty-printed representation of _obj_ to _port_, which defaults to the value of `(current-output-port)`. The output is always followed by a newline. The right margin is determined by parameter object `pp-width`. Shared and circular structures are printed according to parameter objects `pp-graph` and `pp-circle`.

```scheme
(import (srfi 272 basic))
(parameterize ((pp-width 30))
  (pp '(define (fold kons knil ls)
         (let lp ((ls ls) (acc knil))
           (if (null? ls) acc (lp (cdr ls) (kons (car ls) acc)))))))
⇒
(define
 (fold kons knil ls)
 (let
  lp
  ((ls ls) (acc knil))
  (if
   (null? ls)
   acc
   (lp
    (cdr ls)
    (kons (car ls) acc)))))
```

**(pprint _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pprint _obj port_)**  

Pretty prints _obj_ like `pp`, using datum labels only for circular references, like `write`. This is equivalent to calling `pp` with `pp-graph` set to `#f` and `pp-circle` set to `#t`. The current values of `pp-graph` and `pp-circle` are neither used nor affected.

```scheme
(define v (vector 1 2 3))
(vector-set! v 1 v)
(pprint v)  ⇒  #0=#(1 #0# 3)
```

**(pprint-shared _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pprint-shared _obj port_)**  

Pretty prints _obj_ like `pp`, using datum labels for all shared and circular references, like `write-shared`. This is equivalent to calling `pp` with `pp-graph` set to `#t`. The current values of `pp-graph` and `pp-circle` are neither used nor affected. This procedure is usually faster than `pprint`.

```scheme
(define s (string #\a #\b))
(pprint-shared (list s s))  ⇒  (#0="ab" #0#)
(pprint (list s s))         ⇒  ("ab" "ab")
(define l (list 1 2))
(pprint-shared (list l l))  ⇒  ((1 2) (1 2))
```

**(pprint-simple _obj_)** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[procedure]</span>  
**(pprint-simple _obj port_)**  

Pretty prints _obj_ like `pp`, without using datum labels, like `write-simple`. This is equivalent to calling `pp` with `pp-graph` and `pp-circle` set to `#f`. The current values of `pp-graph` and `pp-circle` are neither used nor affected. This procedure does not terminate for circular structures, but it is the fastest of the three printing procedures.

## Parameter objects

**pp-width** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

The soft right margin of the pretty printer, measured in columns. `pp-width` must be set to an exact positive integer. The default is 80.

**pp-graph** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

If set to a true value, `pp` uses datum labels to represent both shared and circular references. The default is `#f`.

**pp-circle** &nbsp;&nbsp;&nbsp; <span style="float:right;text-align:rigth;">[parameter object]</span>  

This parameter is ignored if `pp-graph` is true. If `pp-graph` is `#f` and `pp-circle` is true, `pp` uses datum labels to represent circular references only. If both `pp-graph` and `pp-circle` are set to `#f`, `pp` does not terminate when printing circular structures. The default is `#t`.
