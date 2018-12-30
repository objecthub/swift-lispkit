;;; SCHEME RLIST
;;;
;;; Scheme random-access list library. This library is part of the Scheme Red edition of
;;; the R7RS large language.
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

(define-library (scheme rlist)

  (export (rename quote rquote)
          (rename pair? rpair?)
          (rename cons rcons)
          (rename car rcar)
          (rename cdr rcdr)
          (rename caar rcaar)
          (rename cadr rcadr)
          (rename cddr rcddr)
          (rename cdar rcdar)
          (rename caaar rcaaar)
          (rename caadr rcaadr)
          (rename caddr rcaddr)
          (rename cadar rcadar)
          (rename cdaar rcdaar)
          (rename cdadr rcdadr)
          (rename cdddr rcdddr)
          (rename cddar rcddar)
          (rename caaaar rcaaaar)
          (rename caaadr rcaaadr)
          (rename caaddr rcaaddr)
          (rename caadar rcaadar)
          (rename cadaar rcadaar)
          (rename cadadr rcadadr)
          (rename cadddr rcadddr)
          (rename caddar rcaddar)
          (rename cdaaar rcdaaar)
          (rename cdaadr rcdaadr)
          (rename cdaddr rcdaddr)
          (rename cdadar rcdadar)
          (rename cddaar rcddaar)
          (rename cddadr rcddadr)
          (rename cddddr rcddddr)
          (rename cdddar rcdddar)
          (rename null? rnull?)
          (rename list? rlist?)
          (rename list rlist)
          (rename make-list make-rlist)
          (rename length rlength)
          (rename append rappend)
          (rename reverse rreverse)
          (rename list-tail rlist-tail)
          (rename list-ref rlist-ref)
          (rename list-set rlist-set)
          (rename list-ref/update rlist-ref/update)
          (rename map rmap)
          (rename for-each rfor-each)
          (rename random-access-list->linear-access-list rlist->list)
          (rename linear-access-list->random-access-list list->rlist))

  (import (srfi 101))
)
