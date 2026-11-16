;;; SCHEME ILIST
;;;
;;; Immutable list library. This library is part of the Scheme Red edition of the R7RS
;;; large language. It forwards all definitions to SRFI 116.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
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

(define-library (scheme ilist)

  (export iq ipair ilist xipair ipair* make-ilist ilist-tabulate ilist-copy iiota
          ipair? proper-ilist? ilist? dotted-ilist? not-ipair? null-ilist? ilist= icar
          icdr icaar icadr icdar icddr icaaar icaadr icadar icaddr icdaar icdadr icddar
          icdddr icaaaar icaaadr icaadar icaaddr icadaar icadadr icaddar icadddr
          icdaaar icdaadr icdadar icdaddr icddaar icddadr icdddar icddddr ilist-ref
          ifirst isecond ithird ifourth ififth isixth iseventh ieighth ininth itenth
          icar+icdr itake idrop ilist-tail itake-right idrop-right isplit-at ilast
          last-ipair ilength iappend iconcatenate ireverse iappend-reverse izip iunzip1
          iunzip2 iunzip3 iunzip4 iunzip5 icount imap ifor-each ifold iunfold
          ipair-fold ireduce ifold-right iunfold-right ipair-fold-right ireduce-right
          iappend-map ipair-for-each ifilter-map imap-in-order ifilter ipartition
          iremove imember imemq imemv ifind ifind-tail iany ievery ilist-index
          itake-while idrop-while ispan ibreak idelete idelete-duplicates iassoc iassq
          iassv ialist-cons ialist-delete replace-icar replace-icdr pair->ipair
          ipair->pair list->ilist ilist->list tree->itree itree->tree gtree->itree
          gtree->tree iapply ipair-comparator ilist-comparator make-ipair-comparator
          make-ilist-comparator make-improper-ilist-comparator make-icar-comparator
          make-icdr-comparator)

  (import (srfi 116))
)
