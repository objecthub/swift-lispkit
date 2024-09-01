;;; Text table demo
;;; 
;;; This is a short example showcasing how library `(lispkit text-tables)`
;;; works. A new text table is created and populated with data from
;;; countries of the European Union. Finally, the text table is printed
;;; on the console.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2021 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may
;;; not use this file except in compliance with the License. You may obtain
;;; a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(import (lispkit base)
        (lispkit text-table))

;; Make a new text table, consisting of four columns
(define eu (make-text-table
              '(("Country" center left)
                ("Population\n(millions)" center right)
                ("Area\n(km²)" center right)
                ("Density\n(people/km²)" center right))
              double-line-sep
              round-edges))

;; Populate the text table with rows
(add-text-table-row! eu '("Austria" "8.3" "83,858" "99"))
(add-text-table-row! eu '("Belgium" "10.5" "30,510" "344"))
(add-text-table-row! eu '("Bulgaria" "7.7" "110,912" "70"))
(add-text-table-row! eu '("Croatia" "4.3" "56,594" "75.8"))
(add-text-table-row! eu '("Cyprus" "0.8" "9,250" "84"))
(add-text-table-row! eu '("Czech Republic" "10.3" "78,866" "131"))
(add-text-table-row! eu '("Denmark" "5.4" "43,094" "126"))
(add-text-table-row! eu '("Estonia" "1.3" "45,226" "29"))
(add-text-table-row! eu '("Finland" "5.3" "337,030" "16"))
(add-text-table-row! eu '("France" "65.03" "643,548" "111"))
(add-text-table-row! eu '("Germany" "80.4" "357,021" "225"))
(add-text-table-row! eu '("Greece" "11.1" "131,940" "84"))
(add-text-table-row! eu '("Hungary" "10.1" "93,030" "108"))
(add-text-table-row! eu '("Ireland" "4.6" "70,280" "60"))
(add-text-table-row! eu '("Italy" "58.8" "301,320" "195"))
(add-text-table-row! eu '("Latvia" "2.3" "64,589" "35"))
(add-text-table-row! eu '("Lithuania" "3.4" "65,200" "45"))
(add-text-table-row! eu '("Luxembourg" "0.5" "2,586" "181"))
(add-text-table-row! eu '("Malta" "0.5" "316" "1,261"))
(add-text-table-row! eu '("Netherlands" "17" "41,526" "394"))
(add-text-table-row! eu '("Poland" "38.1" "312,685" "122"))
(add-text-table-row! eu '("Portugal" "10.6" "92,931" "114"))
(add-text-table-row! eu '("Romania" "21.6" "238,391" "91"))
(add-text-table-row! eu '("Spain" "44.7" "504,782" "87"))
(add-text-table-row! eu '("Slovakia" "5.4" "48,845" "111"))
(add-text-table-row! eu '("Slovenia" "2.0" "20,253" "99"))
(add-text-table-row! eu '("Sweden" "10" "449,964" "20"))
(add-text-table-separator! eu line-sep)
(add-text-table-row! eu '("European Union" "494.8" "4,422,773" "112"))

;; Display the text table on the console
(display (text-table->string eu))
(newline)
