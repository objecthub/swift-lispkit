;;; COVID-19 statistics per country
;;;
;;; This is a suite of tools:
;;;   - to download COVID-19 metrics from the GitHub respository maintained by Johns
;;;     Hopkins University,
;;;   - to convert them into a format stored on local disk,
;;;   - to upload the data into a SQLite database,
;;;   - to query the database for per-country statistics, and
;;;   - to illustrate the results by saving various graphs in a PDF file.
;;;
;;; This example code makes use of a whole range of LispKit libraries: `(lispkit gvector)`,
;;; `(lispkit date-time)`, `(lispkit csv)`, `(lispkit sqlite)`, and `(lispkit draw)`.
;;;
;;; Usage:
;;;   1. Create a new empty directory
;;;      (make-directory "~/Desktop/covid")
;;;   2. Download all data from GitHub. This function does not expect an empty directory.
;;;      It will complete the missing data and not touch existing files.
;;;      (download-daily-metrics "~/Desktop/covid")
;;;   3. Read the metric files in the given directory and upload the data into a new SQLite
;;;      database.
;;;      (create-db "~/Desktop/covid.sqlite3" "~/Desktop/covid")
;;;   4. Query the database to obtain a report for the given country.
;;;      (define covid-start (date-time 2020 02 25))
;;;      (define covid-ch (query-country-metrics "~/Desktop/covid.sqlite3" "CH" covid-start))
;;;      (define covid-de (query-country-metrics "~/Desktop/covid.sqlite3" "DE" covid-start))
;;;      (define covid-us (query-country-metrics "~/Desktop/covid.sqlite3" "US" covid-start))
;;;   5. Generate a country-specific report as a PDF.
;;;      (save-country-report "~/Desktop/covid-ch-report.pdf" covid-ch "CH" 3 12 1000 100)
;;;      (save-country-report "~/Desktop/covid-de-report.pdf" covid-de "DE" 3 12 10000 500)
;;;      (save-country-report "~/Desktop/covid-us-report.pdf" covid-us "US" 3 12 100000 2500)
;;;
;;; LispKit comes with all the assets needed for this example code, in case an internet connection
;;; is not available. The directory containing the metric files can be found at
;;; `internal-covid-dir`, the sqlite3 database containing the processed data is located at
;;; `internal-covid-db`.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2020 Matthias Zenger. All rights reserved.
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

(import (lispkit gvector)
        (lispkit date-time)
        (lispkit csv)
        (lispkit sqlite)
        (lispkit draw))

;; Pre-computed SQLite database file path

(define internal-covid-db (asset-file-path "Covid" "sqlite3" "Datasets/Covid"))
(define internal-covid-dir (parent-file-path internal-covid-db))

;; URL of directory in GitHub repository from which data is being downloaded

(define daily-report-base
  (string-append "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/"
                 "master/csse_covid_19_data/csse_covid_19_daily_reports/"))

;; Returns the URL containing the data for the given date.

(define (daily-report-url date)
  (string-append daily-report-base (date-time->string date #f "MM-dd-YYYY'.csv'")))

;; Country name synonyms to deal with some noisy data

(define country-name-synonyms
  '(("antigua and barbuda" . "AG") ("bahamas, the" . "BS")
    ("bosnia and herzegovina" . "BA") ("burma" . "MM")
    ("cabo verde" . "CV") ("channel islands" . "GB")
    ("china" . "CN") ("congo (brazzaville)" . "CG")
    ("congo (kinshasa)" . "CD") ("cote d'ivoire" . "CI")
    ("cruise ship" . "??") ("curacao" . "CW")
    ("czech republic" . "CZ") ("diamond princess" . "??")
    ("east timor" . "TL") ("gambia, the" . "GM")
    ("holy see" . "VA") ("hong kong sar" . "HK")
    ("iran (islamic republic of)" . "IR") ("ivory coast" . "CI")
    ("korea, south" . "KR") ("macao sar" . "MO")
    ("macau" . "MO") ("mainland china" . "CN")
    ("ms zaandam" . "??") ("north ireland" . "GB")
    ("occupied palestinian territory" . "PS") ("others" . "??")
    ("palestine" . "PS") ("republic of ireland" . "IE")
    ("republic of korea" . "KR") ("republic of moldova" . "MD")
    ("republic of the congo" . "CG") ("reunion" . "RE")
    ("russian federation" . "RU") ("saint barthelemy" . "BL")
    ("saint kitts and nevis" . "KN") ("saint lucia" . "LC")
    ("saint martin" . "MF") ("saint vincent and the grenadines" . "VC")
    ("sao tome and principe" . "ST") ("taipei and environs" . "TW")
    ("taiwan*" . "TW") ("the bahamas" . "BS")
    ("the gambia" . "GM") ("trinidad and tobago" . "TT")
    ("uk" . "GB") ("us" . "US")
    ("viet nam" . "VN") ("west bank and gaza" . "PS")))

;; Create a mapping from country name strings to ISO country code strings

(define country-iso-codes
  (do ((map (alist->equal-hashtable country-name-synonyms))
       (codes (available-regions) (cdr codes)))
      ((null? codes) map)
    (hashtable-set! map (string-downcase (region-name (car codes) 'en_US)) (car codes))))

;; Returns the ISO country code for the given country name

(define (normalize-country name)
  (let ((res (hashtable-ref country-iso-codes (string-downcase (string-trim name)) #f)))
    (if res res (error "unknown country $0" name))))

;; Template for file names of data files written to local disk

(define filename-template "yyyy-MM-dd'.data'")

;; Returns the file path for the file on local disk that contains the data for the given
;; date. `base` is the directory path where the data files are being stored.

(define (daily-report-path date base)
  (file-path (date-time->string date #f filename-template) base))

;; Factory function used by library `(lispkit csv)` to create an entry for a row in a CSV file.

(define (make-record columns fields)
  (let* ((res (make-alist-record columns fields))
         (backfill (lambda (col default)
                     (if (not (assv col res)) (set! res (cons (cons col default) res))))))
    (backfill 'confirmed 0)
    (backfill 'deaths 0)
    (backfill 'recovered 0)
    (backfill 'active (- (cdr (assv 'confirmed res))
                         (cdr (assv 'deaths res))
                         (cdr (assv 'recovered res))))
    res))

;; Mapping function used by library `(lispkit csv)` to create a

(define (map-key str)
  (case (string->symbol (string-downcase (string-trim str)))
    ((admin admin2)                  (assoc-proc 'admin string-trim))
    ((province_state province/state) (assoc-proc 'region string-trim))
    ((country_region country/region) (assoc-proc 'country normalize-country))
    ((confirmed)                     (assoc-proc 'confirmed string->number))
    ((deaths)                        (assoc-proc 'deaths string->number))
    ((recovered)                     (assoc-proc 'recovered string->number))
    ; Don't read active counts; they are sometimes quite off
    ; ((active)                      (assoc-proc 'active string->number))
    (else                            #f)))

(define (assoc-proc key conv)
  (lambda (x)
    (if (string-empty? (string-trim x)) #f (cons key (conv x)))))

;; Fetch the daily metrics for the given date from GitHub.

(define (fetch-daily-metrics date)
  (try-call-with-input-url (daily-report-url date)
    (lambda (port) (csv-read-records (make-csv-port port) map-key make-record))
    (lambda () #f)))

;; Write the daily metrics into the file at `path`.

(define (write-daily-metrics metrics path)
  (if metrics
      (call-with-output-file path (lambda (port) (write metrics port)))))

;; Download the daily metrics from GitHub, transform the data into a Scheme data structure,
;; and write this data structure to a file within directory `dir`.

(define (download-daily-metrics dir . args)
  (assert (directory-exists? dir))
  (let-optionals args ((start (date-time 'UTC 2020 01 22 13 00))
                       (end (date-time 'UTC)))
    (display (date-time->string start #f "'Updating data between 'yyyy-MM-dd"))
    (display (date-time->string end #f "' and 'yyyy-MM-dd':'"))
    (newline)
    (do ((date start (date-time-add date 1)))
        ((date-time>=? date end))
      (let ((path (daily-report-path date dir)))
        (if (not (file-exists? path))
            (begin (display (date-time->string date #f "'- fetching data for 'yyyy-MM-dd"))
                   (newline)
                   (write-daily-metrics (fetch-daily-metrics date) path)))))))

;; SQL statement to create a `CovidLog` table.

(define create-table-stmt
  (string-append
    "CREATE TABLE IF NOT EXISTS CovidLog ("
    "  Date char(10) NOT NULL,"
    "  Country char(2),"
    "  Region char(64),"
    "  Admin char(64),"
    "  Confirmed integer NOT NULL,"
    "  Deaths integer NOT NULL,"
    "  Recovered integer NOT NULL,"
    "  Active integer NOT NULL"
    ");"))

;; SQL statement to insert an entry into the `CovidLog` table.

(define insert-stmt "INSERT INTO CovidLog VALUES (?, ?, ?, ?, ?, ?, ?, ?);")

;; Date formatting function.

(define (date-string date)
  (date-time->string date #f "yyyy-MM-dd"))

;; Creates a new SQLite database at file path `dbpath`, loads metrics from the data files
;; contained in directory `dir`, and inserts the data into the new database.

(define (create-db dbpath dir)
  (assert (directory-exists? dir))
  (if (file-exists? dbpath)
      (delete-file dbpath))
  (let ((db (open-database dbpath)))
    (unwind-protect
      (begin
        (display "Created database at ")
        (display dbpath)
        (newline)
        (assert (process-statement (prepare-statement db create-table-stmt)))
        (display "Created table `CovidLog`")
        (newline)
        (let* ((stmt2 (prepare-statement db insert-stmt))
               (bind (lambda (i x) (bind-parameter stmt2 i (if x (cdr x) '())))))
          (do ((files (sort string<? (directory-list dir)) (cdr files)))
              ((null? files))
            (let ((date (string->date-time (car files) #f #f filename-template)))
              (if date
                  (let ((datestr (date-string date))
                        (content (call-with-input-file (file-path (car files) dir) read)))
                    (if (vector? content)
                        (begin
                          (display "- loading data from ")
                          (display (car files))
                          (newline)
                          (process-statement (prepare-statement db "BEGIN TRANSACTION;"))
                          (vector-for-each
                            (lambda (entry)
                              (bind 1 (cons 'date datestr))
                              (bind 2 (assv 'country entry))
                              (bind 3 (assv 'region entry))
                              (bind 4 (assv 'admin entry))
                              (bind 5 (assv 'confirmed entry))
                              (bind 6 (assv 'deaths entry))
                              (bind 7 (assv 'recovered entry))
                              (bind 8 (assv 'active entry))
                              (assert (process-statement stmt2)))
                            content)
                          (process-statement (prepare-statement db "COMMIT;"))))))))))
      (close-database db))))

;; SQL select statement for querying the `CovidLog` table for a given country and date range.
;; The resulting table provides the following columns:
;;   - Date
;;   - Total confirmed cases (cumulative)
;;   - Daily new confirmed cases
;;   - Total deaths (cumulative)
;;   - Daily new deaths
;;   - Total recovered (cumulative)
;;   - Daily new recovered cases
;;   - Current active cases
;;   - New active cases

(define country-report-stmt
  (string-append
    "SELECT Date,"
    "       SUM(Confirmed) TotalConfirmed,\n"
    "       (SUM(Confirmed) -\n"
    "        ifnull((SELECT SUM(Confirmed)\n"
    "                FROM CovidLog p\n"
    "                WHERE p.Country = c.Country AND p.Date = date(c.Date, \"-1 day\")\n"
    "                GROUP BY Date, Country), 0)) NewConfirmed,\n"
    "       SUM(Deaths) TotalDeaths,\n"
    "       (SUM(Deaths) -\n"
    "        ifnull((SELECT SUM(Deaths)\n"
    "                FROM CovidLog p\n"
    "                WHERE p.Country = c.Country AND p.Date = date(c.Date, \"-1 day\")\n"
    "                GROUP BY Date, Country), 0)) NewDeaths,\n"
    "       SUM(Recovered) TotalRecovered,\n"
    "       (SUM(Recovered) -\n"
    "        ifnull((SELECT SUM(Recovered)\n"
    "                FROM CovidLog p\n"
    "                WHERE p.Country = c.Country AND p.Date = date(c.Date, \"-1 day\")\n"
    "                GROUP BY Date, Country), 0)) NewRecovered,\n"
    "       SUM(Active) TotalActive,\n"
    "       (SUM(Active) -\n"
    "        ifnull((SELECT SUM(Active)\n"
    "                FROM CovidLog p\n"
    "                WHERE p.Country = c.Country AND p.Date = date(c.Date, \"-1 day\")\n"
    "                GROUP BY Date, Country), 0)) NewActive\n"
    "FROM CovidLog c\n"
    "WHERE c.Country = ?\n"
    "AND c.Date >= ? AND c.Date <= ?\n"
    "GROUP BY Date, Country\n"
    "ORDER BY Date;"))

;; Queries the database for a given country and date range. The resulting report is returned
;; in form of a growable vector containing a list per day. The lists contain the following
;; 9 elements in this order:
;;   - Date
;;   - Total confirmed cases (cumulative)
;;   - Daily new confirmed cases
;;   - Total deaths (cumulative)
;;   - Daily new deaths
;;   - Total recovered (cumulative)
;;   - Daily new recovered cases
;;   - Current active cases
;;   - New active cases

(define (query-country-metrics dbpath country . args)
  (let-optionals args ((start (date-time 'UTC 2020 01 22))
                       (end (date-time 'UTC)))
    (let ((db (open-database dbpath sqlite-readonly)))
      (unwind-protect
        (let ((stmt (prepare-statement db country-report-stmt)))
          (bind-parameter stmt 1 country)
          (bind-parameter stmt 2 (date-string start))
          (bind-parameter stmt 3 (date-string end))
          (do ((report (make-gvector 0)))
              ((process-statement stmt) report)
            (gvector-add! report (row-values stmt))))
        (close-database db)))))

;; Draws a date label on the x-axis of a graph at point `pt` using `font` as font and rotating
;; the text by `angle` (in radians).

(define (draw-label text font angle pt)
  (let ((size (increase-size (text-size text font) 10 0)))
    (fill (transform-shape
            (glyphs text (point (- (size-width size)) 0) size font)
            (rotate angle (translate (point-x pt) (point-y pt)))))))

;; Draws a number `num` in font `font` at point `pt`. `rightaligned` determines if the number
;; is drawn right or left-aligned.

(define (draw-number num font pt rightaligned)
  (let* ((text (number->string (exact (round num))))
         (size (increase-size (text-size text font) 10 0)))
    (if rightaligned
        (draw-text text (point (- (point-x pt) (size-width size)) (point-y pt)) font)
        (draw-text text (point (point-x pt) (point-y pt)) font))))

;; Draws a legend consisting of up to two metrics `fst` and `snd` (both strings). `fstcol` and
;; `sndcol` are both colors. `font` determines the font used.

(define (draw-legend fst fstcol snd sndcol font)
  (let* ((fstsize (text-size fst font))
         (sndsize (if snd (text-size snd font) (size 0 0)))
         (diam    (* (size-height fstsize) 0.5))
         (border  (rectangle (point 14 14)
                             (size (+ (max (size-width fstsize) (size-width sndsize)) diam 24)
                                   (+ (size-height fstsize) (size-height sndsize) 6))
                              4 4)))
    (set-fill-color (color 0.94 0.94 0.94))
    (fill border)
    (set-fill-color fstcol)
    (fill-ellipse (rect 21 20 diam diam))
    (draw-text fst (point (+ 27 diam) (- 20 (/ diam 2))) font)
    (if snd
        (begin
          (set-fill-color sndcol)
          (fill-ellipse (rect 21 (+ 20 (size-height fstsize)) diam diam))
          (draw-text snd (point (+ 27 diam) (- (+ 20 (size-height fstsize)) (/ diam 2))) font)))
    (set-fill-color black)
    (set-color (color 0.4 0.4 0.4))
    (draw-dashed border '(2 1) 0)
    (set-color black)))

;; Returns a new drawing showing graphs for the given report (a result of calling
;; `query-country-metrics`) in rectangle `rect`. `title` is the title of the graph. `cname`
;; is the country name. `tfont` is the font for the title, `afont` is the font for the
;; x-axis labels. `xstep` is a fixnum determining the interval of showing x-axis labels
;; (dates); e.g. if `xstep` is 3, every third date is shown. `content` is a procedure
;; accepting two arguments: the report and the rectangle. It draws the content
;; (i.e. the metric as a graph).

(define (graph-drawing report rect title cname tfont afont xstep content)
  (let* ((n      (vector-length report))
         (xfac   (/ (rect-width rect) (fx1- n)))
         (ctitle (string-append "Country: " cname))
         (csize  (text-size ctitle tfont)))
    (drawing
      ; Draw title
      (draw-text
        title
        (move-point (rect-point rect) 0 (- 0 (size-height csize) 8))
        (font (font-family-name tfont) (font-size tfont) bold))
      (draw-text
        ctitle
        (move-point (rect-point rect)
                    (- (rect-width rect) (size-width csize))
                    (- -8 (size-height csize)))
        tfont)
      ; Draw bounding box
      (draw-rect rect)
      ; Draw labels and graph
      (transform (translate (rect-x rect) (rect-y rect))
        ; Draw x-axis labels
        (do ((i 0 (+ i xstep))
             (anchor (point 0 (rect-height rect)) (move-point anchor (* xfac xstep) 0)))
            ((>= i n))
          (draw-line (move-point anchor 0 -3) (move-point anchor 0 5))
          (draw-label (car (vector-ref report i)) afont -0.8 (move-point anchor 4 0)))
        ; Draw back graph
        (content report rect)))))

;; Draws a graph for report `report` into rectangular `rect` in color `col` using `afont`
;; as the font for labeling the y-axis and `ystep`/`maxystep` to provide hints on what
;; labels to use. `ystep` determines the unit for which a label is being displayed,
;; `maxystep` determines the maximum amount of labels drawn.
;;
;; As opposed to `draw-bottom-graph`, `draw-top-graph` uses the left y-axis to display
;; labels.

(define (draw-top-graph report selector rect col afont ystep maxystep)
  (let* ((n (vector-length report))
         (xs (iota n))
         (ys (map (lambda (x) (selector (gvector-ref report x))) xs))
         (ymax (apply max ys))
         (xfac (/ (rect-width rect) (fx1- n)))
         (yfac (/ (rect-height rect) ymax))
         (ystep (* (exact (round (/ ymax (* maxystep ystep)))) ystep))
         (ps (map (lambda (x y) (point (* xfac x) (* yfac y))) xs ys))
         (graph (flip-shape (interpolate ps))))
    ; Draw y-axis labels
    (do ((i ystep (+ i ystep))
         (anchor (point 0 (- (rect-height rect) (* yfac ystep)))
                 (move-point anchor 0 (- (* yfac ystep)))))
        ((> i ymax))
      (draw-line (move-point anchor -5 0) (move-point anchor 3 0))
      (draw-number i afont (move-point anchor 0 -8) #t))
    ; Draw flipped interpolation shape
    (set-color col)
    (draw graph 1.5)
    ; Draw graph points
    (set-fill-color yellow)
    (for-each
      (lambda (p)
        (let ((s (flip-shape (circle p 2.5) (shape-bounds graph))))
          (fill s)
          (draw s 1.0)))
      ps)))

;; Draws a graph for report `report` into rectangular `rect` in color `col` using `afont`
;; as the font for labeling the y-axis and `ystep`/`maxystep` to provide hints on what
;; labels to use. `ystep` determines the unit for which a label is being displayed,
;; `maxystep` determines the maximum amount of labels drawn.
;;
;; As opposed to `draw-top-graph`, `draw-bottom-graph` uses the right y-axis to display
;; labels.

(define (draw-bottom-graph report selector rect col afont ystep maxystep)
  (let* ((n (vector-length report))
         (xs (iota n))
         (ys (map (lambda (x) (selector (gvector-ref report x))) xs))
         (ymax (apply max ys))
         (xfac (/ (rect-width rect) (fx1- n)))
         (yfac (/ (rect-height rect) ymax))
         (ystep (* (exact (round (/ ymax (* maxystep ystep)))) ystep))
         (ps (map (lambda (x y) (point (* xfac x) (* yfac y))) xs ys))
         (graph (flip-shape (interpolate ps))))
    ; Draw y-axis labels
    (do ((i ystep (+ i ystep))
         (anchor (point (rect-width rect) (- (rect-height rect) (* yfac ystep)))
                 (move-point anchor 0 (- (* yfac ystep)))))
        ((> i ymax))
      (draw-line (move-point anchor -3 0) (move-point anchor 5 0))
      (draw-number i afont (move-point anchor 8 -8) #f))
    ; Draw flipped interpolation shape
    (set-color col)
    (draw graph 1.5)
    ; Draw graph points
    (set-fill-color yellow)
    (for-each
      (lambda (p)
        (let ((s (flip-shape (circle p 2.5) (shape-bounds graph))))
          (fill s)
          (draw s 1.0)))
      ps)))

;; Creates a drawing for displaying graphs showing a report for country `country`.
;; `daystep` is a fixnum determining the interval of showing x-axis labels (dates); e.g.
;; if `daystep` is 3, every third date is shown. `maxlabels`, `cumunit`, and `newunit`
;; are used to specify what labels are drawn and how many.

(define (country-report-drawing report country daystep maxlabels cumunit newunit)
  (let* ((tfont (font "Times" 16 normal))
         (afont (font "Times" 10 normal))
         (lfont (font "Times" 12 normal italic))
         (cname (region-name country 'en)))
    (drawing
      (draw-drawing
        (graph-drawing
          report (rect 60 40 700 270) "Confirmed COVID-19 cases" cname
          tfont afont daystep
          (lambda (report rect)
            (draw-legend "Cumulative" blue "Daily new" green lfont)
            (draw-bottom-graph report caddr rect green afont newunit maxlabels)
            (draw-top-graph report cadr rect blue afont cumunit maxlabels))))
      (draw-drawing
        (graph-drawing
          report (rect 60 405 700 270) "Active COVID-19 cases" cname
          tfont afont daystep
          (lambda (report rect)
            (draw-legend "Daily active" red #f #f lfont)
            (draw-top-graph report (lambda (xs) (list-ref xs 7))
                            rect red afont cumunit maxlabels)))))))

;; Creates a PDF file displaying graphs showing a report for country `country`.
;; `daystep` is a fixnum determining the interval of showing x-axis labels (dates); e.g.
;; if `daystep` is 3, every third date is shown. `maxlabels`, `cumunit`, and `newunit`
;; are used to specify what labels are drawn and how many.

(define (save-country-report path report country daystep maxlabels cumunit newunit)
  (save-drawing path
                (country-report-drawing report country daystep maxlabels cumunit newunit)
                (size 820 770)))
