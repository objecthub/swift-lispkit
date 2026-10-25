;;; Collect movies from a web API in a local SQLite database
;;;
;;; This program retrieves movies from the web API of The Movie Database
;;; (TMDB) and merges them into a local SQLite database. It runs several
;;; queries with different criteria, e.g. all English-language thrillers
;;; released in 2025. The queries overlap, so the same movie is returned
;;; more than once. The merge identifies movies by their TMDB identifier,
;;; adds movies that are new, updates movies whose data changed, and leaves
;;; everything else untouched. Thus, the program can be run repeatedly: each
;;; run merges the current TMDB data into the same database. Finally, the
;;; program queries the database for statistics, such as the number of
;;; movies, the distribution of genres, and the best rated movies.
;;;
;;; Merging: The same movie can be returned by several queries and by several
;;; runs of the program, and its versions can differ. A movie is identified by
;;; its TMDB identifier, so it is stored only once. Descriptive data of a new
;;; version replaces the existing data unless it is missing. For the statistics
;;; of a movie (popularity, rating, and number of votes), the version with more
;;; votes is kept, since votes only accumulate. The genres of a movie are replaced
;;; by the genres of the new version, and a table records which queries found
;;; which movies. The movies of a query are merged in a single transaction, so
;;; an error does not leave the database in an inconsistent state.
;;;
;;; The program combines the following libraries:
;;;   - `(lispkit http)` sends requests to the web API. Requests are sent
;;;     asynchronously, so the pages of a query are fetched concurrently.
;;;   - `(lispkit json)` represents and inspects the responses of the API.
;;;   - `(lispkit sqlite)` creates the database, merges the movies in
;;;     transactions, and answers the queries for the statistics.
;;;   - `(lispkit system keychain)` keeps the credentials for the web API in
;;;     the system keychain, so they do not need to be part of the code.
;;;   - `(lispkit text-table)` formats the statistics as text tables.
;;;
;;; Credentials: TMDB requires an _API read access token_ or an _API key_,
;;; which are available for free in the account settings at themoviedb.org.
;;; The program reads them from the environment variables `TMDB_ACCESS_TOKEN`
;;; and `TMDB_API_KEY`, or else from the keychain. To store the credentials
;;; of the environment variables in the keychain, run the program once with
;;; the argument `store-credentials`. The variables are not needed after that.
;;; With the argument `forget-credentials`, the credentials are removed from
;;; the keychain again. In a REPL that loaded the program, the call
;;; `(store-credentials! <access token> <API key>)` stores the credentials that are
;;; given as strings, and `(main)` runs the program again.
;;;
;;; Usage: Movies.scm [<database> | store-credentials | forget-credentials]
;;; The default database is `~/Documents/Movies.sqlite`.
;;;
;;; This product uses the TMDB API but is not endorsed or certified by TMDB.
;;;
;;; Author: Matthias Zenger
;;; Copyright © 2026 Matthias Zenger. All rights reserved.
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
        (lispkit thread)
        (lispkit thread future)
        (lispkit http)
        (lispkit url)
        (lispkit json)
        (lispkit sqlite)
        (lispkit text-table)
        (lispkit system keychain))

;;; Settings

;; The default location of the database
(define default-database "~/Documents/Movies.sqlite")

;; The maximum number of result pages (20 movies each) fetched for a query
(define max-pages 3)

;; The queries. Each query has a name, a description, and the parameters of a
;; request for TMDB's `discover/movie` endpoint. The parameters select genres
;; (53 = Thriller, 27 = Horror, 9648 = Mystery; `|` means "or"), the original
;; language, the release year, the minimum number of votes, and the sort order.
;; The first three queries overlap on purpose.
(define queries
  '(("english-thrillers"
     "English-language thrillers released in 2025"
     (("with_genres" . "53") ("with_original_language" . "en")
      ("primary_release_year" . "2025") ("sort_by" . "popularity.desc")))
    ("english-horror"
     "English-language horror movies released in 2025"
     (("with_genres" . "27") ("with_original_language" . "en")
      ("primary_release_year" . "2025") ("sort_by" . "popularity.desc")))
    ("mystery-or-thriller"
     "Mystery or thriller movies in any language released in 2025"
     (("with_genres" . "9648|53") ("primary_release_year" . "2025")
      ("sort_by" . "popularity.desc")))
    ("thriller-classics"
     "Thrillers of all times with many votes, best rated first"
     (("with_genres" . "53") ("vote_count.gte" . "5000")
      ("sort_by" . "vote_average.desc")))))

;;; Credentials

;; The keychain service under which the credentials are stored, and for each
;; credential its key, the environment variable, and a label.
(define keychain (make-keychain "TMDB"))

(define credentials
  '(("access-token" "TMDB_ACCESS_TOKEN" "TMDB API read access token")
    ("api-key" "TMDB_API_KEY" "TMDB API key")))

;; Returns the value of a credential: an environment variable overrides the
;; keychain. The result is `#f` if the credential is unknown.
(define (credential key)
  (let ((entry (assoc key credentials)))
    (or (get-environment-variable (cadr entry))
        (keychain-ref-string keychain key))))

;; Stores credentials in the keychain. The optional arguments are the access token
;; and the API key, in the order of `credentials`. If at least one of them is a
;; string, the credentials given as strings are stored. Otherwise, the values of the
;; environment variables are stored.
(define (store-credentials! . args)
  (let-optionals args ((token #f) (key #f))
    (let ((given? (or (string? token) (string? key)))
          (count 0))
      (for-each (lambda (entry arg)
                  (let ((value (if given?
                                   (and (string? arg) arg)
                                   (get-environment-variable (cadr entry)))))
                    (when value
                      (keychain-set-string! keychain (car entry) value (caddr entry))
                      (set! count (+ count 1))
                      (display* "stored " (caddr entry) " in the keychain\n"))))
                credentials
                (list token key))
      (when (= count 0)
        (display* "nothing to store; set TMDB_ACCESS_TOKEN or TMDB_API_KEY\n")))))

;; Removes the credentials from the keychain
(define (forget-credentials!)
  (for-each (lambda (entry)
              (when (keychain-exists? keychain (car entry))
                (keychain-remove! keychain (car entry))
                (display* "removed " (caddr entry) " from the keychain\n")))
            credentials))

;; Returns the credential for authenticating requests as a pair of either `token`
;; or `key` and the credential, or `#f` if there is none. The access token is
;; preferred.
(define (authentication)
  (let ((token (credential "access-token")))
    (if token
        (cons 'token token)
        (let ((key (credential "api-key")))
          (and key (cons 'key key))))))

;;; Accessing the web API

;; The credential used for authenticating requests, as returned by `authentication`.
;; It is bound by `main` while the program runs.
(define tmdb-auth (make-parameter #f))

;; Returns a request for the given path of the web API with the given query
;; parameters. An access token is sent as a bearer token in the HTTP header
;; and an API key as a query parameter.
(define (tmdb-request path params)
  (let ((auth (tmdb-auth))
        (headers '(("Accept" . "application/json"))))
    (if (eq? (car auth) 'token)
        (make-http-request
          (url "https" "api.themoviedb.org" (string-append "/3/" path) params)
          "GET"
          (cons (cons "Authorization" (string-append "Bearer " (cdr auth))) headers))
        (make-http-request
          (url "https" "api.themoviedb.org" (string-append "/3/" path)
               (cons (cons "api_key" (cdr auth)) params))
          "GET"
          headers))))

;; Waits for the response to `request` that is delivered by `future` and returns
;; its body as a JSON value. Requests that were rejected because of the rate limit
;; or because of a problem with the server are repeated twice.
(define (tmdb-await request future)
  (let retry ((response (future-get future)) (attempt 1))
    (let ((status (http-response-status-code response)))
      (cond ((= status 200)
             (bytevector->json (http-response-content response)))
            ((and (< attempt 3) (or (= status 429) (>= status 500)))
             (thread-sleep! attempt)
             (retry (future-get (http-session-send request)) (+ attempt 1)))
            (else
             (error "TMDB request failed with status $0 ($1)"
                    status
                    (http-status-code->string status)))))))

;; Sends a request and waits for the response
(define (tmdb-get path params)
  (let ((request (tmdb-request path params)))
    (tmdb-await request (http-session-send request))))

;; Returns the genres of movies as an association list from identifiers to names
(define (tmdb-genres)
  (map (lambda (genre) (cons (json-ref/value genre 'id) (json-ref/value genre 'name)))
       (json-children (json-ref (tmdb-get "genre/movie/list" '(("language" . "en"))) 'genres))))

;; Runs a query and returns the movies of its first `max-pages` result pages as
;; a list of JSON objects. The first page tells how many pages there are. The
;; other pages are requested all at once and arrive concurrently.
(define (tmdb-discover params)
  (define (page-params n)
    (append params `(("include_adult" . "false") ("language" . "en-US")
                     ("page" . ,(number->string n)))))
  (let* ((page1 (tmdb-get "discover/movie" (page-params 1)))
         (last-page (min max-pages (json-ref/value page1 'total_pages)))
         (requests (map (lambda (n) (tmdb-request "discover/movie" (page-params n)))
                        (iota (max 0 (- last-page 1)) 2)))
         (futures (map http-session-send requests))
         (pages (cons page1 (map tmdb-await requests futures))))
    (append-map (lambda (page) (json-children (json-ref page 'results))) pages)))

;; Returns the Scheme value of member `key` of JSON object `obj`; JSON `null`
;; and missing members are represented by `()`, which is also SQL's NULL.
(define (json-ref/value obj key)
  (let ((value (json-ref obj key)))
    (if (or (not value) (json-null? value)) '() (json->value value))))

;;; Managing the database

;; Combines the lines of an SQL statement. Since string literals cannot span
;; several lines, statements are written as a sequence of strings.
(define (sql . lines)
  (apply string-append (map (lambda (line) (string-append line " ")) lines)))

;; The tables of the database. Genres and movies are in a many-to-many relation.
;; The table `movie_queries` records which queries found which movies.
(define schema
  (list
    (sql "CREATE TABLE IF NOT EXISTS genres ("
         "  id INTEGER PRIMARY KEY,"
         "  name TEXT NOT NULL)")
    (sql "CREATE TABLE IF NOT EXISTS movies ("
         "  id INTEGER PRIMARY KEY,"
         "  title TEXT NOT NULL,"
         "  original_title TEXT,"
         "  language TEXT,"
         "  release_date TEXT,"
         "  overview TEXT,"
         "  popularity REAL,"
         "  vote_average REAL,"
         "  vote_count INTEGER,"
         "  added TEXT NOT NULL,"
         "  updated TEXT NOT NULL)")
    (sql "CREATE TABLE IF NOT EXISTS movie_genres ("
         "  movie_id INTEGER NOT NULL REFERENCES movies(id) ON DELETE CASCADE,"
         "  genre_id INTEGER NOT NULL REFERENCES genres(id),"
         "  PRIMARY KEY (movie_id, genre_id))")
    (sql "CREATE TABLE IF NOT EXISTS movie_queries ("
         "  movie_id INTEGER NOT NULL REFERENCES movies(id) ON DELETE CASCADE,"
         "  query TEXT NOT NULL,"
         "  PRIMARY KEY (movie_id, query))")))

;; Executes an SQL statement with the given parameters, ignoring its results
(define (execute! db sql . args)
  (let ((stmt (prepare-statement db sql)))
    (bind-parameters stmt args)
    (process-statement stmt)
    (void)))

;; Executes an SQL query with the given parameters and returns its results as a
;; list of rows, each of which is a list of column values
(define (query db sql . args)
  (let ((stmt (prepare-statement db sql)))
    (bind-parameters stmt args)
    (let loop ((rows '()))
      (if (process-statement stmt)
          (reverse rows)
          (loop (cons (row-values stmt) rows))))))

;; Returns the first row of the results of an SQL query, or `#f` if there is none
(define (query-row db sql . args)
  (let ((rows (apply query db sql args)))
    (and (pair? rows) (car rows))))

;; Returns the first column of the first row of the results of an SQL query
(define (query-value db sql . args)
  (car (apply query-row db sql args)))

;; Runs `proc` in a transaction. Either all changes of `proc` are written to the
;; database, or none of them if `proc` raises an error.
(define (call-with-transaction db proc)
  (execute! db "BEGIN")
  (guard (condition (#t (execute! db "ROLLBACK") (raise condition)))
    (let ((result (proc)))
      (execute! db "COMMIT")
      result)))

;; Opens the database at `path`, creating it with all tables if necessary
(define (open-movie-database path)
  (let ((db (open-database path)))
    (execute! db "PRAGMA foreign_keys = ON")
    (for-each (lambda (sql) (execute! db sql)) schema)
    db))

;;; Merging data into the database

;; Merges genres, given as an association list from identifiers to names. This is
;; a plain "upsert": a genre is inserted, or the name of an existing genre is updated.
(define (merge-genres! db genres)
  (call-with-transaction db
    (thunk
      (for-each (lambda (genre)
                  (execute! db
                    (sql "INSERT INTO genres (id, name) VALUES (?, ?)"
                         "ON CONFLICT(id) DO UPDATE SET name = excluded.name")
                    (car genre) (cdr genre)))
                genres))))

;; Returns the values of a movie for the columns `title`, `original_title`,
;; `language`, `release_date`, `overview`, `popularity`, `vote_average`, and
;; `vote_count`. Real numbers are always inexact.
(define (movie-values movie)
  (define (real key)
    (let ((x (json-ref/value movie key))) (if (number? x) (inexact x) x)))
  (list (json-ref/value movie 'title)
        (json-ref/value movie 'original_title)
        (json-ref/value movie 'original_language)
        (json-ref/value movie 'release_date)
        (json-ref/value movie 'overview)
        (real 'popularity)
        (real 'vote_average)
        (json-ref/value movie 'vote_count)))

;; Conflict resolution. If a movie is already in the database, its data is merged
;; with the data that was just retrieved. Descriptive columns (title, language,
;; release date, ...) take the new value, unless it is missing: a gap in the data
;; returned by the web API must not erase what is known already. The statistics
;; (popularity, rating, and number of votes) of two versions of a movie can differ,
;; because TMDB serves each query from a cache that was filled at a different time.
;; Votes only accumulate, so the version with more votes is the more recent one.
(define column-kinds '(text text text text text statistics statistics statistics))

;; Returns the merged values of a movie, given the new values and the values that
;; are in the database. Both are lists as returned by `movie-values`.
(define (resolve new old)
  (let* ((new-votes (list-ref new 7))
         (old-votes (list-ref old 7))
         (newer? (cond ((null? new-votes) #f)
                       ((null? old-votes) #t)
                       (else (>= new-votes old-votes)))))
    (map (lambda (kind n o)
           (if (eq? kind 'text)
               (if (or (null? n) (equal? n "")) o n)
               (if newer? n o)))
         column-kinds new old)))

;; Replaces the genres of a movie
(define (set-genres! db id genres)
  (execute! db "DELETE FROM movie_genres WHERE movie_id = ?" id)
  (for-each (lambda (genre)
              (execute! db "INSERT INTO movie_genres (movie_id, genre_id) VALUES (?, ?)" id genre))
            genres))

;; Merges a movie, given as a JSON object, into the database and records that
;; query `name` found it. The movie is identified by its TMDB identifier. The
;; result is `added` if the movie was not in the database, `updated` if the
;; database changed, and `unchanged` if the database already knew everything.
(define (merge-movie! db name movie)
  (let* ((id (json-ref/value movie 'id))
         (new (movie-values movie))
         (genres (sort < (vector->list (json-ref/value movie 'genre_ids))))
         (old (query-row db
                (sql "SELECT title, original_title, language, release_date, overview,"
                     "       popularity, vote_average, vote_count"
                     "FROM movies WHERE id = ?")
                id))
         (result
           (cond ((not old)
                  (apply execute! db
                    (sql "INSERT INTO movies (title, original_title, language, release_date,"
                         "                    overview, popularity, vote_average, vote_count,"
                         "                    id, added, updated)"
                         "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)")
                    (append new (list id)))
                  (set-genres! db id genres)
                  'added)
                 (else
                  (let ((merged (resolve new old))
                        (known (map car (query db
                                          (sql "SELECT genre_id FROM movie_genres"
                                               "WHERE movie_id = ? ORDER BY genre_id")
                                          id))))
                    (cond ((and (equal? merged old)
                                (or (null? genres) (equal? genres known)))
                           'unchanged)
                          (else
                           (apply execute! db
                             (sql "UPDATE movies SET title = ?, original_title = ?, language = ?,"
                                  "                  release_date = ?, overview = ?, popularity = ?,"
                                  "                  vote_average = ?, vote_count = ?,"
                                  "                  updated = CURRENT_TIMESTAMP"
                                  "WHERE id = ?")
                             (append merged (list id)))
                           (unless (null? genres)
                             (set-genres! db id genres))
                           'updated)))))))
    (execute! db "INSERT OR IGNORE INTO movie_queries (movie_id, query) VALUES (?, ?)" id name)
    result))

;; Runs a query against the web API and merges all movies it returns into the
;; database within one transaction. Returns a row for the summary table with the
;; name of the query, the number of movies returned, and how many of them were
;; added, updated, and unchanged.
(define (run-query! db spec)
  (let* ((name (car spec))
         (movies (tmdb-discover (caddr spec)))
         (results (call-with-transaction db
                    (thunk (map (lambda (movie) (merge-movie! db name movie)) movies)))))
    (define (tally kind) (length (filter (lambda (r) (eq? r kind)) results)))
    (list name (length movies) (tally 'added) (tally 'updated) (tally 'unchanged))))

;;; Statistics

;; Returns the text for a table cell
(define (text x)
  (cond ((string? x) x)
        ((null? x) "-")
        (else (number->string x))))

;; Appends to each row a bar whose length is proportional to the number in the
;; second column of the row
(define (with-bars rows)
  (let ((peak (apply max 1 (map cadr rows))))
    (map (lambda (row)
           (append row (list (make-string (exact (round (/ (* (cadr row) 30) peak))) #\█))))
         rows)))

;; Prints a table with a title. `headers` are the names of the columns and `rows`
;; are lists of values. Columns with numbers are aligned to the right.
(define (print-table title headers rows)
  (let ((table (make-text-table
                 (map (lambda (header i)
                        (list header 'center
                              (if (and (pair? rows) (number? (list-ref (car rows) i))) 'right 'left)))
                      headers
                      (iota (length headers))))))
    (for-each (lambda (row) (add-text-table-row! table (map text row))) rows)
    (display* "\n" title "\n" (text-table->string table) "\n")))

;; Prints statistics about the movies in the database
(define (print-statistics db)
  (print-table "Overview"
    '("Statistic" "Value")
    (list (list "Movies"
                (query-value db "SELECT COUNT(*) FROM movies"))
          (list "Genres in use"
                (query-value db "SELECT COUNT(DISTINCT genre_id) FROM movie_genres"))
          (list "Languages"
                (query-value db "SELECT COUNT(DISTINCT language) FROM movies"))
          (list "Earliest release"
                (query-value db "SELECT MIN(release_date) FROM movies"))
          (list "Latest release"
                (query-value db "SELECT MAX(release_date) FROM movies"))
          (list "Average rating (50+ votes)"
                (query-value db (sql "SELECT ROUND(AVG(vote_average), 2) FROM movies"
                                     "WHERE vote_count >= 50")))
          (list "Thrillers"
                (query-value db (sql "SELECT COUNT(*) FROM movie_genres mg"
                                     "JOIN genres g ON g.id = mg.genre_id"
                                     "WHERE g.name = 'Thriller'")))
          (list "Found by more than one query"
                (query-value db (sql "SELECT COUNT(*) FROM"
                                     "  (SELECT movie_id FROM movie_queries"
                                     "   GROUP BY movie_id HAVING COUNT(*) > 1)")))))
  (print-table "Movies per query"
    '("Query" "Movies" "Found only by this query")
    (query db (sql "SELECT query, COUNT(*),"
                   "       SUM((SELECT COUNT(*) FROM movie_queries other"
                   "            WHERE other.movie_id = this.movie_id) = 1)"
                   "FROM movie_queries this"
                   "GROUP BY query ORDER BY COUNT(*) DESC, query")))
  (print-table "Movies per genre (a movie can have several genres)"
    '("Genre" "Movies" "Share %" "Avg. rating" "")
    (with-bars
      (query db (sql "SELECT g.name, COUNT(*),"
                     "       ROUND(100.0 * COUNT(*) / (SELECT COUNT(*) FROM movies), 1),"
                     "       ROUND(AVG(CASE WHEN m.vote_count >= 50 THEN m.vote_average END), 2)"
                     "FROM movie_genres mg"
                     "JOIN genres g ON g.id = mg.genre_id"
                     "JOIN movies m ON m.id = mg.movie_id"
                     "GROUP BY g.id ORDER BY COUNT(*) DESC, g.name"))))
  (print-table "Most frequent combinations of genres"
    '("Genres" "Movies" "")
    (with-bars
      (query db (sql "SELECT a.name || ' + ' || b.name, COUNT(*)"
                     "FROM movie_genres x"
                     "JOIN movie_genres y ON x.movie_id = y.movie_id AND x.genre_id < y.genre_id"
                     "JOIN genres a ON a.id = x.genre_id"
                     "JOIN genres b ON b.id = y.genre_id"
                     "GROUP BY x.genre_id, y.genre_id ORDER BY COUNT(*) DESC, 1 LIMIT 5"))))
  (print-table "Movies per language (top 5)"
    '("Language" "Movies" "")
    (with-bars
      (query db (sql "SELECT language, COUNT(*) FROM movies"
                     "GROUP BY language ORDER BY COUNT(*) DESC, language LIMIT 5"))))
  (print-table "Movies per decade of the release"
    '("Decade" "Movies" "")
    (with-bars
      (query db (sql "SELECT substr(release_date, 1, 3) || '0s', COUNT(*) FROM movies"
                     "WHERE release_date IS NOT NULL GROUP BY 1 ORDER BY 1"))))
  (print-table "Best rated movies (at least 1000 votes)"
    '("Title" "Released" "Rating" "Votes")
    (query db (sql "SELECT title, release_date, ROUND(vote_average, 2), vote_count FROM movies"
                   "WHERE vote_count >= 1000 ORDER BY vote_average DESC, title LIMIT 5"))))

;;; Main program

;; Fetches movies for all queries, merges them into the database at `path`,
;; and prints statistics
(define (collect-movies path)
  (let ((db (open-movie-database path)))
    (display* "Database " path " with "
              (query-value db "SELECT COUNT(*) FROM movies") " movies\n")
    (merge-genres! db (tmdb-genres))
    (print-table "Merged queries"
      '("Query" "Returned" "Added" "Updated" "Unchanged")
      (map (lambda (spec)
             (display* "Running query " (car spec) ": " (cadr spec) "\n")
             (run-query! db spec))
           queries))
    (print-statistics db)
    (close-database db)))

;; Handles the command line arguments, authenticates, and runs the program. The
;; optional argument is the list of command line arguments; it is empty by default.
(define (main . args)
  (let-optionals args ((arguments '()))
    (cond ((equal? arguments '("store-credentials"))
           (store-credentials!))
          ((equal? arguments '("forget-credentials"))
           (forget-credentials!))
          (else
           (let ((auth (authentication)))
             (if auth
                 (parameterize ((tmdb-auth auth))
                   (collect-movies (if (pair? arguments) (car arguments) default-database)))
                 (display* "No TMDB credentials found. Set the environment variables TMDB_ACCESS_TOKEN\n"
                           "and/or TMDB_API_KEY. Run this program with the argument store-credentials\n"
                           "to keep them in the keychain.\n")))))))

(main (let ((arguments (command-line)))
        (if (pair? arguments) (cdr arguments) '())))
