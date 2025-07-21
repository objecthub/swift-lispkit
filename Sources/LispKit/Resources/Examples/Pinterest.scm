;;; Pinterest Board PDF Creator
;;; 
;;; Program for downloading a Pinterest board and creating a PDF
;;; document out of it. The PDF document shows all the sections of
;;; a board together with the pins in each section. There is also
;;; an outline for navigating the sections of large boards.
;;; 
;;; The program uses the `(lispkit http)`, `(lispkit http oauth)`
;;; and `(lispkit json)` libraries to interact with the Pinterest
;;; API. The API requires an "app id" and a "secret key" which
;;; both can be obtained from http://developers.pinterest.com (but
;;; they require a Pinterest business account). Such secrets are
;;; stored in the system keychain and managed with library
;;; `(lispkit system keychain)`. They can also be synchronized across
;;; devices. Without credentials, the program will prompt the user to
;;; set those via `(set-pinterest-credentials <app-id> <secret-key>)`.
;;; Alternatively, one can just set the variables `app-id` and
;;; `secret-key` (those will prevent the keychain from being used).
;;; 
;;; Once the credentials are set, the program will proceed, use OAuth
;;; 2.0 to authenticate a user and then display all the users boards:
;;; 
;;; ```
;;; 906560668681187022: Architecture
;;; 906560668681183226: Drone videos
;;; 906560668681285685: Dubai
;;; ...
;;; ```
;;; 
;;; One can then proceed and create a PDF for one of the listed boards
;;; using procedure `save-board-pdf`. Loading of images is done
;;; concurrently by utilizing library `(lispkit thread future)`.
;;; 
;;; ```
;;; (save-board-pdf <board-id>        ; board or board id
;;;                 [<method>]        ; #f, 'preview, 'share, or filename
;;;                 [<page-size>      ; default is a4-size
;;;                  [<section-font>  ; font for section header
;;;                   [<meta-font>    ; font for section meta data
;;;                    [<title-font>  ; font for pin title
;;;                     [<pin-font>]]]]])  ; font for pin meta data
;;; ```
;;; 
;;; The `<method>` argument can also be a string which either ends in
;;; ".pdf", in which case the argument is a filename, or it's a
;;; directory path and the file name will match the board name (with a
;;; ".pdf" suffix).
;;; 
;;; Example: `(save-board-pdf "906560668681183226" 'preview)`
;;; 
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2025 Matthias Zenger. All rights reserved.
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
        (lispkit record)
        (lispkit enum)
        (lispkit http)
        (lispkit http oauth)
        (lispkit thread)
        (lispkit thread future)
        (lispkit system keychain)
        (lispkit date-time)
        (lispkit draw)
        (lispkit json)
        (lispkit format)
        (lispkit url)
        (lispkit pdf))

(cond-expand
  ((library (lisppad system macos))
    (import (lisppad system macos)))
  ((library (lisppad system ios))
    (import (lisppad system ios))))


;;: OAUTH2 CONFIGURATION

;; Global configuration of Pinterest client

(define min-log-level 5)         ; leave out debug messages (0 = all; 5 = none)
(define remember-auth-tokens #t) ; forget auth tokens after each demo
(define embedded-auth-flow #t)   ; use an embedded auth flow
(define app-id #f)               ; set this to your Pinterest application id
                                 ; (this is a number in a string)
(define secret-key #f)           ; set this to your secret API key

;; Define a service for this application in the system keychain.
(define kc (make-keychain "LispKit Pinterest PDF"))

(define (set-pinterest-credentials . args)
  (let-optionals args ((app-id #f)
                       (secret-key #f))
    (if (not app-id)
        (begin
          (display "app id: ")
          (set! app-id (read-line))))
    (if (not secret-key)
        (begin
          (display "secret key: ")
          (set! secret-key (read-line))))
    (keychain-set-string! kc "app id" app-id)
    (keychain-set-string! kc "secret key" secret-key)
    (display "stored credentials in keychain")
    (newline)))

;; Check if credentials are available; if not prompt the user to
;; provide them via procedure `set-pinterest-credentials`.
(cond
  ((not (or app-id (keychain-ref-string kc "app id")))
    (display "Pinterest app id unknown.\n")
    (display "Invoke (set-pinterest-credentials [app-id [secret-key]]).\n")
    (exit #f))
  ((not (or secret-key (keychain-ref-string kc "secret key")))
    (display "Pinterest secret key unknown.\n")
    (display "Invoke (set-pinterest-credentials [app-id [secret-key]]).\n")
    (exit #f)))

;; Set up Pinterest API client
(define pinterest
  (make-oauth2
    'code-grant-no-token-type
    `((client_id . ,(or app-id (keychain-ref-string kc "app id")))
      (client_secret . ,(or secret-key (keychain-ref-string kc "secret key")))
      (authorize_uri . "https://www.pinterest.com/oauth/")
      (token_uri . "https://api.pinterest.com/v5/oauth/token")
      (redirect_uris . #("lisppad://oauth/callback"))
      (scope . "boards:read,pins:read")
      (secret_in_body . #f)
      (auth_embedded . ,embedded-auth-flow)
      (keychain . ,remember-auth-tokens)
      (verbose . #t)
      (log . ,min-log-level))))

;; Set up an OAuth 2.0 HTTP session that uses the client to authorize requests.
;; Use this session to interact with the Pinterest API.
(define session (make-oauth2-session pinterest))


;;; UTILITIES

;; Turn an association list with symbolic keys and arbitrary Scheme values
;; into an association list mapping strings to strings.
(define (query-params alist)
  (fold-left
    (lambda (z x)
      (if (cdr x)
          (cons (cons (symbol->string (car x)) (object->string (cdr x))) z)
          z))
    '()
    alist))

;; Returns the first result of a multi-valued response.
(define (result values)
  (apply-with-values (lambda args (car args)) values))

;; Returns the second result of a multi-valued response.
(define (bookmark values)
  (apply-with-values (lambda args (cadr args)) values))

;; Handle responses of the Pinterest API by extracting the status code
;; and the JSON content from the response, returning the JSON value
;; if the status code signals success, otherwise fail with a suitable error.
(define (handle-response response success)
  (let ((code (http-response-status-code response))
        (json (bytevector->json (http-response-content response))))
    (if (= code success)
        json
        (error "[Pinterest API] HTTP $0 $,1: $,2 ($3)"
               code
               (string-titlecase (http-status-code->string code))
               (json-member json 'message)
               (json-member json 'code)))))

;; Compute the luminance of a given color `col`.
(define (color-luminance col)
  ; `linearize` converts an sRGB-based component into a linear RGB component
  (let ((linearize
          (lambda (c)
            (if (<= c 0.04045) (/ c 12.92) (expt (/ (+ c 0.055) 1.055) 2.4)))))
    ; Compute luminance based on linearized components
    (+ (* 0.2126 (linearize (color-red col)))
       (* 0.7152 (linearize (color-green col)))
       (* 0.0722 (linearize (color-blue col))))))

;; Choose a luminance-based best text color
(define (text-color col)
  (let* (; First compute luminance (0 <= y <= 1)
         (y (color-luminance col))
         ; Then compute perceived lightness (0 <= ys <= 100)
         (ys (if (<= y 0.008856) (* y 903.3) (- (* 116 (expt y 1/3)) 16))))
    ; 55 as a threshold was picked based on personal preference
    (if (>= ys 55) black white)))

;; Right-align text in the given rectangle `box` and draw it in the given
;; font and color.
(define (draw-text-right str box font col)
  (let ((tsize (text-size str font (rect-size box))))
    (draw-text str
               (rect (- (+ (rect-x box) (rect-width box)) (size-width tsize))
                     (rect-y box)
                     (size-width tsize)
                     (size-height tsize))
               font col)))


;;; IMAGES

;; Start loading the image from the given URL, but don't block on it and
;; return a future for the response instead.
(define (pinterest-image-async url)
  (http-session-send (make-http-request url "GET")))

;; Retrieve image object from the future created via `pinterest-image-async`
(define (pinterest-image-get future)
  (bytevector->image (http-response-content (future-get future))))

;; Load image synchronously and return image object
(define (pinterest-image url)
  (pinterest-image-get (pinterest-image-async url)))


;;; BOARDS

;; Board privacy options
(define-enum privacy
  (public protected secret)
  privacy-set)

(define (string->privacy-set str)
  (case (string->symbol str)
    ((PUBLIC) (privacy-set public))
    ((PROTECTED) (privacy-set protected))
    ((SECRET) (privacy-set secret))
    ((ALL) (privacy-set public protected secret))
    ((PUBLIC_AND_SECRET) (privacy-set public secret))))

(define (privacy-set->string ps)
  (cond
    ((not ps) "PUBLIC")
    ((enum-set=? ps (privacy-set public)) "PUBLIC")
    ((enum-set=? ps (privacy-set protected)) "PROTECTED")
    ((enum-set=? ps (privacy-set secret)) "SECRET")
    ((enum-set=? ps (privacy-set public protected secret)) "ALL")
    ((enum-set=? ps (privacy-set public secret)) "PUBLIC_AND_SECRET")
    (else (error "illegal privacy specification"))))

;; Board representation as a record
(define-record-type board
  (new-board id owner created modified privacy name description
             cover thumbnails pins collaborators followers ads-only?)
  board?
  (id internal-board-id)
  (owner board-owner)
  (created board-created)
  (modified board-modified)
  (privacy board-privacy set-board-privacy!)
  (name board-name set-board-name!)
  (description board-description set-board-description!)
  (cover board-cover-url)
  (thumbnails board-thumbnail-urls)
  (pins board-pin-count)
  (collaborators board-collaborator-count)
  (followers board-follower-count)
  (ads-only? board-ads-only?))

;; Returns the board id of `x`.
(define (board-id x)
  (if (string? x) x (internal-board-id x)))

;; Makes a new empty board.
(define make-board
  (case-lambda
    ((name)
      (make-board name #f (privacy-set public) #f))
    ((name description)
      (make-board name description (privacy-set public) #f))
    ((name description priv)
      (make-board name description (privacy-set public) #f))
    ((name description priv ads-only?)
      (new-board #f #f #f #f priv name description #f #() 0 0 0 ads-only?))))

;; Converts the given JSON object into two result values: a list of
;; board objects and a bookmark string which can be used to retrive the
;; next batch of boards.
(define (json->boards res)
  (values
    (map json->board (json-children (json-ref res 'items)))
    (json-member res 'bookmark)))

;; Converts the given JSON object into a board record.
(define (json->board j)
  (new-board
    (json-member j 'id)
    (json-member j '(owner username))
    (string->date-time (json-member j 'created_at) 'UTC)
    (string->date-time (json-member j 'board_pins_modified_at) 'UTC)
    (string->privacy-set (json-member j 'privacy "PUBLIC"))
    (json-member j 'name)
    (json-member j 'description)
    (json-member j '(media image_cover_url))
    (json-member j '(media pin_thumbnail_urls))
    (json-member j 'pin_count)
    (json-member j 'collaborator_count)
    (json-member j 'follower_count)
    (json-member j 'is_ads_only)))

;; Converts the given board record into a JSON object.
(define (board->json board)
  (json-object
    (id (internal-board-id board))
    (owner (json-object (username (board-owner board))))
    (created_at (date-time->string (board-created board) #t #t))
    (board_pins_modified_at (date-time->string (board-modified board) #t #t))
    (privacy (privacy-set->string (board-privacy board)))
    (name (board-name board))
    (description (board-description board))
    (media (json-object
             (image_cover_url (board-cover-url board))
             (pin_thumbnail_urls (board-thumbnail-urls board))))
    (pin_count (board-pin-count board))
    (collaborator_count (board-collaborator-count board))
    (follower_count (board-follower-count board))
    (is_ads_only (board-ads-only? board) boolean?)))

;; Sends a request for retrieving boards via an OAuth 2.0 session. This
;; procedure returns a future containing the response of the request (or
;; an error if the request failed).
(define (pinterest-boards-async session)
  (assert (oauth2-session? session))
  (oauth2-session-send
    (make-http-request "https://api.pinterest.com/v5/boards" "GET")
    session))

;; Retrieves the body of the HTTP response from the future created via
;; `pinterest-boards-async`. This procedure will block until the response
;; has been received.
(define (pinterest-boards-get future)
  (json->boards (handle-response (future-get future) 200)))

;; Returns a list of boards for the authenticated session synchronously.
(define (pinterest-boards session)
  (pinterest-boards-get (pinterest-boards-async session)))

;; Sends a request for retrieving board `id` via an OAuth 2.0 session.
;; This procedure returns a future containing the response of the
;; request (or an error if the request failed).
(define (pinterest-board-async session id)
  (assert (oauth2-session? session))
  (oauth2-session-send
    (make-http-request
      (format "https://api.pinterest.com/v5/boards/~A" id)
      "GET")
    session))

;; Retrieves the body of the HTTP response from the future created
;; via `pinterest-board-async`. This procedure will block until the
;; response has been received.
(define (pinterest-board-get future)
  (json->board (handle-response (future-get future) 200)))

; Returns the board for the given board `id` using the given
; authenticated HTTP session.
(define (pinterest-board session id)
  (pinterest-board-get (pinterest-board-async session id)))


;;; SECTIONS

;; Section representation as a record.
(define-record-type section
  (new-section id board-id name)
  section?
  (id section-id)
  (board-id section-board-id)
  (name section-name set-section-name!))

;; Creates a new section.
(define make-section
  (case-lambda
    ((id board-id)
      (new-section id board-id #f))
    ((id board-id name)
      (new-section id board-id name))))

;; Converts a JSON object `res` into a list of sections and returns this list
;; together with a bookmark as a second return value. The bookmark can be used
;; to retrieve the next batch of sections (unless it is `'null`).
(define (json->sections board res)
  (values
    (map (lambda (j) (json->section board j)) (json-children (json-ref res 'items)))
    (json-member res 'bookmark)))

;; Converts the given JSON object into a section record.
(define (json->section board j)
  (new-section
    (json-member j 'id)
    (board-id board)
    (json-member j 'name)))

;; Sends a request for retrieving sections via an OAuth 2.0 session. This
;; procedure returns a future containing the response of the request (or
;; an error if the request failed).
(define (pinterest-board-sections-async session board)
  (assert (oauth2-session? session)
          (or (string? board) (board? board)))
  (oauth2-session-send
    (make-http-request
      (format "https://api.pinterest.com/v5/boards/~A/sections"
              (board-id board))
      "GET")
    session))

;; Retrieves the body of the HTTP response from the future created via
;; `pinterest-board-sections-async`. This procedure will block until the
;; response has been received.
(define (pinterest-board-sections-get board future)
  (json->sections board (handle-response (future-get future) 200)))

;; Returns a list of sections for the authenticated session and the given
;; board synchronously.
(define (pinterest-board-sections session board)
  (pinterest-board-sections-get board (pinterest-board-sections-async session board)))


;;; PINS

;; Representation of a pin as a record.
(define-record-type pin
  (new-pin id parent created title description link media color alt type bid bsid bowner
           owner? standard? promoted? removable? tags note metrics msource)
  pin?
  (id internal-pin-id)
  (parent pin-parent)
  (created pin-created)
  (title pin-title set-pin-title!)
  (description pin-description set-pin-description!)
  (link pin-link set-pin-link!)
  (media pin-media)
  (color pin-dominant-color)
  (alt pin-alt-text set-pin-alt-text!)
  (type pin-creative-type)
  (bid pin-board-id set-pin-board-id!)
  (bsid pin-board-section-id set-pin-board-section-id!)
  (bowner pin-board-owner)
  (owner? pin-owner?)
  (standard? pin-standard?)
  (promoted? pin-promoted?)
  (removable? pin-removable?)
  (tags pin-tags)
  (note pin-note set-pin-note!)
  (metrics pin-metrics)
  (msource pin-media-source))

;; Returns the pin id for the given pin object or string.
(define (pin-id x)
  (if (string? x) x (internal-pin-id x)))

;; Creates a new pin.
(define make-pin
  (case-lambda
    (()
      (new-pin #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
    ((title)
      (new-pin #f #f #f title #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
    ((title descr)
      (new-pin #f #f #f title descr #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
    ((title descr link)
      (new-pin #f #f #f title descr link #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
    ((title descr link media)
      (new-pin #f #f #f title descr link #f #f #f #f #f #f #f #f #f #f #f #f #f #f media))
    ((title descr link media color)
      (new-pin #f #f #f title descr link #f color #f #f #f #f #f #f #f #f #f #f #f #f media))
    ((title descr link media color parent)
      (new-pin #f parent #f title descr link #f color #f #f #f #f #f #f #f #f #f #f #f #f media))))

;; The following procedures create media sources as JSON objects. These
;; can be used to upload media into pins. The procedures are provided as
;; a basis for creating different Pinterest integrations. They are not
;; used in this program (which just reads content).

(define (media-source-image-url url)
  (json-object
    (source_type "image_url")
    (url url)))

(define (media-source-multiple-image-urls refs . args)
  (let-optionals args ((index 0))
    (json-object
      (source_type "multiple_image_urls")
      (index index)
      (items (list->vector
               (map (lambda (ref)
                      (if (string? ref)
                          (json-object (url ref))
                          (json-object (title (car ref) string?)
                                       (description (cadr ref) string?)
                                       (link (caddr ref) string?)
                                       (url (cadddr ref)))))
                    ref))))))

(define (media-source-image content-type image)
  (json-object
    (source_type "image_base64")
    (content_type content-type)
    (data (bytevector->base64 (bitmap->bytevector image)))))

(define (media-source-multiple-images refs . args)
  (let-optionals args ((index 0))
    (json-object?
      (source_type "multiple_image_base64")
      (index index)
      (items (list->vector
               (map (lambda (ref)
                      (if (= (length ref) 2)
                          (json-object (content_type (car ref))
                                       (data (bytevector->base64
                                               (bitmap->bytevector (cadr ref)))))
                          (json-object (title (car ref) string?)
                                       (description (cadr ref) string?)
                                       (link (caddr ref) string?)
                                       (content_type (cadddr ref))
                                       (data (bytevector->base64
                                               (bitmap->bytevector
                                                 (car (cddddr ref))))))))
                    ref))))))

;; Representation of creative types as an enum.
(define-enum creative-type
  ((regular "REGULAR")
   (video "VIDEO")
   (shopping "SHOPPING")
   (carousel "CAROUSEL")
   (max-video "MAX_VIDEO")
   (collection "COLLECTION")
   (idea "IDEA")
   (shop-the-pin "SHOP_THE_PIN"))
  creative-type-set)

(define creative-type? (enum-type-test-predicate (creative-type)))
(define creative-type->string enum-tag)
(define string->creative-type (enum-tag-mapper (creative-type)))

;; Representation of media types as an enum.
(define-enum media-type
  ((image "image")
   (video "video")
   (multiple-images "multiple_images")
   (multiple-videos "multiple_videos")
   (multiple-mixed "multiple_mixed"))
  media-type-set)

(define media-type? (enum-type-test-predicate (media-type)))
(define media-type->string enum-tag)
(define string->media-type (enum-tag-mapper (media-type)))

;; Converts a JSON object into a list of pin records.
(define (json->pins res)
  (values
    (map json->pin (json-children (json-ref res 'items)))
    (json-member res 'bookmark)))

;; Converts a JSON object into a pin record.
(define (json->pin j)
  (new-pin
    (json-member j 'id)
    (json-member j 'parent_pin_id)
    (string->date-time (json-member j 'created_at) 'UTC)
    (json-member j 'title)
    (json-member j 'description)
    (if (string? (json-member j 'link)) (json-member j 'link) #f)
    (json-member j 'media)
    (color (json-member j 'dominant_color))
    (json-member j 'alt_text)
    (string->creative-type (json-member j 'creative_type))
    (json-member j 'board_id)
    (json-member j 'board_section_id)
    (json-member j '(board_owner username))
    (json-member j 'is_owner)
    (json-member j 'is_standard)
    (json-member j 'has_been_promoted)
    (json-member j 'is_removable)
    (json-member j 'product_tags)
    (json-member j 'note)
    (json-member j 'pin_metrics)
    #f))

;; Converts a pin into a JSON object.
(define (pin->json pin)
  (json-object
    (id (internal-pin-id pin))
    (parent_pin_id (pin-parent pin))
    (created_at (date-time->string (pin-created pin) #t #t))
    (title (pin-title pin))
    (description (pin-description pin))
    (link (pin-link pin))
    (media (pin-media pin))
    (dominant_color (opt color->hex (pin-dominant-color pin) #f))
    (alt_text (pin-alt-text pin))
    (creative_type (creative-type->string (pin-creative-type pin)))
    (board_id (pin-board-id pin))
    (board_section_id (pin-board-section-id pin))
    (board_owner (json-object (username (pin-board-owner pin))))
    (is_owner (pin-owner? pin) boolean?)
    (is_standard (pin-standard? pin) boolean?)
    (has_been_promoted (pin-promoted? pin) boolean?)
    (is_removable (pin-removable? pin) boolean?)
    (product_tags (pin-tags pin))
    (note (pin-note pin))
    (pin_metrics (pin-metrics pin))
    (media_source (pin-media-source pin))))

;; Sends a request for retrieving pins via an OAuth 2.0 session. This
;; procedure returns a future containing the response of the request (or
;; an error if the request failed).
(define (pinterest-pins-async session . args)
  (assert (oauth2-session? session))
  (oauth2-session-send
    (make-http-request
      (url
        "https" "api.pinterest.com" "/v5/pins"
        (if (and (pair? args) (pair? (car args)))
          (query-params args)
          (let-optionals args ((bookmark #f)(page-size 25)(filter #f)
                               (prot #f)(ctype #f)(metrics #f))
            (query-params `((bookmark . ,bookmark)
                            (page_size . ,page-size)
                            (pin_filter . ,filter)
                            (include_protected_pins . ,prot)
                            (creative_types . ,ctype)
                            (pin_metrics . ,metrics))))))
      "GET")
    session))

;; Retrieves the body of the HTTP response from the future created via
;; `pinterest-pins-async`. This procedure will block until the
;; response has been received.
(define (pinterest-pins-get future)
  (json->pins (handle-response (future-get future) 200)))

;; Returns a list of pins for the authenticated session synchronously.
(define (pinterest-pins session . args)
  (pinterest-pins-get (apply pinterest-pins-async session args)))

;; Sends a request for retrieving pins for the given board via an OAuth
;; 2.0 session. This procedure returns a future containing the response
;; of the request (or an error if the request failed).
(define (pinterest-board-pins-async session board . args)
  (assert (oauth2-session? session)
          (or (string? board) (board? board)))
  (oauth2-session-send
    (make-http-request
      (url "https" "api.pinterest.com"
        (format "/v5/boards/~A/pins" (board-id board))
        (if (and (pair? args) (pair? (car args)))
          (query-params args)
          (let-optionals args ((bookmark #f)(page-size 25)(ctype #f)(metrics #f))
            (query-params `((bookmark . ,bookmark)
                            (page_size . ,page-size)
                            (creative_types . ,ctype)
                            (pin_metrics . ,metrics))))))
      "GET")
    session))

;; Retrieves the body of the HTTP response from the future created via
;; `pinterest-board-pins-async`. This procedure will block until the
;; response has been received.
(define (pinterest-board-pins-get future)
  (json->pins (handle-response (future-get future) 200)))

;; Returns a list of pins for the given board of the authenticated session
;; synchronously.
(define (pinterest-board-pins session . args)
  (pinterest-board-pins-get (apply pinterest-board-pins-async session args)))

;; Sends a request for retrieving pins for the given section via an OAuth
;; 2.0 session. This procedure returns a future containing the response
;; of the request (or an error if the request failed).
(define (pinterest-section-pins-async session section . args)
  (assert (oauth2-session? session)
          (section? section))
  (oauth2-session-send
    (make-http-request
      (url "https" "api.pinterest.com"
        (format "/v5/boards/~A/sections/~A/pins"
                (section-board-id section)
                (section-id section))
        (if (and (pair? args) (pair? (car args)))
          (query-params args)
          (let-optionals args ((bookmark #f)(page-size 25))
            (query-params `((bookmark . ,bookmark)
                            (page_size . ,page-size))))))
      "GET")
    session))

;; Retrieves the body of the HTTP response from the future created via
;; `pinterest-board-pins-async`. This procedure will block until the
;; response has been received.
(define (pinterest-section-pins-get future)
  (json->pins (handle-response (future-get future) 200)))

;; Returns a list of pins for the given section of the authenticated
;; session synchronously.
(define (pinterest-section-pins session . args)
  (pinterest-section-pins-get (apply pinterest-section-pins-async session args)))


;;; PINTEREST BOARD PDFS

;; Return a PDF page for the given board, section, and page number. All
;; meta data needed for drawing the page is provided as an argument to
;; procedure `make-section-pdf-page`.
(define (make-section-pdf-page board
                               section
                               total
                               pnr
                               page-size
                               indent
                               section-font
                               meta-font
                               title-font
                               pin-font
                               pins
                               verbose)
  (let ((page (make-pdf-page page-size))  ; Make a new PDF page
        (canvas (make-drawing))           ; Create a new empty canvas
        (content-width (- (size-width page-size) (* 2 (point-x indent))))
        ; Kick off loading of all images; this is done asynchronously
        ; to enable loading of the content in parallel.
        (pin-images
          (map
            (lambda (pin)  
              (let* ((images (assoc 'images (pin-media pin)))
                     (meta (and images (assoc '|400x300| (cdr images))))
                     (url (and meta (assoc 'url (cdr meta)))))
                (pinterest-image-async (cdr url))))
            pins)))
    ; Draw all content onto the canvas...
    (with-drawing canvas
      (set-color black)
      ; Draw the header
      (cond
        ((<= pnr 1)
          (draw-text (section-name section) indent section-font black)
          (draw-text-right
            (board-name board)
            (rect (move-point indent 0 4) (size content-width 30))
            meta-font
            black)
          (draw-text-right
            (format "~D of ~D pins" total (board-pin-count board))
            (rect (move-point indent 0 (+ (font-size meta-font) 4))
                  (size content-width 30))
            meta-font
            black))
        (else
          (let ((topleft (move-point indent 0 (- (font-size section-font)
                                                 (font-size meta-font)))))
            (draw-text
              (format "~A (~A)" (section-name section) (board-name board))
              topleft
              meta-font
              black)
            (draw-text-right
              (format "Page ~D" pnr)
              (rect topleft (size content-width 30))
              meta-font
              black))))
      ; Some output if verbose is set to `#t`
      (if verbose
          (display* "Creating page with " (length pins)
                    " pins from section '" (section-name section) "'\n"))
      ; Iterate through all pins...
      (do ((i 0 (+ i 1)))
          ((>= i (length pins)))
        ; Set up geometry of page
        (let* ((pin (list-ref pins i))
               (pin-link (or (pin-link pin)
                             (format "https://pinterest.com/pin/~A/" (pin-id pin))))
               (image-box (rect (+ (point-x indent) (* (truncate-remainder i 3) 175))
                                (+ (point-y indent) 45 (* (truncate-quotient i 3) 180))
                                160
                                120))
               (title-box (inset-rect
                            (rect (rect-x image-box)
                                  (+ (rect-y image-box) (rect-height image-box))
                                  (rect-width image-box)
                                  (- 165 (rect-height image-box)))
                            5 3 0 0))
               (info-box (rect (+ (rect-x image-box) 8)
                               (+ (rect-y image-box) 155)
                               (- (rect-width image-box) 16)
                               8))
               (frame (rectangle (rect-point image-box)
                                 (size (rect-width image-box) 165) 10))
               (text-color (text-color (pin-dominant-color pin)))
               (linka (make-pdf-annotation (transpose image-box page-size #t) 'link)))
          ; Start by creating the background color (using the dominant
          ; color of the pin)
          (set-fill-color (pin-dominant-color pin))
          (fill frame)
          ; Clip all content into the frame of the pin
          (when url
            (clip-drawing
              (drawing
                (draw-text (pin-title pin) title-box title-font text-color)
                (draw-text (pin-id pin) info-box pin-font text-color)
                (draw-text-right (date-time->string (pin-created pin))
                                 info-box
                                 pin-font
                                 text-color)
                (draw-image (pinterest-image-get (list-ref pin-images i)) image-box))
              frame))
          ; Draw the frame in black
          (set-color black)
          (draw frame 0.5)
          (set-fill-color white)
          ; Define a PDF annotation for the page that implements the
          ; external link
          (pdf-annotation-name-set! linka (pin-id pin))
          (pdf-annotation-url-set! linka pin-link)
          (pdf-page-annotation-add! page linka))))
    ; Add the canvas to the PDF page and return the PDF page object
    (pdf-page-overlay-set! page canvas)
    page))

;; This procedure creates and returns a PDF file (in memory) for the given
;; board (or board id) using the given authenticated HTTP session. There
;; are several optional arguments which can be used to customize the look
;; of the generated PDF file.
(define (make-board-pdf session bid . args)
  (let-optionals args ((verbose #t)
                       (page-size a4-size)
                       (section-font (font "Georgia" 24 bold))
                       (meta-font (font "Georgia" 10 normal))
                       (title-font (font "Georgia" 8 bold))
                       (pin-font (font "Georgia" 6 normal)))
    (let* ((board (pinterest-board session (board-id bid)))
           (sections (result (pinterest-board-sections session board)))
           (pdf (make-pdf))
           (outline (make-pdf-outline))
           (indent (point (/ (- (size-width page-size) 175 175 160) 2)
                          (/ (- (size-height page-size) 180 180 180 165 45) 2))))
      ; Set a few document attributes
      (pdf-attribute-set! pdf 'Title (format "Pinterest Board: ~A" (board-name board)))
      (pdf-attribute-set! pdf 'Author (board-owner board))
      (pdf-attribute-set! pdf 'Creator "Pinterest Board PDF Creator")
      ; Register the PDF outline
      (pdf-outline-set! pdf outline)
      ; Iterate through all sections...
      (for-each
        (lambda (section)
          (let-values
            (((total pages) ; Create a list of all pages for this section
              (let iter ((bookmark #f)(pnr 1))
                (if (eq? bookmark 'null)
                    (values 0 '())
                    (let-values
                      (((pins next)
                        (pinterest-section-pins session section bookmark 12)))
                      (if (list? pins)
                          (apply-with-values
                            (lambda (t rest)
                              (values (+ (length pins) t) (cons (cons pnr pins) rest)))
                            (iter next (+ pnr 1)))
                          (iter next pnr))))))
             ((first-page) #f))
            ; Create a PDF page for all pages representing content from
            ; this section. Delegate most work to `make-section-pdf-page`.
            (for-each
              (lambda (page)
                (let ((pdf-page
                        (make-section-pdf-page
                          board section total (car page) page-size indent
                          section-font meta-font title-font pin-font (cdr page) verbose)))
                  (pdf-insert-page! pdf pdf-page)
                  (if (not first-page)
                      (set! first-page pdf-page))))
              pages)
            ; Create an entry in the table of contents
            (if first-page
                (pdf-outline-child-insert!
                  outline
                  (make-pdf-outline (section-name section) first-page)))))
        sections)
        pdf)))

;;; This is a wrapper around `make-board-pdf` which uses the globally
;;; defined session and provides different means to "deliver" the generated
;;; PDF file. The second parameter determines the delivery method. Supported
;;; are `#f` (write PDF file to disk into the current directory with the
;;; board name as filename), `'share` (only supported on iOS: share the
;;; generated PDF file via the system's share panel), `'preview` (open the
;;; generated PDF in a preview panel or in a suitable application),
;;; `"~/dir/fname.pdf"` (write the PDF file using the given full file path),
;;; `"~/dir" (write the PDF file into the given directory using the board
;;; name as filename). All other arguments are passed to `make-board-pdf`
;;; as they are.
(define (save-board-pdf bid . args)
  (let ((pdf (apply make-board-pdf session bid (if (pair? args) (cdr args) '())))
        (board (pinterest-board session (board-id bid))))
    (if (and (pair? args) (string? (car args)))
        (if (string-suffix? (car args) ".pdf")
            (save-pdf (car args) pdf)
            (save-pdf (file-path (format "~A.pdf" (board-name board)) (car args)) pdf))
        (if (or (null? args) (and (pair? args) (not (car args))))
            (save-pdf (format "~A.pdf" (board-name board)) pdf)
            (cond-expand
              ((library (lisppad system macos))
                (if (and (pair? args) (eq? (car args) 'preview))
                    (let ((filepath (file-path (format "~A.pdf" (board-name board))
                                               (car (system-directory 'temporary)))))
                      (save-pdf filepath pdf)
                      (open-file filepath))
                    (save-pdf (show-save-panel "Save Pinterest board PDF" '("pdf")) pdf)))
              ((library (lisppad system ios))
                (cond
                  ((and (pair? args) (eq? (car args) 'share))
                    (show-share-panel (pdf->bytevector pdf) "pdf"))
                  ((and (pair? args) (eq? (car args) 'preview))
                    (show-preview-panel (pdf->bytevector pdf) "pdf"))
                  (else
                    (save-pdf (show-save-panel "Save Pinterest board PDF") pdf))))
              (else
                (let ((filepath (file-path (format "~A.pdf" (board-name board))
                                           (car (system-directory 'temporary)))))
                  (save-pdf filepath pdf)
                  (open-file filepath))))))))


;;; SHOW ALL BOARDS

;; Store a list of all Pinterest boards
(define boards (result (pinterest-boards session)))

;; Display board id and board name for all boards
(for-each
  (lambda (b)
    (display (format "~A: ~A" (board-id b) (board-name b)))
    (newline))
  boards)
