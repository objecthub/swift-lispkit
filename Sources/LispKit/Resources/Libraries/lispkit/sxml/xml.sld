;;; LISPKIT SXML XML
;;; 
;;; The library implements an XML parser and a version of the parser which returns the XML
;;; in SXML form. The XML standard supported by this library is
;;; http://www.w3.org/TR/1998/REC-xml-19980210.html
;;; The library fully supports the XML namespaces recommendation at
;;; http://www.w3.org/TR/REC-xml-names
;;; 
;;; Copyright information:
;;;   xml-parse.scm - XML parsing and conversion to SXML (Scheme-XML)
;;;   Copyright © 2007 Aubrey Jaffer
;;;   2007-04 jaffer: demacrofied from public-domain SSAX 5.1
;;;   2017: Packaged for R7RS Scheme by Peter Lane
;;;   2020: Packaged and adapted for LispKit by Matthias Zenger
;;;
;;;   Permission to copy-bit this software, to modify it, to redistribute it,
;;;   to distribute modified versions, and to use it for any purpose is
;;;   granted, subject to the following restrictions and understandings.
;;;   
;;;   1. Any copy made of this software must include this copyright notice in full.
;;;   2. I have made no warranty or representation that the operation of this software
;;;      will be error-free, and I am under no obligation to provide any services, by
;;;      way of maintenance, update, or otherwise.
;;;   3. In conjunction with products arising from the use of this material, there
;;;      shall be no use of my name in any advertising, promotional, or sales literature
;;;      without prior written consent in each case.
;;; 
;;; Adaptation to LispKit
;;;   Copyright © 2020 Matthias Zenger. All rights reserved.

(define-library (lispkit sxml xml)
  
  (export ssax-read-char-data
          ssax-read-attributes
          ssax-read-external-id
          ssax-scan-misc
          ssax-resolve-name
          make-ssax-parser
          make-ssax-pi-parser
          make-ssax-elem-parser
          xml->sxml
          ssax-warn
          xml-token-kind
          xml-token-head)

  ; (export attlist-add
  ;         attlist-remove-top
  ;         ssax:assert-current-char
  ;         ssax:assert-token
  ;         ssax:complete-start-tag
  ;         ssax:handle-parsed-entity
  ;         ssax:init-buffer
  ;         make-ssax-elem-parser
  ;         make-ssax-parser
  ;         make-ssax-pi-parser
  ;         ssax:next-token
  ;         ssax:next-token-of
  ;         ssax:Prefix-XML
  ;         ssax:read-NCName
  ;         ssax:read-QName
  ;         ssax-read-attributes
  ;         ssax:read-cdata-body
  ;         ssax-read-char-data
  ;         ssax:read-char-ref
  ;         ssax-read-external-id
  ;         ssax:read-markup-token
  ;         ssax:read-pi-body-as-string
  ;         ssax:read-string
  ;         ssax-resolve-name
  ;         ssax:reverse-collect-str-drop-ws
  ;         ssax-scan-misc
  ;         ssax:skip-S
  ;         ssax:skip-internal-dtd
  ;         ssax:skip-pi
  ;         ssax:skip-while
  ;         xml->sxml
  ;         ssax-warn
  ;         ssax:reverse-collect-str)
  
  (import (lispkit base)
          (srfi 1))
  
  (begin
    
    (define (ssax-warn port msg . other-msg)
      (for-each (lambda (x) (display x (current-error-port)))
                `("\nWarning: " msg ,@other-msg "\n")))
    
    ;; STRING UTILITIES

    (define (substring-move-left! string1 start1 end1 string2 start2)
      (do ((i start1 (+ i 1))
           (j start2 (+ j 1))
           (l (- end1 start1) (- l 1)))
        ((<= l 0))
        (string-set! string2 j (string-ref string1 i))))
        
    (define (string-null? str)
      (= 0 (string-length str)))
      
    (define (string-index str chr)
      (do ((len (string-length str))
           (pos 0 (+ 1 pos)))
          ((or (>= pos len)
               (char=? chr (string-ref str pos)))
               (and (< pos len) pos))))
    
    (define (find-string-from-port? str <input-port> . max-no-char-in)
      (let ((max-no-char (if (null? max-no-char-in) #f (car max-no-char-in))))
        (letrec ((no-chars-read 0)
                 (peeked? #f)
                 (my-peek-char              ; Return a peeked char or #f
                   (lambda ()
                     (and (or (not (number? max-no-char))
                              (< no-chars-read max-no-char))
                          (let ((c (peek-char <input-port>)))
                            (cond (peeked? c)
                                  ((eof-object? c) #f)
                                  ((procedure? max-no-char)
                                    (set! peeked? #t)
                                    (if (max-no-char c) #f c))
                                  ((eqv? max-no-char c) #f)
                                  (else c))))))
                 (next-char
                   (lambda ()
                     (set! peeked? #f)
                     (read-char <input-port>)
                     (set! no-chars-read (+ 1 no-chars-read))))
                 (match-1st-char	           ; of the string str
                   (lambda ()
                     (let ((c (my-peek-char)))
                       (and c (begin (next-char)
                                     (if (char=? c (string-ref str 0))
                                         (match-other-chars 1)
                                         (match-1st-char)))))))
                 ;; There has been a partial match, up to the point pos-to-match
                 ;; (for example, str[0] has been found in the stream)
                 ;; Now look to see if str[pos-to-match] for would be found, too
                 (match-other-chars
                   (lambda (pos-to-match)
                     (if (>= pos-to-match (string-length str))
                       no-chars-read	       ; the entire string has matched
                       (let ((c (my-peek-char)))
                         (and c
                              (if (not (char=? c (string-ref str pos-to-match)))
                                (backtrack 1 pos-to-match)
                                (begin (next-char)
                                       (match-other-chars (+ 1 pos-to-match)))))))))
                 ;; There had been a partial match, but then a wrong char showed up.
                 ;; Before discarding previously read (and matched) characters, we check
                 ;; to see if there was some smaller partial match. Note, characters read
                 ;; so far (which matter) are those of str[0..matched-substr-len - 1]
                 ;; In other words, we will check to see if there is such i>0 that
                 ;; substr(str,0,j) = substr(str,i,matched-substr-len)
                 ;; where j=matched-substr-len - i
                 (backtrack
                   (lambda (i matched-substr-len)
                     (let ((j (- matched-substr-len i)))
                       (if (<= j 0)
                           ;; backed off completely to the begining of str
                           (match-1st-char)
                           (let loop ((k 0))
                             (if (>= k j)
                                 (match-other-chars j) ; there was indeed a shorter match
                                 (if (char=? (string-ref str k)
                                             (string-ref str (+ i k)))
                                     (loop (+ 1 k))
                                     (backtrack (+ 1 i) matched-substr-len)))))))))
          (match-1st-char))))

    ;; Three functions from SRFI-13
    
    ;; procedure string-concatenate-reverse STRINGS [FINAL END]
    (define (ssax:string-concatenate-reverse strs final end)
      (if (null? strs) (string-copy final 0 end)
        (let*
          ((total-len
             (let loop ((len end) (lst strs))
               (if (null? lst) len
                 (loop (+ len (string-length (car lst))) (cdr lst)))))
           (result (make-string total-len)))
          (let loop ((len end) (j total-len) (str final) (lst strs))
            (substring-move-left! str 0 len result (- j len))
            (if (null? lst) result
              (loop (string-length (car lst)) (- j len)
                    (car lst) (cdr lst)))))))
    
    ; string-concatenate/shared STRING-LIST -> STRING
    (define (ssax:string-concatenate/shared strs)
      (cond ((null? strs) "")		; Test for the fast path first
            ((null? (cdr strs)) (car strs))
            (else
              (let*
                ((total-len
                   (let loop ((len (string-length (car strs))) (lst (cdr strs)))
                     (if (null? lst) len
                       (loop (+ len (string-length (car lst))) (cdr lst)))))
                 (result (make-string total-len)))
                (let loop ((j 0) (str (car strs)) (lst (cdr strs)))
                  (substring-move-left! str 0 (string-length str) result j)
                  (if (null? lst) result
                    (loop (+ j (string-length str))
                          (car lst) (cdr lst))))))))
    
    ;; string-concatenate-reverse/shared STRING-LIST [FINAL-STRING END] -> STRING
    ;; We do not use the optional arguments of this procedure.  Therefore,
    ;; we do not implement them.  See SRFI-13 for the complete implementation.
    (define (ssax:string-concatenate-reverse/shared strs)
      (cond ((null? strs) "")		; Test for the fast path first
            ((null? (cdr strs)) (car strs))
            (else
              (ssax:string-concatenate-reverse (cdr strs)
                                               (car strs)
                                               (string-length (car strs))))))
    
    ;; Given the list of fragments (some of which are text strings), reverse the list and
    ;; concatenate adjacent text strings.  If LIST-OF-FRAGS has zero or one element, the
    ;; result of the procedure is `equal?` to its argument.
    (define (ssax:reverse-collect-str fragments)
      (cond
        ((null? fragments) '())		; a shortcut
        ((null? (cdr fragments)) fragments)	; see the comment above
        (else
          (let loop ((fragments fragments) (result '()) (strs '()))
            (cond
              ((null? fragments)
               (if (null? strs)
                 result
                 (cons (ssax:string-concatenate/shared strs) result)))
              ((string? (car fragments))
               (loop (cdr fragments) result (cons (car fragments) strs)))
              (else
                (loop (cdr fragments)
                      (cons (car fragments)
                            (if (null? strs)
                              result
                              (cons (ssax:string-concatenate/shared strs) result)))
                      '())))))))
    
    ;; Given the list of fragments (some of which are text strings), reverse the list and
    ;; concatenate adjacent text strings while dropping "unsignificant" whitespace, that is,
    ;; whitespace in front, behind and between elements. The whitespace that is included in
    ;; character data is not affected.
    ;;
    ;; Use this procedure to "intelligently" drop "insignificant" whitespace in the parsed
    ;; SXML. If the strict compliance with the XML Recommendation regarding the whitespace
    ;; is desired, use the `ssax:reverse-collect-str` procedure instead.
    (define (ssax:reverse-collect-str-drop-ws fragments)
      ; Test if a string is made of only whitespace.
      ; An empty string is considered made of whitespace as well
      (define (string-whitespace? str)
        (let ((len (string-length str)))
          (cond ((zero? len) #t)
                ((= 1 len) (char-whitespace? (string-ref str 0)))
                (else
                  (let loop ((i 0))
                    (or (>= i len)
                        (and (char-whitespace? (string-ref str i))
                             (loop (+ 1 i)))))))))
      (cond
        ((null? fragments) '())	        ; a shortcut
        ((null? (cdr fragments))       ; another shortcut
         (if (and (string? (car fragments)) (string-whitespace? (car fragments)))
           '()				; remove trailing ws
           fragments))
        (else
          (let loop ((fragments fragments) (result '()) (strs '())
                                           (all-whitespace? #t))
            (cond
              ((null? fragments)
               (if all-whitespace?
                 result			; remove leading ws
                 (cons (ssax:string-concatenate/shared strs) result)))
              ((string? (car fragments))
               (loop (cdr fragments)
                     result
                     (cons (car fragments) strs)
                     (and all-whitespace? (string-whitespace? (car fragments)))))
              (else
                (loop (cdr fragments)
                      (cons (car fragments)
                            (if all-whitespace?
                              result
                              (cons (ssax:string-concatenate/shared strs) result)))
                      '()
                      #t)))))))

    ;; CHARACTER AND TOKEN FUNCTIONS
    
    ;; The following functions either skip, or build and return tokens,
    ;; according to inclusion or delimiting semantics.  The list of
    ;; characters to expect, include, or to break at may vary from one
    ;; invocation of a function to another.  This allows the functions to
    ;; easily parse even context-sensitive languages.
    ;;
    ;; Exceptions are mentioned specifically.  The list of expected
    ;; characters (characters to skip until, or break-characters) may
    ;; include an EOF "character", which is coded as symbol *eof*
    ;;
    ;; The input stream to parse is specified as a PORT, which is the last argument.
     
    ;; Reads a character from the @3 and looks it up in the
    ;; @1 of expected characters.  If the read character was
    ;; found among expected, it is returned.  Otherwise, the
    ;; procedure writes a message using @2 as a comment and quits.
    (define (ssax:assert-current-char expected-chars comment port)
      (let ((c (read-char port)))
        (if (memv c expected-chars) c
          (error port "Wrong character " c
                 " (0x" (if (eof-object? c)
                          "*eof*"
                          (number->string (char->integer c) 16)) ") "
                 comment ". " expected-chars " expected"))))
    
    ;; Reads characters from the @2 and disregards them, as long as they
    ;; are mentioned in the @1.  The first character (which may be EOF)
    ;; peeked from the stream that is @emph{not} a member of the @1 is returned.
    (define (ssax:skip-while skip-chars port)
      (do ((c (peek-char port) (peek-char port)))
        ((not (memv c skip-chars)) c)
        (read-char port)))

    ;; STREAM TOKENIZERS
    
    ;; Note: since we can't tell offhand how large the token being read is
    ;; going to be, we make a guess, pre-allocate a string, and grow it by
    ;; quanta if necessary.  The quantum is always the length of the string
    ;; before it was extended the last time.  Thus the algorithm does a
    ;; Fibonacci-type extension, which has been proven optimal.
    ;;
    ;; Size 32 turns out to be fairly good, on average.  That policy is
    ;; good only when a Scheme system is multi-threaded with preemptive
    ;; scheduling, or when a Scheme system supports shared substrings.  In
    ;; all the other cases, it's better for ssax:init-buffer to return the
    ;; same static buffer.  ssax:next-token* functions return a copy (a
    ;; substring) of accumulated data, so the same buffer can be reused.
    ;; We shouldn't worry about an incoming token being too large:
    ;; ssax:next-token will use another chunk automatically.  Still, the
    ;; best size for the static buffer is to allow most of the tokens to
    ;; fit in.  Using a static buffer _dramatically_ reduces the amount of
    ;; produced garbage (e.g., during XML parsing).
    
    ;; Returns an initial buffer for `ssax:next-token*` procedures; may allocate a
    ;; new buffer at each invocation.
    (define (ssax:init-buffer)
      (make-string 32))

    ;;;(define ssax:init-buffer
    ;;;  (let ((buffer (make-string 512)))
    ;;;    (lambda () buffer)))
    
    ;; Skips any number of the prefix characters (members of the @1), if
    ;; any, and reads the sequence of characters up to (but not including)
    ;; a break character, one of the `break-chars`.
    ;;
    ;; The string of characters thus read is returned.  The break character
    ;; is left on the input stream.  `break-chars` may include the symbol `*eof*`;
    ;; otherwise, EOF is fatal, generating an error message including a
    ;; specified `comment`.
    (define (ssax:next-token prefix-skipped-chars break-chars comment port)
      (let outer ((buffer (ssax:init-buffer)) (filled-buffer-l '())
                                              (c (ssax:skip-while prefix-skipped-chars port)))
        (let ((curr-buf-len (string-length buffer)))
          (let loop ((i 0) (c c))
            (cond
              ((memv c break-chars)
               (if (null? filled-buffer-l) (string-copy buffer 0 i)
                 (ssax:string-concatenate-reverse filled-buffer-l buffer i)))
              ((eof-object? c)
               (if (memq '*eof* break-chars)	; was EOF expected?
                 (if (null? filled-buffer-l) (string-copy buffer 0 i)
                   (ssax:string-concatenate-reverse filled-buffer-l buffer i))
                 (error port "EOF while reading a token " comment)))
              ((>= i curr-buf-len)
               (outer (make-string curr-buf-len)
                      (cons buffer filled-buffer-l) c))
              (else
                (string-set! buffer i c)
                (read-char port)		; move to the next char
                (loop (+ 1 i) (peek-char port))))))))
    
    ;; will try to read an alphabetic token from the current input port,
    ;; and return it in lower case.
    (define (ssax:next-token-of incl-list/pred port)
      (let* ((buffer (ssax:init-buffer))
             (curr-buf-len (string-length buffer)))
        (if (procedure? incl-list/pred)
          (let outer ((buffer buffer) (filled-buffer-l '()))
            (let loop ((i 0))
              (if (>= i curr-buf-len)	; make sure we have space
                (outer (make-string curr-buf-len) (cons buffer filled-buffer-l))
                (let ((c (incl-list/pred (peek-char port))))
                  (if c
                    (begin
                      (string-set! buffer i c)
                      (read-char port) ; move to the next char
                      (loop (+ 1 i)))
                    ;; incl-list/pred decided it had had enough
                    (if (null? filled-buffer-l) (string-copy buffer 0 i)
                      (ssax:string-concatenate-reverse filled-buffer-l buffer i)))))))

          ;; incl-list/pred is a list of allowed characters
          (let outer ((buffer buffer) (filled-buffer-l '()))
            (let loop ((i 0))
              (if (>= i curr-buf-len)	; make sure we have space
                (outer (make-string curr-buf-len) (cons buffer filled-buffer-l))
                (let ((c (peek-char port)))
                  (cond
                    ((not (memv c incl-list/pred))
                     (if (null? filled-buffer-l) (string-copy buffer 0 i)
                       (ssax:string-concatenate-reverse filled-buffer-l buffer i)))
                    (else
                      (string-set! buffer i c)
                      (read-char port)	; move to the next char
                      (loop (+ 1 i))))))))
          )))
    
    ;; Reads @1 characters from the @2, and returns them in a string.  If
    ;; EOF is encountered before @1 characters are read, a shorter string
    ;; will be returned.
    (define (ssax:read-string len port)
      (define buffer (make-string len))
      (do ((idx 0 (+ 1 idx)))
        ((>= idx len) (string-copy buffer 0 idx))
        (let ((chr (read-char port)))
          (cond ((eof-object? chr)
                 (set! idx (+ -1 idx))
                 (set! len idx))
                (else (string-set! buffer idx chr))))))

    ;; DATA TYPES
    
    ;; TAG-KIND
    ;;	 a symbol 'START, 'END, 'PI, 'DECL, 'COMMENT, 'CDSECT or 'ENTITY-REF that identifies
    ;;	 a markup token
    ;;
    ;; UNRES-NAME
    ;;   a name (called GI in the XML Recommendation) as given in an xml document for a markup
    ;;   token: start-tag, PI target, attribute name. If a GI is an NCName, UNRES-NAME is this
    ;;   NCName converted into a Scheme symbol. If a GI is a QName, UNRES-NAME is a pair of
    ;;   symbols: (PREFIX . LOCALPART)
    ;;
    ;; RES-NAME
    ;;   An expanded name, a resolved version of an UNRES-NAME. For an element or an attribute
    ;;   name with a non-empty namespace URI, RES-NAME is a pair of symbols,
    ;;   (URI-SYMB . LOCALPART). Otherwise, it's a single symbol.
    ;;
    ;; ELEM-CONTENT-MODEL
    ;;   A symbol:
    ;;   ANY       - anything goes, expect an END tag.
    ;;   EMPTY-TAG - no content, and no END-tag is coming
    ;;   EMPTY     - no content, expect the END-tag as the next token
    ;;   PCDATA    - expect character data only, and no children elements
    ;;   MIXED
    ;;   ELEM-CONTENT
    ;;
    ;; URI-SYMB
    ;;	A symbol representing a namespace URI -- or other symbol chosen
    ;;	by the user to represent URI. In the former case,
    ;;	URI-SYMB is created by %-quoting of bad URI characters and
    ;;	converting the resulting string into a symbol.
    ;;
    ;; NAMESPACES
    ;;   A list representing namespaces in effect. An element of the list has one of the
    ;;   following forms:
    ;;	   (PREFIX URI-SYMB . URI-SYMB) or
    ;;	   (PREFIX USER-PREFIX . URI-SYMB)
    ;;	 USER-PREFIX is a symbol chosen by the user to represent the URI.
    ;;	 (#f USER-PREFIX . URI-SYMB)
    ;;     Specification of the user-chosen prefix and a URI-SYMBOL.
    ;;	 (*DEFAULT* USER-PREFIX . URI-SYMB)
    ;;     Declaration of the default namespace
    ;;	 (*DEFAULT* #f . #f)
    ;;     Un-declaration of the default namespace. This notation represents overriding of the
    ;;     previous declaration
    ;;   A NAMESPACES list may contain several elements for the same PREFIX.
    ;;   The one closest to the beginning of the list takes effect.
    ;;
    ;; ATTLIST
    ;;   An ordered collection of (NAME . VALUE) pairs, where NAME is
    ;;   a RES-NAME or an UNRES-NAME. The collection is an ADT
    ;;
    ;; STR-HANDLER
    ;;   A procedure of three arguments: STRING1 STRING2 SEED returning a new SEED
    ;;   The procedure is supposed to handle a chunk of character data
    ;;   STRING1 followed by a chunk of character data STRING2.
    ;;   STRING2 is a short string, often "\n" and even ""
    ;;
    ;; ENTITIES
    ;;   An assoc list of pairs:
    ;;     (named-entity-name . named-entity-body)
    ;;   where named-entity-name is a symbol under which the entity was declared,
    ;;   named-entity-body is either a string, or (for an external entity) a thunk that
    ;;   will return an input port (from which the entity can be read). named-entity-body
    ;;   may also be #f. This is an indication that a named-entity-name is currently being
    ;;   expanded. A reference to this named-entity-name will be an error: violation of the
    ;;   WFC nonrecursion.
    ;; 
    ;; XML-TOKEN -- a record
    ;;
    ;;   We define xml-token simply as a pair. Furthermore, xml-token-kind and xml-token-head
    ;;   can be defined as simple procedures.
    ;;  
    ;;   This record represents a markup, which is, according to the XML
    ;;   recommendation, "takes the form of start-tags, end-tags, empty-element tags,
    ;;   entity references, character references, comments, CDATA section delimiters,
    ;;   document type declarations, and processing instructions."
    ;;
    ;;    kind -- a TAG-KIND
    ;;	  head -- an UNRES-NAME. For xml-tokens of kinds 'COMMENT and 'CDSECT, the head is #f
    ;;
    ;;   For example,
    ;;     <P>   => kind='START, head='P
    ;;     </P>  => kind='END, head='P
    ;;     <BR/> => kind='EMPTY-EL, head='BR
    ;;	   <!DOCTYPE OMF ...> => kind='DECL, head='DOCTYPE
    ;;     <?xml version="1.0"?> => kind='PI, head='xml
    ;;     &my-ent; => kind = 'ENTITY-REF, head='my-ent
    ;;
    ;;   Character references are not represented by xml-tokens as these references
    ;;   are transparently resolved into the corresponding characters.
    ;;
    ;; XML-DECL -- a record
    ;; 
    ;; The record represents a datatype of an XML document: the list of declared elements and
    ;; their attributes, declared notations, list of replacement strings or loading procedures
    ;; for parsed general entities, etc. Normally an xml-decl record is created from a DTD or
    ;; an XML Schema, although it can be created and filled in in many other ways (e.g.,
    ;; loaded from a file).
    ;;
    ;; elems: an (assoc) list of decl-elem or #f. The latter instructs the parser to do no
    ;;        validation of elements and attributes.
    ;;
    ;; decl-elem: declaration of one element:
    ;;   (elem-name elem-content decl-attrs)
    ;;   elem-name is an UNRES-NAME for the element.
    ;;   elem-content is an ELEM-CONTENT-MODEL.
    ;;   decl-attrs is an ATTLIST, of (ATTR-NAME . VALUE) associations
    ;;   !!!This element can declare a user procedure to handle parsing of an
    ;;   element (e.g., to do a custom validation, or to build a hash of
    ;;   IDs as they're encountered).
    ;;
    ;; decl-attr: an element of an ATTLIST, declaration of one attribute
    ;;   (attr-name content-type use-type default-value)
    ;;   attr-name is an UNRES-NAME for the declared attribute
    ;;   content-type is a symbol: CDATA, NMTOKEN, NMTOKENS, ...
    ;;   or a list of strings for the enumerated type.
    ;;   use-type is a symbol: REQUIRED, IMPLIED, FIXED
    ;;   default-value is a string for the default value, or #f if not given.
    
    ;; see a function make-empty-xml-decl to make a XML declaration entry
    ;; suitable for a non-validating parsing.

    ;; We define xml-token simply as a pair.
    
    (define (make-xml-token kind head) (cons kind head))
    (define xml-token? pair?)
    (define xml-token-kind car)
    (define xml-token-head cdr)

    ;; LOW-LEVEL PARSERS AND SCANNERS
    ;;
    ;; These procedures deal with primitive lexical units (Names, whitespaces, tags)
    ;; and with pieces of more generic productions. Most of these parsers
    ;; must be called in appropriate context. For example, ssax:complete-start-tag
    ;; must be called only when the start-tag has been detected and its GI
    ;; has been read.

    (define char-return (integer->char 13))
    (define ssax:S-chars (map integer->char '(32 10 9 13)))

    ;; Skip the S (whitespace) production as defined by
    ;; [3] S ::= (#x20 | #x9 | #xD | #xA)
    ;; The procedure returns the first not-whitespace character it
    ;; encounters while scanning the PORT. This character is left
    ;; on the input stream.
    ;; 
    ;; `ssax:skip-S` returns the first not-whitespace character it encounters while
    ;; scanning `port`.  This character is left on the input stream.
    (define (ssax:skip-S port)
      (ssax:skip-while ssax:S-chars port))

    ;; Check to see if a-char may start a NCName
    (define (ssax:ncname-starting-char? a-char)
      (and (char? a-char)
           (or (char-alphabetic? a-char)
               (char=? #\_ a-char))))

    ;; Read a Name lexem and return it as string
    ;; 
    ;; [4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar | Extender
    ;; [5] Name     ::= (Letter | '_' | ':') (NameChar)*
    ;;
    ;; This code supports the XML Namespace Recommendation REC-xml-names,
    ;; which modifies the above productions as follows:
    ;;
    ;; [4] NCNameChar ::= Letter | Digit | '.' | '-' | '_' | CombiningChar | Extender
    ;; [5] NCName ::= (Letter | '_') (NCNameChar)*
    ;; 
    ;; As the Rec-xml-names says,
    ;; "An XML document conforms to this specification if all other tokens
    ;; [other than element types and attribute names] in the document which
    ;; are required, for XML conformance, to match the XML production for
    ;; Name, match this specification's production for NCName."
    ;; Element types and attribute names must match the production QName,
    ;; defined below.
    ;; 
    ;; Read a NCName starting from the current position in the PORT and
    ;; return it as a symbol.
    (define (ssax:read-NCName port)
      (let ((first-char (peek-char port)))
        (or (ssax:ncname-starting-char? first-char)
            (error port "XMLNS [4] for '" first-char "'")))
      (string->symbol (ssax:next-token-of (lambda (c)
                                            (cond ((eof-object? c) #f)
                                                  ((char-alphabetic? c) c)
                                                  ((string-index "0123456789.-_" c) c)
                                                  (else #f)))
                                          port)))
    
    ;; Read a (namespace-) Qualified Name, QName, from the current
    ;; position in the PORT.
    ;; From REC-xml-names:
    ;;	[6] QName     ::= (Prefix ':')? LocalPart
    ;;	[7] Prefix    ::= NCName
    ;;	[8] LocalPart ::= NCName
    ;; Return: an UNRES-NAME
    (define (ssax:read-QName port)
      (let ((prefix-or-localpart (ssax:read-NCName port)))
        (case (peek-char port)
          ((#\:)
           (read-char port)
           (cons prefix-or-localpart (ssax:read-NCName port)))
          (else prefix-or-localpart))))

    ;;The prefix of the pre-defined XML namespace
    (define ssax:Prefix-XML (string->symbol "xml"))

    ;;An UNRES-NAME that is postulated to be larger than anything that can
    ;;occur in a well-formed XML document.  ssax:name-compare enforces
    ;;this postulate.
    (define ssax:largest-unres-name (cons (string->symbol "#LARGEST-SYMBOL")
                                          (string->symbol "#LARGEST-SYMBOL")))

    ;; Compare one RES-NAME or an UNRES-NAME with the other.
    ;; Return a symbol '<, '>, or '= depending on the result of
    ;; the comparison.
    ;; Names without PREFIX are always smaller than those with the PREFIX.
    (define ssax:name-compare
      (letrec ((symbol-compare
                 (lambda (symb1 symb2)
                   (cond
                     ((eq? symb1 symb2) '=)
                     ((string<? (symbol->string symb1) (symbol->string symb2))
                      '<)
                     (else '>)))))
        (lambda (name1 name2)
          (cond
            ((symbol? name1) (if (symbol? name2) (symbol-compare name1 name2)
                               '<))
            ((symbol? name2) '>)
            ((eq? name2 ssax:largest-unres-name) '<)
            ((eq? name1 ssax:largest-unres-name) '>)
            ((eq? (car name1) (car name2))	; prefixes the same
             (symbol-compare (cdr name1) (cdr name2)))
            (else (symbol-compare (car name1) (car name2)))))))

    ;; procedure: ssax:read-markup-token PORT
    ;; This procedure starts parsing of a markup token. The current position
    ;; in the stream must be #\<. This procedure scans enough of the input stream
    ;; to figure out what kind of a markup token it is seeing. The procedure returns
    ;; an xml-token structure describing the token. Note, generally reading
    ;; of the current markup is not finished! In particular, no attributes of
    ;; the start-tag token are scanned.
    ;;
    ;; Here's a detailed break out of the return values and the position in the PORT
    ;; when that particular value is returned:
    ;;	PI-token:	only PI-target is read.
    ;;			To finish the Processing Instruction and disregard it,
    ;;			call ssax:skip-pi. ssax-read-attributes may be useful
    ;;			as well (for PIs whose content is attribute-value
    ;;			pairs)
    ;;	END-token:	The end tag is read completely; the current position
    ;;			is right after the terminating #\> character.	
    ;;	COMMENT		is read and skipped completely. The current position
    ;;			is right after "-->" that terminates the comment.
    ;;	CDSECT		The current position is right after "<!CDATA["
    ;;			Use ssax:read-cdata-body to read the rest.
    ;;	DECL		We have read the keyword (the one that follows "<!")
    ;;			identifying this declaration markup. The current
    ;;			position is after the keyword (usually a
    ;;			whitespace character)
    ;;
    ;;	START-token	We have read the keyword (GI) of this start tag.
    ;;			No attributes are scanned yet. We don't know if this
    ;;			tag has an empty content either.
    ;;			Use ssax:complete-start-tag to finish parsing of
    ;;			the token.
    (define ssax:read-markup-token ; procedure ssax:read-markup-token port
      (let ()
        ;; we have read "<!-".  Skip through the rest of the comment
        ;; Return the 'COMMENT token as an indication we saw a comment
        ;; and skipped it.
        (define (skip-comment port)
          (ssax:assert-current-char '(#\-) "XML [15], second dash" port)
          (if (not (find-string-from-port? "-->" port))
            (error port "XML [15], no -->"))
          (make-xml-token 'COMMENT #f))
        ;; we have read "<![" that must begin a CDATA section
        (define (read-cdata port)
          (define cdstr (ssax:read-string 6 port))
          (if (not (string=? "CDATA[" cdstr))
            (error "expected 'CDATA[' but read " cdstr))
          (make-xml-token 'CDSECT #f))

        (lambda (port)
          (ssax:assert-current-char '(#\<) "start of the token" port)
          (case (peek-char port)
            ((#\/) (read-char port)
                   (let ((val (make-xml-token 'END (ssax:read-QName port))))
                     (ssax:skip-S port)
                     (ssax:assert-current-char '(#\>) "XML [42]" port)
                     val))
            ((#\?) (read-char port) (make-xml-token 'PI (ssax:read-NCName port)))
            ((#\!)
             (read-char port)
             (case (peek-char port)
               ((#\-) (read-char port) (skip-comment port))
               ((#\[) (read-char port) (read-cdata port))
               (else (make-xml-token 'DECL (ssax:read-NCName port)))))
            (else (make-xml-token 'START (ssax:read-QName port)))))))

    ;; The current position is inside a PI. Skip till the rest of the PI
    (define (ssax:skip-pi port)
      (if (not (find-string-from-port? "?>" port))
        (error port "Failed to find ?> terminating the PI")))

    ;; procedure: ssax:read-pi-body-as-string PORT
    ;; The current position is right after reading the PITarget. We read the
    ;; body of PI and return is as a string. The port will point to the
    ;; character right after '?>' combination that terminates PI.
    ;; [16] PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
    (define (ssax:read-pi-body-as-string port)
      (ssax:skip-S port)		    ; skip WS after the PI target name
      (ssax:string-concatenate/shared
        (let loop ()
          (let ((pi-fragment
                  (ssax:next-token '() '(#\?) "reading PI content" port)))
            (read-char port)
            (if (eqv? #\> (peek-char port))
              (begin
                (read-char port)
                (cons pi-fragment '()))
              (cons* pi-fragment "?" (loop)))))))

    ;; procedure: ssax:skip-internal-dtd PORT
    ;; The current pos in the port is inside an internal DTD subset
    ;; (e.g., after reading #\[ that begins an internal DTD subset)
    ;; Skip until the "]>" combination that terminates this DTD
    (define (ssax:skip-internal-dtd port)
      (ssax-warn port "Internal DTD subset is not currently handled ")
      (if (not (find-string-from-port? "]>" port))
        (error port "Failed to find ]> terminating the internal DTD subset")))

    ;; procedure+: ssax:read-cdata-body PORT STR-HANDLER SEED
    ;;
    ;; This procedure must be called after we have read a string "<![CDATA["
    ;; that begins a CDATA section. The current position must be the first
    ;; position of the CDATA body. This function reads _lines_ of the CDATA
    ;; body and passes them to a STR-HANDLER, a character data consumer.
    ;;
    ;; The str-handler is a STR-HANDLER, a procedure STRING1 STRING2 SEED.
    ;; The first STRING1 argument to STR-HANDLER never contains a newline.
    ;; The second STRING2 argument often will. On the first invocation of
    ;; the STR-HANDLER, the seed is the one passed to ssax:read-cdata-body
    ;; as the third argument. The result of this first invocation will be
    ;; passed as the seed argument to the second invocation of the line
    ;; consumer, and so on. The result of the last invocation of the
    ;; STR-HANDLER is returned by the ssax:read-cdata-body.  Note a
    ;; similarity to the fundamental 'fold' iterator.
    ;;
    ;; Within a CDATA section all characters are taken at their face value,
    ;; with only three exceptions:
    ;;	CR, LF, and CRLF are treated as line delimiters, and passed
    ;;	as a single #\newline to the STR-HANDLER
    ;;	"]]>" combination is the end of the CDATA section.
    ;;	&gt; is treated as an embedded #\> character
    ;; Note, &lt; and &amp; are not specially recognized (and are not expanded)!
    (define ssax:read-cdata-body
      (let ((cdata-delimiters (list char-return #\newline #\] #\&)))
        (lambda (port str-handler seed)
          (let loop ((seed seed))
            (let ((fragment (ssax:next-token '() cdata-delimiters "reading CDATA" port)))
              ;; that is, we're reading the char after the 'fragment'
              (case (read-char port)
                ((#\newline) (loop (str-handler fragment (string #\newline) seed)))
                ((#\])
                 (if (not (eqv? (peek-char port) #\]))
                   (loop (str-handler fragment "]" seed))
                   (let check-after-second-braket
                     ((seed (if (string-null? fragment) seed
                              (str-handler fragment "" seed))))
                     (read-char port)
                     (case (peek-char port) ; after the second bracket
                       ((#\>) (read-char port) seed) ; we have read "]]>"
                       ((#\]) (check-after-second-braket
                                (str-handler "]" "" seed)))
                       (else (loop (str-handler "]]" "" seed)))))))
                ((#\&)   ; Note that #\& within CDATA may stand for itself
                 (let ((ent-ref  ; it does not have to start an entity ref
                         (ssax:next-token-of
                           (lambda (c)
                             (and (not (eof-object? c)) (char-alphabetic? c) c))
                           port)))
                   (cond		   ; replace "&gt;" with #\>
                     ((and (string=? "gt" ent-ref) (eqv? (peek-char port) #\;))
                      (read-char port)
                      (loop (str-handler fragment ">" seed)))
                     (else
                       (loop
                         (str-handler ent-ref ""
                                      (str-handler fragment "&" seed)))))))
                (else ; Must be CR: if the next char is #\newline, skip it
                  (if (eqv? (peek-char port) #\newline) (read-char port))
                  (loop (str-handler fragment (string #\newline) seed)))
                ))))))

    ;; procedure+: ssax:read-char-ref PORT
    ;;
    ;; [66]  CharRef ::=  '&#' [0-9]+ ';' 
    ;;                  | '&#x' [0-9a-fA-F]+ ';'
    ;;
    ;; This procedure must be called after we we have read "&#" 
    ;; that introduces a char reference.
    ;; The procedure reads this reference and returns the corresponding char
    ;; The current position in PORT will be after ";" that terminates
    ;; the char reference
    ;; Faults detected:
    ;;	WFC: XML-Spec.html#wf-Legalchar
    ;;
    ;; According to Section "4.1 Character and Entity References"
    ;; of the XML Recommendation:
    ;;  "[Definition: A character reference refers to a specific character
    ;;   in the ISO/IEC 10646 character set, for example one not directly
    ;;   accessible from available input devices.]"
    ;; Therefore, we use a ucscode->char function to convert a character
    ;; code into the character -- *regardless* of the current character
    ;; encoding of the input stream.
    (define (ssax:read-char-ref port)
      (let* ((base (cond ((eqv? (peek-char port) #\x) (read-char port) 16)
                         (else 10)))
             (name (ssax:next-token '() '(#\;) "XML [66]" port))
             (char-code (string->number name base)))
        (read-char port)		       ; read the terminating #\; char
        (if (integer? char-code) (integer->char char-code)
          (error port "[wf-Legalchar] broken for '" name "'"))))

    (define ssax:predefined-parsed-entities
      `(
        (,(string->symbol "amp") . "&")
        (,(string->symbol "lt") . "<")
        (,(string->symbol "gt") . ">")
        (,(string->symbol "apos") . "'")
        (,(string->symbol "quot") . "\"")
        ))

    ;; procedure+: ssax:handle-parsed-entity PORT NAME ENTITIES 
    ;;		CONTENT-HANDLER STR-HANDLER SEED
    ;; 
    ;; Expand and handle a parsed-entity reference
    ;; port - a PORT
    ;; name - the name of the parsed entity to expand, a symbol
    ;; entities - see ENTITIES
    ;; content-handler -- procedure PORT ENTITIES SEED
    ;;	that is supposed to return a SEED
    ;; str-handler - a STR-HANDLER. It is called if the entity in question
    ;; turns out to be a pre-declared entity
    ;;
    ;; The result is the one returned by CONTENT-HANDLER or STR-HANDLER
    ;; Faults detected:
    ;;	WFC: XML-Spec.html#wf-entdeclared
    ;;	WFC: XML-Spec.html#norecursion
    (define (ssax:handle-parsed-entity port name entities content-handler str-handler seed)
      (cond		    ; First we check the list of the declared entities
        ((assq name entities) =>
                              (lambda (decl-entity)
                                (let ((ent-body (cdr decl-entity)) ; mark the list to prevent recursion
                                      (new-entities (cons (cons name #f) entities)))
                                  (cond
                                    ((string? ent-body)
                                     (call-with-port (open-input-string ent-body)
                                                     (lambda (port) (content-handler port new-entities seed))))
                                    ((procedure? ent-body)
                                     (let ((port (ent-body)))
                                       (define val (content-handler port new-entities seed))
                                       (close-input-port port)
                                       val))
                                    (else
                                      (error port "[norecursion] broken for " name))))))
        ((assq name ssax:predefined-parsed-entities)
         => (lambda (decl-entity)
              (str-handler (cdr decl-entity) "" seed)))
        (else (error port "[wf-entdeclared] broken for " name))))

    ;; THE ATTLIST ABSTRACT DATA TYPE

    ;; Currently is implemented as an assoc list sorted in the ascending
    ;; order of NAMES.

    (define attlist-fold fold)
    (define attlist-null? null?)
    (define attlist->alist identity)
    (define (make-empty-attlist) '())

    ;; procedure: attlist-add ATTLIST NAME-VALUE-PAIR
    ;; Add a name-value pair to the existing attlist preserving the order
    ;; Return the new list, in the sorted ascending order.
    ;; Return #f if a pair with the same name already exists in the attlist
    (define (attlist-add attlist name-value)
      (if (null? attlist) (cons name-value attlist)
        (case (ssax:name-compare (car name-value) (caar attlist))
          ((=) #f)
          ((<) (cons name-value attlist))
          (else (cons (car attlist) (attlist-add (cdr attlist) name-value)))
          )))

    ;; procedure: attlist-remove-top ATTLIST
    ;; Given an non-null attlist, return a pair of values: the top and the rest
    (define (attlist-remove-top attlist)
      (values (car attlist) (cdr attlist)))
    
    ;; procedure+:	ssax-read-attributes PORT ENTITIES
    ;;
    ;; This procedure reads and parses a production Attribute*
    ;; [41] Attribute ::= Name Eq AttValue
    ;; [10] AttValue  ::=  '"' ([^<&"] | Reference)* '"' 
    ;;                  | "'" ([^<&'] | Reference)* "'"
    ;; [25] Eq        ::= S? '=' S?
    ;; 
    ;; The procedure returns an ATTLIST, of Name (as UNRES-NAME), Value (as string)
    ;; pairs. The current character on the PORT is a non-whitespace character
    ;; that is not an ncname-starting character.
    ;;
    ;; Note the following rules to keep in mind when reading an 'AttValue'
    ;; "Before the value of an attribute is passed to the application
    ;; or checked for validity, the XML processor must normalize it as follows:
    ;; 
    ;; - a character reference is processed by appending the referenced
    ;;   character to the attribute value 
    ;; - an entity reference is processed by recursively processing the
    ;;   replacement text of the entity [see ENTITIES]
    ;;   [named entities amp lt gt quot apos are assumed pre-declared]
    ;; - a whitespace character (#x20, #xD, #xA, #x9) is processed by appending #x20
    ;;   to the normalized value, except that only a single #x20 is appended for a
    ;;   "#xD#xA" sequence that is part of an external parsed entity or the
    ;;   literal entity value of an internal parsed entity 
    ;; - other characters are processed by appending them to the normalized value "
    ;; 
    ;; Faults detected:
    ;;	WFC: XML-Spec.html#CleanAttrVals
    ;;	WFC: XML-Spec.html#uniqattspec
    (define ssax-read-attributes	  ; ssax-read-attributes port entities
      (let ((value-delimeters (append '(#\< #\&) ssax:S-chars)))
        ;; Read the AttValue from the PORT up to the delimiter (which can
        ;; be a single or double-quote character, or even a symbol *eof*).
        ;; 'prev-fragments' is the list of string fragments, accumulated
        ;; so far, in reverse order.  Return the list of fragments with
        ;; newly read fragments prepended.
        (define (read-attrib-value delimiter port entities prev-fragments)
          (let* ((new-fragments
                   (cons (ssax:next-token '() (cons delimiter value-delimeters)
                                          "XML [10]" port)
                         prev-fragments))
                 (cterm (read-char port)))
            (cond
              ((or (eof-object? cterm) (eqv? cterm delimiter))
               new-fragments)
              ((eqv? cterm char-return)	; treat a CR and CRLF as a LF
               (if (eqv? (peek-char port) #\newline) (read-char port))
               (read-attrib-value delimiter port entities
                                  (cons " " new-fragments)))
              ((memv cterm ssax:S-chars)
               (read-attrib-value delimiter port entities
                                  (cons " " new-fragments)))
              ((eqv? cterm #\&)
               (cond
                 ((eqv? (peek-char port) #\#)
                  (read-char port)
                  (read-attrib-value delimiter port entities
                                     (cons (string (ssax:read-char-ref port)) new-fragments)))
                 (else
                   (read-attrib-value delimiter port entities
                                      (read-named-entity port entities new-fragments)))))
              (else (error port "[CleanAttrVals] broken")))))

        ;; we have read "&" that introduces a named entity reference.
        ;; read this reference and return the result of normalizing of the
        ;; corresponding string (that is, read-attrib-value is applied to
        ;; the replacement text of the entity).  The current position will
        ;; be after ";" that terminates the entity reference
        (define (read-named-entity port entities fragments)
          (let ((name (ssax:read-NCName port)))
            (ssax:assert-current-char '(#\;) "XML [68]" port)
            (ssax:handle-parsed-entity port name entities
                                       (lambda (port entities fragments)
                                         (read-attrib-value '*eof* port entities fragments))
                                       (lambda (str1 str2 fragments)
                                         (if (equal? "" str2) (cons str1 fragments)
                                           (cons* str2 str1 fragments)))
                                       fragments)))

        (lambda (port entities)
          (let loop ((attr-list (make-empty-attlist)))
            (if (not (ssax:ncname-starting-char? (ssax:skip-S port))) attr-list
              (let ((name (ssax:read-QName port)))
                (ssax:skip-S port)
                (ssax:assert-current-char '(#\=) "XML [25]" port)
                (ssax:skip-S port)
                (let ((delimiter
                        (ssax:assert-current-char '(#\' #\" ) "XML [10]" port)))
                  (loop
                    (or (attlist-add attr-list
                                     (cons name
                                           (ssax:string-concatenate-reverse/shared
                                             (read-attrib-value delimiter port entities
                                                                '()))))
                        (error port "[uniqattspec] broken for " name))))))))
        ))
    
    ;; ssax-resolve-name PORT UNRES-NAME NAMESPACES apply-default-ns?
    ;;
    ;; Convert an UNRES-NAME to a RES-NAME given the appropriate NAMESPACES
    ;; declarations.
    ;; the last parameter apply-default-ns? determines if the default
    ;; namespace applies (for instance, it does not for attribute names)
    ;;
    ;; Per REC-xml-names/#nsc-NSDeclared, "xml" prefix is considered pre-declared
    ;; and bound to the namespace name "http://www.w3.org/XML/1998/namespace".
    ;;
    ;; This procedure tests for the namespace constraints:
    ;; http://www.w3.org/TR/REC-xml-names/#nsc-NSDeclared
    (define (ssax-resolve-name port unres-name namespaces apply-default-ns?)
      (cond
        ((pair? unres-name)			; it's a QNAME
         (cons
           (cond
             ((assq (car unres-name) namespaces) => cadr)
             ((eq? (car unres-name) ssax:Prefix-XML) ssax:Prefix-XML)
             (else
               (error port "[nsc-NSDeclared] broken; prefix " (car unres-name))))
           (cdr unres-name)))
        (apply-default-ns?	      ; Do apply the default namespace, if any
          (let ((default-ns (assq '*DEFAULT* namespaces)))
            (if (and default-ns (cadr default-ns))
              (cons (cadr default-ns) unres-name)
              unres-name)))		       ; no default namespace declared
        (else unres-name)))	       ; no prefix, don't apply the default-ns


    ;;Procedure: ssax:uri-string->symbol URI-STR
    ;;Convert a URI-STR to an appropriate symbol
    (define ssax:uri-string->symbol string->symbol)
    
    ;; procedure+: ssax:complete-start-tag TAG PORT ELEMS ENTITIES NAMESPACES
    ;; 
    ;; This procedure is to complete parsing of a start-tag markup. The procedure must be
    ;; called after the start tag token has been read. TAG is an UNRES-NAME. ELEMS is an
    ;; instance of xml-decl::elems; it can be #f to tell the function to do _no_ validation
    ;; of elements and their attributes.
    ;;
    ;; This procedure returns several values:
    ;;  ELEM-GI: a RES-NAME.
    ;;  ATTRIBUTES: element's attributes, an ATTLIST of (RES-NAME . STRING)
    ;;	pairs. The list does NOT include xmlns attributes.
    ;;  NAMESPACES: the input list of namespaces amended with namespace
    ;;	(re-)declarations contained within the start-tag under parsing
    ;;  ELEM-CONTENT-MODEL
    ;; 
    ;; On exit, the current position in PORT will be the first character after
    ;; #\> that terminates the start-tag markup.
    ;;
    ;; Faults detected:
    ;;	VC: XML-Spec.html#enum 
    ;;	VC: XML-Spec.html#RequiredAttr
    ;;	VC: XML-Spec.html#FixedAttr
    ;;	VC: XML-Spec.html#ValueType
    ;;	WFC: XML-Spec.html#uniqattspec (after namespaces prefixes are resolved)
    ;;	VC: XML-Spec.html#elementvalid 
    ;;	WFC: REC-xml-names/#dt-NSName
    ;; 
    ;; Note, although XML Recommendation does not explicitly say it, xmlns and xmlns:
    ;; attributes don't have to be declared (although they can be declared, to specify
    ;; their default value)
    ;; 
    ;; Procedure:  ssax:complete-start-tag tag-head port elems entities namespaces
    (define ssax:complete-start-tag

      (let ((xmlns (string->symbol "xmlns"))
            (largest-dummy-decl-attr (list ssax:largest-unres-name #f #f #f)))

        ;; Scan through the attlist and validate it, against decl-attrs
        ;; Return an assoc list with added fixed or implied attrs.
        ;; Note that both attlist and decl-attrs are ATTLISTs, and therefore,
        ;; sorted
        (define (validate-attrs port attlist decl-attrs)

          ;; Check to see decl-attr is not of use type REQUIRED.  Add
          ;; the association with the default value, if any declared
          (define (add-default-decl decl-attr result)
            (let*-values
              (((attr-name content-type use-type default-value)
                (apply values decl-attr)))
              (and (eq? use-type 'REQUIRED)
                   (error port "[RequiredAttr] broken for" attr-name))
              (if default-value
                (cons (cons attr-name default-value) result)
                result)))

          (let loop ((attlist attlist) (decl-attrs decl-attrs) (result '()))
            (if (attlist-null? attlist)
              (attlist-fold add-default-decl result decl-attrs)
              (let*-values
                (((attr attr-others)
                  (attlist-remove-top attlist))
                 ((decl-attr other-decls)
                  (if (attlist-null? decl-attrs)
                    (values largest-dummy-decl-attr decl-attrs)
                    (attlist-remove-top decl-attrs)))
                 )
                (case (ssax:name-compare (car attr) (car decl-attr))
                  ((<)
                   (if (or (eq? xmlns (car attr))
                           (and (pair? (car attr)) (eq? xmlns (caar attr))))
                     (loop attr-others decl-attrs (cons attr result))
                     (error port "[ValueType] broken for " attr)))
                  ((>)
                   (loop attlist other-decls
                         (add-default-decl decl-attr result)))
                  (else ; matched occurrence of an attr with its declaration
                    (let*-values
                      (((attr-name content-type use-type default-value)
                        (apply values decl-attr)))
                      ;; Run some tests on the content of the attribute
                      (cond
                        ((eq? use-type 'FIXED)
                         (or (equal? (cdr attr) default-value)
                             (error port "[FixedAttr] broken for " attr-name)))
                        ((eq? content-type 'CDATA) #t) ; everything goes
                        ((pair? content-type)
                         (or (member (cdr attr) content-type)
                             (error port "[enum] broken for " attr-name "="
                                    (cdr attr))))
                        (else
                          (ssax-warn port "declared content type " content-type
                                     " not verified yet")))
                      (loop attr-others other-decls (cons attr result)))))
                ))))
       
        ;; Add a new namespace declaration to namespaces.
        ;; First we convert the uri-str to a uri-symbol and search namespaces for
        ;; an association (_ user-prefix . uri-symbol).
        ;; If found, we return the argument namespaces with an association
        ;; (prefix user-prefix . uri-symbol) prepended.
        ;; Otherwise, we prepend (prefix uri-symbol . uri-symbol)
        (define (add-ns port prefix uri-str namespaces)
          (and (equal? "" uri-str)
               (error port "[dt-NSName] broken for " prefix))
          (let ((uri-symbol (ssax:uri-string->symbol uri-str)))
            (let loop ((nss namespaces))
              (cond
                ((null? nss)
                 (cons (cons* prefix uri-symbol uri-symbol) namespaces))
                ((eq? uri-symbol (cddar nss))
                 (cons (cons* prefix (cadar nss) uri-symbol) namespaces))
                (else (loop (cdr nss)))))))

        ;; partition attrs into proper attrs and new namespace declarations
        ;; return two values: proper attrs and the updated namespace declarations
        (define (adjust-namespace-decl port attrs namespaces)
          (let loop ((attrs attrs) (proper-attrs '()) (namespaces namespaces))
            (cond
              ((null? attrs) (values proper-attrs namespaces))
              ((eq? xmlns (caar attrs))  ; re-decl of the default namespace
               (loop (cdr attrs) proper-attrs
                     (if (equal? "" (cdar attrs)) ; un-decl of the default ns
                       (cons (cons* '*DEFAULT* #f #f) namespaces)
                       (add-ns port '*DEFAULT* (cdar attrs) namespaces))))
              ((and (pair? (caar attrs)) (eq? xmlns (caaar attrs)))
               (loop (cdr attrs) proper-attrs
                     (add-ns port (cdaar attrs) (cdar attrs) namespaces)))
              (else
                (loop (cdr attrs) (cons (car attrs) proper-attrs) namespaces)))))

        ;; The body of the function
        (lambda (tag-head port elems entities namespaces)
          (let*-values
            (((attlist) (ssax-read-attributes port entities))
             ((empty-el-tag?)
              (begin
                (ssax:skip-S port)
                (and
                  (eqv? #\/
                        (ssax:assert-current-char '(#\> #\/) "XML [40], XML [44], no '>'" port))
                  (ssax:assert-current-char '(#\>) "XML [44], no '>'" port))))
             ((elem-content decl-attrs)	; see xml-decl for their type
              (if elems			; elements declared: validate!
                (cond
                  ((assoc tag-head elems) =>
                                          (lambda (decl-elem)  ; of type xml-decl::decl-elem
                                            (values
                                              (if empty-el-tag? 'EMPTY-TAG (cadr decl-elem))
                                              (caddr decl-elem))))
                  (else
                    (error port "[elementvalid] broken, no decl for " tag-head)))
                (values			; non-validating parsing
                  (if empty-el-tag? 'EMPTY-TAG 'ANY)
                  #f)			; no attributes declared
                ))
             ((merged-attrs) (if decl-attrs (validate-attrs port attlist decl-attrs)
                               (attlist->alist attlist)))
             ((proper-attrs namespaces)
              (adjust-namespace-decl port merged-attrs namespaces))
             )
            ;; build the return value
            (values
              (ssax-resolve-name port tag-head namespaces #t)
              (fold-right
                (lambda (name-value attlist)
                  (or
                    (attlist-add attlist
                                 (cons (ssax-resolve-name port (car name-value) namespaces #f)
                                       (cdr name-value)))
                    (error port "[uniqattspec] after NS expansion broken for "
                           name-value)))
                (make-empty-attlist)
                proper-attrs)
              namespaces
              elem-content)))))

    ;; procedure+: ssax-read-external-id PORT
    ;;
    ;; This procedure parses an ExternalID production:
    ;; [75] ExternalID    ::= 'SYSTEM' S SystemLiteral
    ;;		                  | 'PUBLIC' S PubidLiteral S SystemLiteral
    ;; [11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'") 
    ;; [12] PubidLiteral  ::=  '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
    ;; [13] PubidChar     ::=  #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
    ;;
    ;; This procedure is supposed to be called when an ExternalID is expected; that is, the
    ;; current character must be either #\S or #\P that start correspondingly a SYSTEM or
    ;; PUBLIC token. This procedure returns the SystemLiteral as a string. A PubidLiteral
    ;; is disregarded if present.
    (define (ssax-read-external-id port)
      (let ((discriminator (ssax:read-NCName port)))
        (ssax:assert-current-char ssax:S-chars "space after SYSTEM or PUBLIC" port)
        (ssax:skip-S port)
        (let ((delimiter
                (ssax:assert-current-char '(#\' #\" ) "XML [11], XML [12]" port)))
          (cond
            ((eq? discriminator (string->symbol "SYSTEM"))
             (let ((val (ssax:next-token '() (list delimiter) "XML [11]" port)))
               (read-char port)		; reading the closing delim
               val))
            ((eq? discriminator (string->symbol "PUBLIC"))
             (let loop ((c (read-char port)))
               (cond
                 ((eqv? c delimiter) c)
                 ((eof-object? c)
                  (error port "Unexpected EOF while skipping until " delimiter))
                 (else (loop (read-char port)))))
             (ssax:assert-current-char ssax:S-chars "space after PubidLiteral" port)
             (ssax:skip-S port)
             (let* ((delimiter
                      (ssax:assert-current-char '(#\' #\" ) "XML [11]" port))
                    (systemid
                      (ssax:next-token '() (list delimiter) "XML [11]" port)))
               (read-char port)		; reading the closing delim
               systemid))
            (else
              (error port "XML [75], " discriminator
                     " rather than SYSTEM or PUBLIC"))))))


    ;; MID-LEVEL PARSERS AND SCANNERS
    
    ;; The following procedures parse productions corresponding to the whole (document) entity
    ;; or its higher-level pieces (prolog, root element, etc).
    ;; 
    ;; Scan the Misc production in the context
    ;; [1]  document ::=  prolog element Misc*
    ;; [22] prolog ::= XMLDecl? Misc* (doctypedec l Misc*)?
    ;; [27] Misc ::= Comment | PI |  S
    ;; 
    ;; The following function should be called in the prolog or epilog contexts.
    ;; In these contexts, whitespaces are completely ignored.
    ;; The return value from ssax-scan-misc is either a PI-token,
    ;; a DECL-token, a START token, or EOF. Comments are ignored and not reported.
    (define (ssax-scan-misc port)
      (let loop ((c (ssax:skip-S port)))
        (cond
          ((eof-object? c) c)
          ((not (char=? c #\<))
           (error port "XML [22], char '" c "' unexpected"))
          (else
            (let ((token (ssax:read-markup-token port)))
              (case (xml-token-kind token)
                ((COMMENT) (loop (ssax:skip-S port)))
                ((PI DECL START) token)
                (else
                  (error port "XML [22], unexpected token of kind "
                         (xml-token-kind token)
                         ))))))))
    
    ;; procedure+: ssax-read-char-data PORT EXPECT-EOF? STR-HANDLER SEED
    ;;
    ;; This procedure is to read the character content of an XML document
    ;; or an XML element.
    ;; [43] content ::= (element | CharData | Reference | CDSect | PI | Comment)*
    ;; To be more precise, the procedure reads CharData, expands CDSect
    ;; and character entities, and skips comments. The procedure stops
    ;; at a named reference, EOF, at the beginning of a PI or a start/end tag.
    ;;
    ;; port
    ;;	a PORT to read
    ;; expect-eof?
    ;;	a boolean indicating if EOF is normal, i.e., the character
    ;;	data may be terminated by the EOF. EOF is normal
    ;;	while processing a parsed entity.
    ;; str-handler
    ;;	a STR-HANDLER
    ;; seed
    ;;	an argument passed to the first invocation of STR-HANDLER.
    ;;
    ;; The procedure returns two results: SEED and TOKEN.
    ;; The SEED is the result of the last invocation of STR-HANDLER, or the original seed
    ;; if STR-HANDLER was never called.
    ;;
    ;; TOKEN can be either an eof-object (this can happen only if expect-eof? was #t), or:
    ;;     - an xml-token describing a START tag or an END-tag;
    ;;	For a start token, the caller has to finish reading it.
    ;;     - an xml-token describing the beginning of a PI. It's up to an
    ;;	application to read or skip through the rest of this PI;
    ;;     - an xml-token describing a named entity reference.
    ;;
    ;; CDATA sections and character references are expanded inline and
    ;; never returned. Comments are silently disregarded.
    ;;
    ;; As the XML Recommendation requires, all whitespace in character data
    ;; must be preserved. However, a CR character (#xD) must be disregarded
    ;; if it appears before a LF character (#xA), or replaced by a #xA character
    ;; otherwise. See Secs. 2.10 and 2.11 of the XML Recommendation. See also
    ;; the canonical XML Recommendation.
    ;; 
    ;; ssax-read-char-data port expect-eof? str-handler seed
    (define ssax-read-char-data
      (let ((terminators-usual (list #\< #\& char-return))
            (terminators-usual-eof (list #\< '*eof* #\& char-return))
            (handle-fragment
              (lambda (fragment str-handler seed)
                (if (string-null? fragment) seed
                  (str-handler fragment "" seed)))))
        (lambda (port expect-eof? str-handler seed)
          ;; Very often, the first character we encounter is #\<
          ;; Therefore, we handle this case in a special, fast path
          (if (eqv? #\< (peek-char port))
            ;; The fast path
            (let ((token (ssax:read-markup-token port)))
              (case (xml-token-kind token)
                ((START END)		; The most common case
                 (values seed token))
                ((CDSECT)
                 (let ((seed (ssax:read-cdata-body port str-handler seed)))
                   (ssax-read-char-data port expect-eof? str-handler seed)))
                ((COMMENT)
                 (ssax-read-char-data port expect-eof? str-handler seed))
                (else
                  (values seed token))))
            ;; The slow path
            (let ((char-data-terminators
                    (if expect-eof? terminators-usual-eof terminators-usual)))
              (let loop ((seed seed))
                (let* ((fragment
                         (ssax:next-token '() char-data-terminators
                                          "reading char data" port))
                       (term-char (peek-char port)) ; one of char-data-terminators
                       )
                  (if (eof-object? term-char)
                    (values
                      (handle-fragment fragment str-handler seed)
                      term-char)
                    (case term-char
                      ((#\<)
                       (let ((token (ssax:read-markup-token port)))
                         (case (xml-token-kind token)
                           ((CDSECT)
                            (loop
                              (ssax:read-cdata-body port str-handler
                                                    (handle-fragment fragment str-handler seed))))
                           ((COMMENT)
                            (loop (handle-fragment fragment str-handler seed)))
                           (else
                             (values
                               (handle-fragment fragment str-handler seed)
                               token)))))
                      ((#\&)
                       (read-char port)
                       (case (peek-char port)
                         ((#\#) (read-char port)
                                (loop (str-handler fragment
                                                   (string (ssax:read-char-ref port))
                                                   seed)))
                         (else
                           (let ((name (ssax:read-NCName port)))
                             (ssax:assert-current-char '(#\;) "XML [68]" port)
                             (values
                               (handle-fragment fragment str-handler seed)
                               (make-xml-token 'ENTITY-REF name))))))
                      (else		; This must be a CR character
                        (read-char port)
                        (if (eqv? (peek-char port) #\newline)
                          (read-char port))
                        (loop (str-handler fragment (string #\newline) seed))))
                    ))))))))

    ;; procedure+: ssax:assert-token TOKEN KIND GI
    ;; Make sure that TOKEN is of anticipated KIND and has anticipated GI. Note GI argument
    ;; may actually be a pair of two symbols, Namespace URI or the prefix, and of the localname.
    ;; If the assertion fails, error-cont is evaluated by passing it three arguments: token
    ;; kind gi. The result of error-cont is returned.
    (define (ssax:assert-token token kind gi error-cont)
      (or (and (xml-token? token)
               (eq? kind (xml-token-kind token))
               (equal? gi (xml-token-head token)))
          (error-cont token kind gi)))

    ;; HIGH-LEVEL PARSERS
    
    ;; These parsers are a set of syntactic forms to instantiate a SSAX parser. A user can
    ;; instantiate the parser to do the full validation, or no validation, or any particular
    ;; validation. The user specifies which PI he wants to be notified about. The user tells
    ;; what to do with the parsed character and element data. The latter handlers
    ;; determine if the parsing follows a SAX or a DOM model.
    ;
    ;; syntax: make-ssax-pi-parser my-pi-handlers
    ;; Create a parser to parse and process one Processing Element (PI).
    ;
    ;; my-pi-handlers
    ;;	An assoc list of pairs (PI-TAG . PI-HANDLER) where PI-TAG is an NCName symbol, the
    ;;	PI target, and PI-HANDLER is a procedure PORT PI-TAG SEED where PORT points to the
    ;;	first symbol after the PI target. The handler should read the rest of the PI up to
    ;;	and including the combination '?>' that terminates the PI. The handler should
    ;;	return a new seed.
    ;;	One of the PI-TAGs may be the symbol *DEFAULT*. The corresponding handler will handle
    ;;	PIs that no other handler will. If the *DEFAULT* PI-TAG is not specified,
    ;;	make-ssax-pi-parser will assume the default handler that skips the body of the PI
    ;;	
    ;; The output of the make-ssax-pi-parser is a procedure
    ;;	PORT PI-TAG SEED
    ;; that will parse the current PI according to the user-specified handlers.
    (define (make-ssax-pi-parser handlers)
      (lambda (port target seed)
        (define pair (assv target handlers))
        (or pair (set! pair (assv '*DEFAULT* handlers)))
        (cond ((not pair)
                (ssax-warn port "Skipping PI: " target #\newline)
                (ssax:skip-pi port)
                seed)
              (else
                ((cdr pair) port target seed)))))
    
    ;; syntax: make-ssax-elem-parser my-new-level-seed my-finish-element
    ;;				my-char-data-handler my-pi-handlers
    ;;
    ;; Create a parser to parse and process one element, including its character content or
    ;; children elements. The parser is typically applied to the root element of a document.
    ;;
    ;; my-new-level-seed
    ;;	procedure ELEM-GI ATTRIBUTES NAMESPACES EXPECTED-CONTENT SEED
    ;;		where ELEM-GI is a RES-NAME of the element
    ;;		about to be processed.
    ;;	This procedure is to generate the seed to be passed to handlers that process the
    ;;	content of the element. This is the function identified as 'fdown' in the denotational
    ;;	semantics of the XML parser given in the title comments to this file.
    ;;
    ;; my-finish-element
    ;;	procedure ELEM-GI ATTRIBUTES NAMESPACES PARENT-SEED SEED
    ;;	This procedure is called when parsing of ELEM-GI is finished. The SEED is the result
    ;;	from the last content parser (or from my-new-level-seed if the element has the empty
    ;;	content). PARENT-SEED is the same seed as was passed to my-new-level-seed.
    ;;	The procedure is to generate a seed that will be the result of the element parser.
    ;;	This is the function identified as 'fup' in the denotational semantics of the XML
    ;;	parser given in the title comments to this file.
    ;;
    ;; my-char-data-handler
    ;;	A STR-HANDLER
    ;;
    ;; my-pi-handlers
    ;;	See ssax:make-pi-handler above
    ;; 
    ;; The generated parser is a
    ;;	procedure START-TAG-HEAD PORT ELEMS ENTITIES
    ;;	NAMESPACES PRESERVE-WS? SEED
    ;; The procedure must be called after the start tag token has been read. START-TAG-HEAD
    ;; is an UNRES-NAME from the start-element tag. ELEMS is an instance of xml-decl::elems.
    ;; See ssax:complete-start-tag::preserve-ws?
    ;; 
    ;; Faults detected:
    ;;	VC: XML-Spec.html#elementvalid 
    ;;	WFC: XML-Spec.html#GIMatch
    (define (make-ssax-elem-parser my-new-level-seed my-finish-element
                                   my-char-data-handler my-pi-handlers)
      (lambda (start-tag-head port elems entities namespaces preserve-ws? seed)
        (define xml-space-gi (cons ssax:Prefix-XML
                                   (string->symbol "space")))
        (let handle-start-tag ((start-tag-head start-tag-head)
                               (port port) (entities entities)
                               (namespaces namespaces)
                               (preserve-ws? preserve-ws?) (parent-seed seed))
          (let*-values
            (((elem-gi attributes namespaces expected-content)
              (ssax:complete-start-tag start-tag-head port elems
                                       entities namespaces))
             ((seed)
              (my-new-level-seed elem-gi attributes
                                 namespaces expected-content parent-seed)))
            (case expected-content
              ((EMPTY-TAG)
               (my-finish-element
                 elem-gi attributes namespaces parent-seed seed))
              ((EMPTY)		 ; The end tag must immediately follow
               (ssax:assert-token (and (eqv? #\< (ssax:skip-S port))
                                       (ssax:read-markup-token port))
                                  'END
                                  start-tag-head
                                  (lambda (token exp-kind exp-head)
                                    (error port "[elementvalid] broken for " token
                                           " while expecting "
                                           exp-kind exp-head)))
               (my-finish-element
                 elem-gi attributes namespaces parent-seed seed))
              (else				; reading the content...
                (let ((preserve-ws?	; inherit or set the preserve-ws? flag
                        (cond ((assoc xml-space-gi attributes) =>
                                                               (lambda (name-value)
                                                                 (equal? "preserve" (cdr name-value))))
                              (else preserve-ws?))))
                  (let loop ((port port) (entities entities)
                                         (expect-eof? #f) (seed seed))
                    (let*-values
                      (((seed term-token)
                        (ssax-read-char-data port expect-eof?
                                             my-char-data-handler seed)))
                      (if (eof-object? term-token)
                        seed
                        (case (xml-token-kind term-token)
                          ((END)
                           (ssax:assert-token term-token 'END  start-tag-head
                                              (lambda (token exp-kind exp-head)
                                                (error port "[GIMatch] broken for "
                                                       term-token " while expecting "
                                                       exp-kind exp-head)))
                           (my-finish-element
                             elem-gi attributes namespaces parent-seed seed))
                          ((PI)
                           (let ((seed
                                   ((make-ssax-pi-parser my-pi-handlers)
                                    port (xml-token-head term-token) seed)))
                             (loop port entities expect-eof? seed)))
                          ((ENTITY-REF)
                           (let ((seed
                                   (ssax:handle-parsed-entity
                                     port (xml-token-head term-token)
                                     entities
                                     (lambda (port entities seed)
                                       (loop port entities #t seed))
                                     my-char-data-handler
                                     seed))) ; keep on reading the content after ent
                             (loop port entities expect-eof? seed)))
                          ((START)		; Start of a child element
                           (if (eq? expected-content 'PCDATA)
                             (error port "[elementvalid] broken for "
                                    elem-gi
                                    " with char content only; unexpected token "
                                    term-token))
                           ;; Do other validation of the element content
                           (let ((seed
                                   (handle-start-tag
                                     (xml-token-head term-token)
                                     port entities namespaces
                                     preserve-ws? seed)))
                             (loop port entities expect-eof? seed)))
                          (else
                            (error port "XML [43] broken for "
                                   term-token))))))))
              )))
        ))


    ;;This is make-ssax-parser with all the (specialization) handlers given
    ;;as positional arguments.  It is called by make-ssax-parser, see below
    (define (make-ssax-parser/positional-args
              *handler-DOCTYPE
              *handler-UNDECL-ROOT
              *handler-DECL-ROOT
              *handler-NEW-LEVEL-SEED
              *handler-FINISH-ELEMENT
              *handler-CHAR-DATA-HANDLER
              *handler-PROCESSING-INSTRUCTIONS)
      (lambda (port seed)
        ;; We must've just scanned the DOCTYPE token.  Handle the
        ;; doctype declaration and exit to
        ;; scan-for-significant-prolog-token-2, and eventually, to the
        ;; element parser.
        (define (handle-decl port token-head seed)
          (or (eq? (string->symbol "DOCTYPE") token-head)
              (error port "XML [22], expected DOCTYPE declaration, found "
                     token-head))
          (ssax:assert-current-char ssax:S-chars "XML [28], space after DOCTYPE" port)
          (ssax:skip-S port)
          (let*-values
            (((docname) (ssax:read-QName port))
             ((systemid)
              (and (ssax:ncname-starting-char? (ssax:skip-S port))
                   (ssax-read-external-id port)))
             ((internal-subset?)
              (begin
                (ssax:skip-S port)
                (eqv? #\[
                      (ssax:assert-current-char '(#\> #\[)
                                                "XML [28], end-of-DOCTYPE" port))))
             ((elems entities namespaces seed)
              (*handler-DOCTYPE port docname systemid internal-subset? seed)))
            (scan-for-significant-prolog-token-2 port elems entities namespaces
                                                 seed)))
        ;; Scan the leading PIs until we encounter either a doctype
        ;; declaration or a start token (of the root element).  In the
        ;; latter two cases, we exit to the appropriate continuation
        (define (scan-for-significant-prolog-token-1 port seed)
          (let ((token (ssax-scan-misc port)))
            (if (eof-object? token)
              (error port "XML [22], unexpected EOF")
              (case (xml-token-kind token)
                ((PI)
                 (let ((seed
                         ((make-ssax-pi-parser *handler-PROCESSING-INSTRUCTIONS)
                          port (xml-token-head token) seed)))
                   (scan-for-significant-prolog-token-1 port seed)))
                ((DECL) (handle-decl port (xml-token-head token) seed))
                ((START)
                 (let*-values
                   (((elems entities namespaces seed)
                     (*handler-UNDECL-ROOT (xml-token-head token) seed)))
                   (element-parser (xml-token-head token) port elems
                                   entities namespaces #f seed)))
                (else (error port "XML [22], unexpected markup "
                             token))))))
        ;; Scan PIs after the doctype declaration, till we encounter
        ;; the start tag of the root element.  After that we exit
        ;; to the element parser
        (define (scan-for-significant-prolog-token-2 port elems entities namespaces seed)
          (let ((token (ssax-scan-misc port)))
            (if (eof-object? token)
              (error port "XML [22], unexpected EOF")
              (case (xml-token-kind token)
                ((PI)
                 (let ((seed ((make-ssax-pi-parser *handler-PROCESSING-INSTRUCTIONS)
                              port (xml-token-head token) seed)))
                   (scan-for-significant-prolog-token-2 port elems entities
                                                        namespaces seed)))
                ((START)
                 (element-parser (xml-token-head token) port elems
                                 entities namespaces #f
                                 (*handler-DECL-ROOT (xml-token-head token) seed)))
                (else (error port "XML [22], unexpected markup "
                             token))))))
        ;; A procedure start-tag-head port elems entities namespaces
        ;;		 preserve-ws? seed
        (define element-parser
          (make-ssax-elem-parser *handler-NEW-LEVEL-SEED
                                 *handler-FINISH-ELEMENT
                                 *handler-CHAR-DATA-HANDLER
                                 *handler-PROCESSING-INSTRUCTIONS))

        ;; Get the ball rolling ...
        (scan-for-significant-prolog-token-1 port seed)
        ))

    (define DOCTYPE 'DOCTYPE)
    (define UNDECL-ROOT 'UNDECL-ROOT)
    (define DECL-ROOT 'DECL-ROOT)
    (define NEW-LEVEL-SEED 'NEW-LEVEL-SEED)
    (define FINISH-ELEMENT 'FINISH-ELEMENT)
    (define CHAR-DATA-HANDLER 'CHAR-DATA-HANDLER)
    (define PROCESSING-INSTRUCTIONS 'PROCESSING-INSTRUCTIONS)

   
    ;; syntax: make-ssax-parser user-handler-tag user-handler-proc ...
    ;;
    ;; Create an XML parser, an instance of the XML parsing framework.
    ;; This will be a SAX, a DOM, or a specialized parser depending
    ;; on the supplied user-handlers.
    ;; 
    ;; user-handler-tag is a symbol that identifies a procedural expression
    ;; that follows the tag. Given below are tags and signatures of the
    ;; corresponding procedures. Not all tags have to be specified. If some
    ;; are omitted, reasonable defaults will apply.
    ;;
    ;; 
    ;; tag: DOCTYPE
    ;; handler-procedure: PORT DOCNAME SYSTEMID INTERNAL-SUBSET? SEED
    ;; If internal-subset? is #t, the current position in the port
    ;; is right after we have read #\[ that begins the internal DTD subset.
    ;; We must finish reading of this subset before we return
    ;; (or must call skip-internal-subset if we aren't interested in reading it).
    ;; The port at exit must be at the first symbol after the whole
    ;; DOCTYPE declaration.
    ;; The handler-procedure must generate four values:
    ;;	ELEMS ENTITIES NAMESPACES SEED
    ;; See xml-decl::elems for ELEMS. It may be #f to switch off the validation.
    ;; NAMESPACES will typically contain USER-PREFIXes for selected URI-SYMBs.
    ;; The default handler-procedure skips the internal subset,
    ;; if any, and returns (values #f '() '() seed)
    ;
    ;; tag: UNDECL-ROOT
    ;; handler-procedure: ELEM-GI SEED
    ;; where ELEM-GI is an UNRES-NAME of the root element. This procedure
    ;; is called when an XML document under parsing contains _no_ DOCTYPE
    ;; declaration.
    ;; The handler-procedure, as a DOCTYPE handler procedure above,
    ;; must generate four values:
    ;;	ELEMS ENTITIES NAMESPACES SEED
    ;; The default handler-procedure returns (values #f '() '() seed)
    ;
    ;; tag: DECL-ROOT
    ;; handler-procedure: ELEM-GI SEED
    ;; where ELEM-GI is an UNRES-NAME of the root element. This procedure
    ;; is called when an XML document under parsing does contains the DOCTYPE
    ;; declaration.
    ;; The handler-procedure must generate a new SEED (and verify
    ;; that the name of the root element matches the doctype, if the handler
    ;; so wishes). 
    ;; The default handler-procedure is the identity function.
    ;
    ;; tag: NEW-LEVEL-SEED
    ;; handler-procedure: see make-ssax-elem-parser, my-new-level-seed
    ;
    ;; tag: FINISH-ELEMENT
    ;; handler-procedure: see make-ssax-elem-parser, my-finish-element
    ;
    ;; tag: CHAR-DATA-HANDLER
    ;; handler-procedure: see make-ssax-elem-parser, my-char-data-handler
    ;
    ;; tag: PI
    ;; handler-procedure: see make-ssax-pi-parser
    ;; The default value is '()
    ; 
    ;; The generated parser is a
    ;;	procedure PORT SEED
    ;
    ;; This procedure parses the document prolog and then exits to
    ;; an element parser (created by make-ssax-elem-parser) to handle
    ;; the rest.
    ;;
    ;; [1]  document ::=  prolog element Misc*
    ;; [22] prolog ::= XMLDecl? Misc* (doctypedec | Misc*)?
    ;; [27] Misc ::= Comment | PI |  S
    ;;
    ;; [28] doctypedecl ::=  '<!DOCTYPE' S Name (S ExternalID)? S? 
    ;;			('[' (markupdecl | PEReference | S)* ']' S?)? '>'
    ;; [29] markupdecl ::= elementdecl | AttlistDecl
    ;;                      | EntityDecl
    ;;                      | NotationDecl | PI
    ;;                      | Comment 
    (define make-ssax-parser
      (let ((descriptors
              `((DOCTYPE
                  ,(lambda (port docname systemid internal-subset? seed)
                     (cond (internal-subset?
                             (ssax:skip-internal-dtd port)))
                     (ssax-warn port "DOCTYPE DECL " docname " " systemid " found and skipped")
                     (values #f '() '() seed)))
                (UNDECL-ROOT
                  ,(lambda (elem-gi seed) (values #f '() '() seed)))
                (DECL-ROOT
                  ,(lambda (elem-gi seed) seed))
                (NEW-LEVEL-SEED)          ; required
                (FINISH-ELEMENT)          ; required
                (CHAR-DATA-HANDLER)       ; required
                (PROCESSING-INSTRUCTIONS ()))))
        (lambda proplist
          (define count 0)
          (if (odd? (length proplist))
            (error 'make-ssax-parser "takes even number of arguments" proplist))
          (let ((posititional-args
                  (map (lambda (spec)
                         (define ptail (member (car spec) proplist))
                         (cond ((and ptail (odd? (length ptail)))
                                (error 'make-ssax-parser 'bad 'argument ptail))
                               (ptail
                                 (set! count (+ 1 count))
                                 (cadr ptail))
                               ((not (null? (cdr spec)))
                                (cadr spec))
                               (else
                                 (error
                                   'make-ssax-parser 'missing (car spec) 'property))))
                       descriptors)))
            (if (= count (quotient (length proplist) 2))
              (apply make-ssax-parser/positional-args posititional-args)
              (error 'make-ssax-parser 'extra 'arguments proplist))))))

    ;; PARSING XML TO SXML
    
    (define (res-name->sxml res-name)
      (string->symbol
        (string-append (symbol->string (car res-name)) ":" (symbol->string (cdr res-name)))))

    ;; This is an instance of the SSAX parser that returns an SXML representation of the XML
    ;; document to be read from `port`. `namespace-prefixes` is a list of
    ;; `(user-prefix . uri-string)` that assigns `user-prefix`es to certain
    ;; namespaces identified by particular `uri-string`s. It may be an empty list.
    ;; `xml->sxml` returns an SXML tree. The port points out to the first character
    ;; after the root element.
    (define xml->sxml
      (lambda args
        (let-optionals args ((port (current-input-port))
                             (namespace-prefixes '()))
          (let* ((namespaces
                   (map (lambda (el) (cons* #f (car el) (ssax:uri-string->symbol (cdr el))))
                        namespace-prefixes))
                 (result
                   (reverse
                     ((make-ssax-parser
                        'DOCTYPE
                        (lambda (port docname systemid internal-subset? seed)
                          (cond (internal-subset?
                                  (ssax:skip-internal-dtd port)))
                          (ssax-warn port "DOCTYPE DECL " docname " " systemid " found and skipped")
                          (values #f '() namespaces seed))
                        'NEW-LEVEL-SEED
                        (lambda (elem-gi attributes namespaces expected-content seed) '())
                        'FINISH-ELEMENT
                        (lambda (elem-gi attributes namespaces parent-seed seed)
                          (let ((nseed (ssax:reverse-collect-str-drop-ws seed))
                                (attrs (attlist-fold
                                         (lambda (attr accum)
                                           (cons (list (if (symbol? (car attr))
                                                           (car attr)
                                                           (res-name->sxml (car attr)))
                                                       (cdr attr))
                                                 accum))
                                         '() attributes)))
                            (cons (cons (if (symbol? elem-gi)
                                            elem-gi
                                            (res-name->sxml elem-gi))
                                        (if (null? attrs)
                                            nseed
                                            (cons (cons '@ attrs) nseed)))
                                  parent-seed)))
                        'CHAR-DATA-HANDLER
                        (lambda (string1 string2 seed)
                          (if (string-null? string2)
                              (cons string1 seed)
                              (cons* string2 string1 seed)))
                        'UNDECL-ROOT
                        (lambda (elem-gi seed) (values #f '() namespaces seed))
                        'PROCESSING-INSTRUCTIONS
                        (list
                          (cons '*DEFAULT*
                                (lambda (port pi-tag seed)
                                  (cons (list '*PROCESSING-INSTRUCTIONS*
                                              pi-tag
                                              (ssax:read-pi-body-as-string port))
                                        seed)))))
                      port
                      '()))))
            (cons '*TOP*
                  (if (null? namespace-prefixes)
                      result
                      (cons (list '@ (cons '*NAMESPACES*
                                           (map (lambda (ns) (list (car ns) (cdr ns)))
                                                namespace-prefixes)))
                            result)))))))
  )
)

