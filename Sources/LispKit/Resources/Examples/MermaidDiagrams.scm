;;; Generate PDF from Mermaid diagrams
;;; 
;;; This is an example combining the functionality of many different libraries.
;;; SXML is used to represent HTML documents containing Mermaid diagrams. These
;;; diagrams are implemented in terms of the official Mermaid JavaScript code
;;; which is injected into the HTML documents after it was loaded from the
;;; LispKit asset tree without needing any network connection. The HTML documents
;;; are then rendered and a PDF snapshot is created. These PDF snapshots are then
;;; combined into a single PDF file, written to disk, and opened by the default
;;; PDF reader of the operating system.
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
        (lispkit draw)
        (lispkit draw web)
        (lispkit sxml)
        (lispkit pdf)
        (lispkit thread future))

;; Returns HTML in SXML format for a simple document that shows a single
;; Mermaid diagram. `spec` is a string specifying the diagram.
(define (mermaid-document spec)
  `(html (@ (lang "en"))
     (body (@ (style "background-color:transparent;"))
       (pre (@ (class "mermaid")) (@raw ,spec)))))

;; Composes a Mermaid script consisting of multiple lines of text.
(define (mermaid-script . lines)
  (mermaid-document
    (string-append
      (fold-left (lambda (acc line) (string-append acc "\n" line)) "" lines)
      "\n")))

;; Mermaid script for a pie chart.
(define example0
  (mermaid-script
    "pie title What Voldemort doesn't have?"
    "  \"FRIENDS\" : 3"
    "  \"FAMILY\" : 5"
    "  \"NOSE\" : 42"))

;; Mermaid script for a left-to-right flowchart.
(define example1
  (mermaid-script
    "%%{init: { 'theme': 'forest' }}%%"
    "graph LR"
    "  A[Square Rect] -- Link text --> B((Circle))"
    "  A --> C(Round Rect)"
    "  B --> D{Rhombus}"
    "  C --> D"))

;; Mermaid script for a sequence diagram.
(define example2
  (mermaid-script
    "%%{init: { 'theme': 'neutral' }}%%"
    "sequenceDiagram"
    "    participant Alice"
    "    participant Bob"
    "    Alice->>John: Hello John, how are you?"
    "    loop HealthCheck"
    "        John->>John: Fight against hypochondria"
    "    end"
    "    Note right of John: Rational thoughts<br/>prevail..."
    "    John-->>Alice: Great!"
    "    John->>Bob: How about you?"
    "    Bob-->>John: Jolly good!"))

;; Mermaid script for a mixture of flowcharts.
(define example3
  (mermaid-script
    "graph TB"
    "    sq[Square shape] --> ci((Circle shape))"
    ""
    "    subgraph A"
    "        od>Odd shape]-- Two line<br/>edge comment --> ro"
    "        di{Diamond with <br/> line break} -.-> ro(Rounded<br>square<br>shape)"
    "        di==>ro2(Rounded square shape)"
    "    end"
    ""
    "    %% Notice that no text in shape are added here instead that is appended further down"
    "    e --> od3>Really long text with linebreak<br>in an Odd shape]"
    ""
    "    %% Comments after double percent signs"
    "    e((Inner / circle<br>and some odd <br>special characters)) --> f(,.?!+-*ز)"
    ""
    "    cyr[Cyrillic]-->cyr2((Circle shape Начало));"
    ""
    "     classDef green fill:#9f6,stroke:#333,stroke-width:2px;"
    "     classDef orange fill:#f96,stroke:#333,stroke-width:4px;"
    "     class sq,e green"
    "     class di orange"))

;; Mermaid script for a top-to-bottom flowcharts.
(define example4
  (mermaid-script
    "flowchart TD"
    "  A[Hungry?] -->|Yes| B[Check the fridge]"
    "  A -->|No| Z[Then why are you here?]"
    "  B -->|Nothing there| C[Check again, just in case]"
    "  C -->|Still nothing| D[Close fridge in disappointment]"
    "  D --> E[Open fridge again, hoping food appeared]"
    "  B -->|Something there| F[Do I want to cook?]"
    "  F -->|No| G[Order takeout]"
    "  F -->|Yes| H[Start cooking]"
    "  H --> I[Get tired halfway through]"
    "  I --> J[Regret not ordering takeout]"
    "  E -->|Still nothing| K[Check pantry]"
    "  K -->|Nothing| L[Check fridge again]"
    "  L --> D"
    "  G --> M[Wait impatiently for food]"
    "  M --> N[Devour it in 2 minutes]"
    "  N --> O[Regret eating too fast]"))

;; Mermaid script for a mind map.
(define example5
  (mermaid-script
    "mindmap"
    "root((Overthinking))"
    "  A[Should I send this text?]"
    "    A1[What if they think I'm weird?]"
    "    A2[What if I don't send it and they think I don't care?]"
    "    A3[What if I send it and autocorrect ruins my life?]"
    "  B[Remembering something embarrassing from 10 years ago]"
    "    B1[Why did I say that??]"
    "    B2[Do they still remember it?]"
    "    B3[Should I move to another country?]"
    "  C[Trying to sleep]"
    "    C1[Did I lock the door?]"
    "    C2[What if my alarm doesn’t go off?]"
    "    C3[What if the sun explodes in my lifetime?]"
    "  D[Making a decision]"
    "    D1[What if I make the wrong choice?]"
    "    D2[What if I make no choice and that’s the wrong choice?]"
    "    D3[What if I overthink and time runs out?]"))

;; Mermaid script for a class diagram.
(define example6
  (mermaid-script
    "  classDiagram"
    "    note \"From Duck till Zebra\""
    "    Animal <|-- Duck"
    "    note for Duck \"can fly\ncan swim\ncan dive\ncan help in debugging\""
    "    Animal <|-- Fish"
    "    Animal <|-- Zebra"
    "    Animal : +int age"
    "    Animal : +String gender"
    "    Animal: +isMammal()"
    "    Animal: +mate()"
    "    class Duck {"
    "      +String beakColor"
    "      +swim()"
    "      +quack()"
    "    }"
    "    class Fish {"
    "      -int sizeInFeet"
    "      -canEat()"
    "    }"
    "    class Zebra {"
    "      +bool is_wild"
    "      +run()"
    "  }"))

;; Mermaid script for an ER diagram.
(define example7
  (mermaid-script
    "erDiagram"
    "    Passenger ||--o{ Booking : \"Makes\""
    "    Booking }|..|{ Flight : \"Books\""
    "    Airport }|..|{ Flight : \"Originates from\""
    "    Airport }|..|{ Flight : \"Arrives at\""
    "    Flight }|..|{ Aircraft : \"Operated by\""
    "    Aircraft }|..|{ Flight : \"Assigned to\""
    ""
    "    Passenger {"
    "        int PassengerID"
    "        string Name"
    "        string Email"
    "        string PhoneNumber"
    "    }"
    "    Booking {"
    "        int BookingID"
    "        datetime BookingDate"
    "        string SeatNumber"
    "    }"
    "    Airport {"
    "        int AirportCode"
    "        string Name"
    "        string Location"
    "    }"
    "    Flight {"
    "        int FlightNumber"
    "        datetime DepartureTime"
    "        datetime ArrivalTime"
    "    }"
    "    Aircraft {"
    "        int AircraftID"
    "        string Model"
    "        int Capacity"
    "    }"
    ""
    "    Passenger }|..|{ Booking : \"Books\""
    "    Booking }|..|{ Flight : \"Assigned to\""
    "    Airport }|..|{ Flight : \"Departure Airport\""
    "    Airport }|..|{ Flight : \"Arrival Airport\""
    "    Flight }|..|{ Aircraft : \"Assigned to\""))

;; Mermaid script for a XY/bar chart.
(define example8
  (mermaid-script
    "xychart-beta"
    "    title \"Sales Revenue\""
    "    x-axis [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec]"
    "    y-axis \"Revenue (in $)\" 4000 --> 10000"
    "    bar [5000, 5600, 7500, 8200, 9000, 9200, 8800, 8100, 7700, 7000, 7300, 7900]"
    "    line [5000, 5600, 7500, 8200, 9000, 9200, 8800, 8100, 7700, 7000, 7300, 7900]"))

;; Load the Mermaid-js code and prepend a statement to initialize Mermaid
;; on startup.
(define mermaid-js
  (string-append
    (read-file (asset-file-path "mermaid.min" "js" "JavaScript/Mermaid"))
    "\nmermaid.initialize({ startOnLoad: true });\n"))

;; Returns a PDF document with one page for each `args` document. Every document is
;; specified by an SXML representation of a web page. This procedure renders the web
;; pages and creates PDF snapshots which become the pages of the returned PDF file.
(define (mermaid-pdf . args)
  (let* (; Make a new web client
         (wc (make-web-client (cond-expand (ios 975) (else 500)) (list mermaid-js)))
         ; Create PDF snapshots for every document
         (pdfs (map (lambda (doc)
                      (web-client-pdf-snapshot-html wc (sxml->html doc) 'trim))
                    args))
         ; Extract the first page from every PDF
         (pages (map (lambda (d) (pdf-page (bytevector->pdf (future-get d)) 0)) pdfs))
         ; Create a new PDF
         (pdf (make-pdf)))
    (do ((pages pages (cdr pages))
         (index 0 (+ index 1)))
        ((null? pages) pdf)
      (pdf-insert-page! pdf index (car pages)))))

;; 9 page PDF document consisting of all the example Mermaid diagrams.
(define mpdf
  (mermaid-pdf example0 example1 example2 example3 example4 example5 example6 example7 example8))

;; Path to the generated PDF file
(define pdf-file-path
  (path (car (system-directory 'documents)) "mermaid-examples.pdf"))

;; Store the PDF file on disk
(save-pdf pdf-file-path mpdf)

;; Open the generated PDF file
(open-file pdf-file-path)

;; Return the path to the generated PDF file
pdf-file-path
