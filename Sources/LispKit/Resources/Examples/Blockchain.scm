;;; Blockchain
;;;
;;; This is an implementation of a very simple blockchain. This code was inspired
;;; by the book "Introducing Blockchain with Lisp" by Boro Sitnikovski (Apress),
;;; which incrementally builds a simple blockchain in Racket. The example code in
;;; this file was written from scratch but keeps a similar structure like the
;;; Racket implementation, so it can be used together with the book. Nevertheless,
;;; the actual code is quite different and makes idiomatic use of LispKit coding
;;; style and libraries. A similar implementation in Java is described here:
;;; https://medium.com/programmers-blockchain/create-simple-blockchain-java-tutorial-from-scratch-6eeed3cb03fa
;;; 
;;; The code below incrementally introduces all abstractions needed for a simple
;;; blockchain starting with wallets, including transaction inputs/outputs,
;;; transactions, blocks, and finally the blockchain data structure itself.
;;; The implementation only allows for one transaction per block.
;;; 
;;; The code provides a comprehensive library of debugging procedures that are
;;; able to print out the various data structures. The code concludes with an
;;; example usage of the abstractions.
;;; 
;;; Author: Matthias Zenger
;;; Copyright © 2023 Matthias Zenger. All rights reserved.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
;;; file except in compliance with the License. You may obtain a copy of the License at
;;;
;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software distributed
;;; under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
;;; CONDITIONS OF ANY KIND, either express or implied. See the License for the specific
;;; language governing permissions and limitations under the License.

(import (lispkit base)
        (lispkit math util)
        (lispkit list set)
        (lispkit iterate)
        (lispkit crypto))

;;; UTILITIES

(define (drop-last lis)
  (if (pair? (cdr lis))
      (cons (car lis) (drop-last (cdr lis)))
      '()))

(define (current-timestamp)
  (exact (floor (* (current-second) 10000.0))))

(define (format-timestamp tstamp)
  (let ((tstr (number->string tstamp)))
    (string-append (substring tstr 6 (string-length tstr)) "t")))

(define (compute-sha256 . strs)
  (bytevector->hex (sha256 (apply bytevector-append (map string->utf8 strs)))))

(define (random-sha256)
  (bytevector->hex (sha256
                     (string->utf8
                       (number->string (exact (floor (* (current-second) 1000000))))))))

;;; WALLETS

;; Objects of record type `<wallet>` provide access to a public and a private key.
(define-record-type <wallet>
  (wallet private-key public-key)
  wallet?
  (private-key wallet-private-key)
  (public-key wallet-public-key))

;; Return a new wallet with a random private and matching public key.
(define (make-wallet)
  (let ((privkey (make-private-key 'rsa 1024)))
    (wallet (bytevector->hex (secure-key->bytevector privkey))
            (bytevector->hex (secure-key->bytevector (public-key privkey))))))

;; Returns the private key object used by library `(lispkit crypto)`.
(define (wallet-private-key-obj w)
  (bytevector->private-key 'rsa (hex->bytevector (wallet-private-key w))))

;; Returns the public key object used by library `(lispkit crypto)`.
(define (wallet-public-key-obj w)
  (bytevector->public-key 'rsa (hex->bytevector (wallet-public-key w))))

;; Return a wallet identifier as a string.
(define (format-wallet w)
  (string-append "@" (substring (wallet-public-key w) 76 80)))

;; Serialize a wallet object into a string.
(define (serialize-wallet w)
  (call-with-output-string
    (lambda (port)
      (write (list (wallet-private-key w)
                   (wallet-public-key w))
             port))))


;;; TRANSACTION INPUT/OUTPUT

;; Objects of record <transaction-io> are representing transaction amounts
;; (the `value`) that belong to an owning wallet (the `owner`).
(define-record-type <transaction-io>
  (transaction-io transaction-hash value owner timestamp)
  transaction-io?
  (transaction-hash transaction-io-hash)
  (value transaction-io-value)
  (owner transaction-io-owner)
  (timestamp transaction-io-timestamp))

;; Compute a hash code for a transaction input/output with the given value,
;; owner, and timestamp.
(define (compute-transaction-io-hash value owner timestamp)
  (compute-sha256 (number->string value)
                  (serialize-wallet owner)
                  (number->string timestamp)))

;; Return a new transaction input/output amount for a given value and
;; owner (ie. wallet).
(define (make-transaction-io value owner)
  (assert (number? value)
          (wallet? owner))
  (let ((timestamp (current-timestamp)))
    (transaction-io (compute-transaction-io-hash value owner timestamp)
                    value
                    owner
                    timestamp)))

;; Return `#t` if the given transaction input/output amount is valid.
;; A transaction amount is valid if its hash code fits to the value,
;; owner and timestamp.
(define (valid-transaction-io? tio)
  (string=? (transaction-io-hash tio)
            (compute-transaction-io-hash (transaction-io-value tio)
                                         (transaction-io-owner tio)
                                         (transaction-io-timestamp tio))))

;; Returns a string representation of the given transaction for display purposes.
(define (format-transaction-io tio)
  (string-append
    "["
    (format-timestamp (transaction-io-timestamp tio))
    ": "
    (format-wallet (transaction-io-owner tio))
    " has amount of "
    (number->string (transaction-io-value tio))
    "]"))

;; Serializes a transaction into a string.
(define (serialize-transaction-io tio)
  (call-with-output-string
    (lambda (port)
      (write (list (transaction-io-hash tio)
                   (transaction-io-value tio)
                   (serialize-wallet (transaction-io-owner tio))
                   (transaction-io-timestamp tio))
             port))))


;;; TRANSACTIONS

;; A transaction object represents a transfer of amount `value` from wallet
;; `from` to wallet `to`. `inputs` define input transaction amounts. `outputs`
;; define output transaction amounts. Transactions are signed objects.
(define-record-type <transaction>
  (transaction signature from to value inputs outputs)
  transaction?
  (signature transaction-signature)
  (from transaction-from)
  (to transaction-to)
  (value transaction-value)
  (inputs transaction-inputs)
  (outputs transaction-outputs))

;; Returns the signature of a transaction over amount `value` from wallet
;; `from` to wallet `to`.
(define (sign-transaction from to value)
  (bytevector->hex
    (sign (wallet-private-key-obj from)
          'rsa-signature-message-pkcs1v15-sha256
          (bytevector-append
            (string->utf8 (serialize-wallet from))
            (string->utf8 (serialize-wallet to))
            (string->utf8 (number->string value))))))

;; Returns a new transaction representing a transfer of `value` from wallet
;; `from` to wallet `to` with the given input transaction amounts.
(define (make-transaction b from to value)
  (assert (opt blockchain? b)
          (wallet? from)
          (wallet? to)
          (number? value))
  (let* ((inputs (filter (lambda (t) (equal? from (transaction-io-owner t)))
                         (if b (blockchain-utxo b) '())))
         (leftover (- (sum (map transaction-io-value inputs)) value)))
    (transaction (sign-transaction from to value)
                 from
                 to
                 value
                 inputs
                 (list (make-transaction-io value to)
                       (make-transaction-io leftover from)))))

;; A transaction signature is valid if we can verify via the public key of the
;; wallet initiating the transaction its signature.
(define (valid-transaction-signature? t)
  (verify (wallet-public-key-obj (transaction-from t))
          'rsa-signature-message-pkcs1v15-sha256
          (hex->bytevector (transaction-signature t))
          (bytevector-append
            (string->utf8 (serialize-wallet (transaction-from t)))
            (string->utf8 (serialize-wallet (transaction-to t)))
            (string->utf8 (number->string (transaction-value t))))))

;; A transaction is valid if it has a valid signature, every output is valid,
;; and the sum of the inputs is at least as high as the sum of the outputs.
(define (valid-transaction? t)
  (and (valid-transaction-signature? t)
       (every? valid-transaction-io? (transaction-outputs t))
       (>= (sum (map transaction-io-value (transaction-inputs t)))
           (sum (map transaction-io-value (transaction-outputs t))))))

;; Returns a string representation of the given transaction for display purposes.
(define (format-transaction t)
  (string-append
    (format-wallet (transaction-from t))
    " sends "
    (format-wallet (transaction-to t))
    " an amount of "
    (number->string (transaction-value t))))

;; Serializes a transaction into a string.
(define (serialize-transaction t)
  (call-with-output-string
    (lambda (port)
      (write (list (transaction-signature t)
                   (serialize-wallet (transaction-from t))
                   (serialize-wallet (transaction-to t))
                   (transaction-value t)
                   (map serialize-transaction-io (transaction-inputs t))
                   (map serialize-transaction-io (transaction-outputs t)))
             port))))


;;; BLOCKS

;; A blockchain is made up of blocks which are linked via their hashes.
;; Each block contains a `previous-hash`, a `current-hash`, a `transaction`,
;; a `timestamp`, and `nonce` field which is used by the hashcash algorithm.
(define-record-type <block>
  (block current-hash previous-hash transaction timestamp nonce)
  block?
  (current-hash block-current-hash)   ; string
  (previous-hash block-previous-hash) ; string
  (transaction block-transaction)     ; transaction
  (timestamp block-timestamp)         ; fixnum
  (nonce block-nonce))                ; fixnum

;; Computes the hash for a block consisting of the given fields.
(define (compute-block-hash previous-hash timestamp transaction nonce)
  (compute-sha256 previous-hash
                  (number->string timestamp)
                  (serialize-transaction transaction)
                  (number->string nonce)))

;; Returns `#t` if block `b` is a valid block; `#f` otherwise.
(define (valid-block? b)
  (string=? (block-current-hash b)
            (compute-block-hash (block-previous-hash b)
                                (block-timestamp b)
                                (block-transaction b)
                                (block-nonce b))))

;; This determines the computational effort needed to mine a block.
;; The higher the number, the more computational power is needed.
(define difficulty 1)  ; set to 2

(define target (bytevector->hex (make-bytevector difficulty 32)))

;; Returns `#t` if the given `block-hash` meets the mining target.
(define (mined-block? block-hash)
  (bytevector=? (bytevector-copy (hex->bytevector block-hash) 0 difficulty)
                (bytevector-copy (hex->bytevector target) 0 difficulty)))

;; Return a new mined block for the given `previous-hash`, `timestamp`,
;; `transaction`, and a minimum `nonce` field.
(define (make-block previous-hash timestamp transaction nonce)
  (let ((current-hash (compute-block-hash previous-hash timestamp transaction nonce)))
    (if (mined-block? current-hash)
        (block current-hash previous-hash transaction timestamp nonce)
        (make-block previous-hash timestamp transaction (+ nonce 1)))))

;; Returns a new mined block for the given transaction and previous hash.
(define (mine-block transaction previous-hash)
  (assert (transaction? transaction)
          (string? previous-hash))
  (make-block previous-hash (current-timestamp) transaction 1))


;;; BLOCKCHAINS

;; A blockchain consists of a list of blocks and a list of unspent transaction
;; outputs (UTXO). UTXO elements are implemented via transaction-io. They represent
;; some amount which has been authorized by one wallet to be spent by another wallet.
(define-record-type <blockchain>
  (blockchain blocks utxo)
  blockchain?
  (blocks blockchain-blocks)  ; list of blocks
  (utxo blockchain-utxo))     ; list of transaction-io

;; Returns a new blockchain with an initial transaction.
(define (make-blockchain transaction . args)
  (let-optionals args ((seed-hash (random-sha256))
                       (utxo #f))
    (assert (transaction? transaction)
            (string? seed-hash)
            (opt pair? utxo))
    (blockchain (list (mine-block transaction seed-hash))
                (or utxo (list (make-transaction-io (transaction-value transaction)
                                                    (transaction-to transaction)))))))

;; Mining reward function; start with 50 coins initially, and halve them on every
;; 210000 blocks (original Bitcoin implementation).
(define (mining-reward blocks)
  (/ 50 (expt 2 (floor (/ (length blocks) 210000)))))

;; Adds a given transaction `t` to blockchain `b`. This simple implementation
;; mines a new block for each transaction and attaches it to the blockchain.
;; It creates an updated UTXO that includes the reward for the mining effort.
(define (add-transaction b t)
  (let ((blocks (cons (mine-block t (block-current-hash (car (blockchain-blocks b))))
                      (blockchain-blocks b))))
    (blockchain
      blocks
      (cons (make-transaction-io (mining-reward blocks) (transaction-from t))
            (lset-union equal?
                        (transaction-outputs t)
                        (lset-difference equal? (blockchain-utxo b) (transaction-inputs t)))))))

;; Returns the balance of a wallet `w` in the given blockchain `b`. This is the
;; sum of all unspent funds of transactions for wallet `w`. These can be found
;; by traversing the current UTXO list of the blockchain.
(define (balance-wallet b w)
  (sum (map transaction-io-value
            (filter (lambda (t) (equal? w (transaction-io-owner t)))
                    (blockchain-utxo b)))))

;; Sends the amount of `value` from wallet `from` to wallet `to` via blockchain
;; `b`. This procedure first creates a corresponding transaction, then verifies
;; that wallet `from` has sufficient funds, and finally creates an updated
;; blockchain incorporating the new transaction. The procedure returns the
;; blockchain unchanged if it wasn't possible to create a valid transaction.
(define (send-money b from to value)
  (let ((t (make-transaction b from to value)))
    (print-transaction t)
    (if (and (>= (balance-wallet b from) value)
             (valid-transaction? t))
        (add-transaction b t)
        b)))

;; Checks if blockchain `b` is well-formed. A blockchain is well-formed if
;; all its blocks are valid, previous hashes are matching, all blocks are
;; mined and their transactions are valid.
(define (valid-blockchain? b)
  (let ((blocks (blockchain-blocks b)))
    (and (every? valid-block? blocks)
         (equal? (drop-last (map block-previous-hash blocks))
                 (cdr (map block-current-hash blocks)))
         (every? valid-transaction? (map block-transaction blocks))
         (every? mined-block? (map block-current-hash blocks)))))


;;; BLOCKCHAIN DEBUGGING

(define (print-wallets b wa wb)
  (display* "\nBalance wallet " (format-wallet wa) ": " (balance-wallet b wa) "\n")
  (display* "Balance wallet " (format-wallet wb) ": " (balance-wallet b wb) "\n\n"))

(define (print-transaction-io tio)
  (display* (format-transaction-io tio) "\n"))

(define (print-transaction t)
  (display* "⎛ From   : " (format-wallet (transaction-from t)) "\n")
  (display* "⎜ To     : " (format-wallet (transaction-to t)) "\n")
  (display* "⎜ Value  : " (number->string (transaction-value t)) "\n")
  (display* "⎜ Inputs : ")
  (if (pair? (transaction-inputs t))
      (begin
        (display* (format-transaction-io (car (transaction-inputs t))) "\n")
        (dolist (inp (cdr (transaction-inputs t)))
          (display* "⎜          " (format-transaction-io inp) "\n")))
      (display* "[]\n"))
  (if (pair? (transaction-outputs t))
      (begin
        (if (pair? (cdr (transaction-outputs t))) (display "⎜") (display "⎝"))
        (display* " Outputs: " (format-transaction-io (car (transaction-outputs t))) "\n")
        (do ((next (cdr (transaction-outputs t)) (cdr next)))
            ((not (pair? next)))
          (if (pair? (cdr next)) (display "⎜") (display "⎝"))
          (display* "          " (format-transaction-io (car next)) "\n")))
      (display* "⎝ Outputs: []\n")))

(define (print-block bl)
  (display* "## Time:    " (format-timestamp (block-timestamp bl)) "\n")
  (display* "## Hash:    " (block-current-hash bl) "\n")
  (display* "## PrvHash: " (block-previous-hash bl) "\n")
  (display* "## Nonce:   " (block-nonce bl) "\n")
  (display* "## Data:    " (format-transaction (block-transaction bl)) "\n"))

(define (print-blockchain b)
  (display "BLOCKS:\n")
  (dolist (block (blockchain-blocks b))
    (display "-----------------\n")
    (print-block block))
  (display "-----------------\n")
  (display "UTXO:\n")
  (dolist (tio (blockchain-utxo b))
    (display "  ")
    (print-transaction-io tio)))


;;: EXAMPLE USAGE

(define root-wallet (make-wallet))
(define wallet-a (make-wallet))
(define wallet-b (make-wallet))

(display "Making genesis transaction...\n")
(define genesis (make-transaction #f root-wallet wallet-a 100))

(display "Mining genesis block and creating initial blockchain...\n")
(define bchain (make-blockchain genesis))
(print-transaction genesis)
(print-wallets bchain wallet-a wallet-b)

(display "Mining second transaction...\n")
(set! bchain (send-money bchain wallet-a wallet-b 20))
(print-wallets bchain wallet-a wallet-b)

(display "Mining third transaction...\n")
(set! bchain (send-money bchain wallet-b wallet-a 10))
(print-wallets bchain wallet-a wallet-b)

(display "Mining fourth transaction...\n")
(set! bchain (send-money bchain wallet-a wallet-b 5))
(print-wallets bchain wallet-a wallet-b)

(display "Attempting to mine fifth (not valid) transaction...\n")
(set! bchain (send-money bchain wallet-b wallet-a 80))
(print-wallets bchain wallet-a wallet-b)

(print-blockchain bchain)
(newline)
(if (valid-blockchain? bchain)
    (display* "The blockchain is well-formed\n")
    (display* "The blockchain is not well-formed\n"))
