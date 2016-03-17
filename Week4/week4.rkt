#lang racket

;; WEEK 4 SCHEME BASICS

;; 1) Higher Order Functions

; Can pass functions as arguments to other functions
; Allows for abstraction.
; Can rewrite recursive/ iterative problems using HOF
; Can return functions from functions
; Can emulate OO features

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2) Using Map

; a)
(define (add1 x)
  (+ x 1))

(map add1 '(1 2 3 4 5))

; Supplied add1 function to map. Map operates over a supplied
; list, result is another list.

; b)
(map
 (lambda (x)
   (+ x 1))
 '(1 2 3 4 5))

; Can also give an anonymous function as an argument using
; lambda

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 3) Higher Order Functions

; Not all functions operate like map, they may not return lists
; Some might return nothing, but they operate the same way..
; .. They are functions which takes a function and applies
; it to a list of values

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 4) Using Fold

; a)
(foldr + 0 '(1 2 3 4 5))

; b)
(foldr * 2 '(1 2 3 4 5))

; We return single vlaue by applying the functio to each
; value and combining the result
; Need to supply an initial argument to bein calculation

; c)
(foldl cons '() '(1 2 3 4 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 5) Using for-each

(require srfi/48)
(define (blastoff)
  (for-each
   (lambda (n)
     (format #t "~a\n" n))
   '(5 4 3 2 1))
  (format #t "Blast off!\n"))

; for-each doesnt return anything, so we make the code
; do something visible

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 6) Applying Predicates

;(any even? '(1 2 3 4 5))

;(every even? '(1 2 3 4 5))

;(every even? (remove odd? '(1 2 3 4 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 7) Bank Account

; Sometimes its good to break rules,
; We said in functional we dont mutate variables, scheme we
; can do it
; We will do this to emaulatethe changing state of a bank
; account... Then go on to show how we can return functions
; from functions to provide an OO like feel

; a) Bank account V1

(require racket/trace)
(define balance 100)

(define (withdrawv1 amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient Funds!"))
(trace withdrawv1)

; Is good but the balance is globally available

; b) Bank account V2

(define withdrawv2
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient Funds!"))))
(trace withdrawv2)

; Better than the previous as one. Using let to assign
; 100 value to the balance variable. balance is not longer
; global

; c) Bank account V3
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m n)
    (cond ((eq? m 'withdraw) (withdraw n))
          ((eq? m 'deposit) (deposit n))
          (else
           (error "Unknown request -- MAKE ACCOUNT" m))))
  dispatch)
(trace make-account)
  