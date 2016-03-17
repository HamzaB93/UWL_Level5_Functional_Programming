#lang racket

;; Week 1 Lecture Scheme Basics

;; 1) Expressions

; a) Correct notation
;(* (+ 2 2) (- 3 5))

; b) Built-in functions
;(sqrt 2)
;(expt 2 3)
;(remainder 3 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2) Variables

; a) Area of a circle when radius is 5
;(define area (* 3.14 (* 5 5)))
; Answer should be 78.5, but it won' show in the console

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 3) Functions

; a) A reusable area function

;(define (area r)
;  (* 3.14 (* r r)))

; b) An improved area function with variable

;(define PI 3.14)

;(define (area r)
;  (* PI (* r r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 4) Lists and Atoms

;; A list is a collection of atoms enclosed by paranetheses

;(list? 'atom)
;(list? '(atom))
;(list? '(this is a (list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 5) The Primitive Car

;; The car of a list is the first item in the list

;(car '(a b c))
;(car '((a) b c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 6) The Primitive Cdr

;; The cdr of a list is the rest of the list, Returns the part
;; of the list that follows the first item

;(cdr '(a b c))
;(cdr '(a (b) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 7) The Primitive Cons

;; The cons functions contructs lists

;(cons 'a '(b c))
;(cons '(a) '(b c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 8) Other Primitive Procedures

;(null? '())
;(and #t #t)
;(not #t)
;(pair? '(a))
;(eq? 'a (car '(a b c)))
