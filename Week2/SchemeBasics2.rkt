#lang racket

;; SCHEME BASICS 2

;; 1) Composing Functions

; a) Calculating the area of a ring
;(define (area-of-ring r1 r2)
;  (- (* 3.14 (* r1 r1))
;     (* 3.14 (* r2 r2))))

; b) Calculating the area of a ring 2 BETTER
(define PI 3.14)

;(define (area-of-circle r)
;  (* PI (* r r)))

;(define (area-of-ring r1 r2)
;  (- (area-of-circle r1)
;     (area-of-circle r2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2) Comparing Expressions

; a) Equals
;(= 1 2)

; b) Less than
;(< 1 2)

; c) Greater than
;(> 1 2)

; d) AND
;(and (= 1 2) (< 2 3))

; e) OR
;(or (= 1 2) (< 2 3))

; f) NOT
;(not (= 1 2))

; g) Mixture

;(and (= 5 5) (< 5 6))
;(and (= 5 5) (< 5 5))
;(and (> 4 3) (<= 10 100))
;(or (> 4 3) (= 10 100))
;(not (= 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 3) Conditional Expressions

; a)
;(define (foo x y)
;  (cond
;    [(= x y) "Equals"]
;    [(> x y) "Greater than"]
;    [(< x y) "Less than"]))

; b)
;(define (bar x y)
;  (cond
;    [(= x y) "Equals"]
;    [(> x y) "Greater than"]
;    [else "Less than"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 4) Lambda

; a) area of circle
; Short form
;(define (area-of-circle r)
;  (* PI (* r r)))

; Lambda form
;(define area-of-circle
;  (lambda (r)
;    (* PI (* r r))))

; b) Foo
;Short form
;(define (foo x y z)
;  (+ x y z))

;(define foo
;  (lambda (x y z)
;    (+ x y z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 5) Let

;; Binds value to variable, can use the variable in limited
;; scope.

; a)
;(define (bar x)
;  (+ x 1))

;(define (foo x)
;  (let ((y (bar x)))
;    (bar y)))

; b)

;; lambda here identifies a new function within the scope
; of the foo function, it wants a paramter called z, which will
; be 10, once the lambda function is done, itll stop and print

;(define foo
;  (lambda (x)
;    (let ((y (lambda (z) (+ z 1))))
;      (y 10))))

; c)
;(define foo
;  (lambda ()
;    (let [(x 10)
;          (y 10)
;          (z 10)]
;      (+ x y z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 6) Let*
;; Allows ids to be used later on in the expression and the body

; a)

; Let
(define foo
  (lambda (x)
    (let ((y 1))
      (let ((x y))
        (+ x y)))))

; Let*
;(define foo
;  (lambda (x)
;    (let* [(y 1)
;           (x y)]
;      (+ x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Named Let
;; Iterative and recursion form, same syntax in Let, but
;; has an idetifier after the let

; a)

;(require srfi/48)

;(define (foo x)
;  (let loop [(count x)]
;    (cond [(zero? count)
;           (format #t "Blast off!\n")]
;          [else
;           (format #t "~a\n" count)
;           (loop (- count 1))])))
                 
; The named let here is called loop, it is called on the
; last line of the function

; b)

;(define (foo x)
;  (let loop [(count 0)
;             (total 0)]
;    (if (> count x)
;        total
;        (loop
;         (+ count 1)
;         (+ total count)))))
                    
