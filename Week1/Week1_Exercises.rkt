#lang racket

;; Week 1 Exercises

;; 1) Below is a sequence of expressions. What is the
;; result printed by the interpreter in
;; response to each expression?
;; Assume that the sequence is to be evaluated in the
;; order in which it is presented.

; a)
;10
; Answer: 10 is printed

; b)
;(+ 5 3 4)
; Answer: 12 is printed

; c)
;(- 9 1)
; Answer: 8 is printed

; d)
;(/ 6 2)
; Answer: 3 is printed

; e)
;(* (* 2 4) (- 4 6))
; Answer: -16 is printed

; f)
;(define a 3)
; Answer: Nothing is printed as only a variable is assigned

; g)
;(define b (+ a 1))
; Answer: Nothing is printed as only a variable is assigned

; h)
;(+ a b (* a b))
; Answer: When linked to f and g, answer is 19

; i)
;(= a b)
; Answer: When linked to f and g, the answer is false

; j)
;(if (and (> b a) (< b (* a b)))
;    b
;    a)
; Answer: 4

; k)
;(cond ((= a 4) 6)
;      ((= b 4) (+ 6 7 a))
;      (else 25))
; Answer: 16

; l)
;(+ 2 (if (> b a) b a))
; Answer: 6

; m)
;(+ (cond ((> a b) a)
;         ((< a b) b)
;         (else -1))
;   (+ a 1))
; Answer: 8 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2) Translate expression into prefix form

;(define expression
;  (/ (+ 5 4(- 2(- 3(+ 6 1/3))))
;     (* 3 (- 6 2)(- 2 7))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 3)  Define a procedure that takes three numbers
;; as arguments and returns the sum of the
;; squares of the two larger numbers.

;(define (square n)
;  (* n n))

;(define (ex x y z)
;  (cond
;    [(and (< z x) (< z y))
;     (+ (square x)
;        (square y))]
;    [(and (< y x) (< y z))
;     (+ (square x)
;        (square z))]
;    [else
;     (+ (square y)
;        (square z))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 4) Write down the steps necessary to evaluate the
;; expression below.

; a) ((car (cdr (list + - * /))) 17 5)

; s1 List puts the symbols into list form
; s2 cdr then return the atoms after the first one
; giving - * /
; s3 car returns the first atom of the new list, being -
; s4 the next expression will be (- 17 5) which gives 12

; ((car (cdr '(+ - * /))) 17 5)
; (car '(- * /) 17 5)
; (- 17 5)
; 12

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 5) Convert the following arithmetic expression into Scheme
;; expression and evaluate them

; a) 1.2 x (2 – 1/3) + -8.7

;(define (expression)
;  (+ -8.7(* 1.2 (- 2 (/ 1 3)))))

; b) (2/3 + 4/9) ÷ (5/11 – 4/3)

;(define (expression2)
;  (/ (+ (/ 2 3) (/ 4 9))
;     (- (/ 5 11) (/ 4 3))))

; c) 1 + 1 ÷ (2 + 1 ÷ (1 + 1/2))
;(define (expression3)
;  (+ 1 1 (/ (+ 2 1 (/ (+ 1 1/2))))))

; d) 1 x -2 x 3 x -4 x 5 x -6 x 7

;(define (expression4)
;  (* 1 (* -2 (* 3 (* -4 (* 5(* -6 7)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 7) Determine the values of the following expressions.
;; Use your Scheme system to verify your answers.

; a)
;(cons 'car 'cdr)
; Result: '(car . cdr)

; b)
;(list 'this '(is silly))
; Result: '(this (is silly))

; c)
;(cons 'is '(this silly?))
; Result: '(is this silly?)

; d)
;(quote (+ 2 3))
; Result: '(+ 2 3)

; e)
;(cons '+ '(2 3))
; Result: '(+ 2 3)

; f)
;(car '(+ 2 3))
; Result: '+

; g)
;(cdr '(+ 2 3))
; Result: '(2 3)

; h)
;cons
; Result: #<procedure:cons>

; i)
;(quote cons)
; Result: 'cons

; j)
;(quote (quote cons))
; Result: ''cons

; k)
;(car (quote (quote cons)))
; Result: 'quote

; l)
;(+ 2 3)
; Result: 5

; m)
;(+ '2 '3)
; Result: 5

; n)
;(+ (car '(2 3)) (car (cdr '(2 3))))
; Result: 5

; o)
;((car (list + - * /)) 2 3)
; Result: 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 8) (car (car '((a b) (c d)))) yields a.
;; Determine which compositions of car and cdr applied
;; to ((a b) (c d)) yield b, c, and d.

; Gets a
;(car (car '((a b) (c d))))

; Gets b
;(car (cdr (car '((a b) (c d)))))

; Gets c
;(car (car (cdr '((a b) (c d)))))

; Gets d
;(car(cdr (car (cdr '((a b) (c d))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 10) Draw the internal list structure produced by the
;; expression below.

(cons 1 (cons '(2 . ((3) . ())) (cons '(()) (cons 4 5)))) 