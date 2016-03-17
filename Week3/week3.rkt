;; Week 3

;; Introduction to scheme

;; 1) Recursion

; Like normal function calls but we call ourself
; Base case: need to finish looping, terminate on condition
; Recursive case: Call own function name, need something to
; help us reach our goal

; a) Factorial

;(define (fac n)
;  (if (zero? n)
;      1
;      (* n (fac (- n 1)))))

; Tracing execution of Factorial
; (fac 3)
; (* 3 ( fac 2))
; (* 3 (* 2 ( fac 1)))
; (* 3 (* 2 (* 1 ( fac 0))))
; (* 3 (* 2 (* 1 1)))
; (* 3 (* 2 1))
; (* 3 2)
; 6

; b) List recursion

;(define (len l)
;  (if (null ? l)
;      0
;   (+ 1 (len (cdr l))))))

; Tracing execution
; ( len ’( a b c ))
; (+ 1 ( len ’( b c )))
; (+ 1 (+ 1 ( len ’( c ))))
; (+ 1 (+ 1 (+ 1 ( len ’()))))
; (+ 1 (+ 1 (+ 1 0)))
; (+ 1 (+ 1 1))
; (+ 1 2)
; 3

;; 2) Working with lists

; a) Member
(define (member x l)
  (cond
    ((null? l) #f)
    ((eq? x (car l)) #t)
    (else (member x (cdr l)))))