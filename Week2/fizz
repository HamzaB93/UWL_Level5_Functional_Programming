#lang racket
(require racket/local)
(require racket/stream)

(define (fizzbuzz2 x)
  (cond
    [(and (zero? (modulo 5 x))
          (zero? (modulo 3 x))) "Fizz Buzz"]
    [(zero? (modulo 5 x)) "Fizz"]
    [(zero? (modulo 3 x)) "Buzz"]
    [else (number->string x)]))


(define (fizzbuzz)
  (do ((i 1 (+ i 1)))
    ((> i 100))
    (display
      (cond ((= 0 (modulo i 15)) "FizzBuzz")
            ((= 0 (modulo i 3))  "Fizz")
            ((= 0 (modulo i 5))  "Buzz")
            (else i)))
    (newline)))
     

      
       
      
          
        
        