#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)
(require racket/include)
(include "assoc_and_decision.rkt")
(include "object_functions.rkt")
(include "room_navigation.rkt")

; Game loop for actions
(define (game-loop id input response)
  (cond
    ((eq? response 'look)
     (get-directions id))
    ((eq? response 'pick)
     (pick-and-put id input pick))
    ((eq? response 'drop)
     (pick-and-put id input drop))
    ((eq? response 'inventory)
     (display-inventory))
    ((eq? response 'quit)
     (format #t "Goodbye. Hope to see you again soon!\n")
     (exit))))

; Takes an id
(define (startgame initial-id)
  ; Named let loop, where id is the initial id and description is true
  (let loop ((id initial-id) (description #t))
    ; When description is true
    (if description
        ; Get the response/ description
        (printf "~a\n" (get-response id))
        (printf ""))
    (display-objects objectdb id)
    (printf "> ")
    ; User gives input, either one word or severs
    (let* ((input (string-downcase (read-line)))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      ; Get the response/ room id
      (let ((response (lookup id tokens)))
        ; When a room id is returned
        (cond
          ((number? response)
           (loop response #t))
          ; When the response was not valid
          ((eq? #f response)
           (format #t "Oops, didn't understand that. Try again!\n")
           (loop id #f))
          ((memq response '(quit inventory drop pick look))
           (game-loop id input response)
           (loop id #f)))))))

(startgame 1)