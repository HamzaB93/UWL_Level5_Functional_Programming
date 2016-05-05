#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)
(require racket/include)
; Files to include
(include "assoc_and_decision.rkt")
(include "object_functions.rkt")
(include "room_navigation.rkt")

; Game loop for actions
(define (game-loop id input response)
  ; Conditions
  (cond
    ; When response is for look
    ((eq? response 'look)
     ; Get directions
     (get-directions id))
    ; When response is to pick
    ((eq? response 'pick)
     ; Pick up the item from the room
     (pick-and-put id input pick))
    ; When response is to drop
    ((eq? response 'drop)
     ; Drop the item from the bag
     (pick-and-put id input drop))
    ; When response is to check invetory
    ((eq? response 'inventory)
     ; Show the inventory
     (display-inventory))
    ; When respone to quit
    ((eq? response 'quit)
     ; Use the quit game function
     (quit-game id))))

(define (quit-game id)
  (cond
    ; If the room id was 10 (the final room)
    ((eq? id 10)
     (printf "Well done for finding the exit!\n"))
    (else
     ; Give message
     (printf "You didn't find the exit! Maybe next time!\n")))
  ; Shows what items were in the bag
  (printf "Here are the items you collected: \n")
  (display-inventory)
  ; Goodbye message
  (format #t "Goodbye. Hope to see you again soon!\n")
  ; Exit the game
  (exit))

; Start game with given id
(define (startgame initial-id)
  ; Named let loop, where id is the initial id and description is true
  (let loop ((id initial-id) (description #t))
    ; When description is true
    (if description
        ; Get the response/ description of the room
        (printf "~a\n" (get-response id))
        (printf ""))
    ; Display the objects of the room
    (display-objects objectdb id)
    (printf "> ")
    ; User gives input, either one word or several
    (let* ((input (string-downcase (read-line)))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      ; Set the return of lookup, can be room id or actions key words
      (let ((response (lookup id tokens)))
        ; Conditions
        (cond
          ; If the returned response from lookup is a number/ room id
          ((number? response)
           ; Loop with new room id
           (loop response #t))
          ; When the response was not valid
          ((eq? #f response)
           ; Give a message
           (format #t "Oops, didn't understand that. Try again!\n")
           ; Loop back with the current room id
           (loop id #f))
          ; If the response matches of any of the keywords in the list
          ((memq response '(quit inventory drop pick look))
           ; Call game-loop so that actions can take place
           (game-loop id input response)
           ; Loop back with current room id
           (loop id #f)))))))

; Start the game with initial room id
(startgame 10)