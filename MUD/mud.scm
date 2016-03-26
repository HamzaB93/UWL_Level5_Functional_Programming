#lang racket

;; The first version of MUD game
;; First version is basic command line

; Dungeon game where player navigates rooms and tries to reach an exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Association tables

; Maps a room id to a description
(define descriptions '((1 "You are in the lobby you can see an exit to the North.")
                       (2 "You are in the hallway there is an exit to the South") ))

; Structured data, Where the player can go depending on the current room
(define directions '((1 (north 2) (south 0) (east 0) (west 0))
                     (2 (north 0) (south 1) (east 0) (west 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieving data

; Retrieve data from a association table
; the table and the room id are arguments
; assq will return the list that contains the given id
; cdrs the assq to return the sentence
(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

; Get the room description by taking the room id
; uses the descriptions table and the rid
; uses assq-ref function to return the appropriate list depending on the id
; cars the list to look presentable
(define (get-room-description rid)
  (car (assq-ref descriptions rid)))

; Gets the id of the room that the user wants to go to
; Uses the current room and the atom the user inputs

; First assq-ref is used to return the list of directions that the user
; can go from the current room.
; assq-ref used again to return the the room id by checking to see if the
; input matches any direction from the returned list
; lastly the room id is retunred of the new room
(define (lookup room-id direction)
  (car (assq-ref (assq-ref directions room-id) direction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The game

; Game takes an initial id, being room 1
(define (startgame room-id)
  ; Named let loop, makes rid the room-id
  (let loop ((rid room-id))
    ; Prints the room description
    (printf "~a\n" (get-room-description rid))
    (printf "> ")
    ; User input
    (let ((input (read)))
      ; If the input is 'quit, exit game
      (if (eq? input 'quit) (exit) 'continue)
      ; If the input is anyone of these
      (if (member input '(north south east west))
          ; Will try and get the id of the new room depending on the input
          (let ((direction (lookup rid input)))
            ; If the return is 0, ask again
            (if (zero? direction)
                (loop rid)
                (loop direction)))
          (begin
            ; If the input wasnt any of the memebers
            (printf "huh? I didn't understand: ~a\n" input)
            ; Otherwise loop back to the top with the new room id
            (loop rid))))))

;(startgame 1)
