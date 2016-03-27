#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)

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

(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

; Get the room description by taking the room id
; uses the descriptions table and the rid
; uses assq-ref function to return the appropriate list depending on the id
; cars the list to look presentable
(define (get-room-description rid)
  (car (assq-ref descriptions rid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyword based input

(define (get-keywords id)
  (let ((keys (assq-ref directions id)))
    (map (lambda (key) (car key)) keys)))

(define (list-of-lengths keylist tokens)
  (map
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       (* (/ (length set) (length x)) (length set))))
   keylist))

(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
        #f
        (list-index (lambda (x) (eq? x n)) list-of-numbers))))


; Gets the id of the room that the user wants to go to
; Uses the current room and the atom the user inputs

; First assq-ref is used to return the list of directions that the user
; can go from the current room.
; assq-ref used again to return the the room id by checking to see if the
; input matches any direction from the returned list
; lastly the room id is retunred of the new room
(define (lookup id tokens)
  (let* ((record (assv-ref directions id))
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    (if index
        (cadr (list-ref record index))
        #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The game

; Game takes an initial id, being room 1
(define (startgame initial-id)
  (let loop ((id initial-id))
    (format #t "~a\n> " (get-room-description id))
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
    (let ((response (lookup id tokens)))
      (cond ((number? response)
             (loop response #t))
            ((eq? #f response)
             (format #t "Huh? I didn't understand that!\n")
             (loop id #f))
            ((eq? response 'quit)
             (format #t "BYE\n")
             (exit)))))))

(startgame 1)
