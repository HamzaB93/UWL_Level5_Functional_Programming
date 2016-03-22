#lang racket

;; The first version of MUD game
; BASIC COMMAND LINE

; Links numbers with description
(define descriptions '((1 "You are in the lobby you can see an exit to the North.")
                       (2 "You are in the hallway there is an exit to the South") ))

; Association table
(define directions '((1 (north 2) (south 0) (east 0) (west 0))
                     (2 (north 0) (south 1) (east 0) (west 0))))

; Gets "descriptions" and "rid" as arguments
(define (assq-ref assqlist id)
  ; Checks with assq if the "rid" matches any in the "descriptions" list,
  ; then returns the cdr, which will be the written description
  (cdr (assq id assqlist)))

; get-room-description is given the rid as arguemnt
(define (get-room-description rid)
  ; calls another function assq-ref and gives "descriptions"/ association table
  ; and the "rid" arguments
  ; Once the description is returned, the car will be returned
  (car (assq-ref descriptions rid)))

; lookup uses "rid" and the "input" as arguments
(define (lookup room-id direction)
  ; assq-ref is called with "direction" and "rid" as arguments and returns a
  ; direction
  ; it will check to see if the returned direction and input are the same and
  ; return the car
  (car (assq-ref (assq-ref directions room-id) direction)))

; Function takes the given room-id
(define (startgame room-id)
  ; Let the room-id be rid with names let "loop"
  (let loop ((rid room-id))
    ; call get-room-description and pass the current rid as argument
    (printf "~a\n" (get-room-description rid))
    (printf "> ")
    ; Let makes read be input
    (let ((input (read)))
      ; if the input equals quit, exit the program
      (if (eq? input 'quit) (exit) 'continue)
      ; member locates the first occurance of the input from the list
      ; if it is found....
      (if (member input '(north south east west))
          ; let the direction be the value returned from the lookup function
          ; lookup gives "rid" and the "input" as arguements
          (let ((direction (lookup rid input)))
            ; if the direction is empty
            (if (zero? direction)
                ; loop back 
                (loop rid)
                (loop direction)))
          ; begin sequences expression
          (begin
            ; If the input was wrong ...
            (printf "huh? I didn't understand: ~a\n" input)
            ; otherwise loop to the start so that the next room will be
            ; displayed
            ; eg if the direction was north, the id will be 2
            (loop rid))))))

; Calling our function and passing it a room-id as argument
(startgame 1)
