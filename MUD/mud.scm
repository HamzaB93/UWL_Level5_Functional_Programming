#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)

;; MUD Version 5
;; Has advanced command line and actions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Decision tables and actions

; Room id and descriptions
(define descriptions '((1 "You have entered the dungeon! Tread carefully.")
                       (2 "Now you're in a hallway, seems to be two ways to go.")
                       (3 "You have entered a kitchen area. Looks like there's a storage area.")
                       (4 "It's an empty storage area.")
                       (5 "You've entered another hallway.")
                       (6 "Seems, youve entered a bedroom area.")
                       (7 "Looks like another bedroom.")
                       (8 "It's a living room.")
                       (9 "You've exited the bedroom and reached the balcony. Looks like there are stiars down.")
                       (10 "It's a courtyard. There a a few paths to take.")
                       (11 "You entered a green house.")
                       (12 "You go downstairs and find an armoury.")
                       (13 "You've entered an underground entrance.")
                       (14 "You went upstairs and hit a dead end.")
                       (15 "There's an exit!")))

; Objects decision table
(define objects '((1 "a silver dagger")
                  (1 "a gold coin")))


; Actions association list
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
; More actions
(define pick '(((get) pick) ((pickup) pick) ((pick) pick)))
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory)))

; Get put into another list
; Quasiquoting the list, to give special properties
; List filled with unquote (,) Using unquote splicing ,@ so the extra list is removed
(define actions `(,@look ,@quit ,@pick ,@put ,@inventory))


; Decision table data helps drive the game, and what happens in each room
(define decisiontable `((1 ((north) 2) ,@actions)
                        (2 ((north east) 5) ((east) 3) ,@actions)
                        (3 ((north) 5) ((south) 4) ((west) 2),@actions)
                        (4 ((north) 3) ,@actions)
                        (5 ((south) 3) ((south west) 2) ((east) 6) ,@actions)
                        (6 ((north) 8) ((south) 7) ((east) 9),@actions)
                        (7 ((north) 6) ,@actions)
                        (8 ((south) 6) ((south east) 9) ,@actions)
                        (9 ((north west) 8) ((west) 6) ((south) 10) ,@actions)
                        (10 ((north) 9) ((east) 13) ((north east) 14)
                            ((south) 11) ((south west) 12) ,@actions)
                        (11 ((north) 10) ((north east) 13) ,@actions)
                        (12 ((north east) 10) ,@actions)
                        (13 ((north) 14) ((west) 10) ((south west) 11)
                            ((east) 15) ,@actions)
                        (14 ((south) 13) ((south west) 10) ,@actions)
                        (15 ((west) 13) ,@actions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Loading object databse

; Hash table to track whats in a room
(define objectdb (make-hash))

; Hash table to track what we are carrying
(define inventorydb (make-hash))

; Adding the object to the databse
(define (add-object db id object)
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons object record)))
      (hash-set! db id (cons object empty))))

; Function will load what is in our databse into an objects database
(define (add-objects db)
  (for-each
   (lambda (r)
     (add-object db (first r) (second r))) objects))

(add-objects objectdb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Displaying our objects

; We use this to display either what objects are in the room or bag
; NEEDS IMPROVEMENTS FOR EMPTY ROOM AND BAG
(define (display-objects db id)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (output (string-join record " and ")))
      (when (not (equal? output ""))
        (if (eq? id 'bag)
            (printf "You are carrying ~a.\n" output)
            (printf "You can see ~a.\n" output))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Removing objects

; Remove from room
; When we remove an item from a room we need to add it our inventory
(define (remove-object-from-room db id str)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item)
             (printf "I dont see that item in the room!\n"))
            (else
             (printf "Added ~a to your bag.\n" (first item))
             (add-object inventorydb 'bag (first item))
             (hash-set! db id result))))))

; Remove objects from your inventory
; When we remove an item from our bag/ inventory, we put it back into a room
(define (remove-object-from-inventory db id str)
  (when (hash-has-key? db 'bag)
    (let* ((record (hash-ref db 'bag))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item)
             (print "You are not carrying that item!\n"))
            (else
             (printf "Removed ~a from your bag.\n" (first item))
             (add-object objectdb id (first item))
             (hash-set! db 'bag result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions to call from the main loop

(define (pick-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-room objectdb id item)))

(define (put-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-inventory inventorydb id item)))

(define (display-inventory)
  (display-objects inventorydb 'bag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Obtaining room direction

(define (slist->string l)
  (string-join (map symbol->string l)))

; Get direction with a given id
(define (get-directions id)
  ; Get the list depending on the id
  (let ((record (assq id decisiontable)))
    ; Finding the length of the list of directions
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
      ; If there are no room
      (cond ((= 0 n)
             (printf "You appear to have entered a room with no exits.\n"))
            ; 1 rooms
            ((= 1 n)
             (printf "You can see an exit to the ~a.\n" (slist->string (caar result))))
            (else
             ; Or more rooms
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               (printf "You can see exits to the ~a.\n" (string-join lostr " and "))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; User input

; Returns the list depending on the id, table and functions
(define (ass-ref assqlist id func)
  (cdr (func id assqlist)))

(define (get-response id)
  (car (ass-ref descriptions id assq)))

(define (get-keywords id)
  (let ((keys (ass-ref decisiontable id assq)))
    (map (lambda (key) (car key)) keys)))


;; outputs a list in the form: (0 0 0 2 0 0)
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       ;; apply some weighting to the result
       (* (/ (length set) (length x)) (length set))))
   keylist))

; Find the index of the largest number of the keylist
(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
      #f
      (list-index (lambda (x) (eq? x n)) list-of-numbers))))

; Wrapper for multi word input
; Return the id of the room
(define (lookup id tokens)
  (let* ((record (ass-ref decisiontable id assv))
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    (if index 
      (cadr (list-ref record index))
      #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Game loop

; Takes an id
(define (startgame initial-id)
  ; Named let loop, where id is the initial id and description is true
  (let loop ((id initial-id) (description #t))
    ; When description is true
    (if description
        ; Get the response/ description
        (printf "~a\n> " (get-response id))
        (printf "> "))
    ; User gives input, either one word or severs
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      ; Get the response/ room id
      (let ((response (lookup id tokens)))
        ; When a room id is returned
        (cond ((number? response)
               (loop response #t))
              ; When the response was not valid
              ((eq? #f response)
               (format #t "huh? I didn't understand that!\n")
               (loop id #f))
              ; When the input is the look action
              ((eq? response 'look)
               (get-directions id)
               (loop id #f))
              ; When the input is to pick
              ((eq? response 'pick)
               (pick-item id input)
               (loop id #f))
              ; When the input is to drop/put
              ((eq? response 'drop)
               (put-item id input)
               (loop id #f))
              ; When the input is the quit action
              ((eq? response 'quit)
               (format #t "So Long, and Thanks for All the Fish...\n")
               (exit)))))))

(startgame 1)
 