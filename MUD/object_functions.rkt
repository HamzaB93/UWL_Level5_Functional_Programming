; File contains all the functions for objects in rooms and bag

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

; Displaying our objects
; We use this to display either what objects are in the room or bag
(define (display-objects db id)
  ; Check if the db has the desired id
  (when (hash-has-key? db id)
    ; Save the object in record
    (let* ((record (hash-ref db id))
           ; when there are many objects, put an "and" between them
           (output (string-join record " and ")))
      ; If the output is not nothing
      (when (not (equal? output ""))
        ; If the id is the bag
        (if (eq? id 'bag)
            ; Give a message including the output
            (printf "You are carrying ~a.\n" output)
            ; Then tell them if they see something 
            (printf "You can see ~a.\n" output)))))
  ; If the id isnt found
  (when (not (hash-has-key? db id))
    ; Trigger condition
    (cond
      ; If the id was bag, tell them there's nothing there
      ((eq? id 'bag) (printf "There are no items in your bag!\n"))
      ; Else, it will be the room. Tell them there is nothing there
      (else (printf "There are no items in the room!\n")))))

; Removing objects
(define (remove-object db from add-to str)
  ; If the object isnt in the db, from the place we are removing from
  ; can be the 'bag or id (of room)
  (when (hash-has-key? db from)
    (let* ((record (hash-ref db from))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      ; Trigger conditional
      (cond
        ; If the item provided doesnt exist
        ((null? item)
         ; Provide a generic error response
         (printf "Oops. Either the item isn't in the room, or you aren't carrying it!\n"))
        ; If the place we are removing from is a room/ number
        ((number? from)
         ; Give message that its added to bag
         (printf "Added ~a to your bag.\n" (first item))
         ; Use the add object functions to add to the inventory
         ; where add-to is the 'bag
         (add-object inventorydb add-to (first item))
         (hash-set! db from result))
        ; If the place we are removing from is 'bag
        ((eq? from 'bag)
         ; Give messagethe item is being moved to the bag
         (printf "Removed ~a from your bag.\n" (first item))
         ; Use the add-object function to add to the room
         ; where add-to is the room id 
         (add-object objectdb add-to (first item))
         (hash-set! db from result))))))

; Pick and put wrapper function
(define (pick-and-put id input action)
  (let ((item (string-join (cdr (string-split input)))))
    ; Trigger conditional
    (cond
      ; If the action was pick, remove from the room, and add to the bag
      ((eq? action pick) (remove-object objectdb id 'bag item))
      ; If the action was to drop, remove from the bag and add to the room
      ((eq? action drop) (remove-object inventorydb 'bag id item)))))

; Display the items in the inventory
(define (display-inventory)
  ; Use display-objects function where the db is inventory
  (display-objects inventorydb 'bag))