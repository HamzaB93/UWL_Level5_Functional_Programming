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
            (printf "You can see ~a.\n" output)))))
    (when (not (hash-has-key? db id))
      (cond
        ((eq? id 'bag) (printf "There are no items in your bag!\n"))
        (else (printf "There are no items in the room!\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Removing objects

(define (remove-object db from add-to str)
  (when (hash-has-key? db from)
    (let* ((record (hash-ref db from))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond
        ; remove from room
        ((null? item)
         (printf "Oops. Either the item isn't in the room, or you aren't carrying it!\n"))
        ((number? from)
         (printf "Added ~a to your bag.\n" (first item))
         (add-object inventorydb add-to (first item))
         (hash-set! db from result))
        ; remove from bag
        ((eq? from 'bag)
         (printf "Removed ~a from your bag.\n" (first item))
         (add-object objectdb add-to (first item))
         (hash-set! db from result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions to call from the main loop

(define (pick-and-put id input func)
  (let ((item (string-join (cdr (string-split input)))))
    (cond
      ((eq? func pick) (remove-object objectdb id 'bag item))
      ((eq? func drop) (remove-object inventorydb 'bag id item)))))

(define (display-inventory)
  (display-objects inventorydb 'bag))