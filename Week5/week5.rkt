#lang racket
;(require racket/pretty)

;; WEEK %

;; 1) List Creating Functions

; Given arguments
(define (make-train name
                    departs
                    carriages
                    carriage_capacity)
  ; (label ,argument)
  `((name ,name)
    (departs ,departs)
    (carriages ,carriages)
    (carriage_capacity ,carriage_capacity)))

; The fuction takes 4 arguments.
; We then use the values to construt a list.
; Its like a template for creating lists containing train info.


; Will keep using this function with new data to create more entries
; Syntax for the list has changed, not the normal appostraphy ' but is now
; quasiquoate `
; ` allows you to pass values of variables into the list

; comma , idenitfies the variable

;; 2) List creating function continue

; Can now supply data to the function several times

; new train
(define gordon
  ; make-train with name, departs, carriages, and capacity
  (make-train "Gordon" "10:00" 5 20))

(define thomas
  (make-train "Thomas" "09:00" 10 40))

(define edward
  (make-train "Edward" "11:32" 7 25))

(define henry
  (make-train "Henry" "10:07" 2 50))

; collecting the data into a new list
(define trains (list gordon thomas edward henry))

;; Summary
; We make a template function to make a list of details for trains
; then we supply data to the function to create the list
; we then can gather all the lists into one and print them

;; 3) Retrieving data from lists

; a) assoc

; Using assoc, Use in the terminal
; (assoc '(name "Edward") trains)

; The list is searched sequentially to see if we can match agains the
; first item in that list, if match, then list is returned

; b) assq
; Using assq
;(assq 'departs (assoc '(name "Gordon") trains))

; It is like assoc, but it finds an element using eq?
; So in our case we use assoc to find the train named gordon, then we print
; print the value that is equal to departs

;; 4) Wokring with has tables

; Hash tables have key which retrieves some data which can be anything
; from a symbol to a complex nested list

; Create hash table

; set a a key name, to john
;(define db (make-hash))

; set the key name to john
;(hash-set! db 'name "John")
;(hash-set! db 'name "John Doe")

; reference the key
;(hash-ref db 'name)

; remove the value from key name
;(hash-remove! db 'name)
;(hash-ref db 'name)


;; 5) Making an application

; a) Getting started

; Make has table called db
(define db (make-hash))

; Function to add student to the hash table
(define (add-new-student id student)
  (hash-set! db id student))

; Making studets
(define s1
  `((fname "John") (sname "Smith") (age 21)))
(define s2
  `((fname "Joe") (sname "Blogs") (age 22)))

; Adding student using the function
(add-new-student 100 s1)
(add-new-student 101 s2)

; b) Get it working

; Function to get a student
(define (get-student-v1 id field)
  (cond
    ((eq? field 'age)
     (cadr (assq 'age (hash-ref db id))))
    ((eq? field 'fname)
     (cadr (assq 'fname (hash-ref db id))))
    ((eq? field 'sname)
     (cadr (assq 'sname (hash-ref db id))))))

; This function takes an id and the field that you want
; first the table is referenced with the id,
; then depending on the field, if it equals a variable, then it will
; itll return the cdr

; Not that great, as there is a lot of repetition and no error handling

; c) V2

(define (get-student-v2 id field)
  ; Made the hash ref to the id a named let
  (let ((record (hash-ref db id)))
    ; switch case, the field name
    (case field
      ; will return accordingly
      ((age) (cadr (assq field record)))
      ((fname) (cadr (assq field record)))
      ((sname) (cadr (assq field record)))
      ; if the field name wasnt found, return an error
      (else "Wrong field type"))))

; Removed repetition
; Some error handling

; d) V3
(define (get-student-v3 id field)
  ; Does the hash table even exist
  (if (hash? db)
      ; Named let for the ref, db and id
      (let ((record (hash-ref db id)))
        ; memq is like member? but uses eq?
        ; so if field is equal to anything in the list
        (if (memq field '(age fname sname))
            ; return the cadr of the assq of the field and record
            (cadr (assq field record))
            "Wrong field type"))
      "NOT VALID HASH TABLE"))

; Removed morerepition
; More error handling, even to check if the hash exists