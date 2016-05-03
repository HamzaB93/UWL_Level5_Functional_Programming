; File contains room navigation functions

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