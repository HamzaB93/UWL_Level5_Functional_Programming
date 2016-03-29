#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)

;; MUD Version 3
;; Has advanced command line and actions
;; NEEDS COMMENTS ON CODE

(define descriptions '((1 "room 1") (2 "room 2")
                       (3 "room 3") (4 "room 4")
                       (5 "room 5") (6 "room 6")
                       (7 "room 7") (8 "room 8")
                       (9 "room 9") (10 "room 10")
                       (11 "room 11") (12 "room 12")
                       (13 "room 13") (14 "room 14")
                       (15 "room 15")))
  
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
; Get put into another list
(define actions `(,@look ,@quit))

(define decisiontable `((1 ((north) 2) ,@actions)
                        (2 ((north east) 5) ((east) 3) ,@actions)
                        (3 ((north) 5) ((south) 4) ((west) 2),@actions)
                        (4 ((north) 3) ,@actions)
                        (5 ((south) 3) ((south west) 2) ((east) 6) ,@actions)
                        (6 ((north) 8) ((south) 7) ((east) 9),@actions)
                        (7 ((north) 6) ,@actions)
                        (8 ((south) 6) ((south east) 9) ,@actions)
                        (9 ((north west) 8) ((west) 6) ((south) 10) ((east) 14) ,@actions)
                        (10 ((north) 9) ((east) 13) ((north east) 14)
                            ((south) 11) ((south west) 12) ,@actions)
                        (11 ((north) 10) ((north east) 13) ,@actions)
                        (12 ((north east) 10) ,@actions)
                        (13 ((north) 14) ((west) 10) ((south west) 11)
                            ((east) 15) ,@actions)
                        (14 ((west) 9) ((south) 13) ((south west) 10) ,@actions)
                        (15 ((west) 13) ,@actions)))

(define (slist->string l)
  (string-join (map symbol->string l)))

(define (get-directions id)
  (let ((record (assq id decisiontable)))
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
      (cond ((= 0 n)
             (printf "You appear to have entered a room with no exits.\n"))
            ((= 1 n)
             (printf "You can see an exit to the ~a.\n" (slist->string (caar result))))
            (else
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               (printf "You can see exits to the ~a.\n" (string-join lostr " and "))))))))


(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

(define (get-response id)
  (car (assq-ref descriptions id)))

(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
    (map (lambda (key) (car key)) keys)))


;; outputs a list in the form: (0 0 0 2 0 0)
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       ;; apply some weighting to the result
       (* (/ (length set) (length x)) (length set))))
   keylist))

(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
      #f
      (list-index (lambda (x) (eq? x n)) list-of-numbers))))


(define (lookup id tokens)
  (let* ((record (assv-ref decisiontable id))
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    (if index 
      (cadr (list-ref record index))
      #f)))


(define (startgame initial-id)
  (let loop ((id initial-id) (description #t))
    (if description
        (printf "~a\n> " (get-response id))
        (printf "> "))
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      (let ((response (lookup id tokens)))
        (cond ((number? response)
               (loop response #t))
              ((eq? #f response)
               (format #t "huh? I didn't understand that!\n")
               (loop id #f))
              ((eq? response 'look)
               (get-directions id)
               (loop id #f))
              ((eq? response 'quit)
               (format #t "So Long, and Thanks for All the Fish...\n")
               (exit)))))))

(startgame 13)
