#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)

; Week 6 advanced command line processor

(define responses
  '((1 "What type of film do you like?")
    (2 "So you like gore?")
    (7 "Shall I recommend a gory film for you?")
    (8 "Shall I recommend a non-gory film for you?")))

(define decisiontable
  '((1 ((comedy) 20) ((very scary) 2) ((thrillers) 6)
       ((not animated) 5) ((horror) 2) ((scifi) 4))
    (2 ((some) 8) ((a lot) 7) ((yes) 7) ((no) 8) ((not really) 8))
    (7 ((yes) gory) ((ok) gory) ((no) 0))
    (8 ((yes) non-gory) ((ok) gory) ((no) 0))))

(define (get-response id)
  (car (assq-ref responses id)))

(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
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

(define (lookup id tokens)
  (let* ((record (assv-ref decisiontable id))
         (keylist 9get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens)))
  (if index
      (cadr (list-ref record index))
      #f)))

(define (recommend initial-id)
  (let loop ((id initial-id))
    (format #t "~a\n> " (get-response id))
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      (let ((response (lookup id tokens)))
        (cond ((eq? #f response)
               (format #t "huh? I didnt understand that! ")
               (loop id))
              ((eq? 'gory response)
               (format #t "Searching for gory horror films ...\n")
               (exit))
              ((eq? 'non-gory reponse)
               (format #t "Searching for non-gory scarey films ...\n")
               (exit))
              ((zero? response)
               (format #t "So long, and thanks for all the fish...\n")
               (exit))
              (else
               (loop response)))))))