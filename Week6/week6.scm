#lang racket
(require srfi/1)
(require srfi/13)
(require srfi/48)

; WEEK 6 - Working to assignment 2

;; 1) Basic command-line processor

;(define (command)
;  (let* ((input (read-line))
;         (string-tokens (string-tokenize input))
;         (tokens (map string->symbol string-tokens))
;         (cmd (car tokens)))
;    (cond
;      ; Explains the command
;      ((eq? cmd 'help)
;       (format #t "Usage:\nsearch ,term>\nquit\n"))
;      ; Break the loop
;      ((eq? cmd 'quit)
;       (exit))
;      ; Do something
;      ((eq? cmd 'search)
;       (format #t "Searching for ~a...\n" (cadr tokens)))
;      ; Handle unknown input
;      (else
;       (format #t "Huh?\n"))))
;  (command))

; First part gets the input and break the string into a list of strings
; We then convert this to a list of symbols which can process efficiently our
;  conditional part
; It only matching single keywords      

;; 2) Advanced command-line processor

; This will be used so we can match multiple keywords in a sentence
; Can build a state machine, first we need to define some data

; Defining data
; Associative lists

; First maps numbers to sentences
(define responses
  '((1 "What type of films do you like?")
    (2 "So you like gore?")
    (7 "Shall I recommend a gory film for you?")
    (8 "Shall I recommend a non-gory scary film for you?")))

; This one returns some structured data
(define decisiontable
  '((1 ((comedy) 20) ((very scary) 2) ((thrillers) 6)
       ((not animated) 5) ((horror) 2) ((scifi) 4))
    (2 ((some) 8) ((a lot) 7) ((yes) 7) ((no) 8) ((not really) 8))
    (7 ((yes) gory) ((ok) gory) ((no) 0))
    (8 ((yes) non-gory) ((ok) gory) ((no) 0))))

; Now we need some function to make it easy to work with both associative lists

; Return a string based on the id given
(define (get-response id)
  ; from responses, if the id and the reponses are equal give the car of it
  (car (assq-ref responses id)))

; Generate a keyword list based on id given
(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
    (map (lambda (key) (car key)) keys)))

; We need to evaluate what is in a sentence
; We arent going to be doing natural language processing
; Instead we will try and examine the keyword matches and
;  and apply some kind of weighting to what we find

(define (list-of-lengths keylist tokens)
  (map
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       ;; apply some weighting to the result
       (* (/ (length set) (length x)) (length set))))
   keylist))

; This accepts a keyword list, does a match against a list of tokens.
; Resulting output is a list of weihts looking like '(0 0 0 2 0 0) indicating
;  if there was a match.

; We need to examine this further to find the best match

; All we do is return the position of the largets value in our list
(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    #f
    (list-index (lambda (x) (eq? (eq? x n)) list-of-numbers))))

; To use the previous two functions we will provide a wrapper
;  function which allows us to supply an id and list of tokens
; It then returns the resulting matching id

(define (lookup id tokens)
  (let* ((record (assv-ref decisiontable id))
         (keylist (get-keywords id))
         (index (index-of0largest-number (list-of-lengths keylist tokens))))
    (if index
        (cadr (list-ref record index))
        #f)))

; Finally we have the simpe command loop with some basic conditional statemets
; to try and handle the responses

; Finally we have a simple command loop with some basic statmenets to try
;  and handle the responses

(define (recommend initial-id)
  (let loop ((id initial-id))
    (format #t "~a\n> " (get-response id))
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokes)))
      (let ((response (lookup id tokens)))
        (cond ((eq? #f reponse)
               (format #t "huh? I didn't understand that!")
               (loop id))
              ((eq? 'gory repsonse)
               (format #t "Searching fo gory horror filsm ....\n")
               (exit))
              ((eq? 'non-gory reponse)
               (format #t "Searching for non-gory scary films ...\n")
               (exit))
              ((zer? response)
               (format #t "So long, and thanks for all the fish ...\n")
               (exit))
              (else
               (loop response)))))))
              

