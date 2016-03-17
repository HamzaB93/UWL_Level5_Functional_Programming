
;; Here the user defined functions from the book
;; "The Little Schemer" will be written. The aim is to
;; understand what the code does and give the tracing
;; execution

;; Chapter 1) Toys

(define atom?
  ; x is element
  (lambda (x)
    ; To be true, must not be pair
    (and (not (pair? x))
         ; Must not be null
         (not (null? x)))))

;; Tracing execution
; 1)
; (atom? 'Adam)

;1 (and (not (pair? 'Adam))(not (null? 'Adam)))
;2 (and (not #f)(not #f))
;3 (and #t #t))
;4 #t

; 2)
; (atom? '())

;1 (and (not (pair? '()))(not (null? '())))
;2 (and (not #f)(not #t))
;3 (and #t #f)
;4 #f

;; Chapter 2: Do it, Do it again, and again and again

;; lat?
(define lat?
  ; l is list
  (lambda (l)
    (cond
      ; If null, is true
      [(null? l) #t]
      ; If car is atom, then recur
      [(atom? (car l)) (lat? (cdr l))]
      ; Else element is not atom
      (else #f))))
(trace lat?)

; Tracing execution

; 1)
; (lat? '(Andrew Ben Charlie))

;1 (#t (lat? '(Ben Charlie)))
;2 (#t (#t (lat? '(Charlie))))
;3 (#t (#t (#t (lat? '()))))
;4 #t

; 2)
; (lat? '(Andrew (Ben) Charlie))

;1 (#t (lat? '((Ben) Charlie)))
;2 #f


;; member?
(define member?
  ; a is element and lat is list
  (lambda (a lat)
    (cond
      ; if null, a is not present
      [(null? lat) #f]
      (else
       ; either car of lat is equal to a.. if false
       (or (eq? (car lat) a)
           ; start recursion
                (member? a (cdr lat)))))))
(trace member?)

; Tracing execution

; 1)
; (member? 'Joe '(Hamza Amrit Joe Tashan))

;1 (member? 'Joe '(Amrit Joe Tashan))
;2 (member? 'Joe '(Joe Tashan))
;3 #t

; 2)
; (member? 'Joe '(Hamza Amrit Tashan))

;1 (member? 'Joe '(Amrit Tashan))
;2 (member? 'Joe '(Tashan))
;3 (member? 'Joe '())
;4 #f

;; Chapter 3: Cons the Magnificent

;; rember

(define rember
  ; takes element and list
  (lambda (a lat)
    (cond
      ; if list is null, return empty list
       ((null? lat) '())
       ; if car of list is equal to a, return cdr of lat
       ((eq? (car lat) a) (cdr lat)) 
       (else
        ; cons the car of lat, then recurse withe a and cdr of lat
        (cons (car lat) (rember a (cdr lat)))))))
(trace rember)

; Tracing execution

; 1) (rember 'a '(a b c))

;1 '(b c)

; 2) (rember 'b '(a b c))

;1 (cons 'a) (rember 'b '(b c))
;2 (cons 'a '(c))
;3 '(a c)

; 3) (rember 'c '(a b c))

;1 (cons 'a) (rember 'c '(b c))
;2 (cons 'a) (cons 'b) (rember 'c '(c))
;3 (cons 'a) (cons 'b '())
;4 (cons 'a '(b))
;5 '(a b)

;; firsts

(define firsts
  ; l is list with nest lists
  (lambda (l)
    (cond
      ; if list is null, return empty list
      ((null? l) '())
      (else
       ; else cons the car of the car of l and recurse with cdr of l
       (cons (car (car l)) (firsts (cdr l)))))))
(trace firsts)

; Tracing execution

; 1) (firsts '((Adam Bob) (Bob Charlie) (Charlie David))

;1 (cons 'Adam) (firsts '((Bob Charlie) (Charlie David))
;2 (cons 'Adam) (cons 'Bob) (firsts '((Charlie David)))
;3 (cons 'Adam) (cons 'Bob) (cons 'Charlie) (firsts '())
;4 (cons 'Adam) (cons 'Bob) (cons 'Charlie '())
;5 (cons 'Adam) (cons 'Bob '(Charlie))
;6 (cons 'Adam '(Bob Charlie))
;7 '(Adam Bob Charlie)

;; insertR
(define insertR
  ; takesa new element, and old element to insert
  ; next to and a list
  (lambda (new old lat)
    (cond
      ; if list is null give empty list
      ((null? lat) '())
      (else
       (cond
         ; if car of list equals old
         ((eq? (car lat) old)
          ; first cons new then old to cdr of lat
          (cons old (cons new (cdr lat))))
         (else
          ; cons car lat, then recurse with cdr of lat as lat
          (cons (car lat)
                (insertR new old (cdr lat)))))))))
(trace insertR)

; Tracing execution

; 1)
; (insertR 'cream 'and '(I like cookies and ice-cream))

;1 (cons 'I) (insertR 'cream 'and '(like cookies and ice-cream))
;2 (cons 'I) (cons 'like) (insertR 'cream 'and '(cookies and ice-cream))
;3 (cons 'I) (cons 'like) (cons 'cookies) (insertR 'ceram 'and '(and ice-cream))
;4 (cons 'I) (cons 'like) (cons 'cookies) (cons 'and) (cons 'cream '(ice-cream))
;5 (cons 'I) (cons 'like) (cons 'cookies) (cons 'and '(cream ice-cream))
;6 (cons 'I) (cons 'like) (cons 'cookies '(and cream ice-cream))
;7 (cons 'I) (cons 'like '(cookies and cream ice-cream))
;8 (cons 'I '(like cookes and cream ice-cream))
;9 '(I like cookies and cream ice-cream)

; insertL
; similar to insertR BUT....
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? (car lat) old)
          ; ...cons old first, then new to the cdr of lat
          (cons new (cons old (cdr lat))))
         (else (cons (car lat)
                     (insertL new old (cdr lat)))))))))
(trace insertR)

; subst
(define subst
  ; takes an element, old element to replace and a list
  (lambda (new old lat)
    (cond
      ; if list is null, return empty list
      ((null? lat) '())
      (else
       (cond
         ; if car lat equals old
         ((eq? (car lat) old)
          ; then cons the new and return cdr lat
          (cons new (cdr lat)))
         (else
          ; else, cons car of lat, then recurse with new old and cdr of lat
          (cons (car lat) (subst new old (cdr lat)))))))))
(trace subst)

; Tracing execution

; (subst 'love 'like '(i like programming))

;1 (cons 'I) (subst 'love 'like '(like programming))
;2 (cons 'I) (cons 'love '(programming))
;3 (cons 'I '(love programming))
;4 '(I love programming)

; subst2
(define subst2
  ; takes an element, occurane 1 and 2 from list
  ; and a list
  (lambda (new o1 o2 lat)
    (cond
      ; if list is null, return empty list
      ((null? lat) '())
      (else
       (cond
         ; if either car lat is equal to o1 or o2
         ((or (eq? (car lat) o1) (eq? (car lat) o2)
              ; then cons the new to the cdr of lat
              (cons new (cdr lat))))
          (else
           ; else, cons the car of lat, then recrus with cdr of lat
           (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))

; Tracing execution

; (subst2 'love 'hate 'videogames '(I really hate videogames))

;1 (cons 'I) (subst2 'love 'hate 'videogames '(really hate videogames))
;2 (cons 'I) (cons 'really) (subst2 'love 'hate 'videogames '(hate videogames))
;3 (cons 'I) (cons 'really) (cons 'love '(videogames))
;4 (cons 'I) (cons 'really '(love videogames))
;5 (cons 'I '(really love videogames))
;6 '(I really love vidoegames)

; multirember
(define multirember
  ; takes an element, and a list
  (lambda (a lat)
    (cond
      ; if list is null, return empty list
      ((null? lat) '())
      (else
       (cond
         ; if car lat is equal to a, then start recursion with cdr of lat
         ; NOTE, will return here till list is null
         ((eq? (car lat) a) (multirember a (cdr lat)))
         (else
          ; otherwise, cons the cdr of lat, then recrus with cdr of lat
          (cons (car lat) (multirember a (cdr lat)))))))))
(trace multirember)

; Tracing execution

; (multirember 'icecream '(chocolate icecream and strawberry icecream and vanilla icecream))

;1 (cons 'chocolate) (multirember 'icecream '(icecream and strawberry icecream and vanilla icecream))
;2 (cons 'chocolate) (multirember 'icecream '(and strawberry icecream and vanilla icecream))
;3 (cons 'chocolate) (cons 'and) (multirember 'icecream '(strawberry icecream and vanilla icecream))
;4 (cons 'chocolate) (cons 'and) (cons 'strawberry) (multirember 'icecream '(icecream and vanilla icecream))
;5 (cons 'chocolate) (cons 'and) (cons 'strawberry) (multirember 'icecream '(and vanilla icecream))
;6 (cons 'chocolate) (cons 'and) (cons 'strawberry) (cons 'and) (multirember 'icecream '(vanilla icecream))
;7 (cons 'chocolate) (cons 'and) (cons 'strawberry) (cons 'and) (cons 'vanilla) (multirember 'icecream '(icecream))
;8 (cons 'chocolate) (cons 'and) (cons 'strawberry) (cons 'and) (cons 'vanilla) (multirember 'icecream '())
;9 (cons 'chocolate) (cons 'and) (cons 'strawberry) (cons 'and) (cons 'vanilla '())
;10 (cons 'chocolate) (cons 'and) (cons 'strawberry) (cons 'and '(vanilla))
;11 (cons 'chocolate) (cons 'and) (cons 'strawberry '(and vanilla))
;12 (cons 'chocolate) (cons 'and '(strawberry and vanilla))
;13 (cons 'chocolate '(and strawberry and vanilla))
;14 '(chocolate and strawberry and vanilla)

; multiinserR
(define multiinsertR
  ; takes a new element, old reference, and list
  (lambda (new old lat)
    (cond
      ; if list is null, return empty list
      ((null? lat) '())
      (else
       (cond
         ; if car of list is equal to old
         ((eq? (car lat) old)
          ; then cons new then old and recruse with cdr of lat
          (cons old) (cons new (multiinsertR new old (cdr lat))))
         (else
          ; else cons car of lat, then recurs with cdr of lat
          (cons (car lat) (multiinsertR new old (cdr lat)))))))))

; Tracing execution

; (multiinsertR 'cake 'chocolate '(chocolate and more chocolate))

;1 (cons 'chocolate) (cons 'cake) (multiinsertR 'cake 'chocolate '(and more chocolate))
;2 (cons 'chocolate) (cons 'cake) (cons 'and) (multiinsertR 'cake 'chocolate '(more chocolate))
;3 (cons 'chocolate) (cons 'cake) (cons 'and) (cons 'more) (multiinsertR 'cake 'chocolate '(chocolate))
;4 (cons 'chocolate) (cons 'cake) (cons 'and) (cons 'more) (cons 'chocolate)(cons 'cake) (multiinsertR 'cake 'chocolate '())
;5 (cons 'chocolate) (cons 'cake) (cons 'and) (cons 'more) (cons 'chocolate)(cons 'cake '())
;6 (cons 'chocolate) (cons 'cake) (cons 'and) (cons 'more) (cons 'chocolate '(cake))
;7 (cons 'chocolate) (cons 'cake) (cons 'and) (cons 'more '(chocolate cake))
;8 (cons 'chocolate) (cons 'cake) (cons 'and '(more chocolate cake))
;9 (cons 'chocolate) (cons 'cake '(and more chocolate cake))
;10 (cons 'chocolate '(cake and more chocolate cake))
;11 '(chocolate cake and more chocolate cake)

;multiinsertL
; similar to multiinsertR BUT...
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
       (cond
         ((eq? (car lat) old)
          ; will cons old first, then new, and then recurs
          (cons new) (cons old (multiinsertL new old (cdr lat))))
         (else
          (cons (car lat) (multiinsertL new old (cdr lat)))))))))

;multisubst
(define multisubst
  ; takes a new element, an old element to sub, and list
  (lambda (new old lat)
    (cond
      ; if list is null, return an empty list
      ((null? lat) '())
      (else
       (cond
         ; if the car of list is equal to old
         ((eq? (car lat) old)
          ; then cons the new and recurs with the cdr of lat
          (cons new (multisubst new old (cdr lat))))
         (else
          ; else, cons the car of lat, then recrus with the cdr of lat
          (cons (car lat) (multisubst new old (cdr lat)))))))))
(trace multisubst)

; Tracing execution

; (multisubst 'love 'hate '(I hate comics and hate games))

;1 (cons 'I) (multisubst 'love 'hate '(hate comics and hate games))
;2 (cons 'I) (cons 'love) (multisubst 'love 'hate '(comics and hate games))
;3 (cons 'I) (cons 'love) (cons 'comics) (multisubst 'love 'hate '(and hate games))
;4 (cons 'I) (cons 'love) (cons 'comics) (cons 'and) (multisubst 'love 'hate '(hate games))
;5 (cons 'I) (cons 'love) (cons 'comics) (cons 'and) (cons 'love) (multisubst 'love 'hate '(games))
;6 (cons 'I) (cons 'love) (cons 'comics) (cons 'and) (cons 'love) (cons 'games) (multisubst 'love 'hate '())
;7 (cons 'I) (cons 'love) (cons 'comics) (cons 'and) (cons 'love) (cons 'games '())
;8 (cons 'I) (cons 'love) (cons 'comics) (cons 'and) (cons 'love '(games))
;9 (cons 'I) (cons 'love) (cons 'comics) (cons 'and '(love games))
;10 (cons 'I) (cons 'love) (cons 'comics (and love games))
;11 (cons 'I) (cons 'love (comics and love games))
;12 (cons 'I (love comics and love games))
;13 '(I love comics and love games))
