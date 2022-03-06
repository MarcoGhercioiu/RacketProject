#lang racket

(provide (all-defined-out))

(define ((divisible-by-x? x) y)
(zero? (modulo y x)))

(define (function-9 λ)
  (λ 9))

(define (my-map λ l)
(cond
  [(empty? l) '()]
  [else
        (cons (λ (first l)) (my-map λ (rest l)))]
 ))

(define (pair-up l1 l2)
(cond
  [(or(empty? l1) (empty? l2)) '()]
  [else
        (cons (list (first l1) (first l2)) (pair-up (rest l1) (rest l2)))]
 ))



(define (classify λ l)
  (sublists λ l '() '())
 )

(define (sublists λ l1 even odd)
  (cond
    [(empty? l1) (list even odd)]
    [(λ (first l1))  (sublists λ (rest l1) (append even (list (first l1))) odd)]
    [else (sublists λ (rest l1) even (append odd (list (first l1))))]
  )
  )



(define (is-member? num list)
  (cond
    [(empty? list) #f]
    [(equal? num (first list)) #t]
    [else (is-member? num (rest list))]
    )
 )


(define (my-sorted? λ list)
    (cond
    [(empty? (rest list)) #t]
    [(λ (first list) (first (rest list))) (my-sorted? λ (rest list))]
    [else #f]
    )
 )



(define (my-flatten lit)
  (cond
    [(empty? lit) '()]
    [(pair? lit) (append (my-flatten (first lit)) (my-flatten (rest lit)))]
    [else (list lit)]
   )
)



(define (upper-threshold list thresh)
    (cond
    [(empty? list) '()]
    [(< (first list) thresh) (cons (first list) (upper-threshold (rest list) thresh))]
    [else (upper-threshold (rest list) thresh)]
    )
 )


(define (my-list-ref list num)
  (cond
    [(empty? list) (raise-syntax-error 'ERROR "Index out of bounds")]
    [(equal? num 0) (first list)]
    [else (my-list-ref (rest list) (sub1 num))]
    )
  
  )


(define (deep-reverse lit)
  (cond
    [(empty? lit) '()]
    [(pair? lit) (append (deep-reverse (rest lit)) (list (deep-reverse (first lit))))]
    [else lit]
   )
)
















