
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; 1
(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      null))

;; 2
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

;; 3
(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))

;; 4
(define (stream-for-n-steps s n)
  (if (= n 0)
        null
        (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;; 5
(define funny-number-stream 
  (letrec ([f (lambda (count) 
                (cons (if (= 0 (remainder count 5)) (* -1 count) count) (lambda () (f (+ count 1)))))])
    (lambda () (f 1))))

;; 6
(define dan-then-dog
  (letrec ([f (lambda (sgn) 
                (cons (if (= sgn 1) "dan.jpg" "dog.jpg") (lambda () (f (* -1 sgn)))))])
    (lambda () (f 1))))

;; 7
(define (stream-add-zero s)
  (letrec ([f (lambda (s) 
                (cons (cons 0 (car (s))) (lambda () (f (cdr (s))))))])
    (lambda () (f s))))

;; 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (cnt) 
                (cons 
                 (cons (list-nth-mod xs cnt) (list-nth-mod ys cnt)) 
                 (lambda () (f (+ cnt 1)))))])
    (lambda () (f 0))))

;; 9
(define (vector-assoc v vec)
  (letrec ([f (lambda (pos) 
                (if (>= pos (vector-length v)) 
                    #f
                    (if (pair? (vector-ref v pos)) 
                        (if (equal? (car (vector-ref v pos)) v) 
                            (vector-ref v pos) 
                            (f (+ pos 1))) 
                        (f (+ pos 1)))))])
    (f 0)))

;; 10
(define (cached-assoc xs n)
  (letrec () 
    ()))


;;;; test
(define xs (cons "a" (cons "b" (cons "c" null))))
(define ys (cons 1 (cons 2 null)))
(define t (cycle-lists xs ys))