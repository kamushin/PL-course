#lang racket

(require "hw5.rkt")

; a test case that uses problems 1, 2, and 4
; should produce (list (int 10) (int 11) (int 16))
; (define test1
;  (mupllist->racketlist
;   (eval-exp (call (call mupl-mapAddN (int 7))
;                   (racketlist->mupllist 
;                    (list (int 3) (int 4) (int 9)))))))

(require rackunit "hw5.rkt")

;; shadowing
(check-equal? (eval-exp (mlet "a" (int 1) (mlet "a" (int 2) (int 2)))) 
              (int 2))
(check-equal? (eval-exp (mlet "a" (int 1) 
                              (mlet "test" (fun "tmp" "b" (mlet "a" (int 2) (var "a"))) 
                                    (call (var "test") (int 1)))))
              (int 2))
