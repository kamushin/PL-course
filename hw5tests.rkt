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
(check-equal? (int 43)
             (eval-exp (call (fun "incr" "x" (add (var "x") (int 1))) (int 42))))

(check-equal? (eval-exp (mlet "f1"
                              (fun "f1" "a"
                                   (mlet "x" (var "a")
                                          (fun "f2" "z" (add (var "x") (int 1)))))
                              (mlet "f3"
                                    (fun "f3" "f"
                                         (mlet "x" (int 1729)
                                               (call (var "f") (aunit))))
                                    (call (var "f3") (call (var "f1") (int 1))))))
              (int 2))

;; curring
(define addtwo (fun "addone" "x" (add (var "x") (int 2))))
(define mupl-map-addtwo (call mupl-map addtwo))
(check-equal? (eval-exp (call mupl-map-addtwo (aunit))) (aunit))
(define my-mupl-list (apair (int 23) (apair (int 42) (aunit))))
(define my-answers (apair (int 25) (apair (int 44) (aunit))))
(check-equal? (eval-exp (call mupl-map-addtwo my-mupl-list)) my-answers)