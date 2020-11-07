#lang racket

;; Credit by Sihan Li.

;; (produce-EEA a b) consumes two integers, a, b. Apply EEA to compute gcd(a,b)
;;   and give a certificate of correctness for gcd(a, b).

;; produce-EEA: Int Int -> Void
(define (produce-EEA a b)
  (void (pretty-write (list 1 0 a 0))
        (produce-EEA-table 1 0 a 0 0 1 b 0)))


;; produce-EEA-table: Int Int Int Int Int Int Int Int -> Void
(define (produce-EEA-table x1 y1 r1 q1 x2 y2 r2 q2)
  (cond [(= 0 r2) (pretty-write (list x2 y2 r2 q2))]
        [else
         (local [(define new-q (quotient r1 r2))]
           (void (pretty-write (list x2 y2 r2 q2))
                 (produce-EEA-table x2 y2 r2 q2
                                    (- x1 (* x2 new-q))
                                    (- y1 (* y2 new-q))
                                    (- r1 (* r2 new-q))
                                    new-q)))]))

;; Examples:
(produce-EEA 2172 423)