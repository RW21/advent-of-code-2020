#lang racket

(define l 0)
(+ (call/cc
    (λ (k)
      (begin
        (set! l k)
        (k (* 3 4)))))
   5)


