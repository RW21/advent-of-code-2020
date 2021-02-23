#lang racket

(define input
  (for/set ([line (file->lines "input10.txt")])
    (string->number line)))

(define max-joltage (apply max (in-set input)))

(define part1
  (let loop [(ones 0)
             (threes 0)
             (curr 0)]
    (cond
      [(set-member? input (add1 curr)) (loop (add1 ones) threes (add1 curr))]
      [(set-member? input (+ 3 curr)) (loop ones (add1 threes) (+ 3 curr))]
      [(= curr max-joltage) (* ones (add1 threes))])))

part1

(define part2
  (define ht (make-hash))
  (for ([i (in-set input)])
    (hash-set! ht i 0))
  (define (num-of-acceptable n)
    (count identity
           (for/list ([i '(1 2 3)])
             (hash-has-key? ht (- n i)))))
  (for ([i (in-range max-joltage)]
        #:when (hash-has-key? ht i))
    (define n (num-of-acceptable i))
    (cond
      [(= n 3) (hash-update! ht i 
    





