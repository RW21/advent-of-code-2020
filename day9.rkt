#lang racket

(define input
  (for/vector ([line (file->lines "input9.txt")])
;  (for/vector ([line (file->lines "test.txt")])
    (string->number line)))

(define (part-a)
  (define (verify num index)
    (cond
      [(<= index 25) #t]
      [else (member num (map (λ (l) (apply + l))
                               (sequence->list
                               (in-combinations
                                (sequence->list (in-vector input (- index 25) index))
                                2))))]))

  (for/first ([n input]
              [i (range (vector-length input))]
              #:unless (verify n i))
    n))
    
(define invalid-num (part-a))
(println invalid-num)
(define input-length (vector-length input))

(define (valid-set start)
  (let loop ([size 0]
             [sum 0])
    (cond
      [(>= (+ start size) input-length) #f]
      [(= (+ sum (vector-ref input (+ start size))) invalid-num) size]
      [else (loop (add1 size) (+ sum (vector-ref input (+ start size))))]
      )))

(define (part-b)
  (for/first ([i (in-range input-length)]
              #:when (valid-set i))
    (let ([result-range (vector-sort (vector-copy input i (+ i (valid-set i))) <)])
      (+ (vector-ref result-range 0) (vector-ref result-range (sub1 (vector-length result-range))))
      )))

(part-b)

