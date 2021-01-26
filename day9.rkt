#lang racket

(define input
  (for/vector ([line (file->lines "input9.txt")])
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
    
    
(part-a)

(define (part-b)
  (let ([target (part-a)])
    (for ([n (vector-length input)])
