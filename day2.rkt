#lang racket

(define l
  (for/list ([line (file->lines "input2.txt")])
    line))

(define (valid?/a s)
  (define-values (_ min-possible max-possible char target)
    (apply values
           (regexp-match #rx"([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)" s)))
  (<= (string->number min-possible)
      (count (λ (c) (string=? (make-string 1 c) char)) (string->list target))
      (string->number max-possible)
      ))

(define (valid?/b s)
  (define-values (_ index-1 index-2 char target)
    (apply values
           (regexp-match #rx"([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)" s)))
  (define (valid-position? index)
    (let ([index (string->number index)])
      (cond
        [(< (string-length target) index) #f]
        [(char=? (string-ref target (sub1 index)) (car (string->list char))) #t]
        [else #f])))
  (xor (valid-position? index-1)
       (valid-position? index-2)))

(define (part-a)
  (count valid?/a l))

(define (part-b)
  (count valid?/b l))
  

(part-a)
(part-b)
