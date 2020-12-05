#lang racket

(define input (file->lines "input3.txt"))
(define l-length (string-length (car input)))

(define l
  (for/list ([line input])
    (list->set
     (for/list ([i (in-range 0 l-length)]
                [j line]
                #:when (char=? j #\#))
       i))))

(define (part-a right down l)
  (let loop ([l (cdr l)]
             [pos 0]
             [res 0])
    (let ([new-pos (modulo (+ pos right) l-length)])
          (cond
            [(empty? l) res]
            [(set-member? (car l) new-pos)
             (loop (cdr l) new-pos (add1 res))]
            [else (loop (cdr l) new-pos res)]))
      ))

(part-a 3 1 l)

(define (part-b)
  (define l-odd-index
    (for/list ([i (length l)] #:when (even? i))
      (list-ref l i))
    )
  (* (part-a 1 1 l)
     (part-a 3 1 l)
     (part-a 5 1 l)
     (part-a 7 1 l)
     (part-a 1 2 l-odd-index))
)
    
(part-b)
