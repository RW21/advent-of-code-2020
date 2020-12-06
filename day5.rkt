#lang racket

(require data/integer-set)

(define input
  (for/list ([l (file->lines "input5.txt")])
    (string->list l)))

(define (get-id bsp)
  (let loop ([min-row 0]
             [max-row 127]
             [min-col 0]
             [max-col 7]
             [l bsp])
    (cond
      [(empty? l) (+ min-col (* min-row 8))]
      [(char=? (car l) #\F)
       (loop
        min-row
        (- max-row (ceiling (/ (- max-row min-row) 2)))
        min-col max-col (cdr l))]
      [(char=? (car l) #\B)
       (loop
        (+ min-row (ceiling (/ (- max-row min-row) 2)))
        max-row min-col max-col (cdr l))]
      [(char=? (car l) #\R)
       (loop
        min-row max-row
        (+ min-col (ceiling (/ (- max-col min-col) 2)))
        max-col (cdr l))]
      [(char=? (car l) #\L)
       (loop
        min-row max-row min-col
        (- max-col (ceiling (/ (- max-col min-col) 2)))
        (cdr l))])))

(define (part-a)
  (apply max (map get-id input)))

(part-a)

(define (part-b)
  (let loop ([id 95]
             [l (sort (map get-id input) <)])
    (if (= (car l) id)
        (loop (add1 id) (cdr l))
        id)))

(part-b)
     

  
       
