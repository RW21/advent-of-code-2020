#lang racket

(define l
  (for/list ([line (file->lines "input1.txt")])
    (string->number line)))

(define h (list->set l))

(filter (λ (n)
          (if (set-member? h (- 2020 n))
              #t
              #f
              )) l)

(for* ([i l]
      [j l]
      [k l]
      #:when (= 2020 (+ i j k)))
  (println i)
  (println j)
  (println k))
