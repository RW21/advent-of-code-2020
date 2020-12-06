#lang racket

(require threading)

(define input
  (map string->list (string-split (file->string "input6.txt") "\n\n")))

(define (part-a)
  (for/sum ([l input])
    (length (remove-duplicates (filter (λ (c) (not (char=? #\newline c))) l)))))

(part-a)

(define (part-b)
  (for/sum ([l input])
    (~>>
    (for/list ([person (string-split (list->string l) "\n")])
      person)
    (map string->list)
    (map list->set)
    (apply set-intersect)
    set-count
    )))

(part-b)
