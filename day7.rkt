#lang racket

(define ht (make-hash))

(define (parse-rule str)
  (match-define (list _ bag insides)
    (regexp-match #px"(.+) bags contain (.+)." str))
  (hash-set! ht
             bag
             (for/set ([inside (string-split insides ",")])
               inside)))

(for ([l (file->lines "test.txt")])
  (parse-rule l))


;; 3 bright white bags
(define (parse-bag str)
  (regexp-match #px"([\\d])\\s([\\w]+)\\s([\\w]+)\\s(bag(s?))" str))
   

(define (part-a)
  (define (search bag-set)
    (for/or ([bag bag-set])
      (cond
        [(string=? bag "no other bags") #f]
        [(and
          (string=? (third (parse-bag bag)) "shiny")
          (string=? (fourth (parse-bag bag)) "gold"))
         #t]
        [else
         (search (hash-ref ht
                           (string-append
                            (third (parse-bag bag))
                            " "
                            (fourth (parse-bag bag)))
                                   ))])))
  (count search (hash-values ht)))
         
(part-a)


(define (part-b)
  (inside-count (hash-ref ht "shiny gold")))

(part-b)
    
      
