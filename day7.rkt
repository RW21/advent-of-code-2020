#lang racket

(define ht (make-hash))

(define (parse-rule str)
  (match-define (list _ bag insides)
    (regexp-match #px"(.+) bags contain (.+)." str))
  (hash-set! ht
             bag
             (for/set ([inside (string-split insides ",")])
               inside)))

(for ([l (file->lines "input7.txt")])
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

(define (build-bag-string b)
  (string-append
   (third b) " " (fourth b)))

(for ([a (hash-ref ht "shiny gold")])
  (display (build-bag-string (parse-bag a))))

 
(define (part-b)
  (define (search bag-name)
    (for/sum ([bag (hash-ref ht bag-name)])
      (cond
        [(string=? bag "no other bags") 0]
        [else
         (let ([n (string->number (second (parse-bag bag)))]
               [bag-string (build-bag-string (parse-bag bag))])
           (+ n
              (* n (search bag-string))))])))

  (search "shiny gold"))

(part-b)

          
                    
  
