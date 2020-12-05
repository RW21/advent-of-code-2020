#lang racket

(define input (string-split (file->string "input4.txt") "\n\n"))
;(define input (string-split (file->string "test.txt") "\n\n"))

(define (contains-requirements passport)
  (define requirements '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))
  (andmap (λ (requirement)
            (string-contains? passport requirement))
          requirements))

(define (part-a)
  (count contains-requirements input))

(part-a)

(define (part-b)
  (define (valid? s)
    (match s
      [(regexp #px"iyr:([0-9]+)" (list _ year))
       (<= 2010 (string->number year) 2020)]
      [(regexp #px"byr:([0-9]+)" (list _ year))
       (<= 1920 (string->number year) 2002)]
      [(regexp #px"eyr:([0-9]+)" (list _ year))
       (<= 2020 (string->number year) 2030)]
      [(regexp #px"hgt:([0-9]+)(cm|in)" (list _ height unit))
       (cond
         [(string=? unit "cm") (<= 150 (string->number height) 193)]
         [(string=? unit "in") (<= 59 (string->number height) 76)])]
      [(regexp #px"hcl:#([0-9]+|[a-f]+){6}$") #t]
      [(regexp #px"ecl:(amb|blu|brn|gry|grn|hzl|oth)") #t]
      [(regexp #px"pid:([0-9]){9}$") #t]
      [(regexp #px"cid:([0-9]+)") #t]
      [_ #f]))
              
  (count (λ (passport)
           (and
            (andmap valid? (string-split passport))
            (contains-requirements passport)))
         input))

(part-b)
