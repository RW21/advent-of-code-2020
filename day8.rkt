#lang racket

(struct instruction (operation argument))

(define instructions
  (for/vector ([l (file->lines "input8.txt")])
    (let ([split (string-split l)])
      (instruction (first split) (string->number (second split))))))


(define (acc-when-looped instructions)
  (let loop ([position 0]
             [acc 0]
             [saw empty])
    (cond
      [(= position (vector-length instructions)) (cons 'terminated acc)]
      [(memq position saw) acc]
      [(equal? (instruction-operation (vector-ref instructions position)) "nop")
       (loop (add1 position) acc (cons position saw))]
      [(equal? (instruction-operation (vector-ref instructions position)) "jmp")
       (loop (+ position (instruction-argument (vector-ref instructions position)))
             acc (cons position saw))]
      [(equal? (instruction-operation (vector-ref instructions position)) "acc")
       (loop (add1 position)
             (+ acc (instruction-argument (vector-ref instructions position)))
             (cons position saw))]
      )))

(acc-when-looped instructions)

(define (part-b)
  (for/or ([curr-inst instructions]
           [i (range (vector-length instructions))]
           #:when (or
                   (string=? (instruction-operation curr-inst) "jmp")
                   (string=? (instruction-operation curr-inst) "nop")))
           
           
    (define new-curr (vector-copy instructions))
    (define new-instructions
      (if (string=? (instruction-operation curr-inst) "jmp")
          (struct-copy instruction curr-inst [operation "nop"])
          (struct-copy instruction curr-inst [operation "jmp"])))
    (vector-set! new-curr i new-instructions)
    (match (acc-when-looped new-curr)
      [(cons 'terminated acc) acc]
      [_ #f])))

(part-b)
