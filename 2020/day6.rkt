#lang racket
(require rackunit)

(define (input-hashes input-file)
  (for/list ([ group (string-split (file->string  input-file) #rx"\n\n+") ])
    (for/hash ([person (flatten (map string->list (string-split group)))])
      (values person empty))))

(define (any-different-answers input-file)
  (apply + (map (λ (x) (length (hash-keys x))) (input-hashes input-file))))

;; Nope, this is not what they are asking
(define (every-different-answers input-file)
  (length (hash-keys 
           (for/hash ([key (flatten (map  (λ (x) (hash-keys x)) (input-hashes "inputs/day6_test.txt")))])
             (values key empty)))))


;; I originally wrote it using hash tables (couldn't find the hash set!). First part worked reasonably well, but the second part was getting too complicated. I rewrote it using stes with inspiration from Matthew Butteric
(define (count-answers input-file set-proc)
  (for/sum ([ group (string-split (file->string  input-file) #rx"\n\n+") ])
    (set-count (apply set-proc (for/list ([person (string-split group)])
                                 (for/set ([answer person])
                                   answer))))))
    


(println "Part 1")
(count-answers "inputs/day6.txt" set-union)

(println "Part 2")
(count-answers "inputs/day6.txt" set-intersect)

;; Tests
;;(check-equal? (any-different-answers "inputs/day6_test.txt") 11)
;;(check-equal? (every-different-answers "inputs/day6_test.txt") 6)

(check-equal? (count-answers "inputs/day6_test.txt" set-union) 11)
(check-equal? (count-answers "inputs/day6_test.txt" set-intersect) 6)
