#lang racket
(require rackunit)

(define input-map-test
  (map string->list (file->lines  "inputs/day3_test.txt")))

(define input-map
  (map string->list (file->lines  "inputs/day3.txt")))

(define (slope right down)
  (list right down))

(define (right-slope slope)
  (first slope))

(define (down-slope slope)
  (second slope))

(define tree #\#)

(define (tree? input-map row column)
  (equal? (list-ref (list-ref input-map row) column) tree))
   
(define (count-trees slope input-map)
  (define rows-slope (length input-map))
  (define columns-slope (length (car input-map)))
  (define (helper row column counter)
    (cond ((>= row rows-slope) counter)
          (else (helper (+ row (down-slope slope))
                        (modulo (+ column (right-slope slope)) columns-slope)
                        (if (tree? input-map row column)
                            (+ counter 1)
                            counter)))))
  (helper 0 0 0))

;; Make sure it works with the example provided
(check-equal? (count-trees (slope 3 1) input-map-test) 7)

(println "Part 1")
(count-trees (slope 3 1) input-map)

(println "Part 2")
(define input-slopes
  (list (slope 1 1) (slope 3 1) (slope 5 1) (slope 7 1) (slope 1 2)))

(apply * (map (λ (x) (count-trees x input-map)) input-slopes))
