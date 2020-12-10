#lang racket
(require rackunit)

;; The boarding pass is a binary number where F is 0 and B is 1
(define (id? boarding-pass)
  (let-values ([(row-list column-list) (split-at (string->list boarding-pass) 7)])
    (+ (* (board->int row-list #\B) 8) (board->int column-list #\R))))

(define (board->int board-list one)
  (define (helper board-list n counter)
    (if (empty? board-list)
        counter
        (helper (cdr board-list) (- n 1) (if (equal? (first board-list) one)
                                             (+ counter (expt 2 n))
                                             counter))))
  (helper board-list (- (length board-list) 1) 0))

(define (highest-number xs)
  (define (max x1 x2)
    (if (> x1 x2) x1 x2))
  (foldl max (first xs) (rest xs)))

(define (find-first-space input-list)
  (let ((current (first input-list))
        (next (second input-list)))
    (if (not (equal? (- next current) 1))
        (+ current 1)
        (find-first-space (rest input-list)))))

(define my-id
  (find-first-space (sort (map (λ (x) (id? x)) (file->lines "inputs/day5.txt")) <)))


(println "Part 1")
(highest-number (map (λ (x) (id? x)) (file->lines "inputs/day5.txt")))

(println "Part 2")
my-id

;; Tests
(check-equal? (board->int (string->list "BFFFBBF") #\B) 70)
(check-equal? (board->int (string->list "RRR") #\R) 7)

(check-equal? (id? "BFFFBBFRRR") 567)
(check-equal? (id? "FFFBBBFRRR") 119)
(check-equal? (id? "BBFFBBFRLL") 820)
(check-equal? (find-first-space '(1 2 4 5 6 7)) 3)
