#lang racket
(require rackunit)

(define (input->vector input-file)
  (list->vector (file->list input-file)))

(define (is-valid? vector number)
  (for/first ([pair (in-combinations (vector->list vector))]
	      #:when (equal? (apply + pair) number))
    #t))
    
(define (first-invalid-input input-file preamble)
  (let ((input-vector (input->vector input-file)))
    (for/first ([idx (- (vector-length input-vector) preamble)]
		#:unless (is-valid? (vector-copy input-vector idx (+ idx preamble))
				 (vector-ref input-vector (+ idx preamble))))
      (vector-ref input-vector (+ idx preamble)))))
      

;; Part 1
;; Brute force solution; check all the #preamble choose 2 combinations of the previous #preamble numbers 
(first-invalid-input "inputs/day9.txt" 25)

(check-equal? (first-invalid-input "inputs/day9_test.txt" 5) 127)
