#lang racket
(require rackunit)

(define (prepare-input input-file)
  (let* ((sorted-input (sort (file->list input-file) <))
         (max-input (last sorted-input)))
    (append (list 0) (append sorted-input (list (+ max-input 3))))))


(define (sum-jumps prepared-input)
  (define (sum-jumps* prepared-input one-jolt three-jolts)
    (cond [(< (length prepared-input) 2)
           (* one-jolt three-jolts)]
          [else
           (let ((diff (- (second prepared-input) (first prepared-input))))
             (cond [(equal? diff 1)
                    (sum-jumps* (cdr prepared-input) (+ one-jolt 1) three-jolts)]
                   [(equal? diff 3)
                    (sum-jumps* (cdr prepared-input) one-jolt (+ three-jolts 1))]
                   [else (sum-jumps* (cdr prepared-input) one-jolt three-jolts)]))]))
  (sum-jumps* prepared-input 0 0))

(println "Part 1")
(sum-jumps (prepare-input "inputs/day10.txt"))

;;tests
(check-equal? (sum-jumps (prepare-input "inputs/day10_test.txt")) 220)
