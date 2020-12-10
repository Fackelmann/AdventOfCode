#lang racket

(define get_input
  (file->list "inputs/day1.txt"))

(define (find-number input number)
  (cond ((empty? input) #f)
        ((equal? (car input) number) #t)
        (else (find-number (cdr input) number))))

(define (add-two-sum input total)
  (let* ((curr-num (car input))
         (comp-num (- total curr-num)))
    (cond ((equal? (length input) 1) #f)
          ((find-number (cdr input) comp-num)
           (list curr-num comp-num))
          (else (add-two-sum (cdr input) total)))))

(println "Part 1")
(define part1-numbers (add-two-sum get_input 2020))
(println part1-numbers)
(* (car part1-numbers) (cadr part1-numbers))


(define (add-three-sum input total)
  (let* ((num1 (car input))
         (comp-num (- total num1))
         (nums (add-two-sum (cdr input) comp-num)))
    (cond ((equal? (length input) 2) #f)
          (nums (cons num1 nums))
          (else (add-three-sum (cdr input) total)))))

(println "Part 2")
(define part2-numbers (add-three-sum get_input 2020))
(println part2-numbers)
(* (car part2-numbers) (cadr part2-numbers) (caddr part2-numbers))
