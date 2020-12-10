#lang racket
(require rackunit)

;; Make the struct transparent to support equal?
(struct instruction (operator operand) #:transparent)
(struct result (finished acc))

(define (input->vector input-file)
  (list->vector (for/list ([line (file->lines input-file)])
                  (match (string-split line)
                    [(list operator operand)
                     (instruction operator operand)]))))


(define (execute-program program)  
  (define (execute-instruction program pointer accumulator visited)
    (cond [(set-member? visited pointer)
           (result #f accumulator)]
          [(equal? pointer (vector-length program))
           (result #t accumulator)]
          [else (let* ((instruction (vector-ref program pointer))
                       (operator (instruction-operator instruction))
                       (operand (instruction-operand instruction)))               
                  (cond [(equal? operator "nop") (execute-instruction program
                                                                      (+ pointer 1)
                                                                      accumulator
                                                                      (set-add visited pointer))]
                        [(equal? operator "jmp") (execute-instruction program
                                                                      (+ pointer (string->number operand))
                                                                      accumulator
                                                                      (set-add visited pointer))]
                        [(equal? operator "acc") (execute-instruction program
                                                                      (+ pointer 1)
                                                                      (+ accumulator (string->number operand))
                                                                      (set-add visited pointer))]
                        [else (println "Invalid operator")]))]))
  (execute-instruction program 0 0 (set empty)))


(define (fix-program program)
  (for/or ([current-instruction program]
        #:when (member (instruction-operator current-instruction) '("nop" "jmp")))
    (let ((index (vector-member current-instruction program))
          (new-program (vector-copy program)))
      (vector-set! new-program index (if (equal? (instruction-operator current-instruction) "nop")
                                         (instruction "jmp" (instruction-operand current-instruction))
                                         (instruction "nop" (instruction-operand current-instruction))))
      (let ((result (execute-program new-program)))
       (if (equal? (result-finished result) #f)
            #f
            (result-acc result))))))
                                                               
    
  
(println "Part 1")
(result-acc (execute-program (input->vector "inputs/day8.txt")))
(println "Part 2")
(fix-program (input->vector "inputs/day8.txt"))
;;Test
(check-equal? (result-acc (execute-program (input->vector "inputs/day8_test.txt"))) 5)
(check-equal? (fix-program (input->vector "inputs/day8_test.txt")) 8)
