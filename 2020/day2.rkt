#lang racket

(define (extract-specifications line)
  (match line
    [(regexp #rx"^([0-9]+)-([0-9]+) ([a-z]+): ([a-z]+)$" (list _ min max letter password))
     (list min max letter password)]))

(define (count-letter-occurrences word letter)
  (define (helper word letter letter-count)
    (cond ((empty? word) letter-count)
          ((equal? (car word) letter)
           (helper (cdr word) letter (+ letter-count 1)))
          (else (helper (cdr word) letter letter-count))))
  (helper (string->list word) (car (string->list letter)) 0))

(define get-input
  (file->lines "inputs/day2.txt"))

(define (min-spec spec)
  (string->number (car spec)))

(define (max-spec spec)
  (string->number (cadr spec)))

(define (letter-spec spec)
  (caddr spec))

(define (password-spec spec)
  (cadddr spec))

(define (valid-passwords-part1 input)
  (define (helper input counter)
    (if (empty? input)
        counter
        (let* ((specification (extract-specifications (car input)))
               (min-letter (min-spec specification))
               (max-letter (max-spec specification))
               (letter (letter-spec specification))
               (password (password-spec specification))
               (letter-occurrences (count-letter-occurrences password letter)))
          (if (and (>= letter-occurrences min-letter) (<= letter-occurrences max-letter))
               (helper (cdr input) (+ counter 1))
               (helper (cdr input) counter)))))

  (helper input 0))

(valid-passwords-part1 get-input)

(define (valid-passwords-part2 input)
  (define (helper input counter)
    (if (empty? input)
        counter
        (let* ((specification (extract-specifications (car input)))
               (min-letter-pos (min-spec specification))
               (max-letter-pos (max-spec specification))
               (letter (car (string->list (letter-spec specification))))
               (password (password-spec specification))
               (min-letter (string-ref password (- min-letter-pos 1)))
               (max-letter (string-ref password (- max-letter-pos 1))))
          (if (xor (equal? letter min-letter) (equal? letter max-letter))
              (helper (cdr input) (+ 1 counter))
              (helper (cdr input) counter)))))
    (helper input 0))


(valid-passwords-part2 get-input)
