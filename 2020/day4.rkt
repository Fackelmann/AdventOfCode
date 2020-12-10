#lang racket
(require rackunit)

(define input-hashes
  (for/list ([ line (string-split (file->string  "inputs/day4.txt") #rx"\n\n+") ])
    (for/hash ([elem (string-split line)])
      (apply values (string-split elem ":")))))

(define (passport-has-all-fields? hash-passport)
  (and (hash-has-key? hash-passport "byr")
       (hash-has-key? hash-passport "iyr")
       (hash-has-key? hash-passport "eyr")
       (hash-has-key? hash-passport "hgt")
       (hash-has-key? hash-passport "hcl")
       (hash-has-key? hash-passport "ecl")
       (hash-has-key? hash-passport "pid")))

(define (passport-byr-valid? hash-passport)
  (if (equal? (hash-has-key? hash-passport "byr") #f)
      #f
      (let ((byr (hash-ref hash-passport "byr")))
        (and (equal? (string-length byr) 4)
             (>= (string->number byr) 1920)
             (<= (string->number byr) 2002)))))

(define (passport-iyr-valid? hash-passport)
  (if (equal? (hash-has-key? hash-passport "iyr") #f)
      #f
      (let ((iyr (hash-ref hash-passport "iyr")))
        (and (equal? (string-length iyr) 4)
             (>= (string->number iyr) 2010)
             (<= (string->number iyr) 2020)))))

(define (passport-eyr-valid? hash-passport)
  (if (equal? (hash-has-key? hash-passport "eyr") #f)
      #f
      (let ((eyr (hash-ref hash-passport "eyr")))
        (and (equal? (string-length eyr) 4)
             (>= (string->number eyr) 2020)
             (<= (string->number eyr) 2030)))))

(define (passport-hgt-valid? hash-passport)
  (if (equal? (hash-has-key? hash-passport "hgt") #f)
      #f
      (let* ((hgt (hash-ref hash-passport "hgt"))
             (hgt-cm (regexp-match #rx"([0-9]+)cm" hgt))
             (hgt-in (regexp-match #rx"([0-9]+)in" hgt)))
        (cond (hgt-cm (and (>= (string->number (cadr hgt-cm)) 150)
                           (<= (string->number (cadr hgt-cm)) 193)))
              (hgt-in (and (>= (string->number (cadr hgt-in)) 59)
                                (<= (string->number (cadr hgt-in)) 76)))
              (else #f)))))

(define (passport-hcl-valid? hash-passport)
  (if (equal? (hash-has-key? hash-passport "hcl") #f)
      #f
      (let* ((hcl (hash-ref hash-passport "hcl"))
             (hcl-id (regexp-match #rx"#([0-9a-f]+)" hcl)))
        (if (equal? hcl-id #f)
            #f
            (equal? (string-length (cadr hcl-id)) 6)))))

(define (passport-ecl-valid? hash-passport)
  (if (equal? (hash-has-key? hash-passport "ecl") #f)
      #f
      (let ((ecl (hash-ref hash-passport "ecl")))
        (or (equal? ecl "amb")
            (equal? ecl "blu")
            (equal? ecl "brn")
            (equal? ecl "gry")
            (equal? ecl "grn")
            (equal? ecl "hzl")
            (equal? ecl "oth")))))

(define (passport-pid-valid? hash-passport)
  (if (equal? (hash-has-key? hash-passport "pid") #f)
      #f
      (let* ((pid (hash-ref hash-passport "pid"))
             (pid-id (regexp-match #rx"^([0-9]+)$" pid)))
        (if (equal? pid-id #f)
            #f
            (equal? (string-length (cadr pid-id)) 9)))))

(define (number-valid-passports input-hashes)
  (for/sum ([input-hash input-hashes])
    (if (passport-has-all-fields? input-hash)
        1
        0)))

(define (number-valid-passports-part2 input-hashes)
  (for/sum ([input-hash input-hashes])
    (if (and (passport-byr-valid? input-hash)
             (passport-iyr-valid? input-hash)
             (passport-eyr-valid? input-hash)
             (passport-hgt-valid? input-hash)
             (passport-hcl-valid? input-hash)
             (passport-ecl-valid? input-hash)
             (passport-pid-valid? input-hash))
        1
        0)))

(println "Part 1")
(number-valid-passports input-hashes)

(println "Part 2")
(number-valid-passports-part2 input-hashes)


;; Tests
(define passport
  '("iyr:2009" "eyr:1937" "ecl:#bfd0ee" "byr:1964" "hcl:#733820" "ecl:blu" "hgt:169cm" "pid:331814490"))

(define ht (for/hash ([elem passport])
             (apply values (string-split elem ":"))))

(check-true (passport-byr-valid? ht))
(check-false (passport-iyr-valid? ht))
(check-false (passport-eyr-valid? ht))
(check-true (passport-hgt-valid? ht))
(check-true (passport-hcl-valid? ht))
(check-true (passport-ecl-valid? ht))
(check-true (passport-pid-valid? ht))
