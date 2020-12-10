#lang racket

(define (node name [neighbors empty])
  (list name neighbors))

(define (node-name node)
  (first node))

(define (node-neighbors node)
  (second node))

(define (node-add-neighbor node neighbor)
  (cons neighbor (node-neighbors node)))

(define (neighbor name weight)
  (list name weight))

(define (neighbor-name neighbor)
  (first neighbor))

(define (neighbor-weight neighbor)
  (second neighbor))

(define make-graph
  make-hash)

(define (add-node graph node)
  (hash-set! graph (node-name node) (node-neighbors node)))

(define (parse-input-line input-line)
  (let ((parsed-input (regexp-match #rx"^(.*) bags? contain (.*)." input-line)))
    (cdr parsed-input)))

(define (parse-neighbor neighbor)
  (let* ((parsed-neighbor (regexp-match #rx"^[ ]*([0-9]) (.*) bags?" neighbor))
         (neighbor-name (third parsed-neighbor))
         (neighbor-weight (second parsed-neighbor)))
    (list neighbor-name neighbor-weight)))

(define (get-node-input parsed-input)
  (first parsed-input))

(define (get-neighbors parsed-input)
  (let ((neighbors (string-split (car (rest parsed-input)) ",")))
    (cond ((and (= (length neighbors) 1)
		(equal? (first neighbors) "no other bags"))
           empty)
	  (else
	   (for/list ([neighbor neighbors])
             (parse-neighbor neighbor))))))


(define (parse-input input-file)
  (let ((lines (file->lines "inputs/day7_test.txt")))))

(define (make-graph-input input-file)
  (let* ((lines (file->lines input-file))
         (parsed-lines (map (λ (x) (parse-input-line x)) lines)))
    (for/hash ([parsed-line parsed-lines])
      (values (get-node-input parsed-line) (get-neighbors parsed-line)))))

(define (DFS graph node dest)
  (define counter 0)
  (define (DFS* node discovered)
    (set-add! discovered node)
    (for ([neighbor (hash-ref graph node)])
      (cond ((equal? (neighbor-name neighbor) dest)
             (set! counter (+ counter 1)))
            ((equal? (set-member? discovered (neighbor-name neighbor)) #f)
             (DFS* (neighbor-name neighbor) discovered)))))
  (DFS* node (mutable-set empty))
  (if (> counter 0)
      1
      0))


(define (get-bags input-file)
  (let ((graph (make-graph-input input-file)))
    (for/sum ([bag (hash-keys graph)])
      (DFS graph bag "shiny gold"))))

;; And then we also need parsing 
(provide node
	 node-name
	 node-neighbors
	 neighbor-name
         neighbor-weight
	 make-graph
	 add-node)

