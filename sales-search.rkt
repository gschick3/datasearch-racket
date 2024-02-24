#lang racket
(require data/maybe)

(define search-criteria '("name" "date" "publisher" "region" "genre" "search"))

(define (read-criterion)
  (let ([input (string-downcase (read-line (current-input-port) 'any))])
    (if (member input search-criteria) (just input)
        (begin
          (displayln "Invalid criterion")
          nothing))))

(define (display-criteria)
  (for-each (lambda (s) (printf "~a    " s)) search-criteria)
  (displayln ""))

(define (add-to-list lst)
  (lambda (n) (append lst (list n))))

(define (get-user-input)
  (define (loop criteria)
    (cond
      [(and (cons? criteria) (equal? (last criteria) "search")) (drop-right criteria 1)]
      [(= (length criteria) 3) criteria]
      [else (loop (maybe criteria (add-to-list criteria) (read-criterion)))]))
  (begin
    (display-criteria)
    (loop '())))