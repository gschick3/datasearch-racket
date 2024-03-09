#lang racket
(require data/maybe)
(require csv-reading)

; USER INPUT

(define (get-user-input)
  (string-downcase (read-line (current-input-port) 'any)))

(define (get-criterion-input)
  (display "Enter next search category: ")
  (get-user-input))

(define (get-query-input criterion)
  (display "Enter search term: ")
  (get-user-input))

; get date range by regex
(define (extract-date-range s)
  (let ([dates (map string->number (regexp-match* (pregexp "\\d{4}") s))])
    (if (>= (length dates) 2)
        (just (sort (take dates 2) <))
        (begin (displayln "Invalid date") nothing))))

; add value to list at key
(define (add-to-hash hsh key val)
  (hash-set hsh key (append (hash-ref hsh key) (list val))))

(define (display-list lst)
  (for-each (lambda (s) (printf "~s   " s)) lst)
  (displayln ""))

; get user-defined search terms
(define (get-search-terms)
  (define search-criteria (make-immutable-hash '(["name"] ["year"] ["publisher"] ["region"] ["genre"])))
  (define (loop criteria input)
    (cond
      [(equal? input "search") criteria]
      [(= (length (append* (hash-values criteria))) 3)
       (begin (displayln "Capped at 3 criteria") criteria)]
      [(hash-has-key? criteria input)
       (loop (add-to-hash criteria input (get-query-input input))
             (get-criterion-input))]
      [else (loop criteria (get-criterion-input))]))
  (begin
    (displayln "Regions: North America, Europe, Japan, Rest of World, Global")
    (displayln "Year: Enter start and end years")
    (displayln "")
    (display "Search criteria: ")
    (display-list (hash-keys search-criteria))
    (displayln "Enter 'search' to search with current parameters")
    (loop search-criteria (get-criterion-input))))

; get user-defined sort-by columns
(define (get-sort-col)
  (define sort-criteria '("rank" "review"))
  (begin
    (display-list sort-criteria)
    (display "Enter sorting criteria (default rank): " )
    (let ([ui (get-user-input)])
      (if (member ui sort-criteria)
          ui "rank"))))


; FILTERING

; general filter function
(define (filter-by-string csv lst col-func)
  (filter (lambda (line)
            (ormap (lambda (value)
                     (string-contains? (string-downcase (col-func line)) (string-downcase value)))
                   lst))
          csv))

; specific filter functions
(define (filter-by-name csv name-list)
  (filter-by-string csv name-list third))

(define (filter-by-publisher csv publisher-list)
  (filter-by-string csv publisher-list seventh))

(define (filter-by-genre csv genre-list)
  (filter-by-string csv genre-list sixth))

(define (get-region-col-func str)
  (cond
    [(string-ci=? str "North America") eighth]
    [(string-ci=? str "Europe") ninth]
    [(string-ci=? str "Japan") tenth]
    [(string-ci=? str "Rest of World") (lambda (lst) (list-ref lst 11))]
    [(string-ci=? str "Global") (lambda (lst) (list-ref lst 12))]))

(define (filter-by-region csv region-list)
  (filter (lambda (line)
            (ormap (lambda (region)
                     (> (string->number ((get-region-col-func region) line)) 0))
                   region-list))
          csv))

(define (filter-by-year csv year-list)
  (define year-ranges (map-maybe extract-date-range year-list))
  (filter (lambda (line)
            (ormap (lambda (year-range)
                     (<= (first year-range) (string->number (fifth line)) (second year-range)))
                   year-ranges))
          csv))

; very general filter function
(define (filter-by csv col-name value-list)
  (cond
    [(empty? value-list) csv]
    [(string-ci=? col-name "name") (filter-by-name csv value-list)]
    [(string-ci=? col-name "publisher") (filter-by-publisher csv value-list)]
    [(string-ci=? col-name "genre") (filter-by-genre csv value-list)]
    [(string-ci=? col-name "region") (filter-by-region csv value-list)]
    [(string-ci=? col-name "year") (filter-by-year csv value-list)]
    [else csv]))

; apply all filters specified by hash
(define (filter-all csv filter-hash)
  (define (loop csv keys)
    (cond
      [(empty? keys) csv]
      [(empty? (hash-ref filter-hash (first keys))) (loop csv (rest keys))]
      [else (loop (filter-by csv (first keys) (hash-ref filter-hash (first keys))) (rest keys))]))
  (loop csv (hash-keys filter-hash)))

; SORTING

(define (sort-by csv col-name)
  (let ([col-func (cond [(string-ci=? col-name "rank") second]
                        [(string-ci=? col-name "review") last])])
    (sort csv (lambda (a b) (< (string->number (col-func a)) (string->number (col-func b)))))))

; DRIVER

(define (main-loop)
  (define csv-list (csv->list (open-input-file "Video Games Sales.csv")))
  (define csv-header (first (take csv-list 1)))
  (define csv-lines (drop csv-list 1))

  (let ([result (sort-by
                 (filter-all csv-lines (get-search-terms))
                 (get-sort-col))])
    (begin
      (if (cons? result)
          (begin
            (map display-list result)
            (display-list csv-header))
          (displayln "No results found."))
      (when (string-prefix? "yes"
                            (begin (display "Would you like to search again? (y/n) ")
                                   (get-user-input)))
        (main-loop)))))

(main-loop)