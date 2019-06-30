#lang racket

(provide (all-defined-out))

(define getbool
  (lambda (str index)
    (if (eq? (list-ref (string-split str) index) "true")
        #t
        #f) 
    )
  )

(define getint
  (lambda (str index)
    (string->number (list-ref (string-split str) index))
    )
  )

(define getreal
  (lambda (str index)
    (string->number (list-ref (string-split str) index))
    )
  )


;this is for slicing a list!
(define get-n-items
    (lambda (lst num)
        (if (> num 0)
            (cons (car lst) (get-n-items (cdr lst) (- num 1)))
            '()))) 

(define slice2
    (lambda (lst start count)
        (if (> start 1)
            (slice (cdr lst) (- start 1) count)
            (get-n-items lst count))))

(define slice
  (lambda (lst n)
    (cond
      ((zero? n) lst)
      (else (slice (cdr lst) (- n 1)))
    ))
  )
;end of slicing