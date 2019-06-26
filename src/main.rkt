#lang racket

(require (lib "eopl.ss" "eopl"))

(define program-url "sample.txt")


(define remove-all
  (lambda (ele lst)
    (cond
      ((null? lst) '())           ; if the list is empty then we're done
      ((not (pair? (car lst)))    ; if the first element is an atom
       (if (equal? (car lst) ele) ; check if it's the one we're looking for
           (remove-all ele (cdr lst)) ; if so we skip over it, eliminating it
           (cons (car lst) (remove-all ele (cdr lst))))) ; otherwise we add it
      (else (cons (remove-all ele (car lst))      ; else advance recursion
                  (remove-all ele (cdr lst))))))) ; over both `car` and `cdr`


(define program-lines
  (remove-all ""
              (map string-normalize-spaces
                   (file->lines program-url #:mode'text))))

(define extract-setup-commands
  (lambda (lines)
    (let ([store '(() ()) ])
      (extract-setup-commands-helping lines 'doesntmatter store))))

; ((setup) (commands))


(define extract-setup-commands-helping
  (lambda (lines dir result)
    (if (equal? lines '())
        result
        (let ([head (car lines)]
              [tail (cdr lines)]
              [setup (car result)]
              [commands (cadr result)])
          (cond
            [(equal? head "setup") (extract-setup-commands-helping tail 's result)]
            [(equal? head "commands") (extract-setup-commands-helping tail 'c result)]
            [(equal? dir 's) (extract-setup-commands-helping tail 's
                                                             (list (append setup (list head)) commands))]
            [(equal? dir 'c) (extract-setup-commands-helping tail 'c
                                                             (list setup (append commands (list head))))]
            [else (error "something is wrong")])))))

(define commands '())
(define setup '())

(define dummy
  (match (extract-setup-commands program-lines)
    [(list lsetup lcommands)
     (begin
     (set! commands lcommands)
     (set! setup lsetup))
     ]))

setup
commands

(define-datatype account-type account-type?
  (a-account-type (account-id number?)
                  (current-account boolean?)
                  (bank-fee number?)
                  (minimum-deposit number?)
                  (monthly boolean?)
                  (period number?)
                  (renewable boolean?)
                  (interest-rate number?)
                  (credit number?)
                  (variability boolean?)
                  (span-for-increase number?)
                  (increase-rate number?)
                  (has-cheque boolean?)
                  (has-card boolean?)
                  (transfer-fee number?)))

(define-datatype loan-type loan-type?
  (no-loan)
  (a-loan-type (loan-id number?)
               (loan-amount number?)
               (blocking-money number?)
               (return-span number?)
               (interest number?)
               (last-loan number?)
               (minimum-credit number?)
               ))

(define-datatype customer customer?
  (a-customer (account account-type?)
              (loan loan-type?)
              (customer-id number?)
              (initial-balance number?)
              (balance number?)
              (credit number?)
              (interest-rate number?)
              (year-counter number?)
              (interest-rate-increase-counter number?)
              (end-of-contract-month number?)
              (last-loan-month number?)
              (end-of-loan-month number?)
              (debt number?)
              )
  )


(define getbool
  (lambda (str index)
    (if (eqv? (list-ref (string-split str) index) "true")
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


(define account-types '())
(define loan-types '())

(define initialize-setup
  (lambda (setup-list)
    (cond
      ((null? setup-list) "finished") 
      ( (string=? (car (string-split (car setup-list))) "Account") (let ((account-id (getint (list-ref setup-list 0) 2))
          (current-account (getbool (list-ref setup-list 1) 1))
          (bank-fee (getreal (list-ref setup-list 2) 1))
          (minimum-deposit (getreal (list-ref setup-list 3) 1))
          (monthly (getbool (list-ref setup-list 4) 1))
          (period (getint (list-ref setup-list 5) 1))
          (renewable (getbool (list-ref setup-list 6) 1))
          (interest-rate (getint (list-ref setup-list 7) 1))
          (credit (getreal (list-ref setup-list 8) 1))
          (variability (getbool (list-ref setup-list 9) 1))
          (span-for-increase (getint (list-ref setup-list 10) 1))
          (increase-rate (getreal (list-ref setup-list 11) 1))
          (has-cheque (getbool (list-ref setup-list 12) 1))
          (has-card (getbool (list-ref setup-list 13) 1))
          (transfer-fee (getreal (list-ref setup-list 14) 1))
          )
      (set! account-types (cons (a-account-type account-id
                        current-account
                        bank-fee
                        minimum-deposit
                        monthly
                        period
                        renewable
                        interest-rate
                        credit
                        variability
                        span-for-increase
                        increase-rate
                        has-cheque
                        has-card
                        transfer-fee) account-types))
      (initialize-setup (slice setup-list 15))
       ))
      (else
       (let ((loan-id (getint (list-ref setup-list 0) 2))
             (loan-amount (getreal (list-ref setup-list 1) 1))
             (blocking-money (getreal (list-ref setup-list 2) 1))
             (return-span (getint (list-ref setup-list 3) 1))
             (interest (getreal (list-ref setup-list 4) 1))
             (last-loan (getint (list-ref setup-list 5) 1))
             (minimum-credit (getreal (list-ref setup-list 6) 1))
             )
         (display (car setup-list))
       (set! loan-types (cons (a-loan-type loan-id
                          loan-amount
                          blocking-money
                          return-span
                          interest
                          last-loan
                          minimum-credit) loan-types))
       (initialize-setup (slice setup-list 7))
       ))
      )
    )
  )



(initialize-setup setup)
account-types
loan-types