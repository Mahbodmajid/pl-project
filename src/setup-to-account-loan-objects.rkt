#lang racket

(require "data-types.rkt")
(require "utils.rkt")

(provide set-account-loan-types!)

(define initialize-setup
  (lambda (setup-list store)
    (let ([account-types (car store)]
          [loan-types (cadr store)])
    (cond
      ((null? setup-list) store) 
      ((string=? (car (string-split (car setup-list))) "Account")
        (let ([account-id (getint (list-ref setup-list 0) 2)]
              [current-account (getbool (list-ref setup-list 1) 1)]
              [bank-fee (getreal (list-ref setup-list 2) 1)]
              [minimum-deposit (getreal (list-ref setup-list 3) 1)]
              [monthly (getbool (list-ref setup-list 4) 1)]
              [period (getint (list-ref setup-list 5) 1)]
              [renewable (getbool (list-ref setup-list 6) 1)]
              [interest-rate (getint (list-ref setup-list 7) 1)]
              [credit (getreal (list-ref setup-list 8) 1)]
              [variability (getbool (list-ref setup-list 9) 1)]
              [span-for-increase (getint (list-ref setup-list 10) 1)]
              [increase-rate (getreal (list-ref setup-list 11) 1)]
              [has-cheque (getbool (list-ref setup-list 12) 1)]
              [has-card (getbool (list-ref setup-list 13) 1)]
              [transfer-fee (getreal (list-ref setup-list 14) 1)])
          (let ([new-account-types (cons (a-account-type account-id
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
                                                    transfer-fee) account-types)])
          (initialize-setup (slice setup-list 15) (list new-account-types loan-types)))))
      (else
       (let ([loan-id (getint (list-ref setup-list 0) 2)]
             [loan-amount (getreal (list-ref setup-list 1) 1)]
             [blocking-money (getreal (list-ref setup-list 2) 1)]
             [return-span (getint (list-ref setup-list 3) 1)]
             [interest (getreal (list-ref setup-list 4) 1)]
             [last-loan (getint (list-ref setup-list 5) 1)]
             [minimum-credit (getreal (list-ref setup-list 6) 1)])
         (let ([new-loan-types (cons (a-loan-type loan-id
                                             loan-amount
                                             blocking-money
                                             return-span
                                             interest
                                             last-loan
                                             minimum-credit) loan-types)])
         (initialize-setup (slice setup-list 7) (list account-types new-loan-types)))))))))

(define-syntax-rule (set-account-loan-types! a l setup)
    (match (initialize-setup setup '(() ()))
    [(list account-types loan-types)
     (begin
     (set! a account-types)
     (set! l loan-types))
     ]))