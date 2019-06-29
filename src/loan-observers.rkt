#lang racket

(require (lib "eopl.ss" "eopl"))
(require "data-types.rkt")

(provide (all-defined-out))

(define loan->loan-id
  (lambda (loan)
    (cases loan-type loan
      [a-loan-type (loan-id loan-amount blocking-money return-span interest last-loan minimum-credit)
                   loan-id]
      [else (raise-argument-error 'loan->loan-id "loan?" loan)]
      )
    )
  )

(define loan->loan-amount
  (lambda (loan)
    (cases loan-type loan
      [a-loan-type (loan-id loan-amount blocking-money return-span interest last-loan minimum-credit)
                   loan-amount]
      [else (raise-argument-error 'loan->loan-amount "loan?" loan)]
      )
    )
  )

(define loan->blocking-money
  (lambda (loan)
    (cases loan-type loan
      [a-loan-type (loan-id loan-amount blocking-money return-span interest last-loan minimum-credit)
                   blocking-money]
      [else (raise-argument-error 'loan->blocking-money "loan?" loan)]
      )
    )
  )

(define loan->return-span
  (lambda (loan)
    (cases loan-type loan
      [a-loan-type (loan-id loan-amount blocking-money return-span interest last-loan minimum-credit)
                   return-span]
      [else (raise-argument-error 'loan->return-span "loan?" loan)]
      )
    )
  )

(define loan->interest
  (lambda (loan)
    (cases loan-type loan
      [a-loan-type (loan-id loan-amount blocking-money return-span interest last-loan minimum-credit)
                   interest]
      [else (raise-argument-error 'loan->interest "loan?" loan)]
      )
    )
  )

(define loan->last-loan
  (lambda (loan)
    (cases loan-type loan
      [a-loan-type (loan-id loan-amount blocking-money return-span interest last-loan minimum-credit)
                   last-loan]
      [else (raise-argument-error 'loan->last-loan "loan?" loan)]
      )
    )
  )

(define loan->minimum-credit
  (lambda (loan)
    (cases loan-type loan
      [a-loan-type (loan-id loan-amount blocking-money return-span interest last-loan minimum-credit)
                   minimum-credit]
      [else (raise-argument-error 'loan->minimum-credit "loan?" loan)]
      )
    )
  )