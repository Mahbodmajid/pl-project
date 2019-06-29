#lang racket

(require (lib "eopl.ss" "eopl"))
(require "data-types.rkt")

(provide (all-defined-out))

(define loan-type->loan-id
  (lambda (loan)
    (cases loan-type loan
      [a-loan-type (loan-id loan-amount blocking-money return-span interest last-loan minimum-credit)
                   loan-id]
      [else (raise-argument-error 'loan-type->loan-id "loan?" loan)]
      )
    )
  )

(define loan-type->loan-amount
  (lambda (loan)
    (cases loan-type loan
      [a-loan-type (loan-id loan-amount blocking-money return-span interest last-loan minimum-credit)
                   loan-amount]
      [else (raise-argument-error 'loan-type->loan-amount "loan?" loan)]
      )
    )
  )

(define loan-type->blocking-money
  (lambda (loan)
    (cases loan-type loan
      [a-loan-type (loan-id loan-amount blocking-money return-span interest last-loan minimum-credit)
                   blocking-money]
      [else (raise-argument-error 'loan-type->blocking-money "loan?" loan)]
      )
    )
  )

(define loan-type->return-span
  (lambda (loan)
    (cases loan-type loan
      [a-loan-type (loan-id loan-amount blocking-money return-span interest last-loan minimum-credit)
                   return-span]
      [else (raise-argument-error 'loan-type->return-span "loan?" loan)]
      )
    )
  )

(define loan-type->interest
  (lambda (loan)
    (cases loan-type loan
      [a-loan-type (loan-id loan-amount blocking-money return-span interest last-loan minimum-credit)
                   interest]
      [else (raise-argument-error 'loan-type->interest "loan?" loan)]
      )
    )
  )

(define loan-type->last-loan
  (lambda (loan)
    (cases loan-type loan
      [a-loan-type (loan-id loan-amount blocking-money return-span interest last-loan minimum-credit)
                   last-loan]
      [else (raise-argument-error 'loan-type->last-loan "loan?" loan)]
      )
    )
  )

(define loan-type->minimum-credit
  (lambda (loan)
    (cases loan-type loan
      [a-loan-type (loan-id loan-amount blocking-money return-span interest last-loan minimum-credit)
                   minimum-credit]
      [else (raise-argument-error 'loan-type->minimum-credit "loan?" loan)]
      )
    )
  )