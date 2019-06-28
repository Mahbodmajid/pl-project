#lang racket

(require (lib "eopl.ss" "eopl"))
(require "data-types.rkt")

(define loan-type->loan-id
  (lambda (loan)
    (cases loan-type loan
      (a-loan-type (loan-id loan-amount blocking-money return-span interest last-loan minimum-credit)
                   loan-id)
      )
    )
  )

(define loan-type->loan-amount
  (lambda (loan)
    (cases loan-type loan
      (a-loan-type (loan-id loan-amount blocking-money return-span interest last-loan minimum-credit)
                   loan-amount)
      (else (display "Something is not right!"))
      )
    )
  )

(define loan-type->blocking-money
  (lambda (loan)
    (cases loan-type loan
      (a-loan-type (loan-id loan-amount blocking-money return-span interest last-loan minimum-credit)
                   blocking-money)
      (else (display "Something is not right!"))
      )
    )
  )

(define loan-type->return-span
  (lambda (loan)
    (cases loan-type loan
      (a-loan-type (loan-id loan-amount blocking-money return-span interest last-loan minimum-credit)
                   return-span)
      (else (display "Something is not right!"))
      )
    )
  )

(define loan-type->interest
  (lambda (loan)
    (cases loan-type loan
      (a-loan-type (loan-id loan-amount blocking-money return-span interest last-loan minimum-credit)
                   interest)
      (else (display "Something is not right!"))
      )
    )
  )

(define loan-type->last-loan
  (lambda (loan)
    (cases loan-type loan
      (a-loan-type (loan-id loan-amount blocking-money return-span interest last-loan minimum-credit)
                   last-loan)
      (else (display "Something is not right!"))
      )
    )
  )

(define loan-type->minimum-credit
  (lambda (loan)
    (cases loan-type loan
      (a-loan-type (loan-id loan-amount blocking-money return-span interest last-loan minimum-credit)
                   minimum-credit)
      (else (display "Something is not right!"))
      )
    )
  )