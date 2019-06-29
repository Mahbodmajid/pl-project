#lang racket

(require (lib "eopl.ss" "eopl"))
(require "data-types.rkt")

(define debt-paid-setter
  (lambda (my-debt new-paid)
    (cases debt my-debt
      [a-debt (loan-type-id start-month paid done withdrawn)
              (a-debt loan-type-id start-month new-paid done withdrawn)
              ]
      [else (raise-argument-error 'debt-paid-setter "debt?" my-debt)]
      )
    )
  )

(define debt-done-setter
  (lambda (my-debt)
    (cases debt my-debt
      [a-debt (loan-type-id start-month paid done withdrawn)
              (a-debt loan-type-id start-month paid #t withdrawn)
              ]
      [else (raise-argument-error 'debt-done-setter "debt?" my-debt)]
      )
    )
  )

(define debt-withdrawn-setter
  (lambda (my-debt)
    (cases debt my-debt
      [a-debt (loan-type-id start-month paid done withdrawn)
              (a-debt loan-type-id start-month paid done #t)
              ]
      [else (raise-argument-error 'debt-withdrawn-setter "debt?" my-debt)]
      )
    )
  )