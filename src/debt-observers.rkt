#lang racket

(require (lib "eopl.ss" "eopl"))
(require "data-types.rkt")

(define debt->loan-type-id
  (lambda (dbt)
    (cases debt dbt
      [a-debt (loan-type-id start-month paid done withdrawn) loan-type-id]
      [else (raise-argument-error 'debt->loan-type-id "debt?" dbt)]
      )
    )
  )

(define debt->start-month
  (lambda (dbt)
    (cases debt dbt
      [a-debt (loan-type-id start-month paid done withdrawn) start-month]
      [else (raise-argument-error 'debt->start-month "debt?" dbt)]
      )
    )
  )

(define debt->paid
  (lambda (dbt)
    (cases debt dbt
      [a-debt (loan-type-id start-month paid done withdrawn) paid]
      [else (raise-argument-error 'debt->paid "debt?" dbt)]
      )
    )
  )

(define debt->done
  (lambda (dbt)
    (cases debt dbt
      [a-debt (loan-type-id start-month paid done withdrawn) done]
      [else (raise-argument-error 'debt->done "debt?" dbt)]
      )
    )
  )

(define debt->withdrawn
  (lambda (dbt)
    (cases debt dbt
      [a-debt (loan-type-id start-month paid done withdrawn) withdrawn]
      [else (raise-argument-error 'debt->withdrawn "debt?" dbt)]
      )
    )
  )