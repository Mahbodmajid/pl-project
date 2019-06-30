#lang racket

(require (lib "eopl.ss" "eopl"))
(require "data-types.rkt")

(define debt->debt-id
  (lambda (dbt)
    (cases debt dbt
      [a-debt (debt-id loan-type-id start-month paid done withdrawn) debt-id]
      [else (raise-argument-error 'debt->debt-id "debt?" dbt)]
      )
    )
  )

(define debt->loan-type-id
  (lambda (dbt)
    (cases debt dbt
      [a-debt (debt-id loan-type-id start-month paid done withdrawn) loan-type-id]
      [else (raise-argument-error 'debt->loan-type-id "debt?" dbt)]
      )
    )
  )



(define debt->start-month
  (lambda (dbt)
    (cases debt dbt
      [a-debt (debt-id loan-type-id start-month paid done withdrawn) start-month]
      [else (raise-argument-error 'debt->start-month "debt?" dbt)]
      )
    )
  )

(define debt->paid
  (lambda (dbt)
    (cases debt dbt
      [a-debt (debt-id loan-type-id start-month paid done withdrawn) paid]
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
      [a-debt (debt-id loan-type-id start-month paid done withdrawn) withdrawn]
      [else (raise-argument-error 'debt->withdrawn "debt?" dbt)]
      )
    )
  )


(define debt->total-debt
  (lambda (dbt loan-type current-month)
    (let* ([original-debt (loan->loan-amount loan-type)]
           [start-month (debt->start-month dbt)]
           [month-diff (- current-month start-month)]
           [interest-rate (/ (loan->interest loan-type) 100)]
           [year-diff (floor (/ month-diff 12))])
      (* (+ 1 (* year-diff interest-rate)) original-debt)
    )
  )
)

(define debt->remaining
  (lambda (dbt loan-type current-month)
    (let ([tot-remaining (debt->total-debt dbt loan-type current-month)]
          [paid (debt->paid dbt)])
      (- tot-remaining paid))
  )
)
