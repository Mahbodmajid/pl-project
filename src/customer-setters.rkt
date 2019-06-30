#lang racket

(require (lib "eopl.ss" "eopl"))
(require "data-types.rkt")

(define customer-contract-start-month-setter
  (lambda (my-customer new-contract-start-month)
    (cases customer my-customer
      [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            (a-customer customer-id start-month new-contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            ]
      [else (raise-argument-error 'customer-contract-start-month-setter "customer?" my-customer)]
      )
    )
  )

(define customer-history-setter
  (lambda (my-customer new-entry)
    (cases customer my-customer
      [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            (a-customer customer-id start-month contract-start-month account-type-id 
            (cons new-entry history) initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            ]
      [else (raise-argument-error 'customer-history-setter "customer?" my-customer)]
      )
    )
  )

(define customer-balance-setter
  (lambda (my-customer new-balance)
    (cases customer my-customer
      [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            (a-customer customer-id start-month contract-start-month account-type-id 
            history initial-balance new-balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            ]
      [else (raise-argument-error 'customer-balance-setter "customer?" my-customer)]
      )
    )
  )

(define customer-last-decrease-month-setter
  (lambda (my-customer new-last-decrease-month)
    (cases customer my-customer
      [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            (a-customer customer-id start-month contract-start-month account-type-id 
            history initial-balance balance new-last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            ]
      [else (raise-argument-error 'customer-last-decrease-month-setter "customer?" my-customer)]
      )
    )
  )

(define customer-credit-setter
  (lambda (my-customer credit-change)
    (cases customer my-customer
      [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            (a-customer customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month (+ credit credit-change) credit-reset-month
            new-interest-rate interest-rate debts)
            ]
      [else (raise-argument-error 'customer-credit-setter "customer?" my-customer)]
      )
    )
  )

(define customer-credit-reset-month-setter
  (lambda (my-customer new-credit-reset-month)
    (cases customer my-customer
      [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            (a-customer customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit new-credit-reset-month
            new-interest-rate interest-rate debts)
            ]
      [else (raise-argument-error 'customer-credit-reset-month-setter "customer?" my-customer)]
      )
    )
  )

(define customer-new-interest-rate-setter
  (lambda (my-customer increase-rate)
    (cases customer my-customer
      [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            (a-customer customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            (+ new-interest-rate increase-rate) interest-rate debts)
            ]
      [else (raise-argument-error 'customer-new-interest-rate-setter "customer?" my-customer)]
      )
    )
  )

(define customer-interest-rate-setter
  (lambda (my-customer changed-interest-rate)
    (cases customer my-customer
      [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            (a-customer customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate changed-interest-rate debts)
            ]
      [else (raise-argument-error 'customer-interest-rate-setter "customer?" my-customer)]
      )
    )
  )


(define customer-debts-setter
  (lambda (my-customer new-debt)
    (cases customer my-customer
      [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            (a-customer customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate (cons new-debt debts))
            ]
      [else (raise-argument-error 'customer-debts-setter "customer?" my-customer)]
      )
    )
  )

