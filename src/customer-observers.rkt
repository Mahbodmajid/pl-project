#lang racket

(require (lib "eopl.ss" "eopl"))
(require "data-types.rkt")

(provide (all-defined-out))

(define customer->customer-id 
    (lambda (my-customer) 
        (cases customer my-customer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            customer-id]
            [else (raise-argument-error 'customer->customer-id "customer?" my-customer)]
        )
    )
)

(define customer->start-month
    (lambda (my-customer) 
        (cases customer my-customer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            start-month]
            [else (raise-argument-error 'customer->start-month "customer?" my-customer)]
        )
    )
)

(define customer->contract-start-month
    (lambda (my-customer) 
        (cases customer my-customer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            contract-start-month]
            [else (raise-argument-error 'customer->contract-start-month "customer?" my-customer)]
        )
    )
)

(define customer->account-type-id
    (lambda (my-customer) 
        (cases customer my-customer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            account-type-id]
            [else (raise-argument-error 'customer->account-type-id "customer?" my-customer)]
        )
    )
)
(define customer->history
    (lambda (my-customer) 
        (cases customer my-customer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            history]
            [else (raise-argument-error 'customer->history "customer?" my-customer)]
        )
    )
)
(define customer->balance
    (lambda (my-customer) 
        (cases customer my-customer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            balance]
            [else (raise-argument-error 'customer->balance "customer?" my-customer)]
        )
    )
)
(define customer->last-decrease-month
    (lambda (my-customer) 
        (cases customer my-customer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            last-decrease-month]
            [else (raise-argument-error 'customer->last-decrease-month "customer?" my-customer)]
        )
    )
)


(define customer->credit
    (lambda (my-customer) 
        (cases customer my-customer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            credit]
            [else (raise-argument-error 'customer->credit "customer?" my-customer)]
        )
    )
)

(define customer->credit-reset-month
    (lambda (my-customer) 
        (cases customer my-customer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            credit-reset-month]
            [else (raise-argument-error 'customer->credit-reset-month "customer?" my-customer)]
        )
    )
)


(define customer->new-interest-rate
    (lambda (my-customer) 
        (cases customer my-customer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            new-interest-rate]
            [else (raise-argument-error 'customer->new-interest-rate "customer?" my-customer)]
        )
    )
)

(define customer->interest-rate
    (lambda (my-customer) 
        (cases customer my-customer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            interest-rate]
            [else (raise-argument-error 'customer->interest-rate "customer?" my-customer)]
        )
    )
)

(define customer->initial-balance
    (lambda (my-customer) 
        (cases customer my-customer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            initial-balance]
            [else (raise-argument-error 'customer->initial-balance "customer?" my-customer)]
        )
    )
)

(define customer->debts
    (lambda (my-customer) 
        (cases customer my-customer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history initial-balance balance last-decrease-month credit credit-reset-month
            new-interest-rate interest-rate debts)
            debts]
            [else (raise-argument-error 'customer->debts "customer?" my-customer)]
        )
    )
)

(define customer->credit-increase-reached
    (lambda (my-customer month)
        (let* ([credit-reset-month (customer->credit-reset-month my-customer)]
                [month-distance (- month credit-reset-month)]
                [year-distance (floor (/ month-distance 12))])
            (if (eq? year-distance 0)
                #f 
                (if eq? (eq? (* year-distance 12) month-distance)
                #t
                #f)
            )
        )
    )
)