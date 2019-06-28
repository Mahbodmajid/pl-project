#lang racket

(provide (all-defined-out))

(define customer->customer-id 
    (lambda (my-customer) 
        (cases customer mycustomer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history balance last-withdraw-month credit new-interest-rate 
            interest-rate loan-amount-to-add debts)
            customer-id]
            [else (raise-argument-error 'customer->customer-id "customer?" my-customer)]
        )
    )
)

(define customer->start-month
    (lambda (my-customer) 
        (cases customer mycustomer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history balance last-withdraw-month credit new-interest-rate 
            interest-rate loan-amount-to-add debts)
            start-month]
            [else (raise-argument-error 'customer->start-month "customer?" my-customer)]
        )
    )
)

(define customer->contract-start-month
    (lambda (my-customer) 
        (cases customer mycustomer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history balance last-withdraw-month credit new-interest-rate 
            interest-rate loan-amount-to-add debts)
            contract-start-month]
            [else (raise-argument-error 'customer->contract-start-month "customer?" my-customer)]
        )
    )
)

(define customer->account-type-id
    (lambda (my-customer) 
        (cases customer mycustomer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history balance last-withdraw-month credit new-interest-rate 
            interest-rate loan-amount-to-add debts)
            account-type-id]
            [else (raise-argument-error 'customer->account-type-id "customer?" my-customer)]
        )
    )
)
(define customer->history
    (lambda (my-customer) 
        (cases customer mycustomer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history balance last-withdraw-month credit new-interest-rate 
            interest-rate loan-amount-to-add debts)
            history]
            [else (raise-argument-error 'customer->history "customer?" my-customer)]
        )
    )
)
(define customer->balance
    (lambda (my-customer) 
        (cases customer mycustomer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history balance last-withdraw-month credit new-interest-rate 
            interest-rate loan-amount-to-add debts)
            balance]
            [else (raise-argument-error 'customer->balance "customer?" my-customer)]
        )
    )
)
(define customer->last-withdraw-month
    (lambda (my-customer) 
        (cases customer mycustomer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history balance last-withdraw-month credit new-interest-rate 
            interest-rate loan-amount-to-add debts)
            last-withdraw-month]
            [else (raise-argument-error 'customer->last-withdraw-month "customer?" my-customer)]
        )
    )
)
(define customer->credit
    (lambda (my-customer) 
        (cases customer mycustomer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history balance last-withdraw-month credit new-interest-rate 
            interest-rate loan-amount-to-add debts)
            credit]
            [else (raise-argument-error 'customer->credit "customer?" my-customer)]
        )
    )
)


(define customer->new-interest-rate
    (lambda (my-customer) 
        (cases customer mycustomer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history balance last-withdraw-month credit new-interest-rate 
            interest-rate loan-amount-to-add debts)
            new-interest-rate]
            [else (raise-argument-error 'customer->new-interest-rate "customer?" my-customer)]
        )
    )
)

(define customer->interest-rate
    (lambda (my-customer) 
        (cases customer mycustomer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history balance last-withdraw-month credit new-interest-rate 
            interest-rate loan-amount-to-add debts)
            interest-rate]
            [else (raise-argument-error 'customer->interest-rate "customer?" my-customer)]
        )
    )
)

(define customer->loan-amount-to-add
    (lambda (my-customer) 
        (cases customer mycustomer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history balance last-withdraw-month credit new-interest-rate 
            interest-rate loan-amount-to-add debts)
            loan-amount-to-add]
            [else (raise-argument-error 'customer->loan-amount-to-add "customer?" my-customer)]
        )
    )
)

(define customer->debts
    (lambda (my-customer) 
        (cases customer mycustomer
            [a-customer
            (customer-id start-month contract-start-month account-type-id 
            history balance last-withdraw-month credit new-interest-rate 
            interest-rate loan-amount-to-add debts)
            debts]
            [else (raise-argument-error 'customer->debts "customer?" my-customer)]
        )
    )
)