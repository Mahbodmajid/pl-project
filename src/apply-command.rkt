#lang racket

(define consif 
    (lambda (e l)
        (if e (cons e l) l)))

(define find-customer-index (customer-id all-customers)
    (let ([customer-ids (map customer->customer-id all-customers)])
        (index-of customer-ids customer-id)))

(define pass-time-signle 
    (lambda (customer account-types loan-types)
        customer
    )    
)

(define pass-time-all
    (lambda (all-customers account-types loan-types)
        (map pass-time-single all-customers)
    )
)

(define new-account ;returns #f if not valid o.w. new-account's object
    (lambda (customer-id account-type-id initial-money account-types loan-types)
        #f
    )
)

(define add-money
    (lambda (customer amount account-types loan-types)
        customer
    )
)

(define renewal
    (lambda (customer account-types loan-types)
        customer
    )
)

(define write-cheque
    (lambda (customer amount account-types loan-types)
        customer
    )
)

(define spend
    (lambda (customer amount account-types loan-types)
        customer
    )
)


; equiv to spend
(define transfer
    (lambda (customer amount account-types loan-types)
        (spend customer amount)
    )
)

; equiv to spend
(define withdraw
    (lambda (customer amount account-types loan-types)
        (spend customer amount)
    )
)

(define new-loan
    (lambda (customer loan-type-id account-types loan-types)
        customer
    )
)

(define pay-debt
    (lambda  (customer amount account-types loan-types)
        customer
    )
)

(define withdraw-loan
    (lambda (customer account-types loan-types)
        customer
    )
)

(define apply-command 
    (lambda (command all-customers account-types loan-types)
    (match command
    [(list 'time) (pass-time-all all-customers)]
    [else 
    (let ([customer-id (cadr command) ])
        (let ([customer-index (find-customer-index customer-id all-customers])
            (if (customer-index)
                (let ([current-customer (list-ref all-customers customer-index)])
                    (match command
                        [(list 'close _) (remove all-customers current-customer)]
                        [else
                        (list-set all-customers customer-index ;replace the customer
                            (match command 
                            [(list 'add-money _ amount) 
                            (add-money current-customer amount account-types loan-types)]
                            [(list 'renewal _)
                            (renewal current-customer account-types loan-types)]
                            [(list 'write-cheque _ amount)
                            (write-cheque current-customer amount account-types loan-types)]
                            [(list 'spend _ amount)
                            (spend current-customer amount account-types loan-types)]
                            [(list 'transfer _ amount)
                            (transfer current-customer amount account-types loan-types)]
                            [(list 'withdraw _ amount)
                            (withdraw current-customer amount account-types loan-types)]
                            [(list 'new-loan _ loan-type-id)
                            (new-loan current-customer loan-type-id account-types loan-types)]
                            [(list 'pay-debt _ amount)
                            (pay-debt current-customer amount account-types loan-types)]
                            [(list 'withdraw-loan _)
                            (withdraw-loan current-customer account-types loan-types)]
                            [else current-customer]) ;ignore
                        )]
                    )
                )
                (match command
                    [(list 'new-account _ account-type-id initial-money)
                    (consif (new-account customer-id account-type-id initial-money account-types loan-types) all-customers)] ; add the new-account
                    [else all-customers] ;ignore
                ))
        )]))
    )
)