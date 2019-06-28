#lang racket

(define consif 
  (lambda (e l)
    (if e (cons e l) l)))

(define find-customer-index
  (lambda (customer-id all-customers)
    (let ([customer-ids (map customer->customer-id all-customers)])
      (index-of customer-ids customer-id)
      )
    )
  )

(define pass-time-all
  (lambda (all-customers account-types loan-types month)
    (map 
     (lambda (customer)
       customer
       )    
     all-customers)
    )
  )

(define new-account ;returns #f if not valid o.w. new-account's object
  (lambda (customer-id account-type-id initial-money account-types loan-types month)
    #f
    )
  )

(define add-money
  (lambda (customer amount account-types loan-types month)
    customer
    )
  )

(define renewal
  (lambda (customer account-types loan-types month)
    customer
    )
  )

(define write-cheque
  (lambda (customer amount account-types loan-types month)
    customer
    )
  )

(define spend
  (lambda (customer amount account-types loan-types month)
    customer
    )
  )


; equiv to spend
(define transfer
  (lambda (customer amount account-types loan-types month)
    (spend customer amount month)
    )
  )

; equiv to spend
(define withdraw
  (lambda (customer amount account-types loan-types month)
    (spend customer amount month)
    )
  )

(define new-loan
  (lambda (customer loan-type-id account-types loan-types month)
    customer
    )
  )

(define pay-debt
  (lambda  (customer amount account-types loan-types month)
    customer
    )
  )

(define withdraw-loan
  (lambda (customer account-types loan-types month)
    customer
    )
  )

(define apply-command 
  (lambda (command all-customers-month account-types loan-types)
    (let ([all-customers (car all-customers-month)]
          [month (cadr all-customers-month)])
      (match command
        [(list 'time) (cons (pass-time-all all-customers month) (list (+ month 1)))]
        [else 
         (cons 
          ; this section returns the new all-customers
          (let ([customer-id (cadr command) ])
            (let ([customer-index (find-customer-index customer-id all-customers)])
              (if (customer-index)
                  (let ([current-customer (list-ref all-customers customer-index)])
                    (match command
                      [(list 'close _) (remove all-customers current-customer)]
                      [else
                       (list-set all-customers customer-index ;replace the customer
                                 (match command 
                                   [(list 'add-money _ amount) 
                                    (add-money current-customer amount account-types loan-types month)]
                                   [(list 'renewal _)
                                    (renewal current-customer account-types loan-types month)]
                                   [(list 'write-cheque _ amount)
                                    (write-cheque current-customer amount account-types loan-types month)]
                                   [(list 'spend _ amount)
                                    (spend current-customer amount account-types loan-types month)]
                                   [(list 'transfer _ amount)
                                    (transfer current-customer amount account-types loan-types month)]
                                   [(list 'withdraw _ amount)
                                    (withdraw current-customer amount account-types loan-types month)]
                                   [(list 'new-loan _ loan-type-id)
                                    (new-loan current-customer loan-type-id account-types loan-types month)]
                                   [(list 'pay-debt _ amount)
                                    (pay-debt current-customer amount account-types loan-types month)]
                                   [(list 'withdraw-loan _)
                                    (withdraw-loan current-customer account-types loan-types month)]
                                   [else current-customer]) ;ignore
                                 )]
                      )
                    )
                  (match command
                    [(list 'new-account _ account-type-id initial-money)
                     (consif (new-account customer-id account-type-id initial-money account-types loan-types month) all-customers)] ; add the new-account
                    [else all-customers] ;ignore
                    )
                  )
              )
            )
          (list month)
          )]))))