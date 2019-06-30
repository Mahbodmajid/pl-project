#lang racket

(require "data-types.rkt")
(require "customer-observers.rkt")
(require "account-observers.rkt")
(require "loan-observers.rkt")
(require "customer-setters.rkt")
(require "debts-setters.rkt")

(provide apply-command)

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

(define find-account-type
  (lambda (account-type-id account-types) 
    (let ([account-ids (map account->account-id account-types)])
      (let ([account-index (index-of account-ids account-type-id)])
        (if account-index
            (list-ref account-types account-index)
            #f
            )
        )
      )
    )
  )

(define find-loan-type
  (lambda (loan-type-id loan-types) 
    (let ([loan-ids (map loan->loan-id loan-types)])
      (let ([loan-index (index-of loan-ids loan-type-id)])
        (if loan-index
            (list-ref loan-types loan-index)
            #f
            )
        )
      )
    )
  )



(define new-account 
  ; customer constructor
  ; returns #f if not valid o.w. new-account's object
  (lambda (customer-id account-type-id initial-money account-types loan-types month)
    (let ([account (find-account-type account-type-id account-types)])
      (if account
          (let ([can-open (can-open-account account initial-money)]
                [money-after (money-after-opening account initial-money)]
                [interest-rate (account->interest-rate account)])
            (if can-open
                (a-customer customer-id
                            month
                            month
                            account-type-id
                            '()
                            money-after
                            month
                            0
                            interest-rate
                            interest-rate
                            0
                            '()
                            )
                #f
                )
            )
          #f
          )
      )
    )
  )

(define add-money
  (lambda (my-customer amount account-types loan-types month)
    (customer-balance-setter my-customer (+ (customer->balance my-customer) amount))
    )
  )

(define is-valid-contract?
  (lambda (my-customer account month)
    (> (- month (customer->contract-start-month my-customer)) (account->period account))
    )
  )
    
(define renewal
  (lambda (customer account-types loan-types month)
    (let ((account-id (customer->account-type-id customer))
          (account (find-account-type account-id account-types)))
      (if account
          (if (and account->renewable (is-valid-contract? customer account month))
              (customer-contract-start-month customer month)
              customer)
          (raise-argument-error 'renewal "customer?" my-customer)
          )
      )
    )
  )

(define blocked-money-amount
  (lambda (my-debt loan-types)
    (if (my-debt->done)
        0
        (let ((my-loan (find-loan-type (debt->loan-type-id my-debt) loan-types)))
          (loan-type->blocked-money my-loan))
        )
    )
  )

(define sum
  (lambda (lst)
    (if (null? lst)
        0
        (+ (car lst) (sum (cdr lst)))
        )
    )
  )

(define sum-of-blocked-money
  (lambda (my-customer loan-type)
    (let ((my-debts (customer->debts my-customer))
          (blocked-money-list (map blocked-money-amount my-debts)))
      (sum blocked-money-list)
      )
    )
  )

(define free-money
  (lambda (my-customer account)
    (- (customer->balance my-customer) (+ (account->minimum-deposit account) (sum-of-blocked-money my-customer loan-types)))
    )
  )

(define write-cheque
  (lambda (my-customer amount account-types loan-types month)
    (let ((account-id (customer->account-type-id customer))
          (account (find-account-type account-id account-types)))
      (if (account->has-cheque)
          (let ((free-amount (free-money my-customer account)))
            (if (>= free-amount amount)
                (customer-last-decrease-month-setter (customer-balance-setter my-customer (- (customer->balance my-customer) amount)) month)
                my-customer
                )
            )
          (customer-credit-setter my-customer (- (/ (account->credit account) 2)))
          )
      )
    )
  )

(define spend
  (lambda (customer amount account-types loan-types month)
    (let ((account-id (customer->account-type-id customer))
          (account (find-account-type account-id account-types))
          (valid (is-valid-contract? my-customer account month)))
      (if (account->has-card)
          (if valid
              (let ((free-amount (free-money my-customer account)))
                (if (>= free-amount amount)
                    (customer-last-decrease-month-setter (customer-balance-setter my-customer (- (customer->balance my-customer) amount)) month)
                    my-customer
                    )
                )
              (let ((free-amount (- (customer->balance my-customer) (sum-of-blocked-money my-customer loan-types))))
                (if (>= free-amount amount)
                    (customer-last-decrease-month-setter (customer-balance-setter my-customer (- (customer->balance my-customer) amount)) month)
                    my-customer
                    )
                )
            )
          (if valid
              (customer-credit-setter my-customer (- (/ (account->credit account) 2)))
              my-customer
          )
          )
      )
    )
  )


; equiv to spend
(define transfer
  (lambda (my-customer amount account-types loan-types month)
    (spend customer amount account-types loan-types month)
    )
  )

; equiv to spend
(define withdraw
  (lambda (my-customer amount account-types loan-types month)
    (spend customer amount account-types loan-types month)
    )
  )

(define 
(define new-loan
  (lambda (my-customer loan-type-id account-types loan-types month)
    (let ((loan (find-loan-type loan-type-id loan-types))
          (minimum-credit (loan-type->minimum-credit loan))
          (last-loan (loan-type->last-loan loan))
          (blocking-money (loan-type->blocking-money loan))
          (account-id (customer->account-type-id customer))
          (account (find-account-type account-id account-types)))
      (if (and (>= (customer->credit my-customer) minimum-credit) (>= (free-money my-customer account))))
      )
    )
  )

(define pay-debt
  (lambda  (my-customer amount account-types loan-types month)
    my-customer
    )
  )

(define withdraw-loan
  (lambda (my-customer account-types loan-types month)
    my-customer
    )
  )

(define apply-command 
  (lambda (command all-customers-month account-types loan-types)
    (let ([all-customers (car all-customers-month)]
          [month (cadr all-customers-month)])
      (match command
        [(list 'time) (cons (pass-time-all all-customers account-types loan-types month) (list (+ month 1)))]
        [else 
         (cons 
          ; this section returns the new all-customers
          (let ([customer-id (cadr command) ])
            (let ([customer-index (find-customer-index customer-id all-customers)])
              (if customer-index
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