#lang racket

(require "data-types.rkt")


(provide (all-defined-out))

(define account->account-id 
    (lambda (account) 
        (cases account-type account
            [a-account-type 
            (account-id current_account bank-fee minimum-deposit monthly period renewable interest-rate credit variability span-for-increase has-cheque has-card transfer-fee)
            account-id]
            [else (raise-argument-error 'account->account-id "account-type?" account)]
        )
    )
)

(define account->bank-fee
    (lambda (account) 
        (cases account-type account
            [a-account-type 
            (account-id current_account bank-fee minimum-deposit monthly period renewable interest-rate credit variability span-for-increase has-cheque has-card transfer-fee)
            bank-fee]
            [else (raise-argument-error 'account->bank-fee "account-type?" account)]
        )
    )
)

(define account->minimum-deposit
    (lambda (account) 
        (cases account-type account
            [a-account-type 
            (account-id current_account bank-fee minimum-deposit monthly period renewable interest-rate credit variability span-for-increase has-cheque has-card transfer-fee)
            minimum-deposit]
            [else (raise-argument-error 'account->minimum-deposit "account-type?" account)]
        )
    )
)

(define account->monthly
    (lambda (account) 
        (cases account-type account
            [a-account-type 
            (account-id current_account bank-fee minimum-deposit monthly period renewable interest-rate credit variability span-for-increase has-cheque has-card transfer-fee)
            monthly]
            [else (raise-argument-error 'account->monthly "account-type?" account)]
        )
    )
)

(define account->period
    (lambda (account) 
        (cases account-type account
            [a-account-type 
            (account-id current_account bank-fee minimum-deposit monthly period renewable interest-rate credit variability span-for-increase has-cheque has-card transfer-fee)
            period]
            [else (raise-argument-error 'account->period "account-type?" account)]
        )
    )
)

(define account->renewable
    (lambda (account) 
        (cases account-type account
            [a-account-type 
            (account-id current_account bank-fee minimum-deposit monthly period renewable interest-rate credit variability span-for-increase has-cheque has-card transfer-fee)
            renewable]
            [else (raise-argument-error 'account->renewable "account-type?" account)]
        )
    )
)

(define account->interest-rate
    (lambda (account) 
        (cases account-type account
            [a-account-type 
            (account-id current_account bank-fee minimum-deposit monthly period renewable interest-rate credit variability span-for-increase has-cheque has-card transfer-fee)
            interest-rate]
            [else (raise-argument-error 'account->interest-rate "account-type?" account)]
        )
    )
)

(define account->credit
    (lambda (account) 
        (cases account-type account
            [a-account-type 
            (account-id current_account bank-fee minimum-deposit monthly period renewable interest-rate credit variability span-for-increase has-cheque has-card transfer-fee)
            credit]
            [else (raise-argument-error 'account->credit "account-type?" account)]
        )
    )
)

(define account->variability
    (lambda (account) 
        (cases account-type account
            [a-account-type 
            (account-id current_account bank-fee minimum-deposit monthly period renewable interest-rate credit variability span-for-increase has-cheque has-card transfer-fee)
            variability]
            [else (raise-argument-error 'account->variability "account-type?" account)]
        )
    )
)

(define account->span-for-increase
    (lambda (account) 
        (cases account-type account
            [a-account-type 
            (account-id current_account bank-fee minimum-deposit monthly period renewable interest-rate credit variability span-for-increase has-cheque has-card transfer-fee)
            span-for-increase]
            [else (raise-argument-error 'account->span-for-increase "account-type?" account)]
        )
    )
)

(define account->increase-rate
    (lambda (account) 
        (cases account-type account
            [a-account-type 
            (account-id current_account bank-fee minimum-deposit monthly period renewable interest-rate credit variability span-for-increase has-cheque has-card transfer-fee)
            increase-rate]
            [else (raise-argument-error 'account->increase-rate "account-type?" account)]
        )
    )
)

(define account->has-cheque
    (lambda (account) 
        (cases account-type account
            [a-account-type 
            (account-id current_account bank-fee minimum-deposit monthly period renewable interest-rate credit variability span-for-increase has-cheque has-card transfer-fee)
            has-cheque]
            [else (raise-argument-error 'account->has-cheque "account-type?" account)]
        )
    )
)

(define account->has-card
    (lambda (account) 
        (cases account-type account
            [a-account-type 
            (account-id current_account bank-fee minimum-deposit monthly period renewable interest-rate credit variability span-for-increase has-cheque has-card transfer-fee)
            has-card]
            [else (raise-argument-error 'account->has-card "account-type?" account)]
        )
    )
)

(define account->transfer-fee
    (lambda (account) 
        (cases account-type account
            [a-account-type 
            (account-id current_account bank-fee minimum-deposit monthly period renewable interest-rate credit variability span-for-increase has-cheque has-card transfer-fee)
            transfer-fee]
            [else (raise-argument-error 'account->transfer-fee "account-type?" account)]
        )
    )
)