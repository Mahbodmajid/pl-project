#lang racket

(provide translate-command)

(define translate-command
    (lambda (command)
        (let ([command-word-list (string-split (string-replace command "." " "))])
        (match command-word-list
        [(list "Time" "goes" "by") (list 'time)]
        [(list "Customer" customer-id "wants" "to" "create" "an" "account" "of" "type" account-type-id "Customer" customer-id "wants" "to" "start" "with" initial-money "Tomans")
        (list 'new-account customer-id account-type-id initial-money)]
        [(list "Customer" customer-id "adds" amount "Tomans" "to" "his" "account")
        (list 'add-money customer-id amount)]
        [(list "Customer" customer-id "requests" "renewal")
        (list 'renewal customer-id)]
        [(list "Customer" customer-id "writes" "a" "cheque" "for" amount "Tomans")
        (list 'write-cheque customer-id amount)]
        [(list "Customer" customer-id "spends" amount "Tomans" "via" "his" "card")
        (list 'spend customer-id amount)]
        [(list "Customer" customer-id "transfers" amount "Tomans")
        (list 'transfer customer-id amount)]
        [(list "Customer" customer-id "withdraws" amount "Tomans" "from" "his" "account")
        (list 'withdraws customer-id amount)]
        [(list "Customer" customer-id "closes" "his" "account")
        (list 'close customer-id)]
        [(list "Customer" customer-id "requests" "a" "loan" "of" "type" loan-type-id)
        (list 'new-loan customer-id loan-type-id)]
        [(list "Customer" customer-id "pays" amount "Tomans" "of" "his" "debt")
        (list 'pay-debt customer-id amount)]
        [(list "Customer" customer-id "withdraws" "the" "loan")
        (list 'withdraw-loan customer-id)]
        [else (raise-argument-error 'translate-command "command?" command)]
        ))))