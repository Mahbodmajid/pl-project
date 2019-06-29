#lang racket

(require (lib "eopl.ss" "eopl"))

(provide (all-defined-out))

(define-datatype account-type account-type?
  (a-account-type (account-id string?)
                  (current-account boolean?)
                  (bank-fee number?)
                  (minimum-deposit number?)
                  (monthly boolean?)
                  (period number?)
                  (renewable boolean?)
                  (interest-rate number?)
                  (credit number?)
                  (variability boolean?)
                  (span-for-increase number?)
                  (increase-rate number?)
                  (has-cheque boolean?)
                  (has-card boolean?)
                  (transfer-fee number?)))

(define-datatype loan-type loan-type?
  (no-loan)
  (a-loan-type (loan-id string?)
               (loan-amount number?)
               (blocking-money number?)
               (return-span number?)
               (interest number?)
               (last-loan number?)
               (minimum-credit number?)
               ))

(define-datatype customer customer?
  (a-customer (customer-id string?)
              (start-month number?)
              (contract-start-month number?)
              (account-type-id string?)
              (history (list-of number?))
              (initial-balance (number?))
              (balance number?)
              ; (blocked-balance number?) computable from debts
              (last-decrease-month number?)
              (credit number?)
              (new-interest-rate number?) ;(for variable yearly interest)
              (interest-rate number?)
              (debts (list-of debt?))))

(define-datatype debt debt?
  (a-debt (loan-type-id string?)
          (start-month number?)
          (paid number?)
          (done boolean?)
          (withdrawn boolean?)))