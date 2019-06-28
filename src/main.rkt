#lang racket

(require "program-to-setup-and-command.rkt")
(require "data-types.rkt")
(require "utils.rkt")
(require "setup-to-account-loan-objects.rkt")
(require "commands-translator.rkt")
(require "apply-command")

(define commands '())
(define setup '())

(set-commands-setup! commands setup)

commands
setup

(define account-types '())
(define loan-types '())

(set-account-loan-types! account-types loan-types setup)
account-types
loan-types

; '(list-of-all-customers month)
(define all-customers-month '(() 0)

(define apply-commands
    (lambda (cs) 
        (if (null? cs)
            'finish
            (let ([current (translate-command (car cs))]
                [rest (cdr cs)])
                (begin
                    (set! all-customers-month (apply-command all-customers-month account-types loan-types))
                    (apply-commands rest)
                )
            )
        )
    )
)

(apply-commands commands)

all-customers