#lang racket

(require "program-to-setup-and-command.rkt")

(require "data-types.rkt")
(require "utils.rkt")
(require "setup-to-account-loan-objects.rkt")
(require "commands-translator.rkt")

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