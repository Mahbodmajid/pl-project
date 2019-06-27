#lang racket

(define program-url "sample/sample_input.txt")

(define remove-all
  (lambda (ele lst)
    (cond
      ((null? lst) '())           ; if the list is empty then we're done
      ((not (pair? (car lst)))    ; if the first element is an atom
       (if (equal? (car lst) ele) ; check if it's the one we're looking for
           (remove-all ele (cdr lst)) ; if so we skip over it, eliminating it
           (cons (car lst) (remove-all ele (cdr lst))))) ; otherwise we add it
      (else (cons (remove-all ele (car lst))      ; else advance recursion
                  (remove-all ele (cdr lst))))))) ; over both `car` and `cdr`


(define program-lines
  (remove-all ""
              (map string-normalize-spaces
                   (file->lines program-url #:mode'text))))

(define extract-setup-commands
  (lambda (lines)
    (let ([store '(() ()) ])
      (extract-setup-commands-helping lines 'doesntmatter store))))

; ((setup) (commands))


(define extract-setup-commands-helping
  (lambda (lines dir result)
    (if (equal? lines '())
        result
        (let ([head (car lines)]
              [tail (cdr lines)]
              [setup (car result)]
              [commands (cadr result)])
          (cond
            [(equal? head "setup") (extract-setup-commands-helping tail 's result)]
            [(equal? head "commands") (extract-setup-commands-helping tail 'c result)]
            [(equal? dir 's) (extract-setup-commands-helping tail 's
                                                             (list (append setup (list head)) commands))]
            [(equal? dir 'c) (extract-setup-commands-helping tail 'c
                                                             (list setup (append commands (list head))))]
            [else (error "something is wrong")])))))

(define-syntax-rule (set-commands-setup! c s)
  (match (extract-setup-commands program-lines)
    [(list lsetup lcommands)
     (begin
     (set! c lcommands)
     (set! s lsetup))
     ]))

(provide set-commands-setup!)