#lang racket
(define-syntax minha-macro (lambda arg (syntax "executa isso")))

(define-syntax minha-macro2
(lambda (arg-sintatico)
(displayln arg-sintatico)
(displayln (syntax->datum arg-sintatico))
(syntax "Executa isto")))

(define-syntax (inverta-me arg)
  (datum->syntax arg (reverse (cdr (syntax->datum arg)))
    ))

(define-syntax (meu-if args)
  (syntax-case args ()
    [(_ condition true false)
     #'(cond[condition true][else false])
     ]
    )
  )

(define-syntax-rule (meu-if-2 condition true false)
  (cond [condition true][else false])
  )

(define-syntax (hello-world stx)
  (syntax-case stx ()
    [(_ nome lugar)
     (with-syntax ([print-nome #'(printf "~a\n" 'nome)]
                   [print-lugar #'(printf "~a\n" 'lugar)]
                   [nome
                    (datum->syntax #'nome
                                   (string->symbol
                                    (format"hello-~a"
                                           (syntax->datum #'nome))))]
       [lugar (datum->syntax #'lugar (string->symbol (format"hello-~a" (syntax->datum #'lugar))))])
    
       #'(begin
           (define (nome times)
             (printf "Hello\n")
             (for ([i (in-range 0 times)]) print-nome))
           (define (lugar times)
             (printf "From\n")
             (for ([i (in-range 0 times)]) print-lugar))
  ))]))
