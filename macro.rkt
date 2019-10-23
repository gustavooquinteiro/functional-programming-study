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


(define minha-funcao
  (lambda (meu-argumento)
       "hello world"))


(define-syntax minha-macro
  (lambda (meu-arg-sintatico)
     (syntax "meu-arg-sintatico é substituido por isto aqui")))

(define-syntax minha-macro2
  (lambda (arg-sintatico)
    (displayln arg-sintatico)
    (displayln (syntax->datum arg-sintatico))
    (syntax "Retorna isto")))

;; loop infinito - pense porquê ??
(define-syntax minha-macro-xx
  (lambda (arg-sintatico)
    (displayln (syntax->datum arg-sintatico))
    arg-sintatico))


;; versão abreviada de syntax
(define-syntax minha-macro3
  (lambda (meu-arg-sintatico)
     (syntax "Executa isto")))

(define-syntax minha-macro4
  (lambda (meu-arg-sintatico)
     #'"Executa isto"))


;; fazendo uma transformação
(define-syntax (diga-alo arg)
    (datum->syntax
        arg
        `(display ,(cadr (syntax->datum arg)))))

;; Inverte sintaxe
(define-syntax (inverta-me stx)
    (datum->syntax stx 
                   (reverse (cdr (syntax->datum stx)))))

; (inverta-me "contrário" "ao" "Estou" values)

;; exemplo com IF

(define (meu-if condition true-expr false-expr)
    (cond [condition true-expr]
          [else false-expr]))

(define (meu-if2 condx exp1 exp2)
  (cond
    [(eval condx) (eval exp1)]
    [else (eval exp2)]
  ))

(define-syntax (meu-if-v2 stx)
    (define xs (syntax->datum stx))
    (datum->syntax stx `(cond [,(cadr xs) ,(caddr xs)]
                              [else ,(cadddr xs)])))


(require (for-syntax racket/list))

(define-syntax (meu-if-v3 stx)
    (define xs (syntax->datum stx))
    (datum->syntax stx `(cond [,(second xs) ,(third xs)]
                              [else ,(fourth xs)])))


(require (for-syntax racket/match))

(define-syntax (meu-if-v4 stx)
    (match (syntax->datum stx)
      [(list name condition true-expr false-expr)
       (datum->syntax stx `(cond [,condition ,true-expr]
                                 [else ,false-expr]))]
      ))


(define-syntax (meu-if-v4-1 stx)
    (match (syntax->datum stx)
      [(list name condition true-expr false-expr)
       (datum->syntax stx `(cond [,condition ,true-expr]
                                 [else ,false-expr]))]
      [(list name condition true-expr)
       (datum->syntax stx `(cond [,condition ,true-expr]))]
      ))



;; meu if com syntax-case
(define-syntax (meu-if-v5 stx)
    (syntax-case stx ()
      [(_ condition true-expr false-expr)
       #'(cond [condition true-expr]
               [else false-expr])]))



;; meu if com syntax-case
(define-syntax (meu-if-v5-1 stx)
    (syntax-case stx ()
      [(_ condition true-expr false-expr)
       #'(cond [condition true-expr]
               [else false-expr])]
      [(_ condition true-expr)
       #'(cond [condition true-expr])]
      ))



;; meu if com syntax-case
(define-syntax (meu-if-v5-2 stx)
    (syntax-case stx ()
      [(_ condition true-expr false-expr)
       #'(cond [condition true-expr]
               [else false-expr])]
      [(_ condition true-expr)
       #'(cond [condition true-expr])]
      ))

;; meu if com syntax rule
(define-syntax-rule (meu-if-v6 condition true-expr false-expr)
    (cond [condition true-expr]
          [else false-expr]))


;; Exemplo de swap de valores

(define-syntax (swap stx)
  (syntax-case stx ()
    [(swap x y) #'(let ([tmp x])
                    (set! x y)
                    (set! y tmp))]))


;; Exemplo de swap de valores

(define-syntax (swap21 stx)
  (syntax-case stx ()
    [(swap21 x y) #'(let ([tmp x])
                    (define x y)
                    (define y tmp))]))

;; Exemplo de swap com syntax-rule
(define-syntax-rule (swap-sr v1 v2)
  (let ([tmp v1])
    (set! v1 v2)
    (set! v2 tmp)))

;; Exemplo de swap com mensagens de erro
(define-syntax (swap2 stx)
  (syntax-case stx ()
    [(swap x y)
     (if (and (identifier? #'x)
              (identifier? #'y))
         #'(let ([tmp x])
             (set! x y)
             (set! y tmp))
         (raise-syntax-error #f
                             "not an identifier"
                             stx
                             (if (identifier? #'x)
                                 #'y
                                 #'x)))]))

; (let ([x 1] [y 2]) (swap x y)(values x y))



;; meu adiciona hífen
;; objetivo
;; Substituir (define-hyphen a b (args) body) por
;;            (define (a-b args) body)

(define-syntax (define-hifenizado0 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
         #'(define (a-b args ...) body0 body ...)
         ]))


; (define-hyphen ma noel (d e) (+ d e))


;; (define-hyphen11 ma noel (x y) (+ x y))

;; define ma

(define-syntax (define-hifenizado1 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     
         #'(define (a args ...) body0 body ...)
         
         ]))

;; define noel
(define-syntax (define-hifenizado2 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     
         #'(define (b args ...) body0 body ...)
         
         ]))

;; Identificador aaa não localizado como antes em a-b

(define-syntax (define-hifenizado3 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     
         #'(define (aaa args ...) body0 body ...)
         ;#'"aaa"
         ]))


;; cria nova variável sintática usando outro syntax-case
;;
;; no exemplo criamos a função foo com os args e
;; corpo passado
;;
;; Agora funciona, conseguios criar a função foo
;;
(define-syntax (define-hifenizado4 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
        (syntax-case
               (datum->syntax #'a 'aaa)
               ()
          [nome
              #'(define (nome args ...) body0 body ...)
           ])
        ]
    ))

;;

;; Primeira tentativa
;; Vamos substituir
;;
;; (datum->syntax #'a 'foo)  
;;  por
;; (datum->syntax #'a (string->symbol (format "~a-~a" a b)))
;;

(define-syntax (define-hifenizado5 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
        (syntax-case
       ;  (datum->syntax #'a (string->symbol (format "~a-~a" a b)))
          (datum->syntax #'a (string->symbol (format "~a-~b" 'a 'b)))
          ()
          [nome
              #'(define (nome args ...) body0 body ...)
           ])
        ]
    ))


;; A rotina acima dá erro!
;;
;; Precisamos lembrar que valores de variáveis de
;; padrão devem ser acessadas de dentro de uma sintaxe
;;
;; Vamos então usar #'a e #'b
;;
;; Neste caso precisamos substituir
;;
;; (datum->syntax #'a (string->symbol (format "~a-~a" a b)))
;;
;; por
;;
;; (datum->sintaxe #'a
;;    (string->symbol 
;;       (format  "~a-~a"
;;          (syntax->datum #'a)
;;          (syntax->datum #'b))))
;;
;; Ficamos então com
;;
(define-syntax (define-hifenizado stx)
    (syntax-case stx ()
      [(_ a b (args ...) corpo0 resto ...)
       (syntax-case 
                 (datum->syntax #'a   ;; minha stx
                   (string->symbol
                     (format "~a-~a"
                       (syntax->datum #'a)
                       (syntax->datum #'b))))
                 ()                  ;; ()
          [nome  ;; template
             #'(define (nome args ...)
                          corpo0 resto ...)])]))

;; Esta versão finalmente funciona !
;;
; (define-hifenizado foo bar () #t)
; (define-hifenizado mul soma (x y) (* x (+ x y)))


;; Hifenizador com with-syntax
(define-syntax (define-hifenizado6 stx)
    (syntax-case stx ()
      [(_ a b (args ...) corpo0 resto ...)
       (with-syntax
            ([nome 
                 (datum->syntax #'a   ;; minha stx
                   (string->symbol
                     (format "~a-~a"
                       (syntax->datum #'a)
                       (syntax->datum #'b))))])
           ; [nome2 (datum->syntax #'a (syntax->datum #'foo))])
            #'(define (nome args ...)
                         corpo0 resto ...))]))


;; Hello-world
(define-syntax (hello-world stx)
    (syntax-case stx ()
      [(_ nome lugar)
       (with-syntax ([print-nome #'(printf "~a\n" 'nome)]
                     [print-lugar #'(printf "~a\n" 'lugar)])
         #'(begin
             (define (nome times)
               (printf "Hello\n")
               (for ([i (in-range 0 times)])
                    print-nome))
             (define (lugar times)
               (printf "From\n")
               (for ([i (in-range 0 times)])
                    print-lugar))))]))


;; Hello-world - Exercício
(define-syntax (hello-world2 stx)
    (syntax-case stx ()
      [(_ nome lugar)
       (with-syntax ([print-nome #'(printf "~a\n" 'nome)]
                     [print-lugar #'(printf "~a\n" 'lugar)]
                     [f-nome (datum->syntax
                              #'nome
                               (string->symbol
                                (format "hello-~a"
                                   (syntax->datum #'nome))))]
                     [f-lugar (datum->syntax
                              #'lugar
                               (string->symbol
                                (format "hello-~a"
                                   (syntax->datum #'lugar))))]
                     )
         #'(begin
             (define (f-nome times)
               (printf "Hello\n")
               (for ([i (in-range 0 times)])
                    print-nome))
             (define (f-lugar times)
               (printf "From\n")
               (for ([i (in-range 0 times)])
                    print-lugar))))]))

; (hello-world2 manoel salvador)
; (hello-manoel 3)
; (hello-salvador 2)


;; withsyntax*
(require (for-syntax racket/syntax))
(define-syntax (foo stx)
    (syntax-case stx ()
      [(_ a)
        (with-syntax* ([b #'a]
                       [c #'b])
          #'c)]))


;; format-id
(require (for-syntax racket/syntax))

(define-syntax (define-hifenizado7 stx)
    (syntax-case stx ()
      [(_ a b (args ...) corpo0 resto ...)
       (with-syntax
            ([nome 
                 (format-id #'a "~a-~a" #'a #'b)])
            #'(define (nome args ...)
                         corpo0 resto ...))]))



;; Hello-world3 com format-id
(define-syntax (hello-world3 stx)
    (syntax-case stx ()
      [(_ nome lugar)
       (with-syntax ([print-nome #'(printf "~a\n" 'nome)]
                     [print-lugar #'(printf "~a\n" 'lugar)]
                     [f-nome (format-id #'nome "hello-~a" #'nome)]
                     [f-lugar(format-id #'lugar "hello-~a" #'lugar)]
                     )
         #'(begin
             (define (f-nome times)
               (printf "Hello\n")
               (for ([i (in-range 0 times)])
                    print-nome))
             (define (f-lugar times)
               (printf "From\n")
               (for ([i (in-range 0 times)])
                    print-lugar))))]))

; (hello-world3 manoel salvador)
; (hello-manoel 3)
; (hello-salvador 2)


;; Meu cond-v1
;; cond nulo
(define-syntax (meu-cond-v1 stx)
  (syntax-case stx ()
    [(_) #'(void)]
   ))


;; Meu cond-v2n
;; cond nulo ou com uma condição sem else
(define-syntax (meu-cond-v2 stx)
  (syntax-case stx ()
    [(_) #'(void)]
    [(_ {{cond-expr s-expr}})
        #'(if cond-expr
              s-expr
              (meu-cond-v2))]
   ))


;; Meu cond-v3
;; cond nulo, cond com uma expressão ou
;; cond com uma exprressão seguida de else
(define-syntax (meu-cond-v3 stx)
  (syntax-case stx ()
    [(_) #'(void)]
    [(_ {{cond-expr s-expr}})
        #'(if cond-expr
              s-expr
              (meu-cond-v3))]
    [(_ {{cond-expr s-expr}} {{else final-expr}})
     (eq? 'else (syntax->datum #'else)) 
        #'(if cond-expr
              s-expr
              final-expr)]
   ))


;; Meu cond-v4
;; cond nulo, cond com uma expressão ou
;; cond com uma exprressão seguida de else
;; ou cond com N expressões seguidas ou não por else
(define-syntax (meu-cond-v4 stx)
  (syntax-case stx ()
    [(_) #'(void)]
    [(_ {{cond-expr s-expr}})
        #'(if cond-expr
              s-expr
              (meu-cond-v4))]
    [(_ {{cond-expr s-expr}} {{var-else final-expr}})
        (eq? 'else (syntax->datum #'var-else))
        #'(if cond-expr
              s-expr
              final-expr)]
    [(_ {{cond-expr s-expr}} outras-cond ...)
        #'(if cond-expr
              s-expr
              (meu-cond-v4 outras-cond ...))]
   ))

(define (compara2 x y)
  (meu-cond-v4
   {{(< x y) 'menor}}
   {{(> x y) 'maior}}
   {{else 'iguais}}
  ))


;; (meu-cond-v4 {{#f 4}} {{#f 3}})
;; (meu-cond-v4 {{#f 4}} {{#f 3}} {{else}} 7)



(require (for-syntax racket/match))
         
(define-syntax (meu-cond-v5 stx)
  (match (syntax->datum stx)
    [(list _) (datum->syntax stx '(void))]
    [(list _ 'begin cond-expr s-expr 'end )
        (datum->syntax stx
                       `(if ,cond-expr
                            ,s-expr
                            (meu-cond-v5)))]
    [(list _ 'begin cond-expr s-expr 'end 'begin 'else final-expr 'end)
         (datum->syntax stx
                       `(if ,cond-expr
                            ,s-expr
                            ,final-expr))]
   ; [(list _ expr ...) (datum->syntax stx `(quote ,expr))]
    [(list _ 'begin cond-expr s-expr 'end outras_cond ...)
        (datum->syntax stx
                       `(if ,cond-expr
                            ,s-expr
                            (meu-cond-v5  . ,outras_cond)))]
   ))

(define (compara3 x y)
  (meu-cond-v5 begin (> x y) 'maior end
               begin (< x y) 'menor end
               begin else 'igual end
              ))

;; Testes
;; (meu-cond-v5 begin #f 4 end begin #f 3 end begin else 5 end )
;; (meu-cond-v5 begin #f 4 end begin #f 3 end )

(define-syntax (meu-cond-v6 stx)
  (match (syntax->datum stx)
    [(list _) (datum->syntax stx '(void))]
    [(list _ 'tente cond-expr s-expr)
        (datum->syntax stx
                       `(if ,cond-expr
                            ,s-expr
                            (meu-cond-v6)))]
    [(list _ 'tente cond-expr s-expr 'senao final-expr)
         (datum->syntax stx
                       `(if ,cond-expr
                            ,s-expr
                            ,final-expr))]
   ; [(list _ expr ...) (datum->syntax stx `(quote ,expr))]
    [(list _ 'tente cond-expr s-expr outras_cond ...)
        (datum->syntax stx
                       `(if ,cond-expr
                            ,s-expr
                            (meu-cond-v6  . ,outras_cond)))]
   ))


(define (compara4 x y)
  (meu-cond-v6
       tente (> x y) 'maior
       tente (< x y) 'menor
       senao 'igual
              ))
 
 
(require (for-syntax racket/string))

;; este arquivo contém as funções recebe uma string de caracteres
;; separados por hífen e faz camel case dela

;; (camel-case string regexp)
;; recebe uma string de caracteres separados por hífen (default) ou
;; outro caracter, e transforma a mesma em uma string camel case.
;;
;; Exemplos:
;; (camelCase "manoel-gomes-mendonca")
;; > "manoelGomesMendonca"
;; (camelCase "manoel$gomes$mendonca" "$")
;; > "manoelGomesMendonca"
;;
(define-for-syntax (camel-case str [c "-"])
  (let ([str-list (string-split str c)]) ; 
    (cond
      [(null? str-list) str]
      [(null? (cdr str-list)) str]
      [else
         (string-append (car str-list)
                        (foldr string-append
                               ""
                               (map coloque-maiuscula
                                    (cdr str-list))))])))

;; (coloque-maiuscula string)
;; (transforma o primeiro caractere de uma string em maiúscula)
(define-for-syntax (coloque-maiuscula str)
  (string-append
      (string-upcase (substring str 0 1))
      (substring str 1)))


;; Dada uma função com nome hífenizado, cria uma cópia
;; da mesma utilizando a convenção Camel Case
;; exemplo:
;;
;; (camel-case-m minha-funcao-hifenizada)
;;
;; Cria uma cópia minha-funcao-hifenizada com o nome
;;
;; minhaFuncaoHifenizada
;;
(define-syntax (camel-case-m stx)
  (syntax-case stx ()
      [(_ func)
       (with-syntax
           ([nome
                (datum->syntax #'func
                      (string->symbol
                         (camel-case
                             (symbol->string
                              (syntax->datum #'func)))))])
         #'(define nome func)
       )]
    ))


;; teste
(define (minha-funcao a b c)
  (+ a b c))
                                          
;; Cria uma função com nome Camel Case a partir de uma
;; sequencia de nomes, usando o padrão:
;;
;; (define-cc nomes ... (args) body0 body)
;;
;; por exemplo:
;;
;; (define-cc minha funcao com camel case (x y) (+ x y))
;;
;; deve gerar
;;
;; (define (minhaFuncaoComCamelCase x y) (+ x y))
;;
(define-syntax (define-cc stx)
  (syntax-case stx ()
      [(_ nome0 nomes ... (args ...) body0)
       (with-syntax
           ([nome
                (datum->syntax #'nome0
                      (string->symbol
                         (camel-case2
                             (map symbol->string
                                  (syntax->datum #'(nome0 nomes ...))))))])
         #'(define (nome args ...) body0)
       )]
    ))

(define-for-syntax (camel-case2 str-list) 
    (cond
      [(null? (cdr str-list)) (car str-list)]
      [else
         (string-append (car str-list)
                        (foldr string-append
                               ""
                               (map coloque-maiuscula
                                    (cdr str-list))))]))

; BOTA COMO CASE------------------------
(define-syntax (define-hifenizado stx)
                 (syntax-case stx()
                   [( _ nomes ... (args ...) corpo)
                    (with-syntax
                        ([nome (datum->syntax stx
                                             (string->symbol
                                              (string-append (symbol->string (car (syntax->datum #'(nomes ...))))
                                               (transforma-hifen
                                                (map symbol->string
                                                      (cdr (syntax->datum #'(nomes ...))))))))])
                      #'(define (nome args ...) corpo))]))




(define-for-syntax (transforma-hifen l)
  (cond
    [(null? (cdr l)) (string-append (string-upcase (substring (car l) 0 1)) (substring (car l) 1))]
    [else (string-append (string-append (string-upcase (substring (car l) 0 1)) (substring (car l) 1)) (transforma-hifen (cdr l)))])) 

(define-syntax (define-hifenizado stx)
                 (syntax-case stx()
                   [( _ nomes ... (args ...) corpo)
                    (with-syntax
                        ([nome (datum->syntax stx
                                             (string->symbol
                                               (transforma-hifen
                                                (map symbol->string
                                                     (syntax->datum  #'(nomes ...))))))])
                      #'(define (nome args ...) corpo))]))




(define-for-syntax (transforma-hifen l)
  (cond
    [(null? (cdr l)) (car l)]
    [else (string-append (car l) "-" (transforma-hifen (cdr l)))])) 
                                    
