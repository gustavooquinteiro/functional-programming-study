#lang racket

;; Prova 1 Solução

; Nesta prova vamos escrever um conjunto de versões para a macro pipe.
; Esta macro deve produzir uma forma sintática que encadeia
; linearmente a saída de uma expressão na próxima expressão listada.
; Para isto vamos, inicialmente, usar a  seguinte gramática:

;    (pipe val-expr clause ...)
;    clause	= (fn-expr arg-expr ...)
 	 	  
; Por exemplo:

; (pipe 10 (+ 2) (* 5)) , deve resultar na sintaxe (* (+ 10 2) 5) ,
; produzindo 60 quando executada.

; Ou seja,  jogamos o valor 10 na expressão (+ 10 2) e o seu
; resultado na expressão seguinte 
; (* resultado 5)  , resultando em (* (+ 10 2) 5)

; Da mesma maneira:

; (pipe 'a (cons '(b)) (append '(1 2 3))) , deve resultar na sintaxe
; (append (cons 'a '(b)) '(1 2 3)) 
; produzindo '(a b 1 2 3) quando executada.

; Ou seja, jogamos o valor 'a na expressão (cons 'a '(b)) e o seu
; resultado na expressão seguinte
; (append resultado '(1 2 3))  ,
; resultando em (append (cons 'a '(b)) '(1 2 3))

;; QUESTÃO 1
;;
;; Escreva pipe-v1, uma versão simplificada de pipe
;; que tem a seguinte gramática:
;;
;; (pipe-v1 val-expr)


; A macro deve passar nos seguintes testes
;
; > (pipe-v1)
; . pipe-v1: bad syntax in: (pipe-v1)
; > (pipe-v1 3)
; 3
; > (pipe-v1 (* 4 30))
; 120

;; Solução

(define-syntax (pipe-v1 stx)
  (syntax-case stx ()
      [(_ val-expr) #'val-expr]
      )
  )



;; QUESTÃO 2
;;
;; Escreva pipe-v2, uma versão simplificada de pipe que tem a seguinte gramática:
;;
;; (pipe-v2 val-expr) |
;; (pipe-v2 val-expr (fn-expr arg-expr ...))


; A macro deve passar nos seguintes testes
;
; > (pipe-v2)
; . pipe-v2: bad syntax in: (pipe-v2)
; > (pipe-v2 4)
; 4
; > (pipe-v2 (* 4 30))
; 120
; > (pipe-v2 5 (* 4 30))
; 600
; >

;; Solução

(define-syntax (pipe-v2 stx)
  (syntax-case stx ()
    [(_ val-expr) #'val-expr]
    [(_ val-expr (fn-expr arg-expr ...))
          #'(fn-expr val-expr arg-expr ...)]
    )
)



;; QUESTÃO 3

;; Escreva pipe-v3, uma versão de pipe que tem a seguinte gramática:
;;
;; (pipe-v3 val-expr clause ...)
;; clause = (fn-expr arg-expr ...)

; A macro deve passar nos seguintes testes
;
; > (pipe-v3)
; . pipe-v3: bad syntax in: (pipe-v3)
; > (pipe-v3 4)
; 4
; > (pipe-v3 (* 4 30))
; 120
; > (pipe-v3 5 (* 4 30))
; 600
; > (pipe-v3 5 (* 4 30) (+ 10) (/ 2))
; 305
; > (pipe-v3 #\a (list #\z) (list->string))
; "az"
; > (pipe-v3 'abc (symbol->string) (string-ref 0) (char->integer) (- 2))
; 95

;; Solução

(define-syntax (pipe-v3 stx)
  (syntax-case stx ()
    [(_ val-expr) #'val-expr]
    [(_ val-expr (fn-expr arg-expr ...) clause ...)
          #'(pipe-v3 (fn-expr val-expr arg-expr ...) clause ...)]
    )
)



;; QUESTÃO 4

;; Escreva pipe-v4, uma versão de pipe que tem a seguinte gramática:
;;
;; (pipe-v4 val-expr clause ...)
;; clause = (fn-expr arg-expr ...) |
;;          id-fn-expr
;;
;; Esta gramática permite a inclusão de funções sem parênteses,
;; id-fn-expr, no encadeamento. Esta notação deve ser equivalente
;; a escrever (id-fn-expr) e devem resultar na sintaxe
;; (id-fn-expr resultado), sobre a sintaxe anterior.


; A macro deve passar nos seguintes testes
;
; > (pipe-v4)
; . pipe-v3: bad syntax in: (pipe-v4)
; > (pipe-v4 4)
; 4
; > (pipe-v4 (* 4 30))
; 120
; > (pipe-v4 5 (* 4 30))
; 600
; > (pipe-v4 5 (* 4 30) (+ 10) (/ 2))
; 305
; > (pipe-v4 #\a (list #\z) (list->string))
; "az"
; > (pipe-v4 'abc (symbol->string) (string-ref 0) (char->integer) (- 2))
; 95
; > (pipe-v4 #\a (list #\z) list->string)
; "az"
; > (pipe-v4 'abc symbol->string (string-ref 0) char->integer (- 2))
; 95

;; Solução

(define-syntax (pipe-v4 stx)
  (syntax-case stx ()
    [(_ val-expr) #'val-expr]
    [(_ val-expr (fn-expr arg-expr ...) clause ...)
          #'(pipe-v4 (fn-expr val-expr arg-expr ...) clause ...)]
    [(_ val-expr id-fn-expr clause ...)
          #'(pipe-v4 (id-fn-expr val-expr) clause ...)]
    )
)

;; QUESTÃO 5

;; As gramáticas anteriores sempre inseriam o resultado da expressão
;; anterior como o primeiro parâmetro da expressão sendo processada.
;; Vamos criar uma macro que permite a inclusão de um marcador "_"
;; para indicar aonde o resultado da expressão anterior deve se
;; encaixar na expressão sendo processada.

;; Escreva pipe-v5, uma versão de pipe que tem a seguinte gramática:

;; (pipe-v5 val-expr clause ...)
;;  clause =(fn-expr pre-expr ... _ post-expr ...) |
;;          (fn-expr arg-expr ...) |
;;          id
;;
;; DICA
;;
;; O syntax-case não deixa você usar a expressão
;; (_ val-expr (fn-expr pre-expr ... '_ post-expr ...))
;; como template
;;
;; use um MATCH com
;; (list _ val-expr (list fn-expr pre-expr ... '_ post-expr ... ))

; A macro deve passar nos seguintes testes
;
; > (pipe-v5)
; . pipe-v5: bad syntax in: (pipe-v5)
; > (pipe-v5 4)
; 4
; > (pipe-v5 (* 4 30))
; 120
; > (pipe-v5 5 (* 4 30))
; 600
; > (pipe-v5 5 (* 4 30) (+ 10) (/ 2))
; 305
; > (pipe-v5 #\a (list #\z) (list->string))
; "az"
; > (pipe-v5 'abc (symbol->string) (string-ref 0) (char->integer) (- 2))
; 95
; > (pipe-v5 #\a (list #\z) list->string)
; "az"
; > (pipe-v5 'abc symbol->string (string-ref 0) char->integer (- 2))
; 95
; > (pipe-v5 1 (list-ref '(10 20) _))
; 20
; > (pipe-v5 '(1 2 3) (map add1 _) (apply + _) (/ 1 7))
; 1 2/7

(require (for-syntax racket/match))

(define-syntax (pipe-v5 stx)
  (match (syntax->datum stx)
    ;; processa caso base
    [(list _ val-expr)
     (datum->syntax stx val-expr)]
    ;; processa clausula com expressão com marcador
    [(list _ val-expr (list fn-expr pre-expr ... '_ post-expr ...) clause ...)
     (datum->syntax stx
                    (cons 'pipe-v5
                          (cons 
                                (cons fn-expr
                                      (append pre-expr
                                             (cons val-expr
                                                   post-expr)))
                                clause)))]
    ;; processa clausula com expressão sem marcador
    [(list _ val-expr (list fn-expr arg-expr ...) clause ...)
     (datum->syntax stx
                    (cons 'pipe-v5
                          (cons 
                                (cons fn-expr
                                      (cons val-expr
                                            arg-expr))
                                clause)))]
    ;; processa clausula com id
    [(list _ val-expr id clause ...)
     (datum->syntax stx
                    (cons 'pipe-v5
                          (cons (list id val-expr)
                                clause)))]
 ))

;; Outra solução
;; com quasiquotes

(define-syntax (pipe-v5-2 stx)
  (match (syntax->datum stx)
    ;; processa caso base
    [(list _ val-expr)
     (datum->syntax stx val-expr)]
    ;; processa clausula com expressão com marcador
    [(list _ val-expr (list fn-expr pre-expr ... '_ post-expr ...) clause ...)
     (datum->syntax stx
           `(pipe-v5-2
               ,(append (cons fn-expr pre-expr) (cons val-expr post-expr)) . ,clause))]
    ;; processa clausula com expressão sem marcador
    [(list _ val-expr (list fn-expr arg-expr ...) clause ...)
     (datum->syntax stx
           `(pipe-v5-2 ,(cons fn-expr (cons val-expr arg-expr)) . ,clause))]
    ;; processa clausula com id
    [(list _ val-expr id clause ...)
     (datum->syntax stx
            `(pipe-v5-2 ,(list id val-expr) . ,clause))]
 )) 
