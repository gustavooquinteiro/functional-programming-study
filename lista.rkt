#lang racket
(define (remove* e l)
  (cond
    [(null? l) l]
    [(equal? e (first l)) (remove* e (rest l))]
    [else (cons (first l)(remove* e (rest l)))]
   ))

(define (remove** e l)
  (cond
    [(null? l) l]
    [(list? (first l))(cons (remove** e (first l)) (remove** e (rest l)))]
    [(eq? e (first l))(remove** e (rest l))]
    [else (cons (first l)(remove** e (rest l)))]
    )
  )

(define (remove*** e l [f eq?])
  (cond
    [(null? l) l]
    [(list? (first l))(cons (remove*** e (first l) f) (remove*** e (rest l) f))]
    [(f e (first l))(remove*** e (rest l) f)]
    [else (cons (first l)(remove*** e (rest l) f))]
    )
  )

(define (remove4* e l [f eq?] [c identity])
  (cond
    [(null? l) l]
    [(list? (first l))(cons (remove4* e (first l) f) (remove4* e (rest l) f c))]
    [(f e (c (first l)))(remove4* e (rest l) f c)]
    [else (cons (first l)(remove4* e (rest l) f c))]
    )
  )
; funcoes das listas

(define (concatenar a b)
  (cond
    [(null? a) b]
    [(null? b) a]
    [else (cons(first a) (concatenar (rest a) b))]
    )  
  )
(define (concatenarInv a b)
  (concatenar b a)
  )

(define (concatenar2 ll)
  (cond
    [(null? ll) ll]
    [(list? (first ll)) (cons (first ll) (concatenar2 (rest ll)))]
    [else (cons ll (concatenar2 (rest ll)))]
    )
  )
(define (adicionarFinal z a)
  (cond
    [(null? a) z]
    [else (cons (first a) (adicionarFinal z (rest a)))]
    )
  )
(define (inverter l)
  (if (null?  l)
    l
    (inverter-aux (car l) (inverter (cdr l)))
    )
    )
  (define (inverter-aux n l)
    (if (null? l)
        (cons n '())
        (cons (car l)(inverter-aux n (cdr l)))
        )
    )
    
    
  
(define (juntar a b)
  (cond
    [(null? a) b]
    [(null? b) a]
    [else (cons (first a) (juntar b (rest a)))]
    )
  )


(define (parear a l)
  (cond
    [(null? l) a]
    [(null? (cdr l)) (cons a (car l))]
    [else (cons (cons a (first l)) (parear a (cdr l)))]
    )
  )
(define (pares l)
  (cond
    [(null? l) l]
    [(null? (cdr l)) l]
    [else (cons((parear (car l) (cdr l)) (pares(cdr l))))]
    )
  )
(define (conjunto c)
  (if(null? c)
    #t
    (and (verificar (car c) (cdr c)) conjunto(cdr c))
    )
  )
(define (verificar a c)
  (cond
    [(null? c) #t]
    [(eq? a (car c)) #f]
    [else (verificar a (cdr c))]
    )
  )

; Questões da prova
; Questão 1

(define (contagem-inversa n)
  (cond
    [(< n 1) '()]
    [else (cons n (contagem-inversa (- n 1)))]))


; Questão 2

(define (contagem n [acc '()])
  (cond
    [(< n 1) acc]
    [else (contagem (- n 1) (cons n acc))]))
     

  
; Questão Q3


(define (aplanar lg)
  (cond
    [(null? lg) lg]
    [(not (list? (first lg))) (cons (first lg) (aplanar (rest lg)))]
    [else
     (append (aplanar (first lg)) (aplanar (rest lg)))]))
  
  
 
; Questão 4

(define (prof-ll lg)
  (cond
    [(not (list? lg)) 0]
    [(null? lg) 1]
    [(list? (first lg)) 
       (let ([p_car (prof-ll (first lg))]
             [p_cdr (prof-ll (rest lg))])
         (if (> p_cdr (+ 1 p_car))
             p_cdr
             (+ 1 p_car)))]
    [else
     (prof-ll (rest lg))]))
  
  
; Q4  solução 2
(define (prof2-ll lg)
  (cond 
    [(not (list? lg)) 0]
    [(null? lg) 1]
    [else
      (+ 1 (apply max (map prof2-ll lg)))]))

; Q4 solução 3
(define (prof3-ll lg)
  (cond
    [(not (list? lg)) 0]
    [(null? lg) 1]
    [else
       (maximo (+ 1 (prof3-ll (first lg)))
               (prof3-ll (rest lg)))]))

(define (maximo n1 n2)
  (if (< n2 n1)
      n1
      n2))


; Questão 5

;funções de referência

(define (merge l1 l2)
  (cond
    [(null? l1) l2]
    [(null? l2) l1]
    [(< (first l2) (first l1)) 
          (cons (first l2) (merge l1 (rest l2)))]
    [else (cons (first l1) (merge (rest l1) l2))]))

(define (divide l [l1 '()] [l2 '()])
  (cond
    [(null? l) (cons l1 l2)]
    [else 
     (divide (rest l)
             l2
             (cons (first l) l1))]))

(define (merge-sort l)
  (cond
   [(null? l) l]
   [(null? (rest l)) l]
   [else (let ([l_dividida (divide l)])
         (merge
            (merge-sort (first l_dividida))
            (merge-sort (rest l_dividida))))]))

; solução

(define (merge2 l1 l2 #:func [f <] #:chave [c identity])
  (cond
    [(null? l1) l2]
    [(null? l2) l1]
    [(f (c (first l2)) (c (first l1))) 
             (cons (first l2) (merge2 l1 (rest l2) #:func f #:chave c))]
    [else (cons (first l1) (merge2 (rest l1) l2 #:func f #:chave c))]
    ))

(define (merge-sort2 l #:f [f <] #:k [c identity])
  (cond
    [(null? l) l]
    [(null? (rest l)) l]
    [else (let ([l_dividida (divide l)])
            (merge2 (merge-sort2 (first l_dividida) #:f f #:k c) 
                    (merge-sort2 (rest l_dividida) #:f f #:k c)
                    #:func f #:chave c))]
    ))

(define (contador elemento l)
  (cond
    [(null? l) 0]
    [(equal? elemento (car l)) (+ 1 (contador elemento (cdr l)))]
    [(not (equal? elemento (car l))) (+ 0 (contador elemento (cdr l)))]
    )
  )

(define (maximol l)
  (cond
    [(null? l) 0]
    [else (max (maximol(cdr l)) (car l))]
    )
  )


(define (concatenar1 l1 l2)
	(cond
	  ((null? l1) l2)
	  (else (cons (first l1) (concatenar1 (rest l1) l2)))))


;; outra versão

(define (concatenarInv2 l1 l2) (concatenar1 l2 l1))

;; concatena lista de listas
(define (concatenarLL ll (la '()))
	(cond
	  [(null? ll) la]
	  [else (concatenar1 (first ll) 
                             (concatenarLL (rest ll) la))]
          )
  )


(define (juntar1 l1 l2)
  (cond
    ((null? l2) l1)
    ((null? l1) l2)
    (else (cons (first l1) (cons (first l2) (juntar (rest l1) (rest l2)))))))

;; Outra versão de juntar, mais elegante
(define (juntar2 l1 l2)
  (cond
    [(null? l2) l1]
    [else (cons (first l1) (juntar2 l2 (rest l1)))]
    )
  )


(define (adicionarFinal1 a l)
  (cond
     ((null? l) (cons a l))
     (else (cons (first l) (adicionarFinal a (rest l))))))

(define (inverter1 l (la '()))
  (cond
     ((null? l) la)
     (else (inverter (rest l) (cons (first l) la))))) 

(define (parear1 e l)
  (cond
     ((null? l) l)
     (else (cons (list e (first l)) (parear e (rest l))))))

(define (pares1 l)
   (cond
      ((null? l) l)
      ((null? (rest l)) '())
      (else (concatenar1 (parear (first l) (rest l)) (pares (rest l))))))

;;; permute é difícil, aqui seguem três versões

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funçao insereTodasPosicoes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recebe um elemento "e" uma lista "l" e retorna uma
; lista de listas onde "e" está inserido em todas as
; posições de l
;
; Ex. (insereTodasPosicoes 'a '(1 2 3))
; retorna    ((A 1 2 3) (1 A 2 3) (1 2 A 3) (1 2 3 A))
;;;';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (insereTodasPosicoes e l)
  (cond
     ((null? l) (list (list e)))
     (else (cons (cons e l) 
                 (map (lambda (x) (cons (first l) x)) 
                                 (insereTodasPosicoes e (rest l)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funçao inserirTodos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Recebe um elemento "e" e uma lista de listas ll
; retorna uma lista de listas onde "e" está
; inserido em cada posicao de cada
; lista de ll
;
; Ex. (inserirTodos 'a '((1 2) (2 1)))
; retorna ((A 1 2) (1 A 2) (1 2 A) (A 2 1) (2 A 1) (2 1 A))
;
;;;';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (inserirTodos e la)
  (cond
     ((null? la) '())
     (else (concatenar1 (insereTodasPosicoes e (first la)) 
                        (inserirTodos e (rest la))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funçao permuta
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Faz a permutação aplicando inserirTodos 
;
;;;';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (permutar l)
  (cond
     ((null? l) '(()))
     (else (inserirTodos (first l) (permutar (rest l))))))


;;; insere todas as posições(mesma função com nome menor)
(define (itp e l)
  (cond
    [(null? l) (list (list e))]
    [else
      (cons (cons e l)
            (map (lambda (x) (cons (first l) x)) 
                 (itp e (rest l))))])) 

(define (permutar2 l)
  (cond
    [(null? l) l]
    [(null? (rest l)) (list l)]
    [else (concatenarLL 
              (map
                 (lambda (x) (itp (first l) x))
                 (permutar2 (rest l))))]
    )
  )

;;;; Este é mais "simples"
(define (permutar3 l)
  (cond
    [(null? l) '(())]
    [else (concatenarLL
             (map (lambda (x)
		(map (lambda (y) (cons x y))
			(permutar3 (remove x l))))
	    l))]
    )
  )

;;;;;;;;;;;;

(define (conjunto1 l)
   (cond
      ((null? l) #t)
      ((and (not (member (first l) (rest l))) (conjunto (rest l))) #t)
      (else #f)
      ))



(define (prefixo l1 l2)
  (cond
    ((null? l1) #t)
    ((null? l2) #f)
    ((not (equal? (first l1) (first l2))) #f)
    (else (prefixo (rest l1) (rest l2))))) 

(define (subsequencia l1 l2)
  (cond
    ((null? l1) #t)
    ((null? l2) #f)
    ((prefixo l1 l2) #t)
    (else (subsequencia l1 (rest l2)))
    )
  )  

(define (oculte elemento lista)
  (cond
    ((null? lista) lista)
    ((equal? elemento (car lista))(cons 'xxxx (oculte elemento (cdr lista))))
    (else (cons(car lista)(oculte elemento (cdr lista))))
    )
  )

(define (oculteConj elementos lista)
  (cond
    ((null? lista) lista)
    ((null? elementos) lista)
    (else (oculteConj (cdr elementos) (oculte (car elementos) lista)))
    )
  )

(define (intercala x y n)
  (cond
    ((eq? n 0) '())
    (else (cons x (intercala y x (- n 1))))
    )
  )

(define (conta elemento lista [contador 1])
  (cond
    ((null? lista) contador)
    ((equal? elemento (car lista)) (conta elemento (cdr lista) (+ contador 1)))
    (else (conta elemento (cdr lista) contador))
    )
  )

(define (sumarize lista)
  (cond
    ((null? lista) lista)
    (else (let ([l1 (conta (car lista) (cdr lista))])
            (cons (cons ( car lista) l1) (sumarize (remove* (car lista) (cdr lista))))
            )
          )
    )
  )

(define (proximo elemento lista)
  (if (null? lista)
      lista
      (if (equal? elemento (car lista))
          (cons (car lista) (proximo elemento (cdr lista)) )
          (proximo elemento (cdr lista))
      )
  )
  )
(define (empacote lista)
  (cond
    ((null? lista) lista)
    (else (let ([ll (proximo (car lista) lista)])
            (cons ll (empacote(remove*(car lista) (cdr lista))))
            )
     )
    )
  )
(define a1 '(10 (5 (4 () ()) (7 () ())) ()))
(define (subarvore e a)
  (cond
    ((null? a) a)
    ((eq? e (car a)) a)
    ((< e (car a)) (subarvore e (cadr a)))
    (else (subarvore e (caddr a)))
    )
  )
(define (alturas a)
  (cond
    [(null? a) '()]
    [else
     (let [(sae (alturas (first (rest a))))
           (sad (alturas (first (rest(rest a)))))]
       (let [(ae (obtem-alt sae))
             (ad (obtem-alt sad))]
         (cons (cons (first a)
                     (+ 1 (max ae ad)))
               (append sae sad))
         )
       )]
    )
  )
(define (obtem-alt lm)
  (if (null? lm)
      0
      (cdr(car lm))
      )
  )