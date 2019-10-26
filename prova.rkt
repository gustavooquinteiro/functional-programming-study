#lang racket

(define-syntax (pipe-v1 stx)
  (syntax-case stx ()
               [(_ qualquer) #'qualquer]))

(define-syntax (pipe-v2 stx)
  (syntax-case stx ()
               [(_ qualquer) #'qualquer]
  [(_ id (exp args ...)) #'(exp id args ...)]
  ))

(define-syntax (pipe-v3 stx)
  (syntax-case stx ()
    [(_ qualquer) #'qualquer]
    [(_ id (exp args ...)) #'(exp id args ...)]
    [(_ id (exp args ...) ... (expn argn ...)) #'(expn (pipe-v3 id (exp args ...) ...) argn ...)]
  ))

(define-syntax (pipe-v4 stx)
  (syntax-case stx ()
    [(_ qualquer) #'qualquer]
    [(_ id (exp args ...)) #'(exp id args ...)]
    [(_ id exp) #'(exp id)]
    [(_ id (exp args ...) ... (expn argn ...)) #'(expn (pipe-v4 id (exp args ...) ...) argn ...)]
    [(_ id (exp args ...) ... expn) #'(expn (pipe-v4 id (exp args ...) ...))]
    [(_ id exp ... (expn args ...)) #'(expn (pipe-v4 id exp ...) args ...)]
    [(_ id exp ... expn) #'(expn (pipe-v4 id exp ...))]
  ))


;(define-syntax (pipe-v5 stx)
;  (syntax-case stx ()
;    [(_ qualquer) #'qualquer]
;    [(_ id (exp args ... underscore))
;     (eq? syntax->datum #'underscore '_) #'(exp args ... id)]
;  ))


 (require (for-syntax racket/match))
(define-syntax (pipe-v5 stx)
  (match (syntax->datum stx)
    [(list name id) (datum->syntax stx `,id)]
    [(list name id exp args) (datum->syntax stx `((,exp ,id ,args)))]
    [(list name id exp) (datum->syntax stx `((,exp ,id)))]
    [(list name id exp args ... expn argn ...) (datum->syntax stx `((,expn (pipe-v5 ,id (,exp ,args ...) ...) ,argn ...)))]
    [(list name id exp args ... expn) (datum->syntax stx `((,expn (pipe-v5 ,id (,exp ,args ...) ...))))]
    [(list name id exp ... expn args ...) (datum->syntax stx `((,expn (pipe-v5 ,id ,exp ...) ,args ...)))]
    [(list name id exp ... expn) (datum->syntax stx `((,expn (pipe-v5 ,id ,exp ...))))]
    [(list name id exp args "_" ) (datum->syntax stx `((,exp args id)))]
    ))
