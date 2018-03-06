#lang racket/base

(provide define-compiler)

(require racket/match
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(begin-for-syntax
  (define-syntax-class pat
    #:description "pattern"
    #:attributes (gen)
    (pattern x:id
             #:attr gen
             (λ (⟦_⟧)
               #`(define #,(format-id #'x "⟦~a⟧" #'x) (#,⟦_⟧ x))))
    (pattern (x:id (~literal ...))
             #:attr gen
             (λ (⟦_⟧)
               #`(define #,(format-id #'x "⟦~a⟧" #'x) (map #,⟦_⟧ x)))))
  
  (define-syntax-class clause
    #:description "compilation clause"
    #:attributes (gen)
    (pattern [lhs:expr rhs:expr]
             #:attr gen (λ _ #'[lhs rhs]))
    (pattern [(~literal =>) (p:expr components:id ...) execution:expr
              (~optional (~seq #:where [lhs:id rhs ...] ...)
                         #:defaults ([(lhs 1) null]
                                     [(rhs 2) null]))
              (~optional (~seq #:recur q:pat ...)
                         #:defaults ([(q 1) null]))]
             #:attr gen
             (λ (⟦_⟧)
               (with-syntax ([(def-⟦q⟧ ...)
                              (map (syntax-parser [q:pat ((attribute q.gen) ⟦_⟧)])
                                   (syntax->list #'(q ...)))])
                 #'[p
                    (define lhs rhs ...) ...
                    def-⟦q⟧ ...
                    (λ (components ...) execution)])))))

(define-syntax-parser define-compiler
  [(_ ⟦_⟧:id clauses:clause ...)
   (with-syntax ([(compiled-clauses ...)
                  (map (syntax-parser [c:clause ((attribute c.gen) #'⟦_⟧)])
                       (syntax->list #'(clauses ...)))])
     #'(define ⟦_⟧
         (match-lambda
           compiled-clauses ...)))])
