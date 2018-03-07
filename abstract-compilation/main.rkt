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
             #:attr gen (λ (⟦_⟧) #`[#,(format-id #'x "⟦~a⟧" #'x) (#,⟦_⟧ x)]))
    (pattern (x:id (~literal ...))
             #:attr gen (λ (⟦_⟧) #`[#,(format-id #'x "⟦~a⟧" #'x) (map #,⟦_⟧ x)]))
    (pattern (x:id #:as y:id)
             #:attr gen (λ (⟦_⟧) #`[y (#,⟦_⟧ x)]))
    (pattern ((x:id (~literal ...)) #:as y:id)
             #:attr gen (λ (⟦_⟧) #`[y (map #,⟦_⟧ x)])))
  
  (define-syntax-class clause
    #:description "compilation clause"
    #:attributes (gen)
    (pattern [lhs:expr rhs:expr]
             #:attr gen (λ _ #`[lhs rhs]))
    (pattern [(~literal =>) p:expr execution:expr
              (~optional (~seq #:where [lhs rhs] ...)
                         #:defaults ([(lhs 1) null]
                                     [(rhs 1) null]))
              (~optional (~seq #:recur q:pat ...)
                         #:defaults ([(q 1) null]))]
             #:attr gen
             (λ (⟦_⟧ comps)
               (with-syntax ([(def-⟦q⟧ ...)
                              (map (syntax-parser [q:pat ((attribute q.gen) ⟦_⟧)])
                                   (syntax->list #'(q ...)))]
                             [(components ...) comps])
                 #'[p
                    (let (def-⟦q⟧ ...)
                      (match-let* ([lhs rhs] ...)
                        (λ (components ...) execution)))])))))

(define-syntax-parser define-compiler
  [(_ ((⟦_⟧:id e:id) components:id ...) clauses:clause ...)
   (with-syntax ([(compiled-clauses ...)
                  (map (syntax-parser [c:clause ((attribute c.gen) #'⟦_⟧ #'(components ...))])
                       (syntax->list #'(clauses ...)))])
     #'(define (⟦_⟧ e)
         (match e
           compiled-clauses ...)))])
