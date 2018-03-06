[![Build Status](https://travis-ci.org/philnguyen/abstract-compilation.svg?branch=master)](https://travis-ci.org/philnguyen/abstract-compilation) abstract-compilation
=========================================

Simple DSL for defining abstract compilers,
making the task not much more verbose than defining interpreters
by getting rid of some boilerplates.

### Install

```
raco pkg install abstract-compilation
```

### Example

```racket
#lang typed/racket/base
(require abstract-compilation)
(define-type ρ (Immutable-HashTable Symbol Integer))
(define-type Comp (ρ → Integer))

(: ⟦_⟧ : Any → Comp)
(define-compiler ⟦_⟧
  ;; e ::= x | ℤ | (+ e ...) | (- e ...) | (let ([x e]) e)
  [(? symbol? x) (intern x)]
  [(? exact-integer? n) (intern n)]
  [=> (`(+ ,es ...) ρ)
      (for/sum ([f ⟦e⟧s]) (f ρ))
      #:recur [(es ...) #:as ⟦e⟧s]]
  [=> (`(- ,e) ρ)
      (- (⟦e⟧ ρ))
      #:recur e]
  [=> (`(- ,e₁ ,es ...) ρ)
      (- (⟦e₁⟧ ρ) (for/sum : Integer ([f ⟦es⟧]) (f ρ)))
      #:recur e₁ (es ...)]
  [=> (`(let ([,(? symbol? x) ,eₓ]) ,e) ρ)
      (⟦e⟧ (hash-set ρ x (⟦eₓ⟧ ρ)))
      #:recur eₓ e])

(define intern : ((U Integer Symbol) → Comp)
  (let ([m : (Mutable-HashTable (U Integer Symbol) Comp) (make-hasheq)])
    (λ (x)
      (define (mk) : Comp
        (cond [(integer? x) (λ _ x)]
              [else (λ (ρ) (hash-ref ρ x (λ () (error x "unbound"))))]))
      (hash-ref! m x mk))))
      
(define prog '(let ([x (+ 1 2)])
                (let ([y (- 1 2 3)])
                  (+ x y))))
(define ⟦prog⟧ (⟦_⟧ prog))
(⟦prog⟧ (hasheq)) ; ==> -1
```
