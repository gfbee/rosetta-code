#lang racket

(module Soegaard₀ racket
  
  (provide merge-sort)
  
  ; Using recent `define/match`.
  (define/match (merge-sort xs)
    [((or (list) (list _))) xs]
    [(_) (define-values (ys zs) (split-at xs (quotient (length xs) 2)))
         (merge (merge-sort ys) (merge-sort zs))])
  
  ; Style more consistent with `merge-sort` and internally.
  (define/match (merge xs ys)
    [((list) _) ys]
    [(_ (list)) xs]
    [((list* x₀ xs-rest) (list* y₀ ys-rest)) (if (<= x₀ y₀)
                                                 (list* x₀ (merge xs-rest ys))
                                                 (list* y₀ (merge xs ys-rest)))]))


(module Soegaard₁ racket
  
  (provide merge-sort)
  
  ; Result was list of one sorted list, return the sorted list instead.
  (define (merge-sort xs)
    (first (merge* (map list xs))))
  
  (define (merge* xss)
    (match xss
      [(or (list) (list _)) xss]
      [(list* xs ys zss) (merge* (list* (merge xs ys) (merge* zss)))]))
  
  (define (merge xs ys)
    (match* (xs ys)
      [((list) _) ys]
      [(_ (list)) xs]
      [((list* a as) (list* b bs))
       (if (<= a b)
           (list* a (merge as ys))
           (list* b (merge xs bs)))])))


(require (rename-in 'Soegaard₀ [merge-sort merge-sort₀])
         (rename-in 'Soegaard₁ [merge-sort merge-sort₁]))

(define (racket-sort ℓ) (sort ℓ <))

(define ℓ (for/list ([_ (expt 10 6)]) (random (expt 10 6))))

(for ([sort (list racket-sort merge-sort₀ merge-sort₁)])
  (time (void (sort ℓ))))