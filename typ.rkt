#lang racket
(require "mk.rkt")

#|Hindley-Milner type system|#

#|
⊢ is called the turnstile, and typed in as \vdash
Γ ⊢ e : τ means from Γ, we know e has type τ,
|#

(defrel (lookupᵒ Γ x τ)
  (fresh (aa da d)
    (≡ `((,aa . ,da) . ,d) Γ)
    (condᵉ
     [(≡ aa x) (≡ da τ)]
     [(≠ aa x) (lookupᵒ d x τ)])))

#;
(run 2 τ
  (lookupᵒ '((y . Bool) (x . Nat) (z . Nat) (y . Nat))
           'y
           τ))

(defrel (⊢ Γ e τ)
  (condᵉ
    #|
    when e is a symbol and lookup Γ e is τ
    ------------------------   
    Γ ⊢ e : τ
    |#
   [(symbolo e) (lookupᵒ Γ e τ)]
    #|
    Γ,x:τₓ ⊢ body : τᵣ
    ------------------------   
    Γ ⊢ (λ (x) body) : (→ τₓ τᵣ)
    |#
   [(fresh (x body)
      (≡ `(λ (,x) ,body) e)
      (fresh (τₓ τᵣ)
        (≡ `(→ ,τₓ ,τᵣ) τ)
        (⊢ `((,x . ,τₓ) . ,Γ) body τᵣ)))]
    #|
    Γ ⊢ rator : (→ τₓ τᵣ)
    Γ ⊢ rand : τₓ
    ------------------------   
    Γ ⊢ (rator rand) : τᵣ
    |#
   [(fresh (rator rand)
      (≡ `(,rator ,rand) e)
      (fresh (τₓ)
        (⊢ Γ rator `(→ ,τₓ ,τ))
        (⊢ Γ rand τₓ)))]
    #|
    Γ ⊢ a : Nat
    ------------------------   
    Γ ⊢ (sub1 a) : Nat
    |#
   [(fresh (a)
      (≡ `(sub1 ,a) e)
      (≡ 'Nat τ)
      (⊢ Γ a 'Nat))]
    #|
    Γ ⊢ a : Nat
    ------------------------   
    Γ ⊢ (zero? a) : Bool
    |#
   [(fresh (a)
      (≡ `(zero? ,a) e)
      (≡ τ 'Bool)
      (⊢ Γ a 'Nat))]
    #|
    Γ ⊢ a : Nat    Γ ⊢ b : Nat
    ------------------------   
    Γ ⊢ (* a b) : Nat
    |#
   [(fresh (a b)
      (≡ `(* ,a ,b) e)
      (≡ τ 'Nat)
      (⊢ Γ a 'Nat)
      (⊢ Γ b 'Nat))]
   #|
   when e is a number
   ------------------------   
   Γ ⊢ e : Nat
   |#
   [(numbero e) (≡ 'Nat τ)]))

(run 1000 (e τ)
  (⊢ '()
     e
     τ))
