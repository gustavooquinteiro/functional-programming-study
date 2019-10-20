#lang racket 

(struct dog (name breed age))
(define my-pet
  (dog "lassie" "collie" 5))

; returns whether the variable was constructed with the dog constructor
(dog? my-pet) ; => #t
; accesses the name field of the variable constructed with the dog constructor
(dog-name my-pet) ; => "lassie"

; You can explicitly declare a struct to be mutable with the #:mutable option
(struct rgba-color (red green blue alpha) #:mutable)
(define burgundy
   (rgba-color 144 0 32 1.0))
(set-rgba-color-green! burgundy 10)
(rgba-color-green burgundy) ; => 10
