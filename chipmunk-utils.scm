(import chicken scheme)

(use chipmunk
     srfi-4 ; vectors
)

(define (damped-spring body-a body-b #!key length (stiffness 40.) (damping 0.99) anchor-a anchor-b)
  (damped-spring-new
   body-a
   body-b
   (or anchor-a (cpv 0. 0.))
   (or anchor-b (cpv 0. 0.))
   (or length (cpv-dist
	       (body-get-pos body-a)
	       (body-get-pos body-b)))
   stiffness
   damping))

(define (damped-rotary-spring body-a body-b #!key angle (stiffness 40.) (damping 0.99))
  (damped-rotary-spring-new
   body-a
   body-b
   (or angle (vtoangle
	      (v-
	       (body-get-position body-b)
	       (body-get-position body-a))))
   stiffness
   damping))
