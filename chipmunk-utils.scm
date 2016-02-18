(import chicken scheme)

(use (prefix chipmunk cp-)
     srfi-4) ; vectors


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

(define (constraint-type x)
  (cond
   [(eq? #\x1 (cp-constraint-is-damped-rotary-spring  x)) 'damped-rotary-spring]
   [(eq? #\x1 (cp-constraint-is-damped-spring         x)) 'damped-spring]
   [(eq? #\x1 (cp-constraint-is-gear-joint            x)) 'gear-joint]
   [(eq? #\x1 (cp-constraint-is-groove-joint          x)) 'groove-joint]
   [(eq? #\x1 (cp-constraint-is-pin-joint             x)) 'pin-joint]
   [(eq? #\x1 (cp-constraint-is-pivot-joint           x)) 'pivot-joint]
   [(eq? #\x1 (cp-constraint-is-ratchet-joint         x)) 'ratchet-joint]
   [(eq? #\x1 (cp-constraint-is-rotary-limit-joint    x)) 'rotary-limit-joint]
   [(eq? #\x1 (cp-constraint-is-simple-motor          x)) 'simple-motor]
   [(eq? #\x1 (cp-constraint-is-slide-joint           x)) 'slide-joint]))

(define (damped-spring body-a body-b #!key length (stiffness 40.) (damping 0.99) anchor-a anchor-b)
  (let ([c  (cp:damped-spring-new
	     (./trace body-a)
	     (./trace body-b)
	     (./trace (or anchor-a cp:v0))
	     (./trace (or anchor-b cp:v0))
	     (./trace (or length (cp:vdist
				  (cp:body-get-position body-a)
				  (cp:body-get-position body-b))) )
	     (./trace stiffness)
	     (./trace damping))])
    (./trace c)
    ))
