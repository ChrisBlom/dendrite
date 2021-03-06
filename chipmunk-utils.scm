(module chipmunk-utils *
  (import chicken scheme)
  (use (prefix chipmunk cp:)
       srfi-4
       global
       synth-utils) ; vectors


  (define (damped-rotary-spring body-a body-b #!key angle (stiffness 40.) (damping 0.99))
    (cp:damped-rotary-spring-new
     body-a
     body-b
     (or angle (cp:vtoangle
		(cp:v-
		 (cp:body-get-position body-b)
		 (cp:body-get-position body-a))))
     stiffness
     damping))

  (define (make-slide-joint from to #!key (margin 0.2))
    (let* [(s (cp:vlength (cp:v- (cp:body-position from)
				 (cp:body-position to))))
	   (spring (cp:slide-joint-new from
				       to
				       cp:v0
				       cp:v0
				       (* (- 1 margin) s)
				       (* (+ 1 margin) s)))]
      spring))

  (define (constraint-type x)
    (cond
     [(eq? #\x1 (cp:constraint-is-damped-rotary-spring  x)) 'damped-rotary-spring]
     [(eq? #\x1 (cp:constraint-is-damped-spring         x)) 'damped-spring]
     [(eq? #\x1 (cp:constraint-is-gear-joint            x)) 'gear-joint]
     [(eq? #\x1 (cp:constraint-is-groove-joint          x)) 'groove-joint]
     [(eq? #\x1 (cp:constraint-is-pin-joint             x)) 'pin-joint]
     [(eq? #\x1 (cp:constraint-is-pivot-joint           x)) 'pivot-joint]
     [(eq? #\x1 (cp:constraint-is-ratchet-joint         x)) 'ratchet-joint]
     [(eq? #\x1 (cp:constraint-is-rotary-limit-joint    x)) 'rotary-limit-joint]
     [(eq? #\x1 (cp:constraint-is-simple-motor          x)) 'simple-motor]
     [(eq? #\x1 (cp:constraint-is-slide-joint           x)) 'slide-joint]))

  (define (damped-spring body-a body-b #!key length (stiffness 40.) (damping 0.99) anchor-a anchor-b)
    (let ([c  (cp:damped-spring-new
	       body-a
	       body-b
	       (or anchor-a cp:v0)
	       (or anchor-b cp:v0)
	       (or length (cp:vdist
			   (cp:body-get-position body-a)
			   (cp:body-get-position body-b)))
	       stiffness
	       damping)])
      c
      ))

  (define (damped-spring-update-stiffness spring f . args)
    (let ([new (apply f (cp:damped-spring-get-stiffness spring) args)])
      (cp:damped-spring-set-stiffness spring new)
      new))

  (define (damped-spring-update-damping spring f . args)
    (let ([new (apply f (cp:damped-spring-get-damping spring) args)])
      (cp:damped-spring-set-damping spring new)
      new))

  (define (fixed-line-segment space from to #!key (radius 0.1))
    (cp:segment-shape-new (cp:space-get-static-body space) from to radius))

  (define (v-rand n)
    (f64vector (rand n) (rand n)))

  ;; (define (update-stiffness delta)
  ;;   (map
  ;;    (lambda (c) (damped-spring-update-stiffness c + delta))
  ;;    constraints))

  ;; (define (update-damping delta)
  ;;   (map
  ;;    (lambda (c) (damped-spring-update-damping c + delta))
  ;;    (cp:space-constraints the-space)))

  (define (clear-space space)
    (for-each (cut cp:space-remove-body the-space <>) (cp:space-bodies the-space)))
  )
