(use (prefix chipmunk cp:)
     ringbuffer
     clojurian-syntax
     srfi-42
     gl-math
     chipmunk-utils
     synth-utils
     scene)

(define wsz 5)

(define the-damping 0.5)

(define (init-scene scene-node space)

  (set! (cp:space-gravity space) (cp:v 0 -9.8))

  (define stiffness 50.)

  ;; walls
  (let ([wall-friction 1.0]
	[wall-elasticity 0.95]
	[right 20]
	[l -40]
	[r 40]
	[bottom -10]
	[top 60 ])
    (cp:space-add-shapes space
			 (doto (fixed-line-segment space (cp:v l bottom) (cp:v l top) radius: 0.4)
			       (cp:shape-set-elasticity wall-elasticity)
			       (cp:shape-set-friction wall-friction))
			 (doto (fixed-line-segment space (cp:v l top) (cp:v r top) radius: 0.4)
			       (cp:shape-set-elasticity wall-elasticity)
			       (cp:shape-set-friction wall-friction))
			 (doto (fixed-line-segment space (cp:v r top) (cp:v r bottom) radius: 0.4)
			       (cp:shape-set-elasticity wall-elasticity)
			       (cp:shape-set-friction wall-friction))
			 (doto (fixed-line-segment space (cp:v r bottom) (cp:v l bottom) radius: 0.4)
			       (cp:shape-set-elasticity wall-elasticity)
			       (cp:shape-set-friction wall-friction))


			 (doto (fixed-line-segment space
						   (cp:v -2 bottom)
						   (cp:v l (* 0.5 bottom))
						   radius: 0.4)
			       (cp:shape-set-elasticity wall-elasticity)
			       (cp:shape-set-friction wall-friction))))




  (for-each (lambda (j) (list-ec (:range i 0 40)
				 (box-node scene-node space
					   (cp:v (+ (* 0.25 (% i 2)) (* j 0.51))
						 (+ -9 (* i 0.49)))
					   #:mass 0.1)))
	    (range -6 6))

  ;; (poly-node scene-node space
  ;; 	     #:postion (cp:v 0 0)
  ;; 	     #:vertices
  ;; 	     (list (cp:v -3 7)
  ;; 		   (cp:v 3 7)
  ;; 		   (cp:v 0.1 9)
  ;; 		   (cp:v -0.1 9)))


  (list "tower" scene-node space))

(set-init init-scene)
