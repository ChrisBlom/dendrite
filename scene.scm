
(define (scene-1 space)

    ;; walls
    (cp:space-add-shapes space
			 (doto (fixed-line-segment space (cp:v -5. -5.) (cp:v -5. +5.) radius: 0.4)
			       (cp:shape-set-elasticity 0.95))
			 (doto (fixed-line-segment space (cp:v -5. +5.) (cp:v +5. +5.) radius: 0.4)
			       (cp:shape-set-elasticity 0.95))
			 (doto (fixed-line-segment space (cp:v +5. +5.) (cp:v +5. -5.) radius: 0.4)
			       (cp:shape-set-elasticity 0.95))
			 (doto (fixed-line-segment space (cp:v +5. -5.) (cp:v -5. -5.) radius: 0.4)
			       (cp:shape-set-elasticity 0.95)))






  )
