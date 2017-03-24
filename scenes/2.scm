(use (prefix chipmunk cp:)
     ringbuffer
     clojurian-syntax)

(define wsz 5.)

(define (circle-ring n)
  (let ([b (list->ringbuffer
	    (list-ec (:range i 0 n)
		     (let ([angle (* i (/ n) pi 2)])
		       (cons (* 2 (sin angle))
			     (* 2 (cos angle))))))])
    (list-ec (:range j 0 n)
	     `(,(ringbuffer-get b j)
	       ,(ringbuffer-get b (+ 1 j))))))

(define the-damping 0.5)

(define (init-scene scene-node space)

  ;; walls
  (cp:space-add-shapes space
		       (doto (fixed-line-segment space (cp:v (- wsz) (- wsz)) (cp:v (- wsz) wsz) radius: 0.4)
			     (cp:shape-set-elasticity 0.95))
		       (doto (fixed-line-segment space (cp:v (- wsz) wsz) (cp:v wsz wsz) radius: 0.4)
			     (cp:shape-set-elasticity 0.95))
		       (doto (fixed-line-segment space (cp:v wsz wsz) (cp:v wsz (- wsz)) radius: 0.4)
			     (cp:shape-set-elasticity 0.95))
		       (doto (fixed-line-segment space (cp:v wsz (- wsz)) (cp:v (- wsz) (- wsz)) radius: 0.4)
			     (cp:shape-set-elasticity 0.95)))


  (define center-radius 0.3)

  (define center
    (let* ([center-pos (cp:v 0 0)]
	   [mass 0.3]
	   [radius center-radius]
	   [circle-body (cp:body-new (cp:moment-for-circle mass radius 0 cp:v0)
				     mass
				     )]
	   [circle-shape (cp:circle-shape-new circle-body radius cp:v0)])
      (set! (cp:body-position circle-body) center-pos)
      (cp:space-add-body space circle-body)
      (cp:space-add-shape space circle-shape)
      (let* ([n (new-node scene-node
			  (lambda (node projection-matrix view-matrix ctx-matrix)
			    ((*render-circle-shape*) projection-matrix view-matrix circle-shape))
			  circle-body
			  circle-shape)])
	n)))

  (define count 24)

  ;; balls for outer edge
  (define edges-outer
    (list->vector
     (list-ec (:list x (circle-ring count))
	      (let* ([start (first x)]
		     [end (second x)]
		     [start-pos (cp:v (car start) (cdr start))]
		     [end-pos (cp:v (car end) (cdr end))]
		     [center-pos (cp:vlerp start-pos end-pos 0.5)]
		     [mass (/ 1.0 count)]
		     [radius 0.2]
		     [circle-body (cp:body-new (cp:moment-for-circle mass radius 0 cp:v0) mass)]
		     [circle-shape (cp:circle-shape-new circle-body radius cp:v0)])
		(set! (cp:body-position circle-body) center-pos)
		(cp:space-add-body space circle-body)
		(cp:space-add-shape space circle-shape)
		(let* ([n (new-node scene-node
				    (lambda (node projection-matrix view-matrix ctx-matrix)
				      ((*render-circle-shape*) projection-matrix view-matrix circle-shape))
				    circle-body
				    circle-shape)])
		  n)))))

  (define outer-joints
    (append-ec (:range i 0 (vector-length edges-outer))
	       (let* ([current (vector-ref edges-outer i)]
		      [before (vector-ref edges-outer (% (+ i 1) (vector-length edges-outer)))]
		      [body-current (node-body current)]
		      [body-before (node-body before)]
		      [a (cp:body-position body-before)]
		      [b (cp:body-position body-current)]
		      [mass 6.0]
		      [c (cp:slide-joint-new body-current body-before (cp:v 0.1 0.1)(cp:v 0.1 0.1)
					     0
					     1.1)
			 ]

		      [c-c (cp:slide-joint-new (node-body center) body-current (cp:v* b center-radius) cp:v0
					       0
					       0.9 ; radius * squish
					       )]

		      [cr (cp:damped-spring-new body-current body-before cp:v0 cp:v0
						(cp:vlength (cp:v- (cp:body-position body-current)
								   (cp:body-position body-before)))
					      1
					      1)]

		      [cr2 (cp:damped-spring-new (node-body center) body-current  (cp:v* b center-radius) cp:v0
						 (* 1.1 (cp:vlength (cp:v- (cp:body-position body-current)
									   (cp:body-position body-before))))
						 1.1
						 1)]
		      )
		 (space-add-constraints space cr c-c c)


		 (list (new-node scene-node
				 (lambda (node projection-matrix view-matrix ctx-matrix)
				   ((*render-constraint*) projection-matrix view-matrix ctx-matrix c))
				 )
		       (new-node scene-node
				 (lambda (node projection-matrix view-matrix ctx-matrix)
				   ((*render-constraint*) projection-matrix view-matrix ctx-matrix c-c))
)))))

  ;; filling
  (let ([n 100])
    (list-ec (:range i n)
	     (let ([angle (/ (* pi 2 i 8) n)]
		   [radius (* 2 (/ i n))])
	       (add-ball scene-node space
			 (* radius (sin angle))
			 (* radius (cos angle))
			 i
			 #:radius 0.1))))

  (list "2" scene-node space)
  )
