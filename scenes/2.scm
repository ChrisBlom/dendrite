(use (prefix chipmunk cp:)
     ringbuffer
     clojurian-syntax
     srfi-42
     gl-math)

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

(comment (load-scene "1"))

(define outer-count 32)

(define inner-count 16)

(define (init-scene scene-node space)

  ;; walls
  (cp:space-add-shapes space
		       (doto (fixed-line-segment space (cp:v (- wsz) (- wsz)) (cp:v (- wsz) wsz) radius: 0.4)
			     (cp:shape-set-elasticity 0.95)
			     (cp:shape-set-friction 0.1))
		       (doto (fixed-line-segment space (cp:v (- wsz) wsz) (cp:v wsz wsz) radius: 0.4)
			     (cp:shape-set-elasticity 0.95)
			     (cp:shape-set-friction 0.1))
		       (doto (fixed-line-segment space (cp:v wsz wsz) (cp:v wsz (- wsz)) radius: 0.4)
			     (cp:shape-set-elasticity 0.95)
			     (cp:shape-set-friction 0.1))
		       (doto (fixed-line-segment space (cp:v wsz (- wsz)) (cp:v (- wsz) (- wsz)) radius: 0.4)
			     (cp:shape-set-elasticity 0.95)
			     (cp:shape-set-friction 0.1)))

  (define edges-outer
    (list->vector
     (list-ec (:list x (circle-ring outer-count))
	      (let* ([start (first x)]
		     [end (second x)]
		     [start-pos (cp:v (car start) (cdr start))]
		     [end-pos (cp:v (car end) (cdr end))]
		     [center-pos (cp:vlerp start-pos end-pos 0.5)]
		     [mass 0.3]
		     [radius 0.2]
		     [body-center (cp:body-new (cp:moment-for-segment mass start-pos end-pos radius) mass)]
		     [shape (cp:circle-shape-new body-center radius cp:v0)])

		(set! (cp:body-position body-center) center-pos)
		(cp:space-add-body space body-center)
		(cp:space-add-shape space shape)
		(let* ([n (new-node  scene-node
				     #:render (lambda (node projection-matrix view-matrix ctx-matrix)
						((*render-circle-shape*) node projection-matrix view-matrix shape))
				     #:body body-center
				     #:shape shape
				     #:color (vector 1 0 0))])
		  n)))))

  (define edges-inner
    (list->vector
     (list-ec (:list x (circle-ring inner-count))
	      (let* ([start (first x)]
		     [end (second x)]
		     [start-pos (cp:v (car start) (cdr start))]
		     [end-pos (cp:v (car end) (cdr end))]
		     [center-pos (cp:vlerp start-pos end-pos 0.5)]
		     [mass 0.3]
		     [radius 0.2]
		     [body-center (cp:body-new (cp:moment-for-segment mass start-pos end-pos radius) mass)]
		     [shape (cp:circle-shape-new body-center radius cp:v0)])

		(cp:body-set-position body-center (cp:vmult center-pos 0.5))
		(cp:space-add-body space body-center)
		(cp:space-add-shape space shape)
		(let* ([edge-node (new-node scene-node
					    #:render (lambda (node projection-matrix view-matrix ctx-matrix)
						       ((*render-circle-shape*) node projection-matrix view-matrix shape))
					    #:body body-center
					    #:shape shape
					    #:color (vector 1 1 1))])

		  `((body-center . ,body-center)
		    (shape . ,shape))

		  edge-node)))))

  ;; outer shell
  (comment
   (for-each (cut cp:space-add-constraint space <>)
	     (append-ec (:list offset '(1 3 7))
			(:range i 0 (vector-length edges-outer))
			(let* ([current (vector-ref edges-outer i)]
			       [before (vector-ref edges-outer (% (+ i offset) (vector-length edges-outer)))]
			       [body-current (node-body current)]
			       [body-before (node-body before)]
			       [constraint (cp:damped-spring-new
					    body-before
					    body-current
					    cp:v0
					    cp:v0
					    (cp:vlength (cp:v- (cp:body-position body-before)
							       (cp:body-position body-current)))
					    100.
					    the-damping)]
			       [rot-constraint (cp:damped-rotary-spring-new
						body-before
						body-current
						(cp:vtoangle (cp:v- (cp:body-position body-before)
								    (cp:body-position body-current)))
						100.
						the-damping
						)])
			  (new-node scene-node
				    #:render
				    (lambda (node projection-matrix view-matrix ctx-matrix)
				      ((*render-constraint*) projection-matrix view-matrix ctx-matrix constraint)))
			  (new-node scene-node
				    #:render (lambda (node projection-matrix view-matrix ctx-matrix)
					       ((*render-constraint*) projection-matrix view-matrix ctx-matrix rot-constraint)))
			  (list constraint rot-constraint)))))

  ;; inner shell
  (comment
   (apply cp:space-add-constraints space
	  (append-ec (:list offset '(1 2 5))
		     (:range i 0 (vector-length edges-inner))
		     (let* ([current (vector-ref edges-inner i)]
			    [before (vector-ref edges-inner (% (+ i offset) (vector-length edges-inner)))]
			    [body-current (node-body current)]
			    [body-before (node-body before)]
			    [constraint (cp:damped-spring-new
					 body-before
					 body-current
					 cp:v0
					 cp:v0
					 (cp:vlength (cp:v- (cp:body-position body-before)
							    (cp:body-position body-current)))
					 100.
					 the-damping)]
			    [rot-constraint (cp:damped-rotary-spring-new
					     body-before
					     body-current
					     (cp:vtoangle (cp:v- (cp:body-position body-before)
								 (cp:body-position body-current)))
					     100.
					     the-damping
					     )])
		       (new-node scene-node #:render (lambda (node projection-matrix view-matrix ctx-matrix)
						       ((*render-constraint*) projection-matrix view-matrix ctx-matrix constraint)))
		       (new-node scene-node #:render (lambda (node projection-matrix view-matrix ctx-matrix)
						       ((*render-constraint*) projection-matrix view-matrix ctx-matrix rot-constraint)))
		       (list constraint rot-constraint)))))


  (list "2" scene-node space))
