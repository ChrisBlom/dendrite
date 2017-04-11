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

  (define stiffness 50.)

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
					   stiffness
					   the-damping)]
			      [rot-constraint (cp:damped-rotary-spring-new
					       body-before
					       body-current
					       (cp:vtoangle (cp:v- (cp:body-position body-before)
								   (cp:body-position body-current)))
					       stiffness
					       the-damping
					       )])
			 (new-node scene-node
				   #:render
				   (lambda (node projection-matrix view-matrix ctx-matrix)
				     ((*render-constraint*) projection-matrix view-matrix ctx-matrix constraint)))
			 (new-node scene-node
				   #:render (lambda (node projection-matrix view-matrix ctx-matrix)
					      ((*render-constraint*) projection-matrix view-matrix ctx-matrix rot-constraint)))
			 (list constraint rot-constraint))))

  ;; inner shell
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
					stiffness
					the-damping)]
			   [rot-constraint (cp:damped-rotary-spring-new
					    body-before
					    body-current
					    (cp:vtoangle (cp:v- (cp:body-position body-before)
								(cp:body-position body-current)))
					    stiffness
					    the-damping
					    )])
		      (new-node scene-node #:render (lambda (node projection-matrix view-matrix ctx-matrix)
						      ((*render-constraint*) projection-matrix view-matrix ctx-matrix constraint)))
		      (new-node scene-node #:render (lambda (node projection-matrix view-matrix ctx-matrix)
						      ((*render-constraint*) projection-matrix view-matrix ctx-matrix rot-constraint)))
		      (list constraint rot-constraint))))

  (define (outer-for-inner i)
    (list (- (* 2 i) 1)
	  (* 2 i)))

  ;; outer -> inner
  (apply cp:space-add-constraints space
	 (append-ec (:range i 0 (vector-length edges-inner))
		    (:list outer (outer-for-inner i))
		    (let* ([current (vector-ref edges-inner i)]
			   [before (vector-ref edges-outer (% outer (vector-length edges-outer)))]
			   [body-current (node-body current)]
			   [body-before (node-body before)]
			   [constraint (cp:damped-spring-new
					body-before
					body-current
					cp:v0
					cp:v0
					(cp:vlength (cp:v- (cp:body-position body-before)
							   (cp:body-position body-current)))
					stiffness
					the-damping)]
			   [rot-constraint (cp:damped-rotary-spring-new
					    body-before
					    body-current
					    (cp:vtoangle (cp:v- (cp:body-position body-before)
								(cp:body-position body-current)))
					    stiffness
					    the-damping
					    )])
		      (list (new-node scene-node
				      #:render (lambda (node projection-matrix view-matrix ctx-matrix)
						 ((*render-constraint*) projection-matrix view-matrix ctx-matrix constraint)))
			    (new-node scene-node
				      #:render (lambda (node projection-matrix view-matrix ctx-matrix)
						 ((*render-constraint*) projection-matrix view-matrix ctx-matrix rot-constraint))))
		      (list constraint rot-constraint))))

  (define rcs
    (let* ([poss (map (lambda (x) (cp:body-position (node-body x)))
		      (vector->list edges-inner))]
	   [center-pos (cp:v* (reduce cp:v+ cp:v0 poss)
			      (/ 1 (length poss)))]
	   [body-center (cp:body-new 10. 20.)]
	   [radius 0.1]
	   [shape (cp:circle-shape-new body-center radius cp:v0)])

      (cp:body-set-position body-center center-pos)
      (cp:space-add-body space body-center)
      (cp:space-add-shape space shape)

      (let* ([body body-center])
	(new-node scene-node
		  #:render (lambda (node projection-matrix view-matrix ctx-matrix)
			     ((*render-circle-shape*) node projection-matrix view-matrix shape))
		  #:body  body
		  #:shape shape))

      (append-ec (:range i 0 (vector-length edges-inner))
		 (let* ([current (vector-ref edges-inner i)]
			[before (vector-ref edges-inner (% (+ i 1) (vector-length edges-inner)))]
			[body-current (node-body current)]
			[constraint (cp:damped-spring-new
				     body-center
				     body-current
				     (cp:vlerp cp:v0 (cp:body-position body-center) 0.1)
				     cp:v0
				     (cp:vlength (cp:v- (cp:body-position body-center)
							(cp:body-position body-current)))
				     stiffness
				     the-damping
				     )]
			[rot-constraint (cp:damped-rotary-spring-new
					 body-center
					 body-current
					 (cp:vtoangle (cp:v- (cp:body-position body-center)
							     (cp:body-position body-current)))
					 stiffness
					 the-damping)])

		   (new-node scene-node
			     #:render (lambda (node projection-matrix view-matrix ctx-matrix)
					((*render-constraint*) projection-matrix view-matrix ctx-matrix constraint)))
		   (new-node scene-node
			     #:render (lambda (node projection-matrix view-matrix ctx-matrix)
					((*render-constraint*) projection-matrix view-matrix ctx-matrix rot-constraint)))

		   (list constraint rot-constraint)))))

  (apply cp:space-add-constraints space rcs)

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

  (list "1" scene-node space))
