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

(define (init-scene scene-node space)

  ;; walls
  (cp:space-add-shapes space
		       (doto (fixed-line-segment space (cp:v (- wsz) (- wsz))
						 (cp:v (- wsz) wsz) radius: 0.4)
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
	   [body (cp:body-new (cp:moment-for-circle mass radius 0 cp:v0) mass)]
	   [shape (cp:circle-shape-new body radius cp:v0)])
      (set! (cp:body-position body) center-pos)
      (cp:space-add-body space body)
      (cp:space-add-shape space shape)
      (let* ([n (new-node scene-node
			  #:render *render-circle-node*
			  #:body body
			  #:shape shape
			  #:color (vector 1 0 0))])
	n)))

  (define count 6)

  (define transform-identity (cp:transform-new 1.0 0.0 0.0 1.0 0.0 0.0) )

  ;; balls for outer edge
  (define edges-outer
    (list->vector
     (list-ec (:list x (circle-ring count))
	      (let* ([start (first x)]
		     [end (second x)]
		     [start-pos (cp:v (car start) (cdr start))]
		     [end-pos (cp:v (car end) (cdr end))]
		     [center-pos (cp:vlerp start-pos end-pos 0.5)]
		     [mass 1.0]
		     [radius 0.1]
		     [v (cp:varray start-pos end-pos)]
		     [body (cp:body-new (cp:moment-for-poly mass 2 v cp:v0 radius) mass)]
		     [shape (cp:poly-shape-new body 2 v transform-identity radius)])

		(cp:shape-set-elasticity shape 0.95)
		`
		(set! (cp:body-position body) center-pos)
		(cp:space-add-body space body)
		(cp:space-add-shape space shape)



		;; (let* ([n (new-node scene-node
		;; 		    (lambda (node projection-matrix view-matrix ctx-matrix)
		;; 		      ((*render-segment*) projection-matrix view-matrix ctx-matrix shape))
		;; 		    body
		;; 		    shape)])
		;;   n)
		1
		))))

  (list "3" scene-node space)
  )
