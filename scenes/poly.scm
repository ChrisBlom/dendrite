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

(define dbg #f)

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
  ;;  -1 1       1 1
  ;;
  ;;
  ;;  -1 -1       1 -1
  (let* ([mass 200.0]
	 [radius 0.05]
	 [v (list (cp:v -4  0.25)
		  (cp:v  4  0.25)
		  (cp:v  4 -0.25)
		  (cp:v -4 -0.25))]
	 [body (cp:body-new mass (cp:moment-for-polygon mass v cp:v0 radius))]
	 [shape (cp:create-polygon-shape body v cp:transform-identity radius)])

    (set! (cp:shape-elasticity shape) 0.95)
    (set! (cp:shape-friction shape) 0.2)

    (cp:space-add-body space body)
    (cp:space-add-shape space shape)



    (set! dbg (new-node scene-node
			#:render
			(lambda (node projection-matrix view-matrix ctx-matrix)
			  ((*render-poly*) node projection-matrix view-matrix ctx-matrix))
			#:render-init
			(lambda (node)
			  (let ([mesh (poly-mesh v)])

			    (mesh-make-vao! mesh
					    `((position . ,(gl:get-attrib-location (cell-get program-line) "position"))
					      (color . ,(gl:get-attrib-location (cell-get program-line) "color"))))
			    (node-mesh-set! node mesh)))

			#:body body
			#:shape shape)))


  (list "poly" scene-node space))

(comment

; (cp:space-constraints space)
 )


(cp:varray (cp:v -1 1)
	   (cp:v 1 1)
	   (cp:v 1 -1)
	   (cp:v -1 -1))
