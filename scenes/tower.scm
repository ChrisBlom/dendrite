(use (prefix chipmunk cp:)
     ringbuffer
     clojurian-syntax
     srfi-42
     gl-math)

(define wsz 5.)

(define the-damping 0.5)

(define (box-node parent-node space position
		  #!key
		  (width 0.6)
		  (height 0.6)
		  (elasticity 0.02)
		  (friction 0.6)
		  (mass 1.)
		  (radius 0.02))
  (let* ([body (cp:body-new mass (cp:moment-for-box width height radius))]
	 [shape (cp:box-shape-new body width height radius)])

    (set! (cp:shape-elasticity shape) elasticity)
    (set! (cp:shape-friction shape) friction)
    (set! (cp:body-position body) position)

    (cp:space-add-body space body)
    (cp:space-add-shape space shape)

    (new-node parent-node
	      #:render
	      (lambda (node projection-matrix view-matrix ctx-matrix)
		((*render-poly*) node projection-matrix view-matrix ctx-matrix))
	      #:render-init
	      (lambda (node)
		(let ([mesh (mesh-for-poly-shape shape)])
		  (mesh-make-vao! mesh `((position . ,(gl:get-attrib-location (cell-get program-poly) "position"))
					 (color . ,(gl:get-attrib-location (cell-get program-poly) "color"))))
		  (node-mesh-set! node mesh)))
	      #:body body
	      #:shape shape)))

(define (add-box position)
  (box-node interaction-node the-space position))

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

  (let ([n 10])
    (list-ec (:range i 0 n)
	     (let ([angle (* i (/ n) pi 2)])
	       (box-node scene-node space (cp:v (* 2 (sin angle))
						(* 2 (cos angle)))))))

  (list "tower" scene-node space))

(comment

; (cp:space-constraints space)
 )


(cp:varray (cp:v -1 1)
	   (cp:v 1 1)
	   (cp:v 1 -1)
	   (cp:v -1 -1))
