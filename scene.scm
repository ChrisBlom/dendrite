
;; (name node-scene space)
(define-once loaded-scene #f)

(define-once current-scene #f)

(define (unload-scene name scene-node space)
  ;; remove all nodes
  (remove-subtree root-node scene-node)
  (set! loaded-scene #f)
  name)

(define (unload)
  (when loaded-scene
    (apply unload-scene loaded-scene)))

(define (scene-reload)
  (load-scene current-scene)
  (remove-children interaction-node))

;; returns (name scene-node space)
(define (load-scene n)

  (load-relative (string-append "scenes/" n ".scm"))

  (-run-physics- #f)

  (unload)

  (let ([space (cp:space-new)]
	[scene-node (new-node root-node)])

    (cp:space-set-iterations space 20) ; 10 default, can be overwritten by scene
    (set! loaded-scene (init-scene scene-node space))
    (set! current-scene n)

    (set! the-space space)

    )

  (* 2 (/ 500 3))

  (-run-physics- #t)

  (first loaded-scene))


;; (comment

;;  (unload)

;;  (load-scene "1")

;;  (display "1")

;;  )





(define (add-spring from to #!key
		    (damping 0.5 )
		    (stiffness 10.))
  (let [(spring (cp:damped-spring-new (node-body from)
				      (node-body to)
				      cp:v0
				      cp:v0
				      (* 0.8 (cp:vlength (cp:v- (cp:body-position (node-body from))
								(cp:body-position (node-body to)))))
				      stiffness
				      damping))]
    (cp:space-add-constraint the-space spring)
    (new-node interaction-node
	      #:render
	      (lambda (node projection-matrix view-matrix ctx-matrix)
		((*render-constraint*) projection-matrix view-matrix ctx-matrix spring)))))

(define (add-rotary-spring from to #!key
		    (damping 0.8 )
		    (stiffness 20.))
  (let [(spring (cp:damped-rotary-spring-new
		 (node-body from)
		 (node-body to)
		 (cp:vtoangle (cp:v- (cp:body-position (node-body from))
				     (cp:body-position (node-body to))))
		 stiffness
		 damping))]
    (cp:space-add-constraint the-space spring)
    (new-node interaction-node
	      #:render
	      (lambda (node projection-matrix view-matrix ctx-matrix)
		((*render-constraint*) projection-matrix view-matrix ctx-matrix spring)))))

(define (add-slide from to #!key
		   (stiffness 10.))
  (let* [(s (cp:vlength (cp:v- (cp:body-position (node-body from))
			       (cp:body-position (node-body to)))))
	(spring (cp:slide-joint-new (node-body from)
				    (node-body to)
				    cp:v0
				    cp:v0
				    (* 0.8 s)
				    (* 1.0 s)))]
    (cp:space-add-constraint the-space spring)
    (new-node interaction-node
	      #:render
	      (lambda (node projection-matrix view-matrix ctx-matrix)
		((*render-constraint*) projection-matrix view-matrix ctx-matrix spring)))))

(define (add-line from to)
  (let* ([a (cp:body-position (node-body from))]
	 [b (cp:body-position (node-body to))]
	 [mass 2.0]
	 [radius 0.05]
	 [v (list a
		  (cp:v+ a (cp:v 0.1 0.1))
		  b
		  (cp:v+ b (cp:v 0.1 0.1)))]
	 [body (cp:body-new mass 0.1)]
	 [shape (cp:create-polygon-shape body v cp:transform-identity radius)]
	 [pin-a (cp:pin-joint-new (node-body from)
				  body
				  (cp:body-world-to-local (node-body from) a)
				  (cp:body-world-to-local body b))]
	 [pin-b (cp:pin-joint-new (node-body to)
				  body
				  (cp:body-world-to-local (node-body to) b)
				  (cp:body-world-to-local body a))])

    (set! (cp:pin-joint-dist pin-a) 0.2)
    (set! (cp:pin-joint-dist pin-b) 0.2)

    (cp:shape-set-filter shape (cp:shape-filter 1
						(list 3)
						(list 3)))

    (cp:space-add-body the-space body)
    (cp:space-add-shape the-space shape)
    (cp:space-add-constraint the-space pin-a)
    (cp:space-add-constraint the-space pin-b)

    ;; (set! (cp:pin-joint-dist pin-a) 0.2)
    ;; (set! (cp:pin-joint-dist pin-b) 0.2)


    (new-node interaction-node
	      #:body body
	      #:shape shape
	      #:render
	      (lambda (node projection-matrix view-matrix ctx-matrix)
		((*render-poly*) node projection-matrix view-matrix ctx-matrix))
	      #:render-init
	      (lambda (node)
		(let ([mesh (poly-mesh v)])
		  (mesh-make-vao! mesh
				  (list (cons 'position (gl:get-attrib-location (cell-get program-poly) "position"))
					(cons 'color (gl:get-attrib-location (cell-get program-poly) "color"))))
		  (node-mesh-set! node mesh))))))

(define (add-ball parent-node space x y idx
		  #!key
		  (elasticity 0.95)
		  (friction 0.2)
		  (mass 1.)
		  (radius 0.1)
		  (velocity #f)
		  (color (vector 0 1 1)))
  (let* ((moment (cp:moment-for-circle mass 0. radius cp:v0))
	 (body (cp:body-new  mass moment))
	 (shape (cp:circle-shape-new body radius cp:v0)))

    (set! (cp:shape-collision-type shape) 1)
    (set! (cp:body-position body) (cp:v (exact->inexact x) (exact->inexact y)))

    (when velocity
      (set! (cp:body-velocity body) velocity))
    (cp:space-add-body space body)

    (set! (cp:shape-friction shape) friction)
    (set! (cp:shape-elasticity shape) elasticity)

    (cp:space-add-shape space shape)

    (new-node parent-node
	      #:render (render-ball idx)
	      #:id idx
	      #:body body
	      #:shape shape
	      #:color color)))



(comment

 (add-line (first (node-children interaction-node)) (second (node-children interaction-node)))

 )
