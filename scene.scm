
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
    ;; TODO free old space
    (set! the-space space)
    (init-node scene-node))

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
				    (* 1.2 s)))]
    (cp:space-add-constraint the-space spring)
    (new-node interaction-node
	      #:render
	      (lambda (node projection-matrix view-matrix ctx-matrix)
		((*render-constraint*) projection-matrix view-matrix ctx-matrix spring)))))
