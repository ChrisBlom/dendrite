(import chicken scheme)

(use (prefix glfw3 glfw:)
     (prefix opengl-glew gl:)
     (prefix chipmunk cp:)
     gl-math
     gl-utils
     srfi-4 ; vectors
     srfi-18 ; threads
     srfi-42 ; eager comprehension
     box ; mutable box
     nrepl
     clojurian-syntax
     prepl
     utils
     symbol-utils
     posix
     ringbuffer
     matchable
     extras)

;;;;; Utils ;;;;;

(include "utils.scm")
(include "mesh.scm")
(include "chipmunk-utils.scm")
(include "reactive.scm")
(include "pipeline.scm")
(include "nodes.scm")

(when (unbound? 'REPL)
  (define repl-port 6061)
  (define REPL (make-prepl repl-port)))

;;;; Globals

(define the-space #f)
(define the-nodes (box (list)))
(define the-mouse-v (make-parameter (cp:v 0 0)))
(define the-mouse-ball (make-parameter #f))

(define root-node (new-node (lambda (x v p c) #f)
			   #f #f))

(comment
 ;; reset root node
 (node-children-set! root-node '()))

;;;; Rendering

(define (render-node node projection view ctx-matrix)
  (let ([render-fn (node-render-fn node)])
    ;; Render node
    (when render-fn
      (render-fn node
		 projection
		 (m* ctx-matrix view)
		 ctx-matrix))
    ;; render children with sub-ctx
    (let ([sub-ctx (m* (node-matrix node) ctx-matrix)])
      (for-each (lambda (child)
		  (render-node child projection view sub-ctx))
		(node-children node)))))

(define ((render-ball idx) node projection-matrix view-matrix ctx-matrix)
  (let* ([body  (node-body node)]
	 [angle (- (cp:body-angle body))]
	 [body-pos (cp:body-position body)]
	 [mvp (m* projection-matrix
		  (m* (translation (make-point (cp:v.x body-pos)
					       (cp:v.y body-pos)
					       0))
		      (m* view-matrix
			  (rotate-z angle (model-matrix)))))])
    (render-mesh circle-mesh
		 (cell-get program1)
		 mvp
		 (cp:vlength (cp:body-velocity body))
		 (vector
		  1
		  (+ 0.5 (* 0.5 (sin idx)))
		  (+ 0.5 (* 0.5 (cos idx)))))))

(comment

 (define the-space (cp:space-new))

 (define ff (cp:shape-filter-new (cp:uint 1) cp:all-categories cp:all-categories))

 (define out (allocate 1))

 (define p (cp:space-point-query-nearest the-space (cp:v 0. 0.1) 4 ff out))

 (filter
  (lambda (x) (eq? (node-shape x) p))
  (map print (map node-shape all-nodes)))

 (filter
  (o (cut equal? p <>) node-shape)
  (node-descendants root-node))

 (filter
  (cut equal? p <>)
  (cp:space-shapes the-space))

)

;;;; Scene Setup


(define *render-circle-shape* (make-parameter #f))

(*render-circle-shape* (lambda (projection-matrix view-matrix segment )
			 (let* ([body (cp:shape-get-body segment)]
				[angle (+ (/ pi 4) (- (cp:body-get-angle body)))]
				[body-pos (cp:body-get-position body)]
				[trans (make-point (cp:v.x body-pos)
						   (cp:v.y body-pos)
						   0)])
			   (render-mesh circle-mesh
					(cell-get program3)
					(m* projection-matrix
					    (m* (translation trans)
						(m* view-matrix
						    (rotate-z angle (model-matrix)))))

					(cp:vlength (cp:body-get-velocity body))

					(vector 1 1 0)))))

(define *render-constraint* (make-parameter #f))

(*render-constraint* (lambda (projection-matrix view-matrix ctx-matrix constraint)
		       (let* ([pos-a (cp:body-get-position (cp:constraint-get-body-a constraint))]
			      [pos-b (cp:body-get-position (cp:constraint-get-body-b constraint))]
			      [angle (cp:vtoangle (cp:v- pos-a pos-b))]
			      [middle (cp:vlerp pos-a pos-b 0.5)]
			      [trans (make-point (cp:v.x middle) (cp:v.y  middle)
						 0.)])

			 (mesh-update! the-line-mesh (line-mesh-vertices (cp:v.x pos-a) (cp:v.y pos-a)
									 (cp:v.x pos-b) (cp:v.y pos-b)))

			 (render-mesh the-line-mesh
				      (cell-get program-line)
				      (m* projection-matrix (m* view-matrix (model-matrix)))
				      (cp:constraint-get-impulse constraint)
				      (vector 1 0 1)))))

(define *render-link* (make-parameter #f))

(*render-link* (lambda (projection-matrix view-matrix ctx-matrix src trg)
		 (let* ([pos-a (cp:body-get-position src)]
			[pos-b (cp:body-get-position trg)]
			[angle (cp:vtoangle (cp:v- pos-a pos-b))]
			[middle (cp:vlerp pos-a pos-b 0.5)]
			[trans (make-point (cp:v.x middle) (cp:v.y  middle)
					   0.)])

		   (mesh-update! the-line-mesh (line-mesh-vertices (cp:v.x pos-a) (cp:v.y pos-a)
								   (cp:v.x pos-b) (cp:v.y pos-b)))

		   (render-mesh the-line-mesh
				(cell-get program-line)
				(m* projection-matrix (m* view-matrix (model-matrix)))
				(cp:constraint-get-impulse constraint)
				(vector 1 1 0)))))


(define (add-ball space x y idx #!key (elasticity 0.95) (friction 0.2) (mass 1.) (radius 0.1) (velocity #f))
  (let* ((moment (cp:moment-for-circle mass 0. radius cp:v0))
	 (body (cp:body-new  mass moment))
	 (shape (cp:circle-shape-new body radius cp:v0)))

    (set! (cp:body-position body) (cp:v (exact->inexact x) (exact->inexact y)))

    (when velocity
      (set! (cp:body-velocity body) velocity))
    (cp:space-add-body space body)

    (set! (cp:shape-friction shape) friction)
    (set! (cp:shape-elasticity shape) elasticity)

    (cp:space-add-shape space shape)

    (new-node (render-ball idx) body shape)))


(include "scene.scm")

;;;; Physics

(define (init-physics)
  (let* ([scene (load-scene "1")]
	 [node (second scene)]
	 [space (third scene)])
        (the-mouse-ball (add-ball space 10. 10. 0 #:radius 0.2 #:friction 0.01))
    (node-children-update! node append
			   (list (the-mouse-ball)))
    (set! the-space space)
    (set! root-node node)))

(init-physics)

;; add ball attached to cursor
(node-children-update! root-node append
		       (list (the-mouse-ball))
		       (let ([n 100])
			 (list-ec (:range i n)
				  (let ([angle (/ (* pi 2 i 8) n)]
					[radius (* 2 (/ i n))])
				    (add-ball the-space
					      (* radius (sin angle))
					      (* radius (cos angle))
					      i
					      #:radius 0.1)))))

(define (add-node-at-pos)
  (let ([m (cp:body-get-position (node-body (the-mouse-ball)))])
    (node-children-update! root-node append
			   (list (add-ball the-space (cp:v.x m) (cp:v.y m) (next-id)
					   #:radius 0.2
					   #:velocity (v-rand 10))))
    m))

;;;; Graphics

;;; Camera

(define camera-position (box (make-point 0. 0. 0.)))

(define camera-zoom (box 1))

;;(define projection-matrix (make-parameter (perspective 600 600 1 10 70)))
(define projection-matrix (make-parameter (ortho 10 10  0.1 100)))

(projection-matrix (ortho 10 10  0.1 100))

(define the-eye-point (make-parameter (make-point 0 0 5)))

(the-eye-point (make-point 0 0 5))

(define the-object-point (make-parameter (make-point 0 0 0)))

(the-object-point (make-point 0 0 0))

(define view-matrix
  (make-parameter (look-at (the-eye-point)
			   (the-object-point)
			   (make-point 0 1 0)))) ; up vector


(define (model-matrix)
  (mat4-identity))

;;;  Meshes

(define circle-mesh (disk 20))

(define rectangle-mesh rect)

(define (render-mesh mesh program mvp energy color)
  (gl:use-program program)
  (gl:bind-vertex-array 0)

  ;; set shader parameters
  (let ([id (gl:get-uniform-location program "ENERGY")])
    (when (> id -1)
      (gl:uniform1f id energy)))

  ;; set MVP matrix
  (gl:uniform-matrix4fv (gl:get-uniform-location program "MVP") 1 #f mvp)

  (gl:uniform3fv (gl:get-uniform-location program "colormod") 1
		 (list->f32vector (vector->list color)))
  ;; render mesh
  (let ([vao (mesh-vao mesh)])
    (when vao
      (gl:bind-vertex-array vao)
      (gl:draw-elements-base-vertex (mode->gl (mesh-mode mesh))
				    (mesh-n-indices mesh)
				    (type->gl (mesh-index-type mesh))
				    #f 0))))

;;; Window setup

(include "input.scm")

(define on-frame (make-parameter #f))

(define (main)
  (glfw:with-window
   (600 600 "Dendrite"
	resizable: #f
	context-version-major: 3
	context-version-minor: 2
	opengl-forward-compat: #t
	opengl-profile: glfw:+opengl-core-profile+)

   (gl:init)
   (gl:enable gl:+blend+)
   (gl:blend-func gl:+src-alpha+ gl:+one-minus-src-alpha+ )

   (start-all-cells)

   ;(gl:bind-texture gl:+texture-2d+ (cell-get noise-image-texture))
 ;  (gl:tex-parameteri gl:+texture-2d+ gl:+texture-min-filter+ gl:+linear+)

   ;; create vertex array object for mesh
   (mesh-make-vao! circle-mesh
		   `((position . ,(gl:get-attrib-location (cell-get program1) "position"))
		     (color . ,(gl:get-attrib-location (cell-get program1) "color"))))

   (mesh-make-vao! square-mesh
		   `((position . ,(gl:get-attrib-location (cell-get program1) "position"))
		     (color . ,(gl:get-attrib-location (cell-get program1) "color"))))

   (mesh-make-vao! rectangle-mesh
		   `((position . ,(gl:get-attrib-location (cell-get program1) "position"))
		     (color . ,(gl:get-attrib-location (cell-get program1) "color"))))

   (mesh-make-vao! the-line-mesh
		   `((position . ,(gl:get-attrib-location (cell-get program-line) "position"))
		     (color . ,(gl:get-attrib-location (cell-get program-line) "color")))
		   #:stream)

   ;; main loop
   (let loop ([i 0]
	      [pt (current-milliseconds)])
     (let* ([now (current-milliseconds)]
	    [dt (- now pt)]
	    [on-frame* (on-frame)])
       (when on-frame* (on-frame* dt)) ;; OPTIMIZE only unbox every x ms
       (unless (glfw:window-should-close (glfw:window))
	 (loop (+ 1 i)
	       now))))))

(on-frame (lambda (dt)

 	   ;; process repl event
	   (REPL)

	   ;; advance physics
	   (cp:space-step the-space 1/30)

	   ;; set mouse position
	   (set! (cp:body-position (node-body (the-mouse-ball))) (the-mouse-v))

	   ;; draw all nodes
	   (glfw:swap-buffers (glfw:window))
	   (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))

	   (render-node root-node
			(projection-matrix)
			(view-matrix)
			(m*s (mat4-identity) 2))

	   ;; after render
	   (check-error)
	   (glfw:poll-events) ; Because of the context version, initializing GLEW results in a harmless invalid enum

	   (thread-yield!)))

(define main-thread (thread-start! (make-thread main)))



(comment

 (set! (cp:space-iterations the-space) 10)

 (thread-terminate! main-thread)

 (update-gravity the-space cp:v+ (cp:v 0. 1.))

 (update-gravity the-space (constantly (cp:v 0 0)))

 (cp:space-get-gravity the-space)

 )

;; Local Variables:
;; eval: (eldoc-mode -1)
;; eval: (geiser-autodoc-mode -1)
;; eval: (setq company-minimum-prefix-length 4)
;; End:


(let ((old (current-input-port)))
     (current-input-port (make-parley-port old)))

(repl)
