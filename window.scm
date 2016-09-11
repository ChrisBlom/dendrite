(import chicken scheme)

(use (prefix glfw3 glfw:)
     (prefix opengl-glew gl:)
     (prefix gl opengl:)
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

(define the-counter (box 0))

(define (next-id)
  (box-swap! the-counter inc))

;;;;; Utils ;;;;;

(include "utils.scm")
(include "mesh.scm")
(include "chipmunk-utils.scm")
(include "pipeline.scm")
(include "nodes.scm")

(when (unbound? 'REPL)
  (define repl-port 6061)
  (define REPL (make-prepl repl-port)))

;;;; Globals

(define the-space #f)

;;;; Balls

(define (render-node node projection view ctx-matrix)
  (let ([render-fn (node-render-fn node)])
    (render-fn node
	       projection
	       (m* ctx-matrix view)
	       ctx-matrix)
    ;; render children with sub-ctx
    (let ([sub-ctx (m* (node-matrix node) ctx-matrix)])
      (for-each (lambda (child)
		  (render-node child projection view sub-ctx))
		(node-children node)))))

(define ((render-ball idx)  node projection-matrix view-matrix ctx-matrix)
  (let* ([body  (node-body node)]
	 [angle (- (cp:body-get-angle body))]
	 [body-pos (cp:body-get-position body)]
	 [mvp (m* projection-matrix
		  (m* (translation (make-point (cp:v.x body-pos)
					       (cp:v.y body-pos)
					       0))
		      (m* view-matrix
			  (rotate-z angle (model-matrix)))))])
    (render-mesh circle-mesh
		 (cell-get program1)
		 mvp
		 (cp:vlength (cp:body-get-velocity body))
		 (vector
		  1
		  (+ 0.5 (* 0.5 (sin idx)))
		  (+ 0.5 (* 0.5 (cos idx)))))))

(define (add-ball space x y idx #!key (elasticity 0.95) (friction 0.2) (mass 1.) (radius 0.1) (velocity #f))
  (let* ((moment (cp:moment-for-circle mass 0. radius cp:v0))
	 (body (cp:body-new  mass moment))
	 (shape (cp:circle-shape-new body radius cp:v0)))

    (cp:body-set-position body (cp:v (exact->inexact x) (exact->inexact y)))
    (when velocity
      (cp:body-set-velocity body velocity))
    (cp:space-add-body space body)

    (cp:shape-set-friction shape friction)
    (cp:shape-set-elasticity shape elasticity)
    (cp:space-add-shape space shape)

    (new-node (render-ball idx) body shape)))

;;;; Global State

(define the-nodes (box (list)))

(define the-mouse-v (make-parameter (cp:v 0 0)))

(define the-mouse-ball (make-parameter #f))

;; set root node
(define root-node (new-node (lambda (x v p c) #f)
			   #f #f))

(node-children-set! root-node '())

(comment

 (define ff (cp:shape-filter-new (cp:uint 1) cp:all-categories cp:all-categories))

 (define out (allocate 1))

 (define p (cp:space-point-query-nearest the-space (the-mouse-v) 4 ff out))

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

(define (circle-ring n)
  (let ([b (list->ringbuffer
	    (list-ec (:range i 0 n)
		     (let ([angle (* i (/ n) pi 2)])
		       (cons (* 2 (sin angle))
			     (* 2 (cos angle))))))])
    (list-ec (:range j 0 n)
	     `(,(ringbuffer-get b j)
	       ,(ringbuffer-get b (+ 1 j))))))


(include "scene.scm")

;;;; Physics

(define (init-physics)
  (let ([space (cp:space-new)])

    (cp:space-set-iterations space 30) ; 10 default

    (scene-1 space)

    (the-mouse-ball (add-ball space 10. 10. 0 #:radius 0.2 #:friction 0.01))

    (node-children-update! root-node append
			   (list (the-mouse-ball)))

    ;; return space
    space))

(set! the-space (init-physics))

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

;; inserts a new ball at the current cursor
(define (add-node-at-pos)
  (let ([m (cp:body-get-position (node-body (the-mouse-ball)))])
    (node-children-update! root-node append
			   (list (add-ball the-space (cp:v.x m) (cp:v.y m) (next-id)
					   #:radius 0.2
					   #:velocity (v-rand 10))))
    m))



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

(define edges
  (list->vector
   (list-ec (:list x (circle-ring 64))
	    (let* ([start (first x)]
		   [end (second x)]
		   [start-pos (cp:v (car start) (cdr start))]
		   [end-pos (cp:v (car end) (cdr end))]
		   [center-pos (cp:vlerp start-pos end-pos 0.5)]
		   [mass 0.3]
		   [radius 0.2]
		   [body-center (cp:body-new (cp:moment-for-segment mass start-pos end-pos radius) mass)]
		   [shape (cp:circle-shape-new body-center radius cp:v0)])

	      (cp:body-set-position body-center center-pos)
	      (cp:space-add-body the-space body-center)
	      (cp:space-add-shape the-space shape)
	      (let* ([n (new-node (lambda (node projection-matrix view-matrix ctx-matrix)
				     ((*render-circle-shape*) projection-matrix view-matrix shape))
				   body-center
				   shape)])
		(node-children-update! root-node (cut cons n <>))
		`((body-center . ,body-center)
		  (shape . ,shape))
		n
		)))))

(define edges-inner
  (list->vector
   (list-ec (:list x (circle-ring 32))
	    (let* ([start (first x)]
		   [end (second x)]
		   [start-pos (cp:v (car start) (cdr start))]
		   [end-pos (cp:v (car end) (cdr end))]
		   [center-pos (cp:vlerp start-pos end-pos 0.5)]
		   [mass 0.3]
		   [radius 0.15]
		   [body-center (cp:body-new (cp:moment-for-segment mass start-pos end-pos radius) mass)]
		   [shape (cp:circle-shape-new body-center radius cp:v0)])

	      (cp:body-set-position body-center (cp:vmult center-pos 0.5))
	      (cp:space-add-body the-space body-center)
	      (cp:space-add-shape the-space shape)
	      (let* ([edge-node (new-node (lambda (node projection-matrix view-matrix ctx-matrix)
					     ((*render-circle-shape*) projection-matrix view-matrix shape))
					   body-center
					   shape)])
		(node-children-update! root-node (cut cons edge-node <>))

		`((body-center . ,body-center)
		  (shape . ,shape))

		edge-node)))))

(define the-damping 0.4)

(comment

 (cp:constraint-get-type (first (cp:space-constraints the-space)))

 (update-stiffness 10)

 )

;; outer shell
(for-each (cut cp:space-add-constraint the-space <>)
	  (append-ec (:list offset '(1 2 3 7))
		     (:range i 0 (vector-length edges))
		   (let* ([current (vector-ref edges i)]
			  [before (vector-ref edges (% (+ i offset) (vector-length edges)))]
			  [body-current (node-body current)]
			  [body-before (node-body before)]
			  [constraint (add-constraint (cp:damped-spring-new
						       body-before
						       body-current
						       cp:v0
						       cp:v0
						       (cp:vlength (cp:v- (cp:body-get-position body-before)
									  (cp:body-get-position body-current)))
						       100.
						       the-damping))]
			  [rot-constraint (cp:damped-rotary-spring-new
					   body-before
					   body-current
					   (cp:vtoangle (cp:v- (cp:body-get-position body-before)
							       (cp:body-get-position body-current)))
					   100.
					   the-damping
					   )])
		     (node-children-update! root-node
		      (cut append <>
			   (list (new-node (lambda (node projection-matrix view-matrix ctx-matrix)
					     ((*render-constraint*) projection-matrix view-matrix ctx-matrix constraint)))
				 (new-node (lambda (node projection-matrix view-matrix ctx-matrix)
					     ((*render-constraint*) projection-matrix view-matrix ctx-matrix rot-constraint))))))
		     (list constraint rot-constraint))))

;; inner shell
(for-each (cut cp:space-add-constraint the-space <>)
	  (append-ec (:list offset '(1 2 5))
		     (:range i 0 (vector-length edges-inner))
		     (let* ([current (vector-ref edges-inner i)]
			    [before (vector-ref edges-inner (% (+ i offset) (vector-length edges-inner)))]
			  [body-current (node-body current)]
			  [body-before (node-body before)]
			  [constraint (add-constraint (cp:damped-spring-new
						       body-before
						       body-current
						       cp:v0
						       cp:v0
						       (cp:vlength (cp:v- (cp:body-get-position body-before)
									  (cp:body-get-position body-current)))
						       100.
						       the-damping))]
			  [rot-constraint (cp:damped-rotary-spring-new
					   body-before
					   body-current
					   (cp:vtoangle (cp:v- (cp:body-get-position body-before)
							       (cp:body-get-position body-current)))
					   100.
					   the-damping
					   )])
		     (node-children-update! root-node
		      (cut append <>
			   (list (new-node (lambda (node projection-matrix view-matrix ctx-matrix)
					     ((*render-constraint*) projection-matrix view-matrix ctx-matrix constraint)))
				 (new-node (lambda (node projection-matrix view-matrix ctx-matrix)
					     ((*render-constraint*) projection-matrix view-matrix ctx-matrix rot-constraint))))))
		     (list constraint rot-constraint))))

(define (outer-for-inner i)
  (list (- (* 2 i) 1) (* 2 i)))

;; outer -> inner
(for-each (cut cp:space-add-constraint the-space <>)
	  (append-ec (:range i 0 (vector-length edges-inner))
		     (:list outer (outer-for-inner i))
		     (let* ([current (vector-ref edges-inner i)]
			    [before (vector-ref edges (% outer (vector-length edges)))]
			  [body-current (node-body current)]
			  [body-before (node-body before)]
			  [constraint (add-constraint (cp:damped-spring-new
						       body-before
						       body-current
						       cp:v0
						       cp:v0
						       (cp:vlength (cp:v- (cp:body-get-position body-before)
									  (cp:body-get-position body-current)))
						       100.
						       the-damping))]
			  [rot-constraint (cp:damped-rotary-spring-new
					   body-before
					   body-current
					   (cp:vtoangle (cp:v- (cp:body-get-position body-before)
							       (cp:body-get-position body-current)))
					   100.
					   the-damping
					   )])
		     (node-children-update! root-node
		      (cut append <>
			   (list (new-node (lambda (node projection-matrix view-matrix ctx-matrix)
					     ((*render-constraint*) projection-matrix view-matrix ctx-matrix constraint)))
				 (new-node (lambda (node projection-matrix view-matrix ctx-matrix)
					     ((*render-constraint*) projection-matrix view-matrix ctx-matrix rot-constraint))))))
		     (list constraint rot-constraint))))

(use ringbuffer)

;; TODO add to common ancestor of src and trg

;; outer -> inner links
;; (append-ec (:range i 0 (vector-length edges-inner))
;; 	   (:list outer (outer-for-inner i))
;; 	   (let* ([current (vector-ref edges-inner i)]
;; 		  [before (vector-ref edges (% outer (vector-length edges)))]
;; 		  [body-current (alist-ref 'body-center current)]
;; 		  [body-before (alist-ref 'body-center before)])
;; 	     (node-children-append! root-node
;; 				    (new-node (lambda (node projection-matrix view-matrix ctx-matrix)
;; 						((*render-link*) projection-matrix view-matrix ctx-matrix src trg))))))

(define rcs
  (let* ([poss (map (lambda (x) (cp:body-get-position (node-body x)))
		    (vector->list edges-inner))]
	 [center-pos (cp:v* (reduce cp:v+ cp:v0 poss)
			    (/ 1 (length poss)))]
	 [body-center (cp:body-new 10. 20.)]
	 [radius 0.1]
	 [shape (cp:circle-shape-new body-center radius cp:v0)])

    (cp:body-set-position body-center center-pos)
    (cp:space-add-body the-space body-center)
    (cp:space-add-shape the-space shape)

    (node-children-update! root-node
			   (cut cons
			     (let* ([body body-center])
			       (new-node (lambda (node projection-matrix view-matrix ctx-matrix)
					   ((*render-circle-shape*) projection-matrix view-matrix shape))
					 body
					 shape))
			     <>))

    (append-ec (:range i 0 (vector-length edges-inner))
	       (let* ([current (vector-ref edges-inner i)]
		      [before (vector-ref edges-inner (% (+ i 1) (vector-length edges-inner)))]
		      [body-current (node-body current)]
		      [constraint (add-constraint (cp:damped-spring-new
						   body-center
						   body-current
						   (cp:vlerp cp:v0 (cp:body-get-position body-center) 0.1)
						   cp:v0
						   (cp:vlength (cp:v- (cp:body-get-position body-center)
								      (cp:body-get-position body-current)))
						   100.
						   the-damping
						   ))]
		      [rot-constraint (cp:damped-rotary-spring-new
				       body-center
				       body-current
				       (cp:vtoangle (cp:v- (cp:body-get-position body-center)
							   (cp:body-get-position body-current)))
				       100.
				       the-damping)])

		 (let ([c (new-node (lambda (node projection-matrix view-matrix ctx-matrix)
				      ((*render-constraint*) projection-matrix view-matrix ctx-matrix constraint)))]
		       [rc (new-node (lambda (node projection-matrix view-matrix ctx-matrix)
				       ((*render-constraint*) projection-matrix view-matrix ctx-matrix rot-constraint)))])
		   (node-children-update! root-node append
					  (list c rc)))
		 (list constraint rot-constraint)))))

;; 1 -> 1 2
;; 2 -> 3 4
(apply cp:space-add-constraints the-space rcs)

;;;; Graphics

;;; Camera

(define camera-position (box (make-point 0. 0. 0.)))

(define camera-zoom (box 1))

;;(define projection-matrix (make-parameter (perspective 600 600 1 10 70)))
(define projection-matrix (make-parameter (ortho 10 10  0.1 100)))

(projection-matrix
 (ortho 10 10  0.1 100))

(define the-eye-point (make-parameter (make-point 0 0 5)))

(the-eye-point (make-point 0 0 5))

(define the-object-point (make-parameter (make-point 0 0 0)))

(the-object-point (make-point 0 0 0))

(define view-matrix
  (make-parameter (look-at (the-eye-point)
			   (the-object-point)
			   (make-point 0 1 0) ; up vector
	    )))


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
   ;; (print (:supported? "GL_ARB_framebuffer_object"))

   ;; enable alpha blending
   (opengl:gl:Enable gl:+blend+)
   (opengl:gl:BlendFunc gl:+src-alpha+ gl:+one-minus-src-alpha+ )

   (start-all-cells)


;   (gl:bind-texture gl:+texture-2d+ (cell-get noise-image-texture))
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

	   ;; set cursor ball
	   (cp:body-set-position (node-body (the-mouse-ball))
				 (the-mouse-v))

 	   ;; process repl event
	   (REPL)

	   ;; advance physics
	   (cp:space-step the-space 1/60 )

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

(repl)

(comment

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
