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

;;;;; Utils ;;;;;

(include "utils.scm")
(include "mesh.scm")
(include "chipmunk-utils.scm")

(define the-line-mesh (line-mesh '(0 0) '(0 0)))

(when (unbound? 'REPL)
  (define repl-port 6060)
  (define REPL (make-prepl repl-port)))

(include "pipeline.scm")

;;;;; Physics ;;;;;

;; TODO perform physics in a separate thread
(define the-space #f)


(define-record node render-fn children matrix body shape) ; takes node , mvp ,

(define (new-node render-fn #!optional (body #f) (shape #f))
  (make-node render-fn
	     '()
	     (mat4-identity)
	     body
	     shape))

(define (node-children-update! node f . args)
  (node-children-set! node (apply f (node-children node) args)))

(define (node-children-append! node . children)
  (node-children-update! node append children))

(define (add-ball space x y idx #!key (elasticity 0.95) (friction 0.2) (mass 1.) (radius 0.1) (velocity #f))
  (let* ((moment (cp:moment-for-circle mass 0. radius cp:vzero))
	 (body (cp:space-add-body space (cp:body-new  mass moment)))
	 (shape (cp:circle-shape-new body radius cp:vzero)))

    (cp:shape-set-friction shape friction)
    (cp:shape-set-elasticity shape elasticity)

    (cp:body-set-position body (cp:v (exact->inexact x)
				     (exact->inexact y)))
    (cp:space-add-shape space shape)

    (when velocity
      (cp:body-set-velocity body velocity))

    (let ([n (new-node (lambda (node projection-matrix view-matrix ctx-matrix)
			 (let ([angle (- (cp:body-get-angle body))]
			       [body-pos (cp:body-get-position body)])
			   (render-mesh circle-mesh
					(cell-get program1)
					(m* projection-matrix
					    (m* (translation (make-point (cp:v.x body-pos)
									 (- (cp:v.y body-pos))
									 0))
						(m* view-matrix
						    (rotate-z angle (model-matrix)))))
					(cp:vlength (cp:body-get-velocity body))
					(vector
					 1
					 (+ 0.5 (* 0.5 (sin idx)))
					 (+ 0.5 (* 0.5 (cos idx)))))))
		       body
		       shape)])
      n)))

(define (fixed-line-segment space from to #!key (radius 0.1))
  (cp:segment-shape-new (cp:space-get-static-body space) from to radius))

;;;; Global State

(define the-nodes (box (list)))

(define the-mouse-ball (box #f))

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

;; set root node
(define root-node (make-node (lambda (x v p c) #f)
			     '()
			     (mat4-identity)
			     #f
			     #f))


(node-children-set! root-node '())

(define (init-physics)
  (let ([space (cp:space-new)])

    (cp:space-set-iterations space 30) ; 10 default

    (scene-1 space)

    (box-set! the-mouse-ball (add-ball space 10. 10. 0 #:radius 0.2 #:friction 0.01))

    (node-children-update! root-node append
			   (list (unbox the-mouse-ball)))

    ;; return space
    space))

(set! the-space (init-physics))



(define (rand n)
  (let ([sz 10000])
    (* n (- (* 2 (/ (random sz) sz)) 1))))

(define (v-rand n)
  (f64vector (rand n) (rand n)))

(node-children-update! root-node append
		       (list (unbox the-mouse-ball))
		       (let ([n 300])
			 (list-ec (:range i n)
				  (let ([angle (/ (* pi 2 i 8) n)]
					[radius (* 2 (/ i n))])
				    (add-ball the-space
					      (* radius (sin angle))
					      (* radius (cos angle))
					      i
					      #:radius 0.1)))))


(define (without list elem)
  (remove (cut eq? elem <>) list))

(define (remove-node parent node)
;  (for-each (cut remove-node node <>) (node-children node))
  (node-children-update! parent without node)
  (when (node-body node) (cp:space-remove-body the-space (node-body p)))
  (when (node-shape node)  (cp:space-remove-shape the-space (node-shape p))))


(define the-counter (box 0))

(define (repeatedly n f)
  (if (positive? n)
    (cons (f) (repeatedly (- n 1) f))
    '()))

(define (add-node-at-pos)
					;   space x y idx
  (let ([m (cp:body-get-position (node-body (unbox the-mouse-ball)))])
    (node-children-update! root-node append
			   (list (add-ball the-space (cp:v.x m) (cp:v.y m) (box-swap! the-counter inc)
					   #:radius 0.4
					   #:velocity (v-rand 10))))
    m))


(comment

 (begin
   (sleep 4)
   (add-node-at-pos))

 )


(define *render-constraint* (make-parameter #f))

(define *render-circle-shape* (make-parameter #f))

(*render-circle-shape* (lambda (projection-matrix view-matrix segment )
			 (let* ([body (cp:shape-get-body segment)]
				[angle (+ (/ pi 4) (- (cp:body-get-angle body)))]
				[body-pos (cp:body-get-position body)]
				[trans (make-point (cp:v.x body-pos)
						   (- (cp:v.y body-pos))
						   0)])
			   (render-mesh circle-mesh
					(cell-get program3)

					(m* projection-matrix
					    (m* (translation trans)
						(m* view-matrix
						    (rotate-z angle (model-matrix)))))

					(cp:vlength (cp:body-get-velocity body))

					(vector 1 1 0)))))

(*render-constraint* (lambda (projection-matrix view-matrix ctx-matrix constraint)
		       (let* ([pos-a (cp:body-get-position (cp:constraint-get-body-a constraint))]
			      [pos-b (cp:body-get-position (cp:constraint-get-body-b constraint))]
			      [angle (cp:vtoangle (cp:v- pos-a pos-b))]
			      [middle (cp:vlerp pos-a pos-b 0.5)]
			      [trans (make-point (cp:v.x middle) (- (cp:v.y  middle))
						 0.)])


			 (mesh-update! the-line-mesh (line-mesh-vertices (cp:v.x pos-a) (- (cp:v.y pos-a))
									 (cp:v.x pos-b) (- (cp:v.y pos-b))))

			 (render-mesh the-line-mesh
				      (cell-get program-line)
				      (m* projection-matrix (m* view-matrix (model-matrix)))
				      (cp:constraint-get-impulse constraint)
				      (vector 1 0 1)))))

(define edges
  (list->vector
   (list-ec (:list x (circle-ring 24))
	    (let* ([start (first x)]
		   [end (second x)]
		   [start-pos (cp:v (car start) (cdr start))]
		   [end-pos (cp:v (car end) (cdr end))]
		   [center-pos (cp:vlerp start-pos end-pos 0.5)]
		   [mass 0.3]
		   [radius 0.3]
		   [body-center (cp:body-new (cp:moment-for-segment mass start-pos end-pos radius) mass)]
		   [shape (cp:circle-shape-new body-center radius cp:vzero)])

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
	      `((body-center . ,body-center)
		(shape . ,shape))))))

(define the-damping 0.4)

(define constraints '())

(define (add-constraint x)
  (set! constraints (cons x constraints))
  x)


(define (damped-spring-update-stiffness spring f . args)
  (let ([new (apply f (cp:damped-spring-get-stiffness spring) args)])
    (cp:damped-spring-set-stiffness spring new)
    new))

(define (damped-spring-update-damping spring f . args)
  (let ([new (apply f (cp:damped-spring-get-damping spring) args)])
    (cp:damped-spring-set-damping spring new)
    new))

(define (update-stiffness delta)
  (map
   (lambda (c) (damped-spring-update-stiffness c + delta))
   constraints))

(define (update-damping delta)
  (map
   (lambda (c) (damped-spring-update-damping c + delta))
   constraints))

(comment

 (update-stiffness 10)

 )

(for-each (cut cp:space-add-constraint the-space <>)
	  (append-ec (:list offset '(1 3 9 11))
		     (:range i 0 (vector-length edges))
		   (let* ([current (vector-ref edges i)]
			  [before (vector-ref edges (% (+ i offset) (vector-length edges)))]
			  [body-current (alist-ref 'body-center current)]
			  [body-before (alist-ref 'body-center before)]
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

(define rcs
  (let* ([poss (map (lambda (x) (cp:body-get-position (alist-ref 'body-center x)))
		    (vector->list edges))]
	 [il (/ 1 (length poss))]
	 [center-pos (cp:v* (reduce cp:v+ (cp:v 0. 0.) poss) il)]
	 [body-center (cp:body-new 10. 20.)]
	 [radius 0.1]
	 [shape (cp:circle-shape-new body-center radius cp:vzero)])

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

    (append-ec (:range i 0 (vector-length edges))
	       (let* ([current (vector-ref edges i)]
		      [before (vector-ref edges (% (+ i 1) (vector-length edges)))]
		      [body-current (alist-ref 'body-center current)]
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
		 (node-children-update! root-node append
			    (list (new-node (lambda (node projection-matrix view-matrix ctx-matrix)
					      ((*render-constraint*) projection-matrix view-matrix ctx-matrix constraint)))
				  (new-node (lambda (node projection-matrix view-matrix ctx-matrix)
					      ((*render-constraint*) projection-matrix view-matrix ctx-matrix rot-constraint)))))

		 (list constraint rot-constraint)))))

(for-each (cut cp:space-add-constraint the-space <>)  rcs)

(define (clear-space space)
  (for-each (cut cp:space-remove-body the-space <>) (cp:space-bodies the-space)))

;;;;; Graphics ;;;;;

(define projection-matrix (make-parameter (perspective 600 600 0.1 100 70)))

(define the-eye-point
  (box (make-point 0 0 7)))

(define the-object-point
  (box (make-point 0 0 0)))

(define (view-matrix)
  (look-at (unbox the-eye-point)
	   (unbox the-object-point)
           (make-point 0 1 0) ; up vector
	   ))

(define (model-matrix)
  (mat4-identity))

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

;;;; Window setup

(include "input.scm")

(define (screen->world xy)
  (let* ([x (car xy)]
	 [y (cdr xy)]
	 [v (view-matrix)]
	 [p (projection-matrix)]
	 [world->screen (m* v p)]
	 [m-screen->world (inverse world->screen)]
	 [screen-pos (make-point x y 0)])
    (m*vector! m-screen->world (make-point x y 0))))

(define (set-gravity)
  (let ([v (screen->world (the-mouse-pos))])
    (cp:space-set-gravity the-space
			  (cp:v (f32vector-ref v 0)
				(f32vector-ref v 1)))))

(define (update-gravity f . args)
  (let ([new-gravity (apply f (cp:space-get-gravity the-space) args)])
    (cp:space-set-gravity the-space new-gravity)
    new-gravity))

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

	   ;; set mouse position
	   (let* ([p (screen->world (the-mouse-pos))]
		  [mouse-x (f32vector-ref p 0)]
		  [mouse-y (f32vector-ref p 1)])
	     (cp:body-set-position (node-body (unbox the-mouse-ball))
				   (cp:v mouse-x mouse-y)))

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

(comment

 (remove-node root-node (unbox the-mouse-ball))

 )

(define main-thread (make-thread main))


(use soil)

(define mt (thread-start! main-thread))



(comment (thread-terminate! mt )


	 (begin (update-gravity cp:v+ (cp:v 0. 1.))

		(update-gravity (constantly (cp:v 0 0)))

		(cp:space-get-gravity the-space))

	 (unbox foo)


	 )
					;(main)

(require-extension apropos)
(require-extension chicken-doc)
(require-extension parley)
(require-extension parley-auto-completion)

(define exit? (make-parameter #f))

(define stack (box '()))

(define (read-balanced-port)
  (let loop ()
    (./trace stack)
    (let ([line (read-line)])
      (./trace line)
      (if (eof-object? line)
	  line
	  (handle-exceptions exn
	      (begin
		(./trace exn)
		(box-swap! stack (cut cons line <>))
		(loop))
	    (let ([str (string-concatenate (reverse (cons line (unbox stack))))])
	      (./trace str)
	      (let ([result (with-input-from-string str read)])
		(box-set! stack '())
		result)))))))

;; (define (shell-repl)
;;   (if (exit?)
;;       #t
;;       (begin (handle-exceptions
;; 	      exn
;; 	      (begin (print-error-message exn)
;; 		     (display (with-output-to-string (lambda () (print-call-chain)))))
;; 	      (let ((x (with-input-from-string (parley ((repl-prompt))) read-balanced-port)))
;; 		(write (eval x))
;;                 ;(line-num (+ (line-num) 1))
;; 		))
;; 	     (newline)
;; 	     (shell-repl))))

;; (shell-repl)
;;(geiser-start-server)
