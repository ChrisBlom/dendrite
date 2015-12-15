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
     ringbuffer)

;;;;; Utils ;;;;;

(include "utils.scm")
(include "mesh.scm")
(include "chipmunk-utils.scm")

(define the-line-mesh (line-mesh '(0 0) '(0 0)))

(when (unbound? 'REPL)
  (define REPL (make-prepl 1113)))

(define (compile-shaders! vertex-string fragment-string)
  (let ([vertex-shader-id   (make-shader gl:+vertex-shader+ vertex-string)]
	[fragment-shader-id (make-shader gl:+fragment-shader+ fragment-string)])
    ;; compile shader, set program parameter using
    (make-program (list vertex-shader-id fragment-shader-id))))

(define -file-v1-  (file-cell "vertex-shaders/v1.glsl"))
(define -file-v2-  (file-cell "vertex-shaders/v2.glsl"))
(define -file-v3-  (file-cell "vertex-shaders/v3.glsl"))
(define -file-vline-  (file-cell "vertex-shaders/line.glsl"))

(define *v1* (create-cell read-all -file-v1-))
(define *v2* (create-cell read-all -file-v2-))
(define *v3* (create-cell read-all -file-v3-))
(define *vline* (create-cell read-all -file-vline-))

(define *fragment* (create-cell read-all (file-cell "fragment-shaders/simple.glsl")))
(define *fragment-constant* (create-cell read-all (file-cell "fragment-shaders/constant.glsl")))
(define *fragment-line* (create-cell read-all (file-cell "fragment-shaders/line.glsl")))

;; paused as compile-shader can only be used after gl is initialized
(define program1     (create-paused-cell compile-shaders! *v1* *fragment*))
(define program2     (create-paused-cell compile-shaders! *v2* *fragment*))
(define program3     (create-paused-cell compile-shaders! *v3* *fragment-constant*))
(define program-line (create-paused-cell compile-shaders! *vline* *fragment-line*))

(define (ball->node idx ball)
  (let* ([body (alist-ref 'body ball)])
    (make-node (lambda (node projection-matrix view-matrix)
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
				 (+ 0.5 (* 0.5 (cos idx))))))))))

;;;;; Physics ;;;;;

;; TODO perform physics in a separate thread
(define the-space (box #f))

(define-record node
  render-fn) ; takes node , mvp ,
  

(define (add-ball space x y #!key (elasticity 0.95) (friction 0.2) (mass 1.) (radius 0.1))
  (let* ((moment (cp:moment-for-circle mass 0. radius cp:vzero))
	 (body (cp:space-add-body space (cp:body-new  mass moment)))
	 (shape (cp:circle-shape-new body radius cp:vzero)))

    (cp:shape-set-friction shape friction)
    (cp:shape-set-elasticity shape elasticity)

    (cp:body-set-position body (cp:v (exact->inexact x)
				     (exact->inexact y)))
    (cp:space-add-shape space shape)
    (list
     (cons 'body  body)
     (cons 'shape shape))))

(define (fixed-line-segment space from to #!key (radius 0.1))
  (cp:segment-shape-new (cp:space-get-static-body space) from to radius))

(define the-balls (box #f))

(define the-nodes (box (list)))

(define the-mouse-ball (box #f))

(define (circle-ring n)
  (let ([b (list->ringbuffer
	    (map (lambda (i)
		   (let ([angle (* i (/ n) pi 2)])
		     (cons (* 2 (sin angle))
			   (* 2 (cos angle)))))
		 (range 0 n)))])
    (map (lambda (j)
	   `( ,(ringbuffer-get b j)
	      ,(ringbuffer-get b (+ 1 j))))
	 (range 0 n))))

;;   ==== end> 0 <start =========body========= end> 0 <start ====




(define (init-physics)
  (let ([space (cp:space-new)])

    (cp:space-set-iterations space 30) ; 10 default

    ;; walls
    (cp:space-add-shapes space
			 (doto (fixed-line-segment space (cp:v -5. -5.) (cp:v -5. +5.) radius: 0.4)
			       (cp:shape-set-elasticity 0.95))
			 (doto (fixed-line-segment space (cp:v -5. +5.) (cp:v +5. +5.) radius: 0.4)
			       (cp:shape-set-elasticity 0.95))
			 (doto (fixed-line-segment space (cp:v +5. +5.) (cp:v +5. -5.) radius: 0.4)
			       (cp:shape-set-elasticity 0.95))
			 (doto (fixed-line-segment space (cp:v +5. -5.) (cp:v -5. -5.) radius: 0.4)
			       (cp:shape-set-elasticity 0.95)))

    (box-set! the-mouse-ball (add-ball space 10. 10. #:radius 1. #:friction 0.01))

    (box-set! the-space space)

    (box-set! the-balls (cons (unbox the-mouse-ball)
			      (let ([n 300])
				(map (lambda (i)
				       (let ([angle (/ (* pi 2 i 8) n)]
					     [radius (* 2 (/ i n))])
					 (add-ball space
						   (* radius (sin angle))
						   (* radius (cos angle))
						   #:radius 0.1
						   )))
				     (range 0 n)))))

    (box-set! the-nodes (map-indexed ball->node (box-ref the-balls)))))

(init-physics)


(define (damped-spring body-a body-b #!key length (stiffness 40.) (damping 0.99) anchor-a anchor-b)
  (let ([c  (cp:damped-spring-new
	     (./trace body-a)
	     (./trace body-b)
	     (./trace (or anchor-a cp:v0))
	     (./trace (or anchor-b cp:v0))
	     (./trace (or length (cp:vdist
				  (cp:body-get-position body-a)
				  (cp:body-get-position body-b))) )
	     (./trace stiffness)
	     (./trace damping))])
    (./trace c)
    ))



(define render-circle-shape (box #f))
(define *render-constraint* (box #f))


(box-set! render-circle-shape
	  (lambda (projection-matrix view-matrix segment )
	    (let* (
		   [body (cp:shape-get-body segment)]
					;[pos-a (cp:segment-shape-get-a segment)]
					;		   [pos-b (cp:segment-shape-get-b segment)]
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

(box-set! *render-constraint*
	  (lambda (projection-matrix view-matrix constraint)
	    (let* ([pos-a (cp:body-get-position (cp:constraint-get-body-a constraint))]
		   [pos-b (cp:body-get-position (cp:constraint-get-body-b constraint))]
		   [angle (cp:vtoangle (cp:v- pos-a pos-b))]
		   [middle (cp:vlerp pos-a pos-b 0.5)]
		   [trans (make-point (cp:v.x middle)
				      (- (cp:v.y  middle))
				      0.)]
		   )


	      (mesh-update! the-line-mesh (line-mesh-vertices (cp:v.x pos-a) (- (cp:v.y pos-a))
							      (cp:v.x pos-b) (- (cp:v.y pos-b))))

	      (render-mesh the-line-mesh
			   (cell-get program-line)
			   (m* projection-matrix
			       (m* view-matrix
				   (model-matrix)))
			   (cp:constraint-get-impulse constraint)
			   (vector 1 0 1)))))

(define edges
  (list->vector
   (map (lambda (x)
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
	    (cp:space-add-body (unbox the-space) body-center)
	    (cp:space-add-shape (unbox the-space) shape)
	    (box-swap! the-nodes
		       (cut cons
			 (let* ([body body-center])
			   (make-node (lambda (node projection-matrix view-matrix )
					((unbox render-circle-shape) projection-matrix view-matrix shape))))
			 <>))

	    `((body-center . ,body-center)
	      (shape . ,shape))))
	(circle-ring 24))))

(define the-damping 0.4)

(define cs
  (append-map (lambda (offset)
		(append-map (lambda (i) (let* ([current (vector-ref edges i)]
					  [before (vector-ref edges (% (+ i offset) (vector-length edges)))]
					  [body-current (alist-ref 'body-center current)]
					  [body-before (alist-ref 'body-center before)]
					  [constraint (cp:damped-spring-new
						       body-before
						       body-current
						       cp:v0
						       cp:v0
						       (cp:vlength (cp:v- (cp:body-get-position body-before)
									  (cp:body-get-position body-current)))
						       100.
						       the-damping
						       )]
					  [rot-constraint (cp:damped-rotary-spring-new
							   body-before
							   body-current
							   (cp:vtoangle (cp:v- (cp:body-get-position body-before)
									       (cp:body-get-position body-current)))
							   100.
							   the-damping
							   )])
				     (box-swap! the-nodes
						(cut append <>
						     (list (make-node (lambda (node projection-matrix view-matrix )
									((unbox *render-constraint*) projection-matrix view-matrix constraint)))
							   (make-node (lambda (node projection-matrix view-matrix )
									((unbox *render-constraint*) projection-matrix view-matrix rot-constraint))))))
				     (list constraint rot-constraint)
				     ))
			    (range 0 (vector-length edges))))
	      '(1 3 9 11)))


(for-each (cut cp:space-add-constraint (unbox the-space) <>)  cs)

(define rcs
  (let* ([poss (map (lambda (x) (cp:body-get-position (alist-ref 'body-center x)))
		    (vector->list edges))]
	 [il (/ 1 (length poss))]
	 [center-pos (cp:v* (reduce cp:v+ (cp:v 0. 0.) poss) il)]
	 [body-center (cp:body-new 10. 20.)]
	 [radius 0.1]
	 [shape (cp:circle-shape-new body-center radius cp:vzero)])

    (cp:body-set-position body-center center-pos)
    (cp:space-add-body (unbox the-space) body-center)
    (cp:space-add-shape (unbox the-space) shape)

    (box-swap! the-nodes
	       (cut cons
		 (let* ([body body-center])
		   (make-node (lambda (node projection-matrix view-matrix )
				((unbox render-circle-shape) projection-matrix view-matrix shape))))
		 <>))


    (append-map (lambda (i) (let* ([current (vector-ref edges i)]
			      [before (vector-ref edges (% (+ i 1) (vector-length edges)))]
			      [body-current (alist-ref 'body-center current)]
			      [constraint (cp:damped-spring-new
					   body-center
					   body-current
					   (cp:vlerp cp:v0 (cp:body-get-position body-center) 0.1)
					   cp:v0
					   (cp:vlength (cp:v- (cp:body-get-position body-center)
							      (cp:body-get-position body-current)))
					   100.
					   the-damping
					   )]
			      [rot-constraint (cp:damped-rotary-spring-new
					       body-center
					       body-current
					       (cp:vtoangle (cp:v- (cp:body-get-position body-center)
								   (cp:body-get-position body-current)))
					       100.
					       the-damping
					       )])
			 (box-swap! the-nodes
				    (cut append <>
					 (list (make-node (lambda (node projection-matrix view-matrix )
							    ((unbox *render-constraint*) projection-matrix view-matrix constraint)))
					       (make-node (lambda (node projection-matrix view-matrix )
							    ((unbox *render-constraint*) projection-matrix view-matrix rot-constraint))))
					 ))

			 (list constraint rot-constraint)
			 ))
		(range 0 (vector-length edges)))))

(for-each (cut cp:space-add-constraint (unbox the-space) <>)  rcs)



;; (for-each
;;  (cut cp:shape-set-elasticity <> 0.1)
;;  (cp:space-shapes (unbox the-space)))

;; (cp:space-bodies the-space)

(define (clear-space space)
  (for-each (cut cp:space-remove-body the-space <>) (cp:space-bodies the-space)))

;;;;; Graphics ;;;;;
(define (watch-reload! file on-change)
  (on-change file)
  (define (tsleep n)
    (thread-sleep! (seconds->time (+ n (time->seconds (current-time))))))
  (define (get-time)
    (file-modification-time file))
  (define active (box #t))
  (define (stop)
    (box-set! active #f))
  (thread-start!
   (lambda ()
     (let loop ((filetime '()))

       (let ((newtime (get-time)))
	 (when (not (equal? filetime newtime))
	   (handle-exceptions e (lambda (e) (display e))
	     (display "Updated: ") (display file) (newline)
	     (on-change file)))
	 (tsleep 1)
	 (loop newtime)))))
  stop)

(define (watched-slurp file on-change)
  (let ([active (box #t)]
	[box-val (box (read-all file))])
    (thread-start!
     (lambda () (let loop ([filetime (file-modification-time file)])
	     ;; ;(display "Polling ") (display file) (newline)
	     (let ([newtime (file-modification-time file)])
	       (when (not (equal? filetime newtime))
		 (handle-exceptions e (lambda (e) (display e) (newline e))
		   (let ([new-value (read-all file)])
		     (display "Updated: ") (display file) (newline)
		     (box-set! box-val new-value)
		     (on-change file new-value))))
	       (thread-sleep! 1)
	       (loop newtime)))))))

(define (projection-matrix)
  (perspective 600 600 0.1 100 70))

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


(define (render nodes)
  (let ([projection (projection-matrix)]
	[view (view-matrix)])
    ;; call render fn of each node
    (for-each (lambda (node)
		(let ([render-fn (node-render-fn node)])
		  (render-fn node
			     projection
			     view
			     )))
	      nodes)

    (check-error)))

;;
(glfw:key-callback (lambda (window key scancode action mods)
		     ;(display (list 'key= key scancode action mods)) (newline)
                     (cond
                      [(and (eq? key glfw:+key-escape+) (eq? action glfw:+press+))
                       (glfw:set-window-should-close window #t)])))

(define the-mouse-pos (box (cons 10. 10.)))

(define (screen->world xy)
  (let* ([x (car xy)]
	 [y (cdr xy)]
	 [v (view-matrix)]
	 [p (projection-matrix)]
	 [world->screen (m* v p)]
	 [m-screen->world (inverse world->screen)]
	 [screen-pos (make-point x y 0)])
    (m*vector! m-screen->world (make-point x y 0))))

;;
(glfw:cursor-position-callback (lambda (window x y)
				 (box-set! the-mouse-pos (cons (+ (- (/ x 2)) 150)
							       (+ (- (/ y 2)) 150)))
					;(display (list 'cursor= x y)) (newline)
					;(display (list 'cursor-world= (screen->world (cons x y)))) (newline)
				 ))


(define (set-gravity)
  (let ([v (screen->world (unbox the-mouse-pos))])
    (cp:space-set-gravity (unbox the-space)
			  (cp:v (f32vector-ref v 0)
				(f32vector-ref v 1)))))


(define (u32deref x)
  (u32vector-ref x 0))

(define (render-to-texture)
  ;;; http://www.opengl-tutorial.org/intermediate-tutorials/tutorial-14-render-to-texture/
  (let* ([fb-name 1]
	 [rendered-texture (u32vector 0)]
	 [depthrenderbuffer (u32vector 0)])


    ;; create framebuffer
    (gl:gen-framebuffers fb-name (u32vector 1 2 3))
    (gl:bind-framebuffer gl:+framebuffer+ fb-name)

    ;; create texture
    (gl:gen-textures 1 rendered-texture)
    (gl:bind-texture gl:+texture-2d+ (u32deref rendered-texture))
    (gl:tex-image-2d gl:+texture-2d+
    		     0
    		     gl:+rgb+
    		     1024 ;width
    		     768 ; height
    		     0 ; border
    		     gl:+rgb+ ; format
    		     gl:+unsigned-byte+ ;type
    		     #f ; empty
    		     )

    (gl:tex-parameteri gl:+texture-2d+ gl:+texture-mag-filter+ gl:+nearest+)
    (gl:tex-parameteri gl:+texture-2d+ gl:+texture-min-filter+ gl:+nearest+)

    ;; created renderbuffer
    (gl:gen-renderbuffers 1 depthrenderbuffer)	      ;
    (gl:bind-renderbuffer gl:+renderbuffer+ (u32deref depthrenderbuffer)) ;
    (gl:renderbuffer-storage gl:+renderbuffer+ gl:+depth-component+ 1024 768) ;
    (gl:framebuffer-renderbuffer gl:+framebuffer+ gl:+depth-attachment+ gl:+renderbuffer+ (u32deref depthrenderbuffer)) ;


    ;; configure framebuffer
    (gl:framebuffer-texture gl:+framebuffer+ gl:+color-attachment0+ (u32deref rendered-texture) 0)

    (list rendered-texture depthrenderbuffer)))



(define (main)
  (glfw:with-window (600 600 "Dendrite"
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

		    (cell-unpause program1)
		    (cell-unpause program2)
		    (cell-unpause program3)
		    (cell-unpause program-line)

		    ;; create vertex array object for mesh
		    (mesh-make-vao! circle-mesh `((position . ,(gl:get-attrib-location (cell-get program1) "position"))
						  (color . ,(gl:get-attrib-location (cell-get program1) "color"))))

		    (mesh-make-vao! rectangle-mesh `((position . ,(gl:get-attrib-location (cell-get program1) "position"))
						     (color . ,(gl:get-attrib-location (cell-get program1) "color"))))

		    (mesh-make-vao! the-line-mesh `((position . ,(gl:get-attrib-location (cell-get program-line) "position"))
						    (color . ,(gl:get-attrib-location (cell-get program-line) "color")))
				    #:stream)


		    (let loop ([i 0]
			       [t (current-milliseconds)])
		      (REPL) ; process repl event
		      (cp:space-step (unbox the-space) 1/60 ) ;; TODO use delta time
					;(cp:space-step (unbox the-space) 1/60 ) ;; TODO use delta time
					;(cp:space-step (unbox the-space) 1/60 ) ;; TODO use delta time
		      (let* ([p (screen->world (unbox the-mouse-pos))]
			     [mouse-x (f32vector-ref p 0)]
			     [mouse-y (f32vector-ref p 1)])
			(cp:body-set-position (alist-ref 'body (unbox the-mouse-ball))
					      (cp:v mouse-x mouse-y)))

		      (glfw:swap-buffers (glfw:window))
		      (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
		      (render (box-ref the-nodes))
		      (glfw:poll-events) ; Because of the context version, initializing GLEW results in a harmless invalid enum
		      (thread-yield!)
		      (unless (glfw:window-should-close (glfw:window))
			(if (eq? 0 (% i 100))
			    (let ([dt (- (current-milliseconds) t)])
					;(display (/ 1000 dt)) (newline)
			      #f
			      ))
			(loop (+ 1 i) (current-milliseconds))))))

(define main-thread (thread-start! (make-thread main)))

					;(thread-join! main-thread)

					;(main)

(require-extension apropos)
(require-extension chicken-doc)
(require-extension parley)
(require-extension parley-auto-completion)

(define exit? (make-parameter #f))

(define (shell-repl)
  (if (exit?)
      #t
      (begin (handle-exceptions
	      exn
	      (begin (print-error-message exn)
		     (display (with-output-to-string (lambda () (print-call-chain)))))
	      (let ((x (with-input-from-string (parley ((repl-prompt))) (lambda () (read)))))
		(write (eval x))
                ;(line-num (+ (line-num) 1))
		))
	     (newline)
	     (shell-repl))))

(when (not (feature? 'csi))
  (shell-repl))
