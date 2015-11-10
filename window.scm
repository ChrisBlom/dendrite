(import chicken scheme)

(use (prefix glfw3 glfw:)
     (prefix opengl-glew gl:)
     (prefix gl opengl:)
     (prefix chipmunk cp-)
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
     posix)

;;;;; Utils ;;;;;

(include "utils.scm")
(include "mesh.scm")
(include "chipmunk-utils.scm")



(when (unbound? 'REPL)
    (define REPL (make-prepl 1113)))

(define-syntax comment
  (syntax-rules ()
    [(comment expr ...)
     #f]))

(define-syntax ./trace
  (syntax-rules ()
    [(_ a ...)
     (let ([name-to-val (map cons (list 'a ...) (list a ...))])
       (printf "---- TRACE: ---- \n" )
       (for-each
	(lambda (x)
	  (printf " ~s : ~s\n" (car x) (cdr x))
	  )
	name-to-val)
       (printf "---------------- \n" )
       (cdr (last name-to-val)))]))

(define (compile-shaders! vertex-string fragment-string)
  (let ([vertex-shader-id   (make-shader gl:+vertex-shader+ vertex-string)]
	[fragment-shader-id (make-shader gl:+fragment-shader+ fragment-string)])
    ;; compile shader, set program parameter using
    (make-program (list vertex-shader-id fragment-shader-id))))

(define program1 (make-box #f))
(define program2 (make-box #f))
(define program3 (make-box #f))

(define *v1* (box (read-all "vertex-shaders/v1.glsl")))
(define *v2* (box (read-all "vertex-shaders/v2.glsl")))
(define *v3* (box (read-all "vertex-shaders/v3.glsl")))

(define *fragment* (box (read-all "fragment-shaders/simple.glsl")))

(define (ball->node idx ball)
  (let* ([body (alist-ref 'body ball)]
	 [programs (vector program1 program2 program3)])
    (make-node (lambda (node projection-matrix view-matrix)
		 (let ([angle (- (cp-body-get-angle body))]
		       [body-pos (cp-body-get-position body)])
		   (render-mesh circle-mesh
				 (unbox (vector-ref programs (modulo idx 3)))
				 (m* projection-matrix
				     (m* (translation (make-point (cp-v.x body-pos)
								  (- (cp-v.y body-pos))
								  0))
					 (m* view-matrix
					     (rotate-z angle (model-matrix)))))
				 (cp-vlength (cp-body-get-velocity body))
				 (vector
				  1
				  (+ 0.5 (* 0.5 (sin idx)))
				  (+ 0.5 (* 0.5 (cos idx))))))))))


;;;;; Physics ;;;;;

;; TODO perform physics in a separate thread
(define the-space (box #f))

(define-record node
  render-fn ; takes node , mvp ,
  )


;(for-each (lambda (s) (cp-shape-set-friction s 0.)) (cp-space-shapes (unbox the-space)))

(define (add-ball space x y #!key (elasticity 0.95) (friction 0.2) (mass 1.) (radius 0.1))
  (let* ((moment (cp-moment-for-circle mass 0. radius cp-vzero))
	 (body (cp-space-add-body space (cp-body-new  mass moment)))
	 (shape (cp-circle-shape-new body radius cp-vzero)))
    (cp-shape-set-friction shape friction)
    (cp-shape-set-elasticity shape elasticity)
    (cp-body-set-position body (cp-v (exact->inexact x)
				     (exact->inexact y)))
    (cp-space-add-shape space shape)
    (list
     (cons 'body  body)
     (cons 'shape shape))))

(define (fixed-line-segment space from to #!key (radius 0.1))
  (cp-segment-shape-new (cp-space-get-static-body space) from to radius))

(define the-balls (box #f))

(define the-nodes (box (list)))

(define the-mouse-ball (box #f))

(load "ringbuffer.scm")
(import ringbuffer)

(define (circle-ring n)
  (let ([b (list->ringbuffer
	    (map (lambda (i)
		   (let ([angle (* i (/ n) pi 2)])
		     (cons (sin angle)
			   (cos angle))))
		 (range 0 n)))])
    (map (lambda (j)
	   `( ,(ringbuffer-get b j)
	      ,(ringbuffer-get b (+ 1 j))))
	 (range 0 n))))

;;   ==== end> 0 <start =========body========= end> 0 <start ====




(define (init-physics)
  (let ([space (cp-space-new)])

    (cp-space-set-iterations space 30) ; 10 default

    ;; walls
    (cp-space-add-shapes space
     (doto (fixed-line-segment space (cp-v -5. -5.) (cp-v -5. +5.) radius: 0.4)
	   (cp-shape-set-elasticity 0.95))
     (doto (fixed-line-segment space (cp-v -5. +5.) (cp-v +5. +5.) radius: 0.4)
	   (cp-shape-set-elasticity 0.95))
     (doto (fixed-line-segment space (cp-v +5. +5.) (cp-v +5. -5.) radius: 0.4)
	   (cp-shape-set-elasticity 0.95))
     (doto (fixed-line-segment space (cp-v +5. -5.) (cp-v -5. -5.) radius: 0.4)
	   (cp-shape-set-elasticity 0.95))
     ;; diagional
     ;; (doto (fixed-line-segment space (cp-v +5. +5.) (cp-v -5. -5.))
     ;; 	   (cp-shape-set-elasticity 0.95))
     ;; (doto (fixed-line-segment space (cp-v -5. +5.) (cp-v +5. -5.))
     ;; 	   (cp-shape-set-elasticity 0.95))
     )

    ;(cp-space-set-gravity space (cp-v 0. -0.98))

    ;; (cp-space-add-constraint
    ;;  the-space
    ;;  (damped-rotary-spring
    ;;   (alist-ref 'body (first (unbox the-balls)))
    ;;   (alist-ref 'body (second (unbox the-balls)))))

    (box-set! the-mouse-ball (add-ball space 0. 0. #:radius 1.))

    (box-set! the-space space)

    (box-set! the-balls (cons (unbox the-mouse-ball)
			      (let ([n 2000])
				(map (lambda (i)
				       (let ([angle (/ (* pi 2 i 8) n)]
					     [radius (* 4 (/ i n))])
					 (add-ball space
						   (* radius (sin angle))
						   (* radius (cos angle)))))
				     (range 0 n)))))

    (box-set! the-nodes (map-indexed ball->node (box-ref the-balls)))))

(init-physics)

(define cs
  (let* ([m (list->ringbuffer (map (lambda (x)
				     (let* ([start (first x)]
					    [end (second x)]
					    [start-pos (cp-v (car start) (cdr start))]
					    [end-pos (cp-v (car end) (cdr end))]
					    [center-pos (cp-vlerp start-pos end-pos 0.5)]
					    [body-start (cp-body-new 1. ; moment
								     1. ; mass
								     )]

					    [body-end (cp-body-new 1. ; moment
								   1. ; mass
								   )]

					    [body-center (cp-body-new 11. ; moment
								      1. ; mass
								      )]

					    [shape (cp-segment-shape-new body-center
									 start-pos
									 end-pos
									 .4 ; radius
									 )])
					;				     (cp-body-set-position body-start start-pos)
					;				     (cp-body-set-position body-end end-pos)
				       (cp-body-set-position body-center center-pos)
				       (display the-space) (newline)
				       (display shape) (newline)
					;				     (cp-space-add-shape (unbox the-space) body-center)

				       (cp-space-add-shape (unbox the-space) shape)
				       (cp-space-add-body (unbox the-space) body-center)


				       `((body-start . ,body-start)
					 (body-end . ,body-end)
					 (body-center . ,body-center)
					 (shape . ,shape))))
				   (circle-ring 9)
				   ))])
    (map (lambda (i)
	   (let* ([current (ringbuffer-get m i)]
		  [before (ringbuffer-get m (+ 1 i))]
		  [after (ringbuffer-get m (- i 1))]

		  [c (damped-spring (alist-ref 'body-center current)
				    (alist-ref 'body-center before)
					;anchor-a: (./trace (cp-body-get-position (alist-ref 'body-center current)))
					;anchor-b: (./trace (cp-body-get-position (alist-ref 'body-center before)))
				    )])

	     ;(./trace i c (constraint-type c))
	     	      (cp-space-add-constraint (unbox the-space) c)
	     ;; [panic] out of memory - heap full while resizing - execution terminated
	     c
	     ))
	 (range 0 (ringbuffer-length m)))))

;; (for-each
;;  (cut cp-shape-set-elasticity <> 0.1)
;;  (cp-space-shapes (unbox the-space)))

;; (cp-space-bodies the-space)

(define (clear-space space)
  (for-each (cut cp-space-remove-body the-space <>) (cp-space-bodies the-space)))

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
  (perspective 800 600 0.1 100 70))

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

(define circle-mesh (disk 4))

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
		     (display (list 'key= key scancode action mods)) (newline)
                     (cond
                      [(and (eq? key glfw:+key-escape+) (eq? action glfw:+press+))
                       (glfw:set-window-should-close window #t)])))

(define the-mouse-pos (box (cons 0 0)))

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
				 (box-set! the-mouse-pos (cons (+ (- (/ x 2)) 200)
							       (+ (- (/ y 2)) 150)))
				 ;(display (list 'cursor= x y)) (newline)
				 ;(display (list 'cursor-world= (screen->world (cons x y)))) (newline)
				 ))


(define (set-gravity)
  (let ([v (screen->world (unbox the-mouse-pos))])
    (cp-space-set-gravity (unbox the-space)
			  (cp-v (f32vector-ref v 0)
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
  (glfw:with-window (800 600 "Dendrite"
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

   (set-box! program1 (compile-shaders! (unbox *v1*) (unbox *fragment*)))
   (set-box! program2 (compile-shaders! (unbox *v2*) (unbox *fragment*)))
   (set-box! program3 (compile-shaders! (unbox *v3*) (unbox *fragment*)))

   ;; create vertex array object for mesh
   (mesh-make-vao! circle-mesh `((position . ,(gl:get-attrib-location (box-ref program1) "position"))
				 (color . ,(gl:get-attrib-location (box-ref program1) "color"))))

   (let loop ([i 0]
	      [t (current-milliseconds)])
     (REPL) ; process repl event
     (cp-space-step (unbox the-space) 1/60 ) ;; TODO use delta time
     ;(cp-space-step (unbox the-space) 1/60 ) ;; TODO use delta time
     ;(cp-space-step (unbox the-space) 1/60 ) ;; TODO use delta time
     (let* ([p (screen->world (unbox the-mouse-pos))]
	    [mouse-x (f32vector-ref p 0)]
	    [mouse-y (f32vector-ref p 1)])
       (cp-body-set-position (alist-ref 'body (unbox the-mouse-ball))
			     (cp-v mouse-x mouse-y)))

     (glfw:swap-buffers (glfw:window))
     (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
     (render (box-ref the-nodes))
     (glfw:poll-events) ; Because of the context version, initializing GLEW results in a harmless invalid enum
     (thread-yield!)
     (unless (glfw:window-should-close (glfw:window))
       (if (eq? 0 (% i 100))
	 (let ([dt (- (current-milliseconds) t)])
	   (display (/ 1000 dt)) (newline)))
       (loop (+ 1 i) (current-milliseconds))))))

(define main-thread (thread-start! (make-thread main)))

(define watcher-1 (watch-reload! "vertex-shaders/v1.glsl"
			       (lambda (f)
				 (when f
				   (display "Updated") (display f) (newline)
				   (box-set! *v1* (read-all f))
				   (set-box! program1 (compile-shaders! (read-all f) (unbox *fragment*)))
				   (read-all f)
				   (newline)))))

(define watcher-2 (watch-reload! "vertex-shaders/v1.glsl"
			       (lambda (f)
				 (when f
				   (display "Updated") (display f) (newline)
				   (box-set! *v2* (read-all f))
				   (set-box! program2 (compile-shaders! (read-all f) (unbox *fragment*) ))
				   (read-all f)
				   (newline)))))

(define watcher-3 (watch-reload! "vertex-shaders/v1.glsl"
			       (lambda (f)
				 (when f
				   (display "Updated") (display f) (newline)
				   (box-set! *v3* (read-all f))
				   (set-box! program3 (compile-shaders! (read-all f) (unbox *fragment*) ))
				   (read-all f)
				   (newline)))))

(define watcher-4 (watch-reload! "fragment-shaders/simple.glsl"
			       (lambda (f)
				 (when f
				   (display "Updated") (display f) (newline)
				   (box-set! *fragment* (read-all f))
				   (set-box! program1 (compile-shaders! (unbox *v1*) (unbox *fragment*) ))
				   (set-box! program2 (compile-shaders! (unbox *v2*) (unbox *fragment*) ))
				   (set-box! program3 (compile-shaders! (unbox *v3*) (unbox *fragment*) ))
				   (read-all f)
				   (newline)))))

;(thread-join! main-thread)

;(main)
