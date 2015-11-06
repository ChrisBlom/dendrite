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

(when (unbound? 'REPL)
    (define REPL (make-prepl 1113)))

(define *fragment*
#<<END
#version 330
in vec4 c;
out vec4 fragColor;
void main(){
  fragColor = vec4(c.x,c.y,c.z,c.w);			;
}
END
)

(define (set-shaders! vertex-string fragment-string)
  (let ([vertex-shader-id   (make-shader gl:+vertex-shader+ vertex-string)]
	[fragment-shader-id (make-shader gl:+fragment-shader+ fragment-string)])
    ;; compile shader, set program parameter using
    (make-program (list vertex-shader-id fragment-shader-id))))

(define program1 (make-box #f))
(define program2 (make-box #f))
(define program3 (make-box #f))

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
				 (cp-vlength (cp-body-get-velocity body))))))))

;;;;; Utils ;;;;;

(include "utils.scm")

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

    (box-set! the-mouse-ball (add-ball space 0. 0. radius: 3.))

    (box-set! the-space space)

    (box-set! the-balls (cons (unbox the-mouse-ball)
			      (let ([n 500])
				(map (lambda (i)
				       (let ([angle (/ (* pi 2 i 8) n)]
					     [radius (* 4 (/ i n))])
					 (add-ball space
						   (* radius (sin angle))
						   (* radius (cos angle)))))
				     (range 0 n)))))

    (box-set! the-nodes (map-indexed ball->node (box-ref the-balls)))))

(init-physics)

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
       ;(display "Polling ") (display file) (newline)
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

(define v1 (box (read-all "vertex-shaders/v1.glsl")))
(define v2 (box (read-all "vertex-shaders/v2.glsl")))
(define v3 (box (read-all "vertex-shaders/v3.glsl")))

(include "mesh.scm")

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

(define circle-mesh (disk 100))

(define the-hex (disk 6))

(define (render-mesh mesh program mvp energy)
  (gl:use-program program)
  (gl:bind-vertex-array 0)

  ;; set shader parameters
  (let ([id (gl:get-uniform-location program "ENERGY")])
    (when (> id -1)
      (gl:uniform1f id energy)))

  ;; set MVP matrix
  (gl:uniform-matrix4fv (gl:get-uniform-location program "MVP")
			1 #f
			mvp)
  ;; render mesh
  (gl:bind-vertex-array (mesh-vao mesh))
  (gl:draw-elements-base-vertex (mode->gl (mesh-mode mesh))
				(mesh-n-indices mesh)
				(type->gl (mesh-index-type mesh))
				#f 0))


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

    (list rendered-texture depthrenderbuffer)
    )


  )

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

   (set-box! program1 (set-shaders! (unbox v1) *fragment*))
   (set-box! program2 (set-shaders! (unbox v2) *fragment*))
   (set-box! program3 (set-shaders! (unbox v3) *fragment*))

   ;; create vertex array object for mesh
   (mesh-make-vao! circle-mesh `((position . ,(gl:get-attrib-location (box-ref program1) "position"))
				 (color . ,(gl:get-attrib-location (box-ref program1) "color"))))

   (let loop ([i 0]
	      [t (current-milliseconds)])
     (REPL) ; process repl event
     (cp-space-step (unbox the-space) 1/60 ) ;; TODO use delta time
     (cp-space-step (unbox the-space) 1/60 ) ;; TODO use delta time
     (cp-space-step (unbox the-space) 1/60 ) ;; TODO use delta time
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
				   (box-set! v1 (read-all f))
				   (set-box! program1 (set-shaders! (read-all f) *fragment*))
				   (read-all f)
				   (newline)))))

(define watcher-2 (watch-reload! "vertex-shaders/v2.glsl"
			       (lambda (f)
				 (when f
				   (display "Updated") (display f) (newline)
				   (box-set! v2 (read-all f))
				   (set-box! program2 (set-shaders! (read-all f) *fragment*))
				   (read-all f)
				   (newline)))))

(define watcher-3 (watch-reload! "vertex-shaders/v3.glsl"
			       (lambda (f)
				 (when f
				   (display "Updated") (display f) (newline)
				   (box-set! v3 (read-all f))
				   (set-box! program3 (set-shaders! (read-all f) *fragment*))
				   (read-all f)
				   (newline)))))

;(thread-join! main-thread)

;(main)
