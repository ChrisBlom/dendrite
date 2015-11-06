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

(define program (make-box #f))
(define program2 (make-box #f))
(define program3 (make-box #f))

(define (ball->node idx ball)
  (let* ([body (alist-ref 'body ball)]
	 [programs (vector program program2 program3)])
    (make-node (lambda (node projection-matrix view-matrix)
		 (let ([angle (- (cp-body-get-angle body))]
		       [body-pos (cp-body-get-position body)])
		   (render-shape the-shape
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

(define (add-ball space x y #!key (elasticity 0.95) (friction 0.2) (mass 1.) (radius 0.2))
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

(define (fixed-line-segment space from to)
  (let ([radius 0.1])
    (cp-segment-shape-new (cp-space-get-static-body space) from to radius)))

(define (v.y cpv)
  (f64vector-ref cpv 1))

;; -1 -1  ------- +1 +1
;;
;;
;; -1 -1 ------- +1 -1
(define the-balls (box #f))

(define the-nodes
  (box (list)))

(define the-mouse-ball (box #f))

(define (init-physics)
  (let ([space (cp-space-new)])
    (cp-space-add-shapes space
     (doto (fixed-line-segment space (cp-v -5. -5.) (cp-v -5. +5.))
	   (cp-shape-set-elasticity 0.95))
     (doto (fixed-line-segment space (cp-v -5. +5.) (cp-v +5. +5.))
	   (cp-shape-set-elasticity 0.95))
     (doto (fixed-line-segment space (cp-v +5. +5.) (cp-v +5. -5.))
	   (cp-shape-set-elasticity 0.95))

     (doto (fixed-line-segment space (cp-v +5. -5.) (cp-v -5. -5.))
	   (cp-shape-set-elasticity 0.95))

     ;; diag
     ;; (doto (fixed-line-segment space (cp-v +5. +5.) (cp-v -5. -5.))
     ;; 	   (cp-shape-set-elasticity 0.95))
     ;; (doto (fixed-line-segment space (cp-v -5. +5.) (cp-v +5. -5.))
     ;; 	   (cp-shape-set-elasticity 0.95))
     )

    (cp-space-set-gravity space (cp-v 0. 9.8))

    ;; (cp-space-add-constraint
    ;;  the-space
    ;;  (damped-rotary-spring
    ;;   (alist-ref 'body (first (unbox the-balls)))
    ;;   (alist-ref 'body (second (unbox the-balls)))))

    (box-set! the-mouse-ball (add-ball space 0. 0. radius: 1.))

    (box-set! the-space space)
    (box-set! the-balls (cons (unbox the-mouse-ball)
			      (let ([n 1000])
				(map (lambda (i)
				       (let ([angle (/ (* pi 2 i 8) n)]
					     [radius (* 4 (/ i n))])
					 (add-ball space
						   (* radius (sin angle))
						   (* radius (cos angle)))))
				     (range 0 n)))))

    (box-set! the-nodes (map-indexed ball->node (box-ref the-balls)))))


(init-physics)


;(cp-space-set-gravity (unbox the-space) (cp-v 1 9.8))


;; (for-each
;;  (cut cp-shape-set-elasticity <> 0.1)
;;  (cp-space-shapes (unbox the-space)))

;; (cp-space-bodies the-space)


(define (clear-space space)

  (for-each (cut cp-space-remove-body the-space <>) (cp-space-bodies the-space))

  )






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

(define (circle-positions n)
  (apply append (cons '(0 0)
		      (map (lambda (i)
			     (let ([angle (/ (* pi 2 i) n)]
				   [radius 0.2])
			       (list (* radius (sin angle))
				     (* radius (cos angle)))))
			   (range 0 n)))))

(define (circle-colors n)
  (apply append (cons '(255 0 255)
		      (map (lambda (i)
			     (let ([h (% i 2)])
			       (map (compose (cut * <> 255.))
				    (list h
					  h
					  h))))
			   (range 0 n 1)))))

(define (circle-indices n)
  (apply append
	 (map (lambda (i) (list 0
			   (inc i)
			   (inc (modulo (inc i) n))))
		     (range 0 n))))

(define hexagon-positions (circle-positions 6))

(define hexagon-colors (circle-colors 6))

(define hexagon-indices
  (apply append '((0 1 2)
		  (0 2 3)
		  (0 3 4)
		  (0 4 5)
		  (0 5 6)
		  (0 6 1))))

;;
;;      1_________2
;;      /         \
;;     /           \
;;  6 /     0.      \3
;;    \            /
;;     \          /
;;      \________/
;;     5          4
(define hex (make-mesh
              vertices: `(attributes: ((position #:float 2)
                                       (color #:unsigned-byte 3
					      normalized: #t))
                          initial-elements: ((position . ,hexagon-positions)
                                             (color . ,hexagon-colors)))
              indices: `(type: #:ushort
			       initial-elements: ,hexagon-indices)))

(define (disk n)
  (make-mesh
   vertices: `(attributes: ((position #:float 2)
			    (color #:unsigned-byte 3
				   normalized: #t))
			   initial-elements: ((position . ,(circle-positions n))
					      (color . ,(circle-colors n))))
   indices: `(type: #:ushort
		    initial-elements: ,(circle-indices n))))

;;   0     ->              1
;;  -1,-1               1,-1
;;     _________________
;;    |                |
;;    |                |
;;    |                |
;;    |                |   |
;;    |                |   v
;;    |                |
;;    |                |
;;    |                |
;;    |________________|
;;  -1,1     <-          1,1 2					;
;; 3
(define rect (make-mesh
              vertices: '(attributes: ((position #:float 2)
                                       (color #:float 3
                                              normalized: #t))
                          initial-elements: ((position . (-1 -1
							  +1 -1
                                                          +1 +1
                                                          -1 +1))
                                             (color . (255 255 0
                                                       0   255 0
                                                       0   0   255
                                                       255 0   255))))
              indices: '(type: #:ushort
                         initial-elements: (0 1 2
					      0 2 3))))


(define (projection-matrix)
  (perspective 800 600 0.1 100 70))



(define d (make-box 5.0))
(define r (make-box 0.0))

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

(define (render-shape shape program mvp energy)
  (gl:use-program program)
  (gl:bind-vertex-array 0)
  (let ([id (gl:get-uniform-location program "ENERGY")])
    (when (> id -1)
      (gl:uniform1f id energy)))

  ;; set MVP matrix
  (gl:uniform-matrix4fv (gl:get-uniform-location program "MVP")
			1 #f
			mvp)
  (gl:bind-vertex-array (mesh-vao shape))
  (gl:draw-elements-base-vertex (mode->gl (mesh-mode shape))
				(mesh-n-indices shape)
				(type->gl (mesh-index-type shape))
				#f 0))

(define the-shape (disk 100))
(define the-hex (disk 6))

(define (render nodes)
  (let ([projection (projection-matrix)]
	[view (view-matrix)])
    ;; shaders

    (for-each (lambda (node)
		(let ([render-fn (node-render-fn node)])
		  (render-fn node
			     projection
			     view
			     )))
      nodes)

        ;; draw shape
    (gl:draw-elements-base-vertex (mode->gl (mesh-mode the-shape))
    				  (mesh-n-indices the-shape)
    				  (type->gl (mesh-index-type the-shape))
    				  #f 0)

    (check-error)))

(glfw:key-callback (lambda (window key scancode action mods)
		     (display (list 'key= key scancode action mods)) (newline)
                     (cond
                      [(and (eq? key glfw:+key-escape+) (eq? action glfw:+press+))
                       (glfw:set-window-should-close window #t)])))

(define mouse-pos (box (cons 0 0)))

(define (screen->world xy)
  (let* ([x (car xy)]
	 [y (cdr xy)]
	 [v (view-matrix)]
	 [p (projection-matrix)]
	 [world->screen (m* v p)]
	 [m-screen->world (inverse world->screen)]
	 [screen-pos (make-point x y 0)])
    (m*vector! m-screen->world (make-point x y 0))))

(glfw:cursor-position-callback (lambda (window x y)
				 (box-set! mouse-pos (cons (+ (- (/ x 2)) 200)
							   (+ (- (/ y 2)) 150)))
				 (display (list 'cursor= x y)) (newline)
				 (display (list 'cursor-world= (screen->world (cons x y)))) (newline)))


(define (main)
  (glfw:with-window (800 600 "Example"
			 resizable: #f
			 context-version-major: 3
			 context-version-minor: 2
			 opengl-forward-compat: #t
			 opengl-profile: glfw:+opengl-core-profile+)
   (gl:init)
   ;; (print (:supported? "GL_ARB_framebuffer_object"))

   (opengl:gl:Enable gl:+blend+)
   (opengl:gl:BlendFunc gl:+src-alpha+ gl:+one-minus-src-alpha+ )


   ;(set! repl-thread (thread-start! (make-thread repl)))

   (set-box! program (set-shaders! (unbox v1) *fragment*))
   (set-box! program2 (set-shaders! (unbox v2) *fragment*))
   (set-box! program3 (set-shaders! (unbox v3) *fragment*))

   ;; create shape vertex array opbject
   (mesh-make-vao! the-shape `((position . ,(gl:get-attrib-location
					(box-ref program) "position"))
			       (color . ,(gl:get-attrib-location
				     (box-ref program) "color"))))

   (let loop ([i 0])
     (REPL) ; process repl event
     (cp-space-step (unbox the-space) 1/60 ) ;; TODO use delta time

     (let* ([p (screen->world (unbox mouse-pos))]
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
       (box-swap! r + 0.01)
       (loop (+ .01 i))))))


;;(use nrepl)
;(define nrepl-thread (thread-start! (lambda () (nrepl 1234))))

(define main-thread (thread-start! (make-thread main)))

;(define repl-thread (thread-start! (make-thread repl)))

(define (ch)
  (use box)
  (set-box! program (set-shaders! *vertex2* *fragment*)))






(define watcher-1 (watch-reload! "vertex-shaders/v1.glsl"
			       (lambda (f)
				 (when f
				   (display "Updated") (display f) (newline)
				   (box-set! v1 (read-all f))
				   (set-box! program (set-shaders! (read-all f) *fragment*))
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
;(repl)



;; run (use box) again in the repl
