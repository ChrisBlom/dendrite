(import chicken scheme)

(use (prefix glfw3 glfw:)
     (prefix opengl-glew gl:)
     gl-math
     gl-utils
     srfi-18 ; threads
     srfi-42 ; eager comprehension
     box
     nrepl)

(define (range init limit #!optional [step 1])
  (if (>= init limit)
      '()
      (cons init (range (+ init step) limit step ))))

(define (inc i) (+ 1 i))

(define *vertex*
#<<END
#version 330
in vec2 position;
in vec3 color;
out vec3 c;
uniform mat4 MVP;

void main(){
   gl_Position = MVP * vec4(position, 0.0, 1.0);
   c = color;
}
END
)

(define *fragment*
#<<END
#version 330
in vec3 c;
out vec4 fragColor;
void main(){
  fragColor = vec4(c, 0.5);
}
END
)

(define (circle-positions n)
  (apply append (cons '(0 0)
		      (map (lambda (i)
			     (let ([angle (/ (* pi 2 i) n)])
			       (list (sin angle) (cos angle))))
			   (range 0 n)))))

(define (circle-colors n)
  (apply append (cons '(255 0 255)
		      (map (lambda (i)
			     (let ([h (modulo i 2)])
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
                                                           1 -1
                                                           1  1
                                                           -1  1

							   ))
                                             (color . (255 255 0
                                                       0   255 0
                                                       0   0   255
                                                       255 0   255))))
              indices: '(type: #:ushort
                         initial-elements: (0 1 2
					      0 2 3))))

(define program (make-parameter #f))

(define (projection-matrix)
  (perspective 800 600 0.1 100 70))

(define d (make-box 5.0))

(define r (make-box 0.0))

(define (eye)
  (make-point 0 ;(* (box-ref d) (sin (box-ref r)))
	      0
	      10; (* (box-ref d) (cos (box-ref r)))
	      ))

(define (view-matrix)
  (look-at (eye)
           (make-point 0 0 0)
           (make-point 0 2 0)))

(define (model-matrix) (mat4-identity))

(define shape (disk 100))


(define (render)
  (let ([mvp (m* (projection-matrix)
		 (m* (view-matrix)
		     (model-matrix)))]

	[mvp2 (m* (projection-matrix)
		  (m* (view-matrix)
		      (translate (make-point (sin (box-ref r))
					     (cos (box-ref r))
					     0)
				 (model-matrix))))]
	)
    ;; shaders
    (gl:use-program (program))


    (gl:bind-vertex-array 0)
    (gl:uniform-matrix4fv (gl:get-uniform-location (program) "MVP")
			  1 #f
			  mvp)
    (gl:bind-vertex-array (mesh-vao shape))
    (gl:draw-elements-base-vertex (mode->gl (mesh-mode shape))
				  (mesh-n-indices shape)
				  (type->gl (mesh-index-type shape))
				  #f 0)

    (gl:bind-vertex-array 0)
    (gl:uniform-matrix4fv (gl:get-uniform-location (program) "MVP")
    			  1 #f
    			  mvp2)
    (gl:bind-vertex-array (mesh-vao shape))

    ;; draw shape
    (gl:draw-elements-base-vertex (mode->gl (mesh-mode shape))
    				  (mesh-n-indices shape)
    				  (type->gl (mesh-index-type shape))
    				  #f 0)

    (check-error)))

(glfw:key-callback (lambda (window key scancode action mods)
                     (cond
                      [(and (eq? key glfw:+key-escape+) (eq? action glfw:+press+))
                       (glfw:set-window-should-close window #t)])))

(define (main)
  (glfw:with-window (800 600 "Example"
			 resizable: #f
			 context-version-major: 3
			 context-version-minor: 2
			 opengl-forward-compat: #t
			 opengl-profile: glfw:+opengl-core-profile+)
   (gl:init)
   ;; (print (gl:supported? "GL_ARB_framebuffer_object"))

   (set! *vertex* (make-shader gl:+vertex-shader+ *vertex*))

   (set! *fragment* (make-shader gl:+fragment-shader+ *fragment*))

   ;; compile shader, set program parameter
   (program (make-program (list *vertex* *fragment*)))

   ;; create shape vertex array opbject
   (mesh-make-vao! shape `((position . ,(gl:get-attrib-location
					(program) "position"))
			  (color . ,(gl:get-attrib-location
				     (program) "color"))))

   (let loop ([i 0])
     (glfw:swap-buffers (glfw:window))
     (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
     (render)
     (glfw:poll-events) ; Because of the context version, initializing GLEW results in a harmless invalid enum
     (thread-yield!)
     (unless (glfw:window-should-close (glfw:window))
       (box-swap! r + 0.01)
       (loop (+ .01 i))))))


;(use nrepl)
;(define nrepl-thread (thread-start! (lambda () (nrepl 1234))))

(define main-thread (thread-start! (make-thread (lambda () (print 'main) (main)))))

(repl)


;; run (use box) again in the repl
