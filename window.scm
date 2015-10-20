(import chicken scheme)

(use (prefix glfw3 glfw:)
     (prefix opengl-glew gl:)
     gl-math
     gl-utils
     srfi-18 ; threads
     srfi-42 ; eager comprehension
     box ; mutable box
     nrepl)

;(require-extension sequence-comprehensions)

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
   float d = sqrt ( (position.x * position.x) + (position.y * position.y) ) ;
   float r = atan(position.x , position.y);
   float v = max(d ,d+ sin(r*8));
   c = vec3(v,v,v)		   ;
}
END
)

(define *vertex2*
#<<END
#version 330
in vec2 position;
in vec3 color;
out vec3 c;
uniform mat4 MVP;

void main(){
   gl_Position = MVP * vec4(position, 0.0, 1.0);
   float d = sqrt ( (position.x * position.x) + (position.y * position.y) ) ;
   float r = atan(position.x , position.y);
   float v = 1.0 - d;
   c = vec3(v+ sin(r*20),v,v)		;
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
  (look-at (eye) ; eye
           (make-point 0 0 0) ; object
           (make-point 0 1 0) ; up vector
	   ))

(define (model-matrix)
  (mat4-identity))


(define-record node
  render-fn ; takes node , mvp ,
)

(define program (make-box #f))
(define program2 (make-box #f))

(define (render-shape shape program mvp)
  (gl:use-program program)
  (gl:bind-vertex-array 0)
  (gl:uniform-matrix4fv (gl:get-uniform-location program "MVP")
			1 #f
			mvp)
  (gl:bind-vertex-array (mesh-vao shape))
  (gl:draw-elements-base-vertex (mode->gl (mesh-mode shape))
				(mesh-n-indices shape)
				(type->gl (mesh-index-type shape))
				#f 0))

(define the-nodes
  (box (list)))

(define the-shape (disk 100))
(define the-hex (disk 6))


(define (render nodes)
  (let ([mvp (m* (projection-matrix)
		 (m* (view-matrix)
		     (model-matrix)))]

	[mvp2 (m* (projection-matrix)
		  (m*
		   (translation (make-point (* 2 (sin (box-ref r)))
					    (* 2 (cos (box-ref r)))
					    0))
		   (m* (view-matrix)
		       (model-matrix))))]
	[mvp3 (m* (projection-matrix)
		  (m*
		   (translation (make-point (* 2 (cos (box-ref r)))
					    (* 2 (sin (box-ref r)))
					    0))
		   (m* (view-matrix)
		       (model-matrix))))])
    ;; shaders

    (render-shape the-shape (box-ref program) mvp)


    (render-shape the-shape  (box-ref program2) mvp2)
;; ;

    (for-each (lambda (node)
		(let ([render-fn (node-render-fn node)])
		  (render-fn node mvp3)))
      nodes)

        ;; draw shape
    (gl:draw-elements-base-vertex (mode->gl (mesh-mode the-shape))
    				  (mesh-n-indices the-shape)
    				  (type->gl (mesh-index-type the-shape))
    				  #f 0)


    (check-error)))

(glfw:key-callback (lambda (window key scancode action mods)
                     (cond
                      [(and (eq? key glfw:+key-escape+) (eq? action glfw:+press+))
                       (glfw:set-window-should-close window #t)])))

(define (set-shaders! vertex-string fragment-string)
  (let ([vertex-shader-id   (make-shader gl:+vertex-shader+ vertex-string)]
	[fragment-shader-id (make-shader gl:+fragment-shader+ fragment-string)])
    ;; compile shader, set program parameter using
    (make-program (list vertex-shader-id fragment-shader-id))))

(define (main)
  (glfw:with-window (800 600 "Example"
			 resizable: #f
			 context-version-major: 3
			 context-version-minor: 2
			 opengl-forward-compat: #t
			 opengl-profile: glfw:+opengl-core-profile+)
   (gl:init)
   ;; (print (:supported? "GL_ARB_framebuffer_object"))

   ;(set! repl-thread (thread-start! (make-thread repl)))

   (set-box! program (set-shaders! *vertex* *fragment*))
   (set-box! program2 (set-shaders! *vertex2* *fragment*))

   ;; create shape vertex array opbject
   (mesh-make-vao! the-shape `((position . ,(gl:get-attrib-location
					(box-ref program) "position"))
			       (color . ,(gl:get-attrib-location
				     (box-ref program) "color"))))

   (box-swap! the-nodes (lambda [l]
			  (cons
			   (make-node (lambda (node mvp)
					(gl:use-program (box-ref program))
					(render-shape the-shape (box-ref program) mvp)
					node;;
					))
			   l)))


   (let loop ([i 0])
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

(define repl-thread (thread-start! (make-thread repl)))

(define (ch)
  (use box)
  (set-box! program (set-shaders! *vertex2* *fragment*)))

(thread-join! main-thread)

;; run (use box) again in the repl
