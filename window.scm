(import chicken scheme)
(use (prefix glfw3 glfw:)
     (prefix opengl-glew gl:)
     gl-math
     gl-utils
     srfi-18 ; threads
     box)

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
  fragColor = vec4(c, 1.0);
}
END
)

(define rect (make-mesh
              vertices: '(attributes: ((position #:float 2)
                                       (color #:unsigned-byte 3
                                              normalized: #t))
                          initial-elements: ((position . (-1 -1
                                                           1 -1
                                                           1  1
                                                           -1  1))
                                             (color . (255 0   0
                                                       0   255 0
                                                       0   0   255
                                                       255 0   255))))
              indices: '(type: #:ushort
                         initial-elements: (0 1 2
                                            0 2 3))))

(define program (make-parameter #f))

(define projection-matrix
  (perspective 640 480 0.1 100 70))

(define d (make-box 5.0))
(define r (make-box 0.0))

(define (eye)
  (make-point (* (box-ref d) (sin (box-ref r)))
	      0
	      (* (box-ref d) (cos (box-ref r)))))

(define (view-matrix)
  (look-at (eye)
           (make-point 0 0 0)
           (make-point 0 2 0)))

(define model-matrix (mat4-identity))

(define (render)
  (gl:use-program (program))
  (gl:bind-vertex-array 0)
  (gl:uniform-matrix4fv (gl:get-uniform-location (program) "MVP")
                        1 #f
                        (m* projection-matrix
                            (m* (view-matrix)
				model-matrix)))
  (gl:bind-vertex-array (mesh-vao rect))
  (gl:draw-elements-base-vertex (mode->gl (mesh-mode rect))
                                (mesh-n-indices rect)
                                (type->gl (mesh-index-type rect))
                                #f 0)
  (check-error))

(glfw:key-callback (lambda (window key scancode action mods)
                     (cond
                      [(and (eq? key glfw:+key-escape+) (eq? action glfw:+press+))
                       (glfw:set-window-should-close window #t)])))

(define (main)
  (glfw:with-window
   (640 480 "Example"
	resizable: #f
	context-version-major: 3
	context-version-minor: 2
	opengl-forward-compat: #t
	opengl-profile: glfw:+opengl-core-profile+
	)
   (gl:init)
					; (print (gl:supported? "GL_ARB_framebuffer_object"))
   (set! *vertex* (make-shader gl:+vertex-shader+ *vertex*))
   (set! *fragment* (make-shader gl:+fragment-shader+ *fragment*))
   (program (make-program (list *vertex* *fragment*)))
   (mesh-make-vao! rect `((position . ,(gl:get-attrib-location
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

(define main-thread (thread-start! (make-thread (lambda () (print 'main) (main)))))
(repl)

;; run (use box) again in the repl
