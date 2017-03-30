(import chicken scheme)

(use (prefix glfw3 glfw:)
     (prefix opengl-glew gl:)
     (prefix chipmunk cp:)
     parley
     gl-math
     gl-utils
     srfi-4 ; vectors
     srfi-18 ; green threads
     srfi-42 ; eager comprehension
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

(include "mesh.scm")
(include "utils.scm")
(include "chipmunk-utils.scm")
(include "reactive.scm")
(include "pipeline.scm")
(include "nodes.scm")
(include "scene.scm")
(include "render.scm")

(when (unbound? 'REPL)
  (define repl-port 6062)
  (define REPL (make-prepl repl-port)))

;;;; Globals

(define the-space #f)
(define the-mouse-v (make-parameter (cp:v 0 0)))
(define the-mouse-ball (make-parameter #f))

(define -run-physics- (make-parameter #t))

(define root-node (new-node #f))

(define interaction-node (new-node root-node))
;;;; Scene Setup

(define (add-ball parent-node space x y idx
		  #!key
		  (elasticity 0.95)
		  (friction 0.2)
		  (mass 1.)
		  (radius 0.1)
		  (velocity #f)
		  (color (vector 0 1 1)))
  (let* ((moment (cp:moment-for-circle mass 0. radius cp:v0))
	 (body (cp:body-new  mass moment))
	 (shape (cp:circle-shape-new body radius cp:v0)))

    (set! (cp:shape-collision-type shape) 1)
    (set! (cp:body-position body) (cp:v (exact->inexact x) (exact->inexact y)))

    (when velocity
      (set! (cp:body-velocity body) velocity))
    (cp:space-add-body space body)

    (set! (cp:shape-friction shape) friction)
    (set! (cp:shape-elasticity shape) elasticity)

    (cp:space-add-shape space shape)

    (new-node parent-node
	      #:render (render-ball idx)
	      #:id idx
	      #:body body
	      #:shape shape
	      #:color color)))

;;;; Physics

(define (init-physics)
  (load-scene "1"))

(init-physics)

(comment

 (load-scene "2")

 (first loaded-scene)

 (unload)

; (add-ball (cadr loaded-scene) the-space 10. 10. 0 #:radius 0.2 #:friction 0.01)

 )

(define (add-node-at-pos)
  (let ([m (the-mouse-v)])
    (add-ball interaction-node the-space (cp:v.x m) (cp:v.y m) (next-id)
	      #:radius 0.2
	      #:velocity (v-rand 10))
    m))

;;;; Graphics

;;; Camera

;(define projection-matrix (make-parameter (perspective 600 600 1 10 70)))

(define projection-matrix (make-parameter (ortho 10 10  0.2 100)))

(define the-eye-point (make-parameter (make-point 0 0 5)))

(the-eye-point (make-point 0 0 1))

(define the-object-point (make-parameter (make-point 0 0 0)))

(the-object-point (make-point 0 0 0))

(define view-matrix
  (make-parameter (look-at (the-eye-point)
			   (the-object-point)
			   (make-point 0 1 0)))) ; up vector

(view-matrix
 (look-at (make-point 0 0 10)
	  (make-point 0 0 0)
	  (make-point 0 1 0)))

(define (model-matrix)
  (mat4-identity))

;;;  Meshes


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
	   (when (-run-physics-)
	     (cp:space-step the-space 1/30))

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





(comment

 (remove-node (the-mouse-ball))


 (define ff (cp:shape-filter-new (cp:uint 1) cp:all-categories cp:all-categories))

 (let ([p (cp:space-point-query-nearest the-space (the-mouse-v) 4 ff #f)])

   (node-color-set! (first (filter
			    (lambda (x) (equal? (node-shape x) p))
			    all-nodes))
		    (vector 1 1 1))

   (first (filter
	   (lambda (x) (equal? (node-shape x) p))
	   all-nodes)))

 (node-color
  (first (filter (lambda (x) (equal? (node-shape x) p)) all-nodes)))

 (filter
  (o (cut equal? p <>) node-shape)
  (node-descendants root-node))

 (filter
  (cut equal? p <>)
  (cp:space-shapes the-space))

)
(comment
 ;; reset root node
 (node-children-set! root-node '()))
