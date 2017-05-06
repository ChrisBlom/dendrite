(import chicken scheme srfi-42 synth-utils)

(import-for-syntax srfi-42 synth-utils)

(use (prefix glfw3 glfw:)
     (prefix opengl-glew gl:)
     (prefix chipmunk cp:)
     parley
     gl-math
     gl-utils
     srfi-4 ; vectors
     srfi-18 ; green threads
     nrepl
     clojurian-syntax
     prepl
     utils
     symbol-utils
     posix
     ringbuffer
     matchable
     extras)

(define (repeat-list n x)
  (if (positive? n)
    (cons x (repeat-list (- n 1) x))
    '()))

;;;;; Utils ;;;;;

(include "mesh.scm")
(include "chipmunk-utils.scm")
(include "reactive.scm")
(include "pipeline.scm")
(include "nodes.scm")
(include "scene.scm")
(include "render.scm")

(when (unbound? 'REPL)
  (define repl-port 6062)
  (define REPL (make-prepl repl-port)))

;; (use nrepl)
;; (when (unbound? 'NREPL)
;;   (define nrepl-port 6060)
;;   (define NREPL (thread-start! (lambda () (nrepl nrepl-port)))))

;;;; Globals

(define the-space #f)
(define the-mouse-v (make-parameter (cp:v 0 0)))
(define the-mouse-ball (make-parameter #f))
(define -run-physics- (make-parameter #t))

(define on-frame (make-parameter #f))

(define (toggle-physics)
  (-run-physics- (not (-run-physics-))))

(define root-node (new-node #f))

(define interaction-node (new-node root-node))

(define (add-node-at-pos)
  (let ([m (the-mouse-v)])
    (add-ball interaction-node the-space (cp:v.x m) (cp:v.y m) (next-id)
	      #:radius 0.2
	      #:velocity (v-rand 10)
	      #:color (vector
		       (/ (random 255) 255.0)
		       (/ (random 255) 255.0)
		       (/ (random 255) 255.0))
	      )
    m))

(include "camera.scm")
(include "input.scm")

(define (main)

  (glfw:window-hint glfw:+samples+ 4)

  (glfw:with-window
   (600 600 "Dendrite"
	resizable: #f
	context-version-major: 3
	context-version-minor: 2
	opengl-forward-compat: #t
	opengl-profile: glfw:+opengl-core-profile+)

   (gl:init)
   (gl:enable gl:+multisample+)
   (gl:enable gl:+blend+)
   (gl:blend-func gl:+src-alpha+ gl:+one-minus-src-alpha+ )

   (start-all-cells)

   (load-scene (string-trim-both (read-all "SCENE")))

   ;(gl:bind-texture gl:+texture-2d+ (cell-get noise-image-texture))
 ;  (gl:tex-parameteri gl:+texture-2d+ gl:+texture-min-filter+ gl:+linear+)

   ;; create vertex array object for mesh
   (mesh-init-vaos)

   ;; main loop
   (let loop ([i 0]
	      [pt (current-milliseconds)])
     (let* ([now (current-milliseconds)]
	    [dt (- now pt)]
	    [on-frame* (on-frame)])
       (when on-frame* (on-frame* dt)) ;; OPTIMIZE only unbox every x ms
       (unless (glfw:window-should-close (glfw:window))
	 (loop (+ 1 i) now))))))

(define fps-buffer (new-ringbuffer 100))
(define fps-sum 0)
(define (update-fps dt)
  (ringbuffer-advance fps-buffer )
  (let ([oldest (ringbuffer-get fps-buffer -1)])
    (ringbuffer-set! fps-buffer 0 dt)
    (set! fps-sum (+ fps-sum dt (- oldest)))))
(define (fps)
  (/ 1000 fps-sum (- (ringbuffer-length fps-buffer) 1)))

(on-frame (lambda (dt)

 	   ;; process repl event
	   (REPL)

	   ;; update physics
	   (when (-run-physics-) (cp:space-step the-space 1/30))

	   ;; draw all nodes
	   (glfw:swap-buffers (glfw:window))
	   (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
	   (render-node root-node
			(projection-matrix)
			(view-matrix)
			(m*s (mat4-identity) 2))

	   (update-fps dt)

	   ;; after render
	   (check-error)
	   (glfw:poll-events) ; Because of the context version, initializing GLEW results in a harmless invalid

	   (thread-yield!)))

(define main-thread (thread-start! (make-thread main)))

(let* ((old (current-input-port))
       (parley-input-port (make-parley-port old)))
  (current-input-port parley-input-port))

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
