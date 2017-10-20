(module main *
  (import chicken scheme srfi-1 srfi-42 foreign srfi-13)
  (use (prefix glfw3 glfw:)
       (prefix opengl-glew gl:)
       (prefix chipmunk cp:)
					;     parley
       gl-math
       gl-utils
       srfi-4 ; vectors
       srfi-18 ; green threads
					;     nrepl
       clojurian-syntax
       synth-utils
       prepl
       utils
       posix
       ringbuffer
       matchable
       extras
       symbol-utils


       global
       nodes
       render
       camera
       pipeline
       reactive
       mesh
       scene
       chipmunk-utils)

  (define (repeat-list n x)
  (if (positive? n)
    (cons x (repeat-list (- n 1) x))
    '()))

;;;;; Utils ;;;;;

(when (unbound? 'REPL)
  (define repl-port 6062)
  (define REPL (make-prepl repl-port)))

;; (use nrepl)
;; (when (unbound? 'NREPL)
;;   (define nrepl-port 6060)
;;   (define NREPL (thread-start! (lambda () (nrepl nrepl-port)))))

;;;; Globals


(define (toggle-physics)
  (-run-physics- (not (-run-physics-))))


(define (in)
  (define event #f)

 ;;  (define begin-func (lambda (space arbiter data)
;; 		       (set! event 'begin ) #t))
;; ;
  (define presolve-func (lambda (space arbiter data)
			  (set! event 'presolve ) 1))

  (define postsolve-func (lambda (space arbiter data)
			   (set! event 'postsolve)))

  (define separate-func (lambda (space arbiter data)
			  (set! event 'separate)))

  ;; (define dispatch-data-tuple (cp:new-gc-root
  ;; 			       (list (lambda (type)
  ;; 				       (print 'dispatch type)
  ;; 				       (case type
  ;; 					 ((begin-func) begin-func)
  ;; 					 ((presolve-func) presolve-func)
  ;; 					 ((postsolve-func) postsolve-func)
  ;; 					 ((separate-func) separate-func)
  ;; 					 (else (error "internal error dispatching callback function."))))
  ;; 				     (list))))

  (define handler (cp:space-add-default-collision-handler the-space))

  `(handler ,handler)

;  (cp:collision-handler-user-data-set! handler dispatch-data-tuple)
  (cp:collision-handler-begin-func-init! handler)
)

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

   (let ([s (string-trim-both (read-all "INIT_SCENE"))])
     (println "Loading scene" s)
     (load-scene s))
   ;;(in)
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

(define step (make-parameter 1/60))


(on-frame (lambda (dt)

 	   ;; ;; process repl event
	   ;; (REPL)

	   ;; update physics
	   (when (-run-physics-) (cp:space-step the-space (step)))

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
;(main)

;; (let* ((old (current-input-port))
;;        (parley-input-port (make-parley-port old)))
;;   (current-input-port parley-input-port))

					;(cp:collision-handler-presolve-func-set! handler cp:_collision_presolve_bridge)
					;(cp:collision-handler-postsolve-func-set! handler cp:_collision_postsolve_bridge)


(comment

 (when begin-func (set! (collision-handler-begin-func handler) #$_collision_begin_bridge))
 (when presolve-func (set! (collision-handler-pre-solve-func handler) #$_collision_presolve_bridge))
 (when postsolve-func (set! (collision-handler-post-solve-func handler) #$_collision_postsolve_bridge))
 (when separate-func (set! (collision-handler-separate-func handler) #$_collision_separate_bridge))
					;(set! (collision-handler-user-data handler) dispatch-data-tuple)
 (cons handler dispatch-data-tuple))

;; (cp:xspace-add-collision-handler the-space
;; 				 #:collision-type-a cp:wildcard-collision-type
;; 				 #:collision-type-b cp:wildcard-collision-type
;; 				 #:data 0)

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

; (define h (cp:space-add-collision-handler the-space 0 0 ))

 (define hdbg #f)





)
(comment
 ;; reset root node
 (node-children-set! root-node '()))
)
