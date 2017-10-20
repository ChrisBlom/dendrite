(module input *
  (import scheme chicken srfi-1 srfi-4)
  (use (prefix glfw3 glfw:)
       (prefix chipmunk cp:)
       gl-math
       gl-utils
       synth-utils

       scene
       nodes
       camera
       global)

  ;; Key handler
  (define key-handler (make-parameter
		       (lambda (a . args)
			 (display 'no-keyhandler))))

  (glfw:key-callback (lambda (window key scancode action mods)
		       ;; (print 'key window key scancode action mods)
		       (when (and (eq? key glfw:+key-escape+) (eq? action glfw:+press+))
			 (glfw:set-window-should-close window #t))
		       ;; delegate to key-handler
		       ((key-handler) window key scancode action mods)))

  ;; Mouse handler
  (define mouse-position-handler (make-parameter (lambda (a . args)
						   (display 'no-curser-position-handler))))

  (glfw:cursor-position-callback
   (lambda (window x y)
     (let [(orig (current-exception-handler))]
       ((mouse-position-handler) window x y))))

  ;; Mouse button handler
  (define mouse-button-handler
    (make-parameter (lambda (a . args)
		      (display 'no-mouse-button-handler))))

  (glfw:mouse-button-callback
   (lambda (window button action mods)
     ((mouse-button-handler) window button action mods)))

  ;; Assignment of handlers

  (define the-mouse-pos (make-parameter (cons 10. 10.)))

  (define (screen->world screen-point)
    (let* ([v (view-matrix)]
	   [p (projection-matrix)]
	   [world->screen (m* v p)]
	   [ivp (make-f32vector 16)])
      (inverse world->screen ivp)
      (m* ivp screen-point)))

  (define (window->screen window-point)
    (v- (v* window-point 2/600)
	(make-point 1 1 0)))

  ;; TODO to coordinate transformation properly
  (mouse-position-handler
   (lambda (window xx yy)
     (let* ([window-point (make-point xx yy 0)]
	    [screen-point (window->screen window-point)]
	    [world-pos (screen->world screen-point)]
	    [x (point-x world-pos)]
	    [y (- (point-y world-pos))])
       (comment (./trace window-point
			 screen-point
			 world-pos))
       (the-mouse-v (cp:v x y)))))

  (define ff (cp:shape-filter-new (cp:uint 2) cp:all-categories cp:all-categories))

  (define prev-event '(none #f))

  (define cur-event '(none #f) )

  (define modes '(spring line ball box impulse force))

  (define (rotate-left xs)
    (if (null? xs)
	'()
	(append (cdr xs)
		(cons (car xs)
		      '()))))

  (define (cycle-mode)
    (set! modes (rotate-left modes))
    (println "mode:" (car modes))
    modes)

  ;; set key handler
  (key-handler
   (lambda (window key scancode action mods)
     (when (or (equal? action glfw:+press+)
	       (equal? action glfw:+repeat+))
       ;; (print "input:" key)
       (condp equal? key
	      [glfw:+key-down+
	       (update-gravity the-space cp:v+ (cp:v 0.0 -1.0))
	       'update-gravity-down]

	      [glfw:+key-space+
	       (add-node-at-pos)]

	      [glfw:+key-up+
	       (update-gravity the-space cp:v+ (cp:v 0.0 1.0))
	       'update-gravity-up]

	      [glfw:+key-left+
	       (update-gravity the-space cp:v+ (cp:v -1.0 0.0))]

	      [glfw:+key-r+
	       (scene-reload)]

	      [glfw:+key-right+
	       (update-gravity the-space cp:v+ (cp:v 1.0 0.0))]

	      [glfw:+key-equal+ (zoom-in)]
	      [glfw:+key-minus+ (zoom-out)]

	      [glfw:+key-w+ (move 0 0.1)]
	      [glfw:+key-s+ (move 0 -0.1)]
	      [glfw:+key-a+ (move -0.1 0)]
	      [glfw:+key-d+ (move 0.1 0)]

	      [glfw:+key-p+ (-run-physics- (not (-run-physics-)) )]

	      [glfw:+key-m+ (cycle-mode)]


	      [glfw:+key-0+
	       (print 'reset-gravity)
	       (update-gravity the-space (lambda (x) cp:v0))]

	      [glfw:+key-1+ (load-scene "1")]
	      [glfw:+key-2+ (load-scene "2")]
	      [glfw:+key-3+ (load-scene "3")]
	      [glfw:+key-4+ (load-scene "4")]
	      [glfw:+key-5+ (load-scene "tower")]


	      [glfw:+key-minus+
	       (update-gravity the-space cp:v+ (cp:v 1.0 0.0))]

	      [glfw:+key-equal+
	       (parameter-update the-eye-point v- (make-point 0 0 0.1))]

	      [glfw:+key-minus+
	       (parameter-update the-eye-point v+ (make-point 0 0 0.1))]))))

  (define (drag?)
    (when (and (eq? 'release (car cur-event))
	       (eq? 'press (car prev-event))
	       (cdr prev-event)
	       (cdr cur-event))
      (print "drag" (car modes)) (newline)
      (let [(from (cdr prev-event))
	    (to (cdr cur-event))]
	(case (car modes)
	  [(spring) (when (not (eq? from to))
		      (add-slide from to)
		      (add-rotary-spring from to))]
	  [(ball) (add-node-at-pos)]
	  [(box) (add-box (the-mouse-v))]
	  [(line) (when (not (eq? from to))
		    (add-line from to))]
	  [(impulse)
	   (cp:body-apply-impulse-at-local-point
	    (node-body from)
	    (cp:v- (cp:body-position (node-body to))
		   (cp:body-position (node-body from)))
	    cp:v0)]
	  [(force)
	   (cp:body-apply-force-at-local-point
	    (node-body from)
	    (cp:v- (cp:body-position (node-body to))
		   (cp:body-position (node-body from)))
	    cp:v0)]))))

  (define (emit-mouse-event pair)
    (set! prev-event cur-event)
    (set! cur-event pair)
    (drag?)
    #;(click?))

  ;; set mouse button handler
  (mouse-button-handler
   (lambda (window button action mods)

     (and (equal? action glfw:+press+)
	  (let* ([p (cp:space-point-query-nearest the-space (the-mouse-v) 4 ff #f)]
		 [res (filter (lambda (x) (equal? (node-shape x) p)) all-nodes)])
	    ;; (display `(press ,res)) (newline)
	    (when (not (null? res))
	      (node-color-set! (first res) (vector 1 1 1))
	      (emit-mouse-event (cons 'press (first res))))
	    res))

     (and (equal? action glfw:+release+)
	  (let* ([p (cp:space-point-query-nearest the-space (the-mouse-v) 4 ff #f)]
		 [res (filter (lambda (x) (equal? (node-shape x) p)) all-nodes)])
	    ;; (display `(release ,res)) (newline)
	    (when (not (null? res))
	      (node-color-set! (first res) (vector 1 1 1))
	      (emit-mouse-event (cons 'release (first res))))
	    res))))
  )
