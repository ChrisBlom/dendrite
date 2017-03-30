;; Mouse and keyboard input

;;;; Key handler

(define key-handler (make-parameter
		     (lambda (a . args)
		       (display 'no-keyhandler))))


(glfw:key-callback (lambda (window key scancode action mods)
		     (print 'key window key scancode action mods)
		     ((key-handler) window key scancode action mods)
                     (cond
                      [(and (eq? key glfw:+key-escape+) (eq? action glfw:+press+))
                       (glfw:set-window-should-close window #t)]
		      [(and (eq? key glfw:+key-space+)
			    (or (eq? action glfw:+press+)
				(eq? action glfw:+repeat+)))
		       (add-node-at-pos)])))

;;;; Mouse handler

(define cursor-position-handler (make-parameter (lambda (a . args)
						  (display 'no-curser-position-handler))))


(glfw:cursor-position-callback
 (lambda (window x y)
   (let [(orig (current-exception-handler))]
     (comment (with-exception-handler
		  (lambda (e)
		    (println "Error in cursor-position-handler")
		    (println e)
		    (orig e))
		(lambda ())))
     ((cursor-position-handler) window x y))))

(define mouse-button-handler
  (make-parameter (lambda (a . args)
		    (display 'no-mouse-button-handler))))

(glfw:mouse-button-callback
 (lambda (window button action mods)
   ;(./trace (list 'mouse-button window button action mods))
   ((mouse-button-handler) window button action mods)))

;;;; User:


(case 1
  [(1) 'a]
  )




;; set key handler
(define (gravity-controller window key scancode action mods)
  (when (or (equal? action glfw:+press+)
	    (equal? action glfw:+repeat+))
    (print "input:" key)
    (print "action"
	   (condp equal? key
		  [glfw:+key-down+
		   (update-gravity the-space cp:v+ (cp:v 0.0 -1.0))
		   'update-gravity-down]

		  [glfw:+key-up+
		   (update-gravity the-space cp:v+ (cp:v 0.0 1.0))
		   'update-gravity-up]

		  [glfw:+key-left+
		   (update-gravity the-space cp:v+ (cp:v -1.0 0.0))]

		  [glfw:+key-r+
		   (scene-reload)]

		  [glfw:+key-right+
		   (update-gravity the-space cp:v+ (cp:v 1.0 0.0))]

		  [glfw:+key-0+
		   (print 'reset-gravity)
		   (update-gravity the-space (lambda (x) cp:v0))]

		  [glfw:+key-1+ (load-scene "1")]
		  [glfw:+key-2+ (load-scene "2")]
		  [glfw:+key-3+ (load-scene "3")]
		  [glfw:+key-4+ (load-scene "4")]


		  [glfw:+key-minus+
		   (update-gravity the-space cp:v+ (cp:v 1.0 0.0))]

		  [glfw:+key-equal+
		   (parameter-update the-eye-point v- (make-point 0 0 0.1))]

		  [glfw:+key-minus+
		   (parameter-update the-eye-point v+ (make-point 0 0 0.1))]))))

;; set mouse handlers

(define the-mouse-pos (make-parameter (cons 10. 10.)))

(key-handler gravity-controller)

(define (screen->world screen-point)
  (let* ([v (view-matrix)]
	 [p (projection-matrix)]
	 [world->screen (m* v p)]
	 [ivp (make-f32vector 16)])
    (inverse world->screen ivp)
    (m* ivp screen-point)))

;; 1    18
;; 5  2000
;; 9 25000

(define (window->screen window-point)
  (v- (v* window-point 2/600)
      (make-point 1 1 0)))

;; TODO to coordinate transformation properly
(cursor-position-handler
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

 (define ff (cp:shape-filter-new (cp:uint 1) cp:all-categories cp:all-categories))

(define prev-event '(none #f))
(define cur-event '(none #f) )

(define (drag?)
  (when (and (eq? 'release (car cur-event))
	     (eq? 'press (car prev-event))
	   (cdr prev-event)
	   (cdr cur-event))
    (display "add-spring") (newline)
    (let [(from (cdr prev-event))
	  (to (cdr cur-event))]
      (add-slide from to)
      (add-rotary-spring from to)
)))

(define (emit-mouse-event pair)
  (set! prev-event cur-event)
  (set! cur-event pair)
  (drag?))

(mouse-button-handler
 (lambda (window button action mods)
   ;; (when (and (or (equal? button glfw:+mouse-button-1+))
   ;; 	      (or (equal? action glfw:+release+)
   ;; 		  (equal? action glfw:+repeat+)))
   ;;   (add-node-at-pos))
   (and (equal? action glfw:+press+)
	(let* ([p (cp:space-point-query-nearest the-space (the-mouse-v) 4 ff #f)]
	       [res (filter (lambda (x) (equal? (node-shape x) p)) all-nodes)])
	  (display `(press ,res)) (newline)
	  (when (not (null? res))
	    (node-color-set! (first res) (vector 1 1 1))
	    (emit-mouse-event (cons 'press (first res))))
	  res))

   (and (equal? action glfw:+release+)
	(let* ([p (cp:space-point-query-nearest the-space (the-mouse-v) 4 ff #f)]
	       [res (filter (lambda (x) (equal? (node-shape x) p)) all-nodes)])
	  (display `(release ,res)) (newline)
	  (when (not (null? res))
	    (node-color-set! (first res) (vector 1 1 1))
	    (emit-mouse-event (cons 'release (first res)))
	    )
	  res))))
