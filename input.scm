;; Mouse and keyboard input

;;;; Key handler

(define key-handler (make-parameter
		     (lambda (a . args)
		       (display 'no-keyhandler))))


(glfw:key-callback (lambda (window key scancode action mods)
		     (display (list 'key window key scancode action mods))
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
   (./trace (list 'mouse-button window button action mods))
   ((mouse-button-handler) window button action mods)))

;;;; User:

;; set key handler
(define (gravity-controller window key scancode action mods)
  (when (or (equal? (./trace action) glfw:+press+)
	    (equal? (./trace action) glfw:+repeat+))
    (./trace (list  window key scancode action mods))
    (cond [(equal? key glfw:+key-down+)
	   (update-gravity cp:v+ (cp:v 0.0 -1.0))]

	  [(equal? key glfw:+key-up+)
	   (update-gravity cp:v+ (cp:v 0.0 1.0))]

	  [(equal? key glfw:+key-left+)
	   (update-gravity cp:v+ (cp:v -1.0 0.0))]

	  [(equal? key glfw:+key-right+)
	   (update-gravity cp:v+ (cp:v 1.0 0.0))]

	  [(equal? key glfw:+key-equal+)
	   (parameter-update the-eye-point v- (make-point 0 0 0.1))]

	  [(equal? key glfw:+key-minus+)
	   (parameter-update the-eye-point v+ (make-point 0 0 0.1))])))

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
     (./trace window-point
	      screen-point
	      world-pos)
     (the-mouse-v (cp:v x y)))))

(mouse-button-handler
 (lambda (window button action mods)
   (when (and (or (equal? button glfw:+mouse-button-1+))
	      (or (equal? action glfw:+release+)
		  (equal? action glfw:+repeat+)))
     (add-node-at-pos))))
