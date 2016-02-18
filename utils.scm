(import matchable)
(use box)

(define % modulo)

(define (map-indexed f elems)
  (let loop ([i 0]
	     [todo elems])
    (if (eq? '() todo)
	'()
	(cons (f i (car todo))
	      (loop (+ i 1) (cdr todo))))))

(define (for-each-indexed f elems)
  (define i 0)
  (for-each (lambda (x)
	      (f i x)
	      (set! i (+ i 1)))  elems))

(define (range init limit #!optional [step 1])
  (if (>= init limit)
      '()
      (cons init (range (+ init step) limit step ))))

(define (inc i) (+ 1 i))



(define (nth list n)
  (let loop ([i 0]
	     [todo list])
    (match todo
      [() #f]
      [(h . t) (if (eq? n i)
		   h
		   (loop (inc i) t))])))

(define-syntax comment
  (syntax-rules ()
    [(comment expr ...)
     #f]))

(define-syntax ./trace
  (syntax-rules ()
    [(_ a ...)
     (let ([name-to-val (map cons (list 'a ...) (list a ...))])
       (printf "---- TRACE: ---- \n" )
       (for-each
	(lambda (x)
	  (printf " ~s : ~s\n" (car x) (cdr x))
	  )
	name-to-val)
       (printf "---------------- \n" )
       (cdr (last name-to-val)))]))


(define-record cell
  box
  fn
  ins
  outs
  paused)


(define (cell-get cell)
  (unbox (cell-box cell)))

(define (cell-update! cell)
  (when (not (cell-paused cell))
    ;(display "Updated")
    ;(display (cell-fn cell))
    ;(newline)
    ((cell-fn cell))))



(define (create-cell* fn args #!key (paused #f))
  (letrec* ([ins (filter cell? args)]

	    [update-fn (lambda ()
			 (let ([arg-vals (map (lambda (x) (if (cell? x) (cell-get x) x)) args)])
			   (if (any (cut eq? 'uninitialized-cell <>) arg-vals)
			       (print "Skipping update of cell with unitialized inputs")
			       (box-set! (cell-box cell)
					 (if (eq? args '())
					     fn
					     (apply fn arg-vals))))))]
	    [cell (make-cell (make-box 'uninitialized-cell) update-fn ins (list) paused)])
    (cell-update! cell)

    (for-each (lambda (in)
		(cell-outs-set! in (cons cell (cell-outs in ))))
	      ins)

    cell))

(define (create-cell fn #!rest args)
  (create-cell* fn args paused: #f))

(define (create-paused-cell fn . args)
  (create-cell* fn args paused: #t))

(define (cell-unpause cell)
  (cell-paused-set! cell #f)
  (cell-update! cell))

(define (recompute-dependents cell)
  (for-each (lambda (out)
	      ((cell-fn out))
	      (recompute-dependents out))
	    (cell-outs cell)))

(define (cell-set! cell val)
  ;; TODO update
  (box-set! (cell-box cell) val)
  (recompute-dependents cell)
  cell)


;; (let* ([a (create-cell 1)]
;;        [b (create-cell 2)]
;;        [c (create-cell 3)]
;;        [d (create-cell 4)]
;;        [e (create-cell 5)]
;;        [f (create-cell + a b c d e)])
;;   (display
;;    (list (cell-get f)
;; 	 (cell-set! a 10)
;; 	 (cell-get f))))


(define (file-cell file)
  (define fcell (create-cell file))
  (define (tsleep n)
    (thread-sleep! (seconds->time (+ n (time->seconds (current-time))))))
  (define (get-time)
    (file-modification-time file))
  ;; TODO use single thread to watch all files
  (thread-start!
   (lambda ()
     (let loop ((filetime '()))
       (let ((newtime (get-time)))
	 (when (not (equal? filetime newtime))
	   (handle-exceptions e (lambda (e) (display e))
	     (display "Updated: ") (display file) (newline)
	     (cell-set! fcell file)))
	 (tsleep 1)
	 (loop newtime)))))
  fcell)



(define (circle-ring n)
  (let ([b (list->ringbuffer
	    (map (lambda (i)
		   (let ([angle (* i (/ n) pi 2)])
		     (cons (* 2 (sin angle))
			   (* 2 (cos angle)))))
		 (range 0 n)))])
    (map (lambda (j)
	   `( ,(ringbuffer-get b j)
	      ,(ringbuffer-get b (+ 1 j))))
	 (range 0 n))))




;; (define v1 (file-cell "vertex-shaders/v1.glsl"))

;; (define v1-string (create-cell read-all v1))



;; (comment


;;  (define line-vertex-string
;;    (let ([cell (create-cell (read-all "vertex-file.glsl"))])
;;      (change-watcher "vertex-file.glsl"
;; 		     (lambda (new-value) (set-cell! cell new-value)))
;;      cell))

;;  (define-cell (link-shader line-vertex-string
;; 			   line-fragment-string)
;;    (compile-shader line-vertex-string
;; 		   line-fragment-string))







;;  )
