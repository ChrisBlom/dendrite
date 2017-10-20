(module reactive *
  (import scheme chicken)
  (use srfi-1
       (prefix posix posix/)
       box
       srfi-18
       synth-utils)


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
    (box-set! (cell-box cell) val)
    (recompute-dependents cell)
    cell)

  (define (file-cell file)
    (define fcell (create-cell file))
    (define (tsleep n)
      (thread-sleep! (seconds->time (+ n (time->seconds (current-time))))))
    (define (get-time)
      (posix/file-modification-time file))
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
  )
