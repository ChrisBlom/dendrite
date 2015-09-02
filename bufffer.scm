(module buffer (new-buffer buffer-advance buffer-set! buffer-set-start! buffer-get buffer-get-start buffer-get-end buffer->vector)

  (import chicken scheme)

  (define-record buffer
    content
    offset
    length)

  (define (inc i) (+ 1 i))

  (define % modulo)

  (define (new-buffer length)
    (make-buffer
     (make-vector length 0.)
     0
     length))

  (define (buffer-advance* b)
    (buffer-offset-set! b (% (+ 1 (buffer-offset b))
			     (buffer-length b))))

  (define (buffer-advance b)
    (buffer-offset-set! b (% (+ 1 (buffer-offset b))
			     (buffer-length b)))
    (buffer-set-start! b 0.)
    b)


  (define (buffer-set! b off value)
    (vector-set! (buffer-content b)
		 (% (+ off (buffer-offset b))
		    (buffer-length b))
		 value)
    b)

  (define (buffer-update b off f . args)
    (let ([new-val (apply f (buffer-get b off) args)])
      (buffer-set! b off new-val)
      b))

  (define (buffer-set-start! b value)
    (buffer-set! b 0 value))

  (define (buffer-get b off)
    (vector-ref (buffer-content b)
		(% (- (buffer-offset b) off)
		   (buffer-length b))))


  (define (buffer-get-start b)
    (buffer-get b 0))

  (define (buffer-get-end b)
    (buffer-get b -1))

  (define (print-buffer b)
    (let loop ([i 0])
      (if (< i (buffer-length b))
	  (begin (print (buffer-get b i))
		 (loop (inc i))))))

  (define (buffer->vector b)
    (let ([v (make-vector (buffer-length b))])
      (let loop ([i 0])
	(if (< i (buffer-length b))
	    (begin (vector-set! v i (buffer-get b i))
		   (loop (inc i)))))
      v))

  (if #f
      (begin

	(define b (new-buffer 10))

	(buffer-content b)



	(buffer-update b 0 + 10.)


	(buffer-get-start b)
	(buffer-length b)

	(print-buffer b)


	(buffer-set! b 0 3)

	(begin
	  (buffer-advance b)
	  (buffer-set-start! b 0)
	  (buffer->vector b))


	)
      )
  )
