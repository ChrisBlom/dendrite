(module ringbuffer
  (new-ringbuffer
   ringbuffer-advance
   ringbuffer-set!
   ringbuffer-length
   ringbuffer-set-start!
   ringbuffer-get
   ringbuffer-get-start
   ringbuffer-get-end
   ringbuffer->vector
   vector->ringbuffer
   list->ringbuffer)

  (import chicken scheme)

  (define-record ringbuffer
    content
    offset
    length)

  (define (inc i) (+ 1 i))

  (define % modulo)

  (define (new-ringbuffer length)
    (make-ringbuffer
     (make-vector length 0.)
     0
     length))

  (define (ringbuffer-advance* b)
    (ringbuffer-offset-set! b (% (+ 1 (ringbuffer-offset b))
			     (ringbuffer-length b))))

  (define (ringbuffer-advance b)
    (ringbuffer-offset-set! b (% (+ 1 (ringbuffer-offset b))
			     (ringbuffer-length b)))
    (ringbuffer-set-start! b 0.)
    b)


  (define (ringbuffer-set! b off value)
    (vector-set! (ringbuffer-content b)
		 (% (+ off (ringbuffer-offset b))
		    (ringbuffer-length b))
		 value)
    b)

  (define (ringbuffer-update b off f . args)
    (let ([new-val (apply f (ringbuffer-get b off) args)])
      (ringbuffer-set! b off new-val)
      b))

  (define (ringbuffer-set-start! b value)
    (ringbuffer-set! b 0 value))

  (define (ringbuffer-get b off)
    (vector-ref (ringbuffer-content b)
		(% (- (ringbuffer-offset b) off)
		   (ringbuffer-length b))))


  (define (ringbuffer-get-start b)
    (ringbuffer-get b 0))

  (define (ringbuffer-get-end b)
    (ringbuffer-get b -1))

  (define (print-buffer b)
    (let loop ([i 0])
      (if (< i (ringbuffer-length b))
	  (begin (print (ringbuffer-get b i))
		 (loop (inc i))))))


  (define (vector->ringbuffer v)
    (make-ringbuffer v 0 (vector-length v)))

  (define (list->ringbuffer l)
    (vector->ringbuffer (list->vector l)))

  (define (ringbuffer->vector b)
    (let ([v (make-vector (ringbuffer-length b))])
      (let loop ([i 0])
	(if (< i (ringbuffer-length b))
	    (begin (vector-set! v i (ringbuffer-get b i))
		   (loop (inc i)))))
      v))


  )
