(import-for-syntax chicken scheme srfi-1)
(import chicken scheme matchable)
(use box)

(define-syntax comment
  (syntax-rules ()
    [(comment expr ...)
     #f]))

(define-syntax set!!
  (syntax-rules ()
    [(set!! var expr)
     (set! var expr)]
    [(set!! var expr more ...)
     (begin (set! var expr)
	    (set!! more ...))]))

(define-syntax update!
  (ir-macro-transformer
   (lambda (e r c)
     (let ([field (cadr e)]
	   [f (caddr e)]
	   [args (cdddr e)])
       `(begin
	  (set! ,field (,f ,field ,@args))
	  ,(if (list? field)
	       (last field)
	       field))))))

(define-syntax update-slot!
  (syntax-rules ()
    ((update-slot! rec slot f args ...)
     (update! (slot rec) f args ...))))

(comment

 (define x 1)

 x

 (update! x + 1)

 x

 (define-record foo (setter a))

 (define foox (make-foo 1))

 (foo-a foox)

 (foo-a (update! (foo-a foox) + 1))

 (foo-a (update-slot! foox foo-a + 1))

 )

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

(define rest cdr)

(define (keep f xs)
  (if (null-list? xs)
      xs
      (let ([fx (f (first xs))])
	(if fx
	    (cons fx (keep f (rest xs)))
	    (keep f (rest xs))))))

(define (nth list n)
  (let loop ([i 0]
	     [todo list])
    (match todo
      [() #f]
      [(h . t) (if (eq? n i)
		   h
		   (loop (inc i) t))])))

(define (parameter-update param f . args)
  (param (apply f (param) args)))


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

(define-syntax (define-gs-record x r c)
  (let ((type (cadr x))
        (fields (cddr x))
	(%begin (r 'begin))
	(%define-record (r 'define-record))
	(%define (r 'define))
	(%getter-with-setter (r 'getter-with-setter)))
    `(,%begin
      (,%define-record ,type ,@fields)
      ,@(map (lambda (f)
	       (let* ((getter (symbol-append (strip-syntax type) '- (strip-syntax f)))
		      (setter (symbol-append (strip-syntax getter) '-set!)))
		 (list %define getter (list %getter-with-setter getter setter))))
	     fields))))


(define-syntax define-box
  (syntax-rules ()
    [(_ f body)
     (define (first f)
       (lambda (rest f)
	 body))]))

(define (println a . args)
  (display a) (display " ")
  (for-each (lambda (x)
	      (display x)
	      (display " "))
	    args)
  (newline))

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

(define (rand n)
  (let ([sz 10000])
    (* n (- (* 2 (/ (random sz) sz)) 1))))

(define (without list elem)
  (remove (cut eq? elem <>) list))

(define (repeatedly n f)
  (if (positive? n)
      (cons (f) (repeatedly (- n 1) f))
      '()))
