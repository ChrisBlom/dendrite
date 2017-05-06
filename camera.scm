
(define zoom 10)

(define (zoom-out)
  (set! zoom (+ zoom 1))
  (projection-matrix
   (ortho zoom zoom  0.2 100)))

(define (zoom-in)
  (set! zoom (- zoom 1))
  (projection-matrix
   (ortho zoom zoom 0.2 100)))

;(define projection-matrix (make-parameter (perspective 600 600 1 10 70)))
(define projection-matrix (make-parameter (ortho zoom zoom  0.2 100)))

(define the-eye-point (make-parameter (make-point 0 0 5)))

(the-eye-point (make-point 0 0 1))

(define the-object-point (make-parameter (make-point 0 0 0)))

(the-object-point (make-point 0 10 0))

(define view-matrix
  (make-parameter (look-at (the-eye-point)
			   (the-object-point)
			   (make-point 0 1 0)))) ; up vector
(define pos (cp:v 0 0))

(define (move x y)
  (f64vector-set! pos 0 (+ (f64vector-ref pos 0) x))
  (f64vector-set! pos 1 (+ (f64vector-ref pos 1) y))
  (update-view-matrix))

(define (v->point vect)
  (make-point (f64vector-ref vect 0)
	      (f64vector-ref vect 1)
	      0))

(define (update-view-matrix)
  (view-matrix
   (look-at (make-point 0 0 10)
	    (v->point pos)
	    (make-point 0 1 0))))

(update-view-matrix)

(define (model-matrix)
  (mat4-identity))
