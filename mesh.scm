(include "utils.scm")

(define (circle-positions n)
  (apply append (cons '(0 0)
		      (map (lambda (i)
			     (let ([angle (/ (* pi 2 i) n)]
				   [radius 0.2])
			       (list (* radius (sin angle))
				     (* radius (cos angle)))))
			   (range 0 n)))))

(define (circle-colors n)
  (apply append (cons '(255 0 255)
		      (map (lambda (i)
			     (let ([h (% i 2)])
			       (map (compose (cut * <> 255.))
				    (list h
					  h
					  h))))
			   (range 0 n 1)))))

(define (circle-indices n)
  (apply append
	 (map (lambda (i) (list 0
			   (inc i)
			   (inc (modulo (inc i) n))))
		     (range 0 n))))

(define hexagon-positions (circle-positions 6))

(define hexagon-colors (circle-colors 6))

(define hexagon-indices
  (apply append '((0 1 2)
		  (0 2 3)
		  (0 3 4)
		  (0 4 5)
		  (0 5 6)
		  (0 6 1))))

;;
;;      1_________2
;;      /         \
;;     /           \
;;  6 /     0.      \3
;;    \            /
;;     \          /
;;      \________/
;;     5          4
(define hex (make-mesh
              vertices: `(attributes: ((position #:float 2)
                                       (color #:unsigned-byte 3
					      normalized: #t))
                          initial-elements: ((position . ,hexagon-positions)
                                             (color . ,hexagon-colors)))
              indices: `(type: #:ushort
			       initial-elements: ,hexagon-indices)))

;; (-1,1)   1             0 (1,1)
;;            +-----------+
;;            | .         \
;;            |   .       \
;;            |     .     \
;;            |       .   \
;;            |         . \
;;            +-----------+
;; (-1,-1)   3             2  (1,-1)

(define square-positions (list  0.5   0.5 ;; 0
			       -0.5   0.5 ;; 1
			        0.5  -0.5 ;; 2
			       -0.5  -0.5 ;; 3
			    ))

(define square-colors (list 255 255 255 ;; 0
			    255 255 255 ;; 1
			    255 255 255 ;; 2
			    255 255 255 ;; 3
			    ))

(define square-indices
  (apply append '((0 1 2)
		  (3 1 2))))

(define square-mesh
  (make-mesh
   vertices: `(attributes: ((position #:float 2)
			    (color #:unsigned-byte 3
				   normalized: #t))
			   initial-elements: ((position . ,square-positions)
					      (color . ,square-colors)))
   indices: `(type: #:ushort
		    initial-elements: ,square-indices)))

(define (disk n)
  (make-mesh
   vertices: `(attributes: ((position #:float 2)
			    (color #:unsigned-byte 3
				   normalized: #t))
			   initial-elements: ((position . ,(circle-positions n))
					      (color . ,(circle-colors n))))
   indices: `(type: #:ushort
		    initial-elements: ,(circle-indices n))))

;;   0     ->              1
;;  -1,-1               1,-1
;;     _________________
;;    |                |
;;    |                |
;;    |                |
;;    |                |   |
;;    |                |   v
;;    |                |
;;    |                |
;;    |                |
;;    |________________|
;;  -1,1     <-          1,1 2					;
;; 3
(define rect (make-mesh
              vertices: '(attributes: ((position #:float 2)
                                       (color #:float 3
                                              normalized: #t))
                          initial-elements: ((position . (-1 -1
							  +1 -1
                                                          +1 +1
                                                          -1 +1))
                                             (color . (255 255 0
                                                       0   255 0
                                                       0   0   255
                                                       255 0   255))))
              indices: '(type: #:ushort
                         initial-elements: (0 1 2
					      0 2 3))))

(use matchable)

(define (line-mesh start end)
  (make-mesh
   vertices: `(attributes: ((position #:float 2)
			    (color #:float 3 normalized: #t))
	       initial-elements: ((position . ,(append start end))
					      (color . (255 255 0
						        0   255 0))))
   indices: '(type: #:ushort
		    initial-elements: (0 1))
   mode: #:lines))


(define (line-mesh-vertices x1 y1 x2 y3)
  `((position . ,(list x1 y1 x2 y3))
    (color . (255 255 0
           	  0   255 0))))
