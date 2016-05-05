

(define wsz 5.)

(define (scene-1 space)

    ;; walls
    (cp:space-add-shapes space
			 (doto (fixed-line-segment space (cp:v (- wsz) (- wsz)) (cp:v (- wsz) wsz) radius: 0.4)
			       (cp:shape-set-elasticity 0.95))
			 (doto (fixed-line-segment space (cp:v (- wsz) wsz) (cp:v wsz wsz) radius: 0.4)
			       (cp:shape-set-elasticity 0.95))
			 (doto (fixed-line-segment space (cp:v wsz wsz) (cp:v wsz (- wsz)) radius: 0.4)
			       (cp:shape-set-elasticity 0.95))
			 (doto (fixed-line-segment space (cp:v wsz (- wsz)) (cp:v (- wsz) (- wsz)) radius: 0.4)
			       (cp:shape-set-elasticity 0.95)))






    )

(define (scene-2)
  (map (lambda (xy)
	 (let* ([x (car xy)]
		[y (cdr xy)]
		[body (cp:body-new-static)]
		[shape (cp:circle-shape-new body 0.2 (cp:v 0 0))])
	   (cp:body-set-position body (cp:v x y))
	   (cp:space-add-shape the-space shape)


	   (node-children-append! root-node
				  (new-node (render-ball -1)
					    body
					    shape)
				  )
	   (cons body shape)))
       '([1 . -1]
	 [-1 . -1]
	 [1 . 1]
	 [-1 . 1]
	 [-1 . -1]
	 )))


(comment
 (begin
   (when (not (unbound? 'scene-2))
     (cp:space-remove-shape the-space (cdr scene-2)))

   (define scene-2

     )

   (define scene-3
     (let* ([body (cp:body-new-static)]
	    [shape (cp:circle-shape-new body 2.0 (cp:v -5. -5.))])


       (cp:space-add-shape the-space shape)


       (node-children-append! root-node
			      (new-node (render-ball -1)
					body
					shape)
			      )
       (cons body shape))))

 )
