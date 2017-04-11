
(define (render-node node projection view ctx-matrix)
  (let ([render-fn (node-render-fn node)])
    ;; Render node
    (when render-fn
      (render-fn node
		 projection
		 (m* ctx-matrix view)
		 ctx-matrix))
    ;; render children with sub-ctx
    (let ([sub-ctx (m* (node-matrix node) ctx-matrix)])
      (for-each (lambda (child)
		  (render-node child projection view sub-ctx))
		(node-children node)))))

(define (init-node node)
  (let ([init-fn (node-render-init-fn node)])
    ;; Render node
    (when init-fn
      (init-fn node))
    (for-each (lambda (child)
		(init-node child))
	      (node-children node))))

(define circle-mesh (disk 20))

(define rectangle-mesh rect)

(define (render-mesh mesh program mvp energy color)
  (gl:use-program program)
  (gl:bind-vertex-array 0)

  ;; set shader parameters
  (let ([id (gl:get-uniform-location program "ENERGY")])
    (when (> id -1)
      (gl:uniform1f id energy)))

  ;; set MVP matrix
  (gl:uniform-matrix4fv (gl:get-uniform-location program "MVP") 1 #f mvp)

  (gl:uniform3fv (gl:get-uniform-location program "colormod") 1
		 (list->f32vector (vector->list color)))
  ;; render mesh
  (let ([vao (mesh-vao mesh)])
    (when vao
      (gl:bind-vertex-array vao)
      (gl:draw-elements-base-vertex (mode->gl (mesh-mode mesh))
				    (mesh-n-indices mesh)
				    (type->gl (mesh-index-type mesh))
				    #f 0))))

(define ((render-ball idx) node projection-matrix view-matrix ctx-matrix)
  (let* ([body  (node-body node)]
	 [angle (- (cp:body-angle body))]
	 [body-pos (cp:body-position body)]
	 [mvp (m* projection-matrix
		  (m* (translation (make-point (cp:v.x body-pos)
					       (cp:v.y body-pos)
					       0))
		      (m* view-matrix
			  (rotate-z angle (model-matrix)))))])
    (render-mesh circle-mesh
		 (cell-get program1)
		 mvp
		 (cp:vlength (cp:body-velocity body))
		 (or (node-color node)
		     (vector
		      1
		      (+ 0.5 (* 0.5 (sin idx)))
		      (+ 0.5 (* 0.5 (cos idx))))))))

;; TODO pass node to all render fns

(define *render-circle-shape* (make-parameter #f))

(*render-circle-shape* (lambda (node projection-matrix view-matrix segment )
			 (let* ([body (cp:shape-get-body segment)]
				[angle (+ (/ pi 4) (- (cp:body-get-angle body)))]
				[body-pos (cp:body-get-position body)]
				[trans (make-point (cp:v.x body-pos)
						   (cp:v.y body-pos)
						   0)])

			   (render-mesh circle-mesh
					(cell-get program3)
					(m* projection-matrix
					    (m* (translation trans)
						(m* view-matrix
						    (rotate-z angle (model-matrix)))))

					(cp:vlength (cp:body-get-velocity body))

					(or (node-color node) (vector 1 1 0))))))


(define *render-circle-node* (make-parameter #f))

(*render-circle-node*
 (lambda (node projection-matrix view-matrix ctx-matrix)
   (let* ([body (node-body node)]
	  [angle (+ (/ pi 4) (- (cp:body-get-angle body)))]
	  [body-pos (cp:body-get-position body)]
	  [trans (make-point (cp:v.x body-pos)
			     (cp:v.y body-pos)
			     0)])

     (render-mesh circle-mesh
		  (cell-get program3)
		  (m* projection-matrix
		      (m* (translation trans)
			  (m* view-matrix
			      (rotate-z angle (model-matrix)))))

		  (cp:vlength (cp:body-get-velocity body))

		  (or (node-color node) (vector 1 1 0))))))

(define *render-constraint* (make-parameter #f))

(*render-constraint* (lambda (projection-matrix view-matrix ctx-matrix constraint)
		       (let* ([pos-a (cp:body-get-position (cp:constraint-get-body-a constraint))]
			      [pos-b (cp:body-get-position (cp:constraint-get-body-b constraint))]
			      [angle (cp:vtoangle (cp:v- pos-a pos-b))]
			      [middle (cp:vlerp pos-a pos-b 0.5)]
			      [trans (make-point (cp:v.x middle) (cp:v.y  middle)
						 0.)])

			 (mesh-update! the-line-mesh (line-mesh-vertices (cp:v.x pos-a) (cp:v.y pos-a)
									 (cp:v.x pos-b) (cp:v.y pos-b)))

			 (render-mesh the-line-mesh
				      (cell-get program-line)
				      (m* projection-matrix (m* view-matrix (model-matrix)))
				      (cp:constraint-get-impulse constraint)
				      (vector 1 0 1)))))



(define *render-poly* (make-parameter #f))

(define (v->point v) (make-point (cp:v.x v) (cp:v.y v) 0))

(*render-poly* (lambda (node projection-matrix view-matrix ctx-matrix)
		 (let* ([poly-shape (node-shape node)]
			[n (cp:poly-shape-count poly-shape)]
			[body-pos (cp:body-position (cp:shape-body poly-shape))]
			[angle (cp:body-angle (cp:shape-body poly-shape))])

		   (render-mesh (node-mesh node)
				(cell-get program-poly)
			      (m* projection-matrix
				  (m* (translation (make-point (cp:v.x body-pos)
							       (cp:v.y body-pos)
							       0))
				      (m* view-matrix
					  (rotate-z angle (model-matrix)))))
		   	      0.5
		   	      (vector 0.5 3 0.5)))))

(define *render-segment* (make-parameter #f))

(*render-segment* (lambda (projection-matrix view-matrix ctx-matrix segment-shape)
		       (let* ([pos-a (cp:segment-shape-get-a segment-shape)]
			      [pos-b (cp:segment-shape-get-b segment-shape)]
			      [angle (cp:vtoangle (cp:v- pos-a pos-b))]
			      [middle (cp:vlerp pos-a pos-b 0.5)]
			      [trans (make-point (cp:v.x middle) (cp:v.y  middle) 0.)])

			 (mesh-update! the-line-mesh (line-mesh-vertices (cp:v.x pos-a) (cp:v.y pos-a)
									 (cp:v.x pos-b) (cp:v.y pos-b)))

			 (render-mesh the-line-mesh
				      (cell-get program-line)
				      (m* projection-matrix
					  (m* view-matrix
					      (model-matrix)))
				      0.5
				      (vector 1 0 1)))))

(define *render-link* (make-parameter #f))

(*render-link* (lambda (projection-matrix view-matrix ctx-matrix src trg)
		 (let* ([pos-a (cp:body-get-position src)]
			[pos-b (cp:body-get-position trg)]
			[angle (cp:vtoangle (cp:v- pos-a pos-b))]
			[middle (cp:vlerp pos-a pos-b 0.5)]
			[trans (make-point (cp:v.x middle) (cp:v.y  middle)
					   0.)])

		   (mesh-update! the-line-mesh (line-mesh-vertices (cp:v.x pos-a) (cp:v.y pos-a)
								   (cp:v.x pos-b) (cp:v.y pos-b)))

		   (render-mesh the-line-mesh
				(cell-get program-line)
				(m* projection-matrix (m* view-matrix (model-matrix)))
				(cp:constraint-get-impulse constraint)
				(vector 1 1 0)))))
