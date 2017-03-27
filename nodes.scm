(use srfi-1 gl-math   gl-utils)

(define-record node
  render-fn ;; takes node , mvp
  parent ;; node, #f for root
  children ;; list of nodes
  matrix
  body				;; optional
  shape 				;; optional
  id
  inputs ;; list of links
  outputs ;; list of links
  color
  )

(define-record-printer (node x out)
  (fprintf out "#,(node ~S ~S)"
           (node-id x)
	   (map node-id (node-children x))))

(define-record link
  (setter source)				;; node
  (setter target)				; node
  (setter buffer))

(define all-nodes '())

(define (conj list elem)
  (cons elem list))


;; inserts a new ball at the current cursor
(define the-counter (box 0))

(define (next-id)
  (box-swap! the-counter (cut + <> 1)))

(define (new-node parent-node #!key
		  (render #f)
		  (body #f)
		  (shape #f)
		  (id #f)
		  (color #f))
  (let ([n (make-node render
		      parent-node
		      '()
		      (mat4-identity)
		      body
		      shape
		      (or id (next-id))
		      '()
		      '()
		      color)])
    (update! all-nodes conj n)
    (when parent-node
      (node-children-set! parent-node
			  (cons n (node-children parent-node))))
    n))

(define (node-children-update! node f . args)
  (node-children-set! node (apply f (node-children node) args)))

(define (node-children-append! node . children)
  (node-children-update! node append children))

(define (node-descendants node)
  (let ([children (node-children node)])
    (if (null-list? children)
	'())
    (apply append
	   children
	   (map node-descendants children))))

(define (remove-node parent node)
;  (node-children-update! parent (cut delete node <> eq?))
 ; (when (node-body node) (cp:space-remove-body the-space (node-body parent)))
  (when (node-shape node)  (cp:space-remove-shape the-space (node-shape parent))))

(define (link-add src trg n)
  (let ([buffer (new-ringbuffer n)]
	[link (make-link src trg buffer)])
    (update! (node-outputs src) (conj link))
    (update! (node-inputs trg) (conj link))
    link))

(define a '())

(define (remove-subtree parent node)

  (set! a (cons node a))

  (for-each (lambda (child)
	      (remove-subtree node child))
	    (node-children node))

  (when (and (node-shape node) (cp:space-contains-shape the-space (node-shape node)))
    (cp:space-remove-shape the-space (node-shape node)))

  (when (and (node-body node) (cp:space-contains-body the-space (node-body node)))
    (cp:space-remove-body the-space (node-body node)))

  ;; remove from parent
  (when parent
    (node-children-set! parent (delete node (node-children parent) equal?)))


  )

(define (remove-children node)
  (for-each (lambda (child)
	      (remove-subtree node child))
	    (node-children node)))
