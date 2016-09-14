
(define-record node
  render-fn ;; takes node , mvp
  children ;; list of nodes
  matrix
  body				;; optional
  shape 				;; optional
  id
  inputs ;; list of links
  outputs ;; list of links
  )


(define-record link
  (setter source)				;; node
  (setter target)				; node
  (setter buffer))

(define all-nodes '())

(define (conj list elem)
  (cons elem list))

(define (new-node render-fn #!optional (body #f) (shape #f) (id #f))
  (let ([n (make-node render-fn
		      '()
		      (mat4-identity)
		      body
		      shape
		      (or id (next-id))
		      '()
		      '()
		      )])
    (update! all-nodes conj n)
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
;  (for-each (cut remove-node node <>) (node-children node))
  (node-children-update! parent without node)
  (when (node-body node) (cp:space-remove-body the-space (node-body p)))
  (when (node-shape node)  (cp:space-remove-shape the-space (node-shape p))))

(define (link-add src trg n)

  (let ([buffer (new-ringbuffer n)]
	[link (make-link src trg buffer)])
    (update! (node-outputs src) (conj link))
    (update! (node-inputs trg) (conj link))
    link))
