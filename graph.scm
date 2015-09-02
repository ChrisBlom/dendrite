(import chicken scheme)

(require-extension srfi-69)

(define (distinct lst-)
  (let ((seen (make-hash-table)))
    (let distinct* ((lst lst-))
      (if (null? lst)
	  '()
	  (if (hash-table-ref/default seen (car lst) #f)
	      (distinct* (cdr lst))
	      (begin
		(hash-table-set! seen (car lst) #t)
		(cons (car lst) (distinct* (cdr lst)))))))))

; (distinct '(1 2 3 1))

(define-record graph
  edges-forward ;; source -> target -> elem
  edges-inverse
  nodes
  edges)

(define (new-graph)
  (make-graph
   (make-hash-table)
   (make-hash-table)
   '() ; nodes
   '() ; edges
   ))

(define (graph-add-edge graph source target label)
  (graph-edges-set! graph (cons label (or (graph-edges graph) '())))
  (graph-nodes-set! graph (distinct (cons source (cons target (or (graph-nodes graph) '())))))
  (let ([target->label (hash-table-ref/default (graph-edges-forward graph) source (make-hash-table))])
    (hash-table-set! target->label target label)
    (hash-table-set! (graph-edges-forward graph) source target->label))
  (let ([source->label (hash-table-ref/default (graph-edges-inverse graph) target (make-hash-table))])
    (hash-table-set! source->label source label)
    (hash-table-set! (graph-edges-inverse graph) target source->label)))

(define (graph-node-inputs graph node)
  (let ([to-node (hash-table-ref/default (graph-edges-inverse graph) node #f)])
    (if to-node
	(hash-table-values to-node)
	'())))

(define (graph-node-outputs graph node)
  (let ([from-node (hash-table-ref/default (graph-edges-forward graph) node #f)])
    (if from-node
	(hash-table-values from-node)
	'())))

(define (graph-edge-label graph source target)
  (let ([target->label (hash-table-ref/default (graph-edges-forward graph) source #f)])
    (and target->label (hash-table-ref/default target->label target #f))))


(if #f

    (let ([g (new-graph)])
      (graph-add-edge g 1 2 'a)
      (graph-add-edge g 1 3 'b)
      (graph-add-edge g 4 3 'b)
      (graph-add-edge g 4 3 'b)
      (map
       (lambda (kv)
	 (list (car kv) (hash-table->alist (cdr kv))))
       (hash-table->alist (graph-edges-forward g)))))
