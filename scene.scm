
;; (name node-scene space)
(define-once loaded-scene #f)

(define (unload-scene name scene-node space)

  ;; remove all nodes
  (remove-subtree root-node scene-node)
  (set! loaded-scene #f)
  name)

(define (unload)
  (when loaded-scene
    (apply unload-scene loaded-scene)))


;; returns (name scene-node space)
(define (load-scene n)

  (load-relative (string-append "scenes/" n ".scm"))

  (-run-physics- #f)

  (unload)

  (let ([space (cp:space-new)]
	[scene-node (new-node root-node)])

    (cp:space-set-iterations space 20) ; 10 default, can be overwritten by scene
    (set! loaded-scene (init-scene scene-node space))

    ;; TODO free old space
    (set! the-space space))

  (-run-physics- #t)

  (first loaded-scene))


(comment

 (unload)

 (load-scene "1")

 (display "1")

 )
