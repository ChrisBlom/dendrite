
;; (name root-note space)
(define loaded-scene #f)

(define (unload-scene scene)
  (display "TODO")
  (newline))

(define (load-scene n)

  (load-relative (string-append "scenes/" n ".scm"))

  (when loaded-scene
    (unload-scene loaded-scene))

  (let ([space (cp:space-new)])
    (cp:space-set-iterations space 10) ; 10 default
    (set! loaded-scene (init-scene root-node space)))

  loaded-scene)


(comment

 (load-scene "1")

 )
