(module global *
  (import chicken scheme)
  (use (prefix chipmunk cp:))


  (define the-space #f)

  (define the-mouse-v (make-parameter (cp:v 0 0)))
  (define the-mouse-ball (make-parameter #f))
  (define -run-physics- (make-parameter #t))

  (define on-frame (make-parameter #f))


  )
