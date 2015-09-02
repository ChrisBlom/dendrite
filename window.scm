(import chicken scheme foreign)
(use opengl-glew gl-math gl-utils gl-type noise (prefix glfw3 glfw:) )


(glfw:key-callback (lambda (window key scancode action mods)
                     (cond
                      [(and (eq? key glfw:+key-escape+) (eq? action glfw:+press+))
                       (glfw:set-window-should-close window #t)])))

(glfw:with-window (640 480 "Example" resizable: #f)
    (let loop ()
      (glfw:swap-buffers (glfw:window))
      (glfw:poll-events)
      (unless (glfw:window-should-close (glfw:window))
        (loop))))
