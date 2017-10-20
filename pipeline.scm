(module pipeline *
  (import chicken scheme)
  (use (prefix glfw3 glfw:)
       (prefix opengl-glew gl:)
       (prefix chipmunk cp:)
       gl-math
       gl-utils
       clojurian-syntax
       synth-utils
       (prefix utils utils/))
  (use reactive)


  (define (compile-shaders! vertex-string fragment-string)
    (let ([vertex-shader-id   (make-shader gl:+vertex-shader+ vertex-string)]
	  [fragment-shader-id (make-shader gl:+fragment-shader+ fragment-string)])
      ;; compile shader, set program parameter using
      (make-program (list vertex-shader-id fragment-shader-id))))

  (define -file-v1-  (file-cell "vertex-shaders/v1.glsl"))
  (define -file-v2-  (file-cell "vertex-shaders/v2.glsl"))
  (define -file-v3-  (file-cell "vertex-shaders/v3.glsl"))
  (define -file-vline-  (file-cell "vertex-shaders/line.glsl"))
  (define -file-vpoly-  (file-cell "vertex-shaders/poly.glsl"))

  (define *v1* (create-cell utils/read-all -file-v1-))
  (define *v2* (create-cell utils/read-all -file-v2-))
  (define *v3* (create-cell utils/read-all -file-v3-))
  (define *vline* (create-cell utils/read-all -file-vline-))

  (define *vpoly* (create-cell utils/read-all -file-vpoly-))

  (define *fragment* (create-cell utils/read-all (file-cell "fragment-shaders/simple.glsl")))
  (define *fragment-constant* (create-cell utils/read-all (file-cell "fragment-shaders/constant.glsl")))
  (define *fragment-line* (create-cell utils/read-all (file-cell "fragment-shaders/line.glsl")))
  (define *fragment-poly* (create-cell utils/read-all (file-cell "fragment-shaders/poly.glsl")))

  ;; paused as compile-shader can only be used after gl is initialized
  (define program1     (create-paused-cell compile-shaders! *v1* *fragment*))
  (define program2     (create-paused-cell compile-shaders! *v2* *fragment*))
  (define program3     (create-paused-cell compile-shaders! *v3* *fragment-constant*))
  <(define program-line (create-paused-cell compile-shaders! *vline* *fragment-line*))
  (define program-poly (create-paused-cell compile-shaders! *vpoly* *fragment-poly*))

					;(use soil)

  ;; ;; returns a texture id, requires gl init
  ;; (define (load-texture image-path)
  ;;   (let ([tex (u32vector 0)])
  ;;     (gl:gen-textures 1 tex)
  ;;     (let ([tex-id (u32vector-ref tex 0)])
  ;;       (load-ogl-texture image-path force-channels/auto tex-id texture/repeats)
  ;;       )))



  ;; (define noise-image-file (file-cell "resources/img/noise.jpg"))

  ;; (define noise-image-texture (create-paused-cell load-texture noise-image-file))



  ;; after open gl init
  (define (start-all-cells)
    (cell-unpause program1)
    (cell-unpause program2)
    (cell-unpause program3)
    (cell-unpause program-line)
    (cell-unpause program-poly)
					;   (cell-unpause noise-image-texture)
    )
  )
