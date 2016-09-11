(define (render-to-texture)
  ;;; http://www.opengl-tutorial.org/intermediate-tutorials/tutorial-14-render-to-texture/
  (let* ([fb-name 1]
	 [rendered-texture (u32vector 0)]
	 [depthrenderbuffer (u32vector 0)])


    ;; create framebuffer
    (gl:gen-framebuffers fb-name (u32vector 1 2 3))
    (gl:bind-framebuffer gl:+framebuffer+ fb-name)

    ;; create texture
    (gl:gen-textures 1 rendered-texture)
    (gl:bind-texture gl:+texture-2d+ (u32deref rendered-texture))
    (gl:tex-image-2d gl:+texture-2d+
    		     0
    		     gl:+rgb+
    		     1024 ;width
    		     768 ; height
    		     0 ; border
    		     gl:+rgb+ ; format
    		     gl:+unsigned-byte+ ;type
    		     #f ; empty
    		     )

    (gl:tex-parameteri gl:+texture-2d+ gl:+texture-mag-filter+ gl:+nearest+)
    (gl:tex-parameteri gl:+texture-2d+ gl:+texture-min-filter+ gl:+nearest+)

    ;; created renderbuffer
    (gl:gen-renderbuffers 1 depthrenderbuffer)	      ;
    (gl:bind-renderbuffer gl:+renderbuffer+ (u32deref depthrenderbuffer)) ;
    (gl:renderbuffer-storage gl:+renderbuffer+ gl:+depth-component+ 1024 768) ;
    (gl:framebuffer-renderbuffer gl:+framebuffer+ gl:+depth-attachment+ gl:+renderbuffer+ (u32deref depthrenderbuffer)) ;


    ;; configure framebuffer
    (gl:framebuffer-texture gl:+framebuffer+ gl:+color-attachment0+ (u32deref rendered-texture) 0)

    (list rendered-texture depthrenderbuffer)))


(define (u32deref x) (u32vector-ref x 0))
