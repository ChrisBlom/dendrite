;;;; ringbuffer.import.scm - GENERATED BY CHICKEN 4.9.0.1 -*- Scheme -*-

(eval '(import chicken scheme))
(##sys#register-compiled-module
  'ringbuffer
  (list)
  '((new-ringbuffer . ringbuffer#new-ringbuffer)
    (ringbuffer-advance . ringbuffer#ringbuffer-advance)
    (ringbuffer-set! . ringbuffer#ringbuffer-set!)
    (ringbuffer-length . ringbuffer#ringbuffer-length)
    (ringbuffer-set-start! . ringbuffer#ringbuffer-set-start!)
    (ringbuffer-get . ringbuffer#ringbuffer-get)
    (ringbuffer-get-start . ringbuffer#ringbuffer-get-start)
    (ringbuffer-get-end . ringbuffer#ringbuffer-get-end)
    (ringbuffer->vector . ringbuffer#ringbuffer->vector)
    (vector->ringbuffer . ringbuffer#vector->ringbuffer)
    (list->ringbuffer . ringbuffer#list->ringbuffer))
  (list)
  (list))

;; END OF FILE
