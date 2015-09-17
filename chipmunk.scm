(import scheme chicken foreign srfi-4 bind bind-translator matchable)

#>
#include <chipmunk/chipmunk.h>
typedef unsigned char cpBool;
<#

(define-foreign-type cpVect "cpVect")

(define-foreign-type cpVect* f64vector
  (lambda (x)
    (assert (= 2 (f64vector-length x)))
    x))


(define cpv* (foreign-lambda* void ( (f64vector dest) (float x) (float y))
	      "cpVect* r = (cpVect*) dest;
               r->x = x;
               r->y = y;
"	      ))

(define cpv+* (foreign-lambda* void ( (f64vector dest) (f64vector x) (f64vector y))
	      "cpVect* xx = (cpVect*) x;
               cpVect* yy = (cpVect*) y;
               cpVect* r = (cpVect*) dest;
               *r = cpvadd(*xx,*yy);
"	      ))

(define (cpv x y)
  (let ([ret (make-f64vector 2 0)])
    (cpv* ret x y)
    ret))

(define (cpv+ x y)
  (let ([ret (make-f64vector 2 0) ])
    (cpv+* ret x y)
    ret))


(begin-for-syntax
 (import chicken matchable debug bind bind-translator)
 (import-for-syntax bind-translator matchable debug)
					;(include "acorn-transformer.scm")

 (define (convert-arg-type type)
   (match type
     [('const c) `(const ,(convert-arg-type c)) ]
     ["cpVect" `(c-pointer ,type )]
     [other other]
     ))

(define (vect-type? arg-type)
   (match arg-type
     [('const c) (vect-type? c)]
     ["cpVect" #t]
     [other other]))

 (define (vect-args? args.)
   (any vect-type? (map car args)))

 (define (convert-ret-type type)
;   (pp (cons 'reference-vect= type))
   (match type
     [('const c) `(const ,(convert-arg-type c)) ]
     ["cpVect" 'void]
     [other other]
     ))
; (convert-ret-type "cpVect") ;=> void

(define (convert-args type-var-pairs add-destination)
  (let ([converted-args (map (lambda (type-var)
			       (list (convert-arg-type (car type-var))
				     (cadr type-var)))
			     type-var-pairs)])
    (if add-destination
	(cons '((c-pointer "cpVect")  dest) converted-args)
	converted-args)))

 (define (spy x)
   (pp x)
   x
   )

 ;; workaound to convert passing cpVect by value to passing cpVect by reference
 (define (f64struct-arg-transformer x rename)
   (display " x --------\n")
   (pp x)
   (display " rename --------\n")
   (spy
    (match x
      [(foreign-lambda* return-type args body)
       (begin (pp args) x)

					;(pp (cons 'mpd= (convert-args args)))

       (bind-foreign-lambda*
	`(,foreign-lambda*
	     ,(convert-ret-type return-type)
	     ,(spy  (convert-args args (vect-type? return-type)))
	   ,body ;; todo, if cpVect, deref cpVect args and set destination arg
	   )
	rename)]
      [other other])))

 )
(bind-options default-renaming: "" foreign-transformer: f64struct-arg-transformer)


(bind* "
typedef unsigned char cpBool;


static inline cpBool cpvnear1(const cpVect v1, const cpVect v2, const double dist)
{
	return cpvdistsq(v1, v2) < dist*dist;
}


static inline cpVect cpvadd1(const cpVect v1, const cpVect v2)
{
	return cpv(v1.x + v2.x, v1.y + v2.y);
}



")
