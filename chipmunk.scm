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


 (define (->reference-type type)
   (pp (cons 'reference-vect= type))
   (match type
     [('const c) (begin (pp 'const) `(const ,(->reference-type c))) ]
     ["cpVect" `(c-pointer ,type )]
     [other other]
     ))

 (define (refer-args type-var-pairs)
   (map (lambda (type-var)
	  (list (->reference-type (car type-var))
		(cdr type-var)))
	type-var-pairs))


 ;; workaound to convert passing cpVect by value to passing cpVect by reference
 (define (f64struct-arg-transformer x rename)
   (display " x --------\n")
   (pp x)
   (display " rename --------\n")
   (match x
     [(foreign-lambda* return-type args body)
      (begin (pp args) x)

      (pp (cons 'mpd= (refer-args args)))

      (bind-foreign-lambda*
       `(,foreign-lambda*
	    ,return-type ;; todo, if cpVect, return void
	    ,args ;; todo, if cpVect add 'destination' args
	  ,body ;; todo, if cpVect, deref cpVect args and set destination arg
	  )
       rename)]
     [other other]))

 )
(bind-options default-renaming: "" foreign-transformer: f64struct-arg-transformer)


(bind* "typedef unsigned char cpBool;

static inline cpBool cpvnear(const cpVect v1, const cpVect v2, const cpFloat dist)
{
	return cpvdistsq(v1, v2) < dist*dist;
}


static inline cpVect cpvadd(const cpVect v1, const cpVect v2)
{
	return cpv(v1.x + v2.x, v1.y + v2.y);
}



")
