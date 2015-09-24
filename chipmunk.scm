(import scheme chicken foreign srfi-4 bind bind-translator matchable)

#>
#include <chipmunk/chipmunk.h>
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
     ["cpVect" 'f64vector;'(c-pointer "cpVect")		;`(c-pointer ,type )
      ] ;; TODO convert to f64vector
     [other other]
     ))

(define (vect-type? arg-type)
   (match arg-type
     [('const c) (vect-type? c)]
     ["cpVect" #t]
     [other #f]))

 (define (vect-args? args.)
   (any vect-type? (map car args)))

 (define (convert-ret-type type)
   (match type
     [('const c) `(const ,(convert-arg-type c)) ]
     ["cpVect" 'void]
     [other other]))

(define (convert-args type-var-pairs add-destination)
  (let ([converted-args (map (lambda (type-var)
			       (list (convert-arg-type (car type-var))
				     (cadr type-var)))
			     type-var-pairs)])
    (if add-destination
	(cons '(f64vector dest) converted-args)
	converted-args)))

(define (spy . args)
   (pp args)
   (last args))

(define (convert-body body args)
  (let ([conv-args (apply append (map cdr (spy 'vect-args (filter (compose vect-type? car) args))))])
    (let loop ([x body])
      (match x
	[() '()]
	[ (h . t) (cons (if (member h conv-args)
			    (spy 'derefed= `(deref ,(conc "((cpVect*)"  h ")") ))
			    h)
			(loop t))]))))


(define (wrap-destination body)
  `(stmt
    (= "cpVect* r" "(cpVect*) dest")
    (= "*r" ,(spy 'actual-body= body))))

 ;; workaound to convert passing cpVect by value to passing cpVect by reference
 (define (f64struct-arg-transformer x rename)
   (display " x --------\n")
   (pp x)
   (display " rename --------\n")
   (spy
    (match x
      [(foreign-lambda* return-type args body)

       (let* ([name (string->symbol (car body))]

	     [argnames (apply append (map cdr args))]

	     ;; bind the foreign function with converted return type, args and body
	     [bound-foreign-lambda (bind-foreign-lambda*

				    `(,foreign-lambda*
					 ; return type : cpVect -> void
					 ,(convert-ret-type return-type)
					 ;; args: cpVect -> f64vector, prepend 'dest' arg if return type is cpVect
					 ,(convert-args args (vect-type? return-type)) ; args
				       ;;  body: cast and deref al cpVect args
				       ;; if a cpVect must be returned, assign the dest arg instead
				       ,((if (vect-type? return-type)
					     wrap-destination
					     identity)
					 (convert-body body args)))

				    rename)])

	 (if (vect-type? return-type)
	     ;; wrap in a function that provides the 'dest' arg
	     `(lambda ,argnames
		(,(rename 'let) ([dest (make-f64vector 2 0)])
		 (,bound-foreign-lambda ,@(cons 'dest argnames))
		 dest))

	     bound-foreign-lambda))]
      [other other]))))

(bind-options default-renaming: "" foreign-transformer: f64struct-arg-transformer)


(define cpv+* (foreign-lambda* void ( (f64vector dest) (f64vector x) (f64vector y))
	      "cpVect* r = (cpVect*) dest;
               *r = cpvadd( *( (cpVect*) x) , *( (cpVect*) y) );
"	      ))


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
