(import scheme chicken foreign srfi-4 bind bind-translator matchable)

#>
#include <chipmunk/chipmunk.h>
<#


(define type-size
  '(("cpVect" . 2)
    ("cpMat2x2" . 4)))

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
     ["cpVect" 'f64vector]
     [('struct "cpVect") (convert-arg-type "cpVect")  ]
     ["cpMat2x2" 'f64vector]
     [other other]))

 (define (vect-type? arg-type)
   ;; (spy 'vect-type? arg-type  '=?  (convert-arg-type arg-type))
   (not (equal? arg-type
		(convert-arg-type arg-type))))

 (define (vect-args? args.)
   (any vect-type? (map car args)))

 (define (convert-ret-type type)
   (if (vect-type? type)
       'void
       type))

 (define (convert-args type-var-pairs add-destination)
   (spy 'convert-args= type-var-pairs '=>
	(let ([converted-args (map (lambda (type-var)
				     (list (convert-arg-type (car type-var))
					   (cadr type-var)))
				   type-var-pairs)])
	  (if add-destination
	      (cons '(f64vector dest) converted-args)
	      converted-args))))

(define (spy . args)
   (pp args)
   (last args))

(define (convert-body body args return-type)
  (let ([conv-args (apply append (map cdr (filter (compose vect-type? car) args)))]
	[arg->type (fold (lambda (arg-pair acc)
			     (let* ([var (second arg-pair)]
				    [type (extract-type (first arg-pair))])
			       (alist-cons var type acc)))
			   '()
			   args)])
    (let loop ([x body])
      (match x
	[() '()]
	[ (h . t)
	  (cons (if (member h conv-args)
		    (spy 'derefed= `(deref ,(conc "((" (spy (alist-ref h arg->type)) "*)"  h ")") ))
		    h)
		(loop t))]))))

;; (define cpv+* (foreign-lambda* void ( (f64vector dest) (f64vector x) (f64vector y))
;; 	      "cpVect* r = (cpVect*) dest;
;;                *r = cpvadd( *( (cpVect*) x) , *( (cpVect*) y) );"))


(define (extract-type type)
  (match type
    [('struct x) (extract-type x)]
    [('const x) (extract-type x)]
    [other other]))

(define (wrap-destination body return-type)
  `(stmt
    (= ,(string-append return-type "* r")
       ,(string-append "(" return-type"*) dest"))
    (= "*r" ,(spy 'actual-body= body))))

 ;; workaound to convert passing cpVect by value to passing cpVect by reference
 (define (f64struct-arg-transformer x rename)
   (display " x --------\n")
   (spy 'before= x)
   (display " rename --------\n")
   (spy 'rename=
    (match x
      [(foreign-lambda* return-type args body)
       (begin
	 (spy 'body= body)
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
						(lambda (x) (wrap-destination x (extract-type return-type)))
						identity)
					    (convert-body body args (extract-type return-type))))

				       rename)])

	   (if (vect-type? return-type)
	       ;; wrap in a function that provides the 'dest' arg
	       `(lambda ,argnames
		  (,(rename 'let) ([dest (make-f64vector 2 0)])
		   (,bound-foreign-lambda ,@(cons 'dest argnames))
		   dest))

	       bound-foreign-lambda)))]
      [other other]))))


(bind-include-path "./include")

(bind-options ;default-renaming: ""
	      foreign-transformer: f64struct-arg-transformer)

;; (bind* "





;;   static inline cpVect cpvadd1(const cpVect v1, const cpVect v2)
;;   {
;;   	return cpv(v1.x + v2.x, v1.y + v2.y);
;;   }


;; /// Convenience constructor for cpVect structs.
;; static inline cpVect cpvX(const double x, const double y)
;; {
;; 	cpVect v = {x, y};
;; 	return v;
;; }

;; static inline double cpvdotX(const cpVect v1, const cpVect v2)
;; {
;; 	return v1.x*v2.x + v1.y*v2.y;
;; }

;; static inline cpVect cpvperpX(const cpVect v)
;; {
;; 	return cpv(-v.y, v.x);
;; }

;;   ")


;; (bind-file "include/chipmunk_types.h")
; (bind-file "include/cpVect.h")

(bind-file "include/cpv.h")

(define x (cpv 1. 1.))
(define y (cpv 1. 1.))
