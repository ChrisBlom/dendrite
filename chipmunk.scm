(import scheme chicken foreign srfi-4 bind bind-translator matchable)

#>
#include <chipmunk/chipmunk_private.h>
#include <chipmunk/chipmunk.h>
<#

(define type-size
  '(("cpVect" . 2)
    ("cpMat2x2" . 4)))

(define cpv* (foreign-lambda* void ( (f64vector dest) (float x) (float y))
	      "cpVect* r = (cpVect*) dest;
               r->x = x;
               r->y = y;"))

(define (cpv x y)
  (let ([ret (make-f64vector 2 0)])
    (cpv* ret x y)
    ret))

;; binding transformer
(begin-for-syntax
 (import chicken matchable debug bind bind-translator)
 (import-for-syntax bind-translator matchable debug)
					;(include "acorn-transformer.scm")
 (define (convert-arg-type type)
   (match type
     [('const c) `(const ,(convert-arg-type c)) ]
     ["cpVect" 'f64vector]
     [('struct "cpVect") (convert-arg-type "cpVect")  ]
     [('struct "cpBB") (convert-arg-type "cpVect")  ]
     ["cpBB" (convert-arg-type "cpVect")  ]
     ["cpTransform" (convert-arg-type "cpVect")  ]
     [('struct "cpTransform") (convert-arg-type "cpVect")  ]
     ["cpMat2x2" 'f64vector]
     ["uintptr_t" 'long]
;     ["cpContactPointSet" '(pointer cpContactPointSet)]		;
     [other other]))

 (define (vect-type? arg-type)
   ;; (spy 'vect-type? arg-type  '=?  (convert-arg-type arg-type))
   (not (equal? arg-type
		(convert-arg-type arg-type))))

 (define (vect-args? args)
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
		    `(deref ,(conc "((" (alist-ref h arg->type) "*)"  h ")") )
		    h)
		(loop t))]))))

(define (extract-type type)
  (match type
    [('struct x) (extract-type x)]
    [('const x) (extract-type x)]
    [other other]))

(define (wrap-destination body return-type)
  `(stmt
    (= ,(string-append return-type "* r_")
       ,(string-append "(" return-type"*) dest"))
    (= "*r_" ,body )))

 ;; workaound to convert passing cpVect by value to passing cpVect by reference
 (define (f64struct-arg-transformer x rename)
   (display " x --------\n")
   (spy 'before= x)
   (display " rename --------\n")
   (spy 'rename=
    (match x

      ;; [(and form
      ;; 	    (foreign-lambda* return-type args body)
      ;; 	    (not (? vect-type? return-type))
      ;; 	    (not (? vect-args? args)))
      ;;  (spy 'form form)
      ;;  (spy 'foreign
      ;; 	    (bind-foreign-lambda*
      ;; 	     `(foreign-lambda* ,return-type ,args ,body)
      ;; 	     rename
      ;; 	     ))]

      [(and form (foreign-lambda* return-type args body))
       (let* ([name (string->symbol (car body))]

	      [rename- (lambda (x) (spy 'after (rename (spy 'before x))))]

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

				     rename-)])
	 (if (vect-type? return-type)
	     ;; wrap in a function that provides the 'dest' arg
	     `(lambda ,argnames
		(,(rename- 'let) ([dest (make-f64vector ,(match (spy (extract-type return-type))
							  ["cpBB" 4]
							  ["cpVect" 2]
							  ["cpMat2x2" 4]
							  ["cpTransform" 6]) 0)])
		 (,bound-foreign-lambda ,@(cons 'dest argnames))
		 dest))
	     (if (vect-args? args)
		 bound-foreign-lambda
		 bound-foreign-lambda)))]



      [other other]))))

;; modified headers for compatibility with chicken bind
(bind-include-path "./include")

;; TODO support binding of functions
(bind-options ;default-renaming: ""
	      foreign-transformer: f64struct-arg-transformer)

;; testing:
;; (bind* "


;;   ")

(bind-file "include/chipmunk_types.h")
(bind-file "include/cpVect.h")
(bind-file "include/cpBB.h")


(bind-file "include/cpSpatialIndex.h")
; (bind-file "include/cpTransform.h")

;;(bind-file "include/chipmunk.h")

(bind-file "include/cpArbiter.h")
(bind-file "include/cpConstraint.h")




(define x (cpv 1. 1.))
(define y (cpv 1. 1.))
