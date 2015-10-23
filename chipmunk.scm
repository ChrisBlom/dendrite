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

 (define (extract-type type)
   (match type
     [('struct x) (extract-type x)]
     [('const x) (extract-type x)]
     [other other]))

 (define (map-type f type)
   (match type
     [('const c) `(const ,(map-type f c))]
     [('struct c) `(struct ,(map-type f c))]
     [atomic (f atomic)]))

 ; (extract-type '(struct (const "cpVect")))

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

 (define (convert-arg-type? type)
   (match type
     [('const c) (convert-arg-type? c) ]
     [('struct x) (convert-arg-type? x )  ]
     ["cpVect" #t]
     ["cpBB" #t]
     ["cpTransform" #t]
     ["cpMat2x2" #t]
     ["uintptr_t" #t]
     [other #f]))

 (define (convert-return-type? type)
   (match type
     [('const c) (convert-return-type? c) ]
     [('struct x) (convert-return-type? x)  ]
     ["cpVect" #t]
     ["cpBB" #t]
     ["cpTransform" #t]
     ["cpMat2x2" #t]
     ["uintptr_t" #f] ;; <-
     [other #f]))

 ; (convert-return-type? '(struct "cpTransform"))

 (define (convert-args? args)
   (any convert-arg-type? (map car args)))

 (define (convert-ret-type type)
   (spy 'CONVERT-RET-TYPE type
	(match (extract-type (convert-arg-type type))
	  ['f64vector 'void]
	  ["uintptr_t" 'long]
	  [other other])))

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
   (let ([conv-args (apply append (map cdr (filter (compose convert-arg-type? car) args)))]
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


 (define (wrap-destination body return-type)
   `(stmt
     (= ,(string-append return-type "* r_")
	,(string-append "(" return-type"*) dest"))
     (= "*r_" ,body )))

 (define (convert? args ret)
   (or (convert-return-type? ret)
       (convert-args? args)))

 ;; workaound to convert passing cpVect by value to passing cpVect by reference
 (define (f64struct-arg-transformer x rename)
   (display "================================================================================")
   (spy 'before= x)
   (display "--\n")
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

	     [argnames (apply append (map cdr args))]

	     ;; bind the foreign function with converted return type, args and body
	     [bound-foreign-lambda (bind-foreign-lambda*

				    `(,foreign-lambda*
					; return type : cpVect -> void
					 ,(spy 'CONVERT-RET-TYPE return-type (convert-ret-type return-type))
					 ;; args: cpVect -> f64vector, prepend 'dest' arg if return type is cpVect
					 ,(convert-args args (convert-return-type? return-type)) ; args
				       ;;  body: cast and deref al cpVect args
				       ;; if a cpVect must be returned, assign the dest arg instead
				       ,((if (convert-return-type? return-type)
					     (lambda (x) (wrap-destination x (extract-type return-type)))
					     identity)
					 (convert-body body args (extract-type return-type))))

				    rename)])
	(spy 'AFTER=
	     (if (convert-return-type? return-type)
		 ;; wrap in a function that provides the 'dest' arg
		 `(lambda ,argnames
		    (,(rename 'let) ([dest (make-f64vector ,(match (spy 'EXTRACT-TYPE return-type (extract-type return-type))
							      ["cpBB" 4]
							      ["cpVect" 2]
							      ["cpMat2x2" 4]
							      ["cpTransform" 6]) 0)])
		     (,bound-foreign-lambda ,@(cons 'dest argnames))
		     dest))
		 (if (convert-args? args)
		     bound-foreign-lambda
		     bound-foreign-lambda))))]

;     [other (bind-foreign-lambda* other rename)]
     )))

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
(bind-file "include/cpTransform.h")
(bind-file "include/cpArbiter.h")
(bind-file "include/cpConstraint.h")
(bind-file "include/cpShape.h")
(bind-file "include/cpSpace.h")

;(bind-file "include/chipmunk.h")
