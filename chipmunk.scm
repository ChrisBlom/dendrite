(import chicken bind)

#>
#include <chipmunk/chipmunk_private.h>
#include <chipmunk/chipmunk.h>
<#

;; binding transformer
(define transformer (parameter #f))


(begin-for-syntax
 (import chicken scheme)
 (include "struct-by-value-transformer.scm"))

;; these headers are modified for compatibility with chicken bind
(bind-include-path "./include")

;; (bind-rename/pattern "^cp" "")
;; (bind-rename/pattern "make-cp" "make")

(bind-options default-renaming: ""
	      foreign-transformer: struct-by-value-transformer)

(bind-file "include/chipmunk_types.h")

(bind-file "include/cpVect.h")
(bind-file "include/cpBB.h")
(bind-file "include/cpSpatialIndex.h")
(bind-file "include/cpTransform.h")
(bind-file "include/cpArbiter.h")
(bind-file "include/cpConstraint.h")
(bind-file "include/cpShape.h")
(bind-file "include/cpSpace.h")

;;(bind-file "include/chipmunk.h")
