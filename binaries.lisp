; binaries.lisp
; Support for generating binary object files

(in-package "AMD64-ASM")

(defparameter *scopes* '(:int :ext :und))

(defstruct asmdef
  name
  scope
  bin)

(defstruct asmobj
  cdefs
  ddefs)

(defun new-asmdef (name sc bin)
  (make-asmdef :name name :scope sc :bin bin))

(defun new-asmobj ()
  (make-asmobj :cdefs (make-array 0 :fill-pointer t)
	       :ddefs (make-array 0  :fill-pointer t)))


(defun emit-code-def (obj name sc bin)
  (vector-push-extend (new-asmdef name sc bin)
		      (asmobj-cdefs obj)))

(defun emit-data-def (obj name sc bin)
  (vector-push-extend (new-asmdef name sc bin)
		      (asmobj-ddefs obj)))

(defstruct strtab
  vec
  table)

(defun new-strtab ()
  (make-strtab :vec (make-array 1 :fill-pointer t :initial-element 0) 
	       :table (make-hash-table)))

(defun strtab-intern (tab sym)
  (if (gethash sym (strtab-table tab))
      (gethash sym (strtab-table tab))
      (let ((ndx (fill-pointer (strtab-vec tab))))
	(iter (for char in-vector (symbol-name sym))
	      (vector-push-extend (char-code char) (strtab-vec tab)))
	(vector-push-extend 0 (strtab-vec tab))
	(setf (gethash sym (strtab-table tab)) ndx)
	ndx)))

(defun strtab-member? (tab sym)
  (gethash sym (strtab-table tab)))

(defun strtab-size (tab)
  (length (strtab-vec tab)))

(defun emit-strtab (frag strtab)
  (emit-byte-vector frag (strtab-vec strtab)))

(defgeneric emit-c-struct (struct frag))

(defgeneric sizeof-c-struct (struct))

(defmacro define-c-struct (name &body slots)
  (labels ((slot-accessor (name slot-name)
	     (catsym- name slot-name))
	   (emitter-for-spec (spec)
	     (ecase spec
	       (:byte 'emit-byte)
	       (:half 'emit-half)
	       (:word 'emit-word)
	       (:wide 'emit-wide)))
	   (emitter-for-slot (struct frag slot)
	     (ecase (length slot)
	       (2 `(,(emitter-for-spec (second slot)) 
		     ,frag 
		     (,(slot-accessor name (first slot)) ,struct)))
	       (3 `(let ((vec (,(slot-accessor name (first slot)) ,struct)))
		     (assert (<= (length vec) ,(third slot)))
		     (iter (for e in-vector vec)
			   (,(emitter-for-spec (second slot)) ,frag e))
		     (iter (for i from (length vec) below ,(third slot))
			   (,(emitter-for-spec (second slot)) ,frag 0)))))))
    `(progn 
       (defstruct ,name
	 ,@(iter (for slot in slots)
		 (collect (car slot))))
       (defmethod emit-c-struct ((xstruct ,name) xfrag)
	 ,@(iter (for slot in slots)
		 (collect (emitter-for-slot 'xstruct 'xfrag slot))))
       (defmethod sizeof-c-struct ((xstruct ,name))
	 ,(iter (for slot in slots)
		(ecase (length slot)
		  (2 (sum (specifier-width (second slot))))
		  (3 (sum (* (third slot) 
			     (specifier-width (second slot)))))))))))
