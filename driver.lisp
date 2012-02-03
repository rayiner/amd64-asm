; driver.lisp
; Driver for assembler

(in-package "AMD64-ASM")

(defun assemble (source)
  (let ((obj (new-asmobj)))
    (iter (for def in source)
	  (ecase (first def)
	    (:proc (emit-code-def obj 
				  (third def) 
				  (second def)
				  (assemble-code (cdddr def))))
	    (:var (emit-data-def obj
				 (third def)
				 (second def)
				 (assemble-data (cdddr def))))))
    obj))

(defun assemble-and-output (source type filename)
  (let* ((obj (assemble source))
	 (buf (ecase type
		(:mach-o (generate-mach-o-obj obj)))))
    (store-vector-into-file buf filename)
    filename))
