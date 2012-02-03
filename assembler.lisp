; amd64-asm.lisp
; Assembler for AMD64 code. Contains assembler, linker, stitcher.

(in-package "AMD64-ASM")

(defstruct asmrel
  symbol
  offset
  width
  type)

(defstruct asmlbl
  symbol
  offset)

(defstruct asmfrag
  buffer
  relocs
  labels
  buffer-chkpt
  relocs-chkpt
  sdi-source)

(defstruct asmbin
  buffer
  relocs)

(defstruct asmfun
  frags)

(defun new-asmfrag ()
  (make-asmfrag :buffer (make-array 0 :fill-pointer t)
		:relocs (make-array 0 :fill-pointer t)
		:labels (make-array 0 :fill-pointer t)))

(defun new-asmbin ()
  (make-asmbin :buffer (make-array 0 :fill-pointer t)
	       :relocs (make-array 0 :fill-pointer t)))

(defun new-asmfun ()
  (make-asmfun :frags (make-array 0 :fill-pointer t)))
 
(defun reset-asmfrag (ln)
  (setf (fill-pointer (asmfrag-relocs ln)) 0)
  (setf (fill-pointer (asmfrag-buffer ln)) 0))

(defun checkpoint-asmfrag (ln)
  (setf (asmfrag-buffer-chkpt ln)
	(fill-pointer (asmfrag-buffer ln)))
  (setf (asmfrag-relocs-chkpt ln)
	(fill-pointer (asmfrag-relocs ln))))

(defun restore-asmfrag (ln)
  (setf (fill-pointer (asmfrag-buffer ln))
	(asmfrag-buffer-chkpt ln))
  (setf (fill-pointer (asmfrag-relocs ln))
	(asmfrag-relocs-chkpt ln)))

(defun asmfrag-empty? (ln)
  (eql (fill-pointer (asmfrag-buffer ln)) 0))

(defun add-asmfrag (frag fun)
  (vector-push-extend frag (asmfun-frags fun)))

(defun emit-byte (ln b)
  (vector-push-extend b (asmfrag-buffer ln)))

(defun emit-bytes (ln w n)
  (iter (for i from 0 below n)
	(emit-byte ln (logand #xFF (ash w (- (* i 8)))))))

(defun emit-half (ln w)
  (emit-bytes ln w 4))

(defun emit-word (ln w)
  (emit-bytes ln w 8))

(defun emit-wide (ln w)
  (emit-bytes ln w 16))

(defun emit-ascii (ln str &optional field)
  (iter (for char in-vector str)
	(emit-byte ln (char-code char)))
  (when field
    (iter (for i from (length str) below field)
	  (emit-byte ln 0))))

(defun emit-byte-vector (ln vec)
  (iter (for byte in-vector vec)
	(emit-byte ln byte)))

(defun emit-reloc (ln sym width type)
  (vector-push-extend (make-asmrel :symbol sym  
				   :offset (fill-pointer (asmfrag-buffer ln)) 
				   :width width
				   :type type)
		      (asmfrag-relocs ln)))

(defun record-label (ln sym)
  (vector-push-extend (make-asmlbl :symbol sym 
				   :offset (fill-pointer (asmfrag-buffer ln)))
		      (asmfrag-labels ln)))

(defun save-sdi-source (ln line)
  (setf (asmfrag-sdi-source ln) line)) 

(defun label-line? (ln)
  (symbolp ln))

(defun translate-label-ref (insn)
  (list (first insn) (list :byte (second insn) 0)))

(defun encode (source)
  (let ((fun (new-asmfun))
	(frag (new-asmfrag)))
    (iter (for line in source)
	  (cond 
	    ((label-line? line) (record-label frag line))
	    ((sdi? line)
	     (checkpoint-asmfrag frag)
	     (let ((tx-source (translate-label-ref line) frag))
	       (save-sdi-source frag tx-source)
	       (encode-insn tx-source frag))
	     (add-asmfrag frag fun)
	     (setf frag (new-asmfrag)))
	    (t (encode-insn line frag))))
    (unless (asmfrag-empty? frag)
      (add-asmfrag frag fun))
    fun))

(defun compute-labels (fun)
  (let ((labels (make-hash-table))
	(ip 0))
    (iter (for frag in-vector (asmfun-frags fun))
	  (iter (for label in-vector (asmfrag-labels frag))
		(setf (gethash (asmlbl-symbol label) labels)
		      (+ ip (asmlbl-offset label))))
	  (setf ip (+ ip (length (asmfrag-buffer frag)))))
    labels))

(defun sdi-target (insn)
  (second insn))

(defun sdi-target-specifier (insn)
  (first (second insn)))

(defun sdi-target-symbol (insn)
  (second (second insn)))

(defun submaximal-sdi? (insn)
  (and (sdi? insn)
       (not (eql (sdi-target-specifier insn) :half))))

(defun jump-in-range (insn delta)
  (<= (signed-width delta)
       (specifier-width (sdi-target-specifier insn))))

(defun widen-sdi (sdi)
  (list (first sdi) (list (specifier-next (sdi-target-specifier sdi))
			  (sdi-target-symbol sdi) 0)))

(defun resolve-sdi (sdi labels ip)
  (list (first sdi) (- (gethash (sdi-target-symbol sdi) labels) ip)))

(defun link-jumps (fun labels)
  (let ((ip 0)
	(relaxed))
    (iter (for frag in-vector (asmfun-frags fun))
	  (setf ip (+ ip (length (asmfrag-buffer frag))))
	  (when (not (asmfrag-sdi-source frag)) (next-iteration))
	  (let* ((sdi (asmfrag-sdi-source frag))
		 (del (- ip (gethash (sdi-target-symbol sdi) labels))))
	    (if (jump-in-range sdi del)
		(progn (setf (asmfrag-sdi-source frag) 
			     (resolve-sdi sdi labels ip))
		       (restore-asmfrag frag)
		       (encode-insn (asmfrag-sdi-source frag) frag))
		(progn (setf (asmfrag-sdi-source frag) (widen-sdi sdi))
		       (restore-asmfrag frag)
		       (encode-insn (asmfrag-sdi-source frag) frag)
		       (setf relaxed t)))))
    relaxed))

(defun link (fun)
  (to-fixpoint
    (when (link-jumps fun (compute-labels fun))
      (mark-changed)))
  fun)

(defun update-reloc-offsets (bin frag)
  (let ((base (fill-pointer (asmbin-buffer bin))))
    (iter (for reloc in-vector (asmfrag-relocs frag))
	  (incf (asmrel-offset reloc) base))))

(defun stitch (fun)
  (let ((bin (new-asmbin)))
    (iter (for frag in-vector (asmfun-frags fun))
	  (update-reloc-offsets bin frag)
	  (extend-vector-with-vector (asmfrag-buffer frag)
				     (asmbin-buffer bin))
	  (extend-vector-with-vector (asmfrag-relocs frag)
				     (asmbin-relocs bin)))
    bin))

(defun assemble-code (source)
  (handler-case 
      (stitch (link (encode source)))
    (assertion-failed (as) (format t "Assertion failed: ~A~%"
				   (assertion-failed-check as)))
    (encoding-error (ee) (format t "Error encoding form: ~A~%"
				 (encoding-error-form ee)))))

(defun encode-known-data (datum)
  (let ((frag (new-asmfrag)))
    (destructuring-bind (spec value) datum
      (emit-bytes frag value (specifier-width spec)))
    frag))

(defun encode-unknown-data (datum)
  (let ((frag (new-asmfrag)))
    (destructuring-bind (spec sym addn) datum
      (emit-reloc frag sym (specifier-width spec) :abs)
      (emit-bytes frag addn (specifier-width spec)))
    frag))

(defun aggregate (source)
  (let ((fun (new-asmfun)))
    (iter (for datum in source)
	  (ecase (length datum)
	    (2 (add-asmfrag (encode-known-data datum) fun))
	    (3 (add-asmfrag (encode-unknown-data datum) fun))))
    fun))	  

(defun assemble-data (source) 
  (handler-case
      (stitch (aggregate source))
    (assertion-failed (as) (format t "Assertion failed: ~A~%"
				   (assertion-failed-check as)))
    (encoding-error (ee) (format t "Error aggregating form: ~A~%"
				 (encoding-error-form ee)))))

