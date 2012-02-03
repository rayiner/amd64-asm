; encoders.lisp
; Encoders for AMD64 instruction set.

(in-package "AMD64-ASM")

; maybe move condition definitions somewhere else
(define-condition assembler-error (error)
  (()))

(define-condition encoding-error (assembler-error)
  ((form :initarg :form :reader encoding-error-form)))

(define-condition assertion-failed (encoding-error)
  ((check :initarg :check :reader assertion-failed-check)))

; make this more sophisticated for error handling later
(defmacro with-checks (pred &body body)
  `(if ,pred 
       (progn ,@body)
       (error 'assertion-failed :check ',pred)))

(defparameter *byte-regs* '(:al :bl :cl :dl :sil :dil :bpl :spl
			    :r8b :r9b :r10b :r11b :r12b :r13b :r14b :r15b))

(defparameter *half-regs* '(:eax :ebx :ecx :edx :esi :edi :ebp :esp
			    :r8d :r9d :r10d :r11d :r12d :r13d :r14d :r15d))

(defparameter *word-regs* '(:rax :rbx :rcx :rdx :rsi :rdi :rbp :rsp
			    :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15))

(defparameter *vec-regs* '(:xmm0 :xmm3 :xmm1 :xmm2 :xmm6 :xmm7 :xmm5 :xmm4
			   :xmm8 :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 
			   :xmm15))

(defparameter *sdis* '(:jo :jno :jb :jnb :jz :jnz :jbe :jnbe
		       :js :jns :jp :jnp :jl :jge :jle :jg :jmp))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *prefixes* '(#x66 #x67 #x64 #x65 #xF0 #xF3 #xF2)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *encoders* nil))

; info about operands of an instruction
(defstruct oprinfo
  oc.ext
  modrm.mod
  modrm.reg
  modrm.rm
  sib.scale
  sib.index
  sib.base
  disp
  imm
  imm.bytes
  imm.rel-type
  imm.rel-addn
  disp.bytes
  disp.rel-type
  disp.rel-addn)

; info about the opcodes of an instruction
(defstruct ocinfo
  override?
  prefixes
  opcodes)

(defun new-ocinfo ()
  (make-ocinfo :opcodes (make-array 0 :fill-pointer t)
	       :prefixes (make-array 0 :fill-pointer t)))

(defun specifier-width (spec)
  (case spec
    (:byte 1)
    (:half 4)
    (:word 8)
    (:wide 16)))

(defun specifier-next (spec)
  (case spec
    (:byte :half)
    (:half :word)
    (:word :word)))

(defun register-number (reg)
  (let ((rnums '(0 3 1 2 6 7 5 4 8 9 10 11 12 13 14 15)))
    (let ((idx (or (position reg *byte-regs*)
		   (position reg *half-regs*)
		   (position reg *word-regs*)
		   (position reg *vec-regs*))))
      (when idx (nth idx rnums)))))

(defun reg? (operand)
  (register-number operand))

(defun byte-reg? (reg)
  (member reg *byte-regs*))

(defun half-reg? (reg)
  (member reg *half-regs*))

(defun word-reg? (reg)
  (member reg *word-regs*))

(defun xmm-reg? (reg)
  (member reg *vec-regs*))

(defun same-reg? (rega regb)
  (eql (register-number rega) (register-number regb)))

(defun immediate? (operand)
  (or (integerp operand)
      (and (listp operand)
	   (or (and (eql (length operand) 2)
		    (symbolp (first operand))
		    (symbolp (second operand)))
	       (and (eql (length operand) 3)
		    (symbolp (first operand))
		    (symbolp (second operand))
		    (integerp (third operand))
		    (or (<= (signed-width (third operand))
			    (specifier-width (first operand)))
			(<= (unsigned-width (third operand))
			    (specifier-width (first operand)))))))))

(defun immediate-width (operand)
  (if (integerp operand)
      (signed-width operand)
      (specifier-width (first operand))))

(defun byte-immediate? (operand)
  (and (immediate? operand) (<= (immediate-width operand) 1)))

(defun short-immediate? (operand)
  (and (immediate? operand) (<= (immediate-width operand) 2)))

(defun half-immediate? (operand)
  (and (immediate? operand) (<= (immediate-width operand) 4)))

(defun word-immediate? (operand)
  (and (immediate? operand) (<= (immediate-width operand) 8)))

(defun mem? (operand)
  (and (listp operand)
       (eql (length operand) 5)
       (symbolp (first operand))
       (symbolp (second operand))
       (symbolp (third operand))
       (integerp (fourth operand))
       (immediate? (fifth operand))))

(defun byte-mem? (operand)
  (and (mem? operand) (eql (first operand) :byte)))

(defun half-mem? (operand)
  (and (mem? operand) (eql (first operand) :half)))

(defun word-mem? (operand)
  (and (mem? operand) (eql (first operand) :word)))

(defun wide-mem? (operand)
  (and (mem? operand) (eql (first operand) :wide)))

(defun sdi? (insn)
  (and (member (first insn) *sdis*)
       (symbolp (second insn))))

(defun compose-rex (w r x b)
  (with-checks (and (< w 2) (< r 2) (< x 2) (< b 2))
    (+ #x40 b (ash x 1) (ash r 2) (ash w 3))))

(defun decode-rex (r)
  (with-checks (integerp r)
    (list :w (ash (logand r #x8) -3)
	  :r (ash (logand r #x4) -2)	  
	  :x (ash (logand r #x2) -1)
	  :b (logand r #x1))))

(defun compose-modrm (mod reg rm)
  (with-checks (and (< mod 4) (< reg 8) (< rm 8))
    (+ rm (ash reg 3) (ash mod 6))))

(defun decode-modrm (m)
  (with-checks (integerp m)
    (list :mod (logand (ash m -6) #x3)
	  :reg (logand (ash m -3) #x7)
	  :rm (logand m #x7))))

(defun compose-sib (scale index base)
  (with-checks (and (< scale 8) (< index 8) (< base 8))
    (+ base (ash index 3) (ash scale 6))))

(defun decode-sib (s)
  (with-checks (integerp s)
    (list :scale (logand (ash s -6) #x3)
	  :index (logand (ash s -3) #x7)
	  :base (logand s #x7))))

(defun add-reg-operand (insn reg where)
  (with-checks (reg? reg)
    (let ((num (register-number reg)))
      (ecase where
	(reg (setf (oprinfo-modrm.reg insn) num))
	(rm (setf (oprinfo-modrm.mod insn) #x3)
	     (setf (oprinfo-modrm.rm insn) num))
	(op (setf (oprinfo-oc.ext insn) num))))))

(defun add-immediate-operand (insn imm width type)
  (with-checks (immediate? imm)
    (if (integerp imm)
	(progn
	  (setf (oprinfo-imm insn) imm)
	  (setf (oprinfo-imm.bytes insn) width))
	(progn
	  (setf (oprinfo-imm insn) (second imm))
	  (setf (oprinfo-imm.bytes insn) width)
	  (setf (oprinfo-imm.rel-type insn) type)
	  (setf (oprinfo-imm.rel-addn insn) (or (third imm) 0))))))
  
(defun add-opcode-extension (insn subcode)
  (with-checks (integerp subcode)
    (setf (oprinfo-modrm.reg insn) subcode)))

(defun modrm.mod-for-disp (disp)
  (cond
    ((eql disp 0) 0)
    ((integerp disp)
     (ecase (signed-width disp)
       (1 1)
       ((2 4) 2)))
    ((listp disp)
     (ecase (first disp)
       (:byte 1)
       (:half 2)))))

(defun sib.scale-for-scale (scale)
  (ecase scale
    (1 0)
    (2 1)
    (4 2)
    (8 3)))

(defun add-sib.index (insn index scale)
  (setf (oprinfo-sib.scale insn) (sib.scale-for-scale scale))
  (if index
      (setf (oprinfo-sib.index insn) (register-number index))
      (setf (oprinfo-sib.index insn) #x04)))

(defun compute-disp.bytes (disp bytes)
  (or bytes 
      (let ((sz (immediate-width disp)))
	(if (eql sz 2) 4 sz))))

(defun add-disp (insn disp type &optional bytes)
  (if (or (not (eql disp 0)) bytes)
      (let ((sz (compute-disp.bytes disp bytes)))
	(if (integerp disp)
	    (progn
	      (setf (oprinfo-disp insn) disp)
	      (setf (oprinfo-disp.bytes insn) sz))
	    (progn
	      (setf (oprinfo-disp insn) (second disp))
	      (setf (oprinfo-disp.bytes insn) sz)
	      (setf (oprinfo-disp.rel-type insn) type)
	      (setf (oprinfo-disp.rel-addn insn) (or (third disp) 0)))))))

(defun add-mem-rest (insn base index scale)
  (if (or index (same-reg? base :rsp) (same-reg? base :r12))
      (progn (setf (oprinfo-modrm.rm insn) #x04)
	     (setf (oprinfo-sib.base insn) 
		   (or (register-number base) #x5))
	     (add-sib.index insn index scale))
      (setf (oprinfo-modrm.rm insn) (register-number base))))

(defun add-modrm.mod-and-modrm.rm (insn mod rm)
  (setf (oprinfo-modrm.mod insn) mod)
  (setf (oprinfo-modrm.rm insn) rm))

(defun add-modrm.mod-only (insn mod)
  (setf (oprinfo-modrm.mod insn) mod))

(defun add-mem-operand (insn mem)
  (with-checks (mem? mem)
    (destructuring-bind (sz base index scale disp) mem
      (declare (ignore sz))
      (unless (or (member base '(:rip :abs)) (register-number base))
	(error 'encoding-error :form mem))
      (cond
	((eql base :rip)
	 (add-modrm.mod-and-modrm.rm insn #x0 #x05)
	 (add-disp insn disp :rel #x04))
	((eql base :abs)
	 (add-modrm.mod-and-modrm.rm insn #x0 #x04)
	 (setf (oprinfo-sib.base insn) #x05)
	 (add-sib.index insn index scale)
	 (add-disp insn disp :rel #x04))
	((and (or (same-reg? base :rbp) 
		  (same-reg? base :r13)) 
	      (eql disp 0))
	 (add-modrm.mod-only insn #x01)
	 (add-disp insn disp :rel #x01)
	 (add-mem-rest insn base index scale))
	(t
	 (add-modrm.mod-only insn (modrm.mod-for-disp disp))
	 (add-disp insn disp :rel)
	 (add-mem-rest insn base index scale))))))

; Syntax for defining instruction encoders.
; Encoder consists of sequences of clauses, each
; with two parts: a pattern, and a production.
; The pattern is a sequence of one or more of 
; the following symbols:
; r8 rm8 r32 rm32 r64 rm64 imm8 imm32 imm64 s32 s64
; x xm32 xm64 xm128
; rX stands for a register of width X bits
; x stands for a vector register
; xmX stands for a vector register or memory
; operand of width X bits
; rmX stands for a register or memory operand
; of width X bits
; immX stands for an immediate of width X bits.
; sX stands for a symbolic immediate of width X bits.
; The product is a sequence of either integers,
; representing opcodes, or one or more of the
; following symbols:
; ib id iq cb cd /0 /1 /2 /3 /4 /5 /6 /7 /r /rm +r *
; * means that the instruction defaults to 64-bit, and needs
; no override prefix. It must be specified at the beginning.
; ib id and iq mean to follow the instruction
; with a 1, 4, or 8 byte immediate, respectively.
; /0 through /7 mean to specify that digit in modrm.reg
; /r means to use a regular modrm form, with modrm.reg as dest
; /rm means to use a regular modrm form, with modrm.rm as dest
; +r means to use a short form, adding the dest register to opcode
; The instruction width is determined by the form of the destination.
; The /0 through /7 /r /rm and +r terms are necessary to match
; the syntax of the processor reference manual, but are somewhat
; awkward to use programatically because they have multiple 
; implications. These terms are transformed as follows:
; /0 through /7 -> /n /rm
; ib through iw -> ix
; /r -> /r /rm
; /rm -> /rm /r
; These terms are used as follows, mapped to corresponding operand
; /n -> set modrm.reg to subcode
; /rm -> add reg or mem operand to modrm.rm
; /r -> add reg parameter to modrm.reg
; ix -> add immediate operand
; cx -> add immediate operand (RIP-relative)

(defun operand-matches? (opr constraint)
  (if (or (reg? constraint) (immediate? constraint))
      (eql opr constraint)
      (ecase constraint
	(rm8 (or (byte-reg? opr) (byte-mem? opr)))
	(rm32 (or (half-reg? opr) (half-mem? opr)))
	(rm64 (or (word-reg? opr) (word-mem? opr)))
	(m8 (byte-mem? opr))
	(m32 (half-mem? opr))
	(m64 (word-mem? opr))
	(m128 (wide-mem? opr))
	(r8 (byte-reg? opr))
	(r32 (half-reg? opr))
	(r64 (word-reg? opr))
	(x (xmm-reg? opr))
	(xm32 (or (xmm-reg? opr) (half-mem? opr)))
	(xm64 (or (xmm-reg? opr) (word-mem? opr)))
	(xm128 (or (xmm-reg? opr) (wide-mem? opr)))
	(imm8 (byte-immediate? opr))
	(imm16 (short-immediate? opr))
	(imm32 (half-immediate? opr))
	(imm64 (word-immediate? opr)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun operand-needs-override? (opr)
    (member opr '(rm64 r64 imm64)))

  (defun subcode-for-subcode-command (cmd)
    (case cmd
      (/0 0)
      (/1 1)
      (/2 2)
      (/3 3)
      (/4 4)
      (/5 5)
      (/6 6)
      (/7 7)
      (t nil)))

  (defun subcode-command? (cmd)
    (member cmd '(/0 /1 /2 /3 /4 /5 /6 /7)))

  (defun width-for-immediate-command (cmd)
    (case cmd
      ((ib cb) 1)
      (iw 2)
      ((id cd) 4)
      (iq 8)
      (t nil)))

  (defun rel-for-immediate-command (cmd)
    (case cmd
      ((ib iw id iq) :abs)
      ((cb cd) :bra)))

  (defun immediate-command? (cmd)
    (member cmd '(ib iw id iq cb cd)))

  (defun regularize-commands (cmds)
    (iter (for cmd in cmds)
	  (cond
	    ((subcode-command? cmd)
	     (collect cmd)
	     (collect '/rm))
	    ((immediate-command? cmd)
	     (collect cmd))
	    ((eql cmd '/r)
	     (collect '/r)
	     (collect '/rm))
	    ((eql cmd '/rm)
	     (collect '/rm)
	     (collect '/r))
	    (t
	     (collect cmd)))))

  (defun generate-operand-handlers (ocinfo oinfo cmds operands)
    (if (and cmds operands)
	(let ((cmd (car cmds))
	      (opr (car operands)))
	  (flet ((advance () (generate-operand-handlers ocinfo oinfo (cdr cmds)
							(cdr operands)))
		 (ignore () (generate-operand-handlers ocinfo oinfo (cdr cmds)
						       operands)))
	    (cond
	      ((subcode-command? cmd)
	       (cons `(add-opcode-extension ,oinfo 
					    ,(subcode-for-subcode-command cmd))
		     (ignore)))
	      ((immediate-command? cmd)
	       (cons `(add-immediate-operand ,oinfo ,opr 
					     ,(width-for-immediate-command cmd)
					     ',(rel-for-immediate-command cmd))
		     (advance)))
	      ((eql cmd '+r)
	       (cons `(add-reg-operand ,oinfo ,opr 'op) (advance)))
	      ((eql cmd '/r)
	       (cons `(add-reg-operand ,oinfo ,opr 'reg) (advance)))
	      ((eql cmd '/rm)
	       (cons `(cond ((reg? ,opr)
			     (add-reg-operand ,oinfo ,opr 'rm))
			    ((mem? ,opr)
			     (add-mem-operand ,oinfo ,opr)))
		     (advance)))
	      ((eql cmd '*)
	       (cons `(setf (ocinfo-override? ,ocinfo) nil)
		     (ignore)))
	      (t (ignore)))))))

  (defun find-first-non-prefix (ocs)
    (position (find-if-not #'(lambda (elt)
			       (member elt *prefixes*))
			   ocs)
	      ocs))

  (defun collect-prefixes (ocs)
    (subseq ocs 0 (find-first-non-prefix ocs)))

  (defun collect-opcodes (ocs)
    (subseq ocs (find-first-non-prefix ocs) nil))

  (defun generate-opcode-handlers (ocinfo cmds)
    (let* ((ocs (remove-if-not #'integerp cmds))
	   (pfxs (collect-prefixes ocs))
	   (opcodes (collect-opcodes ocs)))
      `(,@(mapcar #'(lambda (pfx)
		      `(vector-push-extend ,pfx (ocinfo-prefixes ,ocinfo))) 
		  pfxs)
	  ,@(mapcar #'(lambda (oc)
			`(vector-push-extend ,oc (ocinfo-opcodes ,ocinfo))) 
		    opcodes))))

  ; note that this may latter be undone in the command handlers
  (defun maybe-generate-override-setter (ocinfo constraints)
    (if (some #'operand-needs-override? constraints)
	`(setf (ocinfo-override? ,ocinfo) t)
	`(progn)))

  (defun transform-production (pattern production operands)
    (let ((cmds (regularize-commands production))
	  (oprinfo (gensym))
	  (ocinfo (gensym)))
      `(let ((,oprinfo (make-oprinfo))
	     (,ocinfo (new-ocinfo)))
	 ,(maybe-generate-override-setter ocinfo pattern)
	 ,@(generate-operand-handlers ocinfo oprinfo cmds operands)
	 ,@(generate-opcode-handlers ocinfo cmds)
	 (values ,ocinfo ,oprinfo))))
  
  (defun transform-constraint (constraint operand)
    `(operand-matches? ,operand ',constraint))

  (defun transform-clause (clause operands)
    (let ((pattern (car clause))
	  (production (cadr clause)))
      `((and ,@(mapcar #'transform-constraint pattern operands))
	,(transform-production pattern production operands)))))

(defmacro define-encoder (insn operands &body body)
  (let ((name (prefixsym "ENCODE-" insn)))
    (push (list (intern (symbol-name insn) "KEYWORD") body) *encoders*)
    `(defun ,name ,operands 
       (cond ,@(mapcar #'(lambda (clause)
			   (transform-clause clause operands))
		       body)))))

(defun register-low-part (reg)
  (if (integerp reg) (logand reg #x7)))

(defun req-rex-bit (&rest regs)
  (let ((vals (iter (for reg in regs)
		    (if (and (integerp reg) (> reg 7))
			(collect 1)
			(collect 0)))))
    (apply #'max vals)))

(defun maybe-emit-rex (ln ocinfo oprinfo)
  (with-slots ((reg modrm.reg)
	       (rm modrm.rm)
	       (index sib.index)
	       (base sib.base)
	       (ext oc.ext)) oprinfo
    (let ((rex (compose-rex (if (ocinfo-override? ocinfo) 1 0)
			    (req-rex-bit reg)
			    (req-rex-bit index)
			    (req-rex-bit base rm ext))))
      (if (not (eql rex #x40))
	  (emit-byte ln rex)))))

(defun maybe-emit-prefixes (ln ocinfo)
  (iter (for pfx in-vector (ocinfo-prefixes ocinfo))
	(emit-byte ln pfx)))

(defun emit-opcode-maybe-extended (ln opc oprinfo)
  (emit-byte ln (+ opc (register-low-part (or (oprinfo-oc.ext oprinfo) 0)))))

(defun emit-opcodes (ln ocinfo oprinfo)
  (if (eql (elt (ocinfo-opcodes ocinfo) 0) #x0F)
      (progn
	(emit-byte ln #x0F)
	(emit-opcode-maybe-extended ln (elt (ocinfo-opcodes ocinfo) 1) oprinfo))
      (emit-opcode-maybe-extended ln (elt (ocinfo-opcodes ocinfo) 0) oprinfo)))

(defun maybe-emit-modrm (ln oprinfo)
  (with-slots ((mod modrm.mod) (reg modrm.reg) (rm modrm.rm)) oprinfo
    (and mod reg rm
	(emit-byte ln (compose-modrm mod
				     (register-low-part reg)
				     (register-low-part rm))))))

(defun maybe-emit-sib (ln oprinfo)
  (with-slots ((scale sib.scale) (index sib.index) (base sib.base)) oprinfo
    (and scale index base
	 (emit-byte ln (compose-sib scale 
				    (register-low-part index)
				    (register-low-part base))))))

(defun do-emit-disp-or-imm (ln disp-or-imm bytes type addn)
  (when (and disp-or-imm bytes)
    (if (integerp disp-or-imm)
	(emit-bytes ln disp-or-imm bytes)
	(progn
	  (emit-reloc ln disp-or-imm bytes type)
	  (emit-bytes ln addn bytes)))))

(defun maybe-emit-disp (ln oprinfo)
  (with-slots ((disp disp) (bytes disp.bytes)) oprinfo
    (do-emit-disp-or-imm ln disp bytes (oprinfo-disp.rel-type oprinfo)
			 (oprinfo-disp.rel-addn oprinfo))))

(defun maybe-emit-imm (ln oprinfo)
  (with-slots ((imm imm) (bytes imm.bytes)) oprinfo
    (do-emit-disp-or-imm ln imm bytes (oprinfo-imm.rel-type oprinfo)
			 (oprinfo-imm.rel-addn oprinfo))))

(defun encode-instruction (ln ocinfo oprinfo)
  (maybe-emit-prefixes ln ocinfo)
  (maybe-emit-rex ln  ocinfo oprinfo)
  (emit-opcodes ln ocinfo oprinfo)
  (maybe-emit-modrm ln oprinfo)
  (maybe-emit-sib ln oprinfo)
  (maybe-emit-disp ln oprinfo)
  (maybe-emit-imm ln oprinfo))

(defun do-encode (ln fun args)
  (multiple-value-bind (ocinfo oprinfo)
      (apply fun args)
    (encode-instruction ln ocinfo oprinfo)))

(defun encode-insn (insn ln)
  (handler-case 
      (let ((fun (prefixsym "ENCODE-" (car insn) "AMD64-ASM")))
	(do-encode ln (symbol-function fun) (cdr insn)))
    (assertion-failed (as) (error 'assertion-failed :form insn
				  :check (assertion-failed-check as)))
    (condition (condition) (declare (ignore condition))
	       (error 'encoding-error :form insn))))

; Encoders for general 8/32/64-bit integer instructions

; instructions not encoded
; aaa, aad, aam, aas, bound, call (far), cbw, cwde,
; cdqe, cwd, cdq, cqo, cmov, cmps, cmps, cmpsw, cmpsd, cmpsq,
; daa, das, enter, in, ins, insb, insw, insd, into, jcx, jecx, jrcx,
; lahf, lds, les, lfs, lgs, lss, lfence, lods, lodsb, lodsw, lodsd,
; lodsq, loop, loope, loopne, loopnz, loopz, mfence, movs, movsb,
; movsw, movsd, movsq, outs, outsb, outsw, outsd, popa, popad, popf,
; popa, popad, popf, popfd, popfq, prefetch, prefetchw, pusha, pushad,
; pushf, pushfd, ret (far), sahf, scas, scasb, scasw, scasd, scasq,
; sfence, shld, shrd, std, stos, stosb, stosw, stosd, stosq, xlat, xlatb

(defmacro define-type0-encoder (name base subcode)
  (let ((base1 base)
	(base2 (+ base 1))
	(base3 (+ base 2))
	(base4 (+ base 3)))
    `(define-encoder ,name (dest source)
       ((rm8 imm8) (#x80 ,subcode ib))
       ((rm32 imm8) (#x83 ,subcode ib))
       ((rm64 imm8) (#x83 ,subcode ib))
       ((rm32 imm32) (#x81 ,subcode id))
       ((rm64 imm32) (#x81 ,subcode id))
       ((rm8 r8) (,base1 /rm))
       ((rm32 r32) (,base2 /rm))
       ((rm64 r64) (,base2 /rm))
       ((r8 rm8) (,base3 /r))
       ((r32 rm32) (,base4 /r))
       ((r64 rm64) (,base4 /r)))))

(defmacro define-type1-encoder (name base code)
  (let ((base1 base)
	(base2 (+ base 1)))
    `(define-encoder ,name (dest)
       ((rm8) (,base1 ,code))
       ((rm32) (,base2 ,code))
       ((rm64) (,base2 ,code)))))

(defmacro define-type2-encoder (name subcode)
  `(define-encoder ,name (dest source)
     ((rm8 1) (#xD0 ,subcode))
     ((rm32 1) (#xD1 ,subcode))
     ((rm64 1) (#xD1 ,subcode))
     ((rm8 imm8) (#xC0 ,subcode ib))
     ((rm32 imm8) (#xC1 ,subcode ib))
     ((rm64 imm8) (#xC1 ,subcode ib))))

(defmacro define-type3-encoder (name &rest opcodes)
  `(define-encoder ,name ()
     (() (,@opcodes))))

(defmacro define-type4-encoder (name base1 base2 code)
  `(define-encoder ,name (dest source)
     ((rm32 r32) (#x0F ,base1 /rm))
     ((rm64 r64) (#x0F ,base1 /rm))
     ((rm32 imm8) (#x0F ,base2 ,code ib))
     ((rm64 imm8) (#x0F ,base2 ,code ib))))

(defmacro define-type5-encoder (name code)
  `(define-encoder ,name (dest count)
     ((rm8 1) (#xD0 ,code))
     ((rm8 :cl) (#xD2 ,code))
     ((rm8 imm8) (#xC0 ,code ib))
     ((rm32 1) (#xD1 ,code))
     ((rm32 :cl) (#xD3 ,code))
     ((rm32 imm8) (#xC1 ,code ib))
     ((rm64 1) (#xD1 ,code))
     ((rm64 :cl) (#xD3 ,code))
     ((rm64 imm8) (#xC1 ,code ib))))

(define-type0-encoder add #x00 /0)
(define-type0-encoder adc #x10 /2)
(define-type0-encoder and #x20 /4)
(define-type0-encoder xor #x30 /6)
(define-type0-encoder or  #x08 /1)
(define-type0-encoder sbb #x18 /3)
(define-type0-encoder sub #x28 /5)
(define-type0-encoder cmp #x38 /7)

(define-encoder bsf (dest source)
  ((r32 rm32) (#x0F #xBC /r))
  ((r64 rm64) (#x0F #xBC /r)))

(define-encoder bsr (dest source)
  ((r32 rm32) (#x0F #xBD /r))
  ((r64 rm64) (#x0F #xBD /r)))

(define-encoder bswap (dest)
  ((r32) (#x0F #xC8 +r))
  ((r64) (#x0F #xC8 +r)))

(define-type4-encoder bt #xA3 #xBA /4)
(define-type4-encoder btc #xBB #xBA /7)
(define-type4-encoder btr #xB3 #xBA /6)
(define-type4-encoder bts #xAB #xBA /5)

(define-encoder call (target)
  ((imm32) (#xE8 cd))
  ((rm64) (* #xFF /2)))

(define-type3-encoder clc #xF8)

(define-encoder clflush (addr)
  ((m8) (#x0F #xAE /7)))

(define-type3-encoder cmc #xF5)

(defmacro define-cmovcc-encoders ()
  `(progn ,@(iter (for oc from #x40 to #x4F)
		  (for insn in '(cmovo cmovno cmovb cmovnb
				 cmovz cmovnz cmovbe cmovnbe
				 cmovs cmovns cmovp cmovnp 
				 cmovl cmovge cmovle cmovg))
		  (collect
		      `(define-encoder ,insn (dest source)
			 ((r32 rm32) (#x0F ,oc /r))
			 ((r64 rm64) (#x0F ,oc /r)))))))

(define-cmovcc-encoders)

(define-encoder cmpxchg (dest source)
  ((rm8 r8) (#x0F #xB0 /rm))
  ((rm32 r32) (#x0F #xB1 /rm))
  ((rm64 r64) (#x0F #xB1 /rm)))

(define-type3-encoder cpuid #x0F #xA2)

(define-type1-encoder dec #xFE /1)
(define-type1-encoder div #xF6 /6)
(define-type1-encoder idiv #xF6 /7)
(define-type1-encoder inc #xFE /0)
(define-type1-encoder mul #xF6 /4)
(define-type1-encoder neg #xF6 /3)
(define-type1-encoder not #xF6 /2)

(define-encoder imul (dest source)
  ((r32 rm32) (#x0F #xAF /r))
  ((r64 rm64) (#x0F #xAF /r)))

(define-encoder imul3 (dest source scale)
  ((r32 rm32 imm8) (#x6B /r ib))
  ((r64 rm64 imm8) (#x6B /r ib))
  ((r32 rm32 imm32) (#x69 /r id))
  ((r64 rm64 imm32) (#x69 /r id)))

(define-encoder int (idx)
  ((imm8) (#xCD ib)))

(defmacro define-jcc-encoders ()
  `(progn ,@(iter (for oc from #x70 to #x7F)
		  (for oc2 from #x80 to #x8F)
		  (for insn in '(jo jno jb jnb
				 jz jnz jbe jnbe
				 js jns jp jnp 
				 jl jge jle jg))
		  (collect
		      `(define-encoder ,insn (offset)
			 ((imm8) (,oc cb))
			 ((imm32) (#x0F ,oc2 cd)))))))

(define-jcc-encoders)
		       
(define-encoder jmp (target)
  ((imm8) (#xEB cb))
  ((imm32) (#xE9 cd))
  ((rm64) (* #xFF /4)))

(define-type3-encoder leave #xC9)

(define-encoder mov (dest source)
  ((rm8 r8) (#x88 /rm))
  ((rm32 r32) (#x89 /rm))
  ((rm64 r64) (#x89 /rm))
  ((r8 rm8) (#x8A /r))
  ((r32 rm32) (#x8B /r))
  ((r64 rm64) (#x8B /r))
  ((r8 imm8) (#xB0 +r ib))
  ((r32 imm32) (#xB8 +r id))
  ((rm64 imm32) (#xC7 /0 id))
  ((r64 imm64) (#xB8 +r iq))
  ((rm8 imm8) (#xC6 /0 ib))
  ((rm32 imm32) (#xC7 /0 id)))

(define-encoder movnti (dest source)
  ((m32 r32) (#x0F #xC3 /rm))
  ((m64 r64) (#x0F #xC3 /rm)))

(define-encoder movsx (dest source)
  ((r32 rm8) (#x0F #xBE /r))
  ((r64 rm8) (#x0F #xBE /r)))

(define-encoder movsxd (dest source)
  ((r64 rm32) (#x63 /r)))

(define-encoder movzx (dest source)
  ((r32 rm8) (#x0F #xB6 /r))
  ((r64 rm8) (#x0F #xB6 /r)))

(define-type3-encoder nop #x90)
(define-type3-encoder pause #xF3 #x90)

(define-encoder pop (dest)
  ((r64) (* #x58 +r))
  ((rm64) (* #x8F /0)))

(define-encoder push (source)
  ((r64) (* #x50 +r))
  ((rm64) (* #xFF /6))
  ((imm8) (#x6A ib))
  ((imm32) (#x68 id)))

(define-type5-encoder rcl /2)
(define-type5-encoder rcr /3)

(define-type3-encoder ret #xC3)

(define-type5-encoder rol /0)
(define-type5-encoder ror /1)

(define-encoder retn (bytes)
  ((imm16) (#xC2 iw)))

(defmacro define-setcc-encoders ()
  `(progn ,@(iter (for oc from #x90 to #x9F)
		  (for insn in '(seto setno setb setnb
				 setz setnz setbe setnbe
				 sets setns setp setnp 
				 setl setge setle setg))
		  (collect
		      `(define-encoder ,insn (offset)
			 ((rm8) (#x0F ,oc /2)))))))

(define-setcc-encoders)

(define-type2-encoder sal /4)
(define-type2-encoder sar /7)
(define-type2-encoder shr /5)

(define-type3-encoder stc #xF9)

(define-encoder test (dest source)
  ((rm8 imm8) (#xF6 /0 ib))
  ((rm32 imm32) (#xF7 /0 id))
  ((rm64 imm32) (#xF7 /0 id))
  ((rm8 r8) (#x84 /rm))
  ((rm32 r32) (#x85 /rm))
  ((rm64 r64) (#x85 /rm)))

(define-encoder xchg (dest source)
  ((rm8 r8) (#x86 /rm))
  ((r8 rm8) (#x86 /r))
  ((rm32 r32) (#x87 /rm))
  ((r32 rm32) (#x87 /r))
  ((rm64 r64) (#x87 /rm))
  ((r64 rm64) (#x87 /r)))

(define-encoder xadd (dest source)
  ((rm8 r8) (#x0F #xC0 /rm))
  ((rm32 r32) (#x0F #xC1 /rm))
  ((rm64 r64) (#x0F #xC1 /rm)))

; Man there are a lot of SSE instructions
; The choice of xm32/xm64/xm128 is seemingly random.
; The specifier chosen is the one that makes yasm happy.

(defmacro define-x-encoder (name &rest opcodes)
  `(define-encoder ,name (dest source)
     ((x x) (,@opcodes /r))))

(defmacro define-xm128-encoder (name &rest opcodes)
  `(define-encoder ,name (dest source)
     ((x xm128) (,@opcodes /r))))

(defmacro define-xm64-encoder (name &rest opcodes)
  `(define-encoder ,name (dest source)
     ((x xm64) (,@opcodes /r))))

(defmacro define-xm32-encoder (name &rest opcodes)
  `(define-encoder ,name (dest source)
     ((x xm32) (,@opcodes /r))))

(defmacro define-cmp-encoder (name &rest opcodes)
  `(define-encoder ,name (dest source cmp)
     ((x xm128 imm8) (,@opcodes /r ib))))

(defmacro define-rx64-encoder (name &rest opcodes)
  `(define-encoder ,name (dest source)
     ((r32 xm64) (,@opcodes /r))
     ((r64 xm64) (,@opcodes /r))))

(defmacro define-rx32-encoder (name &rest opcodes)
  `(define-encoder ,name (dest source)
     ((r32 xm32) (,@opcodes /r))
     ((r64 xm32) (,@opcodes /r))))

(defmacro define-rx-encoder (name &rest opcodes)
  `(define-encoder ,name (dest source)
     ((r32 xm32) (,@opcodes /r))
     ((r64 xm64) (,@opcodes /r))))

(defmacro define-xr-encoder (name &rest opcodes)
  `(define-encoder ,name (dest source)
     ((x rm32) (,@opcodes /r))
     ((x rm64) (,@opcodes /r))))

(defmacro define-shift0-encoder (name code1 code2 sub)
  `(define-encoder ,name (dest shift)
     ((x xm128) (#x66 #x0F ,code1 /r))
     ((x imm8) (#x66 #x0F ,code2 ,sub ib))))

(defmacro define-shift1-encoder (name code sub)
  `(define-encoder ,name (dest shift)
     ((x imm8) (#x66 #x0F ,code ,sub ib))))

(defmacro define-mov1-encoder (name opcodes1 opcodes2)
  `(define-encoder ,name (dest source)
     ((x m64) (,@opcodes1 /r))
     ((m64 x) (,@opcodes2 /rm))))

(defmacro define-mov2-encoder (name &rest opcodes)
  `(define-encoder ,name (dest source)
     ((r32 x) (,@opcodes /r))))

(defmacro define-mov3-encoder (name &rest opcodes)
  `(define-encoder ,name (dest source)
     ((m128 x) (,@opcodes /rm))))

(defmacro define-mov0-128-encoder (name opcodes1 opcodes2)
  `(define-encoder ,name (dest source) 
     ((x xm128) (,@opcodes1 /r))
     ((xm128 x) (,@opcodes2 /rm))))

(defmacro define-mov0-64-encoder (name opcodes1 opcodes2)
  `(define-encoder ,name (dest source) 
     ((x xm64) (,@opcodes1 /r))
     ((xm64 x) (,@opcodes2 /rm))))

(defmacro define-mov0-32-encoder (name opcodes1 opcodes2)
  `(define-encoder ,name (dest source) 
     ((x xm32) (,@opcodes1 /r))
     ((xm32 x) (,@opcodes2 /rm))))

(define-xm128-encoder addpd #x66 #x0F #x58)
(define-xm128-encoder addps #x0F #x58)
(define-xm128-encoder addsd #xF2 #x0F #x58)
(define-xm128-encoder addss #xF3 #x0F #x58)

(define-xm128-encoder addsubpd #x66 #x0F #xD0)
(define-xm128-encoder addsubps #xF2 #x0F #xD0)

(define-xm128-encoder andnpd #x66 #x0F #x55)
(define-xm128-encoder andnps #x0F #x55)
(define-xm128-encoder andpd #x66 #x0F #x54)
(define-xm128-encoder andps #x0F #x54)

(define-cmp-encoder cmppd #x66 #x0F #xC2)
(define-cmp-encoder cmpps #x0F #xC2)
(define-cmp-encoder cmpsd #xF2 #x0F #xC2)
(define-cmp-encoder cmpss #xF3 #x0F #xC2)

(define-xm128-encoder comisd #x66 #x0F #x2F)
(define-xm128-encoder comiss #x0F #x2F)

(define-xm64-encoder cvtdq2pd #xF3 #x0F #xE6)
(define-xm128-encoder cvtdq2ps #x0F #x5B)
(define-xm128-encoder cvtpd2dq #xF2 #x0F #xE6)

; cvtpd2pi

(define-xm128-encoder cvtpd2ps #x66 #x0F #x5A)

; cvtpi2pd
; cvtpi2ps

(define-xm128-encoder cvtps2dq #x66 #x0F #x5B)
(define-xm64-encoder cvtps2pd #x0F #x5A)

; cvtps2pi

(define-rx64-encoder cvtsd2si #xF2 #x0F #x2D)
(define-xm64-encoder cvtsd2ss #xF2 #x0F #x5A)
(define-xr-encoder cvtsi2sd #xF2 #x0F #x2A)
(define-xr-encoder cvtsi2ss #xF3 #x0F #x2A)
(define-xm32-encoder cvtss2sd #xF3 #x0F #x5A)
(define-rx32-encoder cvtss2si #xF3 #x0F #x2D)
(define-xm128-encoder cvttpd2dq #x66 #x0F #xE6)

; cvtpd2pi

(define-xm128-encoder cvttps2dq #xF3 #x0F #x5b)

; cvttpd2pi

(define-rx64-encoder cvttsd2si #xF2 #x0F #x2C)
(define-rx32-encoder cvttss2si #xF3 #x0F #x2C)

(define-xm128-encoder divpd #x66 #x0F #x5E)
(define-xm128-encoder divps #x0F #x5E)
(define-xm128-encoder divsd #xF2 #x0F #x5E)
(define-xm128-encoder divss #xF3 #x0F #x5E)

; fxrstor
; fxsave

(define-xm128-encoder haddpd #x66 #x0F #x7C)
(define-xm128-encoder haddps #xF2 #x0F #x7C)

(define-xm128-encoder hsubpd #x66 #x0F #x7D)
(define-xm128-encoder hsubps #xF2 #x0F #x7D)

; lddqu

(define-encoder ldmxcsr (source)
  ((m32) (#x0F #xAE /2)))

(define-x-encoder maskmovdqu #x66 #x0F #xF7)

(define-xm128-encoder maxpd #x66 #x0F #x5F)
(define-xm128-encoder maxps #x0F #x5F)
(define-xm128-encoder maxsd #xF2 #x0F #x5F)
(define-xm128-encoder maxss #xF3 #x0F #x5F)

(define-xm128-encoder minpd #x66 #x0F #x5D)
(define-xm128-encoder minps #x0F #x5D)
(define-xm128-encoder minsd #xF2 #x0F #x5D)
(define-xm128-encoder minss #xF3 #x0F #x5D)

(define-mov0-128-encoder movapd (#x66 #x0F #x28) (#x66 #x0F #x29))
(define-mov0-128-encoder movaps (#x0F #x28) (#x0F #x29))

(define-encoder movd (dest source)
  ((x rm32) (#x66 #x0F #x6E /r))
  ((x rm64) (#x66 #x0F #x6E /r))
  ((rm32 x) (#x66 #x0F #x7E /rm))
  ((rm64 x) (#x66 #x0F #x7E /rm)))

(define-xm64-encoder movddup #xF2 #x0F #x12)

; movdq2q

(define-mov0-128-encoder movdqa (#x66 #x0F #x6F) (#x66 #x0F #x7F))
(define-mov0-128-encoder movdqu (#xF3 #x0F #x6F) (#xF3 #x0F #x7F))
(define-x-encoder movhlps #x0F #x12)
(define-mov1-encoder movhpd (#x66 #x0F #x16) (#x66 #x0F #x17))
(define-mov1-encoder movhps (#x0F #x16) (#x0F #x17))
(define-x-encoder movlhps #x0F #x16)
(define-mov1-encoder movlpd (#x66 #x0F #x12) (#x66 #x0F #x13))
(define-mov1-encoder movlps (#x0F #x12) (#x0F #x13))

(define-mov2-encoder movmskpd #x66 #x0F #x50)
(define-mov2-encoder movmskps #x0F #x50)

(define-mov3-encoder movntdq #x66 #x0F #xE7)
(define-mov3-encoder movntpd #x66 #x0F #x2B)
(define-mov3-encoder movntps #x0F #x2B)

(define-mov0-64-encoder movq (#xF3 #x0F #x7E) (#x66 #x0F #xD6))

; movq2dq

(define-mov0-64-encoder movsd (#xF2 #x0F #x10) (#xF2 #x0F #x11))
(define-xm128-encoder movshdup #xF3 #x0F #x16)
(define-xm128-encoder movsldup #xF3 #x0F #x12)
(define-mov0-32-encoder movss (#xF3 #x0F #x10) (#xF3 #x0F #x11))
(define-mov0-128-encoder movupd (#x66 #x0F #x10) (#x66 #x0F #x11))
(define-mov0-128-encoder movups (#x0F #x10) (#x0F #x11))

(define-xm128-encoder mulpd #x66 #x0F #x59)
(define-xm128-encoder mulps #x0F #x59)
(define-xm128-encoder mulsd #xF2 #x0F #x59)
(define-xm128-encoder mulss #xF3 #x0F #x59)

(define-xm128-encoder orpd #x66 #x0F #x56)
(define-xm128-encoder orps #x0F #x56)

(define-xm128-encoder packssdw #x66 #x0F #x6B)
(define-xm128-encoder packsswb #x66 #x0F #x63)
(define-xm128-encoder packuswb #x66 #x0F #x67)

(define-xm128-encoder paddb #x66 #x0F #xFC)
(define-xm128-encoder paddd #x66 #x0F #xFE)
(define-xm128-encoder paddq #x66 #x0F #xD4)
(define-xm128-encoder paddsb #x66 #x0F #xEC)
(define-xm128-encoder paddsw #x66 #x0F #xED)
(define-xm128-encoder paddusb #x66 #x0F #xDC)
(define-xm128-encoder paddusw #x66 #x0F #xDD)
(define-xm128-encoder paddw #x66 #x0F #xFD)

(define-xm128-encoder pand #x66 #x0F #xDB)
(define-xm128-encoder pandn #x66 #x0F #xDF)

(define-xm128-encoder pavgb #x66 #x0F #xE0)
(define-xm128-encoder pavgw #x66 #x0F #xE3)

(define-xm128-encoder pcmpeqb #x66 #x0F #x74)
(define-xm128-encoder pcmpeqd #x66 #x0F #x76)
(define-xm128-encoder pcmpeqw #x66 #x0F #x75)
(define-xm128-encoder pcmpgtb #x66 #x0F #x64)
(define-xm128-encoder pcmpgtd #x66 #x0F #x66)
(define-xm128-encoder pcmpgtw #x66 #x0F #x65)

(define-encoder pextrw (dest source sel)
  ((r32 x imm8) (#x66 #x0F #xC5 /r ib)))

(define-encoder pinsrw (dest source sel)
  ((x rm32 imm8) (#x66 #x0F #xC4 /r ib)))

(define-xm128-encoder pmaddwd #x66 #x0F #xF5)

(define-xm128-encoder pmaxsw #x66 #x0F #xEE)
(define-xm128-encoder pmaxub #x66 #x0F #xDE)

(define-xm128-encoder pminsw #x66 #x0F #xEA)
(define-xm128-encoder pminub #x66 #x0F #xDA)

(define-mov2-encoder pmovmskb #x66 #x0F #xD7)

(define-xm128-encoder pmulhuw #x66 #x0F #xE4)
(define-xm128-encoder pmulhw #x66 #x0F #xE5)
(define-xm128-encoder pmullw #x66 #x0F #xD5)
(define-xm128-encoder pmuludq #x66 #x0F #xF4)

(define-xm128-encoder por #x66 #x0F #xEB)

(define-xm128-encoder psadbw #x66 #x0F #xF6)

(define-cmp-encoder pshufd #x66 #x0F #x70)
(define-cmp-encoder pshufhw #xF3 #x0F #x70)
(define-cmp-encoder pshuflw #xF2 #x0F #x70)

(define-shift0-encoder pslld #xF2 #x72 /6)
(define-shift1-encoder pslldq #x73 /7)
(define-shift0-encoder psllq #xF3 #x73 /6)
(define-shift0-encoder psllw #xF1 #x71 /6)
(define-shift0-encoder psrad #xE2 #x72 /4)
(define-shift0-encoder psraw #xE1 #x71 /4)
(define-shift0-encoder psrld #xD2 #x72 /2)
(define-shift1-encoder psrldq #x73 /3)
(define-shift0-encoder psrlq #xD3 #x73 /2)
(define-shift0-encoder psrlw #xD1 #x71 /2)

(define-xm128-encoder psubb #x66 #x0F #xF8)
(define-xm128-encoder psubd #x66 #x0F #xFA)
(define-xm128-encoder psubq #x66 #x0F #xFB)
(define-xm128-encoder psubsb #x66 #x0F #xE8)
(define-xm128-encoder psubsw #x66 #x0F #xE9)
(define-xm128-encoder psubusb #x66 #x0F #xD8)
(define-xm128-encoder psubusw #x66 #x0F #xD9)
(define-xm128-encoder psubw #x66 #x0F #xF9)

(define-xm128-encoder punpckhbw #x66 #x0F #x68)
(define-xm128-encoder punpckhdq #x66 #x0F #x6A)
(define-xm128-encoder punpckhqdq #x66 #x0F #x6D)
(define-xm128-encoder punpckhwd #x66 #x0F #x69)
(define-xm128-encoder punpcklbw #x66 #x0F #x60)
(define-xm128-encoder punpckldq #x66 #x0F #x62)
(define-xm128-encoder punpcklqdq #x66 #x0F #x6C)
(define-xm128-encoder punpcklwd #x66 #x0F #x61)

(define-xm128-encoder pxor #x66 #x0F #xEF)

(define-xm128-encoder rcpps #x0F #x53)
(define-xm128-encoder rcpss #xF3 #x0F #x53)
(define-xm128-encoder rsqrtps #x0F #x52)
(define-xm128-encoder rsqrtss #xF3 #x0F #x52)

(define-cmp-encoder shufpd #x66 #x0F #xC6)
(define-cmp-encoder shufps #x0F #xC6)

(define-xm128-encoder sqrtpd #x66 #x0F #x51)
(define-xm128-encoder sqrtps #x0F #x51)
(define-xm128-encoder sqrtsd #xF2 #x0F #x51)
(define-xm128-encoder sqrtss #xF3 #x0F #x51)

(define-encoder stmxcsr (dest)
  ((m32) (#x0F #xAE /3)))

(define-xm128-encoder subpd #x66 #x0F #x5C)
(define-xm128-encoder subps #x0F #x5C)
(define-xm128-encoder subsd #xF2 #x0F #x5C)
(define-xm128-encoder subss #xF3 #x0F #x5C)

(define-xm128-encoder ucomisd #x66 #x0F #x2E)
(define-xm128-encoder ucomiss #x0F #x2E)

(define-xm128-encoder unpckhpd #x66 #x0F #x15)
(define-xm128-encoder unpckhps #x0F #x15)
(define-xm128-encoder unpcklpd #x66 #x0F #x14)
(define-xm128-encoder unpcklps #x0F #x14)

(define-xm128-encoder xorpd #x66 #x0F #x57)
(define-xm128-encoder xorps #x0F #x57)
