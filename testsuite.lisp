; testsuite.lisp
; Testsuite for the assembler. 

(in-package "AMD64-ASM")

(eval-when (:compile-toplevel)
  (defparameter *tests* nil))

(defparameter *yasmbin* "/usr/local/bin/yasm")
(defparameter *tempfile* "test.nasm")
(defparameter *tempbin* "test")

(defparameter *skip-tests* (append '(:call :pinsrw) *sdis*))

(defparameter *insn-mapping*
  '((:imul3 . :imul)))

(defmacro defasmtest (name &body body)
  (if (not (member name *tests*))
      (push name *tests*))
  `(defun ,name () 
     (let ((asm ',body))
       (do-test ',name asm))))

(defun do-test (name source)
  (declare (special *passes*)
	   (special *fails*))
  (let ((bin (assemble-code source))
	(ref (yasm-assemble source)))
    (format t "Running ~A... " name)
    (if (equalp (asmbin-buffer bin) ref)
	(progn
	  (incf *passes*)
	  (format t "pass~%"))
	(progn
	  (incf *fails*)
	  (format t "fail~%")
	  (print-tempfile *tempfile*)
	  (format t "~A~%" source)
	  (print-codevector ref)
	  (print-codevector (asmbin-buffer bin))))))

(defun run-tests (&optional inc exc)
  (let ((*passes* 0)
	(*fails* 0))
    (declare (special *passes*)
	     (special *fails*))
    (when (not (or inc exc))
      (iter (for name in (reverse *tests*))
	    (funcall name)))
    (randomly-test-all-encoders *encoders* inc (or exc *skip-tests*))
    (map nil #'delete-file (list *tempfile* *tempbin*))
    (format t "Passed ~A, Failed ~A~%" *passes* *fails*)))

(defun random-reg-operand (width)
  (nth (random 5) (ecase width
		    (:byte '(:bl :cl :r8b :r12b :r13b))
		    (:half '(:ebx :ecx :r8d :r12d :r13d))
		    (:word '(:rbx :rcx :r8 :r12 :r13)))))

(defun random-xreg-operand ()
  (nth (random 5) '(:xmm0 :xmm1 :xmm9 :xmm10 :xmm11)))		

(defun random-imm-operand (width)
  (let ((range (ecase width
		 (:byte 100)
		 (:short 1000)
		 (:half 1000000)
		 (:word 100000000000))))
    (- (random (* 2 range)) range)))

(defun random-mem-operand (width)
  (nth (random 8)
       `((,width :rax nil 1 0)
	 (,width :rbp nil 1 0)
	 (,width :rcx nil 1 100)
	 (,width :rcx :rax 4 1000)
	 (,width :rsp nil 1 100)
	 (,width :rip nil 1 1000)
	 (,width :abs nil 1 100)
	 (,width :abs :rcx 8 1000))))

(defun random-reg-or-mem-operand (width)
  (if (eql (random 2) 0)
      (random-reg-operand width)
      (random-mem-operand width)))

(defun random-xreg-or-mem-operand (width)
  (if (eql (random 2) 0)
      (random-xreg-operand)
      (random-mem-operand width)))

(defun random-operand (constraint)
  (if (or (reg? constraint) (immediate? constraint))
      constraint
      (ecase constraint
	(rm8 (random-reg-or-mem-operand :byte))
	(rm32 (random-reg-or-mem-operand :half))
	(rm64 (random-reg-or-mem-operand :word))
	(m8 (random-mem-operand :byte))
	(m32 (random-mem-operand :half))
	(m64 (random-mem-operand :word))
	(m128 (random-mem-operand :wide))
	(r8 (random-reg-operand :byte))
	(r32 (random-reg-operand :half))
	(r64 (random-reg-operand :word))
	(x (random-xreg-operand))
	(xm32 (random-xreg-or-mem-operand :half))
	(xm64 (random-xreg-or-mem-operand :word))
	(xm128 (random-xreg-or-mem-operand :wide))
	(imm8 (random-imm-operand :byte))
	(imm16 (random-imm-operand :short))
	(imm32 (random-imm-operand :half))
	(imm64 (random-imm-operand :word)))))

(defun generate-random-test (insn pat)
  (cons insn (mapcar #'random-operand pat)))

(defun generate-test-name (insn i)
  (intern (concatenate 'string "TEST-" (symbol-name insn)
		       (format nil "-~D" i))))

(defun randomly-test-clause (num insn clause)
  (let ((src (iter (for i from 0 below 4)
		   (collect (generate-random-test insn clause)))))
    (do-test (generate-test-name insn num) src)))

(defun randomly-test-encoder (enc)
  (let ((insn (first enc)))
    (iter (for clause in (second enc))
	  (for num from 0 below (length (second enc)))
	  (randomly-test-clause num insn (first clause)))))

(defun should-run? (insn inc exc)
  (or (and (not inc) (not exc))
      (and inc (not exc) (member insn inc))
      (and (not inc) exc (not (member insn exc)))
      (and inc exc (member insn inc) (not (member insn exc)))))

(defun randomly-test-all-encoders (encs inc exc)
  (iter (for enc in encs)
	(when (should-run? (first enc) inc exc)
	  (randomly-test-encoder enc))))

(defun nasmize-symbol (sym)
  (string-downcase (symbol-name sym)))

(defun nasmize-specifier (spec)
  (case spec
    (:byte "byte")
    (:half "dword")
    (:word "qword")
    (:wide "dqword")))

(defun nasmize-mem (mem)
  (with-output-to-string (str)
    (format str "~A " (nasmize-specifier (first mem)))
    (format str "[ ")
    (cond 
      ((eql (second mem) :rip)
       (format str "rip ")
       (unless (eql (fifth mem) 0)
	 (format str "+ ~A " (fifth mem))))
      ((eql (second mem) :abs)
       (format str "~A" (fifth mem))
       (when (third mem)
	 (format str " + ~A * ~A " (nasmize-symbol (third mem)) (fourth mem))))
      (t
       (format str "~A " (nasmize-symbol (second mem)))
       (when (third mem)
	 (format str " + ~A * ~A " (nasmize-symbol (third mem)) (fourth mem)))
       (unless (eql (fifth mem) 0)
	 (format str " + ~A " (fifth mem)))))
    (format str " ]")))

(defun nasmize-label-ref (elt)
  (string-downcase (symbol-name elt)))

(defun nasmize-element (str elt)
  (cond
    ((reg? elt)
     (format str "~A " (nasmize-symbol elt)))
    ((immediate? elt)
     (format str "~D " elt))
    ((mem? elt)
     (format str "~A " (nasmize-mem elt)))
    ((symbolp elt)
     (format str "~A" (nasmize-label-ref elt)))))

(defun nasmize-opcode (str opc)
  (format str "~A " (nasmize-symbol (or (cdr (assoc opc *insn-mapping*)) opc))))

(defun nasmize-instruction (insn)
  (with-output-to-string (str)
    (nasmize-opcode str (first insn))
    (iter (for elt in (butlast (rest insn)))
	  (nasmize-element str elt)
	  (format str ", "))
    (when (rest insn)
      (nasmize-element str (first (last insn))))))

(defun nasmize-label (label)
  (string-downcase (concatenate 'string (symbol-name label) ":")))

(defun nasmize-source (source file)
  (with-open-file (file file :direction :output :if-exists :supersede)
    (format file "bits 64~%")
    (iter (for line in source)
	  (if (label-line? line)
	      (format file "~A~%" (nasmize-label line))
	      (format file "~A~%" (nasmize-instruction line))))))

(defun run-yasm (filename)
  (sb-ext:run-program *yasmbin* (list "-fbin" filename)
		      :input nil :output *trace-output*))

(defun print-tempfile (filename)
  (sb-ext:run-program "/bin/cat" (list filename)
		      :input nil :output *trace-output*))

(defun yasm-assemble (source)
  (nasmize-source source *tempfile*)
  (run-yasm *tempfile*)
  (load-file-into-vector *tempbin*))

; Note that we can't specify tests that might invoke
; special encodings for eax as the destination, since
; yasm does those optimizations and we don't. Also 
; remember that we only emit 64-bit code. We don't
; ever generate address-size overrides, so use only
; 64-bit registers in mem forms. 

(defasmtest reg-test
  (:add :al :bl)
  (:add :al :r10b)
  (:add :eax :ebx)
  (:add :eax :r10d)
  (:add :r10d :ebx)
  (:add :rax :r10)
  (:add :r10 :rax))

(defasmtest reg-test-2
  (:add :ebx 1)
  (:add :ecx 1)
  (:add :edx 1)
  (:add :esi 1)
  (:add :edi 1)
  (:add :esp 1)
  (:add :ebp 1))

(defasmtest reg-test-3
  (:add :r8d 1)
  (:add :r9d 1)
  (:add :r10d 1)
  (:add :r11d 1))

(defasmtest reg-test-4
  (:add :r12d 1)
  (:add :r13d 1)
  (:add :r14d 1)
  (:add :r15d 1))

(defasmtest rip-test
  (:mov :al (:byte :rip nil 1 25))
  (:mov :eax (:half :rip nil 1 35))
  (:mov :rbx (:word :rip nil 1 45)))

(defasmtest mem-test
  (:mov :al (:byte :rbp nil 1 0))
  (:mov :r10d (:half :rbp nil 1 10))
  (:mov :r10d (:half :rbp :rcx 4 10))
  (:mov :rax (:word :rbp :rcx 8 130)))

(defasmtest mem-test-2
  (:mov (:byte :rbp nil 1 0) :al)
  (:mov (:half :rbx nil 1 10) :eax)
  (:mov (:word :rbx :rbp 1 150) :r10))

(defasmtest mem-test-3
  (:add (:half :rbp nil 1 0) :ebx)
  (:add (:word :rbp nil 1 0) :rbx)
  (:add (:half :rbp :rbx 1 10) :ecx))

(defasmtest abs-test
  (:mov :al (:byte :abs nil 1 10))
  (:mov :eax (:half :abs nil 1 10))
  (:mov :rax (:word :abs nil 1 1000)))

(defasmtest abs-test-2
  (:mov :al (:byte :abs :rbp 4 10))
  (:mov :eax (:half :abs :rbp 8 25))
  (:mov :rbx (:word :abs :rcx 8 1025)))

(defasmtest test-imm
  (:add :bl 10)
  (:add :ebx 10)
  (:sub :ebx 1000)
  (:sub :rbx 100))

(defasmtest test-sdis
  (:push :rbp)
  (:mov :rbp :rsp)
  (:mov :eax (:half :rbp nil 1 8))
  (:mov :ebx (:half :rbp nil 1 16))
  loophead
  (:inc :ebx)
  (:cmp :ebx :eax)
  (:jnz loophead)
  (:leave)
  (:retn 16))

(defasmtest test-sdis-2
  (:push :rbp)
  (:mov :rbp :rsp)
  (:mov :eax (:half :rbp nil 1 8))
  (:mov :ebx (:half :rbp nil 1 16))
  loophead
  (:cmp :ebx :eax)
  (:jnz loopexit)
  (:inc :ebx)
  (:jmp loophead)
  loopexit
  (:leave)
  (:retn 16))

(defmacro define-big-function (name)
  `(defasmtest ,name
     (:xor :rax :rax)
     loophead
     ,@(iter (for i from 0 below 50) (collect '(:mov :rbx 1)))
     (:jmp loophead)))

(define-big-function test-sdis-3)

(defparameter *source1*
  '((:var :ext |_foo| (:half 12))
    (:var :int |_bar| (:half 13))
    (:proc :ext |_main|
     (:mov :edi (:half :rip nil 1 (:half |_bar|)))
     (:call (:half |_xdouble|))
     (:mov :edi :eax)
     (:call (:half |_xtriple|))
     (:ret))
    (:proc :ext |_xdouble|
     (:add :rdi :rdi)
     (:mov :rax :rdi)
     (:ret))))

(defparameter *source2*
  '((:proc :ext |_xtriple|
     (:mov :rbx 3)
     (:imul :rdi :rbx)
     (:mov :rax :rdi)
     (:ret))))

; x86-64 arg registers: rdi, rsi, rdx, rcx, r8, r9
(defparameter *source3*
  '((:proc :ext |_memcpy16b|
     ; arg dst in rdi
     ; arg src in rsi
     ; arg count, in bytes, in rdx
     ; temp loop-count in rcx
     (:xor :rcx :rcx)
     (:cmp :rcx :rdx)
     (:jz loopexit)
     loophead
     (:movdqa :xmm0 (:wide :rsi :rcx 1 0))
     (:movdqa (:wide :rdi :rcx 1 0) :xmm0)
     (:add :rcx 16)
     (:cmp :rcx :rdx)
     (:jnz loophead)
     loopexit
     (:ret))))
