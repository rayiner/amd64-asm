; macho-binaries.lisp
; Support for generating mach-o object files

(in-package "AMD64-ASM")

(defconstant +mh-magic-64+ #xFEEDFACF)
(defconstant +cpu-type-x86-64+ #x01000007)
(defconstant +cpu-subtype-all+ 3)
(defconstant +mh-object+ 1)
(defconstant +mh-def-flags+ #x0)
(defconstant +lc-segment-64+ #x19)
(defconstant +mh-def-prot+ #x7)
(defconstant +mh-def-text-flags+ #x80000000)
(defconstant +mh-def-data-flags+ #x0)
(defconstant +lc-symtab+ #x2)
(defconstant +n-ext+ #x1)
(defconstant +n-undef+ #x0)
(defconstant +n-abs+ #x2)
(defconstant +n-sect+ #xe)
(defconstant +no-sect+ 0)
(defconstant +cs-num+ 1)
(defconstant +ds-num+ 2)
(defconstant +x86-64-reloc-unsigned+ 0)
(defconstant +x86-64-reloc-signed+ 1)
(defconstant +x86-64-reloc-branch+ 2)
(defconstant +r-pcrel-shift+ 24)
(defconstant +r-length-shift+ 25)
(defconstant +r-extern-shift+ 27)
(defconstant +r-type-shift+ 28)

(define-c-struct mach-header-64
  (magic :half)
  (cputype :half)
  (cpusubtype :half)
  (filetype :half)
  (ncmds :half)
  (sizeofcmds :half)
  (flags :half)
  (reserved :half))

(define-c-struct segment-command-64
  (cmd :half)
  (cmdsize :half)
  (segname :byte 16)
  (vmaddr :word)
  (vmsize :word)
  (fileoff :word)
  (filesize :word)
  (maxprot :half)
  (initprot :half)
  (nsects :half)
  (flags :half))

(define-c-struct section-64
  (sectname :byte 16)
  (segname :byte 16)
  (addr :word)
  (size :word)
  (offset :half)
  (align :half)
  (reloff :half)
  (nreloc :half)
  (flags :half)
  (reserved1 :half)
  (reserved2 :half)
  (reserved3 :half))

(define-c-struct symtab-command
  (cmd :half)
  (cmdsize :half)
  (symoff :half)
  (nsyms :half)
  (stroff :half)
  (strsize :half))

(define-c-struct nlist-64
  (nstrx :half)
  (ntype :byte)
  (nsect :byte)
  (ndesc :byte 2)
  (nvalue :word))

(define-c-struct relocation-info
  (r-address :half)
  (r-symbolnum-and-flags :half))

(defstruct mach-o-symtab
  strtab
  ntab
  nvec)

(defun new-mach-o-symtab ()
  (make-mach-o-symtab :strtab (new-strtab) 
		      :ntab (make-hash-table)
		      :nvec (make-array 0 :fill-pointer t)))

(defun add-mach-o-sym (st name sc sect addr)
  (assert (not (gethash name (mach-o-symtab-ntab st))))
  (let* ((nl (make-nlist-64 :nstrx (strtab-intern (mach-o-symtab-strtab st) 
						  name)
			    :ntype (ecase sc
				     (:int +n-sect+)
				     (:ext (+ +n-sect+ +n-ext+))
				     (:und (+ +n-undef+ +n-ext+)))
			    :nsect sect
			    :ndesc #(0 0)
			    :nvalue addr)))
    (setf (gethash name (mach-o-symtab-ntab st))
	  (length (mach-o-symtab-nvec st)))
    (vector-push-extend nl (mach-o-symtab-nvec st))))

(defun mach-o-symtab-member? (st name)
  (gethash name (mach-o-symtab-ntab st)))

(defun mach-o-symtab-strsize (st)
  (strtab-size (mach-o-symtab-strtab st)))

(defun mach-o-symtab-count (st)
  (length (mach-o-symtab-nvec st)))

(defun mach-o-sym-index (st sym)
  (gethash sym (mach-o-symtab-ntab st)))

(defun generate-mach-o-symtab (obj)
  (let ((tab (new-mach-o-symtab))
	(fp 0))
    (iter (for def in-vector (asmobj-cdefs obj))
	  (add-mach-o-sym tab 
			  (asmdef-name def) 
			  (asmdef-scope def) 
			  +cs-num+ 
			  fp)
	  (incf fp (length (asmbin-buffer (asmdef-bin def)))))
    (iter (for def in-vector (asmobj-ddefs obj))
	  (add-mach-o-sym tab
			  (asmdef-name def)
			  (asmdef-scope def)
			  +ds-num+
			  fp)
	  (incf fp (length (asmbin-buffer (asmdef-bin def)))))
    (iter (for def in-vector (asmobj-cdefs obj))
	  (iter (for rel in-vector (asmbin-relocs (asmdef-bin def)))
		(unless (mach-o-symtab-member? tab (asmrel-symbol rel))
		  (add-mach-o-sym tab
				  (asmrel-symbol rel)
				  :und
				  +no-sect+
				  0))))
    (iter (for def in-vector (asmobj-ddefs obj))
	  (iter (for rel in-vector (asmbin-relocs (asmdef-bin def)))
		(unless (mach-o-symtab-member? tab (asmrel-symbol rel))
		  (add-mach-o-sym tab
				  (asmrel-symbol rel)
				  :und
				  +no-sect+
				  0))))
    tab))

(defun compose-mach-o-rel-num-and-flags (snum pcrel length ext type)
  (assert (and (<= pcrel 1) (<= length 3) (<= ext 1) (<= type 15)))
  (+ snum 
     (ash pcrel +r-pcrel-shift+)
     (ash length +r-length-shift+)
     (ash ext +r-extern-shift+)
     (ash type +r-type-shift+)))

(defun add-mach-o-rel (rel fp st relvec)
  (let* ((addr (+ fp (asmrel-offset rel)))
	 (sn (mach-o-sym-index st (asmrel-symbol rel)))
	 (pcrel (ecase (asmrel-type rel)
		  (:abs 0)
		  (:rel 1)
		  (:bra 1)))
	 (len (floor (log (asmrel-width rel) 2)))
	 (ext 1)
	 (type (ecase (asmrel-type rel)
		 (:abs +x86-64-reloc-unsigned+)
		 (:rel +x86-64-reloc-signed+)
		 (:bra +x86-64-reloc-branch+)))
	 (snf (compose-mach-o-rel-num-and-flags sn pcrel len ext type)))
    (vector-push-extend (make-relocation-info :r-address addr
					      :r-symbolnum-and-flags snf)
			relvec)))

(defun add-mach-o-rels-for-defs (defs st relvec)
  (let ((fp 0))
    (iter (for def in-vector defs)
	  (iter (for rel in-vector (asmbin-relocs (asmdef-bin def)))
		(add-mach-o-rel rel fp st relvec))
	  (incf fp (length (asmbin-buffer (asmdef-bin def)))))))

(defun generate-mach-o-relvec (obj st)
  (let ((relvec (make-array 0 :fill-pointer t)))
    (add-mach-o-rels-for-defs (asmobj-cdefs obj) st relvec)
    (add-mach-o-rels-for-defs (asmobj-ddefs obj) st relvec)
    relvec))

(defstruct mach-o-metrics
  header-off
  lc-off
  cs-off
  ds-off
  crel-off
  drel-off
  symtab-off
  strtab-off
  strtab-sz
  crel-count
  drel-count
  sym-count)

(defun code-section-size (obj)
  (iter (for def in-vector (asmobj-cdefs obj))
	(sum (length (asmbin-buffer (asmdef-bin def))))))

(defun data-section-size (obj)
  (iter (for def in-vector (asmobj-ddefs obj))
	(sum (length (asmbin-buffer (asmdef-bin def))))))

(defun count-code-relocations (obj)
  (iter (for def in-vector (asmobj-cdefs obj))
	(sum (length (asmbin-relocs (asmdef-bin def))))))

(defun count-data-relocations (obj)
  (iter (for def in-vector (asmobj-ddefs obj))
	(sum  (length (asmbin-relocs (asmdef-bin def))))))

; mach-o file map
; mach header (32 bytes)
; segment command (72 bytes)
; code section descriptor (80 bytes)
; data section descriptor (80 bytes)
; symtab command (24 bytes)
; code section data
; data section data
; relocation entries
; symbol table entries
; string table

(defun compute-mach-o-metrics (obj st)
  (let* ((mh-size (sizeof-c-struct (make-mach-header-64)))
	 (sc-size (sizeof-c-struct (make-segment-command-64)))
	 (sectc-size (sizeof-c-struct (make-section-64)))
	 (symc-size (sizeof-c-struct (make-symtab-command)))
	 (nlist-size (sizeof-c-struct (make-nlist-64)))
	 (rel-size (sizeof-c-struct (make-relocation-info)))
	 (cs-size (code-section-size obj))
	 (ds-size (data-section-size obj))
	 (crel-count (count-code-relocations obj))
	 (drel-count (count-data-relocations obj))
	 (crel-size (* rel-size crel-count))
	 (drel-size (* rel-size drel-count))
	 (sym-count (mach-o-symtab-count st))
	 (symt-size (* nlist-size sym-count))
	 (header-off 0)
	 (lc-off mh-size)
	 (lc-size (+ sc-size (* 2 sectc-size) symc-size))
	 (cs-off (+ lc-off lc-size))
	 (ds-off (+ cs-off cs-size))
	 (crel-off (+ ds-off ds-size))
	 (drel-off (+ crel-off crel-size))
	 (symtab-off (+ drel-off drel-size))
	 (strtab-off (+ symtab-off symt-size))
	 (strtab-sz (mach-o-symtab-strsize st)))
    (make-mach-o-metrics :header-off header-off
			 :lc-off lc-off
			 :cs-off cs-off
			 :ds-off ds-off
			 :crel-off crel-off
			 :drel-off drel-off
			 :symtab-off symtab-off
			 :strtab-off strtab-off
			 :strtab-sz strtab-sz
			 :crel-count crel-count
			 :drel-count drel-count
			 :sym-count sym-count)))

    
(defun emit-mach-o-header (met frag)
  (let* ((cmdsize (- (mach-o-metrics-cs-off met) (mach-o-metrics-lc-off met)))
	 (mh (make-mach-header-64 :magic +mh-magic-64+
				  :cputype +cpu-type-x86-64+
				  :cpusubtype +cpu-subtype-all+
				  :filetype +mh-object+
				  :ncmds 2
				  :sizeofcmds cmdsize
				  :flags +mh-def-flags+
				  :reserved 0)))
    (emit-c-struct mh frag)))

(defun emit-mach-o-seg-lc (met frag)
  (let* ((sc-size (sizeof-c-struct (make-segment-command-64)))
	 (sct-size (sizeof-c-struct (make-section-64)))
	 (lc-size (+ sc-size (* 2 sct-size)))
	 (seg-size (- (mach-o-metrics-crel-off met) 
		      (mach-o-metrics-cs-off met)))
	 (cs-size (- (mach-o-metrics-ds-off met) (mach-o-metrics-cs-off met)))
	 (ds-size (- (mach-o-metrics-crel-off met) (mach-o-metrics-ds-off met)))
	 (cs-addr 0)
	 (ds-addr cs-size)
	 (crel-count (mach-o-metrics-crel-count met))
	 (drel-count (mach-o-metrics-drel-count met))
	 (crel-off (if (> crel-count 0) (mach-o-metrics-crel-off met) 0))
	 (drel-off (if (> drel-count 0) (mach-o-metrics-drel-off met) 0))
	 (seg-lc (make-segment-command-64 :cmd +lc-segment-64+
					  :cmdsize lc-size
					  :segname ""
					  :vmaddr 0
					  :vmsize seg-size
					  :fileoff (mach-o-metrics-cs-off met)
					  :filesize seg-size
					  :maxprot +mh-def-prot+
					  :initprot +mh-def-prot+
					  :nsects 2
					  :flags 0))
	 (cs-lc (make-section-64 :sectname (asciify-string "__text")
				 :segname (asciify-string "__TEXT")
				 :addr cs-addr
				 :size cs-size
				 :offset (mach-o-metrics-cs-off met)
				 :align 0
				 :reloff crel-off
				 :nreloc crel-count
				 :flags +mh-def-text-flags+
				 :reserved1 0
				 :reserved2 0
				 :reserved3 0))
	 (ds-lc (make-section-64 :sectname (asciify-string "__data")
				 :segname (asciify-string "__DATA")
				 :addr ds-addr
				 :size ds-size
				 :offset (mach-o-metrics-ds-off met)
				 :align 0
				 :reloff drel-off
				 :nreloc drel-count
				 :flags +mh-def-data-flags+
				 :reserved1 0
				 :reserved2 0
				 :reserved3 0)))
    (emit-c-struct seg-lc frag)
    (emit-c-struct cs-lc frag)
    (emit-c-struct ds-lc frag)))

(defun emit-mach-o-symtab-lc (met frag)
  (let* ((symc-size (sizeof-c-struct (make-symtab-command)))
	 (symt-lc 
	  (make-symtab-command :cmd +lc-symtab+
			       :cmdsize symc-size
			       :symoff (mach-o-metrics-symtab-off met)
			       :nsyms (mach-o-metrics-sym-count met)
			       :stroff (mach-o-metrics-strtab-off met)
			       :strsize (mach-o-metrics-strtab-sz met))))
    (emit-c-struct symt-lc frag)))

(defun emit-mach-o-def-vec (defs frag)
  (iter (for def in-vector defs)
	(emit-byte-vector frag (asmbin-buffer (asmdef-bin def)))))

(defun emit-mach-o-sects (obj frag)
  (emit-mach-o-def-vec (asmobj-cdefs obj) frag)
  (emit-mach-o-def-vec (asmobj-ddefs obj) frag))

(defun emit-mach-o-symtab (tab frag)
  (iter (for nl in-vector (mach-o-symtab-nvec tab))
	(emit-c-struct nl frag))
  (emit-byte-vector frag (strtab-vec (mach-o-symtab-strtab tab))))

(defun emit-mach-o-relocs (rv frag)
  (iter (for rel in-vector rv)
	(emit-c-struct rel frag)))
  
(defun generate-mach-o-obj (obj)
  (let* ((frag (new-asmfrag))
	 (st (generate-mach-o-symtab obj))
	 (rv (generate-mach-o-relvec obj st))
	 (met (compute-mach-o-metrics obj st)))
    (emit-mach-o-header met frag)
    (emit-mach-o-seg-lc met frag)
    (emit-mach-o-symtab-lc met frag)
    (emit-mach-o-sects obj frag)
    (emit-mach-o-relocs rv frag)
    (emit-mach-o-symtab st frag)
    (asmfrag-buffer frag)))
