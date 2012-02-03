amd64-asm is a Lisp library for generating AMD64 machine code. 

It features:

- A custom, sexpr-based assembler syntax
- Support for much of the "general purpose instructions" subset
- Nearly complete support for the "128-bit media instructions" subset
- A table-driven encoder for easy definition of new instructions
- Support for undefined references and data relocations
- A test-suite built around comparing the library's output to that of yasm
- A random tester, guided by the encoding tables to ensure coverage
- The expected set of encoding optimizations, including jump relaxation
- Direct generation of Mach-O object files

Notable omissions:

- Support for 16-bit or 32-bit modes
- Instruction aliases (for jxx, cmovxx, setxx)
- Many "antiquated" general purpose instructions
- Instructions that operate on segment, debug, or condition registers
- Segment register overrides for memory operations
- The "system instructions" subset
- The "64-bit media instructions" subset
- The "x87 floating-point" instructions subset

Future work:

- Support for specifying segment overrides in memory references
- A subset of system instructions useful to user-mode code
- Better checking and error messages for incorrect source code

Installation notes:

The library is distributed as a standard ASDF system. It requires cl-iterate.
It has only been tested on SBCL/Darwin. It should, however, work on any CL.
For the testsuite to work, yasm must be installed. The location of the binary
is given by the variable *yasmbin*, which defaults to /usr/local/bin/yasm.

Assembler syntax:

This section gives a pseudo-grammar for assembly fragments. Note that in the 
description {FOO} means "one or more FOO", while [FOO] means "zero or one FOO".

MODULE: ({DEFINITION})
DEFINITION: (DECL SCOPE NAME {STATEMENT})
DECL: :proc | :var
SCOPE: :int | :ext
NAME: a symbol specifying the name of the definition

For data definitions (DECL = :var), the syntax for statements is:

STATEMENT: (WIDTH-SPECIFIER VALUE) | SYMCONST
WIDTH-SPECIFIER: one of :byte, :half, :word, or :wide
VALUE: an appropriately-sized integer
SYMCONST: (WIDTH-SPECIFIER NAME [ADDEND])
NAME: a symbol naming an external value
ADDEND: a signed integer offset from the named symbol

For code definitions (DECL = :proc), the syntax for statements is:

STATEMENT: LABEL | INSTRUCTION
LABEL: a symbol naming the label
INSTRUCTION: (MNEMONIC {OPERAND})
MNEMONIC: an AMD64 instruction name, as a keyword
OPERAND: REGISTER | IMMEDIATE | MEM-REF
REGISTER: an AMD64 byte, dword, qword, or xmm register, as a keyword
IMMEDIATE: an appropriately-sized integer | SYMCONST
MEM-REF: (WIDTH-SPECIFIER BASE-SPECIFIER INDEX-SPECIFIER SCALE IMMEDIATE)
BASE-SPECIFIER: REGISTER | :rip | :abs
INDEX-SPECIFIER: REGISTER | nil
SCALE: one of 1, 2, 4, or 8

For memory operands, a width specifier is required, and that specifier must be
compatible with the other operand to the instruction. Addressing is 64-bit, so
BASE-SPECIFIER and INDEX-SPECIFIER should be 64-bit registers. The special
keywords :rip and :abs as BASE-SPECIFIER's signify RIP-relative and absolute
addressing, respectively. Stack instructions are always 64-bit in AMD64, so
pushing less than a full register is not possible. Scale and displacement must
be integers (specifying nil to signify no scale or displacement is not allowed).

Since instruction aliases aren't supported, only the following cc codes are
recognized for jxx/cmovxx/setxx instructions: 
o no b nb z nz be nbe s ns p np l ge le g

Since the assembler does not default the width of symbolic constants even when
doing so would be unambiguous, the call syntax is slightly awkward:

(call (:half foo)) instead of (call foo)

Note that the binary emitter doesn't try to translate Lisp symbol names. When
interfacing with C code, it is generally necessary to use the case-preserving
syntax for symbols and to avoid special characters in names.

The following bit of code demonstrates an assembly source fragment. 

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

Assembler interface: 

The assembler has a very simple interface. The library is contained in a package
AMD64-ASM, which has the nickname ASM. It exposes one main function:

(assemble-and-output source type file)

This function assembles a source module and writes it to a binary object file.

'source' is a source fragment
'type' is the object type --- the only supported one right now is :mach-o
'file' is the name of the output file

The test suite is exposed via another function:

(run-tests)

This function runs all the tests defined in the test suite.

Assembler internals:

The code is small and simple. Read it ;) Adding a new instruction is usually as
easy as adding a new pattern to encoders.lisp. There is a large comment in that
file that gives the syntax for defining new encoders. 

Licensing: 

This work is copyright 2007 by Rayiner Hashem. Others may use this code 
freely under the terms of the GNU Lesser General Public License (LGPL) with 
Franz Inc's clarified preamble for Lisp libraries. 
See: http://opensource.franz.com/preamble.html
