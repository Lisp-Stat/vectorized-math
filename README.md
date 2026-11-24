<!-- PROJECT SHIELDS -->

[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MS-PL License][license-shield]][license-url]
[![LinkedIn][linkedin-shield]][linkedin-url]



<!-- PROJECT LOGO -->
<br />
<p align="center">
  <a href="https://github.com/lisp-stat/vectorized-math">
    <img src="https://lisp-stat.dev/images/stats-image.svg" alt="Logo" width="80" height="80">
  </a>

  <h3 align="center">Vectorized Math</h3>

  <p align="center">
  Vectorized numeric operators for Common Lisp: CL's own operators for numbers, elementwise for arrays
	<br />
    <a href="https://lisp-stat.dev/docs/"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/Lisp-Stat/vectorized-math/issues">Report Bug</a>
    ·
    <a href="https://github.com/Lisp-Stat/vectorized-math/issues">Request Feature</a>
  </p>
</p>



<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary><h2 style="display: inline-block">Table of Contents</h2></summary>
  <ol>
    <li>
      <a href="#about-the-project">About the Project</a>
      <ul>
        <li><a href="#objectives">Objectives</a></li>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
        <li><a href="#quick-start">Quick start</a></li>
        <li><a href="#running-the-tests">Running the tests</a></li>
      </ul>
    </li>
    <li>
      <a href="#usage">Usage</a>
      <ul>
        <li><a href="#using-the-operators-in-your-package">Using the Operators in Your Package</a></li>
        <li><a href="#semantics">Semantics</a></li>
        <li><a href="#operator-status">Operator Status</a></li>
        <li><a href="#configuration">Configuration</a></li>
        <li><a href="#locating-mkl">Locating MKL</a></li>
        <li><a href="#performance">Performance</a></li>
        <li><a href="#compatibility-notes">Compatibility notes</a></li>
      </ul>
    </li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#known-issues">Known Issues</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About the Project

Vectorized-math is a superset of Common Lisp's numeric operators. The package `VM` exports replacements for operators such as `+`. Applied to numbers, they are exactly the CL operators: same results, same types, same errors, and (through compiler macros) the same speed. Applied to arrays, they work elementwise, recycling the shorter operand as R does. Float and complex float arrays are computed with the Intel Math Kernel Library's Vector Math (VML) functions when MKL is installed, and with typed Lisp loops otherwise.

```lisp
(+ 1 2)                                   ; => 3, as CL
(+ #(1d0 2d0 3d0) 10d0)                   ; => #(11.0d0 12.0d0 13.0d0)
(+ (make-array 3 :element-type 'double-float :initial-element 1d0)
   (make-array 3 :element-type 'double-float :initial-element 2d0))
                                          ; => #(3.0d0 3.0d0 3.0d0) (MKL is used above the size cutoff)
```

### Objectives

- **Exact CL for numbers**: A package that shadows the vm operators loses nothing. Scalar code returns the same values, signals the same conditions, and compiles to the same machine code when argument types are declared. The REPL variables (`+`, `++`, `+++`) and method combinations named `+` keep working.

- **Elementwise for arrays**: Operands may be numbers or arrays of any rank, with R's recycling rules and strict CL contagion, including complex arrays and integer arrays.

- **MKL speed with a pure Lisp fallback**: VML for single and double float and complex arrays above a size cutoff, typed Lisp loops for small arrays, integer arrays and when MKL is not installed.

- **Predictable results**: Array operations use IEEE arithmetic (Inf and NaN, no floating point traps), with MKL in its high-accuracy mode, so results do not depend on which backend ran.

### Built With

* [Intel oneAPI Math Kernel Library](https://www.intel.com/content/www/us/en/developer/tools/oneapi/onemkl.html) (optional, loaded at run time)
* [CFFI](https://github.com/cffi/cffi)
* [clunit2](https://codeberg.org/cage/clunit2) (tests)
* [Bordeaux Threads](https://github.com/sionescu/bordeaux-threads) (tests)



<!-- GETTING STARTED -->
## Getting Started

### Prerequisites

* A Common Lisp implementation: SBCL (tested: 2.6.2) or Clozure CL (tested: 1.14, built with the pull requests listed under [Known Issues](#known-issues)) on Linux x86-64. Other platforms and implementations are untested; the type-specifier hook (see "The `*` wildcard") and the NaN, pointer-pinning and trap-masking code are implementation specific and exist for these two only.
* [CFFI](https://cffi.common-lisp.dev/) (the portable foreign function interface; loaded automatically by Quicklisp).
* Optional, but required for the speed-up on float arrays: Intel oneAPI Math Kernel Library (the single dynamic library `libmkl_rt`). vectorized-math loads it itself; no other Lisp system is needed. Without MKL, vectorized-math prints one warning when loaded and uses Lisp loops for everything. See [Locating MKL](#locating-mkl) if MKL is not on the system's library search path.

On SBCL, loading the system also requires `sb-cltl2` (part of SBCL, `(require :sb-cltl2)` is done by the system definition).

### Installation

#### Using Quicklisp

```lisp
(ql:quickload :vectorized-math)
```

Note: Quicklisp distributions are not updated frequently. If the system is not available through Quicklisp, use the manual installation method below.

#### Manual Installation

Clone the repository in a directory ASDF knows about. By default the `common-lisp` directory in your home directory is known. Create this if it doesn't already exist and then:

1. Clone the repository
```sh
cd ~/common-lisp && \
git clone https://github.com/Lisp-Stat/vectorized-math.git
```
2. Reset the ASDF source-registry to find the new system (from the REPL)
   ```lisp
   (asdf:clear-source-registry)
   ```
3. Load the system
   ```lisp
   (ql:quickload :vectorized-math)   ; also fetches dependencies
   ```

#### Quick start

```lisp
(ql:quickload :vectorized-math)

(defpackage #:demo
  (:use #:cl)
  (:shadowing-import-from #:vm . #.(mapcar #'symbol-name (vm:shadowed-symbols))))
(in-package #:demo)

(defun darray (&rest xs)
  (make-array (length xs) :element-type 'double-float :initial-contents xs))

(let ((x (darray 1d0 2d0 3d0 4d0)))
  (+ x 10d0)                      ; => #(11.0d0 12.0d0 13.0d0 14.0d0)
  (* x x)                         ; => #(1.0d0 4.0d0 9.0d0 16.0d0)
  (sqrt x)                        ; => #(1.0d0 1.4142135623730951d0 1.7320508075688772d0 2.0d0)
  (< x 2.5d0)                     ; => #*1100  (a bit array)
  (max x 2.5d0)                   ; => #(2.5d0 2.5d0 3.0d0 4.0d0)
  (floor (darray 7d0 -7d0) 2d0)   ; => #(3 -4), #(1.0d0 1.0d0)  (quotient and remainder arrays)
  (+ x x :into x)                 ; => x itself, now #(2.0d0 4.0d0 6.0d0 8.0d0)
  (+ 1 2))                        ; => 3, exactly CL
```

`(sqrt (darray -4d0 9d0))` is `#(#C(0.0d0 2.0d0) #C(3.0d0 0.0d0))`: where CL's function would return a complex number for an element, the whole result is a complex array. Integer arrays stay integer (`(+ #(1 2 3) 1)` with a `(signed-byte 64)` array is `#(2 3 4)`; `(/ #(1 2 3) 2)` is the exact `#(1/2 1 3/2)` of element type `T`).

MKL is optional. Without it the first load prints one warning and every array operation uses typed Lisp loops (same results, less speed). If MKL is not found by the system's dynamic loader, set `VM_MKL_LIBRARY` to the full path of `libmkl_rt` before loading, or call `(vm:load-mkl "/path/to/libmkl_rt.so.3")`; see [Locating MKL](#locating-mkl).

To use the operators only in one place, call them qualified (`(vm:+ a b)`) without shadowing anything.

#### Running the tests

```lisp
(asdf:test-system :vectorized-math)       ; the vm suite (clunit2, about 157,000 assertions, about a minute)
```

Other checks, none of them part of that suite:

* `tests/repl-check.sh ccl` and `tests/repl-check.sh sbcl` pipe a session into a real REPL to check the REPL variables (`+ ++ +++`, ...; also `boundp`, `symbol-value` and a global `setf` from another thread) and the wildcard in a package that shadows the operators. `tests/repl-fallback-check.sh [ccl-command]` checks, on CCL, that a failed self-test (`VM_REPL_VARIABLES=fail-self-test`) or `VM_REPL_VARIABLES=symbol-macro` leaves symbol macros and no trace of the aliases.
* `tests/diff-kernels.lisp` is the differential test of the typed kernels against CL itself on large corpora (`DIFF_N` pairs per class and operation, default 200,000; exit status 1 on any difference): every element of `floor ... rem` and `sqrt` that a kernel computes must be `eql` to CL's.
* `tests/large-arrays.lisp` runs arrays of more than 2^30 elements (about 21 GB of memory at its peak, so it is never part of the suite): it checks the elements around the 2^30 chunk boundary of the MKL calls.
* `tests/bench-scalar.lisp` measures the scalar overhead of the compiler macros and compares the disassembly of `(vm:op ...)` with `(cl:op ...)`.
* `bench/` holds the benchmark suite behind the [Performance](#performance) tables.
* The ANSI conformance comparison (every test of the ANSI-TEST suite run in an image where the operators are shadowed, against the same suite in a plain image) uses `ansi-test-runner/run-numbers-vm.sh` of the Lisp-Stat workspace: `ANSI_DIR=<your copy of ansi-test>/ ansi-test-runner/run-numbers-vm.sh ccl|sbcl OUTFILE [numbers|all]`, `ANSI_FILTER_MINI_UNIVERSE=1` for the filtered form. See [Compatibility notes](#compatibility-notes) for the results.

<!-- USAGE -->
## Usage

### Using the Operators in Your Package

The symbols to use are exported from `VM`; `vm:shadowed-symbols` lists the operators a user package should shadow:

```lisp
(defpackage #:my-project
  (:use #:cl)
  (:shadowing-import-from #:vm . #.(mapcar #'symbol-name (vm:shadowed-symbols))))
```

or, naming the operators explicitly:

```lisp
(defpackage #:my-project
  (:use #:cl)
  (:shadowing-import-from #:vm #:+ #:- #:* #:/))
```

Inside `my-project`, `+` is `vm:+`. Nothing else changes: `(+ 1 2)` is `3`; `(funcall #'+ 1 2)`, `(apply #'+ list)` and `(defgeneric f (x) (:method-combination +))` work as in CL; and `+ - * /` evaluated as variables are still the REPL's variables (`+ ++ +++`, `- * ** ***`, `/ // ///`), because `vm:+ vm:- vm:* vm:/` are also symbol macros for the CL symbols.

**The `*` wildcard.** Shadowing `*` changes how `(simple-array double-float (*))`, `(function (fixnum) *)` and `(deftype vec (&optional (n '*)) ...)` read: the `*` is `vm:*`, which no implementation's type system knows. Loading vectorized-math therefore installs a small hook in the implementation's type-specifier parser that treats `vm:*` as `cl:*` (CCL: `ccl:advise` on `ccl::type-expand` and `ccl::%deftype`; SBCL: `sb-int:encapsulate` on `sb-kernel::%parse-type` and `sb-impl::%deftype`). The same hook reads `*` as the "any qualifiers" pattern in the method group specifiers of the long form of `define-method-combination` (`((methods *))`; CCL: advice on `ccl::parse-method-group-specifiers`, SBCL: encapsulation of `sb-pcl::expand-long-defcombin`). This patches implementation internals, so load vectorized-math before the code that uses the wildcard, and note that it needs porting for other implementations (it warns and does nothing on them). Installation is idempotent, and loading ends with a self-test that signals an error with a clear message if the hook does not work (for example on a future SBCL that changed those internals). A `deftype` that tests its argument with `(eq n '*)` sees its own `*`; the hook canonicalises the expansion. Error messages print the canonical wildcard with a package prefix (`COMMON-LISP:*`).

Compiler macros keep scalar code fast. A `(+ ...)` (or `- * / 1+ 1-`) form whose arguments are all known to be numbers (constants, `(the fixnum x)`, variables declared `fixnum`, `double-float`, `number` and so on) is compiled exactly as `cl:+`. Otherwise an inline `numberp` test selects `cl:+` or the array path, evaluating the arguments once, left to right. A form containing a literal `:into` is not touched.

The implementation lives in a separate package, `VM.IMPL`, which uses `CL` and shadows nothing; `VM` contains only the exported interface.

### Semantics

* **Numbers**: exactly CL. `(+)` is `0`; `(+ x)` signals CL's `type-error` unless `x` is a number or an array.
* **Arrays**: when any operand is an array the operator is elementwise and returns a fresh simple array; no argument is modified. Lists are not vectors (they are not numbers, and CL signals a type error). Non-simple arrays (displaced, with fill pointers) are accepted. An array that cannot hold numbers (a string, a `T` array with a non-number in it, even when the result would have zero length) signals a `type-error` with expected type `number` and the array as datum.
* **N-ary operators** fold left, pairwise: `(+ a b c)` is `(+ (+ a b) c)`, with the rules below at each step.
* **Shapes (R's recycling rules)**:
  * two arrays of rank 2 or more must have equal dimensions, otherwise `vm:non-conformable-arrays` is signalled;
  * an array of rank 2 or more and a vector: the vector may not be longer than the array; it is recycled over the array in *column-major* order, as R does, and the result has the array's dimensions;
  * two vectors: the result has the length of the longer; the shorter is recycled, with a `vm:recycling-warning` if the longer length is not a multiple of the shorter;
  * a zero-length operand gives a zero-length result;
  * a scalar is broadcast;
  * a rank-0 array behaves as a length-1 array; when no operand has more than one element the result is rank-0.
* **Contagion** is CL's: integer + single-float gives single-float, single + double gives double, real + complex gives a complex of the wider float type. Single and double floats and complex floats are computed by MKL (above the cutoff) or by typed Lisp loops.
* **Integer arrays** stay integer for `+ - *`: `(unsigned-byte n)`, `(signed-byte n)` and `fixnum` arrays give `(signed-byte 64)` arrays when every result fits (when overflow is impossible from the operand ranges, which for `(signed-byte 64)` arrays are the minimum and maximum of the data, found by one scan, the loop is typed; otherwise the result is computed exactly and narrowed back), else an array with element type `T` holding bignums, exactly as CL would compute them. An integer array with a float operand is converted to the float type in a temporary.
* **`-`, `*`, `/`**: `(- x)` negates (an array elementwise; `(- 0.0)` is `-0.0`), `(- a b c)` is `(a-b)-c` and the order is respected for scalar-array and array-scalar operands: `(- v 1)` and `(- 1 v)` differ. `(*)` is `1`, `(/ x)` is the reciprocal. `(-)` and `(/)` signal CL's own argument-count error. `1+` and `1-` add and subtract 1 elementwise.
* **Division**: integer and rational operands give exact rationals in an array of element type `T` (`(/ #(1 2 3) 2)` is `#(1/2 1 3/2)`, and `(/ #(4 6) #(2 3))` is the `T` array `#(2 2)`, never narrowed back to integers, so the element type is predictable). Integer division by zero signals `division-by-zero`, as in CL. Float arrays give IEEE Inf and NaN, also when the divisor is the integer `0` (`(/ #(1d0) 0)`). An intermediate rational result inside one n-ary call keeps being treated as rational, so `(/ #(1 2) 2 0.5d0)` is a double-float array.
* **`incf` and `decf`** are macros with CL's semantics (place subforms evaluated once, left to right, then the delta, then the place is read) that use `vm:+` and `vm:-`: `(incf (aref m 0) v)` adds an array to an array held in `m`.
* **`:into`**: `(+ a b :into out)` as the last two arguments stores the result in the array `out`, which is returned. The computation is recycled or truncated to `out`'s size (with a `vm:recycling-warning` if it is not an exact multiple); widening conversions are made, narrowing (double into single, float into integer, complex into real) signals `vm:output-type-error` and leaves `out` unchanged. `out` may be one of the operands. Since `cl:+` rejects keywords, existing code is unaffected.
* **IEEE floating point**: array operations run with floating point traps masked and return Inf and NaN as IEEE specifies, never a floating point condition. Scalar operations are untouched: overflow signals, as in CL, unless you have changed the trap mask. MKL is set (per thread, lazily) to high accuracy, errors ignored, flush-to-zero off, so MKL and Lisp loop results agree.
* **Logical results**: comparisons and predicates return `(simple-array bit)` arrays (1 for true, 0 for false) of the result shape, see "Comparisons, predicates, max and min" below.

### Operator Status

| Operators | Status | Array backend | Notes |
|---|---|---|---|
| `+ - * / 1+ 1-`, `incf decf` | implemented | MKL above the cutoff for float and complex float arrays (complex `* /`: always MKL); typed loops for integer arrays and below the cutoff | |
| `exp expt log sqrt isqrt` | implemented | MKL for float arrays (real `sqrt`: a correctly rounded Lisp loop; `expt` with an integer exponent and `isqrt`: CL's function per element) | `log` and `atan` take their optional argument as in CL; `expt` and two-argument `log` use the shape rules and `:into` |
| `sin cos tan asin acos atan` (one and two arguments), `sinh cosh tanh asinh acosh atanh`, `cis` | implemented | MKL | |
| `abs signum conjugate phase realpart imagpart complex` | implemented | see "Exponential, trigonometric and complex-part operators" | `(complex re im)` follows the shape rules |
| `= /= < > <= >=`, `zerop plusp minusp evenp oddp` | implemented | typed Lisp loops | bit arrays for arrays, CL's `t`/`nil` for numbers; `= /=` also for complex |
| `max min` | implemented | typed Lisp loops | elementwise (R's `pmax`/`pmin`), contagion element type, a NaN propagates; also the method combinations `max` and `min` |
| `floor ceiling truncate round`, `ffloor fceiling ftruncate fround` | implemented | typed Lisp loops (non-consing) | two values for arrays too, both arrays; `:into` receives the quotient; see "Division, rounding, `gcd` and `lcm`" |
| `mod rem`, `gcd lcm` | implemented | typed Lisp loops | one array; `gcd` and `lcm` are n-ary and for integer arrays only |
| `logand logior logxor logeqv`, `lognand lognor logandc1 logandc2 logorc1 logorc2`, `lognot logcount integer-length`, `ash` | implemented | typed Lisp loops | integer arrays only; `(signed-byte 64)` results when every value fits, else `T`; see "Integer, bitwise and conversion operators" |
| `logbitp logtest` | implemented | typed Lisp loops | bit arrays |
| `float rational rationalize numerator denominator scale-float float-sign` | implemented | typed Lisp loops (`float`, `scale-float`, `float-sign`); CL's function per element (the others) | float arrays for `float scale-float float-sign`, integer or `T` arrays for the others |

Not planned: the type predicates (`numberp`, `integerp`, ... must keep answering NIL for arrays), byte operations, `random`, `decode-float` and relatives, `boole`, `parse-integer`, `coerce`.

`vm:shadowed-symbols` returns every implemented operator (81): `vm:+ vm:- vm:* vm:/ vm:1+ vm:1- vm:incf vm:decf vm:exp vm:expt vm:log vm:sqrt vm:isqrt vm:sin vm:cos vm:tan vm:asin vm:acos vm:atan vm:sinh vm:cosh vm:tanh vm:asinh vm:acosh vm:atanh vm:cis vm:abs vm:signum vm:conjugate vm:phase vm:realpart vm:imagpart vm:complex vm:= vm:/= vm:< vm:> vm:<= vm:>= vm:zerop vm:plusp vm:minusp vm:evenp vm:oddp vm:max vm:min vm:floor vm:ceiling vm:truncate vm:round vm:ffloor vm:fceiling vm:ftruncate vm:fround vm:mod vm:rem vm:gcd vm:lcm vm:logand vm:logior vm:logxor vm:logeqv vm:lognand vm:lognor vm:logandc1 vm:logandc2 vm:logorc1 vm:logorc2 vm:lognot vm:logcount vm:integer-length vm:ash vm:logbitp vm:logtest vm:float vm:rational vm:rationalize vm:numerator vm:denominator vm:scale-float vm:float-sign`. `vm:+`, `vm:max` and `vm:min` are also the built-in method-combination names: `(define-method-combination vm:max :identity-with-one-argument t)` (likewise `vm:min`) makes `(defgeneric f (x) (:method-combination max))`, with `:most-specific-last`, work in a package that shadows them.

**`complex`, `float`, `rational` and `mod` are also types and classes.** A package that shadows them can still write `float`, `(float 0.0 1.0)`, `(rational * 0)`, `complex`, `(complex double-float)` and `(mod 256)` as types, and name the classes: `(typep x 'float)`, `(subtypep 'rational 'real)`, `(typecase x (float ...) (rational ...))`, `(check-type x float)`, `(coerce 1 'float)`, `(declare (type float x))` and the shorthand `(declare (float x))` (the declaration is respected by the compiler: a violation signals a `type-error` at safety 3), `(declaim (ftype (function (float) rational) f))`, `deftype` bodies, `defstruct` and `defclass` slot types, `(find-class 'float)`, and `defmethod` specialisers `((x float))`, `((x rational))`, `((x complex))`. Two mechanisms do this: the type-specifier hook that handles `*` also reads `vm:complex`, `vm:float`, `vm:rational` and `vm:mod` as the CL symbols wherever it canonicalises a type specifier (including the expansion of a user `deftype`, but not inside `member`, `eql`, `satisfies` or `quote`, so `(typep 'float '(member float))` still means what it says), and each of `vm:complex`, `vm:float` and `vm:rational` is also registered as a name of the CL class (`(setf (find-class 'vm:float) (find-class 'cl:float))`, done at load time; `class-name` stays `cl:float`). What cannot work is the symbol identity: `(eq 'float 'cl:float)` is false in such a package, and `(eql x 'float)` specialisers and `(member float)` types refer to `vm:float`. `tests/typenames.lisp` runs a corpus of 142 forms (types, classes, methods, `typecase`, `check-type`, `coerce`, declarations, `deftype`, structures, classes, compiled and loaded files) once in a shadowing package and once in a plain one and compares the printed results.

#### Comparisons, predicates, `max` and `min`

* **Numbers are CL's**: `(< 1 2 3)` is `t`, `(=)` is CL's argument-count error, `(zerop 0.0)` is `t`, `(max 1 2.0)` is whatever `cl:max` returns. Scalars are passed to `cl:=` and friends untouched, so whatever the implementation does for them (including its handling of a NaN, see below) is what you get.
* **Arrays give bit arrays.** `= /= < > <= >=` and `zerop plusp minusp evenp oddp` return a `(simple-array bit)` with 1 where the relation holds, with the result shape of the R rules (recycling, column-major emulation, `vm:recycling-warning`, `vm:non-conformable-arrays`). `(= v)` with one array is a bit array of ones. Rank-0 arrays give a rank-0 result when nothing else is bigger.
* **N-ary chains** are elementwise over *one* common shape: `(< a b c)` is `(and (< a b) (< b c))` for every element, with each array operand recycled to the shape the R rules give for all of them together (so `(< #(1 2 3 4 5 6) #(2 3) #(9))` warns about nothing, and an operand whose length does not divide the common length warns once). `/=` follows CL: elementwise "all pairwise distinct", every pair `(/= a b) (/= a c) (/= b c) ...` must hold. Scalar pairs inside a chain are CL's own answer: `(< 2 1 v)` is a bit array of zeros of `v`'s shape.
* **Exact comparison of floats and rationals**, as CL requires: `(= 0.1d0 1/10)` is false, `(< 9007199254740992d0 9007199254740993)` is true, a float against an integer array, a single against a double and so on are compared exactly (never by rounding the rational to a float). Integer arrays against float operands run in a double-float loop when every integer is within +-2^53 (checked from the array's data, not only its element type) and a rational scalar is converted only when that is exact; everything else (ratios that are not floats, bignums, element type `T`) goes through `cl:op` per element. Results equal `cl:op` for every pair of the test corpus.
* **NaN in arrays follows IEEE, whatever the implementation does:** every relation with a NaN is 0 (`=`, `<`, `>`, `<=`, `>=`, and `zerop plusp minusp`), except `/=` which is 1, and a NaN is not equal to itself. `-0.0` equals `0.0` (`zerop` is true, `minusp` false, as in CL). Complex numbers with a NaN part count as NaN. CCL's own scalar comparisons of floats against a NaN are wrong (an open upstream CCL pull request, "Answer NIL from float compares against a NaN", fixes them: they treat a NaN as less than, less or equal to and equal to everything), so the library never trusts a comparison with a NaN: a NaN is recognised from its IEEE bits (never from a comparison, so the result does not depend on how the implementation compares a NaN, and stays right when CCL's comparisons are fixed), and the kernels give the same bits on CCL and SBCL. *Scalars are not changed*: `(< (nan) 1d0)` is whatever the implementation says.
* **Complex operands**: `=` and `/=` accept them (arrays, scalars, any mixture with real operands); `< > <= >=`, `plusp` and `minusp` signal CL's `type-error` (expected type `real`) for a complex operand. `zerop` of a complex array is allowed. `evenp` and `oddp` are for integer arrays: a float or complex array signals the `type-error` of `cl:evenp` (expected type `integer`); an array with element type `T` is checked element by element.
* **`max` and `min`** are the elementwise maximum and minimum (R's `pmax` and `pmin`) with the usual shape rules, n-ary (folded left), and `:into`. The result element type is the contagion type of the operands: integer arrays with integer arrays give `(signed-byte 64)` (element type `T` if a value does not fit); a float operand makes the result single or double float (double if either is), including an integer array with a single-float scalar (`(max #(1 5) 3.5f0)` is a `single-float` array); rationals that are not integers and arrays with element type `T` are computed per element (CL's contagion between the two numbers of each pair) and collected into the tightest array that holds the results, so `(max #(1 5) 1/2)` is an array with element type `T`. The values equal `cl:max`/`cl:min` of each pair converted to the contagion type; a tie returns the first operand (`(max -0.0d0 0.0d0)` is `-0.0d0`, as in CL). **NaN rule: a NaN propagates**, as in R: if either element is a NaN the result is that NaN (as a float of the contagion type). This is implemented in typed Lisp loops, not MKL's `v?Fmax`/`v?Fmin`, which return the non-NaN operand; the operations are memory-bound and the loops are cheap. `max` and `min` of complex operands signal CL's `type-error`.
* **`:into`** for comparisons and predicates: `out` must be able to hold 0 and 1 (a bit array, any integer array that includes both, or an array with element type `T`); a float or complex `out` signals `vm:output-type-error` and is left unchanged. For `max` and `min` the rules of `+` apply.
* **Method combinations.** `max` and `min` are, with `+`, names of CL's built-in method combinations; `vm:max` and `vm:min` are defined as such (`:identity-with-one-argument t`), so `(:method-combination max)` and `:most-specific-last` work in a package that shadows them.
* **Caveat of the superset.** A bit array is a true value, so in a package that shadows these operators `(if (< a b) ...)` with an array argument is always true; test an array result with `(every ...)` or `(find 1 ...)`.

#### Division, rounding, `gcd` and `lcm`

`floor ceiling truncate round` and `ffloor fceiling ftruncate fround` take one or two arguments, `mod` and `rem` two, `gcd` and `lcm` any number. For numbers they are `cl:floor` and friends (both values, types, conditions and argument-count errors are CL's). Everything below is about arrays.

* **Two values, both arrays.** `(floor a b)` with an array operand returns two arrays of the result shape of the R rules (recycling, column-major emulation, warnings, `vm:non-conformable-arrays`): the quotient and the remainder. `(floor v)` is `(floor v 1)`. Every element of both arrays is what `cl:floor` returns for the pair of elements. The quotient of `floor ceiling truncate round` is an integer (CL's quotient always is): its array has element type `(signed-byte 64)` when every quotient fits, else `T` (`(floor #(1d300))` holds a bignum). The quotient of the F family is a float of CL's type for the operands: the float format of a float operand, `single-float` for integers and rationals (as in CL). The remainder array has the contagion type of the operands: a float array gives the float format (double if either is a double), integers `(signed-byte 64)` (or `T` if a remainder does not fit), rationals and arrays with element type `T` the tightest array that holds the results (`T` for rationals). An empty result has the element types the same rules give. `mod`, `rem`, `gcd` and `lcm` return one array.
* **`:into` stores the quotient**, the first value, with the rules of `+` (recycling, widening conversions, `vm:output-type-error` for narrowing, `out` left unchanged on an error) and returns `out` as the first value; the remainder is always a fresh array and the second value. `(floor a b :into out)` with numbers only stores the quotient everywhere in `out`. For `mod`, `rem`, `gcd` and `lcm` `:into` receives the result, as for the other operators.
* **`round` rounds to even** at ties, as CL does: `(round #(0.5d0 1.5d0 2.5d0))` is `#(0 2 2)`. MKL's `v?Round` rounds half away from zero, so it is not used; MKL has no division with remainder, nor an integer quotient. All of these operators are typed Lisp loops (integer arrays within `(signed-byte 64)`, double and single float arrays, with integers that convert exactly: within +-2^53 for doubles, +-2^24 for singles) that compute exactly what the CL function computes (see "Tuning" under [Performance](#performance)), and the generic path (the CL function per element on boxed elements) for everything else: rationals, bignums, element type `T`, mixes that do not convert exactly. A typed loop checks each element and stops at the first it does not handle (a zero divisor, an infinity or NaN, a quotient too big for 64 bits) and the call is redone on the generic path, so a result is the same whatever path computed it and never depends on the array length.
* **Zero divisors and non-finite values.** *Integer division by zero signals `division-by-zero`* as in CL, for integer arrays and for any pair of integer or rational elements, with or without float traps. *`floor ceiling truncate round` of floats* need an integer quotient, which Inf and NaN cannot give: a zero divisor, an infinity or a NaN gives whatever `cl:floor` gives for that pair of scalars in the caller's float trap state, element by element and in order, on the implementation at hand: with the traps as they are (the default) it is the condition CL signals for those scalars (`division-by-zero` for `(floor 1d0 0d0)`, `floating-point-invalid-operation` for a NaN or an infinity, `floating-point-overflow` when the quotient overflows, the same on CCL and SBCL for the pairs of the test corpus); with the traps masked CL's own behaviour is reproduced as it is, which differs between the implementations (CCL's `(floor 1d0 inf)` is `(0 NaN)`, SBCL's signals).
* **The F family, `mod` and `rem` of floats give IEEE results**: no signal, whatever the trap state. For a pair of floats (an integer or rational operand is converted to the float type first, so `(ffloor #(1d0) 0)` is a float division) whose computation CL can do (both finite, divisor not zero, quotient finite) the element is `cl:ffloor` etc. of the pair (so a signal of CL's own function for a float with a huge integer or ratio, SBCL's `(rem 1f0 (expt 10 30))` overflows inside CL, does not occur: the integer is converted first; an integer too big for the float format is CL's own error); for the others the results are defined here, the same on both implementations (`x` the number, `y` the divisor; the remainder of `mod` is the second value of `ffloor`, that of `rem` the second value of `ftruncate`):

  | pair | quotient | remainder |
  |---|---|---|
  | a NaN in `x` or `y` | NaN | NaN |
  | `y` = 0 | `x/y` (Inf with the sign of `x*y`, NaN for `x` = 0) | NaN |
  | `x` = Inf | `x/y` (Inf, NaN when `y` is infinite) | NaN |
  | `x` finite, `y` = Inf, `x` = 0 | `x/y` (a signed zero) | `x` |
  | `x` finite nonzero, `y` = Inf | `x/y` (a signed zero), but `ffloor` gives -1 when `x` and `y` have different signs (remainder `y`) and `fceiling` gives +1 when they have the same sign (remainder `-y`) | `x`; `mod` of a negative `x` by +Inf is +Inf, of a positive `x` by -Inf is -Inf |
  | finite `x`, `y` whose quotient overflows | `x/y` (+-Inf) | the exact remainder (computed with rationals, rounded once to a float) |

  This is fmod-style (and R's) behaviour wherever CL leaves it open. CL itself (`cl:ffloor`, `cl:mod` ...) signals or gives implementation-dependent results for these pairs, so there is nothing to match.
* **What equals CL, and what differs between CCL and SBCL.** Every element equals `cl:op` of the pair on the implementation at hand, bit for bit, for every pair of the test corpus. The two implementations' own `cl:floor` functions differ from each other for floats: the sign of a zero quotient in the F family (`(ftruncate -1 3)` is `-0.0` on SBCL, `0.0` on CCL), how the remainder `x - q*y` is rounded when the quotient is large (`(mod -1000 0.3d0)`), and the result when `q*y` overflows (`(floor 1.7976931348623157d308 -1d300)` is a value on CCL and an overflow error on SBCL). The vectorized operators inherit all of it, so results can differ between CCL and SBCL in those cases.
* **`gcd` and `lcm`** are for integer arrays (and integers) only: a float, a ratio or a complex array or scalar signals CL's `type-error` (expected type `integer`), an array with element type `T` is checked element by element. They fold left with the shape rules at each step, `(gcd a)` is the absolute value. The element type is `(signed-byte 64)`, or `T` when a result does not fit (`(lcm #(4611686018427387904) 3)`, `(gcd most-negative-(signed-byte 64) 0)`), as for `*`.
* **Tests.** `tests/division.lisp` checks: scalars against `cl:op` (including masked traps and arity), arrays against `cl:op` per element on corpora (signed zeros, ties at many magnitudes, large floats, bignums, ratios, negative divisors, random pairs) in three shapes (array/array, array/scalar, scalar/array) and for the typed and the generic path, shapes, `:into`, mutation, conditions, empty arrays, `gcd`/`lcm` overflow.

#### Integer, bitwise and conversion operators

`logand logior logxor logeqv` (n-ary; `(logand)` and `(logeqv)` are -1, `(logior)` and `(logxor)` 0), `lognand lognor logandc1 logandc2 logorc1 logorc2` (two arguments), `lognot logcount integer-length` (one), `ash`, and the bit tests `logbitp` (index, integer) and `logtest`; and `float` (one or two arguments), `rational`, `rationalize`, `numerator`, `denominator`, `scale-float`, `float-sign` (one or two). For numbers they are the CL functions (values, types, conditions, argument-count errors). When an operand is an array the operator is elementwise with the shape rules (two operands recycle, n-ary operators fold left, `:into` as for the other operators) and every element is what `cl:op` returns for the element(s): a typed loop where the loop is the same computation (integers within `(signed-byte 64)`, single and double floats), otherwise `cl:op` per element on the boxed values, so the result never depends on the path or the array length. There is no MKL path: these operations are not floating point arithmetic.

* **Integer operators take integer arrays only**: a float, ratio or complex array, or a scalar that is not an integer, signals CL's `type-error` (expected type `integer`; the datum is the scalar or the first element of the array, also for an empty array of floats; the class is `type-error` where CL's own function signals `simple-type-error`). An array of element type `T` is checked per element by `cl:op`.
* **Result element types**: `(signed-byte 64)` when every value fits, else `T` (bignums): `(ash #(1 2 3) 63)` is a `T` array, so is `(lognot a)` for an `(unsigned-byte 64)` array holding 2^63 or more, while `(ash a 1)` of small values is `(signed-byte 64)`; the type depends on the values, not on the operand types: `(unsigned-byte 64)` operands are typed when the values fit `(signed-byte 64)`. An empty result is `(signed-byte 64)`. `logcount` and `integer-length` give `(signed-byte 64)` arrays; `logbitp` and `logtest` give `(simple-array bit)` (a negative `logbitp` index signals CL's error, an index of 64 or more reads the sign bit of a 64-bit value as CL does). `ash` with a left shift that overflows 64 bits is computed with bignums.
* **`float`**: `(float a)` gives single floats for integer and rational arrays and a copy for float arrays (as CL's `(float x)` returns `x`); `(float a proto)` gives a float array of the format of `proto`; either may be an array (R recycling), so `(float 1 #(1d0 2d0))` is a double array. A prototype of element type `T` with mixed formats gives element type `T`. Conversion runs with the float traps masked: `(float #(1d300) 1f0)` is infinity where the scalar signals. An empty result has the prototype's format, `single-float` when there is none.
* **`rational`, `rationalize`**: float arrays give `(signed-byte 64)` when every value is integral and fits (CL returns integers for integral floats), else `T`; integer arrays are copied. NaN and infinities: whatever `cl:rational` does for the element (an error on SBCL, a huge garbage integer on CCL), in the caller's trap state. `rationalize` may return a less simple fraction on CCL than on SBCL for some tiny floats (both are within the float's accuracy). `numerator` and `denominator` take integer and rational arrays (a float array signals CL's `type-error`, expected type `rational`): `(numerator #(1/2 3))` is `#(1 3)`, `(denominator #(1/2 3))` is `#(2 1)`.
* **`scale-float`** takes a float array (or float) and an integer array (or integer); the result has the float's format. It runs masked: overflow gives infinity and underflow zero or a denormal (the unmasked scalar signals `floating-point-overflow`), on both implementations. On CCL 1.14 and earlier the scale must be a fixnum (a CCL bug; see Known Issues).
* **`float-sign`** takes one or two float arguments. With two formats that differ the result format is whatever CL's function returns (the format of the second argument on CCL, the contagion of the two on SBCL), so this case is computed by `cl:float-sign` per element.
* **Tests.** `tests/integer.lisp` compares every operator with `cl:op`: scalars (values, conditions, masked and unmasked traps, arities) over corpora of integers (signed and unsigned 64-bit edges, bignums), ratios, floats (signed zeros, denormals, huge, infinities and NaNs) and non-numbers; arrays element by element for every element-type combination and the three argument layouts (array/array, array/scalar, scalar/array), the result element types, the shapes of the R rules, `:into`, mutation, type errors, empty arrays, the compiler macros. The typed and generic paths are compared on the same data.

#### Exponential, trigonometric and complex-part operators

* **Result element types follow CL, per element type.** Float arrays keep their float format (single stays single, double stays double); complex float arrays stay complex of that format. Irrational functions of integers and rationals give what CL gives: single-floats, so integer and rational arrays give `single-float` arrays (complex single-float when the result is complex), *except* that CL is free to return an exact number: CCL's `(sqrt 4)` is `2` and `(sqrt -4)` is `#C(0 2)`, so on CCL an array holding perfect squares gives an array of element type `T`. The generic path simply collects what `cl:op` returns for each element and chooses the tightest array that holds all of it.
* `abs` of a complex array is a real array of the float format; `abs` and `signum` of integer arrays are `(signed-byte 64)` arrays (element type `T` when a result does not fit: `(abs most-negative-(signed-byte 64))`). `realpart` and `conjugate` of a real array copy it; `imagpart` of a real array gives zeros of the type CL returns (`(imagpart 1.5d0)` is `0.0d0`, `(imagpart 3)` is `0`). `phase` of real arrays is CL's per element. `complex` of float arrays gives complex float arrays; with rational operands CL's rule applies, so `(complex 1 0)` is `1` and the result has element type `T`. `isqrt` accepts integer arrays only and signals the type error `cl:isqrt` signals for any other element.
* `expt`: float (or complex float) base with an integer exponent array or number is computed by CL's own algorithm (repeated multiplication) per element, so `(expt x 0)` is `1` of the type of `x` and the results equal `cl:expt` exactly; float with float uses MKL's `v?Pow`; integer and rational operands use `cl:expt` per element (exact integers, rationals for negative powers; overflow beyond 64 bits gives element type `T`). Two-argument `log` of two real float operands is `(/ (log x) (log base))` like CL; a zero base, and integer or mixed operands, use `cl:log` per element, because CL special-cases them.
* **Out-of-domain real arguments give complex arrays.** `sqrt` and `log` of a negative, `asin` and `acos` outside [-1, 1], `acosh` below 1, `atanh` outside [-1, 1], and `expt` of a negative base with a non-integer power return complex numbers in CL. If any element of a real float array does, the *whole* result is a complex array of the same float format, and every element equals what `cl:op` returns for it (the elements that are real in CL get a zero imaginary part). A scan of the input decides before anything is computed. It classifies each element as certainly real, certainly complex (the result is then computed by MKL's complex kernel), or exotic (NaN, infinities, `-0.0`, and at the edges the behaviour differs between CCL and SBCL; the value is then taken from `cl:op` itself, element by element). The classification follows what `cl:op` really does on the implementation: for instance `(log -0.0d0)` is real on SBCL and complex on CCL, `(asin nan)` is real on CCL and a complex NaN on SBCL, and `(atanh 2d0)` has imaginary part `-pi/2` on CCL and `+pi/2` on SBCL. For `expt`, the pairs with a negative or NaN base are evaluated by `cl:expt`; the other pairs by MKL. Integer arrays go through `cl:op` per element, so they follow CL exactly.
* **One backend per operation and element type.** All transcendental operators use MKL VML at every array size when MKL is available (there is no size cutoff), and typed Lisp loops calling `cl:op` when it is not (`vm:*mkl-available*` NIL), so a result never depends on the array length or on what else is in the array. Exact operations (`abs` of reals, `conjugate`, `realpart`, `imagpart`, `signum`, `complex`, real `phase`) are Lisp loops. Accuracy: MKL in its high-accuracy mode is within one ulp of the correctly rounded value for real functions; complex functions differ from CL's by a few ulps (`tests/` records the maximum observed per operator and type, see "Accuracy" below). Without MKL the results are CL's, bit for bit.
* **Real `sqrt` is correctly rounded.** Real single and double `sqrt` arrays always use a Lisp loop on both implementations (SBCL: the hardware square root instruction; CCL: the C library's `sqrt` and `sqrtf`, which are correctly rounded too, called without boxing, because CCL's own `sqrt` is a boxed full call that takes 65-85 ns per element), so every element is bit-identical to `cl:sqrt`; MKL's `v?Sqrt` is never used for them (it differs from the correctly rounded value in the last bit for about 1% of doubles). Negative elements give a complex array as for the other domain operators (see above), computed by MKL's complex kernel.

#### Accuracy

Maximum difference from `cl:op` observed by the test suite on random arguments (sizes 1 to 1000), in ulps of the CL result for real arrays and in units of epsilon times the magnitude for complex arrays: real single and double ≤ 1.0 for every operator; `log` of two arguments ≤ 2; complex: ≤ 4.2 for the elementary functions (`tan`, `atan`, `tanh` the largest), `expt` of complex ≤ 7.6. The suite fails if a real result is more than 4 ulps or a complex result more than 64 units from CL's.

### Configuration

* `(vm:cutoff '+)` (likewise `'-`, `'*`, `'/`) is the number of elements below which the operator uses the Lisp loop instead of MKL, because the foreign call overhead dominates for short arrays. Defaults (measured, see [Performance](#performance)): CCL 128 for `+ - *` and 192 for `/`; SBCL 256 for `+ - *` and 64 for `/`; on SBCL complex `+ -` use MKL from 128 times the cutoff of the operator (32768 by default). Change it with `(setf (vm:cutoff '-) n)`; use 0 to always use MKL, `most-positive-fixnum` never. The cutoff only applies where MKL and the Lisp loop give bit-identical results (real `+ - * /`, complex `+ -`), so it changes speed only. Complex `*` and `/` are not bit-identical to the Lisp operators (MKL is within a few ulp), so they use MKL at every size when it is available and the Lisp loop only when it is not; a result never depends on the array length.
* `vm:*mkl-available*` is true when MKL was loaded and the VML entry points were found. Bind or set it to `nil` to force the Lisp loops. See [Locating MKL](#locating-mkl).
* The transcendental operators (`exp`, `sin`, ...) have no cutoff: with MKL available they always use it, so that an element's value does not depend on the array length. Real `sqrt` is the exception: it never uses MKL (see above).
* `vm:register-kernel` and `vm:find-kernel` give access to the table of kernels (operation, element type, array/scalar variant), for plugging in your own.



### Locating MKL

vectorized-math depends on CFFI and MKL only. It loads `libmkl_rt` with `cffi:define-foreign-library` (`libmkl_rt.so.3`, `.so.2`, `.so` on Linux; `libmkl_rt.2.dylib`/`.dylib` on macOS and `mkl_rt.3.dll`/`.2.dll`/`.dll` on Windows; macOS and Windows are untested). MKL's threading is left at its default (Intel OpenMP), which measured about twice as fast as the sequential layer on 10^6-element arrays.

1. **Default search.** With nothing configured the system's dynamic loader finds MKL: `ldconfig` (for oneAPI, a file in `/etc/ld.so.conf.d/` naming `.../mkl/latest/lib` and `.../compiler/latest/lib`), `LD_LIBRARY_PATH` (`DYLD_LIBRARY_PATH`, `PATH`) or the usual `setvars.sh`.
2. **`VM_MKL_LIBRARY`.** Set this environment variable to the full path of the library file (for example `/opt/intel/oneapi/mkl/latest/lib/libmkl_rt.so.3`) before loading vectorized-math. It initialises `vm:*mkl-library*`.
3. **`(vm:load-mkl &optional path)`** (re)loads the library from `path` (default `vm:*mkl-library*`; `nil` means the default search), checks that the required symbols are present, sets `vm:*mkl-available*` and returns it. If MKL cannot be loaded it signals one warning, returns `nil`, and the Lisp loops are used. Call it before using array operations.

When a path is given, the companion libraries `libmkl_rt` loads by bare name at first use (OpenMP runtime, core, interface, threading, and the CPU specific kernels) are loaded first from the same directory, so MKL works from any location, without `LD_LIBRARY_PATH` or `setvars.sh`. The minimal set, for the default threading layer on a CPU with AVX2 (223 MB in total), is `libmkl_rt.so.3`, `libmkl_core.so.3`, `libmkl_intel_lp64.so.3`, `libmkl_intel_thread.so.3`, `libmkl_avx2.so.3`, `libmkl_vml_avx2.so.3` and `libiomp5.so` (from `compiler/latest/lib`). Other CPUs select other kernels (`libmkl_avx512`/`libmkl_vml_avx512`, `libmkl_avx10`, `libmkl_mc3`, `libmkl_def` and their `libmkl_vml_*` partners); copy those for the CPUs you target. The script `tests/relocate-mkl.sh` copies this set to a directory, loads it through `vm:load-mkl` with `LD_LIBRARY_PATH` unset and, with `LD_DEBUG=libs`, checks that every MKL library in the process came from that directory.


### Performance

The benchmark suite is in `bench/` (`bench/run.sh TAG` runs it on both implementations, `bench/report.py` renders the tables, `bench/cutoff.lisp` measures where MKL starts to win). The complete results (every operator family, element types double, single, complex double, `(signed-byte 64)` and `T`, sizes 1 to 10^7, the operator with and without MKL, before and after the tuning) are in [`bench/results/RESULTS.md`](bench/results/RESULTS.md); the raw measurements in `bench/results/*.tsv`.

Setup: Intel Core i7-5930K (6 cores, 12 threads, AVX2), Linux, MKL 2026.2 with its default threading, SBCL 2.6.2, CCL 1.14 (with the pull requests listed under [Known Issues](#known-issues)). The machine was shared with other jobs (load average 2 to 4), every number is the best of several repetitions, and a few percent to 20% of noise is normal. "`cl:` loop" is a typed `cl:` loop written for the case (declared arrays, `(speed 3) (safety 0)`) that allocates its result array like the operator does; it is not an equivalent computation for `(signed-byte 64)` (it wraps silently where vm computes the exact integer) or `T` (plain generic arithmetic). Times are nanoseconds per element, except in the second table (nanoseconds per call).

**10^6 elements**, nanoseconds per element

| operator | element type | CCL `cl:` loop | CCL vm | CCL vm, no MKL | SBCL `cl:` loop | SBCL vm | SBCL vm, no MKL |
|---|---|---:|---:|---:|---:|---:|---:|
| `+` | double | 5.04 | 1.27 | 4.32 | 2.08 | 1.31 | 2.11 |
| `*` | single | 4.08 | 0.34 | 4.10 | 0.90 | 0.36 | 1.00 |
| `/` | double | 5.73 | 1.31 | 4.69 | 4.36 | 1.37 | 4.52 |
| `+` | cdouble | 7.31 | 3.12 | 7.65 | 4.42 | 2.88 | 4.40 |
| `+` | sb64 | 4.05 | 10.4 |  | 2.06 | 4.24 |  |
| `+` | T | 65.4 | 96.5 |  | 10.3 | 18.1 |  |
| `exp` | double | 164 | 0.85 | 164 | 7.31 | 0.93 | 9.71 |
| `log` | double | 146 | 18.7 | 144 | 11.9 | 2.74 | 14.6 |
| `sin` | single | 138 | 0.52 | 141 | 15.1 | 0.46 | 16.9 |
| `sqrt` | double | 88.6 | 26.7 |  | 6.50 | 8.21 |  |
| `sqrt` | single | 50.8 | 26.4 |  | 3.88 | 3.67 |  |
| `expt` | double | 314 | 140 | 393 | 26.8 | 20.8 | 38.3 |
| `<` | double | 11.0 | 12.6 |  | 5.30 | 6.02 |  |
| `=` | sb64 | 4.77 | 4.23 |  | 1.85 | 1.91 |  |
| `max` | double | 9.92 | 13.4 |  | 5.09 | 6.01 |  |
| `floor` | double | 203 | 43.6 |  | 7.86 | 11.5 |  |
| `round` | double | 237 | 50.4 |  | 5.30 | 9.15 |  |
| `floor` | single | 126 | 44.0 |  | 6.98 | 10.0 |  |
| `floor` | sb64 | 42.9 | 46.1 |  | 22.0 | 28.6 |  |
| `logand` | sb64 | 3.94 | 4.49 |  | 2.05 | 2.08 |  |
| `ash` | sb64 | 11.4 | 13.8 |  | 1.47 | 3.03 |  |
| `float` | sb64->double | 12.7 | 14.9 |  | 1.49 | 1.57 |  |
| `float` | double->single | 2.72 | 4.40 |  | 0.99 | 1.18 |  |

**16 elements (the cost of the call dominates)**, nanoseconds per call

| operator | element type | CCL `cl:` loop | CCL vm | CCL vm, no MKL | SBCL `cl:` loop | SBCL vm | SBCL vm, no MKL |
|---|---|---:|---:|---:|---:|---:|---:|
| `+` | double | 89.6 | 1,332 | 1,356 | 23.0 | 570 | 572 |
| `*` | single | 79.9 | 1,285 | 1,308 | 19.1 | 566 | 562 |
| `/` | double | 101 | 1,293 | 1,304 | 71.5 | 611 | 614 |
| `+` | cdouble | 114 | 1,454 | 1,473 | 34.7 | 615 | 610 |
| `+` | sb64 | 100 | 3,799 |  | 23.4 | 842 |  |
| `+` | T | 280 | 2,868 |  | 159 | 985 |  |
| `exp` | double | 2,759 | 1,096 | 3,588 | 131 | 464 | 518 |
| `log` | double | 2,427 | 1,707 | 3,768 | 204 | 631 | 730 |
| `sin` | single | 2,269 | 1,276 | 3,424 | 190 | 445 | 578 |
| `sqrt` | double | 1,475 | 1,680 |  | 104 | 508 |  |
| `sqrt` | single | 826 | 1,652 |  | 68.8 | 515 |  |
| `expt` | double | 5,159 | 2,863 | 8,004 | 438 | 1,084 | 1,344 |
| `<` | double | 95.4 | 1,895 |  | 30.1 | 648 |  |
| `=` | sb64 | 90.7 | 3,546 |  | 29.9 | 873 |  |
| `max` | double | 101 | 1,762 |  | 32.8 | 590 |  |
| `floor` | double | 3,224 | 2,778 |  | 138 | 802 |  |
| `round` | double | 3,779 | 2,941 |  | 92.5 | 751 |  |
| `floor` | single | 1,948 | 2,744 |  | 127 | 762 |  |
| `floor` | sb64 | 631 | 5,128 |  | 172 | 990 |  |
| `logand` | sb64 | 99.6 | 3,808 |  | 23.3 | 694 |  |
| `ash` | sb64 | 129 | 3,303 |  | 22.1 | 549 |  |
| `float` | sb64->double | 246 | 5,706 |  | 23.6 | 797 |  |
| `float` | double->single | 62.7 | 4,094 |  | 19.9 | 650 |  |

Reading the tables:

* **MKL pays where there is arithmetic to vectorize.** At 10^6 elements `+ * /` on double and single arrays are 1.6 to 12 times faster than the loop on both implementations, `exp` and `sin` 8 to 270 times (on CCL they are boxed full calls of 140 to 165 ns). Without MKL ("no MKL" columns) the operators are the typed loops, as fast as the `cl:` loop.
* **Operators without an MKL path** (comparisons, `max`, `min`, the integer and conversion operators, `floor`, `round`, `mod`, ...) run typed loops and are within about 2 times of a bare `cl:` loop on arrays of 4096 elements and more (usually 1.0 to 1.5; they check every element for the cases CL signals on, and keep NaN and zero semantics the loop does not), except `(signed-byte 64)` `+ - *` (2.1 to 3.3 times: the exact integer result, see below), `T` arrays and `ash` on SBCL (2.1 at 10^6); where the `cl:` loop is slow itself (CCL's `floor`, `round`, `sqrt`) they are much faster.
* **Small arrays are dominated by the call**: classifying the arguments, the shape rules, the result array, masking the float traps and finding the kernel cost about 0.55 to 1 microsecond per call on SBCL and 1.3 to 2 on CCL for float arrays (`+` on 16 doubles: 570 ns against 23 for the bare loop on SBCL, 1330 against 90 on CCL); `(signed-byte 64)` arrays and bit-array results cost up to 6 microseconds per call on CCL (1 on SBCL). Use the operators for arrays of some hundred elements and up, or for convenience.
* **Scalars cost nothing extra when the types are declared**: the compiler macros compile `(+ a b)` for known numbers exactly as `cl:+` (the disassembly is identical, `tests/bench-scalar.lisp`). Without declarations the inline test for "a number" adds about 1 ns (SBCL `+` on doubles: 6.6 against 7.7 ns per call, CCL 11.9 against 12.6; `max` 5.9 against 6.6 on SBCL). Scalars of the operators that are slow in CCL itself (`sqrt` 85 ns, `exp` 155, `floor` 158 per call) are exactly as slow with vm: they are CCL's own functions.

**Tuning** (10^6 elements, nanoseconds per element, `vm` before and after, and the `cl:` loop for reference; nothing changed any result, see below):

| operator | type | CCL before | CCL after | `cl:` loop (CCL) | SBCL before | SBCL after | `cl:` loop (SBCL) |
|---|---|---:|---:|---:|---:|---:|---:|
| `sqrt` | double | 101 | 26.7 | 88.6 | 5.83 | 8.21 | 6.50 |
| `sqrt` | single | 62.3 | 26.4 | 50.8 | 3.69 | 3.67 | 3.88 |
| `floor` | double | 297 | 43.6 | 203 | 21.6 | 11.5 | 7.86 |
| `ceiling` | double | 300 | 43.7 | 202 | 21.4 | 11.5 | 7.90 |
| `truncate` | double | 145 | 41.0 | 54.5 | 19.5 | 9.18 | 5.31 |
| `round` | double | 328 | 50.4 | 237 | 19.8 | 9.15 | 5.30 |
| `mod` | double | 321 | 42.3 | 194 | 23.4 | 9.87 | 6.21 |
| `rem` | double | 168 | 39.0 | 56.4 | 21.3 | 8.45 | 4.63 |
| `ffloor` | double | 345 | 44.7 | 250 | 21.4 | 8.88 | 5.12 |
| `floor` | single | 163 | 44.0 | 126 | 17.5 | 10.0 | 6.98 |
| `+` | sb64 | 108 | 10.4 | 4.05 | 18.6 | 4.24 | 2.06 |
| `*` | sb64 | 219 | 18.4 | 12.2 | 18.6 | 4.22 | 2.06 |
| `logand` | sb64 | 7.64 | 4.49 | 3.94 | 2.13 | 2.08 | 2.05 |
| `ash` | sb64 | 20.5 | 13.8 | 11.4 | 4.04 | 3.03 | 1.47 |
| `float` | double->single | 13.3 | 4.40 | 2.72 | 1.23 | 1.18 | 0.99 |


* **Non-consing float kernels for `floor ceiling truncate round ffloor fceiling ftruncate fround mod rem`.** Before, the typed kernels called `cl:floor` (and friends) as a full call on every element, which boxes the floats and allocates the results: about 20 ns per element on SBCL and 145 to 345 on CCL. Now SBCL's kernels contain the call open-coded (the compiler expands the CL function for float arguments with its own transforms, which is also the code the function's full call runs), about 2 times faster. CCL's `floor` and relatives are not inlined by its compiler and CCL has no inline float-to-integer truncation, so the CCL kernels repeat CCL's own algorithm (`level-0/l0-numbers.lisp`: `x/y`, truncate, remainder `x - float(q)*y` with the same two roundings, then the adjustment of `floor`, `ceiling` or `round`, ties to even) with inline double and single arithmetic, for quotients below 2^52 (the integer part is read from the bits of `|x/y| + 2^52`); every other element is passed to `cl:floor` itself. The result is `eql` to CL's, signed zeros in remainders included. This is a re-implementation of an implementation's algorithm, so it is checked three ways: at load time each non-consing kernel is compared with the plain full-call kernel on a fixed corpus and registered only if all results are identical (`vm.impl::*fast-division-failures*` lists the kernels that were not, and a warning says so; they then run the plain kernel); the test suite repeats this through the public operators for 4000 pairs per type and operator; and `tests/diff-kernels.lisp` compared them with CL's functions on 187 million pairs per implementation (random bit patterns, exact multiples, exact ties, neighbours of both, zeros, the quotient range up to 2^63) with no difference.
* **`sqrt` on CCL.** CCL has no inline `sqrtsd` and its `sqrt` is a boxed full call (65 to 90 ns per element). The kernel now calls the C library's `sqrt` and `sqrtf` through CFFI without boxing (about 15 ns per element, 26 for the whole operator including the domain scan and the result array). They are correctly rounded as IEEE 754 requires (and as CL's `sqrt` is on both implementations), so the results are those of `cl:sqrt`: `tests/diff-kernels.lisp` compared 40 million non-negative arguments per implementation (every exponent, subnormals, zeros, infinity, NaN) with no difference. MKL's `v?Sqrt` is still never used. (SBCL, which compiles `sqrt` to the instruction, needs 6.5 ns per element for the whole operator.)
* **`(signed-byte 64)` arrays: `+ - *`.** The range of an element type alone can never promise that a sum does not overflow, so such results used to take the generic path (boxed integers, then narrowing: 20 ns per element on SBCL, 108 on CCL). Now one pass over each operand finds the minimum and maximum actually present; when the sum, difference or product of those ranges fits, the typed loop is used (4 and 10 ns), otherwise the generic path as before. Results (and their element type: `(signed-byte 64)` when every element fits, else `T`) are unchanged. What remains over a `cl:` loop that wraps silently is the two scans and the exactness.
* **Smaller changes.** `float` of a double array to single no longer makes a full call per element on CCL (the prototype is a literal inside the kernel; 13 to 4.4 ns); the `(signed-byte 64)` pair kernels (`logand` and the other bitwise operators, `ash`, `logbitp`, `logtest`) are compiled with `speed 3` (CCL open-codes more: 7.6 to 4.5 ns), `ash` of an array by one scalar tests the range of the elements with two comparisons instead of `integer-length` (SBCL 4.0 to 3.0); the kernel table is a hash on the operator name and a short list instead of a hash on a freshly consed key (about 25% of a call on small arrays).
* **Not tuned.** `T` arrays (1.5 to 4 times the bare generic loop: every element is checked to be a number, twice); on CCL the scan that decides whether `sqrt`, `log`, `asin`, `acos`, `acosh` or `atanh` of a real array has complex results costs about 10 ns per element (SBCL: 1 to 2); `floor` of `(signed-byte 64)` arrays on CCL is CCL's own `floor` (46 ns, as fast as the `cl:` loop).

**MKL cutoffs.** Below a size, the Lisp loop is faster than the MKL call; results are bit-identical on both sides for real `+ - * /` and complex `+ -`, so a cutoff changes speed only. `bench/cutoff.lisp` times the two kernels directly (no operator overhead) and prints the crossover per operation and type; the defaults, which `(vm:cutoff op)` returns, are rounded up from those crossovers (`bench/results/cutoff-*.txt`):

| | `+ - *` | `/` | complex `+ -` | crossovers measured |
|---|---:|---:|---:|---|
| CCL | 128 | 192 | 128 | 96 to 128 (`/` double: 192) for every type |
| SBCL | 256 | 64 | 256 * 128 = 32768 | single 96, double 192 to 384; `/`: 32 to 48; complex `+ -`: 24576 to 49152 |

(On SBCL the Lisp loops for complex `+ -` stay faster than MKL's memory-bound complex add up to about 40000 elements, so for these two operators the cutoff is `(vm:cutoff op)` times 128; `(setf (vm:cutoff op) 0)` still means MKL for every size.) Complex `*` and `/` always use MKL when it is available: MKL and the Lisp loops differ in the last bits there, and a result must not depend on the array length. The previous defaults were 256 for all four operators on CCL, and 1024, 2048, 2048 and 256 for `+ - * /` on SBCL.

**Arrays of more than 2^30 elements.** MKL takes its length as a 32-bit integer, so the MKL kernels pass at most 2^30 elements per call and loop (`+mkl-chunk+`); every kernel's indices and counts are `fixnum`s, and no other foreign call takes a count. `tests/large-arrays.lisp` (not part of the suite: about 21 GB at its peak) runs `+ - * /`, `max`, `<`, `floor`, `sqrt`, `exp` and `:into` on arrays of 2^30 + 1000 single floats and checks the elements around the chunk boundary and a sample across the arrays. Both pass on SBCL 2.6.2 (58 s, 21 GB resident) and CCL 1.14 (171 s).

### Compatibility notes

[Comparison with R and NumPy](docs/comparison-with-r-and-numpy.md) goes through every operator and notes where its behaviour differs from R's and NumPy's.

**What changes in a package that shadows the operators.** Scalar code is unchanged (values, types, conditions; with declared argument types the compiler macros compile `(+ a b)` to the same machine code as `cl:+`, which `tests/bench-scalar.lisp` checks by disassembly on both implementations). What does change:

* **The `*` wildcard and the type names** `float`, `rational`, `complex` and `mod` (see "The `*` wildcard" and the paragraph on types above). Both work through hooks in the implementation's type-specifier parser, installed at load time and checked by a self-test that signals an error if a future implementation version breaks them.
* **The REPL variables** `+ ++ +++ - * ** *** / // ///`: in a package that shadows `+ - * /`, the symbols `+ - * /` are vm's, and the REPL's history belongs to the CL symbols. vectorized-math makes `vm:+ vm:- vm:* vm:/` mean the same variables as `cl:+ cl:- cl:* cl:/`; how depends on the implementation, and `vm:*repl-variable-mode*` tells you which way was chosen when vm was loaded (see "How the REPL variables work" under Known Issues).
* **A bit array is a true value**, so `(if (< a b) ...)` with an array argument is always true; test the result with `(every ...)` or `(find 1 ...)`.
* **`LOOP`: the simple type specifier `float` after `sum`/`collect` is not recognised** where `float` is shadowed: `(loop for i from 1 to 4 sum (float i) float)` signals a `simple-program-error` ("unknown keyword FLOAT") on both implementations, because their `LOOP` recognises `float` (and, on CCL, a few other CL type symbols) by symbol identity. This is not caused by vectorized-math (a package that only `(:shadow #:float)`s does the same); `of-type float` and `of-type cl:float` work, and so does `(loop ... sum x of-type float)`. A fix for CCL is submitted as [Clozure/ccl#745](https://github.com/Clozure/ccl/pull/745), and a corresponding SBCL report is being prepared; until then write `of-type cl:float` or leave the type out.

**ANSI conformance.** The whole ANSI-TEST suite (21,878 tests) was run in a plain image and in an image where `vm:shadowed-symbols` is shadowed in the test package, with five rules that only change which symbol or object a test-internal list contains (none hides a vectorized-math result; they are in `ansi-test-runner/run-numbers-vm.lisp` of the Lisp-Stat workspace). On CCL, built with the pull requests listed under [Known Issues](#known-issues), the plain and the shadowed image fail the same 63 tests. On SBCL 2.6.2 the two sets are the same apart from the `LOOP` tests above (`LOOP.6.31 LOOP.6.33 LOOP.8.7 LOOP.10.8 LOOP.10.77 LOOP.10.91`, which fail only with vm) and four tests that pass with vm and fail in the plain image because SBCL constant-folds `cl:plusp` and not `vm:plusp`; the SBCL comparison was made with an earlier version of the library and has not been re-run since. The `numbers` category (1443 tests) fails 8 tests on CCL in both images, `EXP.ERROR.8-11` and `EXPT.ERROR.8-11`, which are CCL's own. Without the rules, the extra failures (74 on CCL) are tests that assume things that are no longer true in a shadowing package: a `(make-array 10)` in their list of "things that are not numbers" (vm accepts numeric arrays), lists of CL symbols built by `set-difference` that now contain symbols that are not the CL functions, and the identity of class names. `(boundp '+)` used to be one of them; on CCL the names are now aliases of the REPL variables, so `CL-VARIABLE-SYMBOLS.1` passes without the rules, while SBCL still needs that rule.

Results of the typed kernels do not depend on the path taken: the tests compare the typed kernels with `cl:op` per element (signed zeros included), the typed and generic paths on the same data, and MKL against the Lisp loops on both sides of every cutoff (bit-identical where the cutoff applies).


<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to be learn, inspire, and create. Any contributions you make are **greatly appreciated**. Please see [CONTRIBUTING](CONTRIBUTING.md) for details on the code of conduct, and the process for submitting pull requests.

<!-- LICENSE -->
## License

Distributed under the MS-PL License. See [`LICENSE`](LICENSE) for more information.

<!-- CONTACT -->
## Contact

Project Link: [https://github.com/Lisp-Stat/vectorized-math](https://github.com/Lisp-Stat/vectorized-math)

## Known Issues

### How the REPL variables work (`vm:*repl-variable-mode*`)

The REPL keeps its history in the variables `+ ++ +++ - * ** *** / // ///` of the `CL` package, and Swank and Jupyter set them from a thread other than the one that reads them. A package that shadows `+ - * /` must still see those variables under its own names. Loading vectorized-math chooses one of three ways, recorded in `vm:*repl-variable-mode*`:

* **`:downstream`: CCL on x86-64 (released 1.14 and the like).** `vm:+ vm:- vm:* vm:/` become aliases of the CL variables: the two names of a pair share one global value and one set of dynamic bindings, so `boundp`, `symbol-value`, `set`, `makunbound`, `progv`, `special` declarations, `eval` and a global `setf` from another thread (what Swank does) see the same variable through either name. To get this, vectorized-math redefines six CCL functions (`%sym-value`, `%set-sym-value`, `%sym-global-value`, `%set-sym-global-value`, `%symbol-binding-address` and `makunbound`) and two compiler functions (`x862-symbol-value-cell`, `nx1-note-vcell-ref`) when it loads, and keeps the originals. The cost is about 1 ns per call of an out-of-line `symbol-value`, `boundp` or `set`; compiled special-variable access is unchanged. The installation is checked by a self-test; if the test fails everything is removed again, one warning is signalled and the third way is used.
* **`:native`: a CCL that provides the alias function itself** (`ccl::%alias-special`). vectorized-math calls it and installs nothing.
* **`:symbol-macro`: SBCL, other implementations, CCL on platforms other than x86-64, and the fallback of the other two.** `vm:+` and the others are global symbol macros for the CL variables: evaluating them, `setq` of them and binding the CL variable all work, but `(boundp 'vm:*)` is false and `(symbol-value 'vm:*)` is an error, because the symbol is not a variable. A symbol cannot be both a global symbol macro and a special variable, so the ways exclude each other.

To use the third way where an alias would be possible, set the environment variable `VM_REPL_VARIABLES=symbol-macro` before loading vectorized-math. (`VM_REPL_VARIABLES=fail-self-test` makes the self-test fail, to exercise the fallback.)

An alias only takes effect in code compiled after it exists. vectorized-math creates the aliases at the very start of loading, before any of its own files and before user code, so this matters only when vm is loaded into an image that already holds compiled code treating `vm:+` (or `vm:-`, `vm:*`, `vm:/`) as a special variable of its own, for example after a `defvar` of it: that code keeps reading and setting the variable's old value cell, which is no longer used (global reads signal unbound-variable, global `setq`s are lost; dynamic bindings stay shared). Code compiled where the names were symbol macros expands to the CL variables, so it works on every path, and a fasl compiled on one path loads on another.

On `:downstream` the aliases are created once, at load time, under vm's own lock; CCL's lock for special-variable binding indices is not reachable from outside CCL, so they are not serialised with CCL's own use of it. Thread-local binding indices are shared between the two names, and the old index of the alias stays in CCL's index-to-symbol table, which only debugging aids read.

vectorized-math would rather not redefine CCL functions. The change it installs is proposed for CCL itself (see the next section); with it, `:native` is used and nothing is redefined.

### Proposed CCL change: special variable aliases

*Status: a branch with tests, to be discussed with the CCL maintainers; no issue or pull request has been opened yet.*

- Branch: [Symbolics/ccl `symbol-value-alias`](https://github.com/Symbolics/ccl/tree/symbol-value-alias)
- Tests: [Symbolics/ccl-tests `symbol-value-alias`](https://github.com/Symbolics/ccl-tests/tree/symbol-value-alias)

**The problem.** A library that shadows `+`, `-`, `*` and `/` breaks the REPL history in any package that uses it. The reader gives `vm:*`, while the listener and Swank set `cl:*`. A global symbol macro, `(define-symbol-macro vm:* cl:*)`, covers plain evaluation. It does not cover `boundp`, `symbol-value`, `set`, `progv`, `special` declarations, or Swank, whose `repl-eval` does a plain global `setq` of `* ** *** + ++ +++ / // ///` in a worker thread that has no binding.

Sharing the binding index (copying `cl:*`'s into `vm:*`) makes dynamic bindings work. Global values still live in two separate value cells, though. After `(setf cl:* 123)` in a thread with no binding, `vm:*` is unbound in that thread.

**The change.** A small patch lets one symbol be an alias of another special variable:

```lisp
(ccl::%alias-special 'vm:* 'cl:*)
```

After that call, both names refer to the same variable: the global value and the thread-local bindings, for compiled code and through `symbol-value`, `set`, `boundp`, `makunbound`, `progv` and `symbol-value-in-process`. It is all Lisp, with no kernel, constant or vinsn changes:

- The alias is marked with the existing, unused `$sym_vbit_indirect` bit, and a weak table maps it to its target. The alias gets the target's binding index.
- `x862-symbol-value-cell` and `nx1-note-vcell-ref` substitute the target for an alias. Compiled code therefore references the target directly, and code that doesn't use aliases compiles to the same machine code as before (the disassemblies were compared).
- `%sym-value`, `%set-sym-value`, `%sym-global-value`, `%set-sym-global-value`, `%symbol-binding-address` and `makunbound` resolve an alias. That adds about 1 ns to an out-of-line `symbol-value`, `boundp` or `set`.

**The limitation.** Code compiled before the alias exists still uses the alias's own value cell, so its global reads and writes miss the target (dynamic bindings are still shared). A library creates its aliases when it loads, before any code that uses them, so in practice this doesn't come up. The docstring says so.

**Testing**, on linuxx8664:

- a real Swank session over its socket, with history correct and no wrapper;
- threads doing global `setq`s through both names;
- `save-application` and restart;
- compile-file, and loading the fasl into an image without the alias;
- the ccl-tests suite and the ANSI tests, with the same results as master.

Only the x86 back end is covered. PPC, ARM and ARM64 would need the same two-line hook in `ppc2-symbol-value-cell`, `arm2-symbol-value-cell` and `arm642-symbol-value-cell`.

Until something like this is in CCL, vectorized-math installs the same change itself at load time, by redefining those functions (the `:downstream` mode above).

**Open questions for the CCL maintainers:**

1. Is this something they would consider merging, as an internal function or with an exported name?
2. Is the flag-plus-table approach acceptable, or would they prefer a different mechanism (for example a forwarding marker in the value cell, which would also cover code compiled before the alias, at the cost of a check on every global read)?

### Complex double-float arrays go wrong through MKL on released CCL

On x86-64 and ARM64, Clozure CL stores a `(simple-array (complex double-float) (*))` with a 64-bit pad before the first element, and `ccl:with-pointer-to-ivector` returns the address of the pad rather than the address of the first element. Vectorized-math deliberately does not work around this: the fix belongs in CCL itself ([Clozure/ccl#737](https://github.com/Clozure/ccl/pull/737)).

As a consequence, on CCL 1.14 and earlier, operations on `(complex double-float)` arrays that go through MKL (that is, at or above the size cutoff, with MKL available) give wrong results. Build CCL with that fix to avoid this. SBCL is not affected, and neither are the other element types (single and double float, complex single float, integers, `T`).

The symptom is that the last `double-float` of every complex double result (the imaginary part of the last element) is not written: `tests/` fails `mkl-and-lisp-bitwise-identical` for `(complex double-float)`. ASDF names its fasl cache after the implementation version only (`ccl-1.14-f99-linux-x64`), so builds of CCL with and without that fix share it, and (the probable cause: `ccl:with-pointer-to-ivector` is a macro, expanded when `ffi.lisp` is compiled) fasls compiled by a build without the fix keep the wrong pointer when a build with the fix loads them. If you switch between CCL builds, give each build its own cache (for example by setting `XDG_CACHE_HOME` per build in the script that starts it), or delete `~/.cache/common-lisp/ccl-1.14-f99-linux-x64/` and reload.

Three more CCL bugs affect the results of vectorized-math on released CCL; the library no longer works around them, so on CCL 1.14 and earlier the corresponding results differ from SBCL's and from the specification: comparing a float infinity with a bignum or a ratio beyond the double range (`(< inf (expt 10 400))` is true; [Clozure/ccl#738](https://github.com/Clozure/ccl/pull/738)), comparing a NaN with `=`, `/=`, `<`, `<=`, `>`, `>=`, `zerop` (a NaN counts as equal to and as less than other numbers, and `(/= x x)` is compiled to false; [Clozure/ccl#729](https://github.com/Clozure/ccl/pull/729) fixes `<`, `<=`, `>`, `>=`, and [Clozure/ccl#739](https://github.com/Clozure/ccl/pull/739) the rest), and the sign of the imaginary part of `asin` and `acos` of real arguments above about 1.8e16 (`(asin 1d300)`; [Clozure/ccl#740](https://github.com/Clozure/ccl/pull/740)). All three are fixed in a CCL built with those pull requests. The test suite's infinity and `asin`/`acos` cases fail on a CCL without the fixes; the NaN handling of arrays does not depend on them.

### Floating-point conversion bugs in released CCL

These are bugs in CCL's own functions on CCL 1.14 and earlier; vectorized-math does not work around them (array elements are what `cl:op` returns), so on those versions the arrays show the same wrong values. SBCL gives the correct results: `(float ratio 1f0)` is not correctly rounded for about a quarter of ratios with large numerator and denominator (`(float 6069/26411177 1f0)`); `scale-float` of a denormal float by a positive scale gives zero or infinity instead of the number (`(scale-float least-positive-single-float 140)`), and loses the sign of a zero produced by underflow (`(scale-float -1f0 -200)` is `0.0`, not `-0.0`); `scale-float` signals a `type-error` for a scale that is an integer but not a fixnum; and `(float n 1d0)` for an integer that rounds to infinity but is below 2^1024 returns infinity without signalling `floating-point-overflow` (likewise for singles, `(float (1- (expt 2 128)) 1f0)`; `(float (expt 2 1024) 1d0)` does signal). All five are fixed in a CCL built with these pull requests: [Clozure/ccl#742](https://github.com/Clozure/ccl/pull/742) (rounding a ratio), [#743](https://github.com/Clozure/ccl/pull/743) (`scale-float`) and [#744](https://github.com/Clozure/ccl/pull/744) (integer overflow to infinity).

### CL implementation quirks the transcendental operators follow

Because array elements must equal what `cl:op` returns, the differences between implementations show through (all are in the implementation's own scalar functions): CCL's `(expt -8d0 2d0)` is a complex number (`#C(63.99999999999998d0 -1.6d-14)`) while SBCL's is `64.0d0`, so a negative base with *any* float power gives a complex array on CCL but only with a non-integer power on SBCL; CCL's `(sqrt 4)` is the integer `2`; and `(atanh 2d0)` has imaginary part `-pi/2` on CCL and `+pi/2` on SBCL (an open question about which is right, reported as the issue [Clozure/ccl#741](https://github.com/Clozure/ccl/issues/741)). (The NaN tests in this library do not use comparisons, and the comparison operators of arrays do not rely on them; see "Comparisons, predicates, `max` and `min`".)

On CCL 1.14 and earlier, `(signum -0.0)` is `0.0` where the CLHS gives `-0.0`, so `vm:signum` of an array holding `-0.0` gives `0.0` there. [Clozure/ccl#752](https://github.com/Clozure/ccl/pull/752) fixes it.

<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://badgen.net/github/contributors/Lisp-Stat/vectorized-math
[contributors-url]: https://github.com/Lisp-Stat/vectorized-math/graphs/contributors
[forks-shield]: https://badgen.net/github/forks/Lisp-Stat/vectorized-math
[forks-url]: https://github.com/Lisp-Stat/vectorized-math/network/members
[stars-shield]: https://badgen.net/github/stars/Lisp-Stat/vectorized-math
[stars-url]: https://github.com/Lisp-Stat/vectorized-math/stargazers
[issues-shield]: https://badgen.net/github/open-issues/Lisp-Stat/vectorized-math
[issues-url]: https://github.com/Lisp-Stat/vectorized-math/issues
[license-shield]: https://badgen.net/badge/license/MS-PL/blue
[license-url]: https://github.com/Lisp-Stat/vectorized-math/blob/master/LICENSE
[linkedin-shield]: https://badgen.net/badge/icon/LinkedIn?icon=linkedin&label
[linkedin-url]: https://www.linkedin.com/company/symbolics/
