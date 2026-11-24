# vectorized-math operators compared with R and Python

As of 24 November 2026.

vm matches R on shapes, recycling operands in column-major order, and gives IEEE results for float arrays like both. Everywhere else it follows Common Lisp: integer arithmetic stays exact, result types follow CL contagion, and out-of-domain real arguments give complex arrays instead of NaN. Two traps catch most people: the two-argument rounding functions take a divisor, not a digit count, and scalars behave exactly as CL, so they can signal where arrays return Inf or NaN.

## At a glance

| Group | vm | R | NumPy | Biggest differences |
| --- | --- | --- | --- | --- |
| Arithmetic | `+ - * / 1+ 1- incf decf` | `+ - * /` | `np.add`, `np.subtract`, `np.multiply`, `np.divide`, `+=` | Integer division gives exact ratios. Integer overflow gives bignums, where R gives `NA` and NumPy wraps. `incf` rebinds instead of mutating. |
| Exponentials, logs, powers, roots | `exp expt log sqrt isqrt` | `exp`, `^`, `log`, `sqrt` | `np.exp`, `np.power`, `np.log`, `np.sqrt`, `math.isqrt` | Out-of-domain input gives a complex array, not NaN. Integer arrays give single floats. `(vm:expt #(2 3) -1)` is exact; NumPy raises. |
| Trigonometric, hyperbolic, complex parts | `sin` … `atanh`, `cis`, `abs signum conjugate phase realpart imagpart complex` | `sin` … `atanh`, `atan2`, `Mod`, `sign`, `Conj`, `Arg`, `Re`, `Im` | `np.sin` … `np.arctanh`, `np.arctan2`, `np.abs`, `np.sign`, `np.angle` | Complex results out of domain. The `atanh(2)` branch cut differs between CCL and SBCL. NumPy 1.26 `sign` of a complex is the sign of its real part. |
| Comparisons, predicates, `max`, `min` | `= /= < > <= >= zerop plusp minusp evenp oddp max min` | `== != < >`, `pmax`, `pmin` | `np.equal` …, `np.maximum`, `np.fmax` | Bit arrays. Integers and floats compare exactly. NaN gives 0, where R gives `NA`. `max` of one array is not a reduction. |
| Division and rounding | `floor ceiling truncate round`, F family, `mod rem gcd lcm` | `floor`, `ceiling`, `trunc`, `round`, `%/%`, `%%` | `np.floor` …, `np.divmod`, `np.fmod`, `np.gcd`, `np.lcm` | Two result arrays. The second argument is a divisor, not decimal digits. NaN, Inf and zero divisors signal. |
| Integer and bitwise | `logand` … `logorc2`, `lognot logcount integer-length ash logbitp logtest` | `bitwAnd` …, `bitwShiftL`, `bitwShiftR` (32-bit) | `np.bitwise_and` …, `np.left_shift`, `np.right_shift` (int64) | Unbounded two's complement. `ash` takes a signed count and never wraps. Negatives count differently from Python. |
| Conversions | `float rational rationalize numerator denominator scale-float float-sign` | `as.double`, `sign` | `astype`, `np.ldexp`, `np.copysign`, `fractions.Fraction` | `float` defaults to single floats. `float-sign` gives ±1.0 for zeros and takes the sign first. Exact rationals have no R or NumPy counterpart. |

## Rules that apply to every operator

vm follows R for shapes and output arguments, and CL for numbers, result types and exact integers; NumPy broadcasts instead of recycling, wraps integers and warns on floating-point errors.

| Topic | vm | R | NumPy |
| --- | --- | --- | --- |
| Vector lengths 6 and 2 | `(vm:+ #(1d0 .. 6d0) #(10d0 20d0))` recycles → `#(11 22 13 24 15 26)` | `c(1:6)+c(10,20)` same | `ValueError: operands could not be broadcast together with shapes (6,) (2,)` |
| Lengths 5 and 2 (not a multiple) | Recycles, signals `vm:recycling-warning` (muffle to continue) | Recycles, warning "longer object length is not a multiple of shorter object length" | `ValueError` (no recycling; only length 1 or equal) |
| Matrix (2x3) + vector | Vector recycled in column-major order: `#2A((1 2 3)(4 5 6)) + #(10 20 30)` → `#2A((11 32 23)(24 15 36))`; vector of 7 → `vm:non-conformable-arrays` | `m + c(10,20,30)` same column-major result; length 7: warning plus error "dims \[product 6\] do not match the length of object \[7\]" | Row-major, trailing-axis rule: `+ [10,20,30]` → `[[11,22,33],[14,25,36]]`; `+ [10,20]` → `ValueError`; `+ [[10],[20]]` works |
| Two arrays, different dims | `vm:non-conformable-arrays` for (2 3)+(3 2) and (2 2 2)+(2 2) | "non-conformable arrays" for both | `ValueError` for (2,3)+(3,2); (2,2,2)+(2,2) broadcasts |
| Zero-length operand | `(vm:+ (d) #(1d0 2d0 3d0))` → `#()`, no error | `numeric(0)+c(1,2,3)` → `numeric(0)` | `zeros(0)+[1,2,3]` → `ValueError`; `zeros(0)+1` and `zeros(0)+zeros(1)` → empty |
| Integer ÷ integer | `(vm:/ #(5 6) #(2 3))` → `#(5/2 2)`, a `T` array of rationals; scalar `(vm:/ 1 3)` → `1/3` | `5L/2L` → `2.5` (double); `1/3` → `0.3333333` | `[5,6]/2` → float64; `np.int64(1)/np.int64(3)` → `0.333...`; Python `1/3` float; `//` floors |
| Result type of mixed operands | CL contagion: `(signed-byte 64)` array + 1 → `(signed-byte 64)` array; + `1.5f0` → single-float array; single array + double → double; integer array + `#C(1d0 1d0)` → `(complex double-float)` array | integer+double → double; `TRUE+TRUE` → integer 2; no single or 64-bit integer types | By dtype: int32+1 → int32; int32+1.5 → float64; float32 array + Python float → float32 (value-based casting in 1.26); float32+float64 array → float64; int8+200 → int16; int64+float32 → float64 |
| Integer overflow | Exact: a `(signed-byte 64)` array holding 9223372036854775807, plus 1, → `#(9223372036854775808)` in a `T` array of bignums; scalar `(vm:+ 9223372036854775807 1)` also exact | `.Machine$integer.max + 1L` → `NA` with warning "NAs produced by integer overflow"; double arithmetic goes on to 2147483648 | Arrays wrap silently: int64 max + 1 → `-9223372036854775808`; int8 100+100 → -56; `np.int64` scalars wrap with `RuntimeWarning`; Python `2**100` exact |
| Huge integer data | `#(100000000000000000000 1)` (`T`) `* 2` → exact | not representable as integer (becomes double) | `np.array([2**100,1])` becomes `object` dtype (exact, slow); `2**63` → uint64; `2**64` → object |
| Missing values | No NA; NaN only. A non-number element is a `type-error` | `NA` (logical, integer, double) and `NaN`; `NA > 1` → `NA`; `pmax` propagates NA unless `na.rm` | No NA; `nan` only; `np.maximum` propagates nan, `np.fmax` ignores it |
| Float array division by zero | `(vm:/ #(1d0 -1d0 0d0) 0d0)` → `Inf -Inf NaN`, no condition; also with an integer divisor `0` | `1/0` → `Inf`, `0/0` → `NaN`, silent | Same values, plus `RuntimeWarning` ("divide by zero", "invalid value"); `np.float64(1)/0` also warns |
| Scalar division by zero, overflow | CL: `(vm:/ 1d0 0d0)` → `division-by-zero`; `(vm:* 1d300 1d300)` → `floating-point-overflow`; `(vm:/ 1 0)` and `(vm:/ #(1 2) 0)` (integers) → `division-by-zero`; array `(vm:* (d 1d300) 1d300)` → Inf | Always `Inf`/`NaN`, no warning; `5L %/% 0L` → `NA` | Python `1.0/0.0` → `ZeroDivisionError`; `1/0` → `ZeroDivisionError`; `math.exp(1000)` → `OverflowError`; int arrays `//0` → 0 with warning |
| Comparing with NaN | For `n = #(NaN Inf)`: `(vm:= n n)` → `#*01`; `(vm:/= n n)` → `#*10`; `(vm:max n 0d0)` keeps the NaN | `NaN > 1` → `NA` | `nan == nan` → `False`; `maximum` → nan |
| Real argument out of domain | `(vm:sqrt #(-1d0 4d0))` → `#(#C(0 1) #C(2 0))`: the whole array becomes `(complex double-float)`; same for `log`, `asin`, `expt` with fractional exponent; scalar `(vm:sqrt -1d0)` → `#C(0.0d0 1.0d0)` | `sqrt(c(-1,4))` → `NaN 2` with warning "NaNs produced"; `sqrt(as.complex(...))` → complex; `(-8)^(1/3)` → `NaN` | `np.sqrt([-1.,4])` → `[nan, 2.]` + `RuntimeWarning`; `np.emath.sqrt` → complex array; `math.sqrt(-1)` → `ValueError`; `cmath.sqrt(-1)` → `1j`; Python `(-8)**(1/3)` → complex |
| Logical result | `(vm:< #(1d0 2d0 3d0) 2d0)` → bit array `#*100` (never T/NIL); numbers give `T`/`NIL` | logical vector `TRUE FALSE FALSE` (with `NA`) | `bool` array |
| Truth in `if` | `(if (vm:< #(5d0 6d0) 1d0) :true :false)` → `:true` (a bit array is a non-NIL object) | `if (c(5,6)<1)` → error "the condition has length > 1"; `if (NA)` → error | `bool(a<1)` for 2 elements → `ValueError`; for 1 element works |
| Output argument | `:into out` (last two arguments). Result recycled or truncated to `out`'s size with `vm:recycling-warning`; widening allowed (int64 into double); narrowing → `vm:output-type-error`; `out` may be an operand | None: `y <- x+1` always makes a new vector; `out[] <- x+1` coerces by assignment | `out=` needs the same shape (`ValueError` for size 3 or 4 vs 2); `casting='same_kind'`: float64 into float32 allowed, into int64 → `UFuncTypeError`; `casting='unsafe'` overrides |
| `:into` with out-of-domain input | `(vm:sqrt #(-1d0 4d0) :into <double array>)` → `vm:output-type-error`; into a complex array works | not applicable | `np.sqrt([-1.,4], out=zeros(2,complex))` → `nan+0j` (computes in real) |
| Scalars | `(vm:/ 1 3)` → `1/3`, `(vm:+ 1/3 1/6)` → `1/2`, `(vm:sqrt 4)` → `2` on CCL and `2.0` on SBCL (CL leaves it open), `(vm:sqrt 2)` → single `1.4142135`, `(vm:sqrt -4)` → `#C(0 2)`, `(vm:expt 2 -1)` → `1/2`, `(vm:expt 2 100)` exact | Always double (or integer/complex): `1/3` → `0.3333333`, `sqrt(4L)` → `2` (double), `2L^2L` → double | `np.sqrt(4)` → float64 `2.0`; `np.float64(1)/3` → float64; `a**-1` for an int array → `ValueError` |
| Non-numeric elements | `#(1 2.5d0 3/2)` (type `T`) works elementwise: `+ 1` → `#(2 3.5d0 5/2)`; strings, symbols, lists → `type-error` expected `NUMBER` | `c("a","b")+1` → error "non-numeric argument to binary operator" | `dtype=object` works (`+ 'x'` on strings); `<U1` + int → `UFuncTypeError`; `[1,2.5,'a']` silently becomes strings |

In this report `(d …)` is a double-float array and `(i64 …)` a `(signed-byte 64)` array; a literal `#(…)` is an array of element type `T`.

- Column- vs row-major: `#2A((1 2 3)(4 5 6)) + #(10 20)` adds 10 and 20 alternately down the columns, giving `#2A((11 12 13)(24 25 26))`, as R does. NumPy rejects the length-2 vector; its length-3 vector runs along rows.
- Rank mismatch: vm and R require equal dims for two arrays of rank 2 or more. NumPy aligns trailing dims, so `(2,2,2)+(2,2)` works there and errors in vm and R.
- Integer arrays: vm returns `(signed-byte 64)` when every result fits, else a `T` array of bignums, so the element type can change between calls. Division always gives `T` rationals.
- Scalar vs array: `(vm:/ 1d0 0d0)` signals, but `(vm:/ #(1d0) 0d0)` returns Inf. Integer division by zero signals `division-by-zero` for scalars and arrays alike: `(vm:/ 1 0)`, `(vm:/ #(1 2) 0)`.
- Single floats: vm keeps `single-float` arrays single. NumPy 1.26 keeps float32 with a Python-float scalar but widens with a float64 array or a Python float outside float32 range.

## Arithmetic

vm keeps integer arithmetic exact (ratios, bignums, `division-by-zero`) where R and NumPy give doubles, `NA` or wrap-around, and `incf`/`decf` rebind the place instead of mutating the array as NumPy `+=` does.

| vm | R | NumPy | Differences |
| --- | --- | --- | --- |
| `vm:+ vm:*` (n-ary) | `+`, `*`; `sum()`, `prod()` | `np.add`, `np.multiply`; `.reduce` | `(vm:+)` → `0`, `(vm:*)` → `1`, `(vm:+ #(1 2) #(3 4) #(5 6))` → `#(9 12)`. R's `` `+`(1,2,3) `` is an error ("needs one or two arguments"); `np.add(1,2,3)` is a TypeError (the third argument is `out`). Use `sum` or `np.add.reduce` for folds. |
| `vm:- vm:/` with one argument | `-x`, `1/x` | `np.negative`, `1/x` | `(vm:- #(1 2))` → `#(-1 -2)`; `(vm:/ #(1 2))` → `#(1 1/2)` (exact ratios, array type `T`). R `1/c(1,2)` → `1 0.5`; NumPy `1/np.array([1,2])` → float64 `[1., .5]`. |
| `vm:/` on integers | `/`, `%/%` | `np.true_divide`, `/` | `(vm:/ #(1 2) 3)` → `#(1/3 2/3)`; `(vm:/ (i64 4 6) (i64 2 3))` → `T` array `#(2 2)`, not an integer array. R `c(1L,2L)/3L` and NumPy `/` always give doubles (`0.333 0.667`). |
| `vm:/` by zero | `/` | `/` with warnings | Integer arrays or scalars: `division-by-zero` signalled (`(vm:/ (i64 1 2) 0)`). Float arrays: `(vm:/ (d 1 -1 0) 0)` → `#(Inf -Inf NaN)`, no signal. R `1L/0L` → `Inf`, `c(1,-1,0)/0` → `Inf -Inf NaN`; NumPy the same values plus RuntimeWarning. Scalars: vm `(vm:/ 1d0 0d0)` signals; Python `1.0/0.0` raises ZeroDivisionError; `np.float64(1)/0` → `inf` with a warning. |
| `vm:+ vm:- vm:* vm:1+ vm:1-` on integers | `+ - *` | `+ - *`, `x + 1` | `(vm:+ (i64 9223372036854775807) 1)` → bignum `#(9223372036854775808)` (the array widens to `T`). R integer overflow gives `NA` with a warning (`.Machine$integer.max + 1L`); NumPy int64 wraps to `-9223372036854775808` silently. Python scalar ints are exact like vm. |
| `vm:+` … on floats | `+ - * /` | `+ - * /` | `(vm:+ (d 1d308) (d 1d308))` → `#(Inf)` for arrays; the same in R (`Inf`) and NumPy (`inf`, overflow warning). The scalar `(vm:+ 1d308 1d308)` signals, as `cl:+`. |
| `vm:/ vm:*` on complex arrays | `/`, `*` on complex | the same | Same values: `(1+2i)*(3+4i)` → `-5+10i`, `/` → `0.44+0.08i` in all three. `(vm:/ #C(1d0 2d0) #C(0d0 0d0))` on an array → `#C(Inf Inf)`; R `(1+2i)/(0+0i)` → `Inf+Infi`; NumPy `inf+infj`. Python `(1+2j)/0j` raises. vm's complex `*` and `/` use MKL, accurate to a few ulps, so they may differ from `cl:*` in the last bit (not observed in the case tried). |
| `vm:incf vm:decf` | no in-place operators (`x <- x + 1`) | `a += 1`, `np.add(a, 1, out=a)` | `(vm:incf a 1d0)` is `(setq a (vm:+ a 1d0))`: a new array, so an alias `b` still sees `#(1 2 3)`. NumPy `a += 1` mutates, and `b` sees `[2. 3. 4.]`. R copies on modify, like vm. For in-place vm use `(vm:+ a 1d0 :into a)`. |
| `incf` changing the type | – | `a += 0.5` | `(vm:incf a 0.5d0)` on an integer array gives a new double array `#(1.5d0 2.5d0 3.5d0)`. NumPy int64 `a += 0.5` raises `UFuncTypeError` (cannot cast float64 to int64). `(vm:+ a 0.5d0 :into a)` signals `vm:output-type-error`. |
| `vm:incf` on places | – | – | `(vm:incf (first m) 10)` works on any `setf` place; `(vm:incf n)` on a number is `cl:incf`. |

- Recycling: `(vm:+ (d 1 2 3) (d 1 2))` → `#(2 4 4)` with a `vm:recycling-warning`; R gives `2 4 4` with a warning; NumPy raises `ValueError`.
- Empty arrays and `-0.0`: `(vm:+ (d) 1)` → an empty double array; R `numeric(0)+1` → `numeric(0)`. `0 - 0.0` gives `0.0` in all three; `1/-0.0` is `-Inf` in R and NumPy.
- Mixed single and double: `(vm:+ (i64 1 2) 0.5f0)` → a single-float array; NumPy `float32 + 0.5` stays float32, `int32 + 0.5` gives float64, `int8 + 200` widens to int16; R has only double.

## Exponentials, logarithms, powers and roots

vm returns exact integers and ratios for integer powers, complex arrays instead of NaN for out-of-domain real input, and single floats for integer-array inputs of `exp`, `log` and `sqrt`.

| vm | R | NumPy | Differences |
| --- | --- | --- | --- |
| `vm:exp` | `exp` | `np.exp` | Float arrays: `#(1 e Inf 0)` for `0 1 1000 -1000`, no signal; the same in R; NumPy adds an overflow warning. Integer arrays give single floats: `(vm:exp (i64 0 1))` → `#(1.0 2.7182817)`; R and NumPy int64 give doubles (NumPy int8 gives float16). Scalar `(vm:exp 1000d0)` signals overflow; `math.exp(1000)` raises OverflowError. |
| `vm:expt`, integer base and exponent | `^`, `**` | `**`, `np.power` | `(vm:expt (i64 2 3) -1)` → `#(1/2 1/3)`; `(vm:expt (i64 2 3) 70)` → exact bignums. R `c(2L,3L)^-1L` → `0.5 0.333`, `^70L` → `1.18e21`. NumPy raises `ValueError: Integers to negative integer powers are not allowed`; `**70` wraps to `[0, 2293070008301402073]`. |
| `vm:expt`, float | `^` | `**`, `np.power` | `0^0` → `1` in all three, also `(vm:expt (d 0 1) 0)` → `#(1 1)`. A float base with an integer exponent is CL's repeated multiplication. `(vm:expt (d 0) -1)` → `#(Inf)`; R `Inf`; NumPy `inf` with a warning. `10^400`: vm array `#(Inf)` (the scalar signals), R `Inf`, NumPy `inf` with a warning, Python `10.0**400` raises OverflowError. |
| `vm:expt`, negative base and non-integer power | `^` | `**`, `np.emath.power` | `(vm:expt (d -8 4) 0.5d0)` → complex array `#(#C(1.7e-16 2.83) #C(2 0))`. R `c(-8,4)^0.5` → `NaN 2`; NumPy `[nan, 2.]` with a warning. `np.emath.power` and Python `(-8)**(1/3)` also give complex. |
| `vm:log`, one argument | `log` | `np.log`, `np.emath.log` | `(vm:log (d 1 0))` → `#(0 -Inf)`; add `-1` and the whole result is complex: `#(#C(0 0) #C(-Inf 0) #C(0 pi))`. R `log(c(1,0,-1))` → `0 -Inf NaN` (warning); NumPy `[0, -inf, nan]`. Scalar `(vm:log 0d0)` signals `division-by-zero`; `math.log(0)` raises ValueError. |
| `vm:log`, two arguments | `log(x, base)`, `log2`, `log10` | no base argument: `np.log2`, `np.log10`, `np.emath.logn(n, x)` | `(vm:log x base)` and R `log(x, base)` take x first; `np.emath.logn` takes the base first. Float arrays compute `(/ (log x) (log base))`: `(vm:log (d 1000) 10d0)` → `2.9999999999999996`; R `log(1000,10)` → `3`, NumPy `np.log10` → `3.0`; Python `math.log(1000,10)` → `2.9999999999999996`. Integer arrays give single floats: `(vm:log (i64 8 100) 2)` → `#(3.0 6.643856)`. Base `1` → `Inf` in vm and R; `math.log(8,1)` raises. |
| `vm:sqrt` | `sqrt` | `np.sqrt`, `np.emath.sqrt` | `(vm:sqrt (d 4 2 -4 0))` → complex array `#(#C(2 0) #C(1.414 0) #C(0 2) #C(0 0))`. R `2 1.414 NaN 0` (warning); NumPy the same with a warning; `np.emath.sqrt` gives complex like vm. `math.sqrt(-1)` raises, `cmath.sqrt(-1)` → `1j`. `(vm:sqrt (d -0.0d0))` → `-0.0` in vm and NumPy; R `sqrt(-0)` prints `0`. Correctly rounded in vm (not MKL). |
| `vm:sqrt`, integer arrays | `sqrt` | `np.sqrt` | Follows the implementation's `cl:sqrt`, which CL leaves open for perfect squares. CCL: `(vm:sqrt (i64 4 9))` → `T` array `#(2 3)` (exact), `(vm:sqrt (i64 4 2))` → `#(2 1.4142135)`, `(vm:sqrt (i64 4 -9))` → `#(2 #C(0 3))`. SBCL: single floats, `#(2.0 3.0)`. R and NumPy int64 always give doubles. |
| `vm:isqrt` | none: `floor(sqrt(x))` | none: `math.isqrt` (scalar), `np.vectorize(math.isqrt)` | `(vm:isqrt (i64 16 17 0 99))` → `#(4 4 0 9)`. Negative or float input signals `type-error` (expected `unsigned-byte`); `math.isqrt(-1)` raises ValueError, `math.isqrt(4.0)` TypeError. Exact for bignums: `(vm:isqrt (1- (expt 10 40)))` → `99999999999999999999`. |

- `isqrt` versus `floor(sqrt(x))` for large values: `math.isqrt(2**62-1)` → `2147483647`, but `int(np.sqrt(np.array([2**62-1])))` → `2147483648`. R doubles cannot hold `10^40-1` (it is `1e40`), so `floor(sqrt(10^40-1))` is `1e20`, not `99999999999999999999`.
- `0^-1` for exact integers: `(vm:expt (i64 0) -1)` signals `division-by-zero`; NumPy int `0 ** -1` raises ValueError; Python `0**-1` raises ZeroDivisionError; R gives `Inf`.
- Complex arguments agree: `(vm:exp #C(0 pi))` → `#C(-1 1.22e-16)` in vm and NumPy; R `exp(1i*pi)` prints `-1+0i`.
- Scalar `(vm:log 8 2)` → single `3.0`, and `(vm:log 8d0 0d0)` → `0.0d0` (CL rules); NumPy `np.log(8.)/np.log(0.)` → `-0.0` with a warning.

## Trigonometric, hyperbolic and complex-part operators

vm returns a complex array where R and NumPy return NaN (out-of-domain `asin`, `acos`, `acosh`, `atanh`), keeps exact integer and rational results, and takes branch cuts and signed zeros from the Lisp implementation.

| vm | R | NumPy | Differences |
| --- | --- | --- | --- |
| `sin cos tan sinh cosh tanh asinh atan` (one argument) | `sin` … `atanh` | `np.sin` … `np.arctan` | Same values. Integer arrays: vm gives single floats (`(vm:sin (i64 0 1))` → `#(0.0 0.84147096)`), R double, NumPy float64 (int8 gives float16). `sin(Inf)`: vm NaN silently; R NaN with a warning; NumPy NaN with a warning; `math.sin(inf)` raises. `sinh(1000)`: vm and R `Inf` silently; NumPy `inf` with an overflow warning; `math.sinh` raises OverflowError. |
| `asin acos acosh atanh` | `asin acos acosh atanh` | `np.arcsin` … `np.arctanh` | Out of domain: `(vm:asin (d 0.5 2))` → `#(#C(0.52 0.0) #C(1.57 -1.317))` (the whole array complex); R `0.52 NaN` with a warning; NumPy `0.52 nan` with a RuntimeWarning, complex only if the input is complex. `math.asin(2)` raises; `cmath.asin(2)` gives `1.57+1.317j`. At the domain's edge, `atanh(1)` is `+Inf` (real) in vm, R and NumPy (NumPy warns). |
| `atan` with two arguments, `(atan y x)` | `atan2(y, x)` | `np.arctan2(y, x)` | Same order and values, including `(atan -0.0 -0.0)` → `-pi`. R `atan(1, 1)` is an error (one argument only). vm integer arrays give single floats; `(vm:atan 1 1)` is `0.7853982` (single). There is no `vm:atan2`. |
| `cis` | `exp(1i*x)` | `np.exp(1j*x)` | vm needs a real argument: a complex array signals an error (`simple-error` on CCL, `type-error` on SBCL). R and NumPy accept complex. Integer or rational input gives complex single floats; a double array gives complex doubles. |
| `abs` | `abs`, `Mod` | `np.abs` | Complex input gives a real float array in all three. Integer overflow: `(vm:abs (i64 (- (expt 2 63))))` → `9223372036854775808` (element type `T`); NumPy int64 stays `-9223372036854775808`; R integers are 32-bit (its smallest integer is `NA`); Python `abs` is exact. `abs(-0.0)` is `0.0` in all three. `abs(3e200+4e200j)`: vm and R `5e200`; NumPy `4.9999999999999995e+200`. |
| `signum` | `sign` | `np.sign` | Complex: vm gives z/\|z\| (`#C(0.6 0.8)`); R errors ("unimplemented complex function"); NumPy 1.26 gives the sign of the real part (`3+4j` → `1+0j`; `0+2j` → `1+0j`). Float: the result keeps the float format; `(signum -0.0)` is `-0.0`, as in the CLHS example, on SBCL and on CCL with [Clozure/ccl#752](https://github.com/Clozure/ccl/pull/752), which fixes it; released CCL 1.14 gives `0.0`. R and NumPy give `0`. NaN: vm follows the NaN's sign bit (`-1.0` or `1.0`; checked on CCL); R and NumPy give NaN. Rationals: `(vm:signum #(-1/2 5/3))` → `#(-1 1)` (integers). |
| `conjugate` | `Conj` | `np.conj` | Real arrays come back unchanged, with the same type, in all three. Complex: `#C(-1 -0.0)` gives imaginary part `0.0` in vm and NumPy. |
| `phase` | `Arg` | `np.angle` | Real input: `(vm:phase #(-1.0 0.0 -0.0))` → `#(pi 0 pi)`, and R and NumPy agree (`-0.0` gives pi). Integer arrays: vm single float, R and NumPy double. Complex with a `-0.0` imaginary part: `-pi` in vm, NumPy, Python `cmath` and R `Arg`. NaN: vm gives pi or 0 by the NaN's sign bit (CCL); R and NumPy give NaN. NumPy has `deg=True`. |
| `realpart imagpart` | `Re`, `Im` | `np.real`, `np.imag` | `(vm:imagpart (i64 1 2))` → `#(0 0)`, `(vm:imagpart (d 1 2))` → `#(0.0 0.0)`, the same type as the input; R `Im(1:2)` is numeric `0 0`; NumPy `np.imag` of int64 is int64 zeros (a read-only view). `(vm:imagpart (d -0.0))` is `-0.0` on CCL and `0.0` on SBCL; R and NumPy give `0`. `np.real` of a real array is a writable view; vm returns a new array. |
| `complex` | `complex(real=, imaginary=)`, `x + 1i*y` | `x + 1j*y`, `np.vectorize(complex)` | Float operands: a complex float array in all three (recycling in vm and R, a broadcasting error for shapes (3,) and (2,) in NumPy). Integers and rationals: CL drops a zero imaginary part, so `(vm:complex #(1 2) 0)` → `#(1 2)` (real); R `complex(real=1:2, imaginary=0)` and NumPy give complex. Mixed: `(vm:complex #(1 2) #(0 1))` → `#(1 #C(2 1))`. One argument: `(vm:complex (d 1 2))` → `#(#C(1.0 0.0) …)`. Complex input is a `type-error` (real required). |

- **Branch cut of `atanh(2)`.** vm on CCL gives `-pi/2` and on SBCL `+pi/2`; R gives `-pi/2`; NumPy `+pi/2`, as `cmath.atanh(2)`. See [Clozure/ccl#741](https://github.com/Clozure/ccl/issues/741).
  - Real array `(vm:atanh (d 2d0))` → `#(#C(0.549 -1.5708))` on CCL, `#(#C(0.549 1.5708))` on SBCL.
  - R `atanh(c(0.5,2))` → `0.549 NaN`. R gives `0.549-1.5708i` for both `2+0i` and `2-0i`: it ignores the zero's sign.
  - NumPy `np.arctanh([2+0j, 2-0j])` → `+1.5708j`, `-1.5708j`.
- **Branch cuts for complex arrays.** vm complex arrays honour `-0.0` on both CCL and SBCL (MKL kernel), like NumPy: `(vm:atanh #(#C(2 0) #C(2 -0.0)))` → `+pi/2`, `-pi/2`; R gives `-pi/2` for both. `acosh(-2+0i)` is `+pi` and `acosh(-2-0i)` is `-pi` in vm and NumPy; R gives `+pi` for both.
- **NaN in a real array that triggers complex.** `(vm:asin (d nan))` is `#(#C(NaN NaN))` on both implementations, while R and NumPy return a real NaN.
- **Integer and rational exactness.** `(vm:abs #(-1/2 3))` → `#(1/2 3)` and `(vm:signum #(-1/2 0 5/3))` → `#(-1 0 1)` exactly; R and NumPy return doubles, and NumPy `np.abs` of an int64 array stays int64. With exact complex elements, `(vm:signum #(#C(3 4)))` → `#C(3/5 4/5)` on CCL but `#C(0.6 0.8)` in single floats on SBCL.
- **Single vs double.** CL contagion: an integer array in gives a single-float array out (`(vm:sin (i64 0 1))`), and `(vm:atan #(1 2) 0)` is single. R is always double; NumPy gives float64 for int64, float32 for float32 and float16 for int8.
- **Shapes.** Matrices keep their shape in all three. R also keeps names; vm arrays have no names.

## Comparisons, predicates, max and min

vm comparisons are n-ary chains returning bit arrays, compare integers and floats exactly and treat a NaN as unordered (0, or 1 for `/=`); `vm:max` and `vm:min` are elementwise, NaN-propagating operators (like `pmax` and `pmin`), not reductions.

| vm | R | NumPy | Differences |
| --- | --- | --- | --- |
| `vm:= vm:/= vm:< vm:> vm:<= vm:>=` on arrays | `== != < > <= >=` | `np.equal`, `np.not_equal`, `np.less`, `np.greater`, `np.less_equal`, `np.greater_equal` | The result is a `(simple-array bit)` (`#*1011`); R gives `logical`, NumPy `bool`. NaN: `(vm:< #(1 nan) #(2 nan))` → `#*10`, and `/=` gives 1; R gives `NA`; NumPy `False`, and `!=` `True`. `-0.0 = 0.0` in all three. |
| n-ary `(vm:< a b c)`, `(vm:/= a b c)` | binary only; chain with `a < b & b < c` | binary only; `np.less(1,2,3)` fails (the third argument is `out`); Python `1<2<3` chains | `(vm:< #(1 2 3) #(2 3 4) #(3 4 4))` → `#*110` (a chain per element). `(vm:/= a b c)` is true only if all pairs differ: `(vm:/= #(1 2 3) #(1 3 3) #(2 2 3))` → `#*000`. `(vm:=)` with one array gives ones. |
| `vm:=`, `vm:<` on integers vs floats | `9007199254740993 == 9007199254740992` is `TRUE` (the literal is a double) | `np.array([9007199254740993]) == 9007199254740992.0` → `True`; `>` → `False` | vm compares exactly: `(vm:= #(9007199254740993) 9007199254740992d0)` → `#*0`, and `vm:>` → `#*1`. Python scalar `int == float` is also exact; NumPy converts to float64. Also `(vm:= (d 0.1d0) 1/10)` → `#*0`, and `(vm:= 0.1f0 0.1d0)` → `NIL`; NumPy `np.float32(0.1)==0.1` is `False` too. |
| `vm:= vm:/=` on complex | `==`, `!=` | `np.equal`, `np.not_equal` | Same results. Ordering differs: vm `vm:<` and the others signal `type-error` (expected `real`); R errors ("invalid comparison with complex values"); NumPy 1.26 compares lexicographically (`np.less([1+1j,3],[2,1j])` → `[True False]`). |
| `vm:zerop vm:plusp vm:minusp` | `x == 0`, `x > 0`, `x < 0` | `x == 0`, `x > 0`, `x < 0` | Bit arrays; `zerop` accepts complex. NaN gives 0 for all three (R `NA`, NumPy `False`). For `-0.0`: `zerop` 1, `plusp` 0, `minusp` 0 in all three. |
| `vm:evenp vm:oddp` | `x %% 2 == 0`, `x %% 2 == 1` | `x % 2 == 0`, `x % 2 == 1` | Integers only, bignums included: `(vm:evenp #(1 2 -1 -2))` → `#*0101`. Float arrays signal `type-error` (`(vm:evenp (d 2d0))`); R `c(1.5,2) %% 2 == 0` and NumPy `np.array([1.5,2.])%2==0` return `FALSE TRUE`. |
| `vm:max vm:min`, two or more operands | `pmax`, `pmin` | `np.maximum`, `np.minimum` (propagate NaN); `np.fmax`, `np.fmin` (ignore NaN) | All elementwise. vm folds n-ary arguments from the left; `pmax` takes `...`; NumPy ufuncs take two (the third argument is `out`). NaN: `(vm:max (d 1 nan 3) (d nan 2 1))` → `#(nan nan 3)`; `np.maximum` the same; `np.fmax` → `[1 2 3]`; R `pmax(c(1,NA,3),c(4,2,NaN))` → `4 NA NaN`, and with `na.rm=TRUE` → `4 2 3`. |
| `vm:max vm:min` of one array | `max(x)`, `min(x)` (reductions) | `np.max`, `np.min`, `np.maximum.reduce` | `(vm:max #(1 2 3))` returns the array unchanged, not 3. A reduction needs `(reduce #'vm:max v)` or a loop. R `max(c(1,5,3),c(4,2,6))` → 6 (a reduction over everything). Empty: R `max(numeric(0))` → `-Inf` with a warning; `np.max([])` raises; `(vm:max #() #())` → `#()`. |
| `vm:max vm:min` result type | `pmax(1:3, 2.5)` numeric | `np.maximum(int64 array, 3.5)` float64 | vm uses contagion: `(vm:max #(1 5) 3.5f0)` → a single-float array; integer arrays → `(signed-byte 64)` (or `T` for `#(…)`). NumPy gives float64 for an int array with a float32 scalar; R double. |
| `vm:max vm:min` of scalars | `max(3, 2.0)` | `np.maximum(3, 2.0)` → `3.0` | Scalars are `cl:max`: `(vm:max 3 2.0)` → the integer `3` (CCL and SBCL), `(vm:max 1 2.0)` → `2.0`. The array form always converts: `(vm:max #(3) 2.0)` → `#(3.0)`. Python `max(3, 2.0)` → `3`, like CL. |
| Signed zeros in `vm:max vm:min` | `pmax`, `pmin` | `np.maximum`, `np.fmax` | A tie returns the first operand: `(vm:max (d -0d0) (d 0d0))` → `#(-0.0d0)`, swapped → `#(0.0d0)`. R `pmax(-0,0)` also keeps the first. NumPy keeps the second: `np.maximum([0.],[-0.])` → `[-0.]`; `np.fmax` the same. |

- **Scalar NaN comparisons signal.** `(vm:< nan 1d0)` and `(vm:= nan nan)` signal `floating-point-invalid-operation` on CCL and SBCL (traps on); the array forms return 0 without signalling. R `NaN == NaN` → `NA`; NumPy `np.nan == np.nan` → `False`.
- **Recycling vs broadcasting.** `(vm:< #(1 2 3) #(2 3))` → `#*110` with a warning; R `1:3 < c(2,3)` → `TRUE TRUE FALSE` with a warning; NumPy `np.less([1,2,3],[2,3])` raises a broadcast error.
- **NaN and NA in reductions.** NumPy `np.max([1,nan,3])` → `nan`, `np.nanmax` → `3.0`; R `max(c(1,NaN,NA))` and `max(c(1,NA,NaN))` → `NA`, `max(c(1,NaN,3))` → `NaN`; vm has no NA.
- **Python `max`.** `max(1, nan, 3)` → `3`, but `max(nan, 1, 3)` → `nan` (order-dependent); vm arrays always give NaN.

## Division and rounding

vm's rounding functions return two arrays (an integer quotient and a remainder) and take a divisor, not a digit count; they signal on zero divisors, NaN and Inf where R and NumPy return Inf or NaN.

| vm | R | NumPy | Differences |
| --- | --- | --- | --- |
| `floor ceiling truncate round`, one argument | `floor`, `ceiling`, `trunc`, `round` | `np.floor`, `np.ceil`, `np.trunc`, `np.round`, `np.rint` | vm gives two values: `(vm:floor (d 7.5 -7.5))` → `#(7 -8)` of `(signed-byte 64)` and the remainder `#(0.5d0 0.5d0)`. R and NumPy return one float64 array, `7 -8`. The quotient has element type `T` (bignums) when a value exceeds 64 bits: `(vm:floor (d 1d300))` is an exact integer; R and NumPy keep `1e300`. |
| `round` ties | `round(c(.5,1.5,2.5,-.5))` → `0 2 2 0` | `np.round`, the same, gives `-0.` for -0.5 | All round ties to even: `(vm:round (d 0.5 1.5 2.5 -0.5))` → `#(0 2 2 0)`. Python `round` the same. |
| Two-argument `round floor ceiling truncate` | `round(x, digits)`; `%/%` for floor | `np.round(x, decimals)`; `np.floor_divide` | vm's second argument is a divisor: `(vm:round (d 3.14159) 100)` → quotient `#(0)`, remainder `#(3.14159d0)`. R `round(3.14159, 2)` and NumPy `np.round(3.14159, 2)` → `3.14`. No vm operator rounds to decimal digits. |
| Two-argument `floor` (with `mod`) | `x %/% y`, `x %% y` | `np.divmod(x, y)`, `//` | Both values are arrays, like `np.divmod`; R has no two-value form. `np.divmod(np.array([7,-7]),2)` → `[3,-4]`, `[1,1]` (int64); `(vm:floor (i64 7 -7) 2)` gives the same values. |
| `ffloor fceiling ftruncate fround` | `floor` etc. (always double) | `np.floor` etc. | vm returns a float quotient plus a remainder array. Integer inputs give single-float quotients: `(vm:ffloor (i64 3 4))` → `#(3.0 4.0)` (single). R and NumPy give doubles. The sign of a zero quotient is the implementation's: on CCL `(vm:ftruncate (d -0.5))` → `0.0`; R and NumPy give `-0`. |
| `mod` | `%%` | `np.mod`, `np.remainder`, `%` | The sign of the divisor in all four: `7 mod -3` = `-2`. `(vm:mod (i64 7 -7) (i64 3 3))` → `#(1 2)`; R `%%` → `1 2`; NumPy → `[1 2]`; Python `%` the same. `(vm:mod #(-0.0d0) 3d0)` keeps `-0.0`; R and NumPy give `+0`. |
| `rem` | `x - trunc(x/y)*y` (no function) | `np.fmod` | The sign of the dividend: `(vm:rem (i64 -7) (i64 3))` → `#(-1)`; `np.fmod` → `-1`; Python `math.fmod(-7,3)` → `-1.0`. |
| `mod rem ffloor` with a float zero or Inf divisor | `%%`, `%/%` | `np.mod`, `np.fmod`, `np.floor_divide` | vm float arrays give IEEE results, no signal. `(vm:mod (d 5 -5) 0d0)` → `NaN NaN`; R `%% 0` → `NaN`; NumPy `NaN` with a RuntimeWarning. Python `5.0 % 0.0` raises ZeroDivisionError. `(vm:ffloor (d 5 -5 0) 0d0)` → `Inf -Inf NaN`, the same as R `%/%` and NumPy `//`. |
| `mod` by infinity | `%% Inf` | `np.mod` | All agree: `5 mod Inf` = 5, `-5 mod Inf` = `Inf`, `5 mod -Inf` = `-Inf`. `rem` and `np.fmod` return the dividend. |
| `floor ceiling truncate round` with integer divisor 0 | `5L %/% 0L` → `NA`; `5 %/% 0` → `Inf` | `np.floor_divide` on ints → `0` with a warning | vm signals `division-by-zero` for integer arrays, and for `mod` and `rem`. Python `5 // 0` raises ZeroDivisionError. |
| `floor` etc. of NaN, Inf, or with a float zero divisor | `floor(NaN)` → `NaN`; `floor(Inf)` → `Inf` | `np.floor` the same | vm's integer-quotient functions signal CL's condition: `(vm:floor (d nan))` → `floating-point-invalid-operation`; `(vm:floor (d inf))` → `floating-point-overflow`; `(vm:floor (d 5) 0d0)` → `division-by-zero`. Python `math.floor(inf)` raises OverflowError, `math.floor(nan)` ValueError. |
| `gcd lcm` | none in base R | `np.gcd`, `np.lcm` (also `.reduce`) | vm is n-ary on arrays: `(vm:gcd (i64 12 18) (i64 18 27) 4)` → `#(2 1)`. Results are never negative; 0 behaves as in NumPy (`lcm(0,5)` = 0). Python `math.gcd` and `math.lcm` are n-ary too. |
| `gcd lcm` overflow and types | – | – | vm grows to `T` with bignums: `(vm:lcm (i64 4611686018427387904) 3)` → `#(13835058055282163712)`. `np.lcm` int64 wraps to `[-4611686018427387904]`. Python `math.lcm(2**62, 3)` is exact. Floats: vm signals `type-error`, `np.gcd` raises `UFuncTypeError`, Python `TypeError`. |
| `(vm:gcd)`, `(vm:lcm)` with no arguments | – | – | `0` and `1`, as in CL and Python `math.gcd()`, `math.lcm()`. NumPy `np.gcd` needs two arguments. |

- **The smallest integer divided by -1.** `(vm:floor (i64 -9223372036854775808) -1)` → `#(9223372036854775808)` of type `T`. `np.floor_divide` of the int64 minimum by -1 gives `-9223372036854775808` with an overflow warning. R integers are 32-bit, and its smallest integer value is `NA`.
- **Ties when rounding to digits.** R `round(0.15, 1)` → `0.1`, NumPy `np.round(0.15, 1)` → `0.2`, Python `round(0.15, 1)` → `0.1`. vm has no equivalent: `(vm:round (d 0.15) 0.1d0)` gives quotient `1` (0.15/0.1 is below 1.5) and remainder `0.05`.
- **Float to integer.** R `floor` and `np.floor` return floats, so `floor(1e300)` is representable. `np.floor(inf).astype(np.int64)` gives `-9223372036854775808` with a RuntimeWarning. vm's integer quotient cannot hold Inf or NaN, hence its error.
- **Large quotients.** For `1e22 mod 3` (exactly 1), R `1e22 %% 3` gives `1` with a "probable complete loss of accuracy in modulus" warning; `(vm:mod (d 1d22) 3d0)` gives `#(0.0d0)` on CCL, silently (CL computes `x - q*y`, implementation-dependent).
- **Scalars** are exactly CL: `(vm:floor 7.5d0 2)` → `3`, `1.5d0`; `(vm:mod 7 -3)` → `-2`.
- **Empty arrays.** `(vm:floor (d))` gives two empty arrays (a `(signed-byte 64)` quotient and a double remainder); R `floor(numeric(0))` returns one empty double.

## Integer and bitwise operators

vm uses unbounded two's complement and grows to bignums on overflow, while R is limited to 32-bit integers (`NA` outside them) and NumPy wraps at a fixed width.

| vm | R | NumPy | Differences |
| --- | --- | --- | --- |
| `vm:logand vm:logior vm:logxor` | `bitwAnd`, `bitwOr`, `bitwXor` | `np.bitwise_and`, `np.bitwise_or`, `np.bitwise_xor` (`&`, `\|`, `^`) | `(vm:logand #(12 10) #(10 6) #(8 8))` → `#(8 0)` (n-ary, folded from the left). R and NumPy take two operands only. `(vm:logand)` → -1 and `(vm:logior)` → 0; NumPy gets -1 from `np.bitwise_and.reduce` of an empty int64 array. |
| `vm:logeqv vm:lognand vm:lognor vm:logandc1 vm:logandc2 vm:logorc1 vm:logorc2` | none; compose, e.g. `bitwNot(bitwXor(a,b))` | none; compose with `np.invert` | `(vm:lognand #(5 3) #(3 3))` → `#(-2 -4)`; NumPy `np.invert(np.bitwise_and(a, b))` gives the same. `vm:logeqv` is n-ary. |
| `vm:lognot` | `bitwNot` | `np.invert`, `~` | `(vm:lognot #(5 0 -1))` → `#(-6 -1 0)`; R and NumPy int64 agree. NumPy `uint8` wraps: `np.invert` of uint8 5 → 250. vm has no unsigned wrap: `lognot` of an `(unsigned-byte 64)` array holding 2^63 gives -2^63-1 (a `T` array). |
| `vm:ash` | `bitwShiftL`, `bitwShiftR` | `np.left_shift`, `np.right_shift` (`<<`, `>>`) | One function with a signed count: a negative count shifts right, arithmetically (rounding down): `(vm:ash #(1 -8 5 -5) -1)` → `#(0 -4 2 -3)`. `np.right_shift` of -8 by 1 gives -4 too. R `bitwShiftR(-8L,1L)` → 2147483644 (an unsigned 32-bit shift). |
| `vm:ash`, overflow | `bitwShiftL(1L,31L)` → `NA` | `np.left_shift([1,2],62)` → `[4611686018427387904, -9223372036854775808]` | `(vm:ash (i64 1 2) 63)` → `#(2^63 2^64)` as a `T` array of bignums. NumPy wraps, and a shift of 64 or more gives 0. R gives `NA` when the result needs more than 31 bits. Python `1<<70` is exact, like vm. |
| `vm:ash`, unusual counts | `bitwShiftL(1L,-1L)` → `NA` | `np.left_shift([1],-1)` → `[0]`; `np.right_shift([-8,8],-1)` → `[-1,0]` | In vm a negative count is a valid right shift, and any count works: `(vm:ash (i64 1 -1) -100)` → `#(0 -1)`. NumPy: negative or ≥64 counts give 0 or -1 silently. R: `NA`. Python `5>>-1` raises ValueError. |
| `vm:logcount` | none; `sum(intToBits(x)==1L)` | `np.bitwise_count` (NumPy 2.0+, absent in 1.26) | `(vm:logcount #(7 -8 0 -1 255))` → `#(3 3 0 0 8)`: for negatives it counts the 0 bits (`(logcount -8)` = 3). The R idiom works on 32 bits and gives 29 for -8. Python `(-8).bit_count()` → 1 (it counts the absolute value). |
| `vm:integer-length` | none | none; per element `int.bit_length()` | `(vm:integer-length #(0 1 255 -1 -8 -256 -257))` → `#(0 1 8 0 3 8 9)`. Python `bit_length` gives `[0,1,8,1,4,9,9]`: it counts the absolute value, so -1 → 1 and -8 → 4. |
| `vm:logbitp vm:logtest` | `bitwAnd(x, bitwShiftL(1L,k)) != 0` | `(x >> k) & 1`, `(x & m) != 0` | Both return bit arrays: `(vm:logbitp 0 #(5 4 -1))` → `#*101`; `(vm:logtest #(5 4 0) 4)` → `#*110`. NumPy `(x & 4) != 0` returns a bool array. The `vm:logbitp` index may be an array too. Index 100 on `(i64 -1 1)` → `#*10` (sign extension). |

- **Non-integer input.** `(vm:logand 1.5 1)`, `(vm:logand (d 1 2) 1)` and `(vm:logand #(1/2) 1)` signal `type-error` (expected `integer`). NumPy `np.bitwise_and([1.5],1)` raises TypeError. R truncates silently: `bitwAnd(1.5,1)` → 1, and whole doubles work, `bitwAnd(12,10)` → 8.
- **R's range.** `bitwAnd(2^31,1)` and `bitwAnd(2^32+3,7)` give `NA` with a warning, and `bitwAnd(TRUE,TRUE)` is an error. vm has no limit: `(vm:logand (i64 -1) (expt 2 70))` → `#(2^70)`.
- **Result element type.** vm gives `(signed-byte 64)` when all values fit, else `T`: `(vm:ash (i64 1 2) 62)` is a `T` array because 2^63 does not fit. NumPy int64 stays int64; `np.bitwise_and([-1], 2**70)` falls back to an `object` array, and `np.bitwise_and([-1], 2**63)` raises TypeError (int64 with uint64).
- **Lengths that don't divide.** `(vm:logand #(1 2 3) #(1 2))` → `#(1 2 1)` with a warning; R `bitwAnd(c(1L,2L,3L),c(1L,2L))` → `1 2 1`; NumPy raises ValueError.
- **Empty input.** `np.bitwise_and(np.array([]), np.array([]))` raises TypeError (the default dtype is float64); with int64 it returns an empty int64 array. `(vm:logand (i64) (i64))` → `#()`.

## Conversions

`vm:float` of an integer or ratio array gives single floats (the CL default), while R and NumPy give doubles; an integer too big for the float format signals on CCL but gives infinity on SBCL and in R.

| vm | R | NumPy | Differences |
| --- | --- | --- | --- |
| `vm:float` | `as.double`, `as.numeric` | `astype(np.float64)`, `astype(np.float32)` | `(vm:float #(1 2))` → a single-float array; `(vm:float #(1 2) 1d0)` → a double array (the prototype argument). R `as.double(1:3)` and NumPy `.astype(float)` give doubles. Ratios: `(vm:float #(1/3))` → `#(0.33333334)`. Complex input is a `type-error`; R drops the imaginary part with a warning, and NumPy `astype(float64)` does too, with a ComplexWarning. |
| `vm:float`, overflow | `as.double(10^400)` → `Inf` | `np.array([10**400]).astype(np.float64)` raises OverflowError | CCL: `(vm:float (expt 10 400) 1d0)` and `(vm:float (vector (expt 10 39) 1))` signal `floating-point-overflow`. SBCL: the array form gives `#(Inf)`, because vm masks the traps and SBCL then returns infinity. A double array converted to single: `(vm:float (d 1d300) 1f0)` → `#(Inf)` quietly; NumPy gives `inf` with a warning. Python `float(10**400)` raises OverflowError. |
| `vm:rational vm:rationalize` | none in base R | none; Python `fractions.Fraction(x)`, `.limit_denominator(n)` | `(vm:rational (d 0.1 0.5 2))` → `#(3602879701896397/36028797018963968 1/2 2)`, exactly; Python `Fraction(0.1)` and `(0.1).as_integer_ratio()` agree. `(vm:rationalize (d 0.1))` → `#(1/10)`, like `limit_denominator` but chosen by the float's accuracy, not a bound. Results are a `T` array unless all values are integral. NaN: CCL returns a huge integer, with no error (not checked on SBCL); Python `as_integer_ratio` raises for NaN and infinity. |
| `vm:numerator vm:denominator` | none | none; `Fraction.numerator` (Python scalars) | `(vm:numerator #(1/2 3 -2/3))` → `#(1 3 -2)`, and `vm:denominator` → `#(2 1 3)`. A float array is a `type-error` (expected `rational`): `(vm:numerator (d 0.5))`. NumPy float64 has no `.numerator` (AttributeError). |
| `vm:scale-float` | `x * 2^k` | `np.ldexp` | `(vm:scale-float (d 1 3) #(1 2))` → `#(2.0 12.0)`; `np.ldexp` the same. Float arrays only: `(vm:scale-float #(1 2) 2)` is a `type-error`, while `np.ldexp(np.array([1,2]),2)` converts to float64. The count must be an integer; NumPy rejects `2.5` (TypeError). |
| `vm:scale-float`, range | `1 * 2^2000` → `Inf`, `2^-2000` → 0 | `np.ldexp([1.0],2000)` → `inf` with a RuntimeWarning; `-2000` → 0 | Array overflow gives `Inf` silently: `(vm:scale-float (d 1) 2000)` → `#(Inf)`; `(d -1)` scaled by -2000 → `#(-0.0)` (NumPy `-0.` too). The scalar signals `floating-point-overflow`; Python `math.ldexp(1.0,2000)` raises OverflowError. NaN stays NaN. |
| `vm:float-sign`, two arguments | none; an `abs(y) * sign` idiom (`sign` loses -0) | `np.copysign(y, x)` (arguments reversed) | `(float-sign x y)` is \|y\| with the sign of x, so the sign comes first: `(vm:float-sign (d 1 1) (d 0 -0d0))` → `#(0.0 -0.0)`. `np.copysign(y,x)` gives the same values. `(vm:float-sign (d -1 0 -0d0 5) 3d0)` → `#(-3.0 3.0 -3.0 3.0)`. NumPy `copysign` takes ints and gives floats. |
| `vm:float-sign`, one argument | `sign` | `np.sign`, `np.copysign(1.0, x)` | Always ±1.0, zeros included: `(vm:float-sign (d -1 0 -0d0 5))` → `#(-1.0 1.0 -1.0 1.0)`. R `sign(c(-1,0,-0,5))` → `-1 0 0 1`; `np.sign` the same. The NumPy equivalent is `np.copysign(1.0, x)` → `[-1,1,-1,1]`. `(vm:float-sign #(1 2))` is a `type-error` (it needs floats). |

- **The sign of a NaN** (CCL; not run on SBCL): `(vm:float-sign nan)` follows the sign bit, giving `-1.0` for the NaN of 0/0. `(vm:float-sign (d 1) nan)` returns a NaN with that sign, not ±1.0. `np.copysign([1.0], [np.nan])` → `[1.]`, and with `-np.nan` → `[-1.]`.
- **Float formats.** `(vm:float (sf 1f0) 1d0)` → a double array. `(vm:float-sign (sf -2f0) 1d0)` → `#(-1.0D0)` on CCL (the second argument's format). NumPy `np.copysign(float32 array, float64 array)` → float64, but with a float64 scalar it stays float32 (value-based promotion in 1.26).
- **Exactness.** 2^53 and 2^53+1 both convert to `9.007199254740992D15` in vm and NumPy (rounding to even); R `as.double(2^53+1)` prints `9.007199e+15`.
- **R integer coercion.** `as.integer(2^31)` → `NA` with a warning, so converting back to integers is lossy in R; vm integers do not overflow.

## How this was checked

Every example in this report was run, not taken from documentation. vm results are from CCL unless a row names SBCL; SBCL was run wherever the two might differ (branch cuts, signed zeros, exact square roots, float conversion).

| System | Version |
| --- | --- |
| vectorized-math | [Lisp-Stat/vectorized-math](https://github.com/Lisp-Stat/vectorized-math) `master` at 2b781b9 |
| Clozure CL (primary) | 1.14 with the pull requests listed in the vectorized-math README (v1.14-79-g65004ab4; the signum result rechecked on v1.14-81-g1f49be44, which adds Clozure/ccl#752), Linux x86-64 |
| SBCL (where it differs from CCL) | 2.6.2 |
| R | 4.3.3, base functions only |
| Python | 3.12.3, with NumPy 1.26.4 |

NumPy 1.26 predates NumPy 2.0, which changed scalar type promotion (NEP 50) and added `np.bitwise_count`. Results quoted for NumPy may differ on 2.x where noted.

vectorized-math's intended behaviour is documented in its [README](https://github.com/Lisp-Stat/vectorized-math#readme). The R and NumPy behaviour shown here is what those versions did when run, not what their manuals say.
