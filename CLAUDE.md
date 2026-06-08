# Wolfram Triangle Center Tools — Developer Guide

## Project purpose

This is a Wolfram Mathematica/wolframscript library for numerically investigating properties of triangle centers from Kimberling's Encyclopedia of Triangle Centers (ETC). The core workflow is: given a symbolic barycentric expression for a candidate triangle center, identify all its relationships to known ETC entries (lines it lies on, conjugates, circumconics, curves, reflections, etc.) so the result can be submitted to ETC.

ETC lives at https://faculty.evansville.edu/ck6/encyclopedia/etc.html. Centers are named X(n); the database currently covers up to ~67400.

---

## Repository layout

```
sources/        .m source files (load these)
db/             .mx binary databases (load these)
var/            working notebooks (PointChecker.nb is the main interactive UI)
examples/       example notebooks
ctr/            web UI (separate concern)
```

### Source files and load order

The canonical load order is in `sources/init.wls` (Linux) / `sources/initwin.wls` (Windows):

```mathematica
Get["sources/KimberlingPoints.m"]    (* X[k], KimberlingCenter[], rule69, checkPointinETC2 *)
Get["sources/TriangleTools.m"]       (* all bXxx[] barycentric geometry primitives *)
Get["db/ETC.mx"]                     (* ETCFull, ETC — symbolic formulas, keyed "X1".."X67400" *)
Get["db/ETCBaryNorm.mx"]             (* ETCBaryNorm — 35-digit numeric normalized bary, same keys *)
Get["db/TriangleCurves.mx"]          (* TriangleCurves association *)
Get["db/CentralCircles.mx"]          (* CentralCircles association *)
Get["sources/fltCircles.m"]          (* fltCentralCircles, fltCircumCircles, fltInCircles *)
Get["sources/TriangleCurves.m"]      (* curve helpers *)
Get["db/KimberlingTrianglesBary.mx"] (* precomputed triangle data *)
Get["sources/TriangleExpressions.m"] (* evaluate[], symmetrizeInternal[], NormalizeBary[], etc. *)
Get["sources/DBTools.m"]             (* pointChecker[] and the full identification pipeline *)
Get["db/KimberlingTrianglesBaryOrthKeys.mx"]
(* then merge extra points: *)
Get["db/ETCExtra.mx"]
Get["db/ETCExtraBary.mx"]
ETCFull = ETC;
ETCBaryNormFull = ETCBaryNorm;
```

After loading, `ETCFull` and `ETCBaryNormFull` are the complete databases including user-added extra points (Y/Z series).

---

## Coordinate system

Everything uses **barycentric coordinates** — 3-vectors `{u, v, w}` relative to triangle vertices A, B, C with side lengths `a, b, c` opposite to them. Points are projective: `{u,v,w}` and `{k*u,k*w,k*w}` are the same point.

**Ideal points** (line at infinity): `Total[{u,v,w}] = 0`. Always check for these before operations that require a finite midpoint (e.g., `bMidpoint`). X(30) is a well-known ideal point.

**Key numeric test triangles:**
- `rule69 = {a->6, b->9, c->13}` — used for initial numeric identification
- `intCheckList = {{a->5,b->6,c->8}, {a->4,b->11,c->13}}` — two independent triangles used for algebraic verification of candidates found numerically

Two-triangle verification is the standard pattern for avoiding false positives.

---

## Key functions by file

### TriangleExpressions.m

**`evaluate[expr]`** — expands Conway notation and trigonometric shorthands into pure `a,b,c` expressions. Must be called on any expression before numerical substitution. Defines: `S = 2*area`, `SA = (b²+c²-a²)/2`, `SB`, `SC`, `SW`, `R` (circumradius), `r` (inradius), `angleA/B/C`.

**`NormalizeBary[v]`** — L2-normalizes a bary vector, fixing sign on first coordinate.

**`symmetrizeInternal[expr]`** — cyclic symmetrization A→B→C→A; produces the canonical first coordinate of a triangle center expression.

**`symmetrizeInternal2[expr]`** — variant that clears `p,q,r,u,v,w` before symmetrizing (use when the expression involves these symbols).

**`intnumericnorm[v]`** — `N[NormalizeBary[v], 35]`; the standard way to get a high-precision numeric bary point.

**`simplifyRationalBarycentrics[expr]`** — simplifies rational barycentric expressions; used throughout for intermediate results.

### TriangleTools.m — barycentric geometry primitives

All functions prefixed `b` work in barycentric. Key ones:

| Function | What it does |
|---|---|
| `bLine[P, Q]` | Line through two points → `{l1,l2,l3}` coefficients |
| `bLineIntersection[l1, l2]` | Intersection of two lines |
| `bMidpoint[P, Q]` | Midpoint (normalized). **Breaks on ideal points** — check `Abs[Total[P]] > 0` first |
| `bPerpendicular[line, pt]` | Line perpendicular to `line` passing through `pt`. Uses `a,b,c` symbolically — apply `/. rule69` to the result |
| `bReflectionPL[pt, {l1,l2,l3}]` | Reflection of `pt` in line. **Reintroduces `a,b,c` symbolically** — always apply `/. rc` after calling |
| `bReflectionPP[pt, center]` | Reflection of `pt` in a point |
| `bDistance[P, Q]` | Euclidean distance in bary |
| `bHarmonicConjugate[A, B, P]` | Harmonic conjugate of P w.r.t. A, B |
| `bIsogonalConjugate[P]` | `{a²vw, b²uw, c²uv}` |
| `bIsotomicConjugate[P]` | `{vw, uw, uv}` |
| `bCollinearityMatrix[A,B,C]` | Det of 3×3 matrix; 0 iff collinear |
| `bComplement[G, P]` | Complement of P w.r.t. centroid G |
| `bAntiComplement[G, P]` | Anticomplement |
| `coincide[P, Q, prec:20]` | Numeric coincidence via Cross product, tolerance `10^(-prec)` |
| `coincideNorm[P, Q, prec:20]` | `coincide` after `intnumericnorm` — the standard verification tool |

**Important:** `bPerpendicular` and `bReflectionPL` contain `a,b,c` in their output even when inputs are numeric. Always chain `/.rule69` or `/.rc` after them.

### KimberlingPoints.m

**`X[k]`** — returns the symbolic bary expression for X(k). Calls `evaluate` and `symmetrizeInternal` internally.

**`KimberlingCenterC[k]`** — same as `X[k]` but accepts either integer or string key.

**`KimberlingCenterCNy[key]`** — same but takes string key directly (e.g. `"X1"`). Used heavily in verification loops inside DBTools.

**`rule69 = {a->6, b->9, c->13}`** — the standard quick-check triangle.

**`checkPointinETC2[ptcoord]`** — checks if a symbolic point is already in `ETCBaryNorm` (numeric lookup + two-triangle algebraic confirmation). Returns list of matching keys, or `{}`.

**`checkPointsOnCurve[crv]`** — find all ETC points on a given curve equation.

### DBTools.m — identification pipeline

The main file. See the pipeline section below.

---

## The `pointChecker` pipeline

```mathematica
pointChecker[expr_, num_:0, full_:False, inname_:"X"]
```

- `expr` — symbolic barycentric 3-vector (the candidate center)
- `num` — ETC number to exclude from the database (0 = nothing excluded)
- `full` — force full analysis even if too few lines found
- `inname` — key under which results are stored in `globalProperties`

**Pipeline steps** (in order):

1. **Guard**: refuses duplicate `globalProperties` keys.
2. **`checkCentralExpression`**: validates symmetry (cyclic + `b↔c` antisymmetry + homogeneity under scaling).
3. **`globalSeenPoints` deduplication**: bails if numerically identical to a previously processed point in this session.
4. **`checkPointinETC2`**: if already in ETC, prints the key and returns `False` (nothing to do).
5. **`linesProcessAlg`**: finds collinear ETC triples, computes the name, barycentrics string, harmonic conjugates, midpoints, point-reflections, **line-reflections** (new), linear combinations. This is the most expensive step. If fewer than `pointCheckerMinProperties` (=3) lines are found and `full=False`, the rest is skipped.
6. **`checkCircumconics`**: circumconics through A,B,C,Xi,Xj containing the point.
7. **`checkCurves`**: membership in named curves and circles.
8. **`checkInverse`**: inverse in central circles.
9. **`checkTrilinearPolar`**: is the point a trilinear pole?
10. **`checkIsogonalConjugates`**: P-isogonal conjugate pairs.
11. **`checkConjugates`** (×5): Dao, Ceva, anticomplementary, complementary, cross conjugates.
12. **`checkVertexConjugates`**.
13. **`checkCircles`**: is the point equidistant from three ETC points?
14. **`checkInconics`**: inscribed conics with ETC perspectors.
15. **`checkPoles`** (180 s cap): pole of a line w.r.t. circles/conics.
16. **`checkPerspector`** / **`checkConicCenter`**: circumconic perspector/center.
17. **`checkBarycentric`**: barycentric product/quotient pairs.
18. **`pointCheckAllProcesses`** (90 s cap): single-point transforms (isogonal, isotomic, complement, anticomplement, zosma, eigentransform, cyclocevian, orthoassociate, antitomic, syngonal conjugates).

All results accumulate in `globalProperties[inname]` and print live (unless `globalSilence = True`).

### Related entry points

```mathematica
pointCheckerTransform[expr, inname, num:0, full:False]
(* runs pointChecker on the point AND on 11 standard transforms *)

quickChecker[expr, num:0, curvescheck:True, ...]
(* fast: only lines + circumconics, returns {lineCount, circumconicCount, rawLineCount} *)
(* does NOT run the line-reflections or any If[!quick,...] block *)

quickCheckerTransform[expr, inname:"PTX", num:0]
(* quickChecker on the point and all transforms *)

addExtraPoint[bary, letter, name, writeout:True]
(* registers a new non-ETC point; letter is "Y" or "Z"; persists to ETCExtra.mx *)
```

---

## Global state variables

| Variable | Default | Purpose |
|---|---|---|
| `globalProperties` | `<\|>` | Accumulates all results; key = `inname` arg |
| `globalSilence` | (unset/False) | Suppress all Print output |
| `globalSeenPoints` | `{}` | Dedup list for current session; set to `False` to disable |
| `globalExcludedNum` | `0` | ETC key excluded from scans (set by `pointChecker`) |
| `globalNoCleanup` | `True` | If True, replace Z/Y symbols in output instead of stripping |
| `colorPrintOn` | (unset) | Enable colored output for Y/Z points |
| `globalCheckAllPoles` | `False` | Extend pole check to all circumconics and inconics |
| `pointCheckerMinProperties` | `3` | Minimum lines required to proceed past `linesProcessAlg` |

**Important:** `globalProperties` persists across `pointChecker` calls in a session. Clear it with `globalProperties = <||>` when starting fresh. `globalSeenPoints` similarly accumulates; clear with `globalSeenPoints = {}`.

---

## Database structure

### `ETCBaryNorm` / `ETCBaryNormFull`
Association of string key → 35-digit numeric normalized barycentric 3-vector.
```
"X1" -> {0.354..., 0.384..., 0.261...}
```
`ETCBaryNorm` = base ETC only. `ETCBaryNormFull` = base + extra (Y/Z) points.

### `ETCFull` / `ETC`
Association of string key → symbolic barycentric expression (first coordinate only; the other two follow by symmetry).

### `ETCExtra` / `ETCExtraBary`
User-defined extra points with `"Y"` or `"Z"` prefix keys.

### `NonETCNames`
Association of extra point key → human-readable name string.

---

## Common patterns

### Evaluate a center numerically
```mathematica
ptc = intnumericnorm[evaluate[X[1]] /. rule69]
```

### Check if a point is in ETC
```mathematica
checkPointinETC2[evaluate[myExpr]]
```

### Verify two points coincide (standard two-triangle pattern)
```mathematica
rc = intCheckList[[1]]; rc2 = intCheckList[[2]];
coincideNorm[someFunc[...] /. rc, ptcoord /. rc] &&
coincideNorm[someFunc[...] /. rc2, ptcoord /. rc2]
```

### Run a quick test on a function before full pointChecker
```mathematica
ptc = intnumericnorm[evaluate[myExpr] /. rule69]
AbsoluteTiming[intReflectionInLineProcess[ptc, 20, KeyDrop[ETCBaryNorm, "X0"]]]
```
To cap the scan for speed testing: `Take[ETCBaryNorm, 1000]`.

### Batch output to file (silent mode)
```mathematica
globalSilence = True;
globalOutputStream = OpenWrite["output.txt"];
pointChecker[expr];
printGlobalProperties[globalProperties, "X"];
Close[globalOutputStream];
globalOutputStream = False;
```

---

## Known gotchas and lessons learned

### 1. `bPerpendicular` and `bReflectionPL` reintroduce `a,b,c`
Both functions use `SA = (b²+c²-a²)/2` etc. internally, so even when given fully numeric inputs they produce symbolic output containing `a, b, c`. Always append `/. rule69` (or `/. rc`) after calling them:
```mathematica
L = N[bPerpendicular[bLine[ptc, xiBary], bMidpoint[ptc, xiBary]] /. rule69, 35]
refl = bReflectionPL[KimberlingCenterCNy["X1"] /. rc, bLine[...] /. rc] /. rc
```

### 2. Ideal points break `bMidpoint`
Points on the line at infinity have `Total[coords] ≈ 0`. `bMidpoint` divides by `Total`, giving `0/0`. This propagates silently and can produce a near-zero perpendicular bisector that matches every ETC point. Always guard:
```mathematica
If[Abs[Total[xiBary]] < 10^(-10), Continue[]]
```

### 3. Module variable names cannot contain underscores
`xi_bary` is parsed by Mathematica as the pattern `Pattern[xi, Blank[bary]]`, not a symbol. Use camelCase: `xiBary`.

### 4. `TrueQ[Simplify[CrossProduct == {0,0,0}]]` is unreliable
Mathematica may simplify `{expr1,expr2,expr3} == {0,0,0}` to `{True,True,True}` (a list), and `TrueQ[{True,True,True}]` returns `False`. Use `coincideNorm` for verification instead — it applies `intnumericnorm` and checks via `Cross` with numeric tolerance.

### 5. `numsortexpr` expects full keys like `"X1"`, not bare numbers
`intnameformat` strips the `"X"` prefix for display. The scan functions return full keys (`"X1"`); `intnameformat` is applied only at the final output stage.

### 6. `quickChecker` skips the `If[!quick,...]` block
The harmonic conjugate, midpoint, reflection, and line-reflection checks all live inside `If[!quick, ...]` in `linesProcessAlg`. `quickChecker` passes `quick=True` and only gets the line count.

---

