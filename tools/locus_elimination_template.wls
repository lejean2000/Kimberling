(* ============================================================
   Template: Eliminate parameters to find locus of a perspector
   Context: Cian's Darboux cubic configuration, 2026-07-12
   ============================================================ *)

Get["/opt/Kimberling/sources/init.wls"];

(* ── Step 1: Compute the symbolic perspector ── *)
(* This is the expensive part — do it ONCE and save if reusing *)

abSym = sr[{x, y, z} /. Solve[
  sr[bPerpendicular[bLine[xB, pP], xA]] . {x, y, z} == 0 && 
  bCircumconicPEq[X[2]] == 0 && x + y + z == 1, {x, y, z}][[2]]];
acSym = sr[{x, y, z} /. Solve[
  sr[bPerpendicular[bLine[xC, pP], xA]] . {x, y, z} == 0 && 
  bCircumconicPEq[X[2]] == 0 && x + y + z == 1, {x, y, z}][[2]]];
{abSym, bcSym, caSym} = symtr[abSym];
{acSym, baSym, cbSym} = symtr[acSym];
trgSym = sr /@ bIntersectionTriangle @@ symtr[bLine[bcSym, cbSym] // sr];
persSym = Factor[Simplify[sr[bTrianglePerspector[trgSym, triangle["ABC"]]]]];

(* ── Step 2: Check if the perspector factors ── *)
(* FactorList gives {{factor, exponent}, ...} with sign/numeric factor first *)
(* This can dramatically simplify elimination if coordinates share factors *)

Print["Factor structure:"];
Do[
  fl = FactorList[persSym[[i]]];
  Print["pers[", i, "] has ", Length[fl], " factors (including numeric)"];
  nonConst = Select[fl, !NumericQ[#[[1]]] &];
  Print["  Non-constant factors: ", Length[nonConst], 
    ", sizes: ", ByteCount /@ nonConst[[All, 1]]];
  , {i, 1, 3}];

(* In our case: pers = {FA*FB, -FC*FD, FB*FD} *)
(* Extract the four factors by identifying which are shared *)
p1f = FactorList[persSym[[1]]];
p2f = FactorList[persSym[[2]]];
p3f = FactorList[persSym[[3]]];

(* Keep only polynomial (non-numeric) factors *)
(* Result: list of {factor, exponent} pairs, NOT flat *)
p1p = Select[p1f, !NumericQ[#[[1]]] &];  (* {{FA,1}, {FB,1}} *)
p2p = Select[p2f, !NumericQ[#[[1]]] &];  (* {{FC,1}, {FD,1}} *)
p3p = Select[p3f, !NumericQ[#[[1]]] &];  (* {{FB,1}, {FD,1}} *)

(* To access a factor polynomial: p1p[[i, 1]] — index into pair, then first element *)

(* Identify shared factors by pairwise equality *)
Print["\nShared factor detection:"];
For[i = 1, i <= 2, i++,
  For[j = 1, j <= 2, j++,
    diff = Simplify[p1p[[i, 1]] - p3p[[j, 1]]];
    If[diff === 0, Print["  p1[", i, "] == p3[", j, "] (shared FB)"]];
    diff2 = Simplify[p2p[[i, 1]] - p3p[[j, 1]]];
    If[diff2 === 0, Print["  p2[", i, "] == p3[", j, "] (shared FD)"]];
  ];
];

(* ── Step 3: Set up elimination equations ── *)

(* After factoring, the perspector in barycentrics is:
   Q = {FA*FB, -FC*FD, FB*FD}  ∝  {FA/FD, -FC/FB, 1}

   So the locus condition is:
     X*FD - Z*FA = 0    (from X:Z = FA:FD)
     Y*FB + Z*FC = 0    (from Y:Z = -FC:FB)

   Together with the curve constraint on P = {u,v,w}:
     Darboux(u,v,w) = 0

   Variables to eliminate: {u, v, w}
   Variables to keep: {X, Y, Z} + side lengths {a, b, c}
*)

(* Get the curve constraint *)
darboux = getTriangleCurve["K004"] // curveSimplify;

(* Build elimination equations *)
(* pP = {u,v,w} is already defined globally *)
(* After shared factor detection, assign explicitly:
   p1p = {{FA,1}, {FB,1}}  — whichever order FactorList gave
   p2p = {{FC,1}, {FD,1}}
   p3p = {{FB,1}, {FD,1}}
   Find which index corresponds to which shared factor: *)

(* For our case, we found: p1[1]==p3[1]=FB, p2[2]==p3[2]=FD *)
(* So: FB = p3p[[1,1]], FD = p3p[[2,1]], FA = p1p[[2,1]], FC = p2p[[1,1]] *)

FB = p3p[[1, 1]];
FD = p3p[[2, 1]];
FA = If[Simplify[p1p[[1,1]] - FB] === 0, p1p[[2,1]], p1p[[1,1]]];
FC = If[Simplify[p2p[[1,1]] - FD] === 0, p2p[[2,1]], p2p[[1,1]]];

(* Verify — use PossibleZeroQ since factors may carry signs *)
Print["\nFactor verification:"];
v1 = Simplify[persSym[[1]] + FA*FB];
Print["pers[1] + FA*FB zero? ", PossibleZeroQ[v1], " (Simplify BC=", ByteCount[v1], ")"];
v2 = Simplify[persSym[[2]] + FC*FD];
Print["pers[2] + FC*FD zero? ", PossibleZeroQ[v2], " (Simplify BC=", ByteCount[v2], ")"];
v3 = Simplify[persSym[[3]] - FB*FD];
Print["pers[3] - FB*FD zero? ", PossibleZeroQ[v3], " (Simplify BC=", ByteCount[v3], ")"];

(* If any fail, try swapping FA<->FB or FC<->FD *)
If[!PossibleZeroQ[v1],
  Print["Trying FA <-> FB swap..."];
  {FA, FB} = {FB, FA};
  v1 = Simplify[persSym[[1]] + FA*FB];
  Print["  After swap: zero? ", PossibleZeroQ[v1]];
];
If[!PossibleZeroQ[v2],
  Print["Trying FC <-> FD swap..."];
  {FC, FD} = {FD, FC};
  v2 = Simplify[persSym[[2]] + FC*FD];
  Print["  After swap: zero? ", PossibleZeroQ[v2]];
];

eq1 = Numerator[Together[X * FD - Z * FA]];  (* X*FD - Z*FA = 0 *)
eq2 = Numerator[Together[Y * FB + Z * FC]];  (* Y*FB + Z*FC = 0 *)
eq3 = darboux /. {x -> u, y -> v, z -> w};

(* ── Step 4: Check feasibility ── *)
Print["\n=== Feasibility diagnostics ==="];
Print["eq1 degree: u=", Exponent[eq1, u], " v=", Exponent[eq1, v], 
  " w=", Exponent[eq1, w], "  ByteCount=", ByteCount[eq1]];
Print["eq2 degree: u=", Exponent[eq2, u], " v=", Exponent[eq2, v], 
  " w=", Exponent[eq2, w], "  ByteCount=", ByteCount[eq2]];
Print["eq3 degree: u=", Exponent[eq3, u], " v=", Exponent[eq3, v], 
  " w=", Exponent[eq3, w], "  ByteCount=", ByteCount[eq3]];
Print["Total system: ", ByteCount[{eq1, eq2, eq3}], " bytes"];

(* Heuristic: if total system > ~200KB, elimination is likely infeasible *)
(* In our case: ~170KB system, but resultant was 35MB → infeasible *)

(* ── Step 5: Three elimination strategies (in order of preference) ── *)

(* Strategy A: GröbnerBasis with EliminationOrder
   - Most general, but most memory-intensive
   - Only attempt if total ByteCount < ~100KB *)
(*
result = GroebnerBasis[{eq1, eq2, eq3}, {X, Y, Z, a, b, c}, {u, v, w},
  MonomialOrder -> EliminationOrder, 
  CoefficientDomain -> RationalFunctions];
*)

(* Strategy B: Sequential resultants
   - Eliminate variables one at a time
   - Check size of each intermediate resultant before continuing *)
(*
r12 = Resultant[eq1, eq2, u];
Print["Resultant[eq1,eq2,u]: ", ByteCount[r12], " bytes"];
If[ByteCount[r12] < 5000000,  (* 5MB threshold *)
  r123 = Resultant[r12, eq3, v];
  Print["Resultant[r12,eq3,v]: ", ByteCount[r123], " bytes"];
  (* ... continue eliminating ... *)
,
  Print["Resultant too large, try different variable order"];
];
*)

(* Strategy C: Use the conic condition to eliminate one variable first
   If there's a geometric relationship like "Q lies on circumconic(H,P)",
   you get a linear equation in u,v,w that eliminates one variable cheaply *)
(*
conicCond = bCircumconicEq[X[4], pP] /. {x -> X, y -> Y, z -> Z};
(* conicCond is linear in u,v,w *)
wSol = Solve[conicCond == 0, w][[1, 1, 2]];  (* express w via u,v,X,Y,Z *)
(* Substitute into everything else — may blow up! *)
*)

(* ── Step 6: If symbolic elimination fails, go numerical ── *)
(*
persNum = Simplify[persSym /. rule69];  (* fix a=6,b=9,c=13 *)

sampleLocusPts = {};
(* Sample P on the curve by intersecting with random lines *)
o = N[sr[X[3] /. rule69]];
For[attempt = 1, attempt <= 500 && Length[sampleLocusPts] < 50, attempt++,
  dir = {RandomReal[{-1,1}], RandomReal[{-1,1}], RandomReal[{-1,1}]};
  eqLine = (darboux /. rule69) /. {x->o[[1]]+t*dir[[1]], y->o[[2]]+t*dir[[2]], z->o[[3]]+t*dir[[3]]};
  sols = NSolve[eqLine == 0, t, WorkingPrecision -> 50];
  (* ... evaluate persNum at each solution ... *)
];

(* Then fit a curve through the points:
   - Determine degree by intersecting with random lines (count intersections)
   - Set up linear system for coefficients
   - Solve for the curve equation
   - Verify with extra sample points
*)
*)

Print["\nDone. See comments for the three elimination strategies."];
