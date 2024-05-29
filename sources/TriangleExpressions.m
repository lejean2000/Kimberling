polynomialDegree[poly_] := Max[Cases[CoefficientRules[poly], 
      (v_)?VectorQ :> Total[v], 2]]
 
countSummands[expr_] := If[Head[Expand[expr]] === Plus, Length[Expand[expr]], 
     If[expr === 0, 0, 1]]
 
symmetrizeEq[eq_] := Module[{partB, partC}, 
     partB = eq /. {a -> b, b -> c, c -> a, x -> y, y -> z, z -> x, pp -> qq, 
         qq -> rr, rr -> pp, A -> B, B -> C, C -> A, SA -> SB, SB -> SC, 
         SC -> SA, p -> q, q -> r, r -> p, u -> v, v -> w, w -> u}; 
      partC = partB /. {a -> b, b -> c, c -> a, x -> y, y -> z, z -> x, 
         pp -> qq, qq -> rr, rr -> pp, A -> B, B -> C, C -> A, SA -> SB, 
         SB -> SC, SC -> SA, p -> q, q -> r, r -> p, u -> v, v -> w, w -> u}; 
      eq + partB + partC]
 
conwaySimplify[poly_] := PolynomialReduce[poly, 
     evaluate[{SW, SA, SB, SC, sp, sa, sb, sc}], {a, b, c}]
 
evaluate[expr_] := Module[{qsp, qsa, qsb, qsc, qS, qr, qSA, qSB, qSC, qSW, 
      qR, qjJ, qangleA, qangleB, qangleC, qangleW, qTT, qJJ}, 
     Clear[sp, sa, sb, sc, S, r, SA, SB, SC, SW, R, jJ, angleA, angleB, 
       angleC, angleW, \[Omega], e, \[Tau], jJ, J, dD, \[CapitalDelta]]; 
      qsp = (a + b + c)/2; qsa = (b + c - a)/2; qsb = (a - b + c)/2; 
      qsc = (a + b - c)/2; qS = 2*Sqrt[qsp*qsa*qsb*qsc]; qr = S/(2*qsp); 
      qSA = (b^2 + c^2 - a^2)/2; qSB = (a^2 - b^2 + c^2)/2; 
      qSC = (a^2 + b^2 - c^2)/2; qSW = (a^2 + b^2 + c^2)/2; 
      qR = a*b*(c/(2*qS)); qangleA = ArcCos[qSA/(b*c)]; 
      qangleB = ArcCos[qSB/(a*c)]; qangleC = ArcCos[qSC/(a*b)]; 
      qangleW = ArcCot[Cot[qangleA] + Cot[qangleB] + Cot[qangleC]]; 
      qjJ = (1/(a*b*c))*Sqrt[a^6 + b^6 + c^6 - a^4*b^2 - a^2*b^4 - a^4*c^2 - 
          a^2*c^4 - c^4*b^2 - c^2*b^4 + 3*a^2*b^2*c^2]; 
      qTT = Sqrt[a^4 - a^2*b^2 + b^4 - a^2*c^2 - b^2*c^2 + c^4]; 
      qJJ = Sqrt[-2*a^8 + 3*a^6*b^2 - 2*a^4*b^4 + 3*a^2*b^6 - 2*b^8 + 
         3*a^6*c^2 - 2*a^4*b^2*c^2 - 2*a^2*b^4*c^2 + 3*b^6*c^2 - 2*a^4*c^4 - 
         2*a^2*b^2*c^4 - 2*b^4*c^4 + 3*a^2*c^6 + 3*b^2*c^6 - 2*c^8 + 
         2*a^2*b^2*c^2*J^2*Sqrt[a^4 - a^2*b^2 + b^4 - a^2*c^2 - b^2*c^2 + 
            c^4]]; qOH = Sqrt[(-a^6 + a^4*(b^2 + c^2) - 
          (b^2 - c^2)^2*(b^2 + c^2) + a^2*(b^4 - 3*b^2*c^2 + c^4))/
         (a^4 + (b^2 - c^2)^2 - 2*a^2*(b^2 + c^2))]; 
      Return[expr /. {sp -> (a + b + c)/2, sa -> (b + c - a)/2, 
         sb -> (a - b + c)/2, sc -> (a + b - c)/2, s -> qsp, S -> qS, 
         r -> qS/(2*qsp), \[CapitalDelta] -> qS/2, SA -> qSA, SB -> qSB, 
         SC -> qSC, SW -> qSW, R -> qR, angleA -> qangleA, angleB -> qangleB, 
         angleC -> qangleC, angleW -> qangleW, A -> qangleA, B -> qangleB, 
         C -> qangleC, \[Omega] -> qangleW, 
         e -> Sqrt[(a^4 - a^2*b^2 + b^4 - a^2*c^2 - b^2*c^2 + c^4)/
            (a^2*b^2 + a^2*c^2 + b^2*c^2)], \[Tau] -> 
          Sqrt[(1/2)*(3 + Sqrt[5])], dD -> 4*qS*Sqrt[qR*(4*qR + qr)], 
         OH -> qOH, jJ -> qjJ, J -> qjJ}]]
 
simplifyRationalBarycentrics[expr_] := Module[{out}, 
     out = Factor[expr*PolynomialLCM @@ Denominator[expr]]; 
      intSimplifyFactors /@ (out/PolynomialGCD @@ out)]
 
intSimplifyFactors[expr_] := Times @@ (#1[[1]]^#1[[2]] & ) /@ 
      (Simplify[#1] & ) /@ FactorList[expr]
 
ggCurve[expr_] := Print[StringJoin["TriangleCurve(A,B,C,", 
      ExpressionToTrad[multiCollect[expr /. Thread[{u, v, w} -> {A, B, C}] /. 
         Thread[{x, y, z} -> {A, B, C}], {A, B, C}]], "=0)"]]
 
ggBaryToCartesian[p_] := Simplify[bToCartesian[p, {ax, ay}, {bx, by}, 
       {cx, cy}]] /. {ax -> "x(A)", bx -> "x(B)", cx -> "x(C)", ay -> "y(A)", 
      by -> "y(B)", cy -> "y(C)"}
 
homogeneousPart[poly_, vars_, deg_] := Module[{intt}, 
     SeriesCoefficient[poly /. Thread[vars -> intt*vars], {intt, 0, deg}] /. 
      intt -> 1]
 
heuristicsCheck[expr_, degree_:16, ratio_:5, cfsum_:200] := 
    Module[{deg, smt, coefsum}, 
     If[ !PolynomialQ[expr, a] ||  !PolynomialQ[expr, b], Return[False]]; 
      deg = (Max[Apply[Plus, CoefficientRules[#1][[All,1]], {1}]] & )[expr]; 
      smt = Total[Select[(1 + countSummands[#1[[1]]] & ) /@ FactorList[expr], 
         #1 > 3 & ]]; coefsum = Total[Flatten[
         Abs[(CoefficientList[#1, {a, b, c, S}] & ) /@ FactorList[expr]]]]; 
      Return[deg <= 5 || (deg <= degree && smt/deg < ratio && 
         coefsum < cfsum)]; ]
 
partialSReplace[expr_] := Module[{exp, exp2, smpl}, 
     If[ListQ[expr], exp2 = simplifyRationalBarycentrics[expr], 
       exp2 = Simplify[expr]]; exp = Expand[exp2 /. \[CapitalDelta] -> S/2]; 
      exp = Simplify[exp /. S^2 -> evaluate[S^2] /. S^4 -> evaluate[S^4] /. 
               S^3 -> S*evaluate[S^2] /. S^5 -> S*evaluate[S^4] /. 
             S^6 -> evaluate[S^6] /. S^7 -> S*evaluate[S^6] /. 
           S^8 -> evaluate[S^8] /. S^9 -> S*evaluate[S^8] /. 
         S^10 -> evaluate[S^10]]; If[ !ListQ[expr], Return[exp]]; 
      smpl = ssimplify[exp]; Return[smpl]; ]
 
ssimplify[pt_] := Module[{ss}, 
     ss[ptn_] := (Activate[Collect[#1, S, Inactive[Simplify]] /. 
           Simplify -> intFullSimplifyFactors] & ) /@ ptn; 
      Return[simplifyRationalBarycentrics[
        ss[ss[pt] /. (a - b - c)*(a + b - c)*(a - b + c)*(a + b + c) -> -4*
                S^2 /. (a + b - c)*(a - b + c)*(-a + b + c)*(a + b + c) -> 
              4*S^2 /. -3*a^4 - 3*(b^2 - c^2)^2 + 6*a^2*(b^2 + c^2) -> 
             12*S^2 /. -((a - b - c)*(a + b - c)*(a - b + c)*(a + b + c)) -> 
            4*S^2 /. a^4 + (b^2 - c^2)^2 - 2*a^2*(b^2 + c^2) -> -4*S^2]]]; ]
 
intFullSimplifyFactors[expr_] := Times @@ (#1[[1]]^#1[[2]] & ) /@ 
      (FullSimplify[#1, S > 0] & ) /@ FactorList[expr]
 
fareySet[n_] := Quiet[Join[Select[Union[FareySequence[n], 
        1/FareySequence[n]], #1 =!= ComplexInfinity && #1 > 0 & ], 
      -Select[Union[FareySequence[n], 1/FareySequence[n]], 
        #1 =!= ComplexInfinity && #1 > 0 & ]]]
 
partialSAconvert[ex_] := 
    ex /. SA -> evaluate[SA] /. SB -> evaluate[SB] /. SC -> evaluate[SC] /. 
        SW -> evaluate[SW] /. sa -> evaluate[sa] /. sb -> evaluate[sb] /. 
     sc -> evaluate[sc]
 
leastBaryFromIntersections[testset_] := 
    Module[{results, tt, out, mon, min, deg}, 
     min = 100; Monitor[results = {}; 
        Do[AbortProtect[CheckAbort[mon = {nx1, nx2}; If[nx1[[1]] >= nx2[[1]], 
               Continue[]]; tt = TimeConstrained[partialSReplace[
                 Expand[Simplify[ssimplify[bLineIntersection[bLine[
                       partialSAconvert[KimberlingCenterC[nx1[[1]]]], 
                       partialSAconvert[KimberlingCenterC[nx1[[2]]]]], 
                      bLine[partialSAconvert[KimberlingCenterC[nx2[[1]]]], 
                       partialSAconvert[KimberlingCenterC[nx2[[2]]]]]]][[
                    1]]]]], 5, -1]; If[tt == -1, Continue[]]; 
              deg = polynomialDegree[tt]; If[deg > 0, AppendTo[results, 
                 {nx1, nx2, tt, deg}]; If[deg < min, Print["New min:"]; 
                  min = deg; Print[{nx1, nx2, deg}]]; ]; , 
             Return[SortBy[results, #1[[4]] & ], Module]]; ]; , 
         {nx1, testset}, {nx2, testset}], mon]; 
      out = SortBy[results, #1[[4]] & ]; Print[out[[1]]]; 
      Print[ExpressionToTrad[out[[1]][[3]]]]; Return[out]; ]
 
replacer[expr_, nu_, np_:0] := simplifyRationalBarycentrics[
     expr /. Thread[{u, v, w} -> simplifyRationalBarycentrics[
          evaluate[KimberlingCenterC[nu]]]] /. 
      Thread[{p, q, r} -> simplifyRationalBarycentrics[
         evaluate[KimberlingCenterC[np]]]]]
 
massHeuristics1[expr_, nmin_, nmax_, deg_:16, ratio_:4.5, docurves_:False] := 
    Module[{out, etc, testexpr, check, curves}, 
     If[docurves, globalSilence = True]; 
      Quiet[Monitor[out = {}; etc = {}; Do[nprg = nx; curves = {}; 
           If[ !MemberQ[Keys[ETC], StringJoin["X", ToString[nx]]], 
            Continue[]]; If[ !RationalExpressionQ[KimberlingCenterCN[nx][[
               1]], {a, b, c, S}], Continue[]]; testexpr = 
            TimeConstrained[simplifyRationalBarycentrics[replacer[expr, nx]], 
             10, -1]; If[testexpr == -1, Continue[]]; 
           check = checkPointinETC2[testexpr]; 
           If[docurves && Length[check] == 0, curves = 
             Quiet[checkCurves[testexpr]]]; If[Length[check] > 0, 
            AppendTo[etc, {nx, check[[1]]}], If[heuristicsCheck[
              partialSAconvert[testexpr[[1]]], deg, ratio], 
             If[Length[curves] > 0, AppendTo[out, {nx, curves}], AppendTo[
                out, nx]]; ]]; , {nx, nmin, nmax}], nprg]]; Print[out]; 
      Print[colorformat[ToString[etc]]]; globalSilence = False; 
      Return[{out, etc}]; ]
 
massHeuristicsFarey[expr_, fset_, deg_:16, ratio_:4.5] := 
    Module[{out, etc, testexpr, check}, 
     Quiet[Monitor[out = {}; etc = {}; Do[nprg = tvar; 
           testexpr = TimeConstrained[simplifyRationalBarycentrics[
              expr /. t -> tvar], 10, -1]; If[testexpr == -1, Continue[]]; 
           check = checkPointinETC2[evaluate[testexpr]]; 
           If[Length[check] > 0, AppendTo[etc, {ExpressionToTrad[tvar], 
              check[[1]]}], If[heuristicsCheck[testexpr[[1]], deg, ratio], 
             AppendTo[out, tvar]]]; , {tvar, fset}], nprg]]; Print[out]; 
      Print[colorformat[ToString[etc]]]; ]
 
rplux = {u -> x, v -> y, w -> z}
 
setHeuristics[expr_, set_] := Module[{seti, etc, check, ptcoord, out}, 
     etc = {}; out = {}; seti = 
       (StringReplace[#1, {"(" -> "", ")" -> ""}] & ) /@ set; 
      Monitor[Do[AbortProtect[CheckAbort[ptcoord = TimeConstrained[
             simplifyRationalBarycentrics[expr /. Thread[{u, v, w} -> 
                 simplifyRationalBarycentrics[evaluate[KimberlingCenterC[
                    ptn]]]]], 8, -1]; If[ptcoord == -1, 
            Print[StringJoin[ptn, " timeout"]]; Continue[]]; 
           check = checkPointinETC2[ptcoord]; If[Length[check] > 0, 
            AppendTo[etc, {ptn, check[[1]]}], If[heuristicsCheck[evaluate[
                ptcoord[[1]]], 16, 5], AppendTo[out, ptn]]; ], 
          Return[{etc, out}, Module]; ]], {ptn, seti}], ptn]; 
      Return[{etc, out}, Module]; ]
 
removeXYZ[set_] := Module[{out, el1, el2}, 
     out = {}; Do[{el1, el2} = ptn; If[StringStartsQ[ptn[[1]], "Y"] || 
          StringStartsQ[ptn[[1]], "Z"], el1 = NonETCNames[ptn[[1]]]]; 
        If[StringStartsQ[ptn[[2]], "Y"] || StringStartsQ[ptn[[2]], "Z"], 
         el2 = NonETCNames[ptn[[2]]]]; {el1, el2} = 
         (StringReplace[#1, {"X" -> "", "IsotConjg" -> "it", 
             "IsogConj" -> "ig"}] & ) /@ {el1, el2}; 
        AppendTo[out, {el1, el2}]; , {ptn, set}]; Return[out]; ]
 
setToAssociation[set_] := AssociationThread[
     (StringJoin["X(", ToString[#1[[1]]], ")"] & ) /@ set, 
     (intaddbrackets[#1[[2]]] & ) /@ DeleteDuplicatesBy[set, #1[[2]] & ]]
 
replacerExpr[expr_, nu_, np_:0] := 
    expr /. Thread[{u, v, w} -> simplifyRationalBarycentrics[
         evaluate[KimberlingCenterC[nu]]]] /. 
     Thread[{p, q, r} -> simplifyRationalBarycentrics[
        evaluate[KimberlingCenterC[np]]]]
 
replacerNoeval[expr_, nu_, np_:0] := simplifyRationalBarycentrics[
     expr /. Thread[{u, v, w} -> simplifyRationalBarycentrics[
          KimberlingCenterC[nu]]] /. Thread[{p, q, r} -> 
        simplifyRationalBarycentrics[KimberlingCenterC[np]]]]
 
replacerExprNoeval[expr_, nu_, np_:0] := 
    expr /. Thread[{u, v, w} -> simplifyRationalBarycentrics[
         KimberlingCenterC[nu]]] /. 
     Thread[{p, q, r} -> simplifyRationalBarycentrics[KimberlingCenterC[np]]]
 
toUVW[expr_] := expr /. Thread[{x, y, z} -> {u, v, w}] /. 
      Thread[{l, m, n} -> {u, v, w}] /. Thread[{p, q, r} -> {u, v, w}]
 
massHeuristicsSet[expr_, set_, deg_:16, ratio_:4.5, docurves_:False] := 
    Module[{curves, out, etc, testexpr, check, mset}, 
     mset = setRemoveBrackets[set]; If[docurves, globalSilence = True]; 
      Quiet[Monitor[out = {}; etc = {}; 
         Do[nprg = nx; If[ !RationalExpressionQ[KimberlingCenterCN[nx][[1]], 
              {a, b, c}], Continue[]]; testexpr = TimeConstrained[
             simplifyRationalBarycentrics[replacer[expr, nx, nx]], 10, -1]; 
           If[testexpr == -1, Continue[]]; check = checkPointinETC2[
             testexpr]; If[docurves && Length[check] == 0, 
            curves = Quiet[checkCurves[testexpr]]]; If[Length[check] > 0, 
            AppendTo[etc, {nx, check[[1]]}], If[heuristicsCheck[
              evaluate[testexpr[[1]]], deg, ratio], If[Length[curves] > 0, 
               AppendTo[out, {nx, curves}], AppendTo[out, nx]]; ]]; , 
          {nx, mset}], nprg]]; Print[out]; Print[colorformat[ToString[etc]]]; 
      globalSilence = False; Return[{out, etc}]; ]
 
setRemoveBrackets[set_] := (StringReplace[#1, {")" -> "", "(" -> ""}] & ) /@ 
     set
 
massHeuristicsNoEval[expr_, nmin_, nmax_, deg_:16, ratio_:4.5, 
     docurves_:False] := Module[{out, etc, testexpr, check, curves}, 
     If[docurves, globalSilence = True]; 
      Quiet[Monitor[out = {}; etc = {}; Do[nprg = nx; curves = {}; 
           If[ !MemberQ[Keys[ETC], StringJoin["X", ToString[nx]]], 
            Continue[]]; If[ !RationalExpressionQ[KimberlingCenterCN[nx][[
               1]], {a, b, c}], Continue[]]; testexpr = TimeConstrained[
             simplifyRationalBarycentrics[expr /. Thread[pP -> 
                 KimberlingCenterC[nx]]], 10, -1]; If[testexpr == -1, 
            Continue[]]; check = checkPointinETC2[testexpr]; 
           If[docurves && Length[check] == 0, curves = 
             Quiet[checkCurves[testexpr]]]; If[Length[check] > 0, 
            AppendTo[etc, {nx, check[[1]]}], If[heuristicsCheck[
              testexpr[[1]], deg, ratio], If[Length[curves] > 0, AppendTo[
                out, {nx, curves}], AppendTo[out, nx]]; ]]; , 
          {nx, nmin, nmax}], nprg]]; Print[out]; 
      Print[colorformat[ToString[etc]]]; globalSilence = False; 
      Return[{out, etc}]; ]
 
massHeuristicsFareyNoEval[expr_, fset_, deg_:16, ratio_:4.5] := 
    Module[{out, etc, testexpr, check}, 
     Quiet[Monitor[out = {}; etc = {}; Do[nprg = tvar; 
           testexpr = TimeConstrained[simplifyRationalBarycentrics[
              expr /. t -> tvar], 10, -1]; If[testexpr == -1, Continue[]]; 
           check = checkPointinETC2[testexpr]; If[Length[check] > 0, 
            AppendTo[etc, {ExpressionToTrad[tvar], check[[1]]}], 
            If[heuristicsCheck[testexpr[[1]], deg, ratio], AppendTo[out, 
              tvar]]]; , {tvar, fset}], nprg]]; Print[out]; 
      Print[colorformat[ToString[etc]]]; ]
 
ttsimplify[ptn_] := (Activate[Collect[#1, t, Inactive[Simplify]] /. 
        Simplify -> intFullSimplifyFactors] & ) /@ ptn
 
intSimplifyFactorsToTrad[expr_] := StringReplace[ExpressionToTrad[
      With[{ex = (#1[[1]]^#1[[2]] & ) /@ (Simplify[#1] & ) /@ 
           Select[FactorList[expr],  !Abs[#1[[1]]] === 1 & ]}, 
       If[Length[ex] > 1, NonCommutativeMultiply @@ ex, ex[[1]]]]], 
     "**" -> "*"]
 
setSquareBary = {2, 3, 4, 5, 6, 20, 22, 23, 24, 25, 26, 32, 39, 49, 51, 52, 
     53, 54, 64, 66, 67, 68, 69, 70, 74, 76, 83, 93, 95, 96, 97, 98, 111, 
     113, 114, 126, 131, 132, 133, 140, 141, 154, 155, 156, 157, 159, 160, 
     161, 182, 183, 184, 185, 187, 193, 194, 195, 206, 211, 216, 217, 230, 
     231, 232, 233, 235, 237, 248, 251, 262, 263, 264, 275, 276, 287, 290, 
     297, 305, 308, 311, 315, 316, 317, 324, 325, 327, 343, 351, 352, 353, 
     373, 384, 384, 393, 394, 439, 468, 570, 574, 576, 577, 597, 598, 599, 
     625, 626, 669, 670, 671, 684, 689, 691, 694, 695, 699, 703, 729, 737, 
     755, 783, 805, 809, 827, 850, 880, 881, 882, 887, 892, 930, 1003, 1031, 
     1078, 1084}
