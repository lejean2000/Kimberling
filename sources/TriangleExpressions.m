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
      qR, qjJ, qangleA, qangleB, qangleC, qangleW}, 
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
      Return[expr /. {sp -> (a + b + c)/2, sa -> (b + c - a)/2, 
         sb -> (a - b + c)/2, sc -> (a + b - c)/2, s -> qsp, S -> qS, 
         r -> qS/(2*qsp), \[CapitalDelta] -> qS/2, SA -> qSA, SB -> qSB, 
         SC -> qSC, SW -> qSW, R -> qR, angleA -> qangleA, angleB -> qangleB, 
         angleC -> qangleC, angleW -> qangleW, A -> qangleA, B -> qangleB, 
         C -> qangleC, \[Omega] -> qangleW, 
         e -> Sqrt[(a^4 - a^2*b^2 + b^4 - a^2*c^2 - b^2*c^2 + c^4)/
            (a^2*b^2 + a^2*c^2 + b^2*c^2)], \[Tau] -> 
          Sqrt[(1/2)*(3 + Sqrt[5])], jJ -> qjJ, J -> qjJ, 
         dD -> 4*qS*Sqrt[qR*(4*qR + qr)]}]]
 
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
         Abs[(CoefficientList[#1, {a, b, c}] & ) /@ FactorList[expr]]]]; 
      Return[deg <= 5 || (deg <= degree && smt/deg < ratio && 
         coefsum < cfsum)]; ]
 
partialSReplace[expr_] := Module[{exp}, 
     exp = multiCollect[expr /. \[CapitalDelta] -> S/2, S]; 
      exp = Simplify[exp /. S^2 -> evaluate[S^2] /. S^4 -> evaluate[S^4] /. 
               S^3 -> S*evaluate[S^2] /. S^5 -> S*evaluate[S^4] /. 
             S^6 -> evaluate[S^6] /. S^7 -> S*evaluate[S^6] /. 
           S^8 -> evaluate[S^8] /. S^9 -> S*evaluate[S^8] /. 
         S^10 -> evaluate[S^10]]; exp = simplifyRationalBarycentrics[
        symmetrizeInternal[exp]]; multiCollect[exp[[1]], S]]
 
fareySet[n_] := Quiet[Select[Union[FareySequence[n], 1/FareySequence[n]], 
      #1 =!= ComplexInfinity && #1 > 0 & ]]
 
partialSAconvert[ex_] := simplifyRationalBarycentrics[
     ex /. SA -> evaluate[SA] /. SB -> evaluate[SB] /. SC -> evaluate[SC]]
 
leastBaryFromIntersections[testset_] := 
    Module[{results, tt, out, mon, min, deg}, 
     min = 100; Monitor[results = {}; 
        Do[AbortProtect[CheckAbort[mon = {nx1, nx2}; If[nx1[[1]] >= nx2[[1]], 
               Continue[]]; tt = TimeConstrained[partialSReplace[
                 Expand[Simplify[bLineIntersection[bLine[partialSAconvert[
                       KimberlingCenterC[nx1[[1]]]], partialSAconvert[
                       KimberlingCenterC[nx1[[2]]]]], bLine[partialSAconvert[
                       KimberlingCenterC[nx2[[1]]]], partialSAconvert[
                       KimberlingCenterC[nx2[[2]]]]]][[1]]]]], 5, -1]; 
              If[tt == -1, Continue[]]; deg = polynomialDegree[tt[[1]]]; 
              AppendTo[results, {nx1, nx2, tt, deg}]; If[deg < min, 
               Print["New min:"]; min = deg; Print[{nx1, nx2, deg}]]; , 
             Return[SortBy[results, #1[[4]] & ], Module]]; ]; , 
         {nx1, testset}, {nx2, testset}], mon]; 
      out = SortBy[results, #1[[4]] & ]; Print[out[[1]]]; 
      Print[ExpressionToTrad[out[[1]][[3]]]]; Return[out]; ]
