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
      Simplify[out/PolynomialGCD @@ out]]
 
ggCurve[expr_] := Print[StringJoin["TriangleCurve(A,B,C,", 
      ExpressionToTrad[multiCollect[expr /. Thread[{u, v, w} -> {A, B, C}] /. 
         Thread[{x, y, z} -> {A, B, C}], {A, B, C}]], "=0)"]]
 
ggBaryToCartesian[p_] := Simplify[bToCartesian[p, {ax, ay}, {bx, by}, 
       {cx, cy}]] /. {ax -> "x(A)", bx -> "x(B)", cx -> "x(C)", ay -> "y(A)", 
      by -> "y(B)", cy -> "y(C)"}
 
homogeneousPart[poly_, vars_, deg_] := Module[{intt}, 
     SeriesCoefficient[poly /. Thread[vars -> intt*vars], {intt, 0, deg}] /. 
      intt -> 1]
 
simplifyFactors[expr_] := Times @@ (#1[[1]]^#1[[2]] & ) /@ 
      (Simplify[#1] & ) /@ FactorList[expr]
