symmetrizeEq[eq_] := Module[{partB, partC}, 
     partB = eq /. {a -> b, b -> c, c -> a, x -> y, y -> z, z -> x, pp -> qq, 
         qq -> rr, rr -> pp}; partC = partB /. {a -> b, b -> c, c -> a, 
         x -> y, y -> z, z -> x, pp -> qq, qq -> rr, rr -> pp}; 
      eq + partB + partC]
 
conwaySimplify[poly_] := PolynomialReduce[poly, {SW, SA, SB, SC, sp, sa, sb, 
      sc}, {a, b, c}]
 
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
         angleC -> qangleC, angleW -> qangleW, \[Omega] -> qangleW, 
         e -> Sqrt[(a^4 - a^2*b^2 + b^4 - a^2*c^2 - b^2*c^2 + c^4)/
            (a^2*b^2 + a^2*c^2 + b^2*c^2)], \[Tau] -> 
          Sqrt[(1/2)*(3 + Sqrt[5])], jJ -> qjJ, J -> qjJ, 
         dD -> 4*qS*Sqrt[qR*(4*qR + qr)]}]]
