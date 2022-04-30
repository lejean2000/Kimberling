sp = (a + b + c)/2
 
sa = (-a + b + c)/2
 
sb = (a - b + c)/2
 
sc = (a + b - c)/2
 
S = Sqrt[(a + b - c)*(a - b + c)*(-a + b + c)*(a + b + c)]/2
 
r = ((a + b + c)*Sqrt[(a + b - c)*(a - b + c)*(-a + b + c)*(a + b + c)])/8
 
SA = (-a^2 + b^2 + c^2)/2
 
SB = (a^2 - b^2 + c^2)/2
 
SC = (a^2 + b^2 - c^2)/2
 
SW = (a^2 + b^2 + c^2)/2
 
R = (a*b*c*Sqrt[(a + b - c)*(a - b + c)*(-a + b + c)*(a + b + c)])/4
 
jJ = Sqrt[a^6 - a^4*b^2 - a^2*b^4 + b^6 - a^4*c^2 + 3*a^2*b^2*c^2 - b^4*c^2 - 
       a^2*c^4 - b^2*c^4 + c^6]/(a*b*c)
 
dD = Sqrt[(a + b - c)*(a - b + c)*(-a + b + c)*(a + b + c)]*
     Sqrt[a*b*c*Sqrt[(a + b - c)*(a - b + c)*(-a + b + c)*(a + b + c)]*
       (a*b*c*Sqrt[(a + b - c)*(a - b + c)*(-a + b + c)*(a + b + c)] + 
        ((a + b + c)*Sqrt[(a + b - c)*(a - b + c)*(-a + b + c)*(a + b + c)])/
         8)]
 
angleA = ArcCos[(-a^2 + b^2 + c^2)/(2*b*c)]
 
angleB = ArcCos[(a^2 - b^2 + c^2)/(2*a*c)]
 
angleC = ArcCos[(a^2 + b^2 - c^2)/(2*a*b)]
 
angleW = ArcCot[(a^2 + b^2 - c^2)/
       (2*a*b*Sqrt[1 - (a^2 + b^2 - c^2)^2/(4*a^2*b^2)]) + 
      (a^2 - b^2 + c^2)/(2*a*c*Sqrt[1 - (a^2 - b^2 + c^2)^2/(4*a^2*c^2)]) + 
      (-a^2 + b^2 + c^2)/(2*b*c*Sqrt[1 - (-a^2 + b^2 + c^2)^2/(4*b^2*c^2)])]
 
symmetrizeEq[eq_] := Module[{partB, partC}, 
     partB = eq /. {a -> b, b -> c, c -> a, x -> y, y -> z, z -> x}; 
      partC = partB /. {a -> b, b -> c, c -> a, x -> y, y -> z, z -> x}; 
      eq + partB + partC]
 
symmetrizeEqGeogebra[eq_] := symmetrizeEq[eq] /. {x -> A, y -> B, z -> C}
