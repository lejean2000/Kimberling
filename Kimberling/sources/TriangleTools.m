bIsogonalConjugate[po_] := Simplify[{a^2*po[[2]]*po[[3]], 
       b^2*po[[1]]*po[[3]], c^2*po[[1]]*po[[2]]} /. setupParamTriangle, 
     c > 0 && a + b > c && a + c > b && b + c > a]
 
setupParamTriangle := setupBaseTriangle[{0, 0}, {c, 0}, 
     {(-a^2 + b^2 + c^2)/(2*c), Sqrt[-a^4 - (b^2 - c^2)^2 + 
         2*a^2*(b^2 + c^2)]/(2*c)}]
 
setupBaseTriangle[x_, y_, z_] := {a -> EuclideanDistance[y, z], 
     b -> EuclideanDistance[x, z], c -> EuclideanDistance[x, y]}
 
bIsotomicConjugate[P1_] := Module[{eq}, eq = symmetrizeInternal[1/pp]; 
      eq /. MapThread[#1 -> #2 & , {{pp, qq, rr}, P1}]]
 
symmetrizeInternal[eq_] := Module[{partB, partC}, 
     partB = eq /. {pp -> qq, qq -> rr, rr -> pp, uu -> vv, vv -> ww, 
         ww -> uu}; partB = partB /. {a -> b, b -> c, c -> a}; 
      partC = partB /. {pp -> qq, qq -> rr, rr -> pp, uu -> vv, vv -> ww, 
         ww -> uu}; partC = partC /. {a -> b, b -> c, c -> a}; 
      {eq, partB, partC}]
 
bPIsogonalConjugate[P1_, U1_] := Module[{eq, eq2}, 
     eq = qq*rr*vv*ww /. {pp -> pp/a, qq -> qq/b, rr -> rr/c, uu -> uu/a, 
         vv -> vv/b, ww -> ww/c}; eq2 = symmetrizeInternal[eq]; 
      bFromTrilinear[eq2 /. MapThread[#1 -> #2 & , {{pp, qq, rr}, P1}] /. 
        MapThread[#1 -> #2 & , {{uu, vv, ww}, U1}]]]
 
bFromTrilinear[p_] := {p[[1]]*a, p[[2]]*b, p[[3]]*c}
 
bDistanceF[p_, q_] := Module[{sp, sq}, sp = p/Total[p]; sq = q/Total[q]; 
      Simplify[Sqrt[(-a^2)*(sp[[2]] - sq[[2]])*(sp[[3]] - sq[[3]]) - 
          b^2*(sp[[1]] - sq[[1]])*(sp[[3]] - sq[[3]]) - 
          c^2*(sp[[1]] - sq[[1]])*(sp[[2]] - sq[[2]])] /. setupParamTriangle, 
       c > 0 && a + b > c && a + c > b && b + c > a]]
 
bCoordChangeK[k_, d_, e_, f_] := Module[{p}, 
     p = KimberlingCenterB[k] /. {a -> bDistanceF[e, f], 
         b -> bDistanceF[d, f], c -> bDistanceF[d, e]}; 
      Transpose[{d/Total[d], e/Total[e], f/Total[f]}] . Transpose[p/Total[p]]]
 
bDistance[p_, q_] := Module[{sp, sq}, sp = p/Total[p]; sq = q/Total[q]; 
      Sqrt[(-a^2)*(sp[[2]] - sq[[2]])*(sp[[3]] - sq[[3]]) - 
        b^2*(sp[[1]] - sq[[1]])*(sp[[3]] - sq[[3]]) - c^2*(sp[[1]] - sq[[1]])*
         (sp[[2]] - sq[[2]])]]
 
orth[p_, q_, r_] := Simplify[{1/(y^2 + z^2 - x^2), 1/(-y^2 + z^2 + x^2), 
       1/(y^2 - z^2 + x^2)} /. {x -> bDistance[q, r], y -> bDistance[p, r], 
       z -> bDistance[p, q]}]
 
bLine[u_, v_] := Module[{m}, 
     m = Det[{{u[[1]], u[[2]], u[[3]]}, {v[[1]], v[[2]], v[[3]]}, 
         {xx, yy, zz}}]; {Coefficient[m, xx], Coefficient[m, yy], 
       Coefficient[m, zz]}]
 
bLineL[{u_, v_}] := Module[{m}, 
     m = Det[{{u[[1]], u[[2]], u[[3]]}, {v[[1]], v[[2]], v[[3]]}, 
         {xx, yy, zz}}]; {Coefficient[m, xx], Coefficient[m, yy], 
       Coefficient[m, zz]}]
 
bLineIntersection[l1_, l2_] := {l1[[2]]*l2[[3]] - l2[[2]]*l1[[3]], 
     l1[[3]]*l2[[1]] - l2[[3]]*l1[[1]], l1[[1]]*l2[[2]] - l2[[1]]*l1[[2]]}
 
bIntersection[a_, b_, c_, d_] := Module[{l1, l2}, 
     l1 = bLine[a, b]; l2 = bLine[c, d]; bLineIntersection[l1, l2]]
 
bCoordChange[p_, d_, e_, f_] := 
    Transpose[{d/Total[d], e/Total[e], f/Total[f]}] . Transpose[p/Total[p]]
 
bReverseCoordChange[p_, d_, e_, f_] := 
    Inverse[Transpose[{d/Total[d], e/Total[e], f/Total[f]}]] . (p/Total[p])
 
tToBary[p_] := {p[[1]]/a, p[[2]]/b, p[[3]]/c}
 
bToCartesian[p_, PA_, PB_, PC_] := (p/Total[p]) . {PA, PB, PC}
 
bPerpendicular[line_, {u_, v_, w_}] := Module[{sa, sb, sc, f, g, h, pp, p, q, 
      r, ff, gg, hh, m}, pp = line/Total[line]; p = pp[[1]]; q = pp[[2]]; 
      r = pp[[3]]; sa = (b^2 + c^2 - a^2)/2; sb = (-b^2 + c^2 + a^2)/2; 
      sc = (b^2 - c^2 + a^2)/2; f = q - r; g = r - p; h = p - q; 
      ff = sb*g - sc*h; gg = sc*h - sa*f; hh = sa*f - sb*g; 
      m = Det[{{ff, gg, hh}, {u, v, w}, {x, y, z}}]; 
      {Coefficient[m, x], Coefficient[m, y], Coefficient[m, z]}]
 
cToBary[v1_, v2_, v3_, xy_] := 
    With[{mat = {{v1[[1]], v2[[1]], v3[[1]]}, {v1[[2]], v2[[2]], v3[[2]]}, 
        {1, 1, 1}}}, LinearSolve[mat, Append[xy, 1]]]
 
bCollinearityMatrix[u_, v_, w_] := Det[{{u[[1]], u[[2]], u[[3]]}, 
      {v[[1]], v[[2]], v[[3]]}, {w[[1]], w[[2]], w[[3]]}}]
 
bConcurrencyMatrix[u_, v_, w_] := Det[{{u[[1]], v[[1]], w[[1]]}, 
      {u[[2]], v[[2]], w[[2]]}, {u[[3]], v[[3]], w[[3]]}}]
 
cCollinearityMatrix[u_, v_, w_] := Det[{{u[[1]], u[[2]], 1}, 
      {v[[1]], v[[2]], 1}, {w[[1]], w[[2]], 1}}]
 
cConcurrencyMatrix[l1_, l2_, l3_] := 
    Det[{{Coefficient[l1, x], Coefficient[l1, y], l1 /. {x -> 0, y -> 0}}, 
      {Coefficient[l2, x], Coefficient[l2, y], l2 /. {x -> 0, y -> 0}}, 
      {Coefficient[l3, x], Coefficient[l3, y], l3 /. {x -> 0, y -> 0}}}]
 
multiCollect[expr_, vars_] := Activate[Expand[Collect[expr, vars, 
       Inactive[Simplify]]]]
 
bAubertLine[a_, b_, c_, d_] := Module[{z}, z = bIntersection[a, b, c, d]; 
      bLine[bCoordChange[orth[z, b, c], z, b, c], bCoordChange[orth[z, a, d], 
        z, a, d]]]
 
bAubertCenter[a_, b_, c_, d_] := Module[{z}, l1 = bAubertLine[a, b, c, d]; 
      l2 = bAubertLine[a, b, d, c]; bLineIntersection[l1, l2]]
 
bAubertCenter2[a_, b_, c_, d_] := Module[{z}, l1 = bAubertLine[a, b, c, d]; 
      l3 = bAubertLine[a, d, b, c]; bLineIntersection[l1, l3]]
 
bAubertCenter3[a_, b_, c_, d_] := Module[{z}, l2 = bAubertLine[a, b, d, c]; 
      l3 = bAubertLine[a, d, b, c]; bLineIntersection[l2, l3]]
 
bKimberlingTriangle[name_] := Module[{A1, B1, C1}, 
     Clear[a, b, c]; A1 = KimberlingTrianglesTrilinear[name]; 
      B1 = Permute[A1, Cycles[{{2, 3, 1}}]] /. {a -> b, b -> c, c -> a}; 
      C1 = Permute[B1, Cycles[{{2, 3, 1}}]] /. {a -> b, b -> c, c -> a}; 
      bFromTrilinear /@ {A1, B1, C1}]
 
bIsParallel[{a1_, b1_, c1_}, {a2_, b2_, c2_}] := 
    b1*c2 - c1*b2 + c1*a2 - a1*c2 + a1*b2 - b1*a2 == 0
 
bMidpoint[a_, b_] := With[{m = Total[b]*a + Total[a]*b}, m/Total[m]]
 
bParallelLine[{p1_, p2_, p3_}, {l1_, l2_, l3_}] := 
    Module[{m, tot}, tot = p1 + p2 + p3; 
      m = Det[{{l2 - l3, l3 - l1, l1 - l2}, {p1/tot, p2/tot, p3/tot}, 
         {xx, yy, zz}}]; {Coefficient[m, xx], Coefficient[m, yy], 
       Coefficient[m, zz]}]
 
bDistancePointLine[p_, l_] := Module[{p1, p2, p3, l1, l2, l3}, 
     {p1, p2, p3} = p/Total[p]; {l1, l2, l3} = l/Total[l]; 
      (1/2)*Sqrt[-(((a^4 + (b^2 - c^2)^2 - 2*a^2*(b^2 + c^2))*(p . l)^2)/
          (a^2*(l1 - l2)*(l1 - l3) + b^2*(-l1 + l2)*(l2 - l3) + 
           c^2*(l1 - l3)*(l2 - l3)))]]
 
bCircle4Check[{p11_, p12_, p13_}, {p21_, p22_, p23_}, {p31_, p32_, p33_}, 
     {p41_, p42_, p43_}] := Module[{ss}, 
     ss[{x1_, x2_, x3_}] := (x2*x3*a^2 + x1*x3*b^2 + x1*x2*c^2)/
        (x1 + x2 + x3); Det[{{ss[{p11, p12, p13}], ss[{p21, p22, p23}], 
         ss[{p31, p32, p33}], ss[{p41, p42, p43}]}, {p11, p21, p31, p41}, 
        {p12, p22, p32, p42}, {p13, p23, p33, p43}}]]
 
bCevianQuotient[P1_, U1_] := Module[{eq}, 
     eq = symmetrizeInternal[uu*(-(uu/pp) + vv/qq + ww/rr)]; 
      eq /. MapThread[#1 -> #2 & , {{pp, qq, rr}, P1}] /. 
       MapThread[#1 -> #2 & , {{uu, vv, ww}, U1}]]
 
bCevianProduct[P1_, U1_] := Module[{eq}, 
     eq = symmetrizeInternal[1/(rr*vv + qq*ww)]; 
      eq /. MapThread[#1 -> #2 & , {{pp, qq, rr}, P1}] /. 
       MapThread[#1 -> #2 & , {{uu, vv, ww}, U1}]]
 
bCrosspoint[P1_, U1_] := Module[{eq}, 
     eq = symmetrizeInternal[1/(rr*vv) + 1/(qq*ww)]; 
      eq /. MapThread[#1 -> #2 & , {{pp, qq, rr}, P1}] /. 
       MapThread[#1 -> #2 & , {{uu, vv, ww}, U1}]]
 
bTripole[P1_, U1_] := Module[{eq}, 
     eq = symmetrizeInternal[1/(rr*vv - qq*ww)]; 
      eq /. MapThread[#1 -> #2 & , {{pp, qq, rr}, P1}] /. 
       MapThread[#1 -> #2 & , {{uu, vv, ww}, U1}]]
 
bCrossConjugate[P1_, U1_] := Module[{eq}, 
     eq = symmetrizeInternal[uu/(-(pp/uu) + qq/vv + rr/ww)]; 
      eq /. MapThread[#1 -> #2 & , {{pp, qq, rr}, P1}] /. 
       MapThread[#1 -> #2 & , {{uu, vv, ww}, U1}]]
 
bComplement[P1_, U1_] := Module[{eq}, 
     eq = symmetrizeInternal[pp*(vv/qq + ww/rr)]; 
      eq /. MapThread[#1 -> #2 & , {{pp, qq, rr}, P1}] /. 
       MapThread[#1 -> #2 & , {{uu, vv, ww}, U1}]]
 
bAntiComplement[P1_, U1_] := Module[{eq}, 
     eq = symmetrizeInternal[pp*(-(uu/pp) + vv/qq + ww/rr)]; 
      eq /. MapThread[#1 -> #2 & , {{pp, qq, rr}, P1}] /. 
       MapThread[#1 -> #2 & , {{uu, vv, ww}, U1}]]
 
bAlephConjugate[P1_, U1_] := Module[{eq, eq2}, 
     eq = (-qq^2)*rr^2*uu^2 + rr^2*pp^2*vv^2 + pp^2*qq^2*ww^2 + 
         (vv*ww + ww*uu + uu*vv)*((-qq^2)*rr^2 + rr^2*pp^2 + pp^2*qq^2) /. 
        {pp -> pp/a, qq -> qq/b, rr -> rr/c, uu -> uu/a, vv -> vv/b, 
         ww -> ww/c}; eq2 = symmetrizeInternal[eq]; 
      (a^2*b^2*c^2)*bFromTrilinear[eq2 /. MapThread[#1 -> #2 & , 
           {{pp, qq, rr}, P1}] /. MapThread[#1 -> #2 & , {{uu, vv, ww}, U1}]]]
 
bCrossDiff[P1_, U1_] := Module[{eq, eq2}, 
     eq = qq*ww - rr*vv /. {pp -> pp/a, qq -> qq/b, rr -> rr/c, uu -> uu/a, 
         vv -> vv/b, ww -> ww/c}; eq2 = symmetrizeInternal[eq]; 
      bFromTrilinear[eq2 /. MapThread[#1 -> #2 & , {{pp, qq, rr}, P1}] /. 
        MapThread[#1 -> #2 & , {{uu, vv, ww}, U1}]]]
 
bCrossSum[P1_, U1_] := Module[{eq, eq2}, 
     eq = qq*ww + rr*vv /. {pp -> pp/a, qq -> qq/b, rr -> rr/c, uu -> uu/a, 
         vv -> vv/b, ww -> ww/c}; eq2 = symmetrizeInternal[eq]; 
      bFromTrilinear[eq2 /. MapThread[#1 -> #2 & , {{pp, qq, rr}, P1}] /. 
        MapThread[#1 -> #2 & , {{uu, vv, ww}, U1}]]]
 
bHirstInverse[P1_, U1_] := Module[{eq}, 
     eq = symmetrizeInternal[qq*rr*uu^2 - vv*ww*pp^2]; 
      eq /. MapThread[#1 -> #2 & , {{pp, qq, rr}, P1}] /. 
       MapThread[#1 -> #2 & , {{uu, vv, ww}, U1}]]
 
bAnticomplementaryConjugate[P1_, U1_] := Module[{eq, eq2}, 
     eq = (1/a)*(b^2/(qq*(a*uu + c*ww)) + c^2/(rr*(a*uu + b*vv)) - 
          a^2/(pp*(b*vv + c*ww))) /. {pp -> pp/a, qq -> qq/b, rr -> rr/c, 
         uu -> uu/a, vv -> vv/b, ww -> ww/c}; eq2 = symmetrizeInternal[eq]; 
      bFromTrilinear[eq2 /. MapThread[#1 -> #2 & , {{pp, qq, rr}, P1}] /. 
        MapThread[#1 -> #2 & , {{uu, vv, ww}, U1}]]]
 
bLineConjugate[P1_, U1_] := Module[{eq, eq2}, 
     eq = pp*(vv^2 + ww^2) - uu*(qq*vv + rr*ww) /. {pp -> pp/a, qq -> qq/b, 
         rr -> rr/c, uu -> uu/a, vv -> vv/b, ww -> ww/c}; 
      eq2 = symmetrizeInternal[eq]; bFromTrilinear[
       eq2 /. MapThread[#1 -> #2 & , {{pp, qq, rr}, P1}] /. 
        MapThread[#1 -> #2 & , {{uu, vv, ww}, U1}]]]
 
bAntigonalImage[P1_] := Module[{eq, eq2}, 
     eq = pp/((b^2 + c^2 - a^2)*pp^2 - a^2*qq*rr + (b^2 - a^2)*pp*qq + 
          (c^2 - a^2)*pp*rr) /. {pp -> pp/a, qq -> qq/b, rr -> rr/c}; 
      eq2 = symmetrizeInternal[eq]; bFromTrilinear[
       eq2 /. MapThread[#1 -> #2 & , {{pp, qq, rr}, P1}]]]
 
bBethConjugate[P1_, U1_] := Module[{eq, eq2}, 
     eq = 2*a*b*c*pp*((a^2 - b^2 + c^2)/(2*a*c) + (a^2 + b^2 - c^2)/(2*a*b))*
          (uu*((b + c - a)/pp) + vv*((a + c - b)/qq) + ww*((a + b - c)/rr)) - 
         uu*(a + b + c)*(b + c - a)*(a + c - b)*(a + b - c) /. 
        {pp -> pp/a, qq -> qq/b, rr -> rr/c, uu -> uu/a, vv -> vv/b, 
         ww -> ww/c}; eq2 = symmetrizeInternal[eq]; 
      bFromTrilinear[eq2 /. MapThread[#1 -> #2 & , {{pp, qq, rr}, P1}] /. 
        MapThread[#1 -> #2 & , {{uu, vv, ww}, U1}]]]
 
bComplementaryConjugate[P1_, U1_] := Module[{eq, eq2}, 
     eq = (1/a)*(b^2/(qq*(a*uu - b*vv + c*ww)) + 
          c^2/(rr*(a*uu + b*vv - c*ww))) /. {pp -> pp/a, qq -> qq/b, 
         rr -> rr/c, uu -> uu/a, vv -> vv/b, ww -> ww/c}; 
      eq2 = symmetrizeInternal[eq]; bFromTrilinear[
       eq2 /. MapThread[#1 -> #2 & , {{pp, qq, rr}, P1}] /. 
        MapThread[#1 -> #2 & , {{uu, vv, ww}, U1}]]]
 
bFivePointConicCoef[PA_, PB_, PC_, PD_, PE_] := Module[{sol, conic}, 
     conic[x_, y_, z_] := ({{x, y, z}} . {{m11, m12, m13}, {m12, m22, m23}, 
           {m13, m23, 1}} . {{x}, {y}, {z}})[[1]][[1]]; 
      First[Solve[{multiCollect[Numerator[Simplify[conic @@ PA]], 
           {m11, m12, m13, m22, m23}] == 0, 
         multiCollect[Numerator[Simplify[conic @@ PB]], {m11, m12, m13, m22, 
            m23}] == 0, multiCollect[Numerator[Simplify[conic @@ PC]], 
           {m11, m12, m13, m22, m23}] == 0, 
         multiCollect[Numerator[Simplify[conic @@ PD]], {m11, m12, m13, m22, 
            m23}] == 0, multiCollect[Numerator[Simplify[conic @@ PE]], 
           {m11, m12, m13, m22, m23}] == 0}, {m11, m12, m13, m22, m23}]]]
 
bFivePointConicEq[PA_, PB_, PC_, PD_, PE_] := 
    multiCollect[First[Flatten[{{x, y, z}} . {{m11, m12, m13}, 
          {m12, m22, m23}, {m13, m23, 1}} . {{x}, {y}, {z}} /. 
        bFivePointConicCoef[PA, PB, PC, PD, PE]]], {x, y, z}]
 
checkPointOnConic[XX_, PA_, PB_, PC_, PD_, PE_] := 
    Simplify[bFivePointConicEq[PA, PB, PC, PD, PE] /. 
      First[(Union[{x:Blank[Subscript] :> x}, Thread[{x, y, z} -> #1]] & ) /@ 
        {XX}]]
 
bConicCenter[{{m11_, m12_, m13_}, {m12_, m22_, m23_}, {m13_, m23_, m33_}}] := 
    Module[{p}, p = {-m23^2 + (m13 + m12)*m23 + m22*m33 - m13*m22 - m12*m33, 
        -m13^2 + (m12 + m23)*m13 + m11*m33 - m12*m33 - m23*m11, 
        -m12^2 + (m23 + m13)*m12 + m11*m22 - m23*m11 - m13*m22}; p/Total[p]]
 
bSaragossa1[{x_, y_, z_}] := Module[{u, v, w, x1, y1, z1, pt}, 
     x1 = x/(a*(x + y + z)); y1 = y/(b*(x + y + z)); z1 = z/(c*(x + y + z)); 
      u = a/(x1*(b*z1 + c*y1)); v = b/(y1*(c*x1 + a*z1)); 
      w = c/(z1*(a*y1 + b*x1)); pt = {a*u, b*v, c*w}; pt/Total[pt]]
 
bSaragossa2[{x1_, y1_, z1_}] := Module[{u, v, w, x, y, z, pt}, 
     x = x1/(a*(x1 + y1 + z1)); y = y1/(b*(x1 + y1 + z1)); 
      z = z1/(c*(x1 + y1 + z1)); u = a*x*((b^2*z^2 + c^2*y^2)*x + x*y*z*b*c + 
         a*y*z*(b*z + c*y)); v = b*y*((c^2*x^2 + a^2*z^2)*y + x*y*z*c*a + 
         b*z*x*(c*x + a*z)); w = c*z*((a^2*y^2 + b^2*x^2)*z + x*y*z*a*b + 
         c*x*y*(a*y + b*x)); pt = {a*u, b*v, c*w}; pt/Total[pt]]
 
bSaragossa3[{x1_, y1_, z1_}] := Module[{u, v, w, x, y, z, pt}, 
     x = x1/(a*(x1 + y1 + z1)); y = y1/(b*(x1 + y1 + z1)); 
      z = z1/(c*(x1 + y1 + z1)); u = a*x*((b^2*z^2 + c^2*y^2)*x + 
         a*y*z*(b*z + c*y)); v = b*y*((c^2*x^2 + a^2*z^2)*y + 
         b*z*x*(c*x + a*z)); w = c*z*((a^2*y^2 + b^2*x^2)*z + 
         c*x*y*(a*y + b*x)); pt = {a*u, b*v, c*w}; pt/Total[pt]]
 
bDistanceMod[p_, q_] := Module[{sp, sq}, sp = p/Total[p]; sq = q/Total[q]; 
      Sqrt[Abs[(-a^2)*(sp[[2]] - sq[[2]])*(sp[[3]] - sq[[3]]) - 
         b^2*(sp[[1]] - sq[[1]])*(sp[[3]] - sq[[3]]) - 
         c^2*(sp[[1]] - sq[[1]])*(sp[[2]] - sq[[2]])]]]
 
bCircleEq[{u1_, v1_, w1_}, {u2_, v2_, w2_}, {u3_, v3_, w3_}] := 
    Module[{d1, d2, d3, d, p, q, r, s1, s2, s3}, s1 = u1 + v1 + w1; 
      s2 = u2 + v2 + w2; s3 = u3 + v3 + w3; 
      d1 = Det[{{a^2*v1*w1 + b^2*w1*u1 + c^2*u1*v1, s1*v1, s1*w1}, 
         {a^2*v2*w2 + b^2*w2*u2 + c^2*u2*v2, s2*v2, s2*w2}, 
         {a^2*v3*w3 + b^2*w3*u3 + c^2*u3*v3, s3*v3, s3*w3}}]; 
      d2 = Det[{{s1*u1, a^2*v1*w1 + b^2*w1*u1 + c^2*u1*v1, s1*w1}, 
         {s2*u2, a^2*v2*w2 + b^2*w2*u2 + c^2*u2*v2, s2*w2}, 
         {s3*u3, a^2*v3*w3 + b^2*w3*u3 + c^2*u3*v3, s3*w3}}]; 
      d3 = Det[{{s1*u1, s1*v1, a^2*v1*w1 + b^2*w1*u1 + c^2*u1*v1}, 
         {s2*u2, s2*v2, a^2*v2*w2 + b^2*w2*u2 + c^2*u2*v2}, 
         {s3*u3, s3*v3, a^2*v3*w3 + b^2*w3*u3 + c^2*u3*v3}}]; 
      d = Det[{{u1, v1, w1}, {u2, v2, w2}, {u3, v3, w3}}]; 
      p = d1/(s1*s2*s3*d); q = d2/(s1*s2*s3*d); r = d3/(s1*s2*s3*d); 
      a^2*y*z + b^2*z*x + c^2*x*y - (x + y + z)*(p*x + q*y + r*z)]
 
bCircleCenter[{u1_, v1_, w1_}, {u2_, v2_, w2_}, {u3_, v3_, w3_}] := 
    Module[{d1, d2, d3, d, p, q, r, s1, s2, s3}, s1 = u1 + v1 + w1; 
      s2 = u2 + v2 + w2; s3 = u3 + v3 + w3; 
      d1 = Det[{{a^2*v1*w1 + b^2*w1*u1 + c^2*u1*v1, s1*v1, s1*w1}, 
         {a^2*v2*w2 + b^2*w2*u2 + c^2*u2*v2, s2*v2, s2*w2}, 
         {a^2*v3*w3 + b^2*w3*u3 + c^2*u3*v3, s3*v3, s3*w3}}]; 
      d2 = Det[{{s1*u1, a^2*v1*w1 + b^2*w1*u1 + c^2*u1*v1, s1*w1}, 
         {s2*u2, a^2*v2*w2 + b^2*w2*u2 + c^2*u2*v2, s2*w2}, 
         {s3*u3, a^2*v3*w3 + b^2*w3*u3 + c^2*u3*v3, s3*w3}}]; 
      d3 = Det[{{s1*u1, s1*v1, a^2*v1*w1 + b^2*w1*u1 + c^2*u1*v1}, 
         {s2*u2, s2*v2, a^2*v2*w2 + b^2*w2*u2 + c^2*u2*v2}, 
         {s3*u3, s3*v3, a^2*v3*w3 + b^2*w3*u3 + c^2*u3*v3}}]; 
      d = Det[{{u1, v1, w1}, {u2, v2, w2}, {u3, v3, w3}}]; 
      p = d1/(s1*s2*s3*d); q = d2/(s1*s2*s3*d); r = d3/(s1*s2*s3*d); 
      bConicCenter[{{-p, (c^2 - p - q)/2, (b^2 - p - r)/2}, 
        {(c^2 - p - q)/2, -q, (a^2 - q - r)/2}, {(b^2 - p - r)/2, 
         (a^2 - q - r)/2, -r}}]]
 
bCircumcevianTriangle[{u1_, v1_, w1_}] := Module[{u, v, w}, 
     Clear[a, b, c]; {u, v, w} = {u1/a, v1/b, w1/c}; 
      A1 = {(-a)*v*w, (b*w + c*v)*v, (b*w + c*v)*w}; 
      B1 = {(c*u + a*w)*u, (-b)*u*w, (c*u + a*w)*w}; 
      C1 = {(a*v + b*u)*u, (a*v + b*u)*v, (-c)*u*v}; 
      bFromTrilinear /@ {A1, B1, C1}]
 
bInconicEq[{p_, q_, r_}] := (x/p)^2 - 2*(y/q)*(z/r) + (y/q)^2 - 
     2*(x/p)*(z/r) + (z/r)^2 - 2*(y/q)*(x/p)
 
bCircumconicEq[{p_, q_, r_}, {u_, v_, w_}] := p*u*(r*v - q*w)*y*z + 
     q*v*(p*w - r*u)*z*x + r*w*(q*u - p*v)*x*y
 
bTripolarEq[{p_, q_, r_}] := {1/p, 1/q, 1/r}
 
bCircumconicPEq[{p_, q_, r_}] = r*x*y + q*x*z + p*y*z
 
bReciprocalConjugate[P1_, U1_] := Module[{eq}, 
     eq = symmetrizeInternal[uu/pp]; 
      eq /. MapThread[#1 -> #2 & , {{pp, qq, rr}, P1}] /. 
       MapThread[#1 -> #2 & , {{uu, vv, ww}, U1}]]
 
bToSearchNumbers[pt_] := S*(pt/(Total[pt]*{a, b, c})) /. 
     {a -> 6, b -> 9, c -> 13}
 
bPerspector[{{m11_, m12_, m13_}, {m12_, m22_, m23_}, {m13_, m23_, m33_}}] := 
    Module[{p}, p = {1/(m12*m13 - m11*m23), 1/((-m13)*m22 + m12*m23), 
        1/(m13*m23 - m12*m33)}; p/Total[p]]
 
conicEqtoMtx[eq_] := {{Coefficient[eq, x^2], (1/2)*Coefficient[eq, x*y], 
      (1/2)*Coefficient[eq, x*z]}, {(1/2)*Coefficient[eq, x*y], 
      Coefficient[eq, y^2], (1/2)*Coefficient[eq, y*z]}, 
     {(1/2)*Coefficient[eq, x*z], (1/2)*Coefficient[eq, y*z], 
      Coefficient[eq, z^2]}}
