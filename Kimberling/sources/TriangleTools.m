bIsogonalConjugate[po_] := Simplify[{a^2*po[[2]]*po[[3]], 
       b^2*po[[1]]*po[[3]], c^2*po[[1]]*po[[2]]} /. setupParamTriangle, 
     c > 0 && a + b > c && a + c > b && b + c > a]
 
setupParamTriangle := setupBaseTriangle[{0, 0}, {c, 0}, 
     {(-a^2 + b^2 + c^2)/(2*c), Sqrt[-a^4 - (b^2 - c^2)^2 + 
         2*a^2*(b^2 + c^2)]/(2*c)}]
 
setupBaseTriangle[x_, y_, z_] := {a -> EuclideanDistance[y, z], 
     b -> EuclideanDistance[x, z], c -> EuclideanDistance[x, y]}
 
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
 
bLineIntersection[l1_, l2_] := {l1[[2]]*l2[[3]] - l2[[2]]*l1[[3]], 
     l1[[3]]*l2[[1]] - l2[[3]]*l1[[1]], l1[[1]]*l2[[2]] - l2[[1]]*l1[[2]]}
 
bIntersection[a_, b_, c_, d_] := Module[{l1, l2}, 
     l1 = bLine[a, b]; l2 = bLine[c, d]; bLineIntersection[l1, l2]]
 
bCoordChange[p_, d_, e_, f_] := 
    Transpose[{d/Total[d], e/Total[e], f/Total[f]}] . Transpose[p/Total[p]]
 
bReverseCoordChange[p_, d_, e_, f_] := 
    Inverse[Transpose[{d/Total[d], e/Total[e], f/Total[f]}]] . (p/Total[p])
 
bFromTrilinear[p_] := {p[[1]]*a, p[[2]]*b, p[[3]]*c}
 
bToCartesian[p_, PA_, PB_, PC_] := (p/Total[p]) . {PA, PB, PC}
 
bPerpendicular[po_, {u_, v_, w_}] := Module[{sa, sb, sc, f, g, h, pp, p, q, 
      r, ff, gg, hh, m}, pp = po/Total[po]; p = pp[[1]]; q = pp[[2]]; 
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
 
bKimberlingTriangle[name_] := Module[{A1, B1, C1}, 
     Clear[a, b, c]; A1 = KimberlingTrianglesTrilinear[name]; 
      B1 = Permute[A1, Cycles[{{2, 3, 1}}]] /. {a -> b, b -> c, c -> a}; 
      C1 = Permute[B1, Cycles[{{2, 3, 1}}]] /. {a -> b, b -> c, c -> a}; 
      bFromTrilinear /@ {A1, B1, C1}]
 
bIsParallel[{a1_, b1_, c1_}, {a2_, b2_, c2_}] := 
    b1*c2 - c1*b2 + c1*a2 - a1*c2 + a1*b2 - b1*a2 == 0
 
bMidpoint[a_, b_] := With[{m = Total[b]*a + Total[a]*b}, m/Total[m]]
 
bParallelLine[p, {l1_, l2_, l3_}] := Module[{m, p1, p2, p3}, 
     p = p/Total[p]; {p1, p2, p3} = p; 
      m = Det[{{l2 - l3, l3 - l1, l1 - l2}, {p1, p2, p3}, {xx, yy, zz}}]; 
      {Coefficient[m, xx], Coefficient[m, yy], Coefficient[m, zz]}]
 
bDistancePointLine[p_, l_] := Module[{p1, p2, p3, sa, sb, sc, l1, l2, l3}, 
     sa = (b^2 + c^2 - a^2)/2; sb = (-b^2 + c^2 + a^2)/2; 
      sc = (b^2 - c^2 + a^2)/2; {p1, p2, p3} = p/Total[p]; {l1, l2, l3} = l; 
      Sqrt[((sa*sb + sa*sc + sb*sc)*(p . l)^2)/(sa*(l3 - l2)^2 + 
         sb*(l1 - l2)^2 + sc*(l2 - l1)^2)]]
 
bCircle4Check[{p11_, p12_, p13_}, {p21_, p22_, p23_}, {p31_, p32_, p33_}, 
     {p41_, p42_, p43_}] := Module[{ss}, 
     ss[{x1_, x2_, x3_}] := (x2*x3*a^2 + x1*x3*b^2 + x1*x2*c^2)/
        (x1 + x2 + x3); Det[{{ss[{p11, p12, p13}], ss[{p21, p22, p23}], 
         ss[{p31, p32, p33}], ss[{p41, p42, p43}]}, {p11, p21, p31, p41}, 
        {p12, p22, p32, p42}, {p13, p23, p33, p43}}]]
 
bCevianQuotient[{u1_, v1_, w1_}, {u2_, v2_, w2_}] := 
    {u2*(-(u2/u1) + v2/v1 + w2/w1), v2*(u2/u1 - v2/v1 + w2/w1), 
     w2*(u2/u1 + v2/v1 - w2/w1)}
 
bCevianProduct[{u1_, v1_, w1_}, {u2_, v2_, w2_}] := 
    {1/(v1*w2 + v2*w1), 1/(u1*w2 + u2*w1), 1/(v1*u2 + v2*u1)}
 
bFivePointConicCoef[PA_, PB_, PC_, PD_, PE_] := Module[{sol, conic}, 
     conic[x_, y_, z_] := ({{x, y, z}} . {{m11, m12, m13}, {m12, m22, m23}, 
           {m13, m23, 1}} . {{x}, {y}, {z}})[[1]][[1]]; 
      First[Solve[{multiCollect[Numerator[Simplify[conic @@ P13]], 
           {m11, m12, m13, m22, m23}] == 0, 
         multiCollect[Numerator[Simplify[conic @@ P23]], {m11, m12, m13, m22, 
            m23}] == 0, multiCollect[Numerator[Simplify[conic @@ P12]], 
           {m11, m12, m13, m22, m23}] == 0, 
         multiCollect[Numerator[Simplify[conic @@ P32]], {m11, m12, m13, m22, 
            m23}] == 0, multiCollect[Numerator[Simplify[conic @@ P21]], 
           {m11, m12, m13, m22, m23}] == 0}, {m11, m12, m13, m22, m23}]]]
 
bFivePointConicEq[PA_, PB_, PC_, PD_, PE_] := 
    First[Flatten[{{x, y, z}} . {{m11, m12, m13}, {m12, m22, m23}, 
         {m13, m23, 1}} . {{x}, {y}, {z}} /. bFivePointConicCoef[PA, PB, PC, 
        PD, PE]]]
 
checkPointOnConic[XX_, PA_, PB_, PC_, PD_, PE_] := 
    Simplify[bFivePointConicEq[PA, PB, PC, PD, PE] /. 
      First[(Thread[{x, y, z} -> #1] & ) /@ {XX}]]
 
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
