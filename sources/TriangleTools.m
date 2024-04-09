bIntersectionTriangleV[pa_, pb_, pc_, qa_, qb_, qc_] := 
    {bLineIntersection[bLine[pb, qb], bLine[pc, qc]], 
     bLineIntersection[bLine[pc, qc], bLine[pa, qa]], 
     bLineIntersection[bLine[pa, qa], bLine[pb, qb]]}
 
bIntersectionTriangleV[{pa_, pb_, pc_}, {qa_, qb_, qc_}] := 
    bIntersectionTriangleV[pa, pb, pc, qa, qb, qc]
 
bLineIntersection[l1_, l2_] := {l1[[2]]*l2[[3]] - l2[[2]]*l1[[3]], 
     l1[[3]]*l2[[1]] - l2[[3]]*l1[[1]], l1[[1]]*l2[[2]] - l2[[1]]*l1[[2]]}
 
bLine[u_, v_] := Module[{m, xx, yy, zz}, 
     m = Det[{{u[[1]], u[[2]], u[[3]]}, {v[[1]], v[[2]], v[[3]]}, 
         {xx, yy, zz}}]; {Coefficient[m, xx], Coefficient[m, yy], 
       Coefficient[m, zz]}]
 
bIntersectionTriangleS[pa_, pb_, pc_, qa_, qb_, qc_] := 
    {bLineIntersection[bLine[pb, pc], bLine[qb, qc]], 
     bLineIntersection[bLine[pc, pa], bLine[qc, qa]], 
     bLineIntersection[bLine[pa, pb], bLine[qa, qb]]}
 
bIntersectionTriangleS[{pa_, pb_, pc_}, {qa_, qb_, qc_}] := 
    bIntersectionTriangleS[pa, pb, pc, qa, qb, qc]
 
bIsogonalConjugate[po_] := {a^2*po[[2]]*po[[3]], b^2*po[[1]]*po[[3]], 
     c^2*po[[1]]*po[[2]]}
 
bIsotomicConjugate[{u_, v_, w_}] := {v*w, u*w, u*v}
 
bPIsogonalConjugate[{u_, v_, w_}, {p_, q_, r_}] := 
    {a^3*q*r*v*w, b^3*p*r*u*w, c^3*p*q*u*v}
 
bDistanceF[p_, q_] := Module[{sp, sq}, sp = p/Total[p]; sq = q/Total[q]; 
      Simplify[Sqrt[(-a^2)*(sp[[2]] - sq[[2]])*(sp[[3]] - sq[[3]]) - 
          b^2*(sp[[1]] - sq[[1]])*(sp[[3]] - sq[[3]]) - 
          c^2*(sp[[1]] - sq[[1]])*(sp[[2]] - sq[[2]])] /. setupParamTriangle, 
       a > 0 && b > 0 && c > 0 && a + b > c && a + c > b && b + c > a]]
 
setupParamTriangle := setupBaseTriangle[{0, 0}, {c, 0}, 
     {(-a^2 + b^2 + c^2)/(2*c), Sqrt[-a^4 - (b^2 - c^2)^2 + 
         2*a^2*(b^2 + c^2)]/(2*c)}]
 
setupBaseTriangle[x_, y_, z_] := {a -> EuclideanDistance[y, z], 
     b -> EuclideanDistance[x, z], c -> EuclideanDistance[x, y]}
 
bCoordChangeK[k_, d_, e_, f_] := Module[{pp}, 
     pp = KimberlingCenterC[k] /. {a -> bDistanceF[e, f], 
         b -> bDistanceF[d, f], c -> bDistanceF[d, e]}; 
      Transpose[{d/Total[d], e/Total[e], f/Total[f]}] . 
       Transpose[pp/Total[pp]]]
 
bCoordChangeK[k_, {d_, e_, f_}] := bCoordChangeK[k, d, e, f]
 
setupBaseTriangleBary[x_, y_, z_] := {a -> bDistance[y, z], 
     b -> bDistance[x, z], c -> bDistance[x, y]}
 
bDistance[p_, q_] := Module[{sp, sq}, sp = p/Total[p]; sq = q/Total[q]; 
      Sqrt[(-a^2)*(sp[[2]] - sq[[2]])*(sp[[3]] - sq[[3]]) - 
        b^2*(sp[[1]] - sq[[1]])*(sp[[3]] - sq[[3]]) - c^2*(sp[[1]] - sq[[1]])*
         (sp[[2]] - sq[[2]])]]
 
bLineL[{u_, v_}] := Module[{m, xx, yy, zz}, 
     m = Det[{{u[[1]], u[[2]], u[[3]]}, {v[[1]], v[[2]], v[[3]]}, 
         {xx, yy, zz}}]; {Coefficient[m, xx], Coefficient[m, yy], 
       Coefficient[m, zz]}]
 
bIntersection[a_, b_, c_, d_] := Module[{l1, l2}, 
     l1 = bLine[a, b]; l2 = bLine[c, d]; bLineIntersection[l1, l2]]
 
bCoordChange[p_, d_, e_, f_] := 
    Transpose[{d/Total[d], e/Total[e], f/Total[f]}] . Transpose[p/Total[p]]
 
bReverseCoordChange[p_, d_, e_, f_] := 
    Inverse[Transpose[{d/Total[d], e/Total[e], f/Total[f]}]] . (p/Total[p])
 
bToTril[p_] := {p[[1]]/a, p[[2]]/b, p[[3]]/c}
 
bFromTrilinear[p_] := {p[[1]]*a, p[[2]]*b, p[[3]]*c}
 
bToCartesian[p_, PA_, PB_, PC_] := (p/Total[p]) . {PA, PB, PC}
 
bPerpendicular[{p_, q_, r_}, {u_, v_, w_}] := 
    Module[{sa, sb, sc, f, g, h, ff, gg, hh, m}, sa = (b^2 + c^2 - a^2)/2; 
      sb = (-b^2 + c^2 + a^2)/2; sc = (b^2 - c^2 + a^2)/2; f = q - r; 
      g = r - p; h = p - q; ff = sb*g - sc*h; gg = sc*h - sa*f; 
      hh = sa*f - sb*g; m = Det[{{ff, gg, hh}, {u, v, w}, {x, y, z}}]; 
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
 
bAubertLine[aa_, bb_, cc_, dd_] := Module[{z}, 
     z = bIntersection[aa, bb, cc, dd]; bLine[bCoordChangeK[4, z, bb, cc], 
       bCoordChangeK[4, z, aa, dd]]]
 
bAubertCenter[aa_, bb_, cc_, dd_] := Module[{l1, l2}, 
     l1 = bAubertLine[aa, bb, cc, dd]; l2 = bAubertLine[aa, bb, dd, cc]; 
      bLineIntersection[l1, l2]]
 
bAubertCenter2[aa_, bb_, cc_, dd_] := Module[{l1, l3}, 
     l1 = bAubertLine[aa, bb, cc, dd]; l3 = bAubertLine[aa, dd, bb, cc]; 
      bLineIntersection[l1, l3]]
 
bAubertCenter3[aa_, bb_, cc_, dd_] := Module[{l2, l3}, 
     l2 = bAubertLine[aa, bb, dd, cc]; l3 = bAubertLine[aa, dd, bb, cc]; 
      bLineIntersection[l2, l3]]
 
bKimberlingTriangle[name_] := symmetrizeTriangleType2[name]
 
symmetrizeTriangleType2[name_] := Module[{v1, v2, v3, partB1, partB2, partB3, 
      partC1, partC2, partC3}, {v1, v2, v3} = KimberlingTrianglesBary[name]; 
      partB1 = v3 /. {a -> b, b -> c, c -> a, sa -> sb, sb -> sc, sc -> sa, 
         SA -> SB, SB -> SC, SC -> SA, A -> B, B -> C, C -> A}; 
      partB2 = v1 /. {a -> b, b -> c, c -> a, sa -> sb, sb -> sc, sc -> sa, 
         SA -> SB, SB -> SC, SC -> SA, A -> B, B -> C, C -> A}; 
      partB3 = v2 /. {a -> b, b -> c, c -> a, sa -> sb, sb -> sc, sc -> sa, 
         SA -> SB, SB -> SC, SC -> SA, A -> B, B -> C, C -> A}; 
      partC1 = partB3 /. {a -> b, b -> c, c -> a, sa -> sb, sb -> sc, 
         sc -> sa, SA -> SB, SB -> SC, SC -> SA, A -> B, B -> C, C -> A}; 
      partC2 = partB1 /. {a -> b, b -> c, c -> a, sa -> sb, sb -> sc, 
         sc -> sa, SA -> SB, SB -> SC, SC -> SA, A -> B, B -> C, C -> A}; 
      partC3 = partB2 /. {a -> b, b -> c, c -> a, sa -> sb, sb -> sc, 
         sc -> sa, SA -> SB, SB -> SC, SC -> SA, A -> B, B -> C, C -> A}; 
      {{v1, v2, v3}, {partB1, partB2, partB3}, {partC1, partC2, partC3}}]
 
bIsParallel[{a1_, b1_, c1_}, {a2_, b2_, c2_}] := b1*c2 - c1*b2 + c1*a2 - 
     a1*c2 + a1*b2 - b1*a2
 
bMidpoint[a_, b_] := With[{m = Total[b]*a + Total[a]*b}, m/Total[m]]
 
bParallelLine[{p1_, p2_, p3_}, {l1_, l2_, l3_}] := 
    Module[{m, tot, xx, yy, zz}, tot = p1 + p2 + p3; 
      m = Det[{{l2 - l3, l3 - l1, l1 - l2}, {p1/tot, p2/tot, p3/tot}, 
         {xx, yy, zz}}]; {Coefficient[m, xx], Coefficient[m, yy], 
       Coefficient[m, zz]}]
 
bDistancePointLine[p_, l_] := Module[{p1, p2, p3, l1, l2, l3, tp}, 
     {p1, p2, p3} = p/Total[p]; {l1, l2, l3} = l; 
      S*Sqrt[({p1, p2, p3} . l)^2/(a^2*(l1 - l2)*(l1 - l3) + 
          b^2*(-l1 + l2)*(l2 - l3) + c^2*(l1 - l3)*(l2 - l3))]]
 
bCircle4Check[{p11_, p12_, p13_}, {p21_, p22_, p23_}, {p31_, p32_, p33_}, 
     {p41_, p42_, p43_}] := Module[{ss}, 
     ss[{x1_, x2_, x3_}] := (x2*x3*a^2 + x1*x3*b^2 + x1*x2*c^2)/
        (x1 + x2 + x3); Det[{{ss[{p11, p12, p13}], ss[{p21, p22, p23}], 
         ss[{p31, p32, p33}], ss[{p41, p42, p43}]}, {p11, p21, p31, p41}, 
        {p12, p22, p32, p42}, {p13, p23, p33, p43}}]]
 
bCevianQuotient[{u_, v_, w_}, {p_, q_, r_}] := {p*((-r)*u*v - q*u*w + p*v*w), 
     q*((-r)*u*v + q*u*w - p*v*w), r*(r*u*v - (q*u + p*v)*w)}
 
bCevianProduct[{u_, v_, w_}, {p_, q_, r_}] := {(q*u + p*v)*(r*u + p*w), 
     (q*u + p*v)*(r*v + q*w), (r*u + p*w)*(r*v + q*w)}
 
bCrosspoint[{u_, v_, w_}, {p_, q_, r_}] := {p*u*(r*v + q*w), q*v*(r*u + p*w), 
     r*(q*u + p*v)*w}
 
bTripole[{u_, v_, w_}, {p_, q_, r_}] := {1/((-r)*v + q*w), 1/(r*u - p*w), 
     1/((-q)*u + p*v)}
 
bCrossConjugate[{u_, v_, w_}, {p_, q_, r_}] := 
    {p*((-q)*r*u - p*r*v + p*q*w)*(q*r*u - p*r*v + p*q*w), 
     q*(q*r*u - p*r*v - p*q*w)*(q*r*u + p*r*v - p*q*w), 
     r*(q*r*u - p*r*v - p*q*w)*(q*r*u - p*r*v + p*q*w)}
 
bComplement[{u_, v_, w_}, {p_, q_, r_}] := {u^2*(r*v + q*w), v^2*(r*u + p*w), 
     w^2*(q*u + p*v)}
 
bAntiComplement[{u_, v_, w_}, {p_, q_, r_}] := {u*(r*u*v + q*u*w - p*v*w), 
     v*(r*u*v - q*u*w + p*v*w), w*((-r)*u*v + q*u*w + p*v*w)}
 
bAlephConjugate[P1_, U1_] := Module[{eq, eq2}, Clear[pp, qq, rr, uu, vv, ww]; 
      eq = (-qq^2)*rr^2*uu^2 + rr^2*pp^2*vv^2 + pp^2*qq^2*ww^2 + 
         (vv*ww + ww*uu + uu*vv)*((-qq^2)*rr^2 + rr^2*pp^2 + pp^2*qq^2) /. 
        {pp -> pp/a, qq -> qq/b, rr -> rr/c, uu -> uu/a, vv -> vv/b, 
         ww -> ww/c}; eq2 = symmetrizeInternal[eq]; 
      (a^2*b^2*c^2)*bFromTrilinear[eq2 /. MapThread[#1 -> #2 & , 
           {{pp, qq, rr}, P1}] /. MapThread[#1 -> #2 & , {{uu, vv, ww}, U1}]]]
 
symmetrizeInternal[eq_] := Module[{partB, partC}, 
     Clear[pp, qq, rr, uu, vv, ww]; partB = eq /. {pp -> qq, qq -> rr, 
         rr -> pp, uu -> vv, vv -> ww, ww -> uu}; 
      partB = partB /. {a -> b, b -> c, c -> a, angleA -> angleB, 
         angleB -> angleC, angleC -> angleA, A -> B, B -> C, C -> A, 
         sa -> sb, sb -> sc, sc -> sa, SA -> SB, SB -> SC, SC -> SA}; 
      partC = partB /. {pp -> qq, qq -> rr, rr -> pp, uu -> vv, vv -> ww, 
         ww -> uu}; partC = partC /. {a -> b, b -> c, c -> a, 
         angleA -> angleB, angleB -> angleC, angleC -> angleA, A -> B, 
         B -> C, C -> A, sa -> sb, sb -> sc, sc -> sa, SA -> SB, SB -> SC, 
         SC -> SA}; {eq, partB, partC}]
 
bCrossDiff[{u_, v_, w_}, {p_, q_, r_}] := {a^2*(r*v - q*w), b^2*(p*w - r*u), 
     c^2*(q*u - p*v)}
 
bCrossSum[{u_, v_, w_}, {p_, q_, r_}] := {a^2*(r*v + q*w), b^2*(r*u + p*w), 
     c^2*(q*u + p*v)}
 
bHirstInverse[{u_, v_, w_}, {p_, q_, r_}] := {(-q)*r*u^2 + p^2*v*w, 
     (-p)*r*v^2 + q^2*u*w, r^2*u*v - p*q*w^2}
 
bAnticomplementaryConjugate[{u_, v_, w_}, {p_, q_, r_}] := 
    {c^3*(p + r)*(q + r)*u*v + (p + q)*(b^3*(q + r)*u - a^3*(p + r)*v)*w, 
     c^3*(p + r)*(q + r)*u*v + (p + q)*((-b^3)*(q + r)*u + a^3*(p + r)*v)*w, 
     (-c^3)*(p + r)*(q + r)*u*v + (p + q)*(b^3*(q + r)*u + a^3*(p + r)*v)*w}
 
bLineConjugate[{u_, v_, w_}, {p_, q_, r_}] := 
    {a^2*(c^2*q*(q*u - p*v) + b^2*r*(r*u - p*w)), 
     b^2*(c^2*p*((-q)*u + p*v) + a^2*r*(r*v - q*w)), 
     c^2*(b^2*p*((-r)*u + p*w) + a^2*q*((-r)*v + q*w))}
 
bBethConjugate[P1_, U1_] := Module[{eq, eq2}, Clear[pp, qq, rr, uu, vv, ww]; 
      eq = 2*a*b*c*pp*((a^2 - b^2 + c^2)/(2*a*c) + (a^2 + b^2 - c^2)/(2*a*b))*
          (uu*((b + c - a)/pp) + vv*((a + c - b)/qq) + ww*((a + b - c)/rr)) - 
         uu*(a + b + c)*(b + c - a)*(a + c - b)*(a + b - c) /. 
        {pp -> pp/a, qq -> qq/b, rr -> rr/c, uu -> uu/a, vv -> vv/b, 
         ww -> ww/c}; eq2 = symmetrizeInternal[eq]; 
      bFromTrilinear[eq2 /. MapThread[#1 -> #2 & , {{pp, qq, rr}, P1}] /. 
        MapThread[#1 -> #2 & , {{uu, vv, ww}, U1}]]]
 
bComplementaryConjugate[{u_, v_, w_}, {p_, q_, r_}] := 
    {(p - q - r)*u*(c^3*(p - q + r)*v + b^3*(p + q - r)*w), 
     (p - q + r)*v*(c^3*(p - q - r)*u - a^3*(p + q - r)*w), 
     (p + q - r)*(b^3*(p - q - r)*u - a^3*(p - q + r)*v)*w}
 
bFivePointConicEq[{p1_, q1_, r1_}, {p2_, q2_, r2_}, {p3_, q3_, r3_}, 
     {p4_, q4_, r4_}, {p5_, q5_, r5_}] := 
    Det[{{x^2, y^2, z^2, y*z, z*x, x*y}, {p1^2, q1^2, r1^2, q1*r1, r1*p1, 
       p1*q1}, {p2^2, q2^2, r2^2, q2*r2, r2*p2, p2*q2}, 
      {p3^2, q3^2, r3^2, q3*r3, r3*p3, p3*q3}, {p4^2, q4^2, r4^2, q4*r4, 
       r4*p4, p4*q4}, {p5^2, q5^2, r5^2, q5*r5, r5*p5, p5*q5}}]
 
checkPointOnConic[XX_, PA_, PB_, PC_, PD_, PE_] := 
    Simplify[bFivePointConicEq[PA, PB, PC, PD, PE] /. 
      First[(Union[{x:Blank[Subscript] :> x}, Thread[{x, y, z} -> #1]] & ) /@ 
        {XX}]]
 
bConicCenter[mx_] := Module[{m11, m12, m13, m21, m22, m23, m31, m32, m33}, 
     If[ !MatrixQ[mx], {{m11, m12, m13}, {m12, m22, m23}, {m13, m23, m33}} = 
        conicEqtoMtx[mx], {{m11, m12, m13}, {m12, m22, m23}, 
         {m13, m23, m33}} = mx]; {-m23^2 + (m13 + m12)*m23 + m22*m33 - 
        m13*m22 - m12*m33, -m13^2 + (m12 + m23)*m13 + m11*m33 - m12*m33 - 
        m23*m11, -m12^2 + (m23 + m13)*m12 + m11*m22 - m23*m11 - m13*m22}]
 
conicEqtoMtx[eq_] := {{Coefficient[eq, x^2], (1/2)*Coefficient[eq, x*y], 
      (1/2)*Coefficient[eq, x*z]}, {(1/2)*Coefficient[eq, x*y], 
      Coefficient[eq, y^2], (1/2)*Coefficient[eq, y*z]}, 
     {(1/2)*Coefficient[eq, x*z], (1/2)*Coefficient[eq, y*z], 
      Coefficient[eq, z^2]}}
 
bSaragossa1[{u_, v_, w_}] := {a^2*v*(b^2*u + a^2*v)*w*(c^2*u + a^2*w), 
     b^2*u*(b^2*u + a^2*v)*w*(c^2*v + b^2*w), c^2*u*v*(c^2*u + a^2*w)*
      (c^2*v + b^2*w)}
 
bSaragossa2[{u_, v_, w_}] := {a^2*u*(c^4*u*v^2 + c^2*v*(b^2*u + a^2*v)*w + 
       b^2*(b^2*u + a^2*v)*w^2), b^2*v*(c^4*u^2*v + c^2*u*(b^2*u + a^2*v)*w + 
       a^2*(b^2*u + a^2*v)*w^2), c^2*w*(b^4*u^2*w + b^2*u*v*(c^2*u + a^2*w) + 
       a^2*v^2*(c^2*u + a^2*w))}
 
bSaragossa3[{u_, v_, w_}] := {a^2*u*(c^4*u*v^2 + a^2*c^2*v^2*w + 
       b^2*(b^2*u + a^2*v)*w^2), b^2*v*(c^4*u^2*v + b^2*c^2*u^2*w + 
       a^2*(b^2*u + a^2*v)*w^2), c^2*w*(b^2*c^2*u^2*v + b^4*u^2*w + 
       a^2*v^2*(c^2*u + a^2*w))}
 
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
 
bCircumcevianTriangle[{u1_, v1_, w1_}] := Module[{u, v, w, A1, B1, C1}, 
     Clear[a, b, c]; {u, v, w} = {u1/a, v1/b, w1/c}; 
      A1 = {(-a)*v*w, (b*w + c*v)*v, (b*w + c*v)*w}; 
      B1 = {(c*u + a*w)*u, (-b)*u*w, (c*u + a*w)*w}; 
      C1 = {(a*v + b*u)*u, (a*v + b*u)*v, (-c)*u*v}; 
      bFromTrilinear /@ {A1, B1, C1}]
 
bInconicEq[{pp_, qq_, rr_}] := (x/pp)^2 - 2*(y/qq)*(z/rr) + (y/qq)^2 - 
     2*(x/pp)*(z/rr) + (z/rr)^2 - 2*(y/qq)*(x/pp)
 
bCircumconicEq[{pp_, qq_, rr_}, {u_, v_, w_}] := pp*u*(rr*v - qq*w)*y*z + 
     qq*v*(pp*w - rr*u)*z*x + rr*w*(qq*u - pp*v)*x*y
 
bTripolarEq[{pp_, qq_, rr_}] := {1/pp, 1/qq, 1/rr}
 
bCircumconicPEq[{pp_, qq_, rr_}] := pp*y*z + qq*x*z + rr*x*y
 
bReciprocalConjugate[{p_, q_, r_}, {u_, v_, w_}] := {p*v*w, q*u*w, r*u*v}
 
bToSearchNumbers[pt_] := S*(pt/(Total[pt]*{a, b, c})) /. 
     {a -> 6, b -> 9, c -> 13}
 
bPerspector[mx_] := Module[{p, m11, m12, m13, m21, m22, m23, m31, m32, m33}, 
     If[ !MatrixQ[mx], {{m11, m12, m13}, {m12, m22, m23}, {m13, m23, m33}} = 
        conicEqtoMtx[mx], {{m11, m12, m13}, {m12, m22, m23}, 
         {m13, m23, m33}} = mx]; {(m13*m22 - m12*m23)*(m13*m23 - m12*m33), 
       (m12*m13 - m11*m23)*((-m13)*m23 + m12*m33), 
       -((m12*m13 - m11*m23)*((-m13)*m22 + m12*m23))}]
 
bVertexConjugate[{u_, v_, w_}, {p_, q_, r_}] := 
    {a^2*((-c^4)*p*q*u*v + (b^2*p + a^2*q)*r*(b^2*u + a^2*v)*w)*
      (c^4*p*q*u*v + r*((-b^4)*p*u + a^4*q*v)*w + a^2*c^2*q*v*(r*u + p*w)), 
     b^2*((-c^4)*p*q*u*v + (b^2*p + a^2*q)*r*(b^2*u + a^2*v)*w)*
      (c^4*p*q*u*v + r*(b^4*p*u - a^4*q*v)*w + b^2*c^2*p*u*(r*v + q*w)), 
     c^2*(c^4*p*q*u*v + r*((-b^4)*p*u + a^4*q*v)*w + a^2*c^2*q*v*(r*u + p*w))*
      (c^4*p*q*u*v + r*(b^4*p*u - a^4*q*v)*w + b^2*c^2*p*u*(r*v + q*w))}
 
bTrilinearProduct[{p_, q_, r_}, {u_, v_, w_}] := {b*c*p*u, c*a*q*v, a*b*r*w}
 
bCirclecevianPerspector[{p_, q_, r_}] := 
    {a^2*(c^2*p*q^2 + b^2*p^2*r + 2*b^2*p*q*r + a^2*q^2*r + b^2*p*r^2)*
      (c^2*p^2*q + c^2*p*q^2 + 2*c^2*p*q*r + b^2*p*r^2 + a^2*q*r^2), 
     b^2*(c^2*p^2*q + b^2*p^2*r + 2*a^2*p*q*r + a^2*q^2*r + a^2*q*r^2)*
      (c^2*p^2*q + c^2*p*q^2 + 2*c^2*p*q*r + b^2*p*r^2 + a^2*q*r^2), 
     c^2*(c^2*p*q^2 + b^2*p^2*r + 2*b^2*p*q*r + a^2*q^2*r + b^2*p*r^2)*
      (c^2*p^2*q + b^2*p^2*r + 2*a^2*p*q*r + a^2*q^2*r + a^2*q*r^2)}
 
bTCCPerspector[{u_, v_, w_}] := 
    {a^2*((-c^4)*u^2*v^2 + ((-b^4)*u^2 + a^4*v^2)*w^2), 
     b^2*((-c^4)*u^2*v^2 + (b^4*u^2 - a^4*v^2)*w^2), 
     c^2*(c^4*u^2*v^2 - (b^4*u^2 + a^4*v^2)*w^2)}
 
bEigentransform[{u_, v_, w_}] := 
    {a^2*v*w*((-c^2)*u^2*v^2 + ((-b^2)*u^2 + a^2*v^2)*w^2), 
     b^2*u*w*((-c^2)*u^2*v^2 + (b^2*u^2 - a^2*v^2)*w^2), 
     c^2*u*v*(c^2*u^2*v^2 - (b^2*u^2 + a^2*v^2)*w^2)}
 
bAntitomicConjugate[{u_, v_, w_}] := {u*(v^2 - u*w)*((-u)*v + w^2), 
     v*(u^2 - v*w)*((-u)*v + w^2), w*(v^2 - u*w)*(u^2 - v*w)}
 
bReflectionPL[ptU_, {pp_, qq_, rr_}] := 
    Module[{tot, uu, vv, ww, mtxv, ssa, ssb, ssc}, 
     {uu, vv, ww} = ptU/Total[ptU]; ssa = (b^2 + c^2 - a^2)/2; 
      ssb = (-b^2 + c^2 + a^2)/2; ssc = (b^2 - c^2 + a^2)/2; 
      mtxv = {ssb*(pp - rr) - ssc*(qq - pp), ssc*(qq - pp) - ssa*(rr - qq), 
        ssa*(rr - qq) - ssb*(pp - rr)}; {uu, vv, ww} - 
       2*({pp, qq, rr} . {uu, vv, ww}/{pp, qq, rr} . mtxv)*mtxv]
 
bTripolarEqGeneral[{u1_, v1_, w1_}, {u2_, v2_, w2_}, {u3_, v3_, w3_}, 
     {u_, v_, w_}] := 
    {-((u3*v1*(v2*w - v*w2) + v3*(-2*u2*v1*w + u1*v2*w + u2*v*w1 - u*v2*w1 - 
           u1*v*w2 + 2*u*v1*w2) + v1*(u2*v - u*v2)*w3)*
        ((u3*v1*w - u1*v3*w - u3*v*w1 + u*v3*w1)*w2 + 
         (u2*v1*w - u1*v2*w - u2*v*w1 + u*v2*w1 + 2*u1*v*w2 - 2*u*v1*w2)*
          w3)) + (u3*v2*(v1*w - v*w1) + v3*(u2*v1*w - 2*u1*v2*w - u2*v*w1 + 
          2*u*v2*w1 + u1*v*w2 - u*v1*w2) + (u1*v - u*v1)*v2*w3)*
       (w1*(u3*v2*w - u2*v3*w - u3*v*w2 + u*v3*w2) + 
        ((-u2)*v1*w + u1*v2*w + 2*u2*v*w1 - 2*u*v2*w1 - u1*v*w2 + u*v1*w2)*
         w3), -((w1*(u3*v2*w - u2*v3*w - u3*v*w2 + u*v3*w2) + 
         ((-u2)*v1*w + u1*v2*w + 2*u2*v*w1 - 2*u*v2*w1 - u1*v*w2 + u*v1*w2)*
          w3)*(u3*((-u1)*v2*w + u*v2*w1 + u1*v*w2 - u*v1*w2) + 
         u2*(2*u3*v1*w - u1*v3*w - 2*u3*v*w1 + u*v3*w1 + u1*v*w3 - 
           u*v1*w3))) - ((u3*v1*w - u1*v3*w - u3*v*w1 + u*v3*w1)*w2 + 
        (u2*v1*w - u1*v2*w - u2*v*w1 + u*v2*w1 + 2*u1*v*w2 - 2*u*v1*w2)*w3)*
       (u*u3*(v2*w1 - v1*w2) + u2*(u3*v1*w + u1*v3*w - u3*v*w1 - u1*v*w3) + 
        u1*(-2*u3*v2*w + 2*u3*v*w2 - u*v3*w2 + u*v2*w3)), 
     (u3*v1*(v2*w - v*w2) + v3*(-2*u2*v1*w + u1*v2*w + u2*v*w1 - u*v2*w1 - 
          u1*v*w2 + 2*u*v1*w2) + v1*(u2*v - u*v2)*w3)*
       (u3*((-u1)*v2*w + u*v2*w1 + u1*v*w2 - u*v1*w2) + 
        u2*(2*u3*v1*w - u1*v3*w - 2*u3*v*w1 + u*v3*w1 + u1*v*w3 - u*v1*w3)) + 
      (u3*v2*(v1*w - v*w1) + v3*(u2*v1*w - 2*u1*v2*w - u2*v*w1 + 2*u*v2*w1 + 
          u1*v*w2 - u*v1*w2) + (u1*v - u*v1)*v2*w3)*(u*u3*(v2*w1 - v1*w2) + 
        u2*(u3*v1*w + u1*v3*w - u3*v*w1 - u1*v*w3) + 
        u1*(-2*u3*v2*w + 2*u3*v*w2 - u*v3*w2 + u*v2*w3))}
 
bInversePoint[ptP_, ptO_, rad_] := Module[{coef, ptP2, ptO2}, 
     ptP2 = ptP/Total[ptP]; ptO2 = ptO/Total[ptO]; 
      coef = Simplify[rad^2/bDistance[ptP, ptO]^2]; 
      {ptO2[[1]] + coef*(ptP2[[1]] - ptO2[[1]]), 
       ptO2[[2]] + coef*(ptP2[[2]] - ptO2[[2]]), 
       ptO2[[3]] + coef*(ptP2[[3]] - ptO2[[3]])}]
 
ExpressionToTrad[expr_, lClearSpaces_:True] := Module[{res, sub}, 
     res = ToString[expr, InputForm]; sub = {"Abs", "Sqrt", "Sign", "Log", 
        "Log10", "Exp", "Sin", "Cos", "Tan", "Cot", "Sec", "Csc", "ArcSin", 
        "ArcCos", "ArcTan", "ArcCot", "ArcSec", "ArcCsc", "Sinh", "Cosh", 
        "Tanh", "Coth", "Sech", "Csch", "ArcSinh", "ArcCosh", "ArcTanh", 
        "ArcCoth", "ArcSech", "ArcCsch"}; 
      sub = SortBy[sub, StringLength[#1] & , Greater]; 
      sub = (StringJoin[#1, "["].. -> StringJoin[ToLowerCase[#1], "["] & ) /@ 
        sub; res = StringReplace[res, sub]; 
      res = StringReplace[res, {"[".. -> "(", "]".. -> ")", "  ".. -> " "}]; 
      If[lClearSpaces, res = StringReplace[res, {" ".. -> ""}]]; 
      Return[res]; ]
 
bZosmaTransform[{u_, v_, w_}] := {a*(a^2 + b^2 - c^2)*(a^2 - b^2 + c^2)*
      (c*v + b*w), b*(a^2 + b^2 - c^2)*(-a^2 + b^2 + c^2)*(c*u + a*w), 
     c*(a^2 - b^2 + c^2)*(-a^2 + b^2 + c^2)*(b*u + a*v)}
 
GetKey[assoc_, index_] := First[Keys[Take[assoc, {index}]]]
 
bCoordChangeParam[p_, d_, e_, f_] := Module[{pp}, 
     pp = p /. {a -> bDistanceF[e, f], b -> bDistanceF[d, f], 
         c -> bDistanceF[d, e]}; Transpose[{d/Total[d], e/Total[e], 
         f/Total[f]}] . Transpose[pp/Total[pp]]]
 
bKirikamiCenter[pA_, pB_, pC_, pD_] := Module[{lAC, lBD, la, lb, lc, ld, pAB, 
      pBC, pCD, pDA}, lAC = bLine[pA, pC]; lBD = bLine[pB, pD]; 
      la = bParallelLine[pA, lBD]; lb = bParallelLine[pB, lAC]; 
      lc = bParallelLine[pC, lBD]; ld = bParallelLine[pD, lAC]; 
      pAB = bLineIntersection[la, lb]; pBC = bLineIntersection[lb, lc]; 
      pCD = bLineIntersection[lc, ld]; pDA = bLineIntersection[ld, la]; 
      bLineIntersection[bLine[pAB, pCD], bLine[pBC, pDA]]]
 
symmetrizeABC[expr_] := Module[{coordx, coordy, coordz}, 
     SetDelayed @@ {coordx[a_, b_, c_], expr[[1]]}; 
      SetDelayed @@ {coordy[a_, b_, c_], expr[[2]]}; 
      SetDelayed @@ {coordz[a_, b_, c_], expr[[3]]}; 
      {coordx[a, b, c] + coordy[c, a, b] + coordz[b, c, a], 
       coordx[b, c, a] + coordy[a, b, c] + coordz[c, a, b], 
       coordx[c, a, b] + coordy[b, c, a] + coordz[a, b, c]}]
 
bOrthopole[{u_, v_, w_}] := {(a^2*u - SC*v - SB*w)*(SB*SC*u - b^2*SB*v - 
       c^2*SC*w), ((-SC)*u + b^2*v - SA*w)*((-a^2)*SA*u + SA*SC*v - 
       c^2*SC*w), ((-SB)*u - SA*v + c^2*w)*((-a^2)*SA*u - b^2*SB*v + SA*SB*w)}
 
bSyngonal[{u_, v_, w_}] := {(-2*c^2*u*v + b^2*u*(u + v - w) + 
       a^2*v*(u + v - w))*(u - v - w)*(-2*b^2*u*w + c^2*u*(u - v + w) + 
       a^2*w*(u - v + w)), (-2*c^2*u*v + b^2*u*(u + v - w) + 
       a^2*v*(u + v - w))*(u - v + w)*(2*a^2*v*w + b^2*(u - v - w)*w - 
       c^2*v*(-u + v + w)), (u + v - w)*(-2*b^2*u*w + c^2*u*(u - v + w) + 
       a^2*w*(u - v + w))*(2*a^2*v*w + b^2*(u - v - w)*w - 
       c^2*v*(-u + v + w))}
 
bCircleEqRad[cent_, rad_] := Module[{u1, v1, w1}, 
     {u1, v1, w1} = cent/Total[cent]; SA*(x - u1*(x + y + z))^2 + 
       SB*(y - v1*(x + y + z))^2 + SC*(z - w1*(x + y + z))^2 - 
       rad^2*(x + y + z)^2]
 
circleBaryCoords[cent_, rad_] := Module[{u1, v1, w1, m1, m2, m3}, 
     {u1, v1, w1} = cent/Total[cent]; m1 = SA*(1 - u1)^2 + SB*v1^2 + 
        SC*w1^2 - rad^2; m2 = SA*u1^2 + SB*(1 - v1)^2 + SC*w1^2 - rad^2; 
      m3 = SA*u1^2 + SB*v1^2 + SC*(1 - w1)^2 - rad^2; {m1, m2, m3}]
 
radicalCenter[c1_, r1_, c2_, r2_, c3_, r3_] := Module[{bc1, bc2, bc3}, 
     bc1 = circleBaryCoords[c1, r1]; bc2 = circleBaryCoords[c2, r2]; 
      bc3 = circleBaryCoords[c3, r3]; 
      {Det[{{1, 1, 1}, {bc1[[2]], bc2[[2]], bc3[[2]]}, {bc1[[3]], bc2[[3]], 
          bc3[[3]]}}], Det[{{1, 1, 1}, {bc1[[3]], bc2[[3]], bc3[[3]]}, 
         {bc1[[1]], bc2[[1]], bc3[[1]]}}], 
       Det[{{1, 1, 1}, {bc1[[1]], bc2[[1]], bc3[[1]]}, {bc1[[2]], bc2[[2]], 
          bc3[[2]]}}]}]
 
symmetrizeABCUVW[expr_] := Module[{coordx, coordy, coordz}, 
     SetDelayed @@ {coordx[a_, b_, c_, u_, v_, w_], expr[[1]]}; 
      SetDelayed @@ {coordy[a_, b_, c_, u_, v_, w_], expr[[2]]}; 
      SetDelayed @@ {coordz[a_, b_, c_, u_, v_, w_], expr[[3]]}; 
      {coordx[a, b, c, u, v, w] + coordy[c, a, b, w, u, v] + 
        coordz[b, c, a, v, w, u], coordx[b, c, a, v, w, u] + 
        coordy[a, b, c, u, v, w] + coordz[c, a, b, w, u, v], 
       coordx[c, a, b, w, u, v] + coordy[b, c, a, v, w, u] + 
        coordz[a, b, c, u, v, w]}]
 
bCevianTriangle[{u_, v_, w_}] := {{0, v, w}, {u, 0, w}, {u, v, 0}}
 
cLineFromBary[{xa_, ya_}, {xb_, yb_}, {xc_, yc_}, {u_, v_, w_}] := 
    {(w*(ya - yb) + u*(yb - yc) + v*(-ya + yc))/(w*(xb*ya - xa*yb) + 
       v*((-xc)*ya + xa*yc) + u*(xc*yb - xb*yc)), 
     (w*(-xa + xb) + v*(xa - xc) + u*(-xb + xc))/(w*xb*ya - v*xc*ya - 
       w*xa*yb + u*xc*yb + v*xa*yc - u*xb*yc)}
 
bReflectionPP[{uu_, vv_, ww_}, {oa_, ob_, oc_}] := 
    {-((ob + oc)*uu) + oa*(uu + 2*(vv + ww)), -((oa + oc)*vv) + 
      ob*(2*uu + vv + 2*ww), -((oa + ob)*ww) + oc*(2*uu + 2*vv + ww)}
 
bHomotethy[ptP_, ptO_, k_] := Module[{u, v, w, oa, ob, oc}, 
     {u, v, w} = ptP/Total[ptP]; {oa, ob, oc} = ptO/Total[ptO]; 
      {(k*oa + ob*u + oc*u - oa*v - oa*w)/k, 
       (k*ob + (oa + oc)*v - ob*(u + w))/k, (k*oc - oc*(u + v) + (oa + ob)*w)/
        k}]
 
bMixtilinearIncircleA[a_, b_, c_] := Module[{s}, s = (a + b + c)/2; 
      multiCollect[Simplify[4*(a^2*y*z + b^2*z*x + c^2*x*y)*s^2 - 
         4*b^2*c^2*(x + y + z)*(x + (s/b - 1)^2*y + (s/c - 1)^2*z)], 
       {x, y, z}]]
 
bMixtilinearIncircleB[a_, b_, c_] := Module[{s}, s = (a + b + c)/2; 
      multiCollect[Simplify[4*(a^2*y*z + b^2*z*x + c^2*x*y)*s^2 - 
         4*a^2*c^2*(x + y + z)*((s/a - 1)^2*x + y + (s/c - 1)^2*z)], 
       {x, y, z}]]
 
bMixtilinearIncircleC[a_, b_, c_] := Module[{s}, s = (a + b + c)/2; 
      multiCollect[Simplify[4*(a^2*y*z + b^2*z*x + c^2*x*y)*s^2 - 
         4*a^2*b^2*(x + y + z)*((s/a - 1)^2*x + (s/b - 1)^2*y + z)], 
       {x, y, z}]]
 
bPolar[mx_, {u_, v_, w_}] := Module[{mxm}, 
     If[ !MatrixQ[mx], mxm = conicEqtoMtx[mx], mxm = mx]; 
      First /@ (mxm . {{u}, {v}, {w}})]
 
bCyclocevianConjugate[{u_, v_, w_}] := 
    {1/(c^2*u*v*(u + w)*(v + w) - (u + v)*w*(a^2*v*(u + w) - b^2*u*(v + w))), 
     1/(a^2*v*(u + v)*w*(u + w) - u*(v + w)*(b^2*(u + v)*w - c^2*v*(u + w))), 
     1/(b^2*u*(u + v)*w*(v + w) - v*(u + w)*((-a^2)*(u + v)*w + 
         c^2*u*(v + w)))}
 
bExsimilicenter[P1_, P2_, P3_, Q1_, Q2_, Q3_] := Module[{rad1, rad2, O1, O2}, 
     O1 = bCircleCenter[P1, P2, P3]; O2 = bCircleCenter[Q1, Q2, Q3]; 
      rad1 = bDistance[O1, P1]; rad2 = bDistance[O2, Q2]; 
      rad1*(O2/Total[O2]) - rad2*(O1/Total[O1])]
 
bExsimilicenter2[O1_, P1_, O2_, P2_] := Module[{rad1, rad2}, 
     rad1 = bDistance[O1, P1]; rad2 = bDistance[O2, P2]; 
      rad1*(O2/Total[O2]) - rad2*(O1/Total[O1])]
 
bInsimilicenter[P1_, P2_, P3_, Q1_, Q2_, Q3_] := Module[{rad1, rad2, O1, O2}, 
     O1 = bCircleCenter[P1, P2, P3]; O2 = bCircleCenter[Q1, Q2, Q3]; 
      rad1 = bDistance[O1, P1]; rad2 = bDistance[O2, Q2]; 
      rad1*(O2/Total[O2]) + rad2*(O1/Total[O1])]
 
bInsimilicenter2[O1_, P1_, O2_, P2_] := Module[{rad1, rad2}, 
     rad1 = bDistance[O1, P1]; rad2 = bDistance[O2, P2]; 
      rad1*(O2/Total[O2]) + rad2*(O1/Total[O1])]
 
bInsimilicenter3[O1_, O2_, rad1_, rad2_] := rad1*(O2/Total[O2]) + 
     rad2*(O1/Total[O1])
 
bPedalTriangle[{u_, v_, w_}] := {{0, a^2*v + SC*u, a^2*w + SB*u}, 
     {b^2*u + SC*v, 0, b^2*w + SA*v}, {c^2*u + SB*w, c^2*v + SA*w, 0}}
 
bBicevianConic[{u1_, v1_, w1_}, {u2_, v2_, w2_}] := 
    (-v1)*v2*w1*w2*x^2 + (u2*v1 + u1*v2)*w1*w2*x*y - u1*u2*w1*w2*y^2 + 
     v1*v2*(u2*w1 + u1*w2)*x*z + u1*u2*(v2*w1 + v1*w2)*y*z - u1*u2*v1*v2*z^2
 
symmetrizeTriangle[name_] := Module[{v1, v2, v3, partB1, partB2, partB3, 
      partC1, partC2, partC3}, {v1, v2, v3} = KimberlingTrianglesBary[name]; 
      partB1 = v2 /. {b -> a, c -> b, a -> c, sb -> sa, sc -> sb, sa -> sc, 
         SB -> SA, SC -> SB, SA -> SC, B -> A, C -> B, A -> C}; 
      partB2 = v1 /. {a -> b, b -> c, c -> a, sa -> sb, sb -> sc, sc -> sa, 
         SA -> SB, SB -> SC, SC -> SA, A -> B, B -> C, C -> A}; partB3 = v3; 
      partC1 = partB1; partC2 = v2; partC3 = 
       partB2 /. {a -> b, b -> c, c -> a, sa -> sb, sb -> sc, sc -> sa, 
         SA -> SB, SB -> SC, SC -> SA, A -> B, B -> C, C -> A}; 
      {{v1, v2, v3}, {partB1, partB2, partB3}, {partC1, partC2, partC3}}]
 
bCircumcircleInverse[{u_, v_, w_}] := 
    {a^2*((-b^4)*u*w + (a^2 - c^2)*v*(c^2*u + a^2*w) + 
       b^2*(c^2*u^2 + a^2*(u - v)*w)), 
     b^6*u*w + b^2*v*((-c^4)*u + a^2*c^2*v - a^4*w) + 
      b^4*(c^2*u*(v - w) + a^2*(-u + v)*w), 
     c^2*((-a^4)*v*w - (b^2 - c^2)*u*(c^2*v + b^2*w) + 
       a^2*(b^2*w^2 + c^2*v*(-u + w)))}
 
bOrthoassociate[P1_] := Module[{eq, g}, g[a_, b_, c_, p_, q_, r_] := 
       (a^2 + b^2 - c^2)*(a^2 - b^2 + c^2)*
        ((q + r)*(b^2*(p + q - r) + c^2*(p - q + r)) - 
         a^2*(q^2 + r^2 + p*(q + r))); {g[a, b, c, pp, qq, rr], 
        g[b, c, a, qq, rr, pp], g[c, a, b, rr, pp, qq]} /. 
       Thread[{pp, qq, rr} -> P1]]
 
bTripoleL[L1_] := 1/L1
 
bDaoConjugate[{u_, v_, w_}, {p_, q_, r_}] := {q*r*u*(-u + v + w), 
     p*r*v*(u - v + w), p*q*(u + v - w)*w}
 
cundyParryPhi[{x_, y_, z_}] := {(SB*y - SC*z)/(c^2*SC*y - b^2*SB*z), 
     (SC*z - SA*x)/(a^2*SA*z - c^2*SC*x), (SA*x - SB*y)/(b^2*SB*x - a^2*SA*y)}
 
cundyParryPsi[{x_, y_, z_}] := {a^2, b^2, c^2}/cundyParryPhi[{x, y, z}]
 
bHarmonicConjugate[{a1_, a2_, a3_}, {b1_, b2_, b3_}, {c1_, c2_, c3_}] := 
    {a3*b1*c1 + a1*b3*c1 - 2*a1*b1*c3, a3*b2*c1 + a2*b3*c1 - a2*b1*c3 - 
      a1*b2*c3, 2*a3*b3*c1 - a3*b1*c3 - a1*b3*c3}
 
bHarmonicConjugateFull[{a1_, a2_, a3_}, {b1_, b2_, b3_}, {c1_, c2_, c3_}] := 
    {a2*b1*c1 - a3*b1*c1 + a1*b2*c1 - a1*b3*c1 - 2*a1*b1*c2 + a3*b1*c2 + 
      a1*b3*c2 + 2*a1*b1*c3 - a2*b1*c3 - a1*b2*c3, 2*a2*b2*c1 - a3*b2*c1 - 
      a2*b3*c1 - a2*b1*c2 - a1*b2*c2 + a3*b2*c2 + a2*b3*c2 + a2*b1*c3 + 
      a1*b2*c3 - 2*a2*b2*c3, a3*b2*c1 + a2*b3*c1 - 2*a3*b3*c1 - a3*b1*c2 - 
      a1*b3*c2 + 2*a3*b3*c2 + a3*b1*c3 - a3*b2*c3 + a1*b3*c3 - a2*b3*c3}
 
NormalizeBary[v_] := Normalize[v]*If[v[[1]] == 0, 1, Sign[v[[1]]]]
 
bAnticevianTriangle[{u_, v_, w_}] := {{-u, v, w}, {u, -v, w}, {u, v, -w}}
 
bDualConic[mx_] := Module[{p, m11, m12, m13, m21, m22, m23, m31, m32, m33}, 
     If[ !MatrixQ[mx], {{m11, m12, m13}, {m12, m22, m23}, {m13, m23, m33}} = 
        conicEqtoMtx[mx], {{m11, m12, m13}, {m12, m22, m23}, 
         {m13, m23, m33}} = mx]; 
      ({{x, y, z}} . Adjugate[{{m11, m12, m13}, {m12, m22, m23}, 
           {m13, m23, m33}}] . {{x}, {y}, {z}})[[1]][[1]]]
 
bDrozFarnyPole[{p_, q_, r_}] := {2*SA*p*(p + q + r) - 
      (a^2*q*r + b^2*r*p + c^2*p*q), 2*SB*q*(p + q + r) - 
      (a^2*q*r + b^2*r*p + c^2*p*q), 2*SC*r*(p + q + r) - 
      (a^2*q*r + b^2*r*p + c^2*p*q)}
 
bOrthocorrespondent[{p_, q_, r_}] := 
    {SA*p*(p + q + r) - (a^2*q*r + b^2*r*p + c^2*p*q), 
     SB*q*(p + q + r) - (a^2*q*r + b^2*r*p + c^2*p*q), 
     SC*r*(p + q + r) - (a^2*q*r + b^2*r*p + c^2*p*q)}
 
bVuPole[{p_, q_, r_}, {u_, v_, w_}] := 
    {q*r*(a^2*(q*r*u*(u + v + w) - p*v*w*(p + q + r)) - 
       b^2*p*u*(w*(p + q) - r*(u + v)) - c^2*p*u*(v*(p + r) - q*(u + w))), 
     r*p*(b^2*(r*p*v*(u + v + w) - q*w*u*(p + q + r)) - 
       c^2*q*v*(u*(q + r) - p*(v + w)) - a^2*q*v*(w*(q + p) - r*(v + u))), 
     p*q*(c^2*(p*q*w*(u + v + w) - r*u*v*(p + q + r)) - 
       a^2*r*w*(v*(r + p) - q*(w + u)) - b^2*r*w*(u*(r + q) - p*(w + v)))}
 
bPole[mx_, ln_] := Module[{mtx}, If[ !MatrixQ[mx], mtx = conicEqtoMtx[mx], 
       mtx = mx]; Adjugate[mtx] . ln]
 
centralCircle[l_] := Module[{expr, eq}, expr = symmetrizeInternal[l]; 
      eq = (expr[[1]]*x + expr[[2]]*y + expr[[3]]*z)*(a*x + b*y + c*z) + 
        (a*y*z + b*x*z + c*x*y); multiCollect[
       Simplify[a*b*c*(eq /. Thread[{x, y, z} -> {x/a, y/b, z/c}])], 
       {x, y, z}]]
 
bImplicitTangent[crv_, pt_] := x*(D[crv, x] /. Thread[{x, y, z} -> pt]) + 
     y*(D[crv, y] /. Thread[{x, y, z} -> pt]) + 
     z*(D[crv, z] /. Thread[{x, y, z} -> pt])
 
bLineFromEq[lineq_] := {lineq /. Thread[{x, y, z} -> {1, 0, 0}], 
     lineq /. Thread[{x, y, z} -> {0, 1, 0}], 
     lineq /. Thread[{x, y, z} -> {0, 0, 1}]}
 
bCollingsTransform[{u_, v_, w_}] := 
    {1/((-b^2)*(u + v - w)*w + c^2*v*(u - v + w)), 
     1/(a^2*(u + v - w)*w - c^2*u*(-u + v + w)), 
     1/((-a^2)*v*(u - v + w) + b^2*u*(-u + v + w))}
 
symmetrizeInternal2[eq_] := Module[{partB, partC}, Clear[p, q, r, u, v, w]; 
      partB = eq /. {p -> q, q -> r, r -> p, u -> v, v -> w, w -> u}; 
      partB = partB /. {a -> b, b -> c, c -> a, angleA -> angleB, 
         angleB -> angleC, angleC -> angleA, A -> B, B -> C, C -> A, 
         sa -> sb, sb -> sc, sc -> sa, SA -> SB, SB -> SC, SC -> SA}; 
      partC = partB /. {p -> q, q -> r, r -> p, u -> v, v -> w, w -> u}; 
      partC = partC /. {a -> b, b -> c, c -> a, angleA -> angleB, 
         angleB -> angleC, angleC -> angleA, A -> B, B -> C, C -> A, 
         sa -> sb, sb -> sc, sc -> sa, SA -> SB, SB -> SC, SC -> SA}; 
      {eq, partB, partC}]
 
symmetrizeTriangleExprType2Bary[{v1_, v2_, v3_}] := 
    Module[{partB1, partB2, partB3, partC1, partC2, partC3, repl}, 
     repl = {a -> b, b -> c, c -> a, sa -> sb, sb -> sc, sc -> sa, SA -> SB, 
        SB -> SC, SC -> SA, u -> v, v -> w, w -> u, p -> q, q -> r, r -> p, 
        A -> B, B -> C, C -> A}; partB1 = v3 /. repl; partB2 = v1 /. repl; 
      partB3 = v2 /. repl; partC1 = partB3 /. repl; partC2 = partB1 /. repl; 
      partC3 = partB2 /. repl; {{v1, v2, v3}, {partB1, partB2, partB3}, 
       {partC1, partC2, partC3}}]
 
bToCartesianN[p_] := N[bToCartesian[p, {31/3, (4*Sqrt[35])/3}, {0, 0}, 
       {6, 0}] /. rule69, 20]
 
symmetrizeTriangleExprType1Bary[{v1_, v2_, v3_}] := 
    Module[{partB1, partB2, partB3, partC1, partC2, partC3}, 
     partB1 = v2 /. {b -> a, c -> b, a -> c, sb -> sa, sc -> sb, sa -> sc, 
         SB -> SA, SC -> SB, SA -> SC, v -> u, w -> v, u -> w}; 
      partB2 = v1 /. {a -> b, b -> c, c -> a, sa -> sb, sb -> sc, sc -> sa, 
         SA -> SB, SB -> SC, SC -> SA, u -> v, v -> w, w -> u}; partB3 = v3; 
      partC1 = partB1; partC2 = v2; partC3 = 
       partB2 /. {a -> b, b -> c, c -> a, sa -> sb, sb -> sc, sc -> sa, 
         SA -> SB, SB -> SC, SC -> SA, u -> v, v -> w, w -> u}; 
      {{v1, v2, v3}, {partB1, partB2, partB3}, {partC1, partC2, partC3}}]
 
bCircumconcevianM1[{u_, v_, w_}, {a1_, a2_, a3_}, {b1_, b2_, b3_}] := 
    {u*(a2*a3*b2*b3*u^2 + a2*a3*b1*b3*u*v + a1*a3*b2*b3*u*v + 
       a1*a3*b1*b3*v^2 - a1*a2*b1*b2*w^2)*(a2*a3*b2*b3*u^2 + 
       a2*a3*b1*b2*u*w + a1*a2*b2*b3*u*w - a1*a3*b1*b3*v^2 + 
       a1*a2*b1*b2*w^2), v*(a2*a3*b2*b3*u^2 + a2*a3*b1*b3*u*v + 
       a1*a3*b2*b3*u*v + a1*a3*b1*b3*v^2 - a1*a2*b1*b2*w^2)*
      ((-a2)*a3*b2*b3*u^2 + a1*a3*b1*b3*v^2 + a1*a3*b1*b2*v*w + 
       a1*a2*b1*b3*v*w + a1*a2*b1*b2*w^2), 
     w*(a2*a3*b2*b3*u^2 - a1*a3*b1*b3*v^2 + a2*a3*b1*b2*u*w + 
       a1*a2*b2*b3*u*w + a1*a2*b1*b2*w^2)*((-a2)*a3*b2*b3*u^2 + 
       a1*a3*b1*b3*v^2 + a1*a3*b1*b2*v*w + a1*a2*b1*b3*v*w + a1*a2*b1*b2*w^2)}
 
bCircumconcevianM2[{u_, v_, w_}, {a1_, a2_, a3_}, {b1_, b2_, b3_}] := 
    {u*(a2*a3*b2*b3*u^2 - a2*a3*b1*b3*u*v - a1*a3*b2*b3*u*v + 
       a1*a3*b1*b3*v^2 - a1*a2*b1*b2*w^2)*(a2*a3*b2*b3*u^2 - 
       a2*a3*b1*b2*u*w - a1*a2*b2*b3*u*w - a1*a3*b1*b3*v^2 + 
       a1*a2*b1*b2*w^2), v*(a2*a3*b2*b3*u^2 - a2*a3*b1*b3*u*v - 
       a1*a3*b2*b3*u*v + a1*a3*b1*b3*v^2 - a1*a2*b1*b2*w^2)*
      ((-a2)*a3*b2*b3*u^2 + a1*a3*b1*b3*v^2 - a1*a3*b1*b2*v*w - 
       a1*a2*b1*b3*v*w + a1*a2*b1*b2*w^2), 
     w*(a2*a3*b2*b3*u^2 - a1*a3*b1*b3*v^2 - a2*a3*b1*b2*u*w - 
       a1*a2*b2*b3*u*w + a1*a2*b1*b2*w^2)*((-a2)*a3*b2*b3*u^2 + 
       a1*a3*b1*b3*v^2 - a1*a3*b1*b2*v*w - a1*a2*b1*b3*v*w + a1*a2*b1*b2*w^2)}
 
bCircumconcevianM3[{u_, v_, w_}, {a1_, a2_, a3_}, {b1_, b2_, b3_}] := 
    {u*(a2*a3*b2*b3*u^2 + a2*a3*b1*b3*u*v + a1*a3*b2*b3*u*v + 
       a1*a3*b1*b3*v^2 + a1*a2*b1*b2*w^2)*(a2*a3*b2*b3*u^2 + 
       a2*a3*b1*b2*u*w + a1*a2*b2*b3*u*w + a1*a3*b1*b3*v^2 + 
       a1*a2*b1*b2*w^2), v*(a2*a3*b2*b3*u^2 + a2*a3*b1*b3*u*v + 
       a1*a3*b2*b3*u*v + a1*a3*b1*b3*v^2 + a1*a2*b1*b2*w^2)*
      (a2*a3*b2*b3*u^2 + a1*a3*b1*b3*v^2 + a1*a3*b1*b2*v*w + 
       a1*a2*b1*b3*v*w + a1*a2*b1*b2*w^2), 
     w*(a2*a3*b2*b3*u^2 + a1*a3*b1*b3*v^2 + a2*a3*b1*b2*u*w + 
       a1*a2*b2*b3*u*w + a1*a2*b1*b2*w^2)*(a2*a3*b2*b3*u^2 + 
       a1*a3*b1*b3*v^2 + a1*a3*b1*b2*v*w + a1*a2*b1*b3*v*w + a1*a2*b1*b2*w^2)}
 
bPerspectorGeneric[mx_, pa_, pb_, pc_] := Module[{pla, plb, plc, a1, b1}, 
     pla = bPolar[mx, pa]; plb = bPolar[mx, pb]; plc = bPolar[mx, pc]; 
      a1 = bLineIntersection[plb, plc]; b1 = bLineIntersection[plc, pla]; 
      bLineIntersection[bLine[pa, a1], bLine[pb, b1]]]
 
b4thConicIntersection[{x1_, y1_, z1_}, {x2_, y2_, z2_}, {x3_, y3_, z3_}, 
     {ix_, iy_, iz_}, {hx_, hy_, hz_}] := 
    {hz^2*(3*iy^2*x1*x2*x3 - 2*ix*iy*(x2*x3*y1 + x1*x3*y2 + x1*x2*y3) + 
        ix^2*(x3*y1*y2 + x2*y1*y3 + x1*y2*y3)) + 
      hy^2*(3*iz^2*x1*x2*x3 - 2*ix*iz*(x2*x3*z1 + x1*x3*z2 + x1*x2*z3) + 
        ix^2*(x3*z1*z2 + x2*z1*z3 + x1*z2*z3)) + 
      hx*hy*(-2*iz^2*(x2*x3*y1 + x1*x3*y2 + x1*x2*y3) + 
        2*iy*iz*(x2*x3*z1 + x1*x3*z2 + x1*x2*z3) + 
        ix*iz*(x3*y2*z1 + x2*y3*z1 + x3*y1*z2 + x1*y3*z2 + x2*y1*z3 + 
          x1*y2*z3) - 2*ix*iy*(x3*z1*z2 + x2*z1*z3 + x1*z2*z3)) + 
      hx^2*(iz^2*(x3*y1*y2 + x2*y1*y3 + x1*y2*y3) - 
        iy*iz*(x3*y2*z1 + x2*y3*z1 + x3*y1*z2 + x1*y3*z2 + x2*y1*z3 + 
          x1*y2*z3) + iy^2*(x3*z1*z2 + x2*z1*z3 + x1*z2*z3)) + 
      hz*(hx*(2*iy*iz*(x2*x3*y1 + x1*x3*y2 + x1*x2*y3) - 
          2*ix*iz*(x3*y1*y2 + x2*y1*y3 + x1*y2*y3) - 
          2*iy^2*(x2*x3*z1 + x1*x3*z2 + x1*x2*z3) + 
          ix*iy*(x3*y2*z1 + x2*y3*z1 + x3*y1*z2 + x1*y3*z2 + x2*y1*z3 + 
            x1*y2*z3)) - hy*(6*iy*iz*x1*x2*x3 - 
          2*ix*iy*(x2*x3*z1 + x1*x3*z2 + x1*x2*z3) + 
          ix*(-2*iz*(x2*x3*y1 + x1*x3*y2 + x1*x2*y3) + 
            ix*(x3*y2*z1 + x2*y3*z1 + x3*y1*z2 + x1*y3*z2 + x2*y1*z3 + 
              x1*y2*z3)))), 
     hz^2*(3*ix^2*y1*y2*y3 + iy^2*(x2*x3*y1 + x1*x3*y2 + x1*x2*y3) - 
        2*ix*iy*(x3*y1*y2 + x2*y1*y3 + x1*y2*y3)) + 
      hy^2*(iz^2*(x2*x3*y1 + x1*x3*y2 + x1*x2*y3) - 
        ix*iz*(x3*y2*z1 + x2*y3*z1 + x3*y1*z2 + x1*y3*z2 + x2*y1*z3 + 
          x1*y2*z3) + ix^2*(y3*z1*z2 + y2*z1*z3 + y1*z2*z3)) + 
      hx*hy*(-2*iz^2*(x3*y1*y2 + x2*y1*y3 + x1*y2*y3) + 
        iy*iz*(x3*y2*z1 + x2*y3*z1 + x3*y1*z2 + x1*y3*z2 + x2*y1*z3 + 
          x1*y2*z3) + 2*ix*iz*(y2*y3*z1 + y1*y3*z2 + y1*y2*z3) - 
        2*ix*iy*(y3*z1*z2 + y2*z1*z3 + y1*z2*z3)) + 
      hx^2*(3*iz^2*y1*y2*y3 - 2*iy*iz*(y2*y3*z1 + y1*y3*z2 + y1*y2*z3) + 
        iy^2*(y3*z1*z2 + y2*z1*z3 + y1*z2*z3)) + 
      hz*(hy*(-2*iy*iz*(x2*x3*y1 + x1*x3*y2 + x1*x2*y3) + 
          2*ix*iz*(x3*y1*y2 + x2*y1*y3 + x1*y2*y3) + 
          ix*iy*(x3*y2*z1 + x2*y3*z1 + x3*y1*z2 + x1*y3*z2 + x2*y1*z3 + 
            x1*y2*z3) - 2*ix^2*(y2*y3*z1 + y1*y3*z2 + y1*y2*z3)) - 
        hx*(6*ix*iz*y1*y2*y3 + iy^2*(x3*y2*z1 + x2*y3*z1 + x3*y1*z2 + 
            x1*y3*z2 + x2*y1*z3 + x1*y2*z3) - 2*iy*(iz*x3*y1*y2 + 
            iz*x2*y1*y3 + iz*x1*y2*y3 + ix*y2*y3*z1 + ix*y1*y3*z2 + 
            ix*y1*y2*z3))), hz^2*(iy^2*(x2*x3*z1 + x1*x3*z2 + x1*x2*z3) - 
        ix*iy*(x3*y2*z1 + x2*y3*z1 + x3*y1*z2 + x1*y3*z2 + x2*y1*z3 + 
          x1*y2*z3) + ix^2*(y2*y3*z1 + y1*y3*z2 + y1*y2*z3)) + 
      hy^2*(3*ix^2*z1*z2*z3 + iz^2*(x2*x3*z1 + x1*x3*z2 + x1*x2*z3) - 
        2*ix*iz*(x3*z1*z2 + x2*z1*z3 + x1*z2*z3)) + 
      hx^2*(3*iy^2*z1*z2*z3 + iz^2*(y2*y3*z1 + y1*y3*z2 + y1*y2*z3) - 
        2*iy*iz*(y3*z1*z2 + y2*z1*z3 + y1*z2*z3)) - 
      hx*hy*(6*ix*iy*z1*z2*z3 + iz^2*(x3*y2*z1 + x2*y3*z1 + x3*y1*z2 + 
          x1*y3*z2 + x2*y1*z3 + x1*y2*z3) - 2*iz*(iy*x3*z1*z2 + ix*y3*z1*z2 + 
          iy*x2*z1*z3 + ix*y2*z1*z3 + iy*x1*z2*z3 + ix*y1*z2*z3)) + 
      hz*(hx*(iy*iz*(x3*y2*z1 + x2*y3*z1 + x3*y1*z2 + x1*y3*z2 + x2*y1*z3 + 
            x1*y2*z3) - 2*ix*iz*(y2*y3*z1 + y1*y3*z2 + y1*y2*z3) - 
          2*iy^2*(x3*z1*z2 + x2*z1*z3 + x1*z2*z3) + 
          2*ix*iy*(y3*z1*z2 + y2*z1*z3 + y1*z2*z3)) + 
        hy*(-2*iy*iz*(x2*x3*z1 + x1*x3*z2 + x1*x2*z3) + 
          2*ix*iy*(x3*z1*z2 + x2*z1*z3 + x1*z2*z3) + 
          ix*(iz*(x3*y2*z1 + x2*y3*z1 + x3*y1*z2 + x1*y3*z2 + x2*y1*z3 + 
              x1*y2*z3) - 2*ix*(y3*z1*z2 + y2*z1*z3 + y1*z2*z3))))}
 
bLineIntersectionETC[n1_, n2_, n3_, n4_] := bLineIntersection[
     bLine[KimberlingCenterC[n1], KimberlingCenterC[n2]], 
     bLine[KimberlingCenterC[n3], KimberlingCenterC[n4]]]
 
bFootPerpendicular[line_, {u_, v_, w_}] := 
    Module[{h}, h = bPerpendicular[line, {u, v, w}]; 
      bLineIntersection[h, line]]
 
bPerspectivityAxis[a1_, b1_, c1_, a2_, b2_, c2_] := 
    Module[{p1, p2}, p1 = bLineIntersection[bLine[a1, b1], bLine[a2, b2]]; 
      p2 = bLineIntersection[bLine[a1, c1], bLine[a2, c2]]; bLine[p1, p2]]
 
bPerspeconic[a1_, b1_, c1_, a2_, b2_, c2_] := Module[{p1, p2, p3, p4, p5}, 
     p1 = bLineIntersection[bLine[a1, b1], bLine[a2, c2]]; 
      p2 = bLineIntersection[bLine[a1, b1], bLine[b2, c2]]; 
      p3 = bLineIntersection[bLine[a1, c1], bLine[a2, b2]]; 
      p4 = bLineIntersection[bLine[a1, c1], bLine[b2, c2]]; 
      p5 = bLineIntersection[bLine[b1, c1], bLine[a2, b2]]; 
      bFivePointConicEq[p1, p2, p3, p4, p5]]
 
bEllipse[f1_, f2_, k_] := Module[{expr}, 
     expr = -(a^2*((-r1)*(x + y) + (p1 + q1)*z)*((p1 + r1)*y - q1*(x + z)) + 
           b^2*((-r1)*(x + y) + (p1 + q1)*z)*(q1*x + r1*x - p1*(y + z)) + 
           c^2*((p1 + r1)*y - q1*(x + z))*(q1*x + r1*x - p1*(y + z)))^2 - 
        (k^2*(x + y + z)^2 + (r2*(x + y) - (p2 + q2)*z)*
           (a^2*((p2 + r2)*y - q2*(x + z)) + b^2*(q2*x + r2*x - 
              p2*(y + z))) + c^2*(q2^2*x*(x + z) + (p2 + r2)*y*
             ((-r2)*x + p2*(y + z)) + q2*(r2*x*(x - y + z) - p2*(z*(y + z) + 
                x*(2*y + z)))))^2 - 2*(k^2*(x + y + z)^2 + 
          (r2*(x + y) - (p2 + q2)*z)*(a^2*((p2 + r2)*y - q2*(x + z)) + 
            b^2*(q2*x + r2*x - p2*(y + z))) + c^2*(q2^2*x*(x + z) + 
            (p2 + r2)*y*((-r2)*x + p2*(y + z)) + q2*(r2*x*(x - y + z) - 
              p2*(z*(y + z) + x*(2*y + z)))))*
         (a^2*((-r1)*(x + y) + (p1 + q1)*z)*((p1 + r1)*y - q1*(x + z)) + 
          b^2*((-r1)*(x + y) + (p1 + q1)*z)*(q1*x + r1*x - p1*(y + z)) + 
          c^2*((p1 + r1)*y - q1*(x + z))*(q1*x + r1*x - p1*(y + z))) - 
        4*k^2*(x + y + z)^2*(a^2*((-r2)*(x + y) + (p2 + q2)*z)*
           ((p2 + r2)*y - q2*(x + z)) + b^2*((-r2)*(x + y) + (p2 + q2)*z)*
           (q2*x + r2*x - p2*(y + z)) + c^2*((p2 + r2)*y - q2*(x + z))*
           (q2*x + r2*x - p2*(y + z))); pr = PolynomialReduce[expr, 
        (x + y + z)^2, {x, y, z}]; 
      pr[[1]][[1]] /. Thread[{p1, q1, r1} -> f1/Total[f1]] /. 
       Thread[{p2, q2, r2} -> f2/Total[f2]]]
 
bIsOrthologic[pa_, pb_, pc_, xa_, xb_, xc_] := bConcurrencyMatrix[
     bPerpendicular[bLine[xb, xc], pa], bPerpendicular[bLine[xa, xc], pb], 
     bPerpendicular[bLine[xa, xb], pc]]
 
bIsOrthologic[{pa_, pb_, pc_}, {xa_, xb_, xc_}] := 
    bConcurrencyMatrix[bPerpendicular[bLine[xb, xc], pa], 
     bPerpendicular[bLine[xa, xc], pb], bPerpendicular[bLine[xa, xb], pc]]
 
bHatzipolakisMoses[{p_, q_, r_}] := 
    {c^2*(a^2 + b^2 - c^2)*(a^2 - b^2 + c^2)*p*q + b^2*(a^2 + b^2 - c^2)*
       (a^2 - b^2 + c^2)*p*r - 2*a^2*((-a^2)*b^2 + b^4 - a^2*c^2 - 
        2*b^2*c^2 + c^4)*q*r, c^2*(a^2 + b^2 - c^2)*(-a^2 + b^2 + c^2)*p*q - 
      2*b^2*(a^4 - a^2*b^2 - 2*a^2*c^2 - b^2*c^2 + c^4)*p*r + 
      a^2*(a^2 + b^2 - c^2)*(-a^2 + b^2 + c^2)*q*r, 
     -2*c^2*(a^4 - 2*a^2*b^2 + b^4 - a^2*c^2 - b^2*c^2)*p*q + 
      b^2*(a^2 - b^2 + c^2)*(-a^2 + b^2 + c^2)*p*r + 
      a^2*(a^2 - b^2 + c^2)*(-a^2 + b^2 + c^2)*q*r}
 
bFregierTriangle[{u_, v_, w_}] := {{SA*u, (-SA)*v + b^2*w, c^2*v - SA*w}, 
     {(-SB)*u + a^2*w, SB*v, c^2*u - SB*w}, {(-SC)*u + a^2*v, b^2*u - SC*v, 
      SC*w}}
 
bFuhrmannTriangle[{u_, v_, w_}] := symmetrizeTriangleExprType2Bary[
     {a^2*v*w, v*((-a^2)*w + c^2*(v + w)), w*((-a^2)*v + b^2*(v + w))}]
 
bZayinConjugate[{u_, v_, w_}, {p_, q_, r_}] := 
    {a*((-b^2)*r*(c*p + a*r)*u + a*c*q^2*((-c)*u + a*w) + 
       b*((-c^2)*p*q*u + a^2*r^2*v + a*c*(-2*q*r*u + p*r*v + p*q*w))), 
     b*((-a^2)*r*(c*q + b*r)*v + b*c*p^2*((-c)*v + b*w) + 
       a*(b^2*r^2*u - c^2*p*q*v + b*c*(q*r*u - 2*p*r*v + p*q*w))), 
     c*((-a^2)*q*(c*q + b*r)*w + b*c*p^2*(c*v - b*w) + 
       a*(c^2*q^2*u - b^2*p*r*w + b*c*(q*r*u + p*r*v - 2*p*q*w)))}
 
bWawConjugate[{u_, v_, w_}, {p_, q_, r_}] := 
    {u*(2*r^2*u^2*v^2 - r*u*v*(4*q*u + p*v)*w + 
       (2*q^2*u^2 - p*q*u*v + p^2*v^2)*w^2), 
     v*(2*r^2*u^2*v^2 - r*u*v*(q*u + 4*p*v)*w + 
       (q^2*u^2 - p*q*u*v + 2*p^2*v^2)*w^2), 
     w*(r^2*u^2*v^2 - r*u*v*(q*u + p*v)*w + 2*(q*u - p*v)^2*w^2)}
 
symmetrizeInternal3[eq_] := Module[{partB, partC}, Clear[p, q, r, u, v, w]; 
      partB = eq /. {p -> q, q -> r, r -> p, u -> v, v -> w, w -> u, x -> y, 
         y -> z, z -> x}; partB = partB /. {a -> b, b -> c, c -> a, 
         angleA -> angleB, angleB -> angleC, angleC -> angleA, A -> B, 
         B -> C, C -> A, sa -> sb, sb -> sc, sc -> sa, SA -> SB, SB -> SC, 
         SC -> SA}; partC = partB /. {p -> q, q -> r, r -> p, u -> v, v -> w, 
         w -> u, x -> y, y -> z, z -> x}; 
      partC = partC /. {a -> b, b -> c, c -> a, angleA -> angleB, 
         angleB -> angleC, angleC -> angleA, A -> B, B -> C, C -> A, 
         sa -> sb, sb -> sc, sc -> sa, SA -> SB, SB -> SC, SC -> SA}; 
      {eq, partB, partC}]
 
radicalAxis[cent1_, rad1_, cent2_, rad2_] := 
    Module[{pa1, pb1, pc1, pa2, pb2, pc2}, 
     pa1 = Simplify[bDistance[xA, cent1]^2 - rad1^2]; 
      pb1 = Simplify[bDistance[xB, cent1]^2 - rad1^2]; 
      pc1 = Simplify[bDistance[xC, cent1]^2 - rad1^2]; 
      pa2 = Simplify[bDistance[xA, cent2]^2 - rad2^2]; 
      pb2 = Simplify[bDistance[xB, cent2]^2 - rad2^2]; 
      pc2 = Simplify[bDistance[xC, cent2]^2 - rad2^2]; 
      Simplify[{pa1 - pa2, pb1 - pb2, pc1 - pc2}]]
 
bPointByAngles[ang1_, ang2_] := {{-a^2, SC + S*Cot[ang1], SB + S*Cot[ang2]}, 
     {SC + S*Cot[ang2], -b^2, SA + S*Cot[ang1]}, {SB + S*Cot[ang1], 
      SA + S*Cot[ang2], -c^2}}
 
bLozadaPerspector[{u_, v_, w_}] := {(-a + b + c)*u^2, (a - b + c)*v^2, 
     (a + b - c)*w^2}
 
bLozadaPerspector2nd[{u_, v_, w_}] := {(a + b - c)*(a - b + c)*u^2, 
     (a + b - c)*(-a + b + c)*v^2, (a - b + c)*(-a + b + c)*w^2}
 
multiCollectFactors[expr_, vars_] := Times @@ (#1[[1]]^#1[[2]] & ) /@ 
      (multiCollect[#1, vars] & ) /@ FactorList[expr]
 
bOrthologyCenter[pa_, pb_, pc_, xa_, xb_, xc_] := 
    bLineIntersection[bPerpendicular[bLine[xb, xc], pa], 
     bPerpendicular[bLine[xa, xc], pb]]
 
bOrthologyCenter[{pa_, pb_, pc_}, {xa_, xb_, xc_}] := 
    bLineIntersection[bPerpendicular[bLine[xb, xc], pa], 
     bPerpendicular[bLine[xa, xc], pb]]
 
bIsPerspective[a1_, b1_, c1_, a2_, b2_, c2_] := bConcurrencyMatrix[
     bLine[a1, a2], bLine[b1, b2], bLine[c1, c2]]
 
bIsPerspective[{a1_, b1_, c1_}, {a2_, b2_, c2_}] := 
    bConcurrencyMatrix[bLine[a1, a2], bLine[b1, b2], bLine[c1, c2]]
 
bCurveForTriangle[crv_, va_, vb_, vc_] := 
    Module[{xa, ya, za, xb, yb, zb, xc, yc, zc, rls, a1, b1, c1, repl}, 
     {a1, b1, c1} = {{xa, ya, za}, {xb, yb, zb}, {xc, yc, zc}}; 
      repl = {((xa + ya + za)*((-xc)*yb*z + xb*yc*z + xc*y*zb - x*yc*zb - 
           xb*y*zc + x*yb*zc))/((-xc)*yb*za + xb*yc*za + xc*ya*zb - 
          xa*yc*zb - xb*ya*zc + xa*yb*zc), 
        ((xb + yb + zb)*((-xc)*ya*z + xa*yc*z + xc*y*za - x*yc*za - xa*y*zc + 
           x*ya*zc))/(xc*yb*za - xb*yc*za - xc*ya*zb + xa*yc*zb + xb*ya*zc - 
          xa*yb*zc), (((-xb)*ya*z + xa*yb*z + xb*y*za - x*yb*za - xa*y*zb + 
           x*ya*zb)*(xc + yc + zc))/((-xc)*yb*za + xb*yc*za + xc*ya*zb - 
          xa*yc*zb - xb*ya*zc + xa*yb*zc)}; 
      rls = Thread[{xa, ya, za, xb, yb, zb, xc, yc, zc} -> 
         Flatten[{va, vb, vc}]]; crv /. setupBaseTriangleBary[a1, b1, c1] /. 
        Thread[{x, y, z} -> repl] /. rls]
 
bSteinerImage[{p_, q_, r_}] := {p/(-p^2 + p*q + q^2 + p*r + q*r + r^2), 
     q/(p^2 + p*q - q^2 + p*r + q*r + r^2), 
     r/(p^2 + p*q + q^2 + p*r + q*r - r^2)}
 
bHodpiece[{p_, q_, r_}] := {a^2/(p*(-(a^2/p) + b^2/q + c^2/r)), 
     b^2/(q*(a^2/p - b^2/q + c^2/r)), c^2/((a^2/p + b^2/q - c^2/r)*r)}
 
bXAntipodeInCircumconic[{p_, q_, r_}, {x_, y_, z_}, {u_, v_, w_}] := 
    {q*r*u^2*(r*v*x + q*w*x + p*w*y)*(r*v*x + q*w*x + p*v*z), 
     p*r*v^2*(q*w*x + r*u*y + p*w*y)*(r*u*y + p*w*y + q*u*z), 
     p*q*w^2*(r*v*x + q*u*z + p*v*z)*(r*u*y + q*u*z + p*v*z)}
 
bCircleInvervse[circ_, pt_] := bLineIntersection[bPolar[circ, pt], 
     bLine[bConicCenter[circ], pt]]
 
bAubertCenterSimplify[aa_, bb_, cc_, dd_] := Module[{l1, l2}, 
     l1 = Simplify[Simplify[bAubertLine[aa, bb, cc, dd]] /. Abs -> RealAbs]; 
      l2 = Simplify[Simplify[bAubertLine[aa, bb, dd, cc]] /. Abs -> RealAbs]; 
      simplifyRationalBarycentrics[bLineIntersection[l1, l2]]]
 
bVuCirclesPoint[{p_, q_, r_}, {u_, v_, w_}] := 
    {b^2*p*u*(r*(u + v) - (p + q)*w) + c^2*p*u*((-p - r)*v + q*(u + w)) + 
      a^2*((-p)*(p + q + r)*v*w + q*r*u*(u + v + w)), 
     a^2*q*v*(r*(u + v) + (-p - q)*w) + c^2*q*v*(-((q + r)*u) + p*(v + w)) + 
      b^2*((-q)*(p + q + r)*u*w + p*r*v*(u + v + w)), 
     a^2*r*w*(-((p + r)*v) + q*(u + w)) + b^2*r*w*((-q - r)*u + p*(v + w)) + 
      c^2*((-r)*(p + q + r)*u*v + p*q*w*(u + v + w))}
 
bVuCirclesPoint2[{p_, q_, r_}, {u_, v_, w_}] := 
    {q*r*((-b^2)*p*u*((-r)*(u + v) + (p + q)*w) - 
       c^2*p*u*((p + r)*v - q*(u + w)) + a^2*((-p)*(p + q + r)*v*w + 
         q*r*u*(u + v + w))), p*r*((-a^2)*q*v*((-r)*(u + v) + (p + q)*w) - 
       c^2*q*v*((q + r)*u - p*(v + w)) + b^2*((-q)*(p + q + r)*u*w + 
         p*r*v*(u + v + w))), p*q*((-a^2)*r*w*((p + r)*v - q*(u + w)) - 
       b^2*r*w*((q + r)*u - p*(v + w)) + c^2*((-r)*(p + q + r)*u*v + 
         p*q*w*(u + v + w)))}
 
bAnticevianIntersectionConicCenter[{u_, v_, w_}, {p_, q_, r_}] := 
    {p*v*w*((-q^2)*u*(2*r*u + p*w) + p*v*((-r^2)*u + p^2*w) - 
       2*q*r*u*(r*u + p*(v + w))), q*u*w*((-r^2)*v*(q*u + 2*p*v) + 
       q*(q^2*u - p^2*v)*w - 2*p*r*v*(p*v + q*(u + w))), 
     r*u*v*(-2*p*q*w*(r*(u + v) + q*w) - p^2*w*(r*v + 2*q*w) + 
       r*u*(r^2*v - q^2*w))}
 
bAnticevianIntersectionConicPerspector[{u_, v_, w_}, {p_, q_, r_}] := 
    {p*(2*q*r*u + 2*p*r*v + p*q*w)*(p*r*v + 2*q*(r*u + p*w)), 
     q*(2*r*(q*u + p*v) + p*q*w)*(q*r*u + 2*p*r*v + 2*p*q*w), 
     r*(2*q*r*u + p*r*v + 2*p*q*w)*(q*r*u + 2*p*(r*v + q*w))}
 
bParallelsConicCenter[{u_, v_, w_}] := {u*(u^2 - 2*v*w - u*(v + w)), 
     v*(v^2 - 2*u*w - v*(u + w)), w*(-2*u*v - (u + v)*w + w^2)}
 
bParallelsConicPerspector[{u_, v_, w_}] := 
    {u*(2*v*w + u*(2*v + w))*(2*v*w + u*(v + 2*w)), 
     v*(2*u*w + v*(2*u + w))*(2*u*w + v*(u + 2*w)), 
     w*(2*u*v + (2*u + v)*w)*(2*u*v + (u + 2*v)*w)}
 
bInverseInConic[ptP_, mx_] := bLineIntersection[bPolar[mx, ptP], 
     bLine[ptP, bConicCenter[mx]]]
 
bDC[{u_, v_, w_}] := {(v*w)/(b*v + c*w), (u*w)/(a*u + c*w), (u*v)/(a*u + b*v)}
 
bCD[{p_, q_, r_}] := {b*c*((-c)*p*q + b*p*r - a*q*r)*
      ((-c)*p*q + b*p*r + a*q*r), a*c*((-c)*p*q - b*p*r + a*q*r)*
      ((-c)*p*q + b*p*r + a*q*r), a*b*((-c)*p*q - b*p*r + a*q*r)*
      (c*p*q - b*p*r + a*q*r)}
 
bPolluxPoint[h_, k_] := {(b - c)*(a*h + (b + c)*k), 
     (-a + c)*(b*h + (a + c)*k), (a - b)*(c*h + (a + b)*k)}
 
bAreLinePerpendicular[{x1_, y1_, z1_}, {x2_, y2_, z2_}] := 
    a^2*x1*x2 + b^2*y1*y2 + c^2*z1*z2 - SA*(y1*z2 + y2*z1) - 
     SB*(z1*x2 + z2*x1) - SC*(x1*y2 + x2*y1)
 
bCircleOnDiameter[U1_, U2_] := curveSimplify[
     evaluate[bAreLinePerpendicular[bLine[{x, y, z}, U1], 
       bLine[{x, y, z}, U2]]]]
 
laHireRadicalCenter[func_] := Module[{e}, e = symmetrizeInternal2[func]; 
      radicalCenter[xA, e[[1]], xB, e[[2]], xC, e[[3]]]]
 
bSecondCircumconcevianTangentialPerspector[{u_, v_, w_}, {p_, q_, r_}] := 
    {-2*q*r*u^2 + p^2*v*w - p*u*(r*v + q*w), -2*p*r*v^2 + q^2*u*w - 
      q*v*(r*u + p*w), r^2*u*v - r*(q*u + p*v)*w - 2*p*q*w^2}
 
bVuCevianTangentialPerspector[{u_, v_, w_}] := {a^2*v*(u + v)*w*(u + w), 
     b^2*u*(u + v)*w*(v + w), c^2*u*v*(u + w)*(v + w)}
 
bCircumconcevianTriangle[{u_, v_, w_}, {p_, q_, r_}] := 
    {{q*r*u, (-q)*(r*v + q*w), (-r)*(r*v + q*w)}, {(-p)*(r*u + p*w), p*r*v, 
      (-r)*(r*u + p*w)}, {(-p)*(q*u + p*v), (-q)*(q*u + p*v), p*q*w}}
 
bMiquelAssociate[{u_, v_, w_}] := 
    {a^2*(u + v)*(u + w)*((-c^2)*u*v*(u + w)*(v + w) + 
       (u + v)*w*(a^2*v*(u + w) - b^2*u*(v + w))), 
     b^2*(u + v)*(v + w)*((-c^2)*u*v*(u + w)*(v + w) - 
       (u + v)*w*(a^2*v*(u + w) - b^2*u*(v + w))), 
     c^2*(u + w)*(v + w)*(c^2*u*v*(u + w)*(v + w) - 
       (u + v)*w*(a^2*v*(u + w) + b^2*u*(v + w)))}
 
bHaimovTriangle[{u_, v_, w_}] := 
    {{b^2*u*(u + v) + c^2*u*(u + w) - a^2*(u + v)*(u + w), 
      b^2*(u + v)*(u + v + w), c^2*(u + w)*(u + v + w)}, 
     {a^2*(u + v)*(u + v + w), a^2*v*(u + v) + c^2*v*(v + w) - 
       b^2*(u + v)*(v + w), c^2*(v + w)*(u + v + w)}, 
     {a^2*(u + w)*(u + v + w), b^2*(v + w)*(u + v + w), 
      a^2*w*(u + w) + b^2*w*(v + w) - c^2*(u + w)*(v + w)}}
 
bTripolarCentroid[{p_, q_, r_}] := {p*(-2*p + q + r)*(r - q), 
     q*(r - p)*(-p + 2*q - r), (q - p)*(p + q - 2*r)*r}
 
bCurveForTriangleNoReplace[crv_, va_, vb_, vc_] := 
    Module[{xa, ya, za, xb, yb, zb, xc, yc, zc, rls, repl}, 
     repl = {((xa + ya + za)*((-xc)*yb*z + xb*yc*z + xc*y*zb - x*yc*zb - 
           xb*y*zc + x*yb*zc))/((-xc)*yb*za + xb*yc*za + xc*ya*zb - 
          xa*yc*zb - xb*ya*zc + xa*yb*zc), 
        ((xb + yb + zb)*((-xc)*ya*z + xa*yc*z + xc*y*za - x*yc*za - xa*y*zc + 
           x*ya*zc))/(xc*yb*za - xb*yc*za - xc*ya*zb + xa*yc*zb + xb*ya*zc - 
          xa*yb*zc), (((-xb)*ya*z + xa*yb*z + xb*y*za - x*yb*za - xa*y*zb + 
           x*ya*zb)*(xc + yc + zc))/((-xc)*yb*za + xb*yc*za + xc*ya*zb - 
          xa*yc*zb - xb*ya*zc + xa*yb*zc)}; 
      rls = Thread[{xa, ya, za, xb, yb, zb, xc, yc, zc} -> 
         Flatten[{va, vb, vc}]]; crv /. Thread[{x, y, z} -> repl] /. rls]
 
bTrianglePerspector[a1_, b1_, c1_, a2_, b2_, c2_] := 
    bLineIntersection[bLine[a1, a2], bLine[b1, b2]]
 
bTrianglePerspector[{a1_, b1_, c1_}, {a2_, b2_, c2_}] := 
    bLineIntersection[bLine[a1, a2], bLine[b1, b2]]
 
bTangentialTriangle[crv_, a1_, b1_, c1_] := Module[{ta, tb, tc, a2, b2, c2}, 
     ta = simplifyRationalBarycentrics[bPolar[crv, a1]]; 
      tb = simplifyRationalBarycentrics[bPolar[crv, b1]]; 
      tc = simplifyRationalBarycentrics[bPolar[crv, c1]]; 
      a2 = simplifyRationalBarycentrics[bLineIntersection[tb, tc]]; 
      b2 = simplifyRationalBarycentrics[bLineIntersection[tc, ta]]; 
      c2 = simplifyRationalBarycentrics[bLineIntersection[ta, tb]]; 
      {a2, b2, c2}]
 
bobillierTransversal[pp_, q1_, q2_, q3_] := Module[{h1, h2, h3, m1, m2, m3}, 
     h1 = bPerpendicular[bLine[pp, q1], pp]; 
      h2 = bPerpendicular[bLine[pp, q2], pp]; 
      h3 = bPerpendicular[bLine[pp, q3], pp]; 
      m1 = bLineIntersection[bLine[q2, q3], h1]; 
      m2 = bLineIntersection[bLine[q1, q3], h2]; 
      m3 = bLineIntersection[bLine[q1, q2], h3]; bLine[m1, m2]]
 
multiCollectFactorsABC[pt_] := {multiCollectFactors[pt[[1]], a], 
     multiCollectFactors[pt[[2]], b], multiCollectFactors[pt[[3]], c]}
 
multiCollectABC[pt_] := {multiCollect[pt[[1]], a], multiCollect[pt[[2]], b], 
     multiCollect[pt[[3]], c]}
 
bPCevianTriangle[{u_, v_, w_}, {p_, q_, r_}] := 
    {{0, p*v + q*(v + w), p*w + r*(v + w)}, {q*u + p*(u + w), 0, 
      q*w + r*(u + w)}, {r*u + p*(u + v), r*v + q*(u + v), 0}}
 
bIsParallelogic[pa_, pb_, pc_, xa_, xb_, xc_] := 
    Simplify[bConcurrencyMatrix[bParallelLine[pa, bLine[xb, xc]], 
      bParallelLine[pb, bLine[xa, xc]], bParallelLine[pc, bLine[xa, xb]]]]
 
bParallelogicCenter[pa_, pb_, pc_, xa_, xb_, xc_] := 
    bLineIntersection[bParallelLine[pa, bLine[xb, xc]], 
     bParallelLine[pb, bLine[xa, xc]]]
 
bTangentialTriangle2[crv_, a1_, b1_, c1_] := Module[{ta, tb, tc, a2, b2, c2}, 
     ta = bLineFromEq[simplifyRationalBarycentrics[bImplicitTangent[crv, 
          a1]]]; tb = bLineFromEq[simplifyRationalBarycentrics[
         bImplicitTangent[crv, b1]]]; 
      tc = bLineFromEq[simplifyRationalBarycentrics[bImplicitTangent[crv, 
          c1]]]; a2 = simplifyRationalBarycentrics[bLineIntersection[tb, 
         tc]]; b2 = simplifyRationalBarycentrics[bLineIntersection[tc, ta]]; 
      c2 = simplifyRationalBarycentrics[bLineIntersection[ta, tb]]; 
      {a2, b2, c2}]
 
bPedalGeneral[pp_, p1_, p2_, p3_] := Module[{h1, h2, h3}, 
     h1 = Simplify[bLineIntersection[bPerpendicular[bLine[p2, p3], pp], 
         bLine[p2, p3]]]; h2 = Simplify[bLineIntersection[
         bPerpendicular[bLine[p1, p3], pp], bLine[p1, p3]]]; 
      h3 = Simplify[bLineIntersection[bPerpendicular[bLine[p1, p2], pp], 
         bLine[p1, p2]]]; {h1, h2, h3}]
 
bReflectionLL[{p_, q_, r_}, {l_, m_, n_}] := 
    {b^2*(m^2*p - m*n*p + l*n*q + l^2*(q - r) + l*m*(-2*q + r)) + 
      c^2*(n^2*p + l*n*(q - 2*r) + l^2*(-q + r) + m*((-n)*p + l*r)) - 
      a^2*((-m)*n*p + l^2*(p - q - r) + l*(n*q + m*r)), 
     a^2*(m*n*p + l^2*q - l*n*q + m^2*(p - r) + l*m*(-2*p + r)) + 
      c^2*(n*(-l + n)*q + m*n*(p - 2*r) + l*m*r + m^2*(-p + r)) + 
      b^2*(l*n*q + m^2*(p - q + r) - m*(n*p + l*r)), 
     a^2*(m*n*p + n^2*(p - q) + l*n*(-2*p + q) + l^2*r - l*m*r) + 
      b^2*(m*n*(p - 2*q) + l*n*q + n^2*(-p + q) - l*m*r + m^2*r) + 
      c^2*(n*((-l)*q + n*(p + q - r)) + m*((-n)*p + l*r))}
 
bSideReflectionTriangle[x1_, x2_, x3_, y1_, y2_, y3_] := 
    Module[{l1, l2, l3, pa, pb, pc}, 
     l1 = simplifyRationalBarycentrics[bReflectionLL[bLine[x2, x3], 
         bLine[y2, y3]]]; l2 = simplifyRationalBarycentrics[
        bReflectionLL[bLine[x1, x3], bLine[y1, y3]]]; 
      l3 = simplifyRationalBarycentrics[bReflectionLL[bLine[x1, x2], 
         bLine[y1, y2]]]; pa = simplifyRationalBarycentrics[
        bLineIntersection[l2, l3]]; pb = simplifyRationalBarycentrics[
        bLineIntersection[l1, l3]]; pc = simplifyRationalBarycentrics[
        bLineIntersection[l1, l2]]; {pa, pb, pc}]
 
polarCurve[crv_, pt_] := pt . Grad[crv, {x, y, z}]
 
bPAntipedal[{u_, v_, w_}, {p_, q_, r_}] := 
    {{p*(r*v + q*(u + v))*(q*w + r*(u + w)), (-q)*(r*v + q*(u + v))*
       (p*w + r*(v + w)), (-r)*(q*w + r*(u + w))*(p*v + q*(v + w))}, 
     {p*(r*u + p*(u + v))*(q*w + r*(u + w)), (-q)*(r*u + p*(u + v))*
       (p*w + r*(v + w)), r*(q*u + p*(u + w))*(p*w + r*(v + w))}, 
     {p*(r*v + q*(u + v))*(q*u + p*(u + w)), q*(r*u + p*(u + v))*
       (p*v + q*(v + w)), (-r)*(q*u + p*(u + w))*(p*v + q*(v + w))}}
 
mixedPolarLine[cubic_, ptP_, ptQ_] := bPolar[polarCurve[cubic, ptQ], ptP]
 
bCrossCevianTriangle[{u_, v_, w_}, {p_, q_, r_}] := 
    {{u*(r*v + q*w), q*v*w, r*v*w}, {p*u*w, v*(r*u + p*w), r*u*w}, 
     {p*u*v, q*u*v, (q*u + p*v)*w}}
 
bExsimilicenter3[O1_, rad1_, O2_, rad2_] := rad1*(O2/Total[O2]) - 
     rad2*(O1/Total[O1])
 
bCircumconcevianM4[{u_, v_, w_}, {a1_, a2_, a3_}, {b1_, b2_, b3_}] := 
    simplifyRationalBarycentrics[
     {u/(u^2/(a1*b1) + (v/a2 - w/a3)*(v/b2 - w/b3)), 
      v/(v^2/(a2*b2) + (u/a1 - w/a3)*(u/b1 - w/b3)), 
      w/(w^2/(a3*b3) + (u/a1 - v/a2)*(u/b1 - v/b2))}]
 
bCevianTriangleGeneral[xp_, xa_, xb_, xc_] := Module[{la, lb, lc}, 
     la = bLine[xb, xc]; lb = bLine[xc, xa]; lc = bLine[xa, xb]; 
      {bLineIntersection[bLine[xp, xa], la], bLineIntersection[bLine[xp, xb], 
        lb], bLineIntersection[bLine[xp, xc], lc]}]
 
sym = symmetrizeInternal
 
sym2 = symmetrizeInternal2
 
sym3 = symmetrizeInternal3
 
bCubicPolar[crv_, pt_] := bPolar[polarCurve[crv, pt], pt]
 
bTripoleEqGeneral[{u1_, v1_, w1_}, {u2_, v2_, w2_}, {u3_, v3_, w3_}, 
     {p_, q_, r_}] := {3*p^2*u1*u2*u3 + q^2*(u3*v1*v2 + u2*v1*v3 + 
        u1*v2*v3) + q*r*(u3*v2*w1 + u2*v3*w1 + u3*v1*w2 + u1*v3*w2 + 
        u2*v1*w3 + u1*v2*w3) + r^2*(u3*w1*w2 + u2*w1*w3 + u1*w2*w3) + 
      2*p*(q*(u2*u3*v1 + u1*u3*v2 + u1*u2*v3) + 
        r*(u2*u3*w1 + u1*u3*w2 + u1*u2*w3)), 3*q^2*v1*v2*v3 + 
      p^2*(u2*u3*v1 + u1*u3*v2 + u1*u2*v3) + 
      2*p*q*(u3*v1*v2 + u2*v1*v3 + u1*v2*v3) + 
      p*r*(u3*v2*w1 + u2*v3*w1 + u3*v1*w2 + u1*v3*w2 + u2*v1*w3 + u1*v2*w3) + 
      2*q*r*(v2*v3*w1 + v1*v3*w2 + v1*v2*w3) + 
      r^2*(v3*w1*w2 + v2*w1*w3 + v1*w2*w3), 3*r^2*w1*w2*w3 + 
      p^2*(u2*u3*w1 + u1*u3*w2 + u1*u2*w3) + 
      p*q*(u3*v2*w1 + u2*v3*w1 + u3*v1*w2 + u1*v3*w2 + u2*v1*w3 + u1*v2*w3) + 
      q^2*(v2*v3*w1 + v1*v3*w2 + v1*v2*w3) + 
      2*p*r*(u3*w1*w2 + u2*w1*w3 + u1*w2*w3) + 
      2*q*r*(v3*w1*w2 + v2*w1*w3 + v1*w2*w3)}
 
bInfinityPoint[{p_, q_, r_}] := {q - r, r - p, p - q}
 
bAngle[p1_, p2_, p3_] := Module[{l1, l2, k}, 
     l1 = simplifyRationalBarycentrics[bLine[p1, p2]]; 
      l2 = simplifyRationalBarycentrics[bLine[p2, p3]]; 
      k = Simplify[Det[{{1, 1, 1}, l1, l2}]]; 
      Return[{Cot[A], Cot[B], Cot[C]} . (bInfinityPoint[l1]*
          bInfinityPoint[l2])/k]; ]
 
bPsiInvolution[{u_, v_, w_}] := {a^2*(u^2 - v^2 + v*w - w^2) - 
      u*(b^2*(u + v - 2*w) + c^2*(u - 2*v + w)), (-a^2)*v*(u + v - 2*w) - 
      c^2*v*(-2*u + v + w) - b^2*(u^2 - v^2 - u*w + w^2), 
     (-a^2)*w*(u - 2*v + w) - b^2*w*(-2*u + v + w) - 
      c^2*(u^2 - u*v + v^2 - w^2)}
 
prmCircumcircle = {a^2/((b - c)*(a + (a + b + c)*t)), 
     b^2/((-a + c)*(b + (a + b + c)*t)), c^2/((a - b)*(c + (a + b + c)*t))}
 
bBicevianPerspector[{u1_, v1_, w1_}, {u2_, v2_, w2_}, {u3_, v3_, w3_}] := 
    {(u1*u3*w2*(u3*v2 - u2*v3)*(v3*w1 - v1*w3))/
      (u1*v3*w2*(u3*v2*w1 + u2*v1*w3) - u3*v1*(u3*v1*w2^2 + u2*v2*w1*w3)), 
     (v2*v3*w1*(u1*v3 - u3*v1)*(u3*w2 - u2*w3))/
      (u1*v3*w2*(u3*v2*w1 - u2*v1*w3) + u3*v1*((-u3)*v1*w2^2 + u2*v2*w1*w3)), 
     (w1*w2*w3*(u1*v3 - u3*v1)*(u3*v2 - u2*v3))/
      (u1*v3*w2*(u3*v2*w1 - u2*v1*w3) + u3*v1*(u3*v1*w2^2 - u2*v2*w1*w3))}
 
bHT[{u_, v_, w_}, {p_, q_, r_}] := 
    sym3[q*r*(p/u + q/v - r/w)*(p/u - q/v + r/w) - p^2*(-(p/u) + q/v + r/w)^2]
 
bKT[{u_, v_, w_}, {p_, q_, r_}] := 
    sym3[(-p^2)*(q/u + r/v - p/w)*(r/u - p/v + q/w) + 
      q*r*(-(q/u) + r/v + p/w)*(-(r/u) + p/v + q/w)]
 
bNinePointConic[va_, vb_, vc_, vd_] := Module[{p1, p2, p3, p4, p5}, 
     p1 = simplifyRationalBarycentrics[bMidpoint[va, vb]]; 
      p2 = simplifyRationalBarycentrics[bMidpoint[va, vc]]; 
      p3 = simplifyRationalBarycentrics[bMidpoint[vc, vb]]; 
      p4 = simplifyRationalBarycentrics[bMidpoint[vd, vb]]; 
      p5 = simplifyRationalBarycentrics[bMidpoint[vd, va]]; 
      bFivePointConicEq[p1, p2, p3, p4, p5]]
 
bOrionTransform[{u_, v_, w_}] := 
    {u*(c^2*u^2*v*(v + w) + b^2*u^2*w*(v + w) - a^2*v*w*(u^2 + v*w)), 
     v*(c^2*u*v^2*(u + w) + a^2*v^2*w*(u + w) - b^2*u*w*(v^2 + u*w)), 
     w*(b^2*u*(u + v)*w^2 + a^2*v*(u + v)*w^2 - c^2*u*v*(u*v + w^2))}
 
bAntipedal[pp_, ref_:{xA, xB, xC}] := Module[{lna, lnb, lnc, ha, hb, hc}, 
     lna = simplifyRationalBarycentrics[bPerpendicular[bLine[ref[[1]], pp], 
         ref[[1]]]]; lnb = simplifyRationalBarycentrics[
        bPerpendicular[bLine[ref[[2]], pp], ref[[2]]]]; 
      lnc = simplifyRationalBarycentrics[bPerpendicular[bLine[ref[[3]], pp], 
         ref[[3]]]]; ha = simplifyRationalBarycentrics[Cross[lnb, lnc]]; 
      hb = simplifyRationalBarycentrics[Cross[lnc, lna]]; 
      hc = simplifyRationalBarycentrics[Cross[lna, lnb]]; 
      Return[{ha, hb, hc}]; ]
 
bBicevianChordal[{u_, v_, w_}, {p_, q_, r_}] := 
    symmetrizeTriangleExprType2Bary[{p*u*(r^2*v^2 + q*r*v*w + q^2*w^2), 
      (-q)*v*(r^2*u*v + 2*r*(q*u + p*v)*w + p*q*w^2), 
      (-r)*w*(p*r*v^2 + q^2*u*w + 2*q*v*(r*u + p*w))}]
 
bKirikamiEuler[{u_, v_, w_}] := 
    {u/((a^2 - b^2 - c^2)*u^2 + (a^2 - b^2 + c^2)*u*v + 
       (a^2 + b^2 - c^2)*u*w + 2*a^2*v*w), 
     v/((-a^2 + b^2 + c^2)*u*v + (-a^2 + b^2 - c^2)*v^2 + 2*b^2*u*w + 
       (a^2 + b^2 - c^2)*v*w), w/(2*c^2*u*v + (-a^2 + b^2 + c^2)*u*w + 
       (a^2 - b^2 + c^2)*v*w + (-a^2 - b^2 + c^2)*w^2)}
 
bAnticevianConic[{p_, q_, r_}, {u_, v_, w_}] := (r^2*v^2 - q^2*w^2)*x^2 + 
     ((-r^2)*u^2 + p^2*w^2)*y^2 + (q^2*u^2 - p^2*v^2)*z^2
 
bAntiparallels[pt_, v1_:xA, v2_:xB, v3_:xC] = 
    {bParallelLine[pt, bPolar[bCircleEq[v1, v2, v3], v1]], 
     bParallelLine[pt, bPolar[bCircleEq[v1, v2, v3], v2]], 
     bParallelLine[pt, bPolar[bCircleEq[v1, v2, v3], v3]]}
 
bIntersectionTriangle[la_, lb_, lc_] := {bLineIntersection[lb, lc], 
     bLineIntersection[lc, la], bLineIntersection[la, lb]}
 
bIsOrthologicSimplify[pa_, pb_, pc_, xa_, xb_, xc_] := 
    Factor[bConcurrencyMatrix[simplifyRationalBarycentrics[
       bPerpendicular[bLine[xb, xc], pa]], simplifyRationalBarycentrics[
       bPerpendicular[bLine[xa, xc], pb]], simplifyRationalBarycentrics[
       bPerpendicular[bLine[xa, xb], pc]]]]
 
bOrthopoleGen[l_, xa_:xA, xb_:xB, xc_:xC] := Module[{ha, hb, hc, la, lb}, 
     ha = bFootPerpendicular[l, xa]; hb = bFootPerpendicular[l, xb]; 
      hc = bFootPerpendicular[l, xc]; la = bPerpendicular[bLine[xb, xc], ha]; 
      lb = bPerpendicular[bLine[xa, xc], hb]; Return[Cross[la, lb]]; ]
 
bOrthopoleGenSimplify[l_, xa_:xA, xb_:xB, xc_:xC] := 
    Module[{ha, hb, hc, la, lb}, ha = simplifyRationalBarycentrics[
        partialSReplace[bFootPerpendicular[l, xa]]]; 
      hb = simplifyRationalBarycentrics[partialSReplace[bFootPerpendicular[l, 
          xb]]]; hc = simplifyRationalBarycentrics[partialSReplace[
         bFootPerpendicular[l, xc]]]; la = simplifyRationalBarycentrics[
        partialSReplace[bPerpendicular[bLine[xb, xc], ha]]]; 
      lb = simplifyRationalBarycentrics[partialSReplace[
         bPerpendicular[bLine[xa, xc], hb]]]; 
      Return[simplifyRationalBarycentrics[Cross[la, lb]]]; ]
 
bCrosspointGeneral[pp_, pq_, {xa_, xb_, xc_}, simplify_:True] := 
    Module[{smpl, a1, b1, c1, a2, b2, c2, a3, b3, c3}, 
     If[simplify, smpl = simplifyRationalBarycentrics, smpl[xvar_] = xvar]; 
      {a1, b1, c1} = smpl[bCevianTriangleGeneral[pp, xa, xb, xc]]; 
      {a2, b2, c2} = smpl[bCevianTriangleGeneral[pq, xa, xb, xc]]; 
      a3 = smpl[Cross[bLine[xa, a2], bLine[b1, c1]]]; 
      b3 = smpl[Cross[bLine[xb, b2], bLine[a1, c1]]]; 
      c3 = smpl[Cross[bLine[xc, c2], bLine[a1, b1]]]; 
      smpl[Cross[bLine[a1, a3], bLine[b1, b3]]]]
 
bAnticevianGeneral[pp_, {xa_, xb_, xc_}] := Module[{pa, pb, pc, qa, qb, qc}, 
     {pa, pb, pc} = bCevianTriangleGeneral[pp, xa, xb, xc]; 
      qa = bHarmonicConjugateFull[xa, pa, pp]; 
      qb = bHarmonicConjugateFull[xb, pb, pp]; 
      qc = bHarmonicConjugateFull[xc, pc, pp]; {qa, qb, qc}]
 
sCollect[expr_, vars_] := Activate[Collect[expr, vars, Inactive[Simplify]] /. 
      Simplify -> intFullSimplifyFactors]
