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
         {x, y, z}}]; {Coefficient[m, x], Coefficient[m, y], 
       Coefficient[m, z]}]
 
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
 
bPerpendicular[po_, l_] := Module[{sa, sb, sc, f, g, h, pp, u, v, w, p, q, r, 
      ff, gg, hh, m}, pp = po/Total[po]; p = pp[[1]]; q = pp[[2]]; 
      r = pp[[3]]; u = l[[1]]; v = l[[2]]; w = pp[[3]]; 
      sa = (b^2 + c^2 - a^2)/2; sb = (-b^2 + c^2 + a^2)/2; 
      sc = (b^2 - c^2 + a^2)/2; f = q - r; g = r - p; h = p - q; 
      ff = sb*g - sc*h; gg = sc*h - sa*f; hh = sa*f - sb*g; 
      m = Det[{{ff, gg, hh}, {u, v, w}, {x, y, z}}]; 
      {Coefficient[m, x], Coefficient[m, y], Coefficient[m, z]}]
 
cToBary[v1_, v2_, v3_, xy_] := 
    With[{mat = {{v1[[1]], v2[[1]], v3[[1]]}, {v1[[2]], v2[[2]], v3[[2]]}, 
        {1, 1, 1}}}, LinearSolve[mat, Append[xy, 1]]]
