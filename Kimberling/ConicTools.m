adjugate[m_] := Map[Reverse, Minors[Transpose[m], Length[m] - 1], {0, 1}]*
     Table[(-1)^(i + j), {i, Length[m]}, {j, Length[m]}]
 
conic5Points[pts_] := Module[{row, a, b, c, f, g, h, poly}, 
     row[{x_, y_}] := {1, x, y, x*y, x^2, y^2}; 
      poly := Det[Prepend[row /@ pts, row[{x, y}]]]; 
      a := Coefficient[poly /. {y -> 0}, x, 2]; 
      b := Coefficient[poly /. {x -> 0}, y, 2]; 
      c := poly /. {y -> 0, x -> 0}; f := Coefficient[poly /. {x -> 0}, y, 1]/
        2; g := Coefficient[poly /. {y -> 0}, x, 1]/2; 
      h := Coefficient[poly, x*y]/2; {{a, h, g}, {h, b, f}, {g, f, c}}]
 
ellipseFociEq[FX_, FY_, GX_, GY_, PX_, PY_] := Module[{d, a, b, c, f, g, h}, 
     d := Sqrt[(PX - FX)^2 + (PY - FY)^2] + Sqrt[(PX - GX)^2 + (PY - GY)^2]; 
      a := -4*(d^2 - (FX - GX)^2); b := -4*(d^2 - (FY - GY)^2); 
      c := d^4 + (FX^2 + FY^2 - GX^2 - GY^2)^2 - 
        2*d^2*(FX^2 + FY^2 + GX^2 + GY^2); 
      f := 2*(d^2*(FY + GY) - (FY - GY)*(FX^2 + FY^2 - GX^2 - GY^2)); 
      g := 2*(d^2*(FX + GX) - (FX - GX)*(FX^2 + FY^2 - GX^2 - GY^2)); 
      h := 4*(FX - GX)*(FY - GY); {{a, h, g}, {h, b, f}, {g, f, c}}]
 
ellipseFociPtEq[{FX_, FY_}, {GX_, GY_}, {PX_, PY_}] := 
    ellipseFociEq[FX, FY, GX, GY, PX, PY]
 
matrixToEq[m_] := Module[{cv, eq}, cv := {{x}, {y}, {1}}; 
      eq = First[First[Transpose[cv] . m . cv]]; 
      Simplify /@ Total[MonomialList[Expand[eq, {x, y}]]]]
 
lineEq[ax_, ay_, bx_, by_] := Det[{{x, y, 1}, {ax, ay, 1}, {bx, by, 1}}]
 
lineEqPt[{ax_, ay_}, {bx_, by_}] := Det[{{x, y, 1}, {ax, ay, 1}, {bx, by, 1}}]
 
lineIntersection[l1_, l2_] := First[{x, y} /. Solve[l1 == 0 && l2 == 0, 
       {x, y}]]
 
footPerpendicular[{x1_, y1_}, lineeq_] := Module[{a, b, c}, 
     a = Coefficient[lineeq, x]; b = Coefficient[lineeq, y]; 
      c = lineeq /. {x -> 0, y -> 0}; 
      {x1 - (a*(a*x1 + b*y1 + c))/(a^2 + b^2), 
       y1 - (b*(a*x1 + b*y1 + c))/(a^2 + b^2)}]
 
tangentEq[a_, b_, c_, d_, e_, f_, px_, py_] := 
    (y - py) + ((2*a*xx + b*yy + d)/(b*xx + 2*c*yy + e))*(x - px)
 
tangentEqM[m_, px_, py_] := Simplify[(y - py) + 
      ((m[[1,1]]*px + m[[1,2]]*py + m[[1,3]])/(m[[1,2]]*px + m[[2,2]]*py + 
         m[[2,3]]))*(x - px)]
 
tangentEqMpt[m_, {px_, py_}] := 
    Simplify[(y - py) + ((m[[1,1]]*px + m[[1,2]]*py + m[[1,3]])/
        (m[[1,2]]*px + m[[2,2]]*py + m[[2,3]]))*(x - px)]
 
tangencyPoints[{{a_, h_, g_}, {h_, b_, f_}, {g_, f_, c_}}, {x0_, y0_}] := 
    DeleteDuplicates[
     Solve[{(g*(u - x0)^2 + f*(u - x0)*(v - y0) + (h*u + b*v - h*x0 - b*y0)*
            ((-v)*x0 + u*y0))^2 - (a*(u - x0)^2 + (2*h*(u - x0) + b*(v - y0))*
            (v - y0))*(c*(u - x0)^2 + (v*x0 - u*y0)*(-2*f*u + 2*f*x0 + 
             b*v*x0 - b*u*y0)) == 0, a*u^2 + 2*h*u*v + b*v^2 + 2*g*u + 
         2*f*v + c == 0}, {u, v}, Reals]]
 
tangencyPointsNum[{{a_, h_, g_}, {h_, b_, f_}, {g_, f_, c_}}, {x0_, y0_}] := 
    Module[{res}, res = DeleteDuplicates[
        NSolve[{(g*(u - x0)^2 + f*(u - x0)*(v - y0) + (h*u + b*v - h*x0 - 
                b*y0)*((-v)*x0 + u*y0))^2 - (a*(u - x0)^2 + (2*h*(u - x0) + 
                b*(v - y0))*(v - y0))*(c*(u - x0)^2 + (v*x0 - u*y0)*(-2*f*u + 
                2*f*x0 + b*v*x0 - b*u*y0)) == 0, a*u^2 + 2*h*u*v + b*v^2 + 
            2*g*u + 2*f*v + c == 0}, {u, v}, Reals]]; 
      If[Length[res] > 2, res = Select[res, Abs[(u /. #1) - x0] > 
           10^(-9) & ]]; res]
 
cPolar[{{axx_, axy_, bx_}, {axy_, ayy_, by_}, {bx_, by_, c_}}, {px_, py_}] := 
    Module[{d, e, f}, d = axx*px + axy*py + bx; e = axy*px + ayy*py + by; 
      f = bx*px + by*py + c; d*x + e*y + f]
 
cPole[m_, d_, e_, f_] := Module[{xx, yy, zz}, 
     {{xx}, {yy}, {zz}} = Inverse[m] . {{d}, {e}, {f}}; {xx/zz, yy/zz}]
 
cPolarTriangle[m_, ptA_, ptB_, ptC_] := Module[{p1, p2, p3, la, lb, lc}, 
     la = cPolar[m, ptA]; lb = cPolar[m, ptB]; lc = cPolar[m, ptC]; 
      p1 = First[{x, y} /. Solve[lb == 0 && lc == 0, {x, y}]]; 
      p2 = First[{x, y} /. Solve[la == 0 && lc == 0, {x, y}]]; 
      p3 = First[{x, y} /. Solve[la == 0 && lb == 0, {x, y}]]; {p1, p2, p3}]
 
cPerspector[m_, ptA_, ptB_, ptC_] := Module[{pa, pb, pc, l1, l2}, 
     {pa, pb, pc} = cPolarTriangle[m, ptA, ptB, ptC]; 
      lineIntersection[lineEqPt[ptA, pa], lineEqPt[ptB, pb]]]
 
cCollinearityMatrix[u_, v_, w_] := Det[{{u[[1]], u[[2]], 1}, 
      {v[[1]], v[[2]], 1}, {w[[1]], w[[2]], 1}}]
 
cConcurrencyMatrix[l1_, l2_, l3_] := 
    Det[{{Coefficient[l1, x], Coefficient[l1, y], l1 /. {x -> 0, y -> 0}}, 
      {Coefficient[l2, x], Coefficient[l2, y], l2 /. {x -> 0, y -> 0}}, 
      {Coefficient[l3, x], Coefficient[l3, y], l3 /. {x -> 0, y -> 0}}}]
 
findIntersections[l1_, l2_] := Module[{eqns, b, m}, 
     eqns = {l1 == 0, l2 == 0}; {b, m} = CoefficientArrays[eqns, {x, y}]; 
      LinearSolve[m, -b]]
