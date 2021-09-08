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
 
matrixToEq[m_] := Module[{cv, eq}, cv := {{x}, {y}, {1}}; 
      eq = First[First[Transpose[cv] . m . cv]]; 
      Simplify /@ Total[MonomialList[Expand[eq, {x, y}]]]]
 
lineEq[ax_, ay_, bx_, by_] := Det[{{x, y, 1}, {ax, ay, 1}, {bx, by, 1}}]
 
tangentEq[a_, b_, c_, d_, e_, f_, px_, py_] := 
    (y - py) + ((2*a*xx + b*yy + d)/(b*xx + 2*c*yy + e))*(x - px)
 
tangentEqM[m_, px_, py_] := Simplify[(y - py) + 
      ((m[[1,1]]*px + m[[1,2]]*py + m[[1,3]])/(m[[1,2]]*px + m[[2,2]]*py + 
         m[[2,3]]))*(x - px)]
