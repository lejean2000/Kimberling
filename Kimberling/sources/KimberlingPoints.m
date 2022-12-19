KimberlingCenter[k_, XPA_, XPB_, XPC_] := Module[{bary}, 
     bary = KimberlingCenterCN[k]; (bary/Total[bary]) . {XPA, XPB, XPC} /. 
       {a -> EuclideanDistance[XPB, XPC], b -> EuclideanDistance[XPA, XPC], 
        c -> EuclideanDistance[XPA, XPB]}]
 
KimberlingCenterCN[k_] := symmetrizeInternalAngle[
      ETC[StringJoin["X", ToString[k]]]] /. 
     Thread[{A -> angleA, B -> angleB, C -> angleC}]
 
KimberlingCenterC[k_] := symmetrizeInternalAngle[
     ETC[StringJoin["X", ToString[k]]]]
 
getTriangleCurve[name_] := TriangleCurves[name]
 
getvalue[ass_, key_] := If[KeyExistsQ[ass, key], ass[key], 
     {Indeterminate, Indeterminate, Indeterminate}]
 
ruleAbsSquare = {Abs[x___]^2 :> x^2}
 
rule69 = {a -> 6, b -> 9, c -> 13}
 
checkLines[lineslist_, pt_] := Module[{}, 
     Do[Print[lines]; checkcol = Simplify[bCollinearityMatrix[
          KimberlingCenterB[lines[[1]]], KimberlingCenterB[lines[[2]]], pt]]; 
       Print[TrueQ[checkcol == 0]]; If[ !TrueQ[checkcol == 0], 
        Print[TrueQ[N[checkcol /. rule69] == 0]]]; , {lines, lineslist}]]
 
checkCurves[pt_] := Do[ptest = pt /. rule69; ptest = ptest/Total[ptest]; 
      d = TriangleCurves[name] /. Thread[{x, y, z} -> ptest] /. rule69; 
      If[Abs[d] < 10^(-10), Print[name]; Print[N[d]]]; , 
     {name, Keys[TriangleCurves]}]
 
checkPointinETC[pt_] := MinimalBy[Value][
     (Total[Abs[#1 - Normalize[pt]]] & ) /@ ETCBaryNorm]
