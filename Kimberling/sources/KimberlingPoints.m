KimberlingCenter[k_, XPA_, XPB_, XPC_] := Module[{bary}, 
     bary = KimberlingCenterB[k]; (bary/Total[bary]) . {XPA, XPB, XPC} /. 
       {a -> EuclideanDistance[XPB, XPC], b -> EuclideanDistance[XPA, XPC], 
        c -> EuclideanDistance[XPA, XPB]}]
 
KimberlingCenterB[k_] := Module[{wA, wB, wC, w, name}, 
     Clear[a, b, c]; name = StringJoin["X", ToString[k]]; 
      If[k <= 1000, wA = getvalue[KimberlingTrilinears1000, name]]; 
      If[k > 1000 && k < 3000, wA = getvalue[KimberlingTrilinears3000, 
         name]]; If[k > 40001 && k <= 42000, 
       wA = getvalue[KimberlingTrilinears42000, name]]; 
      If[k > 42000 && k <= 44000, wA = getvalue[KimberlingTrilinears44000, 
         name]]; If[k > 44000 && k <= 46000, 
       wA = getvalue[KimberlingTrilinears46000, name]]; 
      If[wA[[1]] === Indeterminate, Return[{Indeterminate, Indeterminate, 
         Indeterminate}]]; wA = wA[[1]]; wB = wA /. {a -> b, b -> c, c -> a}; 
      wC = wB /. {a -> b, b -> c, c -> a}; {a*wA, b*wB, c*wC}]
 
getvalue[ass_, key_] := If[KeyExistsQ[ass, key], ass[key], 
     {Indeterminate, Indeterminate, Indeterminate}]
 
getTriangleCurve[name_] := symmetrizeEq[TriangleCurves[name]]
