KimberlingCenter[k_, XPA_, XPB_, XPC_] := Module[{bary}, 
     bary = KimberlingCenterB[k]; (bary/Total[bary]) . {XPA, XPB, XPC} /. 
       {a -> EuclideanDistance[XPB, XPC], b -> EuclideanDistance[XPA, XPC], 
        c -> EuclideanDistance[XPA, XPB]}]
 
KimberlingCenterB[k_] := Module[{wA, wB, wC, w, AlgoKimberlingWeights}, 
     Clear[a, b, c]; AlgoKimberlingWeights = KimberlingTrilinears1000; 
      If[k <= 1000, AlgoKimberlingWeights = KimberlingTrilinears1000]; 
      If[k > 1000 && k < 3000, AlgoKimberlingWeights = 
        KimberlingTrilinears3000]; If[k > 42000 && k <= 44000, 
       AlgoKimberlingWeights = KimberlingTrilinears44000]; 
      If[k > 40001 && k <= 42000, AlgoKimberlingWeights = 
        KimberlingTrilinears42000]; If[k > 42000 && k <= 46000, 
       AlgoKimberlingWeights = KimberlingTrilinears46000]; 
      If[ !KeyExistsQ[AlgoKimberlingWeights, StringJoin["X", ToString[k]]], 
       Return[{Indeterminate, Indeterminate, Indeterminate}]]; 
      wA = AlgoKimberlingWeights[StringJoin["X", ToString[k]]][[1]]; 
      wB = wA /. {a -> b, b -> c, c -> a}; 
      wC = wB /. {a -> b, b -> c, c -> a}; {a*wA, b*wB, c*wC}]
 
getTriangleCurve[name_] := symmetrizeEq[TriangleCurves[name]]
