KimberlingCenter[k_, XPA_, XPB_, XPC_] := Module[{bary}, 
     bary = KimberlingCenterB[k]; (bary/Total[bary]) . {XPA, XPB, XPC} /. 
       {a -> EuclideanDistance[XPB, XPC], b -> EuclideanDistance[XPA, XPC], 
        c -> EuclideanDistance[XPA, XPB]}]
 
KimberlingCenterB[k_] := Module[{wA, wB, wC, w, AlgoKimberlingWeights}, 
     Clear[a, b, c]; AlgoKimberlingWeights = Association[]; 
      If[k <= 1000, AlgoKimberlingWeights = KimberlingTrilinears1000]; 
      If[ !KeyExistsQ[AlgoKimberlingWeights, StringJoin["X", ToString[k]]], 
       Return[{Indeterminate, Indeterminate, Indeterminate}]]; 
      wA = KimberlingTrilinears1000[StringJoin["X", ToString[k]]][[1]]; 
      wB = wA /. {a -> b, b -> c, c -> a}; 
      wC = wB /. {a -> b, b -> c, c -> a}; {a*wA, b*wB, c*wC}]
KimberlingCenter[k_, XPA_, XPB_, XPC_] := Module[{bary}, 
     bary = KimberlingCenterB[k]; (bary/Total[bary]) . {XPA, XPB, XPC} /. 
       {a -> EuclideanDistance[XPB, XPC], b -> EuclideanDistance[XPA, XPC], 
        c -> EuclideanDistance[XPA, XPB]}]
 
KimberlingCenterB[k_] := Module[{wA, wB, wC, w, AlgoKimberlingWeights}, 
     Clear[a, b, c]; AlgoKimberlingWeights = Association[]; 
      If[k <= 1000, AlgoKimberlingWeights = KimberlingTrilinears1000]; 
      If[ !KeyExistsQ[AlgoKimberlingWeights, StringJoin["X", ToString[k]]], 
       Return[{Indeterminate, Indeterminate, Indeterminate}]]; 
      wA = KimberlingTrilinears1000[StringJoin["X", ToString[k]]][[1]]; 
      wB = wA /. {a -> b, b -> c, c -> a}; 
      wC = wB /. {a -> b, b -> c, c -> a}; {a*wA, b*wB, c*wC}]
KimberlingCenter[k_, XPA_, XPB_, XPC_] := Module[{bary}, 
     bary = KimberlingCenterB[k]; (bary/Total[bary]) . {XPA, XPB, XPC} /. 
       {a -> EuclideanDistance[XPB, XPC], b -> EuclideanDistance[XPA, XPC], 
        c -> EuclideanDistance[XPA, XPB]}]
 
KimberlingCenterB[k_] := Module[{wA, wB, wC, w, AlgoKimberlingWeights}, 
     Clear[a, b, c]; AlgoKimberlingWeights = Association[]; 
      If[k <= 1000, AlgoKimberlingWeights = KimberlingTrilinears1000]; 
      If[ !KeyExistsQ[AlgoKimberlingWeights, StringJoin["X", ToString[k]]], 
       Return[{Indeterminate, Indeterminate, Indeterminate}]]; 
      wA = KimberlingTrilinears1000[StringJoin["X", ToString[k]]][[1]]; 
      wB = wA /. {a -> b, b -> c, c -> a}; 
      wC = wB /. {a -> b, b -> c, c -> a}; {a*wA, b*wB, c*wC}]
