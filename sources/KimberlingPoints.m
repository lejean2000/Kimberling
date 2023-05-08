defined[s_] := ToExpression[StringJoin["ValueQ[", s, "]"]] || 
     Head[ToExpression[s]] =!= Symbol || 
     ToExpression[StringJoin["Attributes[", s, "]"]] =!= {} || 
     ToExpression[StringJoin["DownValues[", s, "]"]] =!= {} || 
     ToExpression[StringJoin["SubValues[", s, "]"]] =!= {}
 
KimberlingCenter[k_, XPA_, XPB_, XPC_] := Module[{bary}, 
     bary = KimberlingCenterCN[k]; (bary/Total[bary]) . {XPA, XPB, XPC} /. 
       {a -> EuclideanDistance[XPB, XPC], b -> EuclideanDistance[XPA, XPC], 
        c -> EuclideanDistance[XPA, XPB]}]
 
KimberlingCenterCN[k_] := evaluate[
     symmetrizeInternal[ETC[StringJoin["X", ToString[k]]]] /. 
      Thread[{A -> angleA, B -> angleB, C -> angleC}]]
 
KimberlingCenterC[k_] := symmetrizeInternal[
      ETC[StringJoin["X", ToString[k]]]] /. 
     Thread[{A -> angleA, B -> angleB, C -> angleC}]
 
getTriangleCurve[name_] := evaluate[TriangleCurves[name]]
 
getvalue[ass_, key_] := If[KeyExistsQ[ass, key], ass[key], 
     {Indeterminate, Indeterminate, Indeterminate}]
 
ruleAbsSquare = {Abs[x___]^2 :> x^2}
 
rule69 = {a -> 6, b -> 9, c -> 13}
 
checkCurvesSymb[pt_] := Do[curve = getTriangleCurve[name]; 
      If[Simplify[curve /. Thread[{x, y, z} -> pt]] == 0, Print[name]]; , 
     {name, Keys[TriangleCurves]}]
 
checkPointinETC2[pt_] := Module[{ptnum, set, out}, 
     ptnum = intnumericnorm[evaluate[pt] /. rule69]; 
      set = Keys[Select[ETCBaryNorm, #1 == ptnum & ]]; out = {}; 
      If[Length[set] > 0, 
       Do[If[AllTrue[(coincideNorm[KimberlingCenterCNy[k] /. #1, pt /. 
                #1] & ) /@ intCheckList, #1 & ], AppendTo[out, k]; ]; , 
         {k, set}]; ]; Return[out]; ]
 
checkPointinETC69[pt_] := Module[{ptnum, set}, 
     ptnum = intnumericnorm[evaluate[pt] /. rule69]; 
      Keys[Select[ETCBaryNorm, #1 == ptnum & ]]]
 
rulesSimplify = a > 0 && b > 0 && c > 0 && a + b > c && a + c > b && b + c > a
 
checkPointsOnCurve[crv_] := Module[{curve, curve2, out}, 
     curve = evaluate[crv] /. Thread[{u, v, w} -> {x, y, z}]; 
      curve2 = curve /. rule69; 
      test = Select[(Abs[curve2 /. Thread[{x, y, z} -> #1]] & ) /@ 
         ETCBaryNorm, #1 < 10^(-12) & ]; 
      out = Select[Table[{ni, N[curve /. Thread[{x, y, z} -> 
               simplifyRationalBarycentrics[KimberlingCenterCNy[ni]]] /. 
            intCheckList[[1]], 20]}, {ni, Keys[test]}], #1[[2]] == 0 & ]; 
      (StringJoin[StringTake[#1[[1]], 1], "(", StringTake[#1[[1]], {2, -1}], 
         ")"] & ) /@ out]
