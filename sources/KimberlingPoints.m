defined[s_] := ToExpression[StringJoin["ValueQ[", s, "]"]] || 
     Head[ToExpression[s]] =!= Symbol || 
     ToExpression[StringJoin["Attributes[", s, "]"]] =!= {} || 
     ToExpression[StringJoin["DownValues[", s, "]"]] =!= {} || 
     ToExpression[StringJoin["SubValues[", s, "]"]] =!= {}
 
KimberlingCenter[k_, XPA_, XPB_, XPC_] := Module[{bary}, 
     bary = KimberlingCenterCN[k]; (bary/Total[bary]) . {XPA, XPB, XPC} /. 
       {a -> EuclideanDistance[XPB, XPC], b -> EuclideanDistance[XPA, XPC], 
        c -> EuclideanDistance[XPA, XPB]}]
 
KimberlingCenterCN = X
 
X[k_] := evaluate[symmetrizeInternal[ETC[StringJoin["X", ToString[k]]]] /. 
      Thread[{A -> angleA, B -> angleB, C -> angleC}]]
 
KimberlingCenterC[k_] := Module[{ptname}, 
     If[NumericQ[k], ptname = StringJoin["X", ToString[k]], ptname = k]; 
      Return[symmetrizeInternal[ETC[ptname]] /. 
        Thread[{A -> angleA, B -> angleB, C -> angleC}]]; ]
 
getTriangleCurve[name_, in_:TriangleCurves] := 
    Module[{tmp}, tmp = Select[Keys[in], StringStartsQ[#1, name] & ]; 
      Print[tmp]; Return[evaluate[in[tmp[[1]]]]]; ]
 
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
                #1] & ) /@ intCheckList, #1 ||  !BooleanQ[#1] & ], 
           AppendTo[out, k]; ]; , {k, set}]; ]; Return[out]; ]
 
checkPointinETC69[pt_] := Module[{ptnum, set}, 
     ptnum = intnumericnorm[evaluate[pt] /. rule69]; 
      Keys[Select[ETCBaryNorm, #1 == ptnum & ]]]
 
rulesSimplify = a > 0 && b > 0 && c > 0 && a + b > c && a + c > b && b + c > a
 
checkPointsOnCurve[crv_] := Module[{curve, curve2, out, normcoef}, 
     curve = evaluate[crv] /. Thread[{u, v, w} -> {x, y, z}]; 
      curve2 = curve /. rule69; normcoef = 
       Max[Flatten[Abs[CoefficientList[curve2, {x, y, z}]]]]; 
      test = Select[(Abs[curve2/normcoef /. Thread[{x, y, z} -> #1]] & ) /@ 
         ETCBaryNorm, #1 < 10^(-12) & ]; 
      out = Select[Table[{ni, N[curve /. Thread[{x, y, z} -> 
               KimberlingCenterCNy[ni]] /. intCheckList[[1]], 20]}, 
         {ni, Keys[test]}], #1[[2]] == 0 & ]; 
      out = SortBy[out, numsortexpr[#1[[1]]] & ]; 
      (StringJoin[StringTake[#1[[1]], 1], "(", StringTake[#1[[1]], {2, -1}], 
         ")"] & ) /@ out]
 
checkPointOnCurveNum[crv_, pt_, rules_:intCheckList] := 
    Module[{curve2, normcoef, test}, 
     Do[curve2 = curveSimplify[crv /. r]; normcoef = 
         Max[Flatten[Abs[CoefficientList[curve2, {x, y, z}]]]]; 
        test = Abs[curve2/normcoef /. Thread[{x, y, z} -> N[pt /. r, 35]]]; 
        If[test > 1/10^15, Return[False, Module]]; , {r, rules}]; 
      Return[True]; ]
 
checkPointsOnCurveNamed[crvname_] := Module[{srch}, 
     srch = Select[Keys[TriangleCurves], StringContainsQ[#1, crvname] & ]; 
      If[Length[srch] > 1, Print["Which curve?"]; Print[srch]]; 
      If[Length[srch] == 0, Print["No such curve"]]; Print[srch]; 
      Return[checkPointsOnCurve[TriangleCurves[First[srch]]]]; ]
 
XNy[k_] := Module[{k2}, If[NumberQ[k], k2 = StringJoin["X", ToString[k]], 
       k2 = k]; KimberlingCenterCNy[k2]/Total[KimberlingCenterCNy[k2]]]
