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
 
X[k_] := evaluate[symmetrizeInternal[ETCFull[StringJoin["X", 
         ToString[k]]]] /. Thread[{A -> angleA, B -> angleB, C -> angleC}]]
 
KimberlingCenterC[k_] := Module[{ptname}, 
     If[NumericQ[k], ptname = StringJoin["X", ToString[k]], ptname = k]; 
      Return[symmetrizeInternal[ETCFull[ptname]] /. 
        Thread[{A -> angleA, B -> angleB, C -> angleC}]]; ]
 
getTriangleCurve[name_, in_:TriangleCurves] := 
    Module[{tmp}, tmp = Select[Keys[in], StringContainsQ[ToLowerCase[#1], 
          ToLowerCase[name]] & ]; Print[tmp]; 
      Return[evaluate[in[tmp[[1]]]]]; ]
 
getvalue[ass_, key_] := If[KeyExistsQ[ass, key], ass[key], 
     {Indeterminate, Indeterminate, Indeterminate}]
 
ruleAbsSquare = {Abs[x___]^2 :> x^2}
 
rule69 = {a -> 6, b -> 9, c -> 13}
 
checkCurvesSymb[pt_] := Do[curve = getTriangleCurve[name]; 
      If[Simplify[curve /. Thread[{x, y, z} -> pt]] == 0, Print[name]]; , 
     {name, Keys[TriangleCurves]}]
 
checkPointinETC2[pt_] := Module[{ptnum, set, out, cmplx}, 
     ptnum = intnumericnorm[evaluate[pt] /. rule69]; 
      If[AllTrue[ptnum, Im[#1] == 0 & ], 
       set = Keys[Select[ETCBaryNorm, #1 == ptnum & ]], 
       cmplx = Keys[Select[ETCBaryNorm, AnyTrue[#1, Im[#1] != 0 & ] & ]]; 
        set = Select[cmplx, coincide[ETCBaryNorm[#1], ptnum] & ]; ]; 
      out = {}; If[Length[set] > 0, 
       Do[If[AllTrue[(coincideNorm[KimberlingCenterCNy[k] /. #1, pt /. 
                #1] & ) /@ intCheckList, #1 ||  !BooleanQ[#1] & ], 
           AppendTo[out, k]; ]; , {k, set}]; ]; Return[out]; ]
 
rulesSimplify = a > 0 && b > 0 && c > 0 && a + b > c && a + c > b && 
     b + c > a && S > 0
 
checkPointsOnCurve[crv_] := Module[{curve, curve2, out, normcoef}, 
     curve = evaluate[crv] /. Thread[{u, v, w} -> {x, y, z}]; 
      curve2 = curve /. rule69; normcoef = 
       Max[Flatten[Abs[CoefficientList[curve2, {x, y, z}]]]]; 
      test = Select[(Abs[curve2/normcoef /. Thread[{x, y, z} -> #1]] & ) /@ 
         ETCBaryNorm, #1 < 10^(-12) & ]; 
      out = Select[Table[{ni, N[Simplify[curve /. Thread[{x, y, z} -> 
                KimberlingCenterCNy[ni]] /. intCheckList[[1]]], 20]}, 
         {ni, Keys[test]}], #1[[2]] == 0 & ]; 
      out = SortBy[out, numsortexpr[#1[[1]]] & ]; 
      (StringJoin[StringTake[#1[[1]], 1], "(", StringTake[#1[[1]], {2, -1}], 
         ")"] & ) /@ out]
 
checkPointOnCurveNum[crv_, pt_, rules_:intCheckList] := 
    Module[{curve2, normcoef, test}, 
     Do[curve2 = crv /. r; normcoef = 
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
 
checkTrianglesOnCurve[crv_] := Module[{ecrv, out}, 
     ecrv = evaluate[crv] /. rule69; out = {}; 
      Do[If[(ecrv /. Thread[{x, y, z} -> (KimberlingTrianglesBary[name] /. 
             rule69)]) == 0, AppendTo[out, name]; ], 
       {name, Keys[KimberlingTrianglesBary]}]; Return[out]; ]
 
checkCurvesForTriangle[tr_] := Module[{ecrv, out}, 
     out = {}; Do[ecrv = evaluate[TriangleCurves[name]] /. rule69; 
        If[(ecrv /. Thread[{x, y, z} -> (evaluate[tr[[1]]] /. rule69)]) == 0, 
         AppendTo[out, name]; ], {name, Keys[TriangleCurves]}]; Return[out]; ]
 
sinReplace = {Sin[A] -> S/(b*c), Sin[B] -> S/(a*c), Sin[C] -> S/(a*b)}
 
checkTriangleExists[tr_] := Module[{chk}, 
     Do[chk = Cross[intnumericnorm[evaluate[tr[[1]]] /. rule69], 
          intnumericnorm[evaluate[KimberlingTrianglesBary[trkim]] /. 
            rule69]]; If[Max[Abs[chk]] < 10^(-24), 
         Return[{True, trkim}, Module]; ], 
       {trkim, Keys[KimberlingTrianglesBary]}]; Return[{False}]; ]
 
trgCheckPerspectivity[trchk_, trgname_, set_:KimberlingTrianglesBary] := 
    Module[{out, trsym, ntest, ptcoord, perschk, outname}, 
     out = {}; Do[trsym = If[ListQ[set[trname][[1]]], set[trname], 
          triangle[trname]]; ntest = bIsPerspective @@ 
           (evaluate[Join[trsym, trchk]] /. rule69) /. rule69; 
        If[Abs[ntest] < 10^(-24), ptcoord = bTrianglePerspector[trchk, 
            trsym]; perschk = checkPointinETC2[ptcoord]; 
          outname = StringJoin["Perspector of ", trgname, " and ", trname]; 
          If[Abs[(bDistance[trchk[[1]], trchk[[2]]]/bDistance[trsym[[1]], 
                 trsym[[2]]] /. rule69) - (bDistance[trchk[[1]], trchk[[3]]]/
                bDistance[trsym[[1]], trsym[[3]]] /. rule69)] < 10^(-20), 
           Print[Style[StringJoin["Possibly homothetic: ", trgname, " and ", 
               trname], Red]]; ]; If[Length[perschk] > 0, 
           Print[StringJoin[outname, ": ", perschk[[1]]]], 
           Print[Style[outname, Blue]]; ]; ]; , 
       {trname, Keys[set] /. "infinite-altitude" -> Nothing}]; ]
 
trgCheckParallelogic[trchk_, trgname_, set_:KimberlingTrianglesBary] := 
    Module[{out, trsym, ntest, ptcoord, perschk, outname}, 
     out = {}; Do[trsym = If[ListQ[set[trname][[1]]], set[trname], 
          triangle[trname]]; ntest = bIsParallelogic @@ 
           (evaluate[Join[trsym, trchk]] /. rule69) /. rule69; 
        If[Abs[ntest] < 10^(-24), ptcoord = bParallelogicCenter[trchk, 
            trsym]; perschk = checkPointinETC2[ptcoord]; 
          outname = StringJoin["Parallelogic center of ", trgname, " and ", 
            trname]; If[Length[perschk] > 0, Print[StringJoin[outname, ": ", 
             perschk[[1]]]], Print[Style[outname, Blue]]; ]; 
          ptcoord = bParallelogicCenter[trsym, trchk]; 
          perschk = checkPointinETC2[ptcoord]; outname = 
           StringJoin["Parallelogic center of ", trname, " and ", trgname]; 
          If[Length[perschk] > 0, Print[StringJoin[outname, ": ", 
             perschk[[1]]]], Print[Style[outname, Blue]]; ]; ]; , 
       {trname, Keys[set] /. "infinite-altitude" -> Nothing}]; ]
 
trgCheckOrthologic[trchk_, trgname_, set_:KimberlingTrianglesBary] := 
    Module[{out, trsym, ntest, ptcoord, perschk, outname}, 
     out = {}; Do[trsym = If[ListQ[set[trname][[1]]], set[trname], 
          triangle[trname]]; ntest = bIsOrthologic @@ 
           (evaluate[Join[trsym, trchk]] /. rule69) /. rule69; 
        If[Abs[ntest] < 10^(-24), ptcoord = bOrthologyCenter[trchk, trsym]; 
          perschk = checkPointinETC2[ptcoord]; outname = 
           StringJoin["Orthology center of ", trgname, " and ", trname]; 
          If[Length[perschk] > 0, Print[StringJoin[outname, ": ", 
             perschk[[1]]]], Print[Style[outname, Blue]]; ]; 
          ptcoord = bOrthologyCenter[trsym, trchk]; 
          perschk = checkPointinETC2[ptcoord]; outname = 
           StringJoin["Orthology center of ", trname, " and ", trgname]; 
          If[Length[perschk] > 0, Print[StringJoin[outname, ": ", 
             perschk[[1]]]], Print[Style[outname, Blue]]; ]; ]; , 
       {trname, Keys[set] /. "infinite-altitude" -> Nothing}]; ]
