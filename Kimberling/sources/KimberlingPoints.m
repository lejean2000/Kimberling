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
 
checkCurves[pt_] := Module[{out}, out = {}; 
      Do[ptest = NormalizeBary[(evaluate /. rule69)[pt]]; 
        d = getTriangleCurve[name] /. Thread[{x, y, z} -> ptest] /. rule69; 
        If[Abs[d] < 10^(-12), AppendTo[out, name]]; , 
       {name, Keys[TriangleCurves]}]; If[Length[out] > 0, 
       Print[StringJoin["Lies on curves: ", StringRiffle[out, ", "]]]; ]; ]
 
checkCurvesSymb[pt_] := Do[curve = getTriangleCurve[name]; 
      If[Simplify[curve /. Thread[{x, y, z} -> pt]] == 0, Print[name]]; , 
     {name, Keys[TriangleCurves]}]
 
checkPointinETC[pt_] := MinimalBy[Value][
     (Total[Abs[#1 - NormalizeBary[pt]]] & ) /@ ETCBaryNorm]
 
rulesSimplify = a > 0 && b > 0 && c > 0 && a + b > c && a + c > b && b + c > a
 
checkPointsOnCurve[crv_] := Module[{curve}, 
     curve = evaluate[crv] /. Thread[{u, v, w} -> {x, y, z}] /. rule69; 
      dset = (Abs[curve] /. Thread[{x, y, z} -> #1] & ) /@ ETCBaryNorm; 
      test = Select[dset, #1 < 10^(-15) & ]; 
      (StringJoin["X(", StringTake[#1, {2, -1}], ")"] & ) /@ Keys[test]]
