defined[s_] := ToExpression[StringJoin["ValueQ[", s, "]"]] || 
     Head[ToExpression[s]] =!= Symbol || 
     ToExpression[StringJoin["Attributes[", s, "]"]] =!= {} || 
     ToExpression[StringJoin["DownValues[", s, "]"]] =!= {} || 
     ToExpression[StringJoin["SubValues[", s, "]"]] =!= {}
 
KimberlingCenter[k_, XPA_, XPB_, XPC_] := Module[{bary, rpl}, 
     rpl = {a -> EuclideanDistance[XPB, XPC], b -> EuclideanDistance[XPA, 
          XPC], c -> EuclideanDistance[XPA, XPB]}; 
      bary = KimberlingCenterCN[k] /. rpl; (bary/Total[bary]) . 
       {XPA, XPB, XPC}]
 
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
 
checkPointsOnCurve[crv_] := Block[{curve, curve2, out, normcoef}, 
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
 
checkPointsOnCurveNamed[crvname_] := Block[{srch}, 
     srch = Select[Keys[TriangleCurves], StringContainsQ[#1, crvname] & ]; 
      If[Length[srch] > 1, Print["Which curve?"]; Print[srch]; 
        Return[False, Block]]; If[Length[srch] == 0, Print["No such curve"]; 
        Return[False, Block]]; Print[srch]; 
      Return[checkPointsOnCurve[TriangleCurves[First[srch]]]]; ]
 
XNy[k_] := Module[{k2}, If[NumberQ[k], k2 = StringJoin["X", ToString[k]], 
       k2 = k]; KimberlingCenterCNy[k2]/Total[KimberlingCenterCNy[k2]]]
 
checkTrianglesOnCurve[crv_, trsets_:{"CPTR", "ENCTR", "UNI"}] := 
    Block[{ecrv, out, trgsets}, trgsets = {KimberlingTrianglesBary}; 
      If[MemberQ[trsets, "CPTR"], AppendTo[trgsets, AssociationMap[
         CPTR[#1][[1]] & , Keys[CPTR]]]]; If[MemberQ[trsets, "ENCTR"], 
       AppendTo[trgsets, AssociationMap[ENCTR[#1][[1]] & , Keys[ENCTR]]]]; 
      If[MemberQ[trsets, "UNI"], AppendTo[trgsets, AssociationMap[
         UNCFTRG[#1][[1]] & , Keys[UNCFTRG]]]]; 
      ecrv = evaluate[crv] /. rule69; out = {}; 
      Do[Do[If[(ecrv /. Thread[{x, y, z} -> (set[name] /. rule69)]) == 0, 
          AppendTo[out, name]; ], {name, Keys[set]}]; , {set, trgsets}]; 
      Return[out]; ]
 
checkCurvesForTriangle[tr_] := Block[{ecrv, out}, 
     out = intCurvesForTriangle[tr, rule69]; If[Length[out] > 0, 
       out = Intersection[out, intCurvesForTriangle[tr, intCheckList[[1]]], 
          intCurvesForTriangle[tr, intCheckList[[2]]]]; ]; Return[out]; ]
 
intCurvesForTriangle[tr_, checktr_] := Block[{ecrv, out}, 
     out = {}; Do[ecrv = evaluate[TriangleCurves[name]] /. checktr; 
        If[(ecrv /. Thread[{x, y, z} -> (evaluate[tr[[1]]] /. checktr)]) == 
          0, AppendTo[out, name]; ], {name, Keys[TriangleCurves]}]; 
      Return[out]; ]
 
sinReplace = {Sin[A] -> S/(b*c), Sin[B] -> S/(a*c), Sin[C] -> S/(a*b), 
     Cos[A] -> evaluate[Cos[A]], Cos[B] -> evaluate[Cos[B]], 
     Cos[C] -> evaluate[Cos[C]]}
 
checkTriangleExists[tr_] := Block[{chk}, 
     Do[chk = Cross[intnumericnorm[evaluate[tr[[1]]] /. rule69], 
          intnumericnorm[evaluate[KimberlingTrianglesBary[trkim]] /. 
            rule69]]; If[Max[Abs[chk]] < 10^(-24), 
         Return[{True, trkim}, Block]; ], 
       {trkim, Keys[KimberlingTrianglesBary]}]; 
      Do[chk = Cross[intnumericnorm[evaluate[tr[[1]]] /. rule69], 
          intnumericnorm[evaluate[CPTR[trkim][[1]]] /. rule69]]; 
        If[Max[Abs[chk]] < 10^(-24), Return[{True, trkim}, Block]; ], 
       {trkim, Keys[CPTR]}]; 
      Do[chk = Cross[intnumericnorm[evaluate[tr[[1]]] /. rule69], 
          intnumericnorm[evaluate[ENCTR[trkim][[1]]] /. rule69]]; 
        If[Max[Abs[chk]] < 10^(-24), Return[{True, trkim}, Block]; ], 
       {trkim, Keys[ENCTR]}]; Return[{False}]; ]
 
trgCheckPerspectivity[trchk_, trgname_:"TR", set_:KimberlingTrianglesBary] := 
    Block[{cnt, trchke, trsyme, hmt, out, trsym, ntest, ptcoord, perschk, 
      homval, outname, ishomo, homval2, simtest, homtest}, 
     cnt = 0; If[Length[trgname] != 0, Print["Invalid TRG Name"]; 
        Return[False, Block]]; out = Association[]; 
      trchke = evaluate[trchk] /. rule69; ishomo = False; 
      Do[trsym = If[ListQ[set[trname][[1]]], set[trname], triangle[trname]]; 
        trsyme = evaluate[trsym] /. rule69; ntest = bIsPerspective @@ 
          Join[trsyme, trchke]; simtest = bIsSimilarNumeric[trchke, trsyme]; 
        homtest = Abs[bIsHomothetic[trchke, trsyme] /. rule69] < 10^(-20); 
        If[Abs[ntest] < 10^(-20), cnt = cnt + 1; ptcoord = 
           bTrianglePerspector[evaluate[trchk], evaluate[trsym]]; 
          perschk = checkPointinETC2[ptcoord]; outname = 
           StringJoin["Perspector of ", trgname, " and ", trname]; hmt = ""; 
          If[simtest, If[homtest, If[trname == "ABC", ishomo = True]; 
              hmt = Style[" - possibly homothetic", Red], 
             hmt = Style[" - possibly similar", Green]; ]; 
            If[Length[perschk] > 0 && (trname == "ABC" ||  !ishomo), 
             Print[Row[{StringJoin[outname, ": ", perschk[[1]]], 
                 hmt}]]; ]; ]; If[Length[perschk] > 0, 
           AssociateTo[out, outname -> perschk[[1]]]; , 
           Print[Row[{outname, hmt}]]; ]; , 
         If[simtest && MemberQ[KimberlingTrianglesBaryOrthKeys, trname], 
           Print[Row[{StringJoin[trgname, " and ", trname], Style[
                " - possibly similar", Green]}]]; ]; ]; , 
       {trname, Keys[set] /. "infinite-altitude" -> Nothing}]; 
      KeyValueMap[Print[intaddbrackets[#1] -> #2] & , GroupBy[out, Identity, 
        Keys]]; Return[cnt]; ]
 
trgCheckParallelogic[trchk_, trgname_:"TR", set_:KimberlingTrianglesBary] := 
    Block[{keysset, out, trsym, ntest, ptcoord, perschk, outname}, 
     If[Length[trgname] != 0, Print["Invalid TRG Name"]; 
        Return[False, Block]]; 
      If[TrueQ[0 == Simplify[bCollinearityMatrix @@ evaluate /@ trchk /. 
           rule69]], Print["COLLINEAR POINTS"]; Return[False, Block]]; 
      If[Keys[set][[1]] == "ABC", keysset = KimberlingTrianglesBaryOrthKeys, 
       keysset = Keys[set]; ]; out = Association[]; 
      Do[trsym = If[ListQ[set[trname][[1]]], set[trname], triangle[trname]]; 
        ntest = bIsParallelogic @@ (evaluate[Join[trsym, trchk]] /. 
            rule69) /. rule69; If[Abs[ntest] < 10^(-24), 
         ptcoord = bParallelogicCenter[trchk, trsym]; 
          perschk = checkPointinETC2[ptcoord]; outname = 
           StringJoin["Parallelogic center of ", trgname, " and ", trname]; 
          If[Length[perschk] > 0, AssociateTo[out, outname -> 
              perschk[[1]]]; , Print[outname]; ]; ptcoord = 
           bParallelogicCenter[evaluate[trsym], evaluate[trchk]]; 
          perschk = checkPointinETC2[ptcoord]; outname = 
           StringJoin["Parallelogic center of ", trname, " and ", trgname]; 
          If[Length[perschk] > 0, AssociateTo[out, outname -> 
              perschk[[1]]]; , Print[outname]; ]; ]; , 
       {trname, keysset /. "infinite-altitude" -> Nothing}]; 
      KeyValueMap[Print[intaddbrackets[#1] -> #2] & , GroupBy[out, Identity, 
        Keys]]; ]
 
trgCheckOrthologic[trchk_, trgname_:"TR", set_:KimberlingTrianglesBary, 
     exclset_:{}] := Block[{out, out2, out3, trexcl, htest, trsym, trsyme, 
      trchke, ntest, ptcoord, perschk, outname, collinear, keysset}, 
     If[Length[trgname] != 0, Print["Invalid TRG Name"]; 
        Return[False, Block]]; out = Association[]; collinear = False; 
      trchke = evaluate[trchk] /. rule69; 
      If[TrueQ[0 == bCollinearityMatrix @@ trchke], collinear = True; 
        Print["COLLINEAR POINTS"]]; out2 = {}; out3 = {}; 
      If[Keys[set][[1]] == "ABC", keysset = KimberlingTrianglesBaryOrthKeys, 
       keysset = Keys[set]; ]; Monitor[
       Do[trsym = If[ListQ[set[trname][[1]]], set[trname], triangle[trname]]; 
         trsyme = evaluate[trsym] /. rule69; If[trname == "8th Brocard" || 
           trname == "Ehrmann-cross" || trname == "pedal of X(30)", 
          Continue[]]; If[Length[exclset] > 0, 
          Do[If[KeyExistsQ[set, excl], trexcl = If[ListQ[set[excl][[1]]], 
                set[excl], triangle[excl]]; htest = Abs[bIsHomothetic[trsyme, 
                 evaluate[trexcl] /. rule69]]; If[htest < 10^(-24), Goto[
                end]]; ]; , {excl, exclset}]]; If[collinear, 
          If[TrueQ[0 == Simplify[bCollinearityMatrix @@ trsyme]], 
            Continue[]]; ntest = bIsOrthologic[trchke, trsyme] /. rule69; , 
          ntest = bIsOrthologic[trsyme, trchke] /. rule69; ]; 
         If[Abs[ntest] < 10^(-24), ptcoord = bOrthologyCenter[
             evaluate[trchk], evaluate[trsym]]; perschk = checkPointinETC2[
             ptcoord]; outname = StringJoin["Orthology center of ", trgname, 
             " and ", trname]; PrintTemporary[outname]; 
           If[Length[perschk] > 0, AssociateTo[out, outname -> perschk[[
                1]]]; , AppendTo[out2, trname]; ]; If[ !collinear, 
            ptcoord = bOrthologyCenter[trsym, trchk]; perschk = 
              checkPointinETC2[ptcoord]; outname = StringJoin[
               "Orthology center of ", trname, " and ", trgname]; 
             If[Length[perschk] > 0, AssociateTo[out, outname -> 
                 perschk[[1]]]; , AppendTo[out3, trname]; ]; ]; ]; 
         Label[end]; , {trname, keysset /. "infinite-altitude" -> Nothing}], 
       trname]; If[Length[out2] > 0, Print[StringJoin["Orthology center of ", 
         trgname, " and ", ToString[out2]]]]; If[Length[out3] > 0, 
       Print[StringJoin["Orthology center of ", ToString[out3], " and ", 
         trgname]]]; KeyValueMap[Print[intaddbrackets[#1] -> #2] & , 
       GroupBy[out, Identity, Keys]]; ]
 
checkNumberedPoint[ptc_, trgname_, idx_] := Module[{trgn, newkey}, 
     If[newkey == "ABC-X3-reflections", newkey = "ABC-X3 reflections"]; 
      If[newkey == "anti-X3-ABC-reflections", 
       newkey = "anti-X3-ABC reflections"]; 
      If[newkey == "X3-ABC-reflections", newkey = "X3-ABC reflections"]; 
      If[newkey == "2nd-circumperp-tangential", 
       newkey = "2nd circumperp tangential"]; 
      newkey = StringReplace[trgname, {"1st-" -> "1st ", "2nd-" -> "2nd ", 
         "3rd-" -> "3rd ", "4th-" -> "4th ", "Gemini-" -> "Gemini "}]; 
      If[ !KeyExistsQ[KimberlingTrianglesBary, newkey], 
       Return[False, Module]]; trgn = intnumericnorm /@ 
        (evaluate[triangle[newkey]] /. rule69); 
      If[Total[Abs[bToCartesianN[bCoordChangeK[idx, trgn] /. rule69] - 
           bToCartesianN[ptc /. rule69]]] > 10^(-20), Return[False, Module]]; 
      trgn = intnumericnorm /@ (evaluate[triangle[newkey]] /. 
         intCheckList[[1]]); 
      If[Total[Abs[bToCartesianN[bCoordChangeK[idx, trgn] /. 
             intCheckList[[1]]] - bToCartesianN[ptc /. intCheckList[[1]]]]] > 
        10^(-20), Return[False, Module]]; 
      trgn = intnumericnorm /@ (evaluate[triangle[newkey]] /. 
         intCheckList[[2]]); 
      If[Total[Abs[bToCartesianN[bCoordChangeK[idx, trgn] /. 
             intCheckList[[2]]] - bToCartesianN[ptc /. intCheckList[[2]]]]] > 
        10^(-20), Return[False, Module]]; Return[True]; ]
 
trgCheckBasepoints[trchk_, trgname_, set_:KimberlingTrianglesBary] := 
    Block[{bps, trchke, trsyme, hmt, out, trsym, ntest, ptcoord, perschk, 
      homval, check}, Monitor[If[Length[trgname] != 0, 
        Print["Invalid TRG Name"]; Return[False, Block]]; 
       out = Association[]; trchke = evaluate[trchk] /. rule69; 
       Do[trsym = If[ListQ[set[trname][[1]]], set[trname], triangle[trname]]; 
         trsyme = evaluate[trsym] /. rule69; ntest = bIsPerspective @@ 
           Join[trsyme, trchke]; If[Abs[ntest] < 10^(-24), 
          homval = Abs[bIsHomothetic[trchke, trsyme]]; If[homval < 10^(-20), 
            Continue[]]; bps = TimeConstrained[basepoints[smpl[trsym], trchk, 
              smpl[bTrianglePerspector[trsym, trchk]]], 5, -1]; 
           If[bps == -1, Continue[]]; If[Length[bps[[1]]] > 0, 
            check = checkPointinETC2[bps[[1]]]; If[Length[check] > 0, 
              AssociateTo[out, {StringJoin["1st basepoint of ", trname, 
                  " and ", trgname]} -> check[[1]]], AssociateTo[out, 
               {StringJoin["1st basepoint of ", trname, " and ", trgname]} -> 
                "NP"]], Print[StringJoin["No basepoint for ", trname]]; 
             Continue[]; ]; If[Length[bps[[2]]] > 0, 
            check = checkPointinETC2[bps[[2]]]; If[Length[check] > 0, 
              AssociateTo[out, {StringJoin["2nd basepoint of ", trname, 
                  " and ", trgname]} -> check[[1]]], AssociateTo[out, 
               {StringJoin["2nd basepoint of ", trname, " and ", trgname]} -> 
                "NP"]]; ]; ]; , {trname, Keys[set] /. "infinite-altitude" -> 
           Nothing}]; AssociationMap[Print[#1] & , out]; , trname]]
 
trgFullCheck[trgchk_, scope_:{"pKIM", "oKIM", "plKIM", "pCPTR", "pENCTR", 
       "oCPTR", "oENCTR", "pUNCFTRG"}] := 
    Block[{ex}, Monitor[mon = "check"; ex = checkTriangleExists[trgchk]; 
       If[TrueQ[ex[[1]] != False], Print[ex]]; mon = "curves"; 
       ex = checkCurvesForTriangle[trgchk]; If[Length[ex] > 0, Print[ex]]; 
       If[MemberQ[scope, "pKIM"] || MemberQ[scope, "fKIM"], 
        mon = "pKIM"; Quiet[trgCheckPerspectivity[trgchk]]; ]; 
       If[MemberQ[scope, "pCPTR"] || MemberQ[scope, "fCPTR"], 
        mon = "pCPTR"; Quiet[trgCheckPerspectivity[trgchk, "TR", CPTR]]; ]; 
       If[MemberQ[scope, "pENCTR"] || MemberQ[scope, "fENCTR"], 
        mon = "pENCTR"; Quiet[trgCheckPerspectivity[trgchk, "TR", ENCTR]]; ]; 
       If[MemberQ[scope, "pUNCFTRG"] || MemberQ[scope, "fUNCFTRG"], 
        mon = "pUNCFTRG"; Quiet[trgCheckPerspectivity[trgchk, "TR", 
           UNCFTRG]]; ]; If[MemberQ[scope, "oKIM"] || MemberQ[scope, "fKIM"], 
        mon = "oKIM"; Quiet[trgCheckOrthologic[trgchk]]; ]; 
       If[MemberQ[scope, "oCPTR"] || MemberQ[scope, "fCPTR"], 
        mon = "oCPTR"; Quiet[trgCheckOrthologic[trgchk, "TR", CPTR]]; ]; 
       If[MemberQ[scope, "oENCTR"] || MemberQ[scope, "fENCTR"], 
        mon = "oENCTR"; Quiet[trgCheckOrthologic[trgchk, "TR", ENCTR]]; ]; 
       If[MemberQ[scope, "oUNCFTRG"] || MemberQ[scope, "fUNCFTRG"], 
        mon = "oUNCFTRG"; Quiet[trgCheckOrthologic[trgchk, "TR", 
           UNCFTRG]]; ]; If[MemberQ[scope, "plKIM"] || 
         MemberQ[scope, "fKIM"], mon = "plKIM"; 
         Quiet[trgCheckParallelogic[trgchk]]; ]; 
       If[MemberQ[scope, "plCPTR"] || MemberQ[scope, "fCPTR"], 
        mon = "plCPTR"; Quiet[trgCheckParallelogic[trgchk, "TR", CPTR]]; ]; 
       If[MemberQ[scope, "plENCTR"] || MemberQ[scope, "fENCTR"], 
        mon = "plENCTR"; Quiet[trgCheckParallelogic[trgchk, "TR", ENCTR]]; ]; 
       If[MemberQ[scope, "plUNCFTRG"] || MemberQ[scope, "fUNCFTRG"], 
        mon = "plUNCFTRG"; Quiet[trgCheckParallelogic[trgchk, "TR", 
           UNCFTRG]]; ]; drawTriangles[{trgchk}], mon]]
