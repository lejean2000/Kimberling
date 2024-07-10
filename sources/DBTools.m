pointProcessBary[expr_, rule_:rule69] := Module[{pointsBary, etcbary}, 
     etcbary = KeyDrop[ETCBaryNorm, globalExcludedNum]; 
      pointsBary = Quiet[Table[intnumericnorm[
          expr /. Thread[{u, v, w} -> val] /. rule], {val, etcbary}]]; 
      Return[AssociationThread[Keys[etcbary] -> pointsBary]]; ]
 
globalExcludedNum = 0
 
intnumericnorm[val_] := N[NormalizeBary[val], 35]
 
singlePointProcesses = <|"isogonal conjugate" -> {a^2*v*w, b^2*u*w, c^2*u*v}, 
     "isotomic conjugate" -> {v*w, u*w, u*v}, "complement" -> 
      {v + w, u + w, u + v}, "anticomplement" -> {-u + v + w, u - v + w, 
       u + v - w}, "anticomplement of isogonal conjugate" -> 
      {c^2*u*v + b^2*u*w - a^2*v*w, c^2*u*v - b^2*u*w + a^2*v*w, 
       -(c^2*u*v) + b^2*u*w + a^2*v*w}, 
     "anticomplement of isotomic conjugate" -> {-(v*w) + u*(v + w), 
       u*(v - w) + v*w, -(u*(v - w)) + v*w}, 
     "complement of isogonal conjugate" -> {u*(c^2*v + b^2*w), 
       v*(c^2*u + a^2*w), (b^2*u + a^2*v)*w}, 
     "complement of isotomic conjugate" -> {u*(v + w), v*(u + w), (u + v)*w}, 
     "zosma transform" -> {a*(a^2 + b^2 - c^2)*(a^2 - b^2 + c^2)*(c*v + b*w), 
       b*(a^2 + b^2 - c^2)*(-a^2 + b^2 + c^2)*(c*u + a*w), 
       c*(a^2 - b^2 + c^2)*(-a^2 + b^2 + c^2)*(b*u + a*v)}, 
     "eigentransform" -> bEigentransform[{u, v, w}], 
     "cyclocevian conjugate" -> bCyclocevianConjugate[{u, v, w}], 
     "circlecevian perspector" -> bCirclecevianPerspector[{u, v, w}], 
     "ortoassociate" -> bOrthoassociate[{u, v, w}], 
     "antitomic conjugate" -> bAntitomicConjugate[{u, v, w}], 
     "syngonal conjugate" -> bSyngonal[{u, v, w}]|>
 
intHarmonicProcess[fullgroups_, pt_, prec_] := 
    Module[{fgr1, checks, flatfg2, ingroupnbary, un, hgroups, hgroup, prev, 
      dump}, hgroups = {}; Do[fgr1 = SortBy[set, numsortexpr[#1] & ]; 
        If[Length[fgr1] > 200, fgr1 = Take[fgr1, 200]]; 
        flatfg2 = Subsets[fgr1, {2}]; checks = AssociationMap[
          NormalizeBary[bHarmonicConjugate[ETCBaryNorm[#1[[1]]], 
             ETCBaryNorm[#1[[2]]], pt]] & , flatfg2]; 
        ingroupnbary = KeySelect[ETCBaryNorm, MemberQ[fgr1, #1] & ]; 
        un = SortBy[Union[checks, ingroupnbary], #1[[1]] & ]; hgroup = {}; 
        prev = Association["X0" -> {0, 0, 0}]; dump = 0; 
        Do[If[coincide[un[el], prev[[1]]], AppendTo[hgroup, el]; 
            If[Length[hgroup] == 1, AppendTo[hgroup, Keys[prev][[1]]]; 
              dump = 1], If[dump == 1, If[AnyTrue[Length /@ hgroup, 
                #1 == 0 & ], AppendTo[hgroups, hgroup]]; hgroup = {}; 
              dump = 0; ]; ]; prev = Association[el -> un[el]]; , 
         {el, Keys[un]}], {set, fullgroups}]; Return[hgroups]; ]
 
numsortexpr[str_] := StringJoin[StringTake[str, 1], 
     StringPadLeft[StringTake[str, {2, -1}], 10, "0"]]
 
coincide[pt1_, pt2_, prec_:20] := Module[{t}, 
     If[pt1 == {0, 0, 0} || pt2 == {0, 0, 0}, Return[False], 
       t = Abs /@ Cross[pt1, pt2]; Return[Abs[t[[1]]] < 10^(-prec) && 
          Abs[t[[2]]] < 10^(-prec) && Abs[t[[3]]] < 10^(-prec)]; ]; ]
 
intPointCheck[pt_, process_, rule_:rule69] := Module[{tmp, res, ptn, ptnum}, 
     ptn = intnumericnorm[evaluate[pt] /. rule]; 
      tmp = pointProcessBary[process, rule]; 
      res = Select[(coincide[#1, ptn] & ) /@ tmp, TrueQ]; 
      If[Length[res] > 0, If[intVerifyPointProcess[pt, Keys[res][[1]], 
          process], Return[Keys[res][[1]]]]; ]; ]
 
intVerifyPointProcess[pt_, xnum_, processexpr_] := 
    Module[{ptn, pta, pti}, Do[ptn = intnumericnorm[evaluate[pt] /. rc]; 
        pta = intnumericnorm[KimberlingCenterCNy[xnum] /. rc]; 
        pti = intnumericnorm[evaluate[processexpr] /. {u, v, w} -> pta /. 
           rc]; If[ !coincide[ptn, pti], Return[False, Module]; ]; , 
       {rc, intCheckList}]; Return[True]; ]
 
xnum[str_] := If[NumericQ[ToExpression[StringTake[str, 1]]], 
     ToExpression[str], ToExpression[StringTake[str, {2, -1}]]]
 
KimberlingCenterCNy[key_] := evaluate[symmetrizeInternal[
      ETCFull[key] /. Thread[{A -> angleA, B -> angleB, C -> angleC}]]]
 
intCheckList = {{a -> 5, b -> 6, c -> 8}, {a -> 4, b -> 11, c -> 13}}
 
pointCheckAllProcesses[pt_, name_:"X"] := Module[{res, prop}, 
     Do[If[ !TrueQ[globalSilence], PrintTemporary[proc]]; 
        res = intPointCheck[pt, singlePointProcesses[proc]]; 
        If[StringQ[res], prop = StringJoin["= ", proc, " of ", 
            intaddbrackets[res]]; If[ !TrueQ[globalSilence], 
           Print[colorformat[prop]]]; AssociateTo[globalProperties[name], 
           proc -> intaddbrackets[res]]; ]; , 
       {proc, Keys[singlePointProcesses]}]; ]
 
intaddbrackets[pname_] := StringJoin[StringTake[pname, 1], "(", 
     StringTake[pname, {2, -1}], ")"]
 
colorformat[string_, cases_:RegularExpression[
       "Y\\(\\d+\\)|Y\\d+|Z\\(\\d+\\)|Z\\d+"]] := Module[{pos, agg, res}, 
     If[ !TrueQ[colorPrintOn], If[TrueQ[globalNoCleanup], 
         Return[string, Module], Return[cleanup[string], Module]; ]; ]; 
      pos = StringPosition[string, cases]; 
      agg = ({Switch[#1[[1,2]], 1, Red, 2, Brown, _, Blue], 
          #1[[1 ;; All,{1}]]} & ) /@ GatherBy[
         Tally[Flatten[Apply[Range, pos, {1}]]], Last]; 
      If[Length[agg] > 0, Return[intmark[string, agg]], Return[string]]; ]
 
globalNoCleanup = True
 
cleanup[string_] := StringReplace[string, 
     RegularExpression["[, ]*{+([Y|Z]*?[\\d, \\(\\)ABCX])*[Y|Z].*?}+"] -> ""]
 
intmark[number_, spec:{{_, _}..}] := 
    Row[With[{n = Characters[ToString[number]]}, 
      Fold[Function[{x, y}, MapAt[Style[#1, y[[1]]] & , x, y[[2]]]], n, 
       spec]]]
 
globalProperties = <||>
 
checkCircumconics[pt_, excl_:0, name_:"X"] := 
    Module[{ptc, list1, list2, list3, list4, p1, p2, out, check, bary20}, 
     out = {}; ptc = intnumericnorm[evaluate[pt] /. rule69]; 
      list1 = intLinesProcessFullGroups[intnumericnorm[
          bIsogonalConjugate[ptc] /. rule69], 24, ETCBaryNormFull][[2]]; 
      list2 = Table[AssociationMap[intnumericnorm[
           bIsogonalConjugate[KimberlingCenterCNy[#1]] /. rule69] & , 
         list1[[i]]], {i, 1, Length[list1]}]; 
      bary20 = (Round[#1, 1.*^-19] & ) /@ ETCBaryNormFull; 
      list2 = ((Round[#1, 1.*^-19] & ) /@ #1 & ) /@ list2; 
      list3 = Select[(keyIntersectionValues[bary20, #1] & ) /@ list2, 
        Length[#1] > 1 & ]; list4 = ({#1[[1]][[2]], #1[[2]][[2]]} & ) /@ 
        (Take[#1, 2] & ) /@ (SortBy[#1, numsortexpr[#1[[2]]] & ] & ) /@ 
          list3; list4 = SortBy[list4, numsortexpr[#1[[1]]] & ]; 
      Do[p1 = cnc[[1]]; p2 = cnc[[2]]; If[p1 == excl || p2 == excl, 
         Continue[]]; check = checkPointOnCurveNum[bCircumconicEq[
           KimberlingCenterCNy[p1], KimberlingCenterCNy[p2]], pt]; 
        If[check, AppendTo[out, StringJoin["{{A, B, C, ", intaddbrackets[p1], 
            ", ", intaddbrackets[p2], "}}"]]; ]; , {cnc, list4}]; 
      Quiet[AssociateTo[globalProperties[name], {"circumconics" -> out}]]; 
      If[ !TrueQ[globalSilence], If[Length[out] > 0, 
        Print[colorformat[StringJoin[
            "= intersection, other than A, B, C, of circumconics: ", 
            StringRiffle[out, ", "]]]]; ]]; Return[out]; ]
 
intLinesProcessFullGroups[pt_, prec_, etcset_] := 
    Module[{tplist, tp, prev, outgroups, group, fullgroups, dump, as}, 
     fullgroups = {}; outgroups = {}; tplist = 
       Table[{name, N[NormalizeBary[1/bLine[pt, etcset[name]] /. rule69], 
          prec]}, {name, Keys[etcset]}]; tplist = Select[tplist, 
        AllTrue[#1[[2]], Internal`RealValuedNumericQ] & ]; 
      tplist = SortBy[tplist, #1[[2]][[1]] & ]; 
      as = AssociationThread[(#1[[1]] & ) /@ tplist, (#1[[2]] & ) /@ tplist]; 
      fullgroups = Select[Values[PositionIndex[as]], Length[#1] > 1 & ]; 
      outgroups = (Take[SortBy[#1, numsortexpr[#1] & ], 2] & ) /@ fullgroups; 
      outgroups = SortBy[outgroups, numsortexpr[#1[[1]]] & ]; 
      Return[{outgroups, fullgroups}]; ]
 
keyIntersectionValues[list1_, list2_] := Module[{reverse, res, ress}, 
     reverse[assoc_] := Thread[Values[assoc] -> Keys[assoc]]; 
      res = KeyIntersection[{reverse[list1], PositionIndex[list2]}]; 
      ress = Thread[{First /@ Values[res[[2]]], Values[res[[1]]]}]; ress]
 
checkInconics[pt_, excl_:0, name_:"X"] := Module[{ptc, out, crv}, 
     out = {}; ptc = intnumericnorm[evaluate[pt] /. rule69]; 
      crv = curveSimplify[bInconicEq[{u, v, w}]]; 
      set1 = Keys[Select[Association[Table[
           k -> crv /. Thread[{x, y, z} -> ptc] /. Thread[{u, v, w} -> 
              ETCBaryNorm[k]], {k, Keys[ETCBaryNormFull]}]], 
         Abs[#1] < 10^(-20) & ]]; 
      Do[check = checkPointOnCurveNum[bInconicEq[KimberlingCenterCNy[ptk]], 
          pt]; If[check, AppendTo[out, intaddbrackets[ptk]]; ]; , 
       {ptk, set1}]; AssociateTo[globalProperties[name], 
       {"inconics" -> out}]; If[ !TrueQ[globalSilence], 
       If[Length[out] > 0, Print[colorformat[StringJoin[
            "= lies on inconics with perspector: ", StringRiffle[out, 
             ", "]]]]; ]]; Return[out]; ]
 
linesProcessAlg[ptcoord_, printexpr_, prec_, debug_, abort_, name_, 
     quick_:False] := Module[{res, gr, hg, out, head, test, test2, hgroups, 
      ptc, unproven, rc, rc2, eltest, sout, barys, outname, harm, ff2, 
      outforsort}, rc = intCheckList[[1]]; rc2 = intCheckList[[2]]; 
      ptc = intnumericnorm[evaluate[ptcoord] /. rule69]; 
      res = intLinesProcessFullGroups[ptc, prec, ETCBaryNorm]; gr = res[[1]]; 
      out = {}; unproven = {}; Do[If[debug, PrintTemporary[el]]; 
        test = TimeConstrained[bCollinearityMatrix[
           KimberlingCenterCNy[el[[1]]] /. rc, KimberlingCenterCNy[
             el[[2]]] /. rc, ptcoord /. rc], 10, -1]; 
        test2 = TimeConstrained[bCollinearityMatrix[
           KimberlingCenterCNy[el[[1]]] /. rc2, KimberlingCenterCNy[
             el[[2]]] /. rc2, ptcoord /. rc2], 10, -1]; 
        If[TrueQ[Simplify[test] == 0] && TrueQ[Simplify[test2] == 0], 
         AppendTo[out, el], AppendTo[unproven, el]]; , {el, gr}]; 
      outname = "(name pending)"; If[Length[out] >= 2, 
       outforsort = Select[out, StringTake[#1[[1]], 1] == "X" && 
            StringTake[#1[[2]], 1] == "X" & ]; 
        sout = SortBy[outforsort, xnumforsort[#1[[1]]]*xnumforsort[
             #1[[2]]] & ]; If[Length[sout] >= 2, 
         outname = StringJoin[intaddbrackets[sout[[1]][[1]]], 
            intaddbrackets[sout[[1]][[2]]], "\:2229", intaddbrackets[
             sout[[2]][[1]]], intaddbrackets[sout[[2]][[2]]]]; ]; ]; 
      AssociateTo[globalProperties[name], {"name" -> outname}]; 
      If[ !TrueQ[globalSilence], Print[colorformat[outname]]]; 
      If[ !FreeQ[printexpr, S], barys = ExpressionToTrad[
         Activate[Collect[printexpr, S, Inactive[Simplify]] /. 
           Simplify -> intFullSimplifyFactors]], 
       barys = intSimplifyFactorsToTrad[printexpr]]; 
      AssociateTo[globalProperties[name], {"barycentrics" -> barys}]; 
      If[ !TrueQ[globalSilence], Print[StringJoin["Barycentrics    ", 
         barys]]]; out = ({intnameformat[#1[[1]]], intnameformat[
           #1[[2]]]} & ) /@ out; AssociateTo[globalProperties[name], 
       {"lines" -> out}]; If[ !TrueQ[globalSilence], 
       Print[colorformat[StringJoin["Lies on these lines: ", 
          StringRiffle[out, ", "]]]]]; 
      If[abort && Length[out] < pointCheckerMinProperties, 
       Return[out, Module]]; If[ !quick, checkLinearCombinations[ptcoord, 
         out, name]; harm = intHarmonicProcess[res[[2]], ptc, prec]; 
        harm = (SortBy[#1, Length] & ) /@ harm; ff2[in_] := 
         ({in[[1]], #1} & ) /@ Take[in, {2, -1}]; 
        harm = (#1[[1]] & ) /@ ff2 /@ harm; hg = {}; 
        Do[If[Length[igroup] != 2, Continue[]]; 
          If[Sort[{Length[igroup[[1]]], Length[igroup[[2]]]}] != {0, 2}, 
           Continue[]]; head = Select[igroup, Length[#1] == 0 & ]; 
          Do[If[el == head[[1]], Continue[]]; eltest = 
             Flatten[{el, head[[1]]}]; test = Det[{{1, 1, 1}, 
               bHarmonicConjugate[KimberlingCenterCNy[eltest[[1]]] /. rc, 
                KimberlingCenterCNy[eltest[[2]]] /. rc, KimberlingCenterCNy[
                  eltest[[3]]] /. rc], ptcoord /. rc}]; 
            test2 = Det[{{1, 1, 1}, bHarmonicConjugate[KimberlingCenterCNy[
                  eltest[[1]]] /. rc2, KimberlingCenterCNy[eltest[[2]]] /. 
                 rc2, KimberlingCenterCNy[eltest[[3]]] /. rc2], ptcoord /. 
                rc2}]; If[TrueQ[Simplify[test] == 0] && 
              TrueQ[Simplify[test2] == 0], AppendTo[hg, eltest]; ]; , 
           {el, igroup}], {igroup, harm}]; 
        hg = SortBy[hg, numsortexpr[#1[[1]]] & ]; 
        hg = ({intnameformat[#1[[1]]], intnameformat[#1[[2]]], 
            intnameformat[#1[[3]]]} & ) /@ hg; AssociateTo[
         globalProperties[name], {"harmonic" -> hg}]; 
        If[ !TrueQ[globalSilence], If[Length[hg] > 0, 
          Print[colorformat[StringJoin[
              "= {X(i),X(j)}-harmonic conjugate of X(k) for these (i,j,k): ", 
              StringRiffle[hg, ", "]]]]; ]]; hg = {}; 
        Do[eltest = igroup; test = Cross[bMidpoint[KimberlingCenterCNy[
               eltest[[1]]] /. rc, KimberlingCenterCNy[eltest[[2]]] /. rc], 
            ptcoord /. rc]; test2 = Cross[bMidpoint[KimberlingCenterCNy[
               eltest[[1]]] /. rc2, KimberlingCenterCNy[eltest[[2]]] /. rc2], 
            ptcoord /. rc2]; If[TrueQ[Simplify[test] == {0, 0, 0}] && 
            TrueQ[Simplify[test2] == {0, 0, 0}], AppendTo[hg, eltest]; ]; , 
         {igroup, intMidpointProcess[res[[2]], ptc, prec]}]; 
        hg = SortBy[hg, numsortexpr[#1[[1]]] & ]; 
        hg = ({intnameformat[#1[[1]]], intnameformat[#1[[2]]]} & ) /@ hg; 
        AssociateTo[globalProperties[name], {"midpoints" -> hg}]; 
        If[ !TrueQ[globalSilence], If[Length[hg] > 0, 
          Print[colorformat[StringJoin[
              "= midpoint of X(i) and X(j) for these {i,j}: ", 
              StringRiffle[hg, ", "]]]]; ]]; hg = {}; 
        Do[eltest = igroup; test = Det[{{1, 1, 1}, bReflectionPP[
              KimberlingCenterCNy[eltest[[1]]] /. rc, KimberlingCenterCNy[
                eltest[[2]]] /. rc], ptcoord /. rc}]; 
          test2 = Det[{{1, 1, 1}, bReflectionPP[KimberlingCenterCNy[
                eltest[[1]]] /. rc2, KimberlingCenterCNy[eltest[[2]]] /. 
               rc2], ptcoord /. rc2}]; If[TrueQ[Simplify[test] == 0] && 
            TrueQ[Simplify[test2] == 0], AppendTo[hg, eltest]; ]; , 
         {igroup, intReflectionProcess[res[[2]], ptc, prec]}]; 
        hg = SortBy[hg, numsortexpr[#1[[1]]] & ]; 
        hg = ({intnameformat[#1[[1]]], intnameformat[#1[[2]]]} & ) /@ hg; 
        AssociateTo[globalProperties[name], {"reflections" -> hg}]; 
        If[ !TrueQ[globalSilence], If[Length[hg] > 0, 
          Print[colorformat[StringJoin[
              "= reflection of X(i) in X(j) for these {i,j}: ", 
              StringRiffle[hg, ", "]]]]; ]]; ]; Return[out]; ]
 
xnumforsort[str_] := If[StringTake[str, 1] == "X", 
     ToExpression[StringTake[str, {2, -1}]], 
     100000 + ToExpression[StringTake[str, {2, -1}]]]
 
intnameformat[pname_] := If[StringTake[pname, 1] == "X", 
     StringTake[pname, {2, -1}], pname]
 
pointCheckerMinProperties = 3
 
checkLinearCombinations[pt_, ptpairset_, inname_:"X"] := 
    Module[{out, pt1, pt2, summ, sol, coef, numerator, denominator, p1, p2}, 
     out = {}; Do[pt1 = evaluate[KimberlingCenterC[
           If[MemberQ[{"Y", "Z"}, StringTake[pair[[1]], 1]], pair[[1]], 
            StringJoin["X", pair[[1]]]]]]; 
        If[ !RationalExpressionQ[pt1[[1]], {a, b, c}], Continue[]]; 
        pt1 = simplifyRationalBarycentrics[pt1]; 
        If[Simplify[Total[pt1]] == 0, Continue[]]; 
        pt2 = evaluate[KimberlingCenterC[If[MemberQ[{"Y", "Z"}, 
             StringTake[pair[[2]], 1]], pair[[2]], StringJoin["X", 
             pair[[2]]]]]]; If[ !RationalExpressionQ[pt2[[1]], {a, b, c}], 
         Continue[]]; pt2 = simplifyRationalBarycentrics[pt2]; 
        If[Simplify[Total[pt2]] == 0, Continue[]]; 
        summ = simplifyRationalBarycentrics[(pt1/Total[pt1])*t + 
           pt2/Total[pt2]]; sol = TimeConstrained[
          Simplify[Solve[summ[[1]] == k*pt[[1]] && summ[[2]] == k*pt[[2]], 
            {k, t}]], 5, {}]; If[Length[sol] > 0 && NumberQ[t /. sol[[1]]], 
         coef = t /. sol[[1]]; pair = ToString /@ pair; 
          If[MemberQ[{"Y", "Z"}, StringTake[pair[[1]], 1]], 
           p1 = StringJoin[StringTake[pair[[1]], 1], "[", 
             StringTake[pair[[1]], {2, -1}], "]"], 
           p1 = StringJoin["X[", pair[[1]], "]"]; ]; 
          If[MemberQ[{"Y", "Z"}, StringTake[pair[[2]], 1]], 
           p2 = StringJoin[StringTake[pair[[2]], 1], "[", 
             StringTake[pair[[2]], {2, -1}], "]"], 
           p2 = StringJoin["X[", pair[[2]], "]"]; ]; numerator = 
           If[Numerator[coef] != 1, StringJoin[ToString[Numerator[coef]], 
             "*"], ""]; denominator = If[Denominator[coef] != 1, 
            StringJoin[ToString[Denominator[coef]], "*"], ""]; 
          AppendTo[out, StringJoin[numerator, p1, "+", denominator, p2]]; ]; 
        If[Length[out] >= 20, Break[]]; , {pair, ptpairset}]; 
      AssociateTo[globalProperties[inname], {"linear combinations" -> out}]; 
      If[Length[out] > 0, If[ !TrueQ[globalSilence], 
         Print[colorformat[StringJoin["linear combinations: ", 
            StringRiffle[out, ", "]]]]]; ]; ]
 
intMidpointProcess[fullgroups_, pt_, prec_] := 
    Module[{fgr1, checks, flatfg2, ingroupnbary, un, hgroups, hgroup, prev, 
      dump}, hgroups = {}; Do[fgr1 = SortBy[set, numsortexpr[#1] & ]; 
        If[Length[fgr1] > 200, fgr1 = Take[fgr1, 200]; ]; 
        flatfg2 = Subsets[fgr1, {2}]; ingroupnbary = AssociationMap[
          Abs[NormalizeBary[bMidpoint[ETCBaryNorm[#1[[1]]], ETCBaryNorm[
                #1[[2]]]]] - pt] & , flatfg2]; 
        hgroup = Select[ingroupnbary, #1[[1]] < 10^(-prec) && 
            #1[[2]] < 10^(-prec) && #1[[3]] < 10^(-prec) & ]; 
        If[Length[hgroup] > 0, hgroups = Union[hgroups, Keys[hgroup]]]; , 
       {set, fullgroups}]; Return[hgroups]; ]
 
intReflectionProcess[fullgroups_, pt_, prec_] := 
    Module[{fgr1, checks, flatfg2, ingroupnbary, un, hgroups, hgroup, prev, 
      dump}, hgroups = {}; Do[fgr1 = SortBy[set, numsortexpr[#1] & ]; 
        If[Length[fgr1] > 200, fgr1 = Take[fgr1, 200]; ]; 
        checks = AssociationMap[NormalizeBary[bMidpoint[ETCBaryNorm[#1], 
             pt]] & , fgr1]; 
        Do[refl = Select[fgr1, coincide[ETCBaryNorm[#1], checks[el]] & ]; 
          If[Length[refl] > 0, AppendTo[hgroups, {el, First[refl]}]], 
         {el, Keys[checks]}]; , {set, fullgroups}]; Return[hgroups]; ]
 
checkIsogonalConjugates[pt_, name_:"X"] := 
    Module[{cx, prev, res, idx1, idx2, ptc, rc, ffisoconjugate, etcbary}, 
     ffisoconjugate[pt1_, pt2_] := Module[{local}, 
        local = bPIsogonalConjugate[intnumericnorm[pt1 /. rule69], 
            intnumericnorm[pt2 /. rule69]] /. rule69; 
         Return[intnumericnorm[local]]; ]; etcbary = KeyDrop[ETCBaryNorm, 
        globalExcludedNum]; rc = intCheckList[[1]]; 
      ptc = intnumericnorm[evaluate[pt] /. rule69]; 
      cx = (ffisoconjugate[ptc, #1] & ) /@ etcbary; 
      cx = Union[AssociationThread[(StringJoin["C", #1] & ) /@ Keys[cx], 
         Values[cx]], etcbary]; cx = SortBy[Select[cx, Im[#1[[1]]] == 0 & ], 
        #1[[1]] & ]; prev = Association["X0" -> {-1, -1, -1}]; res = {}; 
      Do[If[(StringTake[Keys[prev][[1]], 1] == "C" && StringTake[n, 1] == 
            "C") || (StringTake[Keys[prev][[1]], 1] != "C" && 
           StringTake[n, 1] != "C"), prev = Association[n -> cx[n]]; 
          Continue[]]; If[coincide[cx[n], prev[[1]], 15], 
         idx1 = If[StringTake[Keys[prev][[1]], 1] == "C", 
            StringTake[Keys[prev][[1]], {2, -1}], Keys[prev][[1]]]; 
          idx2 = If[StringTake[n, 1] == "C", StringTake[n, {2, -1}], n]; 
          If[coincideNorm[evaluate[pt] /. rc, bPIsogonalConjugate[
              KimberlingCenterCNy[idx1], KimberlingCenterCNy[idx2]] /. rc], 
           If[LexicographicOrder[numsortexpr[idx1], numsortexpr[idx2]] == 1, 
            AppendTo[res, {idx1, idx2}], AppendTo[res, {idx2, idx1}]]]]; 
        prev = Association[n -> cx[n]]; , {n, Keys[cx]}]; 
      res = SortBy[DeleteDuplicates[res], numsortexpr[#1[[1]]] & ]; 
      res = ({intnameformat[#1[[1]]], intnameformat[#1[[2]]]} & ) /@ res; 
      AssociateTo[globalProperties[name], {"isoconjugate" -> res}]; 
      If[ !TrueQ[globalSilence], If[Length[res] > 0, 
        Print[colorformat[StringJoin[
            "= X(i)-isoconjugate-of-X(j) for these {i, j}: ", 
            StringRiffle[res, ", "]]]]; ]]; ]
 
coincideNorm[pt1_, pt2_, prec_:20] := Return[coincide[intnumericnorm[pt1], 
      intnumericnorm[pt2], prec]]
 
checkBarycentric[pt_, type_, name_:"X"] := 
    Module[{ff, rc, cx, res, idx1, idx2, ptc, val, sgn, tuples, etcbary}, 
     If[type == "quotient", ff[pt1_, pt2_] := intnumericnorm[
         (pt1 /. rule69)*(pt2 /. rule69)], ff[pt1_, pt2_] := 
        intnumericnorm[(pt1 /. rule69)/(pt2 /. rule69)]]; 
      etcbary = KeyDrop[ETCBaryNorm, globalExcludedNum]; 
      ptc = intnumericnorm[evaluate[pt] /. rule69]; 
      cx = (ff[ptc, #1] & ) /@ etcbary; 
      cx = Union[AssociationThread[(StringJoin["C", #1] & ) /@ Keys[cx], 
         Values[cx]], etcbary]; cx = (Round[#1, 1.*^-19] & ) /@ cx; 
      tuples = Select[Select[Values[PositionIndex[cx]], Length[#1] > 1 & ], 
         !((StringTake[#1[[1]], 1] == "C" && StringTake[#1[[2]], 1] == 
             "C") || (StringTake[#1[[1]], 1] != "C" && 
            StringTake[#1[[2]], 1] != "C")) & ]; res = {}; 
      rc = intCheckList[[1]]; Do[idx1 = If[StringTake[i[[1]], 1] == "C", 
          StringTake[i[[1]], {2, -1}], i[[1]]]; 
        idx2 = If[StringTake[i[[2]], 1] == "C", StringTake[i[[2]], {2, -1}], 
          i[[2]]]; If[type == "quotient", If[coincideNorm[evaluate[pt] /. rc, 
            (KimberlingCenterCNy[idx1] /. rc)/(KimberlingCenterCNy[idx2] /. 
              rc)], AppendTo[res, {idx1, idx2}], 
           If[coincideNorm[evaluate[pt] /. rc, (KimberlingCenterCNy[idx2] /. 
                rc)/(KimberlingCenterCNy[idx1] /. rc)], AppendTo[res, 
              {idx2, idx1}]]; ]; ]; If[type == "product" && 
          coincideNorm[evaluate[pt] /. rc, KimberlingCenterCNy[idx2]*
             KimberlingCenterCNy[idx1] /. rc], 
         If[LexicographicOrder[idx1, idx2] == 1, AppendTo[res, {idx1, idx2}], 
           AppendTo[res, {idx2, idx1}]]; ]; , {i, tuples}]; 
      If[type == "quotient", sgn = "/", sgn = "*"]; 
      res = SortBy[DeleteDuplicates[res], numsortexpr[#1[[1]]] & ]; 
      res = ({intnameformat[#1[[1]]], intnameformat[#1[[2]]]} & ) /@ res; 
      AssociateTo[globalProperties[name], 
       {StringJoin["barycentric ", type] -> res}]; If[ !TrueQ[globalSilence], 
       If[Length[res] > 0, Print[colorformat[StringJoin["= barycentric ", 
             type, " X(i)", sgn, "X(j) for these (i, j): ", StringRiffle[res, 
              ", "]]]]; ]; ]; ]
 
checkTrilinearPolar[pt_, name_:"X"] := Module[{cx, ptc, p1, p2, dset, test}, 
     ptc = N[Normalize[pt /. rule69], 35]; cx = bTripolarEq[ptc] . {x, y, z}; 
      dset = (Abs[cx] /. Thread[{x, y, z} -> #1] & ) /@ ETCBaryNorm; 
      test = Select[dset, #1 < 10^(-10) & ]; If[Length[test] > 1, 
       p1 = Keys[test][[1]]; p2 = Keys[test][[2]]; 
        If[Simplify[Det[{{1, 1, 1}, pt, bTripole[KimberlingCenterCNy[p1], 
              KimberlingCenterCNy[p2]]}]] == 0, 
         AssociateTo[globalProperties[name], {"trilinear polar" -> 
             {p1, p2}}]; If[ !TrueQ[globalSilence], 
           Print[colorformat[StringJoin["= trilinear pole of line {", 
              intnameformat[p1], ", ", intnameformat[p2], "}"]]]]; ]; ]; ]
 
checkVertexConjugates[pt_, name_:"X"] := 
    Module[{cx, prev, res, idx1, idx2, ptc, rc, ffvertexconjugate, tuples}, 
     ffvertexconjugate[pt1_, pt2_] := Module[{local}, 
        local = bVertexConjugate[intnumericnorm[pt1], intnumericnorm[pt2]] /. 
           rule69; Return[intnumericnorm[local]]; ]; rc = intCheckList[[1]]; 
      ptc = intnumericnorm[evaluate[pt] /. rule69]; 
      cx = (ffvertexconjugate[ptc, #1] & ) /@ ETCBaryNorm; 
      cx = Union[AssociationThread[(StringJoin["C", #1] & ) /@ Keys[cx], 
         Values[cx]], ETCBaryNorm]; cx = (Round[#1, 1.*^-19] & ) /@ cx; 
      cx = SortBy[Select[cx, Im[#1[[1]]] == 0 && Im[#1[[2]]] == 0 && 
           Im[#1[[3]]] == 0 & ], #1[[1]] & ]; res = {}; 
      prev = Association["X0" -> {0, 0, 0}]; 
      Do[If[(StringTake[Keys[prev][[1]], 1] == "C" && StringTake[n, 1] == 
            "C") || (StringTake[Keys[prev][[1]], 1] != "C" && 
           StringTake[n, 1] != "C"), prev = Association[n -> cx[n]]; 
          Continue[]]; If[coincide[cx[n], prev[[1]], 15], 
         idx1 = If[StringTake[Keys[prev][[1]], 1] == "C", 
            StringTake[Keys[prev][[1]], {2, -1}], Keys[prev][[1]]]; 
          idx2 = If[StringTake[n, 1] == "C", StringTake[n, {2, -1}], n]; 
          If[coincideNorm[evaluate[pt] /. rc, bVertexConjugate[
              KimberlingCenterCNy[idx1], KimberlingCenterCNy[idx2]] /. rc], 
           If[LexicographicOrder[numsortexpr[idx1], numsortexpr[idx2]] == 1, 
            AppendTo[res, {idx1, idx2}], AppendTo[res, {idx2, idx1}]]]]; 
        prev = Association[n -> cx[n]]; , {n, Keys[cx]}]; 
      res = SortBy[DeleteDuplicates[res], numsortexpr[#1[[1]]] & ]; 
      res = ({intnameformat[#1[[1]]], intnameformat[#1[[2]]]} & ) /@ res; 
      AssociateTo[globalProperties[name], {"vertex conjugate" -> res}]; 
      If[ !TrueQ[globalSilence], If[Length[res] > 0, 
        Print[colorformat[StringJoin[
            "= X(i)-vertex conjugate of X(j) for these {i, j}: ", 
            StringRiffle[res, ", "]]]]; ]]; ]
 
pointChecker[expr_, num_:0, full_:False, inname_:"X"] := 
    Module[{abstime, ptcoord, pt, chk, lines, barys, symcheck, name, numcon}, 
     tmpsessiontime = SessionTime[]; If[inname != "X" && 
        KeyExistsQ[globalProperties, inname], 
       Print[StringJoin["Key ", inname, " exists in global properties !"]]; 
        Return[False, Module]]; globalExcludedNum = addxtoname[num]; 
      ptcoord = evaluate[expr]; If[ !checkCentralExpression[ptcoord], 
       Return[False, Module]; ]; pt = intnumericnorm[evaluate[ptcoord] /. 
         rule69]; If[Length[globalSeenPoints] > 0 && 
        First[MinimalBy[Value][(Total[Abs[#1[[2]] - pt]] & ) /@ 
            globalSeenPoints]] <= 10^(-20), Print[inname]; 
        Print["Point seen"]; Return[False, Module], 
       AppendTo[globalSeenPoints, {ptcoord, pt}]; ]; 
      If[num != 0, chk = 0, chk = checkPointinETC2[ptcoord]]; 
      If[Length[chk] > 0, Print[colorformat[StringJoin["ETC: ", chk]]]; 
        Return[False, Module], barys = Factor[FactorTermsList[expr[[1]]][[
           2]]]; If[StringLength[inname] == 0, 
         name = ToString[ExpressionToTrad[expr[[1]]]], name = inname]; 
        AssociateTo[globalProperties, name -> Association[]]; 
        If[ !TrueQ[globalSilence], Print["Starting..."]]; 
        lines = Quiet[linesProcessAlg[ptcoord, barys, 20, False, True, 
           name]]; If[ !TrueQ[globalSilence], printSessionTime[]; 
          Print["Starting circumconics"]; ]; 
        If[full || Length[lines] >= pointCheckerMinProperties, 
         numcon = Quiet[checkCircumconics[ptcoord, num, name]]; 
          If[ !TrueQ[globalSilence], printSessionTime[]; 
            Print["Starting curves"]; ]; Quiet[checkCurves[ptcoord, name]]; 
          If[ !TrueQ[globalSilence], printSessionTime[]; 
            Print["Starting inverse"]; ]; Quiet[checkInverse[ptcoord, name]]; 
          If[ !TrueQ[globalSilence], printSessionTime[]; 
            Print["Starting trilinear"]; ]; Quiet[checkTrilinearPolar[
            ptcoord, name]]; If[ !TrueQ[globalSilence], printSessionTime[]; 
            Print["Starting conjugates"]; ]; Quiet[checkIsogonalConjugates[
            ptcoord, name]]; Quiet[checkConjugates[ptcoord, bDaoConjugate, 
            "= X(i)-Dao conjugate of X(j) for these {i, j}: ", name]]; 
          Quiet[checkConjugates[ptcoord, bCevianQuotient, 
            "= X(i)-Ceva conjugate of X(j) for these {i, j}: ", name]]; 
          Quiet[checkConjugates[ptcoord, bAnticomplementaryConjugate, 
            "= X(i)-anticomplementary conjugate of X(j) for these {i, j}: ", 
            name]]; Quiet[checkConjugates[ptcoord, bComplementaryConjugate, 
            "= X(i)-complementary conjugate of X(j) for these {i, j}: ", 
            name]]; Quiet[checkConjugates[ptcoord, bCrossConjugate, 
            "= X(i)-cross conjugate of X(j) for these {i, j}: ", name]]; 
          Quiet[checkVertexConjugates[ptcoord, name]]; 
          If[ !TrueQ[globalSilence], printSessionTime[]; 
            Print["Starting circles"]; ]; Quiet[checkCircles[ptcoord, name]]; 
          Quiet[checkInconics[ptcoord, num, name]]; 
          If[ !TrueQ[globalSilence], printSessionTime[]; 
            Print["Starting poles"]; ]; TimeConstrained[
           Quiet[checkPoles[ptcoord, name]], 180]; If[ !TrueQ[globalSilence], 
           printSessionTime[]; Print["Starting conic centers"]; ]; 
          Quiet[checkPerspector[ptcoord, name]]; 
          Quiet[checkConicCenter[ptcoord, name]]; If[ !TrueQ[globalSilence], 
           printSessionTime[]; Print["Starting baricentrics"]; ]; 
          Quiet[checkBarycentric[ptcoord, "product", name]]; 
          Quiet[checkBarycentric[ptcoord, "quotient", name]]; 
          If[ !TrueQ[globalSilence], printSessionTime[]; 
            Print["Starting processes"]; ]; TimeConstrained[
           Quiet[pointCheckAllProcesses[ptcoord, name]], 90]; 
          If[ !TrueQ[globalSilence], Print["==========="]]; ]; ]; 
      Return[True]; ]
 
tmpsessiontime = 0
 
addxtoname[str_] := If[NumericQ[ToExpression[StringTake[ToString[str], 1]]], 
     StringJoin["X", ToString[str]], str]
 
checkCentralExpression[ptc_] := Module[{ptn, symcheck, symcheck2, symcheck3}, 
     ptn = intnumericnorm[evaluate[ptc] /. rule69]; 
      symcheck = Simplify[Cross[ptn, intnumericnorm[
          symmetrizeInternal2[evaluate[ptc[[1]]]] /. rule69]]]; 
      symcheck2 = Simplify[Abs[evaluate[ptc[[1]]]] - 
          Abs[evaluate[ptc[[1]]] /. Thread[{b -> c, c -> b}]] /. rule69]; 
      symcheck3 = Simplify[Cross[ptn, intnumericnorm[
          evaluate[ptc] /. Thread[{a -> 2*a, b -> 2*b, c -> 2*c}] /. 
           rule69]]]; If[AnyTrue[symcheck, Abs[#1] > 10^(-20) & ] || 
        Abs[symcheck2] > 10^(-20) || AnyTrue[symcheck3, 
         Abs[#1] > 10^(-20) & ], Print[ptc]; 
        Print["expression is not symmetric"]; Return[False]; ]; 
      Return[True]; ]
 
globalSeenPoints = {}
 
printSessionTime[] := Module[{t}, Print[SessionTime[] - tmpsessiontime]; 
      tmpsessiontime = SessionTime[]; ]
 
checkCurves[pt_, inname_:"X"] := Module[{out, ptest, ptest2, d, secondcheck, 
      crv, normcoef, curves, circset}, 
     out = {}; ptest = intnumericnorm[evaluate[pt] /. rule69]; 
      curves = TriangleCurves; circset = KeySelect[CentralCircles, 
         !StringContainsQ[#1, "A-"] &&  !StringContainsQ[#1, "B-"] && 
           !StringContainsQ[#1, "C-"] &&  !StringContainsQ[#1, "-A"] && 
           !StringContainsQ[#1, "-B"] &&  !StringContainsQ[#1, "-C"] & ]; 
      AppendTo[curves, circset]; Do[monitorvar = name; 
        crv = evaluate[curves[name]] /. rule69; normcoef = 
         Max[Flatten[Abs[CoefficientList[crv, {x, y, z}]]]]; 
        d = crv/normcoef /. Thread[{x, y, z} -> ptest]; 
        If[Abs[d] < 10^(-12), secondcheck = True; 
          Do[ptest2 = N[NormalizeBary[evaluate[pt] /. rc], 35]; 
            d = curves[name] /. Thread[{x, y, z} -> ptest2] /. rc; 
            If[Abs[d] > 10^(-12), secondcheck = False]; , 
           {rc, intCheckList}]; If[secondcheck, AppendTo[out, name]]; ]; , 
       {name, Keys[curves]}]; Quiet[AssociateTo[globalProperties[inname], 
        {"curves" -> out}]]; If[Length[out] > 0, 
       If[ !TrueQ[globalSilence], Print[StringJoin["= lies on curves: ", 
           StringRiffle[out, ", "]]]]; ]; Return[out]; ]
 
checkInverse[pt_, inname_:"X"] := Module[{out, ptest, circset, ptc, check, 
      secondcheck, d}, out = {}; If[Total[evaluate[pt] /. rule69] != 0, 
       circset = KeySelect[CentralCircles,  !StringContainsQ[#1, "A-"] && 
             !StringContainsQ[#1, "B-"] &&  !StringContainsQ[#1, "C-"] && 
             !StringContainsQ[#1, "-A"] &&  !StringContainsQ[#1, "-B"] && 
             !StringContainsQ[#1, "-C"] & ]; 
        Do[ptc = bInverseInConic[pt, circset[cnc]]; 
          check = checkPointinETC2[ptc]; If[Length[check] > 0 && 
            check[[1]] != globalExcludedNum, 
           AppendTo[out, StringJoin["inverse of ", intaddbrackets[check[[
                1]]], " in ", cnc]]; ]; , {cnc, Keys[circset]}]; ]; 
      AssociateTo[globalProperties[inname], {"inverses" -> out}]; 
      If[ !TrueQ[globalSilence], 
       (Print[colorformat[StringJoin["= ", #1]]] & ) /@ out]; ]
 
checkConjugates[pt_, func_, str_, name_:"X"] := 
    Module[{cx, res, idx1, idx2, i1, i2, ptc, rc, ff, fncname, tuples, 
      etcbary}, ff[pt1_, pt2_] := Module[{local}, 
        local = func[intnumericnorm[pt1 /. rule69], intnumericnorm[
             pt2 /. rule69]] /. rule69; Return[intnumericnorm[local]]; ]; 
      etcbary = KeyDrop[ETCBaryNorm, globalExcludedNum]; 
      rc = intCheckList[[1]]; ptc = intnumericnorm[evaluate[pt] /. rule69]; 
      cx = (ff[#1, ptc] & ) /@ etcbary; 
      cx = Union[AssociationThread[(StringJoin["C", #1] & ) /@ Keys[cx], 
         Values[cx]], etcbary]; cx = (Round[#1, 1.*^-20] & ) /@ cx; 
      tuples = Select[Select[Values[PositionIndex[cx]], Length[#1] > 1 & ], 
         !((StringTake[#1[[1]], 1] == "C" && StringTake[#1[[2]], 1] == 
             "C") || (StringTake[#1[[1]], 1] != "C" && 
            StringTake[#1[[2]], 1] != "C")) & ]; res = {}; 
      Do[idx1 = If[StringTake[i[[1]], 1] == "C", StringTake[i[[1]], {2, -1}], 
          i[[1]]]; idx2 = If[StringTake[i[[2]], 1] == "C", 
          StringTake[i[[2]], {2, -1}], i[[2]]]; 
        If[StringTake[i[[2]], 1] != "C", {i1, i2} = {idx1, idx2}, 
         {i1, i2} = {idx2, idx1}]; If[coincideNorm[evaluate[pt] /. rc, 
          func[KimberlingCenterCNy[i1] /. rc, KimberlingCenterCNy[i2] /. 
             rc] /. rc], AppendTo[res, {i1, i2}]; ], {i, tuples}]; 
      res = SortBy[DeleteDuplicates[res], numsortexpr[#1[[1]]] & ]; 
      res = ({intnameformat[#1[[1]]], intnameformat[#1[[2]]]} & ) /@ res; 
      SetAttributes[fncname, HoldFirst]; fncname[x_] := 
       {SymbolName[Unevaluated[x]], x}; AssociateTo[globalProperties[name], 
       {ToString[First[fncname[func]]] -> res}]; If[ !TrueQ[globalSilence], 
       If[Length[res] > 0, Print[colorformat[StringJoin[str, 
            StringRiffle[res, ", "]]]]; ]]; ]
 
checkCircles[pt_, name_:"X", prec_:24] := 
    Module[{rc, rc2, d1, d2, d3, d4, d5, d6, etcset, distlist, ptn, hg, 
      outgroups, fullgroups, as, out}, fullgroups = {}; outgroups = {}; 
      etcset = Select[ETCBaryNorm, Total[#1] != 0 & ]; 
      ptn = intnumericnorm[evaluate[pt] /. rule69]; 
      distlist = Table[{ptname, N[bDistance[ptn, etcset[ptname]] /. rule69, 
          prec]}, {ptname, Keys[etcset]}]; distlist = 
       Select[distlist, Internal`RealValuedNumericQ[#1[[2]]] & ]; 
      distlist = SortBy[distlist, #1[[2]] & ]; 
      as = AssociationThread[(#1[[1]] & ) /@ distlist, 
        (#1[[2]] & ) /@ distlist]; fullgroups = 
       Select[Values[PositionIndex[as]], Length[#1] > 1 & ]; 
      outgroups = Select[fullgroups, Length[#1] > 2 & ]; 
      outgroups = (Take[SortBy[#1, numsortexpr[#1] & ], 3] & ) /@ outgroups; 
      outgroups = SortBy[outgroups, numsortexpr[#1[[1]]] & ]; hg = {}; 
      Do[rc = intCheckList[[1]]; rc2 = intCheckList[[2]]; 
        ptn = intnumericnorm[evaluate[pt] /. rc]; 
        d1 = N[bDistance[ptn, intnumericnorm[KimberlingCenterCNy[circ[[
                1]]] /. rc]] /. rc, prec]; 
        d2 = N[bDistance[ptn, intnumericnorm[KimberlingCenterCNy[circ[[
                2]]] /. rc]] /. rc, prec]; 
        d3 = N[bDistance[ptn, intnumericnorm[KimberlingCenterCNy[circ[[
                3]]] /. rc]] /. rc, prec]; ptn = intnumericnorm[
          evaluate[pt] /. rc2]; 
        d4 = N[bDistance[ptn, intnumericnorm[KimberlingCenterCNy[circ[[
                1]]] /. rc2]] /. rc2, prec]; 
        d5 = N[bDistance[ptn, intnumericnorm[KimberlingCenterCNy[circ[[
                2]]] /. rc2]] /. rc2, prec]; 
        d6 = N[bDistance[ptn, intnumericnorm[KimberlingCenterCNy[circ[[
                3]]] /. rc2]] /. rc2, prec]; 
        If[AllTrue[{d1, d2}, #1 == d3 & ] && AllTrue[{d4, d5}, #1 == d6 & ], 
         AppendTo[hg, circ]]; , {circ, outgroups}]; 
      hg = ({intnameformat[#1[[1]]], intnameformat[#1[[2]]], 
          intnameformat[#1[[3]]]} & ) /@ hg; If[ !TrueQ[globalSilence], 
       If[Length[hg] > 0, Print[colorformat[StringJoin[
            "= center of circles {{ X(i), X(j), X(k) }} for these {i, j, k}: \
", StringRiffle[hg, ", "]]]]; ]]; AssociateTo[globalProperties[name], 
       {"circles" -> hg}]; Return[hg]; ]
 
checkPoles[pt_, name_:"X"] := Module[{out, prop, plr, set, fltset, outci, 
      outin, conics}, out = {}; outci = {}; outin = {}; 
      conics = Join[fltCentralCircles, fltDuals]; If[ !globalCheckAllPoles, 
       Do[mon = circ; plr = TimeConstrained[bPolar[conics[circ], pt], 5, -1]; 
          If[plr == -1, Continue[]]; set = checkPointsOnCurve[
            evaluate[plr] . {x, y, z}]; If[Length[set] >= 2, 
           fltset = (intnameformat2[#1] & ) /@ Take[set, 2]; 
            prop = StringJoin["= pole of line ", ToString[fltset], 
              " with respect to the ", circ]; AppendTo[out, prop]; 
            AssociateTo[globalProperties[name], {"poles" -> out}]; 
            If[ !TrueQ[globalSilence], Print[colorformat[prop]]]; ]; , 
         {circ, Keys[conics]}]; ]; If[globalCheckAllPoles, 
       Do[plr = TimeConstrained[bPolar[fltCircumCircles[circ], pt], 5, -1]; 
          If[plr == -1, Continue[]]; set = checkPointsOnCurve[
            plr . {x, y, z}]; If[Length[set] >= 2, 
           fltset = (intnameformat2[#1] & ) /@ Take[set, 2]; 
            AppendTo[outci, Join[fltset, {ToString[intnameformat2[
                 circ]]}]]; ]; , {circ, Keys[fltCircumCircles]}]; 
        If[ !TrueQ[globalSilence], If[Length[outci] > 0, 
          Print[colorformat[StringJoin["= pole of line X(i)X(j) wrt the \
circumconic with perspector X(k) for these {i,j,k}: ", StringRiffle[outci, 
               ", "]]]]; ]]; Do[plr = TimeConstrained[
            bPolar[fltInCircles[circ], pt], 5, -1]; If[plr == -1, 
           Continue[]]; set = checkPointsOnCurve[plr . {x, y, z}]; 
          If[Length[set] >= 2, fltset = (intnameformat2[#1] & ) /@ 
              Take[set, 2]; AppendTo[outin, Join[fltset, {ToString[
                intnameformat2[circ]]}]]; ]; , {circ, Keys[fltInCircles]}]; 
        If[ !TrueQ[globalSilence], If[Length[outin] > 0, 
          Print[colorformat[StringJoin["= pole of line X(i)X(j) wrt the \
inconic with perspector X(k) for these {i,j,k}: ", StringRiffle[outin, 
               ", "]]]]; ]]; ]; AssociateTo[globalProperties[name], 
       {"poles" -> out}]; AssociateTo[globalProperties[name], 
       {"polesin" -> outin}]; AssociateTo[globalProperties[name], 
       {"polesci" -> outci}]; Return[{out, outci, outin}]; ]
 
globalCheckAllPoles = False
 
intnameformat2[pname_] := If[StringTake[pname, 1] == "X", 
     ToExpression[StringTake[pname, {2, -1}]], pname]
 
checkPerspector[pt_, inname_:"X"] := Module[{out, ptcheck, crv, set1, rc, 
      ptnamenew, ptest}, crv = bCircumconicPEq[pt]; 
      set1 = checkPointsOnCurve[crv]; out = SortBy[set1, numsortexpr[#1] & ]; 
      If[Length[out] >= 2, out = Take[out, 2]; If[ !TrueQ[globalSilence], 
         Print[colorformat[StringJoin[
            "= perspector of circumconic {{A, B, C, ", StringRiffle[out, 
             ", "], "}}"]]]]; ]; AssociateTo[globalProperties[inname], 
       {"perspector" -> out}]; ]
 
checkConicCenter[pt_, inname_:"X"] := Module[{out, ptest, ptcheck, crv, set1, 
      rc, ptnamenew}, crv = bCircumconicPEq[pt*bAntiComplement[X[2], pt]]; 
      set1 = checkPointsOnCurve[crv]; out = SortBy[set1, numsortexpr[#1] & ]; 
      If[Length[out] >= 2, out = Take[out, 2]; If[ !TrueQ[globalSilence], 
         Print[colorformat[StringJoin["= center of circumconic {{A, B, C, ", 
            StringRiffle[out, ", "], "}}"]]]]; ]; 
      AssociateTo[globalProperties[inname], {"conic center" -> out}]; ]
 
pointCheckerTransform[expr_, inname_, num_:0, full_:False] := 
    Module[{pointProcesses, texpr, procname}, 
     If[ !TrueQ[globalSilence], Print[inname]]; pointChecker[expr, num, full, 
       inname]; If[TrueQ[globalSilence], printGlobalProperties[
        globalProperties, inname]]; pointProcesses = 
       Association["isotomic conjugate" -> Hold[bIsotomicConjugate[#1]], 
        "isogonal conjugate" -> Hold[bIsogonalConjugate[#1]], 
        "complement" -> Hold[bComplement[KimberlingCenterC[2], #1]], 
        "anticomplement" -> Hold[bAntiComplement[KimberlingCenterC[2], #1]], 
        "cyclocevian conjugate" -> Hold[bCyclocevianConjugate[#1]], 
        "circumcircle inverse" -> Hold[bCircumcircleInverse[#1]], 
        "zosma transform" -> Hold[bZosmaTransform[#1]], 
        "polar conjugate" -> Hold[bPIsogonalConjugate[KimberlingCenterCN[48], 
           #1]], "eigentransform" -> Hold[bEigentransform[#1]], 
        "cundyParryPsi" -> Hold[cundyParryPsi[#1]], "cundyParryPhi" -> 
         Hold[cundyParryPhi[#1]]]; 
      Do[texpr = simplifyRationalBarycentrics[
          Factor[Together[evaluate[ReleaseHold[pointProcesses[name] /. #1 -> 
                expr]]]]]; If[heuristicsCheck[texpr[[1]]], 
         procname = StringJoin[name, " of ", inname]; 
          If[ !TrueQ[globalSilence], Print[procname]]; pointChecker[texpr, 0, 
           full, procname]; If[TrueQ[globalSilence], printGlobalProperties[
            globalProperties, procname]]; ], {name, Keys[pointProcesses]}]; ]
 
printGlobalProperties[glob_, name_:"", printname_:""] := 
    Module[{hg, cycle, localprops, colorprint}, 
     If[StringLength[name] > 0, cycle = {name}, cycle = Keys[glob]]; 
      colorprint = colorPrintOn; colorPrintOn = False; 
      Do[print[StringJoin[printname, " = ", glob[pt]["name"]]]; print[]; 
        print[StringJoin["Barycentrics    ", glob[pt]["barycentrics"]]]; 
        print[]; print[]; If[MemberQ[Keys[glob[pt]], "descr"], 
         print[StringJoin["DESCR: ", StringReplace[glob[pt]["descr"], 
             "This point" -> printname]]]; print[]; ]; 
        hg = If[ !MemberQ[Keys[glob[pt]], "linear combinations"], {}, 
          glob[pt]["linear combinations"]]; If[Length[hg] > 0, 
         print[colorformat[StringJoin[printname, " linear combinations: ", 
             StringRiffle[hg, ", "]]]]; print[]; ]; 
        print[colorformat[StringJoin[printname, " lies on these lines: ", 
           StringRiffle[glob[pt]["lines"], ", "]]]]; print[]; 
        If[MemberQ[Keys[glob[pt]], "midpoints"], hg = glob[pt]["midpoints"]; 
          If[Length[hg] > 0, print[colorformat[StringJoin[printname, 
               " = midpoint of X(i) and X(j) for these {i,j}: ", StringRiffle[
                hg, ", "]]]]; ]; ]; If[MemberQ[Keys[glob[pt]], 
          "reflections"], hg = glob[pt]["reflections"]; If[Length[hg] > 0, 
           print[colorformat[StringJoin[printname, 
               " = reflection of X(i) in X(j) for these {i,j}: ", 
               StringRiffle[hg, ", "]]]]; ]; ]; 
        If[ !MemberQ[Keys[glob[pt]], "circumconics"], 
         If[MemberQ[Keys[glob[pt]], "harmonic"], hg = glob[pt]["harmonic"]; 
            If[Length[hg] > 0, print[colorformat[StringJoin[printname, 
                 " = {X(i),X(j)}-harmonic conjugate of X(k) for these \
(i,j,k): ", StringRiffle[SortBy[hg, numsortexpr[#1[[1]]] & ], ", "]]]]; ]; ]; 
          Continue[]; ]; hg = glob[pt]["curves"]; If[Length[hg] > 0, 
         print[colorformat[StringJoin[printname, " lies on these curves: ", 
             StringRiffle[hg, ", "]]]]; ]; hg = glob[pt]["inverses"]; 
        If[Length[hg] > 0, (print[colorformat[StringJoin[printname, " = ", 
               #1]]] & ) /@ hg; ]; Do[If[KeyExistsQ[glob[pt], proc], 
           print[colorformat[StringJoin[printname, " = ", proc, " of ", 
               glob[pt][proc]]]]; ]; , {proc, Keys[singlePointProcesses]}]; 
        If[KeyExistsQ[glob[pt], "trilinear polar"], 
         hg = glob[pt]["trilinear polar"]; If[ListQ[hg] && Length[hg] > 0, 
           print[colorformat[StringJoin[printname, 
               " = trilinear pole of line {", intnameformat[hg[[1]]], ", ", 
               intnameformat[hg[[2]]], "}"]]]; ]; ]; 
        hg = glob[pt]["perspector"]; If[ListQ[hg] && Length[hg] >= 2, 
         print[colorformat[StringJoin[printname, StringJoin[
              " = perspector of circumconic {{A, B, C, ", StringRiffle[hg, 
               ", "], "}}"]]]]; ]; hg = glob[pt]["conic center"]; 
        If[ListQ[hg] && Length[hg] >= 2, 
         print[colorformat[StringJoin[printname, StringJoin[
              " = center of circumconic {{A, B, C, ", StringRiffle[hg, ", "], 
              "}}"]]]]; ]; localprops = Association["isoconjugate" -> 
           "= X(i)-isoconjugate-of-X(j) for these {i, j}: ", 
          "vertex conjugate" -> 
           "= X(i)-vertex conjugate of X(j) for these {i, j}: ", 
          "bDaoConjugate" -> 
           "= X(i)-Dao conjugate of X(j) for these {i, j}: ", 
          "bCevianQuotient" -> 
           "= X(i)-Ceva conjugate of X(j) for these {i, j}: ", 
          "bComplementaryConjugate" -> 
           "= X(i)-complementary conjugate of X(j) for these {i, j}: ", 
          "bAnticomplementaryConjugate" -> 
           "= X(i)-anticomplementary conjugate of X(j) for these {i, j}: ", 
          "bCrossConjugate" -> 
           "= X(i)-cross conjugate of X(j) for these {i, j}: ", 
          "poles" -> " = ", "others" -> " = ", "circles" -> " = ", 
          "inconics" -> "= lies on inconics with perspector: ", 
          "circumconics" -> 
           "= intersection, other than A, B, C, of circumconics ", 
          "barycentric product" -> 
           "= barycentric product X(i)*X(j) for these (i, j): ", 
          "barycentric quotient" -> 
           "= barycentric quotient X(i)/X(j) for these (i, j): ", 
          "harmonic" -> 
           "= {X(i),X(j)}-harmonic conjugate of X(k) for these (i,j,k): "]; 
        Do[hg = glob[pt][name2]; If[ListQ[hg] && Length[hg] > 0 && 
            name2 != "others" && name2 != "poles" && name2 != "circles", 
           print[colorformat[StringJoin[printname, " ", localprops[name2], 
               StringRiffle[hg, ", "]]]]; ]; If[name2 == "poles", 
           If[ListQ[hg] && Length[hg] > 0, Do[print[colorformat[StringJoin[
                  printname, " ", prop]]], {prop, hg}]; ]; 
            hg = glob[pt]["polesci"]; If[ListQ[hg] && Length[hg] > 0, 
             print[colorformat[StringJoin[printname, " = pole of line \
X(i)X(j) wrt the circumconic with perspector X(k) for these {i,j,k}: ", 
                 StringRiffle[hg, ", "]]]]; ]; hg = glob[pt]["polesin"]; 
            If[ListQ[hg] && Length[hg] > 0, print[colorformat[StringJoin[
                 printname, " = pole of line X(i)X(j) wrt the inconic with \
perspector X(k) for these {i,j,k}: ", StringRiffle[hg, ", "]]]]; ]; ]; 
          If[ListQ[hg] && Length[hg] > 0 && name2 == "circles", 
           print[colorformat[StringJoin[printname, " = center of circles {{ \
X(i), X(j), X(k) }} for these {i, j, k}: ", StringRiffle[hg, ", "]]]]; ]; 
          If[ListQ[hg] && Length[hg] > 0 && name2 == "others", 
           Do[print[colorformat[StringJoin[printname, localprops[name2], 
               prop]]], {prop, hg}]]; , {name2, Keys[localprops]}]; , 
       {pt, cycle}]; colorPrintOn = colorprint; ]
 
print[string_] := If[StringContainsQ[string, "KeyAbsent"], 
     Return["", Module], 
     If[ !MemberQ[Names["Global`*"], "globalOutputStream"] || 
        globalOutputStream === False, Print[string], 
       WriteString[globalOutputStream, StringJoin[string, "\n\n"]]]; ]
 
globalOutputStream = False
 
addExtraPoint[bary_, letter_, name_, writeout_:True] := 
    Module[{expr, pt, idxmax, dbname}, If[ !MemberQ[{"Y", "Z"}, letter], 
       Print["Invalid letter"]; Return[False, Module]]; 
      If[name != "unnamed" && MemberQ[Values[NonETCNames], name], 
       Print["Name exists !"]; Return[False, Module]]; 
      If[VectorQ[bary], expr = bary[[1]], expr = bary]; 
      idxmax = Max[1 + ToExpression[StringTake[
           SortBy[Keys[KeySelect[ETCExtra, StringStartsQ[#1, letter] & ]], 
            numsortexpr[#1] & ], {2, -1}]]]; 
      dbname = StringJoin[letter, ToString[idxmax]]; 
      pt = intnumericnorm[evaluate[symmetrizeInternal2[expr]] /. rule69]; 
      AppendTo[ETCBaryNorm, dbname -> pt]; AppendTo[ETCExtraBary, 
       dbname -> pt]; AppendTo[ETCBaryNormFull, dbname -> pt]; 
      AppendTo[ETC, dbname -> expr]; AppendTo[ETCFull, dbname -> expr]; 
      AppendTo[ETCExtra, dbname -> expr]; AppendTo[NonETCNames, 
       dbname -> name]; If[writeout, DumpSave["ETCExtra.mx", ETCExtra]; 
        DumpSave["ETCExtraBary.mx", ETCExtraBary]; DumpSave["NonETCNames.mx", 
         NonETCNames]; ]; Return[dbname]; ]
 
quickChecker[expr_, num_:0, curvescheck_:True, dosymcheck_:True, 
     minprop_:1000] := Module[{ptcoord, pt, chk, lines, barys, symcheck, 
      name, numcon}, lines = 0; numcon = 0; ptcoord = evaluate[expr]; 
      If[dosymcheck, If[ !checkCentralExpression[expr], 
         Return[False, Module]; ]; ]; If[num != 0, chk = 0, 
       chk = checkPointinETC2[ptcoord]]; If[Length[chk] > 0, 
       Print[colorformat[StringJoin["ETC: ", chk]]], 
       barys = Factor[FactorTermsList[expr[[1]]][[2]]]; 
        lines = Length[Quiet[linesProcessAlg[ptcoord, barys, 20, False, True, 
            "X", True]]]; If[curvescheck && lines < minprop, 
         numcon = Length[Quiet[checkCircumconics[ptcoord, num, name]]]]; ]; 
      Return[{lines, numcon}]; ]
