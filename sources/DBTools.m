pointProcessBary[expr_, rule_:rule69] := 
    Module[{pointsBary, pti, pta, str, coords}, pointsBary = Association[]; 
      Do[pta = ETCBaryNorm[k]; If[AnyTrue[Im[pta], #1 > 0 & ], Continue[]]; 
        If[AnyTrue[pta, #1 === ComplexInfinity & ] || AnyTrue[pta, 
           #1 === Infinity & ], Continue[]]; 
        pti = N[ReleaseHold[expr /. #1 -> pta] /. rule, 36]; 
        pti = pti*Sign[pti[[1]]]; If[Total[pti] != 0, pti = Normalize[pti], 
         If[pti[[1]] != 0, pti = pti/pti[[1]]]]; 
        If[pti[[1]] =!= Indeterminate, AppendTo[pointsBary, 
           ToString[k] -> pti]; ], {k, Keys[ETCBaryNorm]}]; 
      Return[pointsBary]; ]
 
singlePointProcesses = <|"complement" -> 
      Hold[bComplement[KimberlingCenterC[2], #1]], 
     "anticomplement" -> Hold[bAntiComplement[KimberlingCenterC[2], #1]], 
     "polar conjugate" -> Hold[bPIsogonalConjugate[KimberlingCenterCN[48], 
        #1]], "cyclocevian conjugate" -> Hold[bCyclocevianConjugate[#1]], 
     "circumcircle inverse" -> Hold[bCircumcircleInverse[#1]], 
     "anticomplement of isogonal conjugate" -> 
      Hold[bAntiComplement[KimberlingCenterC[2], bIsogonalConjugate[#1]]], 
     "anticomplement of isotomic conjugate" -> 
      Hold[bAntiComplement[KimberlingCenterC[2], bIsotomicConjugate[#1]]], 
     "complement of isogonal conjugate" -> 
      Hold[bComplement[KimberlingCenterC[2], bIsogonalConjugate[#1]]], 
     "complement of isotomic conjugate" -> 
      Hold[bComplement[KimberlingCenterC[2], bIsotomicConjugate[#1]]], 
     "zosma transform" -> Hold[bZosmaTransform[#1]], 
     "circlecevian perspector" -> Hold[bCirclecevianPerspector[#1]], 
     "tcc_perspector" -> Hold[bTCCPerspector[#1]], 
     "eigentransform" -> Hold[bEigentransform[#1]], 
     "ortoassociate" -> Hold[bOrthoassociate[#1]], "antitomic conjugate" -> 
      Hold[bAntitomicConjugate[#1]], "syngonal conjugate" -> 
      Hold[bSyngonal[#1]], "1st saragossa point" -> Hold[bSaragossa1[#1]], 
     "2nd saragossa point" -> Hold[bSaragossa2[#1]], 
     "3rd saragossa point" -> Hold[bSaragossa3[#1]]|>
 
intHarmonicProcess[fullgroups_, pt_, prec_] := 
    Module[{fgr1, checks, flatfg2, ingroupnbary, un, hgroups, hgroup, prev, 
      dump}, hgroups = {}; 
      Do[fgr1 = SortBy[set, ToExpression[StringTake[#1, {2, -1}]] & ]; 
        If[Length[fgr1] > 2000, If[ !TrueQ[globalSilence], 
           Print[StringJoin["Found lines with ", ToString[Length[fgr1]], 
             " points"]]]; fgr1 = Take[fgr1, 2000]; ]; 
        flatfg2 = Subsets[fgr1, {2}]; checks = AssociationMap[
          NormalizeBary[bHarmonicConjugate[ETCBaryNorm[#1[[1]]], 
             ETCBaryNorm[#1[[2]]], pt]] & , flatfg2]; 
        ingroupnbary = KeySelect[ETCBaryNorm, MemberQ[fgr1, #1] & ]; 
        un = SortBy[Union[checks, ingroupnbary], #1[[1]] & ]; hgroup = {}; 
        prev = Association["X0" -> {0, 0, 0}]; dump = 0; 
        Do[If[Total[Abs[un[el] - prev[[1]]]] < 10^(-15), 
           AppendTo[hgroup, el]; If[Length[hgroup] == 1, 
             AppendTo[hgroup, Keys[prev][[1]]]; dump = 1], 
           If[dump == 1, If[AnyTrue[Length /@ hgroup, #1 == 0 & ], AppendTo[
                hgroups, hgroup]]; hgroup = {}; dump = 0; ]; ]; 
          prev = Association[el -> un[el]]; , {el, Keys[un]}], 
       {set, fullgroups}]; Return[hgroups]; ]
 
intPointCheck[pt_, process_, rule_:rule69] := Module[{tmp, res, ptn, ptnum}, 
     ptn = N[NormalizeBary[pt /. rule], 35]; 
      tmp = pointProcessBary[singlePointProcesses[process], rule]; 
      res = Select[(coincide[#1, ptn] & ) /@ tmp, TrueQ]; 
      If[Length[res] > 0, ptnum = StringTake[Keys[res][[1]], {2, -1}]; 
        intVerifyPointProcess[pt, ToExpression[ptnum], singlePointProcesses[
          process]]; Return[ptnum]; ]; ]
 
coincide[pt1_, pt2_, prec_:20] := Abs[pt1[[1]] - pt2[[1]]] < 10^(-prec) && 
     Abs[pt1[[2]] - pt2[[2]]] < 10^(-prec) && Abs[pt1[[3]] - pt2[[3]]] < 
      10^(-prec)
 
intVerifyPointProcess[pt_, xnum_, processexpr_] := 
    Module[{ptn, pta, pti}, Do[ptn = N[NormalizeBary[pt /. rc], 35]; 
        pta = N[NormalizeBary[KimberlingCenterCN[xnum] /. rc], 35]; 
        pti = N[NormalizeBary[ReleaseHold[singlePointProcesses[
               "eigentransform"] /. #1 -> pta] /. rc], 35]; 
        If[ !coincide[ptn, pti], Return[False, Module]; ]; , 
       {rc, intCheckList}]; Return[True]; ]
 
xnum[str_] := ToExpression[StringTake[str, {2, -1}]]
 
intCheckList = {{a -> 5, b -> 6, c -> 7}, {a -> 4, b -> 11, c -> 13}}
 
pointCheckAllProcesses[pt_, name_:"X"] := Module[{res, prop}, 
     Do[res = intPointCheck[pt, proc]; If[StringQ[res], 
         prop = StringJoin["= ", proc, " of X(", res, ")"]; 
          If[ !TrueQ[globalSilence], Print[prop]]; AssociateTo[
           globalProperties[name], proc -> StringJoin["X(", res, ")"]]; ]; , 
       {proc, Keys[singlePointProcesses]}]; ]
 
globalProperties = <||>
 
checkCircumconics[pt_, start_:1, time_:60, excl_:0, name_:"X"] := 
    Module[{ptc, p1, p2, crv, dset, test, out, conicname, check}, 
     TimeConstrained[out = {}; ptc = N[NormalizeBary[pt /. rule69], 35]; 
        Do[funcind = nx; If[ !PolynomialQ[evaluate[ETC[StringJoin["X", 
                ToString[nx]]]], {a, b, c}], Continue[]]; 
          crv = N[bCircumconicEq[ptc, ETCBaryNorm[StringJoin["X", 
                ToString[nx]]]] /. rule69, 35]; 
          dset = (Abs[crv] /. Thread[{x, y, z} -> #1] & ) /@ ETCBaryNorm; 
          test = Select[dset, #1 < 10^(-10) & ]; If[Length[test] > 1, 
           p1 = ToExpression[StringTake[Keys[test][[1]], {2, -1}]]; 
            p2 = ToExpression[StringTake[Keys[test][[2]], {2, -1}]]; 
            If[p1 == excl || p2 == excl, Continue[]]; 
            check = Simplify[bCircumconicEq[KimberlingCenterCN[p1], 
                KimberlingCenterCN[p2]] /. Thread[{x, y, z} -> pt]]; 
            If[check == 0, conicname = StringJoin["{A,B,C,X(", ToString[p1], 
                "),X(", ToString[p2], ")}"]; If[ !MemberQ[out, conicname], 
               AppendTo[out, conicname]]]; ]; , {nx, start, 
          start + Max[1, Floor[time/60]]*200}]; , time, funcind]; 
      AssociateTo[globalProperties[name], {"circumconics" -> out}]; 
      If[ !TrueQ[globalSilence], If[Length[out] > 0, 
        Print[StringJoin["Lies on circumconics: ", StringRiffle[out, 
            ", "]]]; ]]; Return[funcind]; ]
 
ffcrossconjugate[var_, ptx_] := Module[{local}, 
     local = bCrossConjugate[ptx, var]; Return[NormalizeBary[local]]; ]
 
ffisoconjugate[pt1_, pt2_] := Module[{local}, 
     local = bPIsogonalConjugate[pt1 /. rule69, pt2 /. rule69] /. rule69; 
      Return[NormalizeBary[local]]; ]
 
linesProcessAlg[ptcoord_, printexpr_, prec_, debug_, abort_, name_] := 
    Module[{res, gr, hg, out, head, test, test2, hgroups, ptc, unproven, rc, 
      rc2, eltest, sout, barys}, rc = intCheckList[[1]]; 
      rc2 = intCheckList[[2]]; ptc = N[NormalizeBary[evaluate[ptcoord] /. 
          rule69], 35]; res = intLinesProcessFullGroups[ptc, prec]; 
      gr = (StringJoin["{", StringTake[#1[[1]][[1]], {2, -1}], ",", 
          StringTake[#1[[2]][[1]], {2, -1}], "}"] & ) /@ res[[1]]; out = {}; 
      unproven = {}; Do[If[debug, PrintTemporary[el]]; 
        test = TimeConstrained[bCollinearityMatrix[
           KimberlingCenterCN[ToExpression[el][[1]]] /. rc, 
           KimberlingCenterCN[ToExpression[el][[2]]] /. rc, ptcoord /. rc], 
          10, -1]; test2 = TimeConstrained[bCollinearityMatrix[
           KimberlingCenterCN[ToExpression[el][[1]]] /. rc2, 
           KimberlingCenterCN[ToExpression[el][[2]]] /. rc2, ptcoord /. rc2], 
          10, -1]; If[TrueQ[Simplify[test] == 0] && 
          TrueQ[Simplify[test2] == 0], AppendTo[out, el], 
         AppendTo[unproven, el]]; , {el, gr}]; If[Length[out] >= 2, 
       sout = SortBy[(ToExpression[#1] & ) /@ out, #1[[1]]*#1[[2]] & ]; 
        AssociateTo[globalProperties[name], 
         {"name" -> ToString[StringJoin["X(", ToString[Evaluate[sout[[1]][[
                1]]]], ")X(", ToString[Evaluate[sout[[1]][[2]]]], 
             ")\:2229X(", ToString[Evaluate[sout[[2]][[1]]]], ")X(", 
             ToString[Evaluate[sout[[2]][[2]]]], ")"]]}]; 
        If[ !TrueQ[globalSilence], Print[ToString[StringJoin["X(", 
            ToString[Evaluate[sout[[1]][[1]]]], ")X(", 
            ToString[Evaluate[sout[[1]][[2]]]], ")\:2229X(", 
            ToString[Evaluate[sout[[2]][[1]]]], ")X(", 
            ToString[Evaluate[sout[[2]][[2]]]], ")"]]]]; ]; 
      barys = ExpressionToTrad[Simplify[printexpr]]; 
      AssociateTo[globalProperties[name], {"barycentrics" -> barys}]; 
      If[ !TrueQ[globalSilence], Print[StringJoin["Barycentrics    ", 
         barys]]]; AssociateTo[globalProperties[name], {"lines" -> out}]; 
      If[ !TrueQ[globalSilence], Print[StringJoin["Lies on these lines: ", 
         StringRiffle[out, ", "]]]]; If[abort && Length[out] < 3, 
       Return[out, Module]; ]; hg = {}; 
      Do[head = Select[igroup, Length[#1] == 0 & ]; 
        Do[If[el == head[[1]], Continue[]]; eltest = 
           (StringTake[#1, {2, -1}] & ) /@ Flatten[{el, head[[1]]}]; 
          test = Det[{{1, 1, 1}, bHarmonicConjugate[KimberlingCenterCN[
                eltest[[1]]] /. rc, KimberlingCenterCN[eltest[[2]]] /. rc, 
              KimberlingCenterCN[eltest[[3]]] /. rc], ptcoord /. rc}]; 
          test2 = Det[{{1, 1, 1}, bHarmonicConjugate[KimberlingCenterCN[
                eltest[[1]]] /. rc2, KimberlingCenterCN[eltest[[2]]] /. rc2, 
              KimberlingCenterCN[eltest[[3]]] /. rc2], ptcoord /. rc2}]; 
          If[TrueQ[Simplify[test] == 0] && TrueQ[Simplify[test2] == 0], 
           AppendTo[hg, eltest]; ]; , {el, igroup}], 
       {igroup, intHarmonicProcess[res[[2]], ptc, prec]}]; 
      AssociateTo[globalProperties[name], {"harmonic" -> hg}]; 
      If[ !TrueQ[globalSilence], If[Length[hg] > 0, 
        Print[StringJoin[
           "= {X(i),X(j)}-harmonic conjugate of X(k) for these (i,j,k): ", 
           StringRiffle[SortBy[hg, ToExpression[#1[[1]]] & ], ", "]]]; ]]; 
      hg = {}; Do[eltest = (StringTake[#1, {2, -1}] & ) /@ igroup; 
        test = Det[{{1, 1, 1}, bMidpoint[KimberlingCenterCN[eltest[[1]]] /. 
             rc, KimberlingCenterCN[eltest[[2]]] /. rc], ptcoord /. rc}]; 
        test2 = Det[{{1, 1, 1}, bMidpoint[KimberlingCenterCN[eltest[[1]]] /. 
             rc2, KimberlingCenterCN[eltest[[2]]] /. rc2], ptcoord /. rc2}]; 
        If[TrueQ[Simplify[test] == 0] && TrueQ[Simplify[test2] == 0], 
         AppendTo[hg, eltest]; ]; , {igroup, intMidpointProcess[res[[2]], 
         ptc, prec]}]; AssociateTo[globalProperties[name], 
       {"midpoints" -> hg}]; If[ !TrueQ[globalSilence], 
       If[Length[hg] > 0, Print[StringJoin[
           "= midpoint of X(i) in X(j) for these {i,j}: ", 
           StringRiffle[SortBy[hg, ToExpression[#1[[1]]] & ], ", "]]]; ]]; 
      hg = {}; Do[eltest = (StringTake[#1, {2, -1}] & ) /@ igroup; 
        test = Det[{{1, 1, 1}, bReflectionPP[KimberlingCenterCN[
              eltest[[1]]] /. rc, KimberlingCenterCN[eltest[[2]]] /. rc], 
           ptcoord /. rc}]; test2 = Det[{{1, 1, 1}, bReflectionPP[
            KimberlingCenterCN[eltest[[1]]] /. rc2, 
            KimberlingCenterCN[eltest[[2]]] /. rc2], ptcoord /. rc2}]; 
        If[TrueQ[Simplify[test] == 0] && TrueQ[Simplify[test2] == 0], 
         AppendTo[hg, eltest]; ]; , {igroup, intReflectionProcess[res[[2]], 
         ptc, prec]}]; AssociateTo[globalProperties[name], 
       {"reflections" -> hg}]; If[ !TrueQ[globalSilence], 
       If[Length[hg] > 0, Print[StringJoin[
           "= reflection of X(i) in X(j) for these {i,j}: ", 
           StringRiffle[SortBy[hg, ToExpression[#1[[1]]] & ], ", "]]]; ]]; 
      Return[out]; ]
 
intLinesProcessFullGroups[pt_, prec_] := 
    Module[{tplist, tp, prev, outgroups, group, fullgroups, dump}, 
     tplist = {}; fullgroups = {}; Do[tp = 1/bLine[pt, ETCBaryNorm[name]]; 
        If[Im[tp[[1]]] != 0 || AnyTrue[ !NumberQ[tp]], Continue[]]; 
        AppendTo[tplist, {name, Normalize[tp]*Sign[tp[[1]]]}]; , 
       {name, Keys[ETCBaryNorm]}]; tplist = SortBy[tplist, #1[[2]][[1]] & ]; 
      prev = {"0", {0, 0, 0}}; outgroups = {}; group = {}; 
      Do[If[Abs[el[[2]][[1]] - prev[[2]][[1]]] < 10^(-prec) && 
          Abs[el[[2]][[2]] - prev[[2]][[2]]] < 10^(-prec) && 
          Abs[el[[2]][[3]] - prev[[2]][[3]]] < 10^(-prec), 
         AppendTo[group, el]; If[Length[group] == 1, AppendTo[group, prev]; 
            dump = 1], If[dump == 1, AppendTo[fullgroups, group]; 
            AppendTo[outgroups, Take[SortBy[group, ToExpression[StringTake[
                  #1[[1]], {2, -1}]] & ], 2]]; group = {}; dump = 0]; ]; 
        prev = el; , {el, tplist}]; outgroups = SortBy[outgroups, 
        ToExpression[StringTake[#1[[1]][[1]], {2, -1}]] & ]; 
      fullgroups = ((#1[[1]] & ) /@ #1 & ) /@ fullgroups; 
      Return[{outgroups, fullgroups}]; ]
 
intMidpointProcess[fullgroups_, pt_, prec_] := 
    Module[{fgr1, checks, flatfg2, ingroupnbary, un, hgroups, hgroup, prev, 
      dump}, hgroups = {}; 
      Do[fgr1 = SortBy[set, ToExpression[StringTake[#1, {2, -1}]] & ]; 
        If[Length[fgr1] > 2000, fgr1 = Take[fgr1, 2000]; ]; 
        flatfg2 = Subsets[fgr1, {2}]; ingroupnbary = AssociationMap[
          Abs[NormalizeBary[bMidpoint[ETCBaryNorm[#1[[1]]], ETCBaryNorm[
                #1[[2]]]]] - pt] & , flatfg2]; 
        hgroup = Select[ingroupnbary, #1[[1]] < 10^(-prec) && 
            #1[[2]] < 10^(-prec) && #1[[3]] < 10^(-prec) & ]; 
        If[Length[hgroup] > 0, hgroups = Union[hgroups, Keys[hgroup]]]; , 
       {set, fullgroups}]; Return[hgroups]; ]
 
intReflectionProcess[fullgroups_, pt_, prec_] := 
    Module[{fgr1, checks, flatfg2, ingroupnbary, un, hgroups, hgroup, prev, 
      dump}, hgroups = {}; Do[fgr1 = SortBy[set, xnum[#1] & ]; 
        If[Length[fgr1] > 2000, fgr1 = Take[fgr1, 2000]; ]; 
        checks = AssociationMap[NormalizeBary[bMidpoint[ETCBaryNorm[#1], 
             pt]] & , fgr1]; 
        Do[refl = Select[fgr1, coincide[ETCBaryNorm[#1], checks[el]] & ]; 
          If[Length[refl] > 0, AppendTo[hgroups, {el, First[refl]}]], 
         {el, Keys[checks]}]; , {set, fullgroups}]; Return[hgroups]; ]
 
ffbarycentricproduct[pt1_, pt2_] := Module[{local}, 
     local = (pt1 /. rule69)*(pt2 /. rule69); Return[NormalizeBary[local]]; ]
 
ffbarycentricquotient[pt1_, pt2_] := Module[{local}, 
     local = (pt1 /. rule69)/(pt2 /. rule69); Return[NormalizeBary[local]]; ]
 
ffanticomplconjugate[pt1_, pt2_] := Module[{local}, 
     local = bAnticomplementaryConjugate[pt1 /. rule69, pt2 /. rule69] /. 
        rule69; Return[NormalizeBary[local]]; ]
 
checkIsogonalConjugates[pt_, name_:"X"] := 
    Module[{cx, prev, res, idx1, idx2, ptc, rc}, rc = intCheckList[[1]]; 
      ptc = N[NormalizeBary[pt /. rule69], 35]; 
      cx = (ffisoconjugate[ptc, #1] & ) /@ ETCBaryNorm; 
      cx = Union[AssociationThread[(StringJoin["C", StringTake[#1, 
             {2, -1}]] & ) /@ Keys[cx], Values[cx]], ETCBaryNorm]; 
      cx = SortBy[Select[cx, Im[#1[[1]]] == 0 & ], #1[[1]] & ]; 
      prev = Association["X0" -> {-1, -1, -1}]; res = {}; 
      Do[If[StringTake[Keys[prev][[1]], 1] == StringTake[n, 1], 
         prev = Association[n -> cx[n]]; Continue[]]; 
        If[coincide[cx[n], prev[[1]], 15], 
         idx1 = ToExpression[StringTake[Keys[prev][[1]], {2, -1}]]; 
          idx2 = ToExpression[StringTake[n, {2, -1}]]; 
          If[coincideNorm[pt /. rc, bPIsogonalConjugate[KimberlingCenterCN[
               idx1], KimberlingCenterCN[idx2]] /. rc], If[idx1 < idx2, 
            AppendTo[res, {idx1, idx2}], AppendTo[res, {idx2, idx1}]]]]; 
        prev = Association[n -> cx[n]]; , {n, Keys[cx]}]; 
      res = SortBy[DeleteDuplicates[res], #1[[1]] & ]; 
      AssociateTo[globalProperties[name], {"isoconjugate" -> res}]; 
      If[ !TrueQ[globalSilence], If[Length[res] > 0, 
        Print[StringJoin["= X(i)-isoconjugate-of-X(j) for these {i, j}: ", 
           StringRiffle[res, ", "]]]; ]]; ]
 
coincideNorm[pt1_, pt2_, prec_:20] := Module[{p1, p2}, 
     p1 = N[NormalizeBary[pt1], 35]; p2 = N[NormalizeBary[pt2], 35]; 
      Return[coincide[p1, p2, prec]]; ]
 
checkBarycentricQuotient[pt_, name_:"X"] := 
    Module[{cx, prev, res, idx1, idx2, ptc}, 
     ptc = N[Normalize[pt /. rule69], 35]; 
      cx = (ffbarycentricproduct[ptc, #1] & ) /@ ETCBaryNorm; 
      cx = Union[AssociationThread[(StringJoin["C", StringTake[#1, 
             {2, -1}]] & ) /@ Keys[cx], Values[cx]], ETCBaryNorm]; 
      cx = SortBy[Select[cx, Im[#1[[1]]] == 0 & ], #1[[1]] & ]; 
      prev = Association["X0" -> {-1, -1, -1}]; res = {}; 
      Do[If[StringTake[Keys[prev][[1]], 1] == StringTake[n, 1], 
         prev = Association[n -> cx[n]]; Continue[]]; 
        If[Abs[cx[n][[1]] - prev[[1]][[1]]] < 10^(-15) && 
          Abs[cx[n][[2]] - prev[[1]][[2]]] < 10^(-15) && 
          Abs[cx[n][[3]] - prev[[1]][[3]]] < 10^(-15), 
         idx1 = ToExpression[StringTake[Keys[prev][[1]], {2, -1}]]; 
          idx2 = ToExpression[StringTake[n, {2, -1}]]; 
          If[Simplify[Det[{{1, 1, 1}, pt, KimberlingCenterCN[idx1]/
                KimberlingCenterCN[idx2]}]] == 0, AppendTo[res, 
            {idx1, idx2}]]; If[Simplify[Det[{{1, 1, 1}, pt, 
               KimberlingCenterCN[idx2]/KimberlingCenterCN[idx1]}]] == 0, 
           AppendTo[res, {idx2, idx1}]]; ]; prev = Association[n -> cx[n]]; , 
       {n, Keys[cx]}]; res = SortBy[DeleteDuplicates[res], #1[[1]] & ]; 
      AssociateTo[globalProperties[name], {"barycentric quotient" -> res}]; 
      If[ !TrueQ[globalSilence], If[Length[res] > 0, 
        Print[StringJoin["= barycentric quotient X(i)/X(j) for these (i, j): \
", StringRiffle[res, ", "]]]; ]]; ]
 
checkCrossConjugates[pt_, name_:"X"] := 
    Module[{cx, prev, res, idx1, idx2, ptc, rc, i1, i2}, 
     rc = intCheckList[[1]]; ptc = N[NormalizeBary[pt /. rule69], 35]; 
      cx = (ffcrosspoint[#1, ptc] & ) /@ ETCBaryNorm; 
      cx = Union[AssociationThread[(StringJoin["C", StringTake[#1, 
             {2, -1}]] & ) /@ Keys[cx], Values[cx]], ETCBaryNorm]; 
      cx = SortBy[Select[cx, Im[#1[[1]]] == 0 & ], #1[[1]] & ]; 
      prev = Association["X0" -> {-1, -1, -1}]; res = {}; 
      Do[If[StringTake[Keys[prev][[1]], 1] == StringTake[n, 1], 
         prev = Association[n -> cx[n]]; Continue[]]; 
        If[coincide[cx[n], prev[[1]]], 
         idx1 = ToExpression[StringTake[Keys[prev][[1]], {2, -1}]]; 
          idx2 = ToExpression[StringTake[n, {2, -1}]]; 
          If[StringTake[n, 1] == "X", {i1, i2} = {idx1, idx2}, 
           {i1, i2} = {idx2, idx1}]; If[coincideNorm[pt /. rc, 
            bCrossConjugate[KimberlingCenterCN[i2] /. rc, KimberlingCenterCN[
                i1] /. rc] /. rc], AppendTo[res, {i2, i1}]]]; 
        prev = Association[n -> cx[n]]; , {n, Keys[cx]}]; 
      res = SortBy[DeleteDuplicates[res], #1[[1]] & ]; 
      AssociateTo[globalProperties[name], {"cross conjugate" -> res}]; 
      If[ !TrueQ[globalSilence], If[Length[res] > 0, 
        Print[StringJoin["= X(i) cross conjugate of X(j) for these {i, j}: ", 
           StringRiffle[res, ", "]]]; ]]; ]
 
ffcrosspoint[pt1_, pt2_] := Module[{local}, 
     local = bCrosspoint[pt1 /. rule69, pt2 /. rule69]; 
      Return[NormalizeBary[local]]; ]
 
checkBarycentricProduct[pt_, name_:"X"] := 
    Module[{cx, prev, res, idx1, idx2, ptc}, 
     ptc = N[Normalize[pt /. rule69], 35]; 
      cx = (ffbarycentricquotient[ptc, #1] & ) /@ ETCBaryNorm; 
      cx = Union[AssociationThread[(StringJoin["C", StringTake[#1, 
             {2, -1}]] & ) /@ Keys[cx], Values[cx]], ETCBaryNorm]; 
      cx = SortBy[Select[cx, Im[#1[[1]]] == 0 & ], #1[[1]] & ]; 
      prev = Association["X0" -> {-1, -1, -1}]; res = {}; 
      Do[If[StringTake[Keys[prev][[1]], 1] == StringTake[n, 1], 
         prev = Association[n -> cx[n]]; Continue[]]; 
        If[Abs[cx[n][[1]] - prev[[1]][[1]]] < 10^(-15) && 
          Abs[cx[n][[2]] - prev[[1]][[2]]] < 10^(-15) && 
          Abs[cx[n][[3]] - prev[[1]][[3]]] < 10^(-15), 
         idx1 = ToExpression[StringTake[Keys[prev][[1]], {2, -1}]]; 
          idx2 = ToExpression[StringTake[n, {2, -1}]]; 
          If[Simplify[Det[{{1, 1, 1}, pt, KimberlingCenterCN[idx1]*
                KimberlingCenterCN[idx2]}]] == 0, If[idx1 < idx2, 
            AppendTo[res, {idx1, idx2}], AppendTo[res, {idx2, idx1}]]]]; 
        prev = Association[n -> cx[n]]; , {n, Keys[cx]}]; 
      res = SortBy[DeleteDuplicates[res], #1[[1]] & ]; 
      AssociateTo[globalProperties[name], {"barycentric product" -> res}]; 
      If[ !TrueQ[globalSilence], If[Length[res] > 0, 
        Print[StringJoin[
           "= barycentric product X(i)*X(j) for these (i, j): ", 
           StringRiffle[res, ", "]]]; ]]; ]
 
checkAnticomplementaryConjugates[pt_, name_:"X"] := 
    Module[{cx, prev, res, idx1, idx2, i1, i2, ptc, rc}, 
     rc = intCheckList[[1]]; ptc = N[NormalizeBary[pt /. rule69], 35]; 
      cx = (ffanticomplconjugate[#1, ptc] & ) /@ ETCBaryNorm; 
      cx = Union[AssociationThread[(StringJoin["C", StringTake[#1, 
             {2, -1}]] & ) /@ Keys[cx], Values[cx]], ETCBaryNorm]; 
      cx = SortBy[Select[cx, Im[#1[[1]]] == 0 & ], #1[[1]] & ]; 
      prev = Association["X0" -> {-1, -1, -1}]; res = {}; 
      Do[If[StringTake[Keys[prev][[1]], 1] == StringTake[n, 1], 
         prev = Association[n -> cx[n]]; Continue[]]; 
        If[coincide[cx[n], prev[[1]]], 
         idx1 = ToExpression[StringTake[Keys[prev][[1]], {2, -1}]]; 
          idx2 = ToExpression[StringTake[n, {2, -1}]]; 
          If[StringTake[n, 1] == "X", {i1, i2} = {idx1, idx2}, 
           {i1, i2} = {idx2, idx1}]; If[coincideNorm[pt /. rc, 
            bAnticomplementaryConjugate[KimberlingCenterCN[i1] /. rc, 
              KimberlingCenterCN[i2] /. rc] /. rc], 
           AppendTo[res, {i1, i2}]; ]]; prev = Association[n -> cx[n]]; , 
       {n, Keys[cx]}]; res = SortBy[DeleteDuplicates[res], #1[[1]] & ]; 
      AssociateTo[globalProperties[name], {"anticomplementary conjugate" -> 
         res}]; If[ !TrueQ[globalSilence], If[Length[res] > 0, 
        Print[StringJoin[
           "= X(i)-anticomplementary conjugate of X(j) for these (i,j): ", 
           StringRiffle[res, ", "]]]; ]]; ]
 
ffdaoconjugate[pt1_, pt2_] := Module[{local}, 
     local = bDaoConjugate[pt1 /. rule69, pt2 /. rule69] /. rule69; 
      Return[NormalizeBary[local]]; ]
 
checkDaoConjugates[pt_, name_:"X"] := Module[{cx, prev, res, idx1, idx2, i1, 
      i2, ptc, rc}, rc = intCheckList[[1]]; 
      ptc = N[NormalizeBary[pt /. rule69], 35]; 
      cx = (ffdaoconjugate[#1, ptc] & ) /@ ETCBaryNorm; 
      cx = Union[AssociationThread[(StringJoin["C", StringTake[#1, 
             {2, -1}]] & ) /@ Keys[cx], Values[cx]], ETCBaryNorm]; 
      cx = SortBy[Select[cx, Im[#1[[1]]] == 0 & ], #1[[1]] & ]; 
      prev = Association["X0" -> {-1, -1, -1}]; res = {}; 
      Do[If[StringTake[Keys[prev][[1]], 1] == StringTake[n, 1], 
         prev = Association[n -> cx[n]]; Continue[]]; 
        If[coincide[cx[n], prev[[1]]], 
         idx1 = ToExpression[StringTake[Keys[prev][[1]], {2, -1}]]; 
          idx2 = ToExpression[StringTake[n, {2, -1}]]; 
          If[StringTake[n, 1] == "X", {i1, i2} = {idx1, idx2}, 
           {i1, i2} = {idx2, idx1}]; If[coincideNorm[pt /. rc, 
            bDaoConjugate[KimberlingCenterCN[i1] /. rc, KimberlingCenterCN[
                i2] /. rc] /. rc], AppendTo[res, {i1, i2}]; ]]; 
        prev = Association[n -> cx[n]]; , {n, Keys[cx]}]; 
      res = SortBy[DeleteDuplicates[res], #1[[1]] & ]; 
      AssociateTo[globalProperties[name], {"dao conjugate" -> res}]; 
      If[ !TrueQ[globalSilence], If[Length[res] > 0, 
        Print[StringJoin["= X(i)-Dao conjugate of X(j) for these {i, j}: ", 
           StringRiffle[res, ", "]]]; ]]; ]
 
checkTrilinearPolar[pt_, name_:"X"] := Module[{cx, ptc, p1, p2}, 
     ptc = N[Normalize[pt /. rule69], 35]; cx = bTripolarEq[ptc] . {x, y, z}; 
      dset = (Abs[cx] /. Thread[{x, y, z} -> #1] & ) /@ ETCBaryNorm; 
      test = Select[dset, #1 < 10^(-10) & ]; If[Length[test] > 1, 
       p1 = ToExpression[StringTake[Keys[test][[1]], {2, -1}]]; 
        p2 = ToExpression[StringTake[Keys[test][[2]], {2, -1}]]; 
        If[Simplify[Det[{{1, 1, 1}, pt, bTripole[KimberlingCenterCN[p1], 
              KimberlingCenterCN[p2]]}]] == 0, 
         AssociateTo[globalProperties[name], {"trilinear polar" -> 
             {p1, p2}}]; If[ !TrueQ[globalSilence], 
           Print[StringJoin["= trilinear pole of line {", ToString[p1], ",", 
             ToString[p2], "}"]]]; ]; ]; ]
 
ffcevaconjugate[pt1_, pt2_] := Module[{local}, 
     local = bCevianQuotient[pt1 /. rule69, pt2 /. rule69] /. rule69; 
      Return[NormalizeBary[local]]; ]
 
checkCevaConjugates[pt_, name_:"X"] := Module[{cx, prev, res, idx1, idx2, i1, 
      i2, ptc, rc}, rc = intCheckList[[1]]; 
      ptc = N[NormalizeBary[pt /. rule69], 35]; 
      cx = (ffcevaconjugate[#1, ptc] & ) /@ ETCBaryNorm; 
      cx = Union[AssociationThread[(StringJoin["C", StringTake[#1, 
             {2, -1}]] & ) /@ Keys[cx], Values[cx]], ETCBaryNorm]; 
      cx = SortBy[Select[cx, Im[#1[[1]]] == 0 & ], #1[[1]] & ]; 
      prev = Association["X0" -> {-1, -1, -1}]; res = {}; 
      Do[If[StringTake[Keys[prev][[1]], 1] == StringTake[n, 1], 
         prev = Association[n -> cx[n]]; Continue[]]; 
        If[coincide[cx[n], prev[[1]]], 
         idx1 = ToExpression[StringTake[Keys[prev][[1]], {2, -1}]]; 
          idx2 = ToExpression[StringTake[n, {2, -1}]]; 
          If[StringTake[n, 1] == "X", {i1, i2} = {idx1, idx2}, 
           {i1, i2} = {idx2, idx1}]; If[coincideNorm[pt /. rc, 
            bCevianQuotient[KimberlingCenterCN[i1] /. rc, KimberlingCenterCN[
                i2] /. rc] /. rc], AppendTo[res, {i1, i2}]; ]]; 
        prev = Association[n -> cx[n]]; , {n, Keys[cx]}]; 
      res = SortBy[DeleteDuplicates[res], #1[[1]] & ]; 
      AssociateTo[globalProperties[name], {"ceva conjugate" -> res}]; 
      If[ !TrueQ[globalSilence], If[Length[res] > 0, 
        Print[StringJoin["= X(i)-Ceva conjugate of X(j) for these {i, j}: ", 
           StringRiffle[res, ", "]]]; ]]; ]
 
checkComplementaryConjugates[pt_, name_:"X"] := 
    Module[{cx, prev, res, idx1, idx2, i1, i2, ptc, rc}, 
     rc = intCheckList[[1]]; ptc = N[NormalizeBary[pt /. rule69], 35]; 
      cx = (ffcomplconjugate[#1, ptc] & ) /@ ETCBaryNorm; 
      cx = Union[AssociationThread[(StringJoin["C", StringTake[#1, 
             {2, -1}]] & ) /@ Keys[cx], Values[cx]], ETCBaryNorm]; 
      cx = SortBy[Select[cx, Im[#1[[1]]] == 0 & ], #1[[1]] & ]; 
      prev = Association["X0" -> {-1, -1, -1}]; res = {}; 
      Do[If[StringTake[Keys[prev][[1]], 1] == StringTake[n, 1], 
         prev = Association[n -> cx[n]]; Continue[]]; 
        If[coincide[cx[n], prev[[1]]], 
         idx1 = ToExpression[StringTake[Keys[prev][[1]], {2, -1}]]; 
          idx2 = ToExpression[StringTake[n, {2, -1}]]; 
          If[StringTake[n, 1] == "X", {i1, i2} = {idx1, idx2}, 
           {i1, i2} = {idx2, idx1}]; If[coincideNorm[pt /. rc, 
            bComplementaryConjugate[KimberlingCenterCN[i1] /. rc, 
              KimberlingCenterCN[i2] /. rc] /. rc], 
           AppendTo[res, {i1, i2}]; ]]; prev = Association[n -> cx[n]]; , 
       {n, Keys[cx]}]; res = SortBy[DeleteDuplicates[res], #1[[1]] & ]; 
      AssociateTo[globalProperties[name], {"complementary conjugate" -> 
         res}]; If[ !TrueQ[globalSilence], If[Length[res] > 0, 
        Print[StringJoin[
           "= X(i)-complementary conjugate of X(j) for these (i,j): ", 
           StringRiffle[res, ", "]]]; ]]; ]
 
ffcomplconjugate[pt1_, pt2_] := Module[{local}, 
     local = bComplementaryConjugate[pt1 /. rule69, pt2 /. rule69] /. rule69; 
      Return[NormalizeBary[local]]; ]
 
checkVertexConjugates[pt_, name_:"X"] := 
    Module[{cx, prev, res, idx1, idx2, ptc, rc}, rc = intCheckList[[1]]; 
      ptc = N[NormalizeBary[pt /. rule69], 35]; 
      cx = (ffvertexconjugate[ptc, #1] & ) /@ ETCBaryNorm; 
      cx = Union[AssociationThread[(StringJoin["C", StringTake[#1, 
             {2, -1}]] & ) /@ Keys[cx], Values[cx]], ETCBaryNorm]; 
      cx = SortBy[Select[cx, Im[#1[[1]]] == 0 & ], #1[[1]] & ]; 
      prev = Association["X0" -> {-1, -1, -1}]; res = {}; 
      Do[If[StringTake[Keys[prev][[1]], 1] == StringTake[n, 1], 
         prev = Association[n -> cx[n]]; Continue[]]; 
        If[coincide[cx[n], prev[[1]], 15], 
         idx1 = ToExpression[StringTake[Keys[prev][[1]], {2, -1}]]; 
          idx2 = ToExpression[StringTake[n, {2, -1}]]; 
          If[coincideNorm[pt /. rc, bVertexConjugate[KimberlingCenterCN[
               idx1], KimberlingCenterCN[idx2]] /. rc], If[idx1 < idx2, 
            AppendTo[res, {idx1, idx2}], AppendTo[res, {idx2, idx1}]]]]; 
        prev = Association[n -> cx[n]]; , {n, Keys[cx]}]; 
      res = SortBy[DeleteDuplicates[res], #1[[1]] & ]; 
      AssociateTo[globalProperties[name], {"vertex conjugate" -> res}]; 
      If[ !TrueQ[globalSilence], If[Length[res] > 0, 
        Print[StringJoin[
           "= X(i)-vertex conjugate of X(j) for these {i, j}: ", 
           StringRiffle[res, ", "]]]; ]]; ]
 
ffvertexconjugate[pt1_, pt2_] := Module[{local}, 
     local = bVertexConjugate[pt1 /. rule69, pt2 /. rule69] /. rule69; 
      Return[NormalizeBary[local]]; ]
 
pointChecker[expr_, num_:0, full_:False, inname_:"X"] := 
    Module[{ptcoord, pt, chk, lines, barys, symcheck, name}, 
     ptcoord = evaluate[expr]; pt = N[NormalizeBary[ptcoord /. rule69], 35]; 
      symcheck = pt - N[NormalizeBary[symmetrizeInternal2[ptcoord[[1]]] /. 
           rule69], 35]; If[AnyTrue[symcheck, #1 != 0 & ], 
       Print[ptcoord]; Print["expression is not symmetric"]; 
        Return[False, Module]; ]; If[Length[globalSeenPoints] > 0 && 
        First[MinimalBy[Value][(Total[Abs[#1[[2]] - pt]] & ) /@ 
            globalSeenPoints]] <= 10^(-20), Print[inname]; 
        Print["Point seen"]; Return[False, Module], 
       AppendTo[globalSeenPoints, {ptcoord, pt}]; ]; 
      If[num > 0, chk = {1}, chk = checkPointinETC[pt]]; 
      If[chk[[1]] < 10^(-12), Print[StringJoin["ETC: ", Keys[chk]]], 
       barys = Factor[FactorTermsList[expr[[1]]][[2]]]; 
        If[StringLength[inname] == 0, name = ToString[ExpressionToTrad[
            expr[[1]]]], name = inname]; AssociateTo[globalProperties, 
         name -> Association[]]; If[full, lines = 
          Quiet[linesProcessAlg[ptcoord, barys, 20, False, False, name]], 
         lines = Quiet[linesProcessAlg[ptcoord, barys, 20, False, True, 
             name]]; ]; If[full || Length[lines] > 3, 
         Quiet[checkCircumconics[ptcoord, 1, 60, num, name]]; 
          Quiet[checkCurves[ptcoord, name]]; Quiet[checkTrilinearPolar[
            ptcoord, name]]; Quiet[checkIsogonalConjugates[ptcoord, name]]; 
          Quiet[checkDaoConjugates[ptcoord, name]]; 
          Quiet[checkCevaConjugates[ptcoord, name]]; 
          Quiet[checkVertexConjugates[ptcoord, name]]; 
          Quiet[checkComplementaryConjugates[ptcoord, name]]; 
          Quiet[checkAnticomplementaryConjugates[ptcoord, name]]; 
          Quiet[checkCrossConjugates[ptcoord, name]]; 
          Quiet[checkBarycentricProduct[ptcoord, name]]; 
          Quiet[checkBarycentricQuotient[ptcoord, name]]; 
          Quiet[checkPerspector[ptcoord, name]]; TimeConstrained[
           Quiet[pointCheckAllProcesses[ptcoord, name]], 60]; ]; ]; ]
 
globalSeenPoints = {}
 
checkCurves[pt_, inname_:"X"] := Module[{out, ptest, d}, 
     out = {}; Do[ptest = N[NormalizeBary[(evaluate /. rule69)[pt]], 35]; 
        d = getTriangleCurve[name] /. Thread[{x, y, z} -> ptest] /. rule69; 
        If[Abs[d] < 10^(-12), AppendTo[out, name]]; , 
       {name, Keys[TriangleCurves]}]; AssociateTo[globalProperties[inname], 
       {"curves" -> out}]; If[Length[out] > 0, 
       If[ !TrueQ[globalSilence], Print[StringJoin["Lies on curves: ", 
           StringRiffle[out, ", "]]]]; ]; ]
 
checkPerspector[pt_, inname_:"X"] := Module[{out, ptest, ptcheck, crv, set1, 
      rc}, out = {}; ptest = N[NormalizeBary[evaluate[pt] /. rule69], 35]; 
      crv = bCircumconicPEq[ptest]; set1 = checkPointsOnCurve[crv]; 
      rc = intCheckList[[1]]; Do[ptest = N[NormalizeBary[evaluate[pt] /. rc], 
          35]; crv = bCircumconicPEq[ptest]; ptcheck = 
         N[KimberlingCenterCN[ToExpression[StringTake[ptoncrv, {3, -2}]]] /. 
           rc, 35]; If[(crv /. Thread[{x, y, z} -> ptcheck]) < 10^(-15), 
         AppendTo[out, ptoncrv]], {ptoncrv, set1}]; 
      AssociateTo[globalProperties[inname], {"perspector" -> out}]; 
      If[Length[out] > 0, If[ !TrueQ[globalSilence], 
         Print[StringJoin[
           "Points which lie on circumconic with this perspector: ", 
           StringRiffle[out, ", "]]]]; ]; ]
 
pointCheckerTransform[expr_, inname_, num_:0] := 
    Module[{pointProcesses, deg, texpr, procname}, 
     If[ !TrueQ[globalSilence], Print[inname]]; pointChecker[expr, num, 
       False, inname]; If[TrueQ[globalSilence], printGlobalProperties[
        globalProperties, inname]]; pointProcesses = 
       Association["isotomic conjugate" -> Hold[bIsotomicConjugate[#1]], 
        "isogonal conjugate" -> Hold[bIsogonalConjugate[#1]], 
        "complement" -> Hold[bComplement[KimberlingCenterC[2], #1]], 
        "anticomplement" -> Hold[bAntiComplement[KimberlingCenterC[2], #1]], 
        "cyclocevian conjugate" -> Hold[bCyclocevianConjugate[#1]], 
        "circumcircle inverse" -> Hold[bCircumcircleInverse[#1]], 
        "circlecevian perspector" -> Hold[bCirclecevianPerspector[#1]], 
        "zosma transform" -> Hold[bZosmaTransform[#1]], 
        "antitomic conjugate" -> Hold[bAntitomicConjugate[#1]], 
        "polar conjugate" -> Hold[bPIsogonalConjugate[KimberlingCenterCN[48], 
           #1]], "eigentransform" -> Hold[bEigentransform[#1]], 
        "collings transform" -> Hold[bCollingsTransform[#1]], 
        "cundyParryPsi" -> Hold[cundyParryPsi[#1]], "cundyParryPhi" -> 
         Hold[cundyParryPhi[#1]]]; 
      Do[texpr = simplifyRationalBarycentrics[
          Factor[Together[evaluate[ReleaseHold[pointProcesses[name] /. #1 -> 
                expr]]]]]; deg = (Max[Apply[Plus, CoefficientRules[#1][[All,
              1]], {1}]] & )[texpr[[1]]]; If[deg <= 20, 
         procname = StringJoin[name, " of ", inname]; 
          If[ !TrueQ[globalSilence], Print[procname]]; pointChecker[texpr, 0, 
           False, procname]; If[TrueQ[globalSilence], printGlobalProperties[
            globalProperties, procname]]; ], {name, Keys[pointProcesses]}]; ]
 
printGlobalProperties[glob_, name_:""] := Module[{hg, cycle}, 
     If[StringLength[name] > 0, cycle = {name}, cycle = Keys[glob]]; 
      Do[If[ !MemberQ[Keys[glob[pt]], "name"], Continue[]]; Print[]; Print[]; 
        Print[pt]; Print[]; Print[glob[pt]["name"]]; Print[]; 
        Print[StringJoin["Barycentrics    ", glob[pt]["barycentrics"]]]; 
        Print[]; hg = If[ !MemberQ[Keys[glob[pt]], "circumconics"], {}, 
          glob[pt]["circumconics"]]; If[Length[hg] > 0, 
         Print[StringJoin["lies on circumconics ", StringRiffle[hg, ", "], 
           " and on these lines: ", StringRiffle[glob[pt]["lines"], ", "]]], 
         Print[StringJoin["lies on these lines: ", StringRiffle[
             glob[pt]["lines"], ", "]]]; ]; Print[]; 
        hg = glob[pt]["midpoints"]; If[Length[hg] > 0, 
         Print[StringJoin["= midpoint of X(i) in X(j) for these {i,j}: ", 
            StringRiffle[SortBy[hg, ToExpression[#1[[1]]] & ], ", "]]]; ]; 
        hg = glob[pt]["reflections"]; If[Length[hg] > 0, 
         Print[StringJoin["= reflection of X(i) in X(j) for these {i,j}: ", 
            StringRiffle[SortBy[hg, ToExpression[#1[[1]]] & ], ", "]]]; ]; 
        If[ !MemberQ[Keys[glob[pt]], "circumconics"], 
         hg = glob[pt]["harmonic"]; If[Length[hg] > 0, 
           Print[StringJoin[
              "= {X(i),X(j)}-harmonic conjugate of X(k) for these (i,j,k): ", 
              StringRiffle[SortBy[hg, ToExpression[#1[[1]]] & ], ", "]]]; ]; 
          Continue[]; ]; hg = glob[pt]["curves"]; If[Length[hg] > 0, 
         Print[StringJoin["lies on curves: ", StringRiffle[hg, ", "]]]; ]; 
        Do[If[KeyExistsQ[glob[pt], proc], Print[StringJoin["= ", proc, 
              " of ", glob[pt][proc]]]; ]; , 
         {proc, Keys[singlePointProcesses]}]; 
        If[KeyExistsQ[glob[pt], "trilinear polar"], 
         hg = glob[pt]["trilinear polar"]; If[Length[hg] > 0, 
           Print[StringJoin["= trilinear pole of line {", ToString[hg[[1]]], 
              ",", ToString[hg[[2]]], "}"]]; ]; ]; 
        hg = glob[pt]["isoconjugate"]; If[Length[hg] > 0, 
         Print[StringJoin["= X(i)-isoconjugate-of-X(j) for these {i, j}: ", 
            StringRiffle[hg, ", "]]]; ]; hg = glob[pt]["vertex conjugate"]; 
        If[Length[hg] > 0, Print[StringJoin[
            "= X(i)-vertex conjugate of X(j) for these {i, j}: ", 
            StringRiffle[hg, ", "]]]; ]; hg = glob[pt]["dao conjugate"]; 
        If[Length[hg] > 0, Print[StringJoin[
            "= X(i)-Dao conjugate of X(j) for these {i, j}: ", 
            StringRiffle[hg, ", "]]]; ]; 
        hg = glob[pt]["complementary conjugate"]; If[Length[hg] > 0, 
         Print[StringJoin[
            "= X(i)-complementary conjugate of X(j) for these {i, j}: ", 
            StringRiffle[hg, ", "]]]; ]; 
        hg = glob[pt]["anticomplementary conjugate"]; If[Length[hg] > 0, 
         Print[StringJoin[
            "= X(i)-anticomplementary conjugate of X(j) for these {i, j}: ", 
            StringRiffle[hg, ", "]]]; ]; hg = glob[pt]["cross conjugate"]; 
        If[Length[hg] > 0, Print[StringJoin[
            "= X(i)-cross conjugate of X(j) for these {i, j}: ", 
            StringRiffle[hg, ", "]]]; ]; 
        hg = glob[pt]["barycentric product"]; If[Length[hg] > 0, 
         Print[StringJoin[
            "= barycentric product X(i)*X(j) for these (i, j):  ", 
            StringRiffle[hg, ", "]]]; ]; 
        hg = glob[pt]["barycentric quotient"]; If[Length[hg] > 0, 
         Print[StringJoin[
            "= barycentric quotient X(i)/X(j) for these (i, j):  ", 
            StringRiffle[hg, ", "]]]; ]; hg = glob[pt]["perspector"]; 
        If[Length[hg] > 0, Print[StringJoin[
            "Points which lie on circumconic with this perspector:  ", 
            StringRiffle[hg, ", "]]]; ]; hg = glob[pt]["harmonic"]; 
        If[Length[hg] > 0, Print[StringJoin[
            "= {X(i),X(j)}-harmonic conjugate of X(k) for these (i,j,k): ", 
            StringRiffle[SortBy[hg, ToExpression[#1[[1]]] & ], ", "]]]; ]; , 
       {pt, cycle}]]
