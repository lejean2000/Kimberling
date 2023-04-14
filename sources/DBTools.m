pointProcessBary[expr_, rule_:rule69] := 
    Module[{pointsBary, pti, pta, str, coords}, pointsBary = Association[]; 
      Do[pta = ETCBaryNorm[k]; If[AnyTrue[Im[pta], #1 > 0 & ], Continue[]]; 
        If[AnyTrue[pta, #1 === ComplexInfinity & ] || AnyTrue[pta, 
           #1 === Infinity & ], Continue[]]; 
        pti = intnumericnorm[evaluate[ReleaseHold[expr /. #1 -> pta]] /. 
           rule]; If[pti[[1]] =!= Indeterminate, 
         AppendTo[pointsBary, k -> pti]; ], {k, Keys[ETCBaryNorm]}]; 
      Return[pointsBary]; ]
 
intnumericnorm[val_] := N[NormalizeBary[val], 35]
 
singlePointProcesses = <|"complement" -> 
      Hold[bComplement[KimberlingCenterCN[2], #1]], 
     "anticomplement" -> Hold[bAntiComplement[KimberlingCenterCN[2], #1]], 
     "polar conjugate" -> Hold[bPIsogonalConjugate[KimberlingCenterCN[48], 
        #1]], "cyclocevian conjugate" -> Hold[bCyclocevianConjugate[#1]], 
     "circumcircle inverse" -> Hold[bCircumcircleInverse[#1]], 
     "zosma transform" -> Hold[bZosmaTransform[#1]], 
     "anticomplement of isogonal conjugate" -> 
      Hold[bAntiComplement[KimberlingCenterC[2], bIsogonalConjugate[#1]]], 
     "anticomplement of isotomic conjugate" -> 
      Hold[bAntiComplement[KimberlingCenterC[2], bIsotomicConjugate[#1]]], 
     "complement of isogonal conjugate" -> 
      Hold[bComplement[KimberlingCenterC[2], bIsogonalConjugate[#1]]], 
     "complement of isotomic conjugate" -> 
      Hold[bComplement[KimberlingCenterC[2], bIsotomicConjugate[#1]]], 
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
      dump}, hgroups = {}; Do[fgr1 = SortBy[set, numsortexpr[#1] & ]; 
        If[Length[fgr1] > 2000, If[ !TrueQ[globalSilence], 
           Print[StringJoin["Found lines with ", ToString[Length[fgr1]], 
             " points"]]]; fgr1 = Take[fgr1, 2000]; ]; 
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
 
coincide[pt1_, pt2_, prec_:20] := Abs[pt1[[1]] - pt2[[1]]] < 10^(-prec) && 
     Abs[pt1[[2]] - pt2[[2]]] < 10^(-prec) && Abs[pt1[[3]] - pt2[[3]]] < 
      10^(-prec)
 
intPointCheck[pt_, process_, rule_:rule69] := Module[{tmp, res, ptn, ptnum}, 
     ptn = intnumericnorm[pt /. rule]; tmp = pointProcessBary[process, rule]; 
      res = Select[(coincide[#1, ptn] & ) /@ tmp, TrueQ]; 
      If[Length[res] > 0, If[intVerifyPointProcess[pt, Keys[res][[1]], 
          process], Return[Keys[res][[1]]]]; ]; ]
 
intVerifyPointProcess[pt_, xnum_, processexpr_] := 
    Module[{ptn, pta, pti}, Do[ptn = intnumericnorm[pt /. rc]; 
        pta = intnumericnorm[KimberlingCenterCNy[xnum] /. rc]; 
        pti = intnumericnorm[ReleaseHold[processexpr /. #1 -> pta] /. rc]; 
        If[ !coincide[ptn, pti], Return[False, Module]; ]; , 
       {rc, intCheckList}]; Return[True]; ]
 
xnum[str_] := ToExpression[StringTake[str, {2, -1}]]
 
KimberlingCenterCNy[key_] := If[StringTake[key, 1] == "X", 
     KimberlingCenterCN[ToExpression[StringTake[key, {2, -1}]]], 
     evaluate[symmetrizeInternal[ETC[key] /. 
        Thread[{A -> angleA, B -> angleB, C -> angleC}]]]]
 
intCheckList = {{a -> 5, b -> 6, c -> 7}, {a -> 4, b -> 11, c -> 13}}
 
pointCheckAllProcesses[pt_, name_:"X"] := Module[{res, prop}, 
     Do[If[ !TrueQ[globalSilence], PrintTemporary[proc]]; 
        res = intPointCheck[pt, singlePointProcesses[proc]]; 
        If[StringQ[res], prop = StringJoin["= ", proc, " of ", res]; 
          If[ !TrueQ[globalSilence], Print[colorformat[prop]]]; 
          AssociateTo[globalProperties[name], proc -> intaddbrackets[
             res]]; ]; , {proc, Keys[singlePointProcesses]}]; ]
 
colorformat[string_, cases_:RegularExpression["Y\\(\\d+\\)|Y\\d+"]] := 
    Module[{pos, agg, res}, If[ !TrueQ[colorPrintOn], 
       Return[string, Module]]; pos = StringPosition[string, cases]; 
      agg = ({Switch[#1[[1,2]], 1, Red, 2, Brown, _, Blue], 
          #1[[1 ;; All,{1}]]} & ) /@ GatherBy[
         Tally[Flatten[Apply[Range, pos, {1}]]], Last]; 
      If[Length[agg] > 0, Return[intmark[string, agg]], Return[string]]; ]
 
intmark[number_, spec:{{_, _}..}] := 
    Row[With[{n = Characters[ToString[number]]}, 
      Fold[Function[{x, y}, MapAt[Style[#1, y[[1]]] & , x, y[[2]]]], n, 
       spec]]]
 
globalProperties = <||>
 
intaddbrackets[pname_] := StringJoin[StringTake[pname, 1], "(", 
     StringTake[pname, {2, -1}], ")"]
 
checkCircumconics[pt_, start_:1, time_:60, excl_:0, name_:"X"] := 
    Module[{ptc, p1, p2, crv, dset, test, out, conicname, check}, 
     TimeConstrained[out = {}; ptc = N[NormalizeBary[pt /. rule69], 35]; 
        Do[ClearSystemCache[]; funcind = nx; 
          crv = N[bCircumconicEq[ptc, ETCBaryNorm[nx]] /. rule69, 35]; 
          dset = (Abs[crv] /. Thread[{x, y, z} -> #1] & ) /@ ETCBaryNorm; 
          test = Select[dset, #1 < 10^(-10) & ]; If[Length[test] > 1, 
           p1 = Keys[test][[1]]; p2 = Keys[test][[2]]; 
            If[p1 == excl || p2 == excl, Continue[]]; 
            check = TimeConstrained[Simplify[bCircumconicEq[
                 KimberlingCenterCNy[p1], KimberlingCenterCNy[p2]] /. 
                Thread[{x, y, z} -> pt]], 10, -1]; If[check == 0, 
             conicname = StringJoin["{A,B,C,", intaddbrackets[p1], ",", 
                intaddbrackets[p2], "}"]; If[ !MemberQ[out, conicname], 
               AppendTo[out, conicname]]]; ]; , 
         {nx, Take[Keys[ETC], {start, start + Max[1, Floor[time/60]]*
              200}]}]; , time, funcind]; AssociateTo[globalProperties[name], 
       {"circumconics" -> out}]; If[ !TrueQ[globalSilence], 
       If[Length[out] > 0, Print[colorformat[StringJoin[
            "Lies on circumconics: ", StringRiffle[out, ", "]]]]; ]]; 
      Return[funcind]; ]
 
linesProcessAlg[ptcoord_, printexpr_, prec_, debug_, abort_, name_] := 
    Module[{res, gr, hg, out, head, test, test2, hgroups, ptc, unproven, rc, 
      rc2, eltest, sout, barys, outname}, rc = intCheckList[[1]]; 
      rc2 = intCheckList[[2]]; ptc = N[NormalizeBary[evaluate[ptcoord] /. 
          rule69], 35]; res = intLinesProcessFullGroups[ptc, prec]; 
      gr = ({#1[[1]][[1]], #1[[2]][[1]]} & ) /@ res[[1]]; out = {}; 
      unproven = {}; Do[If[debug, PrintTemporary[el]]; 
        test = TimeConstrained[bCollinearityMatrix[
           KimberlingCenterCNy[el[[1]]] /. rc, KimberlingCenterCNy[
             el[[2]]] /. rc, ptcoord /. rc], 10, -1]; 
        test2 = TimeConstrained[bCollinearityMatrix[
           KimberlingCenterCNy[el[[1]]] /. rc2, KimberlingCenterCNy[
             el[[2]]] /. rc2, ptcoord /. rc2], 10, -1]; 
        If[TrueQ[Simplify[test] == 0] && TrueQ[Simplify[test2] == 0], 
         AppendTo[out, el], AppendTo[unproven, el]]; , {el, gr}]; 
      If[Length[out] >= 2, sout = SortBy[out, 
          StringJoin[numsortexpr[#1[[1]]], numsortexpr[#1[[2]]]] & ]; 
        outname = StringJoin[intaddbrackets[sout[[1]][[1]]], 
          intaddbrackets[sout[[1]][[2]]], "\:2229", intaddbrackets[
           sout[[2]][[1]]], intaddbrackets[sout[[2]][[2]]]]; 
        AssociateTo[globalProperties[name], {"name" -> outname}]; 
        If[ !TrueQ[globalSilence], Print[colorformat[outname]]]; ]; 
      barys = ExpressionToTrad[Simplify[printexpr]]; 
      AssociateTo[globalProperties[name], {"barycentrics" -> barys}]; 
      If[ !TrueQ[globalSilence], Print[StringJoin["Barycentrics    ", 
         barys]]]; out = ({intnameformat[#1[[1]]], intnameformat[
           #1[[2]]]} & ) /@ out; AssociateTo[globalProperties[name], 
       {"lines" -> out}]; If[ !TrueQ[globalSilence], 
       Print[colorformat[StringJoin["Lies on these lines: ", 
          StringRiffle[out, ", "]]]]]; If[abort && Length[out] < 3, 
       Return[out, Module]; ]; hg = {}; 
      Do[If[Length[igroup] != 2, Continue[]]; 
        If[Sort[{Length[igroup[[1]]], Length[igroup[[2]]]}] != {0, 2}, 
         Continue[]]; head = Select[igroup, Length[#1] == 0 & ]; 
        Do[If[el == head[[1]], Continue[]]; eltest = 
           Flatten[{el, head[[1]]}]; test = Det[{{1, 1, 1}, 
             bHarmonicConjugate[KimberlingCenterCNy[eltest[[1]]] /. rc, 
              KimberlingCenterCNy[eltest[[2]]] /. rc, KimberlingCenterCNy[
                eltest[[3]]] /. rc], ptcoord /. rc}]; 
          test2 = Det[{{1, 1, 1}, bHarmonicConjugate[KimberlingCenterCNy[
                eltest[[1]]] /. rc2, KimberlingCenterCNy[eltest[[2]]] /. rc2, 
              KimberlingCenterCNy[eltest[[3]]] /. rc2], ptcoord /. rc2}]; 
          If[TrueQ[Simplify[test] == 0] && TrueQ[Simplify[test2] == 0], 
           AppendTo[hg, eltest]; ]; , {el, igroup}], 
       {igroup, intHarmonicProcess[res[[2]], ptc, prec]}]; 
      hg = SortBy[hg, numsortexpr[#1[[1]]] & ]; 
      hg = ({intnameformat[#1[[1]]], intnameformat[#1[[2]]], 
          intnameformat[#1[[3]]]} & ) /@ hg; AssociateTo[
       globalProperties[name], {"harmonic" -> hg}]; 
      If[ !TrueQ[globalSilence], If[Length[hg] > 0, 
        Print[colorformat[StringJoin[
            "= {X(i),X(j)}-harmonic conjugate of X(k) for these (i,j,k): ", 
            StringRiffle[hg, ", "]]]]; ]]; hg = {}; 
      Do[eltest = igroup; test = Det[{{1, 1, 1}, bMidpoint[
            KimberlingCenterCNy[eltest[[1]]] /. rc, 
            KimberlingCenterCNy[eltest[[2]]] /. rc], ptcoord /. rc}]; 
        test2 = Det[{{1, 1, 1}, bMidpoint[KimberlingCenterCNy[eltest[[1]]] /. 
             rc2, KimberlingCenterCNy[eltest[[2]]] /. rc2], ptcoord /. rc2}]; 
        If[TrueQ[Simplify[test] == 0] && TrueQ[Simplify[test2] == 0], 
         AppendTo[hg, eltest]; ]; , {igroup, intMidpointProcess[res[[2]], 
         ptc, prec]}]; hg = SortBy[hg, numsortexpr[#1[[1]]] & ]; 
      hg = ({intnameformat[#1[[1]]], intnameformat[#1[[2]]]} & ) /@ hg; 
      AssociateTo[globalProperties[name], {"midpoints" -> hg}]; 
      If[ !TrueQ[globalSilence], If[Length[hg] > 0, 
        Print[colorformat[StringJoin[
            "= midpoint of X(i) in X(j) for these {i,j}: ", 
            StringRiffle[hg, ", "]]]]; ]]; hg = {}; 
      Do[eltest = igroup; test = Det[{{1, 1, 1}, bReflectionPP[
            KimberlingCenterCNy[eltest[[1]]] /. rc, 
            KimberlingCenterCNy[eltest[[2]]] /. rc], ptcoord /. rc}]; 
        test2 = Det[{{1, 1, 1}, bReflectionPP[KimberlingCenterCNy[
              eltest[[1]]] /. rc2, KimberlingCenterCNy[eltest[[2]]] /. rc2], 
           ptcoord /. rc2}]; If[TrueQ[Simplify[test] == 0] && 
          TrueQ[Simplify[test2] == 0], AppendTo[hg, eltest]; ]; , 
       {igroup, intReflectionProcess[res[[2]], ptc, prec]}]; 
      hg = SortBy[hg, numsortexpr[#1[[1]]] & ]; 
      hg = ({intnameformat[#1[[1]]], intnameformat[#1[[2]]]} & ) /@ hg; 
      AssociateTo[globalProperties[name], {"reflections" -> hg}]; 
      If[ !TrueQ[globalSilence], If[Length[hg] > 0, 
        Print[colorformat[StringJoin[
            "= reflection of X(i) in X(j) for these {i,j}: ", 
            StringRiffle[hg, ", "]]]]; ]]; Return[out]; ]
 
intLinesProcessFullGroups[pt_, prec_] := 
    Module[{tplist, tp, prev, outgroups, group, fullgroups, dump}, 
     tplist = {}; fullgroups = {}; Do[tp = 1/bLine[pt, ETCBaryNorm[name]]; 
        If[Im[tp[[1]]] != 0 || AnyTrue[ !NumberQ[tp]], Continue[]]; 
        AppendTo[tplist, {name, NormalizeBary[tp]}]; , 
       {name, Keys[ETCBaryNorm]}]; tplist = SortBy[tplist, #1[[2]][[1]] & ]; 
      prev = {"0", {0, 0, 0}}; outgroups = {}; group = {}; 
      Do[If[coincide[el[[2]], prev[[2]]], AppendTo[group, el]; 
          If[Length[group] == 1, AppendTo[group, prev]; dump = 1], 
         If[dump == 1, AppendTo[fullgroups, group]; AppendTo[outgroups, 
             Take[SortBy[group, numsortexpr[#1[[1]]] & ], 2]]; group = {}; 
            dump = 0]; ]; prev = el; , {el, tplist}]; 
      outgroups = SortBy[outgroups, numsortexpr[#1[[1]][[1]]] & ]; 
      fullgroups = ((#1[[1]] & ) /@ #1 & ) /@ fullgroups; 
      Return[{outgroups, fullgroups}]; ]
 
intnameformat[pname_] := If[StringTake[pname, 1] == "X", 
     StringTake[pname, {2, -1}], pname]
 
intMidpointProcess[fullgroups_, pt_, prec_] := 
    Module[{fgr1, checks, flatfg2, ingroupnbary, un, hgroups, hgroup, prev, 
      dump}, hgroups = {}; Do[fgr1 = SortBy[set, numsortexpr[#1] & ]; 
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
      dump}, hgroups = {}; Do[fgr1 = SortBy[set, numsortexpr[#1] & ]; 
        If[Length[fgr1] > 2000, fgr1 = Take[fgr1, 2000]; ]; 
        checks = AssociationMap[NormalizeBary[bMidpoint[ETCBaryNorm[#1], 
             pt]] & , fgr1]; 
        Do[refl = Select[fgr1, coincide[ETCBaryNorm[#1], checks[el]] & ]; 
          If[Length[refl] > 0, AppendTo[hgroups, {el, First[refl]}]], 
         {el, Keys[checks]}]; , {set, fullgroups}]; Return[hgroups]; ]
 
checkIsogonalConjugates[pt_, name_:"X"] := 
    Module[{cx, prev, res, idx1, idx2, ptc, rc, ffisoconjugate}, 
     ffisoconjugate[pt1_, pt2_] := Module[{local}, 
        local = bPIsogonalConjugate[intnumericnorm[pt1 /. rule69], 
            intnumericnorm[pt2 /. rule69]] /. rule69; 
         Return[intnumericnorm[local]]; ]; rc = intCheckList[[1]]; 
      ptc = intnumericnorm[pt /. rule69]; 
      cx = (ffisoconjugate[ptc, #1] & ) /@ ETCBaryNorm; 
      cx = Union[AssociationThread[(StringJoin["C", #1] & ) /@ Keys[cx], 
         Values[cx]], ETCBaryNorm]; 
      cx = SortBy[Select[cx, Im[#1[[1]]] == 0 & ], #1[[1]] & ]; 
      prev = Association["X0" -> {-1, -1, -1}]; res = {}; 
      Do[If[(StringTake[Keys[prev][[1]], 1] == "C" && StringTake[n, 1] == 
            "C") || (StringTake[Keys[prev][[1]], 1] != "C" && 
           StringTake[n, 1] != "C"), prev = Association[n -> cx[n]]; 
          Continue[]]; If[coincide[cx[n], prev[[1]], 15], 
         idx1 = If[StringTake[Keys[prev][[1]], 1] == "C", 
            StringTake[Keys[prev][[1]], {2, -1}], Keys[prev][[1]]]; 
          idx2 = If[StringTake[n, 1] == "C", StringTake[n, {2, -1}], n]; 
          If[coincideNorm[pt /. rc, bPIsogonalConjugate[KimberlingCenterCNy[
               idx1], KimberlingCenterCNy[idx2]] /. rc], 
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
    Module[{ff, cx, prev, res, idx1, idx2, ptc, val, sgn}, 
     ff[pt1_, pt2_] := Module[{local}, If[type == "quotient", 
          local = intnumericnorm[(pt1 /. rule69)*(pt2 /. rule69)], 
          local = intnumericnorm[(pt1 /. rule69)/(pt2 /. rule69)]]; 
         Return[local]; ]; If[type == "quotient", sgn = "/", sgn = "*"]; 
      ptc = intnumericnorm[pt /. rule69]; 
      cx = (ff[ptc, #1] & ) /@ ETCBaryNorm; 
      cx = Union[AssociationThread[(StringJoin["C", #1] & ) /@ Keys[cx], 
         Values[cx]], ETCBaryNorm]; 
      cx = SortBy[Select[cx, Im[#1[[1]]] == 0 & ], #1[[1]] & ]; 
      prev = Association["X0" -> {-1, -1, -1}]; res = {}; 
      Do[If[(StringTake[Keys[prev][[1]], 1] == "C" && StringTake[n, 1] == 
            "C") || (StringTake[Keys[prev][[1]], 1] != "C" && 
           StringTake[n, 1] != "C"), prev = Association[n -> cx[n]]; 
          Continue[]]; If[coincide[cx[n], prev[[1]], 15], 
         idx1 = If[StringTake[Keys[prev][[1]], 1] == "C", 
            StringTake[Keys[prev][[1]], {2, -1}], Keys[prev][[1]]]; 
          idx2 = If[StringTake[n, 1] == "C", StringTake[n, {2, -1}], n]; 
          If[type == "quotient" && Simplify[Det[{{1, 1, 1}, pt, 
                KimberlingCenterCNy[idx1]/KimberlingCenterCNy[idx2]}]] == 0, 
           AppendTo[res, {idx1, idx2}]]; If[type == "quotient" && 
            Simplify[Det[{{1, 1, 1}, pt, KimberlingCenterCNy[idx2]/
                 KimberlingCenterCNy[idx1]}]] == 0, AppendTo[res, 
            {idx2, idx1}]]; If[type == "product" && 
            Simplify[Det[{{1, 1, 1}, pt, KimberlingCenterCNy[idx1]*
                 KimberlingCenterCNy[idx2]}]] == 0, 
           If[LexicographicOrder[idx1, idx2] == 1, AppendTo[res, 
              {idx1, idx2}], AppendTo[res, {idx2, idx1}]]; ]; ]; 
        prev = Association[n -> cx[n]]; , {n, Keys[cx]}]; 
      res = SortBy[DeleteDuplicates[res], numsortexpr[#1[[1]]] & ]; 
      res = ({intnameformat[#1[[1]]], intnameformat[#1[[2]]]} & ) /@ res; 
      AssociateTo[globalProperties[name], 
       {StringJoin["barycentric ", type] -> res}]; If[ !TrueQ[globalSilence], 
       If[Length[res] > 0, Print[colorformat[StringJoin["= barycentric ", 
            type, " X(i)", sgn, "X(j) for these (i, j): ", StringRiffle[res, 
             ", "]]]]; ]]; ]
 
checkTrilinearPolar[pt_, name_:"X"] := Module[{cx, ptc, p1, p2}, 
     ptc = N[Normalize[pt /. rule69], 35]; cx = bTripolarEq[ptc] . {x, y, z}; 
      dset = (Abs[cx] /. Thread[{x, y, z} -> #1] & ) /@ ETCBaryNorm; 
      test = Select[dset, #1 < 10^(-10) & ]; If[Length[test] > 1, 
       p1 = Keys[test][[1]]; p2 = Keys[test][[2]]; 
        If[Simplify[Det[{{1, 1, 1}, pt, bTripole[KimberlingCenterCNy[p1], 
              KimberlingCenterCNy[p2]]}]] == 0, 
         AssociateTo[globalProperties[name], {"trilinear polar" -> 
             {p1, p2}}]; If[ !TrueQ[globalSilence], 
           Print[colorformat[StringJoin["= trilinear pole of line {", 
              intnameformat[p1], ",", intnameformat[p2], "}"]]]]; ]; ]; ]
 
checkVertexConjugates[pt_, name_:"X"] := 
    Module[{cx, prev, res, idx1, idx2, ptc, rc, ffvertexconjugate}, 
     ffvertexconjugate[pt1_, pt2_] := Module[{local}, 
        local = bVertexConjugate[intnumericnorm[pt1 /. rule69], 
            intnumericnorm[pt2 /. rule69]] /. rule69; 
         Return[intnumericnorm[local]]; ]; rc = intCheckList[[1]]; 
      ptc = intnumericnorm[pt /. rule69]; 
      cx = (ffvertexconjugate[ptc, #1] & ) /@ ETCBaryNorm; 
      cx = Union[AssociationThread[(StringJoin["C", #1] & ) /@ Keys[cx], 
         Values[cx]], ETCBaryNorm]; 
      cx = SortBy[Select[cx, Im[#1[[1]]] == 0 & ], #1[[1]] & ]; 
      prev = Association["X0" -> {-1, -1, -1}]; res = {}; 
      Do[If[(StringTake[Keys[prev][[1]], 1] == "C" && StringTake[n, 1] == 
            "C") || (StringTake[Keys[prev][[1]], 1] != "C" && 
           StringTake[n, 1] != "C"), prev = Association[n -> cx[n]]; 
          Continue[]]; If[coincide[cx[n], prev[[1]], 15], 
         idx1 = If[StringTake[Keys[prev][[1]], 1] == "C", 
            StringTake[Keys[prev][[1]], {2, -1}], Keys[prev][[1]]]; 
          idx2 = If[StringTake[n, 1] == "C", StringTake[n, {2, -1}], n]; 
          If[coincideNorm[pt /. rc, bVertexConjugate[KimberlingCenterCNy[
               idx1], KimberlingCenterCNy[idx2]] /. rc], 
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
      If[num != 0, chk = {1}, chk = checkPointinETC[pt]]; 
      If[chk[[1]] < 10^(-12), Print[colorformat[StringJoin["ETC: ", 
          Keys[chk]]]], barys = Factor[FactorTermsList[expr[[1]]][[2]]]; 
        If[StringLength[inname] == 0, name = ToString[ExpressionToTrad[
            expr[[1]]]], name = inname]; AssociateTo[globalProperties, 
         name -> Association[]]; If[full, lines = 
          Quiet[linesProcessAlg[ptcoord, barys, 20, False, False, name]], 
         lines = Quiet[linesProcessAlg[ptcoord, barys, 20, False, True, 
             name]]; ]; If[full || Length[lines] > 3, 
         Quiet[checkCircumconics[ptcoord, 1, 60, num, name]]; 
          Quiet[checkCurves[ptcoord, name]]; Quiet[checkTrilinearPolar[
            ptcoord, name]]; Quiet[checkIsogonalConjugates[ptcoord, name]]; 
          Quiet[checkConjugates[ptcoord, bDaoConjugate, 
            "= X(i)-Dao conjugate of X(j) for these {i, j}: ", name]]; 
          Quiet[checkConjugates[ptcoord, bZayinConjugate, 
            "= X(i)-Zayin conjugate of X(j) for these {i, j}: ", name]]; 
          Quiet[checkConjugates[ptcoord, bCevianQuotient, 
            "= X(i)-Ceva conjugate of X(j) for these {i, j}: ", name]]; 
          Quiet[checkConjugates[ptcoord, bAnticomplementaryConjugate, 
            "= X(i)-anticomplementary conjugate of X(j) for these {i, j}: ", 
            name]]; Quiet[checkConjugates[ptcoord, bComplementaryConjugate, 
            "= X(i)-complementary conjugate of X(j) for these {i, j}: ", 
            name]]; Quiet[checkConjugates[ptcoord, bCrossConjugate, 
            "= X(i)-cross conjugate of X(j) for these {i, j}: ", name]]; 
          Quiet[checkConjugates[ptcoord, bWawConjugate, 
            "= X(i)-Waw conjugate of X(j) for these {i, j}: ", name]]; 
          Quiet[checkVertexConjugates[ptcoord, name]]; 
          Quiet[checkBarycentric[ptcoord, "product", name]]; 
          Quiet[checkBarycentric[ptcoord, "quotient", name]]; 
          Quiet[checkPerspector[ptcoord, name]]; TimeConstrained[
           Quiet[pointCheckAllProcesses[ptcoord, name]], 90]; ]; ]; ]
 
globalSeenPoints = {}
 
checkCurves[pt_, inname_:"X"] := Module[{out, ptest, d, secondcheck}, 
     out = {}; Do[ptest = N[NormalizeBary[(evaluate /. rule69)[pt]], 35]; 
        d = getTriangleCurve[name] /. Thread[{x, y, z} -> ptest] /. rule69; 
        If[Abs[d] < 10^(-12), secondcheck = True; 
          Do[ptest = N[NormalizeBary[pt /. rc], 35]; 
            d = getTriangleCurve[name] /. Thread[{x, y, z} -> ptest] /. rc; 
            If[Abs[d] > 10^(-12), secondcheck = False]; , 
           {rc, intCheckList}]; If[secondcheck, AppendTo[out, name]]; ]; , 
       {name, Keys[TriangleCurves]}]; AssociateTo[globalProperties[inname], 
       {"curves" -> out}]; If[Length[out] > 0, 
       If[ !TrueQ[globalSilence], Print[StringJoin["Lies on curves: ", 
           StringRiffle[out, ", "]]]]; ]; ]
 
checkConjugates[pt_, func_, str_, name_:"X"] := 
    Module[{cx, prev, res, idx1, idx2, i1, i2, ptc, rc, ff, fncname}, 
     ff[pt1_, pt2_] := Module[{local}, 
        local = func[intnumericnorm[pt1 /. rule69], intnumericnorm[
             pt2 /. rule69]] /. rule69; Return[intnumericnorm[local]]; ]; 
      rc = intCheckList[[1]]; ptc = intnumericnorm[pt /. rule69]; 
      cx = (ff[#1, ptc] & ) /@ ETCBaryNorm; 
      cx = Union[AssociationThread[(StringJoin["C", #1] & ) /@ Keys[cx], 
         Values[cx]], ETCBaryNorm]; 
      cx = SortBy[Select[cx, Im[#1[[1]]] == 0 & ], #1[[1]] & ]; 
      prev = Association["X0" -> {-1, -1, -1}]; res = {}; 
      Do[If[(StringTake[Keys[prev][[1]], 1] == "C" && StringTake[n, 1] == 
            "C") || (StringTake[Keys[prev][[1]], 1] != "C" && 
           StringTake[n, 1] != "C"), prev = Association[n -> cx[n]]; 
          Continue[]]; If[coincide[cx[n], prev[[1]]], 
         idx1 = If[StringTake[Keys[prev][[1]], 1] == "C", 
            StringTake[Keys[prev][[1]], {2, -1}], Keys[prev][[1]]]; 
          idx2 = If[StringTake[n, 1] == "C", StringTake[n, {2, -1}], n]; 
          If[StringTake[n, 1] != "C", {i1, i2} = {idx1, idx2}, 
           {i1, i2} = {idx2, idx1}]; If[coincideNorm[pt /. rc, 
            func[KimberlingCenterCNy[i1] /. rc, KimberlingCenterCNy[i2] /. 
               rc] /. rc], AppendTo[res, {i1, i2}]; ]]; 
        prev = Association[n -> cx[n]]; , {n, Keys[cx]}]; 
      res = SortBy[DeleteDuplicates[res], numsortexpr[#1[[1]]] & ]; 
      res = ({intnameformat[#1[[1]]], intnameformat[#1[[2]]]} & ) /@ res; 
      SetAttributes[fncname, HoldFirst]; fncname[x_] := 
       {SymbolName[Unevaluated[x]], x}; AssociateTo[globalProperties[name], 
       {ToString[First[fncname[func]]] -> res}]; If[ !TrueQ[globalSilence], 
       If[Length[res] > 0, Print[colorformat[StringJoin[str, 
            StringRiffle[res, ", "]]]]; ]]; ]
 
checkPerspector[pt_, inname_:"X"] := Module[{out, ptest, ptcheck, crv, set1, 
      rc, ptnamenew}, out = {}; ptest = intnumericnorm[
        evaluate[pt] /. rule69]; crv = bCircumconicPEq[ptest]; 
      set1 = checkPointsOnCurve[crv]; rc = intCheckList[[1]]; 
      Do[ptest = intnumericnorm[evaluate[pt] /. rc]; 
        crv = bCircumconicPEq[ptest]; ptnamenew = StringJoin[
          StringTake[ptoncrv, 1], StringTake[ptoncrv, {3, -2}]]; 
        ptcheck = N[KimberlingCenterCNy[ptnamenew] /. rc, 35]; 
        If[(crv /. Thread[{x, y, z} -> ptcheck]) < 10^(-15), 
         AppendTo[out, ptoncrv]], {ptoncrv, set1}]; 
      AssociateTo[globalProperties[inname], {"perspector" -> out}]; 
      If[Length[out] >= 2, If[ !TrueQ[globalSilence], 
         Print[colorformat[StringJoin[
            "= perspector of circumconic through: ", StringRiffle[out, 
             ", "]]]]]; ]; ]
 
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
           False, procname]; If[TrueQ[globalSilence], printGlobalProperties[
            globalProperties, procname]]; ], {name, Keys[pointProcesses]}]; ]
 
printGlobalProperties[glob_, name_:""] := Module[{hg, cycle, localprops}, 
     If[StringLength[name] > 0, cycle = {name}, cycle = Keys[glob]]; 
      Do[If[ !MemberQ[Keys[glob[pt]], "name"], Continue[]]; Print[]; Print[]; 
        Print[pt]; Print[]; Print[glob[pt]["name"]]; Print[]; 
        Print[StringJoin["Barycentrics    ", glob[pt]["barycentrics"]]]; 
        Print[]; hg = If[ !MemberQ[Keys[glob[pt]], "circumconics"], {}, 
          glob[pt]["circumconics"]]; If[Length[hg] > 0, 
         Print[colorformat[StringJoin["lies on circumconics ", 
            StringRiffle[hg, ", "], " and on these lines: ", 
            StringRiffle[glob[pt]["lines"], ", "]]]], 
         Print[colorformat[StringJoin["lies on these lines: ", 
             StringRiffle[glob[pt]["lines"], ", "]]]]; ]; Print[]; 
        If[MemberQ[Keys[glob[pt]], "midpoints"], hg = glob[pt]["midpoints"]; 
          If[Length[hg] > 0, Print[colorformat[StringJoin[
               "= midpoint of X(i) in X(j) for these {i,j}: ", StringRiffle[
                SortBy[hg, numsortexpr[#1[[1]]] & ], ", "]]]]; ]; ]; 
        If[MemberQ[Keys[glob[pt]], "reflections"], 
         hg = glob[pt]["reflections"]; If[Length[hg] > 0, 
           Print[colorformat[StringJoin[
               "= reflection of X(i) in X(j) for these {i,j}: ", StringRiffle[
                SortBy[hg, numsortexpr[#1[[1]]] & ], ", "]]]]; ]; ]; 
        If[ !MemberQ[Keys[glob[pt]], "circumconics"], 
         If[MemberQ[Keys[glob[pt]], "harmonic"], hg = glob[pt]["harmonic"]; 
            If[Length[hg] > 0, Print[colorformat[StringJoin[
                 "= {X(i),X(j)}-harmonic conjugate of X(k) for these (i,j,k): \
", StringRiffle[SortBy[hg, numsortexpr[#1[[1]]] & ], ", "]]]]; ]; ]; 
          Continue[]; ]; hg = glob[pt]["curves"]; If[Length[hg] > 0, 
         Print[colorformat[StringJoin["lies on curves: ", StringRiffle[hg, 
              ", "]]]]; ]; Do[If[KeyExistsQ[glob[pt], proc], 
           Print[colorformat[StringJoin["= ", proc, " of ", glob[pt][
                proc]]]]; ]; , {proc, Keys[singlePointProcesses]}]; 
        If[KeyExistsQ[glob[pt], "trilinear polar"], 
         hg = glob[pt]["trilinear polar"]; If[Length[hg] > 0, 
           Print[colorformat[StringJoin["= trilinear pole of line {", 
               intnameformat[hg[[1]]], ",", intnameformat[hg[[2]]], 
               "}"]]]; ]; ]; hg = glob[pt]["perspector"]; 
        If[Length[hg] >= 2, Print[colorformat[StringJoin[
             "= perspector of circumconic through: ", StringRiffle[hg, 
              ", "]]]]; ]; localprops = Association["isoconjugate" -> 
           "= X(i)-isoconjugate-of-X(j) for these {i, j}: ", 
          "vertex conjugate" -> 
           "= X(i)-vertex conjugate of X(j) for these {i, j}: ", 
          "bDaoConjugate" -> 
           "= X(i)-Dao conjugate of X(j) for these {i, j}: ", 
          "bZayinConjugate" -> 
           "= X(i)-Zayin conjugate of X(j) for these {i, j}: ", 
          "bCevianQuotient" -> 
           "= X(i)-Ceva conjugate of X(j) for these {i, j}: ", 
          "bComplementaryConjugate" -> 
           "= X(i)-complementary conjugate of X(j) for these {i, j}: ", 
          "bAnticomplementaryConjugate" -> 
           "= X(i)-anticomplementary conjugate of X(j) for these {i, j}: ", 
          "bCrossConjugate" -> 
           "= X(i)-cross conjugate of X(j) for these {i, j}: ", 
          "bWawConjugate" -> 
           "= X(i)-Waw conjugate of X(j) for these {i, j}: ", 
          "barycentric product" -> 
           "= barycentric product X(i)*X(j) for these (i, j): ", 
          "barycentric quotient" -> 
           "= barycentric quotient X(i)/X(j) for these (i, j): ", 
          "harmonic" -> 
           "= {X(i),X(j)}-harmonic conjugate of X(k) for these (i,j,k): "]; 
        Do[hg = glob[pt][name2]; If[Length[hg] > 0, 
           Print[colorformat[StringJoin[localprops[name2], StringRiffle[hg, 
                ", "]]]]; ]; , {name2, Keys[localprops]}]; , {pt, cycle}]]
