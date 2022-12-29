pointProcessBary[expr_, prefix_] := Module[{pointsBary, pti, pta, str, name, 
      coords}, pointsBary = Association[]; 
      Do[pta = ETCBaryNorm[k]; If[AnyTrue[Im[pta], #1 > 0 & ], Continue[]]; 
        If[AnyTrue[pta, #1 === ComplexInfinity & ] || AnyTrue[pta, 
           #1 === Infinity & ], Continue[]]; 
        pti = N[ReleaseHold[expr /. #1 -> pta] /. rule69, 36]; 
        pti = pti*Sign[pti[[1]]]; If[Total[pti] != 0, pti = Normalize[pti], 
         If[pti[[1]] != 0, pti = pti/pti[[1]]]]; 
        If[pti[[1]] =!= Indeterminate, name = StringJoin[prefix, "_", k]; 
          AppendTo[pointsBary, name -> pti]; ], {k, Keys[ETCBaryNorm]}]; 
      Return[pointsBary]; ]
 
pointProcessBaryWriter[pointsBary_, prefix_] := 
    Module[{filename, str, coords}, {filename = StringJoin[prefix, ".txt"]; 
       str = OpenWrite[filename]; 
       Do[coords = StringJoin[ToString[DecimalForm[pointsBary[l][[1]], 30]], 
           ",", ToString[DecimalForm[pointsBary[l][[2]], 30]], ",", 
           ToString[DecimalForm[pointsBary[l][[3]], 30]]]; 
         WriteLine[str, StringJoin[l, ",", coords]]; , 
        {l, Keys[pointsBary]}]; Close[str]; }]
 
singlePointProcesses = <|"complement" -> 
      Hold[bComplement[KimberlingCenterC[2], #1]], 
     "anticomplement" -> Hold[bAntiComplement[KimberlingCenterC[2], #1]], 
     "cyclocevian_conjugate" -> Hold[bCyclocevianConjugate[#1]], 
     "circumcircle_inverse" -> Hold[bCircumcircleInverse[#1]], 
     "circlecevian_perspector" -> Hold[bCirclecevianPerspector[#1]], 
     "zosma_transform" -> Hold[bZosmaTransform[#1]], 
     "antitomic_conjugate" -> Hold[bAntitomicConjugate[#1]], 
     "barycentric_square" -> Hold[#1^2], 
     "anticomplement of isogonal conjugate" -> 
      Hold[bAntiComplement[KimberlingCenterC[2], bIsogonalConjugate[#1]]], 
     "anticomplement of isotomic conjugate" -> 
      Hold[bAntiComplement[KimberlingCenterC[2], bIsotomicConjugate[#1]]], 
     "complement of isogonal conjugate" -> 
      Hold[bComplement[KimberlingCenterC[2], bIsogonalConjugate[#1]]], 
     "complement of isotomic conjugate" -> 
      Hold[bComplement[KimberlingCenterC[2], bIsotomicConjugate[#1]]], 
     "tcc_perspector" -> Hold[bTCCPerspector[#1]], 
     "eigentransform" -> Hold[bEigentransform[#1]], 
     "ortoassociate" -> Hold[bOrthoassociate[#1]], "syngonal_conjugate" -> 
      Hold[bSyngonal[#1]], "1st_saragossa_point" -> Hold[bSaragossa1[#1]], 
     "2nd_saragossa_point" -> Hold[bSaragossa2[#1]], 
     "3rd_saragossa_point" -> Hold[bSaragossa3[#1]]|>
 
intHarmonicProcess[fullgroups_, pt_, prec_] := 
    Module[{fgr1, checks, flatfg2, ingroupnbary, un, hgroups, hgroup, prev, 
      dump}, hgroups = {}; 
      Do[fgr1 = SortBy[set, ToExpression[StringTake[#1, {2, -1}]] & ]; 
        If[Length[fgr1] > 2000, Print[StringJoin["Found lines with ", 
            ToString[Length[fgr1]], " points"]]; fgr1 = Take[fgr1, 2000]; ]; 
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
 
pointCheck[pt_, process_] := Module[{tmp, res, ptn}, 
     ptn = N[NormalizeBary[pt], 35]; tmp = pointProcessBary[
        singlePointProcesses[process], process]; 
      res = MinimalBy[Value][(Abs[(#1[[1]] - ptn[[1]])^2 + 
            (#1[[2]] - ptn[[2]])^2 + (#1[[3]] - ptn[[3]])^2] & ) /@ tmp]; 
      If[res[[1]] < 10^(-20), Return[Keys[res][[1]]]]; ]
 
pointCheckAllProcesses[pt_] := Module[{res}, 
     Do[res = pointCheck[pt /. rule69, name]; If[StringQ[res], Print[res]]; , 
       {name, Keys[singlePointProcesses]}]; ]
 
checkCircumconics[pt_, start_:1, time_:60, excl_:0] := 
    Module[{ptc, p1, p2, crv, dset, test, out, conicname}, 
     TimeConstrained[out = {}; ptc = N[Normalize[pt /. rule69], 35]; 
       Do[funcind = nx; crv = N[bFivePointConicEq[{1, 0, 0}, {0, 1, 0}, 
             {0, 0, 1}, ptc, ETCBaryNorm[StringJoin["X", ToString[nx]]]] /. 
            rule69, 35]; dset = (Abs[crv] /. Thread[{x, y, z} -> #1] & ) /@ 
           ETCBaryNorm; test = Select[dset, #1 < 10^(-10) & ]; 
         If[Length[test] > 1, p1 = ToExpression[StringTake[Keys[test][[1]], 
              {2, -1}]]; p2 = ToExpression[StringTake[Keys[test][[2]], 
              {2, -1}]]; If[p1 == excl || p2 == excl, Continue[]]; 
           If[Simplify[bFivePointConicEq[{1, 0, 0}, {0, 1, 0}, {0, 0, 1}, 
                KimberlingCenterCN[p1], KimberlingCenterCN[p2]] /. Thread[
                {x, y, z} -> pt]] == 0, conicname = StringJoin["{A,B,C,X(", 
               ToString[p1], "),X(", ToString[p2], ")}"]; 
             If[ !MemberQ[out, conicname], AppendTo[out, conicname]; Print[
                conicname]]]; ]; , {nx, start, start + Max[1, Floor[time/60]]*
           200}]; Return[funcind]; , time, funcind]]
 
checkCrossConjugate[pt_, size_:1000] := Module[{res, scope, test}, 
     scope = Take[ETCBaryNorm, size]; test = N[Normalize[pt /. rule69], 35]; 
      Monitor[Do[res = Select[scope, Norm[ffcrossconjugate[#1, scope[i]] - 
               test*Sign[test[[1]]]] < 10^(-15) & , 1]; If[Length[res] > 0, 
          Print[StringJoin["(", StringTake[i, {2, -1}], ",", 
            StringTake[Keys[res][[1]], {2, -1}], ")"]]], {i, Keys[scope]}], 
       i]]
 
ffcrossconjugate[var_, ptx_] := Module[{local}, 
     local = bCrossConjugate[ptx, var]; Return[Normalize[local]*
        Sign[local[[1]]]]; ]
 
ffisoconjugate[pt1_, pt2_] := Module[{local}, 
     local = bPIsogonalConjugate[pt1 /. rule69, pt2 /. rule69] /. rule69; 
      Return[Normalize[local]*Sign[local[[1]]]]; ]
 
linesProcessAlg[ptcoord_, prec_:20, debug_:False] := 
    Module[{res, gr, hg, out, head, test, test2, hgroups, ptc, unproven, rc, 
      rc2, eltest}, rc = {a -> 5, b -> 6, c -> 7}; 
      rc2 = {a -> 4, b -> 11, c -> 13}; 
      ptc = N[NormalizeBary[ptcoord /. rule69], 35]; 
      res = intLinesProcessFullGroups[ptc, prec]; 
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
         AppendTo[unproven, el]]; , {el, gr}]; Print["Lines"]; 
      Print[ToString[out]]; hg = {}; 
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
      Print["Harmonic groups"]; Print[ToString[SortBy[hg, 
         ToExpression[#1[[1]]] & ]]]; hg = {}; 
      Do[eltest = (StringTake[#1, {2, -1}] & ) /@ igroup; 
        test = Det[{{1, 1, 1}, bMidpoint[KimberlingCenterCN[eltest[[1]]] /. 
             rc, KimberlingCenterCN[eltest[[2]]] /. rc], ptcoord /. rc}]; 
        test2 = Det[{{1, 1, 1}, bMidpoint[KimberlingCenterCN[eltest[[1]]] /. 
             rc2, KimberlingCenterCN[eltest[[2]]] /. rc2], ptcoord /. rc2}]; 
        If[TrueQ[Simplify[test] == 0] && TrueQ[Simplify[test2] == 0], 
         AppendTo[hg, eltest]; ]; , {igroup, intMidpointProcess[res[[2]], 
         ptc, prec]}]; Print["Midpoints"]; 
      Print[ToString[SortBy[hg, ToExpression[#1[[1]]] & ]]]; hg = {}; 
      Do[eltest = (StringTake[#1, {2, -1}] & ) /@ igroup; 
        test = Det[{{1, 1, 1}, bReflectionPP[KimberlingCenterCN[
              eltest[[1]]] /. rc, KimberlingCenterCN[eltest[[2]]] /. rc], 
           ptcoord /. rc}]; test2 = Det[{{1, 1, 1}, bReflectionPP[
            KimberlingCenterCN[eltest[[1]]] /. rc2, 
            KimberlingCenterCN[eltest[[2]]] /. rc2], ptcoord /. rc2}]; 
        If[TrueQ[Simplify[test] == 0] && TrueQ[Simplify[test2] == 0], 
         AppendTo[hg, eltest]; ]; , {igroup, intReflectionProcess[res[[2]], 
         ptc, prec]}]; Print["Reflections"]; 
      Print[ToString[SortBy[hg, ToExpression[#1[[1]]] & ]]]; ]
 
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
        If[Length[fgr1] > 2000, Print[Length[fgr1]]; 
          fgr1 = Take[fgr1, 2000]; ]; flatfg2 = Subsets[fgr1, {2}]; 
        ingroupnbary = AssociationMap[
          Abs[NormalizeBary[bMidpoint[ETCBaryNorm[#1[[1]]], ETCBaryNorm[
                #1[[2]]]]] - pt] & , flatfg2]; 
        hgroup = Select[ingroupnbary, #1[[1]] < 10^(-prec) && 
            #1[[2]] < 10^(-prec) && #1[[3]] < 10^(-prec) & ]; 
        If[Length[hgroup] > 0, hgroups = Union[hgroups, Keys[hgroup]]]; , 
       {set, fullgroups}]; Return[hgroups]; ]
 
intReflectionProcess[fullgroups_, pt_, prec_] := 
    Module[{fgr1, checks, flatfg2, ingroupnbary, un, hgroups, hgroup, prev, 
      dump}, hgroups = {}; Do[fgr1 = SortBy[set, xnum[#1] & ]; 
        If[Length[fgr1] > 2000, Print[StringJoin["Found lines with ", 
            ToString[Length[fgr1]], " points"]]; fgr1 = Take[fgr1, 2000]; ]; 
        checks = AssociationMap[NormalizeBary[bMidpoint[ETCBaryNorm[#1], 
             pt]] & , fgr1]; 
        Do[refl = Select[fgr1, coincide[ETCBaryNorm[#1], checks[el]] & ]; 
          If[Length[refl] > 0, AppendTo[hgroups, {el, First[refl]}]], 
         {el, Keys[checks]}]; , {set, fullgroups}]; Return[hgroups]; ]
 
xnum[str_] := ToExpression[StringTake[str, {2, -1}]]
 
coincide[pt1_, pt2_, prec_:20] := Abs[pt1[[1]] - pt2[[1]]] < 10^(-prec) && 
     Abs[pt1[[2]] - pt2[[2]]] < 10^(-prec) && Abs[pt1[[3]] - pt2[[3]]] < 
      10^(-prec)
 
ffbarycentricproduct[pt1_, pt2_] := Module[{local}, 
     local = (pt1 /. rule69)*(pt2 /. rule69); 
      Return[Normalize[local]*Sign[local[[1]]]]; ]
 
ffbarycentricquotient[pt1_, pt2_] := Module[{local}, 
     local = (pt1 /. rule69)/(pt2 /. rule69); 
      Return[Normalize[local]*Sign[local[[1]]]]; ]
 
ffanticomplconjugate[pt1_, pt2_] := Module[{local}, 
     local = bAnticomplementaryConjugate[pt1 /. rule69, pt2 /. rule69] /. 
        rule69; Return[Normalize[local]*Sign[local[[1]]]]; ]
 
checkIsogonalConjugates[pt_] := Module[{cx, prev, res, idx1, idx2, ptc, rc}, 
     rc = {a -> 5, b -> 6, c -> 7}; ptc = N[NormalizeBary[pt /. rule69], 35]; 
      cx = (ffisoconjugate[ptc, #1] & ) /@ ETCBaryNorm; 
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
          If[Det[{{1, 1, 1}, pt /. rc, bPIsogonalConjugate[KimberlingCenterCN[
                 idx1], KimberlingCenterCN[idx2]] /. rc}] == 0, 
           If[idx1 < idx2, AppendTo[res, {idx1, idx2}], AppendTo[res, 
             {idx2, idx1}]], Print[idx1]; Print[idx2]; ]]; 
        prev = Association[n -> cx[n]]; , {n, Keys[cx]}]; 
      res = SortBy[DeleteDuplicates[res], #1[[1]] & ]; Print[ToString[res]]; ]
 
checkBarycentricQuotient[pt_] := Module[{cx, prev, res, idx1, idx2, ptc}, 
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
      Print[ToString[res]]; ]
 
checkBarycentricProduct[pt_] := Module[{cx, prev, res, idx1, idx2, ptc}, 
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
            AppendTo[res, {idx1, idx2}], AppendTo[res, {idx2, idx1}]], 
           Print[idx1]; Print[idx2]; ]]; prev = Association[n -> cx[n]]; , 
       {n, Keys[cx]}]; res = SortBy[DeleteDuplicates[res], #1[[1]] & ]; 
      Print[ToString[res]]; ]
 
checkAnticomplementaryConjugates[pt_] := 
    Module[{cx, prev, res, idx1, idx2, i1, i2, ptc, rc}, 
     rc = {a -> 5, b -> 6, c -> 7}; ptc = N[NormalizeBary[pt /. rule69], 35]; 
      cx = (ffanticomplconjugate[#1, ptc] & ) /@ ETCBaryNorm; 
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
          If[StringTake[n, 1] == "X", {i1, i2} = {idx1, idx2}, 
           {i1, i2} = {idx2, idx1}]; If[Det[{{1, 1, 1}, pt /. rc, 
              bAnticomplementaryConjugate[KimberlingCenterCN[i1] /. rc, 
                KimberlingCenterCN[i2] /. rc] /. rc}] == 0, 
           AppendTo[res, {i1, i2}]; ]]; prev = Association[n -> cx[n]]; , 
       {n, Keys[cx]}]; res = SortBy[DeleteDuplicates[res], #1[[1]] & ]; 
      Print[ToString[res]]; ]
