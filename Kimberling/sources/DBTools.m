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
     "tcc_perspector" -> Hold[bTCCPerspector[#1]], "circumcircle_inverse" -> 
      Hold[bCircumcircleInverse[#1]], "circlecevian_perspector" -> 
      Hold[bCirclecevianPerspector[#1]], "zosma_transform" -> 
      Hold[bZosmaTransform[#1]], "antitomic_conjugate" -> 
      Hold[bAntitomicConjugate[#1]], "barycentric_square" -> Hold[#1^2], 
     "eigentransform" -> Hold[bEigentransform[#1]], 
     "ortoassociate" -> Hold[bOrthoassociate[#1]], "syngonal_conjugate" -> 
      Hold[bSyngonal[#1]], "1st_saragossa_point" -> Hold[bSaragossa1[#1]], 
     "2nd_saragossa_point" -> Hold[bSaragossa2[#1]], 
     "3rd_saragossa_point" -> Hold[bSaragossa3[#1]]|>
 
intHarmonicProcess[fullgroups_, pt_, prec_] := 
    Module[{fgr1, res, checks, fg, flatfg2, ingroupnbary, un, hgroups, 
      hgroup, prev, dump}, hgroups = {}; 
      Do[fgr1 = Take[SortBy[set, ToExpression[StringTake[#1, {2, -1}]] & ], 
          Min[1000, Length[set]]]; flatfg2 = Subsets[fgr1, {2}]; 
        checks = AssociationMap[NormalizeBary[bHarmonicConjugate[
             ETCBaryNorm[#1[[1]]], ETCBaryNorm[#1[[2]]], pt]] & , flatfg2]; 
        ingroupnbary = KeySelect[ETCBaryNorm, MemberQ[fgr1, #1] & ]; 
        un = SortBy[Union[checks, ingroupnbary], #1[[1]] & ]; hgroup = {}; 
        prev = Association["X0" -> {0, 0, 0}]; dump = 0; 
        Do[If[Total[Abs[un[el] - prev[[1]]]] < 10^(-15), 
           AppendTo[hgroup, el]; If[Length[hgroup] == 1, 
             AppendTo[hgroup, Keys[prev][[1]]]; dump = 1], 
           If[dump == 1, If[Length[hgroup[[1]]] != Length[hgroup[[2]]], 
               AppendTo[hgroups, hgroup]]; hgroup = {}; dump = 0; ]; ]; 
          prev = Association[el -> un[el]]; , {el, Keys[un]}], 
       {set, fullgroups}]; Return[hgroups]; ]
 
pointCheck[pt_, process_] := Module[{tmp, res, ptn}, 
     ptn = Normalize[pt]*Sign[pt[[1]]]; tmp = pointProcessBary[
        singlePointProcesses[process], process]; 
      res = MinimalBy[Value][(Abs[(#1[[1]] - ptn[[1]])^2 + 
            (#1[[2]] - ptn[[2]])^2 + (#1[[3]] - ptn[[3]])^2] & ) /@ tmp]; 
      If[res[[1]] < 10^(-20), Return[Keys[res][[1]]]]; ]
 
pointCheckAllProcesses[pt_] := Module[{res}, 
     Do[res = pointCheck[pt /. rule69, name]; If[StringQ[res], Print[res]]; , 
       {name, Keys[singlePointProcesses]}]; ]
 
checkCircumconics[pt_, start_:1, time_:60, excl_:0] := 
    Module[{ptc, p1, p2, crv, dset, test}, TimeConstrained[
      ptc = Normalize[pt /. rule69]; 
       Do[crv = bFivePointConicEqABC[ptc, ETCBaryNorm[StringJoin["X", 
              ToString[nx]]]] /. rule69; 
         dset = (Abs[crv] /. Thread[{x, y, z} -> #1] & ) /@ ETCBaryNorm; 
         test = Select[dset, #1 < 10^(-10) & ]; If[Length[test] > 1, 
          p1 = ToExpression[StringTake[Keys[test][[1]], {2, -1}]]; 
           p2 = ToExpression[StringTake[Keys[test][[2]], {2, -1}]]; 
           If[p1 == excl || p2 == excl, Continue[]]; 
           If[Simplify[bFivePointConicEqABC[KimberlingCenterCN[p1], 
                KimberlingCenterCN[p2]] /. Thread[{x, y, z} -> pt]] == 0, 
            Print[StringJoin["{A,B,C,X(", ToString[p1], "),X(", ToString[p2], 
              ")}"]]]; ]; funcind = nx; , {nx, start, 
         start + Max[1, Floor[time/60]]*200}]; Return[funcind]; , time, 
      funcind]]
 
checkCrossConjugate[pt_, size_:1000] := Module[{res, scope, test}, 
     scope = Take[ETCBaryNorm, size]; test = N[Normalize[pt /. rule69], 30]; 
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
 
linesProcessAlg[ptcoord_, prec_:20] := Module[{res, gr, hg, out, head, test, 
      hgroups, ptc}, ptc = Normalize[ptcoord /. rule69]; 
      res = intLinesProcessFullGroups[ptc, prec]; 
      gr = (StringJoin["{", StringTake[#1[[1]][[1]], {2, -1}], ",", 
          StringTake[#1[[2]][[1]], {2, -1}], "}"] & ) /@ res[[1]]; out = {}; 
      Do[test = bCollinearityMatrix[KimberlingCenterC[ToExpression[el][[1]]], 
          KimberlingCenterC[ToExpression[el][[2]]], ptcoord]; 
        If[TrueQ[Simplify[test] == 0], AppendTo[out, el]]; , {el, gr}]; 
      Print["Lines"]; Print[ToString[out]]; hg = {}; 
      Do[head = Select[igroup, Length[#1] == 0 & ]; 
        Do[If[el == head[[1]], Continue[]]; AppendTo[hg, 
           (StringTake[#1, {2, -1}] & ) /@ Flatten[{el, head[[1]]}]], 
         {el, igroup}], {igroup, intHarmonicProcess[res[[2]], ptc, prec]}]; 
      Print["Harmonic groups"]; Print[ToString[SortBy[hg, 
         ToExpression[#1[[1]]] & ]]]; ]
 
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
 
ffbarycentricproduct[pt1_, pt2_] := Module[{local}, 
     local = (pt1 /. rule69)*(pt2 /. rule69); 
      Return[Normalize[local]*Sign[local[[1]]]]; ]
 
ffbarycentricquotient[pt1_, pt2_] := Module[{local}, 
     local = (pt1 /. rule69)/(pt2 /. rule69); 
      Return[Normalize[local]*Sign[local[[1]]]]; ]
 
checkIsogonalConjugates[pt_] := Module[{cx, prev, res, idx1, idx2}, 
     cx = (ffisoconjugate[pt, #1] & ) /@ ETCBaryNorm; 
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
          If[Simplify[Det[{{1, 1, 1}, pt, bPIsogonalConjugate[
                KimberlingCenterCN[idx1], KimberlingCenterCN[idx2]]}]] == 0, 
           If[idx1 < idx2, AppendTo[res, {idx1, idx2}], AppendTo[res, 
             {idx2, idx1}]], Print[idx1]; Print[idx2]; ]]; 
        prev = Association[n -> cx[n]]; , {n, Keys[cx]}]; 
      res = SortBy[DeleteDuplicates[res], #1[[1]] & ]; Print[ToString[res]]; ]
 
checkBarycentricQuotient[pt_] := Module[{cx, prev, res, idx1, idx2}, 
     cx = (ffbarycentricproduct[pt, #1] & ) /@ ETCBaryNorm; 
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
 
checkBarycentricProduct[pt_] := Module[{cx, prev, res, idx1, idx2}, 
     cx = (ffbarycentricquotient[pt, #1] & ) /@ ETCBaryNorm; 
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
