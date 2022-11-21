pointProcessBary[expr_, prefix_] := Module[{pointsBary, pti, pta, str, name, 
      coords}, pointsBary = Association[]; 
      Do[pta = ETCBary[k]; If[AnyTrue[Im[pta], #1 > 0 & ], Continue[]]; 
        If[AnyTrue[pta, #1 === ComplexInfinity & ] || AnyTrue[pta, 
           #1 === Infinity & ], Continue[]]; 
        pti = N[ReleaseHold[expr /. #1 -> pta] /. rule69, 36]; 
        pti = pti*Sign[pti[[1]]]; If[Total[pti] != 0, pti = Normalize[pti], 
         If[pti[[1]] != 0, pti = pti/pti[[1]]]]; 
        If[pti[[1]] =!= Indeterminate, name = StringJoin[prefix, "_", k]; 
          AppendTo[pointsBary, name -> pti]; ], {k, Keys[ETCBary]}]; 
      Return[pointsBary]; ]
 
pointProcessBaryWriter[pointsBary_, prefix_] := 
    Module[{filename, str, coords}, {filename = StringJoin[prefix, ".txt"]; 
       str = OpenWrite[filename]; 
       Do[coords = StringJoin[ToString[DecimalForm[pointsBary[l][[1]], 30]], 
           ",", ToString[DecimalForm[pointsBary[l][[2]], 30]], ",", 
           ToString[DecimalForm[pointsBary[l][[3]], 30]]]; 
         WriteLine[str, StringJoin[l, ",", coords]]; , 
        {l, Keys[pointsBary]}]; Close[str]; }]
 
singlePointProcesses = <|"isogonal_conjugate" -> 
      Hold[bIsogonalConjugate[#1]], "isotomic_conjugate" -> 
      Hold[bIsotomicConjugate[#1]], "complement" -> 
      Hold[bComplement[KimberlingCenterC[2], #1]], 
     "anticomplement" -> Hold[bAntiComplement[KimberlingCenterC[2], #1]], 
     "cyclocevian_conjugate" -> Hold[bCyclocevianConjugate[#1]], 
     "polar_conjugate" -> Hold[bPIsogonalConjugate[KimberlingCenterC[48], 
        #1]], "tcc_perspector" -> Hold[bTCCPerspector[#1]], 
     "circumcircle_inverse" -> Hold[bCircumcircleInverse[#1]], 
     "circlecevian_perspector" -> Hold[bCirclecevianPerspector[#1]], 
     "zosma_transform" -> Hold[bZosmaTransform[#1]], 
     "antitomic_conjugate" -> Hold[bAntitomicConjugate[#1]], 
     "barycentric_square" -> Hold[#1^2], "eigentransform" -> 
      Hold[bEigentransform[#1]], "ortoassociate" -> 
      Hold[bOrthoassociate[#1]], "syngonal_conjugate" -> Hold[bSyngonal[#1]], 
     "1st_saragossa_point" -> Hold[bSaragossa1[#1]], 
     "2nd_saragossa_point" -> Hold[bSaragossa2[#1]], 
     "3rd_saragossa_point" -> Hold[bSaragossa3[#1]]|>
 
linesProcess[pt_, prec_:20] := Module[{tplist, tp, prev, outgroups, group, 
      dump}, tplist = {}; Do[tp = 1/bLine[pt, ETCBary[name]]; 
        If[Im[tp[[1]]] != 0, Continue[]]; AppendTo[tplist, 
         {name, Normalize[tp]*Sign[tp[[1]]]}]; , {name, Keys[ETCBary]}]; 
      tplist = SortBy[tplist, #1[[2]][[1]] & ]; prev = {"0", {0, 0, 0}}; 
      outgroups = {}; group = {}; 
      Do[If[Abs[el[[2]][[1]] - prev[[2]][[1]]] < 10^(-prec) && 
          Abs[el[[2]][[2]] - prev[[2]][[2]]] < 10^(-prec) && 
          Abs[el[[2]][[3]] - prev[[2]][[3]]] < 10^(-prec), 
         AppendTo[group, el]; If[Length[group] == 1, AppendTo[group, prev]; 
            dump = 1], If[dump == 1, AppendTo[outgroups, 
             Take[SortBy[group, ToExpression[StringTake[#1[[1]], 
                  {2, -1}]] & ], 2]]; group = {}; dump = 0]; ]; prev = el; , 
       {el, tplist}]; outgroups = SortBy[outgroups, 
        ToExpression[StringTake[#1[[1]][[1]], {2, -1}]] & ]; 
      Return[(StringJoin["{", StringTake[#1[[1]][[1]], {2, -1}], ",", 
          StringTake[#1[[2]][[1]], {2, -1}], "}"] & ) /@ outgroups]; ]
 
pointCheck[pt_, process_] := Module[{tmp, res, ptn}, 
     ptn = Normalize[pt]*Sign[pt[[1]]]; tmp = pointProcessBary[
        singlePointProcesses[process], process]; 
      res = MinimalBy[Value][(Abs[(#1[[1]] - ptn[[1]])^2 + 
            (#1[[2]] - ptn[[2]])^2 + (#1[[3]] - ptn[[3]])^2] & ) /@ tmp]; 
      If[res[[1]] < 10^(-20), Return[Keys[res][[1]]]]; ]
 
pointCheckAllProcesses[pt_] := Module[{res}, 
     Do[PrintTemporary[name]; res = pointCheck[pt, name]; 
        If[StringQ[res], Print[res]]; , {name, Keys[singlePointProcesses]}]; ]
