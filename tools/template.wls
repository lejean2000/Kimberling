#!/usr/bin/env wolframscript

$HistoryLength = 0;

If[$OperatingSystem == "Windows",
Get["../sources/initwin.wls"],
Get["../sources/init.wls"]
];

STARTETCNUMBER = 80000;

If[FileExistsQ["globalptnames.mx"],
Get["globalptnames.mx"];
Get["globalptdescr.mx"];
Get["globalptexpr.mx"];
Get["globalptbary.mx"];
,
globalptnames = Association[];
globalptdescr = Association[];
globalptexpr = Association[];
globalptbary = Association[];
];


(* Add Extra points to ETC*)
Do[
  AppendTo[ETCFull, ptname -> ETCExtra[ptname]];
  AppendTo[ETCBaryNormFull, ptname -> ETCExtraBary[ptname]];
  , {ptname, Keys[ETCExtra]}
];

(* Add Extra points to ETC*)
Do[
  AppendTo[ETCFull, ptname -> globalptexpr[ptname]];
  AppendTo[ETCBaryNormFull, ptname -> globalptbary[ptname]];
  , {ptname, Keys[globalptexpr]}
];

(* Add Extra points to ETC*)
Do[
  AppendTo[ETC, ptname -> globalptexpr[ptname]];
  AppendTo[ETCBaryNorm, ptname -> globalptbary[ptname]];
  , {ptname, Keys[globalptexpr]}
];

singlePointProcesses = Association[
  "isogonal conjugate" -> {a^2 v w, b^2 u w, c^2 u v},
  "isotomic conjugate" -> {v w, u w, u v},
  "complement" -> {v + w, u + w, u + v},
  "anticomplement" -> {-u + v + w, u - v + w, u + v - w}
];

globalOutputStream = False;
pointCheckerMinProperties = 10;


quickloop[ptnum_, prcoordexpr_, set_, topname_] := Module[{yy, xname, ptcoord, start, ptnametop},

  start = ptnum;
  Do[
   ptnametop = "";

   ptcoord = TimeConstrained[prcoordexpr/.Thread[pP -> KimberlingCenterC[nx]]//smpl,10,-1];
   If[ptcoord == -1, Continue[]];

   globalNoCleanup = True;
   
   ptnametop = StringReplace[topname, "XX" -> intaddbrackets["X"<>ToString[nx]]];
   If[StringLength[ptnametop]>0, Print[];Print["======="<>ptnametop<>"=======1"]];
   
   Print[];
   
   ptchk = TimeConstrained[quickChecker[ptcoord,0,False],60,-1];
   (*If[Total[ptchk]>=2,crvsnum=checkCurves[ptcoord] // Quiet // Length;,crvsnum=0];*)
   
   totalnum = Total[ptchk];
   
   If[
	   totalnum>=pointCheckerMinProperties,
	   
	   xname = "X" <> ToString[start];

	   AppendTo[ETC, xname->ptcoord[[1]]];
	   yy=intnumericnorm[evaluate[symmetrizeInternal[ETC[xname]]]/.rule69];
	   AppendTo[ETCBaryNorm, xname->yy];
	   AppendTo[ETCFull, xname->ptcoord[[1]]];
	   AppendTo[ETCBaryNormFull, xname->yy];
	   
	   AppendTo[globalptnames,xname->ptnametop];
	   AppendTo[globalptdescr,xname->totalnum];
	   AppendTo[globalptexpr, xname->ptcoord[[1]]];
	   AppendTo[globalptbary, xname->yy];
	   
	   start = start + 1;

	   Print["======="<>ToString[ptchk]<>"======="];Print[xname];
   ];
   ,
   {nx, set}
   ]; 
	DumpSave["globalptnames.mx", globalptnames];
	DumpSave["globalptdescr.mx", globalptdescr];
	DumpSave["globalptexpr.mx", globalptexpr];
	DumpSave["globalptbary.mx", globalptbary];

  Return[start-1];
];


endnum = Max[STARTETCNUMBER-1,Max[ToExpression[
  StringTake[
   SortBy[Keys[KeySelect[globalptnames, StringStartsQ[#, "X"] &]], 
    numsortexpr[#] &], {2, -1}]]]];


pers = {-p u + q v + r w, p u - q v + r w, p u + q v - r w};
 

Do[

Print[num];

expr = pers /. Thread[pQ -> X[num]] // simplifyRationalBarycentrics;
set = massHeuristics1[pers /. Thread[pQ -> X[num]], 1, 170][[1]];

Print[set];

endnum = quickloop[1+endnum, expr, set, "antiproduct of of X("<>ToString[num]<>") and XX"]; 
,{num,{1,2,3,4,6,7,8,9,10,11,12,21,31,37,75,76,81,86}}
];

