#!/usr/bin/env wolframscript
(* ::Package:: *)

If[$OperatingSystem == "Windows",
Get["../sources/initwin.wls"],
Get["../sources/init.wls"]
];

Get["../db/ETCExtra.mx"];
Get["../db/ETCExtraBary.mx"];


STARTETCNUMBER = 70000;

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

ETCFull = ETC;
ETCBaryNormFull = ETCBaryNorm;

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

singlePointProcesses = Association[
  "isogonal conjugate" -> {a^2 v w, b^2 u w, c^2 u v},
  "isotomic conjugate" -> {v w, u w, u v},
  "complement" -> {v + w, u + w, u + v},
  "anticomplement" -> {-u + v + w, u - v + w, u + v - w}
];

globalOutputStream = False;

addpoint[ptnum_, ptcoord_, topname_] := Module[{yy, xname, start, ptnametop, totalnum},
  pointCheckerMinProperties = 10;
  start = ptnum;

   ptnametop = topname;

   globalNoCleanup = True;
   
   Print[];
   
   ptchk = TimeConstrained[quickChecker[ptcoord,0,False],30,-1];
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
  
  Print[];
  Return[start-1];
];



endnum = Max[STARTETCNUMBER-1,Max[ToExpression[
  StringTake[
   SortBy[Keys[KeySelect[globalptnames, StringStartsQ[#, "X"] &]], 
    numsortexpr[#] &], {2, -1}]]]];


trchk=symmetrizeTriangleExprType2Bary[{a^2 - 2 (b - c)^2 + a (b + c), b (-a + b - c), -(a + b - c) c}];

trchk=inv[triangle["Aguilera"]];

Get["../db/UNCFTRG.mx"];
Get["../db/CPTR.mx"];

allsets = <|"KimberlingTrianglesBary"->KimberlingTrianglesBary, "CPTR"->CPTR, "UNCFTRG"->UNCFTRG|>;

trgname = "anti-Aguilera";

Do[
	Do[
		trsym=If[ListQ[allsets[setname][trname][[1]]],allsets[setname][trname],triangle[trname]];
		ntest=bIsPerspective@@(evaluate[Join[trsym,trchk]]/.rule69)/.rule69;
		If[Abs[ntest]<10^-24,
			ptcoord=TimeConstrained[bTrianglePerspector[trchk,trsym]//simplifyRationalBarycentrics//partialSReplace, 60, -1];
			perschk=ptcoord//checkPointinETC2;
			outname = "Perspector of these triangles: "<>trgname<>" and "<>trname;
			Print[outname];
			If[Length[perschk] == 0, endnum = addpoint[1 + endnum, ptcoord, outname] // Quiet;,Print[perschk]];
		];
	,{trname,Keys[allsets[setname]]}
	];

	DumpSave["globalptnames.mx", globalptnames];
	DumpSave["globalptdescr.mx", globalptdescr];
	DumpSave["globalptexpr.mx", globalptexpr];
	DumpSave["globalptbary.mx", globalptbary];


	Do[
		trsym=If[ListQ[allsets[setname][trname][[1]]],allsets[setname][trname],triangle[trname]];
		ntest=bIsOrthologic@@(evaluate[Join[trsym,trchk]]/.rule69)/.rule69;
		If[Abs[ntest]<10^-24,
			outname = "Orthology center of these triangles: "<>trgname<>" and "<>trname;
			Print[outname];
			ptcoord=TimeConstrained[bOrthologyCenter[trchk,trsym]//simplifyRationalBarycentrics//partialSReplace, 60, -1];
			If[ptc == -1, Print["timeout"];Continue[]];
			perschk=ptcoord//checkPointinETC2;

			If[Length[perschk] == 0, endnum = addpoint[1 + endnum, ptcoord, outname] // Quiet;,Print[perschk]];
			
			outname = "Orthology center of these triangles: "<>trname<>" and "<>trgname;
			Print[outname];
			ptcoord=TimeConstrained[bOrthologyCenter[trsym,trchk]//simplifyRationalBarycentrics//partialSReplace, 60, -1];
			If[ptc == -1, Print["timeout"];Continue[]];
			perschk=ptcoord//checkPointinETC2;
			If[Length[perschk] == 0, endnum = addpoint[1 + endnum, ptcoord, outname] // Quiet;,Print[perschk]];
		];
	,{trname,Keys[allsets[setname]]}
	];

	DumpSave["globalptnames.mx", globalptnames];
	DumpSave["globalptdescr.mx", globalptdescr];
	DumpSave["globalptexpr.mx", globalptexpr];
	DumpSave["globalptbary.mx", globalptbary];


	Do[
		trsym=If[ListQ[allsets[setname][trname][[1]]],allsets[setname][trname],triangle[trname]];
		ntest = bIsParallelogic @@ (evaluate[Join[trsym, trchk]] /.rule69)/.rule69;
		If[Abs[ntest]<10^-24,
			outname = "Parallelogic center of these triangles: "<>trgname<>" and "<>trname;
			Print[outname];
			ptcoord=TimeConstrained[bParallelogicCenter[trchk,trsym]//simplifyRationalBarycentrics//partialSReplace, 60, -1];
			If[ptc == -1, Print["timeout"];Continue[]];
			perschk=ptcoord//checkPointinETC2;

			If[Length[perschk] == 0, endnum = addpoint[1 + endnum, ptcoord, outname] // Quiet;,Print[perschk]];
			
			outname = "Orthology center of these triangles: "<>trname<>" and "<>trgname;
			Print[outname];
			ptcoord=TimeConstrained[bParallelogicCenter[trsym,trchk]//simplifyRationalBarycentrics//partialSReplace, 60, -1];
			If[ptc == -1, Print["timeout"];Continue[]];
			perschk=ptcoord//checkPointinETC2;
			If[Length[perschk] == 0, endnum = addpoint[1 + endnum, ptcoord, outname] // Quiet;,Print[perschk]];
		];
	,{trname,Keys[allsets[setname]]}
	];

	DumpSave["globalptnames.mx", globalptnames];
	DumpSave["globalptdescr.mx", globalptdescr];
	DumpSave["globalptexpr.mx", globalptexpr];
	DumpSave["globalptbary.mx", globalptbary];

,{setname, Keys[allsets]}
];
