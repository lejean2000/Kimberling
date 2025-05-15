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

globalNoCleanup = True;
pointCheckerMinProperties = 10;

addpoint[ptnum_, ptcoord_, topname_] := Module[{yy, xname, start, totalnum},

   start = ptnum;
   
   Print[];
   
   ptchk = TimeConstrained[quickChecker[ptcoord,0,False,False,pointCheckerMinProperties,STARTETCNUMBER],60,-1];
   (*If[Total[ptchk]>=2,crvsnum=checkCurves[ptcoord] // Quiet // Length;,crvsnum=0];*)
   
   totalnum = Total[ptchk];
   
   If[
	   totalnum>=pointCheckerMinProperties,
	   
	   xname = "X" <> ToString[start];
	   yy=intnumericnorm[evaluate[ptcoord]/.rule69];
	   
	   AppendTo[ETC, xname->ptcoord[[1]]];
	   AppendTo[ETCBaryNorm, xname->yy];
	   AppendTo[ETCFull, xname->ptcoord[[1]]];
	   AppendTo[ETCBaryNormFull, xname->yy];
	   
	   AppendTo[globalptnames,xname->topname];
	   AppendTo[globalptdescr,xname->totalnum];
	   AppendTo[globalptexpr, xname->ptcoord[[1]]];
	   AppendTo[globalptbary, xname->yy];
	   
	   start = start + 1;

	   Print["======="<>ToString[ptchk]<>"======="];Print[xname];
   ];
  
  Print[];
  Return[start-1];
];

Get["../db/UNCFTRG.mx"];
Get["../db/CPTR.mx"];
Get["../db/ENCTR.mx"];

KeyDropFrom[KimberlingTrianglesBary, {"inner-Hutson", "outer-Hutson", "infinite-altitude"}];

allsets = <|"KimberlingTrianglesBary"->KimberlingTrianglesBary, "CPTR"->CPTR, "UNCFTRG"->UNCFTRG, "ENCTR"->ENCTR|>;
(*
allsets = <|"KimberlingTrianglesBary"->KimberlingTrianglesBary|>;
allsets = <|"CPTR"->CPTR, "UNCFTRG"->UNCFTRG|>;
*)

endnum = Max[STARTETCNUMBER-1,Max[ToExpression[
  StringTake[
   SortBy[Keys[KeySelect[globalptnames, StringStartsQ[#, "X"] &]], 
    numsortexpr[#] &], {2, -1}]]]];


doTrig[trchk_, trgname_] :=  Module[{trsym, ntest, perschk, outname, ptcoord},
Do[
	Do[
		trsym=If[ListQ[allsets[setname][trname][[1]]],allsets[setname][trname],triangle[trname]];
		ntest=bIsPerspective@@(evaluate[Join[trsym,trchk]]/.rule69)/.rule69;
		If[Abs[ntest]<10^-24,
			trsym=TimeConstrained[smpl /@trsym, 15, -1];

			If[trsym==-1, Continue[]];
			outname = "Perspector of these triangles: "<>trgname<>" and "<>trname;
			Print[outname];				
			ptcoord=TimeConstrained[bTrianglePerspector[trchk,trsym]//smpl, 15, -1];
			If[ptcoord==-1, Print["timeout"]];
			perschk=ptcoord//checkPointinETC2;

			If[Length[perschk] == 0, endnum = addpoint[1 + endnum, ptcoord, outname] // Quiet;,Print[perschk]];
			
			outname = "Tripole of perspectivity axis of these triangles: "<>trgname<>" and "<>trname;
			Print[outname];				
			ptcoord=TimeConstrained[bPerspectivityAxis[trchk,trsym]//smpl//bTripoleL//smpl, 15, -1];
			If[ptcoord==-1, Print["timeout"]];
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
		If [
			(StringContainsQ[trname,"UCFT"]&&StringContainsQ[trname,"Dao"])
			||StringContainsQ[trname,"Altintas-isodynamic"]
			||StringContainsQ[trname,"Hutson"]
			||StringContainsQ[trname,"Lucas"],
			Continue[]
		];
		trsym=If[ListQ[allsets[setname][trname][[1]]],allsets[setname][trname],triangle[trname]];
		ntest=bIsOrthologic@@(evaluate[Join[trsym,trchk]]/.rule69)/.rule69;
		If[Abs[ntest]<10^-24,
			trsym=TimeConstrained[smpl/@trsym, 15, -1];
			If[trsym==-1, Continue[]];
			
			outname = "Orthology center of these triangles: "<>trgname<>" wrt "<>trname;
			Print[outname];
			ptcoord=TimeConstrained[bOrthologyCenter[trchk,trsym]//smpl, 15, -1];
			If[ptcoord==-1, Print["timeout"]];
			perschk=ptcoord//checkPointinETC2;

			If[Length[perschk] == 0, endnum = addpoint[1 + endnum, ptcoord, outname] // Quiet;,Print[perschk]];

			outname = "Orthology center of these triangles: "<>trname<>" wrt "<>trgname;
			Print[outname];
			ptcoord=TimeConstrained[bOrthologyCenter[trsym,trchk]//smpl, 15, -1];
			If[ptcoord==-1, Print["timeout"]];
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
		If [
			(StringContainsQ[trname,"UCFT"]&&StringContainsQ[trname,"Dao"])
			||StringContainsQ[trname,"Altintas-isodynamic"]
			||StringContainsQ[trname,"Hutson"]
			||StringContainsQ[trname,"Lucas"],
			Continue[]
		];
		trsym=If[ListQ[allsets[setname][trname][[1]]],allsets[setname][trname],triangle[trname]];
		ntest = bIsParallelogic @@ (evaluate[Join[trsym, trchk]] /.rule69)/.rule69;
		If[Abs[ntest]<10^-24,
			trsym=TimeConstrained[smpl/@trsym, 15, -1];
			If[trsym==-1, Continue[]];
			
			outname = "Parallelogic center of these triangles: "<>trgname<>" wrt "<>trname;
			Print[outname];
			ptcoord=TimeConstrained[bParallelogicCenter[trchk,trsym]//smpl, 15, -1];
			If[ptcoord==-1, Print["timeout"]];
			perschk=ptcoord//checkPointinETC2;

			If[Length[perschk] == 0, endnum = addpoint[1 + endnum, ptcoord, outname] // Quiet;,Print[perschk]];
			
			outname = "Parallelogic center of these triangles: "<>trname<>" wrt "<>trgname;
			Print[outname];
			ptcoord=TimeConstrained[bParallelogicCenter[trsym,trchk]//smpl, 15, -1];
			If[ptcoord==-1, Print["timeout"]];
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

(* Starting centers *)
outname = "Centroid of "<>trgname;
Print[outname];
ptcoord=TimeConstrained[bCentroid[trchk]//smpl, 15, -1];
If[ptc == -1, Print["timeout"];Continue[]];
perschk=ptcoord//checkPointinETC2;
If[Length[perschk] == 0, endnum = addpoint[1 + endnum, ptcoord, outname] // Quiet;,Print[perschk]];

Do[
	outname = "X("<>ToString[nx]<>") of "<>trgname;
	Print[outname];
	ptcoord=TimeConstrained[bCoordChangeK[nx, trchk]//smpl, 15, -1];
	If[ptc == -1, Print["timeout"];Continue[]];
	perschk=ptcoord//checkPointinETC2;
	If[Length[perschk] == 0, endnum = addpoint[1 + endnum, ptcoord, outname] // Quiet;,Print[perschk]];
,{nx,{3,4,5,6}}
]


DumpSave["globalptnames.mx", globalptnames];
DumpSave["globalptdescr.mx", globalptdescr];
DumpSave["globalptexpr.mx", globalptexpr];
DumpSave["globalptbary.mx", globalptbary];

];



kpictr[{u_, v_, w_}, {p_, q_, r_}] := {{
    -u (q r u + p r v - p q w) (q r u - p r v + p q w), 
    v (q r u + p r v + p q w) (p r v - q (r u + p w)), 
    w (-q r u - p r v + p q w) (q r u + p r v + p q w)}, 
	{u (q r u - p r v - p q w) (q r u + p r v + p q w), 
	-v (q r u + p r v - p q w) (-q r u + p r v + p q w), 
    w (-q r u - p r v + p q w) (q r u + p r v + p q w)}, 
	{u (q r u - p r v - p q w) (q r u + p r v + p q w), 
    v (q r u + p r v + p q w) (p r v - q (r u + p w)), 
	-w (q r u - p r v + p q w) (-q r u + p r v + p q w)}};


Do[
trchk=simplifyRationalBarycentrics /@ kpictr[X[v[[1]]], X[v[[2]]]];
trgname = "CTR5-"<>ToString[v[[1]]]<>"."<>ToString[v[[2]]];
doTrig[trchk,trgname];
, {v, {{1,3},{2,3},{3,3},{3,2},{3,1}}}
]