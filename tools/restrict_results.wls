#!/usr/bin/env wolframscript

If[$OperatingSystem == "Windows",
Get["../sources/initwin.wls"],
Get["../sources/init.wls"]
];

Get["../db/ETCExtra.mx"];
Get["../db/ETCExtraBary.mx"];
Get["../db/NonETCNames.mx"];

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
globalNoCleanup = False;
globalptnames2 = Association[];
globalptdescr2 = Association[];
globalptexpr2 = Association[];
globalptbary2 = Association[];

start = STARTETCNUMBER;

Do[
	pointCheckerMinProperties = 10;
	skip = 1;

	If[StringContainsQ[globalptnames[ptn],"antipedal"], pointCheckerMinProperties = 40;skip = 0;];
	If[StringContainsQ[globalptnames[ptn],"X(30)"], Continue[];];
	
	Print[globalptnames[ptn]];
	ptcoord = sym[globalptexpr[ptn]];
	
	If[skip == 0,	
		ptchk = TimeConstrained[quickChecker[ptcoord,0,False],30,-1];
		If[Total[ptchk]>=2,crvsnum=checkCurves[ptcoord] // Quiet // Length;,crvsnum=0],
		
		ptchk = {100,100};crvsnum = 100;
	];
    If[
	   Total[ptchk]+30*crvsnum>=pointCheckerMinProperties,
	   yy=intnumericnorm[evaluate[ptcoord]/.rule69];
	   
	   xname = "X" <> ToString[start];
	   Print[xname];
	   AppendTo[ETC, xname->ptcoord[[1]]];
	   AppendTo[ETCBaryNorm, xname->yy];
	   AppendTo[ETCFull, xname->ptcoord[[1]]];
	   AppendTo[ETCBaryNormFull, xname->yy];
	   AppendTo[globalptnames2,xname->globalptnames[ptn]];
	   AppendTo[globalptdescr2,xname->globalptdescr[ptn]];
	   AppendTo[globalptexpr2, xname->ptcoord[[1]]];
	   AppendTo[globalptbary2, xname->yy];
	   
	   start = start + 1;
	   
	   Print["======="<>ToString[ptchk]<>"======="<>ptn];
	];
	Print[];
,{ptn, Keys[KeySelect[globalptnames, ToExpression[StringTake[#, {2, -1}]] >= STARTETCNUMBER &]]}
];

globalptnames=globalptnames2;
globalptdescr=globalptdescr2;
globalptexpr=globalptexpr2;
globalptbary=globalptbary2;
DumpSave["globalptnames.mx", globalptnames];
DumpSave["globalptdescr.mx", globalptdescr];
DumpSave["globalptexpr.mx", globalptexpr];
DumpSave["globalptbary.mx", globalptbary];
