#!/usr/bin/env wolframscript

If[$OperatingSystem == "Windows",
Get["../sources/initwin.wls"],
Get["../sources/init.wls"]
];

Get["../db/ETCExtra.mx"];
Get["../db/ETCExtraBary.mx"];
Get["../db/NonETCNames.mx"];


Get["ETC2.mx"];
Get["globalptnames.mx"];
Get["globalptdescr.mx"];

ETCFull = ETC;
ETCBaryNormFull = ETCBaryNorm;

(* Add Extra points to ETC*)
Do[
  AppendTo[ETCFull, ptname -> ETCExtra[ptname]];
  AppendTo[ETCBaryNormFull, ptname -> ETCExtraBary[ptname]];
  , {ptname, Keys[ETCExtra]}
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

start = 66000;

Do[
	pointCheckerMinProperties = 60;
	
	If[StringContainsQ[globalptnames[ptn],"X(1), X(3)"], pointCheckerMinProperties = 40];
	If[StringContainsQ[globalptnames[ptn],"X(1), X(7)"], pointCheckerMinProperties = 36];
	If[StringContainsQ[globalptnames[ptn],"X(3), X(1)"], pointCheckerMinProperties = 100];
	
	Print[globalptnames[ptn]];
	
	ptcoord = sym[ETC2[ptn]];
	ptchk = TimeConstrained[quickChecker[ptcoord,0,False],30,-1];
    If[Total[ptchk]>=2,crvsnum=checkCurves[ptcoord] // Quiet // Length;,crvsnum=0]
	
    If[
	   Total[ptchk]+30*crvsnum>=pointCheckerMinProperties,
	   yy=intnumericnorm[evaluate[symmetrizeInternal[ETC[ptn]]]/.rule69];
	   
	   xname = "X" <> ToString[start];
	   Print[xname];
	   AppendTo[ETC, xname->ptcoord[[1]]];
	   AppendTo[ETCBaryNorm, xname->yy];
	   AppendTo[ETCFull, xname->ptcoord[[1]]];
	   AppendTo[ETCBaryNormFull, xname->yy];
	   AppendTo[globalptnames2,xname->globalptnames[ptn]];
	   AppendTo[globalptdescr2,xname->globalptdescr[ptn]];
	   
	   start = start + 1;
	   
	   Print["======="<>ToString[ptchk]<>"======="<>ptn];
	];
	Print[];
,{ptn, Keys[KeySelect[ETC2, ToExpression[StringTake[#, {2, -1}]] >= 66000 &]]}
];

ETCBaryNorm2 = KeySelect[ETCBaryNorm, StringStartsQ[#, "X"] &];
DumpSave["ETCBaryNorm2.mx", ETCBaryNorm2];
ETC2 = KeySelect[ETC, StringStartsQ[#, "X"] &];
DumpSave["ETC2.mx", ETC2];
globalptnames=globalptnames2;
globalptdescr=globalptdescr2;
DumpSave["globalptnames.mx", globalptnames];
DumpSave["globalptdescr.mx", globalptdescr];
