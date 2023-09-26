#!/usr/bin/env wolframscript
(* ::Package:: *)

ClearAll["Global`*"];

If[$OperatingSystem == "Windows",
Get["../sources/initwin.wls"],
Get["../sources/init.wls"]
];

Get["../db/ETCExtra.mx"];
Get["../db/ETCExtraBary.mx"];
Get["../db/NonETCNames.mx"];


If[FileExistsQ["ETC2.mx"],
Get["ETC2.mx"];
Get["ETCBaryNorm2.mx"];
ETC=ETC2;
ETCBaryNorm=ETCBaryNorm2;
Get["globalptnames.mx"];
Get["globalptdescr.mx"];
,
globalptnames = Association[];
globalptdescr = Association[];
]

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

loop[from_, to_] := Module[{xname, ptcoord, pt},
  pointCheckerMinProperties = 0;
  Do[
   xname = "X" <> ToString[nx];

   ptcoord = X[nx];

   pointChecker[ptcoord, xname, False, xname];
   Print["=======END======="];
	
   If [KeyExistsQ[globalptnames,xname]&&StringLength[globalptnames[xname]]>0, globalProperties[xname]["name"]=globalptnames[xname]];
   If [KeyExistsQ[globalptdescr,xname]&&StringLength[globalptdescr[xname]]>0, globalProperties[xname]["descr"]={globalptdescr[xname]}];
   
   globalNoCleanup = False;
   globalOutputStream = OpenWrite[xname <> ".txt", CharacterEncoding -> "UTF8"];
   printGlobalProperties[globalProperties, xname, intaddbrackets[xname]] // Quiet;
   print["=======END======="];
   Close[globalOutputStream];
   ,
   {nx, from, to}
   ];
];


quickloop[ptnum_, prcoordexpr_, set_, topname_, ptnamein_] := Module[{yy, xname, ptcoord, ptdescr, start},
  pointCheckerMinProperties = 1;
  start = ptnum;
  Do[
   ptdescr = "";
   ptnametop = "";

   ptcoord = TimeConstrained[replacer[prcoordexpr,nx]//simplifyRationalBarycentrics,10,-1];

   If[ptcoord == -1, Continue[]];

   globalNoCleanup = True;
   

   ptnametop = StringReplace[topname, "XX" -> intaddbrackets["X"<>ToString[nx]]];
   If[StringLength[ptnametop]>0, Print[];Print["======="<>ptnametop<>"=======1"]];
   ptdescr = StringReplace[ptnamein, "XX" -> intaddbrackets["X"<>ToString[nx]]];
   If[StringLength[ptdescr]>0, Print[];Print["======="<>ptdescr<>"=======2"]];
   
   Print[];Print[nx];Print[];
   
   ptchk = TimeConstrained[quickChecker[ptcoord,0,False],30,-1];
   
   If[
   Total[ptchk]>=pointCheckerMinProperties,
   
   xname = "X" <> ToString[start];

   AppendTo[ETC, xname->ptcoord[[1]]];
   yy=intnumericnorm[evaluate[symmetrizeInternal[ETC[xname]]]/.rule69];
   AppendTo[ETCBaryNorm, xname->yy];
   AppendTo[ETCFull, xname->ptcoord[[1]]];
   AppendTo[ETCBaryNormFull, xname->yy];
   
   AppendTo[globalptnames,xname->ptnametop];
   AppendTo[globalptdescr,xname->ptdescr];
   
   start = start + 1;
   
   Print["======="<>ToString[ptchk]<>"======="];
   ];
   ,
   {nx, set}
   ]; 
   ETCBaryNorm2 = KeySelect[ETCBaryNorm, StringStartsQ[#, "X"] &];
   DumpSave["ETCBaryNorm2.mx", ETCBaryNorm2];
   ETC2 = KeySelect[ETC, StringStartsQ[#, "X"] &];
   DumpSave["ETC2.mx", ETC2];
   DumpSave["globalptnames.mx", globalptnames];
   DumpSave["globalptdescr.mx", globalptdescr];

  Return[start-1];
];

globalpt = Association[];

startnum = Max[62000,1+Max[ToExpression[
  StringTake[
   SortBy[Keys[KeySelect[ETC, StringStartsQ[#, "X"] &]], 
    numsortexpr[#] &], {2, -1}]]]];


ptins = bIsotomicConjugate[{u,v,w}];

set={116,117};

endnum = quickloop[startnum, ptins, set, "Isotomic Conjugate of XX", ""];

(*
loop[62000, 62394];
*)
