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

loop[from_, to_] := Module[{xname, ptcoord, pt},
  pointCheckerMinProperties = 0;
  Do[
   xname = "X" <> ToString[nx];

   ptcoord = sym[globalptexpr[xname]];
   
   Print[];
   pointChecker[ptcoord, xname, False, xname];
   Print["=======END======="];
	
   If [
   KeyExistsQ[globalptnames,xname]&&StringLength[globalptnames[xname]]>0, 
   globalProperties[xname]["name"]=globalptnames[xname]; Print[globalptnames[xname]];
   ];
   
   If [
   KeyExistsQ[globalptdescr,xname]&&StringLength[globalptdescr[xname]]>0, 
   globalProperties[xname]["others"]=globalptdescr[xname]; Print[globalptdescr[xname]];
   ];
   
   globalNoCleanup = False;
   globalOutputStream = OpenWrite[xname <> ".txt", CharacterEncoding -> "UTF8"];
   printGlobalProperties[globalProperties, xname, intaddbrackets[xname]] // Quiet;
   Print["=======END======="];
   Close[globalOutputStream];
   ,
   {nx, from, to}
   ];
];

endnum = Max[STARTETCNUMBER-1,Max[ToExpression[
  StringTake[
   SortBy[Keys[KeySelect[globalptnames, StringStartsQ[#, "X"] &]], 
    numsortexpr[#] &], {2, -1}]
]]];

loop[STARTETCNUMBER, endnum];
