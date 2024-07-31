#!/usr/bin/env wolframscript

If[$OperatingSystem == "Windows",
Get["../sources/initwin.wls"],
Get["../sources/init.wls"]
];

Get["../db/ETCExtra.mx"];
Get["../db/ETCExtraBary.mx"];
Get["../db/NonETCNames.mx"];

Get["../db/TRETC.mx"];
TRETC = Sort[Map[Round[#[[1]], 0.00000000000000000001] &, TRETC]];

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
pointCheckerMinProperties = 2;

loop[from_, to_] := Module[{xname, ptcoord, pt},

  Do[
   xname = "X" <> ToString[nx];

   ptcoord = sym[globalptexpr[xname]];

   Print[];Print[globalptnames[xname]];Print[];

   pointChecker[ptcoord, xname, False, xname];

   pos = Position[TRETC, Round[bToCartesianN[evaluate[ptcoord]][[1]], 0.00000000000000000001]];
   
   globalProperties[xname]["others"]={};
   Do[
		trpt = StringTake[ToString[el],{6,-2+StringLength[ToString[el]]}];
		AppendTo[globalProperties[xname]["others"],trpt];
		Print[trpt];
   ,{el,pos}
   ];
   
   Print["=======END======="];

   If [
   KeyExistsQ[globalptnames,xname]&&StringLength[globalptnames[xname]]>0,
   globalProperties[xname]["name"]=globalptnames[xname];
   ];

(*
   If [
   KeyExistsQ[globalptdescr,xname]&&StringLength[globalptdescr[xname]]>0,
   globalProperties[xname]["others"]=globalptdescr[xname];
   ];
*)

   globalNoCleanup = False;
   globalOutputStream = OpenWrite[xname <> ".txt", CharacterEncoding -> "UTF8"];
   printGlobalProperties[globalProperties, xname, intaddbrackets[xname]] // Quiet;
   print["=======END======="];
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
