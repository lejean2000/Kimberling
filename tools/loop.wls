#!/usr/bin/env wolframscript

$HistoryLength = 0;

If[$OperatingSystem == "Windows",
Get["../sources/initwin.wls"],
Get["../sources/init.wls"]
];

Get["../db/ETCExtra.mx"];
Get["../db/ETCExtraBary.mx"];
Get["../db/NonETCNames.mx"];

(*
Get["../db/TRETC.mx"];
TRETC = Sort[Map[Round[#[[1]], 0.00000000000000000001] &, TRETC]];
*)

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
pointCheckerMinProperties = 2;

loop[from_, to_] := Block[{xname, ptcoord, pt, pos},

  Do[
   xname = "X" <> ToString[nx];

   ptcoord = sym[globalptexpr[xname]];
   
   If[checkPointinETC2[ptcoord], Print[xname<>" EXISTS"]; Continue[]];

   Print[];Print[globalptnames[xname]];Print[];

   pointChecker[ptcoord, xname, False, xname];
   
   globalProperties[xname]["others"]={};

   (*
   pos = Position[TRETC, Round[bToCartesianN[evaluate[ptcoord]][[1]], 0.00000000000000000001]];

   Do[
		trpt = StringTake[ToString[el],{6,-2+StringLength[ToString[el]]}];
		itrg = StringTake[trpt, {1 + StringPosition[trpt, "-of-"][[1]][[2]], StringLength[trpt]}];
		ptidx = ToExpression[StringTake[trpt, {3, -1 + StringPosition[trpt, ")"][[1]][[1]]}]];
		
		If[Not[checkNumberedPoint[ptcoord, itrg, ptidx]],trpt = trpt<>" UNSUCCESSFUL"];

		AppendTo[globalProperties[xname]["others"],trpt];
		Print[trpt];
   ,{el,pos}
   ];
   *)

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

loop[80000, endnum];
