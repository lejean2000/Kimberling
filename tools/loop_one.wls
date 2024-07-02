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

loop[nx_, title_, ptcoord_] := Module[{xname, ptcoord, pt},

   xname = "X" <> ToString[nx];

   Print[];Print[title];Print[];

   pointChecker[ptcoord, xname, False, xname];
   
   srch = Round[bToCartesianN[evaluate[ptcoord]][[1]], 0.00000000000000000001];
   pos = FirstPosition[TRETC, srch][[1]];
   If[Length[pos] > 0, globalProperties[xname]["others"]={pos};Print[pos]];
   
   Print["=======END======="];

   globalProperties[xname]["name"]=title;

   globalNoCleanup = False;
   globalOutputStream = OpenWrite[xname <> ".txt", CharacterEncoding -> "UTF8"];
   printGlobalProperties[globalProperties, xname, intaddbrackets[xname]] // Quiet;
   print["=======END======="];
   Close[globalOutputStream];

];

