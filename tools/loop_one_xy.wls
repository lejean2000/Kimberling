#!/usr/bin/env wolframscript

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

Do[
  AppendTo[ETC, ptname -> ETCExtra[ptname]];
  AppendTo[ETCBaryNorm, ptname -> ETCExtraBary[ptname]];
  , {ptname, Keys[ETCExtra]}
  ];

ETCFull = ETC;
ETCBaryNormFull = ETCBaryNorm;


singlePointProcesses = Association[
  "isogonal conjugate" -> {a^2 v w, b^2 u w, c^2 u v},
  "isotomic conjugate" -> {v w, u w, u v},
  "complement" -> {v + w, u + w, u + v},
  "anticomplement" -> {-u + v + w, u - v + w, u + v - w}
];

globalNoCleanup = True;

loop[xname_, title_, ptcoord_] := Module[{srch, pos},

   Print[];Print[title];Print[];

   pointChecker[ptcoord, xname, False, xname];
   
   (*
   srch = Round[bToCartesianN[evaluate[ptcoord]][[1]], 0.00000000000000000001];
   pos = FirstPosition[TRETC, srch][[1]];
   If[Length[pos] > 0, globalProperties[xname]["others"]={pos};Print[pos]];
   *)

   globalProperties[xname]["name"]=title;
   globalOutputStream = OpenWrite[xname <> ".txt", CharacterEncoding -> "UTF8"];
   printGlobalProperties[globalProperties, xname, intaddbrackets[xname]] // Quiet;
   Close[globalOutputStream];

];

loop[
"Y13691",
"Perspector of these triangles: X(2)-Ehrmann and Lucas antipodal tangents", 
sym[a^2 (4 (a^14 - 4 a^12 (b^2 + c^2) + 7 a^8 (b^2 + c^2)^3 + 
      a^10 (2 b^4 + b^2 c^2 + 2 c^4) - (b - c)^2 (b + c)^2 (b^2 + 
         c^2) (b^8 + 2 b^6 c^2 - 10 b^4 c^4 + 2 b^2 c^6 + c^8) - 
      2 a^4 (b^2 + c^2) (b^8 + 7 b^6 c^2 - 2 b^4 c^4 + 7 b^2 c^6 + 
         c^8) + a^2 (b^2 - c^2)^2 (4 b^8 + 23 b^6 c^2 + 32 b^4 c^4 + 
         23 b^2 c^6 + 4 c^8) - a^6 (7 b^8 + 16 b^6 c^2 + 24 b^4 c^4 + 16 b^2 c^6 + 
         7 c^8)) + (-a^12 - 24 a^10 (b^2 + c^2) - 36 a^6 b^2 c^2 (b^2 + c^2) + 
      51 a^8 (b^2 + c^2)^2 + (b^4 - c^4)^2 (b^4 + 42 b^2 c^2 + c^4) + 
      4 a^2 (b^2 + c^2) (6 b^8 - 3 b^6 c^2 - 14 b^4 c^4 - 3 b^2 c^6 + 6 c^8) - 
      a^4 (51 b^8 + 96 b^6 c^2 + 58 b^4 c^4 + 96 b^2 c^6 + 51 c^8)) S)]
];


