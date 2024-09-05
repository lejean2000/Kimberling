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


loop[xname_, title_, ptcoord_] := Module[{pt, srch, pos},

   Print[];Print[title];Print[];
   
   If[checkPointinETC2[ptcoord], Print[xname<>" EXISTS"]; Continue[]];

   pointChecker[ptcoord, xname, False, xname];
   
   pos = Position[TRETC, Round[bToCartesianN[evaluate[ptcoord]][[1]], 0.00000000000000000001]];
   
   globalProperties[xname]["others"]={};
   Do[
		trpt = StringTake[ToString[el],{6,-2+StringLength[ToString[el]]}];
		itrg = StringTake[trpt, {1 + StringPosition[trpt, "-of-"][[1]][[2]], StringLength[trpt]}];
		ptidx = ToExpression[StringTake[trpt, {3, -1 + StringPosition[trpt, ")"][[1]][[1]]}]];
		
		If[Not[checkNumberedPoint[ptcoord, itrg, ptidx]],trpt = trpt<>" UNSUCCESSFUL"];

		AppendTo[globalProperties[xname]["others"],trpt];
		Print[trpt];
   ,{el,pos}
   ];
   
   Print["=======END======="];

   globalProperties[xname]["name"]=title;

   globalNoCleanup = False;
   globalOutputStream = OpenWrite[xname <> ".txt", CharacterEncoding -> "UTF8"];
   printGlobalProperties[globalProperties, xname, intaddbrackets[xname]] // Quiet;
   print["=======END======="];
   Close[globalOutputStream];

];

loop["X70004","NONAME190",
sym[4*a^6-4*a^5*(b+c)-2*a*b*(b-c)^2*c*(b+c)-(b-c)^4*(b+c)^2-2*a^4*(b^2-b*c+c^2)+2*a^3*(2*b^3+b^2*c+b*c^2+2*c^3)-a^2*(b^4+4*b^2*c^2+c^4)]];
