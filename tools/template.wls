#!/usr/bin/env wolframscript

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
AssociateTo[ETC, KeySelect[ETC2, ToExpression[StringTake[#, {2, -1}]] >= 66000 &]];
AssociateTo[ETCBaryNorm, KeySelect[ETCBaryNorm2, ToExpression[StringTake[#, {2, -1}]] >= 66000 &]];
Get["globalptnames.mx"];
Get["globalptdescr.mx"];
,
globalptnames = Association[];
globalptdescr = Association[];
];

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


addpoint[ptnum_, prcoordexpr_, topname_, ptnamein_:""] := Module[{yy, xname, start},
  pointCheckerMinProperties = 8;
  start = ptnum;

   globalNoCleanup = True;
   
   If[StringLength[topname]>0, Print[];Print["======="<>topname<>"=======1"]];
   If[StringLength[ptnamein]>0, Print[];Print["======="<>ptnamein<>"=======2"]];
   
   
   ptchk = quickChecker[prcoordexpr,0,False];
   
   If[
	   Total[ptchk]>=pointCheckerMinProperties,

	   xname = "X" <> ToString[start];

	   AppendTo[ETC, xname->prcoordexpr[[1]]];
	   yy=intnumericnorm[evaluate[symmetrizeInternal[ETC[xname]]]/.rule69];
	   AppendTo[ETCBaryNorm, xname->yy];
	   AppendTo[ETCFull, xname->prcoordexpr[[1]]];
	   AppendTo[ETCBaryNormFull, xname->yy];
	   
	   AppendTo[globalptnames,xname->topname];
	   AppendTo[globalptdescr,xname->ptnamein];
	   
	   start = start + 1;
	   
	   Print["======="<>ToString[ptchk]<>"======="];
   ];

  Return[start-1];
];


loop[from_, to_] := Module[{xname, ptcoord, pt},
  pointCheckerMinProperties = 0;
  Do[
   xname = "X" <> ToString[nx];

   ptcoord = X[nx];

   pointChecker[ptcoord, xname, False, xname];
   Print["=======END======="];
	
   If [
   KeyExistsQ[globalptnames,xname]&&StringLength[globalptnames[xname]]>0, 
   globalProperties[xname]["name"]=globalptnames[xname]; Print[globalptnames[xname]];
   ];
   
   (*If [KeyExistsQ[globalptdescr,xname]&&StringLength[globalptdescr[xname]]>0, globalProperties[xname]["descr"]={globalptdescr[xname]}];*)
   If [KeyExistsQ[globalptdescr,xname]&&StringLength[globalptdescr[xname]]>0, globalProperties[xname]["others"]={globalptdescr[xname]}];
   
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

   ptcoord = TimeConstrained[prcoordexpr/.Thread[pP -> KimberlingCenterC[nx]]//partialSReplace//simplifyRationalBarycentrics,10,-1];
   If[ptcoord == -1, Continue[]];

   globalNoCleanup = True;
   
   ptnametop = StringReplace[topname, "XX" -> intaddbrackets["X"<>ToString[nx]]];
   If[StringLength[ptnametop]>0, Print[];Print["======="<>ptnametop<>"=======1"]];
   ptdescr = StringReplace[ptnamein, "XX" -> intaddbrackets["X"<>ToString[nx]]];
   If[StringLength[ptdescr]>0, Print[];Print["======="<>ptdescr<>"=======2"]];
   
   Print[];
   
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
	   
	   Print["======="<>ToString[ptchk]<>"======="];Print[];
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


endnum = Max[66000-1,Max[ToExpression[
  StringTake[
   SortBy[Keys[KeySelect[ETC, StringStartsQ[#, "X"] &]], 
    numsortexpr[#] &], {2, -1}]]]];

pers = {q r v w (p v (2 r u + p w) + q u (r u + 2 p w)), 
 p r u w (p r v^2 + q^2 u w + 2 q v (r u + p w)), 
 p q u v (r^2 u v + 2 r (q u + p v) w + p q w^2)};
 

Do[

Print[num];

expr = pers /. Thread[pQ -> X[num]] // simplifyRationalBarycentrics;
set = massHeuristics1[pers /. Thread[pQ -> X[num]], 1, 170][[1]];

Print[set];

endnum = quickloop[1+endnum, expr, set, "", "isotomic conjugate of bicevian chordal perspector of X("<>ToString[num]<>") and XX"]; 
,{num,{1,2,3,4,6,7,8,9,10,11,12,21,31,37,75,76,81,86}}
];


(*
loop[66000, 66189];
*)