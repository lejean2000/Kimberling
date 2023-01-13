#!/usr/bin/env wolframscript

ClearAll["Global`*"];
Get["../sources/KimberlingPoints.m"];
Get["../sources/TriangleTools.m"];
Get["../db/ETC.mx"];
Get["../db/ETCBaryNorm.mx"];
Get["../sources/TriangleCurves.m"];
Get["../sources/KimberlingTriangles.m"];
Get["../sources/DBTools.m"];
Get["../sources/TriangleExpressions.m"];

argv = Rest @ $ScriptCommandLine;

num = argv[[2]];
Print["X(" <> num <> ") of X(2)-circleconcevian triangle"];

Print["Barycentrics    "<>argv[[1]]];

expr = ToExpression[argv[[1]]];


ptcoord = symmetrizeInternal[expr]// evaluate;

(* Check ETC *)
pt = N[NormalizeBary[(ptcoord) /. rule69], 35];
chk = checkPointinETC[pt];
If[chk[[1]] < 10^(-12), Print["ETC: " <> Keys[chk]]];

If[chk[[1]] > 10^(-12),
	full = False;

	If[full,
	  lines = linesProcessAlg[ptcoord, 20, False, False] // Quiet,
	  lines = linesProcessAlg[ptcoord] // Quiet;
	  ];
	If[full || Length[lines] > 3,
	  Print["Lies on circumconics: "];
	  checkCircumconics[ptcoord, 1, 60] // Quiet;
	  
	  Print["Lies on curves: "];
	  checkCurves[ptcoord] // Quiet;
	  
	  checkTrilinearPolar[ptcoord] // Quiet;
	  checkIsogonalConjugates[ptcoord] // Quiet; 
	  checkDaoConjugates[ptcoord] // Quiet; 
	  checkAnticomplementaryConjugates[ptcoord] // Quiet; 
	  checkCrossConjugates[ptcoord] // Quiet; 
	  checkBarycentricProduct[ptcoord] // Quiet; 
	  checkBarycentricQuotient[ptcoord] // Quiet; 
	  TimeConstrained[Quiet[pointCheckAllProcesses[ptcoord]], 90];
	  ];
]
