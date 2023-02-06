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

pP = {u/(v - w), v/(-u + w), w/(u - v)};
Do[
	Print["Trilinear pole for P = X"<>ToString[nx]];

	ptcoord = pP/.Thread[{u,v,w}->KimberlingCenterCN[nx]];
	ptcoord = simplifyRationalBarycentrics[ptcoord];

	pt = N[NormalizeBary[(ptcoord) /. rule69], 35];
	chk = checkPointinETC[pt];
	If[chk[[1]] <= 10^(-12), Print["ETC: " <> Keys[chk]]];

	If[chk[[1]] > 10^(-12),
		Print["Barycentrics    " <> ExpressionToTrad[ptcoord[[1]]]];
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
	];
	,{nx,24,101}
]

