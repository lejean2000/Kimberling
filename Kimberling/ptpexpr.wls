#!/usr/bin/env wolframscript

ClearAll["Global`*"];
Get["../db/EtcCheckNumbers.mx"];
Get["../sources/KimberlingPoints.m"];
Get["../sources/TriangleTools.m"];
Get["../db/ETC.mx"];
Get["../db/ETCBaryNorm.mx"];
Get["../sources/TriangleCurves.m"];
Get["../sources/KimberlingTriangles.m"];
Get["../sources/DBTools.m"];
Get["../sources/TriangleExpressions.m"];

argv = Rest @ $ScriptCommandLine;

Print["Barycentrics    "<>argv[[1]]];

expr = ToExpression[argv[[1]]];

ptcoord = symmetrizeInternal[expr]// evaluate;

(* Check ETC *)
pt = N[NormalizeBary[(ptcoord) /. rule69], 35];
chk = checkPointinETC[pt];
If[chk[[1]] < 10^(-12), Print["ETC: " <> Keys[chk]]];

If[chk[[1]] > 10^(-12),
	linesProcessAlg[ptcoord]//Quiet;

	Print["Curves"];
	checkCurves[ptcoord]// Quiet;
	
	Print["Processes"];
	TimeConstrained[Quiet[pointCheckAllProcesses[ptcoord]],60];

	Print["Lies on circumconics: "];
	checkCircumconics[ptcoord, 1, 120]//Quiet;
	
	Print["= X(i)-isoconjugate-of-X(j) for these {i, j}: "];
	checkIsogonalConjugates[ptcoord]//Quiet;

	Print["= X(i)-anticomplementary conjugate of X(j) for these (i,j): "];
	checkAnticomplementaryConjugates[ptcoord]//Quiet;
	
	Print["= barycentric product X(i)*X(j) for these (i, j): "];
	checkBarycentricProduct[ptcoord]//Quiet;

	Print["= barycentric quotient X(i)/X(j) for these (i, j): "];
	checkBarycentricQuotient[ptcoord]//Quiet;

	Print["Check cross conjugate"];
	checkCrossConjugate[ptcoord,500]//Quiet;
]
