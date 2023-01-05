#!/usr/bin/env wolframscript

ClearAll["Global`*"];
Get["../db/EtcCheckNumbers.mx"];
Get["../sources/KimberlingPoints.m"];
Get["../sources/TriangleTools.m"];
Get["../db/ETC.mx"];
Get["../db/ETCBaryNorm.mx"];
Get["../sources/TriangleCurves.m"];
Get["../sources/conicTools.m"];
Get["../sources/KimberlingTriangles.m"];
Get["../sources/DBTools.m"];
Get["../sources/TriangleExpressions.m"];

argv = Rest @ $ScriptCommandLine;

Print["Start checks for "<>argv[[1]]];

nx = ToExpression[argv[[1]]];

pP={u (a2 a3 b2 b3 u^2+a2 a3 b1 b3 u v+a1 a3 b2 b3 u v+a1 a3 b1 b3 v^2-a1 a2 b1 b2 w^2) (a2 a3 b2 b3 u^2-a1 a3 b1 b3 v^2+a2 a3 b1 b2 u w+a1 a2 b2 b3 u w+a1 a2 b1 b2 w^2),v (a2 a3 b2 b3 u^2+a2 a3 b1 b3 u v+a1 a3 b2 b3 u v+a1 a3 b1 b3 v^2-a1 a2 b1 b2 w^2) (-a2 a3 b2 b3 u^2+a1 a3 b1 b3 v^2+a1 a3 b1 b2 v w+a1 a2 b1 b3 v w+a1 a2 b1 b2 w^2),w (a2 a3 b2 b3 u^2-a1 a3 b1 b3 v^2+a2 a3 b1 b2 u w+a1 a2 b2 b3 u w+a1 a2 b1 b2 w^2) (-a2 a3 b2 b3 u^2+a1 a3 b1 b3 v^2+a1 a3 b1 b2 v w+a1 a2 b1 b3 v w+a1 a2 b1 b2 w^2)};

ptcoord=pP/.Thread[{a1,a2,a3}->KimberlingCenterCN[3]]/.Thread[{b1,b2,b3}->KimberlingCenterCN[nx]]/.Thread[{u,v,w}->KimberlingCenterCN[1]]//Simplify;
(* Check ETC *)
pt = ptcoord /. rule69;
Print[NormalizeBary[pt]];
chk = checkPointinETC[pt];
If[chk[[1]] < 10^(-12), Print["ETC: " <> Keys[chk]]];

If[chk[[1]] > 10^(-12),
	linesProcessAlg[ptcoord]//Quiet;

	Print["Processes"];
	TimeConstrained[Quiet[pointCheckAllProcesses[ptcoord]],60];

	Print["Isogonal conjugates"];
	checkIsogonalConjugates[ptcoord]//Quiet;

	Print["Barycentric product"];
	checkBarycentricProduct[ptcoord]//Quiet;

	Print["Barycentric quotient"];
	checkBarycentricQuotient[ptcoord]//Quiet;

	Print["Check circumconics"];
	checkCircumconics[ptcoord, 1, 180]//Quiet;

	Print["Check cross conjugate"];
	checkCrossConjugate[ptcoord,500]//Quiet;
]

Print["Start checks for isotomic conjugate of "<>argv[[1]]];

ptcoord=bIsotomicConjugate[ptcoord];

(* Check ETC *)
pt = ptcoord /. rule69;
Print[NormalizeBary[pt]];
chk = checkPointinETC[pt];
If[chk[[1]] < 10^(-12), Print["ETC: " <> Keys[chk]]];

If[chk[[1]] > 10^(-12),
	linesProcessAlg[ptcoord]//Quiet;

	Print["Processes"];
	TimeConstrained[Quiet[pointCheckAllProcesses[ptcoord]],60];

	Print["Isogonal conjugates"];
	checkIsogonalConjugates[ptcoord]//Quiet;

	Print["Barycentric product"];
	checkBarycentricProduct[ptcoord]//Quiet;

	Print["Barycentric quotient"];
	checkBarycentricQuotient[ptcoord]//Quiet;

	Print["Check circumconics"];
	checkCircumconics[ptcoord, 1, 180]//Quiet;

	Print["Check cross conjugate"];
	checkCrossConjugate[ptcoord,500]//Quiet;
]

