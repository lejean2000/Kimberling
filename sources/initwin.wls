#!/usr/bin/env wolframscript
(* ::Package:: *)

ClearAll["Global`*"];
Get["../sources/KimberlingPoints.m"];
Get["../sources/TriangleTools.m"];
Get["../db/ETC.mx"];
Get["../db/ETCBaryNorm.mx"];
Get["../db/TriangleCurves.mx"];
Get["../db/CentralCircles.mx"];
Get["../db/KimberlingTrianglesBary.mx"];
Get["../db/KimberlingTrianglesBaryOrthKeys.mx"];
Get["../sources/fltCircles.m"];
Get["../sources/TriangleCurves.m"];
Get["../sources/TriangleExpressions.m"];
Get["../sources/DBTools.m"];
PA = {31/3, (4 \[Sqrt]35)/3}; PB = {0, 0}; PC = {6, 0};
xA = {1, 0, 0}; xB = {0, 1, 0}; xC = {0, 0, 1}; pP = {u, v, w}; pQ = {p, q, r};


Get["../db/ETCExtra.mx"];
Get["../db/ETCExtraBary.mx"];

(* Add Extra points to ETC*)
ETCFull=ETC;
ETCBaryNormFull=ETCBaryNorm;
