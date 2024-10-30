#!/usr/bin/env wolframscript
(* ::Package:: *)

ClearAll["Global`*"];
Get["/home/ivan/wls/sources/KimberlingPoints.m"];
Get["/home/ivan/wls/sources/TriangleTools.m"];
Get["/home/ivan/wls/db/ETC.mx"];
Get["/home/ivan/wls/db/ETCBaryNorm.mx"];
Get["/home/ivan/wls/db/TriangleCurves.mx"];
Get["/home/ivan/wls/db/CentralCircles.mx"];
Get["/home/ivan/wls/sources/fltCircles.m"];
Get["/home/ivan/wls/sources/TriangleCurves.m"];
Get["/home/ivan/wls/sources/KimberlingTriangles.m"];
Get["/home/ivan/wls/sources/TriangleExpressions.m"];
Get["/home/ivan/wls/sources/DBTools.m"];
Get["/home/ivan/wls/sources/KimberlingTrianglesBaryOrthKeys.mx"];
PA = {31/3, (4 \[Sqrt]35)/3}; PB = {0, 0}; PC = {6, 0};
xA = {1, 0, 0}; xB = {0, 1, 0}; xC = {0, 0, 1}; pP = {u, v, w}; pQ = {p, q, r};


(* Add Extra points to ETC*)
ETCFull=ETC;
ETCBaryNormFull=ETCBaryNorm;

