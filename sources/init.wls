#!/usr/bin/env wolframscript
(* ::Package:: *)

ClearAll["Global`*"];
Get["/home/admin/wls/sources/KimberlingPoints.m"];
Get["/home/admin/wls/sources/TriangleTools.m"];
Get["/home/admin/wls/db/ETC.mx"];
Get["/home/admin/wls/db/ETCBaryNorm.mx"];
Get["/home/admin/wls/db/TriangleCurves.mx"];
Get["/home/admin/wls/db/CentralCircles.mx"];
Get["/home/admin/wls/sources/fltCircles.m"];
Get["/home/admin/wls/sources/TriangleCurves.m"];
Get["/home/admin/wls/sources/KimberlingTriangles.m"];
Get["/home/admin/wls/sources/TriangleExpressions.m"];
Get["/home/admin/wls/sources/DBTools.m"];
PA = {31/3, (4 \[Sqrt]35)/3}; PB = {0, 0}; PC = {6, 0};
xA = {1, 0, 0}; xB = {0, 1, 0}; xC = {0, 0, 1}; pP = {u, v, w}; pQ = {p, q, r};


(* Add Extra points to ETC*)
ETCFull=ETC;
ETCBaryNormFull=ETCBaryNorm;

