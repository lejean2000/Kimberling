#!/usr/bin/env wolframscript
(* ::Package:: *)

ClearAll["Global`*"];
Get["/home/ivan/wolfram/sources/KimberlingPoints.m"];
Get["/home/ivan/wolfram/sources/TriangleTools.m"];
Get["/home/ivan/wolfram/db/ETC.mx"];
Get["/home/ivan/wolfram/db/ETCBaryNorm.mx"];
Get["/home/ivan/wolfram/db/TriangleCurves.mx"];
Get["/home/ivan/wolfram/db/CentralCircles.mx"];
Get["/home/ivan/wolfram/db/KimberlingTrianglesBary.mx"];
Get["/home/ivan/wolfram/db/KimberlingTrianglesBaryOrthKeys.mx"];
Get["/home/ivan/wolfram/sources/fltCircles.m"];
Get["/home/ivan/wolfram/sources/TriangleCurves.m"];
Get["/home/ivan/wolfram/sources/TriangleExpressions.m"];
Get["/home/ivan/wolfram/sources/DBTools.m"];
Get["/home/ivan/wolfram/db/CPTR.mx"];
Get["/home/ivan/wolfram/db/UNCFTRG.mx"];
Get["/home/ivan/wolfram/db/ENCTR.mx"];

PA = {31/3, (4 \[Sqrt]35)/3}; PB = {0, 0}; PC = {6, 0};
xA = {1, 0, 0}; xB = {0, 1, 0}; xC = {0, 0, 1}; pP = {u, v, w}; pQ = {p, q, r};

ETCFull = ETC;
ETCBaryNormFull = ETCBaryNorm;

loadedData  =  Import["/home/ivan/wolfram/db/precomputed_triangles.mx"];
precomputedKimberling  =  loadedData["Kimberling"];
precomputedCPTR  =  loadedData["CPTR"];
precomputedENCTR  =  loadedData["ENCTR"];
precomputedUNCFTRG  =  loadedData["UNCFTRG"];
allPrecomputedTrJoined = Join[precomputedKimberling, precomputedCPTR, precomputedENCTR, precomputedUNCFTRG];

Get["/home/ivan/wolfram/db/ETCExtra.mx"];
Get["/home/ivan/wolfram/db/ETCExtraBary.mx"];
Get["/home/ivan/wolfram/db/NonETCNames.mx"];

sr=simplifyRationalBarycentrics;
