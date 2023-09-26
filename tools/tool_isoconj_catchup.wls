#!/usr/bin/env wolframscript

ClearAll["Global`*"];

If[$OperatingSystem == "Windows",
Get["../sources/initwin.wls"],
Get["../sources/init.wls"]
];

Get["../db/ETCExtra.mx"];
Get["../db/ETCExtraBary.mx"];
Get["../db/NonETCNames.mx"];

Do[
  AppendTo[ETC, ptname -> ETCExtra[ptname]];
  AppendTo[ETCBaryNorm, ptname -> ETCExtraBary[ptname]];
  , {ptname, Keys[ETCExtra]}
  ];

ETCFull = ETC;
ETCBaryNormFull = ETCBaryNorm;

Do[
  If[Not[KeyExistsQ[ETC, "X" <> ToString[nx]]], Continue[]];
  ptcoord = KimberlingCenterC[nx] // bIsogonalConjugate;
  check = checkPointinETC2[ptcoord];
  If[
   Length[check] == 0,
   ptcoord = TimeConstrained[simplifyRationalBarycentrics[ptcoord], 5, ptcoord];
   addExtraPoint[ptcoord, "Z", "IsogConj(X" <> ToString[nx] <> ")", False];
  ];
  Print[ToString[nx]];
  If[
  Mod[nx, 100] == 0, 
	DumpSave["ETCExtra.mx", ETCExtra];
	DumpSave["ETCExtraBary.mx", ETCExtraBary];
	DumpSave["NonETCNames.mx", NonETCNames];
   ];
  , {nx, 53412, 58263}
  ];

DumpSave["ETCExtra.mx", ETCExtra];
DumpSave["ETCExtraBary.mx", ETCExtraBary];
DumpSave["NonETCNames.mx", NonETCNames];

	