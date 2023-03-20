# Triangle Center Tools

This repo provides you the `KimberlingCenter[k, A, B, C]` function which given an integer k and three points A, B and C, calculates the k-th triangle center according to Kimberling's [encyclopedia of triangle centers](https://faculty.evansville.edu/ck6/encyclopedia/etc.html).

Note that currrently k<=53101.

Here is a simple example which plots a few random triangle centers:
```
SetDirectory[NotebookDirectory[]]; ClearAll["Global`*"];
Get["../db/ETC.mx"];
Get["../sources/KimberlingPoints.m"];
Get["../sources/TriangleTools.m"];
Get["../sources/TriangleExpressions.m"];

PA = {0, 0}; PB = {3, 0}; PC = {1, 2};
indices = {1, 10, 22, 32, 40};
centers = 
  Table[KimberlingCenter[i, PA, PB, PC], {i, indices}] // Simplify;
names = Table["X" <> TextString[n], {n, indices}];
Graphics[Join[
  {EdgeForm[{Thin, Black}], FaceForm[], Triangle[{PA, PB, PC}]},
  {{PA, PB, PC} /. {x_, y_} :> {Blue, PointSize[0.02], Point[{x, y}]}},
  {centers /. {x_, y_} :> {Red, PointSize[0.01], Point[{x, y}]}},
  Text[#[[1]], #[[2]], -1.5 Sign@#[[2]]] & /@ 
   Transpose@{names, centers}
  ], AspectRatio -> Automatic, Axes -> True
 ]
Print /@ centers;
```
![](https://i.postimg.cc/WzQSTTw3/etc.png)

There are some other helpful tools as well, but most are not documented yet.
