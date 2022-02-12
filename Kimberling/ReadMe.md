# Kimberling Center Tools

This repo provides you the `KimberlingCenter[k, A, B, C]` function which given an integer k and three points A, B and C, calculates the k-th triangle center according to Kimberling's [encyclopedia of triangle centers](https://faculty.evansville.edu/ck6/encyclopedia/etc.html).

Note that currrently k<=2299 and it is TESTED! 

Here is a simple example which plots a few random triangle centers:
```
SetDirectory[NotebookDirectory[]];
Get["sources/KimberlingTrilinears1000.m"]; 
Get["sources/TriangleExpressions.m"]; 
Get["sources/KimberlingPoints.m"];

PA = {0, 0}; PB = {3, 0}; PC = {1, 2};
indices = {1, 3, 7, 9, 14, 97, 111};
centers = Table[KimberlingCenter[i, PA, PB, PC], {i, indices}] // Simplify;
names = Table["X" <> TextString[n], {n, indices}];
Graphics[Join[
  {EdgeForm[{Thin, Black}], FaceForm[], Triangle[{PA, PB, PC}]},
  {{PA, PB, PC} /. {x_, y_} :> {Blue, PointSize[0.02], Point[{x, y}]}},
  {centers /. {x_, y_} :> {Red, PointSize[0.01], Point[{x, y}]}},
  Text[#[[1]], #[[2]], -1.5 Sign@#[[2]]] & /@ 
   Transpose@{names, centers}
  ], AspectRatio -> Automatic, Axes -> True
 ]
```
![](https://i.postimg.cc/rFpHKVnf/etc.jpg)

There are some other helpful tools as well, but they are not documented yet.