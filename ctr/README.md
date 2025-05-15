Catalog of central triangles
----------------------------

###### Last change: 28 Jan 2025

Unlike [triangle centers](https://faculty.evansville.edu/ck6/encyclopedia/ETC.html) which are defined by a single triangle center function, central triangles [are defined by](https://mathworld.wolfram.com/CentralTriangle.html) two functions. There are lots (more than 500) triangles [used in ETC](https://faculty.evansville.edu/ck6/encyclopedia/IndexOfTrianglesReferencedInETC.html).

There are also many famous families of central triangles, which depend in some way on one or more triangle centers - e.g. cevian triangles, pedal triangles, etc.

Many other central triangles families do not have names. The goal of this site is to provide a list of such families, sometimes linking to their common properties. For each family we will at least include a description of how it arises geometrically and its barycentric coordinates.

We will follow a common naming scheme as follows. All families start with CTR followed by an index, e.g. CTR1.

*   If a family depends on one parameter, which is always a triangle center, having an ETC designation, e.g. X(m), then the name of any particular triangle in this series is **CTR1-m**. The listed barycentric coordinates will depend on X(m) = (u:v:w).
*   If a family depends on two parameters, e.g. CTR5, the naming will be **CTR5-m.n** and the listed barycentric coordinates will depend on two points X(m) = (u:v:w) and X(n) = (p:q:r).

Below, the variable points are usually P=(u:v:w) and Q=(p:q:r), unless stated differently.

### CTR1

Let P=(u:v:w) be a point. The CTR1(P) triangle is formed by the Aubert (Steiner) lines of quadrilaterals ABPC, BCPA, and CAPB. The barycentric coordinates of the A-vertex are:  

    (a^2*(-v+w)+(b^2-c^2)*(v+w))*(a^2*(v-w)+(b^2-c^2)*(2*u+v+w)) :
    a^4*(v*(u+2*v)+(u+v)*w+w^2)-2*a^2*(b^2*(v^2+u*w+2*v*w)+c^2*(2*v*(u+v)+(u+2*v)*w+w^2))-(b^2-c^2)*(c^2*((v+w)*(2*v+w)+u*(3*v+w))+b^2*(w*(v+w)+u*(v+3*w))) :
    a^4*(v*(u+v)+(u+v)*w+2*w^2)-2*a^2*(c^2*(u*v+2*v*w+w^2)+b^2*(v*(u+v)+2*(u+v)*w+2*w^2))+(b^2-c^2)*(c^2*(v*(3*u+v)+(u+v)*w)+b^2*(v*(u+v)+3*(u+v)*w+2*w^2))

### CTR2

Let P be a point and A'B'C' the anticevian triangle of P=(u:v:w). The CTR2(P) triangle is formed by the Aubert (Steiner) lines of quadrilaterals CABA', ABCB', BCAC'. The barycentric coordinates of the A-vertex are:  

    (a^2*(2*u-v-w)-(b^2-c^2)*(v-w))*(-((b^2-c^2)*(v-w))+a^2*(v+w)) :
    (b^2-c^2)*(v-w)*(b^2*(u+w)+c^2*(-u-2*v+w))-a^4*(-2*v^2+v*w-w^2+u*(v+w))-2*a^2*(b^2*(v^2+u*w-2*v*w)+c^2*(2*v^2-u*w-2*v*w+w^2)) :
    (b^2-c^2)*(c^2*(u+v)+b^2*(-u+v-2*w))*(v-w)-a^4*(-v^2+v*w-2*w^2+u*(v+w))-2*a^2*(c^2*(u*v-2*v*w+w^2)+b^2*(-(u*v)+v^2-2*v*w+2*w^2))

### CTR3

Let A1B1C1 be the cevian and A2B2C2 be the circlecevian triangle of P=(u:v:w). Circle (BC1C2) intersect BC at Ba, circle (CB1B2) intersect BC at Ca. Define Ab,Cb,Ac,Bc cyclically. Let Pa be the intersection of BaBc and CaCb and similarly define Pb and Pc. The CTR3(P) triangle is PaPbPc. The barycentric coordinates of the A-vertex are:  

    a^2*v*(b^2*u-c^2*u+a^2*v)*w*(-(b^2*u)+c^2*u+a^2*w)*(c^2*u*v*(2*v*w+u*(v+w))+w*(a^2*v*(-u^2+v*w)+b^2*u*(2*v*w+u*(v+w)))) : 
    u*(a^2*v-c^2*v-b^2*w)*(c^2*u*v+b^2*u*w+a^2*v*w)*(b^4*u^2*w*(v+w)-v*(c^2*u+a^2*w)*(-a^2*(u+v)*w+c^2*u*(v+w))+b^2*u*(a^2*v*w*(-u+w)+c^2*u*(v^2-w^2))) : 
    u*(-c^2*v+(a^2-b^2)*w)*(c^2*u*v+b^2*u*w+a^2*v*w)*(-b^4*u^2*w*(v+w)+v*(a^2*c^2*u*(-u+v)*w+a^4*v*w*(u+w)+c^4*u^2*(v+w))+b^2*u*(a^2*(u-v)*v*w+c^2*u*(-v^2+w^2)))

### CTR4

Let P be a point. PaPbPc cevian triangle of P=(u:v:w). APa intersects the circle (APbPc) at A' different from A. Define B', C' cyclically. The CTR4(P) triangle is A'B'C'. The barycentric coordinates of the A-vertex are:  

    c^2*u*v*(u+w)*(v+w)-(u+v)*w*(a^2*v*(u+w)-b^2*u*(v+w)) :
    b^2*v*(u+v)*w^2+c^2*v^3*(u+w) :
    b^2*(u+v)*w^3+c^2*v^2*w*(u+w)

Properties:
1. A'B'C' is orthologic to the orthoidal triangle of ABC wrt the circumcevian traingle of the isogonal conjugate of P.

### CTR5

Let XYZ be the anticevian triangle of P=(u:v:w). Denote with X' the Kimberling-Pavlov X-conjugate of Q=(p:q:r) and Q, and similarly define Y' and Z' (see the preamble to X(55917) in ETC for definitions). The CTR5(P,Q) triangle is X'Y'Z'. The barycentric coordinates of the A-vertex are:  

    u*(q*r*u+p*r*v-p*q*w)*(q*r*u-p*r*v+p*q*w) :
    v*(q*r*u-p*r*v+p*q*w)*(q*r*u+p*r*v+p*q*w) :
    w*(q*r*u+p*r*v-p*q*w)*(q*r*u+p*r*v+p*q*w)

### CTR6

Consider the three parabolas inscribed in ABC and tangent to the sides of the cevian triangle A1B1C1 of P=(u,v,w). Let Pa,Pb, Pc be their perspectors. CTR6 is the triangle PaPbPc. The barycentric coordinates of the A-vertex are:  

`(u + v) (u + w) : (u + v) (v - w) : (u + w) (-v + w)`

### CTR7

CTR7 is the barycentric sum of the cevian triangles of P=(u:v:w) and Q=(p:q:r). In other words, if the cevian traingles of P and Q are PaPbPc and QaQbQc, their barycentric sum is the triangle with vertices Pa+Qa, Pb+Qb, Pc+Qc. The barycentric coordinates of the A-vertex are:  

`0 : r v + q (2 v + w) : q w + r (v + 2 w)`

Note the following triangle equivalences:  
CTR7-1.1 = incentral  
CTR7-1.75 = medial  
CTR7-1.82 = pedal of X(182)  
CTR7-2.2 = medial  
CTR7-2.4 = pedal of X(5)  
CTR7-4.8 = pedal of X(10)  
CTR7-4.69 = medial  
CTR7-5.95 = medial  
CTR7-6.76 = medial  
CTR7-8.105 = cevian of X(80)  

### CTR8

Let PaPbPc be the cevian triangle of P=(u:v:w). Let circles (PPbC) and (PPcB) intersect for the second time at point A'. Similarly define B' and C'. A'B'C' is CTR8. The barycentric coordinates of the A-vertex are:  

    c^2 u (u + w) - (u + v) (-b^2 u + a^2 (u + w)) : 
    b^2 (u + v) (u + v + w) : 
    c^2 (u + w) (u + v + w)

Note the following triangle equivalences:  
CTR8-4 = orthic  
CTR8-8 = excenters-midpoints  
CTR8-146 = anticevian of X(30)  
CTR8-150 = Moses-Soddy  

### CTR9

CTR9 is the barycentric sum of the cevian triangles of P=(u:v:w) and the anticevian triangle of Q=(p:q:r). In other words, if these two traingles are denoted by PaPbPc and QaQbQc, their barycentric sum is the triangle with vertices Pa+Qa, Pb+Qb, Pc+Qc. The barycentric coordinates of the A-vertex are:  

`p (v + w) : p v - r v - q (2 v + w) : (p - q) w - r (v + 2 w)`

### CTR10

Let Pa be the point on the P-circumconic such that APa|| BC. Similarly define Pb and Pc. Then CTR10 is PaPbPc. 
PaPbPc is homothetic to the anticevian triangle of P.
The barycentric coordinates of the A-vertex are:  

`u : -v + w : v - w`

Note the following triangle equivalences:  
CTR10-1 = inner-Conway  
CTR10-2 = ABC  
CTR10-6 = 1st anti-circumperp  
CTR10-9 = Gemini 29  
CTR10-10 = Gemini 17  
CTR10-37 = Gemini 18

### CTR11

CTR11 is the triangle formed by triangle formed by:

*   intersection of BC with polar of A in circle (BCP)
*   intersection of AC with polar of B in circle (ACP)
*   intersection of AB with polar of C in circle (ABP).

The barycentric coordinates of the A-vertex are:  

`0 : b^2 u (u + v) - v (c^2 u + a^2 w) : (b^2 u + a^2 v) w - c^2 u (u + w)`

Note the following triangle equivalences:  
CTR11-1 = incentral  
CTR11-2 is degenerate  
CTR11-3 = cevian of X(110)  
CTR11-6 is degenerate

### CTR12

Let QaQbQc be the cevian triangle of Q. CTR12 is the triangle with vertices at the inversion poles of QbQc, QaQc, and QaQb wrt to the P-circumconic. The barycentric coordinates of the A-vertex are:  

    u (q r u + p r v + p q w) : 
    -v (q r u + p r v - p q w) : 
    -w (q r u - p r v + p q w)

Note the following triangle equivalences:  
CTR12-1.9 = 6th mixtilinear  
CTR12-1.88 = cevian of X(88)  
CTR12-1.100 = cevian of X(100)  
CTR12-2.2 = Gemini 111  
CTR12-2.75 = 2nd Conway  
CTR12-2.76 = anti-inverse-in-incircle  
CTR12-2.99 = Steiner  
CTR12-6.2 = Ara  
CTR12-6.99 = Steiner

### CTR13

Let PaPbPc be the P-circumconcevian triangle of Q. Let TaTbTc be the tangential triangle of PaPbPc wrt P-circumconic. Then TaTbTc is perspective to ABC and is called here CTR13. The barycentric coordinates of the A-vertex are:

    q u (r u + p w) + p v (r u + 2 p w) : 
    -v (q r u + p r v - p q w) : 
    -w (q r u - p r v + p q w)

Note the following triangle equivalences:  

CTR13-6.1 = 2nd circumperp tangential  
CTR13-6.3 = anti-Hutson intouch

### CTR14

Let PaPbPc be the cevian triangle of P. Let Ab and Ac be the reflections of Pa in the midpoints of BPb and CPc. Let AbAc intersect PbPc at point Ta, and similarly define Tb and Tc. TaTbTc is CTR14(P). This is a generalization of the 1st Van Khea-Pavlov triangle, which corresponds to CTR14-7. The barycentric coordinates of the A-vertex are:

`u (v^2 + w^2 + u (v + w)) : v w (u + w) : v (u + v) w`

Note the following triangle equivalences:  
CTR14-2 = Gemini 110  
CTR14-7 = 1st Van Khea-Pavlov

### CTR15

Let PaPbPc be the cevian triangle of P. CTR15(P) is the triangle formed by the trilinear polars of Pa wrt APbPc, Pb wrt BPcPa, Pc wrt CPaPb. The barycentric coordinates of the A-vertex are:

`-2 u : 3 v : 3 w`

### CTR16

Let PaPbPc be the cevian triangle of P. Let A' be the pole of PbPc in circle APbPc and similarly define B' anc C'. CTR16(P) is A'B'C'. The barycentric coordinates of the A-vertex are:

    -a^2 (u + v) (u + w) + u (b^2 (u + v) + c^2 (u + w)) : 
    b^2 w (u + v) : 
    c^2 v (u + w)

### CTR17

Let PaPbPc be the cevian triangle of P. Let A' be the exsimilrcenter of circles ABPa and ACPa. Similarly define B' and C'. CTR17(P) is A'B'C'. The barycentric coordinates of the A-vertex are:

    a^2 (c v - b w) : 
    b (-a^2 v + b (b + c) (v + w)) : 
    c (a^2 w - c (b + c) (v + w))

### CTR18

Given a point P, let Ab, Ac be the intersection of BC with the lines through A parallel to BP and CP respectively. Define Bc, Ba, Ca and Cb cyclically. These six points lie on a conic, called P-orthial.

CTR18(P) is the side-triangle of BcCaAb and CbAcBa.

The barycentric coordinates of the A-vertex are:

`u (v + w) (u + v + w) : v (-u + v - w) w : -v (u + v - w) w`

### CTR19

Given a point P, let Ab, Ac be the intersection of BC with the lines through A parallel to BP and CP respectively. Define Bc, Ba, Ca and Cb cyclically. These six points lie on a conic, called P-orthial.

CTR19(P) is the vertex-triangle of BcCaAb and CbAcBa.

The barycentric coordinates of the A-vertex are:

`-u (v + w) (u + v + w) : v (u + v - w) (u + w) : (u + v) w (u - v + w)`

### CTR20 and CTR21

Let P, Q be arbitrary points not on the sides of ABC.

Ab = intersection of AB with line through P parallel to AQ

Ac - intersection of AC with line through P parallel to AQ

Cyclically define Ba, Bc, Ca, Cb. 

CTR20(P,Q) is the cross-triangle of BcCaAb and CbAcBa.

The barycentric coordinates of the A-vertex are:

    p q v w + p r v w - q r u (v + w) :
    -q v (p w + r (v + w)) : 
    -r w (p v + q (v + w))`

CTR21(P,Q) is the side-triangle of BcCaAb and CbAcBa.

The barycentric coordinates of the A-vertex are:

    r (q^2 + (p + q) r) v^2 w + q (p q + (q + r) r ) v w^2 + u q r (r v^2 + w (p v + q w)) :
    q v (r v - q w) (p w + r (v + w)) :
    -r w (r v - q w) (p v + q (v + w))

### CTR22

Let PaPbPc be the cevian triangle of P and Q be any point. Let la be the line through Pa parallel to AQ, and similarly define lb and lc.

CTR22(P,Q) is the triangle bound by lines la, lb, lc.

The barycentric coordinates of the A-vertex are:

    p (r u (u + v) + q u (u + w) + p (u^2 - v w)):
    (p + r) (q u (v - w) + p v (u + w)):
    (p + q) (p (u + v) w + r u (-v + w))

### CTR23

Let Ab be the projection of A upon BP, Ac - the projection of A upon PC, and Ha the projection of A upon BC. Let Oa be the circumcenter of AbAcHa. Similarly define Ob and Oc. 

CTR23(P) is the triangle OaObOc.

The barycentric coordinates of the A-vertex are:

`u (c^2 v + b^2 w) : ((a^2 - c^2) v + b^2 (u + v)) w : v ((a^2 - b^2) w + c^2 (u + w))`

Note the following triangle equivalences:  
CTR23-1 = extouch-of-Fuhrmann

### CTR24
CTR24(P) is the anticomplementary triangle of the cevian triangle of P.

It is obviously perspective to the cevian triangle of P and to the cross-cevian triangle of P and Q.

The barycentric coordinates of the A-vertex are:

`u (v + w) (2 u + v + w) : -v (u - w) (u + w) : - w (u - v) (u + v)`

Note the following triangle equivalences:  
CTR24-2 = ABC  
CTR24-4 = anti-Wasat  
CTR24-7 = Ursa-minor  

### CTR25
CTR25(P,Q) is the inverse of the cevian triangle of P in the circumconic with perspector Q. For P=X(2) it coincides with the anticevian triangle of Q.

The barycentric coordinates of the A-vertex are:  
`p (p - q - r) v w : q v (r v - p w + q w) : r w (-p v + r v + q w)`

### CTR26
Let PaPbPc and QaQbQc be the cevian triangles of P and Q and X be any point. CTR26(P,Q,X) is the triangle formed by the points XPa∩QbQc, XPb∩QaQc and XPc∩QaQb.

The barycentric coordinates of the A-vertex are:  
`p (r v + q w) x : q (r v x + p w y - p v z) : r (q w x - p w y + p v z)`

### CTR27
CTR27(P,Q) is the anticevian triangle of Q wrt the cevian triangle of P (wrt to ABC).

The barycentric coordinates of the A-vertex are:  
`p u v w : v^2 (-r u + p w) : (-q u + p v) w^2`

### CTR28
If PaPbPc is the cevian triangle of P, CTR28(P) is the triangle with vertices at the orthocenters of APbPc, BPaPc, and CPaPb.

CTR28(P) is bilogic to ABC if P lies on the Lucas cubic K007.

The barycentric coordinates of the A-vertex are:

    a^4 (u^2 - v w) - 2 a^2 u (c^2 (u + v) + b^2 (u + w)) + (b^2 - c^2) (-c^2 (u^2 + 2 u v + v w) + b^2 (u^2 + 2 u w + v w)) :
    (a^2 - b^2 - c^2) (a^2 v (u + w) - c^2 v (u + w) + b^2 (-u v + 2 u w + v w)) : 
    (a^2 - b^2 - c^2) ((a^2 - b^2) (u + v) w + c^2 (2 u v - u w + v w))

Note the following triangle equivalences:  
CTR28-2 = Euler

### CTR29
Let PaPbPc and QaQbQc be the cevian triangles of P and Q. Let Ta be the projection of Qa upon APa, and similarly define Tb, Tc. Triangle TaTbTc is CTR29(P, Q).

The barycentric coordinates of the A-vertex are:

    c^2 (v + w) (2 p v + r v - q w) - b^2 (v + w) (r v - (2 p + q) w) + a^2 (r v (v - w) - 2 p v w + q w (-v + w)), 
    v (-a^2 (r v + q w) + b^2 (r v + q w + 2 r w) + c^2 (r v + q (2 v + w))), 
    w (-a^2 (r v + q w) + b^2 (r v + q w + 2 r w) + c^2 (r v + q (2 v + w)))

Note the following triangle equivalences:  
CTR29-2.4 = 4th Brocard  
CTR29-3.6 = 7th Brocard  
CTR29-4.2 = orthocentroidal  
CTR29-4.6 = 1st orthosymmedial  
CTR29-6.3 = 2nd Brocard  
CTR29-6.4 = 2nd orthosymmedial  
CTR29-7.1 = CTR4-7  
CTR29-8.40 = CTR4-8  
CTR29-98.99 = ABC  
CTR29-4.2 = orthocentroidal  
CTR29-2.3 = CTR4-2  

### CTR30
Let P, Q be two points and QaQbQc be the pedal triangle of Q. CTR30(P, Q) is the triangle with vertices AP∩QQa, BP∩QQb, and CP∩QQc.

The barycentric coordinates of the A-vertex are:

    -((b^2 - c^2) p (v + w)) + a^2 (2 r v + p (v - w) - 2 q w), 
    (a^2 (q - r) - (b^2 - c^2) (q + r)) v, 
    (a^2 (q - r) - (b^2 - c^2) (q + r)) w

Note the following triangle equivalences:  
CTR30-2.1 = 1st Savin  
CTR30-6.1 = CTR20-1.4  
CTR30-7.1 = intouch  
CTR30-8.1 = Hutson intouch  
CTR30-55.1 = CTR26-7.1.1  
CTR30-57.1 = Bevan antipodal  
CTR30-65.1 = anti-tangential-midarc  
CTR30-84.1 = hexyl  

### CTR31
Let PaPbPc be the cevian triangle of P and Q is a point not on the sides of ABC. Through Pa construct a line parallel to CQ and let its intersection with AB be Ab. Through Pa construct a line parallel to BQ and let its intersection with AC be Ac. Let Na be the reflection of Pa in the midpoint of AbAc. Similarly construct Nb and Nc. NaNbNc is CTR31(P,Q).

CTR31(P,Q) is perspective to ABC and the perspector is the Dao conjugate of the complement of Q, and P. 
CTR31(P,Q) is also perspective to the cevian triangle of Q.

The barycentric coordinates of the A-vertex are:

`p (q v + r w + p (v + w)) : q (p + r) w : (p + q) r v`

Note the following triangle equivalences:  
CTR31-1.1 = CTR14-1  
CTR31-1.2 = Gemini 16  
CTR31-1.7 = 1st Savin  
CTR31-2.2 = Gemini 110  
CTR31-2.4 = 2nd anti-Conway  
CTR31-2.7 = inverse-in-incircle  
CTR31-3.2 = CTR26-2.2.5  
CTR31-3.4 = orthic axes  
CTR31-4.2 = 6th anti-mixtilinear  
CTR31-5.2 = CTR26-5.2.2  
CTR31-6.2 = CTR26-6.2.2  
CTR31-6.6 = CTR14-6  
CTR31-7.1 = CTR26-4.1.2  
CTR31-7.2 = 2nd Zaniah  
CTR31-7.7 = 1st Van Khea-Pavlov  
CTR31-8.2 = 1st Zaniah  
CTR31-8.7 = CTR26-7.7.65  
CTR31-8.8 = CTR14-8  
CTR31-10.10 = CTR14-10  
CTR31-10.75 = Gemini 13  
CTR31-42.1 = CTR26-1.1.38 

### CTR32
Given a triangle ABC and two points P and Q, it is well known that there are four conics through the two points and tangent to the three sidelines of ABC.
See https://garciacapitan.blogspot.com/2025/05/inconics-through-two-points.html for more information. From these conics, one is a central conic, the other three are not central.
Let Q be the isogonal conjugate of P. Let Oa, Ob, Oc, be the vertices of the non-central conics.

CTR32(P) is triangle OaObOc.  

Note that CTR32(X) and CTR32(Y) are always perpspective for any X, Y.   

In addition, CTR32 is perspective to the following triangles: medial, excentral, 2nd Savin, 2nd Sharygin, 9th Vijay-Paasche-Hutson, Gemini 2, Gemini 15, Gemini 60, CTR9-1.37, CTR12-1.57, CTR13-1.57, CTR26-7.7.57, CTR27-2.37, CTR30-57.3, UCFT-of-extangents, UCFT-of-intangents

The barycentric coordinates of the A-vertex are:

    2 a (b + c) u v w + a^2 v w (v + w) + u^2 (c^2 v + b^2 w), 
    2 b (a - c) u v w + b^2 u w (u + w) + v^2 (c^2 u + a^2 w), 
    2 c (a - b) u v w + c^2 u v (u + v) + w^2 (b^2 u + a^2 v)
