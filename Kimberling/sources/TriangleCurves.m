TriangleCurves := Association["KiepertHyperbola" -> 
      symmetrizeEq[(b^2 - c^2)*y*z], "KiepertParabola" -> 
      symmetrizeEq[(b - c)^2*x^2 + 2*(a - b)*(a - c)*y*z], 
     "K001" -> symmetrizeEq[(a^2*(b^2 + c^2) + (b^2 - c^2)^2 - 2*a^4)*x*
        (c^2*y^2 - b^2*z^2)], "K002" -> symmetrizeEq[(c^2*y^2 - b^2*z^2)*x*
        (c^2*y^2 - b^2*z^2)], "K003" -> symmetrizeEq[a^2*(b^2 + c^2 - a^2)*x*
        (c^2*y^2 - b^2*z^2)], "K004" -> symmetrizeEq[
       (2*a^2*(b^2 + c^2) + (b^2 - c^2)^2 - 3*a^4)*x*(c^2*y^2 - b^2*z^2)], 
     "K005" -> symmetrizeEq[(a^2*(b^2 + c^2) - (b^2 - c^2)^2)*x*
        (c^2*y^2 - b^2*z^2)], "K006" -> symmetrizeEq[(a^2 + c^2 - b^2)*
        (a^2 + b^2 - c^2)*x*(c^2*y^2 - b^2*z^2)], 
     "K007" -> symmetrizeEq[(b^2 + c^2 - a^2)*x*(y^2 - z^2)], 
     "K008" -> symmetrizeEq[(b^4 + c^4 - a^4 - 2*b^2*c^2)*x*(y^2 - z^2)], 
     "K009" -> symmetrizeEq[a^4*(b^2 + c^2 - a^2)*y*z*(y - z)] + 
       2*(a^2 - b^2)*(b^2 - c^2)*(c^2 - a^2)*x*y*z, 
     "K010" -> symmetrizeEq[a^2*((y + z)/(y - z))], 
     "K011" -> pp*qq*rr*symmetrizeEq[x*(y^2 + z^2)] - 
        x*y*z*symmetrizeEq[pp*(qq^2 + rr^2)] /. 
      Thread[{pp, qq, rr} -> {1/(-a^2 + b^2 + c^2), 1/(a^2 - b^2 + c^2), 
         1/(a^2 + b^2 - c^2)}], 
     "K012" -> pp*qq*rr*symmetrizeEq[x*(y^2 + z^2)] - 
        x*y*z*symmetrizeEq[pp*(qq^2 + rr^2)] /. 
      Thread[{pp, qq, rr} -> KimberlingCenterB[6]], 
     "K013" -> pp*qq*rr*symmetrizeEq[x*(y^2 + z^2)] - 
        x*y*z*symmetrizeEq[pp*(qq^2 + rr^2)] /. 
      Thread[{pp, qq, rr} -> KimberlingCenterB[7]], 
     "K014" -> pp*qq*rr*symmetrizeEq[x*(y^2 + z^2)] - 
        x*y*z*symmetrizeEq[pp*(qq^2 + rr^2)] /. 
      Thread[{pp, qq, rr} -> KimberlingCenterB[1]], 
     "K015" -> symmetrizeEq[x*(y - z)^2], 
     "K016" -> symmetrizeEq[x*(y^2 + z^2)], 
     "K017" -> symmetrizeEq[(a^4 - b^2*c^2)*x*(c^2*y^2 + b^2*z^2)], 
     "K018" -> symmetrizeEq[(b^2 - c^2)*x*(c^2*y^2 + b^2*z^2)], 
     "K019" -> symmetrizeEq[a^2*(b^2 - c^2)*SA*x*(c^2*y^2 + b^2*z^2)], 
     "K020" -> symmetrizeEq[(a^4 + b^2*c^2)*x*(c^2*y^2 - b^2*z^2)], 
     "K021" -> symmetrizeEq[a^2*(b^2 - c^2)*x*(c^2*y^2 - b^2*z^2)], 
     "K022" -> (a^2*b^2*c^2 - 8*SA*SB*SC)*x*y*z + 
       symmetrizeEq[(b^2 + c^2 - 2*a^2)*x*(c^2*SC*y^2 + b^2*SB*z^2)], 
     "K023" -> symmetrizeEq[(b^2*c^2*(a^4 + (b^2 - c^2)^2) - 2*a^6*SA)*
        (x*(c^2*y^2 - b^2*z^2) + 2*y*z*(SB*y - SC*z))], 
     "K024" -> symmetrizeEq[a^2*x*(c^2*y^2 + b^2*z^2)], 
     "K025" -> symmetrizeEq[(SB*((b^2 - c^2)^2 + a^2*(b^2 - a^2))*y - 
          SC*((b^2 - c^2)^2 + a^2*(c^2 - a^2))*z)*y*z] - 
       (b^2 - c^2)*(c^2 - a^2)*(a^2 - b^2)*x*y*z, 
     "K026" -> symmetrizeEq[a^2*((b^2 - c^2)^2 - a^2*(b^2 + c^2))*y*z*
         (y - z)] + 2*(b^2 - c^2)*(c^2 - a^2)*(a^2 - b^2)*x*y*z, 
     "K027" -> symmetrizeEq[2*b^2*c^2*x*(SB^2*y^2 + SC^2*z^2)] + 
       (symmetrizeEq[b^2*c^2*(b^2 - c^2)^2] - a^2*b^2*c^2*(a^2 + b^2 + c^2))*
        x*y*z, "K028" -> symmetrizeEq[b^2*c^2*x*(y + z)*(SB*y - SC*z)], 
     "K029" -> (Cos[angleA/3] + 2*Cos[angleB/3]*Cos[angleC/3])*a*x*
        (c^2*y^2 - b^2*z^2) + (Cos[angleB/3] + 2*Cos[angleA/3]*Cos[angleC/3])*
        b*y*((-c^2)*x^2 + a^2*z^2) + (Cos[angleC/3] + 2*Cos[angleA/3]*
          Cos[angleB/3])*c*(b^2*x^2 - a^2*y^2)*z, 
     "K030" -> (Cos[angleA/3 + 2*(Pi/3)] + 2*Cos[angleB/3 + 2*(Pi/3)]*
          Cos[angleC/3 + 2*(Pi/3)])*a*x*(c^2*y^2 - b^2*z^2) + 
       (Cos[angleB/3 + 2*(Pi/3)] + 2*Cos[angleA/3 + 2*(Pi/3)]*
          Cos[angleC/3 + 2*(Pi/3)])*b*y*((-c^2)*x^2 + a^2*z^2) + 
       (Cos[angleC/3 + 2*(Pi/3)] + 2*Cos[angleA/3 + 2*(Pi/3)]*
          Cos[angleB/3 + 2*(Pi/3)])*c*z*(b^2*x^2 - a^2*y^2), 
     "K031" -> (Cos[angleA/3 - 2*(Pi/3)] + 2*Cos[angleB/3 - 2*(Pi/3)]*
          Cos[angleC/3 - 2*(Pi/3)])*a*x*(c^2*y^2 - b^2*z^2) + 
       (Cos[angleB/3 - 2*(Pi/3)] + 2*Cos[angleA/3 - 2*(Pi/3)]*
          Cos[angleC/3 - 2*(Pi/3)])*b*y*((-c^2)*x^2 + a^2*z^2) + 
       (Cos[angleC/3 - 2*(Pi/3)] + 2*Cos[angleA/3 - 2*(Pi/3)]*
          Cos[angleB/3 - 2*(Pi/3)])*c*z*(b^2*x^2 - a^2*y^2), 
     "K032" -> symmetrizeEq[(a^2*SA - 2*SB*SC)*x*(c^2*y^2 - b^2*z^2)] + 
       (SA*x - SB*y)*(SB*y - SC*z)*(SC*z - SA*x), 
     "K033" -> symmetrizeEq[(b + c - a)*x*(c*(a + b)*y^2 - b*(a + c)*z^2)], 
     "K034" -> symmetrizeEq[b*c*x*(y^2 - z^2)], 
     "K035" -> symmetrizeEq[x*((c^2*y^2 - b^2*z^2)/(c^2 - b^2))], 
     "K037" -> symmetrizeEq[((b^2 - c^2)^2 + a^2*(b^2 + c^2 - 2*a^2))*x*
        (y^2/(4*SC^2 - a^2*b^2) - z^2/(4*SB^2 - a^2*c^2))], 
     "K038" -> b^4*c^2*x^3 - b^2*c^4*x^3 + 2*a^2*b^2*c^2*x^2*y - 
       b^4*c^2*x^2*y - 2*a^2*c^4*x^2*y - b^2*c^4*x^2*y + 2*c^6*x^2*y + 
       a^4*c^2*x*y^2 - 2*a^2*b^2*c^2*x*y^2 + a^2*c^4*x*y^2 + 
       2*b^2*c^4*x*y^2 - 2*c^6*x*y^2 - a^4*c^2*y^3 + a^2*c^4*y^3 + 
       2*a^2*b^4*x^2*z - 2*b^6*x^2*z - 2*a^2*b^2*c^2*x^2*z + b^4*c^2*x^2*z + 
       b^2*c^4*x^2*z + 4*a^4*b^2*x*y*z - 4*a^2*b^4*x*y*z - 4*a^4*c^2*x*y*z + 
       4*b^4*c^2*x*y*z + 4*a^2*c^4*x*y*z - 4*b^2*c^4*x*y*z + 2*a^6*y^2*z - 
       2*a^4*b^2*y^2*z - a^4*c^2*y^2*z + 2*a^2*b^2*c^2*y^2*z - 
       a^2*c^4*y^2*z - a^4*b^2*x*z^2 - a^2*b^4*x*z^2 + 2*b^6*x*z^2 + 
       2*a^2*b^2*c^2*x*z^2 - 2*b^4*c^2*x*z^2 - 2*a^6*y*z^2 + a^4*b^2*y*z^2 + 
       a^2*b^4*y*z^2 + 2*a^4*c^2*y*z^2 - 2*a^2*b^2*c^2*y*z^2 + a^4*b^2*z^3 - 
       a^2*b^4*z^3, "K039" -> a^2*b^2*c^2*(b^2 - c^2)*(c^2 - a^2)*(a^2 - b^2)*
        x*y*z - symmetrizeEq[a^4*SA*(((a^2 - b^2)^2 + c^2*(a^2 - c^2))*b^2*
           z - ((a^2 - c^2)^2 + b^2*(a^2 - b^2))*c^2*y)*y*z], 
     "K040" -> symmetrizeEq[a*(b - c)*(b + c - a)*x*(c^2*y^2 + b^2*z^2)], 
     "K041" -> a^6*x^2*y - 3*a^4*b^2*x^2*y + 3*a^2*b^4*x^2*y - b^6*x^2*y + 
       3*a^4*c^2*x^2*y - 2*a^2*b^2*c^2*x^2*y - b^4*c^2*x^2*y - 
       a^2*c^4*x^2*y + 5*b^2*c^4*x^2*y - 3*c^6*x^2*y + a^6*x*y^2 - 
       3*a^4*b^2*x*y^2 + 3*a^2*b^4*x*y^2 - b^6*x*y^2 + a^4*c^2*x*y^2 + 
       2*a^2*b^2*c^2*x*y^2 - 3*b^4*c^2*x*y^2 - 5*a^2*c^4*x*y^2 + 
       b^2*c^4*x*y^2 + 3*c^6*x*y^2 - a^6*x^2*z - 3*a^4*b^2*x^2*z + 
       a^2*b^4*x^2*z + 3*b^6*x^2*z + 3*a^4*c^2*x^2*z + 2*a^2*b^2*c^2*x^2*z - 
       5*b^4*c^2*x^2*z - 3*a^2*c^4*x^2*z + b^2*c^4*x^2*z + c^6*x^2*z - 
       16*a^4*b^2*x*y*z + 16*a^2*b^4*x*y*z + 16*a^4*c^2*x*y*z - 
       16*b^4*c^2*x*y*z - 16*a^2*c^4*x*y*z + 16*b^2*c^4*x*y*z - 3*a^6*y^2*z - 
       a^4*b^2*y^2*z + 3*a^2*b^4*y^2*z + b^6*y^2*z + 5*a^4*c^2*y^2*z - 
       2*a^2*b^2*c^2*y^2*z - 3*b^4*c^2*y^2*z - a^2*c^4*y^2*z + 
       3*b^2*c^4*y^2*z - c^6*y^2*z - a^6*x*z^2 - a^4*b^2*x*z^2 + 
       5*a^2*b^4*x*z^2 - 3*b^6*x*z^2 + 3*a^4*c^2*x*z^2 - 
       2*a^2*b^2*c^2*x*z^2 - b^4*c^2*x*z^2 - 3*a^2*c^4*x*z^2 + 
       3*b^2*c^4*x*z^2 + c^6*x*z^2 + 3*a^6*y*z^2 - 5*a^4*b^2*y*z^2 + 
       a^2*b^4*y*z^2 + b^6*y*z^2 + a^4*c^2*y*z^2 + 2*a^2*b^2*c^2*y*z^2 - 
       3*b^4*c^2*y*z^2 - 3*a^2*c^4*y*z^2 + 3*b^2*c^4*y*z^2 - c^6*y*z^2, 
     "K042" -> (-a^10)*c^2*x^2*y + a^8*b^2*c^2*x^2*y - 4*a^4*b^6*c^2*x^2*y + 
       a^2*b^8*c^2*x^2*y + 3*b^10*c^2*x^2*y + 2*a^8*c^4*x^2*y - 
       5*a^6*b^2*c^4*x^2*y + 6*a^4*b^4*c^4*x^2*y + 5*a^2*b^6*c^4*x^2*y - 
       8*b^8*c^4*x^2*y + 2*a^6*c^6*x^2*y + 3*a^4*b^2*c^6*x^2*y - 
       9*a^2*b^4*c^6*x^2*y + 2*b^6*c^6*x^2*y - 4*a^4*c^8*x^2*y + 
       4*a^2*b^2*c^8*x^2*y + 6*b^4*c^8*x^2*y - a^2*c^10*x^2*y - 
       5*b^2*c^10*x^2*y + 2*c^12*x^2*y - 3*a^10*c^2*x*y^2 - 
       a^8*b^2*c^2*x*y^2 + 4*a^6*b^4*c^2*x*y^2 - a^2*b^8*c^2*x*y^2 + 
       b^10*c^2*x*y^2 + 8*a^8*c^4*x*y^2 - 5*a^6*b^2*c^4*x*y^2 - 
       6*a^4*b^4*c^4*x*y^2 + 5*a^2*b^6*c^4*x*y^2 - 2*b^8*c^4*x*y^2 - 
       2*a^6*c^6*x*y^2 + 9*a^4*b^2*c^6*x*y^2 - 3*a^2*b^4*c^6*x*y^2 - 
       2*b^6*c^6*x*y^2 - 6*a^4*c^8*x*y^2 - 4*a^2*b^2*c^8*x*y^2 + 
       4*b^4*c^8*x*y^2 + 5*a^2*c^10*x*y^2 + b^2*c^10*x*y^2 - 2*c^12*x*y^2 + 
       a^10*b^2*x^2*z - 2*a^8*b^4*x^2*z - 2*a^6*b^6*x^2*z + 4*a^4*b^8*x^2*z + 
       a^2*b^10*x^2*z - 2*b^12*x^2*z - a^8*b^2*c^2*x^2*z + 
       5*a^6*b^4*c^2*x^2*z - 3*a^4*b^6*c^2*x^2*z - 4*a^2*b^8*c^2*x^2*z + 
       5*b^10*c^2*x^2*z - 6*a^4*b^4*c^4*x^2*z + 9*a^2*b^6*c^4*x^2*z - 
       6*b^8*c^4*x^2*z + 4*a^4*b^2*c^6*x^2*z - 5*a^2*b^4*c^6*x^2*z - 
       2*b^6*c^6*x^2*z - a^2*b^2*c^8*x^2*z + 8*b^4*c^8*x^2*z - 
       3*b^2*c^10*x^2*z + 2*a^12*y^2*z - a^10*b^2*y^2*z - 4*a^8*b^4*y^2*z + 
       2*a^6*b^6*y^2*z + 2*a^4*b^8*y^2*z - a^2*b^10*y^2*z - 
       5*a^10*c^2*y^2*z + 4*a^8*b^2*c^2*y^2*z + 3*a^6*b^4*c^2*y^2*z - 
       5*a^4*b^6*c^2*y^2*z + a^2*b^8*c^2*y^2*z + 6*a^8*c^4*y^2*z - 
       9*a^6*b^2*c^4*y^2*z + 6*a^4*b^4*c^4*y^2*z + 2*a^6*c^6*y^2*z + 
       5*a^4*b^2*c^6*y^2*z - 4*a^2*b^4*c^6*y^2*z - 8*a^4*c^8*y^2*z + 
       a^2*b^2*c^8*y^2*z + 3*a^2*c^10*y^2*z + 3*a^10*b^2*x*z^2 - 
       8*a^8*b^4*x*z^2 + 2*a^6*b^6*x*z^2 + 6*a^4*b^8*x*z^2 - 
       5*a^2*b^10*x*z^2 + 2*b^12*x*z^2 + a^8*b^2*c^2*x*z^2 + 
       5*a^6*b^4*c^2*x*z^2 - 9*a^4*b^6*c^2*x*z^2 + 4*a^2*b^8*c^2*x*z^2 - 
       b^10*c^2*x*z^2 - 4*a^6*b^2*c^4*x*z^2 + 6*a^4*b^4*c^4*x*z^2 + 
       3*a^2*b^6*c^4*x*z^2 - 4*b^8*c^4*x*z^2 - 5*a^2*b^4*c^6*x*z^2 + 
       2*b^6*c^6*x*z^2 + a^2*b^2*c^8*x*z^2 + 2*b^4*c^8*x*z^2 - 
       b^2*c^10*x*z^2 - 2*a^12*y*z^2 + 5*a^10*b^2*y*z^2 - 6*a^8*b^4*y*z^2 - 
       2*a^6*b^6*y*z^2 + 8*a^4*b^8*y*z^2 - 3*a^2*b^10*y*z^2 + 
       a^10*c^2*y*z^2 - 4*a^8*b^2*c^2*y*z^2 + 9*a^6*b^4*c^2*y*z^2 - 
       5*a^4*b^6*c^2*y*z^2 - a^2*b^8*c^2*y*z^2 + 4*a^8*c^4*y*z^2 - 
       3*a^6*b^2*c^4*y*z^2 - 6*a^4*b^4*c^4*y*z^2 + 4*a^2*b^6*c^4*y*z^2 - 
       2*a^6*c^6*y*z^2 + 5*a^4*b^2*c^6*y*z^2 - 2*a^4*c^8*y*z^2 - 
       a^2*b^2*c^8*y*z^2 + a^2*c^10*y*z^2, 
     "K043" -> symmetrizeEq[a^2*(b^2 + c^2 - 2*a^2)*(y - z)*y*z], 
     "K044" -> symmetrizeEq[(SB*b^2 + SC*c^2)*a^2*SA^2*(SB*y - SC*z)*y*z], 
     "K045" -> symmetrizeEq[x*((y^2 - z^2)/(a^2*SA))], 
     "K047" -> symmetrizeEq[a^2*(y*(a^4 - 2*b^2*c^2 + 5*c^4 - 3*b^4 - 
            6*a^2*c^2 + 2*a^2*b^2) - z*(a^4 - 2*b^2*c^2 + 5*b^4 - 3*c^4 - 
            6*a^2*b^2 + 2*a^2*c^2))*y*z] - 8*(b^2 - c^2)*(c^2 - a^2)*
        (a^2 - b^2)*x*y*z, "K048" -> symmetrizeEq[
       b^2*c^2*(b^2 - c^2)*SA*x^3 - a^2*(b^2 + c^2 - 2*a^2)*
         (c^2*SC*y - b^2*SB*z)*y*z], "K049" -> symmetrizeEq[
       ((b^2 - c^2)^2 - a^2*(b^2 + c^2))*(SB*y - SC*z)*y*z], 
     "K050" -> symmetrizeEq[a^2*(4*SA^2 - b^2*c^2)*((b^2 - c^2)^2 - 
         a^2*(b^2 + c^2))*(SB*y - SC*z)*y*z], 
     "K051" -> cubicA1K[KimberlingCenterB[4]], 
     "K052" -> cubicA2K[KimberlingCenterB[115]], 
     "K053a" -> x*(c^2*y^2 - b^2*z^2) + 2*y*z*(SB*y - SC*z), 
     "K053b" -> y*(a^2*z^2 - c^2*x^2) + 2*z*x*(SC*z - SA*x), 
     "K053c" -> z*(b^2*x^2 - a^2*z^2) + 2*x*y*(SA*x - SB*y), 
     "K054" -> (a^4 - 2*a^2*b^2 + b^4 - a^2*c^2 - b^2*c^2)*
        ((a^2 - b^2 + 2*c^2)*x*y^2 - (-a^2 + b^2 + 2*c^2)*x^2*y) + 
       (a^4 - a^2*b^2 - 2*a^2*c^2 - b^2*c^2 + c^4)*
        ((-a^2 + 2*b^2 + c^2)*x^2*z - (a^2 + 2*b^2 - c^2)*x*z^2) + 
       ((-a^2)*b^2 + b^4 - a^2*c^2 - 2*b^2*c^2 + c^4)*
        ((2*a^2 + b^2 - c^2)*y*z^2 - (2*a^2 - b^2 + c^2)*y^2*z) + 
       2*(a^2 - b^2)*(a^2 - c^2)*(b^2 - c^2)*x*y*z, 
     "K055" -> symmetrizeEq[a^4*((a^2 - b^2 + 3*c^2)*y - (a^2 + 3*b^2 - c^2)*
           z)*y*z] + 2*(b^2 - c^2)*(c^2 - a^2)*(a^2 - b^2)*x*y*z, 
     "K056" -> a^4*b^2*(a^2*b^2 - b^4 - b^2*c^2 - 2*c^4)*x^2*y + 
       a^2*b^4*(a^4 + 2*c^4 + a^2*(-b^2 + c^2))*x*y^2 + 
       a^4*c^2*(2*b^4 - a^2*c^2 + b^2*c^2 + c^4)*x^2*z + 
       b^4*c^2*(-2*a^4 - a^2*c^2 + b^2*c^2 - c^4)*y^2*z - 
       a^2*c^4*(a^4 + 2*b^4 + a^2*(b^2 - c^2))*x*z^2 + 
       b^2*c^4*(2*a^4 + a^2*b^2 + b^4 - b^2*c^2)*y*z^2, 
     "K058" -> cubicO[KimberlingCenterB[1]], 
     "K059" -> cubicO[KimberlingCenterB[4]], 
     "K060" -> cubicO[KimberlingCenterB[5]], 
     "K061a" -> cubicO[KimberlingCenterB[13]], 
     "K061b" -> cubicO[KimberlingCenterB[14]], 
     "K062" -> cubicO[KimberlingCenterB[51]], 
     "K063" -> cubicO[KimberlingCenterB[111]], 
     "K064" -> cubicO[KimberlingCenterB[523]], 
     "K102" -> symmetrizeEq[a^2*x*(c^2*y^2 - b^2*z^2)], 
     "K401" -> (a^2 - b^2 - c^2)*(b^2 - c^2)*(a^4 - (b^2 - c^2)^2)^2*x^3 + 
       (a^2 + b^2 - c^2)^2*(a^8 - (b^2 - c^2)^3*(2*b^2 + c^2) - 
         a^6*(b^2 + 4*c^2) + a^4*(-3*b^4 + 13*b^2*c^2 - 2*c^4) + 
         a^2*(5*b^6 - 14*b^4*c^2 + 5*b^2*c^4 + 4*c^6))*x^2*y + 
       (a^2 + b^2 - c^2)^2*(2*a^8 - b^8 + 4*b^6*c^2 + 2*b^4*c^4 - 4*b^2*c^6 - 
         c^8 - 5*a^6*(b^2 + c^2) + a^4*(3*b^4 + 14*b^2*c^2 + 3*c^4) + 
         a^2*(b^6 - 13*b^4*c^2 - 5*b^2*c^4 + c^6))*x*y^2 + 
       (a^2 - c^2)*(a^2 - b^2 + c^2)*(a^4 - b^4 - 2*a^2*c^2 + c^4)^2*y^3 - 
       (a^2 - b^2 + c^2)^2*(a^8 - a^6*(4*b^2 + c^2) + (b^2 - c^2)^3*
          (b^2 + 2*c^2) + a^4*(-2*b^4 + 13*b^2*c^2 - 3*c^4) + 
         a^2*(4*b^6 + 5*b^4*c^2 - 14*b^2*c^4 + 5*c^6))*x^2*z + 
       8*(a^2 - b^2)*(a^2 - c^2)*(b^2 - c^2)*(a^6 - a^2*(b^2 - c^2)^2 - 
         a^4*(b^2 + c^2) + (b^2 - c^2)^2*(b^2 + c^2))*x*y*z + 
       (-a^2 + b^2 + c^2)^2*(a^8 - a^2*(4*b^2 - 5*c^2)*(b^2 - c^2)^2 + 
         a^6*(4*b^2 - c^2) + (b^2 - c^2)^3*(b^2 + 2*c^2) + 
         a^4*(-2*b^4 + 5*b^2*c^2 - 3*c^4))*y^2*z - (a^2 - b^2 + c^2)^2*
        (2*a^8 - b^8 - 4*b^6*c^2 + 2*b^4*c^4 + 4*b^2*c^6 - c^8 - 
         5*a^6*(b^2 + c^2) + a^4*(3*b^4 + 14*b^2*c^2 + 3*c^4) + 
         a^2*(b^6 - 5*b^4*c^2 - 13*b^2*c^4 + c^6))*x*z^2 - 
       (-a^2 + b^2 + c^2)^2*(a^8 - a^6*(b^2 - 4*c^2) + a^2*(5*b^2 - 4*c^2)*
          (b^2 - c^2)^2 - (b^2 - c^2)^3*(2*b^2 + c^2) + 
         a^4*(-3*b^4 + 5*b^2*c^2 - 2*c^4))*y*z^2 - 
       (a^2 - b^2)*(a^2 + b^2 - c^2)*(a^4 - 2*a^2*b^2 + b^4 - c^4)^2*z^3]
 
cubicA1K[{pp_, qq_, rr_}] := -((pp^2 + qq^2 + rr^2)*x*y*z) + 
     pp*qq*z*(x^2 + y^2 - z^2) + pp*rr*y*(x^2 - y^2 + z^2) + 
     qq*rr*x*(-x^2 + y^2 + z^2)
 
cubicA2K[{pp_, qq_, rr_}] := (pp + qq - rr)^2*x*(x - y)*y + 
     2*(pp^2 + qq^2 + rr^2 - 2*(pp*qq + pp*rr + qq*rr))*x*y*z + 
     (-pp + qq + rr)^2*y*(y - z)*z + (pp - qq + rr)^2*x*z*(-x + z)
 
cubicO[{u_, v_, w_}] := x*((c^2*u - 2*SB*w)*y^2 - (b^2*u - 2*SC*v)*z^2) + 
     y*((a^2*v - 2*SC*u)*z^2 - (c^2*v - 2*SA*w)*x^2) + 
     z*((b^2*w - 2*SA*v)*x^2 - (a^2*w - 2*SB*u)*y^2)
 
cubicspK[{p_, q_, r_}, {u_, v_, w_}] := 
    (u + v + w)*(r*((-b^2)*x^2 + a^2*y^2)*z + q*y*(c^2*x^2 - a^2*z^2) + 
       p*x*((-c^2)*y^2 + b^2*z^2)) - 2*(x + y + z)*(c^2*(q*u - p*v)*x*y + 
       b^2*(p*w - r*u)*x*z + a^2*(r*v - q*w)*y*z)
