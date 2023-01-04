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
     "K011" -> p*q*r*symmetrizeEq[x*(y^2 + z^2)] - 
        x*y*z*symmetrizeEq[p*(q^2 + r^2)] /. 
      Thread[{p, q, r} -> {1/(-a^2 + b^2 + c^2), 1/(a^2 - b^2 + c^2), 
         1/(a^2 + b^2 - c^2)}], "K012" -> p*q*r*symmetrizeEq[x*(y^2 + z^2)] - 
        x*y*z*symmetrizeEq[p*(q^2 + r^2)] /. 
      Thread[{p, q, r} -> KimberlingCenterCN[6]], 
     "K013" -> p*q*r*symmetrizeEq[x*(y^2 + z^2)] - 
        x*y*z*symmetrizeEq[p*(q^2 + r^2)] /. 
      Thread[{p, q, r} -> KimberlingCenterCN[7]], 
     "K014" -> p*q*r*symmetrizeEq[x*(y^2 + z^2)] - 
        x*y*z*symmetrizeEq[p*(q^2 + r^2)] /. 
      Thread[{p, q, r} -> KimberlingCenterCN[1]], 
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
     "K051" -> cubicA1K[KimberlingCenterCN[4]], 
     "K052" -> cubicA2K[KimberlingCenterCN[115]], 
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
     "K058" -> cubicO[KimberlingCenterCN[1]], 
     "K059" -> cubicO[KimberlingCenterCN[4]], 
     "K060" -> cubicO[KimberlingCenterCN[5]], 
     "K061a" -> cubicO[KimberlingCenterCN[13]], 
     "K061b" -> cubicO[KimberlingCenterCN[14]], 
     "K062" -> cubicO[KimberlingCenterCN[51]], 
     "K063" -> cubicO[KimberlingCenterCN[111]], 
     "K064" -> cubicO[KimberlingCenterCN[523]], 
     "K065" -> symmetrizeEq[x*((b^4 + c^4 - a^4 - c^2*(a^2 + 2*b^2 - 2*c^2))*
          y^2 - (b^4 + c^4 - a^4 - b^2*(a^2 - 2*b^2 + 2*c^2))*z^2)], 
     "K066a" -> a^6*x^2*y - 3*a^4*b^2*x^2*y + 3*a^2*b^4*x^2*y - b^6*x^2*y - 
       4*a^4*c^2*x^2*y - 2*a^2*b^2*c^2*x^2*y + 6*b^4*c^2*x^2*y + 
       5*a^2*c^4*x^2*y - 3*b^2*c^4*x^2*y - 2*c^6*x^2*y + a^6*x*y^2 - 
       3*a^4*b^2*x*y^2 + 3*a^2*b^4*x*y^2 - b^6*x*y^2 - 6*a^4*c^2*x*y^2 + 
       2*a^2*b^2*c^2*x*y^2 + 4*b^4*c^2*x*y^2 + 3*a^2*c^4*x*y^2 - 
       5*b^2*c^4*x*y^2 + 2*c^6*x*y^2 - a^6*x^2*z + 4*a^4*b^2*x^2*z - 
       5*a^2*b^4*x^2*z + 2*b^6*x^2*z + 3*a^4*c^2*x^2*z + 
       2*a^2*b^2*c^2*x^2*z + 3*b^4*c^2*x^2*z - 3*a^2*c^4*x^2*z - 
       6*b^2*c^4*x^2*z + c^6*x^2*z - 2*a^6*y^2*z + 5*a^4*b^2*y^2*z - 
       4*a^2*b^4*y^2*z + b^6*y^2*z - 3*a^4*c^2*y^2*z - 2*a^2*b^2*c^2*y^2*z - 
       3*b^4*c^2*y^2*z + 6*a^2*c^4*y^2*z + 3*b^2*c^4*y^2*z - c^6*y^2*z - 
       a^6*x*z^2 + 6*a^4*b^2*x*z^2 - 3*a^2*b^4*x*z^2 - 2*b^6*x*z^2 + 
       3*a^4*c^2*x*z^2 - 2*a^2*b^2*c^2*x*z^2 + 5*b^4*c^2*x*z^2 - 
       3*a^2*c^4*x*z^2 - 4*b^2*c^4*x*z^2 + c^6*x*z^2 + 2*a^6*y*z^2 + 
       3*a^4*b^2*y*z^2 - 6*a^2*b^4*y*z^2 + b^6*y*z^2 - 5*a^4*c^2*y*z^2 + 
       2*a^2*b^2*c^2*y*z^2 - 3*b^4*c^2*y*z^2 + 4*a^2*c^4*y*z^2 + 
       3*b^2*c^4*y*z^2 - c^6*y*z^2 + Sqrt[3]*Sqrt[(a + b - c)*(a - b + c)*
          (-a + b + c)*(a + b + c)]*(b^2*c^2*x*(x - y - z)*(y - z) + 
         a^2*((-b^2)*(x - y)*(x + y - z)*z + c^2*y*(x - z)*(x - y + z)) - 
         a^4*(y - z)*(x^2 + 2*y*z + x*(y + z)) + b^4*(x - z)*
          (y*(y + z) + x*(y + 2*z)) + c^4*(y*z*(y + z) - x^2*(2*y + z) + 
           x*(2*y^2 - z^2))), "K066b" -> a^6*x^2*y - 3*a^4*b^2*x^2*y + 
       3*a^2*b^4*x^2*y - b^6*x^2*y - 4*a^4*c^2*x^2*y - 2*a^2*b^2*c^2*x^2*y + 
       6*b^4*c^2*x^2*y + 5*a^2*c^4*x^2*y - 3*b^2*c^4*x^2*y - 2*c^6*x^2*y + 
       a^6*x*y^2 - 3*a^4*b^2*x*y^2 + 3*a^2*b^4*x*y^2 - b^6*x*y^2 - 
       6*a^4*c^2*x*y^2 + 2*a^2*b^2*c^2*x*y^2 + 4*b^4*c^2*x*y^2 + 
       3*a^2*c^4*x*y^2 - 5*b^2*c^4*x*y^2 + 2*c^6*x*y^2 - a^6*x^2*z + 
       4*a^4*b^2*x^2*z - 5*a^2*b^4*x^2*z + 2*b^6*x^2*z + 3*a^4*c^2*x^2*z + 
       2*a^2*b^2*c^2*x^2*z + 3*b^4*c^2*x^2*z - 3*a^2*c^4*x^2*z - 
       6*b^2*c^4*x^2*z + c^6*x^2*z - 2*a^6*y^2*z + 5*a^4*b^2*y^2*z - 
       4*a^2*b^4*y^2*z + b^6*y^2*z - 3*a^4*c^2*y^2*z - 2*a^2*b^2*c^2*y^2*z - 
       3*b^4*c^2*y^2*z + 6*a^2*c^4*y^2*z + 3*b^2*c^4*y^2*z - c^6*y^2*z - 
       a^6*x*z^2 + 6*a^4*b^2*x*z^2 - 3*a^2*b^4*x*z^2 - 2*b^6*x*z^2 + 
       3*a^4*c^2*x*z^2 - 2*a^2*b^2*c^2*x*z^2 + 5*b^4*c^2*x*z^2 - 
       3*a^2*c^4*x*z^2 - 4*b^2*c^4*x*z^2 + c^6*x*z^2 + 2*a^6*y*z^2 + 
       3*a^4*b^2*y*z^2 - 6*a^2*b^4*y*z^2 + b^6*y*z^2 - 5*a^4*c^2*y*z^2 + 
       2*a^2*b^2*c^2*y*z^2 - 3*b^4*c^2*y*z^2 + 4*a^2*c^4*y*z^2 + 
       3*b^2*c^4*y*z^2 - c^6*y*z^2 + Sqrt[3]*Sqrt[(a + b - c)*(a - b + c)*
          (-a + b + c)*(a + b + c)]*(b^2*c^2*x*(y - z)*(-x + y + z) + 
         a^2*(b^2*(x - y)*(x + y - z)*z - c^2*y*(x - z)*(x - y + z)) + 
         a^4*(y - z)*(x^2 + 2*y*z + x*(y + z)) + b^4*(-x + z)*
          (y*(y + z) + x*(y + 2*z)) + c^4*((-y)*z*(y + z) + x^2*(2*y + z) + 
           x*(-2*y^2 + z^2))), "K067" -> 
      (-c^2)*(-a^10 + c^2*(b^2 - c^2)^4 - 5*a^6*c^2*(b^2 + 2*c^2) + 
         a^8*(2*b^2 + 5*c^2) + a^4*(-2*b^6 + b^4*c^2 + 10*c^6) + 
         a^2*(b^8 - 2*b^6*c^2 - b^4*c^4 + 7*b^2*c^6 - 5*c^8))*x^2*y + 
       c^2*(-(b^2 - c^2)^5 + a^8*(b^2 + c^2) - 2*a^6*(b^4 + b^2*c^2 + 
           2*c^4) + a^4*c^2*(b^4 - b^2*c^2 + 6*c^4) + 
         a^2*(2*b^8 - 5*b^6*c^2 + 7*b^2*c^6 - 4*c^8))*x*y^2 + 
       b^2*(-a^10 + b^2*(b^2 - c^2)^4 - 5*a^6*b^2*(2*b^2 + c^2) + 
         a^8*(5*b^2 + 2*c^2) + a^4*(10*b^6 + b^2*c^4 - 2*c^6) + 
         a^2*(-5*b^8 + 7*b^6*c^2 - b^4*c^4 - 2*b^2*c^6 + c^8))*x^2*z + 
       (-a^12 + a^2*b^2*(b^2 - c^2)^3*(b^2 + c^2) + a^10*(5*b^2 + 4*c^2) - 
         a^8*(10*b^4 + 7*b^2*c^2 + 6*c^4) + a^6*(10*b^6 + b^2*c^4 + 4*c^6) - 
         a^4*(5*b^8 - 5*b^6*c^2 + b^4*c^4 - 2*b^2*c^6 + c^8))*y^2*z - 
       b^2*((b^2 - c^2)^5 + a^8*(b^2 + c^2) + a^4*b^2*(6*b^4 - b^2*c^2 + 
           c^4) - 2*a^6*(2*b^4 + b^2*c^2 + c^4) + 
         a^2*(-4*b^8 + 7*b^6*c^2 - 5*b^2*c^6 + 2*c^8))*x*z^2 + 
       a^2*(a^10 + c^2*(b^2 - c^2)^3*(b^2 + c^2) - a^8*(4*b^2 + 5*c^2) + 
         a^6*(6*b^4 + 7*b^2*c^2 + 10*c^4) - a^4*(4*b^6 + b^4*c^2 + 10*c^6) + 
         a^2*(b^8 - 2*b^6*c^2 + b^4*c^4 - 5*b^2*c^6 + 5*c^8))*y*z^2, 
     "K068" -> symmetrizeEq[(b^2 + c^2 - 2*a^2)*x*((a^2 - b^2)*y^2 + 
         (c^2 - a^2)*z^2)], "K069" -> 
      (a^2 - b^2)*c^2*(a^8 + a^6*(b^2 - 2*c^2) - a^2*(b^2 - 2*c^2)*
          (b^2 - c^2)^2 + (b^2 - c^2)^3*(2*b^2 + c^2) - 
         3*a^4*(b^4 - b^2*c^2))*x^2*y + (a^2 - b^2)*c^2*
        (2*a^8 + (b^2 - c^2)^3*(b^2 + c^2) - a^6*(b^2 + 5*c^2) + 
         a^4*(-3*b^4 + 4*b^2*c^2 + 3*c^4) + a^2*(b^6 + 3*b^4*c^2 - 
           5*b^2*c^4 + c^6))*x*y^2 - b^2*(a^2 - c^2)*
        (a^8 + 3*a^4*c^2*(b^2 - c^2) + a^2*(b^2 - c^2)^2*(2*b^2 - c^2) + 
         a^6*(-2*b^2 + c^2) - (b^2 - c^2)^3*(b^2 + 2*c^2))*x^2*z - 
       2*(a^2 - b^2)*(a^2 - c^2)*(b^2 - c^2)*(a^6 - a^4*(b^2 + c^2) + 
         (b^2 - c^2)^2*(b^2 + c^2) - a^2*(b^4 - 6*b^2*c^2 + c^4))*x*y*z + 
       a^2*(b^2 - c^2)*(-a^8 + a^6*(2*b^2 + c^2) + (b^2 - c^2)^2*
          (b^4 + 3*b^2*c^2 + 2*c^4) + a^4*(-5*b^2*c^2 + 3*c^4) + 
         a^2*(-2*b^6 + 3*b^4*c^2 + 4*b^2*c^4 - 5*c^6))*y^2*z - 
       b^2*(a^2 - c^2)*(2*a^8 - (b^2 - c^2)^3*(b^2 + c^2) - 
         a^6*(5*b^2 + c^2) + a^4*(3*b^4 + 4*b^2*c^2 - 3*c^4) + 
         a^2*(b^6 - 5*b^4*c^2 + 3*b^2*c^4 + c^6))*x*z^2 + 
       a^2*(b^2 - c^2)*(-a^8 + a^6*(b^2 + 2*c^2) + a^4*(3*b^4 - 5*b^2*c^2) + 
         (b^2 - c^2)^2*(2*b^4 + 3*b^2*c^2 + c^4) + 
         a^2*(-5*b^6 + 4*b^4*c^2 + 3*b^2*c^4 - 2*c^6))*y*z^2, 
     "K070a" -> symmetrizeEq[(1 - Tan[A])*x*(y^2*Tan[C] - z^2*Tan[B])], 
     "K070b" -> symmetrizeEq[(1 + Tan[A])*x*(y^2*Tan[C] - z^2*Tan[B])], 
     "K071" -> symmetrizeEq[(SB*((a^2 - b^2)^2 - c^2*(a^2 + b^2))*y - 
          SC*((a^2 - c^2)^2 - b^2*(a^2 + c^2))*z)*x^2] + 
       2*(a^2 - b^2)*(b^2 - c^2)*(c^2 - a^2)*x*y*z, 
     "K072" -> symmetrizeEq[(b^2 - c^2)*(b^4 + c^4 - a^4 - b^2*c^2)*x*
         (c^2*y^2 + b^2*z^2)] - 2*(a^2 - b^2)*(b^2 - c^2)*(c^2 - a^2)*SW*x*y*
        z, "K073" -> symmetrizeEq[a^2*SA*x*(c^4*(4*SC^2 - a^2*b^2)*y^2 - 
         b^4*(4*SB^2 - c^2*a^2)*z^2)], 
     "K074" -> 4*SA*SB*SC*symmetrizeEq[x*(c^2*y^2 + b^2*z^2)] - 
       x*y*z*symmetrizeEq[2*b^2*c^2*(a^4 + b^2*c^2) - 
          a^6*(2*b^2 + 2*c^2 - a^2)], "K075" -> 
      cubicKp[KimberlingCenterCN[32]], 
     "K076" -> cubicKc[KimberlingCenterCN[32]], 
     "K077" -> cubicKp[KimberlingCenterCN[6]], 
     "K078" -> cubicKc[KimberlingCenterCN[6]], 
     "K080" -> multiCollect[cubicSpk[KimberlingCenterCN[3], 
        KimberlingCenterCN[550]], {x, y, z}], 
     "K081" -> 2*c^2*(a^6 - a^2*(b^2 - c^2)^2 - a^4*(b^2 + c^2) + 
         (b^2 - c^2)^2*(b^2 + c^2))*x^2*y + 2*c^2*(a^6 - a^2*(b^2 - c^2)^2 - 
         a^4*(b^2 + c^2) + (b^2 - c^2)^2*(b^2 + c^2))*x*y^2 + 
       2*b^2*(a^6 - a^2*(b^2 - c^2)^2 - a^4*(b^2 + c^2) + 
         (b^2 - c^2)^2*(b^2 + c^2))*x^2*z + (3*a^8 - 4*a^6*(b^2 + c^2) - 
         4*a^2*(b^2 - c^2)^2*(b^2 + c^2) + 2*a^4*(b^2 + c^2)^2 + 
         (b^2 - c^2)^2*(3*b^4 + 2*b^2*c^2 + 3*c^4))*x*y*z + 
       2*(a^8 - a^4*(b^2 - c^2)^2 - a^6*(b^2 + c^2) + a^2*(b^2 - c^2)^2*
          (b^2 + c^2))*y^2*z + 2*b^2*(a^6 - a^2*(b^2 - c^2)^2 - 
         a^4*(b^2 + c^2) + (b^2 - c^2)^2*(b^2 + c^2))*x*z^2 + 
       2*(a^8 - a^4*(b^2 - c^2)^2 - a^6*(b^2 + c^2) + a^2*(b^2 - c^2)^2*
          (b^2 + c^2))*y*z^2, "K082" -> symmetrizeEq[a^2*(y + z)*y*z], 
     "K084" -> multiCollect[cubicSpk[KimberlingCenterCN[512], 
        KimberlingCenterCN[99]], {x, y, z}], 
     "K085" -> b*c^2*(a*(b - 2*c) + b*(b + c))*x^2*y + 
       a*c^2*(a^2 - 2*b*c + a*(b + c))*x*y^2 + 
       b^2*c*(a*(-2*b + c) + c*(b + c))*x^2*z - 2*a*b*c*(a^2 + b^2 + c^2)*x*y*
        z + a^2*c*(a*(-2*b + c) + c*(b + c))*y^2*z + 
       a*b^2*(a^2 - 2*b*c + a*(b + c))*x*z^2 + 
       a^2*b*(a*(b - 2*c) + b*(b + c))*y*z^2, 
     "K086" -> multiCollect[cubicSpk[KimberlingCenterCN[519], 
        KimberlingCenterCN[1]], {x, y, z}], 
     "K087" -> (a^6 + 2*b^6 - 3*b^4*c^2 - b^2*c^4 + c^6 - a^4*(b^2 + 2*c^2) + 
         a^2*(-3*b^4 + 8*b^2*c^2 - 2*c^4))*x^2*y + 
       (2*a^6 + b^6 - 2*b^4*c^2 - 2*b^2*c^4 + c^6 - 3*a^4*(b^2 + c^2) - 
         a^2*(b^4 - 8*b^2*c^2 + c^4))*x*y^2 + 
       (a^6 + b^6 - b^4*c^2 - 3*b^2*c^4 + 2*c^6 - a^4*(2*b^2 + c^2) + 
         a^2*(-2*b^4 + 8*b^2*c^2 - 3*c^4))*x^2*z + 
       (4*a^6 + 4*b^6 - 6*b^4*c^2 - 6*b^2*c^4 + 4*c^6 - 6*a^4*(b^2 + c^2) - 
         6*a^2*(b^4 - 4*b^2*c^2 + c^4))*x*y*z + 
       (a^6 + b^6 - b^4*c^2 - 3*b^2*c^4 + 2*c^6 - a^4*(2*b^2 + c^2) + 
         a^2*(-2*b^4 + 8*b^2*c^2 - 3*c^4))*y^2*z + 
       (2*a^6 + b^6 - 2*b^4*c^2 - 2*b^2*c^4 + c^6 - 3*a^4*(b^2 + c^2) - 
         a^2*(b^4 - 8*b^2*c^2 + c^4))*x*z^2 + 
       (a^6 + 2*b^6 - 3*b^4*c^2 - b^2*c^4 + c^6 - a^4*(b^2 + 2*c^2) + 
         a^2*(-3*b^4 + 8*b^2*c^2 - 2*c^4))*y*z^2, 
     "K088" -> (a^4 + a^2*(2*b^2 - 7*c^2) + (b^2 + c^2)^2)*x^2*y + 
       (a^4 + b^4 - 7*b^2*c^2 + c^4 + 2*a^2*(b^2 + c^2))*x*y^2 + 
       (a^4 + (b^2 + c^2)^2 + a^2*(-7*b^2 + 2*c^2))*x^2*z - 
       6*(a^4 + b^4 - b^2*c^2 + c^4 - a^2*(b^2 + c^2))*x*y*z + 
       (a^4 + (b^2 + c^2)^2 + a^2*(-7*b^2 + 2*c^2))*y^2*z + 
       (a^4 + b^4 - 7*b^2*c^2 + c^4 + 2*a^2*(b^2 + c^2))*x*z^2 + 
       (a^4 + a^2*(2*b^2 - 7*c^2) + (b^2 + c^2)^2)*y*z^2, 
     "K089" -> (4*b^2*c^2 + a^2*(4*b^2 - 5*c^2))*x^2*y + 
       (-5*b^2*c^2 + 4*a^2*(b^2 + c^2))*x*y^2 + 
       (4*b^2*c^2 + a^2*(-5*b^2 + 4*c^2))*x^2*z - 
       6*(b^2*c^2 + a^2*(b^2 + c^2))*x*y*z + 
       (4*b^2*c^2 + a^2*(-5*b^2 + 4*c^2))*y^2*z + 
       (-5*b^2*c^2 + 4*a^2*(b^2 + c^2))*x*z^2 + 
       (4*b^2*c^2 + a^2*(4*b^2 - 5*c^2))*y*z^2, 
     "K090" -> (a - 3*b + c)*x^2*y + (-3*a + b + c)*x*y^2 + 
       (a + b - 3*c)*x^2*z + 2*(a + b + c)*x*y*z + (a + b - 3*c)*y^2*z + 
       (-3*a + b + c)*x*z^2 + (a - 3*b + c)*y*z^2, 
     "K091" -> (a^6 + (b^2 - c^2)^2*(b^2 + c^2) - a^4*(b^2 + 2*c^2) - 
         a^2*(b^4 + b^2*c^2 + 2*c^4))*x^2*y + 
       (a^6 + b^6 - 2*b^4*c^2 - 2*b^2*c^4 + c^6 - a^4*(b^2 + c^2) - 
         a^2*(b^4 + b^2*c^2 + c^4))*x*y^2 + 
       (a^6 + (b^2 - c^2)^2*(b^2 + c^2) - a^4*(2*b^2 + c^2) - 
         a^2*(2*b^4 + b^2*c^2 + c^4))*x^2*z + 
       2*(a^6 + b^6 - 2*b^4*c^2 - 2*b^2*c^4 + c^6 - 2*a^4*(b^2 + c^2) - 
         2*a^2*(b^4 + c^4))*x*y*z + (a^6 + (b^2 - c^2)^2*(b^2 + c^2) - 
         a^4*(2*b^2 + c^2) - a^2*(2*b^4 + b^2*c^2 + c^4))*y^2*z + 
       (a^6 + b^6 - 2*b^4*c^2 - 2*b^2*c^4 + c^6 - a^4*(b^2 + c^2) - 
         a^2*(b^4 + b^2*c^2 + c^4))*x*z^2 + 
       (a^6 + (b^2 - c^2)^2*(b^2 + c^2) - a^4*(b^2 + 2*c^2) - 
         a^2*(b^4 + b^2*c^2 + 2*c^4))*y*z^2, 
     "K092" -> cubicpK[KimberlingCenterCN[2], KimberlingCenterCN[11057]], 
     "K093" -> symmetrizeEq[(2*a^2*(b^2 + c^2) - b^2*c^2)*x*(y^2 + z^2)], 
     "K094" -> 2*x*y*z*((b^2 + c^2)*(a^2 + c^2)*(b^2 + a^2) - 
         8*a^2*b^2*c^2) + symmetrizeEq[b^2*c^2*(b^2 + c^2 - 5*a^2)*x*
         (y^2 + z^2)], "K095" -> symmetrizeEq[(4*SA^2 - b^2*c^2)*x^2*
        (b^2*z - c^2*y)], "K097" -> symmetrizeEq[(a/(2*SA + b*c))*(c*y - b*z)*
        y*z], "K098" -> symmetrizeEq[a^2*(b^2*c^2 - 16*\[CapitalDelta]^2)*x*
         (c^2*y^2 + b^2*z^2)] + a^2*b^2*c^2*(a^2 + b^2 + c^2)*x*y*z, 
     "K099" -> symmetrizeEq[a^2*SA^2*(SC*y - SB*z)*y*z], 
     "K100" -> symmetrizeEq[b^2*c^2*((b^2 - c^2)*x^3 + a^2*(y - z)*y*z)], 
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
       (a^2 - b^2)*(a^2 + b^2 - c^2)*(a^4 - 2*a^2*b^2 + b^4 - c^4)^2*z^3, 
     "K1191" -> a^12*(c^2*(3*y - 4*z) + b^2*(4*y - 3*z))*(y - z)^2 - 
       a^14*(y - z)^3 + (b^2 - c^2)^3*(a^8 + b^8 + b^4*c^4 + c^8 - 
         4*a^6*(b^2 + c^2) + a^4*(6*b^4 + 8*b^2*c^2 + 6*c^4) - 
         4*a^2*(b^6 + b^4*c^2 + b^2*c^4 + c^6))*x^3 + 
       c^2*(a^2 - c^2)*(b^2 - c^2)^2*(2*a^6 - 14*b^6 - 2*b^4*c^2 + 
         4*b^2*c^4 - 3*c^6 - a^4*(18*b^2 + 7*c^2) + a^2*(30*b^4 + 8*c^4))*x^2*
        y - c^2*(a^2 - c^2)^2*(b^2 - c^2)*(-14*a^6 + a^4*(30*b^2 - 2*c^2) + 
         (2*b^2 - 3*c^2)*(b^2 - c^2)^2 + a^2*(-18*b^4 + 4*c^4))*x*y^2 + 
       (c^6*(b^2 - c^2)^4 - 2*a^10*(3*b^4 + 4*b^2*c^2 + 2*c^4) - 
         a^2*c^4*(b^2 - c^2)^2*(3*b^4 - 2*b^2*c^2 + 3*c^4) + 
         2*a^8*(2*b^6 + 5*b^4*c^2 + 2*b^2*c^4 + 2*c^6) - 
         a^6*(b^8 + 8*b^6*c^2 + 4*c^8) + a^4*c^2*(3*b^8 - 4*b^2*c^6 + 4*c^8))*
        y^3 - (a^2 - b^2)*(b^3 - b*c^2)^2*(2*a^6 - 3*b^6 + 4*b^4*c^2 - 
         2*b^2*c^4 - 14*c^6 - a^4*(7*b^2 + 18*c^2) + a^2*(8*b^4 + 30*c^4))*
        x^2*z - 14*(b^4*c^4*(b^2 - c^2)^3 + a^10*(b^4 - c^4) + 
         a^2*b^4*c^4*(b^4 - c^4) + a^8*(-3*b^6 + b^4*c^2 - b^2*c^4 + 3*c^6) + 
         3*a^6*(b^8 - c^8) + a^4*(-b^10 - b^8*c^2 + b^2*c^8 + c^10))*x*y*z + 
       a^2*(2*c^4*(b^2 - 7*c^2)*(b^3 - b*c^2)^2 + 
         a^8*(15*b^4 + 26*b^2*c^2 + 13*c^4) - 3*a^6*(3*b^6 + 4*b^4*c^2 + 
           17*b^2*c^4 - 2*c^6) + a^4*(2*b^8 + 9*b^4*c^4 + 54*b^2*c^6 - 
           26*c^8) + a^2*(-4*b^8*c^2 + 27*b^6*c^4 - 42*b^4*c^6 - 4*b^2*c^8 + 
           14*c^10))*y^2*z + b^2*(a^2 - b^2)^2*(b^2 - c^2)*
        (14*a^6 + 2*a^4*(b^2 - 15*c^2) + (3*b^2 - 2*c^2)*(b^2 - c^2)^2 + 
         a^2*(-4*b^4 + 18*c^4))*x*z^2 - 
       a^2*(-2*b^4*c^2*(b^2 - c^2)^2*(7*b^2 - c^2) + 
         a^8*(13*b^4 + 26*b^2*c^2 + 15*c^4) + 3*a^6*(2*b^6 - 17*b^4*c^2 - 
           4*b^2*c^4 - 3*c^6) + a^2*b^2*(14*b^8 - 4*b^6*c^2 - 42*b^4*c^4 + 
           27*b^2*c^6 - 4*c^8) + a^4*(-26*b^8 + 54*b^6*c^2 + 9*b^4*c^4 + 
           2*c^8))*y*z^2 + ((-b^6)*(b^2 - c^2)^4 + a^2*b^4*(b^2 - c^2)^2*
          (3*b^4 - 2*b^2*c^2 + 3*c^4) + a^10*(4*b^4 + 8*b^2*c^2 + 6*c^4) - 
         2*a^8*(2*b^6 + 2*b^4*c^2 + 5*b^2*c^4 + 2*c^6) + 
         a^6*(4*b^8 + 8*b^2*c^6 + c^8) + a^4*(-4*b^10 + 4*b^8*c^2 - 
           3*b^2*c^8))*z^3]
 
cubicA1K[{pp_, qq_, rr_}] := -((pp^2 + qq^2 + rr^2)*x*y*z) + 
     pp*qq*z*(x^2 + y^2 - z^2) + pp*rr*y*(x^2 - y^2 + z^2) + 
     qq*rr*x*(-x^2 + y^2 + z^2)
 
cubicA2K[{pp_, qq_, rr_}] := (pp + qq - rr)^2*x*(x - y)*y + 
     2*(pp^2 + qq^2 + rr^2 - 2*(pp*qq + pp*rr + qq*rr))*x*y*z + 
     (-pp + qq + rr)^2*y*(y - z)*z + (pp - qq + rr)^2*x*z*(-x + z)
 
cubicO[{u_, v_, w_}] := x*((c^2*u - 2*SB*w)*y^2 - (b^2*u - 2*SC*v)*z^2) + 
     y*((a^2*v - 2*SC*u)*z^2 - (c^2*v - 2*SA*w)*x^2) + 
     z*((b^2*w - 2*SA*v)*x^2 - (a^2*w - 2*SB*u)*y^2)
 
cubicKp[{p_, q_, r_}] := symmetrizeEq[2*q*r*(q - r)*x^3 + 
      p*(q + r - p)*(r*y - q*z)*y*z]
 
cubicKc[{p_, q_, r_}] := symmetrizeEq[2*q*r*(q - r)*x^3 + 
      p*((p + 3*q - r)*r*y - (p - q + 3*r)*q*z)*y*z]
 
cubicSpk[{p_, q_, r_}, {u_, v_, w_}] := 
    (u + v + w)*symmetrizeEq[p*x*(b^2*z^2 - c^2*y^2)] - 
     2*(x + y + z)*symmetrizeEq[a^2*(r*v - q*w)*y*z]
 
cubicpK[{p_, q_, r_}, {u_, v_, w_}] := symmetrizeEq[u*x*(r^2*y^2 - q^2*z^2)]
 
cubicnK[{p_, q_, r_}, {u_, v_, w_}, {x1_, y1_, z1_}] := 
    Module[{c1, c2, c3, t}, t = p*y1*z1*(w*y1 + v*z1) + 
        q*z1*x1*(u*z1 + w*x1) + r*x1*y1*(v*x1 + u*y1); 
      c1 = -2*p*x1*y1*z1*(u*y*z + v*z*x + w*x*y) + x^2*t; 
      c2 = -2*q*x1*y1*z1*(u*y*z + v*z*x + w*x*y) + y^2*t; 
      c3 = -2*r*x1*y1*z1*(u*y*z + v*z*x + w*x*y) + z^2*t; 
      jacobianDeterminant[{c1, c2, c3}, {x, y, z}]]
 
jacobianDeterminant[(f_List)?VectorQ, x_List] := Det[JacobianMatrix[f, x]] /; 
     Equal @@ Dimensions /@ {f, x}
 
jacobianMatrix[(f_List)?VectorQ, x_List] := Outer[D, f, x] /; 
     Equal @@ Dimensions /@ {f, x}
