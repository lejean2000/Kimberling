(* ::Package:: *)

KimberlingTrianglesBary = <|"ABC" -> {a, 0, 0}, "ABC-X3 reflections" -> 
      {1/2, -(b^2/(a^2 + b^2 - c^2)), -(c^2/(a^2 - b^2 + c^2))}, 
     "1st Altintas-isodynamic" -> {(-(Sqrt[3]*S*(S^2 + SB*SC)) - 
         S^2*(SA + 3*SW) + 2*SA*(S^2 - SA*SW + SW^2))/(S^2 - Sqrt[3]*S*SA - 
         2*SA*SW), SC, SB}, "2nd Altintas-isodynamic" -> 
      {(-(Sqrt[3]*S*(-S^2 - SB*SC)) - S^2*(SA + 3*SW) + 
         2*SA*(S^2 - SA*SW + SW^2))/(S^2 + Sqrt[3]*S*SA - 2*SA*SW), SC, SB}, 
     "Andromeda" -> {(a*(a^2 + 3*b^2 - 6*b*c + 3*c^2))/(3*a^2 + b^2 - 2*b*c + 
         c^2), b, c}, "anti-Aquila" -> {2*a + b + c, b, c}, 
     "anti-Ara" -> {(b^2 + c^2)/(-a^2 + b^2 + c^2), b^2/(a^2 - b^2 + c^2), 
       c^2/(a^2 + b^2 - c^2)}, "anti-Artzt" -> {(-5*a^2 + b^2 + c^2)/2, 
       2*a^2 + 2*b^2 - c^2, 2*a^2 - b^2 + 2*c^2}, 
     "anti-Ascella" -> {a^2, -((b^2*(a^2 - b^2 + 3*c^2))/(a^2 - b^2 + c^2)), 
       -((c^2*(a^2 + 3*b^2 - c^2))/(a^2 + b^2 - c^2))}, 
     "anti-Atik" -> {(-a^6 + 3*a^4*b^2 - 3*a^2*b^4 + b^6 + 3*a^4*c^2 + 
         6*a^2*b^2*c^2 - b^4*c^2 - 3*a^2*c^4 - b^2*c^4 + c^6)/
        (-a^2 + b^2 + c^2)^2, a^2 - b^2 + c^2, a^2 + b^2 - c^2}, 
     "1st anti-Auriga" -> {a^2*b^2 - b^4 + 2*a*b^2*c + a^2*c^2 + 2*a*b*c^2 + 
        2*b^2*c^2 - c^4 + 4*a*Sqrt[R*(r + 4*R)]*S, 
       b*(-(a^2*b) + b^3 - 2*a*b*c - b*c^2 + 4*Sqrt[R*(r + 4*R)]*S), 
       c*(-(a^2*c) - 2*a*b*c - b^2*c + c^3 + 4*Sqrt[R*(r + 4*R)]*S)}, 
     "2nd anti-Auriga" -> {-(a^2*b^2) + b^4 - 2*a*b^2*c - a^2*c^2 - 
        2*a*b*c^2 - 2*b^2*c^2 + c^4 + 4*a*Sqrt[R*(r + 4*R)]*S, 
       b*(a^2*b - b^3 + 2*a*b*c + b*c^2 + 4*Sqrt[R*(r + 4*R)]*S), 
       c*(a^2*c + 2*a*b*c + b^2*c - c^3 + 4*Sqrt[R*(r + 4*R)]*S)}, 
     "1st anti-Brocard" -> {a^4 - b^2*c^2, -(a^2*b^2) + c^4, b^4 - a^2*c^2}, 
     "4th anti-Brocard" -> {(a^2*(a^2 + b^2 - 3*b*c + c^2)*
         (a^2 + b^2 + 3*b*c + c^2))/(5*a^2 - b^2 - c^2), 
       -(b^2*(a^2 + b^2 - 2*c^2)), -(c^2*(a^2 - 2*b^2 + c^2))}, 
     "5th anti-Brocard" -> {(a^2 + b^2)*(a^2 + c^2), b^4, c^4}, 
     "6th anti-Brocard" -> {a^2*(a^4 - b^2*c^2), -(a^2*b^4) + b^6 - 
        a^2*b^2*c^2 - b^4*c^2 + a^2*c^4 + b^2*c^4, a^2*b^4 - a^2*b^2*c^2 + 
        b^4*c^2 - a^2*c^4 - b^2*c^4 + c^6}, 
     "2nd anti-circumperp-tangential" -> {(b + c)^2/(-a + b + c), 
       b^2/(a - b + c), c^2/(a + b - c)}, "1st anti-circumperp" -> 
      {a^2/(b^2 - c^2), -1, 1}, "anti-Conway" -> 
      {(a^4*(-a^2 + b^2 + c^2))/(a^2*b^2 - b^4 + a^2*c^2 + 2*b^2*c^2 - c^4), 
       b^2, c^2}, "2nd anti-Conway" -> 
      {(a^2*b^2 - b^4 + a^2*c^2 + 2*b^2*c^2 - c^4)/(-a^2 + b^2 + c^2), b^2, 
       c^2}, "anti-Ehrmann-mid" -> {-3*a^2*(-a^2 + b^2 + c^2), 
       -2*a^4 + a^2*b^2 + b^4 + 4*a^2*c^2 + b^2*c^2 - 2*c^4, 
       -2*a^4 + 4*a^2*b^2 - 2*b^4 + a^2*c^2 + b^2*c^2 + c^4}, 
     "anti-Euler" -> {(3*a^4 - 4*a^2*b^2 + b^4 - 4*a^2*c^2 - 2*b^2*c^2 + c^4)/
        (-a^2 + b^2 + c^2), a^2 + b^2 - c^2, a^2 - b^2 + c^2}, 
     "3rd anti-Euler" -> {a^2*(-(a^2*b^2) + b^4 - a^2*c^2 - b^2*c^2 + c^4), 
       b^2*(a^4 - a^2*b^2 + a^2*c^2 + b^2*c^2 - c^4), 
       c^2*(a^4 + a^2*b^2 - b^4 - a^2*c^2 + b^2*c^2)}, 
     "4th anti-Euler" -> {a^2*(-(a^6*b^2) + 3*a^4*b^4 - 3*a^2*b^6 + b^8 - 
         a^6*c^2 + a^4*b^2*c^2 + 3*a^2*b^4*c^2 - 3*b^6*c^2 + 3*a^4*c^4 + 
         3*a^2*b^2*c^4 + 4*b^4*c^4 - 3*a^2*c^6 - 3*b^2*c^6 + c^8), 
       b^2*(a^8 - 3*a^6*b^2 + 3*a^4*b^4 - a^2*b^6 - a^6*c^2 + a^4*b^2*c^2 - 
         a^2*b^4*c^2 + b^6*c^2 - 2*a^4*c^4 - a^2*b^2*c^4 - 3*b^4*c^4 + 
         3*a^2*c^6 + 3*b^2*c^6 - c^8), c^2*(a^8 - a^6*b^2 - 2*a^4*b^4 + 
         3*a^2*b^6 - b^8 - 3*a^6*c^2 + a^4*b^2*c^2 - a^2*b^4*c^2 + 
         3*b^6*c^2 + 3*a^4*c^4 - a^2*b^2*c^4 - 3*b^4*c^4 - a^2*c^6 + 
         b^2*c^6)}, "anti-excenters-reflections" -> 
      {(-4*a^2)/(3*a^4 - 2*a^2*b^2 - b^4 - 2*a^2*c^2 + 2*b^2*c^2 - c^4), 
       (a^2 - b^2 + c^2)^(-1), (a^2 + b^2 - c^2)^(-1)}, 
     "2nd anti-extouch" -> {(4*a^2*S^2)/(-a^2 + b^2 + c^2), 
       b^2*(a^2 - b^2 + c^2), c^2*(a^2 + b^2 - c^2)}, 
     "anti-inner-Garcia" -> {a^2*(-a^2 + b^2 - b*c + c^2), 
       b*(a^2*b - b^3 + b^2*c - a*c^2 + b*c^2 - c^3), 
       c*(-(a*b^2) - b^3 + a^2*c + b^2*c + b*c^2 - c^3)}, 
     "anti-inner-Grebe" -> {-a^2 + S, -b^2, -c^2}, "anti-outer-Grebe" -> 
      {a^2 + S, b^2, c^2}, "anti-Honsberger" -> {a^4/(b^2 + c^2), -b^2, 
       -c^2}, "anti-Hutson intouch" -> 
      {a^2*(-a^4 + 2*a^2*b^2 - b^4 + 2*a^2*c^2 - 6*b^2*c^2 - c^4), 
       b^2*(a^4 - 2*a^2*b^2 + b^4 + 2*a^2*c^2 + 2*b^2*c^2 - 3*c^4), 
       c^2*(a^4 + 2*a^2*b^2 - 3*b^4 - 2*a^2*c^2 + 2*b^2*c^2 + c^4)}, 
     "anti-incircle-circles" -> 
      {-(a^2*(a^4 - 2*a^2*b^2 + b^4 - 2*a^2*c^2 - 6*b^2*c^2 + c^4)), 
       b^2*(a^4 - 2*a^2*b^2 + b^4 - 4*a^2*c^2 - 4*b^2*c^2 + 3*c^4), 
       c^2*(a^4 - 4*a^2*b^2 + 3*b^4 - 2*a^2*c^2 - 4*b^2*c^2 + c^4)}, 
     "anti-inverse-in-incircle" -> {-a^2 - b^2 - c^2, a^2 + b^2 - c^2, 
       a^2 - b^2 + c^2}, "1st anti-Kenmotu centers" -> 
      {-b^2 - c^2 - 2*S, b^2, c^2}, "2nd anti-Kenmotu centers" -> 
      {-b^2 - c^2 + 2*S, b^2, c^2}, "1st anti-Kenmotu-free-vertices" -> 
      {-2*a^4 + 3*a^2*b^2 - b^4 + 3*a^2*c^2 + 2*b^2*c^2 - c^4 + 2*b^2*S + 
        2*c^2*S, b^2*(a^2 - b^2 + c^2 - 2*S), c^2*(a^2 + b^2 - c^2 - 2*S)}, 
     "2nd anti-Kenmotu-free-vertices" -> {2*a^4 - 3*a^2*b^2 + b^4 - 
        3*a^2*c^2 - 2*b^2*c^2 + c^4 + 2*b^2*S + 2*c^2*S, 
       -(b^2*(a^2 - b^2 + c^2 + 2*S)), -(c^2*(a^2 + b^2 - c^2 + 2*S))}, 
     "anti-Lucas(+1) homothetic" -> 
      {-1/2*(S*(-a^4 - (b^2 - c^2)^2 + 2*a^2*(b^2 + c^2) + 2*SA^2 + 
          S*(2*SA + 2*SW))), (b^2*(-a^4 - (b^2 - c^2)^2 + 2*a^2*(b^2 + c^2) + 
          2*SB^2 + S*(2*SB + 2*SW)))/2, 
       (c^2*(-a^4 - (b^2 - c^2)^2 + 2*a^2*(b^2 + c^2) + 2*SC^2 + 
          S*(2*SC + 2*SW)))/2}, "anti-Lucas(-1) homothetic" -> 
      {(S*(-a^4 - (b^2 - c^2)^2 + 2*a^2*(b^2 + c^2) + 2*SA^2 + 
          S*(-2*SA - 2*SW)))/2, (b^2*(-a^4 - (b^2 - c^2)^2 + 
          2*a^2*(b^2 + c^2) + 2*SB^2 + S*(-2*SB - 2*SW)))/2, 
       (c^2*(-a^4 - (b^2 - c^2)^2 + 2*a^2*(b^2 + c^2) + 2*SC^2 + 
          S*(-2*SC - 2*SW)))/2}, "anti-Mandart-incircle" -> 
      {a*(-a^2 + a*b + a*c - 2*b*c), b^2*(a - b + c), (a + b - c)*c^2}, 
     "anti-McCay" -> {-5*a^4 + 2*a^2*b^2 - 2*b^4 + 2*a^2*c^2 + 5*b^2*c^2 - 
        2*c^4, (2*a^2 - 3*a*b + 2*b^2 - c^2)*(2*a^2 + 3*a*b + 2*b^2 - c^2), 
       (2*a^2 - b^2 - 3*a*c + 2*c^2)*(2*a^2 - b^2 + 3*a*c + 2*c^2)}, 
     "6th anti-mixtilinear" -> {2*a^2, a^2 - b^2 + c^2, a^2 + b^2 - c^2}, 
     "anti-orthocentroidal" -> {a^2*(a^2 - b^2 - b*c - c^2)*
        (a^2 - b^2 + b*c - c^2), b^2*(a^4 - 2*a^2*b^2 + b^4 + a^2*c^2 + 
         b^2*c^2 - 2*c^4), c^2*(a^4 + a^2*b^2 - 2*b^4 - 2*a^2*c^2 + b^2*c^2 + 
         c^4)}, "1st anti-orthosymmedial" -> 
      {(a^2*(a^8 - a^6*b^2 - a^4*b^4 + a^2*b^6 - a^6*c^2 - a^4*b^2*c^2 + 
          a^2*b^4*c^2 + b^6*c^2 - a^4*c^4 + a^2*b^2*c^4 - 2*b^4*c^4 + 
          a^2*c^6 + b^2*c^6))/(b^2 + c^2), b^2*(a^6 - a^4*b^2 - a^2*b^4 + 
         b^6 + a^2*c^4 + b^2*c^4 - 2*c^6), c^2*(a^6 + a^2*b^4 - 2*b^6 - 
         a^4*c^2 + b^4*c^2 - a^2*c^4 + c^6)}, "1st anti-Parry" -> 
      {(a^4 - 2*a^2*b^2 + 3*b^4 - 2*a^2*c^2 - 6*b^2*c^2 + 3*c^4)/(b^2 - c^2), 
       (b^2*(-a^2 - b^2 + 5*c^2))/(a^2 - c^2), (c^2*(a^2 - 5*b^2 + c^2))/
        (a^2 - b^2)}, "2nd anti-Parry" -> 
      {(a^8 - a^6*b^2 - a^4*b^4 + a^2*b^6 - a^6*c^2 + 3*a^4*b^2*c^2 - 
         a^2*b^4*c^2 - 3*b^6*c^2 - a^4*c^4 - a^2*b^2*c^4 + 6*b^4*c^4 + 
         a^2*c^6 - 3*b^2*c^6)/(2*a^2 - b^2 - c^2), 
       -(b^2*(a^4 - 2*a^2*b^2 + b^4 + a^2*c^2 + b^2*c^2 - 2*c^4)), 
       c^2*(-a^4 - a^2*b^2 + 2*b^4 + 2*a^2*c^2 - b^2*c^2 - c^4)}, 
     "1st anti-Sharygin" -> {a^2/(a^2*b^2 - b^4 + a^2*c^2 + 2*b^2*c^2 - c^4), 
       (c^2*(a^2 + b^2 - c^2))/((a^2 - b^2 + c^2)*(a^4 - a^2*b^2 - 
          2*a^2*c^2 - b^2*c^2 + c^4)), (b^2*(a^2 - b^2 + c^2))/
        ((a^2 + b^2 - c^2)*(a^4 - 2*a^2*b^2 + b^4 - a^2*c^2 - b^2*c^2))}, 
     "anti-tangential-midarc" -> {-(a^2/(-a + b + c)), 
       (b*(a + c))/(a - b + c), ((a + b)*c)/(a + b - c)}, 
     "3rd anti-tri-squares-central" -> {b^2 + c^2 + 3*S, -b^2 - S, -c^2 - S}, 
     "4th anti-tri-squares-central" -> {b^2 + c^2 - 3*S, -b^2 + S, -c^2 + S}, 
     "3rd anti-tri-squares" -> {(-S^2 + 3*SB*SC + S*(SB + SC))/(-S + SA), 
       -S + 3*SC, -S + 3*SB}, "4th anti-tri-squares" -> 
      {(-S^2 + 3*SB*SC - S*(SB + SC))/(S + SA), S + 3*SC, S + 3*SB}, 
     "anti-Ursa minor" -> {-b^2 - c^2, a^2 - c^2, a^2 - b^2}, 
     "anti-Wasat" -> {a^2*(a^2*b^2 - b^4 + a^2*c^2 + 2*b^2*c^2 - c^4), 
       b^2*(-a^2 + c^2)*(a^2 - b^2 + c^2), (a^2 - b^2)*c^2*
        (-a^2 - b^2 + c^2)}, "anti-X3-ABC reflections" -> 
      {2*a^4 - 3*a^2*b^2 + b^4 - 3*a^2*c^2 - 2*b^2*c^2 + c^4, 
       b^2*(-a^2 + b^2 - c^2), c^2*(-a^2 - b^2 + c^2)}, 
     "anti-inner-Yff" -> {(-a^4 + 2*a^2*b^2 - b^4 + 2*a*b^2*c + 2*a^2*c^2 + 
         2*a*b*c^2 + 2*b^2*c^2 - c^4)/(2*a*b*c), -b, -c}, 
     "anti-outer-Yff" -> {(-a^4 + 2*a^2*b^2 - b^4 - 2*a*b^2*c + 2*a^2*c^2 - 
         2*a*b*c^2 + 2*b^2*c^2 - c^4)/(2*a*b*c), b, c}, 
     "AAOA" -> {(a^8 - 2*a^6*b^2 + 2*a^2*b^6 - b^8 - 2*a^6*c^2 + 
         3*a^4*b^2*c^2 - 2*a^2*b^4*c^2 - 2*a^2*b^2*c^4 + 2*b^4*c^4 + 
         2*a^2*c^6 - c^8)/(b*c), b*c*(a^4 - a^2*b^2 + b^4 - c^4), 
       b*c*(a^4 - b^4 - a^2*c^2 + c^4)}, "anticomplementary" -> {-1, 1, 1}, 
     "Antlia" -> {(a*(a^2 + 3*b^2 - 6*b*c + 3*c^2))/(3*a^2 + b^2 - 2*b*c + 
         c^2), -b, -c}, "AOA" -> {-(a^6*b^2) + a^4*b^4 + a^2*b^6 - b^8 - 
        a^6*c^2 - a^2*b^4*c^2 + a^4*c^4 - a^2*b^2*c^4 + 2*b^4*c^4 + a^2*c^6 - 
        c^8, 2*a^8 - 3*a^6*b^2 - a^4*b^4 + 3*a^2*b^6 - b^8 - 4*a^6*c^2 + 
        10*a^4*b^2*c^2 - 4*a^2*b^4*c^2 - 7*a^2*b^2*c^4 + 3*b^4*c^4 + 
        4*a^2*c^6 - 2*c^8, 2*a^8 - 4*a^6*b^2 + 4*a^2*b^6 - 2*b^8 - 
        3*a^6*c^2 + 10*a^4*b^2*c^2 - 7*a^2*b^4*c^2 - a^4*c^4 - 
        4*a^2*b^2*c^4 + 3*b^4*c^4 + 3*a^2*c^6 - c^8}, 
     "Apollonius" -> {-((a^2*(a*b + b^2 + a*c + c^2)^2)/(a + b + c)), 
       b^2*(a + b - c)*(a + c)^2, (a + b)^2*c^2*(a - b + c)}, 
     "Apus" -> {a^2/(a + b + c), -(b^2/(a + b - c)), -(c^2/(a - b + c))}, 
     "Aquila" -> {a + 2*b + 2*c, -b, -c}, 
     "Ara" -> {a^2*(a^2 + b^2 + c^2), -(b^2*(a^2 + b^2 - c^2)), 
       -(c^2*(a^2 - b^2 + c^2))}, "Aries" -> 
      {(a^4 + b^4 - 2*b^2*c^2 + c^4)/(2*(b^2 - c^2)), -b^2, c^2}, 
     "Artzt" -> {(3*a^4 + b^4 - 2*b^2*c^2 + c^4)/2, -2*a^2*b^2 - a^2*c^2 - 
        b^2*c^2 + c^4, -(a^2*b^2) + b^4 - 2*a^2*c^2 - b^2*c^2}, 
     "Ascella" -> {2*a^2, a^2 - 2*a*b - b^2 - 2*a*c + c^2, 
       a^2 - 2*a*b + b^2 - 2*a*c - c^2}, 
     "Atik" -> {-(a^2*b) + 2*a*b^2 - b^3 - a^2*c - 3*b^2*c + 2*a*c^2 - 
        3*b*c^2 - c^3, b*(a^2 - 2*a*b + b^2 + 2*a*c + 2*b*c - 3*c^2), 
       c*(a^2 + 2*a*b - 3*b^2 - 2*a*c + 2*b*c + c^2)}, 
     "1st Auriga" -> {a^4 - a^2*b^2 - 2*a^2*b*c - a^2*c^2 - 
        4*b*Sqrt[R*(r + 4*R)]*S - 4*c*Sqrt[R*(r + 4*R)]*S, 
       b*(-(a^2*b) + b^3 - 2*a*b*c - b*c^2 + 4*Sqrt[R*(r + 4*R)]*S), 
       c*(-(a^2*c) - 2*a*b*c - b^2*c + c^3 + 4*Sqrt[R*(r + 4*R)]*S)}, 
     "2nd Auriga" -> {a^4 - a^2*b^2 - 2*a^2*b*c - a^2*c^2 + 
        4*b*Sqrt[R*(r + 4*R)]*S + 4*c*Sqrt[R*(r + 4*R)]*S, 
       b*(-(a^2*b) + b^3 - 2*a*b*c - b*c^2 - 4*Sqrt[R*(r + 4*R)]*S), 
       c*(-(a^2*c) - 2*a*b*c - b^2*c + c^3 - 4*Sqrt[R*(r + 4*R)]*S)}, 
     "Ayme" -> {(b + c)*(a^2 + b^2 + 2*b*c + c^2), b*(-a^2 - b^2 + c^2), 
       c*(-a^2 + b^2 - c^2)}, "Bankoff" -> 
      {2*a^2*(-2*a^2 + Sqrt[3]*a^2 + 2*b^2 - Sqrt[3]*b^2 + 2*c^2 - 
         Sqrt[3]*c^2 - 2*S), -(Sqrt[3]*a^4) + 4*a^2*b^2 - 4*b^4 + 
        Sqrt[3]*b^4 + 2*Sqrt[3]*a^2*c^2 + 4*b^2*c^2 - Sqrt[3]*c^4 + 2*a^2*S + 
        2*b^2*S - 2*c^2*S, -(Sqrt[3]*a^4) + 2*Sqrt[3]*a^2*b^2 - Sqrt[3]*b^4 + 
        4*a^2*c^2 + 4*b^2*c^2 - 4*c^4 + Sqrt[3]*c^4 + 2*a^2*S - 2*b^2*S + 
        2*c^2*S}, "BCE" -> {-a, b*(1 - 2*Sin[C/2]), c*(1 - 2*Sin[B/2])}, 
     "BCE-incenters" -> {-a, b*(1 + 2*Sin[C/2]), c*(1 + 2*Sin[B/2])}, 
     "BCI" -> {a, b*(1 + 2*Cos[C/2]), c*(1 + 2*Cos[B/2])}, 
     "BCI-excenters" -> {a, b*(1 - 2*Cos[C/2]), c*(1 - 2*Cos[B/2])}, 
     "Bevan antipodal" -> {-(a*(a + b - c)*(a - b + c)), 
       b*(a + b - c)*(-a + b + c), c*(a - b + c)*(-a + b + c)}, 
     "1st Brocard" -> {a^2, c^2, b^2}, "2nd Brocard" -> 
      {-a^2 + b^2 + c^2, b^2, c^2}, "3rd Brocard" -> {1, b^2/c^2, c^2/b^2}, 
     "4th Brocard" -> {a^2/(-a^2 + b^2 + c^2), 1, 1}, 
     "5th Brocard" -> {a^2*b^2 + b^4 + a^2*c^2 + b^2*c^2 + c^4, -b^4, -c^4}, 
     "6th Brocard" -> {a^2*b^2 + a^2*c^2 - b^2*c^2, -b^4 + b^2*c^2 + c^4, 
       b^4 + b^2*c^2 - c^4}, "7th Brocard" -> {a^4 + b^4 - 2*b^2*c^2 + c^4, 
       b^2*(a^2 - b^2 + c^2), c^2*(a^2 + b^2 - c^2)}, 
     "8th Brocard" -> {-2*a^4, b^2*(a^2 - b^2 + c^2), c^2*(a^2 + b^2 - c^2)}, 
     "9th Brocard" -> {-1/2*1/a^2, (a^2 - b^2 + c^2)^(-1), 
       (a^2 + b^2 - c^2)^(-1)}, "10th Brocard" -> 
      {-a^8 + (b^2 - c^2)^4 - 2*a^2*(b^2 - c^2)*(b^4 - c^4) + 
        2*a^4*(b^4 + c^4), b^2*(a^6 - a^4*(b^2 - 3*c^2) - (b^2 - c^2)^3 + 
         a^2*(b^4 - c^4)), c^2*(a^6 + (b^2 - c^2)^3 + a^4*(3*b^2 - c^2) - 
         a^2*(b^4 - c^4))}, "1st Brocard-reflected" -> 
      {-a^2, a^2 + b^2, a^2 + c^2}, "circummedial" -> 
      {a^2/(b^2 + c^2), -1, -1}, "circumnormal" -> 
      {a/(Cos[B/3]*Cos[C/3] + Sin[B/3]*Sin[C/3]), 
       b/(-(Cos[B/3]*Cos[(2*C)/3]) + Sin[B/3]*Sin[(2*C)/3]), 
       c/(-(Cos[(2*B)/3]*Cos[C/3]) + Sin[(2*B)/3]*Sin[C/3])}, 
     "circumorthic" -> {a^2/(a^2*b^2 - b^4 + a^2*c^2 + 2*b^2*c^2 - c^4), 
       -(a^2 - b^2 + c^2)^(-1), -(a^2 + b^2 - c^2)^(-1)}, 
     "2nd circumperp tangential" -> {a*(a^2 + a*b + a*c + 2*b*c), 
       -(b^2*(a + b - c)), -(c^2*(a - b + c))}, "1st circumperp" -> 
      {a^2/(b - c), (b*(-b + c))/(b - c), c}, "2nd circumperp" -> 
      {-(a^2/(b + c)), b, c}, "circumsymmedial" -> {-1/2*a^2, b^2, c^2}, 
     "circumtangential" -> {-(a/(Cos[C/3]*Sin[B/3] - Cos[B/3]*Sin[C/3])), 
       b/(Cos[(2*C)/3]*Sin[B/3] + Cos[B/3]*Sin[(2*C)/3]), 
       -(c/(Cos[C/3]*Sin[(2*B)/3] + Cos[(2*B)/3]*Sin[C/3]))}, 
     "inner-Conway" -> {a/(b - c), -1, 1}, "Conway" -> 
      {(a*(-a - b - c))/(b + c), a + b - c, a - b + c}, 
     "2nd Conway" -> {-a - b - c, a + b - c, a - b + c}, 
     "3rd Conway" -> {-(a^2*(a + b + c)^2), a^3*b + 2*a^2*b^2 + a*b^3 + 
        2*a^3*c + 2*b^3*c + a*b*c^2 - 2*a*c^3 - 2*b*c^3, 
       2*a^3*b - 2*a*b^3 + a^3*c + a*b^2*c - 2*b^3*c + 2*a^2*c^2 + a*c^3 + 
        2*b*c^3}, "4th Conway" -> {(a^3 + a^2*(-b - c) - 2*b*c*(b + c) + 
         a*(-2*b^2 - 2*b*c - 2*c^2))/(a*(a + b + c)), b, c}, 
     "5th Conway" -> {(a^2*b + a*b^2 + a^2*c + 2*b^2*c + a*c^2 + 2*b*c^2)/
        ((b + c)*(a + b + c)), -b, -c}, 
     "Dao" -> {0, b*(a + b)*c*(a - b + c)^2, b*(a + b - c)^2*c*(a + c)}, 
     "Ehrmann-cross" -> {(b^2 - c^2)*(2*a^4 - a^2*b^2 - b^4 - a^2*c^2 + 
         2*b^2*c^2 - c^4), (-a^2 + b^2 - c^2)*(a^2 - a*b + b^2 - c^2)*
        (a^2 + a*b + b^2 - c^2), (a^2 + b^2 - c^2)*(a^2 - b^2 - a*c + c^2)*
        (a^2 - b^2 + a*c + c^2)}, "Ehrmann-mid" -> 
      {-2*a^4 + a^2*b^2 + b^4 + a^2*c^2 - 2*b^2*c^2 + c^4, 
       2*a^4 - a^2*b^2 - b^4 - 4*a^2*c^2 - b^2*c^2 + 2*c^4, 
       2*a^4 - 4*a^2*b^2 + 2*b^4 - a^2*c^2 - b^2*c^2 - c^4}, 
     "Ehrmann-side" -> {-(a^2*(a^4 + a^2*b^2 - 2*b^4 + a^2*c^2 + 4*b^2*c^2 - 
          2*c^4)), (a^2 - a*b + b^2 - c^2)*(a^2 + a*b + b^2 - c^2)*
        (a^2 - b^2 + c^2), (a^2 + b^2 - c^2)*(a^2 - b^2 - a*c + c^2)*
        (a^2 - b^2 + a*c + c^2)}, "Ehrmann-vertex" -> 
      {(-a^6 + a^4*b^2 + a^2*b^4 - b^6 + a^4*c^2 + b^4*c^2 + a^2*c^4 + 
         b^2*c^4 - c^6)/((a^2 - b^2 - b*c - c^2)*(a^2 - b^2 + b*c - c^2)), 
       a^2 + b^2 - c^2, a^2 - b^2 + c^2}, "1st Ehrmann" -> 
      {a^2*(-a^4 + b^4 - 4*b^2*c^2 + c^4), b^2*(a^4 - b^4 + 5*a^2*c^2 + 
         3*b^2*c^2 - 2*c^4), c^2*(a^4 + 5*a^2*b^2 - 2*b^4 + 3*b^2*c^2 - 
         c^4)}, "2nd Ehrmann" -> {(a^2*(a^2 - 2*b^2 - 2*c^2))/
        (2*a^2 - b^2 - c^2), b^2, c^2}, "Euler" -> 
      {2*(a^2 + b^2 - c^2)*(a^2 - b^2 + c^2)*(-(b^2 - c^2)^2 + 
         a^2*(b^2 + c^2)), (a^2 + b^2 - c^2)^2*(a^2 - b^2 + c^2)*
        (-a^2 + b^2 + c^2), (a^2 + b^2 - c^2)*(a^2 - b^2 + c^2)^2*
        (-a^2 + b^2 + c^2)}, "2nd Euler" -> 
      {-2*a^2*(a^2*b^2 - b^4 + a^2*c^2 + 2*b^2*c^2 - c^4), 
       (a^2 - b^2 + c^2)*(a^4 + b^4 - 2*a^2*c^2 - 2*b^2*c^2 + c^4), 
       (a^2 + b^2 - c^2)*(a^4 - 2*a^2*b^2 + b^4 - 2*b^2*c^2 + c^4)}, 
     "3rd Euler" -> {-(b - c)^2, a^2 + b*c - c^2, a^2 - b^2 + b*c}, 
     "4th Euler" -> {-(b + c)^2, a^2 - b*c - c^2, a^2 - b^2 - b*c}, 
     "5th Euler" -> {(-2*(b^2 + c^2))/(-a^2 + b^2 + c^2), -1, -1}, 
     "excenters-midpoints" -> {2*a - b - c, -b, -c}, 
     "excenters-reflections" -> {a*(-a + 3*b + 3*c), b*(-3*a + b + c), 
       c*(-3*a + b + c)}, "excentral" -> {-a, b, c}, 
     "1st excosine" -> {4*a^2*S^2, b^2*(-a^4 + 2*a^2*b^2 - b^4 - 2*a^2*c^2 - 
         2*b^2*c^2 + 3*c^4), c^2*(-a^4 - 2*a^2*b^2 + 3*b^4 + 2*a^2*c^2 - 
         2*b^2*c^2 - c^4)}, "2nd excosine" -> 
      {(2*(a^2 + b^2 - c^2)*(a^2 - b^2 + c^2)*S^2)/(-a^2 + b^2 + c^2), 
       b^2*(-a^4 + 2*a^2*b^2 - b^4 - 2*a^2*c^2 - 2*b^2*c^2 + 3*c^4), 
       c^2*(-a^4 - 2*a^2*b^2 + 3*b^4 + 2*a^2*c^2 - 2*b^2*c^2 - c^4)}, 
     "extangents" -> {a^2*(a + b + c), b*(-a - c)*(a + b - c), 
       (-a - b)*c*(a - b + c)}, "extouch" -> {0, (a + b - c)^(-1), 
       (a - b + c)^(-1)}, "2nd extouch" -> {a*(-2*b - 2*c), a^2 + b^2 - c^2, 
       a^2 - b^2 + c^2}, "3rd extouch" -> 
      {(2*a*(a + b - c)*(a - b + c)*(b + c))/((-a + b + c)*(a + b + c)), 
       -a^2 - b^2 + c^2, -a^2 + b^2 - c^2}, "4th extouch" -> 
      {(2*a*(b + c)*(a + b + c))/(-a + b + c), -a^2 + b^2 - c^2, 
       -a^2 - b^2 + c^2}, "5th extouch" -> {(2*a*(b + c))/(-a + b + c), 
       (-a^2 - b^2 - 2*a*c - c^2)/(a - b + c), (-a^2 - 2*a*b - b^2 - c^2)/
        (a + b - c)}, "inner-Fermat" -> {2*Sqrt[3]*a^2, 
       -(Sqrt[3]*a^2) - Sqrt[3]*b^2 + Sqrt[3]*c^2 + 2*S, 
       -(Sqrt[3]*a^2) + Sqrt[3]*b^2 - Sqrt[3]*c^2 + 2*S}, 
     "outer-Fermat" -> {-2*Sqrt[3]*a^2, Sqrt[3]*a^2 + Sqrt[3]*b^2 - 
        Sqrt[3]*c^2 + 2*S, Sqrt[3]*a^2 - Sqrt[3]*b^2 + Sqrt[3]*c^2 + 2*S}, 
     "1st Fermat-Dao" -> {(2*a^2*(6*a^8*b^4 - 12*a^6*b^6 + 3*a^4*b^8 + 
          6*a^2*b^10 - 3*b^12 + 12*a^8*b^2*c^2 + 3*a^6*b^4*c^2 - 
          111*a^4*b^6*c^2 + 84*a^2*b^8*c^2 + 6*a^8*c^4 + 3*a^6*b^2*c^4 - 
          225*a^4*b^4*c^4 - 156*a^2*b^6*c^4 + 60*b^8*c^4 - 12*a^6*c^6 - 
          111*a^4*b^2*c^6 - 156*a^2*b^4*c^6 - 114*b^6*c^6 + 3*a^4*c^8 + 
          84*a^2*b^2*c^8 + 60*b^4*c^8 + 6*a^2*c^10 - 3*c^12 + 
          4*Sqrt[3]*a^6*b^4*S - 16*Sqrt[3]*a^4*b^6*S + 14*Sqrt[3]*a^2*b^8*S - 
          2*Sqrt[3]*b^10*S + 4*Sqrt[3]*a^6*b^2*c^2*S - 74*Sqrt[3]*a^4*b^4*c^2*
           S - 16*Sqrt[3]*a^2*b^6*c^2*S + 30*Sqrt[3]*b^8*c^2*S + 
          4*Sqrt[3]*a^6*c^4*S - 74*Sqrt[3]*a^4*b^2*c^4*S - 
          194*Sqrt[3]*a^2*b^4*c^4*S - 34*Sqrt[3]*b^6*c^4*S - 
          16*Sqrt[3]*a^4*c^6*S - 16*Sqrt[3]*a^2*b^2*c^6*S - 
          34*Sqrt[3]*b^4*c^6*S + 14*Sqrt[3]*a^2*c^8*S + 30*Sqrt[3]*b^2*c^8*
           S - 2*Sqrt[3]*c^10*S))/((Sqrt[3]*a^2 + 2*S)*
         (Sqrt[3]*a^2 + Sqrt[3]*b^2 - Sqrt[3]*c^2 + 2*S)*
         (Sqrt[3]*a^2 - Sqrt[3]*b^2 + Sqrt[3]*c^2 + 2*S)), 
       (b^2*(-3*a^6*b^2 + 6*a^4*b^4 - 3*a^2*b^6 + 15*a^4*b^2*c^2 + 
          15*a^2*b^4*c^2 + 3*a^4*c^4 + 3*b^4*c^4 - 6*a^2*c^6 - 6*b^2*c^6 + 
          3*c^8 + 2*Sqrt[3]*a^4*b^2*S + 2*Sqrt[3]*a^2*b^4*S + 
          4*Sqrt[3]*a^4*c^2*S + 16*Sqrt[3]*a^2*b^2*c^2*S + 
          4*Sqrt[3]*b^4*c^2*S - 6*Sqrt[3]*a^2*c^4*S - 6*Sqrt[3]*b^2*c^4*S + 
          2*Sqrt[3]*c^6*S))/(Sqrt[3]*a^2 + Sqrt[3]*b^2 - Sqrt[3]*c^2 + 2*S), 
       (c^2*(3*a^4*b^4 - 6*a^2*b^6 + 3*b^8 - 3*a^6*c^2 + 15*a^4*b^2*c^2 - 
          6*b^6*c^2 + 6*a^4*c^4 + 15*a^2*b^2*c^4 + 3*b^4*c^4 - 3*a^2*c^6 + 
          4*Sqrt[3]*a^4*b^2*S - 6*Sqrt[3]*a^2*b^4*S + 2*Sqrt[3]*b^6*S + 
          2*Sqrt[3]*a^4*c^2*S + 16*Sqrt[3]*a^2*b^2*c^2*S - 
          6*Sqrt[3]*b^4*c^2*S + 2*Sqrt[3]*a^2*c^4*S + 4*Sqrt[3]*b^2*c^4*S))/
        (Sqrt[3]*a^2 - Sqrt[3]*b^2 + Sqrt[3]*c^2 + 2*S)}, 
     "2nd Fermat-Dao" -> {(2*a^2*(-6*a^8*b^4 + 12*a^6*b^6 - 3*a^4*b^8 - 
          6*a^2*b^10 + 3*b^12 - 12*a^8*b^2*c^2 - 3*a^6*b^4*c^2 + 
          111*a^4*b^6*c^2 - 84*a^2*b^8*c^2 - 6*a^8*c^4 - 3*a^6*b^2*c^4 + 
          225*a^4*b^4*c^4 + 156*a^2*b^6*c^4 - 60*b^8*c^4 + 12*a^6*c^6 + 
          111*a^4*b^2*c^6 + 156*a^2*b^4*c^6 + 114*b^6*c^6 - 3*a^4*c^8 - 
          84*a^2*b^2*c^8 - 60*b^4*c^8 - 6*a^2*c^10 + 3*c^12 + 
          4*Sqrt[3]*a^6*b^4*S - 16*Sqrt[3]*a^4*b^6*S + 14*Sqrt[3]*a^2*b^8*S - 
          2*Sqrt[3]*b^10*S + 4*Sqrt[3]*a^6*b^2*c^2*S - 74*Sqrt[3]*a^4*b^4*c^2*
           S - 16*Sqrt[3]*a^2*b^6*c^2*S + 30*Sqrt[3]*b^8*c^2*S + 
          4*Sqrt[3]*a^6*c^4*S - 74*Sqrt[3]*a^4*b^2*c^4*S - 
          194*Sqrt[3]*a^2*b^4*c^4*S - 34*Sqrt[3]*b^6*c^4*S - 
          16*Sqrt[3]*a^4*c^6*S - 16*Sqrt[3]*a^2*b^2*c^6*S - 
          34*Sqrt[3]*b^4*c^6*S + 14*Sqrt[3]*a^2*c^8*S + 30*Sqrt[3]*b^2*c^8*
           S - 2*Sqrt[3]*c^10*S))/((-(Sqrt[3]*a^2) + 2*S)*
         (-(Sqrt[3]*a^2) + Sqrt[3]*b^2 - Sqrt[3]*c^2 + 2*S)*
         (-(Sqrt[3]*a^2) - Sqrt[3]*b^2 + Sqrt[3]*c^2 + 2*S)), 
       (b^2*(3*a^6*b^2 - 6*a^4*b^4 + 3*a^2*b^6 - 15*a^4*b^2*c^2 - 
          15*a^2*b^4*c^2 - 3*a^4*c^4 - 3*b^4*c^4 + 6*a^2*c^6 + 6*b^2*c^6 - 
          3*c^8 + 2*Sqrt[3]*a^4*b^2*S + 2*Sqrt[3]*a^2*b^4*S + 
          4*Sqrt[3]*a^4*c^2*S + 16*Sqrt[3]*a^2*b^2*c^2*S + 
          4*Sqrt[3]*b^4*c^2*S - 6*Sqrt[3]*a^2*c^4*S - 6*Sqrt[3]*b^2*c^4*S + 
          2*Sqrt[3]*c^6*S))/(-(Sqrt[3]*a^2) - Sqrt[3]*b^2 + Sqrt[3]*c^2 + 
         2*S), (c^2*(-3*a^4*b^4 + 6*a^2*b^6 - 3*b^8 + 3*a^6*c^2 - 
          15*a^4*b^2*c^2 + 6*b^6*c^2 - 6*a^4*c^4 - 15*a^2*b^2*c^4 - 
          3*b^4*c^4 + 3*a^2*c^6 + 4*Sqrt[3]*a^4*b^2*S - 6*Sqrt[3]*a^2*b^4*S + 
          2*Sqrt[3]*b^6*S + 2*Sqrt[3]*a^4*c^2*S + 16*Sqrt[3]*a^2*b^2*c^2*S - 
          6*Sqrt[3]*b^4*c^2*S + 2*Sqrt[3]*a^2*c^4*S + 4*Sqrt[3]*b^2*c^4*S))/
        (-(Sqrt[3]*a^2) + Sqrt[3]*b^2 - Sqrt[3]*c^2 + 2*S)}, 
     "3rd Fermat-Dao" -> {9*a^6 - 18*a^4*b^2 + 6*a^2*b^4 + 3*b^6 - 
        18*a^4*c^2 - 30*a^2*b^2*c^2 - 3*b^4*c^2 + 6*a^2*c^4 - 3*b^2*c^4 + 
        3*c^6 - 2*Sqrt[3]*a^4*S - 14*Sqrt[3]*a^2*b^2*S + 6*Sqrt[3]*b^4*S - 
        14*Sqrt[3]*a^2*c^2*S - 12*Sqrt[3]*b^2*c^2*S + 6*Sqrt[3]*c^4*S, 
       6*a^4*b^2 - 9*a^2*b^4 + 3*b^6 + 3*a^4*c^2 - 12*a^2*b^2*c^2 - 
        6*b^4*c^2 - 6*a^2*c^4 + 3*c^6 + 4*Sqrt[3]*a^4*S - 
        4*Sqrt[3]*a^2*b^2*S - 2*Sqrt[3]*b^4*S - 6*Sqrt[3]*a^2*c^2*S - 
        6*Sqrt[3]*b^2*c^2*S + 2*Sqrt[3]*c^4*S, 3*a^4*b^2 - 6*a^2*b^4 + 
        3*b^6 + 6*a^4*c^2 - 12*a^2*b^2*c^2 - 9*a^2*c^4 - 6*b^2*c^4 + 3*c^6 + 
        4*Sqrt[3]*a^4*S - 6*Sqrt[3]*a^2*b^2*S + 2*Sqrt[3]*b^4*S - 
        4*Sqrt[3]*a^2*c^2*S - 6*Sqrt[3]*b^2*c^2*S - 2*Sqrt[3]*c^4*S}, 
     "4th Fermat-Dao" -> {-9*a^6 + 18*a^4*b^2 - 6*a^2*b^4 - 3*b^6 + 
        18*a^4*c^2 + 30*a^2*b^2*c^2 + 3*b^4*c^2 - 6*a^2*c^4 + 3*b^2*c^4 - 
        3*c^6 - 2*Sqrt[3]*a^4*S - 14*Sqrt[3]*a^2*b^2*S + 6*Sqrt[3]*b^4*S - 
        14*Sqrt[3]*a^2*c^2*S - 12*Sqrt[3]*b^2*c^2*S + 6*Sqrt[3]*c^4*S, 
       -6*a^4*b^2 + 9*a^2*b^4 - 3*b^6 - 3*a^4*c^2 + 12*a^2*b^2*c^2 + 
        6*b^4*c^2 + 6*a^2*c^4 - 3*c^6 + 4*Sqrt[3]*a^4*S - 
        4*Sqrt[3]*a^2*b^2*S - 2*Sqrt[3]*b^4*S - 6*Sqrt[3]*a^2*c^2*S - 
        6*Sqrt[3]*b^2*c^2*S + 2*Sqrt[3]*c^4*S, -3*a^4*b^2 + 6*a^2*b^4 - 
        3*b^6 - 6*a^4*c^2 + 12*a^2*b^2*c^2 + 9*a^2*c^4 + 6*b^2*c^4 - 3*c^6 + 
        4*Sqrt[3]*a^4*S - 6*Sqrt[3]*a^2*b^2*S + 2*Sqrt[3]*b^4*S - 
        4*Sqrt[3]*a^2*c^2*S - 6*Sqrt[3]*b^2*c^2*S - 2*Sqrt[3]*c^4*S}, 
     "5th Fermat-Dao" -> {-1/9*(-2*Sqrt[3]*a^4*b^2 + 4*Sqrt[3]*a^2*b^4 - 
          2*Sqrt[3]*b^6 - 2*Sqrt[3]*a^4*c^2 + 5*Sqrt[3]*a^2*b^2*c^2 + 
          5*Sqrt[3]*b^4*c^2 + 4*Sqrt[3]*a^2*c^4 + 5*Sqrt[3]*b^2*c^4 - 
          2*Sqrt[3]*c^6 + 30*b^2*c^2*S)/(-(Sqrt[3]*a^2) + Sqrt[3]*b^2 + 
          Sqrt[3]*c^2 + 2*S), (b^2*(Sqrt[3]*a^4 - 2*Sqrt[3]*a^2*b^2 + 
          Sqrt[3]*b^4 + Sqrt[3]*a^2*c^2 - 5*Sqrt[3]*b^2*c^2 - 2*Sqrt[3]*c^4 + 
          6*a^2*S - 6*b^2*S - 12*c^2*S))/(9*(Sqrt[3]*a^2 - Sqrt[3]*b^2 + 
          Sqrt[3]*c^2 + 2*S)), (c^2*(Sqrt[3]*a^4 + Sqrt[3]*a^2*b^2 - 
          2*Sqrt[3]*b^4 - 2*Sqrt[3]*a^2*c^2 - 5*Sqrt[3]*b^2*c^2 + 
          Sqrt[3]*c^4 + 6*a^2*S - 12*b^2*S - 6*c^2*S))/
        (9*(Sqrt[3]*a^2 + Sqrt[3]*b^2 - Sqrt[3]*c^2 + 2*S))}, 
     "6th Fermat-Dao" -> {-1/9*(2*Sqrt[3]*a^4*b^2 - 4*Sqrt[3]*a^2*b^4 + 
          2*Sqrt[3]*b^6 + 2*Sqrt[3]*a^4*c^2 - 5*Sqrt[3]*a^2*b^2*c^2 - 
          5*Sqrt[3]*b^4*c^2 - 4*Sqrt[3]*a^2*c^4 - 5*Sqrt[3]*b^2*c^4 + 
          2*Sqrt[3]*c^6 + 30*b^2*c^2*S)/(Sqrt[3]*a^2 - Sqrt[3]*b^2 - 
          Sqrt[3]*c^2 + 2*S), (b^2*(-(Sqrt[3]*a^4) + 2*Sqrt[3]*a^2*b^2 - 
          Sqrt[3]*b^4 - Sqrt[3]*a^2*c^2 + 5*Sqrt[3]*b^2*c^2 + 2*Sqrt[3]*c^4 + 
          6*a^2*S - 6*b^2*S - 12*c^2*S))/(9*(-(Sqrt[3]*a^2) + Sqrt[3]*b^2 - 
          Sqrt[3]*c^2 + 2*S)), (c^2*(-(Sqrt[3]*a^4) - Sqrt[3]*a^2*b^2 + 
          2*Sqrt[3]*b^4 + 2*Sqrt[3]*a^2*c^2 + 5*Sqrt[3]*b^2*c^2 - 
          Sqrt[3]*c^4 + 6*a^2*S - 12*b^2*S - 6*c^2*S))/
        (9*(-(Sqrt[3]*a^2) - Sqrt[3]*b^2 + Sqrt[3]*c^2 + 2*S))}, 
     "7th Fermat-Dao" -> {(4*(Sqrt[3]*a^2 + 2*S))/(-(Sqrt[3]*a^2) + 
         Sqrt[3]*b^2 + Sqrt[3]*c^2 + 2*S), 1, 1}, "8th Fermat-Dao" -> 
      {(4*(-(Sqrt[3]*a^2) + 2*S))/(Sqrt[3]*a^2 - Sqrt[3]*b^2 - Sqrt[3]*c^2 + 
         2*S), 1, 1}, "9th Fermat-Dao" -> 
      {(2*(-(a^4*b^2) + 2*a^2*b^4 - b^6 - a^4*c^2 + 10*a^2*b^2*c^2 + 
          b^4*c^2 + 2*a^2*c^4 + b^2*c^4 - c^6 + 2*Sqrt[3]*a^2*b^2*S - 
          2*Sqrt[3]*b^4*S + 2*Sqrt[3]*a^2*c^2*S + 8*Sqrt[3]*b^2*c^2*S - 
          2*Sqrt[3]*c^4*S))/(-(Sqrt[3]*a^2) + Sqrt[3]*b^2 + Sqrt[3]*c^2 + 
          2*S)^2, b^2, c^2}, "10th Fermat-Dao" -> 
      {(-2*(a^4*b^2 - 2*a^2*b^4 + b^6 + a^4*c^2 - 10*a^2*b^2*c^2 - b^4*c^2 - 
          2*a^2*c^4 - b^2*c^4 + c^6 + 2*Sqrt[3]*a^2*b^2*S - 2*Sqrt[3]*b^4*S + 
          2*Sqrt[3]*a^2*c^2*S + 8*Sqrt[3]*b^2*c^2*S - 2*Sqrt[3]*c^4*S))/
        (Sqrt[3]*a^2 - Sqrt[3]*b^2 - Sqrt[3]*c^2 + 2*S)^2, b^2, c^2}, 
     "11th Fermat-Dao" -> {3*a^6 - 9*a^4*b^2 + 3*a^2*b^4 + 3*b^6 - 
        9*a^4*c^2 - 18*a^2*b^2*c^2 - 3*b^4*c^2 + 3*a^2*c^4 - 3*b^2*c^4 + 
        3*c^6 - 2*Sqrt[3]*a^4*S - 8*Sqrt[3]*a^2*b^2*S + 6*Sqrt[3]*b^4*S - 
        8*Sqrt[3]*a^2*c^2*S - 12*Sqrt[3]*b^2*c^2*S + 6*Sqrt[3]*c^4*S, 
       Sqrt[3]*(a^2 + b^2 - c^2)*(Sqrt[3]*a^2*b^2 - Sqrt[3]*b^4 + 
         Sqrt[3]*a^2*c^2 - Sqrt[3]*c^4 + 4*a^2*S - 2*b^2*S - 2*c^2*S), 
       Sqrt[3]*(a^2 - b^2 + c^2)*(Sqrt[3]*a^2*b^2 - Sqrt[3]*b^4 + 
         Sqrt[3]*a^2*c^2 - Sqrt[3]*c^4 + 4*a^2*S - 2*b^2*S - 2*c^2*S)}, 
     "12th Fermat-Dao" -> {-3*a^6 + 9*a^4*b^2 - 3*a^2*b^4 - 3*b^6 + 
        9*a^4*c^2 + 18*a^2*b^2*c^2 + 3*b^4*c^2 - 3*a^2*c^4 + 3*b^2*c^4 - 
        3*c^6 - 2*Sqrt[3]*a^4*S - 8*Sqrt[3]*a^2*b^2*S + 6*Sqrt[3]*b^4*S - 
        8*Sqrt[3]*a^2*c^2*S - 12*Sqrt[3]*b^2*c^2*S + 6*Sqrt[3]*c^4*S, 
       Sqrt[3]*(a^2 + b^2 - c^2)*(-(Sqrt[3]*a^2*b^2) + Sqrt[3]*b^4 - 
         Sqrt[3]*a^2*c^2 + Sqrt[3]*c^4 + 4*a^2*S - 2*b^2*S - 2*c^2*S), 
       Sqrt[3]*(a^2 - b^2 + c^2)*(-(Sqrt[3]*a^2*b^2) + Sqrt[3]*b^4 - 
         Sqrt[3]*a^2*c^2 + Sqrt[3]*c^4 + 4*a^2*S - 2*b^2*S - 2*c^2*S)}, 
     "13th Fermat-Dao" -> {(-(Sqrt[3]*a^2*b^2) + Sqrt[3]*b^4 - 
         Sqrt[3]*a^2*c^2 - 4*Sqrt[3]*b^2*c^2 + Sqrt[3]*c^4 - 2*b^2*S - 
         2*c^2*S)/(Sqrt[3]*a^2 - Sqrt[3]*b^2 - Sqrt[3]*c^2 + 2*S), b^2, c^2}, 
     "14th Fermat-Dao" -> {(Sqrt[3]*a^2*b^2 - Sqrt[3]*b^4 + Sqrt[3]*a^2*c^2 + 
         4*Sqrt[3]*b^2*c^2 - Sqrt[3]*c^4 - 2*b^2*S - 2*c^2*S)/
        (-(Sqrt[3]*a^2) + Sqrt[3]*b^2 + Sqrt[3]*c^2 + 2*S), b^2, c^2}, 
     "15th Fermat-Dao" -> {2*(6*a^8 + 3*a^6*b^2 - 30*a^4*b^4 + 24*a^2*b^6 - 
         3*b^8 + 3*a^6*c^2 - 69*a^4*b^2*c^2 - 33*a^2*b^4*c^2 + 18*b^6*c^2 - 
         30*a^4*c^4 - 33*a^2*b^2*c^4 - 30*b^4*c^4 + 24*a^2*c^6 + 18*b^2*c^6 - 
         3*c^8 + 8*Sqrt[3]*a^6*S - 26*Sqrt[3]*a^4*b^2*S + 
         2*Sqrt[3]*a^2*b^4*S + 6*Sqrt[3]*b^6*S - 26*Sqrt[3]*a^4*c^2*S - 
         50*Sqrt[3]*a^2*b^2*c^2*S - 6*Sqrt[3]*b^4*c^2*S + 
         2*Sqrt[3]*a^2*c^4*S - 6*Sqrt[3]*b^2*c^4*S + 6*Sqrt[3]*c^6*S), 
       (-(Sqrt[3]*a^4) + 3*Sqrt[3]*a^2*b^2 - 2*Sqrt[3]*b^4 + 
         3*Sqrt[3]*a^2*c^2 + 2*Sqrt[3]*b^2*c^2 - 2*Sqrt[3]*c^4 + 6*a^2*S)*
        (2*Sqrt[3]*a^2*b^2 + Sqrt[3]*a^2*c^2 + Sqrt[3]*b^2*c^2 - 
         Sqrt[3]*c^4 + 4*a^2*S + 4*b^2*S - 2*c^2*S), 
       (-(Sqrt[3]*a^4) + 3*Sqrt[3]*a^2*b^2 - 2*Sqrt[3]*b^4 + 
         3*Sqrt[3]*a^2*c^2 + 2*Sqrt[3]*b^2*c^2 - 2*Sqrt[3]*c^4 + 6*a^2*S)*
        (Sqrt[3]*a^2*b^2 - Sqrt[3]*b^4 + 2*Sqrt[3]*a^2*c^2 + 
         Sqrt[3]*b^2*c^2 + 4*a^2*S - 2*b^2*S + 4*c^2*S)}, 
     "16th Fermat-Dao" -> {-2*(-6*a^8 - 3*a^6*b^2 + 30*a^4*b^4 - 24*a^2*b^6 + 
         3*b^8 - 3*a^6*c^2 + 69*a^4*b^2*c^2 + 33*a^2*b^4*c^2 - 18*b^6*c^2 + 
         30*a^4*c^4 + 33*a^2*b^2*c^4 + 30*b^4*c^4 - 24*a^2*c^6 - 18*b^2*c^6 + 
         3*c^8 + 8*Sqrt[3]*a^6*S - 26*Sqrt[3]*a^4*b^2*S + 
         2*Sqrt[3]*a^2*b^4*S + 6*Sqrt[3]*b^6*S - 26*Sqrt[3]*a^4*c^2*S - 
         50*Sqrt[3]*a^2*b^2*c^2*S - 6*Sqrt[3]*b^4*c^2*S + 
         2*Sqrt[3]*a^2*c^4*S - 6*Sqrt[3]*b^2*c^4*S + 6*Sqrt[3]*c^6*S), 
       (Sqrt[3]*a^4 - 3*Sqrt[3]*a^2*b^2 + 2*Sqrt[3]*b^4 - 3*Sqrt[3]*a^2*c^2 - 
         2*Sqrt[3]*b^2*c^2 + 2*Sqrt[3]*c^4 + 6*a^2*S)*(-2*Sqrt[3]*a^2*b^2 - 
         Sqrt[3]*a^2*c^2 - Sqrt[3]*b^2*c^2 + Sqrt[3]*c^4 + 4*a^2*S + 
         4*b^2*S - 2*c^2*S), (Sqrt[3]*a^4 - 3*Sqrt[3]*a^2*b^2 + 
         2*Sqrt[3]*b^4 - 3*Sqrt[3]*a^2*c^2 - 2*Sqrt[3]*b^2*c^2 + 
         2*Sqrt[3]*c^4 + 6*a^2*S)*(-(Sqrt[3]*a^2*b^2) + Sqrt[3]*b^4 - 
         2*Sqrt[3]*a^2*c^2 - Sqrt[3]*b^2*c^2 + 4*a^2*S - 2*b^2*S + 4*c^2*S)}, 
     "1st inner-Fermat-Dao-Nhi" -> {3*Sqrt[3]*a^2 - 7*S, 2*S - 3*Sqrt[3]*SC, 
       2*S - 3*Sqrt[3]*SB}, "2nd inner-Fermat-Dao-Nhi" -> 
      {3*Sqrt[3]*a^2 - 5*S, 4*S - 3*Sqrt[3]*SC, 4*S - 3*Sqrt[3]*SB}, 
     "3rd inner-Fermat-Dao-Nhi" -> {-3*Sqrt[3]*a^2 - 4*S, 5*S + 3*Sqrt[3]*SC, 
       5*S + 3*Sqrt[3]*SB}, "4th inner-Fermat-Dao-Nhi" -> 
      {-3*Sqrt[3]*a^2 - 8*S, S + 3*Sqrt[3]*SC, S + 3*Sqrt[3]*SB}, 
     "1st outer-Fermat-Dao-Nhi" -> {-3*Sqrt[3]*a^2 - 7*S, 2*S + 3*Sqrt[3]*SC, 
       2*S + 3*Sqrt[3]*SB}, "2nd outer-Fermat-Dao-Nhi" -> 
      {-3*Sqrt[3]*a^2 - 5*S, 4*S + 3*Sqrt[3]*SC, 4*S + 3*Sqrt[3]*SB}, 
     "3rd outer-Fermat-Dao-Nhi" -> {3*Sqrt[3]*a^2 - 4*S, 5*S - 3*Sqrt[3]*SC, 
       5*S - 3*Sqrt[3]*SB}, "4th outer-Fermat-Dao-Nhi" -> 
      {3*Sqrt[3]*a^2 - 8*S, S - 3*Sqrt[3]*SC, S - 3*Sqrt[3]*SB}, 
     "Feuerbach" -> {-((b - c)^2*(a + b + c)), (a + b - c)*(a + c)^2, 
       (a + b)^2*(a - b + c)}, "Fuhrmann" -> {-a^2, a^2 - b*c - c^2, 
       a^2 - b^2 - b*c}, "2nd Fuhrmann" -> {-a^2, a^2 + b*c - c^2, 
       a^2 - b^2 + b*c}, "inner-Garcia" -> {a^2, -b^2 + a*c + c^2, 
       a*b + b^2 - c^2}, "outer-Garcia" -> {-a, a + c, a + b}, 
     "Garcia-Moses" -> {a, a + c, a + b}, "Garcia-reflection" -> 
      {-a, a - c, a - b}, "Gossard" -> {(b - c)^2*(b + c)^2*
        (-a^2 + b^2 + c^2)^2*(2*a^4 - a^2*b^2 - b^4 - a^2*c^2 + 2*b^2*c^2 - 
         c^4), (a^4 + a^2*b^2 - 2*b^4 - 2*a^2*c^2 + b^2*c^2 + c^4)*
        (a^8 - 3*a^6*b^2 + 2*a^4*b^4 + a^2*b^6 - b^8 + a^6*c^2 + 
         3*a^4*b^2*c^2 - 5*a^2*b^4*c^2 + b^6*c^2 - 4*a^4*c^4 + 
         3*a^2*b^2*c^4 + 2*b^4*c^4 + a^2*c^6 - 3*b^2*c^6 + c^8), 
       (a^4 - 2*a^2*b^2 + b^4 + a^2*c^2 + b^2*c^2 - 2*c^4)*
        (a^8 + a^6*b^2 - 4*a^4*b^4 + a^2*b^6 + b^8 - 3*a^6*c^2 + 
         3*a^4*b^2*c^2 + 3*a^2*b^4*c^2 - 3*b^6*c^2 + 2*a^4*c^4 - 
         5*a^2*b^2*c^4 + 2*b^4*c^4 + a^2*c^6 + b^2*c^6 - c^8)}, 
     "inner-Grebe" -> {b^2 + c^2 - S, -b^2, -c^2}, 
     "outer-Grebe" -> {b^2 + c^2 + S, -b^2, -c^2}, 
     "1st half-diamonds-central" -> {Sqrt[3]*a^2 + 6*S, 3*S - Sqrt[3]*SC, 
       3*S - Sqrt[3]*SB}, "2nd half-diamonds-central" -> 
      {-(Sqrt[3]*a^2) + 6*S, 3*S + Sqrt[3]*SC, 3*S + Sqrt[3]*SB}, 
     "1st half-diamonds" -> {-(Sqrt[3]*a^2) + 2*S, S + Sqrt[3]*SC, 
       S + Sqrt[3]*SB}, "2nd half-diamonds" -> {-(Sqrt[3]*a^2) - 2*S, 
       -S + Sqrt[3]*SC, -S + Sqrt[3]*SB}, "1st half-squares" -> 
      {a^2 + S, -SC, -SB}, "2nd half-squares" -> {-a^2 + S, SC, SB}, 
     "Hatzipolakis-Moses" -> {a^4*(-b^2 - c^2) - (b^2 - c^2)*(b^4 - c^4) + 
        2*a^2*(b^4 - b^2*c^2 + c^4), b^2*(b^4 - a^2*(-a^2 + c^2) - 
         b^2*(2*a^2 + c^2)), c^2*(a^2*(a^2 - b^2) - (2*a^2 + b^2)*c^2 + 
         c^4)}, "1st Hatzipolakis" -> {a^2*(a - b - c), 
       -((a^2 + b^2 - c^2)^2/(a - b + c)), 
       -((a^2 - b^2 + c^2)^2/(a + b - c))}, "2nd Hatzipolakis" -> 
      {0, 1/((a - b + c)^2*(a^2 - b^2 + c^2)), 
       1/((a + b - c)^2*(a^2 + b^2 - c^2))}, "3rd Hatzipolakis" -> 
      {a^4*(b^2 + c^2) + (b^2 - c^2)*(b^4 - c^4) - 
        2*a^2*(b^4 - 3*b^2*c^2 + c^4), b^2*(-b^4 + a^2*(-a^2 + c^2) + 
         b^2*(2*a^2 + c^2)), c^2*(-(a^2*(a^2 - b^2)) + (2*a^2 + b^2)*c^2 - 
         c^4)}, "hexyl" -> {a*(a^3 - a^2*b - a*b^2 + b^3 - a^2*c - 2*a*b*c - 
         b^2*c - a*c^2 - b*c^2 + c^3), b*(a^3 - a^2*b - a*b^2 + b^3 + a^2*c + 
         2*a*b*c + b^2*c - a*c^2 - b*c^2 - c^3), 
       c*(a^3 + a^2*b - a*b^2 - b^3 - a^2*c + 2*a*b*c - b^2*c - a*c^2 + 
         b*c^2 + c^3)}, "Honsberger" -> 
      {-(a/(a*b - b^2 + a*c + 2*b*c - c^2)), (a - b + c)^(-1), 
       (a + b - c)^(-1)}, "Hung-Feuerbach" -> 
      {-((a + b - c)*(a - b + c)*(b + c)^2*(a^2 + b^2 + a*c - b*c)*
         (a^2 + a*b - b*c + c^2)*(a^2 + b^2 + 2*b*c + c^2)), 
       (a + c)*(a^2 + b^2 - a*c + b*c)*(a^2 + a*b + b*c + c^2)*
        (a^5 - a*b^4 + a^4*c + 3*b^4*c + 2*a*b^2*c^2 - 2*b^2*c^3 - a*c^4 - 
         c^5), (a + b)*(a^2 + b^2 + a*c + b*c)*(a^2 - a*b + b*c + c^2)*
        (a^5 + a^4*b - a*b^4 - b^5 + 2*a*b^2*c^2 - 2*b^3*c^2 - a*c^4 + 
         3*b*c^4)}, "Hutson extouch" -> {(-4*a^2)/(a + b + c), a + b - c, 
       a - b + c}, "inner-Hutson" -> 
      {a*(a^2*Sin[A/2] - 2*a*b*Sin[A/2] + b^2*Sin[A/2] - 2*a*c*Sin[A/2] - 
         2*b*c*Sin[A/2] + c^2*Sin[A/2] + a^2*Sin[B/2] - 2*a*b*Sin[B/2] + 
         b^2*Sin[B/2] + 2*a*c*Sin[B/2] - 2*b*c*Sin[B/2] + c^2*Sin[B/2] + 
         a^2*Sin[C/2] + 2*a*b*Sin[C/2] + b^2*Sin[C/2] - 2*a*c*Sin[C/2] - 
         2*b*c*Sin[C/2] + c^2*Sin[C/2]), a^2*b*Sin[A/2] - 2*a*b^2*Sin[A/2] + 
        b^3*Sin[A/2] + 2*a*b*c*Sin[A/2] + 2*b^2*c*Sin[A/2] - 
        3*b*c^2*Sin[A/2] - a^3*Sin[B/2] + 2*a^2*b*Sin[B/2] - a*b^2*Sin[B/2] - 
        a^2*c*Sin[B/2] + b^2*c*Sin[B/2] + a*c^2*Sin[B/2] - 2*b*c^2*Sin[B/2] + 
        c^3*Sin[B/2] - a^2*b*Sin[C/2] - 2*a*b^2*Sin[C/2] - b^3*Sin[C/2] + 
        2*a*b*c*Sin[C/2] + 2*b^2*c*Sin[C/2] - b*c^2*Sin[C/2], 
       a^2*c*Sin[A/2] + 2*a*b*c*Sin[A/2] - 3*b^2*c*Sin[A/2] - 
        2*a*c^2*Sin[A/2] + 2*b*c^2*Sin[A/2] + c^3*Sin[A/2] - a^2*c*Sin[B/2] + 
        2*a*b*c*Sin[B/2] - b^2*c*Sin[B/2] - 2*a*c^2*Sin[B/2] + 
        2*b*c^2*Sin[B/2] - c^3*Sin[B/2] - a^3*Sin[C/2] - a^2*b*Sin[C/2] + 
        a*b^2*Sin[C/2] + b^3*Sin[C/2] + 2*a^2*c*Sin[C/2] - 2*b^2*c*Sin[C/2] - 
        a*c^2*Sin[C/2] + b*c^2*Sin[C/2]}, "Hutson intouch" -> 
      {(-4*a^2)/(-a + b + c), -a + b - c, -a - b + c}, 
     "outer-Hutson" -> {a*(a^2*Sin[A/2] - 2*a*b*Sin[A/2] + b^2*Sin[A/2] - 
         2*a*c*Sin[A/2] - 2*b*c*Sin[A/2] + c^2*Sin[A/2] - a^2*Sin[B/2] + 
         2*a*b*Sin[B/2] - b^2*Sin[B/2] - 2*a*c*Sin[B/2] + 2*b*c*Sin[B/2] - 
         c^2*Sin[B/2] - a^2*Sin[C/2] - 2*a*b*Sin[C/2] - b^2*Sin[C/2] + 
         2*a*c*Sin[C/2] + 2*b*c*Sin[C/2] - c^2*Sin[C/2]), 
       a^2*b*Sin[A/2] - 2*a*b^2*Sin[A/2] + b^3*Sin[A/2] + 2*a*b*c*Sin[A/2] + 
        2*b^2*c*Sin[A/2] - 3*b*c^2*Sin[A/2] + a^3*Sin[B/2] - 
        2*a^2*b*Sin[B/2] + a*b^2*Sin[B/2] + a^2*c*Sin[B/2] - b^2*c*Sin[B/2] - 
        a*c^2*Sin[B/2] + 2*b*c^2*Sin[B/2] - c^3*Sin[B/2] + a^2*b*Sin[C/2] + 
        2*a*b^2*Sin[C/2] + b^3*Sin[C/2] - 2*a*b*c*Sin[C/2] - 
        2*b^2*c*Sin[C/2] + b*c^2*Sin[C/2], a^2*c*Sin[A/2] + 
        2*a*b*c*Sin[A/2] - 3*b^2*c*Sin[A/2] - 2*a*c^2*Sin[A/2] + 
        2*b*c^2*Sin[A/2] + c^3*Sin[A/2] + a^2*c*Sin[B/2] - 2*a*b*c*Sin[B/2] + 
        b^2*c*Sin[B/2] + 2*a*c^2*Sin[B/2] - 2*b*c^2*Sin[B/2] + c^3*Sin[B/2] + 
        a^3*Sin[C/2] + a^2*b*Sin[C/2] - a*b^2*Sin[C/2] - b^3*Sin[C/2] - 
        2*a^2*c*Sin[C/2] + 2*b^2*c*Sin[C/2] + a*c^2*Sin[C/2] - 
        b*c^2*Sin[C/2]}, "1st Hyacinth" -> 
      {2*a^4*(-a^2 + b^2 + c^2)*(a^4 - 2*a^2*b^2 + b^4 - 2*a^2*c^2 - 
         b^2*c^2 + c^4), a^10 - 3*a^8*b^2 + 4*a^6*b^4 - 4*a^4*b^6 + 
        3*a^2*b^8 - b^10 - 3*a^8*c^2 + 5*a^6*b^2*c^2 - 3*a^4*b^4*c^2 - 
        3*a^2*b^6*c^2 + 4*b^8*c^2 + 2*a^6*c^4 - 5*a^4*b^2*c^4 - 
        4*a^2*b^4*c^4 - 7*b^6*c^4 + 2*a^4*c^6 + 7*a^2*b^2*c^6 + 7*b^4*c^6 - 
        3*a^2*c^8 - 4*b^2*c^8 + c^10, a^10 - 3*a^8*b^2 + 2*a^6*b^4 + 
        2*a^4*b^6 - 3*a^2*b^8 + b^10 - 3*a^8*c^2 + 5*a^6*b^2*c^2 - 
        5*a^4*b^4*c^2 + 7*a^2*b^6*c^2 - 4*b^8*c^2 + 4*a^6*c^4 - 
        3*a^4*b^2*c^4 - 4*a^2*b^4*c^4 + 7*b^6*c^4 - 4*a^4*c^6 - 
        3*a^2*b^2*c^6 - 7*b^4*c^6 + 3*a^2*c^8 + 4*b^2*c^8 - c^10}, 
     "2nd Hyacinth" -> {(a^4*b^2 - 2*a^2*b^4 + b^6 + a^4*c^2 + 
         4*a^2*b^2*c^2 - b^4*c^2 - 2*a^2*c^4 - b^2*c^4 + c^6)/
        (-a^2 + b^2 + c^2), b^2*(a^2 - b^2 + c^2), c^2*(a^2 + b^2 - c^2)}, 
     "incentral" -> {0, b, c}, "incircle-circles" -> 
      {2*a^2, a^2 + 4*a*b + b^2 - c^2, a^2 - b^2 + 4*a*c + c^2}, 
     "infinite-altitude" -> {-2*a^2, a^2 + b^2 - c^2, a^2 - b^2 + c^2}, 
     "intangents" -> {a^2*(a - b - c), b*(a - c)*(a - b + c), 
       (a - b)*(a + b - c)*c}, "intouch" -> {0, (a - b + c)^(-1), 
       (a + b - c)^(-1)}, "inverse-in-Conway" -> 
      {(a*b^2 + a*b*c + b^2*c + a*c^2 + b*c^2)/(a*(b + c)), -b, -c}, 
     "inverse-in-excircles" -> {(-(a*b) - b^2 - a*c + 2*b*c - c^2)/
        (a + b + c), b, c}, "inverse-in-incircle" -> 
      {(-(a*b) + b^2 - a*c - 2*b*c + c^2)/(-a + b + c), -b, -c}, 
     "1st isodynamic-Dao" -> {a^2*(Sqrt[3]*a^2 - Sqrt[3]*b^2 - Sqrt[3]*c^2 - 
         2*S), -(Sqrt[3]*a^4) + Sqrt[3]*a^2*b^2 + 2*Sqrt[3]*a^2*c^2 + 
        Sqrt[3]*b^2*c^2 - Sqrt[3]*c^4 + 2*a^2*S + 4*b^2*S - 2*c^2*S, 
       -(Sqrt[3]*a^4) + 2*Sqrt[3]*a^2*b^2 - Sqrt[3]*b^4 + Sqrt[3]*a^2*c^2 + 
        Sqrt[3]*b^2*c^2 + 2*a^2*S - 2*b^2*S + 4*c^2*S}, 
     "2nd isodynamic-Dao" -> {a^2*(-(Sqrt[3]*a^2) + Sqrt[3]*b^2 + 
         Sqrt[3]*c^2 - 2*S), Sqrt[3]*a^4 - Sqrt[3]*a^2*b^2 - 
        2*Sqrt[3]*a^2*c^2 - Sqrt[3]*b^2*c^2 + Sqrt[3]*c^4 + 2*a^2*S + 
        4*b^2*S - 2*c^2*S, Sqrt[3]*a^4 - 2*Sqrt[3]*a^2*b^2 + Sqrt[3]*b^4 - 
        Sqrt[3]*a^2*c^2 - Sqrt[3]*b^2*c^2 + 2*a^2*S - 2*b^2*S + 4*c^2*S}, 
     "3rd isodynamic-Dao" -> {(-2*(Sqrt[3]*a^2*b^2 - Sqrt[3]*b^4 + 
          Sqrt[3]*a^2*c^2 + 2*Sqrt[3]*b^2*c^2 - Sqrt[3]*c^4 + 4*a^2*S))/
        (-a^2 + b^2 + c^2), -(Sqrt[3]*a^2) - Sqrt[3]*b^2 + Sqrt[3]*c^2 - 2*S, 
       -(Sqrt[3]*a^2) + Sqrt[3]*b^2 - Sqrt[3]*c^2 - 2*S}, 
     "4th isodynamic-Dao" -> {(-2*(-(Sqrt[3]*a^2*b^2) + Sqrt[3]*b^4 - 
          Sqrt[3]*a^2*c^2 - 2*Sqrt[3]*b^2*c^2 + Sqrt[3]*c^4 + 4*a^2*S))/
        (-a^2 + b^2 + c^2), Sqrt[3]*a^2 + Sqrt[3]*b^2 - Sqrt[3]*c^2 - 2*S, 
       Sqrt[3]*a^2 - Sqrt[3]*b^2 + Sqrt[3]*c^2 - 2*S}, 
     "Jenkins-contact" -> {-((b + c)^2/(a + b + c)), c^2/(a + b - c), 
       b^2/(a - b + c)}, "Jenkins-tangential" -> 
      {(b*c*(-3*a^3*b - 2*a^2*b^2 + a*b^3 - 3*a^3*c - 3*a^2*b*c + a*b^2*c + 
          b^3*c - 2*a^2*c^2 + a*b*c^2 + 2*b^2*c^2 + a*c^3 + b*c^3))/a, 
       (a + c)*(a^3*b + 2*a^2*b^2 + a*b^3 + a^3*c - a^2*b*c - a*b^2*c + 
         b^3*c - a*b*c^2 - a*c^3 - b*c^3), (a + b)*(a^3*b - a*b^3 + a^3*c - 
         a^2*b*c - a*b^2*c - b^3*c + 2*a^2*c^2 - a*b*c^2 + a*c^3 + b*c^3)}, 
     "1st Jenkins" -> {-2*a^4*b - 3*a^3*b^2 + a^2*b^3 + a*b^4 - b^5 - 
        2*a^4*c - 4*a^3*b*c + a^2*b^2*c - b^4*c - 3*a^3*c^2 + a^2*b*c^2 - 
        2*a*b^2*c^2 + 2*b^3*c^2 + a^2*c^3 + 2*b^2*c^3 + a*c^4 - b*c^4 - c^5, 
       2*a^3*b^2 + 2*a^2*b^3 + 2*a^3*b*c + 2*a^2*b^2*c + 2*a*b^3*c + 
        a^3*c^2 + a^2*b*c^2 + a*b^2*c^2 + b^3*c^2 + a^2*c^3 - 2*a*b*c^3 + 
        b^2*c^3 - a*c^4 - b*c^4 - c^5, a^3*b^2 + a^2*b^3 - a*b^4 - b^5 + 
        2*a^3*b*c + a^2*b^2*c - 2*a*b^3*c - b^4*c + 2*a^3*c^2 + 2*a^2*b*c^2 + 
        a*b^2*c^2 + b^3*c^2 + 2*a^2*c^3 + 2*a*b*c^3 + b^2*c^3}, 
     "2nd Jenkins" -> {((-a - b - c)*(a*b + b^2 + a*c + c^2))/
        ((a + b - c)*(a - b + c)), a + c, a + b}, 
     "3rd Jenkins" -> {(a + b + c)*(2*a^4*b + 3*a^3*b^2 + a^2*b^3 + 2*a^4*c + 
         4*a^3*b*c + 6*a^2*b^2*c + 3*a*b^3*c + 3*a^3*c^2 + 6*a^2*b*c^2 + 
         3*a*b^2*c^2 + b^3*c^2 + a^2*c^3 + 3*a*b*c^3 + b^2*c^3), 
       (a + c)*(a - b + c)*(a^3*b + 2*a^2*b^2 + a*b^3 + a^3*c + a^2*b*c + 
         a*b^2*c + b^3*c + a^2*c^2 + 3*a*b*c^2 + b^2*c^2), 
       (a + b)*(a + b - c)*(a^3*b + a^2*b^2 + a^3*c + a^2*b*c + 3*a*b^2*c + 
         2*a^2*c^2 + a*b*c^2 + b^2*c^2 + a*c^3 + b*c^3)}, 
     "Johnson" -> {a^2*(-a^2 + b^2 + c^2), a^4 - a^2*b^2 - 2*a^2*c^2 - 
        b^2*c^2 + c^4, a^4 - 2*a^2*b^2 + b^4 - a^2*c^2 - b^2*c^2}, 
     "inner-Johnson" -> {a*(-a^2 + a*b + a*c - 2*b*c), 
       (-a + c)^2*(a - b + c), (a - b)^2*(a + b - c)}, 
     "outer-Johnson" -> {a*(-a^2 - a*b - a*c - 2*b*c), (a + b - c)*(a + c)^2, 
       (a + b)^2*(a - b + c)}, "1st Johnson-Yff" -> {-(a^2/(a - b - c)), 
       (a + c)^2/(a - b + c), (a + b)^2/(a + b - c)}, 
     "2nd Johnson-Yff" -> {a^2*(-a + b + c), (-a + c)^2*(a - b + c), 
       (a - b)^2*(a + b - c)}, "K798e" -> {2*a^2*b*c, 
       b^4 - b^3*c + b*c*(-a^2 + c^2) + (-a^2 + c^2)^2 - 2*b^2*(a^2 + c^2), 
       (a^2 - b^2)^2 - b*(a^2 - b^2)*c - 2*(a^2 + b^2)*c^2 - b*c^3 + c^4}, 
     "K798i" -> {-2*a^2*b*c, a^4 - 2*a^2*b^2 + b^4 + a^2*b*c + b^3*c - 
        2*a^2*c^2 - 2*b^2*c^2 - b*c^3 + c^4, a^4 - 2*a^2*b^2 + b^4 + 
        a^2*b*c - b^3*c - 2*a^2*c^2 - 2*b^2*c^2 + b*c^3 + c^4}, 
     "1st Kenmotu-centers" -> {a^2 + 2*S, b^2, c^2}, 
     "2nd Kenmotu-centers" -> {a^2 - 2*S, b^2, c^2}, 
     "1st Kenmotu diagonals" -> {(a^2*(-a^2 + b^2 + c^2 - 2*S))/
        (-a^2 + b^2 + c^2 + 2*S), b^2, c^2}, "2nd Kenmotu diagonals" -> 
      {(a^2*(-a^2 + b^2 + c^2 + 2*S))/(a^2 - b^2 - c^2 + 2*S), -b^2, -c^2}, 
     "1st Kenmotu-free-vertices" -> {-a^4 + 3*a^2*b^2 - 2*b^4 + 3*a^2*c^2 + 
        4*b^2*c^2 - 2*c^4 + 2*a^2*S, b^2*(-a^2 + b^2 - c^2 + 2*S), 
       c^2*(-a^2 - b^2 + c^2 + 2*S)}, "2nd Kenmotu-free-vertices" -> 
      {-a^4 - 2*(b^2 - c^2)^2 + 3*a^2*(b^2 + c^2) - 2*a^2*S, 
       b^2*(-a^2 + b^2 - c^2 - 2*S), c^2*(-a^2 - b^2 + c^2 - 2*S)}, 
     "Kosnita" -> {a^2*(a^4 - 2*a^2*b^2 + b^4 - 2*a^2*c^2 + c^4), 
       b^2*(-a^4 + 2*a^2*b^2 - b^4 + a^2*c^2 + b^2*c^2), 
       c^2*(-a^4 + a^2*b^2 + 2*a^2*c^2 + b^2*c^2 - c^4)}, 
     "Largest-circumscribed-equilateral" -> 
      {3*Sqrt[3]*a^4 + 3*Sqrt[3]*a^2*b^2 - 2*Sqrt[3]*b^4 + 
        3*Sqrt[3]*a^2*c^2 + 4*Sqrt[3]*b^2*c^2 - 2*Sqrt[3]*c^4 + 18*a^2*S, 
       -7*Sqrt[3]*a^2*b^2 + 3*Sqrt[3]*b^4 - 2*Sqrt[3]*a^2*c^2 - 
        5*Sqrt[3]*b^2*c^2 + 2*Sqrt[3]*c^4 - 12*a^2*S - 6*b^2*S, 
       -2*Sqrt[3]*a^2*b^2 + 2*Sqrt[3]*b^4 - 7*Sqrt[3]*a^2*c^2 - 
        5*Sqrt[3]*b^2*c^2 + 3*Sqrt[3]*c^4 - 12*a^2*S - 6*c^2*S}, 
     "Lemoine" -> {0, (2*a^2 - b^2 + 2*c^2)^(-1), (2*a^2 + 2*b^2 - c^2)^
        (-1)}, "1st Lemoine-Dao" -> 
      {(-2*a^2*(-(Sqrt[3]*a^2) + Sqrt[3]*b^2 + Sqrt[3]*c^2 + 4*S))/
        (-a^2 + b^2 + c^2), Sqrt[3]*a^2 + Sqrt[3]*b^2 - Sqrt[3]*c^2 - 2*S, 
       Sqrt[3]*a^2 - Sqrt[3]*b^2 + Sqrt[3]*c^2 - 2*S}, 
     "2nd Lemoine-Dao" -> {(-2*a^2*(Sqrt[3]*a^2 - Sqrt[3]*b^2 - Sqrt[3]*c^2 + 
          4*S))/(-a^2 + b^2 + c^2), -(Sqrt[3]*a^2) - Sqrt[3]*b^2 + 
        Sqrt[3]*c^2 - 2*S, -(Sqrt[3]*a^2) + Sqrt[3]*b^2 - Sqrt[3]*c^2 - 2*S}, 
     "inner-Le Viet An" -> {(a^2*(-(Sqrt[3]*a^2) - Sqrt[3]*b^2 - 
          Sqrt[3]*c^2 + 6*S))/(b^2 - c^2), 2*Sqrt[3]*b^2, -2*Sqrt[3]*c^2}, 
     "outer-Le Viet An" -> {(a^2*(Sqrt[3]*a^2 + Sqrt[3]*b^2 + Sqrt[3]*c^2 + 
          6*S))/(b^2 - c^2), -2*Sqrt[3]*b^2, 2*Sqrt[3]*c^2}, 
     "Lucas antipodal" -> {a*(Cos[B]*Cos[C] - Sin[A]), -(b*Cos[B]), 
       -(c*Cos[C])}, "Lucas(-1) antipodal" -> {a*(Cos[B]*Cos[C] + Sin[A]), 
       -(b*Cos[B]), -(c*Cos[C])}, "Lucas antipodal tangents" -> 
      {a^2*(-a^2 + b^2 + c^2)*(b^2 + c^2 + S), 
       b^2*(a^2*b^2 - b^4 + a^2*c^2 + 2*b^2*c^2 - c^4 + a^2*S - b^2*S + 
         3*c^2*S), c^2*(a^2*b^2 - b^4 + a^2*c^2 + 2*b^2*c^2 - c^4 + a^2*S + 
         3*b^2*S - c^2*S)}, "Lucas(-1) antipodal tangents" -> 
      {a^2*(-a^2 + b^2 + c^2)*(-b^2 - c^2 + S), 
       b^2*(-(a^2*b^2) + b^4 - a^2*c^2 - 2*b^2*c^2 + c^4 + a^2*S - b^2*S + 
         3*c^2*S), c^2*(-(a^2*b^2) + b^4 - a^2*c^2 - 2*b^2*c^2 + c^4 + 
         a^2*S + 3*b^2*S - c^2*S)}, "Lucas Brocard" -> 
      {a^2*(-a^2 + 2*S), -(b^2*(-a^2 - c^2 + S)), -(c^2*(-a^2 - b^2 + S))}, 
     "Lucas(-1) Brocard" -> {a^2*(a^2 + 2*S), -(b^2*(a^2 + c^2 + S)), 
       -(c^2*(a^2 + b^2 + S))}, "Lucas central" -> {a*(Cos[A] + 2*Sin[A]), 
       b*Cos[B], c*Cos[C]}, "Lucas(-1) central" -> {a*(Cos[A] - 2*Sin[A]), 
       b*Cos[B], c*Cos[C]}, "Lucas homothetic" -> {-1/4*(-a^2 + b^2 + c^2)^2, 
       b^2*(c^2 + S), c^2*(b^2 + S)}, "Lucas(-1) homothetic" -> 
      {-1/4*(a^2 - b^2 - c^2)^2, b^2*(c^2 - S), c^2*(b^2 - S)}, 
     "Lucas inner" -> {a*(2*Cos[A] + (3*Sin[A])/2), b*(2*Cos[B] + Sin[B]), 
       c*(2*Cos[C] + Sin[C])}, "Lucas(-1) inner" -> 
      {a*(2*Cos[A] - (3*Sin[A])/2), b*(2*Cos[B] - Sin[B]), 
       c*(2*Cos[C] - Sin[C])}, "Lucas inner tangential" -> 
      {a*(4*Cos[A] + Sin[A]), b*(4*Cos[B] + 3*Sin[B]), 
       c*(4*Cos[C] + 3*Sin[C])}, "Lucas(-1) inner tangential" -> 
      {a*(4*Cos[A] - Sin[A]), b*(4*Cos[B] - 3*Sin[B]), 
       c*(4*Cos[C] - 3*Sin[C])}, "Lucas reflection" -> 
      {(-a^12 + 3*a^10*b^2 - 2*a^8*b^4 - 2*a^6*b^6 + 3*a^4*b^8 - a^2*b^10 + 
         3*a^10*c^2 + 4*a^8*b^2*c^2 - 12*a^6*b^4*c^2 + a^4*b^6*c^2 + 
         5*a^2*b^8*c^2 - b^10*c^2 - 2*a^8*c^4 - 12*a^6*b^2*c^4 - 
         12*a^4*b^4*c^4 - 4*a^2*b^6*c^4 + 4*b^8*c^4 - 2*a^6*c^6 + 
         a^4*b^2*c^6 - 4*a^2*b^4*c^6 - 6*b^6*c^6 + 3*a^4*c^8 + 
         5*a^2*b^2*c^8 + 4*b^4*c^8 - a^2*c^10 - b^2*c^10 + 4*a^8*b^2*S - 
         8*a^6*b^4*S + 4*a^4*b^6*S + 4*a^8*c^2*S - 8*a^6*b^2*c^2*S - 
         12*a^4*b^4*c^2*S + 8*a^2*b^6*c^2*S - 8*a^6*c^4*S - 
         12*a^4*b^2*c^4*S - 16*a^2*b^4*c^4*S + 4*a^4*c^6*S + 8*a^2*b^2*c^6*S)/
        (b*c), b*c*(a^2 - b^2 + c^2)*(-a^6 - b^6 + 2*a^4*c^2 + 
         3*a^2*b^2*c^2 + 2*b^4*c^2 - a^2*c^4 - b^2*c^4 + 4*a^2*b^2*S), 
       b*c*(a^2 + b^2 - c^2)*(-a^6 + 2*a^4*b^2 - a^2*b^4 + 3*a^2*b^2*c^2 - 
         b^4*c^2 + 2*b^2*c^4 - c^6 + 4*a^2*c^2*S)}, 
     "Lucas(-1) reflection" -> {(-a^12 - 2*a^8*(b^2 - c^2)^2 - 
         b^2*c^2*(b^2 - c^2)^4 + 3*a^10*(b^2 + c^2) - a^2*(b^2 - c^2)*
          (b^4 - c^4)*(b^4 - 4*b^2*c^2 + c^4) - 2*a^6*(b^2 + c^2)*
          (b^4 + 5*b^2*c^2 + c^4) + a^4*(3*b^8 + 3*c^8 + 
           b^2*c^2*(b^4 - 12*b^2*c^2 + c^4)) - 
         4*(2*a^2*b^2*c^2*(b^2 - c^2)^2 + a^8*(b^2 + c^2) + 
           a^4*(b^2 + c^2)*(b^4 - 4*b^2*c^2 + c^4) - 
           2*a^6*(b^4 + b^2*c^2 + c^4))*S)/(b*c), b*c*(a^2 - b^2 + c^2)*
        (-a^6 + 2*a^4*c^2 - b^2*(b^2 - c^2)^2 + a^2*c^2*(3*b^2 - c^2) - 
         4*a^2*b^2*S), b*c*(a^2 + b^2 - c^2)*(-a^6 + 2*a^4*b^2 - 
         a^2*b^2*(b^2 - 3*c^2) - c^2*(b^2 - c^2)^2 - 4*a^2*c^2*S)}, 
     "Lucas secondary central" -> {a*(Cos[A] - 2*Sin[A]), 
       b*(Cos[B] + 4*Sin[B]), c*(Cos[C] + 4*Sin[C])}, 
     "Lucas(-1) secondary central" -> {a*(Cos[A] + 2*Sin[A]), 
       b*(Cos[B] - 4*Sin[B]), c*(Cos[C] - 4*Sin[C])}, 
     "Lucas 1st secondary tangents" -> {a^2*(a^2 - b^2 - c^2 + 4*S), 
       b^2*(-a^2 + b^2 - c^2 - 6*S), c^2*(-a^2 - b^2 + c^2 - 6*S)}, 
     "Lucas(-1) 1st secondary tangents" -> {a^2*(-a^2 + b^2 + c^2 + 4*S), 
       b^2*(a^2 - b^2 + c^2 - 6*S), c^2*(a^2 + b^2 - c^2 - 6*S)}, 
     "Lucas 2nd secondary tangents" -> {a^2*(-a^2 + b^2 + c^2 + 12*S), 
       b^2*(a^2 - b^2 + c^2 - 2*S), c^2*(a^2 + b^2 - c^2 - 2*S)}, 
     "Lucas(-1) 2nd secondary tangents" -> {a^2*(a^2 - b^2 - c^2 + 12*S), 
       b^2*(-a^2 + b^2 - c^2 - 2*S), c^2*(-a^2 - b^2 + c^2 - 2*S)}, 
     "Lucas tangents" -> {a*Cos[A], b*(Cos[B] + Sin[B]), 
       c*(Cos[C] + Sin[C])}, "Lucas(-1) tangents" -> 
      {a*Cos[A], b*(Cos[B] - Sin[B]), c*(Cos[C] - Sin[C])}, 
     "Macbeath" -> {0, 1/(b^2*(a^2 - b^2 + c^2)), 1/(c^2*(a^2 + b^2 - c^2))}, 
     "inner-Malfatti" -> {(a*(1 - Cos[A/2] + 2*Cos[B/2] + 2*Cos[C/2] + 
          2*Cos[B/2]*Cos[C/2]))/(1 + Cos[A/2]), b, c}, 
     "outer-Malfatti" -> {(a*(-1 - Cos[A/2] + 2*Cos[B/2] + 2*Cos[C/2] - 
          2*Cos[B/2]*Cos[C/2]))/(-1 + Cos[A/2]), b, c}, 
     "inner-Malfatti-touchpoints" -> {(a*(1 + Cos[B/2])*(1 + Cos[C/2]))/
        (1 + Cos[A/2]), b*(1 + Cos[C/2])^2, c*(1 + Cos[B/2])^2}, 
     "outer-Malfatti-touchpoints" -> {(a*(-1 + Cos[B/2])*(1 - Cos[C/2]))/
        (-1 + Cos[A/2]), b*(-1 + Cos[C/2])^2, c*(-1 + Cos[B/2])^2}, 
     "Mandart-excircles" -> {-((b - c)^2*(a + b + c)), b^2*(a + b - c), 
       c^2*(a - b + c)}, "Mandart-incircle" -> {-((a - b - c)*(b - c)^2), 
       b^2*(a - b + c), (a + b - c)*c^2}, 
     "McCay" -> {-(a^2*(a^2 + b^2 + c^2)), 2*a^4 - 2*a^2*b^2 + 2*b^4 - 
        3*a^2*c^2 - 3*b^2*c^2 + c^4, 2*a^4 - 3*a^2*b^2 + b^4 - 2*a^2*c^2 - 
        3*b^2*c^2 + 2*c^4}, "medial" -> {0, 1, 1}, 
     "midarc" -> {(a*b - b^2 + a*c + 2*b*c - c^2 + 4*b*c*Sin[A/2])/
        (-a + b + c), b, c}, "2nd midarc" -> 
      {(-(a*b) + b^2 - a*c - 2*b*c + c^2 + 4*b*c*Sin[A/2])/(-a + b + c), -b, 
       -c}, "midheight" -> {2*a^2, a^2 + b^2 - c^2, a^2 - b^2 + c^2}, 
     "mixtilinear" -> {(a^3 + a^2*b - a*b^2 - b^3 + a^2*c - 2*a*b*c + b^2*c - 
         a*c^2 + b*c^2 - c^3)/(4*b*c), -b, -c}, "2nd mixtilinear" -> 
      {(a^3 - a^2*b - a*b^2 + b^3 - a^2*c - 2*a*b*c - b^2*c - a*c^2 - b*c^2 + 
         c^3)/(4*b*c), b, c}, "3rd mixtilinear" -> {a/2, -(b^2/(a - b + c)), 
       -(c^2/(a + b - c))}, "4th mixtilinear" -> {a/2, -(b^2/(a + b - c)), 
       -(c^2/(a - b + c))}, "5th mixtilinear" -> {(a - b - c)/2, b, c}, 
     "6th mixtilinear" -> {a*(a^2 - 2*a*b + b^2 - 2*a*c - 2*b*c + c^2), 
       b*(a^2 - 2*a*b + b^2 + 2*a*c + 2*b*c - 3*c^2), 
       c*(a^2 + 2*a*b - 3*b^2 - 2*a*c + 2*b*c + c^2)}, 
     "7th mixtilinear" -> {((a + b - c)*(a - b + c)*(a^2 - 2*a*b + b^2 - 
          2*a*c - 2*b*c + c^2))/(2*(-a + b + c)), 
       b*(a^2 - 2*a*b + b^2 + 2*a*c + 2*b*c - 3*c^2), 
       c*(a^2 + 2*a*b - 3*b^2 - 2*a*c + 2*b*c + c^2)}, 
     "8th mixtilinear" -> {a*(a^2/2 - (3*(b - c)^2)/2 + a*(b + c)), 
       b^2*(a - b + c), (a + b - c)*c^2}, "9th mixtilinear" -> 
      {a*(a^2/2 - (3*(b - c)^2)/2 - a*(b + c)), b^2*(a + b - c), 
       c^2*(a - b + c)}, "Montesdeoca-Hung" -> 
      {a*(2*a^5*(a + 2*b + 2*c) + 4*a^3*(a + b + c)*(b^2 + 3*b*c + c^2) + 
         (b + c)^2*(b^4 + c^4 + 2*a*(b + c)*(b^2 + c^2)) + 
         a^2*(3*b^4 + 3*c^4 + 4*b*c*(3*b^2 + 5*b*c + 3*c^2))), 
       -(b*(a + c)^2*(a^2 + a*b + c*(b + c))^2), 
       -((a + b)^2*c*(a^2 + a*c + b*(b + c))^2)}, "1st Morley-midpoint" -> 
      {-1/4*(-(Sqrt[3]*a^4) + 2*Sqrt[3]*a^2*b^2 - Sqrt[3]*b^4 + 
          2*Sqrt[3]*a^2*c^2 + 2*Sqrt[3]*b^2*c^2 - Sqrt[3]*c^4 + 4*S*SA + 
          4*Sqrt[3]*SB*SC + 4*S*SW + 8*Sqrt[3]*a^2*b*c*Cos[A/3] + 
          16*b*c*S*Cos[A/3] + 8*a*c*S*Cos[B/3] + 8*Sqrt[3]*a*c*SA*Cos[B/3] + 
          16*Sqrt[3]*a*c*SC*Cos[B/3] + 16*Sqrt[3]*a*b*c^2*Cos[A/3]*Cos[B/3] + 
          8*a*b*S*Cos[C/3] + 8*Sqrt[3]*a*b*SA*Cos[C/3] + 16*Sqrt[3]*a*b*SB*
           Cos[C/3] + 16*Sqrt[3]*a*b^2*c*Cos[A/3]*Cos[C/3] + 
          32*Sqrt[3]*a^2*b*c*Cos[B/3]*Cos[C/3])/(S - Sqrt[3]*SA - 
          2*Sqrt[3]*b*c*Cos[A/3]), b*(b + 2*c*Cos[A/3] + 2*a*Cos[C/3]), 
       c*(c + 2*b*Cos[A/3] + 2*a*Cos[B/3])}, "2nd Morley-midpoint" -> 
      {-1/4*(Sqrt[3]*a^4 - 2*Sqrt[3]*a^2*b^2 + Sqrt[3]*b^4 - 
          2*Sqrt[3]*a^2*c^2 - 2*Sqrt[3]*b^2*c^2 + Sqrt[3]*c^4 - 4*S*SA - 
          4*Sqrt[3]*SB*SC - 4*S*SW - 8*Sqrt[3]*a^2*b*c*Sin[A/3 - Pi/6] - 
          16*b*c*S*Sin[A/3 - Pi/6] - 8*a*c*S*Sin[B/3 - Pi/6] - 
          8*Sqrt[3]*a*c*SA*Sin[B/3 - Pi/6] - 16*Sqrt[3]*a*c*SC*
           Sin[B/3 - Pi/6] - 16*Sqrt[3]*a*b*c^2*Sin[A/3 - Pi/6]*
           Sin[B/3 - Pi/6] - 8*a*b*S*Sin[C/3 - Pi/6] - 8*Sqrt[3]*a*b*SA*
           Sin[C/3 - Pi/6] - 16*Sqrt[3]*a*b*SB*Sin[C/3 - Pi/6] - 
          16*Sqrt[3]*a*b^2*c*Sin[A/3 - Pi/6]*Sin[C/3 - Pi/6] - 
          32*Sqrt[3]*a^2*b*c*Sin[B/3 - Pi/6]*Sin[C/3 - Pi/6])/
         (S - Sqrt[3]*SA - 2*Sqrt[3]*b*c*Sin[A/3 - Pi/6]), 
       b*(-b - 2*c*Sin[A/3 - Pi/6] - 2*a*Sin[C/3 - Pi/6]), 
       c*(-c - 2*b*Sin[A/3 - Pi/6] - 2*a*Sin[B/3 - Pi/6])}, 
     "3rd Morley-midpoint" -> {-1/4*(Sqrt[3]*a^4 - 2*Sqrt[3]*a^2*b^2 + 
          Sqrt[3]*b^4 - 2*Sqrt[3]*a^2*c^2 - 2*Sqrt[3]*b^2*c^2 + Sqrt[3]*c^4 - 
          4*S*SA - 4*Sqrt[3]*SB*SC - 4*S*SW + 8*Sqrt[3]*a^2*b*c*
           Sin[A/3 + Pi/6] + 16*b*c*S*Sin[A/3 + Pi/6] + 
          8*a*c*S*Sin[B/3 + Pi/6] + 8*Sqrt[3]*a*c*SA*Sin[B/3 + Pi/6] + 
          16*Sqrt[3]*a*c*SC*Sin[B/3 + Pi/6] - 16*Sqrt[3]*a*b*c^2*
           Sin[A/3 + Pi/6]*Sin[B/3 + Pi/6] + 8*a*b*S*Sin[C/3 + Pi/6] + 
          8*Sqrt[3]*a*b*SA*Sin[C/3 + Pi/6] + 16*Sqrt[3]*a*b*SB*
           Sin[C/3 + Pi/6] - 16*Sqrt[3]*a*b^2*c*Sin[A/3 + Pi/6]*
           Sin[C/3 + Pi/6] - 32*Sqrt[3]*a^2*b*c*Sin[B/3 + Pi/6]*
           Sin[C/3 + Pi/6])/(S - Sqrt[3]*SA + 2*Sqrt[3]*b*c*Sin[A/3 + Pi/6]), 
       b*(-b + 2*c*Sin[A/3 + Pi/6] + 2*a*Sin[C/3 + Pi/6]), 
       c*(-c + 2*b*Sin[A/3 + Pi/6] + 2*a*Sin[B/3 + Pi/6])}, 
     "1st Morley" -> {a/2, b*Cos[C/3], c*Cos[B/3]}, 
     "2nd Morley" -> {a/2, b*Sin[C/3 - Pi/6], c*Sin[B/3 - Pi/6]}, 
     "3rd Morley" -> {a/2, -(b*Sin[C/3 + Pi/6]), -(c*Sin[B/3 + Pi/6])}, 
     "1st Morley-adjunct midpoint" -> 
      {(Cos[A/3]*(2*Sqrt[3]*a^2*b*c*Cos[A/3] + Sqrt[3]*a*b^2*c*Cos[B/3] + 
          2*a*b*S*Cos[A/3]*Cos[B/3] + 2*Sqrt[3]*a*b*SA*Cos[A/3]*Cos[B/3] + 
          4*Sqrt[3]*a*b*SB*Cos[A/3]*Cos[B/3] + Sqrt[3]*a*b*c^2*Cos[C/3] + 
          2*a*c*S*Cos[A/3]*Cos[C/3] + 2*Sqrt[3]*a*c*SA*Cos[A/3]*Cos[C/3] + 
          4*Sqrt[3]*a*c*SC*Cos[A/3]*Cos[C/3] + 4*b*c*S*Cos[B/3]*Cos[C/3] + 
          2*Sqrt[3]*b*c*SB*Cos[B/3]*Cos[C/3] + 2*Sqrt[3]*b*c*SC*Cos[B/3]*
           Cos[C/3] - Sqrt[3]*a^4*Cos[A/3]*Cos[B/3]*Cos[C/3] + 
          2*Sqrt[3]*a^2*b^2*Cos[A/3]*Cos[B/3]*Cos[C/3] - Sqrt[3]*b^4*Cos[A/3]*
           Cos[B/3]*Cos[C/3] + 2*Sqrt[3]*a^2*c^2*Cos[A/3]*Cos[B/3]*Cos[C/3] + 
          2*Sqrt[3]*b^2*c^2*Cos[A/3]*Cos[B/3]*Cos[C/3] - Sqrt[3]*c^4*Cos[A/3]*
           Cos[B/3]*Cos[C/3] + 4*S*SA*Cos[A/3]*Cos[B/3]*Cos[C/3] + 
          4*Sqrt[3]*SB*SC*Cos[A/3]*Cos[B/3]*Cos[C/3] + 4*S*SW*Cos[A/3]*
           Cos[B/3]*Cos[C/3]))/(-(Sqrt[3]*b*c) + 2*S*Cos[A/3] - 
         2*Sqrt[3]*SA*Cos[A/3]), b*Cos[B/3]*(-(a*Cos[A/3]) - c*Cos[C/3] - 
         2*b*Cos[A/3]*Cos[C/3]), c*(-(a*Cos[A/3]) - b*Cos[B/3] - 
         2*c*Cos[A/3]*Cos[B/3])*Cos[C/3]}, "2nd Morley-adjunct midpoint" -> 
      {-((Sin[A/3 - Pi/6]*(-2*Sqrt[3]*a^2*b*c*Sin[A/3 - Pi/6] - 
           Sqrt[3]*a*b^2*c*Sin[B/3 - Pi/6] - 2*a*b*S*Sin[A/3 - Pi/6]*
            Sin[B/3 - Pi/6] - 2*Sqrt[3]*a*b*SA*Sin[A/3 - Pi/6]*
            Sin[B/3 - Pi/6] - 4*Sqrt[3]*a*b*SB*Sin[A/3 - Pi/6]*
            Sin[B/3 - Pi/6] - Sqrt[3]*a*b*c^2*Sin[C/3 - Pi/6] - 
           2*a*c*S*Sin[A/3 - Pi/6]*Sin[C/3 - Pi/6] - 2*Sqrt[3]*a*c*SA*
            Sin[A/3 - Pi/6]*Sin[C/3 - Pi/6] - 4*Sqrt[3]*a*c*SC*
            Sin[A/3 - Pi/6]*Sin[C/3 - Pi/6] - 4*b*c*S*Sin[B/3 - Pi/6]*
            Sin[C/3 - Pi/6] - 2*Sqrt[3]*b*c*SB*Sin[B/3 - Pi/6]*
            Sin[C/3 - Pi/6] - 2*Sqrt[3]*b*c*SC*Sin[B/3 - Pi/6]*
            Sin[C/3 - Pi/6] + Sqrt[3]*a^4*Sin[A/3 - Pi/6]*Sin[B/3 - Pi/6]*
            Sin[C/3 - Pi/6] - 2*Sqrt[3]*a^2*b^2*Sin[A/3 - Pi/6]*
            Sin[B/3 - Pi/6]*Sin[C/3 - Pi/6] + Sqrt[3]*b^4*Sin[A/3 - Pi/6]*
            Sin[B/3 - Pi/6]*Sin[C/3 - Pi/6] - 2*Sqrt[3]*a^2*c^2*
            Sin[A/3 - Pi/6]*Sin[B/3 - Pi/6]*Sin[C/3 - Pi/6] - 
           2*Sqrt[3]*b^2*c^2*Sin[A/3 - Pi/6]*Sin[B/3 - Pi/6]*
            Sin[C/3 - Pi/6] + Sqrt[3]*c^4*Sin[A/3 - Pi/6]*Sin[B/3 - Pi/6]*
            Sin[C/3 - Pi/6] - 4*S*SA*Sin[A/3 - Pi/6]*Sin[B/3 - Pi/6]*
            Sin[C/3 - Pi/6] - 4*Sqrt[3]*SB*SC*Sin[A/3 - Pi/6]*Sin[B/3 - Pi/6]*
            Sin[C/3 - Pi/6] - 4*S*SW*Sin[A/3 - Pi/6]*Sin[B/3 - Pi/6]*
            Sin[C/3 - Pi/6]))/(Sqrt[3]*b*c - 2*S*Sin[A/3 - Pi/6] + 
          2*Sqrt[3]*SA*Sin[A/3 - Pi/6])), 
       -(b*Sin[B/3 - Pi/6]*(-(a*Sin[A/3 - Pi/6]) - c*Sin[C/3 - Pi/6] - 
          2*b*Sin[A/3 - Pi/6]*Sin[C/3 - Pi/6])), 
       -(c*(-(a*Sin[A/3 - Pi/6]) - b*Sin[B/3 - Pi/6] - 2*c*Sin[A/3 - Pi/6]*
           Sin[B/3 - Pi/6])*Sin[C/3 - Pi/6])}, 
     "3rd Morley-adjunct midpoint" -> 
      {(Sin[A/3 + Pi/6]*(2*Sqrt[3]*a^2*b*c*Sin[A/3 + Pi/6] + 
          Sqrt[3]*a*b^2*c*Sin[B/3 + Pi/6] - 2*a*b*S*Sin[A/3 + Pi/6]*
           Sin[B/3 + Pi/6] - 2*Sqrt[3]*a*b*SA*Sin[A/3 + Pi/6]*
           Sin[B/3 + Pi/6] - 4*Sqrt[3]*a*b*SB*Sin[A/3 + Pi/6]*
           Sin[B/3 + Pi/6] + Sqrt[3]*a*b*c^2*Sin[C/3 + Pi/6] - 
          2*a*c*S*Sin[A/3 + Pi/6]*Sin[C/3 + Pi/6] - 2*Sqrt[3]*a*c*SA*
           Sin[A/3 + Pi/6]*Sin[C/3 + Pi/6] - 4*Sqrt[3]*a*c*SC*Sin[A/3 + Pi/6]*
           Sin[C/3 + Pi/6] - 4*b*c*S*Sin[B/3 + Pi/6]*Sin[C/3 + Pi/6] - 
          2*Sqrt[3]*b*c*SB*Sin[B/3 + Pi/6]*Sin[C/3 + Pi/6] - 
          2*Sqrt[3]*b*c*SC*Sin[B/3 + Pi/6]*Sin[C/3 + Pi/6] - 
          Sqrt[3]*a^4*Sin[A/3 + Pi/6]*Sin[B/3 + Pi/6]*Sin[C/3 + Pi/6] + 
          2*Sqrt[3]*a^2*b^2*Sin[A/3 + Pi/6]*Sin[B/3 + Pi/6]*Sin[C/3 + Pi/6] - 
          Sqrt[3]*b^4*Sin[A/3 + Pi/6]*Sin[B/3 + Pi/6]*Sin[C/3 + Pi/6] + 
          2*Sqrt[3]*a^2*c^2*Sin[A/3 + Pi/6]*Sin[B/3 + Pi/6]*Sin[C/3 + Pi/6] + 
          2*Sqrt[3]*b^2*c^2*Sin[A/3 + Pi/6]*Sin[B/3 + Pi/6]*Sin[C/3 + Pi/6] - 
          Sqrt[3]*c^4*Sin[A/3 + Pi/6]*Sin[B/3 + Pi/6]*Sin[C/3 + Pi/6] + 
          4*S*SA*Sin[A/3 + Pi/6]*Sin[B/3 + Pi/6]*Sin[C/3 + Pi/6] + 
          4*Sqrt[3]*SB*SC*Sin[A/3 + Pi/6]*Sin[B/3 + Pi/6]*Sin[C/3 + Pi/6] + 
          4*S*SW*Sin[A/3 + Pi/6]*Sin[B/3 + Pi/6]*Sin[C/3 + Pi/6]))/
        (Sqrt[3]*b*c + 2*S*Sin[A/3 + Pi/6] - 2*Sqrt[3]*SA*Sin[A/3 + Pi/6]), 
       b*Sin[B/3 + Pi/6]*(a*Sin[A/3 + Pi/6] + c*Sin[C/3 + Pi/6] - 
         2*b*Sin[A/3 + Pi/6]*Sin[C/3 + Pi/6]), 
       c*(a*Sin[A/3 + Pi/6] + b*Sin[B/3 + Pi/6] - 2*c*Sin[A/3 + Pi/6]*
          Sin[B/3 + Pi/6])*Sin[C/3 + Pi/6]}, "1st Morley-adjunct" -> 
      {2*a, b*Sec[C/3], c*Sec[B/3]}, "2nd Morley-adjunct" -> 
      {2*a, b*Csc[C/3 - Pi/6], c*Csc[B/3 - Pi/6]}, "3rd Morley-adjunct" -> 
      {2*a, -(b*Csc[C/3 + Pi/6]), -(c*Csc[B/3 + Pi/6])}, 
     "Moses-Hung" -> {-((2*a^3 + a^2*b + b^3 + a^2*c - b^2*c - b*c^2 + c^3)^2/
         (a + b + c)), (a + b - c)^3*(a + c)^2, (a + b)^2*(a - b + c)^3}, 
     "Moses-Soddy" -> {b - c, a - c, -a + b}, "Moses-Steiner osculatory" -> 
      {-((3*a^4 - 2*a^2*b^2 + b^4 - 2*a^2*c^2 - 2*b^2*c^2 + c^4)/a^2), 
       a^2 + b^2 - c^2, a^2 - b^2 + c^2}, "Moses-Steiner reflection" -> 
      {-a^2 + b^2 + c^2, a^2 - b^2 - 2*c^2, a^2 - 2*b^2 - c^2}, 
     "inner-Napoleon" -> {2*Sqrt[3]*a^2, -(Sqrt[3]*a^2) - Sqrt[3]*b^2 + 
        Sqrt[3]*c^2 + 6*S, -(Sqrt[3]*a^2) + Sqrt[3]*b^2 - Sqrt[3]*c^2 + 6*S}, 
     "outer-Napoleon" -> {-2*Sqrt[3]*a^2, Sqrt[3]*a^2 + Sqrt[3]*b^2 - 
        Sqrt[3]*c^2 + 6*S, Sqrt[3]*a^2 - Sqrt[3]*b^2 + Sqrt[3]*c^2 + 6*S}, 
     "1st Neuberg" -> {a^2*(a^2 + b^2 + c^2), -a^4 - b^4 + a^2*c^2 + b^2*c^2, 
       -a^4 + a^2*b^2 + b^2*c^2 - c^4}, "2nd Neuberg" -> 
      {a^2*(a^2 + b^2 + c^2), -2*a^2*b^2 - a^2*c^2 - b^2*c^2 + c^4, 
       -(a^2*b^2) + b^4 - 2*a^2*c^2 - b^2*c^2}, 
     "orthic" -> {0, (a^2 - b^2 + c^2)^(-1), (a^2 + b^2 - c^2)^(-1)}, 
     "orthic axes" -> {2/(-a^2 + b^2 + c^2), (a^2 - b^2 + c^2)^(-1), 
       (a^2 + b^2 - c^2)^(-1)}, "orthocentroidal" -> 
      {a^2, a^2 + b^2 - c^2, a^2 - b^2 + c^2}, "orthocentroidal-isogonic" -> 
      {(a^2 + b^2 - c^2)*(a^2 - b^2 + c^2), b^2*(a^2 - b^2 + c^2), 
       c^2*(a^2 + b^2 - c^2)}, "1st orthosymmedial" -> 
      {(2*a^4)/(b^2 + c^2), a^2 + b^2 - c^2, a^2 - b^2 + c^2}, 
     "2nd orthosymmedial" -> {(a^2*b^4 - b^6 + b^4*c^2 + a^2*c^4 + b^2*c^4 - 
         c^6)/((b^2 + c^2)*(-a^2 + b^2 + c^2)), b^2, c^2}, 
     "Paasche-Hutson" -> {-((2*a*R - S)*S), b*c*(a*b + S), b*c*(a*c + S)}, 
     "1st Pamfilos-Zhou" -> {a*(-a^6 + a^2*b^4 - a^4*b*c + 2*a^2*b^3*c - 
         b^5*c + 2*a^2*b^2*c^2 + 2*a^2*b*c^3 + 2*b^3*c^3 + a^2*c^4 - b*c^5 + 
         4*a*b^2*c*S + 4*a*b*c^2*S), b*(a^2 - b^2 + c^2)*
        (a^3*b + a*b^3 + a*b*c^2 + 2*c^2*S), c*(a^2 + b^2 - c^2)*
        (a^3*c + a*b^2*c + a*c^3 + 2*b^2*S)}, "2nd Pamfilos-Zhou" -> 
      {a*(a^2*b - b^3 + a^2*c - 4*a*b*c + b^2*c + b*c^2 - c^3 - 2*b*S - 
         2*c*S), -(a^2*b^2) + b^4 - a^3*c + 2*a^2*b*c + a*b^2*c + a^2*c^2 - 
        2*a*b*c^2 + a*c^3 - c^4 + 2*a^2*S - 2*a*c*S, -(a^3*b) + a^2*b^2 + 
        a*b^3 - b^4 + 2*a^2*b*c - 2*a*b^2*c - a^2*c^2 + a*b*c^2 + c^4 + 
        2*a^2*S - 2*a*b*S}, "1st Parry" -> 
      {3*a^4 - 2*a^2*b^2 + b^4 - 2*a^2*c^2 - b^2*c^2 + c^4, 
       -(b^2*(a^2 + b^2 - 2*c^2)), -(c^2*(a^2 - 2*b^2 + c^2))}, 
     "2nd Parry" -> {((b^2 - c^2)*(a^4 - b^4 + b^2*c^2 - c^4))/
        (2*a^2 - b^2 - c^2), b^2*(a^2 - b^2), c^2*(-a^2 + c^2)}, 
     "3rd Parry" -> {(a^2*(a^4 - 3*a^2*b^2 + 2*b^4 - 3*a^2*c^2 + b^2*c^2 + 
          2*c^4))/(b*c*(2*a^2 - b^2 - c^2)), b*c, b*c}, 
     "Pelletier" -> {a*(b - c)*(-a + b + c), b*(a - c)*(a - b + c), 
       (-a + b)*(a + b - c)*c}, "1st Przyby\[LSlash]owski-Bollin" -> 
      {a^2*(S + Sqrt[3]*SA), b*(Sqrt[3]*b*SB + 
         S*(b + 2*Sqrt[Sqrt[3]*S + SW])), 
       c*(Sqrt[3]*c*SC + S*(c + 2*Sqrt[Sqrt[3]*S + SW]))}, 
     "2nd Przyby\[LSlash]owski-Bollin" -> {a^2*(S + Sqrt[3]*SA), 
       b*(Sqrt[3]*b*SB + S*(b - 2*Sqrt[Sqrt[3]*S + SW])), 
       c*(Sqrt[3]*c*SC + S*(c - 2*Sqrt[Sqrt[3]*S + SW]))}, 
     "3rd Przyby\[LSlash]owski-Bollin" -> {a^2*(-S + Sqrt[3]*SA), 
       b*(-(b*S) + Sqrt[3]*b*SB - 2*S*Sqrt[-(Sqrt[3]*S) + SW]), 
       c*(-(c*S) + Sqrt[3]*c*SC - 2*S*Sqrt[-(Sqrt[3]*S) + SW])}, 
     "4th Przyby\[LSlash]owski-Bollin" -> {a^2*(-S + Sqrt[3]*SA), 
       b*(-(b*S) + Sqrt[3]*b*SB + 2*S*Sqrt[-(Sqrt[3]*S) + SW]), 
       c*(-(c*S) + Sqrt[3]*c*SC + 2*S*Sqrt[-(Sqrt[3]*S) + SW])}, 
     "reflection" -> {-a^2, a^2 + b^2 - c^2, a^2 - b^2 + c^2}, 
     "Roussel" -> {a*(-a^2 - 2*a*c*Cos[B/3] - 2*a*b*Cos[C/3] - 
         4*b*c*Cos[B/3]*Cos[C/3] + 16*b*c*Cos[A/3]^2*Cos[B/3]*Cos[C/3]), 
       b*(a*b + 2*a*c*Cos[A/3] + 4*c^2*Cos[A/3]*Cos[B/3] + 
         8*b*c*Cos[A/3]^2*Cos[B/3] + 2*b^2*Cos[C/3] + 4*b*c*Cos[A/3]*
          Cos[C/3]), c*(a*c + 2*a*b*Cos[A/3] + 2*c^2*Cos[B/3] + 
         4*b*c*Cos[A/3]*Cos[B/3] + 4*b^2*Cos[A/3]*Cos[C/3] + 
         8*b*c*Cos[A/3]^2*Cos[C/3])}, "1st Savin" -> 
      {2*a, -a + b + c, -a + b + c}, "2nd Savin" -> {2*a, 3*a + b + c, 
       3*a + b + c}, "1st Schiffler" -> 
      {1, (b*(a - c))/(-(a*b) - b^2 + (a - c)^2), 
       ((a - b)*c)/((a - b)^2 - a*c - c^2)}, "2nd Schiffler" -> 
      {1, (b*(-a + c))/(a^2 + a*b - b^2 - 2*a*c + c^2), 
       ((-a + b)*c)/(a^2 - 2*a*b + b^2 + a*c - c^2)}, 
     "Schroeter" -> {-b^2 + c^2, -a^2 + c^2, a^2 - b^2}, 
     "1st Sharygin" -> {a*(a^2 - b*c), b*(-(a*b) - c^2), c*(-b^2 - a*c)}, 
     "2nd Sharygin" -> {a*(a^2 - b*c), b*(-(a*b) + c^2), c*(b^2 - a*c)}, 
     "Soddy" -> {-(-a + b + c)^(-1), (a - b + c)^(-1), (a + b - c)^(-1)}, 
     "inner-Soddy" -> {(-a^2 + a*b + a*c + 2*S)/(-a + b + c), 
       (a*b - b^2 + b*c + S)/(a - b + c), (a*c + b*c - c^2 + S)/(a + b - c)}, 
     "2nd inner-Soddy" -> {(-a^2 + a*b + a*c + 2*S)/(-a + b + c), b, c}, 
     "outer-Soddy" -> {(a^2 - a*b - a*c + 2*S)/(-a + b + c), 
       (-(a*b) + b^2 - b*c + S)/(a - b + c), (-(a*c) - b*c + c^2 + S)/
        (a + b - c)}, "2nd outer-Soddy" -> 
      {(a^2 - a*b - a*c + 2*S)/(-a + b + c), -b, -c}, 
     "inner-squares" -> {a^2, (a^2 + b^2 - c^2 + 2*S)/2, 
       (a^2 - b^2 + c^2 + 2*S)/2}, "outer-squares" -> 
      {-2*a^2, -a^2 - b^2 + c^2 + 2*S, -a^2 + b^2 - c^2 + 2*S}, 
     "Stammler" -> {a*(Cos[A] - 2*Cos[(B - C)/3]), 
       b*(Cos[B] + 2*Cos[B/3 + (2*C)/3]), c*(2*Cos[(2*B)/3 + C/3] + Cos[C])}, 
     "Steiner" -> {0, -(-a^2 + c^2)^(-1), -(a^2 - b^2)^(-1)}, 
     "submedial" -> {2*a^2, 3*a^2 + b^2 - c^2, 3*a^2 - b^2 + c^2}, 
     "symmedial" -> {0, b/c, c/b}, "tangential" -> {-a^2, b^2, c^2}, 
     "tangential-midarc" -> {2*a*Sin[A/2], -a - b + c - 2*a*Sin[B/2], 
       -a + b - c - 2*a*Sin[C/2]}, "2nd tangential-midarc" -> 
      {-2*a*Sin[A/2], a + b - c - 2*a*Sin[B/2], a - b + c - 2*a*Sin[C/2]}, 
     "Thomson" -> {0, 0, 0}, "Thomson-anticomplementary" -> {0, 0, 0}, 
     "Thomson-excentral" -> {0, 0, 0}, "Thomson-medial" -> {0, 0, 0}, 
     "Thomson-orthic" -> {0, 0, 0}, "inner tri-equilateral" -> 
      {(a^2*(Sqrt[3]*a^2 - Sqrt[3]*b^2 - Sqrt[3]*c^2 + 6*S))/
        (-(Sqrt[3]*a^2) + Sqrt[3]*b^2 + Sqrt[3]*c^2 + 6*S), -b^2, -c^2}, 
     "outer tri-equilateral" -> 
      {(a^2*(-(Sqrt[3]*a^2) + Sqrt[3]*b^2 + Sqrt[3]*c^2 + 6*S))/
        (Sqrt[3]*a^2 - Sqrt[3]*b^2 - Sqrt[3]*c^2 + 6*S), -b^2, -c^2}, 
     "1st tri-squares-central" -> {2*(3*a^2 + 4*S), -a^2 + 3*b^2 + c^2 + 2*S, 
       -a^2 + b^2 + 3*c^2 + 2*S}, "2nd tri-squares-central" -> 
      {2*(-3*a^2 + 4*S), a^2 - 3*b^2 - c^2 + 2*S, a^2 - b^2 - 3*c^2 + 2*S}, 
     "3rd tri-squares-central" -> {a^2 + 2*S, b^2 + S, c^2 + S}, 
     "4th tri-squares-central" -> {-a^2 + 2*S, -b^2 + S, -c^2 + S}, 
     "1st tri-squares" -> {2*S, a^2 + 3*b^2 - c^2 + 2*S, 
       a^2 - b^2 + 3*c^2 + 2*S}, "2nd tri-squares" -> 
      {2*S, -a^2 - 3*b^2 + c^2 + 2*S, -a^2 + b^2 - 3*c^2 + 2*S}, 
     "3rd tri-squares" -> {(4*(a^2 + S))/(-a^2 + b^2 + c^2 + 2*S), 1, 1}, 
     "4th tri-squares" -> {(4*(-a^2 + S))/(a^2 - b^2 - c^2 + 2*S), 1, 1}, 
     "Trinh" -> {a^2*(a^4 - 2*a^2*b^2 + b^4 - 2*a^2*c^2 + 4*b^2*c^2 + c^4), 
       -(b^2*(a^4 - 2*a^2*b^2 + b^4 + a^2*c^2 + b^2*c^2 - 2*c^4)), 
       c^2*(-a^4 - a^2*b^2 + 2*b^4 + 2*a^2*c^2 - b^2*c^2 - c^4)}, 
     "Ursa-major" -> {a*(a^2*b - 2*a*b^2 + b^3 + a^2*c + b^2*c - 2*a*c^2 + 
         b*c^2 + c^3), b*(-a + c)*(a^2 - 2*a*b + b^2 + 2*a*c - c^2), 
       (a - b)*c*(-a^2 - 2*a*b + b^2 + 2*a*c - c^2)}, 
     "Ursa-minor" -> {a*(a*b - b^2 + a*c + 2*b*c - c^2), 
       b*(-a + c)*(a - b + c), (a - b)*c*(-a - b + c)}, 
     "inner-Vecten" -> {2*a^2, -a^2 - b^2 + c^2 + 2*S, 
       -a^2 + b^2 - c^2 + 2*S}, "2nd inner-Vecten" -> 
      {(-a^2 + b^2 + c^2)^(-1), (a^2 - b^2 + c^2 - 2*S)^(-1), 
       (a^2 + b^2 - c^2 - 2*S)^(-1)}, "3rd inner-Vecten" -> 
      {(2*(a^4 + 2*(b^2 - c^2)^2 - 3*a^2*(b^2 + c^2) + 
          (2*a^2 + 2*b^2 + 2*c^2)*S))/(-a^2 + b^2 + c^2), 
       -a^2 - b^2 + c^2 + 2*S, -a^2 + b^2 - c^2 + 2*S}, 
     "outer-Vecten" -> {-2*a^2, a^2 + b^2 - c^2 + 2*S, 
       a^2 - b^2 + c^2 + 2*S}, "2nd outer-Vecten" -> 
      {(-a^2 + b^2 + c^2)^(-1), (a^2 - b^2 + c^2 + 2*S)^(-1), 
       (a^2 + b^2 - c^2 + 2*S)^(-1)}, "3rd outer-Vecten" -> 
      {(-2*(-a^4 + 3*a^2*b^2 - 2*b^4 + 3*a^2*c^2 + 4*b^2*c^2 - 2*c^4 + 
          2*a^2*S + 2*b^2*S + 2*c^2*S))/(-a^2 + b^2 + c^2), 
       -a^2 - b^2 + c^2 - 2*S, -a^2 + b^2 - c^2 - 2*S}, 
     "Vijay polar excentral" -> {(-(a^2*(b - c)^2) - 4*a^3*(b + c) + 
         2*a*(b - c)*(b^2 - c^2) - (b^2 - c^2)^2)/(a + b + c), 
       a*b*(a - c) + b^2*(a + c) + c*(a^2 - c^2), b*(a^2 - b^2) + 
        a*(a - b)*c + (a + b)*c^2}, "Vijay polar incentral" -> 
      {(-b - c)*(2*a - b - c), (a*b*(a - 3*c) + b^2*(a + c) + c*(a^2 - c^2))/
        (a + b - c), (b*(a^2 - b^2) + a*(a - 3*b)*c + (a + b)*c^2)/
        (a - b + c)}, "1st Vijay" -> 
      {-1/2*((a + b + c)*(3*a^3 - a*(b - c)^2 + a^2*(b + c) + 
           (b - c)*(b^2 - c^2)))/(b + c), (a + b - c)*
        (b*c + c^2 + a*(2*b + c)), (a - b + c)*(b^2 + b*c + a*(b + 2*c))}, 
     "2nd Vijay" -> {(b*c*(a + b + c)*(3*a^3 - a*(b - c)^2 + a^2*(b + c) + 
          (b - c)*(b^2 - c^2)))/(a*(-a^2 + b^2 + c^2)), 
       (a + b - c)*(b*c + c^2 + a*(2*b + c)), (a - b + c)*
        (b^2 + b*c + a*(b + 2*c))}, "3rd Vijay" -> 
      {((a + b + c)*(3*a^3 - a*(b - c)^2 + a^2*(b + c) + 
          (b - c)*(b^2 - c^2)))/(4*a*(b + c)), 
       ((a + b)*(a - b + c)*(b^2 + b*c + a*(b + 2*c)))/(a^2 - b^2 + c^2), 
       ((a + b - c)*(a + c)*(b*c + c^2 + a*(2*b + c)))/(a^2 + b^2 - c^2)}, 
     "4th Vijay" -> {-1/2*(a^7 - a^5*(b - c)^2 - a^6*(b + c) + 
          a^4*(b - c)*(b^2 - c^2) - (b + c)^3*(b^4 - 2*b*(b - 2*c)*(2*b - c)*
             c + c^4) - a^3*(-4*b^2*c^2 + (b^2 - c^2)^2) + 
          a^2*(b + c)*(-4*b^2*c^2 + (b^2 - c^2)^2) + a*(b + c)^2*
           (b^4 + c^4 - 2*b*c*(2*b^2 + 3*b*c + 2*c^2)))/
         ((b + c)*(-a^2 + b^2 + c^2)), c*(a^3 - a^2*(b + c) + 
         a*(b^2 + 2*b*c - c^2) + (b + c)*(3*b^2 - 2*b*c + c^2)), 
       b*(a^3 - a^2*(b + c) - a*(b^2 - 2*b*c - c^2) + 
         (b + c)*(b^2 - 2*b*c + 3*c^2))}, "5th Vijay" -> 
      {(a + b + c)*(a^2 + b^2 - c^2)*(a^2 - b^2 + c^2), 
       (a^2 + b^2 - c^2)*(-((a + b - c)*c*(a + b + c)) + (2*a + 2*b)*S), 
       (a^2 - b^2 + c^2)*(-(b*(a - b + c)*(a + b + c)) + (2*a + 2*c)*S)}, 
     "6th Vijay" -> {(-a - b - c)*(a^2 + b^2 - c^2)*(a^2 - b^2 + c^2), 
       (a^2 + b^2 - c^2)*((a + b - c)*c*(a + b + c) + (2*a + 2*b)*S), 
       (a^2 - b^2 + c^2)*(b*(a - b + c)*(a + b + c) + (2*a + 2*c)*S)}, 
     "7th Vijay" -> {0, (a + b)*(a^2 + b^2 - c^2), 
       (a + c)*(a^2 - b^2 + c^2)}, "Vijay-Paasche-midpoints" -> 
      {(a*(b + 2*R)*(c + 2*R))/R, a*b - a*c + b*c + 4*b*R + 4*R^2, 
       -(a*b) + a*c + b*c + 4*c*R + 4*R^2}, "Vijay-Paasche-polar" -> 
      {-4*(2*a*R - S)^2, -a^4 + 2*a^2*b^2 - b^4 + 2*a^2*c^2 + 2*b^2*c^2 - 
        c^4 + 16*a*b*R^2 + 8*a*b*S + 8*a*R*S + 8*b*R*S + 32*R^2*S, 
       -a^4 + 2*a^2*b^2 - b^4 + 2*a^2*c^2 + 2*b^2*c^2 - c^4 + 16*a*c*R^2 + 
        8*a*c*S + 8*a*R*S + 8*c*R*S + 32*R^2*S}, 
     "Vijay-Paasche-reflection" -> 
      {-1/4*(-4*a^6 + 4*a^5*(b + c) + 5*b*c*(b^2 - c^2)^2 + 
          4*a*(b - c)*(b^2 - c^2)*(b^2 + c^2) + a^4*(8*b^2 - 3*b*c + 8*c^2) - 
          8*a^3*(b^3 + c^3) - 2*a^2*(2*b^4 + b^3*c + 2*b^2*c^2 + b*c^3 + 
            2*c^4) - 4*(-a + b + c)*(2*a*b*c + a^2*(b + c) - 
            (b - c)*(b^2 - c^2))*S)/(b*c), b*(a^3 - a^2*b + b*(b^2 - c^2) - 
         a*(b^2 - b*c + c^2) - c*S), c*(a^3 - a^2*c - c*(b^2 - c^2) - 
         a*(b^2 - b*c + c^2) - b*S)}, "Vu-Dao-X(15)-isodynamic" -> 
      {(2*(Sqrt[3]*a^6 + Sqrt[3]*a^4*b^2 - 2*Sqrt[3]*a^2*b^4 + 
          Sqrt[3]*a^4*c^2 + 4*Sqrt[3]*a^2*b^2*c^2 - 2*Sqrt[3]*a^2*c^4 - 
          6*a^4*S + 6*b^4*S - 12*b^2*c^2*S + 6*c^4*S))/
        (Sqrt[3]*(-a^2 + b^2 + c^2)), (Sqrt[3]*a^2 - 2*S)*
        (Sqrt[3]*a^2 + Sqrt[3]*b^2 - Sqrt[3]*c^2 - 2*S), 
       (Sqrt[3]*a^2 - 2*S)*(Sqrt[3]*a^2 - Sqrt[3]*b^2 + Sqrt[3]*c^2 - 2*S)}, 
     "Vu-Dao-X(16)-isodynamic" -> 
      {(2*(Sqrt[3]*a^6 + Sqrt[3]*a^4*b^2 - 2*Sqrt[3]*a^2*b^4 + 
          Sqrt[3]*a^4*c^2 + 4*Sqrt[3]*a^2*b^2*c^2 - 2*Sqrt[3]*a^2*c^4 + 
          6*a^4*S - 6*b^4*S + 12*b^2*c^2*S - 6*c^4*S))/
        (Sqrt[3]*(-a^2 + b^2 + c^2)), (Sqrt[3]*a^2 + 2*S)*
        (Sqrt[3]*a^2 + Sqrt[3]*b^2 - Sqrt[3]*c^2 + 2*S), 
       (Sqrt[3]*a^2 + 2*S)*(Sqrt[3]*a^2 - Sqrt[3]*b^2 + Sqrt[3]*c^2 + 2*S)}, 
     "Walsmith" -> {(-2*a^6 - (b^2 - c^2)*(b^4 - c^4) + 
         a^2*(3*b^4 - 4*b^2*c^2 + 3*c^4))/(2*a^4 - (b^2 - c^2)^2 - 
         a^2*(b^2 + c^2)), b^2, c^2}, "Wasat" -> {-b - c, a - c, a - b}, 
     "X-parabola-tangential" -> {-(b^2 - c^2)^2, (a^2 - c^2)^2, 
       (a^2 - b^2)^2}, "X3-ABC reflections" -> 
      {a^4 - 3*a^2*b^2 + 2*b^4 - 3*a^2*c^2 - 4*b^2*c^2 + 2*c^4, 
       b^2*(a^2 - b^2 + c^2), c^2*(a^2 + b^2 - c^2)}, 
     "Yff central" -> {2*a*Sin[A/2], a + b - c + 2*a*Sin[B/2], 
       a - b + c + 2*a*Sin[C/2]}, "Yff contact" -> {0, -(a - c)^(-1), 
       (a - b)^(-1)}, "inner-Yff" -> 
      {-1/2*(a^4 + (b^2 - c^2)^2 - 2*a^2*(b^2 + b*c + c^2))/(a*b*c), b, c}, 
     "outer-Yff" -> {(a^4 - 2*a^2*b^2 + b^4 + 2*a^2*b*c - 2*a^2*c^2 - 
         2*b^2*c^2 + c^4)/(2*a*b*c), b, c}, "inner-Yff tangents" -> 
      {-1/4*((-a + b + c)*(a^3 + a^2*b - a*b^2 - b^3 + a^2*c + b^2*c - 
           a*c^2 + b*c^2 - c^3))/(a*b*c), -b, -c}, "outer-Yff tangents" -> 
      {((a - b - c)*(a^3 + a^2*(b + c) - (b - c)*(b^2 - c^2) + 
          a*(-b^2 + 4*b*c - c^2)))/(4*a*b*c), b, c}, 
     "Yiu" -> {(a^8 - 4*a^6*b^2 + 6*a^4*b^4 - 4*a^2*b^6 + b^8 - 4*a^6*c^2 + 
         5*a^4*b^2*c^2 + a^2*b^4*c^2 - 2*b^6*c^2 + 6*a^4*c^4 + a^2*b^2*c^4 + 
         2*b^4*c^4 - 4*a^2*c^6 - 2*b^2*c^6 + c^8)/(b*c), 
       b*c*(a^4 - a^2*b^2 - 2*a^2*c^2 - b^2*c^2 + c^4), 
       b*c*(a^4 - 2*a^2*b^2 + b^4 - a^2*c^2 - b^2*c^2)}, 
     "Yiu tangents" -> {-1, (a^2 + 2*b^2 - c^2)/(2*a^2 + b^2 - c^2), 
       (a^2 - b^2 + 2*c^2)/(2*a^2 - b^2 + c^2)}, 
     "1st Zaniah" -> {2*a, a + b - c, a - b + c}, 
     "2nd Zaniah" -> {2*a, a - b + c, a + b - c}, 
     "Moses-Miyamoto"->{-2 a^3+3 a^2 (b+c)-(b-c)^2 (b+c),b (a^2+(b-c)^2-2 a (b+c)),c (a^2+(b-c)^2-2 a (b+c))},
     "1st Moses-Miyamoto-Apollonius triangle"->{-2 a (a+b-c) (a-b+c),(a+b-c) (a^2-b^2+2 b c-c^2-2 S),(a-b+c) (a^2-b^2+2 b c-c^2-2 S)},
     "2nd Moses-Miyamoto-Apollonius triangle"->{-2 a (a+b-c) (a-b+c),(a+b-c) (a^2-b^2+2 b c-c^2+2 S),(a-b+c) (a^2-b^2+2 b c-c^2+2 S)},
     "anti-excenters-incenter reflections"->{(2 a^2)/(S^2-2 SB SC),1/SB,1/SC},
     "Gemini 1" -> {a, b + c, b + c}, "Gemini 2" -> {-a, b + c, b + c}, 
     "Gemini 3" -> {a, a + b, a + c}, "Gemini 4" -> {-a, a + b, a + c}, 
     "Gemini 5" -> {a, a - b, a - c}, "Gemini 6" -> {a, -a + b, -a + c}, 
     "Gemini 7" -> {a, a - c, a - b}, "Gemini 8" -> {a, -a + c, -a + b}, 
     "Gemini 9" -> {a, -a + b + c, -a + b + c}, 
     "Gemini 10" -> {-a, -a + b + c, -a + b + c}, 
     "Gemini 11" -> {a, a + b + c, a + b + c}, "Gemini 12" -> 
      {-a, a + b + c, a + b + c}, "Gemini 13" -> {b + c, a, a}, 
     "Gemini 14" -> {b + c, 2*a, 2*a}, "Gemini 15" -> {b + c, b, c}, 
     "Gemini 16" -> {b + c, c, b}, "Gemini 17" -> {b + c, b - c, -b + c}, 
     "Gemini 18" -> {b + c, -b + c, b - c}, "Gemini 19" -> 
      {b + c, a + b, a + c}, "Gemini 20" -> {2*b + 2*c, a, a}, 
     "Gemini 21" -> {a + b + c, a, a}, "Gemini 22" -> {a + b + c, -a, -a}, 
     "Gemini 23" -> {a + b + c, b + c, b + c}, "Gemini 24" -> 
      {a + b + c, -b - c, -b - c}, "Gemini 25" -> {a + b + c, a + b, a + c}, 
     "Gemini 26" -> {a + b + c, a + c, a + b}, "Gemini 27" -> 
      {a - b - c, a, a}, "Gemini 28" -> {a - b - c, b + c, b + c}, 
     "Gemini 29" -> {a, b - c, -b + c}, "Gemini 30" -> {a, -b + c, b - c}, 
     "Gemini 31" -> {b*c, a^2, a^2}, "Gemini 32" -> {-(b*c), a^2, a^2}, 
     "Gemini 33" -> {a^2, b*c, b*c}, "Gemini 34" -> {-a^2, b*c, b*c}, 
     "Gemini 35" -> {Cos[A], 1, 1}, "Gemini 36" -> {-Cos[A], 1, 1}, 
     "Gemini 37" -> {Sec[A], 1, 1}, "Gemini 38" -> {-Sec[A], 1, 1}, 
     "Gemini 39" -> {-a + b + c, a + b + c, a + b + c}, 
     "Gemini 40" -> {a + b + c, -a + b + c, -a + b + c}, 
     "Gemini 41" -> {b^2 + c^2, a^2, a^2}, "Gemini 42" -> 
      {a^2 + b^2 + c^2, a^2, a^2}, "Gemini 43" -> {a^2, b^2 + c^2, 
       b^2 + c^2}, "Gemini 44" -> {-a^2, b^2 + c^2, b^2 + c^2}, 
     "Gemini 45" -> {(b - c)^2, a^2, a^2}, "Gemini 46" -> 
      {(b + c)^2, a^2, a^2}, "Gemini 47" -> {a^2, (b + c)^2, (b + c)^2}, 
     "Gemini 48" -> {a^2, (b - c)^2, (b - c)^2}, 
     "Gemini 49" -> {(b + c)^2, (b - c)^2, (b - c)^2}, 
     "Gemini 50" -> {(b - c)^2, (b + c)^2, (b + c)^2}, 
     "Gemini 51" -> {(b - c)^2, b^2 + c^2, b^2 + c^2}, 
     "Gemini 52" -> {(b + c)^2, b^2 + c^2, b^2 + c^2}, 
     "Gemini 53" -> {b^2 + c^2, (b - c)^2, (b - c)^2}, 
     "Gemini 54" -> {b^2 + c^2, (b + c)^2, (b + c)^2}, 
     "Gemini 55" -> {a^2, 2*b*c, 2*b*c}, "Gemini 56" -> {-a^2, 2*b*c, 2*b*c}, 
     "Gemini 57" -> {b^2 + c^2, b*c, b*c}, "Gemini 58" -> 
      {b^2 + c^2, -(b*c), -(b*c)}, "Gemini 59" -> {a*b + a*c - b*c, 
       a*b + a*c + b*c, a*b + a*c + b*c}, "Gemini 60" -> 
      {a*b + a*c + b*c, a*b + a*c - b*c, a*b + a*c - b*c}, 
     "Gemini 61" -> {a*(a - b - c)*(a + b + c), (a + b - c)*(a - b + c)*
        (b + c), (a + b - c)*(a - b + c)*(b + c)}, 
     "Gemini 62" -> {(a*b - a*c + b*c)*(-(a*b) + a*c + b*c)*
        (b^2 + b*c + c^2), (a*b - a*c - b*c)*(a*b + a*c - b*c)*(a*b + c^2), 
       (b^2 + a*c)*(-(a*b) + a*c - b*c)*(a*b + a*c - b*c)}, 
     "Gemini 63" -> {-b^2 - b*c - c^2, a*b + 2*a*c + 2*b*c + c^2, 
       2*a*b + b^2 + a*c + 2*b*c}, "Gemini 64" -> 
      {(a*b - a*c - b*c)*(a*b - a*c + b*c)*(b^2 - b*c + c^2), 
       (a*b + a*c - b*c)*(-(a*b) + a*c + b*c)*(a*b - 2*b*c + c^2), 
       (b^2 + a*c - 2*b*c)*(a*b + a*c - b*c)*(a*b - a*c + b*c)}, 
     "Gemini 65" -> {(a - b - c)*(a + b + c), (a + b - c)*(a - b + c), 
       (a + b - c)*(a - b + c)}, "Gemini 66" -> 
      {(b + c)*(a^2 + 2*a*b + b^2 + 2*a*c + b*c + c^2), -(a*(a + b)*(a + c)), 
       -(a*(a + b)*(a + c))}, "Gemini 67" -> 
      {(2*a + b + c)*(a^2 + 2*a*b + b^2 + 2*a*c + b*c + c^2), 
       a*(a + 2*b + c)*(a + b + 2*c), a*(a + 2*b + c)*(a + b + 2*c)}, 
     "Gemini 68" -> {a*(a*b + b^2 + a*c + b*c + c^2), -(b*c*(b + c)), 
       -(b*c*(b + c))}, "Gemini 69" -> {(b + c)*(-a^2 + b^2 - b*c + c^2), 
       -(a*(a + b)*(a + c)), -(a*(a + b)*(a + c))}, 
     "Gemini 70" -> {(a - 2*b - 2*c)*(a*b + b^2 + a*c - b*c + c^2), 
       (2*a + 2*b - c)*(b + c)*(2*a - b + 2*c), (2*a + 2*b - c)*(b + c)*
        (2*a - b + 2*c)}, "Gemini 71" -> {a*(a + b - 3*c)*(a - 3*b + c), 
       (3*a - b - c)*(b - c)*(-a - b + 3*c), (3*a - b - c)*(b - c)*
        (a - 3*b + c)}, "Gemini 72" -> {b*c*(a^2 - b*c)^2, 
       a^2*(b^2 - a*c)*(-(a*b) + c^2), a^2*(b^2 - a*c)*(-(a*b) + c^2)}, 
     "Gemini 73" -> {b*c*(a^4 - b^2*c^2), a^2*(b^2 + a*c)*(a*b + c^2), 
       a^2*(b^2 + a*c)*(a*b + c^2)}, "Gemini 74" -> 
      {(a^2 - b*c)^2, (b^2 - a*c)*(-(a*b) + c^2), 
       (b^2 - a*c)*(-(a*b) + c^2)}, "Gemini 75" -> 
      {a^4 - b^2*c^2, (-b^2 - a*c)*(a*b + c^2), (-b^2 - a*c)*(a*b + c^2)}, 
     "Gemini 76" -> {(a + b + c)*(a^2 + b^2 - 2*b*c + c^2), 
       2*b*(a - b - c)*c, 2*b*(a - b - c)*c}, "Gemini 77" -> 
      {(a - b - c)^2*(a^2 + b^2 - 2*b*c + c^2), -2*b*(a + b - c)*c*
        (a - b + c), -2*b*(a + b - c)*c*(a - b + c)}, 
     "Gemini 78" -> {(a + b + c)*(a^2 + b^2 - 2*b*c + c^2), 
       (-a + b + c)*(a^2 - b^2 - c^2), (-a + b + c)*(a^2 - b^2 - c^2)}, 
     "Gemini 79" -> {(a - b - c)^2*(a^2 + b^2 - 2*b*c + c^2), 
       (a + b - c)*(a - b + c)*(-a^2 + b^2 + c^2), (a + b - c)*(a - b + c)*
        (-a^2 + b^2 + c^2)}, "Gemini 80" -> {a*(a*b + b^2 + a*c + c^2), 
       -(b*c*(a + b + c)), -(b*c*(a + b + c))}, 
     "Gemini 81" -> {a*(a*b + b^2 + a*c + c^2), b*(a - b - c)*c, 
       b*(a - b - c)*c}, "Gemini 82" -> {(a^2 - b^2 - c^2)*(a^2 + b^2 + c^2), 
       (a^2 + b^2 - c^2)*(a^2 - b^2 + c^2), (a^2 + b^2 - c^2)*
        (a^2 - b^2 + c^2)}, "Gemini 83" -> 
      {(b^2 + c^2)*(a^2 + b^2 - b*c + c^2)*(a^2 + b^2 + b*c + c^2), 
       -(a^2*(a^2 + b^2)*(a^2 + c^2)), -(a^2*(a^2 + b^2)*(a^2 + c^2))}, 
     "Gemini 84" -> {a^2*(a^2 - b^2 - c^2)*(a^2 + b^2 + c^2), 
       (a^2 + b^2 - c^2)*(a^2 - b^2 + c^2)*(b^2 + c^2), 
       (a^2 + b^2 - c^2)*(a^2 - b^2 + c^2)*(b^2 + c^2)}, 
     "Gemini 85" -> {a^2 - a*b - a*c + 2*b*c, a*(a - b - c), a*(a - b - c)}, 
     "Gemini 86" -> {(a - b - c)*(a^2 + a*b + a*c + 2*b*c), 
       a*(a + b - c)*(a - b + c), a*(a + b - c)*(a - b + c)}, 
     "Gemini 87" -> {a*(a - b - c)*(a^2 + a*b + a*c + 2*b*c), 
       (a + b - c)*(a - b + c)*(b + c)^2, (a + b - c)*(a - b + c)*(b + c)^2}, 
     "Gemini 88" -> {a*(a^2 - a*b - a*c + 2*b*c), (a - b - c)*(b - c)^2, 
       (a - b - c)*(b - c)^2}, "Gemini 89" -> {(b + c)*(a^2 + b*c), 
       -(a*(b - c)^2), -(a*(b - c)^2)}, "Gemini 90" -> 
      {a^2 + b*c, -(a*b) - a*c, -(a*b) - a*c}, "Gemini 91" -> 
      {a^2*b + a^2*c - 2*a*b*c + b^2*c + b*c^2, -(a*(b^2 + c^2)), 
       -(a*(b^2 + c^2))}, "Gemini 92" -> {a^2*b + a^2*c + 2*a*b*c + b^2*c + 
        b*c^2, -(a*(b^2 + c^2)), -(a*(b^2 + c^2))}, 
     "Gemini 93" -> {a^2*b + a^2*c - 2*a*b*c + b^2*c + b*c^2, -(a*(b - c)^2), 
       -(a*(b - c)^2)}, "Gemini 94" -> {a^2*b + a^2*c + 2*a*b*c + b^2*c + 
        b*c^2, -(a*(b + c)^2), -(a*(b + c)^2)}, 
     "Gemini 95" -> {(a^2 - 2*b*c)*(4*a^2 - b*c), 2*(b^2 - 2*a*c)*
        (-2*a*b + c^2), 2*(b^2 - 2*a*c)*(-2*a*b + c^2)}, 
     "Gemini 96" -> {(4*a^2 - b*c)*(a^2 + 2*b*c), -2*(b^2 + 2*a*c)*
        (2*a*b + c^2), -2*(b^2 + 2*a*c)*(2*a*b + c^2)}, 
     "Gemini 97" -> {(b^2 - b*c + c^2)*(a^4 + a^2*b^2 - a^2*b*c + a^2*c^2 + 
         b^2*c^2), -(b*(a^2 - a*b + b^2)*c*(a^2 - a*c + c^2)), 
       -(b*(a^2 - a*b + b^2)*c*(a^2 - a*c + c^2))}, 
     "Gemini 98" -> {(b^2 + b*c + c^2)*(a^4 + a^2*b^2 - a^2*b*c + a^2*c^2 + 
         b^2*c^2), b*(a^2 + a*b + b^2)*c*(a^2 + a*c + c^2), 
       b*(a^2 + a*b + b^2)*c*(a^2 + a*c + c^2)}, 
     "Gemini 99" -> {a*b^2 + b^2*c + a*c^2 + b*c^2, -(a*(a*b + a*c + b*c)), 
       -(a*(a*b + a*c + b*c))}, "Gemini 100" -> 
      {a*b^2 + b^2*c + a*c^2 + b*c^2, a*(-(a*b) - a*c + b*c), 
       a*(-(a*b) - a*c + b*c)}, "Gemini 101" -> {a^2 - b^2 - c^2, 2*a^2, 
       2*a^2}, "Gemini 102" -> {a - b - c, 2*a, 2*a}, 
     "Gemini 103" -> {a^3 - b^3 - c^3, 2*a^3, 2*a^3}, 
     "Gemini 104" -> {-(b*c), a*(b + c), a*(b + c)}, 
     "Gemini 105" -> {-b - c, 2*a + b + c, 2*a + b + c}, 
     "Gemini 106" -> {-b^2 - c^2, 2*a^2 + b^2 + c^2, 2*a^2 + b^2 + c^2}, 
     "Gemini 107" -> {-1, 2, 2}, "Gemini 108" -> 
      {3*a - b - c, 2*a - 2*b - 2*c, 2*a - 2*b - 2*c}, 
     "Gemini 109" -> {1, 2, 2}, "Gemini 110" -> {2, 1, 1}, 
     "Gemini 111" -> {-3, 1, 1},
     "1st Vijay-Paasche-Hutson"->{-b c+4 R^2,b (c+2 R),c (b+2 R)},
     "2nd Vijay-Paasche-Hutson"->{8 R^3-a S,-b (a c-4 R^2),-c (a b-4 R^2)},
     "3rd Vijay-Paasche-Hutson" ->{-a (b c-4 R^2),4 R^2 (b+2 R),4 R^2 (c+2 R)},
     "4th Vijay-Paasche-Hutson"->{a (2 a b c+(b+c) S),S (a b+S),S (a c+S)},
     "5th Vijay-Paasche-Hutson"->{2 a^6 b c+2 b c (b^2-c^2)^2 (b c+S)+a^5 (b+c) (4 b c+S)-2 a^3 (b+c) (b^2+c^2) (4 b c+S)+2 a^4 b c (-2 b^2+b c-2 c^2+S)+a (b+c) (4 b c (b^2-c^2)^2+(b^4-18 b^2 c^2+c^4) S)+2 a^2 b c (b^4-2 b^3 c+c^4-2 c^2 S-2 b^2 (9 c^2+S)-2 b (c^3+8 c S)),-4 b S (2 b c+S) (2 a b c+(a+c) S),-4 c S (2 b c+S) (2 a b c+(a+b) S)},
     "6th Vijay-Paasche-Hutson"->{a S (2 a b c+(b+c) S),(2 a b+S) (a c+S) (2 b c+S),(a b+S) (2 a c+S) (2 b c+S)},
     "7th Vijay-Paasche-Hutson"->{S (a^2 b c-S^2),a b (a c+S) (2 b c+S),a c (a b+S) (2 b c+S)},
     "8th Vijay-Paasche-Hutson" ->{4 R (c+3 R)+b (c+4 R),b (c+2 R),c (b+2 R)},
     "9th Vijay-Paasche-Hutson"->{a (2 a b c+(b+c) S),-(a b+S) (2 b c+S),-(a c+S) (2 b c+S)},
     "10th Vijay-Paasche-Hutson"->{-a^8+4 a^6 (b^2+c^2)-(b^4-c^4)^2-a^4 (6 b^4+8 b^2 c^2+6 c^4)+4 a^2 (b^6+b^4 c^2+16 b^3 c^3+b^2 c^4+c^6)+32 a b^2 c^2 (b+c) S,4 b c S (a^4+b^4+8 a b^2 c-2 b^2 c^2+c^4-2 a^2 (b^2+c^2)+4 b c S),4 b c S (a^4+b^4+8 a b c^2-2 b^2 c^2+c^4-2 a^2 (b^2+c^2)+4 b c S)}
     |>;
