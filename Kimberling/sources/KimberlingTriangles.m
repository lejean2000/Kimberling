KimberlingTrianglesTrilinear := Association["Yff central" -> 
      {2*Sin[angleA/2], (2*a*Sin[angleB/2] + a + b - c)/b, 
       (2*a*Sin[angleC/2] + a - b + c)/c}, "Yff contact" -> 
      {0, -b^(-1)/(a - c), 1/c/(a - b)}, "inner-Yff" -> 
      {-(a^4 - 2*(b^2 + b*c + c^2)*a^2 + (b^2 - c^2)^2)/(2*a^2*b*c), 1, 1}, 
     "outer-Yff" -> {(a^4 - 2*(b^2 - b*c + c^2)*a^2 + (b^2 - c^2)^2)/
        (2*a^2*b*c), 1, 1}, "inner-Yff tangents" -> 
      {(b + c - a)*((a^3 + (b + c)*a^2 - (b^2 + c^2)*a - (b^2 - c^2)*(b - c))/
         (4*a^2*b*c)), 1, 1}, "outer-Yff tangents" -> 
      -{(b + c - a)*((a^3 + (b + c)*a^2 - (b^2 - 4*b*c + c^2)*a - 
           (b^2 - c^2)*(b - c))/(4*a^2*b*c)), 1, 1}, 
     "Yiu" -> {2*S*((SA^2 - R^2*SA - S^2)/R), c*(S^2 + SA*SC), 
       b*(S^2 + SA*SB)}, "Yiu tangents" -> -{(SB + 3*SC)*((SC + 3*SB)/a), 
        (SA + 3*SC)*((SC + 3*SB)/b), (SA + 3*SB)*((SB + 3*SC)/c)}, 
     "1st Zaniah triangle" -> {2, (a + b - c)/b, (a - b + c)/c}, 
     "2nd Zaniah triangle" -> {2, (a - b + c)/b, (a + b - c)/c}, 
     "Walsmith" -> {-(2*a^6 - (3*b^4 - 4*b^2*c^2 + 3*c^4)*a^2 + 
           (b^4 - c^4)*(b^2 - c^2))/(2*a^4 - (b^2 + c^2)*a^2 - (b^2 - c^2)^2)/
        a, b, c}, "Wasat" -> {(b + c)/a, (c - a)/b, (b - a)/c}, 
     "X-ABC reflections" -> {-(3*S^2 + SB*SC)/a, SB*b, SC*c}, 
     "X-parabola-tangential" -> {-(b^2 - c^2)^2/a, (a^2 - c^2)^2/b, 
       (a^2 - b^2)^2/c}, "Vijay-Paasche-midpoints" -> 
      {(2*R + b)*((2*R + c)/R), (4*R*(R + b) + b*c + (b - c)*a)/b, 
       (4*R*(R + c) + c*b + (c - b)*a)/c}, "Vijay-Paasche-polar" -> 
      {-(-2*a*R + S)^2/a, (S^2 + 2*R*(a + b + 4*R)*S + 2*a*b*(2*R^2 + S))/b, 
       (S^2 + 2*R*(a + c + 4*R)*S + 2*a*c*(2*R^2 + S))/c}, 
     "Vijay-Paasche-tangents" -> {-(-2*a*R + S)^2/a, 
       (S^2 + 2*R*(a + b + 4*R)*S + 2*a*b*(2*R^2 + S))/b, 
       (S^2 + 2*R*(a + c + 4*R)*S + 2*a*c*(2*R^2 + S))/c}, 
     "outer-Vecten" -> {-a, (SC + S)/b, (SB + S)/c}, 
     "Trinh" -> {(-a)*(S^2 + 3*SA^2), b*(S^2 - 3*SA*SB), c*(S^2 - 3*SA*SC)}, 
     "Ursa-major" -> {(-(b + c))*a^2 + 2*(b^2 + c^2)*a - (b + c)*(b^2 + c^2), 
       ((a - b)^2 + (2*a - c)*c)*(a - c), ((a - c)^2 + (2*a - b)*b)*(a - b)}, 
     "Ursa-minor" -> {(-(b + c))*a - (b - c)^2, (a - b + c)*(a - c), 
       (a + b - c)*(a - b)}, "inner-Vecten" -> {-a, (SC - S)/b, (SB - S)/c}, 
     "tangential" -> {-a, b, c}, "(1st) tangential-midarc" -> 
      {-2*b*c*Sin[angleA/2], c*(2*Sin[angleB/2]*a + a + b - c), 
       b*(2*Sin[angleC/2]*a + a + c - b)}, "2nd tangential-midarc" -> 
      {2*b*c*Sin[angleA/2], c*(2*Sin[angleB/2]*a - (a + b - c)), 
       b*(2*Sin[angleC/2]*a - (a + c - b))}, "inner tri-equilateral" -> 
      {a*((SA - Sqrt[3]*S)/(SA + Sqrt[3]*S)), b, c}, 
     "outer tri-equilateral" -> {a*((SA + Sqrt[3]*S)/(SA - Sqrt[3]*S)), b, 
       c}, "AAOA" -> {(a^8 + 3*a^4*b^2*c^2 - 2*(b^2 + c^2)*a^6 + 
         2*(b^4 - c^4)*(b^2 - c^2)*a^2 - (b^4 - c^4)^2)/(a*b*c), 
       ((-b^2)*(a^2 - b^2) - c^4 + a^4)*c, ((-c^2)*(a^2 - c^2) - b^4 + a^4)*
        b}, "ABC-X reflections" -> {-a^(-1), b/SC, c/SB}, 
     "AOA" -> {((b^2 + c^2)*a^6 - (b^4 + c^4)*a^4 - (b^4 - c^4)*(b^2 - c^2)*
          a^2 + (b^4 - c^4)^2)/a, (b^8 - 3*b^6*a^2 - 
         (3*c^4 - 4*c^2*a^2 - a^4)*b^4 + (c^2 - a^2)*(7*c^2 - 3*a^2)*b^2*
          a^2 + 2*(c^4 - a^4)*(c^2 - a^2)^2)/b, 
       (c^8 - 3*c^6*a^2 - (3*b^4 - 4*b^2*a^2 - a^4)*c^4 + 
         (b^2 - a^2)*(7*b^2 - 3*a^2)*c^2*a^2 + 2*(b^4 - a^4)*(b^2 - a^2)^2)/
        c}, "Andromeda" -> {(a^2 + 3*(b - c)^2)/(3*a^2 + (b - c)^2), 1, 1}, 
     "anticomplementary" -> {-a^(-1), 1/b, 1/c}, "anti-Aquila" -> 
      {(2*a + b + c)/a, 1, 1}, "anti-Ara" -> {(2*SA + SB + SC)/(a*SA), b/SB, 
       c/SC}, "anti-Ascella" -> {-a, b*((SA + 2*SB)/SB), c*((SA + 2*SC)/SC)}, 
     "anti-Atik" -> {S^2*((4*R^2 - SA)/(a*SA^2)), SB/b, SC/c}, 
     "1st anti-Brocard" -> {(a^4 - b^2*c^2)/a, (c^4 - a^2*b^2)/b, 
       (b^4 - a^2*c^2)/c}, "4th anti-Brocard" -> 
      {a*(((a^2 + b^2 + c^2)^2 - 9*b^2*c^2)/(-5*a^2 + b^2 + c^2)), 
       b*(a^2 + b^2 - 2*c^2), c*(a^2 - 2*b^2 + c^2)}, 
     "5th anti-Brocard" -> {(a^2 + c^2)*((a^2 + b^2)/a), b^3, c^3}, 
     "6th anti-Brocard" -> {a*(a^4 - b^2*c^2), 
       (b^6 - (a^2 + c^2)*b^4 - (a^2 - c^2)*b^2*c^2 + a^2*c^4)/b, 
       (c^6 - (a^2 + b^2)*c^4 - (a^2 - b^2)*b^2*c^2 + a^2*b^4)/c}, 
     "1st anti-circumperp" -> {-a, (b^2 - c^2)/b, (c^2 - b^2)/c}, 
     "2nd anti-circumperp-tangential" -> {(b + c)^2/(a*(-a + b + c)), 
       b/(a - b + c), c/(a + b - c)}, "(1st) anti-Conway" -> 
      {a^3*((-a^2 + b^2 + c^2)/((b^2 + c^2)*a^2 - (b^2 - c^2)^2)), b, c}, 
     "2nd anti-Conway" -> {((b^2 + c^2)*a^2 - (b^2 - c^2)^2)/a/
        (-a^2 + b^2 + c^2), b, c}, "anti-Ehrmann-mid" -> 
      {-3*a*SA, (S^2 + 3*SA*SC)/b, (S^2 + 3*SA*SB)/c}, 
     "anti-Euler" -> {(3*a^4 - 4*(b^2 + c^2)*a^2 + (b^2 - c^2)^2)/
        (a*(-a^2 + b^2 + c^2)), (a^2 + b^2 - c^2)/b, (a^2 - b^2 + c^2)/c}, 
     "3rd anti-Euler" -> {(-((b^2 + c^2)*a^2 - b^4 + b^2*c^2 - c^4))*a, 
       (a^4 - (b^2 - c^2)*(a^2 - c^2))*b, (a^4 - (c^2 - b^2)*(a^2 - b^2))*c}, 
     "4th anti-Euler" -> {(-(-4*R^2*SA - S^2 + SA^2 - 2*SB*SC))*a, 
       (S^2 - 2*SA*SC - SB^2 + 4*(R^2 - SC)*SB)*b, 
       (S^2 - 2*SA*SB - SC^2 + 4*(R^2 - SB)*SC)*c}, 
     "anti-excenters-incenter reflections" -> {2*(a/(S^2 - 2*SB*SC)), 
       1/(b*SB), 1/(c*SC)}, "2nd anti-extouch" -> {S^2*(a/SA), SB*b, SC*c}, 
     "anti-inner-Garcia" -> {(-(a^2 - b^2 + b*c - c^2))*a, 
       a*(b*a - c^2) - (b^2 - c^2)*(b - c), a*(c*a - b^2) - 
        (c^2 - b^2)*(c - b)}, "anti-inner-Grebe" -> {(a^2 - S)/a, b, c}, 
     "anti-outer-Grebe" -> {(a^2 + S)/a, b, c}, "anti-Honsberger" -> 
      {-a^3/(b^2 + c^2), b, c}, "anti-Hutson intouch" -> 
      {(-(S^2 + 2*SA^2))*a, (S^2 - 2*SA*SB)*b, (S^2 - 2*SC*SA)*c}, 
     "anti-incircle-circles" -> {(-a)*(2*S^2 + SA^2), b*(2*S^2 - SA*SB), 
       c*(2*S^2 - SA*SC)}, "anti-inverse-in-incircle" -> 
      {-(a^2 + b^2 + c^2)/a, (a^2 + b^2 - c^2)/b, (a^2 - b^2 + c^2)/c}, 
     "anti-Mandart-incircle" -> {-a^2 + (b + c)*a - 2*b*c, (a - b + c)*b, 
       (a + b - c)*c}, "anti-McCay" -> 
      {-(5*a^4 - 2*(b^2 + c^2)*a^2 + (2*b^2 - c^2)*(b^2 - 2*c^2))/a, 
       ((2*a^2 + 2*b^2 - c^2)^2 - 9*a^2*b^2)/b, 
       ((2*a^2 + 2*c^2 - b^2)^2 - 9*a^2*c^2)/c}, "6th anti-mixtilinear" -> 
      {2*a, (a^2 - b^2 + c^2)/b, (a^2 + b^2 - c^2)/c}, 
     "anti-orthocentroidal" -> {a*((-a^2 + b^2 + c^2)^2 - b^2*c^2), 
       b*(a^4 - (2*b^2 - c^2)*a^2 + (b^2 - c^2)*(b^2 + 2*c^2)), 
       c*(a^4 + (b^2 - 2*c^2)*a^2 - (b^2 - c^2)*(2*b^2 + c^2))}, 
     "1st anti-orthosymmedial" -> 
      {(S^4 + 3*SA^2*S^2 + 2*(SA^2 - SB*SC - SW^2)*SA^2)*
        (a/(2*SA + SB + SC)), ((SA - SB)*S^2 - 2*(SA*SC - SB^2)*SA)*b, 
       ((SA - SC)*S^2 - 2*(SA*SB - SC^2)*SA)*c}, "anti-1st Parry" -> 
      {-(a^4 - 2*(b^2 + c^2)*a^2 + 3*(b^2 - c^2)^2)/a, 
       (a^2 + b^2 - 5*c^2)*(b/(a^2 - c^2))*(b^2 - c^2), 
       (a^2 - 5*b^2 + c^2)*c*((c^2 - b^2)/(a^2 - b^2))}, 
     "anti-2nd Parry" -> {-(2*S^4 + (5*SA^2 - 8*SB*SC + SW^2)*S^2 + 
          3*(SA^2 - 4*SB*SC - SW^2)*SA^2)/(a*(3*SA - SW)), b*(S^2 - 3*SA*SB), 
       c*(S^2 - 3*SA*SC)}, "1st anti-Sharygin" -> 
      {(-a)*SB*(SC/(S^2 + SB*SC)), SC^2*((SA + SB)/(b*(S^2 + SA*SC))), 
       SB^2*((SA + SC)/(c*(S^2 + SA*SB)))}, "anti-tangential-midarc" -> 
      {-a/(-a + b + c), (a + c)/(a - b + c), (a + b)/(a + b - c)}, 
     "3rd anti-tri-squares" -> 
      {2*S*((2*S^2 - (18*R^2 + SA - 4*SW)*S - 3*SB*SC)/
         (a*(3*SA - S)*(SA - S))), (3*SC - S)/b, (3*SB - S)/c}, 
     "4th anti-tri-squares" -> 
      {-2*S*((2*S^2 + (18*R^2 + SA - 4*SW)*S - 3*SB*SC)/
         (a*(3*SA + S)*(SA + S))), (3*SC + S)/b, (3*SB + S)/c}, 
     "anti-Ursa minor" -> {(b^2 + c^2)/a, (c^2 - a^2)/b, (b^2 - a^2)/c}, 
     "anti-Wasat" -> {(S^2 + SB*SC)*a, SB*(SA - SC)*b, SC*(SA - SB)*c}, 
     "anti-X3-ABC reflections" -> 
      {-(2*a^4 - 3*(b^2 + c^2)*a^2 + (b^2 - c^2)^2)/a, (a^2 - b^2 + c^2)*b, 
       (a^2 + b^2 - c^2)*c}, "Antlia" -> 
      {-(a^2 + 3*(b - c)^2)/(3*a^2 + (b - c)^2), 1, 1}, 
     "Apollonius" -> {(-((b + c)*a + b^2 + c^2)^2)*(a/(a + b + c)), 
       (a + c)^2*(a + b - c)*b, (a + b)^2*(a - b + c)*c}, 
     "Apus" -> {a/(a + b + c), -b/(a + b - c), -c/(a - b + c)}, 
     "Aquila" -> {(a + 2*b + 2*c)/a, -1, -1}, 
     "Ara" -> {a*(a^2 + b^2 + c^2), (-b)*(a^2 + b^2 - c^2), 
       (-c)*(a^2 - b^2 + c^2)}, "Aries" -> {-(a^4 + (b^2 - c^2)^2)/(2*a), 
       b*(b^2 - c^2), c*(c^2 - b^2)}, "Artzt" -> 
      {-(3*a^4 + (b^2 - c^2)^2)/(2*a), ((2*b^2 + c^2)*a^2 + c^2*(b^2 - c^2))/
        b, ((2*c^2 + b^2)*a^2 - (b^2 - c^2)*b^2)/c}, 
     "Ascella" -> {2*a, (a^2 - 2*(b + c)*a + c^2 - b^2)/b, 
       (a^2 - 2*(b + c)*a + b^2 - c^2)/c}, 
     "Atik" -> {-((b + c)*a^2 - 2*(b^2 + c^2)*a + (b + c)^3)/a, 
       a^2 - 2*(b - c)*a + (b + 3*c)*(b - c), a^2 - 2*(c - b)*a + 
        (c + 3*b)*(c - b)}, "Ayme" -> {(b + c)*((a^2 + (b + c)^2)/a), 
       -a^2 - b^2 + c^2, -a^2 - c^2 + b^2}, "Bankoff equilateral" -> 
      {(-((Sqrt[3] - 2)*SA + S))*a, (2*S^2 + S*SC + (Sqrt[3] - 2)*SA*SC)/b, 
       (2*S^2 + S*SB + (Sqrt[3] - 2)*SA*SB)/c}, 
     "BCI" -> {1, 1 + 2*Cos[angleC/2], 1 + 2*Cos[angleB/2]}, 
     "Bevan-antipodal" -> {-(-a + b + c)^(-1), 1/(a - b + c), 1/(a + b - c)}, 
     "1st Brocard" -> {a*b*c, c^3, b^3}, "1st Brocard-reflected" -> 
      {-a, (a^2 + b^2)/b, (a^2 + c^2)/c}, "2nd Brocard" -> 
      {b^2 + c^2 - a^2, a*b, a*c}, "3rd Brocard" -> {b^2*c^2, a*b^3, a*c^3}, 
     "4th Brocard" -> {a*b*(c/(b^2 + c^2 - a^2)), c, b}, 
     "5th Brocard" -> {((b^2 + c^2)*a^2 + (b^2 + c^2)^2 - b^2*c^2)/a, -b^3, 
       -c^3}, "6th Brocard" -> {((b^2 + c^2)*a^2 - b^2*c^2)/a, 
       (c^4 + c^2*b^2 - b^4)/b, (b^4 + b^2*c^2 - c^4)/c}, 
     "7th Brocard" -> {(a^4 + (b^2 - c^2)^2)/a, b*(a^2 - b^2 + c^2), 
       c*(a^2 + b^2 - c^2)}, "8th Brocard" -> {-2*a^3, b*(a^2 - b^2 + c^2), 
       c*(a^2 + b^2 - c^2)}, "9th Brocard" -> 
      {(-(a^2 + b^2 - c^2))*((a^2 - b^2 + c^2)/(2*a^3)), (a^2 + b^2 - c^2)/b, 
       (a^2 - b^2 + c^2)/c}, "circummedial" -> {(-a)*b*(c/(b^2 + c^2)), c, 
       b}, "circumnormal" -> {Sec[angleC/3 - angleB/3], 
       -Sec[angleB/3 + 2*(angleC/3)], -Sec[angleC/3 + 2*(angleB/3)]}, 
     "circumorthic" -> {-a/((b^2 + c^2)*a^2 - (b^2 - c^2)^2), 
       1/(b*(a^2 - b^2 + c^2)), 1/(c*(a^2 + b^2 - c^2))}, 
     "1st circumperp" -> {a, c - b, b - c}, "2nd circumperp" -> 
      {-a, b + c, b + c}, "2nd circumperp tangential" -> 
      {b*c + (a + b)*(a + c), (-b)*(a + b - c), (-c)*(a - b + c)}, 
     "circumsymmedial" -> {-a/2, b, c}, "circumtangential" -> 
      {Csc[angleC/3 - angleB/3], Csc[angleB/3 + 2*(angleC/3)], 
       -Csc[angleC/3 + 2*(angleB/3)]}, "Conway" -> {-(a + b + c)/(b + c), 
       (a + b - c)/b, (a - b + c)/c}, "2nd Conway" -> 
      {-(a + b + c)/a, (a + b - c)/b, (a - b + c)/c}, 
     "3rd Conway" -> {(-a)*(a + b + c)^2, ((b + 2*c)*a^3 + 2*a^2*b^2 + 
         (b - c)*(2*c^2 + b*c + b^2)*a + 2*(b^2 - c^2)*b*c)/b, 
       ((2*b + c)*a^3 + 2*a^2*c^2 - (b - c)*(c^2 + b*c + 2*b^2)*a - 
         2*(b^2 - c^2)*b*c)/c}, "4th Conway" -> 
      {(a^3 - (b + c)*a^2 - 2*(b^2 + b*c + c^2)*a - 2*b*c*(b + c))/
        ((a + b + c)*a^2), 1, 1}, "5th Conway" -> 
      {-((b + c)*a^2 + (b^2 + c^2)*a + 2*b*c*(b + c))/((a + b + c)*(b + c)*
         a), 1, 1}, "inner-Conway" -> {-1, (b - c)/b, (c - b)/c}, 
     "1st Ehrmann" -> {(-(a^4 - b^4 + 4*b^2*c^2 - c^4))*a, 
       (a^2*(a^2 + 5*c^2) - b^4 + 3*b^2*c^2 - 2*c^4)*b, 
       (a^2*(a^2 + 5*b^2) - 2*b^4 + 3*b^2*c^2 - c^4)*c}, 
     "2nd Ehrmann" -> {a*((a^2 - 2*b^2 - 2*c^2)/(2*a^2 - b^2 - c^2)), b, c}, 
     "Ehrmann cross" -> {(S^2 - 3*SB*SC)/a, 
       SB*((S^2 - 3*SC^2)/(b*(SB - SC))), SC*((S^2 - 3*SB^2)/(c*(SC - SB)))}, 
     "Ehrmann mid" -> {-(S^2 - 3*SB*SC)/a, (S^2 + 3*SA*SC)/b, 
       (S^2 + 3*SA*SB)/c}, "Ehrmann side" -> {a*(S^2 + 3*SB*SC), 
       SB*((S^2 - 3*SC^2)/b), SC*((S^2 - 3*SB^2)/c)}, 
     "Ehrmann vertex" -> {-((4*SA + SB + SC)*S^2 - (3*(SB + SC))*SA^2)/
        (a*(S^2 - 3*SA^2)), SC/b, SB/c}, "Euler" -> 
      {2*(((b^2 + c^2)*a^2 - (b^2 - c^2)^2)/(a*(b^2 + c^2 - a^2))), 
       (a^2 + b^2 - c^2)/b, (a^2 - b^2 + c^2)/c}, 
     "2nd Euler" -> {-2*((b^2 + c^2)*a^2 - (b^2 - c^2)^2)*a, 
       (a^2 - b^2 + c^2)*((a^4 - 2*a^2*c^2 + (b^2 - c^2)^2)/b), 
       (a^2 + b^2 - c^2)*((a^4 - 2*a^2*b^2 + (b^2 - c^2)^2)/c)}, 
     "3rd Euler" -> {-(b - c)^2/a, (a^2 + c*(b - c))/b, (a^2 + b*(c - b))/c}, 
     "4th Euler" -> {-(b + c)^2/a, (a^2 - c*(b + c))/b, (a^2 - b*(b + c))/c}, 
     "5th Euler" -> {-2*((b^2 + c^2)/(a*(a^2 - b^2 - c^2))), 1/b, 1/c}, 
     "excenters-incenter reflections" -> {(-a + 3*b + 3*c)/(-3*a + b + c), 1, 
       1}, "excenters-midpoints" -> {(-2*a + b + c)/a, 1, 1}, 
     "excentral" -> {-1, 1, 1}, "extangents" -> {(-a)*(a + b + c), 
       (a + c)*(a + b - c), (a + b)*(a - b + c)}, 
     "extouch" -> {0, 1/(b*(a + b - c)), 1/(c*(a - b + c))}, 
     "2nd extouch" -> {-2*(b + c), (a^2 + b^2 - c^2)/b, (a^2 - b^2 + c^2)/c}, 
     "3rd extouch" -> {-2*(b + c)*(a + b - c)*((a - b + c)/
         ((-a + b + c)*(a + b + c))), (a^2 + b^2 - c^2)/b, 
       (a^2 - b^2 + c^2)/c}, "4th extouch" -> 
      {-2*(b + c)*((a + b + c)/(-a + b + c)), (a^2 - b^2 + c^2)/b, 
       (a^2 + b^2 - c^2)/c}, "5th extouch" -> {-2*((b + c)/(-a + b + c)), 
       (b^2 + c^2 + a^2 + 2*c*a)/(b*(a - b + c)), (b^2 + 2*a*b + a^2 + c^2)/
        (c*(a + b - c))}, "inner-Fermat" -> {(-Sqrt[3])*a, 
       (Sqrt[3]*SC - S)/b, (Sqrt[3]*SB - S)/c}, "outer-Fermat" -> 
      {(-Sqrt[3])*a, (Sqrt[3]*SC + S)/b, (Sqrt[3]*SB + S)/c}, 
     "Feuerbach" -> {(-(b - c)^2)*((a + b + c)/a), (a + c)^2*((a + b - c)/b), 
       (a + b)^2*((a - b + c)/c)}, "Fuhrmann" -> {a, -(a^2 - c*(b + c))/b, 
       -(a^2 - b*(b + c))/c}, "2nd Fuhrmann" -> {-a, (b*c + a^2 - c^2)/b, 
       (b*c + a^2 - b^2)/c}, "inner-Garcia" -> {a, (c*a - b^2 + c^2)/b, 
       (a*b + b^2 - c^2)/c}, "outer-Garcia" -> {-1, (a + c)/b, (a + b)/c}, 
     "Garcia-reflection" -> {1, (c - a)/b, (b - a)/c}, 
     "Gossard" -> {SA^2*(SB - SC)^2*((S^2 - 3*SB*SC)/(a*S^2)), 
       (S^2 - 3*SA*SC)*((4*R^2*(-6*SB + SW) + 5*S^2 - SW^2 + 6*SB^2 - 
          4*SA*SC)/b), (S^2 - 3*SA*SB)*((4*R^2*(-6*SC + SW) + 5*S^2 - SW^2 + 
          6*SC^2 - 4*SA*SB)/c)}, "inner-Grebe" -> {-(b^2 + c^2 - S)/a, b, c}, 
     "outer-Grebe" -> {-(b^2 + c^2 + S)/a, b, c}, "1st Hatzipolakis" -> 
      {(-a + b + c)*a, (a^2 + b^2 - c^2)^2/(b*(a - b + c)), 
       (a^2 - b^2 + c^2)^2/(c*(a + b - c))}, "2nd Hatzipolakis" -> 
      {0, 1/(b*(a^2 - b^2 + c^2)*(a - b + c)^2), 
       1/(c*(a^2 + b^2 - c^2)*(a + b - c)^2)}, "3rd Hatzipolakis" -> 
      {2*S^2*((10*R^2 - 2*SA - SB - SC)/a), b*(S^2 + SA*SB), 
       c*(S^2 + SA*SC)}, "Hatzipolakis-Moses" -> 
      {2*S^2*((6*R^2 - 2*SA - SB - SC)/a), b*(S^2 + SA*SB), c*(S^2 + SC*SA)}, 
     "hexyl" -> {a^3 + (-b - c)*a^2 - (b + c)^2*a + (b^2 - c^2)*(b - c), 
       a^3 + (c - b)*a^2 - (b - c)^2*a + (b^2 - c^2)*(b + c), 
       a^3 + (b - c)*a^2 - (b - c)^2*a - (b^2 - c^2)*(b + c)}, 
     "Honsberger" -> {1/((b + c)*a - (b - c)^2), -(b*(a - b + c))^(-1), 
       -(c*(a + b - c))^(-1)}, "Hung-Feuerbach" -> 
      {(-(b + c)^2)*(a + b - c)*(a - b + c)*(a^2 + (b + c)^2)*
        (a^2 + c*a + b*(b - c))*((a^2 + b*a + 3*(c - b))/a), 
       (a + c)*(a^5 + c*a^4 - (b^2 - c^2)^2*a + c*(b^2 - c^2)*(3*b^2 + c^2))*
        ((a + c)*b + a^2 + c^2)*(((-a + b)*c + a^2 + b^2)/b), 
       (a + b)*(a^5 + b*a^4 - (c^2 - b^2)^2*a + b*(c^2 - b^2)*(3*c^2 + b^2))*
        ((a + b)*c + a^2 + b^2)*(((-a + c)*b + a^2 + c^2)/c)}, 
     "Hutson extouch" -> {-4*(a/(a + b + c)), (a + b - c)/b, (a - b + c)/c}, 
     "Hutson intouch" -> {4*(a/(-a + b + c)), (a - b + c)/b, (a + b - c)/c}, 
     "1st Hyacinth" -> {(-a^3)*SA*((SA^2 - 3*S^2)/(2*S^2)), 
       ((12*R^2 - 2*SW)*S^2 - (2*R^2*(SA + SC) - SC^2 + S^2 - (SA + SC)^2)*
          SB)/b, ((12*R^2 - 2*SW)*S^2 - (2*R^2*(SA + SB) - SB^2 + S^2 - 
           (SA + SB)^2)*SC)/c}, "2nd Hyacinth" -> 
      {-((b^2 + c^2)*a^4 - 2*(b^2 - c^2)^2*a^2 + (b^2 + c^2)*(b^2 - c^2)^2)/
        (a*(a^2 - b^2 - c^2)), (a^2 - b^2 + c^2)*b, (a^2 + b^2 - c^2)*c}, 
     "incentral" -> {0, 1, 1}, "incircle-circles" -> 
      {2*a, (a^2 + 4*a*b + b^2 - c^2)/b, (a^2 + 4*a*c + c^2 - b^2)/c}, 
     "intangents" -> {(-a)*(-a + b + c), (a - c)*(a - b + c), 
       (a - b)*(a + b - c)}, "intouch" -> {0, 1/(b*(a - b + c)), 
       1/(c*(a + b - c))}, "inverse-in-excircles" -> 
      {-((b + c)*a + (b - c)^2)/(a*(a + b + c)), 1, 1}, 
     "inverse-in-incircle" -> {((b + c)*a - (b - c)^2)/(a*(-a + b + c)), 1, 
       1}, "Jenkins-contact" -> {(-(b + c)^2)*(a + b - c)*
        ((a - b + c)/(a*(a + b + c))), c^2*((a - b + c)/b), 
       b^2*((a - c + b)/c)}, "Jenkins-tangential" -> 
      {(-((3*(b + c))*a^3 + (2*b^2 + 3*b*c + 2*c^2)*a^2 - (b + c)^2*b*c - 
          (b + c)*(b^2 + c^2)*a))*b*(c/a^2), ((b + c)*a^3 + (2*b - c)*b*a^2 + 
         (b^3 - c^3 - b*c*(b + c))*a + (b^2 - c^2)*b*c)*((a + c)/b), 
       ((c + b)*a^3 + (2*c - b)*c*a^2 + (c^3 - b^3 - c*b*(c + b))*a + 
         (c^2 - b^2)*c*b)*((a + b)/c)}, "Johnson" -> {(-a)*(a^2 - b^2 - c^2), 
       (a^4 + (-b^2 - 2*c^2)*a^2 - c^2*(b^2 - c^2))/b, 
       (a^4 + (-2*b^2 - c^2)*a^2 + (b^2 - c^2)*b^2)/c}, 
     "inner-Johnson" -> {-a^2 + (b + c)*a - 2*b*c, (a - b + c)*((a - c)^2/b), 
       (a + b - c)*((a - b)^2/c)}, "outer-Johnson" -> 
      {-a^2 - (b + c)*a - 2*b*c, (a + c)^2*((a + b - c)/b), 
       (a + b)^2*((a - b + c)/c)}, "1st Johnson-Yff" -> 
      {a/(-a + b + c), (a + c)^2/(b*(a - b + c)), (a + b)^2/(c*(a + b - c))}, 
     "2nd Johnson-Yff" -> {(-a)*(-b - c + a), (a - b + c)*((a - c)^2/b), 
       (a + b - c)*((a - b)^2/c)}, "1st Kenmotu diagonals" -> 
      {a*((b^2 + c^2 - a^2 - 2*S)/(b^2 + c^2 - a^2 + 2*S)), b, c}, 
     "2nd Kenmotu diagonals" -> {a*((b^2 + c^2 - a^2 + 2*S)/
         (b^2 + c^2 - a^2 - 2*S)), b, c}, "Kosnita" -> 
      {(a^4 + (-2*b^2 - 2*c^2)*a^2 + b^4 + c^4)*a, 
       (-b)*(a^4 + (-2*b^2 - c^2)*a^2 + (b^2 - c^2)*b^2), 
       (-(a^4 + (-b^2 - 2*c^2)*a^2 - c^2*(b^2 - c^2)))*c}, 
     "Lemoine" -> {0, a*(c/(2*a^2 + 2*c^2 - b^2)), 
       a*(b/(2*a^2 + 2*b^2 - c^2))}, "MacBeath" -> 
      {0, 1/(b^3*(a^2 - b^2 + c^2)), 1/(c^3*(a^2 + b^2 - c^2))}, 
     "Malfatti" -> {(2*Cos[angleB/2]*Cos[angleC/2] + 2*Cos[angleB/2] + 
         2*Cos[angleC/2] - Cos[angleA/2] + 1)/(Cos[angleA/2] + 1), 1, 1}, 
     "Mandart-excircles" -> {(-(b - c)^2)*((a + b + c)/a), b*(a + b - c), 
       c*(a - b + c)}, "Mandart-incircle" -> {(b + c - a)*((b - c)^2/a), 
       b*(a - b + c), c*(a + b - c)}, "McCay" -> {a*(a^2 + b^2 + c^2), 
       -(2*a^4 + (-3*c^2 - 2*b^2)*a^2 + (b^2 - c^2)*(-c^2 + 2*b^2))/b, 
       -(2*a^4 + (-2*c^2 - 3*b^2)*a^2 + (b^2 - c^2)*(-2*c^2 + b^2))/c}, 
     "medial" -> {0, 1/b, 1/c}, "midarc" -> 
      {(Cos[angleB/2] + Cos[angleC/2])^2, Cos[angleA/2]^2, Cos[angleA/2]^2}, 
     "2nd midarc" -> {(Cos[angleB/2] - Cos[angleC/2])^2, Cos[angleA/2]^2, 
       Cos[angleA/2]^2}, "midheight" -> {2*a, (a^2 + b^2 - c^2)/b, 
       (a^2 - b^2 + c^2)/c}, "2nd mixtilinear" -> 
      {-(a^3 + (b + c)*a^2 - (b + c)^2*a - (b^2 - c^2)*(b - c))/(4*a*b*c), 1, 
       1}, "3rd mixtilinear" -> {-1, 2*(b/(a - b + c)), 2*(c/(a + b - c))}, 
     "4th mixtilinear" -> {-1, 2*(b/(a + b - c)), 2*(c/(a - b + c))}, 
     "5th mixtilinear" -> {-(b + c - a)/(2*a), 1, 1}, 
     "6th mixtilinear" -> {a^2 - 2*(b + c)*a + (b - c)^2, 
       a^2 - (b - c)*(2*a - b - 3*c), a^2 - (c - b)*(2*a - c - 3*b)}, 
     "7th mixtilinear" -> {(a - b + c)*(a + b - c)*
        ((a^2 - 2*(b + c)*a + (b - c)^2)/(2*(-a + b + c)*a)), 
       a^2 - 2*(b - c)*a + (b + 3*c)*(b - c), a^2 + 2*(b - c)*a + 
        (c + 3*b)*(c - b)}, "Montesdeoca-Hung" -> 
      {2*a^5*(a + 2*b + 2*c) + 4*a^3*(b^2 + 3*b*c + c^2)*(a + b + c) + 
        (3*b^4 + 3*c^4 + 4*b*c*(3*b^2 + 5*b*c + 3*c^2))*a^2 + 
        (b + c)^2*(2*(b + c)*(b^2 + c^2)*a + b^4 + c^4), 
       (-(a + c)^2)*(a^2 + b*a + c*(b + c))^2, (-(a + b)^2)*
        (a^2 + c*a + b*(c + b))^2}, "1st Morley" -> {1, 2*Cos[angleC/3], 
       2*Cos[angleB/3]}, "2nd Morley" -> {-1, 2*Cos[angleC/3 + Pi/3], 
       2*Cos[angleB/3 + Pi/3]}, "3rd Morley" -> {-1, 2*Cos[angleC/3 - Pi/3], 
       2*Cos[angleB/3 - Pi/3]}, "1st Morley-adjunct" -> 
      {2, Sec[angleC/3], Sec[angleB/3]}, "2nd Morley-adjunct" -> 
      {-2, Sec[angleC/3 + Pi/3], Sec[angleB/3 + Pi/3]}, 
     "3rd Morley-adjunct" -> {-2, Sec[angleC/3 - Pi/3], 
       Sec[angleB/3 - Pi/3]}, "Moses-Hung" -> 
      {-(2*a^3 + (b + c)*a^2 + (b^2 - c^2)*(b - c))^2/(a*(a + b + c)), 
       (a + c)^2*((a + b - c)^3/b), (a + b)^2*((a - b + c)^3/c)}, 
     "Moses-Soddy" -> {1/a, (a - c)/(b*(b - c)), (a - b)/(c*(c - b))}, 
     "Moses-Steiner osculatory" -> 
      {(-3*a^4 + 2*(b^2 + c^2)*a^2 - (b^2 - c^2)^2)/a^3, (a^2 + b^2 - c^2)/b, 
       (a^2 - b^2 + c^2)/c}, "Moses-Steiner reflection" -> 
      {(-a^2 + b^2 + c^2)/a, (a^2 - b^2 - 2*c^2)/b, (a^2 - 2*b^2 - c^2)/c}, 
     "inner-Napoleon" -> {-a, (SC - Sqrt[3]*S)/b, (SB - Sqrt[3]*S)/c}, 
     "outer-Napoleon" -> {-a, (SC + Sqrt[3]*S)/b, (SB + Sqrt[3]*S)/c}, 
     "1st Neuberg" -> {(-a)*SW, (SC^2 - SA*SB)/b, (SB^2 - SA*SC)/c}, 
     "2nd Neuberg" -> {(-a)*SW, (SC^2 - SA*SB + 2*S^2)/b, 
       (SB^2 - SA*SC + 2*S^2)/c}, "orthic" -> {0, 1/(b*(a^2 - b^2 + c^2)), 
       1/(c*(a^2 + b^2 - c^2))}, "orthic axes" -> {2*SB*(SC/a), SA*(SC/b), 
       SA*(SB/c)}, "orthocentroidal" -> {a, (a^2 + b^2 - c^2)/b, 
       (a^2 - b^2 + c^2)/c}, "1st orthosymmedial" -> 
      {2*(a^3/(b^2 + c^2)), (a^2 + b^2 - c^2)/b, (a^2 - b^2 + c^2)/c}, 
     "2nd orthosymmedial" -> {-((b^4 + c^4)*a^2 + (b^2 - c^2)^2*(-b^2 - c^2))/
        (a*(b^2 + c^2)*(a^2 - b^2 - c^2)), b, c}, "Paasche-Hutson" -> 
      {S*((S - 2*a*R)/a), (S + a*b)*c, (S + a*c)*b}, 
     "1st Pamfilos-Zhou" -> {(-a^4)*(a^2 + b*c) + (b^2 + c^2)*(b + c)^2*a^2 + 
        b*c*(b + c)*(4*S*a - (b^2 - c^2)*(b - c)), (a^2 - b^2 + c^2)*
        ((a^2 + b^2 + c^2)*a*b + 2*c^2*S), (a^2 + b^2 - c^2)*
        ((a^2 + b^2 + c^2)*a*c + 2*b^2*S)}, "2nd Pamfilos-Zhou" -> 
      {-2*(b + c)*S - (-a + b + c)*((b + c)*a + (b - c)^2), 
       (2*(a - c)*a*S + (-a + b + c)*(a^2*c + (b - c)*(b^2 + a*b + c^2)))/b, 
       (2*(a - b)*a*S + (-a + b + c)*(a^2*b + (c - b)*(c^2 + a*c + b^2)))/c}, 
     "1st Parry" -> {3*a^4 + (-2*b^2 - 2*c^2)*a^2 - b^2*c^2 + c^4 + b^4, 
       (-b)*(a^2 + b^2 - 2*c^2)*a, (-c)*(a^2 - 2*b^2 + c^2)*a}, 
     "2nd Parry" -> {(a^4 + b^2*c^2 - b^4 - c^4)*(b^2 - c^2), 
       (a^2 - b^2)*b*a*(2*a^2 - b^2 - c^2), (-(a^2 - c^2))*c*a*
        (2*a^2 - b^2 - c^2)}, "3rd Parry" -> 
      {a*(a^4 + (-3*b^2 - 3*c^2)*a^2 + 2*b^4 + b^2*c^2 + 2*c^4), 
       b*c^2*(2*a^2 - b^2 - c^2), b^2*c*(2*a^2 - b^2 - c^2)}, 
     "Pelletier" -> {(-(b - c))*(-a + b + c), (c - a)*(a - b + c), 
       (a - b)*(a - c + b)}, "1st Przybyowski-Bollin" -> 
      {a*(Sqrt[3]*SA + S), Sqrt[3]*SB*b + (b + 2*Sqrt[SW + Sqrt[3]*S])*S, 
       Sqrt[3]*SC*c + (c + 2*Sqrt[SW + Sqrt[3]*S])*S}, 
     "2nd Przybyowski-Bollin" -> {a*(Sqrt[3]*SA + S), 
       Sqrt[3]*b*SB + (b - 2*Sqrt[SW + Sqrt[3]*S])*S, 
       Sqrt[3]*c*SC + (c - 2*Sqrt[SW + Sqrt[3]*S])*S}, 
     "3rd Przybyowski-Bollin" -> {a*(Sqrt[3]*SA - S), 
       Sqrt[3]*SB*b - (b + 2*Sqrt[SW - Sqrt[3]*S])*S, 
       Sqrt[3]*SC*c - (c + 2*Sqrt[SW - Sqrt[3]*S])*S}, 
     "4th Przybyowski-Bollin" -> {a*(Sqrt[3]*SA - S), 
       Sqrt[3]*SB*b - (b - 2*Sqrt[SW - Sqrt[3]*S])*S, 
       Sqrt[3]*SC*c - (c - 2*Sqrt[SW - Sqrt[3]*S])*S}, 
     "reflection" -> {-1, (a^2 + b^2 - c^2)/(a*b), (a^2 - b^2 + c^2)/(a*c)}, 
     "Roussel" -> {-a^2 - 2*(c*Cos[angleB/3] + b*Cos[angleC/3])*a + 
        4*b*c*(4*Cos[angleA/3]^2 - 1)*Cos[angleB/3]*Cos[angleC/3], 
       (a + 2*b*Cos[angleC/3])*(2*Cos[angleA/3]*c + b) + 
        4*c*Cos[angleA/3]*Cos[angleB/3]*(2*Cos[angleA/3]*b + c), 
       (a + 2*c*Cos[angleB/3])*(2*Cos[angleA/3]*b + c) + 
        4*b*Cos[angleA/3]*Cos[angleC/3]*(2*Cos[angleA/3]*c + b)}, 
     "1st Schiffler" -> {1/a, (a - c)/(a^2 - (b + 2*c)*a + c^2 - b^2), 
       (a - b)/(a^2 - (c + 2*b)*a + b^2 - c^2)}, "2nd Schiffler" -> 
      {-a^(-1), (a - c)/(a^2 + (b - 2*c)*a + c^2 - b^2), 
       (a - b)/(a^2 + (c - 2*b)*a + b^2 - c^2)}, "Schr\[ODoubleDot]eter" -> 
      {1/a, (a^2 - c^2)/(b*(b^2 - c^2)), (a^2 - b^2)/(c*(c^2 - b^2))}, 
     "1st Sharygin" -> {-a^2 + b*c, a*b + c^2, a*c + b^2}, 
     "2nd Sharygin" -> {-a^2 + b*c, a*b - c^2, a*c - b^2}, 
     "Soddy" -> {-(a*(-a + b + c))^(-1), 1/(b*(a - b + c)), 
       1/(c*(a + b - c))}, "inner-Soddy" -> 
      {(a*(-a + b + c) + 2*S)/(a*(-a + b + c)), (b*(a + c - b) + S)/
        (b*(a - b + c)), (c*(a + b - c) + S)/(c*(a - c + b))}, 
     "outer-Soddy" -> {(a*(-a + b + c) - 2*S)/(a*(-a + b + c)), 
       (b*(a + c - b) - S)/(b*(a - b + c)), (c*(a + b - c) - S)/
        (c*(a - c + b))}, "inner-squares" -> {2*a, (a^2 + b^2 - c^2 + 2*S)/b, 
       (a^2 + c^2 - b^2 + 2*S)/c}, "outer-squares" -> 
      {2*a, (a^2 + b^2 - c^2 - 2*S)/b, (a^2 + c^2 - b^2 - 2*S)/c}, 
     "Stammler" -> {Cos[angleA] - 2*Cos[(angleB - angleC)/3], 
       Cos[angleB] + 2*Cos[angleB/3 + 2*(angleC/3)], 
       Cos[angleC] + 2*Cos[2*(angleB/3) + angleC/3]}, 
     "Steiner" -> {0, (-c)*(a^2 - b^2), b*(a^2 - c^2)}, 
     "submedial" -> {2*a*b*c, (3*a^2 + b^2 - c^2)*c, (3*a^2 + c^2 - b^2)*b}, 
     "symmedial" -> {0, 1/c, 1/b}, "1st Altintas-isodynamic triangle" -> 
      {(-((S^2 + SB*SC)*Sqrt[3]*S + (SA + 3*SW)*S^2 - 2*(S^2 - SA*SW + SW^2)*
           SA))*(a*(S^2 - Sqrt[3]*SA*S - 2*SA*SW)), SC/b, SB/c}, 
     "2nd Altintas-isodynamic triangle" -> 
      {-((-(S^2 + SB*SC))*Sqrt[3]*S + (SA + 3*SW)*S^2 - 
          2*(S^2 - SA*SW + SW^2)*SA)/(a*(S^2 + Sqrt[3]*SA*S - 2*SA*SW)), 
       SC/b, SB/c}, "anti-1st Auriga" -> 
      {-(Sqrt[r*R + 4*R^2]*a + (a + b + c)*(a*(b^2 + c^2) - 
            (b + c)*(b - c)^2))/a, b*(a + b + c)*(a - b + c) - 
        Sqrt[r*R + 4*R^2], c*(a + b + c)*(a + b - c) - Sqrt[r*R + 4*R^2]}, 
     "anti-2nd Auriga" -> 
      {-((-Sqrt[r*R + 4*R^2])*a + (a + b + c)*(a*(b^2 + c^2) - 
            (b + c)*(b - c)^2))/a, b*(a + b + c)*(a - b + c) + 
        Sqrt[r*R + 4*R^2], c*(a + b + c)*(a + b - c) + Sqrt[r*R + 4*R^2]}, 
     "anti-1st Kenmotu-free-vertices" -> {(SA + S)*((SB + SC + 2*S)/a), 
       (SB - S)*b, (SC - S)*c}, "anti-2nd Kenmotu-free-vertices" -> 
      {(SA - S)*((SB + SC - 2*S)/a), (SB + S)*b, (SC + S)*c}, 
     "anti-3rd tri-squares-central" -> {-(b^2 + c^2 + 3*S)/a, (b^2 + S)/b, 
       (c^2 + S)/c}, "anti-4th tri-squares-central" -> 
      {-(b^2 + c^2 - 3*S)/a, (b^2 - S)/b, (c^2 - S)/c}, 
     "anti-inner-Yff" -> {(a^4 - 2*(b^2 + c^2)*a^2 - 2*(b + c)*b*c*a + 
         (b^2 - c^2)^2)/(2*a^2*b*c), 1, 1}, "anti-outer-Yff" -> 
      {(a^4 - 2*(b^2 + c^2)*a^2 + 2*(b + c)*b*c*a + (b^2 - c^2)^2)/
        (2*a^2*b*c), -1, -1}, "1st Auriga" -> 
      {(a^4 - (b + c)^2*a^2 - 4*(b + c)*S*Sqrt[r*R + 4*R^2])/a, 
       (b^4 - b^2*(a + c)^2 + 4*b*S*Sqrt[r*R + 4*R^2])/b, 
       (c^4 - c^2*(a + b)^2 + 4*c*S*Sqrt[r*R + 4*R^2])/c}, 
     "2nd Auriga" -> {(a^4 - (b + c)^2*a^2 + 4*(b + c)*S*Sqrt[r*R + 4*R^2])/
        a, (b^4 - b^2*(a + c)^2 - 4*b*S*Sqrt[r*R + 4*R^2])/b, 
       (c^4 - c^2*(a + b)^2 - 4*c*S*Sqrt[r*R + 4*R^2])/c}, 
     "1st Ehrmann circumscribing triangle" -> 
      {-(S^2 - Sqrt[3]*(SB - SC)*S - 3*SB*SC)/a, (S + Sqrt[3]*SA)*
        ((S + Sqrt[3]*SC)/b), (S - Sqrt[3]*SA)*((S - Sqrt[3]*SB)/c)}, 
     "2nd Ehrmann circumscribing triangle" -> 
      {-(S^2 + Sqrt[3]*(SB - SC)*S - 3*SB*SC)/a, (S - Sqrt[3]*SA)*
        ((S - Sqrt[3]*SC)/b), (S + Sqrt[3]*SA)*((S + Sqrt[3]*SB)/c)}, 
     "1st Ehrmann inscribed triangle" -> {0, (S - Sqrt[3]*SA)*c, 
       (S + Sqrt[3]*SA)*b}, "2nd Ehrmann inscribed triangle" -> 
      {0, (S + Sqrt[3]*SA)*c, (S - Sqrt[3]*SA)*b}, "1st excosine triangle" -> 
      {(-S^2)*a, (S^2 - 2*SA*SB)*b, (S^2 - 2*SA*SC)*c}, 
     "2nd excosine triangle" -> {(-S^2)*SB*(SC/(SA*a)), (S^2 - 2*SA*SB)*b, 
       (S^2 - 2*SA*SC)*c}, "1st Fermat-Dao equilateral triangle" -> 
      {(-a)*((Sqrt[3]*S*((-6*R^2 + 3*SA + 12*SW)*S^2 + 
            (7*SA^2 + SB*SC + SW^2)*SW + 18*R^2*SB*SC) + 
          (20*S^2 - 6*R^2*(3*SA - SW) + 19*SA^2 - SB*SC + 11*SW^2)*S^2 + 
          3*(2*SA - SW)*SA*SW^2)/(Sqrt[3]*a^2 + 2*S)), 
       (S + Sqrt[3]*SB)*(S^2 + Sqrt[3]*(6*R^2 - SW + SC)*S + SW*SC)*b, 
       (S + Sqrt[3]*SC)*(S^2 + Sqrt[3]*(6*R^2 - SW + SB)*S + SB*SW)*c}, 
     "2nd Fermat-Dao equilateral triangle" -> 
      {(-a)*(((-Sqrt[3])*S*((-6*R^2 + 3*SA + 12*SW)*S^2 + 
            (7*SA^2 + SB*SC + SW^2)*SW + 18*R^2*SB*SC) + 
          (20*S^2 - 6*R^2*(3*SA - SW) + 19*SA^2 - SB*SC + 11*SW^2)*S^2 + 
          3*(2*SA - SW)*SA*SW^2)/(Sqrt[3]*a^2 - 2*S)), 
       (-S + Sqrt[3]*SB)*(S^2 - Sqrt[3]*(6*R^2 - SW + SC)*S + SW*SC)*b, 
       (-S + Sqrt[3]*SC)*(S^2 - Sqrt[3]*(6*R^2 - SW + SB)*S + SB*SW)*c}, 
     "3rd Fermat-Dao equilateral triangle" -> 
      {(Sqrt[3]*(3*S^2 + SB*SC) + 5*(SB + SC)*S)/(a*(Sqrt[3]*SA + S)), 
       (SC + Sqrt[3]*S)/b, (SB + Sqrt[3]*S)/c}, 
     "4th Fermat-Dao equilateral triangle" -> 
      {(Sqrt[3]*(3*S^2 + SB*SC) - 5*(SB + SC)*S)/(a*(Sqrt[3]*SA - S)), 
       (SC - Sqrt[3]*S)/b, (SB - Sqrt[3]*S)/c}, 
     "5th Fermat-Dao equilateral triangle" -> 
      {((7*SA + 4*SW)*S^2 + 3*SA^3 + 5*Sqrt[3]*(S^2 + SA^2)*S)/
        (a*(Sqrt[3]*SA + S)^2), b*((Sqrt[3]*c^2 + 2*S)/(Sqrt[3]*SB + S)), 
       c*((Sqrt[3]*b^2 + 2*S)/(Sqrt[3]*SC + S))}, 
     "6th Fermat-Dao equilateral triangle" -> 
      {((7*SA + 4*SW)*S^2 + 3*SA^3 - 5*Sqrt[3]*(S^2 + SA^2)*S)/
        (a*(Sqrt[3]*SA - S)^2), b*((Sqrt[3]*c^2 - 2*S)/(Sqrt[3]*SB - S)), 
       c*((Sqrt[3]*b^2 - 2*S)/(Sqrt[3]*SC - S))}, 
     "7th Fermat-Dao equilateral triangle" -> 
      {2*((Sqrt[3]*(SB + SC) + 2*S)/(a*(Sqrt[3]*SA + S))), 1/b, 1/c}, 
     "8th Fermat-Dao equilateral triangle" -> 
      {2*((Sqrt[3]*(SB + SC) - 2*S)/(a*(Sqrt[3]*SA - S))), 1/b, 1/c}, 
     "9th Fermat-Dao equilateral triangle" -> 
      {2*S*(((2*S^2 + SA^2 + SB*SC)*Sqrt[3] + (6*R^2 + SA + SW)*S)/
         (a*(Sqrt[3]*SA + S)^2)), b, c}, 
     "10th Fermat-Dao equilateral triangle" -> 
      {-2*S*(((2*S^2 + SA^2 + SB*SC)*Sqrt[3] - (6*R^2 + SA + SW)*S)/
         (a*(Sqrt[3]*SA - S)^2)), b, c}, 
     "11th Fermat-Dao equilateral triangle" -> 
      {(Sqrt[3]*(S^2 + SB*SC) + (SB + SC)*S)/(a*(Sqrt[3]*SA - S)), SC/b, 
       SB/c}, "12th Fermat-Dao equilateral triangle" -> 
      {(Sqrt[3]*(S^2 + SB*SC) - (SB + SC)*S)/(a*(Sqrt[3]*SA + S)), SC/b, 
       SB/c}, "13th Fermat-Dao equilateral triangle" -> 
      {(Sqrt[3]*(2*S^2 + SA^2 + SB*SC) + (SA + SW)*S)/(a*(Sqrt[3]*SA - S)), 
       b, c}, "14th Fermat-Dao equilateral triangle" -> 
      {(Sqrt[3]*(2*S^2 + SA^2 + SB*SC) - (SA + SW)*S)/(a*(Sqrt[3]*SA + S)), 
       b, c}, "15th Fermat-Dao equilateral triangle" -> 
      {(Sqrt[3]*(3*S^2 + SB*SC) + 5*(SB + SC)*S)/(a*(SA - Sqrt[3]*S)), 
       (Sqrt[3]*SC + S)/b, (Sqrt[3]*SB + S)/c}, 
     "16th Fermat-Dao equilateral triangle" -> 
      {(Sqrt[3]*(3*S^2 + SB*SC) - 5*(SB + SC)*S)/(a*(SA + Sqrt[3]*S)), 
       (Sqrt[3]*SC - S)/b, (Sqrt[3]*SB - S)/c}, 
     "VPH1" -> {((-b)*c + 4*R^2)/a, 2*R + c, 2*R + b}, 
     "VPH2" -> {(8*R^3 - a*S)/a, (-c)*a + 4*R^2, (-b)*a + 4*R^2}, 
     "VPH3" -> {((-b)*c + 4*R^2)/(4*R^2), (2*R + b)/b, (2*R + c)/c}, 
     "VPH4" -> {(2*a*b*c + (b + c)*S)/S, (b*a + S)/b, (c*a + S)/c}, 
     "VPH6" -> {(2*a*b*c + (b + c)*S)*(S/(2*b*c + S)), 
       (c*a + S)*((2*b*a + S)/b), (b*a + S)*((2*c*a + S)/c)}, 
     "VPH7" -> {(a^4 - 2*(b - c)^2*a^2 + (b^2 - c^2)^2)*
        (S/(4*(2*b*c + S)*a^2)), c*a + S, b*a + S}, 
     "VPH8" -> {((-a^4 + 2*(b^2 + 6*b*c + c^2)*a^2 + 8*(b + c)*S*a - 
          (b^2 - c^2)^2)/4)*S*a, (b*a + S)/b, (c*a + S)/c}, 
     "VPH9" -> {(2*a*b*c + (b + c)*S)/(2*b*c + S), -(b*a + S)/b, 
       -(c*a + S)/c}, "VPH10" -> {(-a^8 + 4*(b^2 + c^2)*a^6 + 
         32*(b + c)*S*a*b^2*c^2 - 2*(3*b^4 + 4*b^2*c^2 + 3*c^4)*a^4 + 
         4*(b^6 + c^6 + (b^2 + 16*b*c + c^2)*b^2*c^2)*a^2 - (b^4 - c^4)^2)/
        (4*a*S), (4*S*b*c + a^4 - 2*(b^2 + c^2)*a^2 + 8*b^2*c*a + 
         (b^2 - c^2)^2)*c, (4*S*b*c + a^4 - 2*(b^2 + c^2)*a^2 + 8*b*c^2*a + 
         (b^2 - c^2)^2)*b}, "VPH11" -> {0, 1/(b*((c + R)*b + 3*R*c + 4*R^2)), 
       1/(c*((c + 3*R)*b + R*c + 4*R^2))}, 
     "VPH12" -> {(-3*a^4 + 2*(3*b^2 + 2*b*c + 3*c^2)*a^2 + 8*(b + c)*S*a - 
         3*(b^2 - c^2)^2)/(4*a*(b*c + 2*S)), -(b*a + S)/b, -(c*a + S)/c}, 
     "VPH13" -> {(-3*a^4 + 2*(3*b^2 + 2*b*c + 3*c^2)*a^2 + 8*(b + c)*S*a - 
         3*(b^2 - c^2)^2)/(4*S*a), -(b*a + S)/b, -(c*a + S)/c}, 
     "VPH14" -> {(-3*a^4 + 2*(3*b^2 + 2*b*c + 3*c^2)*a^2 + 8*(b + c)*S*a - 
         3*(b^2 - c^2)^2)*(S/(4*a*(b*c + 2*S))), (-(c*a + S))*
        ((b*a + 2*S)/b), (-(b*a + S))*((c*a + 2*S)/c)}, 
     "VPH15" -> {(-3*a^4 + 2*(3*b^2 + 2*b*c + 3*c^2)*a^2 + 8*(b + c)*S*a - 
         3*(b^2 - c^2)^2)/(4*a*S^2), -(c*a + S)/(c*a + 2*S)/b, 
       -(b*a + S)/(b*a + 2*S)/c}, "VPH16" -> {S/(a*(b*c + 2*S)), -b^(-1), 
       -c^(-1)}, "VPH17" -> {(a^4 - 2*(b - c)^2*a^2 + (b^2 - c^2)^2)*
        (S/(4*a^2*(b*c + 2*S))), (-c)*a - S, (-b)*a - S}, 
     "VPH18" -> {(2*a*b*c + (b + c)*S)*(S/(b*c + 2*S)), 
       (-(c*a + S))*((2*b*a + S)/b), (-(b*a + S))*((2*c*a + S)/c)}, 
     "VPH19" -> {4*(S/a), -(8*(a + c)*S*b - 3*a^4 + 6*(b^2 + c^2)*a^2 + 
          4*b^2*c*a - 3*(b^2 - c^2)^2)/(b*(b*a + S)), 
       -(8*(a + b)*S*c - 3*a^4 + 6*(b^2 + c^2)*a^2 + 4*b*c^2*a - 
          3*(b^2 - c^2)^2)/(c*(c*a + S))}, 
     "VPH24" -> {0, 1/((c*a + S)*b), 1/((b*a + S)*c)}, 
     "VPH25" -> {4*S^2*((b*c + S)/((b*c + 2*S)*a)), 
       -(8*(a + c)*S*b - 3*a^4 + 6*(b^2 + c^2)*a^2 + 4*b^2*c*a - 
          3*(b^2 - c^2)^2)/b, -(8*(a + b)*S*c - 3*a^4 + 6*(b^2 + c^2)*a^2 + 
          4*b*c^2*a - 3*(b^2 - c^2)^2)/c}, 
     "VPH30" -> {-(a + R)^(-1), 1/b, 1/c}, 
     "VPH31" -> {(2*(2*c + 3*R)*b + 6*R*c + 8*R^2)/a, -2*R - c, -b - 2*R}]
