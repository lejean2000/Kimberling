AlgoKimberlingWeights200[k_, a_, b_, c_] := 
    Module[{w, a2, a3, a4, a5, a6, a7, a8, a9, a10, b2, b3, b4, b5, b6, b7, 
      b8, b9, b10, c2, c3, c4, c5, c6, c7, c8, c9, c10, Q, R, S, T, U, V, 
      angleA, angleB, angleC, SA, SB, SC}, a2 = a*a; a3 = a*a2; a4 = a*a3; 
      a5 = a*a4; a6 = a*a5; a7 = a*a6; a8 = a*a7; a9 = a*a8; a10 = a*a9; 
      b2 = b*b; b3 = b*b2; b4 = b*b3; b5 = b*b4; b6 = b*b5; b7 = b*b6; 
      b8 = b*b7; b9 = b*b8; b10 = b*b9; c2 = c*c; c3 = c*c2; c4 = c*c3; 
      c5 = c*c4; c6 = c*c5; c7 = c*c6; c8 = c*c7; c9 = c*c8; c10 = c*c9; 
      R = b2 + c2; Q = (b2 - c2)*(b2 - c2); 
      angleA = ArcCos[0.5*((b2 + c2 - a2)/b/c)]; 
      angleB = ArcCos[0.5*((-b2 + c2 + a2)/a/c)]; 
      angleC = ArcCos[0.5*((b2 - c2 + a2)/b/a)]; T = a2 - b2 - c2; 
      U = a2 + b2 - c2; V = a2 - b2 + c2; 
      S = Sqrt[(a + b + c)*(-a + b + c)*(a - b + c)*(a + b - c)]; 
      SA = (b + c - a)/2; SB = (a + c - b)/2; SC = (b + a - c)/2; 
      Which[k == 1, a, k == 2, 1, k == 3, a2*T, k == 4, -a4 + Q, k == 5, 
       Q - a2*R, k == 6, a2, k == 7, -((a + b - c)*(a - b + c)), k == 8, 
       -a + b + c, k == 9, a*(a - b - c), k == 10, b + c, k == 11, 
       (-(a - b - c))*(b - c)^2, k == 12, (-(a + b - c))*(a - b + c)*
        (b + c)^2, k == 13, a4 + a2*b2 - 2*b4 + a2*c2 + 4*b2*c2 - 2*c4 + 
        Sqrt[3]*a2*S, k == 14, -a4 - a2*b2 + 2*b4 - a2*c2 - 4*b2*c2 + 2*c4 + 
        Sqrt[3]*a2*S, k == 15, Sqrt[3]*a2*T - a2*S, k == 16, 
       Sqrt[3]*a2*T + a2*S, k == 17, 1/(-a2 + b2 + c2 + Sqrt[3]*S), k == 18, 
       1/(-a2 + b2 + c2 - Sqrt[3]*S), k == 19, a*(a4 - Q), k == 20, 
       3*a4 - Q - 2*a2*R, k == 21, a*(a + b)*(a - b - c)*(a + c), k == 22, 
       a2*(a4 - b4 - c4), k == 23, a2*(a4 - b4 + b2*c2 - c4), k == 24, 
       a2*U*V*(a4 + b4 + c4 - 2*a2*R), k == 25, a2*U*V, k == 26, 
       a2*(a8 - 2*a6*R - Q*(b4 + c4) + 2*a2*(b6 + c6)), k == 27, 
       (a + b)*(a + c)*U*V, k == 28, a*(a + b)*(a + c)*U*V, k == 29, 
       (a + b)*(a - b - c)*(a + c)*U*V, k == 30, 2*a4 - (b2 - c2)^2 - 
        a2*(b2 + c2), k == 31, a3, k == 32, a4, k == 33, a*(a - b - c)*U*V, 
       k == 34, a*(a + b - c)*(a - b + c)*U*V, k == 35, 
       a2*(a2 - b2 - b*c - c2), k == 36, a2*(a2 - b2 + b*c - c2), k == 37, 
       a*(b + c), k == 38, a*R, k == 39, a2*R, k == 40, 
       a*(a3 + a2*(b + c) - (b - c)^2*(b + c) - a*(b + c)^2), k == 41, 
       a3*(a - b - c), k == 42, a2*(b + c), k == 43, a*(-(b*c) + a*(b + c)), 
       k == 44, a*(2*a - b - c), k == 45, a*(a - 2*(b + c)), k == 46, 
       a*(a3 + a2*(b + c) - (b - c)^2*(b + c) - a*R), k == 47, 
       a3*(a4 + b4 + c4 - 2*a2*R), k == 48, a3*T, k == 49, 
       a4*T*(a4 + b4 - b2*c2 + c4 - 2*a2*R), k == 50, a4*(a2 - b2 - b*c - c2)*
        (a2 - b2 + b*c - c2), k == 51, a2*(-Q + a2*R), k == 52, 
       a2*(a4 + b4 + c4 - 2*a2*R)*(-Q + a2*R), k == 53, U*V*(-Q + a2*R), 
       k == 54, a2*(a4 + b4 - b2*c2 - a2*(2*b2 + c2))*(a4 - b2*c2 + c4 - 
         a2*(b2 + 2*c2)), k == 55, a2*(a - b - c), k == 56, 
       a2*(a + b - c)*(a - b + c), k == 57, a*(a + b - c)*(a - b + c), 
       k == 58, a2*(a + b)*(a + c), k == 59, a2*(a - b)^2*(a - c)^2*
        (a + b - c)*(a - b + c), k == 60, a2*(a + b)^2*(a - b - c)*(a + c)^2, 
       k == 61, a2*T - Sqrt[3]*a2*S, k == 62, a2*T + Sqrt[3]*a2*S, k == 63, 
       a*T, k == 64, a2*(a4 + b4 + 2*b2*c2 - 3*c4 - 2*a2*(b2 - c2))*
        (a4 - 3*b4 + 2*b2*c2 + c4 + 2*a2*(b2 - c2)), k == 65, 
       a*(a + b - c)*(a - b + c)*(b + c), k == 66, -a8 + (b4 - c4)^2, 
       k == 67, -((a4 - a2*b2 + b4 - c4)*(a4 - b4 - a2*c2 + c4)), k == 68, 
       -(T*(a4 - 2*a2*b2 + Q)*(a4 - 2*a2*c2 + Q)), k == 69, -a2 + b2 + c2, 
       k == 70, -((a8 + 2*a4*b4 - 2*a6*R + (b2 - c2)^3*R - 2*a2*(b6 - c6))*
         (a8 + 2*a4*c4 - 2*a6*R - (b2 - c2)^3*R + 2*a2*(b6 - c6))), k == 71, 
       a2*(b + c)*T, k == 72, -(a*(b + c)*T), k == 73, 
       a2*(a + b - c)*(a - b + c)*(b + c)*T, k == 74, 
       a2*(a4 - 2*b4 + b2*c2 + c4 + a2*(b2 - 2*c2))*(a4 + b4 + b2*c2 - 2*c4 + 
         a2*(-2*b2 + c2)), k == 75, b*c, k == 76, b2*c2, k == 77, 
       a*(a + b - c)*(a - b + c)*T, k == 78, a*(a - b - c)*T, k == 79, 
       1/(b2 + c2 - a2 + b*c), k == 80, -((a2 - a*b + b2 - c2)*
         (a2 - b2 - a*c + c2)), k == 81, a*(a + b)*(a + c), k == 82, 
       a*(a2 + b2)*(a2 + c2), k == 83, (a2 + b2)*(a2 + c2), k == 84, 
       a*(a3 + a2*(b - c) - a*(b - c)^2 - (b - c)*(b + c)^2)*
        (a3 - a*(b - c)^2 + a2*(-b + c) + (b - c)*(b + c)^2), k == 85, 
       b*(-a + b - c)*(a + b - c)*c, k == 86, (a + b)*(a + c), k == 87, 
       a*(a*(b - c) - b*c)*(a*(b - c) + b*c), k == 88, 
       a*(a + b - 2*c)*(a - 2*b + c), k == 89, a*(2*a + 2*b - c)*
        (2*a - b + 2*c), k == 90, a*(a3 + a2*(b - c) - (b - c)*(b + c)^2 - 
         a*R)*(a3 + a2*(-b + c) + (b - c)*(b + c)^2 - a*R), k == 91, 
       b*c*(a4 - 2*a2*b2 + Q)*(a4 - 2*a2*c2 + Q), k == 92, b*c*(-a4 + Q), 
       k == 93, b2*c2*(-V)*U*(a4 + Q - a2*(2*b2 + c2))*
        (a4 + Q - a2*(b2 + 2*c2)), k == 94, b2*c2*(a2 - a*b + b2 - c2)*
        (a2 + a*b + b2 - c2)*(-a2 + b2 - a*c - c2)*(-a2 + b2 + a*c - c2), 
       k == 95, (a4 + b4 - b2*c2 - a2*(2*b2 + c2))*(a4 - b2*c2 + c4 - 
         a2*(b2 + 2*c2)), k == 96, (a4 - 2*a2*b2 + Q)*(a4 - 2*a2*c2 + Q)*
        (a4 + b4 - b2*c2 - a2*(2*b2 + c2))*(a4 - b2*c2 + c4 - 
         a2*(b2 + 2*c2)), k == 97, a2*T*(a4 + b4 - b2*c2 - a2*(2*b2 + c2))*
        (a4 - b2*c2 + c4 - a2*(b2 + 2*c2)), k == 98, 
       (a4 + b4 - a2*c2 - b2*c2)*(a4 - a2*b2 - b2*c2 + c4), k == 99, 
       (a - b)*(a + b)*(a - c)*(a + c), k == 100, a*(a - b)*(a - c), 
       k == 101, a2*(a - b)*(a - c), k == 102, 
       a2*(a4 - a3*b - 2*b4 + a*b*(b - c)^2 + b3*c + b2*c2 - b*c3 + c4 + 
         a2*(b2 + b*c - 2*c2))*(a4 + b4 - a3*c - b3*c + a*(b - c)^2*c + 
         b2*c2 + b*c3 - 2*c4 + a2*(-2*b2 + b*c + c2)), k == 103, 
       a2*(a3 - 2*b3 - a2*c + b2*c + c3 + a*(b2 - c2))*
        (a3 - a2*b + b3 + b*c2 - 2*c3 + a*(-b2 + c2)), k == 104, 
       a*(a3 - a2*b + b3 - a*(b - c)^2 - b*c2)*(a3 - a*(b - c)^2 - a2*c - 
         b2*c + c3), k == 105, a*(a2 + b*(b - c) - a*c)*
        (a2 - a*b + c*(-b + c)), k == 106, a2*(a + b - 2*c)*(a - 2*b + c), 
       k == 107, (a - b)*(a + b)*(a - c)*(a + c)*(a4 - Q)^2, k == 108, 
       a*(a - b)*(a - c)*(a + b - c)*(a - b + c)*U*V, k == 109, 
       a2*(a - b)*(a - c)*(a + b - c)*(a - b + c), k == 110, 
       a2*(a - b)*(a + b)*(a - c)*(a + c), k == 111, a2*(a2 + b2 - 2*c2)*
        (a2 - 2*b2 + c2), k == 112, a2*(a - b)*(a + b)*(a - c)*(a + c)*U*V, 
       k == 113, -((2*a4 - Q - a2*R)*(a4*R + Q*R - 2*a2*(b4 - b2*c2 + c4))), 
       k == 114, -((2*a4 + Q - a2*R)*(-b4 - c4 + a2*R)), k == 115, 
       (b - c)^2*(b + c)^2, k == 116, (b - c)^2*(b2 + b*c + c2 - a*(b + c)), 
       k == 117, -((2*a4 - a2*(b - c)^2 - a3*(b + c) + a*(b - c)^2*(b + c) - 
          Q)*(-(a3*b*c*(b + c)) + a*b*(b - c)^2*c*(b + c) + a4*R + 
          Q*(b2 - b*c + c2) - a2*(b - c)^2*(2*b2 + 3*b*c + 2*c2))), k == 118, 
       -((2*a3 - a2*(b + c) - (b - c)^2*(b + c))*(b5 - b3*c2 - b2*c3 + c5 - 
          a*Q + a3*R - a2*(b3 + c3))), k == 119, 
       -((-2*a*b*c + a2*(b + c) - (b - c)^2*(b + c))*(a3*(b + c) - 
          a*(b - c)^2*(b + c) + Q - a2*R)), k == 120, 
       -((-b2 - c2 + a*(b + c))*(-2*a*b*c + a2*(b + c) + (b - c)^2*(b + c))), 
       k == 121, -((2*a - b - c)*(b3 - 2*b2*c - 2*b*c2 + c3 + a*R)), 
       k == 122, (b - c)^2*(b + c)^2*(T*T)*(-3*a4 + Q + 2*a2*R), k == 123, 
       -((a - b - c)*(b - c)^2*T*(a4 + 2*a2*b*c - 2*a*b*c*(b + c) - Q)), 
       k == 124, (a - b - c)*(b - c)^2*(-b3 - a*b*c - c3 + a2*(b + c)), 
       k == 125, (b - c)^2*(b + c)^2*(-T), k == 126, 
       -((2*a2 - b2 - c2)*(b4 - 4*b2*c2 + c4 + a2*R)), k == 127, 
       (b - c)^2*(b + c)^2*(-T)*(-a4 + b4 + c4), k == 128, 
       -((a2 - b2 - b*c - c2)*(a2 - b2 + b*c - c2)*(-Q + a2*R)*
         (2*a8 + (b2 - c2)^4 - 4*a6*R - 2*a2*Q*R + 3*a4*(b4 + c4))), 
       k == 129, -(a2*(-Q + a2*R)*(a8 + b2*c2*Q - 2*a6*R + 
          a4*(b4 + b2*c2 + c4))*(a8*(b4 + c4) - 4*a6*(b6 + c6) + 
          Q*(b8 + c8) + a4*(6*b8 - 2*b6*c2 - 2*b4*c4 - 2*b2*c6 + 6*c8) - 
          4*a2*(b10 - b8*c2 - b2*c8 + c10))), k == 130, 
       a2*(b - c)^2*(b + c)^2*(T*T)*(-Q + a2*R)*(a8 - b2*c2*Q - 2*a6*R + 
         a4*(b4 + 3*b2*c2 + c4)), k == 131, 
       -(T*(2*a8 + (b2 - c2)^4 - 3*a6*R - a2*Q*R + a4*(b2 + c2)^2)*
         (a4*R + Q*R - 2*a2*(b4 - b2*c2 + c4))), k == 132, 
       -(U*V*(-b4 - c4 + a2*R)*(2*a6 - a4*R - Q*R)), k == 133, 
       -(U*V*(2*a4 - Q - a2*R)*(a6*R + 3*a2*Q*R + 
          a4*(-3*b4 + 4*b2*c2 - 3*c4) - Q*(b4 + 4*b2*c2 + c4))), k == 134, 
       a2*(b - c)^2*(b + c)^2*(a4 + b4 + c4 - 2*a2*R)^2*(-Q + a2*R)*
        (a10*a2 - b2*c2*(b2 - c2)^4 - 4*a10*R + 2*a2*b2*c2*Q*R + 
         a8*(6*b4 + 5*b2*c2 + 6*c4) + a6*(-4*b6 + 2*b4*c2 + 2*b2*c4 - 4*c6) + 
         a4*(b8 - 4*b6*c2 + 2*b4*c4 - 4*b2*c6 + c8)), k == 135, 
       (b - c)^2*(b + c)^2*(-V)*U*(a4 + b4 + c4 - 2*a2*R)*
        (-a6 + 3*a4*R + Q*R + a2*(-3*b4 + 2*b2*c2 - 3*c4)), k == 136, 
       (b - c)^2*(b + c)^2*(-V)*U*(a4 + b4 + c4 - 2*a2*R), k == 137, 
       (b - c)^2*(b + c)^2*(a4 + b4 - b2*c2 + c4 - 2*a2*R)*(Q - a2*R), 
       k == 138, -(U*V*(-Q + a2*R)*(2*a8 - 4*a6*R + 2*a2*Q*R - Q*(b4 + c4) + 
          a4*(b4 + 4*b2*c2 + c4))*(a8 - 2*a6*R - 2*a2*Q*R + 
          Q*(b4 + 3*b2*c2 + c4) + a4*(2*b4 - b2*c2 + 2*c4))), k == 139, 
       (b - c)^2*(b + c)^2*(-V)*U*(a4 + b4 + c4 - 2*a2*R)*(Q - a2*R)*
        (a10*a2 - 4*a10*R + (b2 - c2)^4*(b4 + b2*c2 + c4) + 
         a8*(7*b4 + 11*b2*c2 + 7*c4) - 2*a2*Q*(2*b6 + b4*c2 + b2*c4 + 2*c6) - 
         2*a6*(4*b6 + 5*b4*c2 + 5*b2*c4 + 4*c6) + 
         a4*(7*b8 + 2*b4*c4 + 7*c8)), k == 140, 2*a4 + Q - 3*a2*R, k == 141, 
       b2 + c2, k == 142, (b - c)^2 - a*(b + c), k == 143, 
       a2*(a4 + b4 - b2*c2 + c4 - 2*a2*R)*(-Q + a2*R), k == 144, 
       3*a2 - (b - c)^2 - 2*a*(b + c), k == 145, 3*a - b - c, k == 146, 
       -a10 - a8*R + (b2 - c2)^4*R + a2*Q*(b4 + 9*b2*c2 + c4) + 
        a6*(8*b4 - 9*b2*c2 + 8*c4) + a4*(-8*b6 + 6*b4*c2 + 6*b2*c4 - 8*c6), 
       k == 147, -a8 + b8 - b6*c2 - b2*c6 + c8 - a6*R + 
        a4*(2*b4 + 3*b2*c2 + 2*c4) - a2*(b6 + b4*c2 + b2*c4 + c6), k == 148, 
       -a4 + b4 - 3*b2*c2 + c4 + a2*R, k == 149, -a3 + a2*(b + c) + 
        (b - c)^2*(b + c) - a*(b2 - b*c + c2), k == 150, 
       -a4 - a2*b*c + a3*(b + c) - a*(b - c)^2*(b + c) + 
        (b - c)^2*(b2 + b*c + c2), k == 151, -a10 + a9*(b + c) - 
        12*a5*b*(b - c)^2*c*(b + c) - a*(b - c)^6*(b + c)^3 + 
        (b2 - c2)^4*(b2 - b*c + c2) - a8*(b2 + 3*b*c + c2) - 
        4*a4*Q*(2*b2 - 3*b*c + 2*c2) - 2*a7*(b3 - 3*b2*c - 3*b*c2 + c3) + 
        2*a3*(b - c)^4*(b3 + 5*b2*c + 5*b*c2 + c3) + 
        a2*Q*(b4 - 6*b3*c + 14*b2*c2 - 6*b*c3 + c4) + 
        2*a6*(4*b4 - b3*c - 8*b2*c2 - b*c3 + 4*c4), k == 152, 
       -a8 + a7*(b + c) - a*(b - c)^4*(b + c)^3 + a4*b*c*(b2 - 6*b*c + c2) + 
        (b - c)^4*(b + c)^2*(b2 + b*c + c2)*((-a6)*(2*b2 + b*c + 2*c2)) + 
        a5*(5*b3 - b2*c - b*c2 + 5*c3) - a3*(b - c)^2*(5*b3 + 11*b2*c + 
          11*b*c2 + 5*c3)*((Plus[a2])*(b - c)^2*(2*b4 + 5*b3*c + 10*b2*c2 + 
           5*b*c3 + 2*c4)), k == 153, -a7 + a6*(b + c) + 
        (b - c)^4*(b + c)^3 + a5*(b2 - 7*b*c + c2) - a*Q*(b2 - 5*b*c + c2) - 
        a4*(b3 - 5*b2*c - 5*b*c2 + c3) - a2*(b - c)^2*(b3 + 7*b2*c + 7*b*c2 + 
          c3) + a3*(b4 + 2*b3*c - 10*b2*c2 + 2*b*c3 + c4), k == 154, 
       a2*(3*a4 - Q - 2*a2*R), k == 155, a2*(a8 - 4*a6*R + (b4 - c4)^2 + 
         a4*(6*b4 + 4*b2*c2 + 6*c4) - 4*a2*(b6 + c6)), k == 156, 
       a2*(a8 + b2*c2*Q - 3*a6*R + a4*(3*b4 + 2*b2*c2 + 3*c4) - 
         a2*(b6 + c6)), k == 157, a2*(a6 - a4*R - Q*R + a2*(b4 + c4)), 
       k == 158, b*c*(a4 - Q)^2, k == 159, 
       a2*(a6 + a4*R - Q*R - a2*(b2 + c2)^2), k == 160, 
       a4*(-b4 - b2*c2 - c4 + a2*R), k == 161, 
       a2*(a10 - a8*R - (b2 - c2)^4*R + a2*Q*(b4 + c4) - 
         2*a6*(b4 + b2*c2 + c4) + 2*a4*(b6 + b4*c2 + b2*c4 + c6)), k == 162, 
       a*(a - b)*(a + b)*(a - c)*(a + c)*U*V, k == 163, 
       a3*(a - b)*(a + b)*(a - c)*(a + c), k == 164, 
       a*((-a)*(a + b - c)*(a - b + c)*Sqrt[(-b)*(a - b - c)*c*(a + b + c)] + 
         b*(a + b - c)*(-a + b + c)*Sqrt[a*c*(a - b + c)*(a + b + c)] + 
         c*(a - b + c)*(-a + b + c)*Sqrt[a*b*(a + b - c)*(a + b + c)]), 
       k == 165, a*(3*a2 - (b - c)^2 - 2*a*(b + c)), k == 166, 
       a*(a4 - 4*a3*b + 6*a2*b2 - 4*a*b3 + b4 - 4*a3*c - 4*a2*b*c + 
         4*a*b2*c + 4*b3*c + 6*a2*c2 + 4*a*b*c2 - 10*b2*c2 - 4*a*c3 + 
         4*b*c3 + c4 - 2*(a - b + c)*(-a + b + c)*Sqrt[a*(-a + b + c)]*
          Sqrt[b*(a - b + c)] - 2*(a + b - c)*(-a + b + c)*
          Sqrt[a*(-a + b + c)]*Sqrt[c*(a + b - c)] + 2*(a + b - c)*
          (a - b + c)*Sqrt[b*(a - b + c)]*Sqrt[c*(a + b - c)]), k == 167, 
       a*((a2 - 2*a*b + b2 - 2*a*c - 2*b*c + c2)*Sqrt[a*(-a + b + c)] + 
         (a2 - 2*a*b + b2 + 2*a*c + 2*b*c - 3*c2)*Sqrt[b*(a - b + c)] + 
         (a2 + 2*a*b - 3*b2 - 2*a*c + 2*b*c + c2)*Sqrt[c*(a + b - c)]), 
       k == 168, a*((-a + b + c)*S - 
         2*a*Sqrt[-(b*(a - b - c)*c*(a + b + c))] + 
         2*(a - c)*Sqrt[a*c*(a - b + c)*(a + b + c)] + 
         2*(a - b)*Sqrt[a*b*(a + b - c)*(a + b + c)]), k == 169, 
       a*(a3 - a2*(b + c) - (b - c)^2*(b + c) + a*R), k == 170, 
       a*(-(b*(b - c)^4*c) + a5*(b + c) + a*(b - c)^2*(b + c)^3 - 
         2*a2*(b - c)^2*(2*b2 + 3*b*c + 2*c2) - a4*(4*b2 + b*c + 4*c2) + 
         a3*(6*b3 - 2*b2*c - 2*b*c2 + 6*c3)), k == 171, a3 + a*b*c, k == 172, 
       a4 + a2*b*c, k == 174, a*Sqrt[b*(a - b + c)]*Sqrt[c*(a + b - c)], 
       k == 175, -2*a*(a - b - c)*(a + b - c)*(a - b + c) - 
        (a + b - c)*(a - b + c)*S, k == 176, 
       2*a*(a - b - c)*(a + b - c)*(a - b + c) - (a + b - c)*(a - b + c)*S, 
       k == 177, (a + b - c)*(a - b + c)*Sqrt[a*(-a + b + c)]*
        (Sqrt[b*(a - b + c)] + Sqrt[c*(a + b - c)]), k == 178, 
       Sqrt[b*(a - b + c)] + Sqrt[c*(a + b - c)], k == 179, 
       b*c*(2*a*c + Sqrt[a*c*(a - b + c)*(a + b + c)])^2*
        (2*a*b + Sqrt[a*b*(a + b - c)*(a + b + c)])^2, k == 180, 
       a*((c2*(2*a*b + Sqrt[a*b*(a + b - c)*(a + b + c)]))/
          ((2*b*c + Sqrt[-(b*(a - b - c)*c*(a + b + c))])*
            (2*a*c + Sqrt[a*c*(a - b + c)*(a + b + c)]) + 
           2*c2*(2*a*b + Sqrt[a*b*(a + b - c)*(a + b + c)])) + 
         (b2*(2*a*c + Sqrt[a*c*(a - b + c)*(a + b + c)]))/
          (2*b2*(2*a*c + Sqrt[a*c*(a - b + c)*(a + b + c)]) + 
           (2*b*c + Sqrt[-(b*(a - b - c)*c*(a + b + c))])*
            (2*a*b + Sqrt[a*b*(a + b - c)*(a + b + c)])) - 
         (a2*(2*b*c + Sqrt[-(b*(a - b - c)*c*(a + b + c))]))/
          (2*a2*(2*b*c + Sqrt[-(b*(a - b - c)*c*(a + b + c))]) + 
           (2*a*c + Sqrt[a*c*(a - b + c)*(a + b + c)])*
            (2*a*b + Sqrt[a*b*(a + b - c)*(a + b + c)]))), k == 181, 
       a2*(a + b - c)*(a - b + c)*(b + c)^2, k == 182, 
       a6 - 2*a2*b2*c2 - a4*R, k == 183, a4 - 2*b2*c2 - a2*R, k == 184, a4*T, 
       k == 185, a2*T*(-2*a2*Q + a4*R + Q*R), k == 186, 
       a2*U*(a2 - b2 - b*c - c2)*(a2 - b2 + b*c - c2)*V, k == 187, 
       a2*(2*a2 - b2 - c2), k == 188, Sqrt[a*(-a + b + c)], k == 189, 
       -((a3 + a2*(b - c) - a*(b - c)^2 - (b - c)*(b + c)^2)*
         (a3 - a*(b - c)^2 + a2*(-b + c) + (b - c)*(b + c)^2)), k == 190, 
       (a - b)*(a - c), k == 191, a*(a3 - b3 - b2*c - b*c2 - c3 + 
         a2*(b + c) - a*(b2 + b*c + c2)), k == 192, -(b*c) + a*(b + c), 
       k == 193, 3*a2 - b2 - c2, k == 194, -(b2*c2) + a2*R, k == 195, 
       a2*(a8 - 4*a6*R + Q*(b4 + c4) + a4*(6*b4 + 5*b2*c2 + 6*c4) + 
         a2*(-4*b6 + b4*c2 + b2*c4 - 4*c6)), k == 196, 
       -((a + b - c)*(a - b + c)*U*V*(a3 + a2*(b + c) - (b - c)^2*(b + c) - 
          a*(b + c)^2)), k == 197, a2*(a4 + 2*a2*b*c - 2*a*b*c*(b + c) - Q), 
       k == 198, a2*(a3 + a2*(b + c) - (b - c)^2*(b + c) - a*(b + c)^2), 
       k == 199, a2*(a4 + a2*b*c + a3*(b + c) - (b + c)^2*(b2 - b*c + c2) - 
         a*(b3 + b2*c + b*c2 + c3)), k == 200, a*(b + c - a)^2; ]]
 
a = 6
 
b = 9
 
c = 13
 
R = (a*b*c*Sqrt[(a + b - c)*(a - b + c)*(-a + b + c)*(a + b + c)])/4
 
S = Sqrt[(a + b - c)*(a - b + c)*(-a + b + c)*(a + b + c)]/2
 
angleA = ArcCos[(-a^2 + b^2 + c^2)/(2*b*c)]
 
angleB = ArcCos[(a^2 - b^2 + c^2)/(2*a*c)]
 
angleC = ArcCos[(a^2 + b^2 - c^2)/(2*a*b)]
 
SA = (-a^2 + b^2 + c^2)/2
 
SB = (a^2 - b^2 + c^2)/2
 
SC = (a^2 + b^2 - c^2)/2
