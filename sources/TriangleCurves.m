cubicpK[{p_, q_, r_}, {u_, v_, w_}] := symmetrizeEq[u*x*(r^2*y^2 - q^2*z^2)]
 
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
 
cubicnK[{p_, q_, r_}, {u_, v_, w_}, {x1_, y1_, z1_}] := 
    Module[{c1, c2, c3, t}, t = p*y1*z1*(w*y1 + v*z1) + 
        q*z1*x1*(u*z1 + w*x1) + r*x1*y1*(v*x1 + u*y1); 
      c1 = -2*p*x1*y1*z1*(u*y*z + v*z*x + w*x*y) + x^2*t; 
      c2 = -2*q*x1*y1*z1*(u*y*z + v*z*x + w*x*y) + y^2*t; 
      c3 = -2*r*x1*y1*z1*(u*y*z + v*z*x + w*x*y) + z^2*t; 
      jacobianDeterminant[{c1, c2, c3}, {x, y, z}]]
 
jacobianDeterminant[(f_List)?VectorQ, x_List] := Det[jacobianMatrix[f, x]] /; 
     Equal @@ Dimensions /@ {f, x}
 
jacobianMatrix[(f_List)?VectorQ, x_List] := Outer[D, f, x] /; 
     Equal @@ Dimensions /@ {f, x}
 
cubicnK0[{u_, v_, w_}] := symmetrizeEq[u*x*(c^2*y^2 + b^2*z^2)]
