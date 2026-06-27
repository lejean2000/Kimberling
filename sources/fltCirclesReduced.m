fltInCircles = <|"X(3)" -> b^4*c^4*(a^4 - (b^2 - c^2)^2)^2*x^2+
 2*a^2*b^2*c^4*(a^2 + b^2 - c^2)^2*(a^4 - 2*a^2*b^2 + b^4 - c^4)*x*y+
 a^4*c^4*(a^4 - b^4 - 2*a^2*c^2 + c^4)^2*y^2+
 2*a^2*b^4*c^2*(a^2 - b^2 + c^2)^2*(a^4 - b^4 - 2*a^2*c^2 + c^4)*x*z-
 2*a^4*b^2*c^2*(-a^2 + b^2 + c^2)^2*(a^4 - (b^2 - c^2)^2)*y*z+
 a^4*b^4*(a^4 - 2*a^2*b^2 + b^4 - c^4)^2*z^2,
 "X(9)" -> b^2*(a^2 - (b - c)^2)^2*c^2*x^2+
 2*a*b*(a + b - c)^2*c^2* (a^2 - 2*a*b + b^2 - c^2)*x*y+
 a^2*c^2*(a^2 - b^2 - 2*a*c + c^2)^2* y^2+
 2*a*b^2*c*(a - b + c)^2*(a^2 - b^2 - 2*a*c + c^2)*x*z-
 2*a^2*b*(a^2 - (b - c)^2)*c*(-a + b + c)^2*y*z+
 a^2*b^2*(a^2 - 2*a*b + b^2 - c^2)^2*z^2,
 "X(10)" -> (a + b)^2*(a + c)^2*x^2-
 2*(a + b)^2*(a + c)*(b + c)*x*y+
 (a + b)^2*(b + c)^2*y^2-
 2*(a + b)*(a + c)^2*(b + c)*x*z-
 2*(a + b)*(a + c)*(b + c)^2*y*z+
 (a + c)^2*(b + c)^2*z^2,
 "X(11)" -> (a - b)^4*(a^2 - (b - c)^2)^2*(a - c)^4*x^2+
 2*(a - b)^4*(a - c)^2*(b - c)^2*(a + b - c)^2*(a^2 - 2*a*b + b^2 - c^2)*x*y+
 (a - b)^4*(b - c)^4*(a^2 - b^2 - 2*a*c + c^2)^2*y^2+
 2*(a - b)^2*(a - c)^4*(b - c)^2*(a - b + c)^2*(a^2 - b^2 - 2*a*c + c^2)*x*z+
 2*(a - b)^2*(-a^2 + (b - c)^2)*(a - c)^2*(b - c)^4* (-a + b + c)^2*y*z+
 (a - c)^4*(b - c)^4*(a^2 - 2*a*b + b^2 - c^2)^2* z^2,
 "X(12)" -> (a + b)^4*(a + c)^4*(-a + b + c)^2*x^2+
 2*(a + b)^4*(a + c)^2*(b + c)^2*(a^2 - 2*a*b + b^2 - c^2)*x*y+
 (a + b)^4*(a - b + c)^2*(b + c)^4*y^2+
 2*(a + b)^2*(a + c)^4* (b + c)^2*(a^2 - b^2 - 2*a*c + c^2)*x*z+
 2*(a + b)^2*(-a^2 + (b - c)^2)*(a + c)^2*(b + c)^4*y*z+
 (a + b - c)^2*(a + c)^4*(b + c)^4*z^2,
 "X(19)" -> b^2*c^2*(-a^2 + b^2 + c^2)^2*x^2+
 2*a*b*c^2*(a^4 - 2*a^2*b^2 + b^4 - c^4)*x*y+
 a^2*c^2*(a^2 - b^2 + c^2)^2*y^2+
 2*a*b^2*c*(a^4 - b^4 - 2*a^2*c^2 + c^4)*x*z+
 2*a^2*b*c*(-a^4 + (b^2 - c^2)^2)*y*z+
 a^2*b^2*(a^2 + b^2 - c^2)^2*z^2,
 "X(27)" -> (b + c)^2*(-a^2 + b^2 + c^2)^2*x^2+
 2*(a + c)*(b + c)*(a^4 - 2*a^2*b^2 + b^4 - c^4)*x*y+
 (a + c)^2*(a^2 - b^2 + c^2)^2*y^2+
 2*(a + b)*(b + c)* (a^4 - b^4 - 2*a^2*c^2 + c^4)*x*z-
 2*(a + b)*(a + c)* (a^4 - (b^2 - c^2)^2)*y*z+
 (a + b)^2*(a^2 + b^2 - c^2)^2*z^2,
 "X(31)" -> b^6*c^6*x^2-
 2*a^3*b^3*c^6*x*y+
 a^6*c^6*y^2-
 2*a^3*b^6*c^3*x*z-
 2*a^6*b^3*c^3*y*z+
 a^6*b^6*z^2,
 "X(32)" -> b^8*c^8*x^2-
 2*a^4*b^4*c^8*x*y+
 a^8*c^8*y^2-
 2*a^4*b^8*c^4*x*z-
 2*a^8*b^4*c^4*y*z+
 a^8*b^8*z^2,
 "X(69)" -> (a^4 - (b^2 - c^2)^2)^2*x^2+
 2*(a^2 + b^2 - c^2)^2* (a^4 - 2*a^2*b^2 + b^4 - c^4)*x*y+
 (a^4 - b^4 - 2*a^2*c^2 + c^4)^2* y^2+
 2*(a^2 - b^2 + c^2)^2*(a^4 - b^4 - 2*a^2*c^2 + c^4)*x*z-
 2*(-a^2 + b^2 + c^2)^2*(a^4 - (b^2 - c^2)^2)*y*z+
 (a^4 - 2*a^2*b^2 + b^4 - c^4)^2*z^2,
 "X(75)" -> a^2*x^2-
 2*a*b*x*y+
 b^2*y^2-
 2*a*c*x*z-
 2*b*c*y*z+
 c^2*z^2,
 "X(76)" -> a^4*x^2-
 2*a^2*b^2*x*y+
 b^4*y^2-
 2*a^2*c^2*x*z-
 2*b^2*c^2*y*z+
 c^4*z^2,
 "X(81)" -> b^2*c^2*(b + c)^2*x^2-
 2*a*b*c^2*(a + c)*(b + c)*x* y + a^2*c^2*(a + c)^2*y^2-
 2*a*b^2*(a + b)*c*(b + c)*x*z-
 2*a^2*b*(a + b)*c*(a + c)*y*z+
 a^2*b^2*(a + b)^2*z^2,
 "X(82)" -> b^2*c^2*(b^2 + c^2)^2*x^2-
 2*a*b*c^2*(a^2 + c^2)*(b^2 + c^2)* x*y+
 a^2*c^2*(a^2 + c^2)^2*y^2-
 2*a*b^2*(a^2 + b^2)*c*(b^2 + c^2)*x* z - 2*a^2*b*(a^2 + b^2)*c*(a^2 + c^2)*y*z+
 a^2*b^2*(a^2 + b^2)^2* z^2,
 "X(83)" -> (b^2 + c^2)^2*x^2-
 2*(a^2 + c^2)*(b^2 + c^2)*x*y+
 (a^2 + c^2)^2*y^2-
 2*(a^2 + b^2)*(b^2 + c^2)*x*z-
 2*(a^2 + b^2)*(a^2 + c^2)*y*z+
 (a^2 + b^2)^2*z^2,
 "X(85)" -> a^2*(-a + b + c)^2*x^2+
 2*a*b*(a^2 - 2*a*b + b^2 - c^2)*x* y + b^2*(a - b + c)^2*y^2+
 2*a*c*(a^2 - b^2 - 2*a*c + c^2)*x*z+
 2*b*(-a^2 + (b - c)^2)*c*y*z+
 (a + b - c)^2*c^2*z^2,
 "X(86)" -> (b + c)^2*x^2-
 2*(a + c)*(b + c)*x*y+
 (a + c)^2*y^2-
 2*(a + b)*(b + c)*x*z-
 2*(a + b)*(a + c)*y*z+
 (a + b)^2*z^2,
 "X(92)" -> a^2*(-a^2 + b^2 + c^2)^2*x^2+
 2*a*b*(a^4 - 2*a^2*b^2 + b^4 - c^4)*x*y+
 b^2*(a^2 - b^2 + c^2)^2* y^2+
 2*a*c*(a^4 - b^4 - 2*a^2*c^2 + c^4)*x*z+
 2*b*c*(-a^4 + (b^2 - c^2)^2)*y*z+
 c^2*(a^2 + b^2 - c^2)^2*z^2,
 "X(100)" -> b^2*(b - c)^2*c^2*x^2+
 2*a*b*(a - c)*(b - c)*c^2*x*y+
 a^2*(a - c)^2*c^2*y^2-
 2*a*(a - b)*b^2*(b - c)*c*x*z+
 2*a^2*(a - b)*b*(a - c)*c*y*z+
 a^2*(a - b)^2*b^2*z^2,
 "X(101)" -> b^4*(b - c)^2*c^4*x^2+
 2*a^2*b^2*(a - c)*(b - c)*c^4*x*y+
 a^4*(a - c)^2*c^4*y^2-
 2*a^2*(a - b)*b^4*(b - c)*c^2*x*z+
 2*a^4*(a - b)*b^2*(a - c)*c^2*y*z+
 a^4*(a - b)^2*b^4*z^2,
 "X(141)" -> (a^2 + b^2)^2*(a^2 + c^2)^2*x^2-
 2*(a^2 + b^2)^2*(a^2 + c^2)* (b^2 + c^2)*x*y+
 (a^2 + b^2)^2*(b^2 + c^2)^2*y^2-
 2*(a^2 + b^2)*(a^2 + c^2)^2*(b^2 + c^2)*x*z-
 2*(a^2 + b^2)*(a^2 + c^2)*(b^2 + c^2)^2*y*z+
 (a^2 + c^2)^2*(b^2 + c^2)^2*z^2|> 
 
 fltCircumCircles = <|"X(1)" -> c*x*y+
 b*x*z+
 a*y*z,
 "X(3)" -> c^2*(-a^2 - b^2 + c^2)*x*y+
 b^2*(-a^2 + b^2 - c^2)*x*z+
 a^2*(a^2 - b^2 - c^2)*y*z,
 "X(4)" -> (a^4 - 2*a^2*b^2 + b^4 - c^4)*x*y+
 (a^4 - b^4 - 2*a^2*c^2 + c^4)*x*z+
 (-a^4 + (b^2 - c^2)^2)*y*z,
 "X(5)" -> (a^4 + b^4 - b^2*c^2 - a^2*(2*b^2 + c^2))*x*y+
 (a^4 - b^2*c^2 + c^4 - a^2*(b^2 + 2*c^2))*x*z+
 ((b^2 - c^2)^2 - a^2*(b^2 + c^2))*y*z,
 "X(7)" -> (a^2 - 2*a*b + b^2 - c^2)*x*y+
 (a^2 - b^2 - 2*a*c + c^2)*x* z + (-a^2 + (b - c)^2)*y*z,
 "X(8)" -> (a + b - c)*x*y+
 (a - b + c)*x*z+
 (-a + b + c)*y*z,
 "X(9)" -> c*(-a - b + c)*x*y+
 b*(-a + b - c)*x*z+
 a*(a - b - c)*y*z,
 "X(10)" -> (a + b)*x*y+
 (a + c)*x*z+
 (b + c)*y*z,
 "X(12)" -> (a + b)^2*(a^2 - 2*a*b + b^2 - c^2)*x*y+
 (a + c)^2*(a^2 - b^2 - 2*a*c + c^2)*x*z+
 (-a^2 + (b - c)^2)*(b + c)^2* y*z,
 "X(19)" -> c*(-a^4 + 2*a^2*b^2 - b^4 + c^4)*x*y+
 b*(-a^4 + b^4 + 2*a^2*c^2 - c^4)*x*z+
 a*(a^4 - (b^2 - c^2)^2)*y*z,
 "X(20)" -> (a^4 + b^4 + 2*b^2*c^2 - 3*c^4 - 2*a^2*(b^2 - c^2))*x*y+
 (a^4 - 3*b^4 + 2*b^2*c^2 + c^4 + 2*a^2*(b^2 - c^2))*x*z+
 (-3*a^4 + (b^2 - c^2)^2 + 2*a^2*(b^2 + c^2))*y*z,
 "X(21)" -> -((a + b - c)*c*(a + c)*(b + c)*x*y) - b*(a + b)*(a - b + c)*(b + c)*x*z+
 a*(a + b)*(a - b - c)*(a + c)*y*z,
 "X(22)" -> c^2*(-a^4 - b^4 + c^4)*x*y+
 b^2*(-a^4 + b^4 - c^4)*x*z+
 a^2*(a^4 - b^4 - c^4)*y*z,
 "X(23)" -> c^2*(-a^4 + a^2*b^2 - b^4 + c^4)*x*y+
 b^2*(-a^4 + b^4 + a^2*c^2 - c^4)* x*z+
 a^2*(a^4 - b^4 + b^2*c^2 - c^4)*y*z,
 "X(25)" -> c^2*(-a^4 + 2*a^2*b^2 - b^4 + c^4)*x*y+
 b^2*(-a^4 + b^4 + 2*a^2*c^2 - c^4)*x*z+
 a^2*(a^4 - (b^2 - c^2)^2)*y*z, 
 "X(27)" -> -((a + c)*(b + c)*(a^4 - 2*a^2*b^2 + b^4 - c^4)*x*y) - 
 (a + b)*(b + c)*(a^4 - b^4 - 2*a^2*c^2 + c^4)*x*z+
 (a + b)*(a + c)*(a^4 - (b^2 - c^2)^2)*y*z,
 "X(28)" -> c*(a + c)*(b + c)*(-a^4 + 2*a^2*b^2 - b^4 + c^4)*x*y+
 b*(a + b)*(b + c)*(-a^4 + b^4 + 2*a^2*c^2 - c^4)*x*z+
 a*(a + b)*(a + c)*(a^4 - (b^2 - c^2)^2)*y*z,
 "X(31)" -> c^3*x*y+
 b^3*x*z+
 a^3*y*z,
 "X(32)" -> c^4*x*y+
 b^4*x*z+
 a^4*y*z,
 "X(35)" -> c^2*(-a^2 - a*b - b^2 + c^2)*x*y+
 b^2*(-a^2 + b^2 - a*c - c^2)*x*z+
 a^2*(a^2 - b^2 - b*c - c^2)*y*z,
 "X(36)" -> c^2*(-a^2 + a*b - b^2 + c^2)*x*y+
 b^2*(-a^2 + b^2 + a*c - c^2)*x*z+
 a^2*(a^2 - b^2 + b*c - c^2)*y*z,
 "X(37)" -> (a + b)*c*x*y+
 b*(a + c)*x*z+
 a*(b + c)*y*z,
 "X(38)" -> (a^2 + b^2)*c*x*y+
 b*(a^2 + c^2)*x*z+
 a*(b^2 + c^2)*y*z,
 "X(39)" -> (a^2 + b^2)*c^2*x*y+
 b^2*(a^2 + c^2)*x*z+
 a^2*(b^2 + c^2)*y*z,
 "X(40)" -> c*(-a^3 + a^2*(b - c) + a*(b - c)^2 - (b - c)*(b + c)^2)*x*y+
 b*(-a^3 + a*(b - c)^2 + a^2*(-b + c) + (b - c)*(b + c)^2)*x*z+
 a*(a^3 + a^2*(b + c) - (b - c)^2*(b + c) - a*(b + c)^2)*y*z,
 "X(41)" -> c^3*(-a - b + c)*x*y+
 b^3*(-a + b - c)*x*z+
 a^3*(a - b - c)*y*z,
 "X(42)" -> (a + b)*c^2*x*y+
 b^2*(a + c)*x*z+
 a^2*(b + c)*y*z,
 "X(43)" -> c*(b*c + a*(-b + c))*x*y+
 b*(a*(b - c) + b*c)*x*z+
 a*(-(b*c) + a*(b + c))*y*z,
 "X(44)" -> c*(-a - b + 2*c)*x*y+
 b*(-a + 2*b - c)*x*z+
 a*(2*a - b - c)*y*z,
 "X(45)" -> c*(-2*a - 2*b + c)*x*y+
 b*(-2*a + b - 2*c)*x*z+
 a*(a - 2*(b + c))*y*z,
 "X(46)" -> c*(-a^3 + a^2*(b - c) - (b - c)*(b + c)^2 + a*(b^2 + c^2))*x*y + 
 b*(-a^3 + a^2*(-b + c) + (b - c)*(b + c)^2 + a*(b^2 + c^2))*x*z+
 a*(a^3 + a^2*(b + c) - (b - c)^2*(b + c) - a*(b^2 + c^2))*y*z,
 "X(47)" -> c^3*(a^4 - 2*a^2*c^2 + (b^2 - c^2)^2)*x*y+
 b^3*(a^4 - 2*a^2*b^2 + (b^2 - c^2)^2)*x*z+
 a^3*(a^4 + b^4 + c^4 - 2*a^2*(b^2 + c^2))*y*z,
 "X(48)" -> c^3*(-a^2 - b^2 + c^2)*x*y+
 b^3*(-a^2 + b^2 - c^2)*x*z+
 a^3*(a^2 - b^2 - c^2)*y*z,
 "X(55)" -> c^2*(-a - b + c)*x*y+
 b^2*(-a + b - c)*x*z+
 a^2*(a - b - c)*y*z,
 "X(56)" -> c^2*(-a^2 + 2*a*b - b^2 + c^2)*x*y+
 b^2*(-a^2 + b^2 + 2*a*c - c^2)*x*z+
 a^2*(a^2 - (b - c)^2)*y*z,
 "X(57)" -> c*(-a^2 + 2*a*b - b^2 + c^2)*x*y+
 b*(-a^2 + b^2 + 2*a*c - c^2)*x*z+
 a*(a^2 - (b - c)^2)*y*z,
 "X(58)" -> c^2*(a + c)*(b + c)*x*y+
 b^2*(a + b)*(b + c)*x*z+
 a^2*(a + b)*(a + c)*y*z,
 "X(59)" -> -((a - c)^2*(b - c)^2*c^2*(a^2 - 2*a*b + b^2 - c^2)*x*y) - 
 (a - b)^2*b^2*(b - c)^2*(a^2 - b^2 - 2*a*c + c^2)*x*z+
 a^2*(a - b)^2*(a^2 - (b - c)^2)*(a - c)^2*y*z,
 "X(60)" -> -((a + b - c)*c^2*(a + c)^2*(b + c)^2*x*y) - 
 b^2*(a + b)^2*(a - b + c)*(b + c)^2*x*z+
 a^2*(a + b)^2*(a - b - c)* (a + c)^2*y*z,
 "X(63)" -> -(c*(a^2 + b^2 - c^2)*x*y) - b*(a^2 - b^2 + c^2)*x*z+
 a*(a^2 - b^2 - c^2)*y*z,
 "X(65)" -> (a + b)*c*(a^2 - 2*a*b + b^2 - c^2)*x*y+
 b*(a + c)*(a^2 - b^2 - 2*a*c + c^2)*x*z+
 a*(-a^2 + (b - c)^2)*(b + c)* y*z,
 "X(66)" -> (a^8 - 2*a^4*b^4 + b^8 - c^8)*x*y+
 (a^8 - b^8 - 2*a^4*c^4 + c^8)*x*z+
 (-a^8 + (b^4 - c^4)^2)*y*z,
 "X(69)" -> (a^2 + b^2 - c^2)*x*y+
 (a^2 - b^2 + c^2)*x*z+
 (-a^2 + b^2 + c^2)*y*z,
 "X(71)" -> -((a + b)*c^2*(a^2 + b^2 - c^2)*x*y) - b^2*(a + c)*(a^2 - b^2 + c^2)*x*z-
 a^2*(b + c)*(-a^2 + b^2 + c^2)*y*z, 
 "X(72)" -> (a + b)*c*(a^2 + b^2 - c^2)*x*y+
 b*(a + c)*(a^2 - b^2 + c^2)*x*z+
 a*(b + c)*(-a^2 + b^2 + c^2)*y*z,
 "X(75)" -> a*b*x*y+
 a*c*x*z+
 b*c*y*z,
 "X(76)" -> a^2*b^2*x*y+
 a^2*c^2*x*z+
 b^2*c^2*y*z,
 "X(81)" -> c*(a + c)*(b + c)*x*y+
 b*(a + b)*(b + c)*x*z+
 a*(a + b)*(a + c)*y*z,
 "X(82)" -> c*(a^2 + c^2)*(b^2 + c^2)*x*y+
 b*(a^2 + b^2)*(b^2 + c^2)*x*z+
 a*(a^2 + b^2)*(a^2 + c^2)*y*z,
 "X(83)" -> (a^2 + c^2)*(b^2 + c^2)*x*y+
 (a^2 + b^2)*(b^2 + c^2)*x*z+
 (a^2 + b^2)*(a^2 + c^2)*y*z,
 "X(85)" -> a*b*(a^2 - 2*a*b + b^2 - c^2)*x*y+
 a*c*(a^2 - b^2 - 2*a*c + c^2)*x*z+
 b*(-a^2 + (b - c)^2)*c*y*z,
 "X(86)" -> (a + c)*(b + c)*x*y+
 (a + b)*(b + c)*x*z+
 (a + b)*(a + c)*y*z,
 "X(87)" -> (-2*a*b*c^3 + b^2*c^3 + a^2*(-(b^2*c) + c^3))*x*y+
 (-2*a*b^3*c + b^3*c^2 + a^2*(b^3 - b*c^2))*x*z+
 (a^3*(b - c)^2 - a*b^2*c^2)*y*z,
 "X(88)" -> c*(-2*a^2 + 5*a*b - 2*b^2 - a*c - b*c + c^2)*x*y+
 b*(-2*a^2 - a*b + b^2 + 5*a*c - b*c - 2*c^2)*x*z+
 a*(a^2 - 2*b^2 + 5*b*c - 2*c^2 - a*(b + c))*y*z,
 "X(89)" -> c*(-2*a^2 + 5*a*b - 2*b^2 + 2*a*c + 2*b*c + 4*c^2)*x*y+
 b*(-2*a^2 + 2*a*b + 4*b^2 + 5*a*c + 2*b*c - 2*c^2)*x*z+
 a*(4*a^2 - 2*b^2 + 5*b*c - 2*c^2 + 2*a*(b + c))*y*z,
 "X(99)" -> (a^2 - c^2)*(b^2 - c^2)*x*y-
 (a^2 - b^2)*(b^2 - c^2)*x*z+
 (a^2 - b^2)*(a^2 - c^2)*y*z,
 "X(101)" -> (a - c)*(b - c)*c^2*x*y-
 (a - b)*b^2*(b - c)*x*z+
 a^2*(a - b)*(a - c)*y*z,
 "X(109)" -> (a - c)*c^2*(-b + c)*(a^2 - 2*a*b + b^2 - c^2)*x*y+
 (a - b)*b^2*(b - c)*(a^2 - b^2 - 2*a*c + c^2)*x*z+
 a^2*(a - b)*(a^2 - (b - c)^2)*(a - c)*y*z,
 "X(110)" -> c^2*(-a^2 + c^2)*(-b^2 + c^2)*x*y+
 b^2*(-a^2 + b^2)*(b^2 - c^2)*x*z+
 a^2*(a^2 - b^2)*(a^2 - c^2)*y*z,
 "X(111)" -> c^2*(-2*a^4 - 2*b^4 - b^2*c^2 + c^4 + a^2*(5*b^2 - c^2))*x*y + 
 b^2*(-2*a^4 + b^4 - b^2*c^2 - 2*c^4 - a^2*(b^2 - 5*c^2))*x*z+
 a^2*(a^4 - 2*b^4 + 5*b^2*c^2 - 2*c^4 - a^2*(b^2 + c^2))*y*z,
 "X(112)" -> c^2*(a^2 - c^2)*(-b^2 + c^2)*(a^4 - 2*a^2*b^2 + b^4 - c^4)*x*y + 
 b^2*(a^2 - b^2)*(b^2 - c^2)*(a^4 - b^4 - 2*a^2*c^2 + c^4)*x*z+
 a^2*(a^2 - b^2)*(a^2 - c^2)*(a^4 - (b^2 - c^2)^2)*y*z,
 "X(115)" -> (a^2 - b^2)^2*x*y+
 (a^2 - c^2)^2*x*z+
 (b^2 - c^2)^2*y*z,
 "X(116)" -> (a - b)^2*(a^2 + a*(b - c) + b*(b - c))*x*y+
 (a - c)^2*(a^2 + a*(-b + c) + c*(-b + c))*x*z+
 (b - c)^2*(b^2 + b*c + c^2 - a*(b + c))*y*z,
 "X(125)" -> (a^2 - b^2)^2*(a^2 + b^2 - c^2)*x*y+
 (a^2 - c^2)^2*(a^2 - b^2 + c^2)*x*z-
 (a^2 - b^2 - c^2)*(b^2 - c^2)^2* y*z,
 "X(140)" -> (a^4 + b^4 - 3*b^2*c^2 + 2*c^4 - a^2*(2*b^2 + 3*c^2))*x*y+
 (a^4 + 2*b^4 - 3*b^2*c^2 + c^4 - a^2*(3*b^2 + 2*c^2))*x*z+
 (2*a^4 + (b^2 - c^2)^2 - 3*a^2*(b^2 + c^2))*y*z,
 "X(141)" -> (a^2 + b^2)*x*y+
 (a^2 + c^2)*x*z+
 (b^2 + c^2)*y*z,
 "X(142)" -> (a^2 + b*(b - c) - a*(2*b + c))*x*y+
 (a^2 + c*(-b + c) - a*(b + 2*c))*x*z+
 ((b - c)^2 - a*(b + c))*y*z,
 "X(144)" -> (a^2 - 2*a*b + b^2 + 2*a*c + 2*b*c - 3*c^2)*x*y+
 (a^2 - 3*b^2 + 2*a*(b - c) + 2*b*c + c^2)*x*z+
 (-3*a^2 + (b - c)^2 + 2*a*(b + c))*y*z,
 "X(145)" -> (a + b - 3*c)*x*y+
 (a - 3*b + c)*x*z+
 (-3*a + b + c)*y*z|> 
 
 fltCentralCircles = <|"anticomplementary circle" -> a^2*x^2+
 a^2*x*y+
 b^2*x*y+
 c^2*x*y+
 b^2*y^2+
 a^2*x*z+
 b^2*x*z+
 c^2*x*z+
 a^2*y*z+
 b^2*y*z+
 c^2*y*z+
 c^2*z^2,
 "Bevan circle" -> b*c*x^2+
 a*c*x*y+
 b*c*x*y+
 c^2*x*y+
 a*c*y^2+
 a*b*x*z+
 b^2*x*z+
 b*c*x*z+
 a^2*y*z+
 a*b*y*z+
 a*c*y*z+
 a*b*z^2,
 "1st Brocard circle" -> b^2*c^2*x^2-
 c^4*x*y+
 a^2*c^2*y^2-
 b^4*x*z-
 a^4*y*z+
 a^2*b^2*z^2,
 "2nd Brocard circle" -> a^2*b^2*c^2*x^2+
 a^2*b^2*c^2*x*y-
 a^2*c^4*x*y-
 b^2*c^4*x*y+
 a^2*b^2*c^2*y^2-
 a^2*b^4*x*z+
 a^2*b^2*c^2*x*z-
 b^4*c^2*x*z-
 a^4*b^2*y*z-
 a^4*c^2*y*z+
 a^2*b^2*c^2*y*z+
 a^2*b^2*c^2*z^2,
 "circumcircle" -> c^2*x*y+
 b^2*x*z+
 a^2*y*z,
 "Conway circle" -> a*b*x^2+
 a*c*x^2+
 2*a*b*x*y+
 a*c*x*y+
 b*c*x*y+
 c^2*x*y+
 a*b*y^2+
 b*c*y^2+
 a*b*x*z+
 b^2*x*z+
 2*a*c*x*z+
 b*c*x*z+
 a^2*y*z+
 a*b*y*z+
 a*c*y*z+
 2*b*c*y*z+
 a*c*z^2+
 b*c*z^2,
 "cosine circle" -> 2*a^2*b^2*c^2*x^2-
 2*b^4*c^2*x^2-
 2*b^2*c^4*x^2-
 a^4*c^2*x*y+
 6*a^2*b^2*c^2*x*y-
 b^4*c^2*x*y+
 c^6*x*y-
 2*a^4*c^2*y^2+
 2*a^2*b^2*c^2*y^2-
 2*a^2*c^4*y^2-
 a^4*b^2*x*z+
 b^6*x*z+
 6*a^2*b^2*c^2*x*z-
 b^2*c^4*x*z+
 a^6*y*z-
 a^2*b^4*y*z+
 6*a^2*b^2*c^2*y*z-
 a^2*c^4*y*z-
 2*a^4*b^2*z^2-
 2*a^2*b^4*z^2+
 2*a^2*b^2*c^2*z^2,
 "DeLongchamps circle" -> 2*a^2*x^2+
 2*a^2*x*y+
 2*b^2*x*y-
 2*c^2*x*y+
 2*b^2*y^2+
 2*a^2*x*z-
 2*b^2*x*z+
 2*c^2*x*z-
 2*a^2*y*z+
 2*b^2*y*z+
 2*c^2*y*z+
 2*c^2*z^2,
 "excircles-radical circle" -> a^2*x^2-
 b^2*x^2+
 2*b*c*x^2-
 c^2*x^2+
 2*a*c*x*y+
 2*b*c*x*y+
 2*c^2*x*y-
 a^2*y^2+
 b^2*y^2+
 2*a*c*y^2-
 c^2*y^2+
 2*a*b*x*z+
 2*b^2*x*z+
 2*b*c*x*z+
 2*a^2*y*z+
 2*a*b*y*z+
 2*a*c*y*z-
 a^2*z^2+
 2*a*b*z^2-
 b^2*z^2+
 c^2*z^2,
 "1st Lemoine circle" -> b^4*c^2*x^2+
 b^2*c^4*x^2-
 2*a^2*b^2*c^2*x*y-
 a^2*c^4*x*y-
 b^2*c^4*x*y-
 c^6*x*y+
 a^4*c^2*y^2+
 a^2*c^4*y^2-
 a^2*b^4*x*z-
 b^6*x*z-
 2*a^2*b^2*c^2*x*z-
 b^4*c^2*x*z-
 a^6*y*z-
 a^4*b^2*y*z-
 a^4*c^2*y*z-
 2*a^2*b^2*c^2*y*z+
 a^4*b^2*z^2+
 a^2*b^4*z^2,
 "Fuhrmann circle" -> a^3*x^2-
 a*b^2*x^2-
 a*c^2*x^2+
 a^3*x*y-
 a^2*b*x*y-
 a*b^2*x*y+
 b^3*x*y+
 c^3*x*y-
 a^2*b*y^2+
 b^3*y^2-
 b*c^2*y^2+
 a^3*x*z+
 b^3*x*z-
 a^2*c*x*z-
 a*c^2*x*z+
 c^3*x*z+
 a^3*y*z+
 b^3*y*z-
 b^2*c*y*z-
 b*c^2*y*z+
 c^3*y*z-
 a^2*c*z^2-
 b^2*c*z^2+
 c^3*z^2,
 "incircle" -> a^2*x^2-
 2*a*b*x^2+
 b^2*x^2-
 2*a*c*x^2+
 2*b*c*x^2+
 c^2*x^2+
 2*a^2*x*y-
 4*a*b*x*y+
 2*b^2*x*y-
 2*c^2*x*y+
 a^2*y^2-
 2*a*b*y^2+
 b^2*y^2+
 2*a*c*y^2-
 2*b*c*y^2+
 c^2*y^2+
 2*a^2*x*z-
 2*b^2*x*z-
 4*a*c*x*z+
 2*c^2*x*z-
 2*a^2*y*z+
 2*b^2*y*z-
 4*b*c*y*z+
 2*c^2*y*z+
 a^2*z^2+
 2*a*b*z^2+
 b^2*z^2-
 2*a*c*z^2-
 2*b*c*z^2+
 c^2*z^2,
 "circumcircle of the Johnson triangle" -> a^6*x^2-
 2*a^4*b^2*x^2+
 a^2*b^4*x^2-
 2*a^4*c^2*x^2+
 a^2*b^2*c^2*x^2+
 a^2*c^4*x^2+
 a^6*x*y-
 a^4*b^2*x*y-
 a^2*b^4*x*y+
 b^6*x*y-
 a^4*c^2*x*y-
 b^4*c^2*x*y-
 a^2*c^4*x*y-
 b^2*c^4*x*y+
 c^6*x*y+
 a^4*b^2*y^2-
 2*a^2*b^4*y^2+
 b^6*y^2+
 a^2*b^2*c^2*y^2-
 2*b^4*c^2*y^2+
 b^2*c^4*y^2+
 a^6*x*z-
 a^4*b^2*x*z-
 a^2*b^4*x*z+
 b^6*x*z-
 a^4*c^2*x*z-
 b^4*c^2*x*z-
 a^2*c^4*x*z-
 b^2*c^4*x*z+
 c^6*x*z+
 a^6*y*z-
 a^4*b^2*y*z-
 a^2*b^4*y*z+
 b^6*y*z-
 a^4*c^2*y*z-
 b^4*c^2*y*z-
 a^2*c^4*y*z-
 b^2*c^4*y*z+
 c^6*y*z+
 a^4*c^2*z^2+
 a^2*b^2*c^2*z^2+
 b^4*c^2*z^2-
 2*a^2*c^4*z^2-
 2*b^2*c^4*z^2+
 c^6*z^2,
 "Longuet-Higgins circle" -> b^2*x^2+
 2*b*c*x^2+
 c^2*x^2+
 a^2*x*y+
 b^2*x*y+
 2*a*c*x*y+
 2*b*c*x*y+
 c^2*x*y+
 a^2*y^2+
 2*a*c*y^2+
 c^2*y^2+
 a^2*x*z+
 2*a*b*x*z+
 b^2*x*z+
 2*b*c*x*z+
 c^2*x*z+
 a^2*y*z+
 2*a*b*y*z+
 b^2*y*z+
 2*a*c*y*z+
 c^2*y*z+
 a^2*z^2+
 2*a*b*z^2+
 b^2*z^2,
 "Moses-Longuet-Higgins circle" -> a^2*x^2-
 2*a*b*x^2+
 b^2*x^2-
 2*a*c*x^2+
 2*b*c*x^2+
 c^2*x^2+
 2*a^2*x*y-
 4*a*b*x*y+
 2*b^2*x*y+
 c^2*x*y+
 a^2*y^2-
 2*a*b*y^2+
 b^2*y^2+
 2*a*c*y^2-
 2*b*c*y^2+
 c^2*y^2+
 2*a^2*x*z+
 b^2*x*z-
 4*a*c*x*z+
 2*c^2*x*z+
 a^2*y*z+
 2*b^2*y*z-
 4*b*c*y*z+
 2*c^2*y*z+
 a^2*z^2+
 2*a*b*z^2+
 b^2*z^2-
 2*a*c*z^2-
 2*b*c*z^2+
 c^2*z^2,
 "nine-point circle" -> a^2*x^2-
 b^2*x^2-
 c^2*x^2+
 2*c^2*x*y-
 a^2*y^2+
 b^2*y^2-
 c^2*y^2+
 2*b^2*x*z+
 2*a^2*y*z-
 a^2*z^2-
 b^2*z^2+
 c^2*z^2,
 "orthocentroidal circle" -> a^2*x^2-
 b^2*x^2-
 c^2*x^2+
 c^2*x*y-
 a^2*y^2+
 b^2*y^2-
 c^2*y^2+
 b^2*x*z+
 a^2*y*z-
 a^2*z^2-
 b^2*z^2+
 c^2*z^2,
 "polar circle" -> a^2*x^2-
 b^2*x^2-
 c^2*x^2-
 a^2*y^2+
 b^2*y^2-
 c^2*y^2-
 a^2*z^2-
 b^2*z^2+
 c^2*z^2,
 "Spieker circle" -> 3*a^2*x^2-
 2*a*b*x^2-
 5*b^2*x^2-
 2*a*c*x^2+
 6*b*c*x^2-
 5*c^2*x^2-
 2*a^2*x*y-
 4*a*b*x*y-
 2*b^2*x*y+
 4*a*c*x*y+
 4*b*c*x*y+
 6*c^2*x*y-
 5*a^2*y^2-
 2*a*b*y^2+
 3*b^2*y^2+
 6*a*c*y^2-
 2*b*c*y^2-
 5*c^2*y^2-
 2*a^2*x*z+
 4*a*b*x*z+
 6*b^2*x*z-
 4*a*c*x*z+
 4*b*c*x*z-
 2*c^2*x*z+
 6*a^2*y*z+
 4*a*b*y*z-
 2*b^2*y*z+
 4*a*c*y*z-
 4*b*c*y*z-
 2*c^2*y*z-
 5*a^2*z^2+
 6*a*b*z^2-
 5*b^2*z^2-
 2*a*c*z^2-
 2*b*c*z^2+
 3*c^2*z^2,
 "Stammler circle" -> 3*a^2*b^2*c^2*x^2-
 a^4*c^2*x*y+
 8*a^2*b^2*c^2*x*y-
 b^4*c^2*x*y+
 2*a^2*c^4*x*y+
 2*b^2*c^4*x*y-
 c^6*x*y+
 3*a^2*b^2*c^2*y^2-
 a^4*b^2*x*z+
 2*a^2*b^4*x*z-
 b^6*x*z+
 8*a^2*b^2*c^2*x*z+
 2*b^4*c^2*x*z-
 b^2*c^4*x*z-
 a^6*y*z+
 2*a^4*b^2*y*z-
 a^2*b^4*y*z+
 2*a^4*c^2*y*z+
 8*a^2*b^2*c^2*y*z-
 a^2*c^4*y*z+
 3*a^2*b^2*c^2*z^2,
 "Brocard inellipse" -> b^4*c^4*x^2-
 2*a^2*b^2*c^4*x*y+
 a^4*c^4*y^2-
 2*a^2*b^4*c^2*x*z-
 2*a^4*b^2*c^2*y*z+
 a^4*b^4*z^2,
 "DeLongchamps ellipse" -> a*b^2*c^2*x^2-
 b^3*c^2*x^2-
 b^2*c^3*x^2+
 2*a*b*c^3*x*y-
 a^3*c^2*y^2+
 a^2*b*c^2*y^2-
 a^2*c^3*y^2+
 2*a*b^3*c*x*z+
 2*a^3*b*c*y*z-
 a^3*b^2*z^2-
 a^2*b^3*z^2+
 a^2*b^2*c*z^2,
 "Feuerbach hyperbola" -> a^2*c*x*y-
 b^2*c*x*y-
 a*c^2*x*y+
 b*c^2*x*y-
 a^2*b*x*z+
 a*b^2*x*z-
 b^2*c*x*z+
 b*c^2*x*z-
 a^2*b*y*z+
 a*b^2*y*z+
 a^2*c*y*z-
 a*c^2*y*z,
 "Jerabek hyperbola" -> (a^2 - b^2)*c^2*(a^2 + b^2 - c^2)*x*y+
 b^2*(-a^2 + c^2)* (a^2 - b^2 + c^2)*x*z+
 a^2*(b^2 - c^2)*(-a^2 + b^2 + c^2)*y*z,
 "Kiepert hyperbola" -> a^2*x*y-
 b^2*x*y-
 a^2*x*z+
 c^2*x*z+
 b^2*y*z-
 c^2*y*z,
 "Kiepert parabola" -> (b^2 - c^2)^2*x^2-
 2*(b^2 - c^2)*(-a^2 + c^2)*x* y + (-a^2 + c^2)^2*y^2-
 2*(a^2 - b^2)*(b^2 - c^2)*x*z-
 2*(a^2 - b^2)*(-a^2 + c^2)*y*z+
 (a^2 - b^2)^2*z^2,
 "MacBeath circumconic" -> a^2*c^2*x*y+
 b^2*c^2*x*y-
 c^4*x*y+
 a^2*b^2*x*z-
 b^4*x*z+
 b^2*c^2*x*z-
 a^4*y*z+
 a^2*b^2*y*z+
 a^2*c^2*y*z,
 "Mandart inellipse" -> (a + b - c)^2*(a - b + c)^2*x^2-
 2*(a + b - c)^2*(a - b + c)*(-a + b + c)*x*y+
 (a + b - c)^2*(-a + b + c)^2*y^2-
 2*(a + b - c)*(a - b + c)^2* (-a + b + c)*x*z-
 2*(a + b - c)*(a - b + c)*(-a + b + c)^2*y*z+
 (a - b + c)^2*(-a + b + c)^2*z^2,
 "Orthic inconic" -> a^4*x^2-
 2*a^2*b^2*x^2+
 b^4*x^2-
 2*a^2*c^2*x^2+
 2*b^2*c^2*x^2+
 c^4*x^2+
 2*a^4*x*y-
 4*a^2*b^2*x*y+
 2*b^4*x*y-
 2*c^4*x*y+
 a^4*y^2-
 2*a^2*b^2*y^2+
 b^4*y^2+
 2*a^2*c^2*y^2-
 2*b^2*c^2*y^2+
 c^4*y^2+
 2*a^4*x*z-
 2*b^4*x*z-
 4*a^2*c^2*x*z+
 2*c^4*x*z-
 2*a^4*y*z+
 2*b^4*y*z-
 4*b^2*c^2*y*z+
 2*c^4*y*z+
 a^4*z^2+
 2*a^2*b^2*z^2+
 b^4*z^2-
 2*a^2*c^2*z^2-
 2*b^2*c^2*z^2+
 c^4*z^2,
 "Stammler hyperbola" -> b^4*c^2*x^2-
 b^2*c^4*x^2-
 a^4*c^2*y^2+
 a^2*c^4*y^2+
 a^4*b^2*z^2-
 a^2*b^4*z^2,
 "Steiner circumellipse" -> x*y+
 x*z+
 y*z,
 "Steiner inellipse" -> x^2-
 2*x*y+
 y^2-
 2*x*z-
 2*y*z+
 z^2,
 "Yff parabola" -> (b - c)^2*x^2-
 2*(b - c)*(-a + c)*x*y+
 (-a + c)^2*y^2-
 2*(a - b)*(b - c)*x*z-
 2*(a - b)*(-a + c)*y*z+
 (a - b)^2*z^2,
 "1st Neuberg ellipse" -> a^2*x*y-
 b^2*x*y-
 c^2*x*y-
 a^2*x*z-
 b^2*x*z+
 c^2*x*z-
 a^2*y*z+
 b^2*y*z-
 c^2*y*z,
 "2nd Neuberg ellipse" -> a^2*x*y-
 b^2*x*y+
 c^2*x*y-
 a^2*x*z+
 b^2*x*z+
 c^2*x*z+
 a^2*y*z+
 b^2*y*z-
 c^2*y*z,
 "Hutson-Moses hyperbola" -> (a - c)*(b - c)*c*x*y-
 (a - b)*b*(b - c)*x*z+
 a*(a - b)*(a - c)*y*z,
 "Suppa-Cucoanes circle" -> (a^3 + b^3 + b^2*c + b*c^2 + c^3 - a^2*(b + c) - a*(b^2 + c^2))*x^2+
 2*(a + b)*(a^2 - 2*a*b + b^2 - c^2)*x*y+
 (a^3 + a^2*(-b + c) + (b - c)^2*(b + c) + a*(-b^2 + c^2))* y^2+
 2*(a + c)*(a^2 - b^2 - 2*a*c + c^2)*x*z+
 2*(-a^2 + (b - c)^2)*(b + c)*y*z+
 (a^3 + a^2*(b - c) + (b - c)^2*(b + c) + a*(b^2 - c^2))*z^2,
 "anti-Artzt circle" -> 2*(b^2 - c^2)^2*x^2+
 (2*a^4 + 2*b^4 - 7*a^2*c^2 - 7*b^2*c^2 + c^4)*x* y + 2*(a^2 - c^2)^2*y^2+
 (2*a^4 - 7*a^2*b^2 + b^4 - 7*b^2*c^2 + 2*c^4)*x*z+
 (a^4 - 7*a^2*(b^2 + c^2) + 2*(b^4 + c^4))*y*z+
 2*(a^2 - b^2)^2*z^2,
 "Hutson ellipse" -> -(b*c*(-a + b + c)*x^2) + c*(-a^2 + 3*a*b - b^2 + c^2)*x*y-
 a*c*(a - b + c)*y^2+
 b*(-a^2 + b^2 + 3*a*c - c^2)*x*z+
 a*(a^2 - b^2 + 3*b*c - c^2)*y*z-
 a*b*(a + b - c)*z^2,
 "Hofstadter ellipse" -> b^2*c^2*x^2-
 2*a*b*c^2*x*y+
 a^2*c^2*y^2-
 2*a*b^2*c*x*z-
 2*a^2*b*c*y*z+
 a^2*b^2*z^2,
 "Moses-Feuerbach circumconic" -> (a - b)^2*(a + b - c)*x*y+
 (-a + c)^2*(a - b + c)*x*z+
 (b - c)^2*(-a + b + c)*y*z,
 "Wallace hyperbola" -> c^2*(x^2-y^2) + a^2*(y^2-z^2) + b^2*(-x^2+z^2)|> 
 
 fltDuals = <|"dual conic of 1st Yff-Moses hyperbola" -> (b + c)^2*x^2-
 2*(a*(b - 3*c) + c*(-3*b + c))*x*y+
 (a + c)^2*y^2+
 (6*a*b - 2*b^2 - 2*a*c + 6*b*c)*x*z-
 2*(a^2 + b*c - 3*a*(b + c))*y* z + (a + b)^2*z^2,
 "dual conic of nine-point circle" -> (b^2 - c^2)^2*x^2-
 2*(c^2*(b^2 - c^2) + a^2*(b^2 + c^2))*x*y+
 (a^2 - c^2)^2*y^2-
 2*(-b^4 + b^2*c^2 + a^2*(b^2 + c^2))*x*z+
 2*(a^4 - b^2*c^2 - a^2*(b^2 + c^2))*y*z+
 (a^2 - b^2)^2*z^2,
 "dual conic of polar circle" -> (a^4 - (b^2 - c^2)^2)*x^2+
 (-a^4 + b^4 + 2*a^2*c^2 - c^4)*y^2+
 (-a^4 + 2*a^2*b^2 - b^4 + c^4)*z^2,
 "dual conic of Spieker circle" -> (-a + b + c)*x^2+
 (a + b + c)*x*y+
 (a - b + c)*y^2+
 (a + b + c)*x*z+
 (a + b + c)*y*z+
 (a + b - c)*z^2,
"dual conic of DeLongchamps ellipse" -> a^2*(b - c)^2*x^2-
 2*a*b*((b - c)*c + a*(b + c))*x*y+
 b^2*(a - c)^2*y^2-
 2*a*c*(b*(-b + c) + a*(b + c))*x*z-
 2*b*c*(-a^2 + b*c + a*(b + c))*y* z + (a - b)^2*c^2*z^2,
 "dual conic of Feuerbach hyperbola" -> a^2*(b - c)^2*(-a + b + c)^2*x^2-
 2*a*b*(a - c)*(b - c)* (a^2 - 2*a*b + b^2 - c^2)*x*y+
 b^2*(a - c)^2*(a - b + c)^2*y^2+
 2*a*(a - b)*(b - c)*c*(a^2 - b^2 - 2*a*c + c^2)*x*z+
 2*(a - b)*b*(a^2 - (b - c)^2)*(a - c)*c*y*z+
 (a - b)^2*(a + b - c)^2* c^2*z^2,
 "dual conic of Jerabek hyperbola" -> a^4*(-b^4 + c^4 + a^2*(b^2 - c^2))^2*x^2-
 2*a^2*b^2*(a^2 - c^2)* (b^2 - c^2)*(a^4 - 2*a^2*b^2 + b^4 - c^4)*x*y+
 b^4*(a^2 - c^2)^2*(a^2 - b^2 + c^2)^2*y^2+
 2*a^2*(a^2 - b^2)*c^2* (b^2 - c^2)*(a^4 - b^4 - 2*a^2*c^2 + c^4)*x*z+
 2*b^2*(a^2 - b^2)*c^2*(a^2 - c^2)*(a^4 - (b^2 - c^2)^2)*y*z+
 (a^2 - b^2)^2*c^4*(a^2 + b^2 - c^2)^2*z^2,
 "dual conic of Lemoine inellipse" -> (2*a^2 + 2*b^2 - c^2)*x*y+
 (2*a^2 - b^2 + 2*c^2)*x*z+
 (-a^2 + 2*(b^2 + c^2))*y*z,
 "dual conic of Stammler hyperbola" -> a^2*(a^2 - b^2)*(a^2 - c^2)*x^2+
 b^2*(-a^2 + b^2)*(b^2 - c^2)*y^2+
 c^2*(-a^2 + c^2)*(-b^2 + c^2)*z^2,
 "dual conic of Yff parabola" -> (a - b)*x*y+
 (-a + c)*x*z+
 (b - c)*y*z,
 "dual conic of Yff hyperbola" -> a^4*x^2+
 (-2*a^2*(b^2 - 2*c^2) + 4*c^2*(b^2 - c^2))*x*y+
 b^4*y^2+
 (-4*b^4 + 4*b^2*c^2 + a^2*(4*b^2 - 2*c^2))*x*z+
 (-4*a^4 - 2*b^2*c^2 + 4*a^2*(b^2 + c^2))*y*z+
 c^4*z^2,
 "dual conic of 1st Neuberg ellipse" -> (a^2 - b^2 + c^2)^2*x^2+
 2*(-a^4 + (b^2 - c^2)^2)*x*y+
 (a^2 + b^2 - c^2)^2*y^2+
 2*(a^4 - 2*a^2*b^2 + b^4 - c^4)*x*z+
 2*(a^4 - b^4 - 2*a^2*c^2 + c^4)* y*z+
 (-a^2 + b^2 + c^2)^2*z^2,
 "dual conic of 2nd Neuberg ellipse" -> (a^2 + b^2 - c^2)^2*x^2+
 2*(a^4 - b^4 - 2*a^2*c^2 + c^4)*x*y+
 (-a^2 + b^2 + c^2)^2*y^2+
 2*(-a^4 + (b^2 - c^2)^2)*x*z+
 2*(a^4 - 2*a^2*b^2 + b^4 - c^4)*y*z+
 (a^2 - b^2 + c^2)^2*z^2,
 "dual conic of Hutson-Moses hyperbola" -> a^2*(a - b)^2*(a - c)^2*x^2+
 2*a*(a - b)^2*b*(a - c)*(b - c)*x*y+
 (a - b)^2*b^2*(b - c)^2*y^2-
 2*a*(a - b)*(a - c)^2*(b - c)*c*x*z+
 2*(a - b)*b*(a - c)*(b - c)^2*c*y*z+
 (a - c)^2*(b - c)^2*c^2*z^2,
 "dual conic of Suppa-Cucoanes circle" -> a^2*x^2-
 2*(a^2 + a*b + b^2 - c^2)*x*y+
 b^2*y^2-
 2*(a^2 - b^2 + a*c + c^2)*x*z+
 2*(a^2 - b^2 - b*c - c^2)*y*z+
 c^2*z^2,
 "dual conic of Moses-Feuerbach circumconic" -> (b - c)^4*(-a + b + c)^2*x^2+
 2*(a - c)^2*(b - c)^2* (a^2 - 2*a*b + b^2 - c^2)*x*y+
 (a - c)^4*(a - b + c)^2*y^2+
 2*(a - b)^2*(b - c)^2*(a^2 - b^2 - 2*a*c + c^2)*x*z-
 2*(a - b)^2*(a^2 - (b - c)^2)*(a - c)^2*y*z+
 (a - b)^4*(a + b - c)^2* z^2,
 "dual conic of Wallace hyperbola" -> (a^2 - b^2)*(a^2 - c^2)*x^2-
 (a^2 - b^2)*(b^2 - c^2)*y^2+
 (a^2 - c^2)*(b^2 - c^2)*z^2,
 "dual conic of Moses HK-parabola" -> (a^2 - b^2)*(a^2 + b^2 - c^2)*x*y-
 (a^2 - c^2)*(a^2 - b^2 + c^2)*x*z+
 (b^2 - c^2)*(-a^2 + b^2 + c^2)*y*z|> 
 
 fltOrthoptic = <|"orthoptic circle of circumcircle" -> -(a^2*b^2*c^2*x^2) + c^2*(a^4 + (b^2 - c^2)^2 - 2*a^2*(2*b^2 + c^2))*x*y-
 a^2*b^2*c^2*y^2+
 b^2*(a^4 + (b^2 - c^2)^2 - 2*a^2*(b^2 + 2*c^2))*x* z + a^2*(a^4 + b^4 - 4*b^2*c^2 + c^4 - 2*a^2*(b^2 + c^2))*y*z-
 a^2*b^2*c^2*z^2,
 "orthoptic circle of nine-point circle" -> (a^6 - 3*a^4*(b^2 + c^2) - (b^2 - c^2)^2*(b^2 + c^2) + a^2*(3*b^4 + b^2*c^2 + 3*c^4))*x^2+
 2*c^2*(a^4 + (b^2 - c^2)^2 - a^2*(3*b^2 + 2*c^2))*x*y+
 (-a^6 + (b^2 - c^2)^3 + a^4*(3*b^2 + c^2) + a^2*(-3*b^4 + b^2*c^2 + c^4))*y^2+
 2*b^2*(a^4 + (b^2 - c^2)^2 - a^2*(2*b^2 + 3*c^2))*x*z+
 2*a^2*(a^4 + b^4 - 3*b^2*c^2 + c^4 - 2*a^2*(b^2 + c^2))*y*z+
 (-a^6 - (b^2 - c^2)^3 + a^4*(b^2 + 3*c^2) + a^2*(b^4 + b^2*c^2 - 3*c^4))*z^2,
 "orthoptic circle of orthoptic circle of the Steiner Inellipse" -> (2*a^2 - b^2 - c^2)*x^2+
 (a^2 + b^2 + 7*c^2)*x*y+
 (-a^2 + 2*b^2 - c^2)*y^2+
 (a^2 + 7*b^2 + c^2)*x*z+
 (7*a^2 + b^2 + c^2)*y*z+
 (-a^2 - b^2 + 2*c^2)*z^2,
 "orthoptic circle of Brocard inellipse" -> b^2*c^2*(-a^2 + b^2 + c^2)*x^2+
 c^2*(a^4 + b^4 - b^2*c^2 - a^2*(4*b^2 + c^2))*x*y+
 a^2*c^2*(a^2 - b^2 + c^2)*y^2+
 b^2*(a^4 - b^2*c^2 + c^4 - a^2*(b^2 + 4*c^2))*x*z+
 a^2*(b^4 - 4*b^2*c^2 + c^4 - a^2*(b^2 + c^2))*y*z+
 a^2*b^2*(a^2 + b^2 - c^2)*z^2,
 "orthoptic circle of Feuerbach hyperbola" -> (-a^5 + b^5 - b^4*c - b*c^4 + c^5 + a^4*(b + c) + a^3*(2*b^2 - b*c + 2*c^2) - 2*a^2*(b^3 + b^2*c + b*c^2 + c^3) - a*(b^4 - 3*b^3*c - 3*b*c^3 + c^4))*x^2+
 2*(a - c)*(b - c)*c* (a^2 - 2*a*b + b^2 - c^2)*x*y+
 (a^5 + 2*a^2*b^2*(b - c) - a^4*(b + c) - (b - c)^3*(b + c)^2 + a^3*b*(-2*b + 3*c) + a*(b - c)^2*(b^2 + b*c - c^2))*y^2-
 2*(a - b)*b*(b - c)* (a^2 - b^2 - 2*a*c + c^2)*x*z-
 2*a*(a - b)*(a^2 - (b - c)^2)*(a - c)* y*z+
 (a^5 + a^3*(3*b - 2*c)*c + 2*a^2*c^2*(-b + c) - a^4*(b + c) + (b - c)^3*(b + c)^2 - a*(b - c)^2*(b^2 - b*c - c^2))*z^2,
 "orthoptic circle of Jerabek hyperbola" -> (-a^8 + b^8 - b^6*c^2 - b^2*c^6 + c^8 + 3*a^6*(b^2 + c^2) - a^4*(2*b^4 + 5*b^2*c^2 + 2*c^4) - a^2*(b^6 - 3*b^4*c^2 - 3*b^2*c^4 + c^6))*x^2+
 2*(a^2 - c^2)*(b^2 - c^2)*(a^4 - 2*a^2*b^2 + b^4 - c^4)*x*y+
 (a^8 - a^6*(b^2 + c^2) + a^4*(-2*b^4 + 3*b^2*c^2) - (b^2 - c^2)^2*(b^4 - b^2*c^2 - c^4) + a^2*(3*b^6 - 5*b^4*c^2 + 3*b^2*c^4 - c^6))*y^2-
 2*(a^2 - b^2)*(b^2 - c^2)* (a^4 - b^4 - 2*a^2*c^2 + c^4)*x*z-
 2*(a^2 - b^2)*(a^2 - c^2)* (a^4 - (b^2 - c^2)^2)*y*z+
 (a^8 - a^6*(b^2 + c^2) + a^4*(3*b^2*c^2 - 2*c^4) + (b^2 - c^2)^2*(b^4 + b^2*c^2 - c^4) - a^2*(b^6 - 3*b^4*c^2 + 5*b^2*c^4 - 3*c^6))*z^2,
 "orthoptic circle of Kiepert hyperbola" -> (-a^6 + b^6 + b^4*c^2 + b^2*c^4 + c^6 + 3*a^4*(b^2 + c^2) - 3*a^2*(b^4 + b^2*c^2 + c^4))*x^2-
 2*c^2*(-a^2 + c^2)*(-b^2 + c^2)*x* y + (a^6 - (b^2 - c^2)^3 + a^4*(-3*b^2 + c^2) + a^2*(3*b^4 - 3*b^2*c^2 + c^4))*y^2+
 2*b^2*(a^2 - b^2)*(b^2 - c^2)*x* z - 2*a^2*(a^2 - b^2)*(a^2 - c^2)*y*z+
 (a^6 + a^4*(b^2 - 3*c^2) + (b^2 - c^2)^3 + a^2*(b^4 - 3*b^2*c^2 + 3*c^4))*z^2,
 "orthoptic circle of MacBeath inconic" -> a^2*(-a^2 + b^2 + c^2)^2*x^2+
 (a^6 - a^4*b^2 + b^6 - 3*b^2*c^4 + 2*c^6 - a^2*(b^4 + 3*c^4))*x*y+
 b^2*(a^2 - b^2 + c^2)^2*y^2+
 (a^6 + 2*b^6 - a^4*c^2 - 3*b^4*c^2 + c^6 - a^2*(3*b^4 + c^4))*x*z+
 (2*a^6 - 3*a^4*(b^2 + c^2) + (b^2 - c^2)^2*(b^2 + c^2))*y*z+
 c^2*(a^2 + b^2 - c^2)^2*z^2,
 "orthoptic circle of orthic inconic" -> (-a^2 + b^2 + c^2)^2*x^2+
 2*(a^4 + b^4 - b^2*c^2 - a^2*(2*b^2 + c^2))* x*y+
 (a^2 - b^2 + c^2)^2*y^2+
 2*(a^4 - b^2*c^2 + c^4 - a^2*(b^2 + 2*c^2))*x*z+
 2*((b^2 - c^2)^2 - a^2*(b^2 + c^2))*y*z+
 (a^2 + b^2 - c^2)^2*z^2,
 "orthoptic circle of Stammler hyperbola" -> -(b^2*c^2*(b^2 - c^2)^2*x^2) + c^2*(a^2 - c^2)*(a^2 + b^2 - c^2)* (-b^2 + c^2)*x*y-
 a^2*c^2*(a^2 - c^2)^2*y^2+
 b^2*(a^2 - b^2)*(b^2 - c^2)*(a^2 - b^2 + c^2)*x*z+
 a^2*(a^2 - b^2)*(a^2 - c^2)*(a^2 - b^2 - c^2)*y*z-
 a^2*b^2*(a^2 - b^2)^2*z^2,
 "orthoptic circle of Steiner circumellipse" -> a^2*x^2+
 (a^2 + b^2 + 3*c^2)*x*y+
 b^2*y^2+
 (a^2 + 3*b^2 + c^2)*x*z+
 (3*a^2 + b^2 + c^2)*y*z+
 c^2*z^2,
 "orthoptic circle of Steiner inellipse" -> (a^2 - b^2 - c^2)*x^2+
 4*c^2*x*y+
 (-a^2 + b^2 - c^2)*y^2+
 4*b^2*x*z+
 4*a^2*y*z+
 (-a^2 - b^2 + c^2)*z^2,
 "orthoptic circle of Suppa-Cucoanes circle" -> (a^3 + b^3 + c^3 - a^2*(b + c) - a*(b^2 - b*c + c^2))*x^2+
 (2*a^3 + 2*b^3 - b^2*c - 2*b*c^2 + c^3 - a^2*(2*b + c) - 2*a*(b^2 - b*c + c^2))*x*y+
 (a^3 - a^2*b + a*b*(-b + c) + (b - c)^2*(b + c))*y^2+
 (2*a^3 + b^3 - 2*b^2*c - b*c^2 + 2*c^3 - a^2*(b + 2*c) - 2*a*(b^2 - b*c + c^2))*x*z+
 (a^3 - a*(b - c)^2 - 2*a^2*(b + c) + 2*(b - c)^2*(b + c))*y*z+
 (a^3 - a^2*c + a*(b - c)*c + (b - c)^2*(b + c))*z^2,
 "orthoptic circle of Hofstadter ellipse" -> b*c*(-a^2 + b^2 + c^2)*x^2-
 c*(-a^3 + a^2*b - b^3 + b*c^2 + a*(b + c)^2)*x*y+
 a*c*(a^2 - b^2 + c^2)*y^2-
 b*(-a^3 + a^2*c + a*(b + c)^2 + c*(b^2 - c^2))*x*z-
 a*(2*a*b*c + a^2*(b + c) - (b - c)^2*(b + c))*y* z + a*b*(a^2 + b^2 - c^2)*z^2,
"orthoptic circle of Wallace hyperbola" -> -(a^2*(b^2 - c^2)^2*x^2) + (a^2 - c^2)*(a^2 + b^2 - c^2)*(-b^2 + c^2)*x*y-
 b^2*(a^2 - c^2)^2*y^2+
 (a^2 - b^2)*(b^2 - c^2)*(a^2 - b^2 + c^2)*x* z + (a^2 - b^2)*(a^2 - c^2)*(a^2 - b^2 - c^2)*y*z-
 (a^2 - b^2)^2*c^2*z^2|>

