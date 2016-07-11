check_zeta := proc()
 local a1,a2,a3,a4,v1,v2,u,x,xx,t;

 printf("%a()\n",procname);

 xx := [seq(x[i],i=1..4)];
 
 _ASSERT(
  simplify(disc_zeta_proj(c_E0[3](t)) -~ [(1+1/sqrt(2))/sqrt(3)*cos(t),sin(t)]) = [0,0],
  "zeta on c[3]"
 );

 _ASSERT(
  simplify(disc_zeta_q0(disc_zeta_proj(c_E0[3](t)))) = 0,
  "image of zeta on c[3]"
 );

 _ASSERT(
  simplify(disc_zeta_proj(c_E0[6](t)) -~ [2-(1-cos(t))/sqrt(2),sin(t)]/~sqrt(10-2*cos(t))) = [0,0],
  "zeta on c[6]"
 );

 _ASSERT(
  simplify(disc_zeta_q1(disc_zeta_proj(c_E0[6](t)))) = 0,
  "image of zeta on c[6]"
 );

 _ASSERT(
  simplify(disc_zeta_q1(disc_zeta_proj(c_E0[8](t)))) = 0,
  "image of zeta on c[8]"
 );

 a1 := 34 - 24*sqrt(2);
 a2 := 40 - 28*sqrt(2);
 a3 := 10 -  6*sqrt(2);
 a4 :=  3 -  2*sqrt(2);

 _ASSERT(min(evalf([a1,a2,a3,a4])) > 0,"coefficients for q1 are positive");

 v1 := a3*u[1]^2+a4;
 v2 := a1*u[1]^4+a2*u[1]^2;

 _ASSERT(
  simplify(disc_zeta_q1(u) - ((u[2]^2+v1)^2 - 4*(v2))) = 0,
  "alternative formula for q1"
 );

 _ASSERT(
   {solve(4*v2 - v1^2,u[1])} =
   {1/sqrt(3)-1/sqrt(6),1/sqrt(2),-1/sqrt(2),-(1/sqrt(3)-1/sqrt(6))},
   "intersection of holes with u[1] axis"
 );

 _ASSERT(
  simplify(disc_zeta_det(xx) - x[1]*(disc_zeta_q8(y_proj0(xx)) + 
	    (2+(3-2*sqrt(2))*x[3]*x[4])*(rho(xx)-1) + (3-2*sqrt(2))/2*x[3]*g0(xx))) = 0,
  "zeta determinant formula"
 );

 _ASSERT(
  expand(
   disc_zeta_q5(disc_zeta_proj(xx),x[1]) - 
   (sqrt(2)+1)^8*disc_zeta_q2(disc_zeta_proj(xx)) - 
   disc_zeta_q6(xx)*(rho(xx)-1)+
   disc_zeta_q7(xx)*g0(xx)
  ) = 0,
  "q5267 identity" 
 );

 _ASSERT(
  expand(
  disc_zeta_q5(u,x[1]) - 
  (sqrt(2)+1)^8*disc_zeta_q2(u) - 
  (sqrt(2)+1)^4*u[1]^2*disc_zeta_q3(u,x[1]) + 
  (1-u[1]^2-u[2]^2-x[1]^2)*disc_zeta_q4(u,x[1])^2 
  ) = 0,
  "q5234 identity"
 );
end:


add_check(check_zeta):
