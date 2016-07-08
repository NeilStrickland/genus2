check_invariants := proc()
 local n,B,BI,err;

 printf("%a()\n",procname);

 _ASSERT(
  expand(x[1]^2 - eval(subs(y = yx,uy[1])) - (rho(xx)-1)/2 - g_0(xx)*(g_0(xx)+2*(1/a_E+a_E)*x[3]+4*x[4])/8) = 0,
  "x[1]^2 = uy[1]"
 );

 _ASSERT(
  expand(x[2]^2 - eval(subs(y = yx,uy[2])) - (rho(xx)-1)/2 - g_0(xx)*(g_0(xx)-2*(1/a_E+a_E)*x[3]+4*x[4])/8) = 0,
  "x[2]^2 = uy[2]"
 );

 _ASSERT(
   zx[3] = x[1]^2 + x[2]^2 and
   expand(1/4*zx[3]^2-a_E^2*zx[2]-(a_E+1/a_E)/2*zx[5]-(a_E+1/a_E)^2/4*zx[1]*zx[4]) = x[1]^2*x[2]^2 and
   zx[1] = x[3]^2 and
   zx[4] = x[4]^2 and
   zx[5] + (a_E+1/a_E)*zx[1]*zx[4] = (x[2]^2-x[1]^2)*x[3]*x[4],
   "Basic invariants on R4 in terms of z_1,..,z_5"
 );

 _ASSERT(
  expand(zx[5]^2-4*a_E^2*zx[1]*zx[2]*zx[4]) = 0, 
  "z_5^2 relation"
 );

 _ASSERT(
  expand(zx[3] - (1  - zx[1] - zx[1]*zx[2]) - (rho(xx)-1) - (g_0(xx)/4 + x[4])*g_0(xx)) = 0 and 
  expand(zx[4] - zx[1] * zx[2] + (g_0(xx)/4+x[4])*g_0(xx)) = 0 and
  expand(zx[5] + 2 * a_E * zx[1] * zx[2] + a_E*yx[1]*yx[2]*g_0(xx)) = 0,
  "z_3,..,z_5 in terms of z_1,z_2 on X"
 );

 _ASSERT(
  simplify(y_proj(y_lift([y[1],y[2]])) -~ [y[1],y[2]]) = [0,0],
  "y_proj o y_lift = 1"
 );

 _ASSERT(
  simplify(z_proj(z_lift([z[1],z[2]])) -~ [z[1],z[2]]) = [0,0],
  "z_proj o z_lift = 1"
 );

 _ASSERT(
  NF_x(square_norm_of_dg(xx) - square_norm_of_dg_z(zx)) = 0,
  "square norm of dg in terms of z"
 );

 _ASSERT(
  NF_x(numer(factor(curvature(xx) - curvature_z(zx)))) = 0,
  "curvature in terms of z"
 );

 _ASSERT(
  [(simplify(subs(y[1] = w_to_y_y_1(w[1],y[2]),wy[1] - w[1])) assuming y[2] >= 0),
   (simplify(subs(y[1] = w_to_y_y_1(w[1],y[2]),wy[2] - w_to_y_p(y[2],w[1]))) assuming y[2] >= 0),
   expand(numer(simplify(diff(w_to_y_p(t,w1),t))) - (add(w_to_y_dpc[k]*t^k,k=0..4)*(4*a_E^4)))
  ] = 
  [0,0,0],
  "properties of w_to_y()"
 );

 err := factor(subs(y[1] = w_to_y_y_1(w[1],y[2]),
   uy[1] - 
   (((1-a_E^2)*w[1]+1/2)*y[2]^2 + 
    ((1-w[1])/2*(a_E+1/a_E) + w[1]*(1/a_E-a_E))*y[2] + 
    (1-w[1])/2) *
   (1-w_to_y_p(y[2],wy[1])) / ((y[2]+a_E)*(y[2]+1/a_E))
  ));

  err := simplify(err) assuming y[2]>0;

 _ASSERT(err = 0,"u[1] in w_to_y()");

 err := factor(subs(y[1] = w_to_y_y_1(w[1],y[2]),
   uy[2]- ((1+2*a_E*y[2])*(1-w[1])/2)
  ));

  err := simplify(err) assuming y[2]>0;

 _ASSERT(err = 0, "u[2] in w_to_y");

 _ASSERT(
  {seq(simplify(y_proj(act_R4[T](xx)) -~ act_hex[T](y_proj(xx))),T in G16)} = {[0,0]},
  "y_proj is equivariant"
 );

 _ASSERT(
  {seq(seq(simplify(
    act_A[T](twisted_invariant[i]) - character[i][T] * twisted_invariant[i]
   ),T in G16),i=0..7)} = {0},
  "twisted invariants"
 );
end:

add_check(check_invariants):

######################################################################

check_square := proc()
 local T,i,p,err;

 printf("%a()\n",procname);
 
 for T in G16 do
  _ASSERT(simplify(w_proj(act_R4[T](xx)) -~ w_proj(xx)) = [0,0],
	 sprintf("w_proj invariant for %a",T));
 od:

 _ASSERT(map(limit,w_proj(c_E[1](t)),t= 0)=[1,0],"w_proj(v[0]) 0");
 _ASSERT(map(limit,w_proj(c_E[2](t)),t= 0)=[1,0],"w_proj(v[0]) 1");
 _ASSERT(map(limit,w_proj(c_E[5](t)),t= 0)=[1,0],"w_proj(v[0]) 2");
 _ASSERT(map(limit,w_proj(c_E[6](t)),t= 0)=[1,0],"w_proj(v[0]) 3");

 _ASSERT(map(limit,w_proj(c_E[1](t)),t=Pi)=[1,0],"w_proj(v[1]) 0");
 _ASSERT(map(limit,w_proj(c_E[2](t)),t=Pi)=[1,0],"w_proj(v[1]) 1");
 _ASSERT(map(limit,w_proj(c_E[7](t)),t= 0)=[1,0],"w_proj(v[1]) 2");
 _ASSERT(map(limit,w_proj(c_E[8](t)),t= 0)=[1,0],"w_proj(v[1]) 3");

 for i from 2 to 5 do
  _ASSERT(simplify(w_proj(v_E[i]) -~ [0,1]) = [0,0],sprintf("w_proj(v[%d])",i));
 od:

 for i from 6 to 9 do
  _ASSERT(simplify(w_proj(v_E[i])) = [0,0],sprintf("w_proj(v[%d])",i));
 od:

 for i from 10 to 13 do
  _ASSERT(simplify(w_proj(v_E[i]) -~ [1,1]) = [0,0],sprintf("w_proj(v[%d])",i));
 od:

 for i from 14 to 21 do
  _ASSERT(simplify(w_proj(v_E[i]) -~ [2*a_E^2/(a_E^2+1), 0]) = [0,0],sprintf("w_proj(v[%d])",i));
 od:

 for i from 22 to 29 do
  _ASSERT(simplify(w_proj(v_E[i]) -~ [1, (2*a_E^2-5)/(6*a_E^2-7)]) = [0,0],sprintf("w_proj(v[%d])",i));
 od:

 for i from 30 to 45 do
  _ASSERT(simplify(w_proj(v_E[i]) -~ [(3/4)*(2*a_E^2+1)/(a_E^2+1), (1/4)*(2*a_E^2-3)/((a_E-1)*(a_E+1))]) = [0,0],
    sprintf("w_proj(v[%d])",i));
 od:
end:

add_check(check_square):

