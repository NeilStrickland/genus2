check_map_to_S2_charts := proc()
 local k,aa,bb,cc,dd,MM;

 printf("%a()\n",procname);

 for k in {0,3,6,11} do
  # Chart centres
  _ASSERT(
   chart[k](0,0) -~ v_E0[k] = [0$4],
   sprintf("chart %d is centred",k));
  
  # Charts stay on EX to first order
  _ASSERT(
   simplify(rem(rho(chart[k](e*s,e*t))-1,e^2,e)) = 0,
   sprintf("chart %d is tangent to S^3",k));
   
  _ASSERT(
   simplify(rem(g_00(chart[k](e*s,e*t)),e^2,e)) = 0,
   sprintf("chart %d is tangent to EX^*",k));

  # Charts are conformal to first order
  aa := v_E0[k];
  bb := dg0(aa);
  cc := subs(s=0,map(diff,chart[k](s,0),s));
  dd := subs(t=0,map(diff,chart[k](0,t),t));
  MM := Matrix([aa,bb,cc,dd]);

  _ASSERT(
   evalf(simplify(Determinant(MM))) > 0,
   sprintf("chart %d is oriented",k));

  _ASSERT(
   simplify({dp4(aa,bb),dp4(aa,cc),dp4(aa,dd),dp4(bb,cc),dp4(bb,dd),dp4(cc,dd),
             dp4(aa,aa)-1,dp4(cc,cc)-1,dp4(dd,dd)-1}) = {0},
   sprintf("chart %d is conformal to first order",k));
 od:
end:

add_check(check_map_to_S2_charts):

######################################################################

check_E_to_S2 := proc()
 local s,S3,S4,sgn3,sgn4,f0,df0,n0,m0,J0;
 
 printf("%a()\n",procname);

 _ASSERT(
  FNF_y0(simplify(E_to_S2(xx) *~ sqrt(1 - E_to_S2_s_x(xx)) -~ E_to_S2_tilde(xx))) = [0$3],
  "E_to_S2 in terms of E_to_S2_tilde"
 );

 _ASSERT(
  FNF_y0(simplify(E_to_S2_s_x(xx) - E_to_S2_s_z(z_proj0(xx)))) = 0,
  "E_to_S2_s_x is compatible with E_to_S2_s_z"
 );

 _ASSERT(
  simplify(oval_h([E_to_S2_s_max_z1,E_to_S2_s_max_z2])) = 0,
  "maximising point for E_to_S2_s_z is on the boundary of F16"
 );

 _ASSERT(
  simplify(E_to_S2_s_z([E_to_S2_s_max_z1,E_to_S2_s_max_z2]) - E_to_S2_s_max) = 0,
  "maximum value of E_to_S2_s_z"
 );

 _ASSERT(
  simplify(subs(z[1]=E_to_S2_s_max_z1,factor(diff(subs(solve(oval_h(zz),{z[2]}),E_to_S2_s_z(z)),z[1])))) = 0,
  "maximising point for E_to_S2_s_z is maximising"
 );

 _ASSERT(FNF_y0(nm3(E_to_S2(x))^2 - 1) = 0,"E_to_S2 lands in S2");

 _ASSERT(
   {seq(simplify(E_to_S2(v_E0[i]) -~ v_S2[i]),i=0..13)} = {[0$3]},
   "E_to_S2 on vertices"
 );

 assume(s::real);
 
 _ASSERT(
   {seq(simplify(expand(combine(E_to_S2(c_E0[k](s)) -~ c_S2[k](s)))),k=0..16)} = {[0$3]},
   "E_to_S2 on curves"
 );

 _ASSERT(
   {seq(simplify(E_to_S2(act_R4[T](xx)) -~ act_S2[T](E_to_S2(xx))),T in G16)} = {[0$3]},
   "E_to_S2 is equivariant"
 );

 S3 := combinat[permute](3):
 S4 := combinat[permute](4):
 sgn3 := (s) -> signum(mul(mul(s[j]-s[i],j=i+1..3),i=1..2));
 sgn4 := (s) -> signum(mul(mul(s[j]-s[i],j=i+1..4),i=1..3));
 f0 := E_to_S2(xx);
 df0 := [seq([seq(factor(diff(f0[i],x[j])),j=1..4)],i=1..3)];
 n0 := dg0(xx);
 J0 := add(add(add(add(add(add(add(
        -sgn4([i,j,k,l])*x[i]*n0[j]*df0[p,k]*df0[q,l]*sgn3([p,q,r])*f0[r]/2,
	i=1..4),j=1..4),k=1..4),l=1..4),p=1..3),q=1..3),r=1..3);
 m0 := (4*x[3]^4-2*x[3]^2*x[4]^2+4*x[4]^4+8*x[3]^2+4);
 J0 := FNF_z0(simplify(sqrt(m0)*J0))/sqrt(FNF_z0(m0));
 _ASSERT(simplify(J0 - E_to_S2_jacobian) = 0,"jacobian of E_to_S2");
 
end:

add_check(check_E_to_S2):



