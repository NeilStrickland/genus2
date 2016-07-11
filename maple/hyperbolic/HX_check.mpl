check_hyperbolic_metric := proc()
 local x1,x2,y1,y2,a1,a2,x,y,a,t;

 printf("%a()\n",procname);

 assume(x1::real,x2::real,y1::real,y2::real,a1::real,a2::real);
 assume(t > 0);
 x := x1 + I*x2;
 y := y1 + I*y2;
 a := a1 + I*a2;

 _ASSERT(
  simplify(d_hyp_c(x,y) - d_hyp([x1,x2],[y1,y2])) = 0,
  "real and complex formulae for d_hyp"
 );

 _ASSERT(
  simplify(E_hyp_c(x) - E_hyp([x1,x2])) = 0,
  "real and complex formulae for E_hyp"
 );

 _ASSERT(
  simplify(d_hyp_c(exp(I*t)*x,exp(I*t)*y) - d_hyp_c(x,y)) = 0,
  "rotation invariance of d_hyp"
 );

 _ASSERT(
  simplify(d_hyp_c((x - a)/(1 - conjugate(a)*x),
                   (y - a)/(1 - conjugate(a)*y)) - d_hyp_c(x,y)) = 0,
  "Mobius invariance of d_hyp"
 );

 _ASSERT(
  simplify(convert(series(d_hyp_c(x,x+t*y)^2,t=0,3),polynom,t) - E_hyp_c(x)*t^2*abs(y)^2 = 0),
  "Compatibility of d_hyp and E_hyp"
 );

 _ASSERT(
  simplify(brioschi_formula(E_hyp([t,u]),0,E_hyp([t,u]),t,u)+1) = 0,
  "curvature of d_hyp"
 );
end:

add_check(check_hyperbolic_metric):

######################################################################

check_Pi_action := proc()
 local i,j,k,ij,T,U,A,w,z,err,ncheck,ncheck_b,old_digits;

 printf("%a()\n",procname);

 for i from 0 to 3 do
  _ASSERT(
   simplify(act_Pi([i,i+4],z) - z) = 0,
   sprintf("beta[%d] o beta[%d] on H",i,i+4)
  );
 od:

 w :=          beta[7](z);
 w := simplify(beta[6](w));
 w := simplify(beta[5](w));
 w := simplify(beta[4](w));
 w := simplify(beta[3](w));
 w := simplify(beta[2](w));
 w := simplify(beta[1](w));
 w := simplify(beta[0](w));

 _ASSERT(simplify(w-z) = 0,"long beta relation on H");

 for T in L8 do
  for U in L8 do
   _ASSERT(
    act_H[T](act_H[U](z)) = act_H[G_mult(T,U)](z),
    "composition for action of L8 on H"
   );
  od;
 od;

 _ASSERT(
  simplify(act_H[LM](z) - act_H[L](act_H[M](z))) = 0,
  "LM = L o M on H"
 );

 _ASSERT(
  simplify(act_H[LLM](z) - act_H[LL](act_H[M](z))) = 0,
  "LLM = LL o M on H"
 );

 _ASSERT(
  simplify(act_H[LLLM](z) - act_H[LLL](act_H[M](z))) = 0,
  "LLLM = LLL o M on H"
 );

 _ASSERT(
  simplify(act_H[MN](z) - act_H[M](act_H[N](z))) = 0,
  "MN = M o N on H"
 );

 _ASSERT(
  simplify(act_H[LMN](z) - act_H[LM](act_H[N](z))) = 0,
  "LMN = LM o N on H"
 );

 _ASSERT(
  simplify(act_H[LLMN](z) - act_H[LLM](act_H[N](z))) = 0,
  "LLMN = LLM o N on H"
 );

 _ASSERT(
  simplify(act_H[LLLMN](z) - act_H[LLLM](act_H[N](z))) = 0,
  "LLLMN = LLLM o N on H"
 );

 _ASSERT(
  simplify(act_H[M](act_H[M](z)) - z) = 0,
  "MM = 1 on H"
 );

 _ASSERT(
  simplify(act_H[LM](act_H[LM](z)) - 
	   act_Pi([7,6],z)) = 0,
  "LMLM on H"
 );

 _ASSERT(
  simplify(act_H[N](act_H[M](act_H[N](act_H[M](z)))) - 
	   act_Pi([6,0,7,6],z)) = 0,
  "NMNM on H"
 );

 NULL;
end:

add_check(check_Pi_action):

######################################################################

check_v_H := proc()
 local i,i0,ii,A,T,err;

 printf("%a()\n",procname);

 for ii in {indices(v_H)} do
  i := op(ii);
  if i = evalf(floor(i)) then
   i := floor(i);
  fi;
  _ASSERT(
   R2_zone(op(net_0["v"][i])) = C_zone(v_H1[i]),
   sprintf("zone for v[%a]",i)
  );
 od:

 for ii in indices(v_H_fraction_offset) do
  i := op(ii);
  i0 := floor(i);
  A := v_H_fraction_offset[i]:
  err := simplify(act_Pi(A,v_H[i0])-v_H[i]);
  _ASSERT(err=0,sprintf("v_H_fraction_offset[%a]",i));
 od:

 for ii in indices(v_H_fraction_transform) do
  i := op(ii);
  i0 := floor(i);
  T := v_H_fraction_transform[i]:
  err := simplify(act_H[T](v_H[i0])-v_H[i]);
  _ASSERT(err=0,sprintf("v_H_fraction_transform[%a]",i));
 od:
end:

add_check(check_v_H):

######################################################################

check_c_H_monodromy := proc()
 local k,err;

 printf("%a()\n",procname);

 for k from 0 to 8 do
  err := c_H[k](t+2*Pi) - act_Pi(c_H_cycle[k],c_H[k](t));
  err := factor(expand(simplify_H(err)));
  _ASSERT(
   err = 0,
   sprintf("monodromy for curve c_H[%d]",k)
   );
 od;
end:

add_check(check_c_H_monodromy):

######################################################################

check_H_F1 := proc()
 local t,err,V;

 printf("%a()\n",procname);

 for t in ['a','b','c','d','e','f'] do 
  err := map(simplify,map2(act_Pi,F1_pairing[t],F1_edge[1,-1,t]) -~ F1_edge[1,1,t]);
  _ASSERT(err = [0,0],sprintf("F1 edge pairing for edge %a",t));
 od;

 V := map(u -> simplify(subs(a_H=a_H_regular,(u[1]+I*u[2]) + u[3]*exp(I*u[4]))),F1_arcs):
 err := max(seq(abs(evalf(V[i] - 3^(-1/4) * exp(I*Pi/12*(2*i-1)))),i=1..12));
 _ASSERT(err < 10.^(-90), "F1 as a regular dodecagon");
 
end:

add_check(check_H_F1):

######################################################################

check_F1_area := proc()
 local A,k,s,c,u,err;
 
 printf("%a()\n",procname);

 _ASSERT(simplify(v_H[11] - (c_H_p[3] - c_H_r[3])) = 0,
         "angle for v[11] on C[3]");

 _ASSERT(simplify(v_H[13] - (c_H_p[3] - c_H_r[3] * exp(-I*v_H_theta))) = 0,
         "angle for v[13] on C[3]");

 _ASSERT(simplify(v_H[13] - (c_H_p[7] - c_H_r[7] * I * exp(-I*v_H_theta))) = 0,
         "angle for v[13] on C[7]");

 _ASSERT(simplify(v_H[ 1] - (c_H_p[7] - c_H_r[7])) = 0,
         "angle for v[1] on C[7]");

 _ASSERT({
  simplify(expand(c_H_simple[ 5](0) - v_H[ 0])),
  simplify(expand(c_H_simple[ 5](1) - v_H[11])),
  simplify(expand(c_H_simple[ 3](0) - v_H[11])),
  simplify(expand(c_H_simple[ 3](1) - v_H[13])),
  simplify(expand(c_H_simple[ 7](0) - v_H[13])),
  simplify(expand(c_H_simple[ 7](1) - v_H[ 1])),
  simplify(expand(c_H_simple[ 1](0) - v_H[ 1])),
  simplify(expand(c_H_simple[ 1](1) - v_H[ 0]))} = {0},
  "Parametrisation of the boundary of F8"
 );

 assume(s > 0);
 
 A := 0;
 for k in [1,5,3,7] do
  c := c_H_simple[k](s);
  u := simplify(expand(Im(c) * diff(Re(c),s)));
  A := A - int(u,s=0..1);
 od;
 A := simplify(expand(A));
 err := simplify(expand(8*A - F1_area));

 _ASSERT(err = 0,"area of F1");
end:

add_check(check_F1_area):

######################################################################

check_Pi_bound := proc()
 local a1,a2,L,err;

 printf("%a()\n",procname);

 a1 := (1+(1-a_H^2)/(1+a_H^2))/2;
 a2 := 4/5*(1+(a_H^2*(3-a_H^2))/(5+2*a_H^2+a_H^4));
 err := factor(map(L -> abs(act_Pi(L,0))^2,F1_H_B) -~ [a1$4,a2$8]);
 
 _ASSERT(err = [0$12],"|gamma(0)|^2 >= 1/2");
end:

add_check(check_Pi_bound);

######################################################################

check_square_diffeo_H := proc()
 local s,u,err,i,j,t0,t1,z0;
 
 printf("%a()\n",procname);

 u[ 6] := [0,0];
 u[ 0] := [1,0];
 u[ 3] := [0,1];
 u[11] := [1,1];

 for i in [6,0,3,11] do
  err := square_diffeo_H(v_H[i]) -~ u[i];
  err := rationalize(simplify(expand(err)));
  err := map(numer,err);
  err := factor(expand(rationalize(map(exp,err))));
  err := factor(expand(rationalize(err)));
  err := map(ln,err);
  _ASSERT(err = [0,0],sprintf("square_diffeo_H(v_H[%d])",i));
 od:

 assume(s > 0);
 
 err := simplify(expand(square_diffeo_H(c_H[1](s))[2]));
 _ASSERT(err = 0,"square_diffeo_H(c_H[1](s))[2] = 0");

 err := simplify(expand(square_diffeo_H(c_H[5](s))[1] - 1));
 _ASSERT(err = 0,"square_diffeo_H(c_H[5](s))[1] = 1");

 err := square_diffeo_H(c_H[0](s))[1]:
 err := max(seq(seq(abs(evalf(subs({a_H=i*0.1,s=j*Pi/12},err))),j=-11..12),i=1..9));
 _ASSERT(err < 10.^(-90),"square_diffeo_H(c_H[0](s))[1] = 0");

 err := square_diffeo_H(c_H[3](s))[2] - 1:
 err := max(seq(seq(abs(evalf(subs({a_H=i*0.1,s=j*Pi/12},err))),j=-11..12),i=1..9));
 _ASSERT(err < 10.^(-90),"square_diffeo_H(c_H[3](s))[2] = 1");

 err := 0;
 for i from 1 to 9 do
  for j from 1 to 9 do
   t0 := [i,j] *~ 0.1;
   z0 := square_diffeo_inv_H0(t0);
   t1 := square_diffeo_H0(z0);
   err := max(err,d2f(t0,t1));
  od;
 od;
 _ASSERT(err < 10.^(-90),"square_diffeo_H o square_diffeo_inv_H");
end:

add_check(check_square_diffeo_H):

######################################################################

check_xi := proc()
 local p,r,s,t,u,v;

 printf("%a()\n",procname);

 assume(r > 1);
 assume(t::real);
 assume(s::real);
 assume(u::real);
 assume(v::real);
 p := r*cos(t) + I*r*sin(t);

 _ASSERT(
  simplify(xi(p,xi(p,u+I*v)) - (u+I*v)) = 0,
  "xi(p,-) is an involution"
 ):

 _ASSERT(
  simplify(xi(p,xi_curve(p,s)) - xi_curve(p,s)) = 0,
  "xi_curve(p,s) is fixed by xi(p,-)"
 ):

 _ASSERT(
  simplify(abs(xi_curve(p,s) - p)^2 - (abs(p)^2-1)) = 0,
  "distance of xi_curve(p,s) from p"
 ):

 _ASSERT(
  simplify(4*abs(diff(xi_curve(p,s),s))^2/(1 - abs(xi_curve(p,s))^2)^2 - 1) = 0,
  "xi_curve(p,-) has speed one"
 ):

 _ASSERT(
  simplify(xi_alt(t,xi_alt(t,u+I*v)) - (u+I*v)) = 0,
  "xi_alt(t,-) is an involution"
 ):

 _ASSERT(
  simplify(xi_alt(t,xi_curve_alt(t,s)) - xi_curve_alt(t,s)) = 0,
  "xi_curve_alt(t,s) is fixed by xi_alt(t,-)"
 ):

 _ASSERT(
  simplify(4*abs(diff(xi_curve_alt(t,s),s))^2/(1 - abs(xi_curve_alt(t,s))^2)^2 - 1) = 0,
  "xi_curve_alt(t,-) has speed one"
 ):

end:

add_check(check_xi):

######################################################################

check_move_inwards := proc()
 local a,b,c,w,x,y,z,theta,alpha,lambda,q,r,err;

 printf("%a()\n",procname);

 assume(b::real,c::real,x::real,y::real,theta::real);
 alpha := b + I*c;
 lambda := exp(I*theta);
 z := x+I*y;
 w := lambda*(z - alpha)/(1 - conjugate(alpha)*z);
 q := simplify(abs(w)^2 - abs(z)^2);
 r := abs(alpha/(1 - conjugate(alpha)*z))^2 * 
      (1 - abs(z)^2) *
      (abs(z - 1/conjugate(alpha))^2 - (abs(alpha)^(-2) - 1));
 err := simplify(q-r);
 _ASSERT(err = 0,"|gamma(z)|^2 - |z|^2");
end:

add_check(check_move_inwards):

######################################################################

check_side_lengths := proc()

 printf("%a()\n",procname);

 _ASSERT(
  is(side_length_H[0] >= 0) and 
  simplify_H(m_hyp_c(v_H[ 3],v_H[ 6])^2 - tanh(side_length_H[ 0]/2)^2) = 0,
  "side_length_H[0]"
 );
 
 _ASSERT(
  is(side_length_H[1] >= 0) and 
  simplify_H(m_hyp_c(v_H[ 0],v_H[ 6])^2 - tanh(side_length_H[ 1]/2)^2) = 0,
  "side_length_H[1]"
 );
 
 _ASSERT(
  is(side_length_H[3] >= 0) and 
  simplify_H(m_hyp_c(v_H[ 3],v_H[11])^2 - tanh(side_length_H[ 3]/2)^2) = 0,
  "side_length_H[3]"
 );
 
 _ASSERT(
  is(side_length_H[5] >= 0) and 
  simplify_H(m_hyp_c(v_H[ 0],v_H[11])^2 - tanh(side_length_H[ 5]/2)^2) = 0,
  "side_length_H[5]"
 );
 
end:

add_check(check_side_lengths):

