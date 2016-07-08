check_R4_action := proc()
 local T,U,x;

 printf("%a()\n",procname);

 for T in G16 do
  for U in G16 do
   _ASSERT(act_R4[T](act_R4[U](x)) = act_R4[G_mult(T,U)](x),"composition for action on R4");
  od:
 od:
 NULL;
end:

add_check(check_R4_action):

######################################################################

check_smoothness := proc()
 local x,xx,n,p,q,sols;

 printf("%a()\n",procname);

 xx := [x[1],x[2],x[3],x[4]];
 n := dg(xx);
 p := (1-a_E^2)/16*(n[1]^2+n[2]^2) + a_E/2*(x[1]^2-x[2]^2)*n[3] - (x[1]^2+x[2]^2)/4*n[4];
 q := x[1]^4 + x[2]^4 + (5/2 - a_E^2)*(x[1]^2+x[2]^2)*x[4]^2;

 _ASSERT(expand(p-q) = 0,"X is smooth 0");

 sols := {solve(subs({x[1]=0,x[2]=0},{op(n),rho(xx)-1}))};
 sols := select(s -> (subs(s,a_E) > 0 and subs(s,a_E) < 1),sols);

 _ASSERT(sols = {},"X is smooth 1");
 NULL;
end:

add_check(check_smoothness):

######################################################################

check_PEX_smoothness := proc()
 local x,r;

 r[ 1] := expand(-1/8*(x[2]*dg(x)[1]+x[1]*dg(x)[2]));
 r[ 2] := expand(a_E/4*(x[2]*dg(x)[1]-x[1]*dg(x)[2]));
 r[ 3] := collect(expand(- a_E*dg(x)[3]/2 - dg(x)[4]/4),xx);
 r[ 4] := collect(expand(  a_E*dg(x)[3]/2 - dg(x)[4]/4),xx);
 r[ 5] := expand(x[1]*x[2]*r[3] - ( x[4]*a_E-x[4]/a_E+(a_E^2-1)/(4*a_E^2)*x[3])*r[2] - 3*x[4]/2*r[1] );
 r[ 6] := expand(x[1]*x[2]*r[4] - (-x[4]*a_E+x[4]/a_E+(a_E^2-1)/(4*a_E^2)*x[3])*r[2] - 3*x[4]/2*r[1] );
 r[ 7] := factor(collect(expand(x[1]*r[3] - ((1/8)*(a_E^2-1)*x[3]/a_E+(3/4)*x[4]*(a_E^2-1))*dg(x)[1]),x[3]));
 r[ 8] := factor(collect(expand(x[2]*r[4] - (-(1/8)*(a_E^2-1)*x[3]/a_E+(3/4)*x[4]*(a_E^2-1))*dg(x)[2]),x[3]));

 # NB the next two lines divide by 2*a_E^2 - 1, which is zero in 
 # the special case a_E = 1/sqrt(2).  All other denominators are
 # invertible for all a_E in (0,1).
 r[ 9] := factor(expand((2/3)*(x[1]^2*r[7]-x[2]*r[6])/(2*a_E^2-1)));
 r[10] := factor(expand((2/3)*(x[2]^2*r[8]-x[1]*r[5])/(2*a_E^2-1)));

 r[11] := expand(dg(x)[1]*a_E^2/2*x[1]^2*( x[3]/a_E+2*x[4]) + 4*a_E^2*r[ 9]);
 r[12] := expand(dg(x)[2]*a_E^2/2*x[2]^2*(-x[3]/a_E+2*x[4]) + 4*a_E^2*r[10]);
 r[13] := expand( a_E/2*dg(x)[1]*x[1]^2*x[4]+2*a_E*r[ 9]);
 r[14] := expand(-a_E/2*dg(x)[2]*x[2]^2*x[4]-2*a_E*r[10]);
 r[15] := expand(x[1]^3*r[4]+(1/4*(1/a_E^2-1))*r[11]-3/2*r[ 9]+(a_E-1/a_E)*r[13]);
 r[16] := expand(x[2]^3*r[3]+(1/4*(1/a_E^2-1))*r[12]-3/2*r[10]-(a_E-1/a_E)*r[14]);
 r[17] := expand(1/12*x[3]*dg(x)[3]-1/6*x[4]*dg(x)[4]);
 r[18] := expand(r[17] * (2*x[4]^3 - r[17]) + 1/9*x[1]*r[9] + 1/9*x[2]*r[10] + 1/18/a_E*x[1]*r[13] - 1/18/a_E*x[2]*r[14]-r[2]^2/72/a_E^2+x[1]*r[11]/144/a_E^2+x[2]*r[12]/144/a_E^2+2/9*r[1]^2);
 r[19] := collect(expand((1/a_E^2-1)*x[3]*dg(x)[4]+3*x[4]*dg(x)[3]),xx);

 _ASSERT(r[15] = x[1]^5 and
         r[16] = x[2]^5 and
         r[18] = x[4]^6 and
         expand(subs({x[1]=0,x[2]=0,x[4]=0},r[19]) - (1/a_E^2-1)^2*x[3]^3) = 0,
         "PEX(a) is smooth except when a = 1/sqrt(2)");
 NULL;
end:

######################################################################

check_symmetry := proc()
 global xx;

 printf("%a()\n",procname);

 _ASSERT(simplify(rho(act_R4[L](xx)) - rho(xx))=0,"lambda preserves S3");
 _ASSERT(simplify(rho(act_R4[M](xx)) - rho(xx))=0,"mu preserves S3");
 _ASSERT(simplify(rho(act_R4[N](xx)) - rho(xx))=0,"nu preserves S3");
 _ASSERT(simplify(g(act_R4[L](xx)) + g(xx))=0,"lambda preserves X");
 _ASSERT(simplify(g(act_R4[M](xx)) + g(xx))=0,"mu preserves X");
 _ASSERT(simplify(g(act_R4[N](xx)) - g(xx))=0,"nu preserves X");

 NULL;
end:

add_check(check_symmetry):

######################################################################

check_fixed_points := proc()
 local T,sols,FP0,FP1,i,s;

 printf("%a()\n",procname);

 for T in {op(G8)} minus {1} do
  sols := solve({rho(xx)=1,g_0(xx)=0,seq(act_R4[T](xx)[i]=xx[i],i=1..4)},{op(xx)});
  FP0 := map(simplify,map(s -> subs(s,xx),{sols}));
  FP1 := map(simplify,map(i -> v_E[i],{op(fixed_vertices[T])}));

  # Substitute a fairly generic value for a_E.  Should be replaced by 
  # something more rigorous.
  FP0 := simplify(subs(a_E=1/7,FP0));
  FP1 := simplify(subs(a_E=1/7,FP1));

  _ASSERT(FP0 = FP1,sprintf("fixed points for %a",T));
 od:
 NULL;
end:

add_check(check_fixed_points):

######################################################################

check_E_F4 := proc()
 local T,i,err,r,x1;
 global x,xx;

 printf("%a()\n",procname);

 err := simplify(rho(retract_F4_E(xx)) - rho(xx)) assuming real;
 _ASSERT(err = 0,"retract_F4_E preserves S3");

 err := simplify(g(retract_F4_E(xx)) - g(xx)) assuming real;
 _ASSERT(err = 0,"retract_F4_E preserves EX(a)");

 for T in H4 do
  _ASSERT(simplify(retract_F4_E(act_R4[T](xx)) -~ retract_F4_E(xx)) = [0$4],
         sprintf("retract_F4_E is %a-invariant",T));
 od:

 for i from 0 to 8 do
  err := add(evalf(Int(is_in_F4_E0_measure(c_E1[i](t)),t=r)),r in [F4_curve_limits[i]]);
  _ASSERT(err < 10^(-8),sprintf("F4_curve_limits[%d]",i));
 od;

 _ASSERT(
  simplify(y_proj(y_lift([y[1],y[2]])) -~ [y[1],y[2]]) = [0,0],
  "y_proj o y_lift"
 );

 x1 := y_lift(y_proj(x));
 
 _ASSERT(
  NF_x([x1[1]^2-x[1]^2,x1[2]^2-x[2]^2,x1[3]-x[3],x1[4]-x[4]]) = [0$4],
  "y_lift o y_proj"
 );

 _ASSERT(
  simplify(y_proj(c_algebraic(t)) -~ 
   [sqrt((1-2*a_E*t)/((t-a_E)*(t-1/a_E))),t]) = [0,0],
  "y_proj o c_alg"
 );

 NULL;
end:

add_check(check_E_F4):

######################################################################

check_E_F16 := proc()
 local T,i,err,a;
 local x,xx;

 printf("%a()\n",procname);

 xx := [x[1],x[2],x[3],x[4]];

 _ASSERT(expand(g_0(xx) - (-2*x[4] - x[3]/a_E*g_1(xx))) = 0,"g_1 formula");
 
 _ASSERT(
  expand(x[4] - (-g_0(xx)/2-x[3]*g_1(xx)/(2*a_E))) = 0,
  "x[4] is in (g_0,g_1)"
 );

 _ASSERT(
  expand(x[2]^2 - x[1]^2 - ((1 - (1+1/a_E^2)/2*x[3]^2)*g_1(xx)-(a_E+1/a_E)/2*x[3]*g_0(xx))) = 0,
  "x[2]^2 - x[1]^2 is in (g0,g1)"
 );

 for T in H8 do 
  _ASSERT(g_1(act_R4[T](xx)) = g_1(xx),"g_1, H8");
 od:

 for T in {op(G16)} minus {op(H8)} do 
  _ASSERT(g_1(act_R4[T](xx)) = -g_1(xx),"g1, H8^c");
 od:
 NULL;
end:

add_check(check_E_F16):

######################################################################

check_retract_F16_E := proc()
 local x,xx,T,i,err,a,samples;

 printf("%a()\n",procname);

 samples := [seq(seq(act_R4[T](x),T in G16),x in inner_quasirational_points)]:

 _ASSERT({op(map(rho,map(retract_F16_E0,samples)))} = {1} and
         {op(map(g0,map(retract_F16_E0,samples)))} = {0},
         "retract_F16_E preserves X"
 );

 _ASSERT(`and`(op(map(is_in_F16_E0,map(retract_F16_E0,samples)))),
         "retract_F16_E maps into F16"
 );

 _ASSERT(
  `and`(seq(seq(evalb(simplify(
         retract_F16_E0(act_R4[T](x)) -~ retract_F16_E0(x)) = [0$4]),
        x in samples),T in G16)),
  "retract_F16_E is G16-invariant"
 );
 NULL;
end:

add_check(check_retract_F16_E):

######################################################################

check_c_param_E := proc()
 local err;

 printf("%a()\n",procname);

 err := max([seq(seq(seq(
         abs(evalf(subs({a_E=0.1*i,t=j*Pi/12},c_param_E[k](c_E[k](t)) - t))),
	  j=-11..11),i=1..9),k=0..8)]);

 _ASSERT(err < 10.^(-98),"c_param_E");
 NULL;
end:

add_check(check_c_param_E):



