check_r_rels := proc()
 local errs;

 printf("%a()\n",procname);

 errs := simplify({
  subs(y[2] = y2r[1],ry[1]) - r[1],
  subs(y[2] = y2r[2],ry[2]) - r[2],
  subs(r[1] = ry[1],y2r[1]) - y[2],
  subs(r[2] = ry[2],y2r[2]) - y[2]
 }) assuming r[1] > 0 and r[2] > 0;

 _ASSERT(errs = {0},"Relations involving r[1] and r[2]");
 
 errs :=
  expand(map(e -> subs({r[1]=rx[1],r[2]=rx[2],y[1]=yx0[1],y[2]=yx0[2]},
          rhs(e)^2 - lhs(e)^2),root_rule));

 _ASSERT(errs = {0},"Relations for rewriting square roots in terms of r[1] and r[2]");

end:

add_check(check_r_rels):

######################################################################

check_t_proj := proc()
 printf("%a()\n",procname);

 _ASSERT(simplify(rho(t_lift(t)) - 1) = 0 and
         simplify(g0(t_lift(t))) = 0,
  "t_lift lands in EX(1/sqrt(2))"
 );

 _ASSERT(simplify(t_proj(t_lift(t)) -~ [t[1],t[2]]) = [0,0],
         "t_proj o t_lift");

 _ASSERT(
  simplify(t_proj(c_E0[0](t)) -~ [0,cos(2*t)^2]) = [0,0] and
  simplify(t_proj(c_E0[1](t)) -~ [(1+sin(t)^2)*cos(t)^2,0]) = [0,0] and
  simplify(t_proj(c_E0[3](t)) -~ [(1+sin(t)^2/3)*cos(t)^2,1]) = [0,0] and
  simplify(t_proj(c_E0[5](t)) -~ [1,(1-cos(t))^2/4]) = [0,0],
  "t_proj on boundary of F16"
 );
end:

add_check(check_t_proj):

######################################################################

check_oval := proc()

 printf("%a()\n",procname);

 _ASSERT(
  expand(g0([x[1],0,x[3],x[4]]) - sqrt(2)*(x[3]-sqrt(2)*x[4])*oval_g(xx)) = 0,
  "Factorisation of g(x) when x[2]=0"
 );

 _ASSERT(
   factor(expand(oval_g(xx) - add(oval_m[i]*dp4(oval_e[i],xx)^2,i=1..4))) = 0,
   "Diagonalisation of oval_g"
 );

 _ASSERT(
  factor(expand(rho(xx) - add(dp4(oval_e[i],xx)^2,i=1..4))) = 0,
  "The oval_e basis is orthonormal"
 );

 _ASSERT(
  simplify(oval_g(c_E0[5](t))) = 0,
  "oval_g(c[5](t)) = 0"
 );

 _ASSERT(
  simplify(oval_g(c_E0[7](t))) = 0,
  "oval_g(c[7](t)) = 0"
 );

end:

add_check(check_oval):

######################################################################

check_c_alt := proc()
 local err,i,k,l,m,c,T;
 
 printf("%a()\n",procname);

 _ASSERT({seq(simplify(rho(c_alt[k](t))-1),k=5..8)} = {0},
         "c_alt[k](t) lies in S^3");
	 
 _ASSERT({seq(simplify(g0(c_alt[k](t))),k=5..8)} = {0},
         "c_alt[k](t) lies in EX^*");

 err := NULL;
 for k from 5 to 8 do
  for T in [L,M,N] do
   l,m,c := op(act_c_data[T,k]);
   err := err,simplify(act_R4[T](c_alt[k](t)) -~ c_alt[l](m*t+c));
  od:
 od:

 _ASSERT({err} = {[0$4]},"group action on c_alt");

 err := NULL;
 for k from 5 to 8 do
  for i from 0 to 13 do
   if v_on_c[i,k] <> NULL then
    err := err,simplify(c_alt[k](v_on_c[i,k]) -~ v_E0[i]);
   fi:
  od:
 od:

 _ASSERT({err} = {[0$4]},"vertices on c_alt");
 
 _ASSERT(
  simplify(oval_g(c_alt[5](t))) = 0,
  "oval_g(c_alt[5](t)) = 0"
 );

 _ASSERT(
  simplify(oval_g(c_alt[7](t))) = 0,
  "oval_g(c_alt[7](t)) = 0"
 );

end:

add_check(check_c_alt):

######################################################################

check_elliptic_b := proc()
 global x0,r0,r1,a0,a1,a2,err;

 printf("%a()\n",procname);

 _ASSERT(
  simplify(elliptic_b_poly(elliptic_b_proj(c_E0[5](t)))) = 0,
  "elliptic_b relations 0"
 );

 x0 := [x[1],0,x[3],x[4]];
 r0 := rho(x0) - 1;
 r1 := oval_g(x0);
 a0 := -6*sqrt(2)*(x[3]+sqrt(2)*x[4])*x[4]^3;
 a1 := (1-x[3]^2-x[4]^2)*(x[3]^2-10*x[4]^2+9+x[3]*x[4]/sqrt(2));
 a2 := x[3]^2+2*x[4]^2+x[3]*x[4]/sqrt(2)-x[1]^2+9;
 err := expand(elliptic_b_poly(elliptic_b_proj(x0))
               - a0*r0 - a1*r1 - a2 *r0*r1);

 _ASSERT(err = 0,"elliptic_b relations 1");
end:

add_check(check_elliptic_b):

######################################################################

check_polar_curves := proc()
 _ASSERT(
  simplify(c_E0[0](t) -~ S3_polar([0,t,0])) = [0$4],
  "polar expression for c_E0[0](t)"
 );

 _ASSERT(
  simplify(c_E0[1](t) -~ S3_polar([Pi/2-t,Pi/4,0])) = [0$4],
  "polar expression for c_E0[1](t)"
 );

 _ASSERT(
  simplify(c_E0[3](t) -~ S3_polar([Pi/2-t,Pi/2,-arcsin(1/sqrt(3))])) = [0$4],
  "polar expression for c_E0[3](t)"
 );
end:

add_check(check_polar_curves):

######################################################################

check_tangent_a := proc()
 printf("%a()\n",procname);

 _ASSERT(
  NF_x0(dp4(xx,tangent_a(xx)))=0,
  "tangent_a(x) perpendicular to x"
 );

 _ASSERT(
  NF_x0(dp4(dg0(xx),tangent_a(xx)))=0,
  "tangent_a(x) perpendicular to n(x)"
 );
end:

add_check(check_tangent_a):

check_tangent_b := proc()
 local err0,err1,err2,x0,x1,n1,u1;

 printf("%a()\n",procname);

 err0 := 0;
 err1 := 0;
 err2 := 0;
 
 for x0 in inner_quasirational_points do
  x1 := evalf(x0);
  n1 := dg0(x1);
  n1 := evalf(n1/nm4(n1));
  u1 := unit_tangent_b(x1);
  err0 := max(err0,abs(dp4(u1,x1)));
  err1 := max(err1,abs(dp4(u1,n1)));
  err2 := max(err2,abs(dp4(u1,u1) - 1));
 od:

 _ASSERT(err0 < 10.^(-90),"tangent_b(x) perpendicular to x");
 _ASSERT(err1 < 10.^(-90),"tangent_b(x) perpendicular to n(x)");
 _ASSERT(err2 < 10.^(-90),"tangent_b(x) is a unit vector");
end:

add_check(check_tangent_b):

######################################################################

check_cubic_chart := proc()
 local u,uu,V,B,y,err;

 printf("%a()\n",procname);

 uu := [seq(u[i],i=1..4)];
 V := tdeg(op(xx),op(uu));
 B := Basis([rho(xx)-1,g0(xx),dp4(xx,uu),dp4(dg0(xx),uu)],V);

 y := cubic_chart0(xx,[seq(e*u[i],i=1..4)]);

 err := rho(y) - 1;
 err := convert(series(err,e=0,4),polynom,e);
 err := numer(factor(expand(err)));
 err := NormalForm(err,B,V);

 _ASSERT(err=0,"rho on cubic_chart0");

 err := g0(y);
 err := convert(series(err,e=0,4),polynom,e);
 err := numer(factor(expand(err)));
 err := NormalForm(err,B,V);

 _ASSERT(err=0,"g0 on cubic_chart0");

 err := y -~ xx -~ (e *~ uu);
 err := map(series,err,e=0,2);
 err := map(convert,err,polynom,e);
 err := map(NormalForm,err,B,V);

 _ASSERT(err = [0$4],"cubic_chart0 to first order");
end:

add_check(check_cubic_chart):

######################################################################

check_extra_annular_charts := proc()
 local A0,A1,A2;

 printf("%a()\n",procname);

 A0 := annular_chart0[3]([t,u]);
 A1 := map(diff,A0,t);
 A2 := map(diff,A0,u);

 _ASSERT(
  convert(series(combine(simplify(rho(A0)-1)),u=0,4),polynom,u) = 0,
  "annular_chart[3] lands in S3"
 );

 _ASSERT(
  convert(series(combine(simplify(g0(A0))),u=0,4),polynom,u) = 0,
  "annular_chart[3] lands in EX(a)"
 );

 _ASSERT(
  convert(series(combine(simplify(dp4(A1,A2))),u=0,3),polynom,u) = 0,
  "annular_chart[3] is approximately conformal 1"
 );

 _ASSERT(
  convert(series(combine(simplify(dp4(A1,A1)-dp4(A2,A2))),u=0,4),polynom,u) = 0,
  "annular_chart[3] is approximately conformal 2"
 );

 A0 := annular_chart0[5]([t,u]);
 A1 := map(diff,A0,t);
 A2 := map(diff,A0,u);

 _ASSERT(
  convert(series(combine(simplify(rho(A0)-1)),u=0,4),polynom,u) = 0,
  "annular_chart[5] lands in S3"
 );

 _ASSERT(
  convert(series(combine(simplify(g0(A0))),u=0,4),polynom,u) = 0,
  "annular_chart[5] lands in EX(a)"
 );

 _ASSERT(
  convert(series(combine(simplify(dp4(A1,A2))),u=0,1),polynom,u) = 0,
  "annular_chart[5] is approximately conformal 1"
 );

 _ASSERT(
  convert(series(combine(simplify(dp4(A1,A1)-dp4(A2,A2))),u=0,2),polynom,u) = 0,
  "annular_chart[5] is approximately conformal 2"
 );

end:

add_check(check_extra_annular_charts):
