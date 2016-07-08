# Check that the sphere of radius r has curvature 1/r^2

check_sphere_curvature := proc()
 local r,t,u;

 printf("%a()\n",procname);

 _ASSERT(
  factor(simplify(brioschi_from_embedding([t,u,sqrt(r^2-t^2-u^2)],t,u) - 1/r^2)) = 0,
  "brioschi curvature of sphere 1"
 );

 _ASSERT(
  factor(simplify(brioschi_from_embedding([r*cos(t)*cos(u),r*cos(t)*sin(u),r*sin(t)],t,u) - 1/r^2)) = 0,
  "brioschi curvature of sphere 2"
 );

 _ASSERT(
  factor(simplify(christoffel_from_embedding([t,u,sqrt(r^2-t^2-u^2)],t,u) - 1/r^2)) = 0,
  "christoffel curvature of sphere 1"
 );

 _ASSERT(
  factor(simplify(christoffel_from_embedding([r*cos(t)*cos(u),r*cos(t)*sin(u),r*sin(t)],t,u) - 1/r^2)) = 0,
  "christoffel curvature of sphere 2"
 );
end:

add_check(check_sphere_curvature):

######################################################################

# Check that our extrinsic formula for the curvature of EX(a) is
# compatible with the above intrinsic formulae

check_EX_curvature := proc()
 local z0,K0,K1,t,u,err;

 printf("%a()\n",procname);

 _ASSERT(
  simplify(brioschi_from_embedding(y_lift([s[1],s[2]]),s[1],s[2]) -
           curvature(y_lift([s[1],s[2]]))) = 0,
  "curvature of EX(a)"
 );

 err := simplify(curvature(xx) - curvature_z([zx[1],zx[2]])):
 err := NF_x(numer(err));

 _ASSERT(err = 0,"curvature of EX(a) in terms of z");
end:

add_check(check_EX_curvature):

######################################################################

check_general_hyperbolic := proc()
 local N,E,E0,E1,K1,t,u;

 N := 7;

 E0 := add(add(E[i,j]*t^i*u^j,j=0..N-i),i=0..N);
 E1 := general_hyperbolic_metric(E0,t,u,N);
 K1 := multi_series(brioschi_formula(E1,0,E1,t,u),N-2,t,u);

 _ASSERT(K1 = -1,"general hyperbolic metric has curvature -1");
end:

add_check(check_general_hyperbolic);