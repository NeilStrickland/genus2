check_rational := proc()
 local s,t;
 
 printf("%a()\n",procname);

 assume(s::real,t::real);
 
 _ASSERT(simplify(rho(c_rational[0](t)) - 1) = 0,"c_rational[0] lands in S3");
 _ASSERT(simplify(rho(c_rational[1]([s,t])) - 1) = 2*s^2 + t^2 - 1,"c_rational[1] lands in S3");
 _ASSERT(simplify(rho(c_rational[2]([s,t])) - 1) = 2*s^2 + t^2 - 1,"c_rational[2] lands in S3");
 _ASSERT(simplify(rho(c_quasirational[1](t)) - 1) = 0,"c_quasirational[1] lands in S3");
 _ASSERT(simplify(rho(c_quasirational[2](t)) - 1) = 0,"c_quasirational[2] lands in S3");
 _ASSERT(simplify(rho(c_quasirational[3]([s,t])) - 1) = 3*s^2 + t^2 - 1,"c_quasirational[3] lands in S3");
 _ASSERT(simplify(rho(c_quasirational[4]([s,t])) - 1) = 3*s^2 + t^2 - 1,"c_quasirational[4] lands in S3");

_ASSERT(simplify(g0(c_rational[0](t)))          = 0,"c_rational[0] lands in EX^*");
 _ASSERT(simplify(g0(c_rational[1]([s,t])))      = 0,"c_rational[1] lands in EX^*");
 _ASSERT(simplify(g0(c_rational[2]([s,t])))      = 0,"c_rational[2] lands in EX^*");
 _ASSERT(simplify(g0(c_quasirational[1](t)))     = 0,"c_quasirational[1] lands in EX^*");
 _ASSERT(simplify(g0(c_quasirational[2](t)))     = 0,"c_quasirational[2] lands in EX^*");
 _ASSERT(simplify(g0(c_quasirational[3]([s,t]))) = 0,"c_quasirational[3] lands in EX^*");
 _ASSERT(simplify(g0(c_quasirational[4]([s,t]))) = 0,"c_quasirational[4] lands in EX^*");

 _ASSERT(c_check_E0[0](c_rational[0](t))          = 0.,"c_rational[0] lands in C[0]");
 _ASSERT(c_check_E0[1](c_rational[1](t))          = 0.,"c_rational[1] lands in C[1]");
 _ASSERT(c_check_E0[2](c_rational[2](t))          = 0.,"c_rational[2] lands in C[2]");
 _ASSERT(c_check_E0[1](c_quasirational[1](t))     = 0.,"c_quasirational[1] lands in C[1]");
 _ASSERT(c_check_E0[2](c_quasirational[2](t))     = 0.,"c_quasirational[2] lands in C[2]");
 _ASSERT(c_check_E0[3](c_quasirational[3]([s,t])) = 0.,"c_quasirational[3] lands in C[3]");
 _ASSERT(c_check_E0[4](c_quasirational[4]([s,t])) = 0.,"c_quasirational[4] lands in C[4]");

 _ASSERT({op(map(type,two_circle(100),[rational,rational]))} = {true},
         "two_circle() produces rational pairs");

 _ASSERT({op(map(u -> 2*u[1]^2 + u[2]^2,two_circle(100)))} = {1},
         "two_circle() solves 2 s^2 + t^2 = 1");

 _ASSERT({op(map(type,three_circle(100),[rational,rational]))} = {true},
         "three_circle() produces rational pairs");

 _ASSERT({op(map(u -> 3*u[1]^2 + u[2]^2,three_circle(100)))} = {1},
         "three_circle() solves 3 s^2 + t^2 = 1");

 _ASSERT(expand(g0(quasirational_lift([s[1],s[2]]))) = 0,
         "g0 on quasirational_lift");

 _ASSERT(expand(rho(quasirational_lift([s[1],s[2]])) - 1) = 0,
         "rho on quasirational lift");

 _ASSERT(expand(quasirational_proj(quasirational_lift([s[1],s[2]])) -~ [s[1],s[2]]) = [0,0],
         "quasirational_proj o quasirational_lift");

 _ASSERT({op(map(type,inner_quasirational_projections,[rational,rational]))} = {true},
         "inner_quasirational_projections are rational");

 _ASSERT({op(map(is_quasirational,inner_quasirational_points))} = {true},
         "inner_quasirational_points are quasirational");

 _ASSERT({op(map(g0,inner_quasirational_points)),
          op(map(u -> rho(u) - 1,inner_quasirational_points))} = {0},
         "inner_quasirational_points lie in EX^*");
end:

add_check(check_rational):

######################################################################

check_rational_elliptic := proc()
 local s,t;
 
 printf("%a()\n",procname);

 _ASSERT(simplify(elliptic_r[0](c_E0[5](t))) = 0,
         "elliptic rational relation 0");
	 
 _ASSERT(simplify(elliptic_r[1](c_E0[5](t))) = 0,
         "elliptic rational relation 1");
	 
 _ASSERT(
  expand(
   elliptic_rel(elliptic_u(x),elliptic_t(x)) - 
    (elliptic_a[0](x) * elliptic_r[0](x) + 
     elliptic_a[1](x) * elliptic_r[1](x) + 
     elliptic_a[2](x) * elliptic_r[0](x) * elliptic_r[1](x))) = 0,
  "elliptic rational relation 2"
 ); 
end:

add_check(check_rational_elliptic):

