check_Rn := proc()
 local s,t,r,u,v,w,x,y,z,uu,vv,ww,err,err0,err1,err2,err3;

 printf("%a()\n",procname);

 assume(s::real,t::real,r::real);

 uu := [u[1],u[2],u[3],u[4]];
 vv := [v[1],v[2],v[3],v[4]];
 ww := [w[1],w[2],w[3],w[4]];

 _ASSERT(
  expand(dp4(cross_product4(uu,vv,ww),uu)) = 0 and
  expand(dp4(cross_product4(uu,vv,ww),vv)) = 0 and
  expand(dp4(cross_product4(uu,vv,ww),ww)) = 0 and
  cross_product4([1,0,0,0],[0,1,0,0],[0,0,1,0]) = [0,0,0,1],
  "cross_product4"
 ):

 _ASSERT(
  triangle_proj(triangle_lift([x,y])) = [x,y] and
  triangle_lift(triangle_proj([1-x-y,x,y])) = [1-x-y,x,y],
  "triangle_lift and triangle_proj are inverse"
 ):

 _ASSERT(expand(R2_to_C(C_mult(u,v)) - R2_to_C(u) * R2_to_C(v)) = 0,"C_mult");

 _ASSERT(
  simplify(S3_to_R3(R3_to_S3([x[1],x[2],x[3]])) -~ [x[1],x[2],x[3]]) = [0$3],
  "S3_to_R3 o R3_to_S3 = 1"
 );

 err := R3_to_S3(S3_to_R3([x[1],x[2],x[3],x[4]])) -~ [x[1],x[2],x[3],x[4]];
 err := map(numer,map(simplify,err));
 err := map(rem,err,x[1]^2+x[2]^2+x[3]^2+x[4]^2-1,x[4]);

 _ASSERT(err = [0$4],"R3_to_S3 o S3_to_R3 = 1");

 _ASSERT(
  simplify(S1_to_R(R_to_S1(x)) - x) = 0,
  "S1_to_R o R_to_S1 = 1"
 );

 err := R_to_S1(S1_to_R([x[1],x[2]])) -~ [x[1],x[2]];
 err := map(numer,map(simplify,err));
 err := map(rem,err,x[1]^2+x[2]^2-1,x[2]);

 _ASSERT(err = [0$2],"R_to_S1 o S1_to_R = 1");

 err0 := combine(I_to_R(R_to_I(t)) - t);
 err1 := combine(R_to_I(I_to_R(t)) - t) assuming t >= -1 and t <= 1;
 _ASSERT(err0 = 0 and err1 = 0,"R_to_I and I_to_R are inverse");

 _ASSERT(
  simplify(I_to_S1(S1_to_I([cos(t),sin(t)])) - [cos(t),sin(t)]) = [0,0] and
  S1_to_I(I_to_S1(t)) - t = 0,
  "S1_to_I and I_to_S1 are inverse"
 );

 _ASSERT(
  simplify(expand(tan_sum(tan(s),tan(t)) - tan(s+t))) = 0,
  "tan_sum"
 );

 err0 := nm4(unstereo_shift([x[1],x[2],x[3]],[a[1],a[2],a[3],a[4]]))^2 - 1:
 err0 := rem(simplify(err0),a[1]^2+a[2]^2+a[3]^2+a[4]^2-1,a[4]);
 err1 := stereo_shift(unstereo_shift([x[1],x[2],x[3]],[a[1],a[2],a[3],a[4]]),[a[1],a[2],a[3],a[4]]) -~ [x[1],x[2],x[3]]:
 err1 := map(rem,map(numer,simplify(err1)),a[1]^2+a[2]^2+a[3]^2+a[4]^2-1,a[4]);
 err2 := unstereo_shift(stereo_shift([x[1],x[2],x[3],x[4]],[a[1],a[2],a[3],a[4]]),[a[1],a[2],a[3],a[4]]) -~ [x[1],x[2],x[3],x[4]]:
 err2 := map(rem,map(rem,map(numer,simplify(err2)),a[1]^2+a[2]^2+a[3]^2+a[4]^2-1,a[4]),x[1]^2+x[2]^2+x[3]^2+x[4]^2-1,x[4]);
 err3 := stereo_shift([a[1],a[2],a[3],a[4]],[a[1],a[2],a[3],a[4]]);

 _ASSERT(err0 = 0 and err1 = [0$3] and err2 = [0$4] and err3 = [0$3],"(un)stereo_shift");

 _ASSERT(
  expand(H_mult(uu,H_mult(vv,ww)) - H_mult(H_mult(uu,vv),ww)) = [0$4] and
  expand(H_mult(uu,H_conjugate(uu)) -~ nm4(uu)^2 *~ [0,0,0,1]) = [0$4] and
  expand(H_mult([0,0,0,1],uu) -~ uu) = [0$4] and
  expand(H_mult(uu,[0,0,0,1]) -~ uu) = [0$4],
  "quaternions"
 );

 _ASSERT(simplify(nm3(rational_sphere_point(s,t))^2 - 1) = 0,"rational_sphere_point");

end:

add_check(check_Rn):

######################################################################

check_hopf_map := proc()
 local T,uu;
 
 printf("%a()\n",procname);

 uu := [u[1],u[2],u[3],u[4]];

 _ASSERT(
  simplify(nm3(hopf_map(uu))^2 - 1) = 0 and
  simplify(hopf_map(uu) -~ hopf_map(H_mult(uu,[0,0,x,y]))) = [0$3],
  "Hopf map"
 );

 _ASSERT(
   {seq(factor(hopf_map(act_R4[T](xx)) - act_hopf[T](hopf_map(xx))),T in [1,LL,LM,LLLM,LN,LLLN,MN,LLMN])} =
    {[0$3]},
   "equivariance of the Hopf map"
 );
 
end:

add_check(check_hopf_map):
