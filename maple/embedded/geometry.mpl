# g_stereo(u) = 0 iff  g(unstereo(u)) = 0
#@ g_stereo 
g_stereo := (u::RR_3) -> g([2*u[1],2*u[2],2*u[3],u[1]^2+u[2]^2+u[3]^2-1]);

# Partial derivatives of g wrt x[1], x[2], x[3], x[4]
#@ dg 
dg := (x::RR_4) -> [
  2*x[1]*(x[3]/a_E-2*x[4]),
 -2*x[2]*(x[3]/a_E+2*x[4]),
 2*(1/a_E^2-1)*x[3]*x[4]+1/a_E*(x[1]^2-x[2]^2),
 (1/a_E^2-1)*x[3]^2-2*(x[1]^2+x[2]^2)-6*x[4]^2
];

#@ square_norm_of_dg 
square_norm_of_dg := (x::RR_4) -> 
  4*(1/a_E^3-5/a_E)*(x[1]^2-x[2]^2)*x[3]*x[4]+
  (8-2/a_E^2)*x[1]^2*x[2]^2+
  (4+1/a_E^2)*(x[1]^4+x[2]^4)+
  (4*x[3]^2+40*x[4]^2)*(x[1]^2+x[2]^2)+
  (16-20/a_E^2+4/a_E^4)*x[3]^2*x[4]^2+
  (1-1/a_E^2)^2*x[3]^4+
  36*x[4]^4:

#@ conformal_twist 
conformal_twist := (x::RR_4,u::RR_4) -> cross_product4(x,dg(x),u) /~ sqrt(square_norm_of_dg(x));

# The two curves omega[1] and omega[2] meet only at [0,0,0,1]
# I think that their union is a deformation retract of one component
# of the complement of X in S^3
#@ omega
omega[1] := (t::RR) -> [sin(t)/sqrt(1+a_E^2), 0, (cos(t)-1)/(a_E+1/a_E), (a_E+cos(t)/a_E)/(a_E+1/a_E)];
omega[2] := (t::RR) -> [0, sin(t)/sqrt(1+a_E^2),-(cos(t)-1)/(a_E+1/a_E), (a_E+cos(t)/a_E)/(a_E+1/a_E)];

# move_towards_X() takes a point in R^4 and returns a point in S^3
# that is closer to the surface X.
#@ move_towards_X 
move_towards_X := proc(x::RR0_4)
 local y,u,n,e;
 y := evalf(Vector([x[1],x[2],x[3],x[4]]));
 u := Vector(dg1(y));
 n := sqrt(add(u[i]^2,i=1..4));
 u := u/n;
 e := min(evalf(g1(y))/n,0.1);
 y := y - e*u;
 y := y/sqrt(add(y[i]^2,i=1..4));
 return(convert(y,list));
end:

# move_to_X() takes a point in R^4 and returns a point in X obtained
# by applying move_towards_X() repeatedly.

#@ move_to_X_tolerance 
move_to_X_tolerance := 10.^(-99):

#@ move_to_X 
move_to_X := proc(x::RR0_4)
 local n,a,b,d;
 a := x;
 n := 100;
 d := evalf(abs(g1(a)) + abs(rho(a)-1));
 while (d > move_to_X_tolerance and n > 0) do
  a := move_towards_X(a);
  n := n-1;
  d := evalf(abs(g1(a)));
 od:
 return(convert(a,list)); 
end:

#@ random_X_point 
random_X_point := proc()
 local r,rr,T,i,j,s,t,a;
 r := rand(0..15);
 rr := rand(0..1000);
 T := G16[r()+1];
 i := r();
 j := r();
 s := evalf(rr()/1000);
 t := evalf(rr()/1000);
 a := act_R4[T](w_lift1([s,t]));
 a := move_to_X(a);
 return(a);
end:

# midpoint_X(a,b) returns a point on X roughly half way between a and b.
#@ midpoint_X 
midpoint_X := (a::RR0_4,b::RR0_4) -> move_to_X([seq((a[i]+b[i])/2,i=1..4)]):

# unnormalised_tangent_complement_projector(x) is a symmetric matrix P
# whose image is the subspace of R^4 orthogonal to the tangent space 
# T_xX, such that P^2 is a positive multiple of P.

#@ unnormalised_tangent_complement_projector 
unnormalised_tangent_complement_projector := (x::RR_4) -> 
 map(expand,Matrix(4,4,[seq(seq(x[i]*x[j]*square_norm_of_dg(x) + dg(x)[i]*dg(x)[j],i=1..4),j=1..4)])):

# unnormalised_tangent_projector(x) is a symmetric matrix Q
# whose image is the tangent space T_xX, such that Q^2 is a positive
# multiple of Q.

#@ unnormalised_tangent_projector 
unnormalised_tangent_projector := (x::RR_4) -> 
 square_norm_of_dg(x) * IdentityMatrix(4) - unnormalised_tangent_complement_projector(x);

# tangent_projector(x) is the orthogonal projection of R^4 onto the
# tangent space T_xX.  In other words, it is the unique symmetric matrix 
# R satisfying R^2=R such that the image of R is T_xX.

#@ tangent_projector 
tangent_projector := (x::RR_4) -> unnormalised_tangent_projector(x)/square_norm_of_dg(x);

# We now define various functions that produce tangent vectors.
# tangent_u and tangent_v are continuous everywhere but unnormalised
# Each of them vanishes at two points.  tangent_u1 and tangent_v1 are
# normalised versions, so each is undefined at two points. 
# tangent_frame_u returns an orthonormal basis for the tangent space,
# whose first entry is tangent_u1.  tangent_frame_v is similar.
# tangent_frame returns either tangent_frame_u or tangent_frame_v,
# depending on whether tangent_u or tangent_v has larger norm.

#@ tangent_uu 
tangent_uu := (x::RR_4) -> [x[4],-x[3],x[2],-x[1]];

#@ tangent_u 
tangent_u := proc(x::RR_4) 
 local uu,nn;
 uu := tangent_uu(x);
 nn := dg(x);
 return(uu -~ (dp4(uu,nn)/dp4(nn,nn)) *~ nn);
end:

#@ unit_tangent_u 
unit_tangent_u := proc(x::RR_4) 
 local u;
 u := tangent_u(x);
 return(u /~ sqrt(rho(u)));
end:

#@ full_frame_u 
full_frame_u := proc(x::RR_4)
 local n,r,u,v;

 n := evalf(dg(x));
 n := n /~ nm4(n);
 u := unit_tangent_u(x);
 v := cross_product4(x,n,u);
 return [x,n,u,v];
end:

#@ tangent_frame_u 
tangent_frame_u := proc(x::RR_4)
 local f;
 f := full_frame_u(x);
 return [f[3],f[4]];
end:

#@ tangent_vv 
tangent_vv := (x::RR_4) -> [-x[3],x[4],x[1],-x[2]];

#@ tangent_v 
tangent_v := proc(x::RR_4) 
 local vv,nn;
 vv := tangent_vv(x);
 nn := dg(x);
 return(vv -~ (dp4(vv,nn)/dp4(nn,nn)) *~ nn);
end:

#@ unit_tangent_v 
unit_tangent_v := proc(x::RR_4) 
 local v;
 v := tangent_v(x);
 return(v /~ sqrt(rho(v)));
end:

#@ full_frame_v 
full_frame_v := proc(x::RR_4)
 local n,r,u,v;

 n := evalf(dg(x));
 n := n /~ nm4(n);
 u := unit_tangent_v(x);
 v := cross_product4(x,n,u);
 return [x,n,u,v];
end:

#@ tangent_frame_v 
tangent_frame_v := proc(x::RR_4)
 local f;
 f := full_frame_v(x);
 return [f[3],f[4]];
end:

# tighten_once accepts a list of points on X, thought of as representing
# a closed loop, and returns a slightly different list, which should be 
# closer to a geodesic loop.

#@ tighten_once 
tighten_once := proc(xs)
 local n,xxs,Pxxs;
 n := nops(xs);
 xxs := map(Vector,[xs[n],op(xs),xs[1]]);
 Pxxs := map(evalf,map(tangent_projector,xxs));
 map(move_to_X,[seq(convert(xxs[i] + Pxxs[i] . (xxs[i+1]/2 + xxs[i-1]/2 - xxs[i]),list), i = 2 .. n+1)]); 
end:

# quadratic_chart accepts a point x in X and a tangent vector u at x
# It returns a point y of the form x + u + O(|u|^2) such that 
# \|y\|-1 and g(y) are O(|u|^3).

#@ quadratic_chart 
quadratic_chart := proc(x::RR_4,u::RR_4)
 (1 - dp4(u,u)/2) *~ x +~ u -~ dp4(dg(u),x) *~ dg(x)/~ square_norm_of_dg(x);
end:

# Below we define the curvature function.  In order to use some 
# preliminary definitions without polluting the global namespace,
# we define and immediately invoke an anonymous procedure.

#@ curvature
proc()
 local x,xx,n,m,p;
 global curvature:

 xx := [seq(x[i],i=1..4)];
 n := unapply(dg(xx),x);
 m := unapply(Matrix([seq([seq(diff(g(xx),x[i],x[j]),j=1..4)],i=1..4)]),x);
 p := unapply(Matrix(expand([seq(cross_product4(xx,n(x),convert(Column(m(x),i),list)),i=1..4)])),x);

 curvature := unapply(1 - NF_x(expand(Trace(p(x) . p(x))))/NF_x(2*square_norm_of_dg(xx)^2),x);
end():

#@ laplacian 
laplacian := proc(p)
 local dp,ddp,n,m,rr,r1,r2,L0,L1,L2,L3,L4,L;
 dp  := [seq(diff(p,x[i]),i=1..4)];
 ddp := [seq([seq(diff(p,x[i],x[j]),j=1..4)],i=1..4)];
 n := [seq(diff(g(xx),x[i]),i=1..4)];
 m := [seq([seq(diff(g(xx),x[i],x[j]),j=1..4)],i=1..4)];
 rr := dp4(n,n);
 r1 := add(m[i,i],i=1..4);
 r2 := add(add(n[i]*m[i][j]*n[j],i=1..4),j=1..4);

 L0 := add(ddp[i,i],i=1..4);
 L1 := - add(add(x[i]*x[j]*ddp[i,j],j=1..4),i=1..4);
 L2 := - add(add(n[i]*n[j]*ddp[i,j],j=1..4),i=1..4)/rr;
 L3 := -2*add(x[i]*dp[i],i=1..4); 
 L4 := (-r1/rr + r2/rr^2) * add(n[i] * dp[i],i=1..4);
 L := L0+L1+L2+L3+L4;
 return(L);
end:

#@ laplacian_z_C 
laplacian_z_C := table();

laplacian_z_C[0] := (a_E^4*z[1]^2*z[2]+a_E^4*z[1]^2-12*a_E^4*z[1]*z[2]-4*a_E^4*z[1]+4*a_E^4*z[2]-2*a_E^2*z[1]^2*z[2]+4*a_E^4-2*a_E^2*z[1]^2+4*a_E^2*z[1]*z[2]+z[1]^2*z[2]+z[1]^2)^2;

laplacian_z_C[1] := -6*a_E^8*z[1]^5*z[2]^2-12*a_E^8*z[1]^5*z[2]+104*a_E^8*z[1]^4*z[2]^2-6*a_E^8*z[1]^5+142*a_E^8*z[1]^4*z[2]-224*a_E^8*z[1]^3*z[2]^2+24*a_E^6*z[1]^5*z[2]^2+50*a_E^8*z[1]^4-376*a_E^8*z[1]^3*z[2]+96*a_E^8*z[1]^2*z[2]^2+48*a_E^6*z[1]^5*z[2]-240*a_E^6*z[1]^4*z[2]^2-160*a_E^8*z[1]^3+320*a_E^8*z[1]^2*z[2]-32*a_E^8*z[1]*z[2]^2+24*a_E^6*z[1]^5-328*a_E^6*z[1]^4*z[2]+128*a_E^6*z[1]^3*z[2]^2-36*a_E^4*z[1]^5*z[2]^2+240*a_E^8*z[1]^2-96*a_E^8*z[1]*z[2]-104*a_E^6*z[1]^4+400*a_E^6*z[1]^3*z[2]-72*a_E^4*z[1]^5*z[2]+168*a_E^4*z[1]^4*z[2]^2-160*a_E^8*z[1]+32*a_E^8*z[2]+128*a_E^6*z[1]^3-128*a_E^6*z[1]^2*z[2]-36*a_E^4*z[1]^5+236*a_E^4*z[1]^4*z[2]-32*a_E^4*z[1]^3*z[2]^2+24*a_E^2*z[1]^5*z[2]^2+32*a_E^8-32*a_E^6*z[1]^2+60*a_E^4*z[1]^4-216*a_E^4*z[1]^3*z[2]+48*a_E^2*z[1]^5*z[2]-32*a_E^2*z[1]^4*z[2]^2-64*a_E^4*z[1]^3+32*a_E^4*z[1]^2*z[2]+24*a_E^2*z[1]^5-56*a_E^2*z[1]^4*z[2]-6*z[1]^5*z[2]^2+16*a_E^4*z[1]^2-8*a_E^2*z[1]^4+32*a_E^2*z[1]^3*z[2]-12*z[1]^5*z[2]-6*z[1]^5+6*z[1]^4*z[2]+2*z[1]^4;

laplacian_z_C[2] := 8*a_E^8*z[1]^3*z[2]^3+12*a_E^8*z[1]^3*z[2]^2-112*a_E^8*z[1]^2*z[2]^3+12*a_E^8*z[1]^3*z[2]-264*a_E^8*z[1]^2*z[2]^2+1120*a_E^8*z[1]*z[2]^3-32*a_E^6*z[1]^3*z[2]^3-120*a_E^8*z[1]^2*z[2]+912*a_E^8*z[1]*z[2]^2-320*a_E^8*z[2]^3-56*a_E^6*z[1]^3*z[2]^2+416*a_E^6*z[1]^2*z[2]^3+336*a_E^8*z[1]*z[2]-608*a_E^8*z[2]^2-32*a_E^6*z[1]^3*z[2]+480*a_E^6*z[1]^2*z[2]^2-448*a_E^6*z[1]*z[2]^3+40*a_E^4*z[1]^3*z[2]^3-288*a_E^8*z[2]-8*a_E^6*z[1]^3+136*a_E^6*z[1]^2*z[2]-384*a_E^6*z[1]*z[2]^2+88*a_E^4*z[1]^3*z[2]^2-176*a_E^4*z[1]^2*z[2]^3+40*a_E^6*z[1]^2-64*a_E^6*z[1]*z[2]+64*a_E^6*z[2]^2+48*a_E^4*z[1]^3*z[2]-296*a_E^4*z[1]^2*z[2]^2-16*a_E^2*z[1]^3*z[2]^3-64*a_E^6*z[1]+96*a_E^6*z[2]+16*a_E^4*z[1]^3-168*a_E^4*z[1]^2*z[2]+80*a_E^4*z[1]*z[2]^2-56*a_E^2*z[1]^3*z[2]^2+32*a_E^6-16*a_E^4*z[1]^2+48*a_E^4*z[1]*z[2]-48*a_E^2*z[1]^3*z[2]+48*a_E^2*z[1]^2*z[2]^2-8*a_E^2*z[1]^3+24*a_E^2*z[1]^2*z[2]+12*z[1]^3*z[2]^2+8*a_E^2*z[1]^2+20*z[1]^3*z[2];

laplacian_z_C[3] := -4*z[1]*(z[1]*z[2]+z[1]-1)*(a_E^4*z[1]^2-4*a_E^4*z[1]+4*a_E^4-2*a_E^2*z[1]^2+z[1]^2)*(a_E^4*z[1]^2*z[2]+a_E^4*z[1]^2-12*a_E^4*z[1]*z[2]-4*a_E^4*z[1]+4*a_E^4*z[2]-2*a_E^2*z[1]^2*z[2]+4*a_E^4-2*a_E^2*z[1]^2+4*a_E^2*z[1]*z[2]+z[1]^2*z[2]+z[1]^2);

laplacian_z_C[4] := -16*z[1]*z[2]*(2*a_E^4*z[1]*z[2]-a_E^4*z[1]-4*a_E^4*z[2]+2*a_E^4-2*a_E^2*z[1]+z[1])*(a_E^4*z[1]^2*z[2]+a_E^4*z[1]^2-12*a_E^4*z[1]*z[2]-4*a_E^4*z[1]+4*a_E^4*z[2]-2*a_E^2*z[1]^2*z[2]+4*a_E^4-2*a_E^2*z[1]^2+4*a_E^2*z[1]*z[2]+z[1]^2*z[2]+z[1]^2);

laplacian_z_C[5] := 16*z[2]*(a_E^4*z[1]^2*z[2]+a_E^4*z[1]^2-12*a_E^4*z[1]*z[2]-4*a_E^4*z[1]+4*a_E^4*z[2]-2*a_E^2*z[1]^2*z[2]+4*a_E^4-2*a_E^2*z[1]^2+4*a_E^2*z[1]*z[2]+z[1]^2*z[2]+z[1]^2)*(a_E^4*z[1]*z[2]-4*a_E^4*z[2]^2-4*a_E^4*z[2]-a_E^2*z[1]*z[2]^2-a_E^2*z[1]+a_E^2*z[2]+a_E^2+z[1]*z[2]);

#@ laplacian_z 
laplacian_z := proc(p)
 (laplacian_z_C[1] * diff(p,z[1]) + 
  laplacian_z_C[2] * diff(p,z[2]) + 
  laplacian_z_C[3] * diff(p,z[1],z[1]) + 
  laplacian_z_C[4] * diff(p,z[1],z[2]) + 
  laplacian_z_C[5] * diff(p,z[2],z[2]))/laplacian_z_C[0]; 
end:

######################################################################

#@ gauss_map 
gauss_map := (x::RR_4) -> [op(1..3,H_quotient(dg(x)/~nm4(dg(x)),x))];

#@ act_gauss
act_gauss[1]    := (u) -> [ u[1],  u[2],  u[3]];
act_gauss[LL]   := (u) -> [-u[1], -u[2],  u[3]];
act_gauss[LM]   := (u) -> [ u[2],  u[1], -u[3]];
act_gauss[LLLM] := (u) -> [-u[2], -u[1], -u[3]];
act_gauss[LN]   := (u) -> [ u[2],  u[1],  u[3]];
act_gauss[LLLN] := (u) -> [-u[2], -u[1],  u[3]];
act_gauss[MN]   := (u) -> [ u[1],  u[2], -u[3]];
act_gauss[LLMN] := (u) -> [-u[1], -u[2], -u[3]];



