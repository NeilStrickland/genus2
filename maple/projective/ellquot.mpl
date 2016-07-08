# This file contains code related to two elliptic curves which are
# quotients of PX(a).  One is denoted by E^+ in LaTeX and Ep in Maple,
# the other is E^- or Em.  The curves Ep and Em are defined as
# subvarieties of CP^4, but there are affine curves Ep_0 and
# Em_0 in C^2 which are isomorphic to dense open subvarieties of
# Ep and Em respectively.  These are quartic curves which pass through
# the origin, and we take the origin (not a point at infinity) as the
# zero element for a group structure.

#@ is_equal_CP2
is_equal_CP2 := proc(z,w)
 local i,j;
 for i from 1 to 2 do
  for j from i+1 to 3 do
   if simplify(conjugate(simplify(z[i]*w[j] - z[j]*w[i]))) <> 0 then
    return(false);
   fi;
  od;
 od;
 return(true);
end:

#@ d_CP2
d_CP2 := (u,v) -> sqrt(add(add(abs(u[i]*v[j]-u[j]*v[i])^2,j=i+1..3),i=1..2)/
                       (add(abs(u[i])^2,i=1..3)*add(abs(v[i])^2,i=1..3)));

#@ d_CP2_0
d_CP2_0 := (u,v) -> d_CP2([u[1],u[2],1],[v[1],v[2],1]);

#@ ss
ss := [seq(s[i],i=1..5)];

# The curves Ep_0 and Em_0 have equations y^2 = q_Ep(x) and 
# y^2 = q_Em(x) respectively, where q_Ep and q_Em are given below.

q_Ep := (x) -> 2*x*(x-1)*((1/a_P+a_P)^2/4*x^2-1); #@ q_Ep
q_Em := (x) -> 2*x*(x-1)*((1/a_P-a_P)^2/4*x^2+1); #@ q_Em

is_member_Ep_0 := (yx) -> yx[1]^2 - q_Ep(yx[2]); #@ is_member_Ep_0
is_member_Em_0 := (yx) -> yx[1]^2 - q_Em(yx[2]); #@ is_member_Em_0

#@ Ep_rel
Ep_rel[0] := (z) -> z[1]^2 - 2*(z[4]-z[3])*((1/a_P+a_P)^2/4*z[4]-z[2]); 
Ep_rel[1] := (z) -> z[2]*z[4] - z[3]^2;                                 

#@ Em_rel
Em_rel[0] := (z) -> z[1]^2 - 2*(z[4]-z[3])*((1/a_P-a_P)^2/4*z[4]+z[2]);
Em_rel[1] := (z) -> z[2]*z[4] - z[3]^2;

is_member_Ep := (z) -> simplify_P([seq(Ep_rel[i](z),i=0..1)]) = [0$2]; #@ is_member_Ep
is_member_Em := (z) -> simplify_P([seq(Em_rel[i](z),i=0..1)]) = [0$2]; #@ is_member_Em

#@ is_equal_Ep_list
is_equal_Ep_list := (z,w) -> [seq(seq(z[i]*w[j]-z[j]*w[i],j=i+1..4),i=1..3)]; 

#@ is_equal_Ep
is_equal_Ep := proc(z,w)
 local L,u;

 L := is_equal_Ep_list(z,w);
 for u in L do 
  if simplify_P(conjugate(simplify_P(expand(u)))) <> 0 then
   return(false);
  fi;
 od;
 return(true);
end:

#@ is_equal_Em_list
is_equal_Em_list := (z,w) -> [seq(seq(z[i]*w[j]-z[j]*w[i],j=i+1..4),i=1..3)];

#@ is_equal_Em
is_equal_Em := proc(z,w)
 local L,u;

 L := is_equal_Em_list(z,w);
 for u in L do 
  if simplify_P(conjugate(simplify_P(expand(u)))) <> 0 then
   return(false);
  fi;
 od;
 return(true);
end:

Ep_vars := tdeg(z[1],z[2],z[3],z[4]);                  #@ Ep_vars
Ep_rels := Basis([Ep_rel[0](z),Ep_rel[1](z)],Ep_vars); #@ Ep_rels
NF_Ep := (u) -> NormalForm(u,Ep_rels,Ep_vars);         #@ NF_Ep

Em_vars := tdeg(z[1],z[2],z[3],z[4]);                  #@ Em_vars
Em_rels := Basis([Em_rel[0](z),Em_rel[1](z)],Em_vars); #@ Em_rels
NF_Em := (u) -> NormalForm(u,Em_rels,Em_vars);         #@ NF_Em

j_Ep     := (yx) -> [yx[1],1,yx[2],yx[2]^2]; #@ j_Ep
j_inv_Ep := (x)  -> [x[1]/x[2],x[3]/x[2]];   #@ j_inv_Ep

j_Em     := (yx) -> [yx[1],1,yx[2],yx[2]^2]; #@ j_Em
j_inv_Em := (x)  -> [x[1]/x[2],x[3]/x[2]];   #@ j_inv_Em

#@ d_CP3
d_CP3 := (u,v) -> sqrt(add(add(abs(u[i]*v[j]-u[j]*v[i])^2,j=i+1..4),i=1..3)/
                       (add(abs(u[i])^2,i=1..4)*add(abs(v[i])^2,i=1..4)));

# This function takes values in Ep_0, and has second component exp(I*t).
# It is well-behaved for small positive values of t.

#@ l_Ep
l_Ep := (t) -> [(1/2+1/2*I)*sqrt(2)*(-a_P^2+1)*sqrt(tan((1/2)*t))*
                  sqrt(1+I*tan((1/2)*t)*(1-a_P)^2/(1+a_P)^2)*
                  sqrt(1+I*tan((1/2)*t)*(1+a_P)^2/(1-a_P)^2)*
                   exp((1/2*I)*t)/((1-I*tan((1/2)*t))^(3/2)*a_P),
                 exp(I*t)];
		 
# The function P_to_Ep_0 gives a map from (the affine model) of
# PX(a) to Ep_0, which induces an isomorphism from
# PX(a)/\mu to Ep.

#@ P_to_Ep_0
P_to_Ep_0 := proc(wz)
 local w,z;
 w,z := op(wz);
 return([2*w*(1-z)/((1+z^2)^2),2*z/(1+z^2)]);
end:

#@ Ep_to_P_0
Ep_to_P_0 := proc(yx) 
 local y,x;
 y,x := op(yx);
 return [y*((2-x)*sqrt(1-x^2)-(x+2)*(x-1))/(x^3*(x-1)),(1+sqrt(1-x^2))/x];
end:

#@ P_to_Ep
P_to_Ep := (z) -> [2*(z[2]-z[3])*z[1],
                   z[2]^2+2*z[2]*z[4]+z[3]*z[5],
		   2*z[2]*(z[3]+z[5]),
		   4*z[2]*z[4]];

# The function P_to_Em_0 gives a map from (the affine model) of
# PX(a) to Em_0, which induces an isomorphism from
# PX(a)/\lambda\mu to Em.

#@ P_to_Em_0
P_to_Em_0 := proc(wz)
 local w,z;
 w,z := op(wz);
 return([-(1+I)*sqrt(2)*w*(I+z)/((1-z^2)^2),2*I*z/(1-z^2)]);
end:

#@ Em_to_P_0
Em_to_P_0 := proc(yx) 
 local y,x;
 y,x := op(yx);
 return [(1+I)/sqrt(2)*y*((x-2)*sqrt(1-x^2)-(x+2)*(x-1))/(x^3*(x-1)), -I*(1-sqrt(1-x^2))/x];
end:

#@ P_to_Em
P_to_Em := (z) -> [(1-I)*sqrt(2)*z[1]*(z[2]-I*z[3]),
                   z[2]^2-2*z[2]*z[4]+z[3]*z[5],
		   2*I*z[2]*(z[3]-z[5]),
		   -4*z[2]*z[4]];

#@ P_to_Epm_0
P_to_Epm_0 := (wz) -> [op(P_to_Ep_0(wz)),op(P_to_Em_0(wz))];

#@ PJ_trans
PJ_trans := (u) -> [op(Em_0_trans[1]([u[1],u[2]])),op(Ep_0_trans[1]([u[3],u[4]]))];

#@ PJ_trans
P_to_PJ := proc(wz,u_)
 local w,z,u;

 w,z := op(wz);
 u := `if`(nargs>1,u_,sqrt(z));
 
 return [
  1/sqrt(2)*u*(1-z)*(2*bp_P^2*(1-z)^2-bm_P^2*(w/u+1+z^2))/(bm_P^2*z-  (1-z)^2)^2,
    (w/u - (1-z)^2)/(bm_P^2*z-  (1-z)^2)/2,
  (1+I)/2  *u*(I+z)*(2*bm_P^2*(I+z)^2+bp_P^2*(w/u+1-z^2))/(bp_P^2*z+I*(I+z)^2)^2,
  I*(w/u + (I+z)^2)/(bp_P^2*z+I*(I+z)^2)/2
 ];

end:

#@ PJ_to_Epm_0
PJ_to_Epm_0 := (u) -> [op(Em_0_to_Ep_0([u[1],u[2]])),op(Ep_0_to_Em_0([u[3],u[4]]))];


######################################################################
# Note that only the normaliser of M acts on Ep, and only the 
# normaliser of LM acts on Em.

#@ act_Ep_0
act_Ep_0[1]    := yx -> [ yx[1],yx[2]];
act_Ep_0[LL]   := yx -> [-yx[1],yx[2]];
act_Ep_0[M]    := yx -> [ yx[1],yx[2]];
act_Ep_0[LLM]  := yx -> [-yx[1],yx[2]];
act_Ep_0[N]    := yx -> [ conjugate(yx[1]),conjugate(yx[2])];
act_Ep_0[LLN]  := yx -> [-conjugate(yx[1]),conjugate(yx[2])];
act_Ep_0[MN]   := yx -> [ conjugate(yx[1]),conjugate(yx[2])];
act_Ep_0[LLMN] := yx -> [-conjugate(yx[1]),conjugate(yx[2])];

#@ act_Ep
act_Ep[1]    := z -> [ z[1], z[2], z[3], z[4]];
act_Ep[LL]   := z -> [-z[1], z[2], z[3], z[4]];
act_Ep[M]    := z -> [ z[1], z[2], z[3], z[4]];
act_Ep[LLM]  := z -> [-z[1], z[2], z[3], z[4]];
act_Ep[N]    := z -> map(conjugate,[ z[1], z[2], z[3], z[4]]);
act_Ep[LLN]  := z -> map(conjugate,[-z[1], z[2], z[3], z[4]]);
act_Ep[MN]   := z -> map(conjugate,[ z[1], z[2], z[3], z[4]]);
act_Ep[LLMN] := z -> map(conjugate,[-z[1], z[2], z[3], z[4]]);

#@ act_Em_0
act_Em_0[1]    := yx -> yx;
act_Em_0[LL]   := yx -> [-yx[1],yx[2]];
act_Em_0[LM]   := yx -> yx;
act_Em_0[LLLM] := yx -> [-yx[1],yx[2]];
act_Em_0[LN]   := yx -> [ conjugate(yx[1]),conjugate(yx[2])];
act_Em_0[LLLN] := yx -> [-conjugate(yx[1]),conjugate(yx[2])];
act_Em_0[MN]   := yx -> [ conjugate(yx[1]),conjugate(yx[2])];
act_Em_0[LLMN] := yx -> [-conjugate(yx[1]),conjugate(yx[2])];

#@ act_Em
act_Em[1]    := z -> [ z[1], z[2], z[3], z[4]];
act_Em[LL]   := z -> [-z[1], z[2], z[3], z[4]];
act_Em[LM]   := z -> [ z[1], z[2], z[3], z[4]];
act_Em[LLLM] := z -> [-z[1], z[2], z[3], z[4]];
act_Em[LN]   := z -> map(conjugate,[ z[1], z[2], z[3], z[4]]);
act_Em[LLLN] := z -> map(conjugate,[-z[1], z[2], z[3], z[4]]);
act_Em[MN]   := z -> map(conjugate,[ z[1], z[2], z[3], z[4]]);
act_Em[LLMN] := z -> map(conjugate,[-z[1], z[2], z[3], z[4]]);

######################################################################

#@ v_Ep_0
v_Ep_0[ 0] := [0,0];
v_Ep_0[ 1] := [0,0];
v_Ep_0[ 2] := [a_P-1/a_P,-1];
v_Ep_0[ 3] := [0,1];
v_Ep_0[ 4] := [1/a_P-a_P,-1];
v_Ep_0[ 5] := [0,1];
v_Ep_0[ 6] := [infinity,infinity];
v_Ep_0[ 7] := [infinity,infinity];
v_Ep_0[ 8] := [infinity,infinity];
v_Ep_0[ 9] := [infinity,infinity];
v_Ep_0[10] := [0,-2*a_P/(1+a_P^2)];
v_Ep_0[11] := [0, 2*a_P/(1+a_P^2)];
v_Ep_0[12] := [0,-2*a_P/(1+a_P^2)];
v_Ep_0[13] := [0, 2*a_P/(1+a_P^2)];

v_Ep_0[14] := [ 2*sqrt(2)*a_P*(1-a_P^2)^2/(1+a_P^2)^3, 4*a_P^2/(1+a_P^2)^2];
v_Ep_0[15] := [-2*sqrt(2)*a_P*(1-a_P^2)^2/(1+a_P^2)^3, 4*a_P^2/(1+a_P^2)^2];

#@ v_Ep
for i from 0 to 15 do
 if not(has(v_Ep_0[i],infinity)) then
  v_Ep[i] := j_Ep(v_Ep_0[i]);
 fi;
od:

v_Ep[ 6] := [ a_P+1/a_P,0,0,-sqrt(2)];
v_Ep[ 7] := [-a_P-1/a_P,0,0,-sqrt(2)];
v_Ep[ 8] := [-a_P-1/a_P,0,0,-sqrt(2)];
v_Ep[ 9] := [ a_P+1/a_P,0,0,-sqrt(2)];

#@ omega_Em
omega_Em := (1+I)/sqrt(2);

#@ v_Em_0
v_Em_0[ 0] := [0,0];
v_Em_0[ 1] := [0,0];
v_Em_0[ 2] := [infinity,infinity];
v_Em_0[ 3] := [infinity,infinity];
v_Em_0[ 4] := [infinity,infinity];
v_Em_0[ 5] := [infinity,infinity];
v_Em_0[ 6] := [ (a_P+1/a_P),-1];
v_Em_0[ 7] := [0,1];
v_Em_0[ 8] := [-(a_P+1/a_P),-1];
v_Em_0[ 9] := [0,1];
v_Em_0[10] := [0,-2*I*a_P/(1-a_P^2)];
v_Em_0[11] := [0, 2*I*a_P/(1-a_P^2)];
v_Em_0[12] := [0, 2*I*a_P/(1-a_P^2)];
v_Em_0[13] := [0,-2*I*a_P/(1-a_P^2)];

#@ v_Em
for i from 0 to 13 do
 if not(has(v_Em_0[i],infinity)) then
  v_Em[i] := j_Em(v_Em_0[i]);
 fi;
od:

v_Em[ 2] := [ a_P-1/a_P,0,0,-sqrt(2)];
v_Em[ 3] := [ a_P-1/a_P,0,0,-sqrt(2)];
v_Em[ 4] := [-a_P+1/a_P,0,0,-sqrt(2)];
v_Em[ 5] := [-a_P+1/a_P,0,0,-sqrt(2)];

for i from 0 to 8 do 
 c_Ep_0[i] := unapply(simplify(P_to_Ep_0(pq_P(c_P[i](t)))),t); #@ c_Ep_0
 c_Em_0[i] := unapply(simplify(P_to_Em_0(pq_P(c_P[i](t)))),t); #@ c_Em_0
 c_Ep[i]   := unapply(simplify(P_to_Ep(c_P[i](t))),t);         #@ c_Ep
 c_Em[i]   := unapply(simplify(P_to_Em(c_P[i](t))),t);         #@ c_Em
od:

######################################################################

# We make Ep into a group in the usual way, with the point (0,0) in Ep_0
# as the zero element, and similarly for Em.  The points of order two are
# as follows.

#@ Ep_0_e
Ep_0_e[0] := [0,0];
Ep_0_e[1] := [0,1];
Ep_0_e[2] := [0, 2*a_P/(1+a_P^2)];
Ep_0_e[3] := [0,-2*a_P/(1+a_P^2)];

#@ Ep_0_e
Em_0_e[0] := [0,0];
Em_0_e[1] := [0,1];
Em_0_e[2] := [0, 2*I*a_P/(1-a_P^2)];
Em_0_e[3] := [0,-2*I*a_P/(1-a_P^2)];

for i from 0 to 3 do
 Ep_e[i] := j_Ep(Ep_0_e[i]); #@ Ep_e
 Em_e[i] := j_Em(Em_0_e[i]); #@ Em_e
od:

# The map Ep_0_trans[i] is u |-> u + Ep_0_e[i].

#@ Ep_0_trans
Ep_0_trans[0] := (yx) -> yx;
Ep_0_trans[1] := (yx) -> [
 4*a_P^2*(1-a_P^2)^2 * yx[1]/((1+a_P^2)^2*yx[2] - 4*a_P^2)^2,
 4*a_P^2*(yx[2]-1)/((1+a_P^2)^2*yx[2] - 4*a_P^2)
];
Ep_0_trans[2] := (yx) -> [
 -8*a_P^2*(1-a_P)^2/(1+a_P^2)*yx[1]/((1-a_P)^2*yx[2]+2*a_P*(1-yx[2]))^2,
 2*a_P/(1+a_P^2)*(2*a_P-(1+a_P^2)*yx[2])/((1-a_P)^2*yx[2]+2*a_P*(1-yx[2]))
];
Ep_0_trans[3] := (yx) -> [
 -8*a_P^2*(1+a_P)^2/(1+a_P^2)*yx[1]/((1+a_P)^2*yx[2]-2*a_P*(1-yx[2]))^2,
 2*a_P/(1+a_P^2)*(2*a_P+(1+a_P^2)*yx[2])/((1+a_P)^2*yx[2]-2*a_P*(1-yx[2]))
];

#@ Ep_trans
Ep_trans[0] := (z) -> [z[1],z[2],z[3],z[4]];

Ep_trans[1] := (z) -> [bm_P^2*z[1],
                       z[2]-2*bp_P^2*z[3]+bp_P^4*z[4],
		       z[2] - (1+bp_P^2)*z[3] + bp_P^2*z[4],
		       z[2]-2*z[3]+z[4]] /~ bm_P^2;
Ep_trans[2] := (z) -> [ 2*(1-bp_P)*z[1],
                       bp_P*z[2]+(2*bp_P^2-4*bp_P)*z[3]+(bp_P^3-4*bp_P^2+4*bp_P)*z[4],
		       z[2]-2*z[3]-(bp_P^2-2*bp_P)*z[4],
		       z[2]/bp_P-2*z[3]+bp_P*z[4]] /~ ( 2*(1-bp_P));
Ep_trans[3] := (z) -> [-2*(1+bp_P)*z[1],
                       bp_P*z[2]-(2*bp_P^2+4*bp_P)*z[3]+(bp_P^3+4*bp_P^2+4*bp_P)*z[4],
		       -z[2]+2*z[3]+(bp_P^2+2*bp_P)*z[4],
		       z[2]/bp_P+2*z[3]+bp_P*z[4]] /~ (-2*(1+bp_P));

# The map Em_0_trans[i] is u |-> u + Em_0_e[i].

#@ Em_0_trans
Em_0_trans[0] := (yx) -> yx;
Em_0_trans[1] := (yx) -> [
 -4*a_P^2*(1+a_P^2)^2*yx[1]/((1-a_P^2)^2*yx[2]+4*a_P^2)^2,
 -(4*a_P^2*(yx[2]-1)/((1-a_P^2)^2*yx[2]+4*a_P^2))
];
Em_0_trans[2] := (yx) -> [
 -8*a_P^2*(I+a_P)^2/(1-a_P^2)*yx[1]/((I+a_P)^2*yx[2]+2*I*a_P*(yx[2]-1))^2,
 -2*I*a_P/(1-a_P^2)*(2*I*a_P-(1-a_P^2)*yx[2])/((I+a_P)^2*yx[2]+2*I*a_P*(yx[2]-1))
];
Em_0_trans[3] := (yx) -> [
 -8*a_P^2*(I-a_P)^2/(1-a_P^2)*yx[1]/((I-a_P)^2*yx[2]-2*I*a_P*(yx[2]-1))^2,
 -2*I*a_P/(1-a_P^2)*(2*I*a_P+(1-a_P^2)*yx[2])/((I-a_P)^2*yx[2]-2*I*a_P*(yx[2]-1))
];

#@ Em_trans
Em_trans[0] := (z) -> [z[1],z[2],z[3],z[4]];

Em_trans[1] := (z) -> [bp_P^2*z[1],
                       -z[2]-2*bm_P^2*z[3]-bm_P^4*z[4],
		       -z[2] + (1-bm_P^2)*z[3] + bm_P^2*z[4],
		       -z[2] + 2*z[3] - z[4]] /~ (bp_P^2);

Em_trans[2] := (z) -> [(I-bm_P)*z[1],
                       -(-bm_P/2*z[2]+bm_P*(bm_P-2*I)^2/2*z[4]+(bm_P*2+I*bm_P^2)*z[3]),
                       -(-I/2*z[2]+I*z[3]-(I/2*bm_P^2+bm_P)*z[4]),
                       -(z[2]/(2*bm_P)+I*z[3]-bm_P/2*z[4])] /~ (I-bm_P);

Em_trans[3] := (z) -> [(I+bm_P)*z[1],
                        (-bm_P/2*z[2]+bm_P*(bm_P+2*I)^2/2*z[4]+(bm_P*2-I*bm_P^2)*z[3]),
                        (I/2*z[2]-I*z[3]-(-I/2*bm_P^2+bm_P)*z[4]),
                        (z[2]/(2*bm_P)-I*z[3]-bm_P/2*z[4])] /~ ((bm_P+I));

######################################################################
# It turns out that Ep and Em are isogenous.  It should be possible to
# see this abstractly using some representation theory and duality 
# theory for Jacobians.  More specifically, the function 
# Ep_0_to_Em_0 defines an isogeny Ep -> Em with kernel generated by
# the point Ep_0_e[1], whereas Em_0_to_Ep_0 is an isogeny in the 
# opposite direction with kernel generated by Em_0_e[1].

#@ Ep_0_to_Em_0
Ep_0_to_Em_0 := proc(yx)
 local y,x;
 y,x := op(yx);
 return [y*sqrt(2)*((1-x)^2+bm_P^2*x^2)/((1-x)^2-bm_P^2*x^2)^2,
         2*x*(x-1)/((1-x)^2-bm_P^2*x^2)];
end:

#@ Em_0_to_Ep_0
Em_0_to_Ep_0 := proc(yx)
 local y,x;
 y,x := op(yx);
 return [y*sqrt(2)*((1-x)^2-bp_P^2*x^2)/((1-x)^2+bp_P^2*x^2)^2,
         2*x*(x-1)/((1-x)^2+bp_P^2*x^2)];
end:

#@ Ep_to_Em
Ep_to_Em := (z) -> [
 sqrt(2)*z[1]*(z[2]-2*z[3]+bp_P^2*z[4]),
 (2-bp_P^2)^2*z[4]^2 +(z[2]-2*z[3]+2*(2-bp_P^2)*z[4])*(z[2]-2*z[3]),
 2*(z[2]-bp_P^2*z[4])*(z[4]-z[3])+4*(z[4]^2-2*z[3]*z[4]+z[2]*z[4]),
 4*(z[4]^2-2*z[3]*z[4]+z[2]*z[4])
];

#@ Em_to_Ep
Em_to_Ep := (z) -> [
 sqrt(2)*z[1]*(z[2]-2*z[3]-bm_P^2*z[4]),
 (2+bm_P^2)^2*z[4]^2 +(z[2]-2*z[3]+2*(2+bm_P^2)*z[4])*(z[2]-2*z[3]),
 2*(z[2]+bm_P^2*z[4])*(z[4]-z[3])+4*(z[4]^2-2*z[3]*z[4]+z[2]*z[4]),
 4*(z[4]^2-2*z[3]*z[4]+z[2]*z[4])
];

#@ Epm_w
Epm_w[1] := [ 2*bm_P/(1+bm_P)^2, 1/(1+bm_P)];
Epm_w[2] := [ 2*bm_P/(1-bm_P)^2, 1/(1-bm_P)];
Epm_w[3] := [-2*bm_P/(1+bm_P)^2, 1/(1+bm_P)];
Epm_w[4] := [-2*bm_P/(1-bm_P)^2, 1/(1-bm_P)];

######################################################################

#@ P_0_to_J_0
P_0_to_J_0 := proc(wz)
 local w,z,ym,xm,yp,xp;

 w,z := op(wz);

 ym := 1/sqrt(2)*sqrt(z)*(1-z)*(2*bp_P^2*(1-z)^2-bm_P^2*(w/sqrt(z)+1+z^2))/(bm_P^2*z-  (1-z)^2)^2; 
 yp := (1+I)/2  *sqrt(z)*(I+z)*(2*bm_P^2*(I+z)^2+bp_P^2*(w/sqrt(z)+1-z^2))/(bp_P^2*z+I*(I+z)^2)^2;
 xm :=   (w/sqrt(z) - (1-z)^2)/(bm_P^2*z-  (1-z)^2)/2;
 xp := I*(w/sqrt(z) + (I+z)^2)/(bp_P^2*z+I*(I+z)^2)/2;

 return [ym,xm,yp,xp];
end:

######################################################################
# The curves Ep_0 and Em_0 can be parametrised using the 
# Weierstrass P-function and its derivative.  More specifically, the
# function C_to_Ep_0 gives a universal covering of Ep by C, with kernel
# generated by latt_a (in R) and latt_b (in iR).  Similarly, the function 
# C_to_Em_0 gives a universal covering of Em by C, with kernel 
# generated by latt_e and latt_f.  The numbers latt_c (in R) and
# latt_d (in iR) generate a subgroup of index 2 in the kernel.

Wg2p := 4*(1/3+bp_P^2);                                  #@ Wg2p
Wg3p := 8/3*(1/9-bp_P^2);                                #@ Wg3p
WPp  := (z) -> WeierstrassP(z/sqrt(2),Wg2p,Wg3p):        #@ WPp
WPPp := (z) -> WeierstrassPPrime(z/sqrt(2),Wg2p,Wg3p):   #@ WPPp

Wg2m := 4*(1/3-bm_P^2);                                  #@ Wg2m
Wg3m := 8/3*(1/9+bm_P^2);                                #@ Wg3m
WPm  := (z) -> WeierstrassP(I*z/sqrt(2),Wg2m,Wg3m):      #@ WPm
WPPm := (z) -> WeierstrassPPrime(I*z/sqrt(2),Wg2m,Wg3m): #@ WPPm

C_to_Ep_0 := (z) -> [- WPPp(z)/(WPp(z)+1/3)^2/sqrt(2), 1/(WPp(z)+1/3)]; #@ C_to_Ep_0
C_to_Em_0 := (z) -> [I*WPPm(z)/(WPm(z)+1/3)^2/sqrt(2), 1/(WPm(z)+1/3)]; #@ C_to_Em_0

latt_a := ap_period;                       #@ latt_a
latt_b := am_period*I;                     #@ latt_b
latt_c := ap_period*sqrt(2);               #@ latt_c
latt_d := am_period*sqrt(2)*I;             #@ latt_d
latt_e := (ap_period+am_period*I)/sqrt(2); #@ latt_e
latt_f := (ap_period-am_period*I)/sqrt(2); #@ latt_f

# These are isomorphisms C -> R^2 that send the relevant lattices to Z^2

#@ div_Ep
div_Ep  := (z) -> [   Im(z/latt_b0)/Im(latt_a0/latt_b0),
                      Im(z/latt_a0)/Im(latt_b0/latt_a0)];

#@ div_Em
div_Em  := (z) -> [   Im(z/latt_e0)/Im(latt_f0/latt_e0),
                      Im(z/latt_f0)/Im(latt_e0/latt_f0)];

#@ div_Emq
div_Emq := (z) -> [-2*Im(z/latt_c0)/Im(latt_d0/latt_c0),
                    2*Im(z/latt_d0)/Im(latt_c0/latt_d0)];

######################################################################

#@ d_Ep

d_Ep[0] := (t) -> [(1+a_P)^2*sin(t)*sqrt((1-a_P)^2/a_P + (1+a_P)^2/a_P*sin(t/2)^2)/(4*a_P),
                 (1/a_P)*((a_P+1)^2/4*cos(t)-(1-a_P)^2/4)];

d_Ep[1] := (t) -> [I*(1-a_P)^2*sin(t)*sqrt((1+a_P)^2/a_P+(1-a_P)^2/a_P*sin(t/2)^2)/(4*a_P),
                 (1/a_P)*((1+a_P)^2/4-(1-a_P)^2/4*cos(t))];

######################################################################
# Near v[0], the functions below are inverse to C_to_Ep_0 and C_to_Em_0

#@ Ep_0_to_C
Ep_0_to_C := (yx) ->
 2/(sqrt(a_P)-1/sqrt(a_P))*
  EllipticF((sqrt(a_P)-1/sqrt(a_P))*yx[1]/2/(1-yx[2])/sqrt(1-yx[2]^2/4*(a_P+1/a_P)^2),
             I*(1+a_P)/(1-a_P));

#@ Em_0_to_C
Em_0_to_C := (yx) -> 
 sqrt(2)*(1-I)/(1/sqrt(a_P)-I*sqrt(a_P))*
  EllipticF((1+I)/sqrt(2)*yx[1]/2*(1/sqrt(a_P)-I*sqrt(a_P))/(1-yx[2])/sqrt(1+yx[2]^2/4*(1/a_P-a_P)^2),
            -I*(1+I*a_P)/(1-I*a_P));


######################################################################

#@ find_ca
#@ ca
#@ ca_string

find_ca := proc()
 global i,ca,ca_string;
 ca[0] := fsolve(Re(C_to_Ep0_0(t*latt_a0+latt_b0/2)[2])+1=0,t=0.271..0.272);
 ca[1] := Re(fsolve(Ep0_0_trans[1](C_to_Ep0_0(t*latt_a0 + latt_b0/2))[2] - v_Ep0_0[14][2],t = 0.33));
 ca[2] := brent_fsolve((t) -> table(["err" = Re(1/C_to_Em0_0(t*latt_c0)[2])]),0.37,0.4,false,false,10.^(-90))[1];
 ca[3] := brent_fsolve((t) -> table(["err" = Re(3*C_to_Em0_0(latt_c0/2+t*latt_d0)[2]-1)]),0.1,0.2,false,false,10.^(-90))[1];
 ca[4] := evalf(1/4 - 1/sqrt(8) + ca[1]/sqrt(2));

 ca_string := "";
 for i from 0 to 4 do 
  ca_string := cat(ca_string,sprintf("ca[%d] := %A;\n",i,ca[i]));
 od:
end:

ca[0] := .2712198345337267409183751260826859683620639254846839852546832980918572068609141666452521341087864403;
ca[1] := .3380191917841065905435356360705057573602229775275223773576731512721457153287874464581375181454782068;
ca[2] := .3856099172668633704591875630413429841810319627423419926273416490459286034304570833226260670543932199;
ca[3] := .1690819268841186594794124671806576109783450143041054095003132968033979158300508721043901321239984256;
ca[4] := .1354622720884641394270016572560535181460733569063074476782555872580258479260520219330188552098104961;

#@ v_TEp
v_TEp[ 0] := 0;
v_TEp[ 1] := 0;
v_TEp[ 2] :=  ca[0]*latt_a0 + latt_b0/2;
v_TEp[ 3] :=    1/2*latt_a0 + latt_b0/2;
v_TEp[ 4] := -ca[0]*latt_a0 + latt_b0/2;
v_TEp[ 5] :=    1/2*latt_a0 + latt_b0/2;
v_TEp[10] := latt_b0/2;
v_TEp[12] := latt_b0/2;

######################################################################
# These are maps R -> C such that the composite with C_to_Ep_0 is
# approximately the same as the composite
#
#     c[k]
#  R ------> PX_0 ----> Ep_0

#@ c_TEp_approx
c_TEp_approx[0] := (t) -> (1/2 - (1/2 - ca[0])*cos(t))*latt_a0+latt_b0/2;
c_TEp_approx[1] := (t) -> latt_b0 * t/Pi + c_TEp_a[1,1]*sin(t) + c_TEp_a[1,2]*sin(2*t)*I + c_TEp_a[1,3]*sin(3*t);
c_TEp_approx[2] := (t) -> latt_b0 * t/Pi - c_TEp_a[1,1]*sin(t) + c_TEp_a[1,2]*sin(2*t)*I - c_TEp_a[1,3]*sin(3*t);
c_TEp_approx[3] := (t) -> latt_a0/2 + t*latt_b0/Pi + c_TEp_a[3,2] * sin(2*t) * I;
c_TEp_approx[4] := (t) -> latt_b0/2 - c_TEp_a[4,1] * sin(t) - c_TEp_a[4,3] * sin(3*t);
c_TEp_approx[5] := (t) -> t*latt_a0/(2*Pi) + c_TEp_a[5,1] * sin(t) + c_TEp_a[5,2] * sin(2*t) + c_TEp_a[5,3] * sin(3*t);
c_TEp_approx[6] := (t) -> t*latt_b0/(2*Pi) + (c_TEp_a[6,1] * sin(t) + c_TEp_a[6,2] * sin(2*t) + c_TEp_a[6,3] * sin(3*t)) * I;
c_TEp_approx[7] := unapply(c_TEp_approx[5]( t),t);
c_TEp_approx[8] := unapply(c_TEp_approx[6](-t),t);

#@ c_TEp_a
c_TEp_a[1,1] := 0.6247347995397494979304990644922278959925160558822906310627061638472069242981199313035975369049495378;
c_TEp_a[1,2] := 0.1271025486963173673053533258315918977758877826348357864853356763913426014752169850359977340543651160;
c_TEp_a[1,3] := 0.04381746206649332486668820474497225695049799820309213349946374104678578367977790428958078654155281204;
c_TEp_a[3,2] := 0.1042805083196201573848763132278556884018577757032944082115538163208094109327358933030888052414138109;
c_TEp_a[4,1] := 0.4968296088425958313893062468630599274335873218089671291381643204020161460960238235450241440954199888;
c_TEp_a[4,3] := 0.03071315983358916926430176920663159406977183930465613106149221110811287954564497454251589873134635628;
c_TEp_a[5,1] := 0.03427325618888229644323397515847565305897064093054365551658309204485785383605755460381667885437777016;
c_TEp_a[5,2] := 0.002482478177069518908910207898024965359042928275607876444506771571486982421239589861216569732858358200;
c_TEp_a[5,3] := 0.000238565053005790472482742854541616044857974248413386109265566228277239808644583552185718631302795458;
c_TEp_a[6,1] := 0.0558713002227124085540383146452715943529179849726750467583063953157534082410475171178334557306278623;
c_TEp_a[6,2] := 0.00332541457691846974120052631026377039577655088554540434419946854408626923142436070630415134361428515;
c_TEp_a[6,3] := 0.00030771228226226118360872862753417350317298244674914294818400377809424655149917305792839447143257881;

#@ c_Ep_0_approx[k]
for k from 0 to 8 do
 c_Ep_0_approx[k] := unapply(C_to_Ep0_0(c_TEp_approx[k](t)),t);
od:

#@ c_TEm_approx
c_TEm_approx[0] := (t) -> (1/2 - (1/2-ca[1])*sin(t+Pi/4))*latt_c0;
c_TEm_approx[1] := (t) -> c_TEm_a[1,1] * sin(t) + c_TEm_a[1,3] * sin(3*t);
c_TEm_approx[2] := (t) -> latt_d0 * t/Pi + c_TEm_a[2,2] * sin(2*t) * I;
c_TEm_approx[3] := (t) -> latt_c0/4 + (t/(2*Pi)-1/4)*latt_d0 + c_TEm_a[3,1] * sin(t) +
                            c_TEm_a[3,2] * sin(2*t) * I + c_TEm_a[3,3] * sin(3*t);  
c_TEm_approx[4] := (t) -> latt_c0/4 + (t/(2*Pi)+1/4)*latt_d0 - c_TEm_a[3,1] * sin(t) +
                            c_TEm_a[3,2] * sin(2*t) * I - c_TEm_a[3,3] * sin(3*t);  
c_TEm_approx[5] := (t) -> t/(4*Pi)*(latt_c0 - latt_d0) + add(c_TEm_a[5,k] * sin(k*t),k=1..3);
c_TEm_approx[6] := (t) -> t/(4*Pi)*(latt_c0 + latt_d0) + add(conjugate(c_TEm_a[5,k]) * sin(k*t),k=1..3);
c_TEm_approx[7] := eval(c_TEm_approx[6]);
c_TEm_approx[8] := eval(c_TEm_approx[5]);

#@ c_TEm_a
c_TEm_a[1,1] := 0.8835084263955505585783987716840000325926615540942614052941584433964337822854465613444230237701180424;
c_TEm_a[1,3] := 0.06196724912320348395700677267078302081353815486017812159352605843249840400768595265032504859994172416;
c_TEm_a[2,2] := 0.1797501481785187719080376142790183135790694888474575684045644044454056920630789380074211242867244138;
c_TEm_a[3,1] := 0.3511321628863063015929856160517480228866539212422554128973412749459584607846880678817297806565675990;
c_TEm_a[3,2] := 0.07373745457838359930142006274768329182000846163947333927778741589054611329411213393198338264266919790;
c_TEm_a[3,3] := 0.0218969062105503030917796584579880912692573475308448365690517306473414539254961242930202103669553839;
c_TEm_a[4,1] := 0.3511321628863063015929856160517480228866539212422554128973412749459584607846880678817297806565675990;
c_TEm_a[4,2] := 0.0737374545783835993014200627476832918200084616394733392777874158905461132941121339319833826426691980;
c_TEm_a[4,3] := 0.0218969062105503030917796584579880912692573475308448365690517306473414539254961242930202103669553839;
c_TEm_a[5,1] := 0.2423485186450247972105483564735158211534303209384437520256882975599151270558722587190358743762353599e-1-
                0.3950697526118940704300624129705708722011083550171984012465296564337878450572058529379116267915483335e-1*I;
c_TEm_a[5,2] := 0.1755377153153475687209569034468230122971514325013707849129110392164278545559798591021656717836478500e-2-
                0.2351423197595643895076828999665351369198735842034167404505480689584486014610436048646424754749428350e-2*I;
c_TEm_a[5,3] := 0.1686909667345225954045278047227347451235211096899810957501328202832040081462419337428583331437649896e-3-
                0.2175854414420338678065767392569984415709072283534971244700121606992596899087070672886264959461280586e-3*I;

#@ c_Em_0_approx
c_Em_0_approx[0] := (t) -> C_to_Em0_0(c_TEm_approx[0](t));
c_Em_0_approx[1] := (t) -> C_to_Em0_0(c_TEm_approx[1](t));
c_Em_0_approx[2] := (t) -> C_to_Em0_0(c_TEm_approx[2](t));
c_Em_0_approx[3] := (t) -> C_to_Em0_0(c_TEm_approx[3](t));
c_Em_0_approx[4] := (t) -> C_to_Em0_0(c_TEm_approx[4](t));
c_Em_0_approx[5] := (t) -> C_to_Em0_0(c_TEm_approx[5](t));
c_Em_0_approx[6] := (t) -> C_to_Em0_0(c_TEm_approx[6](t));
c_Em_0_approx[7] := (t) -> C_to_Em0_0(c_TEm_approx[7](t));
c_Em_0_approx[8] := (t) -> C_to_Em0_0(c_TEm_approx[8](t));

######################################################################

# The second component of C_to_Ep0_0 gives a doubly periodic map from C
# to C, which is real on the lines of a square grid, and purely
# imaginary on curves corresponding to C1 and C2.  It is useful to
# understand the preimage of the unit circle under this map.  One
# component of the preimage (which we call the base component) is a
# figure eight centred at the point (latt_a0+latt_b0)/2.  This should 
# be thought of as two separate lobes that happen to meet. The other
# components are translates of the base component by multiples of
# latt_a0 and latt_b0.

# These maps land in the relevant locus, but are discontinuous. 
#@ Ep0_circle_lift_a
Ep0_circle_lift_a :=
 unapply(evalf(subs(a_P=a_P0,simplify(Ep_0_to_C(          l_Ep(t))))),t):

#@ Ep0_circle_lift_b
Ep0_circle_lift_b :=
 unapply(evalf(subs(a_P=a_P0,simplify(Ep_0_to_C([-1,1] *~ l_Ep(t))))),t):

# This map is a continuous parametrisation of the right hand lobe of
# the base component, satisfying
# C_to_Ep0_0(Ep0_circle_lift_c(t))[2] = exp(I*t).

#@ Ep0_circle_lift_c
Ep0_circle_lift_c := proc(t)
 local s0,n0;
 s0 := evalf(t/(2*Pi));
 n0 := floor(s0);
 s0 := s0 - n0;
 if s0 < 0.5 then
  Ep0_circle_lift_a(Pi*(   2*s0)) + latt_a0 + latt_b0;
 else 
  Ep0_circle_lift_b(Pi*(-2+2*s0)) + latt_a0;
 fi;
end:

# This map is a continuous parametrisation of the left hand lobe of
# the base component, satisfying
# C_to_Ep0_0(Ep0_circle_lift_d(t))[2] = exp(I*t).

#@ Ep0_circle_lift_d
Ep0_circle_lift_d := (t) -> latt_a0 + latt_b0 - Ep0_circle_lift_c(t);

# This is an approximation to Ep0_circle_lift_c

#@ Ep0_circle_lift_e
Ep0_circle_lift_e := (t) ->
  sqrt(0.077410-0.076893*cos(t)-0.00051645*cos(2*t)+
       I*(0.076738*sin(t)+0.00033641*sin(2*t))) +
  (latt_a0+latt_b0)/2;

# The image of this map is approximately the same as the base
# component, but the parametrisation is arbitrary.

#@ Ep0_circle_lift_f
Ep0_circle_lift_f := (t) -> 
   latt_a0/2+latt_b0/2+
   0.393*sin(t)+I*(0.13254*sin(2.*t)+0.01876*sin(4.*t));

# These functions give implicit equations for the relevant locus.

#@ Ep0_circle_cos_equation
Ep0_circle_cos_equation := (u) -> 
 0.331208-0.0885156*u[1]^3-0.367140*u[1]^2*u[2]-0.269277*u[1]*u[2]^2-
 0.0788400*u[2]^3-0.165916*u[1]^2-0.715646*u[1]*u[2]-0.338685*u[2]^2-
 0.116555*u[1]+0.0312848*u[2];

#@ Ep0_circle_equation
Ep0_circle_equation := (z) ->
 Ep0_circle_cos_equation([cos(Re(z)/latt_a0*2*Pi),cos(Im(z)/abs(latt_b0)*2*Pi)]);

