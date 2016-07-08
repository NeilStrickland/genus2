######################################################################
######################################################################

# The covering by the Poincare disk

assume (0 < a_H and a_H < 1);

ap_H := sqrt(1+a_H^2); #@ ap_H
am_H := sqrt(1-a_H^2); #@ am_H

#@ simplify_H 
simplify_H := proc(u)
 local v;
 v := simplify(u);
 v := simplify(subs(sqrt(1-a_H^4)=ap_H * am_H,u));
 v := simplify(subs(sqrt(5+2*a_H^2+a_H^4)=
                    sqrt(3+a_H^2+2*sqrt(1+a_H^2)) *
                    sqrt(3+a_H^2-2*sqrt(1+a_H^2)),v));
 v := simplify(subs(sqrt(1-a_H^4)=ap_H * am_H,v));
 return(v);
end:

######################################################################

#@ is_member_H 
is_member_H := (z) -> is(simplify_H(1 - z*conjugate(z)) > 0);

#@ is_equal_H 
is_equal_H := proc(z,w,witness_)
 local z0;
 z0 := z;
 if nargs > 2 then z0 := act_Pi(witness_,z0); fi;
 
 evalb(factor(expand(simplify_H(z0-w))) = 0);
end;

######################################################################
# Hyperbolic metric on the unit disc, scaled to have curvature -1.

# m_hyp, d_hyp and E_hyp take arguments in R^2
# m_hyp_c, d_hyp_c and E_hyp_c take arguments in C

# d_hyp and d_hyp_c give the actual metric, ie the distance between 
# a pair of points.  E_hyp and E_hyp_c give the multiplier for the
# metric tensor relative to the standard euclidean metric.

#@ m_hyp_c 
m_hyp_c := (z,w) -> abs((z-w)/(1-z*conjugate(w)));

#@ m_hyp   
m_hyp   := (x,y) -> sqrt(((x[1]-y[1])^2+(x[2]-y[2])^2)/((x[1]*y[2]-x[2]*y[1])^2+(x[1]*y[1]+x[2]*y[2]-1)^2)):

#@ d_hyp_c 
d_hyp_c := (z,w) -> 2*arctanh(m_hyp_c(z,w));

#@ d_hyp   
d_hyp   := (x,y) -> 2*arctanh(m_hyp(x,y));

#@ E_hyp_c 
E_hyp_c := (z) -> 4/(1 - abs(z)^2)^2;

#@ E_hyp   
E_hyp   := (x) -> 4/(1 - x[1]^2 - x[2]^2)^2;

# This returns a parametrisation of the locus of points at 
# hyperbolic distance r from a.

#@ hyp_sphere 
hyp_sphere := proc(a,r)
 local t,u,v,m;
 t := tanh(r/2);
 u := abs(a);
 v := (1-t^2)/(1-t^2*u^2)*a;
 m := t*(1-u^2)/(1-t^2*u^2);
 unapply(v + m * exp(I*s),s);
end:

#@ hyp_midpoint 
hyp_midpoint := proc(a,b)
 (1 - abs(a*b)^2 - sqrt((1-abs(a)^2)*(1-abs(b)^2)) * abs(1 - conjugate(a)*b))/
  ((1-abs(a)^2)*conjugate(b) + (1-abs(b)^2)*conjugate(a));
end:

######################################################################

lambda_H := (z) -> I*z;                                #@ lambda_H
lambda_inv_H := (z) -> -I*z;                           #@ lambda_inv_H
lambda_sq_H := (z) -> -z;                              #@ lambda_sq_H
mu_H := (z) -> ((ap_H*z-a_H^2-I)/((a_H^2-I)*z-ap_H));  #@ mu_H
nu_H := (z) -> conjugate(z);                           #@ nu_H

#@ act_H
act_H[1]    := (z) ->  z;
act_H[L]    := (z) -> I*z;
act_H[LL]   := (z) -> -z;
act_H[LLL]  := (z) -> -I*z;
act_H[M]    := unapply(simplify(   mu_H(z)),z);
act_H[LM]   := unapply(simplify( I*mu_H(z)),z);
act_H[LLM]  := unapply(simplify(  -mu_H(z)),z);
act_H[LLLM] := unapply(simplify(-I*mu_H(z)),z);
act_H[N]    := unapply(simplify(   nu_H(z)),z);
act_H[LN]   := unapply(simplify( I*nu_H(z)),z);
act_H[LLN]  := unapply(simplify(  -nu_H(z)),z);
act_H[LLLN] := unapply(simplify(-I*nu_H(z)),z);
act_H[MN]   := unapply(simplify(   mu_H(nu_H(z))),z);
act_H[LMN]  := unapply(simplify( I*mu_H(nu_H(z))),z);
act_H[LLMN] := unapply(simplify(  -mu_H(nu_H(z))),z);
act_H[LLLMN]:= unapply(simplify(-I*mu_H(nu_H(z))),z);

#@ beta
beta[0] := unapply(simplify((ap_H*z+1)/(z+ap_H)),z):
beta[1] := unapply(simplify((ap_H^3*z-(2+I)*a_H^2-I)/(((I-2)*a_H^2+I)*z + ap_H^3)),z);
beta[2] := unapply(simplify(I*beta[0](-I*z)),z):
beta[3] := unapply(simplify(I*beta[1](-I*z)),z):
beta[4] := unapply(simplify(I*beta[2](-I*z)),z):
beta[5] := unapply(simplify(I*beta[3](-I*z)),z):
beta[6] := unapply(simplify(I*beta[4](-I*z)),z):
beta[7] := unapply(simplify(I*beta[5](-I*z)),z):

# There is a standard isomorphism between the conformal automorphism
# group of the unit disc, and PSL_2(R).  The following matrices
# correspond to the maps beta[i].

#@ beta_SL2R
beta_SL2R[0] := <<ap_H - 1|0>,<0|ap_H + 1>>/a_H:
beta_SL2R[1] := <<ap_H^3+2*a_H^2|-1-a_H^2>,<-1-a_H^2|ap_H^3-2*a_H^2>>/(a_H-a_H^3):
beta_SL2R[2] := <<ap_H|1>,<1|ap_H>>/a_H:
beta_SL2R[3] := <<ap_H^3-ap_H^2|-2*a_H^2>,<-2*a_H^2|ap_H^3+ap_H^2>>/(a_H-a_H^3):
beta_SL2R[4] := <<1 + ap_H|0>,<0|ap_H - 1>>/a_H:
beta_SL2R[5] := <<ap_H^3-2*a_H^2|1+a_H^2>,<1+a_H^2|ap_H^3+2*a_H^2>>/(a_H-a_H^3):
beta_SL2R[6] := <<ap_H|-1>,<-1|ap_H>>/a_H:
beta_SL2R[7] := <<ap_H^3+ap_H^2|2*a_H^2>,<2*a_H^2|ap_H^3-ap_H^2>>/(a_H-a_H^3):

######################################################################

# The point v_H[i] is a choice of lift of the point v[i] in HX(a).

#@ v_H
v_H[ 0] := 0;
v_H[ 1] := ap_H*(1+I)/2;
v_H[ 2] := (a_H*am_H-ap_H)/(I-a_H^2);
v_H[ 3] := (a_H*am_H-ap_H)/(I*a_H^2-1);
v_H[ 4] :=  I*v_H[3];
v_H[ 5] := -I*v_H[2];
v_H[ 6] := (1+I)*(sqrt(2)-am_H)/sqrt(2)/ap_H;
v_H[ 7] :=  I*v_H[6];
v_H[ 8] :=   -v_H[6];
v_H[ 9] := -I*v_H[6];
v_H[10] := I*(ap_H-a_H);
v_H[11] := ap_H-a_H;
v_H[12] := (ap_H+a_H)*(I + (I+2)*a_H^2)/((ap_H+a_H)^2+a_H^2);
v_H[13] := I*conjugate(v_H[12]);

for i from 0 to 13 do v_H[i] := simplify(v_H[i]); od:

# The point v_H[2.1] is in the same Pi-orbit as v[2] and so represents
# the same point of the cromulent surface HX(a) = disc/Pi.  Similarly
# v_H[1.1], v_H[1.2] and v_H[1.3] are in the same Pi-orbit as v_H[1]
# and so on. Recall also that Maple does not treat 1.0 as being exactly
# equal to 1, so we find it convenient to define v_H[1.0] to be v_H[1].

v_H[ 1.0] :=  v_H[1];
v_H[ 1.1] :=  lambda_H(v_H[1.0]);
v_H[ 1.2] :=  lambda_H(v_H[1.1]);
v_H[ 1.3] :=  lambda_H(v_H[1.2]);
v_H[ 2.0] :=  simplify(expand(v_H[2]));
v_H[ 2.1] :=  conjugate(v_H[2.0]);
v_H[ 3.0] :=  simplify(expand(v_H[3]));
v_H[ 3.1] := -conjugate(v_H[3.0]);
v_H[ 4.0] :=  simplify(expand(v_H[4]));
v_H[ 4.1] :=  conjugate(v_H[4.0]);
v_H[ 5.0] :=  simplify(expand(v_H[5]));
v_H[ 5.1] := -conjugate(v_H[5.0]);
v_H[10.0] :=  simplify(expand(v_H[10]));
v_H[10.1] :=  conjugate(v_H[10.0]);
v_H[11.0] :=  simplify(expand(v_H[11]));
v_H[11.1] := -conjugate(v_H[11.0]);
v_H[12.0] :=  simplify(expand(v_H[12]));
v_H[12.1] := -v_H[12.0];
v_H[12.2] := -conjugate(v_H[12.0]);
v_H[12.3] :=  conjugate(v_H[12.0]);
v_H[13.0] :=  simplify(expand(v_H[13]));
v_H[13.1] := -v_H[13.0];
v_H[13.2] :=  conjugate(v_H[13.0]);
v_H[13.3] := -conjugate(v_H[13.0]);

# The following declarations specify elements of Pi that take 
# v_H[floor(i)] to v_H[i].  For example, the first declaration
# corresponds to the fact that v_H[1.1] = beta[2](beta[1](v_H[1])).

#@ v_H_fraction_offset
v_H_fraction_offset[ 1.1] := [2,1]:
v_H_fraction_offset[ 1.2] := [1,2,3,4]:
v_H_fraction_offset[ 1.3] := [3,4]:
v_H_fraction_offset[ 2.1] := [6]:
v_H_fraction_offset[ 3.1] := [4]:
v_H_fraction_offset[ 4.1] := [6]:
v_H_fraction_offset[ 5.1] := [4]:
v_H_fraction_offset[10.1] := [6]:
v_H_fraction_offset[11.1] := [4]:
v_H_fraction_offset[12.1] := [1]:
v_H_fraction_offset[12.2] := [2,1]:
v_H_fraction_offset[12.3] := [6]:
v_H_fraction_offset[13.1] := [4,3,4]:
v_H_fraction_offset[13.2] := [3,4]:
v_H_fraction_offset[13.3] := [4]:

#@ v_H_fraction_transform
v_H_fraction_transform[ 1.1] := L:
v_H_fraction_transform[ 1.2] := LL:
v_H_fraction_transform[ 1.3] := LLL:
v_H_fraction_transform[ 2.1] := N:
v_H_fraction_transform[ 3.1] := LLN:
v_H_fraction_transform[ 4.1] := N:
v_H_fraction_transform[ 5.1] := LLN:
v_H_fraction_transform[10.1] := N:
v_H_fraction_transform[11.1] := LLN:
v_H_fraction_transform[12.1] := LL:
v_H_fraction_transform[12.2] := LLN:
v_H_fraction_transform[12.3] := N:
v_H_fraction_transform[13.1] := LL:
v_H_fraction_transform[13.2] := N:
v_H_fraction_transform[13.3] := LLN:

# If T is in G16 and T(v[i]) = v[j] in X then in the hyperbolic disc 
# H we should have T(v[i]) = w(v[j]) for some word w in the generators
# beta[0], ... , beta[7].  The table v_action_witness_H records the required
# words.
 
#@ v_action_witness_H
for T in {L,M,N} do
 for i from 0 to 13 do
  v_action_witness_H[T,i] := []:
 od:
od:

unassign('T','i'):

v_action_witness_H[L, 1] := [2,1]:
v_action_witness_H[L, 2] := [4]:
v_action_witness_H[L, 4] := [4]:
v_action_witness_H[L,10] := [4]:
v_action_witness_H[L,11] := []:
v_action_witness_H[L,12] := [4]:
v_action_witness_H[L,13] := [2,1]:
v_action_witness_H[M, 0] := [2,3,4]:
v_action_witness_H[M, 1] := [2]:
v_action_witness_H[M, 3] := [2]:
v_action_witness_H[M, 4] := [5,6]:
v_action_witness_H[M, 5] := [2,3,4]:
v_action_witness_H[M, 6] := [2]:
v_action_witness_H[M, 7] := [5]:
v_action_witness_H[M, 8] := [5,4,3]:
v_action_witness_H[M, 9] := [2,3,4]:
v_action_witness_H[M,10] := []:
v_action_witness_H[M,11] := [2,3,4]:
v_action_witness_H[M,12] := []:
v_action_witness_H[M,13] := [2]:
v_action_witness_H[N, 1] := [3,4]:
v_action_witness_H[N, 2] := [6]:
v_action_witness_H[N, 3] := []:
v_action_witness_H[N, 4] := [6]:
v_action_witness_H[N, 5] := []:
v_action_witness_H[N,10] := [6]:
v_action_witness_H[N,12] := [6]:
v_action_witness_H[N,13] := [3,4]:

######################################################################

# For each complex number p with |p| > 1 we have an antiholomorphic
# involution xi(p,-) on the unit disk.  This gives all such 
# antiholomorphic involutions except those of the form 
# z |-> a * conjugate(z) with |a| = 1.  Every geodesic occurs
# as the fixed point set of a unique antiholomorphic involution.

#@ xi 
xi := (p,z) -> (p*conjugate(z) - 1)/conjugate(z - p);

#@ xi_alt
xi_alt := (theta,z) -> exp(I*theta) * conjugate(z);

# The function xi_curve(p,-) is a speed one parametrisation of the
# fixed point curve for xi(p,-).

#@ xi_curve 
xi_curve := proc(p,s)
 local d;
 d := sqrt(abs(p)^2-1);
 (I*d - 1) * ((I*d + 1) - I * abs(p) * exp(s)) / 
      (conjugate(p) * (I * abs(p) * exp(s) + (I * d - 1)));
end:

xi_curve_alt := (theta,s) -> exp(I*theta/2) * (exp(s) - 1)/(exp(s) + 1);

# xi_circle_orthogonal(p,q) returns an equation that will hold 
# iff the fixed point circles for xi(p,-) and xi(q,-) meet 
# orthogonally.

#@ xi_circle_orthogonal 
xi_circle_orthogonal := proc(p,q)
 return(p*conjugate(q) + conjugate(p)*q - 2 = 0);
end:

# If the circles as above do in fact meet orthogonally, then 
# the following function will return the point of intersection.

#@ xi_circle_intersect 
xi_circle_intersect := proc(p,q)
 local r,s,a,b;
 r := simplify(sqrt(factor(p * conjugate(p) - 1)));
 s := simplify(sqrt(factor(q * conjugate(q) - 1)));
 a := factor((p * conjugate(q) - 1 + I * r * s)/conjugate(q-p));
 b := factor((p * conjugate(q) - 1 - I * r * s)/conjugate(q-p));
 return([a,b]);
end:

# This function will return the two fixed points of xi(p,-) that
# lie on the unit circle.

#@ xi_circle_ends 
xi_circle_ends := proc(p) 
 local r;
 r := factor(expand(1 - p * conjugate(p)));
 [(1 + sqrt(r))/conjugate(p),(1 - sqrt(r))/conjugate(p)];
end:

# Given points a and b on the unit circle that are not opposite,
# this returns the centre of the circle that crosses S^1
# orthogonally at a and b.

#@ ends_to_centre 
ends_to_centre := proc(a,b)
 (a + b)/(1 + Re(a/b));
end:

#@ ends_to_radius 
ends_to_radius := proc(a,b)
 local x;
 x := Re(a/b); 
 sqrt((1 - x)/(1 + x));
end:

# We now define geodesics c_H[0] , ... , c_H[8] in the
# Poincare disk that will cover the curves c[0], ... , c[8] in X.  

#@ c_H_p
c_H_p[0] := (1+I)/ap_H;
c_H_p[3] := ap_H;
c_H_p[4] := I*ap_H;
c_H_p[7] := (I*ap_H/2+1/ap_H);
c_H_p[8] := (ap_H/2+I/ap_H);

# These are the corresponding radii so c_H_r[k] = sqrt(abs(c_H_p[k])^2-1)
#@ c_H_r
c_H_r[0] := am_H/ap_H;
c_H_r[3] := a_H;
c_H_r[4] := a_H;
c_H_r[7] := am_H^2/(2*ap_H);
c_H_r[8] := am_H^2/(2*ap_H);

#@ s_H
s_H[0] := 2*log(sqrt(2)*a_H/(ap_H-am_H));
s_H[1] := log((sqrt(2)+ap_H)/(sqrt(2)-ap_H))/2;
s_H[2] := log((1+a_H)/am_H);
s_H[3] := log((ap_H+a_H+1)/(ap_H+a_H-1))/2;
s_H[4] := log((ap_H^2+2*ap_H+2)/(ap_H^2-2*ap_H+2))/4;

# Maple does a bad job of simplifying expressions involving 
# tanh(), so we write our own version that immediately 
# converts to exponentials.

#@ Tanh 
Tanh := (s) -> (exp(s) - exp(-s))/(exp(s) + exp(-s));

#@ c_H
c_H[0] := (t) -> xi_curve(c_H_p[0],2*s_H[0]*(t/Pi-1/4));
c_H[1] := (t) -> ((I + 1)/sqrt(2)) * Tanh(t*s_H[1]/Pi);
c_H[2] := (t) -> ((I - 1)/sqrt(2)) * Tanh(t*s_H[1]/Pi);
c_H[3] := (t) -> xi_curve(c_H_p[3],-2*t*s_H[2]/Pi);
c_H[4] := (t) -> xi_curve(c_H_p[4],-2*t*s_H[2]/Pi);
c_H[5] := (t) -> Tanh(t*s_H[3]/Pi);
c_H[6] := (t) -> I * Tanh(t*s_H[3]/Pi);
c_H[7] := (t) -> xi_curve(c_H_p[7], 2*t*s_H[3]/Pi - 2*s_H[4]);
c_H[8] := (t) -> xi_curve(c_H_p[8],-2*t*s_H[3]/Pi + 2*s_H[4]);

#@ v_H_theta 
v_H_theta := 2*arctan(a_H*(sqrt(a_H^2+1)-a_H));

# The following functions parametrise the boundary of the
# region F8.  They move anticlockwise as the parameter
# s increases from 0 to 1.

#@ c_H_simple
c_H_simple[5] := (s) -> s * v_H[11];
c_H_simple[3] := (s) -> ap_H - a_H * exp(-I*s*v_H_theta);
c_H_simple[7] := (s) -> 1/ap_H + I/2*ap_H + (1 - a_H^2)/ap_H/2 * exp(-I*((1+s)*Pi/2 + (1-s)*v_H_theta));
c_H_simple[1] := (s) -> (1-s)*v_H[1];

# This records the endpoints on the unit circle of the geodesics
# that form the boundary of F16.

#@ c_H_ends
c_H_ends[0] := [-(1-I)/2*(am_H - I*ap_H),(1-I)/2*(am_H+I*ap_H)];
c_H_ends[1] := [-(1+I)/sqrt(2), (1+I)/sqrt(2)];
c_H_ends[3] := [(1 - I*a_H)/ap_H,(1 + I*a_H)/ap_H];
c_H_ends[5] := [-1,1];

# The curves c_H[k](t) have constant speed with respect to the
# hyperbolic metric.  The speeds are given below.

#@ c_H_speed
c_H_speed[0] := 2*s_H[0]/Pi;
c_H_speed[1] := 2*s_H[1]/Pi;
c_H_speed[2] := 2*s_H[1]/Pi;
c_H_speed[3] := 2*s_H[2]/Pi;
c_H_speed[4] := 2*s_H[2]/Pi;
c_H_speed[5] := 2*s_H[3]/Pi;
c_H_speed[6] := 2*s_H[3]/Pi;
c_H_speed[7] := 2*s_H[3]/Pi;
c_H_speed[8] := 2*s_H[3]/Pi;

# c_H_cycle[k] gives the element g in the group Pi such that
# c_H[k](t + 2*Pi) = g . c_H[k](t).

#@ c_H_cycle
c_H_cycle[0] := [0,2,4,6];
c_H_cycle[1] := [0,7,6,5];
c_H_cycle[2] := [2,1,0,7];
c_H_cycle[3] := [0,7];
c_H_cycle[4] := [2,1];
c_H_cycle[5] := [0];
c_H_cycle[6] := [2];
c_H_cycle[7] := [0,2,1];
c_H_cycle[8] := [2,3,4];

#@ c_action_witness_H
for T in {L,M,N} do
 for i from 0 to 8 do
  c_action_witness_H[T,i] := []:
 od:
od:

unassign('T','i'):

c_action_witness_H[L,0] := [4];
c_action_witness_H[L,4] := [4];
c_action_witness_H[L,7] := [2,1];
c_action_witness_H[L,8] := [2,1];
c_action_witness_H[M,1] := [5,4,3];
c_action_witness_H[M,2] := [2,3,4];
c_action_witness_H[M,3] := [2,3,4];
c_action_witness_H[M,5] := [2,3,4];
c_action_witness_H[M,6] := [2,3,4];
c_action_witness_H[M,7] := [2];
c_action_witness_H[M,8] := [2];
c_action_witness_H[N,0] := [6];
c_action_witness_H[N,4] := [6];
c_action_witness_H[N,7] := [3,4];
c_action_witness_H[N,8] := [3,4];

#@ v_on_c_witness_H
for i from 0 to 14 do
 for j from 0 to 8 do
  v_on_c_witness_H[i,j] := [];
 od;
od;

v_on_c_witness_H[ 4,0] := [0];
v_on_c_witness_H[ 5,0] := [2];
v_on_c_witness_H[ 7,0] := [0];
v_on_c_witness_H[ 8,0] := [2,0];
v_on_c_witness_H[ 9,0] := [2];
v_on_c_witness_H[ 1,2] := [2,1];
v_on_c_witness_H[12,4] := [2,1];


######################################################################

#@ c_check_H
c_check_H[0] := (z) -> is_equal_H(z, xi(c_H_p[0],z),args[2..-1]);
c_check_H[1] := (z) -> is_equal_H(z, I*conjugate(z),args[2..-1]);
c_check_H[2] := (z) -> is_equal_H(z,-I*conjugate(z),args[2..-1]);
c_check_H[3] := (z) -> is_equal_H(z, xi(c_H_p[3],z),args[2..-1]);
c_check_H[4] := (z) -> is_equal_H(z, xi(c_H_p[4],z),args[2..-1]);
c_check_H[5] := (z) -> is_equal_H(z,   conjugate(z),args[2..-1]);
c_check_H[6] := (z) -> is_equal_H(z,  -conjugate(z),args[2..-1]);
c_check_H[7] := (z) -> is_equal_H(z, xi(c_H_p[7],z),args[2..-1]);
c_check_H[8] := (z) -> is_equal_H(z, xi(c_H_p[8],z),args[2..-1]);

######################################################################

#@ F1_H_B 
F1_H_B := [[0],[2],[4],[6],[0,7],[1,2],[2,1],[3,4],[4,3],[5,6],[6,5],[7,0]];

#@ is_in_F1_H 
is_in_F1_H := proc(z)
 local T,w,r;

 for T in F1_H_B do
  w := simplify_H(act_Pi(T,z));
  r := simplify_H(z * conjugate(z) - w * conjugate(w));
  if is(r > 0) then return false; fi;
 od;

 return true;
end;

#@ is_in_F4_H 
is_in_F4_H := proc(z)
 local z0;
 z0 := simplify_H(z);
 is_in_F1_H(z0) and is(simplify_H(Re(z)) >= 0) and is(simplify_H(Im(z)) >= 0);
end;

#@ is_in_F16_H 
is_in_F16_H := proc(z)
 local m,d;

 if not is_in_F4_H(z) then return false; fi;
 if is(simplify_H(Im(z) - Re(z)) > 0) then return false; fi;
 m := c_H_p[0];
 d := simplify_H((z-m)*conjugate(z-m) - m*conjugate(m) + 1);
 if is(d < 0) then return false; fi;
 return true;
end;

#@ random_Delta_point 
random_Delta_point := proc(min_abs_)
 local x,y,min_abs;
 x := 1;
 y := 1;
 min_abs := `if`(nargs > 0,min_abs_,0);
 while x^2 + y^2 >= 1 or x^2 + y^2 < min_abs^2 do
  x := (rand(0..2000)()-1000)/1000.;
  y := (rand(0..2000)()-1000)/1000.;
 od;
 return(x + I * y);
end:

#@ F1_edge
F1_edge[ 1, 1,'a'] := [v_H[ 1  ],v_H[12  ]]:
F1_edge[ 1, 1,'b'] := [v_H[12  ],v_H[12.2]]:
F1_edge[ 1, 1,'c'] := [v_H[ 1.1],v_H[13.3]]:
F1_edge[ 1, 1,'d'] := [v_H[13.3],v_H[13.1]]:
F1_edge[ 1, 1,'e'] := [v_H[ 1.2],v_H[12.1]]:
F1_edge[ 1, 1,'f'] := [v_H[ 1.3],v_H[13.2]]:
F1_edge[ 1,-1,'a'] := [v_H[ 1.1],v_H[12.2]]:
F1_edge[ 1,-1,'b'] := [v_H[12.3],v_H[12.1]]:
F1_edge[ 1,-1,'c'] := [v_H[ 1.2],v_H[13.1]]:
F1_edge[ 1,-1,'d'] := [v_H[13  ],v_H[13.2]]:
F1_edge[ 1,-1,'e'] := [v_H[ 1.3],v_H[12.3]]:
F1_edge[ 1,-1,'f'] := [v_H[ 1  ],v_H[13  ]]:

#@ F1_pairing
F1_pairing['a'] := [5,6];
F1_pairing['b'] := [2];
F1_pairing['c'] := [7,0];
F1_pairing['d'] := [4];
F1_pairing['e'] := [1,2];
F1_pairing['f'] := [3,4];

#@ F1_arcs 
F1_arcs := [
  [    ap_H ,  0      , c_H_r[3] , Pi-v_H_theta     , Pi+v_H_theta     , magenta],
  [  1/ap_H ,  ap_H/2 , c_H_r[7] , Pi               , 3*Pi/2-v_H_theta , blue   ],
  [  ap_H/2 ,  1/ap_H , c_H_r[7] , Pi+v_H_theta     , 3*Pi/2           , blue   ],
  [     0   ,  ap_H   , c_H_r[3] , 3*Pi/2-v_H_theta , 3*Pi/2+v_H_theta , magenta],
  [ -ap_H/2 ,  1/ap_H , c_H_r[7] , 3*Pi/2           , 2*Pi-v_H_theta   , blue   ],
  [ -1/ap_H ,  ap_H/2 , c_H_r[7] , 3*Pi/2+v_H_theta , 2*Pi             , blue   ],
  [   -ap_H ,  0      , c_H_r[3] , -v_H_theta       , v_H_theta        , magenta],
  [ -1/ap_H , -ap_H/2 , c_H_r[7] , 0                , Pi/2-v_H_theta   , blue   ],
  [ -ap_H/2 , -1/ap_H , c_H_r[7] , v_H_theta        , Pi/2             , blue   ],
  [     0   ,   -ap_H , c_H_r[3] , Pi/2-v_H_theta   , Pi/2+v_H_theta   , magenta],
  [  ap_H/2 , -1/ap_H , c_H_r[7] , Pi/2             , Pi-v_H_theta     , blue   ],
  [  1/ap_H , -ap_H/2 , c_H_r[7] , Pi/2+v_H_theta   , Pi               , blue   ]
 ];

#@ F1_area 
F1_area :=
 (3+a_H^2)
 -2*a_H*am_H^2/ap_H
 -am_H^4/ap_H^2/2*Pi
 -v_H_theta*(3*a_H^4+6*a_H^2-1)/(1+a_H^2);

# This value of a_H makes F1 into a regular dodecagon, with
# vertices of absolute value 3^(-1/4).

#@ a_H_regular 
a_H_regular := sqrt(2/sqrt(3)-1);

side_length_H[ 0] := 2*arctanh((ap_H - sqrt(2)*a_H)/am_H);
side_length_H[ 1] := 2*arctanh((sqrt(2) - am_H)/ap_H);
side_length_H[ 3] := 2*arctanh((1 - am_H)/a_H);
side_length_H[ 5] := 2*arctanh(ap_H - a_H);

######################################################################

#@ recenter_H
recenter_H[0] := (z) -> z:

recenter_H[3] :=
 unapply(factor(-(-(I*sqrt(-(a_H-1)*(a_H+1))-a_H*sqrt(a_H^2+1))/(I-a_H^2)) * (z - v_H[3])/(1-conjugate(v_H[3])*z)),z):

recenter_H[6] := 
 unapply(simplify((1-I)/sqrt(2)*(z - v_H[6])/(1 - conjugate(v_H[6])*z)),z);

recenter_H[11] := 
 unapply(simplify((z - v_H[11])/(1 - conjugate(v_H[6])*z)),z);

#@ square_diffeo_H_ca 
square_diffeo_H_ca := (1+I*a_H)/ap_H;

#@ square_diffeo_H_ra 
square_diffeo_H_ra := am_H/(1+a_H);

#@ square_diffeo_H_ma 
square_diffeo_H_ma := (z) -> (square_diffeo_H_ca-z)/(1-square_diffeo_H_ca*z);

#@ square_diffeo_H_pa 
square_diffeo_H_pa := (z) -> log(abs(square_diffeo_H_ma(z)))/log(square_diffeo_H_ra);

#@ square_diffeo_H_cb 
square_diffeo_H_cb := (ap_H+I*am_H)/(1+I);

#@ square_diffeo_H_rb 
square_diffeo_H_rb := (ap_H+am_H)/a_H/sqrt(2);

#@ square_diffeo_H_mb 
square_diffeo_H_mb := (z) -> (I*square_diffeo_H_cb-z)/(1-square_diffeo_H_cb*z);

#@ square_diffeo_H_pb 
square_diffeo_H_pb := (z) -> log(abs(square_diffeo_H_mb(z)))/log(square_diffeo_H_rb);

#@ square_diffeo_H 
square_diffeo_H := (z) -> [1-square_diffeo_H_pa(z),square_diffeo_H_pb(z)];

#@ square_diffeo_inv_H0 
square_diffeo_inv_H0 := proc(t)
 local sa,sb,ca,cb,aa,bb,z;
 
 sa := square_diffeo_H_ra0^(1-t[1]);
 sb := square_diffeo_H_rb0^t[2];
 ca := (sa^2/square_diffeo_H_ca0 -     square_diffeo_H_ca0)/(sa^2 - 1);
 cb := (sb^2/square_diffeo_H_cb0 - I * square_diffeo_H_cb0)/(sb^2 - 1);
 aa := Re(ca*conjugate(ca - cb)/abs(ca - cb)^2);
 bb := sqrt((abs(ca)^2 - 1)/abs(ca - cb)^2 - aa^2);
 z := (1 - aa + I * bb) * ca + (aa - I * bb) * cb;
 return z;
 
end:

######################################################################

# This gives an array of group elements in Pi tilde.  The corresponding
# translates of F16 fit together nicely and contain a reasonable 
# neighbourhood of F16.

#@ tile
tile[-1, 2.2] := [LLL,[2,1]]:
tile[-1, 2  ] := [LLLMN,[6,5,6]]:
tile[-1, 1.8] := [LLLM,[5]]:
tile[-1, 1.6] := [LLMN,[6,5,6,7,0]]:
tile[-1, 1.4] := [LLM,[5,4,3]]:
tile[-1, 1.2] := [LMN,[6,0,7]]:
tile[-1, 1  ] := [LM,[2,3,4]]:
tile[-1, 0  ] := [LN,[]]:
tile[-1,-0.2] := [L,[]]:
tile[-1,-0.4] := [LLN,[]]:
tile[-1,-0.6] := [LL,[]]:
tile[-1,-0.8] := [LLLN,[]]:
tile[-1,-1  ] := [LLL,[]]:
tile[-1,-1.2] := [LLLMN,[6]]:

tile[ 0, 4] := [1,[0,7]]:
tile[ 0, 3] := [N,[3,4]]:
tile[ 0, 2] := [M,[2]]:
tile[ 0, 1] := [MN,[6]]:
tile[ 0, 0] := [1,[]]:
tile[ 0,-1] := [N,[]]:
tile[ 0,-2] := [M,[2,3,4]]:
tile[ 0,-3] := [MN,[6,0,7]]:
tile[ 0,-4] := [1,[3,4]]:

tile[ 1, 4] := [LLN,[7]]:
tile[ 1, 3] := [LL,[4,3,4]]:
tile[ 1, 2] := [LLMN,[6,5,6,7]]:
tile[ 1, 1] := [LLM,[5,4,3,4]]:
tile[ 1, 0] := [LLN,[4]]:
tile[ 1,-1] := [LL,[4]]:
tile[ 1,-2] := [LLMN,[6,5,6,4]]:
tile[ 1,-3] := [LLM,[5,4]]:
tile[ 1,-4] := [LLN,[4,3,4]]:



