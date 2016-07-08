# There is a canonical conformal map p : EX^* -> S^2, but it is hard 
# to calculate.  It is also interesting to look for more elementary
# maps EX^* -> S^2 that are not conformal, but have properties similar
# to those of p.  This file contains some code for that problem; 
# there is further code in better_sphere_quotients.mpl and 
# (with an object oriented framework) in E_to_S.mpl

# Here is a fairly elementary map of the required type.

#@ E_to_S2_tilde 
E_to_S2_tilde := unapply(
 [sqrt(2)*yx0[2]*(1-x[3]^2),2*x[1]*x[2],-2*x[3]] /~ (1+x[3]^2) ,x);

#@ E_to_S2_s_z 
E_to_S2_s_z := unapply(z[1]^2*z[2]*(1/2 - z[2])/(1 + z[1])^2,z);

#@ E_to_S2_s_x 
E_to_S2_s_x := (x) -> x[4]^2*(x[3]^2/2-x[4]^2)/(1+x[3]^2)^2;

#@ E_to_S2_s_max_z1 
E_to_S2_s_max_z1 := RootOf(5*z[1]^3-10*z[1]^2+2*z[1]+2,z[1],index=1);

#@ E_to_S2_s_max_z2 
E_to_S2_s_max_z2 := subs(t = E_to_S2_s_max_z1,15*t^2 - 35*t + 18);

#@ E_to_S2_s_max 
E_to_S2_s_max := subs(t = E_to_S2_s_max_z1,(-1810*t^2+4230*t-2149)/15);

#@ E_to_S2 
E_to_S2 := unapply(
 [sqrt(2)*yx0[2]*(1-x[3]^2),2*x[1]*x[2],-2*x[3]]/~
 sqrt(x[3]^4+2*x[3]^2+1-(1/2)*x[3]^2*x[4]^2+x[4]^4),x);

# The images of v[0] to v[9] should be as follows.
#@ v_S2
v_S2[0] := [ 0, 0,-1];
v_S2[1] := [ 0, 0, 1];
v_S2[2] := [-1, 0, 0];
v_S2[3] := [ 1, 0, 0];
v_S2[4] := [-1, 0, 0];
v_S2[5] := [ 1, 0, 0];
v_S2[6] := [ 0, 1, 0];
v_S2[7] := [ 0,-1, 0];
v_S2[8] := [ 0, 1, 0];
v_S2[9] := [ 0,-1, 0];

# The images of v[10] to v[13] depend on the parameter a_P, for which
# we do not have an exact expression.  The code below corresponds to 
# the case a_P = (sqrt(3)-sqrt(2))^( 2), which is approximately 
# correct and is exactly consistent with the map E_to_S2 above.

v_S2[10] := [-1/5, 0, -(2/5)*sqrt(6)];
v_S2[11] := [ 1/5, 0, -(2/5)*sqrt(6)];
v_S2[12] := [-1/5, 0,  (2/5)*sqrt(6)];
v_S2[13] := [ 1/5, 0,  (2/5)*sqrt(6)];

# The points v_C[i] are the images in C u {infinity} of the points v_S2[i].

#@ v_C
v_C[ 0] := 0;
v_C[ 1] := infinity;
v_C[ 2] := -1;
v_C[ 3] :=  1;
v_C[ 4] := -1;
v_C[ 5] :=  1;
v_C[ 6] :=  I;
v_C[ 7] := -I;
v_C[ 8] :=  I;
v_C[ 9] := -I;

v_C[10] := -(sqrt(3)-sqrt(2))^( 2);
v_C[11] :=  (sqrt(3)-sqrt(2))^( 2);
v_C[12] := -(sqrt(3)-sqrt(2))^(-2);
v_C[13] :=  (sqrt(3)-sqrt(2))^(-2);

# These curves are exactly correct for the map E_to_S2, but only
# approximately correct for the canonical conformal map p : EX^* -> S^2.  
#@ c_S2
c_S2[ 0] := (t) -> [-cos(2*t),sin(2*t),0];
c_S2[ 1] := (t) -> [0,(1 - cos(t)^2)/(1 + cos(t)^2),-2*cos(t)/(1 + cos(t)^2)];
c_S2[ 2] := unapply(act_S2[L](c_S2[ 1](t)),t);
c_S2[ 3] := (t) -> [(3 - 2*cos(t)^2)/(3 + 2*cos(t)^2),0,-2*sqrt(6)*cos(t)/(3 + 2*cos(t)^2)];
c_S2[ 4] := unapply(act_S2[L](c_S2[ 3](t)),t);
c_S2[ 5] := unapply([(1-cos(t))^2,0,-8*sqrt(5-cos(t))] /~ sqrt((18-2*cos(t))^2 - (1-cos(t))*(3-cos(t))*sin(t)^2),t);
c_S2[ 6] := unapply(act_S2[ L](c_S2[ 5](t)),t);
c_S2[ 7] := unapply(act_S2[ M](c_S2[ 5](t)),t);
c_S2[ 8] := unapply(act_S2[LM](c_S2[ 5](t)),t);
c_S2[ 9] := unapply([cos(t),sin(t)^2,-2*sqrt(6)] /~ sqrt(25 - sin(t)^2*cos(t)^2),t);
c_S2[10] := unapply(act_S2[ L](c_S2[ 9](t)),t);
c_S2[11] := unapply(act_S2[ M](c_S2[ 9](t)),t);
c_S2[12] := unapply(act_S2[LM](c_S2[ 9](t)),t);
c_S2[13] := unapply([2*sqrt(3+sin(t)^2)*(3-2*sin(t)^2)*sin(t),9*cos(t)^2,-2*sqrt(6)*(3+sin(t)^2)*sin(t)] /~
                     sqrt((9+11*sin(t)^2)^2 - (2*sin(t)*cos(t)*(3+2*sin(t)^2))^2 + (sqrt(8)*sin(t)^2*cos(t))^2),t);
c_S2[14] := unapply(act_S2[ L](c_S2[13](t)),t);
c_S2[15] := unapply(act_S2[ N](c_S2[13](t)),t);
c_S2[16] := unapply(act_S2[LN](c_S2[13](t)),t);

# We use the following approximately conformal charts on EX

#@ chart
chart[0]  := (s,t) -> [s,t,1-s^2/2-t^2/2,sqrt(2)*(t^2-s^2)];
chart[3]  := (s,t) -> [-s,1,-sqrt(2/3)*t,sqrt(1/3)*t];
chart[6]  := (s,t) -> [1/sqrt(2)+(t+s)/2,1/sqrt(2)-(t+s)/2,(t-s)/sqrt(2),0];
chart[11] := (s,t) -> [s,-t,(1+s^2/2-t^2/2)*sqrt(2/3),(-1+5*s^2/2+t^2/2)*sqrt(1/3)];

######################################################################

# If we have a homeomorphism f from EX^*/<LL> to C u {infinity} or
# to S^2, then we can obtain homeomorphisms from EX^*/H to
# C u {infinity} or S^2 (for various other subgroups H) by composing
# with the maps below.  Specifically:
#
# - For H = <L>,     compose with p12
# - For H = <LL,M>,  compose with p13
# - For H = <LL,LM>, compose with p14
# - For H = <L,M>,   compose with p15 = p25 o p12 = p35 o p13 = p45 o p14.
#
# Conventions are chosen such that these maps send small positive reals
# to small positive reals.

p12_C := (z) -> z^2;           #@ p12_C 
p13_C := (z) -> 2*z/(1+z^2);   #@ p13_C 
p14_C := (z) -> 2*z/(1-z^2);   #@ p14_C 
p15_C := (z) -> 2*z^2/(1+z^4); #@ p15_C 
p25_C := (z) -> 2*z/(1+z^2);   #@ p25_C 
p35_C := (z) -> z^2/(2-z^2);   #@ p35_C 
p45_C := (z) -> z^2/(2+z^2);   #@ p45_C 

#@ p12_S2 
p12_S2 := unapply([u[1]^2-u[2]^2, 2*u[1]*u[2],2*u[3]] /~ (1+u[3]^2),u);

#@ p13_S2 
p13_S2 := unapply([2*u[1],-2*u[2]*u[3],u[2]^2-u[3]^2] /~ (1+u[1]^2),u);

#@ p14_S2 
p14_S2 := unapply([-2*u[1]*u[3],2*u[2],u[1]^2-u[3]^2] /~ (1+u[2]^2),u);

#@ p25_S2 
p25_S2 := unapply([2*u[1],-2*u[2]*u[3],u[2]^2-u[3]^2] /~ (1+u[1]^2),u);

#@ p35_S2 
p35_S2 := unapply([ (1-u[3])^2 - 4*(1-u[1]^2),4*u[1]*u[2],4*(u[1]^2+u[3]-1)]/~((1-u[3])^2+4*(1-u[1]^2)),u);

#@ p45_S2 
p45_S2 := unapply([-(1-u[3])^2 + 4*(1-u[2]^2),4*u[1]*u[2],4*(u[2]^2+u[3]-1)]/~((1-u[3])^2+4*(1-u[2]^2)),u);

#@ p15_S2 
p15_S2 := unapply([(1+u[3]^2)*(u[1]^2-u[2]^2),-4*u[1]*u[2]*u[3],2*(1+u[1]^2)*(1+u[2]^2)-4] /~ ((1-u[1]^2)^2+(1-u[2]^2)^2),u);

######################################################################

#@ E_to_S2_jacobian 
E_to_S2_jacobian :=
 8*((1+z[1])*((1-z[1])^2-z[1]^2*z[2]/2) + z[1]^2*z[2]*(1/2-z[2])*(3+z[1]))/
  (1+2*z[1]+z[1]^2*(1-z[2]/2)+z[1]^2*z[2]^2)^(3/2);

#@ E_to_S2_distortion 
E_to_S2_distortion := proc(x0)
 local u0,v0,a0,b0,c0,p0,q0,M0,Cp,Cm,Cq;

 u0,v0 := op(tangent_frame_b(x0)):
 a0 := evalf(E_to_S2(x0)):
 b0,c0 := op(S2_tangent_frame(a0)): 
 p0 := subs(t=0,map(diff,evalf(E_to_S2(x0 +~ t *~ u0)),t)):
 q0 := subs(t=0,map(diff,evalf(E_to_S2(x0 +~ t *~ v0)),t)):
 M0 := <<dp3(p0,b0)|dp3(p0,c0)>,<dp3(q0,b0)|dp3(q0,c0)>>:
 Cp := (M0[1,1]-M0[2,2])^2 + (M0[1,2]+M0[2,1])^2:
 Cm := (M0[1,1]+M0[2,2])^2 + (M0[1,2]-M0[2,1])^2:
 Cq := Cp/(Cp+Cm);

 return Cq;
end:
