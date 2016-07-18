set_a_E0(1/sqrt(2));

#@ ry
ry[1] := sqrt(1-y[2]/sqrt(2));
ry[2] := sqrt(1+y[2]/sqrt(2));

#@ rx
rx[1] := subs(y[2] = yx0[2],ry[1]);
rx[2] := subs(y[2] = yx0[2],ry[2]);

#@ y2r
y2r[1] := sqrt(2)*(1-r[1]^2);
y2r[2] := sqrt(2)*(r[2]^2-1);

#@ y_vars 
y_vars := lexdeg([r[1], r[2]],
                 [x[1], x[2], x[3], x[4]],
		 [z[1], z[2]],
		 [y[1], y[2]]);

#@ y_rels0 
y_rels0 := Basis([r[1]^2-ry[1]^2,r[2]^2-ry[2]^2,op(y_rels0)],y_vars);

#@ z_vars 
z_vars := lexdeg([r[1], r[2]],
                 [x[1], x[2], x[3], x[4]],
		 [y[1], y[2]],
                 [z[1], z[2]]);

#@ z_rels0 
z_rels0 := Basis([r[1]^2-ry[1]^2,r[2]^2-ry[2]^2,op(z_rels0)],z_vars);

#@ root_rule 
root_rule := {
   sqrt(1-y[2]/sqrt(2)) = r[1],
   sqrt(1+y[2]/sqrt(2)) = r[2],
   sqrt(4-2*sqrt(2)*y[2]) = 2 * r[1],
   sqrt(4+2*sqrt(2)*y[2]) = 2 * r[2],
   sqrt(sqrt(2)*(sqrt(2)-y[2])) = (sqrt(2) * r[1]),
   sqrt(sqrt(2)*(sqrt(2)+y[2])) = (sqrt(2) * r[2]),
   (sqrt(2)*(sqrt(2)-y[2]))^(3/2) = 2*sqrt(2)*(1-y[2]/sqrt(2))*r[1],
   (sqrt(2)*(sqrt(2)+y[2]))^(3/2) = 2*sqrt(2)*(1+y[2]/sqrt(2))*r[2],
   sqrt( 3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2+4) = 2*r[1],
   sqrt(-3*sqrt(2)*x[3]*x[4]-2*x[1]^2+2*x[2]^2+4) = 2*r[2],
   sqrt(1 - y[2]^2/2) = r[1] * r[2],
   sqrt(4 - 2*y[2]^2) = 2 * r[1] * r[2],
   sqrt(-18*x[3]^2*x[4]^2-12*sqrt(2)*x[1]^2*x[3]*x[4]+12*sqrt(2)*x[2]^2*x[3]*x[4]-4*x[1]^4+8*x[1]^2*x[2]^2-4*x[2]^4+16)=4*r[1]*r[2]
  };

#@ FNF_y0 
FNF_y0 := proc(u)
 local uu,nu,du,t;

 if type(u,list) or type(u,set) then
  return map(FNF_y0,u);
 fi;

 uu := simplify(u);
 uu := factor(subs(root_rule,uu));
 nu := NF_y0(subs(root_rule,numer(u)));
 du := NF_y0(subs(root_rule,denom(u)));
 uu := factor(nu/du);

 for t in [x[1],x[2],r[1],r[2]] do
  if has(du,t) then
   nu := NF_y0(expand(nu * subs(t = -t,du)));
   du := NF_y0(expand(du * subs(t = -t,du))); 
   uu := factor(nu/du);
   nu := numer(uu);
   du := denom(uu);
  fi;
 od:

 return uu;
end:

#@ FNF_z0 
FNF_z0 := proc(u)
 local uu,nu,du,t;

 if type(u,list) or type(u,set) then
  return map(FNF_y0,u);
 fi;

 uu := simplify(u);
 uu := factor(subs(root_rule,uu));
 nu := NF_z0(subs(root_rule,numer(u)));
 du := NF_z0(subs(root_rule,denom(u)));
 uu := factor(nu/du);

 for t in [x[1],x[2],y[1],y[2],r[1],r[2]] do
  if has(du,t) then
   nu := NF_z0(expand(nu * subs(t = -t,du)));
   du := NF_z0(expand(du * subs(t = -t,du))); 
   uu := factor(nu/du);
   nu := numer(uu);
   du := denom(uu);
  fi;
 od:

 return uu;
end:

z_to_t := (z) -> [2*z[1]-z[1]^2+z[1]^2*z[2]/2,2*z[2]]; #@ z_to_t
t_proj := unapply(z_to_t(z_proj0(x)),x);               #@ t_proj

#@ t_to_z 
t_to_z := (t) -> [(2*(-2+sqrt(t[1]*t[2]-4*t[1]+4)))/(t[2]-4), (1/2)*t[2]];

#@ t_lift 
t_lift := (t) -> [
 (1/2)*sqrt(2)*sqrt((1-sqrt(t[2]))*(sqrt(t[1]*t[2]-4*t[1]+4)+sqrt(t[2]))/(2+sqrt(t[2]))),
 (1/2)*sqrt(2)*sqrt((1+sqrt(t[2]))*(sqrt(t[1]*t[2]-4*t[1]+4)-sqrt(t[2]))/(2-sqrt(t[2]))),
 sqrt(2)*sqrt((2-sqrt(t[1]*t[2]-4*t[1]+4))/(4-t[2])),
 -sqrt((2-sqrt(t[1]*t[2]-4*t[1]+4))/(4-t[2]))*sqrt(t[2])
]:

# The following quadratic vanishes on C[5] and C[7]:

oval_g := (x) -> x[3]*x[4]/sqrt(2)+x[1]^2+x[4]^2; #@ oval_g
oval_h := (z) -> z[1]^2*(z[2]-2) + 4*z[1] -2;     #@ oval_h

# The vectors oval_e[i] form an orthonormal basis, and oval_g(x) is the 
# sum of oval_m[i] <oval_e[i] , x>^2.

#@ oval_e
oval_e[1] := [1,0,0,0];
oval_e[2] := [0,1,0,0];
oval_e[3] := [0,0,sqrt(1/2+1/sqrt(6)),-sqrt(1/2-1/sqrt(6))];
oval_e[4] := [0,0,sqrt(1/2-1/sqrt(6)), sqrt(1/2+1/sqrt(6))];

#@ oval_m
oval_m[1] := 1;
oval_m[2] := 0;
oval_m[3] := -(sqrt(6)-2)/4;
oval_m[4] :=  (sqrt(6)+2)/4;

# The function c_alt[k] is an alternative parametrisation of C[k].

alpha_E := 5+2*sqrt(6);            #@ alpha_E
beta_E := sqrt(3) + sqrt(2);       #@ beta_E

#@ c_alt
c_alt[5] := (t) -> [
 sin(t)/beta_E,
 0,
 (1+beta_E^2)*(sqrt(1-sin(t)^2/beta_E^4)+cos(t)/beta_E^2)/12,
 (cos(t)-sqrt(1-sin(t)^2/beta_E^4))*sqrt(3)/6
];

c_alt[ 6] := unapply(simplify(act_E[ L](c_alt[5](t))),t);
c_alt[ 7] := unapply(simplify(act_E[ M](c_alt[5](t))),t);
c_alt[ 8] := unapply(simplify(act_E[LM](c_alt[5](t))),t);

c_alt_approx[5] := (t) -> [
 sin(t)/beta_E,
 0,
 (1+beta_E^2)*(1+cos(t)/beta_E^2)/12,
 (cos(t)-1)*sqrt(3)/6
];

for k from 0 to 4 do
 c_alt[k] := eval(c_E0[k]):
od:

# We can also describe C[k] (for k in {5,6,7,8}) as part of an elliptic curve.

#@ elliptic_a_poly 
elliptic_a_poly := (t,u) -> u^2-(1-(1-t)/(2*alpha_E^2))*(1-t^2);

#@ c_elliptic_a[5] 
c_elliptic_a[5] := unapply([(sqrt(2)+sqrt(3))*alpha_E*u,0,(alpha-(1-t)/2)^2,-sqrt(8)*alpha_E*(1-t)]/~(alpha_E^2-(1+t)^2/4),t,u);

#@ c_trig[5] 
c_trig[5] := unapply(c_elliptic_a[5](cos(t),sin(t)*sqrt(1-(1+cos(t))/2/alpha_E^2)),t);

c_elliptic_a[ 6] := unapply(simplify(act_E[ L](c_elliptic_a[5](t,u))),t,u);
c_elliptic_a[ 7] := unapply(simplify(act_E[ M](c_elliptic_a[5](t,u))),t,u);
c_elliptic_a[ 8] := unapply(simplify(act_E[LM](c_elliptic_a[5](t,u))),t,u);

c_trig[ 6] := unapply(simplify(act_E[ L](c_trig[5](t))),t);
c_trig[ 7] := unapply(simplify(act_E[ M](c_trig[5](t))),t);
c_trig[ 8] := unapply(simplify(act_E[LM](c_trig[5](t))),t);

#@ elliptic_b_poly 
elliptic_b_poly := tu -> tu[2]^2 - tu[1]^3 + 10*tu[1]^2 - tu[1];

#@ elliptic_b_proj 
elliptic_b_proj := (x) -> [x[1]^2,x[1]*(x[4]^2-(5/2)*sqrt(2)*x[3]*x[4]-1)];

# The next function finds a Fourier series approximation to c[k]
# It seems that the Fourier coefficients are 1/Pi times rational
# combinations of EllipticK and EllipticE of 1/sqrt(3).  We have 
# not worked out the details of this.  

#@ find_c_fourier 
find_c_fourier := proc()
 global c_fourier;
 local a,k,p,c;

 a[0] := int(1/sqrt(10 - 2*cos(t)),t=0..Pi)/Pi;

 for k from 1 to 10 do
  a[k] := int(cos(k*t)/sqrt(10 - 2*cos(t)),t=0..Pi)/Pi;
 od:
 
 p := add(a[k] * cos(k*t),k = 0..10);
 
 c_fourier[ 5] := unapply(combine([p * sin(t)/2,0,sqrt(2)*p,-sin(t/2)^2*p]),t);
end:

#@ set_c_fourier 
set_c_fourier := proc()
 global c_fourier;
 local B,C,p;

 B := [20/3, 44, 348, 20660/7, 1635748/63, 161901340/693, 19230235292/9009, 
       177658107628/9009, 28136843691100/153153, 263857298489132/153153];
 C := [-8, -160/3, -6328/15, -75136/21, -9914776/315, -196266848/693, 
       -116560420904/45045, -215368179200/9009, -34109227409288/153153, 
       -6077419529883040/2909907];

 p := (2/3)*sqrt(3)*EllipticK((1/3)*sqrt(3))/Pi + 
      add((B[k]*EllipticK(1/sqrt(3)) + C[k]*EllipticE(1/sqrt(3)))*sqrt(3)/Pi*cos(k*t),k=1..10);

 c_fourier[ 5] := unapply(combine([p * sin(t)/2,0,sqrt(2)*p,-sin(t/2)^2*p]),t);
end:

#@ propagate_c_fourier 
propagate_c_fourier := proc()
 global c_fourier,c_fourier0;
 local k;
 
 c_fourier[ 6] := unapply(simplify(act_E[ L](c_fourier[5](t))),t);
 c_fourier[ 7] := unapply(simplify(act_E[ M](c_fourier[5](t))),t);
 c_fourier[ 8] := unapply(simplify(act_E[LM](c_fourier[5](t))),t);

 for k from 5 to 8 do 
  c_fourier0[k] := unapply(evalf(c_fourier[k](t)),t);
 od:
end:

set_c_fourier();
propagate_c_fourier();

# Stereographic projection away from various vertices

#@ v_stereo
v_stereo[ 0] := (x) -> [x[1],x[2],x[4]] /~ (1 + x[3]);
v_stereo[ 3] := (x) -> [x[1],x[3],x[4]] /~ (1 + x[2]);
v_stereo[ 6] := (x) -> [(x[2]-x[1])/sqrt(2),x[3],x[4]] /~ (1+(x[1]+x[2])/sqrt(2));
v_stereo[11] := (x) -> [x[1],x[2],x[3]*sqrt(1/3)+x[4]*sqrt(2/3)] /~ (1+x[3]*sqrt(2/3)-x[4]*sqrt(1/3));

# Stereographic projection centred at a point near the middle of F16

#@ F16_stereo
proc()
 local a1,a2,a3,a4,x,xx;
 global F16_stereo;

 xx := [seq(x[i],i=1..4)];
 a1 := evalf([85/198, 623/990, (13/30)*sqrt(2), -104/495]):
 a2 := dg0(a1):
 a2 := simplify(a2 /~ sqrt(dp4(a2,a2))):
 a3 := simplify(subs(a_E=a_E0,tangent_u(a1))):
 a3 := simplify(expand(rationalize(a3 /~ sqrt(dp4(a3,a3))))):
 a4 := cross_product4(a1,a2,a3):

 F16_stereo := unapply([dp4(xx,a2),dp4(xx,a3),dp4(xx,a4)]/~(1+dp4(xx,a1)),x);
end():

# Tangent fields that are nowhere zero on F16

# The following definition would work after loading square_diffeo.mpl:
# tangent_a := unapply(simplify(expand(cross_product4(xx,dg0(xx),[seq(diff(square_diffeo_E0(x)[2],x[i]),i=1..4)]))),x);
# However, we just record the result inline here, so that it works
# without loading square_diffeo.mpl.  Note that the enties in this
# field are homogeneous quartic polynomials.

#@ tangent_a 
tangent_a := (x) -> [-x[3]^4+3*x[1]^2*sqrt(2)*x[3]*x[4]-3*x[2]^2*sqrt(2)*x[3]*x[4]-4*sqrt(2)*x[2]^3*x[4]-(3/2)*x[3]*x[1]^3*sqrt(2)+2*x[1]^3*sqrt(2)*x[4]-sqrt(2)*x[3]^3*x[4]-(3/2)*x[2]*x[4]^3*sqrt(3)+8*sqrt(2)*x[3]*x[4]^3+(3/2)*x[4]*sqrt(3)*x[2]^3+(3/4)*sqrt(2)*x[1]*x[3]^3-sqrt(2)*x[1]*x[2]^3+x[1]^3*x[2]*sqrt(2)-4*x[2]^2*x[1]^2+2*x[1]^2*x[3]^2+2*x[1]^2*x[4]^2-x[2]^2*x[3]^2-4*x[4]^2*x[2]^2+8*x[3]^2*x[4]^2+4*x[1]*x[2]^2*x[3]-4*x[1]^2*x[2]*x[3]+(3/2)*x[1]*x[2]^2*x[4]+16*x[1]*x[3]*x[4]^2-(3/4)*sqrt(2)*x[2]*x[1]^2*sqrt(3)*x[3]-(15/2)*x[2]*x[3]*x[4]^2*sqrt(2)*sqrt(3)+4*x[1]*x[2]*sqrt(2)*x[3]*x[4]+20*x[2]*x[3]*x[4]^2+3*x[1]*x[2]*x[3]^2+2*x[1]*x[2]*x[4]^2+6*x[1]*x[2]*x[3]*x[4]+4*x[1]^3*x[2]-(3/2)*x[1]^3*x[4]-2*x[1]*x[3]^3+4*x[3]*x[1]^3-4*x[2]^3*x[3]+2*x[2]*x[3]^3+2*x[1]*x[2]*x[3]^2*sqrt(2)-6*sqrt(2)*x[1]*x[3]*x[4]^2+12*x[2]*x[3]^2*x[4]*sqrt(2)+4*sqrt(2)*x[2]*x[1]^2*x[4]-2*sqrt(2)*x[1]*x[2]^2*x[4]-(39/4)*x[2]*x[3]^2*x[4]*sqrt(3)+(3/4)*sqrt(2)*x[2]^3*sqrt(3)*x[3]-(9/2)*x[2]*x[4]*sqrt(3)*x[1]^2-(3/2)*x[2]*x[3]^3*sqrt(2)*sqrt(3)-(3/2)*sqrt(2)*x[1]*x[3]*x[2]^2, -4*x[1]^4-x[3]^4-4*x[1]^2*x[3]*x[4]+2*x[2]^2*x[3]*x[4]-5*x[1]^2*sqrt(2)*x[3]*x[4]+x[2]^2*sqrt(2)*x[3]*x[4]+6*x[2]*x[3]*x[4]^2*sqrt(2)+(9/2)*x[4]*sqrt(3)*x[1]^3+(3/2)*x[1]*x[4]^3*sqrt(3)+(3/2)*sqrt(2)*x[2]^3*x[3]+2*x[3]^2*sqrt(2)*x[1]^2+x[1]^2*sqrt(2)*x[4]^2-x[2]^2*sqrt(2)*x[4]^2-(3/4)*x[2]*x[3]^3*sqrt(2)+2*sqrt(2)*x[2]^3*x[4]-3*x[3]*x[1]^3*sqrt(2)-4*x[1]^3*sqrt(2)*x[4]-sqrt(2)*x[3]^3*x[4]+8*sqrt(2)*x[3]*x[4]^3+(3/2)*sqrt(2)*x[1]*x[3]^3+7*x[1]^2*x[3]^2+2*x[2]^2*x[3]^2-2*x[4]^2*x[2]^2+8*x[3]^2*x[4]^2+4*x[1]*x[2]^2*x[3]-4*x[1]^2*x[2]*x[3]+3*x[1]*x[2]^2*x[4]-20*x[1]*x[3]*x[4]^2+(3/2)*x[1]^2*x[2]*x[4]+sqrt(2)*x[1]^2*x[2]^2-sqrt(2)*x[1]^4+8*x[1]*x[2]*sqrt(2)*x[3]*x[4]-(3/4)*sqrt(2)*x[1]*x[2]^2*sqrt(3)*x[3]+(21/2)*sqrt(2)*x[1]*x[3]*x[4]^2*sqrt(3)-16*x[2]*x[3]*x[4]^2-5*x[1]*x[2]*x[3]^2+2*x[1]*x[2]*x[4]^2+(3/2)*sqrt(2)*x[2]*x[3]*x[1]^2+12*x[1]*x[3]^2*x[4]*sqrt(2)-(9/4)*x[1]*x[3]^2*x[4]*sqrt(3)+(3/4)*x[1]^3*sqrt(2)*sqrt(3)*x[3]-(3/2)*x[1]*x[4]*sqrt(3)*x[2]^2-(3/2)*sqrt(2)*x[1]*x[3]^3*sqrt(3)+4*x[1]^3*x[2]-3*x[1]^3*x[4]-2*x[1]*x[3]^3+4*x[3]*x[1]^3-4*x[2]^3*x[3]+2*x[2]*x[3]^3-x[3]^3*x[4]-(3/2)*x[2]^3*x[4]+8*x[3]*x[4]^3-12*sqrt(2)*x[1]*x[3]*x[4]^2-2*sqrt(2)*x[2]*x[1]^2*x[4]+4*sqrt(2)*x[1]*x[2]^2*x[4]-3*sqrt(2)*x[1]*x[3]*x[2]^2, -4*x[1]^4+4*x[2]^4+3*x[1]^2*x[3]*x[4]+3*x[2]^2*x[3]*x[4]-2*x[1]*x[4]^3*sqrt(2)-2*x[2]*x[4]^3*sqrt(2)+(3/4)*x[3]^2*sqrt(2)*x[2]^2-4*x[1]^2*sqrt(2)*x[3]*x[4]-4*x[2]^2*sqrt(2)*x[3]*x[4]+2*x[2]*x[3]*x[4]^2*sqrt(2)-(3/4)*x[3]^2*sqrt(2)*x[1]^2+(3/2)*x[1]^2*sqrt(2)*x[4]^2-(3/2)*x[2]^2*sqrt(2)*x[4]^2-2*sqrt(2)*x[2]^3*x[4]-2*x[1]^3*sqrt(2)*x[4]+3*sqrt(2)*x[1]*x[2]^3+3*x[1]^3*x[2]*sqrt(2)+2*x[1]^2*x[3]^2-4*x[1]^2*x[4]^2-2*x[2]^2*x[3]^2+4*x[4]^2*x[2]^2+6*x[1]*x[2]^2*x[3]-10*x[1]^2*x[2]*x[3]-6*x[1]*x[3]*x[4]^2-2*x[1]^2*x[2]*x[4]+x[2]*x[3]^2*x[4]+3*x[1]*x[2]*x[3]^2*sqrt(2)*sqrt(3)+12*x[1]*x[2]*x[4]*sqrt(3)*x[3]+(3/2)*sqrt(2)*x[1]^4-(3/2)*sqrt(2)*x[2]^4+3*x[1]*x[2]*sqrt(2)*x[4]^2-16*x[1]*x[2]*sqrt(2)*x[3]*x[4]+2*x[2]*x[3]*x[4]^2-6*x[1]*x[2]*x[3]*x[4]-4*sqrt(2)*x[2]*x[3]*x[1]^2-x[1]*x[3]^2*x[4]*sqrt(2)+x[1]*x[3]^3-2*x[3]*x[1]^3-2*x[2]^3*x[3]+x[2]*x[3]^3-2*x[2]^3*x[4]-2*x[2]*x[4]^3-(3/2)*x[1]*x[2]*x[3]^2*sqrt(2)+3*x[2]*x[3]^2*x[4]*sqrt(2)-2*sqrt(2)*x[2]*x[1]^2*x[4]-2*sqrt(2)*x[1]*x[2]^2*x[4], (3/2)*x[1]^4+(3/2)*x[2]^4-12*x[1]^2*x[3]*x[4]+12*x[2]^2*x[3]*x[4]+4*x[3]^2*sqrt(2)*x[2]^2+(9/2)*x[1]^2*sqrt(2)*x[3]*x[4]-(9/2)*x[2]^2*sqrt(2)*x[3]*x[4]-6*x[2]*x[3]*x[4]^2*sqrt(2)+sqrt(2)*x[2]^3*x[3]+4*x[3]^2*sqrt(2)*x[1]^2-2*x[2]*x[3]^3*sqrt(2)+sqrt(2)*x[2]^3*x[4]-x[3]*x[1]^3*sqrt(2)+2*sqrt(2)*x[1]*x[3]^3-3*x[2]^2*x[1]^2-3*x[1]^2*x[3]^2-3*x[2]^2*x[3]^2+2*x[1]*x[2]^2*x[4]-2*x[1]^2*x[2]*x[4]-10*x[2]*x[3]^2*x[4]-2*x[1]*x[3]^2*x[4]+4*sqrt(2)*x[1]^2*x[2]^2-2*sqrt(2)*x[1]^4-2*sqrt(2)*x[2]^4+9*x[1]*x[2]*sqrt(2)*x[3]*x[4]-6*x[2]*x[3]*x[4]^2+6*x[1]*x[2]*x[3]^2-3*x[1]*x[2]*sqrt(2)*x[3]*x[4]*sqrt(3)+3*sqrt(2)*x[2]*x[3]*x[1]^2+3*x[1]^3*x[2]-2*x[1]^3*x[4]+2*x[2]^3*x[4]-3*x[1]*x[2]^3-8*x[1]*x[2]*x[3]^2*sqrt(2)-6*sqrt(2)*x[1]*x[3]*x[4]^2-2*x[2]*x[3]^2*x[4]*sqrt(2)-sqrt(2)*x[2]*x[1]^2*x[4]-3*sqrt(2)*x[1]*x[3]*x[2]^2]:

#@ unit_tangent_a 
unit_tangent_a := proc(x)
 local u;
 u := tangent_a(x);
 u := evalf(u /~ nm4(u));
 return u;
end:

#@ full_frame_a 
full_frame_a := proc(x::[numeric,numeric,numeric,numeric])
 local n,r,u,v;

 n := evalf(dg0(x));
 n := n /~ nm4(n);
 u := unit_tangent_a(x);
 v := cross_product4(x,n,u);
 return [x,n,u,v];
end:

#@ tangent_frame_a 
tangent_frame_a := proc(x::[numeric,numeric,numeric,numeric])
 local f;
 f := full_frame_a(x);
 return [f[3],f[4]];
end:

# This function projects a fixed vector u0 = [-1,18,-13,10] into
# the tangent plane to get a vector u.  With this choice of u0, 
# it works out that |u|/|u0| is always at least 0.7 or so, for
# all choices of x in F16.  With more obvious choices of u0 we
# would find that |u|/|u0| would be very small or zero for some
# choices of x.

#@ full_frame_b 
full_frame_b := proc(x::[numeric,numeric,numeric,numeric])
 local n,r,u,v;

 n := evalf(dg0(x));
 n := n /~ nm4(n);
 u := [-1,18,-13,10];
 u := evalf(u -~ dp4(u,x) *~ x);
 u := evalf(u -~ dp4(u,n) *~ n);
 u := u /~ nm4(u);
 v := cross_product4(x,n,u);
 return [x,n,u,v];
end:

#@ unit_tangent_b 
unit_tangent_b := (x::[numeric,numeric,numeric,numeric]) -> full_frame_b(x)[3];

#@ tangent_frame_b 
tangent_frame_b := proc(x::[numeric,numeric,numeric,numeric])
 local f;
 f := full_frame_b(x);
 return [f[3],f[4]];
end:


######################################################################

#@ cubic_chart0
proc()
 local gg,aa,bb,nn,yy,t,tt,i,j,k;
 global cubic_chart0;

 tt := [seq(t[i],i=1..4)]:
 for i from 1 to 4 do
  for j from 1 to 4 do 
   for k from 1 to 4 do
    gg[i,j,k] := diff(diff(diff(g0(xx),x[i]),x[j]),x[k])/6:
   od:
  od:
 od:

 aa := add(add(add(add(add(gg[i,j,k]*gg[k,l,m]*x[i]*t[j]*x[l]*x[m],m=1..4),l=1..4),k=1..4),j=1..4),i=1..4):
 bb := dp4(dg0(tt),xx):
 nn := square_norm_of_dg0(xx):
 yy := (1 - dp4(tt,tt)/2) *~ xx +~ tt +~ ((18 * aa - nn) * bb / nn^2 - g0(tt)/nn) *~ dg0(xx):
 cubic_chart0 := unapply(yy,x,t): 
end():

######################################################################

#@ annular_chart0

for i from 0 to 2 do 
 annular_chart0[i] :=
  unapply(subs(csgn(cos(s[1])^2-2) = -1,
               simplify(subs(a_E=a_E0,annular_chart[i](s)))),s);
od:

annular_chart0[3] := unapply(subs({t=s[1],u=s[2]},
 [(44*cos(2*t)*u^3+cos(4*t)*u^3+115*u^3+60*cos(2*t)*u-3*cos(4*t)*u-153*u)/
     (-3*cos(4*t)-153+60*cos(2*t)),
  sin(t)-(1/2)*sin(t)*u^2,
  (-u^2*sqrt(6)*cos(3*t)-7*u^2*sqrt(6)*cos(t)+2*sqrt(6)*cos(3*t)-18*sqrt(6)*cos(t))/
     (12*cos(2*t)-60),
  (-41*cos(t)*sqrt(3)*u^2+cos(3*t)*sqrt(3)*u^2+18*cos(t)*sqrt(3)-2*cos(3*t)*sqrt(3))/
     (12*cos(2*t)-60)]),s);

annular_chart0[4] := unapply(act_R4[L](annular_chart0[3](s)),s);

annular_chart0[5] := unapply(subs({t=s[1],u=s[2]},[
  (1/2)*sin(t)*sqrt(2)/sqrt(5-cos(t)) +
   (1/16)*sqrt(2)*(cos(t)^3-21*cos(t)^2+79*cos(t)+53)*sin(t)*u^2/((5-cos(t))^(5/2)*(3-cos(t))),
  (1/2)*sqrt(cos(t)^2-2*cos(t)+9)*u/(5-cos(t)),
  2/sqrt(5-cos(t))-(1/4)*(cos(t)^3-5*cos(t)^2-17*cos(t)+37)*u^2/((5-cos(t))^(5/2)*(3-cos(t))),
  -(1/2)*(1-cos(t))*sqrt(2)/sqrt(5-cos(t))+
   (1/16)*sqrt(2)*(cos(t)^4-22*cos(t)^3+84*cos(t)^2+38*cos(t)+27)*u^2/((5-cos(t))^(5/2)*(3-cos(t)))
  ]),s);

annular_chart0[ 6] := unapply(simplify(act_E[ L](annular_chart0[5](s))),s);
annular_chart0[ 7] := unapply(simplify(act_E[ M](annular_chart0[5](s))),s);
annular_chart0[ 8] := unapply(simplify(act_E[LM](annular_chart0[5](s))),s);

######################################################################

#@ is_geodesic 
is_geodesic := proc(c,d)
 local t,c0,c1,c2,n0,err;

 c0 := multi_series(c(t),d+2,t);
 c1 := map(diff,c0,t);
 c2 := map(diff,c1,t);
 n0 := multi_series(dg0(c0),d+2,t);
 n0 := multi_series(n0 /~ nm4(n0),d+2,t);
 err := c0;
 err := multi_series(err - dp4(err,c0) *~ c0,d,t);
 err := multi_series(err - dp4(err,n0) *~ n0,d,t);
 return tidy(err);
end:

#@ euclidean_spray 
euclidean_spray := proc(x::RR1_4,d::posint := 4)
 local C,p,q,r,s,t,E;

 C := `new/E_chart`();
 C["centre_set_numeric",x0];
 C["set_degree_numeric",6];
 p := eval(C["p"]);
 E := metric_from_embedding(p(s),s[1],s[2])[1]:
 E := tidy(multi_series(E,d,s[1],s[2]));
 q := unapply(geodesic_spray_R2(E,s[1],s[2],d),s);
 r := multi_series(p(q(s)),d,s[1],s[2]);
 return unapply(r,s);
end:

#@ tubular_F0 
tubular_F0 := [u[1],u[2],u[3],u[4]] +~ u[5] *~ dg0([u[1],u[2],u[3],u[4]]);
tubular_F0 := Vector([op(tubular_F0),g0([u[1],u[2],u[3],u[4]])]);

#@ tubular_F 
tubular_F := unapply(tubular_F0,u);

#@ tubular_J0 
tubular_J0 := map(expand,Matrix([seq(map(diff,tubular_F0,u[i]),i=1..5)]));

#@ tubular_J 
tubular_J := unapply(tubular_J0,u);

#@ tubular_F_inv 
tubular_F_inv := proc(x0::RR0_4)
 local i,u0,du,a0,da,x1,tol;

 tol := 10.^(-95);
 u0 := Vector(evalf([op(x0),0]));
 a0 := u0;
 du := evalf(u0 - tubular_F(a0));
 i := 0;
 while Norm(du,2) > tol do 
  i := i+1;
  da := LinearSolve(evalf(tubular_J(a0)),du);
  a0 := a0 + da;
  du := evalf(u0 - F(a0));
 od;
 return a0;
end:

#@ tubular_retract 
tubular_retract := proc(x0::RR0_4)
 local a0,x1;
 a0 := tubular_F_inv(x0);
 x1 := [a0[1],a0[2],a0[3],a0[4]];
 x1 := x1 /~ nm4(x1);
 return x1;
end:

######################################################################

#@ gauss_map0 
gauss_map0 := unapply(
 [2*x[3]*sqrt(2)*x[4]*x[1]-2*sqrt(2)*x[2]^3+x[3]^2*sqrt(2)*x[2]-x[4]^2*sqrt(2)*x[2]-3*x[1]*x[3]^2+6*x[3]*x[4]*x[2]+sqrt(2)*x[2]+2*x[1],
  2*sqrt(2)*x[1]*x[2]^2+3*sqrt(2)*x[1]*x[3]^2+sqrt(2)*x[1]*x[4]^2-2*sqrt(2)*x[2]*x[3]*x[4]-6*x[1]*x[3]*x[4]-3*x[2]*x[3]^2-sqrt(2)*x[1]+2*x[2],
 -4*x[3]*sqrt(2)*x[2]*x[1]-2*sqrt(2)*x[2]^2*x[4]-sqrt(2)*x[3]^2*x[4]-sqrt(2)*x[4]^3-3*x[3]^3+6*x[3]*x[4]^2+x[4]*sqrt(2)+2*x[3]] /~
 sqrt(6+6*sqrt(2)*x[3]^3*x[4]-8*x[2]^2+8*x[2]^4+4*x[4]^2-4*x[3]^2-x[3]^4+8*x[4]^2*x[2]^2-4*x[3]*sqrt(2)*x[4]-8*x[3]^2*x[4]^2+2*x[4]^4),x);

######################################################################

#@ owl_proj 
owl_proj := (x) -> [x[1],x[2],(x[3]-x[4])/sqrt(2)];
