######################################################################
# The declarations below allow us to specify the types of arguments
# to functions that we will define later.  

# Although type RR is intended to refer to real numbers, Maple
# will not test this very stringently.  It will reject sets, lists,
# vectors and so on, but it will accept any algebraic expression.

# Type RR0 will accept expressions that refer to real constants,
# such as sqrt(2) + exp(5) / 1.23.  It will reject expressions
# that involve variables, or that refer to non-real complex
# constants.

# Type RR1 will accept numeric constants like 1.2345 or 1/13, but
# reject symbolic expressions like Pi or sqrt(3).

`type/RR`  := (u) -> type(u,algebraic);           #@ RR
`type/RR0` := (u) -> type(u,realcons);            #@ RR0
`type/RR1` := (u) -> type(u,numeric);             #@ RR1

# Types CC, CC0 and CC1 are analogous to RR, RR0 and RR1, but
# for complex numbers.

`type/CC`  := (u) -> type(u,algebraic);           #@ CC
`type/CC0` := (u) -> type(u,constant);            #@ CC0
`type/CC1` := (u) -> type(u,complex(numeric));    #@ CC1

# Types RR_2, RR0_2 and RR1_2 are for elements of R^2, which
# we represent as lists.  (Note that Maple also has Vectors,
# which are different from lists, but for technical reasons we
# generally prefer to use lists.)

`type/RR_2`  := (u) -> type(u,[algebraic,algebraic]); #@ RR_2
`type/RR0_2` := (u) -> type(u,[realcons,realcons]);   #@ RR0_2
`type/RR1_2` := (u) -> type(u,[numeric,numeric]);     #@ RR1_2

# Types RR_3, RR0_3 and RR1_3 are for elements of R^3
`type/RR_3`  := (u) -> type(u,[algebraic,algebraic,algebraic]);  #@ RR_3
`type/RR0_3` := (u) -> type(u,[realcons,realcons,realcons]);     #@ RR0_3
`type/RR1_3` := (u) -> type(u,[numeric,numeric,numeric]);        #@ RR1_3

# Types RR_4, RR0_4 and RR1_4 are for elements of R^4
`type/RR_4`  := (u) -> type(u,[algebraic,algebraic,algebraic,algebraic]); #@ RR_4
`type/RR0_4` := (u) -> type(u,[realcons,realcons,realcons,realcons]);     #@ RR0_4
`type/RR1_4` := (u) -> type(u,[numeric,numeric,numeric,numeric]);         #@ RR1_4

protect(
 'RR','RR0','RR1',
 'CC','CC0','CC1',
 'RR_2','RR0_2','RR1_2',
 'RR_3','RR0_3','RR1_3',
 'RR_4','RR0_4','RR1_4'
);

######################################################################
# Norms, dot products, cross products

# Distance between points in R^2, R^3 and R^4

nm2 := (u::RR_2) -> sqrt(add(u[i]^2,i=1..2)); #@ nm2 
nm3 := (u::RR_3) -> sqrt(add(u[i]^2,i=1..3)); #@ nm3 
nm4 := (u::RR_4) -> sqrt(add(u[i]^2,i=1..4)); #@ nm4 

dist := (d,a::list(RR),b::list(RR)) -> sqrt(add((a[i]-b[i])^2,i=1..d)): #@ dist 

d2  := proc(u::RR_2 ,v::RR_2 ) sqrt(add((u[i]-v[i])^2,i=1..2)); end: #@ d2  
d2f := proc(u::RR0_2,v::RR0_2) evalf(d2(u,v)); end:                  #@ d2f 
d3  := proc(u::RR_3 ,v::RR_3 ) sqrt(add((u[i]-v[i])^2,i=1..3)); end: #@ d3  
d3f := proc(u::RR0_3,v::RR0_3) evalf(d3(u,v)); end:                  #@ d3f 
d4  := proc(u::RR_4 ,v::RR_4 ) sqrt(add((u[i]-v[i])^2,i=1..4)); end: #@ d4  
d4f := proc(u::RR0_4,v::RR0_4) evalf(d4(u,v)); end:                  #@ d4f 

# Dot products in R^2, R^3 and R^4

dp := (d,a::list(RR),b::list(RR)) -> add(a[i]*b[i],i=1..d):                #@ dp 
dp2 := (a::RR_2,b::RR_2) -> a[1]*b[1] + a[2]*b[2]:                         #@ dp2 
dp3 := (a::RR_3,b::RR_3) -> a[1]*b[1] + a[2]*b[2] + a[3]*b[3]:             #@ dp3 
dp4 := (a::RR_4,b::RR_4) -> a[1]*b[1] + a[2]*b[2] + a[3]*b[3] + a[4]*b[4]: #@ dp4 
dpv := (a::list(RR),b::list(RR)) -> `+`(op(a *~ b));                       #@ dpv 
distv := (a::list(RR),b::list(RR)) -> sqrt(dpv(a -~ b,a -~ b));            #@ distv 

# Ordinary cross product of three-dimensional vectors.

#@ cross_product
cross_product := (u::RR_3,v::RR_3) -> [
 u[2] * v[3] - u[3] * v[2],
 u[3] * v[1] - u[1] * v[3],
 u[1] * v[2] - u[2] * v[1]
]:


# cross_product4(u,v,w) is a vector in R^4 depending on vectors
# u,v,w in R^4.  It is orthogonal to each of u, v and w.  If 
# u,v and w are othogonal then |cross_product4(u,v,w)| = |u||v||w|.

#@ cross_product4
cross_product4 := (u::RR_4,v::RR_4,w::RR_4) -> [
 -u[2]*v[3]*w[4]+u[2]*v[4]*w[3]-v[2]*w[3]*u[4]+v[2]*u[3]*w[4]-w[2]*u[3]*v[4]+w[2]*v[3]*u[4], 
  u[1]*v[3]*w[4]-u[1]*v[4]*w[3]+v[1]*w[3]*u[4]-v[1]*u[3]*w[4]+w[1]*u[3]*v[4]-w[1]*v[3]*u[4], 
 -u[1]*v[2]*w[4]+u[1]*v[4]*w[2]-v[1]*w[2]*u[4]+v[1]*u[2]*w[4]-w[1]*u[2]*v[4]+w[1]*v[2]*u[4], 
  u[1]*v[2]*w[3]-u[1]*v[3]*w[2]+v[1]*w[2]*u[3]-v[1]*u[2]*w[3]+w[1]*u[2]*v[3]-w[1]*v[2]*u[3]
]:

######################################################################
# Isometric simplices

# Map from the 2-simplex to the equilateral triangle in R^2 with 
# vertices [1,0] and [-1/2,+/- sqrt(3)/2].

# Arguments can be given as (t1,t2) or ([t1,t2]) or (t0,t1,t2) or
# ([t0,t1,t2]).  If t0 is missing then it is taken to be 1-t1-t2.

#@ triangle_proj
triangle_proj := proc()
 local tt;
 if nargs = 1 then tt := args[1] else tt := [args]; fi;
 if nops(tt) = 2 then tt = [1 - tt[1] - tt[2],tt[1],tt[2]]; fi;
 return([tt[1]-tt[2]/2-tt[3]/2,(tt[2]-tt[3])*sqrt(3)/2]);
end:

#@ triangle_lift
triangle_lift := (xy) -> [1/3+(2/3)*xy[1],
                          1/3-(1/3)*xy[1]+(1/3)*sqrt(3)*xy[2], 
                          1/3-(1/3)*xy[1]-(1/3)*sqrt(3)*xy[2]]:

######################################################################

# Conversion between complex numbers and pairs of real numbers.

C_to_R2 := (z) -> [Re(z),Im(z)];       #@ C_to_R2 
R2_to_C := (xy) -> xy[1] + I * xy[2];  #@ R2_to_C 

C_mult := (u,v) -> [u[1]*v[1]-u[2]*v[2], u[1]*v[2]+u[2]*v[1]]; #@ C_mult 

# Any polynomial f : R -> R extends in an obvious way to give a
# function C -> C, and by identifying C with R^2 we obtain a 
# function g : R^2 -> R^2.  The function below converts f to g. 

#@ series_on_R2
series_on_R2 := proc(f)
 local t,u,ft,d,up,ans,i,c;
 ft := f(t);
 d := degree(ft,t);
 up := [1,0];
 ans := [0,0];
 for i from 0 to d do
  c := coeff(ft,t,i);
  ans := expand(ans +~ c *~ up);
  up := expand([u[1] * up[1] - u[2] * up[2],u[1] * up[2] + u[2] * up[1]]);
 od;
 return unapply(ans,u);
end:

######################################################################
# Several versions of stereographic projection

# stereo() is the stereographic homeomorphism from 
# S^3 minus { [0,0,0,1] } to R^3.  The inverse homeomorphism is 
# unstereo(). 

#@ S3_to_R3
S3_to_R3 := (x::RR_4) -> [x[1]/(1-x[4]),x[2]/(1-x[4]),x[3]/(1-x[4])];

#@ R3_to_S3
R3_to_S3 := (u::RR_3) -> [seq(2*u[i]/(add(u[j]^2,j=1..3)+1),i=1..3),
                          (add(u[j]^2,j=1..3)-1)/(add(u[j]^2,j=1..3)+1)];

stereo   := eval(S3_to_R3); #@ stereo   
unstereo := eval(R3_to_S3); #@ unstereo 

# Some convenience functions for plotting via stereographic projection.

#@ stereo_point
stereo_point      := proc(x::RR_4) point(stereo(x),args[2..-1]); end:

#@ stereo_curve
stereo_curve      := proc(xs::list(RR_4)) curve(map(stereo,xs),args[2..-1]); end:

#@ stereo_polygon
stereo_polygon    := proc(xs::list(RR_4)) polygon(map(stereo,xs),args[2..-1]); end:

#@ stereo_spacecurve
stereo_spacecurve := proc(x::RR_4) spacecurve(stereo(x),args[2..-1]); end:

#@ R_to_S1
R_to_S1 := proc(x::RR) 
 if x = infinity then
  return([-1,0]);
 else
  return([(1-x^2)/(1+x^2),2*x/(1+x^2)]);
 fi;
end:

#@ S1_to_R 
S1_to_R := proc(u::RR_2) 
 if u[1] = -1 then
  return infinity;
 else
  return u[2]/(1 + u[1]);
 fi;
end:

# SC1 is the unit circle in C (rather than R^2)

#@ R_to_SC1 
R_to_SC1 := proc(x::RR)  
 if x = infinity then
  return(-1);
 else
  return((1-x^2+2*I*x)/(1+x^2));
 fi;
end:

#@ RP1_to_SC1 
RP1_to_SC1 := proc(xy::RR_2)  
 return (xy[2]^2-xy[1]^2+2*I*xy[1]*xy[2])/(xy[1]^2+xy[2]^2);
end:

# SC1_to_R and SC1_to_R_alt are in theory the same.  However,
# SC1_to_R(z) is guaranteed to be real, even if |z| is slightly
# different from 1.  On the other hand, SC1_to_R_alt  

#@ SC1_to_R 
SC1_to_R := proc(u::CC) 
 if u = -1 then
  return infinity;
 else
  return Im(u)/(1 + Re(u));
 fi;
end:

SC1_to_R_alt := (z) -> I*(1-z)/(1+z); #@ SC1_to_R_alt 

# I is the interval [-1,1]

R_to_I   := (t) -> t/sqrt(1+t^2);             #@ R_to_I   
I_to_R   := (t) -> t/sqrt(1-t^2);             #@ I_to_R   

S1_to_I  := u -> u[2]/sqrt(2*(1+u[1]));       #@ S1_to_I  
I_to_S1  := t -> [1-2*t^2,2*t*sqrt(1-t^2)];   #@ I_to_S1  

SC1_to_I := z -> Im(z)/sqrt(2*(1+Re(z)));     #@ SC1_to_I 

# tan_sum satisfies tan_sum(tan(s),tan(t)) = tan(s+t).

tan_sum := (a,b) -> (a + b)/(1 - a*b);        #@ tan_sum 

# C_to_S2 is the usual homeomorphism from the Riemann sphere to
# S^2, normalised to send the unit circle in C to the equator.

#@ C_to_S2 
C_to_S2 := proc(z::CC) 
 if z = infinity then 
  return([0,0,1]);
 else
  return 
   [2*Re(z)/(1+abs(z)^2),2*Im(z)/(1+abs(z)^2),(abs(z)^2-1)/(1+abs(z)^2)];
 fi;
end:

#@ S2_to_C 
S2_to_C := proc(u::RR_3) 
 if u[3] = 1 then
  return(infinity);
 else 
  return(simplify((u[1]+I*u[2])/(1-u[3])));
 fi;
end:

#@ S2_tangent_frame
S2_tangent_frame := proc(a::RR1_3)
 local e,b,c,i,j,m;

 e[1] := [1,0,0];
 e[2] := [0,1,0];
 e[3] := [0,0,1];
 i := 0;
 m := 0;

 for j from 1 to 3 do 
  e[j] := e[j] - dp3(e[j],a) *~ a;
  if nm3(e[j]) > m then 
   m := nm3(e[j]);
   i := j;
  fi;
 od;

 b := e[i] /~ nm3(e[i]);
 c := cross_product(b,a);
 return [b,c];
end:

# stereo_shift(x,a) is a version of stereographic projection that
# sends a to zero and -a to infinity.

#@ stereo_shift 
stereo_shift := (x::RR_4, a::RR_4) ->  
[(-x[1]*a[4]+x[2]*a[3]-x[3]*a[2]+x[4]*a[1])/(1+x[1]*a[1]+x[2]*a[2]+x[3]*a[3]+x[4]*a[4]),
 (-x[1]*a[3]-x[2]*a[4]+x[3]*a[1]+x[4]*a[2])/(1+x[1]*a[1]+x[2]*a[2]+x[3]*a[3]+x[4]*a[4]),
 (x[1]*a[2]-x[2]*a[1]-x[3]*a[4]+x[4]*a[3])/(1+x[1]*a[1]+x[2]*a[2]+x[3]*a[3]+x[4]*a[4])]:

#@ unstereo_shift 
unstereo_shift := (u::RR_3,a::RR_4) -> [ 
 -(2*u[1]*a[4]+2*u[2]*a[3]-2*u[3]*a[2]+a[1]*u[1]^2+a[1]*u[2]^2+a[1]*u[3]^2-a[1])/(u[1]^2+u[2]^2+u[3]^2+1),
 -(-2*u[1]*a[3]+2*u[2]*a[4]+2*u[3]*a[1]+a[2]*u[1]^2+a[2]*u[2]^2+a[2]*u[3]^2-a[2])/(u[1]^2+u[2]^2+u[3]^2+1),
 (-2*u[1]*a[2]+2*u[2]*a[1]-2*u[3]*a[4]-a[3]*u[1]^2-a[3]*u[2]^2-a[3]*u[3]^2+a[3])/(u[1]^2+u[2]^2+u[3]^2+1),
 (2*u[1]*a[1]+2*u[2]*a[2]+2*u[3]*a[3]-a[4]*u[1]^2-a[4]*u[2]^2-a[4]*u[3]^2+a[4])/(u[1]^2+u[2]^2+u[3]^2+1)]:

######################################################################
# A Mobius function that preserves the unit circle

mob := (z,a) -> (z - a)/(1 - conjugate(a)*z); #@ mob 

# This assumes that u and v lie in the unit disc, and returns the
# unique holomorphic involution of the unit disc that exchanges
# u and v.

#@ switcher 
switcher := proc(u,v) 
 local a,z;
 a := (u+v+conjugate(u+v)*u*v)/(1-abs(u*v)^2);
 return unapply((a-z)/(1-conjugate(a)*z),z);
end:

#@ midpoint_Delta 
midpoint_Delta := proc(u,v) 
 local a;
 a := (u+v+conjugate(u+v)*u*v)/(1-abs(u*v)^2);
 if a = 0 then
  return 0;
 else 
  return ((1-sqrt(1-abs(a)^2))/conjugate(a));
 fi;
end:

######################################################################
# Polar coordinates on S^3

#@ S3_polar 
S3_polar := (abc::RR_3) ->
 [cos(abc[1])*cos(abc[2]),cos(abc[1])*sin(abc[2]),
  sin(abc[1])*cos(abc[3]),sin(abc[1])*sin(abc[3])];

#@ S3_polar_inv 
S3_polar_inv := (x::RR_4) ->
 [arcsin(sqrt(x[3]^2+x[4]^2)),
  arcsin(x[2]/sqrt(x[1]^2+x[2]^2)),
  arcsin(x[4]/sqrt(x[3]^2+x[4]^2))];

######################################################################
# Various different models for the 2-torus:
#
# T refers to points s in R^4 with s[1]^2+s[2]^2 = s[3]^2+s[4]^2 = 1.
# TA refers to pairs of angles
# TC refers to pairs of unit complex numbers
# TP refers to (R u infinity)^2

TA_to_TC := (theta) -> [exp(I*theta[1]),exp(I*theta[2])]; #@ TA_to_TC 
TA_to_TP := (theta) -> [tan(theta[1]/2),tan(theta[2]/2)]; #@ TA_to_TP 
TA_to_T  := (theta) -> [cos(theta[1]),sin(theta[1]),cos(theta[2]),sin(theta[2])]; #@ TA_to_T  
TC_to_TA := (z) -> [argument(z[1]),argument(z[2])]; #@ TC_to_TA 
TC_to_TP := (z) -> [Im(z[1])/(1+Re(z[1])),Im(z[2])/(1+Re(z[2]))]; #@ TC_to_TP 
TC_to_T  := (z) -> [Re(z[1]),Im(z[1]),Re(z[2]),Im(z[2])]; #@ TC_to_T  
TP_to_TA := (t) -> [2*arctan(t[1]),2*arctan(t[2])]; #@ TP_to_TA 
TP_to_TC := (t) -> [(1+I*t[1])/(1-I*t[1]),(1+I*t[2])/(1-I*t[2])]; #@ TP_to_TC 
TP_to_T  := (t) -> [(1-t[1]^2)/(1+t[1]^2),2*t[1]/(1+t[1]^2), 
                    (1-t[2]^2)/(1+t[2]^2),2*t[2]/(1+t[2]^2)]; #@ TP_to_T  
T_to_TA  := (s) -> [arctan(s[2],s[1]),arctan(s[4],s[3])]; #@ T_to_TA  
T_to_TC  := (s) -> [s[1]+I*s[2],s[3]+I*s[4]]; #@ T_to_TC  
T_to_TP  := (s) -> [s[2]/(1+s[1]),s[4]/(1+s[3])]; #@ T_to_TP  
 
# T_to_S2 gives a homeomorphism T/C_2 -> S^2.
T_to_S2   := (s)     -> [s[3]-s[1],s[2]*s[4],-s[3]-s[1]]/~ sqrt((1+s[1]^2)*(1+s[3]^2)); #@ T_to_S2   
TP_to_S2  := (a)     -> [a[1]^2-a[2]^2, 2*a[1]*a[2], a[1]^2*a[2]^2-1] /~ sqrt((1+a[1]^4)*(1+a[2]^4)); #@ TP_to_S2  
TA_to_S2  := (theta) -> T_to_S2(TA_to_T(theta)); #@ TA_to_S2  
TC_to_S2  := (z)     -> T_to_S2(TC_to_T(z)); #@ TC_to_S2  

# This is a generically defined section for T_to_S2
#@ S2_to_TP 
S2_to_TP := (u) -> [ 
 abs(u[2])/sqrt((sqrt(1-u[1]^2)-u[3])*(sqrt(1-u[3]^2)-u[1])),
     u[2] /sqrt((sqrt(1-u[1]^2)-u[3])*(sqrt(1-u[3]^2)+u[1]))
];

# T_to_R3 is the standard embedding of T in R^3.
T_to_R3  := (t) -> [t[1]*(2+t[3]),t[2]*(2+t[3]),t[4]]; #@ T_to_R3  
TA_to_R3 := (theta) -> T_to_R3(TA_to_T(theta));        #@ TA_to_R3 
TC_to_R3 := (z)     -> T_to_R3(TC_to_T(z));            #@ TC_to_R3 
TP_to_R3 := (t)     -> T_to_R3(TP_to_T(t));            #@ TP_to_R3 

# We next have similar functions for the 4-torus

TTA_to_TTC := (theta) -> [seq(exp(I*theta[i]),i=1..4)]; #@ TTA_to_TTC 
TTA_to_TTP := (theta) -> [seq(tan(theta[i]/2),i=1..4)]; #@ TTA_to_TTP 
TTA_to_TT  := (theta) -> map(op,[seq([cos(theta[i]),sin(theta[i])],i=1..4)]); #@ TTA_to_TT  
TTC_to_TTA := (z) -> [seq(argument(z[i]),i=1..4)]; #@ TTC_to_TTA 
TTC_to_TTP := (z) -> [seq(Im(z[i])/(1+Re(z[i])),i=1..4)]; #@ TTC_to_TTP 
TTC_to_TT  := (z) -> map(op,[seq([Re(z[i]),Im(z[i])],i=1..4)]); #@ TTC_to_TT  
TTP_to_TTA := (t) -> [seq(2*arctan(t[i]),i=1..4)]; #@ TTP_to_TTA 
TTP_to_TTC := (t) -> [seq((1+I*t[i])/(1-I*t[i]),i=1..4)]; #@ TTP_to_TTC 
TTP_to_TT  := (t) -> map(op,[seq([(1-t[i]^2)/(1+t[i]^2),2*t[i]/(1+t[i]^2)],i=1..4)]); #@ TTP_to_TT  
TT_to_TTA  := (s) -> [seq(arctan(s[2*i],s[2*i-1]),i=1..4)]; #@ TT_to_TTA  
TT_to_TTC  := (s) -> [seq(s[2*i-1]+I*s[2*i],i=1..4)]; #@ TT_to_TTC  
TT_to_TTP  := (s) -> [seq(s[2*i]/(1+s[2*i-1]),i=1..4)]; #@ TT_to_TTP  

######################################################################
# Quaternions 

# We identify R^4 with the quaternions by the rule
# [x1,x2,x3,x4] |-> x1 i + x2 j + x3 k + x4.

#@ H_mult 
H_mult := proc(a::RR_4,b::RR_4) 
 [  a[1]*b[4] + a[2]*b[3] - a[3]*b[2] + a[4]*b[1] ,
  - a[1]*b[3] + a[2]*b[4] + a[3]*b[1] + a[4]*b[2] ,
    a[1]*b[2] - a[2]*b[1] + a[3]*b[4] + a[4]*b[3] ,
  - a[1]*b[1] - a[2]*b[2] - a[3]*b[3] + a[4]*b[4]
 ];
end:

#@ H_conjugate 
H_conjugate := proc(a::RR_4)
 [-a[1],-a[2],-a[3],a[4]];
end:

#@ H_quotient 
H_quotient := proc(a,b)
 H_mult(a,H_conjugate(b)) /~ nm4(b)^2;
end:

######################################################################
# The Hopf fibration

# The Hopf fibration from R^4\{0} to S^2
#@ hopf_map 
hopf_map := (x::RR_4) ->
 [2*x[1]*x[3]+2*x[2]*x[4],
  2*x[1]*x[4]-2*x[2]*x[3],
  x[1]^2+x[2]^2-x[3]^2-x[4]^2]/~(rho(x));

#@ find_hopf_preimages 
find_hopf_preimages := proc(y::RR_3)
 local eqs,sol,fsol,is_real,x,xx; 
 xx := [x[1],x[2],x[3],x[4]];
 is_real := (u) -> (Im(u[1]) = 0 and Im(u[2]) = 0 and Im(u[3]) = 0 and Im(u[4]) = 0):
 eqs := {g0(x)=0,hopf_proj(x)[1]=y[1],hopf_proj(x)[2]=y[2],hopf_proj(x)[3]=y[3]};
 sol := [solve(eqs,{op(xx)})]:
 fsol := select(is_real,evalf(map(s -> subs(s,xx),sol)));
 return(fsol);
end:

#@ act_hopf
act_hopf[1]    := (u) -> [ u[1], u[2], u[3]];
act_hopf[LL]   := (u) -> [-u[1],-u[2], u[3]];
act_hopf[LM]   := (u) -> [ u[2], u[1], u[3]];
act_hopf[LLLM] := (u) -> [-u[2],-u[1], u[3]];
act_hopf[LN]   := (u) -> [-u[2],-u[1], u[3]];
act_hopf[LLLN] := (u) -> [ u[2], u[1], u[3]];
act_hopf[MN]   := (u) -> [-u[1],-u[2], u[3]];
act_hopf[LLMN] := (u) -> [ u[1], u[2], u[3]];


######################################################################
# Random points on spheres

#@ random_S3_point 
random_S3_point := proc()
 local x,rr,nx;
 rr := rand(-1000..1000);
 nx := 0;
 while nx < 0.1 or nx > 1 do 
  x := [rr(),rr(),rr(),rr()] /~ 1000.;
  nx := sqrt(add(x[i]^2,i=1..4));
 od;
 x := x /~ nx;
 return(x);
end:

random_S2_point := () -> hopf_map(random_S3_point());

######################################################################
# R2_zone(x,y) returns an integer between 0 and 16 depending on the
# argument theta of (x,y).  If theta = k * Pi/4 then the result is 
# 2k+1.  If k * Pi/4 < theta < (k+1) * Pi/4 then the result is 2k+2.
# If (x,y) = (0,0) then theta is undefined and the result is 0.
#
# We also define C_zone(x + I*y) = R2_zone(x,y).

#@ R2_zone 
R2_zone := proc(x0,y0)
 local x,y;
 x := evalf(x0);
 y := evalf(y0);
 if x = 0 and y = 0 then
  return(0);
 elif y = 0 and 0 < x then
  return(1);
 elif 0 < y and y < x then
  return(2);
 elif 0 < x and x = y then
  return(3);
 elif 0 < x and x < y then
  return(4);
 elif 0 = x and 0 < y then
  return(5);
 elif 0 < -x and -x < y then
  return(6);
 elif 0 < -x and -x = y then
  return(7);
 elif 0 < y and y < -x then
  return(8);
 elif 0 = y and 0 < -x then
  return(9);
 elif 0 < -y and -y < -x then
  return(10);
 elif 0 < -y and -y = -x then
  return(11);
 elif 0 < -x and -x < -y then
  return(12);
 elif 0 = -x and 0 < -y then
  return(13);
 elif 0 < x and x < -y then
  return(14);
 elif 0 < x and x = -y then
  return(15)
 elif 0 < -y and -y < x then
  return(16);
 else
  return(FAIL);
 fi;
end: 

#@ C_zone 
C_zone := (z::CC) -> R2_zone(Re(z),Im(z)):

######################################################################
# Winding numbers

# This calculates the winding number of a map u : [0,2*Pi] -> C\{0}.
# The second parameter n_ defaults to 24 if it is omitted.  The
# result may be incorrect if n_ is too small, but 24 is ample for
# the functions that occur in practice in this project.

#@ C_winding_number 
C_winding_number := proc(u,n_)
 local n,uu,a;

 n := `if`(nargs > 1,n_,24);
 uu := [seq(evalf(u(2*Pi*i/n)),i=0..(n-1))];
 uu := [op(uu),uu[1]];
 a := add(argument(uu[i+1]/uu[i]),i=1..n);
 return round(evalf(a/(2*Pi)));
end:

#@ R2_winding_number 
R2_winding_number := proc(u,n_)
 C_winding_number((t) -> (u(t)[1] + u(t)[2]*I),args[2..-1]);
end:

######################################################################
# Miscellaneous other things

# rational_sphere point defines a map Q x Q -> S^2 n Q^4 with dense image.

#@ rational_sphere_point 
rational_sphere_point := (s::RR,t::RR) -> [
 2*s/(1+s^2)*2*t/(1+t^2),
 2*s/(1+s^2)*(1-t^2)/(1+t^2),
 (1-s^2)/(1+s^2)
];

# Midpoints of line segments

midpoint_R2 := (a::RR_2,b::RR_2) -> [(a[1]+b[1])/2,(a[2]+b[2])/2]; #@ midpoint_R2 
midpoint_R3 := (a::RR_3,b::RR_3) -> [(a[1]+b[1])/2,(a[2]+b[2])/2,(a[3]+b[3])/2]; #@ midpoint_R3 
midpoint_R4 := (a::RR_4,b::RR_4) -> [(a[1]+b[1])/2,(a[2]+b[2])/2,(a[3]+b[3])/2,(a[4]+b[4])/2]; #@ midpoint_R4 

# Midpoints of arcs in the spheres S^2 and S^3

#@ midpoint_S2 
midpoint_S2 := proc(a::RR_3,b::RR_3)
 local c,r;
 c := midpoint_R3(a,b);
 r := sqrt(c[1]^2+c[2]^2+c[3]^2);
 c := [c[1]/r,c[2]/r,c[3]/r];
 return(c);
end:
 
#@ midpoint_S3 
midpoint_S3 := proc(a::RR_4,b::RR_4)
 local c,r;
 c := midpoint_R4(a,b);
 r := sqrt(c[1]^2+c[2]^2+c[3]^2+c[4]^2);
 c := [c[1]/r,c[2]/r,c[3]/r,c[4]/r];
 return(c);
end:
 
# The area of a flat triangle with vertices in S^3

#@ triangle_area 
triangle_area := proc(a::RR_4,b::RR_4,c::RR_4)
 local p,q;
 p := [seq(b[i]-a[i],i=1..4)];
 q := [seq(c[i]-a[i],i=1..4)];
 sqrt(expand(dp4(p,p)*dp4(q,q)-dp4(p,q)^2))/2;
end:


# xx is a generic vector in R^4
xx := [x[1],x[2],x[3],x[4]]; #@ xx 
yy := [y[1],y[2]];           #@ yy 
zz := [z[1],z[2]];           #@ zz 
tt := [t[1],t[2]];           #@ tt 

assume(x[1]::real);
assume(x[2]::real);
assume(x[3]::real);
assume(x[4]::real);

assume(y[1]::real);
assume(y[2]::real);

protect('x','xx','y','yy','z','zz','t','tt');

