# The code below defines a function crease : [-t0,t0] -> X, where 
# t0 = sqrt(3) - sqrt(2).  I think that the union of the images under
# G16 of crease([-t0,t0]) is precisely the set of singular points for
# the map pi, which is also the set of points where 
# x[3] dg(x)[4] - x[4] dg(x)[3] = 0. 

#@ crease 
crease := (t) -> [
 (t-1/sqrt(2))*sqrt(-(t-sqrt(3)-sqrt(2))*(t+sqrt(3)-sqrt(2))/(1-t^2)/3),
 (t+1/sqrt(2))*sqrt(-(t+sqrt(3)+sqrt(2))*(t-sqrt(3)+sqrt(2))/(1-t^2)/3),
 sqrt((1+t^2)/(1-t^2)*2/3),
 -sqrt((1+t^2)/(1-t^2)*2/3)*t
];

#@ crease_trig 
crease_trig := (t) -> [
  -(sqrt(2)-2/sqrt(3))*(1+sqrt(3/2)-cos(2*t))*cos(t)*sqrt(alpha_E-cos(2*t))/sqrt(alpha_E-cos(2*t)^2),
  -(sqrt(2)-2/sqrt(3))*(1+sqrt(3/2)+cos(2*t))*sin(t)*sqrt(alpha_E+cos(2*t))/sqrt(alpha_E-cos(2*t)^2),
    sqrt(2/3)*sqrt(alpha_E+cos(2*t)^2)/sqrt(alpha_E-cos(2*t)^2),
  -(sqrt(2)-2/sqrt(3))*cos(2*t)*sqrt(alpha_E+cos(2*t)^2)/sqrt(alpha_E-cos(2*t)^2)
];

#@ crease_eq 
crease_eq := (x) ->
 x[1]^8+32*x[1]^6*x[2]^2-66*x[1]^4*x[2]^4+32*x[1]^2*x[2]^6+x[2]^8+
 (-6)*x[1]^6-426*x[1]^4*x[2]^2-426*x[1]^2*x[2]^4-6*x[2]^6-35*x[1]^4+
 502*x[1]^2*x[2]^2-35*x[2]^4-36*x[1]^2-36*x[2]^2+4;

#@ crease_eq_polar 
crease_eq_polar := (r,t) ->
 (-cos(8*t)+(1/2)*cos(4*t)+3/2)*r^8+
 (51*cos(4*t)-57)*r^6+
 (73/2-(143/2)*cos(4*t))*r^4-36*r^2+4;

