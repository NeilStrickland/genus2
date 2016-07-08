ss := [seq(s[i],i=1..5)];

q_Ep := (x) -> 2*(x-1)*(x-(1/a_P+a_P)/2)*(x+(1/a_P+a_P)/2);
q_Em := (x) -> 2*(x-I)*(x-(1/a_P-a_P)/2)*(x+(1/a_P-a_P)/2);

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
 
Ep_rel := (t) -> t[1]^2*t[3] - 2*(t[2]-t[3])*(t[2]+(a_P+1/a_P)/2*t[3])*(t[2]-(a_P+1/a_P)/2*t[3]);
Em_rel := (t) -> t[1]^2*t[3] - 2*(t[2]+I*t[3])*(t[2]+(a_P-1/a_P)/2*t[3])*(t[2]-(a_P-1/a_P)/2*t[3]);

j_CP2 := (yx) -> [op(yx),1];
j_inv_CP2 := (yxz) -> [yxz[1]/yxz[3],yxz[2]/yxz[3]];
 
phi_P_Ep := (s) -> [(s[3]-s[2])*s[1],(s[3]+s[5])*s[2],2*s[3]^2];
phi_P_Ep_alt := (s) -> [
 s[5]^2 + A_P*s[4]*(s[3]-s[4]) + s[3]*(s[3]-s[2]) - s[4]*s[5],
 s[1]*(s[3]+s[5]),
 2*s[1]*s[4]
];

phi_P_0_Ep0 := proc(wz)
 local w,z;
 w,z := op(wz);
 return([w*(z-1)/(2*z^2),(1/z+z)/2]);
end:

phi_P_0_Em0 := proc(wz)
 local w,z;
 w,z := op(wz);
 return([w*(I*z-1)/(2*z^2),(1/z-z)/2]);
end:

mm_E := (1 - a_P)/sqrt(2*(1 + a_P^2));
mp_E := (1 + a_P)/sqrt(2*(1 + a_P^2));
n_E  := (1/a_P + a_P)/2;

r_E := (EllipticK(mp_E) + EllipticK(mm_E))/sqrt(n_E);
s_E := (EllipticK(mp_E) - EllipticK(mm_E))/sqrt(n_E);

d_Ep[0] := (t) -> [(1+a_P)^2*sin(t)*sqrt((1-a_P)^2/a_P + (1+a_P)^2/a_P*sin(t/2)^2)/(4*a_P),
                 (1/a_P)*((a_P+1)^2/4*cos(t)-(1-a_P)^2/4)];

d_Ep[1] := (t) -> [I*(1-a_P)^2*sin(t)*sqrt((1+a_P)^2/a_P+(1-a_P)^2/a_P*sin(t/2)^2)/(4*a_P),
                 (1/a_P)*((1+a_P)^2/4-(1-a_P)^2/4*cos(t))];


