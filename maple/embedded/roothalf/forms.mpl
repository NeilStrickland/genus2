# These differential forms alpha[i] and beta[j] are as in sec-int-props

#@ alpha_form
alpha_form[1] := y[1]*(x[2]*dx[1] - x[1]*dx[2]);
alpha_form[2] := (z[1]*z[2]+z[1])*y[1]*y[2]*(dx[1]*x[2]+dx[2]*x[1]) +
                 (-2)*z[1]*x[1]*x[2]*y[2]*dx[3]+
                 (2*z[1]*z[2]+2)*x[1]*x[2]*dx[4];

#@ beta_form
beta_form[1] := y[1]*(x[2]*dx[1] - x[1]*dx[2]);
beta_form[2] := y[1]*y[2]*(x[2]*dx[1] + x[1]*dx[2]);
beta_form[3] := x[1]*x[2]*y[2]*dx[3];
beta_form[4] := x[1]*x[2]*dx[4];

#@ theta_form
theta_form[1] := add(diff(g0(x),x[i])*dx[i],i=1..4);
theta_form[2] := add(x[i]*dx[i],i=1..4);

alpha_form[3] := x[1]*x[2]*theta_form[1];
alpha_form[4] := x[1]*x[2]*y[1]*y[2]*theta_form[2];

# This is the matrix P such that alpha[i] = sum(P[i,j] * beta[j],j=1..4)

#@ alpha_beta 
alpha_beta := [
 [1,0,0,0],
 [0,z[1]*(1+z[2]),-2*z[1],2*(1+z[1]*z[2])],
 [sqrt(2)*(1-z[1])*(1-2*z[2]),z[1]*(1-2*z[2]),(z[1] - 2),(-2+3*z[1]-4*z[1]*z[2])],
 [(3*z[1]-2)*z[2]/(2*sqrt(2)),(1 - z[1] - z[1]*z[2])/2,z[1],-z[1]*z[2]]
];

# Norm of dg in terms of z
#@ ndg_z 
ndg_z := sqrt((2 - z[1])^2 * (1 + z[2]));

#@ D_alpha

# This is the Stokes operator D = *d applied to alpha[1]
D_alpha[1] := (9*z[1]^2*z[2]-9*z[1]^2+2*z[1]*z[2]+9*z[1]-2)/ndg_z;

# This is the Stokes operator D = *d applied to alpha[2]
D_alpha[2] := (45*z[1]^3*z[2]^2-45*z[1]^3*z[2]+78*z[1]^2*z[2]-20*z[1]*z[2]^2
                -12*z[1]^2-28*z[1]*z[2]+12*z[1]-8*z[2])/sqrt(2)/ndg_z;

# dz_cross_alpha[i,j] is * (dz[i] wedge alpha[j])
#@ dz_cross_alpha

dz_cross_alpha[1,1] := 2*z[1]*(3*z[1]-2)*(z[1]*z[2]-z[1]+1)/ndg_z;
dz_cross_alpha[1,2] := sqrt(2)*z[1]*
                        (9*z[1]^3*z[2]^2-9*z[1]^3*z[2]-12*z[1]^2*z[2]^2+
                         26*z[1]^2*z[2]+4*z[1]*z[2]^2-4*z[1]^2-24*z[1]*z[2]+
                         8*z[1]+8*z[2]-4) / ndg_z;
dz_cross_alpha[2,1] := 4*z[1]*z[2]*(2*z[2]-1) / ndg_z;
dz_cross_alpha[2,2] := 2*sqrt(2)*(3*z[1]^2*z[2]-2*z[1]*z[2]+2*z[1]-2)*z[2]*(2*z[2]-1) / ndg_z;

#@ xy0
xy0[1] := sqrt(uy0[1]);
xy0[2] := sqrt(uy0[2]);
xy0[3] := y[1];
xy0[4] := -y[1]*y[2];

# Rules for rewriting x[i], z[i], dx[i] and dz[i] in terms of y[j] and dy[j]
 
#@ forms_to_y 
forms_to_y := {
 x[1] = sqrt(uy0[1]),
 x[2] = sqrt(uy0[2]),
 x[3] = y[1],
 x[4] = -y[1]*y[2],
 dx[1] = grad_y(sqrt(uy0[1])),
 dx[2] = grad_y(sqrt(uy0[2])),
 dx[3] = grad_y(y[1]),
 dx[4] = grad_y(-y[1]*y[2]),
 z[1] = y[1]^2,
 z[2] = y[2]^2,
 dz[1] = 2*y[1]*dy[1],
 dz[2] = 2*y[2]*dy[2]
}:

######################################################################
# This is an alternative notation, which we are not really using.

#@ de_rham_d0 
de_rham_d0 := (u) -> add(diff(u,x[i])*dx[i],i=1..4):

#@ de_rham_d1 
de_rham_d1 := proc(u)
 local v,w,i,j;
 for i from 1 to 4 do
  for j from 1 to 4 do
   v[i,j] := diff(coeff(u,dx[i]),x[j]);
  od:
 od:
 w := add(add(v[i,j]*dx[j,i],j=1..i-1),i=1..4) - 
      add(add(v[i,j]*dx[i,j],j=i+1..4),i=1..4);
 return(w);
end:

#@ wedge 
wedge := proc(u,v)
 add(add(coeff(u,dx[i])*coeff(v,dx[j])*dx[i,j],i=1..j-1),j=1..4) - 
 add(add(coeff(u,dx[i])*coeff(v,dx[j])*dx[j,i],i=j+1..4),j=1..4);
end:

######################################################################

#@ unnormalised_stokes 
unnormalised_stokes := proc(u)
 local nn,uu,du,S4,sgn4,s;
 nn := dg0(xx);
 uu := [seq(coeff(u,dx[i],1),i=1..4)];
 du := [seq([seq(diff(uu[j],x[i]),j=1..4)],i=1..4)];
 S4 := combinat[permute](4);
 sgn4 := (s) -> signum(mul(mul(s[j]-s[i],j=i+1..4),i=1..3));
 add(sgn4(s)*du[s[1]][s[2]]*x[s[3]]*nn[s[4]],s in S4);
end:

#@ stokes 
stokes := (u) -> unnormalised_stokes(u)/sqrt(square_norm_of_dg0(xx));

# stokes_alpha([f1,f2]) = 
#  stokes(f1 * alpha_form[1] + f2 * alpha_form[2])
# (where f1 and f2 should be given in terms of z[1] and z[2]).
 
#@ stokes_alpha 
stokes_alpha := proc(ff)
 local i,j;

 add(add(diff(ff[i],z[j]) * dz_cross_alpha[j,i],j=1..2),i=1..2) +
 add(ff[i] * D_alpha[i],i=1..2);
end:
