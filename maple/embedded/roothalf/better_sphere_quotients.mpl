# Our primary approach to understanding the canonical map p : EX^* -> S^2 goes 
# via hyperbolic uniformisation using the E_to_S_map class.  The code in this 
# file represents an attempt to understand p, or maps similar to p, by more
# direct and elementary means.  The file sphere_quotients.mpl defines a map
# E_to_S2 : EX^* -> S^2 which has the expected equivariance and which induces
# a homeomorphism EX^*/<1,LL> -> S^2, but it fails quite badly to be conformal.
# Here we study maps which may be slightly better.

######################################################################

# The following function checks whether a map S : EX^* -> R^3 is equivariant,
# sends v[0], v[3] and v[6] to the right places, and is tangent to S^2
# at those points.

#@ map_to_S2_basic_test 
map_to_S2_basic_test := proc(S)
 local i,m,err;

 if evalf(simplify(S(act_E[L](x)) -~ act_S2[L](S(x)))) <> [0. $3] then return false; fi;
 if evalf(simplify(S(act_E[M](x)) -~ act_S2[M](S(x)))) <> [0. $3] then return false; fi;
 if evalf(simplify(S(act_E[N](x)) -~ act_S2[N](S(x)))) <> [0. $3] then return false; fi;

 if evalf(S(v_E0[3]) -~ v_S2[3]) <> [0. $3] then return false; fi;
 if evalf(S(v_E0[6]) -~ v_S2[6]) <> [0. $3] then return false; fi;

 for i in {0,3,6} do
  if evalf(simplify(S(v_E0[i]) -~ v_S2[i])) <> [0. $3] then return false; fi;
  m := `if`(i = 0,3,2);
  err := evalf(simplify(convert(series(nm3(S(chart[0](e*s,e*t)))^2-1,e=0,m),polynom,e)));
  if err <> 0. then return false; fi;
 od;

 true;
end:

# The following functions check whether a map S behaves conformally to the
# lowest relevant order at v[0], v[3], v[6] or v[11].  Each function returns
# a list of terms which should be zero.

#@ v0_conformal_test 
v0_conformal_test := proc(S)
 local aa,bb,cc,yy,zz,e,s,t;
 yy := map(series,S(chart[0](e*s,e*t)),e=0,3);
 yy := map(convert,yy,polynom,e);
 zz := S2_to_C(yy);
 zz := series(zz,e=0,3);
 zz := subs(e=1,convert(zz,polynom,e));
 aa := coeff(zz,s,2);
 bb := coeff(coeff(zz,s,1),t,1);
 cc := coeff(zz,t,2);
 expand([aa+cc,(bb-2*I*aa)/I]);
end:

#@ v3_conformal_test 
v3_conformal_test := proc(S)
 local aa,bb,yy,zz,e,s,t;
 yy := map(series,S(chart[3](e*s,e*t)),e=0,2);
 yy := map(convert,yy,polynom,e);
 zz := S2_to_C(yy);
 zz := series(zz,e=0,2);
 zz := subs(e=1,convert(zz,polynom,e));
 aa := coeff(zz,s,1);
 bb := coeff(zz,t,1);
 expand(bb - I*aa);
end:

#@ v6_conformal_test 
v6_conformal_test := proc(S)
 local aa,bb,yy,zz,e,s,t;
 yy := map(series,S(chart[6](e*s,e*t)),e=0,2);
 yy := map(convert,yy,polynom,e);
 zz := S2_to_C(yy);
 zz := series(zz,e=0,2);
 zz := subs(e=1,convert(zz,polynom,e));
 aa := coeff(zz,s,1);
 bb := coeff(zz,t,1);
 expand((bb - I*aa)*(1+I)/sqrt(2));
end:

#@ v11_conformal_test 
v11_conformal_test := proc(S)
 local aa,bb,cc,dd,yy,e,s,t;
 yy := map(series,S(chart[11](e*s,e*t)),e=0,2);
 yy := map(convert,yy,polynom,e);
 aa := subs(e=0,yy);
 if map(coeff,yy,e,1) <> [0$3] then return FAIL; fi;
 yy := subs(e = 1,yy -~ aa);
 bb := map(coeff,yy,s,2);
 cc := map(coeff,map(coeff,yy,s,1),t,1);
 dd := map(coeff,yy,t,2);
 expand([op(bb+~dd),dp3(aa,bb),dp3(aa,cc),dp3(bb,cc),dp3(aa,aa)-1]);
end:

# One can show that the map must be as follows, for some undetermined
# functions Pi and Qj.

#@ s2p_core 
s2p_core := [
  (x[2]^2-x[1]^2-(3/sqrt(2))*x[3]*x[4]),
  2*x[1]*x[2],
  -x[3]
]:

#@ s2p_multipliers 
s2p_multipliers := [
 (1+z[1]*P1(z[1])+(z[2]-1/2)*Q1(z[1],z[2])),
 (1+z[1]*P2(z[1])+z[2]*Q2(z[1],z[2])),
 (3/2-z[1]/2+(z[1]-1)^2*P3(z[1])+z[2]*Q3(z[1],z[2]))
]:

#@ s2p_pattern 
s2p_pattern := s2p_core *~ s2p_multipliers;

#@ s2p_generic 
s2p_generic := unapply(eval(subs(z=zx0,s2p_pattern)),x);

# Taking the functions Pi and Qj to be affine, we get the following pattern:

#@ s2p_poly_multipliers 
s2p_poly_multipliers := subs({
 P1(z[1]) = C[0] + C[1]*z[1],
 P2(z[1]) = C[2] + C[3]*z[1],
 P3(z[1]) = C[4] + C[5]*z[1],
 Q1(z[1],z[2]) = C[6] + C[7]*z[1] + C[8]*z[2],
 Q2(z[1],z[2]) = C[9] + C[10]*z[1] + C[11]*z[2],
 Q3(z[1],z[2]) = C[12] + C[13]*z[1] + C[14]*z[2]
},s2p_multipliers
):

#@ s2p_poly_pattern 
s2p_poly_pattern := s2p_core *~ s2p_poly_multipliers;

#@ s2p_poly 
s2p_poly := unapply(eval(subs(z=zx0,s2p_poly_pattern)),x):

#@ s2p_conformal_poly_rels 
s2p_conformal_poly_rels := {
 C[6]=1/2-C[4],
 C[7]=-C[2]-C[3]+1/2+2*C[0]+2*C[1]+C[4],
 C[14] = -(1/3*(2*sqrt(6)*C[4]+sqrt(6)*C[12]+3*sqrt(6)-6*C[9]-3*C[11]-12))*sqrt(6)
};

#@ s2p_conformal_poly_multipliers 
s2p_conformal_poly_multipliers := 
 simplify(expand(subs(s2p_conformal_poly_rels,s2p_poly_multipliers)));

#@ s2p_conformal_poly_pattern 
s2p_conformal_poly_pattern := s2p_core *~ s2p_conformal_poly_multipliers;

#@ s2p_conformal_poly 
s2p_conformal_poly := unapply(eval(subs(z=zx0,s2p_conformal_poly_pattern)),x):

# Here are some numerical choices for the coefficients C[i], which
# give a reasonable map.

#@ s2p_approx 
s2p_approx := {
 C[0] = -1.6723,C[1] = 0.80475,C[2] = -1.3774,C[3] = 1.0570,C[4] = 0.60044,
 C[5] = -0.35384,C[8] = -0.49899,C[9] = -0.41948,C[10] = 0.28319,
 C[11] = -0.037759,C[12] = -0.53453,C[13] = 0.53090
}:

#@ s2p0_multipliers 
s2p0_multipliers := evalf(subs(s2p_approx,s2p_conformal_poly_multipliers));

#@ s2p0_pattern 
s2p0_pattern := evalf(s2p_core *~ s2p0_multipliers);

#@ s2p0 
s2p0 := unapply(eval(subs(z=zx0,s2p0_pattern)),x):

# The following code calculates the Dirichlet energy of a map EX^* -> S^2,
# as a map EX^* -> R.  This then needs to be integrated.

#@ find_E_to_S_energy_coeff
#@ E_to_S_energy_denom
#@ E_to_S_energy_coeff
#@ E_to_S_energy_coeff_string

find_E_to_S_energy_coeff := proc()
 global E_to_S_energy_denom,E_to_S_energy_coeff,E_to_S_energy_coeff_string;
 local i,j,k,s,u_vars,U,UU,PP,nn,sndg,E0,E1,E2,En,En_alt,DP;

 UU := [seq(U[k](zx0[1],zx0[2]),k=1..3)];
 PP := UU *~ s2p_core;
 u_vars := [u[1],u[2],u[3],du[1,1],du[1,2],du[2,1],du[2,2],du[3,1],du[3,3]];
 nn := dg0(xx);
 sndg := square_norm_of_dg_z0(z);

 for k from 1 to 3 do 
  for i from 1 to 4 do 
   DP[k,i] := subs({U[1](x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = u[1],
		    U[2](x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = u[2],
		    U[3](x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = u[3],
		    (D[1](U[1]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[1,1],
		    (D[2](U[1]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[1,2],
		    (D[1](U[2]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[2,1],
		    (D[2](U[2]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[2,2],
		    (D[1](U[3]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[3,1],
		    (D[2](U[3]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[3,2]
		   },diff(PP[k],x[i])); 
  od;
 od;

 E0 := collect(NF_z0(add(add(DP[k,i]^2,i=1..4),k=1..3)),u_vars);
 E1 := collect(NF_z0(add(add(add(DP[k,i]*DP[k,j]*x[i]*x[j],j=1..4),i=1..4),k=1..3)),u_vars);
 E2 := collect(NF_z0(add(add(add(DP[k,i]*DP[k,j]*nn[i]*nn[j],j=1..4),i=1..4),k=1..3)),u_vars);
 En := collect(expand(sndg*(E0-E1) - E2),u_vars); 

 E_to_S_energy_denom := sndg;
 E_to_S_energy_coeff := table():

 for k from 1 to 3 do
  E_to_S_energy_coeff[k] := factor(coeff(En,u[k],2));
  for i from 1 to 2 do
   E_to_S_energy_coeff[k,i] := factor(coeff(coeff(En,u[k],1),du[k,i],1));
   for j from 1 to 2 do
    if i <> j then 
     E_to_S_energy_coeff[k,i,j] := factor(coeff(coeff(En,du[k,i],1),du[k,j],1));
    else
     E_to_S_energy_coeff[k,i,j] := factor(coeff(En,du[k,i],2));
    fi;
   od;
  od;
 od:

 En_alt := 
  add(E_to_S_energy_coeff[k] * u[k]^2,k=1..3) + 
  add(add(E_to_S_energy_coeff[k,i] * u[k] * du[k,i],i=1..2),k=1..3) +
  add(add(E_to_S_energy_coeff[k,i,i] * du[k,i]^2,i=1..2),k=1..3) +
  add(E_to_S_energy_coeff[k,1,2] * du[k,1] * du[k,2],k=1..3);

 print(["check",expand(En - En_alt)]);

 s := sprintf("E_to_S_energy_denom := %A;\n",sndg):
 for k from 1 to 3 do 
  s := cat(s,sprintf("E_to_S_energy_coeff[%d]     := %A;\n",k,E_to_S_energy_coeff[k]));
 od:
 for k from 1 to 3 do 
  for i from 1 to 2 do
   s := cat(s,sprintf("E_to_S_energy_coeff[%d,%d]   := %A;\n",k,i,E_to_S_energy_coeff[k,i]));
  od;
 od:
 for k from 1 to 3 do 
  for i from 1 to 2 do
   for j from i to 2 do
    s := cat(s,sprintf("E_to_S_energy_coeff[%d,%d,%d] := %A;\n",k,i,j,E_to_S_energy_coeff[k,i,j]));
   od;
  od;
 od:
 E_to_S_energy_coeff_string := s;
 NULL;

end:

E_to_S_energy_denom := (1+z[2])*(z[1]^2-4*z[1]+4);
E_to_S_energy_coeff[1]     := -8*(2*z[2]-1)*(z[1]*z[2]-2*z[1]+2*z[2]+2);
E_to_S_energy_coeff[2]     := -4*z[1]^4*z[2]^3+6*z[1]^4*z[2]^2-48*z[1]^3*z[2]^3+6*z[1]^4*z[2]+116*z[1]^3*z[2]^2-16*z[1]^2*z[2]^3-4*z[1]^4-104*z[1]^3*z[2]-144*z[1]^2*z[2]^2+20*z[1]^3+224*z[1]^2*z[2]+16*z[1]*z[2]^2-32*z[1]^2-160*z[1]*z[2]+32*z[2]^2+16*z[1]+32*z[2];
E_to_S_energy_coeff[3]     := -(z[1]-2)^2*(z[1]*z[2]+z[1]-1);
E_to_S_energy_coeff[1,1]   := -16*z[1]*z[2]*(2*z[2]-1)*(z[1]-2);
E_to_S_energy_coeff[1,2]   := -32*z[2]*(2*z[2]-1)*(z[1]*z[2]-2*z[1]+2*z[2]+2);
E_to_S_energy_coeff[2,1]   := -4*z[1]*(2*z[2]-1)*(z[1]-2)*(z[1]*z[2]+z[1]+2*z[2]-2)*(z[1]^2*z[2]-2*z[1]^2+4*z[1]-2);
E_to_S_energy_coeff[2,2]   := -16*z[2]*(2*z[2]-1)*(3*z[1]*z[2]-3*z[1]+2*z[2]+2)*(z[1]^2*z[2]-2*z[1]^2+4*z[1]-2);
E_to_S_energy_coeff[3,1]   := -4*z[1]*(z[1]-2)^2*(z[1]*z[2]+z[1]-1);
E_to_S_energy_coeff[3,2]   := -8*z[1]*z[2]*(2*z[2]-1)*(z[1]-2);
E_to_S_energy_coeff[1,1,1] := -8*z[1]*z[2]*(z[1]-2)^2*(z[1]*z[2]+z[1]-1);
E_to_S_energy_coeff[1,1,2] := -32*z[1]*z[2]^2*(2*z[2]-1)*(z[1]-2);
E_to_S_energy_coeff[1,2,2] := -32*z[2]^2*(2*z[2]-1)*(z[1]*z[2]-2*z[1]+2*z[2]+2);
E_to_S_energy_coeff[2,1,1] := -2*z[1]*(2*z[2]-1)*(z[1]-2)^2*(z[1]*z[2]+z[1]-1)*(z[1]^2*z[2]-2*z[1]^2+4*z[1]-2);
E_to_S_energy_coeff[2,1,2] := -8*z[1]*z[2]*(2*z[2]-1)^2*(z[1]-2)*(z[1]^2*z[2]-2*z[1]^2+4*z[1]-2);
E_to_S_energy_coeff[2,2,2] := -8*z[2]*(2*z[2]-1)^2*(z[1]*z[2]-2*z[1]+2*z[2]+2)*(z[1]^2*z[2]-2*z[1]^2+4*z[1]-2);
E_to_S_energy_coeff[3,1,1] := -4*z[1]^2*(z[1]-2)^2*(z[1]*z[2]+z[1]-1);
E_to_S_energy_coeff[3,1,2] := -16*z[1]^2*z[2]*(2*z[2]-1)*(z[1]-2);
E_to_S_energy_coeff[3,2,2] := -16*z[1]*z[2]*(2*z[2]-1)*(z[1]*z[2]-2*z[1]+2*z[2]+2);

# For the function below, u should be a list of three polynomials in z[1] and z[2].
# This corresponds to the map u *~ s2p_core.

#@ E_to_S_energy 
E_to_S_energy := proc(u)
 local i,k,du,En,E;

 for k from 1 to 3 do
  for i from 1 to 2 do 
   du[k,i] := diff(u[k],z[i]);
  od;
 od;

 En := 
  add(E_to_S_energy_coeff[k] * u[k]^2,k=1..3) + 
  add(add(E_to_S_energy_coeff[k,i] * u[k] * du[k,i],i=1..2),k=1..3) +
  add(add(E_to_S_energy_coeff[k,i,i] * du[k,i]^2,i=1..2),k=1..3) +
  add(E_to_S_energy_coeff[k,1,2] * du[k,1] * du[k,2],k=1..3);

 E := En / E_to_S_energy_denom;

 return unapply(E,z);
end;


######################################################################

#@ find_E_to_S_jacobian_coeff 
#@ E_to_S_jacobian_coeff
#@ E_to_S_jacobian_coeff_string

find_E_to_S_jacobian_coeff := proc()
 global E_to_S_jacobian_coeff,E_to_S_jacobian_coeff_string;
 local i,j,k,l,s,U,UU,PP,DP,S3,S4,sg3,sg4,A,B,AB;

 UU := [seq(U[k](zx0[1],zx0[2]),k=1..3)];
 PP := UU *~ s2p_core;

 for k from 1 to 3 do 
  for i from 1 to 4 do 
   DP[k,i] := subs({U[1](x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = u[1],
		    U[2](x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = u[2],
		    U[3](x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = u[3],
		    (D[1](U[1]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[1,1],
		    (D[2](U[1]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[1,2],
		    (D[1](U[2]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[2,1],
		    (D[2](U[2]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[2,2],
		    (D[1](U[3]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[3,1],
		    (D[2](U[3]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[3,2]
		   },diff(PP[k],x[i])); 
  od;
 od;

 PP := [seq(u[i] * s2p_core[i],i=1..3)];

 S3 := combinat[permute](3):
 S4 := combinat[permute](4):
 sg3 := (u) -> signum(mul(mul(u[j]-u[i],j=i+1..3),i=1..2)):
 sg4 := (u) -> signum(mul(mul(u[j]-u[i],j=i+1..4),i=1..3)):

 A := Matrix(4,4):
 for i from 1 to 4 do 
  for j from 1 to 4 do
   A[i,j] := Determinant(Matrix([[seq(PP[k],k=1..3)],[seq(DP[k,i],k=1..3)],[seq(DP[k,j],k=1..3)]]));
  od:
 od:

 B := Matrix(4,4):
 for s in S4 do
  B[s[1],s[2]] := B[s[1],s[2]] + sg4(s) * dg0(xx)[s[3]] * x[s[4]];
 od:

 AB := expand(NF_z0(add(add(A[i,j] * B[i,j]/2,j=1..4),i=1..4))):

 E_to_S_jacobian_coeff := table():

 E_to_S_jacobian_coeff[0] := factor(coeff(coeff(coeff(AB,u[1],1),u[2],1),u[3],1));
 E_to_S_jacobian_coeff[1] := factor(coeff(coeff(coeff(AB,u[1],1),du[2,1],1),du[3,2],1)/2);

 for i from 1 to 3 do
  j,k := op({1,2,3} minus {i});
  for l from 1 to 2 do
   E_to_S_jacobian_coeff[i,l] :=
    factor(coeff(coeff(coeff(AB,u[j],1),u[k],1),du[i,l],1)/2);
  od;
 od;
end:

E_to_S_jacobian_coeff[0]   :=  (-12*z[1]^2*z[2]^2+20*z[1]^2*z[2]-4*z[1]^2-16*z[1]*z[2]+4);
E_to_S_jacobian_coeff[1]   :=  4*z[1]*z[2]*(2*z[2]-1)*(z[1]^2*z[2]-2*z[1]^2+4*z[1]-2);
E_to_S_jacobian_coeff[1,1] :=  2*z[1]*z[2]*(4*z[1]^2*z[2]-5*z[1]^2+8*z[1]-4);
E_to_S_jacobian_coeff[1,2] := -2*z[2]*(2*z[2]-1)*(z[1]^2*z[2]-2*z[1]^2+2);
E_to_S_jacobian_coeff[2,1] := -2*z[1]*(2*z[2]-1)*(z[1]^2*z[2]-2*z[1]^2+4*z[1]-2);
E_to_S_jacobian_coeff[2,2] := -2*z[2]*(2*z[2]-1)*(z[1]^2*z[2]-2*z[1]^2+4*z[1]-2);
E_to_S_jacobian_coeff[3,1] := -4*z[1]*(z[1]*z[2]-z[1]+1)*(z[1]*z[2]+z[1]-1);
E_to_S_jacobian_coeff[3,2] :=  4*z[1]*z[2]*(2*z[2]-1)*(z[1]*z[2]-2*z[1]+2);

#@ E_to_S_jacobian 
E_to_S_jacobian := proc(u)
 local i,k,du,s,S3,sg3,J;

 S3 := combinat[permute](3):
 sg3 := (u) -> signum(mul(mul(u[j]-u[i],j=i+1..3),i=1..2)):

 for k from 1 to 3 do
  for i from 1 to 2 do 
   du[k,i] := diff(u[k],z[i]);
  od;
 od;

 J := 
  E_to_S_jacobian_coeff[0] * u[1]*u[2]*u[3] +
  E_to_S_jacobian_coeff[1] *
    add(sg3(s)*u[s[1]]*(du[s[2],1]*du[s[3],2]-du[s[2],2]*du[s[3],1]),s in S3) +
  (add(add(u[s[1]] * u[s[2]] * du[s[3],k] * 
             E_to_S_jacobian_coeff[s[3],k],k=1..2),s in S3));

 J := J / sqrt(square_norm_of_dg_z0(z));
 return unapply(J,z)
end:

#@ E_to_S_conformal_error 
E_to_S_conformal_error := proc(u)
 local E,J;
 E := E_to_S_energy(u)(z);
 J := E_to_S_jacobian(u)(z);
 return unapply(E - 2*J,z);
end:

#@ s2p_plot 
s2p_plot := proc(S,g)
 display(seq(polygon(evalf(map(s2p0,map(p -> p["x"],g["faces"][i]["corner"])))),i=0..g["num_faces"]-1),
         scaling=constrained,axes=none);
end:

# Let u be a positive smooth function of z[1] with u(1) = 1.  Then the following
# multipliers give a map that is normalised and conformal on a first-order 
# infinitesimal neighbourhood of C_1, with the correct behavious at the endpoints.

#@ p1_multipliers 
p1_multipliers := 
 [-(1-(1/2)*z[1])*(2*z[1]*(D(u))(z[1])-u(z[1]))/(u(z[1])^2+z[1]),
  (u(z[1])^2-z[1])/((u(z[1])^2+z[1])*(1-z[1])),
  2*u(z[1])/(u(z[1])^2+z[1])
 ]:

#@ s2p_p1 
s2p_p1 := unapply(subs({z[1]=zx0[1],z[2]=zx0[2]},p1_multipliers *~ s2p_core),x);

# The following multipliers do the same for C_3 and C_5. 

#@ p3_multipliers 
p3_multipliers := 
 [(u(z[1])^2-z[1])/(u(z[1])^2+z[1]),
  -(1/3)*(2*z[1]*(D(u))(z[1])-u(z[1]))*sqrt(6)/(u(z[1])^2+z[1]),
  2*u(z[1])/(u(z[1])^2+z[1])
 ]:

#@ s2p_p3 
s2p_p3 := unapply(subs({z[1]=zx0[1],z[2]=zx0[2]},p3_multipliers *~ s2p_core),x);

#@ p5_multipliers 
p5_multipliers := 
 [(1/2)*(u(z[1])^2-z[1])*z[1]/((u(z[1])^2+z[1])*(1-z[1])),
  z[1]*(-2*(D(u)(z[1]))*z[1]+u(z[1]))/(sqrt(3*z[1]^2-4*z[1]+2)*(u(z[1])^2+z[1])),
  2*u(z[1])/(u(z[1])^2+z[1])
 ]:

#@ s2p_p5 
s2p_p5 := unapply(subs({z[1]=zx0[1],z[2]=zx0[2]},p5_multipliers *~ s2p_core),x);

# For C_0 we have not found a general system of multiplers depending on a function
# u.  We just have the following choice, with no free parameters.

#@ p0_multipliers 
p0_multipliers := [1,1,2*sqrt(1+z[2])];

#@ s2p_p0 
s2p_p0 := unapply(subs({z[1]=zx0[1],z[2]=zx0[2]},p0_multipliers *~ s2p_core),x);

######################################################################
# Here we make some specific choices of multipliers along the edges.
# With these choices we get maps f[i] : C_i -> S^2 that are conformal to
# first order and agree to first order at the corners.  At v[0] and v[11]
# the maps f[1] and f[5] are conformal to second order and agree to
# second order.
#
# It should be possible in principle to find multipliers on the
# whole surface that agree on the curves C_i with the multipliers 
# below, but we have not yet completed this.  Note that p[0], p[1] and
# p[3] are the same, which is a start.
#
# With these multipliers, the point v[11] maps in C to 
# 5 - 2*sqrt(6) = 0.101018, which is close to the value of a_P
# obtained by the numerical code.

#@ s2p_pp
#@ s2p_qq
 
s2p_qq := map(u -> NF_z0(u^2),s2p_core);
s2p_pp[0] := [ 1-1/2*z[1]-3/4*z[1]*z[2]+1/2*z[1]*z[2]^2,
               1/sqrt(1+3/2*z[1]*z[2]),
               2*sqrt(1+z[2])*sqrt(1+3/2*z[1]*z[2])
             ]/~(1+z[1]+5/2*z[1]*z[2]);
s2p_pp[1] := s2p_pp[0];
s2p_pp[3] := s2p_pp[0];

s2p_pp[5] := [(5/2-(3/4)*z[1]^2*z[2]+(3/2)*z[1]^2-3*z[1])/(2*(1+z[1]+(5/2)*z[1]*z[2])),
              (6-4*z[1]- 2*z[2]+2*z[1]*z[2])/((1+z[1]+(5/2)*z[1]*z[2])*(1+z[2])*sqrt(6*z[1]*z[2]+4)),
              2*sqrt(1+z[2])*sqrt(1+3/2*z[1]*z[2])/(1+z[1]+5/2*z[1]*z[2])];

## s2p_ff

for i in {0,1,3,5} do
 s2p_ff[i] := unapply(eval(subs(z = zx0,s2p_pp[i])) *~ s2p_core,x);
od:

######################################################################

# Suppose we have a map f with multipliers m1,m2,m3.  Then m3 does not
# affect the values of f on C_0, but there is a unique possible value
# of m3 on C_0 that ensures that f is conformal on a first order 
# infinitesimal neighbourhood of C_0.  This m3 is given by 
# p0_conformal_factor([m1,m2,m3]), with the understanding that the
# functions mi have been restricted to C_0 and expressed as functions
# of z[1].  The story is similar for C_1, C_3 and C_5.  Note however
# that z[2] is not constant on C_5, so taking the derivative of the
# restricted function with respect to z[1] is not the same as taking
# the partial derivative and then restricting.

#@ p0_conformal_factor 
p0_conformal_factor := (m) -> 
 sqrt(1+z[2])*(2*m[1](z[2])*m[2](z[2])+
               8*z[2]*(1/2-z[2])*(D(m[1])(z[2])*m[2](z[2])-m[1](z[2])*D(m[2])(z[2])));

#@ p1_conformal_factor 
p1_conformal_factor := (m) -> 
 1/4*(2-z[1])*(-(1+z[1])*m[2](z[1])*m[3](z[1]) +
   2*z[1]*(1-z[1])*(D(m[2])(z[1])*m[3](z[1])-D(m[3])(z[1])*m[2](z[1])));

#@ p3_conformal_factor 
p3_conformal_factor := (m) -> 
 (2*z[1]*(D(m[1])(z[1])*m[3](z[1])-D(m[3])(z[1])*m[1](z[1]))-m[1](z[1])*m[3](z[1]))/sqrt(6);

#@ p5_conformal_factor 
p5_conformal_factor := (m) -> 
 (-2*z[1]*(1-z[1])*(m[3](z[1])*D(m[1])(z[1])-D(m[3])(z[1])*m[1](z[1])) + (3-z[1])*m[1](z[1])*m[3](z[1]))/sqrt(3*z[1]^2-4*z[1]+2);

