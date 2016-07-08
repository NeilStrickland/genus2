# The functions y_1 and y_2 generate the ring of invariants on X for <lambda^2,nu>
#@ yx
yx[1] := x[3];
yx[2] := (x[2]^2 - x[1]^2 - (a_E+1/a_E)*x[3]*x[4])/(2*a_E);

# u_i is an expression for x_i^2 in terms of y_1 and y_2
#@ uy
uy[1] := (1-2*a_E*y[2])/2 - (y[2]-a_E)*(y[2]-1/a_E)*y[1]^2/2;
uy[2] := (1+2*a_E*y[2])/2 - (y[2]+a_E)*(y[2]+1/a_E)*y[1]^2/2;

# xy[i] is an expression for x[i] in terms of y[1] and y[2], valid when x[1],x[2] >= 0
#@ xy
xy[1] := sqrt(uy[1]);
xy[2] := sqrt(uy[2]);
xy[3] := y[1];
xy[4] := - y[1] * y[2];

# u_3 is an expression for 4 x_1^2 x_2^2, and u_3 is an expression for x_1^2 + x_2^2
#@ uz
uz[3] := (1 - z[1] - z[1]*z[2])^2 - z[2] * ((a_E+1/a_E)*z[1]-2*a_E)^2;
uz[4] := 1 - z[1] - z[1]*z[2];

#@ uf
uf[1] := unapply(uy[1],y);
uf[2] := unapply(uy[2],y);

# The functions z_1 and z_2 generate the ring of invariants on X for G
#@ zy
zy[1] := y[1]^2;
zy[2] := y[2]^2;

#@ zx
zx[1] := yx[1]^2;
zx[2] := yx[2]^2;

#@ yz
yz[1] := sqrt(z[1]);
yz[2] := sqrt(z[2]);

# Some additional invariant functions
#@ zx
zx[3] := x[1]^2 + x[2]^2;
zx[4] := x[4]^2;
zx[5] := (x[2]^2-x[1]^2)*x[3]*x[4]-(a_E+1/a_E)*x[3]^2*x[4]^2;

# The functions w_1 and w_2 give a homeomorphism X/G -> [0,1]^2
#@ wz
wz[1] := z[1]*(1+(a_E+1/a_E)*sqrt(z[2])+z[2])/(1+2*a_E*sqrt(z[2]));
wz[2] := sqrt(z[2])*(2*a_E*(1-z[1])+z[1]*sqrt(z[2]))/(1-z[1]+(1/a_E-a_E)*z[1]*sqrt(z[2]));

#@ wy
#@ wx
for i from 1 to 2 do 
# wy[i] := subs({z[1]=y[1]^2,z[2]=y[2]^2},subs(sqrt(z[2]) = abs(y[2]),wz[i]));
 wy[i] := subs({z[1]=y[1]^2,z[2]=y[2]^2},wz[i]);
 wx[i] := subs({y[1]=yx[1],y[2]=yx[2]},wy[i]);
od:

# Gradient operators
grad_x := (u) -> add(diff(u,x[i]) * dx[i],i=1..4); #@ grad_x
grad_y := (u) -> add(diff(u,y[i]) * dy[i],i=1..2); #@ grad_y
grad_z := (u) -> add(diff(u,z[i]) * dz[i],i=1..2); #@ grad_z

#@ dyx
#@ dzx
#@ dzy
#@ dyz
for i from 1 to 2 do 
 dyx[i] := grad_x(yx[i]);
 dzx[i] := grad_x(zx[i]);
 dzy[i] := grad_y(zy[i]);
 dyz[i] := grad_z(yz[i]);
od:

#@ dxy
for i from 1 to 4 do
 dxy[i] := grad_y(xy[i]);
od:

#@ dzx
for i from 3 to 5 do # i = 1 to 2 already done 
 dzx[i] := grad_x(zx[i]);
od:

# Projections from EX(a) to the y-plane and the z-plane
y_proj := unapply([seq(yx[i],i=1..2)],x); #@ y_proj
z_proj := unapply([seq(zx[i],i=1..2)],x); #@ z_proj

# Sections of y_proj and z_proj
y_lift := (y) -> [sqrt(uf[1](y)),sqrt(uf[2](y)),y[1],-y[1]*y[2]]; #@ y_lift
z_lift := (z) -> y_lift([sqrt(z[1]),sqrt(z[2])]);                 #@ z_lift

######################################################################
# Various Grobner bases

x_vars := lexdeg([z[1],z[2]],[y[1],y[2]],[x[1],x[2],x[3],x[4]]); #@ x_vars
y_vars := lexdeg([x[1],x[2],x[3],x[4]],[z[1],z[2]],[y[1],y[2]]); #@ y_vars
z_vars := lexdeg([x[1],x[2],x[3],x[4]],[y[1],y[2]],[z[1],z[2]]); #@ z_vars

#@ x_rels 
x_rels := Basis(
 expand([y[1]-yx[1],y[2]-yx[2],
         z[1]-zx[1],z[2]-zx[2],
         rho(xx)-1,g(xx)] *~ a_E^4),
 x_vars
);

#@ y_rels 
y_rels := Basis(
 expand([z[1]-zy[1],z[2]-zy[2],
         y[1]-yx[1],y[2]-yx[2],
         x[1]^2-uy[1],x[2]^2-uy[2],
         x[4]+y[1]*y[2]] *~ a_E^4),
 y_vars
);

#@ z_rels 
z_rels := Basis([op(y_rels),z[1]-zy[1],z[2]-zy[2]],z_vars);

# NF_x(u) rewrites u in terms of x[1],...,x[4]
# NF_y(u) rewrites u as a linear combination of {1,x[1],x[2],x[1]*x[2]}
#          over R[y[1],y[2]].
# NF_z(u) rewrites u as a linear combination of 
#          {x[1]^i*x[2]^j*y[1]^k*y[2]^l | 0 <= i,j,k,l <= 1}
#           over R[z[1],z[2]].

NF_x := (u) -> NormalForm(u,x_rels,x_vars); #@ NF_x
NF_y := (u) -> NormalForm(u,y_rels,y_vars); #@ NF_y
NF_z := (u) -> NormalForm(u,z_rels,z_vars); #@ NF_z

# List of monomials in x[1],..,x[4] that are reduced wrt the Grobner
# basis x_rels.  This relies on the fact that the leading monomials
# for the elements of the Grobner basis are x[4]^2, x[3]^2*x[4] and
# x[3]^4.

#@ x_reduced_basis 
x_reduced_basis := proc(d)
 local i1,i2,i3,i4;
 [seq(seq(seq(seq(x[1]^i1*x[2]^i2*x[3]^i3*x[4]^i4,
			i4=0..min(d-i1-i2-i3,1-floor(i3/2))),
			i3=0..min(d-i1-i2,3)),
			i2=0..d-i1),
			i1=0..d)];
end:

######################################################################

# Action on the ring of polynomial functions on R4

#@ act_A_rule
#@ act_A
#@ R4_matrix

for T in G16 do 
 act_A_rule[T] := {seq(x[i] = act_R4[G_inv(T)](xx)[i],i=1..4)};
 act_A_rule[T] := {op(act_A_rule[T]),op(subs(x=dx,act_A_rule[T]))};
 act_A[T] := (u) -> subs(act_A_rule[T],u);

 act_A_rule[T] := {op(act_A_rule[T]),
  seq(y[i] = factor(act_A[T](yx[i])/yx[i]) * y[i],i = 1..2),
  seq(dy[i] = factor(act_A[T](yx[i])/yx[i]) * dy[i],i = 1..2)
 };

 act_A[T] := (u) -> subs(act_A_rule[T],u);
 R4_matrix[T] := Matrix([seq([seq(coeff(act_R4[T](xx)[i],x[j],1),j=1..4)],i=1..4)]);
od:

# For some reason we had trouble making the definitions below in a loop
act_A[1]     := (u) -> subs(act_A_rule[1],u):
act_A[L]     := (u) -> subs(act_A_rule[L],u):
act_A[LL]    := (u) -> subs(act_A_rule[LL],u):
act_A[LLL]   := (u) -> subs(act_A_rule[LLL],u):
act_A[M]     := (u) -> subs(act_A_rule[M],u):
act_A[LM]    := (u) -> subs(act_A_rule[LM],u):
act_A[LLM]   := (u) -> subs(act_A_rule[LLM],u):
act_A[LLLM]  := (u) -> subs(act_A_rule[LLLM],u):
act_A[N]     := (u) -> subs(act_A_rule[N],u):
act_A[LN]    := (u) -> subs(act_A_rule[LN],u):
act_A[LLN]   := (u) -> subs(act_A_rule[LLN],u):
act_A[LLLN]  := (u) -> subs(act_A_rule[LLLN],u):
act_A[MN]    := (u) -> subs(act_A_rule[MN],u):
act_A[LMN]   := (u) -> subs(act_A_rule[LMN],u):
act_A[LLMN]  := (u) -> subs(act_A_rule[LLMN],u):
act_A[LLLMN] := (u) -> subs(act_A_rule[LLLMN],u):

#@ check_char
check_char := proc(u)
 local Q,i;
 Q := [seq(factor(act_A[T](u)/u),T in G16)];
 for i from 0 to 7 do
  if Q = [seq(character[i][T],T in G16)] then
   return i;
  fi;
 od;
 return FAIL;
end:

#@ G16_average 
G16_average := (u) -> add(act_A[T](u),T in G16)/16;

#@ G16_twisted_average 
G16_twisted_average := (k,u) -> add(character[k][T] * act_A[T](u),T in G16)/16;

# Action of G on the y-plane

#@ act_hex
act_hex[1]    := (y) -> [ y[1], y[2]];
act_hex[L]    := (y) -> [ y[1],-y[2]];
act_hex[LL]   := (y) -> [ y[1], y[2]];
act_hex[LLL]  := (y) -> [ y[1],-y[2]];
act_hex[M]    := (y) -> [-y[1], y[2]];
act_hex[LM]   := (y) -> [-y[1],-y[2]];
act_hex[LLM]  := (y) -> [-y[1], y[2]];
act_hex[LLLM] := (y) -> [-y[1],-y[2]];
act_hex[N]    := (y) -> [ y[1], y[2]];
act_hex[LN]   := (y) -> [ y[1],-y[2]];
act_hex[LLN]  := (y) -> [ y[1], y[2]];
act_hex[LLLN] := (y) -> [ y[1],-y[2]];
act_hex[MN]   := (y) -> [-y[1], y[2]];
act_hex[LMN]  := (y) -> [-y[1],-y[2]];
act_hex[LLMN] := (y) -> [-y[1], y[2]];
act_hex[LLLMN]:= (y) -> [-y[1],-y[2]];

# Invariants for the action twisted by a linear character (as listed in group.mpl)
# These twisted invariants form a module over the ring of untwisted
# invariants, with generators as listed below.

#@ twisted_invariant

twisted_invariant[0] := 1;
twisted_invariant[1] := yx[2];
twisted_invariant[2] := yx[1];
twisted_invariant[3] := yx[1]*yx[2];
twisted_invariant[4] := x[1]*x[2]:
twisted_invariant[5] := x[1]*x[2]*yx[2]:
twisted_invariant[6] := x[1]*x[2]*yx[1]:
twisted_invariant[7] := x[1]*x[2]*yx[1]*yx[2]:

######################################################################

#@ square_norm_of_dg_z 
square_norm_of_dg_z := (z) -> 
 (1+z[2])*(((1/a_E^2-1)*z[1])^2+(2/a_E^2-8)*z[1]+4)+
 (1-z[2])*z[1]*2*(2-1/a_E^2);

#@ curvature_z 
curvature_z := (z) ->
  1 - 16*(1/a_E^2+(3*a_E^2-1)^2/(4*a_E^6)*z[1]^2-4*z[2]
          -(3*a_E^2-1)/a_E^4*z[1]+4*(a_E^(-2)-1)*z[1]*z[2]
          +4*(2-a_E^(-2))*z[1]^2*z[2]^2+(3+4*a_E^(-2)-3*a_E^(-4))*z[1]^2*z[2])/
          square_norm_of_dg_z(z)^2;

######################################################################

#@ z_to_w 
z_to_w := unapply([wz[1],wz[2]],z);

# The map z_to_w involves quotients of functions that are visibly 
# continuous.  One denominator vanishes at the point z = [1,0].
# The function below is the unique continuous extension.

#@ z_to_wa 
z_to_wa := proc(z)
 if z[1] = 1 and z[2] = 0 then return([1,0]) else return(z_to_w(z)); fi;
end:

w_proj := unapply(z_to_w(zx),x):          #@ w_proj
w_proja := (x) -> z_to_wa(z_proj(x)):     #@ w_proj_a

y_to_w := (y) -> z_to_w([y[1]^2,y[2]^2]); #@ y_to_w

# To find y with y |-> w we first solve p_0(y[2])/p_1(y[2]) = w[2] (where p_0 and p_1
# are as defined below), 

#@ w_to_y_p_0 
w_to_y_p_0 := (t,w1) -> t*((1+w1)*t^2+(1/a_E+a_E+(1/2/a_E-2*a_E)*w1)*t+(1-w1));

#@ w_to_y_p_1 
w_to_y_p_1 := (t,w1) -> (1/2/a_E+w1*(1/a_E-a_E))*t^2 + 1/2*((1/a_E^2-1)*(1+w1)+2*(1-w1))*t + 1/2/a_E*(1-w1);

#@ w_to_y_p   
w_to_y_p   := (t,w1) -> w_to_y_p_0(t,w1)/w_to_y_p_1(t,w1);

#@ w_to_y_q   
w_to_y_q   := (t,w1,w2) -> w2 * w_to_y_p_1(t,w1) - w_to_y_p_0(t,w1);

#@ w_to_y_y_1 
w_to_y_y_1 := (w1,y2) -> sqrt(w1*(1+2*a_E*y2)/((y2+1/a_E)*(y2+a_E)));

# Coefficients of t in the numerator of the derivative of w_to_y_p.
#@ w_to_y_dpc[0] 
w_to_y_dpc[0] := (1-w1)^2/(2*a_E);

#@ w_to_y_dpc[1] 
w_to_y_dpc[1] := (1-w1)*((3/2/a_E^2-1)*w1+(1/a_E^2+1)*(1-w1));

#@ w_to_y_dpc[2] 
w_to_y_dpc[2] := (1-a_E^2)/a_E^3*(1-w1)/2 + 
                 (1-a_E^2)*(3/2-a_E^2)/a_E^3*w1^2 +
		 (1+2*a_E^2)*(5-a_E^2)/4/a_E^3*w1*(1-w1) +
		 (5+a_E^2)/2/a_E*(1-w1)^2;

#@ w_to_y_dpc[3] 
w_to_y_dpc[3] := (1+w1)/a_E^2*(2*(1-a_E^2)*w1+(1+a_E^2)*(1-w1));

#@ w_to_y_dpc[4] 
w_to_y_dpc[4] := (1+w1)*(1+2*(1-a_E^2)*w1)/(2*a_E);

#@ w_to_y1 
w_to_y1 := proc(w)
 option remember;
 local p,t,tt,y1,y2;
 p := subs(a_E=a_E1,w_to_y_q(t,w[1],w[2]));
 tt := select(t -> (t^2 <= 1/a_E1-a_E1),[fsolve(p=0,t)]);
 if tt = [] then return [FAIL,tt]; fi;
 y2 := max(0,op(tt));
 y1 := evalf(subs(a_E=a_E1,w_to_y_y_1(w[1],y2)));
 return([y1,y2]);
end:

#@ w_lift1 
w_lift1 := (w) -> map(Re,evalf(subs(a_E=a_E1,y_lift(w_to_y1(w)))));

protect('uf','uz','wy','wz','xy','yx','yz','zx','zy','dxy','dyx','dyz','dzx','dzx','dzy');