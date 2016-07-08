assume(a_P > 0 and a_P < 1);

######################################################################

# These are b_- and b_+ in the LaTeX document
bp_P := (1/a_P+a_P)/2; #@ bp_P 
bm_P := (1/a_P-a_P)/2; #@ bm_P 

A_P := a_P^2 + a_P^(-2); #@ A_P 

r_P := (z) -> z^5 - A_P*z^3 + z; #@ r_P 

# These are such that
#  r_P_cofactor0 * r_P(z) + r_P_cofactor1 * diff(r_P(z),z) = 1

#@ r_P_cofactor0 
r_P_cofactor0 := (z) -> ((100-30*A_P^2)*z^3+(18*A_P^3-70*A_P)*z)/(4*(A_P^2-4)); 

#@ r_P_cofactor1
r_P_cofactor1 := (z) -> ((6*A_P^2-20)*z^4-(6*A_P^3-22*A_P)*z^2+(4*A_P^2-16))/(4*(A_P^2-4));

# These relations define PX(a) in CP^4
#@ P_rel
P_rel[0] := (z) -> z[1]^2 - (z[2]*z[3] + z[4]*z[5]) + A_P*z[3]*z[4];
P_rel[1] := (z) -> z[2]*z[4] - z[3]^2;
P_rel[2] := (z) -> z[2]*z[5] - z[3]*z[4];
P_rel[3] := (z) -> z[3]*z[5] - z[4]^2;

# This sets up a Grobner basis for the above relations
P_vars := tdeg(z[1],z[2],z[3],z[4],z[5]);                     #@ P_vars 
P_rels := Basis({seq(P_rel[i]([op(P_vars)]),i=0..3)},P_vars); #@ P_rels 
NF_P := (u) -> NormalForm(u,P_rels,P_vars);                   #@ NF_P 

# This is the inclusion of the affine curve PX_0(a) in PX(a)
j_P  := wz   -> [wz[1],1,wz[2],wz[2]^2,wz[2]^3]: #@ j_P  

# This is inverse to j_P
j_inv_P := (x) -> [x[1]/x[2],x[3]/x[2]]: #@ j_inv_P 

#@ jj_P 
jj_P := proc(uvw) 
 local u,v,w;
 u,v,w := op(uvw);
 return([u,v^3,v^2*w,v*w^2,w^3]);
end:

# A metric on CP^4
#@ d_CP4
d_CP4 := (u,v) -> sqrt(add(add(abs(u[i]*v[j]-u[j]*v[i])^2,j=i+1..5),i=1..4)/
                       (add(abs(u[i])^2,i=1..5)*add(abs(v[i])^2,i=1..5)));

#@ d_P_0
d_P_0 := (u,v) -> d_CP4(j_P(u),j_P(v));

# Two projections from PX(a) to C u {infinity}
p_P  := (z) -> z[3]/z[2]; #@ p_P  
q_P  := (z) -> z[1]/z[2]; #@ q_P  

# pq_P is the same as j_inv_P
pq_P := (z) -> [z[1]/z[2],z[3]/z[2]]; #@ pq_P 

# This function simplifies algebraic expressions by manipulating
# some square roots in a way that is valid for all a_P in (0,1),
# but which is not done automatically by Maple.

#@ simplify_P
simplify_P := proc(u)
 local v;
 v := simplify(u);
 v := simplify(subs(sqrt(1 - a_P^2) = sqrt(1 - a_P)*sqrt(1 + a_P),v));
 v := simplify(subs(sqrt((I*a_P+1)/(I*a_P-1)) = -I*sqrt(I-a_P)/sqrt(I+a_P),v));
 return v;
end:

#@ is_member_P_0
is_member_P_0 := (wz) -> simplify_P(wz[1]^2 - r_P(wz[2])) = 0;

#@ is_member_P
is_member_P := (z) -> simplify_P([seq(P_rel[i](z),i=0..3)]) = [0$4];

#@ is_equal_P_list
is_equal_P_list := (z,w) -> [seq(seq(z[i]*w[j]-z[j]*w[i],j=i+1..5),i=1..4)];

#@ is_equal_P
is_equal_P := proc(z,w)
 local L,u;

 L := is_equal_P_list(z,w);
 for u in L do 
  if simplify_P(conjugate(simplify_P(expand(u)))) <> 0 then
   return(false);
  fi;
 od;
 return(true);
end:

######################################################################

#@ act_P_0
act_P_0[1]     := (wz) -> [   wz[1], wz[2]];
act_P_0[L]     := (wz) -> [ I*wz[1],-wz[2]];
act_P_0[LL]    := (wz) -> [-  wz[1], wz[2]];
act_P_0[LLL]   := (wz) -> [-I*wz[1],-wz[2]];
act_P_0[M]     := (wz) -> [-  wz[1]/wz[2]^3, 1/wz[2]];
act_P_0[LM]    := (wz) -> [-I*wz[1]/wz[2]^3,-1/wz[2]];
act_P_0[LLM]   := (wz) -> [   wz[1]/wz[2]^3, 1/wz[2]];
act_P_0[LLLM]  := (wz) -> [ I*wz[1]/wz[2]^3,-1/wz[2]];
act_P_0[N]     := (wz) -> map(conjugate,[   wz[1], wz[2]]);
act_P_0[LN]    := (wz) -> map(conjugate,[-I*wz[1],-wz[2]]);
act_P_0[LLN]   := (wz) -> map(conjugate,[-  wz[1], wz[2]]);
act_P_0[LLLN]  := (wz) -> map(conjugate,[ I*wz[1],-wz[2]]);
act_P_0[MN]    := (wz) -> map(conjugate,[-  wz[1]/wz[2]^3, 1/wz[2]]);
act_P_0[LMN]   := (wz) -> map(conjugate,[-I*wz[1]/wz[2]^3,-1/wz[2]]);
act_P_0[LLMN]  := (wz) -> map(conjugate,[   wz[1]/wz[2]^3, 1/wz[2]]);
act_P_0[LLLMN] := (wz) -> map(conjugate,[ I*wz[1]/wz[2]^3,-1/wz[2]]);

#@ act_P
act_P[1]     := (z) -> [   z[1],z[2], z[3],z[4], z[5]];
act_P[L]     := (z) -> [ I*z[1],z[2],-z[3],z[4],-z[5]];
act_P[LL]    := (z) -> [-  z[1],z[2], z[3],z[4], z[5]];
act_P[LLL]   := (z) -> [-I*z[1],z[2],-z[3],z[4],-z[5]];
act_P[M]     := (z) -> [-  z[1],z[5], z[4],z[3], z[2]];
act_P[LM]    := (z) -> [-I*z[1],z[5],-z[4],z[3],-z[2]];
act_P[LLM]   := (z) -> [   z[1],z[5], z[4],z[3], z[2]];
act_P[LLLM]  := (z) -> [ I*z[1],z[5],-z[4],z[3],-z[2]];
act_P[N]     := (z) -> map(conjugate,[   z[1],z[2], z[3],z[4], z[5]]);
act_P[LN]    := (z) -> map(conjugate,[-I*z[1],z[2],-z[3],z[4],-z[5]]);
act_P[LLN]   := (z) -> map(conjugate,[-  z[1],z[2], z[3],z[4], z[5]]);
act_P[LLLN]  := (z) -> map(conjugate,[ I*z[1],z[2],-z[3],z[4],-z[5]]);
act_P[MN]    := (z) -> map(conjugate,[-  z[1],z[5], z[4],z[3], z[2]]);
act_P[LMN]   := (z) -> map(conjugate,[ I*z[1],z[5],-z[4],z[3],-z[2]]);
act_P[LLMN]  := (z) -> map(conjugate,[   z[1],z[5], z[4],z[3], z[2]]);
act_P[LLLMN] := (z) -> map(conjugate,[-I*z[1],z[5],-z[4],z[3],-z[2]]);

######################################################################
# The map p_P is equivariant with respect to the following action
# of G on C u {infinity}

#@ act_C
act_C[1]    := (z) ->  z;
act_C[L]    := (z) -> -z;
act_C[LL]   := (z) ->  z;
act_C[LLL]  := (z) -> -z;
act_C[M]    := (z) -> `if`(z=0,infinity, 1/z);
act_C[LM]   := (z) -> `if`(z=0,infinity,-1/z);
act_C[LLM]  := (z) -> `if`(z=0,infinity, 1/z);
act_C[LLLM] := (z) -> `if`(z=0,infinity,-1/z);
act_C[N]    := (z) ->  conjugate(z);
act_C[LN]   := (z) -> -conjugate(z);
act_C[LLN]  := (z) ->  conjugate(z);
act_C[LLLN] := (z) -> -conjugate(z);
act_C[MN]   := (z) -> `if`(z=0,infinity, 1/conjugate(z));
act_C[LMN]  := (z) -> `if`(z=0,infinity,-1/conjugate(z));
act_C[LLMN] := (z) -> `if`(z=0,infinity, 1/conjugate(z));
act_C[LLLMN]:= (z) -> `if`(z=0,infinity,-1/conjugate(z));

# If we identify C u {infinity} with S^2 by the map C_to_S2,
# we get the following action.

#@ act_S2
act_S2[1]    := (u) -> [ u[1], u[2], u[3]];
act_S2[L]    := (u) -> [-u[1],-u[2], u[3]];
act_S2[LL]   := (u) -> [ u[1], u[2], u[3]];
act_S2[LLL]  := (u) -> [-u[1],-u[2], u[3]];
act_S2[M]    := (u) -> [ u[1],-u[2],-u[3]];
act_S2[LM]   := (u) -> [-u[1], u[2],-u[3]];
act_S2[LLM]  := (u) -> [ u[1],-u[2],-u[3]];
act_S2[LLLM] := (u) -> [-u[1], u[2],-u[3]];
act_S2[N]    := (u) -> [ u[1],-u[2], u[3]];
act_S2[LN]   := (u) -> [-u[1], u[2], u[3]];
act_S2[LLN]  := (u) -> [ u[1],-u[2], u[3]];
act_S2[LLLN] := (u) -> [-u[1], u[2], u[3]];
act_S2[MN]   := (u) -> [ u[1], u[2],-u[3]];
act_S2[LMN]  := (u) -> [-u[1],-u[2],-u[3]];
act_S2[LLMN] := (u) -> [ u[1], u[2],-u[3]];
act_S2[LLLMN]:= (u) -> [-u[1],-u[2],-u[3]];

######################################################################

#@ v_P_0
v_P_0[ 0] := [0,0]:
v_P_0[ 1] := [infinity,infinity]:
v_P_0[ 2] := [-  (a_P^(-1)-a_P),-1]:
v_P_0[ 3] := [-I*(a_P^(-1)-a_P), 1]:
v_P_0[ 4] := [   (a_P^(-1)-a_P),-1]:
v_P_0[ 5] := [ I*(a_P^(-1)-a_P), 1]:
v_P_0[ 6] := [ (1+I)*(a_P^(-1)+a_P)/sqrt(2), I]:
v_P_0[ 7] := [-(1-I)*(a_P^(-1)+a_P)/sqrt(2),-I]:
v_P_0[ 8] := [-(1+I)*(a_P^(-1)+a_P)/sqrt(2), I]:
v_P_0[ 9] := [ (1-I)*(a_P^(-1)+a_P)/sqrt(2),-I]:
v_P_0[10] := [0,  -a_P]:
v_P_0[11] := [0,   a_P]:
v_P_0[12] := [0,-1/a_P]:
v_P_0[13] := [0, 1/a_P]:

#@ v_P
v_P[ 0] := [0,1,0,0,0]:
v_P[ 1] := [0,0,0,0,1]:
for i from 2 to 13 do v_P[i] := j_P(v_P_0[i]); od:

######################################################################

#@ c_P_jj
c_P_jj[ 0] := (t) -> [
 -sqrt((1/a_P-a_P)^2 + 4*sin(2*t)^2),
  exp(I*t),
 -exp(-I*t)
];

c_P_jj[ 1] := (t) -> [
 (1+I)/sqrt(2)*sin(t)*sqrt(16*cos(t)^2+(a_P^(-1)+a_P)^2*sin(t)^4)/8,
   (1+cos(t))/2,
 I*(1-cos(t))/2
];

c_P_jj[ 3] := (t) -> [
(-I*(a_P^(-1)-a_P)*sin(t)*sqrt((1+a_P)^4-(1-a_P)^4*cos(t)^2)*sqrt((1+a_P)^2-(1-a_P)^2*cos(t)^2))/8,
((1+a_P)+(1-a_P)*cos(t))/2,
((1+a_P)-(1-a_P)*cos(t))/2
];

c_P_jj[5] := (t) -> [
 sin(t)*sqrt(a_P*(3-cos(t))*(4-a_P^4*(1-cos(t))^2)/32),
 1,
 a_P * (1 - cos(t))/2
];

#@ c_P
c_P[ 0] := unapply(jj_P(c_P_jj[0](t)),t);
c_P[ 1] := unapply(jj_P(c_P_jj[1](t)),t);
c_P[ 2] := unapply(act_P[L](c_P[1](t)),t);
c_P[ 3] := unapply(jj_P(c_P_jj[3](t)),t);
c_P[ 4] := unapply(act_P[L](c_P[3](t)),t);
c_P[ 5] := unapply(jj_P(c_P_jj[5](t)),t);
c_P[ 6] := unapply(act_P[ L](c_P[5](t)),t);
c_P[ 7] := unapply(act_P[ M](c_P[5](t)),t);
c_P[ 8] := unapply(act_P[LM](c_P[5](t)),t);

#@ pc_P
pc_P[ 0] := (t) -> -exp(-2*I*t);
pc_P[ 1] := (t) ->  I*(1-cos(t))/(1+cos(t));
pc_P[ 2] := (t) -> -I*(1-cos(t))/(1+cos(t));
pc_P[ 3] := (t) ->  ((1+a_P)-(1-a_P)*cos(t))/((1+a_P)+(1-a_P)*cos(t));
pc_P[ 4] := (t) -> -((1+a_P)-(1-a_P)*cos(t))/((1+a_P)+(1-a_P)*cos(t));
pc_P[ 5] := (t) ->  (1-cos(t))/2 * a_P;
pc_P[ 6] := (t) -> -(1-cos(t))/2 * a_P;
pc_P[ 7] := (t) ->  1/((1-cos(t))/2 * a_P);
pc_P[ 8] := (t) -> -1/((1-cos(t))/2 * a_P);

#@ pc_P_lim
pc_P_lim[ 1] := [0,infinity * I];
pc_P_lim[ 2] := [-infinity * I,0];
pc_P_lim[ 3] := [   a_P, 1/a_P];
pc_P_lim[ 4] := [-1/a_P,  -a_P];
pc_P_lim[ 5] := [     0,   a_P];
pc_P_lim[ 6] := [  -a_P,     0];
pc_P_lim[ 7] := [ 1/a_P, infinity];
pc_P_lim[ 8] := [-infinity,-1/a_P];

######################################################################
# The function c_check_P[k](z) returns true if z lies in C[k]
# (This assumes that we already know that z lies in PX(a))

#@ c_check_P

c_check_P[0] := (z) ->
 simplify(z[2]*conjugate(z[2]) - z[5]*conjugate(z[5])) = 0;

c_check_P[1] := (z) -> is(simplify( I*z[2]*conjugate(z[3])) >= 0);

c_check_P[2] := (z) -> is(simplify(-I*z[2]*conjugate(z[3])) >= 0);

 c_check_P[3] := (z) -> is(simplify((  z[3] - a_P*z[2])*conjugate(z[2])) >= 0) and 
                       is(simplify((  z[4] - a_P*z[5])*conjugate(z[5])) >= 0);

c_check_P[4] := (z) -> is(simplify((- z[3] - a_P*z[2])*conjugate(z[2])) >= 0) and 
                       is(simplify((- z[4] - a_P*z[5])*conjugate(z[5])) >= 0);

c_check_P[5] := (z) -> is(simplify((- z[3] + a_P*z[2])*conjugate(z[2])) >= 0) and
                       is(simplify((- z[5] + a_P*z[4])*conjugate(z[5])) >= 0);

c_check_P[6] := (z) -> is(simplify((  z[3] + a_P*z[2])*conjugate(z[2])) >= 0) and
                       is(simplify((- z[5] - a_P*z[4])*conjugate(z[5])) >= 0);

c_check_P[7] := (z) -> is(simplify((- z[2] + a_P*z[3])*conjugate(z[2])) >= 0) and 
                       is(simplify((- z[4] + a_P*z[5])*conjugate(z[5])) >= 0);

c_check_P[8] := (z) -> is(simplify((- z[2] - a_P*z[3])*conjugate(z[2])) >= 0) and
                       is(simplify((  z[4] + a_P*z[5])*conjugate(z[5])) >= 0);

######################################################################

#@ is_in_F4_P
is_in_F4_P := proc(z)
 local u,v,w,err;
 if not is(simplify(Im(z[3]*conjugate(z[2]))) >= 0) then return false; fi;
 if simplify(z[2]) = 0 then return true; fi;
 u := simplify(z[3]/z[2]);
 v := simplify(z[1]/z[2]);
 w := sqrt(u)*sqrt(u-a_P)*sqrt(u+a_P)*sqrt(u-1/a_P)*sqrt(u+1/a_P);
 err := simplify_P(v + w);

 return evalb(err = 0);
end:

#@ is_in_F16_P
is_in_F16_P := proc(z)
 is(simplify(z[2]*conjugate(z[2]) - z[5]*conjugate(z[5])) >= 0) and
 is(simplify(Re(z[3]*conjugate(z[2]))) >= 0) and
 is(simplify(Im(z[3]*conjugate(z[2]))) >= 0) and
 is(simplify(Re(z[1]*conjugate(z[2]))) >= 0) and
 is(simplify(Re(z[1]*conjugate(z[2])) - Im(z[1]*conjugate(z[2]))) >= 0);
end:

