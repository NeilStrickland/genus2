check_r_P_cofactors := proc()
 local err;

 printf("%a()\n",procname);

 err := r_P_cofactor0(z) * r_P(z) + r_P_cofactor1(z) * diff(r_P(z),z) - 1;
 err := simplify(err);

 _ASSERT(err = 0,"r_P and its derivative are coprime");
end:

add_check(check_r_P_cofactors):

######################################################################

check_P_action := proc()
 local i,a,b,z,T,U,A,B,err;

 printf("%a()\n",procname);

 for i from 1 to 5 do
  assume(a[i]::real);
  assume(b[i]::real);
  z[i] := a[i] + I * b[i];
 od:

 for T in G16 do
  for U in G16 do
   A := act_P[G_mult(T,U)](z);
   B := act_P[T](act_P[U](z));
   assert(A = B or A = (-1) *~ B,sprintf("%a o %a on PX(a)",T,U));
  od;
 od;

 _ASSERT(
  expand(P_rel[0](act_P[L](z)) + P_rel[0](z)) = 0 and
  expand(P_rel[1](act_P[L](z)) - P_rel[1](z)) = 0 and
  expand(P_rel[2](act_P[L](z)) + P_rel[2](z)) = 0 and
  expand(P_rel[3](act_P[L](z)) - P_rel[3](z)) = 0,
  "L preserves PX(a)"
 );

 _ASSERT(
  expand(P_rel[0](act_P[M](z)) - P_rel[0](z)) = 0 and
  expand(P_rel[1](act_P[M](z)) - P_rel[3](z)) = 0 and
  expand(P_rel[2](act_P[M](z)) - P_rel[2](z)) = 0 and
  expand(P_rel[3](act_P[M](z)) - P_rel[1](z)) = 0,
  "M preserves PX(a)"
 );

 _ASSERT(
  expand(P_rel[0](act_P[N](z)) - conjugate(P_rel[0](z))) = 0 and
  expand(P_rel[1](act_P[N](z)) - conjugate(P_rel[1](z))) = 0 and
  expand(P_rel[2](act_P[N](z)) - conjugate(P_rel[2](z))) = 0 and
  expand(P_rel[3](act_P[N](z)) - conjugate(P_rel[3](z))) = 0,
  "N preserves PX(a)"
 );

end:

add_check(check_P_action):

######################################################################

check_j_P := proc()
 local err,z_;

 printf("%a()\n",procname);

 err := expand(P_rel[0](j_P([w,z])) - 
               (w^2 - z*(z-a_P)*(z+a_P)*(z-1/a_P)*(z+1/a_P)));
 _ASSERT(err = 0,"j(PX_0(a)) relation 0");

 _ASSERT(P_rel[1](j_P([w,z])) = 0,"j(PX_0(a)) relation 1");
 _ASSERT(P_rel[2](j_P([w,z])) = 0,"j(PX_0(a)) relation 2");
 _ASSERT(P_rel[3](j_P([w,z])) = 0,"j(PX_0(a)) relation 3");

 _ASSERT(is_equal_P(act_P[L](j_P([w,z])),j_P([I*w,-z])),
        "L(j(w,z)) in PX(a)");
 _ASSERT(is_equal_P(act_P[M](j_P([w,z])),j_P([-w/z^3,1/z])),
        "M(j(w,z)) in PX(a)");

 z_ := [seq(z[i],i=1..5)];
 _ASSERT(NF_P(numer(j_inv_P(z_)[1]^2 - r_P(j_inv_P(z_)[2]))) = 0,
         "j_inv_P sends PX(a) to PX_0(a)");

 _ASSERT(j_inv_P(j_P([w,z])) = [w,z],"j_inv_P o j_P");

 _ASSERT(
  {op(NF_P(numer(j_inv_P(z_)[1]^2 - r_P(j_inv_P(z_)[2]))))} = {0},
  "j_P o j_inv_P");
  
end:

add_check(check_j_P):

######################################################################

check_pc_P := proc()
 local k,u,lm;

 printf("%a()\n",procname);

 for k from 0 to 8 do
  _ASSERT(simplify(p_P(c_P[k](t)) - pc_P[k](t)) = 0,
          sprintf("pc_P[%d] = p o c_P[%d]",k,k));
 od:

 for k from 1 to 8 do 
  if k <= 2 then u := I else u := 1; fi;
  lm := u *~ [minimize(pc_P[k](t)/u,t=0..2*Pi),maximize(pc_P[k](t)/u,t=0..2*Pi)];
  lm := subs({infinity/a_P = infinity,-infinity/a_P = -infinity},lm);
  _ASSERT(pc_P_lim[k] = lm,sprintf("limits for pc_P[%d]",k),
           [k,lm,pc_P_lim[k],lm -~ pc_P_lim[k]]);
 od:
end:

add_check(check_pc_P):

