check_forms := proc()
 local i,j,check_equivariant,err,v,AB_basis,Om7_gens,Om7_rels;

 printf("%a()\n",procname);

 check_equivariant := proc(u)
  v := [act_A[L](u) - u,
        act_A[M](u) - u,
        act_A[N](u) + u];
  return(evalb(map(NF_x,v) = [0,0,0]));
 end;

 for i from 1 to 4 do
  _ASSERT(
   check_equivariant(alpha_form[i]),
   sprintf("alpha[%d] is equivariant",i)
  );
 od;

 for i from 1 to 4 do
  _ASSERT(
   check_equivariant(beta_form[i]),
   sprintf("beta[%d] is equivariant",i)
  );
 od;

 for i from 1 to 4 do
  err := alpha_form[i] - add(alpha_beta[i][j] * beta_form[j],j=1..4);
  err := NF_x0(err);
  _ASSERT(err = 0,sprintf("alpha[%d] in terms of beta",i));
 od;

 _ASSERT(simplify(Determinant(Matrix(alpha_beta)) - (2 - z[1])) = 0,
         "determinant of matrix relating alpha and beta");

 AB_basis := [seq(seq(seq(seq(x[1]^i*x[2]^j*y[1]^k*y[2]^l,i=0..1),j=0..1),k=0..1),l=0..1)]:

 Om7_gens := {seq(seq(G16_twisted_average(7,AB_basis[i]*dx[j]),j=1..4),i=1..nops(AB_basis))} minus {0};
 Om7_gens := map(NF_x0,Om7_gens);

 _ASSERT(Om7_gens = map(NF_x0,expand({beta_form[1]/2,-beta_form[1]/2,beta_form[2]/2,beta_form[3],beta_form[4]})),
         "beta forms generate twisted invariants");

 Om7_rels := expand({seq(seq(G16_twisted_average(7,AB_basis[i]*theta_form[j]),j=1..2),i=1..nops(AB_basis))}) minus {0}:
 Om7_rels := map(NF_x0,Om7_rels);

 _ASSERT(Om7_rels = map(NF_x0,{alpha_form[3],alpha_form[4]}),
         "alpha[3] and alpha[4] generate twisted invariant relations");

 for i from 1 to 2 do 
  _ASSERT(simplify(subs(forms_to_y,theta_form[i])) = 0,
          sprintf("theta[%d] rewrites as zero in terms of y",i));
 od;

 for i from 1 to 2 do 
  err := 
   NF_z0(unnormalised_stokes(subs({y[1]=yx0[1],y[2]=yx0[2],z[1]=zx0[1],z[2]=zx0[2]},alpha_form[i]))) - 
    ndg_z * D_alpha[i];
  err := simplify(err);

  _ASSERT(err = 0,sprintf("D_alpha[%d] = d(alpha_form[%d])",i,i));
 od;
end:

add_check(check_forms):