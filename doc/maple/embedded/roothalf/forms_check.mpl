<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_forms := proc()
<a name="line_2"></a> local i,j,check_equivariant,err,v,AB_basis,Om7_gens,Om7_rels;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> check_equivariant := proc(u)
<a name="line_7"></a>  v := [act_A[L](u) - u,
<a name="line_8"></a>        act_A[M](u) - u,
<a name="line_9"></a>        act_A[N](u) + u];
<a name="line_10"></a>  return(evalb(map(NF_x,v) = [0,0,0]));
<a name="line_11"></a> end;
<a name="line_12"></a>
<a name="line_13"></a> for i from 1 to 4 do
<a name="line_14"></a>  _ASSERT(
<a name="line_15"></a>   check_equivariant(alpha_form[i]),
<a name="line_16"></a>   sprintf("alpha[%d] is equivariant",i)
<a name="line_17"></a>  );
<a name="line_18"></a> od;
<a name="line_19"></a>
<a name="line_20"></a> for i from 1 to 4 do
<a name="line_21"></a>  _ASSERT(
<a name="line_22"></a>   check_equivariant(beta_form[i]),
<a name="line_23"></a>   sprintf("beta[%d] is equivariant",i)
<a name="line_24"></a>  );
<a name="line_25"></a> od;
<a name="line_26"></a>
<a name="line_27"></a> for i from 1 to 4 do
<a name="line_28"></a>  err := alpha_form[i] - add(alpha_beta[i][j] * beta_form[j],j=1..4);
<a name="line_29"></a>  err := NF_x0(err);
<a name="line_30"></a>  _ASSERT(err = 0,sprintf("alpha[%d] in terms of beta",i));
<a name="line_31"></a> od;
<a name="line_32"></a>
<a name="line_33"></a> _ASSERT(simplify(Determinant(Matrix(alpha_beta)) - (2 - z[1])) = 0,
<a name="line_34"></a>         "determinant of matrix relating alpha and beta");
<a name="line_35"></a>
<a name="line_36"></a> AB_basis := [seq(seq(seq(seq(x[1]^i*x[2]^j*y[1]^k*y[2]^l,i=0..1),j=0..1),k=0..1),l=0..1)]:
<a name="line_37"></a>
<a name="line_38"></a> Om7_gens := {seq(seq(G16_twisted_average(7,AB_basis[i]*dx[j]),j=1..4),i=1..nops(AB_basis))} minus {0};
<a name="line_39"></a> Om7_gens := map(NF_x0,Om7_gens);
<a name="line_40"></a>
<a name="line_41"></a> _ASSERT(Om7_gens = map(NF_x0,expand({beta_form[1]/2,-beta_form[1]/2,beta_form[2]/2,beta_form[3],beta_form[4]})),
<a name="line_42"></a>         "beta forms generate twisted invariants");
<a name="line_43"></a>
<a name="line_44"></a> Om7_rels := expand({seq(seq(G16_twisted_average(7,AB_basis[i]*theta_form[j]),j=1..2),i=1..nops(AB_basis))}) minus {0}:
<a name="line_45"></a> Om7_rels := map(NF_x0,Om7_rels);
<a name="line_46"></a>
<a name="line_47"></a> _ASSERT(Om7_rels = map(NF_x0,{alpha_form[3],alpha_form[4]}),
<a name="line_48"></a>         "alpha[3] and alpha[4] generate twisted invariant relations");
<a name="line_49"></a>
<a name="line_50"></a> for i from 1 to 2 do 
<a name="line_51"></a>  _ASSERT(simplify(subs(forms_to_y,theta_form[i])) = 0,
<a name="line_52"></a>          sprintf("theta[%d] rewrites as zero in terms of y",i));
<a name="line_53"></a> od;
<a name="line_54"></a>
<a name="line_55"></a> for i from 1 to 2 do 
<a name="line_56"></a>  err := 
<a name="line_57"></a>   NF_z0(unnormalised_stokes(subs({y[1]=yx0[1],y[2]=yx0[2],z[1]=zx0[1],z[2]=zx0[2]},alpha_form[i]))) - 
<a name="line_58"></a>    ndg_z * D_alpha[i];
<a name="line_59"></a>  err := simplify(err);
<a name="line_60"></a>
<a name="line_61"></a>  _ASSERT(err = 0,sprintf("D_alpha[%d] = d(alpha_form[%d])",i,i));
<a name="line_62"></a> od;
<a name="line_63"></a>end:
<a name="line_64"></a>
<a name="line_65"></a>add_check(check_forms):  </pre>
 </body>
</html>
    