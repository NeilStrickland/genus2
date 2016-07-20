<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_r_P_cofactors := proc()
<a name="line_2"></a> local err;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> err := r_P_cofactor0(z) * r_P(z) + r_P_cofactor1(z) * diff(r_P(z),z) - 1;
<a name="line_7"></a> err := simplify(err);
<a name="line_8"></a>
<a name="line_9"></a> _ASSERT(err = 0,"r_P and its derivative are coprime");
<a name="line_10"></a>end:
<a name="line_11"></a>
<a name="line_12"></a>add_check(check_r_P_cofactors):
<a name="line_13"></a>
<a name="line_14"></a>######################################################################
<a name="line_15"></a>
<a name="line_16"></a>check_P_action := proc()
<a name="line_17"></a> local i,a,b,z,T,U,A,B,err;
<a name="line_18"></a>
<a name="line_19"></a> printf("%a()\n",procname);
<a name="line_20"></a>
<a name="line_21"></a> for i from 1 to 5 do
<a name="line_22"></a>  assume(a[i]::real);
<a name="line_23"></a>  assume(b[i]::real);
<a name="line_24"></a>  z[i] := a[i] + I * b[i];
<a name="line_25"></a> od:
<a name="line_26"></a>
<a name="line_27"></a> for T in G16 do
<a name="line_28"></a>  for U in G16 do
<a name="line_29"></a>   A := act_P[G_mult(T,U)](z);
<a name="line_30"></a>   B := act_P[T](act_P[U](z));
<a name="line_31"></a>   assert(A = B or A = (-1) *~ B,sprintf("%a o %a on PX(a)",T,U));
<a name="line_32"></a>  od;
<a name="line_33"></a> od;
<a name="line_34"></a>
<a name="line_35"></a> _ASSERT(
<a name="line_36"></a>  expand(P_rel[0](act_P[L](z)) + P_rel[0](z)) = 0 and
<a name="line_37"></a>  expand(P_rel[1](act_P[L](z)) - P_rel[1](z)) = 0 and
<a name="line_38"></a>  expand(P_rel[2](act_P[L](z)) + P_rel[2](z)) = 0 and
<a name="line_39"></a>  expand(P_rel[3](act_P[L](z)) - P_rel[3](z)) = 0,
<a name="line_40"></a>  "L preserves PX(a)"
<a name="line_41"></a> );
<a name="line_42"></a>
<a name="line_43"></a> _ASSERT(
<a name="line_44"></a>  expand(P_rel[0](act_P[M](z)) - P_rel[0](z)) = 0 and
<a name="line_45"></a>  expand(P_rel[1](act_P[M](z)) - P_rel[3](z)) = 0 and
<a name="line_46"></a>  expand(P_rel[2](act_P[M](z)) - P_rel[2](z)) = 0 and
<a name="line_47"></a>  expand(P_rel[3](act_P[M](z)) - P_rel[1](z)) = 0,
<a name="line_48"></a>  "M preserves PX(a)"
<a name="line_49"></a> );
<a name="line_50"></a>
<a name="line_51"></a> _ASSERT(
<a name="line_52"></a>  expand(P_rel[0](act_P[N](z)) - conjugate(P_rel[0](z))) = 0 and
<a name="line_53"></a>  expand(P_rel[1](act_P[N](z)) - conjugate(P_rel[1](z))) = 0 and
<a name="line_54"></a>  expand(P_rel[2](act_P[N](z)) - conjugate(P_rel[2](z))) = 0 and
<a name="line_55"></a>  expand(P_rel[3](act_P[N](z)) - conjugate(P_rel[3](z))) = 0,
<a name="line_56"></a>  "N preserves PX(a)"
<a name="line_57"></a> );
<a name="line_58"></a>
<a name="line_59"></a>end:
<a name="line_60"></a>
<a name="line_61"></a>add_check(check_P_action):
<a name="line_62"></a>
<a name="line_63"></a>######################################################################
<a name="line_64"></a>
<a name="line_65"></a>check_j_P := proc()
<a name="line_66"></a> local err,z_;
<a name="line_67"></a>
<a name="line_68"></a> printf("%a()\n",procname);
<a name="line_69"></a>
<a name="line_70"></a> err := expand(P_rel[0](j_P([w,z])) - 
<a name="line_71"></a>               (w^2 - z*(z-a_P)*(z+a_P)*(z-1/a_P)*(z+1/a_P)));
<a name="line_72"></a> _ASSERT(err = 0,"j(PX_0(a)) relation 0");
<a name="line_73"></a>
<a name="line_74"></a> _ASSERT(P_rel[1](j_P([w,z])) = 0,"j(PX_0(a)) relation 1");
<a name="line_75"></a> _ASSERT(P_rel[2](j_P([w,z])) = 0,"j(PX_0(a)) relation 2");
<a name="line_76"></a> _ASSERT(P_rel[3](j_P([w,z])) = 0,"j(PX_0(a)) relation 3");
<a name="line_77"></a>
<a name="line_78"></a> _ASSERT(is_equal_P(act_P[L](j_P([w,z])),j_P([I*w,-z])),
<a name="line_79"></a>        "L(j(w,z)) in PX(a)");
<a name="line_80"></a> _ASSERT(is_equal_P(act_P[M](j_P([w,z])),j_P([-w/z^3,1/z])),
<a name="line_81"></a>        "M(j(w,z)) in PX(a)");
<a name="line_82"></a>
<a name="line_83"></a> z_ := [seq(z[i],i=1..5)];
<a name="line_84"></a> _ASSERT(NF_P(numer(j_inv_P(z_)[1]^2 - r_P(j_inv_P(z_)[2]))) = 0,
<a name="line_85"></a>         "j_inv_P sends PX(a) to PX_0(a)");
<a name="line_86"></a>
<a name="line_87"></a> _ASSERT(j_inv_P(j_P([w,z])) = [w,z],"j_inv_P o j_P");
<a name="line_88"></a>
<a name="line_89"></a> _ASSERT(
<a name="line_90"></a>  {op(NF_P(numer(j_inv_P(z_)[1]^2 - r_P(j_inv_P(z_)[2]))))} = {0},
<a name="line_91"></a>  "j_P o j_inv_P");
<a name="line_92"></a>  
<a name="line_93"></a>end:
<a name="line_94"></a>
<a name="line_95"></a>add_check(check_j_P):
<a name="line_96"></a>
<a name="line_97"></a>######################################################################
<a name="line_98"></a>
<a name="line_99"></a>check_pc_P := proc()
<a name="line_100"></a> local k,u,lm;
<a name="line_101"></a>
<a name="line_102"></a> printf("%a()\n",procname);
<a name="line_103"></a>
<a name="line_104"></a> for k from 0 to 8 do
<a name="line_105"></a>  _ASSERT(simplify(p_P(c_P[k](t)) - pc_P[k](t)) = 0,
<a name="line_106"></a>          sprintf("pc_P[%d] = p o c_P[%d]",k,k));
<a name="line_107"></a> od:
<a name="line_108"></a>
<a name="line_109"></a> for k from 1 to 8 do 
<a name="line_110"></a>  if k <= 2 then u := I else u := 1; fi;
<a name="line_111"></a>  lm := u *~ [minimize(pc_P[k](t)/u,t=0..2*Pi),maximize(pc_P[k](t)/u,t=0..2*Pi)];
<a name="line_112"></a>  lm := subs({infinity/a_P = infinity,-infinity/a_P = -infinity},lm);
<a name="line_113"></a>  _ASSERT(pc_P_lim[k] = lm,sprintf("limits for pc_P[%d]",k),
<a name="line_114"></a>           [k,lm,pc_P_lim[k],lm -~ pc_P_lim[k]]);
<a name="line_115"></a> od:
<a name="line_116"></a>end:
<a name="line_117"></a>
<a name="line_118"></a>add_check(check_pc_P):
<a name="line_119"></a>
  </pre>
 </body>
</html>
    