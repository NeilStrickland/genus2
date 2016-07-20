<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_PK := proc()
<a name="line_2"></a> local i,T,U,t2,err,rr;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> _ASSERT(
<a name="line_7"></a>  `and`(op(map(PK_check_zero,PK_rels))),
<a name="line_8"></a>  "PK relations"
<a name="line_9"></a> );
<a name="line_10"></a>
<a name="line_11"></a> _ASSERT(
<a name="line_12"></a>  {seq(seq(factor(act_PK[T](wz_reduce(a))-wz_reduce(act_PK[T](a))),T in G8),a in PK_vars)} = {0},
<a name="line_13"></a>  "act_PK is consistent with expressions for t,u,v in terms of w and z"
<a name="line_14"></a> );
<a name="line_15"></a>
<a name="line_16"></a>end:
<a name="line_17"></a>
<a name="line_18"></a>add_check(check_PK):
<a name="line_19"></a>
<a name="line_20"></a>######################################################################
<a name="line_21"></a>
<a name="line_22"></a>check_PK_subfields := proc()
<a name="line_23"></a> local i;
<a name="line_24"></a>
<a name="line_25"></a> printf("%a()\n",procname);
<a name="line_26"></a>
<a name="line_27"></a> for i in sort(map(op,[indices(PK_subfields)])) do
<a name="line_28"></a>  _ASSERT(PK_subfields[i]["check"],sprintf("PK_subfields[%d]",i));
<a name="line_29"></a> od;
<a name="line_30"></a>end:
<a name="line_31"></a>
<a name="line_32"></a>add_check(check_PK_subfields):
<a name="line_33"></a>
<a name="line_34"></a>######################################################################
<a name="line_35"></a>
<a name="line_36"></a>check_transition_maps := proc()
<a name="line_37"></a> local i,j,ij,x,y,tg,err0,err1;
<a name="line_38"></a> 
<a name="line_39"></a> printf("%a()\n",procname);
<a name="line_40"></a>
<a name="line_41"></a> for i in [2,3,8,9,10] do
<a name="line_42"></a>  tg[i] := PK_subfields[i]["generating_sets"][1][1];
<a name="line_43"></a> od:
<a name="line_44"></a>
<a name="line_45"></a> for ij in [[2,3],[2,8],[2,9],[2,10],[3,10],[8,10],[9,10]] do
<a name="line_46"></a>  i,j := op(ij);
<a name="line_47"></a>  _ASSERT(
<a name="line_48"></a>   factor(wz_reduce(p_C[i,j](tg[i]) - tg[j])) = 0,
<a name="line_49"></a>   sprintf("p_C[%d,%d] carries the generator of subfield %d to the generator of subfield %d",i,j,i,j)
<a name="line_50"></a>  );
<a name="line_51"></a> od:
<a name="line_52"></a> 
<a name="line_53"></a> _ASSERT(factor(p_C[3,10](p_C[2,3](z)) - p_C[2,10](z)) = 0,"p[3,10] o p[2,3] = p[2,10]");
<a name="line_54"></a> _ASSERT(factor(p_C[8,10](p_C[2,8](z)) - p_C[2,10](z)) = 0,"p[8,10] o p[2,8] = p[2,10]");
<a name="line_55"></a> _ASSERT(factor(p_C[9,10](p_C[2,9](z)) - p_C[2,10](z)) = 0,"p[9,10] o p[2,9] = p[2,10]");
<a name="line_56"></a>
<a name="line_57"></a> _ASSERT(convert(series(p_C[2, 3](z),z=0,3),polynom,z) - z^2   = 0,"p[2, 3] near v[0]");
<a name="line_58"></a> _ASSERT(convert(series(p_C[2, 8](z),z=0,3),polynom,z) - 2*z   = 0,"p[2, 8] near v[0]");
<a name="line_59"></a> _ASSERT(convert(series(p_C[2, 9](z),z=0,3),polynom,z) - 2*I*z = 0,"p[2, 9] near v[0]");
<a name="line_60"></a> _ASSERT(convert(series(p_C[2,10](z),z=0,3),polynom,z) - 2*z^2 = 0,"p[2,10] near v[0]");
<a name="line_61"></a> _ASSERT(convert(series(p_C[3,10](z),z=0,3),polynom,z) - 2*z   = 0,"p[3,10] near v[0]");
<a name="line_62"></a> _ASSERT(convert(series(p_C[8,10](z),z=0,3),polynom,z) - z^2/2 = 0,"p[8,10] near v[0]");
<a name="line_63"></a> _ASSERT(convert(series(p_C[9,10](z),z=0,3),polynom,z) + z^2/2 = 0,"p[9,10] near v[0]");
<a name="line_64"></a>
<a name="line_65"></a> assume(x::real, y::real);
<a name="line_66"></a>
<a name="line_67"></a> _ASSERT(factor(p_S2[2, 3](C_to_S2(x+I*y)) - C_to_S2(p_C[2, 3](x+I*y))) = [0,0,0],
<a name="line_68"></a>         "p_S2[2, 3] is compatible with p_C[2, 3]");
<a name="line_69"></a>	 
<a name="line_70"></a> _ASSERT(factor(p_S2[2, 8](C_to_S2(x+I*y)) - C_to_S2(p_C[2, 8](x+I*y))) = [0,0,0],
<a name="line_71"></a>         "p_S2[2, 8] is compatible with p_C[2, 8]");
<a name="line_72"></a>	 
<a name="line_73"></a> _ASSERT(factor(p_S2[2, 9](C_to_S2(x+I*y)) - C_to_S2(p_C[2, 9](x+I*y))) = [0,0,0],
<a name="line_74"></a>         "p_S2[2, 9] is compatible with p_C[2, 9]");
<a name="line_75"></a>	 
<a name="line_76"></a> _ASSERT(factor(p_S2[2,10](C_to_S2(x+I*y)) - C_to_S2(p_C[2,10](x+I*y))) = [0,0,0],
<a name="line_77"></a>         "p_S2[2,10] is compatible with p_C[2,10]");
<a name="line_78"></a>
<a name="line_79"></a> _ASSERT(factor(p_S2[3,10](C_to_S2(x+I*y)) - C_to_S2(p_C[3,10](x+I*y))) = [0,0,0],
<a name="line_80"></a>         "p_S2[3,10] is compatible with p_C[3,10]");
<a name="line_81"></a>	 
<a name="line_82"></a> _ASSERT(factor(p_S2[8,10](C_to_S2(x+I*y)) - C_to_S2(p_C[8,10](x+I*y))) = [0,0,0],
<a name="line_83"></a>         "p_S2[8,10] is compatible with p_C[8,10]");
<a name="line_84"></a>	 
<a name="line_85"></a> _ASSERT(factor(p_S2[9,10](C_to_S2(x+I*y)) - C_to_S2(p_C[9,10](x+I*y))) = [0,0,0],
<a name="line_86"></a>         "p_S2[9,10] is compatible with p_C[9,10]");
<a name="line_87"></a>	 	 
<a name="line_88"></a>end:
<a name="line_89"></a>
<a name="line_90"></a>add_check(check_transition_maps):
  </pre>
 </body>
</html>
    