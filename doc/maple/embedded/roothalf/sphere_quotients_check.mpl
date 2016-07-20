<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_map_to_S2_charts := proc()
<a name="line_2"></a> local k,aa,bb,cc,dd,MM;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> for k in {0,3,6,11} do
<a name="line_7"></a>  # Chart centres
<a name="line_8"></a>  _ASSERT(
<a name="line_9"></a>   chart[k](0,0) -~ v_E0[k] = [0$4],
<a name="line_10"></a>   sprintf("chart %d is centred",k));
<a name="line_11"></a>  
<a name="line_12"></a>  # Charts stay on EX to first order
<a name="line_13"></a>  _ASSERT(
<a name="line_14"></a>   simplify(rem(rho(chart[k](e*s,e*t))-1,e^2,e)) = 0,
<a name="line_15"></a>   sprintf("chart %d is tangent to S^3",k));
<a name="line_16"></a>   
<a name="line_17"></a>  _ASSERT(
<a name="line_18"></a>   simplify(rem(g_00(chart[k](e*s,e*t)),e^2,e)) = 0,
<a name="line_19"></a>   sprintf("chart %d is tangent to EX^*",k));
<a name="line_20"></a>
<a name="line_21"></a>  # Charts are conformal to first order
<a name="line_22"></a>  aa := v_E0[k];
<a name="line_23"></a>  bb := dg0(aa);
<a name="line_24"></a>  cc := subs(s=0,map(diff,chart[k](s,0),s));
<a name="line_25"></a>  dd := subs(t=0,map(diff,chart[k](0,t),t));
<a name="line_26"></a>  MM := Matrix([aa,bb,cc,dd]);
<a name="line_27"></a>
<a name="line_28"></a>  _ASSERT(
<a name="line_29"></a>   evalf(simplify(Determinant(MM))) > 0,
<a name="line_30"></a>   sprintf("chart %d is oriented",k));
<a name="line_31"></a>
<a name="line_32"></a>  _ASSERT(
<a name="line_33"></a>   simplify({dp4(aa,bb),dp4(aa,cc),dp4(aa,dd),dp4(bb,cc),dp4(bb,dd),dp4(cc,dd),
<a name="line_34"></a>             dp4(aa,aa)-1,dp4(cc,cc)-1,dp4(dd,dd)-1}) = {0},
<a name="line_35"></a>   sprintf("chart %d is conformal to first order",k));
<a name="line_36"></a> od:
<a name="line_37"></a>end:
<a name="line_38"></a>
<a name="line_39"></a>add_check(check_map_to_S2_charts):
<a name="line_40"></a>
<a name="line_41"></a>######################################################################
<a name="line_42"></a>
<a name="line_43"></a>check_E_to_S2 := proc()
<a name="line_44"></a> local s,S3,S4,sgn3,sgn4,f0,df0,n0,m0,J0;
<a name="line_45"></a> 
<a name="line_46"></a> printf("%a()\n",procname);
<a name="line_47"></a>
<a name="line_48"></a> _ASSERT(
<a name="line_49"></a>  FNF_y0(simplify(E_to_S2(xx) *~ sqrt(1 - E_to_S2_s_x(xx)) -~ E_to_S2_tilde(xx))) = [0$3],
<a name="line_50"></a>  "E_to_S2 in terms of E_to_S2_tilde"
<a name="line_51"></a> );
<a name="line_52"></a>
<a name="line_53"></a> _ASSERT(
<a name="line_54"></a>  FNF_y0(simplify(E_to_S2_s_x(xx) - E_to_S2_s_z(z_proj0(xx)))) = 0,
<a name="line_55"></a>  "E_to_S2_s_x is compatible with E_to_S2_s_z"
<a name="line_56"></a> );
<a name="line_57"></a>
<a name="line_58"></a> _ASSERT(
<a name="line_59"></a>  simplify(oval_h([E_to_S2_s_max_z1,E_to_S2_s_max_z2])) = 0,
<a name="line_60"></a>  "maximising point for E_to_S2_s_z is on the boundary of F16"
<a name="line_61"></a> );
<a name="line_62"></a>
<a name="line_63"></a> _ASSERT(
<a name="line_64"></a>  simplify(E_to_S2_s_z([E_to_S2_s_max_z1,E_to_S2_s_max_z2]) - E_to_S2_s_max) = 0,
<a name="line_65"></a>  "maximum value of E_to_S2_s_z"
<a name="line_66"></a> );
<a name="line_67"></a>
<a name="line_68"></a> _ASSERT(
<a name="line_69"></a>  simplify(subs(z[1]=E_to_S2_s_max_z1,factor(diff(subs(solve(oval_h(zz),{z[2]}),E_to_S2_s_z(z)),z[1])))) = 0,
<a name="line_70"></a>  "maximising point for E_to_S2_s_z is maximising"
<a name="line_71"></a> );
<a name="line_72"></a>
<a name="line_73"></a> _ASSERT(FNF_y0(nm3(E_to_S2(x))^2 - 1) = 0,"E_to_S2 lands in S2");
<a name="line_74"></a>
<a name="line_75"></a> _ASSERT(
<a name="line_76"></a>   {seq(simplify(E_to_S2(v_E0[i]) -~ v_S2[i]),i=0..13)} = {[0$3]},
<a name="line_77"></a>   "E_to_S2 on vertices"
<a name="line_78"></a> );
<a name="line_79"></a>
<a name="line_80"></a> assume(s::real);
<a name="line_81"></a> 
<a name="line_82"></a> _ASSERT(
<a name="line_83"></a>   {seq(simplify(expand(combine(E_to_S2(c_E0[k](s)) -~ c_S2[k](s)))),k=0..16)} = {[0$3]},
<a name="line_84"></a>   "E_to_S2 on curves"
<a name="line_85"></a> );
<a name="line_86"></a>
<a name="line_87"></a> _ASSERT(
<a name="line_88"></a>   {seq(simplify(E_to_S2(act_R4[T](xx)) -~ act_S2[T](E_to_S2(xx))),T in G16)} = {[0$3]},
<a name="line_89"></a>   "E_to_S2 is equivariant"
<a name="line_90"></a> );
<a name="line_91"></a>
<a name="line_92"></a> S3 := combinat[permute](3):
<a name="line_93"></a> S4 := combinat[permute](4):
<a name="line_94"></a> sgn3 := (s) -> signum(mul(mul(s[j]-s[i],j=i+1..3),i=1..2));
<a name="line_95"></a> sgn4 := (s) -> signum(mul(mul(s[j]-s[i],j=i+1..4),i=1..3));
<a name="line_96"></a> f0 := E_to_S2(xx);
<a name="line_97"></a> df0 := [seq([seq(factor(diff(f0[i],x[j])),j=1..4)],i=1..3)];
<a name="line_98"></a> n0 := dg0(xx);
<a name="line_99"></a> J0 := add(add(add(add(add(add(add(
<a name="line_100"></a>        -sgn4([i,j,k,l])*x[i]*n0[j]*df0[p,k]*df0[q,l]*sgn3([p,q,r])*f0[r]/2,
<a name="line_101"></a>	i=1..4),j=1..4),k=1..4),l=1..4),p=1..3),q=1..3),r=1..3);
<a name="line_102"></a> m0 := (4*x[3]^4-2*x[3]^2*x[4]^2+4*x[4]^4+8*x[3]^2+4);
<a name="line_103"></a> J0 := FNF_z0(simplify(sqrt(m0)*J0))/sqrt(FNF_z0(m0));
<a name="line_104"></a> _ASSERT(simplify(J0 - E_to_S2_jacobian) = 0,"jacobian of E_to_S2");
<a name="line_105"></a> 
<a name="line_106"></a>end:
<a name="line_107"></a>
<a name="line_108"></a>add_check(check_E_to_S2):
<a name="line_109"></a>
<a name="line_110"></a>
<a name="line_111"></a>
  </pre>
 </body>
</html>
    