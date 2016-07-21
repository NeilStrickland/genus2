<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_extra_curves := proc()
<a name="line_2"></a> local k,d,s,s0,t,err,check_alg;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> for k from 0 to num_curves_E-1 do
<a name="line_7"></a>  _ASSERT(is_member_E(c_E[k](t)),sprintf("c[%d](t) is in EX",k));
<a name="line_8"></a> od;
<a name="line_9"></a>
<a name="line_10"></a> _ASSERT(is_member_E(c_algebraic(t)),"c_algebraic(t) is in EX");
<a name="line_11"></a>
<a name="line_12"></a> err := simplify(c_algebraic(-sqrt((1/a_E^2-1)/2)) -~ v_E[10]);
<a name="line_13"></a> err := max(seq(op(map(abs,evalf(subs(a_E=0.1*i,err)))),i=1..9));
<a name="line_14"></a> _ASSERT(err < 10.^(-90),"v[10] on c_algebraic");
<a name="line_15"></a>
<a name="line_16"></a> err := simplify(c_algebraic( sqrt((1/a_E^2-1)/2)) -~ v_E[11]);
<a name="line_17"></a> err := max(seq(op(map(abs,evalf(subs(a_E=0.1*i,err)))),i=1..9));
<a name="line_18"></a> _ASSERT(err < 10.^(-90),"v[11] on c_algebraic");
<a name="line_19"></a>
<a name="line_20"></a> _ASSERT(simplify(c_algebraic( 0)         - v_E[ 0])=[0$4],"v[ 0] on c_algebraic");
<a name="line_21"></a> _ASSERT(simplify(c_algebraic( 1/2/a_E)   - v_E[ 3])=[0$4],"v[ 3] on c_algebraic");
<a name="line_22"></a>
<a name="line_23"></a> err :=  simplify_E(factor(combine(c_algebraic(tau_algebraic[6](t)) -~ c_E[6](t))))
<a name="line_24"></a>           assuming t>0 and t < Pi;
<a name="line_25"></a>
<a name="line_26"></a> _ASSERT(
<a name="line_27"></a>  err[1] = 0 and err[3] = 0 and err[4] = 0 and
<a name="line_28"></a>  max(seq(seq(abs(evalf(subs({a_E=0.1*i,t=Pi/10*j},err[2]))),i=1..9),j=0..10)) < 10^(-99),
<a name="line_29"></a>  "c_E[6] = c_algebraic o tau_algebraic[6] on (0,Pi)"
<a name="line_30"></a> );
<a name="line_31"></a> 
<a name="line_32"></a> for k from 9 to 12 do
<a name="line_33"></a>  _ASSERT(c_check_E[k](c_E[k](t)) = true,sprintf("c[%d](t) is in C[%d]",k,k));
<a name="line_34"></a> od:
<a name="line_35"></a>
<a name="line_36"></a> for k from 13 to 16 do
<a name="line_37"></a>  _ASSERT(`and`(seq(c_check_E[k](c_E[k](i*Pi/12)),i=0..23)),sprintf("c[%d](t) is in C[%d]",k,k));
<a name="line_38"></a> od;
<a name="line_39"></a>
<a name="line_40"></a> for k from 9 to 16 do 
<a name="line_41"></a>  err := simplify(c_param_E[k](c_E[k](t)) - t) assuming t > -Pi and t < Pi;
<a name="line_42"></a>  _ASSERT(err = 0,sprintf("c_param_E[%d]",k));
<a name="line_43"></a> od:
<a name="line_44"></a>
<a name="line_45"></a> _ASSERT(
<a name="line_46"></a>  select(i -> c_check_E[1](v_E[i]) and c_check_E[9](v_E[i]),
<a name="line_47"></a>	 {seq(i,i=0..num_vertices_E-1)}) = {14,16},
<a name="line_48"></a>  "C[1] intersect C[9] = {v[14],v[16]}"
<a name="line_49"></a> );
<a name="line_50"></a>
<a name="line_51"></a>end:
<a name="line_52"></a>
<a name="line_53"></a>add_check(check_extra_curves):
<a name="line_54"></a>
  </pre>
 </body>
</html>
    