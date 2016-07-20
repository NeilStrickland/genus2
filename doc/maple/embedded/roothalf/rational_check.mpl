<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_rational := proc()
<a name="line_2"></a> local s,t;
<a name="line_3"></a> 
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> assume(s::real,t::real);
<a name="line_7"></a> 
<a name="line_8"></a> _ASSERT(simplify(rho(c_rational[0](t)) - 1) = 0,"c_rational[0] lands in S3");
<a name="line_9"></a> _ASSERT(simplify(rho(c_rational[1]([s,t])) - 1) = 2*s^2 + t^2 - 1,"c_rational[1] lands in S3");
<a name="line_10"></a> _ASSERT(simplify(rho(c_rational[2]([s,t])) - 1) = 2*s^2 + t^2 - 1,"c_rational[2] lands in S3");
<a name="line_11"></a> _ASSERT(simplify(rho(c_quasirational[1](t)) - 1) = 0,"c_quasirational[1] lands in S3");
<a name="line_12"></a> _ASSERT(simplify(rho(c_quasirational[2](t)) - 1) = 0,"c_quasirational[2] lands in S3");
<a name="line_13"></a> _ASSERT(simplify(rho(c_quasirational[3]([s,t])) - 1) = 3*s^2 + t^2 - 1,"c_quasirational[3] lands in S3");
<a name="line_14"></a> _ASSERT(simplify(rho(c_quasirational[4]([s,t])) - 1) = 3*s^2 + t^2 - 1,"c_quasirational[4] lands in S3");
<a name="line_15"></a>
<a name="line_16"></a>_ASSERT(simplify(g0(c_rational[0](t)))          = 0,"c_rational[0] lands in EX^*");
<a name="line_17"></a> _ASSERT(simplify(g0(c_rational[1]([s,t])))      = 0,"c_rational[1] lands in EX^*");
<a name="line_18"></a> _ASSERT(simplify(g0(c_rational[2]([s,t])))      = 0,"c_rational[2] lands in EX^*");
<a name="line_19"></a> _ASSERT(simplify(g0(c_quasirational[1](t)))     = 0,"c_quasirational[1] lands in EX^*");
<a name="line_20"></a> _ASSERT(simplify(g0(c_quasirational[2](t)))     = 0,"c_quasirational[2] lands in EX^*");
<a name="line_21"></a> _ASSERT(simplify(g0(c_quasirational[3]([s,t]))) = 0,"c_quasirational[3] lands in EX^*");
<a name="line_22"></a> _ASSERT(simplify(g0(c_quasirational[4]([s,t]))) = 0,"c_quasirational[4] lands in EX^*");
<a name="line_23"></a>
<a name="line_24"></a> _ASSERT(c_check_E0[0](c_rational[0](t))          = 0.,"c_rational[0] lands in C[0]");
<a name="line_25"></a> _ASSERT(c_check_E0[1](c_rational[1](t))          = 0.,"c_rational[1] lands in C[1]");
<a name="line_26"></a> _ASSERT(c_check_E0[2](c_rational[2](t))          = 0.,"c_rational[2] lands in C[2]");
<a name="line_27"></a> _ASSERT(c_check_E0[1](c_quasirational[1](t))     = 0.,"c_quasirational[1] lands in C[1]");
<a name="line_28"></a> _ASSERT(c_check_E0[2](c_quasirational[2](t))     = 0.,"c_quasirational[2] lands in C[2]");
<a name="line_29"></a> _ASSERT(c_check_E0[3](c_quasirational[3]([s,t])) = 0.,"c_quasirational[3] lands in C[3]");
<a name="line_30"></a> _ASSERT(c_check_E0[4](c_quasirational[4]([s,t])) = 0.,"c_quasirational[4] lands in C[4]");
<a name="line_31"></a>
<a name="line_32"></a> _ASSERT({op(map(type,two_circle(100),[rational,rational]))} = {true},
<a name="line_33"></a>         "two_circle() produces rational pairs");
<a name="line_34"></a>
<a name="line_35"></a> _ASSERT({op(map(u -> 2*u[1]^2 + u[2]^2,two_circle(100)))} = {1},
<a name="line_36"></a>         "two_circle() solves 2 s^2 + t^2 = 1");
<a name="line_37"></a>
<a name="line_38"></a> _ASSERT({op(map(type,three_circle(100),[rational,rational]))} = {true},
<a name="line_39"></a>         "three_circle() produces rational pairs");
<a name="line_40"></a>
<a name="line_41"></a> _ASSERT({op(map(u -> 3*u[1]^2 + u[2]^2,three_circle(100)))} = {1},
<a name="line_42"></a>         "three_circle() solves 3 s^2 + t^2 = 1");
<a name="line_43"></a>
<a name="line_44"></a> _ASSERT(expand(g0(quasirational_lift([s[1],s[2]]))) = 0,
<a name="line_45"></a>         "g0 on quasirational_lift");
<a name="line_46"></a>
<a name="line_47"></a> _ASSERT(expand(rho(quasirational_lift([s[1],s[2]])) - 1) = 0,
<a name="line_48"></a>         "rho on quasirational lift");
<a name="line_49"></a>
<a name="line_50"></a> _ASSERT(expand(quasirational_proj(quasirational_lift([s[1],s[2]])) -~ [s[1],s[2]]) = [0,0],
<a name="line_51"></a>         "quasirational_proj o quasirational_lift");
<a name="line_52"></a>
<a name="line_53"></a> _ASSERT({op(map(type,inner_quasirational_projections,[rational,rational]))} = {true},
<a name="line_54"></a>         "inner_quasirational_projections are rational");
<a name="line_55"></a>
<a name="line_56"></a> _ASSERT({op(map(is_quasirational,inner_quasirational_points))} = {true},
<a name="line_57"></a>         "inner_quasirational_points are quasirational");
<a name="line_58"></a>
<a name="line_59"></a> _ASSERT({op(map(g0,inner_quasirational_points)),
<a name="line_60"></a>          op(map(u -> rho(u) - 1,inner_quasirational_points))} = {0},
<a name="line_61"></a>         "inner_quasirational_points lie in EX^*");
<a name="line_62"></a>end:
<a name="line_63"></a>
<a name="line_64"></a>add_check(check_rational):
<a name="line_65"></a>
<a name="line_66"></a>######################################################################
<a name="line_67"></a>
<a name="line_68"></a>check_rational_elliptic := proc()
<a name="line_69"></a> local s,t;
<a name="line_70"></a> 
<a name="line_71"></a> printf("%a()\n",procname);
<a name="line_72"></a>
<a name="line_73"></a> _ASSERT(simplify(elliptic_r[0](c_E0[5](t))) = 0,
<a name="line_74"></a>         "elliptic rational relation 0");
<a name="line_75"></a>	 
<a name="line_76"></a> _ASSERT(simplify(elliptic_r[1](c_E0[5](t))) = 0,
<a name="line_77"></a>         "elliptic rational relation 1");
<a name="line_78"></a>	 
<a name="line_79"></a> _ASSERT(
<a name="line_80"></a>  expand(
<a name="line_81"></a>   elliptic_rel(elliptic_u(x),elliptic_t(x)) - 
<a name="line_82"></a>    (elliptic_a[0](x) * elliptic_r[0](x) + 
<a name="line_83"></a>     elliptic_a[1](x) * elliptic_r[1](x) + 
<a name="line_84"></a>     elliptic_a[2](x) * elliptic_r[0](x) * elliptic_r[1](x))) = 0,
<a name="line_85"></a>  "elliptic rational relation 2"
<a name="line_86"></a> ); 
<a name="line_87"></a>end:
<a name="line_88"></a>
<a name="line_89"></a>add_check(check_rational_elliptic):
<a name="line_90"></a>
  </pre>
 </body>
</html>
    