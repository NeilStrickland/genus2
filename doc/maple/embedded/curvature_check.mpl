<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># Check that the sphere of radius r has curvature 1/r^2
<a name="line_2"></a>
<a name="line_3"></a>check_sphere_curvature := proc()
<a name="line_4"></a> local r,t,u;
<a name="line_5"></a>
<a name="line_6"></a> printf("%a()\n",procname);
<a name="line_7"></a>
<a name="line_8"></a> _ASSERT(
<a name="line_9"></a>  factor(simplify(brioschi_from_embedding([t,u,sqrt(r^2-t^2-u^2)],t,u) - 1/r^2)) = 0,
<a name="line_10"></a>  "brioschi curvature of sphere 1"
<a name="line_11"></a> );
<a name="line_12"></a>
<a name="line_13"></a> _ASSERT(
<a name="line_14"></a>  factor(simplify(brioschi_from_embedding([r*cos(t)*cos(u),r*cos(t)*sin(u),r*sin(t)],t,u) - 1/r^2)) = 0,
<a name="line_15"></a>  "brioschi curvature of sphere 2"
<a name="line_16"></a> );
<a name="line_17"></a>
<a name="line_18"></a> _ASSERT(
<a name="line_19"></a>  factor(simplify(christoffel_from_embedding([t,u,sqrt(r^2-t^2-u^2)],t,u) - 1/r^2)) = 0,
<a name="line_20"></a>  "christoffel curvature of sphere 1"
<a name="line_21"></a> );
<a name="line_22"></a>
<a name="line_23"></a> _ASSERT(
<a name="line_24"></a>  factor(simplify(christoffel_from_embedding([r*cos(t)*cos(u),r*cos(t)*sin(u),r*sin(t)],t,u) - 1/r^2)) = 0,
<a name="line_25"></a>  "christoffel curvature of sphere 2"
<a name="line_26"></a> );
<a name="line_27"></a>end:
<a name="line_28"></a>
<a name="line_29"></a>add_check(check_sphere_curvature):
<a name="line_30"></a>
<a name="line_31"></a>######################################################################
<a name="line_32"></a>
<a name="line_33"></a># Check that our extrinsic formula for the curvature of EX(a) is
<a name="line_34"></a># compatible with the above intrinsic formulae
<a name="line_35"></a>
<a name="line_36"></a>check_EX_curvature := proc()
<a name="line_37"></a> local z0,K0,K1,t,u,err;
<a name="line_38"></a>
<a name="line_39"></a> printf("%a()\n",procname);
<a name="line_40"></a>
<a name="line_41"></a> _ASSERT(
<a name="line_42"></a>  simplify(brioschi_from_embedding(y_lift([s[1],s[2]]),s[1],s[2]) -
<a name="line_43"></a>           curvature(y_lift([s[1],s[2]]))) = 0,
<a name="line_44"></a>  "curvature of EX(a)"
<a name="line_45"></a> );
<a name="line_46"></a>
<a name="line_47"></a> err := simplify(curvature(xx) - curvature_z([zx[1],zx[2]])):
<a name="line_48"></a> err := NF_x(numer(err));
<a name="line_49"></a>
<a name="line_50"></a> _ASSERT(err = 0,"curvature of EX(a) in terms of z");
<a name="line_51"></a>end:
<a name="line_52"></a>
<a name="line_53"></a>add_check(check_EX_curvature):
<a name="line_54"></a>
<a name="line_55"></a>######################################################################
<a name="line_56"></a>
<a name="line_57"></a>check_general_hyperbolic := proc()
<a name="line_58"></a> local N,E,E0,E1,K1,t,u;
<a name="line_59"></a>
<a name="line_60"></a> N := 7;
<a name="line_61"></a>
<a name="line_62"></a> E0 := add(add(E[i,j]*t^i*u^j,j=0..N-i),i=0..N);
<a name="line_63"></a> E1 := general_hyperbolic_metric(E0,t,u,N);
<a name="line_64"></a> K1 := multi_series(brioschi_formula(E1,0,E1,t,u),N-2,t,u);
<a name="line_65"></a>
<a name="line_66"></a> _ASSERT(K1 = -1,"general hyperbolic metric has curvature -1");
<a name="line_67"></a>end:
<a name="line_68"></a>
<a name="line_69"></a>add_check(check_general_hyperbolic);  </pre>
 </body>
</html>
    