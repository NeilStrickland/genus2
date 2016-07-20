<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_annular_charts := proc()
<a name="line_2"></a> local A0,A1,A2;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> A0 := annular_chart[0]([t,u]);
<a name="line_7"></a> A1 := map(diff,A0,t);
<a name="line_8"></a> A2 := map(diff,A0,u);
<a name="line_9"></a>
<a name="line_10"></a> _ASSERT(
<a name="line_11"></a>  convert(series(combine(simplify(rho(A0)-1)),u=0,4),polynom,u) = 0,
<a name="line_12"></a>  "annular_chart[0] lands in S3"
<a name="line_13"></a> );
<a name="line_14"></a>
<a name="line_15"></a> _ASSERT(
<a name="line_16"></a>  convert(series(combine(simplify(g(A0))),u=0,5),polynom,u) = 0,
<a name="line_17"></a>  "annular_chart[0] lands in EX(a)"
<a name="line_18"></a> );
<a name="line_19"></a>
<a name="line_20"></a> _ASSERT(
<a name="line_21"></a>  convert(series(combine(simplify(dp4(A1,A2))),u=0,3),polynom,u) = 0,
<a name="line_22"></a>  "annular_chart[0] is approximately conformal 1"
<a name="line_23"></a> );
<a name="line_24"></a>
<a name="line_25"></a> _ASSERT(
<a name="line_26"></a>  convert(series(combine(simplify(dp4(A1,A1)-dp4(A2,A2))),u=0,4),polynom,u) = 0,
<a name="line_27"></a>  "annular_chart[0] is approximately conformal 2"
<a name="line_28"></a> );
<a name="line_29"></a>
<a name="line_30"></a> A0 := annular_chart[1]([t,u]);
<a name="line_31"></a> A1 := map(diff,A0,t);
<a name="line_32"></a> A2 := map(diff,A0,u);
<a name="line_33"></a>
<a name="line_34"></a> _ASSERT(
<a name="line_35"></a>  convert(series(combine(simplify(rho(A0)-1)),u=0,4),polynom,u) = 0,
<a name="line_36"></a>  "annular_chart[1] lands in S3"
<a name="line_37"></a> );
<a name="line_38"></a>
<a name="line_39"></a> _ASSERT(
<a name="line_40"></a>  convert(series(combine(simplify(g(A0))),u=0,3),polynom,u) = 0,
<a name="line_41"></a>  "annular_chart[1] lands in EX(a)"
<a name="line_42"></a> );
<a name="line_43"></a>
<a name="line_44"></a> _ASSERT(
<a name="line_45"></a>  combine(simplify(dp4(A1,A2))) = 0,
<a name="line_46"></a>  "annular_chart[1] is approximately conformal 1"
<a name="line_47"></a> );
<a name="line_48"></a>
<a name="line_49"></a> _ASSERT(
<a name="line_50"></a>  convert(series(combine(simplify(dp4(A1,A1)-dp4(A2,A2))),u=0,2),polynom,u) = 0,
<a name="line_51"></a>  "annular_chart[1] is approximately conformal 2"
<a name="line_52"></a> );
<a name="line_53"></a>
<a name="line_54"></a>end:
<a name="line_55"></a>
<a name="line_56"></a>add_check(check_annular_charts);
  </pre>
 </body>
</html>
    