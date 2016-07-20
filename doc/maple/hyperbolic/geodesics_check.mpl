<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_geodesic_H := proc()
<a name="line_2"></a> local i,j,err0,err1,err2,A0,A1,A2,A3;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> i,j := 0,3;
<a name="line_7"></a> err0 := simplify(geodesic_H[i,j](geodesic_start_H[i,j]) - v_H[i]):
<a name="line_8"></a> _ASSERT(err0 = 0,"geodesic passes through v[0]");
<a name="line_9"></a>
<a name="line_10"></a> err1 := simplify(geodesic_H[i,j](geodesic_end_H[i,j]) - v_H[j]):
<a name="line_11"></a> _ASSERT(err1 = 0,"geodesic passes through v[3]");
<a name="line_12"></a>
<a name="line_13"></a> err2 := simplify(act_Pi(geodesic_shift_H[i,j],geodesic_H[i,j](t)) -
<a name="line_14"></a>                   geodesic_H[i,j](geodesic_period_H[i,j] + t)):
<a name="line_15"></a> _ASSERT(err2 = 0,"geodesic has shifted periodicity");
<a name="line_16"></a>
<a name="line_17"></a> A0 := -a_H^2*sqrt(a_H^2+1)+sqrt(2*a_H^4-a_H^2+1);
<a name="line_18"></a> A1 := (1/2*( sqrt(2*a_H^4-a_H^2+1)*sqrt(a_H^2+1)-a_H^2+1))*sqrt(2);
<a name="line_19"></a> A2 := a_H^6-2*sqrt(2*a_H^4-a_H^2+1)*sqrt(a_H^2+1)*a_H^2+3*a_H^4-a_H^2+1;
<a name="line_20"></a> A3 := a_H^6-sqrt(2*a_H^4-a_H^2+1)*sqrt(a_H^2+1)*a_H^2+a_H^4+sqrt(2*a_H^4-a_H^2+1)*sqrt(a_H^2+1)-a_H^2+1;
<a name="line_21"></a>
<a name="line_22"></a> _ASSERT(expand(A2 - A0^2) = 0,"auxiliary square root 1");
<a name="line_23"></a> _ASSERT(expand(A3 - A1^2) = 0,"auxiliary square root 2");
<a name="line_24"></a>
<a name="line_25"></a> i,j := 6,11;
<a name="line_26"></a> err0 := simplify(geodesic_H[i,j](geodesic_start_H[i,j]) - v_H[i]):
<a name="line_27"></a> err0 := simplify(subs(sqrt(A2)=A0,err0));
<a name="line_28"></a> _ASSERT(err0 = 0,"geodesic passes through v[6]");
<a name="line_29"></a>
<a name="line_30"></a> err1 := simplify(geodesic_H[i,j](geodesic_end_H[i,j]) - v_H[j]):
<a name="line_31"></a> err1 := simplify(subs(sqrt(A3)=A1,err1));
<a name="line_32"></a> _ASSERT(err1 = 0,"geodesic passes through v[11]");
<a name="line_33"></a>
<a name="line_34"></a> err2 := simplify(act_Pi(geodesic_shift_H[i,j],geodesic_H[i,j](t)) -
<a name="line_35"></a>                   geodesic_H[i,j](geodesic_period_H[i,j] + t)):
<a name="line_36"></a> _ASSERT(err2 = 0,"geodesic has shifted periodicity");
<a name="line_37"></a>end:
<a name="line_38"></a>
<a name="line_39"></a>add_check(check_geodesic_H):  </pre>
 </body>
</html>
    