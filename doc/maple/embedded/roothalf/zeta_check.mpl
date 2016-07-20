<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_zeta := proc()
<a name="line_2"></a> local a1,a2,a3,a4,v1,v2,u,x,xx,t;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> xx := [seq(x[i],i=1..4)];
<a name="line_7"></a> 
<a name="line_8"></a> _ASSERT(
<a name="line_9"></a>  simplify(disc_zeta_proj(c_E0[3](t)) -~ [(1+1/sqrt(2))/sqrt(3)*cos(t),sin(t)]) = [0,0],
<a name="line_10"></a>  "zeta on c[3]"
<a name="line_11"></a> );
<a name="line_12"></a>
<a name="line_13"></a> _ASSERT(
<a name="line_14"></a>  simplify(disc_zeta_q0(disc_zeta_proj(c_E0[3](t)))) = 0,
<a name="line_15"></a>  "image of zeta on c[3]"
<a name="line_16"></a> );
<a name="line_17"></a>
<a name="line_18"></a> _ASSERT(
<a name="line_19"></a>  simplify(disc_zeta_proj(c_E0[6](t)) -~ [2-(1-cos(t))/sqrt(2),sin(t)]/~sqrt(10-2*cos(t))) = [0,0],
<a name="line_20"></a>  "zeta on c[6]"
<a name="line_21"></a> );
<a name="line_22"></a>
<a name="line_23"></a> _ASSERT(
<a name="line_24"></a>  simplify(disc_zeta_q1(disc_zeta_proj(c_E0[6](t)))) = 0,
<a name="line_25"></a>  "image of zeta on c[6]"
<a name="line_26"></a> );
<a name="line_27"></a>
<a name="line_28"></a> _ASSERT(
<a name="line_29"></a>  simplify(disc_zeta_q1(disc_zeta_proj(c_E0[8](t)))) = 0,
<a name="line_30"></a>  "image of zeta on c[8]"
<a name="line_31"></a> );
<a name="line_32"></a>
<a name="line_33"></a> a1 := 34 - 24*sqrt(2);
<a name="line_34"></a> a2 := 40 - 28*sqrt(2);
<a name="line_35"></a> a3 := 10 -  6*sqrt(2);
<a name="line_36"></a> a4 :=  3 -  2*sqrt(2);
<a name="line_37"></a>
<a name="line_38"></a> _ASSERT(min(evalf([a1,a2,a3,a4])) > 0,"coefficients for q1 are positive");
<a name="line_39"></a>
<a name="line_40"></a> v1 := a3*u[1]^2+a4;
<a name="line_41"></a> v2 := a1*u[1]^4+a2*u[1]^2;
<a name="line_42"></a>
<a name="line_43"></a> _ASSERT(
<a name="line_44"></a>  simplify(disc_zeta_q1(u) - ((u[2]^2+v1)^2 - 4*(v2))) = 0,
<a name="line_45"></a>  "alternative formula for q1"
<a name="line_46"></a> );
<a name="line_47"></a>
<a name="line_48"></a> _ASSERT(
<a name="line_49"></a>   {solve(4*v2 - v1^2,u[1])} =
<a name="line_50"></a>   {1/sqrt(3)-1/sqrt(6),1/sqrt(2),-1/sqrt(2),-(1/sqrt(3)-1/sqrt(6))},
<a name="line_51"></a>   "intersection of holes with u[1] axis"
<a name="line_52"></a> );
<a name="line_53"></a>
<a name="line_54"></a> _ASSERT(
<a name="line_55"></a>  simplify(disc_zeta_det(xx) - x[1]*(disc_zeta_q8(y_proj0(xx)) + 
<a name="line_56"></a>	    (2+(3-2*sqrt(2))*x[3]*x[4])*(rho(xx)-1) + (3-2*sqrt(2))/2*x[3]*g0(xx))) = 0,
<a name="line_57"></a>  "zeta determinant formula"
<a name="line_58"></a> );
<a name="line_59"></a>
<a name="line_60"></a> _ASSERT(
<a name="line_61"></a>  expand(
<a name="line_62"></a>   disc_zeta_q5(disc_zeta_proj(xx),x[1]) - 
<a name="line_63"></a>   (sqrt(2)+1)^8*disc_zeta_q2(disc_zeta_proj(xx)) - 
<a name="line_64"></a>   disc_zeta_q6(xx)*(rho(xx)-1)+
<a name="line_65"></a>   disc_zeta_q7(xx)*g0(xx)
<a name="line_66"></a>  ) = 0,
<a name="line_67"></a>  "q5267 identity" 
<a name="line_68"></a> );
<a name="line_69"></a>
<a name="line_70"></a> _ASSERT(
<a name="line_71"></a>  expand(
<a name="line_72"></a>  disc_zeta_q5(u,x[1]) - 
<a name="line_73"></a>  (sqrt(2)+1)^8*disc_zeta_q2(u) - 
<a name="line_74"></a>  (sqrt(2)+1)^4*u[1]^2*disc_zeta_q3(u,x[1]) + 
<a name="line_75"></a>  (1-u[1]^2-u[2]^2-x[1]^2)*disc_zeta_q4(u,x[1])^2 
<a name="line_76"></a>  ) = 0,
<a name="line_77"></a>  "q5234 identity"
<a name="line_78"></a> );
<a name="line_79"></a>end:
<a name="line_80"></a>
<a name="line_81"></a>
<a name="line_82"></a>add_check(check_zeta):
  </pre>
 </body>
</html>
    