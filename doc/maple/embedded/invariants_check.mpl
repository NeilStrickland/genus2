<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_invariants := proc()
<a name="line_2"></a> local n,B,BI,err;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> _ASSERT(
<a name="line_7"></a>  expand(x[1]^2 - eval(subs(y = yx,uy[1])) - (rho(xx)-1)/2 - g_0(xx)*(g_0(xx)+2*(1/a_E+a_E)*x[3]+4*x[4])/8) = 0,
<a name="line_8"></a>  "x[1]^2 = uy[1]"
<a name="line_9"></a> );
<a name="line_10"></a>
<a name="line_11"></a> _ASSERT(
<a name="line_12"></a>  expand(x[2]^2 - eval(subs(y = yx,uy[2])) - (rho(xx)-1)/2 - g_0(xx)*(g_0(xx)-2*(1/a_E+a_E)*x[3]+4*x[4])/8) = 0,
<a name="line_13"></a>  "x[2]^2 = uy[2]"
<a name="line_14"></a> );
<a name="line_15"></a>
<a name="line_16"></a> _ASSERT(
<a name="line_17"></a>   zx[3] = x[1]^2 + x[2]^2 and
<a name="line_18"></a>   expand(1/4*zx[3]^2-a_E^2*zx[2]-(a_E+1/a_E)/2*zx[5]-(a_E+1/a_E)^2/4*zx[1]*zx[4]) = x[1]^2*x[2]^2 and
<a name="line_19"></a>   zx[1] = x[3]^2 and
<a name="line_20"></a>   zx[4] = x[4]^2 and
<a name="line_21"></a>   zx[5] + (a_E+1/a_E)*zx[1]*zx[4] = (x[2]^2-x[1]^2)*x[3]*x[4],
<a name="line_22"></a>   "Basic invariants on R4 in terms of z_1,..,z_5"
<a name="line_23"></a> );
<a name="line_24"></a>
<a name="line_25"></a> _ASSERT(
<a name="line_26"></a>  expand(zx[5]^2-4*a_E^2*zx[1]*zx[2]*zx[4]) = 0, 
<a name="line_27"></a>  "z_5^2 relation"
<a name="line_28"></a> );
<a name="line_29"></a>
<a name="line_30"></a> _ASSERT(
<a name="line_31"></a>  expand(zx[3] - (1  - zx[1] - zx[1]*zx[2]) - (rho(xx)-1) - (g_0(xx)/4 + x[4])*g_0(xx)) = 0 and 
<a name="line_32"></a>  expand(zx[4] - zx[1] * zx[2] + (g_0(xx)/4+x[4])*g_0(xx)) = 0 and
<a name="line_33"></a>  expand(zx[5] + 2 * a_E * zx[1] * zx[2] + a_E*yx[1]*yx[2]*g_0(xx)) = 0,
<a name="line_34"></a>  "z_3,..,z_5 in terms of z_1,z_2 on X"
<a name="line_35"></a> );
<a name="line_36"></a>
<a name="line_37"></a> _ASSERT(
<a name="line_38"></a>  simplify(y_proj(y_lift([y[1],y[2]])) -~ [y[1],y[2]]) = [0,0],
<a name="line_39"></a>  "y_proj o y_lift = 1"
<a name="line_40"></a> );
<a name="line_41"></a>
<a name="line_42"></a> _ASSERT(
<a name="line_43"></a>  simplify(z_proj(z_lift([z[1],z[2]])) -~ [z[1],z[2]]) = [0,0],
<a name="line_44"></a>  "z_proj o z_lift = 1"
<a name="line_45"></a> );
<a name="line_46"></a>
<a name="line_47"></a> _ASSERT(
<a name="line_48"></a>  NF_x(square_norm_of_dg(xx) - square_norm_of_dg_z(zx)) = 0,
<a name="line_49"></a>  "square norm of dg in terms of z"
<a name="line_50"></a> );
<a name="line_51"></a>
<a name="line_52"></a> _ASSERT(
<a name="line_53"></a>  NF_x(numer(factor(curvature(xx) - curvature_z(zx)))) = 0,
<a name="line_54"></a>  "curvature in terms of z"
<a name="line_55"></a> );
<a name="line_56"></a>
<a name="line_57"></a> _ASSERT(
<a name="line_58"></a>  [(simplify(subs(y[1] = w_to_y_y_1(w[1],y[2]),wy[1] - w[1])) assuming y[2] >= 0),
<a name="line_59"></a>   (simplify(subs(y[1] = w_to_y_y_1(w[1],y[2]),wy[2] - w_to_y_p(y[2],w[1]))) assuming y[2] >= 0),
<a name="line_60"></a>   expand(numer(simplify(diff(w_to_y_p(t,w1),t))) - (add(w_to_y_dpc[k]*t^k,k=0..4)*(4*a_E^4)))
<a name="line_61"></a>  ] = 
<a name="line_62"></a>  [0,0,0],
<a name="line_63"></a>  "properties of w_to_y()"
<a name="line_64"></a> );
<a name="line_65"></a>
<a name="line_66"></a> err := factor(subs(y[1] = w_to_y_y_1(w[1],y[2]),
<a name="line_67"></a>   uy[1] - 
<a name="line_68"></a>   (((1-a_E^2)*w[1]+1/2)*y[2]^2 + 
<a name="line_69"></a>    ((1-w[1])/2*(a_E+1/a_E) + w[1]*(1/a_E-a_E))*y[2] + 
<a name="line_70"></a>    (1-w[1])/2) *
<a name="line_71"></a>   (1-w_to_y_p(y[2],wy[1])) / ((y[2]+a_E)*(y[2]+1/a_E))
<a name="line_72"></a>  ));
<a name="line_73"></a>
<a name="line_74"></a>  err := simplify(err) assuming y[2]>0;
<a name="line_75"></a>
<a name="line_76"></a> _ASSERT(err = 0,"u[1] in w_to_y()");
<a name="line_77"></a>
<a name="line_78"></a> err := factor(subs(y[1] = w_to_y_y_1(w[1],y[2]),
<a name="line_79"></a>   uy[2]- ((1+2*a_E*y[2])*(1-w[1])/2)
<a name="line_80"></a>  ));
<a name="line_81"></a>
<a name="line_82"></a>  err := simplify(err) assuming y[2]>0;
<a name="line_83"></a>
<a name="line_84"></a> _ASSERT(err = 0, "u[2] in w_to_y");
<a name="line_85"></a>
<a name="line_86"></a> _ASSERT(
<a name="line_87"></a>  {seq(simplify(y_proj(act_R4[T](xx)) -~ act_hex[T](y_proj(xx))),T in G16)} = {[0,0]},
<a name="line_88"></a>  "y_proj is equivariant"
<a name="line_89"></a> );
<a name="line_90"></a>
<a name="line_91"></a> _ASSERT(
<a name="line_92"></a>  {seq(seq(simplify(
<a name="line_93"></a>    act_A[T](twisted_invariant[i]) - character[i][T] * twisted_invariant[i]
<a name="line_94"></a>   ),T in G16),i=0..7)} = {0},
<a name="line_95"></a>  "twisted invariants"
<a name="line_96"></a> );
<a name="line_97"></a>end:
<a name="line_98"></a>
<a name="line_99"></a>add_check(check_invariants):
<a name="line_100"></a>
<a name="line_101"></a>######################################################################
<a name="line_102"></a>
<a name="line_103"></a>check_square := proc()
<a name="line_104"></a> local T,i,p,err;
<a name="line_105"></a>
<a name="line_106"></a> printf("%a()\n",procname);
<a name="line_107"></a> 
<a name="line_108"></a> for T in G16 do
<a name="line_109"></a>  _ASSERT(simplify(w_proj(act_R4[T](xx)) -~ w_proj(xx)) = [0,0],
<a name="line_110"></a>	 sprintf("w_proj invariant for %a",T));
<a name="line_111"></a> od:
<a name="line_112"></a>
<a name="line_113"></a> _ASSERT(map(limit,w_proj(c_E[1](t)),t= 0)=[1,0],"w_proj(v[0]) 0");
<a name="line_114"></a> _ASSERT(map(limit,w_proj(c_E[2](t)),t= 0)=[1,0],"w_proj(v[0]) 1");
<a name="line_115"></a> _ASSERT(map(limit,w_proj(c_E[5](t)),t= 0)=[1,0],"w_proj(v[0]) 2");
<a name="line_116"></a> _ASSERT(map(limit,w_proj(c_E[6](t)),t= 0)=[1,0],"w_proj(v[0]) 3");
<a name="line_117"></a>
<a name="line_118"></a> _ASSERT(map(limit,w_proj(c_E[1](t)),t=Pi)=[1,0],"w_proj(v[1]) 0");
<a name="line_119"></a> _ASSERT(map(limit,w_proj(c_E[2](t)),t=Pi)=[1,0],"w_proj(v[1]) 1");
<a name="line_120"></a> _ASSERT(map(limit,w_proj(c_E[7](t)),t= 0)=[1,0],"w_proj(v[1]) 2");
<a name="line_121"></a> _ASSERT(map(limit,w_proj(c_E[8](t)),t= 0)=[1,0],"w_proj(v[1]) 3");
<a name="line_122"></a>
<a name="line_123"></a> for i from 2 to 5 do
<a name="line_124"></a>  _ASSERT(simplify(w_proj(v_E[i]) -~ [0,1]) = [0,0],sprintf("w_proj(v[%d])",i));
<a name="line_125"></a> od:
<a name="line_126"></a>
<a name="line_127"></a> for i from 6 to 9 do
<a name="line_128"></a>  _ASSERT(simplify(w_proj(v_E[i])) = [0,0],sprintf("w_proj(v[%d])",i));
<a name="line_129"></a> od:
<a name="line_130"></a>
<a name="line_131"></a> for i from 10 to 13 do
<a name="line_132"></a>  _ASSERT(simplify(w_proj(v_E[i]) -~ [1,1]) = [0,0],sprintf("w_proj(v[%d])",i));
<a name="line_133"></a> od:
<a name="line_134"></a>
<a name="line_135"></a> for i from 14 to 21 do
<a name="line_136"></a>  _ASSERT(simplify(w_proj(v_E[i]) -~ [2*a_E^2/(a_E^2+1), 0]) = [0,0],sprintf("w_proj(v[%d])",i));
<a name="line_137"></a> od:
<a name="line_138"></a>
<a name="line_139"></a> for i from 22 to 29 do
<a name="line_140"></a>  _ASSERT(simplify(w_proj(v_E[i]) -~ [1, (2*a_E^2-5)/(6*a_E^2-7)]) = [0,0],sprintf("w_proj(v[%d])",i));
<a name="line_141"></a> od:
<a name="line_142"></a>
<a name="line_143"></a> for i from 30 to 45 do
<a name="line_144"></a>  _ASSERT(simplify(w_proj(v_E[i]) -~ [(3/4)*(2*a_E^2+1)/(a_E^2+1), (1/4)*(2*a_E^2-3)/((a_E-1)*(a_E+1))]) = [0,0],
<a name="line_145"></a>    sprintf("w_proj(v[%d])",i));
<a name="line_146"></a> od:
<a name="line_147"></a>end:
<a name="line_148"></a>
<a name="line_149"></a>add_check(check_square):
<a name="line_150"></a>
  </pre>
 </body>
</html>
    