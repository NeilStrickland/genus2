<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_R4_action := proc()
<a name="line_2"></a> local T,U,x;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> _ASSERT(
<a name="line_7"></a>  `and`(seq(seq(
<a name="line_8"></a>   act_R4[T](act_R4[U](x)) = act_R4[G_mult(T,U)](x),
<a name="line_9"></a>    U in G16),T in G16)),
<a name="line_10"></a>  "action on R^4 is associative"
<a name="line_11"></a> );
<a name="line_12"></a>
<a name="line_13"></a>NULL;
<a name="line_14"></a>end:
<a name="line_15"></a>
<a name="line_16"></a>add_check(check_R4_action):
<a name="line_17"></a>
<a name="line_18"></a>######################################################################
<a name="line_19"></a>
<a name="line_20"></a>check_smoothness := proc()
<a name="line_21"></a> local x,xx,n,p,q,sols;
<a name="line_22"></a>
<a name="line_23"></a> printf("%a()\n",procname);
<a name="line_24"></a>
<a name="line_25"></a> xx := [x[1],x[2],x[3],x[4]];
<a name="line_26"></a> n := dg(xx);
<a name="line_27"></a> p := (1-a_E^2)/16*(n[1]^2+n[2]^2) + a_E/2*(x[1]^2-x[2]^2)*n[3] - (x[1]^2+x[2]^2)/4*n[4];
<a name="line_28"></a> q := x[1]^4 + x[2]^4 + (5/2 - a_E^2)*(x[1]^2+x[2]^2)*x[4]^2;
<a name="line_29"></a>
<a name="line_30"></a> _ASSERT(expand(p-q) = 0,"X is smooth 0");
<a name="line_31"></a>
<a name="line_32"></a> sols := {solve(subs({x[1]=0,x[2]=0},{op(n),rho(xx)-1}))};
<a name="line_33"></a> sols := select(s -> (subs(s,a_E) > 0 and subs(s,a_E) < 1),sols);
<a name="line_34"></a>
<a name="line_35"></a> _ASSERT(sols = {},"X is smooth 1");
<a name="line_36"></a> NULL;
<a name="line_37"></a>end:
<a name="line_38"></a>
<a name="line_39"></a>add_check(check_smoothness):
<a name="line_40"></a>
<a name="line_41"></a>######################################################################
<a name="line_42"></a>
<a name="line_43"></a>check_PEX_smoothness := proc()
<a name="line_44"></a> local x,r;
<a name="line_45"></a>
<a name="line_46"></a> r[ 1] := expand(-1/8*(x[2]*dg(x)[1]+x[1]*dg(x)[2]));
<a name="line_47"></a> r[ 2] := expand(a_E/4*(x[2]*dg(x)[1]-x[1]*dg(x)[2]));
<a name="line_48"></a> r[ 3] := collect(expand(- a_E*dg(x)[3]/2 - dg(x)[4]/4),xx);
<a name="line_49"></a> r[ 4] := collect(expand(  a_E*dg(x)[3]/2 - dg(x)[4]/4),xx);
<a name="line_50"></a> r[ 5] := expand(x[1]*x[2]*r[3] - ( x[4]*a_E-x[4]/a_E+(a_E^2-1)/(4*a_E^2)*x[3])*r[2] - 3*x[4]/2*r[1] );
<a name="line_51"></a> r[ 6] := expand(x[1]*x[2]*r[4] - (-x[4]*a_E+x[4]/a_E+(a_E^2-1)/(4*a_E^2)*x[3])*r[2] - 3*x[4]/2*r[1] );
<a name="line_52"></a> r[ 7] := factor(collect(expand(x[1]*r[3] - ((1/8)*(a_E^2-1)*x[3]/a_E+(3/4)*x[4]*(a_E^2-1))*dg(x)[1]),x[3]));
<a name="line_53"></a> r[ 8] := factor(collect(expand(x[2]*r[4] - (-(1/8)*(a_E^2-1)*x[3]/a_E+(3/4)*x[4]*(a_E^2-1))*dg(x)[2]),x[3]));
<a name="line_54"></a>
<a name="line_55"></a> # NB the next two lines divide by 2*a_E^2 - 1, which is zero in 
<a name="line_56"></a> # the special case a_E = 1/sqrt(2).  All other denominators are
<a name="line_57"></a> # invertible for all a_E in (0,1).
<a name="line_58"></a> r[ 9] := factor(expand((2/3)*(x[1]^2*r[7]-x[2]*r[6])/(2*a_E^2-1)));
<a name="line_59"></a> r[10] := factor(expand((2/3)*(x[2]^2*r[8]-x[1]*r[5])/(2*a_E^2-1)));
<a name="line_60"></a>
<a name="line_61"></a> r[11] := expand(dg(x)[1]*a_E^2/2*x[1]^2*( x[3]/a_E+2*x[4]) + 4*a_E^2*r[ 9]);
<a name="line_62"></a> r[12] := expand(dg(x)[2]*a_E^2/2*x[2]^2*(-x[3]/a_E+2*x[4]) + 4*a_E^2*r[10]);
<a name="line_63"></a> r[13] := expand( a_E/2*dg(x)[1]*x[1]^2*x[4]+2*a_E*r[ 9]);
<a name="line_64"></a> r[14] := expand(-a_E/2*dg(x)[2]*x[2]^2*x[4]-2*a_E*r[10]);
<a name="line_65"></a> r[15] := expand(x[1]^3*r[4]+(1/4*(1/a_E^2-1))*r[11]-3/2*r[ 9]+(a_E-1/a_E)*r[13]);
<a name="line_66"></a> r[16] := expand(x[2]^3*r[3]+(1/4*(1/a_E^2-1))*r[12]-3/2*r[10]-(a_E-1/a_E)*r[14]);
<a name="line_67"></a> r[17] := expand(1/12*x[3]*dg(x)[3]-1/6*x[4]*dg(x)[4]);
<a name="line_68"></a> r[18] := expand(r[17] * (2*x[4]^3 - r[17]) + 1/9*x[1]*r[9] + 1/9*x[2]*r[10] + 1/18/a_E*x[1]*r[13] - 1/18/a_E*x[2]*r[14]-r[2]^2/72/a_E^2+x[1]*r[11]/144/a_E^2+x[2]*r[12]/144/a_E^2+2/9*r[1]^2);
<a name="line_69"></a> r[19] := collect(expand((1/a_E^2-1)*x[3]*dg(x)[4]+3*x[4]*dg(x)[3]),xx);
<a name="line_70"></a>
<a name="line_71"></a> _ASSERT(r[15] = x[1]^5 and
<a name="line_72"></a>         r[16] = x[2]^5 and
<a name="line_73"></a>         r[18] = x[4]^6 and
<a name="line_74"></a>         expand(subs({x[1]=0,x[2]=0,x[4]=0},r[19]) - (1/a_E^2-1)^2*x[3]^3) = 0,
<a name="line_75"></a>         "PEX(a) is smooth except when a = 1/sqrt(2)");
<a name="line_76"></a> NULL;
<a name="line_77"></a>end:
<a name="line_78"></a>
<a name="line_79"></a>######################################################################
<a name="line_80"></a>
<a name="line_81"></a>check_symmetry := proc()
<a name="line_82"></a> global xx;
<a name="line_83"></a>
<a name="line_84"></a> printf("%a()\n",procname);
<a name="line_85"></a>
<a name="line_86"></a> _ASSERT(simplify(rho(act_R4[L](xx)) - rho(xx))=0,"lambda preserves S3");
<a name="line_87"></a> _ASSERT(simplify(rho(act_R4[M](xx)) - rho(xx))=0,"mu preserves S3");
<a name="line_88"></a> _ASSERT(simplify(rho(act_R4[N](xx)) - rho(xx))=0,"nu preserves S3");
<a name="line_89"></a> _ASSERT(simplify(g(act_R4[L](xx)) + g(xx))=0,"lambda preserves X");
<a name="line_90"></a> _ASSERT(simplify(g(act_R4[M](xx)) + g(xx))=0,"mu preserves X");
<a name="line_91"></a> _ASSERT(simplify(g(act_R4[N](xx)) - g(xx))=0,"nu preserves X");
<a name="line_92"></a>
<a name="line_93"></a> NULL;
<a name="line_94"></a>end:
<a name="line_95"></a>
<a name="line_96"></a>add_check(check_symmetry):
<a name="line_97"></a>
<a name="line_98"></a>######################################################################
<a name="line_99"></a>
<a name="line_100"></a>check_fixed_points := proc()
<a name="line_101"></a> local T,sols,FP0,FP1,i,s;
<a name="line_102"></a>
<a name="line_103"></a> printf("%a()\n",procname);
<a name="line_104"></a>
<a name="line_105"></a> for T in {op(G8)} minus {1} do
<a name="line_106"></a>  sols := solve({rho(xx)=1,g_0(xx)=0,seq(act_R4[T](xx)[i]=xx[i],i=1..4)},{op(xx)});
<a name="line_107"></a>  FP0 := map(simplify,map(s -> subs(s,xx),{sols}));
<a name="line_108"></a>  FP1 := map(simplify,map(i -> v_E[i],{op(fixed_vertices[T])}));
<a name="line_109"></a>
<a name="line_110"></a>  # Substitute a fairly generic value for a_E.  Should be replaced by 
<a name="line_111"></a>  # something more rigorous.
<a name="line_112"></a>  FP0 := simplify(subs(a_E=1/7,FP0));
<a name="line_113"></a>  FP1 := simplify(subs(a_E=1/7,FP1));
<a name="line_114"></a>
<a name="line_115"></a>  _ASSERT(FP0 = FP1,sprintf("fixed points for %a",T));
<a name="line_116"></a> od:
<a name="line_117"></a> NULL;
<a name="line_118"></a>end:
<a name="line_119"></a>
<a name="line_120"></a>add_check(check_fixed_points):
<a name="line_121"></a>
<a name="line_122"></a>######################################################################
<a name="line_123"></a>
<a name="line_124"></a>check_E_F4 := proc()
<a name="line_125"></a> local T,i,err,r,x1;
<a name="line_126"></a> global x,xx;
<a name="line_127"></a>
<a name="line_128"></a> printf("%a()\n",procname);
<a name="line_129"></a>
<a name="line_130"></a> err := simplify(rho(retract_F4_E(xx)) - rho(xx)) assuming real;
<a name="line_131"></a> _ASSERT(err = 0,"retract_F4_E preserves S3");
<a name="line_132"></a>
<a name="line_133"></a> err := simplify(g(retract_F4_E(xx)) - g(xx)) assuming real;
<a name="line_134"></a> _ASSERT(err = 0,"retract_F4_E preserves EX(a)");
<a name="line_135"></a>
<a name="line_136"></a> for T in H4 do
<a name="line_137"></a>  _ASSERT(simplify(retract_F4_E(act_R4[T](xx)) -~ retract_F4_E(xx)) = [0$4],
<a name="line_138"></a>         sprintf("retract_F4_E is %a-invariant",T));
<a name="line_139"></a> od:
<a name="line_140"></a>
<a name="line_141"></a> for i from 0 to 8 do
<a name="line_142"></a>  err := add(evalf(Int(is_in_F4_E0_measure(c_E1[i](t)),t=r)),r in [F4_curve_limits[i]]);
<a name="line_143"></a>  _ASSERT(err < 10^(-8),sprintf("F4_curve_limits[%d]",i));
<a name="line_144"></a> od;
<a name="line_145"></a>
<a name="line_146"></a> _ASSERT(
<a name="line_147"></a>  simplify(y_proj(y_lift([y[1],y[2]])) -~ [y[1],y[2]]) = [0,0],
<a name="line_148"></a>  "y_proj o y_lift"
<a name="line_149"></a> );
<a name="line_150"></a>
<a name="line_151"></a> x1 := y_lift(y_proj(x));
<a name="line_152"></a> 
<a name="line_153"></a> _ASSERT(
<a name="line_154"></a>  NF_x([x1[1]^2-x[1]^2,x1[2]^2-x[2]^2,x1[3]-x[3],x1[4]-x[4]]) = [0$4],
<a name="line_155"></a>  "y_lift o y_proj"
<a name="line_156"></a> );
<a name="line_157"></a>
<a name="line_158"></a> _ASSERT(
<a name="line_159"></a>  simplify(y_proj(c_algebraic(t)) -~ 
<a name="line_160"></a>   [sqrt((1-2*a_E*t)/((t-a_E)*(t-1/a_E))),t]) = [0,0],
<a name="line_161"></a>  "y_proj o c_alg"
<a name="line_162"></a> );
<a name="line_163"></a>
<a name="line_164"></a> NULL;
<a name="line_165"></a>end:
<a name="line_166"></a>
<a name="line_167"></a>add_check(check_E_F4):
<a name="line_168"></a>
<a name="line_169"></a>######################################################################
<a name="line_170"></a>
<a name="line_171"></a>check_E_F16 := proc()
<a name="line_172"></a> local T,i,err,a;
<a name="line_173"></a> local x,xx;
<a name="line_174"></a>
<a name="line_175"></a> printf("%a()\n",procname);
<a name="line_176"></a>
<a name="line_177"></a> xx := [x[1],x[2],x[3],x[4]];
<a name="line_178"></a>
<a name="line_179"></a> _ASSERT(expand(g_0(xx) - (-2*x[4] - x[3]/a_E*g_1(xx))) = 0,"g_1 formula");
<a name="line_180"></a> 
<a name="line_181"></a> _ASSERT(
<a name="line_182"></a>  expand(x[4] - (-g_0(xx)/2-x[3]*g_1(xx)/(2*a_E))) = 0,
<a name="line_183"></a>  "x[4] is in (g_0,g_1)"
<a name="line_184"></a> );
<a name="line_185"></a>
<a name="line_186"></a> _ASSERT(
<a name="line_187"></a>  expand(x[2]^2 - x[1]^2 - ((1 - (1+1/a_E^2)/2*x[3]^2)*g_1(xx)-(a_E+1/a_E)/2*x[3]*g_0(xx))) = 0,
<a name="line_188"></a>  "x[2]^2 - x[1]^2 is in (g0,g1)"
<a name="line_189"></a> );
<a name="line_190"></a>
<a name="line_191"></a> for T in H8 do 
<a name="line_192"></a>  _ASSERT(g_1(act_R4[T](xx)) = g_1(xx),"g_1, H8");
<a name="line_193"></a> od:
<a name="line_194"></a>
<a name="line_195"></a> for T in {op(G16)} minus {op(H8)} do 
<a name="line_196"></a>  _ASSERT(g_1(act_R4[T](xx)) = -g_1(xx),"g1, H8^c");
<a name="line_197"></a> od:
<a name="line_198"></a> NULL;
<a name="line_199"></a>end:
<a name="line_200"></a>
<a name="line_201"></a>add_check(check_E_F16):
<a name="line_202"></a>
<a name="line_203"></a>######################################################################
<a name="line_204"></a>
<a name="line_205"></a>check_retract_F16_E := proc()
<a name="line_206"></a> local x,xx,T,i,err,a,samples;
<a name="line_207"></a>
<a name="line_208"></a> printf("%a()\n",procname);
<a name="line_209"></a>
<a name="line_210"></a> samples := [seq(seq(act_R4[T](x),T in G16),x in inner_quasirational_points)]:
<a name="line_211"></a>
<a name="line_212"></a> _ASSERT({op(map(rho,map(retract_F16_E0,samples)))} = {1} and
<a name="line_213"></a>         {op(map(g0,map(retract_F16_E0,samples)))} = {0},
<a name="line_214"></a>         "retract_F16_E preserves X"
<a name="line_215"></a> );
<a name="line_216"></a>
<a name="line_217"></a> _ASSERT(`and`(op(map(is_in_F16_E0,map(retract_F16_E0,samples)))),
<a name="line_218"></a>         "retract_F16_E maps into F16"
<a name="line_219"></a> );
<a name="line_220"></a>
<a name="line_221"></a> _ASSERT(
<a name="line_222"></a>  `and`(seq(seq(evalb(simplify(
<a name="line_223"></a>         retract_F16_E0(act_R4[T](x)) -~ retract_F16_E0(x)) = [0$4]),
<a name="line_224"></a>        x in samples),T in G16)),
<a name="line_225"></a>  "retract_F16_E is G16-invariant"
<a name="line_226"></a> );
<a name="line_227"></a> NULL;
<a name="line_228"></a>end:
<a name="line_229"></a>
<a name="line_230"></a>add_check(check_retract_F16_E):
<a name="line_231"></a>
<a name="line_232"></a>######################################################################
<a name="line_233"></a>
<a name="line_234"></a>check_c_param_E := proc()
<a name="line_235"></a> local err;
<a name="line_236"></a>
<a name="line_237"></a> printf("%a()\n",procname);
<a name="line_238"></a>
<a name="line_239"></a> err := max([seq(seq(seq(
<a name="line_240"></a>         abs(evalf(subs({a_E=0.1*i,t=j*Pi/12},c_param_E[k](c_E[k](t)) - t))),
<a name="line_241"></a>	  j=-11..11),i=1..9),k=0..8)]);
<a name="line_242"></a>
<a name="line_243"></a> _ASSERT(err < 10.^(-98),"c_param_E");
<a name="line_244"></a> NULL;
<a name="line_245"></a>end:
<a name="line_246"></a>
<a name="line_247"></a>add_check(check_c_param_E):
<a name="line_248"></a>
<a name="line_249"></a>
<a name="line_250"></a>
  </pre>
 </body>
</html>
    