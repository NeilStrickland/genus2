<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_r_rels := proc()
<a name="line_2"></a> local errs;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> errs := simplify({
<a name="line_7"></a>  subs(y[2] = y2r[1],ry[1]) - r[1],
<a name="line_8"></a>  subs(y[2] = y2r[2],ry[2]) - r[2],
<a name="line_9"></a>  subs(r[1] = ry[1],y2r[1]) - y[2],
<a name="line_10"></a>  subs(r[2] = ry[2],y2r[2]) - y[2]
<a name="line_11"></a> }) assuming r[1] > 0 and r[2] > 0;
<a name="line_12"></a>
<a name="line_13"></a> _ASSERT(errs = {0},"Relations involving r[1] and r[2]");
<a name="line_14"></a> 
<a name="line_15"></a> errs :=
<a name="line_16"></a>  expand(map(e -> subs({r[1]=rx[1],r[2]=rx[2],y[1]=yx0[1],y[2]=yx0[2]},
<a name="line_17"></a>          rhs(e)^2 - lhs(e)^2),root_rule));
<a name="line_18"></a>
<a name="line_19"></a> _ASSERT(errs = {0},"Relations for rewriting square roots in terms of r[1] and r[2]");
<a name="line_20"></a>
<a name="line_21"></a>end:
<a name="line_22"></a>
<a name="line_23"></a>add_check(check_r_rels):
<a name="line_24"></a>
<a name="line_25"></a>######################################################################
<a name="line_26"></a>
<a name="line_27"></a>check_t_proj := proc()
<a name="line_28"></a> printf("%a()\n",procname);
<a name="line_29"></a>
<a name="line_30"></a> _ASSERT(simplify(rho(t_lift(t)) - 1) = 0 and
<a name="line_31"></a>         simplify(g0(t_lift(t))) = 0,
<a name="line_32"></a>  "t_lift lands in EX(1/sqrt(2))"
<a name="line_33"></a> );
<a name="line_34"></a>
<a name="line_35"></a> _ASSERT(simplify(t_proj(t_lift(t)) -~ [t[1],t[2]]) = [0,0],
<a name="line_36"></a>         "t_proj o t_lift");
<a name="line_37"></a>
<a name="line_38"></a> _ASSERT(
<a name="line_39"></a>  simplify(t_proj(c_E0[0](t)) -~ [0,cos(2*t)^2]) = [0,0] and
<a name="line_40"></a>  simplify(t_proj(c_E0[1](t)) -~ [(1+sin(t)^2)*cos(t)^2,0]) = [0,0] and
<a name="line_41"></a>  simplify(t_proj(c_E0[3](t)) -~ [(1+sin(t)^2/3)*cos(t)^2,1]) = [0,0] and
<a name="line_42"></a>  simplify(t_proj(c_E0[5](t)) -~ [1,(1-cos(t))^2/4]) = [0,0],
<a name="line_43"></a>  "t_proj on boundary of F16"
<a name="line_44"></a> );
<a name="line_45"></a>end:
<a name="line_46"></a>
<a name="line_47"></a>add_check(check_t_proj):
<a name="line_48"></a>
<a name="line_49"></a>######################################################################
<a name="line_50"></a>
<a name="line_51"></a>check_oval := proc()
<a name="line_52"></a>
<a name="line_53"></a> printf("%a()\n",procname);
<a name="line_54"></a>
<a name="line_55"></a> _ASSERT(
<a name="line_56"></a>  expand(g0([x[1],0,x[3],x[4]]) - sqrt(2)*(x[3]-sqrt(2)*x[4])*oval_g(xx)) = 0,
<a name="line_57"></a>  "Factorisation of g(x) when x[2]=0"
<a name="line_58"></a> );
<a name="line_59"></a>
<a name="line_60"></a> _ASSERT(
<a name="line_61"></a>   factor(expand(oval_g(xx) - add(oval_m[i]*dp4(oval_e[i],xx)^2,i=1..4))) = 0,
<a name="line_62"></a>   "Diagonalisation of oval_g"
<a name="line_63"></a> );
<a name="line_64"></a>
<a name="line_65"></a> _ASSERT(
<a name="line_66"></a>  factor(expand(rho(xx) - add(dp4(oval_e[i],xx)^2,i=1..4))) = 0,
<a name="line_67"></a>  "The oval_e basis is orthonormal"
<a name="line_68"></a> );
<a name="line_69"></a>
<a name="line_70"></a> _ASSERT(
<a name="line_71"></a>  simplify(oval_g(c_E0[5](t))) = 0,
<a name="line_72"></a>  "oval_g(c[5](t)) = 0"
<a name="line_73"></a> );
<a name="line_74"></a>
<a name="line_75"></a> _ASSERT(
<a name="line_76"></a>  simplify(oval_g(c_E0[7](t))) = 0,
<a name="line_77"></a>  "oval_g(c[7](t)) = 0"
<a name="line_78"></a> );
<a name="line_79"></a>
<a name="line_80"></a>end:
<a name="line_81"></a>
<a name="line_82"></a>add_check(check_oval):
<a name="line_83"></a>
<a name="line_84"></a>######################################################################
<a name="line_85"></a>
<a name="line_86"></a>check_c_alt := proc()
<a name="line_87"></a> local err,i,k,l,m,c,T;
<a name="line_88"></a> 
<a name="line_89"></a> printf("%a()\n",procname);
<a name="line_90"></a>
<a name="line_91"></a> _ASSERT({seq(simplify(rho(c_alt[k](t))-1),k=5..8)} = {0},
<a name="line_92"></a>         "c_alt[k](t) lies in S^3");
<a name="line_93"></a>	 
<a name="line_94"></a> _ASSERT({seq(simplify(g0(c_alt[k](t))),k=5..8)} = {0},
<a name="line_95"></a>         "c_alt[k](t) lies in EX^*");
<a name="line_96"></a>
<a name="line_97"></a> err := NULL;
<a name="line_98"></a> for k from 5 to 8 do
<a name="line_99"></a>  for T in [L,M,N] do
<a name="line_100"></a>   l,m,c := op(act_c_data[T,k]);
<a name="line_101"></a>   err := err,simplify(act_R4[T](c_alt[k](t)) -~ c_alt[l](m*t+c));
<a name="line_102"></a>  od:
<a name="line_103"></a> od:
<a name="line_104"></a>
<a name="line_105"></a> _ASSERT({err} = {[0$4]},"group action on c_alt");
<a name="line_106"></a>
<a name="line_107"></a> err := NULL;
<a name="line_108"></a> for k from 5 to 8 do
<a name="line_109"></a>  for i from 0 to 13 do
<a name="line_110"></a>   if v_on_c[i,k] <> NULL then
<a name="line_111"></a>    err := err,simplify(c_alt[k](v_on_c[i,k]) -~ v_E0[i]);
<a name="line_112"></a>   fi:
<a name="line_113"></a>  od:
<a name="line_114"></a> od:
<a name="line_115"></a>
<a name="line_116"></a> _ASSERT({err} = {[0$4]},"vertices on c_alt");
<a name="line_117"></a> 
<a name="line_118"></a> _ASSERT(
<a name="line_119"></a>  simplify(oval_g(c_alt[5](t))) = 0,
<a name="line_120"></a>  "oval_g(c_alt[5](t)) = 0"
<a name="line_121"></a> );
<a name="line_122"></a>
<a name="line_123"></a> _ASSERT(
<a name="line_124"></a>  simplify(oval_g(c_alt[7](t))) = 0,
<a name="line_125"></a>  "oval_g(c_alt[7](t)) = 0"
<a name="line_126"></a> );
<a name="line_127"></a>
<a name="line_128"></a>end:
<a name="line_129"></a>
<a name="line_130"></a>add_check(check_c_alt):
<a name="line_131"></a>
<a name="line_132"></a>######################################################################
<a name="line_133"></a>
<a name="line_134"></a>check_elliptic_b := proc()
<a name="line_135"></a> global x0,r0,r1,a0,a1,a2,err;
<a name="line_136"></a>
<a name="line_137"></a> printf("%a()\n",procname);
<a name="line_138"></a>
<a name="line_139"></a> _ASSERT(
<a name="line_140"></a>  simplify(elliptic_b_poly(elliptic_b_proj(c_E0[5](t)))) = 0,
<a name="line_141"></a>  "elliptic_b relations 0"
<a name="line_142"></a> );
<a name="line_143"></a>
<a name="line_144"></a> x0 := [x[1],0,x[3],x[4]];
<a name="line_145"></a> r0 := rho(x0) - 1;
<a name="line_146"></a> r1 := oval_g(x0);
<a name="line_147"></a> a0 := -6*sqrt(2)*(x[3]+sqrt(2)*x[4])*x[4]^3;
<a name="line_148"></a> a1 := (1-x[3]^2-x[4]^2)*(x[3]^2-10*x[4]^2+9+x[3]*x[4]/sqrt(2));
<a name="line_149"></a> a2 := x[3]^2+2*x[4]^2+x[3]*x[4]/sqrt(2)-x[1]^2+9;
<a name="line_150"></a> err := expand(elliptic_b_poly(elliptic_b_proj(x0))
<a name="line_151"></a>               - a0*r0 - a1*r1 - a2 *r0*r1);
<a name="line_152"></a>
<a name="line_153"></a> _ASSERT(err = 0,"elliptic_b relations 1");
<a name="line_154"></a>end:
<a name="line_155"></a>
<a name="line_156"></a>add_check(check_elliptic_b):
<a name="line_157"></a>
<a name="line_158"></a>######################################################################
<a name="line_159"></a>
<a name="line_160"></a>check_polar_curves := proc()
<a name="line_161"></a> _ASSERT(
<a name="line_162"></a>  simplify(c_E0[0](t) -~ S3_polar([0,t,0])) = [0$4],
<a name="line_163"></a>  "polar expression for c_E0[0](t)"
<a name="line_164"></a> );
<a name="line_165"></a>
<a name="line_166"></a> _ASSERT(
<a name="line_167"></a>  simplify(c_E0[1](t) -~ S3_polar([Pi/2-t,Pi/4,0])) = [0$4],
<a name="line_168"></a>  "polar expression for c_E0[1](t)"
<a name="line_169"></a> );
<a name="line_170"></a>
<a name="line_171"></a> _ASSERT(
<a name="line_172"></a>  simplify(c_E0[3](t) -~ S3_polar([Pi/2-t,Pi/2,-arcsin(1/sqrt(3))])) = [0$4],
<a name="line_173"></a>  "polar expression for c_E0[3](t)"
<a name="line_174"></a> );
<a name="line_175"></a>end:
<a name="line_176"></a>
<a name="line_177"></a>add_check(check_polar_curves):
<a name="line_178"></a>
<a name="line_179"></a>######################################################################
<a name="line_180"></a>
<a name="line_181"></a>check_tangent_a := proc()
<a name="line_182"></a> printf("%a()\n",procname);
<a name="line_183"></a>
<a name="line_184"></a> _ASSERT(
<a name="line_185"></a>  NF_x0(dp4(xx,tangent_a(xx)))=0,
<a name="line_186"></a>  "tangent_a(x) perpendicular to x"
<a name="line_187"></a> );
<a name="line_188"></a>
<a name="line_189"></a> _ASSERT(
<a name="line_190"></a>  NF_x0(dp4(dg0(xx),tangent_a(xx)))=0,
<a name="line_191"></a>  "tangent_a(x) perpendicular to n(x)"
<a name="line_192"></a> );
<a name="line_193"></a>end:
<a name="line_194"></a>
<a name="line_195"></a>add_check(check_tangent_a):
<a name="line_196"></a>
<a name="line_197"></a>check_tangent_b := proc()
<a name="line_198"></a> local err0,err1,err2,x0,x1,n1,u1;
<a name="line_199"></a>
<a name="line_200"></a> printf("%a()\n",procname);
<a name="line_201"></a>
<a name="line_202"></a> err0 := 0;
<a name="line_203"></a> err1 := 0;
<a name="line_204"></a> err2 := 0;
<a name="line_205"></a> 
<a name="line_206"></a> for x0 in inner_quasirational_points do
<a name="line_207"></a>  x1 := evalf(x0);
<a name="line_208"></a>  n1 := dg0(x1);
<a name="line_209"></a>  n1 := evalf(n1/nm4(n1));
<a name="line_210"></a>  u1 := unit_tangent_b(x1);
<a name="line_211"></a>  err0 := max(err0,abs(dp4(u1,x1)));
<a name="line_212"></a>  err1 := max(err1,abs(dp4(u1,n1)));
<a name="line_213"></a>  err2 := max(err2,abs(dp4(u1,u1) - 1));
<a name="line_214"></a> od:
<a name="line_215"></a>
<a name="line_216"></a> _ASSERT(err0 < 10.^(-90),"tangent_b(x) perpendicular to x");
<a name="line_217"></a> _ASSERT(err1 < 10.^(-90),"tangent_b(x) perpendicular to n(x)");
<a name="line_218"></a> _ASSERT(err2 < 10.^(-90),"tangent_b(x) is a unit vector");
<a name="line_219"></a>end:
<a name="line_220"></a>
<a name="line_221"></a>add_check(check_tangent_b):
<a name="line_222"></a>
<a name="line_223"></a>######################################################################
<a name="line_224"></a>
<a name="line_225"></a>check_cubic_chart := proc()
<a name="line_226"></a> local u,uu,V,B,y,err;
<a name="line_227"></a>
<a name="line_228"></a> printf("%a()\n",procname);
<a name="line_229"></a>
<a name="line_230"></a> uu := [seq(u[i],i=1..4)];
<a name="line_231"></a> V := tdeg(op(xx),op(uu));
<a name="line_232"></a> B := Basis([rho(xx)-1,g0(xx),dp4(xx,uu),dp4(dg0(xx),uu)],V);
<a name="line_233"></a>
<a name="line_234"></a> y := cubic_chart0(xx,[seq(e*u[i],i=1..4)]);
<a name="line_235"></a>
<a name="line_236"></a> err := rho(y) - 1;
<a name="line_237"></a> err := convert(series(err,e=0,4),polynom,e);
<a name="line_238"></a> err := numer(factor(expand(err)));
<a name="line_239"></a> err := NormalForm(err,B,V);
<a name="line_240"></a>
<a name="line_241"></a> _ASSERT(err=0,"rho on cubic_chart0");
<a name="line_242"></a>
<a name="line_243"></a> err := g0(y);
<a name="line_244"></a> err := convert(series(err,e=0,4),polynom,e);
<a name="line_245"></a> err := numer(factor(expand(err)));
<a name="line_246"></a> err := NormalForm(err,B,V);
<a name="line_247"></a>
<a name="line_248"></a> _ASSERT(err=0,"g0 on cubic_chart0");
<a name="line_249"></a>
<a name="line_250"></a> err := y -~ xx -~ (e *~ uu);
<a name="line_251"></a> err := map(series,err,e=0,2);
<a name="line_252"></a> err := map(convert,err,polynom,e);
<a name="line_253"></a> err := map(NormalForm,err,B,V);
<a name="line_254"></a>
<a name="line_255"></a> _ASSERT(err = [0$4],"cubic_chart0 to first order");
<a name="line_256"></a>end:
<a name="line_257"></a>
<a name="line_258"></a>add_check(check_cubic_chart):
<a name="line_259"></a>
<a name="line_260"></a>######################################################################
<a name="line_261"></a>
<a name="line_262"></a>check_extra_annular_charts := proc()
<a name="line_263"></a> local A0,A1,A2;
<a name="line_264"></a>
<a name="line_265"></a> printf("%a()\n",procname);
<a name="line_266"></a>
<a name="line_267"></a> A0 := annular_chart0[3]([t,u]);
<a name="line_268"></a> A1 := map(diff,A0,t);
<a name="line_269"></a> A2 := map(diff,A0,u);
<a name="line_270"></a>
<a name="line_271"></a> _ASSERT(
<a name="line_272"></a>  convert(series(combine(simplify(rho(A0)-1)),u=0,4),polynom,u) = 0,
<a name="line_273"></a>  "annular_chart[3] lands in S3"
<a name="line_274"></a> );
<a name="line_275"></a>
<a name="line_276"></a> _ASSERT(
<a name="line_277"></a>  convert(series(combine(simplify(g0(A0))),u=0,4),polynom,u) = 0,
<a name="line_278"></a>  "annular_chart[3] lands in EX(a)"
<a name="line_279"></a> );
<a name="line_280"></a>
<a name="line_281"></a> _ASSERT(
<a name="line_282"></a>  convert(series(combine(simplify(dp4(A1,A2))),u=0,3),polynom,u) = 0,
<a name="line_283"></a>  "annular_chart[3] is approximately conformal 1"
<a name="line_284"></a> );
<a name="line_285"></a>
<a name="line_286"></a> _ASSERT(
<a name="line_287"></a>  convert(series(combine(simplify(dp4(A1,A1)-dp4(A2,A2))),u=0,4),polynom,u) = 0,
<a name="line_288"></a>  "annular_chart[3] is approximately conformal 2"
<a name="line_289"></a> );
<a name="line_290"></a>
<a name="line_291"></a> A0 := annular_chart0[5]([t,u]);
<a name="line_292"></a> A1 := map(diff,A0,t);
<a name="line_293"></a> A2 := map(diff,A0,u);
<a name="line_294"></a>
<a name="line_295"></a> _ASSERT(
<a name="line_296"></a>  convert(series(combine(simplify(rho(A0)-1)),u=0,4),polynom,u) = 0,
<a name="line_297"></a>  "annular_chart[5] lands in S3"
<a name="line_298"></a> );
<a name="line_299"></a>
<a name="line_300"></a> _ASSERT(
<a name="line_301"></a>  convert(series(combine(simplify(g0(A0))),u=0,4),polynom,u) = 0,
<a name="line_302"></a>  "annular_chart[5] lands in EX(a)"
<a name="line_303"></a> );
<a name="line_304"></a>
<a name="line_305"></a> _ASSERT(
<a name="line_306"></a>  convert(series(combine(simplify(dp4(A1,A2))),u=0,1),polynom,u) = 0,
<a name="line_307"></a>  "annular_chart[5] is approximately conformal 1"
<a name="line_308"></a> );
<a name="line_309"></a>
<a name="line_310"></a> _ASSERT(
<a name="line_311"></a>  convert(series(combine(simplify(dp4(A1,A1)-dp4(A2,A2))),u=0,2),polynom,u) = 0,
<a name="line_312"></a>  "annular_chart[5] is approximately conformal 2"
<a name="line_313"></a> );
<a name="line_314"></a>
<a name="line_315"></a>end:
<a name="line_316"></a>
<a name="line_317"></a>add_check(check_extra_annular_charts):
  </pre>
 </body>
</html>
    