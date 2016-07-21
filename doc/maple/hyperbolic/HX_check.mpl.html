<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_hyperbolic_metric := proc()
<a name="line_2"></a> local x1,x2,y1,y2,a1,a2,x,y,a,t;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> assume(x1::real,x2::real,y1::real,y2::real,a1::real,a2::real);
<a name="line_7"></a> assume(t > 0);
<a name="line_8"></a> x := x1 + I*x2;
<a name="line_9"></a> y := y1 + I*y2;
<a name="line_10"></a> a := a1 + I*a2;
<a name="line_11"></a>
<a name="line_12"></a> _ASSERT(
<a name="line_13"></a>  simplify(d_hyp_c(x,y) - d_hyp([x1,x2],[y1,y2])) = 0,
<a name="line_14"></a>  "real and complex formulae for d_hyp"
<a name="line_15"></a> );
<a name="line_16"></a>
<a name="line_17"></a> _ASSERT(
<a name="line_18"></a>  simplify(E_hyp_c(x) - E_hyp([x1,x2])) = 0,
<a name="line_19"></a>  "real and complex formulae for E_hyp"
<a name="line_20"></a> );
<a name="line_21"></a>
<a name="line_22"></a> _ASSERT(
<a name="line_23"></a>  simplify(d_hyp_c(exp(I*t)*x,exp(I*t)*y) - d_hyp_c(x,y)) = 0,
<a name="line_24"></a>  "rotation invariance of d_hyp"
<a name="line_25"></a> );
<a name="line_26"></a>
<a name="line_27"></a> _ASSERT(
<a name="line_28"></a>  simplify(d_hyp_c((x - a)/(1 - conjugate(a)*x),
<a name="line_29"></a>                   (y - a)/(1 - conjugate(a)*y)) - d_hyp_c(x,y)) = 0,
<a name="line_30"></a>  "Mobius invariance of d_hyp"
<a name="line_31"></a> );
<a name="line_32"></a>
<a name="line_33"></a> _ASSERT(
<a name="line_34"></a>  simplify(convert(series(d_hyp_c(x,x+t*y)^2,t=0,3),polynom,t) - E_hyp_c(x)*t^2*abs(y)^2 = 0),
<a name="line_35"></a>  "Compatibility of d_hyp and E_hyp"
<a name="line_36"></a> );
<a name="line_37"></a>
<a name="line_38"></a> _ASSERT(
<a name="line_39"></a>  simplify(brioschi_formula(E_hyp([t,u]),0,E_hyp([t,u]),t,u)+1) = 0,
<a name="line_40"></a>  "curvature of d_hyp"
<a name="line_41"></a> );
<a name="line_42"></a>end:
<a name="line_43"></a>
<a name="line_44"></a>add_check(check_hyperbolic_metric):
<a name="line_45"></a>
<a name="line_46"></a>######################################################################
<a name="line_47"></a>
<a name="line_48"></a>check_Pi_action := proc()
<a name="line_49"></a> local i,j,k,ij,T,U,A,w,z,err,ncheck,ncheck_b,old_digits;
<a name="line_50"></a>
<a name="line_51"></a> printf("%a()\n",procname);
<a name="line_52"></a>
<a name="line_53"></a> for i from 0 to 3 do
<a name="line_54"></a>  _ASSERT(
<a name="line_55"></a>   simplify(act_Pi([i,i+4],z) - z) = 0,
<a name="line_56"></a>   sprintf("beta[%d] o beta[%d] on H",i,i+4)
<a name="line_57"></a>  );
<a name="line_58"></a> od:
<a name="line_59"></a>
<a name="line_60"></a> w :=          beta[7](z);
<a name="line_61"></a> w := simplify(beta[6](w));
<a name="line_62"></a> w := simplify(beta[5](w));
<a name="line_63"></a> w := simplify(beta[4](w));
<a name="line_64"></a> w := simplify(beta[3](w));
<a name="line_65"></a> w := simplify(beta[2](w));
<a name="line_66"></a> w := simplify(beta[1](w));
<a name="line_67"></a> w := simplify(beta[0](w));
<a name="line_68"></a>
<a name="line_69"></a> _ASSERT(simplify(w-z) = 0,"long beta relation on H");
<a name="line_70"></a>
<a name="line_71"></a> for T in L8 do
<a name="line_72"></a>  for U in L8 do
<a name="line_73"></a>   _ASSERT(
<a name="line_74"></a>    act_H[T](act_H[U](z)) = act_H[G_mult(T,U)](z),
<a name="line_75"></a>    "composition for action of L8 on H"
<a name="line_76"></a>   );
<a name="line_77"></a>  od;
<a name="line_78"></a> od;
<a name="line_79"></a>
<a name="line_80"></a> _ASSERT(
<a name="line_81"></a>  simplify(act_H[LM](z) - act_H[L](act_H[M](z))) = 0,
<a name="line_82"></a>  "LM = L o M on H"
<a name="line_83"></a> );
<a name="line_84"></a>
<a name="line_85"></a> _ASSERT(
<a name="line_86"></a>  simplify(act_H[LLM](z) - act_H[LL](act_H[M](z))) = 0,
<a name="line_87"></a>  "LLM = LL o M on H"
<a name="line_88"></a> );
<a name="line_89"></a>
<a name="line_90"></a> _ASSERT(
<a name="line_91"></a>  simplify(act_H[LLLM](z) - act_H[LLL](act_H[M](z))) = 0,
<a name="line_92"></a>  "LLLM = LLL o M on H"
<a name="line_93"></a> );
<a name="line_94"></a>
<a name="line_95"></a> _ASSERT(
<a name="line_96"></a>  simplify(act_H[MN](z) - act_H[M](act_H[N](z))) = 0,
<a name="line_97"></a>  "MN = M o N on H"
<a name="line_98"></a> );
<a name="line_99"></a>
<a name="line_100"></a> _ASSERT(
<a name="line_101"></a>  simplify(act_H[LMN](z) - act_H[LM](act_H[N](z))) = 0,
<a name="line_102"></a>  "LMN = LM o N on H"
<a name="line_103"></a> );
<a name="line_104"></a>
<a name="line_105"></a> _ASSERT(
<a name="line_106"></a>  simplify(act_H[LLMN](z) - act_H[LLM](act_H[N](z))) = 0,
<a name="line_107"></a>  "LLMN = LLM o N on H"
<a name="line_108"></a> );
<a name="line_109"></a>
<a name="line_110"></a> _ASSERT(
<a name="line_111"></a>  simplify(act_H[LLLMN](z) - act_H[LLLM](act_H[N](z))) = 0,
<a name="line_112"></a>  "LLLMN = LLLM o N on H"
<a name="line_113"></a> );
<a name="line_114"></a>
<a name="line_115"></a> _ASSERT(
<a name="line_116"></a>  simplify(act_H[M](act_H[M](z)) - z) = 0,
<a name="line_117"></a>  "MM = 1 on H"
<a name="line_118"></a> );
<a name="line_119"></a>
<a name="line_120"></a> _ASSERT(
<a name="line_121"></a>  simplify(act_H[LM](act_H[LM](z)) - 
<a name="line_122"></a>	   act_Pi([7,6],z)) = 0,
<a name="line_123"></a>  "LMLM on H"
<a name="line_124"></a> );
<a name="line_125"></a>
<a name="line_126"></a> _ASSERT(
<a name="line_127"></a>  simplify(act_H[N](act_H[M](act_H[N](act_H[M](z)))) - 
<a name="line_128"></a>	   act_Pi([6,0,7,6],z)) = 0,
<a name="line_129"></a>  "NMNM on H"
<a name="line_130"></a> );
<a name="line_131"></a>
<a name="line_132"></a> NULL;
<a name="line_133"></a>end:
<a name="line_134"></a>
<a name="line_135"></a>add_check(check_Pi_action):
<a name="line_136"></a>
<a name="line_137"></a>######################################################################
<a name="line_138"></a>
<a name="line_139"></a>check_v_H := proc()
<a name="line_140"></a> local i,i0,ii,A,T,err;
<a name="line_141"></a>
<a name="line_142"></a> printf("%a()\n",procname);
<a name="line_143"></a>
<a name="line_144"></a> for ii in {indices(v_H)} do
<a name="line_145"></a>  i := op(ii);
<a name="line_146"></a>  if i = evalf(floor(i)) then
<a name="line_147"></a>   i := floor(i);
<a name="line_148"></a>  fi;
<a name="line_149"></a>  _ASSERT(
<a name="line_150"></a>   R2_zone(op(net_0["v"][i])) = C_zone(v_H1[i]),
<a name="line_151"></a>   sprintf("zone for v[%a]",i)
<a name="line_152"></a>  );
<a name="line_153"></a> od:
<a name="line_154"></a>
<a name="line_155"></a> for ii in indices(v_H_fraction_offset) do
<a name="line_156"></a>  i := op(ii);
<a name="line_157"></a>  i0 := floor(i);
<a name="line_158"></a>  A := v_H_fraction_offset[i]:
<a name="line_159"></a>  err := simplify(act_Pi(A,v_H[i0])-v_H[i]);
<a name="line_160"></a>  _ASSERT(err=0,sprintf("v_H_fraction_offset[%a]",i));
<a name="line_161"></a> od:
<a name="line_162"></a>
<a name="line_163"></a> for ii in indices(v_H_fraction_transform) do
<a name="line_164"></a>  i := op(ii);
<a name="line_165"></a>  i0 := floor(i);
<a name="line_166"></a>  T := v_H_fraction_transform[i]:
<a name="line_167"></a>  err := simplify(act_H[T](v_H[i0])-v_H[i]);
<a name="line_168"></a>  _ASSERT(err=0,sprintf("v_H_fraction_transform[%a]",i));
<a name="line_169"></a> od:
<a name="line_170"></a>end:
<a name="line_171"></a>
<a name="line_172"></a>add_check(check_v_H):
<a name="line_173"></a>
<a name="line_174"></a>######################################################################
<a name="line_175"></a>
<a name="line_176"></a>check_c_H_monodromy := proc()
<a name="line_177"></a> local k,err;
<a name="line_178"></a>
<a name="line_179"></a> printf("%a()\n",procname);
<a name="line_180"></a>
<a name="line_181"></a> for k from 0 to 8 do
<a name="line_182"></a>  err := c_H[k](t+2*Pi) - act_Pi(c_H_cycle[k],c_H[k](t));
<a name="line_183"></a>  err := factor(expand(simplify_H(err)));
<a name="line_184"></a>  _ASSERT(
<a name="line_185"></a>   err = 0,
<a name="line_186"></a>   sprintf("monodromy for curve c_H[%d]",k)
<a name="line_187"></a>   );
<a name="line_188"></a> od;
<a name="line_189"></a>end:
<a name="line_190"></a>
<a name="line_191"></a>add_check(check_c_H_monodromy):
<a name="line_192"></a>
<a name="line_193"></a>######################################################################
<a name="line_194"></a>
<a name="line_195"></a>check_H_F1 := proc()
<a name="line_196"></a> local t,err,V;
<a name="line_197"></a>
<a name="line_198"></a> printf("%a()\n",procname);
<a name="line_199"></a>
<a name="line_200"></a> for t in ['a','b','c','d','e','f'] do 
<a name="line_201"></a>  err := map(simplify,map2(act_Pi,F1_pairing[t],F1_edge[1,-1,t]) -~ F1_edge[1,1,t]);
<a name="line_202"></a>  _ASSERT(err = [0,0],sprintf("F1 edge pairing for edge %a",t));
<a name="line_203"></a> od;
<a name="line_204"></a>
<a name="line_205"></a> V := map(u -> simplify(subs(a_H=a_H_regular,(u[1]+I*u[2]) + u[3]*exp(I*u[4]))),F1_arcs):
<a name="line_206"></a> err := max(seq(abs(evalf(V[i] - 3^(-1/4) * exp(I*Pi/12*(2*i-1)))),i=1..12));
<a name="line_207"></a> _ASSERT(err < 10.^(-90), "F1 as a regular dodecagon");
<a name="line_208"></a> 
<a name="line_209"></a>end:
<a name="line_210"></a>
<a name="line_211"></a>add_check(check_H_F1):
<a name="line_212"></a>
<a name="line_213"></a>######################################################################
<a name="line_214"></a>
<a name="line_215"></a>check_F1_area := proc()
<a name="line_216"></a> local A,k,s,c,u,err;
<a name="line_217"></a> 
<a name="line_218"></a> printf("%a()\n",procname);
<a name="line_219"></a>
<a name="line_220"></a> _ASSERT(simplify(v_H[11] - (c_H_p[3] - c_H_r[3])) = 0,
<a name="line_221"></a>         "angle for v[11] on C[3]");
<a name="line_222"></a>
<a name="line_223"></a> _ASSERT(simplify(v_H[13] - (c_H_p[3] - c_H_r[3] * exp(-I*v_H_theta))) = 0,
<a name="line_224"></a>         "angle for v[13] on C[3]");
<a name="line_225"></a>
<a name="line_226"></a> _ASSERT(simplify(v_H[13] - (c_H_p[7] - c_H_r[7] * I * exp(-I*v_H_theta))) = 0,
<a name="line_227"></a>         "angle for v[13] on C[7]");
<a name="line_228"></a>
<a name="line_229"></a> _ASSERT(simplify(v_H[ 1] - (c_H_p[7] - c_H_r[7])) = 0,
<a name="line_230"></a>         "angle for v[1] on C[7]");
<a name="line_231"></a>
<a name="line_232"></a> _ASSERT({
<a name="line_233"></a>  simplify(expand(c_H_simple[ 5](0) - v_H[ 0])),
<a name="line_234"></a>  simplify(expand(c_H_simple[ 5](1) - v_H[11])),
<a name="line_235"></a>  simplify(expand(c_H_simple[ 3](0) - v_H[11])),
<a name="line_236"></a>  simplify(expand(c_H_simple[ 3](1) - v_H[13])),
<a name="line_237"></a>  simplify(expand(c_H_simple[ 7](0) - v_H[13])),
<a name="line_238"></a>  simplify(expand(c_H_simple[ 7](1) - v_H[ 1])),
<a name="line_239"></a>  simplify(expand(c_H_simple[ 1](0) - v_H[ 1])),
<a name="line_240"></a>  simplify(expand(c_H_simple[ 1](1) - v_H[ 0]))} = {0},
<a name="line_241"></a>  "Parametrisation of the boundary of F8"
<a name="line_242"></a> );
<a name="line_243"></a>
<a name="line_244"></a> assume(s > 0);
<a name="line_245"></a> 
<a name="line_246"></a> A := 0;
<a name="line_247"></a> for k in [1,5,3,7] do
<a name="line_248"></a>  c := c_H_simple[k](s);
<a name="line_249"></a>  u := simplify(expand(Im(c) * diff(Re(c),s)));
<a name="line_250"></a>  A := A - int(u,s=0..1);
<a name="line_251"></a> od;
<a name="line_252"></a> A := simplify(expand(A));
<a name="line_253"></a> err := simplify(expand(8*A - F1_area));
<a name="line_254"></a>
<a name="line_255"></a> _ASSERT(err = 0,"area of F1");
<a name="line_256"></a>end:
<a name="line_257"></a>
<a name="line_258"></a>add_check(check_F1_area):
<a name="line_259"></a>
<a name="line_260"></a>######################################################################
<a name="line_261"></a>
<a name="line_262"></a>check_Pi_bound := proc()
<a name="line_263"></a> local a1,a2,L,err;
<a name="line_264"></a>
<a name="line_265"></a> printf("%a()\n",procname);
<a name="line_266"></a>
<a name="line_267"></a> a1 := (1+(1-a_H^2)/(1+a_H^2))/2;
<a name="line_268"></a> a2 := 4/5*(1+(a_H^2*(3-a_H^2))/(5+2*a_H^2+a_H^4));
<a name="line_269"></a> err := factor(map(L -> abs(act_Pi(L,0))^2,F1_H_B) -~ [a1$4,a2$8]);
<a name="line_270"></a> 
<a name="line_271"></a> _ASSERT(err = [0$12],"|gamma(0)|^2 >= 1/2");
<a name="line_272"></a>end:
<a name="line_273"></a>
<a name="line_274"></a>add_check(check_Pi_bound);
<a name="line_275"></a>
<a name="line_276"></a>######################################################################
<a name="line_277"></a>
<a name="line_278"></a>check_square_diffeo_H := proc()
<a name="line_279"></a> local s,u,err,i,j,t0,t1,z0;
<a name="line_280"></a> 
<a name="line_281"></a> printf("%a()\n",procname);
<a name="line_282"></a>
<a name="line_283"></a> u[ 6] := [0,0];
<a name="line_284"></a> u[ 0] := [1,0];
<a name="line_285"></a> u[ 3] := [0,1];
<a name="line_286"></a> u[11] := [1,1];
<a name="line_287"></a>
<a name="line_288"></a> for i in [6,0,3,11] do
<a name="line_289"></a>  err := square_diffeo_H(v_H[i]) -~ u[i];
<a name="line_290"></a>  err := rationalize(simplify(expand(err)));
<a name="line_291"></a>  err := map(numer,err);
<a name="line_292"></a>  err := factor(expand(rationalize(map(exp,err))));
<a name="line_293"></a>  err := factor(expand(rationalize(err)));
<a name="line_294"></a>  err := map(ln,err);
<a name="line_295"></a>  _ASSERT(err = [0,0],sprintf("square_diffeo_H(v_H[%d])",i));
<a name="line_296"></a> od:
<a name="line_297"></a>
<a name="line_298"></a> assume(s > 0);
<a name="line_299"></a> 
<a name="line_300"></a> err := simplify(expand(square_diffeo_H(c_H[1](s))[2]));
<a name="line_301"></a> _ASSERT(err = 0,"square_diffeo_H(c_H[1](s))[2] = 0");
<a name="line_302"></a>
<a name="line_303"></a> err := simplify(expand(square_diffeo_H(c_H[5](s))[1] - 1));
<a name="line_304"></a> _ASSERT(err = 0,"square_diffeo_H(c_H[5](s))[1] = 1");
<a name="line_305"></a>
<a name="line_306"></a> err := square_diffeo_H(c_H[0](s))[1]:
<a name="line_307"></a> err := max(seq(seq(abs(evalf(subs({a_H=i*0.1,s=j*Pi/12},err))),j=-11..12),i=1..9));
<a name="line_308"></a> _ASSERT(err < 10.^(-90),"square_diffeo_H(c_H[0](s))[1] = 0");
<a name="line_309"></a>
<a name="line_310"></a> err := square_diffeo_H(c_H[3](s))[2] - 1:
<a name="line_311"></a> err := max(seq(seq(abs(evalf(subs({a_H=i*0.1,s=j*Pi/12},err))),j=-11..12),i=1..9));
<a name="line_312"></a> _ASSERT(err < 10.^(-90),"square_diffeo_H(c_H[3](s))[2] = 1");
<a name="line_313"></a>
<a name="line_314"></a> err := 0;
<a name="line_315"></a> for i from 1 to 9 do
<a name="line_316"></a>  for j from 1 to 9 do
<a name="line_317"></a>   t0 := [i,j] *~ 0.1;
<a name="line_318"></a>   z0 := square_diffeo_inv_H0(t0);
<a name="line_319"></a>   t1 := square_diffeo_H0(z0);
<a name="line_320"></a>   err := max(err,d2f(t0,t1));
<a name="line_321"></a>  od;
<a name="line_322"></a> od;
<a name="line_323"></a> _ASSERT(err < 10.^(-90),"square_diffeo_H o square_diffeo_inv_H");
<a name="line_324"></a>end:
<a name="line_325"></a>
<a name="line_326"></a>add_check(check_square_diffeo_H):
<a name="line_327"></a>
<a name="line_328"></a>######################################################################
<a name="line_329"></a>
<a name="line_330"></a>check_xi := proc()
<a name="line_331"></a> local p,r,s,t,u,v;
<a name="line_332"></a>
<a name="line_333"></a> printf("%a()\n",procname);
<a name="line_334"></a>
<a name="line_335"></a> assume(r > 1);
<a name="line_336"></a> assume(t::real);
<a name="line_337"></a> assume(s::real);
<a name="line_338"></a> assume(u::real);
<a name="line_339"></a> assume(v::real);
<a name="line_340"></a> p := r*cos(t) + I*r*sin(t);
<a name="line_341"></a>
<a name="line_342"></a> _ASSERT(
<a name="line_343"></a>  simplify(xi(p,xi(p,u+I*v)) - (u+I*v)) = 0,
<a name="line_344"></a>  "xi(p,-) is an involution"
<a name="line_345"></a> ):
<a name="line_346"></a>
<a name="line_347"></a> _ASSERT(
<a name="line_348"></a>  simplify(xi(p,xi_curve(p,s)) - xi_curve(p,s)) = 0,
<a name="line_349"></a>  "xi_curve(p,s) is fixed by xi(p,-)"
<a name="line_350"></a> ):
<a name="line_351"></a>
<a name="line_352"></a> _ASSERT(
<a name="line_353"></a>  simplify(abs(xi_curve(p,s) - p)^2 - (abs(p)^2-1)) = 0,
<a name="line_354"></a>  "distance of xi_curve(p,s) from p"
<a name="line_355"></a> ):
<a name="line_356"></a>
<a name="line_357"></a> _ASSERT(
<a name="line_358"></a>  simplify(4*abs(diff(xi_curve(p,s),s))^2/(1 - abs(xi_curve(p,s))^2)^2 - 1) = 0,
<a name="line_359"></a>  "xi_curve(p,-) has speed one"
<a name="line_360"></a> ):
<a name="line_361"></a>
<a name="line_362"></a> _ASSERT(
<a name="line_363"></a>  simplify(xi_alt(t,xi_alt(t,u+I*v)) - (u+I*v)) = 0,
<a name="line_364"></a>  "xi_alt(t,-) is an involution"
<a name="line_365"></a> ):
<a name="line_366"></a>
<a name="line_367"></a> _ASSERT(
<a name="line_368"></a>  simplify(xi_alt(t,xi_curve_alt(t,s)) - xi_curve_alt(t,s)) = 0,
<a name="line_369"></a>  "xi_curve_alt(t,s) is fixed by xi_alt(t,-)"
<a name="line_370"></a> ):
<a name="line_371"></a>
<a name="line_372"></a> _ASSERT(
<a name="line_373"></a>  simplify(4*abs(diff(xi_curve_alt(t,s),s))^2/(1 - abs(xi_curve_alt(t,s))^2)^2 - 1) = 0,
<a name="line_374"></a>  "xi_curve_alt(t,-) has speed one"
<a name="line_375"></a> ):
<a name="line_376"></a>
<a name="line_377"></a>end:
<a name="line_378"></a>
<a name="line_379"></a>add_check(check_xi):
<a name="line_380"></a>
<a name="line_381"></a>######################################################################
<a name="line_382"></a>
<a name="line_383"></a>check_move_inwards := proc()
<a name="line_384"></a> local a,b,c,w,x,y,z,theta,alpha,lambda,q,r,err;
<a name="line_385"></a>
<a name="line_386"></a> printf("%a()\n",procname);
<a name="line_387"></a>
<a name="line_388"></a> assume(b::real,c::real,x::real,y::real,theta::real);
<a name="line_389"></a> alpha := b + I*c;
<a name="line_390"></a> lambda := exp(I*theta);
<a name="line_391"></a> z := x+I*y;
<a name="line_392"></a> w := lambda*(z - alpha)/(1 - conjugate(alpha)*z);
<a name="line_393"></a> q := simplify(abs(w)^2 - abs(z)^2);
<a name="line_394"></a> r := abs(alpha/(1 - conjugate(alpha)*z))^2 * 
<a name="line_395"></a>      (1 - abs(z)^2) *
<a name="line_396"></a>      (abs(z - 1/conjugate(alpha))^2 - (abs(alpha)^(-2) - 1));
<a name="line_397"></a> err := simplify(q-r);
<a name="line_398"></a> _ASSERT(err = 0,"|gamma(z)|^2 - |z|^2");
<a name="line_399"></a>end:
<a name="line_400"></a>
<a name="line_401"></a>add_check(check_move_inwards):
<a name="line_402"></a>
<a name="line_403"></a>######################################################################
<a name="line_404"></a>
<a name="line_405"></a>check_side_lengths := proc()
<a name="line_406"></a>
<a name="line_407"></a> printf("%a()\n",procname);
<a name="line_408"></a>
<a name="line_409"></a> _ASSERT(
<a name="line_410"></a>  is(side_length_H[0] >= 0) and 
<a name="line_411"></a>  simplify_H(m_hyp_c(v_H[ 3],v_H[ 6])^2 - tanh(side_length_H[ 0]/2)^2) = 0,
<a name="line_412"></a>  "side_length_H[0]"
<a name="line_413"></a> );
<a name="line_414"></a> 
<a name="line_415"></a> _ASSERT(
<a name="line_416"></a>  is(side_length_H[1] >= 0) and 
<a name="line_417"></a>  simplify_H(m_hyp_c(v_H[ 0],v_H[ 6])^2 - tanh(side_length_H[ 1]/2)^2) = 0,
<a name="line_418"></a>  "side_length_H[1]"
<a name="line_419"></a> );
<a name="line_420"></a> 
<a name="line_421"></a> _ASSERT(
<a name="line_422"></a>  is(side_length_H[3] >= 0) and 
<a name="line_423"></a>  simplify_H(m_hyp_c(v_H[ 3],v_H[11])^2 - tanh(side_length_H[ 3]/2)^2) = 0,
<a name="line_424"></a>  "side_length_H[3]"
<a name="line_425"></a> );
<a name="line_426"></a> 
<a name="line_427"></a> _ASSERT(
<a name="line_428"></a>  is(side_length_H[5] >= 0) and 
<a name="line_429"></a>  simplify_H(m_hyp_c(v_H[ 0],v_H[11])^2 - tanh(side_length_H[ 5]/2)^2) = 0,
<a name="line_430"></a>  "side_length_H[5]"
<a name="line_431"></a> );
<a name="line_432"></a> 
<a name="line_433"></a>end:
<a name="line_434"></a>
<a name="line_435"></a>add_check(check_side_lengths):
<a name="line_436"></a>
  </pre>
 </body>
</html>
    