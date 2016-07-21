<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_torus_diagrams := proc()
<a name="line_2"></a> local F1_samples,ok,x0,z0,zp0,zpa0,zm0,zmq0,t0,tp0,tpa0,tm0,tmq0,errs;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> F1_samples := [seq(seq(act_R4[T](inner_quasirational_points[i]),i=1..10),T in G16)]:
<a name="line_7"></a> ok := true:
<a name="line_8"></a>
<a name="line_9"></a> for x0 in F1_samples do
<a name="line_10"></a>  z0   := simplify(expand(rationalize(E_to_TTC(x0))));
<a name="line_11"></a>  zp0  := simplify(expand(rationalize(E_to_TCp(x0))));
<a name="line_12"></a>  zpa0 := simplify(expand(rationalize(E_to_TCpa(x0))));
<a name="line_13"></a>  t0   := simplify(expand(rationalize(E_to_TTP(x0))));
<a name="line_14"></a>  tp0  := simplify(expand(rationalize(E_to_TPp(x0))));
<a name="line_15"></a>  tpa0 := simplify(expand(rationalize(E_to_TPpa(x0))));
<a name="line_16"></a>  errs := [
<a name="line_17"></a>   TTC_to_TTP(z0) -~ t0,
<a name="line_18"></a>   TTP_to_TTC(t0) -~ z0,
<a name="line_19"></a>   TTC_to_TCp(z0) -~ zp0,
<a name="line_20"></a>   TTP_to_TPp(t0) -~ tp0,
<a name="line_21"></a>   TC_to_TP(zp0) -~ tp0,
<a name="line_22"></a>   TP_to_TC(tp0) -~ zp0,
<a name="line_23"></a>   TTC_to_TCpa(z0) -~ zpa0,
<a name="line_24"></a>   TTP_to_TPpa(t0) -~ tpa0,
<a name="line_25"></a>   TC_to_TP(zpa0) -~ tpa0,
<a name="line_26"></a>   TP_to_TC(tpa0) -~ zpa0,
<a name="line_27"></a>   TCp_to_TCpa(zp0) -~ zpa0, 
<a name="line_28"></a>   TPp_to_TPpa(tp0) -~ tpa0, 
<a name="line_29"></a>   TCpa_to_TCp(zpa0) -~ zp0, 
<a name="line_30"></a>   TPpa_to_TPp(tpa0) -~ tp0 
<a name="line_31"></a>  ];
<a name="line_32"></a>  errs := simplify(expand(rationalize(map(op,errs))));
<a name="line_33"></a>  if {op(errs)} <> {0} then
<a name="line_34"></a>   ok := false;
<a name="line_35"></a>   break;
<a name="line_36"></a>  fi;
<a name="line_37"></a> od:
<a name="line_38"></a>
<a name="line_39"></a> _ASSERT(ok,"Diagram for Tp commutes");
<a name="line_40"></a>
<a name="line_41"></a> ok := true:
<a name="line_42"></a> for x0 in F1_samples do
<a name="line_43"></a>  z0   := simplify(expand(rationalize(E_to_TTC(x0))));
<a name="line_44"></a>  zm0  := simplify(expand(rationalize(E_to_TCm(x0))));
<a name="line_45"></a>  zmq0 := simplify(expand(rationalize(E_to_TCmq(x0))));
<a name="line_46"></a>  t0   := simplify(expand(rationalize(E_to_TTP(x0))));
<a name="line_47"></a>  tm0  := simplify(expand(rationalize(E_to_TPm(x0))));
<a name="line_48"></a>  tmq0 := simplify(expand(rationalize(E_to_TPmq(x0))));
<a name="line_49"></a>  errs := [
<a name="line_50"></a>   TTC_to_TTP(z0) -~ t0,
<a name="line_51"></a>   TTP_to_TTC(t0) -~ z0,
<a name="line_52"></a>   TTC_to_TCm(z0) -~ zm0,
<a name="line_53"></a>   TCm_to_TCmq(zm0) -~ zmq0,
<a name="line_54"></a>   TTC_to_TCmq(z0) -~ zmq0,
<a name="line_55"></a>   TTP_to_TPm(t0) -~ tm0,
<a name="line_56"></a>   TPm_to_TPmq(tm0) -~ tmq0,
<a name="line_57"></a>   TTP_to_TPmq(t0) -~ tmq0,
<a name="line_58"></a>   TC_to_TP(zm0) -~ tm0,
<a name="line_59"></a>   TC_to_TP(zmq0) -~ tmq0,
<a name="line_60"></a>   TP_to_TC(tm0) -~ zm0,
<a name="line_61"></a>   TP_to_TC(tmq0) -~ zmq0
<a name="line_62"></a>  ];
<a name="line_63"></a>  errs := simplify(expand(rationalize(map(op,errs))));
<a name="line_64"></a>  if {op(errs)} <> {0} then
<a name="line_65"></a>   ok := false;
<a name="line_66"></a>   break;
<a name="line_67"></a>  fi;
<a name="line_68"></a> od:
<a name="line_69"></a>
<a name="line_70"></a> _ASSERT(ok,"Diagram for Tm and Tmq commutes");
<a name="line_71"></a>end:
<a name="line_72"></a>
<a name="line_73"></a>add_check(check_torus_diagrams):
<a name="line_74"></a>
<a name="line_75"></a>######################################################################
<a name="line_76"></a>
<a name="line_77"></a>check_torus_T := proc()
<a name="line_78"></a> local x0,T,s,k,ca,cb,err;
<a name="line_79"></a>
<a name="line_80"></a> printf("%a()\n",procname);
<a name="line_81"></a>
<a name="line_82"></a> _ASSERT( 
<a name="line_83"></a>  map(FNF_y0,TTP_to_TTC(E_to_TTP_xyr) -~ E_to_TTC_xyr) = [0$4],
<a name="line_84"></a>  "E_to_TTP is consistent with E_to_TTC"
<a name="line_85"></a> );
<a name="line_86"></a>
<a name="line_87"></a> _ASSERT(
<a name="line_88"></a>  map(FNF_y0,simplify(TTP_to_E_generic(E_to_TTP(xx)) -~ xx)) = [0$4],
<a name="line_89"></a>  "TTP_to_E_generic is a left inverse for E_to_TTP"
<a name="line_90"></a> );
<a name="line_91"></a>
<a name="line_92"></a> _ASSERT(
<a name="line_93"></a>  {seq(seq(simplify(TTC_to_E_generic(E_to_TTC(act_R4[T](x0))) -~
<a name="line_94"></a>		    act_R4[T](x0)),
<a name="line_95"></a>   x0 in inner_quasirational_points),T in G16)} = {[0$4]},
<a name="line_96"></a>  "TTC_to_E_generic is a left inverse for E_to_TC"
<a name="line_97"></a>  );
<a name="line_98"></a>
<a name="line_99"></a> _ASSERT(
<a name="line_100"></a>  {seq(seq(simplify(TTC_to_E(E_to_TTC(act_R4[T](x0))) -~
<a name="line_101"></a>		    act_R4[T](x0)),
<a name="line_102"></a>   x0 in quasirational_points),T in G16)} = {[0$4]},
<a name="line_103"></a>  "TTC_to_E is a left inverse for E_to_TC"
<a name="line_104"></a>  );
<a name="line_105"></a>
<a name="line_106"></a> _ASSERT(
<a name="line_107"></a>  {seq(simplify(act_TTP[T](E_to_TTP(xx)) -~ E_to_TTP(act_R4[T](xx))), T in G16)} = {[0$4]},
<a name="line_108"></a>  "E_to_TTP is equivariant"
<a name="line_109"></a> );
<a name="line_110"></a>
<a name="line_111"></a> _ASSERT(
<a name="line_112"></a>  {seq(seq(simplify(act_TTC[T](E_to_TTC(x0)) -~ E_to_TTC(act_R4[T](x0))),
<a name="line_113"></a>	   T in G16),x0 in quasirational_points)} = {[0,0,0,0]},
<a name="line_114"></a>  "E_to_TTC is equivariant"
<a name="line_115"></a> );
<a name="line_116"></a>
<a name="line_117"></a> assume(s::real):
<a name="line_118"></a>
<a name="line_119"></a> for k from 0 to 16 do
<a name="line_120"></a>  if k <= 8 then
<a name="line_121"></a>   ca := simplify(map(convert,map(series,E_to_TTP(annular_chart0[k]([s,u])),u=0,3),polynom,u));
<a name="line_122"></a>   ca := map(a -> `if`(coeff(a,u,-1) <> 0,infinity,subs(u=0,a)),ca);
<a name="line_123"></a>  else
<a name="line_124"></a>   ca := simplify(E_to_TTP(c_E0[k](s)));
<a name="line_125"></a>  fi;
<a name="line_126"></a>  cb := c_TTP[k](s);
<a name="line_127"></a>  err := combine(simplify(subs(infinity=INFINITY,ca) -~ subs(infinity=INFINITY,cb)));
<a name="line_128"></a>
<a name="line_129"></a>  _ASSERT(err = [0$4],sprintf("E_to_TTP(c[%d](t))",k));
<a name="line_130"></a> od:
<a name="line_131"></a>
<a name="line_132"></a> for k from 0 to 16 do
<a name="line_133"></a>  ca := simplify(E_to_TTC(c_E0[k](s)));
<a name="line_134"></a>  cb := c_TTC[k](s);
<a name="line_135"></a>  err := simplify(factor(expand(rationalize(combine(simplify(ca-cb))))));
<a name="line_136"></a>
<a name="line_137"></a>  _ASSERT(err = [0$4],sprintf("E_to_TTC(c[%d](t))",k));
<a name="line_138"></a> od:
<a name="line_139"></a>end:
<a name="line_140"></a>
<a name="line_141"></a>add_check(check_torus_T):
<a name="line_142"></a>
<a name="line_143"></a>######################################################################
<a name="line_144"></a>
<a name="line_145"></a>check_torus_Tp := proc()
<a name="line_146"></a> local F2_samples,x0,T,s,u,z0,k,ca,cb,err;
<a name="line_147"></a>
<a name="line_148"></a> printf("%a()\n",procname);
<a name="line_149"></a>
<a name="line_150"></a> F2_samples := [seq(seq(act_R4[T](inner_quasirational_points[i]),i=1..10),T in L8)]:
<a name="line_151"></a>
<a name="line_152"></a> _ASSERT( 
<a name="line_153"></a>  map(FNF_y0,TP_to_TC(E_to_TPp_xyr) -~ E_to_TCp_xyr) = [0,0],
<a name="line_154"></a>  "E_to_TPp is consistent with E_to_TCp"
<a name="line_155"></a> );
<a name="line_156"></a>
<a name="line_157"></a> _ASSERT(
<a name="line_158"></a>  FNF_y0(simplify(TTP_to_TPp(E_to_TTP(xx)) -~ E_to_TPp(xx))) = [0$2],
<a name="line_159"></a>  "Composite E -> TTP -> TPp"
<a name="line_160"></a> );
<a name="line_161"></a>
<a name="line_162"></a> _ASSERT(
<a name="line_163"></a>  FNF_y0(simplify(TTC_to_TCp(E_to_TTC(xx)) -~ E_to_TCp(xx))) = [0$2],
<a name="line_164"></a>  "Composite E -> TTC -> TCp"
<a name="line_165"></a> );
<a name="line_166"></a>
<a name="line_167"></a> _ASSERT(
<a name="line_168"></a>  map(FNF_y0,simplify(E_to_TPp(TPp_to_E([t[1],t[2]])) -~ [t[1],t[2]])) = [0$2],
<a name="line_169"></a>  "TPp_to_E is a right inverse for E_to_TPp"
<a name="line_170"></a> );
<a name="line_171"></a>
<a name="line_172"></a> assume(s[1]::real,s[2]::real):
<a name="line_173"></a> z0 := [cos(s[1])+I*sin(s[1]),cos(s[2])+I*sin(s[2])]:
<a name="line_174"></a>
<a name="line_175"></a> _ASSERT(
<a name="line_176"></a>  simplify(E_to_TCp(TCp_to_E(z0)) -~ z0) = [0$2],
<a name="line_177"></a>  "TCp_to_E is a right inverse for E_to_TCp"
<a name="line_178"></a> );
<a name="line_179"></a>
<a name="line_180"></a> _ASSERT(
<a name="line_181"></a>  {seq(    simplify(act_TPp[T](E_to_TPp(xx)) -~ E_to_TPp(act_R4[T](xx))),
<a name="line_182"></a>       T in [1,LL,M,LLM,N,LLN,MN,LLMN]),
<a name="line_183"></a>   seq(seq(simplify(act_TPp[T](E_to_TPp(x0)) -~ E_to_TPp(act_R4[T](x0))),
<a name="line_184"></a>       T in [L,LLL,LN,LLLN]),x0 in F2_samples),
<a name="line_185"></a>   seq(seq(simplify(act_TPp[T](E_to_TPp(x0)) -~ E_to_TPp(act_R4[T](x0))),
<a name="line_186"></a>       T in [LM,LLLM,LMN,LLLMN]),x0 in map(act_R4[M],F2_samples))
<a name="line_187"></a>  } = {[0,0]},
<a name="line_188"></a>  "Equivariance properties of E_to_TPp"
<a name="line_189"></a> );
<a name="line_190"></a>
<a name="line_191"></a> assume(s::real,u::real):
<a name="line_192"></a> for k from 0 to 16 do
<a name="line_193"></a>  if k <= 8 then
<a name="line_194"></a>   ca := simplify(E_to_TPp(annular_chart0[k]([s,u])));
<a name="line_195"></a>   ca := map(convert,map(series,ca,u=0,3),polynom,u);
<a name="line_196"></a>   ca := collect(simplify(ca),u);
<a name="line_197"></a>   ca := map(a -> `if`(coeff(a,u,-1) <> 0,infinity,subs(u=0,a)),ca);
<a name="line_198"></a>  else
<a name="line_199"></a>   ca := simplify(E_to_TPp(c_E0[k](s)));
<a name="line_200"></a>  fi;
<a name="line_201"></a>  cb := c_TPp[k](s);
<a name="line_202"></a>  err := combine(simplify(subs(infinity=INFINITY,ca) -~ subs(infinity=INFINITY,cb)));
<a name="line_203"></a>  err := factor(expand(err));
<a name="line_204"></a>
<a name="line_205"></a>  _ASSERT(err = [0$2],sprintf("E_to_TPp(c[%d](t))",k));
<a name="line_206"></a> od:
<a name="line_207"></a>
<a name="line_208"></a> for k from 0 to 16 do
<a name="line_209"></a>  ca := simplify(E_to_TCp(c_E0[k](s)));
<a name="line_210"></a>  cb := c_TCp[k](s);
<a name="line_211"></a>  err := factor(expand(rationalize(combine(simplify(convert(ca-cb,trig))))));
<a name="line_212"></a>
<a name="line_213"></a>  _ASSERT(err = [0$2],sprintf("E_to_TCp(c[%d](t))",k));
<a name="line_214"></a> od:
<a name="line_215"></a>end:
<a name="line_216"></a>
<a name="line_217"></a>add_check(check_torus_Tp):
<a name="line_218"></a>
<a name="line_219"></a>######################################################################
<a name="line_220"></a>
<a name="line_221"></a>check_torus_Tpa := proc()
<a name="line_222"></a> local F2_samples,x0,T,s,u,z0,k,ca,cb,err;
<a name="line_223"></a>
<a name="line_224"></a> printf("%a()\n",procname);
<a name="line_225"></a>
<a name="line_226"></a> F2_samples := [seq(seq(act_R4[T](inner_quasirational_points[i]),i=1..10),T in L8)]:
<a name="line_227"></a>
<a name="line_228"></a> _ASSERT( 
<a name="line_229"></a>  map(FNF_y0,TP_to_TC(E_to_TPpa_xyr) -~ E_to_TCpa_xyr) = [0,0],
<a name="line_230"></a>  "E_to_TPpa is consistent with E_to_TCpa"
<a name="line_231"></a> );
<a name="line_232"></a>
<a name="line_233"></a> _ASSERT(
<a name="line_234"></a>  FNF_y0(simplify(TTP_to_TPpa(E_to_TTP(xx)) -~ E_to_TPpa(xx))) = [0$2],
<a name="line_235"></a>  "Composite E -> TP -> TPpa"
<a name="line_236"></a> );
<a name="line_237"></a>
<a name="line_238"></a> _ASSERT(
<a name="line_239"></a>  FNF_y0(simplify(TTC_to_TCpa(E_to_TTC(xx)) -~ E_to_TCpa(xx))) = [0$2],
<a name="line_240"></a>  "Composite E -> TTC -> TCpa"
<a name="line_241"></a> );
<a name="line_242"></a>
<a name="line_243"></a> _ASSERT(
<a name="line_244"></a>  map(FNF_y0,simplify(E_to_TPpa(TPpa_to_E([t[1],t[2]])) -~ [t[1],t[2]])) = [0$2],
<a name="line_245"></a>  "TPpa_to_E is a right inverse for E_to_TPpa"
<a name="line_246"></a> );
<a name="line_247"></a>
<a name="line_248"></a> assume(s[1]::real,s[2]::real):
<a name="line_249"></a> z0 := [cos(s[1])+I*sin(s[1]),cos(s[2])+I*sin(s[2])]:
<a name="line_250"></a>
<a name="line_251"></a> _ASSERT(
<a name="line_252"></a>  simplify(E_to_TCpa(TCpa_to_E(z0)) -~ z0) = [0$2],
<a name="line_253"></a>  "TCpa_to_E is a right inverse for E_to_TCpa"
<a name="line_254"></a> );
<a name="line_255"></a>
<a name="line_256"></a> _ASSERT(
<a name="line_257"></a>  {seq(    simplify(act_TPpa[T](E_to_TPpa(xx)) -~ E_to_TPpa(act_R4[T](xx))),
<a name="line_258"></a>       T in [1,LL,M,LLM,N,LLN,MN,LLMN]),
<a name="line_259"></a>   seq(seq(simplify(act_TPpa[T](E_to_TPpa(x0)) -~ E_to_TPpa(act_R4[T](x0))),
<a name="line_260"></a>       T in [L,LLL,LN,LLLN]),x0 in F2_samples),
<a name="line_261"></a>   seq(seq(simplify(act_TPpa[T](E_to_TPpa(x0)) -~ E_to_TPpa(act_R4[T](x0))),
<a name="line_262"></a>       T in [LM,LLLM,LMN,LLLMN]),x0 in map(act_R4[M],F2_samples))
<a name="line_263"></a>  } = {[0,0]},
<a name="line_264"></a>  "Equivariance properties of E_to_TPpa"
<a name="line_265"></a> );
<a name="line_266"></a>
<a name="line_267"></a> assume(s::real,u::real):
<a name="line_268"></a> for k from 0 to 16 do
<a name="line_269"></a>  if k <= 8 then
<a name="line_270"></a>   ca := simplify(E_to_TPpa(annular_chart0[k]([s,u])));
<a name="line_271"></a>   ca := map(convert,map(series,ca,u=0,3),polynom,u);
<a name="line_272"></a>   ca := collect(simplify(ca),u);
<a name="line_273"></a>   ca := map(a -> `if`(coeff(a,u,-1) <> 0,infinity,subs(u=0,a)),ca);
<a name="line_274"></a>  else
<a name="line_275"></a>   ca := simplify(E_to_TPpa(c_E0[k](s)));
<a name="line_276"></a>  fi;
<a name="line_277"></a>  cb := c_TPpa[k](s);
<a name="line_278"></a>  err := combine(simplify(subs(infinity=INFINITY,ca) -~ subs(infinity=INFINITY,cb)));
<a name="line_279"></a>  err := factor(expand(err));
<a name="line_280"></a>
<a name="line_281"></a>  _ASSERT(err = [0$2],sprintf("E_to_TPpa(c[%d](t))",k));
<a name="line_282"></a> od:
<a name="line_283"></a>
<a name="line_284"></a> for k from 0 to 16 do
<a name="line_285"></a>  ca := simplify(E_to_TCpa(c_E0[k](s)));
<a name="line_286"></a>  cb := c_TCpa[k](s);
<a name="line_287"></a>  err := factor(expand(rationalize(combine(simplify(convert(ca-cb,trig))))));
<a name="line_288"></a>
<a name="line_289"></a>  _ASSERT(err = [0$2],sprintf("E_to_TCpa(c[%d](t))",k));
<a name="line_290"></a> od:
<a name="line_291"></a>end:
<a name="line_292"></a>
<a name="line_293"></a>add_check(check_torus_Tpa):
<a name="line_294"></a>
<a name="line_295"></a>######################################################################
<a name="line_296"></a>
<a name="line_297"></a>check_torus_Tm := proc()
<a name="line_298"></a> local x0,T,s,z0,k,ca,cb,err;
<a name="line_299"></a>
<a name="line_300"></a> printf("%a()\n",procname);
<a name="line_301"></a>
<a name="line_302"></a> _ASSERT(
<a name="line_303"></a>  {seq(map(NF_x0,map(numer,simplify(
<a name="line_304"></a>       act_TCm[T](E_to_TCm(xx)) -~ E_to_TCm(act_R4[T](xx))))),
<a name="line_305"></a>   T in [1,LL,LM,LLLM,LN,LLLN,MN,LLMN])
<a name="line_306"></a>  } = {[0,0]},
<a name="line_307"></a>  "Equivariance properties of E_to_TCm"
<a name="line_308"></a> );
<a name="line_309"></a>
<a name="line_310"></a> assume(s::real);
<a name="line_311"></a> ca := 0;
<a name="line_312"></a>
<a name="line_313"></a> for k from 0 to 16 do
<a name="line_314"></a>  ca := simplify(E_to_TPm(c_E0[k](s)));
<a name="line_315"></a>  cb := c_TPm[k](s);
<a name="line_316"></a>  ca := factor(expand(rationalize(ca))):
<a name="line_317"></a>  ca := factor(expand(combine(ca)));
<a name="line_318"></a>  cb := factor(expand(rationalize(cb)));
<a name="line_319"></a>  cb := factor(expand(combine(cb)));
<a name="line_320"></a>  err := combine(simplify(subs(infinity=INFINITY,ca) -~ subs(infinity=INFINITY,cb)));
<a name="line_321"></a>  err := factor(expand(err));
<a name="line_322"></a>
<a name="line_323"></a>  _ASSERT(err = [0$2],sprintf("E_to_TPm(c[%d](t))",k));
<a name="line_324"></a> od:
<a name="line_325"></a>
<a name="line_326"></a> for k from 0 to 16 do
<a name="line_327"></a>  ca := simplify(E_to_TCm(c_E0[k](s)));
<a name="line_328"></a>  cb := c_TCm[k](s);
<a name="line_329"></a>  err := factor(expand(rationalize(combine(simplify(convert(ca-cb,trig))))));
<a name="line_330"></a>  err := simplify(err);
<a name="line_331"></a>
<a name="line_332"></a>  _ASSERT(err = [0$2],sprintf("E_to_TCm(c[%d](t))",k));
<a name="line_333"></a> od:
<a name="line_334"></a>
<a name="line_335"></a> _ASSERT(
<a name="line_336"></a>  {seq(simplify(subs(infinity=INFINITY,simplify(map(SC1_to_R,v_TCm[k]))) -~ 
<a name="line_337"></a>		subs(infinity=INFINITY,v_TPm[k])),k=0..18)} = {[0$2]},
<a name="line_338"></a>  "v_TCm and v_TPm"
<a name="line_339"></a> );
<a name="line_340"></a>end:
<a name="line_341"></a>
<a name="line_342"></a>add_check(check_torus_Tm):
<a name="line_343"></a>
<a name="line_344"></a>######################################################################
<a name="line_345"></a>
<a name="line_346"></a>check_torus_Tmq := proc()
<a name="line_347"></a> local F1_samples,x0,T,s,z0,k,ca,cb,err;
<a name="line_348"></a>
<a name="line_349"></a> printf("%a()\n",procname);
<a name="line_350"></a>
<a name="line_351"></a> F1_samples := [seq(seq(act_R4[T](inner_quasirational_points[i]),i=1..10),T in G16)]:
<a name="line_352"></a>
<a name="line_353"></a> _ASSERT( 
<a name="line_354"></a>  {seq(simplify(TTC_to_TCmq(E_to_TTC(x0)) -~ E_to_TCmq(x0)),x0 in F1_samples)} = {[0,0]},
<a name="line_355"></a>  "Composite E -> TTC -> TCmq"
<a name="line_356"></a> );
<a name="line_357"></a>
<a name="line_358"></a> _ASSERT( 
<a name="line_359"></a>  {seq(simplify(TTP_to_TPmq(E_to_TTP(x0)) -~ E_to_TPmq(x0)),x0 in F1_samples)} = {[0,0]},
<a name="line_360"></a>  "Composite E -> TTP -> TPmq"
<a name="line_361"></a> );
<a name="line_362"></a>
<a name="line_363"></a> _ASSERT( 
<a name="line_364"></a>  {seq(simplify(TP_to_TC(E_to_TPmq(x0)) -~ E_to_TCmq(x0)),x0 in F1_samples)} = {[0,0]},
<a name="line_365"></a>  "Composite E -> TPmq -> TCmq"
<a name="line_366"></a> );
<a name="line_367"></a>
<a name="line_368"></a> _ASSERT( 
<a name="line_369"></a>  {seq(simplify(TC_to_TP(E_to_TCmq(x0)) -~ E_to_TPmq(x0)),x0 in F1_samples)} = {[0,0]},
<a name="line_370"></a>  "Composite E -> TCmq -> TPmq"
<a name="line_371"></a> );
<a name="line_372"></a>
<a name="line_373"></a> _ASSERT(
<a name="line_374"></a>  {seq(map(NF_x0,map(numer,simplify(
<a name="line_375"></a>       act_TCmq[T](E_to_TCmq(xx)) -~ E_to_TCmq(act_R4[T](xx))))),
<a name="line_376"></a>   T in [1,LL,LM,LLLM,LN,LLLN,MN,LLMN])
<a name="line_377"></a>  } = {[0,0]},
<a name="line_378"></a>  "Equivariance properties of E_to_TCmq"
<a name="line_379"></a> );
<a name="line_380"></a>
<a name="line_381"></a> assume(s::real);
<a name="line_382"></a>
<a name="line_383"></a> for k from 0 to 16 do
<a name="line_384"></a>  ca := simplify(E_to_TCmq(c_E0[k](s)));
<a name="line_385"></a>  cb := c_TCmq[k](s);
<a name="line_386"></a>  err := factor(expand(rationalize(combine(simplify(convert(ca-cb,trig))))));
<a name="line_387"></a>  err := simplify(err);
<a name="line_388"></a>
<a name="line_389"></a>  _ASSERT(err = [0$2],sprintf("E_to_TCmq(c[%d](t))",k));
<a name="line_390"></a> od:
<a name="line_391"></a>
<a name="line_392"></a> for k in [0,1,2,3,4,5,6,7,8,9,14,15] do
<a name="line_393"></a>  ca := simplify(TPm_to_TPmq(c_TPm[k](s)));
<a name="line_394"></a>  cb := c_TPmq[k](s);
<a name="line_395"></a>  err := combine(simplify(subs(infinity=INFINITY,ca) -~ subs(infinity=INFINITY,cb)));
<a name="line_396"></a>  err := factor(expand(err));
<a name="line_397"></a>
<a name="line_398"></a>  _ASSERT(err = [0$2],sprintf("E_to_TPmq(c[%d](t))",k));
<a name="line_399"></a> od:
<a name="line_400"></a>
<a name="line_401"></a> for k in [10,11,12,13,15,16] do
<a name="line_402"></a>  ca := simplify(map(SC1_to_R,c_TCmq[k](s)));
<a name="line_403"></a>  cb := c_TPmq[k](s);
<a name="line_404"></a>  err := combine(simplify(subs(infinity=INFINITY,ca) -~ subs(infinity=INFINITY,cb)));
<a name="line_405"></a>  err := factor(expand(err));
<a name="line_406"></a>
<a name="line_407"></a>  _ASSERT(err = [0$2],sprintf("E_to_TPmq(c[%d](t))",k));
<a name="line_408"></a> od:
<a name="line_409"></a> _ASSERT(
<a name="line_410"></a>  {seq(simplify(subs(infinity=INFINITY,simplify(map(SC1_to_R,v_TCmq[k]))) -~ 
<a name="line_411"></a>		subs(infinity=INFINITY,v_TPmq[k])),k=0..20)} = {[0$2]},
<a name="line_412"></a>  "v_TCmq and v_TPmq"
<a name="line_413"></a> );
<a name="line_414"></a>end:
<a name="line_415"></a>
<a name="line_416"></a>add_check(check_torus_Tmq):
<a name="line_417"></a>
<a name="line_418"></a>######################################################################
<a name="line_419"></a>
<a name="line_420"></a>check_torus_jacobian := proc()
<a name="line_421"></a> local DT,Jp,jp,Jpa,jpa,Jm,jm,Jmq,jmq;
<a name="line_422"></a>
<a name="line_423"></a> printf("%a()\n",procname);
<a name="line_424"></a>
<a name="line_425"></a> DT := (t) -> [seq(2*diff(t,x[i])/(1+t^2),i=1..4)];
<a name="line_426"></a>
<a name="line_427"></a> Jp := Matrix(simplify([xx,dg0(xx),op(map(DT,E_to_TPp(xx)))])):
<a name="line_428"></a> Jp := map(u -> subs(root_rule,numer(u))/subs(root_rule,denom(u)),Jp):
<a name="line_429"></a> jp := FNF_y0(Determinant(Jp)):
<a name="line_430"></a> _ASSERT(FNF_y0(E_to_TPp_jacobian - jp) = 0,
<a name="line_431"></a>         "Jacobian of E_to_TPp");
<a name="line_432"></a>
<a name="line_433"></a> Jm := Matrix(simplify([xx,dg0(xx),op(map(DT,E_to_TPm(xx)))])):
<a name="line_434"></a> Jm := map(u -> subs(root_rule,numer(u))/subs(root_rule,denom(u)),Jm):
<a name="line_435"></a> jm := FNF_y0(Determinant(Jm)):
<a name="line_436"></a> _ASSERT(FNF_y0(E_to_TPm_jacobian - jm) = 0,
<a name="line_437"></a>         "Jacobian of E_to_TPm");
<a name="line_438"></a>
<a name="line_439"></a> # This is commented out because it is very slow.
<a name="line_440"></a># Jmq := Matrix(simplify([xx,dg0(xx),op(map(DT,E_to_TPmq(xx)))])):
<a name="line_441"></a># Jmq := map(u -> subs(root_rule,numer(u))/subs(root_rule,denom(u)),Jmq):
<a name="line_442"></a># jmq := FNF_y0(Determinant(Jmq)):
<a name="line_443"></a># _ASSERT(FNF_y0(E_to_TPmq_jacobian - jmq) = 0,
<a name="line_444"></a>#         "Jacobian of E_to_TPmq");
<a name="line_445"></a>
<a name="line_446"></a> Jpa := Matrix(simplify([xx,dg0(xx),op(map(DT,E_to_TPpa(xx)))])):
<a name="line_447"></a> Jpa := map(u -> subs(root_rule,numer(u))/subs(root_rule,denom(u)),Jpa):
<a name="line_448"></a> jpa := FNF_y0(Determinant(Jpa)):
<a name="line_449"></a> _ASSERT(FNF_y0(E_to_TPpa_jacobian - jpa) = 0,
<a name="line_450"></a>         "Jacobian of E_to_TPpa");
<a name="line_451"></a>
<a name="line_452"></a>end:
<a name="line_453"></a>
<a name="line_454"></a>add_check(check_torus_jacobian):
<a name="line_455"></a>
<a name="line_456"></a>######################################################################
<a name="line_457"></a>
<a name="line_458"></a>check_torus_inverse := proc()
<a name="line_459"></a> local RR_samples,RR_2_samples,Q_samples,GQ_samples,
<a name="line_460"></a>       a,ok,x0,x1,x2,a1,b1,i2,T,err,k,t0,t1;
<a name="line_461"></a>
<a name="line_462"></a> printf("%a()\n",procname);
<a name="line_463"></a>
<a name="line_464"></a> assume(a[1]::real); 
<a name="line_465"></a> assume(a[2]::real);
<a name="line_466"></a>
<a name="line_467"></a> err := simplify(rho(TPp_to_E([a[1],a[2]]))-1);
<a name="line_468"></a> _ASSERT(err = 0,"TPp_to_E lands in S3");
<a name="line_469"></a>
<a name="line_470"></a> err := simplify(g0(TPp_to_E([a[1],a[2]])));
<a name="line_471"></a> _ASSERT(err = 0,"TPp_to_E lands in EX^*");
<a name="line_472"></a>
<a name="line_473"></a> err := simplify(E_to_TPp(TPp_to_E([a[1],a[2]])) -~ [a[1],a[2]]);
<a name="line_474"></a>
<a name="line_475"></a> _ASSERT(err = [0,0],"E_to_TPp o TPp_to_E");
<a name="line_476"></a>
<a name="line_477"></a> err := simplify(rho(TPpa_to_E([a[1],a[2]]))-1);
<a name="line_478"></a> _ASSERT(err = 0,"TPpa_to_E lands in S3");
<a name="line_479"></a>
<a name="line_480"></a> err := simplify(g0(TPpa_to_E([a[1],a[2]])));
<a name="line_481"></a> _ASSERT(err = 0,"TPpa_to_E lands in EX^*");
<a name="line_482"></a>
<a name="line_483"></a> err := simplify(E_to_TPpa(TPpa_to_E([a[1],a[2]])) -~ [a[1],a[2]]);
<a name="line_484"></a>
<a name="line_485"></a> _ASSERT(err = [0,0],"E_to_TPpa o TPpa_to_E");
<a name="line_486"></a>
<a name="line_487"></a> RR_samples := sort([seq(seq(1.1*(-2)^i*3^j,i=-2..2),j=-2..2)]);
<a name="line_488"></a> RR_2_samples := [seq(seq([t1,1.3*t2],t1 in RR_samples),t2 in RR_samples)]:
<a name="line_489"></a> Q_samples := [seq(inner_quasirational_points[10*i],i=1..10)];
<a name="line_490"></a> GQ_samples := map(op,[seq(map(act_R4[T],Q_samples),T in G16)]);
<a name="line_491"></a>
<a name="line_492"></a> ok := true:
<a name="line_493"></a> for t0 in RR_2_samples do
<a name="line_494"></a>  for k from 1 to 4 do
<a name="line_495"></a>   x0 := evalf(TPmq_to_E(evalf(t0),k));
<a name="line_496"></a>   t1 := evalf(E_to_TPmq(x0));
<a name="line_497"></a>   err := max(abs(evalf(rho(x0)-1)),abs(evalf(g0(x0))),d2f(t0,t1));
<a name="line_498"></a>   if err > 10.^(-90) then
<a name="line_499"></a>    ok := false;
<a name="line_500"></a>    break;
<a name="line_501"></a>   fi;
<a name="line_502"></a>  od;
<a name="line_503"></a> od:
<a name="line_504"></a>
<a name="line_505"></a> _ASSERT(ok,"E_to_TPmq o TPmq_to_E");
<a name="line_506"></a>
<a name="line_507"></a> ok := true;
<a name="line_508"></a>
<a name="line_509"></a> for x1 in GQ_samples do
<a name="line_510"></a>  a1 := simplify(E_to_TPp(x1));
<a name="line_511"></a>  x2 := simplify(TPp_to_E(a1));
<a name="line_512"></a>  if is(x1[3] < 0) then x2 := act_R4[M](x2); fi;
<a name="line_513"></a>  if simplify(x1 -~ x2) <> [0$4] then
<a name="line_514"></a>   ok := false; break;
<a name="line_515"></a>  fi;
<a name="line_516"></a> od:
<a name="line_517"></a>
<a name="line_518"></a> _ASSERT(ok,"TPp_to_E o E_to_TPp");
<a name="line_519"></a>
<a name="line_520"></a> ok := true;
<a name="line_521"></a>
<a name="line_522"></a> for x1 in GQ_samples do
<a name="line_523"></a>  a1 := simplify(E_to_TPpa(x1));
<a name="line_524"></a>  x2 := simplify(TPpa_to_E(a1));
<a name="line_525"></a>  if is(x1[3] < 0) then x2 := act_R4[M](x2); fi;
<a name="line_526"></a>  if simplify(x1 -~ x2) <> [0$4] then
<a name="line_527"></a>   ok := false; break;
<a name="line_528"></a>  fi;
<a name="line_529"></a> od:
<a name="line_530"></a>
<a name="line_531"></a> _ASSERT(ok,"TPpa_to_E o E_to_TPpa");
<a name="line_532"></a>
<a name="line_533"></a> ok := true;
<a name="line_534"></a>
<a name="line_535"></a> for x1 in GQ_samples do
<a name="line_536"></a>  a1 := simplify(combine(expand(rationalize(E_to_TPm(x1)))));
<a name="line_537"></a>  b1 := simplify(E_to_TPmq(x1));
<a name="line_538"></a>  if is(x1[3] > 0) then
<a name="line_539"></a>   if is(simplify((a1[1]*a1[2])^2 - 1) > 0) then
<a name="line_540"></a>    i2 := 1;
<a name="line_541"></a>   else 
<a name="line_542"></a>    i2 := 2;
<a name="line_543"></a>   fi;
<a name="line_544"></a>  else
<a name="line_545"></a>   if is(simplify((a1[1]*a1[2])^2 - 1) > 0) then
<a name="line_546"></a>    i2 := 3;
<a name="line_547"></a>   else 
<a name="line_548"></a>    i2 := 4;
<a name="line_549"></a>   fi;
<a name="line_550"></a>  fi;
<a name="line_551"></a>  x2 := simplify(TPmq_to_E(b1,i2)):
<a name="line_552"></a>  err := d4f(x1,x2);
<a name="line_553"></a>  if (err > 10.^(-80)) then
<a name="line_554"></a>   ok := false; break;
<a name="line_555"></a>  fi;
<a name="line_556"></a> od:
<a name="line_557"></a>
<a name="line_558"></a> _ASSERT(ok,"TPmq_to_E o E_to_TPmq");
<a name="line_559"></a>end:
<a name="line_560"></a>
<a name="line_561"></a>add_check(check_torus_inverse):
  </pre>
 </body>
</html>
    