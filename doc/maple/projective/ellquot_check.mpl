<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_ellquot := proc()
<a name="line_2"></a> local errs,JMp,JMm,rels,i,k,kt,t0,v0,w0,x0,y0,z0;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> JMp := Matrix([seq([seq(diff(Ep_rel[i](z),z[j]),j=1..4)],i=0..1)]);
<a name="line_7"></a> rels := factor([Ep_rel[0](z),Ep_rel[1](z),seq(seq(Determinant(SubMatrix(JMp,[1,2],[i,j])),j=i+1..4),i=1..3)]):
<a name="line_8"></a> _ASSERT(
<a name="line_9"></a>  [seq(solve([z[i]-1,op(rels)],[z[1],z[2],z[3],z[4]]),i=1..4)] = [[]$4],
<a name="line_10"></a>  "Jacobian criterion of smoothness for Ep"
<a name="line_11"></a> );
<a name="line_12"></a>
<a name="line_13"></a> JMm := Matrix([seq([seq(diff(Em_rel[i](z),z[j]),j=1..4)],i=0..1)]);
<a name="line_14"></a> rels := factor([Em_rel[0](z),Em_rel[1](z),seq(seq(Determinant(SubMatrix(JMm,[1,2],[i,j])),j=i+1..4),i=1..3)]):
<a name="line_15"></a> _ASSERT(
<a name="line_16"></a>  [seq(solve([z[i]-1,op(rels)],[z[1],z[2],z[3],z[4]]),i=1..4)] = [[]$4],
<a name="line_17"></a>  "Jacobian criterion of smoothness for Em"
<a name="line_18"></a> );
<a name="line_19"></a>
<a name="line_20"></a> _ASSERT(
<a name="line_21"></a>  map(e -> map(NF_P,e),is_member_Ep(P_to_Ep(z))),
<a name="line_22"></a>  "P_to_Ep lands in Ep"
<a name="line_23"></a> );
<a name="line_24"></a>
<a name="line_25"></a> _ASSERT(
<a name="line_26"></a>  map(e -> map(NF_P,e),is_member_Em(P_to_Em(z))),
<a name="line_27"></a>  "P_to_Em lands in Em"
<a name="line_28"></a> );
<a name="line_29"></a>
<a name="line_30"></a> _ASSERT(`and`(seq(is_equal_Ep(v_Ep[i],P_to_Ep(v_P[i])),i in {0,seq(j,j=0..13)})),
<a name="line_31"></a>  "P_to_Ep sends vertices to vertices"
<a name="line_32"></a> );
<a name="line_33"></a>
<a name="line_34"></a> _ASSERT(`and`(seq(is_equal_Em(v_Em[i],P_to_Em(v_P[i])),i in {0,seq(j,j=0..13)})),
<a name="line_35"></a>  "P_to_Em sends vertices to vertices"
<a name="line_36"></a> );
<a name="line_37"></a>
<a name="line_38"></a> _ASSERT(
<a name="line_39"></a>  `and`(
<a name="line_40"></a>   seq(map(NF_P,is_equal_Ep_list(P_to_Ep(act_P[T](z)),act_Ep[T](P_to_Ep(z)))) = [0$6],
<a name="line_41"></a>       T in [1,LL,M,LLM]),
<a name="line_42"></a>   seq(map(NF_P,map(conjugate,is_equal_Ep_list(P_to_Ep(act_P[T](z)),act_Ep[T](P_to_Ep(z))))) = [0$6],
<a name="line_43"></a>       T in [N,LLN,MN,LLMN])),
<a name="line_44"></a>   "equivariance properties of P_to_Ep"
<a name="line_45"></a> );
<a name="line_46"></a>
<a name="line_47"></a> _ASSERT(
<a name="line_48"></a>  `and`(
<a name="line_49"></a>   seq(map(NF_P,is_equal_Em_list(P_to_Em(act_P[T](z)),act_Em[T](P_to_Em(z)))) = [0$6],
<a name="line_50"></a>       T in [1,LL,LM,LLLM]),
<a name="line_51"></a>   seq(map(NF_P,map(conjugate,is_equal_Em_list(P_to_Em(act_P[T](z)),act_Em[T](P_to_Em(z))))) = [0$6],
<a name="line_52"></a>       T in [LN,LLLN,MN,LLMN])),
<a name="line_53"></a>   "equivariance properties of P_to_Em"
<a name="line_54"></a> );
<a name="line_55"></a>
<a name="line_56"></a> _ASSERT(
<a name="line_57"></a>  factor(Ep_rel[0](j_Ep([y,x])) - is_member_Ep_0([y,x])) = 0 and
<a name="line_58"></a>  factor(Ep_rel[1](j_Ep([y,x]))) = 0,
<a name="line_59"></a>  "j sends Ep_0 to Ep"
<a name="line_60"></a> );
<a name="line_61"></a>
<a name="line_62"></a> _ASSERT(
<a name="line_63"></a>  factor(Em_rel[0](j_Em([y,x])) - is_member_Em_0([y,x])) = 0 and
<a name="line_64"></a>  factor(Em_rel[1](j_Em([y,x]))) = 0,
<a name="line_65"></a>  "j sends Em_0 to Em"
<a name="line_66"></a> );
<a name="line_67"></a>
<a name="line_68"></a> _ASSERT(
<a name="line_69"></a>  factor(is_member_Ep_0(P_to_Ep_0([w,z])) - 4*  (1-z)^2/(1+z^2)^4*(w^2 - r_P(z))) = 0,
<a name="line_70"></a>  "P_to_Ep_0 lands in Ep_0"
<a name="line_71"></a> );
<a name="line_72"></a>
<a name="line_73"></a> _ASSERT(
<a name="line_74"></a>  factor(is_member_Em_0(P_to_Em_0([w,z])) - 4*I*(I+z)^2/(1-z^2)^4*(w^2 - r_P(z))) = 0,
<a name="line_75"></a>  "P_to_Em_0 lands in Em_0"
<a name="line_76"></a> );
<a name="line_77"></a>
<a name="line_78"></a> errs := {seq(P_to_Ep_0(act_P_0[T]([w,z])) -~ act_Ep_0[T](P_to_Ep_0([w,z])),
<a name="line_79"></a>          T in {1,LL,M,LLM,N,LLN,MN,LLMN})};
<a name="line_80"></a> errs := factor(expand(factor(expand(errs))));
<a name="line_81"></a>
<a name="line_82"></a> _ASSERT(errs = {[0,0]},
<a name="line_83"></a>  "Equivariance properties of P_to_Ep_0" 
<a name="line_84"></a> );
<a name="line_85"></a>
<a name="line_86"></a> errs := {seq(P_to_Em_0(act_P_0[T]([w,z])) -~ act_Em_0[T](P_to_Em_0([w,z])),
<a name="line_87"></a>          T in {1,LL,LM,LLLM,LN,LLLN,MN,LLMN})};
<a name="line_88"></a> errs := factor(expand(factor(expand(errs))));
<a name="line_89"></a>
<a name="line_90"></a> _ASSERT(errs = {[0,0]},
<a name="line_91"></a>  "Equivariance properties of P_to_Em_0" 
<a name="line_92"></a> );
<a name="line_93"></a>
<a name="line_94"></a> w0,z0 := op(Ep_to_P_0([y0,x0]));
<a name="line_95"></a> _ASSERT(
<a name="line_96"></a>  factor(subs(y0 = sqrt(q_Ep(x0)),r_P(z0) - w0^2)) = 0,
<a name="line_97"></a>  "Ep_to_P_0 lands in P_0"
<a name="line_98"></a> );
<a name="line_99"></a> 
<a name="line_100"></a> _ASSERT(
<a name="line_101"></a>  simplify(act_P_0[M]([w0,z0]) -~ subs(sqrt(1-x0^2)=-sqrt(1-x0^2),[w0,z0])) = [0,0],
<a name="line_102"></a>  "Ep_to_P_0 interacts as expected with action of M"
<a name="line_103"></a> );
<a name="line_104"></a> 
<a name="line_105"></a> _ASSERT(
<a name="line_106"></a>  factor(P_to_Ep_0(Ep_to_P_0([y0,x0])) -~ [y0,x0]) = [0,0],
<a name="line_107"></a>  "P_to_Ep_0 o Ep_to_P_0 = 1"
<a name="line_108"></a> );
<a name="line_109"></a>
<a name="line_110"></a> w0,z0 := op(Em_to_P_0([y0,x0]));
<a name="line_111"></a> _ASSERT(
<a name="line_112"></a>  factor(subs(y0 = sqrt(q_Em(x0)),r_P(z0) - w0^2)) = 0,
<a name="line_113"></a>  "Em_to_P_0 lands in P_0"
<a name="line_114"></a> );
<a name="line_115"></a>  
<a name="line_116"></a> _ASSERT(
<a name="line_117"></a>  simplify(act_P_0[LM]([w0,z0]) -~ subs(sqrt(1-x0^2)=-sqrt(1-x0^2),[w0,z0])) = [0,0],
<a name="line_118"></a>  "Ep_to_P_0 interacts as expected with action of LM"
<a name="line_119"></a> );
<a name="line_120"></a>  
<a name="line_121"></a> _ASSERT(
<a name="line_122"></a>  factor(P_to_Em_0(Em_to_P_0([y0,x0])) -~ [y0,x0]) = [0,0],
<a name="line_123"></a>  "P_to_Em_0 o Em_to_P_0 = 1"
<a name="line_124"></a> );  
<a name="line_125"></a> 
<a name="line_126"></a> _ASSERT(
<a name="line_127"></a>  {seq(simplify(is_member_Ep_0(v_Ep_0[i])), i in {0,1,2,3,4,5,10,11,12,13})} = {0},
<a name="line_128"></a>  "vertices in the affine curve Ep_0"
<a name="line_129"></a> );
<a name="line_130"></a>
<a name="line_131"></a> for i in [0,2,3,4,5,10,11,12,13] do 
<a name="line_132"></a>  _ASSERT(factor(P_to_Ep_0(pq_P(v_P[i])) -~ v_Ep_0[i]) = [0,0],
<a name="line_133"></a>         sprintf("formula for v_EPp_0[%d]",i));
<a name="line_134"></a> od;
<a name="line_135"></a>
<a name="line_136"></a> kt[ 1] := [1,   Pi  ];
<a name="line_137"></a> kt[ 6] := [0,   Pi/4];
<a name="line_138"></a> kt[ 7] := [0, 3*Pi/4];
<a name="line_139"></a> kt[ 8] := [0,-3*Pi/4];
<a name="line_140"></a> kt[ 9] := [0,  -Pi/4];
<a name="line_141"></a> 
<a name="line_142"></a> for i in [1,6,7,8,9] do
<a name="line_143"></a>  k,t0 := op(kt[i]);
<a name="line_144"></a>  v0 := map(limit,P_to_Ep_0(pq_P(c_P[k](t))),t=t0,complex);
<a name="line_145"></a>  v0 := map(u -> `if`(type(u,cx_infinity),infinity,u),v0);
<a name="line_146"></a>  _ASSERT(v_on_c[i,k] = t0 and v0 = v_Ep_0[i],
<a name="line_147"></a>           sprintf("formula for v_Ep_0[%d]",i));
<a name="line_148"></a> od:
<a name="line_149"></a>
<a name="line_150"></a> _ASSERT(
<a name="line_151"></a>  {seq(simplify(is_member_Em_0(v_Em_0[i])), i in {0,1,6,7,8,9,10,11,12,13})} = {0},
<a name="line_152"></a>  "vertices in the affine curve Ep_0"
<a name="line_153"></a> );
<a name="line_154"></a>
<a name="line_155"></a> for i in [0,6,7,8,9,10,11,12,13] do 
<a name="line_156"></a>  _ASSERT(factor(P_to_Em_0(pq_P(v_P[i])) -~ v_Em_0[i]) = [0,0],
<a name="line_157"></a>         sprintf("formula for v_Em_0[%d]",i));
<a name="line_158"></a> od;
<a name="line_159"></a>
<a name="line_160"></a> kt[ 1] := [1,  Pi  ];
<a name="line_161"></a> kt[ 2] := [0,  0   ];
<a name="line_162"></a> kt[ 3] := [0,  Pi/2];
<a name="line_163"></a> kt[ 4] := [0,  Pi  ];
<a name="line_164"></a> kt[ 5] := [0, -Pi/2];
<a name="line_165"></a> 
<a name="line_166"></a> for i in [1,2,3,4,5] do
<a name="line_167"></a>  k,t0 := op(kt[i]);
<a name="line_168"></a>  v0 := map(limit,P_to_Em_0(pq_P(c_P[k](t))),t=t0,complex);
<a name="line_169"></a>  v0 := map(u -> `if`(type(u,cx_infinity),infinity,u),v0);
<a name="line_170"></a>  _ASSERT(v_on_c[i,k] = t0 and v0 = v_Em_0[i],
<a name="line_171"></a>          sprintf("formula for v_Em_0[%d]",i));
<a name="line_172"></a> od:
<a name="line_173"></a>
<a name="line_174"></a> _ASSERT(
<a name="line_175"></a>  is(convert(series(P_to_Ep_0(pq_P(c_P[5](t)))[1],t=0,3),polynom,t)/t > 0) and
<a name="line_176"></a>  is(convert(series(P_to_Ep_0(pq_P(c_P[1](t)))[1],t=0,3),polynom,t)/(1+I)/t > 0),
<a name="line_177"></a>  "P_to_Ep_0 to first order near v[0]");
<a name="line_178"></a>
<a name="line_179"></a> _ASSERT(
<a name="line_180"></a>  is(convert(series(P_to_Em_0(pq_P(c_P[5](t)))[1],t=0,3),polynom,t)/(1-I)/t > 0) and
<a name="line_181"></a>  is(convert(series(P_to_Em_0(pq_P(c_P[1](t)))[1],t=0,3),polynom,t)/t > 0),
<a name="line_182"></a>  "P_to_Em_0 to first order near v[0]");
<a name="line_183"></a>
<a name="line_184"></a> _ASSERT(
<a name="line_185"></a> simplify(P_to_Ep_0(pq_P(c_P[5](t)))[2] - ( 2/(a_P*sin(t/2)^2 + 1/(a_P*sin(t/2)^2))))=0,
<a name="line_186"></a>  "x-coordinate of c[5] in Ep_0"
<a name="line_187"></a> );
<a name="line_188"></a> 
<a name="line_189"></a> _ASSERT(
<a name="line_190"></a> simplify(P_to_Ep_0(pq_P(c_P[6](t)))[2] - (-2/(a_P*sin(t/2)^2 + 1/(a_P*sin(t/2)^2))))=0,
<a name="line_191"></a>  "x-coordinate of c[6] in Ep_0"
<a name="line_192"></a> );
<a name="line_193"></a>
<a name="line_194"></a> _ASSERT(
<a name="line_195"></a> simplify(P_to_Ep_0(pq_P(c_P[7](t)))[2] - ( 2/(a_P*sin(t/2)^2 + 1/(a_P*sin(t/2)^2))))=0,
<a name="line_196"></a>  "x-coordinate of c[7] in Ep_0"
<a name="line_197"></a> );
<a name="line_198"></a>
<a name="line_199"></a> _ASSERT(
<a name="line_200"></a> simplify(P_to_Ep_0(pq_P(c_P[8](t)))[2] - (-2/(a_P*sin(t/2)^2 + 1/(a_P*sin(t/2)^2))))=0,
<a name="line_201"></a>  "x-coordinate of c[8] in Ep_0"
<a name="line_202"></a> );
<a name="line_203"></a>
<a name="line_204"></a>end:
<a name="line_205"></a>
<a name="line_206"></a>add_check(check_ellquot):
<a name="line_207"></a>
<a name="line_208"></a>######################################################################
<a name="line_209"></a>
<a name="line_210"></a>check_ellquot_origin := proc()
<a name="line_211"></a>
<a name="line_212"></a> printf("%a()\n",procname);
<a name="line_213"></a>
<a name="line_214"></a> _ASSERT(
<a name="line_215"></a>  simplify(map(convert,map(series,j_inv_P(c_Ep[5](t)),t=0,2),polynom,t) -~ [sqrt(a_P)*t,0]) = [0,0],
<a name="line_216"></a>  "c_Ep[5] near the origin"
<a name="line_217"></a> );
<a name="line_218"></a> 
<a name="line_219"></a> _ASSERT(
<a name="line_220"></a>  simplify(map(convert,map(series,j_inv_P(c_Ep[1](t)),t=0,2),polynom,t) -~ [exp(I*Pi/4)*t,0]) = [0,0],
<a name="line_221"></a>  "c_Ep[1] near the origin"
<a name="line_222"></a> );
<a name="line_223"></a> 
<a name="line_224"></a> _ASSERT(
<a name="line_225"></a>  simplify(map(convert,map(series,j_inv_P(c_Em[5](t)),t=0,2),polynom,t) -~ [exp(-I*Pi/4)*sqrt(a_P)*t,0]) = [0,0],
<a name="line_226"></a>  "c_Em[5] near the origin"
<a name="line_227"></a> );
<a name="line_228"></a>
<a name="line_229"></a> _ASSERT(
<a name="line_230"></a>  simplify(map(convert,map(series,j_inv_P(c_Em[1](t)),t=0,2),polynom,t) -~ [t,0]) = [0,0],
<a name="line_231"></a>  "c_Em[1] near the origin"
<a name="line_232"></a> );  
<a name="line_233"></a>
<a name="line_234"></a>end:
<a name="line_235"></a>
<a name="line_236"></a>add_check(check_ellquot_origin);
<a name="line_237"></a>
<a name="line_238"></a>######################################################################
<a name="line_239"></a>
<a name="line_240"></a>check_translations := proc()
<a name="line_241"></a> local i;
<a name="line_242"></a> 
<a name="line_243"></a> printf("%a()\n",procname);
<a name="line_244"></a>
<a name="line_245"></a> _ASSERT(`and`(seq(factor(is_member_Ep(Ep_e[i])),i=0..3),
<a name="line_246"></a>               seq(factor(is_member_Em(Em_e[i])),i=0..3)),
<a name="line_247"></a>  "Points Ep_e[i] and Em_e[i] lie in Ep and Em"
<a name="line_248"></a> );
<a name="line_249"></a>
<a name="line_250"></a> _ASSERT(
<a name="line_251"></a>  {seq(seq(NF_Ep(factor(Ep_rel[i](Ep_trans[j](z)))),i=0..1),j=0..3)} = {0},
<a name="line_252"></a>  "translations preserve Ep"
<a name="line_253"></a> );
<a name="line_254"></a>
<a name="line_255"></a> _ASSERT(
<a name="line_256"></a>  {seq(seq(NF_Em(factor(Em_rel[i](Em_trans[j](z)))),i=0..1),j=0..3)} = {0},
<a name="line_257"></a>  "translations preserve Em"
<a name="line_258"></a> );
<a name="line_259"></a>
<a name="line_260"></a> _ASSERT(
<a name="line_261"></a>  {factor(Ep_trans[1](Ep_trans[1](z)) -~ Ep_trans[0](z)),
<a name="line_262"></a>   factor(Ep_trans[2](Ep_trans[2](z)) -~ Ep_trans[0](z)),
<a name="line_263"></a>   factor(Ep_trans[3](Ep_trans[3](z)) -~ Ep_trans[0](z)),
<a name="line_264"></a>   factor(Ep_trans[1](Ep_trans[2](z)) -~ Ep_trans[3](z)),
<a name="line_265"></a>   factor(Ep_trans[2](Ep_trans[3](z)) -~ Ep_trans[1](z)),
<a name="line_266"></a>   factor(Ep_trans[3](Ep_trans[1](z)) -~ Ep_trans[2](z))} = {[0$4]},
<a name="line_267"></a>  "translations of Ep compose correctly"
<a name="line_268"></a> );
<a name="line_269"></a>
<a name="line_270"></a> _ASSERT(
<a name="line_271"></a>  {factor(Em_trans[1](Em_trans[1](z)) -~ Em_trans[0](z)),
<a name="line_272"></a>   factor(Em_trans[2](Em_trans[2](z)) -~ Em_trans[0](z)),
<a name="line_273"></a>   factor(Em_trans[3](Em_trans[3](z)) -~ Em_trans[0](z)),
<a name="line_274"></a>   factor(Em_trans[1](Em_trans[2](z)) -~ Em_trans[3](z)),
<a name="line_275"></a>   factor(Em_trans[2](Em_trans[3](z)) -~ Em_trans[1](z)),
<a name="line_276"></a>   factor(Em_trans[3](Em_trans[1](z)) -~ Em_trans[2](z))} = {[0$4]},
<a name="line_277"></a>  "translations of Em compose correctly"
<a name="line_278"></a> );
<a name="line_279"></a>
<a name="line_280"></a> for i from 1 to 3 do
<a name="line_281"></a>  _ASSERT(is_equal_Ep(Ep_trans[i](Ep_e[0]),Ep_e[i]),
<a name="line_282"></a>          sprintf("Ep_trans[%d] sends e[0] to e[%d]",i,i));
<a name="line_283"></a>  _ASSERT(is_equal_Em(Em_trans[i](Em_e[0]),Em_e[i]),
<a name="line_284"></a>          sprintf("Em_trans[%d] sends e[0] to e[%d]",i,i));
<a name="line_285"></a> od:
<a name="line_286"></a> 
<a name="line_287"></a> _ASSERT(
<a name="line_288"></a>  `and`(seq(is_equal_Ep(Ep_trans[i](j_Ep([y,x])),j_Ep(Ep_0_trans[i]([y,x]))),i=0..3)),
<a name="line_289"></a>  "translations on Ep and Ep_0 are compatible"
<a name="line_290"></a> );
<a name="line_291"></a> 
<a name="line_292"></a> _ASSERT(
<a name="line_293"></a>  `and`(seq(is_equal_Em(Em_trans[i](j_Em([y,x])),j_Em(Em_0_trans[i]([y,x]))),i=0..3)),
<a name="line_294"></a>  "translations on Em and Em_0 are compatible"
<a name="line_295"></a> );  
<a name="line_296"></a>
<a name="line_297"></a> _ASSERT({seq(factor(is_member_Ep_0(Ep_0_e[i])),i=0..3),
<a name="line_298"></a>          seq(factor(is_member_Em_0(Em_0_e[i])),i=0..3)} = {0},
<a name="line_299"></a>  "Points Ep_0_e[i] and Em_e_0[i] lie in Ep_0 and Em_0"
<a name="line_300"></a> );
<a name="line_301"></a>
<a name="line_302"></a> _ASSERT(
<a name="line_303"></a>  factor(is_member_Ep_0(Ep_0_trans[1]([y,x])) - bm_P^4/(bp_P^2*x-1)^4*is_member_Ep_0([y,x])) = 0 and
<a name="line_304"></a>  factor(is_member_Ep_0(Ep_0_trans[2]([y,x])) - (4*(1-1/bp_P)^2/(bp_P*x+1-2*x)^4*is_member_Ep_0([y,x]))) = 0 and
<a name="line_305"></a>  factor(is_member_Ep_0(Ep_0_trans[3]([y,x])) - (4*(1+1/bp_P)^2/(bp_P*x-1+2*x)^4*is_member_Ep_0([y,x]))) = 0,
<a name="line_306"></a>  "Ep_0_trans[i] preserves Ep_0"
<a name="line_307"></a> );
<a name="line_308"></a>
<a name="line_309"></a> _ASSERT(
<a name="line_310"></a>  factor(is_member_Em_0(Em_0_trans[1]([y,x])) - bp_P^4/(bm_P^2*x+1)^4*is_member_Em_0([y,x])) = 0 and
<a name="line_311"></a>  factor(is_member_Em_0(Em_0_trans[2]([y,x])) - 4*(1-I/bm_P)^2/(bm_P*x+I-2*I*x)^4*is_member_Em_0([y,x])) = 0 and
<a name="line_312"></a>  factor(is_member_Em_0(Em_0_trans[3]([y,x])) - 4*(1+I/bm_P)^2/(bm_P*x-I+2*I*x)^4*is_member_Em_0([y,x])) = 0,
<a name="line_313"></a>  "Em_0_trans[i] preserves Em_0"
<a name="line_314"></a> );
<a name="line_315"></a>
<a name="line_316"></a> _ASSERT({
<a name="line_317"></a>   factor(Ep_0_trans[1](Ep_0_e[0]) -~ Ep_0_e[1]),
<a name="line_318"></a>   factor(Ep_0_trans[1](Ep_0_e[1]) -~ Ep_0_e[0]),
<a name="line_319"></a>   factor(Ep_0_trans[1](Ep_0_e[2]) -~ Ep_0_e[3]),
<a name="line_320"></a>   factor(Ep_0_trans[1](Ep_0_e[3]) -~ Ep_0_e[2]),
<a name="line_321"></a>   factor(Ep_0_trans[2](Ep_0_e[0]) -~ Ep_0_e[2]),
<a name="line_322"></a>   factor(Ep_0_trans[2](Ep_0_e[1]) -~ Ep_0_e[3]),
<a name="line_323"></a>   factor(Ep_0_trans[2](Ep_0_e[2]) -~ Ep_0_e[0]),
<a name="line_324"></a>   factor(Ep_0_trans[2](Ep_0_e[3]) -~ Ep_0_e[1]),
<a name="line_325"></a>   factor(Ep_0_trans[3](Ep_0_e[0]) -~ Ep_0_e[3]),
<a name="line_326"></a>   factor(Ep_0_trans[3](Ep_0_e[1]) -~ Ep_0_e[2]),
<a name="line_327"></a>   factor(Ep_0_trans[3](Ep_0_e[2]) -~ Ep_0_e[1]),
<a name="line_328"></a>   factor(Ep_0_trans[3](Ep_0_e[3]) -~ Ep_0_e[0]),
<a name="line_329"></a>   factor(Em_0_trans[1](Em_0_e[0]) -~ Em_0_e[1]),
<a name="line_330"></a>   factor(Em_0_trans[1](Em_0_e[1]) -~ Em_0_e[0]),
<a name="line_331"></a>   factor(Em_0_trans[1](Em_0_e[2]) -~ Em_0_e[3]),
<a name="line_332"></a>   factor(Em_0_trans[1](Em_0_e[3]) -~ Em_0_e[2]),
<a name="line_333"></a>   factor(Em_0_trans[2](Em_0_e[0]) -~ Em_0_e[2]),
<a name="line_334"></a>   factor(Em_0_trans[2](Em_0_e[1]) -~ Em_0_e[3]),
<a name="line_335"></a>   factor(Em_0_trans[2](Em_0_e[2]) -~ Em_0_e[0]),
<a name="line_336"></a>   factor(Em_0_trans[2](Em_0_e[3]) -~ Em_0_e[1]),
<a name="line_337"></a>   factor(Em_0_trans[3](Em_0_e[0]) -~ Em_0_e[3]),
<a name="line_338"></a>   factor(Em_0_trans[3](Em_0_e[1]) -~ Em_0_e[2]),
<a name="line_339"></a>   factor(Em_0_trans[3](Em_0_e[2]) -~ Em_0_e[1]),
<a name="line_340"></a>   factor(Em_0_trans[3](Em_0_e[3]) -~ Em_0_e[0])
<a name="line_341"></a>  } = {[0,0]},
<a name="line_342"></a>  "Action of Ex_0_trans[i] on Ex_0_e[j]"
<a name="line_343"></a> );
<a name="line_344"></a>
<a name="line_345"></a> _ASSERT(
<a name="line_346"></a>  {seq(factor(diff(Ep_0_trans[i]([y,x])[2],x)/Ep_0_trans[i]([y,x])[1] - 1/y),i=0..3)} = {0},
<a name="line_347"></a>  "Ep translations preserve the invariant differential"
<a name="line_348"></a> );
<a name="line_349"></a> 
<a name="line_350"></a> _ASSERT(
<a name="line_351"></a>  {seq(factor(diff(Em_0_trans[i]([y,x])[2],x)/Em_0_trans[i]([y,x])[1] - 1/y),i=0..3)} = {0},
<a name="line_352"></a>  "Em translations preserve the invariant differential"
<a name="line_353"></a> );
<a name="line_354"></a>  
<a name="line_355"></a> _ASSERT(`and`(
<a name="line_356"></a>  is_equal_Ep(Ep_trans[1](v_Ep[0]),v_Ep[ 3]),
<a name="line_357"></a>  is_equal_Ep(Ep_trans[2](v_Ep[0]),v_Ep[11]),
<a name="line_358"></a>  is_equal_Ep(Ep_trans[3](v_Ep[0]),v_Ep[10]),
<a name="line_359"></a>  is_equal_Em(Em_trans[1](v_Em[0]),v_Em[ 7]),
<a name="line_360"></a>  is_equal_Em(Em_trans[2](v_Em[0]),v_Em[11]),
<a name="line_361"></a>  is_equal_Em(Em_trans[3](v_Em[0]),v_Em[10])),
<a name="line_362"></a>  "action of translations on some vertices"
<a name="line_363"></a> );
<a name="line_364"></a>
<a name="line_365"></a>end:
<a name="line_366"></a>
<a name="line_367"></a>add_check(check_translations):
<a name="line_368"></a>
<a name="line_369"></a>######################################################################
<a name="line_370"></a>
<a name="line_371"></a>check_isogenies := proc()
<a name="line_372"></a> local i,x,y,ww;
<a name="line_373"></a> 
<a name="line_374"></a> printf("%a()\n",procname);
<a name="line_375"></a>
<a name="line_376"></a> _ASSERT(
<a name="line_377"></a>  [seq(solve([z[i]-1,op(Ep_to_Em(z)),op(Ep_rels)],
<a name="line_378"></a>             [z[1],z[2],z[3],z[4]]),i=1..4)] = [[]$4],
<a name="line_379"></a>  "Ep_to_Em is globally defined"
<a name="line_380"></a> );
<a name="line_381"></a>
<a name="line_382"></a> _ASSERT(
<a name="line_383"></a>  [seq(solve([z[i]-1,op(Em_to_Em(z)),op(Em_rels)],
<a name="line_384"></a>             [z[1],z[2],z[3],z[4]]),i=1..4)] = [[]$4],
<a name="line_385"></a>  "Em_to_Ep is globally defined"
<a name="line_386"></a> );
<a name="line_387"></a>
<a name="line_388"></a> _ASSERT([seq(NF_Ep(expand(Em_rel[i](Ep_to_Em(z)))),i=0..1)] = [0,0],
<a name="line_389"></a>  "Ep_to_Em lands in Em"
<a name="line_390"></a> );
<a name="line_391"></a>
<a name="line_392"></a> _ASSERT([seq(NF_Em(expand(Ep_rel[i](Em_to_Ep(z)))),i=0..1)] = [0,0],
<a name="line_393"></a>  "Em_to_Ep lands in Ep"
<a name="line_394"></a> );
<a name="line_395"></a>
<a name="line_396"></a> _ASSERT(
<a name="line_397"></a>  is_equal_Em(Ep_to_Em(z),Ep_to_Em(Ep_trans[1](z))),
<a name="line_398"></a>  "Ep_to_Em is invariant under translation by Ep_e[1]"
<a name="line_399"></a> );
<a name="line_400"></a>
<a name="line_401"></a> _ASSERT(
<a name="line_402"></a>  is_equal_Ep(Em_to_Ep(z),Em_to_Ep(Em_trans[1](z))),
<a name="line_403"></a>  "Em_to_Ep is invariant under translation by Em_e[1]"
<a name="line_404"></a> );
<a name="line_405"></a>
<a name="line_406"></a> _ASSERT(
<a name="line_407"></a>  is_equal_Em(Ep_to_Em(j_Ep([y,x])),j_Em(Ep_0_to_Em_0([y,x]))),
<a name="line_408"></a>  "Ep_to_Em is compatible with Ep_0_to_Em_0"
<a name="line_409"></a> );
<a name="line_410"></a>
<a name="line_411"></a> _ASSERT(
<a name="line_412"></a>  is_equal_Ep(Em_to_Ep(j_Em([y,x])),j_Ep(Em_0_to_Ep_0([y,x]))),
<a name="line_413"></a>  "Em_to_Ep is compatible with Em_0_to_Ep_0"
<a name="line_414"></a> );
<a name="line_415"></a>
<a name="line_416"></a> _ASSERT(
<a name="line_417"></a>  rem(numer(factor(is_member_Em_0(Ep_0_to_Em_0([y,x])))),is_member_Ep_0([y,x]),y) = 0,
<a name="line_418"></a>  "Ep_0_to_Em_0 lands in Em_0"
<a name="line_419"></a> );
<a name="line_420"></a>
<a name="line_421"></a> _ASSERT(
<a name="line_422"></a>  rem(numer(factor(is_member_Ep_0(Em_0_to_Ep_0([y,x])))),is_member_Em_0([y,x]),y) = 0,
<a name="line_423"></a>  "Em_0_to_Ep_0 lands in Ep_0"
<a name="line_424"></a> );
<a name="line_425"></a>
<a name="line_426"></a> _ASSERT(
<a name="line_427"></a>  factor(Ep_0_to_Em_0([y,x]) -~ Ep_0_to_Em_0(Ep_0_trans[1]([y,x]))) = [0,0],
<a name="line_428"></a>  "Ep_0_to_Em_0 is invariant under translation by Ep_0_e[1]"
<a name="line_429"></a> );
<a name="line_430"></a>
<a name="line_431"></a> _ASSERT(
<a name="line_432"></a>  factor(Em_0_to_Ep_0([y,x]) -~ Em_0_to_Ep_0(Em_0_trans[1]([y,x]))) = [0,0],
<a name="line_433"></a>  "Em_0_to_Ep_0 is invariant under translation by Em_0_e[1]"
<a name="line_434"></a> );
<a name="line_435"></a>  
<a name="line_436"></a> _ASSERT(
<a name="line_437"></a>  simplify(PJ_to_Epm_0(PJ_trans(u)) -~ PJ_to_Epm_0(u)) = [0$4],
<a name="line_438"></a>  "PJ -> Ep x Em is invariant under PJ_trans"
<a name="line_439"></a> );
<a name="line_440"></a>
<a name="line_441"></a> _ASSERT(
<a name="line_442"></a>  map(rem,map(numer,simplify(PJ_to_Epm_0(P_to_PJ([w,z])) -~ P_to_Epm_0([w,z]))),w^2-r_P(z),w)
<a name="line_443"></a>   = [0$4],
<a name="line_444"></a>  "(P -> PJ -> Ep x Em) = (P -> Ep x Em)"
<a name="line_445"></a> );
<a name="line_446"></a>
<a name="line_447"></a> _ASSERT(
<a name="line_448"></a>  map(rem,map(numer,simplify(P_to_PJ([w,z],-sqrt(z)) -~
<a name="line_449"></a>                             PJ_trans(P_to_PJ([w,z],sqrt(z))))),w^2-r_P(z),w) = [0$4],
<a name="line_450"></a>  "P -> PJ is well-defined"
<a name="line_451"></a> );
<a name="line_452"></a>
<a name="line_453"></a> _ASSERT(
<a name="line_454"></a>  {seq(factor(is_member_Ep_0(Epm_w[i])),i=1..4)} = {0} and
<a name="line_455"></a>  is_equal_Em(v_Em[2],factor(Ep_to_Em(j_Ep(Epm_w[1])))) and
<a name="line_456"></a>  is_equal_Em(v_Em[2],factor(Ep_to_Em(j_Ep(Epm_w[2])))) and
<a name="line_457"></a>  is_equal_Em(v_Em[4],factor(Ep_to_Em(j_Ep(Epm_w[3])))) and
<a name="line_458"></a>  is_equal_Em(v_Em[4],factor(Ep_to_Em(j_Ep(Epm_w[4])))),
<a name="line_459"></a>  "singular points for Ep_to_Em"
<a name="line_460"></a> );
<a name="line_461"></a>
<a name="line_462"></a>end:
<a name="line_463"></a>
<a name="line_464"></a>add_check(check_isogenies):
<a name="line_465"></a>
<a name="line_466"></a>######################################################################
<a name="line_467"></a>
<a name="line_468"></a>check_weierstrass := proc()
<a name="line_469"></a> local samples,chk,f0,f,g,err_a,err_b,err_c,err_d,err_e,err_f,k,m;
<a name="line_470"></a>
<a name="line_471"></a> printf("%a()\n",procname);
<a name="line_472"></a>
<a name="line_473"></a> _ASSERT(simplify(is_member_Ep_0(C_to_Ep_0(z))) = 0,
<a name="line_474"></a>         "Weierstrass parametrisation C_to_Ep_0 lands in Ep_0");
<a name="line_475"></a>
<a name="line_476"></a> _ASSERT(simplify(is_member_Em_0(C_to_Em_0(z))) = 0,
<a name="line_477"></a>         "Weierstrass parametrisation C_to_Em_0 lands in Em_0");
<a name="line_478"></a>
<a name="line_479"></a> _ASSERT(convert(series(C_to_Ep_0(z)[1],z=0,6),polynom,z) = z,
<a name="line_480"></a>         "C_to_Ep_0(z) to first order");
<a name="line_481"></a>
<a name="line_482"></a> _ASSERT(convert(series(C_to_Em_0(z)[1],z=0,6),polynom,z) = z,
<a name="line_483"></a>         "C_to_Em_0(z) to first order");
<a name="line_484"></a>
<a name="line_485"></a> _ASSERT(simplify(C_to_Ep_0(-z) -~ act_Ep_0[LL](C_to_Ep_0(z))) = [0,0],
<a name="line_486"></a>         "C_to_Ep_0 is equivariant for LL");
<a name="line_487"></a>
<a name="line_488"></a> _ASSERT(simplify(C_to_Em_0(-z) -~ act_Em_0[LL](C_to_Em_0(z))) = [0,0],
<a name="line_489"></a>         "C_to_Em_0 is equivariant for LL");
<a name="line_490"></a>
<a name="line_491"></a> samples := [seq(seq((-2)^i+(-3)^j*I,i=-3..3),j=-3..3)];
<a name="line_492"></a> chk := proc(f)
<a name="line_493"></a>  local g;
<a name="line_494"></a>  g := unapply(evalf(subs(a_P=0.1,max(map(abs,f)))),z);
<a name="line_495"></a>  return evalb(max(map(g,samples)) < 10.^(-90));
<a name="line_496"></a> end;
<a name="line_497"></a>
<a name="line_498"></a> f := C_to_Ep_0(conjugate(z)) -~ act_Ep_0[N](C_to_Ep_0(z)):
<a name="line_499"></a> _ASSERT(chk(f),"C_to_Ep_0 is equivariant for N");
<a name="line_500"></a>
<a name="line_501"></a> f := C_to_Em_0(conjugate(z)) -~ act_Em_0[LLLN](C_to_Em_0(z)):
<a name="line_502"></a> _ASSERT(chk(f),"C_to_Em_0 is equivariant for LLLN");
<a name="line_503"></a>
<a name="line_504"></a> err_a := C_to_Ep_0(z + latt_a) -~ C_to_Ep_0(z):
<a name="line_505"></a> _ASSERT(chk(err_a),"C_to_Ep_0 is latt_a - periodic");
<a name="line_506"></a>
<a name="line_507"></a> err_b := C_to_Ep_0(z + latt_b) -~ C_to_Ep_0(z):
<a name="line_508"></a> _ASSERT(chk(err_b),"C_to_Ep_0 is latt_b - periodic");
<a name="line_509"></a>
<a name="line_510"></a> err_c := C_to_Em_0(z + latt_c) -~ C_to_Em_0(z):
<a name="line_511"></a> _ASSERT(chk(err_c),"C_to_Em_0 is latt_c - periodic");
<a name="line_512"></a>
<a name="line_513"></a> err_d := C_to_Em_0(z + latt_d) -~ C_to_Em_0(z):
<a name="line_514"></a> _ASSERT(chk(err_d),"C_to_Em_0 is latt_d - periodic");
<a name="line_515"></a>
<a name="line_516"></a> err_e := C_to_Em_0(z + latt_e) -~ C_to_Em_0(z):
<a name="line_517"></a> _ASSERT(chk(err_e),"C_to_Em_0 is latt_e - periodic");
<a name="line_518"></a>
<a name="line_519"></a> err_f := C_to_Em_0(z + latt_f) -~ C_to_Em_0(z):
<a name="line_520"></a> _ASSERT(chk(err_f),"C_to_Em_0 is latt_f - periodic");
<a name="line_521"></a>
<a name="line_522"></a> _ASSERT(max(
<a name="line_523"></a>  d2f(div_Ep(latt_a0)    , [ 1, 0]),
<a name="line_524"></a>  d2f(div_Ep(latt_b0)    , [ 0, 1]),
<a name="line_525"></a>  d2f(div_Em(latt_f0)    , [ 1, 0]),
<a name="line_526"></a>  d2f(div_Em(latt_e0)    , [ 0, 1]),
<a name="line_527"></a>  d2f(div_Emq(latt_c0/2) , [ 0, 1]),
<a name="line_528"></a>  d2f(div_Emq(latt_d0/2) , [-1, 0])) < 10.^(-90),
<a name="line_529"></a>  "lattice division maps"
<a name="line_530"></a> );
<a name="line_531"></a>
<a name="line_532"></a> _ASSERT(
<a name="line_533"></a>  factor(simplify(convert(series(Ep_0_to_C(C_to_Ep_0(z)),z=0,20),polynom,z))) = z,
<a name="line_534"></a>  "Ep_0_to_C is inverse to C_to_Ep_0");
<a name="line_535"></a>
<a name="line_536"></a> _ASSERT(
<a name="line_537"></a>  factor(simplify(convert(series(Em_0_to_C(C_to_Em_0(z)),z=0,20),polynom,z))) = z,
<a name="line_538"></a>  "Em_0_to_C is inverse to C_to_Em_0");
<a name="line_539"></a>
<a name="line_540"></a> _ASSERT(
<a name="line_541"></a>  max(
<a name="line_542"></a>   op(map(abs,evalf(C_to_Ep0_0((latt_a0+latt_b0)/2) -~ Ep0_0_e[1]))),
<a name="line_543"></a>   op(map(abs,evalf(C_to_Ep0_0(latt_a0/2)           -~ Ep0_0_e[2]))),
<a name="line_544"></a>   op(map(abs,evalf(C_to_Ep0_0(latt_b0/2)           -~ Ep0_0_e[3])))
<a name="line_545"></a>  ) < 10.^(-90),
<a name="line_546"></a>  "points of order 2 on Ep, via Weierstrass"
<a name="line_547"></a> );
<a name="line_548"></a>
<a name="line_549"></a> _ASSERT(
<a name="line_550"></a>  max(
<a name="line_551"></a>   op(map(abs,evalf(C_to_Em0_0(latt_c0/2)) -~ Em0_0_e[1])),
<a name="line_552"></a>   op(map(abs,evalf(C_to_Em0_0(latt_d0/2)) -~ Em0_0_e[1])),
<a name="line_553"></a>   op(map(abs,evalf(C_to_Em0_0(latt_e0/2)) -~ Em0_0_e[3])),
<a name="line_554"></a>   op(map(abs,evalf(C_to_Em0_0(latt_f0/2)) -~ Em0_0_e[2]))
<a name="line_555"></a>  ) < 10.^(-49),
<a name="line_556"></a>  "points of order 2 on Em, via Weierstrass"
<a name="line_557"></a> );
<a name="line_558"></a>
<a name="line_559"></a> _ASSERT(
<a name="line_560"></a>  max(seq(evalf(d_CP2_0(v_Ep0_0[i],C_to_Ep0_0(v_TEp[i]))), i in [2,3,4,5,10,12])) < 10.^(-90),
<a name="line_561"></a>  "v_TEp[i] maps to v_Ep_0[i]"
<a name="line_562"></a> );
<a name="line_563"></a>
<a name="line_564"></a> unassign('k','m');
<a name="line_565"></a>
<a name="line_566"></a> f := (m,t) -> abs(evalf(d_CP2_0(c_Ep0_0[m](t),c_Ep_0_approx[m](t))));
<a name="line_567"></a> for k from 0 to 8 do 
<a name="line_568"></a>  _ASSERT(max(seq(f(k,i),i=1..20)) < 0.05,
<a name="line_569"></a>          sprintf("c_TEp_approx[%d] is a good approximation",k));
<a name="line_570"></a> od;
<a name="line_571"></a>
<a name="line_572"></a> unassign('k','m');
<a name="line_573"></a>
<a name="line_574"></a> g := (m,t) -> abs(evalf(d_CP2_0(c_Em0_0[m](t),c_Em_0_approx[m](t))));
<a name="line_575"></a> for k from 0 to 8 do 
<a name="line_576"></a>  _ASSERT(max(seq(g(k,i),i=1..20)) < 0.05,
<a name="line_577"></a>          sprintf("c_TEm_approx[%d] is a good approximation",k));
<a name="line_578"></a> od;
<a name="line_579"></a>end:
<a name="line_580"></a>
<a name="line_581"></a>add_check(check_weierstrass):
<a name="line_582"></a>
<a name="line_583"></a>######################################################################
<a name="line_584"></a>
<a name="line_585"></a>check_Ep0_circle := proc()
<a name="line_586"></a> local NN,i,aa,pp_c,pp_d,pp_e,m;
<a name="line_587"></a>
<a name="line_588"></a> printf("%a()\n",procname);
<a name="line_589"></a>
<a name="line_590"></a> # This is a homeomorphism from R to R, with m(x+2*Pi) = m(x) + 2*Pi,
<a name="line_591"></a> # such that m'(0) = 0.
<a name="line_592"></a> m := (x) -> x - sin(x);
<a name="line_593"></a> 
<a name="line_594"></a> NN := 128:
<a name="line_595"></a> for i from 0 to NN do 
<a name="line_596"></a>  aa[i] := m((i+0.5)/NN * 2*Pi);
<a name="line_597"></a>  pp_c[i] := evalf(Ep0_circle_lift_c(aa[i]));
<a name="line_598"></a>  pp_d[i] := evalf(Ep0_circle_lift_d(aa[i]));
<a name="line_599"></a>  pp_e[i] := evalf(Ep0_circle_lift_e(aa[i]));
<a name="line_600"></a> od:
<a name="line_601"></a>
<a name="line_602"></a> _ASSERT(max([seq(abs(pp_c[i]-pp_c[i-1]),i=1..NN)]) < 0.01,
<a name="line_603"></a>         "Ep0_circle_lift_c is continuous");
<a name="line_604"></a>	 
<a name="line_605"></a> _ASSERT(max([seq(abs(pp_d[i]-pp_d[i-1]),i=1..NN)]) < 0.01,
<a name="line_606"></a>         "Ep0_circle_lift_d is continuous");
<a name="line_607"></a>	 
<a name="line_608"></a> _ASSERT(max([seq(abs(pp_c[i]-pp_e[i  ]),i=1..NN)]) < 0.002,
<a name="line_609"></a>         "Ep0_circle_lift_e approximates Ep0_circle_lift_c");
<a name="line_610"></a>	 
<a name="line_611"></a> _ASSERT(
<a name="line_612"></a>   max([seq(abs(evalf(C_to_Ep0_0(pp_c[i])[2] - exp(I*aa[i]))),i=1..NN)]) < 10.^(-90),
<a name="line_613"></a>   "Ep0_circle_lift_c is a lift");
<a name="line_614"></a>   
<a name="line_615"></a> _ASSERT(
<a name="line_616"></a>   max([seq(abs(evalf(C_to_Ep0_0(pp_d[i])[2] - exp(I*aa[i]))),i=1..NN)]) < 10.^(-90),
<a name="line_617"></a>   "Ep0_circle_lift_d is a lift");
<a name="line_618"></a>   
<a name="line_619"></a> _ASSERT(
<a name="line_620"></a>  max([seq(abs(evalf(Ep0_circle_equation(pp_c[i]))),i=0..127)]) < 10.^(-5),
<a name="line_621"></a>  "Ep0_circle_lift_c satisfies the approximate lifted circle equation");
<a name="line_622"></a>
<a name="line_623"></a> _ASSERT(
<a name="line_624"></a>  max([seq(abs(evalf(Ep0_circle_equation(pp_d[i]))),i=0..127)]) < 10.^(-5),
<a name="line_625"></a>  "Ep0_circle_lift_d satisfies the approximate lifted circle equation");
<a name="line_626"></a>
<a name="line_627"></a> _ASSERT(
<a name="line_628"></a>  max([seq(abs(evalf(Ep0_circle_equation(pp_e[i]))),i=0..127)]) < 0.0011,
<a name="line_629"></a>  "Ep0_circle_lift_e satisfies the approximate lifted circle equation");
<a name="line_630"></a>end:
<a name="line_631"></a>
<a name="line_632"></a>add_check(check_Ep0_circle):
<a name="line_633"></a>
  </pre>
 </body>
</html>
    