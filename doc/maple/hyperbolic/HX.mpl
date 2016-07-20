<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>######################################################################
<a name="line_2"></a>######################################################################
<a name="line_3"></a>
<a name="line_4"></a># The covering by the Poincare disk
<a name="line_5"></a>
<a name="line_6"></a>assume (0 < a_H and a_H < 1);
<a name="line_7"></a>
<a name="line_8"></a>ap_H := sqrt(1+a_H^2); <span style="color:red">#@ ap_H
</span><a name="line_9"></a>am_H := sqrt(1-a_H^2); <span style="color:red">#@ am_H
</span><a name="line_10"></a>
<a name="line_11"></a><span style="color:red">#@ simplify_H 
</span><a name="line_12"></a>simplify_H := proc(u)
<a name="line_13"></a> local v;
<a name="line_14"></a> v := simplify(u);
<a name="line_15"></a> v := simplify(subs(sqrt(1-a_H^4)=ap_H * am_H,u));
<a name="line_16"></a> v := simplify(subs(sqrt(5+2*a_H^2+a_H^4)=
<a name="line_17"></a>                    sqrt(3+a_H^2+2*sqrt(1+a_H^2)) *
<a name="line_18"></a>                    sqrt(3+a_H^2-2*sqrt(1+a_H^2)),v));
<a name="line_19"></a> v := simplify(subs(sqrt(1-a_H^4)=ap_H * am_H,v));
<a name="line_20"></a> return(v);
<a name="line_21"></a>end:
<a name="line_22"></a>
<a name="line_23"></a>######################################################################
<a name="line_24"></a>
<a name="line_25"></a><span style="color:red">#@ is_member_H 
</span><a name="line_26"></a>is_member_H := (z) -> is(simplify_H(1 - z*conjugate(z)) > 0);
<a name="line_27"></a>
<a name="line_28"></a><span style="color:red">#@ is_equal_H 
</span><a name="line_29"></a>is_equal_H := proc(z,w,witness_)
<a name="line_30"></a> local z0;
<a name="line_31"></a> z0 := z;
<a name="line_32"></a> if nargs > 2 then z0 := act_Pi(witness_,z0); fi;
<a name="line_33"></a> 
<a name="line_34"></a> evalb(factor(expand(simplify_H(z0-w))) = 0);
<a name="line_35"></a>end;
<a name="line_36"></a>
<a name="line_37"></a>######################################################################
<a name="line_38"></a># Hyperbolic metric on the unit disc, scaled to have curvature -1.
<a name="line_39"></a>
<a name="line_40"></a># m_hyp, d_hyp and E_hyp take arguments in R^2
<a name="line_41"></a># m_hyp_c, d_hyp_c and E_hyp_c take arguments in C
<a name="line_42"></a>
<a name="line_43"></a># d_hyp and d_hyp_c give the actual metric, ie the distance between 
<a name="line_44"></a># a pair of points.  E_hyp and E_hyp_c give the multiplier for the
<a name="line_45"></a># metric tensor relative to the standard euclidean metric.
<a name="line_46"></a>
<a name="line_47"></a><span style="color:red">#@ m_hyp_c 
</span><a name="line_48"></a>m_hyp_c := (z,w) -> abs((z-w)/(1-z*conjugate(w)));
<a name="line_49"></a>
<a name="line_50"></a><span style="color:red">#@ m_hyp   
</span><a name="line_51"></a>m_hyp   := (x,y) -> sqrt(((x[1]-y[1])^2+(x[2]-y[2])^2)/((x[1]*y[2]-x[2]*y[1])^2+(x[1]*y[1]+x[2]*y[2]-1)^2)):
<a name="line_52"></a>
<a name="line_53"></a><span style="color:red">#@ d_hyp_c 
</span><a name="line_54"></a>d_hyp_c := (z,w) -> 2*arctanh(m_hyp_c(z,w));
<a name="line_55"></a>
<a name="line_56"></a><span style="color:red">#@ d_hyp   
</span><a name="line_57"></a>d_hyp   := (x,y) -> 2*arctanh(m_hyp(x,y));
<a name="line_58"></a>
<a name="line_59"></a><span style="color:red">#@ E_hyp_c 
</span><a name="line_60"></a>E_hyp_c := (z) -> 4/(1 - abs(z)^2)^2;
<a name="line_61"></a>
<a name="line_62"></a><span style="color:red">#@ E_hyp   
</span><a name="line_63"></a>E_hyp   := (x) -> 4/(1 - x[1]^2 - x[2]^2)^2;
<a name="line_64"></a>
<a name="line_65"></a># This returns a parametrisation of the locus of points at 
<a name="line_66"></a># hyperbolic distance r from a.
<a name="line_67"></a>
<a name="line_68"></a><span style="color:red">#@ hyp_sphere 
</span><a name="line_69"></a>hyp_sphere := proc(a,r)
<a name="line_70"></a> local t,u,v,m;
<a name="line_71"></a> t := tanh(r/2);
<a name="line_72"></a> u := abs(a);
<a name="line_73"></a> v := (1-t^2)/(1-t^2*u^2)*a;
<a name="line_74"></a> m := t*(1-u^2)/(1-t^2*u^2);
<a name="line_75"></a> unapply(v + m * exp(I*s),s);
<a name="line_76"></a>end:
<a name="line_77"></a>
<a name="line_78"></a><span style="color:red">#@ hyp_midpoint 
</span><a name="line_79"></a>hyp_midpoint := proc(a,b)
<a name="line_80"></a> (1 - abs(a*b)^2 - sqrt((1-abs(a)^2)*(1-abs(b)^2)) * abs(1 - conjugate(a)*b))/
<a name="line_81"></a>  ((1-abs(a)^2)*conjugate(b) + (1-abs(b)^2)*conjugate(a));
<a name="line_82"></a>end:
<a name="line_83"></a>
<a name="line_84"></a>######################################################################
<a name="line_85"></a>
<a name="line_86"></a>lambda_H := (z) -> I*z;                                <span style="color:red">#@ lambda_H
</span><a name="line_87"></a>lambda_inv_H := (z) -> -I*z;                           <span style="color:red">#@ lambda_inv_H
</span><a name="line_88"></a>lambda_sq_H := (z) -> -z;                              <span style="color:red">#@ lambda_sq_H
</span><a name="line_89"></a>mu_H := (z) -> ((ap_H*z-a_H^2-I)/((a_H^2-I)*z-ap_H));  <span style="color:red">#@ mu_H
</span><a name="line_90"></a>nu_H := (z) -> conjugate(z);                           <span style="color:red">#@ nu_H
</span><a name="line_91"></a>
<a name="line_92"></a><span style="color:red">#@ act_H
</span><a name="line_93"></a>act_H[1]    := (z) ->  z;
<a name="line_94"></a>act_H[L]    := (z) -> I*z;
<a name="line_95"></a>act_H[LL]   := (z) -> -z;
<a name="line_96"></a>act_H[LLL]  := (z) -> -I*z;
<a name="line_97"></a>act_H[M]    := unapply(simplify(   mu_H(z)),z);
<a name="line_98"></a>act_H[LM]   := unapply(simplify( I*mu_H(z)),z);
<a name="line_99"></a>act_H[LLM]  := unapply(simplify(  -mu_H(z)),z);
<a name="line_100"></a>act_H[LLLM] := unapply(simplify(-I*mu_H(z)),z);
<a name="line_101"></a>act_H[N]    := unapply(simplify(   nu_H(z)),z);
<a name="line_102"></a>act_H[LN]   := unapply(simplify( I*nu_H(z)),z);
<a name="line_103"></a>act_H[LLN]  := unapply(simplify(  -nu_H(z)),z);
<a name="line_104"></a>act_H[LLLN] := unapply(simplify(-I*nu_H(z)),z);
<a name="line_105"></a>act_H[MN]   := unapply(simplify(   mu_H(nu_H(z))),z);
<a name="line_106"></a>act_H[LMN]  := unapply(simplify( I*mu_H(nu_H(z))),z);
<a name="line_107"></a>act_H[LLMN] := unapply(simplify(  -mu_H(nu_H(z))),z);
<a name="line_108"></a>act_H[LLLMN]:= unapply(simplify(-I*mu_H(nu_H(z))),z);
<a name="line_109"></a>
<a name="line_110"></a><span style="color:red">#@ beta
</span><a name="line_111"></a>beta[0] := unapply(simplify((ap_H*z+1)/(z+ap_H)),z):
<a name="line_112"></a>beta[1] := unapply(simplify((ap_H^3*z-(2+I)*a_H^2-I)/(((I-2)*a_H^2+I)*z + ap_H^3)),z);
<a name="line_113"></a>beta[2] := unapply(simplify(I*beta[0](-I*z)),z):
<a name="line_114"></a>beta[3] := unapply(simplify(I*beta[1](-I*z)),z):
<a name="line_115"></a>beta[4] := unapply(simplify(I*beta[2](-I*z)),z):
<a name="line_116"></a>beta[5] := unapply(simplify(I*beta[3](-I*z)),z):
<a name="line_117"></a>beta[6] := unapply(simplify(I*beta[4](-I*z)),z):
<a name="line_118"></a>beta[7] := unapply(simplify(I*beta[5](-I*z)),z):
<a name="line_119"></a>
<a name="line_120"></a># There is a standard isomorphism between the conformal automorphism
<a name="line_121"></a># group of the unit disc, and PSL_2(R).  The following matrices
<a name="line_122"></a># correspond to the maps beta[i].
<a name="line_123"></a>
<a name="line_124"></a><span style="color:red">#@ beta_SL2R
</span><a name="line_125"></a>beta_SL2R[0] := <<ap_H - 1|0>,<0|ap_H + 1>>/a_H:
<a name="line_126"></a>beta_SL2R[1] := <<ap_H^3+2*a_H^2|-1-a_H^2>,<-1-a_H^2|ap_H^3-2*a_H^2>>/(a_H-a_H^3):
<a name="line_127"></a>beta_SL2R[2] := <<ap_H|1>,<1|ap_H>>/a_H:
<a name="line_128"></a>beta_SL2R[3] := <<ap_H^3-ap_H^2|-2*a_H^2>,<-2*a_H^2|ap_H^3+ap_H^2>>/(a_H-a_H^3):
<a name="line_129"></a>beta_SL2R[4] := <<1 + ap_H|0>,<0|ap_H - 1>>/a_H:
<a name="line_130"></a>beta_SL2R[5] := <<ap_H^3-2*a_H^2|1+a_H^2>,<1+a_H^2|ap_H^3+2*a_H^2>>/(a_H-a_H^3):
<a name="line_131"></a>beta_SL2R[6] := <<ap_H|-1>,<-1|ap_H>>/a_H:
<a name="line_132"></a>beta_SL2R[7] := <<ap_H^3+ap_H^2|2*a_H^2>,<2*a_H^2|ap_H^3-ap_H^2>>/(a_H-a_H^3):
<a name="line_133"></a>
<a name="line_134"></a>######################################################################
<a name="line_135"></a>
<a name="line_136"></a># The point v_H[i] is a choice of lift of the point v[i] in HX(a).
<a name="line_137"></a>
<a name="line_138"></a><span style="color:red">#@ v_H
</span><a name="line_139"></a>v_H[ 0] := 0;
<a name="line_140"></a>v_H[ 1] := ap_H*(1+I)/2;
<a name="line_141"></a>v_H[ 2] := (a_H*am_H-ap_H)/(I-a_H^2);
<a name="line_142"></a>v_H[ 3] := (a_H*am_H-ap_H)/(I*a_H^2-1);
<a name="line_143"></a>v_H[ 4] :=  I*v_H[3];
<a name="line_144"></a>v_H[ 5] := -I*v_H[2];
<a name="line_145"></a>v_H[ 6] := (1+I)*(sqrt(2)-am_H)/sqrt(2)/ap_H;
<a name="line_146"></a>v_H[ 7] :=  I*v_H[6];
<a name="line_147"></a>v_H[ 8] :=   -v_H[6];
<a name="line_148"></a>v_H[ 9] := -I*v_H[6];
<a name="line_149"></a>v_H[10] := I*(ap_H-a_H);
<a name="line_150"></a>v_H[11] := ap_H-a_H;
<a name="line_151"></a>v_H[12] := (ap_H+a_H)*(I + (I+2)*a_H^2)/((ap_H+a_H)^2+a_H^2);
<a name="line_152"></a>v_H[13] := I*conjugate(v_H[12]);
<a name="line_153"></a>
<a name="line_154"></a>for i from 0 to 13 do v_H[i] := simplify(v_H[i]); od:
<a name="line_155"></a>
<a name="line_156"></a># The point v_H[2.1] is in the same Pi-orbit as v[2] and so represents
<a name="line_157"></a># the same point of the cromulent surface HX(a) = disc/Pi.  Similarly
<a name="line_158"></a># v_H[1.1], v_H[1.2] and v_H[1.3] are in the same Pi-orbit as v_H[1]
<a name="line_159"></a># and so on. Recall also that Maple does not treat 1.0 as being exactly
<a name="line_160"></a># equal to 1, so we find it convenient to define v_H[1.0] to be v_H[1].
<a name="line_161"></a>
<a name="line_162"></a>v_H[ 1.0] :=  v_H[1];
<a name="line_163"></a>v_H[ 1.1] :=  lambda_H(v_H[1.0]);
<a name="line_164"></a>v_H[ 1.2] :=  lambda_H(v_H[1.1]);
<a name="line_165"></a>v_H[ 1.3] :=  lambda_H(v_H[1.2]);
<a name="line_166"></a>v_H[ 2.0] :=  simplify(expand(v_H[2]));
<a name="line_167"></a>v_H[ 2.1] :=  conjugate(v_H[2.0]);
<a name="line_168"></a>v_H[ 3.0] :=  simplify(expand(v_H[3]));
<a name="line_169"></a>v_H[ 3.1] := -conjugate(v_H[3.0]);
<a name="line_170"></a>v_H[ 4.0] :=  simplify(expand(v_H[4]));
<a name="line_171"></a>v_H[ 4.1] :=  conjugate(v_H[4.0]);
<a name="line_172"></a>v_H[ 5.0] :=  simplify(expand(v_H[5]));
<a name="line_173"></a>v_H[ 5.1] := -conjugate(v_H[5.0]);
<a name="line_174"></a>v_H[10.0] :=  simplify(expand(v_H[10]));
<a name="line_175"></a>v_H[10.1] :=  conjugate(v_H[10.0]);
<a name="line_176"></a>v_H[11.0] :=  simplify(expand(v_H[11]));
<a name="line_177"></a>v_H[11.1] := -conjugate(v_H[11.0]);
<a name="line_178"></a>v_H[12.0] :=  simplify(expand(v_H[12]));
<a name="line_179"></a>v_H[12.1] := -v_H[12.0];
<a name="line_180"></a>v_H[12.2] := -conjugate(v_H[12.0]);
<a name="line_181"></a>v_H[12.3] :=  conjugate(v_H[12.0]);
<a name="line_182"></a>v_H[13.0] :=  simplify(expand(v_H[13]));
<a name="line_183"></a>v_H[13.1] := -v_H[13.0];
<a name="line_184"></a>v_H[13.2] :=  conjugate(v_H[13.0]);
<a name="line_185"></a>v_H[13.3] := -conjugate(v_H[13.0]);
<a name="line_186"></a>
<a name="line_187"></a># The following declarations specify elements of Pi that take 
<a name="line_188"></a># v_H[floor(i)] to v_H[i].  For example, the first declaration
<a name="line_189"></a># corresponds to the fact that v_H[1.1] = beta[2](beta[1](v_H[1])).
<a name="line_190"></a>
<a name="line_191"></a><span style="color:red">#@ v_H_fraction_offset
</span><a name="line_192"></a>v_H_fraction_offset[ 1.1] := [2,1]:
<a name="line_193"></a>v_H_fraction_offset[ 1.2] := [1,2,3,4]:
<a name="line_194"></a>v_H_fraction_offset[ 1.3] := [3,4]:
<a name="line_195"></a>v_H_fraction_offset[ 2.1] := [6]:
<a name="line_196"></a>v_H_fraction_offset[ 3.1] := [4]:
<a name="line_197"></a>v_H_fraction_offset[ 4.1] := [6]:
<a name="line_198"></a>v_H_fraction_offset[ 5.1] := [4]:
<a name="line_199"></a>v_H_fraction_offset[10.1] := [6]:
<a name="line_200"></a>v_H_fraction_offset[11.1] := [4]:
<a name="line_201"></a>v_H_fraction_offset[12.1] := [1]:
<a name="line_202"></a>v_H_fraction_offset[12.2] := [2,1]:
<a name="line_203"></a>v_H_fraction_offset[12.3] := [6]:
<a name="line_204"></a>v_H_fraction_offset[13.1] := [4,3,4]:
<a name="line_205"></a>v_H_fraction_offset[13.2] := [3,4]:
<a name="line_206"></a>v_H_fraction_offset[13.3] := [4]:
<a name="line_207"></a>
<a name="line_208"></a><span style="color:red">#@ v_H_fraction_transform
</span><a name="line_209"></a>v_H_fraction_transform[ 1.1] := L:
<a name="line_210"></a>v_H_fraction_transform[ 1.2] := LL:
<a name="line_211"></a>v_H_fraction_transform[ 1.3] := LLL:
<a name="line_212"></a>v_H_fraction_transform[ 2.1] := N:
<a name="line_213"></a>v_H_fraction_transform[ 3.1] := LLN:
<a name="line_214"></a>v_H_fraction_transform[ 4.1] := N:
<a name="line_215"></a>v_H_fraction_transform[ 5.1] := LLN:
<a name="line_216"></a>v_H_fraction_transform[10.1] := N:
<a name="line_217"></a>v_H_fraction_transform[11.1] := LLN:
<a name="line_218"></a>v_H_fraction_transform[12.1] := LL:
<a name="line_219"></a>v_H_fraction_transform[12.2] := LLN:
<a name="line_220"></a>v_H_fraction_transform[12.3] := N:
<a name="line_221"></a>v_H_fraction_transform[13.1] := LL:
<a name="line_222"></a>v_H_fraction_transform[13.2] := N:
<a name="line_223"></a>v_H_fraction_transform[13.3] := LLN:
<a name="line_224"></a>
<a name="line_225"></a># If T is in G16 and T(v[i]) = v[j] in X then in the hyperbolic disc 
<a name="line_226"></a># H we should have T(v[i]) = w(v[j]) for some word w in the generators
<a name="line_227"></a># beta[0], ... , beta[7].  The table v_action_witness_H records the required
<a name="line_228"></a># words.
<a name="line_229"></a> 
<a name="line_230"></a><span style="color:red">#@ v_action_witness_H
</span><a name="line_231"></a>for T in {L,M,N} do
<a name="line_232"></a> for i from 0 to 13 do
<a name="line_233"></a>  v_action_witness_H[T,i] := []:
<a name="line_234"></a> od:
<a name="line_235"></a>od:
<a name="line_236"></a>
<a name="line_237"></a>unassign('T','i'):
<a name="line_238"></a>
<a name="line_239"></a>v_action_witness_H[L, 1] := [2,1]:
<a name="line_240"></a>v_action_witness_H[L, 2] := [4]:
<a name="line_241"></a>v_action_witness_H[L, 4] := [4]:
<a name="line_242"></a>v_action_witness_H[L,10] := [4]:
<a name="line_243"></a>v_action_witness_H[L,11] := []:
<a name="line_244"></a>v_action_witness_H[L,12] := [4]:
<a name="line_245"></a>v_action_witness_H[L,13] := [2,1]:
<a name="line_246"></a>v_action_witness_H[M, 0] := [2,3,4]:
<a name="line_247"></a>v_action_witness_H[M, 1] := [2]:
<a name="line_248"></a>v_action_witness_H[M, 3] := [2]:
<a name="line_249"></a>v_action_witness_H[M, 4] := [5,6]:
<a name="line_250"></a>v_action_witness_H[M, 5] := [2,3,4]:
<a name="line_251"></a>v_action_witness_H[M, 6] := [2]:
<a name="line_252"></a>v_action_witness_H[M, 7] := [5]:
<a name="line_253"></a>v_action_witness_H[M, 8] := [5,4,3]:
<a name="line_254"></a>v_action_witness_H[M, 9] := [2,3,4]:
<a name="line_255"></a>v_action_witness_H[M,10] := []:
<a name="line_256"></a>v_action_witness_H[M,11] := [2,3,4]:
<a name="line_257"></a>v_action_witness_H[M,12] := []:
<a name="line_258"></a>v_action_witness_H[M,13] := [2]:
<a name="line_259"></a>v_action_witness_H[N, 1] := [3,4]:
<a name="line_260"></a>v_action_witness_H[N, 2] := [6]:
<a name="line_261"></a>v_action_witness_H[N, 3] := []:
<a name="line_262"></a>v_action_witness_H[N, 4] := [6]:
<a name="line_263"></a>v_action_witness_H[N, 5] := []:
<a name="line_264"></a>v_action_witness_H[N,10] := [6]:
<a name="line_265"></a>v_action_witness_H[N,12] := [6]:
<a name="line_266"></a>v_action_witness_H[N,13] := [3,4]:
<a name="line_267"></a>
<a name="line_268"></a>######################################################################
<a name="line_269"></a>
<a name="line_270"></a># For each complex number p with |p| > 1 we have an antiholomorphic
<a name="line_271"></a># involution xi(p,-) on the unit disk.  This gives all such 
<a name="line_272"></a># antiholomorphic involutions except those of the form 
<a name="line_273"></a># z |-> a * conjugate(z) with |a| = 1.  Every geodesic occurs
<a name="line_274"></a># as the fixed point set of a unique antiholomorphic involution.
<a name="line_275"></a>
<a name="line_276"></a><span style="color:red">#@ xi 
</span><a name="line_277"></a>xi := (p,z) -> (p*conjugate(z) - 1)/conjugate(z - p);
<a name="line_278"></a>
<a name="line_279"></a><span style="color:red">#@ xi_alt
</span><a name="line_280"></a>xi_alt := (theta,z) -> exp(I*theta) * conjugate(z);
<a name="line_281"></a>
<a name="line_282"></a># The function xi_curve(p,-) is a speed one parametrisation of the
<a name="line_283"></a># fixed point curve for xi(p,-).
<a name="line_284"></a>
<a name="line_285"></a><span style="color:red">#@ xi_curve 
</span><a name="line_286"></a>xi_curve := proc(p,s)
<a name="line_287"></a> local d;
<a name="line_288"></a> d := sqrt(abs(p)^2-1);
<a name="line_289"></a> (I*d - 1) * ((I*d + 1) - I * abs(p) * exp(s)) / 
<a name="line_290"></a>      (conjugate(p) * (I * abs(p) * exp(s) + (I * d - 1)));
<a name="line_291"></a>end:
<a name="line_292"></a>
<a name="line_293"></a>xi_curve_alt := (theta,s) -> exp(I*theta/2) * (exp(s) - 1)/(exp(s) + 1);
<a name="line_294"></a>
<a name="line_295"></a># xi_circle_orthogonal(p,q) returns an equation that will hold 
<a name="line_296"></a># iff the fixed point circles for xi(p,-) and xi(q,-) meet 
<a name="line_297"></a># orthogonally.
<a name="line_298"></a>
<a name="line_299"></a><span style="color:red">#@ xi_circle_orthogonal 
</span><a name="line_300"></a>xi_circle_orthogonal := proc(p,q)
<a name="line_301"></a> return(p*conjugate(q) + conjugate(p)*q - 2 = 0);
<a name="line_302"></a>end:
<a name="line_303"></a>
<a name="line_304"></a># If the circles as above do in fact meet orthogonally, then 
<a name="line_305"></a># the following function will return the point of intersection.
<a name="line_306"></a>
<a name="line_307"></a><span style="color:red">#@ xi_circle_intersect 
</span><a name="line_308"></a>xi_circle_intersect := proc(p,q)
<a name="line_309"></a> local r,s,a,b;
<a name="line_310"></a> r := simplify(sqrt(factor(p * conjugate(p) - 1)));
<a name="line_311"></a> s := simplify(sqrt(factor(q * conjugate(q) - 1)));
<a name="line_312"></a> a := factor((p * conjugate(q) - 1 + I * r * s)/conjugate(q-p));
<a name="line_313"></a> b := factor((p * conjugate(q) - 1 - I * r * s)/conjugate(q-p));
<a name="line_314"></a> return([a,b]);
<a name="line_315"></a>end:
<a name="line_316"></a>
<a name="line_317"></a># This function will return the two fixed points of xi(p,-) that
<a name="line_318"></a># lie on the unit circle.
<a name="line_319"></a>
<a name="line_320"></a><span style="color:red">#@ xi_circle_ends 
</span><a name="line_321"></a>xi_circle_ends := proc(p) 
<a name="line_322"></a> local r;
<a name="line_323"></a> r := factor(expand(1 - p * conjugate(p)));
<a name="line_324"></a> [(1 + sqrt(r))/conjugate(p),(1 - sqrt(r))/conjugate(p)];
<a name="line_325"></a>end:
<a name="line_326"></a>
<a name="line_327"></a># Given points a and b on the unit circle that are not opposite,
<a name="line_328"></a># this returns the centre of the circle that crosses S^1
<a name="line_329"></a># orthogonally at a and b.
<a name="line_330"></a>
<a name="line_331"></a><span style="color:red">#@ ends_to_centre 
</span><a name="line_332"></a>ends_to_centre := proc(a,b)
<a name="line_333"></a> (a + b)/(1 + Re(a/b));
<a name="line_334"></a>end:
<a name="line_335"></a>
<a name="line_336"></a><span style="color:red">#@ ends_to_radius 
</span><a name="line_337"></a>ends_to_radius := proc(a,b)
<a name="line_338"></a> local x;
<a name="line_339"></a> x := Re(a/b); 
<a name="line_340"></a> sqrt((1 - x)/(1 + x));
<a name="line_341"></a>end:
<a name="line_342"></a>
<a name="line_343"></a># We now define geodesics c_H[0] , ... , c_H[8] in the
<a name="line_344"></a># Poincare disk that will cover the curves c[0], ... , c[8] in X.  
<a name="line_345"></a>
<a name="line_346"></a><span style="color:red">#@ c_H_p
</span><a name="line_347"></a>c_H_p[0] := (1+I)/ap_H;
<a name="line_348"></a>c_H_p[3] := ap_H;
<a name="line_349"></a>c_H_p[4] := I*ap_H;
<a name="line_350"></a>c_H_p[7] := (I*ap_H/2+1/ap_H);
<a name="line_351"></a>c_H_p[8] := (ap_H/2+I/ap_H);
<a name="line_352"></a>
<a name="line_353"></a># These are the corresponding radii so c_H_r[k] = sqrt(abs(c_H_p[k])^2-1)
<a name="line_354"></a><span style="color:red">#@ c_H_r
</span><a name="line_355"></a>c_H_r[0] := am_H/ap_H;
<a name="line_356"></a>c_H_r[3] := a_H;
<a name="line_357"></a>c_H_r[4] := a_H;
<a name="line_358"></a>c_H_r[7] := am_H^2/(2*ap_H);
<a name="line_359"></a>c_H_r[8] := am_H^2/(2*ap_H);
<a name="line_360"></a>
<a name="line_361"></a><span style="color:red">#@ s_H
</span><a name="line_362"></a>s_H[0] := 2*log(sqrt(2)*a_H/(ap_H-am_H));
<a name="line_363"></a>s_H[1] := log((sqrt(2)+ap_H)/(sqrt(2)-ap_H))/2;
<a name="line_364"></a>s_H[2] := log((1+a_H)/am_H);
<a name="line_365"></a>s_H[3] := log((ap_H+a_H+1)/(ap_H+a_H-1))/2;
<a name="line_366"></a>s_H[4] := log((ap_H^2+2*ap_H+2)/(ap_H^2-2*ap_H+2))/4;
<a name="line_367"></a>
<a name="line_368"></a># Maple does a bad job of simplifying expressions involving 
<a name="line_369"></a># tanh(), so we write our own version that immediately 
<a name="line_370"></a># converts to exponentials.
<a name="line_371"></a>
<a name="line_372"></a><span style="color:red">#@ Tanh 
</span><a name="line_373"></a>Tanh := (s) -> (exp(s) - exp(-s))/(exp(s) + exp(-s));
<a name="line_374"></a>
<a name="line_375"></a><span style="color:red">#@ c_H
</span><a name="line_376"></a>c_H[0] := (t) -> xi_curve(c_H_p[0],2*s_H[0]*(t/Pi-1/4));
<a name="line_377"></a>c_H[1] := (t) -> ((I + 1)/sqrt(2)) * Tanh(t*s_H[1]/Pi);
<a name="line_378"></a>c_H[2] := (t) -> ((I - 1)/sqrt(2)) * Tanh(t*s_H[1]/Pi);
<a name="line_379"></a>c_H[3] := (t) -> xi_curve(c_H_p[3],-2*t*s_H[2]/Pi);
<a name="line_380"></a>c_H[4] := (t) -> xi_curve(c_H_p[4],-2*t*s_H[2]/Pi);
<a name="line_381"></a>c_H[5] := (t) -> Tanh(t*s_H[3]/Pi);
<a name="line_382"></a>c_H[6] := (t) -> I * Tanh(t*s_H[3]/Pi);
<a name="line_383"></a>c_H[7] := (t) -> xi_curve(c_H_p[7], 2*t*s_H[3]/Pi - 2*s_H[4]);
<a name="line_384"></a>c_H[8] := (t) -> xi_curve(c_H_p[8],-2*t*s_H[3]/Pi + 2*s_H[4]);
<a name="line_385"></a>
<a name="line_386"></a><span style="color:red">#@ v_H_theta 
</span><a name="line_387"></a>v_H_theta := 2*arctan(a_H*(sqrt(a_H^2+1)-a_H));
<a name="line_388"></a>
<a name="line_389"></a># The following functions parametrise the boundary of the
<a name="line_390"></a># region F8.  They move anticlockwise as the parameter
<a name="line_391"></a># s increases from 0 to 1.
<a name="line_392"></a>
<a name="line_393"></a><span style="color:red">#@ c_H_simple
</span><a name="line_394"></a>c_H_simple[5] := (s) -> s * v_H[11];
<a name="line_395"></a>c_H_simple[3] := (s) -> ap_H - a_H * exp(-I*s*v_H_theta);
<a name="line_396"></a>c_H_simple[7] := (s) -> 1/ap_H + I/2*ap_H + (1 - a_H^2)/ap_H/2 * exp(-I*((1+s)*Pi/2 + (1-s)*v_H_theta));
<a name="line_397"></a>c_H_simple[1] := (s) -> (1-s)*v_H[1];
<a name="line_398"></a>
<a name="line_399"></a># This records the endpoints on the unit circle of the geodesics
<a name="line_400"></a># that form the boundary of F16.
<a name="line_401"></a>
<a name="line_402"></a><span style="color:red">#@ c_H_ends
</span><a name="line_403"></a>c_H_ends[0] := [-(1-I)/2*(am_H - I*ap_H),(1-I)/2*(am_H+I*ap_H)];
<a name="line_404"></a>c_H_ends[1] := [-(1+I)/sqrt(2), (1+I)/sqrt(2)];
<a name="line_405"></a>c_H_ends[3] := [(1 - I*a_H)/ap_H,(1 + I*a_H)/ap_H];
<a name="line_406"></a>c_H_ends[5] := [-1,1];
<a name="line_407"></a>
<a name="line_408"></a># The curves c_H[k](t) have constant speed with respect to the
<a name="line_409"></a># hyperbolic metric.  The speeds are given below.
<a name="line_410"></a>
<a name="line_411"></a><span style="color:red">#@ c_H_speed
</span><a name="line_412"></a>c_H_speed[0] := 2*s_H[0]/Pi;
<a name="line_413"></a>c_H_speed[1] := 2*s_H[1]/Pi;
<a name="line_414"></a>c_H_speed[2] := 2*s_H[1]/Pi;
<a name="line_415"></a>c_H_speed[3] := 2*s_H[2]/Pi;
<a name="line_416"></a>c_H_speed[4] := 2*s_H[2]/Pi;
<a name="line_417"></a>c_H_speed[5] := 2*s_H[3]/Pi;
<a name="line_418"></a>c_H_speed[6] := 2*s_H[3]/Pi;
<a name="line_419"></a>c_H_speed[7] := 2*s_H[3]/Pi;
<a name="line_420"></a>c_H_speed[8] := 2*s_H[3]/Pi;
<a name="line_421"></a>
<a name="line_422"></a># c_H_cycle[k] gives the element g in the group Pi such that
<a name="line_423"></a># c_H[k](t + 2*Pi) = g . c_H[k](t).
<a name="line_424"></a>
<a name="line_425"></a><span style="color:red">#@ c_H_cycle
</span><a name="line_426"></a>c_H_cycle[0] := [0,2,4,6];
<a name="line_427"></a>c_H_cycle[1] := [0,7,6,5];
<a name="line_428"></a>c_H_cycle[2] := [2,1,0,7];
<a name="line_429"></a>c_H_cycle[3] := [0,7];
<a name="line_430"></a>c_H_cycle[4] := [2,1];
<a name="line_431"></a>c_H_cycle[5] := [0];
<a name="line_432"></a>c_H_cycle[6] := [2];
<a name="line_433"></a>c_H_cycle[7] := [0,2,1];
<a name="line_434"></a>c_H_cycle[8] := [2,3,4];
<a name="line_435"></a>
<a name="line_436"></a><span style="color:red">#@ c_action_witness_H
</span><a name="line_437"></a>for T in {L,M,N} do
<a name="line_438"></a> for i from 0 to 8 do
<a name="line_439"></a>  c_action_witness_H[T,i] := []:
<a name="line_440"></a> od:
<a name="line_441"></a>od:
<a name="line_442"></a>
<a name="line_443"></a>unassign('T','i'):
<a name="line_444"></a>
<a name="line_445"></a>c_action_witness_H[L,0] := [4];
<a name="line_446"></a>c_action_witness_H[L,4] := [4];
<a name="line_447"></a>c_action_witness_H[L,7] := [2,1];
<a name="line_448"></a>c_action_witness_H[L,8] := [2,1];
<a name="line_449"></a>c_action_witness_H[M,1] := [5,4,3];
<a name="line_450"></a>c_action_witness_H[M,2] := [2,3,4];
<a name="line_451"></a>c_action_witness_H[M,3] := [2,3,4];
<a name="line_452"></a>c_action_witness_H[M,5] := [2,3,4];
<a name="line_453"></a>c_action_witness_H[M,6] := [2,3,4];
<a name="line_454"></a>c_action_witness_H[M,7] := [2];
<a name="line_455"></a>c_action_witness_H[M,8] := [2];
<a name="line_456"></a>c_action_witness_H[N,0] := [6];
<a name="line_457"></a>c_action_witness_H[N,4] := [6];
<a name="line_458"></a>c_action_witness_H[N,7] := [3,4];
<a name="line_459"></a>c_action_witness_H[N,8] := [3,4];
<a name="line_460"></a>
<a name="line_461"></a><span style="color:red">#@ v_on_c_witness_H
</span><a name="line_462"></a>for i from 0 to 14 do
<a name="line_463"></a> for j from 0 to 8 do
<a name="line_464"></a>  v_on_c_witness_H[i,j] := [];
<a name="line_465"></a> od;
<a name="line_466"></a>od;
<a name="line_467"></a>
<a name="line_468"></a>v_on_c_witness_H[ 4,0] := [0];
<a name="line_469"></a>v_on_c_witness_H[ 5,0] := [2];
<a name="line_470"></a>v_on_c_witness_H[ 7,0] := [0];
<a name="line_471"></a>v_on_c_witness_H[ 8,0] := [2,0];
<a name="line_472"></a>v_on_c_witness_H[ 9,0] := [2];
<a name="line_473"></a>v_on_c_witness_H[ 1,2] := [2,1];
<a name="line_474"></a>v_on_c_witness_H[12,4] := [2,1];
<a name="line_475"></a>
<a name="line_476"></a>
<a name="line_477"></a>######################################################################
<a name="line_478"></a>
<a name="line_479"></a><span style="color:red">#@ c_check_H
</span><a name="line_480"></a>c_check_H[0] := (z) -> is_equal_H(z, xi(c_H_p[0],z),args[2..-1]);
<a name="line_481"></a>c_check_H[1] := (z) -> is_equal_H(z, I*conjugate(z),args[2..-1]);
<a name="line_482"></a>c_check_H[2] := (z) -> is_equal_H(z,-I*conjugate(z),args[2..-1]);
<a name="line_483"></a>c_check_H[3] := (z) -> is_equal_H(z, xi(c_H_p[3],z),args[2..-1]);
<a name="line_484"></a>c_check_H[4] := (z) -> is_equal_H(z, xi(c_H_p[4],z),args[2..-1]);
<a name="line_485"></a>c_check_H[5] := (z) -> is_equal_H(z,   conjugate(z),args[2..-1]);
<a name="line_486"></a>c_check_H[6] := (z) -> is_equal_H(z,  -conjugate(z),args[2..-1]);
<a name="line_487"></a>c_check_H[7] := (z) -> is_equal_H(z, xi(c_H_p[7],z),args[2..-1]);
<a name="line_488"></a>c_check_H[8] := (z) -> is_equal_H(z, xi(c_H_p[8],z),args[2..-1]);
<a name="line_489"></a>
<a name="line_490"></a>######################################################################
<a name="line_491"></a>
<a name="line_492"></a><span style="color:red">#@ F1_H_B 
</span><a name="line_493"></a>F1_H_B := [[0],[2],[4],[6],[0,7],[1,2],[2,1],[3,4],[4,3],[5,6],[6,5],[7,0]];
<a name="line_494"></a>
<a name="line_495"></a><span style="color:red">#@ is_in_F1_H 
</span><a name="line_496"></a>is_in_F1_H := proc(z)
<a name="line_497"></a> local T,w,r;
<a name="line_498"></a>
<a name="line_499"></a> for T in F1_H_B do
<a name="line_500"></a>  w := simplify_H(act_Pi(T,z));
<a name="line_501"></a>  r := simplify_H(z * conjugate(z) - w * conjugate(w));
<a name="line_502"></a>  if is(r > 0) then return false; fi;
<a name="line_503"></a> od;
<a name="line_504"></a>
<a name="line_505"></a> return true;
<a name="line_506"></a>end;
<a name="line_507"></a>
<a name="line_508"></a><span style="color:red">#@ is_in_F4_H 
</span><a name="line_509"></a>is_in_F4_H := proc(z)
<a name="line_510"></a> local z0;
<a name="line_511"></a> z0 := simplify_H(z);
<a name="line_512"></a> is_in_F1_H(z0) and is(simplify_H(Re(z)) >= 0) and is(simplify_H(Im(z)) >= 0);
<a name="line_513"></a>end;
<a name="line_514"></a>
<a name="line_515"></a><span style="color:red">#@ is_in_F16_H 
</span><a name="line_516"></a>is_in_F16_H := proc(z)
<a name="line_517"></a> local m,d;
<a name="line_518"></a>
<a name="line_519"></a> if not is_in_F4_H(z) then return false; fi;
<a name="line_520"></a> if is(simplify_H(Im(z) - Re(z)) > 0) then return false; fi;
<a name="line_521"></a> m := c_H_p[0];
<a name="line_522"></a> d := simplify_H((z-m)*conjugate(z-m) - m*conjugate(m) + 1);
<a name="line_523"></a> if is(d < 0) then return false; fi;
<a name="line_524"></a> return true;
<a name="line_525"></a>end;
<a name="line_526"></a>
<a name="line_527"></a><span style="color:red">#@ random_Delta_point 
</span><a name="line_528"></a>random_Delta_point := proc(min_abs_)
<a name="line_529"></a> local x,y,min_abs;
<a name="line_530"></a> x := 1;
<a name="line_531"></a> y := 1;
<a name="line_532"></a> min_abs := `if`(nargs > 0,min_abs_,0);
<a name="line_533"></a> while x^2 + y^2 >= 1 or x^2 + y^2 < min_abs^2 do
<a name="line_534"></a>  x := (rand(0..2000)()-1000)/1000.;
<a name="line_535"></a>  y := (rand(0..2000)()-1000)/1000.;
<a name="line_536"></a> od;
<a name="line_537"></a> return(x + I * y);
<a name="line_538"></a>end:
<a name="line_539"></a>
<a name="line_540"></a><span style="color:red">#@ F1_edge
</span><a name="line_541"></a>F1_edge[ 1, 1,'a'] := [v_H[ 1  ],v_H[12  ]]:
<a name="line_542"></a>F1_edge[ 1, 1,'b'] := [v_H[12  ],v_H[12.2]]:
<a name="line_543"></a>F1_edge[ 1, 1,'c'] := [v_H[ 1.1],v_H[13.3]]:
<a name="line_544"></a>F1_edge[ 1, 1,'d'] := [v_H[13.3],v_H[13.1]]:
<a name="line_545"></a>F1_edge[ 1, 1,'e'] := [v_H[ 1.2],v_H[12.1]]:
<a name="line_546"></a>F1_edge[ 1, 1,'f'] := [v_H[ 1.3],v_H[13.2]]:
<a name="line_547"></a>F1_edge[ 1,-1,'a'] := [v_H[ 1.1],v_H[12.2]]:
<a name="line_548"></a>F1_edge[ 1,-1,'b'] := [v_H[12.3],v_H[12.1]]:
<a name="line_549"></a>F1_edge[ 1,-1,'c'] := [v_H[ 1.2],v_H[13.1]]:
<a name="line_550"></a>F1_edge[ 1,-1,'d'] := [v_H[13  ],v_H[13.2]]:
<a name="line_551"></a>F1_edge[ 1,-1,'e'] := [v_H[ 1.3],v_H[12.3]]:
<a name="line_552"></a>F1_edge[ 1,-1,'f'] := [v_H[ 1  ],v_H[13  ]]:
<a name="line_553"></a>
<a name="line_554"></a><span style="color:red">#@ F1_pairing
</span><a name="line_555"></a>F1_pairing['a'] := [5,6];
<a name="line_556"></a>F1_pairing['b'] := [2];
<a name="line_557"></a>F1_pairing['c'] := [7,0];
<a name="line_558"></a>F1_pairing['d'] := [4];
<a name="line_559"></a>F1_pairing['e'] := [1,2];
<a name="line_560"></a>F1_pairing['f'] := [3,4];
<a name="line_561"></a>
<a name="line_562"></a><span style="color:red">#@ F1_arcs 
</span><a name="line_563"></a>F1_arcs := [
<a name="line_564"></a>  [    ap_H ,  0      , c_H_r[3] , Pi-v_H_theta     , Pi+v_H_theta     , magenta],
<a name="line_565"></a>  [  1/ap_H ,  ap_H/2 , c_H_r[7] , Pi               , 3*Pi/2-v_H_theta , blue   ],
<a name="line_566"></a>  [  ap_H/2 ,  1/ap_H , c_H_r[7] , Pi+v_H_theta     , 3*Pi/2           , blue   ],
<a name="line_567"></a>  [     0   ,  ap_H   , c_H_r[3] , 3*Pi/2-v_H_theta , 3*Pi/2+v_H_theta , magenta],
<a name="line_568"></a>  [ -ap_H/2 ,  1/ap_H , c_H_r[7] , 3*Pi/2           , 2*Pi-v_H_theta   , blue   ],
<a name="line_569"></a>  [ -1/ap_H ,  ap_H/2 , c_H_r[7] , 3*Pi/2+v_H_theta , 2*Pi             , blue   ],
<a name="line_570"></a>  [   -ap_H ,  0      , c_H_r[3] , -v_H_theta       , v_H_theta        , magenta],
<a name="line_571"></a>  [ -1/ap_H , -ap_H/2 , c_H_r[7] , 0                , Pi/2-v_H_theta   , blue   ],
<a name="line_572"></a>  [ -ap_H/2 , -1/ap_H , c_H_r[7] , v_H_theta        , Pi/2             , blue   ],
<a name="line_573"></a>  [     0   ,   -ap_H , c_H_r[3] , Pi/2-v_H_theta   , Pi/2+v_H_theta   , magenta],
<a name="line_574"></a>  [  ap_H/2 , -1/ap_H , c_H_r[7] , Pi/2             , Pi-v_H_theta     , blue   ],
<a name="line_575"></a>  [  1/ap_H , -ap_H/2 , c_H_r[7] , Pi/2+v_H_theta   , Pi               , blue   ]
<a name="line_576"></a> ];
<a name="line_577"></a>
<a name="line_578"></a><span style="color:red">#@ F1_area 
</span><a name="line_579"></a>F1_area :=
<a name="line_580"></a> (3+a_H^2)
<a name="line_581"></a> -2*a_H*am_H^2/ap_H
<a name="line_582"></a> -am_H^4/ap_H^2/2*Pi
<a name="line_583"></a> -v_H_theta*(3*a_H^4+6*a_H^2-1)/(1+a_H^2);
<a name="line_584"></a>
<a name="line_585"></a># This value of a_H makes F1 into a regular dodecagon, with
<a name="line_586"></a># vertices of absolute value 3^(-1/4).
<a name="line_587"></a>
<a name="line_588"></a><span style="color:red">#@ a_H_regular 
</span><a name="line_589"></a>a_H_regular := sqrt(2/sqrt(3)-1);
<a name="line_590"></a>
<a name="line_591"></a>side_length_H[ 0] := 2*arctanh((ap_H - sqrt(2)*a_H)/am_H);
<a name="line_592"></a>side_length_H[ 1] := 2*arctanh((sqrt(2) - am_H)/ap_H);
<a name="line_593"></a>side_length_H[ 3] := 2*arctanh((1 - am_H)/a_H);
<a name="line_594"></a>side_length_H[ 5] := 2*arctanh(ap_H - a_H);
<a name="line_595"></a>
<a name="line_596"></a>######################################################################
<a name="line_597"></a>
<a name="line_598"></a><span style="color:red">#@ recenter_H
</span><a name="line_599"></a>recenter_H[0] := (z) -> z:
<a name="line_600"></a>
<a name="line_601"></a>recenter_H[3] :=
<a name="line_602"></a> unapply(factor(-(-(I*sqrt(-(a_H-1)*(a_H+1))-a_H*sqrt(a_H^2+1))/(I-a_H^2)) * (z - v_H[3])/(1-conjugate(v_H[3])*z)),z):
<a name="line_603"></a>
<a name="line_604"></a>recenter_H[6] := 
<a name="line_605"></a> unapply(simplify((1-I)/sqrt(2)*(z - v_H[6])/(1 - conjugate(v_H[6])*z)),z);
<a name="line_606"></a>
<a name="line_607"></a>recenter_H[11] := 
<a name="line_608"></a> unapply(simplify((z - v_H[11])/(1 - conjugate(v_H[6])*z)),z);
<a name="line_609"></a>
<a name="line_610"></a><span style="color:red">#@ square_diffeo_H_ca 
</span><a name="line_611"></a>square_diffeo_H_ca := (1+I*a_H)/ap_H;
<a name="line_612"></a>
<a name="line_613"></a><span style="color:red">#@ square_diffeo_H_ra 
</span><a name="line_614"></a>square_diffeo_H_ra := am_H/(1+a_H);
<a name="line_615"></a>
<a name="line_616"></a><span style="color:red">#@ square_diffeo_H_ma 
</span><a name="line_617"></a>square_diffeo_H_ma := (z) -> (square_diffeo_H_ca-z)/(1-square_diffeo_H_ca*z);
<a name="line_618"></a>
<a name="line_619"></a><span style="color:red">#@ square_diffeo_H_pa 
</span><a name="line_620"></a>square_diffeo_H_pa := (z) -> log(abs(square_diffeo_H_ma(z)))/log(square_diffeo_H_ra);
<a name="line_621"></a>
<a name="line_622"></a><span style="color:red">#@ square_diffeo_H_cb 
</span><a name="line_623"></a>square_diffeo_H_cb := (ap_H+I*am_H)/(1+I);
<a name="line_624"></a>
<a name="line_625"></a><span style="color:red">#@ square_diffeo_H_rb 
</span><a name="line_626"></a>square_diffeo_H_rb := (ap_H+am_H)/a_H/sqrt(2);
<a name="line_627"></a>
<a name="line_628"></a><span style="color:red">#@ square_diffeo_H_mb 
</span><a name="line_629"></a>square_diffeo_H_mb := (z) -> (I*square_diffeo_H_cb-z)/(1-square_diffeo_H_cb*z);
<a name="line_630"></a>
<a name="line_631"></a><span style="color:red">#@ square_diffeo_H_pb 
</span><a name="line_632"></a>square_diffeo_H_pb := (z) -> log(abs(square_diffeo_H_mb(z)))/log(square_diffeo_H_rb);
<a name="line_633"></a>
<a name="line_634"></a><span style="color:red">#@ square_diffeo_H 
</span><a name="line_635"></a>square_diffeo_H := (z) -> [1-square_diffeo_H_pa(z),square_diffeo_H_pb(z)];
<a name="line_636"></a>
<a name="line_637"></a><span style="color:red">#@ square_diffeo_inv_H0 
</span><a name="line_638"></a>square_diffeo_inv_H0 := proc(t)
<a name="line_639"></a> local sa,sb,ca,cb,aa,bb,z;
<a name="line_640"></a> 
<a name="line_641"></a> sa := square_diffeo_H_ra0^(1-t[1]);
<a name="line_642"></a> sb := square_diffeo_H_rb0^t[2];
<a name="line_643"></a> ca := (sa^2/square_diffeo_H_ca0 -     square_diffeo_H_ca0)/(sa^2 - 1);
<a name="line_644"></a> cb := (sb^2/square_diffeo_H_cb0 - I * square_diffeo_H_cb0)/(sb^2 - 1);
<a name="line_645"></a> aa := Re(ca*conjugate(ca - cb)/abs(ca - cb)^2);
<a name="line_646"></a> bb := sqrt((abs(ca)^2 - 1)/abs(ca - cb)^2 - aa^2);
<a name="line_647"></a> z := (1 - aa + I * bb) * ca + (aa - I * bb) * cb;
<a name="line_648"></a> return z;
<a name="line_649"></a> 
<a name="line_650"></a>end:
<a name="line_651"></a>
<a name="line_652"></a>######################################################################
<a name="line_653"></a>
<a name="line_654"></a># This gives an array of group elements in Pi tilde.  The corresponding
<a name="line_655"></a># translates of F16 fit together nicely and contain a reasonable 
<a name="line_656"></a># neighbourhood of F16.
<a name="line_657"></a>
<a name="line_658"></a><span style="color:red">#@ tile
</span><a name="line_659"></a>tile[-1, 2.2] := [LLL,[2,1]]:
<a name="line_660"></a>tile[-1, 2  ] := [LLLMN,[6,5,6]]:
<a name="line_661"></a>tile[-1, 1.8] := [LLLM,[5]]:
<a name="line_662"></a>tile[-1, 1.6] := [LLMN,[6,5,6,7,0]]:
<a name="line_663"></a>tile[-1, 1.4] := [LLM,[5,4,3]]:
<a name="line_664"></a>tile[-1, 1.2] := [LMN,[6,0,7]]:
<a name="line_665"></a>tile[-1, 1  ] := [LM,[2,3,4]]:
<a name="line_666"></a>tile[-1, 0  ] := [LN,[]]:
<a name="line_667"></a>tile[-1,-0.2] := [L,[]]:
<a name="line_668"></a>tile[-1,-0.4] := [LLN,[]]:
<a name="line_669"></a>tile[-1,-0.6] := [LL,[]]:
<a name="line_670"></a>tile[-1,-0.8] := [LLLN,[]]:
<a name="line_671"></a>tile[-1,-1  ] := [LLL,[]]:
<a name="line_672"></a>tile[-1,-1.2] := [LLLMN,[6]]:
<a name="line_673"></a>
<a name="line_674"></a>tile[ 0, 4] := [1,[0,7]]:
<a name="line_675"></a>tile[ 0, 3] := [N,[3,4]]:
<a name="line_676"></a>tile[ 0, 2] := [M,[2]]:
<a name="line_677"></a>tile[ 0, 1] := [MN,[6]]:
<a name="line_678"></a>tile[ 0, 0] := [1,[]]:
<a name="line_679"></a>tile[ 0,-1] := [N,[]]:
<a name="line_680"></a>tile[ 0,-2] := [M,[2,3,4]]:
<a name="line_681"></a>tile[ 0,-3] := [MN,[6,0,7]]:
<a name="line_682"></a>tile[ 0,-4] := [1,[3,4]]:
<a name="line_683"></a>
<a name="line_684"></a>tile[ 1, 4] := [LLN,[7]]:
<a name="line_685"></a>tile[ 1, 3] := [LL,[4,3,4]]:
<a name="line_686"></a>tile[ 1, 2] := [LLMN,[6,5,6,7]]:
<a name="line_687"></a>tile[ 1, 1] := [LLM,[5,4,3,4]]:
<a name="line_688"></a>tile[ 1, 0] := [LLN,[4]]:
<a name="line_689"></a>tile[ 1,-1] := [LL,[4]]:
<a name="line_690"></a>tile[ 1,-2] := [LLMN,[6,5,6,4]]:
<a name="line_691"></a>tile[ 1,-3] := [LLM,[5,4]]:
<a name="line_692"></a>tile[ 1,-4] := [LLN,[4,3,4]]:
<a name="line_693"></a>
<a name="line_694"></a>
<a name="line_695"></a>
  </pre>
 </body>
</html>
    