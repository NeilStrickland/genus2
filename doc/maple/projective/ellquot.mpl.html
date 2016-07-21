<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># This file contains code related to two elliptic curves which are
<a name="line_2"></a># quotients of PX(a).  One is denoted by E^+ in LaTeX and Ep in Maple,
<a name="line_3"></a># the other is E^- or Em.  The curves Ep and Em are defined as
<a name="line_4"></a># subvarieties of CP^4, but there are affine curves Ep_0 and
<a name="line_5"></a># Em_0 in C^2 which are isomorphic to dense open subvarieties of
<a name="line_6"></a># Ep and Em respectively.  These are quartic curves which pass through
<a name="line_7"></a># the origin, and we take the origin (not a point at infinity) as the
<a name="line_8"></a># zero element for a group structure.
<a name="line_9"></a>
<a name="line_10"></a><span style="color:red">#@ is_equal_CP2
</span><a name="line_11"></a>is_equal_CP2 := proc(z,w)
<a name="line_12"></a> local i,j;
<a name="line_13"></a> for i from 1 to 2 do
<a name="line_14"></a>  for j from i+1 to 3 do
<a name="line_15"></a>   if simplify(conjugate(simplify(z[i]*w[j] - z[j]*w[i]))) <> 0 then
<a name="line_16"></a>    return(false);
<a name="line_17"></a>   fi;
<a name="line_18"></a>  od;
<a name="line_19"></a> od;
<a name="line_20"></a> return(true);
<a name="line_21"></a>end:
<a name="line_22"></a>
<a name="line_23"></a><span style="color:red">#@ d_CP2
</span><a name="line_24"></a>d_CP2 := (u,v) -> sqrt(add(add(abs(u[i]*v[j]-u[j]*v[i])^2,j=i+1..3),i=1..2)/
<a name="line_25"></a>                       (add(abs(u[i])^2,i=1..3)*add(abs(v[i])^2,i=1..3)));
<a name="line_26"></a>
<a name="line_27"></a><span style="color:red">#@ d_CP2_0
</span><a name="line_28"></a>d_CP2_0 := (u,v) -> d_CP2([u[1],u[2],1],[v[1],v[2],1]);
<a name="line_29"></a>
<a name="line_30"></a><span style="color:red">#@ ss
</span><a name="line_31"></a>ss := [seq(s[i],i=1..5)];
<a name="line_32"></a>
<a name="line_33"></a># The curves Ep_0 and Em_0 have equations y^2 = q_Ep(x) and 
<a name="line_34"></a># y^2 = q_Em(x) respectively, where q_Ep and q_Em are given below.
<a name="line_35"></a>
<a name="line_36"></a>q_Ep := (x) -> 2*x*(x-1)*((1/a_P+a_P)^2/4*x^2-1); <span style="color:red">#@ q_Ep
</span><a name="line_37"></a>q_Em := (x) -> 2*x*(x-1)*((1/a_P-a_P)^2/4*x^2+1); <span style="color:red">#@ q_Em
</span><a name="line_38"></a>
<a name="line_39"></a>is_member_Ep_0 := (yx) -> yx[1]^2 - q_Ep(yx[2]); <span style="color:red">#@ is_member_Ep_0
</span><a name="line_40"></a>is_member_Em_0 := (yx) -> yx[1]^2 - q_Em(yx[2]); <span style="color:red">#@ is_member_Em_0
</span><a name="line_41"></a>
<a name="line_42"></a><span style="color:red">#@ Ep_rel
</span><a name="line_43"></a>Ep_rel[0] := (z) -> z[1]^2 - 2*(z[4]-z[3])*((1/a_P+a_P)^2/4*z[4]-z[2]); 
<a name="line_44"></a>Ep_rel[1] := (z) -> z[2]*z[4] - z[3]^2;                                 
<a name="line_45"></a>
<a name="line_46"></a><span style="color:red">#@ Em_rel
</span><a name="line_47"></a>Em_rel[0] := (z) -> z[1]^2 - 2*(z[4]-z[3])*((1/a_P-a_P)^2/4*z[4]+z[2]);
<a name="line_48"></a>Em_rel[1] := (z) -> z[2]*z[4] - z[3]^2;
<a name="line_49"></a>
<a name="line_50"></a>is_member_Ep := (z) -> simplify_P([seq(Ep_rel[i](z),i=0..1)]) = [0$2]; <span style="color:red">#@ is_member_Ep
</span><a name="line_51"></a>is_member_Em := (z) -> simplify_P([seq(Em_rel[i](z),i=0..1)]) = [0$2]; <span style="color:red">#@ is_member_Em
</span><a name="line_52"></a>
<a name="line_53"></a><span style="color:red">#@ is_equal_Ep_list
</span><a name="line_54"></a>is_equal_Ep_list := (z,w) -> [seq(seq(z[i]*w[j]-z[j]*w[i],j=i+1..4),i=1..3)]; 
<a name="line_55"></a>
<a name="line_56"></a><span style="color:red">#@ is_equal_Ep
</span><a name="line_57"></a>is_equal_Ep := proc(z,w)
<a name="line_58"></a> local L,u;
<a name="line_59"></a>
<a name="line_60"></a> L := is_equal_Ep_list(z,w);
<a name="line_61"></a> for u in L do 
<a name="line_62"></a>  if simplify_P(conjugate(simplify_P(expand(u)))) <> 0 then
<a name="line_63"></a>   return(false);
<a name="line_64"></a>  fi;
<a name="line_65"></a> od;
<a name="line_66"></a> return(true);
<a name="line_67"></a>end:
<a name="line_68"></a>
<a name="line_69"></a><span style="color:red">#@ is_equal_Em_list
</span><a name="line_70"></a>is_equal_Em_list := (z,w) -> [seq(seq(z[i]*w[j]-z[j]*w[i],j=i+1..4),i=1..3)];
<a name="line_71"></a>
<a name="line_72"></a><span style="color:red">#@ is_equal_Em
</span><a name="line_73"></a>is_equal_Em := proc(z,w)
<a name="line_74"></a> local L,u;
<a name="line_75"></a>
<a name="line_76"></a> L := is_equal_Em_list(z,w);
<a name="line_77"></a> for u in L do 
<a name="line_78"></a>  if simplify_P(conjugate(simplify_P(expand(u)))) <> 0 then
<a name="line_79"></a>   return(false);
<a name="line_80"></a>  fi;
<a name="line_81"></a> od;
<a name="line_82"></a> return(true);
<a name="line_83"></a>end:
<a name="line_84"></a>
<a name="line_85"></a>Ep_vars := tdeg(z[1],z[2],z[3],z[4]);                  <span style="color:red">#@ Ep_vars
</span><a name="line_86"></a>Ep_rels := Basis([Ep_rel[0](z),Ep_rel[1](z)],Ep_vars); <span style="color:red">#@ Ep_rels
</span><a name="line_87"></a>NF_Ep := (u) -> NormalForm(u,Ep_rels,Ep_vars);         <span style="color:red">#@ NF_Ep
</span><a name="line_88"></a>
<a name="line_89"></a>Em_vars := tdeg(z[1],z[2],z[3],z[4]);                  <span style="color:red">#@ Em_vars
</span><a name="line_90"></a>Em_rels := Basis([Em_rel[0](z),Em_rel[1](z)],Em_vars); <span style="color:red">#@ Em_rels
</span><a name="line_91"></a>NF_Em := (u) -> NormalForm(u,Em_rels,Em_vars);         <span style="color:red">#@ NF_Em
</span><a name="line_92"></a>
<a name="line_93"></a>j_Ep     := (yx) -> [yx[1],1,yx[2],yx[2]^2]; <span style="color:red">#@ j_Ep
</span><a name="line_94"></a>j_inv_Ep := (x)  -> [x[1]/x[2],x[3]/x[2]];   <span style="color:red">#@ j_inv_Ep
</span><a name="line_95"></a>
<a name="line_96"></a>j_Em     := (yx) -> [yx[1],1,yx[2],yx[2]^2]; <span style="color:red">#@ j_Em
</span><a name="line_97"></a>j_inv_Em := (x)  -> [x[1]/x[2],x[3]/x[2]];   <span style="color:red">#@ j_inv_Em
</span><a name="line_98"></a>
<a name="line_99"></a><span style="color:red">#@ d_CP3
</span><a name="line_100"></a>d_CP3 := (u,v) -> sqrt(add(add(abs(u[i]*v[j]-u[j]*v[i])^2,j=i+1..4),i=1..3)/
<a name="line_101"></a>                       (add(abs(u[i])^2,i=1..4)*add(abs(v[i])^2,i=1..4)));
<a name="line_102"></a>
<a name="line_103"></a># This function takes values in Ep_0, and has second component exp(I*t).
<a name="line_104"></a># It is well-behaved for small positive values of t.
<a name="line_105"></a>
<a name="line_106"></a><span style="color:red">#@ l_Ep
</span><a name="line_107"></a>l_Ep := (t) -> [(1/2+1/2*I)*sqrt(2)*(-a_P^2+1)*sqrt(tan((1/2)*t))*
<a name="line_108"></a>                  sqrt(1+I*tan((1/2)*t)*(1-a_P)^2/(1+a_P)^2)*
<a name="line_109"></a>                  sqrt(1+I*tan((1/2)*t)*(1+a_P)^2/(1-a_P)^2)*
<a name="line_110"></a>                   exp((1/2*I)*t)/((1-I*tan((1/2)*t))^(3/2)*a_P),
<a name="line_111"></a>                 exp(I*t)];
<a name="line_112"></a>		 
<a name="line_113"></a># The function P_to_Ep_0 gives a map from (the affine model) of
<a name="line_114"></a># PX(a) to Ep_0, which induces an isomorphism from
<a name="line_115"></a># PX(a)/\mu to Ep.
<a name="line_116"></a>
<a name="line_117"></a><span style="color:red">#@ P_to_Ep_0
</span><a name="line_118"></a>P_to_Ep_0 := proc(wz)
<a name="line_119"></a> local w,z;
<a name="line_120"></a> w,z := op(wz);
<a name="line_121"></a> return([2*w*(1-z)/((1+z^2)^2),2*z/(1+z^2)]);
<a name="line_122"></a>end:
<a name="line_123"></a>
<a name="line_124"></a><span style="color:red">#@ Ep_to_P_0
</span><a name="line_125"></a>Ep_to_P_0 := proc(yx) 
<a name="line_126"></a> local y,x;
<a name="line_127"></a> y,x := op(yx);
<a name="line_128"></a> return [y*((2-x)*sqrt(1-x^2)-(x+2)*(x-1))/(x^3*(x-1)),(1+sqrt(1-x^2))/x];
<a name="line_129"></a>end:
<a name="line_130"></a>
<a name="line_131"></a><span style="color:red">#@ P_to_Ep
</span><a name="line_132"></a>P_to_Ep := (z) -> [2*(z[2]-z[3])*z[1],
<a name="line_133"></a>                   z[2]^2+2*z[2]*z[4]+z[3]*z[5],
<a name="line_134"></a>		   2*z[2]*(z[3]+z[5]),
<a name="line_135"></a>		   4*z[2]*z[4]];
<a name="line_136"></a>
<a name="line_137"></a># The function P_to_Em_0 gives a map from (the affine model) of
<a name="line_138"></a># PX(a) to Em_0, which induces an isomorphism from
<a name="line_139"></a># PX(a)/\lambda\mu to Em.
<a name="line_140"></a>
<a name="line_141"></a><span style="color:red">#@ P_to_Em_0
</span><a name="line_142"></a>P_to_Em_0 := proc(wz)
<a name="line_143"></a> local w,z;
<a name="line_144"></a> w,z := op(wz);
<a name="line_145"></a> return([-(1+I)*sqrt(2)*w*(I+z)/((1-z^2)^2),2*I*z/(1-z^2)]);
<a name="line_146"></a>end:
<a name="line_147"></a>
<a name="line_148"></a><span style="color:red">#@ Em_to_P_0
</span><a name="line_149"></a>Em_to_P_0 := proc(yx) 
<a name="line_150"></a> local y,x;
<a name="line_151"></a> y,x := op(yx);
<a name="line_152"></a> return [(1+I)/sqrt(2)*y*((x-2)*sqrt(1-x^2)-(x+2)*(x-1))/(x^3*(x-1)), -I*(1-sqrt(1-x^2))/x];
<a name="line_153"></a>end:
<a name="line_154"></a>
<a name="line_155"></a><span style="color:red">#@ P_to_Em
</span><a name="line_156"></a>P_to_Em := (z) -> [(1-I)*sqrt(2)*z[1]*(z[2]-I*z[3]),
<a name="line_157"></a>                   z[2]^2-2*z[2]*z[4]+z[3]*z[5],
<a name="line_158"></a>		   2*I*z[2]*(z[3]-z[5]),
<a name="line_159"></a>		   -4*z[2]*z[4]];
<a name="line_160"></a>
<a name="line_161"></a><span style="color:red">#@ P_to_Epm_0
</span><a name="line_162"></a>P_to_Epm_0 := (wz) -> [op(P_to_Ep_0(wz)),op(P_to_Em_0(wz))];
<a name="line_163"></a>
<a name="line_164"></a><span style="color:red">#@ PJ_trans
</span><a name="line_165"></a>PJ_trans := (u) -> [op(Em_0_trans[1]([u[1],u[2]])),op(Ep_0_trans[1]([u[3],u[4]]))];
<a name="line_166"></a>
<a name="line_167"></a><span style="color:red">#@ PJ_trans
</span><a name="line_168"></a>P_to_PJ := proc(wz,u_)
<a name="line_169"></a> local w,z,u;
<a name="line_170"></a>
<a name="line_171"></a> w,z := op(wz);
<a name="line_172"></a> u := `if`(nargs>1,u_,sqrt(z));
<a name="line_173"></a> 
<a name="line_174"></a> return [
<a name="line_175"></a>  1/sqrt(2)*u*(1-z)*(2*bp_P^2*(1-z)^2-bm_P^2*(w/u+1+z^2))/(bm_P^2*z-  (1-z)^2)^2,
<a name="line_176"></a>    (w/u - (1-z)^2)/(bm_P^2*z-  (1-z)^2)/2,
<a name="line_177"></a>  (1+I)/2  *u*(I+z)*(2*bm_P^2*(I+z)^2+bp_P^2*(w/u+1-z^2))/(bp_P^2*z+I*(I+z)^2)^2,
<a name="line_178"></a>  I*(w/u + (I+z)^2)/(bp_P^2*z+I*(I+z)^2)/2
<a name="line_179"></a> ];
<a name="line_180"></a>
<a name="line_181"></a>end:
<a name="line_182"></a>
<a name="line_183"></a><span style="color:red">#@ PJ_to_Epm_0
</span><a name="line_184"></a>PJ_to_Epm_0 := (u) -> [op(Em_0_to_Ep_0([u[1],u[2]])),op(Ep_0_to_Em_0([u[3],u[4]]))];
<a name="line_185"></a>
<a name="line_186"></a>
<a name="line_187"></a>######################################################################
<a name="line_188"></a># Note that only the normaliser of M acts on Ep, and only the 
<a name="line_189"></a># normaliser of LM acts on Em.
<a name="line_190"></a>
<a name="line_191"></a><span style="color:red">#@ act_Ep_0
</span><a name="line_192"></a>act_Ep_0[1]    := yx -> [ yx[1],yx[2]];
<a name="line_193"></a>act_Ep_0[LL]   := yx -> [-yx[1],yx[2]];
<a name="line_194"></a>act_Ep_0[M]    := yx -> [ yx[1],yx[2]];
<a name="line_195"></a>act_Ep_0[LLM]  := yx -> [-yx[1],yx[2]];
<a name="line_196"></a>act_Ep_0[N]    := yx -> [ conjugate(yx[1]),conjugate(yx[2])];
<a name="line_197"></a>act_Ep_0[LLN]  := yx -> [-conjugate(yx[1]),conjugate(yx[2])];
<a name="line_198"></a>act_Ep_0[MN]   := yx -> [ conjugate(yx[1]),conjugate(yx[2])];
<a name="line_199"></a>act_Ep_0[LLMN] := yx -> [-conjugate(yx[1]),conjugate(yx[2])];
<a name="line_200"></a>
<a name="line_201"></a><span style="color:red">#@ act_Ep
</span><a name="line_202"></a>act_Ep[1]    := z -> [ z[1], z[2], z[3], z[4]];
<a name="line_203"></a>act_Ep[LL]   := z -> [-z[1], z[2], z[3], z[4]];
<a name="line_204"></a>act_Ep[M]    := z -> [ z[1], z[2], z[3], z[4]];
<a name="line_205"></a>act_Ep[LLM]  := z -> [-z[1], z[2], z[3], z[4]];
<a name="line_206"></a>act_Ep[N]    := z -> map(conjugate,[ z[1], z[2], z[3], z[4]]);
<a name="line_207"></a>act_Ep[LLN]  := z -> map(conjugate,[-z[1], z[2], z[3], z[4]]);
<a name="line_208"></a>act_Ep[MN]   := z -> map(conjugate,[ z[1], z[2], z[3], z[4]]);
<a name="line_209"></a>act_Ep[LLMN] := z -> map(conjugate,[-z[1], z[2], z[3], z[4]]);
<a name="line_210"></a>
<a name="line_211"></a><span style="color:red">#@ act_Em_0
</span><a name="line_212"></a>act_Em_0[1]    := yx -> yx;
<a name="line_213"></a>act_Em_0[LL]   := yx -> [-yx[1],yx[2]];
<a name="line_214"></a>act_Em_0[LM]   := yx -> yx;
<a name="line_215"></a>act_Em_0[LLLM] := yx -> [-yx[1],yx[2]];
<a name="line_216"></a>act_Em_0[LN]   := yx -> [ conjugate(yx[1]),conjugate(yx[2])];
<a name="line_217"></a>act_Em_0[LLLN] := yx -> [-conjugate(yx[1]),conjugate(yx[2])];
<a name="line_218"></a>act_Em_0[MN]   := yx -> [ conjugate(yx[1]),conjugate(yx[2])];
<a name="line_219"></a>act_Em_0[LLMN] := yx -> [-conjugate(yx[1]),conjugate(yx[2])];
<a name="line_220"></a>
<a name="line_221"></a><span style="color:red">#@ act_Em
</span><a name="line_222"></a>act_Em[1]    := z -> [ z[1], z[2], z[3], z[4]];
<a name="line_223"></a>act_Em[LL]   := z -> [-z[1], z[2], z[3], z[4]];
<a name="line_224"></a>act_Em[LM]   := z -> [ z[1], z[2], z[3], z[4]];
<a name="line_225"></a>act_Em[LLLM] := z -> [-z[1], z[2], z[3], z[4]];
<a name="line_226"></a>act_Em[LN]   := z -> map(conjugate,[ z[1], z[2], z[3], z[4]]);
<a name="line_227"></a>act_Em[LLLN] := z -> map(conjugate,[-z[1], z[2], z[3], z[4]]);
<a name="line_228"></a>act_Em[MN]   := z -> map(conjugate,[ z[1], z[2], z[3], z[4]]);
<a name="line_229"></a>act_Em[LLMN] := z -> map(conjugate,[-z[1], z[2], z[3], z[4]]);
<a name="line_230"></a>
<a name="line_231"></a>######################################################################
<a name="line_232"></a>
<a name="line_233"></a><span style="color:red">#@ v_Ep_0
</span><a name="line_234"></a>v_Ep_0[ 0] := [0,0];
<a name="line_235"></a>v_Ep_0[ 1] := [0,0];
<a name="line_236"></a>v_Ep_0[ 2] := [a_P-1/a_P,-1];
<a name="line_237"></a>v_Ep_0[ 3] := [0,1];
<a name="line_238"></a>v_Ep_0[ 4] := [1/a_P-a_P,-1];
<a name="line_239"></a>v_Ep_0[ 5] := [0,1];
<a name="line_240"></a>v_Ep_0[ 6] := [infinity,infinity];
<a name="line_241"></a>v_Ep_0[ 7] := [infinity,infinity];
<a name="line_242"></a>v_Ep_0[ 8] := [infinity,infinity];
<a name="line_243"></a>v_Ep_0[ 9] := [infinity,infinity];
<a name="line_244"></a>v_Ep_0[10] := [0,-2*a_P/(1+a_P^2)];
<a name="line_245"></a>v_Ep_0[11] := [0, 2*a_P/(1+a_P^2)];
<a name="line_246"></a>v_Ep_0[12] := [0,-2*a_P/(1+a_P^2)];
<a name="line_247"></a>v_Ep_0[13] := [0, 2*a_P/(1+a_P^2)];
<a name="line_248"></a>
<a name="line_249"></a>v_Ep_0[14] := [ 2*sqrt(2)*a_P*(1-a_P^2)^2/(1+a_P^2)^3, 4*a_P^2/(1+a_P^2)^2];
<a name="line_250"></a>v_Ep_0[15] := [-2*sqrt(2)*a_P*(1-a_P^2)^2/(1+a_P^2)^3, 4*a_P^2/(1+a_P^2)^2];
<a name="line_251"></a>
<a name="line_252"></a><span style="color:red">#@ v_Ep
</span><a name="line_253"></a>for i from 0 to 15 do
<a name="line_254"></a> if not(has(v_Ep_0[i],infinity)) then
<a name="line_255"></a>  v_Ep[i] := j_Ep(v_Ep_0[i]);
<a name="line_256"></a> fi;
<a name="line_257"></a>od:
<a name="line_258"></a>
<a name="line_259"></a>v_Ep[ 6] := [ a_P+1/a_P,0,0,-sqrt(2)];
<a name="line_260"></a>v_Ep[ 7] := [-a_P-1/a_P,0,0,-sqrt(2)];
<a name="line_261"></a>v_Ep[ 8] := [-a_P-1/a_P,0,0,-sqrt(2)];
<a name="line_262"></a>v_Ep[ 9] := [ a_P+1/a_P,0,0,-sqrt(2)];
<a name="line_263"></a>
<a name="line_264"></a><span style="color:red">#@ omega_Em
</span><a name="line_265"></a>omega_Em := (1+I)/sqrt(2);
<a name="line_266"></a>
<a name="line_267"></a><span style="color:red">#@ v_Em_0
</span><a name="line_268"></a>v_Em_0[ 0] := [0,0];
<a name="line_269"></a>v_Em_0[ 1] := [0,0];
<a name="line_270"></a>v_Em_0[ 2] := [infinity,infinity];
<a name="line_271"></a>v_Em_0[ 3] := [infinity,infinity];
<a name="line_272"></a>v_Em_0[ 4] := [infinity,infinity];
<a name="line_273"></a>v_Em_0[ 5] := [infinity,infinity];
<a name="line_274"></a>v_Em_0[ 6] := [ (a_P+1/a_P),-1];
<a name="line_275"></a>v_Em_0[ 7] := [0,1];
<a name="line_276"></a>v_Em_0[ 8] := [-(a_P+1/a_P),-1];
<a name="line_277"></a>v_Em_0[ 9] := [0,1];
<a name="line_278"></a>v_Em_0[10] := [0,-2*I*a_P/(1-a_P^2)];
<a name="line_279"></a>v_Em_0[11] := [0, 2*I*a_P/(1-a_P^2)];
<a name="line_280"></a>v_Em_0[12] := [0, 2*I*a_P/(1-a_P^2)];
<a name="line_281"></a>v_Em_0[13] := [0,-2*I*a_P/(1-a_P^2)];
<a name="line_282"></a>
<a name="line_283"></a><span style="color:red">#@ v_Em
</span><a name="line_284"></a>for i from 0 to 13 do
<a name="line_285"></a> if not(has(v_Em_0[i],infinity)) then
<a name="line_286"></a>  v_Em[i] := j_Em(v_Em_0[i]);
<a name="line_287"></a> fi;
<a name="line_288"></a>od:
<a name="line_289"></a>
<a name="line_290"></a>v_Em[ 2] := [ a_P-1/a_P,0,0,-sqrt(2)];
<a name="line_291"></a>v_Em[ 3] := [ a_P-1/a_P,0,0,-sqrt(2)];
<a name="line_292"></a>v_Em[ 4] := [-a_P+1/a_P,0,0,-sqrt(2)];
<a name="line_293"></a>v_Em[ 5] := [-a_P+1/a_P,0,0,-sqrt(2)];
<a name="line_294"></a>
<a name="line_295"></a>for i from 0 to 8 do 
<a name="line_296"></a> c_Ep_0[i] := unapply(simplify(P_to_Ep_0(pq_P(c_P[i](t)))),t); <span style="color:red">#@ c_Ep_0
</span><a name="line_297"></a> c_Em_0[i] := unapply(simplify(P_to_Em_0(pq_P(c_P[i](t)))),t); <span style="color:red">#@ c_Em_0
</span><a name="line_298"></a> c_Ep[i]   := unapply(simplify(P_to_Ep(c_P[i](t))),t);         <span style="color:red">#@ c_Ep
</span><a name="line_299"></a> c_Em[i]   := unapply(simplify(P_to_Em(c_P[i](t))),t);         <span style="color:red">#@ c_Em
</span><a name="line_300"></a>od:
<a name="line_301"></a>
<a name="line_302"></a>######################################################################
<a name="line_303"></a>
<a name="line_304"></a># We make Ep into a group in the usual way, with the point (0,0) in Ep_0
<a name="line_305"></a># as the zero element, and similarly for Em.  The points of order two are
<a name="line_306"></a># as follows.
<a name="line_307"></a>
<a name="line_308"></a><span style="color:red">#@ Ep_0_e
</span><a name="line_309"></a>Ep_0_e[0] := [0,0];
<a name="line_310"></a>Ep_0_e[1] := [0,1];
<a name="line_311"></a>Ep_0_e[2] := [0, 2*a_P/(1+a_P^2)];
<a name="line_312"></a>Ep_0_e[3] := [0,-2*a_P/(1+a_P^2)];
<a name="line_313"></a>
<a name="line_314"></a><span style="color:red">#@ Ep_0_e
</span><a name="line_315"></a>Em_0_e[0] := [0,0];
<a name="line_316"></a>Em_0_e[1] := [0,1];
<a name="line_317"></a>Em_0_e[2] := [0, 2*I*a_P/(1-a_P^2)];
<a name="line_318"></a>Em_0_e[3] := [0,-2*I*a_P/(1-a_P^2)];
<a name="line_319"></a>
<a name="line_320"></a>for i from 0 to 3 do
<a name="line_321"></a> Ep_e[i] := j_Ep(Ep_0_e[i]); <span style="color:red">#@ Ep_e
</span><a name="line_322"></a> Em_e[i] := j_Em(Em_0_e[i]); <span style="color:red">#@ Em_e
</span><a name="line_323"></a>od:
<a name="line_324"></a>
<a name="line_325"></a># The map Ep_0_trans[i] is u |-> u + Ep_0_e[i].
<a name="line_326"></a>
<a name="line_327"></a><span style="color:red">#@ Ep_0_trans
</span><a name="line_328"></a>Ep_0_trans[0] := (yx) -> yx;
<a name="line_329"></a>Ep_0_trans[1] := (yx) -> [
<a name="line_330"></a> 4*a_P^2*(1-a_P^2)^2 * yx[1]/((1+a_P^2)^2*yx[2] - 4*a_P^2)^2,
<a name="line_331"></a> 4*a_P^2*(yx[2]-1)/((1+a_P^2)^2*yx[2] - 4*a_P^2)
<a name="line_332"></a>];
<a name="line_333"></a>Ep_0_trans[2] := (yx) -> [
<a name="line_334"></a> -8*a_P^2*(1-a_P)^2/(1+a_P^2)*yx[1]/((1-a_P)^2*yx[2]+2*a_P*(1-yx[2]))^2,
<a name="line_335"></a> 2*a_P/(1+a_P^2)*(2*a_P-(1+a_P^2)*yx[2])/((1-a_P)^2*yx[2]+2*a_P*(1-yx[2]))
<a name="line_336"></a>];
<a name="line_337"></a>Ep_0_trans[3] := (yx) -> [
<a name="line_338"></a> -8*a_P^2*(1+a_P)^2/(1+a_P^2)*yx[1]/((1+a_P)^2*yx[2]-2*a_P*(1-yx[2]))^2,
<a name="line_339"></a> 2*a_P/(1+a_P^2)*(2*a_P+(1+a_P^2)*yx[2])/((1+a_P)^2*yx[2]-2*a_P*(1-yx[2]))
<a name="line_340"></a>];
<a name="line_341"></a>
<a name="line_342"></a><span style="color:red">#@ Ep_trans
</span><a name="line_343"></a>Ep_trans[0] := (z) -> [z[1],z[2],z[3],z[4]];
<a name="line_344"></a>
<a name="line_345"></a>Ep_trans[1] := (z) -> [bm_P^2*z[1],
<a name="line_346"></a>                       z[2]-2*bp_P^2*z[3]+bp_P^4*z[4],
<a name="line_347"></a>		       z[2] - (1+bp_P^2)*z[3] + bp_P^2*z[4],
<a name="line_348"></a>		       z[2]-2*z[3]+z[4]] /~ bm_P^2;
<a name="line_349"></a>Ep_trans[2] := (z) -> [ 2*(1-bp_P)*z[1],
<a name="line_350"></a>                       bp_P*z[2]+(2*bp_P^2-4*bp_P)*z[3]+(bp_P^3-4*bp_P^2+4*bp_P)*z[4],
<a name="line_351"></a>		       z[2]-2*z[3]-(bp_P^2-2*bp_P)*z[4],
<a name="line_352"></a>		       z[2]/bp_P-2*z[3]+bp_P*z[4]] /~ ( 2*(1-bp_P));
<a name="line_353"></a>Ep_trans[3] := (z) -> [-2*(1+bp_P)*z[1],
<a name="line_354"></a>                       bp_P*z[2]-(2*bp_P^2+4*bp_P)*z[3]+(bp_P^3+4*bp_P^2+4*bp_P)*z[4],
<a name="line_355"></a>		       -z[2]+2*z[3]+(bp_P^2+2*bp_P)*z[4],
<a name="line_356"></a>		       z[2]/bp_P+2*z[3]+bp_P*z[4]] /~ (-2*(1+bp_P));
<a name="line_357"></a>
<a name="line_358"></a># The map Em_0_trans[i] is u |-> u + Em_0_e[i].
<a name="line_359"></a>
<a name="line_360"></a><span style="color:red">#@ Em_0_trans
</span><a name="line_361"></a>Em_0_trans[0] := (yx) -> yx;
<a name="line_362"></a>Em_0_trans[1] := (yx) -> [
<a name="line_363"></a> -4*a_P^2*(1+a_P^2)^2*yx[1]/((1-a_P^2)^2*yx[2]+4*a_P^2)^2,
<a name="line_364"></a> -(4*a_P^2*(yx[2]-1)/((1-a_P^2)^2*yx[2]+4*a_P^2))
<a name="line_365"></a>];
<a name="line_366"></a>Em_0_trans[2] := (yx) -> [
<a name="line_367"></a> -8*a_P^2*(I+a_P)^2/(1-a_P^2)*yx[1]/((I+a_P)^2*yx[2]+2*I*a_P*(yx[2]-1))^2,
<a name="line_368"></a> -2*I*a_P/(1-a_P^2)*(2*I*a_P-(1-a_P^2)*yx[2])/((I+a_P)^2*yx[2]+2*I*a_P*(yx[2]-1))
<a name="line_369"></a>];
<a name="line_370"></a>Em_0_trans[3] := (yx) -> [
<a name="line_371"></a> -8*a_P^2*(I-a_P)^2/(1-a_P^2)*yx[1]/((I-a_P)^2*yx[2]-2*I*a_P*(yx[2]-1))^2,
<a name="line_372"></a> -2*I*a_P/(1-a_P^2)*(2*I*a_P+(1-a_P^2)*yx[2])/((I-a_P)^2*yx[2]-2*I*a_P*(yx[2]-1))
<a name="line_373"></a>];
<a name="line_374"></a>
<a name="line_375"></a><span style="color:red">#@ Em_trans
</span><a name="line_376"></a>Em_trans[0] := (z) -> [z[1],z[2],z[3],z[4]];
<a name="line_377"></a>
<a name="line_378"></a>Em_trans[1] := (z) -> [bp_P^2*z[1],
<a name="line_379"></a>                       -z[2]-2*bm_P^2*z[3]-bm_P^4*z[4],
<a name="line_380"></a>		       -z[2] + (1-bm_P^2)*z[3] + bm_P^2*z[4],
<a name="line_381"></a>		       -z[2] + 2*z[3] - z[4]] /~ (bp_P^2);
<a name="line_382"></a>
<a name="line_383"></a>Em_trans[2] := (z) -> [(I-bm_P)*z[1],
<a name="line_384"></a>                       -(-bm_P/2*z[2]+bm_P*(bm_P-2*I)^2/2*z[4]+(bm_P*2+I*bm_P^2)*z[3]),
<a name="line_385"></a>                       -(-I/2*z[2]+I*z[3]-(I/2*bm_P^2+bm_P)*z[4]),
<a name="line_386"></a>                       -(z[2]/(2*bm_P)+I*z[3]-bm_P/2*z[4])] /~ (I-bm_P);
<a name="line_387"></a>
<a name="line_388"></a>Em_trans[3] := (z) -> [(I+bm_P)*z[1],
<a name="line_389"></a>                        (-bm_P/2*z[2]+bm_P*(bm_P+2*I)^2/2*z[4]+(bm_P*2-I*bm_P^2)*z[3]),
<a name="line_390"></a>                        (I/2*z[2]-I*z[3]-(-I/2*bm_P^2+bm_P)*z[4]),
<a name="line_391"></a>                        (z[2]/(2*bm_P)-I*z[3]-bm_P/2*z[4])] /~ ((bm_P+I));
<a name="line_392"></a>
<a name="line_393"></a>######################################################################
<a name="line_394"></a># It turns out that Ep and Em are isogenous.  It should be possible to
<a name="line_395"></a># see this abstractly using some representation theory and duality 
<a name="line_396"></a># theory for Jacobians.  More specifically, the function 
<a name="line_397"></a># Ep_0_to_Em_0 defines an isogeny Ep -> Em with kernel generated by
<a name="line_398"></a># the point Ep_0_e[1], whereas Em_0_to_Ep_0 is an isogeny in the 
<a name="line_399"></a># opposite direction with kernel generated by Em_0_e[1].
<a name="line_400"></a>
<a name="line_401"></a><span style="color:red">#@ Ep_0_to_Em_0
</span><a name="line_402"></a>Ep_0_to_Em_0 := proc(yx)
<a name="line_403"></a> local y,x;
<a name="line_404"></a> y,x := op(yx);
<a name="line_405"></a> return [y*sqrt(2)*((1-x)^2+bm_P^2*x^2)/((1-x)^2-bm_P^2*x^2)^2,
<a name="line_406"></a>         2*x*(x-1)/((1-x)^2-bm_P^2*x^2)];
<a name="line_407"></a>end:
<a name="line_408"></a>
<a name="line_409"></a><span style="color:red">#@ Em_0_to_Ep_0
</span><a name="line_410"></a>Em_0_to_Ep_0 := proc(yx)
<a name="line_411"></a> local y,x;
<a name="line_412"></a> y,x := op(yx);
<a name="line_413"></a> return [y*sqrt(2)*((1-x)^2-bp_P^2*x^2)/((1-x)^2+bp_P^2*x^2)^2,
<a name="line_414"></a>         2*x*(x-1)/((1-x)^2+bp_P^2*x^2)];
<a name="line_415"></a>end:
<a name="line_416"></a>
<a name="line_417"></a><span style="color:red">#@ Ep_to_Em
</span><a name="line_418"></a>Ep_to_Em := (z) -> [
<a name="line_419"></a> sqrt(2)*z[1]*(z[2]-2*z[3]+bp_P^2*z[4]),
<a name="line_420"></a> (2-bp_P^2)^2*z[4]^2 +(z[2]-2*z[3]+2*(2-bp_P^2)*z[4])*(z[2]-2*z[3]),
<a name="line_421"></a> 2*(z[2]-bp_P^2*z[4])*(z[4]-z[3])+4*(z[4]^2-2*z[3]*z[4]+z[2]*z[4]),
<a name="line_422"></a> 4*(z[4]^2-2*z[3]*z[4]+z[2]*z[4])
<a name="line_423"></a>];
<a name="line_424"></a>
<a name="line_425"></a><span style="color:red">#@ Em_to_Ep
</span><a name="line_426"></a>Em_to_Ep := (z) -> [
<a name="line_427"></a> sqrt(2)*z[1]*(z[2]-2*z[3]-bm_P^2*z[4]),
<a name="line_428"></a> (2+bm_P^2)^2*z[4]^2 +(z[2]-2*z[3]+2*(2+bm_P^2)*z[4])*(z[2]-2*z[3]),
<a name="line_429"></a> 2*(z[2]+bm_P^2*z[4])*(z[4]-z[3])+4*(z[4]^2-2*z[3]*z[4]+z[2]*z[4]),
<a name="line_430"></a> 4*(z[4]^2-2*z[3]*z[4]+z[2]*z[4])
<a name="line_431"></a>];
<a name="line_432"></a>
<a name="line_433"></a><span style="color:red">#@ Epm_w
</span><a name="line_434"></a>Epm_w[1] := [ 2*bm_P/(1+bm_P)^2, 1/(1+bm_P)];
<a name="line_435"></a>Epm_w[2] := [ 2*bm_P/(1-bm_P)^2, 1/(1-bm_P)];
<a name="line_436"></a>Epm_w[3] := [-2*bm_P/(1+bm_P)^2, 1/(1+bm_P)];
<a name="line_437"></a>Epm_w[4] := [-2*bm_P/(1-bm_P)^2, 1/(1-bm_P)];
<a name="line_438"></a>
<a name="line_439"></a>######################################################################
<a name="line_440"></a>
<a name="line_441"></a><span style="color:red">#@ P_0_to_J_0
</span><a name="line_442"></a>P_0_to_J_0 := proc(wz)
<a name="line_443"></a> local w,z,ym,xm,yp,xp;
<a name="line_444"></a>
<a name="line_445"></a> w,z := op(wz);
<a name="line_446"></a>
<a name="line_447"></a> ym := 1/sqrt(2)*sqrt(z)*(1-z)*(2*bp_P^2*(1-z)^2-bm_P^2*(w/sqrt(z)+1+z^2))/(bm_P^2*z-  (1-z)^2)^2; 
<a name="line_448"></a> yp := (1+I)/2  *sqrt(z)*(I+z)*(2*bm_P^2*(I+z)^2+bp_P^2*(w/sqrt(z)+1-z^2))/(bp_P^2*z+I*(I+z)^2)^2;
<a name="line_449"></a> xm :=   (w/sqrt(z) - (1-z)^2)/(bm_P^2*z-  (1-z)^2)/2;
<a name="line_450"></a> xp := I*(w/sqrt(z) + (I+z)^2)/(bp_P^2*z+I*(I+z)^2)/2;
<a name="line_451"></a>
<a name="line_452"></a> return [ym,xm,yp,xp];
<a name="line_453"></a>end:
<a name="line_454"></a>
<a name="line_455"></a>######################################################################
<a name="line_456"></a># The curves Ep_0 and Em_0 can be parametrised using the 
<a name="line_457"></a># Weierstrass P-function and its derivative.  More specifically, the
<a name="line_458"></a># function C_to_Ep_0 gives a universal covering of Ep by C, with kernel
<a name="line_459"></a># generated by latt_a (in R) and latt_b (in iR).  Similarly, the function 
<a name="line_460"></a># C_to_Em_0 gives a universal covering of Em by C, with kernel 
<a name="line_461"></a># generated by latt_e and latt_f.  The numbers latt_c (in R) and
<a name="line_462"></a># latt_d (in iR) generate a subgroup of index 2 in the kernel.
<a name="line_463"></a>
<a name="line_464"></a>Wg2p := 4*(1/3+bp_P^2);                                  <span style="color:red">#@ Wg2p
</span><a name="line_465"></a>Wg3p := 8/3*(1/9-bp_P^2);                                <span style="color:red">#@ Wg3p
</span><a name="line_466"></a>WPp  := (z) -> WeierstrassP(z/sqrt(2),Wg2p,Wg3p):        <span style="color:red">#@ WPp
</span><a name="line_467"></a>WPPp := (z) -> WeierstrassPPrime(z/sqrt(2),Wg2p,Wg3p):   <span style="color:red">#@ WPPp
</span><a name="line_468"></a>
<a name="line_469"></a>Wg2m := 4*(1/3-bm_P^2);                                  <span style="color:red">#@ Wg2m
</span><a name="line_470"></a>Wg3m := 8/3*(1/9+bm_P^2);                                <span style="color:red">#@ Wg3m
</span><a name="line_471"></a>WPm  := (z) -> WeierstrassP(I*z/sqrt(2),Wg2m,Wg3m):      <span style="color:red">#@ WPm
</span><a name="line_472"></a>WPPm := (z) -> WeierstrassPPrime(I*z/sqrt(2),Wg2m,Wg3m): <span style="color:red">#@ WPPm
</span><a name="line_473"></a>
<a name="line_474"></a>C_to_Ep_0 := (z) -> [- WPPp(z)/(WPp(z)+1/3)^2/sqrt(2), 1/(WPp(z)+1/3)]; <span style="color:red">#@ C_to_Ep_0
</span><a name="line_475"></a>C_to_Em_0 := (z) -> [I*WPPm(z)/(WPm(z)+1/3)^2/sqrt(2), 1/(WPm(z)+1/3)]; <span style="color:red">#@ C_to_Em_0
</span><a name="line_476"></a>
<a name="line_477"></a>latt_a := ap_period;                       <span style="color:red">#@ latt_a
</span><a name="line_478"></a>latt_b := am_period*I;                     <span style="color:red">#@ latt_b
</span><a name="line_479"></a>latt_c := ap_period*sqrt(2);               <span style="color:red">#@ latt_c
</span><a name="line_480"></a>latt_d := am_period*sqrt(2)*I;             <span style="color:red">#@ latt_d
</span><a name="line_481"></a>latt_e := (ap_period+am_period*I)/sqrt(2); <span style="color:red">#@ latt_e
</span><a name="line_482"></a>latt_f := (ap_period-am_period*I)/sqrt(2); <span style="color:red">#@ latt_f
</span><a name="line_483"></a>
<a name="line_484"></a># These are isomorphisms C -> R^2 that send the relevant lattices to Z^2
<a name="line_485"></a>
<a name="line_486"></a><span style="color:red">#@ div_Ep
</span><a name="line_487"></a>div_Ep  := (z) -> [   Im(z/latt_b0)/Im(latt_a0/latt_b0),
<a name="line_488"></a>                      Im(z/latt_a0)/Im(latt_b0/latt_a0)];
<a name="line_489"></a>
<a name="line_490"></a><span style="color:red">#@ div_Em
</span><a name="line_491"></a>div_Em  := (z) -> [   Im(z/latt_e0)/Im(latt_f0/latt_e0),
<a name="line_492"></a>                      Im(z/latt_f0)/Im(latt_e0/latt_f0)];
<a name="line_493"></a>
<a name="line_494"></a><span style="color:red">#@ div_Emq
</span><a name="line_495"></a>div_Emq := (z) -> [-2*Im(z/latt_c0)/Im(latt_d0/latt_c0),
<a name="line_496"></a>                    2*Im(z/latt_d0)/Im(latt_c0/latt_d0)];
<a name="line_497"></a>
<a name="line_498"></a>######################################################################
<a name="line_499"></a>
<a name="line_500"></a><span style="color:red">#@ d_Ep
</span><a name="line_501"></a>
<a name="line_502"></a>d_Ep[0] := (t) -> [(1+a_P)^2*sin(t)*sqrt((1-a_P)^2/a_P + (1+a_P)^2/a_P*sin(t/2)^2)/(4*a_P),
<a name="line_503"></a>                 (1/a_P)*((a_P+1)^2/4*cos(t)-(1-a_P)^2/4)];
<a name="line_504"></a>
<a name="line_505"></a>d_Ep[1] := (t) -> [I*(1-a_P)^2*sin(t)*sqrt((1+a_P)^2/a_P+(1-a_P)^2/a_P*sin(t/2)^2)/(4*a_P),
<a name="line_506"></a>                 (1/a_P)*((1+a_P)^2/4-(1-a_P)^2/4*cos(t))];
<a name="line_507"></a>
<a name="line_508"></a>######################################################################
<a name="line_509"></a># Near v[0], the functions below are inverse to C_to_Ep_0 and C_to_Em_0
<a name="line_510"></a>
<a name="line_511"></a><span style="color:red">#@ Ep_0_to_C
</span><a name="line_512"></a>Ep_0_to_C := (yx) ->
<a name="line_513"></a> 2/(sqrt(a_P)-1/sqrt(a_P))*
<a name="line_514"></a>  EllipticF((sqrt(a_P)-1/sqrt(a_P))*yx[1]/2/(1-yx[2])/sqrt(1-yx[2]^2/4*(a_P+1/a_P)^2),
<a name="line_515"></a>             I*(1+a_P)/(1-a_P));
<a name="line_516"></a>
<a name="line_517"></a><span style="color:red">#@ Em_0_to_C
</span><a name="line_518"></a>Em_0_to_C := (yx) -> 
<a name="line_519"></a> sqrt(2)*(1-I)/(1/sqrt(a_P)-I*sqrt(a_P))*
<a name="line_520"></a>  EllipticF((1+I)/sqrt(2)*yx[1]/2*(1/sqrt(a_P)-I*sqrt(a_P))/(1-yx[2])/sqrt(1+yx[2]^2/4*(1/a_P-a_P)^2),
<a name="line_521"></a>            -I*(1+I*a_P)/(1-I*a_P));
<a name="line_522"></a>
<a name="line_523"></a>
<a name="line_524"></a>######################################################################
<a name="line_525"></a>
<a name="line_526"></a><span style="color:red">#@ find_ca
</span><a name="line_527"></a><span style="color:red">#@ ca
</span><a name="line_528"></a><span style="color:red">#@ ca_string
</span><a name="line_529"></a>
<a name="line_530"></a>find_ca := proc()
<a name="line_531"></a> global i,ca,ca_string;
<a name="line_532"></a> ca[0] := fsolve(Re(C_to_Ep0_0(t*latt_a0+latt_b0/2)[2])+1=0,t=0.271..0.272);
<a name="line_533"></a> ca[1] := Re(fsolve(Ep0_0_trans[1](C_to_Ep0_0(t*latt_a0 + latt_b0/2))[2] - v_Ep0_0[14][2],t = 0.33));
<a name="line_534"></a> ca[2] := brent_fsolve((t) -> table(["err" = Re(1/C_to_Em0_0(t*latt_c0)[2])]),0.37,0.4,false,false,10.^(-90))[1];
<a name="line_535"></a> ca[3] := brent_fsolve((t) -> table(["err" = Re(3*C_to_Em0_0(latt_c0/2+t*latt_d0)[2]-1)]),0.1,0.2,false,false,10.^(-90))[1];
<a name="line_536"></a> ca[4] := evalf(1/4 - 1/sqrt(8) + ca[1]/sqrt(2));
<a name="line_537"></a>
<a name="line_538"></a> ca_string := "";
<a name="line_539"></a> for i from 0 to 4 do 
<a name="line_540"></a>  ca_string := cat(ca_string,sprintf("ca[%d] := %A;\n",i,ca[i]));
<a name="line_541"></a> od:
<a name="line_542"></a>end:
<a name="line_543"></a>
<a name="line_544"></a>ca[0] := .2712198345337267409183751260826859683620639254846839852546832980918572068609141666452521341087864403;
<a name="line_545"></a>ca[1] := .3380191917841065905435356360705057573602229775275223773576731512721457153287874464581375181454782068;
<a name="line_546"></a>ca[2] := .3856099172668633704591875630413429841810319627423419926273416490459286034304570833226260670543932199;
<a name="line_547"></a>ca[3] := .1690819268841186594794124671806576109783450143041054095003132968033979158300508721043901321239984256;
<a name="line_548"></a>ca[4] := .1354622720884641394270016572560535181460733569063074476782555872580258479260520219330188552098104961;
<a name="line_549"></a>
<a name="line_550"></a><span style="color:red">#@ v_TEp
</span><a name="line_551"></a>v_TEp[ 0] := 0;
<a name="line_552"></a>v_TEp[ 1] := 0;
<a name="line_553"></a>v_TEp[ 2] :=  ca[0]*latt_a0 + latt_b0/2;
<a name="line_554"></a>v_TEp[ 3] :=    1/2*latt_a0 + latt_b0/2;
<a name="line_555"></a>v_TEp[ 4] := -ca[0]*latt_a0 + latt_b0/2;
<a name="line_556"></a>v_TEp[ 5] :=    1/2*latt_a0 + latt_b0/2;
<a name="line_557"></a>v_TEp[10] := latt_b0/2;
<a name="line_558"></a>v_TEp[12] := latt_b0/2;
<a name="line_559"></a>
<a name="line_560"></a>######################################################################
<a name="line_561"></a># These are maps R -> C such that the composite with C_to_Ep_0 is
<a name="line_562"></a># approximately the same as the composite
<a name="line_563"></a>#
<a name="line_564"></a>#     c[k]
<a name="line_565"></a>#  R ------> PX_0 ----> Ep_0
<a name="line_566"></a>
<a name="line_567"></a><span style="color:red">#@ c_TEp_approx
</span><a name="line_568"></a>c_TEp_approx[0] := (t) -> (1/2 - (1/2 - ca[0])*cos(t))*latt_a0+latt_b0/2;
<a name="line_569"></a>c_TEp_approx[1] := (t) -> latt_b0 * t/Pi + c_TEp_a[1,1]*sin(t) + c_TEp_a[1,2]*sin(2*t)*I + c_TEp_a[1,3]*sin(3*t);
<a name="line_570"></a>c_TEp_approx[2] := (t) -> latt_b0 * t/Pi - c_TEp_a[1,1]*sin(t) + c_TEp_a[1,2]*sin(2*t)*I - c_TEp_a[1,3]*sin(3*t);
<a name="line_571"></a>c_TEp_approx[3] := (t) -> latt_a0/2 + t*latt_b0/Pi + c_TEp_a[3,2] * sin(2*t) * I;
<a name="line_572"></a>c_TEp_approx[4] := (t) -> latt_b0/2 - c_TEp_a[4,1] * sin(t) - c_TEp_a[4,3] * sin(3*t);
<a name="line_573"></a>c_TEp_approx[5] := (t) -> t*latt_a0/(2*Pi) + c_TEp_a[5,1] * sin(t) + c_TEp_a[5,2] * sin(2*t) + c_TEp_a[5,3] * sin(3*t);
<a name="line_574"></a>c_TEp_approx[6] := (t) -> t*latt_b0/(2*Pi) + (c_TEp_a[6,1] * sin(t) + c_TEp_a[6,2] * sin(2*t) + c_TEp_a[6,3] * sin(3*t)) * I;
<a name="line_575"></a>c_TEp_approx[7] := unapply(c_TEp_approx[5]( t),t);
<a name="line_576"></a>c_TEp_approx[8] := unapply(c_TEp_approx[6](-t),t);
<a name="line_577"></a>
<a name="line_578"></a><span style="color:red">#@ c_TEp_a
</span><a name="line_579"></a>c_TEp_a[1,1] := 0.6247347995397494979304990644922278959925160558822906310627061638472069242981199313035975369049495378;
<a name="line_580"></a>c_TEp_a[1,2] := 0.1271025486963173673053533258315918977758877826348357864853356763913426014752169850359977340543651160;
<a name="line_581"></a>c_TEp_a[1,3] := 0.04381746206649332486668820474497225695049799820309213349946374104678578367977790428958078654155281204;
<a name="line_582"></a>c_TEp_a[3,2] := 0.1042805083196201573848763132278556884018577757032944082115538163208094109327358933030888052414138109;
<a name="line_583"></a>c_TEp_a[4,1] := 0.4968296088425958313893062468630599274335873218089671291381643204020161460960238235450241440954199888;
<a name="line_584"></a>c_TEp_a[4,3] := 0.03071315983358916926430176920663159406977183930465613106149221110811287954564497454251589873134635628;
<a name="line_585"></a>c_TEp_a[5,1] := 0.03427325618888229644323397515847565305897064093054365551658309204485785383605755460381667885437777016;
<a name="line_586"></a>c_TEp_a[5,2] := 0.002482478177069518908910207898024965359042928275607876444506771571486982421239589861216569732858358200;
<a name="line_587"></a>c_TEp_a[5,3] := 0.000238565053005790472482742854541616044857974248413386109265566228277239808644583552185718631302795458;
<a name="line_588"></a>c_TEp_a[6,1] := 0.0558713002227124085540383146452715943529179849726750467583063953157534082410475171178334557306278623;
<a name="line_589"></a>c_TEp_a[6,2] := 0.00332541457691846974120052631026377039577655088554540434419946854408626923142436070630415134361428515;
<a name="line_590"></a>c_TEp_a[6,3] := 0.00030771228226226118360872862753417350317298244674914294818400377809424655149917305792839447143257881;
<a name="line_591"></a>
<a name="line_592"></a><span style="color:red">#@ c_Ep_0_approx[k]
</span><a name="line_593"></a>for k from 0 to 8 do
<a name="line_594"></a> c_Ep_0_approx[k] := unapply(C_to_Ep0_0(c_TEp_approx[k](t)),t);
<a name="line_595"></a>od:
<a name="line_596"></a>
<a name="line_597"></a><span style="color:red">#@ c_TEm_approx
</span><a name="line_598"></a>c_TEm_approx[0] := (t) -> (1/2 - (1/2-ca[1])*sin(t+Pi/4))*latt_c0;
<a name="line_599"></a>c_TEm_approx[1] := (t) -> c_TEm_a[1,1] * sin(t) + c_TEm_a[1,3] * sin(3*t);
<a name="line_600"></a>c_TEm_approx[2] := (t) -> latt_d0 * t/Pi + c_TEm_a[2,2] * sin(2*t) * I;
<a name="line_601"></a>c_TEm_approx[3] := (t) -> latt_c0/4 + (t/(2*Pi)-1/4)*latt_d0 + c_TEm_a[3,1] * sin(t) +
<a name="line_602"></a>                            c_TEm_a[3,2] * sin(2*t) * I + c_TEm_a[3,3] * sin(3*t);  
<a name="line_603"></a>c_TEm_approx[4] := (t) -> latt_c0/4 + (t/(2*Pi)+1/4)*latt_d0 - c_TEm_a[3,1] * sin(t) +
<a name="line_604"></a>                            c_TEm_a[3,2] * sin(2*t) * I - c_TEm_a[3,3] * sin(3*t);  
<a name="line_605"></a>c_TEm_approx[5] := (t) -> t/(4*Pi)*(latt_c0 - latt_d0) + add(c_TEm_a[5,k] * sin(k*t),k=1..3);
<a name="line_606"></a>c_TEm_approx[6] := (t) -> t/(4*Pi)*(latt_c0 + latt_d0) + add(conjugate(c_TEm_a[5,k]) * sin(k*t),k=1..3);
<a name="line_607"></a>c_TEm_approx[7] := eval(c_TEm_approx[6]);
<a name="line_608"></a>c_TEm_approx[8] := eval(c_TEm_approx[5]);
<a name="line_609"></a>
<a name="line_610"></a><span style="color:red">#@ c_TEm_a
</span><a name="line_611"></a>c_TEm_a[1,1] := 0.8835084263955505585783987716840000325926615540942614052941584433964337822854465613444230237701180424;
<a name="line_612"></a>c_TEm_a[1,3] := 0.06196724912320348395700677267078302081353815486017812159352605843249840400768595265032504859994172416;
<a name="line_613"></a>c_TEm_a[2,2] := 0.1797501481785187719080376142790183135790694888474575684045644044454056920630789380074211242867244138;
<a name="line_614"></a>c_TEm_a[3,1] := 0.3511321628863063015929856160517480228866539212422554128973412749459584607846880678817297806565675990;
<a name="line_615"></a>c_TEm_a[3,2] := 0.07373745457838359930142006274768329182000846163947333927778741589054611329411213393198338264266919790;
<a name="line_616"></a>c_TEm_a[3,3] := 0.0218969062105503030917796584579880912692573475308448365690517306473414539254961242930202103669553839;
<a name="line_617"></a>c_TEm_a[4,1] := 0.3511321628863063015929856160517480228866539212422554128973412749459584607846880678817297806565675990;
<a name="line_618"></a>c_TEm_a[4,2] := 0.0737374545783835993014200627476832918200084616394733392777874158905461132941121339319833826426691980;
<a name="line_619"></a>c_TEm_a[4,3] := 0.0218969062105503030917796584579880912692573475308448365690517306473414539254961242930202103669553839;
<a name="line_620"></a>c_TEm_a[5,1] := 0.2423485186450247972105483564735158211534303209384437520256882975599151270558722587190358743762353599e-1-
<a name="line_621"></a>                0.3950697526118940704300624129705708722011083550171984012465296564337878450572058529379116267915483335e-1*I;
<a name="line_622"></a>c_TEm_a[5,2] := 0.1755377153153475687209569034468230122971514325013707849129110392164278545559798591021656717836478500e-2-
<a name="line_623"></a>                0.2351423197595643895076828999665351369198735842034167404505480689584486014610436048646424754749428350e-2*I;
<a name="line_624"></a>c_TEm_a[5,3] := 0.1686909667345225954045278047227347451235211096899810957501328202832040081462419337428583331437649896e-3-
<a name="line_625"></a>                0.2175854414420338678065767392569984415709072283534971244700121606992596899087070672886264959461280586e-3*I;
<a name="line_626"></a>
<a name="line_627"></a><span style="color:red">#@ c_Em_0_approx
</span><a name="line_628"></a>c_Em_0_approx[0] := (t) -> C_to_Em0_0(c_TEm_approx[0](t));
<a name="line_629"></a>c_Em_0_approx[1] := (t) -> C_to_Em0_0(c_TEm_approx[1](t));
<a name="line_630"></a>c_Em_0_approx[2] := (t) -> C_to_Em0_0(c_TEm_approx[2](t));
<a name="line_631"></a>c_Em_0_approx[3] := (t) -> C_to_Em0_0(c_TEm_approx[3](t));
<a name="line_632"></a>c_Em_0_approx[4] := (t) -> C_to_Em0_0(c_TEm_approx[4](t));
<a name="line_633"></a>c_Em_0_approx[5] := (t) -> C_to_Em0_0(c_TEm_approx[5](t));
<a name="line_634"></a>c_Em_0_approx[6] := (t) -> C_to_Em0_0(c_TEm_approx[6](t));
<a name="line_635"></a>c_Em_0_approx[7] := (t) -> C_to_Em0_0(c_TEm_approx[7](t));
<a name="line_636"></a>c_Em_0_approx[8] := (t) -> C_to_Em0_0(c_TEm_approx[8](t));
<a name="line_637"></a>
<a name="line_638"></a>######################################################################
<a name="line_639"></a>
<a name="line_640"></a># The second component of C_to_Ep0_0 gives a doubly periodic map from C
<a name="line_641"></a># to C, which is real on the lines of a square grid, and purely
<a name="line_642"></a># imaginary on curves corresponding to C1 and C2.  It is useful to
<a name="line_643"></a># understand the preimage of the unit circle under this map.  One
<a name="line_644"></a># component of the preimage (which we call the base component) is a
<a name="line_645"></a># figure eight centred at the point (latt_a0+latt_b0)/2.  This should 
<a name="line_646"></a># be thought of as two separate lobes that happen to meet. The other
<a name="line_647"></a># components are translates of the base component by multiples of
<a name="line_648"></a># latt_a0 and latt_b0.
<a name="line_649"></a>
<a name="line_650"></a># These maps land in the relevant locus, but are discontinuous. 
<a name="line_651"></a><span style="color:red">#@ Ep0_circle_lift_a
</span><a name="line_652"></a>Ep0_circle_lift_a :=
<a name="line_653"></a> unapply(evalf(subs(a_P=a_P0,simplify(Ep_0_to_C(          l_Ep(t))))),t):
<a name="line_654"></a>
<a name="line_655"></a><span style="color:red">#@ Ep0_circle_lift_b
</span><a name="line_656"></a>Ep0_circle_lift_b :=
<a name="line_657"></a> unapply(evalf(subs(a_P=a_P0,simplify(Ep_0_to_C([-1,1] *~ l_Ep(t))))),t):
<a name="line_658"></a>
<a name="line_659"></a># This map is a continuous parametrisation of the right hand lobe of
<a name="line_660"></a># the base component, satisfying
<a name="line_661"></a># C_to_Ep0_0(Ep0_circle_lift_c(t))[2] = exp(I*t).
<a name="line_662"></a>
<a name="line_663"></a><span style="color:red">#@ Ep0_circle_lift_c
</span><a name="line_664"></a>Ep0_circle_lift_c := proc(t)
<a name="line_665"></a> local s0,n0;
<a name="line_666"></a> s0 := evalf(t/(2*Pi));
<a name="line_667"></a> n0 := floor(s0);
<a name="line_668"></a> s0 := s0 - n0;
<a name="line_669"></a> if s0 < 0.5 then
<a name="line_670"></a>  Ep0_circle_lift_a(Pi*(   2*s0)) + latt_a0 + latt_b0;
<a name="line_671"></a> else 
<a name="line_672"></a>  Ep0_circle_lift_b(Pi*(-2+2*s0)) + latt_a0;
<a name="line_673"></a> fi;
<a name="line_674"></a>end:
<a name="line_675"></a>
<a name="line_676"></a># This map is a continuous parametrisation of the left hand lobe of
<a name="line_677"></a># the base component, satisfying
<a name="line_678"></a># C_to_Ep0_0(Ep0_circle_lift_d(t))[2] = exp(I*t).
<a name="line_679"></a>
<a name="line_680"></a><span style="color:red">#@ Ep0_circle_lift_d
</span><a name="line_681"></a>Ep0_circle_lift_d := (t) -> latt_a0 + latt_b0 - Ep0_circle_lift_c(t);
<a name="line_682"></a>
<a name="line_683"></a># This is an approximation to Ep0_circle_lift_c
<a name="line_684"></a>
<a name="line_685"></a><span style="color:red">#@ Ep0_circle_lift_e
</span><a name="line_686"></a>Ep0_circle_lift_e := (t) ->
<a name="line_687"></a>  sqrt(0.077410-0.076893*cos(t)-0.00051645*cos(2*t)+
<a name="line_688"></a>       I*(0.076738*sin(t)+0.00033641*sin(2*t))) +
<a name="line_689"></a>  (latt_a0+latt_b0)/2;
<a name="line_690"></a>
<a name="line_691"></a># The image of this map is approximately the same as the base
<a name="line_692"></a># component, but the parametrisation is arbitrary.
<a name="line_693"></a>
<a name="line_694"></a><span style="color:red">#@ Ep0_circle_lift_f
</span><a name="line_695"></a>Ep0_circle_lift_f := (t) -> 
<a name="line_696"></a>   latt_a0/2+latt_b0/2+
<a name="line_697"></a>   0.393*sin(t)+I*(0.13254*sin(2.*t)+0.01876*sin(4.*t));
<a name="line_698"></a>
<a name="line_699"></a># These functions give implicit equations for the relevant locus.
<a name="line_700"></a>
<a name="line_701"></a><span style="color:red">#@ Ep0_circle_cos_equation
</span><a name="line_702"></a>Ep0_circle_cos_equation := (u) -> 
<a name="line_703"></a> 0.331208-0.0885156*u[1]^3-0.367140*u[1]^2*u[2]-0.269277*u[1]*u[2]^2-
<a name="line_704"></a> 0.0788400*u[2]^3-0.165916*u[1]^2-0.715646*u[1]*u[2]-0.338685*u[2]^2-
<a name="line_705"></a> 0.116555*u[1]+0.0312848*u[2];
<a name="line_706"></a>
<a name="line_707"></a><span style="color:red">#@ Ep0_circle_equation
</span><a name="line_708"></a>Ep0_circle_equation := (z) ->
<a name="line_709"></a> Ep0_circle_cos_equation([cos(Re(z)/latt_a0*2*Pi),cos(Im(z)/abs(latt_b0)*2*Pi)]);
<a name="line_710"></a>
  </pre>
 </body>
</html>
    