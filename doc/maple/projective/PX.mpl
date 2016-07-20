<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>assume(a_P > 0 and a_P < 1);
<a name="line_2"></a>
<a name="line_3"></a>######################################################################
<a name="line_4"></a>
<a name="line_5"></a># These are b_- and b_+ in the LaTeX document
<a name="line_6"></a>bp_P := (1/a_P+a_P)/2; <span style="color:red">#@ bp_P 
</span><a name="line_7"></a>bm_P := (1/a_P-a_P)/2; <span style="color:red">#@ bm_P 
</span><a name="line_8"></a>
<a name="line_9"></a>A_P := a_P^2 + a_P^(-2); <span style="color:red">#@ A_P 
</span><a name="line_10"></a>
<a name="line_11"></a>r_P := (z) -> z^5 - A_P*z^3 + z; <span style="color:red">#@ r_P 
</span><a name="line_12"></a>
<a name="line_13"></a># These are such that
<a name="line_14"></a>#  r_P_cofactor0 * r_P(z) + r_P_cofactor1 * diff(r_P(z),z) = 1
<a name="line_15"></a>
<a name="line_16"></a><span style="color:red">#@ r_P_cofactor0 
</span><a name="line_17"></a>r_P_cofactor0 := (z) -> ((100-30*A_P^2)*z^3+(18*A_P^3-70*A_P)*z)/(4*(A_P^2-4)); 
<a name="line_18"></a>
<a name="line_19"></a><span style="color:red">#@ r_P_cofactor1
</span><a name="line_20"></a>r_P_cofactor1 := (z) -> ((6*A_P^2-20)*z^4-(6*A_P^3-22*A_P)*z^2+(4*A_P^2-16))/(4*(A_P^2-4));
<a name="line_21"></a>
<a name="line_22"></a># These relations define PX(a) in CP^4
<a name="line_23"></a><span style="color:red">#@ P_rel
</span><a name="line_24"></a>P_rel[0] := (z) -> z[1]^2 - (z[2]*z[3] + z[4]*z[5]) + A_P*z[3]*z[4];
<a name="line_25"></a>P_rel[1] := (z) -> z[2]*z[4] - z[3]^2;
<a name="line_26"></a>P_rel[2] := (z) -> z[2]*z[5] - z[3]*z[4];
<a name="line_27"></a>P_rel[3] := (z) -> z[3]*z[5] - z[4]^2;
<a name="line_28"></a>
<a name="line_29"></a># This sets up a Grobner basis for the above relations
<a name="line_30"></a>P_vars := tdeg(z[1],z[2],z[3],z[4],z[5]);                     <span style="color:red">#@ P_vars 
</span><a name="line_31"></a>P_rels := Basis({seq(P_rel[i]([op(P_vars)]),i=0..3)},P_vars); <span style="color:red">#@ P_rels 
</span><a name="line_32"></a>NF_P := (u) -> NormalForm(u,P_rels,P_vars);                   <span style="color:red">#@ NF_P 
</span><a name="line_33"></a>
<a name="line_34"></a># This is the inclusion of the affine curve PX_0(a) in PX(a)
<a name="line_35"></a>j_P  := wz   -> [wz[1],1,wz[2],wz[2]^2,wz[2]^3]: <span style="color:red">#@ j_P  
</span><a name="line_36"></a>
<a name="line_37"></a># This is inverse to j_P
<a name="line_38"></a>j_inv_P := (x) -> [x[1]/x[2],x[3]/x[2]]: <span style="color:red">#@ j_inv_P 
</span><a name="line_39"></a>
<a name="line_40"></a><span style="color:red">#@ jj_P 
</span><a name="line_41"></a>jj_P := proc(uvw) 
<a name="line_42"></a> local u,v,w;
<a name="line_43"></a> u,v,w := op(uvw);
<a name="line_44"></a> return([u,v^3,v^2*w,v*w^2,w^3]);
<a name="line_45"></a>end:
<a name="line_46"></a>
<a name="line_47"></a># A metric on CP^4
<a name="line_48"></a><span style="color:red">#@ d_CP4
</span><a name="line_49"></a>d_CP4 := (u,v) -> sqrt(add(add(abs(u[i]*v[j]-u[j]*v[i])^2,j=i+1..5),i=1..4)/
<a name="line_50"></a>                       (add(abs(u[i])^2,i=1..5)*add(abs(v[i])^2,i=1..5)));
<a name="line_51"></a>
<a name="line_52"></a><span style="color:red">#@ d_P_0
</span><a name="line_53"></a>d_P_0 := (u,v) -> d_CP4(j_P(u),j_P(v));
<a name="line_54"></a>
<a name="line_55"></a># Two projections from PX(a) to C u {infinity}
<a name="line_56"></a>p_P  := (z) -> z[3]/z[2]; <span style="color:red">#@ p_P  
</span><a name="line_57"></a>q_P  := (z) -> z[1]/z[2]; <span style="color:red">#@ q_P  
</span><a name="line_58"></a>
<a name="line_59"></a># pq_P is the same as j_inv_P
<a name="line_60"></a>pq_P := (z) -> [z[1]/z[2],z[3]/z[2]]; <span style="color:red">#@ pq_P 
</span><a name="line_61"></a>
<a name="line_62"></a># This function simplifies algebraic expressions by manipulating
<a name="line_63"></a># some square roots in a way that is valid for all a_P in (0,1),
<a name="line_64"></a># but which is not done automatically by Maple.
<a name="line_65"></a>
<a name="line_66"></a><span style="color:red">#@ simplify_P
</span><a name="line_67"></a>simplify_P := proc(u)
<a name="line_68"></a> local v;
<a name="line_69"></a> v := simplify(u);
<a name="line_70"></a> v := simplify(subs(sqrt(1 - a_P^2) = sqrt(1 - a_P)*sqrt(1 + a_P),v));
<a name="line_71"></a> v := simplify(subs(sqrt((I*a_P+1)/(I*a_P-1)) = -I*sqrt(I-a_P)/sqrt(I+a_P),v));
<a name="line_72"></a> return v;
<a name="line_73"></a>end:
<a name="line_74"></a>
<a name="line_75"></a><span style="color:red">#@ is_member_P_0
</span><a name="line_76"></a>is_member_P_0 := (wz) -> simplify_P(wz[1]^2 - r_P(wz[2])) = 0;
<a name="line_77"></a>
<a name="line_78"></a><span style="color:red">#@ is_member_P
</span><a name="line_79"></a>is_member_P := (z) -> simplify_P([seq(P_rel[i](z),i=0..3)]) = [0$4];
<a name="line_80"></a>
<a name="line_81"></a><span style="color:red">#@ is_equal_P_list
</span><a name="line_82"></a>is_equal_P_list := (z,w) -> [seq(seq(z[i]*w[j]-z[j]*w[i],j=i+1..5),i=1..4)];
<a name="line_83"></a>
<a name="line_84"></a><span style="color:red">#@ is_equal_P
</span><a name="line_85"></a>is_equal_P := proc(z,w)
<a name="line_86"></a> local L,u;
<a name="line_87"></a>
<a name="line_88"></a> L := is_equal_P_list(z,w);
<a name="line_89"></a> for u in L do 
<a name="line_90"></a>  if simplify_P(conjugate(simplify_P(expand(u)))) <> 0 then
<a name="line_91"></a>   return(false);
<a name="line_92"></a>  fi;
<a name="line_93"></a> od;
<a name="line_94"></a> return(true);
<a name="line_95"></a>end:
<a name="line_96"></a>
<a name="line_97"></a>######################################################################
<a name="line_98"></a>
<a name="line_99"></a><span style="color:red">#@ act_P_0
</span><a name="line_100"></a>act_P_0[1]     := (wz) -> [   wz[1], wz[2]];
<a name="line_101"></a>act_P_0[L]     := (wz) -> [ I*wz[1],-wz[2]];
<a name="line_102"></a>act_P_0[LL]    := (wz) -> [-  wz[1], wz[2]];
<a name="line_103"></a>act_P_0[LLL]   := (wz) -> [-I*wz[1],-wz[2]];
<a name="line_104"></a>act_P_0[M]     := (wz) -> [-  wz[1]/wz[2]^3, 1/wz[2]];
<a name="line_105"></a>act_P_0[LM]    := (wz) -> [-I*wz[1]/wz[2]^3,-1/wz[2]];
<a name="line_106"></a>act_P_0[LLM]   := (wz) -> [   wz[1]/wz[2]^3, 1/wz[2]];
<a name="line_107"></a>act_P_0[LLLM]  := (wz) -> [ I*wz[1]/wz[2]^3,-1/wz[2]];
<a name="line_108"></a>act_P_0[N]     := (wz) -> map(conjugate,[   wz[1], wz[2]]);
<a name="line_109"></a>act_P_0[LN]    := (wz) -> map(conjugate,[-I*wz[1],-wz[2]]);
<a name="line_110"></a>act_P_0[LLN]   := (wz) -> map(conjugate,[-  wz[1], wz[2]]);
<a name="line_111"></a>act_P_0[LLLN]  := (wz) -> map(conjugate,[ I*wz[1],-wz[2]]);
<a name="line_112"></a>act_P_0[MN]    := (wz) -> map(conjugate,[-  wz[1]/wz[2]^3, 1/wz[2]]);
<a name="line_113"></a>act_P_0[LMN]   := (wz) -> map(conjugate,[-I*wz[1]/wz[2]^3,-1/wz[2]]);
<a name="line_114"></a>act_P_0[LLMN]  := (wz) -> map(conjugate,[   wz[1]/wz[2]^3, 1/wz[2]]);
<a name="line_115"></a>act_P_0[LLLMN] := (wz) -> map(conjugate,[ I*wz[1]/wz[2]^3,-1/wz[2]]);
<a name="line_116"></a>
<a name="line_117"></a><span style="color:red">#@ act_P
</span><a name="line_118"></a>act_P[1]     := (z) -> [   z[1],z[2], z[3],z[4], z[5]];
<a name="line_119"></a>act_P[L]     := (z) -> [ I*z[1],z[2],-z[3],z[4],-z[5]];
<a name="line_120"></a>act_P[LL]    := (z) -> [-  z[1],z[2], z[3],z[4], z[5]];
<a name="line_121"></a>act_P[LLL]   := (z) -> [-I*z[1],z[2],-z[3],z[4],-z[5]];
<a name="line_122"></a>act_P[M]     := (z) -> [-  z[1],z[5], z[4],z[3], z[2]];
<a name="line_123"></a>act_P[LM]    := (z) -> [-I*z[1],z[5],-z[4],z[3],-z[2]];
<a name="line_124"></a>act_P[LLM]   := (z) -> [   z[1],z[5], z[4],z[3], z[2]];
<a name="line_125"></a>act_P[LLLM]  := (z) -> [ I*z[1],z[5],-z[4],z[3],-z[2]];
<a name="line_126"></a>act_P[N]     := (z) -> map(conjugate,[   z[1],z[2], z[3],z[4], z[5]]);
<a name="line_127"></a>act_P[LN]    := (z) -> map(conjugate,[-I*z[1],z[2],-z[3],z[4],-z[5]]);
<a name="line_128"></a>act_P[LLN]   := (z) -> map(conjugate,[-  z[1],z[2], z[3],z[4], z[5]]);
<a name="line_129"></a>act_P[LLLN]  := (z) -> map(conjugate,[ I*z[1],z[2],-z[3],z[4],-z[5]]);
<a name="line_130"></a>act_P[MN]    := (z) -> map(conjugate,[-  z[1],z[5], z[4],z[3], z[2]]);
<a name="line_131"></a>act_P[LMN]   := (z) -> map(conjugate,[ I*z[1],z[5],-z[4],z[3],-z[2]]);
<a name="line_132"></a>act_P[LLMN]  := (z) -> map(conjugate,[   z[1],z[5], z[4],z[3], z[2]]);
<a name="line_133"></a>act_P[LLLMN] := (z) -> map(conjugate,[-I*z[1],z[5],-z[4],z[3],-z[2]]);
<a name="line_134"></a>
<a name="line_135"></a>######################################################################
<a name="line_136"></a># The map p_P is equivariant with respect to the following action
<a name="line_137"></a># of G on C u {infinity}
<a name="line_138"></a>
<a name="line_139"></a><span style="color:red">#@ act_C
</span><a name="line_140"></a>act_C[1]    := (z) ->  z;
<a name="line_141"></a>act_C[L]    := (z) -> -z;
<a name="line_142"></a>act_C[LL]   := (z) ->  z;
<a name="line_143"></a>act_C[LLL]  := (z) -> -z;
<a name="line_144"></a>act_C[M]    := (z) -> `if`(z=0,infinity, 1/z);
<a name="line_145"></a>act_C[LM]   := (z) -> `if`(z=0,infinity,-1/z);
<a name="line_146"></a>act_C[LLM]  := (z) -> `if`(z=0,infinity, 1/z);
<a name="line_147"></a>act_C[LLLM] := (z) -> `if`(z=0,infinity,-1/z);
<a name="line_148"></a>act_C[N]    := (z) ->  conjugate(z);
<a name="line_149"></a>act_C[LN]   := (z) -> -conjugate(z);
<a name="line_150"></a>act_C[LLN]  := (z) ->  conjugate(z);
<a name="line_151"></a>act_C[LLLN] := (z) -> -conjugate(z);
<a name="line_152"></a>act_C[MN]   := (z) -> `if`(z=0,infinity, 1/conjugate(z));
<a name="line_153"></a>act_C[LMN]  := (z) -> `if`(z=0,infinity,-1/conjugate(z));
<a name="line_154"></a>act_C[LLMN] := (z) -> `if`(z=0,infinity, 1/conjugate(z));
<a name="line_155"></a>act_C[LLLMN]:= (z) -> `if`(z=0,infinity,-1/conjugate(z));
<a name="line_156"></a>
<a name="line_157"></a># If we identify C u {infinity} with S^2 by the map C_to_S2,
<a name="line_158"></a># we get the following action.
<a name="line_159"></a>
<a name="line_160"></a><span style="color:red">#@ act_S2
</span><a name="line_161"></a>act_S2[1]    := (u) -> [ u[1], u[2], u[3]];
<a name="line_162"></a>act_S2[L]    := (u) -> [-u[1],-u[2], u[3]];
<a name="line_163"></a>act_S2[LL]   := (u) -> [ u[1], u[2], u[3]];
<a name="line_164"></a>act_S2[LLL]  := (u) -> [-u[1],-u[2], u[3]];
<a name="line_165"></a>act_S2[M]    := (u) -> [ u[1],-u[2],-u[3]];
<a name="line_166"></a>act_S2[LM]   := (u) -> [-u[1], u[2],-u[3]];
<a name="line_167"></a>act_S2[LLM]  := (u) -> [ u[1],-u[2],-u[3]];
<a name="line_168"></a>act_S2[LLLM] := (u) -> [-u[1], u[2],-u[3]];
<a name="line_169"></a>act_S2[N]    := (u) -> [ u[1],-u[2], u[3]];
<a name="line_170"></a>act_S2[LN]   := (u) -> [-u[1], u[2], u[3]];
<a name="line_171"></a>act_S2[LLN]  := (u) -> [ u[1],-u[2], u[3]];
<a name="line_172"></a>act_S2[LLLN] := (u) -> [-u[1], u[2], u[3]];
<a name="line_173"></a>act_S2[MN]   := (u) -> [ u[1], u[2],-u[3]];
<a name="line_174"></a>act_S2[LMN]  := (u) -> [-u[1],-u[2],-u[3]];
<a name="line_175"></a>act_S2[LLMN] := (u) -> [ u[1], u[2],-u[3]];
<a name="line_176"></a>act_S2[LLLMN]:= (u) -> [-u[1],-u[2],-u[3]];
<a name="line_177"></a>
<a name="line_178"></a>######################################################################
<a name="line_179"></a>
<a name="line_180"></a><span style="color:red">#@ v_P_0
</span><a name="line_181"></a>v_P_0[ 0] := [0,0]:
<a name="line_182"></a>v_P_0[ 1] := [infinity,infinity]:
<a name="line_183"></a>v_P_0[ 2] := [-  (a_P^(-1)-a_P),-1]:
<a name="line_184"></a>v_P_0[ 3] := [-I*(a_P^(-1)-a_P), 1]:
<a name="line_185"></a>v_P_0[ 4] := [   (a_P^(-1)-a_P),-1]:
<a name="line_186"></a>v_P_0[ 5] := [ I*(a_P^(-1)-a_P), 1]:
<a name="line_187"></a>v_P_0[ 6] := [ (1+I)*(a_P^(-1)+a_P)/sqrt(2), I]:
<a name="line_188"></a>v_P_0[ 7] := [-(1-I)*(a_P^(-1)+a_P)/sqrt(2),-I]:
<a name="line_189"></a>v_P_0[ 8] := [-(1+I)*(a_P^(-1)+a_P)/sqrt(2), I]:
<a name="line_190"></a>v_P_0[ 9] := [ (1-I)*(a_P^(-1)+a_P)/sqrt(2),-I]:
<a name="line_191"></a>v_P_0[10] := [0,  -a_P]:
<a name="line_192"></a>v_P_0[11] := [0,   a_P]:
<a name="line_193"></a>v_P_0[12] := [0,-1/a_P]:
<a name="line_194"></a>v_P_0[13] := [0, 1/a_P]:
<a name="line_195"></a>
<a name="line_196"></a><span style="color:red">#@ v_P
</span><a name="line_197"></a>v_P[ 0] := [0,1,0,0,0]:
<a name="line_198"></a>v_P[ 1] := [0,0,0,0,1]:
<a name="line_199"></a>for i from 2 to 13 do v_P[i] := j_P(v_P_0[i]); od:
<a name="line_200"></a>
<a name="line_201"></a>######################################################################
<a name="line_202"></a>
<a name="line_203"></a><span style="color:red">#@ c_P_jj
</span><a name="line_204"></a>c_P_jj[ 0] := (t) -> [
<a name="line_205"></a> -sqrt((1/a_P-a_P)^2 + 4*sin(2*t)^2),
<a name="line_206"></a>  exp(I*t),
<a name="line_207"></a> -exp(-I*t)
<a name="line_208"></a>];
<a name="line_209"></a>
<a name="line_210"></a>c_P_jj[ 1] := (t) -> [
<a name="line_211"></a> (1+I)/sqrt(2)*sin(t)*sqrt(16*cos(t)^2+(a_P^(-1)+a_P)^2*sin(t)^4)/8,
<a name="line_212"></a>   (1+cos(t))/2,
<a name="line_213"></a> I*(1-cos(t))/2
<a name="line_214"></a>];
<a name="line_215"></a>
<a name="line_216"></a>c_P_jj[ 3] := (t) -> [
<a name="line_217"></a>(-I*(a_P^(-1)-a_P)*sin(t)*sqrt((1+a_P)^4-(1-a_P)^4*cos(t)^2)*sqrt((1+a_P)^2-(1-a_P)^2*cos(t)^2))/8,
<a name="line_218"></a>((1+a_P)+(1-a_P)*cos(t))/2,
<a name="line_219"></a>((1+a_P)-(1-a_P)*cos(t))/2
<a name="line_220"></a>];
<a name="line_221"></a>
<a name="line_222"></a>c_P_jj[5] := (t) -> [
<a name="line_223"></a> sin(t)*sqrt(a_P*(3-cos(t))*(4-a_P^4*(1-cos(t))^2)/32),
<a name="line_224"></a> 1,
<a name="line_225"></a> a_P * (1 - cos(t))/2
<a name="line_226"></a>];
<a name="line_227"></a>
<a name="line_228"></a><span style="color:red">#@ c_P
</span><a name="line_229"></a>c_P[ 0] := unapply(jj_P(c_P_jj[0](t)),t);
<a name="line_230"></a>c_P[ 1] := unapply(jj_P(c_P_jj[1](t)),t);
<a name="line_231"></a>c_P[ 2] := unapply(act_P[L](c_P[1](t)),t);
<a name="line_232"></a>c_P[ 3] := unapply(jj_P(c_P_jj[3](t)),t);
<a name="line_233"></a>c_P[ 4] := unapply(act_P[L](c_P[3](t)),t);
<a name="line_234"></a>c_P[ 5] := unapply(jj_P(c_P_jj[5](t)),t);
<a name="line_235"></a>c_P[ 6] := unapply(act_P[ L](c_P[5](t)),t);
<a name="line_236"></a>c_P[ 7] := unapply(act_P[ M](c_P[5](t)),t);
<a name="line_237"></a>c_P[ 8] := unapply(act_P[LM](c_P[5](t)),t);
<a name="line_238"></a>
<a name="line_239"></a><span style="color:red">#@ pc_P
</span><a name="line_240"></a>pc_P[ 0] := (t) -> -exp(-2*I*t);
<a name="line_241"></a>pc_P[ 1] := (t) ->  I*(1-cos(t))/(1+cos(t));
<a name="line_242"></a>pc_P[ 2] := (t) -> -I*(1-cos(t))/(1+cos(t));
<a name="line_243"></a>pc_P[ 3] := (t) ->  ((1+a_P)-(1-a_P)*cos(t))/((1+a_P)+(1-a_P)*cos(t));
<a name="line_244"></a>pc_P[ 4] := (t) -> -((1+a_P)-(1-a_P)*cos(t))/((1+a_P)+(1-a_P)*cos(t));
<a name="line_245"></a>pc_P[ 5] := (t) ->  (1-cos(t))/2 * a_P;
<a name="line_246"></a>pc_P[ 6] := (t) -> -(1-cos(t))/2 * a_P;
<a name="line_247"></a>pc_P[ 7] := (t) ->  1/((1-cos(t))/2 * a_P);
<a name="line_248"></a>pc_P[ 8] := (t) -> -1/((1-cos(t))/2 * a_P);
<a name="line_249"></a>
<a name="line_250"></a><span style="color:red">#@ pc_P_lim
</span><a name="line_251"></a>pc_P_lim[ 1] := [0,infinity * I];
<a name="line_252"></a>pc_P_lim[ 2] := [-infinity * I,0];
<a name="line_253"></a>pc_P_lim[ 3] := [   a_P, 1/a_P];
<a name="line_254"></a>pc_P_lim[ 4] := [-1/a_P,  -a_P];
<a name="line_255"></a>pc_P_lim[ 5] := [     0,   a_P];
<a name="line_256"></a>pc_P_lim[ 6] := [  -a_P,     0];
<a name="line_257"></a>pc_P_lim[ 7] := [ 1/a_P, infinity];
<a name="line_258"></a>pc_P_lim[ 8] := [-infinity,-1/a_P];
<a name="line_259"></a>
<a name="line_260"></a>######################################################################
<a name="line_261"></a># The function c_check_P[k](z) returns true if z lies in C[k]
<a name="line_262"></a># (This assumes that we already know that z lies in PX(a))
<a name="line_263"></a>
<a name="line_264"></a><span style="color:red">#@ c_check_P
</span><a name="line_265"></a>
<a name="line_266"></a>c_check_P[0] := (z) ->
<a name="line_267"></a> simplify(z[2]*conjugate(z[2]) - z[5]*conjugate(z[5])) = 0;
<a name="line_268"></a>
<a name="line_269"></a>c_check_P[1] := (z) -> is(simplify( I*z[2]*conjugate(z[3])) >= 0);
<a name="line_270"></a>
<a name="line_271"></a>c_check_P[2] := (z) -> is(simplify(-I*z[2]*conjugate(z[3])) >= 0);
<a name="line_272"></a>
<a name="line_273"></a> c_check_P[3] := (z) -> is(simplify((  z[3] - a_P*z[2])*conjugate(z[2])) >= 0) and 
<a name="line_274"></a>                       is(simplify((  z[4] - a_P*z[5])*conjugate(z[5])) >= 0);
<a name="line_275"></a>
<a name="line_276"></a>c_check_P[4] := (z) -> is(simplify((- z[3] - a_P*z[2])*conjugate(z[2])) >= 0) and 
<a name="line_277"></a>                       is(simplify((- z[4] - a_P*z[5])*conjugate(z[5])) >= 0);
<a name="line_278"></a>
<a name="line_279"></a>c_check_P[5] := (z) -> is(simplify((- z[3] + a_P*z[2])*conjugate(z[2])) >= 0) and
<a name="line_280"></a>                       is(simplify((- z[5] + a_P*z[4])*conjugate(z[5])) >= 0);
<a name="line_281"></a>
<a name="line_282"></a>c_check_P[6] := (z) -> is(simplify((  z[3] + a_P*z[2])*conjugate(z[2])) >= 0) and
<a name="line_283"></a>                       is(simplify((- z[5] - a_P*z[4])*conjugate(z[5])) >= 0);
<a name="line_284"></a>
<a name="line_285"></a>c_check_P[7] := (z) -> is(simplify((- z[2] + a_P*z[3])*conjugate(z[2])) >= 0) and 
<a name="line_286"></a>                       is(simplify((- z[4] + a_P*z[5])*conjugate(z[5])) >= 0);
<a name="line_287"></a>
<a name="line_288"></a>c_check_P[8] := (z) -> is(simplify((- z[2] - a_P*z[3])*conjugate(z[2])) >= 0) and
<a name="line_289"></a>                       is(simplify((  z[4] + a_P*z[5])*conjugate(z[5])) >= 0);
<a name="line_290"></a>
<a name="line_291"></a>######################################################################
<a name="line_292"></a>
<a name="line_293"></a><span style="color:red">#@ is_in_F4_P
</span><a name="line_294"></a>is_in_F4_P := proc(z)
<a name="line_295"></a> local u,v,w,err;
<a name="line_296"></a> if not is(simplify(Im(z[3]*conjugate(z[2]))) >= 0) then return false; fi;
<a name="line_297"></a> if simplify(z[2]) = 0 then return true; fi;
<a name="line_298"></a> u := simplify(z[3]/z[2]);
<a name="line_299"></a> v := simplify(z[1]/z[2]);
<a name="line_300"></a> w := sqrt(u)*sqrt(u-a_P)*sqrt(u+a_P)*sqrt(u-1/a_P)*sqrt(u+1/a_P);
<a name="line_301"></a> err := simplify_P(v + w);
<a name="line_302"></a>
<a name="line_303"></a> return evalb(err = 0);
<a name="line_304"></a>end:
<a name="line_305"></a>
<a name="line_306"></a><span style="color:red">#@ is_in_F16_P
</span><a name="line_307"></a>is_in_F16_P := proc(z)
<a name="line_308"></a> is(simplify(z[2]*conjugate(z[2]) - z[5]*conjugate(z[5])) >= 0) and
<a name="line_309"></a> is(simplify(Re(z[3]*conjugate(z[2]))) >= 0) and
<a name="line_310"></a> is(simplify(Im(z[3]*conjugate(z[2]))) >= 0) and
<a name="line_311"></a> is(simplify(Re(z[1]*conjugate(z[2]))) >= 0) and
<a name="line_312"></a> is(simplify(Re(z[1]*conjugate(z[2])) - Im(z[1]*conjugate(z[2]))) >= 0);
<a name="line_313"></a>end:
<a name="line_314"></a>
  </pre>
 </body>
</html>
    