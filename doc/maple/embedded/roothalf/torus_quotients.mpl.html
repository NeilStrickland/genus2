<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># Here we aim to give a supply of maps EX^* -> S^1 with interesting properties.
<a name="line_2"></a># The most efficient way to encode such a map is to give a quotient of
<a name="line_3"></a># polynomials, which determines a map from an open subset of EX^* to R.
<a name="line_4"></a># In good cases this will extend to give a continuous function from all of
<a name="line_5"></a># EX^* to R u {infinity}, and then we can identify R u {infinity} with
<a name="line_6"></a># S^1 by the map t |-> (1-t^2,2t)/(1+t^2), with inverse (u,v) |-> v/(1+u).
<a name="line_7"></a># However, it is often delicate to ensure that such an extension exists.
<a name="line_8"></a># 
<a name="line_9"></a># Note that the above identification relates multiplication on S^1 to the
<a name="line_10"></a># operation (s,t) |-> (s+t)/(1-st) = tan_sum(s,t) on R u {infinity}.
<a name="line_11"></a>
<a name="line_12"></a># We use several different models for the circle and the torus, indicated
<a name="line_13"></a># by the notation T, TA, TC and TP.  These are explained in the file 
<a name="line_14"></a># Rn.mpl, which defines functions to convert between the models.
<a name="line_15"></a>
<a name="line_16"></a><span style="color:red">#@ TTC_to_TT 
</span><a name="line_17"></a>TTC_to_TT := (z) -> [
<a name="line_18"></a> Re(z[1]),Im(z[1]),Re(z[2]),Im(z[2]),Re(z[3]),Im(z[3]),Re(z[4]),Im(z[4])
<a name="line_19"></a>];
<a name="line_20"></a>
<a name="line_21"></a><span style="color:red">#@ TT_to_TTC 
</span><a name="line_22"></a>TT_to_TTC := (u) -> [seq(u[2*i-1]+I*u[2*i],i=1..4)];
<a name="line_23"></a>
<a name="line_24"></a>######################################################################
<a name="line_25"></a>######################################################################
<a name="line_26"></a># A map to the 4-torus
<a name="line_27"></a>
<a name="line_28"></a><span style="color:red">#@ E_to_TTC_xyr 
</span><a name="line_29"></a>E_to_TTC_xyr := [
<a name="line_30"></a> ( y[1]*(1-y[2]/sqrt(2))-1/sqrt(2)+I*x[1])/((1-y[1]/sqrt(2))*r[1]),
<a name="line_31"></a> ( y[1]*(1+y[2]/sqrt(2))-1/sqrt(2)+I*x[2])/((1-y[1]/sqrt(2))*r[2]),
<a name="line_32"></a> (-y[1]*(1-y[2]/sqrt(2))-1/sqrt(2)+I*x[1])/((1+y[1]/sqrt(2))*r[1]),
<a name="line_33"></a> (-y[1]*(1+y[2]/sqrt(2))-1/sqrt(2)+I*x[2])/((1+y[1]/sqrt(2))*r[2])
<a name="line_34"></a>];
<a name="line_35"></a>
<a name="line_36"></a><span style="color:red">#@ E_to_TTP_xyr 
</span><a name="line_37"></a>E_to_TTP_xyr :=  [
<a name="line_38"></a> (1/sqrt(2)+r[1])*(1-r[1]*y[1])/x[1],
<a name="line_39"></a> (1/sqrt(2)+r[2])*(1-r[2]*y[1])/x[2],
<a name="line_40"></a> (1/sqrt(2)+r[1])*(1+r[1]*y[1])/x[1],
<a name="line_41"></a> (1/sqrt(2)+r[2])*(1+r[2]*y[1])/x[2]
<a name="line_42"></a>];
<a name="line_43"></a>
<a name="line_44"></a><span style="color:red">#@ E_to_TT_xyr  
</span><a name="line_45"></a>E_to_TT_xyr  :=
<a name="line_46"></a> simplify(TTC_to_TT(E_to_TTC_xyr)) assuming r[1]>0 and r[2]>0;
<a name="line_47"></a> 
<a name="line_48"></a><span style="color:red">#@ E_to_TTC 
</span><a name="line_49"></a>E_to_TTC := unapply(eval(subs({y=yx0,r=rx},E_to_TTC_xyr)),x);
<a name="line_50"></a>
<a name="line_51"></a><span style="color:red">#@ E_to_TTP 
</span><a name="line_52"></a>E_to_TTP := unapply(eval(subs({y=yx0,r=rx},E_to_TTP_xyr)),x);
<a name="line_53"></a>
<a name="line_54"></a><span style="color:red">#@ E_to_TT  
</span><a name="line_55"></a>E_to_TT  := unapply(eval(subs({y=yx0,r=rx},E_to_TT_xyr)),x);
<a name="line_56"></a>
<a name="line_57"></a># A left inverse for E_to_TT
<a name="line_58"></a>
<a name="line_59"></a><span style="color:red">#@ TT_to_E 
</span><a name="line_60"></a>TT_to_E := proc(u)
<a name="line_61"></a> local A,R,S,X,Y,Z;
<a name="line_62"></a>
<a name="line_63"></a> A[ 1] := simplify(factor((2-u[1]*u[5]-u[2]*u[6]-u[3]*u[7]-u[4]*u[8])/4));
<a name="line_64"></a> Z[ 1] := simplify(factor(2*A[1]/(1+A[1])));
<a name="line_65"></a> A[ 2] := simplify(factor((u[1]^2+u[3]^2+u[5]^2+u[7]^2)/4));
<a name="line_66"></a> A[ 3] := simplify(factor((A[2]+2*A[1])/(1+A[1])/(1+2*A[1])));
<a name="line_67"></a> Z[ 2] := simplify(factor(2 - 1/A[3]));
<a name="line_68"></a> S[ 1] := simplify(factor(sqrt(1 - Z[2]/2)));   # r[1] * r[2]
<a name="line_69"></a> S[ 2] := simplify(factor(sqrt(2*(1 + S[1])))); # r[1] + r[2]
<a name="line_70"></a> A[ 4] := simplify(factor(1 - Z[2] + S[1]));
<a name="line_71"></a> A[ 5] := simplify(factor((u[1]+u[3]-u[5]-u[7])*S[2]*S[1]/(1+A[1])/A[4]/2));
<a name="line_72"></a> Y[ 1] := A[5];
<a name="line_73"></a> A[ 6] := simplify(factor((u[3]*u[5]-u[1]*u[7])*S[1]*(1-Z[1]/2)/2));
<a name="line_74"></a> A[ 7] := simplify(factor((1-Z[2]/2)*((u[1]^2-u[3]^2)*(1-Y[1]/sqrt(2))^2+
<a name="line_75"></a>                                      (u[5]^2-u[7]^2)*(1+Y[1]/sqrt(2))^2)));
<a name="line_76"></a> A[ 8] := simplify(factor(((u[1]*u[5]+u[2]*u[6]-u[3]*u[7]-u[4]*u[8])/(1+A[1]))));
<a name="line_77"></a> A[ 9] := simplify(factor((A[7]+A[8])/(1+Z[1]*Z[2])/sqrt(2)));
<a name="line_78"></a> Y[ 2] := A[9];
<a name="line_79"></a> R[ 1] := simplify(factor(sqrt(1 - Y[2]/sqrt(2))));
<a name="line_80"></a> R[ 2] := simplify(factor(sqrt(1 + Y[2]/sqrt(2))));
<a name="line_81"></a> A[10] := simplify(factor(u[2]*R[1]*(1-Y[1]/sqrt(2))));
<a name="line_82"></a> A[11] := simplify(factor(u[4]*R[2]*(1-Y[1]/sqrt(2))));
<a name="line_83"></a> X[ 1] := A[10];
<a name="line_84"></a> X[ 2] := A[11];
<a name="line_85"></a> X[ 3] := Y[1];
<a name="line_86"></a> X[ 4] := simplify(-Y[1]*Y[2]); 
<a name="line_87"></a> return [X[1],X[2],X[3],X[4]];
<a name="line_88"></a>end:
<a name="line_89"></a>
<a name="line_90"></a><span style="color:red">#@ TTC_to_E 
</span><a name="line_91"></a>TTC_to_E := (z) -> TT_to_E(TTC_to_TT(z));
<a name="line_92"></a>
<a name="line_93"></a><span style="color:red">#@ TT_to_E_generic 
</span><a name="line_94"></a>TT_to_E_generic := proc(u)
<a name="line_95"></a> local X,a,b;
<a name="line_96"></a>
<a name="line_97"></a> X[1] := -sqrt(2)*u[2]*u[6]/(u[1]*u[6]+u[2]*u[5]);
<a name="line_98"></a> X[2] := -sqrt(2)*u[4]*u[8]/(u[3]*u[8]+u[4]*u[7]);
<a name="line_99"></a> X[3] :=  sqrt(2)*(u[2]-u[6])/(u[2]+u[6]);
<a name="line_100"></a> a := ((u[1]-u[5])*(1-u[3]*u[7]-u[4]*u[8]))^2;
<a name="line_101"></a> b := ((u[3]-u[7])*(1-u[1]*u[5]-u[2]*u[6]))^2;
<a name="line_102"></a> X[4] := -sqrt(2)*X[3]*(a-b)/(a+b);
<a name="line_103"></a> return [X[1],X[2],X[3],X[4]];
<a name="line_104"></a>end:
<a name="line_105"></a>
<a name="line_106"></a><span style="color:red">#@ TTC_to_E_generic 
</span><a name="line_107"></a>TTC_to_E_generic := (z) -> TT_to_E_generic(TTC_to_TT(z));
<a name="line_108"></a>
<a name="line_109"></a><span style="color:red">#@ TTP_to_E_generic 
</span><a name="line_110"></a>TTP_to_E_generic := (t) -> [
<a name="line_111"></a> 2*sqrt(2)*t[3]*t[1]/((t[1]*t[3]-1)*(t[1]+t[3])),
<a name="line_112"></a> 2*sqrt(2)*t[4]*t[2]/((t[2]*t[4]-1)*(t[2]+t[4])),
<a name="line_113"></a>  -sqrt(2)*(t[1]-t[3])*(t[1]*t[3]-1)/((t[1]+t[3])*(t[1]*t[3]+1)),
<a name="line_114"></a>  -(4*t[1]-4*t[3])*(t[1]*t[3]-1)*(t[1]*t[4]-t[2]*t[3])*(t[1]*t[2]-t[3]*t[4])/
<a name="line_115"></a>    ((t[1]+t[3])*(t[1]*t[3]+1)*(t[1]^2*t[2]^2+t[1]^2*t[4]^2-4*t[1]*t[2]*t[3]*t[4]+t[2]^2*t[3]^2+t[3]^2*t[4]^2))
<a name="line_116"></a>]:
<a name="line_117"></a>
<a name="line_118"></a><span style="color:red">#@ act_TTP
</span><a name="line_119"></a>act_TTP[1]    := (t) -> [ t[1], t[2], t[3], t[4]];
<a name="line_120"></a>act_TTP[L]    := (t) -> [-t[2], t[1],-t[4], t[3]];
<a name="line_121"></a>act_TTP[LL]   := (t) -> [-t[1],-t[2],-t[3],-t[4]];
<a name="line_122"></a>act_TTP[LLL]  := (t) -> [ t[2],-t[1], t[4],-t[3]];
<a name="line_123"></a>act_TTP[M]    := (t) -> [ t[3],-t[4], t[1],-t[2]];
<a name="line_124"></a>act_TTP[LM]   := (t) -> [ t[4], t[3], t[2], t[1]];
<a name="line_125"></a>act_TTP[LLM]  := (t) -> [-t[3], t[4],-t[1], t[2]];
<a name="line_126"></a>act_TTP[LLLM] := (t) -> [-t[4],-t[3],-t[2],-t[1]];
<a name="line_127"></a>act_TTP[N]    := (t) -> [ t[1],-t[2], t[3],-t[4]];
<a name="line_128"></a>act_TTP[LN]   := (t) -> [ t[2], t[1], t[4], t[3]];
<a name="line_129"></a>act_TTP[LLN]  := (t) -> [-t[1], t[2],-t[3], t[4]];
<a name="line_130"></a>act_TTP[LLLN] := (t) -> [-t[2],-t[1],-t[4],-t[3]];
<a name="line_131"></a>act_TTP[MN]   := (t) -> [ t[3], t[4], t[1], t[2]];
<a name="line_132"></a>act_TTP[LMN]  := (t) -> [-t[4], t[3],-t[2], t[1]];
<a name="line_133"></a>act_TTP[LLMN] := (t) -> [-t[3],-t[4],-t[1],-t[2]];
<a name="line_134"></a>act_TTP[LLLMN]:= (t) -> [ t[4],-t[3], t[2],-t[1]];
<a name="line_135"></a>
<a name="line_136"></a><span style="color:red">#@ act_TTC
</span><a name="line_137"></a>act_TTC[1]    := (z) -> [          z[1] ,          z[2] ,          z[3] ,          z[4]];
<a name="line_138"></a>act_TTC[L]    := (z) -> [conjugate(z[2]),          z[1] ,conjugate(z[4]),          z[3]];
<a name="line_139"></a>act_TTC[LL]   := (z) -> [conjugate(z[1]),conjugate(z[2]),conjugate(z[3]),conjugate(z[4])];
<a name="line_140"></a>act_TTC[LLL]  := (z) -> [          z[2] ,conjugate(z[1]),          z[4] ,conjugate(z[3])];
<a name="line_141"></a>act_TTC[M]    := (z) -> [          z[3] ,conjugate(z[4]),          z[1] ,conjugate(z[2])];
<a name="line_142"></a>act_TTC[LM]   := (z) -> [          z[4] ,          z[3] ,          z[2] ,          z[1]];
<a name="line_143"></a>act_TTC[LLM]  := (z) -> [conjugate(z[3]),          z[4] ,conjugate(z[1]),          z[2]];
<a name="line_144"></a>act_TTC[LLLM] := (z) -> [conjugate(z[4]),conjugate(z[3]),conjugate(z[2]),conjugate(z[1])];
<a name="line_145"></a>act_TTC[N]    := (z) -> [          z[1] ,conjugate(z[2]),          z[3] ,conjugate(z[4])];
<a name="line_146"></a>act_TTC[LN]   := (z) -> [          z[2] ,          z[1] ,          z[4] ,          z[3]];
<a name="line_147"></a>act_TTC[LLN]  := (z) -> [conjugate(z[1]),          z[2] ,conjugate(z[3]),          z[4]];
<a name="line_148"></a>act_TTC[LLLN] := (z) -> [conjugate(z[2]),conjugate(z[1]),conjugate(z[4]),conjugate(z[3])];
<a name="line_149"></a>act_TTC[MN]   := (z) -> [          z[3] ,          z[4] ,          z[1] ,          z[2]];
<a name="line_150"></a>act_TTC[LMN]  := (z) -> [conjugate(z[4]),          z[3] ,conjugate(z[2]),          z[1]];
<a name="line_151"></a>act_TTC[LLMN] := (z) -> [conjugate(z[3]),conjugate(z[4]),conjugate(z[1]),conjugate(z[2])];
<a name="line_152"></a>act_TTC[LLLMN]:= (z) -> [          z[4] ,conjugate(z[3]),          z[2] ,conjugate(z[1])];
<a name="line_153"></a>
<a name="line_154"></a><span style="color:red">#@ act_TT
</span><a name="line_155"></a>act_TT[1]     := (u) -> [u[1],  u[2], u[3],  u[4], u[5],  u[6], u[7],  u[8]];
<a name="line_156"></a>act_TT[L]     := (u) -> [u[3], -u[4], u[1],  u[2], u[7], -u[8], u[5],  u[6]];
<a name="line_157"></a>act_TT[LL]    := (u) -> [u[1], -u[2], u[3], -u[4], u[5], -u[6], u[7], -u[8]];
<a name="line_158"></a>act_TT[LLL]   := (u) -> [u[3],  u[4], u[1], -u[2], u[7],  u[8], u[5], -u[6]];
<a name="line_159"></a>act_TT[M]     := (u) -> [u[5],  u[6], u[7], -u[8], u[1],  u[2], u[3], -u[4]];
<a name="line_160"></a>act_TT[LM]    := (u) -> [u[7],  u[8], u[5],  u[6], u[3],  u[4], u[1],  u[2]];
<a name="line_161"></a>act_TT[LLM]   := (u) -> [u[5], -u[6], u[7],  u[8], u[1], -u[2], u[3],  u[4]];
<a name="line_162"></a>act_TT[LLLM]  := (u) -> [u[7], -u[8], u[5], -u[6], u[3], -u[4], u[1], -u[2]];
<a name="line_163"></a>act_TT[N]     := (u) -> [u[1],  u[2], u[3], -u[4], u[5],  u[6], u[7], -u[8]];
<a name="line_164"></a>act_TT[LN]    := (u) -> [u[3],  u[4], u[1],  u[2], u[7],  u[8], u[5],  u[6]];
<a name="line_165"></a>act_TT[LLN]   := (u) -> [u[1], -u[2], u[3],  u[4], u[5], -u[6], u[7],  u[8]];
<a name="line_166"></a>act_TT[LLLN]  := (u) -> [u[3], -u[4], u[1], -u[2], u[7], -u[8], u[5], -u[6]];
<a name="line_167"></a>act_TT[MN]    := (u) -> [u[5],  u[6], u[7],  u[8], u[1],  u[2], u[3],  u[4]];
<a name="line_168"></a>act_TT[LMN]   := (u) -> [u[7], -u[8], u[5],  u[6], u[3], -u[4], u[1],  u[2]];
<a name="line_169"></a>act_TT[LLMN]  := (u) -> [u[5], -u[6], u[7], -u[8], u[1], -u[2], u[3], -u[4]];
<a name="line_170"></a>act_TT[LLLMN] := (u) -> [u[7],  u[8], u[5], -u[6], u[3],  u[4], u[1], -u[2]];
<a name="line_171"></a>
<a name="line_172"></a># Images of the curves c[k] in the 4-torus
<a name="line_173"></a>
<a name="line_174"></a><span style="color:red">#@ c_TTP
</span><a name="line_175"></a>c_TTP[ 0] := (t) -> [(1/2)*(sqrt(4*cos(t)^2+2)+sqrt(2))/cos(t),
<a name="line_176"></a>                    (1/2)*(sqrt(2+4*sin(t)^2)+sqrt(2))/sin(t),
<a name="line_177"></a>                    (1/2)*(sqrt(4*cos(t)^2+2)+sqrt(2))/cos(t),
<a name="line_178"></a>                    (1/2)*(sqrt(2+4*sin(t)^2)+sqrt(2))/sin(t)];
<a name="line_179"></a>c_TTP[ 1] := (t) -> (1+sqrt(2)) *~
<a name="line_180"></a> [ (1-cos(t))/sin(t),(1-cos(t))/sin(t), (1+cos(t))/sin(t), (1+cos(t))/sin(t)];
<a name="line_181"></a>c_TTP[ 2] := (t) -> (1+sqrt(2)) *~
<a name="line_182"></a> [-(1-cos(t))/sin(t),(1-cos(t))/sin(t),-(1+cos(t))/sin(t), (1+cos(t))/sin(t)];
<a name="line_183"></a>c_TTP[ 3] := (t) -> [infinity,  (1/2)*sqrt(2)*(1+sqrt(3))*sin(t)/(1+cos(t)),
<a name="line_184"></a>                    infinity, -(1/2)*sqrt(2)*(1+sqrt(3))*sin(t)/(cos(t)-1)];
<a name="line_185"></a>c_TTP[ 4] := (t) -> [-(1/2)*sqrt(2)*(1+sqrt(3))*sin(t)/(1+cos(t)), infinity,
<a name="line_186"></a>                     (1/2)*sqrt(2)*(1+sqrt(3))*sin(t)/(cos(t)-1), infinity];
<a name="line_187"></a>c_TTP[ 5] := (t) -> [sqrt(2)/sin(t)*((sqrt(2+sin(t/2)^2)-1)*
<a name="line_188"></a>                                    (sqrt(2-sin(t/2)^2)+1)-cos(t/2)^2),0,
<a name="line_189"></a>                    sqrt(2)/sin(t)*((sqrt(2+sin(t/2)^2)+1)*
<a name="line_190"></a>                                    (sqrt(2-sin(t/2)^2)+1)+cos(t/2)^2),infinity];
<a name="line_191"></a>c_TTP[ 6] := unapply(subs(-infinity = infinity,act_TTP[ L](c_TTP[5](t))),t):
<a name="line_192"></a>c_TTP[ 7] := unapply(subs(-infinity = infinity,act_TTP[ M](c_TTP[5](t))),t):
<a name="line_193"></a>c_TTP[ 8] := unapply(subs(-infinity = infinity,act_TTP[LM](c_TTP[5](t))),t):
<a name="line_194"></a>c_TTP[ 9] := unapply([(sqrt(3)-1)*(sqrt(2-cos(t))+1)-(1-cos(t)),
<a name="line_195"></a>                     (sqrt(3)-1)*(sqrt(2+cos(t))+1)-(1+cos(t)),
<a name="line_196"></a>                     (sqrt(3)+1)*(sqrt(2-cos(t))+1)+(1-cos(t)),
<a name="line_197"></a>                     (sqrt(3)+1)*(sqrt(2+cos(t))+1)+(1+cos(t))] /~ sin(t),t);
<a name="line_198"></a>c_TTP[10] := unapply(act_TTP[ L](c_TTP[9](t)),t):
<a name="line_199"></a>c_TTP[11] := unapply(act_TTP[ M](c_TTP[9](t)),t):
<a name="line_200"></a>c_TTP[12] := unapply(act_TTP[LM](c_TTP[9](t)),t):
<a name="line_201"></a>c_TTP[13] := unapply([
<a name="line_202"></a>(sqrt(3)-sin(t))*sqrt(6)/9*(sqrt(3+sin(t)^2)+sin(t))*(3+sin(t)^2)^(1/4)*
<a name="line_203"></a> sqrt(sqrt(3+sin(t)^2)-sin(t))+(sqrt(3)-sin(t))^2/3+sin(t)/3*sqrt(3+sin(t)^2),
<a name="line_204"></a>(sqrt(3)-sin(t))*sqrt(6)/9*(sqrt(3+sin(t)^2)-sin(t))*(3+sin(t)^2)^(1/4)*
<a name="line_205"></a> sqrt(sqrt(3+sin(t)^2)+sin(t))+(sqrt(3)-sin(t))^2/3-sin(t)/3*sqrt(3+sin(t)^2),
<a name="line_206"></a>(sqrt(3)+sin(t))*sqrt(6)/9*(sqrt(3+sin(t)^2)+sin(t))*(3+sin(t)^2)^(1/4)*
<a name="line_207"></a> sqrt(sqrt(3+sin(t)^2)-sin(t))+(sqrt(3)+sin(t))^2/3+sin(t)/3*sqrt(3+sin(t)^2),
<a name="line_208"></a>(sqrt(3)+sin(t))*sqrt(6)/9*(sqrt(3+sin(t)^2)-sin(t))*(3+sin(t)^2)^(1/4)*
<a name="line_209"></a> sqrt(sqrt(3+sin(t)^2)+sin(t))+(sqrt(3)+sin(t))^2/3-sin(t)/3*sqrt(3+sin(t)^2)
<a name="line_210"></a>] /~ cos(t),t);
<a name="line_211"></a>c_TTP[14] := unapply(act_TTP[ L](c_TTP[13](t)),t):
<a name="line_212"></a>c_TTP[15] := unapply(act_TTP[ N](c_TTP[13](t)),t):
<a name="line_213"></a>c_TTP[16] := unapply(act_TTP[LN](c_TTP[13](t)),t):
<a name="line_214"></a>
<a name="line_215"></a>
<a name="line_216"></a><span style="color:red">#@ c_TTC
</span><a name="line_217"></a>c_TTC[ 0] := (t) -> [(sqrt(2)*I*cos(t)-1)/sqrt(2+cos(2*t)),
<a name="line_218"></a>                     (sqrt(2)*I*sin(t)-1)/sqrt(2-cos(2*t)),
<a name="line_219"></a>                     (sqrt(2)*I*cos(t)-1)/sqrt(2+cos(2*t)),
<a name="line_220"></a>                     (sqrt(2)*I*sin(t)-1)/sqrt(2-cos(2*t))];
<a name="line_221"></a>
<a name="line_222"></a>c_TTC[ 1] := (t) -> [( I*sin(t)+sqrt(2)*cos(t)-1)/(sqrt(2)-cos(t)),
<a name="line_223"></a>                     ( I*sin(t)+sqrt(2)*cos(t)-1)/(sqrt(2)-cos(t)),
<a name="line_224"></a>                     ( I*sin(t)-sqrt(2)*cos(t)-1)/(sqrt(2)+cos(t)),
<a name="line_225"></a>                     ( I*sin(t)-sqrt(2)*cos(t)-1)/(sqrt(2)+cos(t))];
<a name="line_226"></a>
<a name="line_227"></a>c_TTC[ 2] := (t) -> [(-I*sin(t)+sqrt(2)*cos(t)-1)/(sqrt(2)-cos(t)),
<a name="line_228"></a>                     ( I*sin(t)+sqrt(2)*cos(t)-1)/(sqrt(2)-cos(t)),
<a name="line_229"></a>                     (-I*sin(t)-sqrt(2)*cos(t)-1)/(sqrt(2)+cos(t)),
<a name="line_230"></a>                     ( I*sin(t)-sqrt(2)*cos(t)-1)/(sqrt(2)+cos(t))];
<a name="line_231"></a>
<a name="line_232"></a>c_TTC[ 3] := (t) -> [-1,(2+I*sqrt(2)*sin(t))/(sqrt(3)-cos(t)) - sqrt(3),
<a name="line_233"></a>                     -1,(2+I*sqrt(2)*sin(t))/(sqrt(3)+cos(t)) - sqrt(3)];
<a name="line_234"></a>
<a name="line_235"></a>c_TTC[ 4] := (t) -> [(2-I*sqrt(2)*sin(t))/(sqrt(3)-cos(t)) - sqrt(3),-1,
<a name="line_236"></a>                     (2-I*sqrt(2)*sin(t))/(sqrt(3)+cos(t)) - sqrt(3),-1];
<a name="line_237"></a> 
<a name="line_238"></a>c_TTC[ 5] := (t) -> [-(1/2)*sqrt(2)*(I*sqrt(5-cos(t))*sin(t)*sqrt(2)+2*sqrt(2)*cos(t)+
<a name="line_239"></a>                     (2*I)*sin(t)+sqrt(5-cos(t))*cos(t)-2*sqrt(2)+
<a name="line_240"></a>                     sqrt(5-cos(t)))*sqrt(2*cos(t)+6)/
<a name="line_241"></a>		     ((-3+cos(t))*(cos(t)+3)), 1,
<a name="line_242"></a>		     -(1/2)*sqrt(2)*(I*sqrt(5-cos(t))*sin(t)*sqrt(2)+2*sqrt(2)*cos(t)-
<a name="line_243"></a>                     (2*I)*sin(t)-sqrt(5-cos(t))*cos(t)-2*sqrt(2)-
<a name="line_244"></a>                     sqrt(5-cos(t)))*sqrt(2*cos(t)+6)/
<a name="line_245"></a>		     ((-3+cos(t))*(cos(t)+3)), -1];
<a name="line_246"></a>
<a name="line_247"></a>c_TTC[ 6] := unapply(act_TTC[ L](c_TTC[5](t)),t):
<a name="line_248"></a>c_TTC[ 7] := unapply(act_TTC[ M](c_TTC[5](t)),t):
<a name="line_249"></a>c_TTC[ 8] := unapply(act_TTC[LM](c_TTC[5](t)),t):
<a name="line_250"></a>
<a name="line_251"></a>c_TTC[ 9] := (t) -> [( sqrt(3)-1 - (sqrt(3)+1)*(cos(t)-I*sin(t)))/(2*sqrt(2-cos(t))),
<a name="line_252"></a>		     ( sqrt(3)-1 + (sqrt(3)+1)*(cos(t)+I*sin(t)))/(2*sqrt(2+cos(t))),
<a name="line_253"></a>		     (-sqrt(3)-1 + (sqrt(3)-1)*(cos(t)+I*sin(t)))/(2*sqrt(2-cos(t))),
<a name="line_254"></a>		     (-sqrt(3)-1 - (sqrt(3)-1)*(cos(t)-I*sin(t)))/(2*sqrt(2+cos(t)))];
<a name="line_255"></a>
<a name="line_256"></a>c_TTC[10] := unapply(act_TTC[ L](c_TTC[9](t)),t):
<a name="line_257"></a>c_TTC[11] := unapply(act_TTC[ M](c_TTC[9](t)),t):
<a name="line_258"></a>c_TTC[12] := unapply(act_TTC[LM](c_TTC[9](t)),t):
<a name="line_259"></a>
<a name="line_260"></a>c_TTC[13] := unapply(map(s -> RP1_to_SC1([s,cos(t)]),c_TTP[13](t) *~ cos(t)),t);
<a name="line_261"></a>c_TTC[14] := unapply(act_TTC[ L](c_TTC[13](t)),t):
<a name="line_262"></a>c_TTC[15] := unapply(act_TTC[ N](c_TTC[13](t)),t):
<a name="line_263"></a>c_TTC[16] := unapply(act_TTC[LN](c_TTC[13](t)),t):
<a name="line_264"></a>
<a name="line_265"></a># Images of the vertices v[i] in the 4-torus
<a name="line_266"></a>
<a name="line_267"></a><span style="color:red">#@ v_TTC
</span><a name="line_268"></a><span style="color:red">#@ v_TTP
</span><a name="line_269"></a>for i from 0 to 20 do
<a name="line_270"></a> v_TTC[i]   := combine(simplify(expand(E_to_TC ( v_E0[i]))));
<a name="line_271"></a> v_TTP[i]   := combine(simplify(expand(rationalize(map(SC1_to_R,v_TC[i])))));
<a name="line_272"></a>od:
<a name="line_273"></a>
<a name="line_274"></a>##################################################
<a name="line_275"></a>##################################################
<a name="line_276"></a># A map to the 2-torus, invariant under M = mu
<a name="line_277"></a>
<a name="line_278"></a><span style="color:red">#@ E_to_TPp_xyr 
</span><a name="line_279"></a>E_to_TPp_xyr := [sqrt(2)*x[1]/(1-sqrt(2)*y[2]),2*x[2]/y[1]/(1+sqrt(2)*y[2])];
<a name="line_280"></a>
<a name="line_281"></a><span style="color:red">#@ E_to_TCp_xyr 
</span><a name="line_282"></a>E_to_TCp_xyr := [
<a name="line_283"></a> (2*I*x[1] + y[1]^2*(1-sqrt(2)*y[2])/sqrt(2) - y[2]*(1-y[1]^2/2))/
<a name="line_284"></a>  (sqrt(2)*(1-y[1]^2/2)*(1-y[2]/sqrt(2))),
<a name="line_285"></a> (2*I*x[2]*y[1] + y[1]^2*(1+sqrt(2)*y[2]) - (1-y[1]^2/2))/
<a name="line_286"></a>  (1-y[1]^2/2)
<a name="line_287"></a>];
<a name="line_288"></a>
<a name="line_289"></a>E_to_TPp := unapply(eval(subs({y=yx0,r=rx},E_to_TPp_xyr)),x); <span style="color:red">#@ E_to_TPp
</span><a name="line_290"></a>E_to_TCp := unapply(eval(subs({y=yx0,r=rx},E_to_TCp_xyr)),x); <span style="color:red">#@ E_to_TCp
</span><a name="line_291"></a>
<a name="line_292"></a><span style="color:red">#@ E_to_TPp_jacobian 
</span><a name="line_293"></a>E_to_TPp_jacobian :=
<a name="line_294"></a> 4*sqrt(2)*(1-x[1]^2)/(1-y[1]^2/2)/(1-y[2]/sqrt(2));
<a name="line_295"></a>
<a name="line_296"></a><span style="color:red">#@ TTP_to_TPp 
</span><a name="line_297"></a>TTP_to_TPp := (t) -> [(t[1]*t[3]-1)/(t[1]+t[3]),(t[2]*t[4]+1)/(t[4]-t[2])];
<a name="line_298"></a>
<a name="line_299"></a><span style="color:red">#@ TTC_to_TCp 
</span><a name="line_300"></a>TTC_to_TCp := (z) -> [-z[1]*z[3],-z[2]/z[4]];
<a name="line_301"></a>
<a name="line_302"></a><span style="color:red">#@ TPp_p
</span><a name="line_303"></a>TPp_p[0] := (a) -> 1+a[2]^2/2+a[1]^2*(a[2]^2+3/2);
<a name="line_304"></a>TPp_p[1] := (a) -> 1+a[2]^2/2-a[1]^2*(a[2]^2+3/2);
<a name="line_305"></a>TPp_p[2] := (a) -> 3/4*a[2]^2+1;
<a name="line_306"></a>TPp_p[3] := (a) -> TPp_p[1](a)^2 + 4*a[1]^2*TPp_p[2](a);
<a name="line_307"></a>TPp_p[4] := (a) -> sqrt((sqrt(TPp_p[3](a))+TPp_p[1](a))/TPp_p[2](a)/2);
<a name="line_308"></a>
<a name="line_309"></a><span style="color:red">#@ TPp_to_E 
</span><a name="line_310"></a>TPp_to_E := (a) -> 
<a name="line_311"></a>[a[1]/sqrt(2)*(2+(1-2/TPp_p[4](a)^2)/(1+a[2]^2)),
<a name="line_312"></a> a[2]/2*(2/TPp_p[4](a)-TPp_p[4](a))/(1+a[2]^2),
<a name="line_313"></a> TPp_p[4](a),
<a name="line_314"></a> sqrt(2)*((1+a[2]^2/2)*TPp_p[4](a)-1/TPp_p[4](a))/(1+a[2]^2)];
<a name="line_315"></a>
<a name="line_316"></a>Tp_p[0] := (s) -> sqrt(9*s[1]^2-2*s[1]*s[3]+s[3]^2-8*s[1]+8*s[3]+8);
<a name="line_317"></a>Tp_p[1] := (s) -> sqrt((s[1]*s[3]+4*s[1]-1+Tp_p[0](s))/(s[1]+1));
<a name="line_318"></a>
<a name="line_319"></a><span style="color:red">#@ Tp_to_E 
</span><a name="line_320"></a>Tp_to_E := (s) -> [
<a name="line_321"></a> sqrt(8)*s[2]/(4-s[1]+s[3]+Tp_p[0](s)),
<a name="line_322"></a> sqrt(8)*sqrt(7+s[3])*s[4]/(8+3*s[1]+s[3]+Tp_p[0](s))/Tp_p[1](s),
<a name="line_323"></a> sqrt(2)*Tp_p[1](s)/sqrt(7+s[3]),
<a name="line_324"></a> -2*sqrt(7+s[3])*(1+s[3])*(1-2*s[1]+s[3])/(10+9*s[3]-5*s[1]+s[1]*s[3]+s[3]^2+(3+s[3])*Tp_p[0](s))/Tp_p[1](s)
<a name="line_325"></a>];
<a name="line_326"></a>
<a name="line_327"></a><span style="color:red">#@ TCp_to_E 
</span><a name="line_328"></a>TCp_to_E := (z) -> Tp_to_E(TC_to_T(z));
<a name="line_329"></a>
<a name="line_330"></a># The definitions below give an action of a group generated by L,M,N
<a name="line_331"></a># with M=1 and so LM = ML; this is not an action of the usual group G
<a name="line_332"></a>
<a name="line_333"></a><span style="color:red">#@ act_TPp
</span><a name="line_334"></a>act_TPp[1]     := (a) -> [ a[1], a[2]]:
<a name="line_335"></a>act_TPp[LL]    := (a) -> [-a[1],-a[2]]:
<a name="line_336"></a>act_TPp[M]     := (a) -> [ a[1], a[2]]:
<a name="line_337"></a>act_TPp[LLM]   := (a) -> [-a[1],-a[2]]:
<a name="line_338"></a>act_TPp[N]     := (a) -> [ a[1],-a[2]]:
<a name="line_339"></a>act_TPp[LLN]   := (a) -> [-a[1], a[2]]:
<a name="line_340"></a>act_TPp[MN]    := (a) -> [ a[1],-a[2]]:
<a name="line_341"></a>act_TPp[LLMN]  := (a) -> [-a[1], a[2]]:
<a name="line_342"></a>act_TPp[L]     := (a) -> [-a[2]*TPp_p[4](a)/sqrt(2), a[1]/TPp_p[4](a)*sqrt(2)];
<a name="line_343"></a>act_TPp[LLL]   := (a) -> [ a[2]*TPp_p[4](a)/sqrt(2),-a[1]/TPp_p[4](a)*sqrt(2)];
<a name="line_344"></a>act_TPp[LM]    := (a) -> [-a[2]*TPp_p[4](a)/sqrt(2), a[1]/TPp_p[4](a)*sqrt(2)];
<a name="line_345"></a>act_TPp[LLLM]  := (a) -> [ a[2]*TPp_p[4](a)/sqrt(2),-a[1]/TPp_p[4](a)*sqrt(2)];
<a name="line_346"></a>act_TPp[LN]    := (a) -> [ a[2]*TPp_p[4](a)/sqrt(2), a[1]/TPp_p[4](a)*sqrt(2)];
<a name="line_347"></a>act_TPp[LLLN]  := (a) -> [-a[2]*TPp_p[4](a)/sqrt(2),-a[1]/TPp_p[4](a)*sqrt(2)];
<a name="line_348"></a>act_TPp[LMN]   := (a) -> [ a[2]*TPp_p[4](a)/sqrt(2), a[1]/TPp_p[4](a)*sqrt(2)];
<a name="line_349"></a>act_TPp[LLLMN] := (a) -> [-a[2]*TPp_p[4](a)/sqrt(2),-a[1]/TPp_p[4](a)*sqrt(2)];
<a name="line_350"></a>
<a name="line_351"></a># Images of curves
<a name="line_352"></a>
<a name="line_353"></a><span style="color:red">#@ c_TPp
</span><a name="line_354"></a>c_TPp[ 0] := (t) -> [1/cos(t)/sqrt(2),infinity];
<a name="line_355"></a>c_TPp[ 1] := (t) -> [ sin(t),tan(t)*sqrt(2)];
<a name="line_356"></a>c_TPp[ 2] := (t) -> [-sin(t),tan(t)*sqrt(2)];
<a name="line_357"></a>c_TPp[ 3] := (t) -> [infinity,sqrt(3/2) * tan(t)];
<a name="line_358"></a>c_TPp[ 4] := (t) -> [-sin(t)/sqrt(2),infinity];
<a name="line_359"></a>c_TPp[ 5] := (t) -> [sqrt(2)*tan(t/2)/sqrt(2+sin(t/2)^2),0];
<a name="line_360"></a>c_TPp[ 6] := (t) -> [0, tan(t/2)*sqrt(2)];
<a name="line_361"></a>c_TPp[ 7] := (t) -> [sqrt(2)*tan(t/2)/sqrt(2+sin(t/2)^2),0];
<a name="line_362"></a>c_TPp[ 8] := (t) -> [0,-tan(t/2)*sqrt(2)];
<a name="line_363"></a>c_TPp[ 9] := (t) -> [ 1/sqrt(3)/tan(t/2),   tan(t/2)];
<a name="line_364"></a>c_TPp[10] := (t) -> [-1/sqrt(3)*tan(t/2), 1/tan(t/2)];
<a name="line_365"></a>c_TPp[11] := (t) -> [ 1/sqrt(3)/tan(t/2),   tan(t/2)];
<a name="line_366"></a>c_TPp[12] := (t) -> [ 1/sqrt(3)*tan(t/2),-1/tan(t/2)];
<a name="line_367"></a>c_TPp[13] := (t) -> [ (1/3)*(cos(t)^2+sin(t)*sqrt(4-cos(t)^2)+2)/cos(t),
<a name="line_368"></a>                      (1/3)*(cos(t)^2-sin(t)*sqrt(4-cos(t)^2)+2)*sqrt(3)/(cos(t)*sin(t))];
<a name="line_369"></a>c_TPp[14] := (t) -> [-(1/3)*(cos(t)^2-sin(t)*sqrt(4-cos(t)^2)+2)/cos(t),
<a name="line_370"></a>                      (1/3)*(cos(t)^2+sin(t)*sqrt(4-cos(t)^2)+2)*sqrt(3)/(cos(t)*sin(t))];
<a name="line_371"></a>c_TPp[15] := (t) -> [ (1/3)*(cos(t)^2+sin(t)*sqrt(4-cos(t)^2)+2)/cos(t),
<a name="line_372"></a>                     -(1/3)*(cos(t)^2-sin(t)*sqrt(4-cos(t)^2)+2)*sqrt(3)/(cos(t)*sin(t))];
<a name="line_373"></a>c_TPp[16] := (t) -> [ (1/3)*(cos(t)^2-sin(t)*sqrt(4-cos(t)^2)+2)/cos(t),
<a name="line_374"></a>                      (1/3)*(cos(t)^2+sin(t)*sqrt(4-cos(t)^2)+2)*sqrt(3)/(cos(t)*sin(t))];
<a name="line_375"></a>
<a name="line_376"></a><span style="color:red">#@ c_TCp
</span><a name="line_377"></a>c_TCp[ 0] := (t) -> [(cos(t) + I/sqrt(2))/(cos(t) - I/sqrt(2)),-1];
<a name="line_378"></a>c_TCp[ 1] := (t) -> [(I - sin(t))/(I + sin(t)),
<a name="line_379"></a>                     (I*cos(t)/sqrt(2)-sin(t))/(I*cos(t)/sqrt(2)+sin(t))];
<a name="line_380"></a>c_TCp[ 2] := (t) -> [(I + sin(t))/(I - sin(t)),
<a name="line_381"></a>                     (I*cos(t)/sqrt(2)-sin(t))/(I*cos(t)/sqrt(2)+sin(t))];
<a name="line_382"></a>c_TCp[ 3] := (t) -> [-1,(cos(t) + I * sqrt(3/2) * sin(t))/
<a name="line_383"></a>                        (cos(t) - I * sqrt(3/2) * sin(t))];
<a name="line_384"></a>c_TCp[ 4] := (t) -> [(I*sqrt(2) + sin(t))/(I*sqrt(2) - sin(t)),-1];
<a name="line_385"></a>c_TCp[ 5] := (t) -> [(1+8*cos(t)-cos(t)^2+4*sqrt(2)*I*sin(t)*sqrt(2+sin(t/2)^2))/
<a name="line_386"></a>                      (9-cos(t)^2),1];
<a name="line_387"></a>c_TCp[ 6] := (t) -> [1,(I*sin(t)/sqrt(2)+cos(t)-1)/(I*sin(t)/sqrt(2)-cos(t)+1)];
<a name="line_388"></a>c_TCp[ 7] := (t) -> [(1+8*cos(t)-cos(t)^2+4*sqrt(2)*I*sin(t)*sqrt(2+sin(t/2)^2))/
<a name="line_389"></a>                       (9-cos(t)^2),1];
<a name="line_390"></a>c_TCp[ 8] := (t) -> [1,(I*sin(t)/sqrt(2)-cos(t)+1)/(I*sin(t)/sqrt(2)+cos(t)-1)];
<a name="line_391"></a>c_TCp[ 9] := (t) -> [(cos(t)-1-I*sin(t)/sqrt(3))/(cos(t)-1+I*sin(t)/sqrt(3)),
<a name="line_392"></a>                     (cos(t)-1+I*sin(t))/(-cos(t)+1+I*sin(t))];
<a name="line_393"></a>c_TCp[10] := (t) -> [(cos(t)-1-I*sin(t)*sqrt(3))/(-cos(t)+1-I*sin(t)*sqrt(3)),
<a name="line_394"></a>                     (cos(t)-1-I*sin(t))/(cos(t)-1+I*sin(t))];
<a name="line_395"></a>c_TCp[11] := (t) -> [(cos(t)-1-I*sin(t)/sqrt(3))/(cos(t)-1+I*sin(t)/sqrt(3)),
<a name="line_396"></a>                     (cos(t)-1+I*sin(t))/(-cos(t)+1+I*sin(t))];
<a name="line_397"></a>c_TCp[12] := (t) -> [(cos(t)-1+I*sin(t)*sqrt(3))/(-cos(t)+1+I*sin(t)*sqrt(3)),
<a name="line_398"></a>                     (cos(t)-1+I*sin(t))/(cos(t)-1-I*sin(t))];
<a name="line_399"></a>
<a name="line_400"></a>for i from 13 to 16 do
<a name="line_401"></a> c_TCp[i] := unapply(simplify(TP_to_TC(c_TPp[i](t))),t);
<a name="line_402"></a>od:
<a name="line_403"></a>
<a name="line_404"></a># Images of vertices
<a name="line_405"></a>
<a name="line_406"></a><span style="color:red">#@ v_TCp
</span><a name="line_407"></a><span style="color:red">#@ v_TPp
</span><a name="line_408"></a>for i from 0 to 20 do
<a name="line_409"></a> v_TCp[i]  := combine(simplify(expand(E_to_TCp( v_E0[i]))));
<a name="line_410"></a> v_TPp[i]  := combine(simplify(expand(rationalize(map(SC1_to_R,v_TCp[i])))));
<a name="line_411"></a>od:
<a name="line_412"></a>
<a name="line_413"></a>##################################################
<a name="line_414"></a>##################################################
<a name="line_415"></a># The map E_to_TPpa is the composite of E_to_TPp with the automorphism
<a name="line_416"></a># a |-> [a[1]/sqrt(2),a[2]/2] of (R u {infinity})^2.  Some formulae
<a name="line_417"></a># for TPpa are tidier than the analogous formulae for TPp.
<a name="line_418"></a>
<a name="line_419"></a><span style="color:red">#@ E_to_TPpa_xyr 
</span><a name="line_420"></a>E_to_TPpa_xyr := [x[1]/(1-y[2]*sqrt(2)),x[2]/y[1]/(1+y[2]*sqrt(2))];
<a name="line_421"></a>
<a name="line_422"></a><span style="color:red">#@ E_to_TCpa_xyr 
</span><a name="line_423"></a>E_to_TCpa_xyr := [(2*y[1]^2*y[2]+8*y[2]-2*sqrt(2)*y[1]^2-2*sqrt(2)-(8*I)*sqrt(2)*x[1])/
<a name="line_424"></a>                  (2*sqrt(2)*y[1]^2-2*y[1]^2*y[2]-6*sqrt(2)+8*y[2]),
<a name="line_425"></a>                 (10*y[1]^2*y[2]+6*sqrt(2)*y[1]^2+(8*I)*sqrt(2)*x[2]*y[1]-2*sqrt(2))/
<a name="line_426"></a>		  (6*y[1]^2*y[2]+2*sqrt(2)*y[1]^2+2*sqrt(2))];
<a name="line_427"></a>
<a name="line_428"></a><span style="color:red">#@ E_to_TPpa 
</span><a name="line_429"></a>E_to_TPpa := unapply(eval(subs({y=yx0,r=rx},E_to_TPpa_xyr)),x);
<a name="line_430"></a>
<a name="line_431"></a><span style="color:red">#@ E_to_TCpa 
</span><a name="line_432"></a>E_to_TCpa := unapply(eval(subs({y=yx0,r=rx},E_to_TCpa_xyr)),x);
<a name="line_433"></a>
<a name="line_434"></a><span style="color:red">#@ E_to_TPpa_jacobian 
</span><a name="line_435"></a>E_to_TPpa_jacobian :=
<a name="line_436"></a> 64*(1-x[1]^2)*(1-y[1]^2/2)/
<a name="line_437"></a>  ((-y[1]^2*sqrt(2)+y[1]^2*y[2]+3*sqrt(2)-4*y[2])*(y[1]^2*sqrt(2)+3*y[1]^2*y[2]+sqrt(2)));
<a name="line_438"></a> 
<a name="line_439"></a><span style="color:red">#@ TPp_to_TPpa 
</span><a name="line_440"></a>TPp_to_TPpa := (t) -> [t[1]/sqrt(2),t[2]/2];
<a name="line_441"></a>
<a name="line_442"></a><span style="color:red">#@ TCp_to_TCpa 
</span><a name="line_443"></a>TCp_to_TCpa := (z) -> [-mob(-z[1],3-2*sqrt(2)),-mob(-z[2],1/3)];
<a name="line_444"></a>
<a name="line_445"></a><span style="color:red">#@ TPpa_to_TPp 
</span><a name="line_446"></a>TPpa_to_TPp := (t) -> [t[1]*sqrt(2),t[2]*2];
<a name="line_447"></a>
<a name="line_448"></a><span style="color:red">#@ TCpa_to_TCp 
</span><a name="line_449"></a>TCpa_to_TCp := (z) -> [-mob(-z[1],-3+2*sqrt(2)),-mob(-z[2],-1/3)];
<a name="line_450"></a>
<a name="line_451"></a><span style="color:red">#@ TTP_to_TPpa  
</span><a name="line_452"></a>TTP_to_TPpa  := (t) -> [(t[1]*t[3]-1)/(t[1]+t[3])/sqrt(2),(t[2]*t[4]+1)/(t[4]-t[2])/2];
<a name="line_453"></a>
<a name="line_454"></a><span style="color:red">#@ TTC_to_TCpa  
</span><a name="line_455"></a>TTC_to_TCpa  := (z) -> [-mob(z[1]*z[3],3-2*sqrt(2)),-mob(z[2]/z[4],1/3)];
<a name="line_456"></a>
<a name="line_457"></a><span style="color:red">#@ TPpa_p
</span><a name="line_458"></a>TPpa_p[0] := (a) -> 1 + 2*a[2]^2 + a[1]^2*(3 + 8*a[2]^2);
<a name="line_459"></a>TPpa_p[1] := (a) -> 1 + 2*a[2]^2 - a[1]^2*(3 + 8*a[2]^2);
<a name="line_460"></a>TPpa_p[2] := (a) -> 1 + 3*a[2]^2;
<a name="line_461"></a>TPpa_p[3] := (a) -> TPpa_p[1](a)^2 + 8*a[1]^2*TPpa_p[2](a);
<a name="line_462"></a>TPpa_p[4] := (a) -> sqrt((sqrt(TPpa_p[3](a))+TPpa_p[1](a))/TPpa_p[2](a)/2);
<a name="line_463"></a>
<a name="line_464"></a># A (discontinuous) section of the map E_to_TPpa
<a name="line_465"></a>
<a name="line_466"></a><span style="color:red">#@ TPpa_to_E 
</span><a name="line_467"></a>TPpa_to_E := (a) -> 
<a name="line_468"></a>[a[1]*(2+(1-2/TPpa_p[4](a)^2)/(1+4*a[2]^2)),
<a name="line_469"></a> a[2]*(2/TPpa_p[4](a)-TPpa_p[4](a))/(1+4*a[2]^2),
<a name="line_470"></a> TPpa_p[4](a),
<a name="line_471"></a> sqrt(2)*((1+2*a[2]^2)*TPpa_p[4](a)-1/TPpa_p[4](a))/(1+4*a[2]^2)];
<a name="line_472"></a>
<a name="line_473"></a><span style="color:red">#@ Tpa_p
</span><a name="line_474"></a>Tpa_p[0] := (s) ->  (4*s[1]*s[3]- 8*s[1]-6*s[3]+14)/((1+s[3])*(1+s[1]));
<a name="line_475"></a>Tpa_p[1] := (s) -> -(6*s[1]*s[3]-14*s[1]-4*s[3]+ 8)/((1+s[3])*(1+s[1]));
<a name="line_476"></a>Tpa_p[2] := (s) ->  (4-2*s[3])/(1+s[3]);
<a name="line_477"></a>Tpa_p[3] := (s) -> (4*(3*s[1]*s[3]-7*s[1]-2*s[3]+4)^2+16*(1-s[1]^2)*(2-s[3])*(1+s[3]))/((1+s[1])^2*(1+s[3])^2);
<a name="line_478"></a>Tpa_p[4] := (s) -> sqrt((13*s[1]*s[3]^2-46*s[1]*s[3]-25*s[3]^2+41*s[1]+98*s[3]-97)/2/(2-s[3])/(sqrt((3*s[1]*s[3]-7*s[1]-2*s[3]+4)^2+4*(1-s[1]^2)*(2-s[3])*(s[3]+1))+11-5*s[3]) + (7-3*s[3])/2/(2-s[3]));
<a name="line_479"></a>
<a name="line_480"></a><span style="color:red">#@ Tpa_to_E 
</span><a name="line_481"></a>Tpa_to_E := (s) -> [
<a name="line_482"></a>s[2]*(2 + (1-2/Tpa_p[4](s)^2)*(1+s[3])/(5-3*s[3]))/(1+s[1]),
<a name="line_483"></a>s[4]*(2/Tpa_p[4](s)-Tpa_p[4](s))/(5-3*s[3]),
<a name="line_484"></a>Tpa_p[4](s),
<a name="line_485"></a>sqrt(2)*((3-s[3])*Tpa_p[4](s)-(1+s[3])/Tpa_p[4](s))/(5-3*s[3])
<a name="line_486"></a>];
<a name="line_487"></a>
<a name="line_488"></a><span style="color:red">#@ TCpa_to_E 
</span><a name="line_489"></a>TCpa_to_E := (z) -> Tpa_to_E(TC_to_T(z));
<a name="line_490"></a>
<a name="line_491"></a># The definitions below give an action of a group generated by L,M,N
<a name="line_492"></a># with M=1 and so LM = ML; this is not an action of the usual group G
<a name="line_493"></a>
<a name="line_494"></a><span style="color:red">#@ act_TPpa
</span><a name="line_495"></a>act_TPpa[1]     := (a) -> [ a[1], a[2]]:
<a name="line_496"></a>act_TPpa[LL]    := (a) -> [-a[1],-a[2]]:
<a name="line_497"></a>act_TPpa[M]     := (a) -> [ a[1], a[2]]:
<a name="line_498"></a>act_TPpa[LLM]   := (a) -> [-a[1],-a[2]]:
<a name="line_499"></a>act_TPpa[N]     := (a) -> [ a[1],-a[2]]:
<a name="line_500"></a>act_TPpa[LLN]   := (a) -> [-a[1], a[2]]:
<a name="line_501"></a>act_TPpa[MN]    := (a) -> [ a[1],-a[2]]:
<a name="line_502"></a>act_TPpa[LLMN]  := (a) -> [-a[1], a[2]]:
<a name="line_503"></a>act_TPpa[L]     := (a) -> [-a[2]*TPpa_p[4](a), a[1]/TPpa_p[4](a)];
<a name="line_504"></a>act_TPpa[LLL]   := (a) -> [ a[2]*TPpa_p[4](a),-a[1]/TPpa_p[4](a)];
<a name="line_505"></a>act_TPpa[LM]    := (a) -> [-a[2]*TPpa_p[4](a), a[1]/TPpa_p[4](a)];
<a name="line_506"></a>act_TPpa[LLLM]  := (a) -> [ a[2]*TPpa_p[4](a),-a[1]/TPpa_p[4](a)];
<a name="line_507"></a>act_TPpa[LN]    := (a) -> [ a[2]*TPpa_p[4](a), a[1]/TPpa_p[4](a)];
<a name="line_508"></a>act_TPpa[LLLN]  := (a) -> [-a[2]*TPpa_p[4](a),-a[1]/TPpa_p[4](a)];
<a name="line_509"></a>act_TPpa[LMN]   := (a) -> [ a[2]*TPpa_p[4](a), a[1]/TPpa_p[4](a)];
<a name="line_510"></a>act_TPpa[LLLMN] := (a) -> [-a[2]*TPpa_p[4](a),-a[1]/TPpa_p[4](a)];
<a name="line_511"></a>
<a name="line_512"></a># Images of curves
<a name="line_513"></a>
<a name="line_514"></a><span style="color:red">#@ c_TPpa
</span><a name="line_515"></a>c_TPpa[ 0] := (t) -> [1/cos(t)/2,infinity];
<a name="line_516"></a>c_TPpa[ 1] := (t) -> [ sin(t)/sqrt(2),tan(t)/sqrt(2)];
<a name="line_517"></a>c_TPpa[ 2] := (t) -> [-sin(t)/sqrt(2),tan(t)/sqrt(2)];
<a name="line_518"></a>c_TPpa[ 3] := (t) -> [infinity,sqrt(3/8) * tan(t)];
<a name="line_519"></a>c_TPpa[ 4] := (t) -> [-sin(t)/2,infinity];
<a name="line_520"></a>c_TPpa[ 5] := (t) -> [tan(t/2)/sqrt(2+sin(t/2)^2),0];
<a name="line_521"></a>c_TPpa[ 6] := (t) -> [0, tan(t/2)/sqrt(2)];
<a name="line_522"></a>c_TPpa[ 7] := (t) -> [tan(t/2)/sqrt(2+sin(t/2)^2),0];
<a name="line_523"></a>c_TPpa[ 8] := (t) -> [0,-tan(t/2)/sqrt(2)];
<a name="line_524"></a>c_TPpa[ 9] := (t) -> [ 1/sqrt(6)/tan(t/2),   tan(t/2)/2];
<a name="line_525"></a>c_TPpa[10] := (t) -> [-1/sqrt(6)*tan(t/2), 1/tan(t/2)/2];
<a name="line_526"></a>c_TPpa[11] := (t) -> [ 1/sqrt(6)/tan(t/2),   tan(t/2)/2];
<a name="line_527"></a>c_TPpa[12] := (t) -> [ 1/sqrt(6)*tan(t/2),-1/tan(t/2)/2];
<a name="line_528"></a>c_TPpa[13] := (t) -> [ (1/3/sqrt(2))*(cos(t)^2+sin(t)*sqrt(4-cos(t)^2)+2)/cos(t),
<a name="line_529"></a>                       (1/6)*(cos(t)^2-sin(t)*sqrt(4-cos(t)^2)+2)*sqrt(3)/(cos(t)*sin(t))];
<a name="line_530"></a>c_TPpa[14] := (t) -> [-(1/3/sqrt(2))*(cos(t)^2-sin(t)*sqrt(4-cos(t)^2)+2)/cos(t),
<a name="line_531"></a>                       (1/6)*(cos(t)^2+sin(t)*sqrt(4-cos(t)^2)+2)*sqrt(3)/(cos(t)*sin(t))];
<a name="line_532"></a>c_TPpa[15] := (t) -> [ (1/3/sqrt(2))*(cos(t)^2+sin(t)*sqrt(4-cos(t)^2)+2)/cos(t),
<a name="line_533"></a>                      -(1/6)*(cos(t)^2-sin(t)*sqrt(4-cos(t)^2)+2)*sqrt(3)/(cos(t)*sin(t))];
<a name="line_534"></a>c_TPpa[16] := (t) -> [ (1/3/sqrt(2))*(cos(t)^2-sin(t)*sqrt(4-cos(t)^2)+2)/cos(t),
<a name="line_535"></a>                       (1/6)*(cos(t)^2+sin(t)*sqrt(4-cos(t)^2)+2)*sqrt(3)/(cos(t)*sin(t))];
<a name="line_536"></a>
<a name="line_537"></a><span style="color:red">#@ c_TCpa
</span><a name="line_538"></a>c_TCpa[ 0] := (t) -> [(cos(t) + I/2)/(cos(t) - I/2),-1];
<a name="line_539"></a>c_TCpa[ 1] := (t) -> [(1 + I*sin(t)/sqrt(2))/(1 - I*sin(t)/sqrt(2)),mob(exp(2*I*t),-(3-sqrt(8)))];
<a name="line_540"></a>c_TCpa[ 2] := (t) -> [(1 - I*sin(t)/sqrt(2))/(1 + I*sin(t)/sqrt(2)),mob(exp(2*I*t),-(3-sqrt(8)))];
<a name="line_541"></a>c_TCpa[ 3] := (t) -> [-1,mob(exp(2*I*t),-(11-sqrt(96))/5)];
<a name="line_542"></a>c_TCpa[ 4] := (t) -> [mob(exp(I*t),2+sqrt(5))*mob(exp(I*t),2-sqrt(5)),-1];
<a name="line_543"></a>c_TCpa[ 5] := (t) -> [-I*(I*cos(t)^2+2*sqrt(10-2*cos(t))*sin(t)-(6*I)*cos(t)-3*I)/(cos(t)^2-2*cos(t)-7), 1];
<a name="line_544"></a>c_TCpa[ 6] := (t) -> [ 1,mob(exp(I*t),-(3-sqrt(8)))];
<a name="line_545"></a>c_TCpa[ 7] := (t) -> [-I*(I*cos(t)^2+2*sqrt(10-2*cos(t))*sin(t)-(6*I)*cos(t)-3*I)/(cos(t)^2-2*cos(t)-7), 1];
<a name="line_546"></a>c_TCpa[ 8] := (t) -> [ 1,mob(exp(I*t),-(3+sqrt(8)))];
<a name="line_547"></a>c_TCpa[ 9] := (t) -> [-mob(exp(I*t), (7+2*sqrt(6))/5), mob(exp(I*t),-1/3)];
<a name="line_548"></a>c_TCpa[10] := (t) -> [ mob(exp(I*t),-(7+2*sqrt(6))/5),-mob(exp(I*t),   3)];
<a name="line_549"></a>c_TCpa[11] := (t) -> [-mob(exp(I*t), (7+2*sqrt(6))/5), mob(exp(I*t),-1/3)];
<a name="line_550"></a>c_TCpa[12] := (t) -> [ mob(exp(I*t),-(7-2*sqrt(6))/5),-mob(exp(I*t), 1/3)];
<a name="line_551"></a>
<a name="line_552"></a>for i from 13 to 16 do
<a name="line_553"></a> c_TCpa[i] := unapply(simplify(TP_to_TC(c_TPpa[i](t))),t);
<a name="line_554"></a>od:
<a name="line_555"></a>
<a name="line_556"></a># Images of vertices
<a name="line_557"></a>
<a name="line_558"></a><span style="color:red">#@ v_TCpa
</span><a name="line_559"></a><span style="color:red">#@ v_TPpa
</span><a name="line_560"></a>for i from 0 to 20 do
<a name="line_561"></a> v_TCpa[i]  := combine(simplify(expand(E_to_TCpa( v_E0[i]))));
<a name="line_562"></a> v_TPpa[i]  := combine(simplify(expand(rationalize(map(SC1_to_R,v_TCpa[i])))));
<a name="line_563"></a>od:
<a name="line_564"></a>
<a name="line_565"></a>######################################################################
<a name="line_566"></a>######################################################################
<a name="line_567"></a># E_to_TCm is map to the 2-torus, invariant under LM = lambda mu
<a name="line_568"></a>
<a name="line_569"></a><span style="color:red">#@ E_to_TPm_xyr 
</span><a name="line_570"></a>E_to_TPm_xyr := [
<a name="line_571"></a> -(x[1]*y[1]*sqrt(2)*y[2]+x[2]*y[1]*sqrt(2)*y[2]+sqrt(2)*x[1]+sqrt(2)*x[2]+2*x[1]*y[1]-2*x[2]*y[1])/
<a name="line_572"></a>  (r[1]*r[2]*y[1]^2+y[1]^2*y[2]^2-2*r[1]*r[2]-2*x[1]*x[2]-2*y[1]^2+2*y[1]*y[2]+1),
<a name="line_573"></a>  (x[1]*y[1]*sqrt(2)*y[2]+x[2]*y[1]*sqrt(2)*y[2]-sqrt(2)*x[1]-sqrt(2)*x[2]+2*x[1]*y[1]-2*x[2]*y[1])/
<a name="line_574"></a>  (r[1]*r[2]*y[1]^2+y[1]^2*y[2]^2-2*r[1]*r[2]-2*x[1]*x[2]-2*y[1]^2-2*y[1]*y[2]+1)
<a name="line_575"></a>];
<a name="line_576"></a>
<a name="line_577"></a><span style="color:red">#@ E_to_TCm_xyr 
</span><a name="line_578"></a>E_to_TCm_xyr := [
<a name="line_579"></a>-(I*x[1] + y[1]*(1 - y[2]/sqrt(2)) - 1/sqrt(2))*
<a name="line_580"></a> (I*x[2] - y[1]*(1 + y[2]/sqrt(2)) - 1/sqrt(2))/(r[1]*r[2]*(1-y[1]^2/2)),
<a name="line_581"></a>-(I*x[1] - y[1]*(1 - y[2]/sqrt(2)) - 1/sqrt(2))*
<a name="line_582"></a> (I*x[2] + y[1]*(1 + y[2]/sqrt(2)) - 1/sqrt(2))/(r[1]*r[2]*(1-y[1]^2/2))
<a name="line_583"></a>];
<a name="line_584"></a>
<a name="line_585"></a><span style="color:red">#@ E_to_TPm    
</span><a name="line_586"></a>E_to_TPm    := unapply(eval(subs({y=yx0,r=rx},E_to_TPm_xyr)),x);
<a name="line_587"></a>
<a name="line_588"></a><span style="color:red">#@ E_to_TCm    
</span><a name="line_589"></a>E_to_TCm    := unapply(eval(subs({y=yx0,r=rx},E_to_TCm_xyr)),x);
<a name="line_590"></a>
<a name="line_591"></a><span style="color:red">#@ TTC_to_TCm   
</span><a name="line_592"></a>TTC_to_TCm   := (z) -> [-z[1]*z[4],-z[2]*z[3]];
<a name="line_593"></a>
<a name="line_594"></a><span style="color:red">#@ TTP_to_TPm   
</span><a name="line_595"></a>TTP_to_TPm   := (t) -> [(t[1]*t[4]-1)/(t[1]+t[4]),(t[2]*t[3]-1)/(t[2]+t[3])];
<a name="line_596"></a>
<a name="line_597"></a><span style="color:red">#@ E_to_TPm_jacobian  
</span><a name="line_598"></a>E_to_TPm_jacobian  := 4*sqrt(2)*(3/2 - (1-y[1]^2/2)*(1-y[2]^2/2) - x[1]*x[2])/((1-y[1]^2/2)*(1-y[2]^2/2));
<a name="line_599"></a>
<a name="line_600"></a>########################################
<a name="line_601"></a>
<a name="line_602"></a><span style="color:red">#@ act_TCm
</span><a name="line_603"></a>act_TCm[1]     := (z) -> [  z[1],  z[2]];
<a name="line_604"></a>act_TCm[LL]    := (z) -> [1/z[1],1/z[2]];
<a name="line_605"></a>act_TCm[LM]    := (z) -> [  z[1],  z[2]];
<a name="line_606"></a>act_TCm[LLLM]  := (z) -> [1/z[1],1/z[2]];
<a name="line_607"></a>act_TCm[LN]    := (z) -> [  z[2],  z[1]];
<a name="line_608"></a>act_TCm[LLLN]  := (z) -> [1/z[2],1/z[1]];
<a name="line_609"></a>act_TCm[MN]    := (z) -> [  z[2],  z[1]];
<a name="line_610"></a>act_TCm[LLMN]  := (z) -> [1/z[2],1/z[1]];
<a name="line_611"></a>
<a name="line_612"></a>########################################
<a name="line_613"></a>
<a name="line_614"></a><span style="color:red">#@ v_TCm
</span><a name="line_615"></a>for i from 0 to num_vertices_E-1 do
<a name="line_616"></a> v_TCm[i]  := simplify(expand(E_to_TCm( v_E0[i])));
<a name="line_617"></a>od:
<a name="line_618"></a>
<a name="line_619"></a><span style="color:red">#@ v_TPm
</span><a name="line_620"></a>v_TPm[ 0] := [0,0];
<a name="line_621"></a>v_TPm[ 1] := [0,0];
<a name="line_622"></a>v_TPm[ 2] := [ 1, 1] *~ ((1+sqrt(3))/sqrt(2));
<a name="line_623"></a>v_TPm[ 3] := [ 1, 1] *~ ((1+sqrt(3))/sqrt(2));
<a name="line_624"></a>v_TPm[ 4] := [-1,-1] *~ ((1+sqrt(3))/sqrt(2));
<a name="line_625"></a>v_TPm[ 5] := [-1,-1] *~ ((1+sqrt(3))/sqrt(2));
<a name="line_626"></a>v_TPm[ 6] := [ 1, 1];
<a name="line_627"></a>v_TPm[ 8] := [-1,-1];
<a name="line_628"></a>v_TPm[ 7] := [infinity,infinity];
<a name="line_629"></a>v_TPm[ 9] := [infinity,infinity];
<a name="line_630"></a>v_TPm[10] := [       0,infinity];
<a name="line_631"></a>v_TPm[13] := [       0,infinity];
<a name="line_632"></a>v_TPm[11] := [infinity,       0];
<a name="line_633"></a>v_TPm[12] := [infinity,       0];
<a name="line_634"></a>v_TPm[14] := [ 1, 1] /~ sqrt(3);
<a name="line_635"></a>v_TPm[19] := [ 1, 1] /~ sqrt(3); 
<a name="line_636"></a>v_TPm[16] := [-1,-1] /~ sqrt(3);
<a name="line_637"></a>v_TPm[21] := [-1,-1] /~ sqrt(3); 
<a name="line_638"></a>v_TPm[17] := [ 1,-1];
<a name="line_639"></a>v_TPm[20] := [ 1,-1];
<a name="line_640"></a>v_TPm[15] := [-1, 1];
<a name="line_641"></a>v_TPm[18] := [-1, 1];
<a name="line_642"></a>
<a name="line_643"></a>#for i from 0 to num_curves_E - 1 do
<a name="line_644"></a># c_TC[i]   := unapply(combine(rationalize(E_to_TC( c_E0[i](t)))),t);
<a name="line_645"></a># c_TCp[i]  := unapply(combine(rationalize(E_to_TCp( c_E0[i](t)))),t);
<a name="line_646"></a># c_TCm[i]  := unapply(simplify(rationalize(factor(expand(combine(E_to_TCm( c_E0[i](t))))))),t);
<a name="line_647"></a># c_TCmq[i] := unapply(combine(factor(expand(rationalize(factor(expand(combine(E_to_TCmq(c_E0[i](t))))))))),t);
<a name="line_648"></a>#od;
<a name="line_649"></a>
<a name="line_650"></a><span style="color:red">#@ c_TPm
</span><a name="line_651"></a>c_TPm[ 0] := (t) -> [ 1, 1] *~ ((sqrt(sin(t-Pi/4)^4+cos(t-Pi/4)^2)+sin(t-Pi/4)^2)/cos(t-Pi/4));
<a name="line_652"></a>c_TPm[ 1] := (t) -> [ 1, 1] *~ sin(t);
<a name="line_653"></a>c_TPm[ 2] := (t) -> [-1, 1] *~ (sqrt(2) * tan(t));
<a name="line_654"></a>c_TPm[ 3] := (t) -> [ (1/2)*sqrt(2)*(1+sqrt(3))*sin(t)/(1-cos(t)),
<a name="line_655"></a>                      (1/2)*sqrt(2)*(1+sqrt(3))*sin(t)/(1+cos(t))];
<a name="line_656"></a>c_TPm[ 4] := (t) -> [-(1/2)*sqrt(2)*(1+sqrt(3))*sin(t)/(1+cos(t)),
<a name="line_657"></a>                     -(1/2)*sqrt(2)*(1+sqrt(3))*sin(t)/(1-cos(t))];
<a name="line_658"></a>c_TPm[ 5] := (t) -> [ sqrt(2)/sin(t)*((sqrt(2+sin(t/2)^2)-1)*(sqrt(2-sin(t/2)^2)+1)-cos(t/2)^2),
<a name="line_659"></a>                      -1/(sqrt(2)/sin(t)*((sqrt(2+sin(t/2)^2)+1)*(sqrt(2-sin(t/2)^2)+1)+cos(t/2)^2))];
<a name="line_660"></a>c_TPm[ 6] := (t) -> [ -1/(sqrt(2)/sin(t)*((sqrt(2+sin(t/2)^2)+1)*(sqrt(2-sin(t/2)^2)+1)+cos(t/2)^2)),
<a name="line_661"></a>                      sqrt(2)/sin(t)*((sqrt(2+sin(t/2)^2)-1)*(sqrt(2-sin(t/2)^2)+1)-cos(t/2)^2)];
<a name="line_662"></a>c_TPm[ 7] := unapply(c_TPm[ 6](t),t);
<a name="line_663"></a>c_TPm[ 8] := unapply(c_TPm[ 5](t),t);
<a name="line_664"></a>c_TPm[ 9] := (t) -> [(sqrt(4-cos(t)^2)-sin(t)^2+sqrt(3)*cos(t))/sin(t)/(sqrt(3)+cos(t)),
<a name="line_665"></a>                     (sqrt(4-cos(t)^2)-sin(t)^2-sqrt(3)*cos(t))/sin(t)/(sqrt(3)-cos(t))];
<a name="line_666"></a>c_TPm[10] := (t) -> [(sqrt(3)*cos(t) - sqrt(4-cos(t)^2))/2/sin(t),
<a name="line_667"></a>                     (sqrt(3)*cos(t) + sqrt(4-cos(t)^2))/2/sin(t)];
<a name="line_668"></a>c_TPm[11] := unapply(c_TPm[10](t),t);
<a name="line_669"></a>c_TPm[12] := unapply(c_TPm[ 9](t),t);
<a name="line_670"></a>c_TPm[13] := (t) -> [sqrt(3+sin(t)^2)/cos(t)/sqrt(3),sqrt(3)*cos(t)/sqrt(3+sin(t)^2)];
<a name="line_671"></a>c_TPm[14] := (t) -> [-cos(t)*(cos(t)^2*sqrt(3)+2*sqrt(3)+3*sqrt(4-cos(t)^2))/(sin(t)*(cos(t)^2+8)),
<a name="line_672"></a>                     (1/3)*(cos(t)^2*sqrt(3)+2*sqrt(3)+3*sqrt(4-cos(t)^2))/(sin(t)*cos(t))];
<a name="line_673"></a>c_TPm[15] := (t) -> [(1/3)*(cos(t)^2*sqrt(3)+2*sqrt(3)+3*sqrt(4-cos(t)^2))/(sin(t)*cos(t)),
<a name="line_674"></a>                     -cos(t)*(cos(t)^2*sqrt(3)+2*sqrt(3)+3*sqrt(4-cos(t)^2))/(sin(t)*(cos(t)^2+8))];
<a name="line_675"></a>c_TPm[16] := (t) -> [sqrt(3)*cos(t)/sqrt(3+sin(t)^2),sqrt(3+sin(t)^2)/cos(t)/sqrt(3)];
<a name="line_676"></a>
<a name="line_677"></a>
<a name="line_678"></a><span style="color:red">#@ c_TCm
</span><a name="line_679"></a>c_TCm[ 0] := (t) -> [(I*sqrt(2)*sin(t)+I*sqrt(2)*cos(t)+2*sin(t)*cos(t)-1)/sqrt(4-cos(2*t)^2),
<a name="line_680"></a>                     (I*sqrt(2)*sin(t)+I*sqrt(2)*cos(t)+2*sin(t)*cos(t)-1)/sqrt(4-cos(2*t)^2)];
<a name="line_681"></a>c_TCm[ 1] := (t) -> [(I - sin(t))/(I + sin(t)),(I - sin(t))/(I + sin(t))];
<a name="line_682"></a>c_TCm[ 2] := (t) -> [-(I*sqrt(2)*cos(t)+2*sin(t))/(-I*sqrt(2)*cos(t)+2*sin(t)),
<a name="line_683"></a>                     -(-I*sqrt(2)*cos(t)+2*sin(t))/(I*sqrt(2)*cos(t)+2*sin(t))];
<a name="line_684"></a>c_TCm[ 3] := (t) -> [(I*sqrt(2)*sin(t)-sqrt(3)*cos(t)-1)/(sqrt(3)+cos(t)),
<a name="line_685"></a>                     (-sqrt(3)*cos(t)+1-I*sqrt(2)*sin(t))/(-sqrt(3)+cos(t))];
<a name="line_686"></a>c_TCm[ 4] := (t) -> [(I*sqrt(2)*sin(t)-sqrt(3)*cos(t)+1)/(-sqrt(3)+cos(t)),
<a name="line_687"></a>                     (-sqrt(3)*cos(t)-1-I*sqrt(2)*sin(t))/(sqrt(3)+cos(t))];
<a name="line_688"></a>c_TCm[ 5] := (t) -> [(((2*I)*sqrt(2)*sin(t)+2*cos(t)+2)*sqrt(2+sin((1/2)*t)^2)+(2*I)*sqrt(2)*sin(t)+4*cos(t)-4)*
<a name="line_689"></a>                       sqrt(2-sin((1/2)*t)^2)/(9-cos(t)^2),
<a name="line_690"></a>		     ((-(2*I)*sqrt(2)*sin(t)+2*cos(t)+2)*sqrt(2+sin((1/2)*t)^2)+(2*I)*sqrt(2)*sin(t)-4*cos(t)+4)*
<a name="line_691"></a>		       sqrt(2-sin((1/2)*t)^2)/(9-cos(t)^2)];
<a name="line_692"></a>c_TCm[ 6] := (t) -> [((-(2*I)*sqrt(2)*sin(t)+2*cos(t)+2)*sqrt(2+sin((1/2)*t)^2)+(2*I)*sqrt(2)*sin(t)-4*cos(t)+4)*
<a name="line_693"></a>                       sqrt(2-sin((1/2)*t)^2)/(9-cos(t)^2),
<a name="line_694"></a>		     (((2*I)*sqrt(2)*sin(t)+2*cos(t)+2)*sqrt(2+sin((1/2)*t)^2)+(2*I)*sqrt(2)*sin(t)+4*cos(t)-4)*
<a name="line_695"></a>		       sqrt(2-sin((1/2)*t)^2)/(9-cos(t)^2)];
<a name="line_696"></a>c_TCm[ 7] := (t) -> [((-(2*I)*sqrt(2)*sin(t)+2*cos(t)+2)*sqrt(2+sin((1/2)*t)^2)+(2*I)*sqrt(2)*sin(t)-4*cos(t)+4)*
<a name="line_697"></a>                       sqrt(2-sin((1/2)*t)^2)/(9-cos(t)^2),
<a name="line_698"></a>		     (((2*I)*sqrt(2)*sin(t)+2*cos(t)+2)*sqrt(2+sin((1/2)*t)^2)+(2*I)*sqrt(2)*sin(t)+4*cos(t)-4)*
<a name="line_699"></a>		       sqrt(2-sin((1/2)*t)^2)/(9-cos(t)^2)];
<a name="line_700"></a>c_TCm[ 8] := (t) -> [(((2*I)*sqrt(2)*sin(t)+2*cos(t)+2)*sqrt(2+sin((1/2)*t)^2)+(2*I)*sqrt(2)*sin(t)+4*cos(t)-4)*
<a name="line_701"></a>                       sqrt(2-sin((1/2)*t)^2)/(9-cos(t)^2),
<a name="line_702"></a>		     ((-(2*I)*sqrt(2)*sin(t)+2*cos(t)+2)*sqrt(2+sin((1/2)*t)^2)+(2*I)*sqrt(2)*sin(t)-4*cos(t)+4)*
<a name="line_703"></a>		       sqrt(2-sin((1/2)*t)^2)/(9-cos(t)^2)];
<a name="line_704"></a>c_TCm[ 9] := (t) -> [(sin(t)+I*cos(t))*(sin(t)+I*sqrt(3))/sqrt(4-cos(t)^2),
<a name="line_705"></a>                     (sin(t)-I*cos(t))*(sin(t)+I*sqrt(3))/sqrt(4-cos(t)^2)];
<a name="line_706"></a>c_TCm[10] := (t) -> [(-(2*I)*sin(t)+sqrt(3)*cos(t))/sqrt(4-cos(t)^2),
<a name="line_707"></a>                      ((2*I)*sin(t)-sqrt(3)*cos(t))/sqrt(4-cos(t)^2)];
<a name="line_708"></a>c_TCm[11] := (t) -> [(-(2*I)*sin(t)+sqrt(3)*cos(t))/sqrt(4-cos(t)^2),
<a name="line_709"></a>                      ((2*I)*sin(t)-sqrt(3)*cos(t))/sqrt(4-cos(t)^2)];
<a name="line_710"></a>c_TCm[12] := (t) -> [(sin(t)+I*cos(t))*(sin(t)+I*sqrt(3))/sqrt(4-cos(t)^2),
<a name="line_711"></a>                     (sin(t)-I*cos(t))*(sin(t)+I*sqrt(3))/sqrt(4-cos(t)^2)];
<a name="line_712"></a>c_TCm[13] := (t) -> [ ( I*sqrt(4-cos(t)^2)*sqrt(3)*cos(t)+2*cos(t)^2-2)/(cos(t)^2+2),
<a name="line_713"></a>                     -(-I*sqrt(4-cos(t)^2)*sqrt(3)*cos(t)+2*cos(t)^2-2)/(cos(t)^2+2)];
<a name="line_714"></a>c_TCm[14] := (t) -> [(I*sqrt(3)*cos(t)-2*sin(t))*(sin(t)*cos(t)^2+(3*I)*cos(t)*sqrt(4-cos(t)^2)-4*sin(t))/
<a name="line_715"></a>                      ((cos(t)^2+2)*(4-cos(t)^2)),
<a name="line_716"></a>		     (I*sqrt(3)*cos(t)+2*sin(t))*(sin(t)*cos(t)^2+(3*I)*cos(t)*sqrt(4-cos(t)^2)-4*sin(t))/
<a name="line_717"></a>		      ((cos(t)^2+2)*(4-cos(t)^2))];
<a name="line_718"></a>c_TCm[15] := (t) -> [(I*sqrt(3)*cos(t)+2*sin(t))*(sin(t)*cos(t)^2+(3*I)*cos(t)*sqrt(4-cos(t)^2)-4*sin(t))/
<a name="line_719"></a>                      ((cos(t)^2+2)*(4-cos(t)^2)),
<a name="line_720"></a>		     (I*sqrt(3)*cos(t)-2*sin(t))*(sin(t)*cos(t)^2+(3*I)*cos(t)*sqrt(4-cos(t)^2)-4*sin(t))/
<a name="line_721"></a>		      ((cos(t)^2+2)*(4-cos(t)^2))];
<a name="line_722"></a>c_TCm[16] := (t) -> [-(-I*sqrt(4-cos(t)^2)*sqrt(3)*cos(t)+2*cos(t)^2-2)/(cos(t)^2+2),
<a name="line_723"></a>                      ( I*sqrt(4-cos(t)^2)*sqrt(3)*cos(t)+2*cos(t)^2-2)/(cos(t)^2+2)];
<a name="line_724"></a>
<a name="line_725"></a>
<a name="line_726"></a>######################################################################
<a name="line_727"></a>######################################################################
<a name="line_728"></a># E_to_TCmq is E_to_TCm followed by an isogeny of degree two.
<a name="line_729"></a>
<a name="line_730"></a><span style="color:red">#@ E_to_TCmq_xyr 
</span><a name="line_731"></a>E_to_TCmq_xyr := [
<a name="line_732"></a> -(2*I*x[1]*y[1] - (1-y[1]^2/2) + y[1]^2*(1 - sqrt(2)*y[2]))*
<a name="line_733"></a>  (2*I*x[2]*y[1] + (1-y[1]^2/2) - y[1]^2*(1 + sqrt(2)*y[2]))/(1-y[1]^2/2)^2,
<a name="line_734"></a>  (2*I*x[1] + y[1]^2/sqrt(2)*(1-y[2]/sqrt(2)) - y[2])*
<a name="line_735"></a>  (2*I*x[2] + y[1]^2/sqrt(2)*(1+y[2]/sqrt(2)) + y[2])/(2*(1-y[1]^2/2)^2*(1-y[2]^2/2))
<a name="line_736"></a>];
<a name="line_737"></a>
<a name="line_738"></a><span style="color:red">#@ E_to_TCmq 
</span><a name="line_739"></a>E_to_TCmq := unapply(eval(subs({y=yx0,r=rx},E_to_TCmq_xyr)),x);
<a name="line_740"></a>
<a name="line_741"></a><span style="color:red">#@ TCm_to_TCmq 
</span><a name="line_742"></a>TCm_to_TCmq := (z) -> [ z[1]/z[2], z[1]*z[2]];
<a name="line_743"></a>
<a name="line_744"></a><span style="color:red">#@ TTC_to_TCmq  
</span><a name="line_745"></a>TTC_to_TCmq  := (z) -> TCm_to_TCmq(TTC_to_TCm(z));
<a name="line_746"></a>
<a name="line_747"></a><span style="color:red">#@ TPm_to_TPmq 
</span><a name="line_748"></a>TPm_to_TPmq := (t) -> [(t[1]-t[2])/(1+t[1]*t[2]),(t[1]+t[2])/(1-t[1]*t[2])];
<a name="line_749"></a>
<a name="line_750"></a><span style="color:red">#@ TTP_to_TPmq  
</span><a name="line_751"></a>TTP_to_TPmq  := (t) -> TPm_to_TPmq(TTP_to_TPm(t));
<a name="line_752"></a>
<a name="line_753"></a><span style="color:red">#@ E_to_TPmq   
</span><a name="line_754"></a>E_to_TPmq   := (x) -> TTP_to_TPmq(E_to_TTP(x));
<a name="line_755"></a>
<a name="line_756"></a><span style="color:red">#@ E_to_TPmq_jacobian 
</span><a name="line_757"></a>E_to_TPmq_jacobian := 2 * E_to_TPm_jacobian;
<a name="line_758"></a>
<a name="line_759"></a>########################################
<a name="line_760"></a>
<a name="line_761"></a># This map is essentially inverse to E_to_TPmq.
<a name="line_762"></a># In more detail, it returns an answer involving 
<a name="line_763"></a># sqrt(RootOf(a quartic polynomial)), where it seems experimentally
<a name="line_764"></a># that the quartic almost always has precisely two nonnegative roots.
<a name="line_765"></a># Using the two possible square roots of these two roots we obtain
<a name="line_766"></a># four points in EX^*, and these are the preimages of a under 
<a name="line_767"></a># E_to_TPmq.
<a name="line_768"></a>#
<a name="line_769"></a># We do not have a full analysis of which preimage we get using the
<a name="line_770"></a># default numerical value of RootOf().  However, experimant shows
<a name="line_771"></a># that the signs of x[3] and 1 - (a[1]*a[2])^2 are relevant, where
<a name="line_772"></a># a = E_to_TPm(x) (not E_to_TPmq(x)).  Alternatively, the sign of
<a name="line_773"></a># z[1]*z[2]+2*z[2]-2*z[1] is relevant.  This sign changes as we 
<a name="line_774"></a># cross C[13].
<a name="line_775"></a>
<a name="line_776"></a><span style="color:red">#@ TPmq_to_E 
</span><a name="line_777"></a>TPmq_to_E := proc(a,i_)
<a name="line_778"></a> local x,xa,p,t,i;
<a name="line_779"></a>
<a name="line_780"></a> i := `if`(nargs > 1,i_,0);
<a name="line_781"></a> 
<a name="line_782"></a> p :=
<a name="line_783"></a>  16*a[2]^2*a[1]^4+
<a name="line_784"></a>  32*a[1]^2*(1+a[1]^2-a[2]^2*a[1]^2)*t+
<a name="line_785"></a>  (-8*a[2]^2*a[1]^4-32*a[2]^2*a[1]^2-96*a[1]^4-160*a[1]^2-64)*t^2+
<a name="line_786"></a>  (24*a[2]^2*a[1]^4+32*a[2]^2*a[1]^2+72*a[1]^4+136*a[1]^2+64)*t^3+
<a name="line_787"></a>  a[2]^2*(3*a[1]^2+4)^2*t^4;
<a name="line_788"></a>
<a name="line_789"></a> if i = 0 then
<a name="line_790"></a>  xa[3] :=  sqrt(RootOf(p,t));
<a name="line_791"></a> elif i = 1 then
<a name="line_792"></a>  xa[3] :=  sqrt(RootOf(p,t,index=1));
<a name="line_793"></a> elif i = 2 then
<a name="line_794"></a>  xa[3] :=  sqrt(RootOf(p,t,index=2));
<a name="line_795"></a> elif i = 3 then
<a name="line_796"></a>  xa[3] := -sqrt(RootOf(p,t,index=1));
<a name="line_797"></a> else
<a name="line_798"></a>  xa[3] := -sqrt(RootOf(p,t,index=2));
<a name="line_799"></a> fi;
<a name="line_800"></a>
<a name="line_801"></a> xa[1] :=  (1/16)*sqrt(2)*(6*sqrt(2)*a[1]^3*xa[3]^3+
<a name="line_802"></a>                           3*a[2]*a[1]^2*xa[3]^4-
<a name="line_803"></a>                           4*sqrt(2)*a[1]^3*xa[3]+
<a name="line_804"></a>                           6*sqrt(2)*a[1]*xa[3]^3+
<a name="line_805"></a>                           4*a[1]^2*a[2]*xa[3]^2+
<a name="line_806"></a>                           4*a[2]*xa[3]^4-
<a name="line_807"></a>                           4*sqrt(2)*a[1]*xa[3]-4*a[2]*a[1]^2)/
<a name="line_808"></a>           (xa[3]^2*(a[1]^2+1));
<a name="line_809"></a>
<a name="line_810"></a> xa[2] := -(1/16)*sqrt(2)*(6*sqrt(2)*a[1]^3*xa[3]^3-
<a name="line_811"></a>                           3*a[2]*a[1]^2*xa[3]^4-
<a name="line_812"></a>                           4*sqrt(2)*a[1]^3*xa[3]+
<a name="line_813"></a>                           6*sqrt(2)*a[1]*xa[3]^3-
<a name="line_814"></a>                           4*a[1]^2*a[2]*xa[3]^2-
<a name="line_815"></a>                           4*a[2]*xa[3]^4-
<a name="line_816"></a>                           4*sqrt(2)*a[1]*xa[3]+
<a name="line_817"></a>                           4*a[2]*a[1]^2)/
<a name="line_818"></a>           (xa[3]^2*(a[1]^2+1));
<a name="line_819"></a>
<a name="line_820"></a> xa[4] := -(1/8)*a[1]*a[2]*(3*a[1]^2*xa[3]^4+4*a[1]^2*xa[3]^2+4*xa[3]^4-4*a[1]^2)/
<a name="line_821"></a>            (xa[3]^2*(a[1]^2+1));
<a name="line_822"></a>
<a name="line_823"></a> return [xa[1],xa[2],xa[3],xa[4]];
<a name="line_824"></a>
<a name="line_825"></a>end:
<a name="line_826"></a>
<a name="line_827"></a>########################################
<a name="line_828"></a>
<a name="line_829"></a><span style="color:red">#@ act_TCmq
</span><a name="line_830"></a>act_TCmq[1]     := (z) -> [  z[1],  z[2]];
<a name="line_831"></a>act_TCmq[LL]    := (z) -> [1/z[1],1/z[2]];
<a name="line_832"></a>act_TCmq[LM]    := (z) -> [  z[1],  z[2]];
<a name="line_833"></a>act_TCmq[LLLM]  := (z) -> [1/z[1],1/z[2]];
<a name="line_834"></a>act_TCmq[LN]    := (z) -> [1/z[1],  z[2]];
<a name="line_835"></a>act_TCmq[LLLN]  := (z) -> [  z[1],1/z[2]];
<a name="line_836"></a>act_TCmq[MN]    := (z) -> [1/z[1],  z[2]];
<a name="line_837"></a>act_TCmq[LLMN]  := (z) -> [  z[1],1/z[2]];
<a name="line_838"></a>
<a name="line_839"></a>########################################
<a name="line_840"></a>
<a name="line_841"></a><span style="color:red">#@ v_TCmq
</span><a name="line_842"></a>for i from 0 to num_vertices_E-1 do
<a name="line_843"></a> v_TCmq[i] := simplify(expand(E_to_TCmq(v_E0[i])));
<a name="line_844"></a>od:
<a name="line_845"></a>
<a name="line_846"></a><span style="color:red">#@ v_TPmq
</span><a name="line_847"></a>v_TPmq[ 0] := [0,0];
<a name="line_848"></a>v_TPmq[ 1] := [0,0];
<a name="line_849"></a>v_TPmq[ 7] := [0,0];
<a name="line_850"></a>v_TPmq[ 9] := [0,0];
<a name="line_851"></a>v_TPmq[ 4] := [0, sqrt(2)];
<a name="line_852"></a>v_TPmq[ 5] := [0, sqrt(2)];
<a name="line_853"></a>v_TPmq[ 2] := [0,-sqrt(2)];
<a name="line_854"></a>v_TPmq[ 3] := [0,-sqrt(2)];
<a name="line_855"></a>v_TPmq[ 6] := [0,infinity];
<a name="line_856"></a>v_TPmq[ 8] := [0,infinity];
<a name="line_857"></a>v_TPmq[10] := [infinity,infinity];
<a name="line_858"></a>v_TPmq[11] := [infinity,infinity];
<a name="line_859"></a>v_TPmq[12] := [infinity,infinity];
<a name="line_860"></a>v_TPmq[13] := [infinity,infinity];
<a name="line_861"></a>v_TPmq[14] := [0, sqrt(3)];
<a name="line_862"></a>v_TPmq[19] := [0, sqrt(3)];
<a name="line_863"></a>v_TPmq[16] := [0,-sqrt(3)];
<a name="line_864"></a>v_TPmq[21] := [0,-sqrt(3)];
<a name="line_865"></a>v_TPmq[15] := [infinity,0];
<a name="line_866"></a>v_TPmq[17] := [infinity,0];
<a name="line_867"></a>v_TPmq[18] := [infinity,0];
<a name="line_868"></a>v_TPmq[20] := [infinity,0];
<a name="line_869"></a>
<a name="line_870"></a><span style="color:red">#@ c_TPmq
</span><a name="line_871"></a>c_TPmq[ 0] := (t) -> [0,-cos(t-Pi/4)/sin(t-Pi/4)^2];
<a name="line_872"></a>c_TPmq[ 1] := (t) -> [ 0,2*sin(t)/cos(t)^2];
<a name="line_873"></a>c_TPmq[ 2] := (t) -> [ sqrt(8)*sin(2*t)/(1 - 3*cos(2*t)),0];
<a name="line_874"></a>c_TPmq[ 3] := (t) -> [sqrt(2/3)/tan(t),-sqrt(2)/sin(t)];
<a name="line_875"></a>c_TPmq[ 4] := (t) -> [sqrt(2/3)/tan(t), sqrt(2)/sin(t)];
<a name="line_876"></a>c_TPmq[ 5] := (t) -> [ sqrt(2)*tan(t/2), sqrt(2)*tan(t/2)/sqrt(2+sin(t/2)^2)];
<a name="line_877"></a>c_TPmq[ 6] := (t) -> [-sqrt(2)*tan(t/2), sqrt(2)*tan(t/2)/sqrt(2+sin(t/2)^2)];
<a name="line_878"></a>c_TPmq[ 7] := (t) -> [-sqrt(2)*tan(t/2), sqrt(2)*tan(t/2)/sqrt(2+sin(t/2)^2)];
<a name="line_879"></a>c_TPmq[ 8] := (t) -> [ sqrt(2)*tan(t/2), sqrt(2)*tan(t/2)/sqrt(2+sin(t/2)^2)];
<a name="line_880"></a>c_TPmq[ 9] := (t) -> [1/tan(t),sqrt(3)/sin(t)];
<a name="line_881"></a>c_TPmq[10] := (t) -> [infinity,sqrt(3)/2/tan(t)];
<a name="line_882"></a>c_TPmq[11] := (t) -> [infinity,sqrt(3)/2/tan(t)];
<a name="line_883"></a>c_TPmq[12] := (t) -> [1/tan(t),sqrt(3)/sin(t)];
<a name="line_884"></a>c_TPmq[13] := (t) -> [ 2/sqrt(3)*sin(t)^2/cos(t)/sqrt(3+sin(t)^2),infinity];
<a name="line_885"></a>c_TPmq[14] := (t) -> [ 2/sqrt(3)*tan(t),tan(t)*sqrt(3+sin(t)^2)/3];
<a name="line_886"></a>c_TPmq[15] := (t) -> [-2/sqrt(3)*tan(t),tan(t)*sqrt(3+sin(t)^2)/3];
<a name="line_887"></a>c_TPmq[16] := (t) -> [-2/sqrt(3)*sin(t)^2/cos(t)/sqrt(3+sin(t)^2),infinity];
<a name="line_888"></a>
<a name="line_889"></a><span style="color:red">#@ c_TCmq
</span><a name="line_890"></a>c_TCmq[ 0] := (t) -> [1, -((4*I)*sqrt(2)*cos(t)^2*sin(t)-(4*I)*sqrt(2)*cos(t)^3-4*cos(t)^4-(2*I)*sqrt(2)*sin(t)+(2*I)*sqrt(2)*cos(t)-8*cos(t)*sin(t)+4*cos(t)^2-1)/(4*cos(t)^4-4*cos(t)^2-3)];
<a name="line_891"></a>c_TCmq[ 1] := (t) -> [1, -((2*I)*sin(t)+cos(t)^2)/((2*I)*sin(t)-cos(t)^2)];
<a name="line_892"></a>c_TCmq[ 2] := (t) -> [-((2*I)*sqrt(2)*cos(t)*sin(t)-3*cos(t)^2+2)/((2*I)*sqrt(2)*cos(t)*sin(t)+3*cos(t)^2-2), 1];
<a name="line_893"></a>c_TCmq[ 3] := (t) -> [-((2*I)*sin(t)*cos(t)*sqrt(6)-5*cos(t)^2+3)/(cos(t)^2-3), ((2*I)*sqrt(2)*sin(t)+cos(t)^2+1)/(cos(t)^2-3)];
<a name="line_894"></a>c_TCmq[ 4] := (t) -> [-((2*I)*sin(t)*cos(t)*sqrt(6)-5*cos(t)^2+3)/(cos(t)^2-3), ((2*I)*sqrt(2)*sin(t)+sin(t)^2-2)/(sin(t)^2+2)];
<a name="line_895"></a>c_TCmq[ 5] := (t) -> [-((2*I)*sqrt(2)*sin(t)+3*cos(t)-1)/(-3+cos(t)),
<a name="line_896"></a>                      -I*(I*cos(t)^2+4*sqrt(5-cos(t))*sin(t)-(8*I)*cos(t)-I)/((-3+cos(t))*(cos(t)+3))];
<a name="line_897"></a>c_TCmq[ 6] := (t) -> [((2*I)*sqrt(2)*sin(t)-3*cos(t)+1)/(-3+cos(t)),
<a name="line_898"></a>                      -I*(I*cos(t)^2+4*sqrt(5-cos(t))*sin(t)-(8*I)*cos(t)-I)/((-3+cos(t))*(cos(t)+3))];
<a name="line_899"></a>c_TCmq[ 7] := (t) -> [((2*I)*sqrt(2)*sin(t)-3*cos(t)+1)/(-3+cos(t)),
<a name="line_900"></a>                      -I*(I*cos(t)^2+4*sqrt(5-cos(t))*sin(t)-(8*I)*cos(t)-I)/((-3+cos(t))*(cos(t)+3))];
<a name="line_901"></a>c_TCmq[ 8] := (t) -> [-((2*I)*sqrt(2)*sin(t)+3*cos(t)-1)/(-3+cos(t)),
<a name="line_902"></a>                      -I*(I*cos(t)^2+4*sqrt(5-cos(t))*sin(t)-(8*I)*cos(t)-I)/((-3+cos(t))*(cos(t)+3))];
<a name="line_903"></a>c_TCmq[ 9] := (t) -> [-cos(2*t)+I*sin(2*t), -((2*I)*sin(t)*sqrt(3)-cos(t)^2-2)/(-4+cos(t)^2)];
<a name="line_904"></a>c_TCmq[10] := (t) -> [-1, -((4*I)*cos(t)*sqrt(3)*sin(t)-7*cos(t)^2+4)/(-4+cos(t)^2)];
<a name="line_905"></a>c_TCmq[11] := (t) -> [-1, -((4*I)*cos(t)*sqrt(3)*sin(t)-7*cos(t)^2+4)/(-4+cos(t)^2)];
<a name="line_906"></a>c_TCmq[12] := (t) -> [-cos(2*t)+I*sin(2*t), -((2*I)*sin(t)*sqrt(3)-cos(t)^2-2)/(-4+cos(t)^2)];
<a name="line_907"></a>c_TCmq[13] := (t) -> [(I*sqrt(4-cos(t)^2)*sqrt(3)*cos(t)+2*cos(t)^2-2)/(I*sqrt(4-cos(t)^2)*sqrt(3)*cos(t)-2*cos(t)^2+2), -1];
<a name="line_908"></a>c_TCmq[14] := (t) -> [-((4*I)*cos(t)*sqrt(3)*sin(t)+7*cos(t)^2-4)/(-4+cos(t)^2),
<a name="line_909"></a>                      ((6*I)*sin(t)*sqrt(4-cos(t)^2)*cos(t)-cos(t)^4+14*cos(t)^2-4)/(4*cos(t)^2+cos(t)^4+4)];
<a name="line_910"></a>c_TCmq[15] := (t) -> [ ((4*I)*cos(t)*sqrt(3)*sin(t)-7*cos(t)^2+4)/(-4+cos(t)^2),
<a name="line_911"></a>                      ((6*I)*sin(t)*sqrt(4-cos(t)^2)*cos(t)-cos(t)^4+14*cos(t)^2-4)/(4*cos(t)^2+cos(t)^4+4)];
<a name="line_912"></a>c_TCmq[16] := (t) -> [(I*sqrt(4-cos(t)^2)*sqrt(3)*cos(t)-2*cos(t)^2+2)/(I*sqrt(4-cos(t)^2)*sqrt(3)*cos(t)+2*cos(t)^2-2), -1];
<a name="line_913"></a>
<a name="line_914"></a># TODO: add checks for c_TCmq[i](t) = c_TCmq_mob[i](cos(t)+I*sin(t))
<a name="line_915"></a><span style="color:red">#@ c_TCmq_mob[ 0] 
</span><a name="line_916"></a>c_TCmq_mob[ 0] := (z) -> [1,-mob(z,(1-sqrt(3))/sqrt(2))*mob(z,(1+sqrt(3))/sqrt(2))*
<a name="line_917"></a>                             mob(z,-I*(1-sqrt(3))/sqrt(2))*mob(z,-I*(1+sqrt(3))/sqrt(2))];
<a name="line_918"></a>c_TCmq_mob[ 1] := (z) -> [1,mob(z,-1+sqrt(2))^2*mob(z,-1-sqrt(2))^2];
<a name="line_919"></a>c_TCmq_mob[ 2] := (z) -> [mob(z, 1+sqrt(2))^2*mob(z,-1-sqrt(2))^2,1];
<a name="line_920"></a>c_TCmq_mob[ 3] := (z) -> [-mob(z, sqrt(3)+sqrt(2))*mob(z,-sqrt(3)-sqrt(2)),
<a name="line_921"></a>                          -mob(z, sqrt(3)-sqrt(2))*mob(z,-sqrt(3)-sqrt(2))];
<a name="line_922"></a>c_TCmq_mob[ 4] := (z) -> [-mob(z, sqrt(3)+sqrt(2))*mob(z,-sqrt(3)-sqrt(2)),
<a name="line_923"></a>                          -mob(z, sqrt(3)+sqrt(2))*mob(z,-sqrt(3)+sqrt(2))];
<a name="line_924"></a>c_TCmq_mob[ 9] := (z) -> [-1/z^2,-mob(z,2+sqrt(3))*mob(z,-2+sqrt(3))];
<a name="line_925"></a>c_TCmq_mob[10] := (z) -> [-1    ,-mob(z,2+sqrt(3))*mob(z,-2-sqrt(3))];
<a name="line_926"></a>c_TCmq_mob[11] := (z) -> [-1    ,-mob(z,2+sqrt(3))*mob(z,-2-sqrt(3))];
<a name="line_927"></a>c_TCmq_mob[12] := (z) -> [-1/z^2,-mob(z,2+sqrt(3))*mob(z,-2+sqrt(3))];
  </pre>
 </body>
</html>
    