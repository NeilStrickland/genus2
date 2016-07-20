<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>assume(a_E > 0 and a_E < 1);
<a name="line_2"></a>
<a name="line_3"></a>######################################################################
<a name="line_4"></a>
<a name="line_5"></a># Functions involved in defining the surface X
<a name="line_6"></a>f_1 := (x::RR_4) -> (2*x[2]^2+(x[4]-1-x[3]/a_E)^2); <span style="color:red">#@ f_1 
</span><a name="line_7"></a>f_2 := (x::RR_4) -> (2*x[1]^2+(x[4]-1+x[3]/a_E)^2); <span style="color:red">#@ f_2 
</span><a name="line_8"></a>f   := (x::RR_4) -> f_1(x)*f_2(x); <span style="color:red">#@ f   
</span><a name="line_9"></a>g_0 := (x::RR_4) -> ((1+1/a_E^2)*x[3]^2-2)*x[4]+1/a_E*(x[1]^2-x[2]^2)*x[3]; <span style="color:red">#@ g_0 
</span><a name="line_10"></a>g_1 := (x::RR_4) -> x[2]^2 - x[1]^2 - (a_E+1/a_E)*x[3]*x[4]; <span style="color:red">#@ g_1 
</span><a name="line_11"></a>g   := (x::RR_4) -> (1/a_E^2-1)*x[3]^2*x[4] - 2*(x[1]^2+x[2]^2)*x[4]
<a name="line_12"></a>                     - 2*x[4]^3 + 1/a_E*(x[1]^2-x[2]^2)*x[3]; <span style="color:red">#@ g   
</span><a name="line_13"></a>
<a name="line_14"></a># rho(xx) is the squared norm of xx
<a name="line_15"></a>rho := (x::RR_4) -> add(x[i]^2,i=1..4); <span style="color:red">#@ rho
</span><a name="line_16"></a>
<a name="line_17"></a># simplify_E is a simplification routine which is useful for 
<a name="line_18"></a># tidying up various expressions that arise when studying EX(a).
<a name="line_19"></a># We have told Maple to assume that a_E > 0 and a_E < 1, and
<a name="line_20"></a># Maple is able to deduce many consequences of these inequalities,
<a name="line_21"></a># but not all of them.  In particular, there are various identities
<a name="line_22"></a># between square roots of polynomials that are valid when 0 < a_E < 1
<a name="line_23"></a># but which are not automatically used by Maple.  The function 
<a name="line_24"></a># simplify_E() applies various identities of this type.
<a name="line_25"></a>
<a name="line_26"></a><span style="color:red">#@ simplify_E 
</span><a name="line_27"></a>simplify_E := proc(u)
<a name="line_28"></a> local v;
<a name="line_29"></a> v := simplify(u);
<a name="line_30"></a> v := simplify(subs(sqrt(1-4*a_E^4)=sqrt(1+2*a_E^2) * sqrt(1-2*a_E^2),v));
<a name="line_31"></a> v := simplify(subs(sqrt(2-2*a_E^2)=sqrt(2)*sqrt(1-a_E^2),v));
<a name="line_32"></a> v := simplify(subs(sqrt(1-a_E^4)=sqrt(1+a_E^2) * sqrt(1-a_E^2),v));
<a name="line_33"></a> v := simplify(subs(sqrt(sqrt(2)*sqrt(1-a_E^2)+2*a_E^2) = 
<a name="line_34"></a>                    sqrt(1+a_E^2)*(sqrt(2)+2*sqrt(1-a_E^2))/
<a name="line_35"></a>                    sqrt(sqrt(2)*sqrt(-a_E^2+1)+1)/sqrt(sqrt(2)*sqrt(-a_E^2+1)+2)
<a name="line_36"></a>               ,v));
<a name="line_37"></a> return(v);
<a name="line_38"></a>end:
<a name="line_39"></a>
<a name="line_40"></a><span style="color:red">#@ is_member_E 
</span><a name="line_41"></a>is_member_E := proc(x)
<a name="line_42"></a> type(x,list) and
<a name="line_43"></a> nops(x) = 4 and
<a name="line_44"></a> simplify_E(rho(x) - 1) = 0 and
<a name="line_45"></a> simplify_E(g_0(x)) = 0;
<a name="line_46"></a>end:
<a name="line_47"></a>
<a name="line_48"></a><span style="color:red">#@ is_equal_E 
</span><a name="line_49"></a>is_equal_E := proc(x,y) 
<a name="line_50"></a> evalb(simplify_E(x -~ y) = [0$4]);
<a name="line_51"></a>end:
<a name="line_52"></a>
<a name="line_53"></a>######################################################################
<a name="line_54"></a> 
<a name="line_55"></a><span style="color:red">#@ act_R4
</span><a name="line_56"></a>act_R4[1]    := (x) -> [ x[1], x[2], x[3], x[4]];
<a name="line_57"></a>act_R4[L]    := (x) -> [-x[2], x[1], x[3],-x[4]];
<a name="line_58"></a>act_R4[LL]   := (x) -> [-x[1],-x[2], x[3], x[4]];
<a name="line_59"></a>act_R4[LLL]  := (x) -> [ x[2],-x[1], x[3],-x[4]];
<a name="line_60"></a>act_R4[M]    := (x) -> [ x[1],-x[2],-x[3],-x[4]];
<a name="line_61"></a>act_R4[LM]   := (x) -> [ x[2], x[1],-x[3], x[4]];
<a name="line_62"></a>act_R4[LLM]  := (x) -> [-x[1], x[2],-x[3],-x[4]];
<a name="line_63"></a>act_R4[LLLM] := (x) -> [-x[2],-x[1],-x[3], x[4]];
<a name="line_64"></a>act_R4[N]    := (x) -> [ x[1],-x[2], x[3], x[4]];
<a name="line_65"></a>act_R4[LN]   := (x) -> [ x[2], x[1], x[3],-x[4]];
<a name="line_66"></a>act_R4[LLN]  := (x) -> [-x[1], x[2], x[3], x[4]];
<a name="line_67"></a>act_R4[LLLN] := (x) -> [-x[2],-x[1], x[3],-x[4]];
<a name="line_68"></a>act_R4[MN]   := (x) -> [ x[1], x[2],-x[3],-x[4]];
<a name="line_69"></a>act_R4[LMN]  := (x) -> [-x[2], x[1],-x[3], x[4]];
<a name="line_70"></a>act_R4[LLMN] := (x) -> [-x[1],-x[2],-x[3],-x[4]];
<a name="line_71"></a>act_R4[LLLMN]:= (x) -> [ x[2],-x[1],-x[3], x[4]];
<a name="line_72"></a>
<a name="line_73"></a><span style="color:red">#@ act_E 
</span><a name="line_74"></a>act_E := eval(act_R4):
<a name="line_75"></a>
<a name="line_76"></a>######################################################################
<a name="line_77"></a>
<a name="line_78"></a><span style="color:red">#@ v_E
</span><a name="line_79"></a>v_E[ 0] := [  0,  0,  1, 0]:
<a name="line_80"></a>v_E[ 1] := [  0,  0, -1, 0]:
<a name="line_81"></a>v_E[ 2] := [  1,  0,  0, 0]:
<a name="line_82"></a>v_E[ 3] := [  0,  1,  0, 0]:
<a name="line_83"></a>v_E[ 4] := [ -1,  0,  0, 0]:
<a name="line_84"></a>v_E[ 5] := [  0, -1,  0, 0]:
<a name="line_85"></a>v_E[ 6] := [  1,  1,  0, 0] /~ sqrt(2):
<a name="line_86"></a>v_E[ 7] := [ -1,  1,  0, 0] /~ sqrt(2):
<a name="line_87"></a>v_E[ 8] := [ -1, -1,  0, 0] /~ sqrt(2):
<a name="line_88"></a>v_E[ 9] := [  1, -1,  0, 0] /~ sqrt(2):
<a name="line_89"></a>v_E[10] := [  0,  0,  sqrt((2*a_E^2)/(1+a_E^2)),  sqrt((1-a_E^2)/(1+a_E^2))]:
<a name="line_90"></a>v_E[11] := [  0,  0,  sqrt((2*a_E^2)/(1+a_E^2)), -sqrt((1-a_E^2)/(1+a_E^2))]:
<a name="line_91"></a>v_E[12] := [  0,  0, -sqrt((2*a_E^2)/(1+a_E^2)), -sqrt((1-a_E^2)/(1+a_E^2))]:
<a name="line_92"></a>v_E[13] := [  0,  0, -sqrt((2*a_E^2)/(1+a_E^2)),  sqrt((1-a_E^2)/(1+a_E^2))]:
<a name="line_93"></a>
<a name="line_94"></a>######################################################################
<a name="line_95"></a>
<a name="line_96"></a><span style="color:red">#@ c_E_tau
</span><a name="line_97"></a>c_E_tau[5] := (t) -> -sqrt((1/a_E^2-1)/2) * sin(t/2)^2;
<a name="line_98"></a>
<a name="line_99"></a><span style="color:red">#@ c_E_p
</span><a name="line_100"></a>c_E_p[3] := (t) -> (a_E^2+1)*sin(t)^2+sqrt((a_E^2+1)*(-a_E^2+1+2*a_E^2*sin(t)^2)+(-a_E^2+1)^2*cos(t)^4);
<a name="line_101"></a>c_E_p[5] := unapply((c_E_tau[5](t) - a_E)*(c_E_tau[5](t) - 1/a_E),t);
<a name="line_102"></a>
<a name="line_103"></a><span style="color:red">#@ c_E
</span><a name="line_104"></a>c_E[ 0] := (t) -> [cos(t),sin(t),0,0];
<a name="line_105"></a>c_E[ 1] := (t) -> [sin(t)/sqrt(2),sin(t)/sqrt(2),cos(t),0];
<a name="line_106"></a>c_E[ 2] := unapply(simplify(act_E[L](c_E[1](t))),t);
<a name="line_107"></a>
<a name="line_108"></a>c_E[3] := unapply(
<a name="line_109"></a>[ 0,
<a name="line_110"></a>  sqrt((2*(1-a_E^2)+4*a_E^2*sin(t)^2)/c_E_p[3](t))*sin(t),
<a name="line_111"></a>  sqrt(2/(1+1/a_E^2))*cos(t),
<a name="line_112"></a> -sqrt(2/(1+1/a_E^2))*(1/a_E-a_E+2*a_E*sin(t)^2)/c_E_p[3](t)*cos(t)
<a name="line_113"></a>],t);
<a name="line_114"></a>
<a name="line_115"></a>c_E[ 4] :=  unapply(simplify(act_E[L](c_E[3](t))),t);
<a name="line_116"></a>
<a name="line_117"></a>c_E[5] := unapply(
<a name="line_118"></a>[ (1/a_E^2-1)^(3/4) * 2^(-5/4) * sqrt(a_E*(1+sin(t/2)^2)/c_E_p[5](t)) * sin(t),
<a name="line_119"></a>  0,
<a name="line_120"></a>  sqrt((1 - 2*a_E*c_E_tau[5](t))/c_E_p[5](t)),
<a name="line_121"></a>  sqrt((1 - 2*a_E*c_E_tau[5](t))/c_E_p[5](t)) * c_E_tau[5](t)
<a name="line_122"></a>],t);
<a name="line_123"></a>
<a name="line_124"></a>c_E[ 6] := unapply(simplify(act_E[L](c_E[5](t))),t);
<a name="line_125"></a>c_E[ 7] := unapply(simplify(act_E[M](c_E[5](t))),t);
<a name="line_126"></a>c_E[ 8] := unapply(simplify(act_E[LM](c_E[5](t))),t);
<a name="line_127"></a>
<a name="line_128"></a>######################################################################
<a name="line_129"></a>
<a name="line_130"></a><span style="color:red">#@ c_check_E
</span><a name="line_131"></a>c_check_E[0] := proc(x::RR_4) x[3] = 0 and x[4] = 0; end:
<a name="line_132"></a>c_check_E[1] := proc(x::RR_4) simplify(x[1]-x[2]) = 0 and simplify(x[4]) = 0; end:
<a name="line_133"></a>c_check_E[2] := proc(x::RR_4) simplify(x[1]+x[2]) = 0 and simplify(x[4]) = 0; end:
<a name="line_134"></a>
<a name="line_135"></a>c_check_E[3] := proc(x::RR_4) 
<a name="line_136"></a> evalb(simplify(x[1]) = 0 and is(simplify( g_1(x)) > 0));
<a name="line_137"></a>end:
<a name="line_138"></a>
<a name="line_139"></a>c_check_E[4] := proc(x::RR_4) 
<a name="line_140"></a> evalb(simplify(x[2]) = 0 and is(simplify(-g_1(x)) > 0));
<a name="line_141"></a>end:
<a name="line_142"></a>
<a name="line_143"></a>c_check_E[5] := proc(x::RR_4) 
<a name="line_144"></a> evalb(simplify(x[2]) = 0 and is(simplify(-g_1(x)) <= 0) and is(x[3] >= 0));
<a name="line_145"></a>end:
<a name="line_146"></a>
<a name="line_147"></a>c_check_E[6] := proc(x::RR_4) 
<a name="line_148"></a> local y;
<a name="line_149"></a> y := simplify(y_proj(x));
<a name="line_150"></a> evalb(simplify(x[1]) = 0 and is(simplify( g_1(x)) <= 0) and is(x[3] >= 0));
<a name="line_151"></a>end:
<a name="line_152"></a>
<a name="line_153"></a>c_check_E[7] := proc(x::RR_4) 
<a name="line_154"></a> local y;
<a name="line_155"></a> y := simplify(y_proj(x));
<a name="line_156"></a> evalb(simplify(x[2]) = 0 and is(simplify(-g_1(x)) <= 0) and is(x[3] <= 0));
<a name="line_157"></a>end:
<a name="line_158"></a>
<a name="line_159"></a>c_check_E[8] := proc(x::RR_4) 
<a name="line_160"></a> local y;
<a name="line_161"></a> y := simplify(y_proj(x));
<a name="line_162"></a> evalb(simplify(x[1]) = 0 and is(simplify( g_1(x)) <= 0) and is(x[3] <= 0));
<a name="line_163"></a>end:
<a name="line_164"></a>
<a name="line_165"></a>######################################################################
<a name="line_166"></a>
<a name="line_167"></a><span style="color:red">#@ c_param_E
</span><a name="line_168"></a>c_param_E[0] := (x::RR_4) -> arctan(x[2],x[1]);
<a name="line_169"></a>c_param_E[1] := (x::RR_4) -> arctan(sqrt(2)*x[2],x[3]);
<a name="line_170"></a>c_param_E[2] := (x::RR_4) -> arctan(sqrt(2)*x[2],x[3]);
<a name="line_171"></a>c_param_E[3] := proc(x::RR_4)
<a name="line_172"></a> local S,C,m;
<a name="line_173"></a> C := x[3]/sqrt(2)/sqrt(a_E/(a_E+1/a_E));
<a name="line_174"></a> m := sqrt((-2*a_E^2+2+4*a_E^2*(1-C^2))/((a_E^2+1)*(1-C^2)+sqrt((a_E^2+1)*(-a_E^2+1+2*a_E^2*(1-C^2))+(-a_E^2+1)^2*C^4)));
<a name="line_175"></a> S := x[2]/m;
<a name="line_176"></a> arctan(S,C);
<a name="line_177"></a>end:
<a name="line_178"></a>
<a name="line_179"></a>c_param_E[4] := (x::RR_4) -> c_param_E[3](act_R4[LLL](x));
<a name="line_180"></a>
<a name="line_181"></a>c_param_E[5] := proc(x::RR_4)
<a name="line_182"></a> local y2,m,S,C;
<a name="line_183"></a>
<a name="line_184"></a> y2 := (1/2)*(-x[1]^2+x[2]^2-(a_E+1/a_E)*x[3]*x[4])/a_E:
<a name="line_185"></a> C := 1-2*y2*sqrt(2)/sqrt((1/a_E-a_E)/a_E):
<a name="line_186"></a> m := sqrt(sqrt(2)*sqrt((1/a_E-a_E)/a_E)*(1/a_E-a_E)*(3/2-C/2)/((-(1/2)*sqrt(2)*sqrt((1/a_E-a_E)/a_E)*(1-C)/2-a_E)*(-(1/2)*sqrt(2)*sqrt((1/a_E-a_E)/a_E)*(1-C)/2-1/a_E))):
<a name="line_187"></a> S := x[1]*4/sqrt(2)/m:
<a name="line_188"></a>
<a name="line_189"></a>arctan(S,C);
<a name="line_190"></a>end:
<a name="line_191"></a>
<a name="line_192"></a>c_param_E[6] := (x::RR_4) -> c_param_E[5](act_R4[LLL](x));
<a name="line_193"></a>c_param_E[7] := (x::RR_4) -> c_param_E[5](act_R4[M  ](x));
<a name="line_194"></a>c_param_E[8] := (x::RR_4) -> c_param_E[5](act_R4[LM ](x));
<a name="line_195"></a>
<a name="line_196"></a>######################################################################
<a name="line_197"></a>
<a name="line_198"></a># The functions below are designed for use with symbolic arguments.
<a name="line_199"></a># They return FAIL if Maple is unable to decide whether the relevant
<a name="line_200"></a># inequalities hold.  Similar functions for use with numerical 
<a name="line_201"></a># arguments (after setting a numerical value for a_E) are defined
<a name="line_202"></a># in EX0.mpl.
<a name="line_203"></a>
<a name="line_204"></a><span style="color:red">#@ is_in_F4_E 
</span><a name="line_205"></a>is_in_F4_E := proc(x::RR_4)
<a name="line_206"></a> if is(x[1] >= 0) and is(x[2] >= 0) then
<a name="line_207"></a>  return true;
<a name="line_208"></a> elif is(x[1] < 0) or is(x[2] < 0) then
<a name="line_209"></a>  return false;
<a name="line_210"></a> else
<a name="line_211"></a>  return FAIL;
<a name="line_212"></a> fi;
<a name="line_213"></a>end:
<a name="line_214"></a>
<a name="line_215"></a><span style="color:red">#@ is_in_F16_E 
</span><a name="line_216"></a>is_in_F16_E := proc(x::RR_4)
<a name="line_217"></a> if is(x[1] >= 0) and is(x[2] >= 0) and
<a name="line_218"></a>    is(x[3] >= 0) and is(simplify(g_1(x)) >= 0) then
<a name="line_219"></a>  return true;
<a name="line_220"></a> elif is(x[1] < 0) or is(x[2] < 0) and
<a name="line_221"></a>      is(x[3] < 0) or is(simplify(g_1(x)) < 0) then
<a name="line_222"></a>  return false;
<a name="line_223"></a> else
<a name="line_224"></a>  return FAIL;
<a name="line_225"></a> fi;
<a name="line_226"></a>end:
<a name="line_227"></a>
<a name="line_228"></a><span style="color:red">#@ retract_F4_E 
</span><a name="line_229"></a>retract_F4_E := (x::RR_4) -> [abs(x[1]),abs(x[2]),x[3],x[4]];
<a name="line_230"></a>
<a name="line_231"></a><span style="color:red">#@ retract_F16_E 
</span><a name="line_232"></a>retract_F16_E := proc(x::RR_4)
<a name="line_233"></a> if is(g_1(x) >= 0) then 
<a name="line_234"></a>  return [abs(x[1]),abs(x[2]),abs(x[3]),-abs(x[4])];
<a name="line_235"></a> elif is(g_1(x) <= 0) then 
<a name="line_236"></a>  return [abs(x[2]),abs(x[1]),abs(x[3]),-abs(x[4])];
<a name="line_237"></a> else
<a name="line_238"></a>  return FAIL;
<a name="line_239"></a> fi;
<a name="line_240"></a>end:
<a name="line_241"></a>
<a name="line_242"></a>retract_F16_E_alt := (x) ->
<a name="line_243"></a> ((abs(x[1])+abs(x[2]))/2) *~ [1,1,0,0] +~
<a name="line_244"></a> (signum(x[3])*signum(x[4]) * (abs(x[1])-abs(x[2]))/2) *~ [-1,1,0,0] +~
<a name="line_245"></a> [0,0,abs(x[3]),-abs(x[4])]:
<a name="line_246"></a>
  </pre>
 </body>
</html>
    