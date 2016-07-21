<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a><span style="color:red">#@ PK_vars
</span><a name="line_2"></a>PK_vars := [z,w,seq(t[i],i=0..4),seq(u[i],i=0..4),seq(v[i],i=1..4)];
<a name="line_3"></a>
<a name="line_4"></a><span style="color:red">#@ PK_rels
</span><a name="line_5"></a>PK_rels := [
<a name="line_6"></a> t[4] - 2*t[1]/(1+t[1]^2),
<a name="line_7"></a> t[4] - t[2]^2/(2-t[2]^2),
<a name="line_8"></a> t[4] - t[3]^2/(t[3]^2-2),
<a name="line_9"></a> t[2] - 2*t[0]/(1+t[0]^2),
<a name="line_10"></a> t[3] - 2*I*t[0]/(1-t[0]^2),
<a name="line_11"></a> u[1]^2 - q_Ep( t[2]),
<a name="line_12"></a> u[2]^2 - q_Em( t[3]),
<a name="line_13"></a> u[3]^2 - q_Ep(-t[2]),
<a name="line_14"></a> u[4]^2 - q_Em(-t[3]),
<a name="line_15"></a> v[1]^2 -     q_Ep( t[2]) * (2+t[2])^2/t[2]^6,
<a name="line_16"></a> v[2]^2 - I * q_Em( t[3]) * (2+t[3])^2/t[3]^6,
<a name="line_17"></a> v[3]^2 +     q_Ep(-t[2]) * (2-t[2])^2/t[2]^6,
<a name="line_18"></a> v[4]^2 + I * q_Em(-t[3]) * (2-t[3])^2/t[3]^6
<a name="line_19"></a>];
<a name="line_20"></a>
<a name="line_21"></a><span style="color:red">#@ twz
</span><a name="line_22"></a>twz[ 0] := z;
<a name="line_23"></a>twz[ 1] := z^2;
<a name="line_24"></a>twz[ 2] := 2*z/(1+z^2);
<a name="line_25"></a>twz[ 3] := 2*I*z/(1-z^2);
<a name="line_26"></a>twz[ 4] := 2*z^2/(1+z^4);
<a name="line_27"></a>
<a name="line_28"></a><span style="color:red">#@ uwz
</span><a name="line_29"></a>uwz[ 0] := w;
<a name="line_30"></a>uwz[ 1] := 2*w*(1-z)/(1+z^2)^2;
<a name="line_31"></a>uwz[ 2] := -(1+I)*sqrt(2)*w*(I+z)/(1-z^2)^2;
<a name="line_32"></a>uwz[ 3] := -2*I*w*(1+z)/(1+z^2)^2;
<a name="line_33"></a>uwz[ 4] := -(1-I)*sqrt(2)*w*(I-z)/(1-z^2)^2;
<a name="line_34"></a>
<a name="line_35"></a><span style="color:red">#@ vwz
</span><a name="line_36"></a>vwz[ 1] := w*(1-1/z^3)/2;
<a name="line_37"></a>vwz[ 2] := w*(1-I/z^3)/2;
<a name="line_38"></a>vwz[ 3] := w*(1+1/z^3)/2;
<a name="line_39"></a>vwz[ 4] := w*(1+I/z^3)/2;
<a name="line_40"></a>
<a name="line_41"></a><span style="color:red">#@ wz_reduce
</span><a name="line_42"></a>wz_reduce := (a) -> eval(subs({t = twz,u = uwz,v = vwz},a));
<a name="line_43"></a>
<a name="line_44"></a><span style="color:red">#@ act_PK_rule
</span><a name="line_45"></a>act_PK_rule[1] := {seq(a = a,a in PK_vars)};
<a name="line_46"></a>
<a name="line_47"></a>act_PK_rule[L] := 
<a name="line_48"></a>{z = -z,w = -I*w,
<a name="line_49"></a> t[0] = -t[0],t[1] = t[1], t[2] = -t[2],t[3] = -t[3],t[4] = t[4],
<a name="line_50"></a> u[0] = -I*u[0],u[1] = u[3],u[2] = u[4],u[3] = -u[1],u[4] = -u[2],
<a name="line_51"></a> v[1] = -I*v[3],v[2] = -I*v[4],v[3] = -I*v[1],v[4] = -I*v[2]
<a name="line_52"></a>};
<a name="line_53"></a>
<a name="line_54"></a>act_PK_rule[M] := 
<a name="line_55"></a>{z = 1/z,w = -w/z^3,
<a name="line_56"></a> t[0] = 1/t[0],t[1] = 1/t[1], t[2] = t[2],t[3] = -t[3],t[4] = t[4],
<a name="line_57"></a> u[0] = -u[0]/t[0]^3,u[1] = u[1],u[2] = -u[4],u[3] = -u[3],u[4] = -u[2],
<a name="line_58"></a> v[1] = v[1],v[2] = I*v[4],v[3] = -v[3],v[4] = -I*v[2]
<a name="line_59"></a>};
<a name="line_60"></a>
<a name="line_61"></a><span style="color:red">#@ act_PK
</span><a name="line_62"></a>for T in [1,L,M] do
<a name="line_63"></a> act_PK[T] := subs(_RULE_ = act_PK_rule[T],(a) -> subs(_RULE_,a));
<a name="line_64"></a>od:
<a name="line_65"></a>
<a name="line_66"></a><span style="color:red">#@ make_PK_rule
</span><a name="line_67"></a>make_PK_rule := proc(T,U,TU)
<a name="line_68"></a> global act_PK_rule,act_PK;
<a name="line_69"></a> act_PK_rule[TU] := {seq(v = act_PK[T](act_PK[U](v)),v in PK_vars)};
<a name="line_70"></a> act_PK[TU] := subs(_RULE_=act_PK_rule[TU],eval((a) -> subs(_RULE_,a))); 
<a name="line_71"></a> NULL;
<a name="line_72"></a>end:
<a name="line_73"></a>
<a name="line_74"></a>make_PK_rule(L,L,LL);
<a name="line_75"></a>make_PK_rule(L,LL,LLL);
<a name="line_76"></a>make_PK_rule(L,M,LM);
<a name="line_77"></a>make_PK_rule(L,LM,LLM);
<a name="line_78"></a>make_PK_rule(L,LLM,LLLM);
<a name="line_79"></a>
<a name="line_80"></a><span style="color:red">#@ PK_check_zero
</span><a name="line_81"></a>PK_check_zero := proc(a)
<a name="line_82"></a> local err;
<a name="line_83"></a> err := factor(wz_reduce(a));
<a name="line_84"></a> err := numer(err);
<a name="line_85"></a> err := expand(rem(err,w^2 - r_P(z),w));
<a name="line_86"></a> return evalb(err = 0);
<a name="line_87"></a>end:
<a name="line_88"></a>
<a name="line_89"></a>p_C[2, 3] := (z) -> z^2;
<a name="line_90"></a>p_C[2, 8] := (z) -> 2*z/(1+z^2);
<a name="line_91"></a>p_C[2, 9] := (z) -> 2*I*z/(1-z^2);
<a name="line_92"></a>p_C[2,10] := (z) -> 2*z^2/(1+z^4);
<a name="line_93"></a>p_C[3,10] := (z) -> 2*z/(1+z^2);
<a name="line_94"></a>p_C[8,10] := (z) -> z^2/(2-z^2);
<a name="line_95"></a>p_C[9,10] := (z) -> z^2/(z^2-2);
<a name="line_96"></a>
<a name="line_97"></a>p_S2[2, 3] := unapply([u[1]^2-u[2]^2, 2*u[1]*u[2],2*u[3]] /~ (1+u[3]^2),u);
<a name="line_98"></a>p_S2[2, 8] := unapply([ 2*u[1],-2*u[2]*u[3],u[2]^2-u[3]^2] /~ (1+u[1]^2),u);
<a name="line_99"></a>p_S2[2, 9] := unapply([-2*u[2],-2*u[1]*u[3],u[1]^2-u[3]^2] /~ (1+u[2]^2),u);
<a name="line_100"></a>p_S2[2,10] := unapply([(1+u[3]^2)*(u[1]^2-u[2]^2),-4*u[1]*u[2]*u[3],2*(1+u[1]^2)*(1+u[2]^2)-4] /~ ((1-u[1]^2)^2+(1-u[2]^2)^2),u);
<a name="line_101"></a>p_S2[3,10] := unapply([2*u[1],-2*u[2]*u[3],u[2]^2-u[3]^2] /~ (1+u[1]^2),u);
<a name="line_102"></a>p_S2[8,10] := unapply([ (1-u[3])^2 - 4*(1-u[1]^2), 4*u[1]*u[2],4*(u[1]^2+u[3]-1)]/~((1-u[3])^2+4*(1-u[1]^2)),u);
<a name="line_103"></a>p_S2[9,10] := unapply([-(1-u[3])^2 + 4*(1-u[1]^2),-4*u[1]*u[2],4*(u[1]^2+u[3]-1)]/~((1-u[3])^2+4*(1-u[1]^2)),u);
<a name="line_104"></a>
  </pre>
 </body>
</html>
    