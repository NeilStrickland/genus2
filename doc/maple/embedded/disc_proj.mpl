<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># disc_delta_proj defines a map from X to the closed unit disc.
<a name="line_2"></a>
<a name="line_3"></a><span style="color:red">#@ disc_delta_proj 
</span><a name="line_4"></a>disc_delta_proj := (x::RR_4) -> [
<a name="line_5"></a> (x[1]-x[2])/sqrt(2),
<a name="line_6"></a> x[3]
<a name="line_7"></a>]:
<a name="line_8"></a>
<a name="line_9"></a><span style="color:red">#@ disc_delta_lift_aux 
</span><a name="line_10"></a>disc_delta_lift_aux := (z::RR_2) -> sqrt((1-z[1]^2-z[2]^2)/(4*a_E^2*z[1]^2*z[2]^2+(2*a_E^2-(1+a_E^2)*z[2]^2)^2)):
<a name="line_11"></a>
<a name="line_12"></a><span style="color:red">#@ disc_delta_lift 
</span><a name="line_13"></a>disc_delta_lift := (z::RR_2) -> [
<a name="line_14"></a> ((2*a_E^2 - (1+a_E^2)*z[2]^2) * disc_delta_lift_aux(z) + z[1])/sqrt(2),
<a name="line_15"></a> ((2*a_E^2 - (1+a_E^2)*z[2]^2) * disc_delta_lift_aux(z) - z[1])/sqrt(2),
<a name="line_16"></a> z[2],
<a name="line_17"></a> 2*a_E*z[1]*z[2]*disc_delta_lift_aux(z)
<a name="line_18"></a>]:
<a name="line_19"></a>
<a name="line_20"></a><span style="color:red">#@ act_disc_delta
</span><a name="line_21"></a>act_disc_delta[1]    := (z::RR_2) -> z;
<a name="line_22"></a>act_disc_delta[LL]   := (z::RR_2) -> [-z[1], z[2]];
<a name="line_23"></a>act_disc_delta[LM]   := (z::RR_2) -> [-z[1],-z[2]];
<a name="line_24"></a>act_disc_delta[LLLM] := (z::RR_2) -> [ z[1],-z[2]];
<a name="line_25"></a>act_disc_delta[LN]   := (z::RR_2) -> [-z[1], z[2]];
<a name="line_26"></a>act_disc_delta[LLLN] := (z::RR_2) -> [ z[1], z[2]];
<a name="line_27"></a>act_disc_delta[MN]   := (z::RR_2) -> [ z[1],-z[2]];
<a name="line_28"></a>act_disc_delta[LLMN] := (z::RR_2) -> [-z[1],-z[2]];
<a name="line_29"></a>
<a name="line_30"></a>######################################################################
<a name="line_31"></a>
<a name="line_32"></a><span style="color:red">#@ disc_pi_proj 
</span><a name="line_33"></a>disc_pi_proj := (x::RR_4) -> [x[1],x[2]]:
<a name="line_34"></a>
<a name="line_35"></a><span style="color:red">#@ act_disc_pi
</span><a name="line_36"></a>act_disc_pi[1]    := (x::RR_2) -> [ x[1], x[2]];
<a name="line_37"></a>act_disc_pi[L]    := (x::RR_2) -> [-x[2], x[1]];
<a name="line_38"></a>act_disc_pi[LL]   := (x::RR_2) -> [-x[1],-x[2]];
<a name="line_39"></a>act_disc_pi[LLL]  := (x::RR_2) -> [ x[2],-x[1]];
<a name="line_40"></a>act_disc_pi[M]    := (x::RR_2) -> [ x[1],-x[2]];
<a name="line_41"></a>act_disc_pi[LM]   := (x::RR_2) -> [ x[2], x[1]];
<a name="line_42"></a>act_disc_pi[LLM]  := (x::RR_2) -> [-x[1], x[2]];
<a name="line_43"></a>act_disc_pi[LLLM] := (x::RR_2) -> [-x[2],-x[1]];
<a name="line_44"></a>act_disc_pi[N]    := (x::RR_2) -> [ x[1],-x[2]];
<a name="line_45"></a>act_disc_pi[LN]   := (x::RR_2) -> [ x[2], x[1]];
<a name="line_46"></a>act_disc_pi[LLN]  := (x::RR_2) -> [-x[1], x[2]];
<a name="line_47"></a>act_disc_pi[LLLN] := (x::RR_2) -> [-x[2],-x[1]];
<a name="line_48"></a>act_disc_pi[MN]   := (x::RR_2) -> [ x[1], x[2]];
<a name="line_49"></a>act_disc_pi[LMN]  := (x::RR_2) -> [-x[2], x[1]];
<a name="line_50"></a>act_disc_pi[LLMN] := (x::RR_2) -> [-x[1],-x[2]];
<a name="line_51"></a>act_disc_pi[LLLMN]:= (x::RR_2) -> [ x[2],-x[1]];
<a name="line_52"></a>
<a name="line_53"></a>######################################################################
<a name="line_54"></a>
<a name="line_55"></a><span style="color:red">#@ disc_zeta_proj 
</span><a name="line_56"></a>disc_zeta_proj := (x::RR_4) -> [(x[3]-x[4])/sqrt(2),x[2]];
<a name="line_57"></a>
<a name="line_58"></a><span style="color:red">#@ act_disc_zeta
</span><a name="line_59"></a>act_disc_zeta[1]    := (x::RR_2) -> [ x[1], x[2]];
<a name="line_60"></a>act_disc_zeta[LL]   := (x::RR_2) -> [ x[1],-x[2]];
<a name="line_61"></a>act_disc_zeta[M]    := (x::RR_2) -> [-x[1],-x[2]];
<a name="line_62"></a>act_disc_zeta[LLM]  := (x::RR_2) -> [-x[1], x[2]];
<a name="line_63"></a>act_disc_zeta[N]    := (x::RR_2) -> [ x[1],-x[2]];
<a name="line_64"></a>act_disc_zeta[LLN]  := (x::RR_2) -> [ x[1], x[2]];
<a name="line_65"></a>act_disc_zeta[MN]   := (x::RR_2) -> [-x[1], x[2]];
<a name="line_66"></a>act_disc_zeta[LLMN] := (x::RR_2) -> [-x[1],-x[2]];
<a name="line_67"></a>
  </pre>
 </body>
</html>
    