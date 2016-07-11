# disc_delta_proj defines a map from X to the closed unit disc.

#@ disc_delta_proj 
disc_delta_proj := (x::RR_4) -> [
 (x[1]-x[2])/sqrt(2),
 x[3]
]:

#@ disc_delta_lift_aux 
disc_delta_lift_aux := (z::RR_2) -> sqrt((1-z[1]^2-z[2]^2)/(4*a_E^2*z[1]^2*z[2]^2+(2*a_E^2-(1+a_E^2)*z[2]^2)^2)):

#@ disc_delta_lift 
disc_delta_lift := (z::RR_2) -> [
 ((2*a_E^2 - (1+a_E^2)*z[2]^2) * disc_delta_lift_aux(z) + z[1])/sqrt(2),
 ((2*a_E^2 - (1+a_E^2)*z[2]^2) * disc_delta_lift_aux(z) - z[1])/sqrt(2),
 z[2],
 2*a_E*z[1]*z[2]*disc_delta_lift_aux(z)
]:

#@ act_disc_delta
act_disc_delta[1]    := (z::RR_2) -> z;
act_disc_delta[LL]   := (z::RR_2) -> [-z[1], z[2]];
act_disc_delta[LM]   := (z::RR_2) -> [-z[1],-z[2]];
act_disc_delta[LLLM] := (z::RR_2) -> [ z[1],-z[2]];
act_disc_delta[LN]   := (z::RR_2) -> [-z[1], z[2]];
act_disc_delta[LLLN] := (z::RR_2) -> [ z[1], z[2]];
act_disc_delta[MN]   := (z::RR_2) -> [ z[1],-z[2]];
act_disc_delta[LLMN] := (z::RR_2) -> [-z[1],-z[2]];

######################################################################

#@ disc_pi_proj 
disc_pi_proj := (x::RR_4) -> [x[1],x[2]]:

#@ act_disc_pi
act_disc_pi[1]    := (x::RR_2) -> [ x[1], x[2]];
act_disc_pi[L]    := (x::RR_2) -> [-x[2], x[1]];
act_disc_pi[LL]   := (x::RR_2) -> [-x[1],-x[2]];
act_disc_pi[LLL]  := (x::RR_2) -> [ x[2],-x[1]];
act_disc_pi[M]    := (x::RR_2) -> [ x[1],-x[2]];
act_disc_pi[LM]   := (x::RR_2) -> [ x[2], x[1]];
act_disc_pi[LLM]  := (x::RR_2) -> [-x[1], x[2]];
act_disc_pi[LLLM] := (x::RR_2) -> [-x[2],-x[1]];
act_disc_pi[N]    := (x::RR_2) -> [ x[1],-x[2]];
act_disc_pi[LN]   := (x::RR_2) -> [ x[2], x[1]];
act_disc_pi[LLN]  := (x::RR_2) -> [-x[1], x[2]];
act_disc_pi[LLLN] := (x::RR_2) -> [-x[2],-x[1]];
act_disc_pi[MN]   := (x::RR_2) -> [ x[1], x[2]];
act_disc_pi[LMN]  := (x::RR_2) -> [-x[2], x[1]];
act_disc_pi[LLMN] := (x::RR_2) -> [-x[1],-x[2]];
act_disc_pi[LLLMN]:= (x::RR_2) -> [ x[2],-x[1]];

######################################################################

#@ disc_zeta_proj 
disc_zeta_proj := (x::RR_4) -> [(x[3]-x[4])/sqrt(2),x[2]];

#@ act_disc_zeta
act_disc_zeta[1]    := (x::RR_2) -> [ x[1], x[2]];
act_disc_zeta[LL]   := (x::RR_2) -> [ x[1],-x[2]];
act_disc_zeta[M]    := (x::RR_2) -> [-x[1],-x[2]];
act_disc_zeta[LLM]  := (x::RR_2) -> [-x[1], x[2]];
act_disc_zeta[N]    := (x::RR_2) -> [ x[1],-x[2]];
act_disc_zeta[LLN]  := (x::RR_2) -> [ x[1], x[2]];
act_disc_zeta[MN]   := (x::RR_2) -> [-x[1], x[2]];
act_disc_zeta[LLMN] := (x::RR_2) -> [-x[1],-x[2]];

