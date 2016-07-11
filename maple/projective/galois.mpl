#@ PK_vars
PK_vars := [z,w,seq(t[i],i=0..4),seq(u[i],i=0..4),seq(v[i],i=1..4)];

#@ PK_rels
PK_rels := [
 t[4] - 2*t[1]/(1+t[1]^2),
 t[4] - t[2]^2/(2-t[2]^2),
 t[4] - t[3]^2/(t[3]^2-2),
 t[2] - 2*t[0]/(1+t[0]^2),
 t[3] - 2*I*t[0]/(1-t[0]^2),
 u[1]^2 - q_Ep( t[2]),
 u[2]^2 - q_Em( t[3]),
 u[3]^2 - q_Ep(-t[2]),
 u[4]^2 - q_Em(-t[3]),
 v[1]^2 -     q_Ep( t[2]) * (2+t[2])^2/t[2]^6,
 v[2]^2 - I * q_Em( t[3]) * (2+t[3])^2/t[3]^6,
 v[3]^2 +     q_Ep(-t[2]) * (2-t[2])^2/t[2]^6,
 v[4]^2 + I * q_Em(-t[3]) * (2-t[3])^2/t[3]^6
];

#@ twz
twz[ 0] := z;
twz[ 1] := z^2;
twz[ 2] := 2*z/(1+z^2);
twz[ 3] := 2*I*z/(1-z^2);
twz[ 4] := 2*z^2/(1+z^4);

#@ uwz
uwz[ 0] := w;
uwz[ 1] := 2*w*(1-z)/(1+z^2)^2;
uwz[ 2] := -(1+I)*sqrt(2)*w*(I+z)/(1-z^2)^2;
uwz[ 3] := -2*I*w*(1+z)/(1+z^2)^2;
uwz[ 4] := -(1-I)*sqrt(2)*w*(I-z)/(1-z^2)^2;

#@ vwz
vwz[ 1] := w*(1-1/z^3)/2;
vwz[ 2] := w*(1-I/z^3)/2;
vwz[ 3] := w*(1+1/z^3)/2;
vwz[ 4] := w*(1+I/z^3)/2;

#@ wz_reduce
wz_reduce := (a) -> eval(subs({t = twz,u = uwz,v = vwz},a));

#@ act_PK_rule
act_PK_rule[1] := {seq(a = a,a in PK_vars)};

act_PK_rule[L] := 
{z = -z,w = -I*w,
 t[0] = -t[0],t[1] = t[1], t[2] = -t[2],t[3] = -t[3],t[4] = t[4],
 u[0] = -I*u[0],u[1] = u[3],u[2] = u[4],u[3] = -u[1],u[4] = -u[2],
 v[1] = -I*v[3],v[2] = -I*v[4],v[3] = -I*v[1],v[4] = -I*v[2]
};

act_PK_rule[M] := 
{z = 1/z,w = -w/z^3,
 t[0] = 1/t[0],t[1] = 1/t[1], t[2] = t[2],t[3] = -t[3],t[4] = t[4],
 u[0] = -u[0]/t[0]^3,u[1] = u[1],u[2] = -u[4],u[3] = -u[3],u[4] = -u[2],
 v[1] = v[1],v[2] = I*v[4],v[3] = -v[3],v[4] = -I*v[2]
};

#@ act_PK
for T in [1,L,M] do
 act_PK[T] := subs(_RULE_ = act_PK_rule[T],(a) -> subs(_RULE_,a));
od:

#@ make_PK_rule
make_PK_rule := proc(T,U,TU)
 global act_PK_rule,act_PK;
 act_PK_rule[TU] := {seq(v = act_PK[T](act_PK[U](v)),v in PK_vars)};
 act_PK[TU] := subs(_RULE_=act_PK_rule[TU],eval((a) -> subs(_RULE_,a))); 
 NULL;
end:

make_PK_rule(L,L,LL);
make_PK_rule(L,LL,LLL);
make_PK_rule(L,M,LM);
make_PK_rule(L,LM,LLM);
make_PK_rule(L,LLM,LLLM);

#@ PK_check_zero
PK_check_zero := proc(a)
 local err;
 err := factor(wz_reduce(a));
 err := numer(err);
 err := expand(rem(err,w^2 - r_P(z),w));
 return evalb(err = 0);
end:

p_C[2, 3] := (z) -> z^2;
p_C[2, 8] := (z) -> 2*z/(1+z^2);
p_C[2, 9] := (z) -> 2*I*z/(1-z^2);
p_C[2,10] := (z) -> 2*z^2/(1+z^4);
p_C[3,10] := (z) -> 2*z/(1+z^2);
p_C[8,10] := (z) -> z^2/(2-z^2);
p_C[9,10] := (z) -> z^2/(z^2-2);

p_S2[2, 3] := unapply([u[1]^2-u[2]^2, 2*u[1]*u[2],2*u[3]] /~ (1+u[3]^2),u);
p_S2[2, 8] := unapply([ 2*u[1],-2*u[2]*u[3],u[2]^2-u[3]^2] /~ (1+u[1]^2),u);
p_S2[2, 9] := unapply([-2*u[2],-2*u[1]*u[3],u[1]^2-u[3]^2] /~ (1+u[2]^2),u);
p_S2[2,10] := unapply([(1+u[3]^2)*(u[1]^2-u[2]^2),-4*u[1]*u[2]*u[3],2*(1+u[1]^2)*(1+u[2]^2)-4] /~ ((1-u[1]^2)^2+(1-u[2]^2)^2),u);
p_S2[3,10] := unapply([2*u[1],-2*u[2]*u[3],u[2]^2-u[3]^2] /~ (1+u[1]^2),u);
p_S2[8,10] := unapply([ (1-u[3])^2 - 4*(1-u[1]^2), 4*u[1]*u[2],4*(u[1]^2+u[3]-1)]/~((1-u[3])^2+4*(1-u[1]^2)),u);
p_S2[9,10] := unapply([-(1-u[3])^2 + 4*(1-u[1]^2),-4*u[1]*u[2],4*(u[1]^2+u[3]-1)]/~((1-u[3])^2+4*(1-u[1]^2)),u);

