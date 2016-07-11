check_ellquot := proc()
 local errs,JMp,JMm,rels,i,k,kt,t0,v0,w0,x0,y0,z0;

 printf("%a()\n",procname);

 JMp := Matrix([seq([seq(diff(Ep_rel[i](z),z[j]),j=1..4)],i=0..1)]);
 rels := factor([Ep_rel[0](z),Ep_rel[1](z),seq(seq(Determinant(SubMatrix(JMp,[1,2],[i,j])),j=i+1..4),i=1..3)]):
 _ASSERT(
  [seq(solve([z[i]-1,op(rels)],[z[1],z[2],z[3],z[4]]),i=1..4)] = [[]$4],
  "Jacobian criterion of smoothness for Ep"
 );

 JMm := Matrix([seq([seq(diff(Em_rel[i](z),z[j]),j=1..4)],i=0..1)]);
 rels := factor([Em_rel[0](z),Em_rel[1](z),seq(seq(Determinant(SubMatrix(JMm,[1,2],[i,j])),j=i+1..4),i=1..3)]):
 _ASSERT(
  [seq(solve([z[i]-1,op(rels)],[z[1],z[2],z[3],z[4]]),i=1..4)] = [[]$4],
  "Jacobian criterion of smoothness for Em"
 );

 _ASSERT(
  map(e -> map(NF_P,e),is_member_Ep(P_to_Ep(z))),
  "P_to_Ep lands in Ep"
 );

 _ASSERT(
  map(e -> map(NF_P,e),is_member_Em(P_to_Em(z))),
  "P_to_Em lands in Em"
 );

 _ASSERT(`and`(seq(is_equal_Ep(v_Ep[i],P_to_Ep(v_P[i])),i in {0,seq(j,j=0..13)})),
  "P_to_Ep sends vertices to vertices"
 );

 _ASSERT(`and`(seq(is_equal_Em(v_Em[i],P_to_Em(v_P[i])),i in {0,seq(j,j=0..13)})),
  "P_to_Em sends vertices to vertices"
 );

 _ASSERT(
  `and`(
   seq(map(NF_P,is_equal_Ep_list(P_to_Ep(act_P[T](z)),act_Ep[T](P_to_Ep(z)))) = [0$6],
       T in [1,LL,M,LLM]),
   seq(map(NF_P,map(conjugate,is_equal_Ep_list(P_to_Ep(act_P[T](z)),act_Ep[T](P_to_Ep(z))))) = [0$6],
       T in [N,LLN,MN,LLMN])),
   "equivariance properties of P_to_Ep"
 );

 _ASSERT(
  `and`(
   seq(map(NF_P,is_equal_Em_list(P_to_Em(act_P[T](z)),act_Em[T](P_to_Em(z)))) = [0$6],
       T in [1,LL,LM,LLLM]),
   seq(map(NF_P,map(conjugate,is_equal_Em_list(P_to_Em(act_P[T](z)),act_Em[T](P_to_Em(z))))) = [0$6],
       T in [LN,LLLN,MN,LLMN])),
   "equivariance properties of P_to_Em"
 );

 _ASSERT(
  factor(Ep_rel[0](j_Ep([y,x])) - is_member_Ep_0([y,x])) = 0 and
  factor(Ep_rel[1](j_Ep([y,x]))) = 0,
  "j sends Ep_0 to Ep"
 );

 _ASSERT(
  factor(Em_rel[0](j_Em([y,x])) - is_member_Em_0([y,x])) = 0 and
  factor(Em_rel[1](j_Em([y,x]))) = 0,
  "j sends Em_0 to Em"
 );

 _ASSERT(
  factor(is_member_Ep_0(P_to_Ep_0([w,z])) - 4*  (1-z)^2/(1+z^2)^4*(w^2 - r_P(z))) = 0,
  "P_to_Ep_0 lands in Ep_0"
 );

 _ASSERT(
  factor(is_member_Em_0(P_to_Em_0([w,z])) - 4*I*(I+z)^2/(1-z^2)^4*(w^2 - r_P(z))) = 0,
  "P_to_Em_0 lands in Em_0"
 );

 errs := {seq(P_to_Ep_0(act_P_0[T]([w,z])) -~ act_Ep_0[T](P_to_Ep_0([w,z])),
          T in {1,LL,M,LLM,N,LLN,MN,LLMN})};
 errs := factor(expand(factor(expand(errs))));

 _ASSERT(errs = {[0,0]},
  "Equivariance properties of P_to_Ep_0" 
 );

 errs := {seq(P_to_Em_0(act_P_0[T]([w,z])) -~ act_Em_0[T](P_to_Em_0([w,z])),
          T in {1,LL,LM,LLLM,LN,LLLN,MN,LLMN})};
 errs := factor(expand(factor(expand(errs))));

 _ASSERT(errs = {[0,0]},
  "Equivariance properties of P_to_Em_0" 
 );

 w0,z0 := op(Ep_to_P_0([y0,x0]));
 _ASSERT(
  factor(subs(y0 = sqrt(q_Ep(x0)),r_P(z0) - w0^2)) = 0,
  "Ep_to_P_0 lands in P_0"
 );
 
 _ASSERT(
  simplify(act_P_0[M]([w0,z0]) -~ subs(sqrt(1-x0^2)=-sqrt(1-x0^2),[w0,z0])) = [0,0],
  "Ep_to_P_0 interacts as expected with action of M"
 );
 
 _ASSERT(
  factor(P_to_Ep_0(Ep_to_P_0([y0,x0])) -~ [y0,x0]) = [0,0],
  "P_to_Ep_0 o Ep_to_P_0 = 1"
 );

 w0,z0 := op(Em_to_P_0([y0,x0]));
 _ASSERT(
  factor(subs(y0 = sqrt(q_Em(x0)),r_P(z0) - w0^2)) = 0,
  "Em_to_P_0 lands in P_0"
 );
  
 _ASSERT(
  simplify(act_P_0[LM]([w0,z0]) -~ subs(sqrt(1-x0^2)=-sqrt(1-x0^2),[w0,z0])) = [0,0],
  "Ep_to_P_0 interacts as expected with action of LM"
 );
  
 _ASSERT(
  factor(P_to_Em_0(Em_to_P_0([y0,x0])) -~ [y0,x0]) = [0,0],
  "P_to_Em_0 o Em_to_P_0 = 1"
 );  
 
 _ASSERT(
  {seq(simplify(is_member_Ep_0(v_Ep_0[i])), i in {0,1,2,3,4,5,10,11,12,13})} = {0},
  "vertices in the affine curve Ep_0"
 );

 for i in [0,2,3,4,5,10,11,12,13] do 
  _ASSERT(factor(P_to_Ep_0(pq_P(v_P[i])) -~ v_Ep_0[i]) = [0,0],
         sprintf("formula for v_EPp_0[%d]",i));
 od;

 kt[ 1] := [1,   Pi  ];
 kt[ 6] := [0,   Pi/4];
 kt[ 7] := [0, 3*Pi/4];
 kt[ 8] := [0,-3*Pi/4];
 kt[ 9] := [0,  -Pi/4];
 
 for i in [1,6,7,8,9] do
  k,t0 := op(kt[i]);
  v0 := map(limit,P_to_Ep_0(pq_P(c_P[k](t))),t=t0,complex);
  v0 := map(u -> `if`(type(u,cx_infinity),infinity,u),v0);
  _ASSERT(v_on_c[i,k] = t0 and v0 = v_Ep_0[i],
           sprintf("formula for v_Ep_0[%d]",i));
 od:

 _ASSERT(
  {seq(simplify(is_member_Em_0(v_Em_0[i])), i in {0,1,6,7,8,9,10,11,12,13})} = {0},
  "vertices in the affine curve Ep_0"
 );

 for i in [0,6,7,8,9,10,11,12,13] do 
  _ASSERT(factor(P_to_Em_0(pq_P(v_P[i])) -~ v_Em_0[i]) = [0,0],
         sprintf("formula for v_Em_0[%d]",i));
 od;

 kt[ 1] := [1,  Pi  ];
 kt[ 2] := [0,  0   ];
 kt[ 3] := [0,  Pi/2];
 kt[ 4] := [0,  Pi  ];
 kt[ 5] := [0, -Pi/2];
 
 for i in [1,2,3,4,5] do
  k,t0 := op(kt[i]);
  v0 := map(limit,P_to_Em_0(pq_P(c_P[k](t))),t=t0,complex);
  v0 := map(u -> `if`(type(u,cx_infinity),infinity,u),v0);
  _ASSERT(v_on_c[i,k] = t0 and v0 = v_Em_0[i],
          sprintf("formula for v_Em_0[%d]",i));
 od:

 _ASSERT(
  is(convert(series(P_to_Ep_0(pq_P(c_P[5](t)))[1],t=0,3),polynom,t)/t > 0) and
  is(convert(series(P_to_Ep_0(pq_P(c_P[1](t)))[1],t=0,3),polynom,t)/(1+I)/t > 0),
  "P_to_Ep_0 to first order near v[0]");

 _ASSERT(
  is(convert(series(P_to_Em_0(pq_P(c_P[5](t)))[1],t=0,3),polynom,t)/(1-I)/t > 0) and
  is(convert(series(P_to_Em_0(pq_P(c_P[1](t)))[1],t=0,3),polynom,t)/t > 0),
  "P_to_Em_0 to first order near v[0]");

 _ASSERT(
 simplify(P_to_Ep_0(pq_P(c_P[5](t)))[2] - ( 2/(a_P*sin(t/2)^2 + 1/(a_P*sin(t/2)^2))))=0,
  "x-coordinate of c[5] in Ep_0"
 );
 
 _ASSERT(
 simplify(P_to_Ep_0(pq_P(c_P[6](t)))[2] - (-2/(a_P*sin(t/2)^2 + 1/(a_P*sin(t/2)^2))))=0,
  "x-coordinate of c[6] in Ep_0"
 );

 _ASSERT(
 simplify(P_to_Ep_0(pq_P(c_P[7](t)))[2] - ( 2/(a_P*sin(t/2)^2 + 1/(a_P*sin(t/2)^2))))=0,
  "x-coordinate of c[7] in Ep_0"
 );

 _ASSERT(
 simplify(P_to_Ep_0(pq_P(c_P[8](t)))[2] - (-2/(a_P*sin(t/2)^2 + 1/(a_P*sin(t/2)^2))))=0,
  "x-coordinate of c[8] in Ep_0"
 );

end:

add_check(check_ellquot):

######################################################################

check_ellquot_origin := proc()

 printf("%a()\n",procname);

 _ASSERT(
  simplify(map(convert,map(series,j_inv_P(c_Ep[5](t)),t=0,2),polynom,t) -~ [sqrt(a_P)*t,0]) = [0,0],
  "c_Ep[5] near the origin"
 );
 
 _ASSERT(
  simplify(map(convert,map(series,j_inv_P(c_Ep[1](t)),t=0,2),polynom,t) -~ [exp(I*Pi/4)*t,0]) = [0,0],
  "c_Ep[1] near the origin"
 );
 
 _ASSERT(
  simplify(map(convert,map(series,j_inv_P(c_Em[5](t)),t=0,2),polynom,t) -~ [exp(-I*Pi/4)*sqrt(a_P)*t,0]) = [0,0],
  "c_Em[5] near the origin"
 );

 _ASSERT(
  simplify(map(convert,map(series,j_inv_P(c_Em[1](t)),t=0,2),polynom,t) -~ [t,0]) = [0,0],
  "c_Em[1] near the origin"
 );  

end:

add_check(check_ellquot_origin);

######################################################################

check_translations := proc()
 local i;
 
 printf("%a()\n",procname);

 _ASSERT(`and`(seq(factor(is_member_Ep(Ep_e[i])),i=0..3),
               seq(factor(is_member_Em(Em_e[i])),i=0..3)),
  "Points Ep_e[i] and Em_e[i] lie in Ep and Em"
 );

 _ASSERT(
  {seq(seq(NF_Ep(factor(Ep_rel[i](Ep_trans[j](z)))),i=0..1),j=0..3)} = {0},
  "translations preserve Ep"
 );

 _ASSERT(
  {seq(seq(NF_Em(factor(Em_rel[i](Em_trans[j](z)))),i=0..1),j=0..3)} = {0},
  "translations preserve Em"
 );

 _ASSERT(
  {factor(Ep_trans[1](Ep_trans[1](z)) -~ Ep_trans[0](z)),
   factor(Ep_trans[2](Ep_trans[2](z)) -~ Ep_trans[0](z)),
   factor(Ep_trans[3](Ep_trans[3](z)) -~ Ep_trans[0](z)),
   factor(Ep_trans[1](Ep_trans[2](z)) -~ Ep_trans[3](z)),
   factor(Ep_trans[2](Ep_trans[3](z)) -~ Ep_trans[1](z)),
   factor(Ep_trans[3](Ep_trans[1](z)) -~ Ep_trans[2](z))} = {[0$4]},
  "translations of Ep compose correctly"
 );

 _ASSERT(
  {factor(Em_trans[1](Em_trans[1](z)) -~ Em_trans[0](z)),
   factor(Em_trans[2](Em_trans[2](z)) -~ Em_trans[0](z)),
   factor(Em_trans[3](Em_trans[3](z)) -~ Em_trans[0](z)),
   factor(Em_trans[1](Em_trans[2](z)) -~ Em_trans[3](z)),
   factor(Em_trans[2](Em_trans[3](z)) -~ Em_trans[1](z)),
   factor(Em_trans[3](Em_trans[1](z)) -~ Em_trans[2](z))} = {[0$4]},
  "translations of Em compose correctly"
 );

 for i from 1 to 3 do
  _ASSERT(is_equal_Ep(Ep_trans[i](Ep_e[0]),Ep_e[i]),
          sprintf("Ep_trans[%d] sends e[0] to e[%d]",i,i));
  _ASSERT(is_equal_Em(Em_trans[i](Em_e[0]),Em_e[i]),
          sprintf("Em_trans[%d] sends e[0] to e[%d]",i,i));
 od:
 
 _ASSERT(
  `and`(seq(is_equal_Ep(Ep_trans[i](j_Ep([y,x])),j_Ep(Ep_0_trans[i]([y,x]))),i=0..3)),
  "translations on Ep and Ep_0 are compatible"
 );
 
 _ASSERT(
  `and`(seq(is_equal_Em(Em_trans[i](j_Em([y,x])),j_Em(Em_0_trans[i]([y,x]))),i=0..3)),
  "translations on Em and Em_0 are compatible"
 );  

 _ASSERT({seq(factor(is_member_Ep_0(Ep_0_e[i])),i=0..3),
          seq(factor(is_member_Em_0(Em_0_e[i])),i=0..3)} = {0},
  "Points Ep_0_e[i] and Em_e_0[i] lie in Ep_0 and Em_0"
 );

 _ASSERT(
  factor(is_member_Ep_0(Ep_0_trans[1]([y,x])) - bm_P^4/(bp_P^2*x-1)^4*is_member_Ep_0([y,x])) = 0 and
  factor(is_member_Ep_0(Ep_0_trans[2]([y,x])) - (4*(1-1/bp_P)^2/(bp_P*x+1-2*x)^4*is_member_Ep_0([y,x]))) = 0 and
  factor(is_member_Ep_0(Ep_0_trans[3]([y,x])) - (4*(1+1/bp_P)^2/(bp_P*x-1+2*x)^4*is_member_Ep_0([y,x]))) = 0,
  "Ep_0_trans[i] preserves Ep_0"
 );

 _ASSERT(
  factor(is_member_Em_0(Em_0_trans[1]([y,x])) - bp_P^4/(bm_P^2*x+1)^4*is_member_Em_0([y,x])) = 0 and
  factor(is_member_Em_0(Em_0_trans[2]([y,x])) - 4*(1-I/bm_P)^2/(bm_P*x+I-2*I*x)^4*is_member_Em_0([y,x])) = 0 and
  factor(is_member_Em_0(Em_0_trans[3]([y,x])) - 4*(1+I/bm_P)^2/(bm_P*x-I+2*I*x)^4*is_member_Em_0([y,x])) = 0,
  "Em_0_trans[i] preserves Em_0"
 );

 _ASSERT({
   factor(Ep_0_trans[1](Ep_0_e[0]) -~ Ep_0_e[1]),
   factor(Ep_0_trans[1](Ep_0_e[1]) -~ Ep_0_e[0]),
   factor(Ep_0_trans[1](Ep_0_e[2]) -~ Ep_0_e[3]),
   factor(Ep_0_trans[1](Ep_0_e[3]) -~ Ep_0_e[2]),
   factor(Ep_0_trans[2](Ep_0_e[0]) -~ Ep_0_e[2]),
   factor(Ep_0_trans[2](Ep_0_e[1]) -~ Ep_0_e[3]),
   factor(Ep_0_trans[2](Ep_0_e[2]) -~ Ep_0_e[0]),
   factor(Ep_0_trans[2](Ep_0_e[3]) -~ Ep_0_e[1]),
   factor(Ep_0_trans[3](Ep_0_e[0]) -~ Ep_0_e[3]),
   factor(Ep_0_trans[3](Ep_0_e[1]) -~ Ep_0_e[2]),
   factor(Ep_0_trans[3](Ep_0_e[2]) -~ Ep_0_e[1]),
   factor(Ep_0_trans[3](Ep_0_e[3]) -~ Ep_0_e[0]),
   factor(Em_0_trans[1](Em_0_e[0]) -~ Em_0_e[1]),
   factor(Em_0_trans[1](Em_0_e[1]) -~ Em_0_e[0]),
   factor(Em_0_trans[1](Em_0_e[2]) -~ Em_0_e[3]),
   factor(Em_0_trans[1](Em_0_e[3]) -~ Em_0_e[2]),
   factor(Em_0_trans[2](Em_0_e[0]) -~ Em_0_e[2]),
   factor(Em_0_trans[2](Em_0_e[1]) -~ Em_0_e[3]),
   factor(Em_0_trans[2](Em_0_e[2]) -~ Em_0_e[0]),
   factor(Em_0_trans[2](Em_0_e[3]) -~ Em_0_e[1]),
   factor(Em_0_trans[3](Em_0_e[0]) -~ Em_0_e[3]),
   factor(Em_0_trans[3](Em_0_e[1]) -~ Em_0_e[2]),
   factor(Em_0_trans[3](Em_0_e[2]) -~ Em_0_e[1]),
   factor(Em_0_trans[3](Em_0_e[3]) -~ Em_0_e[0])
  } = {[0,0]},
  "Action of Ex_0_trans[i] on Ex_0_e[j]"
 );

 _ASSERT(
  {seq(factor(diff(Ep_0_trans[i]([y,x])[2],x)/Ep_0_trans[i]([y,x])[1] - 1/y),i=0..3)} = {0},
  "Ep translations preserve the invariant differential"
 );
 
 _ASSERT(
  {seq(factor(diff(Em_0_trans[i]([y,x])[2],x)/Em_0_trans[i]([y,x])[1] - 1/y),i=0..3)} = {0},
  "Em translations preserve the invariant differential"
 );
  
 _ASSERT(`and`(
  is_equal_Ep(Ep_trans[1](v_Ep[0]),v_Ep[ 3]),
  is_equal_Ep(Ep_trans[2](v_Ep[0]),v_Ep[11]),
  is_equal_Ep(Ep_trans[3](v_Ep[0]),v_Ep[10]),
  is_equal_Em(Em_trans[1](v_Em[0]),v_Em[ 7]),
  is_equal_Em(Em_trans[2](v_Em[0]),v_Em[11]),
  is_equal_Em(Em_trans[3](v_Em[0]),v_Em[10])),
  "action of translations on some vertices"
 );

end:

add_check(check_translations):

######################################################################

check_isogenies := proc()
 local i,x,y,ww;
 
 printf("%a()\n",procname);

 _ASSERT(
  [seq(solve([z[i]-1,op(Ep_to_Em(z)),op(Ep_rels)],
             [z[1],z[2],z[3],z[4]]),i=1..4)] = [[]$4],
  "Ep_to_Em is globally defined"
 );

 _ASSERT(
  [seq(solve([z[i]-1,op(Em_to_Em(z)),op(Em_rels)],
             [z[1],z[2],z[3],z[4]]),i=1..4)] = [[]$4],
  "Em_to_Ep is globally defined"
 );

 _ASSERT([seq(NF_Ep(expand(Em_rel[i](Ep_to_Em(z)))),i=0..1)] = [0,0],
  "Ep_to_Em lands in Em"
 );

 _ASSERT([seq(NF_Em(expand(Ep_rel[i](Em_to_Ep(z)))),i=0..1)] = [0,0],
  "Em_to_Ep lands in Ep"
 );

 _ASSERT(
  is_equal_Em(Ep_to_Em(z),Ep_to_Em(Ep_trans[1](z))),
  "Ep_to_Em is invariant under translation by Ep_e[1]"
 );

 _ASSERT(
  is_equal_Ep(Em_to_Ep(z),Em_to_Ep(Em_trans[1](z))),
  "Em_to_Ep is invariant under translation by Em_e[1]"
 );

 _ASSERT(
  is_equal_Em(Ep_to_Em(j_Ep([y,x])),j_Em(Ep_0_to_Em_0([y,x]))),
  "Ep_to_Em is compatible with Ep_0_to_Em_0"
 );

 _ASSERT(
  is_equal_Ep(Em_to_Ep(j_Em([y,x])),j_Ep(Em_0_to_Ep_0([y,x]))),
  "Em_to_Ep is compatible with Em_0_to_Ep_0"
 );

 _ASSERT(
  rem(numer(factor(is_member_Em_0(Ep_0_to_Em_0([y,x])))),is_member_Ep_0([y,x]),y) = 0,
  "Ep_0_to_Em_0 lands in Em_0"
 );

 _ASSERT(
  rem(numer(factor(is_member_Ep_0(Em_0_to_Ep_0([y,x])))),is_member_Em_0([y,x]),y) = 0,
  "Em_0_to_Ep_0 lands in Ep_0"
 );

 _ASSERT(
  factor(Ep_0_to_Em_0([y,x]) -~ Ep_0_to_Em_0(Ep_0_trans[1]([y,x]))) = [0,0],
  "Ep_0_to_Em_0 is invariant under translation by Ep_0_e[1]"
 );

 _ASSERT(
  factor(Em_0_to_Ep_0([y,x]) -~ Em_0_to_Ep_0(Em_0_trans[1]([y,x]))) = [0,0],
  "Em_0_to_Ep_0 is invariant under translation by Em_0_e[1]"
 );
  
 _ASSERT(
  simplify(PJ_to_Epm_0(PJ_trans(u)) -~ PJ_to_Epm_0(u)) = [0$4],
  "PJ -> Ep x Em is invariant under PJ_trans"
 );

 _ASSERT(
  map(rem,map(numer,simplify(PJ_to_Epm_0(P_to_PJ([w,z])) -~ P_to_Epm_0([w,z]))),w^2-r_P(z),w)
   = [0$4],
  "(P -> PJ -> Ep x Em) = (P -> Ep x Em)"
 );

 _ASSERT(
  map(rem,map(numer,simplify(P_to_PJ([w,z],-sqrt(z)) -~
                             PJ_trans(P_to_PJ([w,z],sqrt(z))))),w^2-r_P(z),w) = [0$4],
  "P -> PJ is well-defined"
 );

 _ASSERT(
  {seq(factor(is_member_Ep_0(Epm_w[i])),i=1..4)} = {0} and
  is_equal_Em(v_Em[2],factor(Ep_to_Em(j_Ep(Epm_w[1])))) and
  is_equal_Em(v_Em[2],factor(Ep_to_Em(j_Ep(Epm_w[2])))) and
  is_equal_Em(v_Em[4],factor(Ep_to_Em(j_Ep(Epm_w[3])))) and
  is_equal_Em(v_Em[4],factor(Ep_to_Em(j_Ep(Epm_w[4])))),
  "singular points for Ep_to_Em"
 );

end:

add_check(check_isogenies):

######################################################################

check_weierstrass := proc()
 local samples,chk,f0,f,g,err_a,err_b,err_c,err_d,err_e,err_f,k,m;

 printf("%a()\n",procname);

 _ASSERT(simplify(is_member_Ep_0(C_to_Ep_0(z))) = 0,
         "Weierstrass parametrisation C_to_Ep_0 lands in Ep_0");

 _ASSERT(simplify(is_member_Em_0(C_to_Em_0(z))) = 0,
         "Weierstrass parametrisation C_to_Em_0 lands in Em_0");

 _ASSERT(convert(series(C_to_Ep_0(z)[1],z=0,6),polynom,z) = z,
         "C_to_Ep_0(z) to first order");

 _ASSERT(convert(series(C_to_Em_0(z)[1],z=0,6),polynom,z) = z,
         "C_to_Em_0(z) to first order");

 _ASSERT(simplify(C_to_Ep_0(-z) -~ act_Ep_0[LL](C_to_Ep_0(z))) = [0,0],
         "C_to_Ep_0 is equivariant for LL");

 _ASSERT(simplify(C_to_Em_0(-z) -~ act_Em_0[LL](C_to_Em_0(z))) = [0,0],
         "C_to_Em_0 is equivariant for LL");

 samples := [seq(seq((-2)^i+(-3)^j*I,i=-3..3),j=-3..3)];
 chk := proc(f)
  local g;
  g := unapply(evalf(subs(a_P=0.1,max(map(abs,f)))),z);
  return evalb(max(map(g,samples)) < 10.^(-90));
 end;

 f := C_to_Ep_0(conjugate(z)) -~ act_Ep_0[N](C_to_Ep_0(z)):
 _ASSERT(chk(f),"C_to_Ep_0 is equivariant for N");

 f := C_to_Em_0(conjugate(z)) -~ act_Em_0[LLLN](C_to_Em_0(z)):
 _ASSERT(chk(f),"C_to_Em_0 is equivariant for LLLN");

 err_a := C_to_Ep_0(z + latt_a) -~ C_to_Ep_0(z):
 _ASSERT(chk(err_a),"C_to_Ep_0 is latt_a - periodic");

 err_b := C_to_Ep_0(z + latt_b) -~ C_to_Ep_0(z):
 _ASSERT(chk(err_b),"C_to_Ep_0 is latt_b - periodic");

 err_c := C_to_Em_0(z + latt_c) -~ C_to_Em_0(z):
 _ASSERT(chk(err_c),"C_to_Em_0 is latt_c - periodic");

 err_d := C_to_Em_0(z + latt_d) -~ C_to_Em_0(z):
 _ASSERT(chk(err_d),"C_to_Em_0 is latt_d - periodic");

 err_e := C_to_Em_0(z + latt_e) -~ C_to_Em_0(z):
 _ASSERT(chk(err_e),"C_to_Em_0 is latt_e - periodic");

 err_f := C_to_Em_0(z + latt_f) -~ C_to_Em_0(z):
 _ASSERT(chk(err_f),"C_to_Em_0 is latt_f - periodic");

 _ASSERT(max(
  d2f(div_Ep(latt_a0)    , [ 1, 0]),
  d2f(div_Ep(latt_b0)    , [ 0, 1]),
  d2f(div_Em(latt_f0)    , [ 1, 0]),
  d2f(div_Em(latt_e0)    , [ 0, 1]),
  d2f(div_Emq(latt_c0/2) , [ 0, 1]),
  d2f(div_Emq(latt_d0/2) , [-1, 0])) < 10.^(-90),
  "lattice division maps"
 );

 _ASSERT(
  factor(simplify(convert(series(Ep_0_to_C(C_to_Ep_0(z)),z=0,20),polynom,z))) = z,
  "Ep_0_to_C is inverse to C_to_Ep_0");

 _ASSERT(
  factor(simplify(convert(series(Em_0_to_C(C_to_Em_0(z)),z=0,20),polynom,z))) = z,
  "Em_0_to_C is inverse to C_to_Em_0");

 _ASSERT(
  max(
   op(map(abs,evalf(C_to_Ep0_0((latt_a0+latt_b0)/2) -~ Ep0_0_e[1]))),
   op(map(abs,evalf(C_to_Ep0_0(latt_a0/2)           -~ Ep0_0_e[2]))),
   op(map(abs,evalf(C_to_Ep0_0(latt_b0/2)           -~ Ep0_0_e[3])))
  ) < 10.^(-90),
  "points of order 2 on Ep, via Weierstrass"
 );

 _ASSERT(
  max(
   op(map(abs,evalf(C_to_Em0_0(latt_c0/2)) -~ Em0_0_e[1])),
   op(map(abs,evalf(C_to_Em0_0(latt_d0/2)) -~ Em0_0_e[1])),
   op(map(abs,evalf(C_to_Em0_0(latt_e0/2)) -~ Em0_0_e[3])),
   op(map(abs,evalf(C_to_Em0_0(latt_f0/2)) -~ Em0_0_e[2]))
  ) < 10.^(-49),
  "points of order 2 on Em, via Weierstrass"
 );

 _ASSERT(
  max(seq(evalf(d_CP2_0(v_Ep0_0[i],C_to_Ep0_0(v_TEp[i]))), i in [2,3,4,5,10,12])) < 10.^(-90),
  "v_TEp[i] maps to v_Ep_0[i]"
 );

 unassign('k','m');

 f := (m,t) -> abs(evalf(d_CP2_0(c_Ep0_0[m](t),c_Ep_0_approx[m](t))));
 for k from 0 to 8 do 
  _ASSERT(max(seq(f(k,i),i=1..20)) < 0.05,
          sprintf("c_TEp_approx[%d] is a good approximation",k));
 od;

 unassign('k','m');

 g := (m,t) -> abs(evalf(d_CP2_0(c_Em0_0[m](t),c_Em_0_approx[m](t))));
 for k from 0 to 8 do 
  _ASSERT(max(seq(g(k,i),i=1..20)) < 0.05,
          sprintf("c_TEm_approx[%d] is a good approximation",k));
 od;
end:

add_check(check_weierstrass):

######################################################################

check_Ep0_circle := proc()
 local NN,i,aa,pp_c,pp_d,pp_e,m;

 printf("%a()\n",procname);

 # This is a homeomorphism from R to R, with m(x+2*Pi) = m(x) + 2*Pi,
 # such that m'(0) = 0.
 m := (x) -> x - sin(x);
 
 NN := 128:
 for i from 0 to NN do 
  aa[i] := m((i+0.5)/NN * 2*Pi);
  pp_c[i] := evalf(Ep0_circle_lift_c(aa[i]));
  pp_d[i] := evalf(Ep0_circle_lift_d(aa[i]));
  pp_e[i] := evalf(Ep0_circle_lift_e(aa[i]));
 od:

 _ASSERT(max([seq(abs(pp_c[i]-pp_c[i-1]),i=1..NN)]) < 0.01,
         "Ep0_circle_lift_c is continuous");
	 
 _ASSERT(max([seq(abs(pp_d[i]-pp_d[i-1]),i=1..NN)]) < 0.01,
         "Ep0_circle_lift_d is continuous");
	 
 _ASSERT(max([seq(abs(pp_c[i]-pp_e[i  ]),i=1..NN)]) < 0.002,
         "Ep0_circle_lift_e approximates Ep0_circle_lift_c");
	 
 _ASSERT(
   max([seq(abs(evalf(C_to_Ep0_0(pp_c[i])[2] - exp(I*aa[i]))),i=1..NN)]) < 10.^(-90),
   "Ep0_circle_lift_c is a lift");
   
 _ASSERT(
   max([seq(abs(evalf(C_to_Ep0_0(pp_d[i])[2] - exp(I*aa[i]))),i=1..NN)]) < 10.^(-90),
   "Ep0_circle_lift_d is a lift");
   
 _ASSERT(
  max([seq(abs(evalf(Ep0_circle_equation(pp_c[i]))),i=0..127)]) < 10.^(-5),
  "Ep0_circle_lift_c satisfies the approximate lifted circle equation");

 _ASSERT(
  max([seq(abs(evalf(Ep0_circle_equation(pp_d[i]))),i=0..127)]) < 10.^(-5),
  "Ep0_circle_lift_d satisfies the approximate lifted circle equation");

 _ASSERT(
  max([seq(abs(evalf(Ep0_circle_equation(pp_e[i]))),i=0..127)]) < 0.0011,
  "Ep0_circle_lift_e satisfies the approximate lifted circle equation");
end:

add_check(check_Ep0_circle):

