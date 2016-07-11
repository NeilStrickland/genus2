check_torus_diagrams := proc()
 local F1_samples,ok,x0,z0,zp0,zpa0,zm0,zmq0,t0,tp0,tpa0,tm0,tmq0,errs;

 printf("%a()\n",procname);

 F1_samples := [seq(seq(act_R4[T](inner_quasirational_points[i]),i=1..10),T in G16)]:
 ok := true:

 for x0 in F1_samples do
  z0   := simplify(expand(rationalize(E_to_TTC(x0))));
  zp0  := simplify(expand(rationalize(E_to_TCp(x0))));
  zpa0 := simplify(expand(rationalize(E_to_TCpa(x0))));
  t0   := simplify(expand(rationalize(E_to_TTP(x0))));
  tp0  := simplify(expand(rationalize(E_to_TPp(x0))));
  tpa0 := simplify(expand(rationalize(E_to_TPpa(x0))));
  errs := [
   TTC_to_TTP(z0) -~ t0,
   TTP_to_TTC(t0) -~ z0,
   TTC_to_TCp(z0) -~ zp0,
   TTP_to_TPp(t0) -~ tp0,
   TC_to_TP(zp0) -~ tp0,
   TP_to_TC(tp0) -~ zp0,
   TTC_to_TCpa(z0) -~ zpa0,
   TTP_to_TPpa(t0) -~ tpa0,
   TC_to_TP(zpa0) -~ tpa0,
   TP_to_TC(tpa0) -~ zpa0,
   TCp_to_TCpa(zp0) -~ zpa0, 
   TPp_to_TPpa(tp0) -~ tpa0, 
   TCpa_to_TCp(zpa0) -~ zp0, 
   TPpa_to_TPp(tpa0) -~ tp0 
  ];
  errs := simplify(expand(rationalize(map(op,errs))));
  if {op(errs)} <> {0} then
   ok := false;
   break;
  fi;
 od:

 _ASSERT(ok,"Diagram for Tp commutes");

 ok := true:
 for x0 in F1_samples do
  z0   := simplify(expand(rationalize(E_to_TTC(x0))));
  zm0  := simplify(expand(rationalize(E_to_TCm(x0))));
  zmq0 := simplify(expand(rationalize(E_to_TCmq(x0))));
  t0   := simplify(expand(rationalize(E_to_TTP(x0))));
  tm0  := simplify(expand(rationalize(E_to_TPm(x0))));
  tmq0 := simplify(expand(rationalize(E_to_TPmq(x0))));
  errs := [
   TTC_to_TTP(z0) -~ t0,
   TTP_to_TTC(t0) -~ z0,
   TTC_to_TCm(z0) -~ zm0,
   TCm_to_TCmq(zm0) -~ zmq0,
   TTC_to_TCmq(z0) -~ zmq0,
   TTP_to_TPm(t0) -~ tm0,
   TPm_to_TPmq(tm0) -~ tmq0,
   TTP_to_TPmq(t0) -~ tmq0,
   TC_to_TP(zm0) -~ tm0,
   TC_to_TP(zmq0) -~ tmq0,
   TP_to_TC(tm0) -~ zm0,
   TP_to_TC(tmq0) -~ zmq0
  ];
  errs := simplify(expand(rationalize(map(op,errs))));
  if {op(errs)} <> {0} then
   ok := false;
   break;
  fi;
 od:

 _ASSERT(ok,"Diagram for Tm and Tmq commutes");
end:

add_check(check_torus_diagrams):

######################################################################

check_torus_T := proc()
 local x0,T,s,k,ca,cb,err;

 printf("%a()\n",procname);

 _ASSERT( 
  map(FNF_y0,TTP_to_TTC(E_to_TTP_xyr) -~ E_to_TTC_xyr) = [0$4],
  "E_to_TTP is consistent with E_to_TTC"
 );

 _ASSERT(
  map(FNF_y0,simplify(TTP_to_E_generic(E_to_TTP(xx)) -~ xx)) = [0$4],
  "TTP_to_E_generic is a left inverse for E_to_TTP"
 );

 _ASSERT(
  {seq(seq(simplify(TTC_to_E_generic(E_to_TTC(act_R4[T](x0))) -~
		    act_R4[T](x0)),
   x0 in inner_quasirational_points),T in G16)} = {[0$4]},
  "TTC_to_E_generic is a left inverse for E_to_TC"
  );

 _ASSERT(
  {seq(seq(simplify(TTC_to_E(E_to_TTC(act_R4[T](x0))) -~
		    act_R4[T](x0)),
   x0 in quasirational_points),T in G16)} = {[0$4]},
  "TTC_to_E is a left inverse for E_to_TC"
  );

 _ASSERT(
  {seq(simplify(act_TTP[T](E_to_TTP(xx)) -~ E_to_TTP(act_R4[T](xx))), T in G16)} = {[0$4]},
  "E_to_TTP is equivariant"
 );

 _ASSERT(
  {seq(seq(simplify(act_TTC[T](E_to_TTC(x0)) -~ E_to_TTC(act_R4[T](x0))),
	   T in G16),x0 in quasirational_points)} = {[0,0,0,0]},
  "E_to_TTC is equivariant"
 );

 assume(s::real):

 for k from 0 to 16 do
  if k <= 8 then
   ca := simplify(map(convert,map(series,E_to_TTP(annular_chart0[k]([s,u])),u=0,3),polynom,u));
   ca := map(a -> `if`(coeff(a,u,-1) <> 0,infinity,subs(u=0,a)),ca);
  else
   ca := simplify(E_to_TTP(c_E0[k](s)));
  fi;
  cb := c_TTP[k](s);
  err := combine(simplify(subs(infinity=INFINITY,ca) -~ subs(infinity=INFINITY,cb)));

  _ASSERT(err = [0$4],sprintf("E_to_TTP(c[%d](t))",k));
 od:

 for k from 0 to 16 do
  ca := simplify(E_to_TTC(c_E0[k](s)));
  cb := c_TTC[k](s);
  err := simplify(factor(expand(rationalize(combine(simplify(ca-cb))))));

  _ASSERT(err = [0$4],sprintf("E_to_TTC(c[%d](t))",k));
 od:
end:

add_check(check_torus_T):

######################################################################

check_torus_Tp := proc()
 local F2_samples,x0,T,s,u,z0,k,ca,cb,err;

 printf("%a()\n",procname);

 F2_samples := [seq(seq(act_R4[T](inner_quasirational_points[i]),i=1..10),T in L8)]:

 _ASSERT( 
  map(FNF_y0,TP_to_TC(E_to_TPp_xyr) -~ E_to_TCp_xyr) = [0,0],
  "E_to_TPp is consistent with E_to_TCp"
 );

 _ASSERT(
  FNF_y0(simplify(TTP_to_TPp(E_to_TTP(xx)) -~ E_to_TPp(xx))) = [0$2],
  "Composite E -> TTP -> TPp"
 );

 _ASSERT(
  FNF_y0(simplify(TTC_to_TCp(E_to_TTC(xx)) -~ E_to_TCp(xx))) = [0$2],
  "Composite E -> TTC -> TCp"
 );

 _ASSERT(
  map(FNF_y0,simplify(E_to_TPp(TPp_to_E([t[1],t[2]])) -~ [t[1],t[2]])) = [0$2],
  "TPp_to_E is a right inverse for E_to_TPp"
 );

 assume(s[1]::real,s[2]::real):
 z0 := [cos(s[1])+I*sin(s[1]),cos(s[2])+I*sin(s[2])]:

 _ASSERT(
  simplify(E_to_TCp(TCp_to_E(z0)) -~ z0) = [0$2],
  "TCp_to_E is a right inverse for E_to_TCp"
 );

 _ASSERT(
  {seq(    simplify(act_TPp[T](E_to_TPp(xx)) -~ E_to_TPp(act_R4[T](xx))),
       T in [1,LL,M,LLM,N,LLN,MN,LLMN]),
   seq(seq(simplify(act_TPp[T](E_to_TPp(x0)) -~ E_to_TPp(act_R4[T](x0))),
       T in [L,LLL,LN,LLLN]),x0 in F2_samples),
   seq(seq(simplify(act_TPp[T](E_to_TPp(x0)) -~ E_to_TPp(act_R4[T](x0))),
       T in [LM,LLLM,LMN,LLLMN]),x0 in map(act_R4[M],F2_samples))
  } = {[0,0]},
  "Equivariance properties of E_to_TPp"
 );

 assume(s::real,u::real):
 for k from 0 to 16 do
  if k <= 8 then
   ca := simplify(E_to_TPp(annular_chart0[k]([s,u])));
   ca := map(convert,map(series,ca,u=0,3),polynom,u);
   ca := collect(simplify(ca),u);
   ca := map(a -> `if`(coeff(a,u,-1) <> 0,infinity,subs(u=0,a)),ca);
  else
   ca := simplify(E_to_TPp(c_E0[k](s)));
  fi;
  cb := c_TPp[k](s);
  err := combine(simplify(subs(infinity=INFINITY,ca) -~ subs(infinity=INFINITY,cb)));
  err := factor(expand(err));

  _ASSERT(err = [0$2],sprintf("E_to_TPp(c[%d](t))",k));
 od:

 for k from 0 to 16 do
  ca := simplify(E_to_TCp(c_E0[k](s)));
  cb := c_TCp[k](s);
  err := factor(expand(rationalize(combine(simplify(convert(ca-cb,trig))))));

  _ASSERT(err = [0$2],sprintf("E_to_TCp(c[%d](t))",k));
 od:
end:

add_check(check_torus_Tp):

######################################################################

check_torus_Tpa := proc()
 local F2_samples,x0,T,s,u,z0,k,ca,cb,err;

 printf("%a()\n",procname);

 F2_samples := [seq(seq(act_R4[T](inner_quasirational_points[i]),i=1..10),T in L8)]:

 _ASSERT( 
  map(FNF_y0,TP_to_TC(E_to_TPpa_xyr) -~ E_to_TCpa_xyr) = [0,0],
  "E_to_TPpa is consistent with E_to_TCpa"
 );

 _ASSERT(
  FNF_y0(simplify(TTP_to_TPpa(E_to_TTP(xx)) -~ E_to_TPpa(xx))) = [0$2],
  "Composite E -> TP -> TPpa"
 );

 _ASSERT(
  FNF_y0(simplify(TTC_to_TCpa(E_to_TTC(xx)) -~ E_to_TCpa(xx))) = [0$2],
  "Composite E -> TTC -> TCpa"
 );

 _ASSERT(
  map(FNF_y0,simplify(E_to_TPpa(TPpa_to_E([t[1],t[2]])) -~ [t[1],t[2]])) = [0$2],
  "TPpa_to_E is a right inverse for E_to_TPpa"
 );

 assume(s[1]::real,s[2]::real):
 z0 := [cos(s[1])+I*sin(s[1]),cos(s[2])+I*sin(s[2])]:

 _ASSERT(
  simplify(E_to_TCpa(TCpa_to_E(z0)) -~ z0) = [0$2],
  "TCpa_to_E is a right inverse for E_to_TCpa"
 );

 _ASSERT(
  {seq(    simplify(act_TPpa[T](E_to_TPpa(xx)) -~ E_to_TPpa(act_R4[T](xx))),
       T in [1,LL,M,LLM,N,LLN,MN,LLMN]),
   seq(seq(simplify(act_TPpa[T](E_to_TPpa(x0)) -~ E_to_TPpa(act_R4[T](x0))),
       T in [L,LLL,LN,LLLN]),x0 in F2_samples),
   seq(seq(simplify(act_TPpa[T](E_to_TPpa(x0)) -~ E_to_TPpa(act_R4[T](x0))),
       T in [LM,LLLM,LMN,LLLMN]),x0 in map(act_R4[M],F2_samples))
  } = {[0,0]},
  "Equivariance properties of E_to_TPpa"
 );

 assume(s::real,u::real):
 for k from 0 to 16 do
  if k <= 8 then
   ca := simplify(E_to_TPpa(annular_chart0[k]([s,u])));
   ca := map(convert,map(series,ca,u=0,3),polynom,u);
   ca := collect(simplify(ca),u);
   ca := map(a -> `if`(coeff(a,u,-1) <> 0,infinity,subs(u=0,a)),ca);
  else
   ca := simplify(E_to_TPpa(c_E0[k](s)));
  fi;
  cb := c_TPpa[k](s);
  err := combine(simplify(subs(infinity=INFINITY,ca) -~ subs(infinity=INFINITY,cb)));
  err := factor(expand(err));

  _ASSERT(err = [0$2],sprintf("E_to_TPpa(c[%d](t))",k));
 od:

 for k from 0 to 16 do
  ca := simplify(E_to_TCpa(c_E0[k](s)));
  cb := c_TCpa[k](s);
  err := factor(expand(rationalize(combine(simplify(convert(ca-cb,trig))))));

  _ASSERT(err = [0$2],sprintf("E_to_TCpa(c[%d](t))",k));
 od:
end:

add_check(check_torus_Tpa):

######################################################################

check_torus_Tm := proc()
 local x0,T,s,z0,k,ca,cb,err;

 printf("%a()\n",procname);

 _ASSERT(
  {seq(map(NF_x0,map(numer,simplify(
       act_TCm[T](E_to_TCm(xx)) -~ E_to_TCm(act_R4[T](xx))))),
   T in [1,LL,LM,LLLM,LN,LLLN,MN,LLMN])
  } = {[0,0]},
  "Equivariance properties of E_to_TCm"
 );

 assume(s::real);
 ca := 0;

 for k from 0 to 16 do
  ca := simplify(E_to_TPm(c_E0[k](s)));
  cb := c_TPm[k](s);
  ca := factor(expand(rationalize(ca))):
  ca := factor(expand(combine(ca)));
  cb := factor(expand(rationalize(cb)));
  cb := factor(expand(combine(cb)));
  err := combine(simplify(subs(infinity=INFINITY,ca) -~ subs(infinity=INFINITY,cb)));
  err := factor(expand(err));

  _ASSERT(err = [0$2],sprintf("E_to_TPm(c[%d](t))",k));
 od:

 for k from 0 to 16 do
  ca := simplify(E_to_TCm(c_E0[k](s)));
  cb := c_TCm[k](s);
  err := factor(expand(rationalize(combine(simplify(convert(ca-cb,trig))))));
  err := simplify(err);

  _ASSERT(err = [0$2],sprintf("E_to_TCm(c[%d](t))",k));
 od:

 _ASSERT(
  {seq(simplify(subs(infinity=INFINITY,simplify(map(SC1_to_R,v_TCm[k]))) -~ 
		subs(infinity=INFINITY,v_TPm[k])),k=0..18)} = {[0$2]},
  "v_TCm and v_TPm"
 );
end:

add_check(check_torus_Tm):

######################################################################

check_torus_Tmq := proc()
 local F1_samples,x0,T,s,z0,k,ca,cb,err;

 printf("%a()\n",procname);

 F1_samples := [seq(seq(act_R4[T](inner_quasirational_points[i]),i=1..10),T in G16)]:

 _ASSERT( 
  {seq(simplify(TTC_to_TCmq(E_to_TTC(x0)) -~ E_to_TCmq(x0)),x0 in F1_samples)} = {[0,0]},
  "Composite E -> TTC -> TCmq"
 );

 _ASSERT( 
  {seq(simplify(TTP_to_TPmq(E_to_TTP(x0)) -~ E_to_TPmq(x0)),x0 in F1_samples)} = {[0,0]},
  "Composite E -> TTP -> TPmq"
 );

 _ASSERT( 
  {seq(simplify(TP_to_TC(E_to_TPmq(x0)) -~ E_to_TCmq(x0)),x0 in F1_samples)} = {[0,0]},
  "Composite E -> TPmq -> TCmq"
 );

 _ASSERT( 
  {seq(simplify(TC_to_TP(E_to_TCmq(x0)) -~ E_to_TPmq(x0)),x0 in F1_samples)} = {[0,0]},
  "Composite E -> TCmq -> TPmq"
 );

 _ASSERT(
  {seq(map(NF_x0,map(numer,simplify(
       act_TCmq[T](E_to_TCmq(xx)) -~ E_to_TCmq(act_R4[T](xx))))),
   T in [1,LL,LM,LLLM,LN,LLLN,MN,LLMN])
  } = {[0,0]},
  "Equivariance properties of E_to_TCmq"
 );

 assume(s::real);

 for k from 0 to 16 do
  ca := simplify(E_to_TCmq(c_E0[k](s)));
  cb := c_TCmq[k](s);
  err := factor(expand(rationalize(combine(simplify(convert(ca-cb,trig))))));
  err := simplify(err);

  _ASSERT(err = [0$2],sprintf("E_to_TCmq(c[%d](t))",k));
 od:

 for k in [0,1,2,3,4,5,6,7,8,9,14,15] do
  ca := simplify(TPm_to_TPmq(c_TPm[k](s)));
  cb := c_TPmq[k](s);
  err := combine(simplify(subs(infinity=INFINITY,ca) -~ subs(infinity=INFINITY,cb)));
  err := factor(expand(err));

  _ASSERT(err = [0$2],sprintf("E_to_TPmq(c[%d](t))",k));
 od:

 for k in [10,11,12,13,15,16] do
  ca := simplify(map(SC1_to_R,c_TCmq[k](s)));
  cb := c_TPmq[k](s);
  err := combine(simplify(subs(infinity=INFINITY,ca) -~ subs(infinity=INFINITY,cb)));
  err := factor(expand(err));

  _ASSERT(err = [0$2],sprintf("E_to_TPmq(c[%d](t))",k));
 od:
 _ASSERT(
  {seq(simplify(subs(infinity=INFINITY,simplify(map(SC1_to_R,v_TCmq[k]))) -~ 
		subs(infinity=INFINITY,v_TPmq[k])),k=0..20)} = {[0$2]},
  "v_TCmq and v_TPmq"
 );
end:

add_check(check_torus_Tmq):

######################################################################

check_torus_jacobian := proc()
 local DT,Jp,jp,Jpa,jpa,Jm,jm,Jmq,jmq;

 printf("%a()\n",procname);

 DT := (t) -> [seq(2*diff(t,x[i])/(1+t^2),i=1..4)];

 Jp := Matrix(simplify([xx,dg0(xx),op(map(DT,E_to_TPp(xx)))])):
 Jp := map(u -> subs(root_rule,numer(u))/subs(root_rule,denom(u)),Jp):
 jp := FNF_y0(Determinant(Jp)):
 _ASSERT(FNF_y0(E_to_TPp_jacobian - jp) = 0,
         "Jacobian of E_to_TPp");

 Jm := Matrix(simplify([xx,dg0(xx),op(map(DT,E_to_TPm(xx)))])):
 Jm := map(u -> subs(root_rule,numer(u))/subs(root_rule,denom(u)),Jm):
 jm := FNF_y0(Determinant(Jm)):
 _ASSERT(FNF_y0(E_to_TPm_jacobian - jm) = 0,
         "Jacobian of E_to_TPm");

 # This is commented out because it is very slow.
# Jmq := Matrix(simplify([xx,dg0(xx),op(map(DT,E_to_TPmq(xx)))])):
# Jmq := map(u -> subs(root_rule,numer(u))/subs(root_rule,denom(u)),Jmq):
# jmq := FNF_y0(Determinant(Jmq)):
# _ASSERT(FNF_y0(E_to_TPmq_jacobian - jmq) = 0,
#         "Jacobian of E_to_TPmq");

 Jpa := Matrix(simplify([xx,dg0(xx),op(map(DT,E_to_TPpa(xx)))])):
 Jpa := map(u -> subs(root_rule,numer(u))/subs(root_rule,denom(u)),Jpa):
 jpa := FNF_y0(Determinant(Jpa)):
 _ASSERT(FNF_y0(E_to_TPpa_jacobian - jpa) = 0,
         "Jacobian of E_to_TPpa");

end:

add_check(check_torus_jacobian):

######################################################################

check_torus_inverse := proc()
 local RR_samples,RR_2_samples,Q_samples,GQ_samples,
       a,ok,x0,x1,x2,a1,b1,i2,T,err,k,t0,t1;

 printf("%a()\n",procname);

 assume(a[1]::real); 
 assume(a[2]::real);

 err := simplify(rho(TPp_to_E([a[1],a[2]]))-1);
 _ASSERT(err = 0,"TPp_to_E lands in S3");

 err := simplify(g0(TPp_to_E([a[1],a[2]])));
 _ASSERT(err = 0,"TPp_to_E lands in EX^*");

 err := simplify(E_to_TPp(TPp_to_E([a[1],a[2]])) -~ [a[1],a[2]]);

 _ASSERT(err = [0,0],"E_to_TPp o TPp_to_E");

 err := simplify(rho(TPpa_to_E([a[1],a[2]]))-1);
 _ASSERT(err = 0,"TPpa_to_E lands in S3");

 err := simplify(g0(TPpa_to_E([a[1],a[2]])));
 _ASSERT(err = 0,"TPpa_to_E lands in EX^*");

 err := simplify(E_to_TPpa(TPpa_to_E([a[1],a[2]])) -~ [a[1],a[2]]);

 _ASSERT(err = [0,0],"E_to_TPpa o TPpa_to_E");

 RR_samples := sort([seq(seq(1.1*(-2)^i*3^j,i=-2..2),j=-2..2)]);
 RR_2_samples := [seq(seq([t1,1.3*t2],t1 in RR_samples),t2 in RR_samples)]:
 Q_samples := [seq(inner_quasirational_points[10*i],i=1..10)];
 GQ_samples := map(op,[seq(map(act_R4[T],Q_samples),T in G16)]);

 ok := true:
 for t0 in RR_2_samples do
  for k from 1 to 4 do
   x0 := evalf(TPmq_to_E(evalf(t0),k));
   t1 := evalf(E_to_TPmq(x0));
   err := max(abs(evalf(rho(x0)-1)),abs(evalf(g0(x0))),d2f(t0,t1));
   if err > 10.^(-90) then
    ok := false;
    break;
   fi;
  od;
 od:

 _ASSERT(ok,"E_to_TPmq o TPmq_to_E");

 ok := true;

 for x1 in GQ_samples do
  a1 := simplify(E_to_TPp(x1));
  x2 := simplify(TPp_to_E(a1));
  if is(x1[3] < 0) then x2 := act_R4[M](x2); fi;
  if simplify(x1 -~ x2) <> [0$4] then
   ok := false; break;
  fi;
 od:

 _ASSERT(ok,"TPp_to_E o E_to_TPp");

 ok := true;

 for x1 in GQ_samples do
  a1 := simplify(E_to_TPpa(x1));
  x2 := simplify(TPpa_to_E(a1));
  if is(x1[3] < 0) then x2 := act_R4[M](x2); fi;
  if simplify(x1 -~ x2) <> [0$4] then
   ok := false; break;
  fi;
 od:

 _ASSERT(ok,"TPpa_to_E o E_to_TPpa");

 ok := true;

 for x1 in GQ_samples do
  a1 := simplify(combine(expand(rationalize(E_to_TPm(x1)))));
  b1 := simplify(E_to_TPmq(x1));
  if is(x1[3] > 0) then
   if is(simplify((a1[1]*a1[2])^2 - 1) > 0) then
    i2 := 1;
   else 
    i2 := 2;
   fi;
  else
   if is(simplify((a1[1]*a1[2])^2 - 1) > 0) then
    i2 := 3;
   else 
    i2 := 4;
   fi;
  fi;
  x2 := simplify(TPmq_to_E(b1,i2)):
  err := d4f(x1,x2);
  if (err > 10.^(-80)) then
   ok := false; break;
  fi;
 od:

 _ASSERT(ok,"TPmq_to_E o E_to_TPmq");
end:

add_check(check_torus_inverse):
