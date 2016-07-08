check_ellquot := proc()
 local t,w,x,y,wz,zz,yx,yxt,err,aa,bb,cc,old_digits;

 printf("%a()\n",procname);

 zz := [seq(z[i],i=1..5)];
 wz := [w,z];

 y,x := op(phi_P_0_Ep0(wz));

 err := simplify(y^2 - q_Ep(x) - (w^2 - r_P(z))*((z-1)^2/(4*z^4)));
 _ASSERT(err = 0,"phi_+(PX0) is in E_+");

 err := NF_P(expand(a_P^2 * Ep_rel(phi_P_Ep(zz))));
 _ASSERT(err = 0,"phi(PX) is in E_+");

 y,x := op(phi_P_0_Em0(wz));

 err := simplify(y^2 - q_Em(x) - (w^2 - r_P(z))*((I*z-1)^2/(4*z^4)));
 _ASSERT(err = 0,"phi_-(PX0) is in E_-");

 unassign('x');

 _ASSERT(simplify(phi_P_0_Ep0(act_P_0[M](wz)) -~ phi_P_0_Ep0(wz)) = [0,0],
   "phi_+ o mu = phi_+"
 );

 _ASSERT(simplify(phi_P_0_Em0(act_P_0[LM](wz)) -~ phi_P_0_Em0(wz)) = [0,0],
   "phi_- o lambda o mu = phi_-"
 );

 _ASSERT(simplify((2*z^2) *~ j_CP2(phi_P_0_Ep0(wz)) -~ phi_P_Ep(j_P(wz))) = [0,0,0],
   "phi_+ consistent on PX0 and PX"
 );

 aa := phi_P_Ep(zz);
 bb := phi_P_Ep_alt(zz);
 cc := phi_P_Ep_alt(act_P[M](zz));
 err := map(NF_P,[seq(seq(aa[i]*cc[j] - aa[j]*cc[i],j=i+1..3),i=1..2)]);

 _ASSERT(err = [0$3],"phi_P_Ep = phi_P_Ep_alt = phi_P_Ep_alt o mu");

# err := 
#  simplify([
#   int((-q_Ep(x))^(-1/2),x=-infinity .. -n_E     ) - EllipticK(mm_E)/sqrt(n_E),
#   int(( q_Ep(x))^(-1/2),x=-n_E      .. 1        ) - EllipticK(mp_E)/sqrt(n_E),
#   int((-q_Ep(x))^(-1/2),x= 1        ..  n_E     ) - EllipticK(mm_E)/sqrt(n_E),
#   int(( q_Ep(x))^(-1/2),x= n_E      ..  infinity) - EllipticK(mp_E)/sqrt(n_E)
#  ]);
#
# _ASSERT(err = [0$4],"elliptic integrals for E_+");

 old_digits := Digits;
 Digits := 100;

 err := series((r_period-r_E)/sqrt(a_P),a_P=0,13);
 err := convert(evalf(err),polynom,a_P);
 err := max(op(map(abs,[coeffs(err,a_P)])));

 _ASSERT(err < 10.^(-90),"Power series for r_E");

 err := series((s_period-s_E)/sqrt(a_P),a_P=0,13);
 err := convert(evalf(err),polynom,a_P);
 err := max(op(map(abs,[coeffs(err,a_P)])));

 _ASSERT(err < 10.^(-90),"Power series for r_E");

 Digits := old_digits;

 _ASSERT(simplify(d_Ep[0](t)[1]^2 - q_Ep(d_Ep[0](t)[2])) = 0,"d^+_0(R) is in E_+");
 _ASSERT(simplify(d_Ep[1](t)[1]^2 - q_Ep(d_Ep[1](t)[2])) = 0,"d^+_1(R) is in E_+");

 _ASSERT(
  simplify(map(convert,map(series,d_Ep[0](t),t=0,2),polynom,t) -~
           [  (1-a_P)*(1+a_P)^2/(4*a_P^(3/2))*t,1]) = [0,0]
  and
  simplify(map(convert,map(series,d_Ep[1](t),t=0,2),polynom,t) -~
           [I*(1-a_P)^2*(1+a_P)/(4*a_P^(3/2))*t,1]) = [0,0],
  "Transverse intersection of d^+_0 and d^+_1"
 );

 NULL;
end:

add_check(check_ellquot):
