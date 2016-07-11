check_picard_fuchs := proc()
 local err;

 printf("%a()\n",procname);

 _ASSERT(simplify(mp_period^2 + mm_period^2 - 1) = 0,"complementary moduli");

 _ASSERT(
  max(seq(evalf(subs(a_P = i*0.1,ap_period - ap_period_legendre)),i=1..9)) < 10.^(-90),
  "Legendre form for ap_period"
 );
 
 _ASSERT(
  max(seq(evalf(subs(a_P = i*0.1,am_period - am_period_legendre)),i=1..9)) < 10.^(-90),
  "Legendre form for am_period"
 );

 _ASSERT(
  factor(picard_fuchs_La(F(A_P)) -
        (2*(1-a_P^4)^5*subs(A = A_P,convert(picard_fuchs_LA(F(A)),D))/a_P^4)) = 0,
  "Two forms of Picard-Fuchs equation are compatible"
 );

 _ASSERT(simplify(picard_fuchs_La(ap_period)) = 0,
         "ap_period satisfies the Picard-Fuchs equation");

 _ASSERT(simplify(picard_fuchs_La(am_period)) = 0,
         "am_period satisfies the Picard-Fuchs equation");

 _ASSERT(simplify(picard_fuchs_La(ap_period_legendre)) = 0,
         "Legendre form for ap_period satisfies the Picard-Fuchs equation");

 _ASSERT(simplify(picard_fuchs_La(am_period_legendre)) = 0,
         "Legendre form for am_period satisfies the Picard-Fuchs equation");

 err := evalf(convert(series((r_period-r_period_approx)/sqrt(a_P),a_P=0,24),polynom,a_P));
 err := max(map(abs,[coeffs(err,a_P)]));

 _ASSERT(err < 10.^(-90),"r_period_approx is a good approximation");

 err := evalf(convert(series((s_period-s_period_approx)/sqrt(a_P),a_P=0,25),polynom,a_P));
 err := max(map(abs,[coeffs(err,a_P)]));

 _ASSERT(err < 10.^(-90),"s_period_approx is a good approximation");

end:

add_check(check_picard_fuchs):

######################################################################

check_period_integrals := proc()
 local F,xp,yp,dxp,xm,ym,dxm,err,alpha,wz,wz_to_t,t,x5,x6;

 printf("%a()\n",procname);
 _ASSERT(simplify(int(1/sqrt( q_Ep(x)),x=0..1/bp_P ) - ap_period/2) = 0,
         "elliptic integral for ap_period");

 assume(t > 0 and t < 1);
 
 x5 := (1-t^2)/(bp_P-t^2);

 _ASSERT(
  simplify(factor(diff(x5,t)/sqrt(q_Ep(x5)) -
                  (-1/sqrt((1-t^2)*(1-mp_period^2*t^2))/sqrt(bp_P)))) = 0 and
  simplify(limit(x5,t=0) - 1/bp_P) = 0 and
  simplify(limit(x5,t=1)) = 0,
  "substitution for elliptic integral for ap_period"
 );
  
 _ASSERT(simplify(int(1/sqrt(-q_Ep(x)),x=-1/bp_P..0) - am_period/2) = 0,
        "elliptic integral for am_period"); 

 x6 := 1/(bp_P*(1-2/t^2));

 _ASSERT(
  simplify(factor(diff(x6,t)/sqrt(-q_Ep(x6))) -
                  (-1/sqrt((1-t^2)*(1-mm_period^2*t^2))/sqrt(bp_P))) = 0 and
  simplify(limit(x6,t=0)) = 0 and
  simplify(limit(x6,t=1) + 1/bp_P) = 0,
  "substitution for elliptic integral for am_period"
 );  

 _ASSERT(simplify(picard_fuchs_LA((z^5-A*z^3+z)^(-1/2)) - 
                  diff((33*z^8-3*A*z^10-27*z^12)*(z^5-A*z^3+z)^(-7/2),z)) = 0,
         "Picard-Fuchs exact differential");

 yp,xp := op(P_to_Ep_0([w,z]));
 dxp := diff(xp,z) * dz + diff(xp,w) * dw;
 
 _ASSERT(simplify(dxp/yp - (1+z)*dz/w) = 0,"Pullback of omega^+");

 ym,xm := op(P_to_Em_0([w,z]));
 dxm := diff(xm,z) * dz + diff(xm,w) * dw;
 
 _ASSERT(simplify(dxm/ym - (1+I)/sqrt(2)*(I-z)*dz/w) = 0,"Pullback of omega^-");

 wz := j_inv_P(c_P[5](t));
 wz_to_t := {w = wz[1],z = wz[2],dw = diff(wz[1],t)*dt,dz = diff(wz[2],t)*dt};
 alpha := combine(simplify(subs(wz_to_t,dxp/yp)));
 err := Int(factor(alpha/dt),t=0..2*Pi) - ap_period;
 err := max(seq(abs(evalf(subs(a_P=a,err))),a=0.1..0.9,0.1));

 _ASSERT(err < 10^(-95),"integral of dx/y over C[5]");

 wz := j_inv_P(c_P[6](t));
 wz_to_t := {w = wz[1],z = wz[2],dw = diff(wz[1],t)*dt,dz = diff(wz[2],t)*dt};
 alpha := combine(simplify(subs(wz_to_t,dxp/yp)));
 err := Int(factor(alpha/dt),t=0..2*Pi) - I*am_period;
 err := max(seq(abs(evalf(subs(a_P=a,err))),a=0.1..0.9,0.1));

 _ASSERT(err < 10^(-95),"integral of dx/y over C[6]");
end:

add_check(check_period_integrals):

######################################################################

check_periods := proc()
 local i,j,err;

 printf("%a()\n",procname);

 for i from 0 to 8 do
  for j from 0 to 1 do
   err := expand(p_period[i,j] - add(c_homology[i][k]*p_period[4+k,j],k=1..4));
   _ASSERT(err = 0,sprintf("period p[%d,%d] consistent with homology",i,j));
  od;
 od;
end:

add_check(check_periods):
