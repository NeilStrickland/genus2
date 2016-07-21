<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_picard_fuchs := proc()
<a name="line_2"></a> local err;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> _ASSERT(simplify(mp_period^2 + mm_period^2 - 1) = 0,"complementary moduli");
<a name="line_7"></a>
<a name="line_8"></a> _ASSERT(
<a name="line_9"></a>  max(seq(evalf(subs(a_P = i*0.1,ap_period - ap_period_legendre)),i=1..9)) < 10.^(-90),
<a name="line_10"></a>  "Legendre form for ap_period"
<a name="line_11"></a> );
<a name="line_12"></a> 
<a name="line_13"></a> _ASSERT(
<a name="line_14"></a>  max(seq(evalf(subs(a_P = i*0.1,am_period - am_period_legendre)),i=1..9)) < 10.^(-90),
<a name="line_15"></a>  "Legendre form for am_period"
<a name="line_16"></a> );
<a name="line_17"></a>
<a name="line_18"></a> _ASSERT(
<a name="line_19"></a>  factor(picard_fuchs_La(F(A_P)) -
<a name="line_20"></a>        (2*(1-a_P^4)^5*subs(A = A_P,convert(picard_fuchs_LA(F(A)),D))/a_P^4)) = 0,
<a name="line_21"></a>  "Two forms of Picard-Fuchs equation are compatible"
<a name="line_22"></a> );
<a name="line_23"></a>
<a name="line_24"></a> _ASSERT(simplify(picard_fuchs_La(ap_period)) = 0,
<a name="line_25"></a>         "ap_period satisfies the Picard-Fuchs equation");
<a name="line_26"></a>
<a name="line_27"></a> _ASSERT(simplify(picard_fuchs_La(am_period)) = 0,
<a name="line_28"></a>         "am_period satisfies the Picard-Fuchs equation");
<a name="line_29"></a>
<a name="line_30"></a> _ASSERT(simplify(picard_fuchs_La(ap_period_legendre)) = 0,
<a name="line_31"></a>         "Legendre form for ap_period satisfies the Picard-Fuchs equation");
<a name="line_32"></a>
<a name="line_33"></a> _ASSERT(simplify(picard_fuchs_La(am_period_legendre)) = 0,
<a name="line_34"></a>         "Legendre form for am_period satisfies the Picard-Fuchs equation");
<a name="line_35"></a>
<a name="line_36"></a> err := evalf(convert(series((r_period-r_period_approx)/sqrt(a_P),a_P=0,24),polynom,a_P));
<a name="line_37"></a> err := max(map(abs,[coeffs(err,a_P)]));
<a name="line_38"></a>
<a name="line_39"></a> _ASSERT(err < 10.^(-90),"r_period_approx is a good approximation");
<a name="line_40"></a>
<a name="line_41"></a> err := evalf(convert(series((s_period-s_period_approx)/sqrt(a_P),a_P=0,25),polynom,a_P));
<a name="line_42"></a> err := max(map(abs,[coeffs(err,a_P)]));
<a name="line_43"></a>
<a name="line_44"></a> _ASSERT(err < 10.^(-90),"s_period_approx is a good approximation");
<a name="line_45"></a>
<a name="line_46"></a>end:
<a name="line_47"></a>
<a name="line_48"></a>add_check(check_picard_fuchs):
<a name="line_49"></a>
<a name="line_50"></a>######################################################################
<a name="line_51"></a>
<a name="line_52"></a>check_period_integrals := proc()
<a name="line_53"></a> local F,xp,yp,dxp,xm,ym,dxm,err,alpha,wz,wz_to_t,t,x5,x6;
<a name="line_54"></a>
<a name="line_55"></a> printf("%a()\n",procname);
<a name="line_56"></a> _ASSERT(simplify(int(1/sqrt( q_Ep(x)),x=0..1/bp_P ) - ap_period/2) = 0,
<a name="line_57"></a>         "elliptic integral for ap_period");
<a name="line_58"></a>
<a name="line_59"></a> assume(t > 0 and t < 1);
<a name="line_60"></a> 
<a name="line_61"></a> x5 := (1-t^2)/(bp_P-t^2);
<a name="line_62"></a>
<a name="line_63"></a> _ASSERT(
<a name="line_64"></a>  simplify(factor(diff(x5,t)/sqrt(q_Ep(x5)) -
<a name="line_65"></a>                  (-1/sqrt((1-t^2)*(1-mp_period^2*t^2))/sqrt(bp_P)))) = 0 and
<a name="line_66"></a>  simplify(limit(x5,t=0) - 1/bp_P) = 0 and
<a name="line_67"></a>  simplify(limit(x5,t=1)) = 0,
<a name="line_68"></a>  "substitution for elliptic integral for ap_period"
<a name="line_69"></a> );
<a name="line_70"></a>  
<a name="line_71"></a> _ASSERT(simplify(int(1/sqrt(-q_Ep(x)),x=-1/bp_P..0) - am_period/2) = 0,
<a name="line_72"></a>        "elliptic integral for am_period"); 
<a name="line_73"></a>
<a name="line_74"></a> x6 := 1/(bp_P*(1-2/t^2));
<a name="line_75"></a>
<a name="line_76"></a> _ASSERT(
<a name="line_77"></a>  simplify(factor(diff(x6,t)/sqrt(-q_Ep(x6))) -
<a name="line_78"></a>                  (-1/sqrt((1-t^2)*(1-mm_period^2*t^2))/sqrt(bp_P))) = 0 and
<a name="line_79"></a>  simplify(limit(x6,t=0)) = 0 and
<a name="line_80"></a>  simplify(limit(x6,t=1) + 1/bp_P) = 0,
<a name="line_81"></a>  "substitution for elliptic integral for am_period"
<a name="line_82"></a> );  
<a name="line_83"></a>
<a name="line_84"></a> _ASSERT(simplify(picard_fuchs_LA((z^5-A*z^3+z)^(-1/2)) - 
<a name="line_85"></a>                  diff((33*z^8-3*A*z^10-27*z^12)*(z^5-A*z^3+z)^(-7/2),z)) = 0,
<a name="line_86"></a>         "Picard-Fuchs exact differential");
<a name="line_87"></a>
<a name="line_88"></a> yp,xp := op(P_to_Ep_0([w,z]));
<a name="line_89"></a> dxp := diff(xp,z) * dz + diff(xp,w) * dw;
<a name="line_90"></a> 
<a name="line_91"></a> _ASSERT(simplify(dxp/yp - (1+z)*dz/w) = 0,"Pullback of omega^+");
<a name="line_92"></a>
<a name="line_93"></a> ym,xm := op(P_to_Em_0([w,z]));
<a name="line_94"></a> dxm := diff(xm,z) * dz + diff(xm,w) * dw;
<a name="line_95"></a> 
<a name="line_96"></a> _ASSERT(simplify(dxm/ym - (1+I)/sqrt(2)*(I-z)*dz/w) = 0,"Pullback of omega^-");
<a name="line_97"></a>
<a name="line_98"></a> wz := j_inv_P(c_P[5](t));
<a name="line_99"></a> wz_to_t := {w = wz[1],z = wz[2],dw = diff(wz[1],t)*dt,dz = diff(wz[2],t)*dt};
<a name="line_100"></a> alpha := combine(simplify(subs(wz_to_t,dxp/yp)));
<a name="line_101"></a> err := Int(factor(alpha/dt),t=0..2*Pi) - ap_period;
<a name="line_102"></a> err := max(seq(abs(evalf(subs(a_P=a,err))),a=0.1..0.9,0.1));
<a name="line_103"></a>
<a name="line_104"></a> _ASSERT(err < 10^(-95),"integral of dx/y over C[5]");
<a name="line_105"></a>
<a name="line_106"></a> wz := j_inv_P(c_P[6](t));
<a name="line_107"></a> wz_to_t := {w = wz[1],z = wz[2],dw = diff(wz[1],t)*dt,dz = diff(wz[2],t)*dt};
<a name="line_108"></a> alpha := combine(simplify(subs(wz_to_t,dxp/yp)));
<a name="line_109"></a> err := Int(factor(alpha/dt),t=0..2*Pi) - I*am_period;
<a name="line_110"></a> err := max(seq(abs(evalf(subs(a_P=a,err))),a=0.1..0.9,0.1));
<a name="line_111"></a>
<a name="line_112"></a> _ASSERT(err < 10^(-95),"integral of dx/y over C[6]");
<a name="line_113"></a>end:
<a name="line_114"></a>
<a name="line_115"></a>add_check(check_period_integrals):
<a name="line_116"></a>
<a name="line_117"></a>######################################################################
<a name="line_118"></a>
<a name="line_119"></a>check_periods := proc()
<a name="line_120"></a> local i,j,err;
<a name="line_121"></a>
<a name="line_122"></a> printf("%a()\n",procname);
<a name="line_123"></a>
<a name="line_124"></a> for i from 0 to 8 do
<a name="line_125"></a>  for j from 0 to 1 do
<a name="line_126"></a>   err := expand(p_period[i,j] - add(c_homology[i][k]*p_period[4+k,j],k=1..4));
<a name="line_127"></a>   _ASSERT(err = 0,sprintf("period p[%d,%d] consistent with homology",i,j));
<a name="line_128"></a>  od;
<a name="line_129"></a> od;
<a name="line_130"></a>end:
<a name="line_131"></a>
<a name="line_132"></a>add_check(check_periods):
  </pre>
 </body>
</html>
    