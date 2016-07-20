<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_schwarz := proc()
<a name="line_2"></a> local err,i,u0,u1,u2,fs,T,t;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> _ASSERT(simplify(S_p_inv(z) - S_p_inv(-z)) = 0,"S_p_inv is even");
<a name="line_7"></a>
<a name="line_8"></a> _ASSERT(simplify(S_p_inv(1/z) - z^4*S_p_inv(z)) = 0,"S_p_inv(1/z)");
<a name="line_9"></a>
<a name="line_10"></a> _ASSERT(simplify(schwarz_phi_inv(schwarz_phi(z)) - z) = 0,"Inverse of schwarz_phi");
<a name="line_11"></a>
<a name="line_12"></a> _ASSERT(simplify(schwarz_psi_inv(schwarz_psi(z)) - z) = 0,"Inverse of schwarz_psi");
<a name="line_13"></a>
<a name="line_14"></a> _ASSERT(simplify(S_p1_inv(z) - subs(z = schwarz_phi_inv(z),S_p_inv(z)) * 
<a name="line_15"></a>                                diff(schwarz_phi_inv(z),z)^2) = 0,
<a name="line_16"></a>         "Schwarzian chain rule for p and p1");
<a name="line_17"></a>
<a name="line_18"></a> _ASSERT({seq(simplify_H(schwarz_psi(v_HS[i]) - v_H[i]),
<a name="line_19"></a>          i in [0,1,2,3,6,10,11,12,13])} = {0},
<a name="line_20"></a>	  "schwartz_psi(v_HS[i]) = v_H[i]");
<a name="line_21"></a>
<a name="line_22"></a> _ASSERT({seq(simplify_P(schwarz_phi(p_P(v_P[i])) - v_PS[i]),
<a name="line_23"></a>          i in [0,2,3,6,10,11,12,13])} = {0},
<a name="line_24"></a>	  "schwartz_phi(v_P[i]) = v_PS[i]");
<a name="line_25"></a>	  
<a name="line_26"></a> _ASSERT(simplify(schwarz_psi( 1) + (1+I)/sqrt(2)) = 0,"schwarz_psi( 1)");
<a name="line_27"></a> _ASSERT(simplify(schwarz_psi(-1) - (1+I)/sqrt(2)) = 0,"schwarz_psi(-1)");
<a name="line_28"></a>
<a name="line_29"></a> _ASSERT(simplify_H(schwarz_psi(c_HS[0](t)) - c_H[0](t)) = 0,
<a name="line_30"></a>         "schwarz_psi on C[0]");
<a name="line_31"></a>
<a name="line_32"></a> err := simplify_H(schwarz_psi(c_HS[1](t)) - c_H[1](t)):
<a name="line_33"></a> err := tidy(max(seq(abs(evalf(subs({a_H=a_H0,t=k},err))),k=-5..5)));
<a name="line_34"></a> _ASSERT(err = 0,"schwarz_psi on C[1]");
<a name="line_35"></a>
<a name="line_36"></a> err := simplify_H(schwarz_psi(c_HS[3](t)) - c_H[3](t)):
<a name="line_37"></a> err := tidy(max(seq(abs(evalf(subs({a_H=a_H0,t=k},err))),k=-5..5)));
<a name="line_38"></a> _ASSERT(err = 0,"schwarz_psi on C[3]");
<a name="line_39"></a>
<a name="line_40"></a> err := simplify_H(schwarz_psi(c_HS[5](t)) - c_H[5](t)):
<a name="line_41"></a> err := tidy(max(seq(abs(evalf(subs({a_H=a_H0,t=k},err))),k=-5..5)));
<a name="line_42"></a> _ASSERT(err = 0,"schwarz_psi on C[5]");
<a name="line_43"></a>
<a name="line_44"></a> assume(u0>0 and u1>0 and u2>0 and t>0);
<a name="line_45"></a> T := series_circle_fit(u0,u1,u2);
<a name="line_46"></a> fs := (t) -> u0 + I*u1*t + u2*t^2;
<a name="line_47"></a> err := (T["d"] - T["c"]*fs(t))*(T["d"] - T["c"]*fs(-t)) - T["r"]^2;
<a name="line_48"></a> err := simplify(expand(err));
<a name="line_49"></a> err := rem(err,t^3,t);
<a name="line_50"></a>
<a name="line_51"></a> _ASSERT(err = 0,"series_circle_fit");
<a name="line_52"></a>
<a name="line_53"></a>end:
<a name="line_54"></a>
<a name="line_55"></a>add_check(check_schwarz):
<a name="line_56"></a>
<a name="line_57"></a>######################################################################
<a name="line_58"></a>
<a name="line_59"></a>check_heun := proc()
<a name="line_60"></a> local err;
<a name="line_61"></a>
<a name="line_62"></a> printf("%a()\n",procname);
<a name="line_63"></a>
<a name="line_64"></a> err := S_p1_inv(schwarz_xi_inv(Z)) * D(schwarz_xi_inv)(Z)^2 +
<a name="line_65"></a>        schwarzian(schwarz_xi_inv(Z),Z) - S_p2_inv(Z);
<a name="line_66"></a>
<a name="line_67"></a> err := factor(expand(rationalize(simplify(err))));
<a name="line_68"></a>
<a name="line_69"></a> _ASSERT(err = 0,"S_p2_inv is consistent with the schwarzian chain rule");
<a name="line_70"></a>
<a name="line_71"></a> err := D(D(heun_F0))(Z) + heun_F0(Z) * S_p2_inv(Z)/2;
<a name="line_72"></a>
<a name="line_73"></a> err := factor(expand(rationalize(simplify(err))));
<a name="line_74"></a>
<a name="line_75"></a> _ASSERT(err = 0,"heun_F0 solves the relevant schwarzian differential equation");
<a name="line_76"></a>
<a name="line_77"></a> err := D(D(heun_F1))(Z) + heun_F1(Z) * S_p2_inv(Z)/2;
<a name="line_78"></a>
<a name="line_79"></a> err := factor(expand(rationalize(simplify(err))));
<a name="line_80"></a>
<a name="line_81"></a> _ASSERT(err = 0,"heun_F1 solves the relevant schwarzian differential equation");
<a name="line_82"></a>end:
<a name="line_83"></a>
<a name="line_84"></a>add_check(check_heun):  </pre>
 </body>
</html>
    