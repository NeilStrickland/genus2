check_schwarz := proc()
 local err,i,u0,u1,u2,fs,T,t;

 printf("%a()\n",procname);

 _ASSERT(simplify(S_p_inv(z) - S_p_inv(-z)) = 0,"S_p_inv is even");

 _ASSERT(simplify(S_p_inv(1/z) - z^4*S_p_inv(z)) = 0,"S_p_inv(1/z)");

 _ASSERT(simplify(schwarz_phi_inv(schwarz_phi(z)) - z) = 0,"Inverse of schwarz_phi");

 _ASSERT(simplify(schwarz_psi_inv(schwarz_psi(z)) - z) = 0,"Inverse of schwarz_psi");

 _ASSERT(simplify(S_p1_inv(z) - subs(z = schwarz_phi_inv(z),S_p_inv(z)) * 
                                diff(schwarz_phi_inv(z),z)^2) = 0,
         "Schwarzian chain rule for p and p1");

 _ASSERT({seq(simplify_H(schwarz_psi(v_HS[i]) - v_H[i]),
          i in [0,1,2,3,6,10,11,12,13])} = {0},
	  "schwartz_psi(v_HS[i]) = v_H[i]");

 _ASSERT({seq(simplify_P(schwarz_phi(p_P(v_P[i])) - v_PS[i]),
          i in [0,2,3,6,10,11,12,13])} = {0},
	  "schwartz_phi(v_P[i]) = v_PS[i]");
	  
 _ASSERT(simplify(schwarz_psi( 1) + (1+I)/sqrt(2)) = 0,"schwarz_psi( 1)");
 _ASSERT(simplify(schwarz_psi(-1) - (1+I)/sqrt(2)) = 0,"schwarz_psi(-1)");

 _ASSERT(simplify_H(schwarz_psi(c_HS[0](t)) - c_H[0](t)) = 0,
         "schwarz_psi on C[0]");

 err := simplify_H(schwarz_psi(c_HS[1](t)) - c_H[1](t)):
 err := tidy(max(seq(abs(evalf(subs({a_H=a_H0,t=k},err))),k=-5..5)));
 _ASSERT(err = 0,"schwarz_psi on C[1]");

 err := simplify_H(schwarz_psi(c_HS[3](t)) - c_H[3](t)):
 err := tidy(max(seq(abs(evalf(subs({a_H=a_H0,t=k},err))),k=-5..5)));
 _ASSERT(err = 0,"schwarz_psi on C[3]");

 err := simplify_H(schwarz_psi(c_HS[5](t)) - c_H[5](t)):
 err := tidy(max(seq(abs(evalf(subs({a_H=a_H0,t=k},err))),k=-5..5)));
 _ASSERT(err = 0,"schwarz_psi on C[5]");

 assume(u0>0 and u1>0 and u2>0 and t>0);
 T := series_circle_fit(u0,u1,u2);
 fs := (t) -> u0 + I*u1*t + u2*t^2;
 err := (T["d"] - T["c"]*fs(t))*(T["d"] - T["c"]*fs(-t)) - T["r"]^2;
 err := simplify(expand(err));
 err := rem(err,t^3,t);

 _ASSERT(err = 0,"series_circle_fit");

end:

add_check(check_schwarz):

######################################################################

check_heun := proc()
 local err;

 printf("%a()\n",procname);

 err := S_p1_inv(schwarz_xi_inv(Z)) * D(schwarz_xi_inv)(Z)^2 +
        schwarzian(schwarz_xi_inv(Z),Z) - S_p2_inv(Z);

 err := factor(expand(rationalize(simplify(err))));

 _ASSERT(err = 0,"S_p2_inv is consistent with the schwarzian chain rule");

 err := D(D(heun_F0))(Z) + heun_F0(Z) * S_p2_inv(Z)/2;

 err := factor(expand(rationalize(simplify(err))));

 _ASSERT(err = 0,"heun_F0 solves the relevant schwarzian differential equation");

 err := D(D(heun_F1))(Z) + heun_F1(Z) * S_p2_inv(Z)/2;

 err := factor(expand(rationalize(simplify(err))));

 _ASSERT(err = 0,"heun_F1 solves the relevant schwarzian differential equation");
end:

add_check(check_heun):