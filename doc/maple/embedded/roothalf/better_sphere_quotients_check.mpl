<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_energy_formula := proc()
<a name="line_2"></a> local u,p,x0,z0,f0,n0,u0,v0,a0,b0,c0,Eu,E0,E1,Ju,J0,J1;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> # This is an essentially random choice of function
<a name="line_7"></a> u := [(1-1/2*z[1]-3/4*z[1]*z[2]+1/2*z[1]*z[2]^2)/(1+z[1]+5/2*z[1]*z[2]),
<a name="line_8"></a>       1/sqrt(1+3/2*z[1]*z[2])/(1+z[1]+5/2*z[1]*z[2]),
<a name="line_9"></a>       2*sqrt(1+z[2])*sqrt(1+3/2*z[1]*z[2])/(1+z[1]+5/2*z[1]*z[2])];
<a name="line_10"></a> p := unapply(eval(subs(z = zx0,u)) *~ s2p_core,x);
<a name="line_11"></a>
<a name="line_12"></a> # This is an essentially random choice of point in EX^*
<a name="line_13"></a> x0 := evalf(inner_quasirational_points[88]);
<a name="line_14"></a>
<a name="line_15"></a> z0 := z_proj1(x0);
<a name="line_16"></a> f0 := full_frame_b(x0):
<a name="line_17"></a> n0 := f0[2]:
<a name="line_18"></a> u0 := f0[3]:
<a name="line_19"></a> v0 := f0[4]:
<a name="line_20"></a> a0 := evalf(p(x0));
<a name="line_21"></a> b0 := evalf(subs(t=0,map(diff,p(x0 +~ t *~ u0),t)));
<a name="line_22"></a> c0 := evalf(subs(t=0,map(diff,p(x0 +~ t *~ v0),t)));
<a name="line_23"></a> E0 := nm3(b0)^2 + nm3(c0)^2;
<a name="line_24"></a> J0 := Determinant(Matrix([-a0,b0,c0]));
<a name="line_25"></a> Eu := E_to_S_energy(u):
<a name="line_26"></a> Ju := E_to_S_jacobian(u):
<a name="line_27"></a> E1 := evalf(Eu(z0));
<a name="line_28"></a> J1 := evalf(Ju(z0));
<a name="line_29"></a>
<a name="line_30"></a> _ASSERT(abs(E0 - E1) < 10.^(-90),"A particular case of E_to_S_energy()");
<a name="line_31"></a> _ASSERT(abs(J0 - J1) < 10.^(-90),"A particular case of E_to_S_jacobian()");
<a name="line_32"></a>
<a name="line_33"></a> [E0 - E1,J0-J1];
<a name="line_34"></a>end:
<a name="line_35"></a>
<a name="line_36"></a>add_check(check_energy_formula):
<a name="line_37"></a>
<a name="line_38"></a>######################################################################
<a name="line_39"></a>
<a name="line_40"></a>check_map_to_S2 := proc()
<a name="line_41"></a>
<a name="line_42"></a> printf("%a()\n",procname);
<a name="line_43"></a>
<a name="line_44"></a> _ASSERT(map_to_S2_basic_test(s2p_generic),       "s2p_generic passes basic tests");
<a name="line_45"></a> _ASSERT(map_to_S2_basic_test(s2p_poly),          "s2p_poly passes basic tests");
<a name="line_46"></a> _ASSERT(map_to_S2_basic_test(s2p_conformal_poly),"s2p_conformal_poly passes basic tests");
<a name="line_47"></a>
<a name="line_48"></a> _ASSERT(
<a name="line_49"></a>  v0_conformal_test(s2p_conformal_poly) = [0,0],
<a name="line_50"></a>  "s2p_conformal_poly is conformal to second order at v[0]"
<a name="line_51"></a> );
<a name="line_52"></a>
<a name="line_53"></a> _ASSERT(
<a name="line_54"></a>  v3_conformal_test(s2p_conformal_poly) = 0,
<a name="line_55"></a>  "s2p_conformal_poly is conformal to first order at v[3]"
<a name="line_56"></a> );
<a name="line_57"></a>
<a name="line_58"></a> _ASSERT(
<a name="line_59"></a>  v6_conformal_test(s2p_conformal_poly) = 0,
<a name="line_60"></a>  "s2p_conformal_poly is conformal to first order at v[6]"
<a name="line_61"></a> );
<a name="line_62"></a>
<a name="line_63"></a>end:
<a name="line_64"></a>
<a name="line_65"></a>add_check(check_map_to_S2):
<a name="line_66"></a>
  </pre>
 </body>
</html>
    