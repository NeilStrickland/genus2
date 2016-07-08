check_energy_formula := proc()
 local u,p,x0,z0,f0,n0,u0,v0,a0,b0,c0,Eu,E0,E1,Ju,J0,J1;

 printf("%a()\n",procname);

 # This is an essentially random choice of function
 u := [(1-1/2*z[1]-3/4*z[1]*z[2]+1/2*z[1]*z[2]^2)/(1+z[1]+5/2*z[1]*z[2]),
       1/sqrt(1+3/2*z[1]*z[2])/(1+z[1]+5/2*z[1]*z[2]),
       2*sqrt(1+z[2])*sqrt(1+3/2*z[1]*z[2])/(1+z[1]+5/2*z[1]*z[2])];
 p := unapply(eval(subs(z = zx0,u)) *~ s2p_core,x);

 # This is an essentially random choice of point in EX^*
 x0 := evalf(inner_quasirational_points[88]);

 z0 := z_proj1(x0);
 f0 := full_frame_b(x0):
 n0 := f0[2]:
 u0 := f0[3]:
 v0 := f0[4]:
 a0 := evalf(p(x0));
 b0 := evalf(subs(t=0,map(diff,p(x0 +~ t *~ u0),t)));
 c0 := evalf(subs(t=0,map(diff,p(x0 +~ t *~ v0),t)));
 E0 := nm3(b0)^2 + nm3(c0)^2;
 J0 := Determinant(Matrix([-a0,b0,c0]));
 Eu := E_to_S_energy(u):
 Ju := E_to_S_jacobian(u):
 E1 := evalf(Eu(z0));
 J1 := evalf(Ju(z0));

 _ASSERT(abs(E0 - E1) < 10.^(-90),"A particular case of E_to_S_energy()");
 _ASSERT(abs(J0 - J1) < 10.^(-90),"A particular case of E_to_S_jacobian()");

 [E0 - E1,J0-J1];
end:

add_check(check_energy_formula):

######################################################################

check_map_to_S2 := proc()

 printf("%a()\n",procname);

 _ASSERT(map_to_S2_basic_test(s2p_generic),       "s2p_generic passes basic tests");
 _ASSERT(map_to_S2_basic_test(s2p_poly),          "s2p_poly passes basic tests");
 _ASSERT(map_to_S2_basic_test(s2p_conformal_poly),"s2p_conformal_poly passes basic tests");

 _ASSERT(
  v0_conformal_test(s2p_conformal_poly) = [0,0],
  "s2p_conformal_poly is conformal to second order at v[0]"
 );

 _ASSERT(
  v3_conformal_test(s2p_conformal_poly) = 0,
  "s2p_conformal_poly is conformal to first order at v[3]"
 );

 _ASSERT(
  v6_conformal_test(s2p_conformal_poly) = 0,
  "s2p_conformal_poly is conformal to first order at v[6]"
 );

end:

add_check(check_map_to_S2):

