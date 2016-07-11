check_geodesic_H := proc()
 local i,j,err0,err1,err2,A0,A1,A2,A3;

 printf("%a()\n",procname);

 i,j := 0,3;
 err0 := simplify(geodesic_H[i,j](geodesic_start_H[i,j]) - v_H[i]):
 _ASSERT(err0 = 0,"geodesic passes through v[0]");

 err1 := simplify(geodesic_H[i,j](geodesic_end_H[i,j]) - v_H[j]):
 _ASSERT(err1 = 0,"geodesic passes through v[3]");

 err2 := simplify(act_Pi(geodesic_shift_H[i,j],geodesic_H[i,j](t)) -
                   geodesic_H[i,j](geodesic_period_H[i,j] + t)):
 _ASSERT(err2 = 0,"geodesic has shifted periodicity");

 A0 := -a_H^2*sqrt(a_H^2+1)+sqrt(2*a_H^4-a_H^2+1);
 A1 := (1/2*( sqrt(2*a_H^4-a_H^2+1)*sqrt(a_H^2+1)-a_H^2+1))*sqrt(2);
 A2 := a_H^6-2*sqrt(2*a_H^4-a_H^2+1)*sqrt(a_H^2+1)*a_H^2+3*a_H^4-a_H^2+1;
 A3 := a_H^6-sqrt(2*a_H^4-a_H^2+1)*sqrt(a_H^2+1)*a_H^2+a_H^4+sqrt(2*a_H^4-a_H^2+1)*sqrt(a_H^2+1)-a_H^2+1;

 _ASSERT(expand(A2 - A0^2) = 0,"auxiliary square root 1");
 _ASSERT(expand(A3 - A1^2) = 0,"auxiliary square root 2");

 i,j := 6,11;
 err0 := simplify(geodesic_H[i,j](geodesic_start_H[i,j]) - v_H[i]):
 err0 := simplify(subs(sqrt(A2)=A0,err0));
 _ASSERT(err0 = 0,"geodesic passes through v[6]");

 err1 := simplify(geodesic_H[i,j](geodesic_end_H[i,j]) - v_H[j]):
 err1 := simplify(subs(sqrt(A3)=A1,err1));
 _ASSERT(err1 = 0,"geodesic passes through v[11]");

 err2 := simplify(act_Pi(geodesic_shift_H[i,j],geodesic_H[i,j](t)) -
                   geodesic_H[i,j](geodesic_period_H[i,j] + t)):
 _ASSERT(err2 = 0,"geodesic has shifted periodicity");
end:

add_check(check_geodesic_H):