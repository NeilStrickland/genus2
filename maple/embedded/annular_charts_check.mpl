check_annular_charts := proc()
 local A0,A1,A2;

 printf("%a()\n",procname);

 A0 := annular_chart[0]([t,u]);
 A1 := map(diff,A0,t);
 A2 := map(diff,A0,u);

 _ASSERT(
  convert(series(combine(simplify(rho(A0)-1)),u=0,4),polynom,u) = 0,
  "annular_chart[0] lands in S3"
 );

 _ASSERT(
  convert(series(combine(simplify(g(A0))),u=0,5),polynom,u) = 0,
  "annular_chart[0] lands in EX(a)"
 );

 _ASSERT(
  convert(series(combine(simplify(dp4(A1,A2))),u=0,3),polynom,u) = 0,
  "annular_chart[0] is approximately conformal 1"
 );

 _ASSERT(
  convert(series(combine(simplify(dp4(A1,A1)-dp4(A2,A2))),u=0,4),polynom,u) = 0,
  "annular_chart[0] is approximately conformal 2"
 );

 A0 := annular_chart[1]([t,u]);
 A1 := map(diff,A0,t);
 A2 := map(diff,A0,u);

 _ASSERT(
  convert(series(combine(simplify(rho(A0)-1)),u=0,4),polynom,u) = 0,
  "annular_chart[1] lands in S3"
 );

 _ASSERT(
  convert(series(combine(simplify(g(A0))),u=0,3),polynom,u) = 0,
  "annular_chart[1] lands in EX(a)"
 );

 _ASSERT(
  combine(simplify(dp4(A1,A2))) = 0,
  "annular_chart[1] is approximately conformal 1"
 );

 _ASSERT(
  convert(series(combine(simplify(dp4(A1,A1)-dp4(A2,A2))),u=0,2),polynom,u) = 0,
  "annular_chart[1] is approximately conformal 2"
 );

end:

add_check(check_annular_charts);
