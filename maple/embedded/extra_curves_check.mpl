check_extra_curves := proc()
 local k,d,s,s0,t,err,check_alg;

 printf("%a()\n",procname);

 for k from 0 to num_curves_E-1 do
  _ASSERT(is_member_E(c_E[k](t)),sprintf("c[%d](t) is in EX",k));
 od;

 _ASSERT(is_member_E(c_algebraic(t)),"c_algebraic(t) is in EX");

 err := simplify(c_algebraic(-sqrt((1/a_E^2-1)/2)) -~ v_E[10]);
 err := max(seq(op(map(abs,evalf(subs(a_E=0.1*i,err)))),i=1..9));
 _ASSERT(err < 10.^(-90),"v[10] on c_algebraic");

 err := simplify(c_algebraic( sqrt((1/a_E^2-1)/2)) -~ v_E[11]);
 err := max(seq(op(map(abs,evalf(subs(a_E=0.1*i,err)))),i=1..9));
 _ASSERT(err < 10.^(-90),"v[11] on c_algebraic");

 _ASSERT(simplify(c_algebraic( 0)         - v_E[ 0])=[0$4],"v[ 0] on c_algebraic");
 _ASSERT(simplify(c_algebraic( 1/2/a_E)   - v_E[ 3])=[0$4],"v[ 3] on c_algebraic");

 err :=  simplify_E(factor(combine(c_algebraic(tau_algebraic[6](t)) -~ c_E[6](t))))
           assuming t>0 and t < Pi;

 _ASSERT(
  err[1] = 0 and err[3] = 0 and err[4] = 0 and
  max(seq(seq(abs(evalf(subs({a_E=0.1*i,t=Pi/10*j},err[2]))),i=1..9),j=0..10)) < 10^(-99),
  "c_E[6] = c_algebraic o tau_algebraic[6] on (0,Pi)"
 );
 
 for k from 9 to 12 do
  _ASSERT(c_check_E[k](c_E[k](t)) = true,sprintf("c[%d](t) is in C[%d]",k,k));
 od:

 for k from 13 to 16 do
  _ASSERT(`and`(seq(c_check_E[k](c_E[k](i*Pi/12)),i=0..23)),sprintf("c[%d](t) is in C[%d]",k,k));
 od;

 for k from 9 to 16 do 
  err := simplify(c_param_E[k](c_E[k](t)) - t) assuming t > -Pi and t < Pi;
  _ASSERT(err = 0,sprintf("c_param_E[%d]",k));
 od:

 _ASSERT(
  select(i -> c_check_E[1](v_E[i]) and c_check_E[9](v_E[i]),
	 {seq(i,i=0..num_vertices_E-1)}) = {14,16},
  "C[1] intersect C[9] = {v[14],v[16]}"
 );

end:

add_check(check_extra_curves):

