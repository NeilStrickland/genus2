check_extra_vertices := proc()
 local i,j,k,t0,T,ok,err;

 printf("%a()\n",procname);

 for k from 0 to num_vertices_E-1 do
  _ASSERT(is_member_E(v_E[k]),sprintf("v[%d] is in EX(a)",k));

  for T in G16 do
   _ASSERT(
    act_E[T](v_E[k]) = v_E[v_E_permlist[T][k+1]],
    sprintf("Action of %A on v[%d]",T,k)
   );
  od:
 od;

 for i from 0 to num_vertices_E-1 do
  for j from 0 to num_curves_E-1 do
   t0 := v_on_c_E[i,j];
   if t0 <> NULL then
    if i < 22 then 
     err := simplify_E(v_E[i] -~ c_E[j](t0));
     ok := evalb(err = [0$4]);
    else
     err := v_E[i] -~ c_E[j](t0);
     err := max(seq(evalf(nm4(subs(a_E = 0.1*i,err))),i=1..7));
     ok := evalb (err < 10.^(-95));
    fi;
    _ASSERT(ok,sprintf("v[%2d] on c[%2d]",i,j));
   fi;
  od:
 od:
end:

add_check(check_extra_vertices):

