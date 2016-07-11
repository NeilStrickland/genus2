check_square_diffeo_E0 := proc()
 local err,vc,NN,ok,i,j,x0;

 printf("%a()\n",procname);

 _ASSERT( 
  simplify(square_diffeo_E0(c_E0[0](t))[1]) = 0,
  "square_diffeo_E0(C[0])"
 );

 _ASSERT(
  simplify(square_diffeo_E0(c_E0[1](t))[2]) = 0,
  "square_diffeo_E0(C[1])"
 );

 _ASSERT(
  simplify(1 - square_diffeo_E0(c_E0[3](t))[2]) = 0,
  "square_diffeo_E0(C[3])"
 );
 
 err := 1 - square_diffeo_E0(c_alt[5](t))[1];
 err := factor(simplify(err))  assuming t > 0 and t < Pi;

 _ASSERT(err=0,"square_diffeo_E0(C[5])");

 NN := 64;
 ok := true;

 for i from 0 to NN do
  for j from 0 to NN do
   x0 := evalf(t_lift([i/NN,j/NN]));
   if evalf(square_diffeo_E0_J(x0)) < 0.1 then
    ok := false;
    break;
   fi;   
  od;
  if not ok then break; fi;
 od;

 _ASSERT(ok,"Jacobian of square_diffeo_E0 is >= 0.1 everywhere in F16");
end:

add_check(check_square_diffeo_E0):

######################################################################

check_square_diffeo_E0_alt := proc()
 local err,vc,NN,ok,i,j,x0;

 printf("%a()\n",procname);

 _ASSERT( 
  simplify(square_diffeo_E0_alt(c_E0[0](t))[1]) = 0,
  "square_diffeo_E0_alt(C[0])"
 );

 _ASSERT(
  simplify(square_diffeo_E0_alt(c_E0[1](t))[2]) = 0,
  "square_diffeo_E0_alt(C[1])"
 );

 _ASSERT(
  simplify(1 - square_diffeo_E0_alt(c_E0[3](t))[2]) = 0,
  "square_diffeo_E0_alt(C[3])"
 );
 
 _ASSERT(
  simplify(1 - square_diffeo_E0_alt(c_E0[5](t))[1]) = 0,
  "square_diffeo_E0_alt(C[5])"
 );
 
 NN := 64;
 ok := true;

 for i from 0 to NN do
  for j from 0 to NN do
   x0 := evalf(t_lift([i/NN,j/NN]));
   if evalf(square_diffeo_E0_alt_J(x0)) < 0.1 then
    ok := false;
    break;
   fi;   
  od;
  if not ok then break; fi;
 od;

 _ASSERT(ok,"Jacobian of square_diffeo_E0_alt is >= 0.1 everywhere in F16");
end:

add_check(check_square_diffeo_E0_alt):
