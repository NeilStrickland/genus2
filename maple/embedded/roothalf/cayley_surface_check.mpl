check_cayley_surface := proc()
 local x,xx,X,XX,T;

 printf("%a()\n",procname);

 xx := [x[1],x[2],x[3],x[4]];
 XX := [X[1],X[2],X[3],X[4]];

 _ASSERT(uncayley(cayley(xx)) -~ xx = [0$4] and
         cayley(uncayley(XX)) -~ XX = [0$4],
         "cayley and uncayley are inverse");

 _ASSERT(
   {seq(act_Cayley[T](cayley(xx)) -~ cayley(act_R4[T](xx)),T in G16)} = 
   {[0$4]},
   "cayley is equivariant");

 _ASSERT(expand(cayley_g(cayley(xx)) + 2*g0(xx)) = 0,
         "g via cayley");
end:

add_check(check_cayley_surface):
