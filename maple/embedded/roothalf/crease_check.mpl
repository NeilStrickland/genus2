check_crease := proc()
 local t,w,err;

 printf("%a()\n",procname);

 w := crease(t):

 _ASSERT(simplify(rho(w)-1) = 0,"crease(t) is in S3");
 _ASSERT(simplify(g0(w)) = 0,"crease(t) is in EX^*");
 err := simplify(expand(w[3]*dg0(w)[4] - w[4]*dg0(w)[3])):
 _ASSERT(err = 0,"crease is singular");

 _ASSERT(simplify(act_R4[LLLN](crease(t)) -~ crease(-t)) = [0$4],
         "crease is LLLN-equivariant");

 _ASSERT(simplify(crease_eq(crease(t))) = 0,
         "crease_eq o crease = 0");

 w := crease_trig(t):

 _ASSERT(simplify(rho(w)-1) = 0,"crease_trig(t) is in S3");
 _ASSERT(simplify(g0(w)) = 0,"crease_trig(t) is in EX^*");
 err := simplify(expand(w[3]*dg0(w)[4] - w[4]*dg0(w)[3])):
 _ASSERT(err = 0,"crease_trig is singular");

 _ASSERT(simplify(act_R4[L](crease_trig(t)) -~ crease_trig(t+Pi/2)) = [0$4],
         "crease_trig is L-equivariant");

 _ASSERT(simplify(act_R4[N](crease_trig(t)) -~ crease_trig(-t)) = [0$4],
         "crease_trig is N-equivariant");

 _ASSERT(simplify(crease_eq(crease(t))) = 0,
         "crease_eq o crease = 0");



 # We now check that the projection x |-> [x[1],x[2]] is singular
 # on the image of the crease map.


 _ASSERT(simplify(crease_eq([r*cos(t),r*sin(t)]) - crease_eq_polar(r,t)) = 0,
         "two versions of crease_eq");
end:

add_check(check_crease):
