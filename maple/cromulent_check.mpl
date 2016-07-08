# This file checks the internal consistency of various definitions
# given in the file cromulent.mpl.  

check_precromulent_defs := proc()
 local ij,iijj,i,j,m,c,ok,T;

 printf("%a()\n",procname);

 iijj := select(ij -> v_on_c[op(ij)] <> NULL,[indices(v_on_c)]);
 _ASSERT(
  `and`(op(map(evalb,map(ij -> c_gen[ij[2]](v_on_c[op(ij)]) = v_gen[ij[1]],iijj)))),
  "c_gen is consistent with v_on_c"
 );

 _ASSERT(
  `and`(seq(evalb(v_on_c[c_basepoint[k],k] <> NULL),k=0..8)),
  "c_basepoint[k] lies on C[k]"
 );

 _ASSERT(
  `and`(op(map(op,[seq(map(i -> evalb(act_V[c_involution[k]](i) = i),map(lhs,c_track[k])),k = 0 .. 8)]))),
  "c_involution[k] fixes all vertices on C[k]"
 );

 ok := true;
 for i from 0 to 8 do
  for T in [L,M,N] do
   j,m,c := op(act_c_data[T,i]);
   ok := ok and (act_Z4[T](c_homology[i]) -~ m *~ c_homology[j] = [0$4]);
  od;
 od;

 _ASSERT(ok,"Action on homology is consistent with action on curves");
end:

add_check(check_precromulent_defs):