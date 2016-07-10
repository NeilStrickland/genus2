check_homology := proc()
 local i,j,T,U,V,m,c,n,nn,mm;

 printf("%a()\n",procname);

 for T in [L,M,N] do
  for i from 0 to 8 do 
   j,m,c := op(act_c_data[T,i]);
   _ASSERT(
    act_Z4[T](c_homology[i]) -~ m *~ c_homology[j] = [0$4],
    sprintf("homology action of %A on c[%d]",T,i));
  od;
 od:

 unassign('m');

 nn := [n[1],n[2],n[3],n[4]];
 mm := [m[1],m[2],m[3],m[4]];

 _ASSERT(
  `and`(seq(seq(
    act_Z4[T](act_Z4[U](nn)) -~ act_Z4[V](nn) = [0$4],
     U in G16),T in G16)),
  "Action on homology is associative"
 );

 _ASSERT({
  act_Z4[L](homology_u[1,1]) -~ homology_u[1,2],
  act_Z4[L](homology_u[1,2]) +~ homology_u[1,1],
  act_Z4[L](homology_u[2,1]) -~ homology_u[2,2],
  act_Z4[L](homology_u[2,2]) +~ homology_u[2,1],
  act_Z4[M](homology_u[1,1]) -~ homology_u[1,1],
  act_Z4[M](homology_u[1,2]) +~ homology_u[1,2],
  act_Z4[M](homology_u[2,1]) +~ homology_u[2,1],
  act_Z4[M](homology_u[2,2]) -~ homology_u[2,2],
  act_Z4[N](homology_u[1,1]) -~ homology_u[1,1],
  act_Z4[N](homology_u[1,2]) +~ homology_u[1,2],
  act_Z4[N](homology_u[2,1]) -~ homology_u[2,1],
  act_Z4[N](homology_u[2,2]) +~ homology_u[2,2]
  } = {[0,0,0,0]},
  "action on alternative homology basis"
 );

 _ASSERT(
  {seq(expand(cap_product(act_Z4[T](nn),act_Z4[T](mm)) - 
              character[7][T] * cap_product(nn,mm)),
       T in G16)} = {0},
  "cap product is equivariant"
 );

 _ASSERT(
  homology_theta_p(act_Z4[ M](nn)) -~ homology_theta_p(nn) = [0,0],
  "homology_theta_p coequalises M"
 );
 
 _ASSERT(
  homology_theta_m(act_Z4[LM](nn)) -~ homology_theta_m(nn) = [0,0],
  "homology_theta_m coequalises M"
 );
 
 NULL;
end:

add_check(check_homology):
