check_PK := proc()
 local i,T,U,t2,err,rr;

 printf("%a()\n",procname);

 _ASSERT(
  `and`(op(map(PK_check_zero,PK_rels))),
  "PK relations"
 );

 _ASSERT(
  {seq(seq(factor(act_PK[T](wz_reduce(a))-wz_reduce(act_PK[T](a))),T in G8),a in PK_vars)} = {0},
  "act_PK is consistent with expressions for t,u,v in terms of w and z"
 );

end:

add_check(check_PK):

######################################################################

check_PK_subfields := proc()
 local i;

 printf("%a()\n",procname);

 for i in sort(map(op,[indices(PK_subfields)])) do
  _ASSERT(PK_subfields[i]["check"],sprintf("PK_subfields[%d]",i));
 od;
end:

add_check(check_PK_subfields):

######################################################################

check_transition_maps := proc()
 local i,j,ij,x,y,tg,err0,err1;
 
 printf("%a()\n",procname);

 for i in [2,3,8,9,10] do
  tg[i] := PK_subfields[i]["generating_sets"][1][1];
 od:

 for ij in [[2,3],[2,8],[2,9],[2,10],[3,10],[8,10],[9,10]] do
  i,j := op(ij);
  _ASSERT(
   factor(wz_reduce(p_C[i,j](tg[i]) - tg[j])) = 0,
   sprintf("p_C[%d,%d] carries the generator of subfield %d to the generator of subfield %d",i,j,i,j)
  );
 od:
 
 _ASSERT(factor(p_C[3,10](p_C[2,3](z)) - p_C[2,10](z)) = 0,"p[3,10] o p[2,3] = p[2,10]");
 _ASSERT(factor(p_C[8,10](p_C[2,8](z)) - p_C[2,10](z)) = 0,"p[8,10] o p[2,8] = p[2,10]");
 _ASSERT(factor(p_C[9,10](p_C[2,9](z)) - p_C[2,10](z)) = 0,"p[9,10] o p[2,9] = p[2,10]");

 _ASSERT(convert(series(p_C[2, 3](z),z=0,3),polynom,z) - z^2   = 0,"p[2, 3] near v[0]");
 _ASSERT(convert(series(p_C[2, 8](z),z=0,3),polynom,z) - 2*z   = 0,"p[2, 8] near v[0]");
 _ASSERT(convert(series(p_C[2, 9](z),z=0,3),polynom,z) - 2*I*z = 0,"p[2, 9] near v[0]");
 _ASSERT(convert(series(p_C[2,10](z),z=0,3),polynom,z) - 2*z^2 = 0,"p[2,10] near v[0]");
 _ASSERT(convert(series(p_C[3,10](z),z=0,3),polynom,z) - 2*z   = 0,"p[3,10] near v[0]");
 _ASSERT(convert(series(p_C[8,10](z),z=0,3),polynom,z) - z^2/2 = 0,"p[8,10] near v[0]");
 _ASSERT(convert(series(p_C[9,10](z),z=0,3),polynom,z) + z^2/2 = 0,"p[9,10] near v[0]");

 assume(x::real, y::real);

 _ASSERT(factor(p_S2[2, 3](C_to_S2(x+I*y)) - C_to_S2(p_C[2, 3](x+I*y))) = [0,0,0],
         "p_S2[2, 3] is compatible with p_C[2, 3]");
	 
 _ASSERT(factor(p_S2[2, 8](C_to_S2(x+I*y)) - C_to_S2(p_C[2, 8](x+I*y))) = [0,0,0],
         "p_S2[2, 8] is compatible with p_C[2, 8]");
	 
 _ASSERT(factor(p_S2[2, 9](C_to_S2(x+I*y)) - C_to_S2(p_C[2, 9](x+I*y))) = [0,0,0],
         "p_S2[2, 9] is compatible with p_C[2, 9]");
	 
 _ASSERT(factor(p_S2[2,10](C_to_S2(x+I*y)) - C_to_S2(p_C[2,10](x+I*y))) = [0,0,0],
         "p_S2[2,10] is compatible with p_C[2,10]");

 _ASSERT(factor(p_S2[3,10](C_to_S2(x+I*y)) - C_to_S2(p_C[3,10](x+I*y))) = [0,0,0],
         "p_S2[3,10] is compatible with p_C[3,10]");
	 
 _ASSERT(factor(p_S2[8,10](C_to_S2(x+I*y)) - C_to_S2(p_C[8,10](x+I*y))) = [0,0,0],
         "p_S2[8,10] is compatible with p_C[8,10]");
	 
 _ASSERT(factor(p_S2[9,10](C_to_S2(x+I*y)) - C_to_S2(p_C[9,10](x+I*y))) = [0,0,0],
         "p_S2[9,10] is compatible with p_C[9,10]");
	 	 
end:

add_check(check_transition_maps):
