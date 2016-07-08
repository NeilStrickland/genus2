check_cayley := proc()
 local CL,i,j;

 printf("%a()\n",procname);
 
 _ASSERT(nops(cayley_lines) = 27,"27 lines");

 _ASSERT(
  map(ab -> simplify_E(g(ab[1] +~ t *~ ab[2])),cayley_lines) = [0$27],
  "The 27 lines all lie in the cubic surface"
 );

 CL := proc(i,u,v) 
  u *~ cayley_lines[i+1][1] +~ v *~ cayley_lines[i+1][2];
 end:

 _ASSERT( simplify_E(c_E[ 0](t) -~ CL(0,cos(t),sin(t))) = [0$4],
          "C[0] is a Cayley line");

 _ASSERT( simplify_E(c_E[ 1](t) -~ CL(1, sin(t)/sqrt(2),cos(t))) = [0$4],
          "C[1] is a Cayley line");

 _ASSERT( simplify_E(c_E[ 2](t) -~ CL(2,-sin(t)/sqrt(2),cos(t))) = [0$4],
          "C[2] is a Cayley line");

 _ASSERT( simplify_E(c_cayley[1](t) -~ CL(3, (1/3)*sqrt(3)*sqrt(-2*a_E^2+1)*sin(t)/sqrt(2*a_E^2+1),-cos(t))) = [0$4],
          "c_cayley[1] is a Cayley line");

 _ASSERT( simplify_E(c_cayley[2](t) -~ CL(4, cos(t), (1/3)*sqrt(3)*sqrt(-2*a_E^2+1)*sin(t)/sqrt(2*a_E^2+1))) = [0$4],
          "c_cayley[2] is a Cayley line");

 _ASSERT( simplify_E(c_cayley[3](t) -~ CL(5,-(1/3)*sqrt(3)*sqrt(-2*a_E^2+1)*sin(t)/sqrt(2*a_E^2+1), cos(t))) = [0$4],
          "c_cayley[3] is a Cayley line");

 _ASSERT( simplify_E(c_cayley[4](t) -~ CL(6,-cos(t),-(1/3)*sqrt(3)*sqrt(-2*a_E^2+1)*sin(t)/sqrt(2*a_E^2+1))) = [0$4],
          "c_cayley[4] is a Cayley line");

 for i from 0 to num_vertices_E - 1 do
  for j from 1 to 4 do 
   if v_on_c_cayley[i,j] <> NULL then
    _ASSERT(simplify_E(v_E[i] -~ c_cayley[j](v_on_c_cayley[i,j])) = [0$4],
	    sprintf("v[%d] on c_cayley[%d]",i,j));
   fi;
  od:
 od:
end:

add_check(check_cayley):