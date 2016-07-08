check_barycentric := proc()
 local P,P0,P1,P2,A,A0,t,tolerance,i;

 printf("%a()\n",procname);

 # We will use the following points as test data.
 # [P[1],P[2],P[3]] and [P[2],P[3],P[4]] are triangles of reasonable size,
 # not too far from being equilateral.  P[5] is in [P[1],P[2],P[3]].

 P[1] := [2902516/8526427, 903675/1218061,  sqrt(2) * 1424/3913, -2237104/8526427];
 P[2] := [681/1406,        5797/7030,       sqrt(2) * 37/190,    -344/3515];
 P[3] := [788/2665,        459/533,         sqrt(2) * 52/205,    -548/2665];
 P[4] := [214099/533478,   217940/266739,   sqrt(2) * 73/274,    -91469/533478];
 P[5] := [284246/653235,   1387235/1829058, sqrt(2) * 827/2590,  -1623401/9145290];

 for i from 1 to 5 do P0[i] := evalf(P[i]); od;
 A  := [P[1],P[2],P[3]];
 A0 := [P0[1],P0[2],P0[3]];

 t := barycentric_coords(A0,P[5]);

 P2[5] := barycentric_inverse(A0,t);
 P2[6] := barycentric_inverse(A0,[0,1/3,2/3]);

 tolerance := 10^(-98);

 _ASSERT(barycentric_coords(A,P[1]) = [1,0,0] and
         barycentric_coords(A,P[2]) = [0,1,0] and
         barycentric_coords(A,P[3]) = [0,0,1],
  "barycentric_coords on vertices"
 );

 _ASSERT(d4f(P0[5],P2[5]) < tolerance,
  "barycentric_inverse o barycentric_coords");

 _ASSERT(
  d3f(barycentric_coords(A0,P2[6]),[0,1/3,2/3]) < tolerance,
  "barycentric_coords o barycentric_inverse"
 );

 _ASSERT(
  d3f(barycentric_coords([P0[2],P0[3],P0[4]],P2[6]),[1/3,2/3,0]) < tolerance,
  "edge matching via barycentric_inverse"
 );
end:

add_check(check_barycentric):