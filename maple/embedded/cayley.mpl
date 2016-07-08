# This file finds the 27 lines on the cubic surface g = 0.

#@ find_cayley_lines 
find_cayley_lines := proc() 
 local a,aa,aa0,b,bb,bb0,rels,cell_rules,all_sols,sols,R;
 global cayley_lines;

 # aa and bb span a Cayley line if the terms in rels are all zero.
 aa := [seq(a[i],i=1..4)];
 bb := [seq(b[i],i=1..4)];
 rels := [coeffs(expand(g(aa +~ t *~ bb)),t)];

 # We can describe each Cayley line in terms of its row-reduced
 # basis.  This divides the relevant Grassmannian into six Schubert
 # cells, as follows.
 cell_rules := [
  {a[1]=1,a[2]=0,b[1]=0,b[2]=1},
  {a[1]=1,a[3]=0,b[1]=0,b[2]=0,b[3]=1},
  {a[1]=1,a[4]=0,b[1]=0,b[2]=0,b[3]=0,b[4]=1},
  {a[1]=0,a[2]=1,a[3]=0,b[1]=0,b[2]=0,b[3]=1},
  {a[1]=0,a[2]=1,a[4]=0,b[1]=0,b[2]=0,b[3]=0,b[4]=1},
  {a[1]=0,a[2]=0,a[3]=1,a[4]=0,b[1]=0,b[2]=0,b[3]=0,b[4]=1}
 ]:

 # We now find the solutions in each cell.

 all_sols := NULL:
 for R in cell_rules do
  aa0 := subs(R,aa);
  bb0 := subs(R,bb);
  sols := [solve(subs(R,rels),{op(aa0),op(bb0)} minus {0,1})];
  sols := map(s -> simplify_E(subs(s,[aa0,bb0])),sols);
  all_sols := all_sols,op(sols);
 od:
 cayley_lines := {all_sols}:

 return cayley_lines;
end:

######################################################################

# The procedure below executes automatically because of the () at the end.
# This sets cayley_lines in essentially the same way as find_cayley_lines(),
# but the result is ordered in a more structured way and annotated.

#@ cayley_lines
proc()
 global cayley_lines;
 local p1,p2;

 p1 := sqrt(-4*a_E^2+2);
 p2 := sqrt(-4*a_E^4+6*a_E^2-2);

 cayley_lines := [
  [[1, 0, 0, 0],
   [0, 1, 0, 0]], # C[0]
  [[1, 1, 0, 0],
   [0, 0, 1, 0]], # C[1]
  [[1,-1, 0, 0],
   [0, 0, 1, 0]], # C[2]
  [[1, 0, -4*a_E/p1,  2/p1],
   [0, 1, 0, 0]],
  [[1, 0, 0, 0],
   [0, 1, -4*a_E/p1, -2/p1]],
  [[1, 0,  4*a_E/p1, -2/p1], [0, 1, 0, 0]],
  [[1, 0, 0, 0], [0, 1,  4*a_E/p1,  2/p1]],
  [[1, 0, -(1/2)*(I*sqrt(2)-p1)*a_E/(a_E^2-1),  (1/2*I)*sqrt(2)],
   [0, 1, -(1/2)*(I*sqrt(2)*p1+4*a_E^2-2)*a_E/(p1*(a_E^2-1)), -(1/2*I)*sqrt(2)]],
  [[1, 0, -(1/2)*(I*sqrt(2)-p1)*a_E/(a_E^2-1),  (1/2*I)*sqrt(2)],
   [0, 1,  (1/2)*(I*sqrt(2)*p1+4*a_E^2-2)*a_E/(p1*(a_E^2-1)),  (1/2*I)*sqrt(2)]],
  [[1, 0,  (1/2)*(I*sqrt(2)-p1)*a_E/(a_E^2-1), -(1/2*I)*sqrt(2)],
   [0, 1, -(1/2)*(I*sqrt(2)*p1+4*a_E^2-2)*a_E/(p1*(a_E^2-1)), -(1/2*I)*sqrt(2)]],
  [[1, 0,  (1/2)*(I*sqrt(2)-p1)*a_E/(a_E^2-1), -(1/2*I)*sqrt(2)],
   [0, 1,  (1/2)*(I*sqrt(2)*p1+4*a_E^2-2)*a_E/(p1*(a_E^2-1)),  (1/2*I)*sqrt(2)]],
  [[1, 0, -(1/2)*(I*sqrt(2)+p1)*a_E/(a_E^2-1),  (1/2*I)*sqrt(2)],
   [0, 1, -(1/2)*(I*sqrt(2)*p1-4*a_E^2+2)*a_E/(p1*(a_E^2-1)), -(1/2*I)*sqrt(2)]],
  [[1, 0, -(1/2)*(I*sqrt(2)+p1)*a_E/(a_E^2-1),  (1/2*I)*sqrt(2)],
   [0, 1,  (1/2)*(I*sqrt(2)*p1-4*a_E^2+2)*a_E/(p1*(a_E^2-1)),  (1/2*I)*sqrt(2)]],
  [[1, 0,  (1/2)*(I*sqrt(2)+p1)*a_E/(a_E^2-1), -(1/2*I)*sqrt(2)],
   [0, 1, -(1/2)*(I*sqrt(2)*p1-4*a_E^2+2)*a_E/(p1*(a_E^2-1)), -(1/2*I)*sqrt(2)]],
  [[1, 0,  (1/2)*(I*sqrt(2)+p1)*a_E/(a_E^2-1), -(1/2*I)*sqrt(2)],
   [0, 1,  (1/2)*(I*sqrt(2)*p1-4*a_E^2+2)*a_E/(p1*(a_E^2-1)),  (1/2*I)*sqrt(2)]],
  [[1, 0, -(1/2)*(-2*a_E^2+p2+2)*a_E/(a_E^2-1)^(3/2),  1/(2*sqrt(a_E^2-1))],
   [0, 1,  a_E*(-2*a_E^2+1+p2)/(sqrt(a_E^2-1)*p2), -1/(2*sqrt(a_E^2-1))]],
  [[1, 0, -(1/2)*(-2*a_E^2+p2+2)*a_E/(a_E^2-1)^(3/2),  1/(2*sqrt(a_E^2-1))],
   [0, 1, -a_E*(-2*a_E^2+1+p2)/(sqrt(a_E^2-1)*p2),  1/(2*sqrt(a_E^2-1))]],
  [[1, 0,  (1/2)*(-2*a_E^2+p2+2)*a_E/(a_E^2-1)^(3/2), -1/(2*sqrt(a_E^2-1))],
   [0, 1,  a_E*(-2*a_E^2+1+p2)/(sqrt(a_E^2-1)*p2), -1/(2*sqrt(a_E^2-1))]],
  [[1, 0,  (1/2)*(-2*a_E^2+p2+2)*a_E/(a_E^2-1)^(3/2), -1/(2*sqrt(a_E^2-1))],
   [0, 1, -a_E*(-2*a_E^2+1+p2)/(sqrt(a_E^2-1)*p2),  1/(2*sqrt(a_E^2-1))]],
  [[1, 0, -(1/2)*( 2*a_E^2+p2-2)*a_E/(a_E^2-1)^(3/2), -1/(2*sqrt(a_E^2-1))],
   [0, 1,  a_E*( 2*a_E^2-1+p2)/(sqrt(a_E^2-1)*p2), -1/(2*sqrt(a_E^2-1))]],
  [[1, 0, -(1/2)*( 2*a_E^2+p2-2)*a_E/(a_E^2-1)^(3/2), -1/(2*sqrt(a_E^2-1))],
   [0, 1, -a_E*( 2*a_E^2-1+p2)/(sqrt(a_E^2-1)*p2),  1/(2*sqrt(a_E^2-1))]],
  [[1, 0,  (1/2)*( 2*a_E^2+p2-2)*a_E/(a_E^2-1)^(3/2),  1/(2*sqrt(a_E^2-1))],
   [0, 1,  a_E*( 2*a_E^2-1+p2)/(sqrt(a_E^2-1)*p2), -1/(2*sqrt(a_E^2-1))]],
  [[1, 0,  (1/2)*( 2*a_E^2+p2-2)*a_E/(a_E^2-1)^(3/2),  1/(2*sqrt(a_E^2-1))],
   [0, 1, -a_E*( 2*a_E^2-1+p2)/(sqrt(a_E^2-1)*p2),  1/(2*sqrt(a_E^2-1))]],
  [[1,  sqrt(3-2*a_E^2-2*sqrt(2)*sqrt(-a_E^2+1))/sqrt(2*a_E^2-1), 0, 0],
   [0, 0, 1,  (1/2)*sqrt(2)*sqrt(-a_E^2+1)/a_E]],
  [[1,  sqrt(3-2*a_E^2+2*sqrt(2)*sqrt(-a_E^2+1))/sqrt(2*a_E^2-1), 0, 0],
   [0, 0, 1, -(1/2)*sqrt(2)*sqrt(-a_E^2+1)/a_E]],
  [[1, -sqrt(3-2*a_E^2-2*sqrt(2)*sqrt(-a_E^2+1))/sqrt(2*a_E^2-1), 0, 0],
   [0, 0, 1,  (1/2)*sqrt(2)*sqrt(-a_E^2+1)/a_E]],
  [[1, -sqrt(3-2*a_E^2+2*sqrt(2)*sqrt(-a_E^2+1))/sqrt(2*a_E^2-1), 0, 0],
   [0, 0, 1, -(1/2)*sqrt(2)*sqrt(-a_E^2+1)/a_E]]
 ]:
end():

#@ c_cayley

c_cayley[1] := (t) -> [sqrt((1-2*a_E^2)/(1+2*a_E^2)/3)*sin(t),
                       -cos(t),
                       -4*a_E/sqrt(6)/sqrt(1+2*a_E^2)*sin(t),
                       2/sqrt(6)/sqrt(1+2*a_E^2)*sin(t)];

c_cayley[2] := unapply(simplify(act_E[L](c_cayley[1](t))),t);
c_cayley[3] := unapply(simplify(act_E[L](c_cayley[2](t))),t);
c_cayley[4] := unapply(simplify(act_E[L](c_cayley[3](t))),t);

######################################################################

# Assuming that ab=[a,b], where a and b are linearly independent in C^4,
# the function is_in_line(ab,c) will return true if c lies in the C-linear
# span of a and b.

#@ is_in_line 
is_in_line := proc(ab,c)
 local i,d,M,N,J;
 M := Matrix([op(ab),c]);
 for i from 1 to 4 do
  J := sort([op({1,2,3,4} minus {i})]);
  N := SubMatrix(M,1..3,J);
  d := simplify(Determinant(N));
  if d <> 0 then return false; fi;
 od:
 return true;
end:
