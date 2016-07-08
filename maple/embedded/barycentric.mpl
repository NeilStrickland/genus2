#@ unnormalised_barycentric_coords 
unnormalised_barycentric_coords := proc(A::[RR_4,RR_4,RR_4],x::RR_4)
 local n;
# n := evalf(subs(a_E=a_E1,dg(x)));
 n := dg0(x);

 return(([
   Determinant(Matrix([x,n,A[2],A[3]])),
   Determinant(Matrix([x,n,A[3],A[1]])),
   Determinant(Matrix([x,n,A[1],A[2]]))
 ])):
end:

#@ barycentric_coords 
barycentric_coords := proc(A::[RR_4,RR_4,RR_4],x::RR_4)
 local b;
 b := unnormalised_barycentric_coords(A,x);
 return(b /~ (b[1]+b[2]+b[3]));
end:

barycentric_inverse_F := proc(A::[RR_4,RR_4,RR_4])
 local F0;
 F0 := evalf(unnormalised_barycentric_coords(A,xx)):
 F0 := [op(F0),g1(xx)]:
 return F0;
end:

barycentric_inverse := proc(A::[RR_4,RR_4,RR_4],t0::RR0_3,F0_)
 local st,et,t1,t2,t3,x0,x1,tt,m,F0;

 st := time();
 F0 := `if`(nargs > 2,F0_,barycentric_inverse_F(A));
 t1 := evalf(t0);
 x0 := convert(evalf(add(t1[i] * Vector(A[i]),i=1..3)),list);
 x0 := move_to_X(x0);
 t2 := unnormalised_barycentric_coords(A,x0);
 t3 := `+`(op(t2)) *~ t1;
 x1 := subs(fsolve(F0 -~ [op(t3),0],{seq(x[i] = x0[i],i=1..4)}),xx);
 x1 := x1 /~ nm4(x1);
 et := time();
 userinfo(8,genus2,sprintf("Time in barycentric_inverse(): %d ms",round(1000*(et - st)))); 
 return x1;
end:

# The function below works only when the arguments are numerical.
# It calculates the area scaling function for the barycentric 
# coordinate map, where the measure on the simplex is scaled to
# have total area one.

#@ barycentric_jacobian 
barycentric_jacobian := proc(A::[RR_4,RR_4,RR_4],x::RR0_4)
 local tu,tv,p,q,J,nn;

 nn := evalf(subs(a_E=a_E1,dg(x)));
 nn := nn /~ sqrt(dp4(nn,nn));
 tu := [x[4],-x[3],x[2],-x[1]];
 tu := tu -~ dp4(tu,nn) *~ nn;
 tu := tu /~ sqrt(dp4(tu,tu));
 tv := cross_product4(x,nn,tu);

 p := subs(e=0,map(diff,barycentric_coords(A,x +~ e *~ tu),e));
 q := subs(e=0,map(diff,barycentric_coords(A,x +~ e *~ tv),e));

 J := sqrt(dp3(p,p)*dp3(q,q) - dp3(p,q)^2) * evalf(2/sqrt(3));
 return(J);
end:

#@ barycentric_face_plot 
barycentric_face_plot := proc(p)
 local i0,i1,i2,a,N;
 a := table();
 N := 16;
 for i1 from 0 to N do
  for i2 from 0 to N-i1 do
   i0 := N-i1-i2;
   a[i1,i2] := evalf(stereo(subs({t[0]=i0/N,t[1]=i1/N,t[2]=i2/N},p)));
  od:
 od:
 return(display(
  seq(seq(polygon([a[i1,i2],a[i1+1,i2],a[i1,i2+1]]),i2=0..N-i1-1),i1=0..N-1),
  seq(seq(polygon([a[i1,i2],a[i1-1,i2],a[i1,i2-1]]),i2=1..N-i1),i1=1..N))):
end:

#@ random_triangle 
random_triangle := proc()
 local a1,a2,a3,u1,v1,r;

 a1 := random_X_point():
 u1,v1 := op(tangent_frame_b(a1));
 r := rand(-1000..1000);
 a2 := move_to_X(a1 +~ (r()/10000.) *~ u1 +~ (r()/10000.) *~ v1);
 a3 := move_to_X(a1 +~ (r()/10000.) *~ u1 +~ (r()/10000.) *~ v1);
 return [a1,a2,a3];
end:

