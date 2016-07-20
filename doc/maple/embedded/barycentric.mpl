<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a><span style="color:red">#@ unnormalised_barycentric_coords 
</span><a name="line_2"></a>unnormalised_barycentric_coords := proc(A::[RR_4,RR_4,RR_4],x::RR_4)
<a name="line_3"></a> local n;
<a name="line_4"></a># n := evalf(subs(a_E=a_E1,dg(x)));
<a name="line_5"></a> n := dg0(x);
<a name="line_6"></a>
<a name="line_7"></a> return(([
<a name="line_8"></a>   Determinant(Matrix([x,n,A[2],A[3]])),
<a name="line_9"></a>   Determinant(Matrix([x,n,A[3],A[1]])),
<a name="line_10"></a>   Determinant(Matrix([x,n,A[1],A[2]]))
<a name="line_11"></a> ])):
<a name="line_12"></a>end:
<a name="line_13"></a>
<a name="line_14"></a><span style="color:red">#@ barycentric_coords 
</span><a name="line_15"></a>barycentric_coords := proc(A::[RR_4,RR_4,RR_4],x::RR_4)
<a name="line_16"></a> local b;
<a name="line_17"></a> b := unnormalised_barycentric_coords(A,x);
<a name="line_18"></a> return(b /~ (b[1]+b[2]+b[3]));
<a name="line_19"></a>end:
<a name="line_20"></a>
<a name="line_21"></a>barycentric_inverse_F := proc(A::[RR_4,RR_4,RR_4])
<a name="line_22"></a> local F0;
<a name="line_23"></a> F0 := evalf(unnormalised_barycentric_coords(A,xx)):
<a name="line_24"></a> F0 := [op(F0),g1(xx)]:
<a name="line_25"></a> return F0;
<a name="line_26"></a>end:
<a name="line_27"></a>
<a name="line_28"></a>barycentric_inverse := proc(A::[RR_4,RR_4,RR_4],t0::RR0_3,F0_)
<a name="line_29"></a> local st,et,t1,t2,t3,x0,x1,tt,m,F0;
<a name="line_30"></a>
<a name="line_31"></a> st := time();
<a name="line_32"></a> F0 := `if`(nargs > 2,F0_,barycentric_inverse_F(A));
<a name="line_33"></a> t1 := evalf(t0);
<a name="line_34"></a> x0 := convert(evalf(add(t1[i] * Vector(A[i]),i=1..3)),list);
<a name="line_35"></a> x0 := move_to_X(x0);
<a name="line_36"></a> t2 := unnormalised_barycentric_coords(A,x0);
<a name="line_37"></a> t3 := `+`(op(t2)) *~ t1;
<a name="line_38"></a> x1 := subs(fsolve(F0 -~ [op(t3),0],{seq(x[i] = x0[i],i=1..4)}),xx);
<a name="line_39"></a> x1 := x1 /~ nm4(x1);
<a name="line_40"></a> et := time();
<a name="line_41"></a> userinfo(8,genus2,sprintf("Time in barycentric_inverse(): %d ms",round(1000*(et - st)))); 
<a name="line_42"></a> return x1;
<a name="line_43"></a>end:
<a name="line_44"></a>
<a name="line_45"></a># The function below works only when the arguments are numerical.
<a name="line_46"></a># It calculates the area scaling function for the barycentric 
<a name="line_47"></a># coordinate map, where the measure on the simplex is scaled to
<a name="line_48"></a># have total area one.
<a name="line_49"></a>
<a name="line_50"></a><span style="color:red">#@ barycentric_jacobian 
</span><a name="line_51"></a>barycentric_jacobian := proc(A::[RR_4,RR_4,RR_4],x::RR0_4)
<a name="line_52"></a> local tu,tv,p,q,J,nn;
<a name="line_53"></a>
<a name="line_54"></a> nn := evalf(subs(a_E=a_E1,dg(x)));
<a name="line_55"></a> nn := nn /~ sqrt(dp4(nn,nn));
<a name="line_56"></a> tu := [x[4],-x[3],x[2],-x[1]];
<a name="line_57"></a> tu := tu -~ dp4(tu,nn) *~ nn;
<a name="line_58"></a> tu := tu /~ sqrt(dp4(tu,tu));
<a name="line_59"></a> tv := cross_product4(x,nn,tu);
<a name="line_60"></a>
<a name="line_61"></a> p := subs(e=0,map(diff,barycentric_coords(A,x +~ e *~ tu),e));
<a name="line_62"></a> q := subs(e=0,map(diff,barycentric_coords(A,x +~ e *~ tv),e));
<a name="line_63"></a>
<a name="line_64"></a> J := sqrt(dp3(p,p)*dp3(q,q) - dp3(p,q)^2) * evalf(2/sqrt(3));
<a name="line_65"></a> return(J);
<a name="line_66"></a>end:
<a name="line_67"></a>
<a name="line_68"></a><span style="color:red">#@ barycentric_face_plot 
</span><a name="line_69"></a>barycentric_face_plot := proc(p)
<a name="line_70"></a> local i0,i1,i2,a,N;
<a name="line_71"></a> a := table();
<a name="line_72"></a> N := 16;
<a name="line_73"></a> for i1 from 0 to N do
<a name="line_74"></a>  for i2 from 0 to N-i1 do
<a name="line_75"></a>   i0 := N-i1-i2;
<a name="line_76"></a>   a[i1,i2] := evalf(stereo(subs({t[0]=i0/N,t[1]=i1/N,t[2]=i2/N},p)));
<a name="line_77"></a>  od:
<a name="line_78"></a> od:
<a name="line_79"></a> return(display(
<a name="line_80"></a>  seq(seq(polygon([a[i1,i2],a[i1+1,i2],a[i1,i2+1]]),i2=0..N-i1-1),i1=0..N-1),
<a name="line_81"></a>  seq(seq(polygon([a[i1,i2],a[i1-1,i2],a[i1,i2-1]]),i2=1..N-i1),i1=1..N))):
<a name="line_82"></a>end:
<a name="line_83"></a>
<a name="line_84"></a><span style="color:red">#@ random_triangle 
</span><a name="line_85"></a>random_triangle := proc()
<a name="line_86"></a> local a1,a2,a3,u1,v1,r;
<a name="line_87"></a>
<a name="line_88"></a> a1 := random_X_point():
<a name="line_89"></a> u1,v1 := op(tangent_frame_b(a1));
<a name="line_90"></a> r := rand(-1000..1000);
<a name="line_91"></a> a2 := move_to_X(a1 +~ (r()/10000.) *~ u1 +~ (r()/10000.) *~ v1);
<a name="line_92"></a> a3 := move_to_X(a1 +~ (r()/10000.) *~ u1 +~ (r()/10000.) *~ v1);
<a name="line_93"></a> return [a1,a2,a3];
<a name="line_94"></a>end:
<a name="line_95"></a>
  </pre>
 </body>
</html>
    