<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># g_stereo(u) = 0 iff  g(unstereo(u)) = 0
<a name="line_2"></a><span style="color:red">#@ g_stereo 
</span><a name="line_3"></a>g_stereo := (u::RR_3) -> g([2*u[1],2*u[2],2*u[3],u[1]^2+u[2]^2+u[3]^2-1]);
<a name="line_4"></a>
<a name="line_5"></a># Partial derivatives of g wrt x[1], x[2], x[3], x[4]
<a name="line_6"></a><span style="color:red">#@ dg 
</span><a name="line_7"></a>dg := (x::RR_4) -> [
<a name="line_8"></a>  2*x[1]*(x[3]/a_E-2*x[4]),
<a name="line_9"></a> -2*x[2]*(x[3]/a_E+2*x[4]),
<a name="line_10"></a> 2*(1/a_E^2-1)*x[3]*x[4]+1/a_E*(x[1]^2-x[2]^2),
<a name="line_11"></a> (1/a_E^2-1)*x[3]^2-2*(x[1]^2+x[2]^2)-6*x[4]^2
<a name="line_12"></a>];
<a name="line_13"></a>
<a name="line_14"></a><span style="color:red">#@ square_norm_of_dg 
</span><a name="line_15"></a>square_norm_of_dg := (x::RR_4) -> 
<a name="line_16"></a>  4*(1/a_E^3-5/a_E)*(x[1]^2-x[2]^2)*x[3]*x[4]+
<a name="line_17"></a>  (8-2/a_E^2)*x[1]^2*x[2]^2+
<a name="line_18"></a>  (4+1/a_E^2)*(x[1]^4+x[2]^4)+
<a name="line_19"></a>  (4*x[3]^2+40*x[4]^2)*(x[1]^2+x[2]^2)+
<a name="line_20"></a>  (16-20/a_E^2+4/a_E^4)*x[3]^2*x[4]^2+
<a name="line_21"></a>  (1-1/a_E^2)^2*x[3]^4+
<a name="line_22"></a>  36*x[4]^4:
<a name="line_23"></a>
<a name="line_24"></a><span style="color:red">#@ conformal_twist 
</span><a name="line_25"></a>conformal_twist := (x::RR_4,u::RR_4) -> cross_product4(x,dg(x),u) /~ sqrt(square_norm_of_dg(x));
<a name="line_26"></a>
<a name="line_27"></a># The two curves omega[1] and omega[2] meet only at [0,0,0,1]
<a name="line_28"></a># I think that their union is a deformation retract of one component
<a name="line_29"></a># of the complement of X in S^3
<a name="line_30"></a><span style="color:red">#@ omega
</span><a name="line_31"></a>omega[1] := (t::RR) -> [sin(t)/sqrt(1+a_E^2), 0, (cos(t)-1)/(a_E+1/a_E), (a_E+cos(t)/a_E)/(a_E+1/a_E)];
<a name="line_32"></a>omega[2] := (t::RR) -> [0, sin(t)/sqrt(1+a_E^2),-(cos(t)-1)/(a_E+1/a_E), (a_E+cos(t)/a_E)/(a_E+1/a_E)];
<a name="line_33"></a>
<a name="line_34"></a># move_towards_X() takes a point in R^4 and returns a point in S^3
<a name="line_35"></a># that is closer to the surface X.
<a name="line_36"></a><span style="color:red">#@ move_towards_X 
</span><a name="line_37"></a>move_towards_X := proc(x::RR0_4)
<a name="line_38"></a> local y,u,n,e;
<a name="line_39"></a> y := evalf(Vector([x[1],x[2],x[3],x[4]]));
<a name="line_40"></a> u := Vector(dg1(y));
<a name="line_41"></a> n := sqrt(add(u[i]^2,i=1..4));
<a name="line_42"></a> u := u/n;
<a name="line_43"></a> e := min(evalf(g1(y))/n,0.1);
<a name="line_44"></a> y := y - e*u;
<a name="line_45"></a> y := y/sqrt(add(y[i]^2,i=1..4));
<a name="line_46"></a> return(convert(y,list));
<a name="line_47"></a>end:
<a name="line_48"></a>
<a name="line_49"></a># move_to_X() takes a point in R^4 and returns a point in X obtained
<a name="line_50"></a># by applying move_towards_X() repeatedly.
<a name="line_51"></a>
<a name="line_52"></a><span style="color:red">#@ move_to_X_tolerance 
</span><a name="line_53"></a>move_to_X_tolerance := 10.^(-99):
<a name="line_54"></a>
<a name="line_55"></a><span style="color:red">#@ move_to_X 
</span><a name="line_56"></a>move_to_X := proc(x::RR0_4)
<a name="line_57"></a> local n,a,b,d;
<a name="line_58"></a> a := x;
<a name="line_59"></a> n := 100;
<a name="line_60"></a> d := evalf(abs(g1(a)) + abs(rho(a)-1));
<a name="line_61"></a> while (d > move_to_X_tolerance and n > 0) do
<a name="line_62"></a>  a := move_towards_X(a);
<a name="line_63"></a>  n := n-1;
<a name="line_64"></a>  d := evalf(abs(g1(a)));
<a name="line_65"></a> od:
<a name="line_66"></a> return(convert(a,list)); 
<a name="line_67"></a>end:
<a name="line_68"></a>
<a name="line_69"></a><span style="color:red">#@ random_X_point 
</span><a name="line_70"></a>random_X_point := proc()
<a name="line_71"></a> local r,rr,T,i,j,s,t,a;
<a name="line_72"></a> r := rand(0..15);
<a name="line_73"></a> rr := rand(0..1000);
<a name="line_74"></a> T := G16[r()+1];
<a name="line_75"></a> i := r();
<a name="line_76"></a> j := r();
<a name="line_77"></a> s := evalf(rr()/1000);
<a name="line_78"></a> t := evalf(rr()/1000);
<a name="line_79"></a> a := act_R4[T](w_lift1([s,t]));
<a name="line_80"></a> a := move_to_X(a);
<a name="line_81"></a> return(a);
<a name="line_82"></a>end:
<a name="line_83"></a>
<a name="line_84"></a># midpoint_X(a,b) returns a point on X roughly half way between a and b.
<a name="line_85"></a><span style="color:red">#@ midpoint_X 
</span><a name="line_86"></a>midpoint_X := (a::RR0_4,b::RR0_4) -> move_to_X([seq((a[i]+b[i])/2,i=1..4)]):
<a name="line_87"></a>
<a name="line_88"></a># unnormalised_tangent_complement_projector(x) is a symmetric matrix P
<a name="line_89"></a># whose image is the subspace of R^4 orthogonal to the tangent space 
<a name="line_90"></a># T_xX, such that P^2 is a positive multiple of P.
<a name="line_91"></a>
<a name="line_92"></a><span style="color:red">#@ unnormalised_tangent_complement_projector 
</span><a name="line_93"></a>unnormalised_tangent_complement_projector := (x::RR_4) -> 
<a name="line_94"></a> map(expand,Matrix(4,4,[seq(seq(x[i]*x[j]*square_norm_of_dg(x) + dg(x)[i]*dg(x)[j],i=1..4),j=1..4)])):
<a name="line_95"></a>
<a name="line_96"></a># unnormalised_tangent_projector(x) is a symmetric matrix Q
<a name="line_97"></a># whose image is the tangent space T_xX, such that Q^2 is a positive
<a name="line_98"></a># multiple of Q.
<a name="line_99"></a>
<a name="line_100"></a><span style="color:red">#@ unnormalised_tangent_projector 
</span><a name="line_101"></a>unnormalised_tangent_projector := (x::RR_4) -> 
<a name="line_102"></a> square_norm_of_dg(x) * IdentityMatrix(4) - unnormalised_tangent_complement_projector(x);
<a name="line_103"></a>
<a name="line_104"></a># tangent_projector(x) is the orthogonal projection of R^4 onto the
<a name="line_105"></a># tangent space T_xX.  In other words, it is the unique symmetric matrix 
<a name="line_106"></a># R satisfying R^2=R such that the image of R is T_xX.
<a name="line_107"></a>
<a name="line_108"></a><span style="color:red">#@ tangent_projector 
</span><a name="line_109"></a>tangent_projector := (x::RR_4) -> unnormalised_tangent_projector(x)/square_norm_of_dg(x);
<a name="line_110"></a>
<a name="line_111"></a># We now define various functions that produce tangent vectors.
<a name="line_112"></a># tangent_u and tangent_v are continuous everywhere but unnormalised
<a name="line_113"></a># Each of them vanishes at two points.  tangent_u1 and tangent_v1 are
<a name="line_114"></a># normalised versions, so each is undefined at two points. 
<a name="line_115"></a># tangent_frame_u returns an orthonormal basis for the tangent space,
<a name="line_116"></a># whose first entry is tangent_u1.  tangent_frame_v is similar.
<a name="line_117"></a># tangent_frame returns either tangent_frame_u or tangent_frame_v,
<a name="line_118"></a># depending on whether tangent_u or tangent_v has larger norm.
<a name="line_119"></a>
<a name="line_120"></a><span style="color:red">#@ tangent_uu 
</span><a name="line_121"></a>tangent_uu := (x::RR_4) -> [x[4],-x[3],x[2],-x[1]];
<a name="line_122"></a>
<a name="line_123"></a><span style="color:red">#@ tangent_u 
</span><a name="line_124"></a>tangent_u := proc(x::RR_4) 
<a name="line_125"></a> local uu,nn;
<a name="line_126"></a> uu := tangent_uu(x);
<a name="line_127"></a> nn := dg(x);
<a name="line_128"></a> return(uu -~ (dp4(uu,nn)/dp4(nn,nn)) *~ nn);
<a name="line_129"></a>end:
<a name="line_130"></a>
<a name="line_131"></a><span style="color:red">#@ unit_tangent_u 
</span><a name="line_132"></a>unit_tangent_u := proc(x::RR_4) 
<a name="line_133"></a> local u;
<a name="line_134"></a> u := tangent_u(x);
<a name="line_135"></a> return(u /~ sqrt(rho(u)));
<a name="line_136"></a>end:
<a name="line_137"></a>
<a name="line_138"></a><span style="color:red">#@ full_frame_u 
</span><a name="line_139"></a>full_frame_u := proc(x::RR_4)
<a name="line_140"></a> local n,r,u,v;
<a name="line_141"></a>
<a name="line_142"></a> n := evalf(dg(x));
<a name="line_143"></a> n := n /~ nm4(n);
<a name="line_144"></a> u := unit_tangent_u(x);
<a name="line_145"></a> v := cross_product4(x,n,u);
<a name="line_146"></a> return [x,n,u,v];
<a name="line_147"></a>end:
<a name="line_148"></a>
<a name="line_149"></a><span style="color:red">#@ tangent_frame_u 
</span><a name="line_150"></a>tangent_frame_u := proc(x::RR_4)
<a name="line_151"></a> local f;
<a name="line_152"></a> f := full_frame_u(x);
<a name="line_153"></a> return [f[3],f[4]];
<a name="line_154"></a>end:
<a name="line_155"></a>
<a name="line_156"></a><span style="color:red">#@ tangent_vv 
</span><a name="line_157"></a>tangent_vv := (x::RR_4) -> [-x[3],x[4],x[1],-x[2]];
<a name="line_158"></a>
<a name="line_159"></a><span style="color:red">#@ tangent_v 
</span><a name="line_160"></a>tangent_v := proc(x::RR_4) 
<a name="line_161"></a> local vv,nn;
<a name="line_162"></a> vv := tangent_vv(x);
<a name="line_163"></a> nn := dg(x);
<a name="line_164"></a> return(vv -~ (dp4(vv,nn)/dp4(nn,nn)) *~ nn);
<a name="line_165"></a>end:
<a name="line_166"></a>
<a name="line_167"></a><span style="color:red">#@ unit_tangent_v 
</span><a name="line_168"></a>unit_tangent_v := proc(x::RR_4) 
<a name="line_169"></a> local v;
<a name="line_170"></a> v := tangent_v(x);
<a name="line_171"></a> return(v /~ sqrt(rho(v)));
<a name="line_172"></a>end:
<a name="line_173"></a>
<a name="line_174"></a><span style="color:red">#@ full_frame_v 
</span><a name="line_175"></a>full_frame_v := proc(x::RR_4)
<a name="line_176"></a> local n,r,u,v;
<a name="line_177"></a>
<a name="line_178"></a> n := evalf(dg(x));
<a name="line_179"></a> n := n /~ nm4(n);
<a name="line_180"></a> u := unit_tangent_v(x);
<a name="line_181"></a> v := cross_product4(x,n,u);
<a name="line_182"></a> return [x,n,u,v];
<a name="line_183"></a>end:
<a name="line_184"></a>
<a name="line_185"></a><span style="color:red">#@ tangent_frame_v 
</span><a name="line_186"></a>tangent_frame_v := proc(x::RR_4)
<a name="line_187"></a> local f;
<a name="line_188"></a> f := full_frame_v(x);
<a name="line_189"></a> return [f[3],f[4]];
<a name="line_190"></a>end:
<a name="line_191"></a>
<a name="line_192"></a># tighten_once accepts a list of points on X, thought of as representing
<a name="line_193"></a># a closed loop, and returns a slightly different list, which should be 
<a name="line_194"></a># closer to a geodesic loop.
<a name="line_195"></a>
<a name="line_196"></a><span style="color:red">#@ tighten_once 
</span><a name="line_197"></a>tighten_once := proc(xs)
<a name="line_198"></a> local n,xxs,Pxxs;
<a name="line_199"></a> n := nops(xs);
<a name="line_200"></a> xxs := map(Vector,[xs[n],op(xs),xs[1]]);
<a name="line_201"></a> Pxxs := map(evalf,map(tangent_projector,xxs));
<a name="line_202"></a> map(move_to_X,[seq(convert(xxs[i] + Pxxs[i] . (xxs[i+1]/2 + xxs[i-1]/2 - xxs[i]),list), i = 2 .. n+1)]); 
<a name="line_203"></a>end:
<a name="line_204"></a>
<a name="line_205"></a># quadratic_chart accepts a point x in X and a tangent vector u at x
<a name="line_206"></a># It returns a point y of the form x + u + O(|u|^2) such that 
<a name="line_207"></a># \|y\|-1 and g(y) are O(|u|^3).
<a name="line_208"></a>
<a name="line_209"></a><span style="color:red">#@ quadratic_chart 
</span><a name="line_210"></a>quadratic_chart := proc(x::RR_4,u::RR_4)
<a name="line_211"></a> (1 - dp4(u,u)/2) *~ x +~ u -~ dp4(dg(u),x) *~ dg(x)/~ square_norm_of_dg(x);
<a name="line_212"></a>end:
<a name="line_213"></a>
<a name="line_214"></a># Below we define the curvature function.  In order to use some 
<a name="line_215"></a># preliminary definitions without polluting the global namespace,
<a name="line_216"></a># we define and immediately invoke an anonymous procedure.
<a name="line_217"></a>
<a name="line_218"></a><span style="color:red">#@ curvature
</span><a name="line_219"></a>proc()
<a name="line_220"></a> local x,xx,n,m,p;
<a name="line_221"></a> global curvature:
<a name="line_222"></a>
<a name="line_223"></a> xx := [seq(x[i],i=1..4)];
<a name="line_224"></a> n := unapply(dg(xx),x);
<a name="line_225"></a> m := unapply(Matrix([seq([seq(diff(g(xx),x[i],x[j]),j=1..4)],i=1..4)]),x);
<a name="line_226"></a> p := unapply(Matrix(expand([seq(cross_product4(xx,n(x),convert(Column(m(x),i),list)),i=1..4)])),x);
<a name="line_227"></a>
<a name="line_228"></a> curvature := unapply(1 - NF_x(expand(Trace(p(x) . p(x))))/NF_x(2*square_norm_of_dg(xx)^2),x);
<a name="line_229"></a>end():
<a name="line_230"></a>
<a name="line_231"></a><span style="color:red">#@ laplacian 
</span><a name="line_232"></a>laplacian := proc(p)
<a name="line_233"></a> local dp,ddp,n,m,rr,r1,r2,L0,L1,L2,L3,L4,L;
<a name="line_234"></a> dp  := [seq(diff(p,x[i]),i=1..4)];
<a name="line_235"></a> ddp := [seq([seq(diff(p,x[i],x[j]),j=1..4)],i=1..4)];
<a name="line_236"></a> n := [seq(diff(g(xx),x[i]),i=1..4)];
<a name="line_237"></a> m := [seq([seq(diff(g(xx),x[i],x[j]),j=1..4)],i=1..4)];
<a name="line_238"></a> rr := dp4(n,n);
<a name="line_239"></a> r1 := add(m[i,i],i=1..4);
<a name="line_240"></a> r2 := add(add(n[i]*m[i][j]*n[j],i=1..4),j=1..4);
<a name="line_241"></a>
<a name="line_242"></a> L0 := add(ddp[i,i],i=1..4);
<a name="line_243"></a> L1 := - add(add(x[i]*x[j]*ddp[i,j],j=1..4),i=1..4);
<a name="line_244"></a> L2 := - add(add(n[i]*n[j]*ddp[i,j],j=1..4),i=1..4)/rr;
<a name="line_245"></a> L3 := -2*add(x[i]*dp[i],i=1..4); 
<a name="line_246"></a> L4 := (-r1/rr + r2/rr^2) * add(n[i] * dp[i],i=1..4);
<a name="line_247"></a> L := L0+L1+L2+L3+L4;
<a name="line_248"></a> return(L);
<a name="line_249"></a>end:
<a name="line_250"></a>
<a name="line_251"></a><span style="color:red">#@ laplacian_z_C 
</span><a name="line_252"></a>laplacian_z_C := table();
<a name="line_253"></a>
<a name="line_254"></a>laplacian_z_C[0] := (a_E^4*z[1]^2*z[2]+a_E^4*z[1]^2-12*a_E^4*z[1]*z[2]-4*a_E^4*z[1]+4*a_E^4*z[2]-2*a_E^2*z[1]^2*z[2]+4*a_E^4-2*a_E^2*z[1]^2+4*a_E^2*z[1]*z[2]+z[1]^2*z[2]+z[1]^2)^2;
<a name="line_255"></a>
<a name="line_256"></a>laplacian_z_C[1] := -6*a_E^8*z[1]^5*z[2]^2-12*a_E^8*z[1]^5*z[2]+104*a_E^8*z[1]^4*z[2]^2-6*a_E^8*z[1]^5+142*a_E^8*z[1]^4*z[2]-224*a_E^8*z[1]^3*z[2]^2+24*a_E^6*z[1]^5*z[2]^2+50*a_E^8*z[1]^4-376*a_E^8*z[1]^3*z[2]+96*a_E^8*z[1]^2*z[2]^2+48*a_E^6*z[1]^5*z[2]-240*a_E^6*z[1]^4*z[2]^2-160*a_E^8*z[1]^3+320*a_E^8*z[1]^2*z[2]-32*a_E^8*z[1]*z[2]^2+24*a_E^6*z[1]^5-328*a_E^6*z[1]^4*z[2]+128*a_E^6*z[1]^3*z[2]^2-36*a_E^4*z[1]^5*z[2]^2+240*a_E^8*z[1]^2-96*a_E^8*z[1]*z[2]-104*a_E^6*z[1]^4+400*a_E^6*z[1]^3*z[2]-72*a_E^4*z[1]^5*z[2]+168*a_E^4*z[1]^4*z[2]^2-160*a_E^8*z[1]+32*a_E^8*z[2]+128*a_E^6*z[1]^3-128*a_E^6*z[1]^2*z[2]-36*a_E^4*z[1]^5+236*a_E^4*z[1]^4*z[2]-32*a_E^4*z[1]^3*z[2]^2+24*a_E^2*z[1]^5*z[2]^2+32*a_E^8-32*a_E^6*z[1]^2+60*a_E^4*z[1]^4-216*a_E^4*z[1]^3*z[2]+48*a_E^2*z[1]^5*z[2]-32*a_E^2*z[1]^4*z[2]^2-64*a_E^4*z[1]^3+32*a_E^4*z[1]^2*z[2]+24*a_E^2*z[1]^5-56*a_E^2*z[1]^4*z[2]-6*z[1]^5*z[2]^2+16*a_E^4*z[1]^2-8*a_E^2*z[1]^4+32*a_E^2*z[1]^3*z[2]-12*z[1]^5*z[2]-6*z[1]^5+6*z[1]^4*z[2]+2*z[1]^4;
<a name="line_257"></a>
<a name="line_258"></a>laplacian_z_C[2] := 8*a_E^8*z[1]^3*z[2]^3+12*a_E^8*z[1]^3*z[2]^2-112*a_E^8*z[1]^2*z[2]^3+12*a_E^8*z[1]^3*z[2]-264*a_E^8*z[1]^2*z[2]^2+1120*a_E^8*z[1]*z[2]^3-32*a_E^6*z[1]^3*z[2]^3-120*a_E^8*z[1]^2*z[2]+912*a_E^8*z[1]*z[2]^2-320*a_E^8*z[2]^3-56*a_E^6*z[1]^3*z[2]^2+416*a_E^6*z[1]^2*z[2]^3+336*a_E^8*z[1]*z[2]-608*a_E^8*z[2]^2-32*a_E^6*z[1]^3*z[2]+480*a_E^6*z[1]^2*z[2]^2-448*a_E^6*z[1]*z[2]^3+40*a_E^4*z[1]^3*z[2]^3-288*a_E^8*z[2]-8*a_E^6*z[1]^3+136*a_E^6*z[1]^2*z[2]-384*a_E^6*z[1]*z[2]^2+88*a_E^4*z[1]^3*z[2]^2-176*a_E^4*z[1]^2*z[2]^3+40*a_E^6*z[1]^2-64*a_E^6*z[1]*z[2]+64*a_E^6*z[2]^2+48*a_E^4*z[1]^3*z[2]-296*a_E^4*z[1]^2*z[2]^2-16*a_E^2*z[1]^3*z[2]^3-64*a_E^6*z[1]+96*a_E^6*z[2]+16*a_E^4*z[1]^3-168*a_E^4*z[1]^2*z[2]+80*a_E^4*z[1]*z[2]^2-56*a_E^2*z[1]^3*z[2]^2+32*a_E^6-16*a_E^4*z[1]^2+48*a_E^4*z[1]*z[2]-48*a_E^2*z[1]^3*z[2]+48*a_E^2*z[1]^2*z[2]^2-8*a_E^2*z[1]^3+24*a_E^2*z[1]^2*z[2]+12*z[1]^3*z[2]^2+8*a_E^2*z[1]^2+20*z[1]^3*z[2];
<a name="line_259"></a>
<a name="line_260"></a>laplacian_z_C[3] := -4*z[1]*(z[1]*z[2]+z[1]-1)*(a_E^4*z[1]^2-4*a_E^4*z[1]+4*a_E^4-2*a_E^2*z[1]^2+z[1]^2)*(a_E^4*z[1]^2*z[2]+a_E^4*z[1]^2-12*a_E^4*z[1]*z[2]-4*a_E^4*z[1]+4*a_E^4*z[2]-2*a_E^2*z[1]^2*z[2]+4*a_E^4-2*a_E^2*z[1]^2+4*a_E^2*z[1]*z[2]+z[1]^2*z[2]+z[1]^2);
<a name="line_261"></a>
<a name="line_262"></a>laplacian_z_C[4] := -16*z[1]*z[2]*(2*a_E^4*z[1]*z[2]-a_E^4*z[1]-4*a_E^4*z[2]+2*a_E^4-2*a_E^2*z[1]+z[1])*(a_E^4*z[1]^2*z[2]+a_E^4*z[1]^2-12*a_E^4*z[1]*z[2]-4*a_E^4*z[1]+4*a_E^4*z[2]-2*a_E^2*z[1]^2*z[2]+4*a_E^4-2*a_E^2*z[1]^2+4*a_E^2*z[1]*z[2]+z[1]^2*z[2]+z[1]^2);
<a name="line_263"></a>
<a name="line_264"></a>laplacian_z_C[5] := 16*z[2]*(a_E^4*z[1]^2*z[2]+a_E^4*z[1]^2-12*a_E^4*z[1]*z[2]-4*a_E^4*z[1]+4*a_E^4*z[2]-2*a_E^2*z[1]^2*z[2]+4*a_E^4-2*a_E^2*z[1]^2+4*a_E^2*z[1]*z[2]+z[1]^2*z[2]+z[1]^2)*(a_E^4*z[1]*z[2]-4*a_E^4*z[2]^2-4*a_E^4*z[2]-a_E^2*z[1]*z[2]^2-a_E^2*z[1]+a_E^2*z[2]+a_E^2+z[1]*z[2]);
<a name="line_265"></a>
<a name="line_266"></a><span style="color:red">#@ laplacian_z 
</span><a name="line_267"></a>laplacian_z := proc(p)
<a name="line_268"></a> (laplacian_z_C[1] * diff(p,z[1]) + 
<a name="line_269"></a>  laplacian_z_C[2] * diff(p,z[2]) + 
<a name="line_270"></a>  laplacian_z_C[3] * diff(p,z[1],z[1]) + 
<a name="line_271"></a>  laplacian_z_C[4] * diff(p,z[1],z[2]) + 
<a name="line_272"></a>  laplacian_z_C[5] * diff(p,z[2],z[2]))/laplacian_z_C[0]; 
<a name="line_273"></a>end:
<a name="line_274"></a>
<a name="line_275"></a>######################################################################
<a name="line_276"></a>
<a name="line_277"></a><span style="color:red">#@ gauss_map 
</span><a name="line_278"></a>gauss_map := (x::RR_4) -> [op(1..3,H_quotient(dg(x)/~nm4(dg(x)),x))];
<a name="line_279"></a>
<a name="line_280"></a><span style="color:red">#@ act_gauss
</span><a name="line_281"></a>act_gauss[1]    := (u) -> [ u[1],  u[2],  u[3]];
<a name="line_282"></a>act_gauss[LL]   := (u) -> [-u[1], -u[2],  u[3]];
<a name="line_283"></a>act_gauss[LM]   := (u) -> [ u[2],  u[1], -u[3]];
<a name="line_284"></a>act_gauss[LLLM] := (u) -> [-u[2], -u[1], -u[3]];
<a name="line_285"></a>act_gauss[LN]   := (u) -> [ u[2],  u[1],  u[3]];
<a name="line_286"></a>act_gauss[LLLN] := (u) -> [-u[2], -u[1],  u[3]];
<a name="line_287"></a>act_gauss[MN]   := (u) -> [ u[1],  u[2], -u[3]];
<a name="line_288"></a>act_gauss[LLMN] := (u) -> [-u[1], -u[2], -u[3]];
<a name="line_289"></a>
<a name="line_290"></a>
<a name="line_291"></a>
  </pre>
 </body>
</html>
    