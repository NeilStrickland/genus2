<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>set_a_E0(1/sqrt(2));
<a name="line_2"></a>
<a name="line_3"></a><span style="color:red">#@ ry
</span><a name="line_4"></a>ry[1] := sqrt(1-y[2]/sqrt(2));
<a name="line_5"></a>ry[2] := sqrt(1+y[2]/sqrt(2));
<a name="line_6"></a>
<a name="line_7"></a><span style="color:red">#@ rx
</span><a name="line_8"></a>rx[1] := subs(y[2] = yx0[2],ry[1]);
<a name="line_9"></a>rx[2] := subs(y[2] = yx0[2],ry[2]);
<a name="line_10"></a>
<a name="line_11"></a><span style="color:red">#@ y2r
</span><a name="line_12"></a>y2r[1] := sqrt(2)*(1-r[1]^2);
<a name="line_13"></a>y2r[2] := sqrt(2)*(r[2]^2-1);
<a name="line_14"></a>
<a name="line_15"></a><span style="color:red">#@ y_vars 
</span><a name="line_16"></a>y_vars := lexdeg([r[1], r[2]],
<a name="line_17"></a>                 [x[1], x[2], x[3], x[4]],
<a name="line_18"></a>		 [z[1], z[2]],
<a name="line_19"></a>		 [y[1], y[2]]);
<a name="line_20"></a>
<a name="line_21"></a><span style="color:red">#@ y_rels0 
</span><a name="line_22"></a>y_rels0 := Basis([r[1]^2-ry[1]^2,r[2]^2-ry[2]^2,op(y_rels0)],y_vars);
<a name="line_23"></a>
<a name="line_24"></a><span style="color:red">#@ z_vars 
</span><a name="line_25"></a>z_vars := lexdeg([r[1], r[2]],
<a name="line_26"></a>                 [x[1], x[2], x[3], x[4]],
<a name="line_27"></a>		 [y[1], y[2]],
<a name="line_28"></a>                 [z[1], z[2]]);
<a name="line_29"></a>
<a name="line_30"></a><span style="color:red">#@ z_rels0 
</span><a name="line_31"></a>z_rels0 := Basis([r[1]^2-ry[1]^2,r[2]^2-ry[2]^2,op(z_rels0)],z_vars);
<a name="line_32"></a>
<a name="line_33"></a><span style="color:red">#@ root_rule 
</span><a name="line_34"></a>root_rule := {
<a name="line_35"></a>   sqrt(1-y[2]/sqrt(2)) = r[1],
<a name="line_36"></a>   sqrt(1+y[2]/sqrt(2)) = r[2],
<a name="line_37"></a>   sqrt(4-2*sqrt(2)*y[2]) = 2 * r[1],
<a name="line_38"></a>   sqrt(4+2*sqrt(2)*y[2]) = 2 * r[2],
<a name="line_39"></a>   sqrt(sqrt(2)*(sqrt(2)-y[2])) = (sqrt(2) * r[1]),
<a name="line_40"></a>   sqrt(sqrt(2)*(sqrt(2)+y[2])) = (sqrt(2) * r[2]),
<a name="line_41"></a>   (sqrt(2)*(sqrt(2)-y[2]))^(3/2) = 2*sqrt(2)*(1-y[2]/sqrt(2))*r[1],
<a name="line_42"></a>   (sqrt(2)*(sqrt(2)+y[2]))^(3/2) = 2*sqrt(2)*(1+y[2]/sqrt(2))*r[2],
<a name="line_43"></a>   sqrt( 3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2+4) = 2*r[1],
<a name="line_44"></a>   sqrt(-3*sqrt(2)*x[3]*x[4]-2*x[1]^2+2*x[2]^2+4) = 2*r[2],
<a name="line_45"></a>   sqrt(1 - y[2]^2/2) = r[1] * r[2],
<a name="line_46"></a>   sqrt(4 - 2*y[2]^2) = 2 * r[1] * r[2],
<a name="line_47"></a>   sqrt(-18*x[3]^2*x[4]^2-12*sqrt(2)*x[1]^2*x[3]*x[4]+12*sqrt(2)*x[2]^2*x[3]*x[4]-4*x[1]^4+8*x[1]^2*x[2]^2-4*x[2]^4+16)=4*r[1]*r[2]
<a name="line_48"></a>  };
<a name="line_49"></a>
<a name="line_50"></a><span style="color:red">#@ FNF_y0 
</span><a name="line_51"></a>FNF_y0 := proc(u)
<a name="line_52"></a> local uu,nu,du,t;
<a name="line_53"></a>
<a name="line_54"></a> if type(u,list) or type(u,set) then
<a name="line_55"></a>  return map(FNF_y0,u);
<a name="line_56"></a> fi;
<a name="line_57"></a>
<a name="line_58"></a> uu := simplify(u);
<a name="line_59"></a> uu := factor(subs(root_rule,uu));
<a name="line_60"></a> nu := NF_y0(subs(root_rule,numer(u)));
<a name="line_61"></a> du := NF_y0(subs(root_rule,denom(u)));
<a name="line_62"></a> uu := factor(nu/du);
<a name="line_63"></a>
<a name="line_64"></a> for t in [x[1],x[2],r[1],r[2]] do
<a name="line_65"></a>  if has(du,t) then
<a name="line_66"></a>   nu := NF_y0(expand(nu * subs(t = -t,du)));
<a name="line_67"></a>   du := NF_y0(expand(du * subs(t = -t,du))); 
<a name="line_68"></a>   uu := factor(nu/du);
<a name="line_69"></a>   nu := numer(uu);
<a name="line_70"></a>   du := denom(uu);
<a name="line_71"></a>  fi;
<a name="line_72"></a> od:
<a name="line_73"></a>
<a name="line_74"></a> return uu;
<a name="line_75"></a>end:
<a name="line_76"></a>
<a name="line_77"></a><span style="color:red">#@ FNF_z0 
</span><a name="line_78"></a>FNF_z0 := proc(u)
<a name="line_79"></a> local uu,nu,du,t;
<a name="line_80"></a>
<a name="line_81"></a> if type(u,list) or type(u,set) then
<a name="line_82"></a>  return map(FNF_y0,u);
<a name="line_83"></a> fi;
<a name="line_84"></a>
<a name="line_85"></a> uu := simplify(u);
<a name="line_86"></a> uu := factor(subs(root_rule,uu));
<a name="line_87"></a> nu := NF_z0(subs(root_rule,numer(u)));
<a name="line_88"></a> du := NF_z0(subs(root_rule,denom(u)));
<a name="line_89"></a> uu := factor(nu/du);
<a name="line_90"></a>
<a name="line_91"></a> for t in [x[1],x[2],y[1],y[2],r[1],r[2]] do
<a name="line_92"></a>  if has(du,t) then
<a name="line_93"></a>   nu := NF_z0(expand(nu * subs(t = -t,du)));
<a name="line_94"></a>   du := NF_z0(expand(du * subs(t = -t,du))); 
<a name="line_95"></a>   uu := factor(nu/du);
<a name="line_96"></a>   nu := numer(uu);
<a name="line_97"></a>   du := denom(uu);
<a name="line_98"></a>  fi;
<a name="line_99"></a> od:
<a name="line_100"></a>
<a name="line_101"></a> return uu;
<a name="line_102"></a>end:
<a name="line_103"></a>
<a name="line_104"></a>z_to_t := (z) -> [2*z[1]-z[1]^2+z[1]^2*z[2]/2,2*z[2]]; <span style="color:red">#@ z_to_t
</span><a name="line_105"></a>t_proj := unapply(z_to_t(z_proj0(x)),x);               <span style="color:red">#@ t_proj
</span><a name="line_106"></a>
<a name="line_107"></a><span style="color:red">#@ t_to_z 
</span><a name="line_108"></a>t_to_z := (t) -> [(2*(-2+sqrt(t[1]*t[2]-4*t[1]+4)))/(t[2]-4), (1/2)*t[2]];
<a name="line_109"></a>
<a name="line_110"></a><span style="color:red">#@ t_lift 
</span><a name="line_111"></a>t_lift := (t) -> [
<a name="line_112"></a> (1/2)*sqrt(2)*sqrt((1-sqrt(t[2]))*(sqrt(t[1]*t[2]-4*t[1]+4)+sqrt(t[2]))/(2+sqrt(t[2]))),
<a name="line_113"></a> (1/2)*sqrt(2)*sqrt((1+sqrt(t[2]))*(sqrt(t[1]*t[2]-4*t[1]+4)-sqrt(t[2]))/(2-sqrt(t[2]))),
<a name="line_114"></a> sqrt(2)*sqrt((2-sqrt(t[1]*t[2]-4*t[1]+4))/(4-t[2])),
<a name="line_115"></a> -sqrt((2-sqrt(t[1]*t[2]-4*t[1]+4))/(4-t[2]))*sqrt(t[2])
<a name="line_116"></a>]:
<a name="line_117"></a>
<a name="line_118"></a># The following quadratic vanishes on C[5] and C[7]:
<a name="line_119"></a>
<a name="line_120"></a>oval_g := (x) -> x[3]*x[4]/sqrt(2)+x[1]^2+x[4]^2; <span style="color:red">#@ oval_g
</span><a name="line_121"></a>oval_h := (z) -> z[1]^2*(z[2]-2) + 4*z[1] -2;     <span style="color:red">#@ oval_h
</span><a name="line_122"></a>
<a name="line_123"></a># The vectors oval_e[i] form an orthonormal basis, and oval_g(x) is the 
<a name="line_124"></a># sum of oval_m[i] <oval_e[i] , x>^2.
<a name="line_125"></a>
<a name="line_126"></a><span style="color:red">#@ oval_e
</span><a name="line_127"></a>oval_e[1] := [1,0,0,0];
<a name="line_128"></a>oval_e[2] := [0,1,0,0];
<a name="line_129"></a>oval_e[3] := [0,0,sqrt(1/2+1/sqrt(6)),-sqrt(1/2-1/sqrt(6))];
<a name="line_130"></a>oval_e[4] := [0,0,sqrt(1/2-1/sqrt(6)), sqrt(1/2+1/sqrt(6))];
<a name="line_131"></a>
<a name="line_132"></a><span style="color:red">#@ oval_m
</span><a name="line_133"></a>oval_m[1] := 1;
<a name="line_134"></a>oval_m[2] := 0;
<a name="line_135"></a>oval_m[3] := -(sqrt(6)-2)/4;
<a name="line_136"></a>oval_m[4] :=  (sqrt(6)+2)/4;
<a name="line_137"></a>
<a name="line_138"></a># The function c_alt[k] is an alternative parametrisation of C[k].
<a name="line_139"></a>
<a name="line_140"></a>alpha_E := 5+2*sqrt(6);            <span style="color:red">#@ alpha_E
</span><a name="line_141"></a>beta_E := sqrt(3) + sqrt(2);       <span style="color:red">#@ beta_E
</span><a name="line_142"></a>
<a name="line_143"></a><span style="color:red">#@ c_alt
</span><a name="line_144"></a>c_alt[5] := (t) -> [
<a name="line_145"></a> sin(t)/beta_E,
<a name="line_146"></a> 0,
<a name="line_147"></a> (1+beta_E^2)*(sqrt(1-sin(t)^2/beta_E^4)+cos(t)/beta_E^2)/12,
<a name="line_148"></a> (cos(t)-sqrt(1-sin(t)^2/beta_E^4))*sqrt(3)/6
<a name="line_149"></a>];
<a name="line_150"></a>
<a name="line_151"></a>c_alt[ 6] := unapply(simplify(act_E[ L](c_alt[5](t))),t);
<a name="line_152"></a>c_alt[ 7] := unapply(simplify(act_E[ M](c_alt[5](t))),t);
<a name="line_153"></a>c_alt[ 8] := unapply(simplify(act_E[LM](c_alt[5](t))),t);
<a name="line_154"></a>
<a name="line_155"></a>c_alt_approx[5] := (t) -> [
<a name="line_156"></a> sin(t)/beta_E,
<a name="line_157"></a> 0,
<a name="line_158"></a> (1+beta_E^2)*(1+cos(t)/beta_E^2)/12,
<a name="line_159"></a> (cos(t)-1)*sqrt(3)/6
<a name="line_160"></a>];
<a name="line_161"></a>
<a name="line_162"></a>for k from 0 to 4 do
<a name="line_163"></a> c_alt[k] := eval(c_E0[k]):
<a name="line_164"></a>od:
<a name="line_165"></a>
<a name="line_166"></a># We can also describe C[k] (for k in {5,6,7,8}) as part of an elliptic curve.
<a name="line_167"></a>
<a name="line_168"></a><span style="color:red">#@ elliptic_a_poly 
</span><a name="line_169"></a>elliptic_a_poly := (t,u) -> u^2-(1-(1-t)/(2*alpha_E^2))*(1-t^2);
<a name="line_170"></a>
<a name="line_171"></a><span style="color:red">#@ c_elliptic_a[5] 
</span><a name="line_172"></a>c_elliptic_a[5] := unapply([(sqrt(2)+sqrt(3))*alpha_E*u,0,(alpha-(1-t)/2)^2,-sqrt(8)*alpha_E*(1-t)]/~(alpha_E^2-(1+t)^2/4),t,u);
<a name="line_173"></a>
<a name="line_174"></a><span style="color:red">#@ c_trig[5] 
</span><a name="line_175"></a>c_trig[5] := unapply(c_elliptic_a[5](cos(t),sin(t)*sqrt(1-(1+cos(t))/2/alpha_E^2)),t);
<a name="line_176"></a>
<a name="line_177"></a>c_elliptic_a[ 6] := unapply(simplify(act_E[ L](c_elliptic_a[5](t,u))),t,u);
<a name="line_178"></a>c_elliptic_a[ 7] := unapply(simplify(act_E[ M](c_elliptic_a[5](t,u))),t,u);
<a name="line_179"></a>c_elliptic_a[ 8] := unapply(simplify(act_E[LM](c_elliptic_a[5](t,u))),t,u);
<a name="line_180"></a>
<a name="line_181"></a>c_trig[ 6] := unapply(simplify(act_E[ L](c_trig[5](t))),t);
<a name="line_182"></a>c_trig[ 7] := unapply(simplify(act_E[ M](c_trig[5](t))),t);
<a name="line_183"></a>c_trig[ 8] := unapply(simplify(act_E[LM](c_trig[5](t))),t);
<a name="line_184"></a>
<a name="line_185"></a><span style="color:red">#@ elliptic_b_poly 
</span><a name="line_186"></a>elliptic_b_poly := tu -> tu[2]^2 - tu[1]^3 + 10*tu[1]^2 - tu[1];
<a name="line_187"></a>
<a name="line_188"></a><span style="color:red">#@ elliptic_b_proj 
</span><a name="line_189"></a>elliptic_b_proj := (x) -> [x[1]^2,x[1]*(x[4]^2-(5/2)*sqrt(2)*x[3]*x[4]-1)];
<a name="line_190"></a>
<a name="line_191"></a># The next function finds a Fourier series approximation to c[k]
<a name="line_192"></a># It seems that the Fourier coefficients are 1/Pi times rational
<a name="line_193"></a># combinations of EllipticK and EllipticE of 1/sqrt(3).  We have 
<a name="line_194"></a># not worked out the details of this.  
<a name="line_195"></a>
<a name="line_196"></a><span style="color:red">#@ find_c_fourier 
</span><a name="line_197"></a>find_c_fourier := proc()
<a name="line_198"></a> global c_fourier;
<a name="line_199"></a> local a,k,p,c;
<a name="line_200"></a>
<a name="line_201"></a> a[0] := int(1/sqrt(10 - 2*cos(t)),t=0..Pi)/Pi;
<a name="line_202"></a>
<a name="line_203"></a> for k from 1 to 10 do
<a name="line_204"></a>  a[k] := int(cos(k*t)/sqrt(10 - 2*cos(t)),t=0..Pi)/Pi;
<a name="line_205"></a> od:
<a name="line_206"></a> 
<a name="line_207"></a> p := add(a[k] * cos(k*t),k = 0..10);
<a name="line_208"></a> 
<a name="line_209"></a> c_fourier[ 5] := unapply(combine([p * sin(t)/2,0,sqrt(2)*p,-sin(t/2)^2*p]),t);
<a name="line_210"></a>end:
<a name="line_211"></a>
<a name="line_212"></a><span style="color:red">#@ set_c_fourier 
</span><a name="line_213"></a>set_c_fourier := proc()
<a name="line_214"></a> global c_fourier;
<a name="line_215"></a> local B,C,p;
<a name="line_216"></a>
<a name="line_217"></a> B := [20/3, 44, 348, 20660/7, 1635748/63, 161901340/693, 19230235292/9009, 
<a name="line_218"></a>       177658107628/9009, 28136843691100/153153, 263857298489132/153153];
<a name="line_219"></a> C := [-8, -160/3, -6328/15, -75136/21, -9914776/315, -196266848/693, 
<a name="line_220"></a>       -116560420904/45045, -215368179200/9009, -34109227409288/153153, 
<a name="line_221"></a>       -6077419529883040/2909907];
<a name="line_222"></a>
<a name="line_223"></a> p := (2/3)*sqrt(3)*EllipticK((1/3)*sqrt(3))/Pi + 
<a name="line_224"></a>      add((B[k]*EllipticK(1/sqrt(3)) + C[k]*EllipticE(1/sqrt(3)))*sqrt(3)/Pi*cos(k*t),k=1..10);
<a name="line_225"></a>
<a name="line_226"></a> c_fourier[ 5] := unapply(combine([p * sin(t)/2,0,sqrt(2)*p,-sin(t/2)^2*p]),t);
<a name="line_227"></a>end:
<a name="line_228"></a>
<a name="line_229"></a><span style="color:red">#@ propagate_c_fourier 
</span><a name="line_230"></a>propagate_c_fourier := proc()
<a name="line_231"></a> global c_fourier,c_fourier0;
<a name="line_232"></a> local k;
<a name="line_233"></a> 
<a name="line_234"></a> c_fourier[ 6] := unapply(simplify(act_E[ L](c_fourier[5](t))),t);
<a name="line_235"></a> c_fourier[ 7] := unapply(simplify(act_E[ M](c_fourier[5](t))),t);
<a name="line_236"></a> c_fourier[ 8] := unapply(simplify(act_E[LM](c_fourier[5](t))),t);
<a name="line_237"></a>
<a name="line_238"></a> for k from 5 to 8 do 
<a name="line_239"></a>  c_fourier0[k] := unapply(evalf(c_fourier[k](t)),t);
<a name="line_240"></a> od:
<a name="line_241"></a>end:
<a name="line_242"></a>
<a name="line_243"></a>set_c_fourier();
<a name="line_244"></a>propagate_c_fourier();
<a name="line_245"></a>
<a name="line_246"></a># Stereographic projection away from various vertices
<a name="line_247"></a>
<a name="line_248"></a><span style="color:red">#@ v_stereo
</span><a name="line_249"></a>v_stereo[ 0] := (x) -> [x[1],x[2],x[4]] /~ (1 + x[3]);
<a name="line_250"></a>v_stereo[ 3] := (x) -> [x[1],x[3],x[4]] /~ (1 + x[2]);
<a name="line_251"></a>v_stereo[ 6] := (x) -> [(x[2]-x[1])/sqrt(2),x[3],x[4]] /~ (1+(x[1]+x[2])/sqrt(2));
<a name="line_252"></a>v_stereo[11] := (x) -> [x[1],x[2],x[3]*sqrt(1/3)+x[4]*sqrt(2/3)] /~ (1+x[3]*sqrt(2/3)-x[4]*sqrt(1/3));
<a name="line_253"></a>
<a name="line_254"></a># Stereographic projection centred at a point near the middle of F16
<a name="line_255"></a>
<a name="line_256"></a><span style="color:red">#@ F16_stereo
</span><a name="line_257"></a>proc()
<a name="line_258"></a> local a1,a2,a3,a4,x,xx;
<a name="line_259"></a> global F16_stereo;
<a name="line_260"></a>
<a name="line_261"></a> xx := [seq(x[i],i=1..4)];
<a name="line_262"></a> a1 := evalf([85/198, 623/990, (13/30)*sqrt(2), -104/495]):
<a name="line_263"></a> a2 := dg0(a1):
<a name="line_264"></a> a2 := simplify(a2 /~ sqrt(dp4(a2,a2))):
<a name="line_265"></a> a3 := simplify(subs(a_E=a_E0,tangent_u(a1))):
<a name="line_266"></a> a3 := simplify(expand(rationalize(a3 /~ sqrt(dp4(a3,a3))))):
<a name="line_267"></a> a4 := cross_product4(a1,a2,a3):
<a name="line_268"></a>
<a name="line_269"></a> F16_stereo := unapply([dp4(xx,a2),dp4(xx,a3),dp4(xx,a4)]/~(1+dp4(xx,a1)),x);
<a name="line_270"></a>end():
<a name="line_271"></a>
<a name="line_272"></a># Tangent fields that are nowhere zero on F16
<a name="line_273"></a>
<a name="line_274"></a># The following definition would work after loading square_diffeo.mpl:
<a name="line_275"></a># tangent_a := unapply(simplify(expand(cross_product4(xx,dg0(xx),[seq(diff(square_diffeo_E0(x)[2],x[i]),i=1..4)]))),x);
<a name="line_276"></a># However, we just record the result inline here, so that it works
<a name="line_277"></a># without loading square_diffeo.mpl.  Note that the enties in this
<a name="line_278"></a># field are homogeneous quartic polynomials.
<a name="line_279"></a>
<a name="line_280"></a><span style="color:red">#@ tangent_a 
</span><a name="line_281"></a>tangent_a := (x) -> [-x[3]^4+3*x[1]^2*sqrt(2)*x[3]*x[4]-3*x[2]^2*sqrt(2)*x[3]*x[4]-4*sqrt(2)*x[2]^3*x[4]-(3/2)*x[3]*x[1]^3*sqrt(2)+2*x[1]^3*sqrt(2)*x[4]-sqrt(2)*x[3]^3*x[4]-(3/2)*x[2]*x[4]^3*sqrt(3)+8*sqrt(2)*x[3]*x[4]^3+(3/2)*x[4]*sqrt(3)*x[2]^3+(3/4)*sqrt(2)*x[1]*x[3]^3-sqrt(2)*x[1]*x[2]^3+x[1]^3*x[2]*sqrt(2)-4*x[2]^2*x[1]^2+2*x[1]^2*x[3]^2+2*x[1]^2*x[4]^2-x[2]^2*x[3]^2-4*x[4]^2*x[2]^2+8*x[3]^2*x[4]^2+4*x[1]*x[2]^2*x[3]-4*x[1]^2*x[2]*x[3]+(3/2)*x[1]*x[2]^2*x[4]+16*x[1]*x[3]*x[4]^2-(3/4)*sqrt(2)*x[2]*x[1]^2*sqrt(3)*x[3]-(15/2)*x[2]*x[3]*x[4]^2*sqrt(2)*sqrt(3)+4*x[1]*x[2]*sqrt(2)*x[3]*x[4]+20*x[2]*x[3]*x[4]^2+3*x[1]*x[2]*x[3]^2+2*x[1]*x[2]*x[4]^2+6*x[1]*x[2]*x[3]*x[4]+4*x[1]^3*x[2]-(3/2)*x[1]^3*x[4]-2*x[1]*x[3]^3+4*x[3]*x[1]^3-4*x[2]^3*x[3]+2*x[2]*x[3]^3+2*x[1]*x[2]*x[3]^2*sqrt(2)-6*sqrt(2)*x[1]*x[3]*x[4]^2+12*x[2]*x[3]^2*x[4]*sqrt(2)+4*sqrt(2)*x[2]*x[1]^2*x[4]-2*sqrt(2)*x[1]*x[2]^2*x[4]-(39/4)*x[2]*x[3]^2*x[4]*sqrt(3)+(3/4)*sqrt(2)*x[2]^3*sqrt(3)*x[3]-(9/2)*x[2]*x[4]*sqrt(3)*x[1]^2-(3/2)*x[2]*x[3]^3*sqrt(2)*sqrt(3)-(3/2)*sqrt(2)*x[1]*x[3]*x[2]^2, -4*x[1]^4-x[3]^4-4*x[1]^2*x[3]*x[4]+2*x[2]^2*x[3]*x[4]-5*x[1]^2*sqrt(2)*x[3]*x[4]+x[2]^2*sqrt(2)*x[3]*x[4]+6*x[2]*x[3]*x[4]^2*sqrt(2)+(9/2)*x[4]*sqrt(3)*x[1]^3+(3/2)*x[1]*x[4]^3*sqrt(3)+(3/2)*sqrt(2)*x[2]^3*x[3]+2*x[3]^2*sqrt(2)*x[1]^2+x[1]^2*sqrt(2)*x[4]^2-x[2]^2*sqrt(2)*x[4]^2-(3/4)*x[2]*x[3]^3*sqrt(2)+2*sqrt(2)*x[2]^3*x[4]-3*x[3]*x[1]^3*sqrt(2)-4*x[1]^3*sqrt(2)*x[4]-sqrt(2)*x[3]^3*x[4]+8*sqrt(2)*x[3]*x[4]^3+(3/2)*sqrt(2)*x[1]*x[3]^3+7*x[1]^2*x[3]^2+2*x[2]^2*x[3]^2-2*x[4]^2*x[2]^2+8*x[3]^2*x[4]^2+4*x[1]*x[2]^2*x[3]-4*x[1]^2*x[2]*x[3]+3*x[1]*x[2]^2*x[4]-20*x[1]*x[3]*x[4]^2+(3/2)*x[1]^2*x[2]*x[4]+sqrt(2)*x[1]^2*x[2]^2-sqrt(2)*x[1]^4+8*x[1]*x[2]*sqrt(2)*x[3]*x[4]-(3/4)*sqrt(2)*x[1]*x[2]^2*sqrt(3)*x[3]+(21/2)*sqrt(2)*x[1]*x[3]*x[4]^2*sqrt(3)-16*x[2]*x[3]*x[4]^2-5*x[1]*x[2]*x[3]^2+2*x[1]*x[2]*x[4]^2+(3/2)*sqrt(2)*x[2]*x[3]*x[1]^2+12*x[1]*x[3]^2*x[4]*sqrt(2)-(9/4)*x[1]*x[3]^2*x[4]*sqrt(3)+(3/4)*x[1]^3*sqrt(2)*sqrt(3)*x[3]-(3/2)*x[1]*x[4]*sqrt(3)*x[2]^2-(3/2)*sqrt(2)*x[1]*x[3]^3*sqrt(3)+4*x[1]^3*x[2]-3*x[1]^3*x[4]-2*x[1]*x[3]^3+4*x[3]*x[1]^3-4*x[2]^3*x[3]+2*x[2]*x[3]^3-x[3]^3*x[4]-(3/2)*x[2]^3*x[4]+8*x[3]*x[4]^3-12*sqrt(2)*x[1]*x[3]*x[4]^2-2*sqrt(2)*x[2]*x[1]^2*x[4]+4*sqrt(2)*x[1]*x[2]^2*x[4]-3*sqrt(2)*x[1]*x[3]*x[2]^2, -4*x[1]^4+4*x[2]^4+3*x[1]^2*x[3]*x[4]+3*x[2]^2*x[3]*x[4]-2*x[1]*x[4]^3*sqrt(2)-2*x[2]*x[4]^3*sqrt(2)+(3/4)*x[3]^2*sqrt(2)*x[2]^2-4*x[1]^2*sqrt(2)*x[3]*x[4]-4*x[2]^2*sqrt(2)*x[3]*x[4]+2*x[2]*x[3]*x[4]^2*sqrt(2)-(3/4)*x[3]^2*sqrt(2)*x[1]^2+(3/2)*x[1]^2*sqrt(2)*x[4]^2-(3/2)*x[2]^2*sqrt(2)*x[4]^2-2*sqrt(2)*x[2]^3*x[4]-2*x[1]^3*sqrt(2)*x[4]+3*sqrt(2)*x[1]*x[2]^3+3*x[1]^3*x[2]*sqrt(2)+2*x[1]^2*x[3]^2-4*x[1]^2*x[4]^2-2*x[2]^2*x[3]^2+4*x[4]^2*x[2]^2+6*x[1]*x[2]^2*x[3]-10*x[1]^2*x[2]*x[3]-6*x[1]*x[3]*x[4]^2-2*x[1]^2*x[2]*x[4]+x[2]*x[3]^2*x[4]+3*x[1]*x[2]*x[3]^2*sqrt(2)*sqrt(3)+12*x[1]*x[2]*x[4]*sqrt(3)*x[3]+(3/2)*sqrt(2)*x[1]^4-(3/2)*sqrt(2)*x[2]^4+3*x[1]*x[2]*sqrt(2)*x[4]^2-16*x[1]*x[2]*sqrt(2)*x[3]*x[4]+2*x[2]*x[3]*x[4]^2-6*x[1]*x[2]*x[3]*x[4]-4*sqrt(2)*x[2]*x[3]*x[1]^2-x[1]*x[3]^2*x[4]*sqrt(2)+x[1]*x[3]^3-2*x[3]*x[1]^3-2*x[2]^3*x[3]+x[2]*x[3]^3-2*x[2]^3*x[4]-2*x[2]*x[4]^3-(3/2)*x[1]*x[2]*x[3]^2*sqrt(2)+3*x[2]*x[3]^2*x[4]*sqrt(2)-2*sqrt(2)*x[2]*x[1]^2*x[4]-2*sqrt(2)*x[1]*x[2]^2*x[4], (3/2)*x[1]^4+(3/2)*x[2]^4-12*x[1]^2*x[3]*x[4]+12*x[2]^2*x[3]*x[4]+4*x[3]^2*sqrt(2)*x[2]^2+(9/2)*x[1]^2*sqrt(2)*x[3]*x[4]-(9/2)*x[2]^2*sqrt(2)*x[3]*x[4]-6*x[2]*x[3]*x[4]^2*sqrt(2)+sqrt(2)*x[2]^3*x[3]+4*x[3]^2*sqrt(2)*x[1]^2-2*x[2]*x[3]^3*sqrt(2)+sqrt(2)*x[2]^3*x[4]-x[3]*x[1]^3*sqrt(2)+2*sqrt(2)*x[1]*x[3]^3-3*x[2]^2*x[1]^2-3*x[1]^2*x[3]^2-3*x[2]^2*x[3]^2+2*x[1]*x[2]^2*x[4]-2*x[1]^2*x[2]*x[4]-10*x[2]*x[3]^2*x[4]-2*x[1]*x[3]^2*x[4]+4*sqrt(2)*x[1]^2*x[2]^2-2*sqrt(2)*x[1]^4-2*sqrt(2)*x[2]^4+9*x[1]*x[2]*sqrt(2)*x[3]*x[4]-6*x[2]*x[3]*x[4]^2+6*x[1]*x[2]*x[3]^2-3*x[1]*x[2]*sqrt(2)*x[3]*x[4]*sqrt(3)+3*sqrt(2)*x[2]*x[3]*x[1]^2+3*x[1]^3*x[2]-2*x[1]^3*x[4]+2*x[2]^3*x[4]-3*x[1]*x[2]^3-8*x[1]*x[2]*x[3]^2*sqrt(2)-6*sqrt(2)*x[1]*x[3]*x[4]^2-2*x[2]*x[3]^2*x[4]*sqrt(2)-sqrt(2)*x[2]*x[1]^2*x[4]-3*sqrt(2)*x[1]*x[3]*x[2]^2]:
<a name="line_282"></a>
<a name="line_283"></a><span style="color:red">#@ unit_tangent_a 
</span><a name="line_284"></a>unit_tangent_a := proc(x)
<a name="line_285"></a> local u;
<a name="line_286"></a> u := tangent_a(x);
<a name="line_287"></a> u := evalf(u /~ nm4(u));
<a name="line_288"></a> return u;
<a name="line_289"></a>end:
<a name="line_290"></a>
<a name="line_291"></a><span style="color:red">#@ full_frame_a 
</span><a name="line_292"></a>full_frame_a := proc(x::[numeric,numeric,numeric,numeric])
<a name="line_293"></a> local n,r,u,v;
<a name="line_294"></a>
<a name="line_295"></a> n := evalf(dg0(x));
<a name="line_296"></a> n := n /~ nm4(n);
<a name="line_297"></a> u := unit_tangent_a(x);
<a name="line_298"></a> v := cross_product4(x,n,u);
<a name="line_299"></a> return [x,n,u,v];
<a name="line_300"></a>end:
<a name="line_301"></a>
<a name="line_302"></a><span style="color:red">#@ tangent_frame_a 
</span><a name="line_303"></a>tangent_frame_a := proc(x::[numeric,numeric,numeric,numeric])
<a name="line_304"></a> local f;
<a name="line_305"></a> f := full_frame_a(x);
<a name="line_306"></a> return [f[3],f[4]];
<a name="line_307"></a>end:
<a name="line_308"></a>
<a name="line_309"></a># This function projects a fixed vector u0 = [-1,18,-13,10] into
<a name="line_310"></a># the tangent plane to get a vector u.  With this choice of u0, 
<a name="line_311"></a># it works out that |u|/|u0| is always at least 0.7 or so, for
<a name="line_312"></a># all choices of x in F16.  With more obvious choices of u0 we
<a name="line_313"></a># would find that |u|/|u0| would be very small or zero for some
<a name="line_314"></a># choices of x.
<a name="line_315"></a>
<a name="line_316"></a><span style="color:red">#@ full_frame_b 
</span><a name="line_317"></a>full_frame_b := proc(x::[numeric,numeric,numeric,numeric])
<a name="line_318"></a> local n,r,u,v;
<a name="line_319"></a>
<a name="line_320"></a> n := evalf(dg0(x));
<a name="line_321"></a> n := n /~ nm4(n);
<a name="line_322"></a> u := [-1,18,-13,10];
<a name="line_323"></a> u := evalf(u -~ dp4(u,x) *~ x);
<a name="line_324"></a> u := evalf(u -~ dp4(u,n) *~ n);
<a name="line_325"></a> u := u /~ nm4(u);
<a name="line_326"></a> v := cross_product4(x,n,u);
<a name="line_327"></a> return [x,n,u,v];
<a name="line_328"></a>end:
<a name="line_329"></a>
<a name="line_330"></a><span style="color:red">#@ unit_tangent_b 
</span><a name="line_331"></a>unit_tangent_b := (x::[numeric,numeric,numeric,numeric]) -> full_frame_b(x)[3];
<a name="line_332"></a>
<a name="line_333"></a><span style="color:red">#@ tangent_frame_b 
</span><a name="line_334"></a>tangent_frame_b := proc(x::[numeric,numeric,numeric,numeric])
<a name="line_335"></a> local f;
<a name="line_336"></a> f := full_frame_b(x);
<a name="line_337"></a> return [f[3],f[4]];
<a name="line_338"></a>end:
<a name="line_339"></a>
<a name="line_340"></a>
<a name="line_341"></a>######################################################################
<a name="line_342"></a>
<a name="line_343"></a><span style="color:red">#@ cubic_chart0
</span><a name="line_344"></a>proc()
<a name="line_345"></a> local gg,aa,bb,nn,yy,t,tt,i,j,k;
<a name="line_346"></a> global cubic_chart0;
<a name="line_347"></a>
<a name="line_348"></a> tt := [seq(t[i],i=1..4)]:
<a name="line_349"></a> for i from 1 to 4 do
<a name="line_350"></a>  for j from 1 to 4 do 
<a name="line_351"></a>   for k from 1 to 4 do
<a name="line_352"></a>    gg[i,j,k] := diff(diff(diff(g0(xx),x[i]),x[j]),x[k])/6:
<a name="line_353"></a>   od:
<a name="line_354"></a>  od:
<a name="line_355"></a> od:
<a name="line_356"></a>
<a name="line_357"></a> aa := add(add(add(add(add(gg[i,j,k]*gg[k,l,m]*x[i]*t[j]*x[l]*x[m],m=1..4),l=1..4),k=1..4),j=1..4),i=1..4):
<a name="line_358"></a> bb := dp4(dg0(tt),xx):
<a name="line_359"></a> nn := square_norm_of_dg0(xx):
<a name="line_360"></a> yy := (1 - dp4(tt,tt)/2) *~ xx +~ tt +~ ((18 * aa - nn) * bb / nn^2 - g0(tt)/nn) *~ dg0(xx):
<a name="line_361"></a> cubic_chart0 := unapply(yy,x,t): 
<a name="line_362"></a>end():
<a name="line_363"></a>
<a name="line_364"></a>######################################################################
<a name="line_365"></a>
<a name="line_366"></a><span style="color:red">#@ annular_chart0
</span><a name="line_367"></a>
<a name="line_368"></a>for i from 0 to 2 do 
<a name="line_369"></a> annular_chart0[i] :=
<a name="line_370"></a>  unapply(subs(csgn(cos(s[1])^2-2) = -1,
<a name="line_371"></a>               simplify(subs(a_E=a_E0,annular_chart[i](s)))),s);
<a name="line_372"></a>od:
<a name="line_373"></a>
<a name="line_374"></a>annular_chart0[3] := unapply(subs({t=s[1],u=s[2]},
<a name="line_375"></a> [(44*cos(2*t)*u^3+cos(4*t)*u^3+115*u^3+60*cos(2*t)*u-3*cos(4*t)*u-153*u)/
<a name="line_376"></a>     (-3*cos(4*t)-153+60*cos(2*t)),
<a name="line_377"></a>  sin(t)-(1/2)*sin(t)*u^2,
<a name="line_378"></a>  (-u^2*sqrt(6)*cos(3*t)-7*u^2*sqrt(6)*cos(t)+2*sqrt(6)*cos(3*t)-18*sqrt(6)*cos(t))/
<a name="line_379"></a>     (12*cos(2*t)-60),
<a name="line_380"></a>  (-41*cos(t)*sqrt(3)*u^2+cos(3*t)*sqrt(3)*u^2+18*cos(t)*sqrt(3)-2*cos(3*t)*sqrt(3))/
<a name="line_381"></a>     (12*cos(2*t)-60)]),s);
<a name="line_382"></a>
<a name="line_383"></a>annular_chart0[4] := unapply(act_R4[L](annular_chart0[3](s)),s);
<a name="line_384"></a>
<a name="line_385"></a>annular_chart0[5] := unapply(subs({t=s[1],u=s[2]},[
<a name="line_386"></a>  (1/2)*sin(t)*sqrt(2)/sqrt(5-cos(t)) +
<a name="line_387"></a>   (1/16)*sqrt(2)*(cos(t)^3-21*cos(t)^2+79*cos(t)+53)*sin(t)*u^2/((5-cos(t))^(5/2)*(3-cos(t))),
<a name="line_388"></a>  (1/2)*sqrt(cos(t)^2-2*cos(t)+9)*u/(5-cos(t)),
<a name="line_389"></a>  2/sqrt(5-cos(t))-(1/4)*(cos(t)^3-5*cos(t)^2-17*cos(t)+37)*u^2/((5-cos(t))^(5/2)*(3-cos(t))),
<a name="line_390"></a>  -(1/2)*(1-cos(t))*sqrt(2)/sqrt(5-cos(t))+
<a name="line_391"></a>   (1/16)*sqrt(2)*(cos(t)^4-22*cos(t)^3+84*cos(t)^2+38*cos(t)+27)*u^2/((5-cos(t))^(5/2)*(3-cos(t)))
<a name="line_392"></a>  ]),s);
<a name="line_393"></a>
<a name="line_394"></a>annular_chart0[ 6] := unapply(simplify(act_E[ L](annular_chart0[5](s))),s);
<a name="line_395"></a>annular_chart0[ 7] := unapply(simplify(act_E[ M](annular_chart0[5](s))),s);
<a name="line_396"></a>annular_chart0[ 8] := unapply(simplify(act_E[LM](annular_chart0[5](s))),s);
<a name="line_397"></a>
<a name="line_398"></a>######################################################################
<a name="line_399"></a>
<a name="line_400"></a><span style="color:red">#@ is_geodesic 
</span><a name="line_401"></a>is_geodesic := proc(c,d)
<a name="line_402"></a> local t,c0,c1,c2,n0,err;
<a name="line_403"></a>
<a name="line_404"></a> c0 := multi_series(c(t),d+2,t);
<a name="line_405"></a> c1 := map(diff,c0,t);
<a name="line_406"></a> c2 := map(diff,c1,t);
<a name="line_407"></a> n0 := multi_series(dg0(c0),d+2,t);
<a name="line_408"></a> n0 := multi_series(n0 /~ nm4(n0),d+2,t);
<a name="line_409"></a> err := c0;
<a name="line_410"></a> err := multi_series(err - dp4(err,c0) *~ c0,d,t);
<a name="line_411"></a> err := multi_series(err - dp4(err,n0) *~ n0,d,t);
<a name="line_412"></a> return tidy(err);
<a name="line_413"></a>end:
<a name="line_414"></a>
<a name="line_415"></a><span style="color:red">#@ euclidean_spray 
</span><a name="line_416"></a>euclidean_spray := proc(x::RR1_4,d::posint := 4)
<a name="line_417"></a> local C,p,q,r,s,t,E;
<a name="line_418"></a>
<a name="line_419"></a> C := `new/E_chart`();
<a name="line_420"></a> C["centre_set_numeric",x0];
<a name="line_421"></a> C["set_degree_numeric",6];
<a name="line_422"></a> p := eval(C["p"]);
<a name="line_423"></a> E := metric_from_embedding(p(s),s[1],s[2])[1]:
<a name="line_424"></a> E := tidy(multi_series(E,d,s[1],s[2]));
<a name="line_425"></a> q := unapply(geodesic_spray_R2(E,s[1],s[2],d),s);
<a name="line_426"></a> r := multi_series(p(q(s)),d,s[1],s[2]);
<a name="line_427"></a> return unapply(r,s);
<a name="line_428"></a>end:
<a name="line_429"></a>
<a name="line_430"></a><span style="color:red">#@ tubular_F0 
</span><a name="line_431"></a>tubular_F0 := [u[1],u[2],u[3],u[4]] +~ u[5] *~ dg0([u[1],u[2],u[3],u[4]]);
<a name="line_432"></a>tubular_F0 := Vector([op(tubular_F0),g0([u[1],u[2],u[3],u[4]])]);
<a name="line_433"></a>
<a name="line_434"></a><span style="color:red">#@ tubular_F 
</span><a name="line_435"></a>tubular_F := unapply(tubular_F0,u);
<a name="line_436"></a>
<a name="line_437"></a><span style="color:red">#@ tubular_J0 
</span><a name="line_438"></a>tubular_J0 := map(expand,Matrix([seq(map(diff,tubular_F0,u[i]),i=1..5)]));
<a name="line_439"></a>
<a name="line_440"></a><span style="color:red">#@ tubular_J 
</span><a name="line_441"></a>tubular_J := unapply(tubular_J0,u);
<a name="line_442"></a>
<a name="line_443"></a><span style="color:red">#@ tubular_F_inv 
</span><a name="line_444"></a>tubular_F_inv := proc(x0::RR0_4)
<a name="line_445"></a> local i,u0,du,a0,da,x1,tol;
<a name="line_446"></a>
<a name="line_447"></a> tol := 10.^(-95);
<a name="line_448"></a> u0 := Vector(evalf([op(x0),0]));
<a name="line_449"></a> a0 := u0;
<a name="line_450"></a> du := evalf(u0 - tubular_F(a0));
<a name="line_451"></a> i := 0;
<a name="line_452"></a> while Norm(du,2) > tol do 
<a name="line_453"></a>  i := i+1;
<a name="line_454"></a>  da := LinearSolve(evalf(tubular_J(a0)),du);
<a name="line_455"></a>  a0 := a0 + da;
<a name="line_456"></a>  du := evalf(u0 - F(a0));
<a name="line_457"></a> od;
<a name="line_458"></a> return a0;
<a name="line_459"></a>end:
<a name="line_460"></a>
<a name="line_461"></a><span style="color:red">#@ tubular_retract 
</span><a name="line_462"></a>tubular_retract := proc(x0::RR0_4)
<a name="line_463"></a> local a0,x1;
<a name="line_464"></a> a0 := tubular_F_inv(x0);
<a name="line_465"></a> x1 := [a0[1],a0[2],a0[3],a0[4]];
<a name="line_466"></a> x1 := x1 /~ nm4(x1);
<a name="line_467"></a> return x1;
<a name="line_468"></a>end:
<a name="line_469"></a>
<a name="line_470"></a>######################################################################
<a name="line_471"></a>
<a name="line_472"></a><span style="color:red">#@ gauss_map0 
</span><a name="line_473"></a>gauss_map0 := unapply(
<a name="line_474"></a> [2*x[3]*sqrt(2)*x[4]*x[1]-2*sqrt(2)*x[2]^3+x[3]^2*sqrt(2)*x[2]-x[4]^2*sqrt(2)*x[2]-3*x[1]*x[3]^2+6*x[3]*x[4]*x[2]+sqrt(2)*x[2]+2*x[1],
<a name="line_475"></a>  2*sqrt(2)*x[1]*x[2]^2+3*sqrt(2)*x[1]*x[3]^2+sqrt(2)*x[1]*x[4]^2-2*sqrt(2)*x[2]*x[3]*x[4]-6*x[1]*x[3]*x[4]-3*x[2]*x[3]^2-sqrt(2)*x[1]+2*x[2],
<a name="line_476"></a> -4*x[3]*sqrt(2)*x[2]*x[1]-2*sqrt(2)*x[2]^2*x[4]-sqrt(2)*x[3]^2*x[4]-sqrt(2)*x[4]^3-3*x[3]^3+6*x[3]*x[4]^2+x[4]*sqrt(2)+2*x[3]] /~
<a name="line_477"></a> sqrt(6+6*sqrt(2)*x[3]^3*x[4]-8*x[2]^2+8*x[2]^4+4*x[4]^2-4*x[3]^2-x[3]^4+8*x[4]^2*x[2]^2-4*x[3]*sqrt(2)*x[4]-8*x[3]^2*x[4]^2+2*x[4]^4),x);
<a name="line_478"></a>
<a name="line_479"></a>######################################################################
<a name="line_480"></a>
<a name="line_481"></a><span style="color:red">#@ owl_proj 
</span><a name="line_482"></a>owl_proj := (x) -> [x[1],x[2],(x[3]-x[4])/sqrt(2)];
  </pre>
 </body>
</html>
    