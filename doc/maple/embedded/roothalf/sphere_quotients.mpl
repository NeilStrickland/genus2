<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># There is a canonical conformal map p : EX^* -> S^2, but it is hard 
<a name="line_2"></a># to calculate.  It is also interesting to look for more elementary
<a name="line_3"></a># maps EX^* -> S^2 that are not conformal, but have properties similar
<a name="line_4"></a># to those of p.  This file contains some code for that problem; 
<a name="line_5"></a># there is further code in better_sphere_quotients.mpl and 
<a name="line_6"></a># (with an object oriented framework) in E_to_S.mpl
<a name="line_7"></a>
<a name="line_8"></a># Here is a fairly elementary map of the required type.
<a name="line_9"></a>
<a name="line_10"></a><span style="color:red">#@ E_to_S2_tilde 
</span><a name="line_11"></a>E_to_S2_tilde := unapply(
<a name="line_12"></a> [sqrt(2)*yx0[2]*(1-x[3]^2),2*x[1]*x[2],-2*x[3]] /~ (1+x[3]^2) ,x);
<a name="line_13"></a>
<a name="line_14"></a><span style="color:red">#@ E_to_S2_s_z 
</span><a name="line_15"></a>E_to_S2_s_z := unapply(z[1]^2*z[2]*(1/2 - z[2])/(1 + z[1])^2,z);
<a name="line_16"></a>
<a name="line_17"></a><span style="color:red">#@ E_to_S2_s_x 
</span><a name="line_18"></a>E_to_S2_s_x := (x) -> x[4]^2*(x[3]^2/2-x[4]^2)/(1+x[3]^2)^2;
<a name="line_19"></a>
<a name="line_20"></a><span style="color:red">#@ E_to_S2_s_max_z1 
</span><a name="line_21"></a>E_to_S2_s_max_z1 := RootOf(5*z[1]^3-10*z[1]^2+2*z[1]+2,z[1],index=1);
<a name="line_22"></a>
<a name="line_23"></a><span style="color:red">#@ E_to_S2_s_max_z2 
</span><a name="line_24"></a>E_to_S2_s_max_z2 := subs(t = E_to_S2_s_max_z1,15*t^2 - 35*t + 18);
<a name="line_25"></a>
<a name="line_26"></a><span style="color:red">#@ E_to_S2_s_max 
</span><a name="line_27"></a>E_to_S2_s_max := subs(t = E_to_S2_s_max_z1,(-1810*t^2+4230*t-2149)/15);
<a name="line_28"></a>
<a name="line_29"></a><span style="color:red">#@ E_to_S2 
</span><a name="line_30"></a>E_to_S2 := unapply(
<a name="line_31"></a> [sqrt(2)*yx0[2]*(1-x[3]^2),2*x[1]*x[2],-2*x[3]]/~
<a name="line_32"></a> sqrt(x[3]^4+2*x[3]^2+1-(1/2)*x[3]^2*x[4]^2+x[4]^4),x);
<a name="line_33"></a>
<a name="line_34"></a># The images of v[0] to v[9] should be as follows.
<a name="line_35"></a><span style="color:red">#@ v_S2
</span><a name="line_36"></a>v_S2[0] := [ 0, 0,-1];
<a name="line_37"></a>v_S2[1] := [ 0, 0, 1];
<a name="line_38"></a>v_S2[2] := [-1, 0, 0];
<a name="line_39"></a>v_S2[3] := [ 1, 0, 0];
<a name="line_40"></a>v_S2[4] := [-1, 0, 0];
<a name="line_41"></a>v_S2[5] := [ 1, 0, 0];
<a name="line_42"></a>v_S2[6] := [ 0, 1, 0];
<a name="line_43"></a>v_S2[7] := [ 0,-1, 0];
<a name="line_44"></a>v_S2[8] := [ 0, 1, 0];
<a name="line_45"></a>v_S2[9] := [ 0,-1, 0];
<a name="line_46"></a>
<a name="line_47"></a># The images of v[10] to v[13] depend on the parameter a_P, for which
<a name="line_48"></a># we do not have an exact expression.  The code below corresponds to 
<a name="line_49"></a># the case a_P = (sqrt(3)-sqrt(2))^( 2), which is approximately 
<a name="line_50"></a># correct and is exactly consistent with the map E_to_S2 above.
<a name="line_51"></a>
<a name="line_52"></a>v_S2[10] := [-1/5, 0, -(2/5)*sqrt(6)];
<a name="line_53"></a>v_S2[11] := [ 1/5, 0, -(2/5)*sqrt(6)];
<a name="line_54"></a>v_S2[12] := [-1/5, 0,  (2/5)*sqrt(6)];
<a name="line_55"></a>v_S2[13] := [ 1/5, 0,  (2/5)*sqrt(6)];
<a name="line_56"></a>
<a name="line_57"></a># The points v_C[i] are the images in C u {infinity} of the points v_S2[i].
<a name="line_58"></a>
<a name="line_59"></a><span style="color:red">#@ v_C
</span><a name="line_60"></a>v_C[ 0] := 0;
<a name="line_61"></a>v_C[ 1] := infinity;
<a name="line_62"></a>v_C[ 2] := -1;
<a name="line_63"></a>v_C[ 3] :=  1;
<a name="line_64"></a>v_C[ 4] := -1;
<a name="line_65"></a>v_C[ 5] :=  1;
<a name="line_66"></a>v_C[ 6] :=  I;
<a name="line_67"></a>v_C[ 7] := -I;
<a name="line_68"></a>v_C[ 8] :=  I;
<a name="line_69"></a>v_C[ 9] := -I;
<a name="line_70"></a>
<a name="line_71"></a>v_C[10] := -(sqrt(3)-sqrt(2))^( 2);
<a name="line_72"></a>v_C[11] :=  (sqrt(3)-sqrt(2))^( 2);
<a name="line_73"></a>v_C[12] := -(sqrt(3)-sqrt(2))^(-2);
<a name="line_74"></a>v_C[13] :=  (sqrt(3)-sqrt(2))^(-2);
<a name="line_75"></a>
<a name="line_76"></a># These curves are exactly correct for the map E_to_S2, but only
<a name="line_77"></a># approximately correct for the canonical conformal map p : EX^* -> S^2.  
<a name="line_78"></a><span style="color:red">#@ c_S2
</span><a name="line_79"></a>c_S2[ 0] := (t) -> [-cos(2*t),sin(2*t),0];
<a name="line_80"></a>c_S2[ 1] := (t) -> [0,(1 - cos(t)^2)/(1 + cos(t)^2),-2*cos(t)/(1 + cos(t)^2)];
<a name="line_81"></a>c_S2[ 2] := unapply(act_S2[L](c_S2[ 1](t)),t);
<a name="line_82"></a>c_S2[ 3] := (t) -> [(3 - 2*cos(t)^2)/(3 + 2*cos(t)^2),0,-2*sqrt(6)*cos(t)/(3 + 2*cos(t)^2)];
<a name="line_83"></a>c_S2[ 4] := unapply(act_S2[L](c_S2[ 3](t)),t);
<a name="line_84"></a>c_S2[ 5] := unapply([(1-cos(t))^2,0,-8*sqrt(5-cos(t))] /~ sqrt((18-2*cos(t))^2 - (1-cos(t))*(3-cos(t))*sin(t)^2),t);
<a name="line_85"></a>c_S2[ 6] := unapply(act_S2[ L](c_S2[ 5](t)),t);
<a name="line_86"></a>c_S2[ 7] := unapply(act_S2[ M](c_S2[ 5](t)),t);
<a name="line_87"></a>c_S2[ 8] := unapply(act_S2[LM](c_S2[ 5](t)),t);
<a name="line_88"></a>c_S2[ 9] := unapply([cos(t),sin(t)^2,-2*sqrt(6)] /~ sqrt(25 - sin(t)^2*cos(t)^2),t);
<a name="line_89"></a>c_S2[10] := unapply(act_S2[ L](c_S2[ 9](t)),t);
<a name="line_90"></a>c_S2[11] := unapply(act_S2[ M](c_S2[ 9](t)),t);
<a name="line_91"></a>c_S2[12] := unapply(act_S2[LM](c_S2[ 9](t)),t);
<a name="line_92"></a>c_S2[13] := unapply([2*sqrt(3+sin(t)^2)*(3-2*sin(t)^2)*sin(t),9*cos(t)^2,-2*sqrt(6)*(3+sin(t)^2)*sin(t)] /~
<a name="line_93"></a>                     sqrt((9+11*sin(t)^2)^2 - (2*sin(t)*cos(t)*(3+2*sin(t)^2))^2 + (sqrt(8)*sin(t)^2*cos(t))^2),t);
<a name="line_94"></a>c_S2[14] := unapply(act_S2[ L](c_S2[13](t)),t);
<a name="line_95"></a>c_S2[15] := unapply(act_S2[ N](c_S2[13](t)),t);
<a name="line_96"></a>c_S2[16] := unapply(act_S2[LN](c_S2[13](t)),t);
<a name="line_97"></a>
<a name="line_98"></a># We use the following approximately conformal charts on EX
<a name="line_99"></a>
<a name="line_100"></a><span style="color:red">#@ chart
</span><a name="line_101"></a>chart[0]  := (s,t) -> [s,t,1-s^2/2-t^2/2,sqrt(2)*(t^2-s^2)];
<a name="line_102"></a>chart[3]  := (s,t) -> [-s,1,-sqrt(2/3)*t,sqrt(1/3)*t];
<a name="line_103"></a>chart[6]  := (s,t) -> [1/sqrt(2)+(t+s)/2,1/sqrt(2)-(t+s)/2,(t-s)/sqrt(2),0];
<a name="line_104"></a>chart[11] := (s,t) -> [s,-t,(1+s^2/2-t^2/2)*sqrt(2/3),(-1+5*s^2/2+t^2/2)*sqrt(1/3)];
<a name="line_105"></a>
<a name="line_106"></a>######################################################################
<a name="line_107"></a>
<a name="line_108"></a># If we have a homeomorphism f from EX^*/<LL> to C u {infinity} or
<a name="line_109"></a># to S^2, then we can obtain homeomorphisms from EX^*/H to
<a name="line_110"></a># C u {infinity} or S^2 (for various other subgroups H) by composing
<a name="line_111"></a># with the maps below.  Specifically:
<a name="line_112"></a>#
<a name="line_113"></a># - For H = <L>,     compose with p12
<a name="line_114"></a># - For H = <LL,M>,  compose with p13
<a name="line_115"></a># - For H = <LL,LM>, compose with p14
<a name="line_116"></a># - For H = <L,M>,   compose with p15 = p25 o p12 = p35 o p13 = p45 o p14.
<a name="line_117"></a>#
<a name="line_118"></a># Conventions are chosen such that these maps send small positive reals
<a name="line_119"></a># to small positive reals.
<a name="line_120"></a>
<a name="line_121"></a>p12_C := (z) -> z^2;           <span style="color:red">#@ p12_C 
</span><a name="line_122"></a>p13_C := (z) -> 2*z/(1+z^2);   <span style="color:red">#@ p13_C 
</span><a name="line_123"></a>p14_C := (z) -> 2*z/(1-z^2);   <span style="color:red">#@ p14_C 
</span><a name="line_124"></a>p15_C := (z) -> 2*z^2/(1+z^4); <span style="color:red">#@ p15_C 
</span><a name="line_125"></a>p25_C := (z) -> 2*z/(1+z^2);   <span style="color:red">#@ p25_C 
</span><a name="line_126"></a>p35_C := (z) -> z^2/(2-z^2);   <span style="color:red">#@ p35_C 
</span><a name="line_127"></a>p45_C := (z) -> z^2/(2+z^2);   <span style="color:red">#@ p45_C 
</span><a name="line_128"></a>
<a name="line_129"></a><span style="color:red">#@ p12_S2 
</span><a name="line_130"></a>p12_S2 := unapply([u[1]^2-u[2]^2, 2*u[1]*u[2],2*u[3]] /~ (1+u[3]^2),u);
<a name="line_131"></a>
<a name="line_132"></a><span style="color:red">#@ p13_S2 
</span><a name="line_133"></a>p13_S2 := unapply([2*u[1],-2*u[2]*u[3],u[2]^2-u[3]^2] /~ (1+u[1]^2),u);
<a name="line_134"></a>
<a name="line_135"></a><span style="color:red">#@ p14_S2 
</span><a name="line_136"></a>p14_S2 := unapply([-2*u[1]*u[3],2*u[2],u[1]^2-u[3]^2] /~ (1+u[2]^2),u);
<a name="line_137"></a>
<a name="line_138"></a><span style="color:red">#@ p25_S2 
</span><a name="line_139"></a>p25_S2 := unapply([2*u[1],-2*u[2]*u[3],u[2]^2-u[3]^2] /~ (1+u[1]^2),u);
<a name="line_140"></a>
<a name="line_141"></a><span style="color:red">#@ p35_S2 
</span><a name="line_142"></a>p35_S2 := unapply([ (1-u[3])^2 - 4*(1-u[1]^2),4*u[1]*u[2],4*(u[1]^2+u[3]-1)]/~((1-u[3])^2+4*(1-u[1]^2)),u);
<a name="line_143"></a>
<a name="line_144"></a><span style="color:red">#@ p45_S2 
</span><a name="line_145"></a>p45_S2 := unapply([-(1-u[3])^2 + 4*(1-u[2]^2),4*u[1]*u[2],4*(u[2]^2+u[3]-1)]/~((1-u[3])^2+4*(1-u[2]^2)),u);
<a name="line_146"></a>
<a name="line_147"></a><span style="color:red">#@ p15_S2 
</span><a name="line_148"></a>p15_S2 := unapply([(1+u[3]^2)*(u[1]^2-u[2]^2),-4*u[1]*u[2]*u[3],2*(1+u[1]^2)*(1+u[2]^2)-4] /~ ((1-u[1]^2)^2+(1-u[2]^2)^2),u);
<a name="line_149"></a>
<a name="line_150"></a>######################################################################
<a name="line_151"></a>
<a name="line_152"></a><span style="color:red">#@ E_to_S2_jacobian 
</span><a name="line_153"></a>E_to_S2_jacobian :=
<a name="line_154"></a> 8*((1+z[1])*((1-z[1])^2-z[1]^2*z[2]/2) + z[1]^2*z[2]*(1/2-z[2])*(3+z[1]))/
<a name="line_155"></a>  (1+2*z[1]+z[1]^2*(1-z[2]/2)+z[1]^2*z[2]^2)^(3/2);
<a name="line_156"></a>
<a name="line_157"></a><span style="color:red">#@ E_to_S2_distortion 
</span><a name="line_158"></a>E_to_S2_distortion := proc(x0)
<a name="line_159"></a> local u0,v0,a0,b0,c0,p0,q0,M0,Cp,Cm,Cq;
<a name="line_160"></a>
<a name="line_161"></a> u0,v0 := op(tangent_frame_b(x0)):
<a name="line_162"></a> a0 := evalf(E_to_S2(x0)):
<a name="line_163"></a> b0,c0 := op(S2_tangent_frame(a0)): 
<a name="line_164"></a> p0 := subs(t=0,map(diff,evalf(E_to_S2(x0 +~ t *~ u0)),t)):
<a name="line_165"></a> q0 := subs(t=0,map(diff,evalf(E_to_S2(x0 +~ t *~ v0)),t)):
<a name="line_166"></a> M0 := <<dp3(p0,b0)|dp3(p0,c0)>,<dp3(q0,b0)|dp3(q0,c0)>>:
<a name="line_167"></a> Cp := (M0[1,1]-M0[2,2])^2 + (M0[1,2]+M0[2,1])^2:
<a name="line_168"></a> Cm := (M0[1,1]+M0[2,2])^2 + (M0[1,2]-M0[2,1])^2:
<a name="line_169"></a> Cq := Cp/(Cp+Cm);
<a name="line_170"></a>
<a name="line_171"></a> return Cq;
<a name="line_172"></a>end:
  </pre>
 </body>
</html>
    