<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>set_a_E0 := proc(a::RR0) 
<a name="line_2"></a> global a_E0,a_E1,g0,g_00,g_10,g1,g_01,g_11,dg0,dg1,
<a name="line_3"></a>   square_norm_of_dg0,square_norm_of_dg1,
<a name="line_4"></a>   curvature0,curvature1,
<a name="line_5"></a>   square_norm_of_dg_z0,square_norm_of_dg_z1,
<a name="line_6"></a>   curvature_z0,curvature_z1,g_stereo0,g_stereo1,
<a name="line_7"></a>   laplacian_z_C0,laplacian_z0,
<a name="line_8"></a>   omega0,omega1,
<a name="line_9"></a>   v_E0,v_E1,c_E0,c_E1,c_param_E0,c_param_E1,
<a name="line_10"></a>   yx0,yx1,xy0,xy1,uy0,uy1,uf0,uf1,uz0,uz1,zx0,zx1,
<a name="line_11"></a>   dyx0,dyx1,dxy0,dxy1,dzx0,dzx1,
<a name="line_12"></a>   y_proj0,y_proj1,z_proj0,z_proj1,w_proj0,w_proj1,
<a name="line_13"></a>   y_lift0,y_lift1,z_lift0,z_lift1,
<a name="line_14"></a>   z_to_w0,z_to_w1,y_to_w0,y_to_w1,
<a name="line_15"></a>   z_rels0,y_rels0,x_rels0,NF_z0,NF_y0,NF_x0,
<a name="line_16"></a>   is_in_F4_E0_measure,is_in_F16_E0_measure,
<a name="line_17"></a>   num_vertices_E,num_curves_E;
<a name="line_18"></a> local i,t;
<a name="line_19"></a>
<a name="line_20"></a> a_E0 := a; <span style="color:red">#@  a_E0 
</span><a name="line_21"></a> a_E1 := evalf(a_E0); <span style="color:red">#@  a_E1 
</span><a name="line_22"></a>
<a name="line_23"></a> g0    := unapply(subs(a_E=a_E0,g(xx)),x): <span style="color:red">#@  g0    
</span><a name="line_24"></a> g_00  := unapply(subs(a_E=a_E0,g_0(xx)),x): <span style="color:red">#@  g_00  
</span><a name="line_25"></a> g_10  := unapply(subs(a_E=a_E0,g_1(xx)),x): <span style="color:red">#@  g_10  
</span><a name="line_26"></a> dg0   := unapply(subs(a_E=a_E0,dg(xx)),x): <span style="color:red">#@  dg0   
</span><a name="line_27"></a> square_norm_of_dg0 := unapply(simplify(subs(a_E=a_E0,square_norm_of_dg(xx))),x): <span style="color:red">#@  square_norm_of_dg0 
</span><a name="line_28"></a> curvature0 := unapply(simplify(subs(a_E=a_E0,curvature(xx))),x): <span style="color:red">#@  curvature0 
</span><a name="line_29"></a> square_norm_of_dg_z0 := unapply(subs(a_E=a_E0,square_norm_of_dg_z(z)),z): <span style="color:red">#@  square_norm_of_dg_z0 
</span><a name="line_30"></a> curvature_z0 := unapply(1 - factor(1-subs(a_E=a_E0,curvature_z(z))),z): <span style="color:red">#@  curvature_z0 
</span><a name="line_31"></a> g_stereo0 := unapply(subs(a_E=a_E0,g_stereo([u[1],u[2],u[3]])),u): <span style="color:red">#@  g_stereo0 
</span><a name="line_32"></a>
<a name="line_33"></a> g1    := unapply(subs(a_E=a_E1,g(xx)),x): <span style="color:red">#@  g1    
</span><a name="line_34"></a> g_01  := unapply(subs(a_E=a_E1,g_0(xx)),x): <span style="color:red">#@  g_01  
</span><a name="line_35"></a> g_11  := unapply(subs(a_E=a_E1,g_1(xx)),x): <span style="color:red">#@  g_11  
</span><a name="line_36"></a> dg1   := unapply(subs(a_E=a_E1,dg(xx)),x): <span style="color:red">#@  dg1   
</span><a name="line_37"></a> square_norm_of_dg1 := unapply(evalf(square_norm_of_dg0(xx)),x): <span style="color:red">#@  square_norm_of_dg1 
</span><a name="line_38"></a> curvature1 := unapply(evalf(curvature0(xx)),x): <span style="color:red">#@  curvature1 
</span><a name="line_39"></a> square_norm_of_dg_z1 := unapply(subs(a_E=a_E1,square_norm_of_dg_z(z)),z): <span style="color:red">#@  square_norm_of_dg_z1 
</span><a name="line_40"></a> curvature_z1 := unapply(1 - factor(1-subs(a_E=a_E1,curvature_z(z))),z): <span style="color:red">#@  curvature_z1 
</span><a name="line_41"></a> g_stereo1 := unapply(evalf(subs(a_E=a_E0,g_stereo([u[1],u[2],u[3]]))),u): <span style="color:red">#@  g_stereo1 
</span><a name="line_42"></a>
<a name="line_43"></a> <span style="color:red">#@ laplacian_z_C0
</span><a name="line_44"></a> for i from 0 to 5 do 
<a name="line_45"></a>  laplacian_z_C0[i] := factor(subs(a_E=a_E0,laplacian_z_C[i])); 
<a name="line_46"></a> od;
<a name="line_47"></a>
<a name="line_48"></a> <span style="color:red">#@  laplacian_z0
</span><a name="line_49"></a> laplacian_z0 := proc(p)
<a name="line_50"></a>  (laplacian_z_C0[1] * diff(p,z[1]) + 
<a name="line_51"></a>   laplacian_z_C0[2] * diff(p,z[2]) + 
<a name="line_52"></a>   laplacian_z_C0[3] * diff(p,z[1],z[1]) + 
<a name="line_53"></a>   laplacian_z_C0[4] * diff(p,z[1],z[2]) + 
<a name="line_54"></a>   laplacian_z_C0[5] * diff(p,z[2],z[2]))/laplacian_z_C0[0]; 
<a name="line_55"></a> end:
<a name="line_56"></a>
<a name="line_57"></a> <span style="color:red">#@ omega0
</span><a name="line_58"></a> <span style="color:red">#@ omega1
</span><a name="line_59"></a> for i from 1 to 2 do
<a name="line_60"></a>  omega0[i] := unapply(simplify(subs(a_E=a_E0,omega[i](t))),t);
<a name="line_61"></a>  omega1[i] := unapply(evalf(subs(a_E=a_E1,omega[i](t))),t);
<a name="line_62"></a> od;
<a name="line_63"></a>
<a name="line_64"></a> if not(type(num_vertices_E,posint)) then
<a name="line_65"></a>  num_vertices_E := 14;
<a name="line_66"></a> fi;
<a name="line_67"></a>
<a name="line_68"></a> <span style="color:red">#@ v_E0
</span><a name="line_69"></a> <span style="color:red">#@ v_E1
</span><a name="line_70"></a> for i from 0 to num_vertices_E-1 do
<a name="line_71"></a>  v_E0[i] := combine(simplify(subs(a_E = a_E0,v_E[i]))):
<a name="line_72"></a>  v_E1[i] := evalf(v_E0[i]);
<a name="line_73"></a> od:
<a name="line_74"></a>
<a name="line_75"></a> assume(t::real):
<a name="line_76"></a>
<a name="line_77"></a> if not(type(num_curves_E,posint)) then
<a name="line_78"></a>  num_curves_E := 9;
<a name="line_79"></a> fi;
<a name="line_80"></a> 
<a name="line_81"></a> <span style="color:red">#@ c_E0
</span><a name="line_82"></a> <span style="color:red">#@ c_E1
</span><a name="line_83"></a> <span style="color:red">#@ c_param_E0
</span><a name="line_84"></a> <span style="color:red">#@ c_param_E1
</span><a name="line_85"></a> for i from 0 to num_curves_E - 1 do
<a name="line_86"></a>  c_E0[i] := unapply(simplify(subs(csgn(cos(t)^2-3)=-1,simplify(subs(a_E=a_E0,c_E[i](t))))),t):
<a name="line_87"></a>  c_E1[i] := unapply(evalf(c_E0[i](t)),t):
<a name="line_88"></a>  c_param_E0[i] := unapply(simplify(subs(a_E=a_E0,c_param_E[i](xx))),x);
<a name="line_89"></a>  c_param_E1[i] := unapply(simplify(evalf(subs(a_E=a_E1,c_param_E[i](xx)))),x);
<a name="line_90"></a> od:
<a name="line_91"></a>
<a name="line_92"></a> <span style="color:red">#@ yx0
</span><a name="line_93"></a> <span style="color:red">#@ dyx0
</span><a name="line_94"></a> <span style="color:red">#@ uy0
</span><a name="line_95"></a> <span style="color:red">#@ uf0
</span><a name="line_96"></a> for i from 1 to 2 do 
<a name="line_97"></a>  yx0[i]  := simplify(subs(a_E=a_E0,yx[i])):
<a name="line_98"></a>  dyx0[i] := simplify(subs(a_E=a_E0,dyx[i])):
<a name="line_99"></a>  uy0[i]  := simplify(subs(a_E=a_E0,uy[i])):
<a name="line_100"></a>  uf0[i]  := unapply(uy0[i],y):
<a name="line_101"></a> od:
<a name="line_102"></a>
<a name="line_103"></a> <span style="color:red">#@ xy0
</span><a name="line_104"></a> <span style="color:red">#@ dxy0
</span><a name="line_105"></a> for i from 1 to 4 do 
<a name="line_106"></a>  xy0[i]  := simplify(subs(a_E=a_E0,xy[i])):
<a name="line_107"></a>  dxy0[i] := simplify(subs(a_E=a_E0,dxy[i])):
<a name="line_108"></a> od:
<a name="line_109"></a>
<a name="line_110"></a> <span style="color:red">#@ uz0
</span><a name="line_111"></a> uz0[3] := simplify(subs(a_E=a_E0,uz[3])):
<a name="line_112"></a> uz0[4] := simplify(subs(a_E=a_E0,uz[4])):
<a name="line_113"></a>
<a name="line_114"></a> for i from 1 to 5 do
<a name="line_115"></a>  zx0[i]  := simplify(subs(a_E=a_E0,zx[i])):
<a name="line_116"></a>  dzx0[i] := simplify(subs(a_E=a_E0,dyx[i])):
<a name="line_117"></a> od:
<a name="line_118"></a>
<a name="line_119"></a> y_proj0 := unapply(subs(a_E=a_E0,y_proj(xx)),x): <span style="color:red">#@ y_proj0 
</span><a name="line_120"></a> z_proj0 := unapply(subs(a_E=a_E0,z_proj(xx)),x): <span style="color:red">#@ z_proj0 
</span><a name="line_121"></a> w_proj0 := unapply(subs(a_E=a_E0,w_proj(xx)),x): <span style="color:red">#@ w_proj0 
</span><a name="line_122"></a> z_to_w0 := unapply(subs(a_E=a_E0,z_to_w(z)),z):  <span style="color:red">#@ z_to_w0 
</span><a name="line_123"></a> y_to_w0 := unapply(subs(sqrt(y[2]^2)=abs(y[2]),subs(a_E=a_E0,y_to_w(y))),y): <span style="color:red">#@  y_to_w0 
</span><a name="line_124"></a> y_lift0 := unapply(subs(a_E=a_E0,y_lift(xx)),x): <span style="color:red">#@ y_lift0 
</span><a name="line_125"></a> z_lift0 := unapply(subs(a_E=a_E0,z_lift(xx)),x): <span style="color:red">#@ z_lift0 
</span><a name="line_126"></a>
<a name="line_127"></a> yx1[1] := simplify(evalf(subs(a_E=a_E1,yx[1]))): <span style="color:red">#@ yx1
</span><a name="line_128"></a> yx1[2] := simplify(evalf(subs(a_E=a_E1,yx[2]))):
<a name="line_129"></a> uy1[1] := simplify(evalf(subs(a_E=a_E1,uy[1]))): <span style="color:red">#@ uy1
</span><a name="line_130"></a> uy1[2] := simplify(evalf(subs(a_E=a_E1,uy[2]))):
<a name="line_131"></a> uf1[1] := unapply(uy1[1],y):                     <span style="color:red">#@ uf1
</span><a name="line_132"></a> uf1[2] := unapply(uy1[2],y):
<a name="line_133"></a> uz1[3] := simplify(evalf(subs(a_E=a_E1,uz[3]))): <span style="color:red">#@ uz1
</span><a name="line_134"></a> uz1[4] := simplify(evalf(subs(a_E=a_E1,uz[4]))):
<a name="line_135"></a>
<a name="line_136"></a> <span style="color:red">#@ zx1
</span><a name="line_137"></a> for i from 1 to 5 do
<a name="line_138"></a>  zx1[i] := simplify(evalf(subs(a_E=a_E1,zx[i]))):
<a name="line_139"></a> od:
<a name="line_140"></a>
<a name="line_141"></a> y_proj1 := unapply(evalf(subs(a_E=a_E1,y_proj(xx))),x): <span style="color:red">#@ y_proj1 
</span><a name="line_142"></a> z_proj1 := unapply(evalf(subs(a_E=a_E1,z_proj(xx))),x): <span style="color:red">#@ z_proj1 
</span><a name="line_143"></a> w_proj1 := unapply(evalf(subs(a_E=a_E1,w_proj(xx))),x): <span style="color:red">#@ w_proj1 
</span><a name="line_144"></a> z_to_w1 := unapply(evalf(subs(a_E=a_E1,z_to_w(z))),z):  <span style="color:red">#@ z_to_w1 
</span><a name="line_145"></a> y_to_w1 := unapply(subs(sqrt(y[2]^2)=abs(y[2]),evalf(subs(a_E=a_E1,y_to_w(y)))),y): <span style="color:red">#@  y_to_w1 
</span><a name="line_146"></a> y_lift1 := unapply(evalf(subs(a_E=a_E1,y_lift(xx))),x): <span style="color:red">#@ y_lift1 
</span><a name="line_147"></a> z_lift1 := unapply(evalf(subs(a_E=a_E1,z_lift(xx))),x): <span style="color:red">#@ z_lift1 
</span><a name="line_148"></a>
<a name="line_149"></a> z_rels0 := simplify(subs(a_E=a_E0,z_rels));   <span style="color:red">#@ z_rels0 
</span><a name="line_150"></a> NF_z0 := (u) -> NormalForm(u,z_rels0,z_vars); <span style="color:red">#@ NF_z0 
</span><a name="line_151"></a> y_rels0 := simplify(subs(a_E=a_E0,y_rels));   <span style="color:red">#@ y_rels0 
</span><a name="line_152"></a> NF_y0 := (u) -> NormalForm(u,y_rels0,y_vars); <span style="color:red">#@ NF_y0 
</span><a name="line_153"></a> x_rels0 := simplify(subs(a_E=a_E0,x_rels));   <span style="color:red">#@ x_rels0 
</span><a name="line_154"></a> NF_x0 := (u) -> NormalForm(u,x_rels0,x_vars); <span style="color:red">#@ NF_x0 
</span><a name="line_155"></a>
<a name="line_156"></a> <span style="color:red">#@ is_in_F4_E0_measure 
</span><a name="line_157"></a> is_in_F4_E0_measure := (x) -> max(0,-evalf(x[1]),-evalf(x[2])); 
<a name="line_158"></a>
<a name="line_159"></a> <span style="color:red">#@ is_in_F16_E0_measure
</span><a name="line_160"></a> is_in_F16_E0_measure := (x) ->
<a name="line_161"></a>   max(0,-evalf(x[1]),-evalf(x[2]),-evalf(x[3]),-evalf(g_11(x)));
<a name="line_162"></a>
<a name="line_163"></a> NULL;
<a name="line_164"></a>end:
<a name="line_165"></a>
<a name="line_166"></a><span style="color:red">#@ c_check_E0
</span><a name="line_167"></a>c_check_E0[0] := proc(x) max(abs(evalf(x[3])),abs(evalf(x[4]))); end:
<a name="line_168"></a>c_check_E0[1] := proc(x) max(abs(evalf(x[1]-x[2])),abs(evalf(x[4]))); end:
<a name="line_169"></a>c_check_E0[2] := proc(x) max(abs(evalf(x[1]+x[2])),abs(evalf(x[4]))); end:
<a name="line_170"></a>c_check_E0[3] := proc(x) max(abs(evalf(x[1])),evalf(-g_10(x))); end:
<a name="line_171"></a>c_check_E0[4] := proc(x) max(abs(evalf(x[2])),evalf( g_10(x))); end:
<a name="line_172"></a>c_check_E0[5] := proc(x) max(abs(evalf(x[2])),evalf(-g_10(x)),-evalf(x[3])); end:
<a name="line_173"></a>c_check_E0[6] := proc(x) max(abs(evalf(x[1])),evalf( g_10(x)),-evalf(x[3])); end:
<a name="line_174"></a>c_check_E0[7] := proc(x) max(abs(evalf(x[2])),evalf(-g_10(x)), evalf(x[3])); end:
<a name="line_175"></a>c_check_E0[8] := proc(x) max(abs(evalf(x[1])),evalf( g_10(x)), evalf(x[3])); end:
<a name="line_176"></a>
<a name="line_177"></a><span style="color:red">#@ classify_point_E0 
</span><a name="line_178"></a>classify_point_E0 := proc(x0::RR0_4,tol)
<a name="line_179"></a> local i;
<a name="line_180"></a>
<a name="line_181"></a> for i from 0 to 13 do
<a name="line_182"></a>  if d4f(x0,v_E0[i]) < tol then
<a name="line_183"></a>   return sprintf("V%d",i);
<a name="line_184"></a>  fi;
<a name="line_185"></a> od;
<a name="line_186"></a> for i from 0 to 8 do
<a name="line_187"></a>  if c_check_E0[i](x0) < tol then
<a name="line_188"></a>   return sprintf("C%d",i);
<a name="line_189"></a>  fi;
<a name="line_190"></a> od;
<a name="line_191"></a> return "FREE";
<a name="line_192"></a>end:
<a name="line_193"></a>
<a name="line_194"></a><span style="color:red">#@ is_in_F4_E0 
</span><a name="line_195"></a>is_in_F4_E0 := proc(x::RR0_4)
<a name="line_196"></a> is(x[1] >= 0) and is(x[2] >= 0);
<a name="line_197"></a>end:
<a name="line_198"></a>
<a name="line_199"></a><span style="color:red">#@ is_in_F16_E0 
</span><a name="line_200"></a>is_in_F16_E0 := proc(x::RR0_4)
<a name="line_201"></a> is(x[1] >= 0) and is(x[2] >= 0) and is(x[3] >= 0) and is(simplify(g_10(x)) >= 0);
<a name="line_202"></a>end:
<a name="line_203"></a>
<a name="line_204"></a><span style="color:red">#@ retract_F16_E0 
</span><a name="line_205"></a>retract_F16_E0 := proc(x::RR0_4)
<a name="line_206"></a> if is(g_10(x) >= 0) then 
<a name="line_207"></a>  [abs(x[1]),abs(x[2]),abs(x[3]),-abs(x[4])];
<a name="line_208"></a> elif is(g_10(x) <= 0) then 
<a name="line_209"></a>  [abs(x[2]),abs(x[1]),abs(x[3]),-abs(x[4])];
<a name="line_210"></a> else
<a name="line_211"></a>  FAIL;
<a name="line_212"></a> fi;
<a name="line_213"></a>end:
<a name="line_214"></a>
<a name="line_215"></a><span style="color:red">#@ is_in_F4_E1 
</span><a name="line_216"></a>is_in_F4_E1 := proc(x::RR0_4)
<a name="line_217"></a> evalf(x[1]) >= 0 and evalf(x[2]) >= 0;
<a name="line_218"></a>end:
<a name="line_219"></a>
<a name="line_220"></a><span style="color:red">#@ is_in_F16_E1 
</span><a name="line_221"></a>is_in_F16_E1 := proc(x::RR0_4)
<a name="line_222"></a> evalf(x[1]) >= 0 and evalf(x[2]) >= 0 and evalf(x[3]) >= 0 and evalf(g_10(x)) >= 0;
<a name="line_223"></a>end:
<a name="line_224"></a>
<a name="line_225"></a><span style="color:red">#@ retract_F16_E1 
</span><a name="line_226"></a>retract_F16_E1 := proc(x::RR0_4)
<a name="line_227"></a> if evalf(g_10(x)) >= 0 then 
<a name="line_228"></a>  [abs(x[1]),abs(x[2]),abs(x[3]),-abs(x[4])];
<a name="line_229"></a> else
<a name="line_230"></a>  [abs(x[2]),abs(x[1]),abs(x[3]),-abs(x[4])];
<a name="line_231"></a> fi;
<a name="line_232"></a>end:
<a name="line_233"></a>
<a name="line_234"></a>set_a_E0(1/sqrt(2));
<a name="line_235"></a>
  </pre>
 </body>
</html>
    