<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># The functions y_1 and y_2 generate the ring of invariants on X for <lambda^2,nu>
<a name="line_2"></a><span style="color:red">#@ yx
</span><a name="line_3"></a>yx[1] := x[3];
<a name="line_4"></a>yx[2] := (x[2]^2 - x[1]^2 - (a_E+1/a_E)*x[3]*x[4])/(2*a_E);
<a name="line_5"></a>
<a name="line_6"></a># u_i is an expression for x_i^2 in terms of y_1 and y_2
<a name="line_7"></a><span style="color:red">#@ uy
</span><a name="line_8"></a>uy[1] := (1-2*a_E*y[2])/2 - (y[2]-a_E)*(y[2]-1/a_E)*y[1]^2/2;
<a name="line_9"></a>uy[2] := (1+2*a_E*y[2])/2 - (y[2]+a_E)*(y[2]+1/a_E)*y[1]^2/2;
<a name="line_10"></a>
<a name="line_11"></a># xy[i] is an expression for x[i] in terms of y[1] and y[2], valid when x[1],x[2] >= 0
<a name="line_12"></a><span style="color:red">#@ xy
</span><a name="line_13"></a>xy[1] := sqrt(uy[1]);
<a name="line_14"></a>xy[2] := sqrt(uy[2]);
<a name="line_15"></a>xy[3] := y[1];
<a name="line_16"></a>xy[4] := - y[1] * y[2];
<a name="line_17"></a>
<a name="line_18"></a># u_3 is an expression for 4 x_1^2 x_2^2, and u_3 is an expression for x_1^2 + x_2^2
<a name="line_19"></a><span style="color:red">#@ uz
</span><a name="line_20"></a>uz[3] := (1 - z[1] - z[1]*z[2])^2 - z[2] * ((a_E+1/a_E)*z[1]-2*a_E)^2;
<a name="line_21"></a>uz[4] := 1 - z[1] - z[1]*z[2];
<a name="line_22"></a>
<a name="line_23"></a><span style="color:red">#@ uf
</span><a name="line_24"></a>uf[1] := unapply(uy[1],y);
<a name="line_25"></a>uf[2] := unapply(uy[2],y);
<a name="line_26"></a>
<a name="line_27"></a># The functions z_1 and z_2 generate the ring of invariants on X for G
<a name="line_28"></a><span style="color:red">#@ zy
</span><a name="line_29"></a>zy[1] := y[1]^2;
<a name="line_30"></a>zy[2] := y[2]^2;
<a name="line_31"></a>
<a name="line_32"></a><span style="color:red">#@ zx
</span><a name="line_33"></a>zx[1] := yx[1]^2;
<a name="line_34"></a>zx[2] := yx[2]^2;
<a name="line_35"></a>
<a name="line_36"></a><span style="color:red">#@ yz
</span><a name="line_37"></a>yz[1] := sqrt(z[1]);
<a name="line_38"></a>yz[2] := sqrt(z[2]);
<a name="line_39"></a>
<a name="line_40"></a># Some additional invariant functions
<a name="line_41"></a><span style="color:red">#@ zx
</span><a name="line_42"></a>zx[3] := x[1]^2 + x[2]^2;
<a name="line_43"></a>zx[4] := x[4]^2;
<a name="line_44"></a>zx[5] := (x[2]^2-x[1]^2)*x[3]*x[4]-(a_E+1/a_E)*x[3]^2*x[4]^2;
<a name="line_45"></a>
<a name="line_46"></a># The functions w_1 and w_2 give a homeomorphism X/G -> [0,1]^2
<a name="line_47"></a><span style="color:red">#@ wz
</span><a name="line_48"></a>wz[1] := z[1]*(1+(a_E+1/a_E)*sqrt(z[2])+z[2])/(1+2*a_E*sqrt(z[2]));
<a name="line_49"></a>wz[2] := sqrt(z[2])*(2*a_E*(1-z[1])+z[1]*sqrt(z[2]))/(1-z[1]+(1/a_E-a_E)*z[1]*sqrt(z[2]));
<a name="line_50"></a>
<a name="line_51"></a><span style="color:red">#@ wy
</span><a name="line_52"></a><span style="color:red">#@ wx
</span><a name="line_53"></a>for i from 1 to 2 do 
<a name="line_54"></a># wy[i] := subs({z[1]=y[1]^2,z[2]=y[2]^2},subs(sqrt(z[2]) = abs(y[2]),wz[i]));
<a name="line_55"></a> wy[i] := subs({z[1]=y[1]^2,z[2]=y[2]^2},wz[i]);
<a name="line_56"></a> wx[i] := subs({y[1]=yx[1],y[2]=yx[2]},wy[i]);
<a name="line_57"></a>od:
<a name="line_58"></a>
<a name="line_59"></a># Gradient operators
<a name="line_60"></a>grad_x := (u) -> add(diff(u,x[i]) * dx[i],i=1..4); <span style="color:red">#@ grad_x
</span><a name="line_61"></a>grad_y := (u) -> add(diff(u,y[i]) * dy[i],i=1..2); <span style="color:red">#@ grad_y
</span><a name="line_62"></a>grad_z := (u) -> add(diff(u,z[i]) * dz[i],i=1..2); <span style="color:red">#@ grad_z
</span><a name="line_63"></a>
<a name="line_64"></a><span style="color:red">#@ dyx
</span><a name="line_65"></a><span style="color:red">#@ dzx
</span><a name="line_66"></a><span style="color:red">#@ dzy
</span><a name="line_67"></a><span style="color:red">#@ dyz
</span><a name="line_68"></a>for i from 1 to 2 do 
<a name="line_69"></a> dyx[i] := grad_x(yx[i]);
<a name="line_70"></a> dzx[i] := grad_x(zx[i]);
<a name="line_71"></a> dzy[i] := grad_y(zy[i]);
<a name="line_72"></a> dyz[i] := grad_z(yz[i]);
<a name="line_73"></a>od:
<a name="line_74"></a>
<a name="line_75"></a><span style="color:red">#@ dxy
</span><a name="line_76"></a>for i from 1 to 4 do
<a name="line_77"></a> dxy[i] := grad_y(xy[i]);
<a name="line_78"></a>od:
<a name="line_79"></a>
<a name="line_80"></a><span style="color:red">#@ dzx
</span><a name="line_81"></a>for i from 3 to 5 do # i = 1 to 2 already done 
<a name="line_82"></a> dzx[i] := grad_x(zx[i]);
<a name="line_83"></a>od:
<a name="line_84"></a>
<a name="line_85"></a># Projections from EX(a) to the y-plane and the z-plane
<a name="line_86"></a>y_proj := unapply([seq(yx[i],i=1..2)],x); <span style="color:red">#@ y_proj
</span><a name="line_87"></a>z_proj := unapply([seq(zx[i],i=1..2)],x); <span style="color:red">#@ z_proj
</span><a name="line_88"></a>
<a name="line_89"></a># Sections of y_proj and z_proj
<a name="line_90"></a>y_lift := (y) -> [sqrt(uf[1](y)),sqrt(uf[2](y)),y[1],-y[1]*y[2]]; <span style="color:red">#@ y_lift
</span><a name="line_91"></a>z_lift := (z) -> y_lift([sqrt(z[1]),sqrt(z[2])]);                 <span style="color:red">#@ z_lift
</span><a name="line_92"></a>
<a name="line_93"></a>######################################################################
<a name="line_94"></a># Various Grobner bases
<a name="line_95"></a>
<a name="line_96"></a>x_vars := lexdeg([z[1],z[2]],[y[1],y[2]],[x[1],x[2],x[3],x[4]]); <span style="color:red">#@ x_vars
</span><a name="line_97"></a>y_vars := lexdeg([x[1],x[2],x[3],x[4]],[z[1],z[2]],[y[1],y[2]]); <span style="color:red">#@ y_vars
</span><a name="line_98"></a>z_vars := lexdeg([x[1],x[2],x[3],x[4]],[y[1],y[2]],[z[1],z[2]]); <span style="color:red">#@ z_vars
</span><a name="line_99"></a>
<a name="line_100"></a><span style="color:red">#@ x_rels 
</span><a name="line_101"></a>x_rels := Basis(
<a name="line_102"></a> expand([y[1]-yx[1],y[2]-yx[2],
<a name="line_103"></a>         z[1]-zx[1],z[2]-zx[2],
<a name="line_104"></a>         rho(xx)-1,g(xx)] *~ a_E^4),
<a name="line_105"></a> x_vars
<a name="line_106"></a>);
<a name="line_107"></a>
<a name="line_108"></a><span style="color:red">#@ y_rels 
</span><a name="line_109"></a>y_rels := Basis(
<a name="line_110"></a> expand([z[1]-zy[1],z[2]-zy[2],
<a name="line_111"></a>         y[1]-yx[1],y[2]-yx[2],
<a name="line_112"></a>         x[1]^2-uy[1],x[2]^2-uy[2],
<a name="line_113"></a>         x[4]+y[1]*y[2]] *~ a_E^4),
<a name="line_114"></a> y_vars
<a name="line_115"></a>);
<a name="line_116"></a>
<a name="line_117"></a><span style="color:red">#@ z_rels 
</span><a name="line_118"></a>z_rels := Basis([op(y_rels),z[1]-zy[1],z[2]-zy[2]],z_vars);
<a name="line_119"></a>
<a name="line_120"></a># NF_x(u) rewrites u in terms of x[1],...,x[4]
<a name="line_121"></a># NF_y(u) rewrites u as a linear combination of {1,x[1],x[2],x[1]*x[2]}
<a name="line_122"></a>#          over R[y[1],y[2]].
<a name="line_123"></a># NF_z(u) rewrites u as a linear combination of 
<a name="line_124"></a>#          {x[1]^i*x[2]^j*y[1]^k*y[2]^l | 0 <= i,j,k,l <= 1}
<a name="line_125"></a>#           over R[z[1],z[2]].
<a name="line_126"></a>
<a name="line_127"></a>NF_x := (u) -> NormalForm(u,x_rels,x_vars); <span style="color:red">#@ NF_x
</span><a name="line_128"></a>NF_y := (u) -> NormalForm(u,y_rels,y_vars); <span style="color:red">#@ NF_y
</span><a name="line_129"></a>NF_z := (u) -> NormalForm(u,z_rels,z_vars); <span style="color:red">#@ NF_z
</span><a name="line_130"></a>
<a name="line_131"></a># List of monomials in x[1],..,x[4] that are reduced wrt the Grobner
<a name="line_132"></a># basis x_rels.  This relies on the fact that the leading monomials
<a name="line_133"></a># for the elements of the Grobner basis are x[4]^2, x[3]^2*x[4] and
<a name="line_134"></a># x[3]^4.
<a name="line_135"></a>
<a name="line_136"></a><span style="color:red">#@ x_reduced_basis 
</span><a name="line_137"></a>x_reduced_basis := proc(d)
<a name="line_138"></a> local i1,i2,i3,i4;
<a name="line_139"></a> [seq(seq(seq(seq(x[1]^i1*x[2]^i2*x[3]^i3*x[4]^i4,
<a name="line_140"></a>			i4=0..min(d-i1-i2-i3,1-floor(i3/2))),
<a name="line_141"></a>			i3=0..min(d-i1-i2,3)),
<a name="line_142"></a>			i2=0..d-i1),
<a name="line_143"></a>			i1=0..d)];
<a name="line_144"></a>end:
<a name="line_145"></a>
<a name="line_146"></a>######################################################################
<a name="line_147"></a>
<a name="line_148"></a># Action on the ring of polynomial functions on R4
<a name="line_149"></a>
<a name="line_150"></a><span style="color:red">#@ act_A_rule
</span><a name="line_151"></a><span style="color:red">#@ act_A
</span><a name="line_152"></a><span style="color:red">#@ R4_matrix
</span><a name="line_153"></a>
<a name="line_154"></a>for T in G16 do 
<a name="line_155"></a> act_A_rule[T] := {seq(x[i] = act_R4[G_inv(T)](xx)[i],i=1..4)};
<a name="line_156"></a> act_A_rule[T] := {op(act_A_rule[T]),op(subs(x=dx,act_A_rule[T]))};
<a name="line_157"></a> act_A[T] := (u) -> subs(act_A_rule[T],u);
<a name="line_158"></a>
<a name="line_159"></a> act_A_rule[T] := {op(act_A_rule[T]),
<a name="line_160"></a>  seq(y[i] = factor(act_A[T](yx[i])/yx[i]) * y[i],i = 1..2),
<a name="line_161"></a>  seq(dy[i] = factor(act_A[T](yx[i])/yx[i]) * dy[i],i = 1..2)
<a name="line_162"></a> };
<a name="line_163"></a>
<a name="line_164"></a> act_A[T] := (u) -> subs(act_A_rule[T],u);
<a name="line_165"></a> R4_matrix[T] := Matrix([seq([seq(coeff(act_R4[T](xx)[i],x[j],1),j=1..4)],i=1..4)]);
<a name="line_166"></a>od:
<a name="line_167"></a>
<a name="line_168"></a># For some reason we had trouble making the definitions below in a loop
<a name="line_169"></a>act_A[1]     := (u) -> subs(act_A_rule[1],u):
<a name="line_170"></a>act_A[L]     := (u) -> subs(act_A_rule[L],u):
<a name="line_171"></a>act_A[LL]    := (u) -> subs(act_A_rule[LL],u):
<a name="line_172"></a>act_A[LLL]   := (u) -> subs(act_A_rule[LLL],u):
<a name="line_173"></a>act_A[M]     := (u) -> subs(act_A_rule[M],u):
<a name="line_174"></a>act_A[LM]    := (u) -> subs(act_A_rule[LM],u):
<a name="line_175"></a>act_A[LLM]   := (u) -> subs(act_A_rule[LLM],u):
<a name="line_176"></a>act_A[LLLM]  := (u) -> subs(act_A_rule[LLLM],u):
<a name="line_177"></a>act_A[N]     := (u) -> subs(act_A_rule[N],u):
<a name="line_178"></a>act_A[LN]    := (u) -> subs(act_A_rule[LN],u):
<a name="line_179"></a>act_A[LLN]   := (u) -> subs(act_A_rule[LLN],u):
<a name="line_180"></a>act_A[LLLN]  := (u) -> subs(act_A_rule[LLLN],u):
<a name="line_181"></a>act_A[MN]    := (u) -> subs(act_A_rule[MN],u):
<a name="line_182"></a>act_A[LMN]   := (u) -> subs(act_A_rule[LMN],u):
<a name="line_183"></a>act_A[LLMN]  := (u) -> subs(act_A_rule[LLMN],u):
<a name="line_184"></a>act_A[LLLMN] := (u) -> subs(act_A_rule[LLLMN],u):
<a name="line_185"></a>
<a name="line_186"></a><span style="color:red">#@ check_char
</span><a name="line_187"></a>check_char := proc(u)
<a name="line_188"></a> local Q,i;
<a name="line_189"></a> Q := [seq(factor(act_A[T](u)/u),T in G16)];
<a name="line_190"></a> for i from 0 to 7 do
<a name="line_191"></a>  if Q = [seq(character[i][T],T in G16)] then
<a name="line_192"></a>   return i;
<a name="line_193"></a>  fi;
<a name="line_194"></a> od;
<a name="line_195"></a> return FAIL;
<a name="line_196"></a>end:
<a name="line_197"></a>
<a name="line_198"></a><span style="color:red">#@ G16_average 
</span><a name="line_199"></a>G16_average := (u) -> add(act_A[T](u),T in G16)/16;
<a name="line_200"></a>
<a name="line_201"></a><span style="color:red">#@ G16_twisted_average 
</span><a name="line_202"></a>G16_twisted_average := (k,u) -> add(character[k][T] * act_A[T](u),T in G16)/16;
<a name="line_203"></a>
<a name="line_204"></a># Action of G on the y-plane
<a name="line_205"></a>
<a name="line_206"></a><span style="color:red">#@ act_hex
</span><a name="line_207"></a>act_hex[1]    := (y) -> [ y[1], y[2]];
<a name="line_208"></a>act_hex[L]    := (y) -> [ y[1],-y[2]];
<a name="line_209"></a>act_hex[LL]   := (y) -> [ y[1], y[2]];
<a name="line_210"></a>act_hex[LLL]  := (y) -> [ y[1],-y[2]];
<a name="line_211"></a>act_hex[M]    := (y) -> [-y[1], y[2]];
<a name="line_212"></a>act_hex[LM]   := (y) -> [-y[1],-y[2]];
<a name="line_213"></a>act_hex[LLM]  := (y) -> [-y[1], y[2]];
<a name="line_214"></a>act_hex[LLLM] := (y) -> [-y[1],-y[2]];
<a name="line_215"></a>act_hex[N]    := (y) -> [ y[1], y[2]];
<a name="line_216"></a>act_hex[LN]   := (y) -> [ y[1],-y[2]];
<a name="line_217"></a>act_hex[LLN]  := (y) -> [ y[1], y[2]];
<a name="line_218"></a>act_hex[LLLN] := (y) -> [ y[1],-y[2]];
<a name="line_219"></a>act_hex[MN]   := (y) -> [-y[1], y[2]];
<a name="line_220"></a>act_hex[LMN]  := (y) -> [-y[1],-y[2]];
<a name="line_221"></a>act_hex[LLMN] := (y) -> [-y[1], y[2]];
<a name="line_222"></a>act_hex[LLLMN]:= (y) -> [-y[1],-y[2]];
<a name="line_223"></a>
<a name="line_224"></a># Invariants for the action twisted by a linear character (as listed in group.mpl)
<a name="line_225"></a># These twisted invariants form a module over the ring of untwisted
<a name="line_226"></a># invariants, with generators as listed below.
<a name="line_227"></a>
<a name="line_228"></a><span style="color:red">#@ twisted_invariant
</span><a name="line_229"></a>
<a name="line_230"></a>twisted_invariant[0] := 1;
<a name="line_231"></a>twisted_invariant[1] := yx[2];
<a name="line_232"></a>twisted_invariant[2] := yx[1];
<a name="line_233"></a>twisted_invariant[3] := yx[1]*yx[2];
<a name="line_234"></a>twisted_invariant[4] := x[1]*x[2]:
<a name="line_235"></a>twisted_invariant[5] := x[1]*x[2]*yx[2]:
<a name="line_236"></a>twisted_invariant[6] := x[1]*x[2]*yx[1]:
<a name="line_237"></a>twisted_invariant[7] := x[1]*x[2]*yx[1]*yx[2]:
<a name="line_238"></a>
<a name="line_239"></a>######################################################################
<a name="line_240"></a>
<a name="line_241"></a><span style="color:red">#@ square_norm_of_dg_z 
</span><a name="line_242"></a>square_norm_of_dg_z := (z) -> 
<a name="line_243"></a> (1+z[2])*(((1/a_E^2-1)*z[1])^2+(2/a_E^2-8)*z[1]+4)+
<a name="line_244"></a> (1-z[2])*z[1]*2*(2-1/a_E^2);
<a name="line_245"></a>
<a name="line_246"></a><span style="color:red">#@ curvature_z 
</span><a name="line_247"></a>curvature_z := (z) ->
<a name="line_248"></a>  1 - 16*(1/a_E^2+(3*a_E^2-1)^2/(4*a_E^6)*z[1]^2-4*z[2]
<a name="line_249"></a>          -(3*a_E^2-1)/a_E^4*z[1]+4*(a_E^(-2)-1)*z[1]*z[2]
<a name="line_250"></a>          +4*(2-a_E^(-2))*z[1]^2*z[2]^2+(3+4*a_E^(-2)-3*a_E^(-4))*z[1]^2*z[2])/
<a name="line_251"></a>          square_norm_of_dg_z(z)^2;
<a name="line_252"></a>
<a name="line_253"></a>######################################################################
<a name="line_254"></a>
<a name="line_255"></a><span style="color:red">#@ z_to_w 
</span><a name="line_256"></a>z_to_w := unapply([wz[1],wz[2]],z);
<a name="line_257"></a>
<a name="line_258"></a># The map z_to_w involves quotients of functions that are visibly 
<a name="line_259"></a># continuous.  One denominator vanishes at the point z = [1,0].
<a name="line_260"></a># The function below is the unique continuous extension.
<a name="line_261"></a>
<a name="line_262"></a><span style="color:red">#@ z_to_wa 
</span><a name="line_263"></a>z_to_wa := proc(z)
<a name="line_264"></a> if z[1] = 1 and z[2] = 0 then return([1,0]) else return(z_to_w(z)); fi;
<a name="line_265"></a>end:
<a name="line_266"></a>
<a name="line_267"></a>w_proj := unapply(z_to_w(zx),x):          <span style="color:red">#@ w_proj
</span><a name="line_268"></a>w_proja := (x) -> z_to_wa(z_proj(x)):     <span style="color:red">#@ w_proj_a
</span><a name="line_269"></a>
<a name="line_270"></a>y_to_w := (y) -> z_to_w([y[1]^2,y[2]^2]); <span style="color:red">#@ y_to_w
</span><a name="line_271"></a>
<a name="line_272"></a># To find y with y |-> w we first solve p_0(y[2])/p_1(y[2]) = w[2] (where p_0 and p_1
<a name="line_273"></a># are as defined below), 
<a name="line_274"></a>
<a name="line_275"></a><span style="color:red">#@ w_to_y_p_0 
</span><a name="line_276"></a>w_to_y_p_0 := (t,w1) -> t*((1+w1)*t^2+(1/a_E+a_E+(1/2/a_E-2*a_E)*w1)*t+(1-w1));
<a name="line_277"></a>
<a name="line_278"></a><span style="color:red">#@ w_to_y_p_1 
</span><a name="line_279"></a>w_to_y_p_1 := (t,w1) -> (1/2/a_E+w1*(1/a_E-a_E))*t^2 + 1/2*((1/a_E^2-1)*(1+w1)+2*(1-w1))*t + 1/2/a_E*(1-w1);
<a name="line_280"></a>
<a name="line_281"></a><span style="color:red">#@ w_to_y_p   
</span><a name="line_282"></a>w_to_y_p   := (t,w1) -> w_to_y_p_0(t,w1)/w_to_y_p_1(t,w1);
<a name="line_283"></a>
<a name="line_284"></a><span style="color:red">#@ w_to_y_q   
</span><a name="line_285"></a>w_to_y_q   := (t,w1,w2) -> w2 * w_to_y_p_1(t,w1) - w_to_y_p_0(t,w1);
<a name="line_286"></a>
<a name="line_287"></a><span style="color:red">#@ w_to_y_y_1 
</span><a name="line_288"></a>w_to_y_y_1 := (w1,y2) -> sqrt(w1*(1+2*a_E*y2)/((y2+1/a_E)*(y2+a_E)));
<a name="line_289"></a>
<a name="line_290"></a># Coefficients of t in the numerator of the derivative of w_to_y_p.
<a name="line_291"></a><span style="color:red">#@ w_to_y_dpc[0] 
</span><a name="line_292"></a>w_to_y_dpc[0] := (1-w1)^2/(2*a_E);
<a name="line_293"></a>
<a name="line_294"></a><span style="color:red">#@ w_to_y_dpc[1] 
</span><a name="line_295"></a>w_to_y_dpc[1] := (1-w1)*((3/2/a_E^2-1)*w1+(1/a_E^2+1)*(1-w1));
<a name="line_296"></a>
<a name="line_297"></a><span style="color:red">#@ w_to_y_dpc[2] 
</span><a name="line_298"></a>w_to_y_dpc[2] := (1-a_E^2)/a_E^3*(1-w1)/2 + 
<a name="line_299"></a>                 (1-a_E^2)*(3/2-a_E^2)/a_E^3*w1^2 +
<a name="line_300"></a>		 (1+2*a_E^2)*(5-a_E^2)/4/a_E^3*w1*(1-w1) +
<a name="line_301"></a>		 (5+a_E^2)/2/a_E*(1-w1)^2;
<a name="line_302"></a>
<a name="line_303"></a><span style="color:red">#@ w_to_y_dpc[3] 
</span><a name="line_304"></a>w_to_y_dpc[3] := (1+w1)/a_E^2*(2*(1-a_E^2)*w1+(1+a_E^2)*(1-w1));
<a name="line_305"></a>
<a name="line_306"></a><span style="color:red">#@ w_to_y_dpc[4] 
</span><a name="line_307"></a>w_to_y_dpc[4] := (1+w1)*(1+2*(1-a_E^2)*w1)/(2*a_E);
<a name="line_308"></a>
<a name="line_309"></a><span style="color:red">#@ w_to_y1 
</span><a name="line_310"></a>w_to_y1 := proc(w)
<a name="line_311"></a> option remember;
<a name="line_312"></a> local p,t,tt,y1,y2;
<a name="line_313"></a> p := subs(a_E=a_E1,w_to_y_q(t,w[1],w[2]));
<a name="line_314"></a> tt := select(t -> (t^2 <= 1/a_E1-a_E1),[fsolve(p=0,t)]);
<a name="line_315"></a> if tt = [] then return [FAIL,tt]; fi;
<a name="line_316"></a> y2 := max(0,op(tt));
<a name="line_317"></a> y1 := evalf(subs(a_E=a_E1,w_to_y_y_1(w[1],y2)));
<a name="line_318"></a> return([y1,y2]);
<a name="line_319"></a>end:
<a name="line_320"></a>
<a name="line_321"></a><span style="color:red">#@ w_lift1 
</span><a name="line_322"></a>w_lift1 := (w) -> map(Re,evalf(subs(a_E=a_E1,y_lift(w_to_y1(w)))));
<a name="line_323"></a>
<a name="line_324"></a>protect('uf','uz','wy','wz','xy','yx','yz','zx','zy','dxy','dyx','dyz','dzx','dzx','dzy');  </pre>
 </body>
</html>
    