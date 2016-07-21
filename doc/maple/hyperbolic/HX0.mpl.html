<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># Many functions related to the hyperbolic family treat the parameter a_H
<a name="line_2"></a># as a symbol.  One might also want to work with a numeric value of a_H.
<a name="line_3"></a># For that, we can invoke set_a_H0(4/5) (for example).  This will set
<a name="line_4"></a># a_H0 to the exact rational value 4/5, and a_H1 to the approximate value
<a name="line_5"></a># 0.8.  It will also set a long list of other global variables.  Typically,
<a name="line_6"></a># there is an existing global variable, say foo, whose value involves
<a name="line_7"></a># a_H, and the function set_a_H0(4/5) will set foo0 and foo1 to the
<a name="line_8"></a># values obtained by substituting a_H = 4/5 or a_H = 0.8 in foo.
<a name="line_9"></a>
<a name="line_10"></a>set_a_H0 := proc(a)
<a name="line_11"></a> global a_H0,a_H1,ap_H0,ap_H1,am_H0,am_H1,v_H0,v_H1,
<a name="line_12"></a>        v_H0xy,v_H1xy,v_H_label,beta0,beta1,beta_SL2R0,beta_SL2R1,
<a name="line_13"></a>        beta_matrix,beta0_matrix,beta1_matrix,c_H0,c_H1,c_H_label,
<a name="line_14"></a>        c_H_p0,c_colour,s_H0,c_H_speed0,
<a name="line_15"></a>        lambda_H0,lambda_inv_H0,lambda_sq_H0,mu_H0,nu_H0,
<a name="line_16"></a>        lambda_H1,lambda_inv_H1,lambda_sq_H1,mu_H1,nu_H1,
<a name="line_17"></a>        act_H0,act_H1,T,min_centre_a0,
<a name="line_18"></a>        corner_shift_H0,corner_unshift_H0,
<a name="line_19"></a>        square_diffeo_H_ca0,square_diffeo_H_ra0,square_diffeo_H_ma0,square_diffeo_H_pa0,
<a name="line_20"></a>        square_diffeo_H_cb0,square_diffeo_H_rb0,square_diffeo_H_mb0,square_diffeo_H_pb0;
<a name="line_21"></a>
<a name="line_22"></a> local ii,i,j,k,c_H_eqs,alpha,lambda,s;
<a name="line_23"></a>
<a name="line_24"></a> a_H0 := a;               <span style="color:red">#@ a_H0
</span><a name="line_25"></a> a_H1 := evalf(a);        <span style="color:red">#@ a_H1
</span><a name="line_26"></a>
<a name="line_27"></a> ap_H0 := sqrt(1+a_H0^2); <span style="color:red">#@ ap_H0
</span><a name="line_28"></a> am_H0 := sqrt(1-a_H0^2); <span style="color:red">#@ am_H0
</span><a name="line_29"></a> ap_H1 := sqrt(1+a_H1^2); <span style="color:red">#@ ap_H1
</span><a name="line_30"></a> am_H1 := sqrt(1-a_H1^2); <span style="color:red">#@ am_H1
</span><a name="line_31"></a>
<a name="line_32"></a> for ii in indices(v_H) do
<a name="line_33"></a>  i := op(ii);
<a name="line_34"></a>  v_H0[i] := simplify(subs(a_H=a_H0,v_H[i])); <span style="color:red">#@ v_H0
</span><a name="line_35"></a>  v_H0xy[i] := [Re(v_H0[i]),Im(v_H0[i])];     <span style="color:red">#@ v_H0xy
</span><a name="line_36"></a>  v_H1[i] := evalf(v_H0[i]);                  <span style="color:red">#@ v_H1
</span><a name="line_37"></a>  v_H1xy[i] := [Re(v_H1[i]),Im(v_H1[i])];     <span style="color:red">#@ v_H1xy
</span><a name="line_38"></a>  v_H_label[i] := TEXT(v_H1xy[i],i);          <span style="color:red">#@ v_H_label
</span><a name="line_39"></a> od:
<a name="line_40"></a>
<a name="line_41"></a> for k from 0 to 7 do
<a name="line_42"></a>  beta0[k] := unapply(evalf(subs(a_H=a_H0,beta[k](z))),z):      <span style="color:red">#@ beta0
</span><a name="line_43"></a>  beta1[k] := unapply(evalf(subs(a_H=a_H1,beta[k](z))),z):      <span style="color:red">#@ beta1
</span><a name="line_44"></a>  beta_SL2R0[k] := map(evalf,map2(subs,a_H=a_H0,beta_SL2R[k])); <span style="color:red">#@ beta_SL2R0
</span><a name="line_45"></a>  beta_SL2R1[k] := map(evalf,map2(subs,a_H=a_H1,beta_SL2R[k])); <span style="color:red">#@ beta_SL2R1
</span><a name="line_46"></a>  beta_matrix[k]  := map(simplify,mobius_matrix(beta[k](z),z)); <span style="color:red">#@ beta_matrix
</span><a name="line_47"></a>  beta0_matrix[k] := mobius_matrix(beta0[k](z),z);              <span style="color:red">#@ beta0_matrix
</span><a name="line_48"></a>  beta1_matrix[k] := mobius_matrix(beta1[k](z),z);              <span style="color:red">#@ beta1_matrix
</span><a name="line_49"></a> od:
<a name="line_50"></a>
<a name="line_51"></a> for i from 0 to 8 do
<a name="line_52"></a>  c_H0[i] := unapply(simplify((subs(a_H=a_H0,c_H[i](s)))),s): <span style="color:red">#@ c_H0
</span><a name="line_53"></a>  c_H1[i] := unapply(evalf((subs(a_H=a_H0,c_H0[i](s)))),s):   <span style="color:red">#@ c_H1
</span><a name="line_54"></a>  c_H_label[i] := ctext(c_H0[i](0),i,colour = c_colour[i]):   <span style="color:red">#@ c_H_label
</span><a name="line_55"></a> od:
<a name="line_56"></a>
<a name="line_57"></a> c_H_eqs := [abs(v_H0[1])^2 + 1 - 2*Re(v_H0[1])*x - 2*Im(v_H0[1])*y,
<a name="line_58"></a>	       abs(v_H0[3])^2 + 1 - 2*Re(v_H0[3])*x - 2*Im(v_H0[3])*y]:
<a name="line_59"></a>
<a name="line_60"></a> c_H_p0[17] := subs(solve(c_H_eqs,{x,y}),x+I*y);
<a name="line_61"></a> for k from 1 to 3 do c_H_p0[17+k] := I^k * c_H_p0[17]: od:
<a name="line_62"></a>
<a name="line_63"></a> for i from 17 to 20 do
<a name="line_64"></a>  c_colour[i] := "DarkGreen";
<a name="line_65"></a>  c_H0[i] := unapply(xi_curve(c_H_p0[i],s),s);
<a name="line_66"></a>  c_H_label[i] := ctext(c_H0[i](0),i,colour = c_colour[i]):
<a name="line_67"></a> od:
<a name="line_68"></a>
<a name="line_69"></a> lambda_H0     := lambda_H;                          <span style="color:red">#@ lambda_H0
</span><a name="line_70"></a> lambda_inv_H0 := lambda_inv_H;                      <span style="color:red">#@ lambda_inv_H0
</span><a name="line_71"></a> lambda_sq_H0  := lambda_sq_H;                       <span style="color:red">#@ lambda_sq_H0
</span><a name="line_72"></a> mu_H0         := unapply(subs(a_H=a_H0,mu_H(z)),z); <span style="color:red">#@ mu_H0
</span><a name="line_73"></a> nu_H0         := nu_H:                              <span style="color:red">#@ nu_H0
</span><a name="line_74"></a>
<a name="line_75"></a> lambda_H1     := lambda_H;                                 <span style="color:red">#@ lambda_H1
</span><a name="line_76"></a> lambda_inv_H1 := lambda_inv_H;                             <span style="color:red">#@ lambda_inv_H1
</span><a name="line_77"></a> lambda_sq_H1  := lambda_sq_H;                              <span style="color:red">#@ lambda_sq_H1
</span><a name="line_78"></a> mu_H1         := unapply(evalf(subs(a_H=a_H0,mu_H(z))),z); <span style="color:red">#@ mu_H1
</span><a name="line_79"></a> nu_H1         := nu_H:                                     <span style="color:red">#@ nu_H1
</span><a name="line_80"></a>
<a name="line_81"></a> for T in G16 do
<a name="line_82"></a>  act_H0[T] := unapply(simplify(subs(a_H=a_H0,act_H[T](z))),z): <span style="color:red">#@ act_H0
</span><a name="line_83"></a>  act_H1[T] := unapply(evalf(subs(a_H=a_H1,act_H[T](z))),z):    <span style="color:red">#@ act_H1
</span><a name="line_84"></a> od:
<a name="line_85"></a>
<a name="line_86"></a> for i from 0 to 4 do
<a name="line_87"></a>  s_H0[i] := evalf(subs({a_H=a_H0},s_H[i]));  <span style="color:red">#@ s_H0
</span><a name="line_88"></a> od;
<a name="line_89"></a>
<a name="line_90"></a> for i from 0 to 8 do
<a name="line_91"></a>  c_H_speed0[i] := evalf(subs({a_H=a_H0},c_H_speed[i])); <span style="color:red">#@ c_H_speed0
</span><a name="line_92"></a> od;
<a name="line_93"></a>
<a name="line_94"></a> for i in [0,3,4,7,8] do
<a name="line_95"></a>  c_H_p0[i] := evalf(subs(a_H=a_H0,c_H_p[i])); <span style="color:red">#@ c_H_p0
</span><a name="line_96"></a> od;
<a name="line_97"></a>
<a name="line_98"></a> min_centre_a0 := evalf(subs(a_H=a_H0,min_centre_a)); <span style="color:red">#@ min_centre_a0 
</span><a name="line_99"></a>
<a name="line_100"></a> <span style="color:red">#@ corner_shift_H0
</span><a name="line_101"></a> <span style="color:red">#@ corner_unshift_H0
</span><a name="line_102"></a> for i from 1 to 3 do
<a name="line_103"></a>  j := [11,3,6,0][i];
<a name="line_104"></a>  k := [11,3,6,0][i+1];
<a name="line_105"></a>  alpha := v_H0[j];
<a name="line_106"></a>  lambda := conjugate((v_H0[k] - v_H0[j])/(1 - conjugate(v_H0[j])*v_H0[k]));
<a name="line_107"></a>  lambda := lambda/abs(lambda):
<a name="line_108"></a>  corner_shift_H0[j] := unapply((lambda*z - lambda*alpha)/(1 - conjugate(alpha)*z),z);
<a name="line_109"></a>  corner_unshift_H0[j] := unapply((z + alpha*lambda)/(conjugate(alpha)*z + lambda),z);
<a name="line_110"></a> od:
<a name="line_111"></a>
<a name="line_112"></a> square_diffeo_H_ca0 := evalf(subs(a_H=a_H0,square_diffeo_H_ca));               <span style="color:red">#@ square_diffeo_H_ca0 
</span><a name="line_113"></a> square_diffeo_H_ra0 := evalf(subs(a_H=a_H0,square_diffeo_H_ra));               <span style="color:red">#@ square_diffeo_H_ra0 
</span><a name="line_114"></a> square_diffeo_H_cb0 := evalf(subs(a_H=a_H0,square_diffeo_H_cb));               <span style="color:red">#@ square_diffeo_H_cb0 
</span><a name="line_115"></a> square_diffeo_H_rb0 := evalf(subs(a_H=a_H0,square_diffeo_H_rb));               <span style="color:red">#@ square_diffeo_H_rb0 
</span><a name="line_116"></a> square_diffeo_H_ma0 := unapply(evalf(subs(a_H=a_H0,square_diffeo_H_ma(z))),z); <span style="color:red">#@ square_diffeo_H_ma0 
</span><a name="line_117"></a> square_diffeo_H_pa0 := unapply(evalf(subs(a_H=a_H0,square_diffeo_H_pa(z))),z); <span style="color:red">#@ square_diffeo_H_pa0 
</span><a name="line_118"></a> square_diffeo_H_mb0 := unapply(evalf(subs(a_H=a_H0,square_diffeo_H_mb(z))),z); <span style="color:red">#@ square_diffeo_H_mb0 
</span><a name="line_119"></a> square_diffeo_H_pb0 := unapply(evalf(subs(a_H=a_H0,square_diffeo_H_pb(z))),z); <span style="color:red">#@ square_diffeo_H_pb0 
</span><a name="line_120"></a>end:
<a name="line_121"></a>
<a name="line_122"></a><span style="color:red">#@ c_check_H0
</span><a name="line_123"></a>c_check_H0[0] := (z) -> abs(z - xi(c_H_p0[0],z));
<a name="line_124"></a>c_check_H0[1] := (z) -> abs(z -  I*conjugate(z));
<a name="line_125"></a>c_check_H0[2] := (z) -> abs(z +  I*conjugate(z));
<a name="line_126"></a>c_check_H0[3] := (z) -> abs(z - xi(c_H_p0[3],z));
<a name="line_127"></a>c_check_H0[4] := (z) -> abs(z - xi(c_H_p0[4],z));
<a name="line_128"></a>c_check_H0[5] := (z) -> abs(z -    conjugate(z));
<a name="line_129"></a>c_check_H0[6] := (z) -> abs(z +    conjugate(z));
<a name="line_130"></a>c_check_H0[7] := (z) -> abs(z - xi(c_H_p0[7],z));
<a name="line_131"></a>c_check_H0[8] := (z) -> abs(z - xi(c_H_p0[8],z));
<a name="line_132"></a>
<a name="line_133"></a><span style="color:red">#@ is_in_F1_H0 
</span><a name="line_134"></a>is_in_F1_H0 := proc(z) 
<a name="line_135"></a> local T,w,r;
<a name="line_136"></a>
<a name="line_137"></a> for T in [[0],[2],[4],[6],[0,7],[1,2],[2,1],[3,4],[4,3],[5,6],[6,5],[7,0]] do
<a name="line_138"></a>  w := evalf(act_Pi0(T,z));
<a name="line_139"></a>  r := evalf(z * conjugate(z) - w * conjugate(w));
<a name="line_140"></a>  if r > 0 then return false; fi;
<a name="line_141"></a> od;
<a name="line_142"></a>
<a name="line_143"></a> return true;
<a name="line_144"></a>end;
<a name="line_145"></a>
<a name="line_146"></a><span style="color:red">#@ is_in_F4_H0 
</span><a name="line_147"></a>is_in_F4_H0 := proc(z)
<a name="line_148"></a> local z0;
<a name="line_149"></a> z0 := evalf(z);
<a name="line_150"></a> is_in_F1_H0(z0) and (Re(z0) >= 0) and (Im(z0) >= 0);
<a name="line_151"></a>end;
<a name="line_152"></a>
<a name="line_153"></a><span style="color:red">#@ is_in_F16_H0 
</span><a name="line_154"></a>is_in_F16_H0 := proc(z)
<a name="line_155"></a> local m,d;
<a name="line_156"></a>
<a name="line_157"></a> if not is_in_F4_H0(z) then return false; fi;
<a name="line_158"></a> if evalf(Im(z) - Re(z)) > 0 then return false; fi;
<a name="line_159"></a> m := c_H_p0[0];
<a name="line_160"></a> d := evalf((z-m)*conjugate(z-m) - m*conjugate(m) + 1);
<a name="line_161"></a> if d < 0 then return false; fi;
<a name="line_162"></a> return true;
<a name="line_163"></a>end;
<a name="line_164"></a>
<a name="line_165"></a># Given z in Delta, this returns [T0,z0] where  T0 is in Pi
<a name="line_166"></a># and the point T0 z = z0 lies in F1.
<a name="line_167"></a>
<a name="line_168"></a><span style="color:red">#@ retract_F1_H0_aux 
</span><a name="line_169"></a>retract_F1_H0_aux := proc(z) 
<a name="line_170"></a> local z0,z1,T0,T1,T;
<a name="line_171"></a>
<a name="line_172"></a> z0 := evalf(z);
<a name="line_173"></a> T0 := []:
<a name="line_174"></a> for T in [[0],[2],[4],[6],[0,7],[1,2],[2,1],[3,4],[4,3],[5,6],[6,5],[7,0]] do
<a name="line_175"></a>  z1 := evalf(act_Pi1_map(T)(z));
<a name="line_176"></a>  if abs(z1) < abs(z0) then
<a name="line_177"></a>   T0 := T;
<a name="line_178"></a>   z0 := z1;
<a name="line_179"></a>  fi:
<a name="line_180"></a> od:
<a name="line_181"></a>
<a name="line_182"></a> if T0 = [] then
<a name="line_183"></a>  return([[],z0]);
<a name="line_184"></a> else
<a name="line_185"></a>  T1,z1 := op(retract_F1_H0_aux(z0));
<a name="line_186"></a>  return([Pi_mult(T1,T0),z1]);
<a name="line_187"></a> fi;
<a name="line_188"></a>end:
<a name="line_189"></a>
<a name="line_190"></a><span style="color:red">#@ retract_F1_H0 
</span><a name="line_191"></a>retract_F1_H0 := (z) -> retract_F1_H0_aux(z)[2]: 
<a name="line_192"></a>
<a name="line_193"></a># Given z in Delta, this returns [T1,T0,z0] where T1 is in G and T0 is in Pi
<a name="line_194"></a># and the point T1 T0 z = z0 lies in F16.
<a name="line_195"></a>
<a name="line_196"></a><span style="color:red">#@ retract_F16_H0_aux
</span><a name="line_197"></a>retract_F16_H0_aux := proc(z)
<a name="line_198"></a> local T0,T1,z0,m,d;
<a name="line_199"></a>
<a name="line_200"></a> T0,z0 := op(retract_F1_H0_aux(z));
<a name="line_201"></a>
<a name="line_202"></a> if Re(z0) < 0 then
<a name="line_203"></a>  if Im(z0) < 0 then
<a name="line_204"></a>   T1 := LL;
<a name="line_205"></a>  else
<a name="line_206"></a>   T1 := LLL;
<a name="line_207"></a>  fi:
<a name="line_208"></a> else
<a name="line_209"></a>  if Im(z0) < 0 then
<a name="line_210"></a>   T1 := L;
<a name="line_211"></a>  else
<a name="line_212"></a>   T1 := 1;
<a name="line_213"></a>  fi:
<a name="line_214"></a> fi:
<a name="line_215"></a>
<a name="line_216"></a> z0 := act_H1[T1](z0);
<a name="line_217"></a>
<a name="line_218"></a> if Re(z0) < Im(z0) then
<a name="line_219"></a>  T1 := G_mult(LN,T1);
<a name="line_220"></a>  z0 := act_H1[LN](z0);
<a name="line_221"></a> fi;
<a name="line_222"></a>
<a name="line_223"></a> m := c_H_p0[0];
<a name="line_224"></a> d := evalf((z0-m)*conjugate(z0-m) - m*conjugate(m) + 1);
<a name="line_225"></a> if d < 0 then
<a name="line_226"></a>  z0 := act_H1[MN](act_Pi1([6],z0));
<a name="line_227"></a>  T1,T0 := op(Pi_tilde_mult([MN,[6]],[T1,T0]));
<a name="line_228"></a> fi;
<a name="line_229"></a>
<a name="line_230"></a> return [T1,T0,z0];
<a name="line_231"></a>end:
<a name="line_232"></a>
<a name="line_233"></a># This is a continuous retraction of the unit disc onto HF16 which
<a name="line_234"></a># sends the complement of HF16 to the boundary of HF16 (and so
<a name="line_235"></a># has no interesting equivariance).
<a name="line_236"></a>
<a name="line_237"></a><span style="color:red">#@ squash_F16_H0
</span><a name="line_238"></a>squash_F16_H0 := proc(z)
<a name="line_239"></a> local w,i;
<a name="line_240"></a> w := z;
<a name="line_241"></a> for i in [3,6,11] do
<a name="line_242"></a>  w := corner_shift_H0[i](w);
<a name="line_243"></a>  w := max(0,Re(w)) + max(0,Im(w))*I;
<a name="line_244"></a>  w := corner_unshift_H0[i](w);
<a name="line_245"></a> od;
<a name="line_246"></a> if Im(w) <= 0 then
<a name="line_247"></a>  if Re(w) >= 0 then
<a name="line_248"></a>   w := Re(w);
<a name="line_249"></a>  else
<a name="line_250"></a>   w := 0;
<a name="line_251"></a>  fi;
<a name="line_252"></a> elif Im(w) >= Re(w) then
<a name="line_253"></a>  if Im(w) + Re(w) >= 0 then
<a name="line_254"></a>   w := (Re(w)+Im(w)) * (1+I)/2;
<a name="line_255"></a>  else
<a name="line_256"></a>   w := 0;
<a name="line_257"></a>  fi;
<a name="line_258"></a> fi;
<a name="line_259"></a>
<a name="line_260"></a> return w;
<a name="line_261"></a>end:
<a name="line_262"></a>
<a name="line_263"></a><span style="color:red">#@ square_diffeo_H0
</span><a name="line_264"></a>square_diffeo_H0 := (z) -> [1-square_diffeo_H_pa0(z),square_diffeo_H_pb0(z)];
<a name="line_265"></a>
<a name="line_266"></a><span style="color:red">#@ square_diffeo_H0_inverse
</span><a name="line_267"></a>square_diffeo_H0_inverse := proc(t0)
<a name="line_268"></a> local ca,sa,sb,pa,pb,aa,bb,zz,mm;
<a name="line_269"></a> sa := square_diffeo_H_ra0^(1-t0[1]);
<a name="line_270"></a> sb := square_diffeo_H_rb0^t0[2];
<a name="line_271"></a>
<a name="line_272"></a> if trim(sa) = 1 then
<a name="line_273"></a>  if trim(sb) = 1 then
<a name="line_274"></a>   return(0);
<a name="line_275"></a>  else
<a name="line_276"></a>   return evalf(
<a name="line_277"></a>    (1/sqrt(2)*a_H0*square_diffeo_H_rb0*sb^2-(sqrt(1+a_H0^2)-sqrt(1-a_H0^2))/2-
<a name="line_278"></a>     sqrt(((sqrt(1-a_H0^4)-1)*sb^4+(4-2*a_H0^2)*sb^2-(sqrt(1-a_H0^4)+1))/2))/(sb^2-1));
<a name="line_279"></a>  fi;
<a name="line_280"></a> fi;
<a name="line_281"></a>
<a name="line_282"></a> if trim(sb) = 1 then
<a name="line_283"></a>  ca := square_diffeo_H_ca0;
<a name="line_284"></a>  mm := evalf(sqrt(2)*(ca-1/ca)/I/(sa^2-1));
<a name="line_285"></a>  zz := evalf((1/ca+I*ca)/2 - (1+I)*sqrt(2)*mm/4 -
<a name="line_286"></a>        sqrt((1/ca^2-ca^2)/4-I*(2-mm^2)/4+((1-I)*ca-(1+I)/ca)*sqrt(2)*mm/4));
<a name="line_287"></a>  return zz;
<a name="line_288"></a> fi;
<a name="line_289"></a>
<a name="line_290"></a> pa := (conjugate(square_diffeo_H_ca0)*sa^2-  square_diffeo_H_ca0)/(sa^2-1);
<a name="line_291"></a> pb := (conjugate(square_diffeo_H_cb0)*sb^2-I*square_diffeo_H_cb0)/(sb^2-1);
<a name="line_292"></a> aa := Re(pa*conjugate(pa-pb))/abs(pa-pb)^2;
<a name="line_293"></a> bb := sqrt((abs(pa)^2-1)/abs(pa-pb)^2-aa^2);
<a name="line_294"></a> return (1-aa+I*bb)*pa+(aa-I*bb)*pb;
<a name="line_295"></a>end:
<a name="line_296"></a>
<a name="line_297"></a># set_a_H0(4/5);
<a name="line_298"></a>
<a name="line_299"></a># The value below is obtained as follows:
<a name="line_300"></a>#
<a name="line_301"></a># 1) Find an approximation to the function f such that exp(2f) times
<a name="line_302"></a>#    the euclidean metric on EX^* has curvature -1.
<a name="line_303"></a># 2) Find the lengths of the sides of F16 with respect to this
<a name="line_304"></a>#    rescaled metric.
<a name="line_305"></a># 3) Find the value of a_H that makes the sides of F16 in HX
<a name="line_306"></a>#    have the same lengths as in (2).  We get a different answer
<a name="line_307"></a>#    for each side, but the differences are around 10^(-5.5).
<a name="line_308"></a>#    The value below is the average.
<a name="line_309"></a>#
<a name="line_310"></a># I think that the error in the value below is probably < 10^(-9).
<a name="line_311"></a>
<a name="line_312"></a>set_a_H0(0.8005319048923638104265325104767778171017180985241883922169730841584541075292965796346466097572031440);  </pre>
 </body>
</html>
    