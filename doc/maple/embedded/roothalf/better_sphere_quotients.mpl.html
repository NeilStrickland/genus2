<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># Our primary approach to understanding the canonical map p : EX^* -> S^2 goes 
<a name="line_2"></a># via hyperbolic uniformisation using the E_to_S_map class.  The code in this 
<a name="line_3"></a># file represents an attempt to understand p, or maps similar to p, by more
<a name="line_4"></a># direct and elementary means.  The file sphere_quotients.mpl defines a map
<a name="line_5"></a># E_to_S2 : EX^* -> S^2 which has the expected equivariance and which induces
<a name="line_6"></a># a homeomorphism EX^*/<1,LL> -> S^2, but it fails quite badly to be conformal.
<a name="line_7"></a># Here we study maps which may be slightly better.
<a name="line_8"></a>
<a name="line_9"></a>######################################################################
<a name="line_10"></a>
<a name="line_11"></a># The following function checks whether a map S : EX^* -> R^3 is equivariant,
<a name="line_12"></a># sends v[0], v[3] and v[6] to the right places, and is tangent to S^2
<a name="line_13"></a># at those points.
<a name="line_14"></a>
<a name="line_15"></a><span style="color:red">#@ map_to_S2_basic_test 
</span><a name="line_16"></a>map_to_S2_basic_test := proc(S)
<a name="line_17"></a> local i,m,err;
<a name="line_18"></a>
<a name="line_19"></a> if evalf(simplify(S(act_E[L](x)) -~ act_S2[L](S(x)))) <> [0. $3] then return false; fi;
<a name="line_20"></a> if evalf(simplify(S(act_E[M](x)) -~ act_S2[M](S(x)))) <> [0. $3] then return false; fi;
<a name="line_21"></a> if evalf(simplify(S(act_E[N](x)) -~ act_S2[N](S(x)))) <> [0. $3] then return false; fi;
<a name="line_22"></a>
<a name="line_23"></a> if evalf(S(v_E0[3]) -~ v_S2[3]) <> [0. $3] then return false; fi;
<a name="line_24"></a> if evalf(S(v_E0[6]) -~ v_S2[6]) <> [0. $3] then return false; fi;
<a name="line_25"></a>
<a name="line_26"></a> for i in {0,3,6} do
<a name="line_27"></a>  if evalf(simplify(S(v_E0[i]) -~ v_S2[i])) <> [0. $3] then return false; fi;
<a name="line_28"></a>  m := `if`(i = 0,3,2);
<a name="line_29"></a>  err := evalf(simplify(convert(series(nm3(S(chart[0](e*s,e*t)))^2-1,e=0,m),polynom,e)));
<a name="line_30"></a>  if err <> 0. then return false; fi;
<a name="line_31"></a> od;
<a name="line_32"></a>
<a name="line_33"></a> true;
<a name="line_34"></a>end:
<a name="line_35"></a>
<a name="line_36"></a># The following functions check whether a map S behaves conformally to the
<a name="line_37"></a># lowest relevant order at v[0], v[3], v[6] or v[11].  Each function returns
<a name="line_38"></a># a list of terms which should be zero.
<a name="line_39"></a>
<a name="line_40"></a><span style="color:red">#@ v0_conformal_test 
</span><a name="line_41"></a>v0_conformal_test := proc(S)
<a name="line_42"></a> local aa,bb,cc,yy,zz,e,s,t;
<a name="line_43"></a> yy := map(series,S(chart[0](e*s,e*t)),e=0,3);
<a name="line_44"></a> yy := map(convert,yy,polynom,e);
<a name="line_45"></a> zz := S2_to_C(yy);
<a name="line_46"></a> zz := series(zz,e=0,3);
<a name="line_47"></a> zz := subs(e=1,convert(zz,polynom,e));
<a name="line_48"></a> aa := coeff(zz,s,2);
<a name="line_49"></a> bb := coeff(coeff(zz,s,1),t,1);
<a name="line_50"></a> cc := coeff(zz,t,2);
<a name="line_51"></a> expand([aa+cc,(bb-2*I*aa)/I]);
<a name="line_52"></a>end:
<a name="line_53"></a>
<a name="line_54"></a><span style="color:red">#@ v3_conformal_test 
</span><a name="line_55"></a>v3_conformal_test := proc(S)
<a name="line_56"></a> local aa,bb,yy,zz,e,s,t;
<a name="line_57"></a> yy := map(series,S(chart[3](e*s,e*t)),e=0,2);
<a name="line_58"></a> yy := map(convert,yy,polynom,e);
<a name="line_59"></a> zz := S2_to_C(yy);
<a name="line_60"></a> zz := series(zz,e=0,2);
<a name="line_61"></a> zz := subs(e=1,convert(zz,polynom,e));
<a name="line_62"></a> aa := coeff(zz,s,1);
<a name="line_63"></a> bb := coeff(zz,t,1);
<a name="line_64"></a> expand(bb - I*aa);
<a name="line_65"></a>end:
<a name="line_66"></a>
<a name="line_67"></a><span style="color:red">#@ v6_conformal_test 
</span><a name="line_68"></a>v6_conformal_test := proc(S)
<a name="line_69"></a> local aa,bb,yy,zz,e,s,t;
<a name="line_70"></a> yy := map(series,S(chart[6](e*s,e*t)),e=0,2);
<a name="line_71"></a> yy := map(convert,yy,polynom,e);
<a name="line_72"></a> zz := S2_to_C(yy);
<a name="line_73"></a> zz := series(zz,e=0,2);
<a name="line_74"></a> zz := subs(e=1,convert(zz,polynom,e));
<a name="line_75"></a> aa := coeff(zz,s,1);
<a name="line_76"></a> bb := coeff(zz,t,1);
<a name="line_77"></a> expand((bb - I*aa)*(1+I)/sqrt(2));
<a name="line_78"></a>end:
<a name="line_79"></a>
<a name="line_80"></a><span style="color:red">#@ v11_conformal_test 
</span><a name="line_81"></a>v11_conformal_test := proc(S)
<a name="line_82"></a> local aa,bb,cc,dd,yy,e,s,t;
<a name="line_83"></a> yy := map(series,S(chart[11](e*s,e*t)),e=0,2);
<a name="line_84"></a> yy := map(convert,yy,polynom,e);
<a name="line_85"></a> aa := subs(e=0,yy);
<a name="line_86"></a> if map(coeff,yy,e,1) <> [0$3] then return FAIL; fi;
<a name="line_87"></a> yy := subs(e = 1,yy -~ aa);
<a name="line_88"></a> bb := map(coeff,yy,s,2);
<a name="line_89"></a> cc := map(coeff,map(coeff,yy,s,1),t,1);
<a name="line_90"></a> dd := map(coeff,yy,t,2);
<a name="line_91"></a> expand([op(bb+~dd),dp3(aa,bb),dp3(aa,cc),dp3(bb,cc),dp3(aa,aa)-1]);
<a name="line_92"></a>end:
<a name="line_93"></a>
<a name="line_94"></a># One can show that the map must be as follows, for some undetermined
<a name="line_95"></a># functions Pi and Qj.
<a name="line_96"></a>
<a name="line_97"></a><span style="color:red">#@ s2p_core 
</span><a name="line_98"></a>s2p_core := [
<a name="line_99"></a>  (x[2]^2-x[1]^2-(3/sqrt(2))*x[3]*x[4]),
<a name="line_100"></a>  2*x[1]*x[2],
<a name="line_101"></a>  -x[3]
<a name="line_102"></a>]:
<a name="line_103"></a>
<a name="line_104"></a><span style="color:red">#@ s2p_multipliers 
</span><a name="line_105"></a>s2p_multipliers := [
<a name="line_106"></a> (1+z[1]*P1(z[1])+(z[2]-1/2)*Q1(z[1],z[2])),
<a name="line_107"></a> (1+z[1]*P2(z[1])+z[2]*Q2(z[1],z[2])),
<a name="line_108"></a> (3/2-z[1]/2+(z[1]-1)^2*P3(z[1])+z[2]*Q3(z[1],z[2]))
<a name="line_109"></a>]:
<a name="line_110"></a>
<a name="line_111"></a><span style="color:red">#@ s2p_pattern 
</span><a name="line_112"></a>s2p_pattern := s2p_core *~ s2p_multipliers;
<a name="line_113"></a>
<a name="line_114"></a><span style="color:red">#@ s2p_generic 
</span><a name="line_115"></a>s2p_generic := unapply(eval(subs(z=zx0,s2p_pattern)),x);
<a name="line_116"></a>
<a name="line_117"></a># Taking the functions Pi and Qj to be affine, we get the following pattern:
<a name="line_118"></a>
<a name="line_119"></a><span style="color:red">#@ s2p_poly_multipliers 
</span><a name="line_120"></a>s2p_poly_multipliers := subs({
<a name="line_121"></a> P1(z[1]) = C[0] + C[1]*z[1],
<a name="line_122"></a> P2(z[1]) = C[2] + C[3]*z[1],
<a name="line_123"></a> P3(z[1]) = C[4] + C[5]*z[1],
<a name="line_124"></a> Q1(z[1],z[2]) = C[6] + C[7]*z[1] + C[8]*z[2],
<a name="line_125"></a> Q2(z[1],z[2]) = C[9] + C[10]*z[1] + C[11]*z[2],
<a name="line_126"></a> Q3(z[1],z[2]) = C[12] + C[13]*z[1] + C[14]*z[2]
<a name="line_127"></a>},s2p_multipliers
<a name="line_128"></a>):
<a name="line_129"></a>
<a name="line_130"></a><span style="color:red">#@ s2p_poly_pattern 
</span><a name="line_131"></a>s2p_poly_pattern := s2p_core *~ s2p_poly_multipliers;
<a name="line_132"></a>
<a name="line_133"></a><span style="color:red">#@ s2p_poly 
</span><a name="line_134"></a>s2p_poly := unapply(eval(subs(z=zx0,s2p_poly_pattern)),x):
<a name="line_135"></a>
<a name="line_136"></a><span style="color:red">#@ s2p_conformal_poly_rels 
</span><a name="line_137"></a>s2p_conformal_poly_rels := {
<a name="line_138"></a> C[6]=1/2-C[4],
<a name="line_139"></a> C[7]=-C[2]-C[3]+1/2+2*C[0]+2*C[1]+C[4],
<a name="line_140"></a> C[14] = -(1/3*(2*sqrt(6)*C[4]+sqrt(6)*C[12]+3*sqrt(6)-6*C[9]-3*C[11]-12))*sqrt(6)
<a name="line_141"></a>};
<a name="line_142"></a>
<a name="line_143"></a><span style="color:red">#@ s2p_conformal_poly_multipliers 
</span><a name="line_144"></a>s2p_conformal_poly_multipliers := 
<a name="line_145"></a> simplify(expand(subs(s2p_conformal_poly_rels,s2p_poly_multipliers)));
<a name="line_146"></a>
<a name="line_147"></a><span style="color:red">#@ s2p_conformal_poly_pattern 
</span><a name="line_148"></a>s2p_conformal_poly_pattern := s2p_core *~ s2p_conformal_poly_multipliers;
<a name="line_149"></a>
<a name="line_150"></a><span style="color:red">#@ s2p_conformal_poly 
</span><a name="line_151"></a>s2p_conformal_poly := unapply(eval(subs(z=zx0,s2p_conformal_poly_pattern)),x):
<a name="line_152"></a>
<a name="line_153"></a># Here are some numerical choices for the coefficients C[i], which
<a name="line_154"></a># give a reasonable map.
<a name="line_155"></a>
<a name="line_156"></a><span style="color:red">#@ s2p_approx 
</span><a name="line_157"></a>s2p_approx := {
<a name="line_158"></a> C[0] = -1.6723,C[1] = 0.80475,C[2] = -1.3774,C[3] = 1.0570,C[4] = 0.60044,
<a name="line_159"></a> C[5] = -0.35384,C[8] = -0.49899,C[9] = -0.41948,C[10] = 0.28319,
<a name="line_160"></a> C[11] = -0.037759,C[12] = -0.53453,C[13] = 0.53090
<a name="line_161"></a>}:
<a name="line_162"></a>
<a name="line_163"></a><span style="color:red">#@ s2p0_multipliers 
</span><a name="line_164"></a>s2p0_multipliers := evalf(subs(s2p_approx,s2p_conformal_poly_multipliers));
<a name="line_165"></a>
<a name="line_166"></a><span style="color:red">#@ s2p0_pattern 
</span><a name="line_167"></a>s2p0_pattern := evalf(s2p_core *~ s2p0_multipliers);
<a name="line_168"></a>
<a name="line_169"></a><span style="color:red">#@ s2p0 
</span><a name="line_170"></a>s2p0 := unapply(eval(subs(z=zx0,s2p0_pattern)),x):
<a name="line_171"></a>
<a name="line_172"></a># The following code calculates the Dirichlet energy of a map EX^* -> S^2,
<a name="line_173"></a># as a map EX^* -> R.  This then needs to be integrated.
<a name="line_174"></a>
<a name="line_175"></a><span style="color:red">#@ find_E_to_S_energy_coeff
</span><a name="line_176"></a><span style="color:red">#@ E_to_S_energy_denom
</span><a name="line_177"></a><span style="color:red">#@ E_to_S_energy_coeff
</span><a name="line_178"></a><span style="color:red">#@ E_to_S_energy_coeff_string
</span><a name="line_179"></a>
<a name="line_180"></a>find_E_to_S_energy_coeff := proc()
<a name="line_181"></a> global E_to_S_energy_denom,E_to_S_energy_coeff,E_to_S_energy_coeff_string;
<a name="line_182"></a> local i,j,k,s,u_vars,U,UU,PP,nn,sndg,E0,E1,E2,En,En_alt,DP;
<a name="line_183"></a>
<a name="line_184"></a> UU := [seq(U[k](zx0[1],zx0[2]),k=1..3)];
<a name="line_185"></a> PP := UU *~ s2p_core;
<a name="line_186"></a> u_vars := [u[1],u[2],u[3],du[1,1],du[1,2],du[2,1],du[2,2],du[3,1],du[3,3]];
<a name="line_187"></a> nn := dg0(xx);
<a name="line_188"></a> sndg := square_norm_of_dg_z0(z);
<a name="line_189"></a>
<a name="line_190"></a> for k from 1 to 3 do 
<a name="line_191"></a>  for i from 1 to 4 do 
<a name="line_192"></a>   DP[k,i] := subs({U[1](x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = u[1],
<a name="line_193"></a>		    U[2](x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = u[2],
<a name="line_194"></a>		    U[3](x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = u[3],
<a name="line_195"></a>		    (D[1](U[1]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[1,1],
<a name="line_196"></a>		    (D[2](U[1]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[1,2],
<a name="line_197"></a>		    (D[1](U[2]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[2,1],
<a name="line_198"></a>		    (D[2](U[2]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[2,2],
<a name="line_199"></a>		    (D[1](U[3]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[3,1],
<a name="line_200"></a>		    (D[2](U[3]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[3,2]
<a name="line_201"></a>		   },diff(PP[k],x[i])); 
<a name="line_202"></a>  od;
<a name="line_203"></a> od;
<a name="line_204"></a>
<a name="line_205"></a> E0 := collect(NF_z0(add(add(DP[k,i]^2,i=1..4),k=1..3)),u_vars);
<a name="line_206"></a> E1 := collect(NF_z0(add(add(add(DP[k,i]*DP[k,j]*x[i]*x[j],j=1..4),i=1..4),k=1..3)),u_vars);
<a name="line_207"></a> E2 := collect(NF_z0(add(add(add(DP[k,i]*DP[k,j]*nn[i]*nn[j],j=1..4),i=1..4),k=1..3)),u_vars);
<a name="line_208"></a> En := collect(expand(sndg*(E0-E1) - E2),u_vars); 
<a name="line_209"></a>
<a name="line_210"></a> E_to_S_energy_denom := sndg;
<a name="line_211"></a> E_to_S_energy_coeff := table():
<a name="line_212"></a>
<a name="line_213"></a> for k from 1 to 3 do
<a name="line_214"></a>  E_to_S_energy_coeff[k] := factor(coeff(En,u[k],2));
<a name="line_215"></a>  for i from 1 to 2 do
<a name="line_216"></a>   E_to_S_energy_coeff[k,i] := factor(coeff(coeff(En,u[k],1),du[k,i],1));
<a name="line_217"></a>   for j from 1 to 2 do
<a name="line_218"></a>    if i <> j then 
<a name="line_219"></a>     E_to_S_energy_coeff[k,i,j] := factor(coeff(coeff(En,du[k,i],1),du[k,j],1));
<a name="line_220"></a>    else
<a name="line_221"></a>     E_to_S_energy_coeff[k,i,j] := factor(coeff(En,du[k,i],2));
<a name="line_222"></a>    fi;
<a name="line_223"></a>   od;
<a name="line_224"></a>  od;
<a name="line_225"></a> od:
<a name="line_226"></a>
<a name="line_227"></a> En_alt := 
<a name="line_228"></a>  add(E_to_S_energy_coeff[k] * u[k]^2,k=1..3) + 
<a name="line_229"></a>  add(add(E_to_S_energy_coeff[k,i] * u[k] * du[k,i],i=1..2),k=1..3) +
<a name="line_230"></a>  add(add(E_to_S_energy_coeff[k,i,i] * du[k,i]^2,i=1..2),k=1..3) +
<a name="line_231"></a>  add(E_to_S_energy_coeff[k,1,2] * du[k,1] * du[k,2],k=1..3);
<a name="line_232"></a>
<a name="line_233"></a> print(["check",expand(En - En_alt)]);
<a name="line_234"></a>
<a name="line_235"></a> s := sprintf("E_to_S_energy_denom := %A;\n",sndg):
<a name="line_236"></a> for k from 1 to 3 do 
<a name="line_237"></a>  s := cat(s,sprintf("E_to_S_energy_coeff[%d]     := %A;\n",k,E_to_S_energy_coeff[k]));
<a name="line_238"></a> od:
<a name="line_239"></a> for k from 1 to 3 do 
<a name="line_240"></a>  for i from 1 to 2 do
<a name="line_241"></a>   s := cat(s,sprintf("E_to_S_energy_coeff[%d,%d]   := %A;\n",k,i,E_to_S_energy_coeff[k,i]));
<a name="line_242"></a>  od;
<a name="line_243"></a> od:
<a name="line_244"></a> for k from 1 to 3 do 
<a name="line_245"></a>  for i from 1 to 2 do
<a name="line_246"></a>   for j from i to 2 do
<a name="line_247"></a>    s := cat(s,sprintf("E_to_S_energy_coeff[%d,%d,%d] := %A;\n",k,i,j,E_to_S_energy_coeff[k,i,j]));
<a name="line_248"></a>   od;
<a name="line_249"></a>  od;
<a name="line_250"></a> od:
<a name="line_251"></a> E_to_S_energy_coeff_string := s;
<a name="line_252"></a> NULL;
<a name="line_253"></a>
<a name="line_254"></a>end:
<a name="line_255"></a>
<a name="line_256"></a>E_to_S_energy_denom := (1+z[2])*(z[1]^2-4*z[1]+4);
<a name="line_257"></a>E_to_S_energy_coeff[1]     := -8*(2*z[2]-1)*(z[1]*z[2]-2*z[1]+2*z[2]+2);
<a name="line_258"></a>E_to_S_energy_coeff[2]     := -4*z[1]^4*z[2]^3+6*z[1]^4*z[2]^2-48*z[1]^3*z[2]^3+6*z[1]^4*z[2]+116*z[1]^3*z[2]^2-16*z[1]^2*z[2]^3-4*z[1]^4-104*z[1]^3*z[2]-144*z[1]^2*z[2]^2+20*z[1]^3+224*z[1]^2*z[2]+16*z[1]*z[2]^2-32*z[1]^2-160*z[1]*z[2]+32*z[2]^2+16*z[1]+32*z[2];
<a name="line_259"></a>E_to_S_energy_coeff[3]     := -(z[1]-2)^2*(z[1]*z[2]+z[1]-1);
<a name="line_260"></a>E_to_S_energy_coeff[1,1]   := -16*z[1]*z[2]*(2*z[2]-1)*(z[1]-2);
<a name="line_261"></a>E_to_S_energy_coeff[1,2]   := -32*z[2]*(2*z[2]-1)*(z[1]*z[2]-2*z[1]+2*z[2]+2);
<a name="line_262"></a>E_to_S_energy_coeff[2,1]   := -4*z[1]*(2*z[2]-1)*(z[1]-2)*(z[1]*z[2]+z[1]+2*z[2]-2)*(z[1]^2*z[2]-2*z[1]^2+4*z[1]-2);
<a name="line_263"></a>E_to_S_energy_coeff[2,2]   := -16*z[2]*(2*z[2]-1)*(3*z[1]*z[2]-3*z[1]+2*z[2]+2)*(z[1]^2*z[2]-2*z[1]^2+4*z[1]-2);
<a name="line_264"></a>E_to_S_energy_coeff[3,1]   := -4*z[1]*(z[1]-2)^2*(z[1]*z[2]+z[1]-1);
<a name="line_265"></a>E_to_S_energy_coeff[3,2]   := -8*z[1]*z[2]*(2*z[2]-1)*(z[1]-2);
<a name="line_266"></a>E_to_S_energy_coeff[1,1,1] := -8*z[1]*z[2]*(z[1]-2)^2*(z[1]*z[2]+z[1]-1);
<a name="line_267"></a>E_to_S_energy_coeff[1,1,2] := -32*z[1]*z[2]^2*(2*z[2]-1)*(z[1]-2);
<a name="line_268"></a>E_to_S_energy_coeff[1,2,2] := -32*z[2]^2*(2*z[2]-1)*(z[1]*z[2]-2*z[1]+2*z[2]+2);
<a name="line_269"></a>E_to_S_energy_coeff[2,1,1] := -2*z[1]*(2*z[2]-1)*(z[1]-2)^2*(z[1]*z[2]+z[1]-1)*(z[1]^2*z[2]-2*z[1]^2+4*z[1]-2);
<a name="line_270"></a>E_to_S_energy_coeff[2,1,2] := -8*z[1]*z[2]*(2*z[2]-1)^2*(z[1]-2)*(z[1]^2*z[2]-2*z[1]^2+4*z[1]-2);
<a name="line_271"></a>E_to_S_energy_coeff[2,2,2] := -8*z[2]*(2*z[2]-1)^2*(z[1]*z[2]-2*z[1]+2*z[2]+2)*(z[1]^2*z[2]-2*z[1]^2+4*z[1]-2);
<a name="line_272"></a>E_to_S_energy_coeff[3,1,1] := -4*z[1]^2*(z[1]-2)^2*(z[1]*z[2]+z[1]-1);
<a name="line_273"></a>E_to_S_energy_coeff[3,1,2] := -16*z[1]^2*z[2]*(2*z[2]-1)*(z[1]-2);
<a name="line_274"></a>E_to_S_energy_coeff[3,2,2] := -16*z[1]*z[2]*(2*z[2]-1)*(z[1]*z[2]-2*z[1]+2*z[2]+2);
<a name="line_275"></a>
<a name="line_276"></a># For the function below, u should be a list of three polynomials in z[1] and z[2].
<a name="line_277"></a># This corresponds to the map u *~ s2p_core.
<a name="line_278"></a>
<a name="line_279"></a><span style="color:red">#@ E_to_S_energy 
</span><a name="line_280"></a>E_to_S_energy := proc(u)
<a name="line_281"></a> local i,k,du,En,E;
<a name="line_282"></a>
<a name="line_283"></a> for k from 1 to 3 do
<a name="line_284"></a>  for i from 1 to 2 do 
<a name="line_285"></a>   du[k,i] := diff(u[k],z[i]);
<a name="line_286"></a>  od;
<a name="line_287"></a> od;
<a name="line_288"></a>
<a name="line_289"></a> En := 
<a name="line_290"></a>  add(E_to_S_energy_coeff[k] * u[k]^2,k=1..3) + 
<a name="line_291"></a>  add(add(E_to_S_energy_coeff[k,i] * u[k] * du[k,i],i=1..2),k=1..3) +
<a name="line_292"></a>  add(add(E_to_S_energy_coeff[k,i,i] * du[k,i]^2,i=1..2),k=1..3) +
<a name="line_293"></a>  add(E_to_S_energy_coeff[k,1,2] * du[k,1] * du[k,2],k=1..3);
<a name="line_294"></a>
<a name="line_295"></a> E := En / E_to_S_energy_denom;
<a name="line_296"></a>
<a name="line_297"></a> return unapply(E,z);
<a name="line_298"></a>end;
<a name="line_299"></a>
<a name="line_300"></a>
<a name="line_301"></a>######################################################################
<a name="line_302"></a>
<a name="line_303"></a><span style="color:red">#@ find_E_to_S_jacobian_coeff 
</span><a name="line_304"></a><span style="color:red">#@ E_to_S_jacobian_coeff
</span><a name="line_305"></a><span style="color:red">#@ E_to_S_jacobian_coeff_string
</span><a name="line_306"></a>
<a name="line_307"></a>find_E_to_S_jacobian_coeff := proc()
<a name="line_308"></a> global E_to_S_jacobian_coeff,E_to_S_jacobian_coeff_string;
<a name="line_309"></a> local i,j,k,l,s,U,UU,PP,DP,S3,S4,sg3,sg4,A,B,AB;
<a name="line_310"></a>
<a name="line_311"></a> UU := [seq(U[k](zx0[1],zx0[2]),k=1..3)];
<a name="line_312"></a> PP := UU *~ s2p_core;
<a name="line_313"></a>
<a name="line_314"></a> for k from 1 to 3 do 
<a name="line_315"></a>  for i from 1 to 4 do 
<a name="line_316"></a>   DP[k,i] := subs({U[1](x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = u[1],
<a name="line_317"></a>		    U[2](x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = u[2],
<a name="line_318"></a>		    U[3](x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = u[3],
<a name="line_319"></a>		    (D[1](U[1]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[1,1],
<a name="line_320"></a>		    (D[2](U[1]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[1,2],
<a name="line_321"></a>		    (D[1](U[2]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[2,1],
<a name="line_322"></a>		    (D[2](U[2]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[2,2],
<a name="line_323"></a>		    (D[1](U[3]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[3,1],
<a name="line_324"></a>		    (D[2](U[3]))(x[3]^2, (1/8)*(3*sqrt(2)*x[3]*x[4]+2*x[1]^2-2*x[2]^2)^2) = du[3,2]
<a name="line_325"></a>		   },diff(PP[k],x[i])); 
<a name="line_326"></a>  od;
<a name="line_327"></a> od;
<a name="line_328"></a>
<a name="line_329"></a> PP := [seq(u[i] * s2p_core[i],i=1..3)];
<a name="line_330"></a>
<a name="line_331"></a> S3 := combinat[permute](3):
<a name="line_332"></a> S4 := combinat[permute](4):
<a name="line_333"></a> sg3 := (u) -> signum(mul(mul(u[j]-u[i],j=i+1..3),i=1..2)):
<a name="line_334"></a> sg4 := (u) -> signum(mul(mul(u[j]-u[i],j=i+1..4),i=1..3)):
<a name="line_335"></a>
<a name="line_336"></a> A := Matrix(4,4):
<a name="line_337"></a> for i from 1 to 4 do 
<a name="line_338"></a>  for j from 1 to 4 do
<a name="line_339"></a>   A[i,j] := Determinant(Matrix([[seq(PP[k],k=1..3)],[seq(DP[k,i],k=1..3)],[seq(DP[k,j],k=1..3)]]));
<a name="line_340"></a>  od:
<a name="line_341"></a> od:
<a name="line_342"></a>
<a name="line_343"></a> B := Matrix(4,4):
<a name="line_344"></a> for s in S4 do
<a name="line_345"></a>  B[s[1],s[2]] := B[s[1],s[2]] + sg4(s) * dg0(xx)[s[3]] * x[s[4]];
<a name="line_346"></a> od:
<a name="line_347"></a>
<a name="line_348"></a> AB := expand(NF_z0(add(add(A[i,j] * B[i,j]/2,j=1..4),i=1..4))):
<a name="line_349"></a>
<a name="line_350"></a> E_to_S_jacobian_coeff := table():
<a name="line_351"></a>
<a name="line_352"></a> E_to_S_jacobian_coeff[0] := factor(coeff(coeff(coeff(AB,u[1],1),u[2],1),u[3],1));
<a name="line_353"></a> E_to_S_jacobian_coeff[1] := factor(coeff(coeff(coeff(AB,u[1],1),du[2,1],1),du[3,2],1)/2);
<a name="line_354"></a>
<a name="line_355"></a> for i from 1 to 3 do
<a name="line_356"></a>  j,k := op({1,2,3} minus {i});
<a name="line_357"></a>  for l from 1 to 2 do
<a name="line_358"></a>   E_to_S_jacobian_coeff[i,l] :=
<a name="line_359"></a>    factor(coeff(coeff(coeff(AB,u[j],1),u[k],1),du[i,l],1)/2);
<a name="line_360"></a>  od;
<a name="line_361"></a> od;
<a name="line_362"></a>end:
<a name="line_363"></a>
<a name="line_364"></a>E_to_S_jacobian_coeff[0]   :=  (-12*z[1]^2*z[2]^2+20*z[1]^2*z[2]-4*z[1]^2-16*z[1]*z[2]+4);
<a name="line_365"></a>E_to_S_jacobian_coeff[1]   :=  4*z[1]*z[2]*(2*z[2]-1)*(z[1]^2*z[2]-2*z[1]^2+4*z[1]-2);
<a name="line_366"></a>E_to_S_jacobian_coeff[1,1] :=  2*z[1]*z[2]*(4*z[1]^2*z[2]-5*z[1]^2+8*z[1]-4);
<a name="line_367"></a>E_to_S_jacobian_coeff[1,2] := -2*z[2]*(2*z[2]-1)*(z[1]^2*z[2]-2*z[1]^2+2);
<a name="line_368"></a>E_to_S_jacobian_coeff[2,1] := -2*z[1]*(2*z[2]-1)*(z[1]^2*z[2]-2*z[1]^2+4*z[1]-2);
<a name="line_369"></a>E_to_S_jacobian_coeff[2,2] := -2*z[2]*(2*z[2]-1)*(z[1]^2*z[2]-2*z[1]^2+4*z[1]-2);
<a name="line_370"></a>E_to_S_jacobian_coeff[3,1] := -4*z[1]*(z[1]*z[2]-z[1]+1)*(z[1]*z[2]+z[1]-1);
<a name="line_371"></a>E_to_S_jacobian_coeff[3,2] :=  4*z[1]*z[2]*(2*z[2]-1)*(z[1]*z[2]-2*z[1]+2);
<a name="line_372"></a>
<a name="line_373"></a><span style="color:red">#@ E_to_S_jacobian 
</span><a name="line_374"></a>E_to_S_jacobian := proc(u)
<a name="line_375"></a> local i,k,du,s,S3,sg3,J;
<a name="line_376"></a>
<a name="line_377"></a> S3 := combinat[permute](3):
<a name="line_378"></a> sg3 := (u) -> signum(mul(mul(u[j]-u[i],j=i+1..3),i=1..2)):
<a name="line_379"></a>
<a name="line_380"></a> for k from 1 to 3 do
<a name="line_381"></a>  for i from 1 to 2 do 
<a name="line_382"></a>   du[k,i] := diff(u[k],z[i]);
<a name="line_383"></a>  od;
<a name="line_384"></a> od;
<a name="line_385"></a>
<a name="line_386"></a> J := 
<a name="line_387"></a>  E_to_S_jacobian_coeff[0] * u[1]*u[2]*u[3] +
<a name="line_388"></a>  E_to_S_jacobian_coeff[1] *
<a name="line_389"></a>    add(sg3(s)*u[s[1]]*(du[s[2],1]*du[s[3],2]-du[s[2],2]*du[s[3],1]),s in S3) +
<a name="line_390"></a>  (add(add(u[s[1]] * u[s[2]] * du[s[3],k] * 
<a name="line_391"></a>             E_to_S_jacobian_coeff[s[3],k],k=1..2),s in S3));
<a name="line_392"></a>
<a name="line_393"></a> J := J / sqrt(square_norm_of_dg_z0(z));
<a name="line_394"></a> return unapply(J,z)
<a name="line_395"></a>end:
<a name="line_396"></a>
<a name="line_397"></a><span style="color:red">#@ E_to_S_conformal_error 
</span><a name="line_398"></a>E_to_S_conformal_error := proc(u)
<a name="line_399"></a> local E,J;
<a name="line_400"></a> E := E_to_S_energy(u)(z);
<a name="line_401"></a> J := E_to_S_jacobian(u)(z);
<a name="line_402"></a> return unapply(E - 2*J,z);
<a name="line_403"></a>end:
<a name="line_404"></a>
<a name="line_405"></a><span style="color:red">#@ s2p_plot 
</span><a name="line_406"></a>s2p_plot := proc(S,g)
<a name="line_407"></a> display(seq(polygon(evalf(map(s2p0,map(p -> p["x"],g["faces"][i]["corner"])))),i=0..g["num_faces"]-1),
<a name="line_408"></a>         scaling=constrained,axes=none);
<a name="line_409"></a>end:
<a name="line_410"></a>
<a name="line_411"></a># Let u be a positive smooth function of z[1] with u(1) = 1.  Then the following
<a name="line_412"></a># multipliers give a map that is normalised and conformal on a first-order 
<a name="line_413"></a># infinitesimal neighbourhood of C_1, with the correct behavious at the endpoints.
<a name="line_414"></a>
<a name="line_415"></a><span style="color:red">#@ p1_multipliers 
</span><a name="line_416"></a>p1_multipliers := 
<a name="line_417"></a> [-(1-(1/2)*z[1])*(2*z[1]*(D(u))(z[1])-u(z[1]))/(u(z[1])^2+z[1]),
<a name="line_418"></a>  (u(z[1])^2-z[1])/((u(z[1])^2+z[1])*(1-z[1])),
<a name="line_419"></a>  2*u(z[1])/(u(z[1])^2+z[1])
<a name="line_420"></a> ]:
<a name="line_421"></a>
<a name="line_422"></a><span style="color:red">#@ s2p_p1 
</span><a name="line_423"></a>s2p_p1 := unapply(subs({z[1]=zx0[1],z[2]=zx0[2]},p1_multipliers *~ s2p_core),x);
<a name="line_424"></a>
<a name="line_425"></a># The following multipliers do the same for C_3 and C_5. 
<a name="line_426"></a>
<a name="line_427"></a><span style="color:red">#@ p3_multipliers 
</span><a name="line_428"></a>p3_multipliers := 
<a name="line_429"></a> [(u(z[1])^2-z[1])/(u(z[1])^2+z[1]),
<a name="line_430"></a>  -(1/3)*(2*z[1]*(D(u))(z[1])-u(z[1]))*sqrt(6)/(u(z[1])^2+z[1]),
<a name="line_431"></a>  2*u(z[1])/(u(z[1])^2+z[1])
<a name="line_432"></a> ]:
<a name="line_433"></a>
<a name="line_434"></a><span style="color:red">#@ s2p_p3 
</span><a name="line_435"></a>s2p_p3 := unapply(subs({z[1]=zx0[1],z[2]=zx0[2]},p3_multipliers *~ s2p_core),x);
<a name="line_436"></a>
<a name="line_437"></a><span style="color:red">#@ p5_multipliers 
</span><a name="line_438"></a>p5_multipliers := 
<a name="line_439"></a> [(1/2)*(u(z[1])^2-z[1])*z[1]/((u(z[1])^2+z[1])*(1-z[1])),
<a name="line_440"></a>  z[1]*(-2*(D(u)(z[1]))*z[1]+u(z[1]))/(sqrt(3*z[1]^2-4*z[1]+2)*(u(z[1])^2+z[1])),
<a name="line_441"></a>  2*u(z[1])/(u(z[1])^2+z[1])
<a name="line_442"></a> ]:
<a name="line_443"></a>
<a name="line_444"></a><span style="color:red">#@ s2p_p5 
</span><a name="line_445"></a>s2p_p5 := unapply(subs({z[1]=zx0[1],z[2]=zx0[2]},p5_multipliers *~ s2p_core),x);
<a name="line_446"></a>
<a name="line_447"></a># For C_0 we have not found a general system of multiplers depending on a function
<a name="line_448"></a># u.  We just have the following choice, with no free parameters.
<a name="line_449"></a>
<a name="line_450"></a><span style="color:red">#@ p0_multipliers 
</span><a name="line_451"></a>p0_multipliers := [1,1,2*sqrt(1+z[2])];
<a name="line_452"></a>
<a name="line_453"></a><span style="color:red">#@ s2p_p0 
</span><a name="line_454"></a>s2p_p0 := unapply(subs({z[1]=zx0[1],z[2]=zx0[2]},p0_multipliers *~ s2p_core),x);
<a name="line_455"></a>
<a name="line_456"></a>######################################################################
<a name="line_457"></a># Here we make some specific choices of multipliers along the edges.
<a name="line_458"></a># With these choices we get maps f[i] : C_i -> S^2 that are conformal to
<a name="line_459"></a># first order and agree to first order at the corners.  At v[0] and v[11]
<a name="line_460"></a># the maps f[1] and f[5] are conformal to second order and agree to
<a name="line_461"></a># second order.
<a name="line_462"></a>#
<a name="line_463"></a># It should be possible in principle to find multipliers on the
<a name="line_464"></a># whole surface that agree on the curves C_i with the multipliers 
<a name="line_465"></a># below, but we have not yet completed this.  Note that p[0], p[1] and
<a name="line_466"></a># p[3] are the same, which is a start.
<a name="line_467"></a>#
<a name="line_468"></a># With these multipliers, the point v[11] maps in C to 
<a name="line_469"></a># 5 - 2*sqrt(6) = 0.101018, which is close to the value of a_P
<a name="line_470"></a># obtained by the numerical code.
<a name="line_471"></a>
<a name="line_472"></a><span style="color:red">#@ s2p_pp
</span><a name="line_473"></a><span style="color:red">#@ s2p_qq
</span><a name="line_474"></a> 
<a name="line_475"></a>s2p_qq := map(u -> NF_z0(u^2),s2p_core);
<a name="line_476"></a>s2p_pp[0] := [ 1-1/2*z[1]-3/4*z[1]*z[2]+1/2*z[1]*z[2]^2,
<a name="line_477"></a>               1/sqrt(1+3/2*z[1]*z[2]),
<a name="line_478"></a>               2*sqrt(1+z[2])*sqrt(1+3/2*z[1]*z[2])
<a name="line_479"></a>             ]/~(1+z[1]+5/2*z[1]*z[2]);
<a name="line_480"></a>s2p_pp[1] := s2p_pp[0];
<a name="line_481"></a>s2p_pp[3] := s2p_pp[0];
<a name="line_482"></a>
<a name="line_483"></a>s2p_pp[5] := [(5/2-(3/4)*z[1]^2*z[2]+(3/2)*z[1]^2-3*z[1])/(2*(1+z[1]+(5/2)*z[1]*z[2])),
<a name="line_484"></a>              (6-4*z[1]- 2*z[2]+2*z[1]*z[2])/((1+z[1]+(5/2)*z[1]*z[2])*(1+z[2])*sqrt(6*z[1]*z[2]+4)),
<a name="line_485"></a>              2*sqrt(1+z[2])*sqrt(1+3/2*z[1]*z[2])/(1+z[1]+5/2*z[1]*z[2])];
<a name="line_486"></a>
<a name="line_487"></a>## s2p_ff
<a name="line_488"></a>
<a name="line_489"></a>for i in {0,1,3,5} do
<a name="line_490"></a> s2p_ff[i] := unapply(eval(subs(z = zx0,s2p_pp[i])) *~ s2p_core,x);
<a name="line_491"></a>od:
<a name="line_492"></a>
<a name="line_493"></a>######################################################################
<a name="line_494"></a>
<a name="line_495"></a># Suppose we have a map f with multipliers m1,m2,m3.  Then m3 does not
<a name="line_496"></a># affect the values of f on C_0, but there is a unique possible value
<a name="line_497"></a># of m3 on C_0 that ensures that f is conformal on a first order 
<a name="line_498"></a># infinitesimal neighbourhood of C_0.  This m3 is given by 
<a name="line_499"></a># p0_conformal_factor([m1,m2,m3]), with the understanding that the
<a name="line_500"></a># functions mi have been restricted to C_0 and expressed as functions
<a name="line_501"></a># of z[1].  The story is similar for C_1, C_3 and C_5.  Note however
<a name="line_502"></a># that z[2] is not constant on C_5, so taking the derivative of the
<a name="line_503"></a># restricted function with respect to z[1] is not the same as taking
<a name="line_504"></a># the partial derivative and then restricting.
<a name="line_505"></a>
<a name="line_506"></a><span style="color:red">#@ p0_conformal_factor 
</span><a name="line_507"></a>p0_conformal_factor := (m) -> 
<a name="line_508"></a> sqrt(1+z[2])*(2*m[1](z[2])*m[2](z[2])+
<a name="line_509"></a>               8*z[2]*(1/2-z[2])*(D(m[1])(z[2])*m[2](z[2])-m[1](z[2])*D(m[2])(z[2])));
<a name="line_510"></a>
<a name="line_511"></a><span style="color:red">#@ p1_conformal_factor 
</span><a name="line_512"></a>p1_conformal_factor := (m) -> 
<a name="line_513"></a> 1/4*(2-z[1])*(-(1+z[1])*m[2](z[1])*m[3](z[1]) +
<a name="line_514"></a>   2*z[1]*(1-z[1])*(D(m[2])(z[1])*m[3](z[1])-D(m[3])(z[1])*m[2](z[1])));
<a name="line_515"></a>
<a name="line_516"></a><span style="color:red">#@ p3_conformal_factor 
</span><a name="line_517"></a>p3_conformal_factor := (m) -> 
<a name="line_518"></a> (2*z[1]*(D(m[1])(z[1])*m[3](z[1])-D(m[3])(z[1])*m[1](z[1]))-m[1](z[1])*m[3](z[1]))/sqrt(6);
<a name="line_519"></a>
<a name="line_520"></a><span style="color:red">#@ p5_conformal_factor 
</span><a name="line_521"></a>p5_conformal_factor := (m) -> 
<a name="line_522"></a> (-2*z[1]*(1-z[1])*(m[3](z[1])*D(m[1])(z[1])-D(m[3])(z[1])*m[1](z[1])) + (3-z[1])*m[1](z[1])*m[3](z[1]))/sqrt(3*z[1]^2-4*z[1]+2);
<a name="line_523"></a>
  </pre>
 </body>
</html>
    