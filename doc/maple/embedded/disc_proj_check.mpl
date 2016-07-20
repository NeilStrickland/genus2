<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_disc_delta := proc()
<a name="line_2"></a> local i,T,sols,z,zz,err;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> _ASSERT(disc_delta_proj(v_E[0]) -~ [0, 1] = [0,0],"disc_delta_proj(v[0])");
<a name="line_7"></a> _ASSERT(disc_delta_proj(v_E[1]) -~ [0,-1] = [0,0],"disc_delta_proj(v[1])");
<a name="line_8"></a> _ASSERT(disc_delta_proj(v_E[2]) -~ [ 1/sqrt(2),0] = [0,0],"disc_delta_proj(v[2])");
<a name="line_9"></a> _ASSERT(disc_delta_proj(v_E[3]) -~ [-1/sqrt(2),0] = [0,0],"disc_delta_proj(v[3])");
<a name="line_10"></a> _ASSERT(disc_delta_proj(v_E[4]) -~ [-1/sqrt(2),0] = [0,0],"disc_delta_proj(v[4])");
<a name="line_11"></a> _ASSERT(disc_delta_proj(v_E[5]) -~ [ 1/sqrt(2),0] = [0,0],"disc_delta_proj(v[5])");
<a name="line_12"></a> _ASSERT(disc_delta_proj(v_E[6]) -~ [ 0,0] = [0,0],"disc_delta_proj(v[6])");
<a name="line_13"></a> _ASSERT(disc_delta_proj(v_E[7]) -~ [-1,0] = [0,0],"disc_delta_proj(v[7])");
<a name="line_14"></a> _ASSERT(disc_delta_proj(v_E[8]) -~ [ 0,0] = [0,0],"disc_delta_proj(v[8])");
<a name="line_15"></a> _ASSERT(disc_delta_proj(v_E[9]) -~ [ 1,0] = [0,0],"disc_delta_proj(v[9])");
<a name="line_16"></a>
<a name="line_17"></a> for i in {10,11,12,13,14,16,19,21} do
<a name="line_18"></a>  _ASSERT(
<a name="line_19"></a>   simplify(map(abs,disc_delta_proj(v_E[i])) - [0, sqrt(2)*a_E/sqrt(a_E^2+1)]) = [0,0],
<a name="line_20"></a>   sprintf("disc_delta_proj(v[%d])",i)
<a name="line_21"></a>  );
<a name="line_22"></a> od:
<a name="line_23"></a>
<a name="line_24"></a> for i in {15,17,18,20} do
<a name="line_25"></a>  _ASSERT(
<a name="line_26"></a>   simplify(map(abs,disc_delta_proj(v_E[i])) - [sqrt((1-a_E^2)/(a_E^2+1)), sqrt(2)*a_E/sqrt(a_E^2+1)]) = [0,0],
<a name="line_27"></a>   sprintf("disc_delta_proj(v[%d])",i)
<a name="line_28"></a>  );
<a name="line_29"></a> od:
<a name="line_30"></a>
<a name="line_31"></a> _ASSERT(
<a name="line_32"></a>  simplify(expand(disc_delta_proj(c_E[0](t)) - [cos(t+Pi/4),0])) = [0,0],
<a name="line_33"></a>  "disc_delta_proj(c[0](t))"
<a name="line_34"></a> );
<a name="line_35"></a>
<a name="line_36"></a> _ASSERT(
<a name="line_37"></a>  simplify(expand(disc_delta_proj(c_E[1](t)) - [0,cos(t)])) = [0,0],
<a name="line_38"></a>  "disc_delta_proj(c[1](t))"
<a name="line_39"></a> );
<a name="line_40"></a>
<a name="line_41"></a> _ASSERT(
<a name="line_42"></a>  simplify(expand(disc_delta_proj(c_E[2](t)) - [-sin(t),cos(t)])) = [0,0],
<a name="line_43"></a>  "disc_delta_proj(c[2](t))"
<a name="line_44"></a> );
<a name="line_45"></a>
<a name="line_46"></a> _ASSERT(
<a name="line_47"></a>  simplify(expand(disc_delta_proj(c_E[ 9](t)) - [0, sqrt(2)*a_E/sqrt(a_E^2+1)])) = [0,0],
<a name="line_48"></a>  "disc_delta_proj(c[ 9](t))"
<a name="line_49"></a> );
<a name="line_50"></a>
<a name="line_51"></a> _ASSERT(
<a name="line_52"></a>  simplify(expand(disc_delta_proj(c_E[12](t)) - [0,-sqrt(2)*a_E/sqrt(a_E^2+1)])) = [0,0],
<a name="line_53"></a>  "disc_delta_proj(c[12](t))"
<a name="line_54"></a> );
<a name="line_55"></a>
<a name="line_56"></a> _ASSERT(
<a name="line_57"></a>  simplify(expand(disc_delta_proj(c_E[10](t)) - [-sqrt(-a_E^4+1)*sin(t)/(a_E^2+1), sqrt(2)*a_E/sqrt(a_E^2+1)])) = [0,0],
<a name="line_58"></a>  "disc_delta_proj(c[10](t))"
<a name="line_59"></a> );
<a name="line_60"></a>
<a name="line_61"></a> _ASSERT(
<a name="line_62"></a>  simplify(expand(disc_delta_proj(c_E[11](t)) - [sqrt(-a_E^4+1)*sin(t)/(a_E^2+1), -sqrt(2)*a_E/sqrt(a_E^2+1)])) = [0,0],
<a name="line_63"></a>  "disc_delta_proj(c[11](t))"
<a name="line_64"></a> );
<a name="line_65"></a>
<a name="line_66"></a> _ASSERT(
<a name="line_67"></a>  simplify(expand(disc_delta_proj(c_cayley[1](t)) - 
<a name="line_68"></a>     [(1/3)*sqrt(6)*sqrt((a_E^2+1)/(2*a_E^2+1))*cos(t-arctan((1/3)*sqrt(3)*sqrt((-2*a_E^2+1)/(2*a_E^2+1)))),
<a name="line_69"></a>      -(2/3)*a_E*sqrt(6)*sin(t)/sqrt(2*a_E^2+1)])) = [0,0],
<a name="line_70"></a>  "disc_delta_proj(c_cayley[1](t))"
<a name="line_71"></a> );
<a name="line_72"></a>
<a name="line_73"></a> _ASSERT(
<a name="line_74"></a>  simplify(expand(disc_delta_proj(c_cayley[4](Pi-t)) - 
<a name="line_75"></a>     [(1/3)*sqrt(6)*sqrt((a_E^2+1)/(2*a_E^2+1))*cos(t-arctan((1/3)*sqrt(3)*sqrt((-2*a_E^2+1)/(2*a_E^2+1)))),
<a name="line_76"></a>      -(2/3)*a_E*sqrt(6)*sin(t)/sqrt(2*a_E^2+1)])) = [0,0],
<a name="line_77"></a>  "disc_delta_proj(c_cayley[4](t))"
<a name="line_78"></a> );
<a name="line_79"></a>
<a name="line_80"></a> zz := disc_delta_proj(c_cayley[1](t));
<a name="line_81"></a> err := simplify(expand(2*zz[1]^2+sqrt(1-4*a_E^4)/(sqrt(1+2*a_E^2)*a_E)*zz[1]*zz[2]+(1+1/a_E^2)/2*zz[2]^2-1));
<a name="line_82"></a>
<a name="line_83"></a> _ASSERT(err = 0,"disc_delta_proj(c_cayley[1]) ellipse");
<a name="line_84"></a>
<a name="line_85"></a> unassign('zz');
<a name="line_86"></a>
<a name="line_87"></a> for T in K8 do
<a name="line_88"></a>  _ASSERT(
<a name="line_89"></a>   simplify(disc_delta_proj(act_R4[T](xx)) - act_disc_delta[T](disc_delta_proj(xx)))=[0,0],
<a name="line_90"></a>   sprintf("disc_delta_proj is %a-equivariant",T)
<a name="line_91"></a>  );
<a name="line_92"></a> od:
<a name="line_93"></a>
<a name="line_94"></a> sols :=
<a name="line_95"></a>  map(s -> subs(s,xx),
<a name="line_96"></a>   {solve({disc_delta_proj(xx)[1]=cos(t),
<a name="line_97"></a>	   disc_delta_proj(xx)[2]=sin(t),
<a name="line_98"></a>	   rho(xx)=1,
<a name="line_99"></a>	   g0(xx)=0},
<a name="line_100"></a>	  {x[1],x[2],x[3],x[4]})});
<a name="line_101"></a> _ASSERT(sols = {c_E[2](t-Pi/2)},"disc_delta proj over boundary");
<a name="line_102"></a>
<a name="line_103"></a> zz := [z[1],z[2]];
<a name="line_104"></a>
<a name="line_105"></a> _ASSERT(
<a name="line_106"></a>  is_member_E(disc_delta_lift(zz)),
<a name="line_107"></a>  "disc_delta_lift maps to X"
<a name="line_108"></a> );
<a name="line_109"></a>
<a name="line_110"></a> _ASSERT(
<a name="line_111"></a>  simplify(disc_delta_proj(disc_delta_lift(zz)) - zz) = [0,0],
<a name="line_112"></a>  "disc_delta_proj o disc_delta_lift"
<a name="line_113"></a> );
<a name="line_114"></a>
<a name="line_115"></a>end:
<a name="line_116"></a>
<a name="line_117"></a>add_check(check_disc_delta):
<a name="line_118"></a>
<a name="line_119"></a>######################################################################
<a name="line_120"></a>
<a name="line_121"></a>check_disc_pi := proc()
<a name="line_122"></a> local i,j,t,T;
<a name="line_123"></a>
<a name="line_124"></a> printf("%a()\n",procname);
<a name="line_125"></a>
<a name="line_126"></a> for i in {0,1,10,11,12,13} do
<a name="line_127"></a>  _ASSERT(disc_pi_proj(v_E[i]) = [0,0],sprintf("disc_pi_proj(v[%d])",i));
<a name="line_128"></a> od;
<a name="line_129"></a> for i from 0 to 3 do
<a name="line_130"></a>  j := i+2;
<a name="line_131"></a>  t := i*Pi/2;
<a name="line_132"></a>  _ASSERT(simplify(disc_pi_proj(v_E[j]) -~ [cos(t),sin(t)]) = [0,0],
<a name="line_133"></a>	  sprintf("disc_pi_proj(v[%d])",j));
<a name="line_134"></a> od:
<a name="line_135"></a> for i from 0 to 3 do
<a name="line_136"></a>  j := i+6;
<a name="line_137"></a>  t := i*Pi/2 + Pi/4;
<a name="line_138"></a>  _ASSERT(simplify(disc_pi_proj(v_E[j]) -~ [cos(t),sin(t)]) = [0,0],
<a name="line_139"></a>	  sprintf("disc_pi_proj(v[%d])",j));
<a name="line_140"></a> od:
<a name="line_141"></a> for i from 0 to 3 do
<a name="line_142"></a>  j := i+14;
<a name="line_143"></a>  t := i*Pi/2 + Pi/4;
<a name="line_144"></a>  _ASSERT(simplify(disc_pi_proj(v_E[j]) -~ sqrt((1-a_E^2)/(a_E^2+1)) *~ [cos(t),sin(t)]) = [0,0],
<a name="line_145"></a>	  sprintf("disc_pi_proj(v[%d])",j));
<a name="line_146"></a> od:
<a name="line_147"></a> for i from 0 to 3 do
<a name="line_148"></a>  j := i+18;
<a name="line_149"></a>  t := i*Pi/2 - Pi/4;
<a name="line_150"></a>  _ASSERT(simplify(disc_pi_proj(v_E[j]) -~ sqrt((1-a_E^2)/(a_E^2+1)) *~ [cos(t),sin(t)]) = [0,0],
<a name="line_151"></a>	  sprintf("disc_pi_proj(v[%d])",j));
<a name="line_152"></a> od:
<a name="line_153"></a>
<a name="line_154"></a> for T in G16 do
<a name="line_155"></a>  _ASSERT(
<a name="line_156"></a>   simplify(disc_pi_proj(act_R4[T](xx)) - act_disc_pi[T](disc_pi_proj(xx)))=[0,0],
<a name="line_157"></a>   sprintf("disc_pi_proj is %a-equivariant",T)
<a name="line_158"></a>  );
<a name="line_159"></a> od:
<a name="line_160"></a>end:
<a name="line_161"></a>
<a name="line_162"></a>add_check(check_disc_pi):
<a name="line_163"></a>
<a name="line_164"></a>######################################################################
<a name="line_165"></a>
<a name="line_166"></a>check_disc_zeta := proc()
<a name="line_167"></a> local i,j,t,T,c0,c1;
<a name="line_168"></a>
<a name="line_169"></a> printf("%a()\n",procname);
<a name="line_170"></a> 
<a name="line_171"></a> for i in {0,1} do
<a name="line_172"></a>  _ASSERT(disc_zeta_proj(v_E[i]) = [(-1)^i/sqrt(2),0],sprintf("disc_zeta_proj(v[%d])",i));
<a name="line_173"></a> od;
<a name="line_174"></a> for i in {2,4} do
<a name="line_175"></a>  _ASSERT(disc_zeta_proj(v_E[i]) = [0,0],sprintf("disc_zeta_proj(v[%d])",i));
<a name="line_176"></a> od;
<a name="line_177"></a> for i in {3,5} do
<a name="line_178"></a>  _ASSERT(disc_zeta_proj(v_E[i]) = [0,(-1)^((i-3)/2)],sprintf("disc_zeta_proj(v[%d])",i));
<a name="line_179"></a> od;
<a name="line_180"></a> for i in {6,7,8,9} do
<a name="line_181"></a>  _ASSERT(disc_zeta_proj(v_E[i]) = [0,(-1)^floor((i-6)/2)/sqrt(2)],sprintf("disc_zeta_proj(v[%d])",i));
<a name="line_182"></a> od;
<a name="line_183"></a>
<a name="line_184"></a> c0 :=  (1/2)*(sqrt(2)*a_E-sqrt(1-a_E^2))*sqrt(2)/sqrt(1+a_E^2);
<a name="line_185"></a> c1 :=  (1/2)*(sqrt(2)*a_E+sqrt(1-a_E^2))*sqrt(2)/sqrt(1+a_E^2);
<a name="line_186"></a>
<a name="line_187"></a> for i in {10,12} do
<a name="line_188"></a>  _ASSERT(
<a name="line_189"></a>   simplify_E(disc_zeta_proj(v_E[i]) -~ [(-1)^((i-10)/2)*c0,0]) = [0,0],
<a name="line_190"></a>   sprintf("disc_zeta_proj(v[%d])",i)
<a name="line_191"></a>  );
<a name="line_192"></a> od;
<a name="line_193"></a>
<a name="line_194"></a> for i in {11,13} do
<a name="line_195"></a>  _ASSERT(
<a name="line_196"></a>   simplify_E(disc_zeta_proj(v_E[i]) -~ [(-1)^((i-11)/2)*c1,0]) = [0,0],
<a name="line_197"></a>   sprintf("disc_zeta_proj(v[%d])",i)
<a name="line_198"></a>  );
<a name="line_199"></a> od;
<a name="line_200"></a>
<a name="line_201"></a> _ASSERT(
<a name="line_202"></a>  disc_zeta_proj(c_E[0](t)) -~ [0,sin(t)] = [0,0],
<a name="line_203"></a>  "disc_zeta_proj(c[0](t))"
<a name="line_204"></a> );
<a name="line_205"></a>
<a name="line_206"></a> _ASSERT(
<a name="line_207"></a>  disc_zeta_proj(c_E[1](t)) -~ [cos(t),sin(t)] /~ sqrt(2) = [0,0],
<a name="line_208"></a>  "disc_zeta_proj(c[1](t))"
<a name="line_209"></a> );
<a name="line_210"></a>
<a name="line_211"></a> _ASSERT(
<a name="line_212"></a>  disc_zeta_proj(c_E[2](t)) -~ [cos(t),sin(t)] /~ sqrt(2) = [0,0],
<a name="line_213"></a>  "disc_zeta_proj(c[2](t))"
<a name="line_214"></a> );
<a name="line_215"></a>
<a name="line_216"></a> _ASSERT(
<a name="line_217"></a>  simplify(disc_zeta_proj(c_E0[3](t)) -~ [(sqrt(1/3)+sqrt(1/6)) * cos(t),sin(t)]) = [0,0],
<a name="line_218"></a>  "disc_zeta_proj(c[3](t))"
<a name="line_219"></a> );
<a name="line_220"></a>
<a name="line_221"></a> _ASSERT(
<a name="line_222"></a>  simplify(disc_zeta_proj(c_E0[4](t)) -~ [(sqrt(1/3)-sqrt(1/6)) * cos(t),0]) = [0,0],
<a name="line_223"></a>  "disc_zeta_proj(c[4](t))"
<a name="line_224"></a> );
<a name="line_225"></a>
<a name="line_226"></a> _ASSERT(
<a name="line_227"></a>  simplify(disc_zeta_proj(c_E0[5](t)) -~ [2+(1-cos(t))/sqrt(2),0] /~ sqrt(10 - 2*cos(t))) = [0,0],
<a name="line_228"></a>  "disc_zeta_proj(c[5](t))"
<a name="line_229"></a> );
<a name="line_230"></a>
<a name="line_231"></a> _ASSERT(
<a name="line_232"></a>  simplify(disc_zeta_proj(c_E0[6](t)) -~ [2-(1-cos(t))/sqrt(2), sin(t)] /~ sqrt(10 - 2*cos(t))) = [0,0],
<a name="line_233"></a>  "disc_zeta_proj(c[6](t))"
<a name="line_234"></a> );
<a name="line_235"></a>
<a name="line_236"></a> _ASSERT(
<a name="line_237"></a>  simplify(disc_zeta_proj(c_E0[7](t)) +~ [2+(1-cos(t))/sqrt(2),0] /~ sqrt(10 - 2*cos(t))) = [0,0],
<a name="line_238"></a>  "disc_zeta_proj(c[7](t))"
<a name="line_239"></a> );
<a name="line_240"></a>
<a name="line_241"></a> _ASSERT(
<a name="line_242"></a>  simplify(disc_zeta_proj(c_E0[8](t)) +~ [2-(1-cos(t))/sqrt(2),-sin(t)] /~ sqrt(10 - 2*cos(t))) = [0,0],
<a name="line_243"></a>  "disc_zeta_proj(c[8](t))"
<a name="line_244"></a> );
<a name="line_245"></a>
<a name="line_246"></a> for T in [1,LL,M,LLM,N,LLN,MN,LLMN] do
<a name="line_247"></a>  _ASSERT(
<a name="line_248"></a>   simplify(disc_zeta_proj(act_R4[T](xx)) - act_disc_zeta[T](disc_zeta_proj(xx)))=[0,0],
<a name="line_249"></a>   sprintf("disc_zeta_proj is %a-equivariant",T)
<a name="line_250"></a>  );
<a name="line_251"></a> od:
<a name="line_252"></a>end:
<a name="line_253"></a>
<a name="line_254"></a>add_check(check_disc_zeta):
<a name="line_255"></a>
  </pre>
 </body>
</html>
    