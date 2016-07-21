<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a><a name="CLASS_H_to_P_map"></a><span style="color:red">#@ CLASS: H_to_P_map
</span><a name="line_2"></a>
<a name="line_3"></a>`Class/Declare`("H_to_P_map",
<a name="line_4"></a> "An instance of this class encapsulates data about a cromulent isomorphism $HX(a_H) \\to PX(a_P)$ for some specific numerical values of $a_H$ and $a_P$",
<a name="line_5"></a>
<a name="line_6"></a> ["Field","a_H"::numeric,"This should be set by the @set_a_H@ method."],
<a name="line_7"></a> ["Field","a_P"::numeric,"This will be calculated by the @find_p1@ method, and should not be set directly."],
<a name="line_8"></a>
<a name="line_9"></a> ["Field","v_HS"::table,"As in Definition defn-schwarz-phi"],
<a name="line_10"></a> ["Field","v_PS"::table,"As in Definition defn-schwarz-phi"],
<a name="line_11"></a> ["Field","c_HS"::table,"As in Definition defn-schwarz-phi"],
<a name="line_12"></a> ["Field","c_PS"::table,"As in Definition defn-schwarz-phi"],
<a name="line_13"></a> ["Field","psi","As in Definition defn-schwarz-phi.  This is set up so that if @X@ is an instance of the present class, the required syntax is @X[\"psi\"](z)@."],
<a name="line_14"></a> ["Field","psi_inv","As in Definition defn-schwarz-phi, with the same kind of syntax as for @psi@"],
<a name="line_15"></a> ["Field","phi","As in Definition defn-schwarz-phi, with the same kind of syntax as for @psi@"],
<a name="line_16"></a> ["Field","phi_inv","As in Definition defn-schwarz-phi, with the same kind of syntax as for @psi@"],
<a name="line_17"></a>
<a name="line_18"></a> ["Field","samples"::list(CC0),"This is a list of points lying in $\\psi^{-1}((C_3\\cup C_5)\\cap F_{16})$.  These should be sent by $p_1$ to the unit circle, and the @find_p1@ method will try to choose the coefficients of $p_1$ to make this true.  This should be set by the @make_samples@ method."],
<a name="line_19"></a>
<a name="line_20"></a> ["Field","num_samples"::posint = 200,"Number of sample points.  This should be significantly larger than the @poly_deg@ field.  It should be set by the @make_samples@ method."],
<a name="line_21"></a> ["Field","errs"::list,"The list of differences $|p_1(z)|^2-1$, as $z$ runs through the sample points. "],
<a name="line_22"></a> ["Field","err"::RR0,"The maximum absolute value of the above errors."],
<a name="line_23"></a>
<a name="line_24"></a> ["Field","poly_deg"::posint = 20,"Number of degrees of freedom when choosing $p_1$.  It should be set by the @set_poly_deg@ method."],
<a name="line_25"></a>
<a name="line_26"></a> ["Field","a"::list(RR0),"This is a list of coefficients (of length equal to @poly_deg@) which determines the approximating function $p_1$.  In more detail, we have $p_1=p_{10}+a_1p_{11}(z)+a_2p_{12}(z)+a_3p_{13}(z)+(\sum_{i>3}a_iz^{2*i-8})p_{14}(z)$ for certain fixed functions $p_{1k}(z)$."],
<a name="line_27"></a>
<a name="line_28"></a> ["Field","p1","A rational function of $z$ which approximates the map $p_1\\:\\Delta\\to\\C$."],
<a name="line_29"></a> ["Field","p1_series","A Taylor series for $p_1$."],
<a name="line_30"></a> ["Field","p10","An auxiliary polynomial."],
<a name="line_31"></a> ["Field","p11","An auxiliary polynomial."],
<a name="line_32"></a> ["Field","p12","An auxiliary polynomial."],
<a name="line_33"></a> ["Field","p13","An auxiliary rational function."],
<a name="line_34"></a> ["Field","p14","An auxiliary polynomial."],
<a name="line_35"></a> ["Field","p1_inv","A polynomial approximation to the compositional inverse of $p_1$."],
<a name="line_36"></a> ["Field","S_p1_inv","A polynomial approximation to the schwarzian derivative of $p_1^{-1}$."],
<a name="line_37"></a> ["Field","d","A constant such that the schwarzian derivative of $p_1^{-1}$ is @S0_p1_inv + d * S1_p1_inv@"],
<a name="line_38"></a> ["Field","schwarzian_errs"::list,"A list of differences between the coefficients of $S(p_1^{-1})$ and @S0_p1_inv + d * S1_p1_inv@"],
<a name="line_39"></a> ["Field","m_series","A Taylor series for the function $m(z)$ such that $\\omega_0=m(z) dz$."],
<a name="line_40"></a> ["Field","p_series","A Taylor series for $p(z)$."],
<a name="line_41"></a> ["Field","mp_series","A Taylor series for $m(z)p(z)$."],
<a name="line_42"></a>
<a name="line_43"></a> ["Method","set_a_H"::RR1,
<a name="line_44"></a>  "Set the @a_H@ field and perform associated bookkeeping.",
<a name="line_45"></a>  proc(this,a::RR1)
<a name="line_46"></a>   local i,t,z;
<a name="line_47"></a>   this["a_H"] := evalf(a);
<a name="line_48"></a>   this["v_HS"] := table();
<a name="line_49"></a>   for i in [0,1,2,3,6,10,11,12,13] do 
<a name="line_50"></a>    this["v_HS"][i] := evalf(subs(a_H = a,v_HS[i]));
<a name="line_51"></a>   od;
<a name="line_52"></a>   this["c_HS"] := table();
<a name="line_53"></a>   for i in [0,1,3,5] do
<a name="line_54"></a>    this["c_HS"][i] := unapply(evalf(subs(a_H = a,c_HS[i](t))),t);
<a name="line_55"></a>   od;
<a name="line_56"></a>   this["psi"]     := unapply(evalf(subs(a_H = a,schwarz_psi(z))),z);
<a name="line_57"></a>   this["psi_inv"] := unapply(evalf(subs(a_H = a,schwarz_psi_inv(z))),z);
<a name="line_58"></a>
<a name="line_59"></a>   this["set_p_aux"];
<a name="line_60"></a>   return a;
<a name="line_61"></a>  end
<a name="line_62"></a> ],
<a name="line_63"></a> 
<a name="line_64"></a> ["Method","set_a_P"::RR1,
<a name="line_65"></a>  "Set the @a_P@ field and perform associated bookkeeping.  Normally one should only use the @set_a_H@ method directly; then other code will calculate an appropriate value for @a_P@ and call the @set_a_P@ method.",
<a name="line_66"></a>  proc(this,a::RR1)
<a name="line_67"></a>   local i,t,z;
<a name="line_68"></a>   this["a_P"] := evalf(a);
<a name="line_69"></a>   this["v_PS"] := table();
<a name="line_70"></a>   for i in [0,1,2,3,6,10,11,12,13] do 
<a name="line_71"></a>    this["v_PS"][i] := evalf(subs(a_P = a,v_PS[i]));
<a name="line_72"></a>   od;
<a name="line_73"></a>   this["c_PS"] := table();
<a name="line_74"></a>   for i in [0,1,3,5] do
<a name="line_75"></a>    this["c_PS"][i] := unapply(evalf(subs(a_P = a,c_PS[i](t))),t);
<a name="line_76"></a>   od;
<a name="line_77"></a>   this["phi"]     := eval(schwarz_phi);
<a name="line_78"></a>   this["phi_inv"] := eval(schwarz_phi_inv);
<a name="line_79"></a>
<a name="line_80"></a>   this["set_p_aux"];
<a name="line_81"></a>   return a;
<a name="line_82"></a>  end
<a name="line_83"></a> ],
<a name="line_84"></a> 
<a name="line_85"></a> ["Method","set_poly_deg"::void,
<a name="line_86"></a>  "Set the @poly_deg@ field and perform associated bookkeeping.",
<a name="line_87"></a>  proc(this,d::posint)
<a name="line_88"></a>   this["poly_deg"] := d;
<a name="line_89"></a>   this["fix_a"];
<a name="line_90"></a>   this["set_p_aux"];
<a name="line_91"></a>   NULL;
<a name="line_92"></a>  end
<a name="line_93"></a> ],
<a name="line_94"></a>
<a name="line_95"></a> ["Method","fix_a"::void,
<a name="line_96"></a>  "Truncate or extend the @a@ field to ensure that it has the right length.",
<a name="line_97"></a>  proc(this)
<a name="line_98"></a>   local a,n,m,z0;
<a name="line_99"></a>   n := this["poly_deg"];
<a name="line_100"></a>   a := this["a"];
<a name="line_101"></a>   if not(type([a],[list])) then
<a name="line_102"></a>    if type(this["a_P"],numeric) then
<a name="line_103"></a>     z0 := evalf(subs(a_P=this["a_P"],v_PS[11])):
<a name="line_104"></a>    else 
<a name="line_105"></a>     z0 := evalf(subs(a_P=0.5,v_PS[11])):
<a name="line_106"></a>    fi;
<a name="line_107"></a>    if n >= 2 then
<a name="line_108"></a>     this["a"] := [Re(z0),Im(z0),0$(n-2)];
<a name="line_109"></a>    else
<a name="line_110"></a>     this["a"] := [0$n];
<a name="line_111"></a>    fi;
<a name="line_112"></a>    return;
<a name="line_113"></a>   fi;
<a name="line_114"></a>   m := nops(a);
<a name="line_115"></a>   if m < n then
<a name="line_116"></a>    this["a"] := [op(a),0 $ (n-m)];
<a name="line_117"></a>   else
<a name="line_118"></a>    this["a"] := [op(1..n,a)];
<a name="line_119"></a>   fi;
<a name="line_120"></a>   NULL;
<a name="line_121"></a>  end
<a name="line_122"></a> ],
<a name="line_123"></a>
<a name="line_124"></a> ["Method","set_p_aux"::void,
<a name="line_125"></a>  "Set the auxiliary functions @p11@ to @p15@.  These are all odd polynomials with real coefficients and vanishing derivatives at $\\psi^{-1}(v_0)$ and $\\psi^{-1}(v_{11})$.  The polynomials @p11@ and @p12@ vanish at $\\psi^{-1}(v_0)$ and $\\psi^{-1}(v_3)$, and take the values $1$ and $i$ respectively at $\\psi^{-1}(v_{11})$.  The polynomial @p13@ sends $\\psi^{-1}(v_i)$ to $\\phi(v_i)$ for $i\\in\\{0,3,6,11\\}$, whereas @p14@ sends all these points to zero.  The polynomials @p11@, @p12@ and @p13@ have degree $13$, whereas @p14@ has degree $15$, with linear term $z$.  These properties charecterise the polynomials uniquely.",
<a name="line_126"></a>  proc(this)
<a name="line_127"></a>   local v,e,d,a,i,aP,sol,z0,p10,p11,p12,p13,p14;
<a name="line_128"></a>   v := eval(this["v_HS"]);
<a name="line_129"></a>   e := (f) -> evalf([Im(f(v[3])),Re(f(v[0])),Re(D(f)(v[0])),
<a name="line_130"></a>                      Re(f(v[11])),Im(f(v[11])),
<a name="line_131"></a>                      Re(D(f)(v[11])),Im(D(f)(v[11]))]);
<a name="line_132"></a>   d := this["poly_deg"];
<a name="line_133"></a>   for i from 0 to max(6,d) do assume(a[i]::real); od:
<a name="line_134"></a>
<a name="line_135"></a>   aP := this["a_P"];
<a name="line_136"></a>   if not(type([aP],[numeric])) then
<a name="line_137"></a>    aP := fsolve(schwarz_b_approx(a) - this["a_H"],a=0..1);
<a name="line_138"></a>    this["set_a_P"][aP];
<a name="line_139"></a>   fi;
<a name="line_140"></a>
<a name="line_141"></a>
<a name="line_142"></a>   p10 := unapply(add(a[i]*z^(2*i+1),i=0..6),z):
<a name="line_143"></a>#   z0 := subs(a_P=aP,v_PS[11]):
<a name="line_144"></a>#   sol := solve(e(p10) -~ [1,1,0,Re(z0),Im(z0),0,0]):
<a name="line_145"></a>   sol := solve(e(p10) -~ [1,1,0,0,0,0,0]):
<a name="line_146"></a>   p10 := unapply(subs(sol,p10(z)),z):
<a name="line_147"></a>
<a name="line_148"></a>   p11 := unapply(add(a[i]*z^(2*i+1),i=0..6),z):
<a name="line_149"></a>   sol := solve(e(p11) -~ [0,0,0,1,0,0,0]):
<a name="line_150"></a>   p11 := unapply(subs(sol,p11(z)),z):
<a name="line_151"></a>
<a name="line_152"></a>   p12 := unapply(add(a[i]*z^(2*i+1),i=0..6),z):
<a name="line_153"></a>   sol := solve(e(p12) -~ [0,0,0,0,1,0,0]):
<a name="line_154"></a>   p12 := unapply(subs(sol,p12(z)),z):
<a name="line_155"></a>
<a name="line_156"></a>   p14 := unapply(z+add(a[i]*z^(2*i+1),i=1..7),z):
<a name="line_157"></a>   sol := solve(e(p14)):
<a name="line_158"></a>   p14 := unapply(tidy(subs(sol,p14(z))),z):
<a name="line_159"></a>
<a name="line_160"></a>   p13 := unapply(p14(z)/evalf(subs(a_H = this["a_H"],tiny_p1_denom(z))),z);
<a name="line_161"></a>
<a name="line_162"></a>   this["p10"] := eval(p10):
<a name="line_163"></a>   this["p11"] := eval(p11):
<a name="line_164"></a>   this["p12"] := eval(p12):
<a name="line_165"></a>   this["p13"] := eval(p13):
<a name="line_166"></a>   this["p14"] := eval(p14):
<a name="line_167"></a>
<a name="line_168"></a>   NULL;
<a name="line_169"></a>  end
<a name="line_170"></a> ],
<a name="line_171"></a>
<a name="line_172"></a> ["Method","p"::CC0,"This calculates the function $p=\\phi^{-1}\\circ p_1\\circ\\psi$",
<a name="line_173"></a>  proc(this,z::CC0)
<a name="line_174"></a>   this["phi_inv"](this["p1"](this["psi_inv"](z)));
<a name="line_175"></a>  end
<a name="line_176"></a> ],
<a name="line_177"></a>
<a name="line_178"></a> ["Method","p_piecewise"::CC0,
<a name="line_179"></a>  "This also calculates $p(z)$, but it uses the equivariance properties of $p_1$ so we only need to calculate $p_1(w)$ when $w$ is small",
<a name="line_180"></a>  proc(this,z::CC0)
<a name="line_181"></a>   local T0,T1,z0,w0,w;
<a name="line_182"></a>
<a name="line_183"></a>   # Note: this assumes that a_H0 = this["a_H"], which we have generally 
<a name="line_184"></a>   # not assumed elsewhere.
<a name="line_185"></a>   T1,T0,z0 := op(retract_F16_H0_aux(z));
<a name="line_186"></a>   w0 := this["p",z0];
<a name="line_187"></a>   w := act_C[G_inv(T1)](w0);
<a name="line_188"></a>   return w;
<a name="line_189"></a>  end
<a name="line_190"></a> ],
<a name="line_191"></a>
<a name="line_192"></a> ["Method","m"::CC0,
<a name="line_193"></a>  "This calculates the function $m(z)$ such that $p^*(\\omega_0)=m(z) dz$",
<a name="line_194"></a>  proc(this,z0::CC0)
<a name="line_195"></a>   local z,z1,z2,z3,w3,a1;
<a name="line_196"></a>   
<a name="line_197"></a>   z1 := this["psi_inv"](z0);
<a name="line_198"></a>   z2 := this["p1"](z1);
<a name="line_199"></a>   z3 := this["phi_inv"](z2);
<a name="line_200"></a>   w3 := sqrt(subs(a_P = this["a_P"],r_P(z3)));
<a name="line_201"></a>   a1 := -2*I/(1+z2)^2 *
<a name="line_202"></a>         subs(z=z1,diff(this["p1"](z),z)) *
<a name="line_203"></a>         subs(z=z0,diff(this["psi_inv"](z),z));
<a name="line_204"></a>   return a1/w3;
<a name="line_205"></a>  end
<a name="line_206"></a> ],
<a name="line_207"></a> 
<a name="line_208"></a> ["Method","m_piecewise"::CC0,
<a name="line_209"></a>  "This also calculates $m(z)$, but it uses the equivariance properties of $p_1$ so we only need to calculate $p_1(w)$ when $w$ is small",
<a name="line_210"></a>  proc(this,z0::CC0)
<a name="line_211"></a>   local T0,T1,z1,z2,m0,m1,m2;
<a name="line_212"></a>
<a name="line_213"></a>   # Note: this assumes that a_H0 = this["a_H"], which we have generally 
<a name="line_214"></a>   # not assumed elsewhere.
<a name="line_215"></a>   T1,T0,z2 := op(retract_F16_H0_aux(z0));
<a name="line_216"></a>   z1 := act_Pi0(T0,z0);
<a name="line_217"></a>   m2 := this["m",z2];
<a name="line_218"></a>
<a name="line_219"></a>   if member(T1,{1,L,LL,LLL}) then
<a name="line_220"></a>    m1 := m2;
<a name="line_221"></a>   elif member(T1,{N,LN,LLN,LLLN}) then
<a name="line_222"></a>    m1 := conjugate(m2);
<a name="line_223"></a>   elif member(T1,{M,LM,LLM,LLLM}) then
<a name="line_224"></a>    m1 := m2 * D(act_H1[M])(z1) / act_C[G_inv(T1)](this["p",z2]);
<a name="line_225"></a>   elif member(T1,{MN,LMN,LLMN,LLLMN}) then
<a name="line_226"></a>    m1 := conjugate(m2 * D(act_H1[M])(conjugate(z1))) / act_C[G_inv(T1)](this["p",z2]);
<a name="line_227"></a>   fi;
<a name="line_228"></a>
<a name="line_229"></a>   m0 := m1 * subs(z = z0,diff(act_Pi0(T0,z),z));
<a name="line_230"></a>   return m0;
<a name="line_231"></a>  end
<a name="line_232"></a> ],
<a name="line_233"></a>
<a name="line_234"></a> ["Method","find_m_series",
<a name="line_235"></a>  "This finds a power series approximation to $m(z)$",
<a name="line_236"></a>  proc(this,radius::RR0,num_samples::posint,poly_deg::posint)
<a name="line_237"></a>   local i,j,ri,samples,v,M,a,z;
<a name="line_238"></a>
<a name="line_239"></a>   samples := evalf([seq(radius * exp(Pi*I/2*i/num_samples),i=1..num_samples)]);
<a name="line_240"></a>   ri := (u) -> map(z -> (Re(z),Im(z)),u);
<a name="line_241"></a>   v := Vector(ri(map(z -> this["m_piecewise",z],samples)));
<a name="line_242"></a>   M := Transpose(Matrix([seq(ri([seq(samples[i]^(4*j),i=1..num_samples)]),j=0..poly_deg-1)]));
<a name="line_243"></a>   a := LeastSquares(M,v,method='QR');
<a name="line_244"></a>   this["m_series"] := unapply(add(a[j]*z^(4*j-4),j=1..poly_deg),z);
<a name="line_245"></a>   return eval(this["m_series"]);
<a name="line_246"></a>  end
<a name="line_247"></a> ],
<a name="line_248"></a>
<a name="line_249"></a> ["Method","find_mp_series",
<a name="line_250"></a>  "This finds a power series approximation to $m(z)p(z)$",
<a name="line_251"></a>  proc(this,radius::RR0,num_samples::posint,poly_deg::posint)
<a name="line_252"></a>   local i,j,ri,samples,v,M,a,z;
<a name="line_253"></a>
<a name="line_254"></a>   samples := evalf([seq(radius * exp(Pi*I/2*i/num_samples),i=1..num_samples)]);
<a name="line_255"></a>   ri := (u) -> map(z -> (Re(z),Im(z)),u);
<a name="line_256"></a>   v := Vector(ri(map(z -> this["m_piecewise",z]*this["p_piecewise",z],samples)));
<a name="line_257"></a>   M := Transpose(Matrix([seq(ri([seq(samples[i]^(4*j+2),i=1..num_samples)]),j=0..poly_deg-1)]));
<a name="line_258"></a>   a := LeastSquares(M,v,method='QR');
<a name="line_259"></a>   this["mp_series"] := unapply(add(a[j]*z^(4*j-2),j=1..poly_deg),z);
<a name="line_260"></a>   return eval(this["mp_series"]);
<a name="line_261"></a>  end
<a name="line_262"></a> ],
<a name="line_263"></a>
<a name="line_264"></a> ["Method","find_p_series",
<a name="line_265"></a>  "This finds a power series approximation to $p(z)$",
<a name="line_266"></a>  proc(this,radius::RR0,num_samples::posint,poly_deg::posint)
<a name="line_267"></a>   local i,j,ri,samples,v,M,a,z;
<a name="line_268"></a>
<a name="line_269"></a>   samples := evalf([seq(radius * exp(Pi*I/2*i/num_samples),i=1..num_samples)]);
<a name="line_270"></a>   ri := (u) -> map(z -> (Re(z),Im(z)),u);
<a name="line_271"></a>   v := Vector(ri(map(z -> this["p_piecewise",z],samples)));
<a name="line_272"></a>   M := Transpose(Matrix([seq(ri([seq(samples[i]^(4*j+2),i=1..num_samples)]),j=0..poly_deg-1)]));
<a name="line_273"></a>   a := LeastSquares(M,v,method='QR');
<a name="line_274"></a>   this["p_series"] := unapply(add(a[j]*z^(4*j-2),j=1..poly_deg),z);
<a name="line_275"></a>   return eval(this["p_series"]);
<a name="line_276"></a>  end
<a name="line_277"></a> ],
<a name="line_278"></a>
<a name="line_279"></a> ["Method","make_samples"::void,
<a name="line_280"></a>  "Calculate a list of sample points.  We have used equally spaced points, but Chebyshev spacing might be better.",
<a name="line_281"></a>  proc(this,num_samples_)
<a name="line_282"></a>   local n3,n5,c3,c5,i;
<a name="line_283"></a>   
<a name="line_284"></a>   if nargs > 1 and num_samples_ > 4 then
<a name="line_285"></a>    this["num_samples"] := num_samples_;
<a name="line_286"></a>   else
<a name="line_287"></a>    this["num_samples"] := 50;
<a name="line_288"></a>   fi;
<a name="line_289"></a>
<a name="line_290"></a>   n3 := floor(this["num_samples"]/2);
<a name="line_291"></a>   n5 := this["num_samples"] - n3;
<a name="line_292"></a>   c3 := eval(this["c_HS"][3]);
<a name="line_293"></a>   c5 := eval(this["c_HS"][5]);
<a name="line_294"></a>
<a name="line_295"></a>   this["samples"] := [
<a name="line_296"></a>    seq(evalf(c3(i*Pi/(2*n3))),i=1..n3),
<a name="line_297"></a>    seq(evalf(c5(i*Pi/(n5-1))),i=0..n5-1)
<a name="line_298"></a>   ];
<a name="line_299"></a>
<a name="line_300"></a>   NULL;
<a name="line_301"></a>  end
<a name="line_302"></a> ],
<a name="line_303"></a>
<a name="line_304"></a> ["Method","find_p1"::void,
<a name="line_305"></a>  "This method searches for a polynomial @p1@ of the required form, which sends the sample points as close as possible to the unit circle.",
<a name="line_306"></a>  proc(this,num_steps::posint := 10)
<a name="line_307"></a>   local i,j,k,d,ns,S,pv,PV,CV,A,dA,J,E,aP,u,p10,p11,p12,p13,p14;
<a name="line_308"></a>
<a name="line_309"></a>   this["set_p_aux"];
<a name="line_310"></a>   this["fix_a"];
<a name="line_311"></a>   d := this["poly_deg"];
<a name="line_312"></a>   if not(type([this["samples"]],[list(CC0)])) then
<a name="line_313"></a>    this["make_samples"]:
<a name="line_314"></a>   fi; 
<a name="line_315"></a>
<a name="line_316"></a>   S := this["samples"];
<a name="line_317"></a>   ns := nops(S);
<a name="line_318"></a>
<a name="line_319"></a>   p10 := eval(this["p10"]);
<a name="line_320"></a>   p11 := eval(this["p11"]);
<a name="line_321"></a>   p12 := eval(this["p12"]);
<a name="line_322"></a>   p13 := eval(this["p13"]);
<a name="line_323"></a>   p14 := eval(this["p14"]);
<a name="line_324"></a>
<a name="line_325"></a>   pv := proc(z)
<a name="line_326"></a>    u := p14(z);
<a name="line_327"></a>    [p11(z),p12(z),p13(z),seq(z^(2*i)*u,i=0..d-4)];
<a name="line_328"></a>   end:
<a name="line_329"></a>   PV := Matrix(map(pv,S));
<a name="line_330"></a>   CV := Vector(map(p10,S));
<a name="line_331"></a>
<a name="line_332"></a>   A := Vector(this["a"]);
<a name="line_333"></a>   J := Matrix(ns,d);
<a name="line_334"></a>
<a name="line_335"></a>   # We want this vector to be zero.
<a name="line_336"></a>   E := map(z -> abs(z)^2 - 1,PV . A + CV);
<a name="line_337"></a>
<a name="line_338"></a>   for i from 1 to num_steps do
<a name="line_339"></a>
<a name="line_340"></a>    # Set J to be the Jacobian matrix of the map A |-> E
<a name="line_341"></a>    for j from 1 to ns do
<a name="line_342"></a>     for k from 1 to d do
<a name="line_343"></a>      J[j,k] := 2*Re(conjugate(PV[j,k]) * (add(PV[j,l]*A[l],l=1..d) + CV[j]));
<a name="line_344"></a>     od;
<a name="line_345"></a>    od;
<a name="line_346"></a>
<a name="line_347"></a>    # Newton-Raphson step
<a name="line_348"></a>    dA := LeastSquares(J,-E);
<a name="line_349"></a>    A := A + dA;
<a name="line_350"></a>    E := map(z -> abs(z)^2 - 1,PV . A + CV);
<a name="line_351"></a>
<a name="line_352"></a>    userinfo(7,genus2,
<a name="line_353"></a>     sprintf("log[10](step_size) = %A, log[10](max_err) = %A",
<a name="line_354"></a>      evalf[5](log[10](Norm(dA,2))), 
<a name="line_355"></a>      evalf[5](log[10](Norm(E))))
<a name="line_356"></a>    );
<a name="line_357"></a>   od:
<a name="line_358"></a>
<a name="line_359"></a>   this["a"] := convert(A,list);
<a name="line_360"></a>   this["p1"] := 
<a name="line_361"></a>    unapply(A[3] * p13(z) + 
<a name="line_362"></a>     expand(p10(z) + A[1] * p11(z) + A[2] * p12(z) + 
<a name="line_363"></a>            add(A[i]*z^(2*(i-4)),i=4..d) * p14(z)),z);
<a name="line_364"></a>
<a name="line_365"></a>   this["p1_series"] := unapply(convert(series(this["p1"](z),z=0,2*d+9),polynom,z),z);
<a name="line_366"></a>
<a name="line_367"></a>   aP := Re(schwarz_phi_inv(this["p1"](this["v_HS"][11])));
<a name="line_368"></a>   this["set_a_P",aP];
<a name="line_369"></a>
<a name="line_370"></a>   this["errs"] := convert(E,list);
<a name="line_371"></a>   this["err"] := Norm(E);
<a name="line_372"></a>   NULL;
<a name="line_373"></a>  end
<a name="line_374"></a> ],
<a name="line_375"></a>
<a name="line_376"></a> ["Method","set_p1_inv"::void,
<a name="line_377"></a>  "This sets the fields @p1_inv@, @S_p1_inv@ and @d@ based on the @p1@ field.",
<a name="line_378"></a>  proc(this)
<a name="line_379"></a>   local m,z,p1,p1i,Sp1i,err;
<a name="line_380"></a>
<a name="line_381"></a>   p1 := this["p1_series"](z);
<a name="line_382"></a>   p1i := revert_series(p1,z);
<a name="line_383"></a>   m := degree(p1,z);
<a name="line_384"></a>
<a name="line_385"></a>   this["p1_inv"] := unapply(p1i,z);
<a name="line_386"></a>   Sp1i := convert(series(schwarzian(p1i,z),z=0,m+1),polynom,z);
<a name="line_387"></a>   this["S_p1_inv"] := unapply(Sp1i,z);
<a name="line_388"></a>
<a name="line_389"></a>   err := evalf(subs({a_P = this["a_P"]},subs(z = 0,Sp1i) - S_p1_inv(0)));
<a name="line_390"></a>   this["d"] := solve(err,d_p_inv);
<a name="line_391"></a>
<a name="line_392"></a>   err := Sp1i - evalf(subs({a_P=this["a_P"],d_p_inv=this["d"]},S_p1_inv(z)));
<a name="line_393"></a>   err := convert(series(err,z=0,m+1),polynom,z); 
<a name="line_394"></a>   this["schwarzian_errs"] := [seq(abs(coeff(err,z,i)),i=2..m-3,2)];
<a name="line_395"></a>
<a name="line_396"></a>   NULL;
<a name="line_397"></a>  end
<a name="line_398"></a> ],
<a name="line_399"></a>
<a name="line_400"></a> ["Method","p1_plot"::plot,
<a name="line_401"></a>  "Generates a plot of p1(F16), which should just be the first quadrant of the unit disc",
<a name="line_402"></a>  proc(this)
<a name="line_403"></a>   local c,i,t;
<a name="line_404"></a>   for i in [0,1,3,5] do
<a name="line_405"></a>    c[i] := evalf(this["p1"](this["c_HS"][i](t)));
<a name="line_406"></a>   od:
<a name="line_407"></a>
<a name="line_408"></a>   display(
<a name="line_409"></a>    seq(cplot(c[i],t=F16_curve_limits[i],colour=c_colour[i]),i in [0,1,3,5]),
<a name="line_410"></a>    axes=none,scaling=constrained
<a name="line_411"></a>   );
<a name="line_412"></a>  end
<a name="line_413"></a> ],
<a name="line_414"></a>
<a name="line_415"></a> ["Method","v11_plot"::plot,
<a name="line_416"></a>  "Generates a plot of the behaviour of $p_1$ near $v_{11}$",
<a name="line_417"></a>  proc(this,eps := 0.01)
<a name="line_418"></a>   local c,i,t,t0,z0;
<a name="line_419"></a>   for i in [3,5] do
<a name="line_420"></a>    c[i] := evalf(this["p1"](this["c_HS"][i](t)));
<a name="line_421"></a>   od:
<a name="line_422"></a>   z0 := this["v_PS"][11];
<a name="line_423"></a>   t0 := argument(z0);
<a name="line_424"></a>
<a name="line_425"></a>   display(
<a name="line_426"></a>    plot([cos(t),sin(t),t=t0-eps..t0+eps],colour=grey),
<a name="line_427"></a>    cplot(c[3],t=-2*sqrt(eps)..2*sqrt(eps),colour=c_colour[3]),
<a name="line_428"></a>    cplot(c[5],t=Pi-4*sqrt(eps)..Pi+4*sqrt(eps),colour=c_colour[5]),
<a name="line_429"></a>    cpoint(subs(t=0,c[3]),colour=black),
<a name="line_430"></a>    scaling=constrained,
<a name="line_431"></a>    view=[Re(z0)-eps..Re(z0)+eps,Im(z0)-eps..Im(z0)+eps]
<a name="line_432"></a>   );
<a name="line_433"></a>  end
<a name="line_434"></a> ],
<a name="line_435"></a>
<a name="line_436"></a> ["Method","err_plot"::plot,
<a name="line_437"></a>  "This generates a plot showing the errors stored in the @errs@ field.",
<a name="line_438"></a>  proc(this) listplot(this["errs"],style=point); end
<a name="line_439"></a> ],
<a name="line_440"></a>
<a name="line_441"></a> ["Method","a_plot"::plot,
<a name="line_442"></a>  "This generates a plot showing the logs of the absolute values of the coefficients in the @a@ field, which determine the approximating function $p_1$",
<a name="line_443"></a>  proc(this) listplot(map(log,map(abs,this["a"])),style=point); end
<a name="line_444"></a> ],
<a name="line_445"></a>
<a name="line_446"></a> ["Method","p1_coeff_plot"::plot,
<a name="line_447"></a>  "This generates a plot showing the logs of the absolute values of the coefficients in the Taylor expansion of $p_1(z)$",
<a name="line_448"></a>  proc(this)
<a name="line_449"></a>   local p1,d,z;
<a name="line_450"></a>
<a name="line_451"></a>   p1 := this["p1_series"](z):
<a name="line_452"></a>   d := (degree(p1,z)-1)/2;
<a name="line_453"></a>   display(seq(point([2*i+1,log(abs(coeff(p1,z,2*i+1)))]),i=0..d));
<a name="line_454"></a>  end
<a name="line_455"></a> ],
<a name="line_456"></a>
<a name="line_457"></a> ["Method","p1_inv_coeff_plot",
<a name="line_458"></a>  "This generates a plot showing the logs of the absolute values of the coefficients in the Taylor expansion of $p_1^{-1}(z)$",
<a name="line_459"></a>  proc(this)
<a name="line_460"></a>   local i,d,z,p1i,cols;
<a name="line_461"></a>
<a name="line_462"></a>   cols := ["Red","YellowGreen","BlueViolet","Gold","Olive","Tomato","Lime",
<a name="line_463"></a>            "VioletRed","DeepSkyBlue","DarkOrange","Cyan","Magenta","Goldenrod",
<a name="line_464"></a>            "Turquoise","ForestGreen","DarkOrchid"];
<a name="line_465"></a>
<a name="line_466"></a>   p1i := this["p1_inv"](z):
<a name="line_467"></a>   d := (degree(p1i,z)-1)/2;
<a name="line_468"></a>   display(seq(point([2*i+1,log(abs(coeff(p1i,z,2*i+1)))],
<a name="line_469"></a>                     colour=cols[modp(i,16)+1],
<a name="line_470"></a>                     symbol=solidcircle),
<a name="line_471"></a>               i=0..d));
<a name="line_472"></a>
<a name="line_473"></a>  end
<a name="line_474"></a> ],
<a name="line_475"></a>
<a name="line_476"></a> ["Method","schwarzian_err_plot",
<a name="line_477"></a>  "This generates a plot showing the logs of the absolute values of the errors stored in the @schwarzian_errs@ field.",
<a name="line_478"></a>  proc(this) listplot(map(log,map(abs,this["schwarzian_errs"])),style=point); end
<a name="line_479"></a> ],
<a name="line_480"></a>
<a name="line_481"></a> ["Method","m_plot",
<a name="line_482"></a>  "This generates a plot showing the curve $m(r e^{it})$, where $r$ is the @radius@ argument.",
<a name="line_483"></a>  proc(this,radius,num_points)
<a name="line_484"></a>   local ms,P,i,z;
<a name="line_485"></a>   
<a name="line_486"></a>   ms := eval(this["m_series"]);
<a name="line_487"></a>   P := NULL;
<a name="line_488"></a>   for i from 0 to num_points do
<a name="line_489"></a>    z := evalf(radius * exp(Pi*I*i/2/num_points));
<a name="line_490"></a>    P := P,C_to_R2(ms(z));
<a name="line_491"></a>   od:
<a name="line_492"></a>   display(curve([P],args[4..-1]),axes=none,scaling=constrained);
<a name="line_493"></a>  end
<a name="line_494"></a> ],
<a name="line_495"></a>
<a name="line_496"></a> ["Method","m_plot_tikz",
<a name="line_497"></a>  "This generates a tikzpicture environment showing the curve $m(r e^{it})$, where $r$ is the @radius@ argument.",
<a name="line_498"></a>  proc(this,radius,num_points,scale)
<a name="line_499"></a>   local ms,s,i,z,w;
<a name="line_500"></a>   
<a name="line_501"></a>   ms := eval(this["m_series"]);
<a name="line_502"></a>   s := "\\begin{center}\n";
<a name="line_503"></a>   s := cat(s,sprintf(" \\begin{tikzpicture}[scale=%A]\n",scale));
<a name="line_504"></a>   s := cat(s,"  \\draw[smooth] "):
<a name="line_505"></a>   for i from 1 to num_points do
<a name="line_506"></a>    z := evalf(radius * exp(Pi*I*i/2/num_points));
<a name="line_507"></a>    w := ms(z);
<a name="line_508"></a>    s := cat(s,sprintf("(%1.4f,%1.4f) -- ",-Im(w),Re(w)));
<a name="line_509"></a>   od:
<a name="line_510"></a>   s := cat(s," cycle;\n \\end{tikzpicture}\n\\end{center}\n"):
<a name="line_511"></a>
<a name="line_512"></a>   return s;
<a name="line_513"></a>  end
<a name="line_514"></a> ],
<a name="line_515"></a>
<a name="line_516"></a> ["Method","check","",
<a name="line_517"></a>  proc(this)
<a name="line_518"></a>   return [
<a name="line_519"></a>   trim(P["p10"](z) + P["p10"](-z)),
<a name="line_520"></a>   trim(P["p11"](z) + P["p11"](-z)),
<a name="line_521"></a>   trim(P["p12"](z) + P["p12"](-z)),
<a name="line_522"></a>   trim(P["p14"](z) + P["p14"](-z)),
<a name="line_523"></a>   map(trim,map(Im,[coeffs(P["p10"](z),z)])),
<a name="line_524"></a>   map(trim,map(Im,[coeffs(P["p11"](z),z)])),
<a name="line_525"></a>   map(trim,map(Im,[coeffs(P["p12"](z),z)])),
<a name="line_526"></a>   map(trim,map(Im,[coeffs(P["p14"](z),z)])),
<a name="line_527"></a>   degree(P["p10"](z),z) - 13,
<a name="line_528"></a>   degree(P["p11"](z),z) - 13,
<a name="line_529"></a>   degree(P["p12"](z),z) - 13,
<a name="line_530"></a>   degree(P["p14"](z),z) - 15,
<a name="line_531"></a>   trim(evalf(subs(a_H = P["a_H"],D(P["p10"])(v_HS[ 0])))),
<a name="line_532"></a>   trim(evalf(subs(a_H = P["a_H"],D(P["p11"])(v_HS[ 0])))),
<a name="line_533"></a>   trim(evalf(subs(a_H = P["a_H"],D(P["p12"])(v_HS[ 0])))),
<a name="line_534"></a>   trim(evalf(subs(a_H = P["a_H"],D(P["p14"])(v_HS[ 0])))),
<a name="line_535"></a>   trim(evalf(subs(a_H = P["a_H"],D(P["p10"])(v_HS[11])))),
<a name="line_536"></a>   trim(evalf(subs(a_H = P["a_H"],D(P["p11"])(v_HS[11])))),
<a name="line_537"></a>   trim(evalf(subs(a_H = P["a_H"],D(P["p12"])(v_HS[11])))),
<a name="line_538"></a>   trim(evalf(subs(a_H = P["a_H"],D(P["p14"])(v_HS[11])))),
<a name="line_539"></a>   trim(evalf(subs(a_H = P["a_H"],P["p10"](v_HS[ 0]) - 1))),
<a name="line_540"></a>   trim(evalf(subs(a_H = P["a_H"],P["p10"](v_HS[ 3]) - I))),
<a name="line_541"></a>   trim(evalf(subs(a_H = P["a_H"],P["p10"](v_HS[11])))),
<a name="line_542"></a>   trim(evalf(subs(a_H = P["a_H"],P["p11"](v_HS[ 0])))),
<a name="line_543"></a>   trim(evalf(subs(a_H = P["a_H"],P["p11"](v_HS[ 3])))),
<a name="line_544"></a>   trim(evalf(subs(a_H = P["a_H"],P["p11"](v_HS[11]) - 1))),
<a name="line_545"></a>   trim(evalf(subs(a_H = P["a_H"],P["p12"](v_HS[ 0])))),
<a name="line_546"></a>   trim(evalf(subs(a_H = P["a_H"],P["p12"](v_HS[ 3])))),
<a name="line_547"></a>   trim(evalf(subs(a_H = P["a_H"],P["p12"](v_HS[11]) - I))),
<a name="line_548"></a>   trim(evalf(subs(a_H = P["a_H"],P["p14"](v_HS[ 0])))),
<a name="line_549"></a>   trim(evalf(subs(a_H = P["a_H"],P["p14"](v_HS[ 3])))),
<a name="line_550"></a>   trim(evalf(subs(a_H = P["a_H"],P["p14"](v_HS[11])))),
<a name="line_551"></a>   trim(rem(P["p14"](z),z^3,z) - z)
<a name="line_552"></a>   ];
<a name="line_553"></a>  end
<a name="line_554"></a> ]
<a name="line_555"></a>):
<a name="line_556"></a>
  </pre>
 </body>
</html>
    