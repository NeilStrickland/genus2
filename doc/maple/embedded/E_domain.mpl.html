<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a><a name="CLASS_E_point"></a><span style="color:red">#@ CLASS: E_point
</span><a name="line_2"></a>
<a name="line_3"></a>`Class/Declare`("E_point",
<a name="line_4"></a> "An instance of this class  represents a point on the embedded surface EX(a)",
<a name="line_5"></a>
<a name="line_6"></a> ["Extends","domain_point"],
<a name="line_7"></a>
<a name="line_8"></a> ["StaticField","tolerance"::float = 10.^(-98)],
<a name="line_9"></a> ["StaticField","max_iterations"::integer = 100],
<a name="line_10"></a>
<a name="line_11"></a> ["StaticField","a"::scalar,"This is the value of $a$, where we are considering $EX(a)$."],
<a name="line_12"></a> ["StaticField","alpha0"::scalar, "This is computed from a by the @set_a()@ method"],
<a name="line_13"></a> ["StaticField","alpha1"::scalar, "This is computed from a by the @set_a()@ method"],
<a name="line_14"></a> ["StaticField","alpha2"::scalar, "This is computed from a by the @set_a()@ method"],
<a name="line_15"></a> ["StaticField","alpha3"::scalar, "This is computed from a by the @set_a()@ method"],
<a name="line_16"></a> ["StaticField","alpha4"::scalar, "This is computed from a by the @set_a()@ method"],
<a name="line_17"></a> ["StaticField","alpha5"::scalar, "This is computed from a by the @set_a()@ method"],
<a name="line_18"></a> ["StaticField","alpha6"::scalar, "This is computed from a by the @set_a()@ method"],
<a name="line_19"></a> ["StaticField","alpha7"::scalar, "This is computed from a by the @set_a()@ method"],
<a name="line_20"></a> ["StaticField","alpha8"::scalar, "This is computed from a by the @set_a()@ method"],
<a name="line_21"></a> ["StaticField","alpha9"::scalar, "This is computed from a by the @set_a()@ method"],
<a name="line_22"></a> ["StaticField","alpha10"::scalar,"This is computed from a by the @set_a()@ method"],
<a name="line_23"></a> ["StaticField","alpha11"::scalar,"This is computed from a by the @set_a()@ method"],
<a name="line_24"></a> ["StaticField","alpha12"::scalar,"This is computed from a by the @set_a()@ method"],
<a name="line_25"></a> ["StaticField","alpha13"::scalar,"This is computed from a by the @set_a()@ method"],
<a name="line_26"></a> ["StaticField","alpha14"::scalar,"This is computed from a by the @set_a()@ method"],
<a name="line_27"></a>
<a name="line_28"></a> # This should be a static method, but we have not implemented those.
<a name="line_29"></a> ["Method","set_a"::void,"",
<a name="line_30"></a>  proc(this,a_)
<a name="line_31"></a>   local a;
<a name="line_32"></a>   a := `if`(nargs > 1, a_, a_E0);
<a name="line_33"></a>   this["a"] := a;
<a name="line_34"></a>   this["alpha0" ] := 1 + a^2;
<a name="line_35"></a>   this["alpha1" ] := 1 - a^2;
<a name="line_36"></a>   this["alpha2" ] := this["alpha0"]^2;
<a name="line_37"></a>   this["alpha3" ] := this["alpha1"]^2;
<a name="line_38"></a>   this["alpha4" ] := sqrt(2*a^2/this["alpha0"]);
<a name="line_39"></a>   this["alpha5" ] := sqrt(2*(1-a^2));
<a name="line_40"></a>   this["alpha6" ] := this["alpha0"]*this["alpha5"];
<a name="line_41"></a>   this["alpha7" ] := this["alpha1"]+this["alpha6"];
<a name="line_42"></a>   this["alpha8" ] := 1/2 + 7*a^2/2 + this["alpha6"];
<a name="line_43"></a>   this["alpha9" ] := 2*a*this["alpha5"];
<a name="line_44"></a>   this["alpha10"] := 4*(1/a^3-5/a);
<a name="line_45"></a>   this["alpha11"] := 8-2/a^2;
<a name="line_46"></a>   this["alpha12"] := 4+1/a^2;
<a name="line_47"></a>   this["alpha13"] := 16-20/a^2+4/a^4;
<a name="line_48"></a>   this["alpha14"] := (1-1/a^2)^2;
<a name="line_49"></a>  end
<a name="line_50"></a> ],
<a name="line_51"></a>
<a name="line_52"></a> ["Field","x"::[scalar,scalar,scalar,scalar],"Coordinates of the point"],
<a name="line_53"></a> ["Field","u"::[scalar,scalar,scalar,scalar],"A unit vector orthogonal to $x$, $n$ and $v$"],
<a name="line_54"></a> ["Field","v"::[scalar,scalar,scalar,scalar],"A unit vector orthogonal to $x$, $n$ and $u$"],
<a name="line_55"></a> ["Field","n"::[scalar,scalar,scalar,scalar],"Normalised gradient of $g$"],
<a name="line_56"></a> ["Field","z"::[scalar,scalar],"Basic G-invariant functions of $x$"],
<a name="line_57"></a> ["Field","ndg"::scalar,"Norm of the gradient of $g$"],
<a name="line_58"></a> ["Field","sndg"::scalar,"Square norm of the gradient of $g$"],
<a name="line_59"></a> ["Field","indg"::scalar,"Inverse norm of the gradient of $g$"],
<a name="line_60"></a> ["Field","curvature"::scalar,"Gaussian curvature of the surface"],
<a name="line_61"></a>
<a name="line_62"></a> ["Method","fix"::void,"",
<a name="line_63"></a>  proc(this)
<a name="line_64"></a>   this["fix_x"];
<a name="line_65"></a>   this["fix_n"];
<a name="line_66"></a>   this["fix_uv"];
<a name="line_67"></a>   this["z"] := simp(z_proj0(this["x"]));
<a name="line_68"></a>   this["curvature"] := simp(curvature0(this["x"]));
<a name="line_69"></a>   NULL;
<a name="line_70"></a>  end
<a name="line_71"></a> ],
<a name="line_72"></a>
<a name="line_73"></a> ["Method","fix_x"::void,
<a name="line_74"></a>  "This method adjusts the coordinates of a point to ensure that it lies on the surface $EX(a)$ and in the fundamental domain.  Moreover, if the constraint field is set to a value indicating that the point should lie on one of the curves $C_0$, $C_1$, $C_3$ or $C_5$, then the coordinates are adjusted if necessary to ensure that that holds.",
<a name="line_75"></a>  proc(this)
<a name="line_76"></a>   local iterations_left,gx,r,err,pp,sndg,x,x1,constraint; 
<a name="line_77"></a>
<a name="line_78"></a>   x := this["x"];
<a name="line_79"></a>   x := simp(x);
<a name="line_80"></a>   this["x"] := x;
<a name="line_81"></a>
<a name="line_82"></a>   constraint := this["constraint"];
<a name="line_83"></a>
<a name="line_84"></a>   # If x lies exactly on EX and any boundary constraint is exactly
<a name="line_85"></a>   # satisfied then we do nothing (and in particular, we do not convert
<a name="line_86"></a>   # any coordinates to floating point numbers).
<a name="line_87"></a>
<a name="line_88"></a>   if not(hastype(x,float)) and
<a name="line_89"></a>      simplify(g0(x)) = 0 and
<a name="line_90"></a>      simplify(rho(x) - 1) = 0 then
<a name="line_91"></a>    if constraint = CONSTRAINT_FIXED then
<a name="line_92"></a>     return;
<a name="line_93"></a>    elif constraint = CONSTRAINT_C0 and x[3] = 0 and x[4] = 0 then
<a name="line_94"></a>     return;
<a name="line_95"></a>    elif constraint = CONSTRAINT_C1 and simplify(x[1] - x[2]) = 0 and x[4] = 0 then
<a name="line_96"></a>     return;
<a name="line_97"></a>    elif constraint = CONSTRAINT_C3 and x[1] = 0 then
<a name="line_98"></a>     return;
<a name="line_99"></a>    elif constraint = CONSTRAINT_C5 and x[2] = 0 then
<a name="line_100"></a>     return;
<a name="line_101"></a>    fi;
<a name="line_102"></a>   fi;
<a name="line_103"></a>
<a name="line_104"></a>   # If we reach this point, then we are going to need to do numerical 
<a name="line_105"></a>   # adjustments, so we convert everything to floating point.
<a name="line_106"></a>   x := evalf(x);
<a name="line_107"></a>   this["x"] := x;
<a name="line_108"></a>
<a name="line_109"></a>   if (constraint = CONSTRAINT_V0) then
<a name="line_110"></a>    this["set_v0",true];
<a name="line_111"></a>   elif (constraint = CONSTRAINT_V3) then
<a name="line_112"></a>    this["set_v3",true];
<a name="line_113"></a>   elif (constraint = CONSTRAINT_V6) then
<a name="line_114"></a>    this["set_v6",true];
<a name="line_115"></a>   elif (constraint = CONSTRAINT_V11) then
<a name="line_116"></a>    this["set_v11",true];
<a name="line_117"></a>   elif (constraint >= CONSTRAINT_FIXED) then
<a name="line_118"></a>    # This should not occur
<a name="line_119"></a>   elif (constraint = CONSTRAINT_C0) then
<a name="line_120"></a>     # The general method would be inefficient for points on C0
<a name="line_121"></a>     x := [max(x[1],0),max(x[2],0),0,0];
<a name="line_122"></a>     r := sqrt(x[1]*x[1] + x[2]*x[2]);
<a name="line_123"></a>     this["x"] := x /~ r;
<a name="line_124"></a>   elif (constraint = CONSTRAINT_C1) then
<a name="line_125"></a>     # The general method would be inefficient for points on C1
<a name="line_126"></a>     x := [(x[1]+x[2])/2,(x[1]+x[2])/2,x[3],0];
<a name="line_127"></a>     r := sqrt(2*x[1]^2 + x[3]^2);
<a name="line_128"></a>     this["x"] := x /~ r;
<a name="line_129"></a>   else 
<a name="line_130"></a>    # We now do a Newton-Raphson procedure to find a nearby point on
<a name="line_131"></a>    # the surface EX.  The method for points on C3 and C5 is essentially
<a name="line_132"></a>    # the same as for interior points, except for a couple of extra 
<a name="line_133"></a>    # lines where we impose the boundary conditions.
<a name="line_134"></a>
<a name="line_135"></a>    iterations_left := this["max_iterations"];
<a name="line_136"></a>
<a name="line_137"></a>    x := this["x"];
<a name="line_138"></a>    r := sqrt(rho(x));
<a name="line_139"></a>    x := x /~ r;
<a name="line_140"></a>    gx := g0(x);
<a name="line_141"></a>    this["x"] := x;
<a name="line_142"></a>    this["fix_n"];
<a name="line_143"></a>    sndg := this["sndg"];
<a name="line_144"></a>    err := 1.;
<a name="line_145"></a>
<a name="line_146"></a>    while((err > this["tolerance"]) and (iterations_left > 0)) do 
<a name="line_147"></a>     pp := evalf(gx/sqrt(sndg));
<a name="line_148"></a>     if (pp >  0.1) then pp :=  0.1; fi;
<a name="line_149"></a>     if (pp < -0.1) then pp := -0.1; fi;
<a name="line_150"></a>
<a name="line_151"></a>     this["x"] := this["x"] -~ pp * this["n"];
<a name="line_152"></a>
<a name="line_153"></a>     if (constraint = CONSTRAINT_C3) then
<a name="line_154"></a>      this["x"] := map(abs,this["x"] *~ [0,1,1,-1]) *~ [0,1,1,-1];
<a name="line_155"></a>     fi;
<a name="line_156"></a>
<a name="line_157"></a>     if (constraint = CONSTRAINT_C5) then
<a name="line_158"></a>      this["x"] := map(abs,this["x"] *~ [1,0,1,-1]) *~ [1,0,1,-1];
<a name="line_159"></a>     fi;
<a name="line_160"></a>
<a name="line_161"></a>     r := sqrt(rho(this["x"]));
<a name="line_162"></a>     this["x"] := this["x"] /~ r;
<a name="line_163"></a>
<a name="line_164"></a>     this["fix_n"];
<a name="line_165"></a>     gx := evalf(g0(this["x"]));
<a name="line_166"></a>     sndg := this["sndg"];
<a name="line_167"></a>
<a name="line_168"></a>     err := abs(gx);
<a name="line_169"></a>
<a name="line_170"></a>     iterations_left := iterations_left - 1;
<a name="line_171"></a>    od;
<a name="line_172"></a>   fi;
<a name="line_173"></a>   NULL;
<a name="line_174"></a>  end
<a name="line_175"></a> ],
<a name="line_176"></a>
<a name="line_177"></a> ["Method","fix_n"::void,
<a name="line_178"></a>  "This method sets @this[\"n\"]@ to the normalisation of the gradient of $g$ at the point @this[\"x\"]@.  It uses the @simp()@ function, which simplifies symbolically unless we have any floating point subexpressions, in which case it evaluates numerically.",
<a name="line_179"></a>  proc(this)
<a name="line_180"></a>   local i,n,sndg,ndg,x;
<a name="line_181"></a>
<a name="line_182"></a>   x := this["x"];
<a name="line_183"></a>
<a name="line_184"></a>   n := simp(dg0(x));
<a name="line_185"></a>   sndg := simp(n[1]*n[1] + n[2]*n[2] + n[3]*n[3] + n[4]*n[4]);
<a name="line_186"></a>   ndg := simp(sqrt(sndg));
<a name="line_187"></a>   n := simp(n /~ ndg);
<a name="line_188"></a>
<a name="line_189"></a>   this["n"] := n;
<a name="line_190"></a>   this["sndg"] := sndg;
<a name="line_191"></a>   this["ndg"]  := ndg;
<a name="line_192"></a>   this["indg"] := simp(1/ndg);
<a name="line_193"></a>   NULL;
<a name="line_194"></a>  end
<a name="line_195"></a> ],
<a name="line_196"></a>
<a name="line_197"></a> ["Method","fix_uv"::void,
<a name="line_198"></a>  "This method sets @this[\"u\"]@ and @this[\"v\"]@ in such a way that @[this[\"x\"],this[\"n\"],this[\"u\"],this[\"v\"]]@ is an oriented orthonormal basis for $\\mathbb{R}^4$.",
<a name="line_199"></a>  proc(this)
<a name="line_200"></a>   local r,dp,uu,vv,t,constraint,x,n,u,v,u1,u2,u3,u4;
<a name="line_201"></a>
<a name="line_202"></a>   constraint := this["constraint"];
<a name="line_203"></a>   x := this["x"];
<a name="line_204"></a>   n := this["n"];
<a name="line_205"></a>   u := this["u"];
<a name="line_206"></a>   v := this["v"];
<a name="line_207"></a>
<a name="line_208"></a>   if (constraint = CONSTRAINT_C0) then
<a name="line_209"></a>    u := [-x[2],x[1],0,0];
<a name="line_210"></a>    v := [0,0,-2*a_E0,1 - 2*x[1]*x[1]];
<a name="line_211"></a>    vv := simplify(sqrt(v[3]*v[3] + v[4]*v[4]));
<a name="line_212"></a>    v := v /~ vv;
<a name="line_213"></a>   elif (constraint = CONSTRAINT_C1) then
<a name="line_214"></a>    u := [x[3]/sqrt(2),x[3]/sqrt(2),-x[1]*sqrt(2),0];
<a name="line_215"></a>    v := [2 - (1+1/a_E0^2)*x[3]*x[3],-2 + (1+1/a_E0^2)*x[3]*x[3],0,4*x[1]*x[3]/a_E0];
<a name="line_216"></a>    vv := simplify(sqrt(v[1]*v[1] + v[2]*v[2] + v[3]*v[3] + v[4]*v[4]));
<a name="line_217"></a>    v := v /~ vv;
<a name="line_218"></a>   elif (constraint = CONSTRAINT_C3) then
<a name="line_219"></a>    u2 := -a_E0*x[2]*x[2]*x[4]-x[3]*x[3]*x[3]+(a_E0^3+a_E0)*x[4]*x[3]*x[3]+
<a name="line_220"></a>	      ((3*a_E0^2+2)*x[4]*x[4]+a_E0^2)*x[3]-2*a_E0^3*x[4];
<a name="line_221"></a>    u3 := -2*x[2]*x[2]*x[2]*a_E0^2+((1-a_E0^2)*x[3]*x[3]+2*a_E0*x[3]*x[4]-2*a_E0^2*x[4]*x[4])*x[2];
<a name="line_222"></a>    u4 := x[2]*x[2]*x[2]*a_E0+(-2*a_E0*x[3]*x[3]+(-2*a_E0^2-2)*x[4]*x[3])*x[2];
<a name="line_223"></a>
<a name="line_224"></a>    u := simplify([0,u2,u3,u4]);
<a name="line_225"></a>    uu := simplify(sqrt(u[2]*u[2] + u[3]*u[3] + u[4]*u[4]));
<a name="line_226"></a>    u := simplify(u /~ uu);
<a name="line_227"></a>    v := [1,0,0,0];
<a name="line_228"></a>   elif (constraint = CONSTRAINT_C5) then
<a name="line_229"></a>    u1 := -x[3]*x[3]*x[3]-(a_E0^3+2*a_E0)*x[4]*x[3]*x[3]+((3*a_E0^2+2)*x[4]*x[4]+a_E0^2)*x[3]-
<a name="line_230"></a>	   a_E0*x[4]*x[4]*x[4]+(2*a_E0^3+a_E0)*x[4];
<a name="line_231"></a>    u3 := ((1+a_E0^2)*x[3]*x[3]-2*a_E0*x[3]*x[4]-2*a_E0^2)*x[1];
<a name="line_232"></a>    u4 := (3*a_E0*x[3]*x[3]-2*(1+a_E0^2)*x[4]*x[3]+a_E0*x[4]*x[4]-a_E0)*x[1];
<a name="line_233"></a>
<a name="line_234"></a>    u := simplify([u1,0,u3,u4]);
<a name="line_235"></a>    uu := simplify(sqrt(u[1]*u[1] + u[3]*u[3] + u[4]*u[4]));
<a name="line_236"></a>    u := simplify(u /~ uu);
<a name="line_237"></a>    v := [0,-1,0,0];
<a name="line_238"></a>   else
<a name="line_239"></a>    u := [1,1,1,1];
<a name="line_240"></a>    dp := add(x[t]*u[t],t=1..4);
<a name="line_241"></a>    u := simplify(u -~ dp *~ x);
<a name="line_242"></a>    dp := add(n[t]*u[t],t=1..4);
<a name="line_243"></a>    u := simplify(u -~ dp *~ n);
<a name="line_244"></a>    u := simplify(u /~ sqrt(add(u[t]^2,t=1..4)));
<a name="line_245"></a>    v := simplify([
<a name="line_246"></a>      -x[2]*n[3]*u[4]+x[2]*u[3]*n[4]-n[2]*u[3]*x[4]+n[2]*x[3]*u[4]-u[2]*x[3]*n[4]+u[2]*n[3]*x[4],
<a name="line_247"></a>       x[1]*n[3]*u[4]-x[1]*u[3]*n[4]-n[1]*x[3]*u[4]+n[1]*u[3]*x[4]+u[1]*x[3]*n[4]-u[1]*n[3]*x[4],
<a name="line_248"></a>      -x[1]*n[2]*u[4]+x[1]*u[2]*n[4]+n[1]*x[2]*u[4]-n[1]*u[2]*x[4]-u[1]*x[2]*n[4]+u[1]*n[2]*x[4],
<a name="line_249"></a>       x[1]*n[2]*u[3]-x[1]*u[2]*n[3]-n[1]*x[2]*u[3]+n[1]*u[2]*x[3]+u[1]*x[2]*n[3]-u[1]*n[2]*x[3]
<a name="line_250"></a>      ]);
<a name="line_251"></a>    v := simplify(v /~ sqrt(add(v[t]^2,t=1..4)));
<a name="line_252"></a>   fi;
<a name="line_253"></a>
<a name="line_254"></a>   this["u"] := simp(u);
<a name="line_255"></a>   this["v"] := simp(v);
<a name="line_256"></a>   NULL;
<a name="line_257"></a>  end
<a name="line_258"></a> ],
<a name="line_259"></a>
<a name="line_260"></a> ["Method","set_C0"::void,"This method sets the current point to lie on the curve $C_0$.  As the parameter $t$ runs from $0$ to $1$, the point covers the section of $C_0$ contained in the fundamental domain $F_{16}$ starting with $v_6$ at $t=0$, and moving to $v_3$ at $t=1$.",
<a name="line_261"></a>  proc(this,t::scalar)
<a name="line_262"></a>   local theta;
<a name="line_263"></a>
<a name="line_264"></a>   this["constraint"] := CONSTRAINT_C0;
<a name="line_265"></a>   theta := simp(Pi/4 * (1 + t));
<a name="line_266"></a>   this["x"] := simp([cos(theta),sin(theta),0,0]);
<a name="line_267"></a>   this["fix"];
<a name="line_268"></a>   NULL;
<a name="line_269"></a>  end
<a name="line_270"></a> ],
<a name="line_271"></a>
<a name="line_272"></a> ["Method","set_C1"::void,"This method sets the current point to lie on the curve $C_1$.  As the parameter $t$ runs from $0$ to $1$, the point covers the section of $C_1$ contained in the fundamental domain $F_{16}$ starting with $v_0$ at $t=0$, and moving to $v_6$ at $t=1$.",
<a name="line_273"></a>  proc(this,t::scalar)
<a name="line_274"></a>   local theta;
<a name="line_275"></a>
<a name="line_276"></a>   this["constraint"] := CONSTRAINT_C1;
<a name="line_277"></a>   theta := simp(Pi/2*t);
<a name="line_278"></a>   this["x"] := simp([sin(theta)/sqrt(2),sin(theta)/sqrt(2),cos(theta),0]);
<a name="line_279"></a>   this["fix"];
<a name="line_280"></a>   NULL;
<a name="line_281"></a>  end
<a name="line_282"></a> ],
<a name="line_283"></a>
<a name="line_284"></a> ["Method","set_C3"::void,"This method sets the current point to lie on the curve $C_3$.  As the parameter $t$ runs from $0$ to $1$, the point covers the section of $C_3$ contained in the fundamental domain $F_{16}$ starting with $v_{11}$ at $t=0$, and moving to $v_3$ at $t=1$.",
<a name="line_285"></a>  proc(this,t::scalar)
<a name="line_286"></a>   local theta,C,S,u0,u1,u2,x2,x3,x4;
<a name="line_287"></a>
<a name="line_288"></a>   this["constraint"] := CONSTRAINT_C3;
<a name="line_289"></a>   theta := simp(Pi/2*t);
<a name="line_290"></a>   C := simp(cos(theta));
<a name="line_291"></a>   S := simp(sin(theta));
<a name="line_292"></a>
<a name="line_293"></a>   u0 := this["alpha2"] - 2*this["a"]^2*this["alpha0"] * C^2 + this["alpha3"] * C^4;
<a name="line_294"></a>   u1 := this["alpha0"] * S^2 + sqrt(u0);
<a name="line_295"></a>   u2 := this["alpha1"] + 2*this["a"]^2*S^2;
<a name="line_296"></a>   x2 := S * sqrt(2*u2/u1);
<a name="line_297"></a>   x3 := this["alpha4"] * C;
<a name="line_298"></a>   x4 := -x3*u2/(this["a"]*u1);
<a name="line_299"></a>
<a name="line_300"></a>   this["x"] := simp([0,x2,x3,x4]);
<a name="line_301"></a>
<a name="line_302"></a>   this["fix"];
<a name="line_303"></a>   NULL;
<a name="line_304"></a>  end
<a name="line_305"></a> ],
<a name="line_306"></a>
<a name="line_307"></a> ["Method","set_C5"::void,"This method sets the current point to lie on the curve $C_5$.  As the parameter $t$ runs from $0$ to $1$, the point covers the section of $C_5$ contained in the fundamental domain $F_{16}$ starting with $v_0$ at $t=0$, and moving to $v_{11}$ at $t=1$.",
<a name="line_308"></a>  proc(this,t::scalar)
<a name="line_309"></a>   local theta,C,S,u0,u1,u2,x1,x3,x4;
<a name="line_310"></a>
<a name="line_311"></a>   this["constraint"] := CONSTRAINT_C5;
<a name="line_312"></a>   theta := simp(Pi*t);
<a name="line_313"></a>   C := simp(cos(theta));
<a name="line_314"></a>   S := simp(sin(theta));
<a name="line_315"></a>
<a name="line_316"></a>   u0 := this["alpha1"]/2*C^2 - this["alpha7"]*C + this["alpha8"];
<a name="line_317"></a>   u1 := this["alpha9"]*(1-C) + 4*this["a"];
<a name="line_318"></a>   u2 := this["alpha1"]*this["alpha5"]*(3-C);
<a name="line_319"></a>   x1 := -1/2*S*sqrt(u2/u0);
<a name="line_320"></a>   x3 := sqrt(this["a"]*u1/u0);
<a name="line_321"></a>   x4 := this["alpha5"]*(C-1)*x3/(4*this["a"]);
<a name="line_322"></a>
<a name="line_323"></a>   this["x"] := simp([x1,0,x3,x4]);
<a name="line_324"></a>
<a name="line_325"></a>   this["fix"];
<a name="line_326"></a>   NULL;
<a name="line_327"></a>  end
<a name="line_328"></a> ],
<a name="line_329"></a>
<a name="line_330"></a> ["Method","set_random"::void,"This sets the current point to a randomly generated point in $F_{16}$",
<a name="line_331"></a>  proc(this) 
<a name="line_332"></a>   this["set",CONSTRAINT_FREE,retract_F16_E(random_X_point())];
<a name="line_333"></a>  end
<a name="line_334"></a> ],
<a name="line_335"></a>
<a name="line_336"></a> NULL
<a name="line_337"></a>);
<a name="line_338"></a>
<a name="line_339"></a>######################################################################
<a name="line_340"></a>
<a name="line_341"></a><a name="CLASS_E_sample_point"></a><span style="color:red">#@ CLASS: E_sample_point
</span><a name="line_342"></a>
<a name="line_343"></a>`Class/Declare`("E_sample_point",
<a name="line_344"></a> "An instance of this class represents a point in a triangular face of a domain of type @E@.  In addition to the usual data for a point in such a domain, it has some additional fields that only make sense relative to the containing face.",
<a name="line_345"></a>
<a name="line_346"></a> ["Extends","E_point"],
<a name="line_347"></a>
<a name="line_348"></a> ["Field","barycentric_coords"::list(scalar),
<a name="line_349"></a>  "The (generalised) barycentric coordinates of the point, relative to the containing face.  This is a list of three nonnegative scalars, whose sum is one."
<a name="line_350"></a> ],
<a name="line_351"></a>
<a name="line_352"></a> ["Field","ibj"::scalar = 0,
<a name="line_353"></a>  "This field is the Jacobian of the inverse of the barycentric coordinate map.  (TODO: clarify the normalisation conditions.)"
<a name="line_354"></a> ],
<a name="line_355"></a>
<a name="line_356"></a> ["Field","quadrature_weight"::scalar,
<a name="line_357"></a>  "This will be set in such a way that the integral (with respect to metric area) of a function on the face can be approximated by the sum of the values at the sample points multiplied by the corresponding quadrature weights"
<a name="line_358"></a> ]
<a name="line_359"></a>);
<a name="line_360"></a>
<a name="line_361"></a>######################################################################
<a name="line_362"></a>
<a name="line_363"></a><a name="CLASS_E_edge"></a><span style="color:red">#@ CLASS: E_edge
</span><a name="line_364"></a>
<a name="line_365"></a>`Class/Declare`(
<a name="line_366"></a> "E_edge",
<a name="line_367"></a> "",
<a name="line_368"></a> ["Extends","domain_edge"],
<a name="line_369"></a>
<a name="line_370"></a> ["Field","hyperbolic_length"::scalar],
<a name="line_371"></a>
<a name="line_372"></a> ["Constructor","",
<a name="line_373"></a>  proc(this,P0::E_point,P1::E_point)
<a name="line_374"></a>   this["end"] := table();
<a name="line_375"></a>   this["set",P0,P1];
<a name="line_376"></a>  end
<a name="line_377"></a> ]
<a name="line_378"></a>);
<a name="line_379"></a>
<a name="line_380"></a>#######################################################################
<a name="line_381"></a>
<a name="line_382"></a><a name="CLASS_E_face"></a><span style="color:red">#@ CLASS: E_face
</span><a name="line_383"></a>
<a name="line_384"></a>`Class/Declare`(
<a name="line_385"></a> "E_face",
<a name="line_386"></a> "",
<a name="line_387"></a> ["Extends","domain_face"],
<a name="line_388"></a> ["Field","samples"::table,"This is a table indexed by some points in the 2-simplex $\\Delta_2$.  The entries are instances of the class @E_sample_point@.  Typically we will fix a triangle quadrature rule @Q@, with points $t_i$ say.  The @samples@ table will have indices $t_i$, and the corresponding entry will represent a point $u_i\\in EX^*$ with barycentric coordinates $t_i$ with respect to this face.  This is set up by the @create_samples@ method."],
<a name="line_389"></a> ["Field","sample_list"::list,"This contains the same @E_sample_point@ objects as the @samples@ table, but organised as a list.  This is set up by the @create_samples@ method."],
<a name="line_390"></a> ["Field","total_area"::scalar = 0,"The area of this face, with respect to the metric on $EX^*$ inherited from the standard metric on $S^3$.  This is calculated by the @create_samples@ method."],
<a name="line_391"></a> ["Field","total_curvature"::scalar = 0,"The integral of the Gaussian curvature function over this face.  This is calculated by the @create_samples@ method."],
<a name="line_392"></a> ["Field","barycentric_table","obsolete"],
<a name="line_393"></a> 
<a name="line_394"></a> ["Constructor","",
<a name="line_395"></a>  proc(this,S0::E_edge,S1::E_edge,S2::E_edge)
<a name="line_396"></a>   local C;
<a name="line_397"></a>
<a name="line_398"></a>   this["corner"] := table();
<a name="line_399"></a>   this["side"] := table();
<a name="line_400"></a>   this["samples"] := table();
<a name="line_401"></a>   this["set",S0,S1,S2];
<a name="line_402"></a>   C := eval(this["corner"]);
<a name="line_403"></a>  end
<a name="line_404"></a> ],
<a name="line_405"></a>
<a name="line_406"></a> ["Method","create_samples"::void,
<a name="line_407"></a>  "This method sets up the fields @samples@, @samples_list@, @total_area@ and @total_curvature@, as described above.",
<a name="line_408"></a>  proc(this,Q::triangle_quadrature_rule)
<a name="line_409"></a>   local A,F,tt,ttt,x,xt,P,i,j,k,V,L;
<a name="line_410"></a>
<a name="line_411"></a>   this["samples"] := table():
<a name="line_412"></a>
<a name="line_413"></a>   A := [this["corner"][0]["x"],
<a name="line_414"></a>         this["corner"][1]["x"],
<a name="line_415"></a>	 this["corner"][2]["x"]];
<a name="line_416"></a>
<a name="line_417"></a>   L := [];
<a name="line_418"></a>   this["total_area"] := 0;
<a name="line_419"></a>   this["total_curvature"] := 0;
<a name="line_420"></a>
<a name="line_421"></a>   F := barycentric_inverse_F(A);
<a name="line_422"></a>
<a name="line_423"></a>   for i from 1 to Q["num_points"] do
<a name="line_424"></a>    tt := Q["points"][i];
<a name="line_425"></a>    P := `new/E_sample_point`();
<a name="line_426"></a>    P["set",CONSTRAINT_FREE,barycentric_inverse(A,tt,F)];
<a name="line_427"></a>    P["ibj"] := simp(1/barycentric_jacobian(A,P["x"]));
<a name="line_428"></a>    P["quadrature_weight"] := simp(P["ibj"] * Q["weights"][i]); 
<a name="line_429"></a>    this["samples"][op(tt)] := eval(P);
<a name="line_430"></a>    L := [op(L),eval(P)];
<a name="line_431"></a>    this["total_area"] := this["total_area"] + P["quadrature_weight"];
<a name="line_432"></a>    this["total_curvature"] := this["total_curvature"] + 
<a name="line_433"></a>      simp(P["quadrature_weight"] * P["curvature"]);
<a name="line_434"></a>   od;
<a name="line_435"></a>
<a name="line_436"></a>   this["sample_list"] := L;
<a name="line_437"></a>   NULL;
<a name="line_438"></a>  end
<a name="line_439"></a> ],
<a name="line_440"></a>
<a name="line_441"></a> ["Method","check","This checks the correctness of the sample points.  It returns an offset error (which measures the failure of the points to lie exactly on $EX^*$) and a barycentric error (which measures the failure of the barycentric coordinates to be equal to the indices in the @samples@ table).",
<a name="line_442"></a>  proc(this)
<a name="line_443"></a>   local a1,a2,a3,e,x0,x1,x2,t1,tt,i,
<a name="line_444"></a>	 barycentric_err,barycentric_worst,
<a name="line_445"></a>	 interpolation_err,interpolation_worst,
<a name="line_446"></a>	 offset_err,offset_worst,
<a name="line_447"></a>	 F16_err,F16_worst;
<a name="line_448"></a>
<a name="line_449"></a>   a1 := this["corner"][0]["x"];
<a name="line_450"></a>   a2 := this["corner"][1]["x"];
<a name="line_451"></a>   a3 := this["corner"][2]["x"];
<a name="line_452"></a>
<a name="line_453"></a>   barycentric_worst := NULL;
<a name="line_454"></a>   offset_worst := NULL;
<a name="line_455"></a>   F16_worst := NULL;
<a name="line_456"></a>
<a name="line_457"></a>   barycentric_err := 0;
<a name="line_458"></a>   offset_err := 0;
<a name="line_459"></a>   F16_err := 0;
<a name="line_460"></a>   
<a name="line_461"></a>   for tt in indices(this["samples"]) do
<a name="line_462"></a>    x0 := this["samples"][op(tt)]["x"];
<a name="line_463"></a>    e := max(abs(rho(x0)-1),abs(g_01(x0)));
<a name="line_464"></a>    if (e > offset_err) then
<a name="line_465"></a>     offset_err := e;
<a name="line_466"></a>     offset_worst := tt;
<a name="line_467"></a>    fi;
<a name="line_468"></a>
<a name="line_469"></a>    e := abs(min(evalf([0,x0[1],x0[2],x0[3],g_10(x0)])));
<a name="line_470"></a>    if (e > F16_err) then
<a name="line_471"></a>     F16_err := e;
<a name="line_472"></a>     F16_worst := tt;
<a name="line_473"></a>    fi;
<a name="line_474"></a>    
<a name="line_475"></a>    t1 := barycentric_coords([a1,a2,a3],x0);
<a name="line_476"></a>    e := d3f(tt,t1);
<a name="line_477"></a>    if (e > barycentric_err) then
<a name="line_478"></a>     barycentric_err := e;
<a name="line_479"></a>     barycentric_worst := tt;
<a name="line_480"></a>    fi;
<a name="line_481"></a>   od:
<a name="line_482"></a>
<a name="line_483"></a>   return [barycentric_err,barycentric_worst,
<a name="line_484"></a>           offset_err,offset_worst,
<a name="line_485"></a>	   F16_err,F16_worst];
<a name="line_486"></a>  end
<a name="line_487"></a> ],
<a name="line_488"></a>
<a name="line_489"></a> ["Method","plot_x","",
<a name="line_490"></a>  proc(this,f,vertical_range := -2..2)
<a name="line_491"></a>   local S;
<a name="line_492"></a>   S := eval(this["samples"]);
<a name="line_493"></a>   display(
<a name="line_494"></a>    seq(point([op(triangle_proj(t0)),f(S[op(t0)]["x"])],colour=red),
<a name="line_495"></a>     t0 in [indices(S)]),
<a name="line_496"></a>    triangle_axes(vertical_range),
<a name="line_497"></a>    axes=none,scaling=constrained
<a name="line_498"></a>   );
<a name="line_499"></a>  end
<a name="line_500"></a> ],
<a name="line_501"></a>
<a name="line_502"></a> ["Method","plot_z","",
<a name="line_503"></a>  proc(this,f,z_range := -2..2)
<a name="line_504"></a>   this["plot_x",(u) -> f(z_proj1(u))];
<a name="line_505"></a>  end
<a name="line_506"></a> ],
<a name="line_507"></a> 
<a name="line_508"></a> ["Method","plot_x_ibj","",
<a name="line_509"></a>  proc(this,f,vertical_range := -2..2)
<a name="line_510"></a>   local S;
<a name="line_511"></a>   S := eval(this["samples"]);
<a name="line_512"></a>   display(
<a name="line_513"></a>    seq(point([op(triangle_proj(t0)),f(S[op(t0)]["x"]) * S[op(t0)]["ibj"]],colour=red),
<a name="line_514"></a>     t0 in [indices(S)]),
<a name="line_515"></a>    triangle_axes(vertical_range),
<a name="line_516"></a>    axes=none,scaling=constrained
<a name="line_517"></a>   );
<a name="line_518"></a>  end
<a name="line_519"></a> ],
<a name="line_520"></a>
<a name="line_521"></a> ["Method","plot_z_ibj","",
<a name="line_522"></a>  proc(this,f,z_range := -2..2)
<a name="line_523"></a>   this["plot_x",(u) -> f(z_proj1(u))];
<a name="line_524"></a>  end
<a name="line_525"></a> ]
<a name="line_526"></a>);
<a name="line_527"></a>
<a name="line_528"></a>######################################################################
<a name="line_529"></a>
<a name="line_530"></a><a name="CLASS_E_grid"></a><span style="color:red">#@ CLASS: E_grid
</span><a name="line_531"></a>
<a name="line_532"></a>`Class/Declare`(
<a name="line_533"></a> "E_grid",
<a name="line_534"></a> "",
<a name="line_535"></a> ["Extends","grid"],
<a name="line_536"></a> ["Field","triangle_quadrature_rule"],
<a name="line_537"></a> ["Field","int_table"::table,"This is a table indexed by triples $(i,j,k)$; the values are the integrals over $F_{16}$ of $z_1^iz_2^j|n|^k$"],
<a name="line_538"></a>
<a name="line_539"></a> ["Constructor",
<a name="line_540"></a>  "",
<a name="line_541"></a>  proc(this)
<a name="line_542"></a>   this["points"] := table();
<a name="line_543"></a>   this["edges"] := table();
<a name="line_544"></a>   this["faces"] := table();
<a name="line_545"></a>   this["edges_by_ends"] := table();
<a name="line_546"></a>   this["faces_by_corners"] := table();
<a name="line_547"></a>   this["int_table"] := table();
<a name="line_548"></a>   this["domain"] := E_domain;
<a name="line_549"></a>  end
<a name="line_550"></a> ],
<a name="line_551"></a>
<a name="line_552"></a> ["Method","create_samples"::void,"This invokes the @create_samples@ method for each face.",
<a name="line_553"></a>  proc(this)
<a name="line_554"></a>   local i,n,F;
<a name="line_555"></a>   n := this["num_faces"];
<a name="line_556"></a>   for i from 0 to n-1 do
<a name="line_557"></a>    userinfo(6,genus2,sprintf("Creating samples for face %d/%d",i,n));
<a name="line_558"></a>    F := eval(this["faces"][i]);
<a name="line_559"></a>    F["create_samples",this["triangle_quadrature_rule"]];
<a name="line_560"></a>   od;
<a name="line_561"></a>   NULL;
<a name="line_562"></a>  end
<a name="line_563"></a> ],
<a name="line_564"></a>
<a name="line_565"></a> ["Method","int_x",
<a name="line_566"></a>  "The argument $f$ is assumed to be a polynomial in $x_1,\\dotsc,x_4$.  The method returns an approximation to the integral over $EX(a)$ of $f$, or of $f |n|^k$ if $k$ is given as an additional argument.  Here $n$ is the gradient of $g$.",
<a name="line_567"></a>  proc(this,f,k_)
<a name="line_568"></a>   local f1;
<a name="line_569"></a>   f1 := NF_z(expand(add(act_A[T](f),T in G16)));
<a name="line_570"></a>   return this["int_z",f1,args[2..-1]];
<a name="line_571"></a>  end
<a name="line_572"></a> ],
<a name="line_573"></a>
<a name="line_574"></a> ["Method","int_z",
<a name="line_575"></a>  "The argument $f$ is assumed to be an expression in $z_1$ and $z_2$.  The method returns an approximation to the integral over F16 of $f$, or of $f |n|^k$ if $k$ is given as an additional argument.  Note that this is 1/16 times the integral over $EX(a)$.",
<a name="line_576"></a>  proc(this,f,k_)
<a name="line_577"></a>   local J,i,F,tt,P,f0,k;
<a name="line_578"></a>
<a name="line_579"></a>   J := 0;
<a name="line_580"></a>   k := `if`(nargs > 2,k_,0);
<a name="line_581"></a>
<a name="line_582"></a>   for i in indices(this["faces"]) do
<a name="line_583"></a>    F := eval(this["faces"][op(i)]);
<a name="line_584"></a>    for tt in indices(F["samples"]) do
<a name="line_585"></a>     P := eval(F["samples"][op(tt)]);
<a name="line_586"></a>     f0 := subs({z[1]=P["z"][1],z[2]=P["z"][2]},f);
<a name="line_587"></a>     J := J + f0 * P["quadrature_weight"] * P["ndg"]^k;
<a name="line_588"></a>    od;
<a name="line_589"></a>   od;
<a name="line_590"></a>
<a name="line_591"></a>   return(J);
<a name="line_592"></a>  end
<a name="line_593"></a> ],
<a name="line_594"></a>
<a name="line_595"></a> ["Method","set_int_table","This sets one value in @int_table@",
<a name="line_596"></a>  proc(this,i,j,k_)
<a name="line_597"></a>   local k;
<a name="line_598"></a>   k := `if`(nargs > 3, k_, 0);
<a name="line_599"></a>   this["int_table"][i,j,k] := this["int_z",z[1]^i*z[2]^j,k];
<a name="line_600"></a>  end
<a name="line_601"></a> ],
<a name="line_602"></a>
<a name="line_603"></a> ["Method","int_z_by_table","This assumes that $f$ is a polynomial in $z_1$ and $z_2$.  It calculates the integral of $f$ over $F_{16}$ by looking up the integrals of the individual monomials in @int_table@.  If any of the required integrals are missing from @int_table@, then they will be calculated and saved there.",
<a name="line_604"></a>  proc(this,f,k_)
<a name="line_605"></a>   local f0,k,n1,n2,i1,i2,b,c,J;
<a name="line_606"></a>   f0 := expand(f);
<a name="line_607"></a>   k := `if`(nargs > 2,k_,0);
<a name="line_608"></a>
<a name="line_609"></a>   n1 := degree(f0,z[1]);
<a name="line_610"></a>   n2 := degree(f0,z[2]);
<a name="line_611"></a>   J := 0;
<a name="line_612"></a>
<a name="line_613"></a>   for i1 from 0 to n1 do
<a name="line_614"></a>    for i2 from 0 to n2 do
<a name="line_615"></a>     c := coeff(coeff(f0,z[1],i1),z[2],i2);
<a name="line_616"></a>     if c <> 0 then
<a name="line_617"></a>      b := this["int_table"][i1,i2,k];
<a name="line_618"></a>      if not(type(b,numeric)) then
<a name="line_619"></a>       this["set_int_table",i1,i2,k];
<a name="line_620"></a>       b := this["int_table"][i1,i2,k];
<a name="line_621"></a>      fi;
<a name="line_622"></a>      J := J + b * c;
<a name="line_623"></a>     fi;
<a name="line_624"></a>    od;
<a name="line_625"></a>   od;
<a name="line_626"></a>   return(J);
<a name="line_627"></a>  end
<a name="line_628"></a> ],
<a name="line_629"></a>
<a name="line_630"></a> ["Method","set_max_deg"::void,"Calculate and save integrals for all monomials in $z_1$ and $z_2$ of degree at most $d$",
<a name="line_631"></a>  proc(this,d::posint)
<a name="line_632"></a>   local i,j,m,u;
<a name="line_633"></a>
<a name="line_634"></a>   for m from 0 to d do
<a name="line_635"></a>    for i from 0 to m do
<a name="line_636"></a>     j := m - i;
<a name="line_637"></a>     u := z[1]^i * z[2]^j;
<a name="line_638"></a>     userinfo(7,genus2,sprintf("Integrating %A",u));
<a name="line_639"></a>     this["int_z_by_table",u];
<a name="line_640"></a>    od;
<a name="line_641"></a>   od:
<a name="line_642"></a>   NULL;
<a name="line_643"></a>  end
<a name="line_644"></a> ],
<a name="line_645"></a>
<a name="line_646"></a> ["Method","total_area","This returns the total area of $F_{16}$",
<a name="line_647"></a>  proc(this)
<a name="line_648"></a>   add(this["faces"][i]["total_area"],i=0..this["num_faces"]-1);
<a name="line_649"></a>  end
<a name="line_650"></a> ],
<a name="line_651"></a>
<a name="line_652"></a> ["Method","total_curvature","This returns the integral of the curvature over $F_{16}$",
<a name="line_653"></a>  proc(this)
<a name="line_654"></a>   add(this["faces"][i]["total_curvature"],i=0..this["num_faces"]-1);
<a name="line_655"></a>  end
<a name="line_656"></a> ],
<a name="line_657"></a>
<a name="line_658"></a> ["Method","curvature_error","If the integration rule is accurate, then the result of this method should be zero, by the Gauss-Bonet Theorem.",
<a name="line_659"></a>  proc(this)
<a name="line_660"></a>   evalf(Pi/4 + this["total_curvature"]);
<a name="line_661"></a>  end
<a name="line_662"></a> ],
<a name="line_663"></a>
<a name="line_664"></a> ["Method","stokes_error","Here @ff@ should be a list $(f_1,f_2)$ of two expressions in $z_1$ and $z_2$.  The method returns the approximated integral of the exterior derivative of $f_1\\alpha_1+f_2\\alpha_2$, where the forms $\\alpha_i$ are defined in @embedded/roothalf/forms.mpl@.  If the integration rule is accurate, then the result should be zero, by Stokes's Theorem.",
<a name="line_665"></a>  proc(this,ff)
<a name="line_666"></a>   if `class/E_point`["StaticFieldValue"]["a"] <> 1/sqrt(2) then
<a name="line_667"></a>    return(FAIL);
<a name="line_668"></a>   fi;
<a name="line_669"></a>   
<a name="line_670"></a>   this["int_z",stokes_alpha(ff)];
<a name="line_671"></a>  end
<a name="line_672"></a> ]
<a name="line_673"></a>);
<a name="line_674"></a>
<a name="line_675"></a>######################################################################
<a name="line_676"></a>
<a name="line_677"></a><span style="color:red">#@ E_domain
</span><a name="line_678"></a>
<a name="line_679"></a>E_domain := `new/domain`():
<a name="line_680"></a>E_domain["set_point_class","E_point"]:
<a name="line_681"></a>E_domain["set_edge_class" ,"E_edge" ]:
<a name="line_682"></a>E_domain["set_face_class" ,"E_face" ]:
<a name="line_683"></a>E_domain["set_grid_class" ,"E_grid" ]:
<a name="line_684"></a>
<a name="line_685"></a>if (type(a_E0,realcons)) then
<a name="line_686"></a> `new/E_point`()["set_a",a_E0];
<a name="line_687"></a>fi:
<a name="line_688"></a>
<a name="line_689"></a>######################################################################
<a name="line_690"></a>
  </pre>
 </body>
</html>
    