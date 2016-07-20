<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a><a name="CLASS_P_to_H_chart"></a><span style="color:red">#@ CLASS: P_to_H_chart
</span><a name="line_2"></a>
<a name="line_3"></a>`Class/Declare`(
<a name="line_4"></a> "P_to_H_chart",
<a name="line_5"></a> "An instance of this class represents a pair of approximate polynomial solutions (f0,f1) near z=z0 to a certain linear second order differential equation depending on a parameter @d@.  If @d@ is chosen correctly, then f0 and f1 will be closely related to the canonical hyperbolic covering of the surface PX(a_P).",
<a name="line_6"></a> ["Field","a_P"::RR1,""],
<a name="line_7"></a> ["Field","degree"::posint=100,"The degree of polynomials used for approximate power series."],
<a name="line_8"></a> ["Field","d"::RR1,""],
<a name="line_9"></a>
<a name="line_10"></a> ["Field","z0"::CC1,"The centre around which we expand various power series."],
<a name="line_11"></a>
<a name="line_12"></a> ["Field","ss0"::procedure,"The series @ff0@ and @ff1@ satisfy $f''+s f/2=0$, where $s$ is @ss0 + d * ss1@.  Note that we are primarily interested in the function @s0(z)=ss0(z-z0)@ rather than @ss0@ itself."],
<a name="line_13"></a> ["Field","ss1"::procedure,"The series @ff0@ and @ff1@ satisfy $f''+s f/2=0$, where $s$ is @ss0 + d * ss1@.  Note that we are primarily interested in the function @s1(z)=ss1(z-z0)@ rather than @ss1@ itself."],
<a name="line_14"></a> ["Field","ff0"::procedure,"The series @ff0@ satisfies $f''+s f/2=0$ with $f(z)=1+O(z^2)$.  Note that we are primarily interested in the function @f0(z)=ff0(z-z0)@ rather than @ff0@ itself."],
<a name="line_15"></a> ["Field","ff1"::procedure,"The series @ff1@ satisfies $f''+s f/2=0$ with $f(z)=z+O(z^2)$.  Note that we are primarily interested in the function @f1(z)=ff1(z-z0)@ rather than @ff1@ itself."],
<a name="line_16"></a> ["Field","M"::Matrix,"This should be a $2\\times 2$ invertible complex matrix, to be interpreted as follows.  We have solutions $f_0$ and $f_1$ satisfying $f_i=(z-z_0)^i+O((z-z_0)^2)$, and there are also solutions $f_{00}$ and $f_{01}$ satisfying $f_{0i}=z^i+O(z^2)$.  The @M@ field should be set so that $(f_0,f_1)=M.(f_{00},f_{01})$."],
<a name="line_17"></a> ["Field","index"::integer,"Objects of the class @P_to_H_map@ will maintain a table of charts, identified by an integer index, which is stored in this field."],
<a name="line_18"></a> ["Field","parent_index"::integer,"This is the index of another chart, whose centre is closer to the origin.  We will set the @M@ field of this chart by comparing it with the parent chart, whose @M@ field will have been calculated already.  (The induction starts with the chart of index zero, which is centred at the origin and has @M@ equal to the identity matrix.)"],
<a name="line_19"></a>
<a name="line_20"></a> ["Method","ss"::procedure,"",
<a name="line_21"></a>  proc(this)
<a name="line_22"></a>   local z;
<a name="line_23"></a>   unapply(expand(this["ss0"](z) + this["d"] * this["ss1"](z)),z);
<a name="line_24"></a>  end
<a name="line_25"></a> ],
<a name="line_26"></a>
<a name="line_27"></a> ["Method","s0"::CC,"",
<a name="line_28"></a>  proc(this,z::CC) this["ss0"](z-this["z0"]); end
<a name="line_29"></a> ],
<a name="line_30"></a>
<a name="line_31"></a> ["Method","s1"::CC,"",
<a name="line_32"></a>  proc(this,z::CC) this["ss1"](z-this["z0"]); end
<a name="line_33"></a> ],
<a name="line_34"></a>
<a name="line_35"></a> ["Method","s"::CC,"",
<a name="line_36"></a>  proc(this,z::CC) this["ss"](z-this["z0"]); end
<a name="line_37"></a> ],
<a name="line_38"></a>
<a name="line_39"></a> ["Method","f0"::CC,"",
<a name="line_40"></a>  proc(this,z::CC) this["ff0"](z-this["z0"]); end
<a name="line_41"></a> ],
<a name="line_42"></a>
<a name="line_43"></a> ["Method","f1"::CC,"",
<a name="line_44"></a>  proc(this,z::CC) this["ff1"](z-this["z0"]); end
<a name="line_45"></a> ],
<a name="line_46"></a>
<a name="line_47"></a> ["Method","df0"::CC,"",
<a name="line_48"></a>  proc(this,z::CC) 
<a name="line_49"></a>   local w;
<a name="line_50"></a>   evalf(subs(w = z,diff(this["ff0"](w-this["z0"]),w)));
<a name="line_51"></a>  end
<a name="line_52"></a> ],
<a name="line_53"></a>
<a name="line_54"></a> ["Method","df1"::CC,"",
<a name="line_55"></a>  proc(this,z::CC) 
<a name="line_56"></a>   local w;
<a name="line_57"></a>   evalf(subs(w = z,diff(this["ff1"](w-this["z0"]),w)));
<a name="line_58"></a>  end
<a name="line_59"></a> ],
<a name="line_60"></a>
<a name="line_61"></a> ["Method","f00"::CC,"",
<a name="line_62"></a>  proc(this,z::CC)
<a name="line_63"></a>   ((1/this["M"]) . <this["f0",z],this["f1",z]>)[1];
<a name="line_64"></a>  end
<a name="line_65"></a> ],
<a name="line_66"></a>
<a name="line_67"></a> ["Method","f01"::CC,"",
<a name="line_68"></a>  proc(this,z::CC)
<a name="line_69"></a>   ((1/this["M"]) . <this["f0",z],this["f1",z]>)[2];
<a name="line_70"></a>  end
<a name="line_71"></a> ],
<a name="line_72"></a>
<a name="line_73"></a> ["Method","g"::CC,
<a name="line_74"></a>  "This is a function of the form $z+O(z^3)$, whose schwarzian derivative is $s$.",
<a name="line_75"></a>  proc(this,z::CC)
<a name="line_76"></a>   local v;
<a name="line_77"></a>   v := ((1/this["M"]) . <this["f0",z],this["f1",z]>);
<a name="line_78"></a>   v[2]/v[1];
<a name="line_79"></a>  end
<a name="line_80"></a> ],
<a name="line_81"></a>
<a name="line_82"></a> ["Method","set_a_P"::void,"This sets the @a_P@ field, and also sets the @d@ field to a value that is approximately correct.",
<a name="line_83"></a>  proc(this,a,d_)
<a name="line_84"></a>   this["a_P"] := a;
<a name="line_85"></a>   if nargs > 2 then
<a name="line_86"></a>    this["d"] := d_
<a name="line_87"></a>   else
<a name="line_88"></a>    this["d"] := schwarz_d_approx(a);
<a name="line_89"></a>   fi;
<a name="line_90"></a>
<a name="line_91"></a>   NULL;
<a name="line_92"></a>  end
<a name="line_93"></a> ],
<a name="line_94"></a>
<a name="line_95"></a> ["Method","set_s"::void,"This method sets the fields @ss0@ and @ss1@.  Note that these are independent of @d@, so we do not need to use this method again if we choose a new value of @d@.",
<a name="line_96"></a>  proc(this)
<a name="line_97"></a>   local z,s0,s1;
<a name="line_98"></a>
<a name="line_99"></a>   s0 := evalf(subs(a_P = this["a_P"],S0_p1_inv(z+this["z0"])));
<a name="line_100"></a>   s1 := evalf(subs(a_P = this["a_P"],S1_p1_inv(z+this["z0"])));
<a name="line_101"></a>   s0 := convert(series(s0,z=0,this["degree"]+3),polynom,z);
<a name="line_102"></a>   s1 := convert(series(s1,z=0,this["degree"]+3),polynom,z);
<a name="line_103"></a>   this["ss0"] := unapply(s0,z);
<a name="line_104"></a>   this["ss1"] := unapply(s1,z);
<a name="line_105"></a>
<a name="line_106"></a>   NULL;
<a name="line_107"></a>  end
<a name="line_108"></a> ],
<a name="line_109"></a>
<a name="line_110"></a> ["Method","set_f"::void,
<a name="line_111"></a>  "This sets the fields @ff0@ and @ff1@",
<a name="line_112"></a>  proc(this)
<a name="line_113"></a>   local z,n,s,a,b,c,i,l;
<a name="line_114"></a>
<a name="line_115"></a>   n := this["degree"];
<a name="line_116"></a>   s := expand(this["ss0"](z) + this["d"] * this["ss1"](z));
<a name="line_117"></a>   c := table():
<a name="line_118"></a>   for i from 0 to n+3 do c[i] := coeff(s,z,i); od:
<a name="line_119"></a>
<a name="line_120"></a>   a[0] := 1;
<a name="line_121"></a>   a[1] := 0;
<a name="line_122"></a>   for l from 0 to n-1 do
<a name="line_123"></a>    a[l+2] := -add(a[i]*c[l-i],i=0..l)/2/(l+1)/(l+2);
<a name="line_124"></a>   od:
<a name="line_125"></a>   this["ff0"] := unapply(add(a[i]*z^i,i=0..n+1),z);
<a name="line_126"></a>
<a name="line_127"></a>   b[0] := 0;
<a name="line_128"></a>   b[1] := 1;
<a name="line_129"></a>   for l from 0 to n-1 do
<a name="line_130"></a>    b[l+2] := -add(b[i]*c[l-i],i=0..l)/2/(l+1)/(l+2);
<a name="line_131"></a>   od:
<a name="line_132"></a>   this["ff1"] := unapply(add(b[i]*z^i,i=0..n+1),z);
<a name="line_133"></a> 
<a name="line_134"></a>   if this["z0"] = 0 then this["M"] := IdentityMatrix(2); fi;
<a name="line_135"></a>
<a name="line_136"></a>   NULL;   
<a name="line_137"></a>  end
<a name="line_138"></a> ],
<a name="line_139"></a>
<a name="line_140"></a> ["Method","propagate_M"::void,
<a name="line_141"></a>  "This method sets the @M@ field of this chart based on the @M@ field for another chart @C@ (whose centre should be close to the centre of this chart)",
<a name="line_142"></a>  proc(this,C::P_to_H_chart)
<a name="line_143"></a>   local z,f0,f1,df0,df1,N;
<a name="line_144"></a>   # Note: we could do this by evaluating f0 and f1 from this chart, or from C.
<a name="line_145"></a>   # The latter is better because the functions from C will usually have a 
<a name="line_146"></a>   # larger radius of convergence.
<a name="line_147"></a>   f0  := C["f0", this["z0"]];
<a name="line_148"></a>   f1  := C["f1", this["z0"]];
<a name="line_149"></a>   df0 := C["df0",this["z0"]];
<a name="line_150"></a>   df1 := C["df1",this["z0"]];
<a name="line_151"></a>   N := <<f0|df0>,<f1|df1>>;
<a name="line_152"></a>   this["M"] := (1/N) . C["M"];
<a name="line_153"></a>   NULL;
<a name="line_154"></a>  end
<a name="line_155"></a> ]
<a name="line_156"></a>):
<a name="line_157"></a>
<a name="line_158"></a>######################################################################
<a name="line_159"></a>
<a name="line_160"></a><a name="CLASS_P_to_H_map"></a><span style="color:red">#@ CLASS: P_to_H_map
</span><a name="line_161"></a>
<a name="line_162"></a>`Class/Declare`(
<a name="line_163"></a> "P_to_H_map",
<a name="line_164"></a> "An instance of this class encapsulates data about a cromulent isomorphism PX(a_P) -> HX(a_PH) for some specific numerical values of a_H and a_P",
<a name="line_165"></a>
<a name="line_166"></a> ["Field","a_H"::RR1,""],
<a name="line_167"></a> ["Field","a_P"::RR1,""],
<a name="line_168"></a> ["Field","degree"::posint=20,"The degree of polynomials used for approximate power series."],
<a name="line_169"></a> ["Field","d"::RR1,"This is the number $d$ that enters into the formula for the schwarzian derivative of $p^{-1}$"],
<a name="line_170"></a> ["Field","test_point"::CC],
<a name="line_171"></a>
<a name="line_172"></a> ["Field","ap_H"::RR1],
<a name="line_173"></a> ["Field","am_H"::RR1],
<a name="line_174"></a> ["Field","v_HS"::table,"Table containing the points $\\psi^{-1}(v_{Hi})$"],
<a name="line_175"></a> ["Field","v_PS"::table,"Table containing the points $\\phi(v_{Pi})$"],
<a name="line_176"></a> ["Field","c_HS"::table,"Table containing the curves $\\psi^{-1}(c_{Hi}(t))$"],
<a name="line_177"></a> ["Field","c_PS"::table,"Table containing the curves $\\phi(c_{Pi}(t))$"],
<a name="line_178"></a> ["Field","psi"::procedure],
<a name="line_179"></a> ["Field","psi_inv"::procedure],
<a name="line_180"></a> ["Field","phi"::procedure],
<a name="line_181"></a> ["Field","phi_inv"::procedure],
<a name="line_182"></a>
<a name="line_183"></a> ["Field","c"::RR1,"The coefficient of $z$ in $p_1^{-1}(z)$"],
<a name="line_184"></a> ["Field","num_charts"::integer = 0],
<a name="line_185"></a> ["Field","charts"::table],
<a name="line_186"></a> ["Field","zero_chart"::P_to_H_chart],
<a name="line_187"></a> ["Field","unit_chart"::P_to_H_chart],
<a name="line_188"></a> ["Field","test_chart"::P_to_H_chart],
<a name="line_189"></a> ["Field","err"::RR1],
<a name="line_190"></a> ["Field","errs"::table],
<a name="line_191"></a> ["Field","p1_inv"::procedure],
<a name="line_192"></a> ["Field","p1"::procedure],
<a name="line_193"></a>
<a name="line_194"></a> ["Method","set_a_H"::RR1,
<a name="line_195"></a>  "Set the @a_H@ field and perform associated bookkeeping.  Normally one should only use the @set_a_P@ method directly; then other code will calculate an appropriate value for @a_H@ and call the @set_a_H@ method.",
<a name="line_196"></a>  proc(this,a::RR0)
<a name="line_197"></a>   local i;
<a name="line_198"></a>
<a name="line_199"></a>   this["a_H"]  := evalf(a);
<a name="line_200"></a>   this["am_H"] := evalf(sqrt(1-a^2));
<a name="line_201"></a>   this["ap_H"] := evalf(sqrt(1+a^2));
<a name="line_202"></a>
<a name="line_203"></a>   this["v_HS"] := table();
<a name="line_204"></a>   for i in [0,1,2,3,6,10,11,12,13] do 
<a name="line_205"></a>    this["v_HS"][i] := evalf(subs(a_H = a,v_HS[i]));
<a name="line_206"></a>   od;
<a name="line_207"></a>   this["c_HS"] := table();
<a name="line_208"></a>   for i in [0,1,3,5] do
<a name="line_209"></a>    this["c_HS"][i] := unapply(evalf(subs(a_H = a,c_HS[i](t))),t);
<a name="line_210"></a>   od;
<a name="line_211"></a>   this["psi"]     := unapply(evalf(subs(a_H = a,schwarz_psi(z))),z);
<a name="line_212"></a>   this["psi_inv"] := unapply(evalf(subs(a_H = a,schwarz_psi_inv(z))),z);
<a name="line_213"></a>
<a name="line_214"></a>   return this["a_H"];
<a name="line_215"></a>  end
<a name="line_216"></a> ],
<a name="line_217"></a>
<a name="line_218"></a> ["Method","set_a_P"::void,
<a name="line_219"></a>  "Set the @a_H@ field and perform associated bookkeeping.",
<a name="line_220"></a>  proc(this,a::RR0,d_)
<a name="line_221"></a>   local i,n,theta,CC,C;
<a name="line_222"></a>
<a name="line_223"></a>   this["a_P"] := evalf(a);
<a name="line_224"></a>
<a name="line_225"></a>   this["v_PS"] := table();
<a name="line_226"></a>   for i in [0,1,2,3,6,10,11,12,13] do 
<a name="line_227"></a>    this["v_PS"][i] := evalf(subs(a_P = a,v_PS[i]));
<a name="line_228"></a>   od;
<a name="line_229"></a>   this["c_PS"] := table();
<a name="line_230"></a>   for i in [0,1,3,5] do
<a name="line_231"></a>    this["c_PS"][i] := unapply(evalf(subs(a_P = a,c_PS[i](t))),t);
<a name="line_232"></a>   od;
<a name="line_233"></a>   this["phi"]     := eval(schwarz_phi);
<a name="line_234"></a>   this["phi_inv"] := eval(schwarz_phi_inv);
<a name="line_235"></a>
<a name="line_236"></a>   theta := argument(this["v_PS"][11]):
<a name="line_237"></a>
<a name="line_238"></a>   this["test_point"] :=  evalf(exp(I*(theta/2)));
<a name="line_239"></a>
<a name="line_240"></a>   this["errs"] := table();
<a name="line_241"></a>
<a name="line_242"></a>   if nargs > 2 then
<a name="line_243"></a>    this["d"] := d_
<a name="line_244"></a>   else
<a name="line_245"></a>    this["d"] := schwarz_d_approx(a);
<a name="line_246"></a>   fi;
<a name="line_247"></a>   CC := eval(this["charts"]);
<a name="line_248"></a>   n := this["num_charts"];
<a name="line_249"></a>   for i from 0 to n-1 do 
<a name="line_250"></a>    CC[i]["set_a_P",this["a_P"],this["d"]];
<a name="line_251"></a>   od;
<a name="line_252"></a>   NULL;
<a name="line_253"></a>  end
<a name="line_254"></a> ],
<a name="line_255"></a>
<a name="line_256"></a> ["Method","add_chart"::P_to_H_chart,
<a name="line_257"></a>  "Create and add a new chart centred at @z0@",
<a name="line_258"></a>  proc(this,z0::CC)
<a name="line_259"></a>   local C,n;
<a name="line_260"></a>
<a name="line_261"></a>   C := eval(`new/P_to_H_chart`());
<a name="line_262"></a>   C["set_a_P",this["a_P"],this["d"]];
<a name="line_263"></a>   C["degree"] := this["degree"];
<a name="line_264"></a>   C["z0"] := z0;
<a name="line_265"></a>   C["set_s"];
<a name="line_266"></a>   if not(type(this["charts"],table)) then
<a name="line_267"></a>    this["charts"] := table():
<a name="line_268"></a>   fi;
<a name="line_269"></a>   n := this["num_charts"];
<a name="line_270"></a>   C["index"] := n;
<a name="line_271"></a>   this["charts"][n] := eval(C);
<a name="line_272"></a>   this["num_charts"] := n+1;
<a name="line_273"></a>
<a name="line_274"></a>   userinfo(5,genus2,sprintf("Added chart %d centred at %A",n,evalf[5](z0)));
<a name="line_275"></a>   return eval(C);
<a name="line_276"></a>  end
<a name="line_277"></a> ],
<a name="line_278"></a>
<a name="line_279"></a> ["Method","add_charts"::void,
<a name="line_280"></a>  "Create and add $2n+1$ new charts.  Of these, one is centred at the origin, $n$ are centred at equally spaced points between $0$ and $i$, and $n$ are centred at equally spaed points between $0$ and @test_point@.",
<a name="line_281"></a>  proc(this,n::posint := 10)
<a name="line_282"></a>   local i,C;
<a name="line_283"></a>
<a name="line_284"></a>   this["charts"] := table():
<a name="line_285"></a>   this["num_charts"] := 0;
<a name="line_286"></a>
<a name="line_287"></a>   C := eval(this["add_chart",0.]);
<a name="line_288"></a>   this["zero_chart"] := eval(C);
<a name="line_289"></a>
<a name="line_290"></a>   for i from 1 to n do
<a name="line_291"></a>    C := eval(this["add_chart",evalf(I*i/n)]);
<a name="line_292"></a>    C["parent_index"] := i-1;
<a name="line_293"></a>   od:
<a name="line_294"></a>   this["unit_chart"] := eval(this["charts"][n]);
<a name="line_295"></a>
<a name="line_296"></a>   for i from 1 to n do
<a name="line_297"></a>    C := eval(this["add_chart",evalf(i/n * this["test_point"])]);
<a name="line_298"></a>    if i = 1 then
<a name="line_299"></a>     C["parent_index"] := 0;
<a name="line_300"></a>    else
<a name="line_301"></a>     C["parent_index"] := n+i-1;
<a name="line_302"></a>    fi;
<a name="line_303"></a>   od:
<a name="line_304"></a>   this["test_chart"] := eval(this["charts"][2*n]);
<a name="line_305"></a>   NULL;
<a name="line_306"></a>  end
<a name="line_307"></a> ],
<a name="line_308"></a>
<a name="line_309"></a> ["Method","set_d"::RR1,
<a name="line_310"></a>  "This sets the @d@ field, then sets the @M@ fields of all charts based on that, then calculates @c@ and @a_H@ and @p1_inv@ as described in the text.  It returns an error term, which should be zero if we have the correct value of @d@.",
<a name="line_311"></a>  proc(this,d::RR1)
<a name="line_312"></a>   local i,j,n,CC,C,v,g,z,t,u0,u1,u2,p1i;
<a name="line_313"></a>
<a name="line_314"></a>   this["d"] := evalf(d);
<a name="line_315"></a>   userinfo(5,genus2,sprintf("set_d(%A)",evalf[5](this["d"])));
<a name="line_316"></a>
<a name="line_317"></a>   n := this["num_charts"];
<a name="line_318"></a>   CC := eval(this["charts"]);
<a name="line_319"></a>   for i from 0 to n-1 do
<a name="line_320"></a>    C := eval(CC[i]);
<a name="line_321"></a>    C["d"] := evalf(d);
<a name="line_322"></a>    C["set_f"];
<a name="line_323"></a>    if i = 0 then
<a name="line_324"></a>     C["M"] := IdentityMatrix(2);
<a name="line_325"></a>     userinfo(6,genus2,"set_d for chart 0");
<a name="line_326"></a>    else
<a name="line_327"></a>     j := C["parent_index"];
<a name="line_328"></a>     C["propagate_M",CC[j]];
<a name="line_329"></a>     userinfo(6,genus2,sprintf("set_d for chart %d, parent is %d",i,j));
<a name="line_330"></a>    fi;
<a name="line_331"></a>   od;
<a name="line_332"></a>
<a name="line_333"></a>   C := eval(this["unit_chart"]);
<a name="line_334"></a>   g := C["g",I*exp(I*t)]/I;
<a name="line_335"></a>   g := convert(series(g,t=0,3),polynom,t);
<a name="line_336"></a>   u0 := Re(coeff(g,t,0));
<a name="line_337"></a>   u1 := Re(coeff(g,t,1)/I);
<a name="line_338"></a>   u2 := Re(coeff(g,t,2));
<a name="line_339"></a>   this["c"] := sqrt(u2/(u0*(u0*u2+u1^2)));
<a name="line_340"></a>   this["set_a_H", u1^2/sqrt(8*u0*u2*(u0*u2+u1^2)+u1^4)];
<a name="line_341"></a>
<a name="line_342"></a>   C := eval(this["zero_chart"]);
<a name="line_343"></a>   p1i := this["c"] * C["g",z];
<a name="line_344"></a>   p1i := convert(series(p1i,z=0,this["degree"]),polynom,z);
<a name="line_345"></a>
<a name="line_346"></a>   this["p1_inv"] := unapply(p1i,z);
<a name="line_347"></a>
<a name="line_348"></a>   C := eval(this["test_chart"]);
<a name="line_349"></a>   g := this["c"] * C["g",C["z0"]];
<a name="line_350"></a>   this["err"] := Im(this["psi"](g));
<a name="line_351"></a>
<a name="line_352"></a>   this["errs"][this["d"]] := this["err"];
<a name="line_353"></a>
<a name="line_354"></a>   userinfo(5,genus2,sprintf("log[10](|err|) = %A",evalf[5](log[10](abs(this["err"])))));
<a name="line_355"></a>
<a name="line_356"></a>   return this["err"];
<a name="line_357"></a>  end
<a name="line_358"></a> ],
<a name="line_359"></a>
<a name="line_360"></a> ["Method","find_p1_inv"::void,
<a name="line_361"></a>  "This calls the @set_d@ method repeatedly with different values of @d@, searching for the value that makes the @set_d@ error term equal to zero.  It also sets the fields @c@, @a_H@ and @p1_inv@.",
<a name="line_362"></a>  proc(this,tol := 10.^(-20),gap := 1)
<a name="line_363"></a>   local F,d0;
<a name="line_364"></a>
<a name="line_365"></a>   F := proc(d) 
<a name="line_366"></a>    local T;
<a name="line_367"></a>    `P_to_H_map!set_d`(this,d);
<a name="line_368"></a>    T := table():
<a name="line_369"></a>    T["err"] := this["err"];
<a name="line_370"></a>    return eval(T);
<a name="line_371"></a>   end:
<a name="line_372"></a>
<a name="line_373"></a>   d0 := this["d"];
<a name="line_374"></a>   if not type(d0,RR1) then
<a name="line_375"></a>    d0 := schwarz_d_approx(this["a_H"]);
<a name="line_376"></a>   fi;
<a name="line_377"></a>
<a name="line_378"></a>   brent_fsolve(F,d0-gap,d0+gap,false,false,tol);
<a name="line_379"></a>   NULL;
<a name="line_380"></a>  end
<a name="line_381"></a> ],
<a name="line_382"></a>
<a name="line_383"></a> ["Method","set_p1"::void,
<a name="line_384"></a>  "This sets the @p1@ field based on the @p1_inv@ field.",
<a name="line_385"></a>  proc(this)
<a name="line_386"></a>   local z,p1,p1i;
<a name="line_387"></a>
<a name="line_388"></a>   p1i := this["p1_inv"](z);
<a name="line_389"></a>   p1 := revert_series(p1i,z);
<a name="line_390"></a>   this["p1"] := unapply(p1,z);
<a name="line_391"></a>
<a name="line_392"></a>   NULL;
<a name="line_393"></a>  end
<a name="line_394"></a> ],
<a name="line_395"></a>
<a name="line_396"></a> ["Method","p1_coeff_plot"::plot,
<a name="line_397"></a>  "This generates a plot of the logs of the absolute values of the coefficients of the power series for $p_1(z)$",
<a name="line_398"></a>  proc(this)
<a name="line_399"></a>   local p1,d,z;
<a name="line_400"></a>
<a name="line_401"></a>   p1 := this["p1"](z):
<a name="line_402"></a>   d := (degree(p1,z)-1)/2;
<a name="line_403"></a>   display(seq(point([2*i+1,log(abs(coeff(p1,z,2*i+1)))]),i=0..d));
<a name="line_404"></a>  end
<a name="line_405"></a> ],
<a name="line_406"></a>
<a name="line_407"></a> ["Method","p1_inv_coeff_plot",
<a name="line_408"></a>  "This generates a plot of the logs of the absolute values of the coefficients of the power series for $p^{-1}_1(z)$",
<a name="line_409"></a>  proc(this)
<a name="line_410"></a>   local i,d,z,p1i,cols;
<a name="line_411"></a>
<a name="line_412"></a>   cols := ["Red","YellowGreen","BlueViolet","Gold","Olive","Tomato","Lime",
<a name="line_413"></a>            "VioletRed","DeepSkyBlue","DarkOrange","Cyan","Magenta","Goldenrod",
<a name="line_414"></a>            "Turquoise","ForestGreen","DarkOrchid"];
<a name="line_415"></a>
<a name="line_416"></a>   p1i := this["p1_inv"](z):
<a name="line_417"></a>   d := (degree(p1i,z)-1)/2;
<a name="line_418"></a>   display(seq(point([2*i+1,log(abs(coeff(p1i,z,2*i+1)))],
<a name="line_419"></a>                     colour=cols[modp(i,16)+1],
<a name="line_420"></a>                     symbol=solidcircle),
<a name="line_421"></a>               i=0..d));
<a name="line_422"></a>
<a name="line_423"></a>  end
<a name="line_424"></a> ]
<a name="line_425"></a>):
<a name="line_426"></a>
<a name="line_427"></a>######################################################################
<a name="line_428"></a>######################################################################
<a name="line_429"></a>######################################################################
<a name="line_430"></a>######################################################################
<a name="line_431"></a>
<a name="line_432"></a># Here is some older code for schwarzian solutions near a critical point.
<a name="line_433"></a># Ideally, it should be incorporated in the above framework.
<a name="line_434"></a>
<a name="line_435"></a># # For technical reasons this returns power series in z, but they should
<a name="line_436"></a># # really be regarded as power series in (z-alpha).
<a name="line_437"></a># schwarz_critical_basic_solutions := proc(a0,d0)
<a name="line_438"></a>#  local ss,c,g0,g1,err,sol;
<a name="line_439"></a># 
<a name="line_440"></a>#  ss := simplify(S_p1_inv(z+schwarz_alpha)):
<a name="line_441"></a>#  ss := evalf(subs({a_P = a0,d_p_inv = d0},ss));
<a name="line_442"></a>#  ss := convert(series(ss,z=0,2*p1_inv_N),polynom,z):
<a name="line_443"></a># 
<a name="line_444"></a>#  # This funny sequence seems to be necessary to remove a tiny imaginary 
<a name="line_445"></a>#  # part from the coefficient of z^(-2).
<a name="line_446"></a>#  c  := coeff(ss,z,-2):
<a name="line_447"></a>#  ss := expand(ss - c*z^(-2)):
<a name="line_448"></a>#  ss := expand(ss + Re(c)*z^(-2)):
<a name="line_449"></a># 
<a name="line_450"></a>#  g0 := z^(1/4) + add(aa0[k]*z^(k+1/4),k=1..2*p1_inv_N+1):
<a name="line_451"></a>#  err := convert(series(expand((diff(g0,z,z)+ss*g0/2)/z^(1/4)),z=0,2*p1_inv_N),polynom,z):
<a name="line_452"></a>#  err := err - coeff(err,z,-2)/z^2: # should be zero already, but there is a small numerical error
<a name="line_453"></a>#  sol := solve({coeffs(err,z)}):
<a name="line_454"></a>#  g0 := expand(z^(-1/4) * subs(sol,g0)):
<a name="line_455"></a># 
<a name="line_456"></a>#  g1 := z^(3/4) + add(aa0[k]*z^(k+3/4),k=1..2*p1_inv_N+1):
<a name="line_457"></a>#  err := convert(series(expand((diff(g1,z,z)+ss*g1/2)/z^(3/4)),z=0,2*p1_inv_N),polynom,z):
<a name="line_458"></a>#  err := err - coeff(err,z,-2)/z^2: # should be zero already, but there is a small numerical error
<a name="line_459"></a>#  sol := solve({coeffs(err,z)}):
<a name="line_460"></a>#  g1 := expand(z^(-3/4) * subs(sol,g1)):
<a name="line_461"></a># 
<a name="line_462"></a>#  return([g0,g1]);
<a name="line_463"></a># end:
<a name="line_464"></a># 
<a name="line_465"></a># schwarz_critical_rebase := proc(a0,d0,z0,f)
<a name="line_466"></a>#  local c0,c1,c2,alpha0,g0,g1,g0o,g1o,ff,ffo,p0,p1,p2,err,sol;
<a name="line_467"></a>#  c0 := subs(z=z0,f);
<a name="line_468"></a>#  c1 := subs(z=z0,diff(f,z));
<a name="line_469"></a>#  c2 := subs(z=z0,diff(f,z,z));
<a name="line_470"></a>#  g0o,g1o := op(schwarz_critical_basic_solutions(a0,d0));
<a name="line_471"></a>#  alpha0 := evalf(subs(a_P = a0,schwarz_alpha));
<a name="line_472"></a>#  g0 := subs(z = z+z0-alpha0,g0o);
<a name="line_473"></a>#  g1 := subs(z = z+z0-alpha0,g1o);
<a name="line_474"></a># 
<a name="line_475"></a>#  ffo := (p0*g0o+p1*g1o*sqrt(z))/(g0o+p2*g1o*sqrt(z)):
<a name="line_476"></a>#  err := (p0*g0+p1*g1*sqrt(z+z0-alpha0)) - (c0+c1*z+c2*z^2/2)*(g0+p2*g1*sqrt(z+z0-alpha0)):
<a name="line_477"></a>#  err := convert(series(err,z=0,3),polynom,z);
<a name="line_478"></a>#  sol := solve({coeffs(err,z)},{p0,p1,p2});
<a name="line_479"></a>#  ffo := subs(sol,ffo):
<a name="line_480"></a>#  ffo := convert(series(ffo,z=0,2*p1_inv_N),polynom,z):
<a name="line_481"></a>#  ffo := simplify(subs(z=t^2,ffo),symbolic):
<a name="line_482"></a>#  
<a name="line_483"></a>#  return(subs(t = -I*sqrt(alpha0-z),ffo));
<a name="line_484"></a># end:
  </pre>
 </body>
</html>
    