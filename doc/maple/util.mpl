<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>with(plots): 
<a name="line_2"></a>with(plottools): 
<a name="line_3"></a>with(LinearAlgebra): 
<a name="line_4"></a>with(Groebner):
<a name="line_5"></a>with(Optimization):
<a name="line_6"></a>_EnvExplicit := true;
<a name="line_7"></a>Digits := 100;
<a name="line_8"></a>
<a name="line_9"></a>######################################################################
<a name="line_10"></a># This block of code defines the function _ASSERT(p,s).  The normal
<a name="line_11"></a># setup is that executing _ASSERT(p,s) continues silently if p 
<a name="line_12"></a># evaluates to true, but generates an error including the string s 
<a name="line_13"></a># if p is false.  If assert_verbosely is true then a message is
<a name="line_14"></a># generated even if the test is passed.  If assert_stop_on_fail is
<a name="line_15"></a># false then failed assertions still generate a message but the 
<a name="line_16"></a># maple error() command is not used, so execution will usually 
<a name="line_17"></a># continue.
<a name="line_18"></a>
<a name="line_19"></a>kernelopts(assertlevel=1):
<a name="line_20"></a>assert_count := 0;
<a name="line_21"></a>assert_verbosely := false;
<a name="line_22"></a>assert_stop_on_fail := true;
<a name="line_23"></a>
<a name="line_24"></a><span style="color:red">#@ _ASSERT 
</span><a name="line_25"></a>_ASSERT := proc(p,s::string)
<a name="line_26"></a> global assert_count,assert_verbosely,assert_stop_on_fail,assert_fail_data;
<a name="line_27"></a> assert_count := assert_count+1;
<a name="line_28"></a> if assert_verbosely then printf("Checking: %s\n",s); fi;
<a name="line_29"></a> if not p then
<a name="line_30"></a>  assert_fail_data := args[3..-1];
<a name="line_31"></a>  if assert_stop_on_fail then
<a name="line_32"></a>   error cat("Assertion failed: ",s);
<a name="line_33"></a>  else
<a name="line_34"></a>   printf(cat("Assertion failed: ",s,"\n"));
<a name="line_35"></a>  fi;
<a name="line_36"></a> fi;
<a name="line_37"></a>end:
<a name="line_38"></a>
<a name="line_39"></a>######################################################################
<a name="line_40"></a># When this code is executed, Maple may consider the current directory
<a name="line_41"></a># to be the directory where all the worksheets are saved, or the
<a name="line_42"></a># directory where this text file is saved, or the common parent of
<a name="line_43"></a># those two directories.  The next block of code tries to determine
<a name="line_44"></a># which of these cases applies, and sets the global variables
<a name="line_45"></a># genus2_dir,maple_dir,worksheets_dir and latex_dir accordingly.
<a name="line_46"></a>
<a name="line_47"></a><span style="color:red">#@ find_directories 
</span><a name="line_48"></a>find_directories := proc()
<a name="line_49"></a> local olddir;
<a name="line_50"></a> global genus2_dir,data_dir,maple_dir,maple_output_dir,
<a name="line_51"></a>  worksheets_dir,latex_dir,plots_dir,thumbs_dir,images_dir,doc_dir,lib_dir;
<a name="line_52"></a>
<a name="line_53"></a> genus2_dir := NULL;
<a name="line_54"></a> 
<a name="line_55"></a> if
<a name="line_56"></a>  FileTools[Exists]("../maple") and
<a name="line_57"></a>  FileTools[Exists]("../worksheets") and
<a name="line_58"></a>  FileTools[IsDirectory]("../maple") and
<a name="line_59"></a>  FileTools[IsDirectory]("../worksheets") then
<a name="line_60"></a>   olddir := currentdir("..");
<a name="line_61"></a>   genus2_dir := currentdir();
<a name="line_62"></a>  currentdir(olddir);
<a name="line_63"></a> elif
<a name="line_64"></a>  FileTools[Exists]("maple") and
<a name="line_65"></a>  FileTools[Exists]("worksheets") and
<a name="line_66"></a>  FileTools[IsDirectory]("maple") and
<a name="line_67"></a>  FileTools[IsDirectory]("worksheets") then
<a name="line_68"></a>   genus2_dir := currentdir();
<a name="line_69"></a> fi:
<a name="line_70"></a>
<a name="line_71"></a> if genus2_dir = NULL then
<a name="line_72"></a>  error("genus2 directory not found");
<a name="line_73"></a> fi;
<a name="line_74"></a>
<a name="line_75"></a> data_dir         := cat(genus2_dir,"/data");
<a name="line_76"></a> maple_dir        := cat(genus2_dir,"/maple");
<a name="line_77"></a> maple_output_dir := cat(genus2_dir,"/maple_output");
<a name="line_78"></a> worksheets_dir   := cat(genus2_dir,"/worksheets");
<a name="line_79"></a> latex_dir        := cat(genus2_dir,"/latex");
<a name="line_80"></a> plots_dir        := cat(genus2_dir,"/plots");
<a name="line_81"></a> thumbs_dir       := cat(genus2_dir,"/plots/thumbs");
<a name="line_82"></a> images_dir       := cat(genus2_dir,"/images");
<a name="line_83"></a> doc_dir          := cat(genus2_dir,"/doc");
<a name="line_84"></a> lib_dir          := cat(genus2_dir,"/lib");
<a name="line_85"></a>end:
<a name="line_86"></a>
<a name="line_87"></a>find_directories();
<a name="line_88"></a>
<a name="line_89"></a>######################################################################
<a name="line_90"></a># Simplify a complex number by setting real and imaginary parts
<a name="line_91"></a># to zero if they are smaller than some threshhold (10^(-12) by
<a name="line_92"></a># default).  This is useful because some calculations produce a
<a name="line_93"></a># tiny spurious imaginary part when the result should be real.
<a name="line_94"></a>
<a name="line_95"></a><span style="color:red">#@ trim 
</span><a name="line_96"></a>trim := proc(t,e::numeric := 10^(-12))
<a name="line_97"></a> local v,x,y;
<a name="line_98"></a> if type(t,list) or type(t,set) then
<a name="line_99"></a>  return(map(trim,t,e));
<a name="line_100"></a> fi;
<a name="line_101"></a>
<a name="line_102"></a> v := evalf(t);
<a name="line_103"></a> x := Re(v);
<a name="line_104"></a> y := Im(v);
<a name="line_105"></a> if (abs(x - round(x)) < e) then
<a name="line_106"></a>  x := round(x);
<a name="line_107"></a> fi;
<a name="line_108"></a> if (abs(y - round(y)) < e) then
<a name="line_109"></a>  y := round(y);
<a name="line_110"></a> fi;
<a name="line_111"></a> return(x + I * y);
<a name="line_112"></a>end:
<a name="line_113"></a>
<a name="line_114"></a># This is a similar function that works on algebraic expressions.
<a name="line_115"></a># It should be consolidated with trim() at some point.
<a name="line_116"></a>
<a name="line_117"></a><span style="color:red">#@ tidy 
</span><a name="line_118"></a>tidy := proc(u,e::numeric := 10.^(-90))
<a name="line_119"></a> local aa,a,c,u0;
<a name="line_120"></a>
<a name="line_121"></a> if type(u,`+`) or type(u,`*`) or type(u,list) or type(u,set) then
<a name="line_122"></a>  return(map(tidy,u,e));
<a name="line_123"></a> elif type(u,`^`) then
<a name="line_124"></a>  return(tidy(op(1,u),e)^op(2,u));
<a name="line_125"></a> elif type(u,float) then
<a name="line_126"></a>  if abs(24*u - round(24*u)) < e then
<a name="line_127"></a>   return round(24*u)/24;
<a name="line_128"></a>  else
<a name="line_129"></a>   return u;
<a name="line_130"></a>  fi;
<a name="line_131"></a> elif type(u,complex) then
<a name="line_132"></a>  return tidy(Re(u)) + I * tidy(Im(u));
<a name="line_133"></a> else
<a name="line_134"></a>  return u;
<a name="line_135"></a> fi;
<a name="line_136"></a>end:
<a name="line_137"></a>
<a name="line_138"></a><span style="color:red">#@ strip_sign 
</span><a name="line_139"></a>strip_sign := proc(v)
<a name="line_140"></a> local a,b;
<a name="line_141"></a> if type(v,rational) then
<a name="line_142"></a>  return abs(v);
<a name="line_143"></a> elif type(v,`*`) then 
<a name="line_144"></a>  a,b := selectremove(type,v,rational);
<a name="line_145"></a>  return(abs(a) * b);
<a name="line_146"></a> else 
<a name="line_147"></a>  return v;
<a name="line_148"></a> fi;
<a name="line_149"></a>end:
<a name="line_150"></a>
<a name="line_151"></a>######################################################################
<a name="line_152"></a>
<a name="line_153"></a><span style="color:red">#@ is_inexact 
</span><a name="line_154"></a>is_inexact := (x) -> hastype(x,float);
<a name="line_155"></a>
<a name="line_156"></a>######################################################################
<a name="line_157"></a># For some reason, Maple does not always apply the obvious algebraic
<a name="line_158"></a># rules for complex conjugation.  The function below is designed to
<a name="line_159"></a># fix this.
<a name="line_160"></a>
<a name="line_161"></a><span style="color:red">#@ conj 
</span><a name="line_162"></a>conj := proc(u)
<a name="line_163"></a> if type(u,`*`) or type(u,`+`) or type(u,list) then
<a name="line_164"></a>  return map(conj,u);
<a name="line_165"></a> elif type(u,`^`) then 
<a name="line_166"></a>  return conj(op(1,u))^op(2,u);
<a name="line_167"></a> else
<a name="line_168"></a>  return(conjugate(u));
<a name="line_169"></a> fi;
<a name="line_170"></a>end:
<a name="line_171"></a>
<a name="line_172"></a>######################################################################
<a name="line_173"></a># This function should be called with some additional arguments
<a name="line_174"></a># (say t_1,..,t_r) as well as the named arguments f and n.  It 
<a name="line_175"></a># assumes that f depends on t_1,..,t_r and returns a polynomial in
<a name="line_176"></a># these variables of total degree less than n, which is an
<a name="line_177"></a># approximation to f.
<a name="line_178"></a>
<a name="line_179"></a><span style="color:red">#@ multi_series 
</span><a name="line_180"></a>multi_series := proc(f,n::posint)
<a name="line_181"></a> local e,g,R;
<a name="line_182"></a>
<a name="line_183"></a> if type(f,list) or type(f,set) or type(f,Vector) or type(f,Matrix) then
<a name="line_184"></a>  return map(multi_series,f,args[2..-1]);
<a name="line_185"></a> fi;
<a name="line_186"></a> 
<a name="line_187"></a> R := [args[3..-1]];
<a name="line_188"></a> R := map(t -> (t = e*t),R);
<a name="line_189"></a> g := subs(R,f);
<a name="line_190"></a> g := series(g,e=0,n);
<a name="line_191"></a> g := expand(subs(e=1,convert(g,polynom,e)));
<a name="line_192"></a> return g;
<a name="line_193"></a>end:
<a name="line_194"></a>
<a name="line_195"></a>######################################################################
<a name="line_196"></a># revert_series(p,z,m) expects p to be a polynomial in z, with 
<a name="line_197"></a># p(0) = 0 and p'(0) <> 0.  It returns another polynomial q such that
<a name="line_198"></a># p(q(z)) = q(p(z)) = z mod z^(m+1).  If m is not supplied, it 
<a name="line_199"></a># defaults to the degree of p.
<a name="line_200"></a>#
<a name="line_201"></a># Maple can do this as a special case of solve/series, but that 
<a name="line_202"></a># seems to be extremely slow.
<a name="line_203"></a>
<a name="line_204"></a><span style="color:red">#@ revert_series 
</span><a name="line_205"></a>revert_series := proc(p,z,m_)
<a name="line_206"></a> local i,m,a,pp,q,qp;
<a name="line_207"></a>
<a name="line_208"></a> if nargs > 2 then
<a name="line_209"></a>  m := m_;
<a name="line_210"></a> else
<a name="line_211"></a>  m := degree(p,z);
<a name="line_212"></a> fi;
<a name="line_213"></a>
<a name="line_214"></a> if coeff(p,z,0) <> 0 then 
<a name="line_215"></a>  error("constant term is nonzero");
<a name="line_216"></a> fi;
<a name="line_217"></a>
<a name="line_218"></a> a := coeff(p,z,1);
<a name="line_219"></a>
<a name="line_220"></a> if a = 0 then 
<a name="line_221"></a>  error("linear term is zero");
<a name="line_222"></a> fi;
<a name="line_223"></a>
<a name="line_224"></a> q := z/a;
<a name="line_225"></a> pp := rem(p,z^(m+1),z);
<a name="line_226"></a> qp := expand(pp/a);
<a name="line_227"></a>
<a name="line_228"></a> for i from 2 to m do
<a name="line_229"></a>  pp := rem(p * pp,z^(m+1),z);
<a name="line_230"></a>  a := -coeff(qp,z,i)/coeff(pp,z,i);
<a name="line_231"></a>  q := q + a*z^i;
<a name="line_232"></a>  qp := expand(qp + a * pp);
<a name="line_233"></a> od;
<a name="line_234"></a>
<a name="line_235"></a> return q;
<a name="line_236"></a>end:
<a name="line_237"></a>
<a name="line_238"></a>######################################################################
<a name="line_239"></a># simp(x) returns x if x only involves exact numbers, but if x 
<a name="line_240"></a># contains any floating point terms then simp(x) attempts to convert
<a name="line_241"></a># everything to floating point.  Thus, if a is an unassigned symbol then
<a name="line_242"></a># have simp(a + Pi) = a + Pi but simp(a + Pi + 0.1) = a + 3.24159...
<a name="line_243"></a> 
<a name="line_244"></a><span style="color:red">#@ simp 
</span><a name="line_245"></a>simp := proc(x)
<a name="line_246"></a> if hastype(x,float) then
<a name="line_247"></a>  return(evalf(x));
<a name="line_248"></a> else
<a name="line_249"></a>  return(simplify(x));
<a name="line_250"></a> fi;
<a name="line_251"></a>end:
<a name="line_252"></a>
<a name="line_253"></a>######################################################################
<a name="line_254"></a># if a = \sum_{i\in A} 2^i and b = \sum_{i\in B} 2^i then
<a name="line_255"></a># bitwise_and(a,b) = \sum_{i\in A\cap B} 2^i.
<a name="line_256"></a>#
<a name="line_257"></a># This is only included to allow us to write certain parts of the
<a name="line_258"></a># code in a way that translates easily into C (where bitwise and
<a name="line_259"></a># is a primitive operation and is often used to calculate 
<a name="line_260"></a># intersections of small sets).
<a name="line_261"></a>
<a name="line_262"></a><span style="color:red">#@ bitwise_and 
</span><a name="line_263"></a>bitwise_and := proc(a,b)
<a name="line_264"></a> local aa,bb,c,x;
<a name="line_265"></a>
<a name="line_266"></a> aa := a;
<a name="line_267"></a> bb := b;
<a name="line_268"></a> c := 0;
<a name="line_269"></a> x := 1;
<a name="line_270"></a>
<a name="line_271"></a> while aa + bb > 0 do
<a name="line_272"></a>  if type(aa,odd) and type(bb,odd) then c := c + x; fi;
<a name="line_273"></a>  aa := iquo(aa,2);
<a name="line_274"></a>  bb := iquo(bb,2);
<a name="line_275"></a>  x := 2*x;
<a name="line_276"></a> od;
<a name="line_277"></a>
<a name="line_278"></a> return(c);
<a name="line_279"></a>end:
<a name="line_280"></a>
<a name="line_281"></a>######################################################################
<a name="line_282"></a>
<a name="line_283"></a># Find the n'th order Fourier series for a function of t
<a name="line_284"></a># that is periodic of period 2 Pi.
<a name="line_285"></a>
<a name="line_286"></a><span style="color:red">#@ fourier_series 
</span><a name="line_287"></a>fourier_series := proc(u,n_,e_)
<a name="line_288"></a> local n,e;
<a name="line_289"></a> n := `if`(nargs > 1, n_, 10);
<a name="line_290"></a> e := `if`(nargs > 2, e_, 10^(-12));
<a name="line_291"></a> trim(Re(Int(u/(2*Pi),t=0..2*Pi))) + 
<a name="line_292"></a>      add(Re(trim(Int(u*cos(k*t)/Pi,t=0..2*Pi),e))*cos(k*t),k=1..n) + 
<a name="line_293"></a>      add(Re(trim(Int(u*sin(k*t)/Pi,t=0..2*Pi),e))*sin(k*t),k=1..n);
<a name="line_294"></a>end:
<a name="line_295"></a>
<a name="line_296"></a>######################################################################
<a name="line_297"></a># Given matrices M1 and M2 of the same height, this function returns 
<a name="line_298"></a># [v,w] where M1.w is close to M2.v and |v| = 1.  This is useful if we
<a name="line_299"></a># want to approximate an arbitrary function (usually of two of 
<a name="line_300"></a># variables) by a rational function, possibly with constraints on 
<a name="line_301"></a># the numerator and denominator.  In that context, M1 will be a matrix
<a name="line_302"></a># that sends a vector of coefficients to a vector of values at some
<a name="line_303"></a># sample points, and similarly for M2.
<a name="line_304"></a>
<a name="line_305"></a><span style="color:red">#@ quot_approx 
</span><a name="line_306"></a>quot_approx := proc(M1,M2)
<a name="line_307"></a> local Q1,Q2,R1,R2,QQ,V,v,w,vv,ww;
<a name="line_308"></a> userinfo(5,genus2,"Finding QR decompositions"):
<a name="line_309"></a> Q1,R1 := QRDecomposition(M1):
<a name="line_310"></a> Q2,R2 := QRDecomposition(M2):
<a name="line_311"></a> QQ := Transpose(Q1).Q2:
<a name="line_312"></a> userinfo(5,genus2,"Finding SVD decompositions"):
<a name="line_313"></a> V := SingularValues(QQ,output=['Vt']):
<a name="line_314"></a> v := Column(Transpose(V),1):
<a name="line_315"></a> w := QQ.v:
<a name="line_316"></a> vv := (1/R2).v:
<a name="line_317"></a> ww := (1/R1).w:
<a name="line_318"></a> return([vv,ww]);
<a name="line_319"></a>end:
<a name="line_320"></a>
<a name="line_321"></a>######################################################################
<a name="line_322"></a>
<a name="line_323"></a><span style="color:red">#@ prime_factors 
</span><a name="line_324"></a>prime_factors := proc(n::posint)
<a name="line_325"></a> local F;
<a name="line_326"></a> F := ifactor(n);
<a name="line_327"></a> if type(F,`*`) then F := {op(F)}; else F := {F}; fi;
<a name="line_328"></a> F := map(u -> `if`(type(u,`^`),op(1,u),u),F);
<a name="line_329"></a> F := map(op,F);
<a name="line_330"></a> return F;
<a name="line_331"></a>end:
<a name="line_332"></a>
<a name="line_333"></a>######################################################################
<a name="line_334"></a>
<a name="line_335"></a># This function can be used to process a chunk of LaTeX code 
<a name="line_336"></a># generated by Maple.  If we call process_latex(s) then the string s
<a name="line_337"></a># is combined with the contents of the file maple_output/maple_blank.tex
<a name="line_338"></a># (which provides headers and footers) to produce the file
<a name="line_339"></a># maple_output/maple_output.tex.  This is then processed to produce
<a name="line_340"></a># maple_output/maple_output.pdf.  
<a name="line_341"></a>
<a name="line_342"></a><span style="color:red">#@ process_latex 
</span><a name="line_343"></a>process_latex := proc(s::string)
<a name="line_344"></a> local olddir,fd,header,footer,in_header,line,full_latex;
<a name="line_345"></a> global maple_dir;
<a name="line_346"></a>
<a name="line_347"></a> olddir := currentdir(maple_output_dir);
<a name="line_348"></a>
<a name="line_349"></a> fd := fopen("maple_blank.tex",READ);
<a name="line_350"></a> header := "";
<a name="line_351"></a> footer := "";
<a name="line_352"></a> in_header := true;
<a name="line_353"></a> line := readline(fd);
<a name="line_354"></a> while line <> 0 do
<a name="line_355"></a>  if length(line) >= 15 and substring(line,1..15) = "%%% INSERT HERE" then
<a name="line_356"></a>   in_header := false;
<a name="line_357"></a>  elif in_header then
<a name="line_358"></a>   header := cat(header,"\n",line);
<a name="line_359"></a>  else
<a name="line_360"></a>   footer := cat(footer,"\n",line);
<a name="line_361"></a>  fi;
<a name="line_362"></a>  line := readline(fd);
<a name="line_363"></a> od;
<a name="line_364"></a> fclose(fd);
<a name="line_365"></a> full_latex := cat(header,"\n",s,"\n",footer);
<a name="line_366"></a> fd := fopen("maple_output.tex",WRITE);
<a name="line_367"></a> fprintf(fd,"%s",full_latex);
<a name="line_368"></a> fclose(fd);
<a name="line_369"></a> ssystem("pdflatex maple_output");
<a name="line_370"></a>
<a name="line_371"></a> currentdir(olddir);
<a name="line_372"></a>end:
<a name="line_373"></a>
<a name="line_374"></a>######################################################################
<a name="line_375"></a>
<a name="line_376"></a>util_stripbackquotes :=
<a name="line_377"></a>proc(s::string)
<a name="line_378"></a> local t;
<a name="line_379"></a>
<a name="line_380"></a> t := s;
<a name="line_381"></a> if t <> "" and substring(t,1..1) = "`" then
<a name="line_382"></a>  t := substring(t,2..-1);
<a name="line_383"></a> fi;
<a name="line_384"></a> if t <> "" and substring(t,-1..-1) = "`" then
<a name="line_385"></a>  t := substring(t,1..-2);
<a name="line_386"></a> fi;
<a name="line_387"></a>
<a name="line_388"></a> RETURN(t);
<a name="line_389"></a>end:
<a name="line_390"></a>
<a name="line_391"></a>util_unixify :=
<a name="line_392"></a>proc(s::string)
<a name="line_393"></a> convert(subs(13 = NULL,convert(s,bytes)),bytes);
<a name="line_394"></a>end:
<a name="line_395"></a>
<a name="line_396"></a>######################################################################
<a name="line_397"></a>
<a name="line_398"></a>util_splitstring :=
<a name="line_399"></a>proc(s::string,c::integer) 
<a name="line_400"></a> local b,n,t,i;
<a name="line_401"></a>
<a name="line_402"></a> b := convert(s,bytes):
<a name="line_403"></a> n := nops(b):
<a name="line_404"></a> t := select((i,x,d) -> x[i] = d,[seq(i,i=1..n)],b,c):
<a name="line_405"></a> t := [0,op(t),n+1]:
<a name="line_406"></a> seq(convert(b[(t[i]+1)..(t[i+1]-1)],bytes),i=1..nops(t)-1);
<a name="line_407"></a>end:
<a name="line_408"></a>
<a name="line_409"></a>util_commasplit := proc(s) util_splitstring(s, 44); end:
<a name="line_410"></a>util_semisplit  := proc(s) util_splitstring(s, 59); end:
<a name="line_411"></a>util_colonsplit := proc(s) util_splitstring(s, 58); end:
<a name="line_412"></a>util_barsplit   := proc(s) util_splitstring(s,124); end:
<a name="line_413"></a>util_tabsplit   := proc(s) util_splitstring(s,  9); end:
<a name="line_414"></a>
<a name="line_415"></a># This splits at forward slashes and backslashes, and ignores
<a name="line_416"></a># slashes at the end
<a name="line_417"></a>util_slashsplit :=
<a name="line_418"></a>proc(s::string)
<a name="line_419"></a> local b,n,t,i;
<a name="line_420"></a>
<a name="line_421"></a> b := subs(92 = 47,convert(s,bytes)):
<a name="line_422"></a> n := nops(b):
<a name="line_423"></a> while (n > 0 and b[n] = 47) do n := n - 1; od;
<a name="line_424"></a> if (n = 0) then RETURN(); fi;
<a name="line_425"></a> t := select((i,x) -> x[i] = 47,[seq(i,i=1..n)],b):
<a name="line_426"></a> t := [0,op(t),n+1]:
<a name="line_427"></a> seq(convert(b[(t[i]+1)..(t[i+1]-1)],bytes),i=1..nops(t)-1);
<a name="line_428"></a>
<a name="line_429"></a>end:
<a name="line_430"></a>
<a name="line_431"></a>util_joinstring :=
<a name="line_432"></a>proc(c::string)
<a name="line_433"></a> local l;
<a name="line_434"></a> if nargs = 1 then RETURN(""); fi;
<a name="line_435"></a> l := map((t) -> op([sprintf("%A",t),c]),[args[2..-1]]);
<a name="line_436"></a> RETURN(cat(op(l[1..-2])));
<a name="line_437"></a>end:
<a name="line_438"></a>
<a name="line_439"></a>util_commajoin := proc() util_joinstring(",",args) end:
<a name="line_440"></a>util_colonjoin := proc() util_joinstring(":",args) end:
<a name="line_441"></a>util_barjoin   := proc() util_joinstring("|",args) end:
<a name="line_442"></a>util_tabjoin   := proc() util_joinstring(",",args) end:
<a name="line_443"></a>util_slashjoin := proc() util_joinstring("/",args) end:
<a name="line_444"></a>util_linejoin  := proc() util_joinstring("\n",args) end:
<a name="line_445"></a>
<a name="line_446"></a>######################################################################
<a name="line_447"></a>
<a name="line_448"></a>util_startswith :=
<a name="line_449"></a>proc(s::string,t::string)
<a name="line_450"></a> evalb(length(s) >= length(t) and substring(s,1..length(t)) = t);
<a name="line_451"></a>end:
<a name="line_452"></a>
<a name="line_453"></a>######################################################################
<a name="line_454"></a>
<a name="line_455"></a>util_splitfilename :=
<a name="line_456"></a>proc(file::string,
<a name="line_457"></a>     # optional
<a name="line_458"></a>     suffix_::string)
<a name="line_459"></a> local pieces,m,dirname,basename;
<a name="line_460"></a>
<a name="line_461"></a> pieces := [util_slashsplit(file)];
<a name="line_462"></a> if pieces = [] then
<a name="line_463"></a>  ERROR("Empty file name");
<a name="line_464"></a> fi;
<a name="line_465"></a>
<a name="line_466"></a> dirname := util_slashjoin(op(pieces[1..-2]));
<a name="line_467"></a> basename := pieces[-1];
<a name="line_468"></a>
<a name="line_469"></a> if nargs > 1 then
<a name="line_470"></a>  m := length(suffix_);
<a name="line_471"></a>  if length(basename) >= m and
<a name="line_472"></a>     substring(basename,(-m)..(-1)) = suffix_ then
<a name="line_473"></a>   basename := substring(basename,1..(-m-1));
<a name="line_474"></a>  fi;
<a name="line_475"></a> fi;
<a name="line_476"></a>
<a name="line_477"></a> RETURN([dirname,basename]);
<a name="line_478"></a>end:
<a name="line_479"></a>
<a name="line_480"></a>util_dirname :=
<a name="line_481"></a>proc(file::string,
<a name="line_482"></a>     # optional
<a name="line_483"></a>     suffix_::string)
<a name="line_484"></a> util_splitfilename(args)[1];
<a name="line_485"></a>end:
<a name="line_486"></a>
<a name="line_487"></a>util_basename :=
<a name="line_488"></a>proc(file::string,
<a name="line_489"></a>     # optional
<a name="line_490"></a>     suffix_::string)
<a name="line_491"></a> util_splitfilename(args)[2];
<a name="line_492"></a>end:
<a name="line_493"></a>
<a name="line_494"></a>
<a name="line_495"></a>
<a name="line_496"></a>######################################################################
<a name="line_497"></a>
<a name="line_498"></a><span style="color:red">#@ strip_worksheet
</span><a name="line_499"></a>strip_worksheet := proc(file)
<a name="line_500"></a> local doc;
<a name="line_501"></a>
<a name="line_502"></a> doc := Worksheet[ReadFile](cat(worksheets_dir,"/",file,".mw"));
<a name="line_503"></a> doc := XMLTools[ApplyElement](doc,"Output",() -> NULL):
<a name="line_504"></a> Worksheet[WriteFile](cat(worksheets_dir,"/small/",file,".mw"),doc);
<a name="line_505"></a> NULL;
<a name="line_506"></a>end:
<a name="line_507"></a>
<a name="line_508"></a><span style="color:red">#@ strip_worksheet
</span><a name="line_509"></a>strip_worksheets := proc()
<a name="line_510"></a> map(strip_worksheet,[
<a name="line_511"></a>  "build_data",
<a name="line_512"></a>  "build_data_toy",
<a name="line_513"></a>  "check_all",
<a name="line_514"></a>  "checks",
<a name="line_515"></a>  "genus2",
<a name="line_516"></a>  "genus2_pics",
<a name="line_517"></a>  "genus2_talk_pics",
<a name="line_518"></a>  "text_check"
<a name="line_519"></a> ]);
<a name="line_520"></a> NULL;
<a name="line_521"></a>end:
<a name="line_522"></a>
<a name="line_523"></a>
<a name="line_524"></a>
  </pre>
 </body>
</html>
    