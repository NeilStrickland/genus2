<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>######################################################################
<a name="line_2"></a># The declarations below allow us to specify the types of arguments
<a name="line_3"></a># to functions that we will define later.  
<a name="line_4"></a>
<a name="line_5"></a># Although type RR is intended to refer to real numbers, Maple
<a name="line_6"></a># will not test this very stringently.  It will reject sets, lists,
<a name="line_7"></a># vectors and so on, but it will accept any algebraic expression.
<a name="line_8"></a>
<a name="line_9"></a># Type RR0 will accept expressions that refer to real constants,
<a name="line_10"></a># such as sqrt(2) + exp(5) / 1.23.  It will reject expressions
<a name="line_11"></a># that involve variables, or that refer to non-real complex
<a name="line_12"></a># constants.
<a name="line_13"></a>
<a name="line_14"></a># Type RR1 will accept numeric constants like 1.2345 or 1/13, but
<a name="line_15"></a># reject symbolic expressions like Pi or sqrt(3).
<a name="line_16"></a>
<a name="line_17"></a>`type/RR`  := (u) -> type(u,algebraic);           <span style="color:red">#@ RR
</span><a name="line_18"></a>`type/RR0` := (u) -> type(u,realcons);            <span style="color:red">#@ RR0
</span><a name="line_19"></a>`type/RR1` := (u) -> type(u,numeric);             <span style="color:red">#@ RR1
</span><a name="line_20"></a>
<a name="line_21"></a># Types CC, CC0 and CC1 are analogous to RR, RR0 and RR1, but
<a name="line_22"></a># for complex numbers.
<a name="line_23"></a>
<a name="line_24"></a>`type/CC`  := (u) -> type(u,algebraic);           <span style="color:red">#@ CC
</span><a name="line_25"></a>`type/CC0` := (u) -> type(u,constant);            <span style="color:red">#@ CC0
</span><a name="line_26"></a>`type/CC1` := (u) -> type(u,complex(numeric));    <span style="color:red">#@ CC1
</span><a name="line_27"></a>
<a name="line_28"></a># Types RR_2, RR0_2 and RR1_2 are for elements of R^2, which
<a name="line_29"></a># we represent as lists.  (Note that Maple also has Vectors,
<a name="line_30"></a># which are different from lists, but for technical reasons we
<a name="line_31"></a># generally prefer to use lists.)
<a name="line_32"></a>
<a name="line_33"></a>`type/RR_2`  := (u) -> type(u,[algebraic,algebraic]); <span style="color:red">#@ RR_2
</span><a name="line_34"></a>`type/RR0_2` := (u) -> type(u,[realcons,realcons]);   <span style="color:red">#@ RR0_2
</span><a name="line_35"></a>`type/RR1_2` := (u) -> type(u,[numeric,numeric]);     <span style="color:red">#@ RR1_2
</span><a name="line_36"></a>
<a name="line_37"></a># Types RR_3, RR0_3 and RR1_3 are for elements of R^3
<a name="line_38"></a>`type/RR_3`  := (u) -> type(u,[algebraic,algebraic,algebraic]);  <span style="color:red">#@ RR_3
</span><a name="line_39"></a>`type/RR0_3` := (u) -> type(u,[realcons,realcons,realcons]);     <span style="color:red">#@ RR0_3
</span><a name="line_40"></a>`type/RR1_3` := (u) -> type(u,[numeric,numeric,numeric]);        <span style="color:red">#@ RR1_3
</span><a name="line_41"></a>
<a name="line_42"></a># Types RR_4, RR0_4 and RR1_4 are for elements of R^4
<a name="line_43"></a>`type/RR_4`  := (u) -> type(u,[algebraic,algebraic,algebraic,algebraic]); <span style="color:red">#@ RR_4
</span><a name="line_44"></a>`type/RR0_4` := (u) -> type(u,[realcons,realcons,realcons,realcons]);     <span style="color:red">#@ RR0_4
</span><a name="line_45"></a>`type/RR1_4` := (u) -> type(u,[numeric,numeric,numeric,numeric]);         <span style="color:red">#@ RR1_4
</span><a name="line_46"></a>
<a name="line_47"></a>protect(
<a name="line_48"></a> 'RR','RR0','RR1',
<a name="line_49"></a> 'CC','CC0','CC1',
<a name="line_50"></a> 'RR_2','RR0_2','RR1_2',
<a name="line_51"></a> 'RR_3','RR0_3','RR1_3',
<a name="line_52"></a> 'RR_4','RR0_4','RR1_4'
<a name="line_53"></a>);
<a name="line_54"></a>
<a name="line_55"></a>######################################################################
<a name="line_56"></a># Norms, dot products, cross products
<a name="line_57"></a>
<a name="line_58"></a># Distance between points in R^2, R^3 and R^4
<a name="line_59"></a>
<a name="line_60"></a>nm2 := (u::RR_2) -> sqrt(add(u[i]^2,i=1..2)); <span style="color:red">#@ nm2 
</span><a name="line_61"></a>nm3 := (u::RR_3) -> sqrt(add(u[i]^2,i=1..3)); <span style="color:red">#@ nm3 
</span><a name="line_62"></a>nm4 := (u::RR_4) -> sqrt(add(u[i]^2,i=1..4)); <span style="color:red">#@ nm4 
</span><a name="line_63"></a>
<a name="line_64"></a>dist := (d,a::list(RR),b::list(RR)) -> sqrt(add((a[i]-b[i])^2,i=1..d)): <span style="color:red">#@ dist 
</span><a name="line_65"></a>
<a name="line_66"></a>d2  := proc(u::RR_2 ,v::RR_2 ) sqrt(add((u[i]-v[i])^2,i=1..2)); end: <span style="color:red">#@ d2  
</span><a name="line_67"></a>d2f := proc(u::RR0_2,v::RR0_2) evalf(d2(u,v)); end:                  <span style="color:red">#@ d2f 
</span><a name="line_68"></a>d3  := proc(u::RR_3 ,v::RR_3 ) sqrt(add((u[i]-v[i])^2,i=1..3)); end: <span style="color:red">#@ d3  
</span><a name="line_69"></a>d3f := proc(u::RR0_3,v::RR0_3) evalf(d3(u,v)); end:                  <span style="color:red">#@ d3f 
</span><a name="line_70"></a>d4  := proc(u::RR_4 ,v::RR_4 ) sqrt(add((u[i]-v[i])^2,i=1..4)); end: <span style="color:red">#@ d4  
</span><a name="line_71"></a>d4f := proc(u::RR0_4,v::RR0_4) evalf(d4(u,v)); end:                  <span style="color:red">#@ d4f 
</span><a name="line_72"></a>
<a name="line_73"></a># Dot products in R^2, R^3 and R^4
<a name="line_74"></a>
<a name="line_75"></a>dp := (d,a::list(RR),b::list(RR)) -> add(a[i]*b[i],i=1..d):                <span style="color:red">#@ dp 
</span><a name="line_76"></a>dp2 := (a::RR_2,b::RR_2) -> a[1]*b[1] + a[2]*b[2]:                         <span style="color:red">#@ dp2 
</span><a name="line_77"></a>dp3 := (a::RR_3,b::RR_3) -> a[1]*b[1] + a[2]*b[2] + a[3]*b[3]:             <span style="color:red">#@ dp3 
</span><a name="line_78"></a>dp4 := (a::RR_4,b::RR_4) -> a[1]*b[1] + a[2]*b[2] + a[3]*b[3] + a[4]*b[4]: <span style="color:red">#@ dp4 
</span><a name="line_79"></a>dpv := (a::list(RR),b::list(RR)) -> `+`(op(a *~ b));                       <span style="color:red">#@ dpv 
</span><a name="line_80"></a>distv := (a::list(RR),b::list(RR)) -> sqrt(dpv(a -~ b,a -~ b));            <span style="color:red">#@ distv 
</span><a name="line_81"></a>
<a name="line_82"></a># Ordinary cross product of three-dimensional vectors.
<a name="line_83"></a>
<a name="line_84"></a><span style="color:red">#@ cross_product
</span><a name="line_85"></a>cross_product := (u::RR_3,v::RR_3) -> [
<a name="line_86"></a> u[2] * v[3] - u[3] * v[2],
<a name="line_87"></a> u[3] * v[1] - u[1] * v[3],
<a name="line_88"></a> u[1] * v[2] - u[2] * v[1]
<a name="line_89"></a>]:
<a name="line_90"></a>
<a name="line_91"></a>
<a name="line_92"></a># cross_product4(u,v,w) is a vector in R^4 depending on vectors
<a name="line_93"></a># u,v,w in R^4.  It is orthogonal to each of u, v and w.  If 
<a name="line_94"></a># u,v and w are othogonal then |cross_product4(u,v,w)| = |u||v||w|.
<a name="line_95"></a>
<a name="line_96"></a><span style="color:red">#@ cross_product4
</span><a name="line_97"></a>cross_product4 := (u::RR_4,v::RR_4,w::RR_4) -> [
<a name="line_98"></a> -u[2]*v[3]*w[4]+u[2]*v[4]*w[3]-v[2]*w[3]*u[4]+v[2]*u[3]*w[4]-w[2]*u[3]*v[4]+w[2]*v[3]*u[4], 
<a name="line_99"></a>  u[1]*v[3]*w[4]-u[1]*v[4]*w[3]+v[1]*w[3]*u[4]-v[1]*u[3]*w[4]+w[1]*u[3]*v[4]-w[1]*v[3]*u[4], 
<a name="line_100"></a> -u[1]*v[2]*w[4]+u[1]*v[4]*w[2]-v[1]*w[2]*u[4]+v[1]*u[2]*w[4]-w[1]*u[2]*v[4]+w[1]*v[2]*u[4], 
<a name="line_101"></a>  u[1]*v[2]*w[3]-u[1]*v[3]*w[2]+v[1]*w[2]*u[3]-v[1]*u[2]*w[3]+w[1]*u[2]*v[3]-w[1]*v[2]*u[3]
<a name="line_102"></a>]:
<a name="line_103"></a>
<a name="line_104"></a>######################################################################
<a name="line_105"></a># Isometric simplices
<a name="line_106"></a>
<a name="line_107"></a># Map from the 2-simplex to the equilateral triangle in R^2 with 
<a name="line_108"></a># vertices [1,0] and [-1/2,+/- sqrt(3)/2].
<a name="line_109"></a>
<a name="line_110"></a># Arguments can be given as (t1,t2) or ([t1,t2]) or (t0,t1,t2) or
<a name="line_111"></a># ([t0,t1,t2]).  If t0 is missing then it is taken to be 1-t1-t2.
<a name="line_112"></a>
<a name="line_113"></a><span style="color:red">#@ triangle_proj
</span><a name="line_114"></a>triangle_proj := proc()
<a name="line_115"></a> local tt;
<a name="line_116"></a> if nargs = 1 then tt := args[1] else tt := [args]; fi;
<a name="line_117"></a> if nops(tt) = 2 then tt = [1 - tt[1] - tt[2],tt[1],tt[2]]; fi;
<a name="line_118"></a> return([tt[1]-tt[2]/2-tt[3]/2,(tt[2]-tt[3])*sqrt(3)/2]);
<a name="line_119"></a>end:
<a name="line_120"></a>
<a name="line_121"></a><span style="color:red">#@ triangle_lift
</span><a name="line_122"></a>triangle_lift := (xy) -> [1/3+(2/3)*xy[1],
<a name="line_123"></a>                          1/3-(1/3)*xy[1]+(1/3)*sqrt(3)*xy[2], 
<a name="line_124"></a>                          1/3-(1/3)*xy[1]-(1/3)*sqrt(3)*xy[2]]:
<a name="line_125"></a>
<a name="line_126"></a>######################################################################
<a name="line_127"></a>
<a name="line_128"></a># Conversion between complex numbers and pairs of real numbers.
<a name="line_129"></a>
<a name="line_130"></a>C_to_R2 := (z) -> [Re(z),Im(z)];       <span style="color:red">#@ C_to_R2 
</span><a name="line_131"></a>R2_to_C := (xy) -> xy[1] + I * xy[2];  <span style="color:red">#@ R2_to_C 
</span><a name="line_132"></a>
<a name="line_133"></a>C_mult := (u,v) -> [u[1]*v[1]-u[2]*v[2], u[1]*v[2]+u[2]*v[1]]; <span style="color:red">#@ C_mult 
</span><a name="line_134"></a>
<a name="line_135"></a># Any polynomial f : R -> R extends in an obvious way to give a
<a name="line_136"></a># function C -> C, and by identifying C with R^2 we obtain a 
<a name="line_137"></a># function g : R^2 -> R^2.  The function below converts f to g. 
<a name="line_138"></a>
<a name="line_139"></a><span style="color:red">#@ series_on_R2
</span><a name="line_140"></a>series_on_R2 := proc(f)
<a name="line_141"></a> local t,u,ft,d,up,ans,i,c;
<a name="line_142"></a> ft := f(t);
<a name="line_143"></a> d := degree(ft,t);
<a name="line_144"></a> up := [1,0];
<a name="line_145"></a> ans := [0,0];
<a name="line_146"></a> for i from 0 to d do
<a name="line_147"></a>  c := coeff(ft,t,i);
<a name="line_148"></a>  ans := expand(ans +~ c *~ up);
<a name="line_149"></a>  up := expand([u[1] * up[1] - u[2] * up[2],u[1] * up[2] + u[2] * up[1]]);
<a name="line_150"></a> od;
<a name="line_151"></a> return unapply(ans,u);
<a name="line_152"></a>end:
<a name="line_153"></a>
<a name="line_154"></a>######################################################################
<a name="line_155"></a># Several versions of stereographic projection
<a name="line_156"></a>
<a name="line_157"></a># stereo() is the stereographic homeomorphism from 
<a name="line_158"></a># S^3 minus { [0,0,0,1] } to R^3.  The inverse homeomorphism is 
<a name="line_159"></a># unstereo(). 
<a name="line_160"></a>
<a name="line_161"></a><span style="color:red">#@ S3_to_R3
</span><a name="line_162"></a>S3_to_R3 := (x::RR_4) -> [x[1]/(1-x[4]),x[2]/(1-x[4]),x[3]/(1-x[4])];
<a name="line_163"></a>
<a name="line_164"></a><span style="color:red">#@ R3_to_S3
</span><a name="line_165"></a>R3_to_S3 := (u::RR_3) -> [seq(2*u[i]/(add(u[j]^2,j=1..3)+1),i=1..3),
<a name="line_166"></a>                          (add(u[j]^2,j=1..3)-1)/(add(u[j]^2,j=1..3)+1)];
<a name="line_167"></a>
<a name="line_168"></a>stereo   := eval(S3_to_R3); <span style="color:red">#@ stereo   
</span><a name="line_169"></a>unstereo := eval(R3_to_S3); <span style="color:red">#@ unstereo 
</span><a name="line_170"></a>
<a name="line_171"></a># Some convenience functions for plotting via stereographic projection.
<a name="line_172"></a>
<a name="line_173"></a><span style="color:red">#@ stereo_point
</span><a name="line_174"></a>stereo_point      := proc(x::RR_4) point(stereo(x),args[2..-1]); end:
<a name="line_175"></a>
<a name="line_176"></a><span style="color:red">#@ stereo_curve
</span><a name="line_177"></a>stereo_curve      := proc(xs::list(RR_4)) curve(map(stereo,xs),args[2..-1]); end:
<a name="line_178"></a>
<a name="line_179"></a><span style="color:red">#@ stereo_polygon
</span><a name="line_180"></a>stereo_polygon    := proc(xs::list(RR_4)) polygon(map(stereo,xs),args[2..-1]); end:
<a name="line_181"></a>
<a name="line_182"></a><span style="color:red">#@ stereo_spacecurve
</span><a name="line_183"></a>stereo_spacecurve := proc(x::RR_4) spacecurve(stereo(x),args[2..-1]); end:
<a name="line_184"></a>
<a name="line_185"></a><span style="color:red">#@ R_to_S1
</span><a name="line_186"></a>R_to_S1 := proc(x::RR) 
<a name="line_187"></a> if x = infinity then
<a name="line_188"></a>  return([-1,0]);
<a name="line_189"></a> else
<a name="line_190"></a>  return([(1-x^2)/(1+x^2),2*x/(1+x^2)]);
<a name="line_191"></a> fi;
<a name="line_192"></a>end:
<a name="line_193"></a>
<a name="line_194"></a><span style="color:red">#@ S1_to_R 
</span><a name="line_195"></a>S1_to_R := proc(u::RR_2) 
<a name="line_196"></a> if u[1] = -1 then
<a name="line_197"></a>  return infinity;
<a name="line_198"></a> else
<a name="line_199"></a>  return u[2]/(1 + u[1]);
<a name="line_200"></a> fi;
<a name="line_201"></a>end:
<a name="line_202"></a>
<a name="line_203"></a># SC1 is the unit circle in C (rather than R^2)
<a name="line_204"></a>
<a name="line_205"></a><span style="color:red">#@ R_to_SC1 
</span><a name="line_206"></a>R_to_SC1 := proc(x::RR)  
<a name="line_207"></a> if x = infinity then
<a name="line_208"></a>  return(-1);
<a name="line_209"></a> else
<a name="line_210"></a>  return((1-x^2+2*I*x)/(1+x^2));
<a name="line_211"></a> fi;
<a name="line_212"></a>end:
<a name="line_213"></a>
<a name="line_214"></a><span style="color:red">#@ RP1_to_SC1 
</span><a name="line_215"></a>RP1_to_SC1 := proc(xy::RR_2)  
<a name="line_216"></a> return (xy[2]^2-xy[1]^2+2*I*xy[1]*xy[2])/(xy[1]^2+xy[2]^2);
<a name="line_217"></a>end:
<a name="line_218"></a>
<a name="line_219"></a># SC1_to_R and SC1_to_R_alt are in theory the same.  However,
<a name="line_220"></a># SC1_to_R(z) is guaranteed to be real, even if |z| is slightly
<a name="line_221"></a># different from 1.  On the other hand, SC1_to_R_alt  
<a name="line_222"></a>
<a name="line_223"></a><span style="color:red">#@ SC1_to_R 
</span><a name="line_224"></a>SC1_to_R := proc(u::CC) 
<a name="line_225"></a> if u = -1 then
<a name="line_226"></a>  return infinity;
<a name="line_227"></a> else
<a name="line_228"></a>  return Im(u)/(1 + Re(u));
<a name="line_229"></a> fi;
<a name="line_230"></a>end:
<a name="line_231"></a>
<a name="line_232"></a>SC1_to_R_alt := (z) -> I*(1-z)/(1+z); <span style="color:red">#@ SC1_to_R_alt 
</span><a name="line_233"></a>
<a name="line_234"></a># I is the interval [-1,1]
<a name="line_235"></a>
<a name="line_236"></a>R_to_I   := (t) -> t/sqrt(1+t^2);             <span style="color:red">#@ R_to_I   
</span><a name="line_237"></a>I_to_R   := (t) -> t/sqrt(1-t^2);             <span style="color:red">#@ I_to_R   
</span><a name="line_238"></a>
<a name="line_239"></a>S1_to_I  := u -> u[2]/sqrt(2*(1+u[1]));       <span style="color:red">#@ S1_to_I  
</span><a name="line_240"></a>I_to_S1  := t -> [1-2*t^2,2*t*sqrt(1-t^2)];   <span style="color:red">#@ I_to_S1  
</span><a name="line_241"></a>
<a name="line_242"></a>SC1_to_I := z -> Im(z)/sqrt(2*(1+Re(z)));     <span style="color:red">#@ SC1_to_I 
</span><a name="line_243"></a>
<a name="line_244"></a># tan_sum satisfies tan_sum(tan(s),tan(t)) = tan(s+t).
<a name="line_245"></a>
<a name="line_246"></a>tan_sum := (a,b) -> (a + b)/(1 - a*b);        <span style="color:red">#@ tan_sum 
</span><a name="line_247"></a>
<a name="line_248"></a># C_to_S2 is the usual homeomorphism from the Riemann sphere to
<a name="line_249"></a># S^2, normalised to send the unit circle in C to the equator.
<a name="line_250"></a>
<a name="line_251"></a><span style="color:red">#@ C_to_S2 
</span><a name="line_252"></a>C_to_S2 := proc(z::CC) 
<a name="line_253"></a> if z = infinity then 
<a name="line_254"></a>  return([0,0,1]);
<a name="line_255"></a> else
<a name="line_256"></a>  return 
<a name="line_257"></a>   [2*Re(z)/(1+abs(z)^2),2*Im(z)/(1+abs(z)^2),(abs(z)^2-1)/(1+abs(z)^2)];
<a name="line_258"></a> fi;
<a name="line_259"></a>end:
<a name="line_260"></a>
<a name="line_261"></a><span style="color:red">#@ S2_to_C 
</span><a name="line_262"></a>S2_to_C := proc(u::RR_3) 
<a name="line_263"></a> if u[3] = 1 then
<a name="line_264"></a>  return(infinity);
<a name="line_265"></a> else 
<a name="line_266"></a>  return(simplify((u[1]+I*u[2])/(1-u[3])));
<a name="line_267"></a> fi;
<a name="line_268"></a>end:
<a name="line_269"></a>
<a name="line_270"></a><span style="color:red">#@ S2_tangent_frame
</span><a name="line_271"></a>S2_tangent_frame := proc(a::RR1_3)
<a name="line_272"></a> local e,b,c,i,j,m;
<a name="line_273"></a>
<a name="line_274"></a> e[1] := [1,0,0];
<a name="line_275"></a> e[2] := [0,1,0];
<a name="line_276"></a> e[3] := [0,0,1];
<a name="line_277"></a> i := 0;
<a name="line_278"></a> m := 0;
<a name="line_279"></a>
<a name="line_280"></a> for j from 1 to 3 do 
<a name="line_281"></a>  e[j] := e[j] - dp3(e[j],a) *~ a;
<a name="line_282"></a>  if nm3(e[j]) > m then 
<a name="line_283"></a>   m := nm3(e[j]);
<a name="line_284"></a>   i := j;
<a name="line_285"></a>  fi;
<a name="line_286"></a> od;
<a name="line_287"></a>
<a name="line_288"></a> b := e[i] /~ nm3(e[i]);
<a name="line_289"></a> c := cross_product(b,a);
<a name="line_290"></a> return [b,c];
<a name="line_291"></a>end:
<a name="line_292"></a>
<a name="line_293"></a># stereo_shift(x,a) is a version of stereographic projection that
<a name="line_294"></a># sends a to zero and -a to infinity.
<a name="line_295"></a>
<a name="line_296"></a><span style="color:red">#@ stereo_shift 
</span><a name="line_297"></a>stereo_shift := (x::RR_4, a::RR_4) ->  
<a name="line_298"></a>[(-x[1]*a[4]+x[2]*a[3]-x[3]*a[2]+x[4]*a[1])/(1+x[1]*a[1]+x[2]*a[2]+x[3]*a[3]+x[4]*a[4]),
<a name="line_299"></a> (-x[1]*a[3]-x[2]*a[4]+x[3]*a[1]+x[4]*a[2])/(1+x[1]*a[1]+x[2]*a[2]+x[3]*a[3]+x[4]*a[4]),
<a name="line_300"></a> (x[1]*a[2]-x[2]*a[1]-x[3]*a[4]+x[4]*a[3])/(1+x[1]*a[1]+x[2]*a[2]+x[3]*a[3]+x[4]*a[4])]:
<a name="line_301"></a>
<a name="line_302"></a><span style="color:red">#@ unstereo_shift 
</span><a name="line_303"></a>unstereo_shift := (u::RR_3,a::RR_4) -> [ 
<a name="line_304"></a> -(2*u[1]*a[4]+2*u[2]*a[3]-2*u[3]*a[2]+a[1]*u[1]^2+a[1]*u[2]^2+a[1]*u[3]^2-a[1])/(u[1]^2+u[2]^2+u[3]^2+1),
<a name="line_305"></a> -(-2*u[1]*a[3]+2*u[2]*a[4]+2*u[3]*a[1]+a[2]*u[1]^2+a[2]*u[2]^2+a[2]*u[3]^2-a[2])/(u[1]^2+u[2]^2+u[3]^2+1),
<a name="line_306"></a> (-2*u[1]*a[2]+2*u[2]*a[1]-2*u[3]*a[4]-a[3]*u[1]^2-a[3]*u[2]^2-a[3]*u[3]^2+a[3])/(u[1]^2+u[2]^2+u[3]^2+1),
<a name="line_307"></a> (2*u[1]*a[1]+2*u[2]*a[2]+2*u[3]*a[3]-a[4]*u[1]^2-a[4]*u[2]^2-a[4]*u[3]^2+a[4])/(u[1]^2+u[2]^2+u[3]^2+1)]:
<a name="line_308"></a>
<a name="line_309"></a>######################################################################
<a name="line_310"></a># A Mobius function that preserves the unit circle
<a name="line_311"></a>
<a name="line_312"></a>mob := (z,a) -> (z - a)/(1 - conjugate(a)*z); <span style="color:red">#@ mob 
</span><a name="line_313"></a>
<a name="line_314"></a># This assumes that u and v lie in the unit disc, and returns the
<a name="line_315"></a># unique holomorphic involution of the unit disc that exchanges
<a name="line_316"></a># u and v.
<a name="line_317"></a>
<a name="line_318"></a><span style="color:red">#@ switcher 
</span><a name="line_319"></a>switcher := proc(u,v) 
<a name="line_320"></a> local a,z;
<a name="line_321"></a> a := (u+v+conjugate(u+v)*u*v)/(1-abs(u*v)^2);
<a name="line_322"></a> return unapply((a-z)/(1-conjugate(a)*z),z);
<a name="line_323"></a>end:
<a name="line_324"></a>
<a name="line_325"></a><span style="color:red">#@ midpoint_Delta 
</span><a name="line_326"></a>midpoint_Delta := proc(u,v) 
<a name="line_327"></a> local a;
<a name="line_328"></a> a := (u+v+conjugate(u+v)*u*v)/(1-abs(u*v)^2);
<a name="line_329"></a> if a = 0 then
<a name="line_330"></a>  return 0;
<a name="line_331"></a> else 
<a name="line_332"></a>  return ((1-sqrt(1-abs(a)^2))/conjugate(a));
<a name="line_333"></a> fi;
<a name="line_334"></a>end:
<a name="line_335"></a>
<a name="line_336"></a>######################################################################
<a name="line_337"></a># Polar coordinates on S^3
<a name="line_338"></a>
<a name="line_339"></a><span style="color:red">#@ S3_polar 
</span><a name="line_340"></a>S3_polar := (abc::RR_3) ->
<a name="line_341"></a> [cos(abc[1])*cos(abc[2]),cos(abc[1])*sin(abc[2]),
<a name="line_342"></a>  sin(abc[1])*cos(abc[3]),sin(abc[1])*sin(abc[3])];
<a name="line_343"></a>
<a name="line_344"></a><span style="color:red">#@ S3_polar_inv 
</span><a name="line_345"></a>S3_polar_inv := (x::RR_4) ->
<a name="line_346"></a> [arcsin(sqrt(x[3]^2+x[4]^2)),
<a name="line_347"></a>  arcsin(x[2]/sqrt(x[1]^2+x[2]^2)),
<a name="line_348"></a>  arcsin(x[4]/sqrt(x[3]^2+x[4]^2))];
<a name="line_349"></a>
<a name="line_350"></a>######################################################################
<a name="line_351"></a># Various different models for the 2-torus:
<a name="line_352"></a>#
<a name="line_353"></a># T refers to points s in R^4 with s[1]^2+s[2]^2 = s[3]^2+s[4]^2 = 1.
<a name="line_354"></a># TA refers to pairs of angles
<a name="line_355"></a># TC refers to pairs of unit complex numbers
<a name="line_356"></a># TP refers to (R u infinity)^2
<a name="line_357"></a>
<a name="line_358"></a>TA_to_TC := (theta) -> [exp(I*theta[1]),exp(I*theta[2])]; <span style="color:red">#@ TA_to_TC 
</span><a name="line_359"></a>TA_to_TP := (theta) -> [tan(theta[1]/2),tan(theta[2]/2)]; <span style="color:red">#@ TA_to_TP 
</span><a name="line_360"></a>TA_to_T  := (theta) -> [cos(theta[1]),sin(theta[1]),cos(theta[2]),sin(theta[2])]; <span style="color:red">#@ TA_to_T  
</span><a name="line_361"></a>TC_to_TA := (z) -> [argument(z[1]),argument(z[2])]; <span style="color:red">#@ TC_to_TA 
</span><a name="line_362"></a>TC_to_TP := (z) -> [Im(z[1])/(1+Re(z[1])),Im(z[2])/(1+Re(z[2]))]; <span style="color:red">#@ TC_to_TP 
</span><a name="line_363"></a>TC_to_T  := (z) -> [Re(z[1]),Im(z[1]),Re(z[2]),Im(z[2])]; <span style="color:red">#@ TC_to_T  
</span><a name="line_364"></a>TP_to_TA := (t) -> [2*arctan(t[1]),2*arctan(t[2])]; <span style="color:red">#@ TP_to_TA 
</span><a name="line_365"></a>TP_to_TC := (t) -> [(1+I*t[1])/(1-I*t[1]),(1+I*t[2])/(1-I*t[2])]; <span style="color:red">#@ TP_to_TC 
</span><a name="line_366"></a>TP_to_T  := (t) -> [(1-t[1]^2)/(1+t[1]^2),2*t[1]/(1+t[1]^2), 
<a name="line_367"></a>                    (1-t[2]^2)/(1+t[2]^2),2*t[2]/(1+t[2]^2)]; <span style="color:red">#@ TP_to_T  
</span><a name="line_368"></a>T_to_TA  := (s) -> [arctan(s[2],s[1]),arctan(s[4],s[3])]; <span style="color:red">#@ T_to_TA  
</span><a name="line_369"></a>T_to_TC  := (s) -> [s[1]+I*s[2],s[3]+I*s[4]]; <span style="color:red">#@ T_to_TC  
</span><a name="line_370"></a>T_to_TP  := (s) -> [s[2]/(1+s[1]),s[4]/(1+s[3])]; <span style="color:red">#@ T_to_TP  
</span><a name="line_371"></a> 
<a name="line_372"></a># T_to_S2 gives a homeomorphism T/C_2 -> S^2.
<a name="line_373"></a>T_to_S2   := (s)     -> [s[3]-s[1],s[2]*s[4],-s[3]-s[1]]/~ sqrt((1+s[1]^2)*(1+s[3]^2)); <span style="color:red">#@ T_to_S2   
</span><a name="line_374"></a>TP_to_S2  := (a)     -> [a[1]^2-a[2]^2, 2*a[1]*a[2], a[1]^2*a[2]^2-1] /~ sqrt((1+a[1]^4)*(1+a[2]^4)); <span style="color:red">#@ TP_to_S2  
</span><a name="line_375"></a>TA_to_S2  := (theta) -> T_to_S2(TA_to_T(theta)); <span style="color:red">#@ TA_to_S2  
</span><a name="line_376"></a>TC_to_S2  := (z)     -> T_to_S2(TC_to_T(z)); <span style="color:red">#@ TC_to_S2  
</span><a name="line_377"></a>
<a name="line_378"></a># This is a generically defined section for T_to_S2
<a name="line_379"></a><span style="color:red">#@ S2_to_TP 
</span><a name="line_380"></a>S2_to_TP := (u) -> [ 
<a name="line_381"></a> abs(u[2])/sqrt((sqrt(1-u[1]^2)-u[3])*(sqrt(1-u[3]^2)-u[1])),
<a name="line_382"></a>     u[2] /sqrt((sqrt(1-u[1]^2)-u[3])*(sqrt(1-u[3]^2)+u[1]))
<a name="line_383"></a>];
<a name="line_384"></a>
<a name="line_385"></a># T_to_R3 is the standard embedding of T in R^3.
<a name="line_386"></a>T_to_R3  := (t) -> [t[1]*(2+t[3]),t[2]*(2+t[3]),t[4]]; <span style="color:red">#@ T_to_R3  
</span><a name="line_387"></a>TA_to_R3 := (theta) -> T_to_R3(TA_to_T(theta));        <span style="color:red">#@ TA_to_R3 
</span><a name="line_388"></a>TC_to_R3 := (z)     -> T_to_R3(TC_to_T(z));            <span style="color:red">#@ TC_to_R3 
</span><a name="line_389"></a>TP_to_R3 := (t)     -> T_to_R3(TP_to_T(t));            <span style="color:red">#@ TP_to_R3 
</span><a name="line_390"></a>
<a name="line_391"></a># We next have similar functions for the 4-torus
<a name="line_392"></a>
<a name="line_393"></a>TTA_to_TTC := (theta) -> [seq(exp(I*theta[i]),i=1..4)]; <span style="color:red">#@ TTA_to_TTC 
</span><a name="line_394"></a>TTA_to_TTP := (theta) -> [seq(tan(theta[i]/2),i=1..4)]; <span style="color:red">#@ TTA_to_TTP 
</span><a name="line_395"></a>TTA_to_TT  := (theta) -> map(op,[seq([cos(theta[i]),sin(theta[i])],i=1..4)]); <span style="color:red">#@ TTA_to_TT  
</span><a name="line_396"></a>TTC_to_TTA := (z) -> [seq(argument(z[i]),i=1..4)]; <span style="color:red">#@ TTC_to_TTA 
</span><a name="line_397"></a>TTC_to_TTP := (z) -> [seq(Im(z[i])/(1+Re(z[i])),i=1..4)]; <span style="color:red">#@ TTC_to_TTP 
</span><a name="line_398"></a>TTC_to_TT  := (z) -> map(op,[seq([Re(z[i]),Im(z[i])],i=1..4)]); <span style="color:red">#@ TTC_to_TT  
</span><a name="line_399"></a>TTP_to_TTA := (t) -> [seq(2*arctan(t[i]),i=1..4)]; <span style="color:red">#@ TTP_to_TTA 
</span><a name="line_400"></a>TTP_to_TTC := (t) -> [seq((1+I*t[i])/(1-I*t[i]),i=1..4)]; <span style="color:red">#@ TTP_to_TTC 
</span><a name="line_401"></a>TTP_to_TT  := (t) -> map(op,[seq([(1-t[i]^2)/(1+t[i]^2),2*t[i]/(1+t[i]^2)],i=1..4)]); <span style="color:red">#@ TTP_to_TT  
</span><a name="line_402"></a>TT_to_TTA  := (s) -> [seq(arctan(s[2*i],s[2*i-1]),i=1..4)]; <span style="color:red">#@ TT_to_TTA  
</span><a name="line_403"></a>TT_to_TTC  := (s) -> [seq(s[2*i-1]+I*s[2*i],i=1..4)]; <span style="color:red">#@ TT_to_TTC  
</span><a name="line_404"></a>TT_to_TTP  := (s) -> [seq(s[2*i]/(1+s[2*i-1]),i=1..4)]; <span style="color:red">#@ TT_to_TTP  
</span><a name="line_405"></a>
<a name="line_406"></a>######################################################################
<a name="line_407"></a># Quaternions 
<a name="line_408"></a>
<a name="line_409"></a># We identify R^4 with the quaternions by the rule
<a name="line_410"></a># [x1,x2,x3,x4] |-> x1 i + x2 j + x3 k + x4.
<a name="line_411"></a>
<a name="line_412"></a><span style="color:red">#@ H_mult 
</span><a name="line_413"></a>H_mult := proc(a::RR_4,b::RR_4) 
<a name="line_414"></a> [  a[1]*b[4] + a[2]*b[3] - a[3]*b[2] + a[4]*b[1] ,
<a name="line_415"></a>  - a[1]*b[3] + a[2]*b[4] + a[3]*b[1] + a[4]*b[2] ,
<a name="line_416"></a>    a[1]*b[2] - a[2]*b[1] + a[3]*b[4] + a[4]*b[3] ,
<a name="line_417"></a>  - a[1]*b[1] - a[2]*b[2] - a[3]*b[3] + a[4]*b[4]
<a name="line_418"></a> ];
<a name="line_419"></a>end:
<a name="line_420"></a>
<a name="line_421"></a><span style="color:red">#@ H_conjugate 
</span><a name="line_422"></a>H_conjugate := proc(a::RR_4)
<a name="line_423"></a> [-a[1],-a[2],-a[3],a[4]];
<a name="line_424"></a>end:
<a name="line_425"></a>
<a name="line_426"></a><span style="color:red">#@ H_quotient 
</span><a name="line_427"></a>H_quotient := proc(a,b)
<a name="line_428"></a> H_mult(a,H_conjugate(b)) /~ nm4(b)^2;
<a name="line_429"></a>end:
<a name="line_430"></a>
<a name="line_431"></a>######################################################################
<a name="line_432"></a># The Hopf fibration
<a name="line_433"></a>
<a name="line_434"></a># The Hopf fibration from R^4\{0} to S^2
<a name="line_435"></a><span style="color:red">#@ hopf_map 
</span><a name="line_436"></a>hopf_map := (x::RR_4) ->
<a name="line_437"></a> [2*x[1]*x[3]+2*x[2]*x[4],
<a name="line_438"></a>  2*x[1]*x[4]-2*x[2]*x[3],
<a name="line_439"></a>  x[1]^2+x[2]^2-x[3]^2-x[4]^2]/~(rho(x));
<a name="line_440"></a>
<a name="line_441"></a><span style="color:red">#@ find_hopf_preimages 
</span><a name="line_442"></a>find_hopf_preimages := proc(y::RR_3)
<a name="line_443"></a> local eqs,sol,fsol,is_real,x,xx; 
<a name="line_444"></a> xx := [x[1],x[2],x[3],x[4]];
<a name="line_445"></a> is_real := (u) -> (Im(u[1]) = 0 and Im(u[2]) = 0 and Im(u[3]) = 0 and Im(u[4]) = 0):
<a name="line_446"></a> eqs := {g0(x)=0,hopf_proj(x)[1]=y[1],hopf_proj(x)[2]=y[2],hopf_proj(x)[3]=y[3]};
<a name="line_447"></a> sol := [solve(eqs,{op(xx)})]:
<a name="line_448"></a> fsol := select(is_real,evalf(map(s -> subs(s,xx),sol)));
<a name="line_449"></a> return(fsol);
<a name="line_450"></a>end:
<a name="line_451"></a>
<a name="line_452"></a><span style="color:red">#@ act_hopf
</span><a name="line_453"></a>act_hopf[1]    := (u) -> [ u[1], u[2], u[3]];
<a name="line_454"></a>act_hopf[LL]   := (u) -> [-u[1],-u[2], u[3]];
<a name="line_455"></a>act_hopf[LM]   := (u) -> [ u[2], u[1], u[3]];
<a name="line_456"></a>act_hopf[LLLM] := (u) -> [-u[2],-u[1], u[3]];
<a name="line_457"></a>act_hopf[LN]   := (u) -> [-u[2],-u[1], u[3]];
<a name="line_458"></a>act_hopf[LLLN] := (u) -> [ u[2], u[1], u[3]];
<a name="line_459"></a>act_hopf[MN]   := (u) -> [-u[1],-u[2], u[3]];
<a name="line_460"></a>act_hopf[LLMN] := (u) -> [ u[1], u[2], u[3]];
<a name="line_461"></a>
<a name="line_462"></a>
<a name="line_463"></a>######################################################################
<a name="line_464"></a># Random points on spheres
<a name="line_465"></a>
<a name="line_466"></a><span style="color:red">#@ random_S3_point 
</span><a name="line_467"></a>random_S3_point := proc()
<a name="line_468"></a> local x,rr,nx;
<a name="line_469"></a> rr := rand(-1000..1000);
<a name="line_470"></a> nx := 0;
<a name="line_471"></a> while nx < 0.1 or nx > 1 do 
<a name="line_472"></a>  x := [rr(),rr(),rr(),rr()] /~ 1000.;
<a name="line_473"></a>  nx := sqrt(add(x[i]^2,i=1..4));
<a name="line_474"></a> od;
<a name="line_475"></a> x := x /~ nx;
<a name="line_476"></a> return(x);
<a name="line_477"></a>end:
<a name="line_478"></a>
<a name="line_479"></a>random_S2_point := () -> hopf_map(random_S3_point());
<a name="line_480"></a>
<a name="line_481"></a>######################################################################
<a name="line_482"></a># R2_zone(x,y) returns an integer between 0 and 16 depending on the
<a name="line_483"></a># argument theta of (x,y).  If theta = k * Pi/4 then the result is 
<a name="line_484"></a># 2k+1.  If k * Pi/4 < theta < (k+1) * Pi/4 then the result is 2k+2.
<a name="line_485"></a># If (x,y) = (0,0) then theta is undefined and the result is 0.
<a name="line_486"></a>#
<a name="line_487"></a># We also define C_zone(x + I*y) = R2_zone(x,y).
<a name="line_488"></a>
<a name="line_489"></a><span style="color:red">#@ R2_zone 
</span><a name="line_490"></a>R2_zone := proc(x0,y0)
<a name="line_491"></a> local x,y;
<a name="line_492"></a> x := evalf(x0);
<a name="line_493"></a> y := evalf(y0);
<a name="line_494"></a> if x = 0 and y = 0 then
<a name="line_495"></a>  return(0);
<a name="line_496"></a> elif y = 0 and 0 < x then
<a name="line_497"></a>  return(1);
<a name="line_498"></a> elif 0 < y and y < x then
<a name="line_499"></a>  return(2);
<a name="line_500"></a> elif 0 < x and x = y then
<a name="line_501"></a>  return(3);
<a name="line_502"></a> elif 0 < x and x < y then
<a name="line_503"></a>  return(4);
<a name="line_504"></a> elif 0 = x and 0 < y then
<a name="line_505"></a>  return(5);
<a name="line_506"></a> elif 0 < -x and -x < y then
<a name="line_507"></a>  return(6);
<a name="line_508"></a> elif 0 < -x and -x = y then
<a name="line_509"></a>  return(7);
<a name="line_510"></a> elif 0 < y and y < -x then
<a name="line_511"></a>  return(8);
<a name="line_512"></a> elif 0 = y and 0 < -x then
<a name="line_513"></a>  return(9);
<a name="line_514"></a> elif 0 < -y and -y < -x then
<a name="line_515"></a>  return(10);
<a name="line_516"></a> elif 0 < -y and -y = -x then
<a name="line_517"></a>  return(11);
<a name="line_518"></a> elif 0 < -x and -x < -y then
<a name="line_519"></a>  return(12);
<a name="line_520"></a> elif 0 = -x and 0 < -y then
<a name="line_521"></a>  return(13);
<a name="line_522"></a> elif 0 < x and x < -y then
<a name="line_523"></a>  return(14);
<a name="line_524"></a> elif 0 < x and x = -y then
<a name="line_525"></a>  return(15)
<a name="line_526"></a> elif 0 < -y and -y < x then
<a name="line_527"></a>  return(16);
<a name="line_528"></a> else
<a name="line_529"></a>  return(FAIL);
<a name="line_530"></a> fi;
<a name="line_531"></a>end: 
<a name="line_532"></a>
<a name="line_533"></a><span style="color:red">#@ C_zone 
</span><a name="line_534"></a>C_zone := (z::CC) -> R2_zone(Re(z),Im(z)):
<a name="line_535"></a>
<a name="line_536"></a>######################################################################
<a name="line_537"></a># Winding numbers
<a name="line_538"></a>
<a name="line_539"></a># This calculates the winding number of a map u : [0,2*Pi] -> C\{0}.
<a name="line_540"></a># The second parameter n_ defaults to 24 if it is omitted.  The
<a name="line_541"></a># result may be incorrect if n_ is too small, but 24 is ample for
<a name="line_542"></a># the functions that occur in practice in this project.
<a name="line_543"></a>
<a name="line_544"></a><span style="color:red">#@ C_winding_number 
</span><a name="line_545"></a>C_winding_number := proc(u,n_)
<a name="line_546"></a> local n,uu,a;
<a name="line_547"></a>
<a name="line_548"></a> n := `if`(nargs > 1,n_,24);
<a name="line_549"></a> uu := [seq(evalf(u(2*Pi*i/n)),i=0..(n-1))];
<a name="line_550"></a> uu := [op(uu),uu[1]];
<a name="line_551"></a> a := add(argument(uu[i+1]/uu[i]),i=1..n);
<a name="line_552"></a> return round(evalf(a/(2*Pi)));
<a name="line_553"></a>end:
<a name="line_554"></a>
<a name="line_555"></a><span style="color:red">#@ R2_winding_number 
</span><a name="line_556"></a>R2_winding_number := proc(u,n_)
<a name="line_557"></a> C_winding_number((t) -> (u(t)[1] + u(t)[2]*I),args[2..-1]);
<a name="line_558"></a>end:
<a name="line_559"></a>
<a name="line_560"></a>######################################################################
<a name="line_561"></a># Miscellaneous other things
<a name="line_562"></a>
<a name="line_563"></a># rational_sphere point defines a map Q x Q -> S^2 n Q^4 with dense image.
<a name="line_564"></a>
<a name="line_565"></a><span style="color:red">#@ rational_sphere_point 
</span><a name="line_566"></a>rational_sphere_point := (s::RR,t::RR) -> [
<a name="line_567"></a> 2*s/(1+s^2)*2*t/(1+t^2),
<a name="line_568"></a> 2*s/(1+s^2)*(1-t^2)/(1+t^2),
<a name="line_569"></a> (1-s^2)/(1+s^2)
<a name="line_570"></a>];
<a name="line_571"></a>
<a name="line_572"></a># Midpoints of line segments
<a name="line_573"></a>
<a name="line_574"></a>midpoint_R2 := (a::RR_2,b::RR_2) -> [(a[1]+b[1])/2,(a[2]+b[2])/2]; <span style="color:red">#@ midpoint_R2 
</span><a name="line_575"></a>midpoint_R3 := (a::RR_3,b::RR_3) -> [(a[1]+b[1])/2,(a[2]+b[2])/2,(a[3]+b[3])/2]; <span style="color:red">#@ midpoint_R3 
</span><a name="line_576"></a>midpoint_R4 := (a::RR_4,b::RR_4) -> [(a[1]+b[1])/2,(a[2]+b[2])/2,(a[3]+b[3])/2,(a[4]+b[4])/2]; <span style="color:red">#@ midpoint_R4 
</span><a name="line_577"></a>
<a name="line_578"></a># Midpoints of arcs in the spheres S^2 and S^3
<a name="line_579"></a>
<a name="line_580"></a><span style="color:red">#@ midpoint_S2 
</span><a name="line_581"></a>midpoint_S2 := proc(a::RR_3,b::RR_3)
<a name="line_582"></a> local c,r;
<a name="line_583"></a> c := midpoint_R3(a,b);
<a name="line_584"></a> r := sqrt(c[1]^2+c[2]^2+c[3]^2);
<a name="line_585"></a> c := [c[1]/r,c[2]/r,c[3]/r];
<a name="line_586"></a> return(c);
<a name="line_587"></a>end:
<a name="line_588"></a> 
<a name="line_589"></a><span style="color:red">#@ midpoint_S3 
</span><a name="line_590"></a>midpoint_S3 := proc(a::RR_4,b::RR_4)
<a name="line_591"></a> local c,r;
<a name="line_592"></a> c := midpoint_R4(a,b);
<a name="line_593"></a> r := sqrt(c[1]^2+c[2]^2+c[3]^2+c[4]^2);
<a name="line_594"></a> c := [c[1]/r,c[2]/r,c[3]/r,c[4]/r];
<a name="line_595"></a> return(c);
<a name="line_596"></a>end:
<a name="line_597"></a> 
<a name="line_598"></a># The area of a flat triangle with vertices in S^3
<a name="line_599"></a>
<a name="line_600"></a><span style="color:red">#@ triangle_area 
</span><a name="line_601"></a>triangle_area := proc(a::RR_4,b::RR_4,c::RR_4)
<a name="line_602"></a> local p,q;
<a name="line_603"></a> p := [seq(b[i]-a[i],i=1..4)];
<a name="line_604"></a> q := [seq(c[i]-a[i],i=1..4)];
<a name="line_605"></a> sqrt(expand(dp4(p,p)*dp4(q,q)-dp4(p,q)^2))/2;
<a name="line_606"></a>end:
<a name="line_607"></a>
<a name="line_608"></a>
<a name="line_609"></a># xx is a generic vector in R^4
<a name="line_610"></a>xx := [x[1],x[2],x[3],x[4]]; <span style="color:red">#@ xx 
</span><a name="line_611"></a>yy := [y[1],y[2]];           <span style="color:red">#@ yy 
</span><a name="line_612"></a>zz := [z[1],z[2]];           <span style="color:red">#@ zz 
</span><a name="line_613"></a>tt := [t[1],t[2]];           <span style="color:red">#@ tt 
</span><a name="line_614"></a>
<a name="line_615"></a>assume(x[1]::real);
<a name="line_616"></a>assume(x[2]::real);
<a name="line_617"></a>assume(x[3]::real);
<a name="line_618"></a>assume(x[4]::real);
<a name="line_619"></a>
<a name="line_620"></a>assume(y[1]::real);
<a name="line_621"></a>assume(y[2]::real);
<a name="line_622"></a>
<a name="line_623"></a>protect('x','xx','y','yy','z','zz','t','tt');
<a name="line_624"></a>
  </pre>
 </body>
</html>
    