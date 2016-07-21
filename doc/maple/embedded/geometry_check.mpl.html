<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_stereo := proc()
<a name="line_2"></a> local u,x,uu,xx;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> xx := [x[1],x[2],x[3],x[4]];
<a name="line_7"></a> uu := [u[1],u[2],u[3]];
<a name="line_8"></a>
<a name="line_9"></a> _ASSERT(simplify(rho(unstereo(uu))-1)=0,"|unstereo(u)|=1");
<a name="line_10"></a>
<a name="line_11"></a> _ASSERT(
<a name="line_12"></a>  factor(
<a name="line_13"></a>   unstereo(stereo(xx)) -~ xx +~ (
<a name="line_14"></a>   (rho(xx)-1)/(rho(xx)+1-2*x[4]) *~ (xx -~ [0,0,0,1])
<a name="line_15"></a>  )) = [0,0,0,0],
<a name="line_16"></a>  "unstereo o stereo = id"
<a name="line_17"></a> );
<a name="line_18"></a>
<a name="line_19"></a> _ASSERT(
<a name="line_20"></a>  factor(stereo(unstereo(uu)) -~ uu) = [0,0,0],
<a name="line_21"></a>  "stereo o unstereo = id"
<a name="line_22"></a> );
<a name="line_23"></a>end:
<a name="line_24"></a>
<a name="line_25"></a>add_check(check_stereo):
<a name="line_26"></a>
<a name="line_27"></a>######################################################################
<a name="line_28"></a>
<a name="line_29"></a>check_omega := proc() 
<a name="line_30"></a> local u;
<a name="line_31"></a>
<a name="line_32"></a> printf("%a()\n",procname);
<a name="line_33"></a>
<a name="line_34"></a> _ASSERT(simplify(f_1(omega[1](t)))=0,"f_1(omega[1](t))=0");
<a name="line_35"></a> _ASSERT(simplify(f_2(omega[2](t)))=0,"f_2(omega[2](t))=0");
<a name="line_36"></a> _ASSERT(
<a name="line_37"></a>  solve({seq(omega[1](s1)[i]=omega[2](s2)[i],i=1..3)},{s1,s2})={s1=0,s2=0},
<a name="line_38"></a> "Omega[1] intersect Omega[2]"
<a name="line_39"></a> );
<a name="line_40"></a>
<a name="line_41"></a> u := simplify(stereo(omega[1](t)));
<a name="line_42"></a> _ASSERT(u[2]=0 and u[3]=-a_E,"stereo(Omega[1]+)");
<a name="line_43"></a> u := simplify(stereo(omega[2](t)));
<a name="line_44"></a> _ASSERT(u[1]=0 and u[3]= a_E,"stereo(Omega[2]+)");
<a name="line_45"></a>
<a name="line_46"></a> u := simplify(stereo(-omega[1](t)));
<a name="line_47"></a> _ASSERT(u[2]=0 and simplify(u[1]^2+(u[3]-1/(2*a_E))^2-1/(4*a_E^2))=0,"stereo(Omega[1]-)");
<a name="line_48"></a> u := simplify(stereo(-omega[2](t)));
<a name="line_49"></a> _ASSERT(u[1]=0 and simplify(u[2]^2+(u[3]+1/(2*a_E))^2-1/(4*a_E^2))=0,"stereo(Omega[2]-)");
<a name="line_50"></a>
<a name="line_51"></a>end:
<a name="line_52"></a>
<a name="line_53"></a>add_check(check_omega):
<a name="line_54"></a>
<a name="line_55"></a>######################################################################
<a name="line_56"></a>
<a name="line_57"></a>check_g := proc()
<a name="line_58"></a> local x,xx,u,uu,i,P;
<a name="line_59"></a>
<a name="line_60"></a> printf("%a()\n",procname);
<a name="line_61"></a>
<a name="line_62"></a> uu := [u[1],u[2],u[3]];
<a name="line_63"></a> xx := [x[1],x[2],x[3],x[4]];
<a name="line_64"></a>
<a name="line_65"></a> _ASSERT(expand(g_0(xx) - ((f(xx)-f(-xx))/8 + x[4]*(rho(xx) - 1)))=0,"g_0(x) formula");
<a name="line_66"></a> _ASSERT(expand(g(xx)  - ((f(xx)-f(-xx))/8 - x[4]*(rho(xx) - 1)))=0,"g(x) formula");
<a name="line_67"></a> _ASSERT(expand(g_0(xx) - (-2*x[4]-1/a_E*x[3]*g_1(xx))) = 0, "g_1(x) formula");
<a name="line_68"></a>
<a name="line_69"></a> for i from 1 to 4 do
<a name="line_70"></a>  _ASSERT(expand(dg(xx)[i] - diff(g(xx),x[i]))=0,"dg(x)");
<a name="line_71"></a> od:
<a name="line_72"></a>
<a name="line_73"></a> _ASSERT(simplify(square_norm_of_dg(xx) - nm4(dg(xx))^2) = 0,
<a name="line_74"></a>         "square norm of dg formula");
<a name="line_75"></a>
<a name="line_76"></a> _ASSERT(
<a name="line_77"></a>  factor(g_stereo(uu) - ((nm3(uu)^2+1)^3 * g(unstereo(uu)))) = 0,
<a name="line_78"></a>  "g_stereo"
<a name="line_79"></a> );
<a name="line_80"></a> 
<a name="line_81"></a> _ASSERT( 
<a name="line_82"></a>  expand((1-a_E^2)/16*(dg(xx)[1]^2+dg(xx)[2]^2)+
<a name="line_83"></a>         a_E/2*(x[1]^2-x[2]^2)*dg(xx)[3]-(x[1]^2+x[2]^2)/4*dg(xx)[4] - (
<a name="line_84"></a>         x[1]^4+x[2]^4+(5/2-a_E^2)*(x[1]^2+x[2]^2)*x[4]^2)
<a name="line_85"></a>        ) = 0,
<a name="line_86"></a>  "smoothness identity"
<a name="line_87"></a> );
<a name="line_88"></a>
<a name="line_89"></a>end:
<a name="line_90"></a>
<a name="line_91"></a>add_check(check_g):
<a name="line_92"></a>
<a name="line_93"></a>######################################################################
<a name="line_94"></a>
<a name="line_95"></a>check_quadratic_chart := proc()
<a name="line_96"></a> local x,u,xx,uu,e,yy,err;
<a name="line_97"></a>
<a name="line_98"></a> printf("%a()\n",procname);
<a name="line_99"></a>
<a name="line_100"></a> xx := [x[1],x[2],x[3],x[4]];
<a name="line_101"></a> uu := [u[1],u[2],u[3],u[4]];
<a name="line_102"></a>
<a name="line_103"></a> yy := quadratic_chart(xx,e *~ uu);
<a name="line_104"></a> err := rho(yy) - 1;
<a name="line_105"></a> err := err - (rho(xx)-1)
<a name="line_106"></a>            - 2*dp4(xx,uu)*e 
<a name="line_107"></a>            + 6*dp4(xx,dg(uu)) * g(xx)*e^2/square_norm_of_dg(xx) 
<a name="line_108"></a>            + (rho(xx)-1)*rho(uu)*e^2;
<a name="line_109"></a> err := factor(expand(convert(series(err,e=0,3),polynom,e)));
<a name="line_110"></a> _ASSERT(err=0,"rho on quadratic_chart");
<a name="line_111"></a>
<a name="line_112"></a> err := g(yy);
<a name="line_113"></a> err := err - g(xx)
<a name="line_114"></a>            - e*dp4(uu,dg(xx)) 
<a name="line_115"></a>            + e^2*(3/2)*dp4(uu,uu)*g(xx);
<a name="line_116"></a> err := factor(expand(convert(series(err,e=0,3),polynom,e)));
<a name="line_117"></a> _ASSERT(err=0,"g on quadratic_chart");
<a name="line_118"></a>end:
<a name="line_119"></a>
<a name="line_120"></a>add_check(check_quadratic_chart):
<a name="line_121"></a>
<a name="line_122"></a>######################################################################
<a name="line_123"></a>
<a name="line_124"></a>check_laplacian_a := proc()
<a name="line_125"></a> local i,j,k,gg,n0x,m0x,r0x,r1x,r2x;
<a name="line_126"></a>
<a name="line_127"></a> printf("%a()\n",procname);
<a name="line_128"></a>
<a name="line_129"></a> for i from 1 to 4 do 
<a name="line_130"></a>  for j from 1 to 4 do 
<a name="line_131"></a>   for k from 1 to 4 do 
<a name="line_132"></a>    gg[i,j,k] := diff(g0(x),x[i],x[j],x[k])/6;
<a name="line_133"></a>   od:
<a name="line_134"></a>  od:
<a name="line_135"></a> od:
<a name="line_136"></a>
<a name="line_137"></a> n0x := expand([seq(diff(g0(x),x[i]),i=1..4)]);
<a name="line_138"></a> m0x := [seq([seq(diff(g0(x),x[i],x[j]),j=1..4)],i=1..4)];
<a name="line_139"></a> 
<a name="line_140"></a> _ASSERT(
<a name="line_141"></a>  n0x -~ [seq(add(add(3*gg[p,j,k]*x[j]*x[k],j=1..4),k=1..4),p=1..4)] = [0$4],
<a name="line_142"></a>  "Formula for n(x) in terms of g_{ijk}"
<a name="line_143"></a> );
<a name="line_144"></a>
<a name="line_145"></a> _ASSERT(
<a name="line_146"></a>  m0x -~ [seq([seq(6*add(gg[p,q,k]*x[k],k=1..4),q=1..4)],p=1..4)] = [[0$4]$4],
<a name="line_147"></a>  "Formula for m(x) in terms of g_{ijk}"
<a name="line_148"></a> );
<a name="line_149"></a>
<a name="line_150"></a> # These are called r, r' and r'' in the LaTeX document
<a name="line_151"></a> r0x := sqrt(expand(dp4(n0x,n0x)));
<a name="line_152"></a> r1x := expand(add(m0x[i][i],i=1..4));
<a name="line_153"></a> r2x := expand(add(add(n0x[i]*m0x[i][j]*n0x[j],j=1..4),i=1..4));
<a name="line_154"></a>
<a name="line_155"></a> 
<a name="line_156"></a> _ASSERT(
<a name="line_157"></a>  expand(r0x^2 - 9*add(add(add(add(add(
<a name="line_158"></a>                    gg[i,j,m]*gg[k,l,m]*x[i]*x[j]*x[k]*x[l],
<a name="line_159"></a>		     i=1..4),j=1..4),k=1..4),l=1..4),m=1..4)
<a name="line_160"></a>  ) = 0,
<a name="line_161"></a>  "Formula for r^2 in terms of g_{ijk}"
<a name="line_162"></a> );
<a name="line_163"></a> 
<a name="line_164"></a> _ASSERT(
<a name="line_165"></a>  expand(r1x - 6*add(add(gg[i,j,j]*x[i],i=1..4),j=1..4)) = 0,
<a name="line_166"></a>  "Formula for r' in terms of g_{ijk}"
<a name="line_167"></a> );
<a name="line_168"></a>
<a name="line_169"></a> _ASSERT(
<a name="line_170"></a>  expand(r2x - 54*add(add(add(add(add(add(add(
<a name="line_171"></a>                   gg[i,j,k]*gg[k,l,m]*gg[m,n,p]*x[i]*x[j]*x[l]*x[n]*x[p],
<a name="line_172"></a>	            i=1..4),j=1..4),k=1..4),l=1..4),m=1..4),n=1..4),p=1..4)) = 0,
<a name="line_173"></a>  "Formula for r'' in terms of g_{ijk}"
<a name="line_174"></a> );
<a name="line_175"></a>end:
<a name="line_176"></a>
<a name="line_177"></a>add_check(check_laplacian_a):
<a name="line_178"></a>
<a name="line_179"></a>######################################################################
<a name="line_180"></a>
<a name="line_181"></a>check_laplacian_b := proc()
<a name="line_182"></a> local x0,u0,v0,E0,F0,G0,M0,D0,DI0,MI0,i1,i2,i3,i4,f0,p0,L0,L1,err,ok;
<a name="line_183"></a>
<a name="line_184"></a> printf("%a()\n",procname);
<a name="line_185"></a>
<a name="line_186"></a> x0 := y_lift([s[1],s[2]]):
<a name="line_187"></a> u0 := simplify(map(diff,x0,s[1])):
<a name="line_188"></a> v0 := simplify(map(diff,x0,s[2])):
<a name="line_189"></a> E0 := factor(expand(rationalize(simplify(dp4(u0,u0))))):
<a name="line_190"></a> F0 := factor(expand(rationalize(simplify(dp4(u0,v0))))):
<a name="line_191"></a> G0 := factor(expand(rationalize(simplify(dp4(v0,v0))))):
<a name="line_192"></a> M0 := <<E0|F0>,<F0|G0>>:
<a name="line_193"></a> D0 := factor(expand(rationalize(Determinant(M0)))):
<a name="line_194"></a> DI0 := factor(expand(rationalize(1/D0))):
<a name="line_195"></a> MI0 := factor(expand(map(rationalize,DI0 * <<G0|-F0>,<-F0|E0>>))):
<a name="line_196"></a>
<a name="line_197"></a> ok := true;
<a name="line_198"></a>
<a name="line_199"></a> for i1 from 0 to 2 do
<a name="line_200"></a>  for i2 from 0 to 2 do
<a name="line_201"></a>   for i3 from 0 to 2 do
<a name="line_202"></a>    for i4 from 0 to 2 do
<a name="line_203"></a>     if ok then
<a name="line_204"></a>      f0 := unapply(t[1]^i1 * t[2]^i2 * t[3]^i3 * t[4]^i4,t);
<a name="line_205"></a>      L0 := subs({seq(x[i]=x0[i],i=1..4)},laplacian(f0(x)));
<a name="line_206"></a>      p0 := factor(expand(simplify(sqrt(D0) * MI0 . <diff(f0(x0),s[1]),diff(f0(x0),s[2])>))):
<a name="line_207"></a>      L1 := simplify((diff(p0[1],s[1]) + diff(p0[2],s[2]))/sqrt(D0)):
<a name="line_208"></a>      err := simplify(L0 - L1);
<a name="line_209"></a>      if err <> 0 then
<a name="line_210"></a>       ok := false;
<a name="line_211"></a>      fi;
<a name="line_212"></a>     fi;
<a name="line_213"></a>    od;
<a name="line_214"></a>   od;
<a name="line_215"></a>  od;
<a name="line_216"></a> od;
<a name="line_217"></a>
<a name="line_218"></a> _ASSERT(ok = true,"laplacian on monomials");
<a name="line_219"></a>
<a name="line_220"></a>end:
<a name="line_221"></a>
<a name="line_222"></a>add_check(check_laplacian_b):
<a name="line_223"></a>
<a name="line_224"></a>######################################################################
<a name="line_225"></a>
<a name="line_226"></a>check_laplacian_z := proc()
<a name="line_227"></a> local i,j,mz,mx,Lmz,Lmx,err;
<a name="line_228"></a>
<a name="line_229"></a> printf("%a()\n",procname);
<a name="line_230"></a>
<a name="line_231"></a> for i from 0 to 3 do
<a name="line_232"></a>  for j from 0 to 3 do 
<a name="line_233"></a>   mz := z[1]^i*z[2]^j:
<a name="line_234"></a>   userinfo(5,genus2,sprintf("Checking laplacian of %A",mz));
<a name="line_235"></a>   mx := NF_x(eval(subs(z = zx,mz))):
<a name="line_236"></a>   Lmz := simplify(laplacian_z(mz)):
<a name="line_237"></a>   Lmx := simplify(laplacian(mx)):
<a name="line_238"></a>   Lmx := simplify(NF_z(numer(Lmx))/NF_z(denom(Lmx))):
<a name="line_239"></a>   err := simplify(Lmz - Lmx);
<a name="line_240"></a>   _ASSERT(err = 0,sprintf("laplacian = laplacian_z on %A",mz));
<a name="line_241"></a>  od:
<a name="line_242"></a> od:
<a name="line_243"></a>end:
<a name="line_244"></a>
<a name="line_245"></a>add_check(check_laplacian_z):
<a name="line_246"></a>
<a name="line_247"></a>######################################################################
<a name="line_248"></a>
<a name="line_249"></a>check_gauss_map := proc()
<a name="line_250"></a> local i,j,x0,n0,u0,v0,a0,b0,c0,errs,rels,sols,wl;
<a name="line_251"></a>
<a name="line_252"></a> printf("%a()\n",procname);
<a name="line_253"></a>
<a name="line_254"></a>_ASSERT(
<a name="line_255"></a>  NF_x(numer(factor(nm3(gauss_map(xx))^2 - 1))) = 0,
<a name="line_256"></a>  "gauss_map lands in S^2"
<a name="line_257"></a> );
<a name="line_258"></a>
<a name="line_259"></a> _ASSERT(
<a name="line_260"></a>  {seq(simplify(gauss_map(act_R4[T](xx)) - act_gauss[T](gauss_map(xx))),
<a name="line_261"></a>       T in [1,LL,LM,LLLM,LN,LLLN,MN,LLMN])} = {[0$3]},
<a name="line_262"></a>  "equivariance of gauss_map"
<a name="line_263"></a> );
<a name="line_264"></a>
<a name="line_265"></a> x0 := v_E[6];
<a name="line_266"></a> n0 := dg(x0);
<a name="line_267"></a> u0 := [1,-1,0,0] /~ sqrt(2);
<a name="line_268"></a> v0 := [0,0,1,0];
<a name="line_269"></a> a0 := gauss_map(x0);
<a name="line_270"></a> b0 := simplify(subs(e = 0,map(diff,gauss_map(x0 +~ e *~ u0),e)));
<a name="line_271"></a> c0 := simplify(subs(e = 0,map(diff,gauss_map(x0 +~ e *~ v0),e)));
<a name="line_272"></a>
<a name="line_273"></a> errs := 
<a name="line_274"></a> {dp4(u0,x0),dp4(u0,n0),dp4(v0,x0),dp4(v0,n0),dp4(u0,v0),
<a name="line_275"></a>  Determinant(Matrix([x0,n0,u0,v0])) - 2,
<a name="line_276"></a>  Determinant(Matrix([a0,b0,c0])) - (1/a_E^2 - 1) 
<a name="line_277"></a> };
<a name="line_278"></a>
<a name="line_279"></a> errs := map(NF_x,map(numer,map(factor,errs)));
<a name="line_280"></a>
<a name="line_281"></a> _ASSERT(errs = {0},"gauss_map reverses orientation at v[6]");
<a name="line_282"></a> 
<a name="line_283"></a> # We now check that the preimage of gauss_map(v_E[6]) consists only
<a name="line_284"></a> # of v_E[6].  The preimage is defined by the relations in rels[0].
<a name="line_285"></a> rels[0] := simplify([rho(xx)-1,g(xx),op(gauss_map(xx) -~ gauss_map(v_E[6]))]):
<a name="line_286"></a>
<a name="line_287"></a> # These imply the relations in rels[1] and rels[2]
<a name="line_288"></a> rels[1] := [rels[0][1],
<a name="line_289"></a>             a_E^2*rels[0][2],
<a name="line_290"></a>	     numer(factor(rels[0][3]-rels[0][4])),
<a name="line_291"></a>	     numer(factor(rels[0][5]))];
<a name="line_292"></a> rels[2] := Basis(rels[1],vars_x);
<a name="line_293"></a>
<a name="line_294"></a> # We now check that the relation in rels[2] imply
<a name="line_295"></a> # a_E^3*x[4]*((x[1]+x[2])^2/2+x[4]^2) = 0.
<a name="line_296"></a> 
<a name="line_297"></a> _ASSERT(
<a name="line_298"></a>  NormalForm(a_E^3*x[4]*((x[1]+x[2])^2/2+x[4]^2),rels[2],vars_x) = 0,
<a name="line_299"></a>  "gauss map preimage a"
<a name="line_300"></a> );
<a name="line_301"></a>
<a name="line_302"></a> # The above relation together with positivity arguments gives x[4] = 0,
<a name="line_303"></a> # so we add that to our list of relations.
<a name="line_304"></a> 
<a name="line_305"></a> rels[3] := Basis([x[4],op(rels[2])],vars_x);
<a name="line_306"></a>
<a name="line_307"></a> # We now check that the relation in rels[3] imply
<a name="line_308"></a> # a_E^3*(x[1]-x[2])*(a_E+(x[1]+x[2])^2/2) = 0.
<a name="line_309"></a>
<a name="line_310"></a> _ASSERT(
<a name="line_311"></a>  NormalForm(a_E^3*(x[1]-x[2])*(a_E+(x[1]+x[2])^2/2),rels[3],vars_x) = 0,
<a name="line_312"></a>  "gauss map preimage b"
<a name="line_313"></a> );
<a name="line_314"></a>
<a name="line_315"></a> # The above relation together with positivity arguments gives x[1]-x[2] = 0,
<a name="line_316"></a> # so we add that to our list of relations.
<a name="line_317"></a> 
<a name="line_318"></a> rels[4] := Basis([x[1]-x[2],op(rels[3])],vars_x);
<a name="line_319"></a>
<a name="line_320"></a> # We now check that the relation in rels[3] imply
<a name="line_321"></a> # (1-a_E)*(2*a_E + (1-a_E)*x[3]^2)*x[3] = 0
<a name="line_322"></a>
<a name="line_323"></a> _ASSERT(
<a name="line_324"></a>  NormalForm((1-a_E)*(2*a_E + (1-a_E)*x[3]^2)*x[3],rels[4],vars_x) = 0,
<a name="line_325"></a>  "gauss map preimage c"
<a name="line_326"></a> );
<a name="line_327"></a>  
<a name="line_328"></a> # The above relation together with positivity arguments gives x[3] = 0,
<a name="line_329"></a> # so we add that to our list of relations.
<a name="line_330"></a> 
<a name="line_331"></a> rels[5] := Basis([x[3],op(rels[4])],vars_x);
<a name="line_332"></a>
<a name="line_333"></a> wl := interface(warnlevel = 0);
<a name="line_334"></a> sols := solve([op(rels[5]),op(rels[0])],xx);
<a name="line_335"></a> interface(warnlevel = wl);
<a name="line_336"></a> 
<a name="line_337"></a> sols := map(subs,sols,xx);
<a name="line_338"></a>
<a name="line_339"></a> _ASSERT(sols = [v_E[6]],"gauss map preimage d");
<a name="line_340"></a>end:
<a name="line_341"></a>
<a name="line_342"></a>add_check(check_gauss_map):
<a name="line_343"></a>
  </pre>
 </body>
</html>
    