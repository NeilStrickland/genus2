<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># This function is a homeomorphism from F16 to [0,1]^2 given by 
<a name="line_2"></a># quadratic polynomials.  However, the inverse is not differentiable 
<a name="line_3"></a># along the boundary of [0,1]^2, so it is not a diffeomorphism.
<a name="line_4"></a>
<a name="line_5"></a><span style="color:red">#@ square_homeo 
</span><a name="line_6"></a>square_homeo := (x) -> [
<a name="line_7"></a> x[3]*(x[3] - x[4]/sqrt(2)),
<a name="line_8"></a> x[2]*(x[2] - x[1]) + x[4]*(x[4] - sqrt(2)*x[3])
<a name="line_9"></a>];
<a name="line_10"></a>
<a name="line_11"></a><span style="color:red">#@ square_homeo_cone_a 
</span><a name="line_12"></a>square_homeo_cone_a := (s,t) ->
<a name="line_13"></a> [2*cos(t),cos(t+s)+cos(t),sqrt(2)*sin(t),-sin(t+s)+sin(t)];
<a name="line_14"></a>
<a name="line_15"></a>######################################################################
<a name="line_16"></a># This function gives a smooth embedding from a open neighbourhood of
<a name="line_17"></a># F16 in EX^* to a neighbourhood of [0,1]^2 in R^2.  It restricts to
<a name="line_18"></a># give a diffeomorphism from F16 to [0,1]^2.  The second component is 
<a name="line_19"></a># a quadratic polynomial, but the first term has some cubic terms and
<a name="line_20"></a># some quadratic terms.
<a name="line_21"></a>
<a name="line_22"></a><span style="color:red">#@ sd_alpha
</span><a name="line_23"></a>sd_alpha[0] := (x) -> x[3] - x[4]/sqrt(2) + x[1]^2 + x[4]^2 + x[3] * (x[4] - x[2])/sqrt(2);
<a name="line_24"></a>sd_alpha[1] := (x) -> ((3/4)*sqrt(2)-1)*x[1]+x[2]-x[3]-sqrt(2)*x[4];
<a name="line_25"></a>sd_alpha[2] := (x) -> x[1] - 3/4*sqrt(3)*x[3] + (3 - 3/4*sqrt(6))*x[4];
<a name="line_26"></a>
<a name="line_27"></a><span style="color:red">#@ square_diffeo_E0 
</span><a name="line_28"></a>square_diffeo_E0 := (x) -> [
<a name="line_29"></a> x[3] * sd_alpha[0](x) - x[2]^2 * x[4],
<a name="line_30"></a> (x[2]-x[1]) * sd_alpha[1](x) +
<a name="line_31"></a> x[4] * sd_alpha[2](x)
<a name="line_32"></a>];
<a name="line_33"></a>
<a name="line_34"></a># Here is a different diffeomorphism, which may actually be better.
<a name="line_35"></a>
<a name="line_36"></a><span style="color:red">#@ square_diffeo_E0_alt
</span><a name="line_37"></a>square_diffeo_E0_alt := 
<a name="line_38"></a> (x) -> [x[3]/(x[2]+x[3]),
<a name="line_39"></a>         ((x[1]-x[2])*(1-2*x[1]-2*x[2])-3*sqrt(2)*x[3]*x[4])/(2+2*x[1]-x[2])];
<a name="line_40"></a>
<a name="line_41"></a><span style="color:red">#@ square_diffeo_E0_alt_inv_approx
</span><a name="line_42"></a>square_diffeo_E0_alt_inv_approx := (t) -> 
<a name="line_43"></a> [-(247/164)*t[2]^2*t[1]^2+(129/455)*t[2]^2*t[1]+(192/83)*t[2]*t[1]^2-(62/921)*t[2]^2-(303/790)*t[1]*t[2]-(113/140)*t[1]^2-(119/186)*t[2]+(349/3489)*t[1]+169/239, -(127/234)*t[2]^2*t[1]^2+(386/411)*t[2]^2*t[1]+(254/315)*t[2]*t[1]^2-(379/956)*t[2]^2-(347/232)*t[1]*t[2]-(113/140)*t[1]^2+(375/544)*t[2]+(349/3489)*t[1]+169/239, (695/2213)*t[2]^2*t[1]^2-(373/1262)*t[2]^2*t[1]-(173/240)*t[2]*t[1]^2+(179/345)*t[1]*t[2]-(154/383)*t[1]^2+(537/383)*t[1], -(1013/1124)*t[2]^2*t[1]^2+(298/299)*t[2]^2*t[1]+(165/112)*t[2]*t[1]^2-(294/137)*t[1]*t[2]];
<a name="line_44"></a>
<a name="line_45"></a><span style="color:red">#@ square_diffeo_E0_alt_inv
</span><a name="line_46"></a>square_diffeo_E0_alt_inv := proc(t::RR0_2)
<a name="line_47"></a> local x,x0,x1,sol;
<a name="line_48"></a>
<a name="line_49"></a> x0 := [x[1],x[2],x[3],x[4]];
<a name="line_50"></a> x1 := evalf(square_diffeo_E0_alt_inv_approx(t));
<a name="line_51"></a> 
<a name="line_52"></a> sol := fsolve(
<a name="line_53"></a>         [op(square_diffeo_E0_alt(x0) -~ t),rho(x0)-1,g0(x0)],
<a name="line_54"></a>         {x[1] = x1[1],x[2] = x1[2],x[3] = x1[3],x[4] = x1[4]}
<a name="line_55"></a>        );
<a name="line_56"></a> return subs(sol,x0);
<a name="line_57"></a>end:
<a name="line_58"></a>
<a name="line_59"></a>######################################################################
<a name="line_60"></a># This function finds the preimage under square_diffeo_E0 of a specified point
<a name="line_61"></a># in [0,1]^2 by an iterative search.  I am not sure whether this is actually
<a name="line_62"></a># better than using fsolve().  I previously had some problems with fsolve(),
<a name="line_63"></a># but I think that they may have been caused by a misunderstanding about
<a name="line_64"></a># the behaviour of evalf[n](...).
<a name="line_65"></a>
<a name="line_66"></a><span style="color:red">#@ square_diffeo_E0_inverse_search 
</span><a name="line_67"></a>square_diffeo_E0_inverse_search := proc(t0::[numeric,numeric])
<a name="line_68"></a> local s,s0,tol,n,i,e,u,uu,n0,x0,dx,b0,db,sda,sdj,J0,err;
<a name="line_69"></a>
<a name="line_70"></a> # We handle points on the boundary of the square separately, to ensure that
<a name="line_71"></a> # we get points that lie exactly on the corresponding loci in EX^*.
<a name="line_72"></a>
<a name="line_73"></a> if t0[1] = 0 then
<a name="line_74"></a>  if t0[2] = 0 then
<a name="line_75"></a>   return v_E1[6];
<a name="line_76"></a>  elif t0[2] = 1 then
<a name="line_77"></a>   return v_E1[3];
<a name="line_78"></a>  else 
<a name="line_79"></a>   s0 := fsolve(square_diffeo_E0(c_E0[0](s))[2] = t0[2],s=evalf(Pi/4*(1+t0[2])));
<a name="line_80"></a>   return evalf(c_E0[0](s0));
<a name="line_81"></a>  fi;
<a name="line_82"></a> elif t0[1] = 1 then
<a name="line_83"></a>  if t0[2] = 0 then
<a name="line_84"></a>   return v_E1[0];
<a name="line_85"></a>  elif t0[2] = 1 then
<a name="line_86"></a>   return v_E1[11];
<a name="line_87"></a>  else 
<a name="line_88"></a>   s0 := fsolve(square_diffeo_E0(c_E0[5](s))[2] = t0[2],s=evalf(Pi*t0[2]));
<a name="line_89"></a>   return evalf(c_E0[5](s0));
<a name="line_90"></a>  fi;
<a name="line_91"></a> else 
<a name="line_92"></a>  if t0[2] = 0 then
<a name="line_93"></a>   s0 := fsolve(square_diffeo_E0(c_E0[1](s))[1] = t0[1],s=evalf(Pi/2*(1-t0[1])));
<a name="line_94"></a>   return evalf(c_E0[1](s0));
<a name="line_95"></a>  elif t0[2] = 1 then
<a name="line_96"></a>   s0 := fsolve(square_diffeo_E0(c_E0[3](s))[1] = t0[1],s=evalf(Pi/2*(1-t0[1])));
<a name="line_97"></a>   return evalf(c_E0[3](s0));
<a name="line_98"></a>  fi;
<a name="line_99"></a> fi;
<a name="line_100"></a>
<a name="line_101"></a> # The rest of this function handles the general case, where t0 is
<a name="line_102"></a> # not on the boundary of the square.
<a name="line_103"></a>
<a name="line_104"></a> x0 :=
<a name="line_105"></a>   [-2.05850*t0[1]^2*t0[2]^2+0.918130*t0[1]*t0[2]^2+2.66136*t0[1]^2*t0[2]-0.115622*t0[2]^2-
<a name="line_106"></a>    0.813875*t0[1]*t0[2]-0.602855*t0[1]^2-0.591483*t0[2]-0.104250*t0[1]+0.707105,
<a name="line_107"></a>    0.129961*t0[1]^2*t0[2]^2+0.254890*t0[1]*t0[2]^2-0.479913*t0[1]^2*t0[2]-0.384851*t0[2]^2-
<a name="line_108"></a>    0.197833*t0[1]*t0[2]-0.602855*t0[1]^2+0.677745*t0[2]-0.104250*t0[1]+0.707105,
<a name="line_109"></a>    -0.492388*t0[1]^2*t0[2]^2+0.473871*t0[1]*t0[2]^2+0.818861*t0[1]^2*t0[2]-
<a name="line_110"></a>    0.983847*t0[1]*t0[2]-0.825307*t0[1]^2+1.82530*t0[1],
<a name="line_111"></a>    -1.95651*t0[1]^2*t0[2]^2+2.15931*t0[1]*t0[2]^2+2.30924*t0[1]^2*t0[2]-3.08939*t0[1]*t0[2]];
<a name="line_112"></a>
<a name="line_113"></a> sda := unapply(evalf(expand([op(square_diffeo_E0(xx)),rho(xx) - 1,g0(xx)])),x):
<a name="line_114"></a> sdj := unapply([seq(map(diff,sda(x),x[i]),i=1..4)],x):
<a name="line_115"></a>
<a name="line_116"></a> x0 := Vector(x0);
<a name="line_117"></a> b0 := Vector([op(t0),0,0]);
<a name="line_118"></a> db := b0 - Vector(sda(x0));
<a name="line_119"></a> n := 100;
<a name="line_120"></a> tol := 10.^(-95);
<a name="line_121"></a>
<a name="line_122"></a> err := Norm(db);
<a name="line_123"></a>
<a name="line_124"></a> while err > tol and n > 0 do 
<a name="line_125"></a>  n := n-1;
<a name="line_126"></a>  J0 := Transpose(Matrix(sdj(x0)));
<a name="line_127"></a>  dx := LinearSolve(J0,db);
<a name="line_128"></a>  x0 := x0 + dx;
<a name="line_129"></a>  db := b0 - Vector(sda(x0));
<a name="line_130"></a>  err := Norm(db);
<a name="line_131"></a> od:
<a name="line_132"></a>
<a name="line_133"></a> return convert(x0,list);
<a name="line_134"></a>end:
<a name="line_135"></a>
<a name="line_136"></a>######################################################################
<a name="line_137"></a># This function tabulates some values of the inverse of square_diffeo_E0(),
<a name="line_138"></a># and computes a Chebyshev approximation to that inverse.
<a name="line_139"></a>
<a name="line_140"></a><span style="color:red">#@ find_square_diffeo_E0_inverse 
</span><a name="line_141"></a><span style="color:red">#@ square_diffeo_E0_inverse_order
</span><a name="line_142"></a><span style="color:red">#@ square_diffeo_E0_inverse
</span><a name="line_143"></a><span style="color:red">#@ square_diffeo_E0_inverse_table
</span><a name="line_144"></a><span style="color:red">#@ square_diffeo_E0_inverse_chebyshev_table
</span><a name="line_145"></a>
<a name="line_146"></a>find_square_diffeo_E0_inverse := proc(NN::posint)
<a name="line_147"></a> global square_diffeo_E0_inverse_order,square_diffeo_E0_inverse,
<a name="line_148"></a>        square_diffeo_E0_inverse_table,
<a name="line_149"></a>        square_diffeo_E0_inverse_chebyshev_table;
<a name="line_150"></a> local CP,SDI,i,j,k,l,m,x0,sol,aa;
<a name="line_151"></a>
<a name="line_152"></a> square_diffeo_E0_inverse_order := NN;
<a name="line_153"></a>
<a name="line_154"></a> # CP is a table of NN+1 Chebyshev points for the interval [0,1], indexed by 
<a name="line_155"></a> # 0 to NN.  Recall that all these points are actually in (0,1).
<a name="line_156"></a> CP := table():
<a name="line_157"></a> for i from 0 to NN do
<a name="line_158"></a>  CP[i] := evalf(1/2 + cos((2*NN-2*i+1)*Pi/(2*NN+2))/2):
<a name="line_159"></a> od:
<a name="line_160"></a>
<a name="line_161"></a> # It turns out to be convenient to add in the end points.  These are not used
<a name="line_162"></a> # in the Chebyshev approximation formula, but they are used to provide a 
<a name="line_163"></a> # starting point when searching for values of the inverse function.
<a name="line_164"></a> CP[-1] := 0:
<a name="line_165"></a> CP[NN+1] := 1:
<a name="line_166"></a>
<a name="line_167"></a> SDI := table():
<a name="line_168"></a> SDI[  -1,  -1] := evalf(v_E0[ 6]):
<a name="line_169"></a> SDI[NN+1,  -1] := evalf(v_E0[ 0]):
<a name="line_170"></a> SDI[  -1,NN+1] := evalf(v_E0[ 3]):
<a name="line_171"></a> SDI[NN+1,NN+1] := evalf(v_E0[11]):
<a name="line_172"></a>
<a name="line_173"></a> userinfo(5,genus2,"Finding edge Chebyshev points");
<a name="line_174"></a>
<a name="line_175"></a> for i from 0 to NN do
<a name="line_176"></a>  SDI[i,-1] := 
<a name="line_177"></a>   evalf(c_E0[1](fsolve(square_diffeo_E0(c_E0[1](t))[1] - CP[i],t=0..Pi/2)));
<a name="line_178"></a>
<a name="line_179"></a>  SDI[i,NN+1] := 
<a name="line_180"></a>   evalf(c_E0[3](fsolve(square_diffeo_E0(c_E0[3](t))[1] - CP[i],t=0..Pi/2)));
<a name="line_181"></a>
<a name="line_182"></a>  SDI[-1,i] := 
<a name="line_183"></a>   evalf(c_E0[0](fsolve(square_diffeo_E0(c_E0[0](t))[2] - CP[i],t=Pi/4..Pi/2)));
<a name="line_184"></a>
<a name="line_185"></a>  SDI[NN+1,i] :=
<a name="line_186"></a>   evalf(c_E0[5](fsolve(square_diffeo_E0(c_E0[5](t))[2] - CP[i],t=0..Pi)));
<a name="line_187"></a> od:
<a name="line_188"></a>
<a name="line_189"></a> userinfo(5,genus2,"Finding interior Chebyshev points");
<a name="line_190"></a>
<a name="line_191"></a> for i from 0 to NN do
<a name="line_192"></a>  for j from 0 to NN do
<a name="line_193"></a>#   x0 := SDI[i-1,j];
<a name="line_194"></a>#   sol := fsolve({op(square_diffeo_E0(x) -~ [CP[i],CP[j]]),rho(x)-1,g0(x)},{seq(x[i]=x0[i],i=1..4)});
<a name="line_195"></a>#   SDI[i,j] := subs(sol,xx);
<a name="line_196"></a>   SDI[i,j] := square_diffeo_E0_inverse_search([CP[i],CP[j]]);
<a name="line_197"></a>  od:
<a name="line_198"></a> od:
<a name="line_199"></a>
<a name="line_200"></a> userinfo(5,genus2,"Finding Chebyshev approximation");
<a name="line_201"></a>
<a name="line_202"></a> aa := table():
<a name="line_203"></a> for i from 0 to NN do
<a name="line_204"></a>  for j from 0 to NN do
<a name="line_205"></a>   for m from 1 to 4 do
<a name="line_206"></a>    aa[i,j,m] := add(add(SDI[k,l][m]*ChebyshevT(i,2*CP[k]-1)*ChebyshevT(j,2*CP[l]-1)*4/(NN+1)^2,k=0..NN),l=0..NN);
<a name="line_207"></a>    if i = 0 then aa[i,j,m] := aa[i,j,m]/2; fi;
<a name="line_208"></a>    if j = 0 then aa[i,j,m] := aa[i,j,m]/2; fi;
<a name="line_209"></a>   od:
<a name="line_210"></a>  od:
<a name="line_211"></a> od:
<a name="line_212"></a>
<a name="line_213"></a> square_diffeo_E0_inverse :=
<a name="line_214"></a>  unapply(expand([seq(add(add(aa[i,j,m]*ChebyshevT(i,2*t[1]-1)*ChebyshevT(j,2*t[2]-1),j=0..NN),i=0..NN),m=1..4)]),t):
<a name="line_215"></a>
<a name="line_216"></a> square_diffeo_E0_inverse_chebyshev_table := eval(SDI);
<a name="line_217"></a>
<a name="line_218"></a> userinfo(5,genus2,"Finding interior grid points");
<a name="line_219"></a>
<a name="line_220"></a> square_diffeo_E0_inverse_table := table();
<a name="line_221"></a>
<a name="line_222"></a> for i from 0 to NN do
<a name="line_223"></a>  for j from 0 to NN do
<a name="line_224"></a>   square_diffeo_E0_inverse_table[i/NN,j/NN] :=  
<a name="line_225"></a>    square_diffeo_E0_inverse_search([i/NN,j/NN],square_diffeo_E0_inverse([i/NN,j/NN]));
<a name="line_226"></a>  od:
<a name="line_227"></a> od:
<a name="line_228"></a>
<a name="line_229"></a> NULL;
<a name="line_230"></a>end:
<a name="line_231"></a>
<a name="line_232"></a>##################################################
<a name="line_233"></a>
<a name="line_234"></a><span style="color:red">#@ save_square_diffeo_E0_inverse 
</span><a name="line_235"></a>save_square_diffeo_E0_inverse := proc()
<a name="line_236"></a> save(square_diffeo_E0_inverse,
<a name="line_237"></a>      square_diffeo_E0_inverse_order,
<a name="line_238"></a>      square_diffeo_E0_inverse_table,
<a name="line_239"></a>      square_diffeo_E0_inverse_chebyshev_table,
<a name="line_240"></a>      cat(data_dir,"/embedded/roothalf/square_diffeo_E0_inverse.m"));
<a name="line_241"></a>end:
<a name="line_242"></a>
<a name="line_243"></a><span style="color:red">#@ load_square_diffeo_E0_inverse 
</span><a name="line_244"></a>load_square_diffeo_E0_inverse := proc()
<a name="line_245"></a> read(cat(data_dir,"/embedded/roothalf/square_diffeo_E0_inverse.m"));
<a name="line_246"></a>end:
<a name="line_247"></a>
<a name="line_248"></a><span style="color:red">#@ require_square_diffeo_E0_inverse 
</span><a name="line_249"></a>require_square_diffeo_E0_inverse := proc()
<a name="line_250"></a> if not type(square_diffeo_E0_inverse_order,integer) then
<a name="line_251"></a>  load_square_diffeo_E0_inverse();
<a name="line_252"></a> fi;
<a name="line_253"></a>end:
<a name="line_254"></a>
<a name="line_255"></a>######################################################################
<a name="line_256"></a>
<a name="line_257"></a><span style="color:red">#@ make_square_diffeo_E0_inverse_plot 
</span><a name="line_258"></a>make_square_diffeo_E0_inverse_plot := proc()
<a name="line_259"></a> global pics;
<a name="line_260"></a>
<a name="line_261"></a> local T,N,i,j;
<a name="line_262"></a>
<a name="line_263"></a> T := square_diffeo_E0_inverse_table:
<a name="line_264"></a> N := square_diffeo_E0_inverse_order:
<a name="line_265"></a> 
<a name="line_266"></a> pics["square_diffeo_E0_inverse"] :=
<a name="line_267"></a>  display(seq(seq(op([
<a name="line_268"></a>   polygon([stereo(T[i/N,j/N]),stereo(T[(i+1)/N,j/N]),stereo(T[(i+1)/N,(j+1)/N])],style=patchnogrid),
<a name="line_269"></a>   polygon([stereo(T[i/N,j/N]),stereo(T[i/N,(j+1)/N]),stereo(T[(i+1)/N,(j+1)/N])],style=patchnogrid)]),
<a name="line_270"></a>   j=0..N-1),i=0..N-1),
<a name="line_271"></a>   axes=none,scaling=constrained);
<a name="line_272"></a>
<a name="line_273"></a> save_plot("square_diffeo_E0_inverse");
<a name="line_274"></a> save_jpg("square_diffeo_E0_inverse");
<a name="line_275"></a>
<a name="line_276"></a> pics["square_diffeo_E0_inverse"];
<a name="line_277"></a>end:
<a name="line_278"></a>
<a name="line_279"></a>######################################################################
<a name="line_280"></a># This is the most general function R^2 -> R^4 with the following
<a name="line_281"></a># properties:
<a name="line_282"></a>#
<a name="line_283"></a># 0) Each entry is polynomial with degree <= 2 in each of t[1] and t[2]
<a name="line_284"></a># 1) The corners of [0,1]^2 map to v[0], v[3], v[6] and v[11] in the 
<a name="line_285"></a>#     usual order.
<a name="line_286"></a># 2) The edges are mapped in accordance with the patterns
<a name="line_287"></a>#     [0,t] |-> [x[1], x[2], 0, 0] 
<a name="line_288"></a>#     [1,t] |-> [x[1], 0, x[3], x[4]]
<a name="line_289"></a>#     [t,0] |-> [x[1], x[1], x[2], 0]
<a name="line_290"></a>#     [t,1] |-> [0, x[2], x[3], -x[3]/sqrt(2)].
<a name="line_291"></a>#
<a name="line_292"></a># The point is that the inverse of square_diffeo_E0 also has
<a name="line_293"></a># properties (1) and (2).
<a name="line_294"></a>
<a name="line_295"></a>eqs := map(coeffs,expand([sdi([0,t[2]])[3],sdi([0,t[2]])[4],sdi([1,t[2]])[2],sdi([t[1],0])[2]-sdi([t[1],0])[1],sdi([t[1],0])[4],sdi([t[1],1])[1],sdi([t[1],1])[3]+sqrt(2)*sdi([t[1],1])[4]]),[t[1],t[2]]);
<a name="line_296"></a>
<a name="line_297"></a><span style="color:red">#@ square_diffeo_E0_inverse_pattern 
</span><a name="line_298"></a>square_diffeo_E0_inverse_pattern := (A,t) -> [
<a name="line_299"></a> A[9]*t[1]^2*t[2]^2+A[6]*t[1]*t[2]^2-t[1]^2*t[2]*A[9]-t[1]^2*t[2]*A[16]+A[3]*t[2]^2+
<a name="line_300"></a>  t[1]*t[2]*A[16]+(1/2)*t[1]*t[2]*sqrt(2)-t[1]*t[2]*A[6]+A[16]*t[1]^2-
<a name="line_301"></a>  (1/2)*sqrt(2)*t[2]-t[2]*A[3]-t[1]*A[16]-(1/2)*sqrt(2)*t[1]+(1/2)*sqrt(2),
<a name="line_302"></a> A[18]*t[1]^2*t[2]^2+A[15]*t[1]*t[2]^2+A[17]*t[1]^2*t[2]-t[2]^2*A[15]-t[2]^2*A[18]-
<a name="line_303"></a> t[1]*t[2]*A[15]-t[1]*t[2]*A[18]-t[1]*t[2]-t[1]*t[2]*A[17]+(1/2)*t[1]*t[2]*sqrt(2)+
<a name="line_304"></a> A[16]*t[1]^2+t[2]*A[15]+t[2]*A[18]+t[2]-(1/2)*sqrt(2)*t[2]-t[1]*A[16]-
<a name="line_305"></a> (1/2)*sqrt(2)*t[1]+(1/2)*sqrt(2),
<a name="line_306"></a> A[27]*t[1]^2*t[2]^2+A[24]*t[1]*t[2]^2+A[26]*t[1]^2*t[2]-t[1]*t[2]*A[26]-
<a name="line_307"></a>  t[1]*t[2]*A[27]-t[1]*t[2]*A[24]+(1/3)*sqrt(6)*t[1]*t[2]-t[1]*t[2]-A[26]*t[1]^2-
<a name="line_308"></a>  sqrt(2)*A[35]*t[1]^2-sqrt(2)*A[36]*t[1]^2-A[27]*t[1]^2+sqrt(2)*t[1]*A[36]+
<a name="line_309"></a>  sqrt(2)*t[1]*A[35]+t[1]*A[26]+t[1]*A[27]+t[1],
<a name="line_310"></a> A[36]*t[1]^2*t[2]^2+A[33]*t[1]*t[2]^2+A[35]*t[1]^2*t[2]-t[1]*t[2]*A[36]-
<a name="line_311"></a>  t[1]*t[2]*A[33]-t[1]*t[2]*A[35]-(1/3)*sqrt(3)*t[1]*t[2]
<a name="line_312"></a> ]:
<a name="line_313"></a>
<a name="line_314"></a>######################################################################
<a name="line_315"></a>
<a name="line_316"></a><span style="color:red">#@ make_square_diffeo_E0_record 
</span><a name="line_317"></a>make_square_diffeo_E0_record := proc(x0)
<a name="line_318"></a> local u,e,n0,u0,v0,t0,tu0,tv0,J0,eqs,sol,x1,P;
<a name="line_319"></a> n0 := evalf(dg0(x0));
<a name="line_320"></a> n0 := n0 /~ nm4(n0);
<a name="line_321"></a> t0 := evalf(square_diffeo_E0(x0));
<a name="line_322"></a> u0 := [u[1],u[2],u[3],u[4]];
<a name="line_323"></a> x1 := x0 +~ e *~ u0;
<a name="line_324"></a> eqs := expand(evalf([rho(x1),g0(x1),op(square_diffeo_E0(x1))]));
<a name="line_325"></a> eqs := map(coeff,eqs,e,1) -~ [0,0,1,0];
<a name="line_326"></a> sol := solve(eqs);
<a name="line_327"></a> u0 := evalf(subs(sol,u0));
<a name="line_328"></a> u0 := u0 /~ nm4(u0);
<a name="line_329"></a> v0 := cross_product4(x0,n0,u0);
<a name="line_330"></a> tu0 := evalf(map(coeff,expand(square_diffeo_E0(x0 +~ e *~ u0)),e,1));
<a name="line_331"></a> tv0 := evalf(map(coeff,expand(square_diffeo_E0(x0 +~ e *~ v0)),e,1));
<a name="line_332"></a> J0 := Determinant(Matrix([tu0,tv0]));
<a name="line_333"></a> P := table();
<a name="line_334"></a> P["x"] := x0;
<a name="line_335"></a> P["n"] := n0;
<a name="line_336"></a> P["u"] := u0;
<a name="line_337"></a> P["v"] := v0;
<a name="line_338"></a> P["t"] := t0;
<a name="line_339"></a> P["tu"] := tu0;
<a name="line_340"></a> P["tv"] := tv0;
<a name="line_341"></a> P["J"] := J0;
<a name="line_342"></a> P["K"] := evalf(curvature0(x0));
<a name="line_343"></a> P["M"] := evalf(rescale_x(x0));
<a name="line_344"></a> return(eval(P));
<a name="line_345"></a>end:
<a name="line_346"></a>
<a name="line_347"></a>######################################################################
<a name="line_348"></a>
<a name="line_349"></a><span style="color:red">#@ square_diffeo_E0_JM 
</span><a name="line_350"></a>square_diffeo_E0_JM := 
<a name="line_351"></a> unapply(Matrix([xx,dg0(xx),seq([seq(diff(square_diffeo_E0(xx)[i],x[j]),j=1..4)],i=1..2)]),x);
<a name="line_352"></a>
<a name="line_353"></a><span style="color:red">#@ square_diffeo_E0_J 
</span><a name="line_354"></a>square_diffeo_E0_J := unapply(simplify(NF_x0(Determinant(square_diffeo_E0_JM(x)))/nm4(dg0(xx))),x);
<a name="line_355"></a>
<a name="line_356"></a><span style="color:red">#@ square_diffeo_E0_alt_JM 
</span><a name="line_357"></a>square_diffeo_E0_alt_JM := 
<a name="line_358"></a> unapply(Matrix([xx,dg0(xx),seq([seq(diff(square_diffeo_E0_alt(xx)[i],x[j]),j=1..4)],i=1..2)]),x);
<a name="line_359"></a>
<a name="line_360"></a><span style="color:red">#@ square_diffeo_E0_alt_J 
</span><a name="line_361"></a>square_diffeo_E0_alt_J := unapply(simplify((Determinant(square_diffeo_E0_alt_JM(x)))/nm4(dg0(xx))),x);
<a name="line_362"></a>
<a name="line_363"></a>######################################################################
<a name="line_364"></a>
<a name="line_365"></a><span style="color:red">#@ sd_plot_base 
</span><a name="line_366"></a>sd_plot_base := display(
<a name="line_367"></a> polygon([[0,0,0],[1,0,0],[1,1,0],[0,1,0]],colour=grey),
<a name="line_368"></a> line([0,0,0],[0,1,0],colour=c_colour[0]),
<a name="line_369"></a> line([0,0,0],[1,0,0],colour=c_colour[1]),
<a name="line_370"></a> line([0,1,0],[1,1,0],colour=c_colour[3]),
<a name="line_371"></a> line([1,0,0],[1,1,0],colour=c_colour[5]),
<a name="line_372"></a> axes=none,scaling=constrained
<a name="line_373"></a>):
<a name="line_374"></a>
<a name="line_375"></a><span style="color:red">#@ sd_plot 
</span><a name="line_376"></a>sd_plot := proc(f)
<a name="line_377"></a> local i,j,SDI,N,P;
<a name="line_378"></a>
<a name="line_379"></a> require_square_diffeo_E0_inverse():
<a name="line_380"></a>
<a name="line_381"></a> SDI := eval(square_diffeo_E0_inverse_table);
<a name="line_382"></a> N := square_diffeo_E0_inverse_order;
<a name="line_383"></a> P := table():
<a name="line_384"></a> for i from 0 to N do
<a name="line_385"></a>  for j from 0 to N do
<a name="line_386"></a>   P[i,j] := [i/N,j/N,evalf(eval(subs(x = SDI[i/N,j/N],f)))];
<a name="line_387"></a>  od;
<a name="line_388"></a> od;
<a name="line_389"></a>
<a name="line_390"></a> display(
<a name="line_391"></a>  seq(seq(polygon([P[i,j],P[i+1,j],P[i+1,j+1]],style=patchnogrid),i=0..N-1),j=0..N-1),
<a name="line_392"></a>  seq(seq(polygon([P[i,j],P[i,j+1],P[i+1,j+1]],style=patchnogrid),i=0..N-1),j=0..N-1)
<a name="line_393"></a> );
<a name="line_394"></a>
<a name="line_395"></a>end:
  </pre>
 </body>
</html>
    