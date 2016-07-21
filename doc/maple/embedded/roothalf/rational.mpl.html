<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># This file contains information about points in EX^* whose coordinates
<a name="line_2"></a># are rational, or rational multiples of sqrt(2), or otherwise 
<a name="line_3"></a># arithmetically simple.
<a name="line_4"></a>
<a name="line_5"></a><span style="color:red">#@ c_rational
</span><a name="line_6"></a>c_rational[0] := (t) -> [t^2-1,2*t,0,0] /~ (t^2+1);
<a name="line_7"></a>c_rational_condition[0] := (t) -> type(t,rational);
<a name="line_8"></a>
<a name="line_9"></a>c_rational[1] := (st) -> [ st[1],st[1],st[2],0];
<a name="line_10"></a>c_rational_condition[1] :=
<a name="line_11"></a> (st) -> type(st,[rational,rational]) and 2*st[1]^2 + st[2]^2 = 1;
<a name="line_12"></a>
<a name="line_13"></a>c_rational[2] := (st) -> [-st[1],st[1],st[2],0];
<a name="line_14"></a>c_rational_condition[2] :=
<a name="line_15"></a> (st) -> type(st,[rational,rational]) and 2*st[1]^2 + st[2]^2 = 1;
<a name="line_16"></a>
<a name="line_17"></a><span style="color:red">#@ c_quasirational
</span><a name="line_18"></a>c_quasirational[1] := (t) -> 
<a name="line_19"></a> [  1-t^2-2*t ,1-t^2-2*t,sqrt(2)*(1-t^2+2*t),0] /~ (2*(1+t^2));
<a name="line_20"></a>c_quasirational_condition[1] := (t) -> type(t,rational);
<a name="line_21"></a>
<a name="line_22"></a>c_quasirational[2] := (t) -> 
<a name="line_23"></a> [-(1-t^2-2*t),1-t^2-2*t,sqrt(2)*(1-t^2+2*t),0] /~ (2*(1+t^2));
<a name="line_24"></a>c_quasirational_condition[2] := (t) -> type(t,rational);
<a name="line_25"></a>
<a name="line_26"></a>c_quasirational[3] := (st) -> [0, st[2],sqrt(2)*st[1],-st[1]];
<a name="line_27"></a>c_quasirational_condition[3] :=
<a name="line_28"></a> (st) -> type(st,[rational,rational]) and 3*st[1]^2 + st[2]^2 = 1;
<a name="line_29"></a>
<a name="line_30"></a>c_quasirational[4] := (st) -> [-st[2],0,sqrt(2)*st[1], st[1]];
<a name="line_31"></a>c_quasirational_condition[4] :=
<a name="line_32"></a> (st) -> type(st,[rational,rational]) and 3*st[1]^2 + st[2]^2 = 1;
<a name="line_33"></a>
<a name="line_34"></a>c_quasirational[5] := (t) -> [sqrt((1-t)*t/(t+2)), 0, sqrt(2/(t+2)), -sqrt(t^2/(t+2))];
<a name="line_35"></a>
<a name="line_36"></a>##################################################
<a name="line_37"></a>
<a name="line_38"></a># farey_count(n) is the number of rationals in [0,1] with denominator
<a name="line_39"></a># at most n.
<a name="line_40"></a>
<a name="line_41"></a><span style="color:red">#@ farey_count 
</span><a name="line_42"></a>farey_count := (n) -> 1 + add(numtheory[phi](i),i=1..n):
<a name="line_43"></a>
<a name="line_44"></a># farey(n) is the list of rationals in [0,1] with denominator at most n.
<a name="line_45"></a>
<a name="line_46"></a><span style="color:red">#@ farey 
</span><a name="line_47"></a>farey := proc(n)
<a name="line_48"></a> local F,a,b,c,d,a0,b0,k;
<a name="line_49"></a>
<a name="line_50"></a> a := 0;
<a name="line_51"></a> b := 1;
<a name="line_52"></a> c := 1;
<a name="line_53"></a> d := n;
<a name="line_54"></a>
<a name="line_55"></a> F := a/b;
<a name="line_56"></a>
<a name="line_57"></a> while c <= n do 
<a name="line_58"></a>  k := floor((n+b)/d);
<a name="line_59"></a>  a0 := a;
<a name="line_60"></a>  b0 := b;
<a name="line_61"></a>  a := c;
<a name="line_62"></a>  b := d;
<a name="line_63"></a>  c := k*c - a0;
<a name="line_64"></a>  d := k*d - b0;
<a name="line_65"></a>  F := F,a/b;
<a name="line_66"></a> od;
<a name="line_67"></a>
<a name="line_68"></a> return [F];
<a name="line_69"></a>end:
<a name="line_70"></a>
<a name="line_71"></a>##################################################
<a name="line_72"></a># This section is about solutions to 2 s^2 + t^2 = 1.
<a name="line_73"></a>
<a name="line_74"></a># two_roots is the set of roots of unity in Q(sqrt(-2)).
<a name="line_75"></a>
<a name="line_76"></a><span style="color:red">#@ two_roots 
</span><a name="line_77"></a>two_roots := [1,-1];
<a name="line_78"></a>
<a name="line_79"></a># two_primes is a list of prime numbers that factor in Q(sqrt(-2))
<a name="line_80"></a># For each such prime p, two_pi[p] is the prime element of 
<a name="line_81"></a># Q(sqrt(-2)) in the upper half plane that divides p, and 
<a name="line_82"></a># two_u[p] is two_pi[p] / conjugate(two_pi[p]), which has
<a name="line_83"></a># norm one.
<a name="line_84"></a>
<a name="line_85"></a><span style="color:red">#@ two_primes 
</span><a name="line_86"></a>two_primes := NULL;
<a name="line_87"></a>
<a name="line_88"></a><span style="color:red">#@ two_pi
</span><a name="line_89"></a><span style="color:red">#@ two_u
</span><a name="line_90"></a>for i from 2 to 200 do
<a name="line_91"></a> p := ithprime(i);
<a name="line_92"></a> if modp(p,8) = 1 or modp(p,8) = 3 then
<a name="line_93"></a>  two_primes := two_primes,p;
<a name="line_94"></a>  uv := select(ab -> (ab[1]>0 and ab[2]>0),map(s -> subs(s,[a,b]),[isolve(p=2*a^2+b^2)]))[1]; 
<a name="line_95"></a>  two_pi[p] := uv[1]*sqrt(-2) + uv[2]; 
<a name="line_96"></a>  two_u[p] := expand(rationalize(two_pi[p]/conjugate(two_pi[p])));
<a name="line_97"></a> fi;
<a name="line_98"></a>od;
<a name="line_99"></a>
<a name="line_100"></a># two_units_aux(n) is a list of units in Q(sqrt(-2)) with
<a name="line_101"></a># denominator equal to n.
<a name="line_102"></a>
<a name="line_103"></a><span style="color:red">#@ two_units_aux 
</span><a name="line_104"></a>two_units_aux := proc(n)
<a name="line_105"></a> local ff,f,Q;
<a name="line_106"></a> if n = 1 then return {1}; fi;
<a name="line_107"></a> ff := ifactor(n);
<a name="line_108"></a> if type(ff,`*`) then ff := [op(ff)]; else ff := [ff]; fi;
<a name="line_109"></a> ff := map(f -> `if`(type(f,`^`),[op(f)],[f,1]),ff);
<a name="line_110"></a> ff := map(f -> two_u[op(op(1,f))]^op(2,f),ff);
<a name="line_111"></a> Q := {1};
<a name="line_112"></a> for f in ff do
<a name="line_113"></a>  Q := {op(Q *~ f),op(Q /~ f)};
<a name="line_114"></a> od;
<a name="line_115"></a> Q := expand(rationalize(Q));
<a name="line_116"></a> return(Q);
<a name="line_117"></a>end:
<a name="line_118"></a>
<a name="line_119"></a><span style="color:red">#@ two_circle 
</span><a name="line_120"></a>two_circle := proc(max_denom)
<a name="line_121"></a> local NN,C;
<a name="line_122"></a>
<a name="line_123"></a> NN := {seq(n,n=1..max_denom)};
<a name="line_124"></a> NN := select(n -> mods({1,3,op(prime_factors(n))},8) = {1,3},NN);
<a name="line_125"></a> C := map(op,map(two_units_aux,NN));
<a name="line_126"></a> C := map(op,map(u -> expand(u *~ two_roots),C));
<a name="line_127"></a> C := map(u -> [Im(u)/sqrt(2),Re(u)],C);
<a name="line_128"></a> C := sort([op(C)]);
<a name="line_129"></a> return C;
<a name="line_130"></a>end:
<a name="line_131"></a>
<a name="line_132"></a>##################################################
<a name="line_133"></a># This section is about solutions to 3 s^2 + t^2 = 1.
<a name="line_134"></a>
<a name="line_135"></a># three_roots is the set of roots of unity in Q(sqrt(-3)).
<a name="line_136"></a>
<a name="line_137"></a><span style="color:red">#@ three_roots 
</span><a name="line_138"></a>three_roots := simplify([seq(exp(Pi*I*k/3),k=0..5)]);
<a name="line_139"></a>
<a name="line_140"></a># three_primes is a list of prime numbers that factor in Q(sqrt(-3))
<a name="line_141"></a># For each such prime p, three_pi[p] is the prime element of 
<a name="line_142"></a># Q(sqrt(-3)) in the upper half plane that divides p, and 
<a name="line_143"></a># three_u[p] is three_pi[p] / conjugate(three_pi[p]), which has
<a name="line_144"></a># norm one.
<a name="line_145"></a>
<a name="line_146"></a><span style="color:red">#@ three_primes 
</span><a name="line_147"></a>three_primes := NULL:
<a name="line_148"></a>
<a name="line_149"></a><span style="color:red">#@ three_pi
</span><a name="line_150"></a><span style="color:red">#@ three_u
</span><a name="line_151"></a>for i from 3 to 200 do
<a name="line_152"></a> p := ithprime(i);
<a name="line_153"></a> if mods(p,3) = 1 then
<a name="line_154"></a>  three_primes := three_primes,p;
<a name="line_155"></a>  uv := select(ab -> (ab[1]>0 and ab[2]>0),map(s -> subs(s,[a,b]),[isolve(p=3*a^2+b^2)]))[1]; 
<a name="line_156"></a>  three_pi[p] := uv[1]*sqrt(-3) + uv[2]; 
<a name="line_157"></a>  three_u[p] := expand(rationalize(three_pi[p]/conjugate(three_pi[p])));
<a name="line_158"></a> fi;
<a name="line_159"></a>od:
<a name="line_160"></a>
<a name="line_161"></a># three_units_aux(n) is a list of units in Q(sqrt(-3)) with
<a name="line_162"></a># denominator equal to n.
<a name="line_163"></a>
<a name="line_164"></a><span style="color:red">#@ three_units_aux 
</span><a name="line_165"></a>three_units_aux := proc(n)
<a name="line_166"></a> local ff,f,Q;
<a name="line_167"></a> if n = 1 then return {1}; fi;
<a name="line_168"></a> ff := ifactor(n);
<a name="line_169"></a> if type(ff,`*`) then ff := [op(ff)]; else ff := [ff]; fi;
<a name="line_170"></a> ff := map(f -> `if`(type(f,`^`),[op(f)],[f,1]),ff);
<a name="line_171"></a> ff := map(f -> three_u[op(op(1,f))]^op(2,f),ff);
<a name="line_172"></a> Q := {1};
<a name="line_173"></a> for f in ff do
<a name="line_174"></a>  Q := {op(Q *~ f),op(Q /~ f)};
<a name="line_175"></a> od;
<a name="line_176"></a> Q := expand(rationalize(Q));
<a name="line_177"></a> return(Q);
<a name="line_178"></a>end:
<a name="line_179"></a>
<a name="line_180"></a><span style="color:red">#@ three_circle 
</span><a name="line_181"></a>three_circle := proc(max_denom)
<a name="line_182"></a> local NN,C;
<a name="line_183"></a>
<a name="line_184"></a> NN := {seq(n,n=1..max_denom)};
<a name="line_185"></a> NN := select(n -> mods({1,op(prime_factors(n))},3) = {1},NN);
<a name="line_186"></a> C := map(op,map(three_units_aux,NN));
<a name="line_187"></a> C := map(op,map(u -> expand(u *~ three_roots),C));
<a name="line_188"></a> C := map(u -> [Im(u)/sqrt(3),Re(u)],C);
<a name="line_189"></a> C := sort([op(C)]);
<a name="line_190"></a> return C;
<a name="line_191"></a>end:
<a name="line_192"></a>
<a name="line_193"></a>######################################################################
<a name="line_194"></a>
<a name="line_195"></a><span style="color:red">#@ elliptic_t
</span><a name="line_196"></a>elliptic_t := (x) -> x[1]^2;
<a name="line_197"></a>
<a name="line_198"></a><span style="color:red">#@ elliptic_u
</span><a name="line_199"></a>elliptic_u := (x) -> x[1]*(x[4]^2-5/sqrt(2)*x[3]*x[4]-1);
<a name="line_200"></a>
<a name="line_201"></a><span style="color:red">#@ elliptic_r
</span><a name="line_202"></a>elliptic_r[0] := (x) -> x[1]^2+x[3]^2+x[4]^2-1;
<a name="line_203"></a>elliptic_r[1] := (x) -> x[1]^2+x[4]^2+x[3]*x[4]/sqrt(2);
<a name="line_204"></a>
<a name="line_205"></a><span style="color:red">#@ elliptic_a
</span><a name="line_206"></a>elliptic_a[0] := (x) -> -6*sqrt(2)*(x[3]+sqrt(2)*x[4])*x[4]^3;
<a name="line_207"></a>elliptic_a[1] := (x) -> (1-x[3]^2-x[4]^2) * (x[3]^2 -10*x[4]^2 + 9 + x[3]*x[4]/sqrt(2)); 
<a name="line_208"></a>elliptic_a[2] := (x) -> x[3]^2 + 2*x[4]^2 + x[3]*x[4]/sqrt(2) - x[1]^2 + 9;
<a name="line_209"></a>
<a name="line_210"></a><span style="color:red">#@ elliptic_rel
</span><a name="line_211"></a>elliptic_rel := (u,t) -> u^2 - (t^3 - 10*t^2 + t);
<a name="line_212"></a>
<a name="line_213"></a>######################################################################
<a name="line_214"></a>
<a name="line_215"></a><span style="color:red">#@ quasirational_proj 
</span><a name="line_216"></a>quasirational_proj := (x) -> [x[3]/sqrt(2),x[2]^2 - x[1]^2 - 3*x[3]*x[4]/sqrt(2)];
<a name="line_217"></a>
<a name="line_218"></a><span style="color:red">#@ quasirational_lift 
</span><a name="line_219"></a>quasirational_lift := proc(s)
<a name="line_220"></a> local t1,t2,t3,p1,p2,p3,p4;
<a name="line_221"></a>
<a name="line_222"></a> t1 := s[1]^2;
<a name="line_223"></a> t2 := 6*s[1]^2 - 2;
<a name="line_224"></a> t3 := -2*s[2]^2-4;
<a name="line_225"></a> p1 := 2+t1*t3;
<a name="line_226"></a> p2 := s[2]*t2;
<a name="line_227"></a> p3 := p1+p2;
<a name="line_228"></a> p4 := p1-p2;
<a name="line_229"></a> return([sqrt(p3)/2,sqrt(p4)/2,sqrt(2)*s[1],-s[1]*s[2]]);
<a name="line_230"></a>end;
<a name="line_231"></a>
<a name="line_232"></a><span style="color:red">#@ is_quasirational 
</span><a name="line_233"></a>is_quasirational := proc(x)
<a name="line_234"></a> type(x /~ [1,1,sqrt(2),1],[rational $ 4]);
<a name="line_235"></a>end:
<a name="line_236"></a>
<a name="line_237"></a><span style="color:red">#@ inner_quasirational_projections 
</span><a name="line_238"></a>inner_quasirational_projections := [
<a name="line_239"></a>[103/154, 65/449],
<a name="line_240"></a>[103/554, 2945/3041],
<a name="line_241"></a>[104/1073, 105/233],
<a name="line_242"></a>[1/10, 13/19],
<a name="line_243"></a>[1/10, 16/17],
<a name="line_244"></a>[1/10, 245/267],
<a name="line_245"></a>[11/118, 819/1331],
<a name="line_246"></a>[11/1190, 1664/2057],
<a name="line_247"></a>[1127/2194, 581/731],
<a name="line_248"></a>[1135/2114, 1088/1137],
<a name="line_249"></a>[1147/2390, 97/961],
<a name="line_250"></a>[1147/2830, 640/1369],
<a name="line_251"></a>[1151/3722, 11/139],
<a name="line_252"></a>[1223/2330, 16/17],
<a name="line_253"></a>[12/29, 247/297],
<a name="line_254"></a>[125/262, 16/59],
<a name="line_255"></a>[1255/2146, 1547/1675],
<a name="line_256"></a>[1260/3599, 559/1009],
<a name="line_257"></a>[127/450, 416/417],
<a name="line_258"></a>[1301/2270, 16/33],
<a name="line_259"></a>[13/30, 145/1521],
<a name="line_260"></a>[13/30, 16/33],
<a name="line_261"></a>[1389/3038, 176/1049],
<a name="line_262"></a>[1424/3913, 1571/2179],
<a name="line_263"></a>[148/215, 29/323],
<a name="line_264"></a>[151/338, 279/1129],
<a name="line_265"></a>[1549/3038, 2768/3307],
<a name="line_266"></a>[1603/2350, 19/147],
<a name="line_267"></a>[1611/3878, 355/643],
<a name="line_268"></a>[1648/2875, 2295/3113],
<a name="line_269"></a>[1672/3329, 1257/2057],
<a name="line_270"></a>[169/322, 475/507],
<a name="line_271"></a>[17/106, 1760/2601],
<a name="line_272"></a>[172/511, 15/17],
<a name="line_273"></a>[175/298, 2464/2825],
<a name="line_274"></a>[175/362, 112/163],
<a name="line_275"></a>[1792/3635, 2623/3649],
<a name="line_276"></a>[1809/3050, 32/41],
<a name="line_277"></a>[1829/2750, 97/961],
<a name="line_278"></a>[1864/2843, 145/2993],
<a name="line_279"></a>[191/3490, 1211/1243],
<a name="line_280"></a>[1939/3350, 19/147],
<a name="line_281"></a>[196/335, 2719/3553],
<a name="line_282"></a>[1964/3085, 31/97],
<a name="line_283"></a>[19/70, 16/33],
<a name="line_284"></a>[2093/3534, 115/147],
<a name="line_285"></a>[209/338, 819/1331],
<a name="line_286"></a>[211/350, 80/107],
<a name="line_287"></a>[23/154, 895/993],
<a name="line_288"></a>[23/34, 481/2881],
<a name="line_289"></a>[23/50, 32/41],
<a name="line_290"></a>[2435/3854, 16/59],
<a name="line_291"></a>[248/401, 95/449],
<a name="line_292"></a>[248/665, 29/323],
<a name="line_293"></a>[256/377, 35/387],
<a name="line_294"></a>[25/82, 237/275],
<a name="line_295"></a>[2636/3887, 445/4003],
<a name="line_296"></a>[280/457, 1243/2107],
<a name="line_297"></a>[28/45, 559/1617],
<a name="line_298"></a>[307/830, 267/779],
<a name="line_299"></a>[31/202, 355/1507],
<a name="line_300"></a>[313/466, 560/2651],
<a name="line_301"></a>[3/14, 5/27],
<a name="line_302"></a>[31/50, 355/1507],
<a name="line_303"></a>[316/487, 835/2243],
<a name="line_304"></a>[32/49, 767/2433],
<a name="line_305"></a>[341/1070, 97/961],
<a name="line_306"></a>[359/610, 463/1969],
<a name="line_307"></a>[364/687, 409/441],
<a name="line_308"></a>[36/77, 2575/2673],
<a name="line_309"></a>[367/986, 2176/3401],
<a name="line_310"></a>[37/190, 688/1369],
<a name="line_311"></a>[373/630, 16/33],
<a name="line_312"></a>[37/70, 5/27],
<a name="line_313"></a>[401/650, 16/1067],
<a name="line_314"></a>[403/590, 97/961],
<a name="line_315"></a>[404/685, 31/97],
<a name="line_316"></a>[415/706, 341/459],
<a name="line_317"></a>[427/694, 128/803],
<a name="line_318"></a>[43/166, 224/251],
<a name="line_319"></a>[432/1681, 1055/3553],
<a name="line_320"></a>[43/94, 480/1849],
<a name="line_321"></a>[4/47, 65/193],
<a name="line_322"></a>[455/778, 973/1075],
<a name="line_323"></a>[463/1970, 16/17],
<a name="line_324"></a>[468/775, 385/673],
<a name="line_325"></a>[47/106, 80/97],
<a name="line_326"></a>[4/7, 15/17],
<a name="line_327"></a>[48/235, 107/459],
<a name="line_328"></a>[501/742, 5/27],
<a name="line_329"></a>[52/205, 137/169],
<a name="line_330"></a>[555/974, 163/675],
<a name="line_331"></a>[560/3163, 951/1225],
<a name="line_332"></a>[56/107, 605/931],
<a name="line_333"></a>[569/1042, 2735/2993],
<a name="line_334"></a>[57/130, 32/41],
<a name="line_335"></a>[577/1546, 1680/1873],
<a name="line_336"></a>[600/913, 7/25],
<a name="line_337"></a>[61/718, 581/731],
<a name="line_338"></a>[65/122, 32/507],
<a name="line_339"></a>[65/122, 973/1075],
<a name="line_340"></a>[65/274, 2777/2873],
<a name="line_341"></a>[700/1303, 931/3075],
<a name="line_342"></a>[704/1163, 1435/2299],
<a name="line_343"></a>[7/106, 245/267],
<a name="line_344"></a>[71/130, 13/19],
<a name="line_345"></a>[7/194, 1855/3777],
<a name="line_346"></a>[72/115, 217/729],
<a name="line_347"></a>[73/274, 1253/1947],
<a name="line_348"></a>[735/1594, 1024/2401],
<a name="line_349"></a>[739/1318, 249/601],
<a name="line_350"></a>[7/466, 656/931],
<a name="line_351"></a>[75/134, 341/459],
<a name="line_352"></a>[75/134, 656/675],
<a name="line_353"></a>[76/143, 327/473],
<a name="line_354"></a>[767/1834, 235/779],
<a name="line_355"></a>[777/2050, 224/513],
<a name="line_356"></a>[7/90, 19/147],
<a name="line_357"></a>[80/187, 799/2049],
<a name="line_358"></a>[8/25, 1007/1041],
<a name="line_359"></a>[8/25, 31/97],
<a name="line_360"></a>[827/2590, 1963/3531],
<a name="line_361"></a>[83/126, 5/27],
<a name="line_362"></a>[839/2170, 736/857],
<a name="line_363"></a>[881/1538, 160/209],
<a name="line_364"></a>[907/2046, 1075/1203],
<a name="line_365"></a>[92/381, 1127/2073],
<a name="line_366"></a>[95/202, 1264/1411],
<a name="line_367"></a>[95/202, 301/499],
<a name="line_368"></a>[965/2086, 1072/1075],
<a name="line_369"></a>NULL
<a name="line_370"></a>]:
<a name="line_371"></a>
<a name="line_372"></a><span style="color:red">#@ inner_quasirational_points 
</span><a name="line_373"></a>inner_quasirational_points :=
<a name="line_374"></a> map(quasirational_lift,inner_quasirational_projections);
<a name="line_375"></a>
<a name="line_376"></a><span style="color:red">#@ F16_C0_quasirational_points 
</span><a name="line_377"></a>F16_C0_quasirational_points := select(u -> (is_in_F16_E0_measure(evalf(u)) = 0),map(q -> c_rational[0](1/(1-q/2)),farey(12))):
<a name="line_378"></a>
<a name="line_379"></a><span style="color:red">#@ F16_C1_quasirational_points 
</span><a name="line_380"></a>F16_C1_quasirational_points := select(u -> (is_in_F16_E0_measure(evalf(u)) = 0),map(q -> c_quasirational[1](q-1/2),farey(12))):
<a name="line_381"></a>
<a name="line_382"></a><span style="color:red">#@ F16_C3_quasirational_points 
</span><a name="line_383"></a>F16_C3_quasirational_points := select(u -> (is_in_F16_E0_measure(evalf(u)) = 0),map(c_quasirational[3],three_circle(99))):
<a name="line_384"></a>
<a name="line_385"></a><span style="color:red">#@ F16_C5_quasirational_points 
</span><a name="line_386"></a>F16_C5_quasirational_points := select(u -> (is_in_F16_E0_measure(evalf(u)) = 0),map(c_quasirational[5],farey(20))):
<a name="line_387"></a>
<a name="line_388"></a><span style="color:red">#@ quasirational_points 
</span><a name="line_389"></a>quasirational_points := [
<a name="line_390"></a> op(inner_quasirational_points),
<a name="line_391"></a> op(F16_C0_quasirational_points),
<a name="line_392"></a> op(F16_C1_quasirational_points),
<a name="line_393"></a> op(F16_C3_quasirational_points),
<a name="line_394"></a> op(F16_C5_quasirational_points)
<a name="line_395"></a>];
<a name="line_396"></a>
<a name="line_397"></a><span style="color:red">#@ inner_quasirational_points_latex 
</span><a name="line_398"></a>inner_quasirational_points_latex := proc() 
<a name="line_399"></a> local P,num_cols,S,T,L,i,j,s;
<a name="line_400"></a> P := inner_quasirational_projections;
<a name="line_401"></a> num_cols := 8;
<a name="line_402"></a> S := cat("\\[ \\renewcommand{\\arraystretch}{2} \n \\begin{array}{","c" $ num_cols,"}\n");
<a name="line_403"></a> j := 0;
<a name="line_404"></a> L := "\\\\ \n";
<a name="line_405"></a> for i from 1 to nops(P) do
<a name="line_406"></a>  s := P[i];
<a name="line_407"></a>  T := sprintf("\\left(\\frac{%4d}{%4d},\\frac{%4d}{%4d}\\right)",
<a name="line_408"></a>               numer(s[1]),denom(s[1]),numer(s[2]),denom(s[2]));
<a name="line_409"></a>  S := cat(S,T);
<a name="line_410"></a>  j := j+1;
<a name="line_411"></a>  if j = num_cols then
<a name="line_412"></a>   if i < nops(P) then
<a name="line_413"></a>    S := cat(S,L);
<a name="line_414"></a>   fi;
<a name="line_415"></a>   j := 0;
<a name="line_416"></a>  else
<a name="line_417"></a>   S := cat(S," &\n");
<a name="line_418"></a>  fi;
<a name="line_419"></a> od;
<a name="line_420"></a> S := cat(S,"\n\\end{array} \\]\n");
<a name="line_421"></a> return(S);
<a name="line_422"></a>end:
<a name="line_423"></a>
<a name="line_424"></a><span style="color:red">#@ inner_quasirational_points_tikz 
</span><a name="line_425"></a>inner_quasirational_points_tikz := proc()
<a name="line_426"></a> local P,S,T,s,i;
<a name="line_427"></a> P := inner_quasirational_projections;
<a name="line_428"></a>
<a name="line_429"></a> S := "\\begin{center}\n \\begin{tikzpicture}[scale=8]\n";
<a name="line_430"></a> S := cat(S,"  \\draw[cyan   ] (0.000,0.000) -- (0.000,0.707);\n");
<a name="line_431"></a> S := cat(S,"  \\draw[green  ] (0.000,0.000) -- (1.000,0.000);\n");
<a name="line_432"></a> S := cat(S,"  \\draw[magenta] (0.000,0.707) -- (0.816,0.707);\n");
<a name="line_433"></a> T := "  \\draw[blue,smooth] ";
<a name="line_434"></a> for i from 0 to 11 do
<a name="line_435"></a>  T := cat(T, sprintf("(%1.3f,%1.3f) -- ",op(evalf(y_proj0(c_E0[5](i*Pi/12))))));
<a name="line_436"></a> od; 
<a name="line_437"></a> T := cat(T, "(0.816,0.707);\n");
<a name="line_438"></a> S := cat(S,T);
<a name="line_439"></a> for i from 1 to nops(P) do
<a name="line_440"></a>  s := evalf(P[i] *~ [sqrt(2),1/sqrt(2)]);
<a name="line_441"></a>  S := cat(S,sprintf("  \\fill(%1.3f,%1.3f) circle(0.004);\n",s[1],s[2]));
<a name="line_442"></a> od;
<a name="line_443"></a> S := cat(S," \\end{tikzpicture}\n\\end{center}\n");
<a name="line_444"></a> return(S);
<a name="line_445"></a>end:
<a name="line_446"></a>
<a name="line_447"></a>######################################################################
<a name="line_448"></a># The following table gives a 7 x 5 grid of arithmetically simple
<a name="line_449"></a># points that lie in F16 and are quite evenly spaced.
<a name="line_450"></a>
<a name="line_451"></a><span style="color:red">#@ rational_grid_points 
</span><a name="line_452"></a>rational_grid_points := [[
<a name="line_453"></a> [(1/2)*sqrt(2), (1/2)*sqrt(2), 0, 0],
<a name="line_454"></a> [23/34, 23/34, (7/34)*sqrt(2), 0],
<a name="line_455"></a> [79/130, 79/130, (47/130)*sqrt(2), 0],
<a name="line_456"></a> [1/2, 1/2, (1/2)*sqrt(2), 0],
<a name="line_457"></a> [79/202, 79/202, (119/202)*sqrt(2), 0],
<a name="line_458"></a> [7/34, 7/34, (23/34)*sqrt(2), 0],
<a name="line_459"></a> [0, 0, 1, 0]
<a name="line_460"></a>],[
<a name="line_461"></a> [407/745, 624/745, 0, 0],
<a name="line_462"></a> [92124/152207, 232883/304414, (31/202)*sqrt(2), -11005/304414],
<a name="line_463"></a> [1287/2425, 344/485, (8/25)*sqrt(2), -248/2425],
<a name="line_464"></a> [7267/15458, 8565/15458, (125/262)*sqrt(2), -1000/7729],
<a name="line_465"></a> [92619/240218, 224276/600545, (359/610)*sqrt(2), -166217/1201090],
<a name="line_466"></a> [143/486, 298/1701, (83/126)*sqrt(2), -415/3402],
<a name="line_467"></a> [(1/60)*sqrt(195), 0, (1/4)*sqrt(15), -(1/60)*sqrt(30)]
<a name="line_468"></a>],[
<a name="line_469"></a> [5/13, 12/13, 0, 0], [15/38, 86/95, (1/10)*sqrt(2), -13/190],
<a name="line_470"></a> [214099/533478, 217940/266739, (73/274)*sqrt(2), -91469/533478],
<a name="line_471"></a> [37323/100798, 29390/50399, (95/202)*sqrt(2), -28595/100798],
<a name="line_472"></a> [5389/14982, 27671/74910, (1301/2270)*sqrt(2), -10408/37455],
<a name="line_473"></a> [42732/137557, 20435/137557, (280/457)*sqrt(2), -49720/137557],
<a name="line_474"></a> [(3/70)*sqrt(55), 0, (2/7)*sqrt(10), -(9/70)*sqrt(5)]
<a name="line_475"></a>],[
<a name="line_476"></a> [9/41, 40/41, 0, 0], [4793/21846, 72098/76461, (23/154)*sqrt(2), -20585/152922],
<a name="line_477"></a> [1121/4510, 1864/2255, (25/82)*sqrt(2), -237/902],
<a name="line_478"></a> [22828/111879, 1572959/2461338, (907/2046)*sqrt(2), -975025/2461338],
<a name="line_479"></a> [342221/1435238, 4932765/10046666, (1549/3038)*sqrt(2), -2143816/5023333],
<a name="line_480"></a> [23/119, 4/17, (4/7)*sqrt(2), -60/119],
<a name="line_481"></a> [(1/68)*sqrt(238), 0, (1/12)*sqrt(102), -(7/102)*sqrt(51)]
<a name="line_482"></a>],[
<a name="line_483"></a> [0, 1, 0, 0],
<a name="line_484"></a> [0, 59/62, (11/62)*sqrt(2), -11/62],
<a name="line_485"></a> [0, 11/13, (4/13)*sqrt(2), -4/13],
<a name="line_486"></a> [0, 61/86, (35/86)*sqrt(2), -35/86],
<a name="line_487"></a> [0, 1/2, (1/2)*sqrt(2), -1/2],
<a name="line_488"></a> [0, 11/38, (21/38)*sqrt(2), -21/38],
<a name="line_489"></a> [0, 0, (1/3)*sqrt(6), -(1/3)*sqrt(3)]
<a name="line_490"></a>]];
<a name="line_491"></a>
  </pre>
 </body>
</html>
    