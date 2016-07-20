<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># This file contains information about the general theory of cromulent
<a name="line_2"></a># surfaces.
<a name="line_3"></a>
<a name="line_4"></a>######################################################################
<a name="line_5"></a># A cromulent surface has certain labelled points v[i] (0 <= i <= 13).
<a name="line_6"></a># A curve system consists of maps c[j] from R to the surface satisfying
<a name="line_7"></a># various axioms.  One axiom says that for certain values of i,j and t,
<a name="line_8"></a># we must have c[j](t) = v[i].  In these cases, t appears as v_on_c[i,j]
<a name="line_9"></a># in the table defined below.  For example, the entry v_on_c[4,0] = Pi
<a name="line_10"></a># means that c[0](Pi) = v[4].
<a name="line_11"></a>
<a name="line_12"></a><span style="color:red">#@ v_on_c 
</span><a name="line_13"></a>v_on_c := table():
<a name="line_14"></a>
<a name="line_15"></a>for i from 0 to 13 do
<a name="line_16"></a> for j from 0 to 8 do
<a name="line_17"></a>  v_on_c[i,j] := NULL;
<a name="line_18"></a> od:
<a name="line_19"></a>od:
<a name="line_20"></a>
<a name="line_21"></a>v_on_c[ 0, 1] := 0:
<a name="line_22"></a>v_on_c[ 0, 2] := 0:
<a name="line_23"></a>v_on_c[ 0, 5] := 0:
<a name="line_24"></a>v_on_c[ 0, 6] := 0:
<a name="line_25"></a>v_on_c[ 1, 1] := Pi:
<a name="line_26"></a>v_on_c[ 1, 2] := Pi:
<a name="line_27"></a>v_on_c[ 1, 7] := 0:
<a name="line_28"></a>v_on_c[ 1, 8] := 0:
<a name="line_29"></a>v_on_c[ 2, 0] := 0:
<a name="line_30"></a>v_on_c[ 2, 4] := -Pi/2:
<a name="line_31"></a>v_on_c[ 3, 0] := Pi/2:
<a name="line_32"></a>v_on_c[ 3, 3] := Pi/2:
<a name="line_33"></a>v_on_c[ 4, 0] := Pi:
<a name="line_34"></a>v_on_c[ 4, 4] := Pi/2:
<a name="line_35"></a>v_on_c[ 5, 0] := -Pi/2:
<a name="line_36"></a>v_on_c[ 5, 3] := -Pi/2:
<a name="line_37"></a>v_on_c[ 6, 0] := Pi/4:
<a name="line_38"></a>v_on_c[ 6, 1] := Pi/2:
<a name="line_39"></a>v_on_c[ 7, 0] := 3*Pi/4:
<a name="line_40"></a>v_on_c[ 7, 2] := Pi/2:
<a name="line_41"></a>v_on_c[ 8, 0] := -3*Pi/4:
<a name="line_42"></a>v_on_c[ 8, 1] := -Pi/2:
<a name="line_43"></a>v_on_c[ 9, 0] := -Pi/4:
<a name="line_44"></a>v_on_c[ 9, 2] := -Pi/2:
<a name="line_45"></a>v_on_c[10, 4] := 0:
<a name="line_46"></a>v_on_c[10, 6] := Pi:
<a name="line_47"></a>v_on_c[11, 3] := 0:
<a name="line_48"></a>v_on_c[11, 5] := Pi:
<a name="line_49"></a>v_on_c[12, 4] := Pi:
<a name="line_50"></a>v_on_c[12, 8] := Pi:
<a name="line_51"></a>v_on_c[13, 3] := Pi:
<a name="line_52"></a>v_on_c[13, 7] := Pi:
<a name="line_53"></a>
<a name="line_54"></a># The next block encodes the same information in a slightly different way.
<a name="line_55"></a># It sets things up so that (for example) c_gen[0](Pi) returns v_gen[4].
<a name="line_56"></a># Some arcane features of Maple evaluation rules make it awkward to do
<a name="line_57"></a># this automatically, so we do it by hand instead.
<a name="line_58"></a>
<a name="line_59"></a>unassign('c_gen');
<a name="line_60"></a>
<a name="line_61"></a><span style="color:red">#@ c_gen
</span><a name="line_62"></a><span style="color:red">#@ v_gen
</span><a name="line_63"></a>c_gen[ 0](      0) := v_gen[ 2]:
<a name="line_64"></a>c_gen[ 0](   Pi  ) := v_gen[ 4]:
<a name="line_65"></a>c_gen[ 0](   Pi/2) := v_gen[ 3]:
<a name="line_66"></a>c_gen[ 0](   Pi/4) := v_gen[ 6]:
<a name="line_67"></a>c_gen[ 0](  -Pi/2) := v_gen[ 5]:
<a name="line_68"></a>c_gen[ 0](  -Pi/4) := v_gen[ 9]:
<a name="line_69"></a>c_gen[ 0]( 3*Pi/4) := v_gen[ 7]:
<a name="line_70"></a>c_gen[ 0](-3*Pi/4) := v_gen[ 8]:
<a name="line_71"></a>c_gen[ 1](      0) := v_gen[ 0]:
<a name="line_72"></a>c_gen[ 1](   Pi  ) := v_gen[ 1]:
<a name="line_73"></a>c_gen[ 1](   Pi/2) := v_gen[ 6]:
<a name="line_74"></a>c_gen[ 1](  -Pi/2) := v_gen[ 8]:
<a name="line_75"></a>c_gen[ 2](      0) := v_gen[ 0]:
<a name="line_76"></a>c_gen[ 2](   Pi  ) := v_gen[ 1]:
<a name="line_77"></a>c_gen[ 2](   Pi/2) := v_gen[ 7]:
<a name="line_78"></a>c_gen[ 2](  -Pi/2) := v_gen[ 9]:
<a name="line_79"></a>c_gen[ 3](      0) := v_gen[11]:
<a name="line_80"></a>c_gen[ 3](   Pi  ) := v_gen[13]:
<a name="line_81"></a>c_gen[ 3](   Pi/2) := v_gen[ 3]:
<a name="line_82"></a>c_gen[ 3](  -Pi/2) := v_gen[ 5]:
<a name="line_83"></a>c_gen[ 4](      0) := v_gen[10]:
<a name="line_84"></a>c_gen[ 4](   Pi  ) := v_gen[12]:
<a name="line_85"></a>c_gen[ 4](   Pi/2) := v_gen[ 4]:
<a name="line_86"></a>c_gen[ 4](  -Pi/2) := v_gen[ 2]:
<a name="line_87"></a>c_gen[ 5](      0) := v_gen[ 0]:
<a name="line_88"></a>c_gen[ 5](   Pi  ) := v_gen[11]:
<a name="line_89"></a>c_gen[ 6](      0) := v_gen[ 0]:
<a name="line_90"></a>c_gen[ 6](   Pi  ) := v_gen[10]:
<a name="line_91"></a>c_gen[ 7](      0) := v_gen[ 1]:
<a name="line_92"></a>c_gen[ 7](   Pi  ) := v_gen[13]:
<a name="line_93"></a>c_gen[ 8](      0) := v_gen[ 1]:
<a name="line_94"></a>c_gen[ 8](   Pi  ) := v_gen[12]:
<a name="line_95"></a>
<a name="line_96"></a># The next block encodes the same information in yet another way.
<a name="line_97"></a># For example, v_track[4] will be [0=0,4=-Pi/2], indicating that
<a name="line_98"></a># v[4] = c[0](0) = c[4](-Pi/2).  Similarly, c_track[7] will be
<a name="line_99"></a># [1=0,13=Pi], indicating that c[7](0) = v[1] and c[7](Pi) = v[13].
<a name="line_100"></a>
<a name="line_101"></a><span style="color:red">#@ v_track
</span><a name="line_102"></a>for i from 0 to 13 do 
<a name="line_103"></a> v_track[i] := []:
<a name="line_104"></a>od:
<a name="line_105"></a>
<a name="line_106"></a><span style="color:red">#@ c_track
</span><a name="line_107"></a>for j from 0 to 8 do
<a name="line_108"></a> c_track[j] := []:
<a name="line_109"></a>od:
<a name="line_110"></a>
<a name="line_111"></a>for i from 0 to 13 do
<a name="line_112"></a> for j from 0 to 8 do
<a name="line_113"></a>  if v_on_c[i,j] <> NULL then
<a name="line_114"></a>   v_track[i] := [op(v_track[i]),j = v_on_c[i,j]];
<a name="line_115"></a>   c_track[j] := [op(c_track[j]),i = v_on_c[i,j]];
<a name="line_116"></a>  fi;
<a name="line_117"></a> od:
<a name="line_118"></a>od:
<a name="line_119"></a>
<a name="line_120"></a>######################################################################
<a name="line_121"></a># We now divide the curves c[i] into segments with endpoints at
<a name="line_122"></a># the vertices v[j], oriented in the direction of increasing values
<a name="line_123"></a># of the parameter t.  marked_edges will be a list of triples
<a name="line_124"></a># [j1,j2,i], one for each segment of c[i] running from v[j1] to v[j2].
<a name="line_125"></a># edges will be a list of pairs [j1,j2], now written with j1 < j2,
<a name="line_126"></a># irrespective of the direction of t.  Note that c[10] runs from 
<a name="line_127"></a># v[4] to v[6] (with 0 <= t <= Pi) and then back to v[4] (with
<a name="line_128"></a># Pi <= t <= 2 Pi).  It contributes entries [4,6,10] and [6,4,10]
<a name="line_129"></a># to marked_edges, but only a single entry [4,6] to edges.  Similar
<a name="line_130"></a># remarks apply to c[11], c[12] and c[13].
<a name="line_131"></a>
<a name="line_132"></a><span style="color:red">#@ edges
</span><a name="line_133"></a><span style="color:red">#@ marked_edges
</span><a name="line_134"></a>proc()
<a name="line_135"></a> local i,j,t,V;
<a name="line_136"></a> global edges,marked_edges;
<a name="line_137"></a>
<a name="line_138"></a> marked_edges := NULL:
<a name="line_139"></a> for i from 0 to 8 do
<a name="line_140"></a>  V := NULL;
<a name="line_141"></a>  for j from 0 to 13 do
<a name="line_142"></a>   if v_on_c[j,i] <> NULL then
<a name="line_143"></a>    t := v_on_c[j,i]/(2*Pi);
<a name="line_144"></a>    if (t < 0) then t := t + 1; fi;
<a name="line_145"></a>    V := V,[j,t];
<a name="line_146"></a>   fi;
<a name="line_147"></a>  od;
<a name="line_148"></a>  V := sort([V],(a,b) -> a[2] < b[2]);
<a name="line_149"></a>  V := [op(V),V[1]];
<a name="line_150"></a>  marked_edges := marked_edges,seq([V[j][1],V[j+1][1],i],j=1..nops(V)-1);
<a name="line_151"></a> od:
<a name="line_152"></a>
<a name="line_153"></a> marked_edges := [marked_edges];
<a name="line_154"></a> edges := sort([op({op(map(u -> sort([u[1],u[2]]),marked_edges))})]);
<a name="line_155"></a>end():
<a name="line_156"></a>
<a name="line_157"></a>######################################################################
<a name="line_158"></a># Another one of the axioms for a curve system describes g.c[i](t) 
<a name="line_159"></a># for all g in G and i from 0 to 8.  If act_c_data[g,i] = [j,m,c] 
<a name="line_160"></a># in the table below, then g.c[i](t) = c[j](m*t + c).
<a name="line_161"></a>
<a name="line_162"></a><span style="color:red">#@ act_c_data
</span><a name="line_163"></a>act_c_data[L,0] := [0, 1,Pi/2]:
<a name="line_164"></a>act_c_data[L,1] := [2, 1,0]:
<a name="line_165"></a>act_c_data[L,2] := [1,-1,0]:
<a name="line_166"></a>act_c_data[L,3] := [4, 1,0]:
<a name="line_167"></a>act_c_data[L,4] := [3,-1,0]:
<a name="line_168"></a>act_c_data[L,5] := [6, 1,0]:
<a name="line_169"></a>act_c_data[L,6] := [5,-1,0]:
<a name="line_170"></a>act_c_data[L,7] := [8, 1,0]:
<a name="line_171"></a>act_c_data[L,8] := [7,-1,0]:
<a name="line_172"></a>
<a name="line_173"></a>act_c_data[M,0] := [0,-1,0]:
<a name="line_174"></a>act_c_data[M,1] := [2, 1,Pi]:
<a name="line_175"></a>act_c_data[M,2] := [1, 1,Pi]:
<a name="line_176"></a>act_c_data[M,3] := [3, 1,Pi]:
<a name="line_177"></a>act_c_data[M,4] := [4,-1,-Pi]:
<a name="line_178"></a>act_c_data[M,5] := [7, 1,0]:
<a name="line_179"></a>act_c_data[M,6] := [8,-1,0]:
<a name="line_180"></a>act_c_data[M,7] := [5, 1,0]:
<a name="line_181"></a>act_c_data[M,8] := [6,-1,0]:
<a name="line_182"></a>
<a name="line_183"></a>act_c_data[N,0] := [0,-1,0]:
<a name="line_184"></a>act_c_data[N,1] := [2,-1,0]:
<a name="line_185"></a>act_c_data[N,2] := [1,-1,0]:
<a name="line_186"></a>act_c_data[N,3] := [3,-1,0]:
<a name="line_187"></a>act_c_data[N,4] := [4, 1,0]:
<a name="line_188"></a>act_c_data[N,5] := [5, 1,0]:
<a name="line_189"></a>act_c_data[N,6] := [6,-1,0]:
<a name="line_190"></a>act_c_data[N,7] := [7, 1,0]:
<a name="line_191"></a>act_c_data[N,8] := [8,-1,0]:
<a name="line_192"></a>
<a name="line_193"></a># c_involution[i] is an antiholomorphic involution in G that
<a name="line_194"></a># acts as the identity on c[i](t) for all t.
<a name="line_195"></a>
<a name="line_196"></a><span style="color:red">#@ c_involution
</span><a name="line_197"></a>c_involution[0] := MN:
<a name="line_198"></a>c_involution[1] := LN:
<a name="line_199"></a>c_involution[2] := LLLN:
<a name="line_200"></a>c_involution[3] := LLN:
<a name="line_201"></a>c_involution[4] := N:
<a name="line_202"></a>c_involution[5] := N:
<a name="line_203"></a>c_involution[6] := LLN:
<a name="line_204"></a>c_involution[7] := N:
<a name="line_205"></a>c_involution[8] := LLN:
<a name="line_206"></a>
<a name="line_207"></a># For each i, the set C[i] is defined to be the connected
<a name="line_208"></a># component of the fixed set of c_involution[i] that contains
<a name="line_209"></a># v[j] for a certain value of j.  These values are stored as
<a name="line_210"></a># c_basepoint[i].
<a name="line_211"></a>
<a name="line_212"></a><span style="color:red">#@ c_basepoint
</span><a name="line_213"></a>c_basepoint[0] := 2;
<a name="line_214"></a>c_basepoint[1] := 0;
<a name="line_215"></a>c_basepoint[2] := 0;
<a name="line_216"></a>c_basepoint[3] := 11;
<a name="line_217"></a>c_basepoint[4] := 10;
<a name="line_218"></a>c_basepoint[5] := 0;
<a name="line_219"></a>c_basepoint[6] := 0;
<a name="line_220"></a>c_basepoint[7] := 1;
<a name="line_221"></a>c_basepoint[8] := 1;
<a name="line_222"></a>
<a name="line_223"></a>######################################################################
<a name="line_224"></a>
<a name="line_225"></a># All our plots display the curves c[i] (or their images under various
<a name="line_226"></a># maps) using the colour scheme defined below.  Note that curves have
<a name="line_227"></a># the same colour iff they are in the same G-orbit.
<a name="line_228"></a>
<a name="line_229"></a><span style="color:red">#@ c_colour
</span><a name="line_230"></a>c_colour[0] := cyan:
<a name="line_231"></a>c_colour[1] := green:
<a name="line_232"></a>c_colour[2] := green:
<a name="line_233"></a>c_colour[3] := magenta:
<a name="line_234"></a>c_colour[4] := magenta:
<a name="line_235"></a>for i from  5 to  8 do c_colour[i] := blue: od:
<a name="line_236"></a>
<a name="line_237"></a>c_colour[false] := black:
<a name="line_238"></a>
<a name="line_239"></a># If v[i] and v[j] both lie on the curve c[k], then edge_curve(i,j)
<a name="line_240"></a># will return k.  
<a name="line_241"></a>#
<a name="line_242"></a># In some cases, we express a cromulent surface X as a quotient of
<a name="line_243"></a># some other space X0.  Preimages in X0 of the point v[3] in X (for
<a name="line_244"></a># example) may be labelled as v[3.1], v[3.2] and so on.  We use
<a name="line_245"></a># the floor() function in edge_curve() to handle this. 
<a name="line_246"></a>
<a name="line_247"></a><span style="color:red">#@ edge_curve 
</span><a name="line_248"></a>edge_curve := proc(ij)
<a name="line_249"></a> option remember;
<a name="line_250"></a> local i,j,k;
<a name="line_251"></a>
<a name="line_252"></a> i,j := op(map(floor,ij));
<a name="line_253"></a> for k from 0 to 8 do
<a name="line_254"></a>  if v_on_c[i,k] <> NULL and v_on_c[j,k] <> NULL then
<a name="line_255"></a>   return(k);
<a name="line_256"></a>  fi;
<a name="line_257"></a> od;
<a name="line_258"></a>
<a name="line_259"></a> return(false);
<a name="line_260"></a>end:
<a name="line_261"></a>
<a name="line_262"></a># edge_colour(i,j) gives the appropriate colour for the edge from
<a name="line_263"></a># v[i] to v[j].
<a name="line_264"></a>
<a name="line_265"></a><span style="color:red">#@ edge_colour 
</span><a name="line_266"></a>edge_colour := (ij) -> c_colour[edge_curve(ij)];
<a name="line_267"></a>
<a name="line_268"></a>######################################################################
<a name="line_269"></a>
<a name="line_270"></a># Every cromulent surface has a fundamental domain F4 for the action 
<a name="line_271"></a># of {1,LL,N,LLN}, and a fundamental domain F16 for the action of the
<a name="line_272"></a># whole group G.  
<a name="line_273"></a>
<a name="line_274"></a># Vertices lying on the boundary of F4.
<a name="line_275"></a>
<a name="line_276"></a><span style="color:red">#@ F4_vertices 
</span><a name="line_277"></a>F4_vertices := [0,1,2,3,6,10,11,12,13]:
<a name="line_278"></a>
<a name="line_279"></a># If F4_curve_limits[k] = a .. b, then
<a name="line_280"></a># { t in [0,2 pi] : c_k(t) in F_4 } = [a,b] (or [a,b] u {2 pi}).
<a name="line_281"></a># If F4_curve_limits[k] = NULL, then
<a name="line_282"></a># { t in [0,2 pi] : c_k(t) in F_4 } is finite (and usually empty).
<a name="line_283"></a>
<a name="line_284"></a><span style="color:red">#@ F4_curve_limits 
</span><a name="line_285"></a>F4_curve_limits := table():
<a name="line_286"></a>F4_curve_limits[ 0] := 0 .. Pi/2:
<a name="line_287"></a>F4_curve_limits[ 1] := 0 .. Pi:
<a name="line_288"></a>F4_curve_limits[ 2] := NULL:
<a name="line_289"></a>F4_curve_limits[ 3] := 0 .. Pi:
<a name="line_290"></a>F4_curve_limits[ 4] := Pi .. 2*Pi:
<a name="line_291"></a>F4_curve_limits[ 5] := 0 .. Pi:
<a name="line_292"></a>F4_curve_limits[ 6] := 0 .. Pi:
<a name="line_293"></a>F4_curve_limits[ 7] := 0 .. Pi:
<a name="line_294"></a>F4_curve_limits[ 8] := 0 .. Pi:
<a name="line_295"></a>
<a name="line_296"></a># Vertices lying on the boundary of F16.
<a name="line_297"></a>
<a name="line_298"></a><span style="color:red">#@ F16_vertices 
</span><a name="line_299"></a>F16_vertices := [0,3,6,11]:
<a name="line_300"></a>
<a name="line_301"></a># Parameter values for edges of F16.
<a name="line_302"></a>
<a name="line_303"></a><span style="color:red">#@ F16_curve_limits
</span><a name="line_304"></a>F16_curve_limits[ 0] := Pi/4 .. Pi/2:
<a name="line_305"></a>F16_curve_limits[ 1] := 0 .. Pi/2:
<a name="line_306"></a>F16_curve_limits[ 2] := NULL:
<a name="line_307"></a>F16_curve_limits[ 3] := 0 .. Pi/2:
<a name="line_308"></a>F16_curve_limits[ 4] := NULL:
<a name="line_309"></a>F16_curve_limits[ 5] := 0 .. Pi:
<a name="line_310"></a>F16_curve_limits[ 6] := NULL:
<a name="line_311"></a>F16_curve_limits[ 7] := NULL:
<a name="line_312"></a>F16_curve_limits[ 8] := NULL:
<a name="line_313"></a>
<a name="line_314"></a>######################################################################
<a name="line_315"></a>
<a name="line_316"></a># The homology group H_1 of a cromulent surface is isomorphic to Z^4.
<a name="line_317"></a># More precisely, there is a unique isomorphism H_1 -> Z^4 which sends
<a name="line_318"></a># the homology class of the curve c[i] to the vector c_homology[i] 
<a name="line_319"></a># as tabulated below.
<a name="line_320"></a>
<a name="line_321"></a><span style="color:red">#@ c_homology
</span><a name="line_322"></a>c_homology[ 0] := [ 0, 0, 0, 0];
<a name="line_323"></a>c_homology[ 1] := [ 1, 1,-1,-1];
<a name="line_324"></a>c_homology[ 2] := [-1, 1, 1,-1];
<a name="line_325"></a>c_homology[ 3] := [ 0, 1, 0,-1];
<a name="line_326"></a>c_homology[ 4] := [-1, 0, 1, 0];
<a name="line_327"></a>c_homology[ 5] := [ 1, 0, 0, 0];
<a name="line_328"></a>c_homology[ 6] := [ 0, 1, 0, 0];
<a name="line_329"></a>c_homology[ 7] := [ 0, 0, 1, 0];
<a name="line_330"></a>c_homology[ 8] := [ 0, 0, 0, 1];
<a name="line_331"></a>
<a name="line_332"></a># This table gives the action of G on Z^4 corresponding (via the 
<a name="line_333"></a># above isomorphism) to the action on H_1.
<a name="line_334"></a>
<a name="line_335"></a><span style="color:red">#@ act_Z4
</span><a name="line_336"></a>act_Z4[1]    := (n) -> [ n[1], n[2], n[3], n[4]];
<a name="line_337"></a>act_Z4[L]    := (n) -> [-n[2], n[1],-n[4], n[3]];
<a name="line_338"></a>act_Z4[LL]   := (n) -> [-n[1],-n[2],-n[3],-n[4]];
<a name="line_339"></a>act_Z4[LLL]  := (n) -> [ n[2],-n[1], n[4],-n[3]];
<a name="line_340"></a>act_Z4[M]    := (n) -> [ n[3],-n[4], n[1],-n[2]];
<a name="line_341"></a>act_Z4[LM]   := (n) -> [ n[4], n[3], n[2], n[1]];
<a name="line_342"></a>act_Z4[LLM]  := (n) -> [-n[3], n[4],-n[1], n[2]];
<a name="line_343"></a>act_Z4[LLLM] := (n) -> [-n[4],-n[3],-n[2],-n[1]];
<a name="line_344"></a>act_Z4[N]    := (n) -> [ n[1],-n[2], n[3],-n[4]];
<a name="line_345"></a>act_Z4[LN]   := (n) -> [ n[2], n[1], n[4], n[3]];
<a name="line_346"></a>act_Z4[LLN]  := (n) -> [-n[1], n[2],-n[3], n[4]];
<a name="line_347"></a>act_Z4[LLLN] := (n) -> [-n[2],-n[1],-n[4],-n[3]];
<a name="line_348"></a>act_Z4[MN]   := (n) -> [ n[3], n[4], n[1], n[2]];
<a name="line_349"></a>act_Z4[LMN]  := (n) -> [-n[4], n[3],-n[2], n[1]];
<a name="line_350"></a>act_Z4[LLMN] := (n) -> [-n[3],-n[4],-n[1],-n[2]];
<a name="line_351"></a>act_Z4[LLLMN]:= (n) -> [ n[4],-n[3], n[2],-n[1]];
<a name="line_352"></a>
<a name="line_353"></a># This gives an alternative basis after tensoring with the rationals.
<a name="line_354"></a># It is orthogonal, which is convenient for analysing the character
<a name="line_355"></a># of the action.
<a name="line_356"></a>
<a name="line_357"></a><span style="color:red">#@ homology_u
</span><a name="line_358"></a>homology_u[1,1] := [ 1, 0, 1, 0]:
<a name="line_359"></a>homology_u[1,2] := [ 0, 1, 0, 1]:
<a name="line_360"></a>homology_u[2,1] := [ 1, 0,-1, 0]:
<a name="line_361"></a>homology_u[2,2] := [ 0, 1, 0,-1]:
<a name="line_362"></a>
<a name="line_363"></a># The span of homology_u[1,1] and homology_u[1,2] is a representation
<a name="line_364"></a># of G with character homology_character[1].
<a name="line_365"></a>
<a name="line_366"></a># The span of homology_u[2,1] and homology_u[2,2] is a representation
<a name="line_367"></a># of G with character homology_character[2].
<a name="line_368"></a>
<a name="line_369"></a><span style="color:red">#@ homology_character
</span><a name="line_370"></a>for T in G16 do
<a name="line_371"></a> for i from 1 to 2 do 
<a name="line_372"></a>  homology_character[i][T] :=
<a name="line_373"></a>   add(dp4(homology_u[i,j],act_Z4[T](homology_u[i,j]))/2,j in [1,2]);
<a name="line_374"></a> od;
<a name="line_375"></a>od;
<a name="line_376"></a>
<a name="line_377"></a><span style="color:red">#@ cap_product 
</span><a name="line_378"></a>cap_product := (u,v) -> u[1]*v[2]-u[2]*v[1]-u[3]*v[4]+u[4]*v[3];
<a name="line_379"></a>
<a name="line_380"></a># This is the coequaliser for the action of M on Z^4
<a name="line_381"></a><span style="color:red">#@ homology_theta_p
</span><a name="line_382"></a>homology_theta_p := (n) -> [n[1]+n[3],n[2]-n[4]];
<a name="line_383"></a>
<a name="line_384"></a># This is the coequaliser for the action of LM on Z^4
<a name="line_385"></a><span style="color:red">#@ homology_theta_m
</span><a name="line_386"></a>homology_theta_m := (n) -> [n[2]+n[3],n[1]+n[4]];
<a name="line_387"></a>
<a name="line_388"></a>######################################################################
<a name="line_389"></a>
<a name="line_390"></a># The function check_precromulent() checks that a cromulent surface
<a name="line_391"></a># with a given curve system satisfies the appropriate axioms.  
<a name="line_392"></a>#
<a name="line_393"></a># The input data is represented as follows.  The cromulent surface is 
<a name="line_394"></a># specified by a single letter E, H or P (for the embedded, hyperbolic
<a name="line_395"></a># and projective families).  The hyperbolic family (with a symbolic 
<a name="line_396"></a># rather than numeric value for the parameter a) is defined in the
<a name="line_397"></a># file hyperbolic/HX.mpl.  That file defines
<a name="line_398"></a>#
<a name="line_399"></a>#  + A function is_member_H(z) which checks whether z is an element
<a name="line_400"></a>#    of the open unit disk.
<a name="line_401"></a>#  + A function is_equal_H(z,w), which checks whether z and w are 
<a name="line_402"></a>#    equal in the unit disc.  It can also be called in the form 
<a name="line_403"></a>#    is_equal_H(z,w,p), where p is assumed to be an element of the
<a name="line_404"></a>#    Fuchsian group Pi; this form checks whether p.z = w.
<a name="line_405"></a>#  + A table of functions act_H[g] acting on the unit disc, for g in G.
<a name="line_406"></a>#    These do not strictly give an action of G on the disc, but they 
<a name="line_407"></a>#    descend to give an action on Disc/Pi.
<a name="line_408"></a>#  + Various other things such as v_H, c_H, c_check_H,
<a name="line_409"></a>#    is_in_F4_H, is_in_F16_H and so on.
<a name="line_410"></a>
<a name="line_411"></a><span style="color:red">#@ check_precromulent 
</span><a name="line_412"></a>check_precromulent := proc(X::string,_stop_on_error)
<a name="line_413"></a> local is_member,is_equal,act,v,c,c_check,is_in_F4,is_in_F16,
<a name="line_414"></a>       v_action_witness,c_action_witness,v_on_c_witness,
<a name="line_415"></a>       stop_on_error,i,j,m,u,T,t,a,b;
<a name="line_416"></a>
<a name="line_417"></a> is_member := eval(convert(cat("is_member_",X),name));
<a name="line_418"></a> is_equal  := eval(convert(cat("is_equal_" ,X),name));
<a name="line_419"></a> act       := eval(convert(cat("act_"      ,X),name));
<a name="line_420"></a> v         := eval(convert(cat("v_"        ,X),name));
<a name="line_421"></a> c         := eval(convert(cat("c_"        ,X),name));
<a name="line_422"></a> c_check   := eval(convert(cat("c_check_"  ,X),name));
<a name="line_423"></a> is_in_F4  := eval(convert(cat("is_in_F4_" ,X),name));
<a name="line_424"></a> is_in_F16 := eval(convert(cat("is_in_F16_",X),name));
<a name="line_425"></a>
<a name="line_426"></a> v_action_witness := eval(convert(cat("v_action_witness_",X),name));
<a name="line_427"></a> c_action_witness := eval(convert(cat("c_action_witness_",X),name));
<a name="line_428"></a> v_on_c_witness   := eval(convert(cat("v_on_c_witness_"  ,X),name));
<a name="line_429"></a>
<a name="line_430"></a> stop_on_error := `if`(nargs > 1,_stop_on_error,true);
<a name="line_431"></a> assume(t::real);
<a name="line_432"></a>
<a name="line_433"></a> for i from 0 to 13 do
<a name="line_434"></a>  if not(is_member(v[i])) then
<a name="line_435"></a>   printf("v[%d] is not an element of X\n",i);
<a name="line_436"></a>   if stop_on_error then return false; fi;
<a name="line_437"></a>  fi;
<a name="line_438"></a> od;
<a name="line_439"></a>
<a name="line_440"></a> for i from 0 to 13 do
<a name="line_441"></a>  for T in [L,M,N] do
<a name="line_442"></a>   if not(is_equal(v[act_V[T](i)],act[T](v[i]),v_action_witness[T,i])) then
<a name="line_443"></a>    printf("incorrect action of %A on v[%d]\n",T,i);
<a name="line_444"></a>    if stop_on_error then return false; fi;
<a name="line_445"></a>   fi;
<a name="line_446"></a>  od;
<a name="line_447"></a> od;
<a name="line_448"></a>
<a name="line_449"></a> for i from 0 to 8 do
<a name="line_450"></a>  if not(is_member(c[i](t))) then
<a name="line_451"></a>   printf("c[%d](t) is not an element of X\n",i);
<a name="line_452"></a>   if stop_on_error then return false; fi;
<a name="line_453"></a>  fi;
<a name="line_454"></a> od;
<a name="line_455"></a>
<a name="line_456"></a> for i from 0 to 8 do
<a name="line_457"></a>  for T in [L,M,N] do
<a name="line_458"></a>   j,m,u := op(act_c_data[T,i]);
<a name="line_459"></a>   if not(is_equal(c[j](m*t+u),act[T](c[i](t)),c_action_witness[T,i])) then
<a name="line_460"></a>    printf("incorrect action of %A on c[%d](t)\n",T,i);
<a name="line_461"></a>    if stop_on_error then return false; fi;
<a name="line_462"></a>   fi;
<a name="line_463"></a>  od;
<a name="line_464"></a> od;
<a name="line_465"></a>
<a name="line_466"></a> for i from 0 to 13 do
<a name="line_467"></a>  for j from 0 to 8 do
<a name="line_468"></a>   if v_on_c[i,j] = NULL then
<a name="line_469"></a>    if c_check[j](v[i]) then
<a name="line_470"></a>     printf("v[%d] should not lie on C[%d]\n",i,j);
<a name="line_471"></a>     if stop_on_error then return false; fi;
<a name="line_472"></a>    fi;
<a name="line_473"></a>   else
<a name="line_474"></a>    if not(is_equal(v[i],c[j](v_on_c[i,j]),v_on_c_witness[i,j])) then
<a name="line_475"></a>     printf("c[%d](%A) should be equal to v[%d]\n",j,v_on_c[i,j],i);
<a name="line_476"></a>     if stop_on_error then return false; fi;
<a name="line_477"></a>    fi;
<a name="line_478"></a>   fi;
<a name="line_479"></a>  od;
<a name="line_480"></a> od;
<a name="line_481"></a>
<a name="line_482"></a> for i from 0 to 13 do
<a name="line_483"></a>  a := member(i,F4_vertices);
<a name="line_484"></a>  b := is_in_F4(v[i]);
<a name="line_485"></a>  if a and not b then 
<a name="line_486"></a>   printf("v[%d] should be in F4\n",i);
<a name="line_487"></a>   if stop_on_error then return false; fi;
<a name="line_488"></a>  fi;
<a name="line_489"></a>  if b and not a then 
<a name="line_490"></a>   printf("v[%d] should not be in F4\n",i);
<a name="line_491"></a>   if stop_on_error then return false; fi;
<a name="line_492"></a>  fi;
<a name="line_493"></a>  a := member(i,F16_vertices);
<a name="line_494"></a>  b := is_in_F16(v[i]);
<a name="line_495"></a>  if a and not b then 
<a name="line_496"></a>   printf("v[%d] should be in F16\n",i);
<a name="line_497"></a>   if stop_on_error then return false; fi;
<a name="line_498"></a>  fi;
<a name="line_499"></a>  if b and not a then 
<a name="line_500"></a>   printf("v[%d] should not be in F16\n",i);
<a name="line_501"></a>   if stop_on_error then return false; fi;
<a name="line_502"></a>  fi;
<a name="line_503"></a> od;
<a name="line_504"></a>
<a name="line_505"></a> if stop_on_error then
<a name="line_506"></a>  return true;
<a name="line_507"></a> else
<a name="line_508"></a>  return NULL;
<a name="line_509"></a> fi;
<a name="line_510"></a>end:
<a name="line_511"></a>
<a name="line_512"></a>
  </pre>
 </body>
</html>
    