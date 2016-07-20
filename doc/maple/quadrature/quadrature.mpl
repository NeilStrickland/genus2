<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>######################################################################
<a name="line_2"></a>
<a name="line_3"></a># These are some auxiliary functions that appear in Dunavant's
<a name="line_4"></a># analysis of symmetric triangle quadrature rules.  If we had 
<a name="line_5"></a># implemented static methods, these would be static methods of the
<a name="line_6"></a># triangle quadrature rule class.
<a name="line_7"></a>
<a name="line_8"></a><span style="color:red">#@ dunavant_v
</span><a name="line_9"></a>dunavant_v := proc(j,k)
<a name="line_10"></a> option remember;
<a name="line_11"></a> int(cos(k*alpha)/cos(alpha)^(j+2),alpha=-Pi/3..Pi/3)/(sqrt(3)*(j+2)*2^j);
<a name="line_12"></a>end:
<a name="line_13"></a>
<a name="line_14"></a><span style="color:red">#@ dunavant_jk_list
</span><a name="line_15"></a>dunavant_jk_list := proc(deg)
<a name="line_16"></a> local L;
<a name="line_17"></a> L := [seq(seq([2*m-3*k,k],m=3*k..(3*k+deg)/2),k=0..6)]:
<a name="line_18"></a>end:
<a name="line_19"></a>
<a name="line_20"></a><span style="color:red">#@ dunavant_m
</span><a name="line_21"></a>dunavant_m := proc(deg)
<a name="line_22"></a> local alpha;
<a name="line_23"></a> 
<a name="line_24"></a> alpha := [3,-4,-1,0,-1,-4][modp(deg,6)+1];
<a name="line_25"></a> return ((deg+3)^3 + alpha)/12;
<a name="line_26"></a>end:
<a name="line_27"></a>
<a name="line_28"></a>######################################################################
<a name="line_29"></a>
<a name="line_30"></a><a name="CLASS_triangle_quadrature_rule"></a><span style="color:red">#@ CLASS: triangle_quadrature_rule
</span><a name="line_31"></a>
<a name="line_32"></a>`Class/Declare`("triangle_quadrature_rule",
<a name="line_33"></a> "An instance of this class represents a rule for approximate integration of functions on the two-simplex.  Such a rule consists of a list of sample points and a list of weights; the approximate integral is obtained by evaluating at the sample points, multiplying by the corresponding weights, and taking the sum.  Following work of Dunavant, we consider only rules that are invariant under the evident action of the symmetric group $S_3$ on the simplex.  We choose one sample point from each $S_3$-orbit, and call these the base points for the rule.",
<a name="line_34"></a>
<a name="line_35"></a> ["Field","description"::string = ""],
<a name="line_36"></a>
<a name="line_37"></a> ["Field","degree"::integer,
<a name="line_38"></a>  "The degree should be set to $d$ if the quadrature rule integrates all polynomials of degree at most $d$ exactly (up to the working precision)."
<a name="line_39"></a> ],
<a name="line_40"></a>
<a name="line_41"></a> ["Field","num_base_points"::integer,
<a name="line_42"></a>  "The number of base points, or equivalently the number of $S_3$-orbits of sample points."
<a name="line_43"></a> ],
<a name="line_44"></a>
<a name="line_45"></a> ["Field","base_points"::list(RR_3),
<a name="line_46"></a>  "The list of base points.  These should have the form @[p1,p2,p3]@ where either @p1 < p2 < p3@ or @p1 <> p2 = p3@ or @p1 = p2 = p3@."
<a name="line_47"></a> ],
<a name="line_48"></a>
<a name="line_49"></a> ["Field","base_weights"::list(RR),"The list of weights of base points"],
<a name="line_50"></a> ["Field","base_multiplicities"::list(posint),
<a name="line_51"></a>  "The list of multiplicities of base points; the multiplicity is the size of the $S_3$-orbit"
<a name="line_52"></a> ],
<a name="line_53"></a>
<a name="line_54"></a> ["Field","base_r"::list(RR),
<a name="line_55"></a>  "The list of $r$-values for base points.  Here the $r$-value of a point $u$ is the distance from the origin to the point @triangle_proj(u)@ in $\\mathbb{R}^2$."
<a name="line_56"></a> ],
<a name="line_57"></a>
<a name="line_58"></a> ["Field","base_theta"::list(RR),
<a name="line_59"></a>  "The list of $\\theta$-values for base points.  Here the $\\theta$-value of a point $u$ is the standard polar coordinate for the point @triangle_proj(u)@ in $\\mathbb{R}^2$."
<a name="line_60"></a> ],
<a name="line_61"></a>
<a name="line_62"></a> ["Field","num_points"::integer,"The number of sample points."],
<a name="line_63"></a> ["Field","points"::list(RR_3),"The list of all sample points"],
<a name="line_64"></a> ["Field","weights"::list(RR),"The list of weights for all sample points"],
<a name="line_65"></a>
<a name="line_66"></a> ["Constructor","",
<a name="line_67"></a>  proc(this,deg::integer,pts::list(RR_3),wgts::list(RR))
<a name="line_68"></a>   this["set",deg,pts,wgts];
<a name="line_69"></a>  end
<a name="line_70"></a> ],
<a name="line_71"></a>
<a name="line_72"></a> ["Method","set"::void,"",
<a name="line_73"></a>  proc(this,deg::integer,pts::list([numeric,numeric,numeric]),wgts::list(numeric)) 
<a name="line_74"></a>   local i,m,p,q,w;
<a name="line_75"></a>
<a name="line_76"></a>   this["degree"] := deg;
<a name="line_77"></a>   this["num_base_points"] := nops(pts);
<a name="line_78"></a>   this["base_points"] := pts;
<a name="line_79"></a>   this["base_weights"] := wgts;
<a name="line_80"></a>   this["base_multiplicities"] := [];
<a name="line_81"></a>   this["base_r"] := [];
<a name="line_82"></a>   this["base_theta"] := [];
<a name="line_83"></a>   this["points"] := [];
<a name="line_84"></a>   this["weights"] := [];
<a name="line_85"></a>
<a name="line_86"></a>   for i from 1 to nops(pts) do
<a name="line_87"></a>    p := pts[i];
<a name="line_88"></a>    w := wgts[i];
<a name="line_89"></a>    q := triangle_proj(p);
<a name="line_90"></a>
<a name="line_91"></a>    if p[2] = p[3] then
<a name="line_92"></a>     if p[1] = p[2] then
<a name="line_93"></a>      m := 1;
<a name="line_94"></a>      this["base_multiplicities"] := [op(this["base_multiplicities"]),m];
<a name="line_95"></a>      this["points"] := [op(this["points"]),p];
<a name="line_96"></a>      this["weights"] := [op(this["weights"]),w];
<a name="line_97"></a>      this["base_r"] := [op(this["base_r"]),0];
<a name="line_98"></a>      this["base_theta"] := [op(this["base_theta"]),0];
<a name="line_99"></a>     else
<a name="line_100"></a>      m := 3;
<a name="line_101"></a>      this["base_multiplicities"] := [op(this["base_multiplicities"]),m];
<a name="line_102"></a>      this["points"] := [op(this["points"]),
<a name="line_103"></a>		     [p[1],p[2],p[2]],
<a name="line_104"></a>		     [p[2],p[1],p[2]],
<a name="line_105"></a>		     [p[2],p[2],p[1]]];
<a name="line_106"></a>      this["weights"] := [op(this["weights"]),w$3];
<a name="line_107"></a>      this["base_r"] := [op(this["base_r"]),q[1]];
<a name="line_108"></a>      this["base_theta"] := [op(this["base_theta"]),0];
<a name="line_109"></a>     fi;
<a name="line_110"></a>    else
<a name="line_111"></a>     m := 6;
<a name="line_112"></a>     this["base_multiplicities"] := [op(this["base_multiplicities"]),m];
<a name="line_113"></a>     this["points"] := [op(this["points"]),
<a name="line_114"></a>		     [p[1],p[2],p[3]],
<a name="line_115"></a>		     [p[1],p[3],p[2]],
<a name="line_116"></a>		     [p[2],p[1],p[3]],
<a name="line_117"></a>		     [p[2],p[3],p[1]],
<a name="line_118"></a>		     [p[3],p[1],p[2]],
<a name="line_119"></a>		     [p[3],p[2],p[1]]];
<a name="line_120"></a>     this["weights"] := [op(this["weights"]),w$6];
<a name="line_121"></a>     this["base_r"] := [op(this["base_r"]),evalf(sqrt(q[1]^2+q[2]^2))];
<a name="line_122"></a>     this["base_theta"] := [op(this["base_theta"]),evalf(arctan(q[2],q[1]))];
<a name="line_123"></a>    fi;
<a name="line_124"></a>   od:
<a name="line_125"></a>
<a name="line_126"></a>   this["num_points"] := nops(this["points"]);
<a name="line_127"></a>   NULL;
<a name="line_128"></a>  end
<a name="line_129"></a> ],
<a name="line_130"></a>
<a name="line_131"></a> ["Method","int",
<a name="line_132"></a>  "This returns the approximate integral of @u@, which is expected to be an expression in the variables @t[1], t[2] and t[3]@",
<a name="line_133"></a>  proc(this,u)
<a name="line_134"></a>   local T,np,p,w,i;
<a name="line_135"></a>   T := 0;
<a name="line_136"></a>   np := this["num_points"];
<a name="line_137"></a>
<a name="line_138"></a>   for i from 1 to np do
<a name="line_139"></a>    p := this["points"][i];
<a name="line_140"></a>    w := this["weights"][i];
<a name="line_141"></a>    T := T + evalf(w * subs({t[1]=p[1],t[2]=p[2],t[3]=p[3]},u));
<a name="line_142"></a>   od:
<a name="line_143"></a>
<a name="line_144"></a>   return T;
<a name="line_145"></a>  end
<a name="line_146"></a> ],
<a name="line_147"></a>
<a name="line_148"></a> ["Method","split_int",
<a name="line_149"></a>  "This returns an approximate integral of @u@, obtained by dividing the triangle into $4^k$ smaller triangles, and applying the givenquadrature rule on each piece.",
<a name="line_150"></a>  proc(this,u,k)
<a name="line_151"></a>   if k = 0 then 
<a name="line_152"></a>    return this["int",u];
<a name="line_153"></a>   else 
<a name="line_154"></a>    return  
<a name="line_155"></a>     this["split_int",subs({t[1]=t[1]+t[2]/2+t[3]/2,t[2]=t[2]/2,t[3]=t[3]/2},u),k-1]/4 + 
<a name="line_156"></a>     this["split_int",subs({t[1]=t[1]/2,t[2]=t[1]/2+t[2]+t[3]/2,t[3]=t[3]/2},u),k-1]/4 + 
<a name="line_157"></a>     this["split_int",subs({t[1]=t[1]/2,t[2]=t[2]/2,t[3]=t[1]/2+t[2]/2+t[3]},u),k-1]/4 + 
<a name="line_158"></a>     this["split_int",subs({t[1]=t[2]/2+t[3]/2,t[2]=t[1]/2+t[3]/2,t[3]=t[1]/2+t[2]/2},u),k-1]/4;
<a name="line_159"></a>   fi:
<a name="line_160"></a>  end
<a name="line_161"></a> ],
<a name="line_162"></a>
<a name="line_163"></a> ["Method","exact_int",
<a name="line_164"></a>  "This calculates the integral of @u@ using Maple's adaptive algorithms.",
<a name="line_165"></a>  proc(this,u)
<a name="line_166"></a>   2 * evalf(int(int(subs(t[3]=1-t[1]-t[2],u),t[2]=0..1-t[1]),t[1]=0..1));
<a name="line_167"></a>  end
<a name="line_168"></a> ],
<a name="line_169"></a>
<a name="line_170"></a> ["Method","base_plot","This generates a plot showing the base points",
<a name="line_171"></a>  proc(this)
<a name="line_172"></a>   return display(
<a name="line_173"></a>    line(triangle_proj([1,0,0]),triangle_proj([0,1,0]),color=red),
<a name="line_174"></a>    line(triangle_proj([0,1,0]),triangle_proj([0,0,1]),color=red),
<a name="line_175"></a>    line(triangle_proj([0,0,1]),triangle_proj([1,0,0]),color=red),
<a name="line_176"></a>    line(triangle_proj([0,0,1]),triangle_proj([1/2,1/2,0]),color=green),
<a name="line_177"></a>    line(triangle_proj([0,1,0]),triangle_proj([1/2,0,1/2]),color=green),
<a name="line_178"></a>    line(triangle_proj([1,0,0]),triangle_proj([0,1/2,1/2]),color=green),
<a name="line_179"></a>    op(map(p -> point(evalf(triangle_proj(p))),this["base_points"])),
<a name="line_180"></a>    axes = none, scaling = constrained
<a name="line_181"></a>   ):
<a name="line_182"></a>  end
<a name="line_183"></a> ],
<a name="line_184"></a>
<a name="line_185"></a> ["Method","plot","This generates a plot showing all the quadrature points",
<a name="line_186"></a>  proc(this)
<a name="line_187"></a>   return display(
<a name="line_188"></a>    line(triangle_proj([1,0,0]),triangle_proj([0,1,0]),color=red),
<a name="line_189"></a>    line(triangle_proj([0,1,0]),triangle_proj([0,0,1]),color=red),
<a name="line_190"></a>    line(triangle_proj([0,0,1]),triangle_proj([1,0,0]),color=red),
<a name="line_191"></a>    line(triangle_proj([0,0,1]),triangle_proj([1/2,1/2,0]),color=green),
<a name="line_192"></a>    line(triangle_proj([0,1,0]),triangle_proj([1/2,0,1/2]),color=green),
<a name="line_193"></a>    line(triangle_proj([1,0,0]),triangle_proj([0,1/2,1/2]),color=green),
<a name="line_194"></a>    op(map(p -> point(evalf(triangle_proj(p))),this["points"])),
<a name="line_195"></a>    axes = none, scaling = constrained
<a name="line_196"></a>   ):
<a name="line_197"></a>  end
<a name="line_198"></a> ],
<a name="line_199"></a>
<a name="line_200"></a> ["Method","moment_eq","This generates an equation depending on @j@ and @k@, which will be satisfied if the quadrature rule is exact.",
<a name="line_201"></a>  proc(this,j,k)
<a name="line_202"></a>   local E,i,r,theta,m,w;
<a name="line_203"></a>
<a name="line_204"></a>   if j = 0 and k = 0 then
<a name="line_205"></a>    return add(w, w in this["weights"]) - 1;
<a name="line_206"></a>   fi;
<a name="line_207"></a>
<a name="line_208"></a>   if j < 2 or j > this["degree"] or k < 0 or 3*k > j or modp(j+3*k,2) <> 0 then return FAIL; fi;
<a name="line_209"></a>
<a name="line_210"></a>   E := (-1)^(k+1)*dunavant_v(j,3*k);
<a name="line_211"></a>   for i from 2 to this["num_base_points"] do
<a name="line_212"></a>    r := this["base_r"][i];
<a name="line_213"></a>    theta := this["base_theta"][i];
<a name="line_214"></a>    m := this["base_multiplicities"][i];
<a name="line_215"></a>    w := this["base_weights"][i];
<a name="line_216"></a>    if m = 3 then
<a name="line_217"></a>     E := E + 3 * w * r^j;
<a name="line_218"></a>    elif m = 6 then
<a name="line_219"></a>     E := E + 6 * w * r^j * cos(3*k*theta);
<a name="line_220"></a>    fi;
<a name="line_221"></a>   od:
<a name="line_222"></a>
<a name="line_223"></a>   return E;
<a name="line_224"></a>  end
<a name="line_225"></a> ],
<a name="line_226"></a>
<a name="line_227"></a> ["Method","accuracy"::numeric,"",
<a name="line_228"></a>  proc(this)
<a name="line_229"></a>   local L;
<a name="line_230"></a>   L := dunavant_jk_list(this["degree"]);
<a name="line_231"></a>   return max(seq(abs(this["moment_eq",op(jk)]),jk in L));
<a name="line_232"></a>  end
<a name="line_233"></a> ],
<a name="line_234"></a>
<a name="line_235"></a> ["Method","adjust","",
<a name="line_236"></a>  proc(this)
<a name="line_237"></a>   local Q1,np,m,r,theta,w,r1,theta1,w1,p1,i;
<a name="line_238"></a>
<a name="line_239"></a>   np := this["num_base_points"];
<a name="line_240"></a>
<a name="line_241"></a>   Q1 := `new/triangle_quadrature_rule`(this["degree"],[],[]);
<a name="line_242"></a>
<a name="line_243"></a>   Q1["degree"]              := this["degree"];
<a name="line_244"></a>   Q1["num_base_points"]     := this["num_base_points"];
<a name="line_245"></a>   Q1["base_multiplicities"] := this["base_multiplicities"];
<a name="line_246"></a>
<a name="line_247"></a>   Q1["base_points"] := [];
<a name="line_248"></a>   Q1["base_weights"] := [];
<a name="line_249"></a>   Q1["base_r"] := [];
<a name="line_250"></a>   Q1["base_theta"] := [];
<a name="line_251"></a>   Q1["points"] := [];
<a name="line_252"></a>   Q1["weights"] := [];
<a name="line_253"></a>
<a name="line_254"></a>   for i from 1 to np do 
<a name="line_255"></a>    m := this["base_multiplicities"][i];
<a name="line_256"></a>    r := this["base_r"][i];
<a name="line_257"></a>    theta := this["base_theta"][i];
<a name="line_258"></a>    w := this["base_weights"][i];
<a name="line_259"></a>
<a name="line_260"></a>    w1 := w + e * dw[i];
<a name="line_261"></a>
<a name="line_262"></a>    if m = 1 then
<a name="line_263"></a>     r1 := 0;
<a name="line_264"></a>     theta1 := 0;
<a name="line_265"></a>    elif m = 3 then
<a name="line_266"></a>     r1 := r + e * dr[i];
<a name="line_267"></a>     theta1 := 0;
<a name="line_268"></a>    elif m = 6 then
<a name="line_269"></a>     r1 := r + e * dr[i];
<a name="line_270"></a>     theta1 := theta + e * dt[i];
<a name="line_271"></a>    fi;
<a name="line_272"></a>
<a name="line_273"></a>    p1 := triangle_lift([r1*cos(theta1),r1*sin(theta1)]);
<a name="line_274"></a>    p1 := subs(e = 0,p1) +~ e *~ subs(e = 0,map(diff,p1,e));
<a name="line_275"></a>    p1 := evalf(expand(p1));
<a name="line_276"></a>
<a name="line_277"></a>    Q1["base_points"]  := [op(Q1["base_points"]),p1];
<a name="line_278"></a>    Q1["base_r"]       := [op(Q1["base_r"]),r1];
<a name="line_279"></a>    Q1["base_theta"]   := [op(Q1["base_theta"]),theta1];
<a name="line_280"></a>    Q1["base_weights"] := [op(Q1["base_weights"]),w1];
<a name="line_281"></a>
<a name="line_282"></a>    if m = 1 then
<a name="line_283"></a>     Q1["points"] := [op(Q1["points"]),p1];
<a name="line_284"></a>    elif m = 3 then
<a name="line_285"></a>     Q1["points"] := [op(Q1["points"]),
<a name="line_286"></a>		     [p1[1],p1[2],p1[2]],
<a name="line_287"></a>		     [p1[2],p1[1],p1[2]],
<a name="line_288"></a>		     [p1[2],p1[2],p1[1]]];
<a name="line_289"></a>    elif m = 6 then
<a name="line_290"></a>     Q1["points"] := [op(Q1["points"]),
<a name="line_291"></a>		     [p1[1],p1[2],p1[3]],
<a name="line_292"></a>		     [p1[1],p1[3],p1[2]],
<a name="line_293"></a>		     [p1[2],p1[1],p1[3]],
<a name="line_294"></a>		     [p1[2],p1[3],p1[1]],
<a name="line_295"></a>		     [p1[3],p1[1],p1[2]],
<a name="line_296"></a>		     [p1[3],p1[2],p1[1]]];
<a name="line_297"></a>    fi;
<a name="line_298"></a>
<a name="line_299"></a>    Q1["weights"] := [op(Q1["weights"]),w1$m];
<a name="line_300"></a>   od;
<a name="line_301"></a>
<a name="line_302"></a>   return eval(Q1);
<a name="line_303"></a>  end
<a name="line_304"></a> ],
<a name="line_305"></a>
<a name="line_306"></a> ["Method","improve","",
<a name="line_307"></a>  proc(this)
<a name="line_308"></a>   local Q1,JK,EE,sol,sol0,sol1,i,p,m,w;
<a name="line_309"></a>
<a name="line_310"></a>   Q1 := eval(this["adjust"]):
<a name="line_311"></a>   JK := dunavant_jk_list(this["degree"]):
<a name="line_312"></a>   EE := map(jk -> Q1["moment_eq",op(jk)],JK):
<a name="line_313"></a>   EE := evalf(map(E -> subs(e=0,E + diff(E,e)), EE)):
<a name="line_314"></a>
<a name="line_315"></a>   sol := solve(EE):
<a name="line_316"></a>
<a name="line_317"></a>   sol0 := map(u -> u=0,indets(map(rhs,sol)));
<a name="line_318"></a>   sol1 := {e=1,op(map(u -> lhs(u) = subs(sol0,rhs(u)),sol))}:
<a name="line_319"></a>   this["base_weights"] := evalf(subs(sol1,Q1["base_weights"])):
<a name="line_320"></a>   this["base_r"] := evalf(subs(sol1,Q1["base_r"])):
<a name="line_321"></a>   this["base_theta"] := evalf(subs(sol1,Q1["base_theta"])):
<a name="line_322"></a>   this["base_points"] := [seq(evalf(triangle_lift(this["base_r"][i] *~ [cos(this["base_theta"][i]),sin(this["base_theta"][i])])),
<a name="line_323"></a>			    i=1..this["num_base_points"])]:
<a name="line_324"></a>   this["points"] := []:
<a name="line_325"></a>   this["weights"] := []:
<a name="line_326"></a>   for i from 1 to this["num_base_points"] do
<a name="line_327"></a>    p := this["base_points"][i];
<a name="line_328"></a>    m := this["base_multiplicities"][i];
<a name="line_329"></a>    w := this["base_weights"][i];
<a name="line_330"></a>    this["weights"] := [op(this["weights"]),w$m];
<a name="line_331"></a>    if m = 1 then
<a name="line_332"></a>     this["points"] := [op(this["points"]),p];
<a name="line_333"></a>    elif m = 3 then
<a name="line_334"></a>     this["points"] := [op(this["points"]),
<a name="line_335"></a>		     [p[1],p[2],p[2]],
<a name="line_336"></a>		     [p[2],p[1],p[2]],
<a name="line_337"></a>		     [p[2],p[2],p[1]]];
<a name="line_338"></a>    elif m = 6 then
<a name="line_339"></a>     this["points"] := [op(this["points"]),
<a name="line_340"></a>		     [p[1],p[2],p[3]],
<a name="line_341"></a>		     [p[1],p[3],p[2]],
<a name="line_342"></a>		     [p[2],p[1],p[3]],
<a name="line_343"></a>		     [p[2],p[3],p[1]],
<a name="line_344"></a>		     [p[3],p[1],p[2]],
<a name="line_345"></a>		     [p[3],p[2],p[1]]];
<a name="line_346"></a>    fi;
<a name="line_347"></a>   od:
<a name="line_348"></a>
<a name="line_349"></a>   NULL;
<a name="line_350"></a>  end
<a name="line_351"></a> ]
<a name="line_352"></a>);
<a name="line_353"></a>
  </pre>
 </body>
</html>
    