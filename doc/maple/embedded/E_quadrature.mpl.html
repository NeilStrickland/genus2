<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a><a name="CLASS_E_quadrature_rule"></a><span style="color:red">#@ CLASS: E_quadrature_rule
</span><a name="line_2"></a>
<a name="line_3"></a>`Class/Declare`("E_quadrature_rule",
<a name="line_4"></a> "An instance of this class represents a quadrature rule for approximate integration of functions on $EX^*$.  Essentially, such a rule consists of a list of points $p_i$ in the fundamental domain $F_{16}$, together with weights $w_i\\geq 0$.  The integral over $F_{16}$ of a function $f$ is approximately $\\sum_i w_i f(p_i)$.  Thus, the integral over $EX^*$ is $\\sum_i\\sum_{g\\in G}w_if(g.p_i)$.\n\n This class also has fields and methods designed to allow us to measure and improve the accuracy of the quadrature rule.  Specifically, suppose we have calculated the integrals of some monomials $z_1^iz_2^j$ by some other method.  We can then list these monomials in the @test_monomials@ field, and their integrals in the @test_integrals@ field.  Various methods will then try to ensure that our quadrature rule gives the correct answer on those monomials.",
<a name="line_5"></a>
<a name="line_6"></a> ["Field","num_points"::integer = 0,"Number of evaluation points"],
<a name="line_7"></a>
<a name="line_8"></a> ["Field","points"::table,"The list of evaluation points.  This is represented as a table indexed by natural numbers (starting at 0), and each point is represented as an object of the class @E_point@ defined in the file @E_domain.mpl@"],
<a name="line_9"></a>
<a name="line_10"></a> ["Field","weights"::table,"The list of weights, represented as a table indexed by natural numbers."],
<a name="line_11"></a>
<a name="line_12"></a> ["Field","num_test_monomials"::integer = 0],
<a name="line_13"></a>
<a name="line_14"></a> ["Field","test_monomials"::table,
<a name="line_15"></a>   "The list of test monomials, represented as a table indexed by natural numbers."],
<a name="line_16"></a>
<a name="line_17"></a> ["Field","test_integrals"::table,
<a name="line_18"></a>   "The list of integrals over $F_{16}$ of test monomials, represented as a table indexed by natural numbers.  This can be used if we have calculated the relevant integrals by some other means.  If not, this field should be null."],
<a name="line_19"></a>
<a name="line_20"></a> ["Field","test_monomial_values"::table,
<a name="line_21"></a>   "This is a table @T@ such that @T[i,j]@ is the value of the $i$'th test monomial $m_i$ at the $j$'th evaluation point $p_j$.  It should be set using the @set_monomial_values@ method."],
<a name="line_22"></a>
<a name="line_23"></a> ["Field","test_monomial_diff_values"::table,
<a name="line_24"></a>   "This is a table @T@ such that @T[i,j,0]@ and @T[i,j,1]@ measure the derivatives of $m_i$ at the point $p_j$.  In more detail, $p_j$ is stored as an instance of the class @E_point@, so it comes packaged with two tangent vectors $u_j$ and $v_j$; the entries @T[i,j,0]@ and @T[i,j,1]@ are the derivatives in these directions.  If $p_j$ is in the interior of $F_{16}$ then $u_j$ and $v_j$ will form an oriented orthonormal frame.  If $p_j$ is on an edge of $F_{16}$ then $u_j$ will be a unit vector pointing along the edge, and $v_j$ will be zero.  If $p_j$ is at a corner then both $u_j$ and $v_j$ will be zero."
<a name="line_25"></a> ],
<a name="line_26"></a>
<a name="line_27"></a> ["Field","errors"::table],
<a name="line_28"></a> ["Field","total_square_error" = 0],
<a name="line_29"></a>
<a name="line_30"></a> ["Field","eval_det"::RR1,
<a name="line_31"></a>  "Assuming that the number of test monomials is at least as large as the number $n$ of evaluation points, the @set_eval_det@ method will set the @eval_det@ field to be the determinant of the $n\\times n$ matrix of values of the first $n$ test monomials at the evaluation points.  Some theory developed in a slightly different context suggests that it is desirable to adjust the evaluation points to make this determinant as large as possible."
<a name="line_32"></a> ],
<a name="line_33"></a>
<a name="line_34"></a> ["Field","eval_det_diff"::table,
<a name="line_35"></a>  "Assuming that the number of test monomials is at least as large as the number $n$ of evaluation points, the @set_eval_det_diff@ method will set the @eval_det_diff@ field to be a table @T@ containing information about how @eval_det@ changes as the evaluation points are moved.  In more detail, @T[j,0]@ is the derivative of @eval_det@ if $p_j$ is moved in the direction $u_j$, and @T[j,1]@ is the derivative of @eval_det@ if $p_j$ is moved in the direction $v_j$."
<a name="line_36"></a> ],
<a name="line_37"></a>
<a name="line_38"></a> ["Constructor","",
<a name="line_39"></a>  proc(this)
<a name="line_40"></a>   local f;
<a name="line_41"></a>
<a name="line_42"></a>   for f in ["points",
<a name="line_43"></a>             "weights",
<a name="line_44"></a>             "test_monomials",
<a name="line_45"></a>             "test_monomial_values",
<a name="line_46"></a>             "test_monomial_diff_values",
<a name="line_47"></a>             "errors"] do 
<a name="line_48"></a>    this[f] := table();
<a name="line_49"></a>   od;
<a name="line_50"></a>
<a name="line_51"></a>   this["test_integrals"] := NULL;
<a name="line_52"></a>   this["eval_det_diff"] := NULL;
<a name="line_53"></a>  end
<a name="line_54"></a> ],
<a name="line_55"></a>
<a name="line_56"></a> ["Method","clone"::E_quadrature_rule,
<a name="line_57"></a>  "Returns a cloned copy of this quadrature rule",
<a name="line_58"></a>  proc(this)
<a name="line_59"></a>   local Q,i,f;
<a name="line_60"></a>
<a name="line_61"></a>   Q := `new/E_quadrature_rule`();
<a name="line_62"></a>   Q["num_points"] := this["num_points"];
<a name="line_63"></a>   Q["num_test_monomials"] := this["num_test_monomials"];
<a name="line_64"></a>
<a name="line_65"></a>   for i from 0 to this["num_points"]-1 do
<a name="line_66"></a>    Q["points"][i] := eval(this["points"][i]["clone"]);
<a name="line_67"></a>   od;
<a name="line_68"></a>
<a name="line_69"></a>   for f in ["weights",
<a name="line_70"></a>             "test_monomials",
<a name="line_71"></a>             "test_integrals",
<a name="line_72"></a>             "test_monomial_values",
<a name="line_73"></a>             "test_monomial_diff_values",
<a name="line_74"></a>             "errors",
<a name="line_75"></a>             "eval_det_diff"] do 
<a name="line_76"></a>    if this[f] = NULL then
<a name="line_77"></a>     Q[f] := NULL;
<a name="line_78"></a>    else
<a name="line_79"></a>     Q[f] := copy(this[f]);
<a name="line_80"></a>    fi;
<a name="line_81"></a>   od;
<a name="line_82"></a>
<a name="line_83"></a>   return eval(Q);
<a name="line_84"></a>  end
<a name="line_85"></a> ],
<a name="line_86"></a>
<a name="line_87"></a> ["Method","degrees_of_freedom"::integer,
<a name="line_88"></a>  "Some of the evaluation points lie in the interior of $F_{16}$ and so have two degrees of freedom to move.  Some points may lie on an  edge or at a corner, so that they have one or zero degrees of freedom.  This methd returns the total number of degrees of freedom for all evaluation points.",
<a name="line_89"></a>  proc(this)
<a name="line_90"></a>   local dof,i,P,c;
<a name="line_91"></a>
<a name="line_92"></a>   dof := 0;
<a name="line_93"></a>   for i from 0 to this["num_points"]-1 do
<a name="line_94"></a>    P := eval(this["points"][i]);
<a name="line_95"></a>    c := P["constraint"];
<a name="line_96"></a>    if c = CONSTRAINT_FREE then
<a name="line_97"></a>     dof := dof + 2;
<a name="line_98"></a>    elif c < CONSTRAINT_FIXED then
<a name="line_99"></a>     dof := dof + 1;
<a name="line_100"></a>    fi;
<a name="line_101"></a>   od;
<a name="line_102"></a>
<a name="line_103"></a>   return dof;
<a name="line_104"></a>  end
<a name="line_105"></a> ],
<a name="line_106"></a>
<a name="line_107"></a> ["Method","choose_random_points"::void,
<a name="line_108"></a>  "This method randomly chooses $n$ evaluation points in the interior of $F_{16}$, and gives them all the same weight.",
<a name="line_109"></a>  proc(this,n)
<a name="line_110"></a>   local i,P;
<a name="line_111"></a>
<a name="line_112"></a>   this["num_points"] := n;
<a name="line_113"></a>   this["points"] := table();
<a name="line_114"></a>   this["weights"] := table();
<a name="line_115"></a>
<a name="line_116"></a>   for i from 0 to n-1 do
<a name="line_117"></a>    P := `new/E_point`();
<a name="line_118"></a>    P["set_random"];
<a name="line_119"></a>    this["points"][i] := eval(P): 
<a name="line_120"></a>    this["weights"][i] := 1.171882066198705389190745405111444384076232479768421659034323598323228171698800702578886902713314708/n;
<a name="line_121"></a>   od:
<a name="line_122"></a>
<a name="line_123"></a>   this["set_monomial_values"];
<a name="line_124"></a>   this["set_eval_det"];
<a name="line_125"></a>   NULL;
<a name="line_126"></a>  end
<a name="line_127"></a> ],
<a name="line_128"></a>
<a name="line_129"></a> ["Method","adjust"::void,"",
<a name="line_130"></a>  proc(this,t,offset_)
<a name="line_131"></a>   local i,j,P,c;
<a name="line_132"></a>
<a name="line_133"></a>   i := `if`(nargs > 2,offset_,0);
<a name="line_134"></a>   for j from 0 to this["num_points"]-1 do
<a name="line_135"></a>    P := eval(this["points"][j]);
<a name="line_136"></a>    c := P["constraint"];
<a name="line_137"></a>    if c = CONSTRAINT_FREE then
<a name="line_138"></a>     P["adjust",t[i],t[i+1]];
<a name="line_139"></a>     i := i+2;
<a name="line_140"></a>    elif c < CONSTRAINT_FIXED then
<a name="line_141"></a>     P["adjust",t[i]];
<a name="line_142"></a>     i := i+1;
<a name="line_143"></a>    fi;
<a name="line_144"></a>   od;
<a name="line_145"></a>
<a name="line_146"></a>   this["set_monomial_values"];
<a name="line_147"></a>   NULL;
<a name="line_148"></a>  end
<a name="line_149"></a> ],
<a name="line_150"></a>
<a name="line_151"></a> ["Method","adjust_randomly"::void,"",
<a name="line_152"></a>  proc(this,scale_)
<a name="line_153"></a>   local scale,r,i,t;
<a name="line_154"></a>   scale := `if`(nargs > 1, scale_, 0.1);
<a name="line_155"></a>   r := () -> 0.001 * rand(-1000..1000)();
<a name="line_156"></a>   t := [seq(scale * r(),i=1..this["degrees_of_freedom"])];
<a name="line_157"></a>   this["adjust",t,1];
<a name="line_158"></a>  end
<a name="line_159"></a> ],
<a name="line_160"></a>
<a name="line_161"></a> ["Method","add_new_point","Add a new evaluation point with the specified coordinates",
<a name="line_162"></a>  proc(this,x0::[scalar,scalar,scalar,scalar])
<a name="line_163"></a>   local k,P;
<a name="line_164"></a>
<a name="line_165"></a>   P := `new/E_point`();
<a name="line_166"></a>   P["set",CONSTRAINT_FREE,x0];
<a name="line_167"></a>   k := this["num_points"];
<a name="line_168"></a>   this["points"][k] := eval(P);
<a name="line_169"></a>   this["weights"][k] := 0;
<a name="line_170"></a>   this["num_points"] := k+1;
<a name="line_171"></a>   this["set_monomial_values"];
<a name="line_172"></a>   this["set_eval_det"];
<a name="line_173"></a>  end
<a name="line_174"></a> ],
<a name="line_175"></a>
<a name="line_176"></a> ["Method","set_max_deg"::void,"Set the list of test monomials to be the list of all monomials of total degree less than or equal to the argument @max_deg@.  Also, calculate the corresponding test integrals using the @E_grid@ object supplied as the @grid@ argument.  (This may be slow.)",
<a name="line_177"></a>  proc(this,max_deg,grid::E_grid)
<a name="line_178"></a>   local i,j,k,m;
<a name="line_179"></a>
<a name="line_180"></a>   this["test_monomials"] := table();
<a name="line_181"></a>   this["test_integrals"] := table();
<a name="line_182"></a>   k := 0;
<a name="line_183"></a>   for m from 0 to max_deg do
<a name="line_184"></a>    for i from 0 to m do
<a name="line_185"></a>     j := m - i;
<a name="line_186"></a>     this["test_monomials"][k] := z[1]^i * z[2]^j;
<a name="line_187"></a>     userinfo(7,genus2,sprintf("Integrating %A",this["test_monomials"][k]));
<a name="line_188"></a>     this["test_integrals"][k] := grid["int_z_by_table",this["test_monomials"][k]];
<a name="line_189"></a>     k := k+1;
<a name="line_190"></a>    od;
<a name="line_191"></a>   od:
<a name="line_192"></a>   this["num_test_monomials"] := k;
<a name="line_193"></a>   this["set_monomial_values"];
<a name="line_194"></a>   NULL;
<a name="line_195"></a>  end
<a name="line_196"></a> ],
<a name="line_197"></a>
<a name="line_198"></a> ["Method","int_z"::scalar,"Integrate @f@, which should be an expression involving $z_1$ and $z_2$.",
<a name="line_199"></a>  proc(this,f)
<a name="line_200"></a>   local i,P,J;
<a name="line_201"></a>   J := 0;
<a name="line_202"></a>   for i from 0 to this["num_points"]-1 do
<a name="line_203"></a>    P := eval(this["points"][i]);
<a name="line_204"></a>    J := evalf(J + this["weights"][i] * subs({z[1]=P["z"][1],z[2]=P["z"][2]},f));
<a name="line_205"></a>   od;
<a name="line_206"></a>   return(J);
<a name="line_207"></a>  end
<a name="line_208"></a> ],
<a name="line_209"></a>
<a name="line_210"></a> ["Method","set_monomial_values"::void,"Calculate the values and derivatives of all the test monomials at the evaluation points.",
<a name="line_211"></a>  proc(this)
<a name="line_212"></a>   local i,j,k,P,z1,z2,z1u,z1v,z2u,z2v,m,n,PP,Px,Pz,Pu,Pv,dzdx,xr,TMV,TMDV;
<a name="line_213"></a>
<a name="line_214"></a>   PP := eval(this["points"]);
<a name="line_215"></a>   n := this["num_points"];
<a name="line_216"></a>
<a name="line_217"></a>   Px := [seq(PP[j]["x"],j=0..n-1)];
<a name="line_218"></a>   Pz := [seq(PP[j]["z"],j=0..n-1)];
<a name="line_219"></a>   Pu := [seq(PP[j]["u"],j=0..n-1)];
<a name="line_220"></a>   Pv := [seq(PP[j]["v"],j=0..n-1)];
<a name="line_221"></a>   dzdx := [seq([seq(diff(zx0[i],x[j]),j=1..4)],i=1..2)];
<a name="line_222"></a>
<a name="line_223"></a>   TMV := table():
<a name="line_224"></a>   TMDV := table():
<a name="line_225"></a>   for j from 0 to n-1 do
<a name="line_226"></a>    z1 := Pz[j+1][1];
<a name="line_227"></a>    z2 := Pz[j+1][2];
<a name="line_228"></a>    xr := {seq(x[i]=Px[j+1][i],i=1..4)};
<a name="line_229"></a>    z1u := subs(xr,add(dzdx[1,k] * Pu[j+1][k],k=1..4));
<a name="line_230"></a>    z1v := subs(xr,add(dzdx[1,k] * Pv[j+1][k],k=1..4));
<a name="line_231"></a>    z2u := subs(xr,add(dzdx[2,k] * Pu[j+1][k],k=1..4));
<a name="line_232"></a>    z2v := subs(xr,add(dzdx[2,k] * Pv[j+1][k],k=1..4));
<a name="line_233"></a>
<a name="line_234"></a>    for i from 0 to this["num_test_monomials"]-1 do 
<a name="line_235"></a>     m := this["test_monomials"][i];
<a name="line_236"></a>     TMV[i,j] := evalf(subs({z[1]=z1,z[2]=z2},m));
<a name="line_237"></a>     TMDV[i,j,0] := 
<a name="line_238"></a>      evalf(subs({z[1]=z1,z[2]=z2},diff(m,z[1])*z1u + diff(m,z[2])*z2u));
<a name="line_239"></a>     TMDV[i,j,1] := 
<a name="line_240"></a>      evalf(subs({z[1]=z1,z[2]=z2},diff(m,z[1])*z1v + diff(m,z[2])*z2v));
<a name="line_241"></a>    od;
<a name="line_242"></a>   od;
<a name="line_243"></a>
<a name="line_244"></a>   this["test_monomial_values"] := eval(TMV);
<a name="line_245"></a>   this["test_monomial_diff_values"] := eval(TMDV);
<a name="line_246"></a>   this["eval_det"] := NULL;
<a name="line_247"></a>   this["eval_det_diff"] := NULL;
<a name="line_248"></a>  end
<a name="line_249"></a> ],
<a name="line_250"></a>
<a name="line_251"></a> ["Method","solve_weights"::void,"Set the @weights@ field to minimize the mean square integration error for all the test monomials, subject to the constraint that all weights should be nonnegative.",
<a name="line_252"></a>  proc(this)
<a name="line_253"></a>   local n,m,i,j,TMV,TMVM,TI,TIV,WV;
<a name="line_254"></a>
<a name="line_255"></a>   n := this["num_test_monomials"];
<a name="line_256"></a>   m := this["num_points"];
<a name="line_257"></a>   TMV := eval(this["test_monomial_values"]):
<a name="line_258"></a>   TI := eval(this["test_integrals"]):
<a name="line_259"></a>   TMVM := Matrix([seq([seq(TMV[i,j],j=0..m-1)],i=0..n-1)]);
<a name="line_260"></a>   TIV := Vector([seq(TI[i],i=0..n-1)]);
<a name="line_261"></a>
<a name="line_262"></a>   WV := LSSolve([TIV,TMVM],assume=nonnegative)[2];
<a name="line_263"></a>   
<a name="line_264"></a>   for j from 0 to m-1 do
<a name="line_265"></a>    this["weights"][j] := WV[j+1];
<a name="line_266"></a>   od;
<a name="line_267"></a>   NULL;
<a name="line_268"></a>  end
<a name="line_269"></a> ],
<a name="line_270"></a>
<a name="line_271"></a> ["Method","weight_list","Convert the @weights@ table to a list.",
<a name="line_272"></a>  proc(this)
<a name="line_273"></a>   [seq(this["weights"][i],i=0..this["num_points"]-1)];
<a name="line_274"></a>  end
<a name="line_275"></a> ],
<a name="line_276"></a>
<a name="line_277"></a> ["Method","sorted_weight_list","Convert the @weights@ table to a list, and sort it.",
<a name="line_278"></a>  proc(this)
<a name="line_279"></a>   sort([seq(this["weights"][i],i=0..this["num_points"]-1)]);
<a name="line_280"></a>  end
<a name="line_281"></a> ],
<a name="line_282"></a>
<a name="line_283"></a> ["Method","set_eval_det"::scalar,"Set the @eval_det@ field.",
<a name="line_284"></a>  proc(this)
<a name="line_285"></a>   local n,m,TMV;
<a name="line_286"></a>
<a name="line_287"></a>   n := this["num_points"];
<a name="line_288"></a>   m := this["num_test_monomials"];
<a name="line_289"></a>   if m < n then return NULL; fi;
<a name="line_290"></a>
<a name="line_291"></a>   TMV := eval(this["test_monomial_values"]);
<a name="line_292"></a>   this["eval_det"] := 
<a name="line_293"></a>    Determinant(Matrix([seq([seq(TMV[i,j],j=0..n-1)],i=0..n-1)]));
<a name="line_294"></a>   return this["eval_det"];
<a name="line_295"></a>  end
<a name="line_296"></a> ],
<a name="line_297"></a>
<a name="line_298"></a> ["Method","set_eval_det_diff","Set the @eval_det_diff@ field.  This method is quite slow.",
<a name="line_299"></a>  proc(this)
<a name="line_300"></a>   local i,j,k,n,M0,M1,T,TMV,TMDV;
<a name="line_301"></a>   n := this["num_points"];
<a name="line_302"></a>   TMV := eval(this["test_monomial_values"]);
<a name="line_303"></a>   TMDV := eval(this["test_monomial_diff_values"]);
<a name="line_304"></a>   M0 := Matrix([seq([seq(TMV[i,j],j=0..n-1)],i=0..n-1)]);
<a name="line_305"></a>   M1 := Matrix([seq([seq(TMV[i,j],j=0..n-1)],i=0..n-1)]);
<a name="line_306"></a>   T := table();
<a name="line_307"></a>   for j from 0 to n-1 do # points
<a name="line_308"></a>    if j > 0 then
<a name="line_309"></a>     for i from 0 to n-1 do M1[i+1,j] := M0[i+1,j]; od;
<a name="line_310"></a>    fi;
<a name="line_311"></a>    for k from 0 to 1 do
<a name="line_312"></a>     for i from 0 to n-1 do # monomials
<a name="line_313"></a>      M1[i+1,j+1] := TMDV[i,j,k];
<a name="line_314"></a>     od;
<a name="line_315"></a>     T[j,k] := Determinant(M1);
<a name="line_316"></a>    od;
<a name="line_317"></a>   od;
<a name="line_318"></a>   this["eval_det_diff"] := eval(T);
<a name="line_319"></a>   return(eval(T));
<a name="line_320"></a>  end
<a name="line_321"></a> ],
<a name="line_322"></a>
<a name="line_323"></a> ["Method","increase_eval_det_once","Adjust the evaluation points to increase @eval_det@.  This assumes that @eval_det@ has already been calculated, but not @eval_det_diff@.  It calculates @eval_det_diff@ for the unadjusted points, but does not recalculate it after performing the adjustment.",
<a name="line_324"></a>  proc(this,step)
<a name="line_325"></a>   local n,d,m,i,j,k,r,R,t,T,c,sgn;
<a name="line_326"></a>
<a name="line_327"></a>   n := this["num_points"];
<a name="line_328"></a>   d := this["degrees_of_freedom"];
<a name="line_329"></a>   m := this["num_test_monomials"];
<a name="line_330"></a>   if (this["eval_det"] = NULL) then 
<a name="line_331"></a>    this["set_eval_det"];
<a name="line_332"></a>   fi;
<a name="line_333"></a>   sgn := signum(this["eval_det"]);
<a name="line_334"></a>
<a name="line_335"></a>   t := table();
<a name="line_336"></a>   T := eval(this["set_eval_det_diff"]):
<a name="line_337"></a>   R := 0;
<a name="line_338"></a>   k := 0;
<a name="line_339"></a>   for i from 0 to n-1 do
<a name="line_340"></a>    c := this["points"][i]["constraint"];
<a name="line_341"></a>    r := 0;
<a name="line_342"></a>    if c < CONSTRAINT_FIXED then
<a name="line_343"></a>     t[k] := T[i,0];
<a name="line_344"></a>     r := abs(t[k]);
<a name="line_345"></a>     k := k + 1;
<a name="line_346"></a>     if c = CONSTRAINT_FREE then
<a name="line_347"></a>      t[k] := T[i,1];
<a name="line_348"></a>      r := sqrt(t[k-1]^2+t[k]^2);
<a name="line_349"></a>      k := k + 1;
<a name="line_350"></a>     fi;
<a name="line_351"></a>    fi;
<a name="line_352"></a>    R := max(R,r);
<a name="line_353"></a>   od;
<a name="line_354"></a>
<a name="line_355"></a>   for i from 0 to d-1 do
<a name="line_356"></a>    t[i] := t[i] * step/R;
<a name="line_357"></a>   od;
<a name="line_358"></a>   this["eval_det_diff"] := NULL;
<a name="line_359"></a>
<a name="line_360"></a>   this["adjust",t];
<a name="line_361"></a>   this["set_eval_det"];
<a name="line_362"></a>  end
<a name="line_363"></a> ],
<a name="line_364"></a>
<a name="line_365"></a> ["Method","increase_eval_det","",
<a name="line_366"></a>  proc(this,step_,num_steps_)
<a name="line_367"></a>   local step,num_steps,d0,d1,i;
<a name="line_368"></a>
<a name="line_369"></a>   step  := `if`(nargs > 1,step_,0.1);
<a name="line_370"></a>   num_steps := `if`(nargs > 2,num_steps,10);
<a name="line_371"></a>   if (this["eval_det"] = NULL) then 
<a name="line_372"></a>    this["set_eval_det"];
<a name="line_373"></a>   fi;
<a name="line_374"></a>   d0 := abs(this["eval_det"]);
<a name="line_375"></a>
<a name="line_376"></a>   for i from 1 to num_steps do
<a name="line_377"></a>    d1 := abs(this["increase_eval_det_once",step]);
<a name="line_378"></a>    userinfo(7,genus2,sprintf("log(abs(det)) = %.3f",log[10](d1)));
<a name="line_379"></a>    if d1 < d0 then
<a name="line_380"></a>     step := step/2;
<a name="line_381"></a>     userinfo(7,genus2,sprintf("reducing step size to %A",step));
<a name="line_382"></a>    fi;
<a name="line_383"></a>    d0 := d1;
<a name="line_384"></a>   od;
<a name="line_385"></a>
<a name="line_386"></a>   this["eval_det"];
<a name="line_387"></a>  end
<a name="line_388"></a> ],
<a name="line_389"></a>
<a name="line_390"></a> ["Method","set_errors","Calculate all the errors obtained when integrating all the test monomials using this quadrature rule.  This assumes that correct values for all the integrals have already been stored in the @test_integrals@ field. ",
<a name="line_391"></a>  proc(this)
<a name="line_392"></a>   local n,m,i,E,J,TI,TMV,W;
<a name="line_393"></a>
<a name="line_394"></a>   n := this["num_points"];
<a name="line_395"></a>   m := this["num_test_monomials"];
<a name="line_396"></a>   TI := eval(this["test_integrals"]);
<a name="line_397"></a>   TMV := eval(this["test_monomial_values"]);
<a name="line_398"></a>   W := eval(this["weights"]);
<a name="line_399"></a>
<a name="line_400"></a>   E := table();
<a name="line_401"></a>   for i from 0 to m-1 do
<a name="line_402"></a>    E[i] := TI[i] - add(W[j] * TMV[i,j],j=0..n-1);
<a name="line_403"></a>   od;
<a name="line_404"></a>
<a name="line_405"></a>   this["errors"] := eval(E);
<a name="line_406"></a>   this["total_square_error"] := add(E[i]^2,i=0..m-1);
<a name="line_407"></a>  end  
<a name="line_408"></a> ],
<a name="line_409"></a>
<a name="line_410"></a> ["Method","reduce_errors_once","This adjusts the evaluation points and weights to attempt to reduce the integration errors.  We have not made much use of this; instead, we have used @increase_eval_det@ repeatedly, followed by @solve_weights@.",
<a name="line_411"></a>  proc(this,max_step_)
<a name="line_412"></a>   local n,d,m,i,j,k,a,B,u,v,c,max_step,TI,TMV,TMDV,W;
<a name="line_413"></a>
<a name="line_414"></a>   max_step := `if`(nargs > 1, max_step_, 0.1);
<a name="line_415"></a>   n := this["num_points"];
<a name="line_416"></a>   d := this["degrees_of_freedom"];
<a name="line_417"></a>   m := this["num_test_monomials"];
<a name="line_418"></a>   this["set_errors"];
<a name="line_419"></a>   a := Transpose(Vector(m));
<a name="line_420"></a>   B := Matrix(m,d+n);
<a name="line_421"></a>
<a name="line_422"></a>   TI := eval(this["test_integrals"]);
<a name="line_423"></a>   TMV := eval(this["test_monomial_values"]);
<a name="line_424"></a>   TMDV := eval(this["test_monomial_diff_values"]);
<a name="line_425"></a>   W := eval(this["weights"]);
<a name="line_426"></a>   
<a name="line_427"></a>   for i from 0 to m-1 do
<a name="line_428"></a>    a[i+1] := this["errors"][i];
<a name="line_429"></a>    k := 0;
<a name="line_430"></a>    for j from 0 to n-1 do
<a name="line_431"></a>     c := this["points"][j]["constraint"];
<a name="line_432"></a>     B[i+1,j    +1] := TMV[i,j];
<a name="line_433"></a>     if c < CONSTRAINT_FIXED then
<a name="line_434"></a>      B[i+1,k+n  +1] := W[j] * TMDV[i,j,0];
<a name="line_435"></a>      k := k + 1;
<a name="line_436"></a>      if c = CONSTRAINT_FREE then
<a name="line_437"></a>       B[i+1,k+n  +1] := W[j] * TMDV[i,j,1];
<a name="line_438"></a>       k := k + 1;
<a name="line_439"></a>      fi;
<a name="line_440"></a>     fi;
<a name="line_441"></a>    od;
<a name="line_442"></a>   od;
<a name="line_443"></a>   u := a . B;
<a name="line_444"></a>   c := (a . Transpose(a))/(2 * u . Transpose(u));
<a name="line_445"></a>   v := c * u;
<a name="line_446"></a>#   v := MatrixInverse(Transpose(B).B,method = pseudo) . (a . B);
<a name="line_447"></a>   if Norm(v,2) > max_step then
<a name="line_448"></a>    v := max_step * v / Norm(v,2);
<a name="line_449"></a>   fi;
<a name="line_450"></a>   for i from 0 to n-1 do
<a name="line_451"></a>    this["weights"][i] := this["weights"][i] + v[i+1];
<a name="line_452"></a>   od;
<a name="line_453"></a>
<a name="line_454"></a>   this["adjust",v,n+1];
<a name="line_455"></a>   this["set_monomial_values"];
<a name="line_456"></a>   this["set_errors"];
<a name="line_457"></a>  end
<a name="line_458"></a> ],
<a name="line_459"></a>
<a name="line_460"></a> ["Method","reduce_errors","",
<a name="line_461"></a>  proc(this,max_step_,num_steps_)
<a name="line_462"></a>   local max_step,num_steps,e0,e1,i;
<a name="line_463"></a>
<a name="line_464"></a>   max_step  := `if`(nargs > 1,max_step_,0.1);
<a name="line_465"></a>   num_steps := `if`(nargs > 2,num_steps,10);
<a name="line_466"></a>   e0 := this["set_errors"];
<a name="line_467"></a>
<a name="line_468"></a>   for i from 1 to num_steps do
<a name="line_469"></a>    e1 := this["reduce_errors_once",max_step];
<a name="line_470"></a>    userinfo(7,genus2,sprintf("log(error) = %.3f",log[10](e1)));
<a name="line_471"></a>    if e1 > e0 then
<a name="line_472"></a>     max_step := max_step/2;
<a name="line_473"></a>     userinfo(7,genus2,sprintf("reducing step size to %4f",max_step));
<a name="line_474"></a>    fi;
<a name="line_475"></a>    e0 := e1;
<a name="line_476"></a>   od;
<a name="line_477"></a>
<a name="line_478"></a>   e1;
<a name="line_479"></a>  end
<a name="line_480"></a> ],
<a name="line_481"></a>
<a name="line_482"></a> ["Method","curvature_error","If the integration rule is accurate, then the result of this method should be zero, by the Gauss-Bonet Theorem.",
<a name="line_483"></a>  proc(this)
<a name="line_484"></a>   evalf(Pi/4 + this["int_z",curvature_z0(z)]);
<a name="line_485"></a>  end
<a name="line_486"></a> ],
<a name="line_487"></a>
<a name="line_488"></a> ["Method","log_curvature_error","",
<a name="line_489"></a>  proc(this)
<a name="line_490"></a>   return log[10](abs(this["curvature_error"]));
<a name="line_491"></a>  end
<a name="line_492"></a> ],
<a name="line_493"></a>
<a name="line_494"></a> ["Method","stokes_error","Here @ff@ should be a list $(f_1,f_2)$ of two expressions in $z_1$ and $z_2$.  The method returns the approximated integral of $d(f_1\\alpha_1+f_2\\alpha_2)$, where the forms $\\alpha_i$ are defined in @embedded/roothalf/forms.mpl@.  If the integration rule is accurate, then the result should be zero, by Stokes's Theorem.  The formulae used are only valid when $a=1/\\sqrt{2}$, so the method gives an error in other cases.",
<a name="line_495"></a>  proc(this,ff)
<a name="line_496"></a>   if `class/E_point`["StaticFieldValue"]["a"] <> 1/sqrt(2) then
<a name="line_497"></a>    error("Method is only valid for EX^*");
<a name="line_498"></a>   fi;
<a name="line_499"></a>   
<a name="line_500"></a>   this["int_z",stokes_alpha(ff)];
<a name="line_501"></a>  end
<a name="line_502"></a> ],
<a name="line_503"></a>
<a name="line_504"></a> ["Method","max_stokes_error","",
<a name="line_505"></a>  proc(this)
<a name="line_506"></a>   local i,m,TM,NM,errs;
<a name="line_507"></a>   
<a name="line_508"></a>   TM := this["test_monomials"];
<a name="line_509"></a>   m := this["num_test_monomials"];
<a name="line_510"></a>   NM := table();
<a name="line_511"></a>   for i from 0 to m-1 do
<a name="line_512"></a>    NM[i] := sqrt(this["int_z",TM[i]^2]);
<a name="line_513"></a>   od:
<a name="line_514"></a>   
<a name="line_515"></a>   errs := [
<a name="line_516"></a>    seq(this["stokes_error",[TM[i],0]]/NM[i],i=0..m-1),
<a name="line_517"></a>    seq(this["stokes_error",[0,TM[i]]]/NM[i],i=0..m-1)
<a name="line_518"></a>   ];
<a name="line_519"></a>
<a name="line_520"></a>   return(max(map(abs,errs)));
<a name="line_521"></a>  end
<a name="line_522"></a> ],
<a name="line_523"></a> 
<a name="line_524"></a> ["Method","log_max_stokes_error","",
<a name="line_525"></a>  proc(this)
<a name="line_526"></a>   return log[10](abs(this["max_stokes_error"]));
<a name="line_527"></a>  end
<a name="line_528"></a> ],
<a name="line_529"></a>
<a name="line_530"></a> ["Method","plot_z","This generates a plot showing the location of the evaluation points in the $z$-plane.",
<a name="line_531"></a>  proc(this)
<a name="line_532"></a>   load_plot("z_proj_F16_bare");
<a name="line_533"></a>   display(pics["z_proj_F16_bare"],seq(point(this["points"][i]["z"]),i=0..this["num_points"]-1));
<a name="line_534"></a>  end
<a name="line_535"></a> ],
<a name="line_536"></a>
<a name="line_537"></a> ["Method","error_plot","Generate a plot showing $\\log_{10}$ of the relative errors $|Q(m)-\\int m|/\\|m\\|_2$, as $m$ runs through the test monomials.",
<a name="line_538"></a>  proc(this)
<a name="line_539"></a>   local i,m,TI,TM,errs;
<a name="line_540"></a>   
<a name="line_541"></a>   TM := this["test_monomials"];
<a name="line_542"></a>   TI := this["test_integrals"];
<a name="line_543"></a>   m := this["num_test_monomials"];
<a name="line_544"></a>   errs := [seq((this["int_z",TM[i]]-TI[i])/sqrt(this["int_z",TM[i]^2]),i=0..m-1)];
<a name="line_545"></a>   listplot(map(log[10],map(abs,errs)),style=point);
<a name="line_546"></a>  end
<a name="line_547"></a> ],
<a name="line_548"></a>
<a name="line_549"></a> ["Method","describe","",
<a name="line_550"></a>  proc(this)
<a name="line_551"></a>   local d,s,nn,nz;
<a name="line_552"></a>
<a name="line_553"></a>   d := max(map(degree,subs(z[2]=z[1],map(op,[entries(this["test_monomials"])])),z[1]));
<a name="line_554"></a>   s := sprintf("This is a quadrature rule with %d points and %d test monomials of maximum degree %d.  ",this["num_points"],this["num_test_monomials"],d);
<a name="line_555"></a>   nz := nops(select(w -> evalf(w) = 0.,this["weight_list"]));
<a name="line_556"></a>   nn := nops(select(w -> evalf(w) < 0.,this["weight_list"]));
<a name="line_557"></a>   if nn = 0 then
<a name="line_558"></a>    if nz = 0 then
<a name="line_559"></a>     s := cat(s,"All weights are strictly positive.  ");
<a name="line_560"></a>    elif nz = 1 then
<a name="line_561"></a>     s := cat(s,"There is one point with weight zero, and all other points have positive weight.  ");
<a name="line_562"></a>    else  
<a name="line_563"></a>     s := cat(s,sprintf("There are %d points with weight zero, and all other points have positive weight.  ",nz));
<a name="line_564"></a>    fi;
<a name="line_565"></a>   elif nn = 1 then
<a name="line_566"></a>    if nz = 0 then
<a name="line_567"></a>     s := cat(s,"There is one point with negative weight, and all other weights are strictly positive.  ");
<a name="line_568"></a>    elif nz = 1 then
<a name="line_569"></a>     s := cat(s,"There is one point with negative weight and one with weight zero, but all other points have positive weight.  ");
<a name="line_570"></a>    else  
<a name="line_571"></a>     s := cat(s,sprintf("There is one point with negative weight and %d points with weight zero, and all other points have positive weight.  ",nz));
<a name="line_572"></a>    fi;
<a name="line_573"></a>   else
<a name="line_574"></a>    if nz = 0 then
<a name="line_575"></a>     s := cat(s,sprintf("There are %d points with negative weight, and all other weights are strictly positive.  ",nn));
<a name="line_576"></a>    elif nz = 1 then
<a name="line_577"></a>     s := cat(s,sprintf("There are %d points with negative weight and one with weight zero, but all other points have positive weight.  ",nn));
<a name="line_578"></a>    else  
<a name="line_579"></a>     s := cat(s,sprintf("There are %d points with negative weight and %d points with weight zero, and all other points have positive weight.  ",nn,nz));
<a name="line_580"></a>    fi;
<a name="line_581"></a>   fi;
<a name="line_582"></a>  
<a name="line_583"></a>   if type(this["eval_det"],numeric) then
<a name="line_584"></a>    s := cat(s,sprintf("The log of the evaluation determinant is %.3f.  ",log[10](abs(this["eval_det"]))));
<a name="line_585"></a>   fi;
<a name="line_586"></a>
<a name="line_587"></a>   return s;
<a name="line_588"></a>  end
<a name="line_589"></a> ]
<a name="line_590"></a>):
  </pre>
 </body>
</html>
    