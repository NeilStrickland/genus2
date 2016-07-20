<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>`Class/Declare`("HP_table",
<a name="line_2"></a> "An instance of this class encapsulates data about a family of cromulent isomorphisms $HX(a_H) \\to PX(a_P)$ for various values of $a_H$ and $a_P$",
<a name="line_3"></a>
<a name="line_4"></a> ["Field","H_to_P_maps"::table,"A table of objects of class @H_to_P_map@, indexed by values of @a_H@"],
<a name="line_5"></a>
<a name="line_6"></a> ["Field","P_to_H_maps"::table,"A table of objects of class @P_to_H_map@, indexed by values of @a_P@"],
<a name="line_7"></a>
<a name="line_8"></a> ["Field","H_to_P_num_samples"::posint = 200],
<a name="line_9"></a> ["Field","H_to_P_poly_deg"::posint = 20],
<a name="line_10"></a> ["Field","H_to_P_num_steps"::posint = 20],
<a name="line_11"></a> ["Field","P_to_H_poly_deg"::posint = 20],
<a name="line_12"></a> ["Field","P_to_H_num_charts"::posint = 10],
<a name="line_13"></a> ["Field","P_to_H_tolerance"::RR1 = 10.^(-20)],
<a name="line_14"></a> ["Field","P_to_H_gap"::RR1 = 0.02],
<a name="line_15"></a>
<a name="line_16"></a> ["Field","a_H_to_a_P_spline" = NULL],
<a name="line_17"></a>
<a name="line_18"></a> ["Constructor","",
<a name="line_19"></a>  proc(this)
<a name="line_20"></a>   this["H_to_P_maps"] := table();
<a name="line_21"></a>   this["P_to_H_maps"] := table();
<a name="line_22"></a>  end
<a name="line_23"></a> ],
<a name="line_24"></a>
<a name="line_25"></a> ["Method","a_H_indices","",
<a name="line_26"></a>  proc(this)
<a name="line_27"></a>   local T,L;
<a name="line_28"></a>   T := eval(this["H_to_P_maps"]);
<a name="line_29"></a>   L := sort(map(op,[indices(T)]));
<a name="line_30"></a>   L := remove(a -> (T[a] = NULL),L);
<a name="line_31"></a>   return L;
<a name="line_32"></a>  end
<a name="line_33"></a> ],
<a name="line_34"></a>
<a name="line_35"></a> ["Method","a_P_indices","",
<a name="line_36"></a>  proc(this)
<a name="line_37"></a>   local T,L;
<a name="line_38"></a>   T := eval(this["P_to_H_maps"]);
<a name="line_39"></a>   L := sort(map(op,[indices(T)]));
<a name="line_40"></a>   L := remove(a -> (T[a] = NULL),L);
<a name="line_41"></a>   return L;
<a name="line_42"></a>  end
<a name="line_43"></a> ],
<a name="line_44"></a>
<a name="line_45"></a> ["Method","a_H_a_P_pairs","",
<a name="line_46"></a>  proc(this)
<a name="line_47"></a>   local A_H;
<a name="line_48"></a>
<a name="line_49"></a>   A_H := this["a_H_indices"];
<a name="line_50"></a>   map(a -> [a,this["H_to_P_maps"][a]["a_P"]],A_H);
<a name="line_51"></a>  end
<a name="line_52"></a> ],
<a name="line_53"></a>
<a name="line_54"></a> ["Method","a_P_a_H_pairs","",
<a name="line_55"></a>  proc(this)
<a name="line_56"></a>   local A_P;
<a name="line_57"></a>
<a name="line_58"></a>   A_P := this["a_P_indices"];
<a name="line_59"></a>   map(a -> [a,this["P_to_H_maps"][a]["a_H"]],A_P);
<a name="line_60"></a>  end
<a name="line_61"></a> ],
<a name="line_62"></a>
<a name="line_63"></a> ["Method","set_spline","",
<a name="line_64"></a>  proc(this)
<a name="line_65"></a>   this["a_H_to_a_P_spline"] := 
<a name="line_66"></a>    unapply(CurveFitting[Spline]([[0,1],op(HP_table["a_H_a_P_pairs"]),[1,0]],a_H),a_H):
<a name="line_67"></a>  end
<a name="line_68"></a> ],
<a name="line_69"></a>
<a name="line_70"></a> ["Method","a_H_to_a_P","",
<a name="line_71"></a>  proc(this,a)
<a name="line_72"></a>   if this["a_H_to_a_P_spline"] = NULL then
<a name="line_73"></a>    this["set_spline"];
<a name="line_74"></a>   fi;
<a name="line_75"></a>   this["a_H_to_a_P_spline"](evalf(a));
<a name="line_76"></a>  end
<a name="line_77"></a> ],
<a name="line_78"></a>
<a name="line_79"></a> ["Method","a_P_to_a_H","",
<a name="line_80"></a>  proc(this,a)
<a name="line_81"></a>   local f,b;
<a name="line_82"></a>   if this["a_H_to_a_P_spline"] = NULL then
<a name="line_83"></a>    this["set_spline"];
<a name="line_84"></a>   fi;
<a name="line_85"></a>   f := eval(this["a_H_to_a_P_spline"]);
<a name="line_86"></a>   return fsolve(f(b) = a,b);
<a name="line_87"></a>  end
<a name="line_88"></a> ],
<a name="line_89"></a>
<a name="line_90"></a> ["Method","add_a_H","",
<a name="line_91"></a>  proc(this,a0)
<a name="line_92"></a>   local A_H,A_HL,A_HR,a_HL,a_HR,aL,aR,HP;
<a name="line_93"></a>
<a name="line_94"></a>   userinfo(7,genus2,sprintf("Adding entry with a_H=%A",a0));
<a name="line_95"></a>
<a name="line_96"></a>   A_H := this["a_H_indices"];
<a name="line_97"></a>   A_HL := select(a -> (a < a0),A_H);
<a name="line_98"></a>   A_HR := select(a -> (a > a0),A_H);
<a name="line_99"></a>
<a name="line_100"></a>   if member(a0,A_H) then
<a name="line_101"></a>    HP := eval(this["H_to_P_maps"][a0]);
<a name="line_102"></a>    HP["make_samples",this["H_to_P_num_samples"]];
<a name="line_103"></a>    HP["set_poly_deg",this["H_to_P_poly_deg"]];
<a name="line_104"></a>   else
<a name="line_105"></a>    HP := `new/H_to_P_map`():
<a name="line_106"></a>    HP["set_a_H",a0];
<a name="line_107"></a>    this["H_to_P_maps"][a0] := eval(HP);
<a name="line_108"></a>
<a name="line_109"></a>    HP["make_samples",this["H_to_P_num_samples"]];
<a name="line_110"></a>    HP["set_poly_deg",this["H_to_P_poly_deg"]];
<a name="line_111"></a>    if A_HL <> [] then
<a name="line_112"></a>     a_HL := max(A_HL);
<a name="line_113"></a>     if A_HR <> [] then
<a name="line_114"></a>      a_HR := min(A_HR);
<a name="line_115"></a>      userinfo(7,genus2,sprintf("Interpolating between a_H=%A and a_H=%A",a_HL,a_HR));
<a name="line_116"></a>      HP["a"] := this["H_to_P_maps"][a_HR]["a"];
<a name="line_117"></a>      HP["fix_a"];
<a name="line_118"></a>      aR := HP["a"];
<a name="line_119"></a>      HP["a"] := this["H_to_P_maps"][a_HL]["a"];
<a name="line_120"></a>      HP["fix_a"];
<a name="line_121"></a>      aL := HP["a"];
<a name="line_122"></a>      HP["a"] := ((a0 - a_HL) *~ aR +~
<a name="line_123"></a>                  (a_HR - a0) *~ aL) /~ (a_HR - a_HL);
<a name="line_124"></a>     else
<a name="line_125"></a>      userinfo(7,genus2,sprintf("Extrapolating from a_H=%A",a_HL));
<a name="line_126"></a>      HP["a"] := this["H_to_P_maps"][a_HL]["a"];
<a name="line_127"></a>      HP["fix_a"];
<a name="line_128"></a>     fi;
<a name="line_129"></a>    else
<a name="line_130"></a>     if A_HR <> [] then
<a name="line_131"></a>      a_HR := min(A_HR);
<a name="line_132"></a>      userinfo(7,genus2,sprintf("Extrapolating from a_H=%A",a_HR));
<a name="line_133"></a>      HP["a"] := this["H_to_P_maps"][a_HR]["a"];
<a name="line_134"></a>      HP["fix_a"];
<a name="line_135"></a>     fi;
<a name="line_136"></a>    fi;
<a name="line_137"></a>   fi;
<a name="line_138"></a>
<a name="line_139"></a>   HP["find_p1",this["H_to_P_num_steps"]];
<a name="line_140"></a>   HP["set_p1_inv"];
<a name="line_141"></a>   NULL;
<a name="line_142"></a>  end
<a name="line_143"></a> ],
<a name="line_144"></a>
<a name="line_145"></a> ["Method","remove_a_H","",
<a name="line_146"></a>  proc(this,a0)
<a name="line_147"></a>   this["H_to_P_maps"][a0] := NULL;
<a name="line_148"></a>  end
<a name="line_149"></a> ],
<a name="line_150"></a>
<a name="line_151"></a> ["Method","add_a_P","",
<a name="line_152"></a>  proc(this,a0)
<a name="line_153"></a>   local A_P,PH;
<a name="line_154"></a>
<a name="line_155"></a>   userinfo(7,genus2,sprintf("Adding entry with a_P=%A",a0));
<a name="line_156"></a>
<a name="line_157"></a>   A_P := this["a_P_indices"];
<a name="line_158"></a>
<a name="line_159"></a>   if member(a0,A_P) then
<a name="line_160"></a>    PH := eval(this["P_to_H_maps"][a0]);
<a name="line_161"></a>    PH["degree"] := this["P_to_H_poly_deg"];
<a name="line_162"></a>   else
<a name="line_163"></a>    PH := `new/P_to_H_map`():
<a name="line_164"></a>    PH["set_a_P",a0];
<a name="line_165"></a>   fi;
<a name="line_166"></a>
<a name="line_167"></a>   this["P_to_H_maps"][a0] := eval(PH);
<a name="line_168"></a>
<a name="line_169"></a>   PH["add_charts",this["P_to_H_num_charts"]];
<a name="line_170"></a>   PH["find_p1_inv",this["P_to_H_tolerance"],this["P_to_H_gap"]];   
<a name="line_171"></a>  end
<a name="line_172"></a> ],
<a name="line_173"></a>
<a name="line_174"></a> ["Method","remove_a_P","",
<a name="line_175"></a>  proc(this,a0)
<a name="line_176"></a>   this["P_to_H_maps"][a0] := NULL;
<a name="line_177"></a>  end
<a name="line_178"></a> ],
<a name="line_179"></a>
<a name="line_180"></a> ["Method","a_H_a_P_plot","",
<a name="line_181"></a>  proc(this)
<a name="line_182"></a>   local P;
<a name="line_183"></a>   P := this["a_H_a_P_pairs"];
<a name="line_184"></a>   if P = [] then
<a name="line_185"></a>    return NULL;
<a name="line_186"></a>   else
<a name="line_187"></a>    return 
<a name="line_188"></a>     display(
<a name="line_189"></a>      map(u -> point([u[1],u[2]],colour=blue),P),
<a name="line_190"></a>      view = [0..1,0..1]
<a name="line_191"></a>     );
<a name="line_192"></a>   fi;
<a name="line_193"></a>  end
<a name="line_194"></a> ],
<a name="line_195"></a>
<a name="line_196"></a> ["Method","a_P_a_H_plot","",
<a name="line_197"></a>  proc(this)
<a name="line_198"></a>   local P;
<a name="line_199"></a>   P := this["a_P_a_H_pairs"];
<a name="line_200"></a>   if P = [] then
<a name="line_201"></a>    return NULL;
<a name="line_202"></a>   else
<a name="line_203"></a>    return 
<a name="line_204"></a>     display(
<a name="line_205"></a>      map(u -> point([u[2],u[1]],colour=red),P),
<a name="line_206"></a>      view = [0..1,0..1]
<a name="line_207"></a>     );
<a name="line_208"></a>   fi;
<a name="line_209"></a>  end
<a name="line_210"></a> ],
<a name="line_211"></a>
<a name="line_212"></a> ["Method","spline_plot","",
<a name="line_213"></a>  proc(this)
<a name="line_214"></a>   display(
<a name="line_215"></a>    plot(this["a_H_to_a_P_spline"](a),a=0..1,colour=grey),
<a name="line_216"></a>    view = [0..1,0..1]
<a name="line_217"></a>   );
<a name="line_218"></a>  end
<a name="line_219"></a> ],
<a name="line_220"></a>
<a name="line_221"></a> ["Method","full_plot","",
<a name="line_222"></a>  proc(this)
<a name="line_223"></a>   display(
<a name="line_224"></a>    this["spline_plot"],
<a name="line_225"></a>    this["a_H_a_P_plot"],
<a name="line_226"></a>    this["a_P_a_H_plot"],
<a name="line_227"></a>    view = [0..1,0..1]
<a name="line_228"></a>   );
<a name="line_229"></a>  end
<a name="line_230"></a> ],
<a name="line_231"></a>
<a name="line_232"></a> ["Method","spline_plot_tikz","",
<a name="line_233"></a>  proc(this)
<a name="line_234"></a>   local s1,t1,p,pts,T;
<a name="line_235"></a>   T := eval(this["H_to_P_maps"]);
<a name="line_236"></a>
<a name="line_237"></a>   pts := [[0,1],
<a name="line_238"></a>	   seq([a,T[a]["a_P"]],
<a name="line_239"></a>	       a in sort(map(op,[indices(T)]))),
<a name="line_240"></a>	   [1,0]
<a name="line_241"></a>	  ];
<a name="line_242"></a>
<a name="line_243"></a>   s1 := sprintf(" \\draw[%s] plot[smooth] coordinates{ ","red");
<a name="line_244"></a>
<a name="line_245"></a>   for p in pts do 
<a name="line_246"></a>    s1 := cat(s1,sprintf("(%.3f,%.3f) ",op(1,p),op(2,p)));
<a name="line_247"></a>   od;
<a name="line_248"></a>   s1 := cat(s1,"};\n");
<a name="line_249"></a>
<a name="line_250"></a>   t1 := cat(
<a name="line_251"></a>     "\\begin{center}\n",
<a name="line_252"></a>     " \\begin{tikzpicture}[scale=4]\n",
<a name="line_253"></a>     "  \\draw[black,->] (-0.05,0) -- (1.05,0);\n",
<a name="line_254"></a>     "  \\draw[black,->] (0,-0.05) -- (0,1.05);\n",
<a name="line_255"></a>     "  \\draw[black] (1,-0.05) -- (1,0);\n",
<a name="line_256"></a>     "  \\draw[black] (-0.05,1) -- (0,1);\n",
<a name="line_257"></a>     "  \\draw ( 0.00,-0.05) node[anchor=north] {$0$};\n",
<a name="line_258"></a>     "  \\draw ( 1.00,-0.05) node[anchor=north] {$1$};\n",
<a name="line_259"></a>     "  \\draw (-0.05, 0.00) node[anchor=east ] {$0$};\n",
<a name="line_260"></a>     "  \\draw (-0.05, 1.00) node[anchor=east ] {$1$};\n",
<a name="line_261"></a>     "  \\draw ( 1.05, 0.00) node[anchor=west ] {$b$};\n",
<a name="line_262"></a>     "  \\draw ( 0.00, 1.05) node[anchor=south] {$a$};\n",
<a name="line_263"></a>     s1,
<a name="line_264"></a>     sprintf("  \\fill[black] (%.3f,%.3f) circle(0.015);\n",a_H0,a_P0),
<a name="line_265"></a>     " \\end{tikzpicture}\n",
<a name="line_266"></a>     "\\end{center}\n"
<a name="line_267"></a>   ):
<a name="line_268"></a>
<a name="line_269"></a>   return t1;
<a name="line_270"></a>  end
<a name="line_271"></a> ]
<a name="line_272"></a>):
  </pre>
 </body>
</html>
    