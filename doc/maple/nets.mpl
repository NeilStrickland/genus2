<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a><a name="CLASS_net"></a><span style="color:red">#@ CLASS: net
</span><a name="line_2"></a>
<a name="line_3"></a>`Class/Declare`("net",
<a name="line_4"></a> "An instance of this class represents a net which can be glued to produce a cromulent surface.  ",
<a name="line_5"></a>
<a name="line_6"></a> ["Constructor","",
<a name="line_7"></a>  proc(this)
<a name="line_8"></a>   this["v"] := table();
<a name="line_9"></a>   this["v_anchor"] := table():
<a name="line_10"></a>   this["squares"] := table():
<a name="line_11"></a>   this["square_centres"] := table():
<a name="line_12"></a>  end
<a name="line_13"></a> ],
<a name="line_14"></a>
<a name="line_15"></a> ["Field","name"::string,"A name for the net"],
<a name="line_16"></a>
<a name="line_17"></a> ["Field","v"::table,"This is a table of points in $\\mathbb{R}^2$.  The indices are integers in $\\{0,1,\\dotsc,13\\}$, or integers in that range plus a multiple of $0.1$.  The idea is that vertices indexed 10, 10.1, 10.2 and so on will all be identified together and will become the vertex $v_{10}$ in the glued surface"],
<a name="line_18"></a>
<a name="line_19"></a> ["Field","edges"::list([numeric,numeric]),"This is a list of pairs.  A pair @[i,j]@ appears in the list if @i@ and @j@ are indices in the @v@ table, and there is an edge from vertex @i@ to vertex @j@ in the net."],
<a name="line_20"></a>
<a name="line_21"></a> ["Field","outer_edges"::list([numeric,numeric]),"This is a list containing some of the entries in the @edges@ list, namely those that lie on the boundary of the net.  It can be set by the @set_outer_edges@ method."],
<a name="line_22"></a>
<a name="line_23"></a> ["Field","squares"::table,"This is a table indexed by the 16 elements of the group $G$.  Each entry is a list of four indices from the @v@ table.  If $g\\in G$ and $F$ is the fundamental domain, then the net will have a quadrilateral region corresponding to $g.F$, and the corners of that region are indexed by the elements of the $g$'th element in the @squares@ table.  These indices should be listed so that they correspond to $g.v_6$, $g.v_0$, $g.v_{11}$ and $g.v_3$ in that order."],
<a name="line_24"></a> 
<a name="line_25"></a> ["Field","square_centres"::table,"This is a table indexed by the 16 elements of the group $G$, withentries in $\\mathbb{R}^2$.  The $g$'th entry is the barycentre of the quadrilateral region in the net corresponding to $g.F$."],
<a name="line_26"></a>
<a name="line_27"></a> ["Field","tikz_scale"::numeric = 1,"This is the scale parameter that should be used when generating a tikzpicture environment illustrating this net."],
<a name="line_28"></a> 
<a name="line_29"></a> ["Field","v_anchor"::table,"This is a table with the same indices as the @v@ table.  The entries are strings like \"north\", \"south east\" and so on.  These entries indicate where the relevant vertex labels should be placed relative to the vertices themselves."],
<a name="line_30"></a>
<a name="line_31"></a> ["Method","set_edges_from_squares","If the @squares@ table has been filled, then this method can be used to fill the @edges@ list.",
<a name="line_32"></a>  proc(this)
<a name="line_33"></a>   local Q,E;
<a name="line_34"></a>
<a name="line_35"></a>   E := map(Q ->[[Q[1],Q[2]],[Q[2],Q[3]],[Q[3],Q[4]],[Q[4],Q[1]]],
<a name="line_36"></a>	    {seq(this["squares"][T],T in G16)});
<a name="line_37"></a>   E := sort(map(sort,map(op,E)));
<a name="line_38"></a>   this["edges"] := [op(E)];
<a name="line_39"></a>  end
<a name="line_40"></a> ],
<a name="line_41"></a>
<a name="line_42"></a> ["Method","set_square_centres","This fills the @square_centres@ table using information from the @v@ and @squares@ tables.",
<a name="line_43"></a>  proc(this)
<a name="line_44"></a>   local T,Q;
<a name="line_45"></a>
<a name="line_46"></a>   for T in G16 do 
<a name="line_47"></a>    Q := this["squares"][T];
<a name="line_48"></a>     this["square_centres"][T] := 
<a name="line_49"></a>     this["v"][Q[1]] /~ 4 +~
<a name="line_50"></a>     this["v"][Q[2]] /~ 4 +~
<a name="line_51"></a>     this["v"][Q[3]] /~ 4 +~
<a name="line_52"></a>     this["v"][Q[4]] /~ 4;
<a name="line_53"></a>   od:
<a name="line_54"></a>  end
<a name="line_55"></a> ],
<a name="line_56"></a>
<a name="line_57"></a> ["Method","set_outer_edges","This works out which edges lie on the boundary of the net.",
<a name="line_58"></a>  proc(this)
<a name="line_59"></a>   local E,S,N,T,Q,e;
<a name="line_60"></a>
<a name="line_61"></a>   E := this["edges"];
<a name="line_62"></a>   S := this["squares"];
<a name="line_63"></a>   N := table();
<a name="line_64"></a>   for e in E do N[e] := 0; od;
<a name="line_65"></a>   for T in G16 do
<a name="line_66"></a>    Q := S[T];
<a name="line_67"></a>    e := sort([Q[1],Q[2]]); N[e] := N[e] + 1;
<a name="line_68"></a>    e := sort([Q[2],Q[3]]); N[e] := N[e] + 1;
<a name="line_69"></a>    e := sort([Q[3],Q[4]]); N[e] := N[e] + 1;
<a name="line_70"></a>    e := sort([Q[4],Q[1]]); N[e] := N[e] + 1;
<a name="line_71"></a>   od;
<a name="line_72"></a>   this["outer_edges"] := select(e -> (N[e] = 1),E);
<a name="line_73"></a>  end
<a name="line_74"></a> ],
<a name="line_75"></a>
<a name="line_76"></a> ["Method","plot","This generates a Maple plot illustrating the net.",
<a name="line_77"></a>  proc(this)
<a name="line_78"></a>   local V;
<a name="line_79"></a>
<a name="line_80"></a>   V := eval(this["v"]);
<a name="line_81"></a>
<a name="line_82"></a>   display(map(e -> line(V[e[1]],V[e[2]],colour=edge_colour(e)),
<a name="line_83"></a>               this["edges"]),
<a name="line_84"></a>    seq(textplot([op(V[i]),i]),
<a name="line_85"></a>        i in map(op,[indices(V)])),
<a name="line_86"></a>    seq(textplot([op(this["square_centres"][T]),T]),T in G16),
<a name="line_87"></a>    axes = none
<a name="line_88"></a>   );
<a name="line_89"></a>  end
<a name="line_90"></a> ],
<a name="line_91"></a>
<a name="line_92"></a> ["Method","save_plot","This calls the @plot@ method and saves the result, both as en entry in the global variable @pics@ and as a file in the @plots@ directory",
<a name="line_93"></a>  proc(this)
<a name="line_94"></a>   global pics;
<a name="line_95"></a>   pics[this["name"]] := this["plot"];
<a name="line_96"></a>   save_plot(this["name"]);
<a name="line_97"></a>  end
<a name="line_98"></a> ],
<a name="line_99"></a>
<a name="line_100"></a> ["Method","outer_edge_plot","This generates a Maple plot showing the outer edges of the net.",
<a name="line_101"></a>  proc(this)
<a name="line_102"></a>   local V,W,E;
<a name="line_103"></a>
<a name="line_104"></a>   V := eval(this["v"]);
<a name="line_105"></a>   E := this["outer_edges"];
<a name="line_106"></a>   W := {op(map(op,E))};
<a name="line_107"></a>
<a name="line_108"></a>   display(map(e -> line(V[e[1]],V[e[2]],colour=edge_colour(e)),
<a name="line_109"></a>               this["outer_edges"]),
<a name="line_110"></a>    seq(textplot([op(V[i]),i]),i in W),
<a name="line_111"></a>    axes = none
<a name="line_112"></a>   );
<a name="line_113"></a>  end
<a name="line_114"></a> ],
<a name="line_115"></a>
<a name="line_116"></a> ["Method","tikz","This generates a tikzpicture environment illustrating the net.",
<a name="line_117"></a>  proc(this,scale_)
<a name="line_118"></a>   local scale,s,e,i,ii,aa,T,V,E;
<a name="line_119"></a>
<a name="line_120"></a>   V := eval(this["v"]);
<a name="line_121"></a>   E := eval(this["edges"]);
<a name="line_122"></a>
<a name="line_123"></a>   scale := `if`(nargs > 1, scale_, this["tikz_scale"]);
<a name="line_124"></a>
<a name="line_125"></a>   s := "% Generated by net_square_tikz()\n";
<a name="line_126"></a>   s := cat(s,sprintf("\\begin{tikzpicture}[scale=%A]\n",scale));
<a name="line_127"></a>   for e in E do
<a name="line_128"></a>    s := cat(s,sprintf(" \\draw[%7A] (%3d,%3d) -- (%3d,%3d);\n",
<a name="line_129"></a>                       edge_colour(e),op(V[e[1]]),op(V[e[2]])));
<a name="line_130"></a>   od:
<a name="line_131"></a>   for i in map(op,[indices(V)]) do
<a name="line_132"></a>    ii := `if`(0<i and i<1,sprintf("%1.1f",i),sprintf("%A",i));
<a name="line_133"></a>    aa := this["v_anchor"][i];
<a name="line_134"></a>    if type(aa,string) then
<a name="line_135"></a>     aa := cat("[anchor=",aa,"]");
<a name="line_136"></a>    else
<a name="line_137"></a>     aa := "";
<a name="line_138"></a>    fi;
<a name="line_139"></a>    s := cat(s,sprintf(" \\draw (%3d,%3d) node%s{$\\ss %s$};\n",op(V[i]),aa,ii));
<a name="line_140"></a>   od;
<a name="line_141"></a>   for T in G16 do
<a name="line_142"></a>    s := cat(s,sprintf(" \\draw (%3.2f,%3.2f) node{$%s$};\n",
<a name="line_143"></a>                       op(this["square_centres"][T]),G_latex[T]));
<a name="line_144"></a>   od;
<a name="line_145"></a>   s := cat(s,"\\end{tikzpicture}\n");
<a name="line_146"></a>   return(s);
<a name="line_147"></a>  end
<a name="line_148"></a> ],
<a name="line_149"></a>
<a name="line_150"></a> ["Method","save_tikz","This calls the @tikz@ method and saves the result",
<a name="line_151"></a>  proc(this)
<a name="line_152"></a>   save_tikz(this["name"],this["tikz"]);
<a name="line_153"></a>  end
<a name="line_154"></a> ],
<a name="line_155"></a>
<a name="line_156"></a> ["Method","check","This checks the combinatorial structure of the net.  It uses the global variable @edges@, which is set in @cromulent.mpl@",
<a name="line_157"></a>  proc(this)
<a name="line_158"></a>   local T;
<a name="line_159"></a>
<a name="line_160"></a>   _ASSERT(
<a name="line_161"></a>    sort([op({op(map(u -> map(floor,u),this["edges"]))})]) = edges,
<a name="line_162"></a>    sprintf("Net %s: correct edge set",this["name"])
<a name="line_163"></a>   );
<a name="line_164"></a>
<a name="line_165"></a>   for T in G16 do
<a name="line_166"></a>    _ASSERT(
<a name="line_167"></a>     map(act_V[T],[6,0,11,3]) = map(floor,this["squares"][T]),
<a name="line_168"></a>     sprintf("Net %s, square %A: correct vertices",this["name"],T)
<a name="line_169"></a>    );
<a name="line_170"></a>   od;
<a name="line_171"></a>  end
<a name="line_172"></a> ]
<a name="line_173"></a>);
<a name="line_174"></a>
<a name="line_175"></a>######################################################################
<a name="line_176"></a>
<a name="line_177"></a><span style="color:red">#@ net_0 
</span><a name="line_178"></a>net_0 := `new/net`();
<a name="line_179"></a>net_0["name"] := "net_0";
<a name="line_180"></a>
<a name="line_181"></a>net_0["v"] := table():
<a name="line_182"></a>
<a name="line_183"></a>net_0["v"][  0  ] := [  0,   0]:
<a name="line_184"></a>net_0["v"][  1  ] := [ 12,  12]:
<a name="line_185"></a>net_0["v"][  1.1] := [-12,  12]:
<a name="line_186"></a>net_0["v"][  1.2] := [-12, -12]:
<a name="line_187"></a>net_0["v"][  1.3] := [ 12, -12]:
<a name="line_188"></a>net_0["v"][  2  ] := [  4,   8]:
<a name="line_189"></a>net_0["v"][  2.1] := [  4,  -8]:
<a name="line_190"></a>net_0["v"][  3  ] := [  8,   4]:
<a name="line_191"></a>net_0["v"][  3.1] := [ -8,   4]:
<a name="line_192"></a>net_0["v"][  4  ] := [ -4,   8]:
<a name="line_193"></a>net_0["v"][  4.1] := [ -4,  -8]:
<a name="line_194"></a>net_0["v"][  5  ] := [  8,  -4]:
<a name="line_195"></a>net_0["v"][  5.1] := [ -8,  -4]:
<a name="line_196"></a>net_0["v"][  6  ] := [  6,   6]:
<a name="line_197"></a>net_0["v"][  7  ] := [ -6,   6]:
<a name="line_198"></a>net_0["v"][  8  ] := [ -6,  -6]:
<a name="line_199"></a>net_0["v"][  9  ] := [  6,  -6]:
<a name="line_200"></a>net_0["v"][ 10  ] := [  0,   8]:
<a name="line_201"></a>net_0["v"][ 10.1] := [  0,  -8]:
<a name="line_202"></a>net_0["v"][ 11  ] := [  8,   0]:
<a name="line_203"></a>net_0["v"][ 11.1] := [ -8,   0]:
<a name="line_204"></a>net_0["v"][ 12  ] := [  4,  12]:
<a name="line_205"></a>net_0["v"][ 12.1] := [ -4, -12]:
<a name="line_206"></a>net_0["v"][ 12.2] := [ -4,  12]:
<a name="line_207"></a>net_0["v"][ 12.3] := [  4, -12]:
<a name="line_208"></a>net_0["v"][ 13  ] := [ 12,   4]:
<a name="line_209"></a>net_0["v"][ 13.1] := [-12,  -4]:
<a name="line_210"></a>net_0["v"][ 13.2] := [ 12,  -4]:
<a name="line_211"></a>net_0["v"][ 13.3] := [-12,   4]:
<a name="line_212"></a>
<a name="line_213"></a>net_0["v_anchor"][ 0  ] := "north";
<a name="line_214"></a>net_0["v_anchor"][ 1  ] := "south west";
<a name="line_215"></a>net_0["v_anchor"][ 1.1] := "south east";
<a name="line_216"></a>net_0["v_anchor"][ 1.2] := "north east";
<a name="line_217"></a>net_0["v_anchor"][ 1.3] := "north west";
<a name="line_218"></a>net_0["v_anchor"][ 2  ] := "south east";
<a name="line_219"></a>net_0["v_anchor"][ 2.1] := "north east";
<a name="line_220"></a>net_0["v_anchor"][ 3.1] := "north east";
<a name="line_221"></a>net_0["v_anchor"][ 3  ] := "north west";
<a name="line_222"></a>net_0["v_anchor"][ 4.1] := "north west";
<a name="line_223"></a>net_0["v_anchor"][ 4  ] := "south west";
<a name="line_224"></a>net_0["v_anchor"][ 5  ] := "south west";
<a name="line_225"></a>net_0["v_anchor"][ 5.1] := "south east";
<a name="line_226"></a>net_0["v_anchor"][ 6  ] := "north";
<a name="line_227"></a>net_0["v_anchor"][ 7  ] := "north";
<a name="line_228"></a>net_0["v_anchor"][ 8  ] := "north";
<a name="line_229"></a>net_0["v_anchor"][ 9  ] := "north";
<a name="line_230"></a>net_0["v_anchor"][10  ] := "south";
<a name="line_231"></a>net_0["v_anchor"][10.1] := "north";
<a name="line_232"></a>net_0["v_anchor"][11  ] := "west";
<a name="line_233"></a>net_0["v_anchor"][11.1] := "east";
<a name="line_234"></a>net_0["v_anchor"][12.2] := "south west";
<a name="line_235"></a>net_0["v_anchor"][12.3] := "north east";
<a name="line_236"></a>net_0["v_anchor"][12  ] := "south east";
<a name="line_237"></a>net_0["v_anchor"][12.1] := "north west";
<a name="line_238"></a>net_0["v_anchor"][13  ] := "north west";
<a name="line_239"></a>net_0["v_anchor"][13.1] := "south east";
<a name="line_240"></a>net_0["v_anchor"][13.2] := "south west";
<a name="line_241"></a>net_0["v_anchor"][13.3] := "north east";
<a name="line_242"></a>
<a name="line_243"></a>net_0["squares"] := table([
<a name="line_244"></a>    1 = [6, 0  , 11  , 3  ],
<a name="line_245"></a>    L = [7, 0  , 10  , 4  ],
<a name="line_246"></a>   LL = [8, 0  , 11.1, 5.1],
<a name="line_247"></a>  LLL = [9, 0  , 10.1, 2.1],
<a name="line_248"></a>    M = [9, 1.3, 13.2, 5  ],
<a name="line_249"></a>   LM = [6, 1  , 12  , 2  ],
<a name="line_250"></a>  LLM = [7, 1.1, 13.3, 3.1],
<a name="line_251"></a> LLLM = [8, 1.2, 12.1, 4.1],
<a name="line_252"></a>    N = [9, 0  , 11  , 5  ],
<a name="line_253"></a>   LN = [6, 0  , 10  , 2  ],
<a name="line_254"></a>  LLN = [7, 0  , 11.1, 3.1],
<a name="line_255"></a> LLLN = [8, 0  , 10.1, 4.1],
<a name="line_256"></a>   MN = [6, 1  , 13  , 3  ],
<a name="line_257"></a>  LMN = [7, 1.1, 12.2, 4  ],
<a name="line_258"></a> LLMN = [8, 1.2, 13.1, 5.1],
<a name="line_259"></a>LLLMN = [9, 1.3, 12.3, 2.1]
<a name="line_260"></a>]):
<a name="line_261"></a>
<a name="line_262"></a>net_0["set_edges_from_squares"];
<a name="line_263"></a>net_0["tikz_scale"] := 0.5;
<a name="line_264"></a>net_0["set_outer_edges"];
<a name="line_265"></a>net_0["set_square_centres"];
<a name="line_266"></a>
<a name="line_267"></a>######################################################################
<a name="line_268"></a>
<a name="line_269"></a><span style="color:red">#@ net_1 
</span><a name="line_270"></a>net_1 := `new/net`();
<a name="line_271"></a>net_1["name"] := "net_1";
<a name="line_272"></a>net_1["v"] := table():
<a name="line_273"></a>
<a name="line_274"></a>net_1["v"][  0  ] := [  4,   4]:
<a name="line_275"></a>net_1["v"][  0.1] := [ -4,   4]:
<a name="line_276"></a>net_1["v"][  0.2] := [ -4,  -4]:
<a name="line_277"></a>net_1["v"][  0.3] := [  4,  -4]:
<a name="line_278"></a>net_1["v"][  1  ] := [  2,   2]:
<a name="line_279"></a>net_1["v"][  1.1] := [ -2,   2]:
<a name="line_280"></a>net_1["v"][  1.2] := [ -2,  -2]:
<a name="line_281"></a>net_1["v"][  1.3] := [  2,  -2]:
<a name="line_282"></a>net_1["v"][  2  ] := [  0,   3]:
<a name="line_283"></a>net_1["v"][  3  ] := [ -3,   0]:
<a name="line_284"></a>net_1["v"][  4  ] := [  0,  -3]:
<a name="line_285"></a>net_1["v"][  5  ] := [  3,   0]:
<a name="line_286"></a>net_1["v"][  6  ] := [ -3,   3]:
<a name="line_287"></a>net_1["v"][  7  ] := [ -3,  -3]:
<a name="line_288"></a>net_1["v"][  8  ] := [  3,  -3]:
<a name="line_289"></a>net_1["v"][  9  ] := [  3,   3]:
<a name="line_290"></a>net_1["v"][ 10  ] := [  0,   4]:
<a name="line_291"></a>net_1["v"][ 10.1] := [  0,  -4]:
<a name="line_292"></a>net_1["v"][ 11  ] := [  4,   0]:
<a name="line_293"></a>net_1["v"][ 11.1] := [ -4,   0]:
<a name="line_294"></a>net_1["v"][ 12  ] := [  0,   2]:
<a name="line_295"></a>net_1["v"][ 12.1] := [  0,  -2]:
<a name="line_296"></a>net_1["v"][ 13  ] := [  2,   0]:
<a name="line_297"></a>net_1["v"][ 13.1] := [ -2,   0]:
<a name="line_298"></a>
<a name="line_299"></a>net_1["v_anchor"][  0  ] := "south west";
<a name="line_300"></a>net_1["v_anchor"][  0.1] := "south east";
<a name="line_301"></a>net_1["v_anchor"][  0.2] := "north east";
<a name="line_302"></a>net_1["v_anchor"][  0.3] := "north west";
<a name="line_303"></a>net_1["v_anchor"][  1  ] := "north east";
<a name="line_304"></a>net_1["v_anchor"][  1.1] := "north west";
<a name="line_305"></a>net_1["v_anchor"][  1.2] := "south west";
<a name="line_306"></a>net_1["v_anchor"][  1.3] := "south east";
<a name="line_307"></a>net_1["v_anchor"][ 10  ] := "south";
<a name="line_308"></a>net_1["v_anchor"][ 10.1] := "north";
<a name="line_309"></a>net_1["v_anchor"][ 11  ] := "west";
<a name="line_310"></a>net_1["v_anchor"][ 11.1] := "east";
<a name="line_311"></a>net_1["v_anchor"][ 12  ] := "north";
<a name="line_312"></a>net_1["v_anchor"][ 12.1] := "south";
<a name="line_313"></a>net_1["v_anchor"][ 13  ] := "east";
<a name="line_314"></a>net_1["v_anchor"][ 13.1] := "west";
<a name="line_315"></a>
<a name="line_316"></a>net_1["squares"] := table([
<a name="line_317"></a>    1 = [6,0.1,11.1,3],
<a name="line_318"></a>    L = [7,0.2,10.1,4],
<a name="line_319"></a>   LL = [8,0.3,11  ,5],
<a name="line_320"></a>  LLL = [9,0  ,10  ,2],
<a name="line_321"></a>    M = [9,1  ,13  ,5],
<a name="line_322"></a>   LM = [6,1.1,12  ,2],
<a name="line_323"></a>  LLM = [7,1.2,13.1,3],
<a name="line_324"></a> LLLM = [8,1.3,12.1,4],
<a name="line_325"></a>    N = [9,0  ,11  ,5],
<a name="line_326"></a>   LN = [6,0.1,10  ,2],
<a name="line_327"></a>  LLN = [7,0.2,11.1,3],
<a name="line_328"></a> LLLN = [8,0.3,10.1,4],
<a name="line_329"></a>   MN = [6,1.1,13.1,3],
<a name="line_330"></a>  LMN = [7,1.2,12.1,4],
<a name="line_331"></a> LLMN = [8,1.3,13  ,5],
<a name="line_332"></a>LLLMN = [9,1  ,12  ,2]
<a name="line_333"></a>]):
<a name="line_334"></a>
<a name="line_335"></a>net_1["set_edges_from_squares"];
<a name="line_336"></a>net_1["set_outer_edges"];
<a name="line_337"></a>net_1["set_square_centres"];
<a name="line_338"></a>
<a name="line_339"></a>######################################################################
<a name="line_340"></a>
<a name="line_341"></a><span style="color:red">#@ net_2 
</span><a name="line_342"></a>net_2 := `new/net`();
<a name="line_343"></a>net_2["name"] := "net_2";
<a name="line_344"></a>net_2["v"] := table():
<a name="line_345"></a>
<a name="line_346"></a>net_2["v"][  0  ] := [ -4,   0]:
<a name="line_347"></a>net_2["v"][  0.1] := [ -4,   8]:
<a name="line_348"></a>net_2["v"][  0.2] := [ -4,  -8]:
<a name="line_349"></a>net_2["v"][  1  ] := [  4,   0]:
<a name="line_350"></a>net_2["v"][  1.1] := [  4,   8]:
<a name="line_351"></a>net_2["v"][  1.2] := [  4,  -8]:
<a name="line_352"></a>net_2["v"][  2  ] := [  0,   4]:
<a name="line_353"></a>net_2["v"][  3  ] := [  0,  -4]:
<a name="line_354"></a>net_2["v"][  4  ] := [  8,   4]:
<a name="line_355"></a>net_2["v"][  4.1] := [ -8,   4]:
<a name="line_356"></a>net_2["v"][  5  ] := [  8,  -4]:
<a name="line_357"></a>net_2["v"][  5.1] := [ -8,  -4]:
<a name="line_358"></a>net_2["v"][  6  ] := [  0,   0]:
<a name="line_359"></a>net_2["v"][  7  ] := [  8,   1]:
<a name="line_360"></a>net_2["v"][  7.1] := [ -8,   1]:
<a name="line_361"></a>net_2["v"][  7.2] := [  0,  -8]:
<a name="line_362"></a>net_2["v"][  8  ] := [  8,   8]:
<a name="line_363"></a>net_2["v"][  8.1] := [ -8,   8]:
<a name="line_364"></a>net_2["v"][  8.2] := [ -8,  -8]:
<a name="line_365"></a>net_2["v"][  8.3] := [  8,  -8]:
<a name="line_366"></a>net_2["v"][  9  ] := [  8,  -1]:
<a name="line_367"></a>net_2["v"][  9.1] := [ -8,  -1]:
<a name="line_368"></a>net_2["v"][  9.2] := [  0,   8]:
<a name="line_369"></a>net_2["v"][ 10  ] := [ -4,   4]:
<a name="line_370"></a>net_2["v"][ 11  ] := [ -4,  -4]:
<a name="line_371"></a>net_2["v"][ 12  ] := [  4,   4]:
<a name="line_372"></a>net_2["v"][ 13  ] := [  4,  -4]:
<a name="line_373"></a>
<a name="line_374"></a>net_2["v_anchor"][  0  ] := "east";
<a name="line_375"></a>net_2["v_anchor"][  0.1] := "south";
<a name="line_376"></a>net_2["v_anchor"][  0.2] := "north";
<a name="line_377"></a>net_2["v_anchor"][  1  ] := "west";
<a name="line_378"></a>net_2["v_anchor"][  1.1] := "south";
<a name="line_379"></a>net_2["v_anchor"][  1.2] := "north";
<a name="line_380"></a>net_2["v_anchor"][  4  ] := "west";
<a name="line_381"></a>net_2["v_anchor"][  4.1] := "east";
<a name="line_382"></a>net_2["v_anchor"][  5  ] := "west";
<a name="line_383"></a>net_2["v_anchor"][  5.1] := "east";
<a name="line_384"></a>net_2["v_anchor"][  7  ] := "west";
<a name="line_385"></a>net_2["v_anchor"][  7.1] := "east";
<a name="line_386"></a>net_2["v_anchor"][  7.2] := "north";
<a name="line_387"></a>net_2["v_anchor"][  8  ] := "south west";
<a name="line_388"></a>net_2["v_anchor"][  8.1] := "south east";
<a name="line_389"></a>net_2["v_anchor"][  8.2] := "north east";
<a name="line_390"></a>net_2["v_anchor"][  8.3] := "north west";
<a name="line_391"></a>net_2["v_anchor"][  9  ] := "west";
<a name="line_392"></a>net_2["v_anchor"][  9.1] := "east";
<a name="line_393"></a>net_2["v_anchor"][  9.2] := "south";
<a name="line_394"></a>
<a name="line_395"></a>net_2["squares"] := table([
<a name="line_396"></a>    1 = [ 6  ,0  , 11  , 3  ], 
<a name="line_397"></a>    L = [ 7.1,0  , 10  , 4.1], 
<a name="line_398"></a>   LL = [ 8.2,0.2, 11  , 5.1], 
<a name="line_399"></a>  LLL = [ 9.2,0.1, 10  , 2  ], 
<a name="line_400"></a>    M = [ 9  ,1  , 13  , 5  ], 
<a name="line_401"></a>   LM = [ 6  ,1  , 12  , 2  ], 
<a name="line_402"></a>  LLM = [ 7.2,1.2, 13  , 3  ], 
<a name="line_403"></a> LLLM = [ 8  ,1.1, 12  , 4  ], 
<a name="line_404"></a>    N = [ 9.1,0  , 11  , 5.1], 
<a name="line_405"></a>   LN = [ 6  ,0  , 10  , 2  ], 
<a name="line_406"></a>  LLN = [ 7.2,0.2, 11  , 3  ], 
<a name="line_407"></a> LLLN = [ 8.1,0.1, 10  , 4.1], 
<a name="line_408"></a>   MN = [ 6  ,1  , 13  , 3  ], 
<a name="line_409"></a>  LMN = [ 7  ,1  , 12  , 4  ], 
<a name="line_410"></a> LLMN = [ 8.3,1.2, 13  , 5  ], 
<a name="line_411"></a>LLLMN = [ 9.2,1.1, 12  , 2  ]
<a name="line_412"></a>]):
<a name="line_413"></a>
<a name="line_414"></a>net_2["tikz_scale"] := 0.5;
<a name="line_415"></a>net_2["set_edges_from_squares"];
<a name="line_416"></a>net_2["set_outer_edges"];
<a name="line_417"></a>net_2["set_square_centres"];
<a name="line_418"></a>
<a name="line_419"></a>######################################################################
<a name="line_420"></a>
<a name="line_421"></a><span style="color:red">#@ net_3 
</span><a name="line_422"></a>net_3 := `new/net`();
<a name="line_423"></a>net_3["name"] := "net_3";
<a name="line_424"></a>net_3["v"] := table():
<a name="line_425"></a>
<a name="line_426"></a>net_3["v"][  0  ] := [  8,   4]:
<a name="line_427"></a>net_3["v"][  0.1] := [ -8,   4]:
<a name="line_428"></a>net_3["v"][  0.2] := [ -8,  -4]:
<a name="line_429"></a>net_3["v"][  0.3] := [  8,  -4]:
<a name="line_430"></a>net_3["v"][  1  ] := [  0,   4]:
<a name="line_431"></a>net_3["v"][  1.1] := [  0,  -4]:
<a name="line_432"></a>net_3["v"][  2  ] := [  4,   8]:
<a name="line_433"></a>net_3["v"][  2.1] := [ -4,   8]:
<a name="line_434"></a>net_3["v"][  3  ] := [ -4,   0]:
<a name="line_435"></a>net_3["v"][  4  ] := [  4,  -8]:
<a name="line_436"></a>net_3["v"][  4.1] := [ -4,  -8]:
<a name="line_437"></a>net_3["v"][  5  ] := [  4,   0]:
<a name="line_438"></a>net_3["v"][  6  ] := [ -4,   4]:
<a name="line_439"></a>net_3["v"][  7  ] := [ -4,  -4]:
<a name="line_440"></a>net_3["v"][  8  ] := [  4,  -4]:
<a name="line_441"></a>net_3["v"][  9  ] := [  4,   4]:
<a name="line_442"></a>net_3["v"][ 10  ] := [  8,   8]:
<a name="line_443"></a>net_3["v"][ 10.1] := [ -8,   8]:
<a name="line_444"></a>net_3["v"][ 10.2] := [ -8,  -8]:
<a name="line_445"></a>net_3["v"][ 10.3] := [  8,  -8]:
<a name="line_446"></a>net_3["v"][ 11  ] := [  8,   0]:
<a name="line_447"></a>net_3["v"][ 11.1] := [ -8,   0]:
<a name="line_448"></a>net_3["v"][ 12  ] := [  1,   8]:
<a name="line_449"></a>net_3["v"][ 12.1] := [ -1,   8]:
<a name="line_450"></a>net_3["v"][ 12.2] := [ -1,  -8]:
<a name="line_451"></a>net_3["v"][ 12.3] := [  1,  -8]:
<a name="line_452"></a>net_3["v"][ 13  ] := [  0,   0]:
<a name="line_453"></a>
<a name="line_454"></a>net_3["v_anchor"][  0  ] := "west";
<a name="line_455"></a>net_3["v_anchor"][  0.1] := "east";
<a name="line_456"></a>net_3["v_anchor"][  0.2] := "east";
<a name="line_457"></a>net_3["v_anchor"][  0.3] := "west";
<a name="line_458"></a>net_3["v_anchor"][  1  ] := "south";
<a name="line_459"></a>net_3["v_anchor"][  1.1] := "north";
<a name="line_460"></a>net_3["v_anchor"][  2  ] := "south";
<a name="line_461"></a>net_3["v_anchor"][  2.1] := "south";
<a name="line_462"></a>net_3["v_anchor"][  4  ] := "north";
<a name="line_463"></a>net_3["v_anchor"][  4.1] := "north";
<a name="line_464"></a>net_3["v_anchor"][ 10  ] := "south west";
<a name="line_465"></a>net_3["v_anchor"][ 10.1] := "south east";
<a name="line_466"></a>net_3["v_anchor"][ 10.2] := "north east";
<a name="line_467"></a>net_3["v_anchor"][ 10.3] := "north west";
<a name="line_468"></a>net_3["v_anchor"][ 11  ] := "west";
<a name="line_469"></a>net_3["v_anchor"][ 11.1] := "east";
<a name="line_470"></a>net_3["v_anchor"][ 12  ] := "south";
<a name="line_471"></a>net_3["v_anchor"][ 12.1] := "south";
<a name="line_472"></a>net_3["v_anchor"][ 12.2] := "north";
<a name="line_473"></a>net_3["v_anchor"][ 12.3] := "north";
<a name="line_474"></a>
<a name="line_475"></a>net_3["squares"] := table([
<a name="line_476"></a>    1 = [ 6  ,0.1, 11.1, 3  ], 
<a name="line_477"></a>    L = [ 7  ,0.2, 10.2, 4.1], 
<a name="line_478"></a>   LL = [ 8  ,0.3, 11  , 5  ], 
<a name="line_479"></a>  LLL = [ 9  ,0  , 10  , 2  ], 
<a name="line_480"></a>    M = [ 9  ,1  , 13  , 5  ], 
<a name="line_481"></a>   LM = [ 6  ,1  , 12.1, 2.1], 
<a name="line_482"></a>  LLM = [ 7  ,1.1, 13  , 3  ], 
<a name="line_483"></a> LLLM = [ 8  ,1.1, 12.3, 4  ], 
<a name="line_484"></a>    N = [ 9  ,0  , 11  , 5  ], 
<a name="line_485"></a>   LN = [ 6  ,0.1, 10.1, 2.1], 
<a name="line_486"></a>  LLN = [ 7  ,0.2, 11.1, 3  ], 
<a name="line_487"></a> LLLN = [ 8  ,0.3, 10.3, 4  ], 
<a name="line_488"></a>   MN = [ 6  ,1  , 13  , 3  ], 
<a name="line_489"></a>  LMN = [ 7  ,1.1, 12.2, 4.1], 
<a name="line_490"></a> LLMN = [ 8  ,1.1, 13  , 5  ], 
<a name="line_491"></a>LLLMN = [ 9  ,1  , 12  , 2  ]
<a name="line_492"></a>]):
<a name="line_493"></a>
<a name="line_494"></a>net_3["tikz_scale"] := 0.5;
<a name="line_495"></a>net_3["set_edges_from_squares"];
<a name="line_496"></a>net_3["set_outer_edges"];
<a name="line_497"></a>net_3["set_square_centres"];
<a name="line_498"></a>
<a name="line_499"></a>######################################################################
<a name="line_500"></a>
<a name="line_501"></a><span style="color:red">#@ net_5 
</span><a name="line_502"></a>net_5 := `new/net`();
<a name="line_503"></a>net_5["name"] := "net_5";
<a name="line_504"></a>net_5["v"] := table():
<a name="line_505"></a>
<a name="line_506"></a>net_5["v"][  0  ] := [  3,   2]:
<a name="line_507"></a>net_5["v"][  0.1] := [  2,   6]:
<a name="line_508"></a>net_5["v"][  0.2] := [ -2,   3]:
<a name="line_509"></a>net_5["v"][  0.3] := [ -6,   2]:
<a name="line_510"></a>net_5["v"][  0.4] := [ -3,  -2]:
<a name="line_511"></a>net_5["v"][  0.5] := [ -2,  -6]:
<a name="line_512"></a>net_5["v"][  0.6] := [  2,  -3]:
<a name="line_513"></a>net_5["v"][  0.7] := [  6,  -2]:
<a name="line_514"></a>net_5["v"][  1  ] := [  0,   0]:
<a name="line_515"></a>net_5["v"][  2  ] := [  3,   1]:
<a name="line_516"></a>net_5["v"][  2.1] := [ -3,   1]:
<a name="line_517"></a>net_5["v"][  3  ] := [  1,  -3]:
<a name="line_518"></a>net_5["v"][  3.1] := [  1,   3]:
<a name="line_519"></a>net_5["v"][  4  ] := [ -3,  -1]:
<a name="line_520"></a>net_5["v"][  4.1] := [  3,  -1]:
<a name="line_521"></a>net_5["v"][  5  ] := [ -1,   3]:
<a name="line_522"></a>net_5["v"][  5.1] := [ -1,  -3]:
<a name="line_523"></a>net_5["v"][  6  ] := [  2,   2]:
<a name="line_524"></a>net_5["v"][  6.1] := [ -6,   1]:
<a name="line_525"></a>net_5["v"][  7  ] := [  2,  -2]:
<a name="line_526"></a>net_5["v"][  7.1] := [  1,   6]:
<a name="line_527"></a>net_5["v"][  8  ] := [ -2,  -2]:
<a name="line_528"></a>net_5["v"][  8.1] := [  6,  -1]:
<a name="line_529"></a>net_5["v"][  9  ] := [ -2,   2]:
<a name="line_530"></a>net_5["v"][  9.1] := [ -1,  -6]:
<a name="line_531"></a>net_5["v"][ 10  ] := [  5,  -3]:
<a name="line_532"></a>net_5["v"][ 10.1] := [ -5,   3]:
<a name="line_533"></a>net_5["v"][ 11  ] := [  3,   5]:
<a name="line_534"></a>net_5["v"][ 11.1] := [ -3,  -5]:
<a name="line_535"></a>net_5["v"][ 12  ] := [  3,   0]:
<a name="line_536"></a>net_5["v"][ 12.1] := [ -3,   0]:
<a name="line_537"></a>net_5["v"][ 13  ] := [  0,   3]:
<a name="line_538"></a>net_5["v"][ 13.1] := [  0,  -3]:
<a name="line_539"></a>
<a name="line_540"></a>net_5["v_anchor"][  0  ] := "west":
<a name="line_541"></a>net_5["v_anchor"][  0.1] := "south west":
<a name="line_542"></a>net_5["v_anchor"][  0.2] := "south":
<a name="line_543"></a>net_5["v_anchor"][  0.3] := "south east":
<a name="line_544"></a>net_5["v_anchor"][  0.4] := "east":
<a name="line_545"></a>net_5["v_anchor"][  0.5] := "north east":
<a name="line_546"></a>net_5["v_anchor"][  0.6] := "north":
<a name="line_547"></a>net_5["v_anchor"][  0.7] := "north west":
<a name="line_548"></a>net_5["v_anchor"][  1  ] := "east":
<a name="line_549"></a>net_5["v_anchor"][  2  ] := "west":
<a name="line_550"></a>net_5["v_anchor"][  2.1] := "north east":
<a name="line_551"></a>net_5["v_anchor"][  3  ] := "north":
<a name="line_552"></a>net_5["v_anchor"][  3.1] := "south east":
<a name="line_553"></a>net_5["v_anchor"][  4  ] := "east":
<a name="line_554"></a>net_5["v_anchor"][  4.1] := "south west":
<a name="line_555"></a>net_5["v_anchor"][  5  ] := "south":
<a name="line_556"></a>net_5["v_anchor"][  5.1] := "north west":
<a name="line_557"></a>net_5["v_anchor"][  6  ] := "south":
<a name="line_558"></a>net_5["v_anchor"][  6.1] := "north":
<a name="line_559"></a>net_5["v_anchor"][  7  ] := "west":
<a name="line_560"></a>net_5["v_anchor"][  7.1] := "east":
<a name="line_561"></a>net_5["v_anchor"][  8  ] := "north":
<a name="line_562"></a>net_5["v_anchor"][  8.1] := "south":
<a name="line_563"></a>net_5["v_anchor"][  9  ] := "east":
<a name="line_564"></a>net_5["v_anchor"][  9.1] := "west":
<a name="line_565"></a>net_5["v_anchor"][ 10  ] := "north":
<a name="line_566"></a>net_5["v_anchor"][ 10.1] := "south":
<a name="line_567"></a>net_5["v_anchor"][ 11  ] := "west":
<a name="line_568"></a>net_5["v_anchor"][ 11.1] := "east":
<a name="line_569"></a>net_5["v_anchor"][ 12  ] := "west":
<a name="line_570"></a>net_5["v_anchor"][ 12.1] := "east":
<a name="line_571"></a>net_5["v_anchor"][ 13  ] := "south":
<a name="line_572"></a>net_5["v_anchor"][ 13.1] := "north":
<a name="line_573"></a>
<a name="line_574"></a>net_5["squares"] := table([
<a name="line_575"></a>    1 = [ 6  ,0  , 11  , 3.1], 
<a name="line_576"></a>    L = [ 7  ,0.6, 10  , 4.1], 
<a name="line_577"></a>   LL = [ 8  ,0.4, 11.1, 5.1], 
<a name="line_578"></a>  LLL = [ 9  ,0.2, 10.1, 2.1], 
<a name="line_579"></a>    M = [ 9  ,1  , 13  , 5  ], 
<a name="line_580"></a>   LM = [ 6  ,1  , 12  , 2  ], 
<a name="line_581"></a>  LLM = [ 7  ,1  , 13.1, 3  ], 
<a name="line_582"></a> LLLM = [ 8  ,1  , 12.1, 4  ], 
<a name="line_583"></a>    N = [ 9.1,0.5, 11.1, 5.1], 
<a name="line_584"></a>   LN = [ 6.1,0.3, 10.1, 2.1], 
<a name="line_585"></a>  LLN = [ 7.1,0.1, 11  , 3.1], 
<a name="line_586"></a> LLLN = [ 8.1,0.7, 10  , 4.1], 
<a name="line_587"></a>   MN = [ 6  ,1  , 13  , 3.1], 
<a name="line_588"></a>  LMN = [ 7  ,1  , 12  , 4.1], 
<a name="line_589"></a> LLMN = [ 8  ,1  , 13.1, 5.1], 
<a name="line_590"></a>LLLMN = [ 9  ,1  , 12.1, 2.1]
<a name="line_591"></a>]):
<a name="line_592"></a>
<a name="line_593"></a>net_5["set_edges_from_squares"];
<a name="line_594"></a>net_5["set_outer_edges"];
<a name="line_595"></a>net_5["set_square_centres"];
<a name="line_596"></a>
<a name="line_597"></a>
  </pre>
 </body>
</html>
    