<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># This file defines Maple functions to generate various chunks of LaTeX to be
<a name="line_2"></a># included in the main document for the project.
<a name="line_3"></a>
<a name="line_4"></a>######################################################################
<a name="line_5"></a>
<a name="line_6"></a># LaTeX names for elements of the group G.
<a name="line_7"></a>
<a name="line_8"></a><span style="color:red">#@ G_latex
</span><a name="line_9"></a>G_latex[1]     := "1";
<a name="line_10"></a>G_latex[L]     := "\\lm";
<a name="line_11"></a>G_latex[LL]    := "\\lm^2";
<a name="line_12"></a>G_latex[LLL]   := "\\lm^3";
<a name="line_13"></a>G_latex[M]     := "\\mu";
<a name="line_14"></a>G_latex[LM]    := "\\lm\\mu";
<a name="line_15"></a>G_latex[LLM]   := "\\lm^2\\mu";
<a name="line_16"></a>G_latex[LLLM]  := "\\lm^3\\mu";
<a name="line_17"></a>G_latex[N]     := "\\nu";
<a name="line_18"></a>G_latex[LN]    := "\\lm\\nu";
<a name="line_19"></a>G_latex[LLN]   := "\\lm^2\\nu";
<a name="line_20"></a>G_latex[LLLN]  := "\\lm^3\\nu";
<a name="line_21"></a>G_latex[MN]    := "\\mu\\nu";
<a name="line_22"></a>G_latex[LMN]   := "\\lm\\mu\\nu";
<a name="line_23"></a>G_latex[LLMN]  := "\\lm^2\\mu\\nu";
<a name="line_24"></a>G_latex[LLLMN] := "\\lm^3\\mu\\nu";
<a name="line_25"></a>
<a name="line_26"></a>######################################################################
<a name="line_27"></a>
<a name="line_28"></a># Latex for common angles
<a name="line_29"></a>
<a name="line_30"></a><span style="color:red">#@ angle_latex
</span><a name="line_31"></a>angle_latex[0]    := "0";
<a name="line_32"></a>angle_latex[Pi]   := "\\pi";
<a name="line_33"></a>angle_latex[2*Pi] := "2\\pi";
<a name="line_34"></a>angle_latex[Pi/4] := "\\qpi";
<a name="line_35"></a>angle_latex[Pi/2] := "\\ppi";
<a name="line_36"></a>
<a name="line_37"></a>######################################################################
<a name="line_38"></a>
<a name="line_39"></a># Both tikz and Maple have the ability to place a text label on a
<a name="line_40"></a># picture and place it intelligently offset from the point being 
<a name="line_41"></a># labelled.  This table helps translate between the tikz syntax and
<a name="line_42"></a># the Maple syntax.
<a name="line_43"></a>
<a name="line_44"></a><span style="color:red">#@ tikz_anchor
</span><a name="line_45"></a>tikz_anchor['above'] := "south";
<a name="line_46"></a>tikz_anchor['below'] := "north";
<a name="line_47"></a>tikz_anchor['right'] := "west";
<a name="line_48"></a>tikz_anchor['left']  := "east";
<a name="line_49"></a>tikz_anchor[{'above','left' }] := "south east";
<a name="line_50"></a>tikz_anchor[{'above','right'}] := "south west";
<a name="line_51"></a>tikz_anchor[{'below','left' }] := "north east";
<a name="line_52"></a>tikz_anchor[{'below','right'}] := "north west";
<a name="line_53"></a>
<a name="line_54"></a>######################################################################
<a name="line_55"></a>
<a name="line_56"></a># Generate a tikz plotting command using syntax parallel to that for
<a name="line_57"></a># Maple plotting commands.
<a name="line_58"></a>
<a name="line_59"></a><span style="color:red">#@ tikz_plot 
</span><a name="line_60"></a>tikz_plot := proc(xy,t_range,num_points,col)
<a name="line_61"></a> local t0,t1,tt,pts,s,p;
<a name="line_62"></a> t0 := op(1,t_range);
<a name="line_63"></a> t1 := op(2,t_range);
<a name="line_64"></a> tt := seq(t0 + i/num_points*(t1 - t0),i=0..num_points);
<a name="line_65"></a> if type(xy,procedure) then
<a name="line_66"></a>  pts := evalf(map(xy,[tt]));
<a name="line_67"></a> else
<a name="line_68"></a>  pts := [seq(evalf(subs(t=s,xy)),s in tt)];
<a name="line_69"></a> fi;
<a name="line_70"></a>
<a name="line_71"></a> s := sprintf(" \\draw[%s] plot[smooth] coordinates{ ",col);
<a name="line_72"></a> for p in pts do 
<a name="line_73"></a>  s := cat(s,sprintf("(%.3f,%.3f) ",op(p)));
<a name="line_74"></a> od;
<a name="line_75"></a> s := cat(s,"};\n");
<a name="line_76"></a> return(s);
<a name="line_77"></a>end:
<a name="line_78"></a>
<a name="line_79"></a>######################################################################
<a name="line_80"></a># Generate tikz code for a point, using syntax similar to Maple
<a name="line_81"></a>
<a name="line_82"></a><span style="color:red">#@ tikz_point 
</span><a name="line_83"></a>tikz_point := proc(xy,sz) 
<a name="line_84"></a> sprintf(" \\fill (%.3f,%.3f) circle(%.3f);\n",op(evalf(xy)),sz);
<a name="line_85"></a>end;
<a name="line_86"></a>
<a name="line_87"></a>########################################
<a name="line_88"></a># Generate tikz code for a line, using syntax similar to Maple
<a name="line_89"></a>
<a name="line_90"></a><span style="color:red">#@ tikz_line 
</span><a name="line_91"></a>tikz_line := proc(xy1,xy2,col_)
<a name="line_92"></a> local s,d;
<a name="line_93"></a>
<a name="line_94"></a> s := " \\draw";
<a name="line_95"></a> if nargs > 2 then 
<a name="line_96"></a>  s := cat(s,"[",col_,"]");
<a name="line_97"></a> fi;
<a name="line_98"></a> 
<a name="line_99"></a> s := cat(s,sprintf(" (%.3f,%.3f) -- (%.3f,%.3f);\n",op(evalf(xy1)),op(evalf(xy2))));
<a name="line_100"></a>
<a name="line_101"></a> return s;
<a name="line_102"></a>end:
<a name="line_103"></a>
<a name="line_104"></a>########################################
<a name="line_105"></a># Generate tikz code for a curve, using syntax similar to Maple
<a name="line_106"></a>
<a name="line_107"></a><span style="color:red">#@ tikz_curve 
</span><a name="line_108"></a>tikz_curve := proc(pts,col_)
<a name="line_109"></a> local s,d,p;
<a name="line_110"></a>
<a name="line_111"></a> if nargs > 1 then
<a name="line_112"></a>  s := cat(" \\draw[smooth,",col_,"] ");
<a name="line_113"></a> else 
<a name="line_114"></a>  s := " \\draw[smooth] ";
<a name="line_115"></a> fi;
<a name="line_116"></a>
<a name="line_117"></a> d := "";
<a name="line_118"></a>
<a name="line_119"></a> for p in pts do 
<a name="line_120"></a>  s := cat(s,d,sprintf(" (%.3f,%.3f) ",op(evalf(p))));
<a name="line_121"></a>  d := "--";
<a name="line_122"></a> od;
<a name="line_123"></a>
<a name="line_124"></a> s := cat(s,";\n");
<a name="line_125"></a>
<a name="line_126"></a> return s;
<a name="line_127"></a>end:
<a name="line_128"></a>
<a name="line_129"></a>######################################################################
<a name="line_130"></a># Generate tikz code for a label, using syntax similar to Maple
<a name="line_131"></a>
<a name="line_132"></a><span style="color:red">#@ tikz_label 
</span><a name="line_133"></a>tikz_label := proc(xy,t,a_)
<a name="line_134"></a> local a,s;
<a name="line_135"></a>
<a name="line_136"></a> a := NULL;
<a name="line_137"></a>
<a name="line_138"></a> if nargs > 2 then
<a name="line_139"></a>  a := a_;
<a name="line_140"></a>  if not(member(a,["north","south","east","west",
<a name="line_141"></a>                   "north west","north east","south west","south east"])) then
<a name="line_142"></a>   a := tikz_anchor[a];
<a name="line_143"></a>   if not(member(a,["north","south","east","west",
<a name="line_144"></a>		    "north west","north east","south west","south east"])) then
<a name="line_145"></a>    a := NULL;
<a name="line_146"></a>   fi;
<a name="line_147"></a>  fi;
<a name="line_148"></a> fi;
<a name="line_149"></a>
<a name="line_150"></a> if a = NULL then
<a name="line_151"></a>  s := sprintf(" \\draw (%.3f,%.3f) node {$%s$};\n",op(evalf(xy)),t);
<a name="line_152"></a> else
<a name="line_153"></a>  s := sprintf(" \\draw (%.3f,%.3f) node[anchor=%s] {$%s$};\n",op(evalf(xy)),a,t);
<a name="line_154"></a> fi;
<a name="line_155"></a>
<a name="line_156"></a> return(s);
<a name="line_157"></a>end;
<a name="line_158"></a>
<a name="line_159"></a>######################################################################
<a name="line_160"></a>
<a name="line_161"></a><span style="color:red">#@ save_tikz 
</span><a name="line_162"></a>save_tikz := proc(s::string,txt::string) 
<a name="line_163"></a> local fd;
<a name="line_164"></a>
<a name="line_165"></a> fd := fopen(cat(latex_dir,"/tikz_includes/",s,".tex"),WRITE);
<a name="line_166"></a> fprintf(fd,"%s",txt);
<a name="line_167"></a> fclose(fd);
<a name="line_168"></a> return(txt);
<a name="line_169"></a>end: 
<a name="line_170"></a>
  </pre>
 </body>
</html>
    