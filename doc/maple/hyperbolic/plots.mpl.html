<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>######################################################################
<a name="line_2"></a>
<a name="line_3"></a># Convenience functions for plotting.
<a name="line_4"></a>
<a name="line_5"></a><span style="color:red">#@ xi_curve_xy
</span><a name="line_6"></a>xi_curve_xy := proc(p,s)
<a name="line_7"></a> local z;
<a name="line_8"></a> z := xi_curve(p,s);
<a name="line_9"></a> return([Re(z),Im(z)]);
<a name="line_10"></a>end:
<a name="line_11"></a>
<a name="line_12"></a><span style="color:red">#@ xi_arc
</span><a name="line_13"></a>xi_arc := proc(p,a,b) 
<a name="line_14"></a> plot(subs(a_H=a_H0,[Re(xi_curve(p,s)),Im(xi_curve(p,s)),s=a..b]),numpoints=200,args[4..-1]);
<a name="line_15"></a>end:
<a name="line_16"></a>
<a name="line_17"></a><span style="color:red">#@ xi_circle
</span><a name="line_18"></a>xi_circle := proc(p) 
<a name="line_19"></a> local q,t;
<a name="line_20"></a> q := p * (1 - sqrt(1-1/abs(p)^2) * exp(I*t));
<a name="line_21"></a> plot([Re(q),Im(q),t=-arcsin(1/abs(p)) .. arcsin(1/abs(p))],args[2..-1]);
<a name="line_22"></a>end:
<a name="line_23"></a>
<a name="line_24"></a><span style="color:red">#@ xi_curve_tikz
</span><a name="line_25"></a>xi_curve_tikz := proc(p,col_)
<a name="line_26"></a> local p0,r,d,x0,x1,y0,y1,m0,m1,theta,phi,col;
<a name="line_27"></a>
<a name="line_28"></a> if nargs > 1 then
<a name="line_29"></a>  if type(col_,string) then
<a name="line_30"></a>   col := sprintf("[%s]",col_);
<a name="line_31"></a>  else
<a name="line_32"></a>   col := sprintf("[%a]",col_);
<a name="line_33"></a>  fi;
<a name="line_34"></a> else
<a name="line_35"></a>  col := "";
<a name="line_36"></a> fi;
<a name="line_37"></a>
<a name="line_38"></a> p0 := evalf(subs(a_H=a_H0,p)); 
<a name="line_39"></a> r := evalf(abs(p0));
<a name="line_40"></a> d := sqrt(r^2-1);
<a name="line_41"></a> theta := evalf(argument(-p0));
<a name="line_42"></a> phi   := evalf(arccos(d/r));
<a name="line_43"></a> x0 := Re(p0);
<a name="line_44"></a> y0 := Im(p0);
<a name="line_45"></a> x1 := x0 + d * cos(theta-phi);
<a name="line_46"></a> y1 := y0 + d * sin(theta-phi);
<a name="line_47"></a> m0 := round(evalf((theta-phi)*180/Pi));
<a name="line_48"></a> m1 := round(evalf((theta+phi)*180/Pi));
<a name="line_49"></a> sprintf("  \\draw%s (%.3f,%.3f) (%.3f,%.3f) arc(%d:%d:%.3f);\n",
<a name="line_50"></a>         col,x0,y0,x1,y1,m0,m1,d);
<a name="line_51"></a>end:
<a name="line_52"></a>
<a name="line_53"></a>######################################################################
<a name="line_54"></a>
<a name="line_55"></a><span style="color:red">#@ make_HX_plot_tikz
</span><a name="line_56"></a>make_HX_plot_tikz := proc()
<a name="line_57"></a> local A,s,i,j,k,ii,x,y,col,c_offset,v_offset;
<a name="line_58"></a> global hyp_plot_tikz;
<a name="line_59"></a>
<a name="line_60"></a> A := map(op,[indices(v_H)]);
<a name="line_61"></a> A := remove(i -> (i = evalf(floor(i)) and not(type(i,integer))),A);
<a name="line_62"></a>
<a name="line_63"></a> s := cat(
<a name="line_64"></a>  "\\begin{center}\n",
<a name="line_65"></a>  " \\begin{tikzpicture}[scale=7]\n",
<a name="line_66"></a>  "  \\draw (0,0) circle(1);\n",
<a name="line_67"></a>  "  \\draw[blue] (-1,0) -- (1,0);\n",
<a name="line_68"></a>  "  \\draw[blue] (0,-1) -- (0,1);\n",
<a name="line_69"></a>  "  \\draw[green] (-0.707,-0.707) -- ( 0.707, 0.707);\n",
<a name="line_70"></a>  "  \\draw[green] (-0.707, 0.707) -- ( 0.707,-0.707);\n"
<a name="line_71"></a> );
<a name="line_72"></a>
<a name="line_73"></a> for k in [0,3,4,7,8] do
<a name="line_74"></a>  s := cat(s,xi_curve_tikz(c_H_p[k],c_colour[k]));
<a name="line_75"></a>  col := sprintf("%a,dotted",c_colour[k]);
<a name="line_76"></a>  for j from 1 to 3 do
<a name="line_77"></a>   s := cat(s,xi_curve_tikz(I^j*c_H_p[k],col));
<a name="line_78"></a>  od:
<a name="line_79"></a> od:
<a name="line_80"></a>
<a name="line_81"></a> for i in A do
<a name="line_82"></a>  if type(i,integer) then
<a name="line_83"></a>   s := sprintf("%s  \\fill[black](%.3f,%.3f) circle(0.007);\n",
<a name="line_84"></a>                s,Re(v_H1[i]),Im(v_H1[i]));
<a name="line_85"></a>  else
<a name="line_86"></a>   s := sprintf("%s  \\fill[gray!60](%.3f,%.3f) circle(0.007);\n",
<a name="line_87"></a>                s,Re(v_H1[i]),Im(v_H1[i]));
<a name="line_88"></a>  fi;
<a name="line_89"></a> od;
<a name="line_90"></a>
<a name="line_91"></a> s := cat(s,
<a name="line_92"></a>  "  \\draw( 0.30, 0.25) node{$c_1$};\n",
<a name="line_93"></a>  "  \\draw(-0.30, 0.25) node{$c_2$};\n",
<a name="line_94"></a>  "  \\draw( 0.80,-0.03) node{$c_5$};\n",
<a name="line_95"></a>  "  \\draw(-0.05, 0.80) node{$c_6$};\n"
<a name="line_96"></a> );
<a name="line_97"></a>
<a name="line_98"></a> c_offset[0] := [ 0.32,-0.15];
<a name="line_99"></a> c_offset[3] := [ 0.07, 0.10];
<a name="line_100"></a> c_offset[4] := [ 0.22, 0.03];
<a name="line_101"></a> c_offset[7] := [-0.01,-0.07];
<a name="line_102"></a> c_offset[8] := [-0.06,-0.02];
<a name="line_103"></a>
<a name="line_104"></a> for k in [0,3,4,7,8] do
<a name="line_105"></a>  x := evalf(Re(c_H1[k](0.5)));
<a name="line_106"></a>  y := evalf(Im(c_H1[k](0.5)));
<a name="line_107"></a>  x := x + c_offset[k][1];
<a name="line_108"></a>  y := y + c_offset[k][2];
<a name="line_109"></a>  s := sprintf("%s  \\draw(%.3f,%.3f) node{$c_{%d}$};\n",
<a name="line_110"></a>               s,x,y,k);
<a name="line_111"></a> od:
<a name="line_112"></a>
<a name="line_113"></a> v_offset[ 0  ] := [ 0.06, 0.02];
<a name="line_114"></a> v_offset[ 1  ] := [ 0.03, 0.03];
<a name="line_115"></a> v_offset[ 1.1] := [-0.05,-0.03];
<a name="line_116"></a> v_offset[ 2  ] := [-0.05, 0.01];
<a name="line_117"></a> v_offset[ 2.1] := [ 0.06, 0.01];
<a name="line_118"></a> v_offset[ 3  ] := [-0.05, 0.00];
<a name="line_119"></a> v_offset[ 3.1] := [-0.05,-0.01];
<a name="line_120"></a> v_offset[ 4  ] := [-0.05, 0.00];
<a name="line_121"></a> v_offset[ 4.1] := [-0.06, 0.00];
<a name="line_122"></a> v_offset[ 5  ] := [ 0.05, 0.00];
<a name="line_123"></a> v_offset[ 5.1] := [-0.05, 0.00];
<a name="line_124"></a> v_offset[ 6  ] := [-0.04, 0.00];
<a name="line_125"></a> v_offset[ 7  ] := [ 0.04, 0.00];
<a name="line_126"></a> v_offset[ 8  ] := [ 0.03,-0.03];
<a name="line_127"></a> v_offset[ 9  ] := [-0.03,-0.03];
<a name="line_128"></a> v_offset[10  ] := [-0.04,-0.03];
<a name="line_129"></a> v_offset[10.1] := [-0.06,-0.03];
<a name="line_130"></a> v_offset[11  ] := [-0.03,-0.03];
<a name="line_131"></a> v_offset[11.1] := [ 0.06,-0.03];
<a name="line_132"></a> v_offset[12  ] := [-0.07, 0.00];
<a name="line_133"></a> v_offset[12.1] := [-0.06, 0.00];
<a name="line_134"></a> v_offset[12.2] := [-0.06, 0.00];
<a name="line_135"></a> v_offset[12.3] := [ 0.06, 0.00];
<a name="line_136"></a> v_offset[13  ] := [ 0.01,-0.04];
<a name="line_137"></a> v_offset[13.1] := [-0.06, 0.00];
<a name="line_138"></a> v_offset[13.2] := [ 0.06, 0.00];
<a name="line_139"></a> v_offset[13.3] := [-0.06, 0.00];
<a name="line_140"></a> v_offset[14  ] := [ 0.00, 0.00];
<a name="line_141"></a>
<a name="line_142"></a> for i in A do
<a name="line_143"></a>  x := Re(v_H1[i]);
<a name="line_144"></a>  y := Im(v_H1[i]);
<a name="line_145"></a>  if not(type(v_offset[i],indexed)) then
<a name="line_146"></a>   x := x + v_offset[i][1];
<a name="line_147"></a>   y := y + v_offset[i][2];
<a name="line_148"></a>  fi;
<a name="line_149"></a>  s := sprintf("%s  \\draw(%.3f,%.3f) node{$v_{%a}$};\n",
<a name="line_150"></a>               s,x,y,i);
<a name="line_151"></a> od;
<a name="line_152"></a>
<a name="line_153"></a> s := cat(s,
<a name="line_154"></a>  " \\end{tikzpicture}\n",
<a name="line_155"></a>  "\\end{center}"
<a name="line_156"></a> );
<a name="line_157"></a>
<a name="line_158"></a> save_tikz("HX",s);
<a name="line_159"></a> return s;
<a name="line_160"></a>end:
<a name="line_161"></a>
<a name="line_162"></a>######################################################################
<a name="line_163"></a>
<a name="line_164"></a><span style="color:red">#@ make_a_H_dependence_plot_tikz
</span><a name="line_165"></a>make_a_H_dependence_plot_tikz := proc(a)
<a name="line_166"></a> local s,k,d,phi,m,p,r,theta;
<a name="line_167"></a>
<a name="line_168"></a> s := cat(
<a name="line_169"></a>  " \\begin{tikzpicture}[scale=3]\n",
<a name="line_170"></a>  "  \\draw (0,0) (1,0) arc(0:90:1);\n",
<a name="line_171"></a>  "  \\draw[blue] (0,1) -- (0,0) -- (1,0);\n",
<a name="line_172"></a>  "  \\draw[green] (0,0) -- ( 0.707, 0.707);\n"
<a name="line_173"></a> );
<a name="line_174"></a>
<a name="line_175"></a> for k in [0,7,8] do
<a name="line_176"></a>  s := cat(s,xi_curve_tikz(evalf(subs(a_H=a,c_H_p[k])),c_colour[k]));
<a name="line_177"></a> od:
<a name="line_178"></a>
<a name="line_179"></a> p := evalf(subs(a_H = a,c_H_p[3]));
<a name="line_180"></a> r := evalf(subs(a_H = a,c_H_r[3]));
<a name="line_181"></a> theta := round(evalf(arctan(1/r) * 180 / Pi));
<a name="line_182"></a> s := sprintf("%s  \\draw[magenta](%.3f,0) (%.3f,0) arc(180:%d:%.3f);\n",
<a name="line_183"></a>	       s,p,p-r,180-theta,r);
<a name="line_184"></a> s := sprintf("%s  \\draw[magenta](0,%.3f) (0,%.3f) arc(270:%d:%.3f);\n",
<a name="line_185"></a>	       s,p,p-r,270+theta,r);
<a name="line_186"></a> 
<a name="line_187"></a> if false then
<a name="line_188"></a>  d := evalf(sqrt(1/p^2-1));
<a name="line_189"></a>  phi := evalf(arccos(d*p));
<a name="line_190"></a>  m := round(evalf(phi * 180/Pi));
<a name="line_191"></a>  s := sprintf("%s  \\draw[magenta](%.3f,0) (%.3f,0) arc(180:%d:%.3f);\n",
<a name="line_192"></a>	       s,evalf(1/p),evalf(1/p-d),180-m,d);
<a name="line_193"></a>  s := sprintf("%s  \\draw[magenta](0,%.3f) (0,%.3f) arc(270:%d:%.3f);\n",
<a name="line_194"></a>	       s,evalf(1/p),evalf(1/p-d),270+m,d);
<a name="line_195"></a> fi;
<a name="line_196"></a> 
<a name="line_197"></a> s := cat(s,
<a name="line_198"></a>  " \\end{tikzpicture}\n"
<a name="line_199"></a> );
<a name="line_200"></a>
<a name="line_201"></a> save_tikz("a_H_dependence",s);
<a name="line_202"></a> return s;
<a name="line_203"></a>end:
<a name="line_204"></a>
<a name="line_205"></a>######################################################################
<a name="line_206"></a>
<a name="line_207"></a><span style="color:red">#@ make_F1_plot_tikz
</span><a name="line_208"></a>make_F1_plot_tikz := proc()
<a name="line_209"></a> local theta,r0,r1,m0,m1,arcs,vertices,a,s;
<a name="line_210"></a> global F1_arcs,F1_theta,F1_tikz;
<a name="line_211"></a> 
<a name="line_212"></a> r0 := period_a;
<a name="line_213"></a> r1 := (2*period^2-1)/(2*period);
<a name="line_214"></a>
<a name="line_215"></a> vertices := [
<a name="line_216"></a>  [13  ,"west"],
<a name="line_217"></a>  [ 1  ,"south west"],
<a name="line_218"></a>  [12.2,"south"],
<a name="line_219"></a>  [12  ,"south"],
<a name="line_220"></a>  [ 1.1,"south east"],
<a name="line_221"></a>  [13.3,"east"],
<a name="line_222"></a>  [13.1,"east"],
<a name="line_223"></a>  [ 1.2,"north east"],
<a name="line_224"></a>  [12.3,"north"],
<a name="line_225"></a>  [12.1,"north"],
<a name="line_226"></a>  [ 1.3,"north west"],
<a name="line_227"></a>  [13.2,"west"]
<a name="line_228"></a> ];
<a name="line_229"></a>
<a name="line_230"></a> arcs := evalf(subs(a_H = a_H0,F1_arcs));
<a name="line_231"></a>
<a name="line_232"></a> s := cat(
<a name="line_233"></a>  "\\begin{center}\n",
<a name="line_234"></a>  " \\begin{tikzpicture}[scale=5]\n"
<a name="line_235"></a> );
<a name="line_236"></a>
<a name="line_237"></a> for a in arcs do
<a name="line_238"></a>  m0 := round(evalf(a[4]*180/Pi));
<a name="line_239"></a>  m1 := round(evalf(a[5]*180/Pi));
<a name="line_240"></a>  s := sprintf("%s  \\draw[%a](%.3f,%.3f) +(%d:%.3f) arc(%d:%d:%.3f);\n",
<a name="line_241"></a>               s,a[6],a[1],a[2],m0,a[3],m0,m1,a[3]);
<a name="line_242"></a> od;
<a name="line_243"></a>
<a name="line_244"></a> for a in vertices do
<a name="line_245"></a>  s := sprintf("%s  \\fill(%.3f,%.3f) circle(0.01);\n",
<a name="line_246"></a>               s,Re(v_H1[a[1]]),Im(v_H1[a[1]]));
<a name="line_247"></a> od:
<a name="line_248"></a>
<a name="line_249"></a> for a in vertices do
<a name="line_250"></a>  s := sprintf("%s  \\draw(%.3f,%.3f) node[anchor=%s]{$v_{%a}$};\n",
<a name="line_251"></a>               s,Re(v_H1[a[1]]),Im(v_H1[a[1]]),a[2],a[1]);
<a name="line_252"></a> od:
<a name="line_253"></a>
<a name="line_254"></a> s := cat(s,
<a name="line_255"></a>  " \\end{tikzpicture}\n",
<a name="line_256"></a>  "\\end{center}"
<a name="line_257"></a> );
<a name="line_258"></a>
<a name="line_259"></a> F1_tikz := s;
<a name="line_260"></a>
<a name="line_261"></a> save_tikz("F1",s);
<a name="line_262"></a> return s;
<a name="line_263"></a>end:
<a name="line_264"></a>
<a name="line_265"></a>######################################################################
<a name="line_266"></a>
<a name="line_267"></a><span style="color:red">#@ beta_offset_latex
</span><a name="line_268"></a>beta_offset_latex := proc()
<a name="line_269"></a> local i,j,k,s,t,A;
<a name="line_270"></a>
<a name="line_271"></a> s := "\\begin{align*}\n";
<a name="line_272"></a>
<a name="line_273"></a> for i from 0 to 13 do
<a name="line_274"></a>  j := act_V[L](i);
<a name="line_275"></a>  t := sprintf(" \\lm(v_{%2d}) &= ",i);
<a name="line_276"></a>  A := v_beta_offset[L,i];
<a name="line_277"></a>  for k in A do
<a name="line_278"></a>   t := sprintf("%s\\bt_%d(",t,k);
<a name="line_279"></a>  od:
<a name="line_280"></a>  t := sprintf("%sv_{%2d}",t,j);
<a name="line_281"></a>  for k in A do
<a name="line_282"></a>   t := cat(t,")");
<a name="line_283"></a>  od:
<a name="line_284"></a>  t := cat(t," &\n");
<a name="line_285"></a>  s := cat(s,t);
<a name="line_286"></a>
<a name="line_287"></a>  j := act_V[M](i);
<a name="line_288"></a>  t := sprintf(" \\mu(v_{%2d}) &= ",i);
<a name="line_289"></a>  A := v_beta_offset[M,i];
<a name="line_290"></a>  for k in A do
<a name="line_291"></a>   t := sprintf("%s\\bt_%d(",t,k);
<a name="line_292"></a>  od:
<a name="line_293"></a>  t := sprintf("%sv_{%2d}",t,j);
<a name="line_294"></a>  for k in A do
<a name="line_295"></a>   t := cat(t,")");
<a name="line_296"></a>  od:
<a name="line_297"></a>  t := cat(t," &\n");
<a name="line_298"></a>  s := cat(s,t);
<a name="line_299"></a>
<a name="line_300"></a>  j := act_V[N](i);
<a name="line_301"></a>  t := sprintf(" \\nu(v_{%2d}) &= ",i);
<a name="line_302"></a>  A := v_beta_offset[N,i];
<a name="line_303"></a>  for k in A do
<a name="line_304"></a>   t := sprintf("%s\\bt_%d(",t,k);
<a name="line_305"></a>  od:
<a name="line_306"></a>  t := sprintf("%sv_{%2d}",t,j);
<a name="line_307"></a>  for k in A do
<a name="line_308"></a>   t := cat(t,")");
<a name="line_309"></a>  od:
<a name="line_310"></a>  t := cat(t," \\\\\n");
<a name="line_311"></a>  s := cat(s,t);
<a name="line_312"></a> od;
<a name="line_313"></a>
<a name="line_314"></a> s := cat(s,"\\end{align*}\n");
<a name="line_315"></a>
<a name="line_316"></a> return(s);
<a name="line_317"></a>end:
<a name="line_318"></a>
<a name="line_319"></a>######################################################################
<a name="line_320"></a>
<a name="line_321"></a><span style="color:red">#@ make_c_H_plots
</span><a name="line_322"></a>make_c_H_plots := proc()
<a name="line_323"></a> local i;
<a name="line_324"></a> global pics,c_H_plot;
<a name="line_325"></a>
<a name="line_326"></a> c_H_plot[1] := line([-1,-1] /~ sqrt(2.),[ 1, 1] /~ sqrt(2.),colour=c_colour[1]);
<a name="line_327"></a> c_H_plot[2] := line([-1, 1] /~ sqrt(2.),[ 1,-1] /~ sqrt(2.),colour=c_colour[2]);
<a name="line_328"></a>
<a name="line_329"></a> c_H_plot[5] := line([-1, 0],[ 1, 0],colour=c_colour[5]);
<a name="line_330"></a> c_H_plot[6] := line([ 0,-1],[ 0, 1],colour=c_colour[6]);
<a name="line_331"></a>
<a name="line_332"></a> for i in [0,3,4,7,8] do
<a name="line_333"></a>  c_H_plot[i] := xi_circle(c_H_p0[i],colour = c_colour[i]);
<a name="line_334"></a> od:
<a name="line_335"></a>
<a name="line_336"></a> for i from 0 to 8 do
<a name="line_337"></a>  pics[sprintf("c_H[%d]",i)] :=
<a name="line_338"></a>   display(c_H_plot[i],axes=none,scaling=constrained);
<a name="line_339"></a> od:
<a name="line_340"></a>
<a name="line_341"></a> pics["curves_H"] := 
<a name="line_342"></a>  display(
<a name="line_343"></a>   circle(),
<a name="line_344"></a>   seq(c_H_plot[i],i=0..8),
<a name="line_345"></a>   scaling=constrained,axes=none
<a name="line_346"></a>  );
<a name="line_347"></a>
<a name="line_348"></a> save_plots(seq(sprintf("c_H[%d]",i),i=0..8),"curves_H");
<a name="line_349"></a> save_jpgs( seq(sprintf("c_H[%d]",i),i=0..8),"curves_H");
<a name="line_350"></a> pics["curves_H"];
<a name="line_351"></a>end:
<a name="line_352"></a>
<a name="line_353"></a>save_c_H_plots := proc()
<a name="line_354"></a> save_plots(seq(sprintf("c_H[%d]",i),i=0..8),"curves_H");
<a name="line_355"></a>end:
<a name="line_356"></a>
<a name="line_357"></a>load_c_H_plots := proc()
<a name="line_358"></a> load_plots(seq(sprintf("c_H[%d]",i),i=0..8),"curves_H");
<a name="line_359"></a>end:
<a name="line_360"></a>
<a name="line_361"></a>######################################################################
<a name="line_362"></a>
<a name="line_363"></a><span style="color:red">#@ make_hyperbolic_domains
</span><a name="line_364"></a>make_hyperbolic_domains := proc()
<a name="line_365"></a> local c_H_list,m,m1,s0,s1,t0,t1,u0,u1,v0,v1;
<a name="line_366"></a> global F1_H_boundary,F1_H_boundaryo,F4_H_boundary,F8_H_boundary,F16_H_boundary;
<a name="line_367"></a>
<a name="line_368"></a> c_H_list := proc(k,a,b)
<a name="line_369"></a>  [seq(evalf(c_H0[k](a + 0.05*i*(b-a))),i=0..20)];
<a name="line_370"></a> end:
<a name="line_371"></a>
<a name="line_372"></a> F16_H_boundary := [
<a name="line_373"></a>  op(c_H_list(5,0,Pi)),
<a name="line_374"></a>  op(c_H_list(3,0,Pi/2)),
<a name="line_375"></a>  op(c_H_list(0,Pi/2,Pi/4)),
<a name="line_376"></a>  op(c_H_list(1,Pi,0))
<a name="line_377"></a> ]:
<a name="line_378"></a>
<a name="line_379"></a> F8_H_boundary := [
<a name="line_380"></a>  op(c_H_list(5,0,Pi)),
<a name="line_381"></a>  op(c_H_list(3,0,Pi/2)),
<a name="line_382"></a>  op(c_H_list(0,Pi/2,0)),
<a name="line_383"></a>  op(c_H_list(4,-Pi/2,0)),
<a name="line_384"></a>  op(c_H_list(6,Pi,0))
<a name="line_385"></a> ]:
<a name="line_386"></a>
<a name="line_387"></a> F4_H_boundary := [
<a name="line_388"></a>  op(c_H_list(5,0,Pi)),
<a name="line_389"></a>  op(c_H_list(3,0,Pi)),
<a name="line_390"></a>  op(c_H_list(7,Pi,0)),
<a name="line_391"></a>  op(c_H_list(8,Pi,0)),
<a name="line_392"></a>  op(c_H_list(4,-Pi,0)),
<a name="line_393"></a>  op(c_H_list(6,Pi,0))
<a name="line_394"></a> ]:
<a name="line_395"></a>
<a name="line_396"></a> F1_H_boundary := [
<a name="line_397"></a>  op(c_H_list(3,-Pi,Pi)),
<a name="line_398"></a>  op(c_H_list(7,Pi,0)),
<a name="line_399"></a>  op(c_H_list(8,0,Pi))
<a name="line_400"></a> ]:
<a name="line_401"></a> F1_H_boundary := [op(F1_H_boundary),op(map(z ->I*z,F1_H_boundary))]:
<a name="line_402"></a> F1_H_boundary := [op(F1_H_boundary),op(map(z -> -z,F1_H_boundary))]:
<a name="line_403"></a>
<a name="line_404"></a> NULL:
<a name="line_405"></a>end:
<a name="line_406"></a>
<a name="line_407"></a>######################################################################
<a name="line_408"></a>
<a name="line_409"></a><span style="color:red">#@ make_H_plots
</span><a name="line_410"></a>make_H_plots := proc()
<a name="line_411"></a> local k,s;
<a name="line_412"></a> global pics;
<a name="line_413"></a>
<a name="line_414"></a> pics["F1_H_boundary"] := display(
<a name="line_415"></a>  seq(cplot(I^k*c_H0[3](s),s=-Pi..Pi,colour = c_colour[3]),k=0..3),
<a name="line_416"></a>  seq(cplot(I^k*c_H0[7](s),s= Pi.. 0,colour = c_colour[7]),k=0..3),
<a name="line_417"></a>  seq(cplot(I^k*c_H0[8](s),s=  0..Pi,colour = c_colour[8]),k=0..3),
<a name="line_418"></a>  axes=none
<a name="line_419"></a> ):
<a name="line_420"></a>
<a name="line_421"></a> pics["F1_H_boundary_b"] :=
<a name="line_422"></a>  display(
<a name="line_423"></a>   map(u -> plot([u[1]+u[3]*cos(t),u[2]+u[3]*sin(t),t=u[4]..u[5]],colour=u[6]),
<a name="line_424"></a>       evalf(subs(a_H = a_H0,F1_arcs))),
<a name="line_425"></a>   axes = none
<a name="line_426"></a>  );
<a name="line_427"></a>
<a name="line_428"></a> pics["F4_H_boundary"] := display(
<a name="line_429"></a>  cplot(c_H0[5](s),s=0..Pi, colour = c_colour[5]),
<a name="line_430"></a>  cplot(c_H0[3](s),s=0..Pi, colour = c_colour[3]),
<a name="line_431"></a>  cplot(c_H0[7](s),s=Pi..0, colour = c_colour[7]),
<a name="line_432"></a>  cplot(c_H0[8](s),s=Pi..0, colour = c_colour[8]),
<a name="line_433"></a>  cplot(c_H0[4](s),s=-Pi..0,colour = c_colour[4]),
<a name="line_434"></a>  cplot(c_H0[6](s),s=Pi..0, colour = c_colour[6]),
<a name="line_435"></a>  axes=none,scaling=constrained
<a name="line_436"></a> ):
<a name="line_437"></a>
<a name="line_438"></a> pics["F8_H_boundary"] := display(
<a name="line_439"></a>  cplot(c_H0[5](s),s=0..Pi,     colour = c_colour[5]),
<a name="line_440"></a>  cplot(c_H0[3](s),s=0..Pi,     colour = c_colour[3]),
<a name="line_441"></a>  cplot(c_H0[0](s),s=Pi/4..Pi/2,colour = c_colour[0]),
<a name="line_442"></a>  cplot(c_H0[7](s),s=0..Pi,     colour = c_colour[7]),
<a name="line_443"></a>  cplot(c_H0[1](s),s=0..Pi,     colour = c_colour[1]),
<a name="line_444"></a>  axes=none,scaling=constrained
<a name="line_445"></a> );
<a name="line_446"></a>
<a name="line_447"></a> pics["F16_H_boundary"] := display(
<a name="line_448"></a>  cplot(c_H0[0](s),s=Pi/4..Pi/2,colour = c_colour[0]),
<a name="line_449"></a>  cplot(c_H0[1](s),s=0..Pi/2,   colour = c_colour[1]),
<a name="line_450"></a>  cplot(c_H0[3](s),s=0..Pi/2,   colour = c_colour[3]),
<a name="line_451"></a>  cplot(c_H0[5](s),s=0..Pi,     colour = c_colour[5]),
<a name="line_452"></a>  axes=none,scaling=constrained
<a name="line_453"></a> ):
<a name="line_454"></a>
<a name="line_455"></a> save_plot("F1_H_boundary");
<a name="line_456"></a> save_plot("F1_H_boundary_b");
<a name="line_457"></a> save_plot("F4_H_boundary");
<a name="line_458"></a> save_plot("F8_H_boundary");
<a name="line_459"></a> save_plot("F16_H_boundary");
<a name="line_460"></a>
<a name="line_461"></a> save_jpg("F1_H_boundary");
<a name="line_462"></a> save_jpg("F1_H_boundary_b");
<a name="line_463"></a> save_jpg("F4_H_boundary");
<a name="line_464"></a> save_jpg("F8_H_boundary");
<a name="line_465"></a> save_jpg("F16_H_boundary");
<a name="line_466"></a>
<a name="line_467"></a> NULL;
<a name="line_468"></a>end:
<a name="line_469"></a>
<a name="line_470"></a>######################################################################
<a name="line_471"></a>
<a name="line_472"></a><span style="color:red">#@ make_radius_plot_tikz
</span><a name="line_473"></a>make_radius_plot_tikz := proc()
<a name="line_474"></a> global tikz_pics;
<a name="line_475"></a> local s;
<a name="line_476"></a>
<a name="line_477"></a> s := cat(
<a name="line_478"></a>   "\\begin{center}\n",
<a name="line_479"></a>   " \\begin{tikzpicture}[scale=3]\n",
<a name="line_480"></a>   "  \\draw[black,->] (-0.05,0) -- (2.05,0);\n",
<a name="line_481"></a>   "  \\draw[black,->] (0,-0.05) -- (0,1.05);\n",
<a name="line_482"></a>   "  \\draw[black] (2,-0.05) -- (2,0);\n",
<a name="line_483"></a>   "  \\draw[black] (-0.05,1) -- (0,1);\n",
<a name="line_484"></a>   "  \\draw ( 0.00,-0.05) node[anchor=north] {$0$};\n",
<a name="line_485"></a>   "  \\draw ( 2.00,-0.05) node[anchor=north] {$1$};\n",
<a name="line_486"></a>   "  \\draw (-0.05, 0.00) node[anchor=east ] {$0$};\n",
<a name="line_487"></a>   "  \\draw (-0.05, 1.00) node[anchor=east ] {$1$};\n",
<a name="line_488"></a>   "  \\draw ( 2.05, 0.00) node[anchor=west ] {$b$};\n",
<a name="line_489"></a>   "  \\draw ( 0.00, 1.05) node[anchor=south] {$|m|$};\n",
<a name="line_490"></a>   tikz_plot(unapply([2*a_H,abs(min_centre_a)],a_H),0.0..0.1,30,red),
<a name="line_491"></a>   tikz_plot(unapply([2*a_H,abs(min_centre_a)],a_H),0.1..0.9,30,red),
<a name="line_492"></a>   tikz_plot(unapply([2*a_H,abs(min_centre_a)],a_H),0.9..1.0,30,red),
<a name="line_493"></a>   " \\end{tikzpicture}\n",
<a name="line_494"></a>   "\\end{center}\n"
<a name="line_495"></a> ):
<a name="line_496"></a>
<a name="line_497"></a> tikz_pics["radius_plot"] := s;
<a name="line_498"></a> save_tikz("radius_plot",s);
<a name="line_499"></a> return s;
<a name="line_500"></a>end:
<a name="line_501"></a>
<a name="line_502"></a>######################################################################
<a name="line_503"></a>
<a name="line_504"></a><span style="color:red">#@ make_H_to_P_graph_tikz
</span><a name="line_505"></a>make_H_to_P_graph_tikz := proc()
<a name="line_506"></a> local s;
<a name="line_507"></a>
<a name="line_508"></a> if not(assigned(HP_table)) then
<a name="line_509"></a>  load_data["HP_table"]();
<a name="line_510"></a> fi;
<a name="line_511"></a> s := HP_table["spline_plot_tikz"];
<a name="line_512"></a> save_tikz("H_to_P_graph",s);
<a name="line_513"></a>
<a name="line_514"></a> return s;
<a name="line_515"></a>end:
<a name="line_516"></a>
<a name="line_517"></a>
<a name="line_518"></a>######################################################################
<a name="line_519"></a>
<a name="line_520"></a><span style="color:red">#@ make_square_diffeo_H_plot
</span><a name="line_521"></a>make_square_diffeo_H_plot := proc()
<a name="line_522"></a> global pics;
<a name="line_523"></a> local NN,PP,i,j,c,t0;
<a name="line_524"></a>
<a name="line_525"></a> NN := 20;
<a name="line_526"></a> PP := NULL;
<a name="line_527"></a> for i from 0 to NN do
<a name="line_528"></a>  for j from 0 to NN do
<a name="line_529"></a>   t0[i,j] := C_to_R2(square_diffeo_H0_inverse([i/NN,j/NN])); 
<a name="line_530"></a>  od:
<a name="line_531"></a> od:
<a name="line_532"></a> for i from 0 to NN do
<a name="line_533"></a>  for j from 0 to NN-1 do
<a name="line_534"></a>   if i = 0 then 
<a name="line_535"></a>    c := c_colour[0];
<a name="line_536"></a>   elif i = NN then
<a name="line_537"></a>    c := c_colour[5];
<a name="line_538"></a>   else
<a name="line_539"></a>    c := grey;
<a name="line_540"></a>   fi;
<a name="line_541"></a>   PP := PP,line(t0[i,j],t0[i,j+1],colour=c);
<a name="line_542"></a>  od:
<a name="line_543"></a> od:
<a name="line_544"></a> for i from 0 to NN-1 do
<a name="line_545"></a>  for j from 0 to NN do
<a name="line_546"></a>   if j = 0 then 
<a name="line_547"></a>    c := c_colour[1];
<a name="line_548"></a>   elif j = NN then
<a name="line_549"></a>    c := c_colour[3];
<a name="line_550"></a>   else
<a name="line_551"></a>    c := grey;
<a name="line_552"></a>   fi;
<a name="line_553"></a>   PP := PP,line(t0[i,j],t0[i+1,j],colour=c);
<a name="line_554"></a>  od:
<a name="line_555"></a> od:
<a name="line_556"></a> pics["square_diffeo_H"] := display(PP,scaling=constrained,axes=none);
<a name="line_557"></a> save_plot("square_diffeo_H");
<a name="line_558"></a> save_jpg( "square_diffeo_H");
<a name="line_559"></a> pics["square_diffeo_H"];
<a name="line_560"></a>end:
<a name="line_561"></a>
<a name="line_562"></a>######################################################################
<a name="line_563"></a>
<a name="line_564"></a><span style="color:red">#@ make_tile_plot
</span><a name="line_565"></a>make_tile_plot := proc()
<a name="line_566"></a> local P,z0,k,T,e0,e1,m1;
<a name="line_567"></a> global pics;
<a name="line_568"></a>
<a name="line_569"></a> P := circle(1):
<a name="line_570"></a> z0 := v_H0[3]/2:
<a name="line_571"></a> for k in [0,1,3,5] do
<a name="line_572"></a>  e0 := evalf(subs(a_H=a_H0,c_H_ends[k])):
<a name="line_573"></a>  for T in [entries(tile)] do
<a name="line_574"></a>   e1 := map2(act_Pi_tilde0,op(T),e0);
<a name="line_575"></a>   if abs(e1[1] + e1[2]) > 10^(-20) then
<a name="line_576"></a>    m1 := ends_to_centre(op(e1));
<a name="line_577"></a>    P := P,xi_circle(m1,colour = c_colour[k]);
<a name="line_578"></a>   else
<a name="line_579"></a>    P := P,line(C_to_R2(e1[1]),C_to_R2(e1[2]),colour=c_colour[k]);
<a name="line_580"></a>   fi:
<a name="line_581"></a>  od:
<a name="line_582"></a> od:
<a name="line_583"></a> for T in [entries(tile)] do
<a name="line_584"></a>  P := P,cpoint(act_Pi_tilde(op(T),z0)):
<a name="line_585"></a> od:
<a name="line_586"></a>
<a name="line_587"></a> P := display(P,axes=none,scaling=constrained);
<a name="line_588"></a> pics["tiles"] := P;
<a name="line_589"></a> save_plot("tiles");
<a name="line_590"></a> save_jpg( "tiles");
<a name="line_591"></a> P;
<a name="line_592"></a>end:
<a name="line_593"></a>
<a name="line_594"></a>######################################################################
<a name="line_595"></a>
<a name="line_596"></a><span style="color:red">#@ make_H_to_P_poles_plot
</span><a name="line_597"></a>make_H_to_P_poles_plot := proc()
<a name="line_598"></a> global pics;
<a name="line_599"></a> 
<a name="line_600"></a> pics["H_to_P_poles"] := display(
<a name="line_601"></a>  circle(1),
<a name="line_602"></a>  circle(0.468,colour=grey),
<a name="line_603"></a>  cplot(subs(a_H=a_H0,c_HS[0](t)),t=F4_curve_limits[0],colour=c_colour[0]),
<a name="line_604"></a>  cplot(subs(a_H=a_H0,c_HS[1](t)),t=F4_curve_limits[1],colour=c_colour[1]),
<a name="line_605"></a>  cplot( subs(a_H=a_H0,c_HS[3](t)),t=F4_curve_limits[3],colour=c_colour[3]),
<a name="line_606"></a>  cplot(-subs(a_H=a_H0,c_HS[3](t)),t=F4_curve_limits[3],colour=c_colour[3]),
<a name="line_607"></a>  cplot( subs(a_H=a_H0,c_HS[5](t)),t=F4_curve_limits[5],colour=c_colour[5]),
<a name="line_608"></a>  cplot(-subs(a_H=a_H0,c_HS[5](t)),t=F4_curve_limits[5],colour=c_colour[5]),
<a name="line_609"></a>  cplot( conjugate(subs(a_H=a_H0,c_HS[5](t))),t=F4_curve_limits[5],colour=c_colour[5]),
<a name="line_610"></a>  cplot(-conjugate(subs(a_H=a_H0,c_HS[5](t))),t=F4_curve_limits[5],colour=c_colour[5]),
<a name="line_611"></a>  map(cpoint,evalf(subs(a_H=a_H0,small_p1_poles)),colour=red),
<a name="line_612"></a>  scaling=constrained,axes=none
<a name="line_613"></a> ):
<a name="line_614"></a>
<a name="line_615"></a> save_plot("H_to_P_poles");
<a name="line_616"></a> save_jpg( "H_to_P_poles");
<a name="line_617"></a>
<a name="line_618"></a> pics["H_to_P_poles"];
<a name="line_619"></a>end:
  </pre>
 </body>
</html>
    