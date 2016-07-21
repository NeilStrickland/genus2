<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># This file defines Maple functions to generate various plots and save
<a name="line_2"></a># them as jpg files or in Maple's internal format.  Some of the jpg
<a name="line_3"></a># files are then included in the main LaTeX document for the project.
<a name="line_4"></a>
<a name="line_5"></a># There are directories called plots and images, which are siblings
<a name="line_6"></a># of the top maple directory.  Plots in Maple's internal format are
<a name="line_7"></a># stored in the plots directory, with names like foo.m.  Images in
<a name="line_8"></a># jpg format are stored in the images directory, with names like
<a name="line_9"></a># foo.jpg.  There is also a global variable called pics, and a Maple
<a name="line_10"></a># representation of the plot may be stored as pics["foo"].
<a name="line_11"></a>
<a name="line_12"></a><span style="color:red">#@ pics 
</span><a name="line_13"></a>pics := table():
<a name="line_14"></a>
<a name="line_15"></a># Files in .m format cannot contain bare, unnamed expressions.
<a name="line_16"></a># Instead, they record values for certain named variables.  To save
<a name="line_17"></a># a plot P, we first assign P to the variable __pic__, then we
<a name="line_18"></a># save that variable.  When we read the file later, the required
<a name="line_19"></a># plot will be assigned to __pic__ again, and we will immediately
<a name="line_20"></a># copy it to somewhere more useful.  The housekeeping is managed
<a name="line_21"></a># by the functions save_plot and load_plot.
<a name="line_22"></a>
<a name="line_23"></a>__pic__ := NULL:
<a name="line_24"></a>make_plot := table():
<a name="line_25"></a>
<a name="line_26"></a>######################################################################
<a name="line_27"></a>
<a name="line_28"></a># This function returns true if P is a 2 or 3 dimensional plot object.
<a name="line_29"></a># This fits into a general Maple framework, so if we define
<a name="line_30"></a># a function like foo := proc(X::plot) ... end, then an error will
<a name="line_31"></a># be generated if we call foo with an argument that is not a plot
<a name="line_32"></a># object.
<a name="line_33"></a>
<a name="line_34"></a><span style="color:red">#@ `type/plot`
</span><a name="line_35"></a>`type/plot` := (P) ->
<a name="line_36"></a> (type(P,function) and
<a name="line_37"></a>  (op(0,P) = PLOT or op(0,P) = PLOT3D or op(0,P) = _PLOTARRAY));
<a name="line_38"></a>
<a name="line_39"></a>######################################################################
<a name="line_40"></a>
<a name="line_41"></a># The function list_plots() returns a list of names of plot files
<a name="line_42"></a># in the plots directory.  They are returned without the suffix .m.
<a name="line_43"></a># One can also supply a regular expression to match agianst.  For
<a name="line_44"></a># example, list_plots("^E") will return a list of files that start
<a name="line_45"></a># with E.
<a name="line_46"></a>
<a name="line_47"></a><span style="color:red">#@ list_plots
</span><a name="line_48"></a>list_plots := proc(regex)
<a name="line_49"></a> local P,Q,f;
<a name="line_50"></a> 
<a name="line_51"></a> Q := FileTools[ListDirectory](plots_dir);
<a name="line_52"></a> P := NULL;
<a name="line_53"></a> for f in Q do
<a name="line_54"></a>  if length(f) > 2 and substring(f,-2..-1) = ".m" then
<a name="line_55"></a>   P := P,substring(f,1..-3);
<a name="line_56"></a>  fi;
<a name="line_57"></a> od;
<a name="line_58"></a> 
<a name="line_59"></a> P := [P];
<a name="line_60"></a> 
<a name="line_61"></a> if nargs > 0 then
<a name="line_62"></a>  P := select(f -> StringTools[RegMatch](regex,f),P);
<a name="line_63"></a> fi;
<a name="line_64"></a> return P;
<a name="line_65"></a>end:
<a name="line_66"></a>
<a name="line_67"></a>######################################################################
<a name="line_68"></a>
<a name="line_69"></a># This function saves a plot.  It can be called in two ways.
<a name="line_70"></a># If we invoke save_plot("foo",P), then P will be saved in the file
<a name="line_71"></a># foo.m in the plots directory, and pics["foo"] will also be set
<a name="line_72"></a># equal to P.  If we just invoke save_plot("foo"), then pics["foo"]
<a name="line_73"></a># should already be set equal to a plot object, and that object
<a name="line_74"></a># will be saved in foo.m.
<a name="line_75"></a>
<a name="line_76"></a><span style="color:red">#@ save_plot 
</span><a name="line_77"></a>save_plot := proc(s::string,P_::plot) 
<a name="line_78"></a> global __pic__,pics;
<a name="line_79"></a> local P;
<a name="line_80"></a>
<a name="line_81"></a> if nargs > 1 then
<a name="line_82"></a>  pics[s] := P_;
<a name="line_83"></a>  P := P_;
<a name="line_84"></a> else 
<a name="line_85"></a>  P := pics[s];
<a name="line_86"></a>  if not type(P,plot) then
<a name="line_87"></a>   error(sprintf("pics[%s] is not a plot",s));
<a name="line_88"></a>  fi;
<a name="line_89"></a> fi;
<a name="line_90"></a> 
<a name="line_91"></a> __pic__ := pics[s];
<a name="line_92"></a> if not(type(plots_dir,string)) then
<a name="line_93"></a>  error("plots_dir is not set");
<a name="line_94"></a> fi;
<a name="line_95"></a> if not(isdir(plots_dir)) then
<a name="line_96"></a>  mkdir(plots_dir);
<a name="line_97"></a> fi;
<a name="line_98"></a> 
<a name="line_99"></a> save(__pic__,cat(plots_dir,"/",s,".m"));
<a name="line_100"></a> __pic__ := NULL;
<a name="line_101"></a>end: 
<a name="line_102"></a>
<a name="line_103"></a># This accepts an arbitrary number of arguments, and applies the
<a name="line_104"></a># single-argument form of save_plot() to each of them.
<a name="line_105"></a>
<a name="line_106"></a><span style="color:red">#@ save_plots 
</span><a name="line_107"></a>save_plots := proc()
<a name="line_108"></a> local s;
<a name="line_109"></a> for s in args do save_plot(s); od;
<a name="line_110"></a>end:
<a name="line_111"></a>
<a name="line_112"></a>######################################################################
<a name="line_113"></a>
<a name="line_114"></a># Invoking load_plot("foo") will read a plot from the file foo.m,
<a name="line_115"></a># save it as pics["foo"], and return the result.
<a name="line_116"></a>
<a name="line_117"></a><span style="color:red">#@ load_plot 
</span><a name="line_118"></a>load_plot := proc(s::string) 
<a name="line_119"></a> global __pic__,pics;
<a name="line_120"></a> read(cat(genus2_dir,"/plots/",s,".m"));
<a name="line_121"></a> pics[s] := __pic__;
<a name="line_122"></a> __pic__ := NULL;
<a name="line_123"></a> pics[s];
<a name="line_124"></a>end: 
<a name="line_125"></a>
<a name="line_126"></a># This accepts an arbitrary number of arguments, and applies 
<a name="line_127"></a># load_plot() to each of them.
<a name="line_128"></a>
<a name="line_129"></a><span style="color:red">#@ load_plots 
</span><a name="line_130"></a>load_plots := proc()
<a name="line_131"></a> local s;
<a name="line_132"></a> for s in args do load_plot(s); od;
<a name="line_133"></a> NULL;
<a name="line_134"></a>end:
<a name="line_135"></a>
<a name="line_136"></a>######################################################################
<a name="line_137"></a>
<a name="line_138"></a># Invoking save_jpg("foo") will convert pics["foo"] to a jpg image
<a name="line_139"></a># and save it in the images directory.  Note that pics["foo"] should
<a name="line_140"></a># have been set equal to a plot object before using this function.
<a name="line_141"></a>
<a name="line_142"></a><span style="color:red">#@ save_jpg 
</span><a name="line_143"></a>save_jpg := proc(s::string,w_,h_) 
<a name="line_144"></a> local P,old_dir,w,h;
<a name="line_145"></a>
<a name="line_146"></a> P := pics[s];
<a name="line_147"></a> if not type(P,plot) then
<a name="line_148"></a>  error(sprintf("pics[%s] is not a plot",s));
<a name="line_149"></a> fi;
<a name="line_150"></a>
<a name="line_151"></a> w := `if`(nargs>1,w_,1000);
<a name="line_152"></a> h := `if`(nargs>2,h_,1000);
<a name="line_153"></a>
<a name="line_154"></a> if not(type(images_dir,string)) then
<a name="line_155"></a>  error("images_dir is not set");
<a name="line_156"></a> fi;
<a name="line_157"></a> if not(isdir(images_dir)) then
<a name="line_158"></a>  mkdir(images_dir);
<a name="line_159"></a> fi;
<a name="line_160"></a>
<a name="line_161"></a> old_dir := currentdir(images_dir);
<a name="line_162"></a> plotsetup(jpeg,
<a name="line_163"></a>  plotoutput=cat(s,".jpg"),
<a name="line_164"></a>  plotoptions=sprintf("height=%d,width=%d",h,w)
<a name="line_165"></a> );
<a name="line_166"></a> print(P);
<a name="line_167"></a> plotsetup(default);
<a name="line_168"></a> currentdir(old_dir);
<a name="line_169"></a>end: 
<a name="line_170"></a>
<a name="line_171"></a>save_jpgs := proc()
<a name="line_172"></a> local s;
<a name="line_173"></a> for s in args do save_jpg(s); od;
<a name="line_174"></a>end:
<a name="line_175"></a>
<a name="line_176"></a><span style="color:red">#@ save_thumbnail
</span><a name="line_177"></a>save_thumbnail := proc(s::string) 
<a name="line_178"></a> local P,old_dir,w,h;
<a name="line_179"></a>
<a name="line_180"></a> P := pics[s];
<a name="line_181"></a> if not type(P,plot) then
<a name="line_182"></a>  error(sprintf("pics[%s] is not a plot",s));
<a name="line_183"></a> fi;
<a name="line_184"></a>
<a name="line_185"></a> if not(type(plots_dir,string)) then
<a name="line_186"></a>  error("plots_dir is not set");
<a name="line_187"></a> fi;
<a name="line_188"></a> if not(isdir(plots_dir)) then
<a name="line_189"></a>  mkdir(plots_dir);
<a name="line_190"></a> fi;
<a name="line_191"></a> if not(isdir(thumbs_dir)) then
<a name="line_192"></a>  mkdir(thumbs_dir);
<a name="line_193"></a> fi;
<a name="line_194"></a> 
<a name="line_195"></a> old_dir := currentdir(thumbs_dir);
<a name="line_196"></a> plotsetup(jpeg,
<a name="line_197"></a>  plotoutput=cat(s,".jpg"),
<a name="line_198"></a>  plotoptions="height=100,width=100"
<a name="line_199"></a> );
<a name="line_200"></a> print(P);
<a name="line_201"></a> plotsetup(default);
<a name="line_202"></a> currentdir(old_dir);
<a name="line_203"></a>end:
<a name="line_204"></a>
<a name="line_205"></a><span style="color:red">#@ make_all_thumbnails
</span><a name="line_206"></a>make_all_thumbnails := proc()
<a name="line_207"></a> local old_pics,labels,label,P;
<a name="line_208"></a> global pics;
<a name="line_209"></a> 
<a name="line_210"></a> # To avoid using an enormous amount of memory, we do not retain
<a name="line_211"></a> # all plot objects in the pics table.
<a name="line_212"></a> old_pics := eval(pics):
<a name="line_213"></a> 
<a name="line_214"></a> labels := list_plots():
<a name="line_215"></a> for label in labels do
<a name="line_216"></a>  printf("%s\n",label);
<a name="line_217"></a>  pics := table():
<a name="line_218"></a>  load_plot(label);
<a name="line_219"></a>  P := pics[label];
<a name="line_220"></a>  if not(type(P,plot)) then
<a name="line_221"></a>   P := display(P);
<a name="line_222"></a>   pics[label] := P;
<a name="line_223"></a>  fi;
<a name="line_224"></a>  if type(P,plot) then
<a name="line_225"></a>   save_thumbnail(label);
<a name="line_226"></a>  fi;
<a name="line_227"></a> od:
<a name="line_228"></a> pics := eval(old_pics);
<a name="line_229"></a> NULL;
<a name="line_230"></a>end:
<a name="line_231"></a>
<a name="line_232"></a>######################################################################
<a name="line_233"></a># This just makes the syntax for plotting plane curves more
<a name="line_234"></a># compatible with the syntax for plotting space curves.
<a name="line_235"></a>
<a name="line_236"></a><span style="color:red">#@ planecurve 
</span><a name="line_237"></a>planecurve := (xy,R) -> plot([op(xy),R],args[3..-1]);
<a name="line_238"></a>
<a name="line_239"></a><span style="color:red">#@ planetext 
</span><a name="line_240"></a>planetext := (xy, t) -> textplot([op(xy),t],args[3..-1]);
<a name="line_241"></a>
<a name="line_242"></a><span style="color:red">#@ spacetext 
</span><a name="line_243"></a>spacetext := (xyz,t) -> textplot3d([op(xyz),t],args[3..-1]);
<a name="line_244"></a>
<a name="line_245"></a>######################################################################
<a name="line_246"></a>
<a name="line_247"></a># Some convenience functions for plotting complex numbers as 
<a name="line_248"></a># points in R^2.  Note that these functions will work with
<a name="line_249"></a># expressions involving the symbolic constants a_H, a_P and a_E.
<a name="line_250"></a># They will be replaced by the numerical values a_H0, a_P0 and a_E0.
<a name="line_251"></a>
<a name="line_252"></a><span style="color:red">#@ cpolygon 
</span><a name="line_253"></a>cpolygon :=
<a name="line_254"></a> proc(points)
<a name="line_255"></a>  polygon(map(z -> evalf(subs({a_H=a_H0,a_P=a_P0,a_E=a_E0},[Re(z),Im(z)])),points),args[2..-1]);
<a name="line_256"></a> end:
<a name="line_257"></a>
<a name="line_258"></a><span style="color:red">#@ ccurve 
</span><a name="line_259"></a>ccurve :=
<a name="line_260"></a> proc(points)
<a name="line_261"></a>  curve(map(z -> evalf(subs({a_H=a_H0,a_P=a_P0,a_E=a_E0},[Re(z),Im(z)])),points),args[2..-1]);
<a name="line_262"></a> end:
<a name="line_263"></a>
<a name="line_264"></a><span style="color:red">#@ cpoint 
</span><a name="line_265"></a>cpoint :=
<a name="line_266"></a> proc(z)
<a name="line_267"></a>  point(evalf(subs({a_H=a_H0,a_P=a_P0,a_E=a_E0},[Re(z),Im(z)])),args[2..-1]);
<a name="line_268"></a> end:
<a name="line_269"></a>
<a name="line_270"></a><span style="color:red">#@ cline
</span><a name="line_271"></a>cline :=
<a name="line_272"></a> proc(z,w)
<a name="line_273"></a>  line(evalf(subs({a_H=a_H0,a_P=a_P0,a_E=a_E0},[Re(z),Im(z)])),
<a name="line_274"></a>       evalf(subs({a_H=a_H0,a_P=a_P0,a_E=a_E0},[Re(w),Im(w)])),
<a name="line_275"></a>       args[3..-1]);
<a name="line_276"></a> end:
<a name="line_277"></a>
<a name="line_278"></a><span style="color:red">#@ ctext 
</span><a name="line_279"></a>ctext := 
<a name="line_280"></a> proc(z,t)
<a name="line_281"></a>  local x0,y0,z0;
<a name="line_282"></a>  z0 := evalf(subs({a_H=a_H0,a_P=a_P0,a_E=a_E0},z));
<a name="line_283"></a>  x0 := Re(z0);
<a name="line_284"></a>  y0 := Im(z0);
<a name="line_285"></a>  textplot([x0,y0,t],args[3..-1]);
<a name="line_286"></a> end:
<a name="line_287"></a>
<a name="line_288"></a><span style="color:red">#@ cplot 
</span><a name="line_289"></a>cplot := proc(c,R) plot([Re(c),Im(c),R],args[3..-1]); end:
<a name="line_290"></a>
<a name="line_291"></a>######################################################################
<a name="line_292"></a>
<a name="line_293"></a># sphere_arc(a,b,col) returns a plot element that can be used as an 
<a name="line_294"></a># argument to the display() function.  It represents an arc of the
<a name="line_295"></a># sphere joining a to b, in the colour specified by the argument col.
<a name="line_296"></a># It is crude and only works well for short arcs.
<a name="line_297"></a>
<a name="line_298"></a><span style="color:red">#@ sphere_arc 
</span><a name="line_299"></a>sphere_arc := proc(a::RR0_3,b::RR0_3,col)
<a name="line_300"></a> local c,i,j,r;
<a name="line_301"></a> for i from 0 to 10 do
<a name="line_302"></a>  c[i] := [seq(0.1* (i * a[j] + (10-i) * b[j]),j=1..3)]:
<a name="line_303"></a>  r := sqrt(add(c[i][j]^2,j=1..3));
<a name="line_304"></a>  c[i] := [seq(c[i][j]/r,j=1..3)];
<a name="line_305"></a> od:
<a name="line_306"></a> curve([seq(c[i],i=0..10)],colour=col);
<a name="line_307"></a>end:
<a name="line_308"></a>
<a name="line_309"></a>
<a name="line_310"></a>######################################################################
<a name="line_311"></a>
<a name="line_312"></a># torus_box_plot(c) expects c to be a function R -> S^1 x S^1 which
<a name="line_313"></a># is periodic of period 2 pi.  It returns a plot of c with the torus
<a name="line_314"></a># unwrapped to a flat square, and it deals with discontinuities in a
<a name="line_315"></a># way that is adequate for the cases that we need.  One can pass
<a name="line_316"></a># additional arguments, such as torus_box_plot(c,colour = red).
<a name="line_317"></a>
<a name="line_318"></a>torus_box_plot_aux := proc(c,s)
<a name="line_319"></a> local a,eps;
<a name="line_320"></a>
<a name="line_321"></a> eps := 0.0001;
<a name="line_322"></a> a := map(argument,c)/~Pi +~ (2 *~ s);
<a name="line_323"></a> display(
<a name="line_324"></a>  seq(planecurve(a,t=j*Pi/2+eps..(j+1)*Pi/2-eps,args[3..-1]),j=0..4),
<a name="line_325"></a>  axes=none,scaling=constrained
<a name="line_326"></a> ):
<a name="line_327"></a>end:
<a name="line_328"></a>
<a name="line_329"></a><span style="color:red">#@ torus_box_plot 
</span><a name="line_330"></a>torus_box_plot := proc(c)
<a name="line_331"></a> if c[1] = -1 then
<a name="line_332"></a>  display(torus_box_plot_aux(c,[ 0, 0],args[2..-1]),
<a name="line_333"></a>          torus_box_plot_aux(c,[-1, 0],args[2..-1]));
<a name="line_334"></a> elif c[2] = -1 then
<a name="line_335"></a>  display(torus_box_plot_aux(c,[ 0, 0],args[2..-1]),
<a name="line_336"></a>          torus_box_plot_aux(c,[ 0,-1],args[2..-1]));
<a name="line_337"></a> else
<a name="line_338"></a>  display(torus_box_plot_aux(c,[ 0, 0],args[2..-1]));
<a name="line_339"></a> fi:
<a name="line_340"></a>end:
<a name="line_341"></a>
<a name="line_342"></a>######################################################################
<a name="line_343"></a>
<a name="line_344"></a>triangle_axes := proc(z_range := -2..2)
<a name="line_345"></a> local tc,z_min,z_max;
<a name="line_346"></a> 
<a name="line_347"></a> tc[1] := evalf([op(triangle_proj([1,0,0])),0]):
<a name="line_348"></a> tc[2] := evalf([op(triangle_proj([0,1,0])),0]):
<a name="line_349"></a> tc[3] := evalf([op(triangle_proj([0,0,1])),0]):
<a name="line_350"></a> z_min := op(1,z_range);
<a name="line_351"></a> z_max := op(2,z_range);
<a name="line_352"></a> 
<a name="line_353"></a> display(
<a name="line_354"></a>  line(tc[1],tc[2],colour=black),
<a name="line_355"></a>  line(tc[2],tc[3],colour=black),
<a name="line_356"></a>  line(tc[3],tc[1],colour=black),
<a name="line_357"></a>  line(tc[1],[0,0,0],colour=black),
<a name="line_358"></a>  line(tc[2],[0,0,0],colour=black),
<a name="line_359"></a>  line(tc[3],[0,0,0],colour=black),
<a name="line_360"></a>  line([0,0,z_min],[0,0,z_max],colour=black),
<a name="line_361"></a>  scaling=constrained,axes=none
<a name="line_362"></a> ):
<a name="line_363"></a>end:
<a name="line_364"></a>
<a name="line_365"></a>######################################################################
<a name="line_366"></a>
<a name="line_367"></a><span style="color:red">#@ make_wireframe_plots
</span><a name="line_368"></a>make_wireframe_plots := proc()
<a name="line_369"></a> global pics;
<a name="line_370"></a>
<a name="line_371"></a> pics["torus_wireframe"] := 
<a name="line_372"></a>  plot3d(TA_to_R3([t,u]),t=0..2*Pi,u=0..2*Pi,
<a name="line_373"></a>           colour=gray,style=wireframe,scaling=constrained,axes=none);
<a name="line_374"></a>
<a name="line_375"></a> pics["sphere_wireframe"] := 
<a name="line_376"></a>  display(sphere([0,0,0],1,colour=grey,style=wireframe),axes=none):
<a name="line_377"></a>
<a name="line_378"></a> save_plot("torus_wireframe");
<a name="line_379"></a> save_plot("sphere_wireframe");
<a name="line_380"></a> save_jpg("torus_wireframe");
<a name="line_381"></a> save_jpg("sphere_wireframe");
<a name="line_382"></a> NULL;
<a name="line_383"></a>end:
  </pre>
 </body>
</html>
    