<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># This is a generic function for drawing a picture of the image of a map
<a name="line_2"></a># p : EX -> R^2.  The function p is given as the first argument.
<a name="line_3"></a># The last argument filter_ can be omitted or set to "F4" or "F16".
<a name="line_4"></a># If it is set, then only p(F4) or p(F16) will be drawn rather than
<a name="line_5"></a># the whole of p(EX).  
<a name="line_6"></a>#
<a name="line_7"></a># The argument v_label_align should be a table indexed by a subset of
<a name="line_8"></a># {0,...,13} specifying the alignment of vertex labels relative to the
<a name="line_9"></a># point marking the vertex itself.  The argument c_label_param should be
<a name="line_10"></a># a table indexed by a subset of {0,...,8}.  If the i'th entry is t0
<a name="line_11"></a># then a label for c[i] will be placed near to p(c[i](t0)), with 
<a name="line_12"></a># alignment specified by c_label_align[i].
<a name="line_13"></a>#
<a name="line_14"></a># There is some logic to provide readable labels in the case where 
<a name="line_15"></a># several different vertices have the same image under p.
<a name="line_16"></a>
<a name="line_17"></a><span style="color:red">#@ plane_proj_plot 
</span><a name="line_18"></a>plane_proj_plot := proc(
<a name="line_19"></a>    p,v_label_align,c_label_param,c_label_align,filter_
<a name="line_20"></a> )
<a name="line_21"></a> local filter,II,JJ,CL,i,j,vv,vla,vlm,cc,cla,t0,a0,x0,P;
<a name="line_22"></a>
<a name="line_23"></a> if nargs < 5 then filter := NULL; else filter := filter_; fi;
<a name="line_24"></a>
<a name="line_25"></a> if filter = "F16" then
<a name="line_26"></a>  II := F16_vertices;
<a name="line_27"></a>  JJ := select(j -> F16_curve_limits[j] <> NULL,{seq(j,j=0..8)});
<a name="line_28"></a>  CL := F16_curve_limits;
<a name="line_29"></a> elif filter = "F4" then
<a name="line_30"></a>  II := F4_vertices;
<a name="line_31"></a>  JJ := select(j -> F4_curve_limits[j] <> NULL,{seq(j,j=0..8)});
<a name="line_32"></a>  CL := F4_curve_limits;
<a name="line_33"></a> else
<a name="line_34"></a>  II := {seq(i,i=0..13)};
<a name="line_35"></a>  JJ := {seq(j,j=0..8)};
<a name="line_36"></a>  CL := table([seq(i = 0..2*Pi,i=0..8)]);
<a name="line_37"></a> fi;
<a name="line_38"></a>
<a name="line_39"></a> vv := table(); vla := table(); vlm := table();
<a name="line_40"></a>
<a name="line_41"></a> for i in II do 
<a name="line_42"></a>  vv[i] := simplify(subs(a_E=a_E0,p(v_E0[i])));
<a name="line_43"></a>  vla[vv[i]] := 'below';
<a name="line_44"></a>  if type(vlm[vv[i]],list) then
<a name="line_45"></a>   vlm[vv[i]] := [op(vlm[vv[i]]),i];
<a name="line_46"></a>  else
<a name="line_47"></a>   vlm[vv[i]] := [i];
<a name="line_48"></a>  fi;
<a name="line_49"></a> od;
<a name="line_50"></a>
<a name="line_51"></a> cc := table(); cla := table();
<a name="line_52"></a>
<a name="line_53"></a> for i in JJ do
<a name="line_54"></a>  cla[i] := 'below'; 
<a name="line_55"></a>  cc[i] := unapply(simplify(subs(a_E=a_E0,p(c_E0[i](t)))),t);
<a name="line_56"></a> od;
<a name="line_57"></a>
<a name="line_58"></a> for i in map(op,[indices(v_label_align)]) do 
<a name="line_59"></a>  vla[vv[i]] :=  v_label_align[i];
<a name="line_60"></a> od;
<a name="line_61"></a>
<a name="line_62"></a> for i in map(op,[indices(c_label_align)]) do 
<a name="line_63"></a>  cla[i] :=  c_label_align[i];
<a name="line_64"></a> od;
<a name="line_65"></a>
<a name="line_66"></a> P := seq(plot([op(cc[i](t)),t=CL[i]],colour=c_colour[i]),i in JJ);
<a name="line_67"></a> P := P,seq(point(vv[i]),i in II);
<a name="line_68"></a> P := P,  seq(
<a name="line_69"></a>    textplot([op(evalf(op(ii))),sprintf("%Q",op(vlm[op(ii)]))],'align'=vla[op(ii)]),
<a name="line_70"></a>    ii in indices(vlm)
<a name="line_71"></a>  );
<a name="line_72"></a> for i in map(op,[indices(c_label_param)]) do
<a name="line_73"></a>  if member(i,JJ) then 
<a name="line_74"></a>   t0 := c_label_param[i];
<a name="line_75"></a>   a0 := cla[i];
<a name="line_76"></a>   x0 := evalf(subs(a_E=a_E0,cc[i](t0)));
<a name="line_77"></a>
<a name="line_78"></a>   P := P,textplot([op(x0),i],'align'=a0,colour=red);
<a name="line_79"></a>  fi;
<a name="line_80"></a> od;
<a name="line_81"></a>
<a name="line_82"></a> return display(P,scaling=constrained,axes=none);
<a name="line_83"></a>end:
<a name="line_84"></a>
<a name="line_85"></a>######################################################################
<a name="line_86"></a>
<a name="line_87"></a># This function is similar to plane_proj_plot() except that it generates
<a name="line_88"></a># tikz code for inclusion in the latex file rather than a Maple plot.
<a name="line_89"></a>
<a name="line_90"></a><span style="color:red">#@ plane_proj_tikz 
</span><a name="line_91"></a>plane_proj_tikz := proc(
<a name="line_92"></a>    p,v_label_align,c_label_param,c_label_align
<a name="line_93"></a> )
<a name="line_94"></a> local filter,II,JJ,CL,i,j,vv,vla,vlm,cc,cla,t0,t1,t2,a0,a1,a2,a3,x0,s,s0,a,ls,comma,
<a name="line_95"></a>       scale,point_size,num_points,label_size,centred,raw,ranges,arrows,extra;
<a name="line_96"></a>
<a name="line_97"></a> filter := NULL;
<a name="line_98"></a> scale := 4;
<a name="line_99"></a> point_size := 0.02;
<a name="line_100"></a> num_points := 10;
<a name="line_101"></a> raw := false;
<a name="line_102"></a> centred := false;
<a name="line_103"></a> ranges := false;
<a name="line_104"></a> label_size := "";
<a name="line_105"></a> arrows := [];
<a name="line_106"></a> extra := "";
<a name="line_107"></a>
<a name="line_108"></a> for a in args[5..-1] do
<a name="line_109"></a>  if a = "F4" or a = "F16" then
<a name="line_110"></a>   filter := a;
<a name="line_111"></a>  elif type(a,`=`) then
<a name="line_112"></a>   if lhs(a) = "scale"      then scale      := rhs(a); fi;
<a name="line_113"></a>   if lhs(a) = "point_size" then point_size := rhs(a); fi;
<a name="line_114"></a>   if lhs(a) = "label_size" then label_size := rhs(a); fi;
<a name="line_115"></a>   if lhs(a) = "num_points" then num_points := rhs(a); fi;
<a name="line_116"></a>   if lhs(a) = "centred"    then centred    := rhs(a); fi;
<a name="line_117"></a>   if lhs(a) = "raw"        then raw        := rhs(a); fi;
<a name="line_118"></a>   if lhs(a) = "ranges"     then ranges     := rhs(a); fi;
<a name="line_119"></a>   if lhs(a) = "arrows"     then arrows     := rhs(a); fi;
<a name="line_120"></a>   if lhs(a) = "extra"      then extra      := rhs(a); fi;
<a name="line_121"></a>  fi;
<a name="line_122"></a> od;
<a name="line_123"></a>
<a name="line_124"></a> if label_size = "subscript" then
<a name="line_125"></a>  ls := "\\ss ";
<a name="line_126"></a> elif label_size = "subsubscript" then
<a name="line_127"></a>  ls := "\\sss ";
<a name="line_128"></a> else
<a name="line_129"></a>  ls := "";
<a name="line_130"></a> fi;
<a name="line_131"></a>
<a name="line_132"></a> if filter = "F16" then
<a name="line_133"></a>  II := F16_vertices;
<a name="line_134"></a>  JJ := select(j -> F16_curve_limits[j] <> NULL,{seq(j,j=0..8)});
<a name="line_135"></a>  CL := F16_curve_limits;
<a name="line_136"></a> elif filter = "F4" then
<a name="line_137"></a>  II := F4_vertices;
<a name="line_138"></a>  JJ := select(j -> F4_curve_limits[j] <> NULL,{seq(j,j=0..8)});
<a name="line_139"></a>  CL := F4_curve_limits;
<a name="line_140"></a> else
<a name="line_141"></a>  II := {seq(i,i=0..13)};
<a name="line_142"></a>  JJ := {seq(j,j=0..8)};
<a name="line_143"></a>  CL := table([seq(i = 0..2*Pi,i=0..8)]);
<a name="line_144"></a> fi;
<a name="line_145"></a>
<a name="line_146"></a> vv := table(); vla := table(); vlm := table();
<a name="line_147"></a>
<a name="line_148"></a> for i in II do 
<a name="line_149"></a>  vv[i] := simplify(subs(a_E=a_E0,p(v_E0[i])));
<a name="line_150"></a>  vla[vv[i]] := 'below';
<a name="line_151"></a>  if type(vlm[vv[i]],list) then
<a name="line_152"></a>   vlm[vv[i]] := [op(vlm[vv[i]]),i];
<a name="line_153"></a>  else
<a name="line_154"></a>   vlm[vv[i]] := [i];
<a name="line_155"></a>  fi;
<a name="line_156"></a> od;
<a name="line_157"></a>
<a name="line_158"></a> cc := table(); cla := table();
<a name="line_159"></a>
<a name="line_160"></a> for i in JJ do
<a name="line_161"></a>  cla[i] := 'below'; 
<a name="line_162"></a>  cc[i] := unapply(simplify(subs(a_E=a_E0,p(c_E0[i](t)))),t);
<a name="line_163"></a> od;
<a name="line_164"></a>
<a name="line_165"></a> for i in map(op,[indices(v_label_align)]) do 
<a name="line_166"></a>  vla[vv[i]] :=  v_label_align[i];
<a name="line_167"></a> od;
<a name="line_168"></a>
<a name="line_169"></a> for i in map(op,[indices(c_label_align)]) do 
<a name="line_170"></a>  cla[i] :=  c_label_align[i];
<a name="line_171"></a> od;
<a name="line_172"></a>
<a name="line_173"></a> if raw = true then 
<a name="line_174"></a>  s := "";
<a name="line_175"></a> else 
<a name="line_176"></a>  s := sprintf("\\begin{tikzpicture}[scale=%A]\n",scale);
<a name="line_177"></a> fi;
<a name="line_178"></a>
<a name="line_179"></a> for i in JJ do
<a name="line_180"></a>  s := cat(s,tikz_plot(cc[i](t),CL[i],num_points,c_colour[i]));
<a name="line_181"></a> od;
<a name="line_182"></a> for a in arrows do
<a name="line_183"></a>  i := op(1,a);
<a name="line_184"></a>  t0 := evalf(op(2,a));
<a name="line_185"></a>  a0 := evalf(cc[i](t0));
<a name="line_186"></a>  a1 := evalf(cc[i](t0+0.01));
<a name="line_187"></a>  s := cat(s,
<a name="line_188"></a>   sprintf(" \\draw[%s,arrows={-angle 90}] (%.4f,%.4f) -- (%.4f,%.4f);\n",
<a name="line_189"></a>           c_colour[i],op(a0),op(a1)));
<a name="line_190"></a> od;
<a name="line_191"></a> for i in II do 
<a name="line_192"></a>  s := cat(s,tikz_point(vv[i],point_size));
<a name="line_193"></a> od;
<a name="line_194"></a> for i in map(op,[indices(vlm)]) do
<a name="line_195"></a>  s0 := ls;
<a name="line_196"></a>  comma := "";
<a name="line_197"></a>  for j in vlm[i] do
<a name="line_198"></a>   s0 := cat(s0,comma,sprintf("v_{%A}",j));
<a name="line_199"></a>   comma := ",";
<a name="line_200"></a>  od;
<a name="line_201"></a>  s := cat(s,tikz_label(i,s0,vla[i]));
<a name="line_202"></a> od;
<a name="line_203"></a> for i in map(op,[indices(c_label_param)]) do
<a name="line_204"></a>  if member(i,JJ) then 
<a name="line_205"></a>   t0 := c_label_param[i];
<a name="line_206"></a>   a0 := cla[i];
<a name="line_207"></a>   x0 := evalf(subs(a_E=a_E0,cc[i](t0)));
<a name="line_208"></a>   s0 := sprintf("%sc_{%d}",ls,i);
<a name="line_209"></a>   if ranges = true then
<a name="line_210"></a>    s0 := sprintf("%s(%s\\dotsb%s)",s0,
<a name="line_211"></a>                  angle_latex[op(1,CL[i])],angle_latex[op(2,CL[i])]);
<a name="line_212"></a>   fi;
<a name="line_213"></a>   s := cat(s,tikz_label(x0,s0,a0));
<a name="line_214"></a>  fi;
<a name="line_215"></a> od;
<a name="line_216"></a>
<a name="line_217"></a> s := cat(s,extra);
<a name="line_218"></a>
<a name="line_219"></a> if raw <> true then
<a name="line_220"></a>  s := cat(s,"\\end{tikzpicture}\n");
<a name="line_221"></a> fi;
<a name="line_222"></a>
<a name="line_223"></a> if centred = true then
<a name="line_224"></a>  s := cat("\\begin{center}\n",s,"\\end{center}\n");
<a name="line_225"></a> fi;
<a name="line_226"></a>
<a name="line_227"></a> return(s);
<a name="line_228"></a>end:
<a name="line_229"></a>
<a name="line_230"></a>######################################################################
<a name="line_231"></a>
<a name="line_232"></a><span style="color:red">#@ surface_plot 
</span><a name="line_233"></a>surface_plot := proc(f,{M::integer := 0,
<a name="line_234"></a>                        plot_style := patchnogrid,
<a name="line_235"></a>                        with_curves := false,
<a name="line_236"></a>                        H := G16})
<a name="line_237"></a> local i,j,T,N,P,SDI,CP,opt;
<a name="line_238"></a>
<a name="line_239"></a> require_square_diffeo_E0_inverse();
<a name="line_240"></a>
<a name="line_241"></a> N := square_diffeo_E0_inverse_order;
<a name="line_242"></a> if  M > 0 and type(N/M,integer) then
<a name="line_243"></a>  N := M;
<a name="line_244"></a> fi;
<a name="line_245"></a> 
<a name="line_246"></a> SDI := eval(square_diffeo_E0_inverse_table):
<a name="line_247"></a> P := table():
<a name="line_248"></a> for T in H do
<a name="line_249"></a>  for i from 0 to N do
<a name="line_250"></a>   for j from 0 to N do
<a name="line_251"></a>    P[T,i,j] := evalf(f(act_R4[T](SDI[i/N,j/N]))):
<a name="line_252"></a>   od:
<a name="line_253"></a>  od:
<a name="line_254"></a> od:
<a name="line_255"></a>
<a name="line_256"></a> if with_curves then
<a name="line_257"></a>  CP := seq(spacecurve(f(c_E0[k](t)),t=0..2*Pi,colour=c_colour[k]),k=0..8);
<a name="line_258"></a> else
<a name="line_259"></a>  CP := NULL;
<a name="line_260"></a> fi;
<a name="line_261"></a>
<a name="line_262"></a> if plot_style = wireframe then
<a name="line_263"></a>  opt := style=wireframe,colour=gray;
<a name="line_264"></a> else
<a name="line_265"></a>  opt := style=plot_style;
<a name="line_266"></a> fi;
<a name="line_267"></a>
<a name="line_268"></a> display(
<a name="line_269"></a>  seq(seq(seq(
<a name="line_270"></a>   polygon([P[T,i,j],P[T,i,j+1],P[T,i+1,j+1]],opt),
<a name="line_271"></a>    i=0..N-1),j=0..N-1),T in H),
<a name="line_272"></a>  seq(seq(seq(
<a name="line_273"></a>   polygon([P[T,i,j],P[T,i+1,j],P[T,i+1,j+1]],opt),
<a name="line_274"></a>    i=0..N-1),j=0..N-1),T in H),
<a name="line_275"></a>  CP,
<a name="line_276"></a>  scaling=constrained,axes=none
<a name="line_277"></a> );
<a name="line_278"></a>end:
<a name="line_279"></a>
<a name="line_280"></a>######################################################################
<a name="line_281"></a>
<a name="line_282"></a><span style="color:red">#@ make_c_E_plots 
</span><a name="line_283"></a>make_c_E_plots := proc()
<a name="line_284"></a> local i;
<a name="line_285"></a> global pics,c_E_plot;
<a name="line_286"></a>
<a name="line_287"></a> for i from 0 to 16 do 
<a name="line_288"></a>  c_E_plot[i] :=
<a name="line_289"></a>    spacecurve(stereo(c_E1[i](t)),t=0..2*Pi,numpoints=200,colour = c_colour[i],axes = none);
<a name="line_290"></a>  pics[sprintf("c_E[%d]",i)] := c_E_plot[i];
<a name="line_291"></a> od:
<a name="line_292"></a> save_plots(seq(sprintf("c_E[%d]",i),i=0..8));
<a name="line_293"></a> save_jpgs( seq(sprintf("c_E[%d]",i),i=0..8));
<a name="line_294"></a>
<a name="line_295"></a> pics["curves_E"] :=
<a name="line_296"></a>  display(seq(c_E_plot[i],i=0..8),scaling=constrained,axes=none);
<a name="line_297"></a> save_plot("curves_E");
<a name="line_298"></a> save_jpg( "curves_E");
<a name="line_299"></a>
<a name="line_300"></a> pics["extra_curves_E"] :=
<a name="line_301"></a>  display(seq(c_E_plot[i],i=9..16),scaling=constrained,axes=none);
<a name="line_302"></a> save_plot("extra_curves_E");
<a name="line_303"></a> save_jpg( "extra_curves_E");
<a name="line_304"></a>
<a name="line_305"></a> pics["all_curves_E"] := 
<a name="line_306"></a>  display(pics["curves_E"],pics["extra_curves_E"]);
<a name="line_307"></a> save_plot("all_curves_E");
<a name="line_308"></a> save_jpg( "all_curves_E");
<a name="line_309"></a>end:
<a name="line_310"></a>
<a name="line_311"></a>load_c_E_plots := proc()
<a name="line_312"></a> load_plots(seq(sprintf("c_E[%d]",i),i=0..8),"curves_E");
<a name="line_313"></a>end:
<a name="line_314"></a>
<a name="line_315"></a>######################################################################
<a name="line_316"></a>
<a name="line_317"></a><span style="color:red">#@ make_E_plots 
</span><a name="line_318"></a>make_E_plots := proc()
<a name="line_319"></a> global pics;
<a name="line_320"></a> pics["EX"]  :=
<a name="line_321"></a>  surface_plot(stereo,M = 8);
<a name="line_322"></a> pics["EX_wireframe"]  :=
<a name="line_323"></a>  surface_plot(stereo,M = 12,plot_style=wireframe);
<a name="line_324"></a>
<a name="line_325"></a> save_plots("EX","EX_wireframe");
<a name="line_326"></a> save_jpgs( "EX","EX_wireframe");
<a name="line_327"></a>
<a name="line_328"></a> pics["EX_with_curves"] := display(
<a name="line_329"></a>  surface_plot(stereo,M = 8,with_curves = true),
<a name="line_330"></a>  scaling=constrained,orientation=[65,80],axes = none
<a name="line_331"></a> ): 
<a name="line_332"></a> save_plot("EX_with_curves"): 
<a name="line_333"></a> save_jpg( "EX_with_curves"): 
<a name="line_334"></a>
<a name="line_335"></a> NULL;
<a name="line_336"></a>end:
<a name="line_337"></a>
<a name="line_338"></a>load_E_plots := () ->
<a name="line_339"></a> load_plots("EX","EX_wireframe","EX_with_curves");
<a name="line_340"></a>
<a name="line_341"></a>######################################################################
<a name="line_342"></a>
<a name="line_343"></a><span style="color:red">#@ make_E_owl_plots 
</span><a name="line_344"></a>make_E_owl_plots := proc()
<a name="line_345"></a> global pics;
<a name="line_346"></a> pics["EX_owl"]  :=
<a name="line_347"></a>  surface_plot(owl_proj,M = 8);
<a name="line_348"></a> pics["EX_owl_wireframe"]  :=
<a name="line_349"></a>  surface_plot(owl_proj,M = 12,plot_style=wireframe);
<a name="line_350"></a>
<a name="line_351"></a> save_plots("EX_owl","EX_owl_wireframe");
<a name="line_352"></a> save_jpgs( "EX_owl","EX_owl_wireframe");
<a name="line_353"></a>
<a name="line_354"></a> pics["EX_owl_with_curves"] := display(
<a name="line_355"></a>  surface_plot(owl_proj,M = 8,with_curves = true),
<a name="line_356"></a>  scaling=constrained,axes = none
<a name="line_357"></a> ): 
<a name="line_358"></a> save_plot("EX_owl_with_curves"): 
<a name="line_359"></a> save_jpg("EX_owl_with_curves"): 
<a name="line_360"></a>
<a name="line_361"></a> NULL;
<a name="line_362"></a>end:
<a name="line_363"></a>
<a name="line_364"></a>load_E_owl_plots := () ->
<a name="line_365"></a> load_plots("EX_owl","EX_owl_wireframe","EX_owl_with_curves");
<a name="line_366"></a>
<a name="line_367"></a>############################################################
<a name="line_368"></a>
<a name="line_369"></a><span style="color:red">#@ make_Omega_plots
</span><a name="line_370"></a>make_Omega_plots := proc()
<a name="line_371"></a> global pics;
<a name="line_372"></a>
<a name="line_373"></a> pics["Omega"] := display(
<a name="line_374"></a>  spacecurve(stereo(omega1[1](t)),t=0.8 .. 5.48,color=red,thickness=3),
<a name="line_375"></a>  spacecurve(stereo(omega1[2](t)),t=0.8 .. 5.48,color=red,thickness=3),
<a name="line_376"></a>  sphere([0,0,1/2],0.05,color=black),
<a name="line_377"></a>  sphere([0,0,-1/2],0.05,color=black),
<a name="line_378"></a>  spacecurve(stereo(-~ omega1[1](t)),t=0..2*Pi,color=blue,thickness=3),
<a name="line_379"></a>  spacecurve(stereo(-~ omega1[2](t)),t=0..2*Pi,color=blue,thickness=3),
<a name="line_380"></a>  line([-3,0,0],[3,0,0],color=black,thickness=3),
<a name="line_381"></a>  line([0,-3,0],[0,3,0],color=black,thickness=3),
<a name="line_382"></a>  line([0,0,-3],[0,0,3],color=black,thickness=3),
<a name="line_383"></a>  textplot3d([3.2,0,0,"x"]),
<a name="line_384"></a>  textplot3d([0,3.2,0,"y"]),
<a name="line_385"></a>  textplot3d([0,0,3.2,"z"]),
<a name="line_386"></a>  view=[-3.5..3.5,-3.5..3.5,-3.5..3.5],orientation=[40,65],axes=none
<a name="line_387"></a> ):
<a name="line_388"></a>
<a name="line_389"></a> save_plot("Omega"):
<a name="line_390"></a> save_jpg("Omega"):
<a name="line_391"></a>
<a name="line_392"></a> pics["XOmega"] := display(
<a name="line_393"></a>  spacecurve(stereo(omega1[1](t)),t=0.8 .. 5.48,color=red,thickness=3),
<a name="line_394"></a>  spacecurve(stereo(omega1[2](t)),t=0.8 .. 5.48,color=red,thickness=3),
<a name="line_395"></a>  spacecurve(stereo(-~ omega1[1](t)),t=0..2*Pi,color=blue,thickness=3),
<a name="line_396"></a>  spacecurve(stereo(-~ omega1[2](t)),t=0..2*Pi,color=blue,thickness=3),
<a name="line_397"></a>  pics["EX_wireframe"],
<a name="line_398"></a>  view=[-3.5..3.5,-3.5..3.5,-3.5..3.5],
<a name="line_399"></a>  scaling=constrained,orientation=[65,80],axes=none
<a name="line_400"></a> ):
<a name="line_401"></a>
<a name="line_402"></a> save_plot("XOmega"):
<a name="line_403"></a> save_jpg("XOmega"):
<a name="line_404"></a>
<a name="line_405"></a> NULL;
<a name="line_406"></a>end:
<a name="line_407"></a>
<a name="line_408"></a>############################################################
<a name="line_409"></a>
<a name="line_410"></a><span style="color:red">#@ make_F4_plot
</span><a name="line_411"></a>make_F4_plot := proc()
<a name="line_412"></a> global pics;
<a name="line_413"></a>
<a name="line_414"></a> pics["F4"] :=
<a name="line_415"></a>  display(
<a name="line_416"></a>   surface_plot(stereo,M = 12,H = [1,LM,LN,MN]),
<a name="line_417"></a>   seq(c_E_plot[i],i=3..8),orientation=[-120,0,50],axes=none
<a name="line_418"></a>  ):
<a name="line_419"></a> save_plot("F4"):
<a name="line_420"></a> save_jpg("F4"):
<a name="line_421"></a> pics["F4"];
<a name="line_422"></a>end:
<a name="line_423"></a>
<a name="line_424"></a>######################################################################
<a name="line_425"></a>
<a name="line_426"></a><span style="color:red">#@ make_F16_plots
</span><a name="line_427"></a>
<a name="line_428"></a>make_F16_plots := proc()
<a name="line_429"></a> global pics;
<a name="line_430"></a>
<a name="line_431"></a> pics["F16_outline"] := display(
<a name="line_432"></a>  seq(
<a name="line_433"></a>   spacecurve(stereo(c_E0[k](t)),
<a name="line_434"></a>              t=F16_curve_limits[k],
<a name="line_435"></a>              colour = c_colour[k]),
<a name="line_436"></a>   k in [0,1,3,5]
<a name="line_437"></a>  ),
<a name="line_438"></a>  scaling = constrained,axes=none
<a name="line_439"></a> );
<a name="line_440"></a>
<a name="line_441"></a> save_plot("F16_outline"):
<a name="line_442"></a> save_jpg("F16_outline"):
<a name="line_443"></a>
<a name="line_444"></a> pics["F16"] := 
<a name="line_445"></a>  surface_plot(stereo,M = 24,H = [1]);
<a name="line_446"></a> save_plot("F16");
<a name="line_447"></a> save_jpg( "F16");
<a name="line_448"></a>
<a name="line_449"></a> pics["F16_wireframe"] := 
<a name="line_450"></a>  surface_plot(stereo,M = 24,H = [1],plot_style = wireframe);
<a name="line_451"></a> save_plot("F16_wireframe");
<a name="line_452"></a> save_jpg( "F16_wireframe");
<a name="line_453"></a>
<a name="line_454"></a> NULL;
<a name="line_455"></a>end:
<a name="line_456"></a>
<a name="line_457"></a>############################################################
<a name="line_458"></a>
<a name="line_459"></a><span style="color:red">#@ make_y_proj_plots
</span><a name="line_460"></a>
<a name="line_461"></a>make_y_proj_plots := proc()
<a name="line_462"></a> global pics;
<a name="line_463"></a>
<a name="line_464"></a> pics["y_proj"] := 
<a name="line_465"></a>  plane_proj_plot(y_proj0,
<a name="line_466"></a>   table([0=right,1=left,3=above,11=above,13=above]),
<a name="line_467"></a>   table([0=Pi/6,1=2,2=1.9,3=2.1,4=2.1,5=1.5,6=1.5,7=1.5,8=1.5]),
<a name="line_468"></a>   table([0=left,3=above,5=right,6=right,7=left,8=left]));
<a name="line_469"></a>
<a name="line_470"></a> save_plot("y_proj");
<a name="line_471"></a> save_jpg("y_proj");
<a name="line_472"></a>
<a name="line_473"></a> pics["y_proj_F4"] :=
<a name="line_474"></a>  plane_proj_plot(y_proj0,
<a name="line_475"></a>   table([0=right,1=left,3=above,11=above,13=above]),
<a name="line_476"></a>   table([0=Pi/6,1=2,2=1.9,3=2.1,4=2.1,5=1.5,6=1.5,7=1.5,8=1.5]),
<a name="line_477"></a>   table([0=left,3=above,5=right,6=right,7=left,8=left]),"F4");
<a name="line_478"></a>
<a name="line_479"></a> save_plot("y_proj_F4");
<a name="line_480"></a> save_jpg("y_proj_F4");
<a name="line_481"></a>
<a name="line_482"></a> pics["y_proj_F16"] :=
<a name="line_483"></a>  plane_proj_plot(y_proj0,
<a name="line_484"></a>   table([1=left,3=above,11=above,13=above]),
<a name="line_485"></a>   table([0=1.05,1=1.1,3=1,5=1.5]),
<a name="line_486"></a>   table([0=left,3=above,5=right,6=right,7=left,8=left]),
<a name="line_487"></a>   "F16");
<a name="line_488"></a>
<a name="line_489"></a> save_plot("y_proj_F16");
<a name="line_490"></a> save_jpg("y_proj_F16");
<a name="line_491"></a>
<a name="line_492"></a> pics["y_proj_extra"] := display(
<a name="line_493"></a>  seq(planecurve(y_proj0(c_E0[k](t)),t=0..2*Pi,colour=c_colour[k]),
<a name="line_494"></a>      k in [0,1,3,4,5,6,7,8,9,11,13,14]),
<a name="line_495"></a>  scaling=constrained,axes=none
<a name="line_496"></a> );
<a name="line_497"></a> save_plot("y_proj_extra");
<a name="line_498"></a> save_jpg("y_proj_extra");
<a name="line_499"></a>
<a name="line_500"></a> NULL;
<a name="line_501"></a>end:
<a name="line_502"></a>
<a name="line_503"></a>############################################################
<a name="line_504"></a>
<a name="line_505"></a><span style="color:red">#@ make_z_proj_plots
</span><a name="line_506"></a>
<a name="line_507"></a>make_z_proj_plots := proc()
<a name="line_508"></a> global pics;
<a name="line_509"></a> local v_z_map,v_z_align0,v_z_align,c_z_map0,c_z_map,c_z_labels,c_z_arrows,c_z_plots,c_z,
<a name="line_510"></a>       i,ii,z0,t0,a,u,L,r,w;
<a name="line_511"></a>
<a name="line_512"></a> pics["z_proj"] := 
<a name="line_513"></a>  plane_proj_plot(z_proj0,
<a name="line_514"></a>   table([2=above,10=above]),
<a name="line_515"></a>   table([0=1.2,1=2.3,2=2.4,3=2.3,4=2.4,5=1.9,6=2.0,7=2.1,8=2.2]),
<a name="line_516"></a>   table([0=left,3={above,left},4={above,left},5=right,6=right,7=right,8=right]));
<a name="line_517"></a>
<a name="line_518"></a> save_plot("z_proj");
<a name="line_519"></a> save_jpg("z_proj");
<a name="line_520"></a>
<a name="line_521"></a> v_z_align0 := table([
<a name="line_522"></a>  0 = 'right',
<a name="line_523"></a>  2 = 'left',
<a name="line_524"></a>  6 = 'below'
<a name="line_525"></a> ]):
<a name="line_526"></a>
<a name="line_527"></a> c_z_map0 := table([
<a name="line_528"></a>  0  = [ 1.20,'left'],
<a name="line_529"></a>  1  = [ 2.30,'below'],
<a name="line_530"></a>  2  = [ 2.40,'below'],
<a name="line_531"></a>  3  = [ 2.30,{'above','left'}],
<a name="line_532"></a>  4  = [ 2.40,{'above','left'}],
<a name="line_533"></a>  5  = [ 1.90,'right'],
<a name="line_534"></a>  6  = [ 2.00,'right'],
<a name="line_535"></a>  7  = [ 2.10,'right'],
<a name="line_536"></a>  8  = [ 2.20,'right']
<a name="line_537"></a> ]):
<a name="line_538"></a>
<a name="line_539"></a> v_z_map := table():
<a name="line_540"></a>
<a name="line_541"></a> v_z_align := table();
<a name="line_542"></a>
<a name="line_543"></a> v_z_align := table();
<a name="line_544"></a> for ii in indices(v_z_align0) do
<a name="line_545"></a>  v_z_align[simplify(z_proj0(v_E0[op(ii)]))] := v_z_align0[op(ii)];
<a name="line_546"></a> od:
<a name="line_547"></a>
<a name="line_548"></a> for i in select(j -> j <= 13,F16_vertices) do
<a name="line_549"></a>  z0 := simplify(z_proj0(v_E0[i]));
<a name="line_550"></a>  if not(assigned(v_z_align[z0])) then
<a name="line_551"></a>   v_z_align[z0] := 'below';
<a name="line_552"></a>  fi;
<a name="line_553"></a>  if type(v_z_map[z0],list) then
<a name="line_554"></a>   v_z_map[z0] := [op(v_z_map[z0]),i];
<a name="line_555"></a>  else
<a name="line_556"></a>   v_z_map[z0] := [i];
<a name="line_557"></a>  fi;
<a name="line_558"></a> od:
<a name="line_559"></a>
<a name="line_560"></a> c_z_labels := NULL;
<a name="line_561"></a> c_z_arrows := NULL;
<a name="line_562"></a> c_z_plots  := NULL;
<a name="line_563"></a>
<a name="line_564"></a> for i from 0 to 8 do 
<a name="line_565"></a>  c_z[i] := unapply(simplify(evalf(simplify(z_proj0(c_E0[i](t))))),t);
<a name="line_566"></a> od:
<a name="line_567"></a>
<a name="line_568"></a> for i from 0 to 8 do
<a name="line_569"></a>  if F16_curve_limits[i] <> NULL then
<a name="line_570"></a>   t0,a := op(c_z_map0[i]);
<a name="line_571"></a>   r := F16_curve_limits[i];
<a name="line_572"></a>   u := evalf(c_z[i](t0));
<a name="line_573"></a>   w := evalf(c_z[i](t0+0.05)) -~ u;
<a name="line_574"></a>   w := w /~ nm2(w);
<a name="line_575"></a>
<a name="line_576"></a>   c_z_plots := c_z_plots,
<a name="line_577"></a>                plot([op(c_z[i](t)),t=r],colour=c_colour[i]);
<a name="line_578"></a>   c_z_labels := c_z_labels,
<a name="line_579"></a>                textplot([op(u),sprintf("%Q",i,op(r))],align=a,colour=red);
<a name="line_580"></a>   c_z_arrows := c_z_arrows,
<a name="line_581"></a>      line(u,u +~ 0.03 *~ [-w[1]-w[2], w[1]-w[2]],colour = c_colour[i]),
<a name="line_582"></a>      line(u,u +~ 0.03 *~ [-w[1]+w[2],-w[1]-w[2]],colour = c_colour[i]);
<a name="line_583"></a>  fi;
<a name="line_584"></a> od;
<a name="line_585"></a>
<a name="line_586"></a> pics["z_proj_F16"] := display(
<a name="line_587"></a>  c_z_plots,
<a name="line_588"></a>  c_z_labels,
<a name="line_589"></a>  c_z_arrows,
<a name="line_590"></a>  seq(point(evalf(op(ii))),ii in indices(v_z_map)),
<a name="line_591"></a>  seq(
<a name="line_592"></a>    textplot([op(evalf(op(ii))),sprintf("%Q",op(v_z_map[op(ii)]))],'align'=v_z_align[op(ii)]),
<a name="line_593"></a>    ii in indices(v_z_map)
<a name="line_594"></a>  ),
<a name="line_595"></a>  scaling=constrained,axes=none
<a name="line_596"></a> );
<a name="line_597"></a>
<a name="line_598"></a> save_plot("z_proj_F16");
<a name="line_599"></a> save_jpg("z_proj_F16");
<a name="line_600"></a>
<a name="line_601"></a> pics["z_proj_F16_bare"] := display(
<a name="line_602"></a>  plot([op(z_proj0(c_E0[0](t))),t=Pi/4..Pi/2],colour = c_colour[0]),
<a name="line_603"></a>  plot([op(z_proj0(c_E0[1](t))),t=0..Pi/2],colour = c_colour[1]),
<a name="line_604"></a>  plot([op(z_proj0(c_E0[3](t))),t=0..Pi/2],colour = c_colour[3]),
<a name="line_605"></a>  plot([op(z_proj0(c_E0[5](t))),t=0..Pi],colour = c_colour[5]),
<a name="line_606"></a>  scaling = constrained,axes=none
<a name="line_607"></a> ):
<a name="line_608"></a>
<a name="line_609"></a> save_plot("z_proj_F16_bare"):
<a name="line_610"></a> save_jpg("z_proj_F16_bare"):
<a name="line_611"></a>
<a name="line_612"></a> pics["z_proj_extra"] := display(
<a name="line_613"></a>  seq(planecurve(z_proj0(c_E0[k](t)),t=0..2*Pi,colour=c_colour[k]),
<a name="line_614"></a>      k in [0,1,3,5,9,13]),
<a name="line_615"></a>  scaling=constrained,axes=none
<a name="line_616"></a> );
<a name="line_617"></a> save_plot("z_proj_extra"):
<a name="line_618"></a> save_jpg("z_proj_extra"):
<a name="line_619"></a>
<a name="line_620"></a> NULL;
<a name="line_621"></a>end:
<a name="line_622"></a>
<a name="line_623"></a>############################################################
<a name="line_624"></a>
<a name="line_625"></a><span style="color:red">#@ make_z_proj_F16_tikz 
</span><a name="line_626"></a>make_z_proj_F16_tikz := proc()
<a name="line_627"></a> local v_z_map,v_z_align0,v_z_align,c_z_map0,c_z_map,c_z_labels,c_z_arrows,c_z_plots,c_z,
<a name="line_628"></a>       i,ii,z0,t0,a,u,L,r,w,s,num_points;
<a name="line_629"></a>
<a name="line_630"></a> v_z_align0 := table([
<a name="line_631"></a>  0 = 'right',
<a name="line_632"></a>  2 = 'left',
<a name="line_633"></a>  6 = 'below'
<a name="line_634"></a> ]):
<a name="line_635"></a>
<a name="line_636"></a> c_z_map0 := table([
<a name="line_637"></a>  0  = [ 1.20,'left'],
<a name="line_638"></a>  1  = [ 2.30,'below'],
<a name="line_639"></a>  2  = [ 2.40,'below'],
<a name="line_640"></a>  3  = [ 2.30,{'above','left'}],
<a name="line_641"></a>  4  = [ 2.40,{'above','left'}],
<a name="line_642"></a>  5  = [ 1.90,'right'],
<a name="line_643"></a>  6  = [ 2.00,'right'],
<a name="line_644"></a>  7  = [ 2.10,'right'],
<a name="line_645"></a>  8  = [ 2.20,'right']
<a name="line_646"></a> ]):
<a name="line_647"></a>
<a name="line_648"></a> v_z_map := table():
<a name="line_649"></a>
<a name="line_650"></a> v_z_align := table();
<a name="line_651"></a>
<a name="line_652"></a> v_z_align := table();
<a name="line_653"></a> for ii in indices(v_z_align0) do
<a name="line_654"></a>  v_z_align[simplify(z_proj0(v_E0[op(ii)]))] := v_z_align0[op(ii)];
<a name="line_655"></a> od:
<a name="line_656"></a>
<a name="line_657"></a> for i in select(j -> j <= 13,F16_vertices) do
<a name="line_658"></a>  z0 := simplify(z_proj0(v_E0[i]));
<a name="line_659"></a>  if not(assigned(v_z_align[z0])) then
<a name="line_660"></a>   v_z_align[z0] := 'below';
<a name="line_661"></a>  fi;
<a name="line_662"></a>  if type(v_z_map[z0],list) then
<a name="line_663"></a>   v_z_map[z0] := [op(v_z_map[z0]),i];
<a name="line_664"></a>  else
<a name="line_665"></a>   v_z_map[z0] := [i];
<a name="line_666"></a>  fi;
<a name="line_667"></a> od:
<a name="line_668"></a>
<a name="line_669"></a> c_z_labels := "";
<a name="line_670"></a> c_z_arrows := "";
<a name="line_671"></a> c_z_plots  := "";
<a name="line_672"></a>
<a name="line_673"></a> for i from 0 to 8 do 
<a name="line_674"></a>  c_z[i] := unapply(simplify(evalf(simplify(z_proj0(c_E0[i](t))))),t);
<a name="line_675"></a> od:
<a name="line_676"></a>
<a name="line_677"></a> num_points := 25;
<a name="line_678"></a> 
<a name="line_679"></a> for i from 0 to 8 do
<a name="line_680"></a>  if F16_curve_limits[i] <> NULL then
<a name="line_681"></a>   t0,a := op(c_z_map0[i]);
<a name="line_682"></a>   r := F16_curve_limits[i];
<a name="line_683"></a>   u := evalf(c_z[i](t0));
<a name="line_684"></a>   w := evalf(c_z[i](t0+0.01));
<a name="line_685"></a>
<a name="line_686"></a>   c_z_plots := cat(c_z_plots,tikz_plot(c_z[i],r,num_points,c_colour[i]));
<a name="line_687"></a>   c_z_labels := cat(c_z_labels,tikz_label(u,sprintf("%Q",i,op(r)),a));
<a name="line_688"></a>   c_z_arrows := cat(c_z_arrows,
<a name="line_689"></a>                      sprintf(" \\draw[%s,arrows={-angle 90}] (%.4f,%.4f) -- (%.4f,%.4f);\n",
<a name="line_690"></a>                              c_colour[i],op(u),op(w)));
<a name="line_691"></a>  fi;
<a name="line_692"></a> od;
<a name="line_693"></a>
<a name="line_694"></a> s := cat("\\begin{center}\n \\begin{tikzpicture}[scale=4]\n",
<a name="line_695"></a>          c_z_plots,
<a name="line_696"></a>          c_z_labels,
<a name="line_697"></a>          c_z_arrows,
<a name="line_698"></a>          " \\end{tikzpicture}\n\\end{center}\n");
<a name="line_699"></a>
<a name="line_700"></a> save_tikz("z_proj_F16_raw",s);
<a name="line_701"></a>
<a name="line_702"></a> return(s);
<a name="line_703"></a>end:
<a name="line_704"></a>
<a name="line_705"></a>############################################################
<a name="line_706"></a>
<a name="line_707"></a><span style="color:red">#@ make_w_proj_plots
</span><a name="line_708"></a>make_w_proj_plots := proc()
<a name="line_709"></a> global wp,pics;
<a name="line_710"></a>
<a name="line_711"></a> wp := (x) -> `if`(x[1]=0 and x[2]=0 and x[4]=0,[1,0],w_proj0(x));
<a name="line_712"></a>
<a name="line_713"></a> pics["w_proj"] := 
<a name="line_714"></a>  plane_proj_plot(wp,
<a name="line_715"></a>   table([2=above,10=above]),
<a name="line_716"></a>   table([0=1.0,1=2.3,2=2.4,3=2.3,4=2.4,5=1.4,6=1.5,7=1.6,8=1.7]),
<a name="line_717"></a>   table([0=left,3={above,left},4={above,left},5=right,6=right,7=right,8=right]));
<a name="line_718"></a>
<a name="line_719"></a> save_plot("w_proj");
<a name="line_720"></a> save_jpg("w_proj");
<a name="line_721"></a>
<a name="line_722"></a> pics["w_proj_extra"] := display(
<a name="line_723"></a>  seq(planecurve(w_proj0(c_E0[k](t)),t=0..2*Pi,colour=c_colour[k]),
<a name="line_724"></a>      k in [0,1,3,5,9,13]),
<a name="line_725"></a>  scaling=constrained,axes=none
<a name="line_726"></a> );
<a name="line_727"></a> save_plot("w_proj_extra"):
<a name="line_728"></a> save_jpg("w_proj_extra"):
<a name="line_729"></a>
<a name="line_730"></a> NULL;
<a name="line_731"></a>end:
<a name="line_732"></a>
<a name="line_733"></a>############################################################
<a name="line_734"></a>
<a name="line_735"></a><span style="color:red">#@ make_t_proj_plots
</span><a name="line_736"></a>make_t_proj_plots := proc()
<a name="line_737"></a> global wp,pics;
<a name="line_738"></a>
<a name="line_739"></a> wp := (x) -> `if`(x[1]=0 and x[2]=0 and x[4]=0,[1,0],t_proj(x));
<a name="line_740"></a>
<a name="line_741"></a> pics["t_proj"] := 
<a name="line_742"></a>  plane_proj_plot(wp,
<a name="line_743"></a>   table([2=above,10=above]),
<a name="line_744"></a>   table([0=1.0,1=2.3,2=2.4,3=2.3,4=2.4,5=1.4,6=1.5,7=1.6,8=1.7]),
<a name="line_745"></a>   table([0=left,3={above,left},4={above,left},5=right,6=right,7=right,8=right]));
<a name="line_746"></a>
<a name="line_747"></a> save_plot("t_proj");
<a name="line_748"></a> save_jpg("t_proj");
<a name="line_749"></a>
<a name="line_750"></a> pics["t_proj_extra"] := display(
<a name="line_751"></a>  seq(planecurve(t_proj(c_E0[k](t)),t=0..2*Pi,colour=c_colour[k]),
<a name="line_752"></a>      k in [0,1,3,5,9,13]),
<a name="line_753"></a>  scaling=constrained,axes=none
<a name="line_754"></a> );
<a name="line_755"></a> save_plot("t_proj_extra"):
<a name="line_756"></a> save_jpg("t_proj_extra"):
<a name="line_757"></a>
<a name="line_758"></a> NULL;
<a name="line_759"></a>end:
<a name="line_760"></a>
<a name="line_761"></a>############################################################
<a name="line_762"></a>
<a name="line_763"></a><span style="color:red">#@ make_v_square_plots 
</span><a name="line_764"></a>make_v_square_plots := proc()
<a name="line_765"></a> local k,s;
<a name="line_766"></a> global pics;
<a name="line_767"></a>
<a name="line_768"></a> for k in {3,6,11} do 
<a name="line_769"></a>  s := sprintf("v_square[%d]",k);
<a name="line_770"></a>  pics[s] := 
<a name="line_771"></a>   display(
<a name="line_772"></a>    seq(op([
<a name="line_773"></a>     plot3d(v_stereo[k](act_E[T](t_lift([t[1],t[2]]))),t[1]=0..1,t[2]=0..1),
<a name="line_774"></a>     spacecurve(v_stereo[k](act_E[T](c_E0[0](t))),t=Pi/4..Pi/2,colour=c_colour[0]),
<a name="line_775"></a>     spacecurve(v_stereo[k](act_E[T](c_E0[1](t))),t=0..Pi/2,colour=c_colour[1]),
<a name="line_776"></a>     spacecurve(v_stereo[k](act_E[T](c_E0[3](t))),t=0..Pi/2,colour=c_colour[3]),
<a name="line_777"></a>     spacecurve(v_stereo[k](act_E[T](c_E0[5](t))),t=0..Pi,colour=c_colour[5])]),
<a name="line_778"></a>     T in v_stabiliser_G16[k]
<a name="line_779"></a>    ),
<a name="line_780"></a>    scaling=constrained,axes=none
<a name="line_781"></a>   );
<a name="line_782"></a>
<a name="line_783"></a>   save_plot(s);
<a name="line_784"></a>   save_jpg( s);
<a name="line_785"></a> od:
<a name="line_786"></a> NULL;
<a name="line_787"></a>end:
<a name="line_788"></a>
<a name="line_789"></a><span style="color:red">#@ load_v_square_plots 
</span><a name="line_790"></a>load_v_square_plots := proc()
<a name="line_791"></a> local ss;
<a name="line_792"></a> ss := seq(sprintf("v_square[%d]",k),k in {3,6,11});
<a name="line_793"></a> load_plots(ss);
<a name="line_794"></a> NULL;
<a name="line_795"></a>end:
<a name="line_796"></a>
<a name="line_797"></a>############################################################
<a name="line_798"></a>
<a name="line_799"></a><span style="color:red">#@ make_disc_delta_plot
</span><a name="line_800"></a>make_disc_delta_plot := proc()
<a name="line_801"></a> global pics;
<a name="line_802"></a> local dpr;
<a name="line_803"></a>
<a name="line_804"></a> dpr := unapply(simplify(subs(a_E=a_E0,disc_delta_proj(xx))),x);
<a name="line_805"></a>
<a name="line_806"></a> pics["disc_delta"] :=
<a name="line_807"></a>  plane_proj_plot(dpr,
<a name="line_808"></a>   table([0 = 'above',2 = {'above','right'},3 = {'above','left'},
<a name="line_809"></a>          7 = 'left',9 = 'right',12 = 'above']),
<a name="line_810"></a>   table(),
<a name="line_811"></a>   table());
<a name="line_812"></a>
<a name="line_813"></a> save_plot("disc_delta");
<a name="line_814"></a> save_jpg("disc_delta");
<a name="line_815"></a> pics["disc_delta"];
<a name="line_816"></a>end:
<a name="line_817"></a>
<a name="line_818"></a>######################################################################
<a name="line_819"></a>
<a name="line_820"></a><span style="color:red">#@ make_disc_pi_plot
</span><a name="line_821"></a>make_disc_pi_plot := proc()
<a name="line_822"></a> global pics;
<a name="line_823"></a> local dpr;
<a name="line_824"></a>
<a name="line_825"></a> dpr := unapply(simplify(subs(a_E=a_E0,disc_pi_proj(xx))),x);
<a name="line_826"></a>
<a name="line_827"></a> pics["disc_pi"] :=
<a name="line_828"></a>  plane_proj_plot(dpr,
<a name="line_829"></a>   table([2=right,3=above,4=left,
<a name="line_830"></a>          6={above,right},7={above,left},8={below,left},9={below,right}]),
<a name="line_831"></a>   table(),
<a name="line_832"></a>   table());
<a name="line_833"></a>
<a name="line_834"></a> save_plot("disc_pi");
<a name="line_835"></a> save_jpg("disc_pi");
<a name="line_836"></a> pics["disc_pi"];
<a name="line_837"></a>end:
<a name="line_838"></a>
<a name="line_839"></a>######################################################################
<a name="line_840"></a>
<a name="line_841"></a><span style="color:red">#@ make_disc_zeta_plots
</span><a name="line_842"></a>make_disc_zeta_plots := proc()
<a name="line_843"></a> global pics;
<a name="line_844"></a> local dpr;
<a name="line_845"></a>
<a name="line_846"></a> dpr := unapply(simplify(subs(a_E=a_E0,disc_zeta_proj(xx))),x);
<a name="line_847"></a>
<a name="line_848"></a> pics["disc_zeta"] :=
<a name="line_849"></a>  plane_proj_plot(dpr,
<a name="line_850"></a>   table([0=left,1=right,3=above,6=above,7=above,10=right,11=right,12=left,13=left]),
<a name="line_851"></a>   table(),table());
<a name="line_852"></a>
<a name="line_853"></a> save_plot("disc_zeta");
<a name="line_854"></a> save_jpg("disc_zeta");
<a name="line_855"></a>
<a name="line_856"></a> pics["disc_zeta_F16"] :=
<a name="line_857"></a>  plane_proj_plot(dpr,
<a name="line_858"></a>   table([3=left,6=left]),
<a name="line_859"></a>   table(),table(),"F16");
<a name="line_860"></a>
<a name="line_861"></a> save_plot("disc_zeta_F16");
<a name="line_862"></a> save_jpg("disc_zeta_F16");
<a name="line_863"></a>
<a name="line_864"></a> NULL;
<a name="line_865"></a>end:
<a name="line_866"></a>
<a name="line_867"></a>######################################################################
<a name="line_868"></a>
<a name="line_869"></a><span style="color:red">#@ make_E_torus_plots 
</span><a name="line_870"></a>make_E_torus_plots := proc()
<a name="line_871"></a> global pics;
<a name="line_872"></a> local k,lp,lm,lmq,wft;
<a name="line_873"></a> 
<a name="line_874"></a> for k from 0 to 16 do
<a name="line_875"></a>  lp  := sprintf("c_Tp[%d]",k);
<a name="line_876"></a>  lm  := sprintf("c_Tm[%d]",k);
<a name="line_877"></a>  lmq := sprintf("c_Tmq[%d]",k);
<a name="line_878"></a>  
<a name="line_879"></a>  pics[lp ] := spacecurve(TC_to_R3(c_TCp[ k](t)),t=0..2*Pi,
<a name="line_880"></a>                 colour=c_colour[k],numpoints=400,scaling=constrained,axes=none);
<a name="line_881"></a>  pics[lm ] := spacecurve(TC_to_R3(c_TCm[ k](t)),t=0..2*Pi,
<a name="line_882"></a>                 colour=c_colour[k],numpoints=400,scaling=constrained,axes=none);
<a name="line_883"></a>  pics[lmq] := spacecurve(TC_to_R3(c_TCmq[k](t)),t=0..2*Pi,
<a name="line_884"></a>                 colour=c_colour[k],numpoints=400,scaling=constrained,axes=none);
<a name="line_885"></a>
<a name="line_886"></a>  save_plot(lp);
<a name="line_887"></a>  save_plot(lm);
<a name="line_888"></a>  save_plot(lmq);
<a name="line_889"></a>
<a name="line_890"></a>  save_jpg(lp);
<a name="line_891"></a>  save_jpg(lm);
<a name="line_892"></a>  save_jpg(lmq);
<a name="line_893"></a>od:
<a name="line_894"></a>
<a name="line_895"></a> wft :=
<a name="line_896"></a>  plot3d(TA_to_R3([t,u]),t=0..2*Pi,u=0..2*Pi,
<a name="line_897"></a>           colour=gray,style=wireframe,scaling=constrained,axes=none);
<a name="line_898"></a>
<a name="line_899"></a> pics["c_Tp" ] := display(wft,seq(pics[sprintf("c_Tp[%d]",k)],
<a name="line_900"></a>                             k in {0,1,3,4,5,6}));
<a name="line_901"></a> pics["c_Tm" ] := display(wft,seq(pics[sprintf("c_Tm[%d]",k)],
<a name="line_902"></a>                             k in {0,1,2,3,5,6}));
<a name="line_903"></a> pics["c_Tmq"] := display(wft,seq(pics[sprintf("c_Tmq[%d]",k)],
<a name="line_904"></a>                             k in {0,1,2,3,5,6}));
<a name="line_905"></a>
<a name="line_906"></a> save_plot("c_Tp");
<a name="line_907"></a> save_plot("c_Tm");
<a name="line_908"></a> save_plot("c_Tmq");
<a name="line_909"></a>
<a name="line_910"></a> save_jpg("c_Tp");
<a name="line_911"></a> save_jpg("c_Tm");
<a name="line_912"></a> save_jpg("c_Tmq");
<a name="line_913"></a>
<a name="line_914"></a> pics["c_Tp_extra"] := display(
<a name="line_915"></a>  pics["c_Tp"],
<a name="line_916"></a>  seq(pics[sprintf("c_Tp[%d]",k)],k in {9,10,13,15})
<a name="line_917"></a> );
<a name="line_918"></a>
<a name="line_919"></a> pics["c_Tm_extra"] := display(
<a name="line_920"></a>  pics["c_Tm"],
<a name="line_921"></a>  seq(pics[sprintf("c_Tm[%d]",k)],k in {9,10,13,14,15,16})
<a name="line_922"></a> );
<a name="line_923"></a>
<a name="line_924"></a> pics["c_Tmq_extra"] := display(
<a name="line_925"></a>  pics["c_Tmq"],
<a name="line_926"></a>  seq(pics[sprintf("c_Tmq[%d]",k)],k in {9,10,13,14,15,16})
<a name="line_927"></a> );
<a name="line_928"></a>
<a name="line_929"></a> save_plot("c_Tp_extra");
<a name="line_930"></a> save_plot("c_Tm_extra");
<a name="line_931"></a> save_plot("c_Tmq_extra");
<a name="line_932"></a>
<a name="line_933"></a> save_jpg("c_Tp_extra");
<a name="line_934"></a> save_jpg("c_Tm_extra");
<a name="line_935"></a> save_jpg("c_Tmq_extra");
<a name="line_936"></a>
<a name="line_937"></a>end:
<a name="line_938"></a>
<a name="line_939"></a>######################################################################
<a name="line_940"></a>
<a name="line_941"></a><span style="color:red">#@ make_E_sphere_plots 
</span><a name="line_942"></a>make_E_sphere_plots := proc()
<a name="line_943"></a> global pics;
<a name="line_944"></a> local wfs;
<a name="line_945"></a>
<a name="line_946"></a> wfs := display(sphere([0,0,0],1,colour=grey,style=wireframe),axes=none):
<a name="line_947"></a>
<a name="line_948"></a> # EX^*/<LL>
<a name="line_949"></a> pics["SQE_LL"] := 
<a name="line_950"></a> display(wfs,
<a name="line_951"></a>  seq(spacecurve(E_to_S2(c_E0[k](t)),
<a name="line_952"></a>       t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
<a name="line_953"></a>  axes=none,scaling=constrained
<a name="line_954"></a> ):
<a name="line_955"></a>
<a name="line_956"></a> # EX^*/<L>
<a name="line_957"></a> pics["SQE_L"] := 
<a name="line_958"></a> display(wfs,
<a name="line_959"></a>  seq(spacecurve(p_S2[ 2, 3](E_to_S2(c_E0[k](t))),
<a name="line_960"></a>       t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
<a name="line_961"></a>  axes=none,scaling=constrained
<a name="line_962"></a> ):
<a name="line_963"></a>
<a name="line_964"></a> # EX^*/<LL,M>
<a name="line_965"></a> pics["SQE_LL_M"] := 
<a name="line_966"></a> display(wfs,
<a name="line_967"></a>  seq(spacecurve(p_S2[ 2, 8](E_to_S2(c_E0[k](t))),
<a name="line_968"></a>       t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
<a name="line_969"></a>  axes=none,scaling=constrained
<a name="line_970"></a> ):
<a name="line_971"></a>
<a name="line_972"></a> # EX^*/<LL,LM>
<a name="line_973"></a> pics["SQE_LL_LM"] := 
<a name="line_974"></a> display(wfs,
<a name="line_975"></a>  seq(spacecurve(p_S2[ 2, 9](E_to_S2(c_E0[k](t))),
<a name="line_976"></a>       t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
<a name="line_977"></a>  axes=none,scaling=constrained
<a name="line_978"></a> ):
<a name="line_979"></a>
<a name="line_980"></a> # EX^*/<L,M>
<a name="line_981"></a> pics["SQE_L_M"] := 
<a name="line_982"></a> display(wfs,
<a name="line_983"></a>  seq(spacecurve(p_S2[ 2,10](E_to_S2(c_E0[k](t))),
<a name="line_984"></a>       t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
<a name="line_985"></a>  axes=none,scaling=constrained
<a name="line_986"></a> ):
<a name="line_987"></a>
<a name="line_988"></a> save_plot("SQE_LL");
<a name="line_989"></a> save_plot("SQE_L");
<a name="line_990"></a> save_plot("SQE_LL_M");
<a name="line_991"></a> save_plot("SQE_LL_LM");
<a name="line_992"></a> save_plot("SQE_L_M");
<a name="line_993"></a>
<a name="line_994"></a> save_jpg("SQE_LL");
<a name="line_995"></a> save_jpg("SQE_L");
<a name="line_996"></a> save_jpg("SQE_LL_M");
<a name="line_997"></a> save_jpg("SQE_LL_LM");
<a name="line_998"></a> save_jpg("SQE_L_M");
<a name="line_999"></a>
<a name="line_1000"></a>end:
  </pre>
 </body>
</html>
    