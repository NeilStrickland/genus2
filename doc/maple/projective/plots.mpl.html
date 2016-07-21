<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>make_PX_plots := proc(a0_)
<a name="line_2"></a> local i,t,a0,v0,c0,mp11,mp13,Rp,mq4,mq5,Rq;
<a name="line_3"></a> global pics;
<a name="line_4"></a>
<a name="line_5"></a> a0 := `if`(nargs > 0,a0_,a_P0);
<a name="line_6"></a>
<a name="line_7"></a> for i from 0 to 8 do
<a name="line_8"></a>  c0[i] := unapply(evalf(subs(a_P=a0,c_P[i](t))),t);
<a name="line_9"></a> od:
<a name="line_10"></a> for i from 0 to 13 do
<a name="line_11"></a>  v0[i] := evalf(subs(a_P=a0,v_P[i]));
<a name="line_12"></a> od:
<a name="line_13"></a>
<a name="line_14"></a> mp11 := p_P(v0[11]);
<a name="line_15"></a> mp13 := p_P(v0[13]);
<a name="line_16"></a> 
<a name="line_17"></a> Rp := 1.5 * p_P(v0[13]);
<a name="line_18"></a> 
<a name="line_19"></a> pics["P_p"] := display(
<a name="line_20"></a>  circle(1,colour=c_colour[0]),
<a name="line_21"></a>  line([  0,-Rp],[  0, Rp],colour=c_colour[1]),
<a name="line_22"></a>  line([ mp11,0],[ mp13,0],colour=c_colour[3]),
<a name="line_23"></a>  line([-mp11,0],[-mp13,0],colour=c_colour[3]),
<a name="line_24"></a>  line([-mp11,0],[ mp11,0],colour=c_colour[5]),
<a name="line_25"></a>  line([ mp13,0],[   Rp,0],colour=c_colour[5]),
<a name="line_26"></a>  line([-mp13,0],[  -Rp,0],colour=c_colour[5]),
<a name="line_27"></a>  seq(cpoint(p_P(v0[i])),i in [0,2,3,6,7,10,11,12,13]),
<a name="line_28"></a>  view=[-Rp..Rp,-Rp..Rp],axes=none
<a name="line_29"></a> );
<a name="line_30"></a> save_plot("P_p");
<a name="line_31"></a> save_jpg("P_p");
<a name="line_32"></a>
<a name="line_33"></a> pics["P_p_alt"] := display(
<a name="line_34"></a>  seq(cplot(p_P(c0[i](t)),t=0..2*Pi,colour=c_colour[i]),i=0..8),
<a name="line_35"></a>  seq(cpoint(p_P(v0[i])),i in [0,2,3,6,7,10,11,12,13]),
<a name="line_36"></a>  view=[-Rp..Rp,-Rp..Rp],axes=none
<a name="line_37"></a> );
<a name="line_38"></a> save_plot("P_p_alt");
<a name="line_39"></a> save_jpg("P_p_alt");
<a name="line_40"></a>
<a name="line_41"></a> mq4 := maximize(q_P(c0[4](t)),t=0..2*Pi);
<a name="line_42"></a> mq5 := maximize(q_P(c0[5](t)),t=0..2*Pi);
<a name="line_43"></a> Rq  := 1.2 * max(mq4,mq5,seq(abs(q_P(v0[i])),i=2..13));
<a name="line_44"></a>
<a name="line_45"></a> pics["P_q"] := display(
<a name="line_46"></a>  line([-mq5,   0],[ mq5,   0],colour=c_colour[5]),
<a name="line_47"></a>  line([   0,-mq5],[   0, mq5],colour=c_colour[5]),
<a name="line_48"></a>  line([ mq5,   0],[ mq4,   0],colour=c_colour[4]),
<a name="line_49"></a>  line([-mq5,   0],[-mq4,   0],colour=c_colour[4]),
<a name="line_50"></a>  line([   0, mq5],[   0, mq4],colour=c_colour[4]),
<a name="line_51"></a>  line([   0,-mq5],[   0,-mq4],colour=c_colour[4]),
<a name="line_52"></a>  line([ mq4,   0],[  Rq,   0],colour=c_colour[7]),
<a name="line_53"></a>  line([-mq4,   0],[ -Rq,   0],colour=c_colour[7]),
<a name="line_54"></a>  line([   0, mq4],[   0,  Rq],colour=c_colour[7]),
<a name="line_55"></a>  line([   0,-mq4],[   0, -Rq],colour=c_colour[7]),
<a name="line_56"></a>  line([ -Rq, -Rq],[  Rq,  Rq],colour=c_colour[1]),
<a name="line_57"></a>  line([ -Rq,  Rq],[  Rq, -Rq],colour=c_colour[1]),
<a name="line_58"></a>  cplot(q_P(c0[0](t)),t=0..2*Pi,colour=c_colour[0]),
<a name="line_59"></a>  seq(cpoint(q_P(v0[i])),i in [0,2,3,4,5,6,7,8,9,10]),
<a name="line_60"></a>  scaling=constrained,axes = none
<a name="line_61"></a> );
<a name="line_62"></a> save_plot("P_q");
<a name="line_63"></a> save_jpg("P_q");
<a name="line_64"></a> 
<a name="line_65"></a> pics["P_q_alt"] := display(
<a name="line_66"></a>  seq(cplot(q_P(c0[i](t)),t=0..2*Pi,colour=c_colour[i]),i in [7,8,0,1,2,3,4,5,6]),
<a name="line_67"></a>  seq(cpoint(q_P(v0[i])),i in [0,2,3,4,5,6,7,8,9,10]),
<a name="line_68"></a>  view=[-Rq..Rq,-Rq..Rq],axes=none
<a name="line_69"></a> );
<a name="line_70"></a> save_plot("P_q_alt");
<a name="line_71"></a> save_jpg("P_q_alt");
<a name="line_72"></a>
<a name="line_73"></a> pics["P_F16_p"] := display(
<a name="line_74"></a>  seq(cplot(p_P(c0[i](t)),t=F16_curve_limits[i],colour=c_colour[i]),i in [0,1,3,5]),
<a name="line_75"></a>  seq(cpoint(p_P(v0[i])),i in [0,3,6,11]),
<a name="line_76"></a>  scaling=constrained,axes=none
<a name="line_77"></a> );
<a name="line_78"></a> save_plot("P_F16_p");
<a name="line_79"></a> save_jpg("P_F16_p");
<a name="line_80"></a>
<a name="line_81"></a> pics["P_F16_q"] := display(
<a name="line_82"></a>  seq(cplot(q_P(c0[i](t)),t=F16_curve_limits[i],colour=c_colour[i]),i in [0,1,3,5]),
<a name="line_83"></a>  seq(cpoint(q_P(v0[i])),i in [0,3,6,11]),
<a name="line_84"></a>  scaling=constrained,axes=none
<a name="line_85"></a> );
<a name="line_86"></a> save_plot("P_F16_q");
<a name="line_87"></a> save_jpg("P_F16_q");
<a name="line_88"></a>end:
<a name="line_89"></a>
<a name="line_90"></a>######################################################################
<a name="line_91"></a>
<a name="line_92"></a>make_PX_plot_tikz := proc(a0_) 
<a name="line_93"></a> local i,t,a0,v0,c0,mp11,mp13,Rp,mq4,mq5,mv3,mv6,Rq,sp,sq,s;
<a name="line_94"></a> global pics;
<a name="line_95"></a>
<a name="line_96"></a> a0 := `if`(nargs > 0,a0_,a_P0);
<a name="line_97"></a>
<a name="line_98"></a> for i from 0 to 8 do
<a name="line_99"></a>  c0[i] := unapply(evalf(subs(a_P=a0,c_P[i](t))),t);
<a name="line_100"></a> od:
<a name="line_101"></a> for i from 0 to 13 do
<a name="line_102"></a>  v0[i] := evalf(subs(a_P=a0,v_P[i]));
<a name="line_103"></a> od:
<a name="line_104"></a>
<a name="line_105"></a> sp := "\\begin{tikzpicture}[scale=3]\n":
<a name="line_106"></a> sp := cat(sp,sprintf("  \\draw[blue] (0,0) -- (%.3f,0);\n",a0)):
<a name="line_107"></a> sp := cat(sp,sprintf("  \\draw[magenta] (%.3f,0) -- (1,0);\n",a0)):
<a name="line_108"></a> sp := cat(sp,"  \\draw[cyan] (0,0) (1,0) arc(0:90:1);\n"):
<a name="line_109"></a> sp := cat(sp,"  \\draw[green] (0,1) -- (0,0);\n"):
<a name="line_110"></a> sp := cat(sp,sprintf("  \\fill[black] (%.3f,0) circle(0.015);\n",a0)):
<a name="line_111"></a> sp := cat(sp,"  \\fill[black] (0,0) circle(0.015);\n"):
<a name="line_112"></a> sp := cat(sp,"  \\fill[black] (1,0) circle(0.015);\n"):
<a name="line_113"></a> sp := cat(sp,"  \\fill[black] (0,1) circle(0.015);\n"):
<a name="line_114"></a> sp := cat(sp,sprintf("  \\draw (%.3f,%.3f) node[anchor=north] {$\\ss v_0$};\n",0,0)):
<a name="line_115"></a> sp := cat(sp,sprintf("  \\draw (%.3f,%.3f) node[anchor=north] {$\\ss v_{11}$};\n",a0,0)):
<a name="line_116"></a> sp := cat(sp,sprintf("  \\draw (%.3f,%.3f) node[anchor=south] {$\\ss v_6$};\n",0,1)):
<a name="line_117"></a> sp := cat(sp,sprintf("  \\draw (%.3f,%.3f) node[anchor=north] {$\\ss v_3$};\n",1,0)):
<a name="line_118"></a> sp := cat(sp," \\end{tikzpicture}\n"):
<a name="line_119"></a> 
<a name="line_120"></a> save_tikz("P_p",sp);
<a name="line_121"></a>
<a name="line_122"></a> mq4 := maximize(q_P(c0[4](t)),t=0..2*Pi);
<a name="line_123"></a> mq5 := maximize(q_P(c0[5](t)),t=0..2*Pi);
<a name="line_124"></a> mv6 := Re(q_P(v0[6]));
<a name="line_125"></a> mv3 := abs(Im(q_P(v0[3])));
<a name="line_126"></a> 
<a name="line_127"></a> Rq  := 1.2 * max(mq4,mv3,seq(abs(q_P(v0[i])),i=2..13));
<a name="line_128"></a>
<a name="line_129"></a> sq := " \\begin{tikzpicture}[scale=1.5]\n":
<a name="line_130"></a> sq := cat(sq,sprintf("  \\draw[magenta] (0,0) -- (0,%.3f);\n",-mv3)):
<a name="line_131"></a> sq := cat(sq,"  \\draw[cyan] plot[smooth] \n   coordinates{ "):
<a name="line_132"></a> for i from 0 to 12 do
<a name="line_133"></a>  sq := cat(sq,sprintf("(%.3f,%.3f) ",op(C_to_R2(q_P(c0[0](evalf(Pi/4*(1+i/12))))))));
<a name="line_134"></a> od:
<a name="line_135"></a> sq := cat(sq,"};\n"):
<a name="line_136"></a> sq := cat(sq,sprintf("  \\draw[green] (%.3f,%.3f) -- (0,0);\n",mv6,mv6)):
<a name="line_137"></a> sq := cat(sq,sprintf("  \\draw[blue] (0,0) -- (%.3f,0);\n",mq5)):
<a name="line_138"></a> sq := cat(sq,sprintf("  \\fill[black] (%.3f,%.3f) circle(0.03);\n",0,0)):
<a name="line_139"></a> sq := cat(sq,sprintf("  \\fill[black] (%.3f,%.3f) circle(0.03);\n",0,-mv3)):
<a name="line_140"></a> sq := cat(sq,sprintf("  \\fill[black] (%.3f,%.3f) circle(0.03);\n",mv6,mv6)):
<a name="line_141"></a> sq := cat(sq,sprintf("  \\draw (%.3f,%.3f) node[anchor=east] {$\\ss v_0,v_{11}$};\n",0,0)):
<a name="line_142"></a> sq := cat(sq,sprintf("  \\draw (%.3f,%.3f) node[anchor=east] {$\\ss v_3$};\n",0,-mv3)):
<a name="line_143"></a> sq := cat(sq,sprintf("  \\draw (%.3f,%.3f) node[anchor=south] {$\\ss v_6$};\n",mv6,mv6)):
<a name="line_144"></a> sq := cat(sq," \\end{tikzpicture}\n"):
<a name="line_145"></a>
<a name="line_146"></a> save_tikz("P_q",sq);
<a name="line_147"></a>
<a name="line_148"></a> s := cat(
<a name="line_149"></a> "\\begin{center}\n",sp,"\\hspace{4em}",sq,"\\end{center}\n"
<a name="line_150"></a> );
<a name="line_151"></a>
<a name="line_152"></a> save_tikz("P",s);
<a name="line_153"></a>end:
<a name="line_154"></a>
<a name="line_155"></a>
<a name="line_156"></a>######################################################################
<a name="line_157"></a>
<a name="line_158"></a>make_Ep_plots := proc()
<a name="line_159"></a> global pics;
<a name="line_160"></a> local k,p,q,cpv,m0,m1;
<a name="line_161"></a>
<a name="line_162"></a> for k from 0 to 8 do 
<a name="line_163"></a>  cpv[k] := [seq(evalf(c_TEp_approx[k](i*2*Pi/20)),i=-50..50)]:
<a name="line_164"></a> od:
<a name="line_165"></a>
<a name="line_166"></a> pics["Ep_patch"] := display(
<a name="line_167"></a>  seq(seq(seq(
<a name="line_168"></a>   curve(map(C_to_R2,cpv[k] +~ (p*latt_a0 + q*latt_b0)),
<a name="line_169"></a>	 colour=c_colour[k]),
<a name="line_170"></a>  q=-2..2),p=-2..2),k=0..8),
<a name="line_171"></a>  seq(seq(cpoint(p*latt_a0 + q*latt_b0),q=-2..2),p=-2..2),
<a name="line_172"></a>  seq(seq(seq(
<a name="line_173"></a>   cpoint(p*latt_a0 + q*latt_b0 + 
<a name="line_174"></a>          evalf(c_TEp_approx[1]((-1)^k*Pi/2))),
<a name="line_175"></a>   q=-2..2),p=-2..2),k=0..1),
<a name="line_176"></a>  seq(seq(seq(
<a name="line_177"></a>   cpoint(p*latt_a0 + q*latt_b0 + 
<a name="line_178"></a>          evalf(c_TEp_approx[3](k*Pi/2))),
<a name="line_179"></a>   q=-2..2),p=-2..2),k=-1..2),
<a name="line_180"></a>  seq(seq(seq(
<a name="line_181"></a>   cpoint(p*latt_a0 + q*latt_b0 + 
<a name="line_182"></a>          evalf(c_TEp_approx[4](k*Pi/2))),
<a name="line_183"></a>    q=-2..2),p=-2..2),k=-1..2),
<a name="line_184"></a>  axes=none,scaling=constrained,view=[-2..2,-2..2]
<a name="line_185"></a> );
<a name="line_186"></a>
<a name="line_187"></a> m0 := Re(evalf(c_TEp_approx[4](-Pi/2)));
<a name="line_188"></a> m1 := Re(evalf(c_TEp_approx[1]( Pi/2)));
<a name="line_189"></a>
<a name="line_190"></a> pics["Ep_cell"] := display(
<a name="line_191"></a>  cline(-latt_a0/2-latt_b0/2,-latt_a0/2+latt_b0/2,colour=c_colour[3]),
<a name="line_192"></a>  cline( latt_a0/2-latt_b0/2, latt_a0/2+latt_b0/2,colour=c_colour[3]),
<a name="line_193"></a>  cline(-latt_a0/2,  latt_a0/2,colour=c_colour[5]),
<a name="line_194"></a>  cline(-latt_b0/2,  latt_b0/2,colour=c_colour[5]),
<a name="line_195"></a>  cline(-latt_a0/2-latt_b0/2,-m0-latt_b0/2,colour=c_colour[0]),
<a name="line_196"></a>  cline(-latt_a0/2+latt_b0/2,-m0+latt_b0/2,colour=c_colour[0]),
<a name="line_197"></a>  cline( latt_a0/2-latt_b0/2, m0-latt_b0/2,colour=c_colour[0]),
<a name="line_198"></a>  cline( latt_a0/2+latt_b0/2, m0+latt_b0/2,colour=c_colour[0]),
<a name="line_199"></a>  cline(-m0+latt_b0/2, m0+latt_b0/2,colour=c_colour[3]),
<a name="line_200"></a>  cline(-m0-latt_b0/2, m0-latt_b0/2,colour=c_colour[3]),
<a name="line_201"></a>  ccurve(evalf([seq(c_TEp_approx[1](i*Pi/20),i=-10..10)]),colour=c_colour[1]),
<a name="line_202"></a>  ccurve(evalf([seq(c_TEp_approx[2](i*Pi/20),i=-10..10)]),colour=c_colour[1]),
<a name="line_203"></a>  map(cpoint,[seq(seq((p*latt_a0+q*latt_b0)/2,q=-1..1),p=-1..1),
<a name="line_204"></a>	       latt_b0/2-m0, latt_b0/2-m1, latt_b0/2+m0, latt_b0/2+m1,
<a name="line_205"></a>	      -latt_b0/2-m0,-latt_b0/2-m1,-latt_b0/2+m0,-latt_b0/2+m1]),
<a name="line_206"></a>  scaling=constrained,axes=none
<a name="line_207"></a> );
<a name="line_208"></a> 
<a name="line_209"></a> save_plot("Ep_patch");
<a name="line_210"></a> save_plot("Ep_cell");
<a name="line_211"></a> save_jpg("Ep_patch");
<a name="line_212"></a> save_jpg("Ep_cell");
<a name="line_213"></a> NULL;
<a name="line_214"></a>end:
<a name="line_215"></a>
<a name="line_216"></a>######################################################################
<a name="line_217"></a>
<a name="line_218"></a>make_Ep_plot_tikz := proc()
<a name="line_219"></a> global tikz_pics;
<a name="line_220"></a> local t0,t1,t2,t3,s;
<a name="line_221"></a>
<a name="line_222"></a> t0 := latt_a0/2;
<a name="line_223"></a> t1 := latt_b0/(2*I);
<a name="line_224"></a> t2 := Re(c_TEp_approx[4](3*Pi/2));
<a name="line_225"></a> t3 := Re(c_TEp_approx[1](Pi/2));
<a name="line_226"></a>
<a name="line_227"></a> s := cat(
<a name="line_228"></a>   "\\begin{center}\n",
<a name="line_229"></a>   " \\begin{tikzpicture}[scale=4]\n",
<a name="line_230"></a>   tikz_line([ t0,-t1],[ t0, t1],"magenta"),
<a name="line_231"></a>   tikz_line([-t0,-t1],[-t0, t1],"magenta"),
<a name="line_232"></a>   tikz_line([-t0,  0],[ t0,  0],"blue"),
<a name="line_233"></a>   tikz_line([  0,-t1],[  0, t1],"blue"),
<a name="line_234"></a>   tikz_line([-t2, t1],[ t2, t1],"magenta"),
<a name="line_235"></a>   tikz_line([-t2,-t1],[ t2,-t1],"magenta"),
<a name="line_236"></a>   tikz_line([-t0, t1],[-t2, t1],"cyan"),
<a name="line_237"></a>   tikz_line([ t0, t1],[ t2, t1],"cyan"),
<a name="line_238"></a>   tikz_line([-t0,-t1],[-t2,-t1],"cyan"),
<a name="line_239"></a>   tikz_line([ t0,-t1],[ t2,-t1],"cyan"),
<a name="line_240"></a>   tikz_curve([seq(C_to_R2(c_TEp_approx[1](i*Pi/16)),i=-8..8)],green),
<a name="line_241"></a>   tikz_curve([seq(C_to_R2(c_TEp_approx[2](i*Pi/16)),i=-8..8)],green),
<a name="line_242"></a>   seq(tikz_point([ s, t1],0.011),s in [-t0,-t3,-t2,0,t2,t3,t0]),
<a name="line_243"></a>   seq(tikz_point([ s,-t1],0.011),s in [-t0,-t3,-t2,0,t2,t3,t0]),
<a name="line_244"></a>   seq(tikz_point([ s,  0],0.011),s in [-t0,0,t0]),
<a name="line_245"></a>   " \\end{tikzpicture}\n",
<a name="line_246"></a>   "\\end{center}"
<a name="line_247"></a> ):
<a name="line_248"></a>
<a name="line_249"></a> tikz_pics["Ep"] := s;
<a name="line_250"></a> save_tikz("Ep",s);
<a name="line_251"></a> return s;
<a name="line_252"></a>end:
<a name="line_253"></a>
<a name="line_254"></a>######################################################################
<a name="line_255"></a>
<a name="line_256"></a>make_Em_plots := proc()
<a name="line_257"></a> global pics;
<a name="line_258"></a> local k,p,q,cmv,m0,m1;
<a name="line_259"></a>
<a name="line_260"></a> for k from 0 to 8 do 
<a name="line_261"></a>  cmv[k] := [seq(evalf(c_TEm_approx[k](i*2*Pi/20)),i=-50..50)]:
<a name="line_262"></a> od:
<a name="line_263"></a>
<a name="line_264"></a> m0 := evalf(latt_c0/2-c_TEm_approx[0](Pi/2));
<a name="line_265"></a> m1 := evalf(latt_c0/2-c_TEm_approx[0](Pi/4));
<a name="line_266"></a>
<a name="line_267"></a> pics["Em_patch"] := display(
<a name="line_268"></a>  seq(seq(seq(
<a name="line_269"></a>   curve(map(C_to_R2,cmv[k] +~ (p*latt_e0 + q*latt_f0)),
<a name="line_270"></a>         colour=c_colour[k]),
<a name="line_271"></a>   q=-2..2),p=-2..2),k = 0..8),
<a name="line_272"></a>  seq(seq(
<a name="line_273"></a>   cpoint(p*latt_e0/2+q*latt_f0/2),
<a name="line_274"></a>   q=-4..4),p=-4..4),
<a name="line_275"></a>  seq(seq(
<a name="line_276"></a>   cpoint(u+v),
<a name="line_277"></a>    v in [latt_c0/2,-latt_c0/2,latt_d0/2,-latt_d0/2]),
<a name="line_278"></a>     u in [-m1,-m0,m0,m1]),
<a name="line_279"></a>  axes=none,scaling=constrained,
<a name="line_280"></a>  view=[-2..2,-2..2]
<a name="line_281"></a> );
<a name="line_282"></a>
<a name="line_283"></a> pics["Em_cell"] := display(
<a name="line_284"></a>  cline(-latt_c0/2-latt_d0/2,-latt_c0/2+latt_d0/2,colour=c_colour[1]),
<a name="line_285"></a>  cline( latt_c0/2-latt_d0/2, latt_c0/2+latt_d0/2,colour=c_colour[1]),
<a name="line_286"></a>  cline(-latt_d0/2, latt_d0/2,colour=c_colour[1]),
<a name="line_287"></a>  cline( latt_d0/2-m1, latt_d0/2+m1,colour=c_colour[0]),
<a name="line_288"></a>  cline( latt_d0/2-latt_c0/2, latt_d0/2-m1,colour=c_colour[1]),
<a name="line_289"></a>  cline( latt_d0/2+latt_c0/2, latt_d0/2+m1,colour=c_colour[1]),
<a name="line_290"></a>  cline(-latt_d0/2-latt_c0/2,-latt_d0/2-m1,colour=c_colour[1]),
<a name="line_291"></a>  cline(-latt_d0/2+latt_c0/2,-latt_d0/2+m1,colour=c_colour[1]),
<a name="line_292"></a>  cline(-latt_d0/2-m1,-latt_d0/2+m1,colour=c_colour[0]),
<a name="line_293"></a>  cline(-latt_c0/2,-latt_c0/2+m1,colour=c_colour[0]),
<a name="line_294"></a>  cline( latt_c0/2, latt_c0/2-m1,colour=c_colour[0]),
<a name="line_295"></a>  cline(-latt_c0/2+m1, latt_c0/2-m1,colour=c_colour[1]),
<a name="line_296"></a>  ccurve(evalf([seq(c_TEm_approx[3](i*Pi/20),i=-10..30)]),colour=c_colour[3]),
<a name="line_297"></a>  ccurve(evalf([seq(c_TEm_approx[3](i*Pi/20)-latt_e0,i=10..50)]),colour=c_colour[3]),
<a name="line_298"></a>  ccurve(evalf([seq(c_TEm_approx[5](i*Pi/20),i=-40..40)]),colour=c_colour[5]),
<a name="line_299"></a>  ccurve(evalf([seq(c_TEm_approx[6](i*Pi/20),i=-40..40)]),colour=c_colour[5]),
<a name="line_300"></a>  cline(-latt_c0/2, latt_d0/2,colour=orange,linestyle=dash),
<a name="line_301"></a>  cline(-latt_c0/2,-latt_d0/2,colour=orange,linestyle=dash),
<a name="line_302"></a>  cline( latt_c0/2, latt_d0/2,colour=orange,linestyle=dash),
<a name="line_303"></a>  cline( latt_c0/2,-latt_d0/2,colour=orange,linestyle=dash),
<a name="line_304"></a>  map(cpoint,[seq(seq((p*latt_c0+q*latt_d0)/2,q=-1..1),p=-1..1),
<a name="line_305"></a>	       latt_c0/2-m0, latt_c0/2-m1,-latt_c0/2+m0,-latt_c0/2+m1,
<a name="line_306"></a>	       latt_d0/2-m0, latt_d0/2-m1, latt_d0/2+m0, latt_d0/2+m1,
<a name="line_307"></a>	      -latt_d0/2-m0,-latt_d0/2-m1,-latt_d0/2+m0,-latt_d0/2+m1,
<a name="line_308"></a>	       latt_e0/2,latt_f0/2,-latt_e0/2,-latt_f0/2]),
<a name="line_309"></a>  axes=none,scaling=constrained
<a name="line_310"></a> );
<a name="line_311"></a>
<a name="line_312"></a> save_plot("Em_patch");
<a name="line_313"></a> save_plot("Em_cell");
<a name="line_314"></a> save_jpg("Em_patch");
<a name="line_315"></a> save_jpg("Em_cell");
<a name="line_316"></a> NULL;
<a name="line_317"></a>end:
<a name="line_318"></a>
<a name="line_319"></a>######################################################################
<a name="line_320"></a>
<a name="line_321"></a>make_Em_plot_tikz := proc()
<a name="line_322"></a> global tikz_pics;
<a name="line_323"></a> local t0,t1,t2,t3,s,p,q;
<a name="line_324"></a>
<a name="line_325"></a> t0 := latt_c0/2;
<a name="line_326"></a> t1 := latt_d0/(2*I);
<a name="line_327"></a> t2 := evalf(latt_c0/2-c_TEm_approx[0](Pi/4));
<a name="line_328"></a> t3 := evalf(c_TEm_approx[0](Pi/2));
<a name="line_329"></a>
<a name="line_330"></a> s := cat(
<a name="line_331"></a>   "\\begin{center}\n",
<a name="line_332"></a>   " \\begin{tikzpicture}[scale=3]\n",
<a name="line_333"></a>   tikz_line([-t2, t1],[t2, t1],"cyan"),
<a name="line_334"></a>   tikz_line([-t2,-t1],[t2,-t1],"cyan"),
<a name="line_335"></a>   tikz_line([-t0,0],[-t0+t2,0],"cyan"),
<a name="line_336"></a>   tikz_line([ t0,0],[ t0-t2,0],"cyan"),
<a name="line_337"></a>   tikz_line([-t0+t2, 0],[ t0-t2,0],"green"),
<a name="line_338"></a>   tikz_line([ t2, t1],[ t0, t1],"green"),
<a name="line_339"></a>   tikz_line([-t2, t1],[-t0, t1],"green"),
<a name="line_340"></a>   tikz_line([ t2,-t1],[ t0,-t1],"green"),
<a name="line_341"></a>   tikz_line([-t2,-t1],[-t0,-t1],"green"),
<a name="line_342"></a>   seq(tikz_line([s,-t1],[s, t1],"green"),s in [-t0,0,t0]),
<a name="line_343"></a>   tikz_curve([seq(C_to_R2(c_TEm_approx[3]( Pi/2+i*Pi/8)),i=-8..8)],"magenta"),
<a name="line_344"></a>   tikz_curve([seq(C_to_R2(c_TEm_approx[3](-Pi/2+i*Pi/8)-t0+I*t1),i=-8..8)],"magenta"),
<a name="line_345"></a>   tikz_curve([seq(C_to_R2(c_TEm_approx[5](i*Pi/8)),i=-16..16)],"blue"),
<a name="line_346"></a>   tikz_curve([seq(C_to_R2(c_TEm_approx[6](i*Pi/8)),i=-16..16)],"blue"),
<a name="line_347"></a>   seq(seq(tikz_point([p,q],0.01),p in [-t0,-t2,-t0+t3,0, t0-t3,t2,t0]),q in [-t1,t1]),
<a name="line_348"></a>   seq(seq(tikz_point([p,q],0.01),p in [-t0/2,t0/2]),q in [-t1/2,t1/2]),
<a name="line_349"></a>   seq(tikz_point([p,0],0.01),p in [-t0,-t0+t2,-t3,0, t3, t0-t2, t0]),
<a name="line_350"></a>   sprintf(" \\draw[orange,dashed] (%.3f,%.3f) -- (%.3f,%.3f) -- (%.3f,%.3f) -- (%.3f,%.3f) -- cycle;\n",
<a name="line_351"></a>    t0,0,0,t1,-t0,0,0,-t1),
<a name="line_352"></a>   " \\end{tikzpicture}\n",
<a name="line_353"></a>   "\\end{center}"
<a name="line_354"></a>  ):
<a name="line_355"></a>
<a name="line_356"></a> tikz_pics["Em_cell"] := s;
<a name="line_357"></a> save_tikz("Em_cell",s);
<a name="line_358"></a> return s;
<a name="line_359"></a>end:
<a name="line_360"></a>
<a name="line_361"></a>######################################################################
<a name="line_362"></a>
<a name="line_363"></a>make_P_torus_plots := proc()
<a name="line_364"></a> global pics;
<a name="line_365"></a> local k,lp,lm,lmq,cp,cm,cmq,wft;
<a name="line_366"></a> 
<a name="line_367"></a> for k from 0 to 8 do
<a name="line_368"></a>  print(k);
<a name="line_369"></a>  cp  := (t) -> map(exp,2*Pi*I *~ div_Ep (c_TEp_approx[k](t)));
<a name="line_370"></a>  cm  := (t) -> map(exp,2*Pi*I *~ div_Em (c_TEm_approx[k](t)));
<a name="line_371"></a>  cmq := (t) -> map(exp,2*Pi*I *~ div_Emq(c_TEm_approx[k](t)));
<a name="line_372"></a>
<a name="line_373"></a>  lp  := sprintf("c_TEp[%d]",k);
<a name="line_374"></a>  lm  := sprintf("c_TEm[%d]",k);
<a name="line_375"></a>  lmq := sprintf("c_TEmq[%d]",k);
<a name="line_376"></a>  
<a name="line_377"></a>  pics[lp ] := spacecurve(TC_to_R3( cp(t)),t=0..2*Pi,
<a name="line_378"></a>                 colour=c_colour[k],numpoints=400,scaling=constrained,axes=none);
<a name="line_379"></a>  pics[lm ] := spacecurve(TC_to_R3( cm(t)),t=0..2*Pi,
<a name="line_380"></a>                 colour=c_colour[k],numpoints=400,scaling=constrained,axes=none);
<a name="line_381"></a>  pics[lmq] := spacecurve(TC_to_R3(cmq(t)),t=0..2*Pi,
<a name="line_382"></a>                 colour=c_colour[k],numpoints=400,scaling=constrained,axes=none);
<a name="line_383"></a>
<a name="line_384"></a>  save_plot(lp);
<a name="line_385"></a>  save_plot(lm);
<a name="line_386"></a>  save_plot(lmq);
<a name="line_387"></a> od:
<a name="line_388"></a>
<a name="line_389"></a> wft := plot3d(TA_to_R3([t,u]),t=0..2*Pi,u=0..2*Pi,
<a name="line_390"></a>          colour=gray,style=wireframe,scaling=constrained,axes=none);
<a name="line_391"></a>
<a name="line_392"></a> pics["c_TEp" ] := display(wft,seq(pics[sprintf("c_TEp[%d]",k)],
<a name="line_393"></a>                             k in {0,1,3,4,5,6}));
<a name="line_394"></a> pics["c_TEm" ] := display(wft,seq(pics[sprintf("c_TEm[%d]",k)],
<a name="line_395"></a>                             k in {0,1,2,3,5,6}));
<a name="line_396"></a> pics["c_TEmq"] := display(wft,seq(pics[sprintf("c_TEmq[%d]",k)],
<a name="line_397"></a>                             k in {0,1,2,3,5,6}));
<a name="line_398"></a>
<a name="line_399"></a> save_plot("c_TEp");
<a name="line_400"></a> save_plot("c_TEm");
<a name="line_401"></a> save_plot("c_TEmq");
<a name="line_402"></a> save_jpg("c_TEp");
<a name="line_403"></a> save_jpg("c_TEm");
<a name="line_404"></a> save_jpg("c_TEmq");
<a name="line_405"></a>end:
<a name="line_406"></a>
<a name="line_407"></a>######################################################################
<a name="line_408"></a>
<a name="line_409"></a>make_P_sphere_plots := proc()
<a name="line_410"></a> global pics;
<a name="line_411"></a> local wfs;
<a name="line_412"></a>
<a name="line_413"></a> wfs := display(sphere([0,0,0],1,colour=grey,style=wireframe),axes=none):
<a name="line_414"></a>
<a name="line_415"></a> # PX(a)/<LL>
<a name="line_416"></a> pics["SQP_LL"] := 
<a name="line_417"></a> display(wfs,
<a name="line_418"></a>  seq(spacecurve(C_to_S2(p_P(c_P0[k](t))),t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
<a name="line_419"></a>  axes=none,scaling=constrained
<a name="line_420"></a> ):
<a name="line_421"></a>
<a name="line_422"></a> # PX(a)/<L>
<a name="line_423"></a> pics["SQP_L"] := 
<a name="line_424"></a> display(wfs,
<a name="line_425"></a>  seq(spacecurve(C_to_S2(p_C[ 2, 3](p_P(c_P0[k](t)))),
<a name="line_426"></a>      t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
<a name="line_427"></a>  axes=none,scaling=constrained
<a name="line_428"></a> ):
<a name="line_429"></a>
<a name="line_430"></a> # PX(a)/<LL,M>
<a name="line_431"></a> pics["SQP_LL_M"] := 
<a name="line_432"></a> display(wfs,
<a name="line_433"></a>  seq(spacecurve(C_to_S2(p_C[ 2, 8](p_P(c_P0[k](t)))),
<a name="line_434"></a>      t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
<a name="line_435"></a>  axes=none,scaling=constrained
<a name="line_436"></a> ):
<a name="line_437"></a>
<a name="line_438"></a> # PX(a)/<LL,LM>
<a name="line_439"></a> pics["SQP_LL_LM"] := 
<a name="line_440"></a> display(wfs,
<a name="line_441"></a>  seq(spacecurve(C_to_S2(p_C[ 2, 9](p_P(c_P0[k](t)))),
<a name="line_442"></a>      t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
<a name="line_443"></a>  axes=none,scaling=constrained
<a name="line_444"></a> ):
<a name="line_445"></a>
<a name="line_446"></a> # PX(a)/<L,M>
<a name="line_447"></a> pics["SQP_L_M"] := 
<a name="line_448"></a> display(wfs,
<a name="line_449"></a>  seq(spacecurve(C_to_S2(p_C[ 2,10](p_P(c_P0[k](t)))),
<a name="line_450"></a>      t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
<a name="line_451"></a>  axes=none,scaling=constrained
<a name="line_452"></a> ):
<a name="line_453"></a>
<a name="line_454"></a> save_plot("SQP_LL");
<a name="line_455"></a> save_plot("SQP_L");
<a name="line_456"></a> save_plot("SQP_LL_M");
<a name="line_457"></a> save_plot("SQP_LL_LM");
<a name="line_458"></a> save_plot("SQP_L_M");
<a name="line_459"></a>
<a name="line_460"></a> save_jpg("SQP_LL");
<a name="line_461"></a> save_jpg("SQP_L");
<a name="line_462"></a> save_jpg("SQP_LL_M");
<a name="line_463"></a> save_jpg("SQP_LL_LM");
<a name="line_464"></a> save_jpg("SQP_L_M");
<a name="line_465"></a>end:
  </pre>
 </body>
</html>
    