make_PX_plots := proc()
 local t;
 global pics;
 
 pics["P_p"] := display(
  seq(cplot(p_P(c_P0[i](t)),t=0..2*Pi,colour=c_colour[i]),i=0..8),
  view=[-2..2,-2..2],axes=none
 );
 save_plot("P_p");
 save_jpg("P_p");

 pics["P_q"] := display(
  seq(cplot(q_P(c_P0[i](t)),t=0..2*Pi,colour=c_colour[i]),i=0..8),
  view=[-4..4,-4..4],axes=none
 );
 save_plot("P_q");
 save_jpg("P_q");

 pics["P_F16_p"] := display(
  seq(cplot(p_P(c_P0[i](t)),t=F16_curve_limits[i],colour=c_colour[i]),i in [0,1,3,5]),
  scaling=constrained,axes=none
 );
 save_plot("P_F16_p");
 save_jpg("P_F16_p");

 pics["P_F16_q"] := display(
  seq(cplot(q_P(c_P0[i](t)),t=F16_curve_limits[i],colour=c_colour[i]),i in [0,1,3,5]),
  scaling=constrained,axes=none
 );
 save_plot("P_F16_q");
 save_jpg("P_F16_q");
end:

######################################################################

make_PX_plot_tikz := proc() 
 local MM,mm1,mm2,mm3,s,i,t;

 MM := seq([Re(F(exp(I*t))),Im(F(exp(I*t)))],t=0..evalf(Pi/2),evalf(Pi/20)):
 mm1 := maximize(subs(a_P=a_P0,Q(c_P[5](t))),t=0..Pi):
 mm2 := abs(MM[1][2]):
 mm3 := MM[11][1]:
 s := "\\[\n \\begin{tikzpicture}[scale=4]\n":
 s := cat(s,sprintf("  \\draw[blue] (0,0) -- (%.3f,0);\n",a_P0)):
 s := cat(s,sprintf("  \\draw[magenta] (%.3f,0) -- (1,0);\n",a_P0)):
 s := cat(s,"  \\draw[cyan] (0,0) (1,0) arc(0:90:1);\n"):
 s := cat(s,"  \\draw[green] (0,1) -- (0,0);\n"):
 s := cat(s,sprintf("  \\fill[black] (%.3f,0) circle(0.015);\n",a_P0)):
 s := cat(s,"  \\fill[black] (0,0) circle(0.015);\n"):
 s := cat(s,"  \\fill[black] (1,0) circle(0.015);\n"):
 s := cat(s,"  \\fill[black] (0,1) circle(0.015);\n"):
 s := cat(s,sprintf("  \\draw (%.3f,%.3f) node[anchor=north] {$\\ss v_0$};\n",0,0)):
 s := cat(s,sprintf("  \\draw (%.3f,%.3f) node[anchor=north] {$\\ss v_{11}$};\n",a_P0,0)):
 s := cat(s,sprintf("  \\draw (%.3f,%.3f) node[anchor=south] {$\\ss v_6$};\n",0,1)):
 s := cat(s,sprintf("  \\draw (%.3f,%.3f) node[anchor=north] {$\\ss v_3$};\n",1,0)):
 s := cat(s," \\end{tikzpicture}\n \\hspace{10em}\n"):
 s := cat(s," \\begin{tikzpicture}[scale=2]\n"):
 s := cat(s,sprintf("  \\draw[magenta] (0,0) -- (0,%.3f);\n",-mm2)):
 s := cat(s,"  \\draw[cyan] plot[smooth] \n   coordinates{ "):
 for i from 1 to 11 do
  s := cat(s,sprintf("(%.3f,%.3f) ",MM[i][1],MM[i][2]));
 od:
 s := cat(s,"};\n"):
 s := cat(s,sprintf("  \\draw[green] (%.3f,%.3f) -- (0,0);\n",mm3,mm3)):
 s := cat(s,sprintf("  \\draw[blue] (0,0) -- (%.3f,0);\n",mm1)):
 s := cat(s,sprintf("  \\fill[black] (%.3f,%.3f) circle(0.03);\n",0,0)):
 s := cat(s,sprintf("  \\fill[black] (%.3f,%.3f) circle(0.03);\n",0,-mm2)):
 s := cat(s,sprintf("  \\fill[black] (%.3f,%.3f) circle(0.03);\n",mm3,mm3)):
 s := cat(s,sprintf("  \\draw (%.3f,%.3f) node[anchor=east] {$\\ss v_0,v_{11}$};\n",0,0)):
 s := cat(s,sprintf("  \\draw (%.3f,%.3f) node[anchor=east] {$\\ss v_3$};\n",0,-mm2)):
 s := cat(s,sprintf("  \\draw (%.3f,%.3f) node[anchor=south] {$\\ss v_6$};\n",mm3,mm3)):
 s := cat(s," \\end{tikzpicture}\n"):
 s := cat(s,"\\]\n"):

 save_tikz("P",s);
end:


######################################################################

make_P_torus_plots := proc()
 global pics;
 local k,lp,lm,lmq,cp,cm,cmq,wft;
 
 for k from 0 to 8 do
  print(k);
  lp  := sprintf("c_TEp[%d] flat",k);
  lm  := sprintf("c_TEm[%d] flat",k);
  lmq := sprintf("c_TEmq[%d] flat",k);
  
  cp  := (t) -> map(exp,2*Pi*I *~ div_Ep (c_TEp_approx[k](t)));
  cm  := (t) -> map(exp,2*Pi*I *~ div_Em (c_TEm_approx[k](t)));
  cmq := (t) -> map(exp,2*Pi*I *~ div_Emq(c_TEm_approx[k](t)));

  pics[lp ] := torus_box_plot( cp(t),colour=c_colour[k]);
  pics[lm ] := torus_box_plot( cm(t),colour=c_colour[k]);
  pics[lmq] := torus_box_plot(cmq(t),colour=c_colour[k]);

  save_plot(lp);
  save_plot(lm);
  save_plot(lmq);

  lp  := sprintf("c_TEp[%d]",k);
  lm  := sprintf("c_TEm[%d]",k);
  lmq := sprintf("c_TEmq[%d]",k);
  
  pics[lp ] := spacecurve(TC_to_R3( cp(t)),t=0..2*Pi,
                 colour=c_colour[k],numpoints=400,scaling=constrained,axes=none);
  pics[lm ] := spacecurve(TC_to_R3( cm(t)),t=0..2*Pi,
                 colour=c_colour[k],numpoints=400,scaling=constrained,axes=none);
  pics[lmq] := spacecurve(TC_to_R3(cmq(t)),t=0..2*Pi,
                 colour=c_colour[k],numpoints=400,scaling=constrained,axes=none);

  save_plot(lp);
  save_plot(lm);
  save_plot(lmq);
 od:

 pics["c_TEp flat"] := display(seq(pics[sprintf("c_TEp[%d] flat",k)],
                             k in {0,1,3,4,5,6}));
 pics["c_TEm flat"] := display(seq(pics[sprintf("c_TEm[%d] flat",k)],
                             k in {0,1,2,3,5,6}));
 pics["c_TEmq flat"] := display(seq(pics[sprintf("c_TEmq[%d] flat",k)],
                             k in {0,1,2,3,5,6}));

 wft := plot3d(angles_to_R3(t,u),t=0..1,u=0..1,
          colour=gray,style=wireframe,scaling=constrained,axes=none);

 pics["c_TEp" ] := display(wft,seq(pics[sprintf("c_TEp[%d]",k)],
                             k in {0,1,3,4,5,6}));
 pics["c_TEm" ] := display(wft,seq(pics[sprintf("c_TEm[%d]",k)],
                             k in {0,1,2,3,5,6}));
 pics["c_TEmq"] := display(wft,seq(pics[sprintf("c_TEmq[%d]",k)],
                             k in {0,1,2,3,5,6}));

 save_plot("c_TEp flat");
 save_plot("c_TEm flat");
 save_plot("c_TEmq flat");
 save_plot("c_TEp");
 save_plot("c_TEm");
 save_plot("c_TEmq");
end:

######################################################################

make_Ep_plot_tikz := proc()
 global tikz_pics;
 local t0,t1,t2,t3,s;

 t0 := latt_a0/2;
 t1 := latt_b0/(2*I);
 t2 := Re(c_TEp_approx[4](3*Pi/2));
 t3 := Re(c_TEp_approx[1](Pi/2));

 s := cat(
   "\\begin{center}\n",
   " \\begin{tikzpicture}[scale=4]\n",
   tikz_line([ t0,-t1],[ t0, t1],"magenta"),
   tikz_line([-t0,-t1],[-t0, t1],"magenta"),
   tikz_line([-t0,  0],[ t0,  0],"blue"),
   tikz_line([  0,-t1],[  0, t1],"blue"),
   tikz_line([-t2, t1],[ t2, t1],"magenta"),
   tikz_line([-t2,-t1],[ t2,-t1],"magenta"),
   tikz_line([-t0, t1],[-t2, t1],"cyan"),
   tikz_line([ t0, t1],[ t2, t1],"cyan"),
   tikz_line([-t0,-t1],[-t2,-t1],"cyan"),
   tikz_line([ t0,-t1],[ t2,-t1],"cyan"),
   tikz_curve([seq(C_to_R2(c_TEp_approx[1](i*Pi/16)),i=-8..8)],green),
   tikz_curve([seq(C_to_R2(c_TEp_approx[2](i*Pi/16)),i=-8..8)],green),
   seq(tikz_point([ s, t1],0.011),s in [-t0,-t3,-t2,0,t2,t3,t0]),
   seq(tikz_point([ s,-t1],0.011),s in [-t0,-t3,-t2,0,t2,t3,t0]),
   seq(tikz_point([ s,  0],0.011),s in [-t0,0,t0]),
   " \\end{tikzpicture}\n",
   "\\end{center}"
 ):

 tikz_pics["Ep"] := s;
 save_tikz("Ep",s);
 return s;
end:

######################################################################

make_Em_plot_tikz := proc()
 global tikz_pics;
 local t0,t1,t2,t3,s,p,q;

 s := cat(
   "\\begin{center}\n",
   " \\begin{tikzpicture}[scale=3]\n",
   tikz_line([-t2, t1],[t2, t1],"cyan"),
   tikz_line([-t2,-t1],[t2,-t1],"cyan"),
   tikz_line([-t0,0],[-t0+t2,0],"cyan"),
   tikz_line([ t0,0],[ t0-t2,0],"cyan"),
   tikz_line([-t0+t2, 0],[ t0-t2,0],"green"),
   tikz_line([ t2, t1],[ t0, t1],"green"),
   tikz_line([-t2, t1],[-t0, t1],"green"),
   tikz_line([ t2,-t1],[ t0,-t1],"green"),
   tikz_line([-t2,-t1],[-t0,-t1],"green"),
   seq(tikz_line([s,-t1],[s, t1],"green"),s in [-t0,0,t0]),
   tikz_curve([seq(C_to_R2(c_TEm_approx[3]( Pi/2+i*Pi/8)),i=-8..8)],"magenta"),
   tikz_curve([seq(C_to_R2(c_TEm_approx[3](-Pi/2+i*Pi/8)-t0+I*t1),i=-8..8)],"magenta"),
   tikz_curve([seq(C_to_R2(c_TEm_approx[5](i*Pi/8)),i=-16..16)],"blue"),
   tikz_curve([seq(C_to_R2(c_TEm_approx[6](i*Pi/8)),i=-16..16)],"blue"),
   seq(seq(tikz_point([p,q],0.01),p in [-t0,-t2,-t0+t3,0, t0-t3,t2,t0]),q in [-t1,t1]),
   seq(seq(tikz_point([p,q],0.01),p in [-t0/2,t0/2]),q in [-t1/2,t1/2]),
   seq(tikz_point([p,0],0.01),p in [-t0,-t0+t2,-t3,0, t3, t0-t2, t0]),
   sprintf(" \\draw[orange,dashed] (%.3f,%.3f) -- (%.3f,%.3f) -- (%.3f,%.3f) -- (%.3f,%.3f) -- cycle;\n",
    t0,0,0,t1,-t0,0,0,-t1),
   " \\end{tikzpicture}\n",
   "\\end{center}"
  ):

 tikz_pics["Em"] := s;
 save_tikz("Em",s);
 return s;
end:

make_P_sphere_plots := proc()
 global pics;
 local wfs;

 wfs := display(sphere([0,0,0],1,colour=grey,style=wireframe),axes=none):

 pics["SQP 1"] := 
 display(wfs,
  seq(spacecurve(C_to_S2(p_P(c_P0[k](t))),t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
  axes=none,scaling=constrained
 ):

 pics["SQP 2"] := 
 display(wfs,
  seq(spacecurve(C_to_S2(p12_C(p_P(c_P0[k](t)))),t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
  axes=none,scaling=constrained
 ):

 pics["SQP 3"] := 
 display(wfs,
  seq(spacecurve(C_to_S2(p13_C(p_P(c_P0[k](t)))),t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
  axes=none,scaling=constrained
 ):

 pics["SQP 4"] := 
 display(wfs,
  seq(spacecurve(C_to_S2(p14_C(p_P(c_P0[k](t)))),t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
  axes=none,scaling=constrained
 ):

 pics["SQP 5"] := 
 display(wfs,
  seq(spacecurve(C_to_S2(p15_C(p_P(c_P0[k](t)))),t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
  axes=none,scaling=constrained
 ):

 save_plot("SQP 1");
 save_plot("SQP 2");
 save_plot("SQP 3");
 save_plot("SQP 4");
 save_plot("SQP 5");

end:
