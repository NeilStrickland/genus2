make_PX_plots := proc(a0_)
 local i,t,a0,v0,c0,mp11,mp13,Rp,mq4,mq5,Rq;
 global pics;

 a0 := `if`(nargs > 0,a0_,a_P0);

 for i from 0 to 8 do
  c0[i] := unapply(evalf(subs(a_P=a0,c_P[i](t))),t);
 od:
 for i from 0 to 13 do
  v0[i] := evalf(subs(a_P=a0,v_P[i]));
 od:

 mp11 := p_P(v0[11]);
 mp13 := p_P(v0[13]);
 
 Rp := 1.5 * p_P(v0[13]);
 
 pics["P_p"] := display(
  circle(1,colour=c_colour[0]),
  line([  0,-Rp],[  0, Rp],colour=c_colour[1]),
  line([ mp11,0],[ mp13,0],colour=c_colour[3]),
  line([-mp11,0],[-mp13,0],colour=c_colour[3]),
  line([-mp11,0],[ mp11,0],colour=c_colour[5]),
  line([ mp13,0],[   Rp,0],colour=c_colour[5]),
  line([-mp13,0],[  -Rp,0],colour=c_colour[5]),
  seq(cpoint(p_P(v0[i])),i in [0,2,3,6,7,10,11,12,13]),
  view=[-Rp..Rp,-Rp..Rp],axes=none
 );
 save_plot("P_p");
 save_jpg("P_p");

 pics["P_p_alt"] := display(
  seq(cplot(p_P(c0[i](t)),t=0..2*Pi,colour=c_colour[i]),i=0..8),
  seq(cpoint(p_P(v0[i])),i in [0,2,3,6,7,10,11,12,13]),
  view=[-Rp..Rp,-Rp..Rp],axes=none
 );
 save_plot("P_p_alt");
 save_jpg("P_p_alt");

 mq4 := maximize(q_P(c0[4](t)),t=0..2*Pi);
 mq5 := maximize(q_P(c0[5](t)),t=0..2*Pi);
 Rq  := 1.2 * max(mq4,mq5,seq(abs(q_P(v0[i])),i=2..13));

 pics["P_q"] := display(
  line([-mq5,   0],[ mq5,   0],colour=c_colour[5]),
  line([   0,-mq5],[   0, mq5],colour=c_colour[5]),
  line([ mq5,   0],[ mq4,   0],colour=c_colour[4]),
  line([-mq5,   0],[-mq4,   0],colour=c_colour[4]),
  line([   0, mq5],[   0, mq4],colour=c_colour[4]),
  line([   0,-mq5],[   0,-mq4],colour=c_colour[4]),
  line([ mq4,   0],[  Rq,   0],colour=c_colour[7]),
  line([-mq4,   0],[ -Rq,   0],colour=c_colour[7]),
  line([   0, mq4],[   0,  Rq],colour=c_colour[7]),
  line([   0,-mq4],[   0, -Rq],colour=c_colour[7]),
  line([ -Rq, -Rq],[  Rq,  Rq],colour=c_colour[1]),
  line([ -Rq,  Rq],[  Rq, -Rq],colour=c_colour[1]),
  cplot(q_P(c0[0](t)),t=0..2*Pi,colour=c_colour[0]),
  seq(cpoint(q_P(v0[i])),i in [0,2,3,4,5,6,7,8,9,10]),
  scaling=constrained,axes = none
 );
 save_plot("P_q");
 save_jpg("P_q");
 
 pics["P_q_alt"] := display(
  seq(cplot(q_P(c0[i](t)),t=0..2*Pi,colour=c_colour[i]),i in [7,8,0,1,2,3,4,5,6]),
  seq(cpoint(q_P(v0[i])),i in [0,2,3,4,5,6,7,8,9,10]),
  view=[-Rq..Rq,-Rq..Rq],axes=none
 );
 save_plot("P_q_alt");
 save_jpg("P_q_alt");

 pics["P_F16_p"] := display(
  seq(cplot(p_P(c0[i](t)),t=F16_curve_limits[i],colour=c_colour[i]),i in [0,1,3,5]),
  seq(cpoint(p_P(v0[i])),i in [0,3,6,11]),
  scaling=constrained,axes=none
 );
 save_plot("P_F16_p");
 save_jpg("P_F16_p");

 pics["P_F16_q"] := display(
  seq(cplot(q_P(c0[i](t)),t=F16_curve_limits[i],colour=c_colour[i]),i in [0,1,3,5]),
  seq(cpoint(q_P(v0[i])),i in [0,3,6,11]),
  scaling=constrained,axes=none
 );
 save_plot("P_F16_q");
 save_jpg("P_F16_q");
end:

######################################################################

make_PX_plot_tikz := proc(a0_) 
 local i,t,a0,v0,c0,mp11,mp13,Rp,mq4,mq5,mv3,mv6,Rq,sp,sq,s;
 global pics;

 a0 := `if`(nargs > 0,a0_,a_P0);

 for i from 0 to 8 do
  c0[i] := unapply(evalf(subs(a_P=a0,c_P[i](t))),t);
 od:
 for i from 0 to 13 do
  v0[i] := evalf(subs(a_P=a0,v_P[i]));
 od:

 sp := "\\begin{tikzpicture}[scale=3]\n":
 sp := cat(sp,sprintf("  \\draw[blue] (0,0) -- (%.3f,0);\n",a0)):
 sp := cat(sp,sprintf("  \\draw[magenta] (%.3f,0) -- (1,0);\n",a0)):
 sp := cat(sp,"  \\draw[cyan] (0,0) (1,0) arc(0:90:1);\n"):
 sp := cat(sp,"  \\draw[green] (0,1) -- (0,0);\n"):
 sp := cat(sp,sprintf("  \\fill[black] (%.3f,0) circle(0.015);\n",a0)):
 sp := cat(sp,"  \\fill[black] (0,0) circle(0.015);\n"):
 sp := cat(sp,"  \\fill[black] (1,0) circle(0.015);\n"):
 sp := cat(sp,"  \\fill[black] (0,1) circle(0.015);\n"):
 sp := cat(sp,sprintf("  \\draw (%.3f,%.3f) node[anchor=north] {$\\ss v_0$};\n",0,0)):
 sp := cat(sp,sprintf("  \\draw (%.3f,%.3f) node[anchor=north] {$\\ss v_{11}$};\n",a0,0)):
 sp := cat(sp,sprintf("  \\draw (%.3f,%.3f) node[anchor=south] {$\\ss v_6$};\n",0,1)):
 sp := cat(sp,sprintf("  \\draw (%.3f,%.3f) node[anchor=north] {$\\ss v_3$};\n",1,0)):
 sp := cat(sp," \\end{tikzpicture}\n"):
 
 save_tikz("P_p",sp);

 mq4 := maximize(q_P(c0[4](t)),t=0..2*Pi);
 mq5 := maximize(q_P(c0[5](t)),t=0..2*Pi);
 mv6 := Re(q_P(v0[6]));
 mv3 := abs(Im(q_P(v0[3])));
 
 Rq  := 1.2 * max(mq4,mv3,seq(abs(q_P(v0[i])),i=2..13));

 sq := " \\begin{tikzpicture}[scale=1.5]\n":
 sq := cat(sq,sprintf("  \\draw[magenta] (0,0) -- (0,%.3f);\n",-mv3)):
 sq := cat(sq,"  \\draw[cyan] plot[smooth] \n   coordinates{ "):
 for i from 0 to 12 do
  sq := cat(sq,sprintf("(%.3f,%.3f) ",op(C_to_R2(q_P(c0[0](evalf(Pi/4*(1+i/12))))))));
 od:
 sq := cat(sq,"};\n"):
 sq := cat(sq,sprintf("  \\draw[green] (%.3f,%.3f) -- (0,0);\n",mv6,mv6)):
 sq := cat(sq,sprintf("  \\draw[blue] (0,0) -- (%.3f,0);\n",mq5)):
 sq := cat(sq,sprintf("  \\fill[black] (%.3f,%.3f) circle(0.03);\n",0,0)):
 sq := cat(sq,sprintf("  \\fill[black] (%.3f,%.3f) circle(0.03);\n",0,-mv3)):
 sq := cat(sq,sprintf("  \\fill[black] (%.3f,%.3f) circle(0.03);\n",mv6,mv6)):
 sq := cat(sq,sprintf("  \\draw (%.3f,%.3f) node[anchor=east] {$\\ss v_0,v_{11}$};\n",0,0)):
 sq := cat(sq,sprintf("  \\draw (%.3f,%.3f) node[anchor=east] {$\\ss v_3$};\n",0,-mv3)):
 sq := cat(sq,sprintf("  \\draw (%.3f,%.3f) node[anchor=south] {$\\ss v_6$};\n",mv6,mv6)):
 sq := cat(sq," \\end{tikzpicture}\n"):

 save_tikz("P_q",sq);

 s := cat(
 "\\begin{center}\n",sp,"\\hspace{4em}",sq,"\\end{center}\n"
 );

 save_tikz("P",s);
end:


######################################################################

make_Ep_plots := proc()
 global pics;
 local k,p,q,cpv,m0,m1;

 for k from 0 to 8 do 
  cpv[k] := [seq(evalf(c_TEp_approx[k](i*2*Pi/20)),i=-50..50)]:
 od:

 pics["Ep_patch"] := display(
  seq(seq(seq(
   curve(map(C_to_R2,cpv[k] +~ (p*latt_a0 + q*latt_b0)),
	 colour=c_colour[k]),
  q=-2..2),p=-2..2),k=0..8),
  seq(seq(cpoint(p*latt_a0 + q*latt_b0),q=-2..2),p=-2..2),
  seq(seq(seq(
   cpoint(p*latt_a0 + q*latt_b0 + 
          evalf(c_TEp_approx[1]((-1)^k*Pi/2))),
   q=-2..2),p=-2..2),k=0..1),
  seq(seq(seq(
   cpoint(p*latt_a0 + q*latt_b0 + 
          evalf(c_TEp_approx[3](k*Pi/2))),
   q=-2..2),p=-2..2),k=-1..2),
  seq(seq(seq(
   cpoint(p*latt_a0 + q*latt_b0 + 
          evalf(c_TEp_approx[4](k*Pi/2))),
    q=-2..2),p=-2..2),k=-1..2),
  axes=none,scaling=constrained,view=[-2..2,-2..2]
 );

 m0 := Re(evalf(c_TEp_approx[4](-Pi/2)));
 m1 := Re(evalf(c_TEp_approx[1]( Pi/2)));

 pics["Ep_cell"] := display(
  cline(-latt_a0/2-latt_b0/2,-latt_a0/2+latt_b0/2,colour=c_colour[3]),
  cline( latt_a0/2-latt_b0/2, latt_a0/2+latt_b0/2,colour=c_colour[3]),
  cline(-latt_a0/2,  latt_a0/2,colour=c_colour[5]),
  cline(-latt_b0/2,  latt_b0/2,colour=c_colour[5]),
  cline(-latt_a0/2-latt_b0/2,-m0-latt_b0/2,colour=c_colour[0]),
  cline(-latt_a0/2+latt_b0/2,-m0+latt_b0/2,colour=c_colour[0]),
  cline( latt_a0/2-latt_b0/2, m0-latt_b0/2,colour=c_colour[0]),
  cline( latt_a0/2+latt_b0/2, m0+latt_b0/2,colour=c_colour[0]),
  cline(-m0+latt_b0/2, m0+latt_b0/2,colour=c_colour[3]),
  cline(-m0-latt_b0/2, m0-latt_b0/2,colour=c_colour[3]),
  ccurve(evalf([seq(c_TEp_approx[1](i*Pi/20),i=-10..10)]),colour=c_colour[1]),
  ccurve(evalf([seq(c_TEp_approx[2](i*Pi/20),i=-10..10)]),colour=c_colour[1]),
  map(cpoint,[seq(seq((p*latt_a0+q*latt_b0)/2,q=-1..1),p=-1..1),
	       latt_b0/2-m0, latt_b0/2-m1, latt_b0/2+m0, latt_b0/2+m1,
	      -latt_b0/2-m0,-latt_b0/2-m1,-latt_b0/2+m0,-latt_b0/2+m1]),
  scaling=constrained,axes=none
 );
 
 save_plot("Ep_patch");
 save_plot("Ep_cell");
 save_jpg("Ep_patch");
 save_jpg("Ep_cell");
 NULL;
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

make_Em_plots := proc()
 global pics;
 local k,p,q,cmv,m0,m1;

 for k from 0 to 8 do 
  cmv[k] := [seq(evalf(c_TEm_approx[k](i*2*Pi/20)),i=-50..50)]:
 od:

 m0 := evalf(latt_c0/2-c_TEm_approx[0](Pi/2));
 m1 := evalf(latt_c0/2-c_TEm_approx[0](Pi/4));

 pics["Em_patch"] := display(
  seq(seq(seq(
   curve(map(C_to_R2,cmv[k] +~ (p*latt_e0 + q*latt_f0)),
         colour=c_colour[k]),
   q=-2..2),p=-2..2),k = 0..8),
  seq(seq(
   cpoint(p*latt_e0/2+q*latt_f0/2),
   q=-4..4),p=-4..4),
  seq(seq(
   cpoint(u+v),
    v in [latt_c0/2,-latt_c0/2,latt_d0/2,-latt_d0/2]),
     u in [-m1,-m0,m0,m1]),
  axes=none,scaling=constrained,
  view=[-2..2,-2..2]
 );

 pics["Em_cell"] := display(
  cline(-latt_c0/2-latt_d0/2,-latt_c0/2+latt_d0/2,colour=c_colour[1]),
  cline( latt_c0/2-latt_d0/2, latt_c0/2+latt_d0/2,colour=c_colour[1]),
  cline(-latt_d0/2, latt_d0/2,colour=c_colour[1]),
  cline( latt_d0/2-m1, latt_d0/2+m1,colour=c_colour[0]),
  cline( latt_d0/2-latt_c0/2, latt_d0/2-m1,colour=c_colour[1]),
  cline( latt_d0/2+latt_c0/2, latt_d0/2+m1,colour=c_colour[1]),
  cline(-latt_d0/2-latt_c0/2,-latt_d0/2-m1,colour=c_colour[1]),
  cline(-latt_d0/2+latt_c0/2,-latt_d0/2+m1,colour=c_colour[1]),
  cline(-latt_d0/2-m1,-latt_d0/2+m1,colour=c_colour[0]),
  cline(-latt_c0/2,-latt_c0/2+m1,colour=c_colour[0]),
  cline( latt_c0/2, latt_c0/2-m1,colour=c_colour[0]),
  cline(-latt_c0/2+m1, latt_c0/2-m1,colour=c_colour[1]),
  ccurve(evalf([seq(c_TEm_approx[3](i*Pi/20),i=-10..30)]),colour=c_colour[3]),
  ccurve(evalf([seq(c_TEm_approx[3](i*Pi/20)-latt_e0,i=10..50)]),colour=c_colour[3]),
  ccurve(evalf([seq(c_TEm_approx[5](i*Pi/20),i=-40..40)]),colour=c_colour[5]),
  ccurve(evalf([seq(c_TEm_approx[6](i*Pi/20),i=-40..40)]),colour=c_colour[5]),
  cline(-latt_c0/2, latt_d0/2,colour=orange,linestyle=dash),
  cline(-latt_c0/2,-latt_d0/2,colour=orange,linestyle=dash),
  cline( latt_c0/2, latt_d0/2,colour=orange,linestyle=dash),
  cline( latt_c0/2,-latt_d0/2,colour=orange,linestyle=dash),
  map(cpoint,[seq(seq((p*latt_c0+q*latt_d0)/2,q=-1..1),p=-1..1),
	       latt_c0/2-m0, latt_c0/2-m1,-latt_c0/2+m0,-latt_c0/2+m1,
	       latt_d0/2-m0, latt_d0/2-m1, latt_d0/2+m0, latt_d0/2+m1,
	      -latt_d0/2-m0,-latt_d0/2-m1,-latt_d0/2+m0,-latt_d0/2+m1,
	       latt_e0/2,latt_f0/2,-latt_e0/2,-latt_f0/2]),
  axes=none,scaling=constrained
 );

 save_plot("Em_patch");
 save_plot("Em_cell");
 save_jpg("Em_patch");
 save_jpg("Em_cell");
 NULL;
end:

######################################################################

make_Em_plot_tikz := proc()
 global tikz_pics;
 local t0,t1,t2,t3,s,p,q;

 t0 := latt_c0/2;
 t1 := latt_d0/(2*I);
 t2 := evalf(latt_c0/2-c_TEm_approx[0](Pi/4));
 t3 := evalf(c_TEm_approx[0](Pi/2));

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

 tikz_pics["Em_cell"] := s;
 save_tikz("Em_cell",s);
 return s;
end:

######################################################################

make_P_torus_plots := proc()
 global pics;
 local k,lp,lm,lmq,cp,cm,cmq,wft;
 
 for k from 0 to 8 do
  print(k);
  cp  := (t) -> map(exp,2*Pi*I *~ div_Ep (c_TEp_approx[k](t)));
  cm  := (t) -> map(exp,2*Pi*I *~ div_Em (c_TEm_approx[k](t)));
  cmq := (t) -> map(exp,2*Pi*I *~ div_Emq(c_TEm_approx[k](t)));

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

 wft := plot3d(TA_to_R3([t,u]),t=0..2*Pi,u=0..2*Pi,
          colour=gray,style=wireframe,scaling=constrained,axes=none);

 pics["c_TEp" ] := display(wft,seq(pics[sprintf("c_TEp[%d]",k)],
                             k in {0,1,3,4,5,6}));
 pics["c_TEm" ] := display(wft,seq(pics[sprintf("c_TEm[%d]",k)],
                             k in {0,1,2,3,5,6}));
 pics["c_TEmq"] := display(wft,seq(pics[sprintf("c_TEmq[%d]",k)],
                             k in {0,1,2,3,5,6}));

 save_plot("c_TEp");
 save_plot("c_TEm");
 save_plot("c_TEmq");
 save_jpg("c_TEp");
 save_jpg("c_TEm");
 save_jpg("c_TEmq");
end:

######################################################################

make_P_sphere_plots := proc()
 global pics;
 local wfs;

 wfs := display(sphere([0,0,0],1,colour=grey,style=wireframe),axes=none):

 # PX(a)/<LL>
 pics["SQP_LL"] := 
 display(wfs,
  seq(spacecurve(C_to_S2(p_P(c_P0[k](t))),t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
  axes=none,scaling=constrained
 ):

 # PX(a)/<L>
 pics["SQP_L"] := 
 display(wfs,
  seq(spacecurve(C_to_S2(p_C[ 2, 3](p_P(c_P0[k](t)))),
      t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
  axes=none,scaling=constrained
 ):

 # PX(a)/<LL,M>
 pics["SQP_LL_M"] := 
 display(wfs,
  seq(spacecurve(C_to_S2(p_C[ 2, 8](p_P(c_P0[k](t)))),
      t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
  axes=none,scaling=constrained
 ):

 # PX(a)/<LL,LM>
 pics["SQP_LL_LM"] := 
 display(wfs,
  seq(spacecurve(C_to_S2(p_C[ 2, 9](p_P(c_P0[k](t)))),
      t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
  axes=none,scaling=constrained
 ):

 # PX(a)/<L,M>
 pics["SQP_L_M"] := 
 display(wfs,
  seq(spacecurve(C_to_S2(p_C[ 2,10](p_P(c_P0[k](t)))),
      t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
  axes=none,scaling=constrained
 ):

 save_plot("SQP_LL");
 save_plot("SQP_L");
 save_plot("SQP_LL_M");
 save_plot("SQP_LL_LM");
 save_plot("SQP_L_M");

 save_jpg("SQP_LL");
 save_jpg("SQP_L");
 save_jpg("SQP_LL_M");
 save_jpg("SQP_LL_LM");
 save_jpg("SQP_L_M");
end:
