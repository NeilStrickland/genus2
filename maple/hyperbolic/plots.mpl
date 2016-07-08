######################################################################

# Convenience functions for plotting.

#@ xi_curve_xy
xi_curve_xy := proc(p,s)
 local z;
 z := xi_curve(p,s);
 return([Re(z),Im(z)]);
end:

#@ xi_arc
xi_arc := proc(p,a,b) 
 plot(subs(a_H=a_H0,[Re(xi_curve(p,s)),Im(xi_curve(p,s)),s=a..b]),numpoints=200,args[4..-1]);
end:

#@ xi_circle
xi_circle := proc(p) 
 local q,t;
 q := p * (1 - sqrt(1-1/abs(p)^2) * exp(I*t));
 plot([Re(q),Im(q),t=-arcsin(1/abs(p)) .. arcsin(1/abs(p))],args[2..-1]);
end:

#@ xi_curve_tikz
xi_curve_tikz := proc(p,col_)
 local p0,r,d,x0,x1,y0,y1,m0,m1,theta,phi,col;

 if nargs > 1 then
  if type(col_,string) then
   col := sprintf("[%s]",col_);
  else
   col := sprintf("[%a]",col_);
  fi;
 else
  col := "";
 fi;

 p0 := evalf(subs(a_H=a_H0,p)); 
 r := evalf(abs(p0));
 d := sqrt(r^2-1);
 theta := evalf(argument(-p0));
 phi   := evalf(arccos(d/r));
 x0 := Re(p0);
 y0 := Im(p0);
 x1 := x0 + d * cos(theta-phi);
 y1 := y0 + d * sin(theta-phi);
 m0 := round(evalf((theta-phi)*180/Pi));
 m1 := round(evalf((theta+phi)*180/Pi));
 sprintf("  \\draw%s (%.3f,%.3f) (%.3f,%.3f) arc(%d:%d:%.3f);\n",
         col,x0,y0,x1,y1,m0,m1,d);
end:

######################################################################

#@ make_HX_plot_tikz
make_HX_plot_tikz := proc()
 local A,s,i,j,k,ii,x,y,col,c_offset,v_offset;
 global hyp_plot_tikz;

 A := map(op,[indices(v_HX)]);
 A := remove(i -> (i = evalf(floor(i)) and not(type(i,integer))),A);
 print(A);

 s := cat(
  "\\begin{center}\n",
  " \\begin{tikzpicture}[scale=7]\n",
  "  \\draw (0,0) circle(1);\n",
  "  \\draw[blue] (-1,0) -- (1,0);\n",
  "  \\draw[blue] (0,-1) -- (0,1);\n",
  "  \\draw[green] (-0.707,-0.707) -- ( 0.707, 0.707);\n",
  "  \\draw[green] (-0.707, 0.707) -- ( 0.707,-0.707);\n"
 );

 for k in [0,3,4,7,8] do
  s := cat(s,xi_curve_tikz(c_p[k],c_colour[k]));
  col := sprintf("%a,dotted",c_colour[k]);
  for j from 1 to 3 do
   s := cat(s,xi_curve_tikz(I^j*c_p[k],col));
  od:
 od:

 for i in A do
  if type(i,integer) then
   s := sprintf("%s  \\fill[black](%.3f,%.3f) circle(0.007);\n",
                s,Re(v_HX1[i]),Im(v_HX1[i]));
  else
   s := sprintf("%s  \\fill[gray!60](%.3f,%.3f) circle(0.007);\n",
                s,Re(v_HX1[i]),Im(v_HX1[i]));
  fi;
 od;

 s := cat(s,
  "  \\draw( 0.30, 0.25) node{$c_1$};\n",
  "  \\draw(-0.30, 0.25) node{$c_2$};\n",
  "  \\draw( 0.80,-0.03) node{$c_5$};\n",
  "  \\draw(-0.05, 0.80) node{$c_6$};\n"
 );

 c_offset[0] := [ 0.10,-0.04];
 c_offset[3] := [-0.09,-0.15];
 c_offset[4] := [ 0.10,-0.01];
 c_offset[7] := [ 0.00,-0.03];
 c_offset[8] := [-0.03, 0.00];

 for k in [0,3,4,7,8] do
  x := evalf(Re(c_HX1[k](0.5)));
  y := evalf(Im(c_HX1[k](0.5)));
  x := x + c_offset[k][1];
  y := y + c_offset[k][2];
  s := sprintf("%s  \\draw(%.3f,%.3f) node{$c_{%d}$};\n",
               s,x,y,k);
 od:

 v_offset[ 0  ] := [ 0.06, 0.02];
 v_offset[ 1  ] := [ 0.03,-0.03];
 v_offset[ 1.1] := [-0.05,-0.03];
 v_offset[ 2  ] := [-0.05, 0.01];
 v_offset[ 2.1] := [ 0.06, 0.01];
 v_offset[ 3  ] := [-0.05, 0.00];
 v_offset[ 3.1] := [-0.05,-0.01];
 v_offset[ 4  ] := [-0.05, 0.00];
 v_offset[ 4.1] := [-0.06, 0.00];
 v_offset[ 5  ] := [ 0.05, 0.00];
 v_offset[ 5.1] := [-0.05, 0.00];
 v_offset[ 6  ] := [-0.04, 0.00];
 v_offset[ 7  ] := [ 0.04, 0.00];
 v_offset[ 8  ] := [ 0.03,-0.03];
 v_offset[ 9  ] := [-0.03,-0.03];
 v_offset[10  ] := [-0.04,-0.03];
 v_offset[10.1] := [-0.06,-0.03];
 v_offset[11  ] := [-0.03,-0.03];
 v_offset[11.1] := [ 0.06,-0.03];
 v_offset[12  ] := [ 0.05, 0.00];
 v_offset[12.1] := [-0.06, 0.00];
 v_offset[12.2] := [-0.06, 0.00];
 v_offset[12.3] := [ 0.06, 0.00];
 v_offset[13  ] := [-0.05, 0.00];
 v_offset[13.1] := [-0.06, 0.00];
 v_offset[13.2] := [ 0.06, 0.00];
 v_offset[13.3] := [-0.06, 0.00];
 v_offset[14  ] := [ 0.00, 0.00];

 for i in A do
  x := Re(v_HX1[i]);
  y := Im(v_HX1[i]);
  if not(type(v_offset[i],indexed)) then
   x := x + v_offset[i][1];
   y := y + v_offset[i][2];
  fi;
  s := sprintf("%s  \\draw(%.3f,%.3f) node{$v_{%a}$};\n",
               s,x,y,i);
 od;

 s := cat(s,
  " \\end{tikzpicture}\n",
  "\\end{center}"
 );

 save_tikz("HX",s);
end:

######################################################################

#@ make_p_dependence_plot_tikz
make_p_dependence_plot_tikz := proc(p)
 local s,k,d,phi,m;

 s := cat(
  " \\begin{tikzpicture}[scale=3]\n",
  "  \\draw (0,0) (1,0) arc(0:90:1);\n",
  "  \\draw[blue] (0,1) -- (0,0) -- (1,0);\n",
  "  \\draw[green] (0,0) -- ( 0.707, 0.707);\n"
 );

 for k in [0,7,8] do
  s := cat(s,xi_curve_tikz(evalf(subs(period=p,c_p[k])),c_colour[k]));
 od:
 
 d := evalf(sqrt(1/p^2-1));
 phi := evalf(arccos(d*p));
 m := round(evalf(phi * 180/Pi));
 s := sprintf("%s  \\draw[magenta](%.3f,0) (%.3f,0) arc(180:%d:%.3f);\n",
              s,evalf(1/p),evalf(1/p-d),180-m,d);
 s := sprintf("%s  \\draw[magenta](0,%.3f) (0,%.3f) arc(270:%d:%.3f);\n",
              s,evalf(1/p),evalf(1/p-d),270+m,d);

 s := cat(s,
  " \\end{tikzpicture}\n"
 );

 save_tikz("p_dependence",s);
end:

######################################################################

#@ make_F1_plot_tikz
make_F1_plot_tikz := proc()
 local theta,r0,r1,m0,m1,arcs,vertices,a,s;
 global F1_arcs,F1_theta,F1_tikz;
 
 r0 := period_a;
 r1 := (2*period^2-1)/(2*period);

 vertices := [
  [13  ,"west"],
  [ 1  ,"south west"],
  [12.2,"south"],
  [12  ,"south"],
  [ 1.1,"south east"],
  [13.3,"east"],
  [13.1,"east"],
  [ 1.2,"north east"],
  [12.3,"north"],
  [12.1,"north"],
  [ 1.3,"north west"],
  [13.2,"west"]
 ];

 arcs := evalf(subs(a_H = a_H0,F1_arcs));

 s := cat(
  "\\begin{center}\n",
  " \\begin{tikzpicture}[scale=5]\n"
 );

 for a in arcs do
  m0 := round(evalf(a[4]*180/Pi));
  m1 := round(evalf(a[5]*180/Pi));
  s := sprintf("%s  \\draw[%a](%.3f,%.3f) +(%d:%.3f) arc(%d:%d:%.3f);\n",
               s,a[6],a[1],a[2],m0,a[3],m0,m1,a[3]);
 od;

 for a in vertices do
  s := sprintf("%s  \\fill(%.3f,%.3f) circle(0.01);\n",
               s,Re(v_HX1[a[1]]),Im(v_HX1[a[1]]));
 od:

 for a in vertices do
  s := sprintf("%s  \\draw(%.3f,%.3f) node[anchor=%s]{$v_{%a}$};\n",
               s,Re(v_HX1[a[1]]),Im(v_HX1[a[1]]),a[2],a[1]);
 od:

 s := cat(s,
  " \\end{tikzpicture}\n",
  "\\end{center}"
 );

 F1_tikz := s;

 save_tikz("F1",s);
# display(
#  op(map(a -> plot([a[1]+a[3]*cos(t),a[2]+a[3]*sin(t),t=a[4]..a[5]],colour=a[6]),
#      arcs)),
#  axes=none
# );
end:

######################################################################

#@ beta_offset_latex
beta_offset_latex := proc()
 local i,j,k,s,t,A;

 s := "\\begin{align*}\n";

 for i from 0 to 13 do
  j := act_V[L](i);
  t := sprintf(" \\lm(v_{%2d}) &= ",i);
  A := v_beta_offset[L,i];
  for k in A do
   t := sprintf("%s\\bt_%d(",t,k);
  od:
  t := sprintf("%sv_{%2d}",t,j);
  for k in A do
   t := cat(t,")");
  od:
  t := cat(t," &\n");
  s := cat(s,t);

  j := act_V[M](i);
  t := sprintf(" \\mu(v_{%2d}) &= ",i);
  A := v_beta_offset[M,i];
  for k in A do
   t := sprintf("%s\\bt_%d(",t,k);
  od:
  t := sprintf("%sv_{%2d}",t,j);
  for k in A do
   t := cat(t,")");
  od:
  t := cat(t," &\n");
  s := cat(s,t);

  j := act_V[N](i);
  t := sprintf(" \\nu(v_{%2d}) &= ",i);
  A := v_beta_offset[N,i];
  for k in A do
   t := sprintf("%s\\bt_%d(",t,k);
  od:
  t := sprintf("%sv_{%2d}",t,j);
  for k in A do
   t := cat(t,")");
  od:
  t := cat(t," \\\\\n");
  s := cat(s,t);
 od;

 s := cat(s,"\\end{align*}\n");

 return(s);
end:

######################################################################

#@ make_c_H_plots
make_c_H_plots := proc()
 local i;
 global pics,c_H_plot;

 c_H_plot[1] := line([-1,-1] /~ sqrt(2.),[ 1, 1] /~ sqrt(2.),colour=c_colour[1]);
 c_H_plot[2] := line([-1, 1] /~ sqrt(2.),[ 1,-1] /~ sqrt(2.),colour=c_colour[2]);

 c_H_plot[5] := line([-1, 0],[ 1, 0],colour=c_colour[5]);
 c_H_plot[6] := line([ 0,-1],[ 0, 1],colour=c_colour[6]);

 for i in [0,3,4,7,8] do
  c_H_plot[i] := xi_circle(c_H_p0[i],colour = c_colour[i]);
  pics[sprintf("c_H[%d]",i)] := c_H_plot[i];
 od:

 for i from 0 to 8 do
  pics[sprintf("c_H[%d]",i)] := c_H_plot[i];
 od:

 pics["curves_H"] := 
  display(
   circle(),
   seq(c_H_plot[i],i=0..8),
   scaling=constrained,axes=none
  );

 save_plots(seq(sprintf("c_H[%d]",i),i=0..8),"curves_H");
 pics["curves_H"];
end:

save_c_H_plots := proc()
 save_plots(seq(sprintf("c_H[%d]",i),i=0..8),"curves_H");
end:

load_c_H_plots := proc()
 load_plots(seq(sprintf("c_H[%d]",i),i=0..8),"curves_H");
end:

######################################################################

#@ make_hyperbolic_domains
make_hyperbolic_domains := proc()
 local c_H_list,m,m1,s0,s1,t0,t1,u0,u1,v0,v1;
 global F1_H_boundary,F1_H_boundaryo,F4_H_boundary,F8_H_boundary,F16_H_boundary;

 c_H_list := proc(k,a,b)
  [seq(evalf(c_H0[k](a + 0.05*i*(b-a))),i=0..20)];
 end:

 F16_H_boundary := [
  op(c_H_list(5,0,Pi)),
  op(c_H_list(3,0,Pi/2)),
  op(c_H_list(0,Pi/2,Pi/4)),
  op(c_H_list(1,Pi,0))
 ]:

 F8_H_boundary := [
  op(c_H_list(5,0,Pi)),
  op(c_H_list(3,0,Pi/2)),
  op(c_H_list(0,Pi/2,0)),
  op(c_H_list(4,-Pi/2,0)),
  op(c_H_list(6,Pi,0))
 ]:

 F4_H_boundary := [
  op(c_H_list(5,0,Pi)),
  op(c_H_list(3,0,Pi)),
  op(c_H_list(7,Pi,0)),
  op(c_H_list(8,Pi,0)),
  op(c_H_list(4,-Pi,0)),
  op(c_H_list(6,Pi,0))
 ]:

 F1_H_boundary := [
  op(c_H_list(3,-Pi,Pi)),
  op(c_H_list(7,Pi,0)),
  op(c_H_list(8,0,Pi))
 ]:
 F1_H_boundary := [op(F1_H_boundary),op(map(z ->I*z,F1_H_boundary))]:
 F1_H_boundary := [op(F1_H_boundary),op(map(z -> -z,F1_H_boundary))]:

 NULL:
end:

######################################################################

#@ make_H_plots
make_H_plots := proc()
 global pics;

 pics["F1_H_boundary"] := display(
  seq(cplot(I^k*c_H0[3](s),s=-Pi..Pi,colour = c_colour[3]),k=0..3),
  seq(cplot(I^k*c_H0[7](s),s= Pi.. 0,colour = c_colour[7]),k=0..3),
  seq(cplot(I^k*c_H0[8](s),s=  0..Pi,colour = c_colour[8]),k=0..3),
  axes=none
 ):

 pics["F1_H_boundary_b"] :=
  display(
   map(u -> plot([u[1]+u[3]*cos(t),u[2]+u[3]*sin(t),t=u[4]..u[5]],colour=u[6]),
       evalf(subs(a_H = a_H0,F1_arcs))),
   axes = none
  );

 pics["F4_H_boundary"] := display(
  cplot(c_H0[5](s),s=0..Pi, colour = c_colour[5]),
  cplot(c_H0[3](s),s=0..Pi, colour = c_colour[3]),
  cplot(c_H0[7](s),s=Pi..0, colour = c_colour[7]),
  cplot(c_H0[8](s),s=Pi..0, colour = c_colour[8]),
  cplot(c_H0[4](s),s=-Pi..0,colour = c_colour[4]),
  cplot(c_H0[6](s),s=Pi..0, colour = c_colour[6]),
  axes=none,scaling=constrained
 ):

 pics["F8_H_boundary"] := display(
  cplot(c_H0[5](s),s=0..Pi,     colour = c_colour[5]),
  cplot(c_H0[3](s),s=0..Pi,     colour = c_colour[3]),
  cplot(c_H0[0](s),s=Pi/4..Pi/2,colour = c_colour[0]),
  cplot(c_H0[7](s),s=0..Pi,     colour = c_colour[7]),
  cplot(c_H0[1](s),s=0..Pi,     colour = c_colour[1]),
  axes=none,scaling=constrained
 );

 pics["F16_H_boundary"] := display(
  cplot(c_H0[0](s),s=Pi/4..Pi/2,colour = c_colour[0]),
  cplot(c_H0[1](s),s=0..Pi/2,   colour = c_colour[1]),
  cplot(c_H0[3](s),s=0..Pi/2,   colour = c_colour[3]),
  cplot(c_H0[5](s),s=0..Pi,     colour = c_colour[5]),
  axes=none,scaling=constrained
 ):

 save_plot("F1_H_boundary");
 save_plot("F1_H_boundary_b");
 save_plot("F4_H_boundary");
 save_plot("F8_H_boundary");
 save_plot("F16_H_boundary");

 NULL;
end:

######################################################################

#@ make_radius_plot_tikz
make_radius_plot_tikz := proc()
 global tikz_pics;
 local s;

 s := cat(
   "\\begin{center}\n",
   " \\begin{tikzpicture}[scale=3]\n",
   "  \\draw[black,->] (-0.05,0) -- (2.05,0);\n",
   "  \\draw[black,->] (0,-0.05) -- (0,1.05);\n",
   "  \\draw[black] (2,-0.05) -- (2,0);\n",
   "  \\draw[black] (-0.05,1) -- (0,1);\n",
   "  \\draw ( 0.00,-0.05) node[anchor=north] {$0$};\n",
   "  \\draw ( 2.00,-0.05) node[anchor=north] {$1$};\n",
   "  \\draw (-0.05, 0.00) node[anchor=east ] {$0$};\n",
   "  \\draw (-0.05, 1.00) node[anchor=east ] {$1$};\n",
   "  \\draw ( 2.05, 0.00) node[anchor=west ] {$b$};\n",
   "  \\draw ( 0.00, 1.05) node[anchor=south] {$|m|$};\n",
   tikz_plot(unapply([2*a_H,abs(min_centre_a)],a_H),0.0..0.1,30,red),
   tikz_plot(unapply([2*a_H,abs(min_centre_a)],a_H),0.1..0.9,30,red),
   tikz_plot(unapply([2*a_H,abs(min_centre_a)],a_H),0.9..1.0,30,red),
   " \\end{tikzpicture}\n",
   "\\end{center}\n"
 ):

 tikz_pics["radius_plot"] := s;
 save_tikz("radius_plot",s);
 return s;
end:

######################################################################

#@ make_H_to_P_graph_tikz
make_H_to_P_graph_tikz := proc()
 global tikz_pics;
 local s1,s2,t1,t2,p,pts;

 pts := [[0,1],
         seq([a,H_to_P_table[a]["a_P"]],
             a in sort(map(op,[indices(H_to_P_table)]))),
         [1,0]
        ];

 s1 := sprintf(" \\draw[%s] plot[smooth] coordinates{ ","red");
 s2 := sprintf(" \\draw[%s] plot[smooth] coordinates{ ","red");

 for p in pts do 
  s1 := cat(s1,sprintf("(%.3f,%.3f) ",op(1,p),op(2,p)));
  s2 := cat(s2,sprintf("(%.3f,%.3f) ",op(2,p),op(1,p)));
 od;
 s1 := cat(s1,"};\n");
 s2 := cat(s2,"};\n");

 t1 := cat(
   "\\begin{center}\n",
   " \\begin{tikzpicture}[scale=4]\n",
   "  \\draw[black,->] (-0.05,0) -- (1.05,0);\n",
   "  \\draw[black,->] (0,-0.05) -- (0,1.05);\n",
   "  \\draw[black] (1,-0.05) -- (1,0);\n",
   "  \\draw[black] (-0.05,1) -- (0,1);\n",
   "  \\draw ( 0.00,-0.05) node[anchor=north] {$0$};\n",
   "  \\draw ( 1.00,-0.05) node[anchor=north] {$1$};\n",
   "  \\draw (-0.05, 0.00) node[anchor=east ] {$0$};\n",
   "  \\draw (-0.05, 1.00) node[anchor=east ] {$1$};\n",
   "  \\draw ( 1.05, 0.00) node[anchor=west ] {$b$};\n",
   "  \\draw ( 0.00, 1.05) node[anchor=south] {$a$};\n",
   s1,
   sprintf("  \\fill[black] (%.3f,%.3f) circle(0.015);\n",a_H0,a_P0),
   " \\end{tikzpicture}\n",
   "\\end{center}\n"
 ):

 t2 := cat(
   "\\begin{center}\n",
   " \\begin{tikzpicture}[scale=4]\n",
   "  \\draw[black,->] (-0.05,0) -- (1.05,0);\n",
   "  \\draw[black,->] (0,-0.05) -- (0,1.05);\n",
   "  \\draw[black] (1,-0.05) -- (1,0);\n",
   "  \\draw[black] (-0.05,1) -- (0,1);\n",
   "  \\draw ( 0.00,-0.05) node[anchor=north] {$0$};\n",
   "  \\draw ( 1.00,-0.05) node[anchor=north] {$1$};\n",
   "  \\draw (-0.05, 0.00) node[anchor=east ] {$0$};\n",
   "  \\draw (-0.05, 1.00) node[anchor=east ] {$1$};\n",
   "  \\draw ( 1.05, 0.00) node[anchor=west ] {$a$};\n",
   "  \\draw ( 0.00, 1.05) node[anchor=south] {$b$};\n",
   s2,
   sprintf("  \\fill[black] (%.3f,%.3f) circle(0.015);\n",a_P0,a_H0),
   " \\end{tikzpicture}\n",
   "\\end{center}\n"
 ):

 tikz_pics["H_to_P_graph"] := t1;
 save_tikz("H_to_P_graph",t1);
 tikz_pics["P_to_H_graph"] := t2;
 save_tikz("P_to_H_graph",t2);

 return t1;
end:


######################################################################

#@ make_square_diffeo_H_plot
make_square_diffeo_H_plot := proc()
 global pics;
 local NN,PP,i,j,c,t0;

 NN := 20;
 PP := NULL;
 for i from 0 to NN do
  for j from 0 to NN do
   t0[i,j] := C_to_R2(square_diffeo_H0_inverse([i/NN,j/NN])); 
  od:
 od:
 for i from 0 to NN do
  for j from 0 to NN-1 do
   if i = 0 then 
    c := c_colour[0];
   elif i = NN then
    c := c_colour[5];
   else
    c := grey;
   fi;
   PP := PP,line(t0[i,j],t0[i,j+1],colour=c);
  od:
 od:
 for i from 0 to NN-1 do
  for j from 0 to NN do
   if j = 0 then 
    c := c_colour[1];
   elif j = NN then
    c := c_colour[3];
   else
    c := grey;
   fi;
   PP := PP,line(t0[i,j],t0[i+1,j],colour=c);
  od:
 od:
 pics["square_diffeo_H"] := display(PP,scaling=constrained,axes=none);
 save_plot("square_diffeo_H");
 pics["square_diffeo_H"];
end:

######################################################################

#@ make_tile_plot
make_tile_plot := proc()
 local P,z0,k,T,e0,e1,m1;
 global pics;

 P := NULL:
 z0 := v_H0[3]/2:
 for k in [0,1,3,5] do
  e0 := evalf(subs(a_H=a_H0,c_H_ends[k])):
  for T in [entries(tile)] do
   e1 := map2(act_Pi_tilde0,op(T),e0);
   if abs(e1[1] + e1[2]) > 10^(-20) then
    m1 := ends_to_centre(op(e1));
    P := P,xi_circle(m1,colour = c_colour[k]);
   else
    P := P,line(C_to_R2(e1[1]),C_to_R2(e1[2]),colour=c_colour[k]);
   fi:
  od:
 od:
 for T in [entries(tile)] do
  P := P,cpoint(act_Pi_tilde(op(T),z0)):
 od:

 pics["tiles"] := P;
 save_plot("tiles");
 P;
end:

######################################################################

#@ make_H_to_P_poles_plot
make_H_to_P_poles_plot := proc()
 global pics;
 
 pics["H_to_P_poles"] := display(
  circle(1),
  circle(0.468,colour=grey),
  cplot(subs(a_H=a_H0,c_HS[0](t)),t=F4_curve_limits[0],colour=c_colour[0]),
  cplot(subs(a_H=a_H0,c_HS[1](t)),t=F4_curve_limits[1],colour=c_colour[1]),
  cplot( subs(a_H=a_H0,c_HS[3](t)),t=F4_curve_limits[3],colour=c_colour[3]),
  cplot(-subs(a_H=a_H0,c_HS[3](t)),t=F4_curve_limits[3],colour=c_colour[3]),
  cplot( subs(a_H=a_H0,c_HS[5](t)),t=F4_curve_limits[5],colour=c_colour[5]),
  cplot(-subs(a_H=a_H0,c_HS[5](t)),t=F4_curve_limits[5],colour=c_colour[5]),
  cplot( conjugate(subs(a_H=a_H0,c_HS[5](t))),t=F4_curve_limits[5],colour=c_colour[5]),
  cplot(-conjugate(subs(a_H=a_H0,c_HS[5](t))),t=F4_curve_limits[5],colour=c_colour[5]),
  map(cpoint,evalf(subs(a_H=a_H0,small_p1_poles)),colour=red),
  scaling=constrained,axes=none
 ):

 save_plot("H_to_P_poles");

 pics["H_to_P_poles"];
end:
