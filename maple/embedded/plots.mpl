# This is a generic function for drawing a picture of the image of a map
# p : EX -> R^2.  The function p is given as the first argument.
# The last argument filter_ can be omitted or set to "F4" or "F16".
# If it is set, then only p(F4) or p(F16) will be drawn rather than
# the whole of p(EX).  
#
# The argument v_label_align should be a table indexed by a subset of
# {0,...,13} specifying the alignment of vertex labels relative to the
# point marking the vertex itself.  The argument c_label_param should be
# a table indexed by a subset of {0,...,8}.  If the i'th entry is t0
# then a label for c[i] will be placed near to p(c[i](t0)), with 
# alignment specified by c_label_align[i].
#
# There is some logic to provide readable labels in the case where 
# several different vertices have the same image under p.

#@ plane_proj_plot 
plane_proj_plot := proc(
    p,v_label_align,c_label_param,c_label_align,filter_
 )
 local filter,II,JJ,CL,i,j,vv,vla,vlm,cc,cla,t0,a0,x0,P;

 if nargs < 5 then filter := NULL; else filter := filter_; fi;

 if filter = "F16" then
  II := F16_vertices;
  JJ := select(j -> F16_curve_limits[j] <> NULL,{seq(j,j=0..8)});
  CL := F16_curve_limits;
 elif filter = "F4" then
  II := F4_vertices;
  JJ := select(j -> F4_curve_limits[j] <> NULL,{seq(j,j=0..8)});
  CL := F4_curve_limits;
 else
  II := {seq(i,i=0..13)};
  JJ := {seq(j,j=0..8)};
  CL := table([seq(i = 0..2*Pi,i=0..8)]);
 fi;

 vv := table(); vla := table(); vlm := table();

 for i in II do 
  vv[i] := simplify(subs(a_E=a_E0,p(v_E0[i])));
  vla[vv[i]] := 'below';
  if type(vlm[vv[i]],list) then
   vlm[vv[i]] := [op(vlm[vv[i]]),i];
  else
   vlm[vv[i]] := [i];
  fi;
 od;

 cc := table(); cla := table();

 for i in JJ do
  cla[i] := 'below'; 
  cc[i] := unapply(simplify(subs(a_E=a_E0,p(c_E0[i](t)))),t);
 od;

 for i in map(op,[indices(v_label_align)]) do 
  vla[vv[i]] :=  v_label_align[i];
 od;

 for i in map(op,[indices(c_label_align)]) do 
  cla[i] :=  c_label_align[i];
 od;

 P := seq(plot([op(cc[i](t)),t=CL[i]],colour=c_colour[i]),i in JJ);
 P := P,seq(point(vv[i]),i in II);
 P := P,  seq(
    textplot([op(evalf(op(ii))),sprintf("%Q",op(vlm[op(ii)]))],'align'=vla[op(ii)]),
    ii in indices(vlm)
  );
 for i in map(op,[indices(c_label_param)]) do
  if member(i,JJ) then 
   t0 := c_label_param[i];
   a0 := cla[i];
   x0 := evalf(subs(a_E=a_E0,cc[i](t0)));

   P := P,textplot([op(x0),i],'align'=a0,colour=red);
  fi;
 od;

 return display(P,scaling=constrained,axes=none);
end:

######################################################################

#@ angle_latex
angle_latex[0]    := "0";
angle_latex[Pi]   := "\\pi";
angle_latex[2*Pi] := "2\\pi";
angle_latex[Pi/4] := "\\qpi";
angle_latex[Pi/2] := "\\ppi";

######################################################################

# This function is similar to plane_proj_plot() except that it generates
# tikz code for inclusion in the latex file rather than a Maple plot.

#@ plane_proj_tikz 
plane_proj_tikz := proc(
    p,v_label_align,c_label_param,c_label_align
 )
 local filter,II,JJ,CL,i,j,vv,vla,vlm,cc,cla,t0,t1,t2,a0,a1,a2,a3,x0,s,s0,a,ls,comma,
       scale,point_size,num_points,label_size,centred,raw,ranges,arrows,extra;

 filter := NULL;
 scale := 4;
 point_size := 0.02;
 num_points := 10;
 raw := false;
 centred := false;
 ranges := false;
 label_size := "";
 arrows := [];
 extra := "";

 for a in args[5..-1] do
  if a = "F4" or a = "F16" then
   filter := a;
  elif type(a,`=`) then
   if lhs(a) = "scale"      then scale      := rhs(a); fi;
   if lhs(a) = "point_size" then point_size := rhs(a); fi;
   if lhs(a) = "label_size" then label_size := rhs(a); fi;
   if lhs(a) = "num_points" then num_points := rhs(a); fi;
   if lhs(a) = "centred"    then centred    := rhs(a); fi;
   if lhs(a) = "raw"        then raw        := rhs(a); fi;
   if lhs(a) = "ranges"     then ranges     := rhs(a); fi;
   if lhs(a) = "arrows"     then arrows     := rhs(a); fi;
   if lhs(a) = "extra"      then extra      := rhs(a); fi;
  fi;
 od;

 if label_size = "subscript" then
  ls := "\\ss ";
 elif label_size = "subsubscript" then
  ls := "\\sss ";
 else
  ls := "";
 fi;

 if filter = "F16" then
  II := F16_vertices;
  JJ := select(j -> F16_curve_limits[j] <> NULL,{seq(j,j=0..8)});
  CL := F16_curve_limits;
 elif filter = "F4" then
  II := F4_vertices;
  JJ := select(j -> F4_curve_limits[j] <> NULL,{seq(j,j=0..8)});
  CL := F4_curve_limits;
 else
  II := {seq(i,i=0..13)};
  JJ := {seq(j,j=0..8)};
  CL := table([seq(i = 0..2*Pi,i=0..8)]);
 fi;

 vv := table(); vla := table(); vlm := table();

 for i in II do 
  vv[i] := simplify(subs(a_E=a_E0,p(v_E0[i])));
  vla[vv[i]] := 'below';
  if type(vlm[vv[i]],list) then
   vlm[vv[i]] := [op(vlm[vv[i]]),i];
  else
   vlm[vv[i]] := [i];
  fi;
 od;

 cc := table(); cla := table();

 for i in JJ do
  cla[i] := 'below'; 
  cc[i] := unapply(simplify(subs(a_E=a_E0,p(c_E0[i](t)))),t);
 od;

 for i in map(op,[indices(v_label_align)]) do 
  vla[vv[i]] :=  v_label_align[i];
 od;

 for i in map(op,[indices(c_label_align)]) do 
  cla[i] :=  c_label_align[i];
 od;

 if raw = true then 
  s := "";
 else 
  s := sprintf("\\begin{tikzpicture}[scale=%A]\n",scale);
 fi;

 for i in JJ do
  s := cat(s,tikz_plot(cc[i](t),CL[i],num_points,c_colour[i]));
 od;
 for a in arrows do
  i := op(1,a);
  t0 := evalf(op(2,a));
  a0 := evalf(cc[i](t0));
  a1 := evalf(cc[i](t0+0.01));
  s := cat(s,
   sprintf(" \\draw[%s,arrows={-angle 90}] (%.4f,%.4f) -- (%.4f,%.4f);\n",
           c_colour[i],op(a0),op(a1)));
 od;
 for i in II do 
  s := cat(s,tikz_point(vv[i],point_size));
 od;
 for i in map(op,[indices(vlm)]) do
  s0 := ls;
  comma := "";
  for j in vlm[i] do
   s0 := cat(s0,comma,sprintf("v_{%A}",j));
   comma := ",";
  od;
  s := cat(s,tikz_label(i,s0,vla[i]));
 od;
 for i in map(op,[indices(c_label_param)]) do
  if member(i,JJ) then 
   t0 := c_label_param[i];
   a0 := cla[i];
   x0 := evalf(subs(a_E=a_E0,cc[i](t0)));
   s0 := sprintf("%sc_{%d}",ls,i);
   if ranges = true then
    s0 := sprintf("%s(%s\\dotsb%s)",s0,
                  angle_latex[op(1,CL[i])],angle_latex[op(2,CL[i])]);
   fi;
   s := cat(s,tikz_label(x0,s0,a0));
  fi;
 od;

 s := cat(s,extra);

 if raw <> true then
  s := cat(s,"\\end{tikzpicture}\n");
 fi;

 if centred = true then
  s := cat("\\begin{center}\n",s,"\\end{center}\n");
 fi;

 return(s);
end:

######################################################################

#@ surface_plot 
surface_plot := proc(f,M_::integer,H_)
 local i,j,T,N,M,H,P,SDI;

 require_square_diffeo_E0_inverse();

 N := square_diffeo_E0_inverse_order;
 M := N;
 if nargs > 1 and M_ > 0 and type(N/M_,integer) then
  M := M_;
 fi;
 
 H := G16;
 if nargs > 2 then H := H_; fi;

 SDI := eval(square_diffeo_E0_inverse_table):
 P := table():
 for T in H do
  for i from 0 to M do
   for j from 0 to M do
    P[T,i,j] := evalf(f(act_R4[T](SDI[i/M,j/M]))):
   od:
  od:
 od:

 display(
  seq(seq(seq(
   polygon([P[T,i,j],P[T,i,j+1],P[T,i+1,j+1]],style=patchnogrid),
    i=0..M-1),j=0..M-1),T in H),
  seq(seq(seq(
   polygon([P[T,i,j],P[T,i+1,j],P[T,i+1,j+1]],style=patchnogrid),
    i=0..M-1),j=0..M-1),T in H),
  seq(spacecurve(f(c_E0[k](t)),t=0..2*Pi,colour=c_colour[k]),k=0..8),
  scaling=constrained,axes=none
 );
end:

######################################################################

#@ make_c_E_plots 
make_c_E_plots := proc()
 local i;
 global pics,c_E_plot;

 for i from 0 to 8 do 
  c_E_plot[i] :=
    spacecurve(stereo(c_E1[i](t)),t=0..2*Pi,numpoints=200,colour = c_colour[i],axes = none);
  pics[sprintf("c_E[%d]",i)] := c_E_plot[i];
 od:

 pics["curves_E"] := display(seq(c_E_plot[i],i=0..8),scaling=constrained,axes=none);
 save_plots(seq(sprintf("c_E[%d]",i),i=0..8),"curves_E");
 pics["curves_E"];
end:

load_c_E_plots := proc()
 load_plots(seq(sprintf("c_E[%d]",i),i=0..8),"curves_E");
end:

######################################################################

#@ make_E_plots 
make_E_plots := proc()
 global pics;
 pics["EX"]  := implicitplot3d(g_stereo0([t,u,v])=0,t=-3..3,u=-3..3,v=-3..3,
                 scaling=constrained,grid=[50,50,50],style=patchnogrid,axes=none):
 pics["EX1"] := implicitplot3d(g_stereo0([t,u,v])=0,t=0..3,u=0..3,v=-3..3,
                 scaling=constrained,grid=[50,50,50],style=patchnogrid,axes=none):
 pics["EX2"] := implicitplot3d(g_stereo0([t,u,v])=0,t=0..3,u=0..3,v=0..3,
                 scaling=constrained,grid=[50,50,50],style=patchnogrid,axes=none):
 pics["EX3"] := implicitplot3d(g_stereo0([t,u,v])=0,t=0..3,u=-3..0,v=0..3,
                 scaling=constrained,grid=[50,50,50],style=patchnogrid,axes=none):

 pics["EX_wireframe"] :=
  implicitplot3d(g_stereo0([t,u,v])=0,t=-3..3,u=-3..3,v=-3..3,
    scaling=constrained,grid=[50,50,50],style=wireframe,color=gray,axes=none):

 save_plots("EX","EX1","EX2","EX3","EX_wireframe");

 NULL;
end:

load_E_plots := () -> load_plots("EX","EX1","EX2","EX3","EX_wireframe");

######################################################################

make_plot["EX_with_curves"] := proc()
 global pics;

 pics["EX_with_curves"] := display(
  pics["EX"],
  seq(pics[sprintf("c_E[%d]",i)],i=0..8),
  scaling=constrained,orientation=[65,80],axes = none
 ): 
 save_plot("EX_with_curves"): 
 save_jpg("EX_with_curves"): 

 pics["EX_with_curves"];
end:

############################################################

make_plot["Omega"] := proc()
 global pics;

 pics["Omega"] := display(
  spacecurve(stereo(omega1[1](t)),t=0.8 .. 5.48,color=red,thickness=3),
  spacecurve(stereo(omega1[2](t)),t=0.8 .. 5.48,color=red,thickness=3),
  sphere([0,0,1/2],0.05,color=black),
  sphere([0,0,-1/2],0.05,color=black),
  spacecurve(stereo(evalm(-omega1[1](t))),t=0..2*Pi,color=blue,thickness=3),
  spacecurve(stereo(evalm(-omega1[2](t))),t=0..2*Pi,color=blue,thickness=3),
  line([-3,0,0],[3,0,0],color=black,thickness=3),
  line([0,-3,0],[0,3,0],color=black,thickness=3),
  line([0,0,-3],[0,0,3],color=black,thickness=3),
  textplot3d([3.2,0,0,"x"]),
  textplot3d([0,3.2,0,"y"]),
  textplot3d([0,0,3.2,"z"]),
  view=[-3.5..3.5,-3.5..3.5,-3.5..3.5],orientation=[40,65],axes=none
 ):

 save_plot("Omega"):
 save_jpg("Omega"):

 pics["Omega"];
end:

############################################################

make_plot["XOmega"] := proc()
 global pics;

 pics["XOmega"] := display(
  spacecurve(stereo(omega1[1](t)),t=0.8 .. 5.48,color=red,thickness=3),
  spacecurve(stereo(omega1[2](t)),t=0.8 .. 5.48,color=red,thickness=3),
  spacecurve(stereo(evalm(-omega1[1](t))),t=0..2*Pi,color=blue,thickness=3),
  spacecurve(stereo(evalm(-omega1[2](t))),t=0..2*Pi,color=blue,thickness=3),
  pics["EX_wireframe"],
  view=[-3.5..3.5,-3.5..3.5,-3.5..3.5],
  scaling=constrained,orientation=[65,80],axes=none
 ):

 save_plot("XOmega"):
 save_jpg("XOmega"):

 pics["XOmega"];
end:

############################################################

make_plot["F4"] := proc()
 global pics;

 pics["F4"] :=
  display(pics["EX1"],seq(c_E_plot[i],i=3..8),orientation=[-120,0,50],axes=none):
 save_plot("F4"):
 save_jpg("F4"):
 pics["F4"];
end:

######################################################################

make_plot["F16_outline"] := proc()
 global pics;

 pics["F16_outline"] := display(
  spacecurve(stereo(c_E0[0](t)),t=Pi/4..Pi/2,colour = c_colour[0]),
  spacecurve(stereo(c_E0[1](t)),t=0..Pi/2,colour = c_colour[1]),
  spacecurve(stereo(c_E0[3](t)),t=0..Pi/2,colour = c_colour[3]),
  spacecurve(stereo(c_E0[5](t)),t=0..Pi,colour = c_colour[5]),
  scaling = constrained,axes=none
 ):

 save_plot("F16_outline"):
 save_jpg("F16_outline"):
 pics["F16_outline"];
end:

############################################################

make_plot["y_proj"] := proc()
 global pics;

 pics["y_proj"] := 
  plane_proj_plot(y_proj0,
   table([0=right,1=left,3=above,11=above,13=above]),
   table([0=Pi/6,1=2,2=1.9,3=2.1,4=2.1,5=1.5,6=1.5,7=1.5,8=1.5]),
   table([0=left,3=above,5=right,6=right,7=left,8=left]));

 save_plot("y_proj");
 save_jpg("y_proj");
 pics["y_proj"];
end:

############################################################

make_plot["y_proj_F4"] := proc()
 global pics;

 pics["y_proj_F4"] :=
  plane_proj_plot(y_proj0,
   table([0=right,1=left,3=above,11=above,13=above]),
   table([0=Pi/6,1=2,2=1.9,3=2.1,4=2.1,5=1.5,6=1.5,7=1.5,8=1.5]),
   table([0=left,3=above,5=right,6=right,7=left,8=left]),"F4");

 save_plot("y_proj_F4");
 save_jpg("y_proj_F4");
 pics["y_proj_F4"];
end:

############################################################

make_plot["y_proj_F16"] := proc()
 global pics;

 pics["y_proj_F16"] :=
  plane_proj_plot(y_proj0,
   table([1=left,3=above,11=above,13=above]),
   table([0=1.05,1=1.1,3=1,5=1.5]),
   table([0=left,3=above,5=right,6=right,7=left,8=left]),
   "F16");

 save_plot("y_proj_F16");
 save_jpg("y_proj_F16");
 pics["y_proj_F16"];
end:

############################################################

make_plot["F16_16x16_grid"] := proc()
 local i,j;
 global W,pics;

 for i from 0 to 16 do
  for j from 0 to 16 do
   W[i,j] := evalf(stereo(map(Re,w_lift1([i/16.,j/16.])))):
  od:
 od:

 W[16,16] := evalf(stereo(v_E1[11]));

 pics["F16_16x16_grid"] := display(
   seq(seq(polygon([W[i,j],W[i+1,j],W[i+1,j+1]],style=patchnogrid),i=0..15),j=0..15),
   seq(seq(polygon([W[i,j],W[i,j+1],W[i+1,j+1]],style=patchnogrid),i=0..15),j=0..15),
   scaling=constrained,orientation=[-145,60,80],axes=none
  ):

 save_plot("F16_16x16_grid");
 save_jpg("F16_16x16_grid"):
 pics["F16_16x16_grid"];
end:

############################################################

make_plot["z_proj"] := proc()
 global pics;

 pics["z_proj"] := 
  plane_proj_plot(z_proj0,
   table([2=above,10=above]),
   table([0=1.2,1=2.3,2=2.4,3=2.3,4=2.4,5=1.9,6=2.0,7=2.1,8=2.2]),
   table([0=left,3={above,left},4={above,left},5=right,6=right,7=right,8=right]));

 save_plot("z_proj");
 save_jpg("z_proj");
 pics["z_proj"];
end:

############################################################

make_plot["z_proj_F16"] := proc()
 global pics;
 local v_z_map,v_z_align0,v_z_align,c_z_map0,c_z_map,c_z_labels,c_z_arrows,c_z_plots,c_z,
       i,ii,z0,t0,a,u,L,r,w;

 v_z_align0 := table([
  0 = 'right',
  2 = 'left',
  6 = 'below'
 ]):

 c_z_map0 := table([
  0  = [ 1.20,'left'],
  1  = [ 2.30,'below'],
  2  = [ 2.40,'below'],
  3  = [ 2.30,{'above','left'}],
  4  = [ 2.40,{'above','left'}],
  5  = [ 1.90,'right'],
  6  = [ 2.00,'right'],
  7  = [ 2.10,'right'],
  8  = [ 2.20,'right']
 ]):

 v_z_map := table():

 v_z_align := table();

 v_z_align := table();
 for ii in indices(v_z_align0) do
  v_z_align[simplify(z_proj0(v_E0[op(ii)]))] := v_z_align0[op(ii)];
 od:

 for i in select(j -> j <= 13,F16_vertices) do
  z0 := simplify(z_proj0(v_E0[i]));
  if not(assigned(v_z_align[z0])) then
   v_z_align[z0] := 'below';
  fi;
  if type(v_z_map[z0],list) then
   v_z_map[z0] := [op(v_z_map[z0]),i];
  else
   v_z_map[z0] := [i];
  fi;
 od:

 c_z_labels := NULL;
 c_z_arrows := NULL;
 c_z_plots  := NULL;

 for i from 0 to 8 do 
  c_z[i] := unapply(simplify(evalf(simplify(z_proj0(c_E0[i](t))))),t);
 od:

 for i from 0 to 8 do
  if F16_curve_limits[i] <> NULL then
   t0,a := op(c_z_map0[i]);
   r := F16_curve_limits[i];
   u := evalf(c_z[i](t0));
   w := evalf(c_z[i](t0+0.05)) -~ u;
   w := w /~ nm2(w);

   c_z_plots := c_z_plots,
                plot([op(c_z[i](t)),t=r],colour=c_colour[i]);
   c_z_labels := c_z_labels,
                textplot([op(u),sprintf("%Q",i,op(r))],align=a,colour=red);
   c_z_arrows := c_z_arrows,
      line(u,u +~ 0.03 *~ [-w[1]-w[2], w[1]-w[2]],colour = c_colour[i]),
      line(u,u +~ 0.03 *~ [-w[1]+w[2],-w[1]-w[2]],colour = c_colour[i]);
  fi;
 od;

 pics["z_proj_F16"] := display(
  c_z_plots,
  c_z_labels,
  c_z_arrows,
  seq(point(evalf(op(ii))),ii in indices(v_z_map)),
  seq(
    textplot([op(evalf(op(ii))),sprintf("%Q",op(v_z_map[op(ii)]))],'align'=v_z_align[op(ii)]),
    ii in indices(v_z_map)
  ),
  scaling=constrained,axes=none
 );

 save_plot("z_proj_F16");
 save_jpg("z_proj_F16");
 pics["z_proj_F16"];
end:

############################################################

make_plot["z_proj_F16_bare"] := proc()
 global pics;

 pics["z_proj_F16_bare"] := display(
  plot([op(z_proj0(c_E0[0](t))),t=Pi/4..Pi/2],colour = c_colour[0]),
  plot([op(z_proj0(c_E0[1](t))),t=0..Pi/2],colour = c_colour[1]),
  plot([op(z_proj0(c_E0[3](t))),t=0..Pi/2],colour = c_colour[3]),
  plot([op(z_proj0(c_E0[5](t))),t=0..Pi],colour = c_colour[5]),
  scaling = constrained,axes=none
 ):

 save_plot("z_proj_F16_bare"):
 save_jpg("z_proj_F16_bare"):
 pics["z_proj_F16_bare"];
end:

############################################################

#@ save_tikz_z_proj_F16 
save_tikz_z_proj_F16 := proc()
 local v_z_map,v_z_align0,v_z_align,c_z_map0,c_z_map,c_z_labels,c_z_arrows,c_z_plots,c_z,
       i,ii,z0,t0,a,u,L,r,w,s,num_points;

 v_z_align0 := table([
  0 = 'right',
  2 = 'left',
  6 = 'below'
 ]):

 c_z_map0 := table([
  0  = [ 1.20,'left'],
  1  = [ 2.30,'below'],
  2  = [ 2.40,'below'],
  3  = [ 2.30,{'above','left'}],
  4  = [ 2.40,{'above','left'}],
  5  = [ 1.90,'right'],
  6  = [ 2.00,'right'],
  7  = [ 2.10,'right'],
  8  = [ 2.20,'right']
 ]):

 v_z_map := table():

 v_z_align := table();

 v_z_align := table();
 for ii in indices(v_z_align0) do
  v_z_align[simplify(z_proj0(v_E0[op(ii)]))] := v_z_align0[op(ii)];
 od:

 for i in select(j -> j <= 13,F16_vertices) do
  z0 := simplify(z_proj0(v_E0[i]));
  if not(assigned(v_z_align[z0])) then
   v_z_align[z0] := 'below';
  fi;
  if type(v_z_map[z0],list) then
   v_z_map[z0] := [op(v_z_map[z0]),i];
  else
   v_z_map[z0] := [i];
  fi;
 od:

 c_z_labels := "";
 c_z_arrows := "";
 c_z_plots  := "";

 for i from 0 to 8 do 
  c_z[i] := unapply(simplify(evalf(simplify(z_proj0(c_E0[i](t))))),t);
 od:

 num_points := 25;
 
 for i from 0 to 8 do
  if F16_curve_limits[i] <> NULL then
   t0,a := op(c_z_map0[i]);
   r := F16_curve_limits[i];
   u := evalf(c_z[i](t0));
   w := evalf(c_z[i](t0+0.01));

   c_z_plots := cat(c_z_plots,tikz_plot(c_z[i],r,num_points,c_colour[i]));
   c_z_labels := cat(c_z_labels,tikz_label(u,sprintf("%Q",i,op(r)),a));
   c_z_arrows := cat(c_z_arrows,
                      sprintf(" \\draw[%s,arrows={-angle 90}] (%.4f,%.4f) -- (%.4f,%.4f);\n",
                              c_colour[i],op(u),op(w)));
  fi;
 od;

 s := cat("\\begin{center}\n \\begin{tikzpicture}[scale=4]\n",
          c_z_plots,
          c_z_labels,
          c_z_arrows,
          " \\end{tikzpicture}\n\\end{center}\n");

 save_tikz("z_proj_F16_raw",s);

 return(s);
end:

############################################################

make_plot["w_proj"] := proc()
 global wp,pics;

 wp := (x) -> `if`(x[1]=0 and x[2]=0 and x[4]=0,[1,0],w_proj0(x));

 pics["w_proj"] := 
  plane_proj_plot(wp,
   table([2=above,10=above]),
   table([0=1.0,1=2.3,2=2.4,3=2.3,4=2.4,5=1.4,6=1.5,7=1.6,8=1.7]),
   table([0=left,3={above,left},4={above,left},5=right,6=right,7=right,8=right]));

 save_plot("w_proj");
 save_jpg("w_proj");
 pics["w_proj"];
end:

############################################################

make_plot["t_proj"] := proc()
 global wp,pics;

 wp := (x) -> `if`(x[1]=0 and x[2]=0 and x[4]=0,[1,0],t_proj(x));

 pics["t_proj"] := 
  plane_proj_plot(wp,
   table([2=above,10=above]),
   table([0=1.0,1=2.3,2=2.4,3=2.3,4=2.4,5=1.4,6=1.5,7=1.6,8=1.7]),
   table([0=left,3={above,left},4={above,left},5=right,6=right,7=right,8=right]));

 save_plot("t_proj");
 save_jpg("t_proj");
 pics["t_proj"];
end:

############################################################

#@ make_v_square_plots 
make_v_square_plots := proc()
 local k,s;
 global pics;

 for k in {3,6,11} do 
  s := sprintf("v_square[%d]",k);
  pics[s] := 
   display(
    seq(op([
     plot3d(v_stereo[k](act_E[T](t_lift([t[1],t[2]]))),t[1]=0..1,t[2]=0..1),
     spacecurve(v_stereo[k](act_E[T](c_E0[0](t))),t=Pi/4..Pi/2,colour=c_colour[0]),
     spacecurve(v_stereo[k](act_E[T](c_E0[1](t))),t=0..Pi/2,colour=c_colour[1]),
     spacecurve(v_stereo[k](act_E[T](c_E0[3](t))),t=0..Pi/2,colour=c_colour[3]),
     spacecurve(v_stereo[k](act_E[T](c_E0[5](t))),t=0..Pi,colour=c_colour[5])]),
     T in v_stabiliser_G16[k]
    ),
    scaling=constrained,axes=none
   );
 od:
 NULL;
end:

#@ save_v_square_plots 
save_v_square_plots := proc()
 local ss;
 ss := seq(sprintf("v_square[%d]",k),k in {3,6,11});
 save_plots(ss);
 save_jpgs(ss);
 NULL;
end:

#@ load_v_square_plots 
load_v_square_plots := proc()
 local ss;
 ss := seq(sprintf("v_square[%d]",k),k in {3,6,11});
 load_plots(ss);
 NULL;
end:

############################################################

make_plot["net_0_glue"] := proc()
 global pics;

 pics["net_0_glue"] :=
 display(
  map(e -> line(net_0["v"][e[1]],net_0["v"][e[2]],colour=edge_colour(e)),
      net_0["outer_edges"]),
  map(e -> textplot([op((net_0["v"][e[1]]+~net_0["v"][e[2]])/~2),edge_curve(e)]),
      net_0["outer_edges"]),
  axes = none
 );

 save_plot("net_0_glue");
 save_jpg("net_0_glue");
 pics["net_0_glue"];
end:

############################################################

make_plot["disc_delta"] := proc()
 global pics;
 local dpr;

 dpr := unapply(simplify(subs(a_E=a_E0,disc_delta_proj(x))),x);

 pics["disc_delta"] :=
  plane_proj_plot(dpr,
   table([0 = 'above',2 = {'above','right'},3 = {'above','left'},
          7 = 'left',9 = 'right',12 = 'above']),
   table(),
   table());

 save_plot("disc_delta");
 save_jpg("disc_delta");
 pics["disc_delta"];
end:

######################################################################

make_plot["disc_pi"] := proc()
 global pics;
 local dpr;

 dpr := unapply(simplify(subs(a_E=a_E0,disc_pi_proj(x))),x);

 pics["disc_pi"] :=
  plane_proj_plot(dpr,
   table([2=right,3=above,4=left,
          6={above,right},7={above,left},8={below,left},9={below,right}]),
   table(),
   table());

 save_plot("disc_pi");
 save_jpg("disc_pi");
 pics["disc_pi"];
end:

######################################################################

make_plot["disc_zeta"] := proc()
 global pics;
 local dpr;

 dpr := unapply(simplify(subs(a_E=a_E0,disc_zeta_proj(x))),x);

 pics["disc_zeta"] :=
  plane_proj_plot(dpr,
   table([0=left,1=right,3=above,6=above,7=above,10=right,11=right,12=left,13=left]),
   table(),table());

 save_plot("disc_zeta");
 save_jpg("disc_zeta");
 pics["disc_zeta"];
end:

############################################################

make_plot["disc_zeta_F16"] := proc()
 global pics;
 local dpr;

 dpr := unapply(simplify(subs(a_E=a_E0,disc_zeta_proj(x))),x);

 pics["disc_zeta_F16"] :=
  plane_proj_plot(dpr,
   table([3=left,6=left]),
   table(),table(),"F16");

 save_plot("disc_zeta_F16");
 save_jpg("disc_zeta_F16");
 pics["disc_zeta_F16"];
end:

######################################################################

#@ make_S2_proj_plots 
make_S2_proj_plots := proc()
 local C_plot;
 global C_proj_outline_plot,S2_proj_plot;

 C_plot := proc(u,a,b) plot([Re(u),Im(u),t=a..b],args[4..-1]); end:

 C_proj_outline_plot := display(
  C_plot(C_proj1(c[0](t)),Pi/4,Pi/2,colour=c_colour[0]),
  C_plot(C_proj1(c[1](t)),0,Pi/2,colour=c_colour[1]),
  C_plot(C_proj1(c[3](t)),0,Pi/2,colour=c_colour[3]),
  C_plot(C_proj1(c[5](t)),0,Pi,colour=c_colour[5]),
  scaling=constrained,axes=none
 ):

 S2_proj_plot := display(
  sphere(0.99,colour=grey,style=patchnogrid),
  seq(seq(polygon([S2_proj1(square_lift_table[i,j]),
                   S2_proj1(square_lift_table[i+1,j]),
                   S2_proj1(square_lift_table[i+1,j+1])]),i=0..15),j=0..15),
  seq(seq(polygon([S2_proj1(square_lift_table[i,j]),
                   S2_proj1(square_lift_table[i,j+1]),
                   S2_proj1(square_lift_table[i+1,j+1])]),i=0..15),j=0..15),
  scaling=constrained
 ):
end:

######################################################################

#@ make_E_torus_plots 
make_E_torus_plots := proc()
 global pics;
 local k,lp,lm,lmq,wft;
 
 for k from 0 to 24 do
  lp  := sprintf("c_Tp[%d] flat",k);
  lm  := sprintf("c_Tm[%d] flat",k);
  lmq := sprintf("c_Tmq[%d] flat",k);
  
  pics[lp ] := torus_box_plot(c_TCp[k](t),colour=c_colour[k]);
  pics[lm ] := torus_box_plot(c_TCm[k](t),colour=c_colour[k]);
  pics[lmq] := torus_box_plot(c_TCmq[k](t),colour=c_colour[k]);

  save_plot(lp);
  save_plot(lm);
  save_plot(lmq);

  lp  := sprintf("c_Tp[%d]",k);
  lm  := sprintf("c_Tm[%d]",k);
  lmq := sprintf("c_Tmq[%d]",k);
  
  pics[lp ] := spacecurve(TC_to_R3(c_TCp[ k](t)),t=0..2*Pi,
                 colour=c_colour[k],numpoints=400,scaling=constrained,axes=none);
  pics[lm ] := spacecurve(TC_to_R3(c_TCm[ k](t)),t=0..2*Pi,
                 colour=c_colour[k],numpoints=400,scaling=constrained,axes=none);
  pics[lmq] := spacecurve(TC_to_R3(c_TCmq[k](t)),t=0..2*Pi,
                 colour=c_colour[k],numpoints=400,scaling=constrained,axes=none);

  save_plot(lp);
  save_plot(lm);
  save_plot(lmq);
 od:

 pics["c_Tp flat"] := display(seq(pics[sprintf("c_Tp[%d] flat",k)],
                             k in {0,1,3,4,5,6}));
 pics["c_Tm flat"] := display(seq(pics[sprintf("c_Tm[%d] flat",k)],
                             k in {0,1,2,3,5,6,9,10}));
 pics["c_Tmq flat"] := display(seq(pics[sprintf("c_Tmq[%d] flat",k)],
                             k in {0,1,2,3,5,6,9,10,17,18,19}));

 wft := plot3d(angles_to_R3(t,u),t=0..1,u=0..1,
          colour=gray,style=wireframe,scaling=constrained,axes=none);

 pics["c_Tp" ] := display(wft,seq(pics[sprintf("c_Tp[%d]",k)],
                             k in {0,1,3,4,5,6}));
 pics["c_Tm" ] := display(wft,seq(pics[sprintf("c_Tm[%d]",k)],
                             k in {0,1,2,3,5,6}));
 pics["c_Tmq"] := display(wft,seq(pics[sprintf("c_Tmq[%d]",k)],
                             k in {0,1,2,3,5,6}));

 save_plot("c_Tp flat");
 save_plot("c_Tm flat");
 save_plot("c_Tmq flat");
 save_plot("c_Tp");
 save_plot("c_Tm");
 save_plot("c_Tmq");
 
end:

######################################################################

#@ make_E_sphere_plots 
make_E_sphere_plots := proc()
 global pics;
 local wfs;

 wfs := display(sphere([0,0,0],1,colour=grey,style=wireframe),axes=none):

 pics["SQE 1"] := 
 display(wfs,
  seq(spacecurve(E_to_S2(c_E0[k](t)),t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
  axes=none,scaling=constrained
 ):

 pics["SQE 2"] := 
 display(wfs,
  seq(spacecurve(p12_S2(E_to_S2(c_E0[k](t))),t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
  axes=none,scaling=constrained
 ):

 pics["SQE 3"] := 
 display(wfs,
  seq(spacecurve(p13_S2(E_to_S2(c_E0[k](t))),t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
  axes=none,scaling=constrained
 ):

 pics["SQE 4"] := 
 display(wfs,
  seq(spacecurve(p14_S2(E_to_S2(c_E0[k](t))),t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
  axes=none,scaling=constrained
 ):

 pics["SQE 5"] := 
 display(wfs,
  seq(spacecurve(p15_S2(E_to_S2(c_E0[k](t))),t=0..2*Pi,colour=c_colour[k],numpoints=200),k=0..8),
  axes=none,scaling=constrained
 ):

 save_plot("SQE 1");
 save_plot("SQE 2");
 save_plot("SQE 3");
 save_plot("SQE 4");
 save_plot("SQE 5");

end:
