# This file defines Maple functions to generate various plots and save
# them as jpg files or in Maple's internal format.  Some of the jpg
# files are then included in the main LaTeX document for the project.

# There are directories called plots and images, which are siblings
# of the top maple directory.  Plots in Maple's internal format are
# stored in the plots directory, with names like foo.m.  Images in
# jpg format are stored in the images directory, with names like
# foo.jpg.  There is also a global variable called pics, and a Maple
# representation of the plot may be stored as pics["foo"].

#@ pics 
pics := table():

# Files in .m format cannot contain bare, unnamed expressions.
# Instead, they record values for certain named variables.  To save
# a plot P, we first assign P to the variable __pic__, then we
# save that variable.  When we read the file later, the required
# plot will be assigned to __pic__ again, and we will immediately
# copy it to somewhere more useful.  The housekeeping is managed
# by the functions save_plot and load_plot.

__pic__ := NULL:
make_plot := table():

######################################################################

# This function returns true if P is a 2 or 3 dimensional plot object.
# This fits into a general Maple framework, so if we define
# a function like foo := proc(X::plot) ... end, then an error will
# be generated if we call foo with an argument that is not a plot
# object.

#@ `type/plot`
`type/plot` := (P) ->
 (type(P,function) and (op(0,P) = PLOT or op(0,P) = PLOT3D));

######################################################################

# The function list_plots() returns a list of names of plot files
# in the plots directory.  They are returned without the suffix .m.
# One can also supply a regular expression to match agianst.  For
# example, list_plots("^E") will return a list of files that start
# with E.

#@ list_plots
list_plots := proc(regex)
 local P,Q,f;
 
 Q := FileTools[ListDirectory](plots_dir);
 P := NULL;
 for f in Q do
  if length(f) > 2 and substring(f,-2..-1) = ".m" then
   P := P,substring(f,1..-3);
  fi;
 od;
 
 P := [P];
 
 if nargs > 0 then
  P := select(f -> StringTools[RegMatch](regex,f),P);
 fi;
 return P;
end:

######################################################################

# This function saves a plot.  It can be called in two ways.
# If we invoke save_plot("foo",P), then P will be saved in the file
# foo.m in the plots directory, and pics["foo"] will also be set
# equal to P.  If we just invoke save_plot("foo"), then pics["foo"]
# should already be set equal to a plot object, and that object
# will be saved in foo.m.

#@ save_plot 
save_plot := proc(s::string,P_::plot) 
 global __pic__,pics;
 local P;

 if nargs > 1 then
  pics[s] := P_;
  P := P_;
 else 
  P := pics[s];
  if not type(P,plot) then
   error(sprintf("pics[%s] is not a plot",s));
  fi;
 fi;
 
 __pic__ := pics[s];
 save(__pic__,cat(genus2_dir,"/plots/",s,".m"));
 __pic__ := NULL;
end: 

# This accepts an arbitrary number of arguments, and applies the
# single-argument form of save_plot() to each of them.

#@ save_plots 
save_plots := proc()
 local s;
 for s in args do save_plot(s); od;
end:

######################################################################

# Invoking load_plot("foo") will read a plot from the file foo.m,
# save it as pics["foo"], and return the result.

#@ load_plot 
load_plot := proc(s::string) 
 global __pic__,pics;
 read(cat(genus2_dir,"/plots/",s,".m"));
 pics[s] := __pic__;
 __pic__ := NULL;
 pics[s];
end: 

# This accepts an arbitrary number of arguments, and applies the
# single-argument form of load_plot() to each of them.

#@ load_plots 
load_plots := proc()
 local s;
 for s in args do load_plot(s); od;
 NULL;
end:

######################################################################

# Invoking save_jpg("foo") will convert pics["foo"] to a jpg image
# and save it in the images directory.  Note that pics["foo"] should
# have been set equal to a plot object before using this function.

#@ save_jpg 
save_jpg := proc(s::string,w_,h_) 
 local P,old_dir,w,h;

 P := pics[s];
 if not type(P,plot) then
  error(sprintf("pics[%s] is not a plot",s));
 fi;

 w := `if`(nargs>1,w_,1000);
 h := `if`(nargs>2,h_,1000);
 old_dir := currentdir(cat(genus2_dir,"/images"));
 plotsetup(jpeg,
  plotoutput=cat(s,".jpg"),
  plotoptions=sprintf("height=%d,width=%d",h,w)
 );
 print(P);
 plotsetup(default);
 currentdir(old_dir);
end: 

save_jpgs := proc()
 local s;
 for s in args do save_jpg(s); od;
end:

#@ save_thumbnail
save_thumbnail := proc(s::string) 
 local P,old_dir,w,h;

 P := pics[s];
 if not type(P,plot) then
  error(sprintf("pics[%s] is not a plot",s));
 fi;

 old_dir := currentdir(cat(genus2_dir,"/plots/thumbs"));
 plotsetup(jpeg,
  plotoutput=cat(s,".jpg"),
  plotoptions="height=100,width=100"
 );
 print(P);
 plotsetup(default);
 currentdir(old_dir);
end:

#@ make_all_thumbnails
make_all_thumbnails := proc()
 local old_pics,labels,label,P;
 global pics;
 
 # To avoid using an enormous amount of memory, we do not retain
 # all plot objects in the pics table.
 old_pics := eval(pics):
 
 labels := list_plots():
 for label in labels do
  printf("%s\n",label);
  pics := table():
  load_plot(label);
  P := pics[label];
  if not(type(P,plot)) then
   P := display(P);
   pics[label] := P;
  fi;
  if type(P,plot) then
   save_thumbnail(label);
  fi;
 od:
 pics := eval(old_pics);
 NULL;
end:

######################################################################
# This just makes the syntax for plotting plane curves more
# compatible with the syntax for plotting space curves.

#@ planecurve 
planecurve := (xy,R) -> plot([op(xy),R],args[3..-1]);

#@ planetext 
planetext := (xy, t) -> textplot([op(xy),t],args[3..-1]);

#@ spacetext 
spacetext := (xyz,t) -> textplot3d([op(xyz),t],args[3..-1]);

######################################################################

# Some convenience functions for plotting complex numbers as 
# points in R^2.  Note that these functions will work with
# expressions involving the symbolic constants a_H, a_P and a_E.
# They will be replaced by the numerical values a_H0, a_P0 and a_E0.

#@ cpolygon 
cpolygon :=
 proc(points)
  polygon(map(z -> evalf(subs({a_H=a_H0,a_P=a_P0,a_E=a_E0},[Re(z),Im(z)])),points),args[2..-1]);
 end:

#@ ccurve 
ccurve :=
 proc(points)
  curve(map(z -> evalf(subs({a_H=a_H0,a_P=a_P0,a_E=a_E0},[Re(z),Im(z)])),points),args[2..-1]);
 end:

#@ cpoint 
cpoint :=
 proc(z)
  point(evalf(subs({a_H=a_H0,a_P=a_P0,a_E=a_E0},[Re(z),Im(z)])),args[2..-1]);
 end:

#@ ctext 
ctext := 
 proc(z,t)
  local x0,y0,z0;
  z0 := evalf(subs({a_H=a_H0,a_P=a_P0,a_E=a_E0},z));
  x0 := Re(z0);
  y0 := Im(z0);
  textplot([x0,y0,t],args[3..-1]);
 end:

#@ cplot 
cplot := proc(c,R) plot([Re(c),Im(c),R],args[3..-1]); end:

######################################################################

# sphere_arc(a,b,col) returns a plot element that can be used as an 
# argument to the display() function.  It represents an arc of the
# sphere joining a to b, in the colour specified by the argument col.
# It is crude and only works well for short arcs.

#@ sphere_arc 
sphere_arc := proc(a::RR0_3,b::RR0_3,col)
 local c,i,j,r;
 for i from 0 to 10 do
  c[i] := [seq(0.1* (i * a[j] + (10-i) * b[j]),j=1..3)]:
  r := sqrt(add(c[i][j]^2,j=1..3));
  c[i] := [seq(c[i][j]/r,j=1..3)];
 od:
 curve([seq(c[i],i=0..10)],colour=col);
end:


######################################################################

# torus_box_plot(c) expects c to be a function R -> S^1 x S^1 which
# is periodic of period 2 pi.  It returns a plot of c with the torus
# unwrapped to a flat square, and it deals with discontinuities in a
# way that is adequate for the cases that we need.  One can pass
# additional arguments, such as torus_box_plot(c,colour = red).

torus_box_plot_aux := proc(c,s)
 local a,eps;

 eps := 0.0001;
 a := map(argument,c)/~Pi +~ (2 *~ s);
 display(
  seq(planecurve(a,t=j*Pi/2+eps..(j+1)*Pi/2-eps,args[3..-1]),j=0..4),
  axes=none,scaling=constrained
 ):
end:

#@ torus_box_plot 
torus_box_plot := proc(c)
 if c[1] = -1 then
  display(torus_box_plot_aux(c,[ 0, 0],args[2..-1]),
          torus_box_plot_aux(c,[-1, 0],args[2..-1]));
 elif c[2] = -1 then
  display(torus_box_plot_aux(c,[ 0, 0],args[2..-1]),
          torus_box_plot_aux(c,[ 0,-1],args[2..-1]));
 else
  display(torus_box_plot_aux(c,[ 0, 0],args[2..-1]));
 fi:
end:

######################################################################

triangle_axes := proc(z_range := -2..2)
 local tc,z_min,z_max;
 
 tc[1] := evalf([op(triangle_proj([1,0,0])),0]):
 tc[2] := evalf([op(triangle_proj([0,1,0])),0]):
 tc[3] := evalf([op(triangle_proj([0,0,1])),0]):
 z_min := op(1,z_range);
 z_max := op(2,z_range);
 
 display(
  line(tc[1],tc[2],colour=black),
  line(tc[2],tc[3],colour=black),
  line(tc[3],tc[1],colour=black),
  line(tc[1],[0,0,0],colour=black),
  line(tc[2],[0,0,0],colour=black),
  line(tc[3],[0,0,0],colour=black),
  line([0,0,z_min],[0,0,z_max],colour=black),
  scaling=constrained,axes=none
 ):
end:
