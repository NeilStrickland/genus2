make_Omega_movie_plots := proc()
 global pics;
 local opts,Omega_axes,Omega_plus,Omega_minus,i,s;
 
 load_plot("EX"):
 load_plot("EX_wireframe"):

 opts := 
  view=[-3.5..3.5,-3.5..3.5,-3.5..3.5],
  orientation=[40,65],
  axes=none,
  scaling=constrained:

 Omega_axes := display(
  line([-3,0,0],[3,0,0],color=black,thickness=2),
  line([0,-3,0],[0,3,0],color=black,thickness=2),
  line([0,0,-3],[0,0,3],color=black,thickness=2),
  textplot3d([3.2,0,0,"x"]),
  textplot3d([0,3.2,0,"y"]),
  textplot3d([0,0,3.2,"z"]),
  opts
 ):
 Omega_plus := display(
  spacecurve(stereo(omega0[1](t)),t=0.8 .. 5.48,color=red,thickness=3),
  spacecurve(stereo(omega0[2](t)),t=0.8 .. 5.48,color=red,thickness=3),
  opts
 ):
 Omega_minus := display(
  spacecurve(stereo(-~ omega0[1](t)),t=0..2*Pi,color=blue,thickness=3),
  spacecurve(stereo(-~ omega0[2](t)),t=0..2*Pi,color=blue,thickness=3),
  opts
 ):

 pics["Omega[0]"] := display(Omega_axes,Omega_minus,opts):
 pics["Omega[1]"] := display(
  Omega_axes,Omega_plus,Omega_minus,
  sphere([0,0,1/2],0.05,color=black),
  sphere([0,0,-1/2],0.05,color=black),
  opts
 ): 
 pics["Omega[2]"] := display(Omega_plus,Omega_minus,opts):
 pics["Omega[3]"] := display(Omega_plus,Omega_minus,pics["EX_wireframe"],opts):
 pics["Omega[4]"] := display(pics["EX_wireframe"],opts):

 for i from 0 to 10 do
  s := sprintf("Omega[%d]",5+i);
  pics[s] := display(
   pics["EX"],
   view=[-3.5..3.5,-3.5..3.5,-3.5..3.5],
   orientation=[40+i,65+2*i,-3*i]
  ): 
 od:

 for i from 1 to 18 do
  s := sprintf("Omega[%d]",15+i);
  pics[s] := display(
   pics["EX"],
   view=[-3.5..3.5,-3.5..3.5,-3.5..3.5],
   orientation=[60+5*i,85,-30]
  ): 
 od:

 for i from 0 to 33 do
  save_jpg(sprintf("Omega[%d]",i));
 od:
end:

######################################################################

# Note: the following function does things in a strange way because
# Maple seems not to like reading, modifying and immediately rewriting
# an image file.

make_EX_movie := proc()
 global pics;
 local i,R,P,n,FN,Q0,Q1,Q2,dir;
 
 load_plot("EX_wireframe"):
 for i from 0 to 8 do
  load_plot(sprintf("c_E[%d]",i));
 od:

 R := 2.0;
 P := display(
   pics["EX_wireframe"],
   seq(pics[sprintf("c_E[%d]",i)],i=0..8),
   view=[-R..R,-R..R,-R..R],
   orientation=[-20,0,-100]
  ):

 dir := cat(images_dir,"/EXmovie"):
 if not(FileTools[Exists](dir)) then mkdir(dir) fi;
 n := 89;
 FN := table():
 Q0 := table():
 Q1 := table():
 Q2 := table():
 for i from 0 to n do
  FN[i] := sprintf("%s/EX%d.png",dir,i):
  Q0[i] := display(P,orientation=[-20+2*i,0,-100]);
  plotsetup(png,plotoutput=FN[i],plotoptions="width=800,height=800");
  print(Q0[i]);
  plotsetup(default);
 od:
 Threads[Sleep](5);
 for i from 0 to n do
  Q1[i] := ImageTools[Read](FN[i]);
  Q2[i] := ImageTools[GetSubImage](Q1[i],250,150,300,500);
 od:
 for i from 0 to n do
  FileTools[Remove](FN[i]);
 od:
 Threads[Sleep](5);
 for i from 0 to n do
  ImageTools[Write](FN[i],Q2[i]);
 od:
 NULL;
end:

