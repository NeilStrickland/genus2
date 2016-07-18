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

make_EX_rot_plots := proc()
 global pics;
 local i,s,P;
 
 load_plot("EX_wireframe");
 for i from 0 to 8 do
  load_plot(sprintf("c_E[%d]",i));
 od;

 P := display(
  pics["EX_wireframe"],
  seq(pics[sprintf("c_E[%d]",i)],i=0..8),
  view=[-3.5..3.5,-3.5..3.5,-3.5..3.5],
  orientation=[-20,0,-100]
 );

 for i from 0 to 50 do
  s := sprintf("EX_rot[%d]",i);
  pics[s] :=
   display(P,orientation=[-20+2*i,0,-100]);
  save_jpg(s);
 od:
end:

