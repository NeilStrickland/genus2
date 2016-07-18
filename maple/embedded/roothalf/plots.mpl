# This assumes that f is an expression in z[1] and z[2], and it 
# generates a plot of the corresponding function on the fundamental
# domain in the z-plane.

#@ z_plot 
z_plot := proc(f)
 local N,TX,TZ,TF,TV,TW,i,j;

 require_square_diffeo_E0_inverse();
 N := square_diffeo_E0_inverse_order;
 TX := square_diffeo_E0_inverse_table;
 TZ := table();
 TF := table();
 TV := table();
 TW := table();
 for i from 0 to N do 
  for j from 0 to N do
   TZ[i,j] := evalf(z_proj0(TX[i/N,j/N]));
   TF[i,j] := evalf(subs({z[1]=TZ[i,j][1],z[2]=TZ[i,j][2]},f));
   TV[i,j] := [op(TZ[i,j]),TF[i,j]];
   TW[i,j] := [op(TZ[i,j]),0];
  od;
 od;

 display(
  display( 
   seq(seq(polygon([TV[i,j],TV[i+1,j],TV[i+1,j+1]]),j=0..N-1),i=0..N-1),
   seq(seq(polygon([TV[i,j],TV[i,j+1],TV[i+1,j+1]]),j=0..N-1),i=0..N-1),
   style=patchnogrid
  ),
  display(
   seq(seq(polygon([TW[i,j],TW[i+1,j],TW[i+1,j+1]]),j=0..N-1),i=0..N-1),
   seq(seq(polygon([TW[i,j],TW[i,j+1],TW[i+1,j+1]]),j=0..N-1),i=0..N-1),
   colour=grey,style=wireframe
  )
 );

end:

# These vectors can be used to define some orthogonal projections
# R^4 -> R^2 which give nice pictures of EX^*.

#@ linear_projection_vector
linear_projection_vector[1] := [ 1, 0, 0, 0];
linear_projection_vector[2] := [ 0, 1, 0, 0];
linear_projection_vector[3] := [ 1, 1, 0, 0] /~ sqrt(2);
linear_projection_vector[4] := [ 1,-1, 0, 0] /~ sqrt(2);
linear_projection_vector[5] := [ 0, 0, 1, 0];
linear_projection_vector[6] := [ 0, 0, 0, 1];
linear_projection_vector[7] := [ 0, 0, 1, 1] /~ sqrt(2);
linear_projection_vector[8] := [ 0, 0, 1,-1] /~ sqrt(2);

#@ make_linear_projection_plots 
make_linear_projection_plots := proc()
 global pics;
 local IJ,ij,i,j,k,p,lbl,PP,QQ;

 IJ := [[3,6],[1,6],[5,6],[1,2],[3,5],[1,5],[1,7],[3,7]];

 for ij in IJ do
  i,j := op(ij);
  lbl := sprintf("lin_proj_%d_%d",i,j);
  p := unapply(evalf([dp4(linear_projection_vector[i],xx),
                      dp4(linear_projection_vector[j],xx)]),x);

  pics[lbl] := 
   display(
    seq(planecurve(p(c_E0[k](t)),t=0..2*Pi,colour=c_colour[k]),k = 0 .. 8),
    scaling=constrained,axes=none
   );
  PP[i,j] := pics[lbl];

  save_plot(lbl);

  lbl := cat(lbl,"_extra");
  pics[lbl] := 
   display(
    seq(planecurve(p(c_E0[k](t)),t=0..2*Pi,colour=c_colour[k]),k = 0 .. 16),
    scaling=constrained,axes=none
   );
  QQ[i,j] := pics[lbl];

  save_plot(lbl);
 od:

 pics["lin_proj_grid"] := 
  display(<<PP[3,6]|PP[1,6]|PP[5,6]|PP[1,2]>,
           <PP[3,5]|PP[1,5]|PP[1,7]|PP[3,7]>>,
          scaling=constrained,axes=none);

 save_plot("lin_proj_grid");

 pics["lin_proj_grid_extra"] := 
  display(<<QQ[3,6]|QQ[1,6]|QQ[5,6]|QQ[1,2]>,
           <QQ[3,5]|QQ[1,5]|QQ[1,7]|QQ[3,7]>>,
          scaling=constrained,axes=none);

 save_plot("lin_proj_grid_extra");
end:

#@ owl_proj 
owl_proj := (x) -> [x[1],x[2],(x[3]-x[4])/sqrt(2)];

