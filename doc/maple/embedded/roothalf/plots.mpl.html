<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># This assumes that f is an expression in z[1] and z[2], and it 
<a name="line_2"></a># generates a plot of the corresponding function on the fundamental
<a name="line_3"></a># domain in the z-plane.
<a name="line_4"></a>
<a name="line_5"></a><span style="color:red">#@ z_plot 
</span><a name="line_6"></a>z_plot := proc(f)
<a name="line_7"></a> local N,TX,TZ,TF,TV,TW,i,j;
<a name="line_8"></a>
<a name="line_9"></a> require_square_diffeo_E0_inverse();
<a name="line_10"></a> N := square_diffeo_E0_inverse_order;
<a name="line_11"></a> TX := square_diffeo_E0_inverse_table;
<a name="line_12"></a> TZ := table();
<a name="line_13"></a> TF := table();
<a name="line_14"></a> TV := table();
<a name="line_15"></a> TW := table();
<a name="line_16"></a> for i from 0 to N do 
<a name="line_17"></a>  for j from 0 to N do
<a name="line_18"></a>   TZ[i,j] := evalf(z_proj0(TX[i/N,j/N]));
<a name="line_19"></a>   TF[i,j] := evalf(subs({z[1]=TZ[i,j][1],z[2]=TZ[i,j][2]},f));
<a name="line_20"></a>   TV[i,j] := [op(TZ[i,j]),TF[i,j]];
<a name="line_21"></a>   TW[i,j] := [op(TZ[i,j]),0];
<a name="line_22"></a>  od;
<a name="line_23"></a> od;
<a name="line_24"></a>
<a name="line_25"></a> display(
<a name="line_26"></a>  display( 
<a name="line_27"></a>   seq(seq(polygon([TV[i,j],TV[i+1,j],TV[i+1,j+1]]),j=0..N-1),i=0..N-1),
<a name="line_28"></a>   seq(seq(polygon([TV[i,j],TV[i,j+1],TV[i+1,j+1]]),j=0..N-1),i=0..N-1),
<a name="line_29"></a>   style=patchnogrid
<a name="line_30"></a>  ),
<a name="line_31"></a>  display(
<a name="line_32"></a>   seq(seq(polygon([TW[i,j],TW[i+1,j],TW[i+1,j+1]]),j=0..N-1),i=0..N-1),
<a name="line_33"></a>   seq(seq(polygon([TW[i,j],TW[i,j+1],TW[i+1,j+1]]),j=0..N-1),i=0..N-1),
<a name="line_34"></a>   colour=grey,style=wireframe
<a name="line_35"></a>  )
<a name="line_36"></a> );
<a name="line_37"></a>
<a name="line_38"></a>end:
<a name="line_39"></a>
<a name="line_40"></a># These vectors can be used to define some orthogonal projections
<a name="line_41"></a># R^4 -> R^2 which give nice pictures of EX^*.
<a name="line_42"></a>
<a name="line_43"></a><span style="color:red">#@ linear_projection_vector
</span><a name="line_44"></a>linear_projection_vector[1] := [ 1, 0, 0, 0];
<a name="line_45"></a>linear_projection_vector[2] := [ 0, 1, 0, 0];
<a name="line_46"></a>linear_projection_vector[3] := [ 1, 1, 0, 0] /~ sqrt(2);
<a name="line_47"></a>linear_projection_vector[4] := [ 1,-1, 0, 0] /~ sqrt(2);
<a name="line_48"></a>linear_projection_vector[5] := [ 0, 0, 1, 0];
<a name="line_49"></a>linear_projection_vector[6] := [ 0, 0, 0, 1];
<a name="line_50"></a>linear_projection_vector[7] := [ 0, 0, 1, 1] /~ sqrt(2);
<a name="line_51"></a>linear_projection_vector[8] := [ 0, 0, 1,-1] /~ sqrt(2);
<a name="line_52"></a>
<a name="line_53"></a><span style="color:red">#@ make_linear_projection_plots 
</span><a name="line_54"></a>make_linear_projection_plots := proc()
<a name="line_55"></a> global pics;
<a name="line_56"></a> local IJ,ij,i,j,k,p,lbl,PP,QQ;
<a name="line_57"></a>
<a name="line_58"></a> IJ := [[3,6],[1,6],[5,6],[1,2],[3,5],[1,5],[1,7],[3,7]];
<a name="line_59"></a>
<a name="line_60"></a> for ij in IJ do
<a name="line_61"></a>  i,j := op(ij);
<a name="line_62"></a>  lbl := sprintf("lin_proj_%d_%d",i,j);
<a name="line_63"></a>  p := unapply(evalf([dp4(linear_projection_vector[i],xx),
<a name="line_64"></a>                      dp4(linear_projection_vector[j],xx)]),x);
<a name="line_65"></a>
<a name="line_66"></a>  pics[lbl] := 
<a name="line_67"></a>   display(
<a name="line_68"></a>    seq(planecurve(p(c_E0[k](t)),t=0..2*Pi,colour=c_colour[k]),k = 0 .. 8),
<a name="line_69"></a>    scaling=constrained,axes=none
<a name="line_70"></a>   );
<a name="line_71"></a>  PP[i,j] := pics[lbl];
<a name="line_72"></a>
<a name="line_73"></a>  save_plot(lbl);
<a name="line_74"></a>  save_jpg( lbl);
<a name="line_75"></a>
<a name="line_76"></a>  lbl := cat(lbl,"_extra");
<a name="line_77"></a>  pics[lbl] := 
<a name="line_78"></a>   display(
<a name="line_79"></a>    seq(planecurve(p(c_E0[k](t)),t=0..2*Pi,colour=c_colour[k]),k = 0 .. 16),
<a name="line_80"></a>    scaling=constrained,axes=none
<a name="line_81"></a>   );
<a name="line_82"></a>  QQ[i,j] := pics[lbl];
<a name="line_83"></a>
<a name="line_84"></a>  save_plot(lbl);
<a name="line_85"></a>  save_jpg( lbl);
<a name="line_86"></a> od:
<a name="line_87"></a>
<a name="line_88"></a> pics["lin_proj_grid"] := 
<a name="line_89"></a>  display(<<PP[3,6]|PP[1,6]|PP[5,6]|PP[1,2]>,
<a name="line_90"></a>           <PP[3,5]|PP[1,5]|PP[1,7]|PP[3,7]>>,
<a name="line_91"></a>          scaling=constrained,axes=none);
<a name="line_92"></a>
<a name="line_93"></a> save_plot("lin_proj_grid");
<a name="line_94"></a>
<a name="line_95"></a> pics["lin_proj_grid_extra"] := 
<a name="line_96"></a>  display(<<QQ[3,6]|QQ[1,6]|QQ[5,6]|QQ[1,2]>,
<a name="line_97"></a>           <QQ[3,5]|QQ[1,5]|QQ[1,7]|QQ[3,7]>>,
<a name="line_98"></a>          scaling=constrained,axes=none);
<a name="line_99"></a>
<a name="line_100"></a> save_plot("lin_proj_grid_extra");
<a name="line_101"></a>end:
<a name="line_102"></a>
<a name="line_103"></a>
  </pre>
 </body>
</html>
    