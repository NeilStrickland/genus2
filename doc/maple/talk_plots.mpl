<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>make_Omega_movie_plots := proc()
<a name="line_2"></a> global pics;
<a name="line_3"></a> local opts,Omega_axes,Omega_plus,Omega_minus,i,s;
<a name="line_4"></a> 
<a name="line_5"></a> load_plot("EX"):
<a name="line_6"></a> load_plot("EX_wireframe"):
<a name="line_7"></a>
<a name="line_8"></a> opts := 
<a name="line_9"></a>  view=[-3.5..3.5,-3.5..3.5,-3.5..3.5],
<a name="line_10"></a>  orientation=[40,65],
<a name="line_11"></a>  axes=none,
<a name="line_12"></a>  scaling=constrained:
<a name="line_13"></a>
<a name="line_14"></a> Omega_axes := display(
<a name="line_15"></a>  line([-3,0,0],[3,0,0],color=black,thickness=2),
<a name="line_16"></a>  line([0,-3,0],[0,3,0],color=black,thickness=2),
<a name="line_17"></a>  line([0,0,-3],[0,0,3],color=black,thickness=2),
<a name="line_18"></a>  textplot3d([3.2,0,0,"x"]),
<a name="line_19"></a>  textplot3d([0,3.2,0,"y"]),
<a name="line_20"></a>  textplot3d([0,0,3.2,"z"]),
<a name="line_21"></a>  opts
<a name="line_22"></a> ):
<a name="line_23"></a> Omega_plus := display(
<a name="line_24"></a>  spacecurve(stereo(omega0[1](t)),t=0.8 .. 5.48,color=red,thickness=3),
<a name="line_25"></a>  spacecurve(stereo(omega0[2](t)),t=0.8 .. 5.48,color=red,thickness=3),
<a name="line_26"></a>  opts
<a name="line_27"></a> ):
<a name="line_28"></a> Omega_minus := display(
<a name="line_29"></a>  spacecurve(stereo(-~ omega0[1](t)),t=0..2*Pi,color=blue,thickness=3),
<a name="line_30"></a>  spacecurve(stereo(-~ omega0[2](t)),t=0..2*Pi,color=blue,thickness=3),
<a name="line_31"></a>  opts
<a name="line_32"></a> ):
<a name="line_33"></a>
<a name="line_34"></a> pics["Omega[0]"] := display(Omega_axes,Omega_minus,opts):
<a name="line_35"></a> pics["Omega[1]"] := display(
<a name="line_36"></a>  Omega_axes,Omega_plus,Omega_minus,
<a name="line_37"></a>  sphere([0,0,1/2],0.05,color=black),
<a name="line_38"></a>  sphere([0,0,-1/2],0.05,color=black),
<a name="line_39"></a>  opts
<a name="line_40"></a> ): 
<a name="line_41"></a> pics["Omega[2]"] := display(Omega_plus,Omega_minus,opts):
<a name="line_42"></a> pics["Omega[3]"] := display(Omega_plus,Omega_minus,pics["EX_wireframe"],opts):
<a name="line_43"></a> pics["Omega[4]"] := display(pics["EX_wireframe"],opts):
<a name="line_44"></a>
<a name="line_45"></a> for i from 0 to 10 do
<a name="line_46"></a>  s := sprintf("Omega[%d]",5+i);
<a name="line_47"></a>  pics[s] := display(
<a name="line_48"></a>   pics["EX"],
<a name="line_49"></a>   view=[-3.5..3.5,-3.5..3.5,-3.5..3.5],
<a name="line_50"></a>   orientation=[40+i,65+2*i,-3*i]
<a name="line_51"></a>  ): 
<a name="line_52"></a> od:
<a name="line_53"></a>
<a name="line_54"></a> for i from 1 to 18 do
<a name="line_55"></a>  s := sprintf("Omega[%d]",15+i);
<a name="line_56"></a>  pics[s] := display(
<a name="line_57"></a>   pics["EX"],
<a name="line_58"></a>   view=[-3.5..3.5,-3.5..3.5,-3.5..3.5],
<a name="line_59"></a>   orientation=[60+5*i,85,-30]
<a name="line_60"></a>  ): 
<a name="line_61"></a> od:
<a name="line_62"></a>
<a name="line_63"></a> for i from 0 to 33 do
<a name="line_64"></a>  save_jpg(sprintf("Omega[%d]",i));
<a name="line_65"></a> od:
<a name="line_66"></a>end:
<a name="line_67"></a>
<a name="line_68"></a>######################################################################
<a name="line_69"></a>
<a name="line_70"></a># Note: the following function does things in a strange way because
<a name="line_71"></a># Maple seems not to like reading, modifying and immediately rewriting
<a name="line_72"></a># an image file.
<a name="line_73"></a>
<a name="line_74"></a>make_EX_movie := proc()
<a name="line_75"></a> global pics;
<a name="line_76"></a> local i,R,P,n,FN,Q0,Q1,Q2,dir;
<a name="line_77"></a> 
<a name="line_78"></a> load_plot("EX_wireframe"):
<a name="line_79"></a> for i from 0 to 8 do
<a name="line_80"></a>  load_plot(sprintf("c_E[%d]",i));
<a name="line_81"></a> od:
<a name="line_82"></a>
<a name="line_83"></a> R := 2.0;
<a name="line_84"></a> P := display(
<a name="line_85"></a>   pics["EX_wireframe"],
<a name="line_86"></a>   seq(pics[sprintf("c_E[%d]",i)],i=0..8),
<a name="line_87"></a>   view=[-R..R,-R..R,-R..R],
<a name="line_88"></a>   orientation=[-20,0,-100]
<a name="line_89"></a>  ):
<a name="line_90"></a>
<a name="line_91"></a> dir := cat(images_dir,"/EXmovie"):
<a name="line_92"></a> if not(FileTools[Exists](dir)) then mkdir(dir) fi;
<a name="line_93"></a> n := 89;
<a name="line_94"></a> FN := table():
<a name="line_95"></a> Q0 := table():
<a name="line_96"></a> Q1 := table():
<a name="line_97"></a> Q2 := table():
<a name="line_98"></a> for i from 0 to n do
<a name="line_99"></a>  FN[i] := sprintf("%s/EX%d.png",dir,i):
<a name="line_100"></a>  Q0[i] := display(P,orientation=[-20+2*i,0,-100]);
<a name="line_101"></a>  plotsetup(png,plotoutput=FN[i],plotoptions="width=800,height=800");
<a name="line_102"></a>  print(Q0[i]);
<a name="line_103"></a>  plotsetup(default);
<a name="line_104"></a> od:
<a name="line_105"></a> Threads[Sleep](5);
<a name="line_106"></a> for i from 0 to n do
<a name="line_107"></a>  Q1[i] := ImageTools[Read](FN[i]);
<a name="line_108"></a>  Q2[i] := ImageTools[GetSubImage](Q1[i],250,150,300,500);
<a name="line_109"></a> od:
<a name="line_110"></a> for i from 0 to n do
<a name="line_111"></a>  FileTools[Remove](FN[i]);
<a name="line_112"></a> od:
<a name="line_113"></a> Threads[Sleep](5);
<a name="line_114"></a> for i from 0 to n do
<a name="line_115"></a>  ImageTools[Write](FN[i],Q2[i]);
<a name="line_116"></a> od:
<a name="line_117"></a> NULL;
<a name="line_118"></a>end:
<a name="line_119"></a>
  </pre>
 </body>
</html>
    