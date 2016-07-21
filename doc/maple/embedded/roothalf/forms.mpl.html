<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># These differential forms alpha[i] and beta[j] are as in sec-int-props
<a name="line_2"></a>
<a name="line_3"></a><span style="color:red">#@ alpha_form
</span><a name="line_4"></a>alpha_form[1] := y[1]*(x[2]*dx[1] - x[1]*dx[2]);
<a name="line_5"></a>alpha_form[2] := (z[1]*z[2]+z[1])*y[1]*y[2]*(dx[1]*x[2]+dx[2]*x[1]) +
<a name="line_6"></a>                 (-2)*z[1]*x[1]*x[2]*y[2]*dx[3]+
<a name="line_7"></a>                 (2*z[1]*z[2]+2)*x[1]*x[2]*dx[4];
<a name="line_8"></a>
<a name="line_9"></a><span style="color:red">#@ beta_form
</span><a name="line_10"></a>beta_form[1] := y[1]*(x[2]*dx[1] - x[1]*dx[2]);
<a name="line_11"></a>beta_form[2] := y[1]*y[2]*(x[2]*dx[1] + x[1]*dx[2]);
<a name="line_12"></a>beta_form[3] := x[1]*x[2]*y[2]*dx[3];
<a name="line_13"></a>beta_form[4] := x[1]*x[2]*dx[4];
<a name="line_14"></a>
<a name="line_15"></a><span style="color:red">#@ theta_form
</span><a name="line_16"></a>theta_form[1] := add(diff(g0(x),x[i])*dx[i],i=1..4);
<a name="line_17"></a>theta_form[2] := add(x[i]*dx[i],i=1..4);
<a name="line_18"></a>
<a name="line_19"></a>alpha_form[3] := x[1]*x[2]*theta_form[1];
<a name="line_20"></a>alpha_form[4] := x[1]*x[2]*y[1]*y[2]*theta_form[2];
<a name="line_21"></a>
<a name="line_22"></a># This is the matrix P such that alpha[i] = sum(P[i,j] * beta[j],j=1..4)
<a name="line_23"></a>
<a name="line_24"></a><span style="color:red">#@ alpha_beta 
</span><a name="line_25"></a>alpha_beta := [
<a name="line_26"></a> [1,0,0,0],
<a name="line_27"></a> [0,z[1]*(1+z[2]),-2*z[1],2*(1+z[1]*z[2])],
<a name="line_28"></a> [sqrt(2)*(1-z[1])*(1-2*z[2]),z[1]*(1-2*z[2]),(z[1] - 2),(-2+3*z[1]-4*z[1]*z[2])],
<a name="line_29"></a> [(3*z[1]-2)*z[2]/(2*sqrt(2)),(1 - z[1] - z[1]*z[2])/2,z[1],-z[1]*z[2]]
<a name="line_30"></a>];
<a name="line_31"></a>
<a name="line_32"></a># Norm of dg in terms of z
<a name="line_33"></a><span style="color:red">#@ ndg_z 
</span><a name="line_34"></a>ndg_z := sqrt((2 - z[1])^2 * (1 + z[2]));
<a name="line_35"></a>
<a name="line_36"></a><span style="color:red">#@ D_alpha
</span><a name="line_37"></a>
<a name="line_38"></a># This is the Stokes operator D = *d applied to alpha[1]
<a name="line_39"></a>D_alpha[1] := (9*z[1]^2*z[2]-9*z[1]^2+2*z[1]*z[2]+9*z[1]-2)/ndg_z;
<a name="line_40"></a>
<a name="line_41"></a># This is the Stokes operator D = *d applied to alpha[2]
<a name="line_42"></a>D_alpha[2] := (45*z[1]^3*z[2]^2-45*z[1]^3*z[2]+78*z[1]^2*z[2]-20*z[1]*z[2]^2
<a name="line_43"></a>                -12*z[1]^2-28*z[1]*z[2]+12*z[1]-8*z[2])/sqrt(2)/ndg_z;
<a name="line_44"></a>
<a name="line_45"></a># dz_cross_alpha[i,j] is * (dz[i] wedge alpha[j])
<a name="line_46"></a><span style="color:red">#@ dz_cross_alpha
</span><a name="line_47"></a>
<a name="line_48"></a>dz_cross_alpha[1,1] := 2*z[1]*(3*z[1]-2)*(z[1]*z[2]-z[1]+1)/ndg_z;
<a name="line_49"></a>dz_cross_alpha[1,2] := sqrt(2)*z[1]*
<a name="line_50"></a>                        (9*z[1]^3*z[2]^2-9*z[1]^3*z[2]-12*z[1]^2*z[2]^2+
<a name="line_51"></a>                         26*z[1]^2*z[2]+4*z[1]*z[2]^2-4*z[1]^2-24*z[1]*z[2]+
<a name="line_52"></a>                         8*z[1]+8*z[2]-4) / ndg_z;
<a name="line_53"></a>dz_cross_alpha[2,1] := 4*z[1]*z[2]*(2*z[2]-1) / ndg_z;
<a name="line_54"></a>dz_cross_alpha[2,2] := 2*sqrt(2)*(3*z[1]^2*z[2]-2*z[1]*z[2]+2*z[1]-2)*z[2]*(2*z[2]-1) / ndg_z;
<a name="line_55"></a>
<a name="line_56"></a><span style="color:red">#@ xy0
</span><a name="line_57"></a>xy0[1] := sqrt(uy0[1]);
<a name="line_58"></a>xy0[2] := sqrt(uy0[2]);
<a name="line_59"></a>xy0[3] := y[1];
<a name="line_60"></a>xy0[4] := -y[1]*y[2];
<a name="line_61"></a>
<a name="line_62"></a># Rules for rewriting x[i], z[i], dx[i] and dz[i] in terms of y[j] and dy[j]
<a name="line_63"></a> 
<a name="line_64"></a><span style="color:red">#@ forms_to_y 
</span><a name="line_65"></a>forms_to_y := {
<a name="line_66"></a> x[1] = sqrt(uy0[1]),
<a name="line_67"></a> x[2] = sqrt(uy0[2]),
<a name="line_68"></a> x[3] = y[1],
<a name="line_69"></a> x[4] = -y[1]*y[2],
<a name="line_70"></a> dx[1] = grad_y(sqrt(uy0[1])),
<a name="line_71"></a> dx[2] = grad_y(sqrt(uy0[2])),
<a name="line_72"></a> dx[3] = grad_y(y[1]),
<a name="line_73"></a> dx[4] = grad_y(-y[1]*y[2]),
<a name="line_74"></a> z[1] = y[1]^2,
<a name="line_75"></a> z[2] = y[2]^2,
<a name="line_76"></a> dz[1] = 2*y[1]*dy[1],
<a name="line_77"></a> dz[2] = 2*y[2]*dy[2]
<a name="line_78"></a>}:
<a name="line_79"></a>
<a name="line_80"></a>######################################################################
<a name="line_81"></a># This is an alternative notation, which we are not really using.
<a name="line_82"></a>
<a name="line_83"></a><span style="color:red">#@ de_rham_d0 
</span><a name="line_84"></a>de_rham_d0 := (u) -> add(diff(u,x[i])*dx[i],i=1..4):
<a name="line_85"></a>
<a name="line_86"></a><span style="color:red">#@ de_rham_d1 
</span><a name="line_87"></a>de_rham_d1 := proc(u)
<a name="line_88"></a> local v,w,i,j;
<a name="line_89"></a> for i from 1 to 4 do
<a name="line_90"></a>  for j from 1 to 4 do
<a name="line_91"></a>   v[i,j] := diff(coeff(u,dx[i]),x[j]);
<a name="line_92"></a>  od:
<a name="line_93"></a> od:
<a name="line_94"></a> w := add(add(v[i,j]*dx[j,i],j=1..i-1),i=1..4) - 
<a name="line_95"></a>      add(add(v[i,j]*dx[i,j],j=i+1..4),i=1..4);
<a name="line_96"></a> return(w);
<a name="line_97"></a>end:
<a name="line_98"></a>
<a name="line_99"></a><span style="color:red">#@ wedge 
</span><a name="line_100"></a>wedge := proc(u,v)
<a name="line_101"></a> add(add(coeff(u,dx[i])*coeff(v,dx[j])*dx[i,j],i=1..j-1),j=1..4) - 
<a name="line_102"></a> add(add(coeff(u,dx[i])*coeff(v,dx[j])*dx[j,i],i=j+1..4),j=1..4);
<a name="line_103"></a>end:
<a name="line_104"></a>
<a name="line_105"></a>######################################################################
<a name="line_106"></a>
<a name="line_107"></a><span style="color:red">#@ unnormalised_stokes 
</span><a name="line_108"></a>unnormalised_stokes := proc(u)
<a name="line_109"></a> local nn,uu,du,S4,sgn4,s;
<a name="line_110"></a> nn := dg0(xx);
<a name="line_111"></a> uu := [seq(coeff(u,dx[i],1),i=1..4)];
<a name="line_112"></a> du := [seq([seq(diff(uu[j],x[i]),j=1..4)],i=1..4)];
<a name="line_113"></a> S4 := combinat[permute](4);
<a name="line_114"></a> sgn4 := (s) -> signum(mul(mul(s[j]-s[i],j=i+1..4),i=1..3));
<a name="line_115"></a> add(sgn4(s)*du[s[1]][s[2]]*x[s[3]]*nn[s[4]],s in S4);
<a name="line_116"></a>end:
<a name="line_117"></a>
<a name="line_118"></a><span style="color:red">#@ stokes 
</span><a name="line_119"></a>stokes := (u) -> unnormalised_stokes(u)/sqrt(square_norm_of_dg0(xx));
<a name="line_120"></a>
<a name="line_121"></a># stokes_alpha([f1,f2]) = 
<a name="line_122"></a>#  stokes(f1 * alpha_form[1] + f2 * alpha_form[2])
<a name="line_123"></a># (where f1 and f2 should be given in terms of z[1] and z[2]).
<a name="line_124"></a> 
<a name="line_125"></a><span style="color:red">#@ stokes_alpha 
</span><a name="line_126"></a>stokes_alpha := proc(ff)
<a name="line_127"></a> local i,j;
<a name="line_128"></a>
<a name="line_129"></a> add(add(diff(ff[i],z[j]) * dz_cross_alpha[j,i],j=1..2),i=1..2) +
<a name="line_130"></a> add(ff[i] * D_alpha[i],i=1..2);
<a name="line_131"></a>end:
  </pre>
 </body>
</html>
    