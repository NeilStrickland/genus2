<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_Rn := proc()
<a name="line_2"></a> local s,t,r,u,v,w,x,y,z,uu,vv,ww,err,err0,err1,err2,err3;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> assume(s::real,t::real,r::real);
<a name="line_7"></a>
<a name="line_8"></a> uu := [u[1],u[2],u[3],u[4]];
<a name="line_9"></a> vv := [v[1],v[2],v[3],v[4]];
<a name="line_10"></a> ww := [w[1],w[2],w[3],w[4]];
<a name="line_11"></a>
<a name="line_12"></a> _ASSERT(
<a name="line_13"></a>  expand(dp4(cross_product4(uu,vv,ww),uu)) = 0 and
<a name="line_14"></a>  expand(dp4(cross_product4(uu,vv,ww),vv)) = 0 and
<a name="line_15"></a>  expand(dp4(cross_product4(uu,vv,ww),ww)) = 0 and
<a name="line_16"></a>  cross_product4([1,0,0,0],[0,1,0,0],[0,0,1,0]) = [0,0,0,1],
<a name="line_17"></a>  "cross_product4"
<a name="line_18"></a> ):
<a name="line_19"></a>
<a name="line_20"></a> _ASSERT(
<a name="line_21"></a>  triangle_proj(triangle_lift([x,y])) = [x,y] and
<a name="line_22"></a>  triangle_lift(triangle_proj([1-x-y,x,y])) = [1-x-y,x,y],
<a name="line_23"></a>  "triangle_lift and triangle_proj are inverse"
<a name="line_24"></a> ):
<a name="line_25"></a>
<a name="line_26"></a> _ASSERT(expand(R2_to_C(C_mult(u,v)) - R2_to_C(u) * R2_to_C(v)) = 0,"C_mult");
<a name="line_27"></a>
<a name="line_28"></a> _ASSERT(
<a name="line_29"></a>  simplify(S3_to_R3(R3_to_S3([x[1],x[2],x[3]])) -~ [x[1],x[2],x[3]]) = [0$3],
<a name="line_30"></a>  "S3_to_R3 o R3_to_S3 = 1"
<a name="line_31"></a> );
<a name="line_32"></a>
<a name="line_33"></a> err := R3_to_S3(S3_to_R3([x[1],x[2],x[3],x[4]])) -~ [x[1],x[2],x[3],x[4]];
<a name="line_34"></a> err := map(numer,map(simplify,err));
<a name="line_35"></a> err := map(rem,err,x[1]^2+x[2]^2+x[3]^2+x[4]^2-1,x[4]);
<a name="line_36"></a>
<a name="line_37"></a> _ASSERT(err = [0$4],"R3_to_S3 o S3_to_R3 = 1");
<a name="line_38"></a>
<a name="line_39"></a> _ASSERT(
<a name="line_40"></a>  simplify(S1_to_R(R_to_S1(x)) - x) = 0,
<a name="line_41"></a>  "S1_to_R o R_to_S1 = 1"
<a name="line_42"></a> );
<a name="line_43"></a>
<a name="line_44"></a> err := R_to_S1(S1_to_R([x[1],x[2]])) -~ [x[1],x[2]];
<a name="line_45"></a> err := map(numer,map(simplify,err));
<a name="line_46"></a> err := map(rem,err,x[1]^2+x[2]^2-1,x[2]);
<a name="line_47"></a>
<a name="line_48"></a> _ASSERT(err = [0$2],"R_to_S1 o S1_to_R = 1");
<a name="line_49"></a>
<a name="line_50"></a> err0 := combine(I_to_R(R_to_I(t)) - t);
<a name="line_51"></a> err1 := combine(R_to_I(I_to_R(t)) - t) assuming t >= -1 and t <= 1;
<a name="line_52"></a> _ASSERT(err0 = 0 and err1 = 0,"R_to_I and I_to_R are inverse");
<a name="line_53"></a>
<a name="line_54"></a> _ASSERT(
<a name="line_55"></a>  simplify(I_to_S1(S1_to_I([cos(t),sin(t)])) - [cos(t),sin(t)]) = [0,0] and
<a name="line_56"></a>  S1_to_I(I_to_S1(t)) - t = 0,
<a name="line_57"></a>  "S1_to_I and I_to_S1 are inverse"
<a name="line_58"></a> );
<a name="line_59"></a>
<a name="line_60"></a> _ASSERT(
<a name="line_61"></a>  simplify(expand(tan_sum(tan(s),tan(t)) - tan(s+t))) = 0,
<a name="line_62"></a>  "tan_sum"
<a name="line_63"></a> );
<a name="line_64"></a>
<a name="line_65"></a> err0 := nm4(unstereo_shift([x[1],x[2],x[3]],[a[1],a[2],a[3],a[4]]))^2 - 1:
<a name="line_66"></a> err0 := rem(simplify(err0),a[1]^2+a[2]^2+a[3]^2+a[4]^2-1,a[4]);
<a name="line_67"></a> err1 := stereo_shift(unstereo_shift([x[1],x[2],x[3]],[a[1],a[2],a[3],a[4]]),[a[1],a[2],a[3],a[4]]) -~ [x[1],x[2],x[3]]:
<a name="line_68"></a> err1 := map(rem,map(numer,simplify(err1)),a[1]^2+a[2]^2+a[3]^2+a[4]^2-1,a[4]);
<a name="line_69"></a> err2 := unstereo_shift(stereo_shift([x[1],x[2],x[3],x[4]],[a[1],a[2],a[3],a[4]]),[a[1],a[2],a[3],a[4]]) -~ [x[1],x[2],x[3],x[4]]:
<a name="line_70"></a> err2 := map(rem,map(rem,map(numer,simplify(err2)),a[1]^2+a[2]^2+a[3]^2+a[4]^2-1,a[4]),x[1]^2+x[2]^2+x[3]^2+x[4]^2-1,x[4]);
<a name="line_71"></a> err3 := stereo_shift([a[1],a[2],a[3],a[4]],[a[1],a[2],a[3],a[4]]);
<a name="line_72"></a>
<a name="line_73"></a> _ASSERT(err0 = 0 and err1 = [0$3] and err2 = [0$4] and err3 = [0$3],"(un)stereo_shift");
<a name="line_74"></a>
<a name="line_75"></a> _ASSERT(
<a name="line_76"></a>  expand(H_mult(uu,H_mult(vv,ww)) - H_mult(H_mult(uu,vv),ww)) = [0$4] and
<a name="line_77"></a>  expand(H_mult(uu,H_conjugate(uu)) -~ nm4(uu)^2 *~ [0,0,0,1]) = [0$4] and
<a name="line_78"></a>  expand(H_mult([0,0,0,1],uu) -~ uu) = [0$4] and
<a name="line_79"></a>  expand(H_mult(uu,[0,0,0,1]) -~ uu) = [0$4],
<a name="line_80"></a>  "quaternions"
<a name="line_81"></a> );
<a name="line_82"></a>
<a name="line_83"></a> _ASSERT(simplify(nm3(rational_sphere_point(s,t))^2 - 1) = 0,"rational_sphere_point");
<a name="line_84"></a>
<a name="line_85"></a>end:
<a name="line_86"></a>
<a name="line_87"></a>add_check(check_Rn):
<a name="line_88"></a>
<a name="line_89"></a>######################################################################
<a name="line_90"></a>
<a name="line_91"></a>check_hopf_map := proc()
<a name="line_92"></a> local T,uu;
<a name="line_93"></a> 
<a name="line_94"></a> printf("%a()\n",procname);
<a name="line_95"></a>
<a name="line_96"></a> uu := [u[1],u[2],u[3],u[4]];
<a name="line_97"></a>
<a name="line_98"></a> _ASSERT(
<a name="line_99"></a>  simplify(nm3(hopf_map(uu))^2 - 1) = 0 and
<a name="line_100"></a>  simplify(hopf_map(uu) -~ hopf_map(H_mult(uu,[0,0,x,y]))) = [0$3],
<a name="line_101"></a>  "Hopf map"
<a name="line_102"></a> );
<a name="line_103"></a>
<a name="line_104"></a> _ASSERT(
<a name="line_105"></a>   {seq(factor(hopf_map(act_R4[T](xx)) - act_hopf[T](hopf_map(xx))),T in [1,LL,LM,LLLM,LN,LLLN,MN,LLMN])} =
<a name="line_106"></a>    {[0$3]},
<a name="line_107"></a>   "equivariance of the Hopf map"
<a name="line_108"></a> );
<a name="line_109"></a> 
<a name="line_110"></a>end:
<a name="line_111"></a>
<a name="line_112"></a>add_check(check_hopf_map):
  </pre>
 </body>
</html>
    