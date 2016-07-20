<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_square_diffeo_E0 := proc()
<a name="line_2"></a> local err,vc,NN,ok,i,j,x0;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> _ASSERT( 
<a name="line_7"></a>  simplify(square_diffeo_E0(c_E0[0](t))[1]) = 0,
<a name="line_8"></a>  "square_diffeo_E0(C[0])"
<a name="line_9"></a> );
<a name="line_10"></a>
<a name="line_11"></a> _ASSERT(
<a name="line_12"></a>  simplify(square_diffeo_E0(c_E0[1](t))[2]) = 0,
<a name="line_13"></a>  "square_diffeo_E0(C[1])"
<a name="line_14"></a> );
<a name="line_15"></a>
<a name="line_16"></a> _ASSERT(
<a name="line_17"></a>  simplify(1 - square_diffeo_E0(c_E0[3](t))[2]) = 0,
<a name="line_18"></a>  "square_diffeo_E0(C[3])"
<a name="line_19"></a> );
<a name="line_20"></a> 
<a name="line_21"></a> err := 1 - square_diffeo_E0(c_alt[5](t))[1];
<a name="line_22"></a> err := factor(simplify(err))  assuming t > 0 and t < Pi;
<a name="line_23"></a>
<a name="line_24"></a> _ASSERT(err=0,"square_diffeo_E0(C[5])");
<a name="line_25"></a>
<a name="line_26"></a> NN := 64;
<a name="line_27"></a> ok := true;
<a name="line_28"></a>
<a name="line_29"></a> for i from 0 to NN do
<a name="line_30"></a>  for j from 0 to NN do
<a name="line_31"></a>   x0 := evalf(t_lift([i/NN,j/NN]));
<a name="line_32"></a>   if evalf(square_diffeo_E0_J(x0)) < 0.1 then
<a name="line_33"></a>    ok := false;
<a name="line_34"></a>    break;
<a name="line_35"></a>   fi;   
<a name="line_36"></a>  od;
<a name="line_37"></a>  if not ok then break; fi;
<a name="line_38"></a> od;
<a name="line_39"></a>
<a name="line_40"></a> _ASSERT(ok,"Jacobian of square_diffeo_E0 is >= 0.1 everywhere in F16");
<a name="line_41"></a>end:
<a name="line_42"></a>
<a name="line_43"></a>add_check(check_square_diffeo_E0):
<a name="line_44"></a>
<a name="line_45"></a>######################################################################
<a name="line_46"></a>
<a name="line_47"></a>check_square_diffeo_E0_alt := proc()
<a name="line_48"></a> local err,vc,NN,ok,i,j,x0;
<a name="line_49"></a>
<a name="line_50"></a> printf("%a()\n",procname);
<a name="line_51"></a>
<a name="line_52"></a> _ASSERT( 
<a name="line_53"></a>  simplify(square_diffeo_E0_alt(c_E0[0](t))[1]) = 0,
<a name="line_54"></a>  "square_diffeo_E0_alt(C[0])"
<a name="line_55"></a> );
<a name="line_56"></a>
<a name="line_57"></a> _ASSERT(
<a name="line_58"></a>  simplify(square_diffeo_E0_alt(c_E0[1](t))[2]) = 0,
<a name="line_59"></a>  "square_diffeo_E0_alt(C[1])"
<a name="line_60"></a> );
<a name="line_61"></a>
<a name="line_62"></a> _ASSERT(
<a name="line_63"></a>  simplify(1 - square_diffeo_E0_alt(c_E0[3](t))[2]) = 0,
<a name="line_64"></a>  "square_diffeo_E0_alt(C[3])"
<a name="line_65"></a> );
<a name="line_66"></a> 
<a name="line_67"></a> _ASSERT(
<a name="line_68"></a>  simplify(1 - square_diffeo_E0_alt(c_E0[5](t))[1]) = 0,
<a name="line_69"></a>  "square_diffeo_E0_alt(C[5])"
<a name="line_70"></a> );
<a name="line_71"></a> 
<a name="line_72"></a> NN := 64;
<a name="line_73"></a> ok := true;
<a name="line_74"></a>
<a name="line_75"></a> for i from 0 to NN do
<a name="line_76"></a>  for j from 0 to NN do
<a name="line_77"></a>   x0 := evalf(t_lift([i/NN,j/NN]));
<a name="line_78"></a>   if evalf(square_diffeo_E0_alt_J(x0)) < 0.1 then
<a name="line_79"></a>    ok := false;
<a name="line_80"></a>    break;
<a name="line_81"></a>   fi;   
<a name="line_82"></a>  od;
<a name="line_83"></a>  if not ok then break; fi;
<a name="line_84"></a> od;
<a name="line_85"></a>
<a name="line_86"></a> _ASSERT(ok,"Jacobian of square_diffeo_E0_alt is >= 0.1 everywhere in F16");
<a name="line_87"></a>end:
<a name="line_88"></a>
<a name="line_89"></a>add_check(check_square_diffeo_E0_alt):
  </pre>
 </body>
</html>
    