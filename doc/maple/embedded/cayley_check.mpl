<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_cayley := proc()
<a name="line_2"></a> local CL,i,j;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a> 
<a name="line_6"></a> _ASSERT(nops(cayley_lines) = 27,"27 lines");
<a name="line_7"></a>
<a name="line_8"></a> _ASSERT(
<a name="line_9"></a>  map(ab -> simplify_E(g(ab[1] +~ t *~ ab[2])),cayley_lines) = [0$27],
<a name="line_10"></a>  "The 27 lines all lie in the cubic surface"
<a name="line_11"></a> );
<a name="line_12"></a>
<a name="line_13"></a> CL := proc(i,u,v) 
<a name="line_14"></a>  u *~ cayley_lines[i+1][1] +~ v *~ cayley_lines[i+1][2];
<a name="line_15"></a> end:
<a name="line_16"></a>
<a name="line_17"></a> _ASSERT( simplify_E(c_E[ 0](t) -~ CL(0,cos(t),sin(t))) = [0$4],
<a name="line_18"></a>          "C[0] is a Cayley line");
<a name="line_19"></a>
<a name="line_20"></a> _ASSERT( simplify_E(c_E[ 1](t) -~ CL(1, sin(t)/sqrt(2),cos(t))) = [0$4],
<a name="line_21"></a>          "C[1] is a Cayley line");
<a name="line_22"></a>
<a name="line_23"></a> _ASSERT( simplify_E(c_E[ 2](t) -~ CL(2,-sin(t)/sqrt(2),cos(t))) = [0$4],
<a name="line_24"></a>          "C[2] is a Cayley line");
<a name="line_25"></a>
<a name="line_26"></a> _ASSERT( simplify_E(c_cayley[1](t) -~ CL(3, (1/3)*sqrt(3)*sqrt(-2*a_E^2+1)*sin(t)/sqrt(2*a_E^2+1),-cos(t))) = [0$4],
<a name="line_27"></a>          "c_cayley[1] is a Cayley line");
<a name="line_28"></a>
<a name="line_29"></a> _ASSERT( simplify_E(c_cayley[2](t) -~ CL(4, cos(t), (1/3)*sqrt(3)*sqrt(-2*a_E^2+1)*sin(t)/sqrt(2*a_E^2+1))) = [0$4],
<a name="line_30"></a>          "c_cayley[2] is a Cayley line");
<a name="line_31"></a>
<a name="line_32"></a> _ASSERT( simplify_E(c_cayley[3](t) -~ CL(5,-(1/3)*sqrt(3)*sqrt(-2*a_E^2+1)*sin(t)/sqrt(2*a_E^2+1), cos(t))) = [0$4],
<a name="line_33"></a>          "c_cayley[3] is a Cayley line");
<a name="line_34"></a>
<a name="line_35"></a> _ASSERT( simplify_E(c_cayley[4](t) -~ CL(6,-cos(t),-(1/3)*sqrt(3)*sqrt(-2*a_E^2+1)*sin(t)/sqrt(2*a_E^2+1))) = [0$4],
<a name="line_36"></a>          "c_cayley[4] is a Cayley line");
<a name="line_37"></a>
<a name="line_38"></a> for i from 0 to num_vertices_E - 1 do
<a name="line_39"></a>  for j from 1 to 4 do 
<a name="line_40"></a>   if v_on_c_cayley[i,j] <> NULL then
<a name="line_41"></a>    _ASSERT(simplify_E(v_E[i] -~ c_cayley[j](v_on_c_cayley[i,j])) = [0$4],
<a name="line_42"></a>	    sprintf("v[%d] on c_cayley[%d]",i,j));
<a name="line_43"></a>   fi;
<a name="line_44"></a>  od:
<a name="line_45"></a> od:
<a name="line_46"></a>end:
<a name="line_47"></a>
<a name="line_48"></a>add_check(check_cayley):  </pre>
 </body>
</html>
    