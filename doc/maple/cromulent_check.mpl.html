<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># This file checks the internal consistency of various definitions
<a name="line_2"></a># given in the file cromulent.mpl.  
<a name="line_3"></a>
<a name="line_4"></a>check_precromulent_defs := proc()
<a name="line_5"></a> local ij,iijj,i,j,m,c,ok,T;
<a name="line_6"></a>
<a name="line_7"></a> printf("%a()\n",procname);
<a name="line_8"></a>
<a name="line_9"></a> iijj := select(ij -> v_on_c[op(ij)] <> NULL,[indices(v_on_c)]);
<a name="line_10"></a> _ASSERT(
<a name="line_11"></a>  `and`(op(map(evalb,map(ij -> c_gen[ij[2]](v_on_c[op(ij)]) = v_gen[ij[1]],iijj)))),
<a name="line_12"></a>  "c_gen is consistent with v_on_c"
<a name="line_13"></a> );
<a name="line_14"></a>
<a name="line_15"></a> _ASSERT(
<a name="line_16"></a>  `and`(seq(evalb(v_on_c[c_basepoint[k],k] <> NULL),k=0..8)),
<a name="line_17"></a>  "c_basepoint[k] lies on C[k]"
<a name="line_18"></a> );
<a name="line_19"></a>
<a name="line_20"></a> _ASSERT(
<a name="line_21"></a>  `and`(op(map(op,[seq(map(i -> evalb(act_V[c_involution[k]](i) = i),map(lhs,c_track[k])),k = 0 .. 8)]))),
<a name="line_22"></a>  "c_involution[k] fixes all vertices on C[k]"
<a name="line_23"></a> );
<a name="line_24"></a>
<a name="line_25"></a> ok := true;
<a name="line_26"></a> for i from 0 to 8 do
<a name="line_27"></a>  for T in [L,M,N] do
<a name="line_28"></a>   j,m,c := op(act_c_data[T,i]);
<a name="line_29"></a>   ok := ok and (act_Z4[T](c_homology[i]) -~ m *~ c_homology[j] = [0$4]);
<a name="line_30"></a>  od;
<a name="line_31"></a> od;
<a name="line_32"></a>
<a name="line_33"></a> _ASSERT(ok,"Action on homology is consistent with action on curves");
<a name="line_34"></a>end:
<a name="line_35"></a>
<a name="line_36"></a>add_check(check_precromulent_defs):  </pre>
 </body>
</html>
    