<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_extra_vertices := proc()
<a name="line_2"></a> local i,j,k,t0,T,ok,err;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> for k from 0 to num_vertices_E-1 do
<a name="line_7"></a>  _ASSERT(is_member_E(v_E[k]),sprintf("v[%d] is in EX(a)",k));
<a name="line_8"></a>
<a name="line_9"></a>  for T in G16 do
<a name="line_10"></a>   _ASSERT(
<a name="line_11"></a>    act_E[T](v_E[k]) = v_E[v_E_permlist[T][k+1]],
<a name="line_12"></a>    sprintf("Action of %A on v[%d]",T,k)
<a name="line_13"></a>   );
<a name="line_14"></a>  od:
<a name="line_15"></a> od;
<a name="line_16"></a>
<a name="line_17"></a> for i from 0 to num_vertices_E-1 do
<a name="line_18"></a>  for j from 0 to num_curves_E-1 do
<a name="line_19"></a>   t0 := v_on_c_E[i,j];
<a name="line_20"></a>   if t0 <> NULL then
<a name="line_21"></a>    if i < 22 then 
<a name="line_22"></a>     err := simplify_E(v_E[i] -~ c_E[j](t0));
<a name="line_23"></a>     ok := evalb(err = [0$4]);
<a name="line_24"></a>    else
<a name="line_25"></a>     err := v_E[i] -~ c_E[j](t0);
<a name="line_26"></a>     err := max(seq(evalf(nm4(subs(a_E = 0.1*i,err))),i=1..7));
<a name="line_27"></a>     ok := evalb (err < 10.^(-95));
<a name="line_28"></a>    fi;
<a name="line_29"></a>    _ASSERT(ok,sprintf("v[%2d] on c[%2d]",i,j));
<a name="line_30"></a>   fi;
<a name="line_31"></a>  od:
<a name="line_32"></a> od:
<a name="line_33"></a>end:
<a name="line_34"></a>
<a name="line_35"></a>add_check(check_extra_vertices):
<a name="line_36"></a>
  </pre>
 </body>
</html>
    