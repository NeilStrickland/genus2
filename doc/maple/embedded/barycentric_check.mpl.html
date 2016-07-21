<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_barycentric := proc()
<a name="line_2"></a> local P,P0,P1,P2,A,A0,t,tolerance,i;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> # We will use the following points as test data.
<a name="line_7"></a> # [P[1],P[2],P[3]] and [P[2],P[3],P[4]] are triangles of reasonable size,
<a name="line_8"></a> # not too far from being equilateral.  P[5] is in [P[1],P[2],P[3]].
<a name="line_9"></a>
<a name="line_10"></a> P[1] := [2902516/8526427, 903675/1218061,  sqrt(2) * 1424/3913, -2237104/8526427];
<a name="line_11"></a> P[2] := [681/1406,        5797/7030,       sqrt(2) * 37/190,    -344/3515];
<a name="line_12"></a> P[3] := [788/2665,        459/533,         sqrt(2) * 52/205,    -548/2665];
<a name="line_13"></a> P[4] := [214099/533478,   217940/266739,   sqrt(2) * 73/274,    -91469/533478];
<a name="line_14"></a> P[5] := [284246/653235,   1387235/1829058, sqrt(2) * 827/2590,  -1623401/9145290];
<a name="line_15"></a>
<a name="line_16"></a> for i from 1 to 5 do P0[i] := evalf(P[i]); od;
<a name="line_17"></a> A  := [P[1],P[2],P[3]];
<a name="line_18"></a> A0 := [P0[1],P0[2],P0[3]];
<a name="line_19"></a>
<a name="line_20"></a> t := barycentric_coords(A0,P[5]);
<a name="line_21"></a>
<a name="line_22"></a> P2[5] := barycentric_inverse(A0,t);
<a name="line_23"></a> P2[6] := barycentric_inverse(A0,[0,1/3,2/3]);
<a name="line_24"></a>
<a name="line_25"></a> tolerance := 10^(-98);
<a name="line_26"></a>
<a name="line_27"></a> _ASSERT(barycentric_coords(A,P[1]) = [1,0,0] and
<a name="line_28"></a>         barycentric_coords(A,P[2]) = [0,1,0] and
<a name="line_29"></a>         barycentric_coords(A,P[3]) = [0,0,1],
<a name="line_30"></a>  "barycentric_coords on vertices"
<a name="line_31"></a> );
<a name="line_32"></a>
<a name="line_33"></a> _ASSERT(d4f(P0[5],P2[5]) < tolerance,
<a name="line_34"></a>  "barycentric_inverse o barycentric_coords");
<a name="line_35"></a>
<a name="line_36"></a> _ASSERT(
<a name="line_37"></a>  d3f(barycentric_coords(A0,P2[6]),[0,1/3,2/3]) < tolerance,
<a name="line_38"></a>  "barycentric_coords o barycentric_inverse"
<a name="line_39"></a> );
<a name="line_40"></a>
<a name="line_41"></a> _ASSERT(
<a name="line_42"></a>  d3f(barycentric_coords([P0[2],P0[3],P0[4]],P2[6]),[1/3,2/3,0]) < tolerance,
<a name="line_43"></a>  "edge matching via barycentric_inverse"
<a name="line_44"></a> );
<a name="line_45"></a>end:
<a name="line_46"></a>
<a name="line_47"></a>add_check(check_barycentric):  </pre>
 </body>
</html>
    