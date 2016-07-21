<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># This file finds the 27 lines on the cubic surface g = 0.
<a name="line_2"></a>
<a name="line_3"></a><span style="color:red">#@ find_cayley_lines 
</span><a name="line_4"></a>find_cayley_lines := proc() 
<a name="line_5"></a> local a,aa,aa0,b,bb,bb0,rels,cell_rules,all_sols,sols,R;
<a name="line_6"></a> global cayley_lines;
<a name="line_7"></a>
<a name="line_8"></a> # aa and bb span a Cayley line if the terms in rels are all zero.
<a name="line_9"></a> aa := [seq(a[i],i=1..4)];
<a name="line_10"></a> bb := [seq(b[i],i=1..4)];
<a name="line_11"></a> rels := [coeffs(expand(g(aa +~ t *~ bb)),t)];
<a name="line_12"></a>
<a name="line_13"></a> # We can describe each Cayley line in terms of its row-reduced
<a name="line_14"></a> # basis.  This divides the relevant Grassmannian into six Schubert
<a name="line_15"></a> # cells, as follows.
<a name="line_16"></a> cell_rules := [
<a name="line_17"></a>  {a[1]=1,a[2]=0,b[1]=0,b[2]=1},
<a name="line_18"></a>  {a[1]=1,a[3]=0,b[1]=0,b[2]=0,b[3]=1},
<a name="line_19"></a>  {a[1]=1,a[4]=0,b[1]=0,b[2]=0,b[3]=0,b[4]=1},
<a name="line_20"></a>  {a[1]=0,a[2]=1,a[3]=0,b[1]=0,b[2]=0,b[3]=1},
<a name="line_21"></a>  {a[1]=0,a[2]=1,a[4]=0,b[1]=0,b[2]=0,b[3]=0,b[4]=1},
<a name="line_22"></a>  {a[1]=0,a[2]=0,a[3]=1,a[4]=0,b[1]=0,b[2]=0,b[3]=0,b[4]=1}
<a name="line_23"></a> ]:
<a name="line_24"></a>
<a name="line_25"></a> # We now find the solutions in each cell.
<a name="line_26"></a>
<a name="line_27"></a> all_sols := NULL:
<a name="line_28"></a> for R in cell_rules do
<a name="line_29"></a>  aa0 := subs(R,aa);
<a name="line_30"></a>  bb0 := subs(R,bb);
<a name="line_31"></a>  sols := [solve(subs(R,rels),{op(aa0),op(bb0)} minus {0,1})];
<a name="line_32"></a>  sols := map(s -> simplify_E(subs(s,[aa0,bb0])),sols);
<a name="line_33"></a>  all_sols := all_sols,op(sols);
<a name="line_34"></a> od:
<a name="line_35"></a> cayley_lines := {all_sols}:
<a name="line_36"></a>
<a name="line_37"></a> return cayley_lines;
<a name="line_38"></a>end:
<a name="line_39"></a>
<a name="line_40"></a>######################################################################
<a name="line_41"></a>
<a name="line_42"></a># The procedure below executes automatically because of the () at the end.
<a name="line_43"></a># This sets cayley_lines in essentially the same way as find_cayley_lines(),
<a name="line_44"></a># but the result is ordered in a more structured way and annotated.
<a name="line_45"></a>
<a name="line_46"></a><span style="color:red">#@ cayley_lines
</span><a name="line_47"></a>proc()
<a name="line_48"></a> global cayley_lines;
<a name="line_49"></a> local p1,p2;
<a name="line_50"></a>
<a name="line_51"></a> p1 := sqrt(-4*a_E^2+2);
<a name="line_52"></a> p2 := sqrt(-4*a_E^4+6*a_E^2-2);
<a name="line_53"></a>
<a name="line_54"></a> cayley_lines := [
<a name="line_55"></a>  [[1, 0, 0, 0],
<a name="line_56"></a>   [0, 1, 0, 0]], # C[0]
<a name="line_57"></a>  [[1, 1, 0, 0],
<a name="line_58"></a>   [0, 0, 1, 0]], # C[1]
<a name="line_59"></a>  [[1,-1, 0, 0],
<a name="line_60"></a>   [0, 0, 1, 0]], # C[2]
<a name="line_61"></a>  [[1, 0, -4*a_E/p1,  2/p1],
<a name="line_62"></a>   [0, 1, 0, 0]],
<a name="line_63"></a>  [[1, 0, 0, 0],
<a name="line_64"></a>   [0, 1, -4*a_E/p1, -2/p1]],
<a name="line_65"></a>  [[1, 0,  4*a_E/p1, -2/p1], [0, 1, 0, 0]],
<a name="line_66"></a>  [[1, 0, 0, 0], [0, 1,  4*a_E/p1,  2/p1]],
<a name="line_67"></a>  [[1, 0, -(1/2)*(I*sqrt(2)-p1)*a_E/(a_E^2-1),  (1/2*I)*sqrt(2)],
<a name="line_68"></a>   [0, 1, -(1/2)*(I*sqrt(2)*p1+4*a_E^2-2)*a_E/(p1*(a_E^2-1)), -(1/2*I)*sqrt(2)]],
<a name="line_69"></a>  [[1, 0, -(1/2)*(I*sqrt(2)-p1)*a_E/(a_E^2-1),  (1/2*I)*sqrt(2)],
<a name="line_70"></a>   [0, 1,  (1/2)*(I*sqrt(2)*p1+4*a_E^2-2)*a_E/(p1*(a_E^2-1)),  (1/2*I)*sqrt(2)]],
<a name="line_71"></a>  [[1, 0,  (1/2)*(I*sqrt(2)-p1)*a_E/(a_E^2-1), -(1/2*I)*sqrt(2)],
<a name="line_72"></a>   [0, 1, -(1/2)*(I*sqrt(2)*p1+4*a_E^2-2)*a_E/(p1*(a_E^2-1)), -(1/2*I)*sqrt(2)]],
<a name="line_73"></a>  [[1, 0,  (1/2)*(I*sqrt(2)-p1)*a_E/(a_E^2-1), -(1/2*I)*sqrt(2)],
<a name="line_74"></a>   [0, 1,  (1/2)*(I*sqrt(2)*p1+4*a_E^2-2)*a_E/(p1*(a_E^2-1)),  (1/2*I)*sqrt(2)]],
<a name="line_75"></a>  [[1, 0, -(1/2)*(I*sqrt(2)+p1)*a_E/(a_E^2-1),  (1/2*I)*sqrt(2)],
<a name="line_76"></a>   [0, 1, -(1/2)*(I*sqrt(2)*p1-4*a_E^2+2)*a_E/(p1*(a_E^2-1)), -(1/2*I)*sqrt(2)]],
<a name="line_77"></a>  [[1, 0, -(1/2)*(I*sqrt(2)+p1)*a_E/(a_E^2-1),  (1/2*I)*sqrt(2)],
<a name="line_78"></a>   [0, 1,  (1/2)*(I*sqrt(2)*p1-4*a_E^2+2)*a_E/(p1*(a_E^2-1)),  (1/2*I)*sqrt(2)]],
<a name="line_79"></a>  [[1, 0,  (1/2)*(I*sqrt(2)+p1)*a_E/(a_E^2-1), -(1/2*I)*sqrt(2)],
<a name="line_80"></a>   [0, 1, -(1/2)*(I*sqrt(2)*p1-4*a_E^2+2)*a_E/(p1*(a_E^2-1)), -(1/2*I)*sqrt(2)]],
<a name="line_81"></a>  [[1, 0,  (1/2)*(I*sqrt(2)+p1)*a_E/(a_E^2-1), -(1/2*I)*sqrt(2)],
<a name="line_82"></a>   [0, 1,  (1/2)*(I*sqrt(2)*p1-4*a_E^2+2)*a_E/(p1*(a_E^2-1)),  (1/2*I)*sqrt(2)]],
<a name="line_83"></a>  [[1, 0, -(1/2)*(-2*a_E^2+p2+2)*a_E/(a_E^2-1)^(3/2),  1/(2*sqrt(a_E^2-1))],
<a name="line_84"></a>   [0, 1,  a_E*(-2*a_E^2+1+p2)/(sqrt(a_E^2-1)*p2), -1/(2*sqrt(a_E^2-1))]],
<a name="line_85"></a>  [[1, 0, -(1/2)*(-2*a_E^2+p2+2)*a_E/(a_E^2-1)^(3/2),  1/(2*sqrt(a_E^2-1))],
<a name="line_86"></a>   [0, 1, -a_E*(-2*a_E^2+1+p2)/(sqrt(a_E^2-1)*p2),  1/(2*sqrt(a_E^2-1))]],
<a name="line_87"></a>  [[1, 0,  (1/2)*(-2*a_E^2+p2+2)*a_E/(a_E^2-1)^(3/2), -1/(2*sqrt(a_E^2-1))],
<a name="line_88"></a>   [0, 1,  a_E*(-2*a_E^2+1+p2)/(sqrt(a_E^2-1)*p2), -1/(2*sqrt(a_E^2-1))]],
<a name="line_89"></a>  [[1, 0,  (1/2)*(-2*a_E^2+p2+2)*a_E/(a_E^2-1)^(3/2), -1/(2*sqrt(a_E^2-1))],
<a name="line_90"></a>   [0, 1, -a_E*(-2*a_E^2+1+p2)/(sqrt(a_E^2-1)*p2),  1/(2*sqrt(a_E^2-1))]],
<a name="line_91"></a>  [[1, 0, -(1/2)*( 2*a_E^2+p2-2)*a_E/(a_E^2-1)^(3/2), -1/(2*sqrt(a_E^2-1))],
<a name="line_92"></a>   [0, 1,  a_E*( 2*a_E^2-1+p2)/(sqrt(a_E^2-1)*p2), -1/(2*sqrt(a_E^2-1))]],
<a name="line_93"></a>  [[1, 0, -(1/2)*( 2*a_E^2+p2-2)*a_E/(a_E^2-1)^(3/2), -1/(2*sqrt(a_E^2-1))],
<a name="line_94"></a>   [0, 1, -a_E*( 2*a_E^2-1+p2)/(sqrt(a_E^2-1)*p2),  1/(2*sqrt(a_E^2-1))]],
<a name="line_95"></a>  [[1, 0,  (1/2)*( 2*a_E^2+p2-2)*a_E/(a_E^2-1)^(3/2),  1/(2*sqrt(a_E^2-1))],
<a name="line_96"></a>   [0, 1,  a_E*( 2*a_E^2-1+p2)/(sqrt(a_E^2-1)*p2), -1/(2*sqrt(a_E^2-1))]],
<a name="line_97"></a>  [[1, 0,  (1/2)*( 2*a_E^2+p2-2)*a_E/(a_E^2-1)^(3/2),  1/(2*sqrt(a_E^2-1))],
<a name="line_98"></a>   [0, 1, -a_E*( 2*a_E^2-1+p2)/(sqrt(a_E^2-1)*p2),  1/(2*sqrt(a_E^2-1))]],
<a name="line_99"></a>  [[1,  sqrt(3-2*a_E^2-2*sqrt(2)*sqrt(-a_E^2+1))/sqrt(2*a_E^2-1), 0, 0],
<a name="line_100"></a>   [0, 0, 1,  (1/2)*sqrt(2)*sqrt(-a_E^2+1)/a_E]],
<a name="line_101"></a>  [[1,  sqrt(3-2*a_E^2+2*sqrt(2)*sqrt(-a_E^2+1))/sqrt(2*a_E^2-1), 0, 0],
<a name="line_102"></a>   [0, 0, 1, -(1/2)*sqrt(2)*sqrt(-a_E^2+1)/a_E]],
<a name="line_103"></a>  [[1, -sqrt(3-2*a_E^2-2*sqrt(2)*sqrt(-a_E^2+1))/sqrt(2*a_E^2-1), 0, 0],
<a name="line_104"></a>   [0, 0, 1,  (1/2)*sqrt(2)*sqrt(-a_E^2+1)/a_E]],
<a name="line_105"></a>  [[1, -sqrt(3-2*a_E^2+2*sqrt(2)*sqrt(-a_E^2+1))/sqrt(2*a_E^2-1), 0, 0],
<a name="line_106"></a>   [0, 0, 1, -(1/2)*sqrt(2)*sqrt(-a_E^2+1)/a_E]]
<a name="line_107"></a> ]:
<a name="line_108"></a>end():
<a name="line_109"></a>
<a name="line_110"></a><span style="color:red">#@ c_cayley
</span><a name="line_111"></a>
<a name="line_112"></a>c_cayley[1] := (t) -> [sqrt((1-2*a_E^2)/(1+2*a_E^2)/3)*sin(t),
<a name="line_113"></a>                       -cos(t),
<a name="line_114"></a>                       -4*a_E/sqrt(6)/sqrt(1+2*a_E^2)*sin(t),
<a name="line_115"></a>                       2/sqrt(6)/sqrt(1+2*a_E^2)*sin(t)];
<a name="line_116"></a>
<a name="line_117"></a>c_cayley[2] := unapply(simplify(act_E[L](c_cayley[1](t))),t);
<a name="line_118"></a>c_cayley[3] := unapply(simplify(act_E[L](c_cayley[2](t))),t);
<a name="line_119"></a>c_cayley[4] := unapply(simplify(act_E[L](c_cayley[3](t))),t);
<a name="line_120"></a>
<a name="line_121"></a>######################################################################
<a name="line_122"></a>
<a name="line_123"></a># Assuming that ab=[a,b], where a and b are linearly independent in C^4,
<a name="line_124"></a># the function is_in_line(ab,c) will return true if c lies in the C-linear
<a name="line_125"></a># span of a and b.
<a name="line_126"></a>
<a name="line_127"></a><span style="color:red">#@ is_in_line 
</span><a name="line_128"></a>is_in_line := proc(ab,c)
<a name="line_129"></a> local i,d,M,N,J;
<a name="line_130"></a> M := Matrix([op(ab),c]);
<a name="line_131"></a> for i from 1 to 4 do
<a name="line_132"></a>  J := sort([op({1,2,3,4} minus {i})]);
<a name="line_133"></a>  N := SubMatrix(M,1..3,J);
<a name="line_134"></a>  d := simplify(Determinant(N));
<a name="line_135"></a>  if d <> 0 then return false; fi;
<a name="line_136"></a> od:
<a name="line_137"></a> return true;
<a name="line_138"></a>end:
  </pre>
 </body>
</html>
    