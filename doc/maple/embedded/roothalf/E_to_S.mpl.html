<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a><a name="CLASS_E_to_S_map"></a><span style="color:red">#@ CLASS: E_to_S_map
</span><a name="line_2"></a>
<a name="line_3"></a>`Class/Declare`("E_to_S_map",
<a name="line_4"></a> "An instance of this class represents a pair of approximations to the canonical conformal maps $p,q\\colon EX^*\\to S^2$.",
<a name="line_5"></a>
<a name="line_6"></a> ["Field","u"::table,"This is a table indexed by $\\{1,2,3\\}$.  Each entry is a rational function from the $z$-plane to $\\mathbb{R}$, which gives an invariant real valued function on $EX^*$.  The $k$'th component of our map $p\\colon EX^*\\to S^2$ will be the product of $u_k$ with a standard function, recorded in the global variable @s2p_core[k]@."],
<a name="line_7"></a>
<a name="line_8"></a> ["Field","samples_z_u"::table,"This is a table indexed by $\\{1,2,3\\}$.  The $k$'th entry is a list of pairs $(a,b)$, where we have learned by some other means that $u_k(a)$ should be equal to $b$.  We will try to adjust the coefficients of $u_k$ to make this true."],
<a name="line_9"></a>
<a name="line_10"></a> ["Field","p","Our approximation to $p$ will be stored in this field."],
<a name="line_11"></a>
<a name="line_12"></a> ["Constructor","",
<a name="line_13"></a>  proc(this)
<a name="line_14"></a>   this["u"] := table():
<a name="line_15"></a>   this["samples_z_u"] := table():
<a name="line_16"></a>   this["samples_z_u"][1] := [];
<a name="line_17"></a>   this["samples_z_u"][2] := [];
<a name="line_18"></a>   this["samples_z_u"][3] := [];
<a name="line_19"></a>  end
<a name="line_20"></a> ],
<a name="line_21"></a>
<a name="line_22"></a> ["Method","find_p","This sets the fields @u@, @samples_z_u@ and @p@ based on information stored in the atlas object which is suppied as a parameter to the method.",
<a name="line_23"></a>  proc(this,EH_atlas,d1::posint := 8)
<a name="line_24"></a>   local i,j,k,psi_inv0,phi_inv0,p1,ns,mf,mm,
<a name="line_25"></a>    samples_H,samples_E,samples_z,samples_S,samples_z_u,
<a name="line_26"></a>    f1,n1,m1,M1,M2,v,w;
<a name="line_27"></a>
<a name="line_28"></a>   psi_inv0 := eval(EH_atlas["H_to_P_map"]["psi_inv"]):
<a name="line_29"></a>   phi_inv0 := eval(EH_atlas["H_to_P_map"]["phi_inv"]):
<a name="line_30"></a>
<a name="line_31"></a>   samples_H := EH_atlas["H_samples"]:
<a name="line_32"></a>   samples_E := EH_atlas["H_samples_q"]:
<a name="line_33"></a>   samples_z := map(z_proj1,samples_E):
<a name="line_34"></a>   p1  := eval(EH_atlas["H_to_P_map"]["p1"]):
<a name="line_35"></a>   samples_S := map(C_to_S2,map(trim,map(phi_inv0,map(p1,map(psi_inv0,samples_H))),10.^(-80))):
<a name="line_36"></a>   ns := nops(samples_H):
<a name="line_37"></a>
<a name="line_38"></a>   this["u"] := table():
<a name="line_39"></a>   this["samples_z_u"] := table():
<a name="line_40"></a>
<a name="line_41"></a>   for k from 1 to 3 do 
<a name="line_42"></a>    mf[k] := unapply(evalf(s2p_core[k]),x);
<a name="line_43"></a>    samples_z_u[k] := NULL:
<a name="line_44"></a>    for i from 1 to ns do
<a name="line_45"></a>     mm := mf[k](samples_E[i]):
<a name="line_46"></a>     if abs(mm) > 10.^(-3) then
<a name="line_47"></a>      samples_z_u[k] :=
<a name="line_48"></a>       samples_z_u[k],[op(samples_z[i]),samples_S[i][k]/mm];
<a name="line_49"></a>     fi;
<a name="line_50"></a>    od:
<a name="line_51"></a>    samples_z_u[k] := [samples_z_u[k]]:
<a name="line_52"></a>    this["samples_z_u"][k] := samples_z_u[k];
<a name="line_53"></a>   od:
<a name="line_54"></a>
<a name="line_55"></a>   for k from 1 to 3 do 
<a name="line_56"></a>    f1 := (z) -> [seq(seq(z[1]^i*z[2]^j,j=0..d1-i),i=0..d1)]:
<a name="line_57"></a>    n1 := nops(f1([0,0]));
<a name="line_58"></a>    m1 := nops(samples_z_u[k]);
<a name="line_59"></a>    M1 := evalf(Matrix([seq(f1(samples_z_u[k][j]),j=1..m1)]));  
<a name="line_60"></a>    M2 := evalf(Matrix([seq(samples_z_u[k][j][3] *~ f1(samples_z_u[k][j]),j=1..m1)])); 
<a name="line_61"></a>    v,w := op(quot_approx(M1,M2)):
<a name="line_62"></a>    this["u"][k] := unapply(add(w[j]*f1(z)[j],j=1..n1)/add(v[j]*f1(z)[j],j=1..n1),z):
<a name="line_63"></a>   od:
<a name="line_64"></a>
<a name="line_65"></a>   this["set_p"];
<a name="line_66"></a>
<a name="line_67"></a>   NULL;
<a name="line_68"></a>  end
<a name="line_69"></a> ],
<a name="line_70"></a>
<a name="line_71"></a> ["Method","set_p","",
<a name="line_72"></a>  proc(this)
<a name="line_73"></a>   local k,mf;
<a name="line_74"></a>
<a name="line_75"></a>   for k from 1 to 3 do 
<a name="line_76"></a>    mf[k] := unapply(evalf(s2p_core[k]),x);
<a name="line_77"></a>   od;
<a name="line_78"></a>
<a name="line_79"></a>   this["p"] := unapply(evalf([seq(mf[k](x)*this["u"][k](z_proj0(x)),k=1..3)]),x):
<a name="line_80"></a>  end
<a name="line_81"></a> ],
<a name="line_82"></a>
<a name="line_83"></a> ["Method","radius_error","This returns $\\|p(x)\|^2-1$ a function of $z$",
<a name="line_84"></a>  proc(this)
<a name="line_85"></a>   local err;
<a name="line_86"></a>
<a name="line_87"></a>   err := add(this["u"][i](z)^2 * NF_z0(s2p_core[i]^2),i=1..3) - 1;
<a name="line_88"></a>   err := simplify(err);
<a name="line_89"></a>   return unapply(err,z);
<a name="line_90"></a>  end
<a name="line_91"></a> ],
<a name="line_92"></a>
<a name="line_93"></a> ["Method","conformal_error","Given a point $x0\\in EX^*$, this measures the failure of $p$ near $x0$ to be conformal.",
<a name="line_94"></a>  proc(this,x0::RR0_4)
<a name="line_95"></a>   local p,u,v,a,b,c,E,F,G;
<a name="line_96"></a>
<a name="line_97"></a>   p := eval(this["p"]);
<a name="line_98"></a>
<a name="line_99"></a>   u,v := op(tangent_frame_b(x0)):
<a name="line_100"></a>   a := p(x0):
<a name="line_101"></a>   b := subs(t=0,map(diff,p(x0 +~ t *~ u),t)):
<a name="line_102"></a>   c := subs(t=0,map(diff,p(x0 +~ t *~ v),t)):
<a name="line_103"></a>   E := dp3(b,b);
<a name="line_104"></a>   F := dp3(b,c);
<a name="line_105"></a>   G := dp3(c,c);
<a name="line_106"></a>
<a name="line_107"></a>   return (E-G)^2 + 4*F^2;
<a name="line_108"></a>  end
<a name="line_109"></a> ],
<a name="line_110"></a>
<a name="line_111"></a> ["Method","p_vertex_check","Check that $p$ has the right behaviour on $v_0$, $v_3$ and $v_6$.  The return value is a list of nine scalars which should all be zero.",
<a name="line_112"></a>  proc(this)
<a name="line_113"></a>   local p;
<a name="line_114"></a>   p := eval(this["p"]);
<a name="line_115"></a>
<a name="line_116"></a>   map(op,tidy(evalf([seq(p(v_E1[k]) -~ v_S2[k],k in [0,3,6])])));
<a name="line_117"></a>  end
<a name="line_118"></a> ],
<a name="line_119"></a>
<a name="line_120"></a> ["Method","p_chart_check","Check that $p$ gives a conformal map to $S^2$.  The argument @A@ can either be an instance of the class @E_chart@, or a point in $EX^*$.  In the latter case, we generate a new chart at the specified point, using the second argument @d_@ to specify the degree.  The return value is a list of three polynomials in $t_1$ and $t_2$, which will be zero if $p$ is exactly conformal. ",
<a name="line_121"></a>  proc(this,A,d_::integer)
<a name="line_122"></a>   local C,d,p,q,pq,pq1,pq2;
<a name="line_123"></a>
<a name="line_124"></a>   if type(A,E_chart) then
<a name="line_125"></a>    C := eval(A):
<a name="line_126"></a>   elif type(A,RR0_4) then
<a name="line_127"></a>    C := `new/E_chart`():
<a name="line_128"></a>    C["centre_set_numeric",A]:
<a name="line_129"></a>    C["set_degree_numeric",d_];
<a name="line_130"></a>   fi;
<a name="line_131"></a>   d := C["degree"];
<a name="line_132"></a>   p := eval(this["p"]);
<a name="line_133"></a>   q := eval(C["p"]);
<a name="line_134"></a>   pq := multi_series(p(q(t)),d+1,t[1],t[2]);
<a name="line_135"></a>   pq1 := map(diff,pq,t[1]);
<a name="line_136"></a>   pq2 := map(diff,pq,t[2]);
<a name="line_137"></a>   multi_series([dp3(pq,pq)-1,dp3(pq1,pq2),dp3(pq1,pq1)-dp3(pq2,pq2)],d,t[1],t[2]);
<a name="line_138"></a>  end
<a name="line_139"></a> ],
<a name="line_140"></a>
<a name="line_141"></a> ["Method","u_plot","Generates a plot of the function $u_k$",
<a name="line_142"></a>  proc(this,k::integer)
<a name="line_143"></a>   display(z_plot(this["u"][k](z)),
<a name="line_144"></a>           map(point,this["samples_z_u"][k],colour=black));
<a name="line_145"></a>  end
<a name="line_146"></a> ],
<a name="line_147"></a>
<a name="line_148"></a> ["Method","u_plots","Generates plots of the functions $u_1$, $u_2$ and $u_3$.",
<a name="line_149"></a>  proc(this)
<a name="line_150"></a>   display(Transpose(Vector([seq(this["u_plot",k],k=1..3)])));
<a name="line_151"></a>  end
<a name="line_152"></a> ],
<a name="line_153"></a>
<a name="line_154"></a> ["Method","p_plot","Generates a plot showing the image in $S^2$ of a grid in $F_{16}\\subset EX^*$.",
<a name="line_155"></a>  proc(this)
<a name="line_156"></a>   local i,j,N,v11,theta,T,U,p,u;
<a name="line_157"></a>
<a name="line_158"></a>   p := eval(this["p"]);
<a name="line_159"></a>   u := eval(this["u"]);
<a name="line_160"></a>   v11 := evalf(p(v_E0[11]));
<a name="line_161"></a>   theta := arctan(-v11[1]/v11[3]);
<a name="line_162"></a>
<a name="line_163"></a>   require_square_diffeo_E0_inverse():
<a name="line_164"></a>   T := square_diffeo_E0_inverse_table:
<a name="line_165"></a>   N := square_diffeo_E0_inverse_order:
<a name="line_166"></a>   for i from 0 to N do
<a name="line_167"></a>    for j from 0 to N do
<a name="line_168"></a>     U[i,j] := evalf(p(T[i/N,j/N]));
<a name="line_169"></a>    od:
<a name="line_170"></a>   od:
<a name="line_171"></a>
<a name="line_172"></a>   display(
<a name="line_173"></a>    seq(seq(polygon([U[i,j],U[i+1,j],U[i+1,j+1]],colour=white,style=patch),i=0..N-1),j=0..N-1),
<a name="line_174"></a>    seq(seq(polygon([U[i,j],U[i,j+1],U[i+1,j+1]],colour=white,style=patch),i=0..N-1),j=0..N-1),
<a name="line_175"></a>    spacecurve([ cos(t),sin(t),0],t=0..Pi/2    ,colour=c_colour[0]),
<a name="line_176"></a>    spacecurve([0,cos(t),-sin(t)],t=0..Pi/2    ,colour=c_colour[1]),
<a name="line_177"></a>    spacecurve([sin(t),0,-cos(t)],t=theta..Pi/2,colour=c_colour[3]),
<a name="line_178"></a>    spacecurve([sin(t),0,-cos(t)],t=0..theta   ,colour=c_colour[5]),
<a name="line_179"></a>    scaling=constrained,axes=none
<a name="line_180"></a>   );
<a name="line_181"></a>  end
<a name="line_182"></a> ],
<a name="line_183"></a>
<a name="line_184"></a> ["Method","radius_error_plot","",
<a name="line_185"></a>  proc(this)
<a name="line_186"></a>   z_plot(this["radius_error"](z));
<a name="line_187"></a>  end
<a name="line_188"></a> ],
<a name="line_189"></a>
<a name="line_190"></a> ["Method","conformal_error_plot","",
<a name="line_191"></a>  proc(this)
<a name="line_192"></a>   local N,TX,TZ,TF,TV,TW,i,j;
<a name="line_193"></a>
<a name="line_194"></a>   require_square_diffeo_E0_inverse();
<a name="line_195"></a>   N := square_diffeo_E0_inverse_order;
<a name="line_196"></a>   TX := square_diffeo_E0_inverse_table;
<a name="line_197"></a>   TF := table();
<a name="line_198"></a>   TV := table();
<a name="line_199"></a>   TW := table();
<a name="line_200"></a>   for i from 0 to N do 
<a name="line_201"></a>    for j from 0 to N do
<a name="line_202"></a>     TZ[i,j] := evalf(z_proj0(TX[i/N,j/N]));
<a name="line_203"></a>     TF[i,j] := this["conformal_error",TX[i/N,j/N]];
<a name="line_204"></a>     TV[i,j] := [op(TZ[i,j]),TF[i,j]];
<a name="line_205"></a>     TW[i,j] := [op(TZ[i,j]),0];
<a name="line_206"></a>    od;
<a name="line_207"></a>   od;
<a name="line_208"></a>
<a name="line_209"></a>   display(
<a name="line_210"></a>    display( 
<a name="line_211"></a>     seq(seq(polygon([TV[i,j],TV[i+1,j],TV[i+1,j+1]]),j=0..N-1),i=0..N-1),
<a name="line_212"></a>     seq(seq(polygon([TV[i,j],TV[i,j+1],TV[i+1,j+1]]),j=0..N-1),i=0..N-1),
<a name="line_213"></a>     style=patchnogrid
<a name="line_214"></a>    ),
<a name="line_215"></a>    display(
<a name="line_216"></a>     seq(seq(polygon([TW[i,j],TW[i+1,j],TW[i+1,j+1]]),j=0..N-1),i=0..N-1),
<a name="line_217"></a>     seq(seq(polygon([TW[i,j],TW[i,j+1],TW[i+1,j+1]]),j=0..N-1),i=0..N-1),
<a name="line_218"></a>     colour=grey,style=wireframe
<a name="line_219"></a>    )
<a name="line_220"></a>   );
<a name="line_221"></a>  end
<a name="line_222"></a> ]
<a name="line_223"></a>);  </pre>
 </body>
</html>
    