<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># Here are two different formulae for the Gaussian curvature of a metric
<a name="line_2"></a># on R^2, in terms of the metric coefficients E, F and G.  
<a name="line_3"></a>
<a name="line_4"></a><span style="color:red">#@ brioschi_formula 
</span><a name="line_5"></a>brioschi_formula := proc(E,F,G,t,u)
<a name="line_6"></a> local E1,E2,E22,F1,F2,F12,G1,G2,G11;
<a name="line_7"></a> E1 := diff(E,t); E2 := diff(E,u); E22 := diff(E2,u);
<a name="line_8"></a> F1 := diff(F,t); F2 := diff(F,u); F12 := diff(F1,u);
<a name="line_9"></a> G1 := diff(G,t); G2 := diff(G,u); G11 := diff(G1,t);
<a name="line_10"></a> (Determinant(<<4*F12 - 2*G11 - 2*E22|E1|2*F1-E2>,
<a name="line_11"></a>              <2*F2-G1|E|F>,<G2|F|G>>) - 
<a name="line_12"></a>  Determinant(<<0|E2|G1>,<E2|E|F>,<G1|F|G>>))/(4*(E*G-F^2)^2);
<a name="line_13"></a>end:
<a name="line_14"></a>
<a name="line_15"></a><span style="color:red">#@ christoffel_symbol 
</span><a name="line_16"></a>christoffel_symbol := proc(E,F,G,t,u)
<a name="line_17"></a> local x,g,gi,C,i,j,k,l,m;
<a name="line_18"></a> 
<a name="line_19"></a> C := table():
<a name="line_20"></a>
<a name="line_21"></a> x := [t,u];
<a name="line_22"></a> g := <<E|F>,<F|G>>;
<a name="line_23"></a> gi := 1/g;
<a name="line_24"></a> for i from 1 to 2 do
<a name="line_25"></a>  for k from 1 to 2 do
<a name="line_26"></a>   for l from 1 to 2 do
<a name="line_27"></a>    C[i,k,l] :=
<a name="line_28"></a>     add(gi[i,m]*(diff(g[m,k],x[l])+diff(g[m,l],x[k])-diff(g[k,l],x[m]))/2,m=1..2);
<a name="line_29"></a>   od;
<a name="line_30"></a>  od;
<a name="line_31"></a> od;
<a name="line_32"></a>
<a name="line_33"></a> return eval(C);
<a name="line_34"></a>end:
<a name="line_35"></a>
<a name="line_36"></a><span style="color:red">#@ christoffel_formula 
</span><a name="line_37"></a>christoffel_formula := proc(E,F,G,t,u)
<a name="line_38"></a> local C;
<a name="line_39"></a> 
<a name="line_40"></a> C := christoffel_symbol(E,F,G,t,u);
<a name="line_41"></a> -(diff(C[2,1,2],t)-diff(C[2,1,1],u)+
<a name="line_42"></a>   C[1,1,2]*C[2,1,1]-C[1,1,1]*C[2,1,2]+C[2,1,2]*C[2,1,2]-C[2,1,1]*C[2,2,2])/E;
<a name="line_43"></a>end:
<a name="line_44"></a>
<a name="line_45"></a># Given a map f : R^2 -> R^d for some d, this calculates the 
<a name="line_46"></a># metric on R^2 obtained by pulling back the standard euclidean
<a name="line_47"></a># metric on R^d.  Here f is given as a vector or list of length d,
<a name="line_48"></a># whose entries are expressions depending on the variables t and u
<a name="line_49"></a># supplied as additional arguments.
<a name="line_50"></a>
<a name="line_51"></a><span style="color:red">#@ metric_from_embedding 
</span><a name="line_52"></a>metric_from_embedding := proc(f,t,u)
<a name="line_53"></a> local d,ft,fu,E,F,G;
<a name="line_54"></a>
<a name="line_55"></a> d := nops(convert(f,list));
<a name="line_56"></a> ft := map(diff,f,t);
<a name="line_57"></a> fu := map(diff,f,u);
<a name="line_58"></a> E := dp(d,ft,ft);
<a name="line_59"></a> F := dp(d,ft,fu);
<a name="line_60"></a> G := dp(d,fu,fu);
<a name="line_61"></a> return([E,F,G]);
<a name="line_62"></a>end:
<a name="line_63"></a>
<a name="line_64"></a># Here are two formulae for the curvature of a metric obtained from
<a name="line_65"></a># a map to R^d as above.
<a name="line_66"></a>
<a name="line_67"></a><span style="color:red">#@ brioschi_from_embedding 
</span><a name="line_68"></a>brioschi_from_embedding := (f,t,u) -> 
<a name="line_69"></a> brioschi_formula(op(metric_from_embedding(f,t,u)),t,u);
<a name="line_70"></a>
<a name="line_71"></a><span style="color:red">#@ christoffel_from_embedding 
</span><a name="line_72"></a>christoffel_from_embedding := (f,t,u) -> 
<a name="line_73"></a> christoffel_formula(op(metric_from_embedding(f,t,u)),t,u);
<a name="line_74"></a>
<a name="line_75"></a># This calculates the laplacian of an expression p depending on t and u,
<a name="line_76"></a># with respect to the metric on R^2 given by E, F and G.
<a name="line_77"></a>
<a name="line_78"></a><span style="color:red">#@ coord_laplacian 
</span><a name="line_79"></a>coord_laplacian := proc(p,E,F,G,t,u)
<a name="line_80"></a> local pt,pu,g;
<a name="line_81"></a> g := E*G - F^2;
<a name="line_82"></a> pt := diff(p,t);
<a name="line_83"></a> pu := diff(p,u);
<a name="line_84"></a> (diff((G*pt-F*pu)/sqrt(g),t) + diff((E*pu-F*pt)/sqrt(g),u))/sqrt(g);
<a name="line_85"></a>end:
<a name="line_86"></a>
<a name="line_87"></a><span style="color:red">#@ laplacian_from_embedding 
</span><a name="line_88"></a>laplacian_from_embedding := proc(p,f,t,u)
<a name="line_89"></a> coord_laplacian(p,op(metric_from_embedding(f,t,u)),t,u);
<a name="line_90"></a>end:
<a name="line_91"></a>
<a name="line_92"></a># This calculates an order N approximation to the geodesic spray for
<a name="line_93"></a># a conformally flat metric on R^2.
<a name="line_94"></a>
<a name="line_95"></a><span style="color:red">#@ geodesic_spray_R2 
</span><a name="line_96"></a>geodesic_spray_R2 := proc(E,t,u,N)
<a name="line_97"></a> local CC,ijk,C,P,Q,PP,QQ,PPe,QQe,PQe,rels,sol,e;
<a name="line_98"></a> CC := christoffel_symbol(E,0,E,t,u):
<a name="line_99"></a> for ijk in indices(CC) do 
<a name="line_100"></a>  C := CC[op(ijk)];
<a name="line_101"></a>  C := subs({t=e*t,u=e*u},C);
<a name="line_102"></a>  C := expand(subs(e=1,convert(series(C,e=0,N+1),polynom,e)));
<a name="line_103"></a>  CC[op(ijk)] := C;
<a name="line_104"></a> od:
<a name="line_105"></a> PP := t + add(add(P[i-j,j] * t^(i-j) * u^j,j=0..i),i=2..N):
<a name="line_106"></a> QQ := u + add(add(Q[i-j,j] * t^(i-j) * u^j,j=0..i),i=2..N):
<a name="line_107"></a> PPe := subs({t=e*t,u=e*u},PP);
<a name="line_108"></a> QQe := subs({t=e*t,u=e*u},QQ);
<a name="line_109"></a> PQe :=[PPe,QQe]:
<a name="line_110"></a> rels := [
<a name="line_111"></a>  diff(PPe,e,e)+add(add(subs({t=PPe,u=QQe},CC[1,j,k])*diff(PQe[j],e)*diff(PQe[k],e),j=1..2),k=1..2),
<a name="line_112"></a>  diff(QQe,e,e)+add(add(subs({t=PPe,u=QQe},CC[2,j,k])*diff(PQe[j],e)*diff(PQe[k],e),j=1..2),k=1..2)
<a name="line_113"></a> ]:
<a name="line_114"></a> rels := map(expand,map(convert,map(series,rels,e=0,N-1),polynom,e));
<a name="line_115"></a> rels := subs(e=1,rels);
<a name="line_116"></a> rels := map(coeffs,rels,{t,u});
<a name="line_117"></a> sol := solve(rels,indets(rels) minus {t,u});
<a name="line_118"></a> return subs(sol,[PP,QQ]);
<a name="line_119"></a>end:
<a name="line_120"></a>
<a name="line_121"></a># This returns a series E in t and u such that E times the standard
<a name="line_122"></a># euclidean metric has curvature -1 mod (t,u)^(N-2), and
<a name="line_123"></a># E[i,j] = E0[i,j] for i < 2 and i + j <= N.  There is a unique
<a name="line_124"></a># series with these properties.
<a name="line_125"></a>
<a name="line_126"></a><span style="color:red">#@ general_hyperbolic_metric 
</span><a name="line_127"></a>general_hyperbolic_metric := proc(E0,t,u,N)
<a name="line_128"></a> local i,j,C,E,K,EE,KK,rels,vars,sol;
<a name="line_129"></a>
<a name="line_130"></a> for i from 0 to N do
<a name="line_131"></a>  for j from 0 to N-i do
<a name="line_132"></a>   C[i,j] := coeff(coeff(E0,t,i),u,j);
<a name="line_133"></a>  od;
<a name="line_134"></a> od;
<a name="line_135"></a>
<a name="line_136"></a> EE := add(add(E[i,j] * t^i * u^j,j=0..N-i),i=0..N):
<a name="line_137"></a> 
<a name="line_138"></a> KK := brioschi_formula(EE,0,EE,t,u):
<a name="line_139"></a> KK := multi_series(KK,N-2,t,u);
<a name="line_140"></a> rels := map(numer,map(factor,{coeffs(expand(KK+1),{t,u})})):
<a name="line_141"></a> vars := [seq(seq(E[i,j],j=0..N-i),i=2..N)];
<a name="line_142"></a> sol := solve(rels,vars)[1]:
<a name="line_143"></a>
<a name="line_144"></a> return eval(subs(E=C,subs(sol,EE)));
<a name="line_145"></a>end:
<a name="line_146"></a>
  </pre>
 </body>
</html>
    