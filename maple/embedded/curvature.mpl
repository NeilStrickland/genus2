# Here are two different formulae for the Gaussian curvature of a metric
# on R^2, in terms of the metric coefficients E, F and G.  

#@ brioschi_formula 
brioschi_formula := proc(E,F,G,t,u)
 local E1,E2,E22,F1,F2,F12,G1,G2,G11;
 E1 := diff(E,t); E2 := diff(E,u); E22 := diff(E2,u);
 F1 := diff(F,t); F2 := diff(F,u); F12 := diff(F1,u);
 G1 := diff(G,t); G2 := diff(G,u); G11 := diff(G1,t);
 (Determinant(<<4*F12 - 2*G11 - 2*E22|E1|2*F1-E2>,
              <2*F2-G1|E|F>,<G2|F|G>>) - 
  Determinant(<<0|E2|G1>,<E2|E|F>,<G1|F|G>>))/(4*(E*G-F^2)^2);
end:

#@ christoffel_symbol 
christoffel_symbol := proc(E,F,G,t,u)
 local x,g,gi,C,i,j,k,l,m;
 
 C := table():

 x := [t,u];
 g := <<E|F>,<F|G>>;
 gi := 1/g;
 for i from 1 to 2 do
  for k from 1 to 2 do
   for l from 1 to 2 do
    C[i,k,l] :=
     add(gi[i,m]*(diff(g[m,k],x[l])+diff(g[m,l],x[k])-diff(g[k,l],x[m]))/2,m=1..2);
   od;
  od;
 od;

 return eval(C);
end:

#@ christoffel_formula 
christoffel_formula := proc(E,F,G,t,u)
 local C;
 
 C := christoffel_symbol(E,F,G,t,u);
 -(diff(C[2,1,2],t)-diff(C[2,1,1],u)+
   C[1,1,2]*C[2,1,1]-C[1,1,1]*C[2,1,2]+C[2,1,2]*C[2,1,2]-C[2,1,1]*C[2,2,2])/E;
end:

# Given a map f : R^2 -> R^d for some d, this calculates the 
# metric on R^2 obtained by pulling back the standard euclidean
# metric on R^d.  Here f is given as a vector or list of length d,
# whose entries are expressions depending on the variables t and u
# supplied as additional arguments.

#@ metric_from_embedding 
metric_from_embedding := proc(f,t,u)
 local d,ft,fu,E,F,G;

 d := nops(convert(f,list));
 ft := map(diff,f,t);
 fu := map(diff,f,u);
 E := dp(d,ft,ft);
 F := dp(d,ft,fu);
 G := dp(d,fu,fu);
 return([E,F,G]);
end:

# Here are two formulae for the curvature of a metric obtained from
# a map to R^d as above.

#@ brioschi_from_embedding 
brioschi_from_embedding := (f,t,u) -> 
 brioschi_formula(op(metric_from_embedding(f,t,u)),t,u);

#@ christoffel_from_embedding 
christoffel_from_embedding := (f,t,u) -> 
 christoffel_formula(op(metric_from_embedding(f,t,u)),t,u);

# This calculates the laplacian of an expression p depending on t and u,
# with respect to the metric on R^2 given by E, F and G.

#@ coord_laplacian 
coord_laplacian := proc(p,E,F,G,t,u)
 local pt,pu,g;
 g := E*G - F^2;
 pt := diff(p,t);
 pu := diff(p,u);
 (diff((G*pt-F*pu)/sqrt(g),t) + diff((E*pu-F*pt)/sqrt(g),u))/sqrt(g);
end:

#@ laplacian_from_embedding 
laplacian_from_embedding := proc(p,f,t,u)
 coord_laplacian(p,op(metric_from_embedding(f,t,u)),t,u);
end:

# This calculates an order N approximation to the geodesic spray for
# a conformally flat metric on R^2.

#@ geodesic_spray_R2 
geodesic_spray_R2 := proc(E,t,u,N)
 local CC,ijk,C,P,Q,PP,QQ,PPe,QQe,PQe,rels,sol,e;
 CC := christoffel_symbol(E,0,E,t,u):
 for ijk in indices(CC) do 
  C := CC[op(ijk)];
  C := subs({t=e*t,u=e*u},C);
  C := expand(subs(e=1,convert(series(C,e=0,N+1),polynom,e)));
  CC[op(ijk)] := C;
 od:
 PP := t + add(add(P[i-j,j] * t^(i-j) * u^j,j=0..i),i=2..N):
 QQ := u + add(add(Q[i-j,j] * t^(i-j) * u^j,j=0..i),i=2..N):
 PPe := subs({t=e*t,u=e*u},PP);
 QQe := subs({t=e*t,u=e*u},QQ);
 PQe :=[PPe,QQe]:
 rels := [
  diff(PPe,e,e)+add(add(subs({t=PPe,u=QQe},CC[1,j,k])*diff(PQe[j],e)*diff(PQe[k],e),j=1..2),k=1..2),
  diff(QQe,e,e)+add(add(subs({t=PPe,u=QQe},CC[2,j,k])*diff(PQe[j],e)*diff(PQe[k],e),j=1..2),k=1..2)
 ]:
 rels := map(expand,map(convert,map(series,rels,e=0,N-1),polynom,e));
 rels := subs(e=1,rels);
 rels := map(coeffs,rels,{t,u});
 sol := solve(rels,indets(rels) minus {t,u});
 return subs(sol,[PP,QQ]);
end:

# This returns a series E in t and u such that E times the standard
# euclidean metric has curvature -1 mod (t,u)^(N-2), and
# E[i,j] = E0[i,j] for i < 2 and i + j <= N.  There is a unique
# series with these properties.

#@ general_hyperbolic_metric 
general_hyperbolic_metric := proc(E0,t,u,N)
 local i,j,C,E,K,EE,KK,rels,vars,sol;

 for i from 0 to N do
  for j from 0 to N-i do
   C[i,j] := coeff(coeff(E0,t,i),u,j);
  od;
 od;

 EE := add(add(E[i,j] * t^i * u^j,j=0..N-i),i=0..N):
 
 KK := brioschi_formula(EE,0,EE,t,u):
 KK := multi_series(KK,N-2,t,u);
 rels := map(numer,map(factor,{coeffs(expand(KK+1),{t,u})})):
 vars := [seq(seq(E[i,j],j=0..N-i),i=2..N)];
 sol := solve(rels,vars)[1]:

 return eval(subs(E=C,subs(sol,EE)));
end:

