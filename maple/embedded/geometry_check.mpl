check_stereo := proc()
 local u,x,uu,xx;

 printf("%a()\n",procname);

 xx := [x[1],x[2],x[3],x[4]];
 uu := [u[1],u[2],u[3]];

 _ASSERT(simplify(rho(unstereo(uu))-1)=0,"|unstereo(u)|=1");

 _ASSERT(
  factor(
   unstereo(stereo(xx)) -~ xx +~ (
   (rho(xx)-1)/(rho(xx)+1-2*x[4]) *~ (xx -~ [0,0,0,1])
  )) = [0,0,0,0],
  "unstereo o stereo = id"
 );

 _ASSERT(
  factor(stereo(unstereo(uu)) -~ uu) = [0,0,0],
  "stereo o unstereo = id"
 );
end:

add_check(check_stereo):

######################################################################

check_omega := proc() 
 local u;

 printf("%a()\n",procname);

 _ASSERT(simplify(f_1(omega[1](t)))=0,"f_1(omega[1](t))=0");
 _ASSERT(simplify(f_2(omega[2](t)))=0,"f_2(omega[2](t))=0");
 _ASSERT(
  solve({seq(omega[1](s1)[i]=omega[2](s2)[i],i=1..3)},{s1,s2})={s1=0,s2=0},
 "Omega[1] intersect Omega[2]"
 );

 u := simplify(stereo(omega[1](t)));
 _ASSERT(u[2]=0 and u[3]=-a_E,"stereo(Omega[1]+)");
 u := simplify(stereo(omega[2](t)));
 _ASSERT(u[1]=0 and u[3]= a_E,"stereo(Omega[2]+)");

 u := simplify(stereo(-omega[1](t)));
 _ASSERT(u[2]=0 and simplify(u[1]^2+(u[3]-1/(2*a_E))^2-1/(4*a_E^2))=0,"stereo(Omega[1]-)");
 u := simplify(stereo(-omega[2](t)));
 _ASSERT(u[1]=0 and simplify(u[2]^2+(u[3]+1/(2*a_E))^2-1/(4*a_E^2))=0,"stereo(Omega[2]-)");

end:

add_check(check_omega):

######################################################################

check_g := proc()
 local x,xx,u,uu,i,P;

 printf("%a()\n",procname);

 uu := [u[1],u[2],u[3]];
 xx := [x[1],x[2],x[3],x[4]];

 _ASSERT(expand(g_0(xx) - ((f(xx)-f(-xx))/8 + x[4]*(rho(xx) - 1)))=0,"g_0(x) formula");
 _ASSERT(expand(g(xx)  - ((f(xx)-f(-xx))/8 - x[4]*(rho(xx) - 1)))=0,"g(x) formula");
 _ASSERT(expand(g_0(xx) - (-2*x[4]-1/a_E*x[3]*g_1(xx))) = 0, "g_1(x) formula");

 for i from 1 to 4 do
  _ASSERT(expand(dg(xx)[i] - diff(g(xx),x[i]))=0,"dg(x)");
 od:

 _ASSERT(simplify(square_norm_of_dg(xx) - nm4(dg(xx))^2) = 0,
         "square norm of dg formula");

 _ASSERT(
  factor(g_stereo(uu) - ((nm3(uu)^2+1)^3 * g(unstereo(uu)))) = 0,
  "g_stereo"
 );
 
 _ASSERT( 
  expand((1-a_E^2)/16*(dg(xx)[1]^2+dg(xx)[2]^2)+
         a_E/2*(x[1]^2-x[2]^2)*dg(xx)[3]-(x[1]^2+x[2]^2)/4*dg(xx)[4] - (
         x[1]^4+x[2]^4+(5/2-a_E^2)*(x[1]^2+x[2]^2)*x[4]^2)
        ) = 0,
  "smoothness identity"
 );

end:

add_check(check_g):

######################################################################

check_quadratic_chart := proc()
 local x,u,xx,uu,e,yy,err;

 printf("%a()\n",procname);

 xx := [x[1],x[2],x[3],x[4]];
 uu := [u[1],u[2],u[3],u[4]];

 yy := quadratic_chart(xx,e *~ uu);
 err := rho(yy) - 1;
 err := err - (rho(xx)-1)
            - 2*dp4(xx,uu)*e 
            + 6*dp4(xx,dg(uu)) * g(xx)*e^2/square_norm_of_dg(xx) 
            + (rho(xx)-1)*rho(uu)*e^2;
 err := factor(expand(convert(series(err,e=0,3),polynom,e)));
 _ASSERT(err=0,"rho on quadratic_chart");

 err := g(yy);
 err := err - g(xx)
            - e*dp4(uu,dg(xx)) 
            + e^2*(3/2)*dp4(uu,uu)*g(xx);
 err := factor(expand(convert(series(err,e=0,3),polynom,e)));
 _ASSERT(err=0,"g on quadratic_chart");
end:

add_check(check_quadratic_chart):

######################################################################

check_laplacian_a := proc()
 local i,j,k,gg,n0x,m0x,r0x,r1x,r2x;

 printf("%a()\n",procname);

 for i from 1 to 4 do 
  for j from 1 to 4 do 
   for k from 1 to 4 do 
    gg[i,j,k] := diff(g0(x),x[i],x[j],x[k])/6;
   od:
  od:
 od:

 n0x := expand([seq(diff(g0(x),x[i]),i=1..4)]);
 m0x := [seq([seq(diff(g0(x),x[i],x[j]),j=1..4)],i=1..4)];
 
 _ASSERT(
  n0x -~ [seq(add(add(3*gg[p,j,k]*x[j]*x[k],j=1..4),k=1..4),p=1..4)] = [0$4],
  "Formula for n(x) in terms of g_{ijk}"
 );

 _ASSERT(
  m0x -~ [seq([seq(6*add(gg[p,q,k]*x[k],k=1..4),q=1..4)],p=1..4)] = [[0$4]$4],
  "Formula for m(x) in terms of g_{ijk}"
 );

 # These are called r, r' and r'' in the LaTeX document
 r0x := sqrt(expand(dp4(n0x,n0x)));
 r1x := expand(add(m0x[i][i],i=1..4));
 r2x := expand(add(add(n0x[i]*m0x[i][j]*n0x[j],j=1..4),i=1..4));

 
 _ASSERT(
  expand(r0x^2 - 9*add(add(add(add(add(
                    gg[i,j,m]*gg[k,l,m]*x[i]*x[j]*x[k]*x[l],
		     i=1..4),j=1..4),k=1..4),l=1..4),m=1..4)
  ) = 0,
  "Formula for r^2 in terms of g_{ijk}"
 );
 
 _ASSERT(
  expand(r1x - 6*add(add(gg[i,j,j]*x[i],i=1..4),j=1..4)) = 0,
  "Formula for r' in terms of g_{ijk}"
 );

 _ASSERT(
  expand(r2x - 54*add(add(add(add(add(add(add(
                   gg[i,j,k]*gg[k,l,m]*gg[m,n,p]*x[i]*x[j]*x[l]*x[n]*x[p],
	            i=1..4),j=1..4),k=1..4),l=1..4),m=1..4),n=1..4),p=1..4)) = 0,
  "Formula for r'' in terms of g_{ijk}"
 );
end:

add_check(check_laplacian_a):

######################################################################

check_laplacian_b := proc()
 local x0,u0,v0,E0,F0,G0,M0,D0,DI0,MI0,i1,i2,i3,i4,f0,p0,L0,L1,err,ok;

 printf("%a()\n",procname);

 x0 := y_lift([s[1],s[2]]):
 u0 := simplify(map(diff,x0,s[1])):
 v0 := simplify(map(diff,x0,s[2])):
 E0 := factor(expand(rationalize(simplify(dp4(u0,u0))))):
 F0 := factor(expand(rationalize(simplify(dp4(u0,v0))))):
 G0 := factor(expand(rationalize(simplify(dp4(v0,v0))))):
 M0 := <<E0|F0>,<F0|G0>>:
 D0 := factor(expand(rationalize(Determinant(M0)))):
 DI0 := factor(expand(rationalize(1/D0))):
 MI0 := factor(expand(map(rationalize,DI0 * <<G0|-F0>,<-F0|E0>>))):

 ok := true;

 for i1 from 0 to 2 do
  for i2 from 0 to 2 do
   for i3 from 0 to 2 do
    for i4 from 0 to 2 do
     if ok then
      f0 := unapply(t[1]^i1 * t[2]^i2 * t[3]^i3 * t[4]^i4,t);
      L0 := subs({seq(x[i]=x0[i],i=1..4)},laplacian(f0(x)));
      p0 := factor(expand(simplify(sqrt(D0) * MI0 . <diff(f0(x0),s[1]),diff(f0(x0),s[2])>))):
      L1 := simplify((diff(p0[1],s[1]) + diff(p0[2],s[2]))/sqrt(D0)):
      err := simplify(L0 - L1);
      if err <> 0 then
       ok := false;
      fi;
     fi;
    od;
   od;
  od;
 od;

 _ASSERT(ok = true,"laplacian on monomials");

end:

add_check(check_laplacian_b):

######################################################################

check_laplacian_z := proc()
 local i,j,mz,mx,Lmz,Lmx,err;

 printf("%a()\n",procname);

 for i from 0 to 3 do
  for j from 0 to 3 do 
   mz := z[1]^i*z[2]^j:
   userinfo(5,genus2,sprintf("Checking laplacian of %A",mz));
   mx := NF_x(eval(subs(z = zx,mz))):
   Lmz := simplify(laplacian_z(mz)):
   Lmx := simplify(laplacian(mx)):
   Lmx := simplify(NF_z(numer(Lmx))/NF_z(denom(Lmx))):
   err := simplify(Lmz - Lmx);
   _ASSERT(err = 0,sprintf("laplacian = laplacian_z on %A",mz));
  od:
 od:
end:

add_check(check_laplacian_z):

######################################################################

check_gauss_map := proc()
 local i,j,x0,n0,u0,v0,a0,b0,c0,errs,rels,sols,wl;

 printf("%a()\n",procname);

_ASSERT(
  NF_x(numer(factor(nm3(gauss_map(xx))^2 - 1))) = 0,
  "gauss_map lands in S^2"
 );

 _ASSERT(
  {seq(simplify(gauss_map(act_R4[T](xx)) - act_gauss[T](gauss_map(xx))),
       T in [1,LL,LM,LLLM,LN,LLLN,MN,LLMN])} = {[0$3]},
  "equivariance of gauss_map"
 );

 x0 := v_E[6];
 n0 := dg(x0);
 u0 := [1,-1,0,0] /~ sqrt(2);
 v0 := [0,0,1,0];
 a0 := gauss_map(x0);
 b0 := simplify(subs(e = 0,map(diff,gauss_map(x0 +~ e *~ u0),e)));
 c0 := simplify(subs(e = 0,map(diff,gauss_map(x0 +~ e *~ v0),e)));

 errs := 
 {dp4(u0,x0),dp4(u0,n0),dp4(v0,x0),dp4(v0,n0),dp4(u0,v0),
  Determinant(Matrix([x0,n0,u0,v0])) - 2,
  Determinant(Matrix([a0,b0,c0])) - (1/a_E^2 - 1) 
 };

 errs := map(NF_x,map(numer,map(factor,errs)));

 _ASSERT(errs = {0},"gauss_map reverses orientation at v[6]");
 
 # We now check that the preimage of gauss_map(v_E[6]) consists only
 # of v_E[6].  The preimage is defined by the relations in rels[0].
 rels[0] := simplify([rho(xx)-1,g(xx),op(gauss_map(xx) -~ gauss_map(v_E[6]))]):

 # These imply the relations in rels[1] and rels[2]
 rels[1] := [rels[0][1],
             a_E^2*rels[0][2],
	     numer(factor(rels[0][3]-rels[0][4])),
	     numer(factor(rels[0][5]))];
 rels[2] := Basis(rels[1],vars_x);

 # We now check that the relation in rels[2] imply
 # a_E^3*x[4]*((x[1]+x[2])^2/2+x[4]^2) = 0.
 
 _ASSERT(
  NormalForm(a_E^3*x[4]*((x[1]+x[2])^2/2+x[4]^2),rels[2],vars_x) = 0,
  "gauss map preimage a"
 );

 # The above relation together with positivity arguments gives x[4] = 0,
 # so we add that to our list of relations.
 
 rels[3] := Basis([x[4],op(rels[2])],vars_x);

 # We now check that the relation in rels[3] imply
 # a_E^3*(x[1]-x[2])*(a_E+(x[1]+x[2])^2/2) = 0.

 _ASSERT(
  NormalForm(a_E^3*(x[1]-x[2])*(a_E+(x[1]+x[2])^2/2),rels[3],vars_x) = 0,
  "gauss map preimage b"
 );

 # The above relation together with positivity arguments gives x[1]-x[2] = 0,
 # so we add that to our list of relations.
 
 rels[4] := Basis([x[1]-x[2],op(rels[3])],vars_x);

 # We now check that the relation in rels[3] imply
 # (1-a_E)*(2*a_E + (1-a_E)*x[3]^2)*x[3] = 0

 _ASSERT(
  NormalForm((1-a_E)*(2*a_E + (1-a_E)*x[3]^2)*x[3],rels[4],vars_x) = 0,
  "gauss map preimage c"
 );
  
 # The above relation together with positivity arguments gives x[3] = 0,
 # so we add that to our list of relations.
 
 rels[5] := Basis([x[3],op(rels[4])],vars_x);

 wl := interface(warnlevel = 0);
 sols := solve([op(rels[5]),op(rels[0])],xx);
 interface(warnlevel = wl);
 
 sols := map(subs,sols,xx);

 _ASSERT(sols = [v_E[6]],"gauss map preimage d");
end:

add_check(check_gauss_map):

