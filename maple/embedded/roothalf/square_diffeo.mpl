# This function is a homeomorphism from F16 to [0,1]^2 given by 
# quadratic polynomials.  However, the inverse is not differentiable 
# along the boundary of [0,1]^2, so it is not a diffeomorphism.

#@ square_homeo 
square_homeo := (x) -> [
 x[3]*(x[3] - x[4]/sqrt(2)),
 x[2]*(x[2] - x[1]) + x[4]*(x[4] - sqrt(2)*x[3])
];

#@ square_homeo_cone_a 
square_homeo_cone_a := (s,t) ->
 [2*cos(t),cos(t+s)+cos(t),sqrt(2)*sin(t),-sin(t+s)+sin(t)];

######################################################################
# This function gives a smooth embedding from a open neighbourhood of
# F16 in EX^* to a neighbourhood of [0,1]^2 in R^2.  It restricts to
# give a diffeomorphism from F16 to [0,1]^2.  The second component is 
# a quadratic polynomial, but the first term has some cubic terms and
# some quadratic terms.

#@ sd_alpha
sd_alpha[0] := (x) -> x[3] - x[4]/sqrt(2) + x[1]^2 + x[4]^2 + x[3] * (x[4] - x[2])/sqrt(2);
sd_alpha[1] := (x) -> ((3/4)*sqrt(2)-1)*x[1]+x[2]-x[3]-sqrt(2)*x[4];
sd_alpha[2] := (x) -> x[1] - 3/4*sqrt(3)*x[3] + (3 - 3/4*sqrt(6))*x[4];

#@ square_diffeo_E0 
square_diffeo_E0 := (x) -> [
 x[3] * sd_alpha[0](x) - x[2]^2 * x[4],
 (x[2]-x[1]) * sd_alpha[1](x) +
 x[4] * sd_alpha[2](x)
];

# Here is a different diffeomorphism, which may actually be better.

#@ square_diffeo_E0_alt
square_diffeo_E0_alt := 
 (x) -> [x[3]/(x[2]+x[3]),
         ((x[1]-x[2])*(1-2*x[1]-2*x[2])-3*sqrt(2)*x[3]*x[4])/(2+2*x[1]-x[2])];

#@ square_diffeo_E0_alt_inv_approx
square_diffeo_E0_alt_inv_approx := (t) -> 
 [-(247/164)*t[2]^2*t[1]^2+(129/455)*t[2]^2*t[1]+(192/83)*t[2]*t[1]^2-(62/921)*t[2]^2-(303/790)*t[1]*t[2]-(113/140)*t[1]^2-(119/186)*t[2]+(349/3489)*t[1]+169/239, -(127/234)*t[2]^2*t[1]^2+(386/411)*t[2]^2*t[1]+(254/315)*t[2]*t[1]^2-(379/956)*t[2]^2-(347/232)*t[1]*t[2]-(113/140)*t[1]^2+(375/544)*t[2]+(349/3489)*t[1]+169/239, (695/2213)*t[2]^2*t[1]^2-(373/1262)*t[2]^2*t[1]-(173/240)*t[2]*t[1]^2+(179/345)*t[1]*t[2]-(154/383)*t[1]^2+(537/383)*t[1], -(1013/1124)*t[2]^2*t[1]^2+(298/299)*t[2]^2*t[1]+(165/112)*t[2]*t[1]^2-(294/137)*t[1]*t[2]];

#@ square_diffeo_E0_alt_inv
square_diffeo_E0_alt_inv := proc(t::RR0_2)
 local x,x0,x1,sol;

 x0 := [x[1],x[2],x[3],x[4]];
 x1 := evalf(square_diffeo_E0_alt_inv_approx(t));
 
 sol := fsolve(
         [op(square_diffeo_E0_alt(x0) -~ t),rho(x0)-1,g0(x0)],
         {x[1] = x1[1],x[2] = x1[2],x[3] = x1[3],x[4] = x1[4]}
        );
 return subs(sol,x0);
end:

######################################################################
# This function finds the preimage under square_diffeo_E0 of a specified point
# in [0,1]^2 by an iterative search.  I am not sure whether this is actually
# better than using fsolve().  I previously had some problems with fsolve(),
# but I think that they may have been caused by a misunderstanding about
# the behaviour of evalf[n](...).

#@ square_diffeo_E0_inverse_search 
square_diffeo_E0_inverse_search := proc(t0::[numeric,numeric])
 local s,s0,tol,n,i,e,u,uu,n0,x0,dx,b0,db,sda,sdj,J0,err;

 # We handle points on the boundary of the square separately, to ensure that
 # we get points that lie exactly on the corresponding loci in EX^*.

 if t0[1] = 0 then
  if t0[2] = 0 then
   return v_E1[6];
  elif t0[2] = 1 then
   return v_E1[3];
  else 
   s0 := fsolve(square_diffeo_E0(c_E0[0](s))[2] = t0[2],s=evalf(Pi/4*(1+t0[2])));
   return evalf(c_E0[0](s0));
  fi;
 elif t0[1] = 1 then
  if t0[2] = 0 then
   return v_E1[0];
  elif t0[2] = 1 then
   return v_E1[11];
  else 
   s0 := fsolve(square_diffeo_E0(c_E0[5](s))[2] = t0[2],s=evalf(Pi*t0[2]));
   return evalf(c_E0[5](s0));
  fi;
 else 
  if t0[2] = 0 then
   s0 := fsolve(square_diffeo_E0(c_E0[1](s))[1] = t0[1],s=evalf(Pi/2*(1-t0[1])));
   return evalf(c_E0[1](s0));
  elif t0[2] = 1 then
   s0 := fsolve(square_diffeo_E0(c_E0[3](s))[1] = t0[1],s=evalf(Pi/2*(1-t0[1])));
   return evalf(c_E0[3](s0));
  fi;
 fi;

 # The rest of this function handles the general case, where t0 is
 # not on the boundary of the square.

 x0 :=
   [-2.05850*t0[1]^2*t0[2]^2+0.918130*t0[1]*t0[2]^2+2.66136*t0[1]^2*t0[2]-0.115622*t0[2]^2-
    0.813875*t0[1]*t0[2]-0.602855*t0[1]^2-0.591483*t0[2]-0.104250*t0[1]+0.707105,
    0.129961*t0[1]^2*t0[2]^2+0.254890*t0[1]*t0[2]^2-0.479913*t0[1]^2*t0[2]-0.384851*t0[2]^2-
    0.197833*t0[1]*t0[2]-0.602855*t0[1]^2+0.677745*t0[2]-0.104250*t0[1]+0.707105,
    -0.492388*t0[1]^2*t0[2]^2+0.473871*t0[1]*t0[2]^2+0.818861*t0[1]^2*t0[2]-
    0.983847*t0[1]*t0[2]-0.825307*t0[1]^2+1.82530*t0[1],
    -1.95651*t0[1]^2*t0[2]^2+2.15931*t0[1]*t0[2]^2+2.30924*t0[1]^2*t0[2]-3.08939*t0[1]*t0[2]];

 sda := unapply(evalf(expand([op(square_diffeo_E0(xx)),rho(xx) - 1,g0(xx)])),x):
 sdj := unapply([seq(map(diff,sda(x),x[i]),i=1..4)],x):

 x0 := Vector(x0);
 b0 := Vector([op(t0),0,0]);
 db := b0 - Vector(sda(x0));
 n := 100;
 tol := 10.^(-95);

 err := Norm(db);

 while err > tol and n > 0 do 
  n := n-1;
  J0 := Transpose(Matrix(sdj(x0)));
  dx := LinearSolve(J0,db);
  x0 := x0 + dx;
  db := b0 - Vector(sda(x0));
  err := Norm(db);
 od:

 return convert(x0,list);
end:

######################################################################
# This function tabulates some values of the inverse of square_diffeo_E0(),
# and computes a Chebyshev approximation to that inverse.

#@ find_square_diffeo_E0_inverse 
#@ square_diffeo_E0_inverse_order
#@ square_diffeo_E0_inverse
#@ square_diffeo_E0_inverse_table
#@ square_diffeo_E0_inverse_chebyshev_table

find_square_diffeo_E0_inverse := proc(NN::posint)
 global square_diffeo_E0_inverse_order,square_diffeo_E0_inverse,
        square_diffeo_E0_inverse_table,
        square_diffeo_E0_inverse_chebyshev_table;
 local CP,SDI,i,j,k,l,m,x0,sol,aa;

 square_diffeo_E0_inverse_order := NN;

 # CP is a table of NN+1 Chebyshev points for the interval [0,1], indexed by 
 # 0 to NN.  Recall that all these points are actually in (0,1).
 CP := table():
 for i from 0 to NN do
  CP[i] := evalf(1/2 + cos((2*NN-2*i+1)*Pi/(2*NN+2))/2):
 od:

 # It turns out to be convenient to add in the end points.  These are not used
 # in the Chebyshev approximation formula, but they are used to provide a 
 # starting point when searching for values of the inverse function.
 CP[-1] := 0:
 CP[NN+1] := 1:

 SDI := table():
 SDI[  -1,  -1] := evalf(v_E0[ 6]):
 SDI[NN+1,  -1] := evalf(v_E0[ 0]):
 SDI[  -1,NN+1] := evalf(v_E0[ 3]):
 SDI[NN+1,NN+1] := evalf(v_E0[11]):

 userinfo(5,genus2,"Finding edge Chebyshev points");

 for i from 0 to NN do
  SDI[i,-1] := 
   evalf(c_E0[1](fsolve(square_diffeo_E0(c_E0[1](t))[1] - CP[i],t=0..Pi/2)));

  SDI[i,NN+1] := 
   evalf(c_E0[3](fsolve(square_diffeo_E0(c_E0[3](t))[1] - CP[i],t=0..Pi/2)));

  SDI[-1,i] := 
   evalf(c_E0[0](fsolve(square_diffeo_E0(c_E0[0](t))[2] - CP[i],t=Pi/4..Pi/2)));

  SDI[NN+1,i] :=
   evalf(c_E0[5](fsolve(square_diffeo_E0(c_E0[5](t))[2] - CP[i],t=0..Pi)));
 od:

 userinfo(5,genus2,"Finding interior Chebyshev points");

 for i from 0 to NN do
  for j from 0 to NN do
#   x0 := SDI[i-1,j];
#   sol := fsolve({op(square_diffeo_E0(x) -~ [CP[i],CP[j]]),rho(x)-1,g0(x)},{seq(x[i]=x0[i],i=1..4)});
#   SDI[i,j] := subs(sol,xx);
   SDI[i,j] := square_diffeo_E0_inverse_search([CP[i],CP[j]]);
  od:
 od:

 userinfo(5,genus2,"Finding Chebyshev approximation");

 aa := table():
 for i from 0 to NN do
  for j from 0 to NN do
   for m from 1 to 4 do
    aa[i,j,m] := add(add(SDI[k,l][m]*ChebyshevT(i,2*CP[k]-1)*ChebyshevT(j,2*CP[l]-1)*4/(NN+1)^2,k=0..NN),l=0..NN);
    if i = 0 then aa[i,j,m] := aa[i,j,m]/2; fi;
    if j = 0 then aa[i,j,m] := aa[i,j,m]/2; fi;
   od:
  od:
 od:

 square_diffeo_E0_inverse :=
  unapply(expand([seq(add(add(aa[i,j,m]*ChebyshevT(i,2*t[1]-1)*ChebyshevT(j,2*t[2]-1),j=0..NN),i=0..NN),m=1..4)]),t):

 square_diffeo_E0_inverse_chebyshev_table := eval(SDI);

 userinfo(5,genus2,"Finding interior grid points");

 square_diffeo_E0_inverse_table := table();

 for i from 0 to NN do
  for j from 0 to NN do
   square_diffeo_E0_inverse_table[i/NN,j/NN] :=  
    square_diffeo_E0_inverse_search([i/NN,j/NN],square_diffeo_E0_inverse([i/NN,j/NN]));
  od:
 od:

 NULL;
end:

##################################################

#@ save_square_diffeo_E0_inverse 
save_square_diffeo_E0_inverse := proc()
 save(square_diffeo_E0_inverse,
      square_diffeo_E0_inverse_order,
      square_diffeo_E0_inverse_table,
      square_diffeo_E0_inverse_chebyshev_table,
      cat(data_dir,"/embedded/roothalf/square_diffeo_E0_inverse.m"));
end:

#@ load_square_diffeo_E0_inverse 
load_square_diffeo_E0_inverse := proc()
 read(cat(data_dir,"/embedded/roothalf/square_diffeo_E0_inverse.m"));
end:

#@ require_square_diffeo_E0_inverse 
require_square_diffeo_E0_inverse := proc()
 if not type(square_diffeo_E0_inverse_order,integer) then
  load_square_diffeo_E0_inverse();
 fi;
end:

######################################################################

#@ make_square_diffeo_E0_inverse_plot 
make_square_diffeo_E0_inverse_plot := proc()
 global pics;

 local T,N,i,j;

 T := square_diffeo_E0_inverse_table:
 N := square_diffeo_E0_inverse_order:
 
 pics["square_diffeo_E0_inverse"] :=
  display(seq(seq(op([
   polygon([stereo(T[i/N,j/N]),stereo(T[(i+1)/N,j/N]),stereo(T[(i+1)/N,(j+1)/N])],style=patchnogrid),
   polygon([stereo(T[i/N,j/N]),stereo(T[i/N,(j+1)/N]),stereo(T[(i+1)/N,(j+1)/N])],style=patchnogrid)]),
   j=0..N-1),i=0..N-1),
   axes=none,scaling=constrained);

 save_plot("square_diffeo_E0_inverse");
 save_jpg("square_diffeo_E0_inverse");

 pics["square_diffeo_E0_inverse"];
end:

######################################################################
# This is the most general function R^2 -> R^4 with the following
# properties:
#
# 0) Each entry is polynomial with degree <= 2 in each of t[1] and t[2]
# 1) The corners of [0,1]^2 map to v[0], v[3], v[6] and v[11] in the 
#     usual order.
# 2) The edges are mapped in accordance with the patterns
#     [0,t] |-> [x[1], x[2], 0, 0] 
#     [1,t] |-> [x[1], 0, x[3], x[4]]
#     [t,0] |-> [x[1], x[1], x[2], 0]
#     [t,1] |-> [0, x[2], x[3], -x[3]/sqrt(2)].
#
# The point is that the inverse of square_diffeo_E0 also has
# properties (1) and (2).

eqs := map(coeffs,expand([sdi([0,t[2]])[3],sdi([0,t[2]])[4],sdi([1,t[2]])[2],sdi([t[1],0])[2]-sdi([t[1],0])[1],sdi([t[1],0])[4],sdi([t[1],1])[1],sdi([t[1],1])[3]+sqrt(2)*sdi([t[1],1])[4]]),[t[1],t[2]]);

#@ square_diffeo_E0_inverse_pattern 
square_diffeo_E0_inverse_pattern := (A,t) -> [
 A[9]*t[1]^2*t[2]^2+A[6]*t[1]*t[2]^2-t[1]^2*t[2]*A[9]-t[1]^2*t[2]*A[16]+A[3]*t[2]^2+
  t[1]*t[2]*A[16]+(1/2)*t[1]*t[2]*sqrt(2)-t[1]*t[2]*A[6]+A[16]*t[1]^2-
  (1/2)*sqrt(2)*t[2]-t[2]*A[3]-t[1]*A[16]-(1/2)*sqrt(2)*t[1]+(1/2)*sqrt(2),
 A[18]*t[1]^2*t[2]^2+A[15]*t[1]*t[2]^2+A[17]*t[1]^2*t[2]-t[2]^2*A[15]-t[2]^2*A[18]-
 t[1]*t[2]*A[15]-t[1]*t[2]*A[18]-t[1]*t[2]-t[1]*t[2]*A[17]+(1/2)*t[1]*t[2]*sqrt(2)+
 A[16]*t[1]^2+t[2]*A[15]+t[2]*A[18]+t[2]-(1/2)*sqrt(2)*t[2]-t[1]*A[16]-
 (1/2)*sqrt(2)*t[1]+(1/2)*sqrt(2),
 A[27]*t[1]^2*t[2]^2+A[24]*t[1]*t[2]^2+A[26]*t[1]^2*t[2]-t[1]*t[2]*A[26]-
  t[1]*t[2]*A[27]-t[1]*t[2]*A[24]+(1/3)*sqrt(6)*t[1]*t[2]-t[1]*t[2]-A[26]*t[1]^2-
  sqrt(2)*A[35]*t[1]^2-sqrt(2)*A[36]*t[1]^2-A[27]*t[1]^2+sqrt(2)*t[1]*A[36]+
  sqrt(2)*t[1]*A[35]+t[1]*A[26]+t[1]*A[27]+t[1],
 A[36]*t[1]^2*t[2]^2+A[33]*t[1]*t[2]^2+A[35]*t[1]^2*t[2]-t[1]*t[2]*A[36]-
  t[1]*t[2]*A[33]-t[1]*t[2]*A[35]-(1/3)*sqrt(3)*t[1]*t[2]
 ]:

######################################################################

#@ make_square_diffeo_E0_record 
make_square_diffeo_E0_record := proc(x0)
 local u,e,n0,u0,v0,t0,tu0,tv0,J0,eqs,sol,x1,P;
 n0 := evalf(dg0(x0));
 n0 := n0 /~ nm4(n0);
 t0 := evalf(square_diffeo_E0(x0));
 u0 := [u[1],u[2],u[3],u[4]];
 x1 := x0 +~ e *~ u0;
 eqs := expand(evalf([rho(x1),g0(x1),op(square_diffeo_E0(x1))]));
 eqs := map(coeff,eqs,e,1) -~ [0,0,1,0];
 sol := solve(eqs);
 u0 := evalf(subs(sol,u0));
 u0 := u0 /~ nm4(u0);
 v0 := cross_product4(x0,n0,u0);
 tu0 := evalf(map(coeff,expand(square_diffeo_E0(x0 +~ e *~ u0)),e,1));
 tv0 := evalf(map(coeff,expand(square_diffeo_E0(x0 +~ e *~ v0)),e,1));
 J0 := Determinant(Matrix([tu0,tv0]));
 P := table();
 P["x"] := x0;
 P["n"] := n0;
 P["u"] := u0;
 P["v"] := v0;
 P["t"] := t0;
 P["tu"] := tu0;
 P["tv"] := tv0;
 P["J"] := J0;
 P["K"] := evalf(curvature0(x0));
 P["M"] := evalf(rescale_x(x0));
 return(eval(P));
end:

######################################################################

#@ square_diffeo_E0_JM 
square_diffeo_E0_JM := 
 unapply(Matrix([xx,dg0(xx),seq([seq(diff(square_diffeo_E0(xx)[i],x[j]),j=1..4)],i=1..2)]),x);

#@ square_diffeo_E0_J 
square_diffeo_E0_J := unapply(simplify(NF_x0(Determinant(square_diffeo_E0_JM(x)))/nm4(dg0(xx))),x);

#@ square_diffeo_E0_alt_JM 
square_diffeo_E0_alt_JM := 
 unapply(Matrix([xx,dg0(xx),seq([seq(diff(square_diffeo_E0_alt(xx)[i],x[j]),j=1..4)],i=1..2)]),x);

#@ square_diffeo_E0_alt_J 
square_diffeo_E0_alt_J := unapply(simplify((Determinant(square_diffeo_E0_alt_JM(x)))/nm4(dg0(xx))),x);

######################################################################

#@ sd_plot_base 
sd_plot_base := display(
 polygon([[0,0,0],[1,0,0],[1,1,0],[0,1,0]],colour=grey),
 line([0,0,0],[0,1,0],colour=c_colour[0]),
 line([0,0,0],[1,0,0],colour=c_colour[1]),
 line([0,1,0],[1,1,0],colour=c_colour[3]),
 line([1,0,0],[1,1,0],colour=c_colour[5]),
 axes=none,scaling=constrained
):

#@ sd_plot 
sd_plot := proc(f)
 local i,j,SDI,N,P;

 require_square_diffeo_E0_inverse():

 SDI := eval(square_diffeo_E0_inverse_table);
 N := square_diffeo_E0_inverse_order;
 P := table():
 for i from 0 to N do
  for j from 0 to N do
   P[i,j] := [i/N,j/N,evalf(eval(subs(x = SDI[i/N,j/N],f)))];
  od;
 od;

 display(
  seq(seq(polygon([P[i,j],P[i+1,j],P[i+1,j+1]],style=patchnogrid),i=0..N-1),j=0..N-1),
  seq(seq(polygon([P[i,j],P[i,j+1],P[i+1,j+1]],style=patchnogrid),i=0..N-1),j=0..N-1)
 );

end:
