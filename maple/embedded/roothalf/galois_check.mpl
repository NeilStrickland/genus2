check_galois := proc()
 local i,j,chi,xi,ok,m0,m1,T;

 printf("%a()\n",procname);

 for i from 1 to 16 do
  _ASSERT(FNF_y0(eval(subs(t=tx,st[i])) - sx[i]) = 0,
          sprintf("s[%d] in terms of x,y,r",i));
 od:

 _ASSERT(
 {seq(seq(
   FNF_y0(act_AT[T](t_xyz_reduce(t[i])) - t_xyz_reduce(act_AT[T](t[i]))),
  T in {L,M,N,A1,A2}),i=1..4)} = {0},
  "Action of G64 on t[i] vs tx[i]"
 );

 _ASSERT(
 {seq(seq(
   FNF_y0(act_AT[T](t_xyz_reduce(s[i])) - t_xyz_reduce(act_AT[T](s[i]))),
  T in {L,M,N,A1,A2}),i=1..16)} = {0},
  "Action of G64 on s[i] vs sx[i]"
 );

 _ASSERT(
 {seq(seq(
   FNF_y0(act_AT[T](t_xyz_reduce(u[i])) - t_xyz_reduce(act_AT[T](u[i]))),
  T in {L,M,N,A1,A2}),i=1..16)} = {0},
  "Action of G64 on u[i] vs ux[i]"
 );

_ASSERT(
  {seq(t_xyz_reduce(t_rels[i]),i=1..2)} = {0},
  "t_rels = 0"
 );

 _ASSERT(
  {seq(t_xyz_reduce(s_rels[i]),i=1..2)} = {0},
  "s_rels = 0"
 );

 _ASSERT(
  {seq(t_xyz_reduce(ts_rels[i]),i=1..4)} = {0},
  "ts_rels = 0"
 );

 _ASSERT(
  {seq(t_xyz_reduce(qt_rels[i]),i=1..4)} = {0},
  "qt_rels = 0 (quadratic relations for t[i] over K)"
 );

 _ASSERT(
  map(t_xyz_reduce,u_lin_rels) = {0},
  "u_lin_rels = 0"
 );

 _ASSERT(
  map(t_xyz_reduce,u_quad_rels) = {0},
  "u_quad_rels = 0"
 );

 _ASSERT(
  {seq(t_xyz_reduce(u_rels[i]),i=1..3)} = {0},
  "u_rels = 0"
 );

 _ASSERT(
  {seq(t_xyz_reduce(sn_rels[i]),i=1..8)} = {0},
  "sn_rels = 0"
 );

 _ASSERT(
  {seq(t_xyz_reduce(numer(factor(eval(subs({a=au,b=bu},ab_rels[i]))))),i=1..2)} = {0},
  "ab_rels = 0"
 );

 _ASSERT(factor(expand({
  ab_rels[3] - (1+a[2]^2)/2*((b[1]-2)*ab_rels[1]+(b[1]+2*a[1]^2)*ab_rels[2]),
  ab_rels[4] - (1+a[1]^2)/2*((b[2]-2-8*a[2]^2)*ab_rels[1] + (b[2]-6*a[2]^2)*ab_rels[2]),
  ab_rels[5] - (ab_rels[1] + ab_rels[2])/2,
  ab_rels[6] - (b[1]*ab_rels[1] + (3*b[1]/4+a[1]^2)*ab_rels[2])/((b[1]+2)*(b[2]+2)+4),
  ab_rels[7] - (ab_rels[2]/b[1]/8)
  })) = {0},
  "relations between ab_rels"
 );

 _ASSERT(
  {op(map(t_xyz_reduce,extra_galois_rels))} = {0},
  "extra_galois_rels = 0"
 );

 _ASSERT(
  {seq(FNF_y0(tr[i]^2 - tx[i]^2),i=1..4),
   seq(seq(simplify(
    eval(subs(x = inner_quasirational_points[j],eval(subs({r=rx,y=yx0},tr[i]-tx[i]))))),
     i=1..4),j=1..130)} = {0},
  "formulae for t (up to sign) in terms of r and y[1]"
 );

 _ASSERT(map(t_xyz_reduce,{
  y[1] - ys[1],
  y[1] - ys_alt[1],
  y[2] - ys[2],
  z[1] - zs[1],
  r[1] - rs[1],
  r[2] - rs[2],
  x[1] - xs[1],
  x[2] - xs[2],
  x[3] - xs[3],
  x[4] - xs[4]
  }) = {0},
  "formulae for x,y,z,r in terms of s"
 );

 _ASSERT(map(t_xyz_reduce,{
  x[1] - xt[1],
  x[2] - xt[2],
  x[3] - xt[3],
  x[4] - xt[4],
  y[1] - yt[1],
  y[1] - yt_alt[1],
  y[2] - yt[2],
  z[1] - zt[1],
  r[1] - rt[1],
  r[2] - rt[2]
  }) = {0},
  "formulae for x,y,z,r in terms of t"
 );

 _ASSERT(
  {seq(t_xyz_reduce(sx[i] - eval(subs(t = tx,st[i]))),i=1..16)} = {0},
  "formulae for s in terms of t and in terms of x are consistent"
 );

 _ASSERT(
  {seq(t_xyz_reduce(t[i] - eval(subs(u = ut,tu[i]))),i=1..4)} = {0},
  "formulae for t in terms of u"
 );
 
 _ASSERT(
  {seq(t_xyz_reduce(s[i] - eval(subs(u = ut,su[i]))),i=1..16)} = {0},
  "formulae for s in terms of u"
 );
 
 _ASSERT(
  map(uorb_AT,{op(BXYR)}) = {op(map(o -> {op(o)},BXYR_orbits))},
  "orbits for G64 action on BXYR (up to sign)"
 );

 ok := true;
 
 for chi in G64_chars do
  for xi in G64_chars do
   m0 := add(chi[i]*xi[i],i=1..64)/64;
   m1 := `if`(chi = xi,1,0);
   if m0 <> m1 then
    ok := false;
   fi;
  od;
 od;

 _ASSERT(ok,"Orthogonality of characters for G64");

 _ASSERT(add(chi[1]^2,chi in G64_chars) = 64,
         "Completeness of character table for G64");

end:

add_check(check_galois):

######################################################################

check_KR_subfields := proc()
 local i;

 printf("%a()\n",procname);

 for i in sort(map(op,[indices(KR_subfields)])) do
  _ASSERT(KR_subfields[i]["check"],sprintf("KR_subfields[%d]",i));
 od;
end:

add_check(check_KR_subfields):