#@ CLASS: E_to_S_map

`Class/Declare`("E_to_S_map",
 "An instance of this class represents a pair of approximations to the canonical conformal maps $p,q\\:EX^*\\to S^2$.",

 ["Field","u"::table,"This is a table indexed by $\\{1,2,3\\}$.  Each entry is a rational function from the $z$-plane to $\\R$, which gives an invariant real valued function on $EX^*$.  The $k$'th component of our map $p\\:EX^*\\to S^2$ will be the product of $u_k$ with a standard function, recorded in the global variable @s2p_core[k]@."],

 ["Field","samples_z_u"::table,"This is a table indexed by $\\{1,2,3\\}$.  The $k$'th entry is a list of pairs $(a,b)$, where we have learned by some other means that $u_k(a)$ should be equal to $b$.  We will try to adjust the coefficients of $u_k$ to make this true."],

 ["Field","p","Our approximation to $p$ will be stored in this field."],

 ["Constructor","",
  proc(this)
   this["u"] := table():
   this["samples_z_u"] := table():
   this["samples_z_u"][1] := [];
   this["samples_z_u"][2] := [];
   this["samples_z_u"][3] := [];
  end
 ],

 ["Method","find_p","This sets the fields @u@, @samples_z_u@ and @p@ based on information stored in the atlas object which is suppied as a parameter to the method.",
  proc(this,EH_atlas,d1::posint := 8)
   local i,j,k,psi_inv0,phi_inv0,p1,ns,mf,mm,
    samples_H,samples_E,samples_z,samples_S,samples_z_u,
    f1,n1,m1,M1,M2,v,w;

   psi_inv0 := eval(EH_atlas["H_to_P_map"]["psi_inv"]):
   phi_inv0 := eval(EH_atlas["H_to_P_map"]["phi_inv"]):

   samples_H := EH_atlas["H_samples"]:
   samples_E := EH_atlas["H_samples_q"]:
   samples_z := map(z_proj1,samples_E):
   p1  := eval(EH_atlas["H_to_P_map"]["p1"]):
   samples_S := map(C_to_S2,map(trim,map(phi_inv0,map(p1,map(psi_inv0,samples_H))),10.^(-80))):
   ns := nops(samples_H):

   this["u"] := table():
   this["samples_z_u"] := table():

   for k from 1 to 3 do 
    mf[k] := unapply(evalf(s2p_core[k]),x);
    samples_z_u[k] := NULL:
    for i from 1 to ns do
     mm := mf[k](samples_E[i]):
     if abs(mm) > 10.^(-3) then
      samples_z_u[k] :=
       samples_z_u[k],[op(samples_z[i]),samples_S[i][k]/mm];
     fi;
    od:
    samples_z_u[k] := [samples_z_u[k]]:
    this["samples_z_u"][k] := samples_z_u[k];
   od:

   for k from 1 to 3 do 
    f1 := (z) -> [seq(seq(z[1]^i*z[2]^j,j=0..d1-i),i=0..d1)]:
    n1 := nops(f1([0,0]));
    m1 := nops(samples_z_u[k]);
    M1 := evalf(Matrix([seq(f1(samples_z_u[k][j]),j=1..m1)]));  
    M2 := evalf(Matrix([seq(samples_z_u[k][j][3] *~ f1(samples_z_u[k][j]),j=1..m1)])); 
    v,w := op(quot_approx(M1,M2)):
    this["u"][k] := unapply(add(w[j]*f1(z)[j],j=1..n1)/add(v[j]*f1(z)[j],j=1..n1),z):
   od:

   this["set_p"];

   NULL;
  end
 ],

 ["Method","set_p","",
  proc(this)
   local k,mf;

   for k from 1 to 3 do 
    mf[k] := unapply(evalf(s2p_core[k]),x);
   od;

   this["p"] := unapply(evalf([seq(mf[k](x)*this["u"][k](z_proj0(x)),k=1..3)]),x):
  end
 ],

 ["Method","radius_error","This returns $\\|p(x)\|^2-1$ a function of $z$",
  proc(this)
   local err;

   err := add(this["u"][i](z)^2 * NF_z0(s2p_core[i]^2),i=1..3) - 1;
   err := simplify(err);
   return unapply(err,z);
  end
 ],

 ["Method","conformal_error","Given a point $x0\\in EX^*$, this measures the failure of $p$ near $x0$ to be conformal.",
  proc(this,x0::RR0_4)
   local p,u,v,a,b,c,E,F,G;

   p := eval(this["p"]);

   u,v := op(tangent_frame_b(x0)):
   a := p(x0):
   b := subs(t=0,map(diff,p(x0 +~ t *~ u),t)):
   c := subs(t=0,map(diff,p(x0 +~ t *~ v),t)):
   E := dp3(b,b);
   F := dp3(b,c);
   G := dp3(c,c);

   return (E-G)^2 + 4*F^2;
  end
 ],

 ["Method","p_vertex_check","Check that $p$ has the right behaviour on $v_0$, $v_3$ and $v_6$.  The return value is a list of nine scalars which should all be zero.",
  proc(this)
   local p;
   p := eval(this["p"]);

   map(op,tidy(evalf([seq(p(v_E1[k]) -~ v_S2[k],k in [0,3,6])])));
  end
 ],

 ["Method","p_chart_check","Check that $p$ gives a conformal map to $S^2$.  The argument @A@ can either be an instance of the class @E_chart@, or a point in $EX^*$.  In the latter case, we generate a new chart at the specified point, using the second argument @d_@ to specify the degree.  The return value is a list of three polynomials in $t_1$ and $t_2$, which will be zero if $p$ is exactly conformal. ",
  proc(this,A,d_::integer)
   local C,d,p,q,pq,pq1,pq2;

   if type(A,E_chart) then
    C := eval(A):
   elif type(A,RR0_4) then
    C := `new/E_chart`():
    C["centre_set_numeric",A]:
    C["set_degree_numeric",d_];
   fi;
   d := C["degree"];
   p := eval(this["p"]);
   q := eval(C["p"]);
   pq := multi_series(p(q(t)),d+1,t[1],t[2]);
   pq1 := map(diff,pq,t[1]);
   pq2 := map(diff,pq,t[2]);
   multi_series([dp3(pq,pq)-1,dp3(pq1,pq2),dp3(pq1,pq1)-dp3(pq2,pq2)],d,t[1],t[2]);
  end
 ],

 ["Method","u_plot","Generates a plot of the function $u_k$",
  proc(this,k::integer)
   display(z_plot(this["u"][k](z)),
           map(point,this["samples_z_u"][k],colour=black));
  end
 ],

 ["Method","u_plots","Generates plots of the functions $u_1$, $u_2$ and $u_3$.",
  proc(this)
   display(Transpose(Vector([seq(this["u_plot",k],k=1..3)])));
  end
 ],

 ["Method","p_plot","Generates a plot showing the image in $S^2$ of a grid in $F_{16}\\subset EX^*$.",
  proc(this)
   local i,j,N,v11,theta,T,U,p,u;

   p := eval(this["p"]);
   u := eval(this["u"]);
   v11 := evalf(p(v_E0[11]));
   theta := arctan(-v11[1]/v11[3]);

   require_square_diffeo_E0_inverse():
   T := square_diffeo_E0_inverse_table:
   N := square_diffeo_E0_inverse_order:
   for i from 0 to N do
    for j from 0 to N do
     U[i,j] := evalf(p(T[i/N,j/N]));
    od:
   od:

   display(
    seq(seq(polygon([U[i,j],U[i+1,j],U[i+1,j+1]],colour=white,style=patch),i=0..N-1),j=0..N-1),
    seq(seq(polygon([U[i,j],U[i,j+1],U[i+1,j+1]],colour=white,style=patch),i=0..N-1),j=0..N-1),
    spacecurve([ cos(t),sin(t),0],t=0..Pi/2    ,colour=c_colour[0]),
    spacecurve([0,cos(t),-sin(t)],t=0..Pi/2    ,colour=c_colour[1]),
    spacecurve([sin(t),0,-cos(t)],t=theta..Pi/2,colour=c_colour[3]),
    spacecurve([sin(t),0,-cos(t)],t=0..theta   ,colour=c_colour[5]),
    scaling=constrained,axes=none
   );
  end
 ],

 ["Method","radius_error_plot","",
  proc(this)
   z_plot(this["radius_error"](z));
  end
 ],

 ["Method","conformal_error_plot","",
  proc(this)
   local N,TX,TZ,TF,TV,TW,i,j;

   require_square_diffeo_E0_inverse();
   N := square_diffeo_E0_inverse_order;
   TX := square_diffeo_E0_inverse_table;
   TF := table();
   TV := table();
   TW := table();
   for i from 0 to N do 
    for j from 0 to N do
     TZ[i,j] := evalf(z_proj0(TX[i/N,j/N]));
     TF[i,j] := this["conformal_error",TX[i/N,j/N]];
     TV[i,j] := [op(TZ[i,j]),TF[i,j]];
     TW[i,j] := [op(TZ[i,j]),0];
    od;
   od;

   display(
    display( 
     seq(seq(polygon([TV[i,j],TV[i+1,j],TV[i+1,j+1]]),j=0..N-1),i=0..N-1),
     seq(seq(polygon([TV[i,j],TV[i,j+1],TV[i+1,j+1]]),j=0..N-1),i=0..N-1),
     style=patchnogrid
    ),
    display(
     seq(seq(polygon([TW[i,j],TW[i+1,j],TW[i+1,j+1]]),j=0..N-1),i=0..N-1),
     seq(seq(polygon([TW[i,j],TW[i,j+1],TW[i+1,j+1]]),j=0..N-1),i=0..N-1),
     colour=grey,style=wireframe
    )
   );
  end
 ]
);