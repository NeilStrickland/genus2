set_a_E0 := proc(a::RR0) 
 global a_E0,a_E1,g0,g_00,g_10,g1,g_01,g_11,dg0,dg1,
   square_norm_of_dg0,square_norm_of_dg1,
   curvature0,curvature1,
   square_norm_of_dg_z0,square_norm_of_dg_z1,
   curvature_z0,curvature_z1,g_stereo0,g_stereo1,
   laplacian_z_C0,laplacian_z0,
   omega0,omega1,
   v_E0,v_E1,c_E0,c_E1,c_param_E0,c_param_E1,
   yx0,yx1,xy0,xy1,uy0,uy1,uf0,uf1,uz0,uz1,zx0,zx1,
   dyx0,dyx1,dxy0,dxy1,dzx0,dzx1,
   y_proj0,y_proj1,z_proj0,z_proj1,w_proj0,w_proj1,
   y_lift0,y_lift1,z_lift0,z_lift1,
   z_to_w0,z_to_w1,y_to_w0,y_to_w1,
   z_rels0,y_rels0,x_rels0,NF_z0,NF_y0,NF_x0,
   is_in_F4_E0_measure,is_in_F16_E0_measure,
   num_vertices_E,num_curves_E;
 local i,t;

 a_E0 := a; #@  a_E0 
 a_E1 := evalf(a_E0); #@  a_E1 

 g0    := unapply(subs(a_E=a_E0,g(xx)),x): #@  g0    
 g_00  := unapply(subs(a_E=a_E0,g_0(xx)),x): #@  g_00  
 g_10  := unapply(subs(a_E=a_E0,g_1(xx)),x): #@  g_10  
 dg0   := unapply(subs(a_E=a_E0,dg(xx)),x): #@  dg0   
 square_norm_of_dg0 := unapply(simplify(subs(a_E=a_E0,square_norm_of_dg(xx))),x): #@  square_norm_of_dg0 
 curvature0 := unapply(simplify(subs(a_E=a_E0,curvature(xx))),x): #@  curvature0 
 square_norm_of_dg_z0 := unapply(subs(a_E=a_E0,square_norm_of_dg_z(z)),z): #@  square_norm_of_dg_z0 
 curvature_z0 := unapply(1 - factor(1-subs(a_E=a_E0,curvature_z(z))),z): #@  curvature_z0 
 g_stereo0 := unapply(subs(a_E=a_E0,g_stereo([u[1],u[2],u[3]])),u): #@  g_stereo0 

 g1    := unapply(subs(a_E=a_E1,g(xx)),x): #@  g1    
 g_01  := unapply(subs(a_E=a_E1,g_0(xx)),x): #@  g_01  
 g_11  := unapply(subs(a_E=a_E1,g_1(xx)),x): #@  g_11  
 dg1   := unapply(subs(a_E=a_E1,dg(xx)),x): #@  dg1   
 square_norm_of_dg1 := unapply(evalf(square_norm_of_dg0(xx)),x): #@  square_norm_of_dg1 
 curvature1 := unapply(evalf(curvature0(xx)),x): #@  curvature1 
 square_norm_of_dg_z1 := unapply(subs(a_E=a_E1,square_norm_of_dg_z(z)),z): #@  square_norm_of_dg_z1 
 curvature_z1 := unapply(1 - factor(1-subs(a_E=a_E1,curvature_z(z))),z): #@  curvature_z1 
 g_stereo1 := unapply(evalf(subs(a_E=a_E0,g_stereo([u[1],u[2],u[3]]))),u): #@  g_stereo1 

 #@ laplacian_z_C0
 for i from 0 to 5 do 
  laplacian_z_C0[i] := factor(subs(a_E=a_E0,laplacian_z_C[i])); 
 od;

 #@  laplacian_z0
 laplacian_z0 := proc(p)
  (laplacian_z_C0[1] * diff(p,z[1]) + 
   laplacian_z_C0[2] * diff(p,z[2]) + 
   laplacian_z_C0[3] * diff(p,z[1],z[1]) + 
   laplacian_z_C0[4] * diff(p,z[1],z[2]) + 
   laplacian_z_C0[5] * diff(p,z[2],z[2]))/laplacian_z_C0[0]; 
 end:

 #@ omega0
 #@ omega1
 for i from 1 to 2 do
  omega0[i] := unapply(simplify(subs(a_E=a_E0,omega[i](t))),t);
  omega1[i] := unapply(evalf(subs(a_E=a_E1,omega[i](t))),t);
 od;

 if not(type(num_vertices_E,posint)) then
  num_vertices_E := 14;
 fi;

 #@ v_E0
 #@ v_E1
 for i from 0 to num_vertices_E-1 do
  v_E0[i] := combine(simplify(subs(a_E = a_E0,v_E[i]))):
  v_E1[i] := evalf(v_E0[i]);
 od:

 assume(t::real):

 if not(type(num_curves_E,posint)) then
  num_curves_E := 9;
 fi;
 
 #@ c_E0
 #@ c_E1
 #@ c_param_E0
 #@ c_param_E1
 for i from 0 to num_curves_E - 1 do
  c_E0[i] := unapply(simplify(subs(csgn(cos(t)^2-3)=-1,simplify(subs(a_E=a_E0,c_E[i](t))))),t):
  c_E1[i] := unapply(evalf(c_E0[i](t)),t):
  c_param_E0[i] := unapply(simplify(subs(a_E=a_E0,c_param_E[i](xx))),x);
  c_param_E1[i] := unapply(simplify(evalf(subs(a_E=a_E1,c_param_E[i](xx)))),x);
 od:

 #@ yx0
 #@ dyx0
 #@ uy0
 #@ uf0
 for i from 1 to 2 do 
  yx0[i]  := simplify(subs(a_E=a_E0,yx[i])):
  dyx0[i] := simplify(subs(a_E=a_E0,dyx[i])):
  uy0[i]  := simplify(subs(a_E=a_E0,uy[i])):
  uf0[i]  := unapply(uy0[i],y):
 od:

 #@ xy0
 #@ dxy0
 for i from 1 to 4 do 
  xy0[i]  := simplify(subs(a_E=a_E0,xy[i])):
  dxy0[i] := simplify(subs(a_E=a_E0,dxy[i])):
 od:

 #@ uz0
 uz0[3] := simplify(subs(a_E=a_E0,uz[3])):
 uz0[4] := simplify(subs(a_E=a_E0,uz[4])):

 for i from 1 to 5 do
  zx0[i]  := simplify(subs(a_E=a_E0,zx[i])):
  dzx0[i] := simplify(subs(a_E=a_E0,dyx[i])):
 od:

 y_proj0 := unapply(subs(a_E=a_E0,y_proj(xx)),x): #@ y_proj0 
 z_proj0 := unapply(subs(a_E=a_E0,z_proj(xx)),x): #@ z_proj0 
 w_proj0 := unapply(subs(a_E=a_E0,w_proj(xx)),x): #@ w_proj0 
 z_to_w0 := unapply(subs(a_E=a_E0,z_to_w(z)),z):  #@ z_to_w0 
 y_to_w0 := unapply(subs(sqrt(y[2]^2)=abs(y[2]),subs(a_E=a_E0,y_to_w(y))),y): #@  y_to_w0 
 y_lift0 := unapply(subs(a_E=a_E0,y_lift(xx)),x): #@ y_lift0 
 z_lift0 := unapply(subs(a_E=a_E0,z_lift(xx)),x): #@ z_lift0 

 yx1[1] := simplify(evalf(subs(a_E=a_E1,yx[1]))): #@ yx1
 yx1[2] := simplify(evalf(subs(a_E=a_E1,yx[2]))):
 uy1[1] := simplify(evalf(subs(a_E=a_E1,uy[1]))): #@ uy1
 uy1[2] := simplify(evalf(subs(a_E=a_E1,uy[2]))):
 uf1[1] := unapply(uy1[1],y):                     #@ uf1
 uf1[2] := unapply(uy1[2],y):
 uz1[3] := simplify(evalf(subs(a_E=a_E1,uz[3]))): #@ uz1
 uz1[4] := simplify(evalf(subs(a_E=a_E1,uz[4]))):

 #@ zx1
 for i from 1 to 5 do
  zx1[i] := simplify(evalf(subs(a_E=a_E1,zx[i]))):
 od:

 y_proj1 := unapply(evalf(subs(a_E=a_E1,y_proj(xx))),x): #@ y_proj1 
 z_proj1 := unapply(evalf(subs(a_E=a_E1,z_proj(xx))),x): #@ z_proj1 
 w_proj1 := unapply(evalf(subs(a_E=a_E1,w_proj(xx))),x): #@ w_proj1 
 z_to_w1 := unapply(evalf(subs(a_E=a_E1,z_to_w(z))),z):  #@ z_to_w1 
 y_to_w1 := unapply(subs(sqrt(y[2]^2)=abs(y[2]),evalf(subs(a_E=a_E1,y_to_w(y)))),y): #@  y_to_w1 
 y_lift1 := unapply(evalf(subs(a_E=a_E1,y_lift(xx))),x): #@ y_lift1 
 z_lift1 := unapply(evalf(subs(a_E=a_E1,z_lift(xx))),x): #@ z_lift1 

 z_rels0 := simplify(subs(a_E=a_E0,z_rels));   #@ z_rels0 
 NF_z0 := (u) -> NormalForm(u,z_rels0,z_vars); #@ NF_z0 
 y_rels0 := simplify(subs(a_E=a_E0,y_rels));   #@ y_rels0 
 NF_y0 := (u) -> NormalForm(u,y_rels0,y_vars); #@ NF_y0 
 x_rels0 := simplify(subs(a_E=a_E0,x_rels));   #@ x_rels0 
 NF_x0 := (u) -> NormalForm(u,x_rels0,x_vars); #@ NF_x0 

 #@ is_in_F4_E0_measure 
 is_in_F4_E0_measure := (x) -> max(0,-evalf(x[1]),-evalf(x[2])); 

 #@ is_in_F16_E0_measure
 is_in_F16_E0_measure := (x) ->
   max(0,-evalf(x[1]),-evalf(x[2]),-evalf(x[3]),-evalf(g_11(x)));

 NULL;
end:

#@ c_check_E0
c_check_E0[0] := proc(x) max(abs(evalf(x[3])),abs(evalf(x[4]))); end:
c_check_E0[1] := proc(x) max(abs(evalf(x[1]-x[2])),abs(evalf(x[4]))); end:
c_check_E0[2] := proc(x) max(abs(evalf(x[1]+x[2])),abs(evalf(x[4]))); end:
c_check_E0[3] := proc(x) max(abs(evalf(x[1])),evalf(-g_10(x))); end:
c_check_E0[4] := proc(x) max(abs(evalf(x[2])),evalf( g_10(x))); end:
c_check_E0[5] := proc(x) max(abs(evalf(x[2])),evalf(-g_10(x)),-evalf(x[3])); end:
c_check_E0[6] := proc(x) max(abs(evalf(x[1])),evalf( g_10(x)),-evalf(x[3])); end:
c_check_E0[7] := proc(x) max(abs(evalf(x[2])),evalf(-g_10(x)), evalf(x[3])); end:
c_check_E0[8] := proc(x) max(abs(evalf(x[1])),evalf( g_10(x)), evalf(x[3])); end:

#@ classify_point_E0 
classify_point_E0 := proc(x0::RR0_4,tol)
 local i;

 for i from 0 to 13 do
  if d4f(x0,v_E0[i]) < tol then
   return sprintf("V%d",i);
  fi;
 od;
 for i from 0 to 8 do
  if c_check_E0[i](x0) < tol then
   return sprintf("C%d",i);
  fi;
 od;
 return "FREE";
end:

#@ is_in_F4_E0 
is_in_F4_E0 := proc(x::RR0_4)
 is(x[1] >= 0) and is(x[2] >= 0);
end:

#@ is_in_F16_E0 
is_in_F16_E0 := proc(x::RR0_4)
 is(x[1] >= 0) and is(x[2] >= 0) and is(x[3] >= 0) and is(simplify(g_10(x)) >= 0);
end:

#@ retract_F16_E0 
retract_F16_E0 := proc(x::RR0_4)
 if is(g_10(x) >= 0) then 
  [abs(x[1]),abs(x[2]),abs(x[3]),-abs(x[4])];
 elif is(g_10(x) <= 0) then 
  [abs(x[2]),abs(x[1]),abs(x[3]),-abs(x[4])];
 else
  FAIL;
 fi;
end:

#@ is_in_F4_E1 
is_in_F4_E1 := proc(x::RR0_4)
 evalf(x[1]) >= 0 and evalf(x[2]) >= 0;
end:

#@ is_in_F16_E1 
is_in_F16_E1 := proc(x::RR0_4)
 evalf(x[1]) >= 0 and evalf(x[2]) >= 0 and evalf(x[3]) >= 0 and evalf(g_10(x)) >= 0;
end:

#@ retract_F16_E1 
retract_F16_E1 := proc(x::RR0_4)
 if evalf(g_10(x)) >= 0 then 
  [abs(x[1]),abs(x[2]),abs(x[3]),-abs(x[4])];
 else
  [abs(x[2]),abs(x[1]),abs(x[3]),-abs(x[4])];
 fi;
end:

set_a_E0(1/sqrt(2));

