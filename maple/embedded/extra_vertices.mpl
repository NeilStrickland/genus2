v_E[14] := [sqrt((1-a_E^2)/(1+a_E^2)/2),sqrt((1-a_E^2)/(1+a_E^2)/2),a_E*sqrt(2/(1+a_E^2)),0];
for i from 1 to 7 do
 v_E[14+i] := act_R4[G16[1+i]](v_E[14]);
od:

# Note: the remaining vertices are undefined for a_E > 1/sqrt(2), because they involve complex numbers.

v_E[22] := [sqrt((1-2*a_E^2)/(1+2*a_E^2)/3),0,2*a_E*sqrt(2/3/(1+2*a_E^2)),-sqrt(2/3/(1+2*a_E^2))];
for i from 1 to 7 do
 v_E[22+i] := act_R4[G16[1+i]](v_E[22]);
od:

v_E[30] := [sqrt((1-2*a_E^2)/(1+a_E^2))/2,sqrt((1-2*a_E^2)/(1+a_E^2))/2,a_E*sqrt(2/(1+a_E^2)),sqrt(1/2/(1+a_E^2))];
for i from 1 to 15 do
 v_E[30+i] := act_R4[G16[1+i]](v_E[30]);
od:

#@ num_vertices_E 
num_vertices_E := 46:

for i from 0 to num_vertices_E-1 do
 v_E1[i] := evalf(subs(a_E = a_E1,v_E[i]));
 v_E_stereo[i] := stereo(v_E1[i]);
 v_E_label[i] := TEXT(v_E_stereo[i],i);
 v_E_tangent_projector[i] := 
  map(simplify,tangent_projector(v_E[i]));
od:

#@ find_v_E_index 
find_v_E_index := proc(x,tolerance_) 
 local i,d,x_float,tolerance;

 x_float := evalf(subs(a_E=a_E1,x));
 tolerance := `if`(nargs > 1, tolerance_, 0.00001);
 for i from 0 to num_vertices_E-1 do
  if evalf(d4(v_E1[i],x_float)) < tolerance then
   return(i);
  fi;
 od;
 return(FAIL);
end:

#@ find_v_E_permlist 
find_v_E_permlist := 
 (phi) -> [seq(find_v_index(phi(v_E[i])),i=0..num_vertices-1)];

to_cycles := proc(s) 
 local s1,s2,c;
 s1 := map(i -> i+1,s);
 s2 := convert(s1,disjcyc);
 map(c -> map(i -> i-1,c),s2);
end:

#@ find_v_E_cycles 
find_v_E_cycles := 
 (phi) -> to_cycles(find_v_E_permlist(phi));

unassign('i');

# The results below could be computed by the function find_v_E_permlist

#@ v_E_permlist
v_E_permlist[1]     := [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45]:
v_E_permlist[L]     := [0,1,3,4,5,2,7,8,9,6,11,10,13,12,15,16,17,14,19,20,21,18,23,24,25,22,27,28,29,26,31,32,33,30,35,36,37,34,39,40,41,38,43,44,45,42]:
v_E_permlist[LL]    := [0,1,4,5,2,3,8,9,6,7,10,11,12,13,16,17,14,15,20,21,18,19,24,25,22,23,28,29,26,27,32,33,30,31,36,37,34,35,40,41,38,39,44,45,42,43]:
v_E_permlist[LLL]   := [0,1,5,2,3,4,9,6,7,8,11,10,13,12,17,14,15,16,21,18,19,20,25,22,23,24,29,26,27,28,33,30,31,32,37,34,35,36,41,38,39,40,45,42,43,44]:
v_E_permlist[M]     := [1,0,2,5,4,3,9,8,7,6,12,13,10,11,18,21,20,19,14,17,16,15,26,29,28,27,22,25,24,23,34,37,36,35,30,33,32,31,42,45,44,43,38,41,40,39]:
v_E_permlist[LM]    := [1,0,3,2,5,4,6,9,8,7,13,12,11,10,19,18,21,20,15,14,17,16,27,26,29,28,23,22,25,24,35,34,37,36,31,30,33,32,43,42,45,44,39,38,41,40]:
v_E_permlist[LLM]   := [1,0,4,3,2,5,7,6,9,8,12,13,10,11,20,19,18,21,16,15,14,17,28,27,26,29,24,23,22,25,36,35,34,37,32,31,30,33,44,43,42,45,40,39,38,41]:
v_E_permlist[LLLM]  := [1,0,5,4,3,2,8,7,6,9,13,12,11,10,21,20,19,18,17,16,15,14,29,28,27,26,25,24,23,22,37,36,35,34,33,32,31,30,45,44,43,42,41,40,39,38]:
v_E_permlist[N]     := [0,1,2,5,4,3,9,8,7,6,10,11,12,13,17,16,15,14,19,18,21,20,22,25,24,23,26,29,28,27,38,41,40,39,42,45,44,43,30,33,32,31,34,37,36,35]:
v_E_permlist[LN]    := [0,1,3,2,5,4,6,9,8,7,11,10,13,12,14,17,16,15,20,19,18,21,23,22,25,24,27,26,29,28,39,38,41,40,43,42,45,44,31,30,33,32,35,34,37,36]:
v_E_permlist[LLN]   := [0,1,4,3,2,5,7,6,9,8,10,11,12,13,15,14,17,16,21,20,19,18,24,23,22,25,28,27,26,29,40,39,38,41,44,43,42,45,32,31,30,33,36,35,34,37]:
v_E_permlist[LLLN]  := [0,1,5,4,3,2,8,7,6,9,11,10,13,12,16,15,14,17,18,21,20,19,25,24,23,22,29,28,27,26,41,40,39,38,45,44,43,42,33,32,31,30,37,36,35,34]:
v_E_permlist[MN]    := [1,0,2,3,4,5,6,7,8,9,12,13,10,11,19,20,21,18,17,14,15,16,26,27,28,29,22,23,24,25,42,43,44,45,38,39,40,41,34,35,36,37,30,31,32,33]:
v_E_permlist[LMN]   := [1,0,3,4,5,2,7,8,9,6,13,12,11,10,20,21,18,19,14,15,16,17,27,28,29,26,23,24,25,22,43,44,45,42,39,40,41,38,35,36,37,34,31,32,33,30]:
v_E_permlist[LLMN]  := [1,0,4,5,2,3,8,9,6,7,12,13,10,11,21,18,19,20,15,16,17,14,28,29,26,27,24,25,22,23,44,45,42,43,40,41,38,39,36,37,34,35,32,33,30,31]:
v_E_permlist[LLLMN] := [1,0,5,2,3,4,9,6,7,8,13,12,11,10,18,19,20,21,16,17,14,15,29,26,27,28,25,22,23,24,45,42,43,44,41,38,39,40,37,34,35,36,33,30,31,32]:


#@ theta_E
theta_E[1] := arctan(sqrt(1-a_E^2)/sqrt(2)/a_E);
theta_E[2] := arctan(sqrt(2)*sqrt(sqrt(2)*sqrt(1-a_E^2)-1)*(sqrt(1-a_E^2)+sqrt(2))/(1+a_E^2));
theta_E[3] := arctan(sqrt(1-2*a_E^2));
theta_E[4] := arctan(sqrt((1-2*a_E^2)/(1+2*a_E^2)/3));

#@ v_on_c_E 
v_on_c_E := table():

for i from 0 to num_vertices_E-1 do
 for j from 0 to num_curves_E-1 do
  v_on_c_E[i,j] := NULL;
 od:
od:

# The values below could be computed by the function find_v_on_c_E

v_on_c_E[ 0, 1] := 0;
v_on_c_E[ 0, 2] := 0;
v_on_c_E[ 0, 5] := 0;
v_on_c_E[ 0, 6] := 0;
v_on_c_E[ 1, 1] := Pi;
v_on_c_E[ 1, 2] := Pi;
v_on_c_E[ 1, 7] := 0;
v_on_c_E[ 1, 8] := 0;
v_on_c_E[ 2, 0] := 0;
v_on_c_E[ 2, 4] := -1/2*Pi;
v_on_c_E[ 3, 0] :=  1/2*Pi;
v_on_c_E[ 3, 3] :=  1/2*Pi;
v_on_c_E[ 4, 0] :=      Pi;
v_on_c_E[ 4, 4] :=  1/2*Pi;
v_on_c_E[ 5, 0] := -1/2*Pi;
v_on_c_E[ 5, 3] := -1/2*Pi;
v_on_c_E[ 6, 0] :=  1/4*Pi;
v_on_c_E[ 6, 1] :=  1/2*Pi;
v_on_c_E[ 6,13] := 0;
v_on_c_E[ 6,16] := 0;
v_on_c_E[ 7, 0] :=  3/4*Pi;
v_on_c_E[ 7, 2] :=  1/2*Pi;
v_on_c_E[ 7,14] := 0;
v_on_c_E[ 7,15] :=      Pi;
v_on_c_E[ 8, 0] := -3/4*Pi;
v_on_c_E[ 8, 1] := -1/2*Pi;
v_on_c_E[ 8,13] :=      Pi;
v_on_c_E[ 8,16] :=      Pi;
v_on_c_E[ 9, 0] := -1/4*Pi;
v_on_c_E[ 9, 2] := -1/2*Pi;
v_on_c_E[ 9,14] :=      Pi;
v_on_c_E[ 9,15] := 0;
v_on_c_E[10, 4] := 0;
v_on_c_E[10, 6] :=      Pi;
v_on_c_E[10, 9] :=      Pi;
v_on_c_E[10,10] := 0;
v_on_c_E[10,14] :=  1/2*Pi;
v_on_c_E[10,16] :=  1/2*Pi;
v_on_c_E[11, 3] := 0;
v_on_c_E[11, 5] :=      Pi;
v_on_c_E[11, 9] := 0;
v_on_c_E[11,10] :=      Pi;
v_on_c_E[11,13] :=  1/2*Pi;
v_on_c_E[11,15] :=  1/2*Pi;
v_on_c_E[12, 4] :=      Pi;
v_on_c_E[12, 8] :=      Pi;
v_on_c_E[12,11] :=      Pi;
v_on_c_E[12,12] := 0;
v_on_c_E[12,13] := -1/2*Pi;
v_on_c_E[12,15] := -1/2*Pi;
v_on_c_E[13, 3] :=      Pi;
v_on_c_E[13, 7] :=      Pi;
v_on_c_E[13,11] := 0;
v_on_c_E[13,12] :=      Pi;
v_on_c_E[13,14] := -1/2*Pi;
v_on_c_E[13,16] := -1/2*Pi;
v_on_c_E[14, 1] :=  theta_E[1];
v_on_c_E[14, 9] :=  1/2*Pi;
v_on_c_E[15, 2] :=  theta_E[1];
v_on_c_E[15,10] :=  1/2*Pi;
v_on_c_E[16, 1] := -theta_E[1];
v_on_c_E[16, 9] := -1/2*Pi;
v_on_c_E[17, 2] := -theta_E[1];
v_on_c_E[17,10] := -1/2*Pi;
v_on_c_E[18, 2] :=  theta_E[1]-Pi;
v_on_c_E[18,11] :=  1/2*Pi;
v_on_c_E[19, 1] := -theta_E[1]+Pi;
v_on_c_E[19,12] :=  1/2*Pi;
v_on_c_E[20, 2] := -theta_E[1]+Pi;
v_on_c_E[20,11] := -1/2*Pi;
v_on_c_E[21, 1] :=  theta_E[1]-Pi;
v_on_c_E[21,12] := -1/2*Pi;
v_on_c_E[22, 5] := -theta_E[2]+Pi;
v_on_c_E[23, 6] := -theta_E[2]+Pi;
v_on_c_E[24, 5] :=  theta_E[2]-Pi;
v_on_c_E[25, 6] :=  theta_E[2]-Pi;
v_on_c_E[26, 7] := -theta_E[2]+Pi;
v_on_c_E[27, 8] := -theta_E[2]+Pi;
v_on_c_E[28, 7] :=  theta_E[2]-Pi;
v_on_c_E[29, 8] :=  theta_E[2]-Pi;
v_on_c_E[30, 9] := -theta_E[3]+Pi;
v_on_c_E[31,10] := -theta_E[3]+Pi;
v_on_c_E[32, 9] :=  theta_E[3]-Pi;
v_on_c_E[33,10] :=  theta_E[3]-Pi;
v_on_c_E[34,11] := -theta_E[3]+Pi;
v_on_c_E[35,12] := -theta_E[3]+Pi;
v_on_c_E[36,11] :=  theta_E[3]-Pi;
v_on_c_E[37,12] :=  theta_E[3]-Pi;
v_on_c_E[38,10] := -theta_E[3];
v_on_c_E[39, 9] :=  theta_E[3];
v_on_c_E[40,10] :=  theta_E[3];
v_on_c_E[41, 9] := -theta_E[3];
v_on_c_E[42,12] :=  theta_E[3];
v_on_c_E[43,11] := -theta_E[3];
v_on_c_E[44,12] := -theta_E[3];
v_on_c_E[45,11] :=  theta_E[3];

#@ v_on_c_cayley
for i from 0 to num_vertices_E-1 do
 for j from 1 to 4 do
  v_on_c_cayley[i,j] := NULL;
 od:
od:

v_on_c_cayley[ 2,2] := 0;
v_on_c_cayley[ 2,4] := Pi;
v_on_c_cayley[ 3,1] := Pi;
v_on_c_cayley[ 3,3] := 0;
v_on_c_cayley[ 4,2] := Pi;
v_on_c_cayley[ 4,4] := 0;
v_on_c_cayley[ 5,1] := 0;
v_on_c_cayley[ 5,3] := Pi;
v_on_c_cayley[22,3] := -1/2*Pi;
v_on_c_cayley[23,4] := -1/2*Pi;
v_on_c_cayley[24,1] := -1/2*Pi;
v_on_c_cayley[25,2] := -1/2*Pi;
v_on_c_cayley[26,1] :=  1/2*Pi;
v_on_c_cayley[27,2] :=  1/2*Pi;
v_on_c_cayley[28,3] :=  1/2*Pi;
v_on_c_cayley[29,4] :=  1/2*Pi;
v_on_c_cayley[30,4] := -1/2*Pi-theta_E[4];
v_on_c_cayley[31,1] := -1/2*Pi-theta_E[4];
v_on_c_cayley[32,2] := -1/2*Pi-theta_E[4];
v_on_c_cayley[33,3] := -1/2*Pi-theta_E[4];
v_on_c_cayley[34,4] :=  1/2*Pi+theta_E[4];
v_on_c_cayley[35,1] :=  1/2*Pi+theta_E[4];
v_on_c_cayley[36,2] :=  1/2*Pi+theta_E[4];
v_on_c_cayley[37,3] :=  1/2*Pi+theta_E[4];
v_on_c_cayley[38,2] := -1/2*Pi+theta_E[4];
v_on_c_cayley[39,3] := -1/2*Pi+theta_E[4];
v_on_c_cayley[40,4] := -1/2*Pi+theta_E[4];
v_on_c_cayley[41,1] := -1/2*Pi+theta_E[4];
v_on_c_cayley[42,2] :=  1/2*Pi-theta_E[4];
v_on_c_cayley[43,3] :=  1/2*Pi-theta_E[4];
v_on_c_cayley[44,4] :=  1/2*Pi-theta_E[4];
v_on_c_cayley[45,1] :=  1/2*Pi-theta_E[4];

#@ v_track_E
for i from 0 to num_vertices_E-1 do 
 v_track_E[i] := []:
od:

#@ c_track_E
for j from 0 to num_curves_E-1 do
 c_track_E[j] := []:
od:

for i from 0 to num_vertices_E-1 do
 for j from 0 to num_curves_E-1 do
  if v_on_c_E[i,j] <> NULL then
   v_track_E[i] := [op(v_track_E[i]),j = v_on_c_E[i,j]];
   c_track_E[j] := [op(c_track_E[j]),i = v_on_c_E[i,j]];
  fi;
 od:
od:

#@ find_v_on_c_E 
find_v_on_c_E := proc()
 local i,j,k,t0,t1,x0,x1,theta0,recognise_theta;
 global v_on_c_E,v_on_c_E_string;

 for k from 1 to 3 do 
  theta0[k] := evalf(subs(a_E=0.1,theta_E[k]));
 od;

 recognise_theta := proc(t)
  local t0,i,k,e;
  if type(t/Pi,rational) then return t; fi;
  t0 := evalf(subs(a_E=0.1,t));
  for k from 1 to 3 do
   i := evalf(12*(t0 - theta0[k])/Pi);
   e := i - round(i);
   if abs(e) < 10.^(-20) then
    return theta[k] + round(i)*Pi/12;
   fi;
   i := evalf(12*(t0 + theta0[k])/Pi);
   e := i - round(i);
   if abs(e) < 10.^(-20) then
    return -theta[k] + round(i)*Pi/12;
   fi;
  od:
  return t;
 end:

 v_on_c_E := table():

 for i from 0 to num_vertices_E-1 do
  for j from 0 to num_curves_E-1 do
   v_on_c_E[i,j] := NULL;
   try
    t0 := simplify_E(c_param_E[j](v_E[i]));
    t1 := Re(evalf(subs(a_E=0.1,t0)));
    x0 := evalf(subs(a_E=0.1,v_E[i]));
    x1 := evalf(subs(a_E=0.1,c_E[j](t1)));
    if d4f(x0,x1) < 10.^(-20) then
     v_on_c_E[i,j] := recognise_theta(t0);
    fi;
   catch:
    v_on_c_E[i,j] := v_on_c[i,j];
   end try:
  od;
 od;

 v_on_c_E_string := "";

 for i from 0 to num_vertices_E-1 do
  for j from 0 to num_curves_E-1 do
   if v_on_c_E[i,j] <> NULL then
    t0 := v_on_c_E[i,j];
    t1 := sprintf("%A",subs(theta='theta_E',t0));
    if t0 <> 0 and type(t0/Pi,rational) then 
     while length(t1) < 7 do t1 := cat(" ",t1); od;
    fi;
    if substring(t1,1..7) = "theta_E" then t1 := cat(" ",t1); fi;
    v_on_c_E_string := cat(v_on_c_E_string,sprintf("v_on_c_E[%2d,%2d] := %s;\n",i,j,t1));
   fi;
  od:
 od:
end:
