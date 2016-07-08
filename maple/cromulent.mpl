# This file contains information about the general theory of cromulent
# surfaces.

######################################################################
# A cromulent surface has certain labelled points v[i] (0 <= i <= 13).
# A curve system consists of maps c[j] from R to the surface satisfying
# various axioms.  One axiom says that for certain values of i,j and t,
# we must have c[j](t) = v[i].  In these cases, t appears as v_on_c[i,j]
# in the table defined below.  For example, the entry v_on_c[4,0] = Pi
# means that c[0](Pi) = v[4].

#@ v_on_c 
v_on_c := table():

for i from 0 to 13 do
 for j from 0 to 8 do
  v_on_c[i,j] := NULL;
 od:
od:

v_on_c[ 0, 1] := 0:
v_on_c[ 0, 2] := 0:
v_on_c[ 0, 5] := 0:
v_on_c[ 0, 6] := 0:
v_on_c[ 1, 1] := Pi:
v_on_c[ 1, 2] := Pi:
v_on_c[ 1, 7] := 0:
v_on_c[ 1, 8] := 0:
v_on_c[ 2, 0] := 0:
v_on_c[ 2, 4] := -Pi/2:
v_on_c[ 3, 0] := Pi/2:
v_on_c[ 3, 3] := Pi/2:
v_on_c[ 4, 0] := Pi:
v_on_c[ 4, 4] := Pi/2:
v_on_c[ 5, 0] := -Pi/2:
v_on_c[ 5, 3] := -Pi/2:
v_on_c[ 6, 0] := Pi/4:
v_on_c[ 6, 1] := Pi/2:
v_on_c[ 7, 0] := 3*Pi/4:
v_on_c[ 7, 2] := Pi/2:
v_on_c[ 8, 0] := -3*Pi/4:
v_on_c[ 8, 1] := -Pi/2:
v_on_c[ 9, 0] := -Pi/4:
v_on_c[ 9, 2] := -Pi/2:
v_on_c[10, 4] := 0:
v_on_c[10, 6] := Pi:
v_on_c[11, 3] := 0:
v_on_c[11, 5] := Pi:
v_on_c[12, 4] := Pi:
v_on_c[12, 8] := Pi:
v_on_c[13, 3] := Pi:
v_on_c[13, 7] := Pi:

# The next block encodes the same information in a slightly different way.
# It sets things up so that (for example) c_gen[0](Pi) returns v_gen[4].
# Some arcane features of Maple evaluation rules make it awkward to do
# this automatically, so we do it by hand instead.

unassign('c_gen');

#@ c_gen
#@ v_gen
c_gen[ 0](      0) := v_gen[ 2]:
c_gen[ 0](   Pi  ) := v_gen[ 4]:
c_gen[ 0](   Pi/2) := v_gen[ 3]:
c_gen[ 0](   Pi/4) := v_gen[ 6]:
c_gen[ 0](  -Pi/2) := v_gen[ 5]:
c_gen[ 0](  -Pi/4) := v_gen[ 9]:
c_gen[ 0]( 3*Pi/4) := v_gen[ 7]:
c_gen[ 0](-3*Pi/4) := v_gen[ 8]:
c_gen[ 1](      0) := v_gen[ 0]:
c_gen[ 1](   Pi  ) := v_gen[ 1]:
c_gen[ 1](   Pi/2) := v_gen[ 6]:
c_gen[ 1](  -Pi/2) := v_gen[ 8]:
c_gen[ 2](      0) := v_gen[ 0]:
c_gen[ 2](   Pi  ) := v_gen[ 1]:
c_gen[ 2](   Pi/2) := v_gen[ 7]:
c_gen[ 2](  -Pi/2) := v_gen[ 9]:
c_gen[ 3](      0) := v_gen[11]:
c_gen[ 3](   Pi  ) := v_gen[13]:
c_gen[ 3](   Pi/2) := v_gen[ 3]:
c_gen[ 3](  -Pi/2) := v_gen[ 5]:
c_gen[ 4](      0) := v_gen[10]:
c_gen[ 4](   Pi  ) := v_gen[12]:
c_gen[ 4](   Pi/2) := v_gen[ 4]:
c_gen[ 4](  -Pi/2) := v_gen[ 2]:
c_gen[ 5](      0) := v_gen[ 0]:
c_gen[ 5](   Pi  ) := v_gen[11]:
c_gen[ 6](      0) := v_gen[ 0]:
c_gen[ 6](   Pi  ) := v_gen[10]:
c_gen[ 7](      0) := v_gen[ 1]:
c_gen[ 7](   Pi  ) := v_gen[13]:
c_gen[ 8](      0) := v_gen[ 1]:
c_gen[ 8](   Pi  ) := v_gen[12]:

# The next block encodes the same information in yet another way.
# For example, v_track[4] will be [0=0,4=-Pi/2], indicating that
# v[4] = c[0](0) = c[4](-Pi/2).  Similarly, c_track[7] will be
# [1=0,13=Pi], indicating that c[7](0) = v[1] and c[7](Pi) = v[13].

#@ v_track
for i from 0 to 13 do 
 v_track[i] := []:
od:

#@ c_track
for j from 0 to 8 do
 c_track[j] := []:
od:

for i from 0 to 13 do
 for j from 0 to 8 do
  if v_on_c[i,j] <> NULL then
   v_track[i] := [op(v_track[i]),j = v_on_c[i,j]];
   c_track[j] := [op(c_track[j]),i = v_on_c[i,j]];
  fi;
 od:
od:

######################################################################
# We now divide the curves c[i] into segments with endpoints at
# the vertices v[j], oriented in the direction of increasing values
# of the parameter t.  marked_edges will be a list of triples
# [j1,j2,i], one for each segment of c[i] running from v[j1] to v[j2].
# edges will be a list of pairs [j1,j2], now written with j1 < j2,
# irrespective of the direction of t.  Note that c[10] runs from 
# v[4] to v[6] (with 0 <= t <= Pi) and then back to v[4] (with
# Pi <= t <= 2 Pi).  It contributes entries [4,6,10] and [6,4,10]
# to marked_edges, but only a single entry [4,6] to edges.  Similar
# remarks apply to c[11], c[12] and c[13].

#@ edges
#@ marked_edges
proc()
 local i,j,t,V;
 global edges,marked_edges;

 marked_edges := NULL:
 for i from 0 to 8 do
  V := NULL;
  for j from 0 to 13 do
   if v_on_c[j,i] <> NULL then
    t := v_on_c[j,i]/(2*Pi);
    if (t < 0) then t := t + 1; fi;
    V := V,[j,t];
   fi;
  od;
  V := sort([V],(a,b) -> a[2] < b[2]);
  V := [op(V),V[1]];
  marked_edges := marked_edges,seq([V[j][1],V[j+1][1],i],j=1..nops(V)-1);
 od:

 marked_edges := [marked_edges];
 edges := sort([op({op(map(u -> sort([u[1],u[2]]),marked_edges))})]);
end():

######################################################################
# Another one of the axioms for a curve system describes g.c[i](t) 
# for all g in G and i from 0 to 8.  If act_c_data[g,i] = [j,m,c] 
# in the table below, then g.c[i](t) = c[j](m*t + c).

#@ act_c_data
act_c_data[L,0] := [0, 1,Pi/2]:
act_c_data[L,1] := [2, 1,0]:
act_c_data[L,2] := [1,-1,0]:
act_c_data[L,3] := [4, 1,0]:
act_c_data[L,4] := [3,-1,0]:
act_c_data[L,5] := [6, 1,0]:
act_c_data[L,6] := [5,-1,0]:
act_c_data[L,7] := [8, 1,0]:
act_c_data[L,8] := [7,-1,0]:

act_c_data[M,0] := [0,-1,0]:
act_c_data[M,1] := [2, 1,Pi]:
act_c_data[M,2] := [1, 1,Pi]:
act_c_data[M,3] := [3, 1,Pi]:
act_c_data[M,4] := [4,-1,-Pi]:
act_c_data[M,5] := [7, 1,0]:
act_c_data[M,6] := [8,-1,0]:
act_c_data[M,7] := [5, 1,0]:
act_c_data[M,8] := [6,-1,0]:

act_c_data[N,0] := [0,-1,0]:
act_c_data[N,1] := [2,-1,0]:
act_c_data[N,2] := [1,-1,0]:
act_c_data[N,3] := [3,-1,0]:
act_c_data[N,4] := [4, 1,0]:
act_c_data[N,5] := [5, 1,0]:
act_c_data[N,6] := [6,-1,0]:
act_c_data[N,7] := [7, 1,0]:
act_c_data[N,8] := [8,-1,0]:

# c_involution[i] is an antiholomorphic involution in G that
# acts as the identity on c[i](t) for all t.

#@ c_involution
c_involution[0] := MN:
c_involution[1] := LN:
c_involution[2] := LLLN:
c_involution[3] := LLN:
c_involution[4] := N:
c_involution[5] := N:
c_involution[6] := LLN:
c_involution[7] := N:
c_involution[8] := LLN:

# For each i, the set C[i] is defined to be the connected
# component of the fixed set of c_involution[i] that contains
# v[j] for a certain value of j.  These values are stored as
# c_basepoint[i].

#@ c_basepoint
c_basepoint[0] := 2;
c_basepoint[1] := 0;
c_basepoint[2] := 0;
c_basepoint[3] := 11;
c_basepoint[4] := 10;
c_basepoint[5] := 0;
c_basepoint[6] := 0;
c_basepoint[7] := 1;
c_basepoint[8] := 1;

######################################################################

# All our plots display the curves c[i] (or their images under various
# maps) using the colour scheme defined below.  Note that curves have
# the same colour iff they are in the same G-orbit.

#@ c_colour
c_colour[0] := cyan:
c_colour[1] := green:
c_colour[2] := green:
c_colour[3] := magenta:
c_colour[4] := magenta:
for i from  5 to  8 do c_colour[i] := blue: od:

c_colour[false] := black:

# If v[i] and v[j] both lie on the curve c[k], then edge_curve(i,j)
# will return k.  
#
# In some cases, we express a cromulent surface X as a quotient of
# some other space X0.  Preimages in X0 of the point v[3] in X (for
# example) may be labelled as v[3.1], v[3.2] and so on.  We use
# the floor() function in edge_curve() to handle this. 

#@ edge_curve 
edge_curve := proc(ij)
 option remember;
 local i,j,k;

 i,j := op(map(floor,ij));
 for k from 0 to 8 do
  if v_on_c[i,k] <> NULL and v_on_c[j,k] <> NULL then
   return(k);
  fi;
 od;

 return(false);
end:

# edge_colour(i,j) gives the appropriate colour for the edge from
# v[i] to v[j].

#@ edge_colour 
edge_colour := (ij) -> c_colour[edge_curve(ij)];

######################################################################

# Every cromulent surface has a fundamental domain F4 for the action 
# of {1,LL,N,LLN}, and a fundamental domain F16 for the action of the
# whole group G.  

# Vertices lying on the boundary of F4.

#@ F4_vertices 
F4_vertices := [0,1,2,3,6,10,11,12,13]:

# If F4_curve_limits[k] = a .. b, then
# { t in [0,2 pi] : c_k(t) in F_4 } = [a,b] (or [a,b] u {2 pi}).
# If F4_curve_limits[k] = NULL, then
# { t in [0,2 pi] : c_k(t) in F_4 } is finite (and usually empty).

#@ F4_curve_limits 
F4_curve_limits := table():
F4_curve_limits[ 0] := 0 .. Pi/2:
F4_curve_limits[ 1] := 0 .. Pi:
F4_curve_limits[ 2] := NULL:
F4_curve_limits[ 3] := 0 .. Pi:
F4_curve_limits[ 4] := Pi .. 2*Pi:
F4_curve_limits[ 5] := 0 .. Pi:
F4_curve_limits[ 6] := 0 .. Pi:
F4_curve_limits[ 7] := 0 .. Pi:
F4_curve_limits[ 8] := 0 .. Pi:

# Vertices lying on the boundary of F16.

#@ F16_vertices 
F16_vertices := [0,3,6,11]:

# Parameter values for edges of F16.

#@ F16_curve_limits
F16_curve_limits[ 0] := Pi/4 .. Pi/2:
F16_curve_limits[ 1] := 0 .. Pi/2:
F16_curve_limits[ 2] := NULL:
F16_curve_limits[ 3] := 0 .. Pi/2:
F16_curve_limits[ 4] := NULL:
F16_curve_limits[ 5] := 0 .. Pi:
F16_curve_limits[ 6] := NULL:
F16_curve_limits[ 7] := NULL:
F16_curve_limits[ 8] := NULL:

######################################################################

# The homology group H_1 of a cromulent surface is isomorphic to Z^4.
# More precisely, there is a unique isomorphism H_1 -> Z^4 which sends
# the homology class of the curve c[i] to the vector c_homology[i] 
# as tabulated below.

#@ c_homology
c_homology[ 0] := [ 0, 0, 0, 0];
c_homology[ 1] := [ 1, 1,-1,-1];
c_homology[ 2] := [-1, 1, 1,-1];
c_homology[ 3] := [ 0, 1, 0,-1];
c_homology[ 4] := [-1, 0, 1, 0];
c_homology[ 5] := [ 1, 0, 0, 0];
c_homology[ 6] := [ 0, 1, 0, 0];
c_homology[ 7] := [ 0, 0, 1, 0];
c_homology[ 8] := [ 0, 0, 0, 1];

# This table gives the action of G on Z^4 corresponding (via the 
# above isomorphism) to the action on H_1.

#@ act_Z4
act_Z4[1]    := (n) -> [ n[1], n[2], n[3], n[4]];
act_Z4[L]    := (n) -> [-n[2], n[1],-n[4], n[3]];
act_Z4[LL]   := (n) -> [-n[1],-n[2],-n[3],-n[4]];
act_Z4[LLL]  := (n) -> [ n[2],-n[1], n[4],-n[3]];
act_Z4[M]    := (n) -> [ n[3],-n[4], n[1],-n[2]];
act_Z4[LM]   := (n) -> [ n[4], n[3], n[2], n[1]];
act_Z4[LLM]  := (n) -> [-n[3], n[4],-n[1], n[2]];
act_Z4[LLLM] := (n) -> [-n[4],-n[3],-n[2],-n[1]];
act_Z4[N]    := (n) -> [ n[1],-n[2], n[3],-n[4]];
act_Z4[LN]   := (n) -> [ n[2], n[1], n[4], n[3]];
act_Z4[LLN]  := (n) -> [-n[1], n[2],-n[3], n[4]];
act_Z4[LLLN] := (n) -> [-n[2],-n[1],-n[4],-n[3]];
act_Z4[MN]   := (n) -> [ n[3], n[4], n[1], n[2]];
act_Z4[LMN]  := (n) -> [-n[4], n[3],-n[2], n[1]];
act_Z4[LLMN] := (n) -> [-n[3],-n[4],-n[1],-n[2]];
act_Z4[LLLMN]:= (n) -> [ n[4],-n[3], n[2],-n[1]];

# This gives an alternative basis after tensoring with the rationals.
# It is orthogonal, which is convenient for analysing the character
# of the action.

#@ homology_u
homology_u[1,1] := [ 1, 0, 1, 0]:
homology_u[1,2] := [ 0, 1, 0, 1]:
homology_u[2,1] := [ 1, 0,-1, 0]:
homology_u[2,2] := [ 0, 1, 0,-1]:

# The span of homology_u[1,1] and homology_u[1,2] is a representation
# of G with character homology_character[1].

# The span of homology_u[2,1] and homology_u[2,2] is a representation
# of G with character homology_character[2].

#@ homology_character
for T in G16 do
 for i from 1 to 2 do 
  homology_character[i][T] :=
   add(dp4(homology_u[i,j],act_Z4[T](homology_u[i,j]))/2,j in [1,2]);
 od;
od;

#@ cap_product 
cap_product := (u,v) -> u[1]*v[2]-u[2]*v[1]-u[3]*v[4]+u[4]*v[3];

# This is the coequaliser for the action of M on Z^4
#@ homology_theta_p
homology_theta_p := (n) -> [n[1]+n[3],n[2]-n[4]];

# This is the coequaliser for the action of LM on Z^4
#@ homology_theta_m
homology_theta_m := (n) -> [n[2]+n[3],n[1]+n[4]];

######################################################################

# The function check_precromulent() checks that a cromulent surface
# with a given curve system satisfies the appropriate axioms.  
#
# The input data is represented as follows.  The cromulent surface is 
# specified by a single letter E, H or P (for the embedded, hyperbolic
# and projective families).  The hyperbolic family (with a symbolic 
# rather than numeric value for the parameter a) is defined in the
# file hyperbolic/HX.mpl.  That file defines
#
#  + A function is_member_H(z) which checks whether z is an element
#    of the open unit disk.
#  + A function is_equal_H(z,w), which checks whether z and w are 
#    equal in the unit disc.  It can also be called in the form 
#    is_equal_H(z,w,p), where p is assumed to be an element of the
#    Fuchsian group Pi; this form checks whether p.z = w.
#  + A table of functions act_H[g] acting on the unit disc, for g in G.
#    These do not strictly give an action of G on the disc, but they 
#    descend to give an action on Disc/Pi.
#  + Various other things such as v_H, c_H, c_check_H,
#    is_in_F4_H, is_in_F16_H and so on.

#@ check_precromulent 
check_precromulent := proc(X::string,_stop_on_error)
 local is_member,is_equal,act,v,c,c_check,is_in_F4,is_in_F16,
       v_action_witness,c_action_witness,v_on_c_witness,
       stop_on_error,i,j,m,u,T,t,a,b;

 is_member := eval(convert(cat("is_member_",X),name));
 is_equal  := eval(convert(cat("is_equal_" ,X),name));
 act       := eval(convert(cat("act_"      ,X),name));
 v         := eval(convert(cat("v_"        ,X),name));
 c         := eval(convert(cat("c_"        ,X),name));
 c_check   := eval(convert(cat("c_check_"  ,X),name));
 is_in_F4  := eval(convert(cat("is_in_F4_" ,X),name));
 is_in_F16 := eval(convert(cat("is_in_F16_",X),name));

 v_action_witness := eval(convert(cat("v_action_witness_",X),name));
 c_action_witness := eval(convert(cat("c_action_witness_",X),name));
 v_on_c_witness   := eval(convert(cat("v_on_c_witness_"  ,X),name));

 stop_on_error := `if`(nargs > 1,_stop_on_error,true);
 assume(t::real);

 for i from 0 to 13 do
  if not(is_member(v[i])) then
   printf("v[%d] is not an element of X\n",i);
   if stop_on_error then return false; fi;
  fi;
 od;

 for i from 0 to 13 do
  for T in [L,M,N] do
   if not(is_equal(v[act_V[T](i)],act[T](v[i]),v_action_witness[T,i])) then
    printf("incorrect action of %A on v[%d]\n",T,i);
    if stop_on_error then return false; fi;
   fi;
  od;
 od;

 for i from 0 to 8 do
  if not(is_member(c[i](t))) then
   printf("c[%d](t) is not an element of X\n",i);
   if stop_on_error then return false; fi;
  fi;
 od;

 for i from 0 to 8 do
  for T in [L,M,N] do
   j,m,u := op(act_c_data[T,i]);
   if not(is_equal(c[j](m*t+u),act[T](c[i](t)),c_action_witness[T,i])) then
    printf("incorrect action of %A on c[%d](t)\n",T,i);
    if stop_on_error then return false; fi;
   fi;
  od;
 od;

 for i from 0 to 13 do
  for j from 0 to 8 do
   if v_on_c[i,j] = NULL then
    if c_check[j](v[i]) then
     printf("v[%d] should not lie on C[%d]\n",i,j);
     if stop_on_error then return false; fi;
    fi;
   else
    if not(is_equal(v[i],c[j](v_on_c[i,j]),v_on_c_witness[i,j])) then
     printf("c[%d](%A) should be equal to v[%d]\n",j,v_on_c[i,j],i);
     if stop_on_error then return false; fi;
    fi;
   fi;
  od;
 od;

 for i from 0 to 13 do
  a := member(i,F4_vertices);
  b := is_in_F4(v[i]);
  if a and not b then 
   printf("v[%d] should be in F4\n",i);
   if stop_on_error then return false; fi;
  fi;
  if b and not a then 
   printf("v[%d] should not be in F4\n",i);
   if stop_on_error then return false; fi;
  fi;
  a := member(i,F16_vertices);
  b := is_in_F16(v[i]);
  if a and not b then 
   printf("v[%d] should be in F16\n",i);
   if stop_on_error then return false; fi;
  fi;
  if b and not a then 
   printf("v[%d] should not be in F16\n",i);
   if stop_on_error then return false; fi;
  fi;
 od;

 if stop_on_error then
  return true;
 else
  return NULL;
 fi;
end:


