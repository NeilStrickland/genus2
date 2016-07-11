# This file defines the group G of order 16 that acts on X, and
# some of its subgroups. 

# The identity element of G is denoted by 1.
# The generator lambda is called L in the Maple code.  Its powers are
# LL and LLL.  The generators mu and nu are M and N respectively.
# Thus LLMN refers to lambda^2 mu nu, for example.

G4  := [1,L,LL,LLL];                       #@ G4  
H4  := [1,LL,N,LLN];                       #@ H4  
G8  := [1,L,LL,LLL,M,LM,LLM,LLLM];         #@ G8  
H8  := [1,LL,M,LLM,N,LLN,MN,LLMN];         #@ H8  
K8  := [1,LL,LM,LLLM,LN,LLLN,MN,LLMN];     #@ K8  
L8  := [1,L,LL,LLL,N,LN,LLN,LLLN];         #@ L8  
G16 := [1,L,LL,LLL,M,LM,LLM,LLLM,          
         N,LN,LLN,LLLN,MN,LMN,LLMN,LLLMN]; #@ G16 

protect(
 'L','LL','LLL',
 'M','LM','LLM','LLLM',
 'N','LN','LLN','LLLN',
 'MN','LMN','LLMN','LLLMN'
);

# The table v_permlist encodes the standard action of G on {0,1,..,13}.
# If g.i = j, then j occurs as entry number i+1 in v_permlist[g].
# (The shift by 1 occurs because Maple indexes lists starting with
# position 1, not position 0.)

#@ v_permlist
v_permlist[1]     := [0,1,2,3,4,5,6,7,8,9,10,11,12,13]:
v_permlist[L]     := [0,1,3,4,5,2,7,8,9,6,11,10,13,12]:
v_permlist[LL]    := [0,1,4,5,2,3,8,9,6,7,10,11,12,13]:
v_permlist[LLL]   := [0,1,5,2,3,4,9,6,7,8,11,10,13,12]:
v_permlist[M]     := [1,0,2,5,4,3,9,8,7,6,12,13,10,11]:
v_permlist[LM]    := [1,0,3,2,5,4,6,9,8,7,13,12,11,10]:
v_permlist[LLM]   := [1,0,4,3,2,5,7,6,9,8,12,13,10,11]:
v_permlist[LLLM]  := [1,0,5,4,3,2,8,7,6,9,13,12,11,10]:
v_permlist[N]     := [0,1,2,5,4,3,9,8,7,6,10,11,12,13]:
v_permlist[LN]    := [0,1,3,2,5,4,6,9,8,7,11,10,13,12]:
v_permlist[LLN]   := [0,1,4,3,2,5,7,6,9,8,10,11,12,13]:
v_permlist[LLLN]  := [0,1,5,4,3,2,8,7,6,9,11,10,13,12]:
v_permlist[MN]    := [1,0,2,3,4,5,6,7,8,9,12,13,10,11]:
v_permlist[LMN]   := [1,0,3,4,5,2,7,8,9,6,13,12,11,10]:
v_permlist[LLMN]  := [1,0,4,5,2,3,8,9,6,7,12,13,10,11]:
v_permlist[LLLMN] := [1,0,5,2,3,4,9,6,7,8,13,12,11,10]:

# This function converts a permutation represented as above, to
# a list of disjoint cycles.  It is essentially the built in 
# function convert(-,disjcyc) with a wrapper to deal with the fact
# that we index vertices from 0.

#@ to_cycles 
to_cycles := proc(s::list(integer)) 
 local s1,s2,c;
 s1 := map(i -> i+1,s);
 s2 := convert(s1,disjcyc);
 map(c -> map(i -> i-1,c),s2);
end:

# This is the inverse of to_cycles.

#@ from_cycles 
from_cycles := proc(s::list(list(integer)))
 map(i -> i-1,convert(map(c -> map(i -> i+1,c),s),permlist,14));
end:

# This gives a different representation of the action, as a table 
# of functions.  If g.i = j, then act_V[g](i) wil be equal to j.

#@ v_cycles
#@ act_V
for T in G16 do
 v_cycles[T]   := to_cycles(v_permlist[T]);
 act_V[T] := unapply(T,i);
 for j from 0 to 13 do
  act_V[T](j) := v_permlist[T][j+1];
 od;
od:

# We now calculate orbits and stabilisers.

#@ v_stabiliser_G16
#@ v_stabiliser_G8
#@ v_orbit_G16
#@ v_orbit_G8

for i from 0 to 13 do
 v_stabiliser_G16[i] := select(T -> act_V[T](i)=i,G16);
 v_stabiliser_G8[i]  := select(T -> act_V[T](i)=i,G8);
 v_orbit_G16[i] := sort([op({op(map(T -> act_V[T](i),G16))})]);
 v_orbit_G8[i] := sort([op({op(map(T -> act_V[T](i),G8))})]);
od:
unassign('i'):

# fixed_vertices[g] is the set of indices i such that g.v[i] = v[i].

#@ fixed_vertices
fixed_vertices[L]    := [0,1];
fixed_vertices[LL]   := [0,1,10,11,12,13];
fixed_vertices[LLL]  := [0,1];
fixed_vertices[M]    := [2,4];
fixed_vertices[LM]   := [6,8];
fixed_vertices[LLM]  := [3,5];
fixed_vertices[LLLM] := [7,9];

# These are generators of the group of equivariant automorphisms of V

#@ aut_V_phi
aut_V_phi[0] := from_cycles([[0,1]]);
aut_V_phi[1] := from_cycles([[2,4],[3,5]]);
aut_V_phi[2] := from_cycles([[6,8],[7,9]]);
aut_V_phi[3] := from_cycles([[10,11],[12,13]]);
aut_V_phi[4] := from_cycles([[10,12],[11,13]]);

# The following block defines functions 
# G_mult and G_inv, such that G_mult(U,V) is the product of U and V
# in G16 (eg G_mult(M,L) = LLLM) and G_inv(U) is the inverse of U.

#@ G_mult
#@ G_inv

make_cayley_table := proc()
 global G_mult,G_inv,G_lookup,G_conj;
 local o,T1,T2,T3,u,v;
 G_mult := proc() procname(args) end;
 G_inv  := proc() procname(args) end;
 o := proc(s,t) [seq(s[t[i+1]+1],i=0..13)]; end:
 
 for T1 in G16 do
  G_lookup[v_permlist[T1]] := T1;
 od;
 for T1 in G16 do
  for T2 in G16 do
   v := o(v_permlist[T1],v_permlist[T2]);
   T3 := G_lookup[v];
   G_mult(T1,T2) := T3;
   if T3 = 1 then
    G_inv(T1) := T2;
   fi;
  od:
 od:

 for T1 in G16 do
  for T2 in G16 do
   G_conj(T1,T2) := G_mult(T1,G_mult(T2,G_inv(T1)));
  od:
 od:
 NULL;
end:

make_cayley_table();

# Conjugacy classes in G

#@ G_classes
G_classes := [
 [1],
 [LL],
 [MN],
 [LLMN],
 [L,LLL],
 [M,LLM],
 [LM,LLLM],
 [N,LLN],
 [LN,LLLN],
 [LMN,LLLMN]
];

# Character table of G

#@ characters
characters[1     ] := [  1,  1,  1,  1,  1,  1,  1,  1,  2,  2]:
characters[LL    ] := [  1,  1,  1,  1,  1,  1,  1,  1, -2, -2]:
characters[MN    ] := [  1,  1, -1, -1,  1,  1, -1, -1,  2, -2]:
characters[LLMN  ] := [  1,  1, -1, -1,  1,  1, -1, -1, -2,  2]:
characters[L     ] := [  1, -1,  1, -1, -1,  1, -1,  1,  0,  0]:
characters[LLL   ] := [  1, -1,  1, -1, -1,  1, -1,  1,  0,  0]:
characters[M     ] := [  1,  1, -1, -1, -1, -1,  1,  1,  0,  0]:
characters[LLM   ] := [  1,  1, -1, -1, -1, -1,  1,  1,  0,  0]:
characters[N     ] := [  1,  1,  1,  1, -1, -1, -1, -1,  0,  0]:
characters[LLN   ] := [  1,  1,  1,  1, -1, -1, -1, -1,  0,  0]:
characters[LM    ] := [  1, -1, -1,  1,  1, -1, -1,  1,  0,  0]:
characters[LLLM  ] := [  1, -1, -1,  1,  1, -1, -1,  1,  0,  0]:
characters[LN    ] := [  1, -1,  1, -1,  1, -1,  1, -1,  0,  0]:
characters[LLLN  ] := [  1, -1,  1, -1,  1, -1,  1, -1,  0,  0]:
characters[LMN   ] := [  1, -1, -1,  1, -1,  1,  1, -1,  0,  0]:
characters[LLLMN ] := [  1, -1, -1,  1, -1,  1,  1, -1,  0,  0]:

#@ character
for i from 0 to 9 do
 for T in G16 do
  character[i][T] := characters[T][i+1];
 od:
od:

unassign('i','j','T');

#@ G16_subgroups
#@ G16_subgroup_classes
proc()
 global G16_subgroups,G16_subgroup_classes;
 local PG,is_subgroup,H,c,T;

 PG := combinat[powerset](G16):

 is_subgroup := proc(H)
  local a,b;

  if not member(1,H) then return false; fi;
  for a in H do 
   for b in H do
    if not member(G_mult(a,b),H) then return false; fi;
   od:
  od:

  return true;
 end:

 G16_subgroups := select(is_subgroup,PG);

 G16_subgroup_classes := []:

 for H in G16_subgroups do
  c := false;
  for T in G16 do 
   if member(map2(G_conj,T,H),G16_subgroup_classes) then
    c := true;
    break;
   fi;
  od:
  if not c then 
   G16_subgroup_classes := [op(G16_subgroup_classes),H];
  fi;
 od:

end():

######################################################################

#@ make_latex_character_table
make_latex_character_table := proc()
 local GG,GGL,tex,i,k;
 GG := [1,LL,MN,LLMN,L,M,LM,N,LN,LMN]:
 GGL := ["1","\\lm^2","\\mu\\nu","\\lm^2\\mu\\nu","\\lm^{\\pm 1}","\\mu,\\lm^2\\mu",
	 "\\lm^{\\pm 1}\\mu","\\nu,\\lm^2\\nu","\\lm^{\\pm 1}\\nu","\\lm^{\\pm 1}\\mu\\nu"]:
 tex := " \\[ \\renewcommand{\\arraystretch}{1.5}\n    \\begin{array}{|c|c|c|c|c|c|c|c|c|c|c|} \\hline\n                  ":
 for i from 0 to 9 do tex := cat(tex,sprintf("& \\chi_%d ",i)); od:
 tex := cat(tex,"\\\\ \\hline\n"):
 for k from 1 to 10 do
  tex := cat(tex,sprintf("%17A ",GGL[k])); 
  for i from 0 to 9 do
   tex := cat(tex,sprintf("& %2d     ",character[i][GG[k]]));
  od: 
  tex := cat(tex,"\\\\ \\hline\n");
 od:
 tex := cat(tex,"   \\end{array}\n \\]\n"):

 save_tikz("char_table",tex);

end: