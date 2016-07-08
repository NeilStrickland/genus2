# For any cromulent surface, we can consider the full subgroupoid of
# the fundamental groupoid whose objects are v[0],...,v[13].
# This file sets up definitions for this groupoid.  It does not
# do everything that one might like.  Ideally, we should reconcile
# the whole structure of Pi_tilde with the groupoid picture.

# The edges of F16 give four elements of the groupoid, which
# we label p,q,r and s.  These have different stabilisers,
# but in each case the group G8 = <L,M> is a comlement for
# the stabiliser, so the G8 orbits of p,q,r,s and their
# inverses give a system of generators that is invariant under
# inversion and under the action of G.

# A triple [T,g,i] represents the image under T of g^i, where
# T is an element of G and g is in {p,q,r,s} and i is in {1,-1}.

#@ Gamma_gens
Gamma_gens := [seq(seq(seq([T,g,i],i in [1,-1]),T in G8),g in ["p","q","r","s"])]:

# Stabilisers
#@ pg_stab
pg_stab  := table(["p" =  N, "q" = LLN, "r" = LN, "s" = MN]):

# p lies along C[5], q along C[3], r along C[1] and s along C[0]
#@ pg_curve
pg_curve := table(["p" =  5, "q" =   3, "r" =  1, "s" =  0]):

# Source vertices
#@ pg_src0
pg_src0  := table(["p" =  0, "q" =  11, "r" =  0, "s" =  6]):

# Target vertices
#@ pg_tgt0
pg_tgt0  := table(["p" = 11, "q" =   3, "r" =  6, "s" =  3]):

# Action of G on the set of generators
#@ pg_act
pg_act := proc(U,Tgi)
 local T,g,i,UT;

 T,g,i := op(Tgi);
 UT := G_mult(U,T);
 if not member(UT,G8) then
  UT := G_mult(UT,pg_stab[g]);
 fi;
 return [UT,g,i];
end:

# Inversion of generators
#@ pg_inv
pg_inv := (Tgi) -> [Tgi[1],Tgi[2],-Tgi[3]]:

# This function sends a generator to its source vertex
#@ pg_src
pg_src := proc(Tgi)
 local T,g,i,j;
 T,g,i := op(Tgi);
 j := `if`(i = 1,pg_src0[g],pg_tgt0[g]);
 return act_V[T](j);
end:

# This function sends a generator to its target vertex
#@ pg_src
pg_tgt := (Tgi) -> pg_src(pg_inv(Tgi)):

#@ `type/Gamma_element`
`type/Gamma_element` := proc(PP)
 local n,m,i,j;

 if not(type(PP,list)) then return false; fi;
 
 n := nops(PP);
 if not type(n,odd) then return false; fi;
 m := (n-1)/2;

 for i from 0 to m do
  j := PP[2*i+1];
  if not(type(j,integer) and j >= 0 and j < 14) then
   return false;
  fi;
 od;
 
 for i from 1 to m do 
  if pg_tgt(PP[2*i]) <> PP[2*i-1] then 
   return false;
  fi;
  if pg_src(PP[2*i]) <> PP[2*i+1] then 
   return false;
  fi;
 od:
 return true;
end:

Gamma_src := (PP) -> PP[nops(PP)]: #@ Gamma_src
Gamma_tgt := (PP) -> PP[1]:        #@ Gamma_tgt

#@ Gamma_reduce
Gamma_reduce := proc(PP)
 local n,m,i,P,QQ,changed;

 n := nops(PP);
 if not type(n,odd) then return FAIL; fi;
 m := (n-1)/2;

 QQ := [PP[1]];

 changed := false;

 for i from 1 to m do
  if nops(QQ) > 1 and PP[2*i] = pg_inv(QQ[nops(QQ)-1]) then
   QQ := [op(1..-3,QQ)];
   changed := true;
  else
   QQ := [op(QQ),PP[2*i],PP[2*i+1]];
  fi;
 od:

 if changed then
  return Gamma_reduce(QQ);
 else
  return QQ;
 fi;
end:

# This implements composition of paths.  The convention is that
# Gamma_o(P,Q) is Q followed by P, so the target of Q should be
# the source of P.  Any strictly positive number of arguments
# can be given.

#@ Gamma_o
Gamma_o   := proc() 
 local PP,QQ,RR;
 if nargs = 0 then
  return FAIL;
 elif nargs = 1 then
  return args[1];
 else
  PP := args[1];
  QQ := args[2];
  if Gamma_src(PP) <> Gamma_tgt(QQ) then
   return FAIL;
  fi;
  RR := Gamma_reduce([op(1..-2,PP),op(QQ)]);
  if nargs = 2 then
   return RR;
  else
   return Gamma_o(RR,args[3..-1]);
  fi;
 fi; 
end:

# This implements inversion of paths
#@ Gamma_inv
Gamma_inv := proc(PP)
 local n,m,i,QQ;

 n := nops(PP);
 if not type(n,odd) then return FAIL; fi;
 m := (n-1)/2;
 QQ := PP[n];

 for i from 1 to m do
  QQ := QQ,pg_inv(PP[2*(m+1-i)]),PP[2*(m-i)+1];
 od:

 return [QQ];
end:

# Gamma_id(i) is the identity path at vertex i
#@ Gamma_id
Gamma_id := (i) -> [i]:

# Our generators are represented in a slightly different form from
# general elements of Gamma.  This function performs the required
# conversion.

#@ gen_path
gen_path := (Tgi) -> [pg_tgt(Tgi),Tgi,pg_src(Tgi)]:

# This implements the action of G on Gamma
#@ Gamma_act

Gamma_act := proc(T,PP)
 local n,m,i,P,QQ;

 n := nops(PP);
 if not type(n,odd) then return FAIL; fi;
 m := (n-1)/2;
 QQ := act_V[T](PP[1]);

 for i from 1 to m do
  QQ := QQ,pg_act(T,PP[2*i]),act_V[T](PP[2*i+1]);
 od: 

 return [QQ];
end:

# Each of our generators is equivalent to a word of length three in
# some other generators, representing a path that goes around the
# opposite side of an appropriate fundamental domain.  This table
# records the details.

#@ Gamma_flip0
Gamma_flip0 := table([
 "p" = [11,[1,"q",-1], 3,[1,"s", 1], 6,[1,"r", 1], 0],
 "q" = [ 3,[1,"s", 1], 6,[1,"r", 1], 0,[1,"p",-1],11],
 "r" = [ 6,[1,"s",-1], 3,[1,"q", 1],11,[1,"p", 1], 0],
 "s" = [ 3,[1,"q", 1],11,[1,"p", 1], 0,[1,"r",-1], 6]
]):

# This function flips the k'th letter in the word PP, and then
# simplifies the result.  Each word lies on the boundary between
# two different translates of F16, so there are two ways to flip
# it; this can be controlled by supplying 0 or 1 as the third
# argument l_.  For convenience we also allow the argument k to
# be zero, in which case the original word is returned unchanged.

#@ Gamma_flip
Gamma_flip := proc(PP,k,l_)
 local T,g,i,Q,l;

 if k = 0 then return PP; fi;

 l := `if`(nargs > 2,l_,0);

 T,g,i := op(PP[2*k]);

 if l > 0 then T := G_mult(T,pg_stab[g]); fi;

 Q := Gamma_act(T,Gamma_flip0[g]);
 if i = -1 then Q := Gamma_inv(Q); fi;

 Gamma_reduce([op(1..2*k-2,PP),op(Q),op(2*k+2..-1,PP)]);
end:

#@ Gamma_flip_multiple
Gamma_flip_multiple := proc(PP,FF)
 local QQ,kl;
 QQ := PP;
 for kl in FF do QQ := Gamma_flip(QQ,op(kl)); od;
 return QQ;
end:

# The following function attempts to find a sequence FF of
# flips that shortens PP.  It will try FF of length at most r.
# It returns [FF,QQ], where QQ is the result of flipping.

#@ find_flip
find_flip := proc(PP,r := 2)
 local m,i1,i2,j1,j2,QQ;
 if r = 0 then return [[],PP]; fi;
 m := (nops(PP)-1)/2;
 for i1 from 1 to m do
  for j1 from 0 to 1 do
   QQ := find_flip(Gamma_flip(PP,i1,j1),r-1);
   if nops(QQ[2]) < nops(PP) then
    return [[[i1,j1],op(QQ[1])],QQ[2]];
   fi;
  od;
 od;
 return [[],PP];
end:

# The  following function applies find_flip() repeatedly to
# shorten PP as much as possible.  It again returns [FF,QQ],
# where FF is a flipping sequence, and QQ is the result of
# applying it.

#@ find_flips
find_flips := proc(PP,r := 2)
 local QQ,RR,EE,FF,ok;
 QQ := PP;
 FF := [];
 ok := true;
 while ok do
  EE,RR := op(find_flip(QQ,r));
  if nops(RR) < nops(QQ) then
   QQ := RR;
   FF := [op(FF),op(EE)];
  else
   ok := false;
  fi;
 od;
 return ([FF,QQ]);
end:


Gamma_u := [12, [LLLM, "p", 1], 1, [LLLM, "r", -1], 8, [LL, "r", 1], 0];

# These elements of Gamma have source = target = 0, so they correspond
# to elements of the fundamental group Pi.  In fact, Gamma_beta[i]
# corresponds to beta[i].
#@ Gamma_beta

Gamma_beta[0] := [0,[LL,"p",-1],11,[1,"p",1],0];
Gamma_beta[1] := Gamma_o(Gamma_inv(Gamma_act(LL,Gamma_u)),Gamma_u);
Gamma_beta[2] := Gamma_act(L,Gamma_beta[0]);
Gamma_beta[3] := Gamma_act(L,Gamma_beta[1]);
Gamma_beta[4] := Gamma_act(L,Gamma_beta[2]);
Gamma_beta[5] := Gamma_act(L,Gamma_beta[3]);
Gamma_beta[6] := Gamma_act(L,Gamma_beta[4]);
Gamma_beta[7] := Gamma_act(L,Gamma_beta[5]);

# Gamma_base[i] is a path from v[i] to the basepoint v[0]
#@ Gamma_base
Gamma_base[ 0] := [ 0];
Gamma_base[ 1] := [ 1,[LM, "r",-1], 6,[1,  "r", 1],0];
Gamma_base[ 2] := [ 2,[LLL,"q", 1],10,[LLL,"p", 1],0];
Gamma_base[ 3] := [ 3,[1,  "q", 1],11,[1,  "p", 1],0];
Gamma_base[ 4] := [ 4,[L,  "q", 1],10,[L,  "p", 1],0];
Gamma_base[ 5] := [ 5,[LL, "q", 1],11,[LL, "p", 1],0];
Gamma_base[ 6] := [ 6,[1,  "r", 1], 0];
Gamma_base[ 7] := [ 7,[L,  "r", 1], 0];
Gamma_base[ 8] := [ 8,[LL, "r", 1], 0];
Gamma_base[ 9] := [ 9,[LLL,"r", 1], 0];
Gamma_base[10] := [10,[L  ,"p", 1], 0];
Gamma_base[11] := [11,[1  ,"p", 1], 0];
Gamma_base[12] := [12,[LM, "p", 1], 1,[LM, "r",-1], 6,[1,"r",1], 0];
Gamma_base[13] := [13,[LLM,"p", 1], 1,[LLM,"r",-1], 7,[L,"r",1], 0];


