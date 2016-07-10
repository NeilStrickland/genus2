check_group_properties := proc()
 local T,U,V,is_subgroup,is_central,i,j,n,C,ok;

 printf("%a()\n",procname);

 _ASSERT(
  `and`(seq(G_mult(1,T) = T,T in G16)),
  "unit axiom for G16"
 );

 _ASSERT(
  `and`(seq(G_mult(T,G_inv(T)) = 1,T in G16)),
  "unit axiom for G16"
 );

 _ASSERT(
  `and`(seq(seq(member(G_mult(T,U),G16),U in G16),T in G16)),
  "closure axiom for G16"
 );

 _ASSERT(
  `and`(seq(seq(seq(
    G_mult(T,G_mult(U,V)) = G_mult(G_mult(T,U),V),
     V in G16),U in G16),T in G16)),
  "associativity axiom for G16"
 );

 _ASSERT(G_mult(G_mult(G_mult(L,L),L),L) = 1,"lambda^4=1");
 _ASSERT(G_mult(M,M) = 1,"mu^2=1");
 _ASSERT(G_mult(N,N) = 1,"nu^2=1");
 _ASSERT(G_mult(G_mult(G_mult(M,N),M),N) = 1,"(mu nu)^2=1");
 _ASSERT(G_mult(G_mult(G_mult(M,L),M),L) = 1,"(mu lambda)^2=1");
 _ASSERT(G_mult(G_mult(G_mult(N,L),N),L) = 1,"(nu lambda)^2=1");

 is_subgroup := proc(H,K)
  if {op(H)} minus {op(K)} <> {} then return false; fi;
  if not(member(1,H)) then return false; fi;
  if {seq(seq(G_mult(T,U),T in H),U in H)} <> {op(H)} then
   return false;
  fi;
  return true;
 end:

 _ASSERT(is_subgroup(G8,G16),"G8 <= G16");
 _ASSERT(is_subgroup(H8,G16),"H8 <= G16");
 _ASSERT(is_subgroup(K8,G16),"K8 <= G16");
 _ASSERT(is_subgroup(L8,G16),"L8 <= G16");
 _ASSERT(is_subgroup(H4,H8 ),"H4 <= H8 ");
 _ASSERT(is_subgroup(H4,L8 ),"H4 <= L8 ");
 _ASSERT(is_subgroup(G4,G8 ),"G4 <= G8 ");
 _ASSERT(is_subgroup(G4,L8 ),"G4 <= L8 ");

 is_central := (T) -> `and`(seq(G_conj(T,U)=U,U in G16));
 _ASSERT(select(is_central,{op(G16)}) = {1,LL,MN,LLMN},
         "centre of G16");

 n := nops(G_classes);
 ok := evalb({op(map(op,G_classes))} = {op(G16)});
 for i from 1 to n-1 do
  C := G_classes[i];
  if {op(map(T -> G_conj(T,C[1]),G16))} <> {op(C)} then
   ok := false;
  fi;
  for j from i+1 to n do
   if {op(C)} intersect {op(G_classes[j])} <> {} then
    ok := false;
   fi;
  od:
 od:

 _ASSERT(ok,"Conjugacy classes in G16");
end:

add_check(check_group_properties):

######################################################################

check_character_table := proc()
 local i,j,T,U,chi;

 printf("%a()\n",procname);

 for i from 0 to 9 do
  chi := character[i];
  if chi[1] = 1 then
   _ASSERT(
    `and`(seq(seq(chi[G_mult(T,U)] = chi[T]*chi[U],U in G16),T in G16)),
    sprintf("chi[%d] is a homomorphism G -> {1,-1}",i)
   );
  fi;

  _ASSERT(add(chi[T]^2,T in G16) = 16,
  	  sprintf("chi[%d] has norm one",i));
 od:

 for i from 0 to 9 do
  for j from i+1 to 9 do
   _ASSERT(add(character[i][T] * character[j][T],T in G16) = 0,
           sprintf("chi[%d] and chi[%d] are orthogonal",i,j));
  od;
 od;

 _ASSERT(add(character[i][1]^2,i=0..9) = 16,
         "sum of squares of character degrees");
end:

add_check(check_character_table):

######################################################################

check_aut_V := proc()
 local o,id,i,j,T;

 printf("%a()\n",procname);

 _ASSERT(
  v_stabiliser_G16[ 0] = [1,L,LL,LLL,N,LN,LLN,LLLN] and
  v_stabiliser_G16[ 1] = [1,L,LL,LLL,N,LN,LLN,LLLN] and
  v_stabiliser_G16[ 2] = [1,M,N,MN] and
  v_stabiliser_G16[ 3] = [1,LLM,LLN,MN] and
  v_stabiliser_G16[ 4] = [1,M,N,MN] and
  v_stabiliser_G16[ 5] = [1,LLM,LLN,MN] and
  v_stabiliser_G16[ 6] = [1,LM,LN,MN] and
  v_stabiliser_G16[ 7] = [1,LLLM,LLLN,MN] and
  v_stabiliser_G16[ 8] = [1,LM,LN,MN] and
  v_stabiliser_G16[ 9] = [1,LLLM,LLLN,MN] and
  v_stabiliser_G16[10] = [1,LL,N,LLN] and
  v_stabiliser_G16[11] = [1,LL,N,LLN] and
  v_stabiliser_G16[12] = [1,LL,N,LLN] and
  v_stabiliser_G16[13] = [1,LL,N,LLN],
  "stabilisers in V"
 );
 
 o := proc(s,t) [seq(s[t[i+1]+1],i=0..13)]; end:
 id := [seq(i,i=0..13)];

 _ASSERT(`and`(seq(evalb(o(aut_V_phi[i],aut_V_phi[i]) = id),i=0..4)),
         "aut_V_phi[i] is an involution");

 _ASSERT(`and`(seq(seq(evalb(o(aut_V_phi[i],aut_V_phi[j]) = o(aut_V_phi[j],aut_V_phi[i])),i=0..4),j=0..4)),
         "aut_V_phi[i] and aut_V_phi[j] commute");


 _ASSERT(`and`(seq(seq(evalb(o(aut_V_phi[i],v_permlist[T]) = o(v_permlist[T],aut_V_phi[i])),i=0..4),T in [L,N,M])),
         "aut_V_phi[i] is equivariant");
end:

add_check(check_aut_V):