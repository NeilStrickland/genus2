<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_group_properties := proc()
<a name="line_2"></a> local T,U,V,is_subgroup,is_central,i,j,n,C,ok;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> _ASSERT(
<a name="line_7"></a>  `and`(seq(G_mult(1,T) = T,T in G16)),
<a name="line_8"></a>  "unit axiom for G16"
<a name="line_9"></a> );
<a name="line_10"></a>
<a name="line_11"></a> _ASSERT(
<a name="line_12"></a>  `and`(seq(G_mult(T,G_inv(T)) = 1,T in G16)),
<a name="line_13"></a>  "unit axiom for G16"
<a name="line_14"></a> );
<a name="line_15"></a>
<a name="line_16"></a> _ASSERT(
<a name="line_17"></a>  `and`(seq(seq(member(G_mult(T,U),G16),U in G16),T in G16)),
<a name="line_18"></a>  "closure axiom for G16"
<a name="line_19"></a> );
<a name="line_20"></a>
<a name="line_21"></a> _ASSERT(
<a name="line_22"></a>  `and`(seq(seq(seq(
<a name="line_23"></a>    G_mult(T,G_mult(U,V)) = G_mult(G_mult(T,U),V),
<a name="line_24"></a>     V in G16),U in G16),T in G16)),
<a name="line_25"></a>  "associativity axiom for G16"
<a name="line_26"></a> );
<a name="line_27"></a>
<a name="line_28"></a> _ASSERT(G_mult(G_mult(G_mult(L,L),L),L) = 1,"lambda^4=1");
<a name="line_29"></a> _ASSERT(G_mult(M,M) = 1,"mu^2=1");
<a name="line_30"></a> _ASSERT(G_mult(N,N) = 1,"nu^2=1");
<a name="line_31"></a> _ASSERT(G_mult(G_mult(G_mult(M,N),M),N) = 1,"(mu nu)^2=1");
<a name="line_32"></a> _ASSERT(G_mult(G_mult(G_mult(M,L),M),L) = 1,"(mu lambda)^2=1");
<a name="line_33"></a> _ASSERT(G_mult(G_mult(G_mult(N,L),N),L) = 1,"(nu lambda)^2=1");
<a name="line_34"></a>
<a name="line_35"></a> is_subgroup := proc(H,K)
<a name="line_36"></a>  if {op(H)} minus {op(K)} <> {} then return false; fi;
<a name="line_37"></a>  if not(member(1,H)) then return false; fi;
<a name="line_38"></a>  if {seq(seq(G_mult(T,U),T in H),U in H)} <> {op(H)} then
<a name="line_39"></a>   return false;
<a name="line_40"></a>  fi;
<a name="line_41"></a>  return true;
<a name="line_42"></a> end:
<a name="line_43"></a>
<a name="line_44"></a> _ASSERT(is_subgroup(G8,G16),"G8 <= G16");
<a name="line_45"></a> _ASSERT(is_subgroup(H8,G16),"H8 <= G16");
<a name="line_46"></a> _ASSERT(is_subgroup(K8,G16),"K8 <= G16");
<a name="line_47"></a> _ASSERT(is_subgroup(L8,G16),"L8 <= G16");
<a name="line_48"></a> _ASSERT(is_subgroup(H4,H8 ),"H4 <= H8 ");
<a name="line_49"></a> _ASSERT(is_subgroup(H4,L8 ),"H4 <= L8 ");
<a name="line_50"></a> _ASSERT(is_subgroup(G4,G8 ),"G4 <= G8 ");
<a name="line_51"></a> _ASSERT(is_subgroup(G4,L8 ),"G4 <= L8 ");
<a name="line_52"></a>
<a name="line_53"></a> is_central := (T) -> `and`(seq(G_conj(T,U)=U,U in G16));
<a name="line_54"></a> _ASSERT(select(is_central,{op(G16)}) = {1,LL,MN,LLMN},
<a name="line_55"></a>         "centre of G16");
<a name="line_56"></a>
<a name="line_57"></a> n := nops(G_classes);
<a name="line_58"></a> ok := evalb({op(map(op,G_classes))} = {op(G16)});
<a name="line_59"></a> for i from 1 to n-1 do
<a name="line_60"></a>  C := G_classes[i];
<a name="line_61"></a>  if {op(map(T -> G_conj(T,C[1]),G16))} <> {op(C)} then
<a name="line_62"></a>   ok := false;
<a name="line_63"></a>  fi;
<a name="line_64"></a>  for j from i+1 to n do
<a name="line_65"></a>   if {op(C)} intersect {op(G_classes[j])} <> {} then
<a name="line_66"></a>    ok := false;
<a name="line_67"></a>   fi;
<a name="line_68"></a>  od:
<a name="line_69"></a> od:
<a name="line_70"></a>
<a name="line_71"></a> _ASSERT(ok,"Conjugacy classes in G16");
<a name="line_72"></a>end:
<a name="line_73"></a>
<a name="line_74"></a>add_check(check_group_properties):
<a name="line_75"></a>
<a name="line_76"></a>######################################################################
<a name="line_77"></a>
<a name="line_78"></a>check_character_table := proc()
<a name="line_79"></a> local i,j,T,U,chi;
<a name="line_80"></a>
<a name="line_81"></a> printf("%a()\n",procname);
<a name="line_82"></a>
<a name="line_83"></a> for i from 0 to 9 do
<a name="line_84"></a>  chi := character[i];
<a name="line_85"></a>  if chi[1] = 1 then
<a name="line_86"></a>   _ASSERT(
<a name="line_87"></a>    `and`(seq(seq(chi[G_mult(T,U)] = chi[T]*chi[U],U in G16),T in G16)),
<a name="line_88"></a>    sprintf("chi[%d] is a homomorphism G -> {1,-1}",i)
<a name="line_89"></a>   );
<a name="line_90"></a>  fi;
<a name="line_91"></a>
<a name="line_92"></a>  _ASSERT(add(chi[T]^2,T in G16) = 16,
<a name="line_93"></a>  	  sprintf("chi[%d] has norm one",i));
<a name="line_94"></a> od:
<a name="line_95"></a>
<a name="line_96"></a> for i from 0 to 9 do
<a name="line_97"></a>  for j from i+1 to 9 do
<a name="line_98"></a>   _ASSERT(add(character[i][T] * character[j][T],T in G16) = 0,
<a name="line_99"></a>           sprintf("chi[%d] and chi[%d] are orthogonal",i,j));
<a name="line_100"></a>  od;
<a name="line_101"></a> od;
<a name="line_102"></a>
<a name="line_103"></a> _ASSERT(add(character[i][1]^2,i=0..9) = 16,
<a name="line_104"></a>         "sum of squares of character degrees");
<a name="line_105"></a>end:
<a name="line_106"></a>
<a name="line_107"></a>add_check(check_character_table):
<a name="line_108"></a>
<a name="line_109"></a>######################################################################
<a name="line_110"></a>
<a name="line_111"></a>check_aut_V := proc()
<a name="line_112"></a> local o,id,i,j,T;
<a name="line_113"></a>
<a name="line_114"></a> printf("%a()\n",procname);
<a name="line_115"></a>
<a name="line_116"></a> _ASSERT(
<a name="line_117"></a>  v_stabiliser_G16[ 0] = [1,L,LL,LLL,N,LN,LLN,LLLN] and
<a name="line_118"></a>  v_stabiliser_G16[ 1] = [1,L,LL,LLL,N,LN,LLN,LLLN] and
<a name="line_119"></a>  v_stabiliser_G16[ 2] = [1,M,N,MN] and
<a name="line_120"></a>  v_stabiliser_G16[ 3] = [1,LLM,LLN,MN] and
<a name="line_121"></a>  v_stabiliser_G16[ 4] = [1,M,N,MN] and
<a name="line_122"></a>  v_stabiliser_G16[ 5] = [1,LLM,LLN,MN] and
<a name="line_123"></a>  v_stabiliser_G16[ 6] = [1,LM,LN,MN] and
<a name="line_124"></a>  v_stabiliser_G16[ 7] = [1,LLLM,LLLN,MN] and
<a name="line_125"></a>  v_stabiliser_G16[ 8] = [1,LM,LN,MN] and
<a name="line_126"></a>  v_stabiliser_G16[ 9] = [1,LLLM,LLLN,MN] and
<a name="line_127"></a>  v_stabiliser_G16[10] = [1,LL,N,LLN] and
<a name="line_128"></a>  v_stabiliser_G16[11] = [1,LL,N,LLN] and
<a name="line_129"></a>  v_stabiliser_G16[12] = [1,LL,N,LLN] and
<a name="line_130"></a>  v_stabiliser_G16[13] = [1,LL,N,LLN],
<a name="line_131"></a>  "stabilisers in V"
<a name="line_132"></a> );
<a name="line_133"></a> 
<a name="line_134"></a> o := proc(s,t) [seq(s[t[i+1]+1],i=0..13)]; end:
<a name="line_135"></a> id := [seq(i,i=0..13)];
<a name="line_136"></a>
<a name="line_137"></a> _ASSERT(`and`(seq(evalb(o(aut_V_phi[i],aut_V_phi[i]) = id),i=0..4)),
<a name="line_138"></a>         "aut_V_phi[i] is an involution");
<a name="line_139"></a>
<a name="line_140"></a> _ASSERT(`and`(seq(seq(evalb(o(aut_V_phi[i],aut_V_phi[j]) = o(aut_V_phi[j],aut_V_phi[i])),i=0..4),j=0..4)),
<a name="line_141"></a>         "aut_V_phi[i] and aut_V_phi[j] commute");
<a name="line_142"></a>
<a name="line_143"></a>
<a name="line_144"></a> _ASSERT(`and`(seq(seq(evalb(o(aut_V_phi[i],v_permlist[T]) = o(v_permlist[T],aut_V_phi[i])),i=0..4),T in [L,N,M])),
<a name="line_145"></a>         "aut_V_phi[i] is equivariant");
<a name="line_146"></a>end:
<a name="line_147"></a>
<a name="line_148"></a>add_check(check_aut_V):  </pre>
 </body>
</html>
    