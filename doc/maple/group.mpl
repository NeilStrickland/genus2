<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># This file defines the group G of order 16 that acts on X, and
<a name="line_2"></a># some of its subgroups. 
<a name="line_3"></a>
<a name="line_4"></a># The identity element of G is denoted by 1.
<a name="line_5"></a># The generator lambda is called L in the Maple code.  Its powers are
<a name="line_6"></a># LL and LLL.  The generators mu and nu are M and N respectively.
<a name="line_7"></a># Thus LLMN refers to lambda^2 mu nu, for example.
<a name="line_8"></a>
<a name="line_9"></a>G4  := [1,L,LL,LLL];                       <span style="color:red">#@ G4  
</span><a name="line_10"></a>H4  := [1,LL,N,LLN];                       <span style="color:red">#@ H4  
</span><a name="line_11"></a>G8  := [1,L,LL,LLL,M,LM,LLM,LLLM];         <span style="color:red">#@ G8  
</span><a name="line_12"></a>H8  := [1,LL,M,LLM,N,LLN,MN,LLMN];         <span style="color:red">#@ H8  
</span><a name="line_13"></a>K8  := [1,LL,LM,LLLM,LN,LLLN,MN,LLMN];     <span style="color:red">#@ K8  
</span><a name="line_14"></a>L8  := [1,L,LL,LLL,N,LN,LLN,LLLN];         <span style="color:red">#@ L8  
</span><a name="line_15"></a>G16 := [1,L,LL,LLL,M,LM,LLM,LLLM,          
<a name="line_16"></a>         N,LN,LLN,LLLN,MN,LMN,LLMN,LLLMN]; <span style="color:red">#@ G16 
</span><a name="line_17"></a>
<a name="line_18"></a>protect(
<a name="line_19"></a> 'L','LL','LLL',
<a name="line_20"></a> 'M','LM','LLM','LLLM',
<a name="line_21"></a> 'N','LN','LLN','LLLN',
<a name="line_22"></a> 'MN','LMN','LLMN','LLLMN'
<a name="line_23"></a>);
<a name="line_24"></a>
<a name="line_25"></a># The table v_permlist encodes the standard action of G on {0,1,..,13}.
<a name="line_26"></a># If g.i = j, then j occurs as entry number i+1 in v_permlist[g].
<a name="line_27"></a># (The shift by 1 occurs because Maple indexes lists starting with
<a name="line_28"></a># position 1, not position 0.)
<a name="line_29"></a>
<a name="line_30"></a><span style="color:red">#@ v_permlist
</span><a name="line_31"></a>v_permlist[1]     := [0,1,2,3,4,5,6,7,8,9,10,11,12,13]:
<a name="line_32"></a>v_permlist[L]     := [0,1,3,4,5,2,7,8,9,6,11,10,13,12]:
<a name="line_33"></a>v_permlist[LL]    := [0,1,4,5,2,3,8,9,6,7,10,11,12,13]:
<a name="line_34"></a>v_permlist[LLL]   := [0,1,5,2,3,4,9,6,7,8,11,10,13,12]:
<a name="line_35"></a>v_permlist[M]     := [1,0,2,5,4,3,9,8,7,6,12,13,10,11]:
<a name="line_36"></a>v_permlist[LM]    := [1,0,3,2,5,4,6,9,8,7,13,12,11,10]:
<a name="line_37"></a>v_permlist[LLM]   := [1,0,4,3,2,5,7,6,9,8,12,13,10,11]:
<a name="line_38"></a>v_permlist[LLLM]  := [1,0,5,4,3,2,8,7,6,9,13,12,11,10]:
<a name="line_39"></a>v_permlist[N]     := [0,1,2,5,4,3,9,8,7,6,10,11,12,13]:
<a name="line_40"></a>v_permlist[LN]    := [0,1,3,2,5,4,6,9,8,7,11,10,13,12]:
<a name="line_41"></a>v_permlist[LLN]   := [0,1,4,3,2,5,7,6,9,8,10,11,12,13]:
<a name="line_42"></a>v_permlist[LLLN]  := [0,1,5,4,3,2,8,7,6,9,11,10,13,12]:
<a name="line_43"></a>v_permlist[MN]    := [1,0,2,3,4,5,6,7,8,9,12,13,10,11]:
<a name="line_44"></a>v_permlist[LMN]   := [1,0,3,4,5,2,7,8,9,6,13,12,11,10]:
<a name="line_45"></a>v_permlist[LLMN]  := [1,0,4,5,2,3,8,9,6,7,12,13,10,11]:
<a name="line_46"></a>v_permlist[LLLMN] := [1,0,5,2,3,4,9,6,7,8,13,12,11,10]:
<a name="line_47"></a>
<a name="line_48"></a># This function converts a permutation represented as above, to
<a name="line_49"></a># a list of disjoint cycles.  It is essentially the built in 
<a name="line_50"></a># function convert(-,disjcyc) with a wrapper to deal with the fact
<a name="line_51"></a># that we index vertices from 0.
<a name="line_52"></a>
<a name="line_53"></a><span style="color:red">#@ to_cycles 
</span><a name="line_54"></a>to_cycles := proc(s::list(integer)) 
<a name="line_55"></a> local s1,s2,c;
<a name="line_56"></a> s1 := map(i -> i+1,s);
<a name="line_57"></a> s2 := convert(s1,disjcyc);
<a name="line_58"></a> map(c -> map(i -> i-1,c),s2);
<a name="line_59"></a>end:
<a name="line_60"></a>
<a name="line_61"></a># This is the inverse of to_cycles.
<a name="line_62"></a>
<a name="line_63"></a><span style="color:red">#@ from_cycles 
</span><a name="line_64"></a>from_cycles := proc(s::list(list(integer)))
<a name="line_65"></a> map(i -> i-1,convert(map(c -> map(i -> i+1,c),s),permlist,14));
<a name="line_66"></a>end:
<a name="line_67"></a>
<a name="line_68"></a># This gives a different representation of the action, as a table 
<a name="line_69"></a># of functions.  If g.i = j, then act_V[g](i) wil be equal to j.
<a name="line_70"></a>
<a name="line_71"></a><span style="color:red">#@ v_cycles
</span><a name="line_72"></a><span style="color:red">#@ act_V
</span><a name="line_73"></a>for T in G16 do
<a name="line_74"></a> v_cycles[T]   := to_cycles(v_permlist[T]);
<a name="line_75"></a> act_V[T] := unapply(T,i);
<a name="line_76"></a> for j from 0 to 13 do
<a name="line_77"></a>  act_V[T](j) := v_permlist[T][j+1];
<a name="line_78"></a> od;
<a name="line_79"></a>od:
<a name="line_80"></a>
<a name="line_81"></a># We now calculate orbits and stabilisers.
<a name="line_82"></a>
<a name="line_83"></a><span style="color:red">#@ v_stabiliser_G16
</span><a name="line_84"></a><span style="color:red">#@ v_stabiliser_G8
</span><a name="line_85"></a><span style="color:red">#@ v_orbit_G16
</span><a name="line_86"></a><span style="color:red">#@ v_orbit_G8
</span><a name="line_87"></a>
<a name="line_88"></a>for i from 0 to 13 do
<a name="line_89"></a> v_stabiliser_G16[i] := select(T -> act_V[T](i)=i,G16);
<a name="line_90"></a> v_stabiliser_G8[i]  := select(T -> act_V[T](i)=i,G8);
<a name="line_91"></a> v_orbit_G16[i] := sort([op({op(map(T -> act_V[T](i),G16))})]);
<a name="line_92"></a> v_orbit_G8[i] := sort([op({op(map(T -> act_V[T](i),G8))})]);
<a name="line_93"></a>od:
<a name="line_94"></a>unassign('i'):
<a name="line_95"></a>
<a name="line_96"></a># fixed_vertices[g] is the set of indices i such that g.v[i] = v[i].
<a name="line_97"></a>
<a name="line_98"></a><span style="color:red">#@ fixed_vertices
</span><a name="line_99"></a>fixed_vertices[L]    := [0,1];
<a name="line_100"></a>fixed_vertices[LL]   := [0,1,10,11,12,13];
<a name="line_101"></a>fixed_vertices[LLL]  := [0,1];
<a name="line_102"></a>fixed_vertices[M]    := [2,4];
<a name="line_103"></a>fixed_vertices[LM]   := [6,8];
<a name="line_104"></a>fixed_vertices[LLM]  := [3,5];
<a name="line_105"></a>fixed_vertices[LLLM] := [7,9];
<a name="line_106"></a>
<a name="line_107"></a># These are generators of the group of equivariant automorphisms of V
<a name="line_108"></a>
<a name="line_109"></a><span style="color:red">#@ aut_V_phi
</span><a name="line_110"></a>aut_V_phi[0] := from_cycles([[0,1]]);
<a name="line_111"></a>aut_V_phi[1] := from_cycles([[2,4],[3,5]]);
<a name="line_112"></a>aut_V_phi[2] := from_cycles([[6,8],[7,9]]);
<a name="line_113"></a>aut_V_phi[3] := from_cycles([[10,11],[12,13]]);
<a name="line_114"></a>aut_V_phi[4] := from_cycles([[10,12],[11,13]]);
<a name="line_115"></a>
<a name="line_116"></a># The following block defines functions 
<a name="line_117"></a># G_mult and G_inv, such that G_mult(U,V) is the product of U and V
<a name="line_118"></a># in G16 (eg G_mult(M,L) = LLLM) and G_inv(U) is the inverse of U.
<a name="line_119"></a>
<a name="line_120"></a><span style="color:red">#@ G_mult
</span><a name="line_121"></a><span style="color:red">#@ G_inv
</span><a name="line_122"></a>
<a name="line_123"></a>make_cayley_table := proc()
<a name="line_124"></a> global G_mult,G_inv,G_lookup,G_conj;
<a name="line_125"></a> local o,T1,T2,T3,u,v;
<a name="line_126"></a> G_mult := proc() procname(args) end;
<a name="line_127"></a> G_inv  := proc() procname(args) end;
<a name="line_128"></a> o := proc(s,t) [seq(s[t[i+1]+1],i=0..13)]; end:
<a name="line_129"></a> 
<a name="line_130"></a> for T1 in G16 do
<a name="line_131"></a>  G_lookup[v_permlist[T1]] := T1;
<a name="line_132"></a> od;
<a name="line_133"></a> for T1 in G16 do
<a name="line_134"></a>  for T2 in G16 do
<a name="line_135"></a>   v := o(v_permlist[T1],v_permlist[T2]);
<a name="line_136"></a>   T3 := G_lookup[v];
<a name="line_137"></a>   G_mult(T1,T2) := T3;
<a name="line_138"></a>   if T3 = 1 then
<a name="line_139"></a>    G_inv(T1) := T2;
<a name="line_140"></a>   fi;
<a name="line_141"></a>  od:
<a name="line_142"></a> od:
<a name="line_143"></a>
<a name="line_144"></a> for T1 in G16 do
<a name="line_145"></a>  for T2 in G16 do
<a name="line_146"></a>   G_conj(T1,T2) := G_mult(T1,G_mult(T2,G_inv(T1)));
<a name="line_147"></a>  od:
<a name="line_148"></a> od:
<a name="line_149"></a> NULL;
<a name="line_150"></a>end:
<a name="line_151"></a>
<a name="line_152"></a>make_cayley_table();
<a name="line_153"></a>
<a name="line_154"></a># Conjugacy classes in G
<a name="line_155"></a>
<a name="line_156"></a><span style="color:red">#@ G_classes
</span><a name="line_157"></a>G_classes := [
<a name="line_158"></a> [1],
<a name="line_159"></a> [LL],
<a name="line_160"></a> [MN],
<a name="line_161"></a> [LLMN],
<a name="line_162"></a> [L,LLL],
<a name="line_163"></a> [M,LLM],
<a name="line_164"></a> [LM,LLLM],
<a name="line_165"></a> [N,LLN],
<a name="line_166"></a> [LN,LLLN],
<a name="line_167"></a> [LMN,LLLMN]
<a name="line_168"></a>];
<a name="line_169"></a>
<a name="line_170"></a># Character table of G
<a name="line_171"></a>
<a name="line_172"></a><span style="color:red">#@ characters
</span><a name="line_173"></a>characters[1     ] := [  1,  1,  1,  1,  1,  1,  1,  1,  2,  2]:
<a name="line_174"></a>characters[LL    ] := [  1,  1,  1,  1,  1,  1,  1,  1, -2, -2]:
<a name="line_175"></a>characters[MN    ] := [  1,  1, -1, -1,  1,  1, -1, -1,  2, -2]:
<a name="line_176"></a>characters[LLMN  ] := [  1,  1, -1, -1,  1,  1, -1, -1, -2,  2]:
<a name="line_177"></a>characters[L     ] := [  1, -1,  1, -1, -1,  1, -1,  1,  0,  0]:
<a name="line_178"></a>characters[LLL   ] := [  1, -1,  1, -1, -1,  1, -1,  1,  0,  0]:
<a name="line_179"></a>characters[M     ] := [  1,  1, -1, -1, -1, -1,  1,  1,  0,  0]:
<a name="line_180"></a>characters[LLM   ] := [  1,  1, -1, -1, -1, -1,  1,  1,  0,  0]:
<a name="line_181"></a>characters[N     ] := [  1,  1,  1,  1, -1, -1, -1, -1,  0,  0]:
<a name="line_182"></a>characters[LLN   ] := [  1,  1,  1,  1, -1, -1, -1, -1,  0,  0]:
<a name="line_183"></a>characters[LM    ] := [  1, -1, -1,  1,  1, -1, -1,  1,  0,  0]:
<a name="line_184"></a>characters[LLLM  ] := [  1, -1, -1,  1,  1, -1, -1,  1,  0,  0]:
<a name="line_185"></a>characters[LN    ] := [  1, -1,  1, -1,  1, -1,  1, -1,  0,  0]:
<a name="line_186"></a>characters[LLLN  ] := [  1, -1,  1, -1,  1, -1,  1, -1,  0,  0]:
<a name="line_187"></a>characters[LMN   ] := [  1, -1, -1,  1, -1,  1,  1, -1,  0,  0]:
<a name="line_188"></a>characters[LLLMN ] := [  1, -1, -1,  1, -1,  1,  1, -1,  0,  0]:
<a name="line_189"></a>
<a name="line_190"></a><span style="color:red">#@ character
</span><a name="line_191"></a>for i from 0 to 9 do
<a name="line_192"></a> for T in G16 do
<a name="line_193"></a>  character[i][T] := characters[T][i+1];
<a name="line_194"></a> od:
<a name="line_195"></a>od:
<a name="line_196"></a>
<a name="line_197"></a>unassign('i','j','T');
<a name="line_198"></a>
<a name="line_199"></a><span style="color:red">#@ G16_subgroups
</span><a name="line_200"></a><span style="color:red">#@ G16_subgroup_classes
</span><a name="line_201"></a>proc()
<a name="line_202"></a> global G16_subgroups,G16_subgroup_classes;
<a name="line_203"></a> local PG,is_subgroup,H,c,T;
<a name="line_204"></a>
<a name="line_205"></a> PG := combinat[powerset](G16):
<a name="line_206"></a>
<a name="line_207"></a> is_subgroup := proc(H)
<a name="line_208"></a>  local a,b;
<a name="line_209"></a>
<a name="line_210"></a>  if not member(1,H) then return false; fi;
<a name="line_211"></a>  for a in H do 
<a name="line_212"></a>   for b in H do
<a name="line_213"></a>    if not member(G_mult(a,b),H) then return false; fi;
<a name="line_214"></a>   od:
<a name="line_215"></a>  od:
<a name="line_216"></a>
<a name="line_217"></a>  return true;
<a name="line_218"></a> end:
<a name="line_219"></a>
<a name="line_220"></a> G16_subgroups := select(is_subgroup,PG);
<a name="line_221"></a>
<a name="line_222"></a> G16_subgroup_classes := []:
<a name="line_223"></a>
<a name="line_224"></a> for H in G16_subgroups do
<a name="line_225"></a>  c := false;
<a name="line_226"></a>  for T in G16 do 
<a name="line_227"></a>   if member(map2(G_conj,T,H),G16_subgroup_classes) then
<a name="line_228"></a>    c := true;
<a name="line_229"></a>    break;
<a name="line_230"></a>   fi;
<a name="line_231"></a>  od:
<a name="line_232"></a>  if not c then 
<a name="line_233"></a>   G16_subgroup_classes := [op(G16_subgroup_classes),H];
<a name="line_234"></a>  fi;
<a name="line_235"></a> od:
<a name="line_236"></a>
<a name="line_237"></a>end():
<a name="line_238"></a>
<a name="line_239"></a>######################################################################
<a name="line_240"></a>
<a name="line_241"></a><span style="color:red">#@ make_latex_character_table
</span><a name="line_242"></a>make_latex_character_table := proc()
<a name="line_243"></a> local GG,GGL,tex,i,k;
<a name="line_244"></a> GG := [1,LL,MN,LLMN,L,M,LM,N,LN,LMN]:
<a name="line_245"></a> GGL := ["1","\\lm^2","\\mu\\nu","\\lm^2\\mu\\nu","\\lm^{\\pm 1}","\\mu,\\lm^2\\mu",
<a name="line_246"></a>	 "\\lm^{\\pm 1}\\mu","\\nu,\\lm^2\\nu","\\lm^{\\pm 1}\\nu","\\lm^{\\pm 1}\\mu\\nu"]:
<a name="line_247"></a> tex := " \\[ \\renewcommand{\\arraystretch}{1.5}\n    \\begin{array}{|c|c|c|c|c|c|c|c|c|c|c|} \\hline\n                  ":
<a name="line_248"></a> for i from 0 to 9 do tex := cat(tex,sprintf("& \\chi_%d ",i)); od:
<a name="line_249"></a> tex := cat(tex,"\\\\ \\hline\n"):
<a name="line_250"></a> for k from 1 to 10 do
<a name="line_251"></a>  tex := cat(tex,sprintf("%17A ",GGL[k])); 
<a name="line_252"></a>  for i from 0 to 9 do
<a name="line_253"></a>   tex := cat(tex,sprintf("& %2d     ",character[i][GG[k]]));
<a name="line_254"></a>  od: 
<a name="line_255"></a>  tex := cat(tex,"\\\\ \\hline\n");
<a name="line_256"></a> od:
<a name="line_257"></a> tex := cat(tex,"   \\end{array}\n \\]\n"):
<a name="line_258"></a>
<a name="line_259"></a> save_tikz("char_table",tex);
<a name="line_260"></a>
<a name="line_261"></a>end:  </pre>
 </body>
</html>
    