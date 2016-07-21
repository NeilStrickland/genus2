<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># For any cromulent surface, we can consider the full subgroupoid of
<a name="line_2"></a># the fundamental groupoid whose objects are v[0],...,v[13].
<a name="line_3"></a># This file sets up definitions for this groupoid.  It does not
<a name="line_4"></a># do everything that one might like.  Ideally, we should reconcile
<a name="line_5"></a># the whole structure of Pi_tilde with the groupoid picture.
<a name="line_6"></a>
<a name="line_7"></a># The edges of F16 give four elements of the groupoid, which
<a name="line_8"></a># we label p,q,r and s.  These have different stabilisers,
<a name="line_9"></a># but in each case the group G8 = <L,M> is a comlement for
<a name="line_10"></a># the stabiliser, so the G8 orbits of p,q,r,s and their
<a name="line_11"></a># inverses give a system of generators that is invariant under
<a name="line_12"></a># inversion and under the action of G.
<a name="line_13"></a>
<a name="line_14"></a># A triple [T,g,i] represents the image under T of g^i, where
<a name="line_15"></a># T is an element of G and g is in {p,q,r,s} and i is in {1,-1}.
<a name="line_16"></a>
<a name="line_17"></a><span style="color:red">#@ Gamma_gens
</span><a name="line_18"></a>Gamma_gens := [seq(seq(seq([T,g,i],i in [1,-1]),T in G8),g in ["p","q","r","s"])]:
<a name="line_19"></a>
<a name="line_20"></a># Stabilisers
<a name="line_21"></a><span style="color:red">#@ pg_stab
</span><a name="line_22"></a>pg_stab  := table(["p" =  N, "q" = LLN, "r" = LN, "s" = MN]):
<a name="line_23"></a>
<a name="line_24"></a># p lies along C[5], q along C[3], r along C[1] and s along C[0]
<a name="line_25"></a><span style="color:red">#@ pg_curve
</span><a name="line_26"></a>pg_curve := table(["p" =  5, "q" =   3, "r" =  1, "s" =  0]):
<a name="line_27"></a>
<a name="line_28"></a># Source vertices
<a name="line_29"></a><span style="color:red">#@ pg_src0
</span><a name="line_30"></a>pg_src0  := table(["p" =  0, "q" =  11, "r" =  0, "s" =  6]):
<a name="line_31"></a>
<a name="line_32"></a># Target vertices
<a name="line_33"></a><span style="color:red">#@ pg_tgt0
</span><a name="line_34"></a>pg_tgt0  := table(["p" = 11, "q" =   3, "r" =  6, "s" =  3]):
<a name="line_35"></a>
<a name="line_36"></a># Action of G on the set of generators
<a name="line_37"></a><span style="color:red">#@ pg_act
</span><a name="line_38"></a>pg_act := proc(U,Tgi)
<a name="line_39"></a> local T,g,i,UT;
<a name="line_40"></a>
<a name="line_41"></a> T,g,i := op(Tgi);
<a name="line_42"></a> UT := G_mult(U,T);
<a name="line_43"></a> if not member(UT,G8) then
<a name="line_44"></a>  UT := G_mult(UT,pg_stab[g]);
<a name="line_45"></a> fi;
<a name="line_46"></a> return [UT,g,i];
<a name="line_47"></a>end:
<a name="line_48"></a>
<a name="line_49"></a># Inversion of generators
<a name="line_50"></a><span style="color:red">#@ pg_inv
</span><a name="line_51"></a>pg_inv := (Tgi) -> [Tgi[1],Tgi[2],-Tgi[3]]:
<a name="line_52"></a>
<a name="line_53"></a># This function sends a generator to its source vertex
<a name="line_54"></a><span style="color:red">#@ pg_src
</span><a name="line_55"></a>pg_src := proc(Tgi)
<a name="line_56"></a> local T,g,i,j;
<a name="line_57"></a> T,g,i := op(Tgi);
<a name="line_58"></a> j := `if`(i = 1,pg_src0[g],pg_tgt0[g]);
<a name="line_59"></a> return act_V[T](j);
<a name="line_60"></a>end:
<a name="line_61"></a>
<a name="line_62"></a># This function sends a generator to its target vertex
<a name="line_63"></a><span style="color:red">#@ pg_src
</span><a name="line_64"></a>pg_tgt := (Tgi) -> pg_src(pg_inv(Tgi)):
<a name="line_65"></a>
<a name="line_66"></a><span style="color:red">#@ `type/Gamma_element`
</span><a name="line_67"></a>`type/Gamma_element` := proc(PP)
<a name="line_68"></a> local n,m,i,j;
<a name="line_69"></a>
<a name="line_70"></a> if not(type(PP,list)) then return false; fi;
<a name="line_71"></a> 
<a name="line_72"></a> n := nops(PP);
<a name="line_73"></a> if not type(n,odd) then return false; fi;
<a name="line_74"></a> m := (n-1)/2;
<a name="line_75"></a>
<a name="line_76"></a> for i from 0 to m do
<a name="line_77"></a>  j := PP[2*i+1];
<a name="line_78"></a>  if not(type(j,integer) and j >= 0 and j < 14) then
<a name="line_79"></a>   return false;
<a name="line_80"></a>  fi;
<a name="line_81"></a> od;
<a name="line_82"></a> 
<a name="line_83"></a> for i from 1 to m do 
<a name="line_84"></a>  if pg_tgt(PP[2*i]) <> PP[2*i-1] then 
<a name="line_85"></a>   return false;
<a name="line_86"></a>  fi;
<a name="line_87"></a>  if pg_src(PP[2*i]) <> PP[2*i+1] then 
<a name="line_88"></a>   return false;
<a name="line_89"></a>  fi;
<a name="line_90"></a> od:
<a name="line_91"></a> return true;
<a name="line_92"></a>end:
<a name="line_93"></a>
<a name="line_94"></a>Gamma_src := (PP) -> PP[nops(PP)]: <span style="color:red">#@ Gamma_src
</span><a name="line_95"></a>Gamma_tgt := (PP) -> PP[1]:        <span style="color:red">#@ Gamma_tgt
</span><a name="line_96"></a>
<a name="line_97"></a><span style="color:red">#@ Gamma_reduce
</span><a name="line_98"></a>Gamma_reduce := proc(PP)
<a name="line_99"></a> local n,m,i,P,QQ,changed;
<a name="line_100"></a>
<a name="line_101"></a> n := nops(PP);
<a name="line_102"></a> if not type(n,odd) then return FAIL; fi;
<a name="line_103"></a> m := (n-1)/2;
<a name="line_104"></a>
<a name="line_105"></a> QQ := [PP[1]];
<a name="line_106"></a>
<a name="line_107"></a> changed := false;
<a name="line_108"></a>
<a name="line_109"></a> for i from 1 to m do
<a name="line_110"></a>  if nops(QQ) > 1 and PP[2*i] = pg_inv(QQ[nops(QQ)-1]) then
<a name="line_111"></a>   QQ := [op(1..-3,QQ)];
<a name="line_112"></a>   changed := true;
<a name="line_113"></a>  else
<a name="line_114"></a>   QQ := [op(QQ),PP[2*i],PP[2*i+1]];
<a name="line_115"></a>  fi;
<a name="line_116"></a> od:
<a name="line_117"></a>
<a name="line_118"></a> if changed then
<a name="line_119"></a>  return Gamma_reduce(QQ);
<a name="line_120"></a> else
<a name="line_121"></a>  return QQ;
<a name="line_122"></a> fi;
<a name="line_123"></a>end:
<a name="line_124"></a>
<a name="line_125"></a># This implements composition of paths.  The convention is that
<a name="line_126"></a># Gamma_o(P,Q) is Q followed by P, so the target of Q should be
<a name="line_127"></a># the source of P.  Any strictly positive number of arguments
<a name="line_128"></a># can be given.
<a name="line_129"></a>
<a name="line_130"></a><span style="color:red">#@ Gamma_o
</span><a name="line_131"></a>Gamma_o   := proc() 
<a name="line_132"></a> local PP,QQ,RR;
<a name="line_133"></a> if nargs = 0 then
<a name="line_134"></a>  return FAIL;
<a name="line_135"></a> elif nargs = 1 then
<a name="line_136"></a>  return args[1];
<a name="line_137"></a> else
<a name="line_138"></a>  PP := args[1];
<a name="line_139"></a>  QQ := args[2];
<a name="line_140"></a>  if Gamma_src(PP) <> Gamma_tgt(QQ) then
<a name="line_141"></a>   return FAIL;
<a name="line_142"></a>  fi;
<a name="line_143"></a>  RR := Gamma_reduce([op(1..-2,PP),op(QQ)]);
<a name="line_144"></a>  if nargs = 2 then
<a name="line_145"></a>   return RR;
<a name="line_146"></a>  else
<a name="line_147"></a>   return Gamma_o(RR,args[3..-1]);
<a name="line_148"></a>  fi;
<a name="line_149"></a> fi; 
<a name="line_150"></a>end:
<a name="line_151"></a>
<a name="line_152"></a># This implements inversion of paths
<a name="line_153"></a><span style="color:red">#@ Gamma_inv
</span><a name="line_154"></a>Gamma_inv := proc(PP)
<a name="line_155"></a> local n,m,i,QQ;
<a name="line_156"></a>
<a name="line_157"></a> n := nops(PP);
<a name="line_158"></a> if not type(n,odd) then return FAIL; fi;
<a name="line_159"></a> m := (n-1)/2;
<a name="line_160"></a> QQ := PP[n];
<a name="line_161"></a>
<a name="line_162"></a> for i from 1 to m do
<a name="line_163"></a>  QQ := QQ,pg_inv(PP[2*(m+1-i)]),PP[2*(m-i)+1];
<a name="line_164"></a> od:
<a name="line_165"></a>
<a name="line_166"></a> return [QQ];
<a name="line_167"></a>end:
<a name="line_168"></a>
<a name="line_169"></a># Gamma_id(i) is the identity path at vertex i
<a name="line_170"></a><span style="color:red">#@ Gamma_id
</span><a name="line_171"></a>Gamma_id := (i) -> [i]:
<a name="line_172"></a>
<a name="line_173"></a># Our generators are represented in a slightly different form from
<a name="line_174"></a># general elements of Gamma.  This function performs the required
<a name="line_175"></a># conversion.
<a name="line_176"></a>
<a name="line_177"></a><span style="color:red">#@ gen_path
</span><a name="line_178"></a>gen_path := (Tgi) -> [pg_tgt(Tgi),Tgi,pg_src(Tgi)]:
<a name="line_179"></a>
<a name="line_180"></a># This implements the action of G on Gamma
<a name="line_181"></a><span style="color:red">#@ Gamma_act
</span><a name="line_182"></a>
<a name="line_183"></a>Gamma_act := proc(T,PP)
<a name="line_184"></a> local n,m,i,P,QQ;
<a name="line_185"></a>
<a name="line_186"></a> n := nops(PP);
<a name="line_187"></a> if not type(n,odd) then return FAIL; fi;
<a name="line_188"></a> m := (n-1)/2;
<a name="line_189"></a> QQ := act_V[T](PP[1]);
<a name="line_190"></a>
<a name="line_191"></a> for i from 1 to m do
<a name="line_192"></a>  QQ := QQ,pg_act(T,PP[2*i]),act_V[T](PP[2*i+1]);
<a name="line_193"></a> od: 
<a name="line_194"></a>
<a name="line_195"></a> return [QQ];
<a name="line_196"></a>end:
<a name="line_197"></a>
<a name="line_198"></a># Each of our generators is equivalent to a word of length three in
<a name="line_199"></a># some other generators, representing a path that goes around the
<a name="line_200"></a># opposite side of an appropriate fundamental domain.  This table
<a name="line_201"></a># records the details.
<a name="line_202"></a>
<a name="line_203"></a><span style="color:red">#@ Gamma_flip0
</span><a name="line_204"></a>Gamma_flip0 := table([
<a name="line_205"></a> "p" = [11,[1,"q",-1], 3,[1,"s", 1], 6,[1,"r", 1], 0],
<a name="line_206"></a> "q" = [ 3,[1,"s", 1], 6,[1,"r", 1], 0,[1,"p",-1],11],
<a name="line_207"></a> "r" = [ 6,[1,"s",-1], 3,[1,"q", 1],11,[1,"p", 1], 0],
<a name="line_208"></a> "s" = [ 3,[1,"q", 1],11,[1,"p", 1], 0,[1,"r",-1], 6]
<a name="line_209"></a>]):
<a name="line_210"></a>
<a name="line_211"></a># This function flips the k'th letter in the word PP, and then
<a name="line_212"></a># simplifies the result.  Each word lies on the boundary between
<a name="line_213"></a># two different translates of F16, so there are two ways to flip
<a name="line_214"></a># it; this can be controlled by supplying 0 or 1 as the third
<a name="line_215"></a># argument l_.  For convenience we also allow the argument k to
<a name="line_216"></a># be zero, in which case the original word is returned unchanged.
<a name="line_217"></a>
<a name="line_218"></a><span style="color:red">#@ Gamma_flip
</span><a name="line_219"></a>Gamma_flip := proc(PP,k,l_)
<a name="line_220"></a> local T,g,i,Q,l;
<a name="line_221"></a>
<a name="line_222"></a> if k = 0 then return PP; fi;
<a name="line_223"></a>
<a name="line_224"></a> l := `if`(nargs > 2,l_,0);
<a name="line_225"></a>
<a name="line_226"></a> T,g,i := op(PP[2*k]);
<a name="line_227"></a>
<a name="line_228"></a> if l > 0 then T := G_mult(T,pg_stab[g]); fi;
<a name="line_229"></a>
<a name="line_230"></a> Q := Gamma_act(T,Gamma_flip0[g]);
<a name="line_231"></a> if i = -1 then Q := Gamma_inv(Q); fi;
<a name="line_232"></a>
<a name="line_233"></a> Gamma_reduce([op(1..2*k-2,PP),op(Q),op(2*k+2..-1,PP)]);
<a name="line_234"></a>end:
<a name="line_235"></a>
<a name="line_236"></a><span style="color:red">#@ Gamma_flip_multiple
</span><a name="line_237"></a>Gamma_flip_multiple := proc(PP,FF)
<a name="line_238"></a> local QQ,kl;
<a name="line_239"></a> QQ := PP;
<a name="line_240"></a> for kl in FF do QQ := Gamma_flip(QQ,op(kl)); od;
<a name="line_241"></a> return QQ;
<a name="line_242"></a>end:
<a name="line_243"></a>
<a name="line_244"></a># The following function attempts to find a sequence FF of
<a name="line_245"></a># flips that shortens PP.  It will try FF of length at most r.
<a name="line_246"></a># It returns [FF,QQ], where QQ is the result of flipping.
<a name="line_247"></a>
<a name="line_248"></a><span style="color:red">#@ find_flip
</span><a name="line_249"></a>find_flip := proc(PP,r := 2)
<a name="line_250"></a> local m,i1,i2,j1,j2,QQ;
<a name="line_251"></a> if r = 0 then return [[],PP]; fi;
<a name="line_252"></a> m := (nops(PP)-1)/2;
<a name="line_253"></a> for i1 from 1 to m do
<a name="line_254"></a>  for j1 from 0 to 1 do
<a name="line_255"></a>   QQ := find_flip(Gamma_flip(PP,i1,j1),r-1);
<a name="line_256"></a>   if nops(QQ[2]) < nops(PP) then
<a name="line_257"></a>    return [[[i1,j1],op(QQ[1])],QQ[2]];
<a name="line_258"></a>   fi;
<a name="line_259"></a>  od;
<a name="line_260"></a> od;
<a name="line_261"></a> return [[],PP];
<a name="line_262"></a>end:
<a name="line_263"></a>
<a name="line_264"></a># The  following function applies find_flip() repeatedly to
<a name="line_265"></a># shorten PP as much as possible.  It again returns [FF,QQ],
<a name="line_266"></a># where FF is a flipping sequence, and QQ is the result of
<a name="line_267"></a># applying it.
<a name="line_268"></a>
<a name="line_269"></a><span style="color:red">#@ find_flips
</span><a name="line_270"></a>find_flips := proc(PP,r := 2)
<a name="line_271"></a> local QQ,RR,EE,FF,ok;
<a name="line_272"></a> QQ := PP;
<a name="line_273"></a> FF := [];
<a name="line_274"></a> ok := true;
<a name="line_275"></a> while ok do
<a name="line_276"></a>  EE,RR := op(find_flip(QQ,r));
<a name="line_277"></a>  if nops(RR) < nops(QQ) then
<a name="line_278"></a>   QQ := RR;
<a name="line_279"></a>   FF := [op(FF),op(EE)];
<a name="line_280"></a>  else
<a name="line_281"></a>   ok := false;
<a name="line_282"></a>  fi;
<a name="line_283"></a> od;
<a name="line_284"></a> return ([FF,QQ]);
<a name="line_285"></a>end:
<a name="line_286"></a>
<a name="line_287"></a>
<a name="line_288"></a>Gamma_u := [12, [LLLM, "p", 1], 1, [LLLM, "r", -1], 8, [LL, "r", 1], 0];
<a name="line_289"></a>
<a name="line_290"></a># These elements of Gamma have source = target = 0, so they correspond
<a name="line_291"></a># to elements of the fundamental group Pi.  In fact, Gamma_beta[i]
<a name="line_292"></a># corresponds to beta[i].
<a name="line_293"></a><span style="color:red">#@ Gamma_beta
</span><a name="line_294"></a>
<a name="line_295"></a>Gamma_beta[0] := [0,[LL,"p",-1],11,[1,"p",1],0];
<a name="line_296"></a>Gamma_beta[1] := Gamma_o(Gamma_inv(Gamma_act(LL,Gamma_u)),Gamma_u);
<a name="line_297"></a>Gamma_beta[2] := Gamma_act(L,Gamma_beta[0]);
<a name="line_298"></a>Gamma_beta[3] := Gamma_act(L,Gamma_beta[1]);
<a name="line_299"></a>Gamma_beta[4] := Gamma_act(L,Gamma_beta[2]);
<a name="line_300"></a>Gamma_beta[5] := Gamma_act(L,Gamma_beta[3]);
<a name="line_301"></a>Gamma_beta[6] := Gamma_act(L,Gamma_beta[4]);
<a name="line_302"></a>Gamma_beta[7] := Gamma_act(L,Gamma_beta[5]);
<a name="line_303"></a>
<a name="line_304"></a># Gamma_base[i] is a path from v[i] to the basepoint v[0]
<a name="line_305"></a><span style="color:red">#@ Gamma_base
</span><a name="line_306"></a>Gamma_base[ 0] := [ 0];
<a name="line_307"></a>Gamma_base[ 1] := [ 1,[LM, "r",-1], 6,[1,  "r", 1],0];
<a name="line_308"></a>Gamma_base[ 2] := [ 2,[LLL,"q", 1],10,[LLL,"p", 1],0];
<a name="line_309"></a>Gamma_base[ 3] := [ 3,[1,  "q", 1],11,[1,  "p", 1],0];
<a name="line_310"></a>Gamma_base[ 4] := [ 4,[L,  "q", 1],10,[L,  "p", 1],0];
<a name="line_311"></a>Gamma_base[ 5] := [ 5,[LL, "q", 1],11,[LL, "p", 1],0];
<a name="line_312"></a>Gamma_base[ 6] := [ 6,[1,  "r", 1], 0];
<a name="line_313"></a>Gamma_base[ 7] := [ 7,[L,  "r", 1], 0];
<a name="line_314"></a>Gamma_base[ 8] := [ 8,[LL, "r", 1], 0];
<a name="line_315"></a>Gamma_base[ 9] := [ 9,[LLL,"r", 1], 0];
<a name="line_316"></a>Gamma_base[10] := [10,[L  ,"p", 1], 0];
<a name="line_317"></a>Gamma_base[11] := [11,[1  ,"p", 1], 0];
<a name="line_318"></a>Gamma_base[12] := [12,[LM, "p", 1], 1,[LM, "r",-1], 6,[1,"r",1], 0];
<a name="line_319"></a>Gamma_base[13] := [13,[LLM,"p", 1], 1,[LLM,"r",-1], 7,[L,"r",1], 0];
<a name="line_320"></a>
<a name="line_321"></a>
  </pre>
 </body>
</html>
    