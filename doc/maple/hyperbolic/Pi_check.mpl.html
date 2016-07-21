<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>check_Pi_relations := proc()
<a name="line_2"></a> local i,alpha;
<a name="line_3"></a>
<a name="line_4"></a> printf("%a()\n",procname);
<a name="line_5"></a>
<a name="line_6"></a> for i from 0 to 3 do
<a name="line_7"></a>  _ASSERT(act_on_Pi[L]([i,i+4])=[],sprintf("L preserves inverse of beta[%d]",i));
<a name="line_8"></a>  _ASSERT(act_on_Pi[M]([i,i+4])=[],sprintf("M preserves inverse of beta[%d]",i));
<a name="line_9"></a>  _ASSERT(act_on_Pi[N]([i,i+4])=[],sprintf("N preserves inverse of beta[%d]",i));
<a name="line_10"></a> od:
<a name="line_11"></a> _ASSERT(act_on_Pi[L]([0,1,2,3,4,5,6,7])=[],"L preserves the long beta relation");
<a name="line_12"></a> _ASSERT(act_on_Pi[M]([0,1,2,3,4,5,6,7])=[],"M preserves the long beta relation");
<a name="line_13"></a> _ASSERT(act_on_Pi[N]([0,1,2,3,4,5,6,7])=[],"N preserves the long beta relation");
<a name="line_14"></a> for i from 0 to 7 do
<a name="line_15"></a>  _ASSERT(
<a name="line_16"></a>   act_on_Pi[L](act_on_Pi[L](act_on_Pi[L](act_on_Pi[L]([i])))) = [i],
<a name="line_17"></a>   sprintf("LLLL=1 on beta[%d]",i)
<a name="line_18"></a>  );
<a name="line_19"></a>  _ASSERT(
<a name="line_20"></a>   act_on_Pi[M](act_on_Pi[M]([i])) = [i],
<a name="line_21"></a>   sprintf("MM=1 on beta[%d]",i)
<a name="line_22"></a>  );
<a name="line_23"></a>  _ASSERT(
<a name="line_24"></a>   act_on_Pi[N](act_on_Pi[N]([i])) = [i],
<a name="line_25"></a>   sprintf("NN=1 on beta[%d]",i)
<a name="line_26"></a>  );
<a name="line_27"></a>  _ASSERT(
<a name="line_28"></a>   act_on_Pi[L](act_on_Pi[N](act_on_Pi[L](act_on_Pi[N]([i])))) = [i],
<a name="line_29"></a>   sprintf("LNLN=1 on beta[%d]",i)
<a name="line_30"></a>  );
<a name="line_31"></a>  _ASSERT(
<a name="line_32"></a>   act_on_Pi[L](act_on_Pi[M](act_on_Pi[L](act_on_Pi[M]([i])))) = 
<a name="line_33"></a>    Pi_mult([7,6],Pi_mult([i],Pi_inv([7,6]))),
<a name="line_34"></a>   sprintf("LMLM is inner on beta[%d]",i)
<a name="line_35"></a>  );
<a name="line_36"></a>  _ASSERT(
<a name="line_37"></a>   act_on_Pi[N](act_on_Pi[M](act_on_Pi[N](act_on_Pi[M]([i])))) = 
<a name="line_38"></a>    Pi_mult([6,0,7,6],Pi_mult([i],Pi_inv([6,0,7,6]))),
<a name="line_39"></a>   sprintf("NMNM is inner on beta[%d]",i)
<a name="line_40"></a>  );
<a name="line_41"></a> od;
<a name="line_42"></a>end:
<a name="line_43"></a>
<a name="line_44"></a>add_check(check_Pi_relations):
<a name="line_45"></a>
<a name="line_46"></a>######################################################################
<a name="line_47"></a>
<a name="line_48"></a>check_Pi_alpha := proc()
<a name="line_49"></a> local i;
<a name="line_50"></a>
<a name="line_51"></a> printf("%a()\n",procname);
<a name="line_52"></a>
<a name="line_53"></a> for i from 0 to 3 do
<a name="line_54"></a>  _ASSERT(
<a name="line_55"></a>   Pi_mult(op(map(a -> `if`(a>=0,Pi_alpha[a],Pi_inv(Pi_alpha[abs(a)])),Pi_beta_alpha[i])))
<a name="line_56"></a>    = [i],
<a name="line_57"></a>   sprintf("beta[%d] in terms of Pi_alpha",i)
<a name="line_58"></a>  );
<a name="line_59"></a> od;
<a name="line_60"></a>
<a name="line_61"></a> _ASSERT(
<a name="line_62"></a>  Pi_mult(
<a name="line_63"></a>   Pi_alpha[0],
<a name="line_64"></a>   Pi_alpha[1],
<a name="line_65"></a>   Pi_inv(Pi_alpha[0]),
<a name="line_66"></a>   Pi_inv(Pi_alpha[1]),
<a name="line_67"></a>   Pi_alpha[2],
<a name="line_68"></a>   Pi_alpha[3],
<a name="line_69"></a>   Pi_inv(Pi_alpha[2]),
<a name="line_70"></a>   Pi_inv(Pi_alpha[3])
<a name="line_71"></a>  ) = [],
<a name="line_72"></a>  "alpha commutator relation"
<a name="line_73"></a> );
<a name="line_74"></a>
<a name="line_75"></a>end:
<a name="line_76"></a>
<a name="line_77"></a>add_check(check_Pi_alpha):
<a name="line_78"></a>
<a name="line_79"></a>######################################################################
<a name="line_80"></a>
<a name="line_81"></a>check_Pi_sigma := proc()
<a name="line_82"></a> local i,k;
<a name="line_83"></a>
<a name="line_84"></a> printf("%a()\n",procname);
<a name="line_85"></a>
<a name="line_86"></a> _ASSERT(
<a name="line_87"></a>  Pi_mult(Pi_sigma_beta["a", 1],Pi_sigma_beta["c", 1],Pi_sigma_beta["e", 1],Pi_sigma_beta["f", 1]) = [] and
<a name="line_88"></a>  Pi_mult(Pi_sigma_beta["b", 1],Pi_sigma_beta["e",-1],Pi_sigma_beta["b",-1],Pi_sigma_beta["a",-1]) = [] and
<a name="line_89"></a>  Pi_mult(Pi_sigma_beta["d", 1],Pi_sigma_beta["f",-1],Pi_sigma_beta["d",-1],Pi_sigma_beta["c",-1]) = [],
<a name="line_90"></a>  "sigma relations in terms of beta"
<a name="line_91"></a> );
<a name="line_92"></a>
<a name="line_93"></a> for k from 0 to 7 do 
<a name="line_94"></a>  _ASSERT(
<a name="line_95"></a>   Pi_mult(op(map(ii -> Pi_sigma_beta[op(ii)],Pi_beta_sigma[k])))=[k],
<a name="line_96"></a>   sprintf("beta[%d] in terms of sigma[j]",k)
<a name="line_97"></a>  );
<a name="line_98"></a> od;
<a name="line_99"></a>end:
<a name="line_100"></a>
<a name="line_101"></a>add_check(check_Pi_sigma):
<a name="line_102"></a>
<a name="line_103"></a>######################################################################
<a name="line_104"></a>
<a name="line_105"></a>check_Pi_tilde_s := proc()
<a name="line_106"></a> local i,k,s;
<a name="line_107"></a>
<a name="line_108"></a> printf("%a()\n",procname);
<a name="line_109"></a>
<a name="line_110"></a> _ASSERT(
<a name="line_111"></a>  `and`(seq(
<a name="line_112"></a>    evalb(Pi_tilde_mult(Pi_tilde_s[i],Pi_tilde_s[i]) = [1,[]]),
<a name="line_113"></a>     i in [0,1,3,5])),
<a name="line_114"></a>  "Reflections Pi_tilde_s[i] are involutions"
<a name="line_115"></a> );
<a name="line_116"></a>
<a name="line_117"></a> assume(s::real);
<a name="line_118"></a>
<a name="line_119"></a> _ASSERT(
<a name="line_120"></a>  {seq(simplify(act_Pi_tilde(Pi_tilde_s[k],c_H[k](s)) - c_H[k](s)),k in [0,1,3,5])} = {0},
<a name="line_121"></a>  "s[i] fixes c[i](t)"
<a name="line_122"></a> );
<a name="line_123"></a>
<a name="line_124"></a> _ASSERT(
<a name="line_125"></a>  Pi_tilde_mult(op(map(i -> Pi_tilde_s[i],[5,1,5,1,5,1,5,1]))) = [1,[]],
<a name="line_126"></a>  "s[5] s[1] has order 4"
<a name="line_127"></a> );
<a name="line_128"></a>
<a name="line_129"></a> _ASSERT(
<a name="line_130"></a>  Pi_tilde_mult(op(map(i -> Pi_tilde_s[i],[1,0,1,0]))) = [1,[]],
<a name="line_131"></a>  "s[1] s[0] has order 2"
<a name="line_132"></a> );
<a name="line_133"></a>
<a name="line_134"></a> _ASSERT(
<a name="line_135"></a>  Pi_tilde_mult(op(map(i -> Pi_tilde_s[i],[0,3,0,3]))) = [1,[]],
<a name="line_136"></a>  "s[0] s[3] has order 2"
<a name="line_137"></a> );
<a name="line_138"></a>
<a name="line_139"></a> _ASSERT(
<a name="line_140"></a>  Pi_tilde_mult(op(map(i -> Pi_tilde_s[i],[3,5,3,5]))) = [1,[]],
<a name="line_141"></a>  "s[3] s[5] has order 2"
<a name="line_142"></a> );
<a name="line_143"></a>
<a name="line_144"></a> _ASSERT(
<a name="line_145"></a>  `and`(seq(evalb(Pi_tilde_mult(op(map(i -> Pi_tilde_s[i],G_s_word[T]))) = [T,[]]),T in G16)),
<a name="line_146"></a>  "Elements of G as words in s[i]"
<a name="line_147"></a> );
<a name="line_148"></a>
<a name="line_149"></a> _ASSERT(
<a name="line_150"></a>  `and`(seq(evalb(Pi_tilde_mult(op(map(i -> Pi_tilde_s[i],beta_s_word[k]))) = [1,[k]]),k = 0..7)),
<a name="line_151"></a>  "Generators beta[j] as words in s[i]"
<a name="line_152"></a> );
<a name="line_153"></a>end:
<a name="line_154"></a>
<a name="line_155"></a>add_check(check_Pi_tilde_s):
  </pre>
 </body>
</html>
    