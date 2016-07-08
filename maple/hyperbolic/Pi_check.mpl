check_Pi_relations := proc()
 local i,alpha;

 printf("%a()\n",procname);

 for i from 0 to 3 do
  _ASSERT(act_on_Pi[L]([i,i+4])=[],sprintf("L preserves inverse of beta[%d]",i));
  _ASSERT(act_on_Pi[M]([i,i+4])=[],sprintf("M preserves inverse of beta[%d]",i));
  _ASSERT(act_on_Pi[N]([i,i+4])=[],sprintf("N preserves inverse of beta[%d]",i));
 od:
 _ASSERT(act_on_Pi[L]([0,1,2,3,4,5,6,7])=[],"L preserves the long beta relation");
 _ASSERT(act_on_Pi[M]([0,1,2,3,4,5,6,7])=[],"M preserves the long beta relation");
 _ASSERT(act_on_Pi[N]([0,1,2,3,4,5,6,7])=[],"N preserves the long beta relation");
 for i from 0 to 7 do
  _ASSERT(
   act_on_Pi[L](act_on_Pi[L](act_on_Pi[L](act_on_Pi[L]([i])))) = [i],
   sprintf("LLLL=1 on beta[%d]",i)
  );
  _ASSERT(
   act_on_Pi[M](act_on_Pi[M]([i])) = [i],
   sprintf("MM=1 on beta[%d]",i)
  );
  _ASSERT(
   act_on_Pi[N](act_on_Pi[N]([i])) = [i],
   sprintf("NN=1 on beta[%d]",i)
  );
  _ASSERT(
   act_on_Pi[L](act_on_Pi[N](act_on_Pi[L](act_on_Pi[N]([i])))) = [i],
   sprintf("LNLN=1 on beta[%d]",i)
  );
  _ASSERT(
   act_on_Pi[L](act_on_Pi[M](act_on_Pi[L](act_on_Pi[M]([i])))) = 
    Pi_mult([7,6],Pi_mult([i],Pi_inv([7,6]))),
   sprintf("LMLM is inner on beta[%d]",i)
  );
  _ASSERT(
   act_on_Pi[N](act_on_Pi[M](act_on_Pi[N](act_on_Pi[M]([i])))) = 
    Pi_mult([6,0,7,6],Pi_mult([i],Pi_inv([6,0,7,6]))),
   sprintf("NMNM is inner on beta[%d]",i)
  );
 od;
end:

add_check(check_Pi_relations):

######################################################################

check_Pi_alpha := proc()
 local i;

 printf("%a()\n",procname);

 for i from 0 to 3 do
  _ASSERT(
   Pi_mult(op(map(a -> `if`(a>=0,Pi_alpha[a],Pi_inv(Pi_alpha[abs(a)])),Pi_beta_alpha[i])))
    = [i],
   sprintf("beta[%d] in terms of Pi_alpha",i)
  );
 od;

 _ASSERT(
  Pi_mult(
   Pi_alpha[0],
   Pi_alpha[1],
   Pi_inv(Pi_alpha[0]),
   Pi_inv(Pi_alpha[1]),
   Pi_alpha[2],
   Pi_alpha[3],
   Pi_inv(Pi_alpha[2]),
   Pi_inv(Pi_alpha[3])
  ) = [],
  "alpha commutator relation"
 );

end:

add_check(check_Pi_alpha):

######################################################################

check_Pi_sigma := proc()
 local i,k;

 printf("%a()\n",procname);

 _ASSERT(
  Pi_mult(Pi_sigma_beta["a", 1],Pi_sigma_beta["c", 1],Pi_sigma_beta["e", 1],Pi_sigma_beta["f", 1]) = [] and
  Pi_mult(Pi_sigma_beta["b", 1],Pi_sigma_beta["e",-1],Pi_sigma_beta["b",-1],Pi_sigma_beta["a",-1]) = [] and
  Pi_mult(Pi_sigma_beta["d", 1],Pi_sigma_beta["f",-1],Pi_sigma_beta["d",-1],Pi_sigma_beta["c",-1]) = [],
  "sigma relations in terms of beta"
 );

 for k from 0 to 7 do 
  _ASSERT(
   Pi_mult(op(map(ii -> Pi_sigma_beta[op(ii)],Pi_beta_sigma[k])))=[k],
   sprintf("beta[%d] in terms of sigma[j]",k)
  );
 od;
end:

add_check(check_Pi_sigma):

######################################################################

check_Pi_tilde_s := proc()
 local i,k,s;

 printf("%a()\n",procname);

 _ASSERT(
  `and`(seq(
    evalb(Pi_tilde_mult(Pi_tilde_s[i],Pi_tilde_s[i]) = [1,[]]),
     i in [0,1,3,5])),
  "Reflections Pi_tilde_s[i] are involutions"
 );

 assume(s::real);

 _ASSERT(
  {seq(simplify(act_Pi_tilde(Pi_tilde_s[k],c_H[k](s)) - c_H[k](s)),k in [0,1,3,5])} = {0},
  "s[i] fixes c[i](t)"
 );

 _ASSERT(
  Pi_tilde_mult(op(map(i -> Pi_tilde_s[i],[5,1,5,1,5,1,5,1]))) = [1,[]],
  "s[5] s[1] has order 4"
 );

 _ASSERT(
  Pi_tilde_mult(op(map(i -> Pi_tilde_s[i],[1,0,1,0]))) = [1,[]],
  "s[1] s[0] has order 2"
 );

 _ASSERT(
  Pi_tilde_mult(op(map(i -> Pi_tilde_s[i],[0,3,0,3]))) = [1,[]],
  "s[0] s[3] has order 2"
 );

 _ASSERT(
  Pi_tilde_mult(op(map(i -> Pi_tilde_s[i],[3,5,3,5]))) = [1,[]],
  "s[3] s[5] has order 2"
 );

 _ASSERT(
  `and`(seq(evalb(Pi_tilde_mult(op(map(i -> Pi_tilde_s[i],G_s_word[T]))) = [T,[]]),T in G16)),
  "Elements of G as words in s[i]"
 );

 _ASSERT(
  `and`(seq(evalb(Pi_tilde_mult(op(map(i -> Pi_tilde_s[i],beta_s_word[k]))) = [1,[k]]),k = 0..7)),
  "Generators beta[j] as words in s[i]"
 );
end:

add_check(check_Pi_tilde_s):
