check_groupoid := proc()
 local GG,PP,FF,i,g;

 printf("%a()\n",procname);

 GG := [
  seq(gen_path(U),U in Gamma_gens),
  seq(Gamma_beta[i],i = 0 .. 7),
  seq(Gamma_base[i],i = 0 .. 13)
 ]:

 _ASSERT(
  `and`(op(map(type,GG,Gamma_element))),
  "all named elements lie in Gamma"
 );

 _ASSERT(
  `and`(seq(evalb(Gamma_o(g,Gamma_inv(g)) = Gamma_id(Gamma_tgt(g))),g in GG)),
  "inverses of named elements a"
 );

 _ASSERT(
  `and`(seq(evalb(Gamma_o(Gamma_inv(g),g) = Gamma_id(Gamma_src(g))),g in GG)),
  "inverses of named elements b"
 );

 _ASSERT(
  `and`(seq(evalb(Gamma_src(Gamma_base[i]) = 0) and
            evalb(Gamma_tgt(Gamma_base[i]) = i),i=0..13)),
  "Gamma_base[i] goes from v[0] to v[i]"
 );

 _ASSERT(
  `and`(seq(evalb(Gamma_src(Gamma_beta[i]) = 0) and
            evalb(Gamma_tgt(Gamma_beta[i]) = 0),i=0..7)),
  "Gamma_beta[i] is a loop based at v[0]"
 );

 _ASSERT(
  `and`(seq(evalb(Gamma_o(Gamma_beta[i],Gamma_beta[modp(i+4,8)]) = Gamma_id(0)),i = 0..7)),
  "beta[i+4] is inverse to beta[i]"
 );

 PP := Gamma_o(Gamma_beta[0],Gamma_beta[1],Gamma_beta[2],Gamma_beta[3],
               Gamma_beta[4],Gamma_beta[5],Gamma_beta[6],Gamma_beta[7]):
 FF := [[6,0],[7,1],[7,0],[5,1],[6,0],[7,1],[7,0],[5,1],
        [6,0],[7,1],[7,0],[5,1],[2,0],[3,1],[3,0],[1,1]];
 PP := Gamma_flip_multiple(PP,FF);
 _ASSERT(PP = Gamma_id(0),
  "composite of all beta[i] is the identity"
 );

end:

add_check(check_groupoid):