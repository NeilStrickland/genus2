#@ cayley 
cayley := (x) -> [
 x[4] + x[3]/sqrt(2) + x[1]*sqrt(-2),
 x[4] + x[3]/sqrt(2) - x[1]*sqrt(-2),
 x[4] - x[3]/sqrt(2) + x[2]*sqrt(-2),
 x[4] - x[3]/sqrt(2) - x[2]*sqrt(-2)
];

#@ uncayley 
uncayley := (X) -> [
 (X[2]-X[1]) * sqrt(-2),
 (X[4]-X[3]) * sqrt(-2),
 (X[1]+X[2]-X[3]-X[4]) * sqrt(2),
 X[1]+X[2]+X[3]+X[4]
] /~ 4;

#@ cayley_g 
cayley_g := (X) -> expand(add(mul(X[j],j in {1,2,3,4} minus {i}),i = 1 .. 4));

#@ act_Cayley
act_Cayley[1]     := (X) -> [ X[1], X[2], X[3], X[4]];
act_Cayley[L]     := (X) -> [-X[3],-X[4],-X[2],-X[1]];
act_Cayley[LL]    := (X) -> [ X[2], X[1], X[4], X[3]];
act_Cayley[LLL]   := (X) -> [-X[4],-X[3],-X[1],-X[2]];
act_Cayley[M]     := (X) -> [-X[2],-X[1],-X[3],-X[4]];
act_Cayley[LM]    := (X) -> [ X[3], X[4], X[1], X[2]];
act_Cayley[LLM]   := (X) -> [-X[1],-X[2],-X[4],-X[3]];
act_Cayley[LLLM]  := (X) -> [ X[4], X[3], X[2], X[1]];
act_Cayley[N]     := (X) -> [ X[1], X[2], X[4], X[3]];
act_Cayley[LN]    := (X) -> [-X[4],-X[3],-X[2],-X[1]];
act_Cayley[LLN]   := (X) -> [ X[2], X[1], X[3], X[4]];
act_Cayley[LLLN]  := (X) -> [-X[3],-X[4],-X[1],-X[2]];
act_Cayley[MN]    := (X) -> [-X[2],-X[1],-X[4],-X[3]];
act_Cayley[LMN]   := (X) -> [ X[4], X[3], X[1], X[2]];
act_Cayley[LLMN]  := (X) -> [-X[1],-X[2],-X[3],-X[4]];
act_Cayley[LLLMN] := (X) -> [ X[3], X[4], X[2], X[1]];