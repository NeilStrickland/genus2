<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a><span style="color:red">#@ cayley 
</span><a name="line_2"></a>cayley := (x) -> [
<a name="line_3"></a> x[4] + x[3]/sqrt(2) + x[1]*sqrt(-2),
<a name="line_4"></a> x[4] + x[3]/sqrt(2) - x[1]*sqrt(-2),
<a name="line_5"></a> x[4] - x[3]/sqrt(2) + x[2]*sqrt(-2),
<a name="line_6"></a> x[4] - x[3]/sqrt(2) - x[2]*sqrt(-2)
<a name="line_7"></a>];
<a name="line_8"></a>
<a name="line_9"></a><span style="color:red">#@ uncayley 
</span><a name="line_10"></a>uncayley := (X) -> [
<a name="line_11"></a> (X[2]-X[1]) * sqrt(-2),
<a name="line_12"></a> (X[4]-X[3]) * sqrt(-2),
<a name="line_13"></a> (X[1]+X[2]-X[3]-X[4]) * sqrt(2),
<a name="line_14"></a> X[1]+X[2]+X[3]+X[4]
<a name="line_15"></a>] /~ 4;
<a name="line_16"></a>
<a name="line_17"></a><span style="color:red">#@ cayley_g 
</span><a name="line_18"></a>cayley_g := (X) -> expand(add(mul(X[j],j in {1,2,3,4} minus {i}),i = 1 .. 4));
<a name="line_19"></a>
<a name="line_20"></a><span style="color:red">#@ act_Cayley
</span><a name="line_21"></a>act_Cayley[1]     := (X) -> [ X[1], X[2], X[3], X[4]];
<a name="line_22"></a>act_Cayley[L]     := (X) -> [-X[3],-X[4],-X[2],-X[1]];
<a name="line_23"></a>act_Cayley[LL]    := (X) -> [ X[2], X[1], X[4], X[3]];
<a name="line_24"></a>act_Cayley[LLL]   := (X) -> [-X[4],-X[3],-X[1],-X[2]];
<a name="line_25"></a>act_Cayley[M]     := (X) -> [-X[2],-X[1],-X[3],-X[4]];
<a name="line_26"></a>act_Cayley[LM]    := (X) -> [ X[3], X[4], X[1], X[2]];
<a name="line_27"></a>act_Cayley[LLM]   := (X) -> [-X[1],-X[2],-X[4],-X[3]];
<a name="line_28"></a>act_Cayley[LLLM]  := (X) -> [ X[4], X[3], X[2], X[1]];
<a name="line_29"></a>act_Cayley[N]     := (X) -> [ X[1], X[2], X[4], X[3]];
<a name="line_30"></a>act_Cayley[LN]    := (X) -> [-X[4],-X[3],-X[2],-X[1]];
<a name="line_31"></a>act_Cayley[LLN]   := (X) -> [ X[2], X[1], X[3], X[4]];
<a name="line_32"></a>act_Cayley[LLLN]  := (X) -> [-X[3],-X[4],-X[1],-X[2]];
<a name="line_33"></a>act_Cayley[MN]    := (X) -> [-X[2],-X[1],-X[4],-X[3]];
<a name="line_34"></a>act_Cayley[LMN]   := (X) -> [ X[4], X[3], X[1], X[2]];
<a name="line_35"></a>act_Cayley[LLMN]  := (X) -> [-X[1],-X[2],-X[3],-X[4]];
<a name="line_36"></a>act_Cayley[LLLMN] := (X) -> [ X[3], X[4], X[2], X[1]];  </pre>
 </body>
</html>
    