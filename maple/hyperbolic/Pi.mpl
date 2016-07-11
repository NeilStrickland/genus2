# This file defines the group Pi generated by beta[0],...,beta[7] 
# (with indices read modulo 8) subject to relations beta[i] beta[i+4] = 1 
# and beta[0] beta[1] beta[2] beta[3] beta[4] beta[5] beta[6] beta[7] = 1 

# Elements of Pi are represented as lists of integers.  For example,
# [1,3,0] represents beta[1] o beta[3] o beta[0].  We allow indices
# outside {0,..,7} with the understanding that beta[i] = beta[i mod 8].

#@ `type/Pi_element`
`type/Pi_element` := (L) -> type(L,list(integer));

# We say that a word in the generators beta[i] is reduced if it contains 
# no subwords beta[i] beta[i+4], no decreasing runs of length at least 4,
# and no increasing runs of length at least 5.  The following function 
# checks whether a word (represented as a list of integers) is reduced.

#@ is_Pi_reduced
is_Pi_reduced := proc(L::Pi_element)
 local n,i,run_length,run_direction;
 n := nops(L);
 for i from 1 to n do
  if L[i] < 0 or L[i] > 7 then
   return(false);
  fi;
 od;
 if n <= 1 then
  return(true);
 fi;
 for i from 1 to n-1 do
  if modp(L[i+1]-L[i]+4,8) = 0 then
   return(false);
  fi;
 od;
 run_length := 0;
 run_direction := 0;
 for i from 2 to n do
  if run_direction = 0 then
   if mods(L[i]-L[i-1],8) = 1 then
    run_direction := 1;
    run_length := 2;
   elif mods(L[i]-L[i-1],8) = -1 then
    run_direction := -1;
    run_length := 2;
   fi; 
  else 
   if mods(L[i]-L[i-1]-run_direction,8) = 0 then
    run_length := run_length+1;
    if run_length >=4 and (run_direction = -1 or run_length > 4) then
     return(false);
    fi;
   else
    run_length := 0;
    run_direction := 0;
   fi;
  fi;
 od:
 return(true);
end; 

# The function Pi_reduce_once eliminates any subwords of the form 
# beta[i] beta[i+4], then removes the first inadmissable run that 
# it finds.  It returns a word that represents the same element of
# Pi as the original word supplied as an argument, but that is 
# closer to being reduced.

#@ Pi_reduce_once
Pi_reduce_once := proc(L::Pi_element)
 local n,i,run_length,run_direction,R;
 n := nops(L);
 if n = 0 then
  return([]);
 fi;

 R := [];
 for i from 1 to n do
  if R <> [] and modp(L[i]-R[-1]-4,8)=0 then
   R := R[1..-2];
  else
   R := [op(R),modp(L[i],8)];
  fi;
 od;
 if nops(R) <= 1 then
  return(R);
 fi;
 n := nops(R);
 run_length := 0;
 run_direction := 0;
 for i from 2 to n do
  if run_direction = 0 then
   if mods(R[i]-R[i-1],8) = 1 then
    run_direction := 1;
    run_length := 2;
   elif mods(R[i]-R[i-1],8) = -1 then
    run_direction := -1;
    run_length := 2;
   fi; 
  else 
   if mods(R[i]-R[i-1]-run_direction,8) = 0 then
    run_length := run_length+1;
    if run_length >=4 and (run_direction = -1 or run_length > 4) then
     return([seq(R[j],j=1..i-run_length),
             seq(modp(R[i+1-run_length]+(4-j)*run_direction,8),j=1..8-run_length),
             seq(R[j],j=i+1..n)]);
    fi;
   else
    run_length := 0;
    run_direction := 0;
   fi;
  fi;
 od:
 return(R);
end; 

# The function Pi_reduce applies Pi_reduce_once repeatedly to find
# the unique reduced word representing the same element of Pi as the 
# argument.

#@ Pi_reduce
Pi_reduce := proc(L)
 local M,N;
 M := L;
 N := Pi_reduce_once(M);
 while (M <> N) do
  M := N;
  N := Pi_reduce_once(M);
 od:
 return(N);
end;

# Pi_mult is the multiplication function for the group Pi.
# It concatenates its arguments to produce a (possibly unreduced)
# word, and then reduces it.

#@ Pi_mult
Pi_mult := proc() Pi_reduce(map(op,[args])); end;

# Pi_inv returns the inverse of a word in Pi.

#@ Pi_inv
Pi_inv := proc(L)
 local n,i;
 n := nops(L);
 [seq(modp(L[-i]+4,8),i=1..n)];
end;

# An alternative system of generators alpha[j]

#@ Pi_alpha
Pi_alpha[0] := [7,6,0]:
Pi_alpha[1] := [1,2,3]:
Pi_alpha[2] := [6]:
Pi_alpha[3] := [7]:

# The original generators beta[i] in terms of the alpha[j]

#@ Pi_beta_alpha
Pi_beta_alpha[0] := [-2,-3, 0];
Pi_beta_alpha[1] := [ 1, 3, 2];
Pi_beta_alpha[2] := [-2];
Pi_beta_alpha[3] := [-3];

# Another alternative system of generators sigma[t]

#@ Pi_sigma_beta
Pi_sigma_beta["a"] := [5,6];
Pi_sigma_beta["b"] := [2];
Pi_sigma_beta["c"] := [7,0];
Pi_sigma_beta["d"] := [4];
Pi_sigma_beta["e"] := [1,2];
Pi_sigma_beta["f"] := [3,4];

for i in ["a","b","c","d","e","f"] do
 Pi_sigma_beta[i, 1] := Pi_sigma_beta[i];
 Pi_sigma_beta[i,-1] := Pi_inv(Pi_sigma_beta[i]);
od:

# The original generators beta[i] in terms of the sigma[t]

#@ Pi_beta_sigma
Pi_beta_sigma[0] := [["d",-1]];
Pi_beta_sigma[1] := [["e", 1],["b",-1]];
Pi_beta_sigma[2] := [["b", 1]];
Pi_beta_sigma[3] := [["f", 1],["d",-1]];
Pi_beta_sigma[4] := [["d", 1]];
Pi_beta_sigma[5] := [["b", 1],["e",-1]];
Pi_beta_sigma[6] := [["b",-1]];
Pi_beta_sigma[7] := [["d", 1],["f",-1]];

# If L is a reduced word in Pi, then Pi_reduced_extensions(L)
# is the sequence of all reduced words that can be obtained by 
# adding an extra letter to the end of L

#@ Pi_reduced_extensions
Pi_reduced_extensions := proc(L::Pi_element)
 local n,A,B;
 n := nops(L);
 A := {0,1,2,3,4,5,6,7};
 if (n = 0) then
  return(seq([i],i=0..7));
 fi;
 A := A minus {modp(L[n]+4,8)};
 if n >= 3 and
  modp(L[n-1]-L[n  ],8) = 1 and
  modp(L[n-2]-L[n-1],8) = 1 then
  A := A minus {modp(L[n]-1,8)};
 elif n >= 4 and
  modp(L[n  ]-L[n-1],8) = 1 and
  modp(L[n-1]-L[n-2],8) = 1 and
  modp(L[n-2]-L[n-3],8) = 1 then
  A := A minus {modp(L[n]+1,8)};
 fi;
 A := sort([op(A)]);
 seq([op(L),A[i]],i=1..nops(A));
end;

# Pi_words(m) is the list of all reduced words of length precisely m.
#@ Pi_words
Pi_words := proc(m::nonnegint)
 option remember;
 if m = 0 then
  return([[]]);
 else
  return (map(Pi_reduced_extensions,Pi_words(m-1)));
 fi;
end;

# Pi_words_leq(m) is the list of all reduced words of length at most m.
#@ Pi_words_leq
Pi_words_leq := proc(m::nonnegint)
 map(op,[seq(Pi_words(k),k=0..m)]);
end;

# Pi_word_band(k) is a subset of Pi, starting with Pi_word_band(0)={1}.
# If we put
# A(k) = union of translates of F1 under Pi_word_band(j) with j <= k
# then A(k) is contained in the interior of A(k+1).

#@ Pi_word_band
Pi_word_band := proc(k)
 option remember;
 local B,L,M,j;

 if k = 0 then
  return([[]]);
 elif k = 1 then 
  return ([
   [0],[1],[2],[3],[4],[5],[6],[7],
   [0,7],[1,2],[2,1],[3,4],[4,3],[5,6],[6,5],[7,0],
   [0,7,0],[2,1,2],[4,3,4],[6,5,6],
   [1,2,3,4],[3,4,5,6],[5,6,7,0],[7,0,1,2]
  ]):
 else
  B := {seq(seq(Pi_mult(L,M),
                L in Pi_word_band(1)),
                M in Pi_word_band(k-1))}
        minus {seq(op(Pi_word_band[j]),j=0..k-1)}:
  B := [op(B)];
  return(B);
 fi;
end;

#@ Pi_word_block
Pi_word_block := proc(k)
 option remember;
 [seq(op(Pi_word_band(j)),j=0..k)];
end;

# act_Pi(L,z) gives the action of the word L on a point z in the
# open unit disc.  This depends on the parameter a_H.

#@ act_Pi
act_Pi := proc(L::Pi_element,z)
 local n,i,j,w;
 n := nops(L);
 w := z;
 for i from 0 to n-1 do
  w := simplify(beta[L[n-i]](w));
 od;
 return(w);
end;

# act_Pi0(L,z) and act_Pi1(L,z) are the same as 
# act_Pi(L,z) except that the numerical value a_H0 or
# a_H1 = evalf(a_H0) is substituted for the symbolic a_H.

#@ act_Pi0
act_Pi0 := proc(L::Pi_element,z)
 local n,i,j,w;
 n := nops(L);
 w := z;
 for i from 0 to n-1 do
  w := simplify(beta0[L[n-i]](w));
 od;
 return(w);
end;

#@ act_Pi1
act_Pi1 := proc(L::Pi_element,z)
 local n,i,j,w;
 n := nops(L);
 w := z;
 for i from 0 to n-1 do
  w := simplify(beta1[L[n-i]](w));
 od;
 return(w);
end;

# This is organised to be more efficient than act_Pi1

#@ act_Pi1_map
act_Pi1_map := proc(L::Pi_element)
 option remember;
 local z,zz,zd,zn,dd,ff;

 if L = [] then
  return((z) -> z);
 else
  zz := simplify(beta1[L[1]](act_Pi1_map(L[2..-1])(z))):
  zd := denom(zz):
  zn := numer(zz):
  dd := sqrt(abs(coeff(zn,z,0))^2 + abs(coeff(zn,z,1))^2 + abs(coeff(zd,z,0))^2 + abs(coeff(zd,z,1))^2);
  zd := expand(zd/dd);
  zn := expand(zn/dd);
  zz := zn/zd;
  return(unapply(zz,z));
 fi:
end;

# Given an expression f of the form (a z + b)/(c z + d), 
# the function mobius_matrix(f,z) returns the matrix
# [[a,b],[c,d]]/r, where r is chosen to make the determinant
# equal to one.

#@ mobius_matrix
mobius_matrix := proc(f,z)
 local ab,cd,a,b,c,d,rd;
 ab := numer(f);
 cd := denom(f);
 a := coeff(ab,z,1);
 b := coeff(ab,z,0);
 c := coeff(cd,z,1);
 d := coeff(cd,z,0);
 rd := sqrt(a*d - b*c);
 a := a/rd;
 b := b/rd;
 c := c/rd;
 d := d/rd;
 return(<<a|b>,<c|d>>);
end;

# Let M be a matrix in SL_2(C) representing a Mobius map that 
# preserves the unit disc.  Then SL2R_conjugate(M) returns a 
# matrix M1 in SL_2(R) that is conjugate to M.
  
#@ SL2R_conjugate
SL2R_conjugate := proc(M)
 local a,b,c,d,A,B,C,D,E;

 a,b,c,d := M[1,1],M[1,2],M[2,1],M[2,2];
 A := (  a - b - c + d)/2;
 B := (- a - b + c + d)/2*I;
 C := (  a - b + c - d)/2*I;
 D := (  a + b + c + d)/2;
 E := sqrt(A*D - B*C);
 A := Re(A/E); B := Re(B/E); C := Re(C/E); D := Re(D/E);
# A := A/E; B := B/E; C := C/E; D := D/E;
 return(<<A|B>,<C|D>>);
end;

# Given a word L in Pi, the function act_Pi1_matrix(L) returns
# a matrix in SL_2(C) that implements the action of L on the unit disk.

#@ act_Pi1_matrix
act_Pi1_matrix := proc(L::Pi_element)
 option remember;
 local z,zz,zd,zn,dd,ff;

 if L = [] then
  return(<<1|0>,<0|1>>);
 else
  return(beta1_matrix[L[1]] . act_Pi1_matrix(L[2..-1]));
 fi;
end;

# beta_L_word[i] is the conjugate of beta[i] by lambda

#@ beta_L_word
beta_L_word[0] := [2]:
beta_L_word[1] := [3]:
beta_L_word[2] := [4]:
beta_L_word[3] := [5]:
beta_L_word[4] := [6]:
beta_L_word[5] := [7]:
beta_L_word[6] := [0]:
beta_L_word[7] := [1]:

# beta_M_word[i] is the conjugate of beta[i] by mu

#@ beta_M_word
beta_M_word[0] := [2,0,1]:
beta_M_word[1] := [5,4,3]:
beta_M_word[2] := [0,7,6]:
beta_M_word[3] := [2,3,1]:
beta_M_word[4] := [5,4,6]:
beta_M_word[5] := [7,0,1]:
beta_M_word[6] := [2,3,4]:
beta_M_word[7] := [5,7,6]:

# beta_N_word[i] is the conjugate of beta[i] by nu

#@ beta_N_word
beta_N_word[0] := [0]:
beta_N_word[1] := [2,1,2]:
beta_N_word[2] := [6]:
beta_N_word[3] := [0,7,0]:
beta_N_word[4] := [4]:
beta_N_word[5] := [6,5,6]:
beta_N_word[6] := [2]:
beta_N_word[7] := [4,3,4]:

# The function act_on_Pi gives the action of L, M and N on Pi 
# by conjugation in Pi_tilde

#@ act_on_Pi
act_on_Pi[L] := proc(W::Pi_element)
 local W1;
 W1 := subs({seq(i = beta_L_word[i],i=0..7)},W);
 W1 := Pi_reduce(map(op,W1));
 return(W1);
end;

act_on_Pi[M] := proc(W::Pi_element)
 local W1;
 W1 := subs({seq(i = beta_M_word[i],i=0..7)},W);
 W1 := Pi_reduce(map(op,W1));
 return(W1);
end;

act_on_Pi[N] := proc(W::Pi_element)
 local W1;
 W1 := subs({seq(i = beta_N_word[i],i=0..7)},W);
 W1 := Pi_reduce(map(op,W1));
 return(W1);
end;

# If T.U = V in G, then T.U = V.L in Pi_tilde for some L in Pi.
# The appropriate value of L is stored as G_Pi_cocycle[T,U].

#@ G_Pi_cocycle
G_Pi_cocycle := table([(M,M)=[],(LLMN,LLLM)=[4,6],(LLMN,LLMN)=[1,2],(L,LLLM)=[],(N,LLL)=[],(LLLN,N)=[],(LL,M)=[],(LM,LLL)=[2,3],(LLN,LLLM)=[6,0,7,6],(LMN,MN)=[2,3,4,2],(1,N)=[],(LLL,LLL)=[],(LLM,1)=[],(LN,1)=[],(LLLM,LLMN)=[2,1,2,3,4,2],(LLLMN,LMN)=[3,2,1,2],(LLM,MN)=[],(MN,LN)=[2,3],(LL,LLM)=[],(LM,LM)=[7,6],(LLN,LLMN)=[2,3,4,2],(LMN,1)=[],(LLLN,LLMN)=[2,3,4,2],(L,LN)=[],(LLL,LM)=[],(N,LLLN)=[],(LLMN,LM)=[0,7,0,1],(M,LLLMN)=[0,2,1,2],(LLN,L)=[],(1,LLN)=[],(MN,LLL)=[6,5,6,4],(LLMN,N)=[],(LM,LLMN)=[2,1,2,3,4,2],(LLLM,1)=[],(LLLM,LLN)=[6,5,6,7,0,6],(L,LL)=[],(MN,LLLN)=[5,4],(LLL,LLLM)=[],(N,LN)=[],(LN,M)=[6,0,7,6],(LLMN,LLLN)=[5,4],(1,L)=[],(LMN,LLM)=[2,1],(1,M)=[],(M,LMN)=[4,3,4,2],(LLN,LLL)=[],(1,MN)=[],(LL,LLN)=[],(LM,LLN)=[6,5,6,7,0,6],(LN,LLM)=[6,0,7,6],(M,LL)=[2,3,4,5],(LLN,LM)=[6,0,7,6],(LMN,LLN)=[2,3,4,5],(LLLM,LM)=[7,6],(LLLMN,LM)=[0,7,0,1],(LLLM,N)=[],(LLLM,L)=[5,4],(L,LMN)=[],(LLM,LLN)=[6,5,6,7,0,6],(LLLM,LLL)=[2,3],(LLLMN,1)=[],(LMN,M)=[6,0,7,6],(LLLMN,LLLN)=[5,4],(N,LMN)=[2,3,4,2],(LMN,LN)=[2,3],(LLM,LL)=[2,3,4,5],(LLLMN,LLL)=[6,5,6,4],(L,M)=[],(M,N)=[],(LLN,LLN)=[],(LL,LLMN)=[],(LLLN,MN)=[2,3,4,2],(MN,LLM)=[2,1],(LMN,LL)=[6,5,6,7,0,6],(LL,1)=[],(LM,LLLM)=[0,1],(LLL,LMN)=[],(M,LN)=[6,5,6,4],(LMN,LLLMN)=[4,2],(LLLMN,L)=[6,0,7,0],(LLLM,MN)=[],(N,LLM)=[6,0,7,6],(LN,LL)=[],(LLN,1)=[],(MN,LMN)=[3,2,1,2],(LLL,LLN)=[],(1,LLL)=[],(LL,LL)=[],(LM,LN)=[6,5,6,4],(LN,LN)=[],(LL,MN)=[],(LLLN,LLN)=[],(LLMN,LLL)=[6,5,6,4],(LLLN,LM)=[6,0,7,6],(LLL,L)=[],(LLN,MN)=[2,3,4,2],(LLM,LLMN)=[2,1,2,3,4,2],(LLLMN,N)=[],(LLM,LLLM)=[0,1],(LN,LLMN)=[2,3,4,2],(LLMN,LLN)=[2,3,4,5],(L,LLLN)=[],(LLLM,LLLMN)=[0,2,1,2],(MN,L)=[6,0,7,0],(MN,LLLMN)=[4,2],(LLL,N)=[],(LLLM,LLM)=[6,7,0,1],(1,LLM)=[],(LM,MN)=[],(LLM,N)=[],(LLLN,LL)=[],(N,L)=[],(N,LLLMN)=[2,3,4,2],(LN,LLLN)=[],(1,LLLMN)=[],(LLLMN,LLLMN)=[4,2],(LLMN,L)=[6,0,7,0],(LL,LLLM)=[],(L,LLM)=[],(M,LLM)=[6,7,0,1],(LLL,LLLMN)=[],(M,LLLN)=[6,0,7,0],(LLLN,LLLMN)=[2,3,4,2],(L,MN)=[],(LM,1)=[],(LLN,LL)=[],(MN,M)=[6,0,7,6],(1,LMN)=[],(LM,LLLMN)=[0,2,1,2],(LLLMN,MN)=[2,3,4,2],(LLM,LM)=[7,6],(LLLN,LLLM)=[6,0,7,6],(LLM,LMN)=[4,3,4,2],(LLMN,LMN)=[3,2,1,2],(LL,LM)=[],(M,LLL)=[2,3],(LN,MN)=[2,3,4,2],(LMN,L)=[6,0,7,0],(LLLN,LMN)=[2,3,4,2],(LMN,LLLN)=[5,4],(LLL,M)=[],(LM,LL)=[2,3,4,5],(N,LLN)=[],(LLLMN,LLN)=[2,3,4,5],(LLL,LL)=[],(1,LLLN)=[],(LLM,M)=[],(LLM,L)=[5,4],(LN,L)=[],(1,LL)=[],(LLLM,LMN)=[4,3,4,2],(LL,LN)=[],(LLN,LMN)=[2,3,4,2],(LLLN,1)=[],(N,M)=[6,0,7,6],(LL,LLL)=[],(M,L)=[5,4],(L,LLN)=[],(LLN,N)=[],(LLLN,M)=[6,0,7,6],(LLMN,LLLMN)=[4,2],(LLLN,LLLN)=[],(LLMN,LL)=[6,5,6,7,0,6],(LLMN,LN)=[2,3],(LL,LLLN)=[],(N,N)=[],(LMN,LM)=[0,7,0,1],(MN,LLN)=[2,3,4,5],(LM,LLLN)=[6,0,7,0],(LN,LLLM)=[6,0,7,6],(1,1)=[],(LLN,LLLMN)=[2,3,4,2],(L,N)=[],(LMN,LLMN)=[1,2],(LLLMN,LLM)=[2,1],(LLN,LLM)=[6,0,7,6],(LLLM,LL)=[2,3,4,5],(LMN,LLLM)=[4,6],(L,LLMN)=[],(LLM,LLLN)=[6,0,7,0],(M,LLMN)=[2,1,2,3,4,2],(LLN,M)=[6,0,7,6],(LLL,MN)=[],(L,L)=[],(LLL,1)=[],(LLLM,M)=[],(LLMN,LLM)=[2,1],(LLLM,LN)=[6,5,6,4],(LLLN,LN)=[],(LLMN,M)=[6,0,7,6],(1,LLLM)=[],(LLL,LLM)=[],(LM,M)=[],(MN,N)=[],(LN,LM)=[6,0,7,6],(LLL,LLLN)=[],(LL,LLLMN)=[],(LM,N)=[],(LN,N)=[],(LLL,LLMN)=[],(L,LLLMN)=[],(MN,LLLM)=[4,6],(LLLMN,LL)=[6,5,6,7,0,6],(LLLM,LLLM)=[0,1],(LLLN,LLL)=[],(L,LLL)=[],(LLLM,LLLN)=[6,0,7,0],(LN,LLL)=[],(LN,LLLMN)=[2,3,4,2],(L,1)=[],(LLN,LLLN)=[],(M,LLLM)=[0,1],(LLN,LN)=[],(M,1)=[],(LM,LLM)=[6,7,0,1],(LMN,LLL)=[6,5,6,4],(M,LLN)=[6,5,6,7,0,6],(N,MN)=[2,3,4,2],(MN,MN)=[2,3,4,2],(LMN,N)=[],(LLLMN,LLLM)=[4,6],(1,LM)=[],(LLLMN,M)=[6,0,7,6],(N,LLLM)=[6,0,7,6],(1,LN)=[],(LL,N)=[],(M,LM)=[7,6],(M,MN)=[],(MN,1)=[],(LLM,LLM)=[6,7,0,1],(N,LL)=[],(LLMN,1)=[],(LLLMN,LN)=[2,3],(N,LLMN)=[2,3,4,2],(LLM,LLLMN)=[0,2,1,2],(N,LM)=[6,0,7,6],(LLLMN,LLMN)=[1,2],(MN,LL)=[6,5,6,7,0,6],(L,LM)=[],(LLLN,LLM)=[6,0,7,6],(LLL,LN)=[],(LLMN,MN)=[2,3,4,2],(LL,LMN)=[],(MN,LLMN)=[1,2],(LL,L)=[],(MN,LM)=[0,7,0,1],(LM,L)=[5,4],(LM,LMN)=[4,3,4,2],(LLM,LN)=[6,5,6,4],(LN,LLN)=[],(LLM,LLL)=[2,3],(LLLN,L)=[],(LN,LMN)=[2,3,4,2],(1,LLMN)=[],(N,1)=[],(LMN,LMN)=[3,2,1,2]]):

# If T is in G then beta[i] . T = T . L in Pi_tilde for some L in Pi.
# The appropriate value of L is stored as beta_G_twist[i,T]. 

#@ beta_G_twist
beta_G_twist := table();
for i from 0 to 7 do
 beta_G_twist[i,1]   := [i];
 beta_G_twist[i,L]   := [modp(i-2,8)];
 beta_G_twist[i,LL]  := [modp(i-4,8)];
 beta_G_twist[i,LLL] := [modp(i-6,8)];
od:
for i from 0 to 7 do
 beta_G_twist[i,M]    := beta_M_word[i]; 
 beta_G_twist[i,LM]   := beta_M_word[modp(i-2,8)]; 
 beta_G_twist[i,LLM]  := beta_M_word[modp(i-4,8)]; 
 beta_G_twist[i,LLLM] := beta_M_word[modp(i-6,8)]; 
 beta_G_twist[i,N]    := beta_N_word[i]; 
 beta_G_twist[i,LN]   := beta_N_word[modp(i-2,8)]; 
 beta_G_twist[i,LLN]  := beta_N_word[modp(i-4,8)]; 
 beta_G_twist[i,LLLN] := beta_N_word[modp(i-6,8)]; 
 beta_G_twist[i,MN]   := Pi_mult(op(map(p -> beta_N_word[p],beta_M_word[i])));
od:
for i from 0 to 7 do
 beta_G_twist[i,LMN]   := beta_G_twist[modp(i-2,8),MN];
 beta_G_twist[i,LLMN]  := beta_G_twist[modp(i-4,8),MN];
 beta_G_twist[i,LLLMN] := beta_G_twist[modp(i-6,8),MN];
od:

# Elements of Pi_tilde are represented by pairs [T,L], where T is in G16
# and L is a list of integers representing an element of Pi

#@ `type/Pi_tilde_element`
`type/Pi_tilde_element` := (TL) ->
 type(TL,list) and nops(TL) = 2 and member(TL[1],G16) and type(TL[2],Pi_element); 

# The identity element in Pi_tilde
#@ Pi_tilde_id
Pi_tilde_id := [1,[]]:

# The multiplication function for Pi_tilde. 
# It accepts an arbitrary number of arguments.

#@ Pi_tilde_mult
Pi_tilde_mult := proc()
 local T0,L0,T1,L1,T2,L2;
 if nargs = 0 then
  return [1,[]];
 elif nargs = 1 then
  return args[1];
 else
  T0,L0 := op(args[1]);
  T1,L1 := op(args[2]);
  T2 := G_mult(T0,T1);
  L2 := Pi_mult(G_Pi_cocycle[T0,T1],op(map(i -> beta_G_twist[i,T1],L0)),L1);
  if nargs = 2 then
   return [T2,L2];
  else
   return Pi_tilde_mult([T2,L2],args[3..-1]);
  fi;
 fi;
end:

# The inversion function for Pi_tilde

#@ Pi_tilde_inv
Pi_tilde_inv := proc(A0::Pi_tilde_element)
 local T0,L0,T1,L1,T2,L2;
 T0,L0 := op(A0);
 T1 := G_inv(T0);
 Pi_tilde_mult([1,Pi_inv(L0)],[T1,Pi_inv(G_Pi_cocycle[T0,T1])]);
end:

# Action of Pi_tilde on the unit disk

#@ act_Pi_tilde
act_Pi_tilde := proc(A0::Pi_tilde_element,z::CC)
 local T0,L0;
 T0,L0 := op(A0);

 act_H[T0](act_Pi(L0,z));
end:

# Action of Pi_tilde on the unit disk (numeric version)

#@ act_Pi_tilde0
act_Pi_tilde0 := proc(A0::Pi_tilde_element,z::CC1)
 local T0,L0;
 T0,L0 := op(A0);

 act_H0[T0](act_Pi0(L0,z));
end:

# Reflections across walls of F16
#@ Pi_tilde_s
Pi_tilde_s[0] := [MN,[6]];
Pi_tilde_s[1] := [LN,[]];
Pi_tilde_s[3] := [LLN,[4]];
Pi_tilde_s[5] := [N,[]];

# Generators of Pi_tilde as words in the reflections
#@ G_s_word
G_s_word[1]     := [];
G_s_word[L]     := [1,5];
G_s_word[LL]    := [1,5,1,5];
G_s_word[LLL]   := [5,1];
G_s_word[M]     := [1,5,3,5,1,0];
G_s_word[LM]    := [1,5,1,5,3,5,1,0];
G_s_word[LLM]   := [5,1,3,5,1,0];
G_s_word[LLLM]  := [3,5,1,0];
G_s_word[N]     := [5];
G_s_word[LN]    := [1];
G_s_word[LLN]   := [1,5,1];
G_s_word[LLLN]  := [5,1,5];
G_s_word[MN]    := [1,5,3,5,1,0,5];
G_s_word[LMN]   := [1,5,1,5,3,5,1,0,5];
G_s_word[LLMN]  := [5,1,3,5,1,0,5];
G_s_word[LLLMN] := [3,5,1,0,5];

#@ beta_s_word
beta_s_word[0] := [3,1,5,1];
beta_s_word[1] := [5,1,5,0,3,5,1,0];
beta_s_word[2] := [1,5,3,5,1,5];
beta_s_word[3] := [5,0,3,5,1,0,5,1];
beta_s_word[4] := [1,5,1,3];
beta_s_word[5] := [0,1,5,3,0,5,1,5];
beta_s_word[6] := [5,1,5,3,5,1];
beta_s_word[7] := [1,5,0,1,5,3,0,5];


######################################################################

# Given an expression p involving a_H  and z, this tries to find an 
# element L in Pi which sends z to p.

#@ find_Pi_word
find_Pi_word := proc(p,max_length_) 
 local max_length,err,found,i,L;

 max_length := `if`(nargs > 1,max_length,5);
 for i from 0 to max_length do
  for L in Pi_words(i) do
   err := simplify(p - act_Pi(L,z));
   if err = 0 then
    return(L);
   fi;
  od;
 od;

 return FAIL;
end;

# Given expressions p and q involving a_H, this tries to find an 
# element L in Pi which sends p to q.

#@ find_Pi_word_points
find_Pi_word_points := proc(p,q,max_length_) 
 local max_length,err,found,i,L;

 max_length := `if`(nargs > 2,max_length,5);
 for i from 0 to max_length do
  for L in Pi_words(i) do
   err := simplify(q - act_Pi(L,p));
   if err = 0 then
    return(L);
   fi;
  od;
 od;

 return FAIL;
end;

# Given complex numbers p and q, this tries to find an element L in Pi which
# sends p to q (where the action is determined by the numerical parameter a_H0).

#@ find_Pi_word_points0
find_Pi_word_points0 := proc(p,q,max_length_,tolerance_) 
 local max_length,tolerance,p1,q1,err,found,i,L;

 max_length := `if`(nargs > 2,max_length_,5);
 tolerance := `if`(nargs > 3,tolerance_,10.^(-8));

 p1 := evalf(subs(a_H = a_H1,p));
 q1 := evalf(subs(a_H = a_H1,q));

 for i from 0 to max_length do
  for L in Pi_words(i) do
   err := q1 - act_Pi1(L,p1);
   if abs(err) < tolerance then
    return(L);
   fi;
  od;
 od;

 return FAIL;
end;


