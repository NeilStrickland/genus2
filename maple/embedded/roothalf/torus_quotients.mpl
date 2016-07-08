# Here we aim to give a supply of maps EX^* -> S^1 with interesting properties.
# The most efficient way to encode such a map is to give a quotient of
# polynomials, which determines a map from an open subset of EX^* to R.
# In good cases this will extend to give a continuous function from all of
# EX^* to R u {infinity}, and then we can identify R u {infinity} with
# S^1 by the map t |-> (1-t^2,2t)/(1+t^2), with inverse (u,v) |-> v/(1+u).
# However, it is often delicate to ensure that such an extension exists.
# 
# Note that the above identification relates multiplication on S^1 to the
# operation (s,t) |-> (s+t)/(1-st) = tan_sum(s,t) on R u {infinity}.

# We use several different models for the circle and the torus, indicated
# by the notation T, TA, TC and TP.  These are explained in the file 
# Rn.mpl, which defines functions to convert between the models.

#@ TTC_to_TT 
TTC_to_TT := (z) -> [
 Re(z[1]),Im(z[1]),Re(z[2]),Im(z[2]),Re(z[3]),Im(z[3]),Re(z[4]),Im(z[4])
];

#@ TT_to_TTC 
TT_to_TTC := (u) -> [seq(u[2*i-1]+I*u[2*i],i=1..4)];

######################################################################
######################################################################
# A map to the 4-torus

#@ E_to_TTC_xyr 
E_to_TTC_xyr := [
 ( y[1]*(1-y[2]/sqrt(2))-1/sqrt(2)+I*x[1])/((1-y[1]/sqrt(2))*r[1]),
 ( y[1]*(1+y[2]/sqrt(2))-1/sqrt(2)+I*x[2])/((1-y[1]/sqrt(2))*r[2]),
 (-y[1]*(1-y[2]/sqrt(2))-1/sqrt(2)+I*x[1])/((1+y[1]/sqrt(2))*r[1]),
 (-y[1]*(1+y[2]/sqrt(2))-1/sqrt(2)+I*x[2])/((1+y[1]/sqrt(2))*r[2])
];

#@ E_to_TTP_xyr 
E_to_TTP_xyr :=  [
 (1/sqrt(2)+r[1])*(1-r[1]*y[1])/x[1],
 (1/sqrt(2)+r[2])*(1-r[2]*y[1])/x[2],
 (1/sqrt(2)+r[1])*(1+r[1]*y[1])/x[1],
 (1/sqrt(2)+r[2])*(1+r[2]*y[1])/x[2]
];

#@ E_to_TT_xyr  
E_to_TT_xyr  :=
 simplify(TTC_to_TT(E_to_TTC_xyr)) assuming r[1]>0 and r[2]>0;
 
#@ E_to_TTC 
E_to_TTC := unapply(eval(subs({y=yx0,r=rx},E_to_TTC_xyr)),x);

#@ E_to_TTP 
E_to_TTP := unapply(eval(subs({y=yx0,r=rx},E_to_TTP_xyr)),x);

#@ E_to_TT  
E_to_TT  := unapply(eval(subs({y=yx0,r=rx},E_to_TT_xyr)),x);

# A left inverse for E_to_TT

#@ TT_to_E 
TT_to_E := proc(u)
 local A,R,S,X,Y,Z;

 A[ 1] := simplify(factor((2-u[1]*u[5]-u[2]*u[6]-u[3]*u[7]-u[4]*u[8])/4));
 Z[ 1] := simplify(factor(2*A[1]/(1+A[1])));
 A[ 2] := simplify(factor((u[1]^2+u[3]^2+u[5]^2+u[7]^2)/4));
 A[ 3] := simplify(factor((A[2]+2*A[1])/(1+A[1])/(1+2*A[1])));
 Z[ 2] := simplify(factor(2 - 1/A[3]));
 S[ 1] := simplify(factor(sqrt(1 - Z[2]/2)));   # r[1] * r[2]
 S[ 2] := simplify(factor(sqrt(2*(1 + S[1])))); # r[1] + r[2]
 A[ 4] := simplify(factor(1 - Z[2] + S[1]));
 A[ 5] := simplify(factor((u[1]+u[3]-u[5]-u[7])*S[2]*S[1]/(1+A[1])/A[4]/2));
 Y[ 1] := A[5];
 A[ 6] := simplify(factor((u[3]*u[5]-u[1]*u[7])*S[1]*(1-Z[1]/2)/2));
 A[ 7] := simplify(factor((1-Z[2]/2)*((u[1]^2-u[3]^2)*(1-Y[1]/sqrt(2))^2+
                                      (u[5]^2-u[7]^2)*(1+Y[1]/sqrt(2))^2)));
 A[ 8] := simplify(factor(((u[1]*u[5]+u[2]*u[6]-u[3]*u[7]-u[4]*u[8])/(1+A[1]))));
 A[ 9] := simplify(factor((A[7]+A[8])/(1+Z[1]*Z[2])/sqrt(2)));
 Y[ 2] := A[9];
 R[ 1] := simplify(factor(sqrt(1 - Y[2]/sqrt(2))));
 R[ 2] := simplify(factor(sqrt(1 + Y[2]/sqrt(2))));
 A[10] := simplify(factor(u[2]*R[1]*(1-Y[1]/sqrt(2))));
 A[11] := simplify(factor(u[4]*R[2]*(1-Y[1]/sqrt(2))));
 X[ 1] := A[10];
 X[ 2] := A[11];
 X[ 3] := Y[1];
 X[ 4] := simplify(-Y[1]*Y[2]); 
 return [X[1],X[2],X[3],X[4]];
end:

#@ TTC_to_E 
TTC_to_E := (z) -> TT_to_E(TTC_to_TT(z));

#@ TT_to_E_generic 
TT_to_E_generic := proc(u)
 local X,a,b;

 X[1] := -sqrt(2)*u[2]*u[6]/(u[1]*u[6]+u[2]*u[5]);
 X[2] := -sqrt(2)*u[4]*u[8]/(u[3]*u[8]+u[4]*u[7]);
 X[3] :=  sqrt(2)*(u[2]-u[6])/(u[2]+u[6]);
 a := ((u[1]-u[5])*(1-u[3]*u[7]-u[4]*u[8]))^2;
 b := ((u[3]-u[7])*(1-u[1]*u[5]-u[2]*u[6]))^2;
 X[4] := -sqrt(2)*X[3]*(a-b)/(a+b);
 return [X[1],X[2],X[3],X[4]];
end:

#@ TTC_to_E_generic 
TTC_to_E_generic := (z) -> TT_to_E_generic(TTC_to_TT(z));

#@ TTP_to_E_generic 
TTP_to_E_generic := (t) -> [
 2*sqrt(2)*t[3]*t[1]/((t[1]*t[3]-1)*(t[1]+t[3])),
 2*sqrt(2)*t[4]*t[2]/((t[2]*t[4]-1)*(t[2]+t[4])),
  -sqrt(2)*(t[1]-t[3])*(t[1]*t[3]-1)/((t[1]+t[3])*(t[1]*t[3]+1)),
  -(4*t[1]-4*t[3])*(t[1]*t[3]-1)*(t[1]*t[4]-t[2]*t[3])*(t[1]*t[2]-t[3]*t[4])/
    ((t[1]+t[3])*(t[1]*t[3]+1)*(t[1]^2*t[2]^2+t[1]^2*t[4]^2-4*t[1]*t[2]*t[3]*t[4]+t[2]^2*t[3]^2+t[3]^2*t[4]^2))
]:

#@ act_TTP
act_TTP[1]    := (t) -> [ t[1], t[2], t[3], t[4]];
act_TTP[L]    := (t) -> [-t[2], t[1],-t[4], t[3]];
act_TTP[LL]   := (t) -> [-t[1],-t[2],-t[3],-t[4]];
act_TTP[LLL]  := (t) -> [ t[2],-t[1], t[4],-t[3]];
act_TTP[M]    := (t) -> [ t[3],-t[4], t[1],-t[2]];
act_TTP[LM]   := (t) -> [ t[4], t[3], t[2], t[1]];
act_TTP[LLM]  := (t) -> [-t[3], t[4],-t[1], t[2]];
act_TTP[LLLM] := (t) -> [-t[4],-t[3],-t[2],-t[1]];
act_TTP[N]    := (t) -> [ t[1],-t[2], t[3],-t[4]];
act_TTP[LN]   := (t) -> [ t[2], t[1], t[4], t[3]];
act_TTP[LLN]  := (t) -> [-t[1], t[2],-t[3], t[4]];
act_TTP[LLLN] := (t) -> [-t[2],-t[1],-t[4],-t[3]];
act_TTP[MN]   := (t) -> [ t[3], t[4], t[1], t[2]];
act_TTP[LMN]  := (t) -> [-t[4], t[3],-t[2], t[1]];
act_TTP[LLMN] := (t) -> [-t[3],-t[4],-t[1],-t[2]];
act_TTP[LLLMN]:= (t) -> [ t[4],-t[3], t[2],-t[1]];

#@ act_TTC
act_TTC[1]    := (z) -> [          z[1] ,          z[2] ,          z[3] ,          z[4]];
act_TTC[L]    := (z) -> [conjugate(z[2]),          z[1] ,conjugate(z[4]),          z[3]];
act_TTC[LL]   := (z) -> [conjugate(z[1]),conjugate(z[2]),conjugate(z[3]),conjugate(z[4])];
act_TTC[LLL]  := (z) -> [          z[2] ,conjugate(z[1]),          z[4] ,conjugate(z[3])];
act_TTC[M]    := (z) -> [          z[3] ,conjugate(z[4]),          z[1] ,conjugate(z[2])];
act_TTC[LM]   := (z) -> [          z[4] ,          z[3] ,          z[2] ,          z[1]];
act_TTC[LLM]  := (z) -> [conjugate(z[3]),          z[4] ,conjugate(z[1]),          z[2]];
act_TTC[LLLM] := (z) -> [conjugate(z[4]),conjugate(z[3]),conjugate(z[2]),conjugate(z[1])];
act_TTC[N]    := (z) -> [          z[1] ,conjugate(z[2]),          z[3] ,conjugate(z[4])];
act_TTC[LN]   := (z) -> [          z[2] ,          z[1] ,          z[4] ,          z[3]];
act_TTC[LLN]  := (z) -> [conjugate(z[1]),          z[2] ,conjugate(z[3]),          z[4]];
act_TTC[LLLN] := (z) -> [conjugate(z[2]),conjugate(z[1]),conjugate(z[4]),conjugate(z[3])];
act_TTC[MN]   := (z) -> [          z[3] ,          z[4] ,          z[1] ,          z[2]];
act_TTC[LMN]  := (z) -> [conjugate(z[4]),          z[3] ,conjugate(z[2]),          z[1]];
act_TTC[LLMN] := (z) -> [conjugate(z[3]),conjugate(z[4]),conjugate(z[1]),conjugate(z[2])];
act_TTC[LLLMN]:= (z) -> [          z[4] ,conjugate(z[3]),          z[2] ,conjugate(z[1])];

#@ act_TT
act_TT[1]     := (u) -> [u[1],  u[2], u[3],  u[4], u[5],  u[6], u[7],  u[8]];
act_TT[L]     := (u) -> [u[3], -u[4], u[1],  u[2], u[7], -u[8], u[5],  u[6]];
act_TT[LL]    := (u) -> [u[1], -u[2], u[3], -u[4], u[5], -u[6], u[7], -u[8]];
act_TT[LLL]   := (u) -> [u[3],  u[4], u[1], -u[2], u[7],  u[8], u[5], -u[6]];
act_TT[M]     := (u) -> [u[5],  u[6], u[7], -u[8], u[1],  u[2], u[3], -u[4]];
act_TT[LM]    := (u) -> [u[7],  u[8], u[5],  u[6], u[3],  u[4], u[1],  u[2]];
act_TT[LLM]   := (u) -> [u[5], -u[6], u[7],  u[8], u[1], -u[2], u[3],  u[4]];
act_TT[LLLM]  := (u) -> [u[7], -u[8], u[5], -u[6], u[3], -u[4], u[1], -u[2]];
act_TT[N]     := (u) -> [u[1],  u[2], u[3], -u[4], u[5],  u[6], u[7], -u[8]];
act_TT[LN]    := (u) -> [u[3],  u[4], u[1],  u[2], u[7],  u[8], u[5],  u[6]];
act_TT[LLN]   := (u) -> [u[1], -u[2], u[3],  u[4], u[5], -u[6], u[7],  u[8]];
act_TT[LLLN]  := (u) -> [u[3], -u[4], u[1], -u[2], u[7], -u[8], u[5], -u[6]];
act_TT[MN]    := (u) -> [u[5],  u[6], u[7],  u[8], u[1],  u[2], u[3],  u[4]];
act_TT[LMN]   := (u) -> [u[7], -u[8], u[5],  u[6], u[3], -u[4], u[1],  u[2]];
act_TT[LLMN]  := (u) -> [u[5], -u[6], u[7], -u[8], u[1], -u[2], u[3], -u[4]];
act_TT[LLLMN] := (u) -> [u[7],  u[8], u[5], -u[6], u[3],  u[4], u[1], -u[2]];

# Images of the curves c[k] in the 4-torus

#@ c_TTP
c_TTP[ 0] := (t) -> [(1/2)*(sqrt(4*cos(t)^2+2)+sqrt(2))/cos(t),
                    (1/2)*(sqrt(2+4*sin(t)^2)+sqrt(2))/sin(t),
                    (1/2)*(sqrt(4*cos(t)^2+2)+sqrt(2))/cos(t),
                    (1/2)*(sqrt(2+4*sin(t)^2)+sqrt(2))/sin(t)];
c_TTP[ 1] := (t) -> (1+sqrt(2)) *~
 [ (1-cos(t))/sin(t),(1-cos(t))/sin(t), (1+cos(t))/sin(t), (1+cos(t))/sin(t)];
c_TTP[ 2] := (t) -> (1+sqrt(2)) *~
 [-(1-cos(t))/sin(t),(1-cos(t))/sin(t),-(1+cos(t))/sin(t), (1+cos(t))/sin(t)];
c_TTP[ 3] := (t) -> [infinity,  (1/2)*sqrt(2)*(1+sqrt(3))*sin(t)/(1+cos(t)),
                    infinity, -(1/2)*sqrt(2)*(1+sqrt(3))*sin(t)/(cos(t)-1)];
c_TTP[ 4] := (t) -> [-(1/2)*sqrt(2)*(1+sqrt(3))*sin(t)/(1+cos(t)), infinity,
                     (1/2)*sqrt(2)*(1+sqrt(3))*sin(t)/(cos(t)-1), infinity];
c_TTP[ 5] := (t) -> [sqrt(2)/sin(t)*((sqrt(2+sin(t/2)^2)-1)*
                                    (sqrt(2-sin(t/2)^2)+1)-cos(t/2)^2),0,
                    sqrt(2)/sin(t)*((sqrt(2+sin(t/2)^2)+1)*
                                    (sqrt(2-sin(t/2)^2)+1)+cos(t/2)^2),infinity];
c_TTP[ 6] := unapply(subs(-infinity = infinity,act_TTP[ L](c_TTP[5](t))),t):
c_TTP[ 7] := unapply(subs(-infinity = infinity,act_TTP[ M](c_TTP[5](t))),t):
c_TTP[ 8] := unapply(subs(-infinity = infinity,act_TTP[LM](c_TTP[5](t))),t):
c_TTP[ 9] := unapply([(sqrt(3)-1)*(sqrt(2-cos(t))+1)-(1-cos(t)),
                     (sqrt(3)-1)*(sqrt(2+cos(t))+1)-(1+cos(t)),
                     (sqrt(3)+1)*(sqrt(2-cos(t))+1)+(1-cos(t)),
                     (sqrt(3)+1)*(sqrt(2+cos(t))+1)+(1+cos(t))] /~ sin(t),t);
c_TTP[10] := unapply(act_TTP[ L](c_TTP[9](t)),t):
c_TTP[11] := unapply(act_TTP[ M](c_TTP[9](t)),t):
c_TTP[12] := unapply(act_TTP[LM](c_TTP[9](t)),t):
c_TTP[13] := unapply([
(sqrt(3)-sin(t))*sqrt(6)/9*(sqrt(3+sin(t)^2)+sin(t))*(3+sin(t)^2)^(1/4)*
 sqrt(sqrt(3+sin(t)^2)-sin(t))+(sqrt(3)-sin(t))^2/3+sin(t)/3*sqrt(3+sin(t)^2),
(sqrt(3)-sin(t))*sqrt(6)/9*(sqrt(3+sin(t)^2)-sin(t))*(3+sin(t)^2)^(1/4)*
 sqrt(sqrt(3+sin(t)^2)+sin(t))+(sqrt(3)-sin(t))^2/3-sin(t)/3*sqrt(3+sin(t)^2),
(sqrt(3)+sin(t))*sqrt(6)/9*(sqrt(3+sin(t)^2)+sin(t))*(3+sin(t)^2)^(1/4)*
 sqrt(sqrt(3+sin(t)^2)-sin(t))+(sqrt(3)+sin(t))^2/3+sin(t)/3*sqrt(3+sin(t)^2),
(sqrt(3)+sin(t))*sqrt(6)/9*(sqrt(3+sin(t)^2)-sin(t))*(3+sin(t)^2)^(1/4)*
 sqrt(sqrt(3+sin(t)^2)+sin(t))+(sqrt(3)+sin(t))^2/3-sin(t)/3*sqrt(3+sin(t)^2)
] /~ cos(t),t);
c_TTP[14] := unapply(act_TTP[ L](c_TTP[13](t)),t):
c_TTP[15] := unapply(act_TTP[ N](c_TTP[13](t)),t):
c_TTP[16] := unapply(act_TTP[LN](c_TTP[13](t)),t):


#@ c_TTC
c_TTC[ 0] := (t) -> [(sqrt(2)*I*cos(t)-1)/sqrt(2+cos(2*t)),
                     (sqrt(2)*I*sin(t)-1)/sqrt(2-cos(2*t)),
                     (sqrt(2)*I*cos(t)-1)/sqrt(2+cos(2*t)),
                     (sqrt(2)*I*sin(t)-1)/sqrt(2-cos(2*t))];

c_TTC[ 1] := (t) -> [( I*sin(t)+sqrt(2)*cos(t)-1)/(sqrt(2)-cos(t)),
                     ( I*sin(t)+sqrt(2)*cos(t)-1)/(sqrt(2)-cos(t)),
                     ( I*sin(t)-sqrt(2)*cos(t)-1)/(sqrt(2)+cos(t)),
                     ( I*sin(t)-sqrt(2)*cos(t)-1)/(sqrt(2)+cos(t))];

c_TTC[ 2] := (t) -> [(-I*sin(t)+sqrt(2)*cos(t)-1)/(sqrt(2)-cos(t)),
                     ( I*sin(t)+sqrt(2)*cos(t)-1)/(sqrt(2)-cos(t)),
                     (-I*sin(t)-sqrt(2)*cos(t)-1)/(sqrt(2)+cos(t)),
                     ( I*sin(t)-sqrt(2)*cos(t)-1)/(sqrt(2)+cos(t))];

c_TTC[ 3] := (t) -> [-1,(2+I*sqrt(2)*sin(t))/(sqrt(3)-cos(t)) - sqrt(3),
                     -1,(2+I*sqrt(2)*sin(t))/(sqrt(3)+cos(t)) - sqrt(3)];

c_TTC[ 4] := (t) -> [(2-I*sqrt(2)*sin(t))/(sqrt(3)-cos(t)) - sqrt(3),-1,
                     (2-I*sqrt(2)*sin(t))/(sqrt(3)+cos(t)) - sqrt(3),-1];
 
c_TTC[ 5] := (t) -> [-(1/2)*sqrt(2)*(I*sqrt(5-cos(t))*sin(t)*sqrt(2)+2*sqrt(2)*cos(t)+
                     (2*I)*sin(t)+sqrt(5-cos(t))*cos(t)-2*sqrt(2)+
                     sqrt(5-cos(t)))*sqrt(2*cos(t)+6)/
		     ((-3+cos(t))*(cos(t)+3)), 1,
		     -(1/2)*sqrt(2)*(I*sqrt(5-cos(t))*sin(t)*sqrt(2)+2*sqrt(2)*cos(t)-
                     (2*I)*sin(t)-sqrt(5-cos(t))*cos(t)-2*sqrt(2)-
                     sqrt(5-cos(t)))*sqrt(2*cos(t)+6)/
		     ((-3+cos(t))*(cos(t)+3)), -1];

c_TTC[ 6] := unapply(act_TTC[ L](c_TTC[5](t)),t):
c_TTC[ 7] := unapply(act_TTC[ M](c_TTC[5](t)),t):
c_TTC[ 8] := unapply(act_TTC[LM](c_TTC[5](t)),t):

c_TTC[ 9] := (t) -> [( sqrt(3)-1 - (sqrt(3)+1)*(cos(t)-I*sin(t)))/(2*sqrt(2-cos(t))),
		     ( sqrt(3)-1 + (sqrt(3)+1)*(cos(t)+I*sin(t)))/(2*sqrt(2+cos(t))),
		     (-sqrt(3)-1 + (sqrt(3)-1)*(cos(t)+I*sin(t)))/(2*sqrt(2-cos(t))),
		     (-sqrt(3)-1 - (sqrt(3)-1)*(cos(t)-I*sin(t)))/(2*sqrt(2+cos(t)))];

c_TTC[10] := unapply(act_TTC[ L](c_TTC[9](t)),t):
c_TTC[11] := unapply(act_TTC[ M](c_TTC[9](t)),t):
c_TTC[12] := unapply(act_TTC[LM](c_TTC[9](t)),t):

c_TTC[13] := unapply(map(s -> RP1_to_SC1([s,cos(t)]),c_TTP[13](t) *~ cos(t)),t);
c_TTC[14] := unapply(act_TTC[ L](c_TTC[13](t)),t):
c_TTC[15] := unapply(act_TTC[ N](c_TTC[13](t)),t):
c_TTC[16] := unapply(act_TTC[LN](c_TTC[13](t)),t):

# Images of the vertices v[i] in the 4-torus

#@ v_TTC
#@ v_TTP
for i from 0 to 20 do
 v_TTC[i]   := combine(simplify(expand(E_to_TC ( v_E0[i]))));
 v_TTP[i]   := combine(simplify(expand(rationalize(map(SC1_to_R,v_TC[i])))));
od:

##################################################
##################################################
# A map to the 2-torus, invariant under M = mu

#@ E_to_TPp_xyr 
E_to_TPp_xyr := [sqrt(2)*x[1]/(1-sqrt(2)*y[2]),2*x[2]/y[1]/(1+sqrt(2)*y[2])];

#@ E_to_TCp_xyr 
E_to_TCp_xyr := [
 (2*I*x[1] + y[1]^2*(1-sqrt(2)*y[2])/sqrt(2) - y[2]*(1-y[1]^2/2))/
  (sqrt(2)*(1-y[1]^2/2)*(1-y[2]/sqrt(2))),
 (2*I*x[2]*y[1] + y[1]^2*(1+sqrt(2)*y[2]) - (1-y[1]^2/2))/
  (1-y[1]^2/2)
];

E_to_TPp := unapply(eval(subs({y=yx0,r=rx},E_to_TPp_xyr)),x); #@ E_to_TPp
E_to_TCp := unapply(eval(subs({y=yx0,r=rx},E_to_TCp_xyr)),x); #@ E_to_TCp

#@ E_to_TPp_jacobian 
E_to_TPp_jacobian :=
 4*sqrt(2)*(1-x[1]^2)/(1-y[1]^2/2)/(1-y[2]/sqrt(2));

#@ TTP_to_TPp 
TTP_to_TPp := (t) -> [(t[1]*t[3]-1)/(t[1]+t[3]),(t[2]*t[4]+1)/(t[4]-t[2])];

#@ TTC_to_TCp 
TTC_to_TCp := (z) -> [-z[1]*z[3],-z[2]/z[4]];

#@ TPp_p
TPp_p[0] := (a) -> 1+a[2]^2/2+a[1]^2*(a[2]^2+3/2);
TPp_p[1] := (a) -> 1+a[2]^2/2-a[1]^2*(a[2]^2+3/2);
TPp_p[2] := (a) -> 3/4*a[2]^2+1;
TPp_p[3] := (a) -> TPp_p[1](a)^2 + 4*a[1]^2*TPp_p[2](a);
TPp_p[4] := (a) -> sqrt((sqrt(TPp_p[3](a))+TPp_p[1](a))/TPp_p[2](a)/2);

#@ TPp_to_E 
TPp_to_E := (a) -> 
[a[1]/sqrt(2)*(2+(1-2/TPp_p[4](a)^2)/(1+a[2]^2)),
 a[2]/2*(2/TPp_p[4](a)-TPp_p[4](a))/(1+a[2]^2),
 TPp_p[4](a),
 sqrt(2)*((1+a[2]^2/2)*TPp_p[4](a)-1/TPp_p[4](a))/(1+a[2]^2)];

Tp_p[0] := (s) -> sqrt(9*s[1]^2-2*s[1]*s[3]+s[3]^2-8*s[1]+8*s[3]+8);
Tp_p[1] := (s) -> sqrt((s[1]*s[3]+4*s[1]-1+Tp_p[0](s))/(s[1]+1));

#@ Tp_to_E 
Tp_to_E := (s) -> [
 sqrt(8)*s[2]/(4-s[1]+s[3]+Tp_p[0](s)),
 sqrt(8)*sqrt(7+s[3])*s[4]/(8+3*s[1]+s[3]+Tp_p[0](s))/Tp_p[1](s),
 sqrt(2)*Tp_p[1](s)/sqrt(7+s[3]),
 -2*sqrt(7+s[3])*(1+s[3])*(1-2*s[1]+s[3])/(10+9*s[3]-5*s[1]+s[1]*s[3]+s[3]^2+(3+s[3])*Tp_p[0](s))/Tp_p[1](s)
];

#@ TCp_to_E 
TCp_to_E := (z) -> Tp_to_E(TC_to_T(z));

# The definitions below give an action of a group generated by L,M,N
# with M=1 and so LM = ML; this is not an action of the usual group G

#@ act_TPp
act_TPp[1]     := (a) -> [ a[1], a[2]]:
act_TPp[LL]    := (a) -> [-a[1],-a[2]]:
act_TPp[M]     := (a) -> [ a[1], a[2]]:
act_TPp[LLM]   := (a) -> [-a[1],-a[2]]:
act_TPp[N]     := (a) -> [ a[1],-a[2]]:
act_TPp[LLN]   := (a) -> [-a[1], a[2]]:
act_TPp[MN]    := (a) -> [ a[1],-a[2]]:
act_TPp[LLMN]  := (a) -> [-a[1], a[2]]:
act_TPp[L]     := (a) -> [-a[2]*TPp_p[4](a)/sqrt(2), a[1]/TPp_p[4](a)*sqrt(2)];
act_TPp[LLL]   := (a) -> [ a[2]*TPp_p[4](a)/sqrt(2),-a[1]/TPp_p[4](a)*sqrt(2)];
act_TPp[LM]    := (a) -> [-a[2]*TPp_p[4](a)/sqrt(2), a[1]/TPp_p[4](a)*sqrt(2)];
act_TPp[LLLM]  := (a) -> [ a[2]*TPp_p[4](a)/sqrt(2),-a[1]/TPp_p[4](a)*sqrt(2)];
act_TPp[LN]    := (a) -> [ a[2]*TPp_p[4](a)/sqrt(2), a[1]/TPp_p[4](a)*sqrt(2)];
act_TPp[LLLN]  := (a) -> [-a[2]*TPp_p[4](a)/sqrt(2),-a[1]/TPp_p[4](a)*sqrt(2)];
act_TPp[LMN]   := (a) -> [ a[2]*TPp_p[4](a)/sqrt(2), a[1]/TPp_p[4](a)*sqrt(2)];
act_TPp[LLLMN] := (a) -> [-a[2]*TPp_p[4](a)/sqrt(2),-a[1]/TPp_p[4](a)*sqrt(2)];

# Images of curves

#@ c_TPp
c_TPp[ 0] := (t) -> [1/cos(t)/sqrt(2),infinity];
c_TPp[ 1] := (t) -> [ sin(t),tan(t)*sqrt(2)];
c_TPp[ 2] := (t) -> [-sin(t),tan(t)*sqrt(2)];
c_TPp[ 3] := (t) -> [infinity,sqrt(3/2) * tan(t)];
c_TPp[ 4] := (t) -> [-sin(t)/sqrt(2),infinity];
c_TPp[ 5] := (t) -> [sqrt(2)*tan(t/2)/sqrt(2+sin(t/2)^2),0];
c_TPp[ 6] := (t) -> [0, tan(t/2)*sqrt(2)];
c_TPp[ 7] := (t) -> [sqrt(2)*tan(t/2)/sqrt(2+sin(t/2)^2),0];
c_TPp[ 8] := (t) -> [0,-tan(t/2)*sqrt(2)];
c_TPp[ 9] := (t) -> [ 1/sqrt(3)/tan(t/2),   tan(t/2)];
c_TPp[10] := (t) -> [-1/sqrt(3)*tan(t/2), 1/tan(t/2)];
c_TPp[11] := (t) -> [ 1/sqrt(3)/tan(t/2),   tan(t/2)];
c_TPp[12] := (t) -> [ 1/sqrt(3)*tan(t/2),-1/tan(t/2)];
c_TPp[13] := (t) -> [ (1/3)*(cos(t)^2+sin(t)*sqrt(4-cos(t)^2)+2)/cos(t),
                      (1/3)*(cos(t)^2-sin(t)*sqrt(4-cos(t)^2)+2)*sqrt(3)/(cos(t)*sin(t))];
c_TPp[14] := (t) -> [-(1/3)*(cos(t)^2-sin(t)*sqrt(4-cos(t)^2)+2)/cos(t),
                      (1/3)*(cos(t)^2+sin(t)*sqrt(4-cos(t)^2)+2)*sqrt(3)/(cos(t)*sin(t))];
c_TPp[15] := (t) -> [ (1/3)*(cos(t)^2+sin(t)*sqrt(4-cos(t)^2)+2)/cos(t),
                     -(1/3)*(cos(t)^2-sin(t)*sqrt(4-cos(t)^2)+2)*sqrt(3)/(cos(t)*sin(t))];
c_TPp[16] := (t) -> [ (1/3)*(cos(t)^2-sin(t)*sqrt(4-cos(t)^2)+2)/cos(t),
                      (1/3)*(cos(t)^2+sin(t)*sqrt(4-cos(t)^2)+2)*sqrt(3)/(cos(t)*sin(t))];

#@ c_TCp
c_TCp[ 0] := (t) -> [(cos(t) + I/sqrt(2))/(cos(t) - I/sqrt(2)),-1];
c_TCp[ 1] := (t) -> [(I - sin(t))/(I + sin(t)),
                     (I*cos(t)/sqrt(2)-sin(t))/(I*cos(t)/sqrt(2)+sin(t))];
c_TCp[ 2] := (t) -> [(I + sin(t))/(I - sin(t)),
                     (I*cos(t)/sqrt(2)-sin(t))/(I*cos(t)/sqrt(2)+sin(t))];
c_TCp[ 3] := (t) -> [-1,(cos(t) + I * sqrt(3/2) * sin(t))/
                        (cos(t) - I * sqrt(3/2) * sin(t))];
c_TCp[ 4] := (t) -> [(I*sqrt(2) + sin(t))/(I*sqrt(2) - sin(t)),-1];
c_TCp[ 5] := (t) -> [(1+8*cos(t)-cos(t)^2+4*sqrt(2)*I*sin(t)*sqrt(2+sin(t/2)^2))/
                      (9-cos(t)^2),1];
c_TCp[ 6] := (t) -> [1,(I*sin(t)/sqrt(2)+cos(t)-1)/(I*sin(t)/sqrt(2)-cos(t)+1)];
c_TCp[ 7] := (t) -> [(1+8*cos(t)-cos(t)^2+4*sqrt(2)*I*sin(t)*sqrt(2+sin(t/2)^2))/
                       (9-cos(t)^2),1];
c_TCp[ 8] := (t) -> [1,(I*sin(t)/sqrt(2)-cos(t)+1)/(I*sin(t)/sqrt(2)+cos(t)-1)];
c_TCp[ 9] := (t) -> [(cos(t)-1-I*sin(t)/sqrt(3))/(cos(t)-1+I*sin(t)/sqrt(3)),
                     (cos(t)-1+I*sin(t))/(-cos(t)+1+I*sin(t))];
c_TCp[10] := (t) -> [(cos(t)-1-I*sin(t)*sqrt(3))/(-cos(t)+1-I*sin(t)*sqrt(3)),
                     (cos(t)-1-I*sin(t))/(cos(t)-1+I*sin(t))];
c_TCp[11] := (t) -> [(cos(t)-1-I*sin(t)/sqrt(3))/(cos(t)-1+I*sin(t)/sqrt(3)),
                     (cos(t)-1+I*sin(t))/(-cos(t)+1+I*sin(t))];
c_TCp[12] := (t) -> [(cos(t)-1+I*sin(t)*sqrt(3))/(-cos(t)+1+I*sin(t)*sqrt(3)),
                     (cos(t)-1+I*sin(t))/(cos(t)-1-I*sin(t))];

for i from 13 to 16 do
 c_TCp[i] := unapply(simplify(TP_to_TC(c_TPp[i](t))),t);
od:

# Images of vertices

#@ v_TCp
#@ v_TPp
for i from 0 to 20 do
 v_TCp[i]  := combine(simplify(expand(E_to_TCp( v_E0[i]))));
 v_TPp[i]  := combine(simplify(expand(rationalize(map(SC1_to_R,v_TCp[i])))));
od:

##################################################
##################################################
# The map E_to_TPpa is the composite of E_to_TPp with the automorphism
# a |-> [a[1]/sqrt(2),a[2]/2] of (R u {infinity})^2.  Some formulae
# for TPpa are tidier than the analogous formulae for TPp.

#@ E_to_TPpa_xyr 
E_to_TPpa_xyr := [x[1]/(1-y[2]*sqrt(2)),x[2]/y[1]/(1+y[2]*sqrt(2))];

#@ E_to_TCpa_xyr 
E_to_TCpa_xyr := [(2*y[1]^2*y[2]+8*y[2]-2*sqrt(2)*y[1]^2-2*sqrt(2)-(8*I)*sqrt(2)*x[1])/
                  (2*sqrt(2)*y[1]^2-2*y[1]^2*y[2]-6*sqrt(2)+8*y[2]),
                 (10*y[1]^2*y[2]+6*sqrt(2)*y[1]^2+(8*I)*sqrt(2)*x[2]*y[1]-2*sqrt(2))/
		  (6*y[1]^2*y[2]+2*sqrt(2)*y[1]^2+2*sqrt(2))];

#@ E_to_TPpa 
E_to_TPpa := unapply(eval(subs({y=yx0,r=rx},E_to_TPpa_xyr)),x);

#@ E_to_TCpa 
E_to_TCpa := unapply(eval(subs({y=yx0,r=rx},E_to_TCpa_xyr)),x);

#@ E_to_TPpa_jacobian 
E_to_TPpa_jacobian :=
 64*(1-x[1]^2)*(1-y[1]^2/2)/
  ((-y[1]^2*sqrt(2)+y[1]^2*y[2]+3*sqrt(2)-4*y[2])*(y[1]^2*sqrt(2)+3*y[1]^2*y[2]+sqrt(2)));
 
#@ TPp_to_TPpa 
TPp_to_TPpa := (t) -> [t[1]/sqrt(2),t[2]/2];

#@ TCp_to_TCpa 
TCp_to_TCpa := (z) -> [-mob(-z[1],3-2*sqrt(2)),-mob(-z[2],1/3)];

#@ TPpa_to_TPp 
TPpa_to_TPp := (t) -> [t[1]*sqrt(2),t[2]*2];

#@ TCpa_to_TCp 
TCpa_to_TCp := (z) -> [-mob(-z[1],-3+2*sqrt(2)),-mob(-z[2],-1/3)];

#@ TTP_to_TPpa  
TTP_to_TPpa  := (t) -> [(t[1]*t[3]-1)/(t[1]+t[3])/sqrt(2),(t[2]*t[4]+1)/(t[4]-t[2])/2];

#@ TTC_to_TCpa  
TTC_to_TCpa  := (z) -> [-mob(z[1]*z[3],3-2*sqrt(2)),-mob(z[2]/z[4],1/3)];

#@ TPpa_p
TPpa_p[0] := (a) -> 1 + 2*a[2]^2 + a[1]^2*(3 + 8*a[2]^2);
TPpa_p[1] := (a) -> 1 + 2*a[2]^2 - a[1]^2*(3 + 8*a[2]^2);
TPpa_p[2] := (a) -> 1 + 3*a[2]^2;
TPpa_p[3] := (a) -> TPpa_p[1](a)^2 + 8*a[1]^2*TPpa_p[2](a);
TPpa_p[4] := (a) -> sqrt((sqrt(TPpa_p[3](a))+TPpa_p[1](a))/TPpa_p[2](a)/2);

# A (discontinuous) section of the map E_to_TPpa

#@ TPpa_to_E 
TPpa_to_E := (a) -> 
[a[1]*(2+(1-2/TPpa_p[4](a)^2)/(1+4*a[2]^2)),
 a[2]*(2/TPpa_p[4](a)-TPpa_p[4](a))/(1+4*a[2]^2),
 TPpa_p[4](a),
 sqrt(2)*((1+2*a[2]^2)*TPpa_p[4](a)-1/TPpa_p[4](a))/(1+4*a[2]^2)];

#@ Tpa_p
Tpa_p[0] := (s) ->  (4*s[1]*s[3]- 8*s[1]-6*s[3]+14)/((1+s[3])*(1+s[1]));
Tpa_p[1] := (s) -> -(6*s[1]*s[3]-14*s[1]-4*s[3]+ 8)/((1+s[3])*(1+s[1]));
Tpa_p[2] := (s) ->  (4-2*s[3])/(1+s[3]);
Tpa_p[3] := (s) -> (4*(3*s[1]*s[3]-7*s[1]-2*s[3]+4)^2+16*(1-s[1]^2)*(2-s[3])*(1+s[3]))/((1+s[1])^2*(1+s[3])^2);
Tpa_p[4] := (s) -> sqrt((13*s[1]*s[3]^2-46*s[1]*s[3]-25*s[3]^2+41*s[1]+98*s[3]-97)/2/(2-s[3])/(sqrt((3*s[1]*s[3]-7*s[1]-2*s[3]+4)^2+4*(1-s[1]^2)*(2-s[3])*(s[3]+1))+11-5*s[3]) + (7-3*s[3])/2/(2-s[3]));

#@ Tpa_to_E 
Tpa_to_E := (s) -> [
s[2]*(2 + (1-2/Tpa_p[4](s)^2)*(1+s[3])/(5-3*s[3]))/(1+s[1]),
s[4]*(2/Tpa_p[4](s)-Tpa_p[4](s))/(5-3*s[3]),
Tpa_p[4](s),
sqrt(2)*((3-s[3])*Tpa_p[4](s)-(1+s[3])/Tpa_p[4](s))/(5-3*s[3])
];

#@ TCpa_to_E 
TCpa_to_E := (z) -> Tpa_to_E(TC_to_T(z));

# The definitions below give an action of a group generated by L,M,N
# with M=1 and so LM = ML; this is not an action of the usual group G

#@ act_TPpa
act_TPpa[1]     := (a) -> [ a[1], a[2]]:
act_TPpa[LL]    := (a) -> [-a[1],-a[2]]:
act_TPpa[M]     := (a) -> [ a[1], a[2]]:
act_TPpa[LLM]   := (a) -> [-a[1],-a[2]]:
act_TPpa[N]     := (a) -> [ a[1],-a[2]]:
act_TPpa[LLN]   := (a) -> [-a[1], a[2]]:
act_TPpa[MN]    := (a) -> [ a[1],-a[2]]:
act_TPpa[LLMN]  := (a) -> [-a[1], a[2]]:
act_TPpa[L]     := (a) -> [-a[2]*TPpa_p[4](a), a[1]/TPpa_p[4](a)];
act_TPpa[LLL]   := (a) -> [ a[2]*TPpa_p[4](a),-a[1]/TPpa_p[4](a)];
act_TPpa[LM]    := (a) -> [-a[2]*TPpa_p[4](a), a[1]/TPpa_p[4](a)];
act_TPpa[LLLM]  := (a) -> [ a[2]*TPpa_p[4](a),-a[1]/TPpa_p[4](a)];
act_TPpa[LN]    := (a) -> [ a[2]*TPpa_p[4](a), a[1]/TPpa_p[4](a)];
act_TPpa[LLLN]  := (a) -> [-a[2]*TPpa_p[4](a),-a[1]/TPpa_p[4](a)];
act_TPpa[LMN]   := (a) -> [ a[2]*TPpa_p[4](a), a[1]/TPpa_p[4](a)];
act_TPpa[LLLMN] := (a) -> [-a[2]*TPpa_p[4](a),-a[1]/TPpa_p[4](a)];

# Images of curves

#@ c_TPpa
c_TPpa[ 0] := (t) -> [1/cos(t)/2,infinity];
c_TPpa[ 1] := (t) -> [ sin(t)/sqrt(2),tan(t)/sqrt(2)];
c_TPpa[ 2] := (t) -> [-sin(t)/sqrt(2),tan(t)/sqrt(2)];
c_TPpa[ 3] := (t) -> [infinity,sqrt(3/8) * tan(t)];
c_TPpa[ 4] := (t) -> [-sin(t)/2,infinity];
c_TPpa[ 5] := (t) -> [tan(t/2)/sqrt(2+sin(t/2)^2),0];
c_TPpa[ 6] := (t) -> [0, tan(t/2)/sqrt(2)];
c_TPpa[ 7] := (t) -> [tan(t/2)/sqrt(2+sin(t/2)^2),0];
c_TPpa[ 8] := (t) -> [0,-tan(t/2)/sqrt(2)];
c_TPpa[ 9] := (t) -> [ 1/sqrt(6)/tan(t/2),   tan(t/2)/2];
c_TPpa[10] := (t) -> [-1/sqrt(6)*tan(t/2), 1/tan(t/2)/2];
c_TPpa[11] := (t) -> [ 1/sqrt(6)/tan(t/2),   tan(t/2)/2];
c_TPpa[12] := (t) -> [ 1/sqrt(6)*tan(t/2),-1/tan(t/2)/2];
c_TPpa[13] := (t) -> [ (1/3/sqrt(2))*(cos(t)^2+sin(t)*sqrt(4-cos(t)^2)+2)/cos(t),
                       (1/6)*(cos(t)^2-sin(t)*sqrt(4-cos(t)^2)+2)*sqrt(3)/(cos(t)*sin(t))];
c_TPpa[14] := (t) -> [-(1/3/sqrt(2))*(cos(t)^2-sin(t)*sqrt(4-cos(t)^2)+2)/cos(t),
                       (1/6)*(cos(t)^2+sin(t)*sqrt(4-cos(t)^2)+2)*sqrt(3)/(cos(t)*sin(t))];
c_TPpa[15] := (t) -> [ (1/3/sqrt(2))*(cos(t)^2+sin(t)*sqrt(4-cos(t)^2)+2)/cos(t),
                      -(1/6)*(cos(t)^2-sin(t)*sqrt(4-cos(t)^2)+2)*sqrt(3)/(cos(t)*sin(t))];
c_TPpa[16] := (t) -> [ (1/3/sqrt(2))*(cos(t)^2-sin(t)*sqrt(4-cos(t)^2)+2)/cos(t),
                       (1/6)*(cos(t)^2+sin(t)*sqrt(4-cos(t)^2)+2)*sqrt(3)/(cos(t)*sin(t))];

#@ c_TCpa
c_TCpa[ 0] := (t) -> [(cos(t) + I/2)/(cos(t) - I/2),-1];
c_TCpa[ 1] := (t) -> [(1 + I*sin(t)/sqrt(2))/(1 - I*sin(t)/sqrt(2)),mob(exp(2*I*t),-(3-sqrt(8)))];
c_TCpa[ 2] := (t) -> [(1 - I*sin(t)/sqrt(2))/(1 + I*sin(t)/sqrt(2)),mob(exp(2*I*t),-(3-sqrt(8)))];
c_TCpa[ 3] := (t) -> [-1,mob(exp(2*I*t),-(11-sqrt(96))/5)];
c_TCpa[ 4] := (t) -> [mob(exp(I*t),2+sqrt(5))*mob(exp(I*t),2-sqrt(5)),-1];
c_TCpa[ 5] := (t) -> [-I*(I*cos(t)^2+2*sqrt(10-2*cos(t))*sin(t)-(6*I)*cos(t)-3*I)/(cos(t)^2-2*cos(t)-7), 1];
c_TCpa[ 6] := (t) -> [ 1,mob(exp(I*t),-(3-sqrt(8)))];
c_TCpa[ 7] := (t) -> [-I*(I*cos(t)^2+2*sqrt(10-2*cos(t))*sin(t)-(6*I)*cos(t)-3*I)/(cos(t)^2-2*cos(t)-7), 1];
c_TCpa[ 8] := (t) -> [ 1,mob(exp(I*t),-(3+sqrt(8)))];
c_TCpa[ 9] := (t) -> [-mob(exp(I*t), (7+2*sqrt(6))/5), mob(exp(I*t),-1/3)];
c_TCpa[10] := (t) -> [ mob(exp(I*t),-(7+2*sqrt(6))/5),-mob(exp(I*t),   3)];
c_TCpa[11] := (t) -> [-mob(exp(I*t), (7+2*sqrt(6))/5), mob(exp(I*t),-1/3)];
c_TCpa[12] := (t) -> [ mob(exp(I*t),-(7-2*sqrt(6))/5),-mob(exp(I*t), 1/3)];

for i from 13 to 16 do
 c_TCpa[i] := unapply(simplify(TP_to_TC(c_TPpa[i](t))),t);
od:

# Images of vertices

#@ v_TCpa
#@ v_TPpa
for i from 0 to 20 do
 v_TCpa[i]  := combine(simplify(expand(E_to_TCpa( v_E0[i]))));
 v_TPpa[i]  := combine(simplify(expand(rationalize(map(SC1_to_R,v_TCpa[i])))));
od:

######################################################################
######################################################################
# E_to_TCm is map to the 2-torus, invariant under LM = lambda mu

#@ E_to_TPm_xyr 
E_to_TPm_xyr := [
 -(x[1]*y[1]*sqrt(2)*y[2]+x[2]*y[1]*sqrt(2)*y[2]+sqrt(2)*x[1]+sqrt(2)*x[2]+2*x[1]*y[1]-2*x[2]*y[1])/
  (r[1]*r[2]*y[1]^2+y[1]^2*y[2]^2-2*r[1]*r[2]-2*x[1]*x[2]-2*y[1]^2+2*y[1]*y[2]+1),
  (x[1]*y[1]*sqrt(2)*y[2]+x[2]*y[1]*sqrt(2)*y[2]-sqrt(2)*x[1]-sqrt(2)*x[2]+2*x[1]*y[1]-2*x[2]*y[1])/
  (r[1]*r[2]*y[1]^2+y[1]^2*y[2]^2-2*r[1]*r[2]-2*x[1]*x[2]-2*y[1]^2-2*y[1]*y[2]+1)
];

#@ E_to_TCm_xyr 
E_to_TCm_xyr := [
-(I*x[1] + y[1]*(1 - y[2]/sqrt(2)) - 1/sqrt(2))*
 (I*x[2] - y[1]*(1 + y[2]/sqrt(2)) - 1/sqrt(2))/(r[1]*r[2]*(1-y[1]^2/2)),
-(I*x[1] - y[1]*(1 - y[2]/sqrt(2)) - 1/sqrt(2))*
 (I*x[2] + y[1]*(1 + y[2]/sqrt(2)) - 1/sqrt(2))/(r[1]*r[2]*(1-y[1]^2/2))
];

#@ E_to_TPm    
E_to_TPm    := unapply(eval(subs({y=yx0,r=rx},E_to_TPm_xyr)),x);

#@ E_to_TCm    
E_to_TCm    := unapply(eval(subs({y=yx0,r=rx},E_to_TCm_xyr)),x);

#@ TTC_to_TCm   
TTC_to_TCm   := (z) -> [-z[1]*z[4],-z[2]*z[3]];

#@ TTP_to_TPm   
TTP_to_TPm   := (t) -> [(t[1]*t[4]-1)/(t[1]+t[4]),(t[2]*t[3]-1)/(t[2]+t[3])];

#@ E_to_TPm_jacobian  
E_to_TPm_jacobian  := 4*sqrt(2)*(3/2 - (1-y[1]^2/2)*(1-y[2]^2/2) - x[1]*x[2])/((1-y[1]^2/2)*(1-y[2]^2/2));

########################################

#@ act_TCm
act_TCm[1]     := (z) -> [  z[1],  z[2]];
act_TCm[LL]    := (z) -> [1/z[1],1/z[2]];
act_TCm[LM]    := (z) -> [  z[1],  z[2]];
act_TCm[LLLM]  := (z) -> [1/z[1],1/z[2]];
act_TCm[LN]    := (z) -> [  z[2],  z[1]];
act_TCm[LLLN]  := (z) -> [1/z[2],1/z[1]];
act_TCm[MN]    := (z) -> [  z[2],  z[1]];
act_TCm[LLMN]  := (z) -> [1/z[2],1/z[1]];

########################################

#@ v_TCm
for i from 0 to num_vertices_E-1 do
 v_TCm[i]  := simplify(expand(E_to_TCm( v_E0[i])));
od:

#@ v_TPm
v_TPm[ 0] := [0,0];
v_TPm[ 1] := [0,0];
v_TPm[ 2] := [ 1, 1] *~ ((1+sqrt(3))/sqrt(2));
v_TPm[ 3] := [ 1, 1] *~ ((1+sqrt(3))/sqrt(2));
v_TPm[ 4] := [-1,-1] *~ ((1+sqrt(3))/sqrt(2));
v_TPm[ 5] := [-1,-1] *~ ((1+sqrt(3))/sqrt(2));
v_TPm[ 6] := [ 1, 1];
v_TPm[ 8] := [-1,-1];
v_TPm[ 7] := [infinity,infinity];
v_TPm[ 9] := [infinity,infinity];
v_TPm[10] := [       0,infinity];
v_TPm[13] := [       0,infinity];
v_TPm[11] := [infinity,       0];
v_TPm[12] := [infinity,       0];
v_TPm[14] := [ 1, 1] /~ sqrt(3);
v_TPm[19] := [ 1, 1] /~ sqrt(3); 
v_TPm[16] := [-1,-1] /~ sqrt(3);
v_TPm[21] := [-1,-1] /~ sqrt(3); 
v_TPm[17] := [ 1,-1];
v_TPm[20] := [ 1,-1];
v_TPm[15] := [-1, 1];
v_TPm[18] := [-1, 1];

#for i from 0 to num_curves_E - 1 do
# c_TC[i]   := unapply(combine(rationalize(E_to_TC( c_E0[i](t)))),t);
# c_TCp[i]  := unapply(combine(rationalize(E_to_TCp( c_E0[i](t)))),t);
# c_TCm[i]  := unapply(simplify(rationalize(factor(expand(combine(E_to_TCm( c_E0[i](t))))))),t);
# c_TCmq[i] := unapply(combine(factor(expand(rationalize(factor(expand(combine(E_to_TCmq(c_E0[i](t))))))))),t);
#od;

#@ c_TPm
c_TPm[ 0] := (t) -> [ 1, 1] *~ ((sqrt(sin(t-Pi/4)^4+cos(t-Pi/4)^2)+sin(t-Pi/4)^2)/cos(t-Pi/4));
c_TPm[ 1] := (t) -> [ 1, 1] *~ sin(t);
c_TPm[ 2] := (t) -> [-1, 1] *~ (sqrt(2) * tan(t));
c_TPm[ 3] := (t) -> [ (1/2)*sqrt(2)*(1+sqrt(3))*sin(t)/(1-cos(t)),
                      (1/2)*sqrt(2)*(1+sqrt(3))*sin(t)/(1+cos(t))];
c_TPm[ 4] := (t) -> [-(1/2)*sqrt(2)*(1+sqrt(3))*sin(t)/(1+cos(t)),
                     -(1/2)*sqrt(2)*(1+sqrt(3))*sin(t)/(1-cos(t))];
c_TPm[ 5] := (t) -> [ sqrt(2)/sin(t)*((sqrt(2+sin(t/2)^2)-1)*(sqrt(2-sin(t/2)^2)+1)-cos(t/2)^2),
                      -1/(sqrt(2)/sin(t)*((sqrt(2+sin(t/2)^2)+1)*(sqrt(2-sin(t/2)^2)+1)+cos(t/2)^2))];
c_TPm[ 6] := (t) -> [ -1/(sqrt(2)/sin(t)*((sqrt(2+sin(t/2)^2)+1)*(sqrt(2-sin(t/2)^2)+1)+cos(t/2)^2)),
                      sqrt(2)/sin(t)*((sqrt(2+sin(t/2)^2)-1)*(sqrt(2-sin(t/2)^2)+1)-cos(t/2)^2)];
c_TPm[ 7] := unapply(c_TPm[ 6](t),t);
c_TPm[ 8] := unapply(c_TPm[ 5](t),t);
c_TPm[ 9] := (t) -> [(sqrt(4-cos(t)^2)-sin(t)^2+sqrt(3)*cos(t))/sin(t)/(sqrt(3)+cos(t)),
                     (sqrt(4-cos(t)^2)-sin(t)^2-sqrt(3)*cos(t))/sin(t)/(sqrt(3)-cos(t))];
c_TPm[10] := (t) -> [(sqrt(3)*cos(t) - sqrt(4-cos(t)^2))/2/sin(t),
                     (sqrt(3)*cos(t) + sqrt(4-cos(t)^2))/2/sin(t)];
c_TPm[11] := unapply(c_TPm[10](t),t);
c_TPm[12] := unapply(c_TPm[ 9](t),t);
c_TPm[13] := (t) -> [sqrt(3+sin(t)^2)/cos(t)/sqrt(3),sqrt(3)*cos(t)/sqrt(3+sin(t)^2)];
c_TPm[14] := (t) -> [-cos(t)*(cos(t)^2*sqrt(3)+2*sqrt(3)+3*sqrt(4-cos(t)^2))/(sin(t)*(cos(t)^2+8)),
                     (1/3)*(cos(t)^2*sqrt(3)+2*sqrt(3)+3*sqrt(4-cos(t)^2))/(sin(t)*cos(t))];
c_TPm[15] := (t) -> [(1/3)*(cos(t)^2*sqrt(3)+2*sqrt(3)+3*sqrt(4-cos(t)^2))/(sin(t)*cos(t)),
                     -cos(t)*(cos(t)^2*sqrt(3)+2*sqrt(3)+3*sqrt(4-cos(t)^2))/(sin(t)*(cos(t)^2+8))];
c_TPm[16] := (t) -> [sqrt(3)*cos(t)/sqrt(3+sin(t)^2),sqrt(3+sin(t)^2)/cos(t)/sqrt(3)];


#@ c_TCm
c_TCm[ 0] := (t) -> [(I*sqrt(2)*sin(t)+I*sqrt(2)*cos(t)+2*sin(t)*cos(t)-1)/sqrt(4-cos(2*t)^2),
                     (I*sqrt(2)*sin(t)+I*sqrt(2)*cos(t)+2*sin(t)*cos(t)-1)/sqrt(4-cos(2*t)^2)];
c_TCm[ 1] := (t) -> [(I - sin(t))/(I + sin(t)),(I - sin(t))/(I + sin(t))];
c_TCm[ 2] := (t) -> [-(I*sqrt(2)*cos(t)+2*sin(t))/(-I*sqrt(2)*cos(t)+2*sin(t)),
                     -(-I*sqrt(2)*cos(t)+2*sin(t))/(I*sqrt(2)*cos(t)+2*sin(t))];
c_TCm[ 3] := (t) -> [(I*sqrt(2)*sin(t)-sqrt(3)*cos(t)-1)/(sqrt(3)+cos(t)),
                     (-sqrt(3)*cos(t)+1-I*sqrt(2)*sin(t))/(-sqrt(3)+cos(t))];
c_TCm[ 4] := (t) -> [(I*sqrt(2)*sin(t)-sqrt(3)*cos(t)+1)/(-sqrt(3)+cos(t)),
                     (-sqrt(3)*cos(t)-1-I*sqrt(2)*sin(t))/(sqrt(3)+cos(t))];
c_TCm[ 5] := (t) -> [(((2*I)*sqrt(2)*sin(t)+2*cos(t)+2)*sqrt(2+sin((1/2)*t)^2)+(2*I)*sqrt(2)*sin(t)+4*cos(t)-4)*
                       sqrt(2-sin((1/2)*t)^2)/(9-cos(t)^2),
		     ((-(2*I)*sqrt(2)*sin(t)+2*cos(t)+2)*sqrt(2+sin((1/2)*t)^2)+(2*I)*sqrt(2)*sin(t)-4*cos(t)+4)*
		       sqrt(2-sin((1/2)*t)^2)/(9-cos(t)^2)];
c_TCm[ 6] := (t) -> [((-(2*I)*sqrt(2)*sin(t)+2*cos(t)+2)*sqrt(2+sin((1/2)*t)^2)+(2*I)*sqrt(2)*sin(t)-4*cos(t)+4)*
                       sqrt(2-sin((1/2)*t)^2)/(9-cos(t)^2),
		     (((2*I)*sqrt(2)*sin(t)+2*cos(t)+2)*sqrt(2+sin((1/2)*t)^2)+(2*I)*sqrt(2)*sin(t)+4*cos(t)-4)*
		       sqrt(2-sin((1/2)*t)^2)/(9-cos(t)^2)];
c_TCm[ 7] := (t) -> [((-(2*I)*sqrt(2)*sin(t)+2*cos(t)+2)*sqrt(2+sin((1/2)*t)^2)+(2*I)*sqrt(2)*sin(t)-4*cos(t)+4)*
                       sqrt(2-sin((1/2)*t)^2)/(9-cos(t)^2),
		     (((2*I)*sqrt(2)*sin(t)+2*cos(t)+2)*sqrt(2+sin((1/2)*t)^2)+(2*I)*sqrt(2)*sin(t)+4*cos(t)-4)*
		       sqrt(2-sin((1/2)*t)^2)/(9-cos(t)^2)];
c_TCm[ 8] := (t) -> [(((2*I)*sqrt(2)*sin(t)+2*cos(t)+2)*sqrt(2+sin((1/2)*t)^2)+(2*I)*sqrt(2)*sin(t)+4*cos(t)-4)*
                       sqrt(2-sin((1/2)*t)^2)/(9-cos(t)^2),
		     ((-(2*I)*sqrt(2)*sin(t)+2*cos(t)+2)*sqrt(2+sin((1/2)*t)^2)+(2*I)*sqrt(2)*sin(t)-4*cos(t)+4)*
		       sqrt(2-sin((1/2)*t)^2)/(9-cos(t)^2)];
c_TCm[ 9] := (t) -> [(sin(t)+I*cos(t))*(sin(t)+I*sqrt(3))/sqrt(4-cos(t)^2),
                     (sin(t)-I*cos(t))*(sin(t)+I*sqrt(3))/sqrt(4-cos(t)^2)];
c_TCm[10] := (t) -> [(-(2*I)*sin(t)+sqrt(3)*cos(t))/sqrt(4-cos(t)^2),
                      ((2*I)*sin(t)-sqrt(3)*cos(t))/sqrt(4-cos(t)^2)];
c_TCm[11] := (t) -> [(-(2*I)*sin(t)+sqrt(3)*cos(t))/sqrt(4-cos(t)^2),
                      ((2*I)*sin(t)-sqrt(3)*cos(t))/sqrt(4-cos(t)^2)];
c_TCm[12] := (t) -> [(sin(t)+I*cos(t))*(sin(t)+I*sqrt(3))/sqrt(4-cos(t)^2),
                     (sin(t)-I*cos(t))*(sin(t)+I*sqrt(3))/sqrt(4-cos(t)^2)];
c_TCm[13] := (t) -> [ ( I*sqrt(4-cos(t)^2)*sqrt(3)*cos(t)+2*cos(t)^2-2)/(cos(t)^2+2),
                     -(-I*sqrt(4-cos(t)^2)*sqrt(3)*cos(t)+2*cos(t)^2-2)/(cos(t)^2+2)];
c_TCm[14] := (t) -> [(I*sqrt(3)*cos(t)-2*sin(t))*(sin(t)*cos(t)^2+(3*I)*cos(t)*sqrt(4-cos(t)^2)-4*sin(t))/
                      ((cos(t)^2+2)*(4-cos(t)^2)),
		     (I*sqrt(3)*cos(t)+2*sin(t))*(sin(t)*cos(t)^2+(3*I)*cos(t)*sqrt(4-cos(t)^2)-4*sin(t))/
		      ((cos(t)^2+2)*(4-cos(t)^2))];
c_TCm[15] := (t) -> [(I*sqrt(3)*cos(t)+2*sin(t))*(sin(t)*cos(t)^2+(3*I)*cos(t)*sqrt(4-cos(t)^2)-4*sin(t))/
                      ((cos(t)^2+2)*(4-cos(t)^2)),
		     (I*sqrt(3)*cos(t)-2*sin(t))*(sin(t)*cos(t)^2+(3*I)*cos(t)*sqrt(4-cos(t)^2)-4*sin(t))/
		      ((cos(t)^2+2)*(4-cos(t)^2))];
c_TCm[16] := (t) -> [-(-I*sqrt(4-cos(t)^2)*sqrt(3)*cos(t)+2*cos(t)^2-2)/(cos(t)^2+2),
                      ( I*sqrt(4-cos(t)^2)*sqrt(3)*cos(t)+2*cos(t)^2-2)/(cos(t)^2+2)];


######################################################################
######################################################################
# E_to_TCmq is E_to_TCm followed by an isogeny of degree two.

#@ E_to_TCmq_xyr 
E_to_TCmq_xyr := [
 -(2*I*x[1]*y[1] - (1-y[1]^2/2) + y[1]^2*(1 - sqrt(2)*y[2]))*
  (2*I*x[2]*y[1] + (1-y[1]^2/2) - y[1]^2*(1 + sqrt(2)*y[2]))/(1-y[1]^2/2)^2,
  (2*I*x[1] + y[1]^2/sqrt(2)*(1-y[2]/sqrt(2)) - y[2])*
  (2*I*x[2] + y[1]^2/sqrt(2)*(1+y[2]/sqrt(2)) + y[2])/(2*(1-y[1]^2/2)^2*(1-y[2]^2/2))
];

#@ E_to_TCmq 
E_to_TCmq := unapply(eval(subs({y=yx0,r=rx},E_to_TCmq_xyr)),x);

#@ TCm_to_TCmq 
TCm_to_TCmq := (z) -> [ z[1]/z[2], z[1]*z[2]];

#@ TTC_to_TCmq  
TTC_to_TCmq  := (z) -> TCm_to_TCmq(TTC_to_TCm(z));

#@ TPm_to_TPmq 
TPm_to_TPmq := (t) -> [(t[1]-t[2])/(1+t[1]*t[2]),(t[1]+t[2])/(1-t[1]*t[2])];

#@ TTP_to_TPmq  
TTP_to_TPmq  := (t) -> TPm_to_TPmq(TTP_to_TPm(t));

#@ E_to_TPmq   
E_to_TPmq   := (x) -> TTP_to_TPmq(E_to_TTP(x));

#@ E_to_TPmq_jacobian 
E_to_TPmq_jacobian := 2 * E_to_TPm_jacobian;

########################################

# This map is essentially inverse to E_to_TPmq.
# In more detail, it returns an answer involving 
# sqrt(RootOf(a quartic polynomial)), where it seems experimentally
# that the quartic almost always has precisely two nonnegative roots.
# Using the two possible square roots of these two roots we obtain
# four points in EX^*, and these are the preimages of a under 
# E_to_TPmq.
#
# We do not have a full analysis of which preimage we get using the
# default numerical value of RootOf().  However, experimant shows
# that the signs of x[3] and 1 - (a[1]*a[2])^2 are relevant, where
# a = E_to_TPm(x) (not E_to_TPmq(x)).  Alternatively, the sign of
# z[1]*z[2]+2*z[2]-2*z[1] is relevant.  This sign changes as we 
# cross C[13].

#@ TPmq_to_E 
TPmq_to_E := proc(a,i_)
 local x,xa,p,t,i;

 i := `if`(nargs > 1,i_,0);
 
 p :=
  16*a[2]^2*a[1]^4+
  32*a[1]^2*(1+a[1]^2-a[2]^2*a[1]^2)*t+
  (-8*a[2]^2*a[1]^4-32*a[2]^2*a[1]^2-96*a[1]^4-160*a[1]^2-64)*t^2+
  (24*a[2]^2*a[1]^4+32*a[2]^2*a[1]^2+72*a[1]^4+136*a[1]^2+64)*t^3+
  a[2]^2*(3*a[1]^2+4)^2*t^4;

 if i = 0 then
  xa[3] :=  sqrt(RootOf(p,t));
 elif i = 1 then
  xa[3] :=  sqrt(RootOf(p,t,index=1));
 elif i = 2 then
  xa[3] :=  sqrt(RootOf(p,t,index=2));
 elif i = 3 then
  xa[3] := -sqrt(RootOf(p,t,index=1));
 else
  xa[3] := -sqrt(RootOf(p,t,index=2));
 fi;

 xa[1] :=  (1/16)*sqrt(2)*(6*sqrt(2)*a[1]^3*xa[3]^3+
                           3*a[2]*a[1]^2*xa[3]^4-
                           4*sqrt(2)*a[1]^3*xa[3]+
                           6*sqrt(2)*a[1]*xa[3]^3+
                           4*a[1]^2*a[2]*xa[3]^2+
                           4*a[2]*xa[3]^4-
                           4*sqrt(2)*a[1]*xa[3]-4*a[2]*a[1]^2)/
           (xa[3]^2*(a[1]^2+1));

 xa[2] := -(1/16)*sqrt(2)*(6*sqrt(2)*a[1]^3*xa[3]^3-
                           3*a[2]*a[1]^2*xa[3]^4-
                           4*sqrt(2)*a[1]^3*xa[3]+
                           6*sqrt(2)*a[1]*xa[3]^3-
                           4*a[1]^2*a[2]*xa[3]^2-
                           4*a[2]*xa[3]^4-
                           4*sqrt(2)*a[1]*xa[3]+
                           4*a[2]*a[1]^2)/
           (xa[3]^2*(a[1]^2+1));

 xa[4] := -(1/8)*a[1]*a[2]*(3*a[1]^2*xa[3]^4+4*a[1]^2*xa[3]^2+4*xa[3]^4-4*a[1]^2)/
            (xa[3]^2*(a[1]^2+1));

 return [xa[1],xa[2],xa[3],xa[4]];

end:

########################################

#@ act_TCmq
act_TCmq[1]     := (z) -> [  z[1],  z[2]];
act_TCmq[LL]    := (z) -> [1/z[1],1/z[2]];
act_TCmq[LM]    := (z) -> [  z[1],  z[2]];
act_TCmq[LLLM]  := (z) -> [1/z[1],1/z[2]];
act_TCmq[LN]    := (z) -> [1/z[1],  z[2]];
act_TCmq[LLLN]  := (z) -> [  z[1],1/z[2]];
act_TCmq[MN]    := (z) -> [1/z[1],  z[2]];
act_TCmq[LLMN]  := (z) -> [  z[1],1/z[2]];

########################################

#@ v_TCmq
for i from 0 to num_vertices_E-1 do
 v_TCmq[i] := simplify(expand(E_to_TCmq(v_E0[i])));
od:

#@ v_TPmq
v_TPmq[ 0] := [0,0];
v_TPmq[ 1] := [0,0];
v_TPmq[ 7] := [0,0];
v_TPmq[ 9] := [0,0];
v_TPmq[ 4] := [0, sqrt(2)];
v_TPmq[ 5] := [0, sqrt(2)];
v_TPmq[ 2] := [0,-sqrt(2)];
v_TPmq[ 3] := [0,-sqrt(2)];
v_TPmq[ 6] := [0,infinity];
v_TPmq[ 8] := [0,infinity];
v_TPmq[10] := [infinity,infinity];
v_TPmq[11] := [infinity,infinity];
v_TPmq[12] := [infinity,infinity];
v_TPmq[13] := [infinity,infinity];
v_TPmq[14] := [0, sqrt(3)];
v_TPmq[19] := [0, sqrt(3)];
v_TPmq[16] := [0,-sqrt(3)];
v_TPmq[21] := [0,-sqrt(3)];
v_TPmq[15] := [infinity,0];
v_TPmq[17] := [infinity,0];
v_TPmq[18] := [infinity,0];
v_TPmq[20] := [infinity,0];

#@ c_TPmq
c_TPmq[ 0] := (t) -> [0,-cos(t-Pi/4)/sin(t-Pi/4)^2];
c_TPmq[ 1] := (t) -> [ 0,2*sin(t)/cos(t)^2];
c_TPmq[ 2] := (t) -> [ sqrt(8)*sin(2*t)/(1 - 3*cos(2*t)),0];
c_TPmq[ 3] := (t) -> [sqrt(2/3)/tan(t),-sqrt(2)/sin(t)];
c_TPmq[ 4] := (t) -> [sqrt(2/3)/tan(t), sqrt(2)/sin(t)];
c_TPmq[ 5] := (t) -> [ sqrt(2)*tan(t/2), sqrt(2)*tan(t/2)/sqrt(2+sin(t/2)^2)];
c_TPmq[ 6] := (t) -> [-sqrt(2)*tan(t/2), sqrt(2)*tan(t/2)/sqrt(2+sin(t/2)^2)];
c_TPmq[ 7] := (t) -> [-sqrt(2)*tan(t/2), sqrt(2)*tan(t/2)/sqrt(2+sin(t/2)^2)];
c_TPmq[ 8] := (t) -> [ sqrt(2)*tan(t/2), sqrt(2)*tan(t/2)/sqrt(2+sin(t/2)^2)];
c_TPmq[ 9] := (t) -> [1/tan(t),sqrt(3)/sin(t)];
c_TPmq[10] := (t) -> [infinity,sqrt(3)/2/tan(t)];
c_TPmq[11] := (t) -> [infinity,sqrt(3)/2/tan(t)];
c_TPmq[12] := (t) -> [1/tan(t),sqrt(3)/sin(t)];
c_TPmq[13] := (t) -> [ 2/sqrt(3)*sin(t)^2/cos(t)/sqrt(3+sin(t)^2),infinity];
c_TPmq[14] := (t) -> [ 2/sqrt(3)*tan(t),tan(t)*sqrt(3+sin(t)^2)/3];
c_TPmq[15] := (t) -> [-2/sqrt(3)*tan(t),tan(t)*sqrt(3+sin(t)^2)/3];
c_TPmq[16] := (t) -> [-2/sqrt(3)*sin(t)^2/cos(t)/sqrt(3+sin(t)^2),infinity];

#@ c_TCmq
c_TCmq[ 0] := (t) -> [1, -((4*I)*sqrt(2)*cos(t)^2*sin(t)-(4*I)*sqrt(2)*cos(t)^3-4*cos(t)^4-(2*I)*sqrt(2)*sin(t)+(2*I)*sqrt(2)*cos(t)-8*cos(t)*sin(t)+4*cos(t)^2-1)/(4*cos(t)^4-4*cos(t)^2-3)];
c_TCmq[ 1] := (t) -> [1, -((2*I)*sin(t)+cos(t)^2)/((2*I)*sin(t)-cos(t)^2)];
c_TCmq[ 2] := (t) -> [-((2*I)*sqrt(2)*cos(t)*sin(t)-3*cos(t)^2+2)/((2*I)*sqrt(2)*cos(t)*sin(t)+3*cos(t)^2-2), 1];
c_TCmq[ 3] := (t) -> [-((2*I)*sin(t)*cos(t)*sqrt(6)-5*cos(t)^2+3)/(cos(t)^2-3), ((2*I)*sqrt(2)*sin(t)+cos(t)^2+1)/(cos(t)^2-3)];
c_TCmq[ 4] := (t) -> [-((2*I)*sin(t)*cos(t)*sqrt(6)-5*cos(t)^2+3)/(cos(t)^2-3), ((2*I)*sqrt(2)*sin(t)+sin(t)^2-2)/(sin(t)^2+2)];
c_TCmq[ 5] := (t) -> [-((2*I)*sqrt(2)*sin(t)+3*cos(t)-1)/(-3+cos(t)),
                      -I*(I*cos(t)^2+4*sqrt(5-cos(t))*sin(t)-(8*I)*cos(t)-I)/((-3+cos(t))*(cos(t)+3))];
c_TCmq[ 6] := (t) -> [((2*I)*sqrt(2)*sin(t)-3*cos(t)+1)/(-3+cos(t)),
                      -I*(I*cos(t)^2+4*sqrt(5-cos(t))*sin(t)-(8*I)*cos(t)-I)/((-3+cos(t))*(cos(t)+3))];
c_TCmq[ 7] := (t) -> [((2*I)*sqrt(2)*sin(t)-3*cos(t)+1)/(-3+cos(t)),
                      -I*(I*cos(t)^2+4*sqrt(5-cos(t))*sin(t)-(8*I)*cos(t)-I)/((-3+cos(t))*(cos(t)+3))];
c_TCmq[ 8] := (t) -> [-((2*I)*sqrt(2)*sin(t)+3*cos(t)-1)/(-3+cos(t)),
                      -I*(I*cos(t)^2+4*sqrt(5-cos(t))*sin(t)-(8*I)*cos(t)-I)/((-3+cos(t))*(cos(t)+3))];
c_TCmq[ 9] := (t) -> [-cos(2*t)+I*sin(2*t), -((2*I)*sin(t)*sqrt(3)-cos(t)^2-2)/(-4+cos(t)^2)];
c_TCmq[10] := (t) -> [-1, -((4*I)*cos(t)*sqrt(3)*sin(t)-7*cos(t)^2+4)/(-4+cos(t)^2)];
c_TCmq[11] := (t) -> [-1, -((4*I)*cos(t)*sqrt(3)*sin(t)-7*cos(t)^2+4)/(-4+cos(t)^2)];
c_TCmq[12] := (t) -> [-cos(2*t)+I*sin(2*t), -((2*I)*sin(t)*sqrt(3)-cos(t)^2-2)/(-4+cos(t)^2)];
c_TCmq[13] := (t) -> [(I*sqrt(4-cos(t)^2)*sqrt(3)*cos(t)+2*cos(t)^2-2)/(I*sqrt(4-cos(t)^2)*sqrt(3)*cos(t)-2*cos(t)^2+2), -1];
c_TCmq[14] := (t) -> [-((4*I)*cos(t)*sqrt(3)*sin(t)+7*cos(t)^2-4)/(-4+cos(t)^2),
                      ((6*I)*sin(t)*sqrt(4-cos(t)^2)*cos(t)-cos(t)^4+14*cos(t)^2-4)/(4*cos(t)^2+cos(t)^4+4)];
c_TCmq[15] := (t) -> [ ((4*I)*cos(t)*sqrt(3)*sin(t)-7*cos(t)^2+4)/(-4+cos(t)^2),
                      ((6*I)*sin(t)*sqrt(4-cos(t)^2)*cos(t)-cos(t)^4+14*cos(t)^2-4)/(4*cos(t)^2+cos(t)^4+4)];
c_TCmq[16] := (t) -> [(I*sqrt(4-cos(t)^2)*sqrt(3)*cos(t)-2*cos(t)^2+2)/(I*sqrt(4-cos(t)^2)*sqrt(3)*cos(t)+2*cos(t)^2-2), -1];

# TODO: add checks for c_TCmq[i](t) = c_TCmq_mob[i](cos(t)+I*sin(t))
#@ c_TCmq_mob[ 0] 
c_TCmq_mob[ 0] := (z) -> [1,-mob(z,(1-sqrt(3))/sqrt(2))*mob(z,(1+sqrt(3))/sqrt(2))*
                             mob(z,-I*(1-sqrt(3))/sqrt(2))*mob(z,-I*(1+sqrt(3))/sqrt(2))];
c_TCmq_mob[ 1] := (z) -> [1,mob(z,-1+sqrt(2))^2*mob(z,-1-sqrt(2))^2];
c_TCmq_mob[ 2] := (z) -> [mob(z, 1+sqrt(2))^2*mob(z,-1-sqrt(2))^2,1];
c_TCmq_mob[ 3] := (z) -> [-mob(z, sqrt(3)+sqrt(2))*mob(z,-sqrt(3)-sqrt(2)),
                          -mob(z, sqrt(3)-sqrt(2))*mob(z,-sqrt(3)-sqrt(2))];
c_TCmq_mob[ 4] := (z) -> [-mob(z, sqrt(3)+sqrt(2))*mob(z,-sqrt(3)-sqrt(2)),
                          -mob(z, sqrt(3)+sqrt(2))*mob(z,-sqrt(3)+sqrt(2))];
c_TCmq_mob[ 9] := (z) -> [-1/z^2,-mob(z,2+sqrt(3))*mob(z,-2+sqrt(3))];
c_TCmq_mob[10] := (z) -> [-1    ,-mob(z,2+sqrt(3))*mob(z,-2-sqrt(3))];
c_TCmq_mob[11] := (z) -> [-1    ,-mob(z,2+sqrt(3))*mob(z,-2-sqrt(3))];
c_TCmq_mob[12] := (z) -> [-1/z^2,-mob(z,2+sqrt(3))*mob(z,-2+sqrt(3))];
