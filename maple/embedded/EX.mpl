assume(a_E > 0 and a_E < 1);

######################################################################

# Functions involved in defining the surface X
f_1 := (x::RR_4) -> (2*x[2]^2+(x[4]-1-x[3]/a_E)^2); #@ f_1 
f_2 := (x::RR_4) -> (2*x[1]^2+(x[4]-1+x[3]/a_E)^2); #@ f_2 
f   := (x::RR_4) -> f_1(x)*f_2(x); #@ f   
g_0 := (x::RR_4) -> ((1+1/a_E^2)*x[3]^2-2)*x[4]+1/a_E*(x[1]^2-x[2]^2)*x[3]; #@ g_0 
g_1 := (x::RR_4) -> x[2]^2 - x[1]^2 - (a_E+1/a_E)*x[3]*x[4]; #@ g_1 
g   := (x::RR_4) -> (1/a_E^2-1)*x[3]^2*x[4] - 2*(x[1]^2+x[2]^2)*x[4]
                     - 2*x[4]^3 + 1/a_E*(x[1]^2-x[2]^2)*x[3]; #@ g   

# rho(xx) is the squared norm of xx
rho := (x::RR_4) -> add(x[i]^2,i=1..4); #@ rho

# simplify_E is a simplification routine which is useful for 
# tidying up various expressions that arise when studying EX(a).
# We have told Maple to assume that a_E > 0 and a_E < 1, and
# Maple is able to deduce many consequences of these inequalities,
# but not all of them.  In particular, there are various identities
# between square roots of polynomials that are valid when 0 < a_E < 1
# but which are not automatically used by Maple.  The function 
# simplify_E() applies various identities of this type.

#@ simplify_E 
simplify_E := proc(u)
 local v;
 v := simplify(u);
 v := simplify(subs(sqrt(1-4*a_E^4)=sqrt(1+2*a_E^2) * sqrt(1-2*a_E^2),v));
 v := simplify(subs(sqrt(2-2*a_E^2)=sqrt(2)*sqrt(1-a_E^2),v));
 v := simplify(subs(sqrt(1-a_E^4)=sqrt(1+a_E^2) * sqrt(1-a_E^2),v));
 v := simplify(subs(sqrt(sqrt(2)*sqrt(1-a_E^2)+2*a_E^2) = 
                    sqrt(1+a_E^2)*(sqrt(2)+2*sqrt(1-a_E^2))/
                    sqrt(sqrt(2)*sqrt(-a_E^2+1)+1)/sqrt(sqrt(2)*sqrt(-a_E^2+1)+2)
               ,v));
 return(v);
end:

#@ is_member_E 
is_member_E := proc(x)
 type(x,list) and
 nops(x) = 4 and
 simplify_E(rho(x) - 1) = 0 and
 simplify_E(g_0(x)) = 0;
end:

#@ is_equal_E 
is_equal_E := proc(x,y) 
 evalb(simplify_E(x -~ y) = [0$4]);
end:

######################################################################
 
#@ act_R4
act_R4[1]    := (x) -> [ x[1], x[2], x[3], x[4]];
act_R4[L]    := (x) -> [-x[2], x[1], x[3],-x[4]];
act_R4[LL]   := (x) -> [-x[1],-x[2], x[3], x[4]];
act_R4[LLL]  := (x) -> [ x[2],-x[1], x[3],-x[4]];
act_R4[M]    := (x) -> [ x[1],-x[2],-x[3],-x[4]];
act_R4[LM]   := (x) -> [ x[2], x[1],-x[3], x[4]];
act_R4[LLM]  := (x) -> [-x[1], x[2],-x[3],-x[4]];
act_R4[LLLM] := (x) -> [-x[2],-x[1],-x[3], x[4]];
act_R4[N]    := (x) -> [ x[1],-x[2], x[3], x[4]];
act_R4[LN]   := (x) -> [ x[2], x[1], x[3],-x[4]];
act_R4[LLN]  := (x) -> [-x[1], x[2], x[3], x[4]];
act_R4[LLLN] := (x) -> [-x[2],-x[1], x[3],-x[4]];
act_R4[MN]   := (x) -> [ x[1], x[2],-x[3],-x[4]];
act_R4[LMN]  := (x) -> [-x[2], x[1],-x[3], x[4]];
act_R4[LLMN] := (x) -> [-x[1],-x[2],-x[3],-x[4]];
act_R4[LLLMN]:= (x) -> [ x[2],-x[1],-x[3], x[4]];

#@ act_E 
act_E := eval(act_R4):

######################################################################

#@ v_E
v_E[ 0] := [  0,  0,  1, 0]:
v_E[ 1] := [  0,  0, -1, 0]:
v_E[ 2] := [  1,  0,  0, 0]:
v_E[ 3] := [  0,  1,  0, 0]:
v_E[ 4] := [ -1,  0,  0, 0]:
v_E[ 5] := [  0, -1,  0, 0]:
v_E[ 6] := [  1,  1,  0, 0] /~ sqrt(2):
v_E[ 7] := [ -1,  1,  0, 0] /~ sqrt(2):
v_E[ 8] := [ -1, -1,  0, 0] /~ sqrt(2):
v_E[ 9] := [  1, -1,  0, 0] /~ sqrt(2):
v_E[10] := [  0,  0,  sqrt((2*a_E^2)/(1+a_E^2)),  sqrt((1-a_E^2)/(1+a_E^2))]:
v_E[11] := [  0,  0,  sqrt((2*a_E^2)/(1+a_E^2)), -sqrt((1-a_E^2)/(1+a_E^2))]:
v_E[12] := [  0,  0, -sqrt((2*a_E^2)/(1+a_E^2)), -sqrt((1-a_E^2)/(1+a_E^2))]:
v_E[13] := [  0,  0, -sqrt((2*a_E^2)/(1+a_E^2)),  sqrt((1-a_E^2)/(1+a_E^2))]:

######################################################################

#@ c_E_tau
c_E_tau[5] := (t) -> -sqrt((1/a_E^2-1)/2) * sin(t/2)^2;

#@ c_E_p
c_E_p[3] := (t) -> (a_E^2+1)*sin(t)^2+sqrt((a_E^2+1)*(-a_E^2+1+2*a_E^2*sin(t)^2)+(-a_E^2+1)^2*cos(t)^4);
c_E_p[5] := unapply((c_E_tau[5](t) - a_E)*(c_E_tau[5](t) - 1/a_E),t);

#@ c_E
c_E[ 0] := (t) -> [cos(t),sin(t),0,0];
c_E[ 1] := (t) -> [sin(t)/sqrt(2),sin(t)/sqrt(2),cos(t),0];
c_E[ 2] := unapply(simplify(act_E[L](c_E[1](t))),t);

c_E[3] := unapply(
[ 0,
  sqrt((2*(1-a_E^2)+4*a_E^2*sin(t)^2)/c_E_p[3](t))*sin(t),
  sqrt(2/(1+1/a_E^2))*cos(t),
 -sqrt(2/(1+1/a_E^2))*(1/a_E-a_E+2*a_E*sin(t)^2)/c_E_p[3](t)*cos(t)
],t);

c_E[ 4] :=  unapply(simplify(act_E[L](c_E[3](t))),t);

c_E[5] := unapply(
[ (1/a_E^2-1)^(3/4) * 2^(-5/4) * sqrt(a_E*(1+sin(t/2)^2)/c_E_p[5](t)) * sin(t),
  0,
  sqrt((1 - 2*a_E*c_E_tau[5](t))/c_E_p[5](t)),
  sqrt((1 - 2*a_E*c_E_tau[5](t))/c_E_p[5](t)) * c_E_tau[5](t)
],t);

c_E[ 6] := unapply(simplify(act_E[L](c_E[5](t))),t);
c_E[ 7] := unapply(simplify(act_E[M](c_E[5](t))),t);
c_E[ 8] := unapply(simplify(act_E[LM](c_E[5](t))),t);

######################################################################

#@ c_check_E
c_check_E[0] := proc(x::RR_4) x[3] = 0 and x[4] = 0; end:
c_check_E[1] := proc(x::RR_4) simplify(x[1]-x[2]) = 0 and simplify(x[4]) = 0; end:
c_check_E[2] := proc(x::RR_4) simplify(x[1]+x[2]) = 0 and simplify(x[4]) = 0; end:

c_check_E[3] := proc(x::RR_4) 
 evalb(simplify(x[1]) = 0 and is(simplify( g_1(x)) > 0));
end:

c_check_E[4] := proc(x::RR_4) 
 evalb(simplify(x[2]) = 0 and is(simplify(-g_1(x)) > 0));
end:

c_check_E[5] := proc(x::RR_4) 
 evalb(simplify(x[2]) = 0 and is(simplify(-g_1(x)) <= 0) and is(x[3] >= 0));
end:

c_check_E[6] := proc(x::RR_4) 
 local y;
 y := simplify(y_proj(x));
 evalb(simplify(x[1]) = 0 and is(simplify( g_1(x)) <= 0) and is(x[3] >= 0));
end:

c_check_E[7] := proc(x::RR_4) 
 local y;
 y := simplify(y_proj(x));
 evalb(simplify(x[2]) = 0 and is(simplify(-g_1(x)) <= 0) and is(x[3] <= 0));
end:

c_check_E[8] := proc(x::RR_4) 
 local y;
 y := simplify(y_proj(x));
 evalb(simplify(x[1]) = 0 and is(simplify( g_1(x)) <= 0) and is(x[3] <= 0));
end:

######################################################################

#@ c_param_E
c_param_E[0] := (x::RR_4) -> arctan(x[2],x[1]);
c_param_E[1] := (x::RR_4) -> arctan(sqrt(2)*x[2],x[3]);
c_param_E[2] := (x::RR_4) -> arctan(sqrt(2)*x[2],x[3]);
c_param_E[3] := proc(x::RR_4)
 local S,C,m;
 C := x[3]/sqrt(2)/sqrt(a_E/(a_E+1/a_E));
 m := sqrt((-2*a_E^2+2+4*a_E^2*(1-C^2))/((a_E^2+1)*(1-C^2)+sqrt((a_E^2+1)*(-a_E^2+1+2*a_E^2*(1-C^2))+(-a_E^2+1)^2*C^4)));
 S := x[2]/m;
 arctan(S,C);
end:

c_param_E[4] := (x::RR_4) -> c_param_E[3](act_R4[LLL](x));

c_param_E[5] := proc(x::RR_4)
 local y2,m,S,C;

 y2 := (1/2)*(-x[1]^2+x[2]^2-(a_E+1/a_E)*x[3]*x[4])/a_E:
 C := 1-2*y2*sqrt(2)/sqrt((1/a_E-a_E)/a_E):
 m := sqrt(sqrt(2)*sqrt((1/a_E-a_E)/a_E)*(1/a_E-a_E)*(3/2-C/2)/((-(1/2)*sqrt(2)*sqrt((1/a_E-a_E)/a_E)*(1-C)/2-a_E)*(-(1/2)*sqrt(2)*sqrt((1/a_E-a_E)/a_E)*(1-C)/2-1/a_E))):
 S := x[1]*4/sqrt(2)/m:

arctan(S,C);
end:

c_param_E[6] := (x::RR_4) -> c_param_E[5](act_R4[LLL](x));
c_param_E[7] := (x::RR_4) -> c_param_E[5](act_R4[M  ](x));
c_param_E[8] := (x::RR_4) -> c_param_E[5](act_R4[LM ](x));

######################################################################

# The functions below are designed for use with symbolic arguments.
# They return FAIL if Maple is unable to decide whether the relevant
# inequalities hold.  Similar functions for use with numerical 
# arguments (after setting a numerical value for a_E) are defined
# in EX0.mpl.

#@ is_in_F4_E 
is_in_F4_E := proc(x::RR_4)
 if is(x[1] >= 0) and is(x[2] >= 0) then
  return true;
 elif is(x[1] < 0) or is(x[2] < 0) then
  return false;
 else
  return FAIL;
 fi;
end:

#@ is_in_F16_E 
is_in_F16_E := proc(x::RR_4)
 if is(x[1] >= 0) and is(x[2] >= 0) and
    is(x[3] >= 0) and is(simplify(g_1(x)) >= 0) then
  return true;
 elif is(x[1] < 0) or is(x[2] < 0) and
      is(x[3] < 0) or is(simplify(g_1(x)) < 0) then
  return false;
 else
  return FAIL;
 fi;
end:

#@ retract_F4_E 
retract_F4_E := (x::RR_4) -> [abs(x[1]),abs(x[2]),x[3],x[4]];

#@ retract_F16_E 
retract_F16_E := proc(x::RR_4)
 if is(g_1(x) >= 0) then 
  return [abs(x[1]),abs(x[2]),abs(x[3]),-abs(x[4])];
 elif is(g_1(x) <= 0) then 
  return [abs(x[2]),abs(x[1]),abs(x[3]),-abs(x[4])];
 else
  return FAIL;
 fi;
end:

retract_F16_E_alt := (x) ->
 ((abs(x[1])+abs(x[2]))/2) *~ [1,1,0,0] +~
 (signum(x[3])*signum(x[4]) * (abs(x[1])-abs(x[2]))/2) *~ [-1,1,0,0] +~
 [0,0,abs(x[3]),-abs(x[4])]:

