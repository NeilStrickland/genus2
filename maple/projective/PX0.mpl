# Many functions related to the hyperbolic family treat the parameter a_P
# as a symbol.  One might also want to work with a numeric value of a_P.
# For that, we can invoke set_a_P0(1/10) (for example).  This will set
# a_P0 to the exact rational value 1/10, and a_P1 to the approximate value
# 0.1.  It will also set a long list of other global variables.  Typically,
# there is an existing global variable, say foo, whose value involves
# a_P, and the function set_a_P0(1/10) will set foo0 and foo1 to the
# values obtained by substituting a_P = 1/10 or a_P = 0.1 in foo.

set_a_P0 := proc(a)
 global a_P0,a_P1,A_P0,A_P1,r_P0,r_P1,P_rel0,P_rel1,
  v_P0,v_P1,c_P0,c_P1,q_Ep0,q_Em0,is_in_Ep0_0,is_in_Em0_0,
  P_to_Ep0_0,P_to_Em0_0,Ep0_0_e,Em0_0_e,Ep0_0_trans,Em0_0_trans,
  Wg2p0,Wg3p0,WPp0,WPPp0,Wg2m0,Wg3m0,WPm0,WPPm0,
  C_to_Ep0_0,C_to_Em0_0,c_Ep0_0,c_Em0_0,
  latt_a0,latt_b0,latt_c0,latt_d0,latt_e0,latt_f0,
  v_Ep0_0,v_Em0_0;
 local i,wz;

 a_P0 := a;                  #@ a_P0 
 a_P1 := evalf(a);           #@ a_P1 
 A_P0 := subs(a_P=a_P0,A_P); #@ A_P0 
 A_P1 := evalf(A_P0);        #@ A_P1 

 r_P0 := subs(a_P=a_P0,r_P); #@ r_P0 
 r_P1 := subs(a_P=a_P1,r_P); #@ r_P1 

 for i from 0 to 3 do 
  P_rel0[i] := subs(a_P=a_P0,P_rel[i]); #@ P_rel0 
  P_rel1[i] := subs(a_P=a_P1,P_rel[i]); #@ P_rel1 
 od;

 for i from 0 to 13 do
  v_P0[i] := simplify(subs(a_P=a_P0,v_P[i])); #@ v_P0
  v_P1[i] := evalf(v_P0[i]);                  #@ v_P1
 od;

 for i from 0 to 8 do
  c_P0[i] := unapply(simplify(subs(a_P=a_P0,c_P[i](t))),t); #@ c_P0
  c_P1[i] := unapply(evalf(c_P0[i](t)),t);                  #@ c_P1

  c_Ep0_0[i] := unapply(simplify(subs(a_P=a_P0,c_Ep_0[i](t))),t); #@ c_Ep0_0
  c_Em0_0[i] := unapply(simplify(subs(a_P=a_P0,c_Em_0[i](t))),t); #@ c_Em0_0
 od;

 q_Ep0 := unapply(eval(subs(a_P=a_P0,q_Ep(x))),x); #@ q_Ep0 
 q_Em0 := unapply(eval(subs(a_P=a_P0,q_Em(x))),x); #@ q_Em0 

 is_in_Ep0_0 := unapply(eval(subs(a_P=a_P0,is_in_Ep_0(YX))),YX); #@ is_in_Ep0_0 
 is_in_Em0_0 := unapply(eval(subs(a_P=a_P0,is_in_Em_0(YX))),YX); #@ is_in_Em0_0 

 P_to_Ep0_0 := unapply(eval(subs(a_P=a_P0,P_to_Ep_0([wz[1],wz[2]]))),wz); #@ P_to_Ep0_0 
 P_to_Em0_0 := unapply(eval(subs(a_P=a_P0,P_to_Em_0([wz[1],wz[2]]))),wz); #@ P_to_Em0_0 

 for i from 0 to 3 do 
  Ep0_0_e[i] := subs(a_P=a_P0,Ep_0_e[i]); #@ Ep0_0_e[i] 
  Em0_0_e[i] := subs(a_P=a_P0,Em_0_e[i]); #@ Em0_0_e[i] 

  Ep0_0_trans[i] := unapply(eval(subs(a_P=a_P0,Ep_0_trans[i](YX))),YX); #@ Ep0_0_trans[i] 
  Em0_0_trans[i] := unapply(eval(subs(a_P=a_P0,Em_0_trans[i](YX))),YX); #@ Em0_0_trans[i] 
 od:

 Wg2p0 := subs(a_P=a_P0,Wg2p);                         #@ Wg2p0
 Wg3p0 := subs(a_P=a_P0,Wg3p);                         #@ Wg3p0
 WPp0  := unapply(subs(a_P=a_P0,WPp(z)),z);            #@ WPp0
 WPPp0 := unapply(subs(a_P=a_P0,WPPp(z)),z);           #@ WPPp0
 C_to_Ep0_0 := unapply(subs(a_P=a_P0,C_to_Ep_0(z)),z); #@ C_to_Ep0_0

 Wg2m0 := subs(a_P=a_P0,Wg2m);                         #@ Wg2m0
 Wg3m0 := subs(a_P=a_P0,Wg3m);                         #@ Wg3m0
 WPm0  := unapply(subs(a_P=a_P0,WPm(z)),z);            #@ WPm0
 WPPm0 := unapply(subs(a_P=a_P0,WPPm(z)),z);           #@ WPPm0
 C_to_Em0_0 := unapply(subs(a_P=a_P0,C_to_Em_0(z)),z); #@ C_to_Em0_0

 # The code above should be correct, but is affected by a bug in Maple.
 # We therefore override it as follows:
 WPPm0 := proc(z)
  local e,u0,u1,v0,v1;
  u0 := WPm0(z);
  e := 10.^(-50);
  u1 := WPm0(z + e);
  v0 := -sqrt(2.)*I*(u1 - u0)/e;
  v1 := sqrt(4*u0^3-Wg2m0*u0-Wg3m0);
  if Re(v1/v0) < 0 then
   v1 := -v1;
  fi;
  return(v1);
 end:

 C_to_Em0_0 := (z) -> [I*WPPm0(z)/(WPm0(z)+1/3)^2/sqrt(2),1/(WPm0(z)+1/3)];
 # end of bugfix.

 latt_a0 := evalf(subs(a_P=a_P0,latt_a)); #@ latt_a0
 latt_b0 := evalf(subs(a_P=a_P0,latt_b)); #@ latt_b0
 latt_c0 := evalf(subs(a_P=a_P0,latt_c)); #@ latt_c0
 latt_d0 := evalf(subs(a_P=a_P0,latt_d)); #@ latt_d0
 latt_e0 := evalf(subs(a_P=a_P0,latt_e)); #@ latt_e0
 latt_f0 := evalf(subs(a_P=a_P0,latt_f)); #@ latt_f0

 for i from 0 to 15 do
  v_Ep0_0[i] := subs(a_P=a_P0,v_Ep_0[i]); #@ v_Ep0_0
 od:

 for i from 0 to 13 do
  v_Em0_0[i] := subs(a_P=a_P0,v_Em_0[i]); #@ v_Em0_0
 od:

 NULL;
end:

#set_a_P0(1/10);

set_a_P0(0.09835622956839951251364823009734668690409027293496644792693847749752317746490473246479165607386090621);

