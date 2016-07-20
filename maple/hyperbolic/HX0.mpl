# Many functions related to the hyperbolic family treat the parameter a_H
# as a symbol.  One might also want to work with a numeric value of a_H.
# For that, we can invoke set_a_H0(4/5) (for example).  This will set
# a_H0 to the exact rational value 4/5, and a_H1 to the approximate value
# 0.8.  It will also set a long list of other global variables.  Typically,
# there is an existing global variable, say foo, whose value involves
# a_H, and the function set_a_H0(4/5) will set foo0 and foo1 to the
# values obtained by substituting a_H = 4/5 or a_H = 0.8 in foo.

set_a_H0 := proc(a)
 global a_H0,a_H1,ap_H0,ap_H1,am_H0,am_H1,v_H0,v_H1,
        v_H0xy,v_H1xy,v_H_label,beta0,beta1,beta_SL2R0,beta_SL2R1,
        beta_matrix,beta0_matrix,beta1_matrix,c_H0,c_H1,c_H_label,
        c_H_p0,c_colour,s_H0,c_H_speed0,
        lambda_H0,lambda_inv_H0,lambda_sq_H0,mu_H0,nu_H0,
        lambda_H1,lambda_inv_H1,lambda_sq_H1,mu_H1,nu_H1,
        act_H0,act_H1,T,min_centre_a0,
        corner_shift_H0,corner_unshift_H0,
        square_diffeo_H_ca0,square_diffeo_H_ra0,square_diffeo_H_ma0,square_diffeo_H_pa0,
        square_diffeo_H_cb0,square_diffeo_H_rb0,square_diffeo_H_mb0,square_diffeo_H_pb0;

 local ii,i,j,k,c_H_eqs,alpha,lambda,s;

 a_H0 := a;               #@ a_H0
 a_H1 := evalf(a);        #@ a_H1

 ap_H0 := sqrt(1+a_H0^2); #@ ap_H0
 am_H0 := sqrt(1-a_H0^2); #@ am_H0
 ap_H1 := sqrt(1+a_H1^2); #@ ap_H1
 am_H1 := sqrt(1-a_H1^2); #@ am_H1

 for ii in indices(v_H) do
  i := op(ii);
  v_H0[i] := simplify(subs(a_H=a_H0,v_H[i])); #@ v_H0
  v_H0xy[i] := [Re(v_H0[i]),Im(v_H0[i])];     #@ v_H0xy
  v_H1[i] := evalf(v_H0[i]);                  #@ v_H1
  v_H1xy[i] := [Re(v_H1[i]),Im(v_H1[i])];     #@ v_H1xy
  v_H_label[i] := TEXT(v_H1xy[i],i);          #@ v_H_label
 od:

 for k from 0 to 7 do
  beta0[k] := unapply(evalf(subs(a_H=a_H0,beta[k](z))),z):      #@ beta0
  beta1[k] := unapply(evalf(subs(a_H=a_H1,beta[k](z))),z):      #@ beta1
  beta_SL2R0[k] := map(evalf,map2(subs,a_H=a_H0,beta_SL2R[k])); #@ beta_SL2R0
  beta_SL2R1[k] := map(evalf,map2(subs,a_H=a_H1,beta_SL2R[k])); #@ beta_SL2R1
  beta_matrix[k]  := map(simplify,mobius_matrix(beta[k](z),z)); #@ beta_matrix
  beta0_matrix[k] := mobius_matrix(beta0[k](z),z);              #@ beta0_matrix
  beta1_matrix[k] := mobius_matrix(beta1[k](z),z);              #@ beta1_matrix
 od:

 for i from 0 to 8 do
  c_H0[i] := unapply(simplify((subs(a_H=a_H0,c_H[i](s)))),s): #@ c_H0
  c_H1[i] := unapply(evalf((subs(a_H=a_H0,c_H0[i](s)))),s):   #@ c_H1
  c_H_label[i] := ctext(c_H0[i](0),i,colour = c_colour[i]):   #@ c_H_label
 od:

 c_H_eqs := [abs(v_H0[1])^2 + 1 - 2*Re(v_H0[1])*x - 2*Im(v_H0[1])*y,
	       abs(v_H0[3])^2 + 1 - 2*Re(v_H0[3])*x - 2*Im(v_H0[3])*y]:

 c_H_p0[17] := subs(solve(c_H_eqs,{x,y}),x+I*y);
 for k from 1 to 3 do c_H_p0[17+k] := I^k * c_H_p0[17]: od:

 for i from 17 to 20 do
  c_colour[i] := "DarkGreen";
  c_H0[i] := unapply(xi_curve(c_H_p0[i],s),s);
  c_H_label[i] := ctext(c_H0[i](0),i,colour = c_colour[i]):
 od:

 lambda_H0     := lambda_H;                          #@ lambda_H0
 lambda_inv_H0 := lambda_inv_H;                      #@ lambda_inv_H0
 lambda_sq_H0  := lambda_sq_H;                       #@ lambda_sq_H0
 mu_H0         := unapply(subs(a_H=a_H0,mu_H(z)),z); #@ mu_H0
 nu_H0         := nu_H:                              #@ nu_H0

 lambda_H1     := lambda_H;                                 #@ lambda_H1
 lambda_inv_H1 := lambda_inv_H;                             #@ lambda_inv_H1
 lambda_sq_H1  := lambda_sq_H;                              #@ lambda_sq_H1
 mu_H1         := unapply(evalf(subs(a_H=a_H0,mu_H(z))),z); #@ mu_H1
 nu_H1         := nu_H:                                     #@ nu_H1

 for T in G16 do
  act_H0[T] := unapply(simplify(subs(a_H=a_H0,act_H[T](z))),z): #@ act_H0
  act_H1[T] := unapply(evalf(subs(a_H=a_H1,act_H[T](z))),z):    #@ act_H1
 od:

 for i from 0 to 4 do
  s_H0[i] := evalf(subs({a_H=a_H0},s_H[i]));  #@ s_H0
 od;

 for i from 0 to 8 do
  c_H_speed0[i] := evalf(subs({a_H=a_H0},c_H_speed[i])); #@ c_H_speed0
 od;

 for i in [0,3,4,7,8] do
  c_H_p0[i] := evalf(subs(a_H=a_H0,c_H_p[i])); #@ c_H_p0
 od;

 min_centre_a0 := evalf(subs(a_H=a_H0,min_centre_a)); #@ min_centre_a0 

 #@ corner_shift_H0
 #@ corner_unshift_H0
 for i from 1 to 3 do
  j := [11,3,6,0][i];
  k := [11,3,6,0][i+1];
  alpha := v_H0[j];
  lambda := conjugate((v_H0[k] - v_H0[j])/(1 - conjugate(v_H0[j])*v_H0[k]));
  lambda := lambda/abs(lambda):
  corner_shift_H0[j] := unapply((lambda*z - lambda*alpha)/(1 - conjugate(alpha)*z),z);
  corner_unshift_H0[j] := unapply((z + alpha*lambda)/(conjugate(alpha)*z + lambda),z);
 od:

 square_diffeo_H_ca0 := evalf(subs(a_H=a_H0,square_diffeo_H_ca));               #@ square_diffeo_H_ca0 
 square_diffeo_H_ra0 := evalf(subs(a_H=a_H0,square_diffeo_H_ra));               #@ square_diffeo_H_ra0 
 square_diffeo_H_cb0 := evalf(subs(a_H=a_H0,square_diffeo_H_cb));               #@ square_diffeo_H_cb0 
 square_diffeo_H_rb0 := evalf(subs(a_H=a_H0,square_diffeo_H_rb));               #@ square_diffeo_H_rb0 
 square_diffeo_H_ma0 := unapply(evalf(subs(a_H=a_H0,square_diffeo_H_ma(z))),z); #@ square_diffeo_H_ma0 
 square_diffeo_H_pa0 := unapply(evalf(subs(a_H=a_H0,square_diffeo_H_pa(z))),z); #@ square_diffeo_H_pa0 
 square_diffeo_H_mb0 := unapply(evalf(subs(a_H=a_H0,square_diffeo_H_mb(z))),z); #@ square_diffeo_H_mb0 
 square_diffeo_H_pb0 := unapply(evalf(subs(a_H=a_H0,square_diffeo_H_pb(z))),z); #@ square_diffeo_H_pb0 
end:

#@ c_check_H0
c_check_H0[0] := (z) -> abs(z - xi(c_H_p0[0],z));
c_check_H0[1] := (z) -> abs(z -  I*conjugate(z));
c_check_H0[2] := (z) -> abs(z +  I*conjugate(z));
c_check_H0[3] := (z) -> abs(z - xi(c_H_p0[3],z));
c_check_H0[4] := (z) -> abs(z - xi(c_H_p0[4],z));
c_check_H0[5] := (z) -> abs(z -    conjugate(z));
c_check_H0[6] := (z) -> abs(z +    conjugate(z));
c_check_H0[7] := (z) -> abs(z - xi(c_H_p0[7],z));
c_check_H0[8] := (z) -> abs(z - xi(c_H_p0[8],z));

#@ is_in_F1_H0 
is_in_F1_H0 := proc(z) 
 local T,w,r;

 for T in [[0],[2],[4],[6],[0,7],[1,2],[2,1],[3,4],[4,3],[5,6],[6,5],[7,0]] do
  w := evalf(act_Pi0(T,z));
  r := evalf(z * conjugate(z) - w * conjugate(w));
  if r > 0 then return false; fi;
 od;

 return true;
end;

#@ is_in_F4_H0 
is_in_F4_H0 := proc(z)
 local z0;
 z0 := evalf(z);
 is_in_F1_H0(z0) and (Re(z0) >= 0) and (Im(z0) >= 0);
end;

#@ is_in_F16_H0 
is_in_F16_H0 := proc(z)
 local m,d;

 if not is_in_F4_H0(z) then return false; fi;
 if evalf(Im(z) - Re(z)) > 0 then return false; fi;
 m := c_H_p0[0];
 d := evalf((z-m)*conjugate(z-m) - m*conjugate(m) + 1);
 if d < 0 then return false; fi;
 return true;
end;

# Given z in Delta, this returns [T0,z0] where  T0 is in Pi
# and the point T0 z = z0 lies in F1.

#@ retract_F1_H0_aux 
retract_F1_H0_aux := proc(z) 
 local z0,z1,T0,T1,T;

 z0 := evalf(z);
 T0 := []:
 for T in [[0],[2],[4],[6],[0,7],[1,2],[2,1],[3,4],[4,3],[5,6],[6,5],[7,0]] do
  z1 := evalf(act_Pi1_map(T)(z));
  if abs(z1) < abs(z0) then
   T0 := T;
   z0 := z1;
  fi:
 od:

 if T0 = [] then
  return([[],z0]);
 else
  T1,z1 := op(retract_F1_H0_aux(z0));
  return([Pi_mult(T1,T0),z1]);
 fi;
end:

#@ retract_F1_H0 
retract_F1_H0 := (z) -> retract_F1_H0_aux(z)[2]: 

# Given z in Delta, this returns [T1,T0,z0] where T1 is in G and T0 is in Pi
# and the point T1 T0 z = z0 lies in F16.

#@ retract_F16_H0_aux
retract_F16_H0_aux := proc(z)
 local T0,T1,z0,m,d;

 T0,z0 := op(retract_F1_H0_aux(z));

 if Re(z0) < 0 then
  if Im(z0) < 0 then
   T1 := LL;
  else
   T1 := LLL;
  fi:
 else
  if Im(z0) < 0 then
   T1 := L;
  else
   T1 := 1;
  fi:
 fi:

 z0 := act_H1[T1](z0);

 if Re(z0) < Im(z0) then
  T1 := G_mult(LN,T1);
  z0 := act_H1[LN](z0);
 fi;

 m := c_H_p0[0];
 d := evalf((z0-m)*conjugate(z0-m) - m*conjugate(m) + 1);
 if d < 0 then
  z0 := act_H1[MN](act_Pi1([6],z0));
  T1,T0 := op(Pi_tilde_mult([MN,[6]],[T1,T0]));
 fi;

 return [T1,T0,z0];
end:

# This is a continuous retraction of the unit disc onto HF16 which
# sends the complement of HF16 to the boundary of HF16 (and so
# has no interesting equivariance).

#@ squash_F16_H0
squash_F16_H0 := proc(z)
 local w,i;
 w := z;
 for i in [3,6,11] do
  w := corner_shift_H0[i](w);
  w := max(0,Re(w)) + max(0,Im(w))*I;
  w := corner_unshift_H0[i](w);
 od;
 if Im(w) <= 0 then
  if Re(w) >= 0 then
   w := Re(w);
  else
   w := 0;
  fi;
 elif Im(w) >= Re(w) then
  if Im(w) + Re(w) >= 0 then
   w := (Re(w)+Im(w)) * (1+I)/2;
  else
   w := 0;
  fi;
 fi;

 return w;
end:

#@ square_diffeo_H0
square_diffeo_H0 := (z) -> [1-square_diffeo_H_pa0(z),square_diffeo_H_pb0(z)];

#@ square_diffeo_H0_inverse
square_diffeo_H0_inverse := proc(t0)
 local ca,sa,sb,pa,pb,aa,bb,zz,mm;
 sa := square_diffeo_H_ra0^(1-t0[1]);
 sb := square_diffeo_H_rb0^t0[2];

 if trim(sa) = 1 then
  if trim(sb) = 1 then
   return(0);
  else
   return evalf(
    (1/sqrt(2)*a_H0*square_diffeo_H_rb0*sb^2-(sqrt(1+a_H0^2)-sqrt(1-a_H0^2))/2-
     sqrt(((sqrt(1-a_H0^4)-1)*sb^4+(4-2*a_H0^2)*sb^2-(sqrt(1-a_H0^4)+1))/2))/(sb^2-1));
  fi;
 fi;

 if trim(sb) = 1 then
  ca := square_diffeo_H_ca0;
  mm := evalf(sqrt(2)*(ca-1/ca)/I/(sa^2-1));
  zz := evalf((1/ca+I*ca)/2 - (1+I)*sqrt(2)*mm/4 -
        sqrt((1/ca^2-ca^2)/4-I*(2-mm^2)/4+((1-I)*ca-(1+I)/ca)*sqrt(2)*mm/4));
  return zz;
 fi;

 pa := (conjugate(square_diffeo_H_ca0)*sa^2-  square_diffeo_H_ca0)/(sa^2-1);
 pb := (conjugate(square_diffeo_H_cb0)*sb^2-I*square_diffeo_H_cb0)/(sb^2-1);
 aa := Re(pa*conjugate(pa-pb))/abs(pa-pb)^2;
 bb := sqrt((abs(pa)^2-1)/abs(pa-pb)^2-aa^2);
 return (1-aa+I*bb)*pa+(aa-I*bb)*pb;
end:

# set_a_H0(4/5);

# The value below is obtained as follows:
#
# 1) Find an approximation to the function f such that exp(2f) times
#    the euclidean metric on EX^* has curvature -1.
# 2) Find the lengths of the sides of F16 with respect to this
#    rescaled metric.
# 3) Find the value of a_H that makes the sides of F16 in HX
#    have the same lengths as in (2).  We get a different answer
#    for each side, but the differences are around 10^(-5.5).
#    The value below is the average.
#
# I think that the error in the value below is probably < 10^(-9).

set_a_H0(0.8005319048923638104265325104767778171017180985241883922169730841584541075292965796346466097572031440);