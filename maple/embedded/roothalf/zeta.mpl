# Function that vanishes on the waist of the pair of pants
#@ disc_zeta_q0 
disc_zeta_q0 := (u) -> 1 - 6*(1-sqrt(2))^2*u[1]^2 - u[2]^2;

# Function that vanishes on the ankles of the pair of pants
#@ disc_zeta_q1 
disc_zeta_q1 := (u) -> 
 u[2]^4+
 (-12*sqrt(2)*u[1]^2+20*u[1]^2-4*sqrt(2)+6)*u[2]^2+
 (-24)*sqrt(2)*u[1]^4+36*u[1]^4+36*sqrt(2)*u[1]^2-52*u[1]^2-12*sqrt(2)+17;

# Function that vanishes on the boundary of the pair of pants
#@ disc_zeta_q2 
disc_zeta_q2 := (u) -> 
 disc_zeta_q0(u) * disc_zeta_q1(u);

#@ disc_zeta_q3 
disc_zeta_q3 := (u,t) ->
 (-36*sqrt(2)+53)*t^4+
 (-48*sqrt(2)*u[1]^2+108*u[1]^2+74*u[2]^2+52*sqrt(2)-66)*t^2+
 48*sqrt(2)*u[1]^2*u[2]^2+36*sqrt(2)*u[2]^4+72*u[1]^4+
  108*u[1]^2*u[2]^2+53*u[2]^4-52*sqrt(2)*u[2]^2-108*u[1]^2-66*u[2]^2+37;

#@ disc_zeta_q4 
disc_zeta_q4 := (u,t) -> 
 t^2+(1+sqrt(2))^4*u[2]^2+(1+sqrt(2))^2;

#@ disc_zeta_q5 
disc_zeta_q5 := (u,t) -> 
 t^6+
 ((38+24*sqrt(2))*u[1]^2+(24*sqrt(2)+35)*u[2]^2+4*sqrt(2)+5)*t^4+
 ((684+480*sqrt(2))*u[1]^4+((912*sqrt(2)+1292)*u[2]^2+96*sqrt(2)+132)*u[1]^2+
   (611+432*sqrt(2))*u[2]^4+(120*sqrt(2)+170)*u[2]^2+8*sqrt(2)+11)*t^2;

#@ disc_zeta_q6 
disc_zeta_q6 := (x) -> 
 x[1]^4+((24*sqrt(2)+34)*x[2]^2+(12*sqrt(2)+18)*x[3]^2+(168*sqrt(2)+234)*x[4]*x[3]+
 (-260*sqrt(2)-366)*x[4]^2+4*sqrt(2)+6)*x[1]^2+(577+408*sqrt(2))*x[2]^4+
 ((420*sqrt(2)+594)*x[3]^2+(-1464*sqrt(2)-2070)*x[4]*x[3]+(692*sqrt(2)+978)*x[4]^2+
 140*sqrt(2)+198)*x[2]^2+(108*sqrt(2)+153)*x[3]^4+(-648*sqrt(2)-918)*x[4]*x[3]^3+
 ((1512*sqrt(2)+2142)*x[4]^2-216*sqrt(2)-306)*x[3]^2+((-1224*sqrt(2)-1734)*x[4]^3+
 (456*sqrt(2)+646)*x[4])*x[3]+(108*sqrt(2)+153)*x[4]^4+(-216*sqrt(2)-306)*x[4]^2+
 17+12*sqrt(2);

#@ disc_zeta_q7 
disc_zeta_q7 := (x) -> 
 8*(1+sqrt(2))^4*(sqrt(2)*x[4]*(x[1]^2-x[2]^2) + x[3]*(3*x[4]^2-2));

#@ disc_zeta_q8 
disc_zeta_q8 := (y) -> (2+(3-2*sqrt(2))*y[1]^2)*y[2] + (2-(3-2*sqrt(2))*y[1]^2);

#@ disc_zeta_det 
disc_zeta_det := (x) -> 
 Determinant(Matrix([[x[1],x[2],x[3],x[4]],
                     dg0(x),
                     [0,0,1,-1],
                     [0,1,0,0]]));

#@ disc_zeta_lift 
disc_zeta_lift := proc(u)
 local t0,x1,x2,x3,x4,t;
 x1 := RootOf(disc_zeta_q5(u,t)-(1+sqrt(2))^8*disc_zeta_q2(u),t);
 t0 := (1+sqrt(2))^2*u[1]*sqrt(disc_zeta_q3(u,x1))/disc_zeta_q4(u,x1);
 x2 := u[2];
 x3 := ( u[1] + t0)/sqrt(2);
 x4 := (-u[1] + t0)/sqrt(2);
 return [x1,x2,x3,x4];
end:

#@ disc_zeta_lift0 
disc_zeta_lift0 := proc(u)
 local t0,xx1,x1,x2,x3,x4,t;

 xx1 := fsolve(disc_zeta_q5(u,t)-(1+sqrt(2))^8*disc_zeta_q2(u),t);
 xx1 := select(t -> t >= 0,xx1);
 if nops(xx1) <> 1 then return FAIL; fi;
 x1 := xx1[1];
 t0 := (1+sqrt(2))^2*u[1]*sqrt(disc_zeta_q3(u,x1))/disc_zeta_q4(u,x1);
 x2 := u[2];
 x3 := ( u[1] + t0)/sqrt(2);
 x4 := (-u[1] + t0)/sqrt(2);
 return evalf([x1,x2,x3,x4]);
end:

#@ disc_zeta_q9 
disc_zeta_q9 := (u,t) ->
 t^3+
 2*sqrt(2)*u[1]*t^2+
 (-6*sqrt(2)*u[1]^2-6*sqrt(2)*u[2]^2-6*u[1]^2-8*u[2]^2-sqrt(2)-2)*t+
 (-8)*u[1]^3*sqrt(2)-8*sqrt(2)*u[1]*u[2]^2-12*u[1]^3-12*u[1]*u[2]^2+4*sqrt(2)*u[1]+6*u[1];

#@ disc_zeta_lift_alt 
disc_zeta_lift_alt := proc(u)
 local r,t;
 r := RootOf(disc_zeta_lift_cubic(u,t),t);
 return [sqrt(-(2*sqrt(2)*u[1]*r+2*r^2+2*u[1]^2+u[2]^2-1)),
         u[2],
         r + sqrt(2)*u[1],
         r];
end:
