mp_period := (1 + a_P)/sqrt(2*(1 + a_P^2)); #@ mp_period
mm_period := (1 - a_P)/sqrt(2*(1 + a_P^2)); #@ mm_period

ap_period := 2/sqrt(bp_P)*EllipticK(mp_period); #@ ap_period
am_period := 2/sqrt(bp_P)*EllipticK(mm_period); #@ am_period

ap_period_legendre := Pi*LegendreP(-1/4,A_P/2) + 2*LegendreQ(-1/4,A_P/2); #@ ap_period_legendre
am_period_legendre := Pi*LegendreP(-1/4,A_P/2);                           #@ am_period_legendre

r_period := (ap_period + am_period)/2; #@ r_period
s_period := (ap_period - am_period)/2; #@ s_period

#@ r_period_approx
r_period_approx :=
   sqrt(Pi)*GAMMA(1/4)/GAMMA(3/4)*a_P^(1/2)*(
    1+(1/6 )*a_P^4+(5/56)*a_P^8+(75/1232)*a_P^12+(65/1408)*a_P^16+(1989/53504)*a_P^20
   );
   
#@ s_period_approx
s_period_approx :=
 4*sqrt(Pi)*GAMMA(3/4)/GAMMA(1/4)*a_P^(3/2)*(
    1+(3/10)*a_P^4+(7/40)*a_P^8+(77/624)*a_P^12+(2695/28288)*a_P^16+(4389/56576)*a_P^20
   );

ap_period_approx := r_period_approx + s_period_approx; #@ ap_period_approx
am_period_approx := r_period_approx - s_period_approx; #@ am_period_approx

# I am not sure what these are for
t_period := (s_period/r_period) * (GAMMA(1/4)^2/GAMMA(3/4)^2)/4 ; #@ t_period
a_from_t_approx := (t) -> t - 2/15*t^5+8*t^9/315-3026*t^13/675675; #@ a_from_t_approx

# p_period[i,j] is the integral of the form omega[i] = z^i dz/w
# around the curve c[i].

#@ p_period
p_period[0,0] := 0;
p_period[1,0] := ( 1+I)*am_period;
p_period[2,0] := (-1+I)*am_period;
p_period[3,0] := (   I)*am_period;
p_period[4,0] := (-1  )*am_period;
p_period[5,0] := 1/2*(ap_period+am_period);
p_period[6,0] := I/2*(ap_period+am_period);
p_period[7,0] := 1/2*(ap_period-am_period);
p_period[8,0] := I/2*(ap_period-am_period);

p_period[0,1] := 0;
p_period[1,1] := (-1+I)*am_period;
p_period[2,1] := ( 1+I)*am_period;
p_period[3,1] := (   I)*am_period;
p_period[4,1] :=        am_period;
p_period[5,1] :=   1 /2*(ap_period-am_period);
p_period[6,1] := (-I)/2*(ap_period-am_period);
p_period[7,1] :=   1 /2*(ap_period+am_period);
p_period[8,1] := (-I)/2*(ap_period+am_period);

#@ picard_fuchs_LA
picard_fuchs_LA := (f) ->
 198 * diff(f,A$2) + 192 * A * diff(f,A$3) + (32 * A^2 - 128) * diff(f,A$4);

#@ picard_fuchs_La
picard_fuchs_La := (f) -> 
  4*(1-a_P^4)^3*a_P^4*diff(f,a_P$4) + 
 24*(1-a_P^4)^3*a_P^3*diff(f,a_P$3) +
  3*(1-a_P^4)*(5*a_P^8-74*a_P^4+5)*a_P^2*diff(f,a_P$2) +
    (15*a_P^12-225*a_P^8-555*a_P^4-3)*a_P*diff(f,a_P);

