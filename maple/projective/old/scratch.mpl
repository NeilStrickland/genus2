p_period[0,0] := 0;
p_period[1,0] := (1+I)*(r_period - s_period);
p_period[2,0] := (1-I)*(s_period - r_period);
p_period[3,0] :=     I*(r_period - s_period);
p_period[4,0] :=       (s_period - r_period);
p_period[5,0] :=        r_period;
p_period[6,0] :=      I*r_period;
p_period[7,0] :=        s_period;
p_period[8,0] :=      I*s_period;

p_period[0,1] := 0;
p_period[1,1] := (1-I)*(s_period - r_period);
p_period[2,1] := (1+I)*(r_period - s_period);
p_period[3,1] :=     I*(r_period - s_period);
p_period[4,1] :=       (r_period - s_period);
p_period[5,1] :=        s_period;
p_period[6,1] :=     -I*s_period;
p_period[7,1] :=        r_period;
p_period[8,1] :=     -I*r_period;

