<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a>mp_period := (1 + a_P)/sqrt(2*(1 + a_P^2)); <span style="color:red">#@ mp_period
</span><a name="line_2"></a>mm_period := (1 - a_P)/sqrt(2*(1 + a_P^2)); <span style="color:red">#@ mm_period
</span><a name="line_3"></a>
<a name="line_4"></a>ap_period := 2/sqrt(bp_P)*EllipticK(mp_period); <span style="color:red">#@ ap_period
</span><a name="line_5"></a>am_period := 2/sqrt(bp_P)*EllipticK(mm_period); <span style="color:red">#@ am_period
</span><a name="line_6"></a>
<a name="line_7"></a>ap_period_legendre := Pi*LegendreP(-1/4,A_P/2) + 2*LegendreQ(-1/4,A_P/2); <span style="color:red">#@ ap_period_legendre
</span><a name="line_8"></a>am_period_legendre := Pi*LegendreP(-1/4,A_P/2);                           <span style="color:red">#@ am_period_legendre
</span><a name="line_9"></a>
<a name="line_10"></a>r_period := (ap_period + am_period)/2; <span style="color:red">#@ r_period
</span><a name="line_11"></a>s_period := (ap_period - am_period)/2; <span style="color:red">#@ s_period
</span><a name="line_12"></a>
<a name="line_13"></a><span style="color:red">#@ r_period_approx
</span><a name="line_14"></a>r_period_approx :=
<a name="line_15"></a>   sqrt(Pi)*GAMMA(1/4)/GAMMA(3/4)*a_P^(1/2)*(
<a name="line_16"></a>    1+(1/6 )*a_P^4+(5/56)*a_P^8+(75/1232)*a_P^12+(65/1408)*a_P^16+(1989/53504)*a_P^20
<a name="line_17"></a>   );
<a name="line_18"></a>   
<a name="line_19"></a><span style="color:red">#@ s_period_approx
</span><a name="line_20"></a>s_period_approx :=
<a name="line_21"></a> 4*sqrt(Pi)*GAMMA(3/4)/GAMMA(1/4)*a_P^(3/2)*(
<a name="line_22"></a>    1+(3/10)*a_P^4+(7/40)*a_P^8+(77/624)*a_P^12+(2695/28288)*a_P^16+(4389/56576)*a_P^20
<a name="line_23"></a>   );
<a name="line_24"></a>
<a name="line_25"></a>ap_period_approx := r_period_approx + s_period_approx; <span style="color:red">#@ ap_period_approx
</span><a name="line_26"></a>am_period_approx := r_period_approx - s_period_approx; <span style="color:red">#@ am_period_approx
</span><a name="line_27"></a>
<a name="line_28"></a># I am not sure what these are for
<a name="line_29"></a>t_period := (s_period/r_period) * (GAMMA(1/4)^2/GAMMA(3/4)^2)/4 ; <span style="color:red">#@ t_period
</span><a name="line_30"></a>a_from_t_approx := (t) -> t - 2/15*t^5+8*t^9/315-3026*t^13/675675; <span style="color:red">#@ a_from_t_approx
</span><a name="line_31"></a>
<a name="line_32"></a># p_period[i,j] is the integral of the form omega[i] = z^i dz/w
<a name="line_33"></a># around the curve c[i].
<a name="line_34"></a>
<a name="line_35"></a><span style="color:red">#@ p_period
</span><a name="line_36"></a>p_period[0,0] := 0;
<a name="line_37"></a>p_period[1,0] := ( 1+I)*am_period;
<a name="line_38"></a>p_period[2,0] := (-1+I)*am_period;
<a name="line_39"></a>p_period[3,0] := (   I)*am_period;
<a name="line_40"></a>p_period[4,0] := (-1  )*am_period;
<a name="line_41"></a>p_period[5,0] := 1/2*(ap_period+am_period);
<a name="line_42"></a>p_period[6,0] := I/2*(ap_period+am_period);
<a name="line_43"></a>p_period[7,0] := 1/2*(ap_period-am_period);
<a name="line_44"></a>p_period[8,0] := I/2*(ap_period-am_period);
<a name="line_45"></a>
<a name="line_46"></a>p_period[0,1] := 0;
<a name="line_47"></a>p_period[1,1] := (-1+I)*am_period;
<a name="line_48"></a>p_period[2,1] := ( 1+I)*am_period;
<a name="line_49"></a>p_period[3,1] := (   I)*am_period;
<a name="line_50"></a>p_period[4,1] :=        am_period;
<a name="line_51"></a>p_period[5,1] :=   1 /2*(ap_period-am_period);
<a name="line_52"></a>p_period[6,1] := (-I)/2*(ap_period-am_period);
<a name="line_53"></a>p_period[7,1] :=   1 /2*(ap_period+am_period);
<a name="line_54"></a>p_period[8,1] := (-I)/2*(ap_period+am_period);
<a name="line_55"></a>
<a name="line_56"></a><span style="color:red">#@ picard_fuchs_LA
</span><a name="line_57"></a>picard_fuchs_LA := (f) ->
<a name="line_58"></a> 198 * diff(f,A$2) + 192 * A * diff(f,A$3) + (32 * A^2 - 128) * diff(f,A$4);
<a name="line_59"></a>
<a name="line_60"></a><span style="color:red">#@ picard_fuchs_La
</span><a name="line_61"></a>picard_fuchs_La := (f) -> 
<a name="line_62"></a>  4*(1-a_P^4)^3*a_P^4*diff(f,a_P$4) + 
<a name="line_63"></a> 24*(1-a_P^4)^3*a_P^3*diff(f,a_P$3) +
<a name="line_64"></a>  3*(1-a_P^4)*(5*a_P^8-74*a_P^4+5)*a_P^2*diff(f,a_P$2) +
<a name="line_65"></a>    (15*a_P^12-225*a_P^8-555*a_P^4-3)*a_P*diff(f,a_P);
<a name="line_66"></a>
  </pre>
 </body>
</html>
    