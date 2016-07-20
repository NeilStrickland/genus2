<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># Suppose that phi is a conformal automorphism of the disc with no fixed points.
<a name="line_2"></a># Note that phi has two fixed points (with multiplicity) in the Riemann sphere,
<a name="line_3"></a># and they are exchanged by z |-> 1/conjugate(z).  As neither lies in the disc,
<a name="line_4"></a># they must both lie on the unit circle.  Assuming that they are different, there
<a name="line_5"></a># is a unique hyperbolic geodesic joining them.  If phi is in our Fuchsian group
<a name="line_6"></a># Pi, then this will descend to a closed geodesic on HX(a).  
<a name="line_7"></a>
<a name="line_8"></a># We are particularly interested in geodesics which cut diagonally across the
<a name="line_9"></a># fundamental domain F16.
<a name="line_10"></a>
<a name="line_11"></a><span style="color:red">#@ geodesic_H
</span><a name="line_12"></a><span style="color:red">#@ geodesic_start_H
</span><a name="line_13"></a><span style="color:red">#@ geodesic_end_H
</span><a name="line_14"></a><span style="color:red">#@ geodesic_period_H
</span><a name="line_15"></a><span style="color:red">#@ geodesic_shift_H
</span><a name="line_16"></a>
<a name="line_17"></a># This describes a geodesic passing through v[0] and v[3]
<a name="line_18"></a>geodesic_H[0,3] := (t) -> Tanh(t/2) * (1 + I * a_H^2)/sqrt(1 + a_H^4);
<a name="line_19"></a>geodesic_start_H[0,3] := 0;
<a name="line_20"></a>geodesic_end_H[0,3] := ln((ap_H+sqrt(a_H^4+1))/(a_H*am_H));
<a name="line_21"></a>geodesic_period_H[0,3] := 2*ln((2+a_H^4+2*ap_H*sqrt(a_H^4+1)+a_H^2)/(a_H^2*(1-a_H^2)));
<a name="line_22"></a>geodesic_shift_H[0,3] := [0,2,1,0,7,0];
<a name="line_23"></a>
<a name="line_24"></a># This describes a geodesic passing through v[6] and v[11]
<a name="line_25"></a>geodesic_H[6,11] := (t) -> xi_curve((1-I)*(a_H^2+I)/ap_H,t);
<a name="line_26"></a>geodesic_start_H[6,11] := ln((1-a_H^2+3*a_H^4+a_H^6-2*a_H^2*sqrt(1+a_H^2)*sqrt(1-a_H^2+2*a_H^4))/((1-a_H^2)*(1+a_H^4)))/2;
<a name="line_27"></a>geodesic_end_H[6,11] := ln(((1-a_H^2+a_H^4+a_H^6) + (1-a_H^2)*sqrt(2*a_H^4-a_H^2+1)*ap_H)/(a_H^2*(1+a_H^4)))/2;
<a name="line_28"></a>geodesic_period_H[6,11] := ln((1+a_H^4+sqrt(2*a_H^4-a_H^2+1)*ap_H)/(1+a_H^4-sqrt(2*a_H^4-a_H^2+1)*ap_H));
<a name="line_29"></a>geodesic_shift_H[6,11] := [0,1];
<a name="line_30"></a>
<a name="line_31"></a><span style="color:red">#@ geodesic_crossing
</span><a name="line_32"></a>geodesic_crossing := -I*(a_H^4+sqrt((a_H-1)*(a_H+1)*(a_H^4-4*a_H^2-3))*a_H-2*a_H^2-1)/(ap_H*(a_H^2+I));
<a name="line_33"></a>
<a name="line_34"></a># The point below is the midpoint (with respect to the hyperbolic metric) of the
<a name="line_35"></a># geodesic joining v[0]=0 to v[3].  Morever, if we let r denote the distance
<a name="line_36"></a># from min_centre_a to v[0] (or equivalently, to v[3]), then the disc of
<a name="line_37"></a># radius r centred at min_centre_a is the minimum radius disc that contains
<a name="line_38"></a># HF_16(a).
<a name="line_39"></a>
<a name="line_40"></a><span style="color:red">#@ min_centre_a
</span><a name="line_41"></a>min_centre_a := (1 + a_H^4 - sqrt(2*a_H*am_H*(ap_H-a_H*am_H)*(1+a_H^4)))/
<a name="line_42"></a>                 ((1-I*a_H^2)*(ap_H - a_H * am_H));
<a name="line_43"></a>
<a name="line_44"></a># The point below is equidistant from v_0, v_3 and v_11.
<a name="line_45"></a>
<a name="line_46"></a><span style="color:red">#@ tricentre_a
</span><a name="line_47"></a>tricentre_a := 
<a name="line_48"></a> a_H+ap_H+sqrt(2*a_H^2+2*a_H*ap_H)*(-((2*am_H+2)*(sqrt(2)*sqrt(a_H)*(ap_H+a_H)^(3/2)+(ap_H+a_H)^2-1)/((ap_H+a_H)^2-1)+sqrt((-2*(ap_H+a_H)^4+8*am_H*(ap_H+a_H)^2+11*(ap_H+a_H)^2-1)*(2*sqrt(2)*sqrt(a_H)*(ap_H+a_H)^(3/2)+2*(ap_H+a_H)^2-1)/((ap_H+a_H)^4-(ap_H+a_H)^2)))^2+1)/(((2*am_H+2)*(sqrt(2)*sqrt(a_H)*(ap_H+a_H)^(3/2)+(ap_H+a_H)^2-1)/((ap_H+a_H)^2-1)+sqrt((-2*(ap_H+a_H)^4+8*am_H*(ap_H+a_H)^2+11*(ap_H+a_H)^2-1)*(2*sqrt(2)*sqrt(a_H)*(ap_H+a_H)^(3/2)+2*(ap_H+a_H)^2-1)/((ap_H+a_H)^4-(ap_H+a_H)^2)))^2+1)+(2*I)*sqrt(2*a_H^2+2*a_H*ap_H)*((2*am_H+2)*(sqrt(2)*sqrt(a_H)*(ap_H+a_H)^(3/2)+(ap_H+a_H)^2-1)/((ap_H+a_H)^2-1)+sqrt((-2*(ap_H+a_H)^4+8*am_H*(ap_H+a_H)^2+11*(ap_H+a_H)^2-1)*(2*sqrt(2)*sqrt(a_H)*(ap_H+a_H)^(3/2)+2*(ap_H+a_H)^2-1)/((ap_H+a_H)^4-(ap_H+a_H)^2)))/(((2*am_H+2)*(sqrt(2)*sqrt(a_H)*(ap_H+a_H)^(3/2)+(ap_H+a_H)^2-1)/((ap_H+a_H)^2-1)+sqrt((-2*(ap_H+a_H)^4+8*am_H*(ap_H+a_H)^2+11*(ap_H+a_H)^2-1)*(2*sqrt(2)*sqrt(a_H)*(ap_H+a_H)^(3/2)+2*(ap_H+a_H)^2-1)/((ap_H+a_H)^4-(ap_H+a_H)^2)))^2+1):  </pre>
 </body>
</html>
    