# Suppose that phi is a conformal automorphism of the disc with no fixed points.
# Note that phi has two fixed points (with multiplicity) in the Riemann sphere,
# and they are exchanged by z |-> 1/conjugate(z).  As neither lies in the disc,
# they must both lie on the unit circle.  Assuming that they are different, there
# is a unique hyperbolic geodesic joining them.  If phi is in our Fuchsian group
# Pi, then this will descend to a closed geodesic on HX(a).  

# We are particularly interested in geodesics which cut diagonally across the
# fundamental domain F16.

#@ geodesic_H
#@ geodesic_start_H
#@ geodesic_end_H
#@ geodesic_period_H
#@ geodesic_shift_H

# This describes a geodesic passing through v[0] and v[3]
geodesic_H[0,3] := (t) -> Tanh(t/2) * (1 + I * a_H^2)/sqrt(1 + a_H^4);
geodesic_start_H[0,3] := 0;
geodesic_end_H[0,3] := ln((ap_H+sqrt(a_H^4+1))/(a_H*am_H));
geodesic_period_H[0,3] := 2*ln((2+a_H^4+2*ap_H*sqrt(a_H^4+1)+a_H^2)/(a_H^2*(1-a_H^2)));
geodesic_shift_H[0,3] := [0,2,1,0,7,0];

# This describes a geodesic passing through v[6] and v[11]
geodesic_H[6,11] := (t) -> xi_curve((1-I)*(a_H^2+I)/ap_H,t);
geodesic_start_H[6,11] := ln((1-a_H^2+3*a_H^4+a_H^6-2*a_H^2*sqrt(1+a_H^2)*sqrt(1-a_H^2+2*a_H^4))/((1-a_H^2)*(1+a_H^4)))/2;
geodesic_end_H[6,11] := ln(((1-a_H^2+a_H^4+a_H^6) + (1-a_H^2)*sqrt(2*a_H^4-a_H^2+1)*ap_H)/(a_H^2*(1+a_H^4)))/2;
geodesic_period_H[6,11] := ln((1+a_H^4+sqrt(2*a_H^4-a_H^2+1)*ap_H)/(1+a_H^4-sqrt(2*a_H^4-a_H^2+1)*ap_H));
geodesic_shift_H[6,11] := [0,1];

#@ geodesic_crossing
geodesic_crossing := -I*(a_H^4+sqrt((a_H-1)*(a_H+1)*(a_H^4-4*a_H^2-3))*a_H-2*a_H^2-1)/(ap_H*(a_H^2+I));

# The point below is the midpoint (with respect to the hyperbolic metric) of the
# geodesic joining v[0]=0 to v[3].  Morever, if we let r denote the distance
# from min_centre_a to v[0] (or equivalently, to v[3]), then the disc of
# radius r centred at min_centre_a is the minimum radius disc that contains
# HF_16(a).

#@ min_centre_a
min_centre_a := (1 + a_H^4 - sqrt(2*a_H*am_H*(ap_H-a_H*am_H)*(1+a_H^4)))/
                 ((1-I*a_H^2)*(ap_H - a_H * am_H));

# The point below is equidistant from v_0, v_3 and v_11.

#@ tricentre_a
tricentre_a := 
 a_H+ap_H+sqrt(2*a_H^2+2*a_H*ap_H)*(-((2*am_H+2)*(sqrt(2)*sqrt(a_H)*(ap_H+a_H)^(3/2)+(ap_H+a_H)^2-1)/((ap_H+a_H)^2-1)+sqrt((-2*(ap_H+a_H)^4+8*am_H*(ap_H+a_H)^2+11*(ap_H+a_H)^2-1)*(2*sqrt(2)*sqrt(a_H)*(ap_H+a_H)^(3/2)+2*(ap_H+a_H)^2-1)/((ap_H+a_H)^4-(ap_H+a_H)^2)))^2+1)/(((2*am_H+2)*(sqrt(2)*sqrt(a_H)*(ap_H+a_H)^(3/2)+(ap_H+a_H)^2-1)/((ap_H+a_H)^2-1)+sqrt((-2*(ap_H+a_H)^4+8*am_H*(ap_H+a_H)^2+11*(ap_H+a_H)^2-1)*(2*sqrt(2)*sqrt(a_H)*(ap_H+a_H)^(3/2)+2*(ap_H+a_H)^2-1)/((ap_H+a_H)^4-(ap_H+a_H)^2)))^2+1)+(2*I)*sqrt(2*a_H^2+2*a_H*ap_H)*((2*am_H+2)*(sqrt(2)*sqrt(a_H)*(ap_H+a_H)^(3/2)+(ap_H+a_H)^2-1)/((ap_H+a_H)^2-1)+sqrt((-2*(ap_H+a_H)^4+8*am_H*(ap_H+a_H)^2+11*(ap_H+a_H)^2-1)*(2*sqrt(2)*sqrt(a_H)*(ap_H+a_H)^(3/2)+2*(ap_H+a_H)^2-1)/((ap_H+a_H)^4-(ap_H+a_H)^2)))/(((2*am_H+2)*(sqrt(2)*sqrt(a_H)*(ap_H+a_H)^(3/2)+(ap_H+a_H)^2-1)/((ap_H+a_H)^2-1)+sqrt((-2*(ap_H+a_H)^4+8*am_H*(ap_H+a_H)^2+11*(ap_H+a_H)^2-1)*(2*sqrt(2)*sqrt(a_H)*(ap_H+a_H)^(3/2)+2*(ap_H+a_H)^2-1)/((ap_H+a_H)^4-(ap_H+a_H)^2)))^2+1):