#@ c_algebraic 
c_algebraic :=
 (t) -> [0,
         sqrt(t*(2*a_E*t^2-1/a_E+a_E)/((t-a_E)*(t-1/a_E))),
         sqrt((-2*a_E*t+1)/((t-a_E)*(t-1/a_E))),
         -t*sqrt((-2*a_E*t+1)/((t-a_E)*(t-1/a_E)))
        ];

# NB c_E[6](t) is the same as c_algebraic(tau_algebraic[6](t)) up to sign issues.
# However, c_E[3](t) is not the same as c_algebraic(tau_algebraic[3](t)).
# The latter gives an alternative parametrisation of C[3] except in the
# case a = 1/sqrt(2).  In that case, tau_algebraic[3](t) is constant at 1/sqrt(2),
# and c_algebraic(tau_algebraic[3](t)) is undefined.

#@ a_star_E
a_star_E := sqrt((1/a_E^2-1)/2);

#@ tau_algebraic
tau_algebraic[3] := (t) -> 1/(2*a_E) + (sqrt((1/a_E^2-1)/2)-1/(2*a_E)) * cos(t)^2;
tau_algebraic[6] := (t) -> -sqrt((1/a_E-a_E)/(2*a_E)) * sin(t/2)^2;

######################################################################

c_E[ 9] := (t) -> [sqrt((1-a_E^2)/(1+a_E^2)/2)*sin(t), sqrt((1-a_E^2)/(1+a_E^2)/2)*sin(t),
                 sqrt(2)*a_E/sqrt(a_E^2+1),-sqrt((1-a_E^2)/(1+a_E^2))*cos(t)];

c_E[10] := unapply(simplify(act_E[ L](c_E[ 9](t))),t);
c_E[11] := unapply(simplify(act_E[ M](c_E[ 9](t))),t);
c_E[12] := unapply(simplify(act_E[LM](c_E[ 9](t))),t);

c_check_E[ 9] := (a) -> evalb(simplify([a[1]-a[2],a[3]-a_E*sqrt(2/(1+a_E^2))]) = [0,0]):
c_check_E[10] := (a) -> evalb(simplify([a[1]+a[2],a[3]-a_E*sqrt(2/(1+a_E^2))]) = [0,0]):
c_check_E[11] := (a) -> evalb(simplify([a[1]+a[2],a[3]+a_E*sqrt(2/(1+a_E^2))]) = [0,0]):
c_check_E[12] := (a) -> evalb(simplify([a[1]-a[2],a[3]+a_E*sqrt(2/(1+a_E^2))]) = [0,0]):

c_param_E[ 9] := (x) -> arctan( sqrt(2)*x[1],-x[4]);
c_param_E[10] := (x) -> arctan(-sqrt(2)*x[1], x[4]);
c_param_E[11] := (x) -> arctan( sqrt(2)*x[1], x[4]);
c_param_E[12] := (x) -> arctan( sqrt(2)*x[1],-x[4]);

c_homology[ 9] := [-1, 1, 0, 0];
c_homology[10] := [-1,-1, 0, 0];
c_homology[11] := [ 0, 0,-1,-1];
c_homology[12] := [ 0, 0, 1,-1];

for i from  9 to 12 do c_colour[i] := red:  od:

######################################################################

c_E[13] := (t) -> [ cos(t)/sqrt(2)*(1 - sin(t)/sqrt(2/(1-a_E^2)-cos(t)^2)),
                    cos(t)/sqrt(2)*(1 + sin(t)/sqrt(2/(1-a_E^2)-cos(t)^2)),
		    sqrt(2)*a_E/sqrt(1+a_E^2)*sin(t),
                   -sqrt(2)*sin(t)^2/sqrt(2/(-a_E^2+1)-cos(t)^2)/sqrt(a_E^2+1)];

c_E[14] := unapply(simplify(act_E[ L](c_E[13](t))),t);
c_E[15] := unapply(simplify(act_E[ N](c_E[13](t))),t);
c_E[16] := unapply(simplify(act_E[LN](c_E[13](t))),t);

#@ h_c13_a 
h_c13_a := (x) -> x[1]*(x[3]-a_E*x[4])-x[2]*(x[3]+a_E*x[4]);

#@ h_c13_b 
h_c13_b := (x) -> (3-a_E^2)*(x[1]-x[2])^2 - 2*(x[1]^2+x[2]^2)*(x[3]^2+x[4]^2)*(1-a_E^2);

c_check_E[13] := (a) -> evalb(simplify(h_c13_a(a)) = 0 and is(a[4] <= 0) and is(h_c13_b(a) >= 0));
c_check_E[14] := (a) -> c_check_E[13](act_R4[LLL](a));
c_check_E[15] := (a) -> c_check_E[13](act_R4[  N](a));
c_check_E[16] := (a) -> c_check_E[13](act_R4[ LN](a));

c_param_E[13] := (x) -> arctan(sqrt(1 + a_E^2)/a_E * x[3], x[1]+x[2]);
c_param_E[14] := (x) -> arctan(sqrt(1 + a_E^2)/a_E * x[3],-x[1]+x[2]);
c_param_E[15] := (x) -> arctan(sqrt(1 + a_E^2)/a_E * x[3], x[1]-x[2]);
c_param_E[16] := (x) -> arctan(sqrt(1 + a_E^2)/a_E * x[3], x[1]+x[2]);

c_homology[13] := [ 0,-1, 1, 0];
c_homology[14] := [ 1, 0, 0, 1];
c_homology[15] := [ 0, 1, 1, 0];
c_homology[16] := [-1, 0, 0, 1];

for i from 13 to 16 do c_colour[i] := "Orange":  od:

#@ num_curves_E 
num_curves_E := 17;

