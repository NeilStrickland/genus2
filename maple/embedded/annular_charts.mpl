#@ annular_chart

annular_chart[0] := unapply(subs({t=s[1],u=s[2]},eval(subs(C=cos(2*t)^2+4*a_E^2,[
  cos(u)*cos(t),
  cos(u)*sin(t),
  -C^(-1/2)*u*2*a_E - C^(-5/2)*u^3*(-(1/6)*a_E*(64*a_E^4+20*a_E^2*cos(4*t)-12*a_E^2+8*cos(4*t)-cos(8*t)+9)),
  -C^(-1/2)*u*cos(2*t) - C^(-5/2)*u^3*(-(40/3)*cos(2*t)*a_E^4-(7/3)*cos(6*t)*a_E^2+(11/3)*a_E^2*cos(2*t)-(5/48)*cos(6*t)-(1/48)*cos(10*t)-(5/24)*cos(2*t))
 ]))),s);

annular_chart[1] := unapply(subs({t=s[1],u=s[2]},[
  (1/2)*sin(t)*sqrt(2)+(1/2)*sin(u)*(sin(t)^2*a_E^2+a_E^2+sin(t)^2-1)*sqrt(2)/sqrt((sin(t)^2+1)^2*a_E^4+cos(t)^4*(-2*a_E^2+1))-(1/4)*u^2*sin(t)*sqrt(2),
  (1/2)*sin(t)*sqrt(2)-(1/2)*sin(u)*(sin(t)^2*a_E^2+a_E^2+sin(t)^2-1)*sqrt(2)/sqrt((sin(t)^2+1)^2*a_E^4+cos(t)^4*(-2*a_E^2+1))-(1/4)*u^2*sin(t)*sqrt(2),
  cos(t)-(1/2)*u^2*cos(t),
  sin(u)*a_E*sin(2*t)/sqrt((sin(t)^2+1)^2*a_E^4+cos(t)^4*(-2*a_E^2+1))
 ]),s);

annular_chart[2] := unapply(act_E[L](annular_chart[1](s)),s);
