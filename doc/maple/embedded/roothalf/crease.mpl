<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># The code below defines a function crease : [-t0,t0] -> X, where 
<a name="line_2"></a># t0 = sqrt(3) - sqrt(2).  I think that the union of the images under
<a name="line_3"></a># G16 of crease([-t0,t0]) is precisely the set of singular points for
<a name="line_4"></a># the map pi, which is also the set of points where 
<a name="line_5"></a># x[3] dg(x)[4] - x[4] dg(x)[3] = 0. 
<a name="line_6"></a>
<a name="line_7"></a><span style="color:red">#@ crease 
</span><a name="line_8"></a>crease := (t) -> [
<a name="line_9"></a> (t-1/sqrt(2))*sqrt(-(t-sqrt(3)-sqrt(2))*(t+sqrt(3)-sqrt(2))/(1-t^2)/3),
<a name="line_10"></a> (t+1/sqrt(2))*sqrt(-(t+sqrt(3)+sqrt(2))*(t-sqrt(3)+sqrt(2))/(1-t^2)/3),
<a name="line_11"></a> sqrt((1+t^2)/(1-t^2)*2/3),
<a name="line_12"></a> -sqrt((1+t^2)/(1-t^2)*2/3)*t
<a name="line_13"></a>];
<a name="line_14"></a>
<a name="line_15"></a><span style="color:red">#@ crease_trig 
</span><a name="line_16"></a>crease_trig := (t) -> [
<a name="line_17"></a>  -(sqrt(2)-2/sqrt(3))*(1+sqrt(3/2)-cos(2*t))*cos(t)*sqrt(alpha_E-cos(2*t))/sqrt(alpha_E-cos(2*t)^2),
<a name="line_18"></a>  -(sqrt(2)-2/sqrt(3))*(1+sqrt(3/2)+cos(2*t))*sin(t)*sqrt(alpha_E+cos(2*t))/sqrt(alpha_E-cos(2*t)^2),
<a name="line_19"></a>    sqrt(2/3)*sqrt(alpha_E+cos(2*t)^2)/sqrt(alpha_E-cos(2*t)^2),
<a name="line_20"></a>  -(sqrt(2)-2/sqrt(3))*cos(2*t)*sqrt(alpha_E+cos(2*t)^2)/sqrt(alpha_E-cos(2*t)^2)
<a name="line_21"></a>];
<a name="line_22"></a>
<a name="line_23"></a><span style="color:red">#@ crease_eq 
</span><a name="line_24"></a>crease_eq := (x) ->
<a name="line_25"></a> x[1]^8+32*x[1]^6*x[2]^2-66*x[1]^4*x[2]^4+32*x[1]^2*x[2]^6+x[2]^8+
<a name="line_26"></a> (-6)*x[1]^6-426*x[1]^4*x[2]^2-426*x[1]^2*x[2]^4-6*x[2]^6-35*x[1]^4+
<a name="line_27"></a> 502*x[1]^2*x[2]^2-35*x[2]^4-36*x[1]^2-36*x[2]^2+4;
<a name="line_28"></a>
<a name="line_29"></a><span style="color:red">#@ crease_eq_polar 
</span><a name="line_30"></a>crease_eq_polar := (r,t) ->
<a name="line_31"></a> (-cos(8*t)+(1/2)*cos(4*t)+3/2)*r^8+
<a name="line_32"></a> (51*cos(4*t)-57)*r^6+
<a name="line_33"></a> (73/2-(143/2)*cos(4*t))*r^4-36*r^2+4;
<a name="line_34"></a>
  </pre>
 </body>
</html>
    