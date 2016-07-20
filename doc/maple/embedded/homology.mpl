<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># The function homology_phi() defines a map from X to R^2\{0}, which
<a name="line_2"></a># induces a surjective map H_1(X) -> Z.
<a name="line_3"></a>
<a name="line_4"></a><span style="color:red">#@ homology_phi 
</span><a name="line_5"></a>homology_phi := (x) -> [1-x[3]/a_E-x[4], -sqrt(2)*x[1]];
<a name="line_6"></a>
<a name="line_7"></a># Given a 2 pi - periodic function u : R -> X, the function 
<a name="line_8"></a># winding_number(u) gives the winding number of the composite 
<a name="line_9"></a># homology_phi o u : R -> R^2\{0} around the origin.  The answer
<a name="line_10"></a># may be incorrect if the function moves too rapidly.  This
<a name="line_11"></a># can be fixed by supplying a number larger than the default
<a name="line_12"></a># value of 24 as the optional second argument.
<a name="line_13"></a>
<a name="line_14"></a><span style="color:red">#@ E_winding_number 
</span><a name="line_15"></a>E_winding_number := proc(u,n_,a_) 
<a name="line_16"></a> local v,w,t,a;
<a name="line_17"></a>
<a name="line_18"></a> a := `if`(nargs > 2, a_, a_E0);
<a name="line_19"></a> 
<a name="line_20"></a> R2_winding_number((t) -> subs(a_E=a,homology_phi(u(t))),args[2..-1]);
<a name="line_21"></a>end:
<a name="line_22"></a>
<a name="line_23"></a><span style="color:red">#@ E_winding_numbers 
</span><a name="line_24"></a>E_winding_numbers := (u,n_,a_) -> map(round,[
<a name="line_25"></a> E_winding_number(u,args[2..-1]),
<a name="line_26"></a> E_winding_number((t) -> act_R4[LLL](u(t)),args[2..-1]),
<a name="line_27"></a> E_winding_number((t) -> act_R4[  M](u(t)),args[2..-1]),
<a name="line_28"></a> E_winding_number((t) -> act_R4[ LM](u(t)),args[2..-1])
<a name="line_29"></a>]);
<a name="line_30"></a>
<a name="line_31"></a><span style="color:red">#@ cohomology_vector 
</span><a name="line_32"></a>cohomology_vector := proc(f,n_,a_)
<a name="line_33"></a> local a,w,k,c,t;
<a name="line_34"></a> 
<a name="line_35"></a> a := `if`(nargs > 2, a_, a_E0);
<a name="line_36"></a>
<a name="line_37"></a> w := NULL;
<a name="line_38"></a> for k from 5 to 8 do
<a name="line_39"></a>  c := unapply(evalf(subs(a_E=a,f(c_E[k](t)))),t);
<a name="line_40"></a>  w := w,R2_winding_number(c,args[2..-1]);
<a name="line_41"></a> od:
<a name="line_42"></a> 
<a name="line_43"></a> return [w];
<a name="line_44"></a>end:
<a name="line_45"></a>
<a name="line_46"></a>
  </pre>
 </body>
</html>
    