# The function homology_phi() defines a map from X to R^2\{0}, which
# induces a surjective map H_1(X) -> Z.

#@ homology_phi 
homology_phi := (x) -> [1-x[3]/a_E-x[4], -sqrt(2)*x[1]];

# Given a 2 pi - periodic function u : R -> X, the function 
# winding_number(u) gives the winding number of the composite 
# homology_phi o u : R -> R^2\{0} around the origin.  The answer
# may be incorrect if the function moves too rapidly.  This
# can be fixed by supplying a number larger than the default
# value of 24 as the optional second argument.

#@ E_winding_number 
E_winding_number := proc(u,n_,a_) 
 local v,w,t,a;

 a := `if`(nargs > 2, a_, a_E0);
 
 R2_winding_number((t) -> subs(a_E=a,homology_phi(u(t))),args[2..-1]);
end:

#@ E_winding_numbers 
E_winding_numbers := (u,n_,a_) -> map(round,[
 E_winding_number(u,args[2..-1]),
 E_winding_number((t) -> act_R4[LLL](u(t)),args[2..-1]),
 E_winding_number((t) -> act_R4[  M](u(t)),args[2..-1]),
 E_winding_number((t) -> act_R4[ LM](u(t)),args[2..-1])
]);

#@ cohomology_vector 
cohomology_vector := proc(f,n_,a_)
 local a,w,k,c,t;
 
 a := `if`(nargs > 2, a_, a_E0);

 w := NULL;
 for k from 5 to 8 do
  c := unapply(evalf(subs(a_E=a,f(c_E[k](t)))),t);
  w := w,R2_winding_number(c,args[2..-1]);
 od:
 
 return [w];
end:


