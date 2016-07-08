#@ CLASS: E_chart

`Class/Declare`("E_chart",
 "An instance of this class represents an approximate conformal map $p\\colon\\mathbb{R}^2\\to EX^*$.  We refer to the point $p(0)$ as the centre of the chart.",
 ["Field","centre"::RR0_4,"A point in $EX^*$ which is the centre of the chart"],
 ["Field","square_centre"::RR0_2,"The image under @square_diffeo_E0()@ of the centre.  This is stored separately to allow for the case where we take square_centre to be an exact rational point, and then compute centre numerically from @square_centre@"],
 ["Field","n"::RR0_4,"The unit normal vector to $EX^*$ in $S^3$ at the centre"],
 ["Field","u"::RR0_4,"A unit tangent vector to $EX^*$ at the centre"],
 ["Field","v"::RR0_4,"Another unit tangent vector to $EX^*$ at the centre"],
 ["Field","p","This is a polynomial function $\\mathbb{R}^2\\to\\mathbb{R}^4$ which sends $0$ to the centre point, and is approximately a conformal map to $EX^*$."],
 ["Field","p0","The same as $p$, but with coefficients evaluated numerically."],
 ["Field","p_inv_approx","A linear map $\\mathbb{R}^4\\to\\mathbb{R}^2$ which is approximately inverse to $p$ near the centre"],
 ["Field","degree"::posint,"The degree of the polynomials in $p$."],
 ["Field","coeffs_exact"::boolean = false,"true if the polynomial coefficients in $p$ are exact."],
 ["Field","vertex_index"::integer = NULL,"If the centre lies at $v_k$, this field should be set to $k$."],
 ["Field","curve_index"::integer = NULL,"If the centre lies on the curve $C_k$, this field should be set to $k$."],
 ["Field","curve_parameter"::RR0 = NULL,"If the centre is $c_{Ek}(t)$, then this field should be set to $t$."],
 ["Field","grid_index"::integer = NULL],

 ["Method","vertex_set_exact","Set @vertex_index@ to @k@, and calculate various associated data, using exact coefficients where appropriate.  This gives a chart of polynomial degree one.",
  proc(this,k)
   this["vertex_index"] := k;

   if k = 0 then
    this["curve_set_exact",1,0];
   elif k = 3 then
    this["curve_set_exact",0,Pi/2];
   elif k = 6 then
    this["curve_set_exact",1,Pi/2];
   elif k = 11 then
    this["curve_set_exact",3,0];
   else
    this["vertex_index"] := NULL;
    error("Invalid vertex index");
   fi;
  end
 ],

 ["Method","vertex_set_numeric","Set @vertex_index@ to @k@, and calculate various associated data, using numerical coefficients.  This gives a chart of polynomial degree one.",
  proc(this,k)
   this["vertex_index"] := k;

   if k = 0 then
    this["curve_set_numeric",1,0];
   elif k = 3 then
    this["curve_set_numeric",0,Pi/2];
   elif k = 6 then
    this["curve_set_numeric",1,Pi/2];
   elif k = 11 then
    this["curve_set_numeric",3,0];
   else
    this["vertex_index"] := NULL;
    error("Invalid vertex index");
   fi;
  end
 ],

 ["Method","curve_set_exact","Set @curve_index@ to @k0@ and @curve_parameter@ to @t0@, and calculate various associated data, using exact coefficients where appropriate.  This gives a chart of polynomial degree one.  For charts of this type, we will ensure that $p(t,0)$ is the Taylor approximation to $c_{k0}(t0+t)$.",
  proc(this,k0::integer,t0::RR0)
   local trig_rels,aa,bb,cc,nn,p0,x;

   trig_rels := {
    sin(Pi/8)=sqrt(2-sqrt(2))/2,
    cos(Pi/8)=sqrt(2+sqrt(2))/2,
    sin(3*Pi/8)=sqrt(2+sqrt(2))/2,
    cos(3*Pi/8)=sqrt(2-sqrt(2))/2
   };

   this["curve_index"] := k0;
   this["curve_parameter"] := t0;
   this["degree"] := 1;
   this["coeffs_exact"] := true;

   aa := simplify(expand(rationalize(subs(trig_rels,c_E0[k0](t0)))));
   aa := simplify(expand(combine(rationalize(aa))));

   this["centre"] := aa;
   this["square_centre"] := simplify(expand(combine(rationalize(square_diffeo_E0(aa)))));

   nn := simplify(expand(combine(rationalize(dg0(aa)))));
   nn := simplify(expand(combine(rationalize(nn /~ nm4(nn)))));
   this["n"] := nn;

   bb := simplify(expand(rationalize(subs(trig_rels,subs(t=t0,map(diff,c_E0[k0](t),t))))));
   bb := simplify(expand(combine(rationalize(bb))));
   cc := simplify(expand(rationalize(subs(trig_rels,subs(a_E=a_E0,conformal_twist(aa,bb))))));
   cc := simplify(expand(combine(rationalize(cc))));

   this["u"] := simplify(expand(combine(rationalize(bb /~ nm4(bb)))));
   this["v"] := simplify(expand(combine(rationalize(cc /~ nm4(cc)))));

   p0 := expand(aa +~ t *~ bb +~ u *~ cc);
   p0 := simplify(expand(combine(p0)));
   p0 := subs({t=s[1],u=s[2]},p0);
   this["p"] := unapply(p0,s);
   this["p0"] := unapply(evalf(p0),s);
   this["p_inv_approx"] := 
    unapply(evalf(
     [add(x[i]*bb[i],i=1..4)/add(bb[i]^2,i=1..4),
      add(x[i]*cc[i],i=1..4)/add(cc[i]^2,i=1..4)]),x);

   NULL;
  end
 ],

 ["Method","curve_improve_exact","Increase the polynomial degree of this chart by one.",
  proc(this)
   local trig_rels,m,t0,d,p0,p0t,p0u,a0,a1,a2,a3,aa0,aa1,aa2,aa3,
    err0,err1,err2,err3,err4,d0,d1,d2,d3,d2c,d3c,eqs,sol,p1,p2,c,i,j,s,t,u,e;
   
   trig_rels := {
    sin(Pi/8)=sqrt(2-sqrt(2))/2,
    cos(Pi/8)=sqrt(2+sqrt(2))/2,
    sin(3*Pi/8)=sqrt(2+sqrt(2))/2,
    cos(3*Pi/8)=sqrt(2-sqrt(2))/2
   };

   m := this["curve_index"];
   t0 := this["curve_parameter"];
   p0 := unapply(eval(this["p"]([t,u])),t,u);
   p0 := unapply(simplify(expand(rationalize(subs(trig_rels,p0(t,u))))),t,u);
   d := this["degree"]+1;
   p0t := unapply(map(diff,p0(t,u),t),t,u);
   p0u := unapply(map(diff,p0(t,u),u),t,u);
   a0 := p0(0,0);
   a1 := simplify(expand(rationalize(subs(trig_rels,dg0(a0)))));
   a2 := map(coeff,map(coeff,p0(t,u),t,1),u,0);
   a3 := map(coeff,map(coeff,p0(t,u),t,0),u,1);
   aa0 := 1;
   aa1 := simplify(expand(dp4(a1,a1)));
   aa2 := simplify(expand(dp4(a2,a2)));
   aa3 := aa2;
   err0 := coeff(rem(rho(p0(e*t,e*u))-1,e^(d+1),e),e,d);
   err1 := coeff(rem(g0(p0(e*t,e*u)),e^(d+1),e),e,d);
   err2 := map(coeff,simplify(expand(rationalize(subs(trig_rels,convert(map(series,p0(e*t,0) -~ c_E0[m](e*t+t0),e=0,d+1),polynom,e))))),e,d);
   err3 := coeff(rem(dp4(p0t(e*t,e*u),p0u(e*t,e*u)),e^d,e),e,d-1);
   err4 := coeff(rem(dp4(p0t(e*t,e*u),p0t(e*t,e*u))-dp4(p0u(e*t,e*u),p0u(e*t,e*u)),e^d,e),e,d-1);
   d0 := expand(-err0/2);
   d1 := expand(-err1/dp4(a1,a1));
   d2 := add(d2c[i]*t^i*u^(d-i),i=0..d);
   d3 := add(d3c[i]*t^i*u^(d-i),i=0..d);
   eqs := [coeffs(expand(err3/aa2 + diff(d2,u) + diff(d3,t)),{t,u}),
	   coeffs(expand(err4/aa2 + 2*diff(d2,t) - 2*diff(d3,u)),{t,u}),
	   d2c[d] + coeff(dp4(err2,a2)/aa2,t,d),
	   d3c[d] + coeff(dp4(err2,a3)/aa3,t,d)];
   sol := solve(eqs);
   d2 := subs(sol,d2);
   d3 := subs(sol,d3);
   p1 := simplify(expand(rationalize(d0 *~ a0 +~ d1 *~ a1 +~ d2 *~ a2 +~ d3 *~ a3)));
   p1 := simplify(expand(combine(p1)));
   p2 := p0(t,u) +~ [p1[1],p1[2],p1[3],p1[4]];
   p2 := subs({t=s[1],u=s[2]},p2);

   this["p"] := unapply(p2,s);
   this["p0"] := unapply(evalf(p2),s);
   this["degree"] := d;
   NULL;
  end
 ],

 ["Method","curve_set_degree_exact","Set the degree of this chart to @d@, and calculate the appropriate coefficients.",
  proc(this,d,force_)
   local p,s;

   while this["degree"] < d do this["curve_improve_exact"]; od;
   if this["degree"] > d and nargs > 2 and force_ then
    p := this["p"](s);
    p := multi_series(p,d+1,s[1],s[2]);
    this["p"] := unapply(p,s);
    this["p0"] := unapply(evalf(p),s);
    this["degree"] := d;
   fi;

   NULL;
  end
 ],

 ["Method","curve_set_numeric","Set @curve_index@ to @k0@ and @curve_parameter@ to @t0@, and calculate various associated data, using numerical coefficients.  This gives a chart of polynomial degree one.  For charts of this type, we will ensure that $p(t,0)$ is the Taylor approximation to $c_{k0}(t0+t)$.",
  proc(this,k0,t0)
   local nn,aa,bb,cc,p0;

   this["curve_index"] := k0;
   this["curve_parameter"] := evalf(t0);
   this["degree"] := 1;
   this["coeffs_exact"] := false;

   aa := evalf(c_E0[k0](t0));

   this["centre"] := aa;
   this["square_centre"] := evalf(square_diffeo_E0(aa));

   nn := evalf(dg0(aa));
   nn := nn /~ nm4(nn);
   this["n"] := nn;

   bb := evalf(subs(t=t0,map(diff,c_E0[k0](t),t)));
   cc := evalf(subs(a_E=a_E0,conformal_twist(aa,bb)));

   this["u"] := bb /~ nm4(bb);
   this["v"] := cc /~ nm4(cc);

   p0 := expand(aa +~ t *~ bb +~ u *~ cc);
   p0 := subs({t=s[1],u=s[2]},p0);
   this["p"] := unapply(p0,s);
   this["p0"] := unapply(p0,s);

   this["p_inv_approx"] := 
    unapply(evalf(
     [add(x[i]*bb[i],i=1..4)/add(bb[i]^2,i=1..4),
      add(x[i]*cc[i],i=1..4)/add(cc[i]^2,i=1..4)]),x);

   NULL;
  end
 ],

 ["Method","curve_improve_numeric","Increase the polynomial degree of this chart by one.",
  proc(this)
   local m,t0,d,p0,p0t,p0u,a0,a1,a2,a3,aa0,aa1,aa2,aa3,err0,err1,err2,err3,err4,d0,d1,d2,d3,d2c,d3c,eqs,sol,p1,p2,c,i,j,s,t,u,e;
   
   m := this["curve_index"];
   t0 := this["curve_parameter"];
   p0 := unapply(this["p"]([t,u]),t,u);
   d := this["degree"]+1;
   p0t := unapply(map(diff,p0(t,u),t),t,u);
   p0u := unapply(map(diff,p0(t,u),u),t,u);
   a0 := evalf(p0(0,0));
   a1 := evalf(dg0(a0));
   a2 := map(coeff,map(coeff,p0(t,u),t,1),u,0);
   a3 := map(coeff,map(coeff,p0(t,u),t,0),u,1);
   aa0 := 1;
   aa1 := simplify(expand(dp4(a1,a1)));
   aa2 := simplify(expand(dp4(a2,a2)));
   aa3 := aa2;
   err0 := coeff(rem(rho(p0(e*t,e*u))-1,e^(d+1),e),e,d);
   err1 := coeff(rem(evalf(g0(p0(e*t,e*u))),e^(d+1),e),e,d);
   err2 := map(coeff,evalf(convert(map(series,p0(e*t,0) -~ c_E0[m](e*t+t0),e=0,d+1),polynom,e)),e,d);
   err3 := coeff(rem(dp4(p0t(e*t,e*u),p0u(e*t,e*u)),e^d,e),e,d-1);
   err4 := coeff(rem(dp4(p0t(e*t,e*u),p0t(e*t,e*u))-dp4(p0u(e*t,e*u),p0u(e*t,e*u)),e^d,e),e,d-1);
   d0 := expand(-err0/2);
   d1 := expand(-err1/dp4(a1,a1));
   d2 := add(d2c[i]*t^i*u^(d-i),i=0..d);
   d3 := add(d3c[i]*t^i*u^(d-i),i=0..d);
   eqs := [coeffs(expand(err3/aa2 + diff(d2,u) + diff(d3,t)),{t,u}),
	   coeffs(expand(err4/aa2 + 2*diff(d2,t) - 2*diff(d3,u)),{t,u}),
	   d2c[d] + coeff(dp4(err2,a2)/aa2,t,d),
	   d3c[d] + coeff(dp4(err2,a3)/aa3,t,d)];
   sol := solve(eqs);
   d2 := subs(sol,d2);
   d3 := subs(sol,d3);
   p1 := expand(d0 *~ a0 +~ d1 *~ a1 +~ d2 *~ a2 +~ d3 *~ a3);
   p2 := p0(t,u) +~ [p1[1],p1[2],p1[3],p1[4]];
   p2 := subs({t=s[1],u=s[2]},p2);

   this["p"] := unapply(p2,s);
   this["p0"] := unapply(p2,s);
   this["degree"] := d;
   NULL;
  end
 ],

 ["Method","curve_set_degree_numeric","Set the degree of this chart to @d@, and calculate the appropriate coefficients.",
  proc(this,d,force_)
   local p,s;

   while this["degree"] < d do this["curve_improve_numeric"]; od;
   if this["degree"] > d and nargs > 2 and force_ then
    p := this["p"](s);
    p := multi_series(p,d+1,s[1],s[2]);
    this["p"] := unapply(p,s);
    this["p0"] := unapply(p,s);
    this["degree"] := d;
   fi;

   NULL;
  end
 ],

 ["Method","centre_set_numeric","Set the centre and calculate coefficients, giving a chart of polynomial degree one.",
  proc(this,x0)
   local f0,p0,rels,sol,s,t,u,x,a;

   this["curve_index"] := NULL;
   this["curve_parameter"] := NULL;
   this["degree"] := 2;
   this["coeffs_exact"] := false;

   this["centre"] := evalf(x0);
   this["square_centre"] := evalf(square_diffeo_E0(x0));
   f0 := full_frame_b(x0):
   this["n"] := f0[2];
   this["u"] := f0[3];
   this["v"] := f0[4];

   p0 := (1-t^2/2.-u^2/2.) *~ x0 +~ t *~ this["u"] +~ u *~ this["v"] +~
           (a[1,0]*u^2+a[1,1]*t*u+a[1,2]*t^2) *~ this["n"]:
   rels := [seq(coeff(coeff(multi_series(g1(p0),3,t,u),t,i),u,2-i),i=0..2)]:
   sol := solve(rels):
   p0 := subs(sol,p0);
   p0 := subs({t=s[1],u=s[2]},p0);

   this["p"] := unapply(p0,s);
   this["p0"] := unapply(evalf(p0),s);
   this["p_inv_approx"] := 
    unapply(evalf(
     [add(x[i]*this["u"][i],i=1..4),
      add(x[i]*this["v"][i],i=1..4)]),x);

   NULL;
  end
 ],

 ["Method","centre_improve_numeric","Increase the polynomial degree of this chart by one.",
  proc(this)
   local p0,p0t,p0u,rels,sol,d,s,t,u,a;


   p0 := this["p0"]([t,u]);
   d := this["degree"];

   p0 := p0 +~ 
	 add(a[1,j]*t^j*u^(d+1-j),j=0..d+1) *~ this["centre"] +~
	 add(a[2,j]*t^j*u^(d+1-j),j=0..d+1) *~ this["n"] +~
	 add(a[3,j]*t^j*u^(d+1-j),j=0..d  ) *~ this["u"] +~
	 add(a[4,j]*t^j*u^(d+1-j),j=0..d  ) *~ this["v"]:
   p0t := map(diff,p0,t):
   p0u := map(diff,p0,u):
   rels := [
    seq(coeff(coeff(multi_series(rho(p0) - 1,d+2,t,u),t,j),u,d+1-j),j=0..d+1),
    seq(coeff(coeff(multi_series(g1(p0),d+2,t,u),t,j),u,d+1-j),j=0..d+1),
    seq(coeff(coeff(multi_series(dp4(p0t,p0u),d+1,t,u),t,j),u,d-j),j=0..d),
    seq(coeff(coeff(multi_series(dp4(p0t,p0t)-dp4(p0u,p0u),d+1,t,u),t,j),u,d-j),j=0..d),
    NULL
   ]:
   sol := solve(rels):
   p0 := subs(sol,p0):
   p0 := subs({t=s[1],u=s[2]},p0);

   this["p"] := unapply(p0,s);
   this["p0"] := unapply(p0,s);
   this["degree"] := d+1;
   NULL;
  end
 ],

 ["Method","centre_set_degree_numeric","Set the degree of this chart to @d@, and calculate the appropriate coefficients.",
  proc(this,d,force_)
   local p,s;

   while this["degree"] < d do
    this["centre_improve_numeric"];
   od;
   if this["degree"] > d and nargs > 2 and force_ then
    p := this["p"](s);
    p := multi_series(p,d+1,s[1],s[2]);
    this["p"] := unapply(p,s);
    this["p0"] := unapply(p,s);
    this["degree"] := d;
   fi;

   NULL;
  end
 ],

 ["Method","improve_numeric","Increase the polynomial degree by one, using the @curve_improve_numeric@ method if applicable, otherwise the @centre_improve_numeric@ method.",
  proc(this)
   if this["curve_index"] = NULL then
    this["centre_improve_numeric"];
   else
    this["curve_improve_numeric"];
   fi;
  end
 ],
 
 ["Method","set_degree_numeric","Set the polynomial degree, using the @curve_set_degree_numeric@ method if applicable, otherwise the @centre_set_degree_numeric@ method.",
  proc(this,d)
   if this["curve_index"] = NULL then
    this["centre_set_degree_numeric",args[2..-1]];
   else
    this["curve_set_degree_numeric",args[2..-1]];
   fi;
  end
 ],
 
 ["Method","square_set_numeric","",
  proc(this,s0)
   local x0,k,t0;
   
   x0 := square_diffeo_E0_inverse_search(s0);

   if s0[2] = 0 then
    k := 1;
    t0 := arctan(sqrt(2.)*x0[2],x0[3]);
   elif s0[2] = 1 then
    k := 3;
    t0 := arctan(x0[2],sqrt(1.5)*x0[3]);
   elif s0[1] = 0 then
    k := 0;
    t0 := arctan(x0[2],x0[1]);
   elif s0[1] = 1 then
    k := 5;
    t0 := arctan(x0[1],x0[4]+x0[3]/sqrt(8.));
   else
    k := NULL;
    t0 := NULL;
   fi;
   
   if k = NULL then
    this["centre_set_numeric",x0];
   else
    this["curve_set_numeric",k,t0];
   fi;

   this["square_centre"] := s0;   
  end
 ],

 ["Method","isometrize","This method assumes that @log_rescale_z@ defines a function $f$ on $EX^*$, and adjusts the chart to make it approximately isometric as a map from (the unit disk with the hyperbolic metric) to ($EX^*$ with $\\exp(2f)$ times the standard metric).",
  proc(this,log_rescale_z)
   local d0,d1,x0,r0,m0,p0,p1,zp0,nlrz,dlrz,nlrp,dlrp,lrp,rp0,rp1,ps1,sp,s1,err,errs,sol,s,a,x;

   d0 := this["degree"];
   x0 := this["centre"];
   r0 := exp(evalf(log_rescale_z(z_proj0(x0))));
   p0 := this["p0"]([s[1],s[2]]);
   ps1 := subs({s[1]=0,s[2]=0},map(diff,p0,s[1]));
   m0 := 2/r0/nm4(ps1);
   p0 := subs({s[1]=s[1]*m0,s[2]=s[2]*m0},p0);
   zp0 := multi_series(z_proj1(p0),d0+1,s[1],s[2]):
   nlrz := unapply(numer(log_rescale_z(z)),z):
   dlrz := unapply(denom(log_rescale_z(z)),z):
   nlrp := multi_series(nlrz(zp0),d0+1,s[1],s[2]):
   dlrp := multi_series(dlrz(zp0),d0+1,s[1],s[2]):
   lrp := multi_series(nlrp/dlrp,d0+1,s[1],s[2]):
   rp0 := multi_series(exp(2*lrp),d0+1,s[1],s[2]);

   sp := [s[1],s[2]]:
   for d1 from 2 to d0 do
    sp := expand(C_mult([s[1],s[2]],sp));
    s1 := expand([s[1],s[2]] +~ C_mult([a[1],a[2]],sp));
    p1 := multi_series(eval(subs(s=s1,p0)),d1+1,s[1],s[2]);
    rp1 := multi_series(eval(subs(s=s1,rp0)),d1+1,s[1],s[2]);
    ps1 := map(diff,p1,s[1]);
    err := multi_series(dp4(ps1,ps1) *~ rp1 - 4/(1-s[1]^2-s[2]^2)^2,d1,s[1],s[2]);
    errs := [seq(coeff(coeff(err,s[1],i),s[2],d1-1-i),i=0..d1-1)];
    sol := LSSolve(errs)[2];
    s1 := subs(sol,s1);
    p0 := multi_series(eval(subs(s=s1,p0)),d0+1,s[1],s[2]);
    rp0 := multi_series(eval(subs(s=s1,rp0)),d0+1,s[1],s[2]);
   od:

   this["p"] := unapply(p0,s);
   this["p0"] := unapply(p0,s);
   this["coeffs_exact"] := false;
   this["p_inv_approx"] :=
    unapply(expand((1/m0) *~ this["p_inv_approx"]([x[1],x[2],x[3],x[4]])),x);
  end
 ],

 ["Method","isometry_error","This returns a measure of the failure of $p$ to be isometric, in the context described above.",
  proc(this,log_rescale_z,s1::RR0_2)
   local x1,u1,v1,r1,E1,F1,G1,E2,s;

   x1 := evalf(this["p0"](s1));
   u1 := evalf(subs({s[1]=s1[1],s[2]=s1[2]},map(diff,this["p0"](s),s[1])));
   v1 := evalf(subs({s[1]=s1[1],s[2]=s1[2]},map(diff,this["p0"](s),s[2])));
   r1 := evalf(exp(log_rescale_z(z_proj1(x1))));
   E1 := dp4(u1,u1) * r1^2;
   F1 := dp4(u1,v1) * r1^2;
   G1 := dp4(v1,v1) * r1^2;
   E2 := evalf(4/(1-s1[1]^2-s1[2]^2)^2);

   return max(abs(E1-E2),abs(F1),abs(G1-E2));
  end
 ],

 ["Method","p_c","This is the composite of $p\\colon\\mathbb{R}^2\\to EX^*$ with the standard isomorphism $\\mathbb{C}\\to\\mathbb{R}^2$.",
  proc(this)
   local z;
   return unapply(this["p"]([Re(z),Im(z)]),z);
  end
 ],

 ["Method","p_inv","Inverse to the map $p\\colon\\mathbb{R}^2\\to EX^*$",
  proc(this,x0)
   local u0,v0,s0,r0,p0,eqs,vars,sol,s1,m;

   s0 := evalf(this["p_inv_approx"](x0));
   r0 := max(nm2(s0)/4,10.^(-10));
   p0 := eval(this["p0"]);
   u0 := evalf(this["u"]);
   v0 := evalf(this["v"]);

   eqs := evalf({dp4(u0,p0([s[1],s[2]]) -~ x0) = 0,
                 dp4(v0,p0([s[1],s[2]]) -~ x0) = 0});

   sol := FAIL;
   m := 10;

   while m > 0 and not type(sol,set) do
    m := m-1;
    r0 := 2*r0;
    vars := {s[1] = s0[1]-r0..s0[1]+r0,
             s[2] = s0[2]-r0..s0[2]+r0};

    sol := fsolve(eqs,vars);
   od;
   
   if not type(sol,set) then return FAIL; fi;

   s1 := subs(sol,[s[1],s[2]]);
   if nm2(s1) < 1 then
    return(s1);
   else
    return FAIL;
   fi;
  end
 ],

 ["Method","p_inv_c","Inverse to the map $p_c\\colon\\mathbb{C}\\to EX^*$",
  proc(this,x0)
   local s0;
   s0 := this["p_inv",x0];

   return `if`(s0 = FAIL,FAIL,R2_to_C(s0));
  end
 ],

 ["Method","disc_plot","Generate a plot showing the image under $p$ of a disc of radius $r$ centred at the origin in $\\mathbb{R}^2$",
  proc(this,r)
   local p0,p1,s,t;
   
   p0 := eval(this["p0"]);
   p1 := p0([s*cos(t),s*sin(t)]);
   plot3d(stereo(p1),s=0..r,t=0..2*Pi,axes=none,scaling=constrained);
  end
 ],

 ["Method","circle_plot","Generate a plot showing the image under $p$ of a circle of radius $r$ centred at the origin in $\\mathbb{R}^2$",
  proc(this,r)
   local p0,p1,s,t;
   
   p0 := eval(this["p0"]);
   p1 := p0([r*cos(t),r*sin(t)]);
   spacecurve(stereo(p1),t=0..2*Pi,colour=red,axes=none,scaling=constrained);
  end
 ],

 ["Method","err_plot","Generate a plot measuring the failure of $p$ to be a conformal map landing in $EX^*$.  This uses the values of $p$ and its derivatives on a circle of radius $r$ centred at the origin in $\\mathbb{R}^2$",
  proc(this,r)
   local p0,p1,s,t,u,v,errs;
   
   p0 := eval(this["p0"]);
   p1 := p0([s*cos(t),s*sin(t)]);
   u := map(diff,p1,s);
   v := map(diff,p1,t) /~ s;
   errs := subs(s=r,[
    rho(p1) - 1,
    g1(p1),
    dp4(u,v),
    dp4(u,u) - dp4(v,v)
   ]);
   
   plot(errs,t=0..2*Pi);
  end
 ],

 ["Method","check","This returns a table @T@ whose entries are nonnegative real numbers.  If the chart has all the properties that it is supposed to have, then the entries in @T@ will all be zero.  The entry @T[\"max\"]@ is the maximum of all the other entries.  In practice we find that when working to 100 digit precision, we end up with @T[\"max\"] < 10^(-89)@.  Note that we only test whether we have the correct Taylor coefficients for $p$, we do not test anything about how well the series is converging.",
  proc(this)
   local T,k0,t0,u0,s,t,e,p,p0,p01,p02,d;

   p := eval(this["p"]);
   p0 := eval(this["p0"]);
   d  := this["degree"];
   
   T := table();
   T["centre_rho"] := abs(evalf(rho(this["centre"]) - 1));
   T["centre_g"]   := abs(evalf(g0(this["centre"])));
   T["square_centre"] := evalf(d2(square_diffeo_E0(this["centre"]),this["square_centre"]));
   if this["curve_index"] <> NULL then
    k0 := this["curve_index"];
    t0 := this["curve_parameter"];
    T["curve_centre"] := evalf(d4(this["centre"],c_E0[k0](t0)));
    u0 := evalf(subs(t=t0,map(diff,c_E0[k0](t),t)));
    u0 := u0 /~ nm4(u0);
    T["curve_u"] := evalf(d4(this["u"],u0));
   fi;
   T["nn"] := abs(evalf(dp4(this["n"],this["n"]) - 1));
   T["uu"] := abs(evalf(dp4(this["u"],this["u"]) - 1));
   T["vv"] := abs(evalf(dp4(this["v"],this["v"]) - 1));
   T["xn"] := abs(evalf(dp4(this["centre"],this["n"])));
   T["xu"] := abs(evalf(dp4(this["centre"],this["u"])));
   T["xv"] := abs(evalf(dp4(this["centre"],this["v"])));
   T["nu"] := abs(evalf(dp4(this["n"],this["u"])));
   T["nv"] := abs(evalf(dp4(this["n"],this["v"])));
   T["uv"] := abs(evalf(dp4(this["u"],this["v"])));
   T["orientation"] :=
    abs(Determinant(Matrix([this["centre"],this["n"],this["u"],this["v"]]))-1);
   T["p_p0"] := max(map(abs,map(coeffs,evalf(p(s) -~ p0(s)),[s[1],s[2]])));
   T["degree"] := max(map(degree,p([e*s[1],e*s[2]]),e)) - d;
   T["p0_rho"] :=
    max(map(abs,[coeffs(evalf(multi_series(rho(p0(s)) - 1,d+1,s[1],s[2])),[s[1],s[2]])]));
   T["p0_g"] :=
    max(map(abs,[coeffs(evalf(multi_series(g0(p0(s)),d+1,s[1],s[2])),[s[1],s[2]])]));
   p01 := map(diff,p0(s),s[1]);
   p02 := map(diff,p0(s),s[2]);
   T["conformal_a"] :=
    max(map(abs,[coeffs(multi_series(dp4(p01,p02),d,s[1],s[2]),[s[1],s[2]])]));
   T["conformal_b"] :=
    max(map(abs,[coeffs(multi_series(dp4(p01,p01)-dp4(p02,p02),d,s[1],s[2]),[s[1],s[2]])]));
   T["p1u"] := abs(signum(evalf(dp4(subs({s[1]=0,s[2]=0},p01),this["u"]))) - 1);
   T["p2v"] := abs(signum(evalf(dp4(subs({s[1]=0,s[2]=0},p02),this["v"]))) - 1);
   T["p1v"] := abs(evalf(dp4(subs({s[1]=0,s[2]=0},p01),this["v"])));
   T["p2u"] := abs(evalf(dp4(subs({s[1]=0,s[2]=0},p02),this["u"])));
   T["p_inv_approx"] := 
    max(map(abs,map(coeffs,multi_series(C["p_inv_approx"](C["p0"](s)),2,s[1],s[2]) -~ [s[1],s[2]],[s[1],s[2]])));

   T["max"] := max(map(op,[entries(T)]));
   
   return eval(T);
  end
 ]
);

######################################################################

#@ CLASS: E_atlas

`Class/Declare`("E_atlas",
 "An instance of this class represents an atlas of approximate conformal charts for $EX^*$",

 ["Constructor","",
  proc(this)
   this["charts"] := table();
  end
 ],
 
 ["Field","num_charts"::integer],
 ["Field","charts"::table],

 ["Method","add_chart","",
  proc(this)
   local C,n;
   
   n := this["num_charts"];
   C := `new/E_chart`();
   C["grid_index"] := n;
   this["charts"][n] := eval(C);
   this["num_charts"] := n+1;
   return eval(C);
  end
 ],

 ["Method","add_vertex_chart_exact","",
  proc(this,k::integer,d::posint)
   local C;
   
   C := eval(this["add_chart"]);
   C["vertex_set_exact",k];
   C["curve_set_degree_exact",d];
   return eval(C);
  end
 ],

 ["Method","add_vertex_chart_numeric","",
  proc(this,k::integer,d::posint)
   local C;
   
   C := eval(this["add_chart"]);
   C["vertex_set_numeric",k];
   C["curve_set_degree_numeric",d];
   return eval(C);
  end
 ],

 ["Method","add_curve_chart_exact","",
  proc(this,k::integer,t0::RR0,d::posint)
   local C;
   
   C := eval(this["add_chart"]);
   C["curve_set_exact",k,t0];
   C["curve_set_degree_exact",d];
   return eval(C);
  end
 ],

 ["Method","add_curve_chart_numeric","",
  proc(this,k::integer,t0::RR0,d::posint)
   local C;
   
   C := eval(this["add_chart"]);
   C["curve_set_numeric",k,t0];
   C["curve_set_degree_numeric",d];
   return eval(C);
  end
 ],

 ["Method","add_square_chart_numeric","",
  proc(this,s0::RR0_2,d::posint)
   local C;
   
   C := eval(this["add_chart"]);
   C["square_centre_set_numeric",s0];
   C["centre_set_degree_numeric",d];
   return eval(C);
  end
 ],

 ["Method","add_centre_chart_numeric","",
  proc(this,x0::RR0_4,d::posint)
   local C;
   
   C := eval(this["add_chart"]);
   C["centre_set_numeric",x0];
   C["centre_set_degree_numeric",d];
   return eval(C);
  end
 ]
);
 
