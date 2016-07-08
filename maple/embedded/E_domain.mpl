#@ CLASS: E_point

`Class/Declare`("E_point",
 "An instance of this class  represents a point on the embedded surface EX(a)",

 ["Extends","domain_point"],

 ["StaticField","tolerance"::float = 10.^(-98)],
 ["StaticField","max_iterations"::integer = 100],

 ["StaticField","a"::scalar,"This is the value of $a$, where we are considering $EX(a)$."],
 ["StaticField","alpha0"::scalar, "This is computed from a by the @set_a()@ method"],
 ["StaticField","alpha1"::scalar, "This is computed from a by the @set_a()@ method"],
 ["StaticField","alpha2"::scalar, "This is computed from a by the @set_a()@ method"],
 ["StaticField","alpha3"::scalar, "This is computed from a by the @set_a()@ method"],
 ["StaticField","alpha4"::scalar, "This is computed from a by the @set_a()@ method"],
 ["StaticField","alpha5"::scalar, "This is computed from a by the @set_a()@ method"],
 ["StaticField","alpha6"::scalar, "This is computed from a by the @set_a()@ method"],
 ["StaticField","alpha7"::scalar, "This is computed from a by the @set_a()@ method"],
 ["StaticField","alpha8"::scalar, "This is computed from a by the @set_a()@ method"],
 ["StaticField","alpha9"::scalar, "This is computed from a by the @set_a()@ method"],
 ["StaticField","alpha10"::scalar,"This is computed from a by the @set_a()@ method"],
 ["StaticField","alpha11"::scalar,"This is computed from a by the @set_a()@ method"],
 ["StaticField","alpha12"::scalar,"This is computed from a by the @set_a()@ method"],
 ["StaticField","alpha13"::scalar,"This is computed from a by the @set_a()@ method"],
 ["StaticField","alpha14"::scalar,"This is computed from a by the @set_a()@ method"],

 # This should be a static method, but we have not implemented those.
 ["Method","set_a"::void,"",
  proc(this,a_)
   local a;
   a := `if`(nargs > 1, a_, a_E0);
   this["a"] := a;
   this["alpha0" ] := 1 + a^2;
   this["alpha1" ] := 1 - a^2;
   this["alpha2" ] := this["alpha0"]^2;
   this["alpha3" ] := this["alpha1"]^2;
   this["alpha4" ] := sqrt(2*a^2/this["alpha0"]);
   this["alpha5" ] := sqrt(2*(1-a^2));
   this["alpha6" ] := this["alpha0"]*this["alpha5"];
   this["alpha7" ] := this["alpha1"]+this["alpha6"];
   this["alpha8" ] := 1/2 + 7*a^2/2 + this["alpha6"];
   this["alpha9" ] := 2*a*this["alpha5"];
   this["alpha10"] := 4*(1/a^3-5/a);
   this["alpha11"] := 8-2/a^2;
   this["alpha12"] := 4+1/a^2;
   this["alpha13"] := 16-20/a^2+4/a^4;
   this["alpha14"] := (1-1/a^2)^2;
  end
 ],

 ["Field","x"::[scalar,scalar,scalar,scalar],"Coordinates of the point"],
 ["Field","u"::[scalar,scalar,scalar,scalar],"A unit vector orthogonal to $x$, $n$ and $v$"],
 ["Field","v"::[scalar,scalar,scalar,scalar],"A unit vector orthogonal to $x$, $n$ and $u$"],
 ["Field","n"::[scalar,scalar,scalar,scalar],"Normalised gradient of $g$"],
 ["Field","z"::[scalar,scalar],"Basic G-invariant functions of $x$"],
 ["Field","ndg"::scalar,"Norm of the gradient of $g$"],
 ["Field","sndg"::scalar,"Square norm of the gradient of $g$"],
 ["Field","indg"::scalar,"Inverse norm of the gradient of $g$"],
 ["Field","curvature"::scalar,"Gaussian curvature of the surface"],

 ["Method","fix"::void,"",
  proc(this)
   this["fix_x"];
   this["fix_n"];
   this["fix_uv"];
   this["z"] := simp(z_proj0(this["x"]));
   this["curvature"] := simp(curvature0(this["x"]));
   NULL;
  end
 ],

 ["Method","fix_x"::void,
  "This method adjusts the coordinates of a point to ensure that it lies on the surface $EX(a)$ and in the fundamental domain.  Moreover, if the constraint field is set to a value indicating that the point should lie on one of the curves $C_0$, $C_1$, $C_3$ or $C_5$, then the coordinates are adjusted if necessary to ensure that that holds.",
  proc(this)
   local iterations_left,gx,r,err,pp,sndg,x,x1,constraint; 

   x := this["x"];
   x := simp(x);
   this["x"] := x;

   constraint := this["constraint"];

   # If x lies exactly on EX and any boundary constraint is exactly
   # satisfied then we do nothing (and in particular, we do not convert
   # any coordinates to floating point numbers).

   if not(hastype(x,float)) and
      simplify(g0(x)) = 0 and
      simplify(rho(x) - 1) = 0 then
    if constraint = CONSTRAINT_FIXED then
     return;
    elif constraint = CONSTRAINT_C0 and x[3] = 0 and x[4] = 0 then
     return;
    elif constraint = CONSTRAINT_C1 and simplify(x[1] - x[2]) = 0 and x[4] = 0 then
     return;
    elif constraint = CONSTRAINT_C3 and x[1] = 0 then
     return;
    elif constraint = CONSTRAINT_C5 and x[2] = 0 then
     return;
    fi;
   fi;

   # If we reach this point, then we are going to need to do numerical 
   # adjustments, so we convert everything to floating point.
   x := evalf(x);
   this["x"] := x;

   if (constraint = CONSTRAINT_V0) then
    this["set_v0",true];
   elif (constraint = CONSTRAINT_V3) then
    this["set_v3",true];
   elif (constraint = CONSTRAINT_V6) then
    this["set_v6",true];
   elif (constraint = CONSTRAINT_V11) then
    this["set_v11",true];
   elif (constraint >= CONSTRAINT_FIXED) then
    # This should not occur
   elif (constraint = CONSTRAINT_C0) then
     # The general method would be inefficient for points on C0
     x := [max(x[1],0),max(x[2],0),0,0];
     r := sqrt(x[1]*x[1] + x[2]*x[2]);
     this["x"] := x /~ r;
   elif (constraint = CONSTRAINT_C1) then
     # The general method would be inefficient for points on C1
     x := [(x[1]+x[2])/2,(x[1]+x[2])/2,x[3],0];
     r := sqrt(2*x[1]^2 + x[3]^2);
     this["x"] := x /~ r;
   else 
    # We now do a Newton-Raphson procedure to find a nearby point on
    # the surface EX.  The method for points on C3 and C5 is essentially
    # the same as for interior points, except for a couple of extra 
    # lines where we impose the boundary conditions.

    iterations_left := this["max_iterations"];

    x := this["x"];
    r := sqrt(rho(x));
    x := x /~ r;
    gx := g0(x);
    this["x"] := x;
    this["fix_n"];
    sndg := this["sndg"];
    err := 1.;

    while((err > this["tolerance"]) and (iterations_left > 0)) do 
     pp := evalf(gx/sqrt(sndg));
     if (pp >  0.1) then pp :=  0.1; fi;
     if (pp < -0.1) then pp := -0.1; fi;

     this["x"] := this["x"] -~ pp * this["n"];

     if (constraint = CONSTRAINT_C3) then
      this["x"] := map(abs,this["x"] *~ [0,1,1,-1]) *~ [0,1,1,-1];
     fi;

     if (constraint = CONSTRAINT_C5) then
      this["x"] := map(abs,this["x"] *~ [1,0,1,-1]) *~ [1,0,1,-1];
     fi;

     r := sqrt(rho(this["x"]));
     this["x"] := this["x"] /~ r;

     this["fix_n"];
     gx := evalf(g0(this["x"]));
     sndg := this["sndg"];

     err := abs(gx);

     iterations_left := iterations_left - 1;
    od;
   fi;
   NULL;
  end
 ],

 ["Method","fix_n"::void,
  "This method sets @this[\"n\"]@ to the normalisation of the gradient of $g$ at the point @this[\"x\"]@.  It uses the @simp()@ function, which simplifies symbolically unless we have any floating point subexpressions, in which case it evaluates numerically.",
  proc(this)
   local i,n,sndg,ndg,x;

   x := this["x"];

   n := simp(dg0(x));
   sndg := simp(n[1]*n[1] + n[2]*n[2] + n[3]*n[3] + n[4]*n[4]);
   ndg := simp(sqrt(sndg));
   n := simp(n /~ ndg);

   this["n"] := n;
   this["sndg"] := sndg;
   this["ndg"]  := ndg;
   this["indg"] := simp(1/ndg);
   NULL;
  end
 ],

 ["Method","fix_uv"::void,
  "This method sets @this[\"u\"]@ and @this[\"v\"]@ in such a way that @[this[\"x\"],this[\"n\"],this[\"u\"],this[\"v\"]]@ is an oriented orthonormal basis for $\\mathbb{R}^4$.",
  proc(this)
   local r,dp,uu,vv,t,constraint,x,n,u,v,u1,u2,u3,u4;

   constraint := this["constraint"];
   x := this["x"];
   n := this["n"];
   u := this["u"];
   v := this["v"];

   if (constraint = CONSTRAINT_C0) then
    u := [-x[2],x[1],0,0];
    v := [0,0,-2*a_E0,1 - 2*x[1]*x[1]];
    vv := simplify(sqrt(v[3]*v[3] + v[4]*v[4]));
    v := v /~ vv;
   elif (constraint = CONSTRAINT_C1) then
    u := [x[3]/sqrt(2),x[3]/sqrt(2),-x[1]*sqrt(2),0];
    v := [2 - (1+1/a_E0^2)*x[3]*x[3],-2 + (1+1/a_E0^2)*x[3]*x[3],0,4*x[1]*x[3]/a_E0];
    vv := simplify(sqrt(v[1]*v[1] + v[2]*v[2] + v[3]*v[3] + v[4]*v[4]));
    v := v /~ vv;
   elif (constraint = CONSTRAINT_C3) then
    u2 := -a_E0*x[2]*x[2]*x[4]-x[3]*x[3]*x[3]+(a_E0^3+a_E0)*x[4]*x[3]*x[3]+
	      ((3*a_E0^2+2)*x[4]*x[4]+a_E0^2)*x[3]-2*a_E0^3*x[4];
    u3 := -2*x[2]*x[2]*x[2]*a_E0^2+((1-a_E0^2)*x[3]*x[3]+2*a_E0*x[3]*x[4]-2*a_E0^2*x[4]*x[4])*x[2];
    u4 := x[2]*x[2]*x[2]*a_E0+(-2*a_E0*x[3]*x[3]+(-2*a_E0^2-2)*x[4]*x[3])*x[2];

    u := simplify([0,u2,u3,u4]);
    uu := simplify(sqrt(u[2]*u[2] + u[3]*u[3] + u[4]*u[4]));
    u := simplify(u /~ uu);
    v := [1,0,0,0];
   elif (constraint = CONSTRAINT_C5) then
    u1 := -x[3]*x[3]*x[3]-(a_E0^3+2*a_E0)*x[4]*x[3]*x[3]+((3*a_E0^2+2)*x[4]*x[4]+a_E0^2)*x[3]-
	   a_E0*x[4]*x[4]*x[4]+(2*a_E0^3+a_E0)*x[4];
    u3 := ((1+a_E0^2)*x[3]*x[3]-2*a_E0*x[3]*x[4]-2*a_E0^2)*x[1];
    u4 := (3*a_E0*x[3]*x[3]-2*(1+a_E0^2)*x[4]*x[3]+a_E0*x[4]*x[4]-a_E0)*x[1];

    u := simplify([u1,0,u3,u4]);
    uu := simplify(sqrt(u[1]*u[1] + u[3]*u[3] + u[4]*u[4]));
    u := simplify(u /~ uu);
    v := [0,-1,0,0];
   else
    u := [1,1,1,1];
    dp := add(x[t]*u[t],t=1..4);
    u := simplify(u -~ dp *~ x);
    dp := add(n[t]*u[t],t=1..4);
    u := simplify(u -~ dp *~ n);
    u := simplify(u /~ sqrt(add(u[t]^2,t=1..4)));
    v := simplify([
      -x[2]*n[3]*u[4]+x[2]*u[3]*n[4]-n[2]*u[3]*x[4]+n[2]*x[3]*u[4]-u[2]*x[3]*n[4]+u[2]*n[3]*x[4],
       x[1]*n[3]*u[4]-x[1]*u[3]*n[4]-n[1]*x[3]*u[4]+n[1]*u[3]*x[4]+u[1]*x[3]*n[4]-u[1]*n[3]*x[4],
      -x[1]*n[2]*u[4]+x[1]*u[2]*n[4]+n[1]*x[2]*u[4]-n[1]*u[2]*x[4]-u[1]*x[2]*n[4]+u[1]*n[2]*x[4],
       x[1]*n[2]*u[3]-x[1]*u[2]*n[3]-n[1]*x[2]*u[3]+n[1]*u[2]*x[3]+u[1]*x[2]*n[3]-u[1]*n[2]*x[3]
      ]);
    v := simplify(v /~ sqrt(add(v[t]^2,t=1..4)));
   fi;

   this["u"] := simp(u);
   this["v"] := simp(v);
   NULL;
  end
 ],

 ["Method","set_C0"::void,"This method sets the current point to lie on the curve $C_0$.  As the parameter $t$ runs from $0$ to $1$, the point covers the section of $C_0$ contained in the fundamental domain $F_{16}$ starting with $v_6$ at $t=0$, and moving to $v_3$ at $t=1$.",
  proc(this,t::scalar)
   local theta;

   this["constraint"] := CONSTRAINT_C0;
   theta := simp(Pi/4 * (1 + t));
   this["x"] := simp([cos(theta),sin(theta),0,0]);
   this["fix"];
   NULL;
  end
 ],

 ["Method","set_C1"::void,"This method sets the current point to lie on the curve $C_1$.  As the parameter $t$ runs from $0$ to $1$, the point covers the section of $C_1$ contained in the fundamental domain $F_{16}$ starting with $v_0$ at $t=0$, and moving to $v_6$ at $t=1$.",
  proc(this,t::scalar)
   local theta;

   this["constraint"] := CONSTRAINT_C1;
   theta := simp(Pi/2*t);
   this["x"] := simp([sin(theta)/sqrt(2),sin(theta)/sqrt(2),cos(theta),0]);
   this["fix"];
   NULL;
  end
 ],

 ["Method","set_C3"::void,"This method sets the current point to lie on the curve $C_3$.  As the parameter $t$ runs from $0$ to $1$, the point covers the section of $C_3$ contained in the fundamental domain $F_{16}$ starting with $v_{11}$ at $t=0$, and moving to $v_3$ at $t=1$.",
  proc(this,t::scalar)
   local theta,C,S,u0,u1,u2,x2,x3,x4;

   this["constraint"] := CONSTRAINT_C3;
   theta := simp(Pi/2*t);
   C := simp(cos(theta));
   S := simp(sin(theta));

   u0 := this["alpha2"] - 2*this["a"]^2*this["alpha0"] * C^2 + this["alpha3"] * C^4;
   u1 := this["alpha0"] * S^2 + sqrt(u0);
   u2 := this["alpha1"] + 2*this["a"]^2*S^2;
   x2 := S * sqrt(2*u2/u1);
   x3 := this["alpha4"] * C;
   x4 := -x3*u2/(this["a"]*u1);

   this["x"] := simp([0,x2,x3,x4]);

   this["fix"];
   NULL;
  end
 ],

 ["Method","set_C5"::void,"This method sets the current point to lie on the curve $C_5$.  As the parameter $t$ runs from $0$ to $1$, the point covers the section of $C_5$ contained in the fundamental domain $F_{16}$ starting with $v_0$ at $t=0$, and moving to $v_{11}$ at $t=1$.",
  proc(this,t::scalar)
   local theta,C,S,u0,u1,u2,x1,x3,x4;

   this["constraint"] := CONSTRAINT_C5;
   theta := simp(Pi*t);
   C := simp(cos(theta));
   S := simp(sin(theta));

   u0 := this["alpha1"]/2*C^2 - this["alpha7"]*C + this["alpha8"];
   u1 := this["alpha9"]*(1-C) + 4*this["a"];
   u2 := this["alpha1"]*this["alpha5"]*(3-C);
   x1 := -1/2*S*sqrt(u2/u0);
   x3 := sqrt(this["a"]*u1/u0);
   x4 := this["alpha5"]*(C-1)*x3/(4*this["a"]);

   this["x"] := simp([x1,0,x3,x4]);

   this["fix"];
   NULL;
  end
 ],

 ["Method","set_random"::void,"This sets the current point to a randomly generated point in $F_{16}$",
  proc(this) 
   this["set",CONSTRAINT_FREE,retract_F16_E(random_X_point())];
  end
 ],

 NULL
);

######################################################################

#@ CLASS: E_sample_point

`Class/Declare`("E_sample_point",
 "An instance of this class represents a point in a triangular face of a domain of type @E@.  In addition to the usual data for a point in such a domain, it has some additional fields that only make sense relative to the containing face.",

 ["Extends","E_point"],

 ["Field","barycentric_coords"::list(scalar),
  "The (generalised) barycentric coordinates of the point, relative to the containing face.  This is a list of three nonnegative scalars, whose sum is one."
 ],

 ["Field","ibj"::scalar = 0,
  "This field is the Jacobian of the inverse of the barycentric coordinate map.  (TODO: clarify the normalisation conditions.)"
 ],

 ["Field","quadrature_weight"::scalar,
  "This will be set in such a way that the integral (with respect to metric area) of a function on the face can be approximated by the sum of the values at the sample points multiplied by the corresponding quadrature weights"
 ]
);

######################################################################

#@ CLASS: E_edge

`Class/Declare`(
 "E_edge",
 "",
 ["Extends","domain_edge"],

 ["Field","hyperbolic_length"::scalar],

 ["Constructor","",
  proc(this,P0::E_point,P1::E_point)
   this["end"] := table();
   this["set",P0,P1];
  end
 ]
);

#######################################################################

#@ CLASS: E_face

`Class/Declare`(
 "E_face",
 "",
 ["Extends","domain_face"],
 ["Field","samples"::table,"This is a table indexed by some points in the 2-simplex $\\Delta_2$.  The entries are instances of the class @E_sample_point@.  Typically we will fix a triangle quadrature rule @Q@, with points $t_i$ say.  The @samples@ table will have indices $t_i$, and the corresponding entry will represent a point $u_i\\in EX^*$ with barycentric coordinates $t_i$ with respect to this face.  This is set up by the @create_samples@ method."],
 ["Field","sample_list"::list,"This contains the same @E_sample_point@ objects as the @samples@ table, but organised as a list.  This is set up by the @create_samples@ method."],
 ["Field","total_area"::scalar = 0,"The area of this face, with respect to the metric on $EX^*$ inherited from the standard metric on $S^3$.  This is calculated by the @create_samples@ method."],
 ["Field","total_curvature"::scalar = 0,"The integral of the Gaussian curvature function over this face.  This is calculated by the @create_samples@ method."],
 ["Field","barycentric_table","obsolete"],
 
 ["Constructor","",
  proc(this,S0::E_edge,S1::E_edge,S2::E_edge)
   local C;

   this["corner"] := table();
   this["side"] := table();
   this["samples"] := table();
   this["set",S0,S1,S2];
   C := eval(this["corner"]);
  end
 ],

 ["Method","create_samples"::void,
  "This method sets up the fields @samples@, @samples_list@, @total_area@ and @total_curvature@, as described above.",
  proc(this,Q::triangle_quadrature_rule)
   local A,F,tt,ttt,x,xt,P,i,j,k,V,L;

   this["samples"] := table():

   A := [this["corner"][0]["x"],
         this["corner"][1]["x"],
	 this["corner"][2]["x"]];

   L := [];
   this["total_area"] := 0;
   this["total_curvature"] := 0;

   F := barycentric_inverse_F(A);

   for i from 1 to Q["num_points"] do
    tt := Q["points"][i];
    P := `new/E_sample_point`();
    P["set",CONSTRAINT_FREE,barycentric_inverse(A,tt,F)];
    P["ibj"] := simp(1/barycentric_jacobian(A,P["x"]));
    P["quadrature_weight"] := simp(P["ibj"] * Q["weights"][i]); 
    this["samples"][op(tt)] := eval(P);
    L := [op(L),eval(P)];
    this["total_area"] := this["total_area"] + P["quadrature_weight"];
    this["total_curvature"] := this["total_curvature"] + 
      simp(P["quadrature_weight"] * P["curvature"]);
   od;

   this["sample_list"] := L;
   NULL;
  end
 ],

 ["Method","check","This checks the correctness of the sample points.  It returns an offset error (which measures the failure of the points to lie exactly on $EX^*$) and a barycentric error (which measures the failure of the barycentric coordinates to be equal to the indices in the @samples@ table).",
  proc(this)
   local a1,a2,a3,e,x0,x1,x2,t1,tt,i,
	 barycentric_err,barycentric_worst,
	 interpolation_err,interpolation_worst,
	 offset_err,offset_worst,
	 F16_err,F16_worst;

   a1 := this["corner"][0]["x"];
   a2 := this["corner"][1]["x"];
   a3 := this["corner"][2]["x"];

   barycentric_worst := NULL;
   offset_worst := NULL;
   F16_worst := NULL;

   barycentric_err := 0;
   offset_err := 0;
   F16_err := 0;
   
   for tt in indices(this["samples"]) do
    x0 := this["samples"][op(tt)]["x"];
    e := max(abs(rho(x0)-1),abs(g_01(x0)));
    if (e > offset_err) then
     offset_err := e;
     offset_worst := tt;
    fi;

    e := abs(min(evalf([0,x0[1],x0[2],x0[3],g_10(x0)])));
    if (e > F16_err) then
     F16_err := e;
     F16_worst := tt;
    fi;
    
    t1 := barycentric_coords([a1,a2,a3],x0);
    e := d3f(tt,t1);
    if (e > barycentric_err) then
     barycentric_err := e;
     barycentric_worst := tt;
    fi;
   od:

   return [barycentric_err,barycentric_worst,
           offset_err,offset_worst,
	   F16_err,F16_worst];
  end
 ],

 ["Method","plot_x","",
  proc(this,f,vertical_range := -2..2)
   local S;
   S := eval(this["samples"]);
   display(
    seq(point([op(triangle_proj(t0)),f(S[op(t0)]["x"])],colour=red),
     t0 in [indices(S)]),
    triangle_axes(vertical_range),
    axes=none,scaling=constrained
   );
  end
 ],

 ["Method","plot_z","",
  proc(this,f,z_range := -2..2)
   this["plot_x",(u) -> f(z_proj1(u))];
  end
 ],
 
 ["Method","plot_x_ibj","",
  proc(this,f,vertical_range := -2..2)
   local S;
   S := eval(this["samples"]);
   display(
    seq(point([op(triangle_proj(t0)),f(S[op(t0)]["x"]) * S[op(t0)]["ibj"]],colour=red),
     t0 in [indices(S)]),
    triangle_axes(vertical_range),
    axes=none,scaling=constrained
   );
  end
 ],

 ["Method","plot_z_ibj","",
  proc(this,f,z_range := -2..2)
   this["plot_x",(u) -> f(z_proj1(u))];
  end
 ]
);

######################################################################

#@ CLASS: E_grid

`Class/Declare`(
 "E_grid",
 "",
 ["Extends","grid"],
 ["Field","triangle_quadrature_rule"],
 ["Field","int_table"::table,"This is a table indexed by triples $(i,j,k)$; the values are the integrals over $F_{16}$ of $z_1^iz_2^j|n|^k$"],

 ["Constructor",
  "",
  proc(this)
   this["points"] := table();
   this["edges"] := table();
   this["faces"] := table();
   this["edges_by_ends"] := table();
   this["faces_by_corners"] := table();
   this["int_table"] := table();
   this["domain"] := E_domain;
  end
 ],

 ["Method","create_samples"::void,"This invokes the @create_samples@ method for each face.",
  proc(this)
   local i,n,F;
   n := this["num_faces"];
   for i from 0 to n-1 do
    userinfo(6,genus2,sprintf("Creating samples for face %d/%d",i,n));
    F := eval(this["faces"][i]);
    F["create_samples",this["triangle_quadrature_rule"]];
   od;
   NULL;
  end
 ],

 ["Method","int_x",
  "The argument $f$ is assumed to be a polynomial in $x_1,\\dotsc,x_4$.  The method returns an approximation to the integral over $EX(a)$ of $f$, or of $f |n|^k$ if $k$ is given as an additional argument.  Here $n$ is the gradient of $g$.",
  proc(this,f,k_)
   local f1;
   f1 := NF_z(expand(add(act_A[T](f),T in G16)));
   return this["int_z",f1,args[2..-1]];
  end
 ],

 ["Method","int_z",
  "The argument $f$ is assumed to be an expression in $z_1$ and $z_2$.  The method returns an approximation to the integral over F16 of $f$, or of $f |n|^k$ if $k$ is given as an additional argument.  Note that this is 1/16 times the integral over $EX(a)$.",
  proc(this,f,k_)
   local J,i,F,tt,P,f0,k;

   J := 0;
   k := `if`(nargs > 2,k_,0);

   for i in indices(this["faces"]) do
    F := eval(this["faces"][op(i)]);
    for tt in indices(F["samples"]) do
     P := eval(F["samples"][op(tt)]);
     f0 := subs({z[1]=P["z"][1],z[2]=P["z"][2]},f);
     J := J + f0 * P["quadrature_weight"] * P["ndg"]^k;
    od;
   od;

   return(J);
  end
 ],

 ["Method","set_int_table","This sets one value in @int_table@",
  proc(this,i,j,k_)
   local k;
   k := `if`(nargs > 3, k_, 0);
   this["int_table"][i,j,k] := this["int_z",z[1]^i*z[2]^j,k];
  end
 ],

 ["Method","int_z_by_table","This assumes that $f$ is a polynomial in $z_1$ and $z_2$.  It calculates the integral of $f$ over $F_{16}$ by looking up the integrals of the individual monomials in @int_table@.  If any of the required integrals are missing from @int_table@, then they will be calculated and saved there.",
  proc(this,f,k_)
   local f0,k,n1,n2,i1,i2,b,c,J;
   f0 := expand(f);
   k := `if`(nargs > 2,k_,0);

   n1 := degree(f0,z[1]);
   n2 := degree(f0,z[2]);
   J := 0;

   for i1 from 0 to n1 do
    for i2 from 0 to n2 do
     c := coeff(coeff(f0,z[1],i1),z[2],i2);
     if c <> 0 then
      b := this["int_table"][i1,i2,k];
      if not(type(b,numeric)) then
       this["set_int_table",i1,i2,k];
       b := this["int_table"][i1,i2,k];
      fi;
      J := J + b * c;
     fi;
    od;
   od;
   return(J);
  end
 ],

 ["Method","set_max_deg"::void,"Calculate and save integrals for all monomials in $z_1$ and $z_2$ of degree at most $d$",
  proc(this,d::posint)
   local i,j,m,u;

   for m from 0 to d do
    for i from 0 to m do
     j := m - i;
     u := z[1]^i * z[2]^j;
     userinfo(7,genus2,sprintf("Integrating %A",u));
     this["int_z_by_table",u];
    od;
   od:
   NULL;
  end
 ],

 ["Method","total_area","This returns the total area of $F_{16}$",
  proc(this)
   add(this["faces"][i]["total_area"],i=0..this["num_faces"]-1);
  end
 ],

 ["Method","total_curvature","This returns the integral of the curvature over $F_{16}$",
  proc(this)
   add(this["faces"][i]["total_curvature"],i=0..this["num_faces"]-1);
  end
 ],

 ["Method","curvature_error","If the integration rule is accurate, then the result of this method should be zero, by the Gauss-Bonet Theorem.",
  proc(this)
   evalf(Pi/4 + this["total_curvature"]);
  end
 ],

 ["Method","stokes_error","Here @ff@ should be a list $(f_1,f_2)$ of two expressions in $z_1$ and $z_2$.  The method returns the approximated integral of the exterior derivative of $f_1\\alpha_1+f_2\\alpha_2$, where the forms $\\alpha_i$ are defined in @embedded/roothalf/forms.mpl@.  If the integration rule is accurate, then the result should be zero, by Stokes's Theorem.",
  proc(this,ff)
   if `class/E_point`["StaticFieldValue"]["a"] <> 1/sqrt(2) then
    return(FAIL);
   fi;
   
   this["int_z",stokes_alpha(ff)];
  end
 ]
);

######################################################################

#@ E_domain

E_domain := `new/domain`():
E_domain["set_point_class","E_point"]:
E_domain["set_edge_class" ,"E_edge" ]:
E_domain["set_face_class" ,"E_face" ]:
E_domain["set_grid_class" ,"E_grid" ]:

if (type(a_E0,realcons)) then
 `new/E_point`()["set_a",a_E0];
fi:

######################################################################

