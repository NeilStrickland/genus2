#@ CLASS: EH_chart

`Class/Declare`("EH_chart",
 "An instance of this class represents an approximate isometric map $p\\colon\\Delta\\to EX^*$, together with data relating $p$ to the canonical map $q\\colon\\Delta\\to EX^*$.  Here $\\Delta$ is given the standard hyperbolic metric, and $EX^*$ is given the rescaled metric that has curvature $-1$.  We refer to the point $p(0)$ as the centre of the chart.  Note that this class extends the class #E_chart#, so documentation for that class should be read in conjunction with documentation for this one.",
 ["Extends","E_chart"],
 ["Field","curve_H_parameter"::RR0,"If the centre is $q(c_{Hk}(t))$, then this field should be set to $t$."],
 ["Field","alpha"::CC0,"This should be set so that $p(z)=q(\\lambda(z-\\alpha)/(1-\\overline{\\alpha}z)$"],
 ["Field","lambda"::CC0,"This should be set so that $p(z)=q(\\lambda(z-\\alpha)/(1-\\overline{\\alpha}z)$"],
 ["Field","beta"::CC0,"This should be set so that $\\beta = -\\lambda\\alpha$, so $p(0)=q(\\beta)$.  However, in some cases we will work out $\\beta$ first, and only set $\\alpha$ and $\\lambda$ later."],
 ["Field","grid_index"::integer],

 ["Method","m_c","The map $z\\mapsto\\lambda(z-\\alpha)/(1-\\overline{\\alpha}z)$",
  proc(this)
   local z;
   if not (type(this["alpha"],CC0) and type(this["lambda"],CC0)) then
    error("(alpha,lambda) is not set");
   fi;

   unapply(this["lambda"] *
          (z - this["alpha"])/(1 - conjugate(this["alpha"]) * z),z);
  end
 ],

 ["Method","m_inv_c","The inverse of the map $z\\mapsto\\lambda(z-\\alpha)/(1-\\overline{\\alpha}z)$",
  proc(this)
   local z,mu,beta;
   if not (type(this["alpha"],CC0) and type(this["lambda"],CC0)) then
    error("(alpha,lambda) is not set");
   fi;

   mu := conjugate(this["lambda"]);
   beta := - this["lambda"] * this["alpha"];

   unapply(mu * (z - beta)/(1 - conjugate(beta) * z),z);
  end
 ],

 ["Method","m","The map $z\\mapsto\\lambda(z-\\alpha)/(1-\\overline{\\alpha}z)$, composed with the standard isomorphism $\\mathbb{R}^2\\to\\mathbb{C}$",
  proc(this)
   local s;
   assume(s[1]::real);
   assume(s[2]::real);
   
   return unapply(simplify(C_to_R2(this["m_c"](s[1]+I*s[2]))),s);
  end
 ],

 ["Method","m_inv","The inverse map $z\\mapsto\\lambda(z-\\alpha)/(1-\\overline{\\alpha}z)$, composed with the standard isomorphism $\\mathbb{C}\\to\\mathbb{R}^2$",
  proc(this)
   local s;
   assume(s[1]::real);
   assume(s[2]::real);
   
   return unapply(simplify(C_to_R2(this["m_inv_c"](s[1]+I*s[2]))),s);
  end
 ],

 ["Method","q","",
  proc(this)
   unapply(this["p"](this["m_inv"](z)),z);
  end
 ],

 ["Method","q_c","",
  proc(this)
   unapply(this["p_c"](this["m_inv_c"](z)),z);
  end
 ],

 ["Method","q_inv","",
  proc(this,x0)
   local s0;
   s0 := this["p_inv",x0];
   `if`(s0 = FAIL,FAIL,this["m"](s0));
  end
 ],

 ["Method","q_inv_c","",
  proc(this,x0)
   local s0;
   s0 := this["p_inv",x0];
   `if`(s0 = FAIL,FAIL,R2_to_C(this["m"](s0)));
  end
 ],

 ["Method","vertex_set","",
  proc(this,k::integer,d::posint,A::EH_atlas)
   this["vertex_index"] := k;

   if k = 0 then
    this["curve_set",1,0,d,A];
   elif k = 3 then
    this["curve_set",0,Pi/2,d,A];
   elif k = 6 then
    this["curve_set",1,Pi/2,d,A];
   elif k = 11 then
    this["curve_set",3,0,d,A];
   else
    this["vertex_index"] := NULL;
    error("Invalid vertex index");
   fi;
  end
 ],

 ["Method","curve_set","",
  proc(this,k::integer,t0::RR0,d::posint,A::EH_atlas)
   local t,t1,cH,lambda,alpha;

   `E_chart!curve_set_numeric`(this,k,evalf(t0));
   `E_chart!curve_set_degree_numeric`(this,d);
   `E_chart!isometrize`(this,A["log_rescale_z"]);

   cH := evalf(subs(a_H=A["a_H"],c_H[k](t)));
   t1 := evalf(A["u_inv"][k](t0));
   this["curve_H_parameter"] := t1;
   lambda := evalf(subs(t=t1,diff(cH,t)));
   lambda := lambda/abs(lambda);
   alpha  := evalf(subs(t=t1,cH))/(-lambda);
   this["lambda"] := lambda;
   this["alpha"]  := alpha;
   this["beta"]   := -lambda*alpha;
   NULL;
  end
 ],

 ["Method","centre_set","",
  proc(this,x0::RR0_4,d::posint,A::EH_atlas)
   local t1,lambda,alpha;

   `E_chart!centre_set_numeric`(this,evalf(x0));
   `E_chart!centre_set_degree_numeric`(this,d);
   `E_chart!isometrize`(this,A["log_rescale_z"]);

   this["lambda"] := NULL;
   this["alpha"]  := NULL;
   this["beta"]   := NULL;
  end
 ],

 ["Method","square_set","",
  proc(this,s0::RR0_2,d::posint,A::EH_atlas)
   local t1,lambda,alpha;

   `E_chart!square_set_numeric`(this,s0);
   `E_chart!centre_set_degree_numeric`(this,d);
   `E_chart!isometrize`(this,A["log_rescale_z"]);

   this["lambda"] := NULL;
   this["alpha"]  := NULL;
   this["beta"]   := NULL;
  end
 ]
):

######################################################################

#@ CLASS: EH_atlas_edge

`Class/Declare`("EH_atlas_edge",
 "An instance of this class represents a pair of charts whose centres are close together.  We can triangulate $F_{16}$ using the chart centres as 0-simplices, and these pairs as 1-simplices.",
 ["Field","grid_index"::integer,"As part of the data of an atlas, we have a list of edges, numbered from 0 to $n-1$ say.  If this is the $i$'th edge in the list, then this @grid_index@ field should be set equal to $i$."],
 ["Field","start_index"::integer,"If this edge links chart number $i$ to chart number $j$ (with $i<j$) then @start_index@ should be $i$ and @end_index@ should be $j$."],
 ["Field","end_index"::integer,"If this edge links chart number $i$ to chart number $j$ (with $i<j$) then @start_index@ should be $i$ and @end_index@ should be $j$."],
 ["Field","start_z"::CC0,"Suppose that this edge links chart number $i$ to chart number $j$, and chart $j$ is given by  $p:C\\to EX^*$.  There will then be a small number $z$ such that $p(z)$ is the centre of chart $i$, and this $z$ should be stored in the @start_z@ field."],
 ["Field","end_z"::CC0,"Suppose that this edge links chart number $i$ to chart number $j$, and chart $i$ is given by  $p:C\\to EX^*$.  There will then be a small number $z$ such that $p(z)$ is the centre of chart $j$, and this $z$ should be stored in the @end_z@ field."],
 ["Field","curve_index"::integer = NULL,"If this edge lies along one of the four curves that bound F16, then this field should be set to the index of the relevant curve (0,1,3 or 5)."],
 ["Field","H_length"::RR0,"This is the hyperbolic distance between the @beta@ fields of the two charts.  If everything is accurate, it should be the same as the @EH_length@ field."],
 ["Field","EH_length"::RR0,"This is an estimate of the distance between the centres of the two charts, with respect to the rescaled metric on $EX^*$.   If everything is accurate, it should be the same as the @H_length@ field."],

 ["Method","colour","If this edge lies along one of the four curves that bound $F_{16}$, then this method will return the appropriate colour.  Edges that do not lie on a boundary curve will be coloured grey.",
  proc(this)
   if this["curve_index"] <> NULL then
    return c_colour[this["curve_index"]];
   else
    return grey;
   fi;
  end
 ]
):

######################################################################

#@ CLASS: EH_atlas

`Class/Declare`("EH_atlas",
 "An instance of this class represents an approximation to the isomorphism $HX(a)\\to EX^*$, together with associated data.  ",

 ["Extends","E_atlas"],

 ["Constructor","",
  proc(this)
   this["curve_lengths"] := table();
   this["curve_a_H_estimates"] := table();
   this["u"] := table();
   this["u_inv"] := table();
   this["c_E_rescaled_speed"] := table();
   this["c_E_rescaled_length"] := table();
   this["c_E_average_rescaled_speed"] := table();
   this["charts"] := table();
   this["edges"] := table();
  end
 ],

 ["Field","a_H"::RR0,"This is an approximation to the number a such that $EX^*$ is isomorphic to $HX(a)$"],

 ["Field","log_rescale_z","This is an approximation to the function $f$ such that $\\exp(2f)$ times the metric tensor inherited from $\\mathbb{R}^4$ has curvature equal to $-1$.  It is represented here as a function from the $z$-plane to $\\mathbb{R}$.  It should be set using the @set_rescale@ method, or one of the @find_rescale_*@ methods."],

 ["Field","log_rescale_x","This is an approximation to the function $f$ such that $\\exp(2f)$ times the metric tensor inherited from $\\mathbb{R}^4$ has curvature equal to $-1$.  It is represented here as a function from $\\mathbb{R}^4$ to $\\mathbb{R}$.  It should be set using the @set_rescale@ method, or one of the @find_rescale_*@ methods."],

 ["Field","rescale_z","This is the composite of $\\exp$ with the map in the @log_rescale_z@ field.  It should be set using the @set_rescale@ method."],
 ["Field","rescale_x","This is the composite of $\\exp$ with the map in the @log_rescale_x@ field.  It should be set using the @set_rescale@ method, or one of the @find_rescale_*@ methods."],

 ["Field","rescale_type"::string,"This should be \"pade\" or \"poly\", to specify whether the @log_rescale_z@ function is polynomial or rational"],

 ["Field","rescale_degree"::posint,"When the @log_rescale_z@ function is polynomial in $z_1$ and $z_2$, this field specifies the total degree.  When the @log_rescale_z@ function is rational in $z_1$ and $z_2$, this field specifies the total degree of the numerator and denominator.  This field should be set by the @set_rescale_type@ method."],

 ["Field","rescale_dof"::posint,"This field holds the number of degrees of freedom that we can vary when specifying the @log_rescale_z@ function.  It is calculated by the @set_rescale_type@ method from the @rescale_type@ and @rescale_degree@ fields."],

 ["Field","curvature_z","This is the curvature of the rescaled metric, represented as a function from the $z$-plane to $\\mathbb{R}$.  It is set by the @set_rescale@ method, and also by the @find_rescale_*@ methods."],

 ["Field","quadrature_rule"::E_quadrature_rule,"This is a quadrature rule, which we use to measure the total deviation of the rescaled curvature from the desired value of $-1$.  It is not important for the quadrature to be accurate.  This field must be set before any of the @find_rescale_*@ methods can be used."],

 ["Field","rescaling_error"::numeric,"This is the integral of $(K+1)^2$, where $K$ is the curvature of the rescaled metric.  It is set by the @set_rescale@ method, and also by the @find_rescale_*@ methods."],

 ["Field","curve_lengths"::table,"This is a table indexed by $\\{0,1,3,5\\}$, which holds the lengths (with respect to the rescaled metric) of the sides of the fundamental domain $F_{16}$.  These lengths are set by the @find_a_H@ method."],

 ["Field","curve_a_H_estimates"::table,"This is a table indexed by $\\{0,1,3,5\\}$.  The $i$'th entry is an estimate of $a_H$ obtained from the length of edge $i$ in $F_{16}$.  These estimates are set by the @find_a_H@ method."],

 ["Field","a_H_discrepancy","This is the maximum of the differences between the estimates of $a_H$ obtained from the different edges of $F_{16}$.  This is set by the @find_a_H@ method."],

 ["Field","u_degree"::posint,"The fields discussed below are Fourier sin series.  The @u_degree@ field holds the number of terms in each of these series.  This number is set by supplying it as an argument to the @find_u@ method."],

 ["Field","u"::table,"This is a table indexed by $\\{0,1,3,5\\}$.  The $i$'th entry is an approximation to the function $u$ such that $q(c_{Hi}(t))=c_{Ei}(u(t))$.  It has the form $u(t) = t + \\sum_j a_j\\sin(jmt)$, where $m=4$ for $i=0$, and $m=2$ for $i\\in\\{1,3\\}$, and $m=1$ for $i=5$.  This table is set by the @find_u@ method."],

 ["Field","u_inv"::table,"This is a table, whose entries are approximately the inverses of the functions stored in the @u@ field.  This table is set by the @find_u@ method."],

 ["Field","c_E_rescaled_speed"::table,"This is a table indexed by $\\{0,1,3,5\\}$.  The $i$'th entry is a Fourier approximation to the speed of $c_{Ei}(t)$ with respect to the rescaled metric.  This table is set by the @find_u@ method."],

 ["Field","c_E_rescaled_length"::table,"This is a table indexed by $\\{0,1,3,5\\}$.  The $i$'th entry is the indefinite integral of the corresponding entry in the @c_E_rescaled_speed@ field; this is a constant multiple of the $i$'th entry in the @u_inv@ field.  This table is set by the @find_u@ method."],

 ["Field","c_E_average_rescaled_speed"::table,"This is a table indexed by $\\{0,1,3,5\\}$.  The $i$'th entry is the time average of the corresponding entry in the @c_E_rescaled_speed@ field.  This table is set by the @find_u@ method."],

 ["Field","H_samples"::list,"A list of closely spaced points in $HF_{16}$, set by the @make_H_samples@ method."],
 ["Field","D_samples"::list,"A list of closely spaced points in a disc centred at the origin that contains $HF_{16}$ (but usually has radius less than 1).  This is set by the @make_D_samples@ method."],
 ["Field","H_samples_q"::list,"Images in $EX^*$ of the points in @H_samples@.  These are set by the @make_H_samples_q@ method, which will only work once we have constructed and adjusted charts to put ourselves in a position to calculate the map @q@."],
 ["Field","D_samples_q"::list,"Images in $EX^*$ of the points in @D_samples@.  These are set by the @make_D_samples_q@ method, which will only work once we have constructed and adjusted charts to put ourselves in a position to calculate the map @q@."],
 ["Field","chart_dist"::table,"Distances between H-sample points and centres of charts.  This can be set by the @set_chart_dist@ method after charts have been created and the @H_samples@ field has been set.  "],

 ["Field","square_q_inv" = NULL,"A polynomial function $\\mathbb{R}^2\\to\\mathbb{C}$ which approximates the inverse of the map $HF_{16}\\xrightarrow{q}EX^*\\xrightarrow{\\delta}[0,1]^2$ (where $\\delta$ is @square_diffeo@).  This can be set by the @set_square_q_inv@ method."],
 
 ["Field","num_edges"::integer,"Some methods rely on having a triangulation of $F_{16}$ with the centres of the charts as vertices.  After adding the charts, we specify the edges of the triangulation using the @add_edge@ method.  The number of edges is stored in the @num_edges@ field."],
 ["Field","edges"::table,"This is a table indexed by natural numbers.  Each entry is an instance of the class @EH_atlas_edge@, representing an edge between chart centres."],
 ["Field","fourier_r_max"::RR0,"The @set_q_approx_fourier@ method will look for a Fourier approximation for $q$ on a disc of this radius centred at the origin.  This radius is set by supplying it as the first argument to @set_q_approx_fourier@."],
 ["Field","fourier_m"::posint,"The number of circles on which we calculate the Fourier series.  This number is set by supplying it as the second argument to @set_q_approx_fourier@."],
 ["Field","fourier_r"::list,"The list of radii of the circles on which we calculate the Fourier series.  This is set by @set_q_approx_fourier@."],
 ["Field","fourier_k"::posint,"The number of sample points on each circle will be $2^k$.  This number is set by supplying it as the third argument to @set_q_approx_fourier@."],
 ["Field","fourier_v"::table,"A table indexed by pairs $(r,t)$, where $r$ is in @fourier_r@ and $t$ is an integer multiple of $2^{-k}$.  The entries are $q(r e^{2\\pi i t})\\in\\mathbb{R}^4$.  This is set by @set_q_approx_fourier@."],
 ["Field","fourier_a"::table,"A table indexed by triples $(k,j,r)$, where $1\\leq k\l\leq 4$ and $j\\geq 0$ and $r$ is in @fourier_r@.  The entries are Fourier coefficients for $q_k$ on the circle of radius $r$.  This is set by @set_q_approx_fourier@."],
 ["Field","fourier_qr"::table,"A table indexed by pairs $(k,r)$, giving a Fourier approximation to $q_k$ on a circle of radius $r$.  This is set by @set_q_approx_fourier@."],
 ["Field","fourier_a_spline"::table,"A table indexed by pairs $(k,j)$, giving a spline approximation to the Fourier coefficient $a_{k,j}(r)$.  This is set by the method @set_fourier_a_spline@, which is called by @set_q_approx_fourier@."],

 ["Field","q_approx","A rational function $\\mathbb{R}^2\\to\\mathbb{R}^4$ which approximates $q$ on a disc containing $HF_4$"],
 ["Field","H_to_P_map","An object encoding information about a cromulent isomorphism $HX(a_H)\\to PX(a_P)$"],
 ["Field","P_to_H_map","An object encoding information about a cromulent isomorphism $PX(a_P)\\to HX(a_H)$"],
 ["Field","E_to_S_map","An object encoding information about a conformal map $EX^*\\to S^2$"],

 ["Method","set_rescale_type","This sets the type and degree of the rescaling function",
  proc(this,t::string,d::posint)
   if t = "poly" then
    this["rescale_type"] := "poly";
    this["rescale_degree"] := d;
    this["rescale_dof"]    := (d+1)*(d+2)/2;
   elif t = "pade" then
    this["rescale_type"]   := "pade";
    this["rescale_degree"] := d;
    this["rescale_dof"]    := d^2+3*d+1;
   else
    error "Invalid scaling type";
   fi;
  end
 ],

 ["Method","set_rescale","This sets the log of rescaling function itself, together with some other fields that can be calculated from it.  The argument should be an expression in $z_1$ and $z_2$.",
  proc(this,f0)
   this["log_rescale_z"] := unapply(f0,z);
   this["log_rescale_x"] := unapply(subs(z = z_proj0(x),f0),x);
   this["rescale_z"]     := unapply(exp(f0),z);
   this["rescale_x"]     := unapply(exp(subs(z = z_proj0(x),f0)),x);
   this["curvature_z"]   := unapply(this["rescaled_curvature",f0],z);

   this["rescaling_error"] := 
    this["find_rescaling_error",f0];
  end
 ],

 ["Method","rescaled_curvature","This calculates the curvature of $\\exp(2f)$ times the metric inherited from $\\mathbb{R}^4$.  It expects $f$ to be an expression in @z[1]@ and @z[2]@ (not a function).  It does not set the @log_rescale_z@ field, because it is intended to be used in an exploratory way when searching for a better rescaling function.",
  proc(this,f)
   return (curvature_z0(z) - laplacian_z0(f))/exp(2*f);
  end
 ],

 ["Method","find_rescaling_error","This calculates the integral of $(K+1)^2$, where $K$ is the curvature of $\\exp(2f)$ times the metric inherited from $\\mathbb{R}^4$.  It expects $f$ to be an expression in @z[1]@ and @z[2]@ (not a function).  It does not set the @rescaling_error@ field, because it is intended to be used in an exploratory way when searching for a better rescaling function.",
  proc(this,f)
   local K;

   K := this["rescaled_curvature",f];
   if not(type(this["quadrature_rule"],E_quadrature_rule)) then
    error("No quadrature rule set");
   fi;
   return this["quadrature_rule"]["int_z",(1+K)^2];
  end
 ],

 ["Method","find_rescale_poly","This method attempts to find a polynomial approximation to the function $f$ such that $\\exp(2f)$ times the standard metric on $EX^*$ has curvature $-1$.  The argument @d@ specifies the degree of polynomial (in $z_1$ and $z_2$) to use.",
  proc(this,d::posint)
   local f0,v0,F,E,A,m;

   this["set_rescale_type","poly",d];

   f0 := this["log_rescale_z"](z);
   if not(type(f0,polynom([z[1],z[2]]))) then
    f0 := .266*z[1]^5-.414*z[1]^4+.353*z[1]^3+0.579e-1*z[1]^2+.421*z[1]-.367-.117*z[1]^4*z[2]-
	   .294*z[1]^3*z[2]-0.952e-1*z[1]^2*z[2]-.197*z[1]*z[2]-.340*z[2]+.676*z[1]^3*z[2]^2+
	   .461*z[1]^2*z[2]^2+.310*z[1]*z[2]^2+.232*z[2]^2-1.06*z[1]^2*z[2]^3-.745*z[1]*z[2]^3-
	   .505*z[2]^3+1.08*z[1]*z[2]^4+1.13*z[2]^4-1.02*z[2]^5:
   fi:

   v0 := this["poly_to_vector",f0,d];

   F := proc(v)
    global F_count;
    local E;
    if type(F_count,integer) then
     F_count := F_count + 1;
    else
     F_count := 1;
    fi;
    E := this["find_rescaling_error",this["vector_to_poly",v]];
    print(["calling F",F_count,log[10](E)]);
    return E;
   end;

   m := this["rescale_dof"];
   E,A := op(NLPSolve(m,F,initialpoint = Vector(v0)));

   this["rescaling_error"] := E;
   f0 := this["vector_to_poly",A];
   this["set_rescale",f0];

   NULL;
  end
 ],

 ["Method","find_rescale_poly_alt","This method attempts to find a polynomial approximation to the function $f$ such that $\\exp(2f)$ times the standard metric on $EX^*$ has curvature $-1$.  The argument @d@ specifies the degree of polynomial (in $z_1$ and $z_2$) to use.  This version is harder to understand than @find_rescale_poly()@, but is much quicker.",
  proc(this,d::posint)
   local Q,n,m,K0,monomials,laplacians,points,rweights,kpoints,lpoints,mpoints,ones,F,J,f0,a0,E,A;

   this["set_rescale_type","poly",d];
   m := this["rescale_dof"];

   Q := eval(this["quadrature_rule"]):
   if not(type([Q],[E_quadrature_rule])) then
    error("No quadrature rule set");
   fi;
   n := Q["num_points"];

   K0 := evalf(curvature_z0(z));

   monomials  := [seq(seq(z[1]^j*z[2]^(i-j),j=0..i),i=0..d)];
   laplacians := map(laplacian_z0,monomials);
   points     := [seq(Q["points"][i]["z"],i=0..n-1)];
   rweights   := Transpose(Vector([seq(sqrt(max(0,Q["weights"][i])),i=0..n-1)]));
   kpoints    := Vector(evalf(map(curvature_z0,points)));
   mpoints    := Matrix([seq(evalf(subs({z[1]=points[i][1],z[2]=points[i][2]},monomials )),i=1..n)]);
   lpoints    := Matrix([seq(evalf(subs({z[1]=points[i][1],z[2]=points[i][2]},laplacians)),i=1..n)]);
   ones       := Vector([1$n]);

   F := proc(a,b)
    local vals,b0,i;

    vals := mpoints . a;
    b0 := rweights *~ (((kpoints - lpoints . a) *~ map(exp, (-2) * vals)) +~ ones);
    for i from 1 to n do b[i] := b0[i]; od;
   end;

   J := proc(a,M)
    local vals,mm,nn,i,j;

    vals  := mpoints . a;
    mm    := rweights *~ map(exp,(-2) * vals);
    nn    := kpoints - lpoints . a;

    for i from 1 to n do
     for j from 1 to m do 
      M[i,j] := mm[i] * (-2*mpoints[i,j]*nn[i] - lpoints[i,j]);
     od;
    od;
   end;

   f0 := this["log_rescale_z"](z);
   if not(type(f0,polynom([z[1],z[2]]))) then
    f0 := .266*z[1]^5-.414*z[1]^4+.353*z[1]^3+0.579e-1*z[1]^2+.421*z[1]-.367-.117*z[1]^4*z[2]-
	   .294*z[1]^3*z[2]-0.952e-1*z[1]^2*z[2]-.197*z[1]*z[2]-.340*z[2]+.676*z[1]^3*z[2]^2+
	   .461*z[1]^2*z[2]^2+.310*z[1]*z[2]^2+.232*z[2]^2-1.06*z[1]^2*z[2]^3-.745*z[1]*z[2]^3-
	   .505*z[2]^3+1.08*z[1]*z[2]^4+1.13*z[2]^4-1.02*z[2]^5:
   fi:

   a0 := Vector(this["poly_to_vector",f0,d]);

   E,A := op(LSSolve([m,n],F,objectivejacobian = J,initialpoint = a0));

   this["rescaling_error"] := 2*E;
   f0 := this["vector_to_poly",A];
   this["set_rescale",f0];

   NULL;
  end
 ],

 ["Method","find_rescale_pade","This method attempts to find a rational approximation to the function $f$ such that $\\exp(2f)$ times the standard metric on $EX^*$ has curvature $-1$.  The argument @d@ specifies the degree of the numerator and denominator (in $z_1$ and $z_2$).",
  proc(this,d::posint)
   local f0,v0,F,E,A,m;

   this["set_rescale_type","pade",d];

   f0 := this["log_rescale_z"](z);
   if not(type(f0,ratpoly(realcons,[z[1],z[2]]))) then
    f0 := (-.36668113-.57931515*z[2]+.76405269*z[1]-0.43866952e-1*z[2]^2+.46321442*z[1]*z[2]-.32418737*z[1]^2)/(1.+.65920001*z[2]-.96109970*z[1]-0.51463451e-2*z[2]^2-.15212659*z[1]*z[2]+.19202748*z[1]^2);
   fi:

   v0 := this["pade_to_vector",f0,d];

   F := proc(v)
    global F_count,last_v;
    local err;

    last_v := convert(v,list):
    
    if type(F_count,integer) then
     F_count := F_count + 1;
    else
     F_count := 1;
    fi;
    err := this["find_rescaling_error",this["vector_to_pade",v]];

    if modp(F_count,10) = 0 then
     print(["rescaling error",F_count,evalf[5](log[10](abs(err)))]);
    fi;
    if modp(F_count,1000) = 0 then
     print(last_v);
    fi;
    
    return err;
   end;

   m := this["rescale_dof"];
   E,A := op(NLPSolve(m,F,initialpoint = Vector(v0)));

   this["rescaling_error"] := E;
   f0 := this["vector_to_pade",A];
   this["set_rescale",f0];

   NULL;
  end
 ],

 ["Method","find_rescale_pade_alt","This method attempts to find a rational approximation to the function $f$ such that $\\exp(2f)$ times the standard metric on $EX^*$ has curvature $-1$.  The argument @d@ specifies the degree of the numerator and denominator (in $z_1$ and $z_2$).  This version is harder to understand than @find_rescale_pade@, but is much quicker.",
  proc(this,d::posint)
   local f0,uv,duv,mb,MB,QQ,np,ZL,WV,vals,C,P,Q,PM,QM,RM,KV0,OV,obj,E,A,i,c,u,v,p,q,r,s,si1,si2,si3,si4,e,FV,LV,KV,EV,JFU,JFV,JF,JLU,JLV,JL,JK,JE,QJ,RJ,fa,FVa,LVa,KVa,KVb,ea,eb,rep,IM,eps,JKa;

   this["set_rescale_type","pade",d];

   f0 := this["log_rescale_z"](z);
   if not(type(f0,ratpoly(realcons,[z[1],z[2]]))) then
    f0 := (-.36668113-.57931515*z[2]+.76405269*z[1]-0.43866952e-1*z[2]^2+.46321442*z[1]*z[2]-.32418737*z[1]^2)/(1.+.65920001*z[2]-.96109970*z[1]-0.51463451e-2*z[2]^2-.15212659*z[1]*z[2]+.19202748*z[1]^2);
   fi:

   uv := Vector(this["pade_to_vector",f0]);

   MB := [seq(seq(z[1]^j*z[2]^(i-j),j=0..i),i=0..d)];
   mb := nops(MB);
   QQ := eval(this["quadrature_rule"]):
   if not(type([QQ],[E_quadrature_rule])) then
    error("No quadrature rule set");
   fi;

   np := QQ["num_points"];
   ZL := [seq(QQ["points"][i]["z"],i=0..np-1)]:
   WV := sqrt(2.) * Vector(map(sqrt,map(max,[seq(QQ["weights"][i],i=0..np-1)],10.^(-4))));
   vals := (u) -> [seq(subs({z[1]=ZL[i][1],z[2]=ZL[i][2]},u),i=1..np)];

   C := factor([seq(laplacian_z_C0[i]/laplacian_z_C0[0],i=1..5)]);
   P[1] := (g) -> C[1]*diff(g,z[1]) + C[2]*diff(g,z[2]) + C[3]*diff(g,z[1],z[1]) + C[4]*diff(g,z[1],z[2]) + C[5]*diff(g,z[2],z[2]);
   P[2] := (g) -> -C[1]*g -2*C[3]*diff(g,z[1]) - C[4]*diff(g,z[2]);
   P[3] := (g) -> -C[2]*g -2*C[5]*diff(g,z[2]) - C[4]*diff(g,z[1]);
   P[4] := (g) -> g;
   P[5] := (g) -> g;
   Q[2] := (h) -> diff(h,z[1]);
   Q[3] := (h) -> diff(h,z[2]);
   Q[4] := (h) -> -C[3]*diff(h,z[1],z[1])-C[4]*diff(h,z[1],z[2])-C[5]*diff(h,z[2],z[2]);
   Q[5] := (h) -> 2*C[3]*diff(h,z[1])+2*C[4]*diff(h,z[2]);
   Q[6] := (h) -> 2*C[5]*diff(h,z[2]);

   for i from 1 to 5 do 
    PM[i] := Transpose(Matrix(map(vals,map(P[i],MB))));
   od:
   for i from 2 to 6 do 
    QM[i] := Transpose(Matrix(map(vals,map(Q[i],MB))));
   od:
   RM := Transpose(Matrix(map(vals,MB))): # matrix of values of monomials

   KV0 := Vector(vals(curvature_z0(z))):  # vector of values of the curvature
   OV := Vector(vals(1));                 # vector of ones

   for c from 1 to 10 do
    u := SubVector(uv,1..mb);       # vector of coefficients of numerator
    v := SubVector(uv,mb..2*mb-1);
    v[1] := 1;                      # vector of coefficients of denominator

    for i from 1 to 5 do p[i] := PM[i] . u; od;
    for i from 2 to 6 do q[i] := QM[i] . v; od;
    r := RM . u;      # vector of values of numerator
    s := RM . v;      # vector of values of denominator
    si1 := 1 /~ s;    # vector of values of denominator^(-1)
    si2 := si1 ^~ 2;  # vector of values of denominator^(-2)
    si3 := si1 ^~ 3;  # vector of values of denominator^(-3)
    si4 := si1 ^~ 4;  # vector of values of denominator^(-4)

    FV := r *~ si1;   # vector of values of f = numerator/denominator
    e := map(t -> exp(-2*t),FV); # vector of values of exp(-2f)

    # Matrix of derivatives of values of f wrt coeffs of numerator
    JFU := DiagonalMatrix(si1) . RM;

    # Matrix of derivatives of values of f wrt coeffs of denominator
    JFV := DiagonalMatrix(- r *~ si2) . RM;
    
    # Matrix of derivatives of values of f wrt all coefficients
    JF := <JFU | SubMatrix(JFV,1..np,2..mb)>;

    # Vector of values of Delta(f)
    LV := (p[1] *~ si1) + 
	  ((p[2] *~ q[2] + p[3] *~ q[3] + p[4] *~ q[4]) *~ si2) + 
	  (p[5] *~ (q[2] *~ q[5] + q[3] *~ q[6]) *~ si3);

    # Matrix of derivatives of values of Delta(f) wrt coeffs of numerator of f
    JLU := DiagonalMatrix(si1) . PM[1] +
	   add(DiagonalMatrix(q[i] *~ si2) . PM[i],i=2..4) +
	   DiagonalMatrix((q[2] *~ q[5] + q[3] *~ q[6]) *~ si3) . PM[5];

    # Matrix of derivatives of values of Delta(f) wrt coeffs of denominator of f
    JLV := DiagonalMatrix((p[2] *~ si2) + (p[5] *~ q[5] *~ si3)) . QM[2] +
	   DiagonalMatrix((p[3] *~ si2) + (p[5] *~ q[6] *~ si3)) . QM[3] +
	   DiagonalMatrix(p[4] *~ si2) . QM[4] +
	   DiagonalMatrix(p[5] *~ q[2] *~ si3) . QM[5] +
	   DiagonalMatrix(p[5] *~ q[3] *~ si3) . QM[6] +
	   DiagonalMatrix((-p[1] *~ si2) + add(-2 *~ p[i] *~ q[i],i=2..4) *~ si3 + (-3*p[5]*~(q[2]*~q[5]+q[3]*~q[6])*~si4)) . RM;

    # Matrix of derivatives of values of Delta(f) wrt all coeffs of f
    JL := <JLU | SubMatrix(JLV,1..np,2..mb)>;

    # Vector of values of the rescaled curvature
    KV := (KV0 - LV) *~ e;

    # Matrix of derivatives of values of the rescaled curvature wrt coeffs of f
    JK := DiagonalMatrix(-e) . JL + DiagonalMatrix(-2*KV) . JF;

    fa := this["vector_to_pade",uv];
    FVa := Vector(vals(fa));
    LVa := Vector(vals(laplacian_z0(fa)));
    KVa := Vector(vals(this["rescaled_curvature",fa]));
    IM := IdentityMatrix(2*mb-1);
    eps := 10.^(-20);

    # Vector of weighted errors, whose 2-norm we want to minimise
    EV := WV *~ (OV + KV);

    # Matrix of derivatives of EV wrt coeffs of f
    JE := DiagonalMatrix(WV) . JK;
    
    duv := LeastSquares(JE,-EV);
    uv := uv + duv;
    rep := evalf([c,log[10](Norm(EV,2)^2),log[10](Norm(duv,2))]);
    rep := evalf[5](rep);
    print(rep);
   od:

   this["rescaling_error"] := Norm(EV,2);
   f0 := this["vector_to_pade",uv];
   this["set_rescale",f0];

   NULL;
  end
 ],

 ["Method","vector_to_poly","This converts a vector (or list) of coefficients to a polynomial in $z_1$ and $z_2$.",
  proc(this,v::{list,Vector})
   local vv,n,m,d,B,fn,fd;

   vv := v;
   if type(vv,Vector) then vv := convert(vv,list); fi;
   m := nops(vv);
   d := this["rescale_degree"];
   B := [seq(seq(z[1]^j*z[2]^(i-j),j=0..i),i=0..d)];
   return add(vv[i] * B[i],i = 1..m);
  end
 ],

 ["Method","poly_to_vector","This is inverse to the @vector_to_poly@ method.",
  proc(this,f)
   local d;
   
   d := this["rescale_degree"];
   return [seq(seq(coeff(coeff(f,z[1],j),z[2],i-j),j=0..i),i=0..d)];
  end
 ],

 ["Method","vector_to_pade","This converts a vector (or list) of coefficients to a rational function in $z_1$ and $z_2$.  The coefficients for the numerator are given first, followed by the coefficients for the denominator.  The constant term in the denominator is always taken to be 1, and this coefficient is omitted from the vector.",
  proc(this,v::{list,Vector})
   local vv,n,m,d,B,fn,fd;

   vv := v;
   if type(vv,Vector) then vv := convert(vv,list); fi;
   n := nops(vv);
   m := (n+1)/2;
   return 
    this["vector_to_poly",[seq(vv[i],i=1..m)]]/
    this["vector_to_poly",[1,seq(vv[m+i],i=1..m-1)]];
  end
 ],

 ["Method","pade_to_vector","This is inverse to the @vector_to_pade@ method.",
  proc(this,f)
   local fn,fd,d,vn,vd;

   d := this["rescale_degree"];
   fn := numer(f);
   fd := denom(f);

   vn := [seq(seq(coeff(coeff(fn,z[1],j),z[2],i-j),j=0..i),i=0..d)];
   vd := [seq(seq(coeff(coeff(fd,z[1],j),z[2],i-j),j=0..i),i=0..d)];
   vn := vn /~ vd[1];
   vd := vd /~ vd[1];
   return [op(vn),op(2..-1,vd)];
  end
 ],

 ["Method","find_a_H","This method comutes the lengths (with respect to the rescaled metric) of the sides of the fundamental domain $F_{16}$.  It then calculates estimates of $a_H$ from these lengths, and averages them.",
  proc(this)
   local i,k,cz,cm,cs,R,cL,a_H_avg,aa;

   cL := table();
   aa := table();

   for k in [0,1,3,5] do
    cz := evalf(z_proj0(c_E0[k](t)));
    cm := this["rescale_z"](cz);
    cs := nm4(map(diff,c_E0[k](t),t));
    R := F16_curve_limits[k];
    cL[k] := evalf(Int(cs * cm,t = R));
   od:

   this["curve_lengths"] := eval(cL);

   for i in [0,1,3,5] do
    aa[i] := fsolve(side_length_H[i] = cL[i],a_H);
   od:

   this["curve_a_H_estimates"] := eval(aa);
   this["a_H"] := (aa[0]+aa[1]+aa[3]+aa[5])/4;

   this["a_H_discrepancy"] := 
    max(seq(seq(abs(aa[i]-aa[j]),i in [0,1,3,5]),j in [0,1,3,5]));

   return this["a_H"];
  end
 ],

 ["Method","find_u","This calculates appropriate values for the @c_E_rescaled_length@ field and various related fields.  These involve various Fourier sin series; the parameter @d@ controls the number of terms in those series.",
  proc(this,d)
   local i,j,k,m,a,cz,cm,cs0,cs,P,AV,BM,CV,IL,FF,AA,BB;

   this["u_degree"] := d;

   this["c_E_rescaled_speed"] := table():
   this["c_E_rescaled_length"] := table():
   this["c_E_average_rescaled_speed"] := table():
   this["u"] := table();
   this["u_inv"] := table();

   for k in [0,1,3,5] do
    userinfo(7,genus2,sprintf("Analysing c[%d]",k)); 
    if k = 0 then
     m := 4;
    elif k <= 4 then
     m := 2;
    else 
     m := 1;
    fi;

    cz := expand(combine(simplify(z_proj0(c_E0[k](t))))):
    cm := combine(simplify(this["log_rescale_z"](cz))):
    cs0 := combine(factor(expand(combine(nm4(map(diff,c_E0[k](t),t)))))):
    cs := cs0 * exp(cm):

    userinfo(7,genus2,"Finding rescaled speed"); 

    # We need to calculate a number of integrals.  For some reason, Maple does these very
    # slowly, even though the integrands are quite tame.  It looks as though Maple may be
    # trying to do some auxiliary symbolic analysis, and making very heavy weather of it.
    # We therefore specify the integration method explicitly; this seems to fix the problem.

    a[0] := evalf(int(cs,t=0..Pi/m,numeric=true,digits=90,method=_CCquad)/(Pi/m));
    for j from 1 to d do
     a[j] := evalf(int(cs * cos(m*j*t),t=0..Pi/m,numeric=true,digits=90,method=_CCquad)*2/(Pi/m));
    od:

    this["c_E_rescaled_speed"][k] := 
     unapply(add(a[j] * cos(m*j*t),j=0..d),t);

    this["c_E_average_rescaled_speed"][k] := a[0];

    userinfo(7,genus2,"Finding rescaled length"); 
    this["c_E_rescaled_length"][k] := 
     unapply(int(this["c_E_rescaled_speed"][k](t),t),t);

    this["u_inv"][k] :=
     unapply(expand(this["c_E_rescaled_length"][k](t)/a[0]),t);

    userinfo(7,genus2,"Finding u"); 
    P := this["u_inv"][k](t) - t;
    for j from 1 to d do
     FF[j] := sin(m*j*(t+P));
     AA[j] := evalf(int(FF[j] * P,t=0..Pi/m,numeric=true,digits=90,method=_CCquad));
    od;

    for i from 1 to d do
     for j from i to d do 
      BB[i,j] := evalf(int(FF[i]*FF[j],t=0..Pi/m,numeric=true,digits=90,method=_CCquad));
      BB[j,i] := BB[i,j];
     od:
    od:

    AV := Vector([seq(AA[i],i=1..d)]):
    BM := Matrix([seq([seq(BB[i,j],j=1..d)],i=1..d)]):
    CV := (1/BM) . AV;

    IL := t - add(CV[i]*sin(m*i*t),i=1..d);

    this["u"][k] := unapply(IL,t);
   od;
  end
 ],

 ["Method","find_v_series","Let $u$ be the function such that $q(c_{Hk}(t))=c_{Ek}(u(t))$.  This method finds an approximate power series for the function $v(z) = u(u^{-1}(t_0) + 2\\arctanh(z)/s_k) - t_0$, where $s_k$ is the speed of $c_{Hk}$.  This can be calculated without knowing $u^{-1}(t_0)$, and does not rely on the Fourier series for $u$ calculated by the @find_u@ method.",
  proc(this,k,t0,d0)
   local t,a,d,cs,css,vs,err,sol,cus;

   cs := expand(evalf(convert(map(series,c_E0[k](t+t0),t=0,d0+1),polynom,t)));
   css := nm4(map(diff,cs,t)) * this["rescale_x"](cs);
   css := expand(evalf(convert(series(css,t=0,d0),polynom,t)));
   vs := 0;
   for d from 1 to d0 do 
    vs := vs + a * t^d;
    err := expand(convert(series(subs(t = vs,css) * diff(vs,t) - 2/(1-t^2),t=0,d),polynom,t));
    sol := solve({coeff(err,t,d-1)},{a});
    vs := subs(sol,vs);
    vs := subs(a = 0,vs); # should not usually be needed
   od:
   return unapply(vs,t);
  end
 ],

 ["Method","add_chart","This adds a chart object to the atlas, but the chart object contains no data at this stage.",
  proc(this)
   local C,n;
   
   n := this["num_charts"];
   C := `new/EH_chart`();
   C["vertex_index"] := NULL;
   C["curve_index"]  := NULL;
   C["grid_index"] := n;
   this["charts"][n] := eval(C);
   this["num_charts"] := n+1;
   return eval(C);
  end
 ],

 ["Method","add_vertex_chart","This adds a chart centred at $v_k$, with polynomial degree $d$, and calculates the coefficients and associated data.",
  proc(this,k::integer,d::posint)
   local C;
   
   C := eval(this["add_chart"]);
   C["vertex_set",k,d,this];
   return eval(C);
  end
 ],

 ["Method","add_curve_chart","This adds a chart centred at $c_k(t0)$, with polynomial degree $d$, and calculates the coefficients and associated data.",
  proc(this,k::integer,t0::RR0,d::posint)
   local C;
   
   C := eval(this["add_chart"]);
   C["curve_set",k,t0,d,this];
   return eval(C);
  end
 ],

 ["Method","add_square_chart","This adds a chart centred at $\\delta^{-1}(s0)$, with polynomial degree $d$, and calculates the coefficients and associated data.  Here $\\delta$ is the map @square_diffeo@.",
  proc(this,s0::RR0_2,d::posint)
   local C;
   
   C := eval(this["add_chart"]);
   C["square_set",s0,d,this];
   return eval(C);
  end
 ],

 ["Method","add_centre_chart","This adds a chart centred at $x0$, with polynomial degree $d$, and calculates the coefficients and associated data.",
  proc(this,x0::RR0_4,d::posint)
   local C;
   
   C := eval(this["add_chart"]);
   C["centre_set",x0,d,this];
   return eval(C);
  end
 ],

 ["Method","max_chart_error","",
  proc(this)
   local n,i,CC,err,worst_i,worst_err;
   
   err := 0;
   worst_err := 0;
   worst_i := NULL;
   
   n := this["num_charts"];
   CC := eval(this["charts"]);
   
   for i from 0 to n-1 do
    err := CC[i]["check"]["max"];
    if err > worst_err then
     worst_i := i;
     worst_err := err;
    fi;
   od;

   return [worst_i,worst_err];
  end
 ],
 
 ["Method","q_c","This evaluates $q(z)$ using the chart whose centre is as close as possible to $z$",
  proc(this,z)
   return this["q_c_aux",z][1];
  end
 ],

 ["Method","q_c_aux","",
  proc(this,z)
   local T0,T1,z0,d0,i0,CC,n,C,d,i,x0,x1;

   # Note: this assumes that a_H0 = this["a_H"], which we have generally 
   # not assumed elsewhere.
   T1,T0,z0 := op(retract_F16_H0_aux(z));

   d0 := infinity;
   i0 := NULL;
   CC := this["charts"];
   n := this["num_charts"];
   if n = 0 then error("No charts"); fi;

   for i from 0 to n-1 do
    C := CC[i];
    if C["beta"] <> NULL then
     d := evalf(m_hyp_c(z0,C["beta"]));
     if d < d0 then
      d0 := d;
      i0 := i;
     fi;
    fi;
   od;

   x0 := CC[i0]["q_c"](z0);
   x1 := act_R4[G_inv(T1)](x0);
   return [x1,T1,T0,z0,i0,x0];
  end
 ],

 ["Method","q","This is the same as @q_c@, except that the argument is in $\\mathbb{R}^2$ instead of $\\mathbb{C}$",
  proc(this,st)
   this["q_c",st[1] + I * st[2]];
  end
 ],

 ["Method","patching_error","This method looks for charts centred at a hyperbolic distance of at most @r0@ from @z0@. For each such chart, it calculates an approximation to @q(z0)@.  It returns a pair giving the number of different approximations, and the maximum discrepancy between them. ",
  proc(this,z0,r0)
   local i,j,n,m,d,CC,C,X;
   
   n := this["num_charts"];
   CC := eval(this["charts"]);
   X := NULL;
   for i from 0 to n-1 do
    C := eval(CC[i]);
    if d_hyp_c(C["beta"],z0) < r0 then
     X := X,C["q_c"](z0);
    fi;
   od;
   X := [X];
   m := nops(X);
   if m <= 1 then
    return [m,1];
   else
    d := max(seq(seq(d4f(X[i],X[j]),j=i+1..m),i=1..m-1));
    return [m,d];
   fi;
  end
 ],

 ["Method","max_patching_error","",
  proc(this,r)
   local z,u,d_worst,m_worst,z_worst,SP;
   
   SP := this["H_samples"];
   d_worst := 0;
   z_worst := NULL;
   m_worst := NULL;
   for z in SP do
    u := this["patching_error",z,r];
    if u[2] > d_worst then
     z_worst := z;
     m_worst,d_worst := op(u);
    fi;
   od:
   return [z_worst,m_worst,d_worst];
  end
 ],
 
 ["Method","make_H_samples",
  "Set the @H_samples@ field to be the set of points of the form $(j + kI)/N$ that lie in $HF_{16}$",
  proc(this,N::posint)
   this["H_samples"] := select(is_in_F16_H0,[seq(seq(evalf((j+k*I)/N),j=0..N),k=0..N)]):
   return this["H_samples"];
  end
 ],

 ["Method","make_D_samples",
  "Set the @D_samples@ field to be the set of points of the form $(j + kI)/N$ that lie in the first quadrant of the disc of radius $r$ centred at the origin.  Here $r$ should be big enough that the disc contains $HF_{16}$, but not much bigger than that.",
  proc(this,N::posint,r::RR0)
   this["D_samples"] := select(z -> abs(z) < r,[seq(seq(evalf((j+k*I)/N),j=0..N),k=0..N)]):
   return this["D_samples"];
  end
 ],

 ["Method","make_H_samples_q","Calculate the images of the H-sample points under the map $q\\colon\\Delta\\to EX^*$",
  proc(this)
   local z,X,i;
   X := NULL;
   # This is written in expanded form to make it easier to debug
   for i from 1 to nops(this["H_samples"]) do
    z := this["H_samples"][i];
    X := X,this["q_c",z];
   od;
   this["H_samples_q"] := [X];
  end
 ],
 
 ["Method","make_D_samples_q","Calculate the images of the D-sample points under the map $q\\colon\\Delta\\to EX^*$",
  proc(this)
   local z,X,i;
   X := NULL;
   # This is written in expanded form to make it easier to debug
   for i from 1 to nops(this["D_samples"]) do
    z := this["D_samples"][i];
    X := X,this["q_c",z];
   od;
   this["D_samples_q"] := [X];
  end
 ],

 ["Method","set_chart_dist","Calculate distances between H-sample points and centres of charts",
  proc(this)
   local dd,i,j,n,m,CH,SP;

   dd := table():
   CH := eval(this["charts"]):
   SP := this["H_samples"]:
   n  := this["num_charts"];
   m  := nops(SP);

   for i from 1 to m do
    for j from 0 to n-1 do
     dd[i,j] := evalf(d_hyp_c(SP[i],CH[j]["beta"])):
    od:
   od:

   this["chart_dist"] := eval(dd);
  end
 ],
 
 ["Method","set_q_approx_pade","Find an approximation to $q(x+iy)$ by rational functions in $x$ and $y$.  The approximation will be equivariant with respect to $\\langle\\lambda,\\nu\\rangle$, and will minimize the error for points $x+iy$ in @D_samples@.",
  proc(this,dd)
   local n,m,d1,d3,d4,DS,ES,f0,F0,M1,M2,v,w,qa,ff;
   
   DS := map(C_to_R2,this["D_samples"]):
   ES := this["D_samples_q"]:
   m := nops(DS);

   if type(dd,[posint,posint,posint]) then
    d1,d3,d4 := op(dd);
   elif type(dd,posint) then
    d1 := dd;
    d3 := dd;
    d4 := dd+3;
   else
    error("invalid degree specification");
   fi;
   
   # The first component of q is s[1] times a function of s[1]^2 and s[2]^2
   f0 := (s) -> [seq(seq(s[1]^(2*i)*s[2]^(2*j),j=0..d1-i),i=0..d1)]:
   n := nops(f0([0,0]));
   M1 := Matrix([seq(DS[i][1] *~ f0(DS[i]),i=1..m)]):
   M2 := Matrix([seq(ES[i][1] *~ f0(DS[i]),i=1..m)]):
   v,w := op(quot_approx(M1,M2));
   F0 := f0(s);
   qa[1] := add(w[i]*F0[i]*s[1],i=1..n)/add(v[i]*F0[i],i=1..n);

   # The second component of q can be deduced from the first
   qa[2] := subs({s[1]=s[2],s[2]=-s[1]},qa[1]);

   # The third component of q is a symmetric function of s[1]^2 and s[2]^2
   f0 := (s) -> [seq(seq((s[1]*s[2])^(2*i)*(s[1]^(2*j)+s[2]^(2*j)),j=0..d3-2*i),i=0..floor(d3/2))]:
   n := nops(f0([0,0]));
   M1 := Matrix([seq(f0(DS[i]),i=1..m)]):
   M2 := Matrix([seq(ES[i][3] *~ f0(DS[i]),i=1..m)]):
   v,w := op(quot_approx(M1,M2));
   F0 := f0(s);
   qa[3] := expand(add(w[i]*F0[i],i=1..n))/expand(add(v[i]*F0[i],i=1..n));

   # The fourth component of q is (s[1]^2-s[2]^2) times a symmetric function of s[1]^2 and s[2]^2
   f0 := (s) -> [seq(seq((s[1]*s[2])^(2*i)*(s[1]^(2*j)+s[2]^(2*j)),j=0..d4-2*i),i=0..floor(d4/2))]:
   n := nops(f0([0,0]));
   M1 := Matrix([seq((DS[i][1]^2-DS[i][2]^2)*f0(DS[i]),i=1..m)]):
   M2 := Matrix([seq(ES[i][4] *~ f0(DS[i]),i=1..m)]):
   v,w := op(quot_approx(M1,M2));
   F0 := f0(s);
   qa[4] := expand(add(w[i]*F0[i]*(s[1]^2-s[2]^2),i=1..n))/expand(add(v[i]*F0[i],i=1..n));

   this["q_approx"] := unapply([qa[1],qa[2],qa[3],qa[4]],s);
   NULL;
  end
 ],
 
 ["Method","set_q_approx_poly","Find an approximation to $q(x+iy)$ by polynomials in $x$ and $y$.  The approximation will be equivariant with respect to $\\langle\\lambda,\\nu\\rangle$, and will minimize the error for points $x+iy$ in @D_samples@.",
  proc(this,dd)
   local n,m,d1,d3,d4,DS,ES,f0,F0,M,Q,R,v,w,qa;
   
   DS := map(C_to_R2,this["D_samples"]):
   ES := this["D_samples_q"]:
   m := nops(DS);

   if type(dd,[posint,posint,posint]) then
    d1,d3,d4 := op(dd);
   elif type(dd,posint) then
    d1 := dd;
    d3 := dd;
    d4 := dd+3;
   else
    error("invalid degree specification");
   fi;
   
   # The first component of q is s[1] times a function of s[1]^2 and s[2]^2
   f0 := (s) -> [seq(seq(s[1]^(2*i+1)*s[2]^(2*j),j=0..d1-i),i=0..d1)]:
   n := nops(f0([0,0]));
   M := Matrix(map(f0,DS));
   Q,R := QRDecomposition(M);
   v := Vector(map(u -> u[1],ES));
   w := (1/R) . Transpose(Q) . v;
   F0 := f0(s);
   qa[1] := add(w[i]*F0[i],i=1..n);

   # The second component of q can be deduced from the first
   qa[2] := subs({s[1]=s[2],s[2]=-s[1]},qa[1]);

   # The third component of q is a symmetric function of s[1]^2 and s[2]^2
   f0 := (s) -> [seq(seq((s[1]*s[2])^(2*i)*(s[1]^(2*j)+s[2]^(2*j)),
                         j=0..d3-2*i),i=0..floor(d3/2))]:
   n := nops(f0([0,0]));
   M := Matrix(map(f0,DS));
   Q,R := QRDecomposition(M);
   v := Vector(map(u -> u[3],ES));
   w := (1/R) . Transpose(Q) . v;
   F0 := f0(s);
   qa[3] := add(w[i]*F0[i],i=1..n);

   # The fourth component of q is (s[1]^2-s[2]^2) times a symmetric
   # function of s[1]^2 and s[2]^2
   f0 := (s) -> [seq(seq((s[1]^2-s[2]^2)*(s[1]*s[2])^(2*i)*(s[1]^(2*j)+s[2]^(2*j)),
                         j=0..d4-2*i),i=0..floor(d4/2))]:
   n := nops(f0([0,0]));
   M := Matrix(map(f0,DS));
   Q,R := QRDecomposition(M);
   v := Vector(map(u -> u[4],ES));
   w := (1/R) . Transpose(Q) . v;
   F0 := f0(s);
   qa[4] := add(w[i]*F0[i],i=1..n);

   this["q_approx"] := unapply([qa[1],qa[2],qa[3],qa[4]],s);
   NULL;
  end
 ],
 
 ["Method","set_q_approx_fourier","This sets the fields @fourier_r_max@, @fourier_m@, @fourier_k@, @fourier_r@, @fourier_v@, @fourier_a@, @fourier_qr@ and @fourier_a_spline@.  See the documentation of those fields for more details.",
  proc(this,r_max,m,k)
   local R,V,A,i,j,l,r,t,z,v,w,s;

   this["fourier_r_max"] := evalf(r_max);
   this["fourier_m"]     := m;
   this["fourier_k"]     := k;
   R := evalf([seq(r_max * sin(i/m*Pi/2),i = 1.. m)]);
   V := table();
   A := table();
   this["fourier_r"] := R;
   this["fourier_v"] := eval(V);
   this["fourier_a"] := eval(A);
   for r in R do
    userinfo(5,genus2,sprintf("Evaluating q with r=%A\n",r));
    for i from 0 to 2^k - 1 do
     t := i/2^k;
     z := evalf(r * exp(t * 2*Pi*I));
     V[r,t] := this["q_c",z];
    od;
   od;

   this["fourier_qr"] := table():

   for r in R do
    userinfo(5,genus2,sprintf("Finding Fourier transform with r=%A\n",r));

    for l in [1,3,4] do
     v := Vector([seq(V[r,i/2^k][l],i=0..2^k-1)]);
     w := map(Re,convert(SignalProcessing[FFT](v,normalization=full),list));
     if l = 1 then
      for j from 0 to 2^(k-2)-1 do
       A[1,j,r] := 2*w[(2*j+1) + 1]; 
       A[2,j,r] := (-1)^j * A[1,j,r];
      od;
     elif l = 3 then
      for j from 0 to 2^(k-3)-1 do
       A[3,j,r] := 2*w[(4*j)+1];
      od;
     elif l = 4 then
      for j from 0 to 2^(k-3)-1 do
       A[4,j,r] := 2*w[(4*j+2)+1];
      od;
     fi;
    od;

    this["fourier_qr"][1,r] := 
     unapply(add(A[1,i,r]*cos((2*i+1)*s),i=0..2^(k-2)-1),s):

    this["fourier_qr"][2,r] := 
     unapply(add(A[2,i,r]*sin((2*i+1)*s),i=0..2^(k-2)-1),s):

    this["fourier_qr"][3,r] := 
     unapply(A[3,0,r]/2 + 
             add(A[3,i,r]*cos((4*i)*s),i=1..2^(k-3)-1),s):

    this["fourier_qr"][4,r] := 
     unapply(add(A[4,i,r]*cos((4*i+2)*s),i=0..2^(k-3)-1),s):
   od;

   this["set_fourier_a_spline"];
   
   NULL;
  end
 ],

 ["Method","set_fourier_a_spline","",
  proc(this)
   local i,k,A,R,S;
   
   this["fourier_a_spline"] := table():

   k := this["fourier_k"];
   A := eval(this["fourier_a"]);
   R := eval(this["fourier_r"]);
   S := table():
   
   for i from 0 to 2^(k-2)-1 do
    S[1,i] := unapply(CurveFitting[Spline]([seq([r,A[1,i,r]],r in R)],t),t);
    S[2,i] := unapply((-1)^i*S[1,i](t),t);
   od;

   for i from 0 to 2^(k-3)-1 do
    S[3,i] := unapply(CurveFitting[Spline]([seq([r,A[3,i,r]],r in R)],t),t);
    S[4,i] := unapply(CurveFitting[Spline]([seq([r,A[4,i,r]],r in R)],t),t);
   od:

   this["fourier_a_spline"] := eval(S):
   NULL;
  end
 ],

 ["Method","q_fourier","",
  proc(this,s::RR0_2)
   local r,theta,S,k,x;
   r := evalf(sqrt(s[1]^2+s[2]^2));
   theta := arctan(s[2],s[1]);
   S := eval(this["fourier_a_spline"]);
   k := this["fourier_k"];
   x := table();

   x[1] := add(S[1,i](r)*cos((2*i+1)*theta),i = 0..2^(k-2)-1); 
   x[2] := add(S[2,i](r)*sin((2*i+1)*theta),i = 0..2^(k-2)-1); 
   x[3] := S[3,0](r)/2 +
           add(S[3,i](r)*cos((4*i  )*theta),i = 1..2^(k-3)-1); 
   x[4] := add(S[4,i](r)*cos((4*i+2)*theta),i = 0..2^(k-3)-1);
   return [x[1],x[2],x[3],x[4]];
  end
 ],
 
 ["Method","q_fourier_c","",
  proc(this,z::CC0)
   this["q_fourier",C_to_R2(z)];
  end
 ],
 
 ["Method","set_square_q_inv_a","",
  proc(this,d::posint)
   local SP,pSP,rSP,ppr,ppi,err,sol,a,b,s;
   
   SP := this["H_samples"];
   if not(type(this["H_samples_q"],list) and
          nops(this["H_samples_q"]) = nops(this["H_samples"])) then
    this["make_H_samples_q"];
   fi;
   pSP := this["H_samples_q"];
   rSP := evalf(map(square_diffeo_E0,pSP));

   ppr := unapply(add(add(a[i,j]*s[1]^i*s[2]^j,j=0..d-i),i=0..d),s):
   err := add((ppr(rSP[i]) - Re(SP[i]))^2,i=1..nops(SP)):
   sol := solve({seq(diff(err,v),v in indets(err))}):
   ppr := unapply(subs(sol,ppr(s)),s):

   ppi := unapply(add(add(b[i,j]*s[1]^i*s[2]^j,j=0..d-i),i=0..d),s):
   err := add((ppi(rSP[i]) - Im(SP[i]))^2,i=1..nops(SP)):
   sol := solve({seq(diff(err,v),v in indets(err))}):
   ppi := unapply(subs(sol,ppi(s)),s):

   this["square_q_inv"] := unapply(ppr(s) + I * ppi(s),s):
  end
 ],

 ["Method","square_q_inv_search","",
  proc(this,s0::RR0_2)
   local x0,x1,x2,x3,s1,u0,v0,w0,w1,z0,t0,t1,i0,CH,T0,T1;
   x0 := square_diffeo_E0_inverse_search(s0);
   u0,v0 := op(tangent_frame_a(x0));
   z0 := this["square_q_inv"](s0);
   x1,T1,T0,w0,i0,x2 := op(this["q_c_aux",z0]);
   CH := eval(this["charts"][i0]):
   t0 := C_to_R2(CH["m_inv_c"](w0));
   t1 := subs(fsolve([dp4(u0,CH["p"](t)),dp4(v0,CH["p"](t))],{t[1]=t0[1],t[2]=t0[2]}),[t[1],t[2]]);
   w1 := R2_to_C(CH["m"](t1));
   return w1;
  end
 ],
 
 ["Method","set_square_q_inv_b","",
  proc(this,n::posint,d::posint)
   local i,j,CP,SP,SQ,rSQ,iSQ,a,ppr,ppi,err,sol;

   CP := table():
   for i from 1 to n do
    CP[i] := evalf(1/2 + cos((2*n-2*i+1)*Pi/(2*n))/2):
   od:
   SP := [seq(seq([CP[i],CP[j]],j=1..n),i=1..n)];
   SQ := map(s -> this["square_q_inv_search",s],SP);
   rSQ := map(Re,SQ);
   iSQ := map(Im,SQ);

   ppr := unapply(add(add(a[i,j]*s[1]^i*s[2]^j,j=0..d-i),i=0..d),s):
   err := add((ppr(SP[i]) - rSQ[i])^2,i=1..nops(SP)):
   sol := solve({seq(diff(err,v),v in indets(err))}):
   ppr := unapply(subs(sol,ppr(s)),s):

   ppi := unapply(add(add(b[i,j]*s[1]^i*s[2]^j,j=0..d-i),i=0..d),s):
   err := add((ppi(SP[i]) - iSQ[i])^2,i=1..nops(SP)):
   sol := solve({seq(diff(err,v),v in indets(err))}):
   ppi := unapply(subs(sol,ppi(s)),s):

   this["square_q_inv"] := unapply(ppr(s) + I * ppi(s),s):
  end
 ],

 ["Method","add_edge","",
  proc(this,i,j)
   local n,m,E,Ci,Cj,ci,cj,vi,vj;
   n := this["num_charts"];
   m := this["num_edges"];

   if i < 0 or i >= n or j < 0 or j >= n then
    error("Chart indices out of bounds");
   fi;

   if i = j then 
    error("Start and end indices are the same");
   fi;

   E := `new/EH_atlas_edge`();
   E["curve_index"] := NULL;
   E["grid_index"] := m;
   E["start_index"] := i;
   E["end_index"]   := j;
   this["edges"][m] := eval(E);
   this["num_edges"] := m+1;

   Ci := eval(this["charts"][i]):
   Cj := eval(this["charts"][j]):
   ci := Ci["curve_index"];
   cj := Cj["curve_index"];
   vi := Ci["vertex_index"];
   vj := Cj["vertex_index"];

   if   ([ci] = [0] or [vi] = [3] or [vi] = [6] ) and
        ([cj] = [0] or [vj] = [3] or [vj] = [6] ) then
    E["curve_index"] := 0;
   elif ([ci] = [1] or [vi] = [0] or [vi] = [6] ) and
        ([cj] = [1] or [vj] = [0] or [vj] = [6] ) then
    E["curve_index"] := 1;
   elif ([ci] = [3] or [vi] = [3] or [vi] = [11]) and
        ([cj] = [3] or [vj] = [3] or [vj] = [11]) then
    E["curve_index"] := 3;
   elif ([ci] = [5] or [vi] = [0] or [vi] = [11]) and
        ([cj] = [5] or [vj] = [0] or [vj] = [11]) then
    E["curve_index"] := 5;
   fi;

   return m;    
  end
 ],

 ["Method","set_edge_lengths","Set the @H_length@ field of each edge, by assuming that the charts are exactly isometric.",
  proc(this)
   local i,j,k,m,Ci,Cj,xi,xj,zi,zj,E;

   m := this["num_edges"];

   for k from 0 to m-1 do
    E := eval(this["edges"][k]);
    i := E["start_index"];
    j := E["end_index"];
    Ci := eval(this["charts"][i]);
    Cj := eval(this["charts"][j]);
    xi := Ci["centre"];
    xj := Cj["centre"];
    zi := Cj["p_inv_c",xi];
    zj := Ci["p_inv_c",xj];
    E["start_z"] := zi;
    E["end_z"]   := zj;
    E["EH_length"] := arctanh(abs(zi)) + arctanh(abs(zj));
    if Ci["beta"] <> NULL and Cj["beta"] <> NULL then
     E["H_length"] := d_hyp_c(Ci["beta"],Cj["beta"]);
    fi;
   od;
   NULL;
  end
 ],

 ["Method","set_beta_approx","This method sets the @beta@ field for each chart that is not centred on an edge of $F_{16}$.  If $q$ is the standard map $\\Delta\\to EX^*$, then $q(\\beta)$ should be the centre of the chart.  If we have already set the @square_q_inv@ field (to an approximation of @(square_diffeo_E o q)^(-1)@) then we use that.  Otherwise, we use @square_diffeo_H^(-1) o square_diffeo_E@.  If the $\\lambda$ field has already been set, then we also put $\\alpha = -\\beta/\\lambda$.",
  proc(this)
   local i,n,sqi,C;

   n := this["num_charts"];
   sqi := this["square_q_inv"];
   if sqi = NULL then
    sqi := eval(square_diffeo_H0_inverse);
   fi;
   for i from 0 to n-1 do
    C := eval(this["charts"][i]);
    if C["curve_index"] = NULL then
     C["beta"] := sqi(evalf(C["square_centre"]));
     if C["lambda"] <> NULL then
      C["alpha"] := - C["beta"]/C["lambda"];
     fi;
    fi;
   od;
  end
 ],

 ["Method","optimize_lambda","This method assumes that we have a value of $\\beta$ for each chart, and then chooses the best possible values of $\\lambda$ consistent with the values of $\\beta$.  It then sets $\\alpha = -\\beta/\\lambda$.  It does not alter the values of $\\alpha$, $\\beta$ or $\\lambda$ for charts centred on the edges of $F_{16}$, because in those cases we have other methods that are considered to be more reliable.",
  proc(this)
   local i,j,k,n,m,CC,EE,C,E,link,ep,lm,L;

   n := this["num_charts"];
   m := this["num_edges"];
   CC := eval(this["charts"]);
   EE := eval(this["edges"]);

   link := table();
   for i from 0 to n-1 do link[i] := {}; od;

   for i from 0 to m do
    j := EE[i]["start_index"];
    k := EE[i]["end_index"];
    link[j] := {op(link[j]),i};
    link[k] := {op(link[k]),i};
   od;

   for i from 0 to n-1 do
    C := eval(CC[i]);
    L := NULL;
    for k in link[i] do
     E := EE[k];
     if i = E["start_index"] then
      j  := E["end_index"];
      ep := E["end_z"];
     else
      j  := E["start_index"];
      ep := E["start_z"];
     fi;
     lm := (CC[j]["beta"]-C["beta"])/(1-conjugate(C["beta"])*CC[j]["beta"])/ep;
     lm := lm/abs(lm);
     L := L,lm;
    od;
    lm := `+`(L)/nops([L]);
    lm := lm/abs(lm);
    C["lambda"] := lm;
    C["alpha"] := -C["beta"]/C["lambda"];
   od;
   NULL;
  end
 ],

 ["Method","optimize_beta","This method adjusts the $\\beta$ fields for all charts that are not centred on an edge of $F_{16}$.  It attempts to ensure that whenever any two charts are joined by an edge, the hyperbolic distance in $\\Delta$ between the coresponding $\\beta$ values is the same as the distance in $EX^*$ between the corresponding centres, measured using the rescaled metric.  It then calls the @optimize_lambda@ method to set $\\lambda$ and $\\alpha$.",
  proc(this)
   local i,j,k,n,m,C,E,CC,EE,Z,obj,objjac,mh,mh1,mh2,mh3,mh4,
    movable_chart_indices,movable_chart_beta,chart_lookup,
    pinned_edge_indices,pinned_edge_ends,pinned_edge_lengths,
    movable_edge_indices,movable_edge_ends,movable_edge_lengths,
    num_vars,num_objs_a,num_objs_b,num_objs,
    Z1,E1,N1,J1,Q1,R1,dZ;
   global obj_count;
   
   n := this["num_charts"];
   m := this["num_edges"];

   movable_chart_indices := NULL;
   movable_chart_beta    := NULL;
  
   pinned_edge_indices  := NULL;
   pinned_edge_ends     := NULL;
   pinned_edge_lengths  := NULL;

   movable_edge_indices  := NULL;
   movable_edge_ends     := NULL;
   movable_edge_lengths  := NULL;

   chart_lookup := table();

   CC := eval(this["charts"]);
   EE := eval(this["edges"]);

   j := 1;

   for i from 0 to n-1 do
    if CC[i]["curve_index"] = NULL then
     chart_lookup[i] := j;
     j := j+1;
     movable_chart_indices := movable_chart_indices,i;
     movable_chart_beta    := movable_chart_beta,   CC[i]["beta"];
    fi;
   od;
   movable_chart_indices := [movable_chart_indices];
   movable_chart_beta    := [movable_chart_beta];

   for i from 0 to m-1 do
    E := eval(EE[i]);
    j := E["start_index"];
    k := E["end_index"];
    if member(j,movable_chart_indices) then
     if member(k,movable_chart_indices) then
      movable_edge_indices := movable_edge_indices,i;
      movable_edge_ends :=
       movable_edge_ends, [chart_lookup[j],chart_lookup[k]];
      movable_edge_lengths := movable_edge_lengths,tanh(E["EH_length"]/2)^2;
     else
      pinned_edge_indices := pinned_edge_indices,i;
      pinned_edge_ends    := pinned_edge_ends,[C_to_R2(CC[k]["beta"]),chart_lookup[j]];
      pinned_edge_lengths := pinned_edge_lengths,tanh(E["EH_length"]/2)^2;
     fi;
    else
     if member(k,movable_chart_indices) then
      pinned_edge_indices := pinned_edge_indices,i;
      pinned_edge_ends    := pinned_edge_ends,[C_to_R2(CC[j]["beta"]),chart_lookup[k]];
      pinned_edge_lengths := pinned_edge_lengths,tanh(E["EH_length"]/2)^2;
     fi;
    fi;
   od;
   movable_edge_indices := [movable_edge_indices];
   movable_edge_ends    := [movable_edge_ends];
   movable_edge_lengths := [movable_edge_lengths];
   pinned_edge_indices  := [pinned_edge_indices];
   pinned_edge_ends     := [pinned_edge_ends];
   pinned_edge_lengths  := [pinned_edge_lengths];

   num_vars := 2*nops(movable_chart_indices);
   num_objs_a := nops(pinned_edge_indices);
   num_objs_b := nops(movable_edge_indices);
   num_objs := num_objs_a + num_objs_b;

   # mh(s,t) determines d_hyp(s,t), but mh is just a rational function,
   # whereas d_hyp involves sqrt and arctanh.  This makes it more tractable
   # to work with mh instead.

   mh  := unapply(simplify(m_hyp(s,t)^2),s,t);

   mh1 := unapply(simplify(diff(mh(s,t),s[1])),s,t);
   mh2 := unapply(simplify(diff(mh(s,t),s[2])),s,t);
   mh3 := unapply(simplify(diff(mh(s,t),t[1])),s,t);
   mh4 := unapply(simplify(diff(mh(s,t),t[2])),s,t);

   obj := proc(coords)
    local i,j,k,t0,t1,errs;
    global last_coords,last_err;

    errs := Vector(num_objs);
    
    for i from 1 to num_objs_a do
     t0 := pinned_edge_ends[i][1];
     j  := pinned_edge_ends[i][2];
     t1 := [coords[2*j-1],coords[2*j]];
     errs[i] := mh(t0,t1) - pinned_edge_lengths[i];
    od;

    for i from 1 to num_objs_b do
     j  := movable_edge_ends[i][1];
     k  := movable_edge_ends[i][2];
     t0 := [coords[2*j-1],coords[2*j]];
     t1 := [coords[2*k-1],coords[2*k]];
     errs[num_objs_a+i] := mh(t0,t1) - movable_edge_lengths[i];
    od;

    obj_count := obj_count+1;
    last_coords := convert(coords,list):
    last_err := convert(err,list):
    userinfo(5,genus2,sprintf("Step %d: err=%A",obj_count,Norm(errs)));
    return errs;
   end;

   objjac := proc(coords)
    local i,j,k,t0,t1,err_diff;

    err_diff := Matrix(num_objs,num_vars);

    for i from 1 to num_objs_a do
     t0 := pinned_edge_ends[i][1];
     j  := pinned_edge_ends[i][2];
     t1 := [coords[2*j-1],coords[2*j]];
     err_diff[i,2*j-1] := mh3(t0,t1);
     err_diff[i,2*j  ] := mh4(t0,t1);
    od;

    for i from 1 to num_objs_b do
     j  := movable_edge_ends[i][1];
     k  := movable_edge_ends[i][2];
     t0 := [coords[2*j-1],coords[2*j]];
     t1 := [coords[2*k-1],coords[2*k]];
     err_diff[num_objs_a+i,2*j-1] := mh1(t0,t1);
     err_diff[num_objs_a+i,2*j  ] := mh2(t0,t1);
     err_diff[num_objs_a+i,2*k-1] := mh3(t0,t1);
     err_diff[num_objs_a+i,2*k  ] := mh4(t0,t1);
    od;

    return err_diff;
   end;

   obj_count := 0;

   Z1 := Vector(map(C_to_R2,movable_chart_beta));
   E1 := obj(Z1);
   N1 := Norm(E1);
   
   for i from 1 to 5 do
    J1 := objjac(Z1);
    Q1,R1 := QRDecomposition(J1);
    dZ := LinearSolve(R1,-Transpose(Q1).E1);
    Z1 := Z1 + dZ;
    E1 := obj(Z1);
    N1 := Norm(E1);
   od:

   # Z1 := LSSolve([num_vars,num_objs],obj,objectivejacobian=objjac,initialpoint=Z1)[2];
   
   for i from 1 to num_vars/2 do
    j := movable_chart_indices[i];
    CC[j]["beta"] := Z1[2*i-1] + I * Z1[2*i];
   od;

   this["set_edge_lengths"];
   this["optimize_lambda"];
   NULL;
  end
 ],

 ["Method","curvature_error_plot","Generate a plot showing the difference between $-1$ and the curvature of the rescaled metric.",
  proc(this)
   z_plot(this["curvature_z"](z)+1);
  end
 ],

 ["Method","log_rescale_plot","Generate a plot of the @log_rescale_z@ field.",
  proc(this)
   z_plot(this["log_rescale_z"](z));
  end
 ],

 ["Method","beta_plot","Generate a plot showing the $\\beta$ values of all the charts, and the edges between them.",
  proc(this)
   local i,m,P,CC,EE,E;
   
   P := NULL;
   m := this["num_edges"];
   CC := eval(this["charts"]);
   EE := eval(this["edges"]);
   
   for i from 0 to m-1 do
    E := eval(EE[i]);
    P := P,line(C_to_R2(CC[E["start_index"]]["beta"]),
                C_to_R2(CC[E["end_index"]]["beta"]),
		colour=E["colour"]);
   od;
   display(P,axes=none,scaling=constrained);
  end
 ],

 ["Method","beta_plot_tikz","Generate a plot showing the $\\beta$ values of all the charts, and the edges between them.",
  proc(this)
   local i,m,s,CC,EE,E;
   
   m := this["num_edges"];
   CC := eval(this["charts"]);
   EE := eval(this["edges"]);

   s := cat(
    "\\begin{center}\n",
    " \\begin{tikzpicture}[scale=12]\n"
   );
    
   for i from 0 to m-1 do
    E := eval(EE[i]);
    s := cat(s,sprintf("  \\draw[maple%A] (%1.3f,%1.3f) -- (%1.3f,%1.3f);\n",
                       E["colour"],
		       op(C_to_R2(CC[E["start_index"]]["beta"])),
		       op(C_to_R2(CC[E["end_index"]]["beta"]))
		       ));
   od;

   s := cat(s,
    " \\end{tikzpicture}\n",
    "\\end{center}\n"
   );

   return s;
  end
 ],

 ["Method","H_sample_plot","Generate a plot showing the H-sample points.",
  proc(this)
   local P,k,cH;
   
   P := NULL;
   for k in [0,1,3,5] do
    cH := evalf(subs(a_H=this["a_H"],c_H[k](t)));
    P := P,cplot(cH,t=F16_curve_limits[k],colour=c_colour[k]);
   od;

   P := P,op(map(cpoint,this["H_samples"],colour=grey));

   display(P,scaling=constrained,axes=none);
  end
 ],

 ["Method","D_sample_plot","Generate a plot showing the D-sample points.",
  proc(this)
   local P,k,cH;
   display(
    circle(1),
    line([-1,-1] /~ sqrt(2.),[ 1, 1] /~ sqrt(2.),colour=c_colour[1]),
    line([-1, 1] /~ sqrt(2.),[ 1,-1] /~ sqrt(2.),colour=c_colour[2]),
    line([-1, 0],[ 1, 0],colour=c_colour[5]),
    line([ 0,-1],[ 0, 1],colour=c_colour[6]),
    seq(seq(xi_circle(I^k*c_H_p0[j],colour=c_colour[j]),k=0..3),j in [0,3,7,8]),
    op(map(cpoint,this["D_samples"],colour=grey)),
    scaling=constrained,axes=none
   );
  end
 ],

 ["Method","H_sample_q_plot","Generate a plot showing the images in $EX^*$ of the H-sample points.",
  proc(this)
   local k,t,x;
   
   display(
    seq(spacecurve(stereo(c_E0[k](t)),t=F16_curve_limits[k],colour=c_colour[k]),k in [0,1,3,5]),
    map(x -> point(stereo(x),colour=grey),this["H_samples_q"]),
    axes=none,scaling=constrained
   );
  end
 ],

 ["Method","D_sample_q_plot","Generate a plot showing the images in $EX^*$ of the D-sample points.",
  proc(this)
   local k,t,x;

   load_plot("EX_wireframe"):

   display(
    pics["EX_wireframe"],
    seq(spacecurve(stereo(c_E0[k](t)),t=0..2*Pi,colour=c_colour[k]),k=0..8),
    map(x -> point(stereo(x),colour=black),this["D_samples_q"]),
    axes=none,scaling=constrained
   );
  end
 ],

 ["Method","square_q_inv_plot","This method shows a distorted grid obtained by applying the @square_q_inv@ map to a regular grid covering the unit square.  Thus, the distorted grid will cover the domain $HF_{16}$ in the unit disc.",
  proc(this)
   local sqi;
   sqi := eval(this["square_q_inv"]);
   display(
    seq(cplot(sqi([0.1*i,t]),t=0..1,colour=blue),i=0..10),
    seq(cplot(sqi([t,0.1*i]),t=0..1,colour=red),i=0..10),
    scaling=constrained
   );
  end
 ],

 ["Method","edge_length_scatter_plot","We usually want to arrange our charts and edges so that most edges have approximately the same length.  Also, if everything was working perfectly, then the edge lengths in the @H_length@ and @EH_length@ fields would be the same. Thus, the plot generated by this method should show a cluster of points that are very close to the diagonal, and mostly close to each other.",
  proc(this)
   local i,m,PP,EE,E;
   
   PP := NULL:
   m := this["num_edges"];
   EE := eval(this["edges"]):
   for i from 0 to m-1 do
    E := eval(EE[i]);
    PP := PP,point([E["EH_length"],E["H_length"]]);
   od:
   display(PP);
  end
 ],

 ["Method","edge_length_anomaly_plot","If everything was working perfectly, then the edge lengths in the @H_length@ and @EH_length@ fields would be the same.  This method generates a plot showing a normalised measure of the difference between them.",
  proc(this)
   local i,m,LA,EE,E;
   
   LA := NULL:
   m := this["num_edges"];
   EE := eval(this["edges"]):
   for i from 0 to m-1 do
    E := eval(EE[i]);
    LA := LA,2*(E["EH_length"]-E["H_length"])/(E["EH_length"]+E["H_length"]);
   od:
   LA := sort([LA]):
   listplot(LA);
  end
 ],

 ["Method","nearest_charts_plot","This generates a plot which helps one to understand how well the fundamental domain $HF_{16}$ is covered by the charts in this atlas.  For each point in @H_samples@ we find the distance to the closest chart centre, then we sort these distances in order, and the result is the red curve.  The green curve shows distances to the second closest chart centre, and so on, up to the cyan curve, which shows distances to the fifth closest chart centre.",
  proc(this)
   local i,j,n,m,dd,dl,cols;

   n := this["num_charts"];
   m := nops(this["H_samples"]);
   dd := eval(this["chart_dist"]);
   if (dd = NULL) then
    this["set_chart_dist"];
    dd := eval(this["chart_dist"]);
   fi;
   dl := table();
   for i from 1 to m do
    dl[i] := sort([seq(dd[i,j],j=0..n-1)]);
   od:

   cols := [red,green,blue,magenta,cyan];

   display(
    seq(listplot(sort([seq(dl[i][j],i=1..m)]),colour=cols[j]),j=1..5),
    seq(line([0,0.02*i],[m,0.02*i],colour=grey),i=1..7)
   );
  end
 ],

 ["Method","fourier_v_plot","This generates a surface plot of the @m@'th component of the map $q\\colon\\Delta\\to EX^*\\subset\\mathbb{R}^4$",
  proc(this,m)
   local i,j,k,T,Z,R,V,P,r;

   k := this["fourier_k"];
   T := [seq(i/2^k,i=0..2^k-1)];
   Z := map(t -> evalf(exp(2*Pi*I*t)),T);
   R := this["fourier_r"];
   V := eval(this["fourier_v"]);
   P := table();
   for i from 1 to nops(R) do
    r := R[i];
    for j from 0 to 2^k-1 do
     P[i,j] := [r*Re(Z[j+1]),r*Im(Z[j+1]),V[R[i],j/2^k][m]];
    od;
    P[i,2^k] := P[i,0];
   od;;

   return display(
    seq(seq(polygon([P[i,j],P[i+1,j],P[i+1,j+1]]),j=0..2^k-1),i=1..nops(R)-1),
    seq(seq(polygon([P[i,j],P[i,j+1],P[i+1,j+1]]),j=0..2^k-1),i=1..nops(R)-1),
    scaling=constrained,axes=none,style=patchnogrid
   );
  end
 ],

 ["Method","fourier_ring_vals","This method generates a list of points in $EX^*\\subset\\mathbb{R}^4$ obtained by applying the map $q\\colon\\Delta\\to EX^*$ to $n$ equally spaced points on the circle of radius $r$ centred at $0$.",
  proc(this,r::RR0,n::posint := 1024)
   local i,j,k,a,S,x;

   a := table();
   k := this["fourier_k"];
   S := eval(this["fourier_a_spline"]);
   x := table();
   x[1] := add(S[1,i](r)*cos((2*i+1)*theta),i = 0..2^(k-2)-1); 
   x[2] := add(S[2,i](r)*sin((2*i+1)*theta),i = 0..2^(k-2)-1); 
   x[3] := S[3,0](r)/2 +
           add(S[3,i](r)*cos((4*i  )*theta),i = 1..2^(k-3)-1); 
   x[4] := add(S[4,i](r)*cos((4*i+2)*theta),i = 0..2^(k-3)-1);
   x := [x[1],x[2],x[3],x[4]];

   return(evalf([seq(subs(theta=2*Pi*i/n,x),i=0..n)]));
  end
 ],

 ["Method","fourier_curve","This generates a plot of the curve $t\mapsto p(q(r\,e^{it}))$, where $r$ is supplied as the first argument, and $p$ is a map from $EX^*$ to $\\mathbb{R}^2$ or $\\mathbb{R}^3$ which is supplied as the third argument.  The second argument is the number of sample points, which needs to be large in order to get a smooth plot. If the third argument is omitted then we use the stereographic projection map to $\\mathbb{R}^3$",
  proc(this,r::RR0,n::posint := 1024,p := stereo)
   return(
    display(
     curve(map(p,this["fourier_ring_vals",r,n]),colour=black),
     scaling=constrained,axes=none
    ));
  end
 ]
):


