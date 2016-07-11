#@ CLASS: H_to_P_map

`Class/Declare`("H_to_P_map",
 "An instance of this class encapsulates data about a cromulent isomorphism $HX(a_H) \\to PX(a_P)$ for some specific numerical values of $a_H$ and $a_P$",

 ["Field","a_H"::numeric,"This should be set by the @set_a_H@ method."],
 ["Field","a_P"::numeric,"This will be calculated by the @find_p1@ method, and should not be set directly."],

 ["Field","v_HS"::table,"As in Definition defn-schwarz-phi"],
 ["Field","v_PS"::table,"As in Definition defn-schwarz-phi"],
 ["Field","c_HS"::table,"As in Definition defn-schwarz-phi"],
 ["Field","c_PS"::table,"As in Definition defn-schwarz-phi"],
 ["Field","psi","As in Definition defn-schwarz-phi.  This is set up so that if @X@ is an instance of the present class, the required syntax is @X[\"psi\"](z)@."],
 ["Field","psi_inv","As in Definition defn-schwarz-phi, with the same kind of syntax as for @psi@"],
 ["Field","phi","As in Definition defn-schwarz-phi, with the same kind of syntax as for @psi@"],
 ["Field","phi_inv","As in Definition defn-schwarz-phi, with the same kind of syntax as for @psi@"],

 ["Field","samples"::list(CC0),"This is a list of points lying in $\\psi^{-1}((C_3\\cup C_5)\\cap F_{16})$.  These should be sent by $p_1$ to the unit circle, and the @find_p1@ method will try to choose the coefficients of $p_1$ to make this true.  This should be set by the @make_samples@ method."],

 ["Field","num_samples"::posint = 200,"Number of sample points.  This should be significantly larger than the @poly_deg@ field.  It should be set by the @make_samples@ method."],
 ["Field","errs"::list,"The list of differences $|p_1(z)|^2-1$, as $z$ runs through the sample points. "],
 ["Field","err"::RR0,"The maximum absolute value of the above errors."],

 ["Field","poly_deg"::posint = 20,"Number of degrees of freedom when choosing $p_1$.  It should be set by the @set_poly_deg@ method."],

 ["Field","a"::list(RR0),"This is a list of coefficients (of length equal to @poly_deg@) which determines the approximating function $p_1$.  In more detail, we have $p_1=p_{10}+a_1p_{11}(z)+a_2p_{12}(z)+a_3p_{13}(z)+(\sum_{i>3}a_iz^{2*i-8})p_{14}(z)$ for certain fixed functions $p_{1k}(z)$."],

 ["Field","p1","A rational function of $z$ which approximates the map $p_1\\:\\Delta\\to\\C$."],
 ["Field","p1_series","A Taylor series for $p_1$."],
 ["Field","p10","An auxiliary polynomial."],
 ["Field","p11","An auxiliary polynomial."],
 ["Field","p12","An auxiliary polynomial."],
 ["Field","p13","An auxiliary rational function."],
 ["Field","p14","An auxiliary polynomial."],
 ["Field","p1_inv","A polynomial approximation to the compositional inverse of $p_1$."],
 ["Field","S_p1_inv","A polynomial approximation to the schwarzian derivative of $p_1^{-1}$."],
 ["Field","d","A constant such that the schwarzian derivative of $p_1^{-1}$ is @S0_p1_inv + d * S1_p1_inv@"],
 ["Field","schwarzian_errs"::list,"A list of differences between the coefficients of $S(p_1^{-1})$ and @S0_p1_inv + d * S1_p1_inv@"],
 ["Field","m_series","A Taylor series for the function $m(z)$ such that $\\omega_0=m(z) dz$."],
 ["Field","p_series","A Taylor series for $p(z)$."],
 ["Field","mp_series","A Taylor series for $m(z)p(z)$."],

 ["Method","set_a_H"::RR1,
  "Set the @a_H@ field and perform associated bookkeeping.",
  proc(this,a::RR1)
   local i,t,z;
   this["a_H"] := evalf(a);
   this["v_HS"] := table();
   for i in [0,1,2,3,6,10,11,12,13] do 
    this["v_HS"][i] := evalf(subs(a_H = a,v_HS[i]));
   od;
   this["c_HS"] := table();
   for i in [0,1,3,5] do
    this["c_HS"][i] := unapply(evalf(subs(a_H = a,c_HS[i](t))),t);
   od;
   this["psi"]     := unapply(evalf(subs(a_H = a,schwarz_psi(z))),z);
   this["psi_inv"] := unapply(evalf(subs(a_H = a,schwarz_psi_inv(z))),z);

   this["set_p_aux"];
   return a;
  end
 ],
 
 ["Method","set_a_P"::RR1,
  "Set the @a_P@ field and perform associated bookkeeping.  Normally one should only use the @set_a_H@ method directly; then other code will calculate an appropriate value for @a_P@ and call the @set_a_P@ method.",
  proc(this,a::RR1)
   local i,t,z;
   this["a_P"] := evalf(a);
   this["v_PS"] := table();
   for i in [0,1,2,3,6,10,11,12,13] do 
    this["v_PS"][i] := evalf(subs(a_P = a,v_PS[i]));
   od;
   this["c_PS"] := table();
   for i in [0,1,3,5] do
    this["c_PS"][i] := unapply(evalf(subs(a_P = a,c_PS[i](t))),t);
   od;
   this["phi"]     := eval(schwarz_phi);
   this["phi_inv"] := eval(schwarz_phi_inv);

   this["set_p_aux"];
   return a;
  end
 ],
 
 ["Method","set_poly_deg"::void,
  "Set the @poly_deg@ field and perform associated bookkeeping.",
  proc(this,d::posint)
   this["poly_deg"] := d;
   this["fix_a"];
   this["set_p_aux"];
   NULL;
  end
 ],

 ["Method","fix_a"::void,
  "Truncate or extend the @a@ field to ensure that it has the right length.",
  proc(this)
   local a,n,m,z0;
   n := this["poly_deg"];
   a := this["a"];
   if not(type([a],[list])) then
    if type(this["a_P"],numeric) then
     z0 := evalf(subs(a_P=this["a_P"],v_PS[11])):
    else 
     z0 := evalf(subs(a_P=0.5,v_PS[11])):
    fi;
    if n >= 2 then
     this["a"] := [Re(z0),Im(z0),0$(n-2)];
    else
     this["a"] := [0$n];
    fi;
    return;
   fi;
   m := nops(a);
   if m < n then
    this["a"] := [op(a),0 $ (n-m)];
   else
    this["a"] := [op(1..n,a)];
   fi;
   NULL;
  end
 ],

 ["Method","set_p_aux"::void,
  "Set the auxiliary functions @p11@ to @p15@.  These are all odd polynomials with real coefficients and vanishing derivatives at $\\psi^{-1}(v_0)$ and $\\psi^{-1}(v_{11})$.  The polynomials @p11@ and @p12@ vanish at $\\psi^{-1}(v_0)$ and $\\psi^{-1}(v_3)$, and take the values $1$ and $i$ respectively at $\\psi^{-1}(v_{11})$.  The polynomial @p13@ sends $\\psi^{-1}(v_i)$ to $\\phi(v_i)$ for $i\\in\\{0,3,6,11\\}$, whereas @p14@ sends all these points to zero.  The polynomials @p11@, @p12@ and @p13@ have degree $13$, whereas @p14@ has degree $15$, with linear term $z$.  These properties charecterise the polynomials uniquely.",
  proc(this)
   local v,e,d,a,i,aP,sol,z0,p10,p11,p12,p13,p14;
   v := eval(this["v_HS"]);
   e := (f) -> evalf([Im(f(v[3])),Re(f(v[0])),Re(D(f)(v[0])),
                      Re(f(v[11])),Im(f(v[11])),
                      Re(D(f)(v[11])),Im(D(f)(v[11]))]);
   d := this["poly_deg"];
   for i from 0 to max(6,d) do assume(a[i]::real); od:

   aP := this["a_P"];
   if not(type([aP],[numeric])) then
    aP := fsolve(schwarz_b_approx(a) - this["a_H"],a=0..1);
    this["set_a_P"][aP];
   fi;


   p10 := unapply(add(a[i]*z^(2*i+1),i=0..6),z):
#   z0 := subs(a_P=aP,v_PS[11]):
#   sol := solve(e(p10) -~ [1,1,0,Re(z0),Im(z0),0,0]):
   sol := solve(e(p10) -~ [1,1,0,0,0,0,0]):
   p10 := unapply(subs(sol,p10(z)),z):

   p11 := unapply(add(a[i]*z^(2*i+1),i=0..6),z):
   sol := solve(e(p11) -~ [0,0,0,1,0,0,0]):
   p11 := unapply(subs(sol,p11(z)),z):

   p12 := unapply(add(a[i]*z^(2*i+1),i=0..6),z):
   sol := solve(e(p12) -~ [0,0,0,0,1,0,0]):
   p12 := unapply(subs(sol,p12(z)),z):

   p14 := unapply(z+add(a[i]*z^(2*i+1),i=1..7),z):
   sol := solve(e(p14)):
   p14 := unapply(tidy(subs(sol,p14(z))),z):

   p13 := unapply(p14(z)/evalf(subs(a_H = this["a_H"],tiny_p1_denom(z))),z);

   this["p10"] := eval(p10):
   this["p11"] := eval(p11):
   this["p12"] := eval(p12):
   this["p13"] := eval(p13):
   this["p14"] := eval(p14):

   NULL;
  end
 ],

 ["Method","p"::CC0,"This calculates the function $p=\\phi^{-1}\\circ p_1\\circ\\psi$",
  proc(this,z::CC0)
   this["phi_inv"](this["p1"](this["psi_inv"](z)));
  end
 ],

 ["Method","p_piecewise"::CC0,
  "This also calculates $p(z)$, but it uses the equivariance properties of $p_1$ so we only need to calculate $p_1(w)$ when $w$ is small",
  proc(this,z::CC0)
   local T0,T1,z0,w0,w;

   # Note: this assumes that a_H0 = this["a_H"], which we have generally 
   # not assumed elsewhere.
   T1,T0,z0 := op(retract_F16_H0_aux(z));
   w0 := this["p",z0];
   w := act_C[G_inv(T1)](w0);
   return w;
  end
 ],

 ["Method","m"::CC0,
  "This calculates the function $m(z)$ such that $p^*(\\omega_0)=m(z) dz$",
  proc(this,z0::CC0)
   local z,z1,z2,z3,w3,a1;
   
   z1 := this["psi_inv"](z0);
   z2 := this["p1"](z1);
   z3 := this["phi_inv"](z2);
   w3 := sqrt(subs(a_P = this["a_P"],r_P(z3)));
   a1 := -2*I/(1+z2)^2 *
         subs(z=z1,diff(this["p1"](z),z)) *
         subs(z=z0,diff(this["psi_inv"](z),z));
   return a1/w3;
  end
 ],
 
 ["Method","m_piecewise"::CC0,
  "This also calculates $m(z)$, but it uses the equivariance properties of $p_1$ so we only need to calculate $p_1(w)$ when $w$ is small",
  proc(this,z0::CC0)
   local T0,T1,z1,z2,m0,m1,m2;

   # Note: this assumes that a_H0 = this["a_H"], which we have generally 
   # not assumed elsewhere.
   T1,T0,z2 := op(retract_F16_H0_aux(z0));
   z1 := act_Pi0(T0,z0);
   m2 := this["m",z2];

   if member(T1,{1,L,LL,LLL}) then
    m1 := m2;
   elif member(T1,{N,LN,LLN,LLLN}) then
    m1 := conjugate(m2);
   elif member(T1,{M,LM,LLM,LLLM}) then
    m1 := m2 * D(act_H1[M])(z1) / act_C[G_inv(T1)](this["p",z2]);
   elif member(T1,{MN,LMN,LLMN,LLLMN}) then
    m1 := conjugate(m2 * D(act_H1[M])(conjugate(z1))) / act_C[G_inv(T1)](this["p",z2]);
   fi;

   m0 := m1 * subs(z = z0,diff(act_Pi0(T0,z),z));
   return m0;
  end
 ],

 ["Method","find_m_series",
  "This finds a power series approximation to $m(z)$",
  proc(this,radius::RR0,num_samples::posint,poly_deg::posint)
   local i,j,ri,samples,v,M,a,z;

   samples := evalf([seq(radius * exp(Pi*I/2*i/num_samples),i=1..num_samples)]);
   ri := (u) -> map(z -> (Re(z),Im(z)),u);
   v := Vector(ri(map(z -> this["m_piecewise",z],samples)));
   M := Transpose(Matrix([seq(ri([seq(samples[i]^(4*j),i=1..num_samples)]),j=0..poly_deg-1)]));
   a := LeastSquares(M,v,method='QR');
   this["m_series"] := unapply(add(a[j]*z^(4*j-4),j=1..poly_deg),z);
   return eval(this["m_series"]);
  end
 ],

 ["Method","find_mp_series",
  "This finds a power series approximation to $m(z)p(z)$",
  proc(this,radius::RR0,num_samples::posint,poly_deg::posint)
   local i,j,ri,samples,v,M,a,z;

   samples := evalf([seq(radius * exp(Pi*I/2*i/num_samples),i=1..num_samples)]);
   ri := (u) -> map(z -> (Re(z),Im(z)),u);
   v := Vector(ri(map(z -> this["m_piecewise",z]*this["p_piecewise",z],samples)));
   M := Transpose(Matrix([seq(ri([seq(samples[i]^(4*j+2),i=1..num_samples)]),j=0..poly_deg-1)]));
   a := LeastSquares(M,v,method='QR');
   this["mp_series"] := unapply(add(a[j]*z^(4*j-2),j=1..poly_deg),z);
   return eval(this["mp_series"]);
  end
 ],

 ["Method","find_p_series",
  "This finds a power series approximation to $p(z)$",
  proc(this,radius::RR0,num_samples::posint,poly_deg::posint)
   local i,j,ri,samples,v,M,a,z;

   samples := evalf([seq(radius * exp(Pi*I/2*i/num_samples),i=1..num_samples)]);
   ri := (u) -> map(z -> (Re(z),Im(z)),u);
   v := Vector(ri(map(z -> this["p_piecewise",z],samples)));
   M := Transpose(Matrix([seq(ri([seq(samples[i]^(4*j+2),i=1..num_samples)]),j=0..poly_deg-1)]));
   a := LeastSquares(M,v,method='QR');
   this["p_series"] := unapply(add(a[j]*z^(4*j-2),j=1..poly_deg),z);
   return eval(this["p_series"]);
  end
 ],

 ["Method","make_samples"::void,
  "Calculate a list of sample points.  We have used equally spaced points, but Chebyshev spacing might be better.",
  proc(this,num_samples_)
   local n3,n5,c3,c5,i;
   
   if nargs > 1 and num_samples_ > 4 then
    this["num_samples"] := num_samples_;
   else
    this["num_samples"] := 50;
   fi;

   n3 := floor(this["num_samples"]/2);
   n5 := this["num_samples"] - n3;
   c3 := eval(this["c_HS"][3]);
   c5 := eval(this["c_HS"][5]);

   this["samples"] := [
    seq(evalf(c3(i*Pi/(2*n3))),i=1..n3),
    seq(evalf(c5(i*Pi/(n5-1))),i=0..n5-1)
   ];

   NULL;
  end
 ],

 ["Method","find_p1"::void,
  "This method searches for a polynomial @p1@ of the required form, which sends the sample points as close as possible to the unit circle.",
  proc(this,num_steps::posint := 10)
   local i,j,k,d,ns,S,pv,PV,CV,A,dA,J,E,aP,u,p10,p11,p12,p13,p14;

   this["set_p_aux"];
   this["fix_a"];
   d := this["poly_deg"];
   if not(type([this["samples"]],[list(CC0)])) then
    this["make_samples"]:
   fi; 

   S := this["samples"];
   ns := nops(S);

   p10 := eval(this["p10"]);
   p11 := eval(this["p11"]);
   p12 := eval(this["p12"]);
   p13 := eval(this["p13"]);
   p14 := eval(this["p14"]);

   pv := proc(z)
    u := p14(z);
    [p11(z),p12(z),p13(z),seq(z^(2*i)*u,i=0..d-4)];
   end:
   PV := Matrix(map(pv,S));
   CV := Vector(map(p10,S));

   A := Vector(this["a"]);
   J := Matrix(ns,d);

   # We want this vector to be zero.
   E := map(z -> abs(z)^2 - 1,PV . A + CV);

   for i from 1 to num_steps do

    # Set J to be the Jacobian matrix of the map A |-> E
    for j from 1 to ns do
     for k from 1 to d do
      J[j,k] := 2*Re(conjugate(PV[j,k]) * (add(PV[j,l]*A[l],l=1..d) + CV[j]));
     od;
    od;

    # Newton-Raphson step
    dA := LeastSquares(J,-E);
    A := A + dA;
    E := map(z -> abs(z)^2 - 1,PV . A + CV);

    userinfo(7,genus2,
     sprintf("log[10](step_size) = %A, log[10](max_err) = %A",
      evalf[5](log[10](Norm(dA,2))), 
      evalf[5](log[10](Norm(E))))
    );
   od:

   this["a"] := convert(A,list);
   this["p1"] := 
    unapply(A[3] * p13(z) + 
     expand(p10(z) + A[1] * p11(z) + A[2] * p12(z) + 
            add(A[i]*z^(2*(i-4)),i=4..d) * p14(z)),z);

   this["p1_series"] := unapply(convert(series(this["p1"](z),z=0,2*d+9),polynom,z),z);

   aP := Re(schwarz_phi_inv(this["p1"](this["v_HS"][11])));
   this["set_a_P",aP];

   this["errs"] := convert(E,list);
   this["err"] := Norm(E);
   NULL;
  end
 ],

 ["Method","set_p1_inv"::void,
  "This sets the fields @p1_inv@, @S_p1_inv@ and @d@ based on the @p1@ field.",
  proc(this)
   local m,z,p1,p1i,Sp1i,err;

   p1 := this["p1_series"](z);
   p1i := revert_series(p1,z);
   m := degree(p1,z);

   this["p1_inv"] := unapply(p1i,z);
   Sp1i := convert(series(schwarzian(p1i,z),z=0,m+1),polynom,z);
   this["S_p1_inv"] := unapply(Sp1i,z);

   err := evalf(subs({a_P = this["a_P"]},subs(z = 0,Sp1i) - S_p1_inv(0)));
   this["d"] := solve(err,d_p_inv);

   err := Sp1i - evalf(subs({a_P=this["a_P"],d_p_inv=this["d"]},S_p1_inv(z)));
   err := convert(series(err,z=0,m+1),polynom,z); 
   this["schwarzian_errs"] := [seq(abs(coeff(err,z,i)),i=2..m-3,2)];

   NULL;
  end
 ],

 ["Method","p1_plot"::plot,
  "Generates a plot of p1(F16), which should just be the first quadrant of the unit disc",
  proc(this)
   local c,i,t;
   for i in [0,1,3,5] do
    c[i] := evalf(this["p1"](this["c_HS"][i](t)));
   od:

   display(
    seq(cplot(c[i],t=F16_curve_limits[i],colour=c_colour[i]),i in [0,1,3,5]),
    axes=none,scaling=constrained
   );
  end
 ],

 ["Method","v11_plot"::plot,
  "Generates a plot of the behaviour of $p_1$ near $v_{11}$",
  proc(this,eps := 0.01)
   local c,i,t,t0,z0;
   for i in [3,5] do
    c[i] := evalf(this["p1"](this["c_HS"][i](t)));
   od:
   z0 := this["v_PS"][11];
   t0 := argument(z0);

   display(
    plot([cos(t),sin(t),t=t0-eps..t0+eps],colour=grey),
    cplot(c[3],t=-2*sqrt(eps)..2*sqrt(eps),colour=c_colour[3]),
    cplot(c[5],t=Pi-4*sqrt(eps)..Pi+4*sqrt(eps),colour=c_colour[5]),
    cpoint(subs(t=0,c[3]),colour=black),
    scaling=constrained,
    view=[Re(z0)-eps..Re(z0)+eps,Im(z0)-eps..Im(z0)+eps]
   );
  end
 ],

 ["Method","err_plot"::plot,
  "This generates a plot showing the errors stored in the @errs@ field.",
  proc(this) listplot(this["errs"],style=point); end
 ],

 ["Method","a_plot"::plot,
  "This generates a plot showing the logs of the absolute values of the coefficients in the @a@ field, which determine the approximating function $p_1$",
  proc(this) listplot(map(log,map(abs,this["a"])),style=point); end
 ],

 ["Method","p1_coeff_plot"::plot,
  "This generates a plot showing the logs of the absolute values of the coefficients in the Taylor expansion of $p_1(z)$",
  proc(this)
   local p1,d,z;

   p1 := this["p1_series"](z):
   d := (degree(p1,z)-1)/2;
   display(seq(point([2*i+1,log(abs(coeff(p1,z,2*i+1)))]),i=0..d));
  end
 ],

 ["Method","p1_inv_coeff_plot",
  "This generates a plot showing the logs of the absolute values of the coefficients in the Taylor expansion of $p_1^{-1}(z)$",
  proc(this)
   local i,d,z,p1i,cols;

   cols := ["Red","YellowGreen","BlueViolet","Gold","Olive","Tomato","Lime",
            "VioletRed","DeepSkyBlue","DarkOrange","Cyan","Magenta","Goldenrod",
            "Turquoise","ForestGreen","DarkOrchid"];

   p1i := this["p1_inv"](z):
   d := (degree(p1i,z)-1)/2;
   display(seq(point([2*i+1,log(abs(coeff(p1i,z,2*i+1)))],
                     colour=cols[modp(i,16)+1],
                     symbol=solidcircle),
               i=0..d));

  end
 ],

 ["Method","schwarzian_err_plot",
  "This generates a plot showing the logs of the absolute values of the errors stored in the @schwarzian_errs@ field.",
  proc(this) listplot(map(log,map(abs,this["schwarzian_errs"])),style=point); end
 ],

 ["Method","m_plot",
  "This generates a plot showing the curve $m(r e^{it})$, where $r$ is the @radius@ argument.",
  proc(this,radius,num_points)
   local ms,P,i,z;
   
   ms := eval(this["m_series"]);
   P := NULL;
   for i from 0 to num_points do
    z := evalf(radius * exp(Pi*I*i/2/num_points));
    P := P,C_to_R2(ms(z));
   od:
   display(curve([P],args[4..-1]),axes=none,scaling=constrained);
  end
 ],

 ["Method","m_plot_tikz",
  "This generates a tikzpicture environment showing the curve $m(r e^{it})$, where $r$ is the @radius@ argument.",
  proc(this,radius,num_points,scale)
   local ms,s,i,z,w;
   
   ms := eval(this["m_series"]);
   s := "\\begin{center}\n";
   s := cat(s,sprintf(" \\begin{tikzpicture}[scale=%A]\n",scale));
   s := cat(s,"  \\draw[smooth] "):
   for i from 1 to num_points do
    z := evalf(radius * exp(Pi*I*i/2/num_points));
    w := ms(z);
    s := cat(s,sprintf("(%1.4f,%1.4f) -- ",-Im(w),Re(w)));
   od:
   s := cat(s," cycle;\n \\end{tikzpicture}\n\\end{center}\n"):

   return s;
  end
 ],

 ["Method","check","",
  proc(this)
   return [
   trim(P["p10"](z) + P["p10"](-z)),
   trim(P["p11"](z) + P["p11"](-z)),
   trim(P["p12"](z) + P["p12"](-z)),
   trim(P["p14"](z) + P["p14"](-z)),
   map(trim,map(Im,[coeffs(P["p10"](z),z)])),
   map(trim,map(Im,[coeffs(P["p11"](z),z)])),
   map(trim,map(Im,[coeffs(P["p12"](z),z)])),
   map(trim,map(Im,[coeffs(P["p14"](z),z)])),
   degree(P["p10"](z),z) - 13,
   degree(P["p11"](z),z) - 13,
   degree(P["p12"](z),z) - 13,
   degree(P["p14"](z),z) - 15,
   trim(evalf(subs(a_H = P["a_H"],D(P["p10"])(v_HS[ 0])))),
   trim(evalf(subs(a_H = P["a_H"],D(P["p11"])(v_HS[ 0])))),
   trim(evalf(subs(a_H = P["a_H"],D(P["p12"])(v_HS[ 0])))),
   trim(evalf(subs(a_H = P["a_H"],D(P["p14"])(v_HS[ 0])))),
   trim(evalf(subs(a_H = P["a_H"],D(P["p10"])(v_HS[11])))),
   trim(evalf(subs(a_H = P["a_H"],D(P["p11"])(v_HS[11])))),
   trim(evalf(subs(a_H = P["a_H"],D(P["p12"])(v_HS[11])))),
   trim(evalf(subs(a_H = P["a_H"],D(P["p14"])(v_HS[11])))),
   trim(evalf(subs(a_H = P["a_H"],P["p10"](v_HS[ 0]) - 1))),
   trim(evalf(subs(a_H = P["a_H"],P["p10"](v_HS[ 3]) - I))),
   trim(evalf(subs(a_H = P["a_H"],P["p10"](v_HS[11])))),
   trim(evalf(subs(a_H = P["a_H"],P["p11"](v_HS[ 0])))),
   trim(evalf(subs(a_H = P["a_H"],P["p11"](v_HS[ 3])))),
   trim(evalf(subs(a_H = P["a_H"],P["p11"](v_HS[11]) - 1))),
   trim(evalf(subs(a_H = P["a_H"],P["p12"](v_HS[ 0])))),
   trim(evalf(subs(a_H = P["a_H"],P["p12"](v_HS[ 3])))),
   trim(evalf(subs(a_H = P["a_H"],P["p12"](v_HS[11]) - I))),
   trim(evalf(subs(a_H = P["a_H"],P["p14"](v_HS[ 0])))),
   trim(evalf(subs(a_H = P["a_H"],P["p14"](v_HS[ 3])))),
   trim(evalf(subs(a_H = P["a_H"],P["p14"](v_HS[11])))),
   trim(rem(P["p14"](z),z^3,z) - z)
   ];
  end
 ]
):

