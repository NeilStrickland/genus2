#@ CLASS: P_to_H_chart

`Class/Declare`(
 "P_to_H_chart",
 "An instance of this class represents a pair of approximate polynomial solutions (f0,f1) near z=z0 to a certain linear second order differential equation depending on a parameter @d@.  If @d@ is chosen correctly, then f0 and f1 will be closely related to the canonical hyperbolic covering of the surface PX(a_P).",
 ["Field","a_P"::RR1,""],
 ["Field","degree"::posint=100,"The degree of polynomials used for approximate power series."],
 ["Field","d"::RR1,""],

 ["Field","z0"::CC1,"The centre around which we expand various power series."],

 ["Field","ss0"::procedure,"The series @ff0@ and @ff1@ satisfy $f''+s f/2=0$, where $s$ is @ss0 + d * ss1@.  Note that we are primarily interested in the function @s0(z)=ss0(z-z0)@ rather than @ss0@ itself."],
 ["Field","ss1"::procedure,"The series @ff0@ and @ff1@ satisfy $f''+s f/2=0$, where $s$ is @ss0 + d * ss1@.  Note that we are primarily interested in the function @s1(z)=ss1(z-z0)@ rather than @ss1@ itself."],
 ["Field","ff0"::procedure,"The series @ff0@ satisfies $f''+s f/2=0$ with $f(z)=1+O(z^2)$.  Note that we are primarily interested in the function @f0(z)=ff0(z-z0)@ rather than @ff0@ itself."],
 ["Field","ff1"::procedure,"The series @ff1@ satisfies $f''+s f/2=0$ with $f(z)=z+O(z^2)$.  Note that we are primarily interested in the function @f1(z)=ff1(z-z0)@ rather than @ff1@ itself."],
 ["Field","M"::Matrix,"This should be a $2\\times 2$ invertible complex matrix, to be interpreted as follows.  We have solutions $f_0$ and $f_1$ satisfying $f_i=(z-z_0)^i+O((z-z_0)^2)$, and there are also solutions $f_{00}$ and $f_{01}$ satisfying $f_{0i}=z^i+O(z^2)$.  The @M@ field should be set so that $(f_0,f_1)=M.(f_{00},f_{01})$."],
 ["Field","index"::integer,"Objects of the class @P_to_H_map@ will maintain a table of charts, identified by an integer index, which is stored in this field."],
 ["Field","parent_index"::integer,"This is the index of another chart, whose centre is closer to the origin.  We will set the @M@ field of this chart by comparing it with the parent chart, whose @M@ field will have been calculated already.  (The induction starts with the chart of index zero, which is centred at the origin and has @M@ equal to the identity matrix.)"],

 ["Method","ss"::procedure,"",
  proc(this)
   local z;
   unapply(expand(this["ss0"](z) + this["d"] * this["ss1"](z)),z);
  end
 ],

 ["Method","s0"::CC,"",
  proc(this,z::CC) this["ss0"](z-this["z0"]); end
 ],

 ["Method","s1"::CC,"",
  proc(this,z::CC) this["ss1"](z-this["z0"]); end
 ],

 ["Method","s"::CC,"",
  proc(this,z::CC) this["ss"](z-this["z0"]); end
 ],

 ["Method","f0"::CC,"",
  proc(this,z::CC) this["ff0"](z-this["z0"]); end
 ],

 ["Method","f1"::CC,"",
  proc(this,z::CC) this["ff1"](z-this["z0"]); end
 ],

 ["Method","df0"::CC,"",
  proc(this,z::CC) 
   local w;
   evalf(subs(w = z,diff(this["ff0"](w-this["z0"]),w)));
  end
 ],

 ["Method","df1"::CC,"",
  proc(this,z::CC) 
   local w;
   evalf(subs(w = z,diff(this["ff1"](w-this["z0"]),w)));
  end
 ],

 ["Method","f00"::CC,"",
  proc(this,z::CC)
   ((1/this["M"]) . <this["f0",z],this["f1",z]>)[1];
  end
 ],

 ["Method","f01"::CC,"",
  proc(this,z::CC)
   ((1/this["M"]) . <this["f0",z],this["f1",z]>)[2];
  end
 ],

 ["Method","g"::CC,
  "This is a function of the form $z+O(z^3)$, whose schwarzian derivative is $s$.",
  proc(this,z::CC)
   local v;
   v := ((1/this["M"]) . <this["f0",z],this["f1",z]>);
   v[2]/v[1];
  end
 ],

 ["Method","set_a_P"::void,"This sets the @a_P@ field, and also sets the @d@ field to a value that is approximately correct.",
  proc(this,a,d_)
   this["a_P"] := a;
   if nargs > 2 then
    this["d"] := d_
   else
    this["d"] := schwarz_d_approx(a);
   fi;

   NULL;
  end
 ],

 ["Method","set_s"::void,"This method sets the fields @ss0@ and @ss1@.  Note that these are independent of @d@, so we do not need to use this method again if we choose a new value of @d@.",
  proc(this)
   local z,s0,s1;

   s0 := evalf(subs(a_P = this["a_P"],S0_p1_inv(z+this["z0"])));
   s1 := evalf(subs(a_P = this["a_P"],S1_p1_inv(z+this["z0"])));
   s0 := convert(series(s0,z=0,this["degree"]+3),polynom,z);
   s1 := convert(series(s1,z=0,this["degree"]+3),polynom,z);
   this["ss0"] := unapply(s0,z);
   this["ss1"] := unapply(s1,z);

   NULL;
  end
 ],

 ["Method","set_f"::void,
  "This sets the fields @ff0@ and @ff1@",
  proc(this)
   local z,n,s,a,b,c,i,l;

   n := this["degree"];
   s := expand(this["ss0"](z) + this["d"] * this["ss1"](z));
   c := table():
   for i from 0 to n+3 do c[i] := coeff(s,z,i); od:

   a[0] := 1;
   a[1] := 0;
   for l from 0 to n-1 do
    a[l+2] := -add(a[i]*c[l-i],i=0..l)/2/(l+1)/(l+2);
   od:
   this["ff0"] := unapply(add(a[i]*z^i,i=0..n+1),z);

   b[0] := 0;
   b[1] := 1;
   for l from 0 to n-1 do
    b[l+2] := -add(b[i]*c[l-i],i=0..l)/2/(l+1)/(l+2);
   od:
   this["ff1"] := unapply(add(b[i]*z^i,i=0..n+1),z);
 
   if this["z0"] = 0 then this["M"] := IdentityMatrix(2); fi;

   NULL;   
  end
 ],

 ["Method","propagate_M"::void,
  "This method sets the @M@ field of this chart based on the @M@ field for another chart @C@ (whose centre should be close to the centre of this chart)",
  proc(this,C::P_to_H_chart)
   local z,f0,f1,df0,df1,N;
   # Note: we could do this by evaluating f0 and f1 from this chart, or from C.
   # The latter is better because the functions from C will usually have a 
   # larger radius of convergence.
   f0  := C["f0", this["z0"]];
   f1  := C["f1", this["z0"]];
   df0 := C["df0",this["z0"]];
   df1 := C["df1",this["z0"]];
   N := <<f0|df0>,<f1|df1>>;
   this["M"] := (1/N) . C["M"];
   NULL;
  end
 ]
):

######################################################################

#@ CLASS: P_to_H_map

`Class/Declare`(
 "P_to_H_map",
 "An instance of this class encapsulates data about a cromulent isomorphism $PX(a_P)\\to HX(a_H)$ for some specific numerical values of $a_H$ and $a_P$",

 ["Field","a_H"::RR1,""],
 ["Field","a_P"::RR1,""],
 ["Field","degree"::posint=20,"The degree of polynomials used for approximate power series."],
 ["Field","d"::RR1,"This is the number $d$ that enters into the formula for the schwarzian derivative of $p^{-1}$"],
 ["Field","test_point"::CC],

 ["Field","ap_H"::RR1],
 ["Field","am_H"::RR1],
 ["Field","v_HS"::table,"Table containing the points $\\psi^{-1}(v_{Hi})$"],
 ["Field","v_PS"::table,"Table containing the points $\\phi(v_{Pi})$"],
 ["Field","c_HS"::table,"Table containing the curves $\\psi^{-1}(c_{Hi}(t))$"],
 ["Field","c_PS"::table,"Table containing the curves $\\phi(c_{Pi}(t))$"],
 ["Field","psi"::procedure],
 ["Field","psi_inv"::procedure],
 ["Field","phi"::procedure],
 ["Field","phi_inv"::procedure],

 ["Field","c"::RR1,"The coefficient of $z$ in $p_1^{-1}(z)$"],
 ["Field","num_charts"::integer = 0],
 ["Field","charts"::table],
 ["Field","zero_chart"::P_to_H_chart],
 ["Field","unit_chart"::P_to_H_chart],
 ["Field","test_chart"::P_to_H_chart],
 ["Field","err"::RR1],
 ["Field","errs"::table],
 ["Field","p1_inv"::procedure],
 ["Field","p1"::procedure],

 ["Method","set_a_H"::RR1,
  "Set the @a_H@ field and perform associated bookkeeping.  Normally one should only use the @set_a_P@ method directly; then other code will calculate an appropriate value for @a_H@ and call the @set_a_H@ method.",
  proc(this,a::RR0)
   local i;

   this["a_H"]  := evalf(a);
   this["am_H"] := evalf(sqrt(1-a^2));
   this["ap_H"] := evalf(sqrt(1+a^2));

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

   return this["a_H"];
  end
 ],

 ["Method","set_a_P"::void,
  "Set the @a_H@ field and perform associated bookkeeping.",
  proc(this,a::RR0,d_)
   local i,n,theta,CC,C;

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

   theta := argument(this["v_PS"][11]):

   this["test_point"] :=  evalf(exp(I*(theta/2)));

   this["errs"] := table();

   if nargs > 2 then
    this["d"] := d_
   else
    this["d"] := schwarz_d_approx(a);
   fi;
   CC := eval(this["charts"]);
   n := this["num_charts"];
   for i from 0 to n-1 do 
    CC[i]["set_a_P",this["a_P"],this["d"]];
   od;
   NULL;
  end
 ],

 ["Method","add_chart"::P_to_H_chart,
  "Create and add a new chart centred at @z0@",
  proc(this,z0::CC)
   local C,n;

   C := eval(`new/P_to_H_chart`());
   C["set_a_P",this["a_P"],this["d"]];
   C["degree"] := this["degree"];
   C["z0"] := z0;
   C["set_s"];
   if not(type(this["charts"],table)) then
    this["charts"] := table():
   fi;
   n := this["num_charts"];
   C["index"] := n;
   this["charts"][n] := eval(C);
   this["num_charts"] := n+1;

   userinfo(5,genus2,sprintf("Added chart %d centred at %A",n,evalf[5](z0)));
   return eval(C);
  end
 ],

 ["Method","add_charts"::void,
  "Create and add $2n+1$ new charts.  Of these, one is centred at the origin, $n$ are centred at equally spaced points between $0$ and $i$, and $n$ are centred at equally spaed points between $0$ and @test_point@.",
  proc(this,n::posint := 10)
   local i,C;

   this["charts"] := table():
   this["num_charts"] := 0;

   C := eval(this["add_chart",0.]);
   this["zero_chart"] := eval(C);

   for i from 1 to n do
    C := eval(this["add_chart",evalf(I*i/n)]);
    C["parent_index"] := i-1;
   od:
   this["unit_chart"] := eval(this["charts"][n]);

   for i from 1 to n do
    C := eval(this["add_chart",evalf(i/n * this["test_point"])]);
    if i = 1 then
     C["parent_index"] := 0;
    else
     C["parent_index"] := n+i-1;
    fi;
   od:
   this["test_chart"] := eval(this["charts"][2*n]);
   NULL;
  end
 ],

 ["Method","set_d"::RR1,
  "This sets the @d@ field, then sets the @M@ fields of all charts based on that, then calculates @c@ and @a_H@ and @p1_inv@ as described in the text.  It returns an error term, which should be zero if we have the correct value of @d@.",
  proc(this,d::RR1)
   local i,j,n,CC,C,v,g,z,t,u0,u1,u2,p1i;

   this["d"] := evalf(d);
   userinfo(5,genus2,sprintf("set_d(%A)",evalf[5](this["d"])));

   n := this["num_charts"];
   CC := eval(this["charts"]);
   for i from 0 to n-1 do
    C := eval(CC[i]);
    C["d"] := evalf(d);
    C["set_f"];
    if i = 0 then
     C["M"] := IdentityMatrix(2);
     userinfo(6,genus2,"set_d for chart 0");
    else
     j := C["parent_index"];
     C["propagate_M",CC[j]];
     userinfo(6,genus2,sprintf("set_d for chart %d, parent is %d",i,j));
    fi;
   od;

   C := eval(this["unit_chart"]);
   g := C["g",I*exp(I*t)]/I;
   g := convert(series(g,t=0,3),polynom,t);
   u0 := Re(coeff(g,t,0));
   u1 := Re(coeff(g,t,1)/I);
   u2 := Re(coeff(g,t,2));
   this["c"] := sqrt(u2/(u0*(u0*u2+u1^2)));
   this["set_a_H", u1^2/sqrt(8*u0*u2*(u0*u2+u1^2)+u1^4)];

   C := eval(this["zero_chart"]);
   p1i := this["c"] * C["g",z];
   p1i := convert(series(p1i,z=0,this["degree"]),polynom,z);

   this["p1_inv"] := unapply(p1i,z);

   C := eval(this["test_chart"]);
   g := this["c"] * C["g",C["z0"]];
   this["err"] := Im(this["psi"](g));

   this["errs"][this["d"]] := this["err"];

   userinfo(5,genus2,sprintf("log[10](|err|) = %A",evalf[5](log[10](abs(this["err"])))));

   return this["err"];
  end
 ],

 ["Method","find_p1_inv"::void,
  "This calls the @set_d@ method repeatedly with different values of @d@, searching for the value that makes the @set_d@ error term equal to zero.  It also sets the fields @c@, @a_H@ and @p1_inv@.",
  proc(this,tol := 10.^(-20),gap := 1)
   local F,d0;

   F := proc(d) 
    local T;
    `P_to_H_map!set_d`(this,d);
    T := table():
    T["err"] := this["err"];
    return eval(T);
   end:

   d0 := this["d"];
   if not type(d0,RR1) then
    d0 := schwarz_d_approx(this["a_H"]);
   fi;

   brent_fsolve(F,d0-gap,d0+gap,false,false,tol);
   NULL;
  end
 ],

 ["Method","set_p1"::void,
  "This sets the @p1@ field based on the @p1_inv@ field.",
  proc(this)
   local z,p1,p1i;

   p1i := this["p1_inv"](z);
   p1 := revert_series(p1i,z);
   this["p1"] := unapply(p1,z);

   NULL;
  end
 ],

 ["Method","p1_coeff_plot"::plot,
  "This generates a plot of the logs of the absolute values of the coefficients of the power series for $p_1(z)$",
  proc(this)
   local p1,d,z;

   p1 := this["p1"](z):
   d := (degree(p1,z)-1)/2;
   display(seq(point([2*i+1,log(abs(coeff(p1,z,2*i+1)))]),i=0..d));
  end
 ],

 ["Method","p1_inv_coeff_plot",
  "This generates a plot of the logs of the absolute values of the coefficients of the power series for $p^{-1}_1(z)$",
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
 ]
):

######################################################################
######################################################################
######################################################################
######################################################################

# Here is some older code for schwarzian solutions near a critical point.
# Ideally, it should be incorporated in the above framework.

# # For technical reasons this returns power series in z, but they should
# # really be regarded as power series in (z-alpha).
# schwarz_critical_basic_solutions := proc(a0,d0)
#  local ss,c,g0,g1,err,sol;
# 
#  ss := simplify(S_p1_inv(z+schwarz_alpha)):
#  ss := evalf(subs({a_P = a0,d_p_inv = d0},ss));
#  ss := convert(series(ss,z=0,2*p1_inv_N),polynom,z):
# 
#  # This funny sequence seems to be necessary to remove a tiny imaginary 
#  # part from the coefficient of z^(-2).
#  c  := coeff(ss,z,-2):
#  ss := expand(ss - c*z^(-2)):
#  ss := expand(ss + Re(c)*z^(-2)):
# 
#  g0 := z^(1/4) + add(aa0[k]*z^(k+1/4),k=1..2*p1_inv_N+1):
#  err := convert(series(expand((diff(g0,z,z)+ss*g0/2)/z^(1/4)),z=0,2*p1_inv_N),polynom,z):
#  err := err - coeff(err,z,-2)/z^2: # should be zero already, but there is a small numerical error
#  sol := solve({coeffs(err,z)}):
#  g0 := expand(z^(-1/4) * subs(sol,g0)):
# 
#  g1 := z^(3/4) + add(aa0[k]*z^(k+3/4),k=1..2*p1_inv_N+1):
#  err := convert(series(expand((diff(g1,z,z)+ss*g1/2)/z^(3/4)),z=0,2*p1_inv_N),polynom,z):
#  err := err - coeff(err,z,-2)/z^2: # should be zero already, but there is a small numerical error
#  sol := solve({coeffs(err,z)}):
#  g1 := expand(z^(-3/4) * subs(sol,g1)):
# 
#  return([g0,g1]);
# end:
# 
# schwarz_critical_rebase := proc(a0,d0,z0,f)
#  local c0,c1,c2,alpha0,g0,g1,g0o,g1o,ff,ffo,p0,p1,p2,err,sol;
#  c0 := subs(z=z0,f);
#  c1 := subs(z=z0,diff(f,z));
#  c2 := subs(z=z0,diff(f,z,z));
#  g0o,g1o := op(schwarz_critical_basic_solutions(a0,d0));
#  alpha0 := evalf(subs(a_P = a0,schwarz_alpha));
#  g0 := subs(z = z+z0-alpha0,g0o);
#  g1 := subs(z = z+z0-alpha0,g1o);
# 
#  ffo := (p0*g0o+p1*g1o*sqrt(z))/(g0o+p2*g1o*sqrt(z)):
#  err := (p0*g0+p1*g1*sqrt(z+z0-alpha0)) - (c0+c1*z+c2*z^2/2)*(g0+p2*g1*sqrt(z+z0-alpha0)):
#  err := convert(series(err,z=0,3),polynom,z);
#  sol := solve({coeffs(err,z)},{p0,p1,p2});
#  ffo := subs(sol,ffo):
#  ffo := convert(series(ffo,z=0,2*p1_inv_N),polynom,z):
#  ffo := simplify(subs(z=t^2,ffo),symbolic):
#  
#  return(subs(t = -I*sqrt(alpha0-z),ffo));
# end:
