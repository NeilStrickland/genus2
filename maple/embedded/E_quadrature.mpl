#@ CLASS: E_quadrature_rule

`Class/Declare`("E_quadrature_rule",
 "An instance of this class represents a quadrature rule for approximate integration of functions on $EX^*$.  Essentially, such a rule consists of a list of points $p_i$ in the fundamental domain $F_{16}$, together with weights $w_i\\geq 0$.  The integral over $F_{16}$ of a function $f$ is approximately $\\sum_i w_i f(p_i)$.  Thus, the integral over $EX^*$ is $\\sum_i\\sum_{g\\in G}w_if(g.p_i)$.\n\n This class also has fields and methods designed to allow us to measure and improve the accuracy of the quadrature rule.  Specifically, suppose we have calculated the integrals of some monomials $z_1^iz_2^j$ by some other method.  We can then list these monomials in the @test_monomials@ field, and their integrals in the @test_integrals@ field.  Various methods will then try to ensure that our quadrature rule gives the correct answer on those monomials.",

 ["Field","num_points"::integer = 0,"Number of evaluation points"],

 ["Field","points"::table,"The list of evaluation points.  This is represented as a table indexed by natural numbers (starting at 0), and each point is represented as an object of the class @E_point@ defined in the file @E_domain.mpl@"],

 ["Field","weights"::table,"The list of weights, represented as a table indexed by natural numbers."],

 ["Field","num_test_monomials"::integer = 0],

 ["Field","test_monomials"::table,
   "The list of test monomials, represented as a table indexed by natural numbers."],

 ["Field","test_integrals"::table,
   "The list of integrals over $F_{16}$ of test monomials, represented as a table indexed by natural numbers.  This can be used if we have calculated the relevant integrals by some other means.  If not, this field should be null."],

 ["Field","test_monomial_values"::table,
   "This is a table @T@ such that @T[i,j]@ is the value of the $i$'th test monomial $m_i$ at the $j$'th evaluation point $p_j$.  It should be set using the @set_monomial_values@ method."],

 ["Field","test_monomial_diff_values"::table,
   "This is a table @T@ such that @T[i,j,0]@ and @T[i,j,1]@ measure the derivatives of $m_i$ at the point $p_j$.  In more detail, $p_j$ is stored as an instance of the class @E_point@, so it comes packaged with two tangent vectors $u_j$ and $v_j$; the entries @T[i,j,0]@ and @T[i,j,1]@ are the derivatives in these directions.  If $p_j$ is in the interior of $F_{16}$ then $u_j$ and $v_j$ will form an oriented orthonormal frame.  If $p_j$ is on an edge of $F_{16}$ then $u_j$ will be a unit vector pointing along the edge, and $v_j$ will be zero.  If $p_j$ is at a corner then both $u_j$ and $v_j$ will be zero."
 ],

 ["Field","errors"::table],
 ["Field","total_square_error" = 0],

 ["Field","eval_det"::RR1,
  "Assuming that the number of test monomials is at least as large as the number $n$ of evaluation points, the @set_eval_det@ method will set the @eval_det@ field to be the determinant of the $n\\times n$ matrix of values of the first $n$ test monomials at the evaluation points.  Some theory developed in a slightly different context suggests that it is desirable to adjust the evaluation points to make this determinant as large as possible."
 ],

 ["Field","eval_det_diff"::table,
  "Assuming that the number of test monomials is at least as large as the number $n$ of evaluation points, the @set_eval_det_diff@ method will set the @eval_det_diff@ field to be a table @T@ containing information about how @eval_det@ changes as the evaluation points are moved.  In more detail, @T[j,0]@ is the derivative of @eval_det@ if $p_j$ is moved in the direction $u_j$, and @T[j,1]@ is the derivative of @eval_det@ if $p_j$ is moved in the direction $v_j$."
 ],

 ["Constructor","",
  proc(this)
   local f;

   for f in ["points",
             "weights",
             "test_monomials",
             "test_monomial_values",
             "test_monomial_diff_values",
             "errors"] do 
    this[f] := table();
   od;

   this["test_integrals"] := NULL;
   this["eval_det_diff"] := NULL;
  end
 ],

 ["Method","clone"::E_quadrature_rule,
  "Returns a cloned copy of this quadrature rule",
  proc(this)
   local Q,i,f;

   Q := `new/E_quadrature_rule`();
   Q["num_points"] := this["num_points"];
   Q["num_test_monomials"] := this["num_test_monomials"];

   for i from 0 to this["num_points"]-1 do
    Q["points"][i] := eval(this["points"][i]["clone"]);
   od;

   for f in ["weights",
             "test_monomials",
             "test_integrals",
             "test_monomial_values",
             "test_monomial_diff_values",
             "errors",
             "eval_det_diff"] do 
    if this[f] = NULL then
     Q[f] := NULL;
    else
     Q[f] := copy(this[f]);
    fi;
   od;

   return eval(Q);
  end
 ],

 ["Method","degrees_of_freedom"::integer,
  "Some of the evaluation points lie in the interior of $F_{16}$ and so have two degrees of freedom to move.  Some points may lie on an  edge or at a corner, so that they have one or zero degrees of freedom.  This methd returns the total number of degrees of freedom for all evaluation points.",
  proc(this)
   local dof,i,P,c;

   dof := 0;
   for i from 0 to this["num_points"]-1 do
    P := eval(this["points"][i]);
    c := P["constraint"];
    if c = CONSTRAINT_FREE then
     dof := dof + 2;
    elif c < CONSTRAINT_FIXED then
     dof := dof + 1;
    fi;
   od;

   return dof;
  end
 ],

 ["Method","choose_random_points"::void,
  "This method randomly chooses $n$ evaluation points in the interior of $F_{16}$, and gives them all the same weight.",
  proc(this,n)
   local i,P;

   this["num_points"] := n;
   this["points"] := table();
   this["weights"] := table();

   for i from 0 to n-1 do
    P := `new/E_point`();
    P["set_random"];
    this["points"][i] := eval(P): 
    this["weights"][i] := 1.171882066198705389190745405111444384076232479768421659034323598323228171698800702578886902713314708/n;
   od:

   this["set_monomial_values"];
   this["set_eval_det"];
   NULL;
  end
 ],

 ["Method","adjust"::void,"",
  proc(this,t,offset_)
   local i,j,P,c;

   i := `if`(nargs > 2,offset_,0);
   for j from 0 to this["num_points"]-1 do
    P := eval(this["points"][j]);
    c := P["constraint"];
    if c = CONSTRAINT_FREE then
     P["adjust",t[i],t[i+1]];
     i := i+2;
    elif c < CONSTRAINT_FIXED then
     P["adjust",t[i]];
     i := i+1;
    fi;
   od;

   this["set_monomial_values"];
   NULL;
  end
 ],

 ["Method","adjust_randomly"::void,"",
  proc(this,scale_)
   local scale,r,i,t;
   scale := `if`(nargs > 1, scale_, 0.1);
   r := () -> 0.001 * rand(-1000..1000)();
   t := [seq(scale * r(),i=1..this["degrees_of_freedom"])];
   this["adjust",t,1];
  end
 ],

 ["Method","add_new_point","Add a new evaluation point with the specified coordinates",
  proc(this,x0::[scalar,scalar,scalar,scalar])
   local k,P;

   P := `new/E_point`();
   P["set",CONSTRAINT_FREE,x0];
   k := this["num_points"];
   this["points"][k] := eval(P);
   this["weights"][k] := 0;
   this["num_points"] := k+1;
   this["set_monomial_values"];
   this["set_eval_det"];
  end
 ],

 ["Method","set_max_deg"::void,"Set the list of test monomials to be the list of all monomials of total degree less than or equal to the argument @max_deg@.  Also, calculate the corresponding test integrals using the @E_grid@ object supplied as the @grid@ argument.  (This may be slow.)",
  proc(this,max_deg,grid::E_grid)
   local i,j,k,m;

   this["test_monomials"] := table();
   this["test_integrals"] := table();
   k := 0;
   for m from 0 to max_deg do
    for i from 0 to m do
     j := m - i;
     this["test_monomials"][k] := z[1]^i * z[2]^j;
     userinfo(7,genus2,sprintf("Integrating %A",this["test_monomials"][k]));
     this["test_integrals"][k] := grid["int_z_by_table",this["test_monomials"][k]];
     k := k+1;
    od;
   od:
   this["num_test_monomials"] := k;
   this["set_monomial_values"];
   NULL;
  end
 ],

 ["Method","int_z"::scalar,"Integrate @f@, which should be an expression involving $z_1$ and $z_2$.",
  proc(this,f)
   local i,P,J;
   J := 0;
   for i from 0 to this["num_points"]-1 do
    P := eval(this["points"][i]);
    J := evalf(J + this["weights"][i] * subs({z[1]=P["z"][1],z[2]=P["z"][2]},f));
   od;
   return(J);
  end
 ],

 ["Method","set_monomial_values"::void,"Calculate the values and derivatives of all the test monomials at the evaluation points.",
  proc(this)
   local i,j,k,P,z1,z2,z1u,z1v,z2u,z2v,m,n,PP,Px,Pz,Pu,Pv,dzdx,xr,TMV,TMDV;

   PP := eval(this["points"]);
   n := this["num_points"];

   Px := [seq(PP[j]["x"],j=0..n-1)];
   Pz := [seq(PP[j]["z"],j=0..n-1)];
   Pu := [seq(PP[j]["u"],j=0..n-1)];
   Pv := [seq(PP[j]["v"],j=0..n-1)];
   dzdx := [seq([seq(diff(zx0[i],x[j]),j=1..4)],i=1..2)];

   TMV := table():
   TMDV := table():
   for j from 0 to n-1 do
    z1 := Pz[j+1][1];
    z2 := Pz[j+1][2];
    xr := {seq(x[i]=Px[j+1][i],i=1..4)};
    z1u := subs(xr,add(dzdx[1,k] * Pu[j+1][k],k=1..4));
    z1v := subs(xr,add(dzdx[1,k] * Pv[j+1][k],k=1..4));
    z2u := subs(xr,add(dzdx[2,k] * Pu[j+1][k],k=1..4));
    z2v := subs(xr,add(dzdx[2,k] * Pv[j+1][k],k=1..4));

    for i from 0 to this["num_test_monomials"]-1 do 
     m := this["test_monomials"][i];
     TMV[i,j] := evalf(subs({z[1]=z1,z[2]=z2},m));
     TMDV[i,j,0] := 
      evalf(subs({z[1]=z1,z[2]=z2},diff(m,z[1])*z1u + diff(m,z[2])*z2u));
     TMDV[i,j,1] := 
      evalf(subs({z[1]=z1,z[2]=z2},diff(m,z[1])*z1v + diff(m,z[2])*z2v));
    od;
   od;

   this["test_monomial_values"] := eval(TMV);
   this["test_monomial_diff_values"] := eval(TMDV);
   this["eval_det"] := NULL;
   this["eval_det_diff"] := NULL;
  end
 ],

 ["Method","solve_weights"::void,"Set the @weights@ field to minimize the mean square integration error for all the test monomials, subject to the constraint that all weights should be nonnegative.",
  proc(this)
   local n,m,i,j,TMV,TMVM,TI,TIV,WV;

   n := this["num_test_monomials"];
   m := this["num_points"];
   TMV := eval(this["test_monomial_values"]):
   TI := eval(this["test_integrals"]):
   TMVM := Matrix([seq([seq(TMV[i,j],j=0..m-1)],i=0..n-1)]);
   TIV := Vector([seq(TI[i],i=0..n-1)]);

   WV := LSSolve([TIV,TMVM],assume=nonnegative)[2];
   
   for j from 0 to m-1 do
    this["weights"][j] := WV[j+1];
   od;
   NULL;
  end
 ],

 ["Method","weight_list","Convert the @weights@ table to a list.",
  proc(this)
   [seq(this["weights"][i],i=0..this["num_points"]-1)];
  end
 ],

 ["Method","sorted_weight_list","Convert the @weights@ table to a list, and sort it.",
  proc(this)
   sort([seq(this["weights"][i],i=0..this["num_points"]-1)]);
  end
 ],

 ["Method","set_eval_det"::scalar,"Set the @eval_det@ field.",
  proc(this)
   local n,m,TMV;

   n := this["num_points"];
   m := this["num_test_monomials"];
   if m < n then return NULL; fi;

   TMV := eval(this["test_monomial_values"]);
   this["eval_det"] := 
    Determinant(Matrix([seq([seq(TMV[i,j],j=0..n-1)],i=0..n-1)]));
   return this["eval_det"];
  end
 ],

 ["Method","set_eval_det_diff","Set the @eval_det_diff@ field.  This method is quite slow.",
  proc(this)
   local i,j,k,n,M0,M1,T,TMV,TMDV;
   n := this["num_points"];
   TMV := eval(this["test_monomial_values"]);
   TMDV := eval(this["test_monomial_diff_values"]);
   M0 := Matrix([seq([seq(TMV[i,j],j=0..n-1)],i=0..n-1)]);
   M1 := Matrix([seq([seq(TMV[i,j],j=0..n-1)],i=0..n-1)]);
   T := table();
   for j from 0 to n-1 do # points
    if j > 0 then
     for i from 0 to n-1 do M1[i+1,j] := M0[i+1,j]; od;
    fi;
    for k from 0 to 1 do
     for i from 0 to n-1 do # monomials
      M1[i+1,j+1] := TMDV[i,j,k];
     od;
     T[j,k] := Determinant(M1);
    od;
   od;
   this["eval_det_diff"] := eval(T);
   return(eval(T));
  end
 ],

 ["Method","increase_eval_det_once","Adjust the evaluation points to increase @eval_det@.  This assumes that @eval_det@ has already been calculated, but not @eval_det_diff@.  It calculates @eval_det_diff@ for the unadjusted points, but does not recalculate it after performing the adjustment.",
  proc(this,step)
   local n,d,m,i,j,k,r,R,t,T,c,sgn;

   n := this["num_points"];
   d := this["degrees_of_freedom"];
   m := this["num_test_monomials"];
   if (this["eval_det"] = NULL) then 
    this["set_eval_det"];
   fi;
   sgn := signum(this["eval_det"]);

   t := table();
   T := eval(this["set_eval_det_diff"]):
   R := 0;
   k := 0;
   for i from 0 to n-1 do
    c := this["points"][i]["constraint"];
    r := 0;
    if c < CONSTRAINT_FIXED then
     t[k] := T[i,0];
     r := abs(t[k]);
     k := k + 1;
     if c = CONSTRAINT_FREE then
      t[k] := T[i,1];
      r := sqrt(t[k-1]^2+t[k]^2);
      k := k + 1;
     fi;
    fi;
    R := max(R,r);
   od;

   for i from 0 to d-1 do
    t[i] := t[i] * step/R;
   od;
   this["eval_det_diff"] := NULL;

   this["adjust",t];
   this["set_eval_det"];
  end
 ],

 ["Method","increase_eval_det","",
  proc(this,step_,num_steps_)
   local step,num_steps,d0,d1,i;

   step  := `if`(nargs > 1,step_,0.1);
   num_steps := `if`(nargs > 2,num_steps,10);
   if (this["eval_det"] = NULL) then 
    this["set_eval_det"];
   fi;
   d0 := abs(this["eval_det"]);

   for i from 1 to num_steps do
    d1 := abs(this["increase_eval_det_once",step]);
    userinfo(7,genus2,sprintf("log(abs(det)) = %.3f",log[10](d1)));
    if d1 < d0 then
     step := step/2;
     userinfo(7,genus2,sprintf("reducing step size to %A",step));
    fi;
    d0 := d1;
   od;

   this["eval_det"];
  end
 ],

 ["Method","set_errors","Calculate all the errors obtained when integrating all the test monomials using this quadrature rule.  This assumes that correct values for all the integrals have already been stored in the @test_integrals@ field. ",
  proc(this)
   local n,m,i,E,J,TI,TMV,W;

   n := this["num_points"];
   m := this["num_test_monomials"];
   TI := eval(this["test_integrals"]);
   TMV := eval(this["test_monomial_values"]);
   W := eval(this["weights"]);

   E := table();
   for i from 0 to m-1 do
    E[i] := TI[i] - add(W[j] * TMV[i,j],j=0..n-1);
   od;

   this["errors"] := eval(E);
   this["total_square_error"] := add(E[i]^2,i=0..m-1);
  end  
 ],

 ["Method","reduce_errors_once","This adjusts the evaluation points and weights to attempt to reduce the integration errors.  We have not made much use of this; instead, we have used @increase_eval_det@ repeatedly, followed by @solve_weights@.",
  proc(this,max_step_)
   local n,d,m,i,j,k,a,B,u,v,c,max_step,TI,TMV,TMDV,W;

   max_step := `if`(nargs > 1, max_step_, 0.1);
   n := this["num_points"];
   d := this["degrees_of_freedom"];
   m := this["num_test_monomials"];
   this["set_errors"];
   a := Transpose(Vector(m));
   B := Matrix(m,d+n);

   TI := eval(this["test_integrals"]);
   TMV := eval(this["test_monomial_values"]);
   TMDV := eval(this["test_monomial_diff_values"]);
   W := eval(this["weights"]);
   
   for i from 0 to m-1 do
    a[i+1] := this["errors"][i];
    k := 0;
    for j from 0 to n-1 do
     c := this["points"][j]["constraint"];
     B[i+1,j    +1] := TMV[i,j];
     if c < CONSTRAINT_FIXED then
      B[i+1,k+n  +1] := W[j] * TMDV[i,j,0];
      k := k + 1;
      if c = CONSTRAINT_FREE then
       B[i+1,k+n  +1] := W[j] * TMDV[i,j,1];
       k := k + 1;
      fi;
     fi;
    od;
   od;
   u := a . B;
   c := (a . Transpose(a))/(2 * u . Transpose(u));
   v := c * u;
#   v := MatrixInverse(Transpose(B).B,method = pseudo) . (a . B);
   if Norm(v,2) > max_step then
    v := max_step * v / Norm(v,2);
   fi;
   for i from 0 to n-1 do
    this["weights"][i] := this["weights"][i] + v[i+1];
   od;

   this["adjust",v,n+1];
   this["set_monomial_values"];
   this["set_errors"];
  end
 ],

 ["Method","reduce_errors","",
  proc(this,max_step_,num_steps_)
   local max_step,num_steps,e0,e1,i;

   max_step  := `if`(nargs > 1,max_step_,0.1);
   num_steps := `if`(nargs > 2,num_steps,10);
   e0 := this["set_errors"];

   for i from 1 to num_steps do
    e1 := this["reduce_errors_once",max_step];
    userinfo(7,genus2,sprintf("log(error) = %.3f",log[10](e1)));
    if e1 > e0 then
     max_step := max_step/2;
     userinfo(7,genus2,sprintf("reducing step size to %4f",max_step));
    fi;
    e0 := e1;
   od;

   e1;
  end
 ],

 ["Method","curvature_error","If the integration rule is accurate, then the result of this method should be zero, by the Gauss-Bonet Theorem.",
  proc(this)
   evalf(Pi/4 + this["int_z",curvature_z0(z)]);
  end
 ],

 ["Method","log_curvature_error","",
  proc(this)
   return log[10](abs(this["curvature_error"]));
  end
 ],

 ["Method","stokes_error","Here @ff@ should be a list $(f_1,f_2)$ of two expressions in $z_1$ and $z_2$.  The method returns the approximated integral of $d(f_1\\alpha_1+f_2\\alpha_2)$, where the forms $\\alpha_i$ are defined in @embedded/roothalf/forms.mpl@.  If the integration rule is accurate, then the result should be zero, by Stokes's Theorem.  The formulae used are only valid when $a=1/\\sqrt{2}$, so the method gives an error in other cases.",
  proc(this,ff)
   if `class/E_point`["StaticFieldValue"]["a"] <> 1/sqrt(2) then
    error("Method is only valid for EX^*");
   fi;
   
   this["int_z",stokes_alpha(ff)];
  end
 ],

 ["Method","max_stokes_error","",
  proc(this)
   local i,m,TM,NM,errs;
   
   TM := this["test_monomials"];
   m := this["num_test_monomials"];
   NM := table();
   for i from 0 to m-1 do
    NM[i] := sqrt(this["int_z",TM[i]^2]);
   od:
   
   errs := [
    seq(this["stokes_error",[TM[i],0]]/NM[i],i=0..m-1),
    seq(this["stokes_error",[0,TM[i]]]/NM[i],i=0..m-1)
   ];

   return(max(map(abs,errs)));
  end
 ],
 
 ["Method","log_max_stokes_error","",
  proc(this)
   return log[10](abs(this["max_stokes_error"]));
  end
 ],

 ["Method","plot_z","This generates a plot showing the location of the evaluation points in the $z$-plane.",
  proc(this)
   load_plot("z_proj_F16_bare");
   display(pics["z_proj_F16_bare"],seq(point(this["points"][i]["z"]),i=0..this["num_points"]-1));
  end
 ],

 ["Method","error_plot","Generate a plot showing $\\log_{10}$ of the relative errors $|Q(m)-\\int m|/\\|m\\|_2$, as $m$ runs through the test monomials.",
  proc(this)
   local i,m,TI,TM,errs;
   
   TM := this["test_monomials"];
   TI := this["test_integrals"];
   m := this["num_test_monomials"];
   errs := [seq((this["int_z",TM[i]]-TI[i])/sqrt(this["int_z",TM[i]^2]),i=0..m-1)];
   listplot(map(log[10],map(abs,errs)),style=point);
  end
 ],

 ["Method","describe","",
  proc(this)
   local d,s,nn,nz;

   d := max(map(degree,subs(z[2]=z[1],map(op,[entries(this["test_monomials"])])),z[1]));
   s := sprintf("This is a quadrature rule with %d points and %d test monomials of maximum degree %d.  ",this["num_points"],this["num_test_monomials"],d);
   nz := nops(select(w -> evalf(w) = 0.,this["weight_list"]));
   nn := nops(select(w -> evalf(w) < 0.,this["weight_list"]));
   if nn = 0 then
    if nz = 0 then
     s := cat(s,"All weights are strictly positive.  ");
    elif nz = 1 then
     s := cat(s,"There is one point with weight zero, and all other points have positive weight.  ");
    else  
     s := cat(s,sprintf("There are %d points with weight zero, and all other points have positive weight.  ",nz));
    fi;
   elif nn = 1 then
    if nz = 0 then
     s := cat(s,"There is one point with negative weight, and all other weights are strictly positive.  ");
    elif nz = 1 then
     s := cat(s,"There is one point with negative weight and one with weight zero, but all other points have positive weight.  ");
    else  
     s := cat(s,sprintf("There is one point with negative weight and %d points with weight zero, and all other points have positive weight.  ",nz));
    fi;
   else
    if nz = 0 then
     s := cat(s,sprintf("There are %d points with negative weight, and all other weights are strictly positive.  ",nn));
    elif nz = 1 then
     s := cat(s,sprintf("There are %d points with negative weight and one with weight zero, but all other points have positive weight.  ",nn));
    else  
     s := cat(s,sprintf("There are %d points with negative weight and %d points with weight zero, and all other points have positive weight.  ",nn,nz));
    fi;
   fi;
  
   if type(this["eval_det"],numeric) then
    s := cat(s,sprintf("The log of the evaluation determinant is %.3f.  ",log[10](abs(this["eval_det"]))));
   fi;

   return s;
  end
 ]
):
