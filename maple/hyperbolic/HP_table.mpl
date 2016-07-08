`Class/Declare`("HP_table",
 "An instance of this class encapsulates data about a family of cromulent isomorphisms $HX(a_H) \\to PX(a_P)$ for various values of $a_H$ and $a_P$",

 ["Field","H_to_P_maps"::table,"A table of objects of class @H_to_P_map@, indexed by values of @a_H@"],

 ["Field","P_to_H_maps"::table,"A table of objects of class @P_to_H_map@, indexed by values of @a_P@"],

 ["Field","H_to_P_num_samples"::posint = 200],
 ["Field","H_to_P_poly_deg"::posint = 20],
 ["Field","H_to_P_num_steps"::posint = 20],
 ["Field","P_to_H_poly_deg"::posint = 20],
 ["Field","P_to_H_num_charts"::posint = 10],
 ["Field","P_to_H_tolerance"::RR1 = 10.^(-20)],
 ["Field","P_to_H_gap"::RR1 = 0.02],

 ["Field","a_H_to_a_P_spline" = NULL],

 ["Constructor","",
  proc(this)
   this["H_to_P_maps"] := table();
   this["P_to_H_maps"] := table();
  end
 ],

 ["Method","a_H_indices","",
  proc(this)
   local T,L;
   T := eval(this["H_to_P_maps"]);
   L := sort(map(op,[indices(T)]));
   L := remove(a -> (T[a] = NULL),L);
   return L;
  end
 ],

 ["Method","a_P_indices","",
  proc(this)
   local T,L;
   T := eval(this["P_to_H_maps"]);
   L := sort(map(op,[indices(T)]));
   L := remove(a -> (T[a] = NULL),L);
   return L;
  end
 ],

 ["Method","a_H_a_P_pairs","",
  proc(this)
   local A_H;

   A_H := this["a_H_indices"];
   map(a -> [a,this["H_to_P_maps"][a]["a_P"]],A_H);
  end
 ],

 ["Method","a_P_a_H_pairs","",
  proc(this)
   local A_P;

   A_P := this["a_P_indices"];
   map(a -> [a,this["P_to_H_maps"][a]["a_H"]],A_P);
  end
 ],

 ["Method","set_spline","",
  proc(this)
   this["a_H_to_a_P_spline"] := 
    unapply(CurveFitting[Spline]([[0,1],op(HP_table["a_H_a_P_pairs"]),[1,0]],a_H),a_H):
  end
 ],

 ["Method","a_H_to_a_P","",
  proc(this,a)
   if this["a_H_to_a_P_spline"] = NULL then
    this["set_spline"];
   fi;
   this["a_H_to_a_P_spline"](evalf(a));
  end
 ],

 ["Method","a_P_to_a_H","",
  proc(this,a)
   local f,b;
   if this["a_H_to_a_P_spline"] = NULL then
    this["set_spline"];
   fi;
   f := eval(this["a_H_to_a_P_spline"]);
   return fsolve(f(b) = a,b);
  end
 ],

 ["Method","add_a_H","",
  proc(this,a0)
   local A_H,A_HL,A_HR,a_HL,a_HR,aL,aR,HP;

   userinfo(7,genus2,sprintf("Adding entry with a_H=%A",a0));

   A_H := this["a_H_indices"];
   A_HL := select(a -> (a < a0),A_H);
   A_HR := select(a -> (a > a0),A_H);

   if member(a0,A_H) then
    HP := eval(this["H_to_P_maps"][a0]);
    HP["make_samples",this["H_to_P_num_samples"]];
    HP["set_poly_deg",this["H_to_P_poly_deg"]];
   else
    HP := `new/H_to_P_map`():
    HP["set_a_H",a0];
    this["H_to_P_maps"][a0] := eval(HP);

    HP["make_samples",this["H_to_P_num_samples"]];
    HP["set_poly_deg",this["H_to_P_poly_deg"]];
    if A_HL <> [] then
     a_HL := max(A_HL);
     if A_HR <> [] then
      a_HR := min(A_HR);
      userinfo(7,genus2,sprintf("Interpolating between a_H=%A and a_H=%A",a_HL,a_HR));
      HP["a"] := this["H_to_P_maps"][a_HR]["a"];
      HP["fix_a"];
      aR := HP["a"];
      HP["a"] := this["H_to_P_maps"][a_HL]["a"];
      HP["fix_a"];
      aL := HP["a"];
      HP["a"] := ((a0 - a_HL) *~ aR +~
                  (a_HR - a0) *~ aL) /~ (a_HR - a_HL);
     else
      userinfo(7,genus2,sprintf("Extrapolating from a_H=%A",a_HL));
      HP["a"] := this["H_to_P_maps"][a_HL]["a"];
      HP["fix_a"];
     fi;
    else
     if A_HR <> [] then
      a_HR := min(A_HR);
      userinfo(7,genus2,sprintf("Extrapolating from a_H=%A",a_HR));
      HP["a"] := this["H_to_P_maps"][a_HR]["a"];
      HP["fix_a"];
     fi;
    fi;
   fi;

   HP["find_p1",this["H_to_P_num_steps"]];
   HP["set_p1_inv"];
   NULL;
  end
 ],

 ["Method","remove_a_H","",
  proc(this,a0)
   this["H_to_P_maps"][a0] := NULL;
  end
 ],

 ["Method","add_a_P","",
  proc(this,a0)
   local A_P,PH;

   userinfo(7,genus2,sprintf("Adding entry with a_P=%A",a0));

   A_P := this["a_P_indices"];

   if member(a0,A_P) then
    PH := eval(this["P_to_H_maps"][a0]);
    PH["degree"] := this["P_to_H_poly_deg"];
   else
    PH := `new/P_to_H_map`():
    PH["set_a_P",a0];
   fi;

   this["P_to_H_maps"][a0] := eval(PH);

   PH["add_charts",this["P_to_H_num_charts"]];
   PH["find_p1_inv",this["P_to_H_tolerance"],this["P_to_H_gap"]];   
  end
 ],

 ["Method","remove_a_P","",
  proc(this,a0)
   this["P_to_H_maps"][a0] := NULL;
  end
 ],

 ["Method","a_H_a_P_plot","",
  proc(this)
   local P;
   P := this["a_H_a_P_pairs"];
   if P = [] then
    return NULL;
   else
    return 
     display(
      map(u -> point([u[1],u[2]],colour=blue),P),
      view = [0..1,0..1]
     );
   fi;
  end
 ],

 ["Method","a_P_a_H_plot","",
  proc(this)
   local P;
   P := this["a_P_a_H_pairs"];
   if P = [] then
    return NULL;
   else
    return 
     display(
      map(u -> point([u[2],u[1]],colour=red),P),
      view = [0..1,0..1]
     );
   fi;
  end
 ],

 ["Method","spline_plot","",
  proc(this)
   display(
    plot(this["a_H_to_a_P_spline"](a),a=0..1,colour=grey),
    view = [0..1,0..1]
   );
  end
 ],

 ["Method","full_plot","",
  proc(this)
   display(
    this["spline_plot"],
    this["a_H_a_P_plot"],
    this["a_P_a_H_plot"],
    view = [0..1,0..1]
   );
  end
 ]
):
