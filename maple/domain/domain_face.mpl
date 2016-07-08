#@ CLASS: domain_face

`Class/Declare`(
 "domain_face",
 "An instance represents a triangular face in a domain.",
 
 ["Field","grid_index"::integer = 0,
  "In many cases faces will form part of a grid, and all the faces in the grid will be numbered from 0 to n-1 for some n.  The index number will be stored in this field."
 ],

 ["Field","corner"::table,
  "This is a table with indices 0, 1 and 2; the entries are instances of an appropriate subclass of @domain_point@, representing the three corners of the face."
 ],

 ["Field","side"::table,
  "This is a table with indices 0, 1 and 2; the entries are instances of an appropriate subclass of @domain_edge@, representing the three sides of the face.  side[0] should join corner[0] to corner[1], and side[1] should join corner[0] to corner[2], and side[2] should join corner[1] to corner[2].  In more detail, end[0] of side[0] should be the same Maple object as corner[0], so that changing the fields of end[0] of side[0] automatically changes the fields of corner[0], and similarly in the other cases; it is not enough for end[0] of side[0] to be a different object with the same coordinates."
 ],

 ["Field","orientation"::integer = 1,
  "This should be +1 if circuit corner[0] -> corner[1] -> corner[2] -> corner[0] runs anticlockwise (with respect to the given orientation of the domain), and -1 otherwise."
 ],

 ["Field","flat_area"::scalar = 0,
  "This is the area of the affine triangle with the same vertices as this face."
 ],

 ["Constructor",
  "Construct a new face with sides @S0@, @S1@ and @S2@.  It is assumed that the endpoints of these sides match up correctly.",
  proc(this,S0::domain_edge,S1::domain_edge,S2::domain_edge)
   this["corner"] := table();
   this["side"] := table();
   this["set",S0,S1,S2];
  end
 ],

 ["Method","set"::void,
  "Set the sides of this face to be @S0@, @S1@ and @S2@, and set the corners and other auxiliary quantities accordingly.  If the argument @s_@ is supplied, then it will be used as the orientation; otherwise the orientation will be calculated by a method that is valid provided that the face is not too strongly curved.  It is assumed that the endpoints of @S0@, @S1@ and @S2@ match up correctly; this can be checked by the @check_matching@ method if necessary.",
  proc(this,S0::domain_edge,S1::domain_edge,S2::domain_edge,s_)

   this["corner"][0] := eval(S0["end"][0]);
   this["corner"][1] := eval(S0["end"][1]);
   this["corner"][2] := eval(S1["end"][1]);

   this["side"][0] := eval(S0);
   this["side"][1] := eval(S1);
   this["side"][2] := eval(S2);

   if nargs >= 5 and s_ <> 0 then 
    this["orientation"] := s_;
   else 
    this["set_orientation"];
   fi;

   this["calculate"];
   NULL;
  end
 ],

 ["Method","set_orientation"::void,
  "Set the orientation,  by a method that is valid provided that the face is not too strongly curved.",
  proc(this)
   local x01,x02,u,v,s;

   u := this["corner"][0]["u"];
   v := this["corner"][0]["v"];

   x01 := this["corner"][1]["x"] -~ this["corner"][1]["x"];
   x02 := this["corner"][2]["x"] -~ this["corner"][0]["x"];

   s := round(signum(dpv(x01,u) * dpv(x02,v) - dpv(x02,u) * dpv(x01,v)));
   this["orientation"] := s;

   return s;
  end
 ],

 ["Method","calculate"::void,
  "Calculate various auxiliary quantities such as the @flat_area@ field.  Subclasses may override this method to perform more interesting work.",
  proc(this)
   local q0,q1,q2,x01,x02,A;

   x01 := this["side"][0]["vector"];
   x02 := this["side"][1]["vector"];

   q0 := simp(dpv(x01,x01));
   q1 := simp(dpv(x02,x02));
   q2 := simp(dpv(x01,x02));

   A := simp(expand(q0*q1 - q2*q2));

   # If calculated exactly then A should be nonnegative.  However,
   # it might be tiny and negative due to rounding errors.
   if type(A,float) and A < 0 then A := 0; fi;

   this["flat_area"] := simp(sqrt(A)/2);
   NULL;
  end
 ],

 ["Method","corner_indices"::list(integer),
  "For an face in a grid, return the indices of the three corners.",
  proc(this)
   local C;
   C := this["corner"];
   [C[0]["grid_index"],C[1]["grid_index"],C[2]["grid_index"]];
  end
 ],
 
 ["Method","side_indices"::list(integer),
  "For an face in a grid, return the indices of the three sidess.",
  proc(this)
   local S;
   S := this["side"];
   [S[0]["grid_index"],S[1]["grid_index"],S[2]["grid_index"]];
  end
 ],
 
 ["Method","check_matching"::boolean,
  "Check whether the endpoints of the sides are the same as the corresponding corners.  They should be the same Maple objects, which can be tested by the built in equality operator.  However, one can also pass an equality checking procedure as a second argument to this method.",
  proc(this,eq_)
   local P0a,P0b,P0c,P1a,P1b,P1c,P2a,P2b,P2c,eq;

   if nargs > 1 then
    eq := eval(eq_);
   else
    eq := (Q0,Q1) -> evalb(Q0 = Q1);
   end;

   P0a := this["corner"][0];
   P0b := this["side"][0]["end"][0];
   P0c := this["side"][1]["end"][0];
   P1a := this["corner"][1];
   P1b := this["side"][0]["end"][1];
   P1c := this["side"][2]["end"][0];
   P2a := this["corner"][2];
   P2b := this["side"][1]["end"][1];
   P2c := this["side"][2]["end"][1];

   return eq(P0a,P0b) and eq(P0b,P0c) and
	  eq(P1a,P1b) and eq(P1b,P1c) and
	  eq(P2a,P2b) and eq(P2b,P2c);
  end
 ],

 NULL
);
