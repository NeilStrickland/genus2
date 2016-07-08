# This file sets up a framework for dealing with points in the fundamental 
# domain of a cromulent surface X.  Many features will depend on the
# structure of X, but this file only contains code that works in general.

# A point will be represented by a table P with entries as follows.

# If P is part of a grid, then P["grid_index"] will be set to a natural 
# number which identifies the relevant point.  These index numbers will
# start from 0.

# P["constraint"] will be a natural number which will be one of the constants
# CONSTRAINT_* defined in domain.mpl.  For points in the interior of the 
# fundamental domain, it will be CONSTRAINT_FREE.  For points on one of the
# boundary curves (but not at a corner) it will be CONSTRAINT_C0,
# CONSTRAINT_C1, CONSTRAINT_C3 or CONSTRAINT_C5.  For points at one of 
# the corners it will be CONSTRAINT_V0, CONSTRAINT_V3, CONSTRAINT_V6 or
# CONSTRAINT_V11.

# P["x"] will be a list of floating point real numbers which give the 
# coordinates of the point, under some specified embedding of the 
# surface in some R^n.  We will use the metric inherited from this 
# embedding.

# P["u"] and P["v"] will be an oriented orthonormal tangent frame at P.
# If P is constrained to lie on one of the curves C[i], then P["u"]
# will point along that curve.

# P["var_index"] is another natural number, whose meaning is as follows.
# We may want to adjust the positions of all the points in our grid
# slightly.  If P is constrained to lie at one of the vertices, then
# it will not move at all.  If P is constrained to lie on an edge, then
# it will move by r * P["u"] for some small number r.  It it is
# unconstrained then it will move by r * P["u"] + s * P["v"] for some
# small r and s.  However, we need consistent naming for the variables
# used to perturb all the different points.  We therefore take 
# r = t[i] (and s = t[i+1] if appropriate) for some number i which is
# saved as P["var_index"].  These numbers are set by the function
# grid_set_var_indices() defined in grid.mpl.  If P is a corner vertex
# then P["var_index"] is irrelevant and is set to NULL. 

new_point := proc(c,x,X)
 local P;

 P := table();

 P["grid_index"] := NULL;
 P["var_index"] := NULL;

 P["constraint"] := c;
 P["x"] := x;
 P["u"] := 0 *~ x;
 P["v"] := 0 *~ x;

 if (nargs > 2) then
  X["fix"](P);  
 fi;

 return eval(P);
end:

######################################################################

point_set := proc(P,c,x,X)
 P["constraint"] := c;
 P["x"] := x;
 P["u"] := 0 *~ x;
 P["v"] := 0 *~ x;

 if (nargs > 2) then
  X["fix"](P);  
 fi;

 return eval(P);
end:

######################################################################

point_adjust := proc(P,s,t,X)
 if P["constraint"] <> CONSTRAINT_FIXED then
  if P["constraint"] = CONSTRAINT_FREE then
   P["x"] := P["x"] +~ (s *~ P["u"]) +~ (t *~ P["v"]);
  else
   P["x"] := P["x"] +~ (s *~ P["u"]);
  fi;

  X["fix"](P);
 fi;

 return eval(P);
end:

######################################################################

point_midpoint := proc(P,Q,X)
 local R;

 R := new_point(bitwise_and(P["constraint"],Q["constraint"]),
                (P["x"] +~ Q["x"]) /~ 2);

 X["fix"](R);

 return eval(R);		
end:

######################################################################

point_midpoint4 := proc(P,Q,R,S,X)
 local c,x,T;

 c := bitwise_and(bitwise_and(P["constraint"],Q["constraint"]),
                  bitwise_and(R["constraint"],S["constraint"]));

 x := (P["x"] +~ Q["x"] +~ R["x"] +~ S["x"]) /~ 4;

 T := new_point(c,x);

 X["fix"](T);

 return eval(T);
end:

######################################################################

point_clone := proc(P)
 return copy(P);
end:
