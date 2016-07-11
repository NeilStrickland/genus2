#@ CLASS: domain_edge

`Class/Declare`("domain_edge",
 "An instance of this class represents an edge in a domain.",

 ["Field","grid_index"::integer = 0, 
  "In many cases edges will form part of a grid, and all the edges in the grid will be numbered from 0 to n-1 for some n.  The index number will be stored in this field."
 ],

 ["Field","constraint"::integer = CONSTRAINT_FREE,
  "The @constraint@ field contains an integer encoding information about whether this edge (ignoring endpoints) is in the interior of the domain, or whether it lies along one of the sides.  We always refer to specific values by symbolic names such as CONSTRAINT_FREE or CONSTRAINT_C3 rather than bare integers.  For more details of the encoding, see the file domain.mpl.  Grids should be set up so that no edge has endpoints on two different sides of the domain."
 ],

 ["Field","curve_index" = NULL,
  "This field contains the same information as the @constraint@ field, but encoded differently.  It takes the value 0, 1, 3 or 5 if the edge lies along C[0], C[1], C[3] or C[5], and it is NULL if the edge does not lie along any of these boundary curves."
 ],

 ["Field","end"::table,
  "This is a table with indices 0 and 1; the entries are instances of an appropriate subclass of @domain_point@, representing the two endpoints of the edge.  We do not store any information about the precise path taken between the endpoints.  The domain may have methods that encode a canonical choice of path."
 ],

 ["Field","vector"::list(scalar),
  "This is the vector from end[0] to end[1]"
 ],

 ["Field","straight_length"::scalar,
  "This is the straight line distance from end[0] to end[1]"
 ],

 ["Field",
  "weighting"::scalar = 0,
  "This field is used when calculating the Dirichlet energy of a map between two domains.  Details will be explained elsewhere."
 ],

 ["Constructor",
  "Construct a new edge with endpoints @P0@ and @P1@.",
  proc(this,P0::domain_point,P1::domain_point)
   this["end"] := table();
   this["set",P0,P1];
  end
 ],

 ["Method","set"::void,
  "Set the endpoints to @P0@ and @P1@",
  proc(this,P0::domain_point,P1::domain_point)
   this["end"][0] := eval(P0);
   this["end"][1] := eval(P1);
   this["constraint"] := bitwise_and(P0["constraint"],P1["constraint"]);
   this["curve_index"] := curve_index_by_constraint[this["constraint"]];
   this["calculate"];
  end
 ],

 ["Method","calculate"::void,
  "Calculate various auxiliary quantities such as the @vector@ and @straight_length@ fields.  Subclasses may override this method to perform more interesting work.",
  proc(this)
   local v,t;

   v := this["end"][1]["x"] -~ this["end"][0]["x"];
   this["vector"] := v;
   this["straight_length"] := simp(sqrt(expand(add(t^2,t in v))));
   NULL;
  end
 ],

 ["Method","end_indices"::list(integer),
  "For an edge in a grid, return the indices of the two endpoints.",
  proc(this)
   local E;
   E := this["end"];
   [E[0]["grid_index"],E[1]["grid_index"]];
  end
 ],

 NULL
);

