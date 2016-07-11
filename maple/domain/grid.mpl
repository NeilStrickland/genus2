#@ CLASS: grid

`Class/Declare`("grid",
 "An instance of this class represents a triangulation of a domain.",

 ["Field","description"::string = ""],

 ["Field","domain"::domain,
  "This field should contain an instance of the @domain@ class (or a subclass), which specifies the domain on which this grid is based."
 ],

 ["Field","num_points"::integer = 0,"Number of points in the grid"],
 ["Field","num_edges"::integer = 0,"Nmber of edges in the grid"],
 ["Field","num_faces"::integer = 0,"Number of triangular faces in the grid"],
 ["Field","num_vars"::integer = 0,
  "Number of degrees of freedom to move the points while respecting the fact that some points are constrained to lie on a side of the domain or at a corner."
 ],

 ["Field","points"::table,
  "A table of points (represented by instances of the class #domain_point#), indexed by natural numbers starting with 0.  If @P=this[\"points\"][i]@, then @P[\"grid_index\"]@ should be set to @i@."
 ],

 ["Field","edges"::table,
  "A table of edges (represented by instances of the class #domain_edge#), indexed by natural numbers starting with 0.  If @E=this[\"edges\"][i]@, then @E[\"grid_index\"]@ should be set to @i@."
 ],

 ["Field","faces"::table,
  "A table of faces (represented by instances of the class #domain_face#), indexed by natural numbers starting with 0.  If @F=this[\"faces\"][i]@, then @F[\"grid_index\"]@ should be set to @i@."
 ],

 ["Field","edges_by_ends"::table,
  "This is a table containing the same objects as the @edges@ table, but indexed differently: if there is an edge @E@ from @this[\"points\"][i]@ to @this[\"points\"][j]@, then @this[\"edges_by_ends\"][i,j]@ will be equal to @E@.  This is set up automatically by the @add_edge@ method.  Note that edges are directed: we are assuming here that @E[\"end\"][0]=this[\"points\"][i]@ and @E[\"end\"][1]=this[\"points\"][j]@, and @this[\"edges_by_ends\"][j,i]@ (with @i@ and @j@ the wrong way around) will not be set."
 ],

 ["Field","faces_by_corners"::table,
  "This is a table containing the same objects as the @faces@ table, but indexed differently: if there is a face @F@ with corners @this[\"points\"][i]@, @this[\"points\"][j]@ and @this[\"points\"][k]@, then @this[\"faces_by_corners\"][i,j,k]@ will be equal to @F@.  This is set up automatically by the @add_face@ method."
 ],

 ["Constructor",
  "Construct a new grid with no points",
  proc(this,X_)
   this["points"] := table();
   this["edges"] := table();
   this["faces"] := table();
   this["edges_by_ends"] := table();
   this["faces_by_corners"] := table();

   if nargs > 1 then
    this["domain"] := eval(X_);
   fi;
  end
 ],

 ["Field","total_flat_area"::scalar = 0,
  "Total area of the simplicial complex whose 2-simplices are affine triangles with the same corners as the grid faces.  If the faces are small then this will be a reasonable approximation to the true area of the domain (but convergence is slow)."
 ],

 ["Field","tension"::scalar = 0,
  "This is a quantity depending on the edge lengths.  The idea is that minimising the tension should give a grid that is quite evenly spaced.  We have tried various different versions at different times."
 ],

 ["Method","has_edge"::boolean,
  "Returns true if there is an edge whose endpoints have indices @i@ and @j@.",
  proc(this,i,j) member([i,j],[indices(this["edges_by_ends"])]); end
 ],
 
 ["Method","has_face"::boolean,
  "Returns true if there is an face whose endpoints have indices @i@, @j@ and @k@.",
  proc(this,i,j,k) member([i,j,k],[indices(this["faces_by_corners"])]); end
 ],
 
 ["Method","edge_indices"::list(list(integer)),
  "Returns the list of pairs @[i,j]@ such that there is an edge from point @i@ to point @j@",
  proc(this) [indices(this["edges_by_ends"])]; end
 ],

 ["Method","face_indices"::list(list(integer)),
  "Returns the list of triples @[i,j,k]@ such that there is a face with corners point @i@, point @j@ and point @k@ (in order)",
  proc(this) [indices(this["faces_by_corners"])]; end
 ],

 ["Method","add_point"::integer,
  "Add a point (represented by an instance of #domain_point#) to the grid; return the index of the added point.",
  proc(this,P::domain_point)
   local i;
   i := this["num_points"];
   this["num_points"] := i+1;
   P["grid_index"] := i;
   this["points"][i] := eval(P);
   return i;
  end
 ],

 ["Method","add_new_point"::integer,
  "Create a new point with constraint @c@ and coordinates @x@, add it to the grid, and return the index.",
  proc(this,c::integer,x::list(scalar))
   local P;

   P := eval(this["domain"]["new_point"]);
   P["set",c,x];
   this["add_point",eval(P)];
  end
 ],

 ["Method","add_new_C_point"::integer,
  "Create a new point at $c_k(t)$, add it to the grid, and return the index.",
  proc(this,k::integer,t)
   local P;

   P := eval(this["domain"]["new_point"]);
   P["set_C",k,t];
   this["add_point",eval(P)];
  end
 ],

 ["Method","add_new_v_point"::integer,
  "Create a new point at $v_k$, add it to the grid, and return the index.",
  proc(this,k::integer)
   local P;

   P := eval(this["domain"]["new_point"]);
   P["set_v",k];
   this["add_point",eval(P)];
  end
 ],

 ["Method","add_edge",
  "Add an edge (represented by an instance of #domain_edge#) to the grid.  It is assumed that the endpoints of the edge have already been added.  Return the index of the added edge.",
  proc(this,E::domain_edge)
   local i,j,n;

   n := this["num_edges"];
   this["num_edges"] := n+1;
   E["grid_index"] := n;

   i,j := op(E["end_indices"]);
   this["edges"][n] := eval(E);
   this["edges_by_ends"][i,j] := eval(E);

   return n;
  end
 ],

 ["Method","add_new_edge",
  "Create a new edge from point @i@ to point @j@, add it to the grid, and return the index.",
  proc(this,i,j)
   local E,k,X;

   if i < 0 or i >= this["num_points"] or j < 0 or j >= this["num_points"] then
    error "Indices out of bounds";
   fi;

   if i >= j then 
    error "Indices out of order";
   fi;

   X := this["domain"];
   E := eval(X["new_edge",eval(this["points"][i]),eval(this["points"][j])]);
   this["add_edge",eval(E)];

   return eval(E);
  end
 ],

 ["Method","add_face",
  "Add a face (represented by an instance of #domain_face#) to the grid.  It is assumed that the corners and sides of the face have already been added.  Return the index of the added face.",
  proc(this,F::domain_face)
   local i,j,k,n;

   n := this["num_faces"];
   this["num_faces"] := n+1;
   F["grid_index"] := n;

   i,j,k := op(F["corner_indices"]);
   this["faces"][n] := eval(F);
   this["faces_by_corners"][i,j,k] := eval(F);

   return n;
  end
 ],

 ["Method","add_new_face",
  "Create a new face with corners at point @i@, point @j@ and point @k@.  Add the edges to the grid if this has not already been done, then add the face, and return the index of the new face.",
  proc(this,i,j,k,s_)
   local s,F,S0,S1,S2,X;

   if i < 0 or i >= this["num_points"] or
      j < 0 or j >= this["num_points"] or 
      k < 0 or k >= this["num_points"] then
    error "Indices out of bounds";
   fi;

   if i >= j or j >= k then 
    error "Indices out of order";
   fi;

   if not(this["has_edge",i,j]) then this["add_new_edge",i,j]; fi;
   if not(this["has_edge",i,k]) then this["add_new_edge",i,k]; fi;
   if not(this["has_edge",j,k]) then this["add_new_edge",j,k]; fi;

   S0 := eval(this["edges_by_ends"][i,j]);
   S1 := eval(this["edges_by_ends"][i,k]);
   S2 := eval(this["edges_by_ends"][j,k]);

   s := `if`(nargs >= 5, s_, 0);

   X := eval(this["domain"]);
   F := eval(X["new_face",S0,S1,S2,s]);
   this["add_face",eval(F)];
  end
 ],

 ["Method","clone"::domain_grid,
  "Return an independent copy of this grid, which can be changed without affecting the original.  The points, edges and faces of the new grid are also independent of the points, edges and faces of the old grid",
  proc(this)
   local G,np,ne,nf,i,ij,ijk,F,X;

   X := eval(this["domain"]);
   G := eval(X["new_grid"]);
   G["domain"] := eval(this["domain"]);

   np := this["num_points"];

   for i from 0 to np - 1 do
    G["points"][i] := G["points"][i]["clone"];
   od;

   G["num_points"] := np;

   for ij in this["edge_indices"] do
    G["add_new_edge",op(ij)];
   od;

   for ijk in this["face_indices"] do
    F := eval(this["faces_by_corners"][op(ijk)]);
    G["add_new_face",op(ijk),F["orientation"]];
   od;

   return eval(G);
  end
 ],

 ["Method","calculate",
  "Calculate various auxiliary quantities such as the @total_flat_area@ field.  Subclasses may override this method to perform more interesting work.",
  proc(this)
   local i,E,F;

   this["total_flat_area"] := 0;
   this["tension"] := 0;

   for i from 0 to this["num_edges"] - 1 do
    E := eval(this["edges"][i]);
    E["calculate"];
    this["tension"] := expand(this["tension"] + (0.25^2-E["straight_length"]^2)^2);
   od;

   for i from 0 to this["num_faces"] - 1 do
    F := eval(this["faces"][i]);
    F["calculate"];
    this["total_flat_area"] := this["total_flat_area"] + F["flat_area"];
   od;

   NULL;
  end
 ],

 ["Method","set_var_indices",
  "Set the @var_index@ fields of all the points in the grid.",
  proc(this)
   local i,nv,P;

   nv := 0;
   for i from 0 to this["num_points"] - 1 do
    P := eval(this["points"][i]);
    if P["constraint"] >= CONSTRAINT_FIXED then
     P["var_index"] := NULL;
    else
     P["var_index"] := nv;
     if P["constraint"] = CONSTRAINT_FREE then
      nv := nv + 2;
     else
      nv := nv + 1;
     fi;
    fi;
   od;

   this["num_vars"] := nv;
  end
 ],

 ["Method","raw_adjust",
  "This method sets the coordinates of all points in the grid to symbolic values involving variables @t[j]@.  The idea is that we can then solve for the values of these variables that minimise some objective function, and then move the grid points accordingly.  Note that in order to isolate the variables @t[j]@ from any existing meaning of the symbol @t@, we have declared @t@ to be a local variable in this procedure.  This creates a potentially confusing situation, because this local variable will exist outside the procedure and will be visually indistinguishable from the global symbol @t@.  In order to refer to the correct variables when solving, we need to use the @indets()@ function.",
  proc(this)
   local t,i,j,P;

   this["vars"] := {seq(t[i],i=0..this["num_vars"]-1)};

   for i from 0 to this["num_points"] - 1 do
    P := eval(this["points"][i]);
    j := P["var_index"];
    if P["constraint"] < CONSTRAINT_FIXED then
     if P["constraint"] = CONSTRAINT_FREE then
      P["x"] := P["x"] +~ t[j] *~ P["u"] +~ t[j+1] *~ P["v"];
     else
      P["x"] := P["x"] +~ t[j] *~ P["u"];
     fi;
    fi;
   od;

   NULL;
  end
 ],

 ["Method","subdivide"::void,
  "This method subdivides the grid by adding new points at the midpoints of all the original edges, and reconnecting everything in an obvious way.  The old information is destroyed by default, so one should use the @clone@ method first if one wants to retain it.",
  proc(this) 
   local X,np,ne,nf,old_edges,old_faces,
    midpoints,edge_parts,cuts,face_parts,E,P0,P1,Q,F,s,
    i,j,k,i0,i1,i2,j0,j1,j2;

   X := eval(this["domain"]);

   np := this["num_points"];
   ne := this["num_edges"];
   nf := this["num_faces"];

   old_edges := eval(this["edges"]);
   old_faces := eval(this["faces"]);
   this["edges"] := table();
   this["faces"] := table();
   this["num_edges"] := 0;
   this["num_faces"] := 0;

   midpoints  := table(); # Midpoints of old edges
   edge_parts := table(); # New edges that cover half of an old edge
   cuts       := table(); # New edges that cut across an old face
   face_parts := table(); # New faces that cover a quarter of an old face

   for j from 0 to ne - 1 do;
    E := eval(old_edges[j]);
    P0 := eval(E["end"][0]);
    P1 := eval(E["end"][1]);

    Q := eval(this["domain"]["new_point"]);
    Q["set",
      bitwise_and(P0["constraint"],P1["constraint"]),
      0.5 *~ (P0["x"] +~ P1["x"])
     ];

    midpoints[j] := eval(Q);
    edge_parts[j,0] := eval(X["new_edge",P0,Q]);
    edge_parts[j,1] := eval(X["new_edge",P1,Q]);

    this["add_point",eval(midpoints[j])];
    this["add_edge",eval(edge_parts[j,0])];
    this["add_edge",eval(edge_parts[j,1])];
   od;

   # We now add in all the subdivided faces.  Note that to keep everything
   # in lexicographic order, we add the three corner pieces for each of
   # the old faces before adding the centre pieces for any of the old faces.

   for k from 0 to nf - 1 do
    F := eval(old_faces[k]);
    j0 := F["side"][0]["grid_index"];
    j1 := F["side"][1]["grid_index"];
    j2 := F["side"][2]["grid_index"];
    i0 := old_edges[j0]["end"][0];
    i1 := old_edges[j0]["end"][1];
    i2 := old_edges[j2]["end"][1];

    cuts[k,0] := eval(X["new_edge",midpoints[j0],midpoints[j1]]);
    cuts[k,1] := eval(X["new_edge",midpoints[j0],midpoints[j2]]);
    cuts[k,2] := eval(X["new_edge",midpoints[j1],midpoints[j2]]);

    s := F["orientation"];

    face_parts[k,0] := eval(X["new_face",edge_parts[j0,0],edge_parts[j1,0],cuts[k,0], s]);
    face_parts[k,1] := eval(X["new_face",edge_parts[j0,1],edge_parts[j2,0],cuts[k,1],-s]);
    face_parts[k,2] := eval(X["new_face",edge_parts[j1,1],edge_parts[j2,1],cuts[k,2], s]);

    face_parts[k,3] := eval(X["new_face",cuts[k,0],cuts[k,1],cuts[k,2],-s]);

    for i from 0 to 2 do this["add_edge",cuts[k,i]]; od;
    for i from 0 to 2 do this["add_face",face_parts[k,i]]; od;
   od;

   for k from 0 to nf - 1 do
    this["add_face",face_parts[k,3]];
   od;

   this["set_var_indices"];
   this["calculate"];
   NULL;
  end
 ],

 ["Method","setup",
  "This method takes an argument @coords@ which should be a list of lists, all of the same length, giving a rectangular array.  Both dimensions of this array should be even.  Each element of the inner lists should itself be a list, giving coordinates of a point in the domain for this grid.  The @setup@ method creates a point for each entry in the array.  Points in the first row are constrained to lie on $C_1$, those in the last row are constrained to lie in $C_3$, those in the first column are constrained to lie on $C_0$, and those in the last column are constrained to lie on $C_5$.  The corners are constrained in the obvious way compatible with this.  Edges and faces are added in a slightly non-obvious pattern to ensure that no edge crosses a corner to join points on different sides of the domain. ",
  proc(this,coords::list(list(list(numeric))))
   local mm,nn,m,n,i,j,EI,FI,p,c,e,f;

   mm := nops(coords);
   nn := op({op(map(nops,coords))});
   if nops([nn]) <> 1 then
    error("Ragged coordinate table");
   fi;
   if not(type(nn,odd) and type(mm,odd)) then
    error("Dimensions not both odd");
   fi;

   m := (mm - 1)/2;
   n := (nn - 1)/2;
   p := (i,j) -> i + j*nn;
   for j from 0 to mm - 1 do
    for i from 0 to nn - 1 do
     c := constraint_for_square(i/(nn-1),j/(mm-1));
     this["add_new_point",c,coords[j+1][i+1]];
    od;
   od;

   FI := [
     seq(seq([p(i,j),p(i+1,j),p(i+1,j+1)],i=0..n-1),j=0..m-1),
     seq(seq([p(i,j),p(i,j+1),p(i+1,j+1)],i=0..n-1),j=0..m-1),
     seq(seq([p(2*n-i,j),p(2*n-i,j+1),p(2*n-i-1,j+1)],i=0..n-1),j=0..m-1),
     seq(seq([p(2*n-i,j),p(2*n-i-1,j),p(2*n-i-1,j+1)],i=0..n-1),j=0..m-1),
     seq(seq([p(i,2*m-j),p(i+1,2*m-j),p(i+1,2*m-j-1)],i=0..n-1),j=0..m-1),
     seq(seq([p(i,2*m-j),p(i,2*m-j-1),p(i+1,2*m-j-1)],i=0..n-1),j=0..m-1),
     seq(seq([p(2*n-i,2*m-j),p(2*n-i-1,2*m-j),p(2*n-i-1,2*m-j-1)],i=0..n-1),j=0..m-1),
     seq(seq([p(2*n-i,2*m-j),p(2*n-i,2*m-j-1),p(2*n-i-1,2*m-j-1)],i=0..n-1),j=0..m-1)   
   ];

   FI := sort(map(sort,FI));

   EI := map(F -> [[F[1],F[2]],[F[1],F[3]],[F[2],F[3]]],FI);
   EI := map(op,EI);
   EI := {op(EI)};
   EI := [op(EI)];
   EI := sort(EI);

   for e in EI do this["add_new_edge",op(e)]; od;
   for f in FI do this["add_new_face",op(f)]; od;

   this["set_var_indices"];
   this["calculate"];
   NULL;
  end
 ],

 ["Method","edge_plot",
  "This method returns a plot structure displaying the edges of the grid.  By default, it just uses the raw coordinates of the grid points (which will cause an error if the dimension is larger than three).  However, one can supply a map @proj_@ as an argument, and that map will then be applied to the coordinates before plotting.  Additional options for the @display()@ command can also be given as a second argument to the method.",
  proc(this,proj_,opts__)
   local L,n,i,E,x0,x1,k,proj,opts;

   if nargs > 1 then proj := eval(proj_); else proj := (x -> x); fi;
   if nargs > 2 then opts := args[3..-1]; else opts := NULL; fi;

   L := NULL;
   n := this["num_edges"];
   for i from 0 to n-1 do
    E := this["edges"][i];
    x0 := proj(E["end"][0]["x"]);
    x1 := proj(E["end"][1]["x"]);
    k := E["curve_index"];
    if k <> NULL and member(k,{0,1,3,5}) then
     L := L,line(x0,x1,colour=c_colour[k]);
    else 
     L := L,line(x0,x1,colour=black);
    fi;
   od;

   display(L,scaling=constrained,axes=none,opts);
  end
 ],

 NULL
);
