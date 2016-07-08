# This file sets up a framework for working with grids covering the fundamental
# domains of a cromulent surfaces.
#
# G["domain"] is a domain as discussed in domains.mpl
#
# G["num_points"], G["num_edges"] and G["num_faces"]
#   are the numbers of points, edges and faces. 
#
# G["num_vars"] is the number of degrees of freedom that we have to 
#   perturb the grid.  It counts 0 for each point that is constrained
#   to lie at one of the corner vertices, 1 for every point that is 
#   constrained to lie on one of the boundary curves, and 2 for all
#   other points.
#
# G["points"] is a table indexed by natural numbers, starting with 0.
#   Each entry is a table representing a point, as discussed in point.mpl.
#
# G["edges"] is a table indexed by natural numbers, starting with 0.
#   Each entry is a table representing an edge, as discussed in edge.mpl.
#   Edges can lie wholly in the interior of the domain, or they can have
#   one end on the boundary and the other in the interior, or they can be
#   contained in one of the four boundary curves.  However, they should
#   not bridge across with one end on one boundary curve and the other
#   end on a different boundary curve.
#
# G["faces"] is a table indexed by natural numbers, starting with 0.
#   Each entry is a table representing a face, as discussed in face.mpl.
#

new_grid := proc(X)
 local G;
 G := table();

 G["domain"] := eval(X);

 G["num_points"] := 0;
 G["num_edges" ] := 0;
 G["num_faces" ] := 0;
 G["num_vars"  ] := 0;

 G["points"] := table();
 G["edges" ] := table();
 G["faces" ] := table();
 
 G["edges_by_ends"] := table();
 G["faces_by_corners"] := table();

 G["total_flat_area"] := 0;
 G["tension"] := 0;

 return eval(G);
end:

######################################################################

grid_has_edge := proc(G,i,j)
 type(G["edges_by_ends"][i,j],table);
end:

######################################################################

grid_has_face := proc(G,i,j,k)
 type(G["faces_by_corners"][i,j,k],table);
end:

######################################################################

grid_edge_indices := proc(G)
 sort([indices(G["edges_by_ends"])]);
end:

######################################################################

grid_face_indices := proc(G)
 sort([indices(G["faces_by_corners"])]);
end:

######################################################################

grid_add_point := proc(G,P)
 local i;
 i := G["num_points"];
 G["num_points"] := i+1;
 P["grid_index"] := i;
 G["points"][i] := eval(P);
 return i;
end;

######################################################################

grid_add_new_point := proc(G,c,x)
 local X,P;

 X := eval(G["domain"]);
 P := X["new_point"](c,x);
 grid_add_point(G,eval(P));
end;

######################################################################

grid_add_edge := proc(G,E)
 local i,j,n;

 n := G["num_edges"];
 G["num_edges"] := n+1;
 E["grid_index"] := n;

 i,j := op(edge_end_indices(E));
 G["edges"][n] := eval(E);
 G["edges_by_ends"][i,j] := eval(E);

 return n;
end:

######################################################################

grid_add_new_edge := proc(G,i,j)
 local E,k;

 if i < 0 or i >= G["num_points"] or j < 0 or j >= G["num_points"] then
  error "Indices out of bounds";
 fi;

 if i >= j then 
  error "Indices out of order";
 fi;

 E := new_edge(eval(G["points"][i]),eval(G["points"][j]));
 grid_add_edge(G,eval(E));

 return eval(E);
end:

######################################################################

grid_add_face := proc(G,F)
 local n,i,j,k;

 n := G["num_faces"];
 i,j,k := op(face_corner_indices(F));

 F["grid_index"] := n;
 G["num_faces"] := n+1;
 G["faces"][n] := eval(F);
 G["faces_by_corners"][i,j,k] := eval(F);

 return n;
end:

######################################################################

grid_add_new_face := proc(G,i,j,k,s_)
 local s,F,S0,S1,S2;

 if i < 0 or i >= G["num_points"] or
    j < 0 or j >= G["num_points"] or 
    k < 0 or k >= G["num_points"] then
  error "Indices out of bounds";
 fi;

 if i >= j or j >= k then 
  error "Indices out of order";
 fi;

 if not(grid_has_edge(G,i,j)) then grid_add_edge(G,i,j); fi;
 if not(grid_has_edge(G,i,k)) then grid_add_edge(G,i,k); fi;
 if not(grid_has_edge(G,j,k)) then grid_add_edge(G,j,k); fi;

 S0 := eval(G["edges_by_ends"][i,j]);
 S1 := eval(G["edges_by_ends"][i,k]);
 S2 := eval(G["edges_by_ends"][j,k]);

 s := `if`(nargs >= 5, s_, 0);

 F := new_face(S0,S1,S2,s);
 grid_add_face(G,eval(F));

 return eval(F);
end:

######################################################################

grid_clone := proc(G)
 local G1,np,ne,nf,i,ij,ijk,F;

 G1 := new_grid(G["domain"]);
 
 np := G["num_points"];

 for i from 0 to np - 1 do
  G1["points"][i] := point_clone(G["points"][i]);
 od;

 G1["num_points"] := np;

 for ij in grid_edge_indices(G) do
  grid_add_new_edge(G1,op(ij));
 od;

 for ijk in grid_face_indices(G) do
  F := eval(G["faces_by_corners"][op(ijk)]);
  grid_add_new_face(G1,op(ijk),F["orientation"]);
 od;

 return eval(G1);
end:

######################################################################

grid_calculate := proc(G)
 local i,E,F;

 G["total_flat_area"] := 0;
 G["tension"] := 0;

 for i from 0 to G["num_edges"] - 1 do
  E := eval(G["edges"][i]);
  edge_calculate(E);
  G["tension"] := expand(G["tension"] + (0.25^2-E["straight_length"]^2)^2);
 od;

 for i from 0 to G["num_faces"] - 1 do
  F := eval(G["faces"][i]);
  face_calculate(F);
  G["total_flat_area"] := G["total_flat_area"] + F["flat_area"];
 od;

 NULL;
end:

######################################################################

grid_set_var_indices := proc(G)
 local i,nv,P;

 nv := 0;
 for i from 0 to G["num_points"] - 1 do
  P := eval(G["points"][i]);
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

 G["num_vars"] := nv;
end:
 
######################################################################
# This function sets the position P["x"] of each point P in G to 
# a symbolic (rather than numerical) value which may depend on 
# 0, 1 or 2 variables t[j].  Later we may calculate appropriate 
# values of the variables t[j] to minimise some objective function,
# and then substitute these values.

# Note that we avoid conflict with any other use of the symbol t by
# declaring t to be local to this procedure.  This means that t will
# still exist outside the procedure, and will be visually
# indistinguishable from the global symbol t, but will be different
# from it.  Thus, when minimising we need to specify G["vars"] as
# the list of variables to be solved for.

grid_raw_adjust := proc(G)
 local t,i,j,P;

 G["vars"] := {seq(t[i],i=0..G["num_vars"]-1)};

 for i from 0 to G["num_points"] - 1 do
  P := eval(G["points"][i]);
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
end:

######################################################################

grid_subdivide := proc(G) 
 local X,old_edges,old_faces,np,ne,nf,i,j,k,E,F;

 X := eval(G["domain"]);

 np := G["num_points"];
 ne := G["num_edges"];
 nf := G["num_faces"];

 old_edges := eval(G["edges"]);
 old_faces := eval(G["faces"]);
 G["edges"] := table();
 G["faces"] := table();
 G["num_edges"] := 0;
 G["num_faces"] := 0;

 for j from 0 to ne - 1 do;
  E := eval(old_edges[j]);
  edge_subdivide(E,X);
  grid_add_point(G,eval(E["midpoint"]));
  grid_add_edge(G,eval(E["part",0]));
  grid_add_edge(G,eval(E["part",1]));
 od;

 # We now add in all the subdivided faces.  Note that to keep everything
 # in lexicographic order, we add the three corner pieces for each of
 # the old faces before adding the centre pieces for any of the old faces.

 for k from 0 to nf - 1 do
  F := eval(old_faces[k]);
  face_subdivide(F);
  for i from 0 to 2 do
   grid_add_edge(G,eval(F["cut",i]));
  od;
  for i from 0 to 2 do
   grid_add_face(G,eval(F["part",i]));
  od;
 od;

 for k from 0 to nf - 1 do
  F := eval(old_faces[k]);
  grid_add_face(G,eval(F["part",3]));
 od;

 grid_set_var_indices(G);
 grid_calculate(G);
 NULL;
end:

######################################################################

grid_setup := proc(G,coords::list(list(list(numeric))))
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
   grid_add_new_point(G,c,coords[j+1][i+1]);
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

 for e in EI do grid_add_new_edge(G,op(e)); od;
 for f in FI do grid_add_new_face(G,op(f)); od;

 grid_set_var_indices(G);
 grid_calculate(G);
 NULL;
end:

######################################################################

grid_edge_plot := proc(G,proj_,opts__)
 local L,n,i,E,x0,x1,k,proj,opts;

 if nargs > 1 then proj := eval(proj_); else proj := (x -> x); fi;
 if nargs > 2 then opts := args[3..-1]; else opts := NULL; fi;
 
 L := NULL;
 n := G["num_edges"];
 for i from 0 to n-1 do
  E := G["edges"][i];
  x0 := proj(E["end",0]["x"]);
  x1 := proj(E["end",1]["x"]);
  k := E["curve_index"];
  if k <> NULL and member(k,{0,1,3,5}) then
   L := L,line(x0,x1,colour=c_colour[k]);
  else 
   L := L,line(x0,x1,colour=black);
  fi;
 od;

 display(L,scaling=constrained,axes=none,opts);
end: