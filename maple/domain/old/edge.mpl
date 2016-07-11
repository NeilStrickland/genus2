# This file sets up a framework for dealing with edges in a domain X,
# which will often be the fundamental domain of a cromulent surface.

# An edge will be represented by a table E with entries as follows.

# If E is part of a grid, then E["grid_index"] will be set to a natural 
# number which identifies the relevant edge.  These index numbers will
# start from 0, and the pair of indices for the endpoints of edge i
# will be lower in lexicographic order than the index pair for edge i+1.

# E["end",0] and E["end",1] will be the ends of the edge.  These are 
# themselves tables representing points on X, as discussed in domain.mpl.
# Note that internally Maple will just store references to these objects,
# not a complete copy.  Thus, a single point can be remembered as the
# endpoint of several different edges without wasting space.  If E is 
# part of a grid, then we will arrange that the index of E["end",0] is
# less than the index of E["end",1].

# For each domain, we will specify a rule that determines an actual 
# curve between the specified endpoints, say x0 and x1.
#
# + For the embedded family, we use the arcs determined by barycentric
#   coordinates.  This means that if y lies in the interior of the edge,
#   and w0 and w1 are the projections of x0 and x1 onto the tangent space
#   at y, then w0 and w1 will be negative real multiples of each other.
#
# + For the hyperbolic family, we can identify the fundamental domain 
#   with a subspace of the Poincare disc, and we use hyperbolic
#   geodesics.
#
# + We have not yet decided on a rule for the projective family.
#
# The rules are set up so that if x0 and x1 both lie on one of the
# boundary curves C[0], C[1], C[3] and C[5], then the edge will 
# run along that boundary curve.

# E["curve_index"] will be the number i in {0,1,3,5} such that 
# the edge runs along the boundary curve C[i], or NULL if there is
# no such curve.  E["constraint"] will be the corresponding constraint
# constant.

# E["vector"] will be a list of floating point numbers giving the vector
# from E["end",0] to E["end",1].  Moreover, E["straight_length"] will be
# set to the length of that vector.  Note that this may only be
# approximation to the geodesic distance along the surface.  Particular
# domains may define more accurate methods, which will set the entry
# E["length"] rather than E["straight_length"].

# E["weighting"] may be set to a floating point number used in calculating
# the Dirichlet energy of a map between domains.

# When E is initially created, the entries E["midpoint"], E["part",0] and
# E["part",1] will be null, and E["is_subdivided"] will be false.  If we 
# later decide to split E into two parts then E["midpoint"] will be set 
# to a newly constructed point in between the two ends, and E["part",0]
# will be set to a newly constructed edge running from E["part",0] to 
# E["midpoint"], and E["part",1] will be set to a newly constructed edge
# running from E["part",1] to E["midpoint"].  Moreover,
# E["is_subdivided"] will be set to true.

# Create and return a new edge running from P0 to P1.
new_edge := proc(P0,P1)
 local E;
 E := table();
 E["grid_index"] := NULL;
 E["is_subdivided"] := false;
 E["midpoint"] := null;
 E["part",0] := null;
 E["part",1] := null;
 E["weighting"] := 0;
 edge_set(E,P0,P1);
 return(eval(E));
end:

######################################################################
# Set the endpoints of E to P0 and P1
edge_set := proc(E,P0,P1)
 E["end",0] := eval(P0);
 E["end",1] := eval(P1);
 E["constraint"] := bitwise_and(P0["constraint"],P1["constraint"]);
 E["curve_index"] := curve_index_by_constraint[E["constraint"]];
 
 E["weighting"] := 0;
 edge_calculate(E);
 return(eval(E));
end:

######################################################################
# Calculate additional quantities
edge_calculate := proc(E)
 local v,t;

 v := E["end",1]["x"] -~ E["end",0]["x"];
 E["vector"] := v;
 E["straight_length"] := simp(sqrt(expand(add(t^2,t in v))));

 NULL;
end:

######################################################################

edge_end_indices := proc(E)
 [E["end",0]["grid_index"],
  E["end",1]["grid_index"]];
end:

######################################################################
# Split the edge in two.  The second argument is the domain in which the
# edge is supposed to lie.

edge_subdivide := proc(E,X)
 E["midpoint"] := X["midpoint"](E["end",0],E["end",1]);
 E["part",0] := new_edge(eval(E["end",0]),eval(E["midpoint"]));
 E["part",1] := new_edge(eval(E["end",1]),eval(E["midpoint"]));
 E["is_subdivided"] := true;

 NULL;
end:
