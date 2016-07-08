# This file sets up a framework for dealing with triangular faces in a
# cromulent surface X.

# A face will be represented by a table F with entries as follows.

# If F is part of a grid, then F["grid_index"] will be set to a natural 
# number which identifies the relevant face.  These index numbers will
# start from 0, and the triple of indices for the corners of face i
# will be lower in lexicographic order than the index triple for face i+1.

# F["corner",0], F["corner",1] and F["corner",2] will be the corners
# of the face.  These are themselves tables representing points on X,
# as discussed in domain.mpl.  Note that internally Maple will just
# store references to these objects, not a complete copy.  Thus, a
# single point can be remembered as a corner of several different
# faces without wasting space.  If F is part of a grid, then we will
# arrange that the indices of E["corner",0], E["corner",1] and
# E["corner",2] are in increasing order.

# F["side",0], F["side",1] and F["side",2] will be the sides
# of the face.  These are themselves tables representing edges on X,
# as discussed in edge.mpl.  These are again references rather than
# complete copies.  They are numbered so that F["side",i] omits
# F["corner",2-i].  This will ensure that the indices of 
# F["side",0], F["side",1] and F["side",2] will be in increasing 
# order.

# F["orientation"] will be +1 if corners 0,1 and 2 are in anticlockwise
# order, or -1 if they are in clockwise order.

# F["flat_area"] will be the area of the affine triangle with the same
# corners as F.  This approximates the area of F itself.

# When F is first created, the entries F["part",i] (for 0 <= i <= 3)
# and F["cut",j] (for 0 <= j <= 2) will be null, and 
# F["is_subdivided"] will be false.  If we later decide to subdivide
# F, then F["cut",j] will be set to a newly created edge joining 
# the midpoints of the two edges other than F["side",2-j], or in other 
# words the midpoints of the two edges that meet at F["corner",j]. 
# Then F["part",j] (for 0 <= j <= 2) will be set to a newly created
# face containing F["cut",j] and F["corner",j], whereas F["part",3]
# will be set to a new face containing all three cuts.  Moreover,
# F["is_subdivided"] will be set to true.

######################################################################
# Create and return a new face with sides S0,S1 and S2 and
# orientation s_.

new_face := proc(S0,S1,S2,s_)
 local F,s;

 s := `if`(nargs >= 4, s_, 0);

 F := table();
 F["grid_index"] := null;
 F["is_subdivided"] := false;
 F["cut",0] := null;
 F["cut",1] := null;
 F["cut",2] := null;
 F["part",0] := null;
 F["part",1] := null;
 F["part",2] := null;
 F["part",3] := null;
 face_set(F,S0,S1,S2,s);
 return(eval(F));
end:

######################################################################
# Set the sides of F to S0, S1 and S2, and the orientation to s_.
# This only makes sense if there are some points P0, P1, P2 such
# that S0 runs from P0 to P1 and S1 from P0 to P2 and S2 from P1 to P2.
# If s_ is omitted or is zero then the orientation will be calculated
# by a  method that is valid when the face is reasonably flat.

face_set := proc(F,S0,S1,S2,s_) 
 local P0,P1,P2;

 F["corner",0] := S0["end",0];
 F["corner",1] := S0["end",1];
 F["corner",2] := S1["end",1];

 F["side",0] := S0;
 F["side",1] := S1;
 F["side",2] := S2;

 if nargs >= 4 and s_ <> 0 then 
  F["orientation"] := s_;
 else 
  face_set_orientation(F);
 fi;

 face_calculate(F);
end:

######################################################################

face_corner_indices := proc(F)
 [F["corner",0]["grid_index"],
  F["corner",1]["grid_index"],
  F["corner",2]["grid_index"]
 ];
end:

######################################################################
# This checks whether the sides and corners of the face F match up 
# correctly.  The second argument is a function which takes two points
# as arguments and returns true if they should be considered to be 
# equal.  Different equality tests may be relevant in different 
# contexts.

face_check_matching := proc(F,eq)
 local P0a,P0b,P0c,P1a,P1b,P1c,P2a,P2b,P2c;

 P0a := F["corner",0];
 P0b := F["side",0]["end",0];
 P0c := F["side",1]["end",0];
 P1a := F["corner",1];
 P1b := F["side",0]["end",1];
 P1c := F["side",2]["end",0];
 P2a := F["corner",2];
 P2b := F["side",1]["end",1];
 P2c := F["side",2]["end",1];

 return eq(P0a,P0b) and eq(P0b,P0c) and
        eq(P1a,P1b) and eq(P1b,P1c) and
        eq(P2a,P2b) and eq(P2b,P2c);
end:

face_check_matching_by_reference := proc(F)
 face_check_matching(F,(P,Q) -> evalb(P = Q));
end:

face_check_matching_by_index := proc(F)
 face_check_matching(F,(P,Q) -> evalb(P["grid_index"] = Q["grid_index"]));
end:

face_check_matching_by_distance := proc(F,tolerance)
 face_check_matching(F,(P,Q) -> evalb(distv(P["x"],Q["x"]) < tolerance));
end:

######################################################################
# The following function should work correctly provided that the 
# face is reasonably flat.

face_set_orientation := proc(F) 
 local x01,x02,u,v,s;

 u := F["corner",0]["u"];
 v := F["corner",0]["v"];

 x01 := F["corner",1]["x"] -~ F["corner",1]["x"];
 x02 := F["corner",2]["x"] -~ F["corner",0]["x"];

 s := signum(dpv(x01,u) * dpv(x02,v) - dpv(x02,u) * dpv(x01,v));
 F["orientation"] := s;

 return s;
end:

######################################################################
# This function just calculates the area of the face.  It assumes
# that edge_calculate() has already been called on all the edges.

face_calculate := proc(F)
 local q0,q1,q2,x01,x02,A;

 x01 := F["side",0]["vector"];
 x02 := F["side",1]["vector"];

 q0 := simp(dpv(x01,x01));
 q1 := simp(dpv(x02,x02));
 q2 := simp(dpv(x01,x02));

 A := simp(expand(q0*q1 - q2*q2));

 # If calculated exactly then A should be nonnegative.  However,
 # it might be tiny and negative due to rounding errors.
 if type(A,float) and A < 0 then A := 0; fi;

 F["flat_area"] := simp(sqrt(A)/2);
end:

######################################################################

face_subdivide := proc(F) 
 local s;

 if (not(F["side",0]["is_subdivided"])) then edge_subdivide(F["side",0]); fi;
 if (not(F["side",1]["is_subdivided"])) then edge_subdivide(F["side",1]); fi;
 if (not(F["side",2]["is_subdivided"])) then edge_subdivide(F["side",2]); fi;

 F["cut",0] := new_edge(eval(F["side",0]["midpoint"]),eval(F["side",1]["midpoint"]));
 F["cut",1] := new_edge(eval(F["side",0]["midpoint"]),eval(F["side",2]["midpoint"]));
 F["cut",2] := new_edge(eval(F["side",1]["midpoint"]),eval(F["side",2]["midpoint"]));

 s := F["orientation"];

 F["part",0] := new_face(eval(F["side",0]["part",0]),eval(F["side",1]["part",0]),eval(F["cut",0]), s);
 F["part",1] := new_face(eval(F["side",0]["part",1]),eval(F["side",2]["part",0]),eval(F["cut",1]),-s);
 F["part",2] := new_face(eval(F["side",1]["part",1]),eval(F["side",2]["part",1]),eval(F["cut",2]), s);
 F["part",3] := new_face(eval(F["cut",0]),eval(F["cut",1]),eval(F["cut",2]),-s);

 F["is_subdivided"] := true;
end:

