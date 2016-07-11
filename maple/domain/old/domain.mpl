# This file sets up a common framework for computation with fundamental
# domains in several different types of precromulent surface.  A "domain"
# will mean a Maple table X whose entries encode information about a 
# specific surface.  Everything is set up in a way that allows for 
# simple translation to C++, so various aspects are somewhat unnatural 
# from a pure Maple point of view.

# There is very little code in this file; mostly, it just serves to 
# document the framework.  Specific domains are defined in the files
# maple/embedded/EX_domian, maple/projective/PX_domain and 
# maple/hyperbolic/HX_domain.

# Some entries in the table X are as follows.
#
# X["type"] should be a string indicating the general type of the 
#   domain: "E", "H" or "P" for the embedded, hyperbolic and 
#   projective families.
#
# X["a"] is the parameter a such that X is EX(a) or HX(a) or PX(a).
#
# X["dim"] should be an integer n such that the domain can be considered
#   as a subspace of R^n; so n=4 in the embedded and projective cases,
#   and n=2 in the hyperbolic case.
#
# X["new_point"] should be a function that returns a table representing 
#   a point of the domain together with various auxiliary information,
#   in a format that will be discussed later.  The arguments are (c,x),
#   where c is an integer an x is a list of floating point numbers.
#   Here c should be one of the constraint constants listed below, which
#   specifies whether the point is one of the vertices v[0], v[3], v[6] 
#   or v[11], or whether it lies on one of the edges C[1], C[1], C[3]
#   or C[5].  The argument x just gives the coordinates of the point.
#   Both arguments are optional; if they are absent, then p is set to v[0].
#
# X["set_point"](p,c,x) is another function.  Here p is expected to be
#   a (table representing a) point of the domain, and c and x are as for
#   X["new_point"].  The function sets the constraint and coordinates of
#   p to c and x and then recalculates all auxiliary information.
#
# X["set_C0"](p,t) sets p to be the point on C0 with parameter t.  Here
#   we use a parametrisation such that t=0 at v[6] (the bottom left
#   corner when F16 is drawn in the standard way) and t=1 at v[3] 
#   (the top left).  There are similar functions X["set_C1"], X["set_C3"]
#   and X["set_C5"], with parameters running from left to right or from
#   bottom to top in all cases.
#
# X["set_V0"](p) sets p to the vertex v[0], and similarly for the functions
#   X["set_V3"], X["set_V6"] and X["set_V11"].
#
# X["fix"](p) makes various adjustments to p.  It corrects the coordinates
#   if necessary so that any equalities or inequalities defining the domain
#   are satisfied as exactly a possible given the working precision, and 
#   similarly for any additional equalities required for the point to lie
#   on C[0], C[1], C[3] or C[5], if required.  It also sets various
#   auxiliary quantities.  Most of the work in the X["set_*"] functions is
#   actually performed by calling X["fix"].
#
# X["adjust"](p,s,t) moves p by a displacement depending on s and t.
#   Part of the auxiliary data attached to p consists of vectors u and v
#   that form an oriented orthonormal basis for the tangent space.  If p
#   is constrained to lie on one of the curves C[i], then u will be tangent
#   to C[i].  If p is unconstrained, then this function will move p by a 
#   vector s u + t v, then apply X["fix"]() to clean up the result.  If
#   p is constrained to lie on C[i], then at the first step we just move
#   p by s u.  If p is constrained to lie at a vertex, then X["adjust"]
#   does nothing.
#
# X["midpoint"](p,q) creates and returns a new point roughly half way 
#   between p and q.  X["midpoint4"](p,q,r,s) similarly creates a point
#   close to (p + q + r + s)/4.
#
# X["orientation"](a,b,c) returns +1 or -1 depending on whether the 
#   points a,b and c rotate anticlockwise or clockwise about their midpoint.
#
# X["clone"](p) creates and returns a copy of p (which can be changed
#    without affecting the original p).
#
# X["equal"](p,q) tests whether p and q have the same coordinates.
#
# X["print"](p) prints a description of p.
#
# X["check"](p) returns a floating point number which is always nonnegative,
#   and should be exactly zero if p lies in the domain and all auxiliary 
#   data is correct.  In practice it will usually be nonzero but tiny,
#   because of rounding errors.

CONSTRAINT["FREE"]  := 0:
CONSTRAINT["C1"]    := 2:
CONSTRAINT["C3"]    := 4:
CONSTRAINT["C5"]    := 5:
CONSTRAINT["C0"]    := 8:
CONSTRAINT["FIXED"] := 15:
CONSTRAINT["V0"]    := 15:
CONSTRAINT["V3"]    := 31:
CONSTRAINT["V6"]    := 47:
CONSTRAINT["V11"]   := 63:

constraint_name := table();

proc()
 local i,n;
 global constraint_name;

 for i in map(op,[indices(CONSTRAINT)]) do 
  n := cat("CONSTRAINT_",i);
  assign(convert(n,name),CONSTRAINT[i]);
  constraint_name[CONSTRAINT[i]] := i;
 od:
end():

curve_index_by_constraint := table();
curve_index_by_constraint[CONSTRAINT_FREE]  := NULL;
curve_index_by_constraint[CONSTRAINT_C1]    := 1;
curve_index_by_constraint[CONSTRAINT_C3]    := 3;
curve_index_by_constraint[CONSTRAINT_C5]    := 5;
curve_index_by_constraint[CONSTRAINT_C0]    := 0;
curve_index_by_constraint[CONSTRAINT_FIXED] := NULL;
curve_index_by_constraint[CONSTRAINT_V0]    := NULL;
curve_index_by_constraint[CONSTRAINT_V3]    := NULL;
curve_index_by_constraint[CONSTRAINT_V6]    := NULL;
curve_index_by_constraint[CONSTRAINT_V11]   := NULL;

constraint_table := [
 [CONSTRAINT_V6,CONSTRAINT_C1,CONSTRAINT_V0],
 [CONSTRAINT_C0,CONSTRAINT_FREE,CONSTRAINT_C5],
 [CONSTRAINT_V3,CONSTRAINT_C3,CONSTRAINT_V11]
];

constraint_for_square := proc(x,y)
 local i,j;

 i := `if`(x = 0,1,`if`(x = 1,3,2));
 j := `if`(y = 0,1,`if`(y = 1,3,2));
 return constraint_table[j][i];
end;
