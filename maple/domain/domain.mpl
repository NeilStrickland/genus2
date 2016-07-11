# In various places we consider points in the fundamental domain
# of a cromulent surface, which may be permitted to move.
# Some points may be constrained to lie on one of the four sides
# of the domain, which are the curves C[0], C[1], C[3] and C[5].
# Other points may be constrained to lie at one of the four
# corners, which are v[0], v[3], v[6] and v[11].  We use numerical
# codes for these cases, as follows:

#@ CONSTRAINT
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

# The following block sets things up so that constraint_name[31]
# (for example) is the string "CONSTRAINT_V3".

#@ constraint_name 
constraint_name := table():

proc()
 local i,n;
 global constraint_name;

 for i in map(op,[indices(CONSTRAINT)]) do 
  n := cat("CONSTRAINT_",i);
  assign(convert(n,name),CONSTRAINT[i]);
  constraint_name[CONSTRAINT[i]] := i;
 od:
end():

#@ curve_index_by_constraint 
curve_index_by_constraint := table():
curve_index_by_constraint[CONSTRAINT_FREE]  := NULL:
curve_index_by_constraint[CONSTRAINT_C1]    := 1:
curve_index_by_constraint[CONSTRAINT_C3]    := 3:
curve_index_by_constraint[CONSTRAINT_C5]    := 5:
curve_index_by_constraint[CONSTRAINT_C0]    := 0:
curve_index_by_constraint[CONSTRAINT_FIXED] := NULL:
curve_index_by_constraint[CONSTRAINT_V0]    := NULL:
curve_index_by_constraint[CONSTRAINT_V3]    := NULL:
curve_index_by_constraint[CONSTRAINT_V6]    := NULL:
curve_index_by_constraint[CONSTRAINT_V11]   := NULL:

#@ constraint_colour 
constraint_colour := table():
constraint_colour[CONSTRAINT_FREE]  := black:
constraint_colour[CONSTRAINT_C1]    := c_colour[1]:
constraint_colour[CONSTRAINT_C3]    := c_colour[3]:
constraint_colour[CONSTRAINT_C5]    := c_colour[5]:
constraint_colour[CONSTRAINT_C0]    := c_colour[0]:
constraint_colour[CONSTRAINT_FIXED] := black:
constraint_colour[CONSTRAINT_V0]    := black:
constraint_colour[CONSTRAINT_V3]    := black:
constraint_colour[CONSTRAINT_V6]    := black:
constraint_colour[CONSTRAINT_V11]   := black:

#@ constraint_table 
constraint_table := [
 [CONSTRAINT_V6,CONSTRAINT_C1,CONSTRAINT_V0],
 [CONSTRAINT_C0,CONSTRAINT_FREE,CONSTRAINT_C5],
 [CONSTRAINT_V3,CONSTRAINT_C3,CONSTRAINT_V11]
]:

#@ constraint_for_square 
constraint_for_square := proc(x,y)
 local i,j;

 i := `if`(x = 0,1,`if`(x = 1,3,2));
 j := `if`(y = 0,1,`if`(y = 1,3,2));
 return constraint_table[j][i];
end:

######################################################################

#@ CLASS: domain
`Class/Declare`("domain",
 "An instance represents the fundamental domain in a cromulent surface",
 
 ["Field","type"::string = "abstract",
  "A string describing the type of domain (e.g. @E@ for a member of the embedded family)"
 ],

 ["Field","dim"::posint,
  "All domains are expected to be subsets of $\\mathbb{R}^d$ for some integer $d$, stored in this field."
 ],

 ["Field","point_class_name",
  "Points in the domain will be represented by instances of a subclass of the abstract class @domain_point@.  The name of the relevant subclass is stored in this field, and the table representing the subclass is stored in the @point_class@ field.  Both fields should be set using the @set_point_class@ method.  Analogous comments apply to the fields @edge_class_name@, @edge_class@, @face_class_name@, @face_class@, @grid_class_name@ and @grid_class@."
 ],

 ["Field","point_class",
  ""
 ],

 ["Field","edge_class_name"],
 ["Field","edge_class"],
 ["Field","face_class_name"],
 ["Field","face_class"],
 ["Field","grid_class_name"],
 ["Field","grid_class"],

 ["Constructor","",
  proc(this)
   this["point_class_name"] := "domain_point";
   this["edge_class_name" ] := "domain_edge";
   this["face_class_name"]  := "domain_face";
   this["grid_class_name"]  := "domain_grid";
   this["point_class"] := eval(`class/domain_point`);
   this["edge_class" ] := eval(`class/domain_edge`);
   this["face_class"]  := eval(`class/domain_face`);
   this["grid_class"]  := eval(`class/domain_grid`);
  end
 ],

 ["Method","set_point_class"::void,"",
  proc(this,classname::string)
   this["point_class_name"] := classname;
   this["point_class"] := eval(convert(cat("class/",classname),name));
   NULL;
  end
 ],

 ["Method","set_edge_class"::void,"",
  proc(this,classname::string)
   this["edge_class_name"] := classname;
   this["edge_class"] := eval(convert(cat("class/",classname),name));
   NULL;
  end
 ],

 ["Method","set_face_class"::void,"",
  proc(this,classname::string)
   this["face_class_name"] := classname;
   this["face_class"] := eval(convert(cat("class/",classname),name));
   NULL;
  end
 ],

 ["Method","set_grid_class"::void,"",
  proc(this,classname::string)
   this["grid_class_name"] := classname;
   this["grid_class"] := eval(convert(cat("class/",classname),name));
   NULL;
  end
 ],

 ["Method","new_point"::void,
  "This method returns a new instance of the relevant point class.",
  proc(this)
   eval(this["point_class"]["FullConstructor"]());
  end
 ],

 ["Method","new_edge"::void,
  "This method returns a new instance of the relevant edge class.",
  proc(this,P0,P1)
   eval(this["edge_class"]["FullConstructor"](P0,P1));
  end
 ],

 ["Method","new_face"::void,
  "This method returns a new instance of the relevant face class.",
  proc(this,S0,S1,S2,s_)
   eval(this["face_class"]["FullConstructor"](args[2..-1]));
  end
 ],

 ["Method","new_grid"::void,
  "This method returns a new instance of the relevant grid class.",
  proc(this)
   local G;

   G := eval(this["grid_class"]["FullConstructor"]());
   G["domain"] := eval(this);
   return eval(G);
  end
 ],

 NULL
);

