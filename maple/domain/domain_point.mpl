#@ CLASS: domain_point

`Class/Declare`("domain_point",
 "This is an abstract class which serves as the parent for various subclasses representing points in various different domains.",

 ["Field","grid_index"::integer = 0,
  "In many cases points will form part of a grid, and all the points in the grid will be numbered from $0$ to $n-1$ for some $n$.  The index number will be stored in this field."
 ],

 ["Field","var_index"::integer = 0,
  "In various situations we will want to adjust slightly the position of points in a grid.  Points in the interior of the domain have two degrees of freedom to move, but points on an edge have only one degree of freedom, and the corners have no freedom.  The adjustment will be governed by variables @t[0],..,t[m-1]@ say.  If this point is in the interior, then the first order adjustment will be @t[j] * u + t[j+1] * v@, where @u@ and @v@ are the vectors described below, and @j@ is the value of @var_index@.  If this point is on an edge, then the first order adjustment will be @t[j] * u@, where again @j@ is the value of @var_index@.  The @var_index@ field is not used for corner points."
 ],

 ["Field","constraint"::integer = CONSTRAINT_FREE,
  "The @constraint@ field contains an integer encoding information about whether this point is in the interior of the domain, or on an edge, or at a corner.  We always refer to specific values by symbolic names such as CONSTRAINT_FREE, CONSTRAINT_C3 or CONSTRAINT_V6 rather than bare integers.  For more details of the encoding, see the file domain.mpl."
 ],

 ["Field","x"::list,"Coordinates of the point"],
 ["Field","u"::list,"A unit vector tangent to the surface at the point.  If the point is constrained to lie on an edge, then @u@ should point along that edge."],
 ["Field","v"::list,"The vector obtained by rotating @u@ anticlockwise by a quarter turn."],

 ["Method","set"::void,
  "Set the constraint and coordinates, then calculate all other fields.  The calculation procedure may change the coordinates slightly if necessary to ensure that the point lies exactly on the surface and the specified constraints are satisfied.",
  proc(this,c::integer,x::list(scalar))
   this["constraint"] := c;
   this["x"] := x;
   this["fix"];
  end
 ],

 ["Method","fix"::void,
  "This is an abstract method, which should be overridden by subclasses.  The overriding method should change the coordinates slightly if necessary to ensure that the point lies exactly on the surface and the specified constraints are satisfied, then it should calculate @u@ and @v@ and possibly other information specific to the relevant domain.",
  proc(this)
   error("This is an abstract method, which should be overridden by subclasses."); 
  end
 ],

 ["Method","adjust"::void,
  "For a point in the interior, this method adds $su + tv$ to $x$ and then calls the @fix@ method.  Here $s$ and $t$ are parameters for this method, and $u$ and $v$ are the unit vectors stored in the @u@ and @v@ fields.  For an edge point we add $su$ instead of $su + tv$; for a corner point we do nothing.",
  proc(this,s::scalar,t::scalar)
   if this["constraint"] <> CONSTRAINT_FIXED then
    if this["constraint"] = CONSTRAINT_FREE then
     this["x"] := this["x"] +~ (s *~ this["u"]) +~ (t *~ this["v"]);
    else
     this["x"] := this["x"] +~ (s *~ this["u"]);
    fi;

    this["fix"];
   fi;
   NULL;
  end
 ],

 ["Method","set_C0"::void,
  "This is an abstract method, which should be overridden by subclasses.  The parameter $t$ should be between $0$ and $1$.  The overriding method should set all fields to represent a point on the left edge of the domain, with $t=0$ corresponding to the point $v_6$ at the bottom left, and $t=1$ corresponding to the point $v_3$ at the top left.",
  proc(this,t::scalar)
   error("This is an abstract method, which should be overridden by subclasses."); 
  end
 ],

 ["Method","set_C1"::void,
  "This is an abstract method, which should be overridden by subclasses.  The parameter $t$ should be between $0$ and $1$.  The overriding method should set all fields to represent a point on the bottom edge of the domain, with $t=0$ corresponding to the point $v_6$ at the bottom left, and $t=1$ corresponding to the point $v_0$ at the bottom right.",
  proc(this,t::scalar)
   error("This is an abstract method, which should be overridden by subclasses."); 
  end
 ],

 ["Method","set_C3"::void,
  "This is an abstract method, which should be overridden by subclasses.  The parameter $t$ should be between $0$ and $1$.  The overriding method should set all fields to represent a point on the top edge of the domain, with $t=0$ corresponding to the point $v_3$ at the top left, and $t=1$ corresponding to the point $v_{11}$ at the top right.",
  proc(this,t::scalar)
   error("This is an abstract method, which should be overridden by subclasses."); 
  end
 ],

 ["Method","set_C5"::void,
  "This is an abstract method, which should be overridden by subclasses.  The parameter t should be between $0$ and $1$.  The overriding method should set all fields to represent a point on the right edge of the domain, with $t=0$ corresponding to the point $v_0$ at the bottom right, and $t=1$ corresponding to the point $v_{11}$ at the top right.",
  proc(this,t::scalar)
   error("This is an abstract method, which should be overridden by subclasses."); 
  end
 ],

 ["Method","set_C"::void,"This invokes one of the methods @set_C0@, @set_C1@, @set_C3@ or @set_C5@, as specified by the parameter @k@.",
  proc(this,k::integer,t::scalar)
   if k = 0 then
    this["set_C0",t];
   elif k = 1 then
    this["set_C1",t];
   elif k = 3 then
    this["set_C3",t];
   elif k = 5 then
    this["set_C5",t];
   else
    error("Invalid index for C");
   fi;
  end
 ],

 ["Method","set_v0"::void,
  "Set this point to be $v_0$, at the bottom right corner of the domain",
  proc(this,float_::boolean)
   if nargs > 1 and float_ = true then 
    this["set_C1",0.];
   else 
    this["set_C1",0];
   fi;
   this["constraint"] := CONSTRAINT_V0;
   NULL;
  end
 ],

 ["Method","set_v3"::void,
  "Set this point to be $v_3$, at the top left corner of the domain",
  proc(this,float_::boolean)
   if nargs > 1 and float_ = true then 
    this["set_C0",1.];
   else 
    this["set_C0",1];
   fi;
   this["constraint"] := CONSTRAINT_V3;
   NULL;
  end
 ],

 ["Method","set_v6"::void,
  "Set this point to be $v_6$, at the bottom left corner of the domain",
  proc(this,float_::boolean)
   if nargs > 1 and float_ = true then 
    this["set_C0",0.];
   else 
    this["set_C0",0];
   fi;
   this["constraint"] := CONSTRAINT_V6;
   NULL;
  end
 ],

 ["Method","set_v11"::void,
  "Set this point to be $v_{11}$, at the top right corner of the domain",
  proc(this,float_::boolean)
   if nargs > 1 and float_ = true then 
    this["set_C3",0.];
   else 
    this["set_C3",0];
   fi;
   this["constraint"] := CONSTRAINT_V11;
   NULL;
  end
 ],

 ["Method","set_v"::void,
  "Set this point to be $v_k$",
  proc(this,k::integer)
   if k = 0 then 
    this["set_v0"];
   elif k = 3 then 
    this["set_v3"];
   elif k = 6 then 
    this["set_v6"];
   elif k = 11 then 
    this["set_v11"];
   else
    error("Invalid index for v");
   fi;

   NULL;
  end
 ],

 ["Method","float"::void,
  "Convert the coordinates etc (which might be exact rational or algebraic numbers) to floating point.",
  proc(this)
   this["x"] := evalf(this["x"]);
   this["fix"];
   NULL;
  end
 ],

 ["Method","clone"::domain_point,
  "Return an independent copy of this point, which can be changed without affecting the original.",
  proc(this)
   return copy(this);
  end
 ],

 NULL
);
