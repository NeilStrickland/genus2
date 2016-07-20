<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a><a name="CLASS_domain_point"></a><span style="color:red">#@ CLASS: domain_point
</span><a name="line_2"></a>
<a name="line_3"></a>`Class/Declare`("domain_point",
<a name="line_4"></a> "This is an abstract class which serves as the parent for various subclasses representing points in various different domains.",
<a name="line_5"></a>
<a name="line_6"></a> ["Field","grid_index"::integer = 0,
<a name="line_7"></a>  "In many cases points will form part of a grid, and all the points in the grid will be numbered from $0$ to $n-1$ for some $n$.  The index number will be stored in this field."
<a name="line_8"></a> ],
<a name="line_9"></a>
<a name="line_10"></a> ["Field","var_index"::integer = 0,
<a name="line_11"></a>  "In various situations we will want to adjust slightly the position of points in a grid.  Points in the interior of the domain have two degrees of freedom to move, but points on an edge have only one degree of freedom, and the corners have no freedom.  The adjustment will be governed by variables @t[0],..,t[m-1]@ say.  If this point is in the interior, then the first order adjustment will be @t[j] * u + t[j+1] * v@, where @u@ and @v@ are the vectors described below, and @j@ is the value of @var_index@.  If this point is on an edge, then the first order adjustment will be @t[j] * u@, where again @j@ is the value of @var_index@.  The @var_index@ field is not used for corner points."
<a name="line_12"></a> ],
<a name="line_13"></a>
<a name="line_14"></a> ["Field","constraint"::integer = CONSTRAINT_FREE,
<a name="line_15"></a>  "The @constraint@ field contains an integer encoding information about whether this point is in the interior of the domain, or on an edge, or at a corner.  We always refer to specific values by symbolic names such as CONSTRAINT_FREE, CONSTRAINT_C3 or CONSTRAINT_V6 rather than bare integers.  For more details of the encoding, see the file domain.mpl."
<a name="line_16"></a> ],
<a name="line_17"></a>
<a name="line_18"></a> ["Field","x"::list,"Coordinates of the point"],
<a name="line_19"></a> ["Field","u"::list,"A unit vector tangent to the surface at the point.  If the point is constrained to lie on an edge, then @u@ should point along that edge."],
<a name="line_20"></a> ["Field","v"::list,"The vector obtained by rotating @u@ anticlockwise by a quarter turn."],
<a name="line_21"></a>
<a name="line_22"></a> ["Method","set"::void,
<a name="line_23"></a>  "Set the constraint and coordinates, then calculate all other fields.  The calculation procedure may change the coordinates slightly if necessary to ensure that the point lies exactly on the surface and the specified constraints are satisfied.",
<a name="line_24"></a>  proc(this,c::integer,x::list(scalar))
<a name="line_25"></a>   this["constraint"] := c;
<a name="line_26"></a>   this["x"] := x;
<a name="line_27"></a>   this["fix"];
<a name="line_28"></a>  end
<a name="line_29"></a> ],
<a name="line_30"></a>
<a name="line_31"></a> ["Method","fix"::void,
<a name="line_32"></a>  "This is an abstract method, which should be overridden by subclasses.  The overriding method should change the coordinates slightly if necessary to ensure that the point lies exactly on the surface and the specified constraints are satisfied, then it should calculate @u@ and @v@ and possibly other information specific to the relevant domain.",
<a name="line_33"></a>  proc(this)
<a name="line_34"></a>   error("This is an abstract method, which should be overridden by subclasses."); 
<a name="line_35"></a>  end
<a name="line_36"></a> ],
<a name="line_37"></a>
<a name="line_38"></a> ["Method","adjust"::void,
<a name="line_39"></a>  "For a point in the interior, this method adds $su + tv$ to $x$ and then calls the @fix@ method.  Here $s$ and $t$ are parameters for this method, and $u$ and $v$ are the unit vectors stored in the @u@ and @v@ fields.  For an edge point we add $su$ instead of $su + tv$; for a corner point we do nothing.",
<a name="line_40"></a>  proc(this,s::scalar,t::scalar)
<a name="line_41"></a>   if this["constraint"] <> CONSTRAINT_FIXED then
<a name="line_42"></a>    if this["constraint"] = CONSTRAINT_FREE then
<a name="line_43"></a>     this["x"] := this["x"] +~ (s *~ this["u"]) +~ (t *~ this["v"]);
<a name="line_44"></a>    else
<a name="line_45"></a>     this["x"] := this["x"] +~ (s *~ this["u"]);
<a name="line_46"></a>    fi;
<a name="line_47"></a>
<a name="line_48"></a>    this["fix"];
<a name="line_49"></a>   fi;
<a name="line_50"></a>   NULL;
<a name="line_51"></a>  end
<a name="line_52"></a> ],
<a name="line_53"></a>
<a name="line_54"></a> ["Method","set_C0"::void,
<a name="line_55"></a>  "This is an abstract method, which should be overridden by subclasses.  The parameter $t$ should be between $0$ and $1$.  The overriding method should set all fields to represent a point on the left edge of the domain, with $t=0$ corresponding to the point $v_6$ at the bottom left, and $t=1$ corresponding to the point $v_3$ at the top left.",
<a name="line_56"></a>  proc(this,t::scalar)
<a name="line_57"></a>   error("This is an abstract method, which should be overridden by subclasses."); 
<a name="line_58"></a>  end
<a name="line_59"></a> ],
<a name="line_60"></a>
<a name="line_61"></a> ["Method","set_C1"::void,
<a name="line_62"></a>  "This is an abstract method, which should be overridden by subclasses.  The parameter $t$ should be between $0$ and $1$.  The overriding method should set all fields to represent a point on the bottom edge of the domain, with $t=0$ corresponding to the point $v_6$ at the bottom left, and $t=1$ corresponding to the point $v_0$ at the bottom right.",
<a name="line_63"></a>  proc(this,t::scalar)
<a name="line_64"></a>   error("This is an abstract method, which should be overridden by subclasses."); 
<a name="line_65"></a>  end
<a name="line_66"></a> ],
<a name="line_67"></a>
<a name="line_68"></a> ["Method","set_C3"::void,
<a name="line_69"></a>  "This is an abstract method, which should be overridden by subclasses.  The parameter $t$ should be between $0$ and $1$.  The overriding method should set all fields to represent a point on the top edge of the domain, with $t=0$ corresponding to the point $v_3$ at the top left, and $t=1$ corresponding to the point $v_{11}$ at the top right.",
<a name="line_70"></a>  proc(this,t::scalar)
<a name="line_71"></a>   error("This is an abstract method, which should be overridden by subclasses."); 
<a name="line_72"></a>  end
<a name="line_73"></a> ],
<a name="line_74"></a>
<a name="line_75"></a> ["Method","set_C5"::void,
<a name="line_76"></a>  "This is an abstract method, which should be overridden by subclasses.  The parameter t should be between $0$ and $1$.  The overriding method should set all fields to represent a point on the right edge of the domain, with $t=0$ corresponding to the point $v_0$ at the bottom right, and $t=1$ corresponding to the point $v_{11}$ at the top right.",
<a name="line_77"></a>  proc(this,t::scalar)
<a name="line_78"></a>   error("This is an abstract method, which should be overridden by subclasses."); 
<a name="line_79"></a>  end
<a name="line_80"></a> ],
<a name="line_81"></a>
<a name="line_82"></a> ["Method","set_C"::void,"This invokes one of the methods @set_C0@, @set_C1@, @set_C3@ or @set_C5@, as specified by the parameter @k@.",
<a name="line_83"></a>  proc(this,k::integer,t::scalar)
<a name="line_84"></a>   if k = 0 then
<a name="line_85"></a>    this["set_C0",t];
<a name="line_86"></a>   elif k = 1 then
<a name="line_87"></a>    this["set_C1",t];
<a name="line_88"></a>   elif k = 3 then
<a name="line_89"></a>    this["set_C3",t];
<a name="line_90"></a>   elif k = 5 then
<a name="line_91"></a>    this["set_C5",t];
<a name="line_92"></a>   else
<a name="line_93"></a>    error("Invalid index for C");
<a name="line_94"></a>   fi;
<a name="line_95"></a>  end
<a name="line_96"></a> ],
<a name="line_97"></a>
<a name="line_98"></a> ["Method","set_v0"::void,
<a name="line_99"></a>  "Set this point to be $v_0$, at the bottom right corner of the domain",
<a name="line_100"></a>  proc(this,float_::boolean)
<a name="line_101"></a>   if nargs > 1 and float_ = true then 
<a name="line_102"></a>    this["set_C1",0.];
<a name="line_103"></a>   else 
<a name="line_104"></a>    this["set_C1",0];
<a name="line_105"></a>   fi;
<a name="line_106"></a>   this["constraint"] := CONSTRAINT_V0;
<a name="line_107"></a>   NULL;
<a name="line_108"></a>  end
<a name="line_109"></a> ],
<a name="line_110"></a>
<a name="line_111"></a> ["Method","set_v3"::void,
<a name="line_112"></a>  "Set this point to be $v_3$, at the top left corner of the domain",
<a name="line_113"></a>  proc(this,float_::boolean)
<a name="line_114"></a>   if nargs > 1 and float_ = true then 
<a name="line_115"></a>    this["set_C0",1.];
<a name="line_116"></a>   else 
<a name="line_117"></a>    this["set_C0",1];
<a name="line_118"></a>   fi;
<a name="line_119"></a>   this["constraint"] := CONSTRAINT_V3;
<a name="line_120"></a>   NULL;
<a name="line_121"></a>  end
<a name="line_122"></a> ],
<a name="line_123"></a>
<a name="line_124"></a> ["Method","set_v6"::void,
<a name="line_125"></a>  "Set this point to be $v_6$, at the bottom left corner of the domain",
<a name="line_126"></a>  proc(this,float_::boolean)
<a name="line_127"></a>   if nargs > 1 and float_ = true then 
<a name="line_128"></a>    this["set_C0",0.];
<a name="line_129"></a>   else 
<a name="line_130"></a>    this["set_C0",0];
<a name="line_131"></a>   fi;
<a name="line_132"></a>   this["constraint"] := CONSTRAINT_V6;
<a name="line_133"></a>   NULL;
<a name="line_134"></a>  end
<a name="line_135"></a> ],
<a name="line_136"></a>
<a name="line_137"></a> ["Method","set_v11"::void,
<a name="line_138"></a>  "Set this point to be $v_{11}$, at the top right corner of the domain",
<a name="line_139"></a>  proc(this,float_::boolean)
<a name="line_140"></a>   if nargs > 1 and float_ = true then 
<a name="line_141"></a>    this["set_C3",0.];
<a name="line_142"></a>   else 
<a name="line_143"></a>    this["set_C3",0];
<a name="line_144"></a>   fi;
<a name="line_145"></a>   this["constraint"] := CONSTRAINT_V11;
<a name="line_146"></a>   NULL;
<a name="line_147"></a>  end
<a name="line_148"></a> ],
<a name="line_149"></a>
<a name="line_150"></a> ["Method","set_v"::void,
<a name="line_151"></a>  "Set this point to be $v_k$",
<a name="line_152"></a>  proc(this,k::integer)
<a name="line_153"></a>   if k = 0 then 
<a name="line_154"></a>    this["set_v0"];
<a name="line_155"></a>   elif k = 3 then 
<a name="line_156"></a>    this["set_v3"];
<a name="line_157"></a>   elif k = 6 then 
<a name="line_158"></a>    this["set_v6"];
<a name="line_159"></a>   elif k = 11 then 
<a name="line_160"></a>    this["set_v11"];
<a name="line_161"></a>   else
<a name="line_162"></a>    error("Invalid index for v");
<a name="line_163"></a>   fi;
<a name="line_164"></a>
<a name="line_165"></a>   NULL;
<a name="line_166"></a>  end
<a name="line_167"></a> ],
<a name="line_168"></a>
<a name="line_169"></a> ["Method","float"::void,
<a name="line_170"></a>  "Convert the coordinates etc (which might be exact rational or algebraic numbers) to floating point.",
<a name="line_171"></a>  proc(this)
<a name="line_172"></a>   this["x"] := evalf(this["x"]);
<a name="line_173"></a>   this["fix"];
<a name="line_174"></a>   NULL;
<a name="line_175"></a>  end
<a name="line_176"></a> ],
<a name="line_177"></a>
<a name="line_178"></a> ["Method","clone"::domain_point,
<a name="line_179"></a>  "Return an independent copy of this point, which can be changed without affecting the original.",
<a name="line_180"></a>  proc(this)
<a name="line_181"></a>   return copy(this);
<a name="line_182"></a>  end
<a name="line_183"></a> ],
<a name="line_184"></a>
<a name="line_185"></a> NULL
<a name="line_186"></a>);
  </pre>
 </body>
</html>
    