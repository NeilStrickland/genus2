<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a><a name="CLASS_domain_edge"></a><span style="color:red">#@ CLASS: domain_edge
</span><a name="line_2"></a>
<a name="line_3"></a>`Class/Declare`("domain_edge",
<a name="line_4"></a> "An instance of this class represents an edge in a domain.",
<a name="line_5"></a>
<a name="line_6"></a> ["Field","grid_index"::integer = 0, 
<a name="line_7"></a>  "In many cases edges will form part of a grid, and all the edges in the grid will be numbered from 0 to n-1 for some n.  The index number will be stored in this field."
<a name="line_8"></a> ],
<a name="line_9"></a>
<a name="line_10"></a> ["Field","constraint"::integer = CONSTRAINT_FREE,
<a name="line_11"></a>  "The @constraint@ field contains an integer encoding information about whether this edge (ignoring endpoints) is in the interior of the domain, or whether it lies along one of the sides.  We always refer to specific values by symbolic names such as CONSTRAINT_FREE or CONSTRAINT_C3 rather than bare integers.  For more details of the encoding, see the file domain.mpl.  Grids should be set up so that no edge has endpoints on two different sides of the domain."
<a name="line_12"></a> ],
<a name="line_13"></a>
<a name="line_14"></a> ["Field","curve_index" = NULL,
<a name="line_15"></a>  "This field contains the same information as the @constraint@ field, but encoded differently.  It takes the value 0, 1, 3 or 5 if the edge lies along C[0], C[1], C[3] or C[5], and it is NULL if the edge does not lie along any of these boundary curves."
<a name="line_16"></a> ],
<a name="line_17"></a>
<a name="line_18"></a> ["Field","end"::table,
<a name="line_19"></a>  "This is a table with indices 0 and 1; the entries are instances of an appropriate subclass of @domain_point@, representing the two endpoints of the edge.  We do not store any information about the precise path taken between the endpoints.  The domain may have methods that encode a canonical choice of path."
<a name="line_20"></a> ],
<a name="line_21"></a>
<a name="line_22"></a> ["Field","vector"::list(scalar),
<a name="line_23"></a>  "This is the vector from end[0] to end[1]"
<a name="line_24"></a> ],
<a name="line_25"></a>
<a name="line_26"></a> ["Field","straight_length"::scalar,
<a name="line_27"></a>  "This is the straight line distance from end[0] to end[1]"
<a name="line_28"></a> ],
<a name="line_29"></a>
<a name="line_30"></a> ["Field",
<a name="line_31"></a>  "weighting"::scalar = 0,
<a name="line_32"></a>  "This field is used when calculating the Dirichlet energy of a map between two domains.  Details will be explained elsewhere."
<a name="line_33"></a> ],
<a name="line_34"></a>
<a name="line_35"></a> ["Constructor",
<a name="line_36"></a>  "Construct a new edge with endpoints @P0@ and @P1@.",
<a name="line_37"></a>  proc(this,P0::domain_point,P1::domain_point)
<a name="line_38"></a>   this["end"] := table();
<a name="line_39"></a>   this["set",P0,P1];
<a name="line_40"></a>  end
<a name="line_41"></a> ],
<a name="line_42"></a>
<a name="line_43"></a> ["Method","set"::void,
<a name="line_44"></a>  "Set the endpoints to @P0@ and @P1@",
<a name="line_45"></a>  proc(this,P0::domain_point,P1::domain_point)
<a name="line_46"></a>   this["end"][0] := eval(P0);
<a name="line_47"></a>   this["end"][1] := eval(P1);
<a name="line_48"></a>   this["constraint"] := bitwise_and(P0["constraint"],P1["constraint"]);
<a name="line_49"></a>   this["curve_index"] := curve_index_by_constraint[this["constraint"]];
<a name="line_50"></a>   this["calculate"];
<a name="line_51"></a>  end
<a name="line_52"></a> ],
<a name="line_53"></a>
<a name="line_54"></a> ["Method","calculate"::void,
<a name="line_55"></a>  "Calculate various auxiliary quantities such as the @vector@ and @straight_length@ fields.  Subclasses may override this method to perform more interesting work.",
<a name="line_56"></a>  proc(this)
<a name="line_57"></a>   local v,t;
<a name="line_58"></a>
<a name="line_59"></a>   v := this["end"][1]["x"] -~ this["end"][0]["x"];
<a name="line_60"></a>   this["vector"] := v;
<a name="line_61"></a>   this["straight_length"] := simp(sqrt(expand(add(t^2,t in v))));
<a name="line_62"></a>   NULL;
<a name="line_63"></a>  end
<a name="line_64"></a> ],
<a name="line_65"></a>
<a name="line_66"></a> ["Method","end_indices"::list(integer),
<a name="line_67"></a>  "For an edge in a grid, return the indices of the two endpoints.",
<a name="line_68"></a>  proc(this)
<a name="line_69"></a>   local E;
<a name="line_70"></a>   E := this["end"];
<a name="line_71"></a>   [E[0]["grid_index"],E[1]["grid_index"]];
<a name="line_72"></a>  end
<a name="line_73"></a> ],
<a name="line_74"></a>
<a name="line_75"></a> NULL
<a name="line_76"></a>);
<a name="line_77"></a>
  </pre>
 </body>
</html>
    