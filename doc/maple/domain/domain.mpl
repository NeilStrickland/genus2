<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># In various places we consider points in the fundamental domain
<a name="line_2"></a># of a cromulent surface, which may be permitted to move.
<a name="line_3"></a># Some points may be constrained to lie on one of the four sides
<a name="line_4"></a># of the domain, which are the curves C[0], C[1], C[3] and C[5].
<a name="line_5"></a># Other points may be constrained to lie at one of the four
<a name="line_6"></a># corners, which are v[0], v[3], v[6] and v[11].  We use numerical
<a name="line_7"></a># codes for these cases, as follows:
<a name="line_8"></a>
<a name="line_9"></a><span style="color:red">#@ CONSTRAINT
</span><a name="line_10"></a>CONSTRAINT["FREE"]  := 0:
<a name="line_11"></a>CONSTRAINT["C1"]    := 2:
<a name="line_12"></a>CONSTRAINT["C3"]    := 4:
<a name="line_13"></a>CONSTRAINT["C5"]    := 5:
<a name="line_14"></a>CONSTRAINT["C0"]    := 8:
<a name="line_15"></a>CONSTRAINT["FIXED"] := 15:
<a name="line_16"></a>CONSTRAINT["V0"]    := 15:
<a name="line_17"></a>CONSTRAINT["V3"]    := 31:
<a name="line_18"></a>CONSTRAINT["V6"]    := 47:
<a name="line_19"></a>CONSTRAINT["V11"]   := 63:
<a name="line_20"></a>
<a name="line_21"></a># The following block sets things up so that constraint_name[31]
<a name="line_22"></a># (for example) is the string "CONSTRAINT_V3".
<a name="line_23"></a>
<a name="line_24"></a><span style="color:red">#@ constraint_name 
</span><a name="line_25"></a>constraint_name := table():
<a name="line_26"></a>
<a name="line_27"></a>proc()
<a name="line_28"></a> local i,n;
<a name="line_29"></a> global constraint_name;
<a name="line_30"></a>
<a name="line_31"></a> for i in map(op,[indices(CONSTRAINT)]) do 
<a name="line_32"></a>  n := cat("CONSTRAINT_",i);
<a name="line_33"></a>  assign(convert(n,name),CONSTRAINT[i]);
<a name="line_34"></a>  constraint_name[CONSTRAINT[i]] := i;
<a name="line_35"></a> od:
<a name="line_36"></a>end():
<a name="line_37"></a>
<a name="line_38"></a><span style="color:red">#@ curve_index_by_constraint 
</span><a name="line_39"></a>curve_index_by_constraint := table():
<a name="line_40"></a>curve_index_by_constraint[CONSTRAINT_FREE]  := NULL:
<a name="line_41"></a>curve_index_by_constraint[CONSTRAINT_C1]    := 1:
<a name="line_42"></a>curve_index_by_constraint[CONSTRAINT_C3]    := 3:
<a name="line_43"></a>curve_index_by_constraint[CONSTRAINT_C5]    := 5:
<a name="line_44"></a>curve_index_by_constraint[CONSTRAINT_C0]    := 0:
<a name="line_45"></a>curve_index_by_constraint[CONSTRAINT_FIXED] := NULL:
<a name="line_46"></a>curve_index_by_constraint[CONSTRAINT_V0]    := NULL:
<a name="line_47"></a>curve_index_by_constraint[CONSTRAINT_V3]    := NULL:
<a name="line_48"></a>curve_index_by_constraint[CONSTRAINT_V6]    := NULL:
<a name="line_49"></a>curve_index_by_constraint[CONSTRAINT_V11]   := NULL:
<a name="line_50"></a>
<a name="line_51"></a><span style="color:red">#@ constraint_colour 
</span><a name="line_52"></a>constraint_colour := table():
<a name="line_53"></a>constraint_colour[CONSTRAINT_FREE]  := black:
<a name="line_54"></a>constraint_colour[CONSTRAINT_C1]    := c_colour[1]:
<a name="line_55"></a>constraint_colour[CONSTRAINT_C3]    := c_colour[3]:
<a name="line_56"></a>constraint_colour[CONSTRAINT_C5]    := c_colour[5]:
<a name="line_57"></a>constraint_colour[CONSTRAINT_C0]    := c_colour[0]:
<a name="line_58"></a>constraint_colour[CONSTRAINT_FIXED] := black:
<a name="line_59"></a>constraint_colour[CONSTRAINT_V0]    := black:
<a name="line_60"></a>constraint_colour[CONSTRAINT_V3]    := black:
<a name="line_61"></a>constraint_colour[CONSTRAINT_V6]    := black:
<a name="line_62"></a>constraint_colour[CONSTRAINT_V11]   := black:
<a name="line_63"></a>
<a name="line_64"></a><span style="color:red">#@ constraint_table 
</span><a name="line_65"></a>constraint_table := [
<a name="line_66"></a> [CONSTRAINT_V6,CONSTRAINT_C1,CONSTRAINT_V0],
<a name="line_67"></a> [CONSTRAINT_C0,CONSTRAINT_FREE,CONSTRAINT_C5],
<a name="line_68"></a> [CONSTRAINT_V3,CONSTRAINT_C3,CONSTRAINT_V11]
<a name="line_69"></a>]:
<a name="line_70"></a>
<a name="line_71"></a><span style="color:red">#@ constraint_for_square 
</span><a name="line_72"></a>constraint_for_square := proc(x,y)
<a name="line_73"></a> local i,j;
<a name="line_74"></a>
<a name="line_75"></a> i := `if`(x = 0,1,`if`(x = 1,3,2));
<a name="line_76"></a> j := `if`(y = 0,1,`if`(y = 1,3,2));
<a name="line_77"></a> return constraint_table[j][i];
<a name="line_78"></a>end:
<a name="line_79"></a>
<a name="line_80"></a>######################################################################
<a name="line_81"></a>
<a name="line_82"></a><a name="CLASS_domain"></a><span style="color:red">#@ CLASS: domain
</span><a name="line_83"></a>`Class/Declare`("domain",
<a name="line_84"></a> "An instance represents the fundamental domain in a cromulent surface",
<a name="line_85"></a> 
<a name="line_86"></a> ["Field","type"::string = "abstract",
<a name="line_87"></a>  "A string describing the type of domain (e.g. @E@ for a member of the embedded family)"
<a name="line_88"></a> ],
<a name="line_89"></a>
<a name="line_90"></a> ["Field","dim"::posint,
<a name="line_91"></a>  "All domains are expected to be subsets of $\\mathbb{R}^d$ for some integer $d$, stored in this field."
<a name="line_92"></a> ],
<a name="line_93"></a>
<a name="line_94"></a> ["Field","point_class_name",
<a name="line_95"></a>  "Points in the domain will be represented by instances of a subclass of the abstract class @domain_point@.  The name of the relevant subclass is stored in this field, and the table representing the subclass is stored in the @point_class@ field.  Both fields should be set using the @set_point_class@ method.  Analogous comments apply to the fields @edge_class_name@, @edge_class@, @face_class_name@, @face_class@, @grid_class_name@ and @grid_class@."
<a name="line_96"></a> ],
<a name="line_97"></a>
<a name="line_98"></a> ["Field","point_class",
<a name="line_99"></a>  ""
<a name="line_100"></a> ],
<a name="line_101"></a>
<a name="line_102"></a> ["Field","edge_class_name"],
<a name="line_103"></a> ["Field","edge_class"],
<a name="line_104"></a> ["Field","face_class_name"],
<a name="line_105"></a> ["Field","face_class"],
<a name="line_106"></a> ["Field","grid_class_name"],
<a name="line_107"></a> ["Field","grid_class"],
<a name="line_108"></a>
<a name="line_109"></a> ["Constructor","",
<a name="line_110"></a>  proc(this)
<a name="line_111"></a>   this["point_class_name"] := "domain_point";
<a name="line_112"></a>   this["edge_class_name" ] := "domain_edge";
<a name="line_113"></a>   this["face_class_name"]  := "domain_face";
<a name="line_114"></a>   this["grid_class_name"]  := "domain_grid";
<a name="line_115"></a>   this["point_class"] := eval(`class/domain_point`);
<a name="line_116"></a>   this["edge_class" ] := eval(`class/domain_edge`);
<a name="line_117"></a>   this["face_class"]  := eval(`class/domain_face`);
<a name="line_118"></a>   this["grid_class"]  := eval(`class/domain_grid`);
<a name="line_119"></a>  end
<a name="line_120"></a> ],
<a name="line_121"></a>
<a name="line_122"></a> ["Method","set_point_class"::void,"",
<a name="line_123"></a>  proc(this,classname::string)
<a name="line_124"></a>   this["point_class_name"] := classname;
<a name="line_125"></a>   this["point_class"] := eval(convert(cat("class/",classname),name));
<a name="line_126"></a>   NULL;
<a name="line_127"></a>  end
<a name="line_128"></a> ],
<a name="line_129"></a>
<a name="line_130"></a> ["Method","set_edge_class"::void,"",
<a name="line_131"></a>  proc(this,classname::string)
<a name="line_132"></a>   this["edge_class_name"] := classname;
<a name="line_133"></a>   this["edge_class"] := eval(convert(cat("class/",classname),name));
<a name="line_134"></a>   NULL;
<a name="line_135"></a>  end
<a name="line_136"></a> ],
<a name="line_137"></a>
<a name="line_138"></a> ["Method","set_face_class"::void,"",
<a name="line_139"></a>  proc(this,classname::string)
<a name="line_140"></a>   this["face_class_name"] := classname;
<a name="line_141"></a>   this["face_class"] := eval(convert(cat("class/",classname),name));
<a name="line_142"></a>   NULL;
<a name="line_143"></a>  end
<a name="line_144"></a> ],
<a name="line_145"></a>
<a name="line_146"></a> ["Method","set_grid_class"::void,"",
<a name="line_147"></a>  proc(this,classname::string)
<a name="line_148"></a>   this["grid_class_name"] := classname;
<a name="line_149"></a>   this["grid_class"] := eval(convert(cat("class/",classname),name));
<a name="line_150"></a>   NULL;
<a name="line_151"></a>  end
<a name="line_152"></a> ],
<a name="line_153"></a>
<a name="line_154"></a> ["Method","new_point"::void,
<a name="line_155"></a>  "This method returns a new instance of the relevant point class.",
<a name="line_156"></a>  proc(this)
<a name="line_157"></a>   eval(this["point_class"]["FullConstructor"]());
<a name="line_158"></a>  end
<a name="line_159"></a> ],
<a name="line_160"></a>
<a name="line_161"></a> ["Method","new_edge"::void,
<a name="line_162"></a>  "This method returns a new instance of the relevant edge class.",
<a name="line_163"></a>  proc(this,P0,P1)
<a name="line_164"></a>   eval(this["edge_class"]["FullConstructor"](P0,P1));
<a name="line_165"></a>  end
<a name="line_166"></a> ],
<a name="line_167"></a>
<a name="line_168"></a> ["Method","new_face"::void,
<a name="line_169"></a>  "This method returns a new instance of the relevant face class.",
<a name="line_170"></a>  proc(this,S0,S1,S2,s_)
<a name="line_171"></a>   eval(this["face_class"]["FullConstructor"](args[2..-1]));
<a name="line_172"></a>  end
<a name="line_173"></a> ],
<a name="line_174"></a>
<a name="line_175"></a> ["Method","new_grid"::void,
<a name="line_176"></a>  "This method returns a new instance of the relevant grid class.",
<a name="line_177"></a>  proc(this)
<a name="line_178"></a>   local G;
<a name="line_179"></a>
<a name="line_180"></a>   G := eval(this["grid_class"]["FullConstructor"]());
<a name="line_181"></a>   G["domain"] := eval(this);
<a name="line_182"></a>   return eval(G);
<a name="line_183"></a>  end
<a name="line_184"></a> ],
<a name="line_185"></a>
<a name="line_186"></a> NULL
<a name="line_187"></a>);
<a name="line_188"></a>
  </pre>
 </body>
</html>
    