<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a><a name="CLASS_domain_face"></a><span style="color:red">#@ CLASS: domain_face
</span><a name="line_2"></a>
<a name="line_3"></a>`Class/Declare`(
<a name="line_4"></a> "domain_face",
<a name="line_5"></a> "An instance represents a triangular face in a domain.",
<a name="line_6"></a> 
<a name="line_7"></a> ["Field","grid_index"::integer = 0,
<a name="line_8"></a>  "In many cases faces will form part of a grid, and all the faces in the grid will be numbered from 0 to n-1 for some n.  The index number will be stored in this field."
<a name="line_9"></a> ],
<a name="line_10"></a>
<a name="line_11"></a> ["Field","corner"::table,
<a name="line_12"></a>  "This is a table with indices 0, 1 and 2; the entries are instances of an appropriate subclass of @domain_point@, representing the three corners of the face."
<a name="line_13"></a> ],
<a name="line_14"></a>
<a name="line_15"></a> ["Field","side"::table,
<a name="line_16"></a>  "This is a table with indices 0, 1 and 2; the entries are instances of an appropriate subclass of @domain_edge@, representing the three sides of the face.  side[0] should join corner[0] to corner[1], and side[1] should join corner[0] to corner[2], and side[2] should join corner[1] to corner[2].  In more detail, end[0] of side[0] should be the same Maple object as corner[0], so that changing the fields of end[0] of side[0] automatically changes the fields of corner[0], and similarly in the other cases; it is not enough for end[0] of side[0] to be a different object with the same coordinates."
<a name="line_17"></a> ],
<a name="line_18"></a>
<a name="line_19"></a> ["Field","orientation"::integer = 1,
<a name="line_20"></a>  "This should be +1 if circuit corner[0] -> corner[1] -> corner[2] -> corner[0] runs anticlockwise (with respect to the given orientation of the domain), and -1 otherwise."
<a name="line_21"></a> ],
<a name="line_22"></a>
<a name="line_23"></a> ["Field","flat_area"::scalar = 0,
<a name="line_24"></a>  "This is the area of the affine triangle with the same vertices as this face."
<a name="line_25"></a> ],
<a name="line_26"></a>
<a name="line_27"></a> ["Constructor",
<a name="line_28"></a>  "Construct a new face with sides @S0@, @S1@ and @S2@.  It is assumed that the endpoints of these sides match up correctly.",
<a name="line_29"></a>  proc(this,S0::domain_edge,S1::domain_edge,S2::domain_edge)
<a name="line_30"></a>   this["corner"] := table();
<a name="line_31"></a>   this["side"] := table();
<a name="line_32"></a>   this["set",S0,S1,S2];
<a name="line_33"></a>  end
<a name="line_34"></a> ],
<a name="line_35"></a>
<a name="line_36"></a> ["Method","set"::void,
<a name="line_37"></a>  "Set the sides of this face to be @S0@, @S1@ and @S2@, and set the corners and other auxiliary quantities accordingly.  If the argument @s_@ is supplied, then it will be used as the orientation; otherwise the orientation will be calculated by a method that is valid provided that the face is not too strongly curved.  It is assumed that the endpoints of @S0@, @S1@ and @S2@ match up correctly; this can be checked by the @check_matching@ method if necessary.",
<a name="line_38"></a>  proc(this,S0::domain_edge,S1::domain_edge,S2::domain_edge,s_)
<a name="line_39"></a>
<a name="line_40"></a>   this["corner"][0] := eval(S0["end"][0]);
<a name="line_41"></a>   this["corner"][1] := eval(S0["end"][1]);
<a name="line_42"></a>   this["corner"][2] := eval(S1["end"][1]);
<a name="line_43"></a>
<a name="line_44"></a>   this["side"][0] := eval(S0);
<a name="line_45"></a>   this["side"][1] := eval(S1);
<a name="line_46"></a>   this["side"][2] := eval(S2);
<a name="line_47"></a>
<a name="line_48"></a>   if nargs >= 5 and s_ <> 0 then 
<a name="line_49"></a>    this["orientation"] := s_;
<a name="line_50"></a>   else 
<a name="line_51"></a>    this["set_orientation"];
<a name="line_52"></a>   fi;
<a name="line_53"></a>
<a name="line_54"></a>   this["calculate"];
<a name="line_55"></a>   NULL;
<a name="line_56"></a>  end
<a name="line_57"></a> ],
<a name="line_58"></a>
<a name="line_59"></a> ["Method","set_orientation"::void,
<a name="line_60"></a>  "Set the orientation,  by a method that is valid provided that the face is not too strongly curved.",
<a name="line_61"></a>  proc(this)
<a name="line_62"></a>   local x01,x02,u,v,s;
<a name="line_63"></a>
<a name="line_64"></a>   u := this["corner"][0]["u"];
<a name="line_65"></a>   v := this["corner"][0]["v"];
<a name="line_66"></a>
<a name="line_67"></a>   x01 := this["corner"][1]["x"] -~ this["corner"][1]["x"];
<a name="line_68"></a>   x02 := this["corner"][2]["x"] -~ this["corner"][0]["x"];
<a name="line_69"></a>
<a name="line_70"></a>   s := round(signum(dpv(x01,u) * dpv(x02,v) - dpv(x02,u) * dpv(x01,v)));
<a name="line_71"></a>   this["orientation"] := s;
<a name="line_72"></a>
<a name="line_73"></a>   return s;
<a name="line_74"></a>  end
<a name="line_75"></a> ],
<a name="line_76"></a>
<a name="line_77"></a> ["Method","calculate"::void,
<a name="line_78"></a>  "Calculate various auxiliary quantities such as the @flat_area@ field.  Subclasses may override this method to perform more interesting work.",
<a name="line_79"></a>  proc(this)
<a name="line_80"></a>   local q0,q1,q2,x01,x02,A;
<a name="line_81"></a>
<a name="line_82"></a>   x01 := this["side"][0]["vector"];
<a name="line_83"></a>   x02 := this["side"][1]["vector"];
<a name="line_84"></a>
<a name="line_85"></a>   q0 := simp(dpv(x01,x01));
<a name="line_86"></a>   q1 := simp(dpv(x02,x02));
<a name="line_87"></a>   q2 := simp(dpv(x01,x02));
<a name="line_88"></a>
<a name="line_89"></a>   A := simp(expand(q0*q1 - q2*q2));
<a name="line_90"></a>
<a name="line_91"></a>   # If calculated exactly then A should be nonnegative.  However,
<a name="line_92"></a>   # it might be tiny and negative due to rounding errors.
<a name="line_93"></a>   if type(A,float) and A < 0 then A := 0; fi;
<a name="line_94"></a>
<a name="line_95"></a>   this["flat_area"] := simp(sqrt(A)/2);
<a name="line_96"></a>   NULL;
<a name="line_97"></a>  end
<a name="line_98"></a> ],
<a name="line_99"></a>
<a name="line_100"></a> ["Method","corner_indices"::list(integer),
<a name="line_101"></a>  "For an face in a grid, return the indices of the three corners.",
<a name="line_102"></a>  proc(this)
<a name="line_103"></a>   local C;
<a name="line_104"></a>   C := this["corner"];
<a name="line_105"></a>   [C[0]["grid_index"],C[1]["grid_index"],C[2]["grid_index"]];
<a name="line_106"></a>  end
<a name="line_107"></a> ],
<a name="line_108"></a> 
<a name="line_109"></a> ["Method","side_indices"::list(integer),
<a name="line_110"></a>  "For an face in a grid, return the indices of the three sidess.",
<a name="line_111"></a>  proc(this)
<a name="line_112"></a>   local S;
<a name="line_113"></a>   S := this["side"];
<a name="line_114"></a>   [S[0]["grid_index"],S[1]["grid_index"],S[2]["grid_index"]];
<a name="line_115"></a>  end
<a name="line_116"></a> ],
<a name="line_117"></a> 
<a name="line_118"></a> ["Method","check_matching"::boolean,
<a name="line_119"></a>  "Check whether the endpoints of the sides are the same as the corresponding corners.  They should be the same Maple objects, which can be tested by the built in equality operator.  However, one can also pass an equality checking procedure as a second argument to this method.",
<a name="line_120"></a>  proc(this,eq_)
<a name="line_121"></a>   local P0a,P0b,P0c,P1a,P1b,P1c,P2a,P2b,P2c,eq;
<a name="line_122"></a>
<a name="line_123"></a>   if nargs > 1 then
<a name="line_124"></a>    eq := eval(eq_);
<a name="line_125"></a>   else
<a name="line_126"></a>    eq := (Q0,Q1) -> evalb(Q0 = Q1);
<a name="line_127"></a>   end;
<a name="line_128"></a>
<a name="line_129"></a>   P0a := this["corner"][0];
<a name="line_130"></a>   P0b := this["side"][0]["end"][0];
<a name="line_131"></a>   P0c := this["side"][1]["end"][0];
<a name="line_132"></a>   P1a := this["corner"][1];
<a name="line_133"></a>   P1b := this["side"][0]["end"][1];
<a name="line_134"></a>   P1c := this["side"][2]["end"][0];
<a name="line_135"></a>   P2a := this["corner"][2];
<a name="line_136"></a>   P2b := this["side"][1]["end"][1];
<a name="line_137"></a>   P2c := this["side"][2]["end"][1];
<a name="line_138"></a>
<a name="line_139"></a>   return eq(P0a,P0b) and eq(P0b,P0c) and
<a name="line_140"></a>	  eq(P1a,P1b) and eq(P1b,P1c) and
<a name="line_141"></a>	  eq(P2a,P2b) and eq(P2b,P2c);
<a name="line_142"></a>  end
<a name="line_143"></a> ],
<a name="line_144"></a>
<a name="line_145"></a> NULL
<a name="line_146"></a>);
  </pre>
 </body>
</html>
    