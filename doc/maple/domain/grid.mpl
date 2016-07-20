<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a><a name="CLASS_grid"></a><span style="color:red">#@ CLASS: grid
</span><a name="line_2"></a>
<a name="line_3"></a>`Class/Declare`("grid",
<a name="line_4"></a> "An instance of this class represents a triangulation of a domain.",
<a name="line_5"></a>
<a name="line_6"></a> ["Field","description"::string = ""],
<a name="line_7"></a>
<a name="line_8"></a> ["Field","domain"::domain,
<a name="line_9"></a>  "This field should contain an instance of the @domain@ class (or a subclass), which specifies the domain on which this grid is based."
<a name="line_10"></a> ],
<a name="line_11"></a>
<a name="line_12"></a> ["Field","num_points"::integer = 0,"Number of points in the grid"],
<a name="line_13"></a> ["Field","num_edges"::integer = 0,"Nmber of edges in the grid"],
<a name="line_14"></a> ["Field","num_faces"::integer = 0,"Number of triangular faces in the grid"],
<a name="line_15"></a> ["Field","num_vars"::integer = 0,
<a name="line_16"></a>  "Number of degrees of freedom to move the points while respecting the fact that some points are constrained to lie on a side of the domain or at a corner."
<a name="line_17"></a> ],
<a name="line_18"></a>
<a name="line_19"></a> ["Field","points"::table,
<a name="line_20"></a>  "A table of points (represented by instances of the class #domain_point#), indexed by natural numbers starting with 0.  If @P=this[\"points\"][i]@, then @P[\"grid_index\"]@ should be set to @i@."
<a name="line_21"></a> ],
<a name="line_22"></a>
<a name="line_23"></a> ["Field","edges"::table,
<a name="line_24"></a>  "A table of edges (represented by instances of the class #domain_edge#), indexed by natural numbers starting with 0.  If @E=this[\"edges\"][i]@, then @E[\"grid_index\"]@ should be set to @i@."
<a name="line_25"></a> ],
<a name="line_26"></a>
<a name="line_27"></a> ["Field","faces"::table,
<a name="line_28"></a>  "A table of faces (represented by instances of the class #domain_face#), indexed by natural numbers starting with 0.  If @F=this[\"faces\"][i]@, then @F[\"grid_index\"]@ should be set to @i@."
<a name="line_29"></a> ],
<a name="line_30"></a>
<a name="line_31"></a> ["Field","edges_by_ends"::table,
<a name="line_32"></a>  "This is a table containing the same objects as the @edges@ table, but indexed differently: if there is an edge @E@ from @this[\"points\"][i]@ to @this[\"points\"][j]@, then @this[\"edges_by_ends\"][i,j]@ will be equal to @E@.  This is set up automatically by the @add_edge@ method.  Note that edges are directed: we are assuming here that @E[\"end\"][0]=this[\"points\"][i]@ and @E[\"end\"][1]=this[\"points\"][j]@, and @this[\"edges_by_ends\"][j,i]@ (with @i@ and @j@ the wrong way around) will not be set."
<a name="line_33"></a> ],
<a name="line_34"></a>
<a name="line_35"></a> ["Field","faces_by_corners"::table,
<a name="line_36"></a>  "This is a table containing the same objects as the @faces@ table, but indexed differently: if there is a face @F@ with corners @this[\"points\"][i]@, @this[\"points\"][j]@ and @this[\"points\"][k]@, then @this[\"faces_by_corners\"][i,j,k]@ will be equal to @F@.  This is set up automatically by the @add_face@ method."
<a name="line_37"></a> ],
<a name="line_38"></a>
<a name="line_39"></a> ["Constructor",
<a name="line_40"></a>  "Construct a new grid with no points",
<a name="line_41"></a>  proc(this,X_)
<a name="line_42"></a>   this["points"] := table();
<a name="line_43"></a>   this["edges"] := table();
<a name="line_44"></a>   this["faces"] := table();
<a name="line_45"></a>   this["edges_by_ends"] := table();
<a name="line_46"></a>   this["faces_by_corners"] := table();
<a name="line_47"></a>
<a name="line_48"></a>   if nargs > 1 then
<a name="line_49"></a>    this["domain"] := eval(X_);
<a name="line_50"></a>   fi;
<a name="line_51"></a>  end
<a name="line_52"></a> ],
<a name="line_53"></a>
<a name="line_54"></a> ["Field","total_flat_area"::scalar = 0,
<a name="line_55"></a>  "Total area of the simplicial complex whose 2-simplices are affine triangles with the same corners as the grid faces.  If the faces are small then this will be a reasonable approximation to the true area of the domain (but convergence is slow)."
<a name="line_56"></a> ],
<a name="line_57"></a>
<a name="line_58"></a> ["Field","tension"::scalar = 0,
<a name="line_59"></a>  "This is a quantity depending on the edge lengths.  The idea is that minimising the tension should give a grid that is quite evenly spaced.  We have tried various different versions at different times."
<a name="line_60"></a> ],
<a name="line_61"></a>
<a name="line_62"></a> ["Method","has_edge"::boolean,
<a name="line_63"></a>  "Returns true if there is an edge whose endpoints have indices @i@ and @j@.",
<a name="line_64"></a>  proc(this,i,j) member([i,j],[indices(this["edges_by_ends"])]); end
<a name="line_65"></a> ],
<a name="line_66"></a> 
<a name="line_67"></a> ["Method","has_face"::boolean,
<a name="line_68"></a>  "Returns true if there is an face whose endpoints have indices @i@, @j@ and @k@.",
<a name="line_69"></a>  proc(this,i,j,k) member([i,j,k],[indices(this["faces_by_corners"])]); end
<a name="line_70"></a> ],
<a name="line_71"></a> 
<a name="line_72"></a> ["Method","edge_indices"::list(list(integer)),
<a name="line_73"></a>  "Returns the list of pairs @[i,j]@ such that there is an edge from point @i@ to point @j@",
<a name="line_74"></a>  proc(this) [indices(this["edges_by_ends"])]; end
<a name="line_75"></a> ],
<a name="line_76"></a>
<a name="line_77"></a> ["Method","face_indices"::list(list(integer)),
<a name="line_78"></a>  "Returns the list of triples @[i,j,k]@ such that there is a face with corners point @i@, point @j@ and point @k@ (in order)",
<a name="line_79"></a>  proc(this) [indices(this["faces_by_corners"])]; end
<a name="line_80"></a> ],
<a name="line_81"></a>
<a name="line_82"></a> ["Method","add_point"::integer,
<a name="line_83"></a>  "Add a point (represented by an instance of #domain_point#) to the grid; return the index of the added point.",
<a name="line_84"></a>  proc(this,P::domain_point)
<a name="line_85"></a>   local i;
<a name="line_86"></a>   i := this["num_points"];
<a name="line_87"></a>   this["num_points"] := i+1;
<a name="line_88"></a>   P["grid_index"] := i;
<a name="line_89"></a>   this["points"][i] := eval(P);
<a name="line_90"></a>   return i;
<a name="line_91"></a>  end
<a name="line_92"></a> ],
<a name="line_93"></a>
<a name="line_94"></a> ["Method","add_new_point"::integer,
<a name="line_95"></a>  "Create a new point with constraint @c@ and coordinates @x@, add it to the grid, and return the index.",
<a name="line_96"></a>  proc(this,c::integer,x::list(scalar))
<a name="line_97"></a>   local P;
<a name="line_98"></a>
<a name="line_99"></a>   P := eval(this["domain"]["new_point"]);
<a name="line_100"></a>   P["set",c,x];
<a name="line_101"></a>   this["add_point",eval(P)];
<a name="line_102"></a>  end
<a name="line_103"></a> ],
<a name="line_104"></a>
<a name="line_105"></a> ["Method","add_new_C_point"::integer,
<a name="line_106"></a>  "Create a new point at $c_k(t)$, add it to the grid, and return the index.",
<a name="line_107"></a>  proc(this,k::integer,t)
<a name="line_108"></a>   local P;
<a name="line_109"></a>
<a name="line_110"></a>   P := eval(this["domain"]["new_point"]);
<a name="line_111"></a>   P["set_C",k,t];
<a name="line_112"></a>   this["add_point",eval(P)];
<a name="line_113"></a>  end
<a name="line_114"></a> ],
<a name="line_115"></a>
<a name="line_116"></a> ["Method","add_new_v_point"::integer,
<a name="line_117"></a>  "Create a new point at $v_k$, add it to the grid, and return the index.",
<a name="line_118"></a>  proc(this,k::integer)
<a name="line_119"></a>   local P;
<a name="line_120"></a>
<a name="line_121"></a>   P := eval(this["domain"]["new_point"]);
<a name="line_122"></a>   P["set_v",k];
<a name="line_123"></a>   this["add_point",eval(P)];
<a name="line_124"></a>  end
<a name="line_125"></a> ],
<a name="line_126"></a>
<a name="line_127"></a> ["Method","add_edge",
<a name="line_128"></a>  "Add an edge (represented by an instance of #domain_edge#) to the grid.  It is assumed that the endpoints of the edge have already been added.  Return the index of the added edge.",
<a name="line_129"></a>  proc(this,E::domain_edge)
<a name="line_130"></a>   local i,j,n;
<a name="line_131"></a>
<a name="line_132"></a>   n := this["num_edges"];
<a name="line_133"></a>   this["num_edges"] := n+1;
<a name="line_134"></a>   E["grid_index"] := n;
<a name="line_135"></a>
<a name="line_136"></a>   i,j := op(E["end_indices"]);
<a name="line_137"></a>   this["edges"][n] := eval(E);
<a name="line_138"></a>   this["edges_by_ends"][i,j] := eval(E);
<a name="line_139"></a>
<a name="line_140"></a>   return n;
<a name="line_141"></a>  end
<a name="line_142"></a> ],
<a name="line_143"></a>
<a name="line_144"></a> ["Method","add_new_edge",
<a name="line_145"></a>  "Create a new edge from point @i@ to point @j@, add it to the grid, and return the index.",
<a name="line_146"></a>  proc(this,i,j)
<a name="line_147"></a>   local E,k,X;
<a name="line_148"></a>
<a name="line_149"></a>   if i < 0 or i >= this["num_points"] or j < 0 or j >= this["num_points"] then
<a name="line_150"></a>    error "Indices out of bounds";
<a name="line_151"></a>   fi;
<a name="line_152"></a>
<a name="line_153"></a>   if i >= j then 
<a name="line_154"></a>    error "Indices out of order";
<a name="line_155"></a>   fi;
<a name="line_156"></a>
<a name="line_157"></a>   X := this["domain"];
<a name="line_158"></a>   E := eval(X["new_edge",eval(this["points"][i]),eval(this["points"][j])]);
<a name="line_159"></a>   this["add_edge",eval(E)];
<a name="line_160"></a>
<a name="line_161"></a>   return eval(E);
<a name="line_162"></a>  end
<a name="line_163"></a> ],
<a name="line_164"></a>
<a name="line_165"></a> ["Method","add_face",
<a name="line_166"></a>  "Add a face (represented by an instance of #domain_face#) to the grid.  It is assumed that the corners and sides of the face have already been added.  Return the index of the added face.",
<a name="line_167"></a>  proc(this,F::domain_face)
<a name="line_168"></a>   local i,j,k,n;
<a name="line_169"></a>
<a name="line_170"></a>   n := this["num_faces"];
<a name="line_171"></a>   this["num_faces"] := n+1;
<a name="line_172"></a>   F["grid_index"] := n;
<a name="line_173"></a>
<a name="line_174"></a>   i,j,k := op(F["corner_indices"]);
<a name="line_175"></a>   this["faces"][n] := eval(F);
<a name="line_176"></a>   this["faces_by_corners"][i,j,k] := eval(F);
<a name="line_177"></a>
<a name="line_178"></a>   return n;
<a name="line_179"></a>  end
<a name="line_180"></a> ],
<a name="line_181"></a>
<a name="line_182"></a> ["Method","add_new_face",
<a name="line_183"></a>  "Create a new face with corners at point @i@, point @j@ and point @k@.  Add the edges to the grid if this has not already been done, then add the face, and return the index of the new face.",
<a name="line_184"></a>  proc(this,i,j,k,s_)
<a name="line_185"></a>   local s,F,S0,S1,S2,X;
<a name="line_186"></a>
<a name="line_187"></a>   if i < 0 or i >= this["num_points"] or
<a name="line_188"></a>      j < 0 or j >= this["num_points"] or 
<a name="line_189"></a>      k < 0 or k >= this["num_points"] then
<a name="line_190"></a>    error "Indices out of bounds";
<a name="line_191"></a>   fi;
<a name="line_192"></a>
<a name="line_193"></a>   if i >= j or j >= k then 
<a name="line_194"></a>    error "Indices out of order";
<a name="line_195"></a>   fi;
<a name="line_196"></a>
<a name="line_197"></a>   if not(this["has_edge",i,j]) then this["add_new_edge",i,j]; fi;
<a name="line_198"></a>   if not(this["has_edge",i,k]) then this["add_new_edge",i,k]; fi;
<a name="line_199"></a>   if not(this["has_edge",j,k]) then this["add_new_edge",j,k]; fi;
<a name="line_200"></a>
<a name="line_201"></a>   S0 := eval(this["edges_by_ends"][i,j]);
<a name="line_202"></a>   S1 := eval(this["edges_by_ends"][i,k]);
<a name="line_203"></a>   S2 := eval(this["edges_by_ends"][j,k]);
<a name="line_204"></a>
<a name="line_205"></a>   s := `if`(nargs >= 5, s_, 0);
<a name="line_206"></a>
<a name="line_207"></a>   X := eval(this["domain"]);
<a name="line_208"></a>   F := eval(X["new_face",S0,S1,S2,s]);
<a name="line_209"></a>   this["add_face",eval(F)];
<a name="line_210"></a>  end
<a name="line_211"></a> ],
<a name="line_212"></a>
<a name="line_213"></a> ["Method","clone"::domain_grid,
<a name="line_214"></a>  "Return an independent copy of this grid, which can be changed without affecting the original.  The points, edges and faces of the new grid are also independent of the points, edges and faces of the old grid",
<a name="line_215"></a>  proc(this)
<a name="line_216"></a>   local G,np,ne,nf,i,ij,ijk,F,X;
<a name="line_217"></a>
<a name="line_218"></a>   X := eval(this["domain"]);
<a name="line_219"></a>   G := eval(X["new_grid"]);
<a name="line_220"></a>   G["domain"] := eval(this["domain"]);
<a name="line_221"></a>
<a name="line_222"></a>   np := this["num_points"];
<a name="line_223"></a>
<a name="line_224"></a>   for i from 0 to np - 1 do
<a name="line_225"></a>    G["points"][i] := G["points"][i]["clone"];
<a name="line_226"></a>   od;
<a name="line_227"></a>
<a name="line_228"></a>   G["num_points"] := np;
<a name="line_229"></a>
<a name="line_230"></a>   for ij in this["edge_indices"] do
<a name="line_231"></a>    G["add_new_edge",op(ij)];
<a name="line_232"></a>   od;
<a name="line_233"></a>
<a name="line_234"></a>   for ijk in this["face_indices"] do
<a name="line_235"></a>    F := eval(this["faces_by_corners"][op(ijk)]);
<a name="line_236"></a>    G["add_new_face",op(ijk),F["orientation"]];
<a name="line_237"></a>   od;
<a name="line_238"></a>
<a name="line_239"></a>   return eval(G);
<a name="line_240"></a>  end
<a name="line_241"></a> ],
<a name="line_242"></a>
<a name="line_243"></a> ["Method","calculate",
<a name="line_244"></a>  "Calculate various auxiliary quantities such as the @total_flat_area@ field.  Subclasses may override this method to perform more interesting work.",
<a name="line_245"></a>  proc(this)
<a name="line_246"></a>   local i,E,F;
<a name="line_247"></a>
<a name="line_248"></a>   this["total_flat_area"] := 0;
<a name="line_249"></a>   this["tension"] := 0;
<a name="line_250"></a>
<a name="line_251"></a>   for i from 0 to this["num_edges"] - 1 do
<a name="line_252"></a>    E := eval(this["edges"][i]);
<a name="line_253"></a>    E["calculate"];
<a name="line_254"></a>    this["tension"] := expand(this["tension"] + (0.25^2-E["straight_length"]^2)^2);
<a name="line_255"></a>   od;
<a name="line_256"></a>
<a name="line_257"></a>   for i from 0 to this["num_faces"] - 1 do
<a name="line_258"></a>    F := eval(this["faces"][i]);
<a name="line_259"></a>    F["calculate"];
<a name="line_260"></a>    this["total_flat_area"] := this["total_flat_area"] + F["flat_area"];
<a name="line_261"></a>   od;
<a name="line_262"></a>
<a name="line_263"></a>   NULL;
<a name="line_264"></a>  end
<a name="line_265"></a> ],
<a name="line_266"></a>
<a name="line_267"></a> ["Method","set_var_indices",
<a name="line_268"></a>  "Set the @var_index@ fields of all the points in the grid.",
<a name="line_269"></a>  proc(this)
<a name="line_270"></a>   local i,nv,P;
<a name="line_271"></a>
<a name="line_272"></a>   nv := 0;
<a name="line_273"></a>   for i from 0 to this["num_points"] - 1 do
<a name="line_274"></a>    P := eval(this["points"][i]);
<a name="line_275"></a>    if P["constraint"] >= CONSTRAINT_FIXED then
<a name="line_276"></a>     P["var_index"] := NULL;
<a name="line_277"></a>    else
<a name="line_278"></a>     P["var_index"] := nv;
<a name="line_279"></a>     if P["constraint"] = CONSTRAINT_FREE then
<a name="line_280"></a>      nv := nv + 2;
<a name="line_281"></a>     else
<a name="line_282"></a>      nv := nv + 1;
<a name="line_283"></a>     fi;
<a name="line_284"></a>    fi;
<a name="line_285"></a>   od;
<a name="line_286"></a>
<a name="line_287"></a>   this["num_vars"] := nv;
<a name="line_288"></a>  end
<a name="line_289"></a> ],
<a name="line_290"></a>
<a name="line_291"></a> ["Method","raw_adjust",
<a name="line_292"></a>  "This method sets the coordinates of all points in the grid to symbolic values involving variables @t[j]@.  The idea is that we can then solve for the values of these variables that minimise some objective function, and then move the grid points accordingly.  Note that in order to isolate the variables @t[j]@ from any existing meaning of the symbol @t@, we have declared @t@ to be a local variable in this procedure.  This creates a potentially confusing situation, because this local variable will exist outside the procedure and will be visually indistinguishable from the global symbol @t@.  In order to refer to the correct variables when solving, we need to use the @indets()@ function.",
<a name="line_293"></a>  proc(this)
<a name="line_294"></a>   local t,i,j,P;
<a name="line_295"></a>
<a name="line_296"></a>   this["vars"] := {seq(t[i],i=0..this["num_vars"]-1)};
<a name="line_297"></a>
<a name="line_298"></a>   for i from 0 to this["num_points"] - 1 do
<a name="line_299"></a>    P := eval(this["points"][i]);
<a name="line_300"></a>    j := P["var_index"];
<a name="line_301"></a>    if P["constraint"] < CONSTRAINT_FIXED then
<a name="line_302"></a>     if P["constraint"] = CONSTRAINT_FREE then
<a name="line_303"></a>      P["x"] := P["x"] +~ t[j] *~ P["u"] +~ t[j+1] *~ P["v"];
<a name="line_304"></a>     else
<a name="line_305"></a>      P["x"] := P["x"] +~ t[j] *~ P["u"];
<a name="line_306"></a>     fi;
<a name="line_307"></a>    fi;
<a name="line_308"></a>   od;
<a name="line_309"></a>
<a name="line_310"></a>   NULL;
<a name="line_311"></a>  end
<a name="line_312"></a> ],
<a name="line_313"></a>
<a name="line_314"></a> ["Method","subdivide"::void,
<a name="line_315"></a>  "This method subdivides the grid by adding new points at the midpoints of all the original edges, and reconnecting everything in an obvious way.  The old information is destroyed by default, so one should use the @clone@ method first if one wants to retain it.",
<a name="line_316"></a>  proc(this) 
<a name="line_317"></a>   local X,np,ne,nf,old_edges,old_faces,
<a name="line_318"></a>    midpoints,edge_parts,cuts,face_parts,E,P0,P1,Q,F,s,
<a name="line_319"></a>    i,j,k,i0,i1,i2,j0,j1,j2;
<a name="line_320"></a>
<a name="line_321"></a>   X := eval(this["domain"]);
<a name="line_322"></a>
<a name="line_323"></a>   np := this["num_points"];
<a name="line_324"></a>   ne := this["num_edges"];
<a name="line_325"></a>   nf := this["num_faces"];
<a name="line_326"></a>
<a name="line_327"></a>   old_edges := eval(this["edges"]);
<a name="line_328"></a>   old_faces := eval(this["faces"]);
<a name="line_329"></a>   this["edges"] := table();
<a name="line_330"></a>   this["faces"] := table();
<a name="line_331"></a>   this["num_edges"] := 0;
<a name="line_332"></a>   this["num_faces"] := 0;
<a name="line_333"></a>
<a name="line_334"></a>   midpoints  := table(); # Midpoints of old edges
<a name="line_335"></a>   edge_parts := table(); # New edges that cover half of an old edge
<a name="line_336"></a>   cuts       := table(); # New edges that cut across an old face
<a name="line_337"></a>   face_parts := table(); # New faces that cover a quarter of an old face
<a name="line_338"></a>
<a name="line_339"></a>   for j from 0 to ne - 1 do;
<a name="line_340"></a>    E := eval(old_edges[j]);
<a name="line_341"></a>    P0 := eval(E["end"][0]);
<a name="line_342"></a>    P1 := eval(E["end"][1]);
<a name="line_343"></a>
<a name="line_344"></a>    Q := eval(this["domain"]["new_point"]);
<a name="line_345"></a>    Q["set",
<a name="line_346"></a>      bitwise_and(P0["constraint"],P1["constraint"]),
<a name="line_347"></a>      0.5 *~ (P0["x"] +~ P1["x"])
<a name="line_348"></a>     ];
<a name="line_349"></a>
<a name="line_350"></a>    midpoints[j] := eval(Q);
<a name="line_351"></a>    edge_parts[j,0] := eval(X["new_edge",P0,Q]);
<a name="line_352"></a>    edge_parts[j,1] := eval(X["new_edge",P1,Q]);
<a name="line_353"></a>
<a name="line_354"></a>    this["add_point",eval(midpoints[j])];
<a name="line_355"></a>    this["add_edge",eval(edge_parts[j,0])];
<a name="line_356"></a>    this["add_edge",eval(edge_parts[j,1])];
<a name="line_357"></a>   od;
<a name="line_358"></a>
<a name="line_359"></a>   # We now add in all the subdivided faces.  Note that to keep everything
<a name="line_360"></a>   # in lexicographic order, we add the three corner pieces for each of
<a name="line_361"></a>   # the old faces before adding the centre pieces for any of the old faces.
<a name="line_362"></a>
<a name="line_363"></a>   for k from 0 to nf - 1 do
<a name="line_364"></a>    F := eval(old_faces[k]);
<a name="line_365"></a>    j0 := F["side"][0]["grid_index"];
<a name="line_366"></a>    j1 := F["side"][1]["grid_index"];
<a name="line_367"></a>    j2 := F["side"][2]["grid_index"];
<a name="line_368"></a>    i0 := old_edges[j0]["end"][0];
<a name="line_369"></a>    i1 := old_edges[j0]["end"][1];
<a name="line_370"></a>    i2 := old_edges[j2]["end"][1];
<a name="line_371"></a>
<a name="line_372"></a>    cuts[k,0] := eval(X["new_edge",midpoints[j0],midpoints[j1]]);
<a name="line_373"></a>    cuts[k,1] := eval(X["new_edge",midpoints[j0],midpoints[j2]]);
<a name="line_374"></a>    cuts[k,2] := eval(X["new_edge",midpoints[j1],midpoints[j2]]);
<a name="line_375"></a>
<a name="line_376"></a>    s := F["orientation"];
<a name="line_377"></a>
<a name="line_378"></a>    face_parts[k,0] := eval(X["new_face",edge_parts[j0,0],edge_parts[j1,0],cuts[k,0], s]);
<a name="line_379"></a>    face_parts[k,1] := eval(X["new_face",edge_parts[j0,1],edge_parts[j2,0],cuts[k,1],-s]);
<a name="line_380"></a>    face_parts[k,2] := eval(X["new_face",edge_parts[j1,1],edge_parts[j2,1],cuts[k,2], s]);
<a name="line_381"></a>
<a name="line_382"></a>    face_parts[k,3] := eval(X["new_face",cuts[k,0],cuts[k,1],cuts[k,2],-s]);
<a name="line_383"></a>
<a name="line_384"></a>    for i from 0 to 2 do this["add_edge",cuts[k,i]]; od;
<a name="line_385"></a>    for i from 0 to 2 do this["add_face",face_parts[k,i]]; od;
<a name="line_386"></a>   od;
<a name="line_387"></a>
<a name="line_388"></a>   for k from 0 to nf - 1 do
<a name="line_389"></a>    this["add_face",face_parts[k,3]];
<a name="line_390"></a>   od;
<a name="line_391"></a>
<a name="line_392"></a>   this["set_var_indices"];
<a name="line_393"></a>   this["calculate"];
<a name="line_394"></a>   NULL;
<a name="line_395"></a>  end
<a name="line_396"></a> ],
<a name="line_397"></a>
<a name="line_398"></a> ["Method","setup",
<a name="line_399"></a>  "This method takes an argument @coords@ which should be a list of lists, all of the same length, giving a rectangular array.  Both dimensions of this array should be even.  Each element of the inner lists should itself be a list, giving coordinates of a point in the domain for this grid.  The @setup@ method creates a point for each entry in the array.  Points in the first row are constrained to lie on $C_1$, those in the last row are constrained to lie in $C_3$, those in the first column are constrained to lie on $C_0$, and those in the last column are constrained to lie on $C_5$.  The corners are constrained in the obvious way compatible with this.  Edges and faces are added in a slightly non-obvious pattern to ensure that no edge crosses a corner to join points on different sides of the domain. ",
<a name="line_400"></a>  proc(this,coords::list(list(list(numeric))))
<a name="line_401"></a>   local mm,nn,m,n,i,j,EI,FI,p,c,e,f;
<a name="line_402"></a>
<a name="line_403"></a>   mm := nops(coords);
<a name="line_404"></a>   nn := op({op(map(nops,coords))});
<a name="line_405"></a>   if nops([nn]) <> 1 then
<a name="line_406"></a>    error("Ragged coordinate table");
<a name="line_407"></a>   fi;
<a name="line_408"></a>   if not(type(nn,odd) and type(mm,odd)) then
<a name="line_409"></a>    error("Dimensions not both odd");
<a name="line_410"></a>   fi;
<a name="line_411"></a>
<a name="line_412"></a>   m := (mm - 1)/2;
<a name="line_413"></a>   n := (nn - 1)/2;
<a name="line_414"></a>   p := (i,j) -> i + j*nn;
<a name="line_415"></a>   for j from 0 to mm - 1 do
<a name="line_416"></a>    for i from 0 to nn - 1 do
<a name="line_417"></a>     c := constraint_for_square(i/(nn-1),j/(mm-1));
<a name="line_418"></a>     this["add_new_point",c,coords[j+1][i+1]];
<a name="line_419"></a>    od;
<a name="line_420"></a>   od;
<a name="line_421"></a>
<a name="line_422"></a>   FI := [
<a name="line_423"></a>     seq(seq([p(i,j),p(i+1,j),p(i+1,j+1)],i=0..n-1),j=0..m-1),
<a name="line_424"></a>     seq(seq([p(i,j),p(i,j+1),p(i+1,j+1)],i=0..n-1),j=0..m-1),
<a name="line_425"></a>     seq(seq([p(2*n-i,j),p(2*n-i,j+1),p(2*n-i-1,j+1)],i=0..n-1),j=0..m-1),
<a name="line_426"></a>     seq(seq([p(2*n-i,j),p(2*n-i-1,j),p(2*n-i-1,j+1)],i=0..n-1),j=0..m-1),
<a name="line_427"></a>     seq(seq([p(i,2*m-j),p(i+1,2*m-j),p(i+1,2*m-j-1)],i=0..n-1),j=0..m-1),
<a name="line_428"></a>     seq(seq([p(i,2*m-j),p(i,2*m-j-1),p(i+1,2*m-j-1)],i=0..n-1),j=0..m-1),
<a name="line_429"></a>     seq(seq([p(2*n-i,2*m-j),p(2*n-i-1,2*m-j),p(2*n-i-1,2*m-j-1)],i=0..n-1),j=0..m-1),
<a name="line_430"></a>     seq(seq([p(2*n-i,2*m-j),p(2*n-i,2*m-j-1),p(2*n-i-1,2*m-j-1)],i=0..n-1),j=0..m-1)   
<a name="line_431"></a>   ];
<a name="line_432"></a>
<a name="line_433"></a>   FI := sort(map(sort,FI));
<a name="line_434"></a>
<a name="line_435"></a>   EI := map(F -> [[F[1],F[2]],[F[1],F[3]],[F[2],F[3]]],FI);
<a name="line_436"></a>   EI := map(op,EI);
<a name="line_437"></a>   EI := {op(EI)};
<a name="line_438"></a>   EI := [op(EI)];
<a name="line_439"></a>   EI := sort(EI);
<a name="line_440"></a>
<a name="line_441"></a>   for e in EI do this["add_new_edge",op(e)]; od;
<a name="line_442"></a>   for f in FI do this["add_new_face",op(f)]; od;
<a name="line_443"></a>
<a name="line_444"></a>   this["set_var_indices"];
<a name="line_445"></a>   this["calculate"];
<a name="line_446"></a>   NULL;
<a name="line_447"></a>  end
<a name="line_448"></a> ],
<a name="line_449"></a>
<a name="line_450"></a> ["Method","edge_plot",
<a name="line_451"></a>  "This method returns a plot structure displaying the edges of the grid.  By default, it just uses the raw coordinates of the grid points (which will cause an error if the dimension is larger than three).  However, one can supply a map @proj_@ as an argument, and that map will then be applied to the coordinates before plotting.  Additional options for the @display()@ command can also be given as a second argument to the method.",
<a name="line_452"></a>  proc(this,proj_,opts__)
<a name="line_453"></a>   local L,n,i,E,x0,x1,k,proj,opts;
<a name="line_454"></a>
<a name="line_455"></a>   if nargs > 1 then proj := eval(proj_); else proj := (x -> x); fi;
<a name="line_456"></a>   if nargs > 2 then opts := args[3..-1]; else opts := NULL; fi;
<a name="line_457"></a>
<a name="line_458"></a>   L := NULL;
<a name="line_459"></a>   n := this["num_edges"];
<a name="line_460"></a>   for i from 0 to n-1 do
<a name="line_461"></a>    E := this["edges"][i];
<a name="line_462"></a>    x0 := proj(E["end"][0]["x"]);
<a name="line_463"></a>    x1 := proj(E["end"][1]["x"]);
<a name="line_464"></a>    k := E["curve_index"];
<a name="line_465"></a>    if k <> NULL and member(k,{0,1,3,5}) then
<a name="line_466"></a>     L := L,line(x0,x1,colour=c_colour[k]);
<a name="line_467"></a>    else 
<a name="line_468"></a>     L := L,line(x0,x1,colour=black);
<a name="line_469"></a>    fi;
<a name="line_470"></a>   od;
<a name="line_471"></a>
<a name="line_472"></a>   display(L,scaling=constrained,axes=none,opts);
<a name="line_473"></a>  end
<a name="line_474"></a> ],
<a name="line_475"></a>
<a name="line_476"></a> NULL
<a name="line_477"></a>);
  </pre>
 </body>
</html>
    