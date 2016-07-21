#@ CLASS: net

`Class/Declare`("net",
 "An instance of this class represents a net which can be glued to produce a cromulent surface.  ",

 ["Constructor","",
  proc(this)
   this["v"] := table();
   this["v_anchor"] := table():
   this["squares"] := table():
   this["square_centres"] := table():
  end
 ],

 ["Field","name"::string,"A name for the net"],

 ["Field","v"::table,"This is a table of points in $\\mathbb{R}^2$.  The indices are integers in $\\{0,1,\\dotsc,13\\}$, or integers in that range plus a multiple of $0.1$.  The idea is that vertices indexed 10, 10.1, 10.2 and so on will all be identified together and will become the vertex $v_{10}$ in the glued surface"],

 ["Field","edges"::list([numeric,numeric]),"This is a list of pairs.  A pair @[i,j]@ appears in the list if @i@ and @j@ are indices in the @v@ table, and there is an edge from vertex @i@ to vertex @j@ in the net."],

 ["Field","outer_edges"::list([numeric,numeric]),"This is a list containing some of the entries in the @edges@ list, namely those that lie on the boundary of the net.  It can be set by the @set_outer_edges@ method."],

 ["Field","squares"::table,"This is a table indexed by the 16 elements of the group $G$.  Each entry is a list of four indices from the @v@ table.  If $g\\in G$ and $F$ is the fundamental domain, then the net will have a quadrilateral region corresponding to $g.F$, and the corners of that region are indexed by the elements of the $g$'th element in the @squares@ table.  These indices should be listed so that they correspond to $g.v_6$, $g.v_0$, $g.v_{11}$ and $g.v_3$ in that order."],
 
 ["Field","square_centres"::table,"This is a table indexed by the 16 elements of the group $G$, withentries in $\\mathbb{R}^2$.  The $g$'th entry is the barycentre of the quadrilateral region in the net corresponding to $g.F$."],

 ["Field","tikz_scale"::numeric = 1,"This is the scale parameter that should be used when generating a tikzpicture environment illustrating this net."],
 
 ["Field","v_anchor"::table,"This is a table with the same indices as the @v@ table.  The entries are strings like \"north\", \"south east\" and so on.  These entries indicate where the relevant vertex labels should be placed relative to the vertices themselves."],

 ["Method","set_edges_from_squares","If the @squares@ table has been filled, then this method can be used to fill the @edges@ list.",
  proc(this)
   local Q,E;

   E := map(Q ->[[Q[1],Q[2]],[Q[2],Q[3]],[Q[3],Q[4]],[Q[4],Q[1]]],
	    {seq(this["squares"][T],T in G16)});
   E := sort(map(sort,map(op,E)));
   this["edges"] := [op(E)];
  end
 ],

 ["Method","set_square_centres","This fills the @square_centres@ table using information from the @v@ and @squares@ tables.",
  proc(this)
   local T,Q;

   for T in G16 do 
    Q := this["squares"][T];
     this["square_centres"][T] := 
     this["v"][Q[1]] /~ 4 +~
     this["v"][Q[2]] /~ 4 +~
     this["v"][Q[3]] /~ 4 +~
     this["v"][Q[4]] /~ 4;
   od:
  end
 ],

 ["Method","set_outer_edges","This works out which edges lie on the boundary of the net.",
  proc(this)
   local E,S,N,T,Q,e;

   E := this["edges"];
   S := this["squares"];
   N := table();
   for e in E do N[e] := 0; od;
   for T in G16 do
    Q := S[T];
    e := sort([Q[1],Q[2]]); N[e] := N[e] + 1;
    e := sort([Q[2],Q[3]]); N[e] := N[e] + 1;
    e := sort([Q[3],Q[4]]); N[e] := N[e] + 1;
    e := sort([Q[4],Q[1]]); N[e] := N[e] + 1;
   od;
   this["outer_edges"] := select(e -> (N[e] = 1),E);
  end
 ],

 ["Method","plot","This generates a Maple plot illustrating the net.",
  proc(this)
   local V;

   V := eval(this["v"]);

   display(map(e -> line(V[e[1]],V[e[2]],colour=edge_colour(e)),
               this["edges"]),
    seq(textplot([op(V[i]),i]),
        i in map(op,[indices(V)])),
    seq(textplot([op(this["square_centres"][T]),T]),T in G16),
    axes = none
   );
  end
 ],

 ["Method","save_plot","This calls the @plot@ method and saves the result, both as en entry in the global variable @pics@ and as a file in the @plots@ directory",
  proc(this)
   global pics;
   pics[this["name"]] := this["plot"];
   save_plot(this["name"]);
  end
 ],

 ["Method","outer_edge_plot","This generates a Maple plot showing the outer edges of the net.",
  proc(this)
   local V,W,E;

   V := eval(this["v"]);
   E := this["outer_edges"];
   W := {op(map(op,E))};

   display(map(e -> line(V[e[1]],V[e[2]],colour=edge_colour(e)),
               this["outer_edges"]),
    seq(textplot([op(V[i]),i]),i in W),
    axes = none
   );
  end
 ],

 ["Method","tikz","This generates a tikzpicture environment illustrating the net.",
  proc(this,scale_)
   local scale,s,e,i,ii,aa,T,V,E;

   V := eval(this["v"]);
   E := eval(this["edges"]);

   scale := `if`(nargs > 1, scale_, this["tikz_scale"]);

   s := "% Generated by net_square_tikz()\n";
   s := cat(s,sprintf("\\begin{tikzpicture}[scale=%A]\n",scale));
   for e in E do
    s := cat(s,sprintf(" \\draw[%7A] (%3d,%3d) -- (%3d,%3d);\n",
                       edge_colour(e),op(V[e[1]]),op(V[e[2]])));
   od:
   for i in map(op,[indices(V)]) do
    ii := `if`(0<i and i<1,sprintf("%1.1f",i),sprintf("%A",i));
    aa := this["v_anchor"][i];
    if type(aa,string) then
     aa := cat("[anchor=",aa,"]");
    else
     aa := "";
    fi;
    s := cat(s,sprintf(" \\draw (%3d,%3d) node%s{$\\ss %s$};\n",op(V[i]),aa,ii));
   od;
   for T in G16 do
    s := cat(s,sprintf(" \\draw (%3.2f,%3.2f) node{$%s$};\n",
                       op(this["square_centres"][T]),G_latex[T]));
   od;
   s := cat(s,"\\end{tikzpicture}\n");
   return(s);
  end
 ],

 ["Method","save_tikz","This calls the @tikz@ method and saves the result",
  proc(this)
   save_tikz(this["name"],this["tikz"]);
  end
 ],

 ["Method","check","This checks the combinatorial structure of the net.  It uses the global variable @edges@, which is set in @cromulent.mpl@",
  proc(this)
   local T;

   _ASSERT(
    sort([op({op(map(u -> map(floor,u),this["edges"]))})]) = edges,
    sprintf("Net %s: correct edge set",this["name"])
   );

   for T in G16 do
    _ASSERT(
     map(act_V[T],[6,0,11,3]) = map(floor,this["squares"][T]),
     sprintf("Net %s, square %A: correct vertices",this["name"],T)
    );
   od;
  end
 ]
);

######################################################################

#@ net_0 
net_0 := `new/net`();
net_0["name"] := "net_0";

net_0["v"] := table():

net_0["v"][  0  ] := [  0,   0]:
net_0["v"][  1  ] := [ 12,  12]:
net_0["v"][  1.1] := [-12,  12]:
net_0["v"][  1.2] := [-12, -12]:
net_0["v"][  1.3] := [ 12, -12]:
net_0["v"][  2  ] := [  4,   8]:
net_0["v"][  2.1] := [  4,  -8]:
net_0["v"][  3  ] := [  8,   4]:
net_0["v"][  3.1] := [ -8,   4]:
net_0["v"][  4  ] := [ -4,   8]:
net_0["v"][  4.1] := [ -4,  -8]:
net_0["v"][  5  ] := [  8,  -4]:
net_0["v"][  5.1] := [ -8,  -4]:
net_0["v"][  6  ] := [  6,   6]:
net_0["v"][  7  ] := [ -6,   6]:
net_0["v"][  8  ] := [ -6,  -6]:
net_0["v"][  9  ] := [  6,  -6]:
net_0["v"][ 10  ] := [  0,   8]:
net_0["v"][ 10.1] := [  0,  -8]:
net_0["v"][ 11  ] := [  8,   0]:
net_0["v"][ 11.1] := [ -8,   0]:
net_0["v"][ 12  ] := [  4,  12]:
net_0["v"][ 12.1] := [ -4, -12]:
net_0["v"][ 12.2] := [ -4,  12]:
net_0["v"][ 12.3] := [  4, -12]:
net_0["v"][ 13  ] := [ 12,   4]:
net_0["v"][ 13.1] := [-12,  -4]:
net_0["v"][ 13.2] := [ 12,  -4]:
net_0["v"][ 13.3] := [-12,   4]:

net_0["v_anchor"][ 0  ] := "north";
net_0["v_anchor"][ 1  ] := "south west";
net_0["v_anchor"][ 1.1] := "south east";
net_0["v_anchor"][ 1.2] := "north east";
net_0["v_anchor"][ 1.3] := "north west";
net_0["v_anchor"][ 2  ] := "south east";
net_0["v_anchor"][ 2.1] := "north east";
net_0["v_anchor"][ 3.1] := "north east";
net_0["v_anchor"][ 3  ] := "north west";
net_0["v_anchor"][ 4.1] := "north west";
net_0["v_anchor"][ 4  ] := "south west";
net_0["v_anchor"][ 5  ] := "south west";
net_0["v_anchor"][ 5.1] := "south east";
net_0["v_anchor"][ 6  ] := "north";
net_0["v_anchor"][ 7  ] := "north";
net_0["v_anchor"][ 8  ] := "north";
net_0["v_anchor"][ 9  ] := "north";
net_0["v_anchor"][10  ] := "south";
net_0["v_anchor"][10.1] := "north";
net_0["v_anchor"][11  ] := "west";
net_0["v_anchor"][11.1] := "east";
net_0["v_anchor"][12.2] := "south west";
net_0["v_anchor"][12.3] := "north east";
net_0["v_anchor"][12  ] := "south east";
net_0["v_anchor"][12.1] := "north west";
net_0["v_anchor"][13  ] := "north west";
net_0["v_anchor"][13.1] := "south east";
net_0["v_anchor"][13.2] := "south west";
net_0["v_anchor"][13.3] := "north east";

net_0["squares"] := table([
    1 = [6, 0  , 11  , 3  ],
    L = [7, 0  , 10  , 4  ],
   LL = [8, 0  , 11.1, 5.1],
  LLL = [9, 0  , 10.1, 2.1],
    M = [9, 1.3, 13.2, 5  ],
   LM = [6, 1  , 12  , 2  ],
  LLM = [7, 1.1, 13.3, 3.1],
 LLLM = [8, 1.2, 12.1, 4.1],
    N = [9, 0  , 11  , 5  ],
   LN = [6, 0  , 10  , 2  ],
  LLN = [7, 0  , 11.1, 3.1],
 LLLN = [8, 0  , 10.1, 4.1],
   MN = [6, 1  , 13  , 3  ],
  LMN = [7, 1.1, 12.2, 4  ],
 LLMN = [8, 1.2, 13.1, 5.1],
LLLMN = [9, 1.3, 12.3, 2.1]
]):

net_0["set_edges_from_squares"];
net_0["tikz_scale"] := 0.5;
net_0["set_outer_edges"];
net_0["set_square_centres"];

######################################################################

#@ net_1 
net_1 := `new/net`();
net_1["name"] := "net_1";
net_1["v"] := table():

net_1["v"][  0  ] := [  4,   4]:
net_1["v"][  0.1] := [ -4,   4]:
net_1["v"][  0.2] := [ -4,  -4]:
net_1["v"][  0.3] := [  4,  -4]:
net_1["v"][  1  ] := [  2,   2]:
net_1["v"][  1.1] := [ -2,   2]:
net_1["v"][  1.2] := [ -2,  -2]:
net_1["v"][  1.3] := [  2,  -2]:
net_1["v"][  2  ] := [  0,   3]:
net_1["v"][  3  ] := [ -3,   0]:
net_1["v"][  4  ] := [  0,  -3]:
net_1["v"][  5  ] := [  3,   0]:
net_1["v"][  6  ] := [ -3,   3]:
net_1["v"][  7  ] := [ -3,  -3]:
net_1["v"][  8  ] := [  3,  -3]:
net_1["v"][  9  ] := [  3,   3]:
net_1["v"][ 10  ] := [  0,   4]:
net_1["v"][ 10.1] := [  0,  -4]:
net_1["v"][ 11  ] := [  4,   0]:
net_1["v"][ 11.1] := [ -4,   0]:
net_1["v"][ 12  ] := [  0,   2]:
net_1["v"][ 12.1] := [  0,  -2]:
net_1["v"][ 13  ] := [  2,   0]:
net_1["v"][ 13.1] := [ -2,   0]:

net_1["v_anchor"][  0  ] := "south west";
net_1["v_anchor"][  0.1] := "south east";
net_1["v_anchor"][  0.2] := "north east";
net_1["v_anchor"][  0.3] := "north west";
net_1["v_anchor"][  1  ] := "north east";
net_1["v_anchor"][  1.1] := "north west";
net_1["v_anchor"][  1.2] := "south west";
net_1["v_anchor"][  1.3] := "south east";
net_1["v_anchor"][ 10  ] := "south";
net_1["v_anchor"][ 10.1] := "north";
net_1["v_anchor"][ 11  ] := "west";
net_1["v_anchor"][ 11.1] := "east";
net_1["v_anchor"][ 12  ] := "north";
net_1["v_anchor"][ 12.1] := "south";
net_1["v_anchor"][ 13  ] := "east";
net_1["v_anchor"][ 13.1] := "west";

net_1["squares"] := table([
    1 = [6,0.1,11.1,3],
    L = [7,0.2,10.1,4],
   LL = [8,0.3,11  ,5],
  LLL = [9,0  ,10  ,2],
    M = [9,1  ,13  ,5],
   LM = [6,1.1,12  ,2],
  LLM = [7,1.2,13.1,3],
 LLLM = [8,1.3,12.1,4],
    N = [9,0  ,11  ,5],
   LN = [6,0.1,10  ,2],
  LLN = [7,0.2,11.1,3],
 LLLN = [8,0.3,10.1,4],
   MN = [6,1.1,13.1,3],
  LMN = [7,1.2,12.1,4],
 LLMN = [8,1.3,13  ,5],
LLLMN = [9,1  ,12  ,2]
]):

net_1["set_edges_from_squares"];
net_1["set_outer_edges"];
net_1["set_square_centres"];

######################################################################

#@ net_2 
net_2 := `new/net`();
net_2["name"] := "net_2";
net_2["v"] := table():

net_2["v"][  0  ] := [ -4,   0]:
net_2["v"][  0.1] := [ -4,   8]:
net_2["v"][  0.2] := [ -4,  -8]:
net_2["v"][  1  ] := [  4,   0]:
net_2["v"][  1.1] := [  4,   8]:
net_2["v"][  1.2] := [  4,  -8]:
net_2["v"][  2  ] := [  0,   4]:
net_2["v"][  3  ] := [  0,  -4]:
net_2["v"][  4  ] := [  8,   4]:
net_2["v"][  4.1] := [ -8,   4]:
net_2["v"][  5  ] := [  8,  -4]:
net_2["v"][  5.1] := [ -8,  -4]:
net_2["v"][  6  ] := [  0,   0]:
net_2["v"][  7  ] := [  8,   1]:
net_2["v"][  7.1] := [ -8,   1]:
net_2["v"][  7.2] := [  0,  -8]:
net_2["v"][  8  ] := [  8,   8]:
net_2["v"][  8.1] := [ -8,   8]:
net_2["v"][  8.2] := [ -8,  -8]:
net_2["v"][  8.3] := [  8,  -8]:
net_2["v"][  9  ] := [  8,  -1]:
net_2["v"][  9.1] := [ -8,  -1]:
net_2["v"][  9.2] := [  0,   8]:
net_2["v"][ 10  ] := [ -4,   4]:
net_2["v"][ 11  ] := [ -4,  -4]:
net_2["v"][ 12  ] := [  4,   4]:
net_2["v"][ 13  ] := [  4,  -4]:

net_2["v_anchor"][  0  ] := "east";
net_2["v_anchor"][  0.1] := "south";
net_2["v_anchor"][  0.2] := "north";
net_2["v_anchor"][  1  ] := "west";
net_2["v_anchor"][  1.1] := "south";
net_2["v_anchor"][  1.2] := "north";
net_2["v_anchor"][  4  ] := "west";
net_2["v_anchor"][  4.1] := "east";
net_2["v_anchor"][  5  ] := "west";
net_2["v_anchor"][  5.1] := "east";
net_2["v_anchor"][  7  ] := "west";
net_2["v_anchor"][  7.1] := "east";
net_2["v_anchor"][  7.2] := "north";
net_2["v_anchor"][  8  ] := "south west";
net_2["v_anchor"][  8.1] := "south east";
net_2["v_anchor"][  8.2] := "north east";
net_2["v_anchor"][  8.3] := "north west";
net_2["v_anchor"][  9  ] := "west";
net_2["v_anchor"][  9.1] := "east";
net_2["v_anchor"][  9.2] := "south";

net_2["squares"] := table([
    1 = [ 6  ,0  , 11  , 3  ], 
    L = [ 7.1,0  , 10  , 4.1], 
   LL = [ 8.2,0.2, 11  , 5.1], 
  LLL = [ 9.2,0.1, 10  , 2  ], 
    M = [ 9  ,1  , 13  , 5  ], 
   LM = [ 6  ,1  , 12  , 2  ], 
  LLM = [ 7.2,1.2, 13  , 3  ], 
 LLLM = [ 8  ,1.1, 12  , 4  ], 
    N = [ 9.1,0  , 11  , 5.1], 
   LN = [ 6  ,0  , 10  , 2  ], 
  LLN = [ 7.2,0.2, 11  , 3  ], 
 LLLN = [ 8.1,0.1, 10  , 4.1], 
   MN = [ 6  ,1  , 13  , 3  ], 
  LMN = [ 7  ,1  , 12  , 4  ], 
 LLMN = [ 8.3,1.2, 13  , 5  ], 
LLLMN = [ 9.2,1.1, 12  , 2  ]
]):

net_2["tikz_scale"] := 0.5;
net_2["set_edges_from_squares"];
net_2["set_outer_edges"];
net_2["set_square_centres"];

######################################################################

#@ net_3 
net_3 := `new/net`();
net_3["name"] := "net_3";
net_3["v"] := table():

net_3["v"][  0  ] := [  8,   4]:
net_3["v"][  0.1] := [ -8,   4]:
net_3["v"][  0.2] := [ -8,  -4]:
net_3["v"][  0.3] := [  8,  -4]:
net_3["v"][  1  ] := [  0,   4]:
net_3["v"][  1.1] := [  0,  -4]:
net_3["v"][  2  ] := [  4,   8]:
net_3["v"][  2.1] := [ -4,   8]:
net_3["v"][  3  ] := [ -4,   0]:
net_3["v"][  4  ] := [  4,  -8]:
net_3["v"][  4.1] := [ -4,  -8]:
net_3["v"][  5  ] := [  4,   0]:
net_3["v"][  6  ] := [ -4,   4]:
net_3["v"][  7  ] := [ -4,  -4]:
net_3["v"][  8  ] := [  4,  -4]:
net_3["v"][  9  ] := [  4,   4]:
net_3["v"][ 10  ] := [  8,   8]:
net_3["v"][ 10.1] := [ -8,   8]:
net_3["v"][ 10.2] := [ -8,  -8]:
net_3["v"][ 10.3] := [  8,  -8]:
net_3["v"][ 11  ] := [  8,   0]:
net_3["v"][ 11.1] := [ -8,   0]:
net_3["v"][ 12  ] := [  1,   8]:
net_3["v"][ 12.1] := [ -1,   8]:
net_3["v"][ 12.2] := [ -1,  -8]:
net_3["v"][ 12.3] := [  1,  -8]:
net_3["v"][ 13  ] := [  0,   0]:

net_3["v_anchor"][  0  ] := "west";
net_3["v_anchor"][  0.1] := "east";
net_3["v_anchor"][  0.2] := "east";
net_3["v_anchor"][  0.3] := "west";
net_3["v_anchor"][  1  ] := "south";
net_3["v_anchor"][  1.1] := "north";
net_3["v_anchor"][  2  ] := "south";
net_3["v_anchor"][  2.1] := "south";
net_3["v_anchor"][  4  ] := "north";
net_3["v_anchor"][  4.1] := "north";
net_3["v_anchor"][ 10  ] := "south west";
net_3["v_anchor"][ 10.1] := "south east";
net_3["v_anchor"][ 10.2] := "north east";
net_3["v_anchor"][ 10.3] := "north west";
net_3["v_anchor"][ 11  ] := "west";
net_3["v_anchor"][ 11.1] := "east";
net_3["v_anchor"][ 12  ] := "south";
net_3["v_anchor"][ 12.1] := "south";
net_3["v_anchor"][ 12.2] := "north";
net_3["v_anchor"][ 12.3] := "north";

net_3["squares"] := table([
    1 = [ 6  ,0.1, 11.1, 3  ], 
    L = [ 7  ,0.2, 10.2, 4.1], 
   LL = [ 8  ,0.3, 11  , 5  ], 
  LLL = [ 9  ,0  , 10  , 2  ], 
    M = [ 9  ,1  , 13  , 5  ], 
   LM = [ 6  ,1  , 12.1, 2.1], 
  LLM = [ 7  ,1.1, 13  , 3  ], 
 LLLM = [ 8  ,1.1, 12.3, 4  ], 
    N = [ 9  ,0  , 11  , 5  ], 
   LN = [ 6  ,0.1, 10.1, 2.1], 
  LLN = [ 7  ,0.2, 11.1, 3  ], 
 LLLN = [ 8  ,0.3, 10.3, 4  ], 
   MN = [ 6  ,1  , 13  , 3  ], 
  LMN = [ 7  ,1.1, 12.2, 4.1], 
 LLMN = [ 8  ,1.1, 13  , 5  ], 
LLLMN = [ 9  ,1  , 12  , 2  ]
]):

net_3["tikz_scale"] := 0.5;
net_3["set_edges_from_squares"];
net_3["set_outer_edges"];
net_3["set_square_centres"];

######################################################################

#@ net_5 
net_5 := `new/net`();
net_5["name"] := "net_5";
net_5["v"] := table():

net_5["v"][  0  ] := [  3,   2]:
net_5["v"][  0.1] := [  2,   6]:
net_5["v"][  0.2] := [ -2,   3]:
net_5["v"][  0.3] := [ -6,   2]:
net_5["v"][  0.4] := [ -3,  -2]:
net_5["v"][  0.5] := [ -2,  -6]:
net_5["v"][  0.6] := [  2,  -3]:
net_5["v"][  0.7] := [  6,  -2]:
net_5["v"][  1  ] := [  0,   0]:
net_5["v"][  2  ] := [  3,   1]:
net_5["v"][  2.1] := [ -3,   1]:
net_5["v"][  3  ] := [  1,  -3]:
net_5["v"][  3.1] := [  1,   3]:
net_5["v"][  4  ] := [ -3,  -1]:
net_5["v"][  4.1] := [  3,  -1]:
net_5["v"][  5  ] := [ -1,   3]:
net_5["v"][  5.1] := [ -1,  -3]:
net_5["v"][  6  ] := [  2,   2]:
net_5["v"][  6.1] := [ -6,   1]:
net_5["v"][  7  ] := [  2,  -2]:
net_5["v"][  7.1] := [  1,   6]:
net_5["v"][  8  ] := [ -2,  -2]:
net_5["v"][  8.1] := [  6,  -1]:
net_5["v"][  9  ] := [ -2,   2]:
net_5["v"][  9.1] := [ -1,  -6]:
net_5["v"][ 10  ] := [  5,  -3]:
net_5["v"][ 10.1] := [ -5,   3]:
net_5["v"][ 11  ] := [  3,   5]:
net_5["v"][ 11.1] := [ -3,  -5]:
net_5["v"][ 12  ] := [  3,   0]:
net_5["v"][ 12.1] := [ -3,   0]:
net_5["v"][ 13  ] := [  0,   3]:
net_5["v"][ 13.1] := [  0,  -3]:

net_5["v_anchor"][  0  ] := "west":
net_5["v_anchor"][  0.1] := "south west":
net_5["v_anchor"][  0.2] := "south":
net_5["v_anchor"][  0.3] := "south east":
net_5["v_anchor"][  0.4] := "east":
net_5["v_anchor"][  0.5] := "north east":
net_5["v_anchor"][  0.6] := "north":
net_5["v_anchor"][  0.7] := "north west":
net_5["v_anchor"][  1  ] := "east":
net_5["v_anchor"][  2  ] := "west":
net_5["v_anchor"][  2.1] := "north east":
net_5["v_anchor"][  3  ] := "north":
net_5["v_anchor"][  3.1] := "south east":
net_5["v_anchor"][  4  ] := "east":
net_5["v_anchor"][  4.1] := "south west":
net_5["v_anchor"][  5  ] := "south":
net_5["v_anchor"][  5.1] := "north west":
net_5["v_anchor"][  6  ] := "south":
net_5["v_anchor"][  6.1] := "north":
net_5["v_anchor"][  7  ] := "west":
net_5["v_anchor"][  7.1] := "east":
net_5["v_anchor"][  8  ] := "north":
net_5["v_anchor"][  8.1] := "south":
net_5["v_anchor"][  9  ] := "east":
net_5["v_anchor"][  9.1] := "west":
net_5["v_anchor"][ 10  ] := "north":
net_5["v_anchor"][ 10.1] := "south":
net_5["v_anchor"][ 11  ] := "west":
net_5["v_anchor"][ 11.1] := "east":
net_5["v_anchor"][ 12  ] := "west":
net_5["v_anchor"][ 12.1] := "east":
net_5["v_anchor"][ 13  ] := "south":
net_5["v_anchor"][ 13.1] := "north":

net_5["squares"] := table([
    1 = [ 6  ,0  , 11  , 3.1], 
    L = [ 7  ,0.6, 10  , 4.1], 
   LL = [ 8  ,0.4, 11.1, 5.1], 
  LLL = [ 9  ,0.2, 10.1, 2.1], 
    M = [ 9  ,1  , 13  , 5  ], 
   LM = [ 6  ,1  , 12  , 2  ], 
  LLM = [ 7  ,1  , 13.1, 3  ], 
 LLLM = [ 8  ,1  , 12.1, 4  ], 
    N = [ 9.1,0.5, 11.1, 5.1], 
   LN = [ 6.1,0.3, 10.1, 2.1], 
  LLN = [ 7.1,0.1, 11  , 3.1], 
 LLLN = [ 8.1,0.7, 10  , 4.1], 
   MN = [ 6  ,1  , 13  , 3.1], 
  LMN = [ 7  ,1  , 12  , 4.1], 
 LLMN = [ 8  ,1  , 13.1, 5.1], 
LLLMN = [ 9  ,1  , 12.1, 2.1]
]):

net_5["set_edges_from_squares"];
net_5["set_outer_edges"];
net_5["set_square_centres"];

