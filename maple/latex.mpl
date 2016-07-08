# This file defines Maple functions to generate various chunks of LaTeX to be
# included in the main document for the project.

######################################################################

# LaTeX names for elements of the group G.

#@ G_latex
G_latex[1]     := "1";
G_latex[L]     := "\\lm";
G_latex[LL]    := "\\lm^2";
G_latex[LLL]   := "\\lm^3";
G_latex[M]     := "\\mu";
G_latex[LM]    := "\\lm\\mu";
G_latex[LLM]   := "\\lm^2\\mu";
G_latex[LLLM]  := "\\lm^3\\mu";
G_latex[N]     := "\\nu";
G_latex[LN]    := "\\lm\\nu";
G_latex[LLN]   := "\\lm^2\\nu";
G_latex[LLLN]  := "\\lm^3\\nu";
G_latex[MN]    := "\\mu\\nu";
G_latex[LMN]   := "\\lm\\mu\\nu";
G_latex[LLMN]  := "\\lm^2\\mu\\nu";
G_latex[LLLMN] := "\\lm^3\\mu\\nu";

######################################################################

# Both tikz and Maple have the ability to place a text label on a
# picture and place it intelligently offset from the point being 
# labelled.  This table helps translate between the tikz syntax and
# the Maple syntax.

#@ tikz_anchor
tikz_anchor['above'] := "south";
tikz_anchor['below'] := "north";
tikz_anchor['right'] := "west";
tikz_anchor['left']  := "east";
tikz_anchor[{'above','left' }] := "south east";
tikz_anchor[{'above','right'}] := "south west";
tikz_anchor[{'below','left' }] := "north east";
tikz_anchor[{'below','right'}] := "north west";

######################################################################

# Generate a tikz plotting command using syntax parallel to that for
# Maple plotting commands.

#@ tikz_plot 
tikz_plot := proc(xy,t_range,num_points,col)
 local t0,t1,tt,pts,s,p;
 t0 := op(1,t_range);
 t1 := op(2,t_range);
 tt := seq(t0 + i/num_points*(t1 - t0),i=0..num_points);
 if type(xy,procedure) then
  pts := evalf(map(xy,[tt]));
 else
  pts := [seq(evalf(subs(t=s,xy)),s in tt)];
 fi;

 s := sprintf(" \\draw[%s] plot[smooth] coordinates{ ",col);
 for p in pts do 
  s := cat(s,sprintf("(%.3f,%.3f) ",op(p)));
 od;
 s := cat(s,"};\n");
 return(s);
end:

######################################################################
# Generate tikz code for a point, using syntax similar to Maple

#@ tikz_point 
tikz_point := proc(xy,sz) 
 sprintf(" \\fill (%.3f,%.3f) circle(%.3f);\n",op(evalf(xy)),sz);
end;

########################################
# Generate tikz code for a line, using syntax similar to Maple

#@ tikz_line 
tikz_line := proc(xy1,xy2,col_)
 local s,d;

 s := " \\draw";
 if nargs > 2 then 
  s := cat(s,"[",col_,"]");
 fi;
 
 s := cat(s,sprintf(" (%.3f,%.3f) -- (%.3f,%.3f);\n",op(evalf(xy1)),op(evalf(xy2))));

 return s;
end:

########################################
# Generate tikz code for a curve, using syntax similar to Maple

#@ tikz_curve 
tikz_curve := proc(pts,col_)
 local s,d,p;

 if nargs > 1 then
  s := cat(" \\draw[smooth,",col_,"] ");
 else 
  s := " \\draw[smooth] ";
 fi;

 d := "";

 for p in pts do 
  s := cat(s,d,sprintf(" (%.3f,%.3f) ",op(evalf(p))));
  d := "--";
 od;

 s := cat(s,";\n");

 return s;
end:

######################################################################
# Generate tikz code for a label, using syntax similar to Maple

#@ tikz_label 
tikz_label := proc(xy,t,a_)
 local a,s;

 a := NULL;

 if nargs > 2 then
  a := a_;
  if not(member(a,["north","south","east","west",
                   "north west","north east","south west","south east"])) then
   a := tikz_anchor[a];
   if not(member(a,["north","south","east","west",
		    "north west","north east","south west","south east"])) then
    a := NULL;
   fi;
  fi;
 fi;

 if a = NULL then
  s := sprintf(" \\draw (%.3f,%.3f) node {$%s$};\n",op(evalf(xy)),t);
 else
  s := sprintf(" \\draw (%.3f,%.3f) node[anchor=%s] {$%s$};\n",op(evalf(xy)),a,t);
 fi;

 return(s);
end;

######################################################################

#@ save_tikz 
save_tikz := proc(s::string,txt::string) 
 local fd;

 fd := fopen(cat(latex_dir,"/tikz_includes/",s,".tex"),WRITE);
 fprintf(fd,"%s",txt);
 fclose(fd);
 return(txt);
end: 

