# This file sets up a framework for object oriented programming in Maple.
# It was borrowed from another project and so has various features that
# are not necessarily relevant here.

# Most of our code is not object oriented, but we have found the OO 
# approach to be useful in a few places.  For example, we represent 
# triangulations and quadrature rules as objects.

######################################################################

`Package/SaveNames`        := [];
`Package/SaveNames/Hidden` := [];
`Package/SaveClasses`      := [];
`Package/Dependencies`     := [];

`Package/Doc/HeaderFormat` := "\n<html>\n <head><title>Package %s</title><script type=\"text/x-mathjax-config\">\nMathJax.Hub.Config({ tex2jax: {inlineMath: [['$','$']]}});</script>\n<script src=\"https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_CHTML\"></script>\n</head>\n <body  style=\"width:700px\">\n  <h1>Package %s</h1>\n  <hr/>\n  %s\n  <br/><hr/>\n":

`Package/Doc/EntryFormat` := "\n <a name='%s'><b><tt>%s</tt></b></a>\n <br/>\n %s\n <br/>\n <hr/>\n":

`Package/Doc/Footer` := "\n </body>\n</html>\n":

PackDocDir := cat(doc_dir,"/");


Package :=
 proc(packname::string,doc::string)
  global `Package/Name`,
         `Package/SaveNames`,`Package/SaveNames/Hidden`,
         `Package/SaveClasses`,
         `Package/Dependencies`,`Package/Documentation`;

  if (assigned(`Package/Name`)) then
   ERROR("Already defining a package");
  fi;

  `Package/Name`             := packname;
  `Package/SaveNames`        := [];
  `Package/SaveNames/Hidden` := [];
  `Package/SaveClasses`      := [];
  `Package/Dependencies`     := [];

  `Package/Documentation` :=
    sprintf(`Package/Doc/HeaderFormat`,
             packname,packname,`Package/ConvertAt`(doc));
   
  NULL;
 end:


`Package/Assign` :=
 proc(namspec::uneval,doc::string,val::anything,
     # optional
     hide_::boolean
     )
  local namspec1,nam,namstring,typespec,params,s,d,i,hide,trans;
  global `Package/SaveNames`,`Package/SaveNames/Hidden`,
         `Package/Documentation`,
         `Package/ReportAssignment`;
  
  hide := `if`(nargs > 3, hide_, false);

  # nam is the name to be assigned (an unevaluated name)
  # namstring is the string version of nam
  # val is the value to be assigned to nam
  # typespec is the declared type for nam
  #   (or the declared return type, if val is a procedure,
  #    or the name 'void' if val is a procedure that does 
  #    not return a value.)

  nam := evaln(namspec); # NB evaln(x::integer) = x
  namstring := convert(nam,string);
  namspec1 := eval(namspec,1);
  if type(namspec1,`::`) then
   typespec := op(2,namspec1);
   if not(type(val,procedure)) and 
      not(type(typespec,type)) then
    ERROR(
     sprintf(
      __("Type specification %a for %a is not a valid type."),
      typespec,nam));
   fi;
   if not(type(val,procedure)) and
      not(type(val,typespec)) then
    ERROR(
     sprintf(
      __("Assigned value %a for %a does not have declared type %a."),
      val,nam,typespec));
   fi;  
  else
   typespec := NULL;
  fi;

  # Remember that nam is to be saved in the .m file for the package
  if (hide = true) then
   `Package/SaveNames/Hidden` := [op(`Package/SaveNames/Hidden`),namstring];
  else
   `Package/SaveNames` := [op(`Package/SaveNames`),namstring];

   # Now construct the HTML documentation for this assignment
   if type(val,procedure) then
    params := [op(1,eval(val))];
    if params = [] then
     s := sprintf("%a()",nam);
    else
     s := sprintf("%a(",nam);
     s := s, op(map((p) -> (`Package/ParamToHTML`(p),","),params));
     s := cat(s[1..-2],")");
    fi;
   else
    s := sprintf("%a",nam);
   fi;
   if typespec <> NULL then
    s := sprintf("%s::#%a#",s,typespec);
   fi;

   d := `Package/ConvertAt`(doc);
   while true do
    i := searchtext("$Value",d);
    if (i > 0) then
     d := cat(substring(d,1..(i-1)),
	      sprintf("%a",val),
	      substring(d,(i+6)..-1));
    else
     break;
    fi;
   od;

   `Package/Documentation` :=
    `Package/Documentation`,
     sprintf(`Package/Doc/EntryFormat`,nam,s,d);
  fi;

  # This actually performs the assignment
  assign(nam = val);

  # Report the assignment, if reporting is switched on
  if (`Package/ReportAssignment` = true) then
   printf("Assigned %s\n",nam);
  fi;

 end:


`Package/Assign/Hidden` :=
 proc(namspec::uneval,val::anything)
  `Package/Assign`(namspec,"",val,true);
 end:


######################################################################

EndPackage :=
 proc()
  local classname,loadname,savefile,savedir,savestatement,docfile,
        nam,trans,s;
  global `Package/Name`,`Package/SaveNames`,`Package/SaveNames/Hidden`,
         `Package/SaveClasses`,
         `Package/Documentation`,`Package/Doc`,
         `Package/List`,`Package/Classes`,
         `Package/Entries`,`Package/AllEntries`,
         `Package/Dependencies`,`Package/Depend`,
         `Package/Index`,`Package/FindTranslationStrings`,
         `Package/TranslationStrings`;

  if not(assigned(`Package/Name`)) then
   ERROR("Not defining a package.");
  fi;

  if assigned(`Package/List`) and
     type(`Package/List`,set) then
   `Package/List` := `Package/List` union {`Package/Name`};
  else
   `Package/List` := {`Package/Name`};
  fi;

  if not(assigned(`Package/Classes`)) then 
   `Package/Classes` := table([]);
  fi;
  `Package/Classes`[`Package/Name`] := `Package/SaveClasses`;

  if not(assigned(`Package/Entries`)) then 
   `Package/Entries` := table([]);
  fi;
  `Package/Entries`[`Package/Name`] := `Package/SaveNames`;

  for classname in `Package/SaveClasses` do
   `Package/SaveNames` :=
     [op(`Package/SaveNames`),cat("class/",classname)];

   `Package/SaveNames/Hidden` :=
      [op(`Package/SaveNames/Hidden`),
       cat("index/",classname),
       cat("type/",classname),
       cat("new/",classname)];
  od;

  if not(assigned(`Package/AllEntries`)) then 
   `Package/AllEntries` := table([]);
  fi;
  `Package/AllEntries`[`Package/Name`] := `Package/SaveNames`;

  `Package/Depend`[`Package/Name`] := `Package/Dependencies`;

  if not(assigned(`Package/Index`)) then 
   `Package/Index` := table([]);
  fi;
  for nam in `Package/SaveNames` do
   `Package/Index`[nam] := `Package/Name`;
  od;
  for nam in `Package/SaveClasses` do
   `Package/Index`[nam] := `Package/Name`;
  od;

  loadname := cat(`Package/Name`,"/IsLoaded");

  `Package/SaveNames` :=
    [op(`Package/SaveNames`),"Package/Dependencies",loadname];
                
  assign(convert(loadname,name) = true);

#  savefile := cat(lib_dir,"/",`Package/Name`,".m");
#  savedir := util_dirname(savefile);
#  os_makedirectory(savedir);

#  savestatement :=
#   cat(
#    "save(",
#    op(map((s) -> cat("`",s,"`,"),`Package/SaveNames`)),
#    op(map((s) -> cat("`",s,"`,"),`Package/SaveNames/Hidden`)),
#    "\"",savefile,"\"",
#    "):"
#   );

#  parse(savestatement,'statement');

  `Package/Documentation` :=
    cat(`Package/Documentation`,`Package/Doc/Footer`);

  if not(assigned(`Package/Doc`)) then
   `Package/Doc` := table([]);
  fi;
  `Package/Doc`[`Package/Name`] :=
   `Package/Documentation`;


  if (`Package/FindTranslationStrings` = true) then
   if not(type([`Package/TranslationStrings`],[list])) then
    `Package/TranslationStrings` := []:
   fi;
   for nam in 
     [op(`Package/SaveNames`),op(`Package/SaveNames/Hidden`)] do
    trans := 
     TranslationStrings(eval(convert(nam,name)));
    trans := map((x,p,n) -> [x,p,n],trans,`Package/Name`,nam);
    `Package/TranslationStrings` := 
     [op(`Package/TranslationStrings`),op(trans)];
   od;
  fi;
  NULL;

  unassign('`Package/Name`');
 end:


######################################################################

`Package/MakeAllDocumentation` := 
 proc()
  local c,d,i,j,l,m,n,p,f,entryfile,html,indexfile,navbar,
        ents,classes,nam,nam0,nam1,docdir;
  global Config,`Package/List`,PackDocDir;

  if type([PackDocDir],[string]) then
   docdir := PackDocDir;
  else
   docdir := Config['DocDir'];
  fi;

  html := "";

  for p in sort([op(`Package/List`)]) do
   `Package/MakeDocumentation`(p,docdir);
   html :=
    html,
    sprintf("<td width='50%%'><a href='%s.html'>%s</a></td>\n",p,p);
  od;

  html := [html][2..-1];
  n := nops(html);
  if type(n,odd) then 
   n := n + 1;
  html := [op(html),"<td>&nbsp;</td>"];
  fi;
  m := n/2;

  html := seq(cat("<tr>\n",html[i],html[i+m],"</tr>\n"),i=1..m);

  html := cat("\n   <html>\n    <head>\n     <title>Index of packages</title>\n    </head>\n    <body bgcolor='white'>\n     <h1>Index of packages</h1>\n     <br/>\n     <table width='100%'>\n     ",      html,"\n     </table>\n     <a href='entries.html'>Index of all package entries</a>\n    </body>\n   </html>\n    ");

  indexfile := cat(docdir,"/packages.html");
  traperror(fclose(indexfile));
  fprintf(indexfile,"%s\n",html);
  fclose(indexfile);

  ents := map((x) -> op(x),{indices(`Package/Index`)});
  ents := map(util_stripbackquotes,ents);
  classes := select(util_startswith,ents,"class/");
  # strip off class/ prefix.
  classes := map((x) -> substring(x,7..-1),classes);
  ents := ents minus map(x -> cat("class/",x),classes);
  ents := ents minus map(x -> cat("type/",x),classes);
  ents := ents minus map(x -> cat("index/",x),classes);
  ents := ents minus map(x -> cat("new/",x),classes);

  l := NULL;
  for nam in  ents do
   nam0,nam1 := op(util_splitfilename(nam));
   l := l,[nam,nam0,nam1,StringTools[LowerCase](nam1)];
  od;
  l := sort([l],(a,b) -> evalb(a[4] < b[4]));

  html := "\n   <html>\n    <head>\n     <title>Index of all package entries</title>\n    </head>\n    <body bgcolor='white'>\n     <h1>Index of all package entries</h1>\n     <br/>\n  ";

  navbar := "";
  for i from 1 to 26 do
   c := convert([64+i],bytes);
   navbar := 
    navbar,
     sprintf("<a href='#%s'>%s</a>\n",c,c);
  od;
  html := cat(html,navbar,"<br/>");

  j := 1;
  for i from 1 to 26 do
   c := convert([64+i],bytes);
   d := convert([96+i],bytes);
   html :=
    html,
    sprintf("<hr/><a name='%s'><h2>%s</h2></a><br/>\n",c,c);
   while(j <= nops(l) and substring(l[j][4],1..1) <= d) do
    nam  := l[j][1];
    nam0 := l[j][2];
    nam1 := l[j][3];
    p := `Package/Index`[nam];
    html :=
     html,
     sprintf("%s/<a href='%s.html#%s'>%s</a><br/>\n",
             nam0,p,nam,nam1);
    j := j+1;
   od;
  od;

  html := cat(html,"</body></html>\n");
  entryfile := cat(docdir,"/entries.html");
  traperror(fclose(entryfile));
  fprintf(entryfile,"%s\n",html);
  fclose(entryfile);

  
 end:


######################################################################

`Package/MakeDocumentation` :=
 proc(packname::string,docdir::string)
  local docfile,docsubdir,depth;

  docfile := cat(docdir,"/",packname,".html");
  docsubdir  := util_dirname(docfile);
  os_makedirectory(docsubdir);
  traperror(fclose(docfile));

  # depth is the number of "/" characters (with ASCII
  # code 47) in the package name.
  depth :=
   nops(select(c -> (c = 47),convert(packname,bytes)));

  fprintf(docfile,"%s\n\n",
   `Package/ConvertHash`(`Package/Doc`[packname],depth));

  fclose(docfile);
 end:


######################################################################

`Package/ConvertAt` := 
 proc(s::string)
  local t,i,j,k,l,n,m,p;

  t := "";
  i := 1;
  while true do
   j := `if`(i <= length(s),searchtext("@",s,i..-1),0);
   if j > 0 then 
    if i + j <= length(s) and
     substring(s,(i+j-1)..(i+j)) = "@@" then
     t := t,substring(s,i..(i+j-1));
     i := i+j+1;
    else
     k := searchtext("@",s,i+j..-1);
     if k = 0 then
      ERROR(
       sprintf("Unmatched @ in documentation string:\n%s\n",s));
     else
      t := t,
           substring(s,i..(i+j-2)),
           "<font color='green'>",
           `Package/HTMLEscape`(substring(s,(i+j)..(i+j+k-2))),
           "</font>";
      i := i+j+k;
     fi;
    fi;
   else
    t := cat(t,substring(s,i..-1));
    break;
   fi;
  od;
  RETURN(t);
 end:


######################################################################

`Package/HTMLEscape` :=
 proc(s::string)
  local b;
  b := convert(s,bytes);

  b := subs( 60 = (38, 108, 116, 59),            # &lt;
      62 = (38, 103, 116, 59),            # &gt;
      34 = (38, 113, 117, 111, 116, 59),  # &quot;
      b);

  convert(b,bytes);
 end:


######################################################################

# This function converts #...# sequences to links as explained at the
# top of this file.  
#
# The argument 'depth_' is used to construct relative URL's.  Suppose
# that the return value of this function is part of the documentation
# for a package 'aim/foo/Bar', and thus will be included in the file
# aim/foo/Bar.html.  This is two levels down from the top documentation
# directory, so URL's for other documentation files should be prefixed
# with the string "../../".  This is achieved by putting depth_ := 2.
#
# The function uses the global variable `Package/Index`, which is a
# table.  If a name n is defined in a package named p, and ns is the
# string version of n, then `Package/Index`[ns] is set to p.  Similarly,
# if a class named c is declared in a package named p, and cs is the
# string version of c, then `Package/Index`[cs] is set to p.

`Package/ConvertHash` :=
 proc(t::string,depth_::nonnegint)
  local u,i,j,k,l,n,m,tm,p,prefix;
  global `Package/Index`;

  # set the prefix for relative URL's
  if nargs = 1 then
   prefix := "";
  else
   prefix := cat("../" $ depth_);
  fi;

  # u is s sequence of strings that will be concatenated to give the
  # return value.
  u := "";

  # i is the position in t of the first character that has not yet
  # been processed.
  i := 1;

  while true do
   j := `if`(i <= length(t),searchtext("#",t,i..-1),0);
   if j > 0 then 
    # there is a '#' at position i+j-1 in t
    if i + j <= length(t) and
     substring(t,(i+j-1)..(i+j)) = "##" then
     # Convert '##' to a literal '#' character
     u := u,substring(t,i..(i+j-1));
     i := i+j+1;
    else
     k := searchtext("#",t,i+j..-1); # find the matching '#'
     if k = 0 then
      ERROR(
       sprintf("Unmatched # in documentation string:\n%s\n",t));
     else
      # the matching '#' is at position i+j+k-1 in t.

      n := substring(t,(i+j)..(i+j+k-2));
      # n is the name enclosed in #...#, possibly including backquotes

      m := n;
      if m <> "" and substring(m,1..1) = "`" then
       m := substring(m,2..-1);
      fi;
      if m <> "" and substring(m,-1..-1) = "`" then
       m := substring(m,1..-2);
      fi;
      # m is the name enclosed in #...#, without backquotes

      tm := cat("type/",m);

      if assigned(`Package/Index`[m]) then
       p := cat(prefix,`Package/Index`[m]);
       l := sprintf("<a href='%s.html#%s'>%s</a>",p,m,n);
       u := u,substring(t,i..(i+j-2)),l;
      elif assigned(`Package/Index`[tm]) then
       p := cat(prefix,`Package/Index`[tm]);
       l := sprintf("<a href='%s.html#%s'>%s</a>",p,tm,n);
       u := u,substring(t,i..(i+j-2)),l;
      else
       u := u,substring(t,i..(i+j-2)),n;
      fi;
      i := i+j+k;
     fi;
    fi;
   else
    u := cat(u,substring(t,i..-1));
    break;
   fi;
  od;
  RETURN(u);
 end:


######################################################################

`Package/TypeToHTML` := 
 proc(t)
###TODO: evaln(type)
  local s, ts, p;

  if type(t,symbol) then
   RETURN(sprintf("#%a#",t));
  elif type(t,{complex(numeric),string}) then
   RETURN(sprintf("%A",t));
  else
   RETURN(sprintf("%A",map(`Package/TypeToHTML`,t)));
  fi;
 end:


######################################################################

`Package/ParamToHTML` :=
 proc(p::{symbol,`::`,assignment})
  if type(p,symbol) then
   RETURN(sprintf("<font color='olive'>%a</font>",p));
  elif type(p,assignment) then
   RETURN(sprintf("%s := %a",`Package/ParamToHTML`(op(1,p)),op(2(p))));
  else
   RETURN(
    sprintf("<font color='olive'>%a</font>::%s",
            op(1,p),`Package/TypeToHTML`(op(2,p))));
  fi;
 end:


######################################################################

convert_at := 
 proc(s::string)
  local t,i,j,k,l,n,m,p;

  t := "";
  i := 1;
  while true do
   j := `if`(i <= length(s),searchtext("@",s,i..-1),0);
   if j > 0 then 
    if i + j <= length(s) and
     substring(s,(i+j-1)..(i+j)) = "@@" then
     t := t,substring(s,i..(i+j-1));
     i := i+j+1;
    else
     k := searchtext("@",s,i+j..-1);
     if k = 0 then
      ERROR(
       sprintf("Unmatched @ in documentation string:\n%s\n",s));
     else
      t := t,
           substring(s,i..(i+j-2)),
           "<font color=\"green\">",
           `Package/HTMLEscape`(substring(s,(i+j)..(i+j+k-2))),
           "</font>";
      i := i+j+k;
     fi;
    fi;
   else
    t := cat(t,substring(s,i..-1));
    break;
   fi;
  od;
  RETURN(t);
 end:


######################################################################

`Class/Constructor` := 
 proc(class::table)
  local t,f;

  t := table(convert(class["Name"],name));

  for f in class["Fields"] do
   t[f] := eval(class["FieldDefault"][f]);
  od;

  class["Constructor"](t,args[2..-1]);

  eval(t);
 end:

`Class/SetIndexFunction` := 
 proc(x::table,f::string)
  subsop(1 = convert(f,name),op(x));
 end:

`Class/IndexFunction` := 
 proc(class,indices,tbl)
  local index,rest,this,val,reqtype,method,N,C;

  if (nargs = 3) then
   if nops(indices) = 0 then
    ERROR("Empty field name");
   fi;
   index := indices[1];

   if nops(indices) = 1 then
    if member(index,class["Methods"]) then
     # A method with no arguments should override a field with the 
     # same name in a parent class
     this := `Class/SetIndexFunction`(tbl,class["Name"]);
     RETURN(eval(convert(cat(class["Name"],"!",index),name))(this));
    elif member(index,class["Fields"]) then
     RETURN(eval(tbl)[index]);
    elif member(index,class["StaticFields"]) then
     RETURN(class["StaticFieldValue"][index]);
    elif member(index,class["IndirectStaticFields"]) then
     N := class["StaticFieldIndirectionTable"][index];
     C := eval(cat(`class/`,convert(N,name)));
     RETURN(C["StaticFieldValue"][index]);
    elif member(index,class["IndirectFields"]) then
     RETURN(eval(tbl)[class["FieldIndirectionTable"][index]][index]);
    fi;
   fi;

   rest := op(indices[2..-1]);

   if member(index,class["Methods"]) then
    this := `Class/SetIndexFunction`(tbl,class["Name"]);
    RETURN(eval(convert(cat(class["Name"],"!",index),name))(this,rest));
   elif member(index,class["IndirectMethods"]) then
    RETURN(eval(tbl)[class["MethodIndirectionTable"][index]][op(indices)]);
   fi;

   ERROR(sprintf(
    "Invalid field or method in class %s: %a",
    class["Name"],
    indices
   ));

  else
   if nops(indices) = 0 then
    ERROR("Empty field name in assignment");
   elif nops(indices) > 1 then
    ERROR(sprintf("Too many indices in assignment: %a",indices));
   fi;
   index := indices[1];

   val := op(args[4]);

   if member(index,class["Fields"]) or
      member(index,class["IndirectFields"]) then 
    reqtype := class["FieldType"][index];
   elif member(index,class["StaticFields"]) then
    reqtype := class["StaticFieldType"][index];
   elif member(index,class["IndirectStaticFields"]) then
    N := class["StaticFieldIndirectionTable"][index];
    C := eval(cat(`class/`,convert(N,name)));
    reqtype := C["StaticFieldType"][index];
   else
    ERROR(sprintf(
     "Assignment to invalid field in class %s: %a",
     class["Name"],
     indices
    ));
   fi;

   if nops([val]) = 1 and not(type(val,reqtype)) then
    ERROR(sprintf(
     "Type error in assignment: field %a in class %a should have type %a",
     index,class["Name"],reqtype));
   fi;
   if nops([val]) > 1 and reqtype <> anything then
    ERROR(sprintf(
     "Type error in assignment: field %a in class %a should have type %a",
     index,class["Name"],reqtype));
   fi;

   if member(index,class["Fields"]) then 
    tbl[index] := eval(val,1);  
   elif member(index,class["StaticFields"]) then
    class["StaticFieldValue"][index] := eval(val,1);
   elif member(index,class["IndirectStaticFields"]) then
    N := class["StaticFieldIndirectionTable"][index];
    C := eval(cat(`class/`,convert(N,name)));
    C["StaticFieldValue"][index] := eval(val,1);
   elif member(index,class["IndirectFields"]) then
    tbl[class["FieldIndirectionTable"][index]][index] := eval(val,1);
   fi;
  fi;
 end:

`Class/TypeFunction` := 
 proc(class,x)
  local xclassname,xclass;
  if not type(eval(x),table) then RETURN(false); fi;
  xclassname := op(1,eval(x));
  if xclassname = NULL then RETURN(false); fi;
  if xclassname = class["Name"] then RETURN(true); fi;

  xclass := convert(cat("class/",xclassname),name);

  RETURN(`Class/IsSuperClass`(class,xclass));
 end:

`Class/IsSuperClass` := 
 proc(class0,class1)
  local p;

  if class0["Name"] = class1["Name"] then RETURN(true); fi;

  for p in class1["Parents"] do
   if `Class/IsSuperClass`(class0,convert(cat("class/",p),name)) then
    RETURN(true);
   fi;
  od;

  RETURN(false);
 end:

`Class/IsClass` := 
 proc(classname)
  if not(type(classname,string)) then
   RETURN(false);
  fi;

  if eval(convert(cat("class/",classname),name)) =
          convert(cat("class/",classname),name) then
   RETURN(false);
  fi;

  RETURN(true);
 end:

`Class/IsObject` := 
 proc(x)
  local xclassname;

  if not type(x,table) then RETURN(false); fi;

  xclassname := op(1,eval(x));
  if xclassname = NULL then RETURN(false); fi;

  RETURN(`Class/IsClass`(xclassname));
 end:

`Class/ClassNameOf` := 
 proc(x)
  if not type(x,table) then RETURN(NULL); fi;
  RETURN(op(1,eval(x)));
 end:

`Class/ClassOf` :=
 proc(x)
  local xclassname;

  xclassname := `Class/ClassNameOf`(x);
  if xclassname = NULL or
     not(`Class/IsClass`(xclassname)) then
   RETURN(NULL);
  fi;

  RETURN(eval(cat(`class/`,xclassname)));
 end:

`Class/Update` := 
 proc(x::anything)
  local classname,class,present,missing,t,f;

  if `Class/IsObject`(x) then
   classname := op(1,eval(x));
   class := eval(cat(`class/`,classname));
   t := subsop(1 = NULL,op(x));
   present := map(op,select((y) -> (nops(y) = 1),{indices(t)}));
   missing := {op(class["Fields"])} minus present;

   for f in missing do
    t[f] := eval(class["FieldDefault"][f]);
   od;
  fi;
  NULL;
 end:

`Class/DefaultValue` := 
 table([
 numeric = 0,
 integer = 0,
 float   = 0,
 string  = "",
 list    = [],
 set     = {}
 ]):

#@ `Class/Declare` 
`Class/Declare` :=  
 proc(classname::string) 
  global CLASS,CLASSNAME,
  `Package/Name`,`Package/SaveClasses`,`Package/SaveNames/Hidden`,
         `Package/Documentation`;
  local
   c,indexfunction,typefunction,
   constructor,consproc,consdoc,x,
   fieldname,fieldtype,fielddefault,realtype,
   typespecified,defaultspecified,
   methodname,fullmethodname,methodtype,
   methodbody,methodsig,methoddoc,params,
   parent,parenttypes,field,method,
   incclass,indfield,indfields,indmethod,indmethods,
   classdoc;

  if (`Package/ReportAssignment` = true) then
   printf("Declaring Class: %s\n",classname);
  fi;

  parenttypes := NULL;

  c := table(["Name" = classname,
              "Package" = "",
	      "Constructor" = proc(this) end,
	      "Fields" = {},
	      "FieldType" = table(),
	      "FieldDefault" = table(),
	      "StaticFields" = {},
	      "StaticFieldValue" = table(),
	      "IndirectFields" = {},
	      "FieldIndirectionTable" = table(),
	      "IndirectStaticFields" = {},
	      "StaticFieldIndirectionTable" = table(),
	      "IndirectMethods" = {},
	      "MethodIndirectionTable" = table(),
	      "Methods" = {},
	      "MethodType" = table(),
	      "MethodSignature" = table(),
	      "MethodTable" = table(),
	      "Parents" = {}]);

  if assigned(`Package/Name`) then
   c["Package"] := `Package/Name`;
  fi;

  classdoc :=
   sprintf("<a name=\"%A\"><h2>Class: %s</h2></a><br/>\n",classname,classname);

  for x in args[2..-1] do
   if type(x,string) then 
    classdoc := 
     classdoc,
      "<p>\n",convert_at(x),"\n</p>\n";
   elif not(type(x,list)) then
    ERROR(
     sprintf("Non-list entry in declaration of class %a",classname));
   fi;
   if nops(x) = 0 then
    ERROR(
     sprintf("Empty list in declaration of class %a",classname));
   fi;

   ##########################
   if type(x,list) then
    if (x[1] = "Extends") then
     if nops(x) = 1 then
      ERROR(
       sprintf(
 "Extends clause with no parent class in declaration of class %a",
 classname));
     fi;

     if (`Package/ReportAssignment` = true) then
      printf("    Extends %a\n",x[2]);
     fi;

     classdoc := 
      classdoc,
       sprintf("<b>Extends: </b>#%s#<br/>\n",x[2]);

     c["Parents"] := c["Parents"] union {x[2]};
     parent := eval(convert(cat("class/",x[2]),name));

     c["Fields"]  := c["Fields"] union parent["Fields"];
     for field in parent["Fields"] do
      c["FieldDefault"][field] := eval(parent["FieldDefault"][field]);
     od;

     for field in indices(parent["FieldType"]) do
      # The RHS below used to be wrapped in eval().  I am not sure if that would be better.
      # It seems to cause trouble for inheritance when the type is 'table'.
      c["FieldType"][op(field)] := parent["FieldType"][op(field)];
     od;

     c["IndirectFields"] := c["IndirectFields"] union
       parent["IndirectFields"];

     for indfield in parent["IndirectFields"] do
      c["FieldIndirectionTable"][indfield] :=
       eval(parent["FieldIndirectionTable"][indfield]);
     od;

     c["IndirectStaticFields"] := c["IndirectStaticFields"] union
       parent["StaticFields"] union 
       parent["IndirectStaticFields"];

     for indfield in parent["IndirectStaticFields"] do
      c["StaticFieldIndirectionTable"][indfield] :=
       eval(parent["StaticFieldIndirectionTable"][indfield]);
     od;

     for indfield in parent["StaticFields"] do
      c["StaticFieldIndirectionTable"][indfield] := x[2];
     od;

     c["Methods"] := c["Methods"] union parent["Methods"];
     for methodname in parent["Methods"] do
      fullmethodname :=
       sprintf("%A!%A",classname,methodname);
      `Package/SaveNames/Hidden` := 
        [op(`Package/SaveNames/Hidden`),fullmethodname];
      assign(convert(fullmethodname,name) =
              eval(convert(cat(parent["Name"],"!",methodname),name)));
      c["MethodSignature"][methodname] := parent["MethodSignature"][methodname];
      c["MethodType"][methodname]      := parent["MethodType"][methodname];
     od;

     c["IndirectMethods"] := c["IndirectMethods"] union
       parent["IndirectMethods"];

     for indmethod in parent["IndirectMethods"] do
      c["MethodIndirectionTable"][indmethod] :=
       eval(parent["MethodIndirectionTable"][indmethod]);
     od;

    ##########################
    elif (x[1] = "Documentation") then
     classdoc := 
      classdoc,
       "<p>\n",convert_at(x[2]),"\n</p>\n";

    ##########################
    elif (x[1] = "Constructor") then
     if nops(x) = 1 then
      ERROR(
       sprintf(
 "Empty constructor clause in declaration of class %a",
 classname));
     fi;

     if (`Package/ReportAssignment` = true) then
      printf("    Constructor\n");
     fi;

     if not type(x[2],string) then
      ERROR(
       sprintf(
 "In declaration of class %s: constructor documentation is not a string.",
 classname));
     fi;
     consdoc := convert_at(x[2]);

     consproc := eval(x[3]);
     if not type(consproc,procedure) then
      ERROR(
       sprintf(
 "In declaration of class %s: constructor is not a procedure.",
 classname));
     fi;

     params := [op(1,eval(consproc))];
     if params <> [] then params := params[2..-1]; fi;

     consdoc := sprintf("<b>Constructor:</b> `new/%A`(",classname),
  op(map((p) -> (`Package/ParamToHTML`(p),","),params)),
  ")\n<p>\n",consdoc,"\n</p>\n";

     c["Constructor"] := eval(consproc);

     classdoc := classdoc,consdoc;

    ##########################
    elif (x[1] = "Field" or
	  x[1] = "IncludedField" or
	  x[1] = "StaticField") then
     if nops(x) = 1 then
      ERROR(
       sprintf(
 "Empty field specification in declaration of class %a",
 classname));
     fi;

     typespecified := false;
     fieldtype := anything;
     defaultspecified := false;
     fielddefault := NULL;
     if type(x[2],equation) then
      fieldname := op(1,x[2]);
      fielddefault := op(2,x[2]);
      defaultspecified := true;
     else
      fieldname := x[2];
     fi;

     if type(fieldname,`::`) then
      fieldtype := op(2,fieldname);
      typespecified := true;
      fieldname := op(1,fieldname);
      if fielddefault = NULL then
       if assigned(`Class/DefaultValue`[fieldtype]) then
        fielddefault := `Class/DefaultValue`[fieldtype];
       elif type(fieldtype,name) then
        realtype := eval(cat(`type/`,fieldtype));
        if assigned(`Class/DefaultValue`[realtype]) then
         fielddefault := `Class/DefaultValue`[realtype];
        fi;
       fi;
      fi;
     fi;

     if not(type(fieldname,string)) then
      ERROR(
       sprintf("In declaration of class %a: field name %a is not a string.",
        classname,fieldname));
     fi;

     if not(type(fieldtype,type)) then
      ERROR(
       sprintf("In declaration of class %a: type specification %a for field  %a is invalid.",
        classname,fieldtype,fieldname));
     fi;

     if nops([fielddefault]) > 1 and fieldtype <> anything then
      ERROR(
       sprintf("In declaration of class %a: default value for field %a is an expression sequence",
        classname,fieldname))
     fi;

     if fielddefault <> NULL and not(type(fielddefault,fieldtype)) then
      ERROR(
       sprintf("In declaration of class %a: default value %a for field %a does not have type %a",
        classname,fielddefault,fieldname,fieldtype))
     fi;

     if (`Package/ReportAssignment` = true) then
      printf("    %a: %a\n",x[1],fieldname);
     fi;

     if x[1] = "StaticField" then
      c["StaticFields"] := {op(c["StaticFields"]),fieldname};
      c["StaticFieldType"][fieldname]  := fieldtype;
      c["StaticFieldValue"][fieldname] := eval(fielddefault);
     else
      c["Fields"] := {op(c["Fields"]),fieldname};
      c["FieldType"][fieldname]    := fieldtype;
      c["FieldDefault"][fieldname] := eval(fielddefault);
     fi;

     classdoc := 
      classdoc,
      sprintf("<b>%s: </b>",x[1]),
      "<font color=\"red\">",fieldname,"</font>";

     if typespecified then
      classdoc := classdoc,sprintf("::#%a#",fieldtype);
     fi;

     if defaultspecified then
      if fielddefault = NULL then
       classdoc := classdoc," = #NULL#";
      else
       classdoc := classdoc,sprintf(" = #%a#",fielddefault);
      fi;
     fi;

     classdoc := classdoc,"<br/>\n";

     if nops(x) > 2 then
      if type(x[3],string) then
       classdoc := classdoc, "<p>\n", convert_at(x[3]), "\n</p>\n";
      else
       ERROR(
 sprintf(
  "In declaration of class %s: documentation for field %a is not a string: %a",
  classname,fieldname,x[3]));
      fi;
     fi;

     if x[1] = "IncludedField" then
      if `Class/IsClass`(fieldtype) then
       incclass := eval(cat(`class/`,fieldtype));
       indfields := 
	incclass["Fields"] union
	incclass["StaticFields"] union
	incclass["IndirectFields"];
       c["IndirectFields"] := c["IndirectFields"] union indfields;

       for indfield in indfields do
	c["FieldIndirectionTable"][indfield] := fieldname;
	c["FieldType"][indfield] := incclass["FieldType"][indfield];
       od;

       indmethods := incclass["Methods"];
       c["IndirectMethods"] := c["IndirectMethods"] union indmethods;
       for indmethod in indmethods do
        c["MethodIndirectionTable"][indmethod] := fieldname;
       od;
      fi;
     fi;

    ##########################
     elif x[1] = "Method" then
      if nops(x) = 1 then
       ERROR(
 sprintf(
  "Empty method specification in declaration of class %a",
  classname));
      fi;

      if type(x[2],`::`) then
       methodname := op(1,x[2]);
       methodtype := op(2,x[2]);
      else
       methodname := x[2];
       methodtype := anything;
      fi;

      if not(type(methodname,string)) then
       ERROR(
        sprintf("In declaration of class %a: method name %a is not a string.",
                classname,methodname));
      fi;

      c["Methods"] := {op(c["Methods"]),methodname};
      c["MethodType"][methodname] := methodtype;

      if nops(x) > 3 then
       methodbody := x[4];
       if not(type(methodbody,procedure)) then
 ERROR(
  sprintf(
   "In declaration of class %a: body of method %a is not a procedure",
   classname,methodname));
       fi;
       fullmethodname :=
        sprintf("%A!%A",classname,methodname);
       `Package/SaveNames/Hidden` := 
         [op(`Package/SaveNames/Hidden`),fullmethodname];
       assign(convert(fullmethodname,name) = eval(methodbody));
      else
       ERROR(
 sprintf(
  "In declaration of class %a: method %a has no body.",
  classname,methodname));
      fi;

      if (`Package/ReportAssignment` = true) then
       printf("    Method: %s\n",methodname);
      fi;

      params := [op(1,eval(methodbody))];
      if params <> [] then params := params[2..-1]; fi;
      c["MethodSignature"][methodname] := params;

      if params = [] then
       methoddoc := sprintf("<font color=\"maroon\">%s</font>()",methodname);
      else
       methoddoc := sprintf("<font color=\"maroon\">%s</font>(",methodname);
       methoddoc := 
         methoddoc, op(map((p) -> (`Package/ParamToHTML`(p),","),params));
       methoddoc := cat(methoddoc[1..-2],")");
      fi;

      if type(x[2],`::`) then
       methoddoc := cat(methoddoc,"::",`Package/TypeToHTML`(methodtype));
      fi;

      if type(x[3],string) then
       if x[3] <> "" then
        methoddoc := cat(methoddoc,"\n<p>\n",convert_at(x[3]),"\n</p>\n");
       fi;
      else
       ERROR(
 sprintf(
  "In declaration of class %s: documentation for method %a is not a string: %a",
  classname,methodname,x[3]));
      fi; 
      classdoc := classdoc, "<b>Method:</b> ",methoddoc,"<br/>\n";

    ##########################
    else
     ERROR(
      sprintf(
       "Invalid entry in declaration of class %a: %a",
       classname,x));
    fi;
   fi;
  od;

  if not(assigned(`Package/Documentation`)) then
   `Package/Documentation` := "";
  fi;

  classdoc := cat(classdoc);
  `Package/Documentation` :=
   `Package/Documentation`,
   classdoc,
   "\n<hr/>\n";

  assign(cat(`class/`,classname) = eval(c));

  indexfunction :=
   proc() `Class/IndexFunction`(CLASS,args) end; 
  indexfunction :=
   subs(CLASS = convert(cat("class/",classname),name),eval(indexfunction));
  assign(convert(cat("index/",classname),name) = eval(indexfunction));

  # Convert from expression sequence to set
  parenttypes := {parenttypes};

  typefunction :=
   proc(x) `Class/TypeFunction`(CLASS,x) end;
  typefunction :=
   subs(CLASS = convert(cat("class/",classname),name),eval(typefunction));
  assign(convert(cat("type/",classname),name) = eval(typefunction));

  constructor := 
   proc() `Class/Constructor`(CLASS,args) end;
  constructor :=
   subs(CLASS = convert(cat("class/",classname),name),eval(constructor));
  assign(convert(cat("new/",classname),name) = eval(constructor));
  c["FullConstructor"] := eval(constructor);

  if assigned(`Package/SaveClasses`) and
     type(`Package/SaveClasses`,list) then
   `Package/SaveClasses` :=
    [op(`Package/SaveClasses`),convert(classname,string)]
  fi;
 end:

#@ `Class/ToString` 
`Class/ToString` := 
 proc(x::anything,
      # optional
      maxdepth_::{integer,identical(infinity)},
      stringlen_::{integer,identical(infinity)})
  local maxdepth,stringlen;

  if nargs = 0 then
   RETURN("NULL\n");
  fi;

  maxdepth  := `if`(nargs > 1,args[2],infinity);
  stringlen := `if`(nargs > 2,args[3],maxdepth * 20);

  `Class/ToString0`(x,0,maxdepth,stringlen);
 end:

`Class/ToString0` := 
 proc(x,level,maxdepth,stringlen)
  `Class/ToString1`([x],level,maxdepth,stringlen);
 end:

`Class/ToString1` := 
 proc(y::list,
      level::integer,
      maxdepth::{integer,identical(infinity)},
      stringlen::{integer,identical(infinity)})
  local prefix,s,f,x,xclass;

  prefix   := cat(" "$level);

  if level > maxdepth then 
   RETURN(cat(prefix,"...\n"));
  fi;

  if nops(y) = 0 then
   RETURN(cat(prefix,"NULL\n"));
  elif nops(y) > 1 then
   RETURN(
    cat(prefix,"EXPRSEQ\n",
 op(map(`Class/ToString0`,x,level + 1,maxdepth,stringlen))))
  fi;

  x := y[1];

  if type(x,list) then
   if level = maxdepth then
    RETURN(cat(prefix,"[...]\n"));
   else
    RETURN(
     cat(prefix,"[\n",
  op(map(`Class/ToString0`,x,level + 1,maxdepth,stringlen)),
  prefix,"]\n"));
   fi;
  elif type(x,set) then
   if level = maxdepth then
    RETURN(cat(prefix,"{...}\n"));
   else
    RETURN(
     cat(prefix,"{\n",
  op(map(`Class/ToString0`,x,level + 1,maxdepth,stringlen)),
  prefix,"}\n"));
   fi;
  elif `Class/IsObject`(x) then
   s := sprintf("%sINSTANCE(%A)\n",prefix,`Class/ClassNameOf`(x));
   if level = maxdepth then
    RETURN(s);
   else
    xclass := eval(`Class/ClassOf`(x));
    for f in xclass["Fields"] do
     s := sprintf("%s%s %A =\n%s",s,prefix,f,
    `Class/ToString1`([eval(x[f])],
                                    level + 2,maxdepth,stringlen));
    od;
    RETURN(s);
   fi;
  elif type(x,table) then
   s := cat(prefix,"TABLE\n");
   if level = maxdepth then
    RETURN(s);
   else
    for f in indices(x) do
     s := sprintf("%s%s %A =\n%s",s,prefix,f,
    `Class/ToString1`([eval(x[op(f)])],
                                    level + 2,maxdepth,stringlen));
    od;
    RETURN(s);
   fi;
  else
   s := sprintf("%a",eval(x));
   if length(s) > stringlen then
    s := cat(substring(s,1..stringlen),"...");
   fi;
   RETURN(cat(prefix,s,"\n"));
  fi;
 end:

#@ `Class/Print`::'void' 
`Class/Print`::'void' := 
 proc()
  printf("%s",`Class/ToString`(args));
  NULL;
 end:

#@ `Class/ShowInfo` 
`Class/ShowInfo` := 
 proc(classname::name)
  local c,f,m,ms,mt,s,t;

  c := eval(cat(`class/`,classname));
  printf("Name:       %a\n",classname);
  printf("Package:    %s\n",c["Package"]);

  if c["Fields"] <> {} then
   printf("Fields:\n");
   for f in sort([op(c["Fields"])]) do
    printf("  %a::%a\n",f,c["FieldType"][f]);
   od;
   printf("\n");
  fi;

  if c["StaticFields"] <> {} then
   printf("Static fields:\n");
   for f in sort([op(c["StaticFields"])]) do
    printf("  %a = %a\n",f,c["StaticFieldValue"][f]);
   od;
   printf("\n");
  fi;

  if c["IndirectFields"] <> {} then
   printf("Indirect fields:\n");
   for f in sort([op(c["IndirectFields"])]) do
    printf("  %a -> %a\n",f,c["FieldIndirectionTable"][f]);
   od;
   printf("\n");
  fi;

  if c["Methods"] <> {} then
   mt := eval(c["MethodType"]);
   ms := eval(c["MethodSignature"]);

   printf("Methods:\n");
   for m in sort([op(c["Methods"])]) do
    s := sprintf("%a",ms[m]);
    if length(s) > 1 then s := substring(s,2..-2); fi;
    t := mt[m];
    printf("  %a(%s)::%a\n",m,s,t);
   od;
   printf("\n");
  fi;

  if c["IndirectMethods"] <> {} then
   printf("Indirect methods:\n");
   for m in sort([op(c["IndirectFields"])]) do
    printf("  %a -> %a\n",m,c["MethodIndirectionTable"][m]);
   od;
   printf("\n");
  fi;

  if c["Parents"] <> {} then
   printf("Parents: %a\n\n",c["Parents"]);
  fi;
 end:

#@ `Class/ShowMethod` 
`Class/ShowMethod` := 
 proc(classname::name,methodname::name)
  print(eval(cat(classname,"!",methodname)));
 end:

#@ `Class/ShowConstructor` 
`Class/ShowConstructor` := 
 proc(classname::name)
  print(eval(eval(cat(`class/`,classname))["Constructor"]));
 end:

#@ `Class/List` 
`Class/List` := 
 proc()
  local classes;
  classes := map(convert,[anames()],string);
  classes := select(util_startswith,classes,"class/");
  classes := sort(map(substring,classes,7..-1));
 end:
