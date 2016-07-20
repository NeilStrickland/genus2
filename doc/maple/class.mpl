<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># This file sets up a framework for object oriented programming in Maple.
<a name="line_2"></a># It was borrowed from another project and so has various features that
<a name="line_3"></a># are not necessarily relevant here.
<a name="line_4"></a>
<a name="line_5"></a># Most of our code is not object oriented, but we have found the OO 
<a name="line_6"></a># approach to be useful in some places.  For example, we represent 
<a name="line_7"></a># triangulations and quadrature rules as objects.
<a name="line_8"></a>
<a name="line_9"></a>######################################################################
<a name="line_10"></a>
<a name="line_11"></a>`Package/SaveNames`        := [];
<a name="line_12"></a>`Package/SaveNames/Hidden` := [];
<a name="line_13"></a>`Package/SaveClasses`      := [];
<a name="line_14"></a>`Package/Dependencies`     := [];
<a name="line_15"></a>
<a name="line_16"></a>`Package/Doc/ClassHeaderFormat` := "\n<html>\n <head><title>%s</title><script type=\"text/x-mathjax-config\">\nMathJax.Hub.Config({ tex2jax: {inlineMath: [['$','$']]}});</script>\n<script src=\"https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_CHTML\"></script>\n</head>\n <body  style=\"width:700px\">\n  <h1>Class %s</h1><br/>\n":
<a name="line_17"></a>
<a name="line_18"></a>`Package/Doc/ClassFooter` := "\n </body>\n</html>\n":
<a name="line_19"></a>
<a name="line_20"></a>PackDocDir := cat(doc_dir,"/");
<a name="line_21"></a>
<a name="line_22"></a>
<a name="line_23"></a>Package :=
<a name="line_24"></a> proc(packname::string,doc::string)
<a name="line_25"></a>  global `Package/Name`,
<a name="line_26"></a>         `Package/SaveNames`,`Package/SaveNames/Hidden`,
<a name="line_27"></a>         `Package/SaveClasses`,
<a name="line_28"></a>         `Package/Dependencies`;
<a name="line_29"></a>
<a name="line_30"></a>  if (assigned(`Package/Name`)) then
<a name="line_31"></a>   ERROR("Already defining a package");
<a name="line_32"></a>  fi;
<a name="line_33"></a>
<a name="line_34"></a>  `Package/Name`             := packname;
<a name="line_35"></a>  `Package/SaveNames`        := [];
<a name="line_36"></a>  `Package/SaveNames/Hidden` := [];
<a name="line_37"></a>  `Package/SaveClasses`      := [];
<a name="line_38"></a>  `Package/Dependencies`     := [];
<a name="line_39"></a>   
<a name="line_40"></a>  NULL;
<a name="line_41"></a> end:
<a name="line_42"></a>
<a name="line_43"></a>
<a name="line_44"></a>`Package/Assign` :=
<a name="line_45"></a> proc(namspec::uneval,doc::string,val::anything,
<a name="line_46"></a>     # optional
<a name="line_47"></a>     hide_::boolean
<a name="line_48"></a>     )
<a name="line_49"></a>  local namspec1,nam,namstring,typespec,params,s,d,i,hide,trans;
<a name="line_50"></a>  global `Package/SaveNames`,`Package/SaveNames/Hidden`,
<a name="line_51"></a>         `Package/ReportAssignment`;
<a name="line_52"></a>  
<a name="line_53"></a>  hide := `if`(nargs > 3, hide_, false);
<a name="line_54"></a>
<a name="line_55"></a>  # nam is the name to be assigned (an unevaluated name)
<a name="line_56"></a>  # namstring is the string version of nam
<a name="line_57"></a>  # val is the value to be assigned to nam
<a name="line_58"></a>  # typespec is the declared type for nam
<a name="line_59"></a>  #   (or the declared return type, if val is a procedure,
<a name="line_60"></a>  #    or the name 'void' if val is a procedure that does 
<a name="line_61"></a>  #    not return a value.)
<a name="line_62"></a>
<a name="line_63"></a>  nam := evaln(namspec); # NB evaln(x::integer) = x
<a name="line_64"></a>  namstring := convert(nam,string);
<a name="line_65"></a>  namspec1 := eval(namspec,1);
<a name="line_66"></a>  if type(namspec1,`::`) then
<a name="line_67"></a>   typespec := op(2,namspec1);
<a name="line_68"></a>   if not(type(val,procedure)) and 
<a name="line_69"></a>      not(type(typespec,type)) then
<a name="line_70"></a>    ERROR(
<a name="line_71"></a>     sprintf(
<a name="line_72"></a>      __("Type specification %a for %a is not a valid type."),
<a name="line_73"></a>      typespec,nam));
<a name="line_74"></a>   fi;
<a name="line_75"></a>   if not(type(val,procedure)) and
<a name="line_76"></a>      not(type(val,typespec)) then
<a name="line_77"></a>    ERROR(
<a name="line_78"></a>     sprintf(
<a name="line_79"></a>      __("Assigned value %a for %a does not have declared type %a."),
<a name="line_80"></a>      val,nam,typespec));
<a name="line_81"></a>   fi;  
<a name="line_82"></a>  else
<a name="line_83"></a>   typespec := NULL;
<a name="line_84"></a>  fi;
<a name="line_85"></a>
<a name="line_86"></a>  # Remember that nam is to be saved in the .m file for the package
<a name="line_87"></a>  if (hide = true) then
<a name="line_88"></a>   `Package/SaveNames/Hidden` := [op(`Package/SaveNames/Hidden`),namstring];
<a name="line_89"></a>  else
<a name="line_90"></a>   `Package/SaveNames` := [op(`Package/SaveNames`),namstring];
<a name="line_91"></a>
<a name="line_92"></a>   # Now construct the HTML documentation for this assignment
<a name="line_93"></a>   if type(val,procedure) then
<a name="line_94"></a>    params := [op(1,eval(val))];
<a name="line_95"></a>    if params = [] then
<a name="line_96"></a>     s := sprintf("%a()",nam);
<a name="line_97"></a>    else
<a name="line_98"></a>     s := sprintf("%a(",nam);
<a name="line_99"></a>     s := s, op(map((p) -> (`Package/ParamToHTML`(p),","),params));
<a name="line_100"></a>     s := cat(s[1..-2],")");
<a name="line_101"></a>    fi;
<a name="line_102"></a>   else
<a name="line_103"></a>    s := sprintf("%a",nam);
<a name="line_104"></a>   fi;
<a name="line_105"></a>   if typespec <> NULL then
<a name="line_106"></a>    s := sprintf("%s::#%a#",s,typespec);
<a name="line_107"></a>   fi;
<a name="line_108"></a>
<a name="line_109"></a>   d := `Package/ConvertAt`(doc);
<a name="line_110"></a>   while true do
<a name="line_111"></a>    i := searchtext("$Value",d);
<a name="line_112"></a>    if (i > 0) then
<a name="line_113"></a>     d := cat(substring(d,1..(i-1)),
<a name="line_114"></a>	      sprintf("%a",val),
<a name="line_115"></a>	      substring(d,(i+6)..-1));
<a name="line_116"></a>    else
<a name="line_117"></a>     break;
<a name="line_118"></a>    fi;
<a name="line_119"></a>   od;
<a name="line_120"></a>  fi;
<a name="line_121"></a>
<a name="line_122"></a>  # This actually performs the assignment
<a name="line_123"></a>  assign(nam = val);
<a name="line_124"></a>
<a name="line_125"></a>  # Report the assignment, if reporting is switched on
<a name="line_126"></a>  if (`Package/ReportAssignment` = true) then
<a name="line_127"></a>   printf("Assigned %s\n",nam);
<a name="line_128"></a>  fi;
<a name="line_129"></a>
<a name="line_130"></a> end:
<a name="line_131"></a>
<a name="line_132"></a>
<a name="line_133"></a>`Package/Assign/Hidden` :=
<a name="line_134"></a> proc(namspec::uneval,val::anything)
<a name="line_135"></a>  `Package/Assign`(namspec,"",val,true);
<a name="line_136"></a> end:
<a name="line_137"></a>
<a name="line_138"></a>
<a name="line_139"></a>######################################################################
<a name="line_140"></a>
<a name="line_141"></a>EndPackage :=
<a name="line_142"></a> proc()
<a name="line_143"></a>  local classname,loadname,savefile,savedir,savestatement,docfile,
<a name="line_144"></a>        nam,trans,s;
<a name="line_145"></a>  global `Package/Name`,`Package/SaveNames`,`Package/SaveNames/Hidden`,
<a name="line_146"></a>         `Package/SaveClasses`,
<a name="line_147"></a>         `Package/List`,`Package/Classes`,
<a name="line_148"></a>         `Package/Entries`,`Package/AllEntries`,
<a name="line_149"></a>         `Package/Dependencies`,`Package/Depend`,
<a name="line_150"></a>         `Package/Index`,`Package/FindTranslationStrings`,
<a name="line_151"></a>         `Package/TranslationStrings`;
<a name="line_152"></a>
<a name="line_153"></a>  if not(assigned(`Package/Name`)) then
<a name="line_154"></a>   ERROR("Not defining a package.");
<a name="line_155"></a>  fi;
<a name="line_156"></a>
<a name="line_157"></a>  if assigned(`Package/List`) and
<a name="line_158"></a>     type(`Package/List`,set) then
<a name="line_159"></a>   `Package/List` := `Package/List` union {`Package/Name`};
<a name="line_160"></a>  else
<a name="line_161"></a>   `Package/List` := {`Package/Name`};
<a name="line_162"></a>  fi;
<a name="line_163"></a>
<a name="line_164"></a>  if not(assigned(`Package/Classes`)) then 
<a name="line_165"></a>   `Package/Classes` := table([]);
<a name="line_166"></a>  fi;
<a name="line_167"></a>  `Package/Classes`[`Package/Name`] := `Package/SaveClasses`;
<a name="line_168"></a>
<a name="line_169"></a>  if not(assigned(`Package/Entries`)) then 
<a name="line_170"></a>   `Package/Entries` := table([]);
<a name="line_171"></a>  fi;
<a name="line_172"></a>  `Package/Entries`[`Package/Name`] := `Package/SaveNames`;
<a name="line_173"></a>
<a name="line_174"></a>  for classname in `Package/SaveClasses` do
<a name="line_175"></a>   `Package/SaveNames` :=
<a name="line_176"></a>     [op(`Package/SaveNames`),cat("class/",classname)];
<a name="line_177"></a>
<a name="line_178"></a>   `Package/SaveNames/Hidden` :=
<a name="line_179"></a>      [op(`Package/SaveNames/Hidden`),
<a name="line_180"></a>       cat("index/",classname),
<a name="line_181"></a>       cat("type/",classname),
<a name="line_182"></a>       cat("new/",classname)];
<a name="line_183"></a>  od;
<a name="line_184"></a>
<a name="line_185"></a>  if not(assigned(`Package/AllEntries`)) then 
<a name="line_186"></a>   `Package/AllEntries` := table([]);
<a name="line_187"></a>  fi;
<a name="line_188"></a>  `Package/AllEntries`[`Package/Name`] := `Package/SaveNames`;
<a name="line_189"></a>
<a name="line_190"></a>  `Package/Depend`[`Package/Name`] := `Package/Dependencies`;
<a name="line_191"></a>
<a name="line_192"></a>  if not(assigned(`Package/Index`)) then 
<a name="line_193"></a>   `Package/Index` := table([]);
<a name="line_194"></a>  fi;
<a name="line_195"></a>  for nam in `Package/SaveNames` do
<a name="line_196"></a>   `Package/Index`[nam] := `Package/Name`;
<a name="line_197"></a>  od;
<a name="line_198"></a>  for nam in `Package/SaveClasses` do
<a name="line_199"></a>   `Package/Index`[nam] := `Package/Name`;
<a name="line_200"></a>  od;
<a name="line_201"></a>
<a name="line_202"></a>  loadname := cat(`Package/Name`,"/IsLoaded");
<a name="line_203"></a>
<a name="line_204"></a>  `Package/SaveNames` :=
<a name="line_205"></a>    [op(`Package/SaveNames`),"Package/Dependencies",loadname];
<a name="line_206"></a>                
<a name="line_207"></a>  assign(convert(loadname,name) = true);
<a name="line_208"></a>
<a name="line_209"></a>  if (`Package/FindTranslationStrings` = true) then
<a name="line_210"></a>   if not(type([`Package/TranslationStrings`],[list])) then
<a name="line_211"></a>    `Package/TranslationStrings` := []:
<a name="line_212"></a>   fi;
<a name="line_213"></a>   for nam in 
<a name="line_214"></a>     [op(`Package/SaveNames`),op(`Package/SaveNames/Hidden`)] do
<a name="line_215"></a>    trans := 
<a name="line_216"></a>     TranslationStrings(eval(convert(nam,name)));
<a name="line_217"></a>    trans := map((x,p,n) -> [x,p,n],trans,`Package/Name`,nam);
<a name="line_218"></a>    `Package/TranslationStrings` := 
<a name="line_219"></a>     [op(`Package/TranslationStrings`),op(trans)];
<a name="line_220"></a>   od;
<a name="line_221"></a>  fi;
<a name="line_222"></a>  NULL;
<a name="line_223"></a>
<a name="line_224"></a>  unassign('`Package/Name`');
<a name="line_225"></a> end:
<a name="line_226"></a>
<a name="line_227"></a>
<a name="line_228"></a>######################################################################
<a name="line_229"></a>
<a name="line_230"></a>`Package/MakeAllDocumentation` := 
<a name="line_231"></a> proc()
<a name="line_232"></a>  local c,cc,f,s,index_file,index_html;
<a name="line_233"></a>  global `Package/List`;
<a name="line_234"></a>
<a name="line_235"></a>  index_html := cat(
<a name="line_236"></a>   "<html>\n",
<a name="line_237"></a>   "<head>\n",
<a name="line_238"></a>   "<title>Classes</title>\n",
<a name="line_239"></a>   "<style type=\"text/css\">\n",
<a name="line_240"></a>   "table { empty-cells:show; }\n",
<a name="line_241"></a>   "\n",
<a name="line_242"></a>   "table.edged { \n",
<a name="line_243"></a>   " border-collapse: collapse;\n",
<a name="line_244"></a>   "}\n",
<a name="line_245"></a>   "\n",
<a name="line_246"></a>   "table.edged td {\n",
<a name="line_247"></a>   " padding: 3px 0.5em;\n",
<a name="line_248"></a>   " margin-left: 3px;\n",
<a name="line_249"></a>   " border: 1px solid #CCCCCC; \n",
<a name="line_250"></a>   " vertical-align: top;\n",
<a name="line_251"></a>   "}\n",
<a name="line_252"></a>   "</style>\n",
<a name="line_253"></a>   "<script type=\"text/x-mathjax-config\">\n",
<a name="line_254"></a>   "MathJax.Hub.Config({ tex2jax: {inlineMath: [['$','$']]}});\n",
<a name="line_255"></a>   "</script>\n",
<a name="line_256"></a>   "<script src=\"https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_CHTML\"></script>\n",
<a name="line_257"></a>   "</head>\n",
<a name="line_258"></a>   "<body>\n",
<a name="line_259"></a>   "<h1>Index of classes</h1>\n<br/>\n",
<a name="line_260"></a>   "<table class=\"edged\">\n"
<a name="line_261"></a>  );
<a name="line_262"></a>
<a name="line_263"></a>  for c in `Package/SaveClasses` do
<a name="line_264"></a>   f := cat(doc_dir,"/classes/",c,".html");
<a name="line_265"></a>   cc := eval(convert(cat("class/",c),name));
<a name="line_266"></a>   traperror(fclose(f));
<a name="line_267"></a>   fprintf(f,"%s\n",cc["FullDoc"]);
<a name="line_268"></a>   traperror(fclose(f));
<a name="line_269"></a>   index_html := cat(
<a name="line_270"></a>    index_html,
<a name="line_271"></a>    "<tr>\n",
<a name="line_272"></a>    "<td width=\"200\"><a href=\"classes/",c,".html\">",c,"</a></td>\n",
<a name="line_273"></a>    "<td width=\"600\">",cc["Doc"],"</td>",
<a name="line_274"></a>    "</tr>\n"
<a name="line_275"></a>   );
<a name="line_276"></a>  od:
<a name="line_277"></a>
<a name="line_278"></a>  index_html := cat(
<a name="line_279"></a>   index_html,
<a name="line_280"></a>   "</table>\n</body>\n</html>\n"
<a name="line_281"></a>  );
<a name="line_282"></a>
<a name="line_283"></a>  index_file := cat(doc_dir,"/classes.html");
<a name="line_284"></a>  traperror(fclose(index_file));
<a name="line_285"></a>  fprintf(index_file,index_html);
<a name="line_286"></a>  traperror(fclose(index_file));
<a name="line_287"></a>
<a name="line_288"></a>  NULL;
<a name="line_289"></a>
<a name="line_290"></a> end:
<a name="line_291"></a>
<a name="line_292"></a>######################################################################
<a name="line_293"></a>
<a name="line_294"></a>`Package/ConvertAt` := 
<a name="line_295"></a> proc(s::string)
<a name="line_296"></a>  local t,i,j,k,l,n,m,p;
<a name="line_297"></a>
<a name="line_298"></a>  t := "";
<a name="line_299"></a>  i := 1;
<a name="line_300"></a>  while true do
<a name="line_301"></a>   j := `if`(i <= length(s),searchtext("@",s,i..-1),0);
<a name="line_302"></a>   if j > 0 then 
<a name="line_303"></a>    if i + j <= length(s) and
<a name="line_304"></a>     substring(s,(i+j-1)..(i+j)) = "@@" then
<a name="line_305"></a>     t := t,substring(s,i..(i+j-1));
<a name="line_306"></a>     i := i+j+1;
<a name="line_307"></a>    else
<a name="line_308"></a>     k := searchtext("@",s,i+j..-1);
<a name="line_309"></a>     if k = 0 then
<a name="line_310"></a>      ERROR(
<a name="line_311"></a>       sprintf("Unmatched @ in documentation string:\n%s\n",s));
<a name="line_312"></a>     else
<a name="line_313"></a>      t := t,
<a name="line_314"></a>           substring(s,i..(i+j-2)),
<a name="line_315"></a>           "<font color='green'>",
<a name="line_316"></a>           `Package/HTMLEscape`(substring(s,(i+j)..(i+j+k-2))),
<a name="line_317"></a>           "</font>";
<a name="line_318"></a>      i := i+j+k;
<a name="line_319"></a>     fi;
<a name="line_320"></a>    fi;
<a name="line_321"></a>   else
<a name="line_322"></a>    t := cat(t,substring(s,i..-1));
<a name="line_323"></a>    break;
<a name="line_324"></a>   fi;
<a name="line_325"></a>  od;
<a name="line_326"></a>  RETURN(t);
<a name="line_327"></a> end:
<a name="line_328"></a>
<a name="line_329"></a>
<a name="line_330"></a>######################################################################
<a name="line_331"></a>
<a name="line_332"></a>`Package/HTMLEscape` :=
<a name="line_333"></a> proc(s::string)
<a name="line_334"></a>  local b;
<a name="line_335"></a>  b := convert(s,bytes);
<a name="line_336"></a>
<a name="line_337"></a>  b := subs( 60 = (38, 108, 116, 59),            # &lt;
<a name="line_338"></a>      62 = (38, 103, 116, 59),            # &gt;
<a name="line_339"></a>      34 = (38, 113, 117, 111, 116, 59),  # &quot;
<a name="line_340"></a>      b);
<a name="line_341"></a>
<a name="line_342"></a>  convert(b,bytes);
<a name="line_343"></a> end:
<a name="line_344"></a>
<a name="line_345"></a>
<a name="line_346"></a>######################################################################
<a name="line_347"></a>
<a name="line_348"></a># This function converts #...# sequences to links as explained at the
<a name="line_349"></a># top of this file.  
<a name="line_350"></a>#
<a name="line_351"></a># The argument 'depth_' is used to construct relative URL's.  Suppose
<a name="line_352"></a># that the return value of this function is part of the documentation
<a name="line_353"></a># for a package 'aim/foo/Bar', and thus will be included in the file
<a name="line_354"></a># aim/foo/Bar.html.  This is two levels down from the top documentation
<a name="line_355"></a># directory, so URL's for other documentation files should be prefixed
<a name="line_356"></a># with the string "../../".  This is achieved by putting depth_ := 2.
<a name="line_357"></a>#
<a name="line_358"></a># The function uses the global variable `Package/Index`, which is a
<a name="line_359"></a># table.  If a name n is defined in a package named p, and ns is the
<a name="line_360"></a># string version of n, then `Package/Index`[ns] is set to p.  Similarly,
<a name="line_361"></a># if a class named c is declared in a package named p, and cs is the
<a name="line_362"></a># string version of c, then `Package/Index`[cs] is set to p.
<a name="line_363"></a>
<a name="line_364"></a>`Package/ConvertHash` :=
<a name="line_365"></a> proc(t::string,depth_::nonnegint)
<a name="line_366"></a>  local u,i,j,k,l,n,m,tm,p,prefix;
<a name="line_367"></a>  global `Package/Index`;
<a name="line_368"></a>
<a name="line_369"></a>  # set the prefix for relative URL's
<a name="line_370"></a>  if nargs = 1 then
<a name="line_371"></a>   prefix := "";
<a name="line_372"></a>  else
<a name="line_373"></a>   prefix := cat("../" $ depth_);
<a name="line_374"></a>  fi;
<a name="line_375"></a>
<a name="line_376"></a>  # u is s sequence of strings that will be concatenated to give the
<a name="line_377"></a>  # return value.
<a name="line_378"></a>  u := "";
<a name="line_379"></a>
<a name="line_380"></a>  # i is the position in t of the first character that has not yet
<a name="line_381"></a>  # been processed.
<a name="line_382"></a>  i := 1;
<a name="line_383"></a>
<a name="line_384"></a>  while true do
<a name="line_385"></a>   j := `if`(i <= length(t),searchtext("#",t,i..-1),0);
<a name="line_386"></a>   if j > 0 then 
<a name="line_387"></a>    # there is a '#' at position i+j-1 in t
<a name="line_388"></a>    if i + j <= length(t) and
<a name="line_389"></a>     substring(t,(i+j-1)..(i+j)) = "##" then
<a name="line_390"></a>     # Convert '##' to a literal '#' character
<a name="line_391"></a>     u := u,substring(t,i..(i+j-1));
<a name="line_392"></a>     i := i+j+1;
<a name="line_393"></a>    else
<a name="line_394"></a>     k := searchtext("#",t,i+j..-1); # find the matching '#'
<a name="line_395"></a>     if k = 0 then
<a name="line_396"></a>      ERROR(
<a name="line_397"></a>       sprintf("Unmatched # in documentation string:\n%s\n",t));
<a name="line_398"></a>     else
<a name="line_399"></a>      # the matching '#' is at position i+j+k-1 in t.
<a name="line_400"></a>
<a name="line_401"></a>      n := substring(t,(i+j)..(i+j+k-2));
<a name="line_402"></a>      # n is the name enclosed in #...#, possibly including backquotes
<a name="line_403"></a>
<a name="line_404"></a>      m := n;
<a name="line_405"></a>      if m <> "" and substring(m,1..1) = "`" then
<a name="line_406"></a>       m := substring(m,2..-1);
<a name="line_407"></a>      fi;
<a name="line_408"></a>      if m <> "" and substring(m,-1..-1) = "`" then
<a name="line_409"></a>       m := substring(m,1..-2);
<a name="line_410"></a>      fi;
<a name="line_411"></a>      # m is the name enclosed in #...#, without backquotes
<a name="line_412"></a>
<a name="line_413"></a>      tm := cat("type/",m);
<a name="line_414"></a>
<a name="line_415"></a>      if assigned(`Package/Index`[m]) then
<a name="line_416"></a>       p := cat(prefix,`Package/Index`[m]);
<a name="line_417"></a>       l := sprintf("<a href='%s.html#%s'>%s</a>",p,m,n);
<a name="line_418"></a>       u := u,substring(t,i..(i+j-2)),l;
<a name="line_419"></a>      elif assigned(`Package/Index`[tm]) then
<a name="line_420"></a>       p := cat(prefix,`Package/Index`[tm]);
<a name="line_421"></a>       l := sprintf("<a href='%s.html#%s'>%s</a>",p,tm,n);
<a name="line_422"></a>       u := u,substring(t,i..(i+j-2)),l;
<a name="line_423"></a>      else
<a name="line_424"></a>       u := u,substring(t,i..(i+j-2)),n;
<a name="line_425"></a>      fi;
<a name="line_426"></a>      i := i+j+k;
<a name="line_427"></a>     fi;
<a name="line_428"></a>    fi;
<a name="line_429"></a>   else
<a name="line_430"></a>    u := cat(u,substring(t,i..-1));
<a name="line_431"></a>    break;
<a name="line_432"></a>   fi;
<a name="line_433"></a>  od;
<a name="line_434"></a>  RETURN(u);
<a name="line_435"></a> end:
<a name="line_436"></a>
<a name="line_437"></a>
<a name="line_438"></a>######################################################################
<a name="line_439"></a>
<a name="line_440"></a>`Package/TypeToHTML` := 
<a name="line_441"></a> proc(t)
<a name="line_442"></a>###TODO: evaln(type)
<a name="line_443"></a>  local s, ts, p;
<a name="line_444"></a>
<a name="line_445"></a>  if type(t,symbol) then
<a name="line_446"></a>   RETURN(sprintf("#%a#",t));
<a name="line_447"></a>  elif type(t,{complex(numeric),string}) then
<a name="line_448"></a>   RETURN(sprintf("%A",t));
<a name="line_449"></a>  else
<a name="line_450"></a>   RETURN(sprintf("%A",map(`Package/TypeToHTML`,t)));
<a name="line_451"></a>  fi;
<a name="line_452"></a> end:
<a name="line_453"></a>
<a name="line_454"></a>
<a name="line_455"></a>######################################################################
<a name="line_456"></a>
<a name="line_457"></a>`Package/ParamToHTML` :=
<a name="line_458"></a> proc(p::{symbol,`::`,assignment})
<a name="line_459"></a>  if type(p,symbol) then
<a name="line_460"></a>   RETURN(sprintf("<font color='olive'>%a</font>",p));
<a name="line_461"></a>  elif type(p,assignment) then
<a name="line_462"></a>   RETURN(sprintf("%s := %a",`Package/ParamToHTML`(op(1,p)),op(2(p))));
<a name="line_463"></a>  else
<a name="line_464"></a>   RETURN(
<a name="line_465"></a>    sprintf("<font color='olive'>%a</font>::%s",
<a name="line_466"></a>            op(1,p),`Package/TypeToHTML`(op(2,p))));
<a name="line_467"></a>  fi;
<a name="line_468"></a> end:
<a name="line_469"></a>
<a name="line_470"></a>
<a name="line_471"></a>######################################################################
<a name="line_472"></a>
<a name="line_473"></a>convert_at := 
<a name="line_474"></a> proc(s::string)
<a name="line_475"></a>  local t,i,j,k,l,n,m,p;
<a name="line_476"></a>
<a name="line_477"></a>  t := "";
<a name="line_478"></a>  i := 1;
<a name="line_479"></a>  while true do
<a name="line_480"></a>   j := `if`(i <= length(s),searchtext("@",s,i..-1),0);
<a name="line_481"></a>   if j > 0 then 
<a name="line_482"></a>    if i + j <= length(s) and
<a name="line_483"></a>     substring(s,(i+j-1)..(i+j)) = "@@" then
<a name="line_484"></a>     t := t,substring(s,i..(i+j-1));
<a name="line_485"></a>     i := i+j+1;
<a name="line_486"></a>    else
<a name="line_487"></a>     k := searchtext("@",s,i+j..-1);
<a name="line_488"></a>     if k = 0 then
<a name="line_489"></a>      ERROR(
<a name="line_490"></a>       sprintf("Unmatched @ in documentation string:\n%s\n",s));
<a name="line_491"></a>     else
<a name="line_492"></a>      t := t,
<a name="line_493"></a>           substring(s,i..(i+j-2)),
<a name="line_494"></a>           "<font color=\"green\">",
<a name="line_495"></a>           `Package/HTMLEscape`(substring(s,(i+j)..(i+j+k-2))),
<a name="line_496"></a>           "</font>";
<a name="line_497"></a>      i := i+j+k;
<a name="line_498"></a>     fi;
<a name="line_499"></a>    fi;
<a name="line_500"></a>   else
<a name="line_501"></a>    t := cat(t,substring(s,i..-1));
<a name="line_502"></a>    break;
<a name="line_503"></a>   fi;
<a name="line_504"></a>  od;
<a name="line_505"></a>  RETURN(t);
<a name="line_506"></a> end:
<a name="line_507"></a>
<a name="line_508"></a>
<a name="line_509"></a>######################################################################
<a name="line_510"></a>
<a name="line_511"></a>`Class/Constructor` := 
<a name="line_512"></a> proc(class::table)
<a name="line_513"></a>  local t,f;
<a name="line_514"></a>
<a name="line_515"></a>  t := table(convert(class["Name"],name));
<a name="line_516"></a>
<a name="line_517"></a>  for f in class["Fields"] do
<a name="line_518"></a>   t[f] := eval(class["FieldDefault"][f]);
<a name="line_519"></a>  od;
<a name="line_520"></a>
<a name="line_521"></a>  class["Constructor"](t,args[2..-1]);
<a name="line_522"></a>
<a name="line_523"></a>  eval(t);
<a name="line_524"></a> end:
<a name="line_525"></a>
<a name="line_526"></a>`Class/SetIndexFunction` := 
<a name="line_527"></a> proc(x::table,f::string)
<a name="line_528"></a>  subsop(1 = convert(f,name),op(x));
<a name="line_529"></a> end:
<a name="line_530"></a>
<a name="line_531"></a>`Class/IndexFunction` := 
<a name="line_532"></a> proc(class,indices,tbl)
<a name="line_533"></a>  local index,rest,this,val,reqtype,method,N,C;
<a name="line_534"></a>
<a name="line_535"></a>  if (nargs = 3) then
<a name="line_536"></a>   if nops(indices) = 0 then
<a name="line_537"></a>    ERROR("Empty field name");
<a name="line_538"></a>   fi;
<a name="line_539"></a>   index := indices[1];
<a name="line_540"></a>
<a name="line_541"></a>   if nops(indices) = 1 then
<a name="line_542"></a>    if member(index,class["Methods"]) then
<a name="line_543"></a>     # A method with no arguments should override a field with the 
<a name="line_544"></a>     # same name in a parent class
<a name="line_545"></a>     this := `Class/SetIndexFunction`(tbl,class["Name"]);
<a name="line_546"></a>     RETURN(eval(convert(cat(class["Name"],"!",index),name))(this));
<a name="line_547"></a>    elif member(index,class["Fields"]) then
<a name="line_548"></a>     RETURN(eval(tbl)[index]);
<a name="line_549"></a>    elif member(index,class["StaticFields"]) then
<a name="line_550"></a>     RETURN(class["StaticFieldValue"][index]);
<a name="line_551"></a>    elif member(index,class["IndirectStaticFields"]) then
<a name="line_552"></a>     N := class["StaticFieldIndirectionTable"][index];
<a name="line_553"></a>     C := eval(cat(`class/`,convert(N,name)));
<a name="line_554"></a>     RETURN(C["StaticFieldValue"][index]);
<a name="line_555"></a>    elif member(index,class["IndirectFields"]) then
<a name="line_556"></a>     RETURN(eval(tbl)[class["FieldIndirectionTable"][index]][index]);
<a name="line_557"></a>    fi;
<a name="line_558"></a>   fi;
<a name="line_559"></a>
<a name="line_560"></a>   rest := op(indices[2..-1]);
<a name="line_561"></a>
<a name="line_562"></a>   if member(index,class["Methods"]) then
<a name="line_563"></a>    this := `Class/SetIndexFunction`(tbl,class["Name"]);
<a name="line_564"></a>    RETURN(eval(convert(cat(class["Name"],"!",index),name))(this,rest));
<a name="line_565"></a>   elif member(index,class["IndirectMethods"]) then
<a name="line_566"></a>    RETURN(eval(tbl)[class["MethodIndirectionTable"][index]][op(indices)]);
<a name="line_567"></a>   fi;
<a name="line_568"></a>
<a name="line_569"></a>   ERROR(sprintf(
<a name="line_570"></a>    "Invalid field or method in class %s: %a",
<a name="line_571"></a>    class["Name"],
<a name="line_572"></a>    indices
<a name="line_573"></a>   ));
<a name="line_574"></a>
<a name="line_575"></a>  else
<a name="line_576"></a>   if nops(indices) = 0 then
<a name="line_577"></a>    ERROR("Empty field name in assignment");
<a name="line_578"></a>   elif nops(indices) > 1 then
<a name="line_579"></a>    ERROR(sprintf("Too many indices in assignment: %a",indices));
<a name="line_580"></a>   fi;
<a name="line_581"></a>   index := indices[1];
<a name="line_582"></a>
<a name="line_583"></a>   val := op(args[4]);
<a name="line_584"></a>
<a name="line_585"></a>   if member(index,class["Fields"]) or
<a name="line_586"></a>      member(index,class["IndirectFields"]) then 
<a name="line_587"></a>    reqtype := class["FieldType"][index];
<a name="line_588"></a>   elif member(index,class["StaticFields"]) then
<a name="line_589"></a>    reqtype := class["StaticFieldType"][index];
<a name="line_590"></a>   elif member(index,class["IndirectStaticFields"]) then
<a name="line_591"></a>    N := class["StaticFieldIndirectionTable"][index];
<a name="line_592"></a>    C := eval(cat(`class/`,convert(N,name)));
<a name="line_593"></a>    reqtype := C["StaticFieldType"][index];
<a name="line_594"></a>   else
<a name="line_595"></a>    ERROR(sprintf(
<a name="line_596"></a>     "Assignment to invalid field in class %s: %a",
<a name="line_597"></a>     class["Name"],
<a name="line_598"></a>     indices
<a name="line_599"></a>    ));
<a name="line_600"></a>   fi;
<a name="line_601"></a>
<a name="line_602"></a>   if nops([val]) = 1 and not(type(val,reqtype)) then
<a name="line_603"></a>    ERROR(sprintf(
<a name="line_604"></a>     "Type error in assignment: field %a in class %a should have type %a",
<a name="line_605"></a>     index,class["Name"],reqtype));
<a name="line_606"></a>   fi;
<a name="line_607"></a>   if nops([val]) > 1 and reqtype <> anything then
<a name="line_608"></a>    ERROR(sprintf(
<a name="line_609"></a>     "Type error in assignment: field %a in class %a should have type %a",
<a name="line_610"></a>     index,class["Name"],reqtype));
<a name="line_611"></a>   fi;
<a name="line_612"></a>
<a name="line_613"></a>   if member(index,class["Fields"]) then 
<a name="line_614"></a>    tbl[index] := eval(val,1);  
<a name="line_615"></a>   elif member(index,class["StaticFields"]) then
<a name="line_616"></a>    class["StaticFieldValue"][index] := eval(val,1);
<a name="line_617"></a>   elif member(index,class["IndirectStaticFields"]) then
<a name="line_618"></a>    N := class["StaticFieldIndirectionTable"][index];
<a name="line_619"></a>    C := eval(cat(`class/`,convert(N,name)));
<a name="line_620"></a>    C["StaticFieldValue"][index] := eval(val,1);
<a name="line_621"></a>   elif member(index,class["IndirectFields"]) then
<a name="line_622"></a>    tbl[class["FieldIndirectionTable"][index]][index] := eval(val,1);
<a name="line_623"></a>   fi;
<a name="line_624"></a>  fi;
<a name="line_625"></a> end:
<a name="line_626"></a>
<a name="line_627"></a>`Class/TypeFunction` := 
<a name="line_628"></a> proc(class,x)
<a name="line_629"></a>  local xclassname,xclass;
<a name="line_630"></a>  if not type(eval(x),table) then RETURN(false); fi;
<a name="line_631"></a>  xclassname := op(1,eval(x));
<a name="line_632"></a>  if xclassname = NULL then RETURN(false); fi;
<a name="line_633"></a>  if xclassname = class["Name"] then RETURN(true); fi;
<a name="line_634"></a>
<a name="line_635"></a>  xclass := convert(cat("class/",xclassname),name);
<a name="line_636"></a>
<a name="line_637"></a>  RETURN(`Class/IsSuperClass`(class,xclass));
<a name="line_638"></a> end:
<a name="line_639"></a>
<a name="line_640"></a>`Class/IsSuperClass` := 
<a name="line_641"></a> proc(class0,class1)
<a name="line_642"></a>  local p;
<a name="line_643"></a>
<a name="line_644"></a>  if class0["Name"] = class1["Name"] then RETURN(true); fi;
<a name="line_645"></a>
<a name="line_646"></a>  for p in class1["Parents"] do
<a name="line_647"></a>   if `Class/IsSuperClass`(class0,convert(cat("class/",p),name)) then
<a name="line_648"></a>    RETURN(true);
<a name="line_649"></a>   fi;
<a name="line_650"></a>  od;
<a name="line_651"></a>
<a name="line_652"></a>  RETURN(false);
<a name="line_653"></a> end:
<a name="line_654"></a>
<a name="line_655"></a>`Class/IsClass` := 
<a name="line_656"></a> proc(classname)
<a name="line_657"></a>  if not(type(classname,string)) then
<a name="line_658"></a>   RETURN(false);
<a name="line_659"></a>  fi;
<a name="line_660"></a>
<a name="line_661"></a>  if eval(convert(cat("class/",classname),name)) =
<a name="line_662"></a>          convert(cat("class/",classname),name) then
<a name="line_663"></a>   RETURN(false);
<a name="line_664"></a>  fi;
<a name="line_665"></a>
<a name="line_666"></a>  RETURN(true);
<a name="line_667"></a> end:
<a name="line_668"></a>
<a name="line_669"></a>`Class/IsObject` := 
<a name="line_670"></a> proc(x)
<a name="line_671"></a>  local xclassname;
<a name="line_672"></a>
<a name="line_673"></a>  if not type(x,table) then RETURN(false); fi;
<a name="line_674"></a>
<a name="line_675"></a>  xclassname := op(1,eval(x));
<a name="line_676"></a>  if xclassname = NULL then RETURN(false); fi;
<a name="line_677"></a>
<a name="line_678"></a>  RETURN(`Class/IsClass`(xclassname));
<a name="line_679"></a> end:
<a name="line_680"></a>
<a name="line_681"></a>`Class/ClassNameOf` := 
<a name="line_682"></a> proc(x)
<a name="line_683"></a>  if not type(x,table) then RETURN(NULL); fi;
<a name="line_684"></a>  RETURN(op(1,eval(x)));
<a name="line_685"></a> end:
<a name="line_686"></a>
<a name="line_687"></a>`Class/ClassOf` :=
<a name="line_688"></a> proc(x)
<a name="line_689"></a>  local xclassname;
<a name="line_690"></a>
<a name="line_691"></a>  xclassname := `Class/ClassNameOf`(x);
<a name="line_692"></a>  if xclassname = NULL or
<a name="line_693"></a>     not(`Class/IsClass`(xclassname)) then
<a name="line_694"></a>   RETURN(NULL);
<a name="line_695"></a>  fi;
<a name="line_696"></a>
<a name="line_697"></a>  RETURN(eval(cat(`class/`,xclassname)));
<a name="line_698"></a> end:
<a name="line_699"></a>
<a name="line_700"></a>`Class/Update` := 
<a name="line_701"></a> proc(x::anything)
<a name="line_702"></a>  local classname,class,present,missing,t,f;
<a name="line_703"></a>
<a name="line_704"></a>  if `Class/IsObject`(x) then
<a name="line_705"></a>   classname := op(1,eval(x));
<a name="line_706"></a>   class := eval(cat(`class/`,classname));
<a name="line_707"></a>   t := subsop(1 = NULL,op(x));
<a name="line_708"></a>   present := map(op,select((y) -> (nops(y) = 1),{indices(t)}));
<a name="line_709"></a>   missing := {op(class["Fields"])} minus present;
<a name="line_710"></a>
<a name="line_711"></a>   for f in missing do
<a name="line_712"></a>    t[f] := eval(class["FieldDefault"][f]);
<a name="line_713"></a>   od;
<a name="line_714"></a>  fi;
<a name="line_715"></a>  NULL;
<a name="line_716"></a> end:
<a name="line_717"></a>
<a name="line_718"></a>`Class/DefaultValue` := 
<a name="line_719"></a> table([
<a name="line_720"></a> numeric = 0,
<a name="line_721"></a> integer = 0,
<a name="line_722"></a> float   = 0,
<a name="line_723"></a> string  = "",
<a name="line_724"></a> list    = [],
<a name="line_725"></a> set     = {}
<a name="line_726"></a> ]):
<a name="line_727"></a>
<a name="line_728"></a><span style="color:red">#@ `Class/Declare` 
</span><a name="line_729"></a>`Class/Declare` :=  
<a name="line_730"></a> proc(classname::string) 
<a name="line_731"></a>  global CLASS,CLASSNAME,
<a name="line_732"></a>  `Package/Name`,`Package/SaveClasses`,`Package/SaveNames/Hidden`;
<a name="line_733"></a>  local
<a name="line_734"></a>   c,indexfunction,typefunction,
<a name="line_735"></a>   constructor,consproc,consdoc,x,s,
<a name="line_736"></a>   fieldname,fieldtype,fielddefault,realtype,
<a name="line_737"></a>   typespecified,defaultspecified,
<a name="line_738"></a>   methodname,fullmethodname,methodtype,
<a name="line_739"></a>   methodbody,methodsig,methoddoc,params,
<a name="line_740"></a>   parent,parenttypes,field,method,
<a name="line_741"></a>   incclass,indfield,indfields,indmethod,indmethods,
<a name="line_742"></a>   classdoc;
<a name="line_743"></a>
<a name="line_744"></a>  if (`Package/ReportAssignment` = true) then
<a name="line_745"></a>   printf("Declaring Class: %s\n",classname);
<a name="line_746"></a>  fi;
<a name="line_747"></a>
<a name="line_748"></a>  parenttypes := NULL;
<a name="line_749"></a>
<a name="line_750"></a>  c := table(["Name" = classname,
<a name="line_751"></a>              "Package" = "",
<a name="line_752"></a>              "Doc" = "",
<a name="line_753"></a>              "FullDoc" = "",
<a name="line_754"></a>	      "Constructor" = proc(this) end,
<a name="line_755"></a>	      "Fields" = {},
<a name="line_756"></a>	      "FieldType" = table(),
<a name="line_757"></a>	      "FieldDefault" = table(),
<a name="line_758"></a>	      "StaticFields" = {},
<a name="line_759"></a>	      "StaticFieldValue" = table(),
<a name="line_760"></a>	      "IndirectFields" = {},
<a name="line_761"></a>	      "FieldIndirectionTable" = table(),
<a name="line_762"></a>	      "IndirectStaticFields" = {},
<a name="line_763"></a>	      "StaticFieldIndirectionTable" = table(),
<a name="line_764"></a>	      "IndirectMethods" = {},
<a name="line_765"></a>	      "MethodIndirectionTable" = table(),
<a name="line_766"></a>	      "Methods" = {},
<a name="line_767"></a>	      "MethodType" = table(),
<a name="line_768"></a>	      "MethodSignature" = table(),
<a name="line_769"></a>	      "MethodTable" = table(),
<a name="line_770"></a>	      "Parents" = {}]);
<a name="line_771"></a>
<a name="line_772"></a>  if assigned(`Package/Name`) then
<a name="line_773"></a>   c["Package"] := `Package/Name`;
<a name="line_774"></a>  fi;
<a name="line_775"></a>
<a name="line_776"></a>  classdoc :=
<a name="line_777"></a>   sprintf(`Package/Doc/ClassHeaderFormat`,classname,classname);
<a name="line_778"></a>
<a name="line_779"></a>  for x in args[2..-1] do
<a name="line_780"></a>   if type(x,string) then 
<a name="line_781"></a>    s := cat("<p>\n",convert_at(x),"\n</p>\n");
<a name="line_782"></a>    classdoc := classdoc,s;
<a name="line_783"></a>    c["Doc"] := cat(c["Doc"],s);
<a name="line_784"></a>   elif not(type(x,list)) then
<a name="line_785"></a>    ERROR(
<a name="line_786"></a>     sprintf("Non-list entry in declaration of class %a",classname));
<a name="line_787"></a>   fi;
<a name="line_788"></a>   if nops(x) = 0 then
<a name="line_789"></a>    ERROR(
<a name="line_790"></a>     sprintf("Empty list in declaration of class %a",classname));
<a name="line_791"></a>   fi;
<a name="line_792"></a>
<a name="line_793"></a>   ##########################
<a name="line_794"></a>   if type(x,list) then
<a name="line_795"></a>    if (x[1] = "Extends") then
<a name="line_796"></a>     if nops(x) = 1 then
<a name="line_797"></a>      ERROR(
<a name="line_798"></a>       sprintf(
<a name="line_799"></a> "Extends clause with no parent class in declaration of class %a",
<a name="line_800"></a> classname));
<a name="line_801"></a>     fi;
<a name="line_802"></a>
<a name="line_803"></a>     if (`Package/ReportAssignment` = true) then
<a name="line_804"></a>      printf("    Extends %a\n",x[2]);
<a name="line_805"></a>     fi;
<a name="line_806"></a>
<a name="line_807"></a>     classdoc := 
<a name="line_808"></a>      classdoc,
<a name="line_809"></a>       sprintf("<b>Extends: </b>#%s#<br/>\n",x[2]);
<a name="line_810"></a>
<a name="line_811"></a>     c["Parents"] := c["Parents"] union {x[2]};
<a name="line_812"></a>     parent := eval(convert(cat("class/",x[2]),name));
<a name="line_813"></a>
<a name="line_814"></a>     c["Fields"]  := c["Fields"] union parent["Fields"];
<a name="line_815"></a>     for field in parent["Fields"] do
<a name="line_816"></a>      c["FieldDefault"][field] := eval(parent["FieldDefault"][field]);
<a name="line_817"></a>     od;
<a name="line_818"></a>
<a name="line_819"></a>     for field in indices(parent["FieldType"]) do
<a name="line_820"></a>      # The RHS below used to be wrapped in eval().  I am not sure if that would be better.
<a name="line_821"></a>      # It seems to cause trouble for inheritance when the type is 'table'.
<a name="line_822"></a>      c["FieldType"][op(field)] := parent["FieldType"][op(field)];
<a name="line_823"></a>     od;
<a name="line_824"></a>
<a name="line_825"></a>     c["IndirectFields"] := c["IndirectFields"] union
<a name="line_826"></a>       parent["IndirectFields"];
<a name="line_827"></a>
<a name="line_828"></a>     for indfield in parent["IndirectFields"] do
<a name="line_829"></a>      c["FieldIndirectionTable"][indfield] :=
<a name="line_830"></a>       eval(parent["FieldIndirectionTable"][indfield]);
<a name="line_831"></a>     od;
<a name="line_832"></a>
<a name="line_833"></a>     c["IndirectStaticFields"] := c["IndirectStaticFields"] union
<a name="line_834"></a>       parent["StaticFields"] union 
<a name="line_835"></a>       parent["IndirectStaticFields"];
<a name="line_836"></a>
<a name="line_837"></a>     for indfield in parent["IndirectStaticFields"] do
<a name="line_838"></a>      c["StaticFieldIndirectionTable"][indfield] :=
<a name="line_839"></a>       eval(parent["StaticFieldIndirectionTable"][indfield]);
<a name="line_840"></a>     od;
<a name="line_841"></a>
<a name="line_842"></a>     for indfield in parent["StaticFields"] do
<a name="line_843"></a>      c["StaticFieldIndirectionTable"][indfield] := x[2];
<a name="line_844"></a>     od;
<a name="line_845"></a>
<a name="line_846"></a>     c["Methods"] := c["Methods"] union parent["Methods"];
<a name="line_847"></a>     for methodname in parent["Methods"] do
<a name="line_848"></a>      fullmethodname :=
<a name="line_849"></a>       sprintf("%A!%A",classname,methodname);
<a name="line_850"></a>      `Package/SaveNames/Hidden` := 
<a name="line_851"></a>        [op(`Package/SaveNames/Hidden`),fullmethodname];
<a name="line_852"></a>      assign(convert(fullmethodname,name) =
<a name="line_853"></a>              eval(convert(cat(parent["Name"],"!",methodname),name)));
<a name="line_854"></a>      c["MethodSignature"][methodname] := parent["MethodSignature"][methodname];
<a name="line_855"></a>      c["MethodType"][methodname]      := parent["MethodType"][methodname];
<a name="line_856"></a>     od;
<a name="line_857"></a>
<a name="line_858"></a>     c["IndirectMethods"] := c["IndirectMethods"] union
<a name="line_859"></a>       parent["IndirectMethods"];
<a name="line_860"></a>
<a name="line_861"></a>     for indmethod in parent["IndirectMethods"] do
<a name="line_862"></a>      c["MethodIndirectionTable"][indmethod] :=
<a name="line_863"></a>       eval(parent["MethodIndirectionTable"][indmethod]);
<a name="line_864"></a>     od;
<a name="line_865"></a>
<a name="line_866"></a>    ##########################
<a name="line_867"></a>    elif (x[1] = "Documentation") then
<a name="line_868"></a>     classdoc := 
<a name="line_869"></a>      classdoc,
<a name="line_870"></a>       "<p>\n",convert_at(x[2]),"\n</p>\n";
<a name="line_871"></a>
<a name="line_872"></a>    ##########################
<a name="line_873"></a>    elif (x[1] = "Constructor") then
<a name="line_874"></a>     if nops(x) = 1 then
<a name="line_875"></a>      ERROR(
<a name="line_876"></a>       sprintf(
<a name="line_877"></a> "Empty constructor clause in declaration of class %a",
<a name="line_878"></a> classname));
<a name="line_879"></a>     fi;
<a name="line_880"></a>
<a name="line_881"></a>     if (`Package/ReportAssignment` = true) then
<a name="line_882"></a>      printf("    Constructor\n");
<a name="line_883"></a>     fi;
<a name="line_884"></a>
<a name="line_885"></a>     if not type(x[2],string) then
<a name="line_886"></a>      ERROR(
<a name="line_887"></a>       sprintf(
<a name="line_888"></a> "In declaration of class %s: constructor documentation is not a string.",
<a name="line_889"></a> classname));
<a name="line_890"></a>     fi;
<a name="line_891"></a>     consdoc := convert_at(x[2]);
<a name="line_892"></a>
<a name="line_893"></a>     consproc := eval(x[3]);
<a name="line_894"></a>     if not type(consproc,procedure) then
<a name="line_895"></a>      ERROR(
<a name="line_896"></a>       sprintf(
<a name="line_897"></a> "In declaration of class %s: constructor is not a procedure.",
<a name="line_898"></a> classname));
<a name="line_899"></a>     fi;
<a name="line_900"></a>
<a name="line_901"></a>     params := [op(1,eval(consproc))];
<a name="line_902"></a>     if params <> [] then params := params[2..-1]; fi;
<a name="line_903"></a>
<a name="line_904"></a>     consdoc := sprintf("<b>Constructor:</b> `new/%A`(",classname),
<a name="line_905"></a>  op(map((p) -> (`Package/ParamToHTML`(p),","),params)),
<a name="line_906"></a>  ")\n<p>\n",consdoc,"\n</p>\n";
<a name="line_907"></a>
<a name="line_908"></a>     c["Constructor"] := eval(consproc);
<a name="line_909"></a>
<a name="line_910"></a>     classdoc := classdoc,consdoc;
<a name="line_911"></a>
<a name="line_912"></a>    ##########################
<a name="line_913"></a>    elif (x[1] = "Field" or
<a name="line_914"></a>	  x[1] = "IncludedField" or
<a name="line_915"></a>	  x[1] = "StaticField") then
<a name="line_916"></a>     if nops(x) = 1 then
<a name="line_917"></a>      ERROR(
<a name="line_918"></a>       sprintf(
<a name="line_919"></a> "Empty field specification in declaration of class %a",
<a name="line_920"></a> classname));
<a name="line_921"></a>     fi;
<a name="line_922"></a>
<a name="line_923"></a>     typespecified := false;
<a name="line_924"></a>     fieldtype := anything;
<a name="line_925"></a>     defaultspecified := false;
<a name="line_926"></a>     fielddefault := NULL;
<a name="line_927"></a>     if type(x[2],equation) then
<a name="line_928"></a>      fieldname := op(1,x[2]);
<a name="line_929"></a>      fielddefault := op(2,x[2]);
<a name="line_930"></a>      defaultspecified := true;
<a name="line_931"></a>     else
<a name="line_932"></a>      fieldname := x[2];
<a name="line_933"></a>     fi;
<a name="line_934"></a>
<a name="line_935"></a>     if type(fieldname,`::`) then
<a name="line_936"></a>      fieldtype := op(2,fieldname);
<a name="line_937"></a>      typespecified := true;
<a name="line_938"></a>      fieldname := op(1,fieldname);
<a name="line_939"></a>      if fielddefault = NULL then
<a name="line_940"></a>       if assigned(`Class/DefaultValue`[fieldtype]) then
<a name="line_941"></a>        fielddefault := `Class/DefaultValue`[fieldtype];
<a name="line_942"></a>       elif type(fieldtype,name) then
<a name="line_943"></a>        realtype := eval(cat(`type/`,fieldtype));
<a name="line_944"></a>        if assigned(`Class/DefaultValue`[realtype]) then
<a name="line_945"></a>         fielddefault := `Class/DefaultValue`[realtype];
<a name="line_946"></a>        fi;
<a name="line_947"></a>       fi;
<a name="line_948"></a>      fi;
<a name="line_949"></a>     fi;
<a name="line_950"></a>
<a name="line_951"></a>     if not(type(fieldname,string)) then
<a name="line_952"></a>      ERROR(
<a name="line_953"></a>       sprintf("In declaration of class %a: field name %a is not a string.",
<a name="line_954"></a>        classname,fieldname));
<a name="line_955"></a>     fi;
<a name="line_956"></a>
<a name="line_957"></a>     if not(type(fieldtype,type)) then
<a name="line_958"></a>      ERROR(
<a name="line_959"></a>       sprintf("In declaration of class %a: type specification %a for field  %a is invalid.",
<a name="line_960"></a>        classname,fieldtype,fieldname));
<a name="line_961"></a>     fi;
<a name="line_962"></a>
<a name="line_963"></a>     if nops([fielddefault]) > 1 and fieldtype <> anything then
<a name="line_964"></a>      ERROR(
<a name="line_965"></a>       sprintf("In declaration of class %a: default value for field %a is an expression sequence",
<a name="line_966"></a>        classname,fieldname))
<a name="line_967"></a>     fi;
<a name="line_968"></a>
<a name="line_969"></a>     if fielddefault <> NULL and not(type(fielddefault,fieldtype)) then
<a name="line_970"></a>      ERROR(
<a name="line_971"></a>       sprintf("In declaration of class %a: default value %a for field %a does not have type %a",
<a name="line_972"></a>        classname,fielddefault,fieldname,fieldtype))
<a name="line_973"></a>     fi;
<a name="line_974"></a>
<a name="line_975"></a>     if (`Package/ReportAssignment` = true) then
<a name="line_976"></a>      printf("    %a: %a\n",x[1],fieldname);
<a name="line_977"></a>     fi;
<a name="line_978"></a>
<a name="line_979"></a>     if x[1] = "StaticField" then
<a name="line_980"></a>      c["StaticFields"] := {op(c["StaticFields"]),fieldname};
<a name="line_981"></a>      c["StaticFieldType"][fieldname]  := fieldtype;
<a name="line_982"></a>      c["StaticFieldValue"][fieldname] := eval(fielddefault);
<a name="line_983"></a>     else
<a name="line_984"></a>      c["Fields"] := {op(c["Fields"]),fieldname};
<a name="line_985"></a>      c["FieldType"][fieldname]    := fieldtype;
<a name="line_986"></a>      c["FieldDefault"][fieldname] := eval(fielddefault);
<a name="line_987"></a>     fi;
<a name="line_988"></a>
<a name="line_989"></a>     classdoc := 
<a name="line_990"></a>      classdoc,
<a name="line_991"></a>      sprintf("<b>%s: </b>",x[1]),
<a name="line_992"></a>      "<font color=\"red\">",fieldname,"</font>";
<a name="line_993"></a>
<a name="line_994"></a>     if typespecified then
<a name="line_995"></a>      classdoc := classdoc,sprintf("::#%a#",fieldtype);
<a name="line_996"></a>     fi;
<a name="line_997"></a>
<a name="line_998"></a>     if defaultspecified then
<a name="line_999"></a>      if fielddefault = NULL then
<a name="line_1000"></a>       classdoc := classdoc," = #NULL#";
<a name="line_1001"></a>      else
<a name="line_1002"></a>       classdoc := classdoc,sprintf(" = #%a#",fielddefault);
<a name="line_1003"></a>      fi;
<a name="line_1004"></a>     fi;
<a name="line_1005"></a>
<a name="line_1006"></a>     classdoc := classdoc,"<br/>\n";
<a name="line_1007"></a>
<a name="line_1008"></a>     if nops(x) > 2 then
<a name="line_1009"></a>      if type(x[3],string) then
<a name="line_1010"></a>       classdoc := classdoc, "<p>\n", convert_at(x[3]), "\n</p>\n";
<a name="line_1011"></a>      else
<a name="line_1012"></a>       ERROR(
<a name="line_1013"></a> sprintf(
<a name="line_1014"></a>  "In declaration of class %s: documentation for field %a is not a string: %a",
<a name="line_1015"></a>  classname,fieldname,x[3]));
<a name="line_1016"></a>      fi;
<a name="line_1017"></a>     fi;
<a name="line_1018"></a>
<a name="line_1019"></a>     if x[1] = "IncludedField" then
<a name="line_1020"></a>      if `Class/IsClass`(fieldtype) then
<a name="line_1021"></a>       incclass := eval(cat(`class/`,fieldtype));
<a name="line_1022"></a>       indfields := 
<a name="line_1023"></a>	incclass["Fields"] union
<a name="line_1024"></a>	incclass["StaticFields"] union
<a name="line_1025"></a>	incclass["IndirectFields"];
<a name="line_1026"></a>       c["IndirectFields"] := c["IndirectFields"] union indfields;
<a name="line_1027"></a>
<a name="line_1028"></a>       for indfield in indfields do
<a name="line_1029"></a>	c["FieldIndirectionTable"][indfield] := fieldname;
<a name="line_1030"></a>	c["FieldType"][indfield] := incclass["FieldType"][indfield];
<a name="line_1031"></a>       od;
<a name="line_1032"></a>
<a name="line_1033"></a>       indmethods := incclass["Methods"];
<a name="line_1034"></a>       c["IndirectMethods"] := c["IndirectMethods"] union indmethods;
<a name="line_1035"></a>       for indmethod in indmethods do
<a name="line_1036"></a>        c["MethodIndirectionTable"][indmethod] := fieldname;
<a name="line_1037"></a>       od;
<a name="line_1038"></a>      fi;
<a name="line_1039"></a>     fi;
<a name="line_1040"></a>
<a name="line_1041"></a>    ##########################
<a name="line_1042"></a>     elif x[1] = "Method" then
<a name="line_1043"></a>      if nops(x) = 1 then
<a name="line_1044"></a>       ERROR(
<a name="line_1045"></a> sprintf(
<a name="line_1046"></a>  "Empty method specification in declaration of class %a",
<a name="line_1047"></a>  classname));
<a name="line_1048"></a>      fi;
<a name="line_1049"></a>
<a name="line_1050"></a>      if type(x[2],`::`) then
<a name="line_1051"></a>       methodname := op(1,x[2]);
<a name="line_1052"></a>       methodtype := op(2,x[2]);
<a name="line_1053"></a>      else
<a name="line_1054"></a>       methodname := x[2];
<a name="line_1055"></a>       methodtype := anything;
<a name="line_1056"></a>      fi;
<a name="line_1057"></a>
<a name="line_1058"></a>      if not(type(methodname,string)) then
<a name="line_1059"></a>       ERROR(
<a name="line_1060"></a>        sprintf("In declaration of class %a: method name %a is not a string.",
<a name="line_1061"></a>                classname,methodname));
<a name="line_1062"></a>      fi;
<a name="line_1063"></a>
<a name="line_1064"></a>      c["Methods"] := {op(c["Methods"]),methodname};
<a name="line_1065"></a>      c["MethodType"][methodname] := methodtype;
<a name="line_1066"></a>
<a name="line_1067"></a>      if nops(x) > 3 then
<a name="line_1068"></a>       methodbody := x[4];
<a name="line_1069"></a>       if not(type(methodbody,procedure)) then
<a name="line_1070"></a> ERROR(
<a name="line_1071"></a>  sprintf(
<a name="line_1072"></a>   "In declaration of class %a: body of method %a is not a procedure",
<a name="line_1073"></a>   classname,methodname));
<a name="line_1074"></a>       fi;
<a name="line_1075"></a>       fullmethodname :=
<a name="line_1076"></a>        sprintf("%A!%A",classname,methodname);
<a name="line_1077"></a>       `Package/SaveNames/Hidden` := 
<a name="line_1078"></a>         [op(`Package/SaveNames/Hidden`),fullmethodname];
<a name="line_1079"></a>       assign(convert(fullmethodname,name) = eval(methodbody));
<a name="line_1080"></a>      else
<a name="line_1081"></a>       ERROR(
<a name="line_1082"></a> sprintf(
<a name="line_1083"></a>  "In declaration of class %a: method %a has no body.",
<a name="line_1084"></a>  classname,methodname));
<a name="line_1085"></a>      fi;
<a name="line_1086"></a>
<a name="line_1087"></a>      if (`Package/ReportAssignment` = true) then
<a name="line_1088"></a>       printf("    Method: %s\n",methodname);
<a name="line_1089"></a>      fi;
<a name="line_1090"></a>
<a name="line_1091"></a>      params := [op(1,eval(methodbody))];
<a name="line_1092"></a>      if params <> [] then params := params[2..-1]; fi;
<a name="line_1093"></a>      c["MethodSignature"][methodname] := params;
<a name="line_1094"></a>
<a name="line_1095"></a>      if params = [] then
<a name="line_1096"></a>       methoddoc := sprintf("<font color=\"maroon\">%s</font>()",methodname);
<a name="line_1097"></a>      else
<a name="line_1098"></a>       methoddoc := sprintf("<font color=\"maroon\">%s</font>(",methodname);
<a name="line_1099"></a>       methoddoc := 
<a name="line_1100"></a>         methoddoc, op(map((p) -> (`Package/ParamToHTML`(p),","),params));
<a name="line_1101"></a>       methoddoc := cat(methoddoc[1..-2],")");
<a name="line_1102"></a>      fi;
<a name="line_1103"></a>
<a name="line_1104"></a>      if type(x[2],`::`) then
<a name="line_1105"></a>       methoddoc := cat(methoddoc,"::",`Package/TypeToHTML`(methodtype));
<a name="line_1106"></a>      fi;
<a name="line_1107"></a>
<a name="line_1108"></a>      if type(x[3],string) then
<a name="line_1109"></a>       if x[3] <> "" then
<a name="line_1110"></a>        methoddoc := cat(methoddoc,"\n<p>\n",convert_at(x[3]),"\n</p>\n");
<a name="line_1111"></a>       fi;
<a name="line_1112"></a>      else
<a name="line_1113"></a>       ERROR(
<a name="line_1114"></a> sprintf(
<a name="line_1115"></a>  "In declaration of class %s: documentation for method %a is not a string: %a",
<a name="line_1116"></a>  classname,methodname,x[3]));
<a name="line_1117"></a>      fi; 
<a name="line_1118"></a>      classdoc := classdoc, "<b>Method:</b> ",methoddoc,"<br/>\n";
<a name="line_1119"></a>
<a name="line_1120"></a>    ##########################
<a name="line_1121"></a>    else
<a name="line_1122"></a>     ERROR(
<a name="line_1123"></a>      sprintf(
<a name="line_1124"></a>       "Invalid entry in declaration of class %a: %a",
<a name="line_1125"></a>       classname,x));
<a name="line_1126"></a>    fi;
<a name="line_1127"></a>   fi;
<a name="line_1128"></a>  od;
<a name="line_1129"></a>
<a name="line_1130"></a>  classdoc := classdoc,`Package/Doc/ClassFooter`;
<a name="line_1131"></a>  classdoc := cat(classdoc);
<a name="line_1132"></a>  classdoc := `Package/ConvertHash`(classdoc);
<a name="line_1133"></a>
<a name="line_1134"></a>  c["FullDoc"] := classdoc;
<a name="line_1135"></a>  c["Doc"] := `Package/ConvertHash`(c["Doc"]);
<a name="line_1136"></a>
<a name="line_1137"></a>  assign(cat(`class/`,classname) = eval(c));
<a name="line_1138"></a>
<a name="line_1139"></a>  indexfunction :=
<a name="line_1140"></a>   proc() `Class/IndexFunction`(CLASS,args) end; 
<a name="line_1141"></a>  indexfunction :=
<a name="line_1142"></a>   subs(CLASS = convert(cat("class/",classname),name),eval(indexfunction));
<a name="line_1143"></a>  assign(convert(cat("index/",classname),name) = eval(indexfunction));
<a name="line_1144"></a>
<a name="line_1145"></a>  # Convert from expression sequence to set
<a name="line_1146"></a>  parenttypes := {parenttypes};
<a name="line_1147"></a>
<a name="line_1148"></a>  typefunction :=
<a name="line_1149"></a>   proc(x) `Class/TypeFunction`(CLASS,x) end;
<a name="line_1150"></a>  typefunction :=
<a name="line_1151"></a>   subs(CLASS = convert(cat("class/",classname),name),eval(typefunction));
<a name="line_1152"></a>  assign(convert(cat("type/",classname),name) = eval(typefunction));
<a name="line_1153"></a>
<a name="line_1154"></a>  constructor := 
<a name="line_1155"></a>   proc() `Class/Constructor`(CLASS,args) end;
<a name="line_1156"></a>  constructor :=
<a name="line_1157"></a>   subs(CLASS = convert(cat("class/",classname),name),eval(constructor));
<a name="line_1158"></a>  assign(convert(cat("new/",classname),name) = eval(constructor));
<a name="line_1159"></a>  c["FullConstructor"] := eval(constructor);
<a name="line_1160"></a>
<a name="line_1161"></a>  if assigned(`Package/SaveClasses`) and
<a name="line_1162"></a>     type(`Package/SaveClasses`,list) then
<a name="line_1163"></a>   `Package/SaveClasses` :=
<a name="line_1164"></a>    [op(`Package/SaveClasses`),convert(classname,string)]
<a name="line_1165"></a>  fi;
<a name="line_1166"></a> end:
<a name="line_1167"></a>
<a name="line_1168"></a><span style="color:red">#@ `Class/ToString` 
</span><a name="line_1169"></a>`Class/ToString` := 
<a name="line_1170"></a> proc(x::anything,
<a name="line_1171"></a>      # optional
<a name="line_1172"></a>      maxdepth_::{integer,identical(infinity)},
<a name="line_1173"></a>      stringlen_::{integer,identical(infinity)})
<a name="line_1174"></a>  local maxdepth,stringlen;
<a name="line_1175"></a>
<a name="line_1176"></a>  if nargs = 0 then
<a name="line_1177"></a>   RETURN("NULL\n");
<a name="line_1178"></a>  fi;
<a name="line_1179"></a>
<a name="line_1180"></a>  maxdepth  := `if`(nargs > 1,args[2],infinity);
<a name="line_1181"></a>  stringlen := `if`(nargs > 2,args[3],maxdepth * 20);
<a name="line_1182"></a>
<a name="line_1183"></a>  `Class/ToString0`(x,0,maxdepth,stringlen);
<a name="line_1184"></a> end:
<a name="line_1185"></a>
<a name="line_1186"></a>`Class/ToString0` := 
<a name="line_1187"></a> proc(x,level,maxdepth,stringlen)
<a name="line_1188"></a>  `Class/ToString1`([x],level,maxdepth,stringlen);
<a name="line_1189"></a> end:
<a name="line_1190"></a>
<a name="line_1191"></a>`Class/ToString1` := 
<a name="line_1192"></a> proc(y::list,
<a name="line_1193"></a>      level::integer,
<a name="line_1194"></a>      maxdepth::{integer,identical(infinity)},
<a name="line_1195"></a>      stringlen::{integer,identical(infinity)})
<a name="line_1196"></a>  local prefix,s,f,x,xclass;
<a name="line_1197"></a>
<a name="line_1198"></a>  prefix   := cat(" "$level);
<a name="line_1199"></a>
<a name="line_1200"></a>  if level > maxdepth then 
<a name="line_1201"></a>   RETURN(cat(prefix,"...\n"));
<a name="line_1202"></a>  fi;
<a name="line_1203"></a>
<a name="line_1204"></a>  if nops(y) = 0 then
<a name="line_1205"></a>   RETURN(cat(prefix,"NULL\n"));
<a name="line_1206"></a>  elif nops(y) > 1 then
<a name="line_1207"></a>   RETURN(
<a name="line_1208"></a>    cat(prefix,"EXPRSEQ\n",
<a name="line_1209"></a> op(map(`Class/ToString0`,x,level + 1,maxdepth,stringlen))))
<a name="line_1210"></a>  fi;
<a name="line_1211"></a>
<a name="line_1212"></a>  x := y[1];
<a name="line_1213"></a>
<a name="line_1214"></a>  if type(x,list) then
<a name="line_1215"></a>   if level = maxdepth then
<a name="line_1216"></a>    RETURN(cat(prefix,"[...]\n"));
<a name="line_1217"></a>   else
<a name="line_1218"></a>    RETURN(
<a name="line_1219"></a>     cat(prefix,"[\n",
<a name="line_1220"></a>  op(map(`Class/ToString0`,x,level + 1,maxdepth,stringlen)),
<a name="line_1221"></a>  prefix,"]\n"));
<a name="line_1222"></a>   fi;
<a name="line_1223"></a>  elif type(x,set) then
<a name="line_1224"></a>   if level = maxdepth then
<a name="line_1225"></a>    RETURN(cat(prefix,"{...}\n"));
<a name="line_1226"></a>   else
<a name="line_1227"></a>    RETURN(
<a name="line_1228"></a>     cat(prefix,"{\n",
<a name="line_1229"></a>  op(map(`Class/ToString0`,x,level + 1,maxdepth,stringlen)),
<a name="line_1230"></a>  prefix,"}\n"));
<a name="line_1231"></a>   fi;
<a name="line_1232"></a>  elif `Class/IsObject`(x) then
<a name="line_1233"></a>   s := sprintf("%sINSTANCE(%A)\n",prefix,`Class/ClassNameOf`(x));
<a name="line_1234"></a>   if level = maxdepth then
<a name="line_1235"></a>    RETURN(s);
<a name="line_1236"></a>   else
<a name="line_1237"></a>    xclass := eval(`Class/ClassOf`(x));
<a name="line_1238"></a>    for f in xclass["Fields"] do
<a name="line_1239"></a>     s := sprintf("%s%s %A =\n%s",s,prefix,f,
<a name="line_1240"></a>    `Class/ToString1`([eval(x[f])],
<a name="line_1241"></a>                                    level + 2,maxdepth,stringlen));
<a name="line_1242"></a>    od;
<a name="line_1243"></a>    RETURN(s);
<a name="line_1244"></a>   fi;
<a name="line_1245"></a>  elif type(x,table) then
<a name="line_1246"></a>   s := cat(prefix,"TABLE\n");
<a name="line_1247"></a>   if level = maxdepth then
<a name="line_1248"></a>    RETURN(s);
<a name="line_1249"></a>   else
<a name="line_1250"></a>    for f in indices(x) do
<a name="line_1251"></a>     s := sprintf("%s%s %A =\n%s",s,prefix,f,
<a name="line_1252"></a>    `Class/ToString1`([eval(x[op(f)])],
<a name="line_1253"></a>                                    level + 2,maxdepth,stringlen));
<a name="line_1254"></a>    od;
<a name="line_1255"></a>    RETURN(s);
<a name="line_1256"></a>   fi;
<a name="line_1257"></a>  else
<a name="line_1258"></a>   s := sprintf("%a",eval(x));
<a name="line_1259"></a>   if length(s) > stringlen then
<a name="line_1260"></a>    s := cat(substring(s,1..stringlen),"...");
<a name="line_1261"></a>   fi;
<a name="line_1262"></a>   RETURN(cat(prefix,s,"\n"));
<a name="line_1263"></a>  fi;
<a name="line_1264"></a> end:
<a name="line_1265"></a>
<a name="line_1266"></a><span style="color:red">#@ `Class/Print`::'void' 
</span><a name="line_1267"></a>`Class/Print`::'void' := 
<a name="line_1268"></a> proc()
<a name="line_1269"></a>  printf("%s",`Class/ToString`(args));
<a name="line_1270"></a>  NULL;
<a name="line_1271"></a> end:
<a name="line_1272"></a>
<a name="line_1273"></a><span style="color:red">#@ `Class/ShowInfo` 
</span><a name="line_1274"></a>`Class/ShowInfo` := 
<a name="line_1275"></a> proc(classname::name)
<a name="line_1276"></a>  local c,f,m,ms,mt,s,t;
<a name="line_1277"></a>
<a name="line_1278"></a>  c := eval(cat(`class/`,classname));
<a name="line_1279"></a>  printf("Name:       %a\n",classname);
<a name="line_1280"></a>  printf("Package:    %s\n",c["Package"]);
<a name="line_1281"></a>
<a name="line_1282"></a>  if c["Fields"] <> {} then
<a name="line_1283"></a>   printf("Fields:\n");
<a name="line_1284"></a>   for f in sort([op(c["Fields"])]) do
<a name="line_1285"></a>    printf("  %a::%a\n",f,c["FieldType"][f]);
<a name="line_1286"></a>   od;
<a name="line_1287"></a>   printf("\n");
<a name="line_1288"></a>  fi;
<a name="line_1289"></a>
<a name="line_1290"></a>  if c["StaticFields"] <> {} then
<a name="line_1291"></a>   printf("Static fields:\n");
<a name="line_1292"></a>   for f in sort([op(c["StaticFields"])]) do
<a name="line_1293"></a>    printf("  %a = %a\n",f,c["StaticFieldValue"][f]);
<a name="line_1294"></a>   od;
<a name="line_1295"></a>   printf("\n");
<a name="line_1296"></a>  fi;
<a name="line_1297"></a>
<a name="line_1298"></a>  if c["IndirectFields"] <> {} then
<a name="line_1299"></a>   printf("Indirect fields:\n");
<a name="line_1300"></a>   for f in sort([op(c["IndirectFields"])]) do
<a name="line_1301"></a>    printf("  %a -> %a\n",f,c["FieldIndirectionTable"][f]);
<a name="line_1302"></a>   od;
<a name="line_1303"></a>   printf("\n");
<a name="line_1304"></a>  fi;
<a name="line_1305"></a>
<a name="line_1306"></a>  if c["Methods"] <> {} then
<a name="line_1307"></a>   mt := eval(c["MethodType"]);
<a name="line_1308"></a>   ms := eval(c["MethodSignature"]);
<a name="line_1309"></a>
<a name="line_1310"></a>   printf("Methods:\n");
<a name="line_1311"></a>   for m in sort([op(c["Methods"])]) do
<a name="line_1312"></a>    s := sprintf("%a",ms[m]);
<a name="line_1313"></a>    if length(s) > 1 then s := substring(s,2..-2); fi;
<a name="line_1314"></a>    t := mt[m];
<a name="line_1315"></a>    printf("  %a(%s)::%a\n",m,s,t);
<a name="line_1316"></a>   od;
<a name="line_1317"></a>   printf("\n");
<a name="line_1318"></a>  fi;
<a name="line_1319"></a>
<a name="line_1320"></a>  if c["IndirectMethods"] <> {} then
<a name="line_1321"></a>   printf("Indirect methods:\n");
<a name="line_1322"></a>   for m in sort([op(c["IndirectFields"])]) do
<a name="line_1323"></a>    printf("  %a -> %a\n",m,c["MethodIndirectionTable"][m]);
<a name="line_1324"></a>   od;
<a name="line_1325"></a>   printf("\n");
<a name="line_1326"></a>  fi;
<a name="line_1327"></a>
<a name="line_1328"></a>  if c["Parents"] <> {} then
<a name="line_1329"></a>   printf("Parents: %a\n\n",c["Parents"]);
<a name="line_1330"></a>  fi;
<a name="line_1331"></a> end:
<a name="line_1332"></a>
<a name="line_1333"></a><span style="color:red">#@ `Class/ShowMethod` 
</span><a name="line_1334"></a>`Class/ShowMethod` := 
<a name="line_1335"></a> proc(classname::name,methodname::name)
<a name="line_1336"></a>  print(eval(cat(classname,"!",methodname)));
<a name="line_1337"></a> end:
<a name="line_1338"></a>
<a name="line_1339"></a><span style="color:red">#@ `Class/ShowConstructor` 
</span><a name="line_1340"></a>`Class/ShowConstructor` := 
<a name="line_1341"></a> proc(classname::name)
<a name="line_1342"></a>  print(eval(eval(cat(`class/`,classname))["Constructor"]));
<a name="line_1343"></a> end:
<a name="line_1344"></a>
<a name="line_1345"></a><span style="color:red">#@ `Class/List` 
</span><a name="line_1346"></a>`Class/List` := 
<a name="line_1347"></a> proc()
<a name="line_1348"></a>  local classes;
<a name="line_1349"></a>  classes := map(convert,[anames()],string);
<a name="line_1350"></a>  classes := select(util_startswith,classes,"class/");
<a name="line_1351"></a>  classes := sort(map(substring,classes,7..-1));
<a name="line_1352"></a> end:
  </pre>
 </body>
</html>
    