with(plots): 
with(plottools): 
with(LinearAlgebra): 
with(Groebner):
with(Optimization):
_EnvExplicit := true;
Digits := 100;

######################################################################
# This block of code defines the function _ASSERT(p,s).  The normal
# setup is that executing _ASSERT(p,s) continues silently if p 
# evaluates to true, but generates an error including the string s 
# if p is false.  If assert_verbosely is true then a message is
# generated even if the test is passed.  If assert_stop_on_fail is
# false then failed assertions still generate a message but the 
# maple error() command is not used, so execution will usually 
# continue.

kernelopts(assertlevel=1):
assert_count := 0;
assert_verbosely := false;
assert_stop_on_fail := true;

#@ _ASSERT 
_ASSERT := proc(p,s::string)
 global assert_count,assert_verbosely,assert_stop_on_fail,assert_fail_data;
 assert_count := assert_count+1;
 if assert_verbosely then printf("Checking: %s\n",s); fi;
 if not p then
  assert_fail_data := args[3..-1];
  if assert_stop_on_fail then
   error cat("Assertion failed: ",s);
  else
   printf(cat("Assertion failed: ",s,"\n"));
  fi;
 fi;
end:

######################################################################
# When this code is executed, Maple may consider the current directory
# to be the directory where all the worksheets are saved, or the
# directory where this text file is saved, or the common parent of
# those two directories.  The next block of code tries to determine
# which of these cases applies, and sets the global variables
# genus2_dir,maple_dir,worksheets_dir and latex_dir accordingly.

#@ find_directories 
find_directories := proc()
 local olddir;
 global genus2_dir,data_dir,maple_dir,maple_output_dir,
  worksheets_dir,latex_dir,plots_dir,thumbs_dir,images_dir,doc_dir,lib_dir;

 genus2_dir := NULL;
 
 if
  FileTools[Exists]("../maple") and
  FileTools[Exists]("../worksheets") and
  FileTools[IsDirectory]("../maple") and
  FileTools[IsDirectory]("../worksheets") then
   olddir := currentdir("..");
   genus2_dir := currentdir();
  currentdir(olddir);
 elif
  FileTools[Exists]("maple") and
  FileTools[Exists]("worksheets") and
  FileTools[IsDirectory]("maple") and
  FileTools[IsDirectory]("worksheets") then
   genus2_dir := currentdir();
 fi:

 if genus2_dir = NULL then
  error("genus2 directory not found");
 fi;

 data_dir         := cat(genus2_dir,"/data");
 maple_dir        := cat(genus2_dir,"/maple");
 maple_output_dir := cat(genus2_dir,"/maple_output");
 worksheets_dir   := cat(genus2_dir,"/worksheets");
 latex_dir        := cat(genus2_dir,"/latex");
 plots_dir        := cat(genus2_dir,"/plots");
 thumbs_dir       := cat(genus2_dir,"/plots/thumbs");
 images_dir       := cat(genus2_dir,"/images");
 doc_dir          := cat(genus2_dir,"/doc");
 lib_dir          := cat(genus2_dir,"/lib");
end:

find_directories();

######################################################################
# Simplify a complex number by setting real and imaginary parts
# to zero if they are smaller than some threshhold (10^(-12) by
# default).  This is useful because some calculations produce a
# tiny spurious imaginary part when the result should be real.

#@ trim 
trim := proc(t,e::numeric := 10^(-12))
 local v,x,y;
 if type(t,list) or type(t,set) then
  return(map(trim,t,e));
 fi;

 v := evalf(t);
 x := Re(v);
 y := Im(v);
 if (abs(x - round(x)) < e) then
  x := round(x);
 fi;
 if (abs(y - round(y)) < e) then
  y := round(y);
 fi;
 return(x + I * y);
end:

# This is a similar function that works on algebraic expressions.
# It should be consolidated with trim() at some point.

#@ tidy 
tidy := proc(u,e::numeric := 10.^(-90))
 local aa,a,c,u0;

 if type(u,`+`) or type(u,`*`) or type(u,list) or type(u,set) then
  return(map(tidy,u,e));
 elif type(u,`^`) then
  return(tidy(op(1,u),e)^op(2,u));
 elif type(u,float) then
  if abs(24*u - round(24*u)) < e then
   return round(24*u)/24;
  else
   return u;
  fi;
 elif type(u,complex) then
  return tidy(Re(u)) + I * tidy(Im(u));
 else
  return u;
 fi;
end:

#@ strip_sign 
strip_sign := proc(v)
 local a,b;
 if type(v,rational) then
  return abs(v);
 elif type(v,`*`) then 
  a,b := selectremove(type,v,rational);
  return(abs(a) * b);
 else 
  return v;
 fi;
end:

######################################################################

#@ is_inexact 
is_inexact := (x) -> hastype(x,float);

######################################################################
# For some reason, Maple does not always apply the obvious algebraic
# rules for complex conjugation.  The function below is designed to
# fix this.

#@ conj 
conj := proc(u)
 if type(u,`*`) or type(u,`+`) or type(u,list) then
  return map(conj,u);
 elif type(u,`^`) then 
  return conj(op(1,u))^op(2,u);
 else
  return(conjugate(u));
 fi;
end:

######################################################################
# This function should be called with some additional arguments
# (say t_1,..,t_r) as well as the named arguments f and n.  It 
# assumes that f depends on t_1,..,t_r and returns a polynomial in
# these variables of total degree less than n, which is an
# approximation to f.

#@ multi_series 
multi_series := proc(f,n::posint)
 local e,g,R;

 if type(f,list) or type(f,set) or type(f,Vector) or type(f,Matrix) then
  return map(multi_series,f,args[2..-1]);
 fi;
 
 R := [args[3..-1]];
 R := map(t -> (t = e*t),R);
 g := subs(R,f);
 g := series(g,e=0,n);
 g := expand(subs(e=1,convert(g,polynom,e)));
 return g;
end:

######################################################################
# revert_series(p,z,m) expects p to be a polynomial in z, with 
# p(0) = 0 and p'(0) <> 0.  It returns another polynomial q such that
# p(q(z)) = q(p(z)) = z mod z^(m+1).  If m is not supplied, it 
# defaults to the degree of p.
#
# Maple can do this as a special case of solve/series, but that 
# seems to be extremely slow.

#@ revert_series 
revert_series := proc(p,z,m_)
 local i,m,a,pp,q,qp;

 if nargs > 2 then
  m := m_;
 else
  m := degree(p,z);
 fi;

 if coeff(p,z,0) <> 0 then 
  error("constant term is nonzero");
 fi;

 a := coeff(p,z,1);

 if a = 0 then 
  error("linear term is zero");
 fi;

 q := z/a;
 pp := rem(p,z^(m+1),z);
 qp := expand(pp/a);

 for i from 2 to m do
  pp := rem(p * pp,z^(m+1),z);
  a := -coeff(qp,z,i)/coeff(pp,z,i);
  q := q + a*z^i;
  qp := expand(qp + a * pp);
 od;

 return q;
end:

######################################################################
# simp(x) returns x if x only involves exact numbers, but if x 
# contains any floating point terms then simp(x) attempts to convert
# everything to floating point.  Thus, if a is an unassigned symbol then
# have simp(a + Pi) = a + Pi but simp(a + Pi + 0.1) = a + 3.24159...
 
#@ simp 
simp := proc(x)
 if hastype(x,float) then
  return(evalf(x));
 else
  return(simplify(x));
 fi;
end:

######################################################################
# if a = \sum_{i\in A} 2^i and b = \sum_{i\in B} 2^i then
# bitwise_and(a,b) = \sum_{i\in A\cap B} 2^i.
#
# This is only included to allow us to write certain parts of the
# code in a way that translates easily into C (where bitwise and
# is a primitive operation and is often used to calculate 
# intersections of small sets).

#@ bitwise_and 
bitwise_and := proc(a,b)
 local aa,bb,c,x;

 aa := a;
 bb := b;
 c := 0;
 x := 1;

 while aa + bb > 0 do
  if type(aa,odd) and type(bb,odd) then c := c + x; fi;
  aa := iquo(aa,2);
  bb := iquo(bb,2);
  x := 2*x;
 od;

 return(c);
end:

######################################################################

# Find the n'th order Fourier series for a function of t
# that is periodic of period 2 Pi.

#@ fourier_series 
fourier_series := proc(u,n_,e_)
 local n,e;
 n := `if`(nargs > 1, n_, 10);
 e := `if`(nargs > 2, e_, 10^(-12));
 trim(Re(Int(u/(2*Pi),t=0..2*Pi))) + 
      add(Re(trim(Int(u*cos(k*t)/Pi,t=0..2*Pi),e))*cos(k*t),k=1..n) + 
      add(Re(trim(Int(u*sin(k*t)/Pi,t=0..2*Pi),e))*sin(k*t),k=1..n);
end:

######################################################################
# Given matrices M1 and M2 of the same height, this function returns 
# [v,w] where M1.w is close to M2.v and |v| = 1.  This is useful if we
# want to approximate an arbitrary function (usually of two of 
# variables) by a rational function, possibly with constraints on 
# the numerator and denominator.  In that context, M1 will be a matrix
# that sends a vector of coefficients to a vector of values at some
# sample points, and similarly for M2.

#@ quot_approx 
quot_approx := proc(M1,M2)
 local Q1,Q2,R1,R2,QQ,V,v,w,vv,ww;
 userinfo(5,genus2,"Finding QR decompositions"):
 Q1,R1 := QRDecomposition(M1):
 Q2,R2 := QRDecomposition(M2):
 QQ := Transpose(Q1).Q2:
 userinfo(5,genus2,"Finding SVD decompositions"):
 V := SingularValues(QQ,output=['Vt']):
 v := Column(Transpose(V),1):
 w := QQ.v:
 vv := (1/R2).v:
 ww := (1/R1).w:
 return([vv,ww]);
end:

######################################################################

#@ prime_factors 
prime_factors := proc(n::posint)
 local F;
 F := ifactor(n);
 if type(F,`*`) then F := {op(F)}; else F := {F}; fi;
 F := map(u -> `if`(type(u,`^`),op(1,u),u),F);
 F := map(op,F);
 return F;
end:

######################################################################

# This function can be used to process a chunk of LaTeX code 
# generated by Maple.  If we call process_latex(s) then the string s
# is combined with the contents of the file maple_output/maple_blank.tex
# (which provides headers and footers) to produce the file
# maple_output/maple_output.tex.  This is then processed to produce
# maple_output/maple_output.pdf.  

#@ process_latex 
process_latex := proc(s::string)
 local olddir,fd,header,footer,in_header,line,full_latex;
 global maple_dir;

 olddir := currentdir(maple_output_dir);

 fd := fopen("maple_blank.tex",READ);
 header := "";
 footer := "";
 in_header := true;
 line := readline(fd);
 while line <> 0 do
  if length(line) >= 15 and substring(line,1..15) = "%%% INSERT HERE" then
   in_header := false;
  elif in_header then
   header := cat(header,"\n",line);
  else
   footer := cat(footer,"\n",line);
  fi;
  line := readline(fd);
 od;
 fclose(fd);
 full_latex := cat(header,"\n",s,"\n",footer);
 fd := fopen("maple_output.tex",WRITE);
 fprintf(fd,"%s",full_latex);
 fclose(fd);
 ssystem("pdflatex maple_output");

 currentdir(olddir);
end:

######################################################################

util_stripbackquotes :=
proc(s::string)
 local t;

 t := s;
 if t <> "" and substring(t,1..1) = "`" then
  t := substring(t,2..-1);
 fi;
 if t <> "" and substring(t,-1..-1) = "`" then
  t := substring(t,1..-2);
 fi;

 RETURN(t);
end:

util_unixify :=
proc(s::string)
 convert(subs(13 = NULL,convert(s,bytes)),bytes);
end:

######################################################################

util_splitstring :=
proc(s::string,c::integer) 
 local b,n,t,i;

 b := convert(s,bytes):
 n := nops(b):
 t := select((i,x,d) -> x[i] = d,[seq(i,i=1..n)],b,c):
 t := [0,op(t),n+1]:
 seq(convert(b[(t[i]+1)..(t[i+1]-1)],bytes),i=1..nops(t)-1);
end:

util_commasplit := proc(s) util_splitstring(s, 44); end:
util_semisplit  := proc(s) util_splitstring(s, 59); end:
util_colonsplit := proc(s) util_splitstring(s, 58); end:
util_barsplit   := proc(s) util_splitstring(s,124); end:
util_tabsplit   := proc(s) util_splitstring(s,  9); end:

# This splits at forward slashes and backslashes, and ignores
# slashes at the end
util_slashsplit :=
proc(s::string)
 local b,n,t,i;

 b := subs(92 = 47,convert(s,bytes)):
 n := nops(b):
 while (n > 0 and b[n] = 47) do n := n - 1; od;
 if (n = 0) then RETURN(); fi;
 t := select((i,x) -> x[i] = 47,[seq(i,i=1..n)],b):
 t := [0,op(t),n+1]:
 seq(convert(b[(t[i]+1)..(t[i+1]-1)],bytes),i=1..nops(t)-1);

end:

util_joinstring :=
proc(c::string)
 local l;
 if nargs = 1 then RETURN(""); fi;
 l := map((t) -> op([sprintf("%A",t),c]),[args[2..-1]]);
 RETURN(cat(op(l[1..-2])));
end:

util_commajoin := proc() util_joinstring(",",args) end:
util_colonjoin := proc() util_joinstring(":",args) end:
util_barjoin   := proc() util_joinstring("|",args) end:
util_tabjoin   := proc() util_joinstring(",",args) end:
util_slashjoin := proc() util_joinstring("/",args) end:
util_linejoin  := proc() util_joinstring("\n",args) end:

######################################################################

util_startswith :=
proc(s::string,t::string)
 evalb(length(s) >= length(t) and substring(s,1..length(t)) = t);
end:

######################################################################

util_splitfilename :=
proc(file::string,
     # optional
     suffix_::string)
 local pieces,m,dirname,basename;

 pieces := [util_slashsplit(file)];
 if pieces = [] then
  ERROR("Empty file name");
 fi;

 dirname := util_slashjoin(op(pieces[1..-2]));
 basename := pieces[-1];

 if nargs > 1 then
  m := length(suffix_);
  if length(basename) >= m and
     substring(basename,(-m)..(-1)) = suffix_ then
   basename := substring(basename,1..(-m-1));
  fi;
 fi;

 RETURN([dirname,basename]);
end:

util_dirname :=
proc(file::string,
     # optional
     suffix_::string)
 util_splitfilename(args)[1];
end:

util_basename :=
proc(file::string,
     # optional
     suffix_::string)
 util_splitfilename(args)[2];
end:



######################################################################




