<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># The function brent_fsolve uses Brent's algorithm to find a root of
<a name="line_2"></a># a real valued function of one variable.   This code is based on the
<a name="line_3"></a># Matlab implementation by John Burkardt, which is in turn based on the 
<a name="line_4"></a># original Fortran implementation by Richard Brent.  However, this
<a name="line_5"></a># version is designed for the case where we want to retain a lot
<a name="line_6"></a># of additional information that is generated in the process of 
<a name="line_7"></a># evaluating the relevant function.
<a name="line_8"></a>#
<a name="line_9"></a># The first argument F should be a function which accepts a single
<a name="line_10"></a># real argument t and returns a table F(t).  This should contain an 
<a name="line_11"></a># entry F(t)["err"], which we want to be zero.  The second and third
<a name="line_12"></a># arguments should be numbers a and b such that F(a)["err"] and 
<a name="line_13"></a># F(b)["err"] have opposite signs.  Other entries in F(t) can contain
<a name="line_14"></a># auxiliary information generated during the calculation.
<a name="line_15"></a>#
<a name="line_16"></a># If F(a) and/or F(b) have already been computed, then they can be 
<a name="line_17"></a># supplied as the arguments Fa_ and Fb_.  Otherwise, these arguments
<a name="line_18"></a># should be set to false.
<a name="line_19"></a>
<a name="line_20"></a><span style="color:red">#@ brent_fsolve 
</span><a name="line_21"></a>brent_fsolve := proc(F,a,b,Fa_,Fb_,tol_,epsilon_) 
<a name="line_22"></a> local tol,tol1,epsilon,c,sa,sb,sc,Fa,Fb,Fc,fa,fb,fc,d,e,m,p,q,r,s,steps,ret;
<a name="line_23"></a>
<a name="line_24"></a> tol := `if`(nargs > 5,tol_,10.^(-20));
<a name="line_25"></a> epsilon := `if`(nargs > 6,epsilon_,10.^(2 - Digits));
<a name="line_26"></a>
<a name="line_27"></a> sa := a;
<a name="line_28"></a> sb := b;
<a name="line_29"></a> if nargs < 4 or Fa_ = false then Fa := eval(F(sa)); else Fa := eval(Fa_); fi;
<a name="line_30"></a> if nargs < 5 or Fb_ = false then Fb := eval(F(sb)); else Fb := eval(Fb_); fi;
<a name="line_31"></a> fa := Fa["err"];
<a name="line_32"></a> fb := Fb["err"];
<a name="line_33"></a>
<a name="line_34"></a> if not((fa <= 0 and 0 <= fb) or 
<a name="line_35"></a>        (fb <= 0 and 0 <= fa)) then
<a name="line_36"></a>  error("function has the same sign at endpoints");
<a name="line_37"></a> fi;
<a name="line_38"></a>
<a name="line_39"></a> c := sa;
<a name="line_40"></a> fc := fa;
<a name="line_41"></a> Fc := eval(Fa);
<a name="line_42"></a> e := sb - sa;
<a name="line_43"></a> d := e;
<a name="line_44"></a>
<a name="line_45"></a> steps := 0;
<a name="line_46"></a> 
<a name="line_47"></a> while true do
<a name="line_48"></a>  steps := steps+1;
<a name="line_49"></a>  
<a name="line_50"></a>  if ( abs ( fc ) < abs ( fb ) ) then
<a name="line_51"></a>   sa := sb;
<a name="line_52"></a>   sb := c;
<a name="line_53"></a>   c := sa;
<a name="line_54"></a>   fa := fb;
<a name="line_55"></a>   fb := fc;
<a name="line_56"></a>   fc := fa;
<a name="line_57"></a>   Fa := eval(Fb);
<a name="line_58"></a>   Fb := eval(Fc);
<a name="line_59"></a>   Fc := eval(Fa);
<a name="line_60"></a>  fi;
<a name="line_61"></a>
<a name="line_62"></a>  tol1 := 2.0 * epsilon * abs ( sb ) + tol;
<a name="line_63"></a>  m := 0.5 * ( c - sb );
<a name="line_64"></a>
<a name="line_65"></a>  if ( abs ( m ) <= tol or fb = 0.0 ) then
<a name="line_66"></a>   break;
<a name="line_67"></a>  fi;
<a name="line_68"></a>
<a name="line_69"></a>  if ( abs ( e ) < tol or abs ( fa ) <= abs ( fb ) ) then
<a name="line_70"></a>   e := m;
<a name="line_71"></a>   d := e;
<a name="line_72"></a>  else
<a name="line_73"></a>   s := fb / fa;
<a name="line_74"></a>   if ( sa = c ) then
<a name="line_75"></a>    p := 2.0 * m * s;
<a name="line_76"></a>    q := 1.0 - s;
<a name="line_77"></a>   else
<a name="line_78"></a>    q := fa / fc;
<a name="line_79"></a>    r := fb / fc;
<a name="line_80"></a>    p := s * ( 2.0 * m * q * ( q - r ) - ( sb - sa ) * ( r - 1.0 ) );
<a name="line_81"></a>    q := ( q - 1.0 ) * ( r - 1.0 ) * ( s - 1.0 );
<a name="line_82"></a>   fi;
<a name="line_83"></a>
<a name="line_84"></a>   if ( 0.0 < p ) then
<a name="line_85"></a>    q := - q;
<a name="line_86"></a>   else
<a name="line_87"></a>    p := - p;
<a name="line_88"></a>   fi;
<a name="line_89"></a>
<a name="line_90"></a>   s := e;
<a name="line_91"></a>   e := d;
<a name="line_92"></a>
<a name="line_93"></a>   if ( 2.0 * p < 3.0 * m * q - abs ( tol * q ) and p < abs ( 0.5 * s * q ) ) then
<a name="line_94"></a>    d := p / q;
<a name="line_95"></a>   else
<a name="line_96"></a>    e := m;
<a name="line_97"></a>    d := e;
<a name="line_98"></a>   fi;
<a name="line_99"></a>  fi;
<a name="line_100"></a>
<a name="line_101"></a>  sa := sb;
<a name="line_102"></a>  fa := fb;
<a name="line_103"></a>  Fa := eval(Fb);
<a name="line_104"></a>
<a name="line_105"></a>  if ( tol < abs ( d ) ) then
<a name="line_106"></a>   sb := sb + d;
<a name="line_107"></a>  elif ( 0.0 < m ) then
<a name="line_108"></a>   sb := sb + tol;
<a name="line_109"></a>  else
<a name="line_110"></a>   sb := sb - tol;
<a name="line_111"></a>  fi;
<a name="line_112"></a>
<a name="line_113"></a>  Fb := eval(F(sb));
<a name="line_114"></a>  fb := Fb["err"];
<a name="line_115"></a>
<a name="line_116"></a>  if ( ( 0.0 < fb and 0.0 < fc ) or ( fb <= 0.0 and fc <= 0.0 ) ) then
<a name="line_117"></a>   c := sa;
<a name="line_118"></a>   fc := fa;
<a name="line_119"></a>   Fc := eval(Fa);
<a name="line_120"></a>   e := sb - sa;
<a name="line_121"></a>   d := e;
<a name="line_122"></a>  fi;
<a name="line_123"></a> od;
<a name="line_124"></a>
<a name="line_125"></a> ret := eval(Fb);
<a name="line_126"></a> ret["steps"] := steps;
<a name="line_127"></a> 
<a name="line_128"></a> return [sb,eval(ret)];
<a name="line_129"></a>end:
<a name="line_130"></a>
<a name="line_131"></a>
  </pre>
 </body>
</html>
    