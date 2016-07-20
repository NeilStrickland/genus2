<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a><a name="CLASS_automorphy_system"></a><span style="color:red">#@ CLASS: automorphy_system
</span><a name="line_2"></a>
<a name="line_3"></a>`Class/Declare`("automorphy_system",
<a name="line_4"></a> "An instance of this class encapsulates information about automorphic functions for $HX(a_H)$",
<a name="line_5"></a>
<a name="line_6"></a> ["Field","a_H"::RR1,"This is the relevant value of a_H.  WARNING: various things will work incorrectly if this field is different from the global variable a_H0.  This is poor design, but significant effort would be required to fix it."],
<a name="line_7"></a> ["Field","poly_deg"::posint = 100,"degree of approximating polynomials"],
<a name="line_8"></a> ["Field","band_number"::posint = 3,"controls the number of group elements to sum over"],
<a name="line_9"></a> ["Field","p_series"::table,"A power series approximation to the function $p_{jk}(z)=\\sum_{\\gamma}\\gamma(z)^j\\gamma'(z)^k$."],
<a name="line_10"></a> ["Field","a_star","This is the constant $a^*$, as defined in Section sec-hol-forms"],
<a name="line_11"></a> ["Field","m_series","This is the function $m(z)$, as defined in Section sec-hol-forms"],
<a name="line_12"></a>
<a name="line_13"></a> ["Method","num_group_elements","This returns the number of group elements that we sum over.  It increases rapidly with the band number.",
<a name="line_14"></a>  proc(this) nops(Pi_word_band(this["band_number"])); end
<a name="line_15"></a> ],
<a name="line_16"></a> 
<a name="line_17"></a> ["Method","p","This calculates individual values of the function $p_{jk}(z)$.",
<a name="line_18"></a>  proc(this,j,k,z::CC1)
<a name="line_19"></a>   local w,B,T,M,a,b;
<a name="line_20"></a>   
<a name="line_21"></a>   w := 0;
<a name="line_22"></a>   B := Pi_word_block(this["band_number"]);
<a name="line_23"></a>   for T in B do
<a name="line_24"></a>    M := act_Pi1_matrix(T);
<a name="line_25"></a>    a := (M[1,1]*z+M[1,2])/(M[2,1]*z+M[2,2]);
<a name="line_26"></a>    b := 1/(M[2,1]*z+M[2,2])^2;
<a name="line_27"></a>    w := w + a^j*b^k;
<a name="line_28"></a>   od;
<a name="line_29"></a>   return w;
<a name="line_30"></a>  end
<a name="line_31"></a> ],
<a name="line_32"></a>
<a name="line_33"></a> ["Method","set_p_series","This calculates a power series approximation to $p_{jk}(z)$",
<a name="line_34"></a>  proc(this,j,k)
<a name="line_35"></a>   local d,w,B,T,M,a,b,t,z;
<a name="line_36"></a>
<a name="line_37"></a>   d := this["poly_deg"];
<a name="line_38"></a>   w := 0;
<a name="line_39"></a>   B := Pi_word_block(this["band_number"]);
<a name="line_40"></a>   for T in B do
<a name="line_41"></a>    M := act_Pi1_matrix(T);
<a name="line_42"></a>    a := (M[1,1]*z+M[1,2])/(M[2,1]*z+M[2,2]);
<a name="line_43"></a>    b := 1/(M[2,1]*z+M[2,2])^2;
<a name="line_44"></a>    t := series(a^j*b^k,z=0,d);
<a name="line_45"></a>    w := w + convert(t,polynom,t);
<a name="line_46"></a>   od;
<a name="line_47"></a>
<a name="line_48"></a>   w := add(Re(coeff(w,z,i))*z^i,i=0..d);
<a name="line_49"></a>   
<a name="line_50"></a>   if not(type(this["p_series"],table)) then
<a name="line_51"></a>    this["p_series"] := table();
<a name="line_52"></a>   fi;
<a name="line_53"></a>   
<a name="line_54"></a>   this["p_series"][j,k] := unapply(w,z);
<a name="line_55"></a>   
<a name="line_56"></a>   return eval(this["p_series"][j,k]);
<a name="line_57"></a>  end
<a name="line_58"></a> ],
<a name="line_59"></a> 
<a name="line_60"></a> ["Method","set_p0_series","This calculates a power series approximation to $p_{0k}(z)$, with some optimizations for this particular case",
<a name="line_61"></a>  proc(this,k)
<a name="line_62"></a>   local d,w,B,bc,T,M,u,v,t,z;
<a name="line_63"></a>
<a name="line_64"></a>   d := floor(this["poly_deg"]/4);
<a name="line_65"></a>   w := 0;
<a name="line_66"></a>   B := Pi_word_block(this["band_number"]);
<a name="line_67"></a>   bc := [seq(binomial(2*k-1+4*l,4*l),l=0..d)];
<a name="line_68"></a>   
<a name="line_69"></a>   for T in B do
<a name="line_70"></a>    M := act_Pi1_matrix(T);
<a name="line_71"></a>    u := M[2,2]^(-2*k);
<a name="line_72"></a>    v := (M[2,1]/M[2,2])^4;
<a name="line_73"></a>    t := add(Re(bc[l+1]*u*v^l)*z^(4*l),l=0..d);
<a name="line_74"></a>    w := w + t;
<a name="line_75"></a>   od;
<a name="line_76"></a>
<a name="line_77"></a>   if not(type(this["p_series"],table)) then
<a name="line_78"></a>    this["p_series"] := table();
<a name="line_79"></a>   fi;
<a name="line_80"></a>   
<a name="line_81"></a>   this["p_series"][0,k] := unapply(w,z);
<a name="line_82"></a>   
<a name="line_83"></a>   return eval(this["p_series"][0,k]);
<a name="line_84"></a>  end
<a name="line_85"></a> ],
<a name="line_86"></a>
<a name="line_87"></a> ["Method","set_m_series","This calculates $a^*$ and $m(z)$",
<a name="line_88"></a>  proc(this)
<a name="line_89"></a>   local p,q,v0,v1,mz,mm,m;
<a name="line_90"></a>
<a name="line_91"></a>   p := eval(this["p_series"][0,2]);
<a name="line_92"></a>   v0 := evalf(subs(a_H = this["a_H"],v_H[0]));
<a name="line_93"></a>   v1 := evalf(subs(a_H = this["a_H"],v_H[1]));
<a name="line_94"></a>   mz := evalf(subs(a_H = this["a_H"],act_H[M](z)));
<a name="line_95"></a>   q := unapply(convert(series(p(mz) * diff(mz,z),z=0,this["poly_deg"]),polynom,z),z);
<a name="line_96"></a>   this["a_star"] := -(1-this["a_H"]^2)/4*p(v1)/p(v0);
<a name="line_97"></a>   mm := expand(p(z) - this["a_star"] * q(z));
<a name="line_98"></a>   mm := expand(mm / subs(z = 0,mm));
<a name="line_99"></a>   m := series(sqrt(mm),z=0,this["poly_deg"]);
<a name="line_100"></a>   m := convert(m,polynom,z);
<a name="line_101"></a>   m := add(Re(coeff(m,z,4*i))*z^(4*i),i=0..this["poly_deg"]/4);
<a name="line_102"></a>   this["m_series"] := unapply(m,z);
<a name="line_103"></a>   return eval(this["m_series"]);
<a name="line_104"></a>  end
<a name="line_105"></a> ]
<a name="line_106"></a>);
<a name="line_107"></a> 
  </pre>
 </body>
</html>
    