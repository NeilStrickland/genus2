#@ CLASS: automorphy_system

`Class/Declare`("automorphy_system",
 "An instance of this class encapsulates information about automorphic functions for $HX(a_H)$",

 ["Field","a_H"::RR1,"This is the relevant value of a_H.  WARNING: various things will work incorrectly if this field is different from the global variable a_H0.  This is poor design, but significant effort would be required to fix it."],
 ["Field","poly_deg"::posint = 100,"degree of approximating polynomials"],
 ["Field","band_number"::posint = 3,"controls the number of group elements to sum over"],
 ["Field","p_series"::table,"A power series approximation to the function $p_{jk}(z)=\\sum_{\\gamma}\\gamma(z)^j\\gamma'(z)^k$."],
 ["Field","a_star","This is the constant $a^*$, as defined in Section sec-hol-forms"],
 ["Field","m_series","This is the function $m(z)$, as defined in Section sec-hol-forms"],

 ["Method","num_group_elements","This returns the number of group elements that we sum over.  It increases rapidly with the band number.",
  proc(this) nops(Pi_word_band(this["band_number"])); end
 ],
 
 ["Method","p","This calculates individual values of the function $p_{jk}(z)$.",
  proc(this,j,k,z::CC1)
   local w,B,T,M,a,b;
   
   w := 0;
   B := Pi_word_block(this["band_number"]);
   for T in B do
    M := act_Pi1_matrix(T);
    a := (M[1,1]*z+M[1,2])/(M[2,1]*z+M[2,2]);
    b := 1/(M[2,1]*z+M[2,2])^2;
    w := w + a^j*b^k;
   od;
   return w;
  end
 ],

 ["Method","set_p_series","This calculates a power series approximation to $p_{jk}(z)$",
  proc(this,j,k)
   local d,w,B,T,M,a,b,t,z;

   d := this["poly_deg"];
   w := 0;
   B := Pi_word_block(this["band_number"]);
   for T in B do
    M := act_Pi1_matrix(T);
    a := (M[1,1]*z+M[1,2])/(M[2,1]*z+M[2,2]);
    b := 1/(M[2,1]*z+M[2,2])^2;
    t := series(a^j*b^k,z=0,d);
    w := w + convert(t,polynom,t);
   od;

   w := add(Re(coeff(w,z,i))*z^i,i=0..d);
   
   if not(type(this["p_series"],table)) then
    this["p_series"] := table();
   fi;
   
   this["p_series"][j,k] := unapply(w,z);
   
   return eval(this["p_series"][j,k]);
  end
 ],
 
 ["Method","set_p0_series","This calculates a power series approximation to $p_{0k}(z)$, with some optimizations for this particular case",
  proc(this,k)
   local d,w,B,bc,T,M,u,v,t,z;

   d := floor(this["poly_deg"]/4);
   w := 0;
   B := Pi_word_block(this["band_number"]);
   bc := [seq(binomial(2*k-1+4*l,4*l),l=0..d)];
   
   for T in B do
    M := act_Pi1_matrix(T);
    u := M[2,2]^(-2*k);
    v := (M[2,1]/M[2,2])^4;
    t := add(Re(bc[l+1]*u*v^l)*z^(4*l),l=0..d);
    w := w + t;
   od;

   if not(type(this["p_series"],table)) then
    this["p_series"] := table();
   fi;
   
   this["p_series"][0,k] := unapply(w,z);
   
   return eval(this["p_series"][0,k]);
  end
 ],

 ["Method","set_m_series","This calculates $a^*$ and $m(z)$",
  proc(this)
   local p,q,v0,v1,mz,mm,m;

   p := eval(this["p_series"][0,2]);
   v0 := evalf(subs(a_H = this["a_H"],v_H[0]));
   v1 := evalf(subs(a_H = this["a_H"],v_H[1]));
   mz := evalf(subs(a_H = this["a_H"],act_H[M](z)));
   q := unapply(convert(series(p(mz) * diff(mz,z),z=0,this["poly_deg"]),polynom,z),z);
   this["a_star"] := -(1-this["a_H"]^2)/4*p(v1)/p(v0);
   mm := expand(p(z) - this["a_star"] * q(z));
   mm := expand(mm / subs(z = 0,mm));
   m := series(sqrt(mm),z=0,this["poly_deg"]);
   m := convert(m,polynom,z);
   m := add(Re(coeff(m,z,4*i))*z^(4*i),i=0..this["poly_deg"]/4);
   this["m_series"] := unapply(m,z);
   return eval(this["m_series"]);
  end
 ]
);
 
