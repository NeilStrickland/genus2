<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># Many functions related to the hyperbolic family treat the parameter a_P
<a name="line_2"></a># as a symbol.  One might also want to work with a numeric value of a_P.
<a name="line_3"></a># For that, we can invoke set_a_P0(1/10) (for example).  This will set
<a name="line_4"></a># a_P0 to the exact rational value 1/10, and a_P1 to the approximate value
<a name="line_5"></a># 0.1.  It will also set a long list of other global variables.  Typically,
<a name="line_6"></a># there is an existing global variable, say foo, whose value involves
<a name="line_7"></a># a_P, and the function set_a_P0(1/10) will set foo0 and foo1 to the
<a name="line_8"></a># values obtained by substituting a_P = 1/10 or a_P = 0.1 in foo.
<a name="line_9"></a>
<a name="line_10"></a>set_a_P0 := proc(a)
<a name="line_11"></a> global a_P0,a_P1,A_P0,A_P1,r_P0,r_P1,P_rel0,P_rel1,
<a name="line_12"></a>  v_P0,v_P1,c_P0,c_P1,q_Ep0,q_Em0,is_in_Ep0_0,is_in_Em0_0,
<a name="line_13"></a>  P_to_Ep0_0,P_to_Em0_0,Ep0_0_e,Em0_0_e,Ep0_0_trans,Em0_0_trans,
<a name="line_14"></a>  Wg2p0,Wg3p0,WPp0,WPPp0,Wg2m0,Wg3m0,WPm0,WPPm0,
<a name="line_15"></a>  C_to_Ep0_0,C_to_Em0_0,c_Ep0_0,c_Em0_0,
<a name="line_16"></a>  latt_a0,latt_b0,latt_c0,latt_d0,latt_e0,latt_f0,
<a name="line_17"></a>  v_Ep0_0,v_Em0_0;
<a name="line_18"></a> local i,wz;
<a name="line_19"></a>
<a name="line_20"></a> a_P0 := a;                  <span style="color:red">#@ a_P0 
</span><a name="line_21"></a> a_P1 := evalf(a);           <span style="color:red">#@ a_P1 
</span><a name="line_22"></a> A_P0 := subs(a_P=a_P0,A_P); <span style="color:red">#@ A_P0 
</span><a name="line_23"></a> A_P1 := evalf(A_P0);        <span style="color:red">#@ A_P1 
</span><a name="line_24"></a>
<a name="line_25"></a> r_P0 := subs(a_P=a_P0,r_P); <span style="color:red">#@ r_P0 
</span><a name="line_26"></a> r_P1 := subs(a_P=a_P1,r_P); <span style="color:red">#@ r_P1 
</span><a name="line_27"></a>
<a name="line_28"></a> for i from 0 to 3 do 
<a name="line_29"></a>  P_rel0[i] := subs(a_P=a_P0,P_rel[i]); <span style="color:red">#@ P_rel0 
</span><a name="line_30"></a>  P_rel1[i] := subs(a_P=a_P1,P_rel[i]); <span style="color:red">#@ P_rel1 
</span><a name="line_31"></a> od;
<a name="line_32"></a>
<a name="line_33"></a> for i from 0 to 13 do
<a name="line_34"></a>  v_P0[i] := simplify(subs(a_P=a_P0,v_P[i])); <span style="color:red">#@ v_P0
</span><a name="line_35"></a>  v_P1[i] := evalf(v_P0[i]);                  <span style="color:red">#@ v_P1
</span><a name="line_36"></a> od;
<a name="line_37"></a>
<a name="line_38"></a> for i from 0 to 8 do
<a name="line_39"></a>  c_P0[i] := unapply(simplify(subs(a_P=a_P0,c_P[i](t))),t); <span style="color:red">#@ c_P0
</span><a name="line_40"></a>  c_P1[i] := unapply(evalf(c_P0[i](t)),t);                  <span style="color:red">#@ c_P1
</span><a name="line_41"></a>
<a name="line_42"></a>  c_Ep0_0[i] := unapply(simplify(subs(a_P=a_P0,c_Ep_0[i](t))),t); <span style="color:red">#@ c_Ep0_0
</span><a name="line_43"></a>  c_Em0_0[i] := unapply(simplify(subs(a_P=a_P0,c_Em_0[i](t))),t); <span style="color:red">#@ c_Em0_0
</span><a name="line_44"></a> od;
<a name="line_45"></a>
<a name="line_46"></a> q_Ep0 := unapply(eval(subs(a_P=a_P0,q_Ep(x))),x); <span style="color:red">#@ q_Ep0 
</span><a name="line_47"></a> q_Em0 := unapply(eval(subs(a_P=a_P0,q_Em(x))),x); <span style="color:red">#@ q_Em0 
</span><a name="line_48"></a>
<a name="line_49"></a> is_in_Ep0_0 := unapply(eval(subs(a_P=a_P0,is_in_Ep_0(YX))),YX); <span style="color:red">#@ is_in_Ep0_0 
</span><a name="line_50"></a> is_in_Em0_0 := unapply(eval(subs(a_P=a_P0,is_in_Em_0(YX))),YX); <span style="color:red">#@ is_in_Em0_0 
</span><a name="line_51"></a>
<a name="line_52"></a> P_to_Ep0_0 := unapply(eval(subs(a_P=a_P0,P_to_Ep_0([wz[1],wz[2]]))),wz); <span style="color:red">#@ P_to_Ep0_0 
</span><a name="line_53"></a> P_to_Em0_0 := unapply(eval(subs(a_P=a_P0,P_to_Em_0([wz[1],wz[2]]))),wz); <span style="color:red">#@ P_to_Em0_0 
</span><a name="line_54"></a>
<a name="line_55"></a> for i from 0 to 3 do 
<a name="line_56"></a>  Ep0_0_e[i] := subs(a_P=a_P0,Ep_0_e[i]); <span style="color:red">#@ Ep0_0_e[i] 
</span><a name="line_57"></a>  Em0_0_e[i] := subs(a_P=a_P0,Em_0_e[i]); <span style="color:red">#@ Em0_0_e[i] 
</span><a name="line_58"></a>
<a name="line_59"></a>  Ep0_0_trans[i] := unapply(eval(subs(a_P=a_P0,Ep_0_trans[i](YX))),YX); <span style="color:red">#@ Ep0_0_trans[i] 
</span><a name="line_60"></a>  Em0_0_trans[i] := unapply(eval(subs(a_P=a_P0,Em_0_trans[i](YX))),YX); <span style="color:red">#@ Em0_0_trans[i] 
</span><a name="line_61"></a> od:
<a name="line_62"></a>
<a name="line_63"></a> Wg2p0 := subs(a_P=a_P0,Wg2p);                         <span style="color:red">#@ Wg2p0
</span><a name="line_64"></a> Wg3p0 := subs(a_P=a_P0,Wg3p);                         <span style="color:red">#@ Wg3p0
</span><a name="line_65"></a> WPp0  := unapply(subs(a_P=a_P0,WPp(z)),z);            <span style="color:red">#@ WPp0
</span><a name="line_66"></a> WPPp0 := unapply(subs(a_P=a_P0,WPPp(z)),z);           <span style="color:red">#@ WPPp0
</span><a name="line_67"></a> C_to_Ep0_0 := unapply(subs(a_P=a_P0,C_to_Ep_0(z)),z); <span style="color:red">#@ C_to_Ep0_0
</span><a name="line_68"></a>
<a name="line_69"></a> Wg2m0 := subs(a_P=a_P0,Wg2m);                         <span style="color:red">#@ Wg2m0
</span><a name="line_70"></a> Wg3m0 := subs(a_P=a_P0,Wg3m);                         <span style="color:red">#@ Wg3m0
</span><a name="line_71"></a> WPm0  := unapply(subs(a_P=a_P0,WPm(z)),z);            <span style="color:red">#@ WPm0
</span><a name="line_72"></a> WPPm0 := unapply(subs(a_P=a_P0,WPPm(z)),z);           <span style="color:red">#@ WPPm0
</span><a name="line_73"></a> C_to_Em0_0 := unapply(subs(a_P=a_P0,C_to_Em_0(z)),z); <span style="color:red">#@ C_to_Em0_0
</span><a name="line_74"></a>
<a name="line_75"></a> # The code above should be correct, but is affected by a bug in Maple.
<a name="line_76"></a> # We therefore override it as follows:
<a name="line_77"></a> WPPm0 := proc(z)
<a name="line_78"></a>  local e,u0,u1,v0,v1;
<a name="line_79"></a>  u0 := WPm0(z);
<a name="line_80"></a>  e := 10.^(-50);
<a name="line_81"></a>  u1 := WPm0(z + e);
<a name="line_82"></a>  v0 := -sqrt(2.)*I*(u1 - u0)/e;
<a name="line_83"></a>  v1 := sqrt(4*u0^3-Wg2m0*u0-Wg3m0);
<a name="line_84"></a>  if Re(v1/v0) < 0 then
<a name="line_85"></a>   v1 := -v1;
<a name="line_86"></a>  fi;
<a name="line_87"></a>  return(v1);
<a name="line_88"></a> end:
<a name="line_89"></a>
<a name="line_90"></a> C_to_Em0_0 := (z) -> [I*WPPm0(z)/(WPm0(z)+1/3)^2/sqrt(2),1/(WPm0(z)+1/3)];
<a name="line_91"></a> # end of bugfix.
<a name="line_92"></a>
<a name="line_93"></a> latt_a0 := evalf(subs(a_P=a_P0,latt_a)); <span style="color:red">#@ latt_a0
</span><a name="line_94"></a> latt_b0 := evalf(subs(a_P=a_P0,latt_b)); <span style="color:red">#@ latt_b0
</span><a name="line_95"></a> latt_c0 := evalf(subs(a_P=a_P0,latt_c)); <span style="color:red">#@ latt_c0
</span><a name="line_96"></a> latt_d0 := evalf(subs(a_P=a_P0,latt_d)); <span style="color:red">#@ latt_d0
</span><a name="line_97"></a> latt_e0 := evalf(subs(a_P=a_P0,latt_e)); <span style="color:red">#@ latt_e0
</span><a name="line_98"></a> latt_f0 := evalf(subs(a_P=a_P0,latt_f)); <span style="color:red">#@ latt_f0
</span><a name="line_99"></a>
<a name="line_100"></a> for i from 0 to 15 do
<a name="line_101"></a>  v_Ep0_0[i] := subs(a_P=a_P0,v_Ep_0[i]); <span style="color:red">#@ v_Ep0_0
</span><a name="line_102"></a> od:
<a name="line_103"></a>
<a name="line_104"></a> for i from 0 to 13 do
<a name="line_105"></a>  v_Em0_0[i] := subs(a_P=a_P0,v_Em_0[i]); <span style="color:red">#@ v_Em0_0
</span><a name="line_106"></a> od:
<a name="line_107"></a>
<a name="line_108"></a> NULL;
<a name="line_109"></a>end:
<a name="line_110"></a>
<a name="line_111"></a>#set_a_P0(1/10);
<a name="line_112"></a>
<a name="line_113"></a>set_a_P0(0.09835622956839951251364823009734668690409027293496644792693847749752317746490473246479165607386090621);
<a name="line_114"></a>
  </pre>
 </body>
</html>
    