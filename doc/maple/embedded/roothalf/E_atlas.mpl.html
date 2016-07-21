<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a><a name="CLASS_E_chart"></a><span style="color:red">#@ CLASS: E_chart
</span><a name="line_2"></a>
<a name="line_3"></a>`Class/Declare`("E_chart",
<a name="line_4"></a> "An instance of this class represents an approximate conformal map $p\\colon\\mathbb{R}^2\\to EX^*$.  We refer to the point $p(0)$ as the centre of the chart.",
<a name="line_5"></a> ["Field","centre"::RR0_4,"A point in $EX^*$ which is the centre of the chart"],
<a name="line_6"></a> ["Field","square_centre"::RR0_2,"The image under @square_diffeo_E0()@ of the centre.  This is stored separately to allow for the case where we take square_centre to be an exact rational point, and then compute centre numerically from @square_centre@"],
<a name="line_7"></a> ["Field","n"::RR0_4,"The unit normal vector to $EX^*$ in $S^3$ at the centre"],
<a name="line_8"></a> ["Field","u"::RR0_4,"A unit tangent vector to $EX^*$ at the centre"],
<a name="line_9"></a> ["Field","v"::RR0_4,"Another unit tangent vector to $EX^*$ at the centre"],
<a name="line_10"></a> ["Field","p","This is a polynomial function $\\mathbb{R}^2\\to\\mathbb{R}^4$ which sends $0$ to the centre point, and is approximately a conformal map to $EX^*$."],
<a name="line_11"></a> ["Field","p0","The same as $p$, but with coefficients evaluated numerically."],
<a name="line_12"></a> ["Field","p_inv_approx","A linear map $\\mathbb{R}^4\\to\\mathbb{R}^2$ which is approximately inverse to $p$ near the centre"],
<a name="line_13"></a> ["Field","degree"::posint,"The degree of the polynomials in $p$."],
<a name="line_14"></a> ["Field","coeffs_exact"::boolean = false,"true if the polynomial coefficients in $p$ are exact."],
<a name="line_15"></a> ["Field","vertex_index"::integer = NULL,"If the centre lies at $v_k$, this field should be set to $k$."],
<a name="line_16"></a> ["Field","curve_index"::integer = NULL,"If the centre lies on the curve $C_k$, this field should be set to $k$."],
<a name="line_17"></a> ["Field","curve_parameter"::RR0 = NULL,"If the centre is $c_{Ek}(t)$, then this field should be set to $t$."],
<a name="line_18"></a> ["Field","grid_index"::integer = NULL],
<a name="line_19"></a>
<a name="line_20"></a> ["Method","vertex_set_exact","Set @vertex_index@ to @k@, and calculate various associated data, using exact coefficients where appropriate.  This gives a chart of polynomial degree one.",
<a name="line_21"></a>  proc(this,k)
<a name="line_22"></a>   this["vertex_index"] := k;
<a name="line_23"></a>
<a name="line_24"></a>   if k = 0 then
<a name="line_25"></a>    this["curve_set_exact",1,0];
<a name="line_26"></a>   elif k = 3 then
<a name="line_27"></a>    this["curve_set_exact",0,Pi/2];
<a name="line_28"></a>   elif k = 6 then
<a name="line_29"></a>    this["curve_set_exact",1,Pi/2];
<a name="line_30"></a>   elif k = 11 then
<a name="line_31"></a>    this["curve_set_exact",3,0];
<a name="line_32"></a>   else
<a name="line_33"></a>    this["vertex_index"] := NULL;
<a name="line_34"></a>    error("Invalid vertex index");
<a name="line_35"></a>   fi;
<a name="line_36"></a>  end
<a name="line_37"></a> ],
<a name="line_38"></a>
<a name="line_39"></a> ["Method","vertex_set_numeric","Set @vertex_index@ to @k@, and calculate various associated data, using numerical coefficients.  This gives a chart of polynomial degree one.",
<a name="line_40"></a>  proc(this,k)
<a name="line_41"></a>   this["vertex_index"] := k;
<a name="line_42"></a>
<a name="line_43"></a>   if k = 0 then
<a name="line_44"></a>    this["curve_set_numeric",1,0];
<a name="line_45"></a>   elif k = 3 then
<a name="line_46"></a>    this["curve_set_numeric",0,Pi/2];
<a name="line_47"></a>   elif k = 6 then
<a name="line_48"></a>    this["curve_set_numeric",1,Pi/2];
<a name="line_49"></a>   elif k = 11 then
<a name="line_50"></a>    this["curve_set_numeric",3,0];
<a name="line_51"></a>   else
<a name="line_52"></a>    this["vertex_index"] := NULL;
<a name="line_53"></a>    error("Invalid vertex index");
<a name="line_54"></a>   fi;
<a name="line_55"></a>  end
<a name="line_56"></a> ],
<a name="line_57"></a>
<a name="line_58"></a> ["Method","curve_set_exact","Set @curve_index@ to @k0@ and @curve_parameter@ to @t0@, and calculate various associated data, using exact coefficients where appropriate.  This gives a chart of polynomial degree one.  For charts of this type, we will ensure that $p(t,0)$ is the Taylor approximation to $c_{k0}(t0+t)$.",
<a name="line_59"></a>  proc(this,k0::integer,t0::RR0)
<a name="line_60"></a>   local trig_rels,aa,bb,cc,nn,p0,x;
<a name="line_61"></a>
<a name="line_62"></a>   trig_rels := {
<a name="line_63"></a>    sin(Pi/8)=sqrt(2-sqrt(2))/2,
<a name="line_64"></a>    cos(Pi/8)=sqrt(2+sqrt(2))/2,
<a name="line_65"></a>    sin(3*Pi/8)=sqrt(2+sqrt(2))/2,
<a name="line_66"></a>    cos(3*Pi/8)=sqrt(2-sqrt(2))/2
<a name="line_67"></a>   };
<a name="line_68"></a>
<a name="line_69"></a>   this["curve_index"] := k0;
<a name="line_70"></a>   this["curve_parameter"] := t0;
<a name="line_71"></a>   this["degree"] := 1;
<a name="line_72"></a>   this["coeffs_exact"] := true;
<a name="line_73"></a>
<a name="line_74"></a>   aa := simplify(expand(rationalize(subs(trig_rels,c_E0[k0](t0)))));
<a name="line_75"></a>   aa := simplify(expand(combine(rationalize(aa))));
<a name="line_76"></a>
<a name="line_77"></a>   this["centre"] := aa;
<a name="line_78"></a>   this["square_centre"] := simplify(expand(combine(rationalize(square_diffeo_E0(aa)))));
<a name="line_79"></a>
<a name="line_80"></a>   nn := simplify(expand(combine(rationalize(dg0(aa)))));
<a name="line_81"></a>   nn := simplify(expand(combine(rationalize(nn /~ nm4(nn)))));
<a name="line_82"></a>   this["n"] := nn;
<a name="line_83"></a>
<a name="line_84"></a>   bb := simplify(expand(rationalize(subs(trig_rels,subs(t=t0,map(diff,c_E0[k0](t),t))))));
<a name="line_85"></a>   bb := simplify(expand(combine(rationalize(bb))));
<a name="line_86"></a>   cc := simplify(expand(rationalize(subs(trig_rels,subs(a_E=a_E0,conformal_twist(aa,bb))))));
<a name="line_87"></a>   cc := simplify(expand(combine(rationalize(cc))));
<a name="line_88"></a>
<a name="line_89"></a>   this["u"] := simplify(expand(combine(rationalize(bb /~ nm4(bb)))));
<a name="line_90"></a>   this["v"] := simplify(expand(combine(rationalize(cc /~ nm4(cc)))));
<a name="line_91"></a>
<a name="line_92"></a>   p0 := expand(aa +~ t *~ bb +~ u *~ cc);
<a name="line_93"></a>   p0 := simplify(expand(combine(p0)));
<a name="line_94"></a>   p0 := subs({t=s[1],u=s[2]},p0);
<a name="line_95"></a>   this["p"] := unapply(p0,s);
<a name="line_96"></a>   this["p0"] := unapply(evalf(p0),s);
<a name="line_97"></a>   this["p_inv_approx"] := 
<a name="line_98"></a>    unapply(evalf(
<a name="line_99"></a>     [add(x[i]*bb[i],i=1..4)/add(bb[i]^2,i=1..4),
<a name="line_100"></a>      add(x[i]*cc[i],i=1..4)/add(cc[i]^2,i=1..4)]),x);
<a name="line_101"></a>
<a name="line_102"></a>   NULL;
<a name="line_103"></a>  end
<a name="line_104"></a> ],
<a name="line_105"></a>
<a name="line_106"></a> ["Method","curve_improve_exact","Increase the polynomial degree of this chart by one.",
<a name="line_107"></a>  proc(this)
<a name="line_108"></a>   local trig_rels,m,t0,d,p0,p0t,p0u,a0,a1,a2,a3,aa0,aa1,aa2,aa3,
<a name="line_109"></a>    err0,err1,err2,err3,err4,d0,d1,d2,d3,d2c,d3c,eqs,sol,p1,p2,c,i,j,s,t,u,e;
<a name="line_110"></a>   
<a name="line_111"></a>   trig_rels := {
<a name="line_112"></a>    sin(Pi/8)=sqrt(2-sqrt(2))/2,
<a name="line_113"></a>    cos(Pi/8)=sqrt(2+sqrt(2))/2,
<a name="line_114"></a>    sin(3*Pi/8)=sqrt(2+sqrt(2))/2,
<a name="line_115"></a>    cos(3*Pi/8)=sqrt(2-sqrt(2))/2
<a name="line_116"></a>   };
<a name="line_117"></a>
<a name="line_118"></a>   m := this["curve_index"];
<a name="line_119"></a>   t0 := this["curve_parameter"];
<a name="line_120"></a>   p0 := unapply(eval(this["p"]([t,u])),t,u);
<a name="line_121"></a>   p0 := unapply(simplify(expand(rationalize(subs(trig_rels,p0(t,u))))),t,u);
<a name="line_122"></a>   d := this["degree"]+1;
<a name="line_123"></a>   p0t := unapply(map(diff,p0(t,u),t),t,u);
<a name="line_124"></a>   p0u := unapply(map(diff,p0(t,u),u),t,u);
<a name="line_125"></a>   a0 := p0(0,0);
<a name="line_126"></a>   a1 := simplify(expand(rationalize(subs(trig_rels,dg0(a0)))));
<a name="line_127"></a>   a2 := map(coeff,map(coeff,p0(t,u),t,1),u,0);
<a name="line_128"></a>   a3 := map(coeff,map(coeff,p0(t,u),t,0),u,1);
<a name="line_129"></a>   aa0 := 1;
<a name="line_130"></a>   aa1 := simplify(expand(dp4(a1,a1)));
<a name="line_131"></a>   aa2 := simplify(expand(dp4(a2,a2)));
<a name="line_132"></a>   aa3 := aa2;
<a name="line_133"></a>   err0 := coeff(rem(rho(p0(e*t,e*u))-1,e^(d+1),e),e,d);
<a name="line_134"></a>   err1 := coeff(rem(g0(p0(e*t,e*u)),e^(d+1),e),e,d);
<a name="line_135"></a>   err2 := map(coeff,simplify(expand(rationalize(subs(trig_rels,convert(map(series,p0(e*t,0) -~ c_E0[m](e*t+t0),e=0,d+1),polynom,e))))),e,d);
<a name="line_136"></a>   err3 := coeff(rem(dp4(p0t(e*t,e*u),p0u(e*t,e*u)),e^d,e),e,d-1);
<a name="line_137"></a>   err4 := coeff(rem(dp4(p0t(e*t,e*u),p0t(e*t,e*u))-dp4(p0u(e*t,e*u),p0u(e*t,e*u)),e^d,e),e,d-1);
<a name="line_138"></a>   d0 := expand(-err0/2);
<a name="line_139"></a>   d1 := expand(-err1/dp4(a1,a1));
<a name="line_140"></a>   d2 := add(d2c[i]*t^i*u^(d-i),i=0..d);
<a name="line_141"></a>   d3 := add(d3c[i]*t^i*u^(d-i),i=0..d);
<a name="line_142"></a>   eqs := [coeffs(expand(err3/aa2 + diff(d2,u) + diff(d3,t)),{t,u}),
<a name="line_143"></a>	   coeffs(expand(err4/aa2 + 2*diff(d2,t) - 2*diff(d3,u)),{t,u}),
<a name="line_144"></a>	   d2c[d] + coeff(dp4(err2,a2)/aa2,t,d),
<a name="line_145"></a>	   d3c[d] + coeff(dp4(err2,a3)/aa3,t,d)];
<a name="line_146"></a>   sol := solve(eqs);
<a name="line_147"></a>   d2 := subs(sol,d2);
<a name="line_148"></a>   d3 := subs(sol,d3);
<a name="line_149"></a>   p1 := simplify(expand(rationalize(d0 *~ a0 +~ d1 *~ a1 +~ d2 *~ a2 +~ d3 *~ a3)));
<a name="line_150"></a>   p1 := simplify(expand(combine(p1)));
<a name="line_151"></a>   p2 := p0(t,u) +~ [p1[1],p1[2],p1[3],p1[4]];
<a name="line_152"></a>   p2 := subs({t=s[1],u=s[2]},p2);
<a name="line_153"></a>
<a name="line_154"></a>   this["p"] := unapply(p2,s);
<a name="line_155"></a>   this["p0"] := unapply(evalf(p2),s);
<a name="line_156"></a>   this["degree"] := d;
<a name="line_157"></a>   NULL;
<a name="line_158"></a>  end
<a name="line_159"></a> ],
<a name="line_160"></a>
<a name="line_161"></a> ["Method","curve_set_degree_exact","Set the degree of this chart to @d@, and calculate the appropriate coefficients.",
<a name="line_162"></a>  proc(this,d,force_)
<a name="line_163"></a>   local p,s;
<a name="line_164"></a>
<a name="line_165"></a>   while this["degree"] < d do this["curve_improve_exact"]; od;
<a name="line_166"></a>   if this["degree"] > d and nargs > 2 and force_ then
<a name="line_167"></a>    p := this["p"](s);
<a name="line_168"></a>    p := multi_series(p,d+1,s[1],s[2]);
<a name="line_169"></a>    this["p"] := unapply(p,s);
<a name="line_170"></a>    this["p0"] := unapply(evalf(p),s);
<a name="line_171"></a>    this["degree"] := d;
<a name="line_172"></a>   fi;
<a name="line_173"></a>
<a name="line_174"></a>   NULL;
<a name="line_175"></a>  end
<a name="line_176"></a> ],
<a name="line_177"></a>
<a name="line_178"></a> ["Method","curve_set_numeric","Set @curve_index@ to @k0@ and @curve_parameter@ to @t0@, and calculate various associated data, using numerical coefficients.  This gives a chart of polynomial degree one.  For charts of this type, we will ensure that $p(t,0)$ is the Taylor approximation to $c_{k0}(t0+t)$.",
<a name="line_179"></a>  proc(this,k0,t0)
<a name="line_180"></a>   local nn,aa,bb,cc,p0;
<a name="line_181"></a>
<a name="line_182"></a>   this["curve_index"] := k0;
<a name="line_183"></a>   this["curve_parameter"] := evalf(t0);
<a name="line_184"></a>   this["degree"] := 1;
<a name="line_185"></a>   this["coeffs_exact"] := false;
<a name="line_186"></a>
<a name="line_187"></a>   aa := evalf(c_E0[k0](t0));
<a name="line_188"></a>
<a name="line_189"></a>   this["centre"] := aa;
<a name="line_190"></a>   this["square_centre"] := evalf(square_diffeo_E0(aa));
<a name="line_191"></a>
<a name="line_192"></a>   nn := evalf(dg0(aa));
<a name="line_193"></a>   nn := nn /~ nm4(nn);
<a name="line_194"></a>   this["n"] := nn;
<a name="line_195"></a>
<a name="line_196"></a>   bb := evalf(subs(t=t0,map(diff,c_E0[k0](t),t)));
<a name="line_197"></a>   cc := evalf(subs(a_E=a_E0,conformal_twist(aa,bb)));
<a name="line_198"></a>
<a name="line_199"></a>   this["u"] := bb /~ nm4(bb);
<a name="line_200"></a>   this["v"] := cc /~ nm4(cc);
<a name="line_201"></a>
<a name="line_202"></a>   p0 := expand(aa +~ t *~ bb +~ u *~ cc);
<a name="line_203"></a>   p0 := subs({t=s[1],u=s[2]},p0);
<a name="line_204"></a>   this["p"] := unapply(p0,s);
<a name="line_205"></a>   this["p0"] := unapply(p0,s);
<a name="line_206"></a>
<a name="line_207"></a>   this["p_inv_approx"] := 
<a name="line_208"></a>    unapply(evalf(
<a name="line_209"></a>     [add(x[i]*bb[i],i=1..4)/add(bb[i]^2,i=1..4),
<a name="line_210"></a>      add(x[i]*cc[i],i=1..4)/add(cc[i]^2,i=1..4)]),x);
<a name="line_211"></a>
<a name="line_212"></a>   NULL;
<a name="line_213"></a>  end
<a name="line_214"></a> ],
<a name="line_215"></a>
<a name="line_216"></a> ["Method","curve_improve_numeric","Increase the polynomial degree of this chart by one.",
<a name="line_217"></a>  proc(this)
<a name="line_218"></a>   local m,t0,d,p0,p0t,p0u,a0,a1,a2,a3,aa0,aa1,aa2,aa3,err0,err1,err2,err3,err4,d0,d1,d2,d3,d2c,d3c,eqs,sol,p1,p2,c,i,j,s,t,u,e;
<a name="line_219"></a>   
<a name="line_220"></a>   m := this["curve_index"];
<a name="line_221"></a>   t0 := this["curve_parameter"];
<a name="line_222"></a>   p0 := unapply(this["p"]([t,u]),t,u);
<a name="line_223"></a>   d := this["degree"]+1;
<a name="line_224"></a>   p0t := unapply(map(diff,p0(t,u),t),t,u);
<a name="line_225"></a>   p0u := unapply(map(diff,p0(t,u),u),t,u);
<a name="line_226"></a>   a0 := evalf(p0(0,0));
<a name="line_227"></a>   a1 := evalf(dg0(a0));
<a name="line_228"></a>   a2 := map(coeff,map(coeff,p0(t,u),t,1),u,0);
<a name="line_229"></a>   a3 := map(coeff,map(coeff,p0(t,u),t,0),u,1);
<a name="line_230"></a>   aa0 := 1;
<a name="line_231"></a>   aa1 := simplify(expand(dp4(a1,a1)));
<a name="line_232"></a>   aa2 := simplify(expand(dp4(a2,a2)));
<a name="line_233"></a>   aa3 := aa2;
<a name="line_234"></a>   err0 := coeff(rem(rho(p0(e*t,e*u))-1,e^(d+1),e),e,d);
<a name="line_235"></a>   err1 := coeff(rem(evalf(g0(p0(e*t,e*u))),e^(d+1),e),e,d);
<a name="line_236"></a>   err2 := map(coeff,evalf(convert(map(series,p0(e*t,0) -~ c_E0[m](e*t+t0),e=0,d+1),polynom,e)),e,d);
<a name="line_237"></a>   err3 := coeff(rem(dp4(p0t(e*t,e*u),p0u(e*t,e*u)),e^d,e),e,d-1);
<a name="line_238"></a>   err4 := coeff(rem(dp4(p0t(e*t,e*u),p0t(e*t,e*u))-dp4(p0u(e*t,e*u),p0u(e*t,e*u)),e^d,e),e,d-1);
<a name="line_239"></a>   d0 := expand(-err0/2);
<a name="line_240"></a>   d1 := expand(-err1/dp4(a1,a1));
<a name="line_241"></a>   d2 := add(d2c[i]*t^i*u^(d-i),i=0..d);
<a name="line_242"></a>   d3 := add(d3c[i]*t^i*u^(d-i),i=0..d);
<a name="line_243"></a>   eqs := [coeffs(expand(err3/aa2 + diff(d2,u) + diff(d3,t)),{t,u}),
<a name="line_244"></a>	   coeffs(expand(err4/aa2 + 2*diff(d2,t) - 2*diff(d3,u)),{t,u}),
<a name="line_245"></a>	   d2c[d] + coeff(dp4(err2,a2)/aa2,t,d),
<a name="line_246"></a>	   d3c[d] + coeff(dp4(err2,a3)/aa3,t,d)];
<a name="line_247"></a>   sol := solve(eqs);
<a name="line_248"></a>   d2 := subs(sol,d2);
<a name="line_249"></a>   d3 := subs(sol,d3);
<a name="line_250"></a>   p1 := expand(d0 *~ a0 +~ d1 *~ a1 +~ d2 *~ a2 +~ d3 *~ a3);
<a name="line_251"></a>   p2 := p0(t,u) +~ [p1[1],p1[2],p1[3],p1[4]];
<a name="line_252"></a>   p2 := subs({t=s[1],u=s[2]},p2);
<a name="line_253"></a>
<a name="line_254"></a>   this["p"] := unapply(p2,s);
<a name="line_255"></a>   this["p0"] := unapply(p2,s);
<a name="line_256"></a>   this["degree"] := d;
<a name="line_257"></a>   NULL;
<a name="line_258"></a>  end
<a name="line_259"></a> ],
<a name="line_260"></a>
<a name="line_261"></a> ["Method","curve_set_degree_numeric","Set the degree of this chart to @d@, and calculate the appropriate coefficients.",
<a name="line_262"></a>  proc(this,d,force_)
<a name="line_263"></a>   local p,s;
<a name="line_264"></a>
<a name="line_265"></a>   while this["degree"] < d do this["curve_improve_numeric"]; od;
<a name="line_266"></a>   if this["degree"] > d and nargs > 2 and force_ then
<a name="line_267"></a>    p := this["p"](s);
<a name="line_268"></a>    p := multi_series(p,d+1,s[1],s[2]);
<a name="line_269"></a>    this["p"] := unapply(p,s);
<a name="line_270"></a>    this["p0"] := unapply(p,s);
<a name="line_271"></a>    this["degree"] := d;
<a name="line_272"></a>   fi;
<a name="line_273"></a>
<a name="line_274"></a>   NULL;
<a name="line_275"></a>  end
<a name="line_276"></a> ],
<a name="line_277"></a>
<a name="line_278"></a> ["Method","centre_set_numeric","Set the centre and calculate coefficients, giving a chart of polynomial degree one.",
<a name="line_279"></a>  proc(this,x0)
<a name="line_280"></a>   local f0,p0,rels,sol,s,t,u,x,a;
<a name="line_281"></a>
<a name="line_282"></a>   this["curve_index"] := NULL;
<a name="line_283"></a>   this["curve_parameter"] := NULL;
<a name="line_284"></a>   this["degree"] := 2;
<a name="line_285"></a>   this["coeffs_exact"] := false;
<a name="line_286"></a>
<a name="line_287"></a>   this["centre"] := evalf(x0);
<a name="line_288"></a>   this["square_centre"] := evalf(square_diffeo_E0(x0));
<a name="line_289"></a>   f0 := full_frame_b(x0):
<a name="line_290"></a>   this["n"] := f0[2];
<a name="line_291"></a>   this["u"] := f0[3];
<a name="line_292"></a>   this["v"] := f0[4];
<a name="line_293"></a>
<a name="line_294"></a>   p0 := (1-t^2/2.-u^2/2.) *~ x0 +~ t *~ this["u"] +~ u *~ this["v"] +~
<a name="line_295"></a>           (a[1,0]*u^2+a[1,1]*t*u+a[1,2]*t^2) *~ this["n"]:
<a name="line_296"></a>   rels := [seq(coeff(coeff(multi_series(g1(p0),3,t,u),t,i),u,2-i),i=0..2)]:
<a name="line_297"></a>   sol := solve(rels):
<a name="line_298"></a>   p0 := subs(sol,p0);
<a name="line_299"></a>   p0 := subs({t=s[1],u=s[2]},p0);
<a name="line_300"></a>
<a name="line_301"></a>   this["p"] := unapply(p0,s);
<a name="line_302"></a>   this["p0"] := unapply(evalf(p0),s);
<a name="line_303"></a>   this["p_inv_approx"] := 
<a name="line_304"></a>    unapply(evalf(
<a name="line_305"></a>     [add(x[i]*this["u"][i],i=1..4),
<a name="line_306"></a>      add(x[i]*this["v"][i],i=1..4)]),x);
<a name="line_307"></a>
<a name="line_308"></a>   NULL;
<a name="line_309"></a>  end
<a name="line_310"></a> ],
<a name="line_311"></a>
<a name="line_312"></a> ["Method","centre_improve_numeric","Increase the polynomial degree of this chart by one.",
<a name="line_313"></a>  proc(this)
<a name="line_314"></a>   local p0,p0t,p0u,rels,sol,d,s,t,u,a;
<a name="line_315"></a>
<a name="line_316"></a>
<a name="line_317"></a>   p0 := this["p0"]([t,u]);
<a name="line_318"></a>   d := this["degree"];
<a name="line_319"></a>
<a name="line_320"></a>   p0 := p0 +~ 
<a name="line_321"></a>	 add(a[1,j]*t^j*u^(d+1-j),j=0..d+1) *~ this["centre"] +~
<a name="line_322"></a>	 add(a[2,j]*t^j*u^(d+1-j),j=0..d+1) *~ this["n"] +~
<a name="line_323"></a>	 add(a[3,j]*t^j*u^(d+1-j),j=0..d  ) *~ this["u"] +~
<a name="line_324"></a>	 add(a[4,j]*t^j*u^(d+1-j),j=0..d  ) *~ this["v"]:
<a name="line_325"></a>   p0t := map(diff,p0,t):
<a name="line_326"></a>   p0u := map(diff,p0,u):
<a name="line_327"></a>   rels := [
<a name="line_328"></a>    seq(coeff(coeff(multi_series(rho(p0) - 1,d+2,t,u),t,j),u,d+1-j),j=0..d+1),
<a name="line_329"></a>    seq(coeff(coeff(multi_series(g1(p0),d+2,t,u),t,j),u,d+1-j),j=0..d+1),
<a name="line_330"></a>    seq(coeff(coeff(multi_series(dp4(p0t,p0u),d+1,t,u),t,j),u,d-j),j=0..d),
<a name="line_331"></a>    seq(coeff(coeff(multi_series(dp4(p0t,p0t)-dp4(p0u,p0u),d+1,t,u),t,j),u,d-j),j=0..d),
<a name="line_332"></a>    NULL
<a name="line_333"></a>   ]:
<a name="line_334"></a>   sol := solve(rels):
<a name="line_335"></a>   p0 := subs(sol,p0):
<a name="line_336"></a>   p0 := subs({t=s[1],u=s[2]},p0);
<a name="line_337"></a>
<a name="line_338"></a>   this["p"] := unapply(p0,s);
<a name="line_339"></a>   this["p0"] := unapply(p0,s);
<a name="line_340"></a>   this["degree"] := d+1;
<a name="line_341"></a>   NULL;
<a name="line_342"></a>  end
<a name="line_343"></a> ],
<a name="line_344"></a>
<a name="line_345"></a> ["Method","centre_set_degree_numeric","Set the degree of this chart to @d@, and calculate the appropriate coefficients.",
<a name="line_346"></a>  proc(this,d,force_)
<a name="line_347"></a>   local p,s;
<a name="line_348"></a>
<a name="line_349"></a>   while this["degree"] < d do
<a name="line_350"></a>    this["centre_improve_numeric"];
<a name="line_351"></a>   od;
<a name="line_352"></a>   if this["degree"] > d and nargs > 2 and force_ then
<a name="line_353"></a>    p := this["p"](s);
<a name="line_354"></a>    p := multi_series(p,d+1,s[1],s[2]);
<a name="line_355"></a>    this["p"] := unapply(p,s);
<a name="line_356"></a>    this["p0"] := unapply(p,s);
<a name="line_357"></a>    this["degree"] := d;
<a name="line_358"></a>   fi;
<a name="line_359"></a>
<a name="line_360"></a>   NULL;
<a name="line_361"></a>  end
<a name="line_362"></a> ],
<a name="line_363"></a>
<a name="line_364"></a> ["Method","improve_numeric","Increase the polynomial degree by one, using the @curve_improve_numeric@ method if applicable, otherwise the @centre_improve_numeric@ method.",
<a name="line_365"></a>  proc(this)
<a name="line_366"></a>   if this["curve_index"] = NULL then
<a name="line_367"></a>    this["centre_improve_numeric"];
<a name="line_368"></a>   else
<a name="line_369"></a>    this["curve_improve_numeric"];
<a name="line_370"></a>   fi;
<a name="line_371"></a>  end
<a name="line_372"></a> ],
<a name="line_373"></a> 
<a name="line_374"></a> ["Method","set_degree_numeric","Set the polynomial degree, using the @curve_set_degree_numeric@ method if applicable, otherwise the @centre_set_degree_numeric@ method.",
<a name="line_375"></a>  proc(this,d)
<a name="line_376"></a>   if this["curve_index"] = NULL then
<a name="line_377"></a>    this["centre_set_degree_numeric",args[2..-1]];
<a name="line_378"></a>   else
<a name="line_379"></a>    this["curve_set_degree_numeric",args[2..-1]];
<a name="line_380"></a>   fi;
<a name="line_381"></a>  end
<a name="line_382"></a> ],
<a name="line_383"></a> 
<a name="line_384"></a> ["Method","square_set_numeric","",
<a name="line_385"></a>  proc(this,s0)
<a name="line_386"></a>   local x0,k,t0;
<a name="line_387"></a>   
<a name="line_388"></a>   x0 := square_diffeo_E0_inverse_search(s0);
<a name="line_389"></a>
<a name="line_390"></a>   if s0[2] = 0 then
<a name="line_391"></a>    k := 1;
<a name="line_392"></a>    t0 := arctan(sqrt(2.)*x0[2],x0[3]);
<a name="line_393"></a>   elif s0[2] = 1 then
<a name="line_394"></a>    k := 3;
<a name="line_395"></a>    t0 := arctan(x0[2],sqrt(1.5)*x0[3]);
<a name="line_396"></a>   elif s0[1] = 0 then
<a name="line_397"></a>    k := 0;
<a name="line_398"></a>    t0 := arctan(x0[2],x0[1]);
<a name="line_399"></a>   elif s0[1] = 1 then
<a name="line_400"></a>    k := 5;
<a name="line_401"></a>    t0 := arctan(x0[1],x0[4]+x0[3]/sqrt(8.));
<a name="line_402"></a>   else
<a name="line_403"></a>    k := NULL;
<a name="line_404"></a>    t0 := NULL;
<a name="line_405"></a>   fi;
<a name="line_406"></a>   
<a name="line_407"></a>   if k = NULL then
<a name="line_408"></a>    this["centre_set_numeric",x0];
<a name="line_409"></a>   else
<a name="line_410"></a>    this["curve_set_numeric",k,t0];
<a name="line_411"></a>   fi;
<a name="line_412"></a>
<a name="line_413"></a>   this["square_centre"] := s0;   
<a name="line_414"></a>  end
<a name="line_415"></a> ],
<a name="line_416"></a>
<a name="line_417"></a> ["Method","isometrize","This method assumes that @log_rescale_z@ defines a function $f$ on $EX^*$, and adjusts the chart to make it approximately isometric as a map from (the unit disk with the hyperbolic metric) to ($EX^*$ with $\\exp(2f)$ times the standard metric).",
<a name="line_418"></a>  proc(this,log_rescale_z)
<a name="line_419"></a>   local d0,d1,x0,r0,m0,p0,p1,zp0,nlrz,dlrz,nlrp,dlrp,lrp,rp0,rp1,ps1,sp,s1,err,errs,sol,s,a,x;
<a name="line_420"></a>
<a name="line_421"></a>   d0 := this["degree"];
<a name="line_422"></a>   x0 := this["centre"];
<a name="line_423"></a>   r0 := exp(evalf(log_rescale_z(z_proj0(x0))));
<a name="line_424"></a>   p0 := this["p0"]([s[1],s[2]]);
<a name="line_425"></a>   ps1 := subs({s[1]=0,s[2]=0},map(diff,p0,s[1]));
<a name="line_426"></a>   m0 := 2/r0/nm4(ps1);
<a name="line_427"></a>   p0 := subs({s[1]=s[1]*m0,s[2]=s[2]*m0},p0);
<a name="line_428"></a>   zp0 := multi_series(z_proj1(p0),d0+1,s[1],s[2]):
<a name="line_429"></a>   nlrz := unapply(numer(log_rescale_z(z)),z):
<a name="line_430"></a>   dlrz := unapply(denom(log_rescale_z(z)),z):
<a name="line_431"></a>   nlrp := multi_series(nlrz(zp0),d0+1,s[1],s[2]):
<a name="line_432"></a>   dlrp := multi_series(dlrz(zp0),d0+1,s[1],s[2]):
<a name="line_433"></a>   lrp := multi_series(nlrp/dlrp,d0+1,s[1],s[2]):
<a name="line_434"></a>   rp0 := multi_series(exp(2*lrp),d0+1,s[1],s[2]);
<a name="line_435"></a>
<a name="line_436"></a>   sp := [s[1],s[2]]:
<a name="line_437"></a>   for d1 from 2 to d0 do
<a name="line_438"></a>    sp := expand(C_mult([s[1],s[2]],sp));
<a name="line_439"></a>    s1 := expand([s[1],s[2]] +~ C_mult([a[1],a[2]],sp));
<a name="line_440"></a>    p1 := multi_series(eval(subs(s=s1,p0)),d1+1,s[1],s[2]);
<a name="line_441"></a>    rp1 := multi_series(eval(subs(s=s1,rp0)),d1+1,s[1],s[2]);
<a name="line_442"></a>    ps1 := map(diff,p1,s[1]);
<a name="line_443"></a>    err := multi_series(dp4(ps1,ps1) *~ rp1 - 4/(1-s[1]^2-s[2]^2)^2,d1,s[1],s[2]);
<a name="line_444"></a>    errs := [seq(coeff(coeff(err,s[1],i),s[2],d1-1-i),i=0..d1-1)];
<a name="line_445"></a>    sol := LSSolve(errs)[2];
<a name="line_446"></a>    s1 := subs(sol,s1);
<a name="line_447"></a>    p0 := multi_series(eval(subs(s=s1,p0)),d0+1,s[1],s[2]);
<a name="line_448"></a>    rp0 := multi_series(eval(subs(s=s1,rp0)),d0+1,s[1],s[2]);
<a name="line_449"></a>   od:
<a name="line_450"></a>
<a name="line_451"></a>   this["p"] := unapply(p0,s);
<a name="line_452"></a>   this["p0"] := unapply(p0,s);
<a name="line_453"></a>   this["coeffs_exact"] := false;
<a name="line_454"></a>   this["p_inv_approx"] :=
<a name="line_455"></a>    unapply(expand((1/m0) *~ this["p_inv_approx"]([x[1],x[2],x[3],x[4]])),x);
<a name="line_456"></a>  end
<a name="line_457"></a> ],
<a name="line_458"></a>
<a name="line_459"></a> ["Method","isometry_error","This returns a measure of the failure of $p$ to be isometric, in the context described above.",
<a name="line_460"></a>  proc(this,log_rescale_z,s1::RR0_2)
<a name="line_461"></a>   local x1,u1,v1,r1,E1,F1,G1,E2,s;
<a name="line_462"></a>
<a name="line_463"></a>   x1 := evalf(this["p0"](s1));
<a name="line_464"></a>   u1 := evalf(subs({s[1]=s1[1],s[2]=s1[2]},map(diff,this["p0"](s),s[1])));
<a name="line_465"></a>   v1 := evalf(subs({s[1]=s1[1],s[2]=s1[2]},map(diff,this["p0"](s),s[2])));
<a name="line_466"></a>   r1 := evalf(exp(log_rescale_z(z_proj1(x1))));
<a name="line_467"></a>   E1 := dp4(u1,u1) * r1^2;
<a name="line_468"></a>   F1 := dp4(u1,v1) * r1^2;
<a name="line_469"></a>   G1 := dp4(v1,v1) * r1^2;
<a name="line_470"></a>   E2 := evalf(4/(1-s1[1]^2-s1[2]^2)^2);
<a name="line_471"></a>
<a name="line_472"></a>   return max(abs(E1-E2),abs(F1),abs(G1-E2));
<a name="line_473"></a>  end
<a name="line_474"></a> ],
<a name="line_475"></a>
<a name="line_476"></a> ["Method","p_c","This is the composite of $p\\colon\\mathbb{R}^2\\to EX^*$ with the standard isomorphism $\\mathbb{C}\\to\\mathbb{R}^2$.",
<a name="line_477"></a>  proc(this)
<a name="line_478"></a>   local z;
<a name="line_479"></a>   return unapply(this["p"]([Re(z),Im(z)]),z);
<a name="line_480"></a>  end
<a name="line_481"></a> ],
<a name="line_482"></a>
<a name="line_483"></a> ["Method","p_inv","Inverse to the map $p\\colon\\mathbb{R}^2\\to EX^*$",
<a name="line_484"></a>  proc(this,x0)
<a name="line_485"></a>   local u0,v0,s0,r0,p0,eqs,vars,sol,s1,m;
<a name="line_486"></a>
<a name="line_487"></a>   s0 := evalf(this["p_inv_approx"](x0));
<a name="line_488"></a>   r0 := max(nm2(s0)/4,10.^(-10));
<a name="line_489"></a>   p0 := eval(this["p0"]);
<a name="line_490"></a>   u0 := evalf(this["u"]);
<a name="line_491"></a>   v0 := evalf(this["v"]);
<a name="line_492"></a>
<a name="line_493"></a>   eqs := evalf({dp4(u0,p0([s[1],s[2]]) -~ x0) = 0,
<a name="line_494"></a>                 dp4(v0,p0([s[1],s[2]]) -~ x0) = 0});
<a name="line_495"></a>
<a name="line_496"></a>   sol := FAIL;
<a name="line_497"></a>   m := 10;
<a name="line_498"></a>
<a name="line_499"></a>   while m > 0 and not type(sol,set) do
<a name="line_500"></a>    m := m-1;
<a name="line_501"></a>    r0 := 2*r0;
<a name="line_502"></a>    vars := {s[1] = s0[1]-r0..s0[1]+r0,
<a name="line_503"></a>             s[2] = s0[2]-r0..s0[2]+r0};
<a name="line_504"></a>
<a name="line_505"></a>    sol := fsolve(eqs,vars);
<a name="line_506"></a>   od;
<a name="line_507"></a>   
<a name="line_508"></a>   if not type(sol,set) then return FAIL; fi;
<a name="line_509"></a>
<a name="line_510"></a>   s1 := subs(sol,[s[1],s[2]]);
<a name="line_511"></a>   if nm2(s1) < 1 then
<a name="line_512"></a>    return(s1);
<a name="line_513"></a>   else
<a name="line_514"></a>    return FAIL;
<a name="line_515"></a>   fi;
<a name="line_516"></a>  end
<a name="line_517"></a> ],
<a name="line_518"></a>
<a name="line_519"></a> ["Method","p_inv_c","Inverse to the map $p_c\\colon\\mathbb{C}\\to EX^*$",
<a name="line_520"></a>  proc(this,x0)
<a name="line_521"></a>   local s0;
<a name="line_522"></a>   s0 := this["p_inv",x0];
<a name="line_523"></a>
<a name="line_524"></a>   return `if`(s0 = FAIL,FAIL,R2_to_C(s0));
<a name="line_525"></a>  end
<a name="line_526"></a> ],
<a name="line_527"></a>
<a name="line_528"></a> ["Method","disc_plot","Generate a plot showing the image under $p$ of a disc of radius $r$ centred at the origin in $\\mathbb{R}^2$",
<a name="line_529"></a>  proc(this,r)
<a name="line_530"></a>   local p0,p1,s,t;
<a name="line_531"></a>   
<a name="line_532"></a>   p0 := eval(this["p0"]);
<a name="line_533"></a>   p1 := p0([s*cos(t),s*sin(t)]);
<a name="line_534"></a>   plot3d(stereo(p1),s=0..r,t=0..2*Pi,axes=none,scaling=constrained);
<a name="line_535"></a>  end
<a name="line_536"></a> ],
<a name="line_537"></a>
<a name="line_538"></a> ["Method","circle_plot","Generate a plot showing the image under $p$ of a circle of radius $r$ centred at the origin in $\\mathbb{R}^2$",
<a name="line_539"></a>  proc(this,r)
<a name="line_540"></a>   local p0,p1,s,t;
<a name="line_541"></a>   
<a name="line_542"></a>   p0 := eval(this["p0"]);
<a name="line_543"></a>   p1 := p0([r*cos(t),r*sin(t)]);
<a name="line_544"></a>   spacecurve(stereo(p1),t=0..2*Pi,colour=red,axes=none,scaling=constrained);
<a name="line_545"></a>  end
<a name="line_546"></a> ],
<a name="line_547"></a>
<a name="line_548"></a> ["Method","err_plot","Generate a plot measuring the failure of $p$ to be a conformal map landing in $EX^*$.  This uses the values of $p$ and its derivatives on a circle of radius $r$ centred at the origin in $\\mathbb{R}^2$",
<a name="line_549"></a>  proc(this,r)
<a name="line_550"></a>   local p0,p1,s,t,u,v,errs;
<a name="line_551"></a>   
<a name="line_552"></a>   p0 := eval(this["p0"]);
<a name="line_553"></a>   p1 := p0([s*cos(t),s*sin(t)]);
<a name="line_554"></a>   u := map(diff,p1,s);
<a name="line_555"></a>   v := map(diff,p1,t) /~ s;
<a name="line_556"></a>   errs := subs(s=r,[
<a name="line_557"></a>    rho(p1) - 1,
<a name="line_558"></a>    g1(p1),
<a name="line_559"></a>    dp4(u,v),
<a name="line_560"></a>    dp4(u,u) - dp4(v,v)
<a name="line_561"></a>   ]);
<a name="line_562"></a>   
<a name="line_563"></a>   plot(errs,t=0..2*Pi);
<a name="line_564"></a>  end
<a name="line_565"></a> ],
<a name="line_566"></a>
<a name="line_567"></a> ["Method","check","This returns a table @T@ whose entries are nonnegative real numbers.  If the chart has all the properties that it is supposed to have, then the entries in @T@ will all be zero.  The entry @T[\"max\"]@ is the maximum of all the other entries.  In practice we find that when working to 100 digit precision, we end up with @T[\"max\"] < 10^(-89)@.  Note that we only test whether we have the correct Taylor coefficients for $p$, we do not test anything about how well the series is converging.",
<a name="line_568"></a>  proc(this)
<a name="line_569"></a>   local T,k0,t0,u0,s,t,e,p,p0,p01,p02,d;
<a name="line_570"></a>
<a name="line_571"></a>   p := eval(this["p"]);
<a name="line_572"></a>   p0 := eval(this["p0"]);
<a name="line_573"></a>   d  := this["degree"];
<a name="line_574"></a>   
<a name="line_575"></a>   T := table();
<a name="line_576"></a>   T["centre_rho"] := abs(evalf(rho(this["centre"]) - 1));
<a name="line_577"></a>   T["centre_g"]   := abs(evalf(g0(this["centre"])));
<a name="line_578"></a>   T["square_centre"] := evalf(d2(square_diffeo_E0(this["centre"]),this["square_centre"]));
<a name="line_579"></a>   if this["curve_index"] <> NULL then
<a name="line_580"></a>    k0 := this["curve_index"];
<a name="line_581"></a>    t0 := this["curve_parameter"];
<a name="line_582"></a>    T["curve_centre"] := evalf(d4(this["centre"],c_E0[k0](t0)));
<a name="line_583"></a>    u0 := evalf(subs(t=t0,map(diff,c_E0[k0](t),t)));
<a name="line_584"></a>    u0 := u0 /~ nm4(u0);
<a name="line_585"></a>    T["curve_u"] := evalf(d4(this["u"],u0));
<a name="line_586"></a>   fi;
<a name="line_587"></a>   T["nn"] := abs(evalf(dp4(this["n"],this["n"]) - 1));
<a name="line_588"></a>   T["uu"] := abs(evalf(dp4(this["u"],this["u"]) - 1));
<a name="line_589"></a>   T["vv"] := abs(evalf(dp4(this["v"],this["v"]) - 1));
<a name="line_590"></a>   T["xn"] := abs(evalf(dp4(this["centre"],this["n"])));
<a name="line_591"></a>   T["xu"] := abs(evalf(dp4(this["centre"],this["u"])));
<a name="line_592"></a>   T["xv"] := abs(evalf(dp4(this["centre"],this["v"])));
<a name="line_593"></a>   T["nu"] := abs(evalf(dp4(this["n"],this["u"])));
<a name="line_594"></a>   T["nv"] := abs(evalf(dp4(this["n"],this["v"])));
<a name="line_595"></a>   T["uv"] := abs(evalf(dp4(this["u"],this["v"])));
<a name="line_596"></a>   T["orientation"] :=
<a name="line_597"></a>    abs(Determinant(Matrix([this["centre"],this["n"],this["u"],this["v"]]))-1);
<a name="line_598"></a>   T["p_p0"] := max(map(abs,map(coeffs,evalf(p(s) -~ p0(s)),[s[1],s[2]])));
<a name="line_599"></a>   T["degree"] := max(map(degree,p([e*s[1],e*s[2]]),e)) - d;
<a name="line_600"></a>   T["p0_rho"] :=
<a name="line_601"></a>    max(map(abs,[coeffs(evalf(multi_series(rho(p0(s)) - 1,d+1,s[1],s[2])),[s[1],s[2]])]));
<a name="line_602"></a>   T["p0_g"] :=
<a name="line_603"></a>    max(map(abs,[coeffs(evalf(multi_series(g0(p0(s)),d+1,s[1],s[2])),[s[1],s[2]])]));
<a name="line_604"></a>   p01 := map(diff,p0(s),s[1]);
<a name="line_605"></a>   p02 := map(diff,p0(s),s[2]);
<a name="line_606"></a>   T["conformal_a"] :=
<a name="line_607"></a>    max(map(abs,[coeffs(multi_series(dp4(p01,p02),d,s[1],s[2]),[s[1],s[2]])]));
<a name="line_608"></a>   T["conformal_b"] :=
<a name="line_609"></a>    max(map(abs,[coeffs(multi_series(dp4(p01,p01)-dp4(p02,p02),d,s[1],s[2]),[s[1],s[2]])]));
<a name="line_610"></a>   T["p1u"] := abs(signum(evalf(dp4(subs({s[1]=0,s[2]=0},p01),this["u"]))) - 1);
<a name="line_611"></a>   T["p2v"] := abs(signum(evalf(dp4(subs({s[1]=0,s[2]=0},p02),this["v"]))) - 1);
<a name="line_612"></a>   T["p1v"] := abs(evalf(dp4(subs({s[1]=0,s[2]=0},p01),this["v"])));
<a name="line_613"></a>   T["p2u"] := abs(evalf(dp4(subs({s[1]=0,s[2]=0},p02),this["u"])));
<a name="line_614"></a>   T["p_inv_approx"] := 
<a name="line_615"></a>    max(map(abs,map(coeffs,multi_series(C["p_inv_approx"](C["p0"](s)),2,s[1],s[2]) -~ [s[1],s[2]],[s[1],s[2]])));
<a name="line_616"></a>
<a name="line_617"></a>   T["max"] := max(map(op,[entries(T)]));
<a name="line_618"></a>   
<a name="line_619"></a>   return eval(T);
<a name="line_620"></a>  end
<a name="line_621"></a> ]
<a name="line_622"></a>);
<a name="line_623"></a>
<a name="line_624"></a>######################################################################
<a name="line_625"></a>
<a name="line_626"></a><a name="CLASS_E_atlas"></a><span style="color:red">#@ CLASS: E_atlas
</span><a name="line_627"></a>
<a name="line_628"></a>`Class/Declare`("E_atlas",
<a name="line_629"></a> "An instance of this class represents an atlas of approximate conformal charts for $EX^*$",
<a name="line_630"></a>
<a name="line_631"></a> ["Constructor","",
<a name="line_632"></a>  proc(this)
<a name="line_633"></a>   this["charts"] := table();
<a name="line_634"></a>  end
<a name="line_635"></a> ],
<a name="line_636"></a> 
<a name="line_637"></a> ["Field","num_charts"::integer],
<a name="line_638"></a> ["Field","charts"::table],
<a name="line_639"></a>
<a name="line_640"></a> ["Method","add_chart","",
<a name="line_641"></a>  proc(this)
<a name="line_642"></a>   local C,n;
<a name="line_643"></a>   
<a name="line_644"></a>   n := this["num_charts"];
<a name="line_645"></a>   C := `new/E_chart`();
<a name="line_646"></a>   C["grid_index"] := n;
<a name="line_647"></a>   this["charts"][n] := eval(C);
<a name="line_648"></a>   this["num_charts"] := n+1;
<a name="line_649"></a>   return eval(C);
<a name="line_650"></a>  end
<a name="line_651"></a> ],
<a name="line_652"></a>
<a name="line_653"></a> ["Method","add_vertex_chart_exact","",
<a name="line_654"></a>  proc(this,k::integer,d::posint)
<a name="line_655"></a>   local C;
<a name="line_656"></a>   
<a name="line_657"></a>   C := eval(this["add_chart"]);
<a name="line_658"></a>   C["vertex_set_exact",k];
<a name="line_659"></a>   C["curve_set_degree_exact",d];
<a name="line_660"></a>   return eval(C);
<a name="line_661"></a>  end
<a name="line_662"></a> ],
<a name="line_663"></a>
<a name="line_664"></a> ["Method","add_vertex_chart_numeric","",
<a name="line_665"></a>  proc(this,k::integer,d::posint)
<a name="line_666"></a>   local C;
<a name="line_667"></a>   
<a name="line_668"></a>   C := eval(this["add_chart"]);
<a name="line_669"></a>   C["vertex_set_numeric",k];
<a name="line_670"></a>   C["curve_set_degree_numeric",d];
<a name="line_671"></a>   return eval(C);
<a name="line_672"></a>  end
<a name="line_673"></a> ],
<a name="line_674"></a>
<a name="line_675"></a> ["Method","add_curve_chart_exact","",
<a name="line_676"></a>  proc(this,k::integer,t0::RR0,d::posint)
<a name="line_677"></a>   local C;
<a name="line_678"></a>   
<a name="line_679"></a>   C := eval(this["add_chart"]);
<a name="line_680"></a>   C["curve_set_exact",k,t0];
<a name="line_681"></a>   C["curve_set_degree_exact",d];
<a name="line_682"></a>   return eval(C);
<a name="line_683"></a>  end
<a name="line_684"></a> ],
<a name="line_685"></a>
<a name="line_686"></a> ["Method","add_curve_chart_numeric","",
<a name="line_687"></a>  proc(this,k::integer,t0::RR0,d::posint)
<a name="line_688"></a>   local C;
<a name="line_689"></a>   
<a name="line_690"></a>   C := eval(this["add_chart"]);
<a name="line_691"></a>   C["curve_set_numeric",k,t0];
<a name="line_692"></a>   C["curve_set_degree_numeric",d];
<a name="line_693"></a>   return eval(C);
<a name="line_694"></a>  end
<a name="line_695"></a> ],
<a name="line_696"></a>
<a name="line_697"></a> ["Method","add_square_chart_numeric","",
<a name="line_698"></a>  proc(this,s0::RR0_2,d::posint)
<a name="line_699"></a>   local C;
<a name="line_700"></a>   
<a name="line_701"></a>   C := eval(this["add_chart"]);
<a name="line_702"></a>   C["square_centre_set_numeric",s0];
<a name="line_703"></a>   C["centre_set_degree_numeric",d];
<a name="line_704"></a>   return eval(C);
<a name="line_705"></a>  end
<a name="line_706"></a> ],
<a name="line_707"></a>
<a name="line_708"></a> ["Method","add_centre_chart_numeric","",
<a name="line_709"></a>  proc(this,x0::RR0_4,d::posint)
<a name="line_710"></a>   local C;
<a name="line_711"></a>   
<a name="line_712"></a>   C := eval(this["add_chart"]);
<a name="line_713"></a>   C["centre_set_numeric",x0];
<a name="line_714"></a>   C["centre_set_degree_numeric",d];
<a name="line_715"></a>   return eval(C);
<a name="line_716"></a>  end
<a name="line_717"></a> ]
<a name="line_718"></a>);
<a name="line_719"></a> 
  </pre>
 </body>
</html>
    