<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a><a name="CLASS_EH_chart"></a><span style="color:red">#@ CLASS: EH_chart
</span><a name="line_2"></a>
<a name="line_3"></a>`Class/Declare`("EH_chart",
<a name="line_4"></a> "An instance of this class represents an approximate isometric map $p\\colon\\Delta\\to EX^*$, together with data relating $p$ to the canonical map $q\\colon\\Delta\\to EX^*$.  Here $\\Delta$ is given the standard hyperbolic metric, and $EX^*$ is given the rescaled metric that has curvature $-1$.  We refer to the point $p(0)$ as the centre of the chart.  Note that this class extends the class #E_chart#, so documentation for that class should be read in conjunction with documentation for this one.",
<a name="line_5"></a> ["Extends","E_chart"],
<a name="line_6"></a> ["Field","curve_H_parameter"::RR0,"If the centre is $q(c_{Hk}(t))$, then this field should be set to $t$."],
<a name="line_7"></a> ["Field","alpha"::CC0,"This should be set so that $p(z)=q(\\lambda(z-\\alpha)/(1-\\overline{\\alpha}z)$"],
<a name="line_8"></a> ["Field","lambda"::CC0,"This should be set so that $p(z)=q(\\lambda(z-\\alpha)/(1-\\overline{\\alpha}z)$"],
<a name="line_9"></a> ["Field","beta"::CC0,"This should be set so that $\\beta = -\\lambda\\alpha$, so $p(0)=q(\\beta)$.  However, in some cases we will work out $\\beta$ first, and only set $\\alpha$ and $\\lambda$ later."],
<a name="line_10"></a> ["Field","grid_index"::integer],
<a name="line_11"></a>
<a name="line_12"></a> ["Method","m_c","The map $z\\mapsto\\lambda(z-\\alpha)/(1-\\overline{\\alpha}z)$",
<a name="line_13"></a>  proc(this)
<a name="line_14"></a>   local z;
<a name="line_15"></a>   if not (type(this["alpha"],CC0) and type(this["lambda"],CC0)) then
<a name="line_16"></a>    error("(alpha,lambda) is not set");
<a name="line_17"></a>   fi;
<a name="line_18"></a>
<a name="line_19"></a>   unapply(this["lambda"] *
<a name="line_20"></a>          (z - this["alpha"])/(1 - conjugate(this["alpha"]) * z),z);
<a name="line_21"></a>  end
<a name="line_22"></a> ],
<a name="line_23"></a>
<a name="line_24"></a> ["Method","m_inv_c","The inverse of the map $z\\mapsto\\lambda(z-\\alpha)/(1-\\overline{\\alpha}z)$",
<a name="line_25"></a>  proc(this)
<a name="line_26"></a>   local z,mu,beta;
<a name="line_27"></a>   if not (type(this["alpha"],CC0) and type(this["lambda"],CC0)) then
<a name="line_28"></a>    error("(alpha,lambda) is not set");
<a name="line_29"></a>   fi;
<a name="line_30"></a>
<a name="line_31"></a>   mu := conjugate(this["lambda"]);
<a name="line_32"></a>   beta := - this["lambda"] * this["alpha"];
<a name="line_33"></a>
<a name="line_34"></a>   unapply(mu * (z - beta)/(1 - conjugate(beta) * z),z);
<a name="line_35"></a>  end
<a name="line_36"></a> ],
<a name="line_37"></a>
<a name="line_38"></a> ["Method","m","The map $z\\mapsto\\lambda(z-\\alpha)/(1-\\overline{\\alpha}z)$, composed with the standard isomorphism $\\mathbb{R}^2\\to\\mathbb{C}$",
<a name="line_39"></a>  proc(this)
<a name="line_40"></a>   local s;
<a name="line_41"></a>   assume(s[1]::real);
<a name="line_42"></a>   assume(s[2]::real);
<a name="line_43"></a>   
<a name="line_44"></a>   return unapply(simplify(C_to_R2(this["m_c"](s[1]+I*s[2]))),s);
<a name="line_45"></a>  end
<a name="line_46"></a> ],
<a name="line_47"></a>
<a name="line_48"></a> ["Method","m_inv","The inverse map $z\\mapsto\\lambda(z-\\alpha)/(1-\\overline{\\alpha}z)$, composed with the standard isomorphism $\\mathbb{C}\\to\\mathbb{R}^2$",
<a name="line_49"></a>  proc(this)
<a name="line_50"></a>   local s;
<a name="line_51"></a>   assume(s[1]::real);
<a name="line_52"></a>   assume(s[2]::real);
<a name="line_53"></a>   
<a name="line_54"></a>   return unapply(simplify(C_to_R2(this["m_inv_c"](s[1]+I*s[2]))),s);
<a name="line_55"></a>  end
<a name="line_56"></a> ],
<a name="line_57"></a>
<a name="line_58"></a> ["Method","q","",
<a name="line_59"></a>  proc(this)
<a name="line_60"></a>   unapply(this["p"](this["m_inv"](z)),z);
<a name="line_61"></a>  end
<a name="line_62"></a> ],
<a name="line_63"></a>
<a name="line_64"></a> ["Method","q_c","",
<a name="line_65"></a>  proc(this)
<a name="line_66"></a>   unapply(this["p_c"](this["m_inv_c"](z)),z);
<a name="line_67"></a>  end
<a name="line_68"></a> ],
<a name="line_69"></a>
<a name="line_70"></a> ["Method","q_inv","",
<a name="line_71"></a>  proc(this,x0)
<a name="line_72"></a>   local s0;
<a name="line_73"></a>   s0 := this["p_inv",x0];
<a name="line_74"></a>   `if`(s0 = FAIL,FAIL,this["m"](s0));
<a name="line_75"></a>  end
<a name="line_76"></a> ],
<a name="line_77"></a>
<a name="line_78"></a> ["Method","q_inv_c","",
<a name="line_79"></a>  proc(this,x0)
<a name="line_80"></a>   local s0;
<a name="line_81"></a>   s0 := this["p_inv",x0];
<a name="line_82"></a>   `if`(s0 = FAIL,FAIL,R2_to_C(this["m"](s0)));
<a name="line_83"></a>  end
<a name="line_84"></a> ],
<a name="line_85"></a>
<a name="line_86"></a> ["Method","vertex_set","",
<a name="line_87"></a>  proc(this,k::integer,d::posint,A::EH_atlas)
<a name="line_88"></a>   this["vertex_index"] := k;
<a name="line_89"></a>
<a name="line_90"></a>   if k = 0 then
<a name="line_91"></a>    this["curve_set",1,0,d,A];
<a name="line_92"></a>   elif k = 3 then
<a name="line_93"></a>    this["curve_set",0,Pi/2,d,A];
<a name="line_94"></a>   elif k = 6 then
<a name="line_95"></a>    this["curve_set",1,Pi/2,d,A];
<a name="line_96"></a>   elif k = 11 then
<a name="line_97"></a>    this["curve_set",3,0,d,A];
<a name="line_98"></a>   else
<a name="line_99"></a>    this["vertex_index"] := NULL;
<a name="line_100"></a>    error("Invalid vertex index");
<a name="line_101"></a>   fi;
<a name="line_102"></a>  end
<a name="line_103"></a> ],
<a name="line_104"></a>
<a name="line_105"></a> ["Method","curve_set","",
<a name="line_106"></a>  proc(this,k::integer,t0::RR0,d::posint,A::EH_atlas)
<a name="line_107"></a>   local t,t1,cH,lambda,alpha;
<a name="line_108"></a>
<a name="line_109"></a>   `E_chart!curve_set_numeric`(this,k,evalf(t0));
<a name="line_110"></a>   `E_chart!curve_set_degree_numeric`(this,d);
<a name="line_111"></a>   `E_chart!isometrize`(this,A["log_rescale_z"]);
<a name="line_112"></a>
<a name="line_113"></a>   cH := evalf(subs(a_H=A["a_H"],c_H[k](t)));
<a name="line_114"></a>   t1 := evalf(A["u_inv"][k](t0));
<a name="line_115"></a>   this["curve_H_parameter"] := t1;
<a name="line_116"></a>   lambda := evalf(subs(t=t1,diff(cH,t)));
<a name="line_117"></a>   lambda := lambda/abs(lambda);
<a name="line_118"></a>   alpha  := evalf(subs(t=t1,cH))/(-lambda);
<a name="line_119"></a>   this["lambda"] := lambda;
<a name="line_120"></a>   this["alpha"]  := alpha;
<a name="line_121"></a>   this["beta"]   := -lambda*alpha;
<a name="line_122"></a>   NULL;
<a name="line_123"></a>  end
<a name="line_124"></a> ],
<a name="line_125"></a>
<a name="line_126"></a> ["Method","centre_set","",
<a name="line_127"></a>  proc(this,x0::RR0_4,d::posint,A::EH_atlas)
<a name="line_128"></a>   local t1,lambda,alpha;
<a name="line_129"></a>
<a name="line_130"></a>   `E_chart!centre_set_numeric`(this,evalf(x0));
<a name="line_131"></a>   `E_chart!centre_set_degree_numeric`(this,d);
<a name="line_132"></a>   `E_chart!isometrize`(this,A["log_rescale_z"]);
<a name="line_133"></a>
<a name="line_134"></a>   this["lambda"] := NULL;
<a name="line_135"></a>   this["alpha"]  := NULL;
<a name="line_136"></a>   this["beta"]   := NULL;
<a name="line_137"></a>  end
<a name="line_138"></a> ],
<a name="line_139"></a>
<a name="line_140"></a> ["Method","square_set","",
<a name="line_141"></a>  proc(this,s0::RR0_2,d::posint,A::EH_atlas)
<a name="line_142"></a>   local t1,lambda,alpha;
<a name="line_143"></a>
<a name="line_144"></a>   `E_chart!square_set_numeric`(this,s0);
<a name="line_145"></a>   `E_chart!centre_set_degree_numeric`(this,d);
<a name="line_146"></a>   `E_chart!isometrize`(this,A["log_rescale_z"]);
<a name="line_147"></a>
<a name="line_148"></a>   this["lambda"] := NULL;
<a name="line_149"></a>   this["alpha"]  := NULL;
<a name="line_150"></a>   this["beta"]   := NULL;
<a name="line_151"></a>  end
<a name="line_152"></a> ]
<a name="line_153"></a>):
<a name="line_154"></a>
<a name="line_155"></a>######################################################################
<a name="line_156"></a>
<a name="line_157"></a><a name="CLASS_EH_atlas_edge"></a><span style="color:red">#@ CLASS: EH_atlas_edge
</span><a name="line_158"></a>
<a name="line_159"></a>`Class/Declare`("EH_atlas_edge",
<a name="line_160"></a> "An instance of this class represents a pair of charts whose centres are close together.  We can triangulate $F_{16}$ using the chart centres as 0-simplices, and these pairs as 1-simplices.",
<a name="line_161"></a> ["Field","grid_index"::integer,"As part of the data of an atlas, we have a list of edges, numbered from 0 to $n-1$ say.  If this is the $i$'th edge in the list, then this @grid_index@ field should be set equal to $i$."],
<a name="line_162"></a> ["Field","start_index"::integer,"If this edge links chart number $i$ to chart number $j$ (with $i<j$) then @start_index@ should be $i$ and @end_index@ should be $j$."],
<a name="line_163"></a> ["Field","end_index"::integer,"If this edge links chart number $i$ to chart number $j$ (with $i<j$) then @start_index@ should be $i$ and @end_index@ should be $j$."],
<a name="line_164"></a> ["Field","start_z"::CC0,"Suppose that this edge links chart number $i$ to chart number $j$, and chart $j$ is given by  $p:C\\to EX^*$.  There will then be a small number $z$ such that $p(z)$ is the centre of chart $i$, and this $z$ should be stored in the @start_z@ field."],
<a name="line_165"></a> ["Field","end_z"::CC0,"Suppose that this edge links chart number $i$ to chart number $j$, and chart $i$ is given by  $p:C\\to EX^*$.  There will then be a small number $z$ such that $p(z)$ is the centre of chart $j$, and this $z$ should be stored in the @end_z@ field."],
<a name="line_166"></a> ["Field","curve_index"::integer = NULL,"If this edge lies along one of the four curves that bound F16, then this field should be set to the index of the relevant curve (0,1,3 or 5)."],
<a name="line_167"></a> ["Field","H_length"::RR0,"This is the hyperbolic distance between the @beta@ fields of the two charts.  If everything is accurate, it should be the same as the @EH_length@ field."],
<a name="line_168"></a> ["Field","EH_length"::RR0,"This is an estimate of the distance between the centres of the two charts, with respect to the rescaled metric on $EX^*$.   If everything is accurate, it should be the same as the @H_length@ field."],
<a name="line_169"></a>
<a name="line_170"></a> ["Method","colour","If this edge lies along one of the four curves that bound $F_{16}$, then this method will return the appropriate colour.  Edges that do not lie on a boundary curve will be coloured grey.",
<a name="line_171"></a>  proc(this)
<a name="line_172"></a>   if this["curve_index"] <> NULL then
<a name="line_173"></a>    return c_colour[this["curve_index"]];
<a name="line_174"></a>   else
<a name="line_175"></a>    return grey;
<a name="line_176"></a>   fi;
<a name="line_177"></a>  end
<a name="line_178"></a> ]
<a name="line_179"></a>):
<a name="line_180"></a>
<a name="line_181"></a>######################################################################
<a name="line_182"></a>
<a name="line_183"></a><a name="CLASS_EH_atlas"></a><span style="color:red">#@ CLASS: EH_atlas
</span><a name="line_184"></a>
<a name="line_185"></a>`Class/Declare`("EH_atlas",
<a name="line_186"></a> "An instance of this class represents an approximation to the isomorphism $HX(a)\\to EX^*$, together with associated data.  ",
<a name="line_187"></a>
<a name="line_188"></a> ["Extends","E_atlas"],
<a name="line_189"></a>
<a name="line_190"></a> ["Constructor","",
<a name="line_191"></a>  proc(this)
<a name="line_192"></a>   this["curve_lengths"] := table();
<a name="line_193"></a>   this["curve_a_H_estimates"] := table();
<a name="line_194"></a>   this["u"] := table();
<a name="line_195"></a>   this["u_inv"] := table();
<a name="line_196"></a>   this["c_E_rescaled_speed"] := table();
<a name="line_197"></a>   this["c_E_rescaled_length"] := table();
<a name="line_198"></a>   this["c_E_average_rescaled_speed"] := table();
<a name="line_199"></a>   this["charts"] := table();
<a name="line_200"></a>   this["edges"] := table();
<a name="line_201"></a>  end
<a name="line_202"></a> ],
<a name="line_203"></a>
<a name="line_204"></a> ["Field","a_H"::RR0,"This is an approximation to the number a such that $EX^*$ is isomorphic to $HX(a)$"],
<a name="line_205"></a>
<a name="line_206"></a> ["Field","log_rescale_z","This is an approximation to the function $f$ such that $\\exp(2f)$ times the metric tensor inherited from $\\mathbb{R}^4$ has curvature equal to $-1$.  It is represented here as a function from the $z$-plane to $\\mathbb{R}$.  It should be set using the @set_rescale@ method, or one of the @find_rescale_*@ methods."],
<a name="line_207"></a>
<a name="line_208"></a> ["Field","log_rescale_x","This is an approximation to the function $f$ such that $\\exp(2f)$ times the metric tensor inherited from $\\mathbb{R}^4$ has curvature equal to $-1$.  It is represented here as a function from $\\mathbb{R}^4$ to $\\mathbb{R}$.  It should be set using the @set_rescale@ method, or one of the @find_rescale_*@ methods."],
<a name="line_209"></a>
<a name="line_210"></a> ["Field","rescale_z","This is the composite of $\\exp$ with the map in the @log_rescale_z@ field.  It should be set using the @set_rescale@ method."],
<a name="line_211"></a> ["Field","rescale_x","This is the composite of $\\exp$ with the map in the @log_rescale_x@ field.  It should be set using the @set_rescale@ method, or one of the @find_rescale_*@ methods."],
<a name="line_212"></a>
<a name="line_213"></a> ["Field","rescale_type"::string,"This should be \"pade\" or \"poly\", to specify whether the @log_rescale_z@ function is polynomial or rational"],
<a name="line_214"></a>
<a name="line_215"></a> ["Field","rescale_degree"::posint,"When the @log_rescale_z@ function is polynomial in $z_1$ and $z_2$, this field specifies the total degree.  When the @log_rescale_z@ function is rational in $z_1$ and $z_2$, this field specifies the total degree of the numerator and denominator.  This field should be set by the @set_rescale_type@ method."],
<a name="line_216"></a>
<a name="line_217"></a> ["Field","rescale_dof"::posint,"This field holds the number of degrees of freedom that we can vary when specifying the @log_rescale_z@ function.  It is calculated by the @set_rescale_type@ method from the @rescale_type@ and @rescale_degree@ fields."],
<a name="line_218"></a>
<a name="line_219"></a> ["Field","curvature_z","This is the curvature of the rescaled metric, represented as a function from the $z$-plane to $\\mathbb{R}$.  It is set by the @set_rescale@ method, and also by the @find_rescale_*@ methods."],
<a name="line_220"></a>
<a name="line_221"></a> ["Field","quadrature_rule"::E_quadrature_rule,"This is a quadrature rule, which we use to measure the total deviation of the rescaled curvature from the desired value of $-1$.  It is not important for the quadrature to be accurate.  This field must be set before any of the @find_rescale_*@ methods can be used."],
<a name="line_222"></a>
<a name="line_223"></a> ["Field","rescaling_error"::numeric,"This is the integral of $(K+1)^2$, where $K$ is the curvature of the rescaled metric.  It is set by the @set_rescale@ method, and also by the @find_rescale_*@ methods."],
<a name="line_224"></a>
<a name="line_225"></a> ["Field","curve_lengths"::table,"This is a table indexed by $\\{0,1,3,5\\}$, which holds the lengths (with respect to the rescaled metric) of the sides of the fundamental domain $F_{16}$.  These lengths are set by the @find_a_H@ method."],
<a name="line_226"></a>
<a name="line_227"></a> ["Field","curve_a_H_estimates"::table,"This is a table indexed by $\\{0,1,3,5\\}$.  The $i$'th entry is an estimate of $a_H$ obtained from the length of edge $i$ in $F_{16}$.  These estimates are set by the @find_a_H@ method."],
<a name="line_228"></a>
<a name="line_229"></a> ["Field","a_H_discrepancy","This is the maximum of the differences between the estimates of $a_H$ obtained from the different edges of $F_{16}$.  This is set by the @find_a_H@ method."],
<a name="line_230"></a>
<a name="line_231"></a> ["Field","u_degree"::posint,"The fields discussed below are Fourier sin series.  The @u_degree@ field holds the number of terms in each of these series.  This number is set by supplying it as an argument to the @find_u@ method."],
<a name="line_232"></a>
<a name="line_233"></a> ["Field","u"::table,"This is a table indexed by $\\{0,1,3,5\\}$.  The $i$'th entry is an approximation to the function $u$ such that $q(c_{Hi}(t))=c_{Ei}(u(t))$.  It has the form $u(t) = t + \\sum_j a_j\\sin(jmt)$, where $m=4$ for $i=0$, and $m=2$ for $i\\in\\{1,3\\}$, and $m=1$ for $i=5$.  This table is set by the @find_u@ method."],
<a name="line_234"></a>
<a name="line_235"></a> ["Field","u_inv"::table,"This is a table, whose entries are approximately the inverses of the functions stored in the @u@ field.  This table is set by the @find_u@ method."],
<a name="line_236"></a>
<a name="line_237"></a> ["Field","c_E_rescaled_speed"::table,"This is a table indexed by $\\{0,1,3,5\\}$.  The $i$'th entry is a Fourier approximation to the speed of $c_{Ei}(t)$ with respect to the rescaled metric.  This table is set by the @find_u@ method."],
<a name="line_238"></a>
<a name="line_239"></a> ["Field","c_E_rescaled_length"::table,"This is a table indexed by $\\{0,1,3,5\\}$.  The $i$'th entry is the indefinite integral of the corresponding entry in the @c_E_rescaled_speed@ field; this is a constant multiple of the $i$'th entry in the @u_inv@ field.  This table is set by the @find_u@ method."],
<a name="line_240"></a>
<a name="line_241"></a> ["Field","c_E_average_rescaled_speed"::table,"This is a table indexed by $\\{0,1,3,5\\}$.  The $i$'th entry is the time average of the corresponding entry in the @c_E_rescaled_speed@ field.  This table is set by the @find_u@ method."],
<a name="line_242"></a>
<a name="line_243"></a> ["Field","H_samples"::list,"A list of closely spaced points in $HF_{16}$, set by the @make_H_samples@ method."],
<a name="line_244"></a> ["Field","D_samples"::list,"A list of closely spaced points in a disc centred at the origin that contains $HF_{16}$ (but usually has radius less than 1).  This is set by the @make_D_samples@ method."],
<a name="line_245"></a> ["Field","H_samples_q"::list,"Images in $EX^*$ of the points in @H_samples@.  These are set by the @make_H_samples_q@ method, which will only work once we have constructed and adjusted charts to put ourselves in a position to calculate the map @q@."],
<a name="line_246"></a> ["Field","D_samples_q"::list,"Images in $EX^*$ of the points in @D_samples@.  These are set by the @make_D_samples_q@ method, which will only work once we have constructed and adjusted charts to put ourselves in a position to calculate the map @q@."],
<a name="line_247"></a> ["Field","chart_dist"::table,"Distances between H-sample points and centres of charts.  This can be set by the @set_chart_dist@ method after charts have been created and the @H_samples@ field has been set.  "],
<a name="line_248"></a>
<a name="line_249"></a> ["Field","square_q_inv" = NULL,"A polynomial function $\\mathbb{R}^2\\to\\mathbb{C}$ which approximates the inverse of the map $HF_{16}\\xrightarrow{q}EX^*\\xrightarrow{\\delta}[0,1]^2$ (where $\\delta$ is @square_diffeo@).  This can be set by the @set_square_q_inv@ method."],
<a name="line_250"></a> 
<a name="line_251"></a> ["Field","num_edges"::integer,"Some methods rely on having a triangulation of $F_{16}$ with the centres of the charts as vertices.  After adding the charts, we specify the edges of the triangulation using the @add_edge@ method.  The number of edges is stored in the @num_edges@ field."],
<a name="line_252"></a> ["Field","edges"::table,"This is a table indexed by natural numbers.  Each entry is an instance of the class @EH_atlas_edge@, representing an edge between chart centres."],
<a name="line_253"></a> ["Field","fourier_r_max"::RR0,"The @set_q_approx_fourier@ method will look for a Fourier approximation for $q$ on a disc of this radius centred at the origin.  This radius is set by supplying it as the first argument to @set_q_approx_fourier@."],
<a name="line_254"></a> ["Field","fourier_m"::posint,"The number of circles on which we calculate the Fourier series.  This number is set by supplying it as the second argument to @set_q_approx_fourier@."],
<a name="line_255"></a> ["Field","fourier_r"::list,"The list of radii of the circles on which we calculate the Fourier series.  This is set by @set_q_approx_fourier@."],
<a name="line_256"></a> ["Field","fourier_k"::posint,"The number of sample points on each circle will be $2^k$.  This number is set by supplying it as the third argument to @set_q_approx_fourier@."],
<a name="line_257"></a> ["Field","fourier_v"::table,"A table indexed by pairs $(r,t)$, where $r$ is in @fourier_r@ and $t$ is an integer multiple of $2^{-k}$.  The entries are $q(r e^{2\\pi i t})\\in\\mathbb{R}^4$.  This is set by @set_q_approx_fourier@."],
<a name="line_258"></a> ["Field","fourier_a"::table,"A table indexed by triples $(k,j,r)$, where $1\\leq k\l\leq 4$ and $j\\geq 0$ and $r$ is in @fourier_r@.  The entries are Fourier coefficients for $q_k$ on the circle of radius $r$.  This is set by @set_q_approx_fourier@."],
<a name="line_259"></a> ["Field","fourier_qr"::table,"A table indexed by pairs $(k,r)$, giving a Fourier approximation to $q_k$ on a circle of radius $r$.  This is set by @set_q_approx_fourier@."],
<a name="line_260"></a> ["Field","fourier_a_spline"::table,"A table indexed by pairs $(k,j)$, giving a spline approximation to the Fourier coefficient $a_{k,j}(r)$.  This is set by the method @set_fourier_a_spline@, which is called by @set_q_approx_fourier@."],
<a name="line_261"></a>
<a name="line_262"></a> ["Field","q_approx","A rational function $\\mathbb{R}^2\\to\\mathbb{R}^4$ which approximates $q$ on a disc containing $HF_4$"],
<a name="line_263"></a> ["Field","H_to_P_map","An object encoding information about a cromulent isomorphism $HX(a_H)\\to PX(a_P)$"],
<a name="line_264"></a> ["Field","P_to_H_map","An object encoding information about a cromulent isomorphism $PX(a_P)\\to HX(a_H)$"],
<a name="line_265"></a> ["Field","E_to_S_map","An object encoding information about a conformal map $EX^*\\to S^2$"],
<a name="line_266"></a>
<a name="line_267"></a> ["Method","set_rescale_type","This sets the type and degree of the rescaling function",
<a name="line_268"></a>  proc(this,t::string,d::posint)
<a name="line_269"></a>   if t = "poly" then
<a name="line_270"></a>    this["rescale_type"] := "poly";
<a name="line_271"></a>    this["rescale_degree"] := d;
<a name="line_272"></a>    this["rescale_dof"]    := (d+1)*(d+2)/2;
<a name="line_273"></a>   elif t = "pade" then
<a name="line_274"></a>    this["rescale_type"]   := "pade";
<a name="line_275"></a>    this["rescale_degree"] := d;
<a name="line_276"></a>    this["rescale_dof"]    := d^2+3*d+1;
<a name="line_277"></a>   else
<a name="line_278"></a>    error "Invalid scaling type";
<a name="line_279"></a>   fi;
<a name="line_280"></a>  end
<a name="line_281"></a> ],
<a name="line_282"></a>
<a name="line_283"></a> ["Method","set_rescale","This sets the log of rescaling function itself, together with some other fields that can be calculated from it.  The argument should be an expression in $z_1$ and $z_2$.",
<a name="line_284"></a>  proc(this,f0)
<a name="line_285"></a>   this["log_rescale_z"] := unapply(f0,z);
<a name="line_286"></a>   this["log_rescale_x"] := unapply(subs(z = z_proj0(x),f0),x);
<a name="line_287"></a>   this["rescale_z"]     := unapply(exp(f0),z);
<a name="line_288"></a>   this["rescale_x"]     := unapply(exp(subs(z = z_proj0(x),f0)),x);
<a name="line_289"></a>   this["curvature_z"]   := unapply(this["rescaled_curvature",f0],z);
<a name="line_290"></a>
<a name="line_291"></a>   this["rescaling_error"] := 
<a name="line_292"></a>    this["find_rescaling_error",f0];
<a name="line_293"></a>  end
<a name="line_294"></a> ],
<a name="line_295"></a>
<a name="line_296"></a> ["Method","rescaled_curvature","This calculates the curvature of $\\exp(2f)$ times the metric inherited from $\\mathbb{R}^4$.  It expects $f$ to be an expression in @z[1]@ and @z[2]@ (not a function).  It does not set the @log_rescale_z@ field, because it is intended to be used in an exploratory way when searching for a better rescaling function.",
<a name="line_297"></a>  proc(this,f)
<a name="line_298"></a>   return (curvature_z0(z) - laplacian_z0(f))/exp(2*f);
<a name="line_299"></a>  end
<a name="line_300"></a> ],
<a name="line_301"></a>
<a name="line_302"></a> ["Method","find_rescaling_error","This calculates the integral of $(K+1)^2$, where $K$ is the curvature of $\\exp(2f)$ times the metric inherited from $\\mathbb{R}^4$.  It expects $f$ to be an expression in @z[1]@ and @z[2]@ (not a function).  It does not set the @rescaling_error@ field, because it is intended to be used in an exploratory way when searching for a better rescaling function.",
<a name="line_303"></a>  proc(this,f)
<a name="line_304"></a>   local K;
<a name="line_305"></a>
<a name="line_306"></a>   K := this["rescaled_curvature",f];
<a name="line_307"></a>   if not(type(this["quadrature_rule"],E_quadrature_rule)) then
<a name="line_308"></a>    error("No quadrature rule set");
<a name="line_309"></a>   fi;
<a name="line_310"></a>   return this["quadrature_rule"]["int_z",(1+K)^2];
<a name="line_311"></a>  end
<a name="line_312"></a> ],
<a name="line_313"></a>
<a name="line_314"></a> ["Method","find_rescale_poly","This method attempts to find a polynomial approximation to the function $f$ such that $\\exp(2f)$ times the standard metric on $EX^*$ has curvature $-1$.  The argument @d@ specifies the degree of polynomial (in $z_1$ and $z_2$) to use.",
<a name="line_315"></a>  proc(this,d::posint)
<a name="line_316"></a>   local f0,v0,F,E,A,m;
<a name="line_317"></a>
<a name="line_318"></a>   this["set_rescale_type","poly",d];
<a name="line_319"></a>
<a name="line_320"></a>   f0 := this["log_rescale_z"](z);
<a name="line_321"></a>   if not(type(f0,polynom([z[1],z[2]]))) then
<a name="line_322"></a>    f0 := .266*z[1]^5-.414*z[1]^4+.353*z[1]^3+0.579e-1*z[1]^2+.421*z[1]-.367-.117*z[1]^4*z[2]-
<a name="line_323"></a>	   .294*z[1]^3*z[2]-0.952e-1*z[1]^2*z[2]-.197*z[1]*z[2]-.340*z[2]+.676*z[1]^3*z[2]^2+
<a name="line_324"></a>	   .461*z[1]^2*z[2]^2+.310*z[1]*z[2]^2+.232*z[2]^2-1.06*z[1]^2*z[2]^3-.745*z[1]*z[2]^3-
<a name="line_325"></a>	   .505*z[2]^3+1.08*z[1]*z[2]^4+1.13*z[2]^4-1.02*z[2]^5:
<a name="line_326"></a>   fi:
<a name="line_327"></a>
<a name="line_328"></a>   v0 := this["poly_to_vector",f0,d];
<a name="line_329"></a>
<a name="line_330"></a>   F := proc(v)
<a name="line_331"></a>    global F_count;
<a name="line_332"></a>    local E;
<a name="line_333"></a>    if type(F_count,integer) then
<a name="line_334"></a>     F_count := F_count + 1;
<a name="line_335"></a>    else
<a name="line_336"></a>     F_count := 1;
<a name="line_337"></a>    fi;
<a name="line_338"></a>    E := this["find_rescaling_error",this["vector_to_poly",v]];
<a name="line_339"></a>    print(["calling F",F_count,log[10](E)]);
<a name="line_340"></a>    return E;
<a name="line_341"></a>   end;
<a name="line_342"></a>
<a name="line_343"></a>   m := this["rescale_dof"];
<a name="line_344"></a>   E,A := op(NLPSolve(m,F,initialpoint = Vector(v0)));
<a name="line_345"></a>
<a name="line_346"></a>   this["rescaling_error"] := E;
<a name="line_347"></a>   f0 := this["vector_to_poly",A];
<a name="line_348"></a>   this["set_rescale",f0];
<a name="line_349"></a>
<a name="line_350"></a>   NULL;
<a name="line_351"></a>  end
<a name="line_352"></a> ],
<a name="line_353"></a>
<a name="line_354"></a> ["Method","find_rescale_poly_alt","This method attempts to find a polynomial approximation to the function $f$ such that $\\exp(2f)$ times the standard metric on $EX^*$ has curvature $-1$.  The argument @d@ specifies the degree of polynomial (in $z_1$ and $z_2$) to use.  This version is harder to understand than @find_rescale_poly()@, but is much quicker.",
<a name="line_355"></a>  proc(this,d::posint)
<a name="line_356"></a>   local Q,n,m,K0,monomials,laplacians,points,rweights,kpoints,lpoints,mpoints,ones,F,J,f0,a0,E,A;
<a name="line_357"></a>
<a name="line_358"></a>   this["set_rescale_type","poly",d];
<a name="line_359"></a>   m := this["rescale_dof"];
<a name="line_360"></a>
<a name="line_361"></a>   Q := eval(this["quadrature_rule"]):
<a name="line_362"></a>   if not(type([Q],[E_quadrature_rule])) then
<a name="line_363"></a>    error("No quadrature rule set");
<a name="line_364"></a>   fi;
<a name="line_365"></a>   n := Q["num_points"];
<a name="line_366"></a>
<a name="line_367"></a>   K0 := evalf(curvature_z0(z));
<a name="line_368"></a>
<a name="line_369"></a>   monomials  := [seq(seq(z[1]^j*z[2]^(i-j),j=0..i),i=0..d)];
<a name="line_370"></a>   laplacians := map(laplacian_z0,monomials);
<a name="line_371"></a>   points     := [seq(Q["points"][i]["z"],i=0..n-1)];
<a name="line_372"></a>   rweights   := Transpose(Vector([seq(sqrt(max(0,Q["weights"][i])),i=0..n-1)]));
<a name="line_373"></a>   kpoints    := Vector(evalf(map(curvature_z0,points)));
<a name="line_374"></a>   mpoints    := Matrix([seq(evalf(subs({z[1]=points[i][1],z[2]=points[i][2]},monomials )),i=1..n)]);
<a name="line_375"></a>   lpoints    := Matrix([seq(evalf(subs({z[1]=points[i][1],z[2]=points[i][2]},laplacians)),i=1..n)]);
<a name="line_376"></a>   ones       := Vector([1$n]);
<a name="line_377"></a>
<a name="line_378"></a>   F := proc(a,b)
<a name="line_379"></a>    local vals,b0,i;
<a name="line_380"></a>
<a name="line_381"></a>    vals := mpoints . a;
<a name="line_382"></a>    b0 := rweights *~ (((kpoints - lpoints . a) *~ map(exp, (-2) * vals)) +~ ones);
<a name="line_383"></a>    for i from 1 to n do b[i] := b0[i]; od;
<a name="line_384"></a>   end;
<a name="line_385"></a>
<a name="line_386"></a>   J := proc(a,M)
<a name="line_387"></a>    local vals,mm,nn,i,j;
<a name="line_388"></a>
<a name="line_389"></a>    vals  := mpoints . a;
<a name="line_390"></a>    mm    := rweights *~ map(exp,(-2) * vals);
<a name="line_391"></a>    nn    := kpoints - lpoints . a;
<a name="line_392"></a>
<a name="line_393"></a>    for i from 1 to n do
<a name="line_394"></a>     for j from 1 to m do 
<a name="line_395"></a>      M[i,j] := mm[i] * (-2*mpoints[i,j]*nn[i] - lpoints[i,j]);
<a name="line_396"></a>     od;
<a name="line_397"></a>    od;
<a name="line_398"></a>   end;
<a name="line_399"></a>
<a name="line_400"></a>   f0 := this["log_rescale_z"](z);
<a name="line_401"></a>   if not(type(f0,polynom([z[1],z[2]]))) then
<a name="line_402"></a>    f0 := .266*z[1]^5-.414*z[1]^4+.353*z[1]^3+0.579e-1*z[1]^2+.421*z[1]-.367-.117*z[1]^4*z[2]-
<a name="line_403"></a>	   .294*z[1]^3*z[2]-0.952e-1*z[1]^2*z[2]-.197*z[1]*z[2]-.340*z[2]+.676*z[1]^3*z[2]^2+
<a name="line_404"></a>	   .461*z[1]^2*z[2]^2+.310*z[1]*z[2]^2+.232*z[2]^2-1.06*z[1]^2*z[2]^3-.745*z[1]*z[2]^3-
<a name="line_405"></a>	   .505*z[2]^3+1.08*z[1]*z[2]^4+1.13*z[2]^4-1.02*z[2]^5:
<a name="line_406"></a>   fi:
<a name="line_407"></a>
<a name="line_408"></a>   a0 := Vector(this["poly_to_vector",f0,d]);
<a name="line_409"></a>
<a name="line_410"></a>   E,A := op(LSSolve([m,n],F,objectivejacobian = J,initialpoint = a0));
<a name="line_411"></a>
<a name="line_412"></a>   this["rescaling_error"] := 2*E;
<a name="line_413"></a>   f0 := this["vector_to_poly",A];
<a name="line_414"></a>   this["set_rescale",f0];
<a name="line_415"></a>
<a name="line_416"></a>   NULL;
<a name="line_417"></a>  end
<a name="line_418"></a> ],
<a name="line_419"></a>
<a name="line_420"></a> ["Method","find_rescale_pade","This method attempts to find a rational approximation to the function $f$ such that $\\exp(2f)$ times the standard metric on $EX^*$ has curvature $-1$.  The argument @d@ specifies the degree of the numerator and denominator (in $z_1$ and $z_2$).",
<a name="line_421"></a>  proc(this,d::posint)
<a name="line_422"></a>   local f0,v0,F,E,A,m;
<a name="line_423"></a>
<a name="line_424"></a>   this["set_rescale_type","pade",d];
<a name="line_425"></a>
<a name="line_426"></a>   f0 := this["log_rescale_z"](z);
<a name="line_427"></a>   if not(type(f0,ratpoly(realcons,[z[1],z[2]]))) then
<a name="line_428"></a>    f0 := (-.36668113-.57931515*z[2]+.76405269*z[1]-0.43866952e-1*z[2]^2+.46321442*z[1]*z[2]-.32418737*z[1]^2)/(1.+.65920001*z[2]-.96109970*z[1]-0.51463451e-2*z[2]^2-.15212659*z[1]*z[2]+.19202748*z[1]^2);
<a name="line_429"></a>   fi:
<a name="line_430"></a>
<a name="line_431"></a>   v0 := this["pade_to_vector",f0,d];
<a name="line_432"></a>
<a name="line_433"></a>   F := proc(v)
<a name="line_434"></a>    global F_count,last_v;
<a name="line_435"></a>    local err;
<a name="line_436"></a>
<a name="line_437"></a>    last_v := convert(v,list):
<a name="line_438"></a>    
<a name="line_439"></a>    if type(F_count,integer) then
<a name="line_440"></a>     F_count := F_count + 1;
<a name="line_441"></a>    else
<a name="line_442"></a>     F_count := 1;
<a name="line_443"></a>    fi;
<a name="line_444"></a>    err := this["find_rescaling_error",this["vector_to_pade",v]];
<a name="line_445"></a>
<a name="line_446"></a>    if modp(F_count,10) = 0 then
<a name="line_447"></a>     print(["rescaling error",F_count,evalf[5](log[10](abs(err)))]);
<a name="line_448"></a>    fi;
<a name="line_449"></a>    if modp(F_count,1000) = 0 then
<a name="line_450"></a>     print(last_v);
<a name="line_451"></a>    fi;
<a name="line_452"></a>    
<a name="line_453"></a>    return err;
<a name="line_454"></a>   end;
<a name="line_455"></a>
<a name="line_456"></a>   m := this["rescale_dof"];
<a name="line_457"></a>   E,A := op(NLPSolve(m,F,initialpoint = Vector(v0)));
<a name="line_458"></a>
<a name="line_459"></a>   this["rescaling_error"] := E;
<a name="line_460"></a>   f0 := this["vector_to_pade",A];
<a name="line_461"></a>   this["set_rescale",f0];
<a name="line_462"></a>
<a name="line_463"></a>   NULL;
<a name="line_464"></a>  end
<a name="line_465"></a> ],
<a name="line_466"></a>
<a name="line_467"></a> ["Method","find_rescale_pade_alt","This method attempts to find a rational approximation to the function $f$ such that $\\exp(2f)$ times the standard metric on $EX^*$ has curvature $-1$.  The argument @d@ specifies the degree of the numerator and denominator (in $z_1$ and $z_2$).  This version is harder to understand than @find_rescale_pade@, but is much quicker.",
<a name="line_468"></a>  proc(this,d::posint)
<a name="line_469"></a>   local f0,uv,duv,mb,MB,QQ,np,ZL,WV,vals,C,P,Q,PM,QM,RM,KV0,OV,obj,E,A,i,c,u,v,p,q,r,s,si1,si2,si3,si4,e,FV,LV,KV,EV,JFU,JFV,JF,JLU,JLV,JL,JK,JE,QJ,RJ,fa,FVa,LVa,KVa,KVb,ea,eb,rep,IM,eps,JKa;
<a name="line_470"></a>
<a name="line_471"></a>   this["set_rescale_type","pade",d];
<a name="line_472"></a>
<a name="line_473"></a>   f0 := this["log_rescale_z"](z);
<a name="line_474"></a>   if not(type(f0,ratpoly(realcons,[z[1],z[2]]))) then
<a name="line_475"></a>    f0 := (-.36668113-.57931515*z[2]+.76405269*z[1]-0.43866952e-1*z[2]^2+.46321442*z[1]*z[2]-.32418737*z[1]^2)/(1.+.65920001*z[2]-.96109970*z[1]-0.51463451e-2*z[2]^2-.15212659*z[1]*z[2]+.19202748*z[1]^2);
<a name="line_476"></a>   fi:
<a name="line_477"></a>
<a name="line_478"></a>   uv := Vector(this["pade_to_vector",f0]);
<a name="line_479"></a>
<a name="line_480"></a>   MB := [seq(seq(z[1]^j*z[2]^(i-j),j=0..i),i=0..d)];
<a name="line_481"></a>   mb := nops(MB);
<a name="line_482"></a>   QQ := eval(this["quadrature_rule"]):
<a name="line_483"></a>   if not(type([QQ],[E_quadrature_rule])) then
<a name="line_484"></a>    error("No quadrature rule set");
<a name="line_485"></a>   fi;
<a name="line_486"></a>
<a name="line_487"></a>   np := QQ["num_points"];
<a name="line_488"></a>   ZL := [seq(QQ["points"][i]["z"],i=0..np-1)]:
<a name="line_489"></a>   WV := sqrt(2.) * Vector(map(sqrt,map(max,[seq(QQ["weights"][i],i=0..np-1)],10.^(-4))));
<a name="line_490"></a>   vals := (u) -> [seq(subs({z[1]=ZL[i][1],z[2]=ZL[i][2]},u),i=1..np)];
<a name="line_491"></a>
<a name="line_492"></a>   C := factor([seq(laplacian_z_C0[i]/laplacian_z_C0[0],i=1..5)]);
<a name="line_493"></a>   P[1] := (g) -> C[1]*diff(g,z[1]) + C[2]*diff(g,z[2]) + C[3]*diff(g,z[1],z[1]) + C[4]*diff(g,z[1],z[2]) + C[5]*diff(g,z[2],z[2]);
<a name="line_494"></a>   P[2] := (g) -> -C[1]*g -2*C[3]*diff(g,z[1]) - C[4]*diff(g,z[2]);
<a name="line_495"></a>   P[3] := (g) -> -C[2]*g -2*C[5]*diff(g,z[2]) - C[4]*diff(g,z[1]);
<a name="line_496"></a>   P[4] := (g) -> g;
<a name="line_497"></a>   P[5] := (g) -> g;
<a name="line_498"></a>   Q[2] := (h) -> diff(h,z[1]);
<a name="line_499"></a>   Q[3] := (h) -> diff(h,z[2]);
<a name="line_500"></a>   Q[4] := (h) -> -C[3]*diff(h,z[1],z[1])-C[4]*diff(h,z[1],z[2])-C[5]*diff(h,z[2],z[2]);
<a name="line_501"></a>   Q[5] := (h) -> 2*C[3]*diff(h,z[1])+2*C[4]*diff(h,z[2]);
<a name="line_502"></a>   Q[6] := (h) -> 2*C[5]*diff(h,z[2]);
<a name="line_503"></a>
<a name="line_504"></a>   for i from 1 to 5 do 
<a name="line_505"></a>    PM[i] := Transpose(Matrix(map(vals,map(P[i],MB))));
<a name="line_506"></a>   od:
<a name="line_507"></a>   for i from 2 to 6 do 
<a name="line_508"></a>    QM[i] := Transpose(Matrix(map(vals,map(Q[i],MB))));
<a name="line_509"></a>   od:
<a name="line_510"></a>   RM := Transpose(Matrix(map(vals,MB))): # matrix of values of monomials
<a name="line_511"></a>
<a name="line_512"></a>   KV0 := Vector(vals(curvature_z0(z))):  # vector of values of the curvature
<a name="line_513"></a>   OV := Vector(vals(1));                 # vector of ones
<a name="line_514"></a>
<a name="line_515"></a>   for c from 1 to 10 do
<a name="line_516"></a>    u := SubVector(uv,1..mb);       # vector of coefficients of numerator
<a name="line_517"></a>    v := SubVector(uv,mb..2*mb-1);
<a name="line_518"></a>    v[1] := 1;                      # vector of coefficients of denominator
<a name="line_519"></a>
<a name="line_520"></a>    for i from 1 to 5 do p[i] := PM[i] . u; od;
<a name="line_521"></a>    for i from 2 to 6 do q[i] := QM[i] . v; od;
<a name="line_522"></a>    r := RM . u;      # vector of values of numerator
<a name="line_523"></a>    s := RM . v;      # vector of values of denominator
<a name="line_524"></a>    si1 := 1 /~ s;    # vector of values of denominator^(-1)
<a name="line_525"></a>    si2 := si1 ^~ 2;  # vector of values of denominator^(-2)
<a name="line_526"></a>    si3 := si1 ^~ 3;  # vector of values of denominator^(-3)
<a name="line_527"></a>    si4 := si1 ^~ 4;  # vector of values of denominator^(-4)
<a name="line_528"></a>
<a name="line_529"></a>    FV := r *~ si1;   # vector of values of f = numerator/denominator
<a name="line_530"></a>    e := map(t -> exp(-2*t),FV); # vector of values of exp(-2f)
<a name="line_531"></a>
<a name="line_532"></a>    # Matrix of derivatives of values of f wrt coeffs of numerator
<a name="line_533"></a>    JFU := DiagonalMatrix(si1) . RM;
<a name="line_534"></a>
<a name="line_535"></a>    # Matrix of derivatives of values of f wrt coeffs of denominator
<a name="line_536"></a>    JFV := DiagonalMatrix(- r *~ si2) . RM;
<a name="line_537"></a>    
<a name="line_538"></a>    # Matrix of derivatives of values of f wrt all coefficients
<a name="line_539"></a>    JF := <JFU | SubMatrix(JFV,1..np,2..mb)>;
<a name="line_540"></a>
<a name="line_541"></a>    # Vector of values of Delta(f)
<a name="line_542"></a>    LV := (p[1] *~ si1) + 
<a name="line_543"></a>	  ((p[2] *~ q[2] + p[3] *~ q[3] + p[4] *~ q[4]) *~ si2) + 
<a name="line_544"></a>	  (p[5] *~ (q[2] *~ q[5] + q[3] *~ q[6]) *~ si3);
<a name="line_545"></a>
<a name="line_546"></a>    # Matrix of derivatives of values of Delta(f) wrt coeffs of numerator of f
<a name="line_547"></a>    JLU := DiagonalMatrix(si1) . PM[1] +
<a name="line_548"></a>	   add(DiagonalMatrix(q[i] *~ si2) . PM[i],i=2..4) +
<a name="line_549"></a>	   DiagonalMatrix((q[2] *~ q[5] + q[3] *~ q[6]) *~ si3) . PM[5];
<a name="line_550"></a>
<a name="line_551"></a>    # Matrix of derivatives of values of Delta(f) wrt coeffs of denominator of f
<a name="line_552"></a>    JLV := DiagonalMatrix((p[2] *~ si2) + (p[5] *~ q[5] *~ si3)) . QM[2] +
<a name="line_553"></a>	   DiagonalMatrix((p[3] *~ si2) + (p[5] *~ q[6] *~ si3)) . QM[3] +
<a name="line_554"></a>	   DiagonalMatrix(p[4] *~ si2) . QM[4] +
<a name="line_555"></a>	   DiagonalMatrix(p[5] *~ q[2] *~ si3) . QM[5] +
<a name="line_556"></a>	   DiagonalMatrix(p[5] *~ q[3] *~ si3) . QM[6] +
<a name="line_557"></a>	   DiagonalMatrix((-p[1] *~ si2) + add(-2 *~ p[i] *~ q[i],i=2..4) *~ si3 + (-3*p[5]*~(q[2]*~q[5]+q[3]*~q[6])*~si4)) . RM;
<a name="line_558"></a>
<a name="line_559"></a>    # Matrix of derivatives of values of Delta(f) wrt all coeffs of f
<a name="line_560"></a>    JL := <JLU | SubMatrix(JLV,1..np,2..mb)>;
<a name="line_561"></a>
<a name="line_562"></a>    # Vector of values of the rescaled curvature
<a name="line_563"></a>    KV := (KV0 - LV) *~ e;
<a name="line_564"></a>
<a name="line_565"></a>    # Matrix of derivatives of values of the rescaled curvature wrt coeffs of f
<a name="line_566"></a>    JK := DiagonalMatrix(-e) . JL + DiagonalMatrix(-2*KV) . JF;
<a name="line_567"></a>
<a name="line_568"></a>    fa := this["vector_to_pade",uv];
<a name="line_569"></a>    FVa := Vector(vals(fa));
<a name="line_570"></a>    LVa := Vector(vals(laplacian_z0(fa)));
<a name="line_571"></a>    KVa := Vector(vals(this["rescaled_curvature",fa]));
<a name="line_572"></a>    IM := IdentityMatrix(2*mb-1);
<a name="line_573"></a>    eps := 10.^(-20);
<a name="line_574"></a>
<a name="line_575"></a>    # Vector of weighted errors, whose 2-norm we want to minimise
<a name="line_576"></a>    EV := WV *~ (OV + KV);
<a name="line_577"></a>
<a name="line_578"></a>    # Matrix of derivatives of EV wrt coeffs of f
<a name="line_579"></a>    JE := DiagonalMatrix(WV) . JK;
<a name="line_580"></a>    
<a name="line_581"></a>    duv := LeastSquares(JE,-EV);
<a name="line_582"></a>    uv := uv + duv;
<a name="line_583"></a>    rep := evalf([c,log[10](Norm(EV,2)^2),log[10](Norm(duv,2))]);
<a name="line_584"></a>    rep := evalf[5](rep);
<a name="line_585"></a>    print(rep);
<a name="line_586"></a>   od:
<a name="line_587"></a>
<a name="line_588"></a>   this["rescaling_error"] := Norm(EV,2);
<a name="line_589"></a>   f0 := this["vector_to_pade",uv];
<a name="line_590"></a>   this["set_rescale",f0];
<a name="line_591"></a>
<a name="line_592"></a>   NULL;
<a name="line_593"></a>  end
<a name="line_594"></a> ],
<a name="line_595"></a>
<a name="line_596"></a> ["Method","vector_to_poly","This converts a vector (or list) of coefficients to a polynomial in $z_1$ and $z_2$.",
<a name="line_597"></a>  proc(this,v::{list,Vector})
<a name="line_598"></a>   local vv,n,m,d,B,fn,fd;
<a name="line_599"></a>
<a name="line_600"></a>   vv := v;
<a name="line_601"></a>   if type(vv,Vector) then vv := convert(vv,list); fi;
<a name="line_602"></a>   m := nops(vv);
<a name="line_603"></a>   d := this["rescale_degree"];
<a name="line_604"></a>   B := [seq(seq(z[1]^j*z[2]^(i-j),j=0..i),i=0..d)];
<a name="line_605"></a>   return add(vv[i] * B[i],i = 1..m);
<a name="line_606"></a>  end
<a name="line_607"></a> ],
<a name="line_608"></a>
<a name="line_609"></a> ["Method","poly_to_vector","This is inverse to the @vector_to_poly@ method.",
<a name="line_610"></a>  proc(this,f)
<a name="line_611"></a>   local d;
<a name="line_612"></a>   
<a name="line_613"></a>   d := this["rescale_degree"];
<a name="line_614"></a>   return [seq(seq(coeff(coeff(f,z[1],j),z[2],i-j),j=0..i),i=0..d)];
<a name="line_615"></a>  end
<a name="line_616"></a> ],
<a name="line_617"></a>
<a name="line_618"></a> ["Method","vector_to_pade","This converts a vector (or list) of coefficients to a rational function in $z_1$ and $z_2$.  The coefficients for the numerator are given first, followed by the coefficients for the denominator.  The constant term in the denominator is always taken to be 1, and this coefficient is omitted from the vector.",
<a name="line_619"></a>  proc(this,v::{list,Vector})
<a name="line_620"></a>   local vv,n,m,d,B,fn,fd;
<a name="line_621"></a>
<a name="line_622"></a>   vv := v;
<a name="line_623"></a>   if type(vv,Vector) then vv := convert(vv,list); fi;
<a name="line_624"></a>   n := nops(vv);
<a name="line_625"></a>   m := (n+1)/2;
<a name="line_626"></a>   return 
<a name="line_627"></a>    this["vector_to_poly",[seq(vv[i],i=1..m)]]/
<a name="line_628"></a>    this["vector_to_poly",[1,seq(vv[m+i],i=1..m-1)]];
<a name="line_629"></a>  end
<a name="line_630"></a> ],
<a name="line_631"></a>
<a name="line_632"></a> ["Method","pade_to_vector","This is inverse to the @vector_to_pade@ method.",
<a name="line_633"></a>  proc(this,f)
<a name="line_634"></a>   local fn,fd,d,vn,vd;
<a name="line_635"></a>
<a name="line_636"></a>   d := this["rescale_degree"];
<a name="line_637"></a>   fn := numer(f);
<a name="line_638"></a>   fd := denom(f);
<a name="line_639"></a>
<a name="line_640"></a>   vn := [seq(seq(coeff(coeff(fn,z[1],j),z[2],i-j),j=0..i),i=0..d)];
<a name="line_641"></a>   vd := [seq(seq(coeff(coeff(fd,z[1],j),z[2],i-j),j=0..i),i=0..d)];
<a name="line_642"></a>   vn := vn /~ vd[1];
<a name="line_643"></a>   vd := vd /~ vd[1];
<a name="line_644"></a>   return [op(vn),op(2..-1,vd)];
<a name="line_645"></a>  end
<a name="line_646"></a> ],
<a name="line_647"></a>
<a name="line_648"></a> ["Method","find_a_H","This method comutes the lengths (with respect to the rescaled metric) of the sides of the fundamental domain $F_{16}$.  It then calculates estimates of $a_H$ from these lengths, and averages them.",
<a name="line_649"></a>  proc(this)
<a name="line_650"></a>   local i,k,cz,cm,cs,R,cL,a_H_avg,aa;
<a name="line_651"></a>
<a name="line_652"></a>   cL := table();
<a name="line_653"></a>   aa := table();
<a name="line_654"></a>
<a name="line_655"></a>   for k in [0,1,3,5] do
<a name="line_656"></a>    cz := evalf(z_proj0(c_E0[k](t)));
<a name="line_657"></a>    cm := this["rescale_z"](cz);
<a name="line_658"></a>    cs := nm4(map(diff,c_E0[k](t),t));
<a name="line_659"></a>    R := F16_curve_limits[k];
<a name="line_660"></a>    cL[k] := evalf(Int(cs * cm,t = R));
<a name="line_661"></a>   od:
<a name="line_662"></a>
<a name="line_663"></a>   this["curve_lengths"] := eval(cL);
<a name="line_664"></a>
<a name="line_665"></a>   for i in [0,1,3,5] do
<a name="line_666"></a>    aa[i] := fsolve(side_length_H[i] = cL[i],a_H);
<a name="line_667"></a>   od:
<a name="line_668"></a>
<a name="line_669"></a>   this["curve_a_H_estimates"] := eval(aa);
<a name="line_670"></a>   this["a_H"] := (aa[0]+aa[1]+aa[3]+aa[5])/4;
<a name="line_671"></a>
<a name="line_672"></a>   this["a_H_discrepancy"] := 
<a name="line_673"></a>    max(seq(seq(abs(aa[i]-aa[j]),i in [0,1,3,5]),j in [0,1,3,5]));
<a name="line_674"></a>
<a name="line_675"></a>   return this["a_H"];
<a name="line_676"></a>  end
<a name="line_677"></a> ],
<a name="line_678"></a>
<a name="line_679"></a> ["Method","find_u","This calculates appropriate values for the @c_E_rescaled_length@ field and various related fields.  These involve various Fourier sin series; the parameter @d@ controls the number of terms in those series.",
<a name="line_680"></a>  proc(this,d)
<a name="line_681"></a>   local i,j,k,m,a,cz,cm,cs0,cs,P,AV,BM,CV,IL,FF,AA,BB;
<a name="line_682"></a>
<a name="line_683"></a>   this["u_degree"] := d;
<a name="line_684"></a>
<a name="line_685"></a>   this["c_E_rescaled_speed"] := table():
<a name="line_686"></a>   this["c_E_rescaled_length"] := table():
<a name="line_687"></a>   this["c_E_average_rescaled_speed"] := table():
<a name="line_688"></a>   this["u"] := table();
<a name="line_689"></a>   this["u_inv"] := table();
<a name="line_690"></a>
<a name="line_691"></a>   for k in [0,1,3,5] do
<a name="line_692"></a>    userinfo(7,genus2,sprintf("Analysing c[%d]",k)); 
<a name="line_693"></a>    if k = 0 then
<a name="line_694"></a>     m := 4;
<a name="line_695"></a>    elif k <= 4 then
<a name="line_696"></a>     m := 2;
<a name="line_697"></a>    else 
<a name="line_698"></a>     m := 1;
<a name="line_699"></a>    fi;
<a name="line_700"></a>
<a name="line_701"></a>    cz := expand(combine(simplify(z_proj0(c_E0[k](t))))):
<a name="line_702"></a>    cm := combine(simplify(this["log_rescale_z"](cz))):
<a name="line_703"></a>    cs0 := combine(factor(expand(combine(nm4(map(diff,c_E0[k](t),t)))))):
<a name="line_704"></a>    cs := cs0 * exp(cm):
<a name="line_705"></a>
<a name="line_706"></a>    userinfo(7,genus2,"Finding rescaled speed"); 
<a name="line_707"></a>
<a name="line_708"></a>    # We need to calculate a number of integrals.  For some reason, Maple does these very
<a name="line_709"></a>    # slowly, even though the integrands are quite tame.  It looks as though Maple may be
<a name="line_710"></a>    # trying to do some auxiliary symbolic analysis, and making very heavy weather of it.
<a name="line_711"></a>    # We therefore specify the integration method explicitly; this seems to fix the problem.
<a name="line_712"></a>
<a name="line_713"></a>    a[0] := evalf(int(cs,t=0..Pi/m,numeric=true,digits=90,method=_CCquad)/(Pi/m));
<a name="line_714"></a>    for j from 1 to d do
<a name="line_715"></a>     a[j] := evalf(int(cs * cos(m*j*t),t=0..Pi/m,numeric=true,digits=90,method=_CCquad)*2/(Pi/m));
<a name="line_716"></a>    od:
<a name="line_717"></a>
<a name="line_718"></a>    this["c_E_rescaled_speed"][k] := 
<a name="line_719"></a>     unapply(add(a[j] * cos(m*j*t),j=0..d),t);
<a name="line_720"></a>
<a name="line_721"></a>    this["c_E_average_rescaled_speed"][k] := a[0];
<a name="line_722"></a>
<a name="line_723"></a>    userinfo(7,genus2,"Finding rescaled length"); 
<a name="line_724"></a>    this["c_E_rescaled_length"][k] := 
<a name="line_725"></a>     unapply(int(this["c_E_rescaled_speed"][k](t),t),t);
<a name="line_726"></a>
<a name="line_727"></a>    this["u_inv"][k] :=
<a name="line_728"></a>     unapply(expand(this["c_E_rescaled_length"][k](t)/a[0]),t);
<a name="line_729"></a>
<a name="line_730"></a>    userinfo(7,genus2,"Finding u"); 
<a name="line_731"></a>    P := this["u_inv"][k](t) - t;
<a name="line_732"></a>    for j from 1 to d do
<a name="line_733"></a>     FF[j] := sin(m*j*(t+P));
<a name="line_734"></a>     AA[j] := evalf(int(FF[j] * P,t=0..Pi/m,numeric=true,digits=90,method=_CCquad));
<a name="line_735"></a>    od;
<a name="line_736"></a>
<a name="line_737"></a>    for i from 1 to d do
<a name="line_738"></a>     for j from i to d do 
<a name="line_739"></a>      BB[i,j] := evalf(int(FF[i]*FF[j],t=0..Pi/m,numeric=true,digits=90,method=_CCquad));
<a name="line_740"></a>      BB[j,i] := BB[i,j];
<a name="line_741"></a>     od:
<a name="line_742"></a>    od:
<a name="line_743"></a>
<a name="line_744"></a>    AV := Vector([seq(AA[i],i=1..d)]):
<a name="line_745"></a>    BM := Matrix([seq([seq(BB[i,j],j=1..d)],i=1..d)]):
<a name="line_746"></a>    CV := (1/BM) . AV;
<a name="line_747"></a>
<a name="line_748"></a>    IL := t - add(CV[i]*sin(m*i*t),i=1..d);
<a name="line_749"></a>
<a name="line_750"></a>    this["u"][k] := unapply(IL,t);
<a name="line_751"></a>   od;
<a name="line_752"></a>  end
<a name="line_753"></a> ],
<a name="line_754"></a>
<a name="line_755"></a> ["Method","find_v_series","Let $u$ be the function such that $q(c_{Hk}(t))=c_{Ek}(u(t))$.  This method finds an approximate power series for the function $v(z) = u(u^{-1}(t_0) + 2\\arctanh(z)/s_k) - t_0$, where $s_k$ is the speed of $c_{Hk}$.  This can be calculated without knowing $u^{-1}(t_0)$, and does not rely on the Fourier series for $u$ calculated by the @find_u@ method.",
<a name="line_756"></a>  proc(this,k,t0,d0)
<a name="line_757"></a>   local t,a,d,cs,css,vs,err,sol,cus;
<a name="line_758"></a>
<a name="line_759"></a>   cs := expand(evalf(convert(map(series,c_E0[k](t+t0),t=0,d0+1),polynom,t)));
<a name="line_760"></a>   css := nm4(map(diff,cs,t)) * this["rescale_x"](cs);
<a name="line_761"></a>   css := expand(evalf(convert(series(css,t=0,d0),polynom,t)));
<a name="line_762"></a>   vs := 0;
<a name="line_763"></a>   for d from 1 to d0 do 
<a name="line_764"></a>    vs := vs + a * t^d;
<a name="line_765"></a>    err := expand(convert(series(subs(t = vs,css) * diff(vs,t) - 2/(1-t^2),t=0,d),polynom,t));
<a name="line_766"></a>    sol := solve({coeff(err,t,d-1)},{a});
<a name="line_767"></a>    vs := subs(sol,vs);
<a name="line_768"></a>    vs := subs(a = 0,vs); # should not usually be needed
<a name="line_769"></a>   od:
<a name="line_770"></a>   return unapply(vs,t);
<a name="line_771"></a>  end
<a name="line_772"></a> ],
<a name="line_773"></a>
<a name="line_774"></a> ["Method","add_chart","This adds a chart object to the atlas, but the chart object contains no data at this stage.",
<a name="line_775"></a>  proc(this)
<a name="line_776"></a>   local C,n;
<a name="line_777"></a>   
<a name="line_778"></a>   n := this["num_charts"];
<a name="line_779"></a>   C := `new/EH_chart`();
<a name="line_780"></a>   C["vertex_index"] := NULL;
<a name="line_781"></a>   C["curve_index"]  := NULL;
<a name="line_782"></a>   C["grid_index"] := n;
<a name="line_783"></a>   this["charts"][n] := eval(C);
<a name="line_784"></a>   this["num_charts"] := n+1;
<a name="line_785"></a>   return eval(C);
<a name="line_786"></a>  end
<a name="line_787"></a> ],
<a name="line_788"></a>
<a name="line_789"></a> ["Method","add_vertex_chart","This adds a chart centred at $v_k$, with polynomial degree $d$, and calculates the coefficients and associated data.",
<a name="line_790"></a>  proc(this,k::integer,d::posint)
<a name="line_791"></a>   local C;
<a name="line_792"></a>   
<a name="line_793"></a>   C := eval(this["add_chart"]);
<a name="line_794"></a>   C["vertex_set",k,d,this];
<a name="line_795"></a>   return eval(C);
<a name="line_796"></a>  end
<a name="line_797"></a> ],
<a name="line_798"></a>
<a name="line_799"></a> ["Method","add_curve_chart","This adds a chart centred at $c_k(t0)$, with polynomial degree $d$, and calculates the coefficients and associated data.",
<a name="line_800"></a>  proc(this,k::integer,t0::RR0,d::posint)
<a name="line_801"></a>   local C;
<a name="line_802"></a>   
<a name="line_803"></a>   C := eval(this["add_chart"]);
<a name="line_804"></a>   C["curve_set",k,t0,d,this];
<a name="line_805"></a>   return eval(C);
<a name="line_806"></a>  end
<a name="line_807"></a> ],
<a name="line_808"></a>
<a name="line_809"></a> ["Method","add_square_chart","This adds a chart centred at $\\delta^{-1}(s0)$, with polynomial degree $d$, and calculates the coefficients and associated data.  Here $\\delta$ is the map @square_diffeo@.",
<a name="line_810"></a>  proc(this,s0::RR0_2,d::posint)
<a name="line_811"></a>   local C;
<a name="line_812"></a>   
<a name="line_813"></a>   C := eval(this["add_chart"]);
<a name="line_814"></a>   C["square_set",s0,d,this];
<a name="line_815"></a>   return eval(C);
<a name="line_816"></a>  end
<a name="line_817"></a> ],
<a name="line_818"></a>
<a name="line_819"></a> ["Method","add_centre_chart","This adds a chart centred at $x0$, with polynomial degree $d$, and calculates the coefficients and associated data.",
<a name="line_820"></a>  proc(this,x0::RR0_4,d::posint)
<a name="line_821"></a>   local C;
<a name="line_822"></a>   
<a name="line_823"></a>   C := eval(this["add_chart"]);
<a name="line_824"></a>   C["centre_set",x0,d,this];
<a name="line_825"></a>   return eval(C);
<a name="line_826"></a>  end
<a name="line_827"></a> ],
<a name="line_828"></a>
<a name="line_829"></a> ["Method","max_chart_error","",
<a name="line_830"></a>  proc(this)
<a name="line_831"></a>   local n,i,CC,err,worst_i,worst_err;
<a name="line_832"></a>   
<a name="line_833"></a>   err := 0;
<a name="line_834"></a>   worst_err := 0;
<a name="line_835"></a>   worst_i := NULL;
<a name="line_836"></a>   
<a name="line_837"></a>   n := this["num_charts"];
<a name="line_838"></a>   CC := eval(this["charts"]);
<a name="line_839"></a>   
<a name="line_840"></a>   for i from 0 to n-1 do
<a name="line_841"></a>    err := CC[i]["check"]["max"];
<a name="line_842"></a>    if err > worst_err then
<a name="line_843"></a>     worst_i := i;
<a name="line_844"></a>     worst_err := err;
<a name="line_845"></a>    fi;
<a name="line_846"></a>   od;
<a name="line_847"></a>
<a name="line_848"></a>   return [worst_i,worst_err];
<a name="line_849"></a>  end
<a name="line_850"></a> ],
<a name="line_851"></a> 
<a name="line_852"></a> ["Method","q_c","This evaluates $q(z)$ using the chart whose centre is as close as possible to $z$",
<a name="line_853"></a>  proc(this,z)
<a name="line_854"></a>   return this["q_c_aux",z][1];
<a name="line_855"></a>  end
<a name="line_856"></a> ],
<a name="line_857"></a>
<a name="line_858"></a> ["Method","q_c_aux","",
<a name="line_859"></a>  proc(this,z)
<a name="line_860"></a>   local T0,T1,z0,d0,i0,CC,n,C,d,i,x0,x1;
<a name="line_861"></a>
<a name="line_862"></a>   # Note: this assumes that a_H0 = this["a_H"], which we have generally 
<a name="line_863"></a>   # not assumed elsewhere.
<a name="line_864"></a>   T1,T0,z0 := op(retract_F16_H0_aux(z));
<a name="line_865"></a>
<a name="line_866"></a>   d0 := infinity;
<a name="line_867"></a>   i0 := NULL;
<a name="line_868"></a>   CC := this["charts"];
<a name="line_869"></a>   n := this["num_charts"];
<a name="line_870"></a>   if n = 0 then error("No charts"); fi;
<a name="line_871"></a>
<a name="line_872"></a>   for i from 0 to n-1 do
<a name="line_873"></a>    C := CC[i];
<a name="line_874"></a>    if C["beta"] <> NULL then
<a name="line_875"></a>     d := evalf(m_hyp_c(z0,C["beta"]));
<a name="line_876"></a>     if d < d0 then
<a name="line_877"></a>      d0 := d;
<a name="line_878"></a>      i0 := i;
<a name="line_879"></a>     fi;
<a name="line_880"></a>    fi;
<a name="line_881"></a>   od;
<a name="line_882"></a>
<a name="line_883"></a>   x0 := CC[i0]["q_c"](z0);
<a name="line_884"></a>   x1 := act_R4[G_inv(T1)](x0);
<a name="line_885"></a>   return [x1,T1,T0,z0,i0,x0];
<a name="line_886"></a>  end
<a name="line_887"></a> ],
<a name="line_888"></a>
<a name="line_889"></a> ["Method","q","This is the same as @q_c@, except that the argument is in $\\mathbb{R}^2$ instead of $\\mathbb{C}$",
<a name="line_890"></a>  proc(this,st)
<a name="line_891"></a>   this["q_c",st[1] + I * st[2]];
<a name="line_892"></a>  end
<a name="line_893"></a> ],
<a name="line_894"></a>
<a name="line_895"></a> ["Method","patching_error","This method looks for charts centred at a hyperbolic distance of at most @r0@ from @z0@. For each such chart, it calculates an approximation to @q(z0)@.  It returns a pair giving the number of different approximations, and the maximum discrepancy between them. ",
<a name="line_896"></a>  proc(this,z0,r0)
<a name="line_897"></a>   local i,j,n,m,d,CC,C,X;
<a name="line_898"></a>   
<a name="line_899"></a>   n := this["num_charts"];
<a name="line_900"></a>   CC := eval(this["charts"]);
<a name="line_901"></a>   X := NULL;
<a name="line_902"></a>   for i from 0 to n-1 do
<a name="line_903"></a>    C := eval(CC[i]);
<a name="line_904"></a>    if d_hyp_c(C["beta"],z0) < r0 then
<a name="line_905"></a>     X := X,C["q_c"](z0);
<a name="line_906"></a>    fi;
<a name="line_907"></a>   od;
<a name="line_908"></a>   X := [X];
<a name="line_909"></a>   m := nops(X);
<a name="line_910"></a>   if m <= 1 then
<a name="line_911"></a>    return [m,1];
<a name="line_912"></a>   else
<a name="line_913"></a>    d := max(seq(seq(d4f(X[i],X[j]),j=i+1..m),i=1..m-1));
<a name="line_914"></a>    return [m,d];
<a name="line_915"></a>   fi;
<a name="line_916"></a>  end
<a name="line_917"></a> ],
<a name="line_918"></a>
<a name="line_919"></a> ["Method","max_patching_error","",
<a name="line_920"></a>  proc(this,r)
<a name="line_921"></a>   local z,u,d_worst,m_worst,z_worst,SP;
<a name="line_922"></a>   
<a name="line_923"></a>   SP := this["H_samples"];
<a name="line_924"></a>   d_worst := 0;
<a name="line_925"></a>   z_worst := NULL;
<a name="line_926"></a>   m_worst := NULL;
<a name="line_927"></a>   for z in SP do
<a name="line_928"></a>    u := this["patching_error",z,r];
<a name="line_929"></a>    if u[2] > d_worst then
<a name="line_930"></a>     z_worst := z;
<a name="line_931"></a>     m_worst,d_worst := op(u);
<a name="line_932"></a>    fi;
<a name="line_933"></a>   od:
<a name="line_934"></a>   return [z_worst,m_worst,d_worst];
<a name="line_935"></a>  end
<a name="line_936"></a> ],
<a name="line_937"></a> 
<a name="line_938"></a> ["Method","make_H_samples",
<a name="line_939"></a>  "Set the @H_samples@ field to be the set of points of the form $(j + kI)/N$ that lie in $HF_{16}$",
<a name="line_940"></a>  proc(this,N::posint)
<a name="line_941"></a>   this["H_samples"] := select(is_in_F16_H0,[seq(seq(evalf((j+k*I)/N),j=0..N),k=0..N)]):
<a name="line_942"></a>   return this["H_samples"];
<a name="line_943"></a>  end
<a name="line_944"></a> ],
<a name="line_945"></a>
<a name="line_946"></a> ["Method","make_D_samples",
<a name="line_947"></a>  "Set the @D_samples@ field to be the set of points of the form $(j + kI)/N$ that lie in the first quadrant of the disc of radius $r$ centred at the origin.  Here $r$ should be big enough that the disc contains $HF_{16}$, but not much bigger than that.",
<a name="line_948"></a>  proc(this,N::posint,r::RR0)
<a name="line_949"></a>   this["D_samples"] := select(z -> abs(z) < r,[seq(seq(evalf((j+k*I)/N),j=0..N),k=0..N)]):
<a name="line_950"></a>   return this["D_samples"];
<a name="line_951"></a>  end
<a name="line_952"></a> ],
<a name="line_953"></a>
<a name="line_954"></a> ["Method","make_H_samples_q","Calculate the images of the H-sample points under the map $q\\colon\\Delta\\to EX^*$",
<a name="line_955"></a>  proc(this)
<a name="line_956"></a>   local z,X,i;
<a name="line_957"></a>   X := NULL;
<a name="line_958"></a>   # This is written in expanded form to make it easier to debug
<a name="line_959"></a>   for i from 1 to nops(this["H_samples"]) do
<a name="line_960"></a>    z := this["H_samples"][i];
<a name="line_961"></a>    X := X,this["q_c",z];
<a name="line_962"></a>   od;
<a name="line_963"></a>   this["H_samples_q"] := [X];
<a name="line_964"></a>  end
<a name="line_965"></a> ],
<a name="line_966"></a> 
<a name="line_967"></a> ["Method","make_D_samples_q","Calculate the images of the D-sample points under the map $q\\colon\\Delta\\to EX^*$",
<a name="line_968"></a>  proc(this)
<a name="line_969"></a>   local z,X,i;
<a name="line_970"></a>   X := NULL;
<a name="line_971"></a>   # This is written in expanded form to make it easier to debug
<a name="line_972"></a>   for i from 1 to nops(this["D_samples"]) do
<a name="line_973"></a>    z := this["D_samples"][i];
<a name="line_974"></a>    X := X,this["q_c",z];
<a name="line_975"></a>   od;
<a name="line_976"></a>   this["D_samples_q"] := [X];
<a name="line_977"></a>  end
<a name="line_978"></a> ],
<a name="line_979"></a>
<a name="line_980"></a> ["Method","set_chart_dist","Calculate distances between H-sample points and centres of charts",
<a name="line_981"></a>  proc(this)
<a name="line_982"></a>   local dd,i,j,n,m,CH,SP;
<a name="line_983"></a>
<a name="line_984"></a>   dd := table():
<a name="line_985"></a>   CH := eval(this["charts"]):
<a name="line_986"></a>   SP := this["H_samples"]:
<a name="line_987"></a>   n  := this["num_charts"];
<a name="line_988"></a>   m  := nops(SP);
<a name="line_989"></a>
<a name="line_990"></a>   for i from 1 to m do
<a name="line_991"></a>    for j from 0 to n-1 do
<a name="line_992"></a>     dd[i,j] := evalf(d_hyp_c(SP[i],CH[j]["beta"])):
<a name="line_993"></a>    od:
<a name="line_994"></a>   od:
<a name="line_995"></a>
<a name="line_996"></a>   this["chart_dist"] := eval(dd);
<a name="line_997"></a>  end
<a name="line_998"></a> ],
<a name="line_999"></a> 
<a name="line_1000"></a> ["Method","set_q_approx_pade","Find an approximation to $q(x+iy)$ by rational functions in $x$ and $y$.  The approximation will be equivariant with respect to $\\langle\\lambda,\\nu\\rangle$, and will minimize the error for points $x+iy$ in @D_samples@.",
<a name="line_1001"></a>  proc(this,dd)
<a name="line_1002"></a>   local n,m,d1,d3,d4,DS,ES,f0,F0,M1,M2,v,w,qa,ff;
<a name="line_1003"></a>   
<a name="line_1004"></a>   DS := map(C_to_R2,this["D_samples"]):
<a name="line_1005"></a>   ES := this["D_samples_q"]:
<a name="line_1006"></a>   m := nops(DS);
<a name="line_1007"></a>
<a name="line_1008"></a>   if type(dd,[posint,posint,posint]) then
<a name="line_1009"></a>    d1,d3,d4 := op(dd);
<a name="line_1010"></a>   elif type(dd,posint) then
<a name="line_1011"></a>    d1 := dd;
<a name="line_1012"></a>    d3 := dd;
<a name="line_1013"></a>    d4 := dd+3;
<a name="line_1014"></a>   else
<a name="line_1015"></a>    error("invalid degree specification");
<a name="line_1016"></a>   fi;
<a name="line_1017"></a>   
<a name="line_1018"></a>   # The first component of q is s[1] times a function of s[1]^2 and s[2]^2
<a name="line_1019"></a>   f0 := (s) -> [seq(seq(s[1]^(2*i)*s[2]^(2*j),j=0..d1-i),i=0..d1)]:
<a name="line_1020"></a>   n := nops(f0([0,0]));
<a name="line_1021"></a>   M1 := Matrix([seq(DS[i][1] *~ f0(DS[i]),i=1..m)]):
<a name="line_1022"></a>   M2 := Matrix([seq(ES[i][1] *~ f0(DS[i]),i=1..m)]):
<a name="line_1023"></a>   v,w := op(quot_approx(M1,M2));
<a name="line_1024"></a>   F0 := f0(s);
<a name="line_1025"></a>   qa[1] := add(w[i]*F0[i]*s[1],i=1..n)/add(v[i]*F0[i],i=1..n);
<a name="line_1026"></a>
<a name="line_1027"></a>   # The second component of q can be deduced from the first
<a name="line_1028"></a>   qa[2] := subs({s[1]=s[2],s[2]=-s[1]},qa[1]);
<a name="line_1029"></a>
<a name="line_1030"></a>   # The third component of q is a symmetric function of s[1]^2 and s[2]^2
<a name="line_1031"></a>   f0 := (s) -> [seq(seq((s[1]*s[2])^(2*i)*(s[1]^(2*j)+s[2]^(2*j)),j=0..d3-2*i),i=0..floor(d3/2))]:
<a name="line_1032"></a>   n := nops(f0([0,0]));
<a name="line_1033"></a>   M1 := Matrix([seq(f0(DS[i]),i=1..m)]):
<a name="line_1034"></a>   M2 := Matrix([seq(ES[i][3] *~ f0(DS[i]),i=1..m)]):
<a name="line_1035"></a>   v,w := op(quot_approx(M1,M2));
<a name="line_1036"></a>   F0 := f0(s);
<a name="line_1037"></a>   qa[3] := expand(add(w[i]*F0[i],i=1..n))/expand(add(v[i]*F0[i],i=1..n));
<a name="line_1038"></a>
<a name="line_1039"></a>   # The fourth component of q is (s[1]^2-s[2]^2) times a symmetric function of s[1]^2 and s[2]^2
<a name="line_1040"></a>   f0 := (s) -> [seq(seq((s[1]*s[2])^(2*i)*(s[1]^(2*j)+s[2]^(2*j)),j=0..d4-2*i),i=0..floor(d4/2))]:
<a name="line_1041"></a>   n := nops(f0([0,0]));
<a name="line_1042"></a>   M1 := Matrix([seq((DS[i][1]^2-DS[i][2]^2)*f0(DS[i]),i=1..m)]):
<a name="line_1043"></a>   M2 := Matrix([seq(ES[i][4] *~ f0(DS[i]),i=1..m)]):
<a name="line_1044"></a>   v,w := op(quot_approx(M1,M2));
<a name="line_1045"></a>   F0 := f0(s);
<a name="line_1046"></a>   qa[4] := expand(add(w[i]*F0[i]*(s[1]^2-s[2]^2),i=1..n))/expand(add(v[i]*F0[i],i=1..n));
<a name="line_1047"></a>
<a name="line_1048"></a>   this["q_approx"] := unapply([qa[1],qa[2],qa[3],qa[4]],s);
<a name="line_1049"></a>   NULL;
<a name="line_1050"></a>  end
<a name="line_1051"></a> ],
<a name="line_1052"></a> 
<a name="line_1053"></a> ["Method","set_q_approx_poly","Find an approximation to $q(x+iy)$ by polynomials in $x$ and $y$.  The approximation will be equivariant with respect to $\\langle\\lambda,\\nu\\rangle$, and will minimize the error for points $x+iy$ in @D_samples@.",
<a name="line_1054"></a>  proc(this,dd)
<a name="line_1055"></a>   local n,m,d1,d3,d4,DS,ES,f0,F0,M,Q,R,v,w,qa;
<a name="line_1056"></a>   
<a name="line_1057"></a>   DS := map(C_to_R2,this["D_samples"]):
<a name="line_1058"></a>   ES := this["D_samples_q"]:
<a name="line_1059"></a>   m := nops(DS);
<a name="line_1060"></a>
<a name="line_1061"></a>   if type(dd,[posint,posint,posint]) then
<a name="line_1062"></a>    d1,d3,d4 := op(dd);
<a name="line_1063"></a>   elif type(dd,posint) then
<a name="line_1064"></a>    d1 := dd;
<a name="line_1065"></a>    d3 := dd;
<a name="line_1066"></a>    d4 := dd+3;
<a name="line_1067"></a>   else
<a name="line_1068"></a>    error("invalid degree specification");
<a name="line_1069"></a>   fi;
<a name="line_1070"></a>   
<a name="line_1071"></a>   # The first component of q is s[1] times a function of s[1]^2 and s[2]^2
<a name="line_1072"></a>   f0 := (s) -> [seq(seq(s[1]^(2*i+1)*s[2]^(2*j),j=0..d1-i),i=0..d1)]:
<a name="line_1073"></a>   n := nops(f0([0,0]));
<a name="line_1074"></a>   M := Matrix(map(f0,DS));
<a name="line_1075"></a>   Q,R := QRDecomposition(M);
<a name="line_1076"></a>   v := Vector(map(u -> u[1],ES));
<a name="line_1077"></a>   w := (1/R) . Transpose(Q) . v;
<a name="line_1078"></a>   F0 := f0(s);
<a name="line_1079"></a>   qa[1] := add(w[i]*F0[i],i=1..n);
<a name="line_1080"></a>
<a name="line_1081"></a>   # The second component of q can be deduced from the first
<a name="line_1082"></a>   qa[2] := subs({s[1]=s[2],s[2]=-s[1]},qa[1]);
<a name="line_1083"></a>
<a name="line_1084"></a>   # The third component of q is a symmetric function of s[1]^2 and s[2]^2
<a name="line_1085"></a>   f0 := (s) -> [seq(seq((s[1]*s[2])^(2*i)*(s[1]^(2*j)+s[2]^(2*j)),
<a name="line_1086"></a>                         j=0..d3-2*i),i=0..floor(d3/2))]:
<a name="line_1087"></a>   n := nops(f0([0,0]));
<a name="line_1088"></a>   M := Matrix(map(f0,DS));
<a name="line_1089"></a>   Q,R := QRDecomposition(M);
<a name="line_1090"></a>   v := Vector(map(u -> u[3],ES));
<a name="line_1091"></a>   w := (1/R) . Transpose(Q) . v;
<a name="line_1092"></a>   F0 := f0(s);
<a name="line_1093"></a>   qa[3] := add(w[i]*F0[i],i=1..n);
<a name="line_1094"></a>
<a name="line_1095"></a>   # The fourth component of q is (s[1]^2-s[2]^2) times a symmetric
<a name="line_1096"></a>   # function of s[1]^2 and s[2]^2
<a name="line_1097"></a>   f0 := (s) -> [seq(seq((s[1]^2-s[2]^2)*(s[1]*s[2])^(2*i)*(s[1]^(2*j)+s[2]^(2*j)),
<a name="line_1098"></a>                         j=0..d4-2*i),i=0..floor(d4/2))]:
<a name="line_1099"></a>   n := nops(f0([0,0]));
<a name="line_1100"></a>   M := Matrix(map(f0,DS));
<a name="line_1101"></a>   Q,R := QRDecomposition(M);
<a name="line_1102"></a>   v := Vector(map(u -> u[4],ES));
<a name="line_1103"></a>   w := (1/R) . Transpose(Q) . v;
<a name="line_1104"></a>   F0 := f0(s);
<a name="line_1105"></a>   qa[4] := add(w[i]*F0[i],i=1..n);
<a name="line_1106"></a>
<a name="line_1107"></a>   this["q_approx"] := unapply([qa[1],qa[2],qa[3],qa[4]],s);
<a name="line_1108"></a>   NULL;
<a name="line_1109"></a>  end
<a name="line_1110"></a> ],
<a name="line_1111"></a> 
<a name="line_1112"></a> ["Method","set_q_approx_fourier","This sets the fields @fourier_r_max@, @fourier_m@, @fourier_k@, @fourier_r@, @fourier_v@, @fourier_a@, @fourier_qr@ and @fourier_a_spline@.  See the documentation of those fields for more details.",
<a name="line_1113"></a>  proc(this,r_max,m,k)
<a name="line_1114"></a>   local R,V,A,i,j,l,r,t,z,v,w,s;
<a name="line_1115"></a>
<a name="line_1116"></a>   this["fourier_r_max"] := evalf(r_max);
<a name="line_1117"></a>   this["fourier_m"]     := m;
<a name="line_1118"></a>   this["fourier_k"]     := k;
<a name="line_1119"></a>   R := evalf([seq(r_max * sin(i/m*Pi/2),i = 1.. m)]);
<a name="line_1120"></a>   V := table();
<a name="line_1121"></a>   A := table();
<a name="line_1122"></a>   this["fourier_r"] := R;
<a name="line_1123"></a>   this["fourier_v"] := eval(V);
<a name="line_1124"></a>   this["fourier_a"] := eval(A);
<a name="line_1125"></a>   for r in R do
<a name="line_1126"></a>    userinfo(5,genus2,sprintf("Evaluating q with r=%A\n",r));
<a name="line_1127"></a>    for i from 0 to 2^k - 1 do
<a name="line_1128"></a>     t := i/2^k;
<a name="line_1129"></a>     z := evalf(r * exp(t * 2*Pi*I));
<a name="line_1130"></a>     V[r,t] := this["q_c",z];
<a name="line_1131"></a>    od;
<a name="line_1132"></a>   od;
<a name="line_1133"></a>
<a name="line_1134"></a>   this["fourier_qr"] := table():
<a name="line_1135"></a>
<a name="line_1136"></a>   for r in R do
<a name="line_1137"></a>    userinfo(5,genus2,sprintf("Finding Fourier transform with r=%A\n",r));
<a name="line_1138"></a>
<a name="line_1139"></a>    for l in [1,3,4] do
<a name="line_1140"></a>     v := Vector([seq(V[r,i/2^k][l],i=0..2^k-1)]);
<a name="line_1141"></a>     w := map(Re,convert(SignalProcessing[FFT](v,normalization=full),list));
<a name="line_1142"></a>     if l = 1 then
<a name="line_1143"></a>      for j from 0 to 2^(k-2)-1 do
<a name="line_1144"></a>       A[1,j,r] := 2*w[(2*j+1) + 1]; 
<a name="line_1145"></a>       A[2,j,r] := (-1)^j * A[1,j,r];
<a name="line_1146"></a>      od;
<a name="line_1147"></a>     elif l = 3 then
<a name="line_1148"></a>      for j from 0 to 2^(k-3)-1 do
<a name="line_1149"></a>       A[3,j,r] := 2*w[(4*j)+1];
<a name="line_1150"></a>      od;
<a name="line_1151"></a>     elif l = 4 then
<a name="line_1152"></a>      for j from 0 to 2^(k-3)-1 do
<a name="line_1153"></a>       A[4,j,r] := 2*w[(4*j+2)+1];
<a name="line_1154"></a>      od;
<a name="line_1155"></a>     fi;
<a name="line_1156"></a>    od;
<a name="line_1157"></a>
<a name="line_1158"></a>    this["fourier_qr"][1,r] := 
<a name="line_1159"></a>     unapply(add(A[1,i,r]*cos((2*i+1)*s),i=0..2^(k-2)-1),s):
<a name="line_1160"></a>
<a name="line_1161"></a>    this["fourier_qr"][2,r] := 
<a name="line_1162"></a>     unapply(add(A[2,i,r]*sin((2*i+1)*s),i=0..2^(k-2)-1),s):
<a name="line_1163"></a>
<a name="line_1164"></a>    this["fourier_qr"][3,r] := 
<a name="line_1165"></a>     unapply(A[3,0,r]/2 + 
<a name="line_1166"></a>             add(A[3,i,r]*cos((4*i)*s),i=1..2^(k-3)-1),s):
<a name="line_1167"></a>
<a name="line_1168"></a>    this["fourier_qr"][4,r] := 
<a name="line_1169"></a>     unapply(add(A[4,i,r]*cos((4*i+2)*s),i=0..2^(k-3)-1),s):
<a name="line_1170"></a>   od;
<a name="line_1171"></a>
<a name="line_1172"></a>   this["set_fourier_a_spline"];
<a name="line_1173"></a>   
<a name="line_1174"></a>   NULL;
<a name="line_1175"></a>  end
<a name="line_1176"></a> ],
<a name="line_1177"></a>
<a name="line_1178"></a> ["Method","set_fourier_a_spline","",
<a name="line_1179"></a>  proc(this)
<a name="line_1180"></a>   local i,k,A,R,S;
<a name="line_1181"></a>   
<a name="line_1182"></a>   this["fourier_a_spline"] := table():
<a name="line_1183"></a>
<a name="line_1184"></a>   k := this["fourier_k"];
<a name="line_1185"></a>   A := eval(this["fourier_a"]);
<a name="line_1186"></a>   R := eval(this["fourier_r"]);
<a name="line_1187"></a>   S := table():
<a name="line_1188"></a>   
<a name="line_1189"></a>   for i from 0 to 2^(k-2)-1 do
<a name="line_1190"></a>    S[1,i] := unapply(CurveFitting[Spline]([seq([r,A[1,i,r]],r in R)],t),t);
<a name="line_1191"></a>    S[2,i] := unapply((-1)^i*S[1,i](t),t);
<a name="line_1192"></a>   od;
<a name="line_1193"></a>
<a name="line_1194"></a>   for i from 0 to 2^(k-3)-1 do
<a name="line_1195"></a>    S[3,i] := unapply(CurveFitting[Spline]([seq([r,A[3,i,r]],r in R)],t),t);
<a name="line_1196"></a>    S[4,i] := unapply(CurveFitting[Spline]([seq([r,A[4,i,r]],r in R)],t),t);
<a name="line_1197"></a>   od:
<a name="line_1198"></a>
<a name="line_1199"></a>   this["fourier_a_spline"] := eval(S):
<a name="line_1200"></a>   NULL;
<a name="line_1201"></a>  end
<a name="line_1202"></a> ],
<a name="line_1203"></a>
<a name="line_1204"></a> ["Method","q_fourier","",
<a name="line_1205"></a>  proc(this,s::RR0_2)
<a name="line_1206"></a>   local r,theta,S,k,x;
<a name="line_1207"></a>   r := evalf(sqrt(s[1]^2+s[2]^2));
<a name="line_1208"></a>   theta := arctan(s[2],s[1]);
<a name="line_1209"></a>   S := eval(this["fourier_a_spline"]);
<a name="line_1210"></a>   k := this["fourier_k"];
<a name="line_1211"></a>   x := table();
<a name="line_1212"></a>
<a name="line_1213"></a>   x[1] := add(S[1,i](r)*cos((2*i+1)*theta),i = 0..2^(k-2)-1); 
<a name="line_1214"></a>   x[2] := add(S[2,i](r)*sin((2*i+1)*theta),i = 0..2^(k-2)-1); 
<a name="line_1215"></a>   x[3] := S[3,0](r)/2 +
<a name="line_1216"></a>           add(S[3,i](r)*cos((4*i  )*theta),i = 1..2^(k-3)-1); 
<a name="line_1217"></a>   x[4] := add(S[4,i](r)*cos((4*i+2)*theta),i = 0..2^(k-3)-1);
<a name="line_1218"></a>   return [x[1],x[2],x[3],x[4]];
<a name="line_1219"></a>  end
<a name="line_1220"></a> ],
<a name="line_1221"></a> 
<a name="line_1222"></a> ["Method","q_fourier_c","",
<a name="line_1223"></a>  proc(this,z::CC0)
<a name="line_1224"></a>   this["q_fourier",C_to_R2(z)];
<a name="line_1225"></a>  end
<a name="line_1226"></a> ],
<a name="line_1227"></a> 
<a name="line_1228"></a> ["Method","set_square_q_inv_a","",
<a name="line_1229"></a>  proc(this,d::posint)
<a name="line_1230"></a>   local SP,pSP,rSP,ppr,ppi,err,sol,a,b,s;
<a name="line_1231"></a>   
<a name="line_1232"></a>   SP := this["H_samples"];
<a name="line_1233"></a>   if not(type(this["H_samples_q"],list) and
<a name="line_1234"></a>          nops(this["H_samples_q"]) = nops(this["H_samples"])) then
<a name="line_1235"></a>    this["make_H_samples_q"];
<a name="line_1236"></a>   fi;
<a name="line_1237"></a>   pSP := this["H_samples_q"];
<a name="line_1238"></a>   rSP := evalf(map(square_diffeo_E0,pSP));
<a name="line_1239"></a>
<a name="line_1240"></a>   ppr := unapply(add(add(a[i,j]*s[1]^i*s[2]^j,j=0..d-i),i=0..d),s):
<a name="line_1241"></a>   err := add((ppr(rSP[i]) - Re(SP[i]))^2,i=1..nops(SP)):
<a name="line_1242"></a>   sol := solve({seq(diff(err,v),v in indets(err))}):
<a name="line_1243"></a>   ppr := unapply(subs(sol,ppr(s)),s):
<a name="line_1244"></a>
<a name="line_1245"></a>   ppi := unapply(add(add(b[i,j]*s[1]^i*s[2]^j,j=0..d-i),i=0..d),s):
<a name="line_1246"></a>   err := add((ppi(rSP[i]) - Im(SP[i]))^2,i=1..nops(SP)):
<a name="line_1247"></a>   sol := solve({seq(diff(err,v),v in indets(err))}):
<a name="line_1248"></a>   ppi := unapply(subs(sol,ppi(s)),s):
<a name="line_1249"></a>
<a name="line_1250"></a>   this["square_q_inv"] := unapply(ppr(s) + I * ppi(s),s):
<a name="line_1251"></a>  end
<a name="line_1252"></a> ],
<a name="line_1253"></a>
<a name="line_1254"></a> ["Method","square_q_inv_search","",
<a name="line_1255"></a>  proc(this,s0::RR0_2)
<a name="line_1256"></a>   local x0,x1,x2,x3,s1,u0,v0,w0,w1,z0,t0,t1,i0,CH,T0,T1;
<a name="line_1257"></a>   x0 := square_diffeo_E0_inverse_search(s0);
<a name="line_1258"></a>   u0,v0 := op(tangent_frame_a(x0));
<a name="line_1259"></a>   z0 := this["square_q_inv"](s0);
<a name="line_1260"></a>   x1,T1,T0,w0,i0,x2 := op(this["q_c_aux",z0]);
<a name="line_1261"></a>   CH := eval(this["charts"][i0]):
<a name="line_1262"></a>   t0 := C_to_R2(CH["m_inv_c"](w0));
<a name="line_1263"></a>   t1 := subs(fsolve([dp4(u0,CH["p"](t)),dp4(v0,CH["p"](t))],{t[1]=t0[1],t[2]=t0[2]}),[t[1],t[2]]);
<a name="line_1264"></a>   w1 := R2_to_C(CH["m"](t1));
<a name="line_1265"></a>   return w1;
<a name="line_1266"></a>  end
<a name="line_1267"></a> ],
<a name="line_1268"></a> 
<a name="line_1269"></a> ["Method","set_square_q_inv_b","",
<a name="line_1270"></a>  proc(this,n::posint,d::posint)
<a name="line_1271"></a>   local i,j,CP,SP,SQ,rSQ,iSQ,a,ppr,ppi,err,sol;
<a name="line_1272"></a>
<a name="line_1273"></a>   CP := table():
<a name="line_1274"></a>   for i from 1 to n do
<a name="line_1275"></a>    CP[i] := evalf(1/2 + cos((2*n-2*i+1)*Pi/(2*n))/2):
<a name="line_1276"></a>   od:
<a name="line_1277"></a>   SP := [seq(seq([CP[i],CP[j]],j=1..n),i=1..n)];
<a name="line_1278"></a>   SQ := map(s -> this["square_q_inv_search",s],SP);
<a name="line_1279"></a>   rSQ := map(Re,SQ);
<a name="line_1280"></a>   iSQ := map(Im,SQ);
<a name="line_1281"></a>
<a name="line_1282"></a>   ppr := unapply(add(add(a[i,j]*s[1]^i*s[2]^j,j=0..d-i),i=0..d),s):
<a name="line_1283"></a>   err := add((ppr(SP[i]) - rSQ[i])^2,i=1..nops(SP)):
<a name="line_1284"></a>   sol := solve({seq(diff(err,v),v in indets(err))}):
<a name="line_1285"></a>   ppr := unapply(subs(sol,ppr(s)),s):
<a name="line_1286"></a>
<a name="line_1287"></a>   ppi := unapply(add(add(b[i,j]*s[1]^i*s[2]^j,j=0..d-i),i=0..d),s):
<a name="line_1288"></a>   err := add((ppi(SP[i]) - iSQ[i])^2,i=1..nops(SP)):
<a name="line_1289"></a>   sol := solve({seq(diff(err,v),v in indets(err))}):
<a name="line_1290"></a>   ppi := unapply(subs(sol,ppi(s)),s):
<a name="line_1291"></a>
<a name="line_1292"></a>   this["square_q_inv"] := unapply(ppr(s) + I * ppi(s),s):
<a name="line_1293"></a>  end
<a name="line_1294"></a> ],
<a name="line_1295"></a>
<a name="line_1296"></a> ["Method","add_edge","",
<a name="line_1297"></a>  proc(this,i,j)
<a name="line_1298"></a>   local n,m,E,Ci,Cj,ci,cj,vi,vj;
<a name="line_1299"></a>   n := this["num_charts"];
<a name="line_1300"></a>   m := this["num_edges"];
<a name="line_1301"></a>
<a name="line_1302"></a>   if i < 0 or i >= n or j < 0 or j >= n then
<a name="line_1303"></a>    error("Chart indices out of bounds");
<a name="line_1304"></a>   fi;
<a name="line_1305"></a>
<a name="line_1306"></a>   if i = j then 
<a name="line_1307"></a>    error("Start and end indices are the same");
<a name="line_1308"></a>   fi;
<a name="line_1309"></a>
<a name="line_1310"></a>   E := `new/EH_atlas_edge`();
<a name="line_1311"></a>   E["curve_index"] := NULL;
<a name="line_1312"></a>   E["grid_index"] := m;
<a name="line_1313"></a>   E["start_index"] := i;
<a name="line_1314"></a>   E["end_index"]   := j;
<a name="line_1315"></a>   this["edges"][m] := eval(E);
<a name="line_1316"></a>   this["num_edges"] := m+1;
<a name="line_1317"></a>
<a name="line_1318"></a>   Ci := eval(this["charts"][i]):
<a name="line_1319"></a>   Cj := eval(this["charts"][j]):
<a name="line_1320"></a>   ci := Ci["curve_index"];
<a name="line_1321"></a>   cj := Cj["curve_index"];
<a name="line_1322"></a>   vi := Ci["vertex_index"];
<a name="line_1323"></a>   vj := Cj["vertex_index"];
<a name="line_1324"></a>
<a name="line_1325"></a>   if   ([ci] = [0] or [vi] = [3] or [vi] = [6] ) and
<a name="line_1326"></a>        ([cj] = [0] or [vj] = [3] or [vj] = [6] ) then
<a name="line_1327"></a>    E["curve_index"] := 0;
<a name="line_1328"></a>   elif ([ci] = [1] or [vi] = [0] or [vi] = [6] ) and
<a name="line_1329"></a>        ([cj] = [1] or [vj] = [0] or [vj] = [6] ) then
<a name="line_1330"></a>    E["curve_index"] := 1;
<a name="line_1331"></a>   elif ([ci] = [3] or [vi] = [3] or [vi] = [11]) and
<a name="line_1332"></a>        ([cj] = [3] or [vj] = [3] or [vj] = [11]) then
<a name="line_1333"></a>    E["curve_index"] := 3;
<a name="line_1334"></a>   elif ([ci] = [5] or [vi] = [0] or [vi] = [11]) and
<a name="line_1335"></a>        ([cj] = [5] or [vj] = [0] or [vj] = [11]) then
<a name="line_1336"></a>    E["curve_index"] := 5;
<a name="line_1337"></a>   fi;
<a name="line_1338"></a>
<a name="line_1339"></a>   return m;    
<a name="line_1340"></a>  end
<a name="line_1341"></a> ],
<a name="line_1342"></a>
<a name="line_1343"></a> ["Method","set_edge_lengths","Set the @H_length@ field of each edge, by assuming that the charts are exactly isometric.",
<a name="line_1344"></a>  proc(this)
<a name="line_1345"></a>   local i,j,k,m,Ci,Cj,xi,xj,zi,zj,E;
<a name="line_1346"></a>
<a name="line_1347"></a>   m := this["num_edges"];
<a name="line_1348"></a>
<a name="line_1349"></a>   for k from 0 to m-1 do
<a name="line_1350"></a>    E := eval(this["edges"][k]);
<a name="line_1351"></a>    i := E["start_index"];
<a name="line_1352"></a>    j := E["end_index"];
<a name="line_1353"></a>    Ci := eval(this["charts"][i]);
<a name="line_1354"></a>    Cj := eval(this["charts"][j]);
<a name="line_1355"></a>    xi := Ci["centre"];
<a name="line_1356"></a>    xj := Cj["centre"];
<a name="line_1357"></a>    zi := Cj["p_inv_c",xi];
<a name="line_1358"></a>    zj := Ci["p_inv_c",xj];
<a name="line_1359"></a>    E["start_z"] := zi;
<a name="line_1360"></a>    E["end_z"]   := zj;
<a name="line_1361"></a>    E["EH_length"] := arctanh(abs(zi)) + arctanh(abs(zj));
<a name="line_1362"></a>    if Ci["beta"] <> NULL and Cj["beta"] <> NULL then
<a name="line_1363"></a>     E["H_length"] := d_hyp_c(Ci["beta"],Cj["beta"]);
<a name="line_1364"></a>    fi;
<a name="line_1365"></a>   od;
<a name="line_1366"></a>   NULL;
<a name="line_1367"></a>  end
<a name="line_1368"></a> ],
<a name="line_1369"></a>
<a name="line_1370"></a> ["Method","set_beta_approx","This method sets the @beta@ field for each chart that is not centred on an edge of $F_{16}$.  If $q$ is the standard map $\\Delta\\to EX^*$, then $q(\\beta)$ should be the centre of the chart.  If we have already set the @square_q_inv@ field (to an approximation of @(square_diffeo_E o q)^(-1)@) then we use that.  Otherwise, we use @square_diffeo_H^(-1) o square_diffeo_E@.  If the $\\lambda$ field has already been set, then we also put $\\alpha = -\\beta/\\lambda$.",
<a name="line_1371"></a>  proc(this)
<a name="line_1372"></a>   local i,n,sqi,C;
<a name="line_1373"></a>
<a name="line_1374"></a>   n := this["num_charts"];
<a name="line_1375"></a>   sqi := this["square_q_inv"];
<a name="line_1376"></a>   if sqi = NULL then
<a name="line_1377"></a>    sqi := eval(square_diffeo_H0_inverse);
<a name="line_1378"></a>   fi;
<a name="line_1379"></a>   for i from 0 to n-1 do
<a name="line_1380"></a>    C := eval(this["charts"][i]);
<a name="line_1381"></a>    if C["curve_index"] = NULL then
<a name="line_1382"></a>     C["beta"] := sqi(evalf(C["square_centre"]));
<a name="line_1383"></a>     if C["lambda"] <> NULL then
<a name="line_1384"></a>      C["alpha"] := - C["beta"]/C["lambda"];
<a name="line_1385"></a>     fi;
<a name="line_1386"></a>    fi;
<a name="line_1387"></a>   od;
<a name="line_1388"></a>  end
<a name="line_1389"></a> ],
<a name="line_1390"></a>
<a name="line_1391"></a> ["Method","optimize_lambda","This method assumes that we have a value of $\\beta$ for each chart, and then chooses the best possible values of $\\lambda$ consistent with the values of $\\beta$.  It then sets $\\alpha = -\\beta/\\lambda$.  It does not alter the values of $\\alpha$, $\\beta$ or $\\lambda$ for charts centred on the edges of $F_{16}$, because in those cases we have other methods that are considered to be more reliable.",
<a name="line_1392"></a>  proc(this)
<a name="line_1393"></a>   local i,j,k,n,m,CC,EE,C,E,link,ep,lm,L;
<a name="line_1394"></a>
<a name="line_1395"></a>   n := this["num_charts"];
<a name="line_1396"></a>   m := this["num_edges"];
<a name="line_1397"></a>   CC := eval(this["charts"]);
<a name="line_1398"></a>   EE := eval(this["edges"]);
<a name="line_1399"></a>
<a name="line_1400"></a>   link := table();
<a name="line_1401"></a>   for i from 0 to n-1 do link[i] := {}; od;
<a name="line_1402"></a>
<a name="line_1403"></a>   for i from 0 to m do
<a name="line_1404"></a>    j := EE[i]["start_index"];
<a name="line_1405"></a>    k := EE[i]["end_index"];
<a name="line_1406"></a>    link[j] := {op(link[j]),i};
<a name="line_1407"></a>    link[k] := {op(link[k]),i};
<a name="line_1408"></a>   od;
<a name="line_1409"></a>
<a name="line_1410"></a>   for i from 0 to n-1 do
<a name="line_1411"></a>    C := eval(CC[i]);
<a name="line_1412"></a>    L := NULL;
<a name="line_1413"></a>    for k in link[i] do
<a name="line_1414"></a>     E := EE[k];
<a name="line_1415"></a>     if i = E["start_index"] then
<a name="line_1416"></a>      j  := E["end_index"];
<a name="line_1417"></a>      ep := E["end_z"];
<a name="line_1418"></a>     else
<a name="line_1419"></a>      j  := E["start_index"];
<a name="line_1420"></a>      ep := E["start_z"];
<a name="line_1421"></a>     fi;
<a name="line_1422"></a>     lm := (CC[j]["beta"]-C["beta"])/(1-conjugate(C["beta"])*CC[j]["beta"])/ep;
<a name="line_1423"></a>     lm := lm/abs(lm);
<a name="line_1424"></a>     L := L,lm;
<a name="line_1425"></a>    od;
<a name="line_1426"></a>    lm := `+`(L)/nops([L]);
<a name="line_1427"></a>    lm := lm/abs(lm);
<a name="line_1428"></a>    C["lambda"] := lm;
<a name="line_1429"></a>    C["alpha"] := -C["beta"]/C["lambda"];
<a name="line_1430"></a>   od;
<a name="line_1431"></a>   NULL;
<a name="line_1432"></a>  end
<a name="line_1433"></a> ],
<a name="line_1434"></a>
<a name="line_1435"></a> ["Method","optimize_beta","This method adjusts the $\\beta$ fields for all charts that are not centred on an edge of $F_{16}$.  It attempts to ensure that whenever any two charts are joined by an edge, the hyperbolic distance in $\\Delta$ between the coresponding $\\beta$ values is the same as the distance in $EX^*$ between the corresponding centres, measured using the rescaled metric.  It then calls the @optimize_lambda@ method to set $\\lambda$ and $\\alpha$.",
<a name="line_1436"></a>  proc(this)
<a name="line_1437"></a>   local i,j,k,n,m,C,E,CC,EE,Z,obj,objjac,mh,mh1,mh2,mh3,mh4,
<a name="line_1438"></a>    movable_chart_indices,movable_chart_beta,chart_lookup,
<a name="line_1439"></a>    pinned_edge_indices,pinned_edge_ends,pinned_edge_lengths,
<a name="line_1440"></a>    movable_edge_indices,movable_edge_ends,movable_edge_lengths,
<a name="line_1441"></a>    num_vars,num_objs_a,num_objs_b,num_objs,
<a name="line_1442"></a>    Z1,E1,N1,J1,Q1,R1,dZ;
<a name="line_1443"></a>   global obj_count;
<a name="line_1444"></a>   
<a name="line_1445"></a>   n := this["num_charts"];
<a name="line_1446"></a>   m := this["num_edges"];
<a name="line_1447"></a>
<a name="line_1448"></a>   movable_chart_indices := NULL;
<a name="line_1449"></a>   movable_chart_beta    := NULL;
<a name="line_1450"></a>  
<a name="line_1451"></a>   pinned_edge_indices  := NULL;
<a name="line_1452"></a>   pinned_edge_ends     := NULL;
<a name="line_1453"></a>   pinned_edge_lengths  := NULL;
<a name="line_1454"></a>
<a name="line_1455"></a>   movable_edge_indices  := NULL;
<a name="line_1456"></a>   movable_edge_ends     := NULL;
<a name="line_1457"></a>   movable_edge_lengths  := NULL;
<a name="line_1458"></a>
<a name="line_1459"></a>   chart_lookup := table();
<a name="line_1460"></a>
<a name="line_1461"></a>   CC := eval(this["charts"]);
<a name="line_1462"></a>   EE := eval(this["edges"]);
<a name="line_1463"></a>
<a name="line_1464"></a>   j := 1;
<a name="line_1465"></a>
<a name="line_1466"></a>   for i from 0 to n-1 do
<a name="line_1467"></a>    if CC[i]["curve_index"] = NULL then
<a name="line_1468"></a>     chart_lookup[i] := j;
<a name="line_1469"></a>     j := j+1;
<a name="line_1470"></a>     movable_chart_indices := movable_chart_indices,i;
<a name="line_1471"></a>     movable_chart_beta    := movable_chart_beta,   CC[i]["beta"];
<a name="line_1472"></a>    fi;
<a name="line_1473"></a>   od;
<a name="line_1474"></a>   movable_chart_indices := [movable_chart_indices];
<a name="line_1475"></a>   movable_chart_beta    := [movable_chart_beta];
<a name="line_1476"></a>
<a name="line_1477"></a>   for i from 0 to m-1 do
<a name="line_1478"></a>    E := eval(EE[i]);
<a name="line_1479"></a>    j := E["start_index"];
<a name="line_1480"></a>    k := E["end_index"];
<a name="line_1481"></a>    if member(j,movable_chart_indices) then
<a name="line_1482"></a>     if member(k,movable_chart_indices) then
<a name="line_1483"></a>      movable_edge_indices := movable_edge_indices,i;
<a name="line_1484"></a>      movable_edge_ends :=
<a name="line_1485"></a>       movable_edge_ends, [chart_lookup[j],chart_lookup[k]];
<a name="line_1486"></a>      movable_edge_lengths := movable_edge_lengths,tanh(E["EH_length"]/2)^2;
<a name="line_1487"></a>     else
<a name="line_1488"></a>      pinned_edge_indices := pinned_edge_indices,i;
<a name="line_1489"></a>      pinned_edge_ends    := pinned_edge_ends,[C_to_R2(CC[k]["beta"]),chart_lookup[j]];
<a name="line_1490"></a>      pinned_edge_lengths := pinned_edge_lengths,tanh(E["EH_length"]/2)^2;
<a name="line_1491"></a>     fi;
<a name="line_1492"></a>    else
<a name="line_1493"></a>     if member(k,movable_chart_indices) then
<a name="line_1494"></a>      pinned_edge_indices := pinned_edge_indices,i;
<a name="line_1495"></a>      pinned_edge_ends    := pinned_edge_ends,[C_to_R2(CC[j]["beta"]),chart_lookup[k]];
<a name="line_1496"></a>      pinned_edge_lengths := pinned_edge_lengths,tanh(E["EH_length"]/2)^2;
<a name="line_1497"></a>     fi;
<a name="line_1498"></a>    fi;
<a name="line_1499"></a>   od;
<a name="line_1500"></a>   movable_edge_indices := [movable_edge_indices];
<a name="line_1501"></a>   movable_edge_ends    := [movable_edge_ends];
<a name="line_1502"></a>   movable_edge_lengths := [movable_edge_lengths];
<a name="line_1503"></a>   pinned_edge_indices  := [pinned_edge_indices];
<a name="line_1504"></a>   pinned_edge_ends     := [pinned_edge_ends];
<a name="line_1505"></a>   pinned_edge_lengths  := [pinned_edge_lengths];
<a name="line_1506"></a>
<a name="line_1507"></a>   num_vars := 2*nops(movable_chart_indices);
<a name="line_1508"></a>   num_objs_a := nops(pinned_edge_indices);
<a name="line_1509"></a>   num_objs_b := nops(movable_edge_indices);
<a name="line_1510"></a>   num_objs := num_objs_a + num_objs_b;
<a name="line_1511"></a>
<a name="line_1512"></a>   # mh(s,t) determines d_hyp(s,t), but mh is just a rational function,
<a name="line_1513"></a>   # whereas d_hyp involves sqrt and arctanh.  This makes it more tractable
<a name="line_1514"></a>   # to work with mh instead.
<a name="line_1515"></a>
<a name="line_1516"></a>   mh  := unapply(simplify(m_hyp(s,t)^2),s,t);
<a name="line_1517"></a>
<a name="line_1518"></a>   mh1 := unapply(simplify(diff(mh(s,t),s[1])),s,t);
<a name="line_1519"></a>   mh2 := unapply(simplify(diff(mh(s,t),s[2])),s,t);
<a name="line_1520"></a>   mh3 := unapply(simplify(diff(mh(s,t),t[1])),s,t);
<a name="line_1521"></a>   mh4 := unapply(simplify(diff(mh(s,t),t[2])),s,t);
<a name="line_1522"></a>
<a name="line_1523"></a>   obj := proc(coords)
<a name="line_1524"></a>    local i,j,k,t0,t1,errs;
<a name="line_1525"></a>    global last_coords,last_err;
<a name="line_1526"></a>
<a name="line_1527"></a>    errs := Vector(num_objs);
<a name="line_1528"></a>    
<a name="line_1529"></a>    for i from 1 to num_objs_a do
<a name="line_1530"></a>     t0 := pinned_edge_ends[i][1];
<a name="line_1531"></a>     j  := pinned_edge_ends[i][2];
<a name="line_1532"></a>     t1 := [coords[2*j-1],coords[2*j]];
<a name="line_1533"></a>     errs[i] := mh(t0,t1) - pinned_edge_lengths[i];
<a name="line_1534"></a>    od;
<a name="line_1535"></a>
<a name="line_1536"></a>    for i from 1 to num_objs_b do
<a name="line_1537"></a>     j  := movable_edge_ends[i][1];
<a name="line_1538"></a>     k  := movable_edge_ends[i][2];
<a name="line_1539"></a>     t0 := [coords[2*j-1],coords[2*j]];
<a name="line_1540"></a>     t1 := [coords[2*k-1],coords[2*k]];
<a name="line_1541"></a>     errs[num_objs_a+i] := mh(t0,t1) - movable_edge_lengths[i];
<a name="line_1542"></a>    od;
<a name="line_1543"></a>
<a name="line_1544"></a>    obj_count := obj_count+1;
<a name="line_1545"></a>    last_coords := convert(coords,list):
<a name="line_1546"></a>    last_err := convert(err,list):
<a name="line_1547"></a>    userinfo(5,genus2,sprintf("Step %d: err=%A",obj_count,Norm(errs)));
<a name="line_1548"></a>    return errs;
<a name="line_1549"></a>   end;
<a name="line_1550"></a>
<a name="line_1551"></a>   objjac := proc(coords)
<a name="line_1552"></a>    local i,j,k,t0,t1,err_diff;
<a name="line_1553"></a>
<a name="line_1554"></a>    err_diff := Matrix(num_objs,num_vars);
<a name="line_1555"></a>
<a name="line_1556"></a>    for i from 1 to num_objs_a do
<a name="line_1557"></a>     t0 := pinned_edge_ends[i][1];
<a name="line_1558"></a>     j  := pinned_edge_ends[i][2];
<a name="line_1559"></a>     t1 := [coords[2*j-1],coords[2*j]];
<a name="line_1560"></a>     err_diff[i,2*j-1] := mh3(t0,t1);
<a name="line_1561"></a>     err_diff[i,2*j  ] := mh4(t0,t1);
<a name="line_1562"></a>    od;
<a name="line_1563"></a>
<a name="line_1564"></a>    for i from 1 to num_objs_b do
<a name="line_1565"></a>     j  := movable_edge_ends[i][1];
<a name="line_1566"></a>     k  := movable_edge_ends[i][2];
<a name="line_1567"></a>     t0 := [coords[2*j-1],coords[2*j]];
<a name="line_1568"></a>     t1 := [coords[2*k-1],coords[2*k]];
<a name="line_1569"></a>     err_diff[num_objs_a+i,2*j-1] := mh1(t0,t1);
<a name="line_1570"></a>     err_diff[num_objs_a+i,2*j  ] := mh2(t0,t1);
<a name="line_1571"></a>     err_diff[num_objs_a+i,2*k-1] := mh3(t0,t1);
<a name="line_1572"></a>     err_diff[num_objs_a+i,2*k  ] := mh4(t0,t1);
<a name="line_1573"></a>    od;
<a name="line_1574"></a>
<a name="line_1575"></a>    return err_diff;
<a name="line_1576"></a>   end;
<a name="line_1577"></a>
<a name="line_1578"></a>   obj_count := 0;
<a name="line_1579"></a>
<a name="line_1580"></a>   Z1 := Vector(map(C_to_R2,movable_chart_beta));
<a name="line_1581"></a>   E1 := obj(Z1);
<a name="line_1582"></a>   N1 := Norm(E1);
<a name="line_1583"></a>   
<a name="line_1584"></a>   for i from 1 to 5 do
<a name="line_1585"></a>    J1 := objjac(Z1);
<a name="line_1586"></a>    Q1,R1 := QRDecomposition(J1);
<a name="line_1587"></a>    dZ := LinearSolve(R1,-Transpose(Q1).E1);
<a name="line_1588"></a>    Z1 := Z1 + dZ;
<a name="line_1589"></a>    E1 := obj(Z1);
<a name="line_1590"></a>    N1 := Norm(E1);
<a name="line_1591"></a>   od:
<a name="line_1592"></a>
<a name="line_1593"></a>   # Z1 := LSSolve([num_vars,num_objs],obj,objectivejacobian=objjac,initialpoint=Z1)[2];
<a name="line_1594"></a>   
<a name="line_1595"></a>   for i from 1 to num_vars/2 do
<a name="line_1596"></a>    j := movable_chart_indices[i];
<a name="line_1597"></a>    CC[j]["beta"] := Z1[2*i-1] + I * Z1[2*i];
<a name="line_1598"></a>   od;
<a name="line_1599"></a>
<a name="line_1600"></a>   this["set_edge_lengths"];
<a name="line_1601"></a>   this["optimize_lambda"];
<a name="line_1602"></a>   NULL;
<a name="line_1603"></a>  end
<a name="line_1604"></a> ],
<a name="line_1605"></a>
<a name="line_1606"></a> ["Method","curvature_error_plot","Generate a plot showing the difference between $-1$ and the curvature of the rescaled metric.",
<a name="line_1607"></a>  proc(this)
<a name="line_1608"></a>   z_plot(this["curvature_z"](z)+1);
<a name="line_1609"></a>  end
<a name="line_1610"></a> ],
<a name="line_1611"></a>
<a name="line_1612"></a> ["Method","log_rescale_plot","Generate a plot of the @log_rescale_z@ field.",
<a name="line_1613"></a>  proc(this)
<a name="line_1614"></a>   z_plot(this["log_rescale_z"](z));
<a name="line_1615"></a>  end
<a name="line_1616"></a> ],
<a name="line_1617"></a>
<a name="line_1618"></a> ["Method","beta_plot","Generate a plot showing the $\\beta$ values of all the charts, and the edges between them.",
<a name="line_1619"></a>  proc(this)
<a name="line_1620"></a>   local i,m,P,CC,EE,E;
<a name="line_1621"></a>   
<a name="line_1622"></a>   P := NULL;
<a name="line_1623"></a>   m := this["num_edges"];
<a name="line_1624"></a>   CC := eval(this["charts"]);
<a name="line_1625"></a>   EE := eval(this["edges"]);
<a name="line_1626"></a>   
<a name="line_1627"></a>   for i from 0 to m-1 do
<a name="line_1628"></a>    E := eval(EE[i]);
<a name="line_1629"></a>    P := P,line(C_to_R2(CC[E["start_index"]]["beta"]),
<a name="line_1630"></a>                C_to_R2(CC[E["end_index"]]["beta"]),
<a name="line_1631"></a>		colour=E["colour"]);
<a name="line_1632"></a>   od;
<a name="line_1633"></a>   display(P,axes=none,scaling=constrained);
<a name="line_1634"></a>  end
<a name="line_1635"></a> ],
<a name="line_1636"></a>
<a name="line_1637"></a> ["Method","beta_plot_tikz","Generate a plot showing the $\\beta$ values of all the charts, and the edges between them.",
<a name="line_1638"></a>  proc(this)
<a name="line_1639"></a>   local i,m,s,CC,EE,E;
<a name="line_1640"></a>   
<a name="line_1641"></a>   m := this["num_edges"];
<a name="line_1642"></a>   CC := eval(this["charts"]);
<a name="line_1643"></a>   EE := eval(this["edges"]);
<a name="line_1644"></a>
<a name="line_1645"></a>   s := cat(
<a name="line_1646"></a>    "\\begin{center}\n",
<a name="line_1647"></a>    " \\begin{tikzpicture}[scale=12]\n"
<a name="line_1648"></a>   );
<a name="line_1649"></a>    
<a name="line_1650"></a>   for i from 0 to m-1 do
<a name="line_1651"></a>    E := eval(EE[i]);
<a name="line_1652"></a>    s := cat(s,sprintf("  \\draw[maple%A] (%1.3f,%1.3f) -- (%1.3f,%1.3f);\n",
<a name="line_1653"></a>                       E["colour"],
<a name="line_1654"></a>		       op(C_to_R2(CC[E["start_index"]]["beta"])),
<a name="line_1655"></a>		       op(C_to_R2(CC[E["end_index"]]["beta"]))
<a name="line_1656"></a>		       ));
<a name="line_1657"></a>   od;
<a name="line_1658"></a>
<a name="line_1659"></a>   s := cat(s,
<a name="line_1660"></a>    " \\end{tikzpicture}\n",
<a name="line_1661"></a>    "\\end{center}\n"
<a name="line_1662"></a>   );
<a name="line_1663"></a>
<a name="line_1664"></a>   return s;
<a name="line_1665"></a>  end
<a name="line_1666"></a> ],
<a name="line_1667"></a>
<a name="line_1668"></a> ["Method","H_sample_plot","Generate a plot showing the H-sample points.",
<a name="line_1669"></a>  proc(this)
<a name="line_1670"></a>   local P,k,cH;
<a name="line_1671"></a>   
<a name="line_1672"></a>   P := NULL;
<a name="line_1673"></a>   for k in [0,1,3,5] do
<a name="line_1674"></a>    cH := evalf(subs(a_H=this["a_H"],c_H[k](t)));
<a name="line_1675"></a>    P := P,cplot(cH,t=F16_curve_limits[k],colour=c_colour[k]);
<a name="line_1676"></a>   od;
<a name="line_1677"></a>
<a name="line_1678"></a>   P := P,op(map(cpoint,this["H_samples"],colour=grey));
<a name="line_1679"></a>
<a name="line_1680"></a>   display(P,scaling=constrained,axes=none);
<a name="line_1681"></a>  end
<a name="line_1682"></a> ],
<a name="line_1683"></a>
<a name="line_1684"></a> ["Method","D_sample_plot","Generate a plot showing the D-sample points.",
<a name="line_1685"></a>  proc(this)
<a name="line_1686"></a>   local P,k,cH;
<a name="line_1687"></a>   display(
<a name="line_1688"></a>    circle(1),
<a name="line_1689"></a>    line([-1,-1] /~ sqrt(2.),[ 1, 1] /~ sqrt(2.),colour=c_colour[1]),
<a name="line_1690"></a>    line([-1, 1] /~ sqrt(2.),[ 1,-1] /~ sqrt(2.),colour=c_colour[2]),
<a name="line_1691"></a>    line([-1, 0],[ 1, 0],colour=c_colour[5]),
<a name="line_1692"></a>    line([ 0,-1],[ 0, 1],colour=c_colour[6]),
<a name="line_1693"></a>    seq(seq(xi_circle(I^k*c_H_p0[j],colour=c_colour[j]),k=0..3),j in [0,3,7,8]),
<a name="line_1694"></a>    op(map(cpoint,this["D_samples"],colour=grey)),
<a name="line_1695"></a>    scaling=constrained,axes=none
<a name="line_1696"></a>   );
<a name="line_1697"></a>  end
<a name="line_1698"></a> ],
<a name="line_1699"></a>
<a name="line_1700"></a> ["Method","H_sample_q_plot","Generate a plot showing the images in $EX^*$ of the H-sample points.",
<a name="line_1701"></a>  proc(this)
<a name="line_1702"></a>   local k,t,x;
<a name="line_1703"></a>   
<a name="line_1704"></a>   display(
<a name="line_1705"></a>    seq(spacecurve(stereo(c_E0[k](t)),t=F16_curve_limits[k],colour=c_colour[k]),k in [0,1,3,5]),
<a name="line_1706"></a>    map(x -> point(stereo(x),colour=grey),this["H_samples_q"]),
<a name="line_1707"></a>    axes=none,scaling=constrained
<a name="line_1708"></a>   );
<a name="line_1709"></a>  end
<a name="line_1710"></a> ],
<a name="line_1711"></a>
<a name="line_1712"></a> ["Method","D_sample_q_plot","Generate a plot showing the images in $EX^*$ of the D-sample points.",
<a name="line_1713"></a>  proc(this)
<a name="line_1714"></a>   local k,t,x;
<a name="line_1715"></a>
<a name="line_1716"></a>   load_plot("EX_wireframe"):
<a name="line_1717"></a>
<a name="line_1718"></a>   display(
<a name="line_1719"></a>    pics["EX_wireframe"],
<a name="line_1720"></a>    seq(spacecurve(stereo(c_E0[k](t)),t=0..2*Pi,colour=c_colour[k]),k=0..8),
<a name="line_1721"></a>    map(x -> point(stereo(x),colour=black),this["D_samples_q"]),
<a name="line_1722"></a>    axes=none,scaling=constrained
<a name="line_1723"></a>   );
<a name="line_1724"></a>  end
<a name="line_1725"></a> ],
<a name="line_1726"></a>
<a name="line_1727"></a> ["Method","square_q_inv_plot","This method shows a distorted grid obtained by applying the @square_q_inv@ map to a regular grid covering the unit square.  Thus, the distorted grid will cover the domain $HF_{16}$ in the unit disc.",
<a name="line_1728"></a>  proc(this)
<a name="line_1729"></a>   local sqi;
<a name="line_1730"></a>   sqi := eval(this["square_q_inv"]);
<a name="line_1731"></a>   display(
<a name="line_1732"></a>    seq(cplot(sqi([0.1*i,t]),t=0..1,colour=blue),i=0..10),
<a name="line_1733"></a>    seq(cplot(sqi([t,0.1*i]),t=0..1,colour=red),i=0..10),
<a name="line_1734"></a>    scaling=constrained
<a name="line_1735"></a>   );
<a name="line_1736"></a>  end
<a name="line_1737"></a> ],
<a name="line_1738"></a>
<a name="line_1739"></a> ["Method","edge_length_scatter_plot","We usually want to arrange our charts and edges so that most edges have approximately the same length.  Also, if everything was working perfectly, then the edge lengths in the @H_length@ and @EH_length@ fields would be the same. Thus, the plot generated by this method should show a cluster of points that are very close to the diagonal, and mostly close to each other.",
<a name="line_1740"></a>  proc(this)
<a name="line_1741"></a>   local i,m,PP,EE,E;
<a name="line_1742"></a>   
<a name="line_1743"></a>   PP := NULL:
<a name="line_1744"></a>   m := this["num_edges"];
<a name="line_1745"></a>   EE := eval(this["edges"]):
<a name="line_1746"></a>   for i from 0 to m-1 do
<a name="line_1747"></a>    E := eval(EE[i]);
<a name="line_1748"></a>    PP := PP,point([E["EH_length"],E["H_length"]]);
<a name="line_1749"></a>   od:
<a name="line_1750"></a>   display(PP);
<a name="line_1751"></a>  end
<a name="line_1752"></a> ],
<a name="line_1753"></a>
<a name="line_1754"></a> ["Method","edge_length_anomaly_plot","If everything was working perfectly, then the edge lengths in the @H_length@ and @EH_length@ fields would be the same.  This method generates a plot showing a normalised measure of the difference between them.",
<a name="line_1755"></a>  proc(this)
<a name="line_1756"></a>   local i,m,LA,EE,E;
<a name="line_1757"></a>   
<a name="line_1758"></a>   LA := NULL:
<a name="line_1759"></a>   m := this["num_edges"];
<a name="line_1760"></a>   EE := eval(this["edges"]):
<a name="line_1761"></a>   for i from 0 to m-1 do
<a name="line_1762"></a>    E := eval(EE[i]);
<a name="line_1763"></a>    LA := LA,2*(E["EH_length"]-E["H_length"])/(E["EH_length"]+E["H_length"]);
<a name="line_1764"></a>   od:
<a name="line_1765"></a>   LA := sort([LA]):
<a name="line_1766"></a>   listplot(LA);
<a name="line_1767"></a>  end
<a name="line_1768"></a> ],
<a name="line_1769"></a>
<a name="line_1770"></a> ["Method","nearest_charts_plot","This generates a plot which helps one to understand how well the fundamental domain $HF_{16}$ is covered by the charts in this atlas.  For each point in @H_samples@ we find the distance to the closest chart centre, then we sort these distances in order, and the result is the red curve.  The green curve shows distances to the second closest chart centre, and so on, up to the cyan curve, which shows distances to the fifth closest chart centre.",
<a name="line_1771"></a>  proc(this)
<a name="line_1772"></a>   local i,j,n,m,dd,dl,cols;
<a name="line_1773"></a>
<a name="line_1774"></a>   n := this["num_charts"];
<a name="line_1775"></a>   m := nops(this["H_samples"]);
<a name="line_1776"></a>   dd := eval(this["chart_dist"]);
<a name="line_1777"></a>   if (dd = NULL) then
<a name="line_1778"></a>    this["set_chart_dist"];
<a name="line_1779"></a>    dd := eval(this["chart_dist"]);
<a name="line_1780"></a>   fi;
<a name="line_1781"></a>   dl := table();
<a name="line_1782"></a>   for i from 1 to m do
<a name="line_1783"></a>    dl[i] := sort([seq(dd[i,j],j=0..n-1)]);
<a name="line_1784"></a>   od:
<a name="line_1785"></a>
<a name="line_1786"></a>   cols := [red,green,blue,magenta,cyan];
<a name="line_1787"></a>
<a name="line_1788"></a>   display(
<a name="line_1789"></a>    seq(listplot(sort([seq(dl[i][j],i=1..m)]),colour=cols[j]),j=1..5),
<a name="line_1790"></a>    seq(line([0,0.02*i],[m,0.02*i],colour=grey),i=1..7)
<a name="line_1791"></a>   );
<a name="line_1792"></a>  end
<a name="line_1793"></a> ],
<a name="line_1794"></a>
<a name="line_1795"></a> ["Method","fourier_v_plot","This generates a surface plot of the @m@'th component of the map $q\\colon\\Delta\\to EX^*\\subset\\mathbb{R}^4$",
<a name="line_1796"></a>  proc(this,m)
<a name="line_1797"></a>   local i,j,k,T,Z,R,V,P,r;
<a name="line_1798"></a>
<a name="line_1799"></a>   k := this["fourier_k"];
<a name="line_1800"></a>   T := [seq(i/2^k,i=0..2^k-1)];
<a name="line_1801"></a>   Z := map(t -> evalf(exp(2*Pi*I*t)),T);
<a name="line_1802"></a>   R := this["fourier_r"];
<a name="line_1803"></a>   V := eval(this["fourier_v"]);
<a name="line_1804"></a>   P := table();
<a name="line_1805"></a>   for i from 1 to nops(R) do
<a name="line_1806"></a>    r := R[i];
<a name="line_1807"></a>    for j from 0 to 2^k-1 do
<a name="line_1808"></a>     P[i,j] := [r*Re(Z[j+1]),r*Im(Z[j+1]),V[R[i],j/2^k][m]];
<a name="line_1809"></a>    od;
<a name="line_1810"></a>    P[i,2^k] := P[i,0];
<a name="line_1811"></a>   od;;
<a name="line_1812"></a>
<a name="line_1813"></a>   return display(
<a name="line_1814"></a>    seq(seq(polygon([P[i,j],P[i+1,j],P[i+1,j+1]]),j=0..2^k-1),i=1..nops(R)-1),
<a name="line_1815"></a>    seq(seq(polygon([P[i,j],P[i,j+1],P[i+1,j+1]]),j=0..2^k-1),i=1..nops(R)-1),
<a name="line_1816"></a>    scaling=constrained,axes=none,style=patchnogrid
<a name="line_1817"></a>   );
<a name="line_1818"></a>  end
<a name="line_1819"></a> ],
<a name="line_1820"></a>
<a name="line_1821"></a> ["Method","fourier_ring_vals","This method generates a list of points in $EX^*\\subset\\mathbb{R}^4$ obtained by applying the map $q\\colon\\Delta\\to EX^*$ to $n$ equally spaced points on the circle of radius $r$ centred at $0$.",
<a name="line_1822"></a>  proc(this,r::RR0,n::posint := 1024)
<a name="line_1823"></a>   local i,j,k,a,S,s,x,v,V;
<a name="line_1824"></a>
<a name="line_1825"></a>   a := table();
<a name="line_1826"></a>   k := this["fourier_k"];
<a name="line_1827"></a>   S := eval(this["fourier_a_spline"]);
<a name="line_1828"></a>   x := table();
<a name="line_1829"></a>   x[1] := add(S[1,i](r)*cos((2*i+1)*theta),i = 0..2^(k-2)-1); 
<a name="line_1830"></a>   x[2] := add(S[2,i](r)*sin((2*i+1)*theta),i = 0..2^(k-2)-1); 
<a name="line_1831"></a>   x[3] := S[3,0](r)/2 +
<a name="line_1832"></a>           add(S[3,i](r)*cos((4*i  )*theta),i = 1..2^(k-3)-1); 
<a name="line_1833"></a>   x[4] := add(S[4,i](r)*cos((4*i+2)*theta),i = 0..2^(k-3)-1);
<a name="line_1834"></a>   x := [x[1],x[2],x[3],x[4]];
<a name="line_1835"></a>
<a name="line_1836"></a>   V := NULL;
<a name="line_1837"></a>   for i from 0 to n do
<a name="line_1838"></a>    if modp(i,10) = 0 then
<a name="line_1839"></a>     userinfo(8,genus2,sprintf("fourier_ring_vals: i = %d",i));
<a name="line_1840"></a>    fi;
<a name="line_1841"></a>    s := evalf(2*Pi*i/n);
<a name="line_1842"></a>    v := evalf(subs(theta = s,x));
<a name="line_1843"></a>    V := V,v;
<a name="line_1844"></a>   od:
<a name="line_1845"></a>   
<a name="line_1846"></a>   return([V]);
<a name="line_1847"></a>  end
<a name="line_1848"></a> ],
<a name="line_1849"></a>
<a name="line_1850"></a> ["Method","fourier_curve","This generates a plot of the curve $t\mapsto p(q(r\,e^{it}))$, where $r$ is supplied as the first argument, and $p$ is a map from $EX^*$ to $\\mathbb{R}^2$ or $\\mathbb{R}^3$ which is supplied as the third argument.  The second argument is the number of sample points, which needs to be large in order to get a smooth plot. If the third argument is omitted then we use the stereographic projection map to $\\mathbb{R}^3$",
<a name="line_1851"></a>  proc(this,r::RR0,n::posint := 1024,p := stereo)
<a name="line_1852"></a>   return(
<a name="line_1853"></a>    display(
<a name="line_1854"></a>     curve(map(p,this["fourier_ring_vals",r,n]),colour=black),
<a name="line_1855"></a>     scaling=constrained,axes=none
<a name="line_1856"></a>    ));
<a name="line_1857"></a>  end
<a name="line_1858"></a> ]
<a name="line_1859"></a>):
<a name="line_1860"></a>
<a name="line_1861"></a>
  </pre>
 </body>
</html>
    