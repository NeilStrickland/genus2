<html>
 <head>
 </head>
 <body>
  <pre>
    <a name="line_1"></a># This file just reads in various other files. 
<a name="line_2"></a>
<a name="line_3"></a># We start by defining the function _read(), which announces the name
<a name="line_4"></a># of a file before reading it.
<a name="line_5"></a>
<a name="line_6"></a>__FILE__ := NULL;
<a name="line_7"></a>
<a name="line_8"></a>_read := proc(s) 
<a name="line_9"></a> global __FILE__;
<a name="line_10"></a> __FILE__ := s;
<a name="line_11"></a> printf("Reading %s\n",s);
<a name="line_12"></a> read(s); 
<a name="line_13"></a> __FILE__ := NULL;
<a name="line_14"></a>end:
<a name="line_15"></a>
<a name="line_16"></a>_read("util.mpl");
<a name="line_17"></a>_read("class.mpl");
<a name="line_18"></a>
<a name="line_19"></a>Package("genus2",""):
<a name="line_20"></a>
<a name="line_21"></a>_read("Rn.mpl");
<a name="line_22"></a>_read("brent.mpl");
<a name="line_23"></a>_read("group.mpl");
<a name="line_24"></a>_read("groupoid.mpl");
<a name="line_25"></a>_read("cromulent.mpl");
<a name="line_26"></a>_read("nets.mpl");
<a name="line_27"></a>_read("latex.mpl");
<a name="line_28"></a>_read("plots.mpl");
<a name="line_29"></a>_read("checks.mpl");
<a name="line_30"></a>_read("build_data.mpl");
<a name="line_31"></a>
<a name="line_32"></a>_read("quadrature/quadrature.mpl");
<a name="line_33"></a>
<a name="line_34"></a>_read("domain/domain.mpl"):
<a name="line_35"></a>_read("domain/domain_point.mpl"):
<a name="line_36"></a>_read("domain/domain_edge.mpl"):
<a name="line_37"></a>_read("domain/domain_face.mpl"):
<a name="line_38"></a>_read("domain/grid.mpl"):
<a name="line_39"></a>
<a name="line_40"></a>_read("embedded/EX.mpl");
<a name="line_41"></a>_read("embedded/invariants.mpl");
<a name="line_42"></a>_read("embedded/geometry.mpl");
<a name="line_43"></a>_read("embedded/curvature.mpl");
<a name="line_44"></a>_read("embedded/EX0.mpl");
<a name="line_45"></a>_read("embedded/extra_curves.mpl"):
<a name="line_46"></a>_read("embedded/extra_vertices.mpl"):
<a name="line_47"></a>_read("embedded/annular_charts.mpl"):
<a name="line_48"></a>_read("embedded/disc_proj.mpl"):
<a name="line_49"></a>_read("embedded/cayley.mpl");
<a name="line_50"></a>_read("embedded/barycentric.mpl");
<a name="line_51"></a>_read("embedded/E_domain.mpl"):
<a name="line_52"></a>_read("embedded/E_quadrature.mpl"):
<a name="line_53"></a>_read("embedded/homology.mpl"):
<a name="line_54"></a>_read("embedded/plots.mpl"):
<a name="line_55"></a>_read("embedded/roothalf/E_roothalf.mpl");
<a name="line_56"></a>_read("embedded/roothalf/cayley_surface.mpl");
<a name="line_57"></a>_read("embedded/roothalf/E_atlas.mpl");
<a name="line_58"></a>_read("embedded/roothalf/EH_atlas.mpl");
<a name="line_59"></a>_read("embedded/roothalf/crease.mpl");
<a name="line_60"></a>_read("embedded/roothalf/forms.mpl");
<a name="line_61"></a>_read("embedded/roothalf/rational.mpl");
<a name="line_62"></a>_read("embedded/roothalf/square_diffeo.mpl");
<a name="line_63"></a>_read("embedded/roothalf/zeta.mpl");
<a name="line_64"></a>_read("embedded/roothalf/sphere_quotients.mpl");
<a name="line_65"></a>_read("embedded/roothalf/better_sphere_quotients.mpl");
<a name="line_66"></a>_read("embedded/roothalf/E_to_S.mpl");
<a name="line_67"></a>_read("embedded/roothalf/torus_quotients.mpl");
<a name="line_68"></a>_read("embedded/roothalf/group64.mpl");
<a name="line_69"></a>_read("embedded/roothalf/galois.mpl");
<a name="line_70"></a>_read("embedded/roothalf/KR_subfields.mpl");
<a name="line_71"></a>_read("embedded/roothalf/plots.mpl");
<a name="line_72"></a>
<a name="line_73"></a>_read("projective/PX.mpl");
<a name="line_74"></a>_read("projective/galois.mpl");
<a name="line_75"></a>_read("projective/ellquot.mpl");
<a name="line_76"></a>_read("projective/picard_fuchs.mpl");
<a name="line_77"></a>_read("projective/PK_subfields.mpl");
<a name="line_78"></a>_read("projective/PX0.mpl");
<a name="line_79"></a>_read("projective/plots.mpl");
<a name="line_80"></a>
<a name="line_81"></a>_read("hyperbolic/Pi.mpl");
<a name="line_82"></a>_read("hyperbolic/HX.mpl");
<a name="line_83"></a>_read("hyperbolic/HX0.mpl");
<a name="line_84"></a>_read("hyperbolic/HP_table.mpl");
<a name="line_85"></a>_read("hyperbolic/H_to_P.mpl");
<a name="line_86"></a>_read("hyperbolic/P_to_H.mpl");
<a name="line_87"></a>_read("hyperbolic/geodesics.mpl");
<a name="line_88"></a>_read("hyperbolic/schwarz.mpl");
<a name="line_89"></a>_read("hyperbolic/plots.mpl");
<a name="line_90"></a>_read("hyperbolic/automorphic.mpl");
<a name="line_91"></a>
<a name="line_92"></a>EndPackage():
<a name="line_93"></a>
<a name="line_94"></a>
<a name="line_95"></a>
<a name="line_96"></a>
  </pre>
 </body>
</html>
    