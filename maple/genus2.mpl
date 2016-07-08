# This file just reads in various other files. 

# We start by defining the function _read(), which announces the name
# of a file before reading it.
_read := proc(s) printf("Reading %s\n",s); read(s); end:
# _read := proc(s) read(s); end:


_read("util.mpl");
_read("class.mpl");

Package("genus2",""):

_read("Rn.mpl");
_read("brent.mpl");
_read("group.mpl");
_read("groupoid.mpl");
_read("cromulent.mpl");
_read("nets.mpl");
_read("latex.mpl");
_read("plots.mpl");
_read("checks.mpl");

_read("quadrature/quadrature.mpl");

_read("domain/domain.mpl"):
_read("domain/domain_point.mpl"):
_read("domain/domain_edge.mpl"):
_read("domain/domain_face.mpl"):
_read("domain/grid.mpl"):

_read("embedded/EX.mpl");
_read("embedded/invariants.mpl");
_read("embedded/geometry.mpl");
_read("embedded/curvature.mpl");
_read("embedded/EX0.mpl");
_read("embedded/extra_curves.mpl"):
_read("embedded/extra_vertices.mpl"):
_read("embedded/annular_charts.mpl"):
_read("embedded/disc_proj.mpl"):
_read("embedded/cayley.mpl");
_read("embedded/barycentric.mpl");
_read("embedded/E_domain.mpl"):
_read("embedded/E_quadrature.mpl"):
_read("embedded/homology.mpl"):
_read("embedded/plots.mpl"):
_read("embedded/roothalf/E_roothalf.mpl");
_read("embedded/roothalf/cayley_surface.mpl");
_read("embedded/roothalf/E_atlas.mpl");
_read("embedded/roothalf/EH_atlas.mpl");
_read("embedded/roothalf/crease.mpl");
_read("embedded/roothalf/forms.mpl");
_read("embedded/roothalf/rational.mpl");
_read("embedded/roothalf/square_diffeo.mpl");
_read("embedded/roothalf/zeta.mpl");
_read("embedded/roothalf/sphere_quotients.mpl");
_read("embedded/roothalf/better_sphere_quotients.mpl");
_read("embedded/roothalf/E_to_S.mpl");
_read("embedded/roothalf/torus_quotients.mpl");
_read("embedded/roothalf/group64.mpl");
_read("embedded/roothalf/galois.mpl");
_read("embedded/roothalf/KR_subfields.mpl");
_read("embedded/roothalf/plots.mpl");
_read("embedded/roothalf/isometric_grid.mpl");

_read("projective/PX.mpl");
_read("projective/galois.mpl");
_read("projective/ellquot.mpl");
_read("projective/picard_fuchs.mpl");
_read("projective/PK_subfields.mpl");
_read("projective/PX0.mpl");
_read("projective/plots.mpl");

_read("hyperbolic/Pi.mpl");
_read("hyperbolic/HX.mpl");
_read("hyperbolic/HX0.mpl");
_read("hyperbolic/HP_table.mpl");
_read("hyperbolic/H_to_P.mpl");
_read("hyperbolic/P_to_H.mpl");
_read("hyperbolic/geodesics.mpl");
_read("hyperbolic/schwarz.mpl");
_read("hyperbolic/plots.mpl");
_read("hyperbolic/automorphic.mpl");

EndPackage():




